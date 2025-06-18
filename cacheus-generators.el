;;; cacheus-generators.el --- Generic function generators for Cacheus -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is the code-generation engine for the Cacheus framework. It
;; builds the `defvar` boilerplate and administrative functions for any cache.
;; Public API interaction (get/put) is handled by universal functions in
;; `cacheus-core.el`.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'ts)
(require 'dash)
(require 'concur nil t)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-persistence)
(require 'cacheus-storage)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primary Backend Generator (Package-Private)

(defun cacheus-make-cache-backend (instance instance-var-sym)
  "Generate the complete backend code for a cache INSTANCE.

This is the central code-generation engine for the Cacheus
framework, called by the `defcacheus` macro. It constructs a
large `progn` form containing all the necessary `defvar` and
`defun` forms for a given cache configuration.

Arguments:
  INSTANCE (cacheus-instance): The template instance struct for the cache.
  INSTANCE-VAR-SYM (symbol): The symbol for the cache's global instance
    variable (e.g., `my-cache--instance`).

Returns:
  (list) A Lisp form (a `progn` block) ready for evaluation."
  (cacheus-let*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :name name :cache-file file :logger lopt) opts)
       ((&struct :all-struct-fields-for-entries fields
                 :struct-name-for-entries s-name
                 :make-fn-constructor-for-entries ctor
                 :clear-fn clear-fn
                 :save-fn save-fn
                 :load-fn load-fn
                 :inspect-cache-fn insp-cache-fn
                 :inspect-entry-fn insp-entry-fn
                 :invalidate-tags-fn inv-tags-fn)
        syms)
       (unique-fields (cl-remove-duplicates fields :key #'car :test #'eq))
       (cache-type (pcase (type-of instance)
                     ('cacheus-memoize-instance 'memoize)
                     ('cacheus-cache-instance 'cache)
                     (t (error "Unknown instance type: %S" (type-of instance))))))
    `(progn
       ;; --- Define the per-entry struct ---
       ;; This is defined at compile time so that the compiler knows about the
       ;; struct's accessors when compiling the generated functions.
       (eval-when-compile
         (unless (fboundp ',ctor)
           (cl-defstruct (,s-name (:constructor ,ctor)) ,@unique-fields)))

       ;; --- Define core variables for the cache instance ---
       (defvar ,instance-var-sym ,instance
         ,(format "The global instance struct for the '%s' cache." name))

       ;; --- Register the cache in the global registry ---
       ;; This makes the cache discoverable by the universal API and management
       ;; functions like `cacheus-get!` and `cacheus-list-all-caches`.
       (ht-set! cacheus-global-cache-registry ',name
                (list :name ',name
                      :type ',cache-type
                      :config (cacheus-instance-options ,instance-var-sym)
                      :symbols-struct (cacheus-instance-symbols ,instance-var-sym)
                      :instance-var-sym ',instance-var-sym
                      :clear-fn-symbol ',clear-fn
                      :save-fn-symbol ',save-fn
                      :load-fn-symbol ',load-fn
                      :inspect-fn-symbol ',insp-cache-fn
                      :invalidate-by-tags-fn-symbol ',inv-tags-fn))

       ;; --- Generate administrative functions ---
       ,(cacheus--make-clear-fn-form clear-fn name instance-var-sym)
       ,(cacheus--make-save-fn-form save-fn name instance-var-sym)
       ,(cacheus--make-load-fn-form load-fn name instance-var-sym)
       ,(cacheus--make-inspect-cache-fn-form insp-cache-fn name instance-var-sym)
       ,(cacheus--make-inspect-entry-fn-form insp-entry-fn name instance-var-sym)
       ,(cacheus--make-invalidate-by-tags-fn-form inv-tags-fn name instance-var-sym)

       ;; --- Set up initial loading from file ---
       ;; If a :cache-file is specified, schedule the cache to be loaded from
       ;; that file once the Cacheus system is fully loaded.
       ,(when (and load-fn file)
          `(eval-after-load 'cacheus-core
             '(lambda ()
                (let ((logger (cacheus-resolve-logger ,lopt)))
                  (when (file-exists-p ,file)
                    (funcall logger :info "[C:%s] Initial load from %s" ',name ,file)
                    (condition-case-unless-debug e (,load-fn)
                      (error
                       (funcall logger :error "[C:%s] Initial load failed: %S"
                                ',name e :trace)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Administrative Function Generators (File-Local)

(defun cacheus--make-clear-fn-form (fn-sym cache-name instance-var-sym)
  "Generate the `defun` form for a 'clear cache' function."
  `(defun ,fn-sym ()
     ,(format "Clear all entries from the '%s' cache." cache-name)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
            (hook (cacheus-options-clear-hook opts)))
       (funcall logger :info "[C:%s] Clearing cache..." ',cache-name)
       (cacheus-clear-runtime-data instance)
       (when hook
         (condition-case-unless-debug e (funcall hook)
           (error (funcall logger :error "[C:%s] clear-hook error: %S"
                           ',cache-name e :trace)))))
     ',fn-sym))

(defun cacheus--make-save-fn-form (fn-sym cache-name instance-var-sym)
  "Generate the `defun` form for a 'save cache' function."
  `(defun ,fn-sym ()
     ,(format "Save the '%s' cache to its file." cache-name)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (progn
             (funcall logger :info "[C:%s] Saving to %s..." ',cache-name file)
             (condition-case-unless-debug e
                 (cacheus-persist-cache-instance instance logger)
               (error
                (funcall logger :error "[C:%s] Save failed: %S" ',cache-name e)
                (signal 'cacheus-error (list "Save failed" e)))))
         (funcall logger :warn "[C:%s] Save failed: No :cache-file configured."
                  ',cache-name)))
     ',fn-sym))

(defun cacheus--make-load-fn-form (fn-sym cache-name instance-var-sym)
  "Generate the `defun` form for a 'load cache' function."
  `(defun ,fn-sym ()
     ,(format "Load the '%s' cache from its file." cache-name)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (if (file-readable-p file)
               (progn
                 (funcall logger :info "[C:%s] Loading from %s..." ',cache-name file)
                 (cacheus-load-cache-instance instance logger))
             (funcall logger :warn "[C:%s] Load failed: File not readable: %s"
                      ',cache-name file))
         (funcall logger :warn "[C:%s] Load failed: No :cache-file configured."
                  ',cache-name)))
     ',fn-sym))

(defun cacheus--make-inspect-cache-fn-form (fn-sym cache-name instance-var-sym)
  "Generate a wrapper function to inspect an entire cache."
  `(defun ,fn-sym ()
     ,(format "Inspect all entries in the '%s' cache." cache-name)
     (interactive)
     (cacheus-inspect-instance-dispatch (symbol-value ',instance-var-sym) nil)))

(defun cacheus--make-inspect-entry-fn-form (fn-sym cache-name instance-var-sym)
  "Generate a wrapper function to inspect a single cache entry."
  `(defun ,fn-sym (key)
     ,(format "Inspect entry in '%s' by KEY." cache-name)
     (interactive "sKey (Lisp string): ")
     (cacheus-inspect-instance-dispatch (symbol-value ',instance-var-sym) key)))

(defun cacheus--make-invalidate-by-tags-fn-form (fn-sym cache-name instance-var-sym)
  "Generate the `defun` form for an 'invalidate-by-tags' function."
  `(cl-defun ,fn-sym (tags &key (all-must-match nil) (run-hooks t))
     ,(format "Invalidate entries by TAGS for '%s'." cache-name)
     (interactive
      (list
       (condition-case err
           (read (read-from-minibuffer "Invalidate Tags (Lisp form): "))
         (error (message "Invalid Lisp syntax: %s" (error-message-string err))
                nil))))
     (when tags
       (cacheus-invalidate-keys-by-tags (symbol-value ',instance-var-sym)
                                          tags
                                          :all-must-match all-must-match
                                          :run-hooks run-hooks))
     ',fn-sym))

(provide 'cacheus-generators)
;;; cacheus-generators.el ends here