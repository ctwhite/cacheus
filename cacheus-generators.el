;;; cacheus-generators.el --- Generic function generators for Cacheus -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file is the code-generation engine for the Cacheus framework. It builds
;; the `defvar` boilerplate and administrative functions for any cache.
;; Public API interaction (get/put) is handled by universal functions in
;; `cacheus-core.el`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Primary Backend Generator

(defun cacheus-make-cache-backend (instance instance-var-sym)
  "The central code-generation engine for the Cacheus framework.
This function is called by macros like `cacheus-memoize!`. It takes a
pre-configured `INSTANCE` blueprint and generates a `(progn ...)` form
containing all the Lisp code required to create the backing variables and
administrative functions for a cache instance.

Arguments:
- `INSTANCE` (cacheus-instance): A complete blueprint of the cache.
- `INSTANCE-VAR-SYM` (symbol): The symbol of the `defvar` that will hold the
  runtime instance of this cache.

Returns:
  (list): A `(progn ...)` Lisp form ready for macro expansion."
  (cacheus-let*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :name name :logger logger-name :version ver :cache-file file
                 :capacity cap :clear-hook hook) opts)
       ((&struct :all-struct-fields-for-entries all-fields
                 :struct-name-for-entries s-name
                 :make-fn-constructor-for-entries ctor
                 :clear-fn clear-fn :save-fn save-fn :load-fn load-fn
                 :cache-var cache-var :version-id-var ver-id-var
                 :size-var size-var :inspect-cache-fn inspect-cache-fn
                 :inspect-entry-fn inspect-entry-fn
                 :invalidate-tags-fn inv-tags-fn
                 :inflight-var inflight-var) syms)
        (unique-all-fields (cl-remove-duplicates all-fields
                                                 :key #'car :test #'eq))
        ;; Determine the cache type at macro-expansion time.
        (cache-type (let ((type-sym (type-of instance)))
                      (cond ((eq type-sym 'cacheus-memoize-instance)
                             'memoize)
                            ((eq type-sym 'cacheus-cache-instance)
                             'cache)
                            (t (error "Unknown instance type: %S"
                                      type-sym))))))
    `(progn
       ;; 1. Define the cache-entry struct at compile-time.
       (eval-when-compile
         (unless (fboundp ',ctor)
           (cl-defstruct (,s-name (:constructor ,ctor))
             ,@unique-all-fields)))

       ;; 2. Define the core variables for this cache instance.
       (defvar ,cache-var (ht-create) ,(format "Cache for %S." name))
       (defvar ,size-var ,cap ,(format "Capacity for %S." name))
       (defvar ,ver-id-var ,ver ,(format "Functional version for %S." name))
       ,(when inflight-var `(defvar ,inflight-var (ht-create)
                              ,(format "In-flight requests for %S." name)))

       ;; 3. Register the cache in the global registry.
       (ht-set! cacheus-global-cache-registry ',name
                (list :name ',name
                      :config (cacheus-instance-options ',instance)
                      :symbols-struct (cacheus-instance-symbols ',instance)
                      :macro-time-instance ',instance
                      :instance-var-sym ',instance-var-sym
                      :clear-fn-symbol ',clear-fn
                      :save-fn-symbol ',save-fn
                      :load-fn-symbol ',load-fn
                      :invalidate-by-tags-fn-symbol ',inv-tags-fn
                      :type ',cache-type))

       ;; 4. Generate the administrative API functions.
       ,(when clear-fn
          (cacheus-make-clear-fn-form clear-fn instance-var-sym hook))
       ,(when save-fn
          (cacheus-make-save-fn-form save-fn instance-var-sym))
       ,(when load-fn
          (cacheus-make-load-fn-form load-fn instance-var-sym))
       ,(when inspect-cache-fn
          (cacheus-make-inspect-cache-fn-form inspect-cache-fn instance-var-sym))
       ,(when inspect-entry-fn
          (cacheus-make-inspect-entry-fn-form inspect-entry-fn instance-var-sym))
       ,(when inv-tags-fn
          (cacheus-make-invalidate-by-tags-fn-form inv-tags-fn instance-var-sym))

       ;; 5. Set up the initial load from a persistent file, if configured.
       ,(when (and load-fn file)
          `(eval-after-load 'cacheus
             '(lambda ()
                (let ((logger (cacheus-resolve-logger ,logger-name)))
                  (when (file-exists-p ,file)
                    (funcall logger :info "[C:%S] Initial load from %s"
                             ',name ,file)
                    (condition-case-unless-debug e (,load-fn)
                      (error (funcall logger :error
                                      "[C:%S] Initial load failed: %S"
                                      ',name e :trace)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Administrative Function Generators

(defun cacheus-make-clear-fn-form (fn-sym instance-var-sym hook)
  "Generate the `defun` form for a 'clear cache' function."
  `(defun ,fn-sym ()
     ,(format "Clear all entries from the '%S' cache." fn-sym)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (funcall logger :info "[C:%S] Clearing cache..." (cacheus-options-name opts))
       (cacheus-clear-runtime-data instance)
       (when ,hook
         (condition-case-unless-debug e (funcall ,hook)
           (error (funcall logger :error "[C:%S] clear-hook error: %S"
                           (cacheus-options-name opts) e :trace)))))
     ',fn-sym))

(defun cacheus-make-save-fn-form (fn-sym instance-var-sym)
  "Generate the `defun` form for a 'save cache' function."
  `(defun ,fn-sym ()
     ,(format "Save the '%S' cache to its file." fn-sym)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (progn
             (funcall logger :info "[C:%S] Saving to %s..."
                      (cacheus-options-name opts) file)
             (condition-case-unless-debug e
                 (cacheus-persist-cache-instance instance logger)
               (error (funcall logger :error "[C:%S] Save failed: %S"
                               (cacheus-options-name opts) e)
                      (signal 'cacheus-error (list "Save failed" e)))))
         (funcall logger :warn "[C:%S] Save: No :cache-file configured."
                  (cacheus-options-name opts))))
     ',fn-sym))

(defun cacheus-make-load-fn-form (fn-sym instance-var-sym)
  "Generate the `defun` form for a 'load cache' function."
  `(defun ,fn-sym ()
     ,(format "Load the '%S' cache from its file." fn-sym)
     (interactive)
     (let* ((instance (symbol-value ',instance-var-sym))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (progn
             (funcall logger :info "[C:%S] Loading from %s..."
                      (cacheus-options-name opts) file)
             (cacheus-load-cache-instance instance logger))
         (funcall logger :warn "[C:%S] Load: No :cache-file configured."
                  (cacheus-options-name opts))))
     ',fn-sym))

(defun cacheus-make-inspect-cache-fn-form (fn-sym instance-var-sym)
  "Generate a simple wrapper function to inspect an entire cache."
  `(defun ,fn-sym ()
     ,(format "Inspect all entries in the '%S' cache." fn-sym)
     (interactive)
     (cacheus-inspect-instance-dispatch (symbol-value ',instance-var-sym) nil)))

(defun cacheus-make-inspect-entry-fn-form (fn-sym instance-var-sym)
  "Generate a simple wrapper function to inspect a single cache entry."
  `(defun ,fn-sym (key)
     ,(format "Inspect a single entry in the '%S' cache by KEY." fn-sym)
     (interactive "sKey (as a Lisp string): ")
     (cacheus-inspect-instance-dispatch (symbol-value ',instance-var-sym) key)))

(defun cacheus-make-invalidate-by-tags-fn-form (fn-sym instance-var-sym)
  "Generate the `defun` form for an 'invalidate-by-tags' function."
  `(cl-defun ,fn-sym (tags &key (all-must-match nil) (run-hooks t))
     ,(format "Invalidate entries by TAGS for `%S`." fn-sym)
     (interactive (list (read-from-minibuffer "Invalidate Tags (Lisp form): ")))
     (cacheus-invalidate-keys-by-tags
      (symbol-value ',instance-var-sym)
      tags :all-must-match all-must-match :run-hooks run-hooks)
     ',fn-sym))

(provide 'cacheus-generators)
;;; cacheus-generators.el ends here