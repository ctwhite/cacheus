;;; cacheus-generators.el --- Generic function generators for Cacheus -*-
;;; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is the code-generation engine for the Cacheus framework. It
;; contains the logic for building the `defvar` boilerplate and administrative
;; functions (e.g., -save, -clear, -load) for any new cache defined via
;; the `defcacheus` macro.
;;
;; Public API interaction (get/put) is handled by universal functions in
;; `cacheus-core.el`. This module focuses solely on generating the
;; cache-specific backend code.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'ts)
(require 'dash)
(require 'concur-core nil t)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-persistence)
(require 'cacheus-storage)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primary Backend Generator (Package-Private)

(defun cacheus-make-cache-backend (instance instance-var-sym)
  "Generate the complete backend code for a cache INSTANCE.
This is the central code-generation engine for the Cacheus framework,
called by the `defcacheus` macro. It constructs a large `progn` form
containing all necessary `defvar` and `defun` forms for a given
cache configuration.

Arguments:
- `INSTANCE` (cacheus-instance): The template instance struct for the cache.
- `INSTANCE-VAR-SYM` (symbol): The symbol for the cache's global instance
  variable (e.g., `my-cache--instance`).

Returns:
  (list) A Lisp form (a `progn` block) ready for evaluation."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (name (cacheus-options-name opts))
         (file (cacheus-options-cache-file opts))
         (lopt (cacheus-options-logger opts))
         (fields (cacheus-symbols-all-struct-fields-for-entries syms))
         (s-name (cacheus-symbols-struct-name-for-entries syms))
         (ctor (cacheus-symbols-make-fn-constructor-for-entries syms))
         (clear-fn (cacheus-symbols-clear-fn syms))
         (save-fn (cacheus-symbols-save-fn syms))
         (load-fn (cacheus-symbols-load-fn syms))
         (insp-cache-fn (cacheus-symbols-inspect-fn syms))
         (inv-tags-fn (cacheus-symbols-invalidate-tags-fn syms))
         (cache-type (pcase (type-of instance)
                       ('cacheus-memoize-instance 'memoize)
                       ('cacheus-cache-instance 'cache)
                       (t (error "Unknown instance type: %S"
                                 (type-of instance))))))
    `(progn
       ;; Define the per-entry struct at compile time so the compiler knows
       ;; its shape and accessors when compiling the generated functions.
       (eval-when-compile
         (unless (fboundp ',ctor)
           (cl-defstruct (,s-name (:constructor ,ctor)) ,@fields)))

       ;; Define the global instance variable for this cache.
       ;; It holds the "template" instance until first use.
       (defvar ,instance-var-sym ,instance
         ,(format "The global instance struct for the '%s' cache." name))

       ;; Register the cache in the global registry, making it discoverable
       ;; by universal functions like `cacheus:get!` and `cacheus:list-all-caches`.
       (puthash ',name
                (list :name ',name
                      :type ',cache-type
                      :config (cacheus-instance-options ,instance-var-sym)
                      :symbols-struct (cacheus-instance-symbols
                                       ,instance-var-sym)
                      :instance-var-sym ',instance-var-sym
                      :clear-fn-symbol ',clear-fn
                      :save-fn-symbol ',save-fn
                      :load-fn-symbol ',load-fn
                      :inspect-fn-symbol ',insp-cache-fn
                      :invalidate-by-tags-fn-symbol ',inv-tags-fn)
                cacheus-global-cache-registry)

       ;; Generate administrative functions for this specific cache.
       ,(cacheus--make-clear-fn-form clear-fn name)
       ,(cacheus--make-save-fn-form save-fn name)
       ,(cacheus--make-load-fn-form load-fn name)
       ,(cacheus--make-inspect-cache-fn-form insp-cache-fn name)
       ,(cacheus--make-invalidate-by-tags-fn-form inv-tags-fn name)

       ;; If a file is specified, schedule an initial load operation.
       ,(when (and load-fn file)
          `(eval-after-load 'cacheus-core
             '(lambda ()
                (let ((logger (cacheus-resolve-logger ,lopt)))
                  (when (file-exists-p ,file)
                    (funcall logger :info "[C:%s] Initial load from %s"
                             ',name ,file)
                    (condition-case e (,load-fn)
                      (error
                       (funcall logger :error "[C:%s] Initial load failed: %S"
                                ',name e)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Administrative Function Generators (File-Local)

(defun cacheus--make-clear-fn-form (fn-sym cache-name)
  "Generate the `defun` form for a 'clear cache' function."
  `(defun ,fn-sym ()
     ,(format "Clear all entries from the '%s' cache." cache-name)
     (interactive)
     (let* ((instance (cacheus-get-instance-by-name ',cache-name))
            (opts (cacheus-instance-options instance))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
            (hook (cacheus-options-clear-hook opts)))
       (funcall logger :info "[C:%s] Clearing cache..." ',cache-name)
       (cacheus-clear-runtime-data instance)
       (when hook
         (condition-case e (funcall hook)
           (error (funcall logger :error "[C:%s] clear-hook error: %S"
                           ',cache-name e)))))))

(defun cacheus--make-save-fn-form (fn-sym cache-name)
  "Generate the `defun` form for a 'save cache' function."
  `(defun ,fn-sym ()
     ,(format "Save the '%s' cache to its file." cache-name)
     (interactive)
     (let* ((instance (cacheus-get-instance-by-name ',cache-name))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (condition-case e
               (progn
                 (funcall logger :info "[C:%s] Saving to %s..."
                          ',cache-name file)
                 (cacheus-persist-cache-instance instance logger))
             (error
              (funcall logger :error "[C:%s] Save failed: %S" ',cache-name e)
              (signal 'cacheus-error (list "Save failed" e))))
         (funcall logger :warn
                  "[C:%s] Save failed: No :cache-file configured."
                  ',cache-name)))))

(defun cacheus--make-load-fn-form (fn-sym cache-name)
  "Generate the `defun` form for a 'load cache' function."
  `(defun ,fn-sym ()
     ,(format "Load the '%s' cache from its file." cache-name)
     (interactive)
     (let* ((instance (cacheus-get-instance-by-name ',cache-name))
            (opts (cacheus-instance-options instance))
            (file (cacheus-options-cache-file opts))
            (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
       (if file
           (if (file-readable-p file)
               (progn
                 (funcall logger :info "[C:%s] Loading from %s..."
                          ',cache-name file)
                 (cacheus-load-cache-instance instance logger))
             (funcall logger :warn
                      "[C:%s] Load failed: File not readable: %s"
                      ',cache-name file))
         (funcall logger :warn
                  "[C:%s] Load failed: No :cache-file configured."
                  ',cache-name)))))

(defun cacheus--make-inspect-cache-fn-form (fn-sym cache-name)
  "Generate a wrapper function to inspect an entire cache."
  `(defun ,fn-sym ()
     ,(format "Inspect all entries in the '%s' cache." cache-name)
     (interactive)
     (cacheus-inspect-instance-dispatch
      (cacheus-get-instance-by-name ',cache-name)
      nil)))

(defun cacheus--make-invalidate-by-tags-fn-form (fn-sym cache-name)
  "Generate the `defun` form for an 'invalidate-by-tags' function."
  `(cl-defun ,fn-sym (tags &key (all-must-match nil) (run-hooks t))
     ,(format "Invalidate entries by TAGS for '%s'.
TAGS should be a list of symbols, e.g., '(tag1 tag2).
When called interactively, you will be prompted for a Lisp
form, so you should enter a quoted list: ''(tag1 tag2)

Arguments:
- `TAGS` (list): A list of tag symbols to invalidate.
- `:ALL-MUST-MATCH` (boolean): If non-nil, only entries that
  have *all* of the specified tags will be invalidated.
  If nil (default), entries with *any* of the tags are invalidated.
- `:RUN-HOOKS` (boolean): If non-nil (default), run the cache's
  `:expiration-hook` for each invalidated entry." cache-name)
     (interactive
      (list (read-from-minibuffer "Invalidate Tags (Lisp form): ")))
     (when tags
       (cacheus-invalidate-keys-by-tags
        (cacheus-get-instance-by-name ',cache-name)
        tags
        :all-must-match all-must-match
        :run-hooks run-hooks))))

(provide 'cacheus-generators)
;;; cacheus-generators.el ends here