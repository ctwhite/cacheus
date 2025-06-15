;;; cacheus-memoize.el --- A TTL-aware memoization framework -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file provides the `cacheus-memoize!` macro, a user-friendly way to
;; define memoized functions. It builds upon the core framework to handle
;; TTL-based expiration, capacity management, persistence, and tagging.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 's)
(require 'dash)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module-Specific Definitions

(define-error 'cacheus-memoize-error
  "Cacheus-memoize specific error."
  'cacheus-error)

(cl-defstruct (cacheus-memoize-options (:include cacheus-options))
  "Configuration options for a memoized function."
  key-fn
  arglist)

(cl-defstruct (cacheus-memoize-instance (:include cacheus-instance))
  "A type-specific instance struct for memoized functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun cacheus-memoize--parse-args (original-args macro-name)
  "Parse `(docstring? options* body*)` from a macro's arguments."
  (let (docstring options-plist body-forms
        (args (copy-sequence original-args)))
    (when (stringp (car args))
      (setq docstring (pop args)))
    (let ((options-len 0))
      (while (and (< options-len (length args))
                  (keywordp (nth options-len args))
                  (< (1+ options-len) (length args)))
        (cl-incf options-len 2))
      (setq options-plist (cl-subseq args 0 options-len))
      (setq body-forms (nthcdr options-len args)))
    (unless (cacheus-plist-valid-p options-plist)
      (error "%S: Options list is not a valid plist: %S"
             macro-name options-plist))
    (list docstring options-plist body-forms)))

(defun cacheus-memoize--create-options (name-sym options-plist)
  "Create and validate a `cacheus-memoize-options` struct."
  (let ((base-options (cacheus-create-options name-sym options-plist)))
    (cacheus-let* (((&plist :key-fn key-fn-opt :arglist arglist)
                    options-plist))
      (cacheus-validate-fn-option key-fn-opt :key-fn)
      (when (and arglist (not (listp arglist)))
        (error "cacheus-memoize: :arglist must be a list, got %S"
               arglist))
      (make-cacheus-memoize-options
       :key-fn key-fn-opt
       :arglist arglist
       :name (cacheus-options-name base-options)
       :logger (cacheus-options-logger base-options)
       :capacity (cacheus-options-capacity base-options)
       :eviction-strategy (cacheus-options-eviction-strategy base-options)
       :cache-file (cacheus-options-cache-file base-options)
       :version (cacheus-options-version base-options)
       :periodic-cleanup (cacheus-options-periodic-cleanup base-options)
       :predicate (cacheus-options-predicate base-options)
       :async (cacheus-options-async base-options)
       :error-handler (cacheus-options-error-handler base-options)
       :ttl (cacheus-options-ttl base-options)
       :refresh-ttl (cacheus-options-refresh-ttl base-options)
       :expiration-hook (cacheus-options-expiration-hook base-options)
       :dirty-p (cacheus-options-dirty-p base-options)
       :clear-hook (cacheus-options-clear-hook base-options)
       :prefix (cacheus-options-prefix base-options)
       :fields-data (cacheus-options-fields-data base-options)
       :meta-fn (cacheus-options-meta-fn base-options)
       :tags-fn (cacheus-options-tags-fn base-options)))))

(defun cacheus-memoize--create-instance (name-sym options-list)
  "Create a `cacheus-memoize-instance` blueprint from macro arguments."
  (let* ((options (cacheus-memoize--create-options name-sym options-list))
         (symbols (cacheus-generate-symbols options)))
    (make-cacheus-memoize-instance
     :options options
     :symbols symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macros and Functions

;;;###autoload
(defmacro cacheus-memoize! (name args &rest body-and-options)
  "Define a memoized Emacs Lisp function `NAME`."
  (declare (indent 1))
  (let* ((parsed-args (cacheus-memoize--parse-args
                       body-and-options 'cacheus-memoize!))
         (docstring (car parsed-args))
         (opts-plist (cadr parsed-args))
         (body (caddr parsed-args))
         (final-opts (if (plist-member opts-plist :name)
                         opts-plist
                       (plist-put opts-plist :name name)))
         (instance (cacheus-memoize--create-instance name final-opts))
         (options (cacheus-instance-options instance))
         (key-fn-opt (cacheus-memoize-options-key-fn options))
         (async-opt (cacheus-options-async options))
         (instance-var (intern (format "cacheus--%s-instance" (symbol-name name)))))
    (unless (listp args) (error "`ARGS` must be a list, got %S" args))
    (let* ((key-fn (or key-fn-opt `(lambda ,args (list ,@args))))
           (backend-progn (cacheus-make-cache-backend instance instance-var)))
      `(progn
         ,backend-progn
         (defvar ,instance-var ',instance)
         (defun ,name ,args
           ,(or docstring (format "Memoized version of %S." name))
           ;; The call to `cacheus--get-instance-by-name` ensures that the
           ;; cache is lazily initialized on the first call.
           (cacheus-get-or-compute
            (cacheus--get-instance-by-name ',name)
            (funcall ,key-fn ,@args)
            (lambda () ,@body)
            :user-key (list ,@args)
            :async ,async-opt))
         ',name))))

;;;###autoload
(defmacro cacheus-macro-memoize! (name args &rest body-and-options)
  "Define a macro `NAME` that memoizes its own expansion at compile time.
This is useful for complex macros whose expansion is computationally
expensive and deterministic based on their arguments. The memoization happens
during Emacs's compilation phase.

Arguments:
- `NAME` (symbol): The symbol for the new memoized macro.
- `ARGS` (list): A list of arguments for the macro, like `defmacro`.
- `BODY-AND-OPTIONS` (list): The body of the macro, optionally preceded by a
  docstring and a plist of options.

Returns:
  (symbol): The `NAME` of the macro just defined."
  (declare (indent 2))
  (let* ((parsed-args (cacheus-memoize--parse-args
                       body-and-options 'cacheus-macro-memoize!))
         (docstring (car parsed-args))
         (opts-plist (cadr parsed-args))
         (body (caddr parsed-args))
         (key-fn-opt (plist-get opts-plist :key-fn))
         (version-opt (plist-get opts-plist :version))
         (doc (or docstring (format "Memoized macro `%S`." name)))
         (cache-var (gensym (format "%S-expansion-cache-" name))))
    (unless (listp args) (error "ARGLIST must be a list for %S" name))
    (let ((key-fn (or key-fn-opt `(lambda ,args (list ,@args)))))
      `(progn
         (defvar ,cache-var (ht-create)
           ,(format "Macro expansion cache for `%S`." name))
         (defmacro ,name ,args
           ,doc
           (let* ((user-key (funcall #',key-fn ,@args))
                  (effective-key (if ,version-opt
                                     (list user-key ,version-opt)
                                   user-key)))
             (or (ht-get (symbol-value ',cache-var) effective-key)
                 (let ((expansion (progn ,@body)))
                   (ht-set! (symbol-value ',cache-var) effective-key expansion)
                   expansion))))))))

;;;###autoload
(defun cacheus-memoize-fn (fn &rest options-plist)
  "Return a memoized version of an existing function `FN` at runtime.
This allows you to take any Emacs Lisp function `FN` and wrap it with
memoization capabilities. This is useful for functions that cannot be
defined with `cacheus-memoize!` directly.

Arguments:
- `FN` (function): The function (symbol or lambda) to be memoized.
- `OPTIONS-PLIST` (plist): Options to configure the memoized function.
  Crucially, `:arglist` must be provided.

Returns:
  (function): A new lambda function that wraps `FN` with memoization."
  (let* ((final-opts (if (plist-member options-plist :name)
                         options-plist
                       (plist-put options-plist :name fn)))
         (instance (cacheus-memoize--create-instance fn final-opts))
         (opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (arglist (cacheus-memoize-options-arglist opts))
         (async-opt (cacheus-options-async opts))
         (key-fn (or (cacheus-memoize-options-key-fn opts)
                     `(lambda ,arglist (list ,@arglist)))))
    (unless arglist
      (error "cacheus-memoize-fn: The :arglist option is required"))
    (let ((memoized-lambda
           (lambda (&rest args)
             (apply (lambda ,arglist
                      (let ((key (funcall key-fn ,@arglist)))
                        (cacheus-get-or-compute
                         instance
                         key
                         (lambda () (apply fn args))
                         :user-key (list ,@arglist)
                         :async async-opt)))
                    args)))
      ;; Attach helper functions as properties to the memoized lambda.
      (let* ((clear-fn-sym (cacheus-symbols-clear-fn syms))
             (save-fn-sym (cacheus-symbols-save-fn syms))
             (inv-fn-sym (cacheus-symbols-invalidate-tags-fn syms))
             (insp-fn-sym (cacheus-symbols-inspect-cache-fn syms))
             (insp-entry-fn-sym (cacheus-symbols-inspect-entry-fn syms)))
        (fset clear-fn-sym
              (lambda () (interactive) (cacheus-clear-runtime-data instance)))
        (when save-fn-sym
          (fset save-fn-sym
                (lambda () (interactive)
                  (cacheus-persist-cache-instance
                   instance
                   (cacheus-resolve-logger (cacheus-options-logger opts))))))
        (when inv-fn-sym
          (fset inv-fn-sym
                (lambda (tags &key all-must-match run-hooks)
                  (cacheus-invalidate-keys-by-tags
                   instance tags
                   :all-must-match all-must-match :run-hooks run-hooks))))
        (when insp-fn-sym
          (fset insp-fn-sym
                (lambda ()
                  (interactive)
                  (cacheus-inspect-instance-dispatch instance nil))))
        (when insp-entry-fn-sym
          (fset insp-entry-fn-sym
                (lambda (key) (interactive "sKey:")
                  (cacheus-inspect-instance-dispatch instance key))))
        (put memoized-lambda 'clear-cache clear-fn-sym)
        (put memoized-lambda 'save-cache save-fn-sym)
        (put memoized-lambda 'invalidate-by-tags inv-fn-sym)
        (put memoized-lambda 'inspect-cache insp-fn-sym)
        (put memoized-lambda 'inspect-entry insp-entry-fn-sym)
        (put memoized-lambda 'cache-instance-name
             (cacheus-options-name opts)))
      memoized-lambda))))

(provide 'cacheus-memoize)
;;; cacheus-memoize.el ends here