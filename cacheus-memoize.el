;;; cacheus-memoize.el --- A TTL-aware memoization framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file builds upon the core Cacheus framework to provide robust
;; memoization for Emacs Lisp functions. It offers both compile-time and
;; runtime memoization, integrating features like TTL-based expiration,
;; capacity management, persistence, and tagging.
;;
;; Key features provided by this library:
;; - `cacheus-memoize!`: The primary macro for defining a memoized function.
;;
;; - `cacheus-memoize-fn`: A function to memoize an *existing* function at
;;   runtime, returning a new lambda with helper functions attached.
;;
;; - `cacheus-macro-memoize!`: A specialized macro for memoizing the
;;   *expansion* of another macro at compile-time.
;;
;; Customization Options:
;; - `:key-fn`: A function `(lambda (args...))` to transform function arguments
;;   into a canonical cache key.
;; - `:predicate`: A function `(lambda (value))` that returns non-nil if a
;;   result is worth caching.
;; - Other options from `cacheus-cache!` are also supported, including `:ttl`,
;;   `:capacity`, `:version`, `:fields`, `:meta-fn`, and `:tags-fn`.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 's)
(require 'dash)

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

(defun cacheus-memoize--parse-args (original-args _macro-name)
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
      (error "cacheus: Options list is not a valid plist: %S"
             options-plist))
    (list docstring options-plist body-forms)))

(defun cacheus-memoize--create-options (name-sym options-plist)
  "Create and validate a `cacheus-memoize-options` struct."
  (let ((base-options (cacheus-create-options name-sym options-plist)))
    (-let-pattern* (((&plist :key-fn key-fn-opt :arglist arglist)
                     options-plist))
      (cacheus-validate-fn-option key-fn-opt :key-fn)
      (when (and arglist (not (listp arglist)))
        (error "cacheus-memoize: :arglist must be a list, got %S" arglist))
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

(defun cacheus-memoize--generate-symbols (options-struct)
  "Generate a `cacheus-symbols` struct for a memoized function."
  (let ((base-symbols (cacheus-generate-symbols options-struct)))
    (setf (cacheus-symbols-inflight-var base-symbols)
          (intern (format "%s-inflight-ht"
                          (cacheus-symbols-sym-prefix base-symbols))))
    base-symbols))

(defun cacheus-memoize--create-instance (name-sym options-list)
  "Create a `cacheus-memoize-instance` blueprint from macro arguments."
  (let* ((options (cacheus-memoize--create-options name-sym options-list))
         (symbols (cacheus-memoize--generate-symbols options)))
    (make-cacheus-memoize-instance
     :options options
     :symbols symbols
     :runtime-data (make-cacheus-runtime-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macros and Functions

;;;###autoload
(defmacro cacheus-memoize! (name args &rest body-and-options)
  "Define a memoized Emacs Lisp function `NAME`."
  (declare (indent 2))
  (let* ((parsed-args (cacheus-memoize--parse-args
                       body-and-options 'cacheus-memoize!))
         (docstring (car parsed-args))
         (opts-plist (cadr parsed-args))
         (body (caddr parsed-args))
         (final-opts (if (plist-member opts-plist :name)
                         opts-plist
                       (plist-put opts-plist :name name)))
         (instance (cacheus-memoize--create-instance name final-opts))
         (symbols (cacheus-instance-symbols instance))
         (options (cacheus-instance-options instance))
         (get-fn-sym (cacheus-symbols-get-fn symbols))
         (key-fn-opt (cacheus-memoize-options-key-fn options)))
    (unless (listp args) (error "`ARGS` must be a list, got %S" args))
    (let ((key-fn (or key-fn-opt `(lambda ,args (list ,@args)))))
      `(progn
         ,(cacheus-make-cache-backend
           instance
           :instance-constructor 'make-cacheus-memoize-instance)
         (defun ,name ,args
           ,(or docstring (format "Memoized version of %S." name))
           (funcall #',get-fn-sym
                    (funcall ,key-fn ,@args)
                    t
                    (list ,@args)
                    (lambda () ,@body)))
         ',name))))

;;;###autoload
(defmacro cacheus-macro-memoize! (name args &rest body-and-options)
  "Define a macro `NAME` that memoizes its own expansion at compile time."
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
This allows for runtime memoization of arbitrary functions. The returned
lambda will have helper functions (clear, save, inspect) attached as
function properties for programmatic interaction.

The `:arglist` option is required so the memoized function can have a
correct signature.

Arguments:
- `FN`: The function or function symbol to memoize.
- `OPTIONS-PLIST`: A plist of keyword options, such as `:ttl`, `:capacity`,
  `:key-fn`, and the required `:arglist`.

Returns:
A new function (a lambda closure) that acts as the memoized version of `FN`."
  (let* ((final-opts (if (plist-member options-plist :name)
                         options-plist
                       (plist-put options-plist :name fn)))
         (instance (cacheus-memoize--create-instance fn final-opts))
         (opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (arglist (cacheus-memoize-options-arglist opts))
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
                         ;; This thunk executes the original function on a cache miss.
                         (lambda () (apply fn args))
                         :user-key args
                         ;; Runtime memoization does not currently support async.
                         :async (cacheus-options-async opts))))
                    args))))
      ;; Attach helper functions as properties to the returned lambda.
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
      memoized-lambda)))

(provide 'cacheus-memoize)
;;; cacheus-memoize.el ends here