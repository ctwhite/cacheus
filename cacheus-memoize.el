;;; cacheus-memoize.el --- A TTL-aware memoization framework -*-
;;; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the `cacheus:memoize!` macro, a user-friendly way to
;; define memoized functions. It builds upon the core framework to handle
;; TTL-based expiration, capacity management, persistence, and tagging.

;;; Code:

(require 'cl-lib)
(require 's)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-core)
(require 'cacheus-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module-Specific Definitions

(define-error 'cacheus-memoize-error
  "Cacheus-memoize specific error."
  'cacheus-error)

(cl-defstruct (cacheus-memoize-options (:include cacheus-options))
  "Configuration options for a memoized function.

Fields:
- `key-fn` (function): A function that computes a cache key from the
  function's arguments. Defaults to `list`.
- `arglist` (list): The argument list of the function being memoized.
  Required for `cacheus:memoize-fn`."
  key-fn
  arglist)

(cl-defstruct (cacheus-memoize-instance (:include cacheus-instance))
  "A type-specific instance struct for memoized functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers (File-Local)

(defun cacheus-memoize--parse-args (original-args)
  "Parse `(docstring? options* body*)` from a macro's arguments."
  (let (docstring opts body (args (copy-sequence original-args)))
    (when (stringp (car args)) (setq docstring (pop args)))
    (let ((len 0))
      (while (and (< len (length args)) (keywordp (nth len args))
                  (< (1+ len) (length args)))
        (cl-incf len 2))
      (setq opts (cl-subseq args 0 len))
      (setq body (nthcdr len args)))
    (unless (cacheus-plist-valid-p opts)
      (error "cacheus:memoize!: Options are not a valid plist: %S" opts))
    (list docstring opts body)))

(defun cacheus-memoize--create-options (name-sym options-plist)
  "Create and validate a `cacheus-memoize-options` struct."
  (let ((base-opts (cacheus-create-options name-sym options-plist)))
    (let ((key-fn (plist-get options-plist :key-fn))
          (arglist (plist-get options-plist :arglist)))
      (cacheus-validate-fn-option key-fn :key-fn)
      (make-cacheus-memoize-options
       :key-fn key-fn :arglist arglist
       :name (cacheus-options-name base-opts)
       :logger (cacheus-options-logger base-opts)
       :capacity (cacheus-options-capacity base-opts)
       :eviction-strategy (cacheus-options-eviction-strategy base-opts)
       :cache-file (cacheus-options-cache-file base-opts)
       :version (cacheus-options-version base-opts)
       :periodic-cleanup (cacheus-options-periodic-cleanup base-opts)
       :predicate (cacheus-options-predicate base-opts)
       :async (cacheus-options-async base-opts)
       :error-handler (cacheus-options-error-handler base-opts)
       :ttl (cacheus-options-ttl base-opts)
       :refresh-ttl (cacheus-options-refresh-ttl base-opts)
       :expiration-hook (cacheus-options-expiration-hook base-opts)
       :dirty-p (cacheus-options-dirty-p base-opts)
       :clear-hook (cacheus-options-clear-hook base-opts)
       :prefix (cacheus-options-prefix base-opts)
       :fields-data (cacheus-options-fields-data base-opts)
       :meta-fn (cacheus-options-meta-fn base-opts)
       :tags-fn (cacheus-options-tags-fn base-opts)))))

(defun cacheus-memoize--create-instance (name-sym options-plist)
  "Create a `cacheus-memoize-instance` blueprint from macro arguments."
  (let* ((options (cacheus-memoize--create-options name-sym options-plist))
         (symbols (cacheus-generate-symbols options)))
    (make-cacheus-memoize-instance
     :options options :symbols symbols :runtime-data nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macros and Functions

;;;###autoload
(defmacro cacheus:memoize! (name args &rest body-and-options)
  "Define a memoized Emacs Lisp function `NAME`.
This macro transforms a normal `defun` into a memoized one,
wrapping it with the Cacheus framework. The function's return
values will be cached based on its arguments.

Arguments:
- `NAME` (symbol): The name of the function to define.
- `ARGS` (list): The argument list for the function, like in `defun`.
- `BODY-AND-OPTIONS...`: An optional docstring, followed by a plist
  of cache options (see `cacheus-options` struct), followed by the
  function body forms.

Example:
  (cacheus:memoize! fib (n)
    \"Compute the Nth Fibonacci number.\"
    :ttl 3600  ; Cache results for 1 hour
    (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

Returns:
  (symbol) The `NAME` of the defined function."
  (declare (indent defun))
  (let* ((parsed (cacheus-memoize--parse-args body-and-options))
         (docstring (car parsed))
         (opts (cadr parsed))
         (body (caddr parsed))
         (instance (cacheus-memoize--create-instance name opts))
         (options (cacheus-instance-options instance))
         (key-fn (or (cacheus-memoize-options-key-fn options)
                     `(lambda ,args (list ,@args))))
         (async (cacheus-options-async options))
         (ivar (intern (format "cacheus--%s-instance" (symbol-name name)))))
    (unless (listp args) (error "`ARGS` must be a list, got %S" args))
    `(progn
       ,(cacheus-make-cache-backend instance ivar)
       (defun ,name ,args
         ,(or docstring (format "Memoized version of %s." name))
         (let* ((user-key (funcall ,key-fn ,@args))
                (instance-var (symbol-value ',ivar))
                (ver (cacheus-options-version (cacheus-instance-options
                                               instance-var)))
                (eff-key (if ver (list user-key ver) user-key)))
           (cacheus-get-or-compute
            instance-var eff-key (lambda () ,@body)
            :user-key user-key :async ,async)))
       ',name)))

;;;###autoload
(defun cacheus:memoize-fn (fn &rest options-plist)
  "Return a memoized version of an existing function `FN` at runtime.
This is useful for memoizing functions you don't own, or for
creating memoized lambdas dynamically. The `:arglist` option is
required so the memoized function can have the correct signature.

Arguments:
- `FN` (function): The function to memoize.
- `OPTIONS-PLIST` (plist): A plist of cache options. `:arglist` is required.

Returns:
  (function) A new anonymous function (lambda) that is the
  memoized version of `FN`."
  (let* ((name (or (plist-get options-plist :name) fn))
         (instance (cacheus-memoize--create-instance name options-plist))
         (opts (cacheus-instance-options instance))
         (arglist (cacheus-memoize-options-arglist opts))
         (async (cacheus-options-async opts))
         (key-fn (or (cacheus-memoize-options-key-fn opts)
                     `(lambda ,arglist (list ,@arglist)))))
    (unless arglist
      (error "cacheus:memoize-fn: The :arglist option is required"))
    (let ((memoized-lambda
           (lambda (&rest args)
             (apply (lambda ,arglist
                      (let* ((user-key (funcall key-fn ,@arglist))
                             (ver (cacheus-options-version opts))
                             (eff-key (if ver (list user-key ver) user-key)))
                        (cacheus-get-or-compute
                         instance eff-key (lambda () (apply fn args))
                         :user-key user-key :async async)))
                    args))))
      ;; Attach helper functions directly to the lambda's property list.
      (put memoized-lambda 'clear-cache
           (lambda () (interactive) (cacheus-clear-runtime-data instance)))
      memoized-lambda)))

(provide 'cacheus-memoize)
;;; cacheus-memoize.el ends here