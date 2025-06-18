;;; cacheus-memoize.el --- A TTL-aware memoization framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the `cacheus-memoize!` macro, a user-friendly way to
;; define memoized functions. It builds upon the core framework to handle
;; TTL-based expiration, capacity management, persistence, and tagging.

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
  "Configuration options for a memoized function.

Fields:
  key-fn (function): A function that computes a cache key from the
    function's arguments. Defaults to `list`.
  arglist (list): The argument list of the function being memoized.
    Required for `cacheus-memoize-fn`."
  key-fn
  arglist)

(cl-defstruct (cacheus-memoize-instance (:include cacheus-instance))
  "A type-specific instance struct for memoized functions.

Fields:
  options (cacheus-memoize-options): Inherited/specialized configuration.
  symbols (cacheus-symbols): Inherited. Generated names for vars/functions.
  runtime-data (cacheus-runtime-data): Inherited. Live cache data.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (error "cacheus-memoize!: Options are not a valid plist: %S" opts))
    (list docstring opts body)))

(defun cacheus-memoize--create-options (name-sym options-plist)
  "Create and validate a `cacheus-memoize-options` struct."
  (let ((base-opts (cacheus-create-options name-sym options-plist)))
    (cacheus-let* (((&plist :key-fn key-fn :arglist arglist) options-plist))
      (cacheus-validate-fn-option key-fn :key-fn)
      (when (and arglist (not (listp arglist)))
        (error "cacheus-memoize: :arglist must be a list, got %S" arglist))
      ;; This is verbose because `cl-defstruct :include` does not provide
      ;; a convenient way to copy all parent slots in the constructor.
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
     :options options
     :symbols symbols
     :runtime-data nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macros and Functions

;;;###autoload
(defmacro cacheus-memoize! (name args &rest body-and-options)
  "Define a memoized Emacs Lisp function `NAME`.

This macro transforms a normal `defun` into a memoized one,
wrapping it with the Cacheus framework. The function's return
values will be cached based on its arguments.

Arguments:
  NAME (symbol): The name of the function to define.
  ARGS (list): The argument list for the function, like in `defun`.
  BODY-AND-OPTIONS...: An optional docstring, followed by a plist
    of cache options, followed by the function body.

Example:
  (cacheus-memoize! fib (n)
    \"Compute the Nth Fibonacci number.\"
    :ttl 3600  ; Cache results for 1 hour
    (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

Returns:
  (symbol) The `NAME` of the defined function."
  (declare (indent 1))
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
         (cacheus-get-or-compute
          (symbol-value ',ivar)
          (funcall ,key-fn ,@args)
          (lambda () ,@body)
          :user-key (list ,@args)
          :async ,async))
       ',name)))

;;;###autoload
(defmacro cacheus-macro-memoize! (name args &rest body-and-options)
  "Define a macro `NAME` that memoizes its own expansion at compile time.

This is a specialized, lightweight memoizer for macro-expansions.
It is useful for macros that perform expensive computations during
compilation. The cache is simple and only lasts for the duration
of the compilation process.

Arguments:
  NAME (symbol): The name of the macro to define.
  ARGS (list): The argument list for the macro.
  BODY-AND-OPTIONS...: An optional docstring, plist of options, and body.
    Supported options are `:key-fn` and `:version`.

Returns:
  (symbol) The `NAME` of the defined macro."
  (declare (indent 2))
  (let* ((parsed (cacheus-memoize--parse-args body-and-options))
         (docstring (car parsed))
         (opts (cadr parsed))
         (body (caddr parsed))
         (key-fn (or (plist-get opts :key-fn) `(lambda ,args (list ,@args))))
         (version (plist-get opts :version))
         (doc (or docstring (format "Memoized macro `%s`." name)))
         (cache-var (gensym (format "%s-expansion-cache-" name))))
    (unless (listp args) (error "ARGLIST must be a list for %s" name))
    `(progn
       (defvar ,cache-var (ht-create)
         ,(format "Macro expansion cache for `%s`." name))
       (defmacro ,name ,args
         ,doc
         (let* ((ukey (funcall #',key-fn ,@args))
                (ekey (if ,version (list ukey ,version) ukey)))
           (or (ht-get (symbol-value ',cache-var) ekey)
               (let ((expansion (progn ,@body)))
                 (ht-set! (symbol-value ',cache-var) ekey expansion)
                 expansion)))))))

;;;###autoload
(defun cacheus-memoize-fn (fn &rest options-plist)
  "Return a memoized version of an existing function `FN` at runtime.

This is useful for memoizing functions you don't own, or for
creating memoized lambdas dynamically. The `:arglist` option is
required.

Arguments:
  FN (function): The function to memoize.
  OPTIONS-PLIST (plist): A plist of cache options. `:arglist` is
    required.

Returns:
  (function) A new anonymous function (lambda) that is the
  memoized version of `FN`."
  (let* ((final-opts (if (plist-member options-plist :name)
                         options-plist (plist-put options-plist :name fn)))
         (instance (cacheus-memoize--create-instance fn final-opts))
         (opts (cacheus-instance-options instance))
         (arglist (cacheus-memoize-options-arglist opts))
         (async (cacheus-options-async opts))
         (key-fn (or (cacheus-memoize-options-key-fn opts)
                     `(lambda ,arglist (list ,@arglist)))))
    (unless arglist
      (error "cacheus-memoize-fn: The :arglist option is required"))
    (let ((memoized-lambda
           (lambda (&rest args)
             (apply (lambda ,arglist
                      (let ((key (funcall key-fn ,@arglist)))
                        (cacheus-get-or-compute
                         instance key (lambda () (apply fn args))
                         :user-key (list ,@arglist) :async async)))
                    args))))
      ;; Attach helper functions directly to the lambda's property list
      ;; to provide an object-like interface for runtime-created caches.
      (put memoized-lambda 'cache-instance-name (cacheus-options-name opts))
      (put memoized-lambda 'clear-cache
           (lambda ()
             (interactive)
             (cacheus-clear-runtime-data instance)))
      (when (cacheus-options-cache-file opts)
        (put memoized-lambda 'save-cache
             (lambda ()
               (interactive)
               (cacheus-persist-cache-instance
                instance
                (cacheus-resolve-logger (cacheus-options-logger opts))))))
      (put memoized-lambda 'invalidate-by-tags
           (lambda (tags &key all-must-match run-hooks)
             (cacheus-invalidate-keys-by-tags
              instance tags
              :all-must-match all-must-match
              :run-hooks run-hooks)))
      memoized-lambda)))

(provide 'cacheus-memoize)
;;; cacheus-memoize.el ends here