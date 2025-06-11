;;; cacheus-util.el --- Core utilities for the Cacheus framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is the home for fundamental utility functions and macros that are
;; used across the Cacheus caching framework. It centralizes common operations
;; to promote code reuse, consistency, and a clean architecture.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'json)
(require 's)
(require 'ring)
(require 'dash)

(require 'cacheus-structs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging Utilities

(defun cacheus-resolve-logger (logger-opt)
  "Resolve `LOGGER-OPT` to a callable logger function.

Arguments:
- `LOGGER-OPT`: Can be `t` (for `message`), `nil` (for a no-op function),
  a function symbol, or a lambda.

Returns:
A callable logger function that accepts `(LEVEL FORMAT &rest ARGS)`."
  (cond
   ((eq logger-opt t) #'message)
   ((null logger-opt) (lambda (&rest _args) nil))
   ((functionp logger-opt) logger-opt)
   ((symbolp logger-opt)
    (let ((resolved-func nil))
      (if (and (symbolp logger-opt) (boundp logger-opt))
          (let ((val (symbol-value logger-opt)))
            (if (functionp val)
                (setq resolved-func val))))
      (if resolved-func
          resolved-func
        (if (fboundp logger-opt)
            (symbol-function logger-opt)
          (warn "cacheus: Logger %S not fbound or its value is not a function; using no-op." logger-opt)
          (lambda (&rest _args) nil))))
   (t (warn "cacheus: Invalid logger option %S; using no-op." logger-opt)
      (lambda (&rest _args) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Serialization Utilities

(defun cacheus-stringify-key (key logger)
  "Convert an arbitrary Elisp `KEY` into a JSON-compatible string.
This is used for persisting keys to disk in cache files. It handles
common types like strings, symbols, and numbers directly, and uses
`prin1-to-string` as a general-purpose fallback.

Arguments:
- `KEY`: The Elisp key to stringify.
- `LOGGER`: A resolved logger function for error reporting.

Returns:
A string representation of `KEY`, or `nil` on failure."
  (condition-case-unless-debug err
      (typecase key
        (string key)
        (symbol (symbol-name key))
        (number (number-to-string key))
        (t (prin1-to-string key)))
    (error
     (funcall logger :error "stringify-key: Failed for key %S: %S" key err)
     nil)))

(defun cacheus-parse-key (skey logger)
  "Parse a stringified key `SKEY` back into its original Elisp form.
This is the reverse of `cacheus-stringify-key`.

Arguments:
- `SKEY`: The stringified key from a cache file.
- `LOGGER`: A resolved logger function for error reporting.

Returns:
The parsed Elisp key, or `nil` on failure."
  (condition-case-unless-debug err
      (read skey)
    (error
     (funcall logger :error "parse-key: Failed for string '%s': %S" skey err)
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument and Struct Utilities

(defun cacheus-plist-valid-p (plist)
  "Return `t` if `PLIST` is a well-formed property list.
A well-formed plist must have an even number of elements, and every
other element starting from the first must be a keyword.

Arguments:
- `PLIST`: The list to check.

Returns:
`t` if `PLIST` is a valid property list, `nil` otherwise."
  (or (null plist)
      (and (zerop (% (length plist) 2))
           (cl-loop for i from 0 below (length plist) by 2
                    always (keywordp (nth i plist))))))

(defun cacheus-validate-fn-option (value option-name)
  "Validate that VALUE is suitable for an option that expects a function.
An acceptable value is nil, a symbol (quoted or not), a lambda form,
or a function object. Signals an error if the validation fails."
  (when (and value
             (not (or (symbolp value)
                      (functionp value)
                      (and (listp value)
                           (memq (car-safe value) '(lambda function quote))))))
    (error "cacheus: %S option must be a function name, a lambda, or a function object, got %S"
           option-name value)))

(defun cacheus-struct-to-plist (struct-instance)
  "Convert a `cl-defstruct` instance into a property list.
This function introspects the `STRUCT-INSTANCE` to find all its slot names
and creates a plist of the form `(:slot1 val1 :slot2 val2 ...)`.

Arguments:
- `STRUCT-INSTANCE`: The `cl-defstruct` instance.

Returns:
A property list representation of the struct's slots and values, or `nil`
if `STRUCT-INSTANCE` is not a valid struct."
  (when (and (consp struct-instance) (symbolp (car struct-instance)))
    (let* ((struct-type (type-of struct-instance))
           (slot-names-vec (get struct-type 'cl-struct-slot-info)))
      (when slot-names-vec
        (apply #'append
               (-map (list (intern (concat ":" (symbol-name it)))
                           (slot-value struct-instance it))
                     (cl-coerce slot-names-vec 'list)))))))

(defmacro -let-pattern* (bindings &rest body)
  "A destructuring `let*` that supports `&struct` for `cl-defstruct`.
This macro extends `-let*` by adding a `(&struct ...)` keyword,
allowing for convenient binding of struct slots to local variables.

The binding form is `((&struct :SLOT-1 VAR-1 :SLOT-2) STRUCT-FORM)`.
This mimics the behavior of `dash`'s `&plist` destructuring. All other
binding forms are passed directly to `-let*`.

It works by expanding each `&struct` form into a series of `let*`
bindings that call the appropriate accessor function for each slot.

Usage Example:
  (cl-defstruct person :name :age)
  (let ((p1 (make-person :name \"Alice\" :age 30)))
    (-let-pattern* (((&struct :name person-name :age) p1))
      (message \"Name: %s, Age: %d\" person-name age)))"
  (declare (indent 1))
  (cl-check-type bindings list)
  (let ((final-forms nil))
    (dolist (binding bindings)
      (if (and (consp binding) (consp (car binding))
               (eq (caar binding) '&struct))
          (let* ((struct-expr (cadr binding))
                 (slot-specs (copy-sequence (cdr (car binding))))
                 (g-struct (gensym "struct-")))
            (push `(,g-struct ,struct-expr) final-forms)
            (while slot-specs
              (let* ((slot-key (pop slot-specs))
                     (var-name (if (and slot-specs (not (keywordp (car slot-specs))))
                                   (pop slot-specs)
                                 (intern (substring (symbol-name slot-key) 1))))
                     (acc-name (substring (symbol-name slot-key) 1)))
                (unless (keywordp slot-key)
                  (error "Struct slot key must be a keyword: %S" slot-key))
                ;; NOTE: Using `(type-of ,g-struct)` here is a non-standard macro expansion pattern
                ;; that relies on specific behavior of your Emacs environment.
                ;; In typical Emacs Lisp macro expansion, `(type-of symbol)` evaluates to `symbol`,
                ;; not the runtime type of the variable.
                (push `(,var-name
                        (let ((accessor (intern (format "%s-%s"
                                                        (type-of ,g-struct)
                                                        ,acc-name))))
                          (funcall accessor ,g-struct)))
                      final-forms))))
        (push binding final-forms)))
    `(-let* ,(nreverse final-forms)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instance Reconstruction

(defun cacheus-resolve-option-value (value)
  "Evaluate VALUE if it is a symbol, otherwise return it.
This is used at runtime to resolve cache options that can be provided
as either a literal value or a variable."
  (if (symbolp value)
      (symbol-value value)
    value))

(defun cacheus-get-runtime-instance-from-macro-vars
    (blueprint-ref instance-constructor)
  "Reconstruct a live cache instance at runtime from its blueprint.
This is the bridge between a compile-time macro definition and the live
runtime state of its associated global variables (`defvar`s). It allows
generic functions to operate on specific cache instances.

Arguments:
- `BLUEPRINT-REF`: A symbol (the cache name) or a macro-time instance struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A live `cacheus-instance` populated with current runtime data."
  (let ((blueprint (if (symbolp blueprint-ref)
                       (when-let ((entry (gethash blueprint-ref
                                                  cacheus-global-cache-registry)))
                         (plist-get entry :macro-time-instance))
                     blueprint-ref)))
    (unless blueprint
      (error "cacheus: Blueprint for %S not found in registry" blueprint-ref))
    (let* ((options (cacheus-instance-options blueprint))
           (symbols (cacheus-instance-symbols blueprint))
           (live-data-plist (cacheus-default-slot-value-provider-lambda symbols))
           (runtime-instance (funcall instance-constructor
                                      :options options :symbols symbols)))
      (setf (cacheus-instance-runtime-data runtime-instance)
            (apply #'make-cacheus-runtime-data live-data-plist))
      runtime-instance)))

(defun cacheus-default-slot-value-provider-lambda (m-syms)
  "Return a plist of live runtime data for a `cacheus-symbols` instance.
This function is the default implementation for retrieving the current
values of all cache-related `defvar`s. It safely checks if optional
feature variables are bound before accessing them.

Arguments:
- `M-SYMS`: The `cacheus-symbols` struct from a macro-time instance.

Returns:
A plist containing the live runtime data for the cache, suitable for
passing to `make-cacheus-runtime-data`."
  (list
   :cache-ht      (symbol-value (cacheus-symbols-cache-var m-syms))
   :timestamps-ht (when-let ((s (cacheus-symbols-timestamps-var m-syms)))
                    (when (boundp s) (symbol-value s)))
   :order-data    (when-let ((s (cacheus-symbols-order-ring-or-queue-var m-syms)))
                    (when (boundp s) (symbol-value s)))
   :frequency-ht  (when-let ((s (cacheus-symbols-frequency-var m-syms)))
                    (when (boundp s) (symbol-value s)))
   :entry-tags-ht (when-let ((s (cacheus-symbols-entry-tags-var m-syms)))
                    (when (boundp s) (symbol-value s)))
   :tags-idx-ht   (when-let ((s (cacheus-symbols-tags-idx-var m-syms)))
                    (when (boundp s) (symbol-value s)))
   :inflight-ht   (when-let ((s (cacheus-symbols-inflight-var m-syms)))
                    (when (boundp s) (symbol-value s)))))

(provide 'cacheus-util)
;;; cacheus-util.el ends here