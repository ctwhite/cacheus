;;; cacheus-util.el --- Core utilities for the Cacheus framework -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file is the home for fundamental utility functions and macros that are
;; used across the Cacheus caching framework. It centralizes common operations
;; to promote code reuse, consistency, and a clean architecture.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'json)
(require 's)
(require 'ring)
(require 'dash)
(require 'ts)

(require 'cacheus-structs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging Utilities

(defun cacheus-resolve-logger (logger-opt)
  "Resolve `LOGGER-OPT` to a callable logger function.
This function correctly handles symbols that are hook variables by
returning a lambda that will execute the hook.

Arguments:
- `LOGGER-OPT`: Can be `t` (for `message`), `nil` (for a no-op function),
  a function symbol, a hook symbol, or a lambda.

Returns:
  (function): A callable logger function that accepts `(LEVEL FORMAT &rest ARGS)`."
  (cond
   ((eq logger-opt t) #'message)
   ((null logger-opt) (lambda (&rest _args) nil))
   ((functionp logger-opt) logger-opt)
   ((symbolp logger-opt)
    (cond
     ((fboundp logger-opt) (symbol-function logger-opt))
     ((boundp logger-opt)
      (lambda (level fmt &rest args)
        (let ((final-args (-flatten args)))
          (apply #'run-hook-with-args logger-opt level fmt final-args))))
     (t
      (warn "cacheus: Logger symbol %S is void; using no-op." logger-opt)
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
- `LOGGER` (function): A resolved logger function for error reporting.

Returns:
  (string): A string representation of `KEY`, or `nil` on failure."
  (condition-case-unless-debug err
      (cl-typecase key
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
- `SKEY` (string): The stringified key from a cache file.
- `LOGGER` (function): A resolved logger function for error reporting.

Returns:
  (any): The parsed Elisp key, or `nil` on failure."
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
- `PLIST` (list): The list to check.

Returns:
  (boolean): `t` if `PLIST` is a valid property list, `nil` otherwise."
  (or (null plist)
      (and (zerop (% (length plist) 2))
           (cl-loop for i from 0 below (length plist) by 2
                    always (keywordp (nth i plist))))))

(defun cacheus-validate-fn-option (value option-name)
  "Validate that VALUE is suitable for an option that expects a function.
An acceptable value is nil, a symbol (quoted or not), a lambda form,
or a function object. Signals an error if the validation fails.

Arguments:
- `VALUE` (any): The option value to validate.
- `OPTION-NAME` (keyword): The name of the option for error messages.

Returns:
  nil. Signals an error as a side-effect if invalid."
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
- `STRUCT-INSTANCE` (struct): The `cl-defstruct` instance.

Returns:
  (plist): A property list representation of the struct's slots and values,
or `nil` if `STRUCT-INSTANCE` is not a valid struct."
  (when (and (consp struct-instance) (symbolp (car struct-instance)))
    (let* ((struct-type (type-of struct-instance))
           (slot-names-vec (get struct-type 'cl-struct-slot-info)))
      (when slot-names-vec
        (apply #'append
               (-map (list (intern (concat ":" (symbol-name it)))
                           (slot-value struct-instance it))
                     (cl-coerce slot-names-vec 'list)))))))

(defmacro cacheus-let* (bindings &rest body)
  "A destructuring `let*` that supports `&struct` for `cl-defstruct`.
This macro extends `-let*` by adding a `(&struct ...)` keyword,
allowing for convenient binding of struct slots to local variables."
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
;;; Inspection Dispatcher

(defun cacheus-inspect-instance-dispatch (instance key-str)
  "Display inspection details for INSTANCE in a new buffer.
If `KEY-STR` is nil, displays all entries. If `KEY-STR` is a string,
displays details for that specific key. This is the universal inspection
logic called by all generated `-inspect` helper functions.

Arguments:
- `INSTANCE` (cacheus-instance): The runtime instance of the cache.
- `KEY-STR` (string|nil): The key to inspect, or nil to inspect all."
  (cacheus-let*
      (((&struct :options opts :symbols syms :runtime-data data) instance)
       ((&struct :name name :version ver) opts)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht
                 :entry-tags-ht et-ht) data)
       ((&struct :version-id-var ver-var
                 :key-accessor-for-entries key-acc
                 :data-accessor-for-entries data-acc
                 :ts-accessor-for-entries ts-acc
                 :entry-ver-accessor-for-entries ver-acc) syms))
    (if key-str
        ;; --- Inspect a single entry ---
        (condition-case-unless-debug e
            (let* ((user-key (read key-str))
                   (eff-key (if ver (list user-key ver) user-key))
                   (entry (ht-get cache-ht eff-key)))
              (if entry
                  (message
                   (format
                    (concat "Cache '%S' Entry:\n"
                            "  User Key: %S\n"
                            "  Effective Key: %S\n"
                            "  Version (Entry/Cache): %S / %S\n"
                            "  Data: %S\n"
                            "  Timestamp: %s\n"
                            "  Age: ~a\n"
                            "  Tags: %S")
                    name user-key eff-key (funcall ver-acc entry) ver
                    (funcall data-acc entry)
                    (if-let ((ts (funcall ts-acc entry))) (ts-to-iso8601 ts) "N/A")
                    (if-let ((rts (ht-get ts-ht eff-key))) (ts-diff (ts-now) rts)
                      (if-let ((ts (funcall ts-acc entry)))
                          (ts-diff (ts-now) ts) "N/A"))
                    (ht-get et-ht eff-key "N/A")))
                (message "No entry for key %S in cache %S." user-key name)))
          (error (message "Error inspecting %S (key: %s): %S"
                          name key-str e)))
      ;; --- Inspect all entries ---
      (with-output-to-temp-buffer (format "*Cacheus Inspection: %S*" name)
        (princ (format "Cache: %S\nVersion: %S\nEntries: %d\n---\n"
                       name
                       (if (boundp ver-var) (symbol-value ver-var) "N/A")
                       (ht-size cache-ht)))
        (if (ht-empty? cache-ht)
            (princ "Empty.\n")
          (ht-map
           cache-ht
           (lambda (k v)
             (princ
              (format
               (concat "Key: %S\n  Data: %S\n  TS: %s\n"
                       "  Age: ~a\n  Tags: %S\n---\n")
               (funcall key-acc v)
               (funcall data-acc v)
               (if-let ((ts (funcall ts-acc v))) (ts-to-iso8601 ts) "N/A")
               (if-let ((rts (ht-get ts-ht k))) (ts-diff (ts-now) rts)
                 (if-let ((ts (funcall ts-acc v)))
                     (ts-diff (ts-now) ts) "N/A"))
               (ht-get et-ht k "N/A"))))))))))

(provide 'cacheus-util)
;;; cacheus-util.el ends here