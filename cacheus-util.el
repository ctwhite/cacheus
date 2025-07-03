;;; cacheus-util.el --- Core utilities for the Cacheus framework -*-
;;; lexical-binding: t; -*-

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
(require 'ts)

(require 'cacheus-structs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging Utilities (Package-Private)

(defun cacheus-resolve-logger (logger-opt)
  "Resolve `LOGGER-OPT` to a callable logger function.
Handles `t` for `message`, `nil` for no-op, a function symbol,
or a direct function.

Arguments:
- `LOGGER-OPT` (any): The option value to resolve.

Returns:
  (function) A function that can be called with log arguments."
  (cond
   ((eq logger-opt t) #'message)
   ((null logger-opt) (lambda (&rest _args) nil))
   ((functionp logger-opt) logger-opt)
   ((symbolp logger-opt)
    (if (fboundp logger-opt)
        (symbol-function logger-opt)
      (lambda (&rest _args) nil)))
   (t (lambda (&rest _args) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Serialization Utilities (Package-Private)

(defun cacheus-stringify-key (key logger)
  "Convert an arbitrary Elisp `KEY` into a string for serialization."
  (condition-case err
      (cl-typecase key
        (string key)
        (symbol (symbol-name key))
        (number (number-to-string key))
        (t (prin1-to-string key)))
    (error (funcall logger :error "stringify-key: Failed for key %S: %S" key err)
           nil)))

(defun cacheus-parse-key (skey logger)
  "Parse a stringified key `SKEY` back into its original Elisp form."
  (condition-case err (read skey)
    (error (funcall logger :error "parse-key: Failed for string '%s': %S" skey err)
           nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument and Struct Utilities (Package-Private)

(defun cacheus-validate-fn-option (value option-name)
  "Validate that VALUE is suitable for an option that expects a function."
  (when (and value
             (not (or (symbolp value)
                      (functionp value)
                      (and (listp value)
                           (memq (car-safe value) '(lambda function quote))))))
    (error "cacheus: %S option must be function, lambda, or symbol, got %S"
           option-name value)))

(defmacro cacheus-let* (bindings &rest body)
  "A destructuring `let*` that supports `&struct` for `cl-defstruct`.
This macro extends `let*` with a special `&struct` keyword to
easily destructure Cacheus structs without needing to know their
exact type at compile time. It dynamically builds the correct
accessor function names at runtime.

Example:
  (cacheus-let* (((&struct :options opts :symbols syms) instance))
    ... use opts and syms ...) "
  (declare (indent 1))
  (cl-check-type bindings list)
  (let ((final-forms nil))
    (dolist (binding bindings)
      (if (and (consp binding) (consp (car binding))
               (eq (caar binding) '&struct))
          ;; This is the special &struct clause.
          (let* ((struct-expr (cadr binding))
                 (slot-specs (copy-sequence (cdr (car binding))))
                 (g-struct (gensym "struct-")))
            (push `(,g-struct ,struct-expr) final-forms)
            (while slot-specs
              (let* ((slot-key (pop slot-specs))
                     (var-name (if (and slot-specs (not (keywordp (car slot-specs))))
                                   (pop slot-specs)
                                 (intern (substring (symbol-name slot-key) 1))))
                     (acc-name-str (substring (symbol-name slot-key) 1)))
                (unless (keywordp slot-key)
                  (error "Struct slot key must be a keyword: %S" slot-key))
                (push `(,var-name
                        (let ((acc (intern (format "%s-%s" (type-of ,g-struct)
                                                   ,acc-name-str))))
                          (if (fboundp acc) (funcall acc ,g-struct)
                            (error "Invalid accessor %S for struct type %S"
                                   acc (type-of ,g-struct)))))
                      final-forms))))
        ;; This is a regular let* binding.
        (push binding final-forms)))
    `(-let* ,(nreverse final-forms)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inspection Dispatcher (Package-Private)

(defun cacheus-inspect-instance-dispatch (instance key-str)
  "Display inspection details for INSTANCE in a new buffer."
  (if key-str
      (cacheus--inspect-single-entry instance key-str)
    (cacheus--inspect-all-entries instance)))

(defun cacheus--inspect-single-entry (instance key-str)
  "Helper to inspect a single cache entry by KEY-STR."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (ver (cacheus-options-version opts))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (condition-case e
        (let* ((user-key (read key-str))
               (eff-key (if ver (list user-key ver) user-key))
               (entry (gethash eff-key cache-ht)))
          (if entry
              (message "%s" (cacheus--format-entry-for-inspection
                             name ver entry eff-key rtd))
            (message "No entry for key %S in cache %S." user-key name)))
      (error (message "Error inspecting %s (key: %s): %S" name key-str e)))))

(defun cacheus--inspect-all-entries (instance)
  "Helper to inspect all entries in a cache."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (rtd (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (ver-var (cacheus-symbols-version-id-var syms))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (with-output-to-temp-buffer (format "*Cacheus Inspection: %s*" name)
      (princ (format "Cache: %s\nVersion: %S\nEntries: %d\n---\n"
                     name (if (boundp ver-var) (symbol-value ver-var) "N/A")
                     (hash-table-count cache-ht)))
      (if (hash-table-empty-p cache-ht)
          (princ "Empty.\n")
        (maphash (lambda (k v)
                   (princ (cacheus--format-entry-for-inspection name nil v k rtd))
                   (princ "\n---\n"))
                 cache-ht)))))

(defun cacheus--format-entry-for-inspection (name ver entry eff-key rtd)
  "Helper to format a single entry for display."
  (let ((ts-ht (cacheus-runtime-data-timestamps-ht rtd))
        (et-ht (cacheus-runtime-data-entry-tags-ht rtd)))
    (format
     (concat "Cache '%s' Entry:\n"
             "  User Key: %S\n"
             "  Effective Key: %S\n"
             "  Version (Entry/Cache): %S / %S\n"
             "  Data: %S\n"
             "  Timestamp: %s\n"
             "  Age (sec): %s\n"
             "  Tags: %S")
     name
     (cacheus:entry-field name entry 'key)
     eff-key
     (cacheus:entry-version name entry)
     ver
     (cacheus:entry-data name entry)
     (if-let ((ts (cacheus:entry-timestamp name entry))) (ts-to-iso8601 ts) "N/A")
     (if-let ((ts (gethash eff-key ts-ht))) (ts-diff (ts-now) ts) "N/A")
     (gethash eff-key et-ht "None"))))

(provide 'cacheus-util)
;;; cacheus-util.el ends here