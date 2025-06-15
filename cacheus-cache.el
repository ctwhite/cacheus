;;; cacheus-cache.el --- A configurable, TTL-aware cache framework -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file provides the `cacheus-cache!` macro, a user-friendly way to
;; define namespaced, TTL-aware, general-purpose caches in Emacs Lisp. It
;; builds upon the core framework to handle persistence, eviction, and other
;; advanced features.
;;
;; Interaction with the cache is handled via the universal `cacheus-get!`
;; and `cacheus-put!` macros defined in `cacheus-core.el`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module-Specific Definitions

(define-error 'cacheus-cache-error
  "Cacheus-cache specific error."
  'cacheus-error)

(cl-defstruct (cacheus-cache-instance (:include cacheus-instance))
  "A type-specific instance struct for generic caches.
This struct is functionally identical to the base `cacheus-instance` but
provides a distinct type for caches created with `cacheus-cache!`.

Fields:
- `options` (cacheus-options): Inherited. User-provided configuration.
- `symbols` (cacheus-symbols): Inherited. Generated names for vars/functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun cacheus-cache--parse-macro-args (name-sym options-list macro-name)
  "Parse and validate the top-level arguments for `cacheus-cache!`.

Arguments:
- `NAME-SYM` (symbol): The symbol provided as the cache name.
- `OPTIONS-LIST` (plist): The plist of options.
- `MACRO-NAME` (symbol): The name of the macro calling this helper.

Returns:
  (list): A list `(NAME-SYM OPTIONS-PLIST)` if valid, or signals an error."
  (unless (symbolp name-sym)
    (error "%S: NAME argument must be a symbol, got %S"
           macro-name name-sym))
  (unless (cacheus-plist-valid-p options-list)
    (error "%S: Options are not a valid plist: %S"
           macro-name options-list))
  (list name-sym options-list))

(defun cacheus-cache--create-instance (name-sym options-plist)
  "Create a `cacheus-cache-instance` blueprint from macro arguments.

Arguments:
- `NAME-SYM` (symbol): The cache name symbol.
- `OPTIONS-PLIST` (plist): The plist of options from the macro call.

Returns:
  (cacheus-cache-instance): A complete instance blueprint struct."
  ;; 1. Get the generic options and symbols from the core generators.
  (let* ((options (cacheus-create-options name-sym options-plist))
         (symbols (cacheus-generate-symbols options)))

    ;; 2. Handle `:fields`, an option specific to `cacheus-cache!`. The user
    ;; can provide custom fields to be added to the cache entry struct.
    (let ((fields-data (plist-get options-plist :fields)))
      (when fields-data
        (setf (cacheus-symbols-all-struct-fields-for-entries symbols)
              (append (cacheus-symbols-all-struct-fields-for-entries symbols)
                      fields-data))))

    ;; 3. Create and return the final instance blueprint.
    ;; The `:runtime-data` slot is intentionally omitted, as runtime state
    ;; is now managed via global defvars that are lazily initialized.
    (make-cacheus-cache-instance
     :options options
     :symbols symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macro

;;;###autoload
(defmacro cacheus-cache! (name &rest options-list)
  "Define a namespaced, general-purpose, and optionally persistent cache.
This macro generates the necessary data structures for the cache.
Interaction is handled via `cacheus-get!` and `cacheus-put!`.

Arguments:
- `NAME` (symbol): The base name for the cache (e.g., `my-app-data`).
- `OPTIONS-LIST` (plist): Keyword arguments for customization.

Example:
  (cacheus-cache! my-project-cache
    :capacity 100
    :ttl (* 60 60 24))

  (cacheus-put! 'my-project-cache \"file-status.el\" \"modified\")
  (cacheus-get! 'my-project-cache \"file-status.el\") ; => \"modified\"

Returns:
  (symbol): The `NAME` of the cache, for convenience."
  (declare (indent 2))
  (let* ((parsed-args (cacheus-cache--parse-macro-args name options-list
                                                       'cacheus-cache!))
         (name-sym (car parsed-args))
         (opts-plist (cadr parsed-args))
         (instance (cacheus-cache--create-instance name-sym opts-plist))
         (instance-var (intern (format "cacheus--%s-instance" (symbol-name name-sym)))))
    `(progn
       ;; Generate the backend variables and admin functions.
       ,(cacheus-make-cache-backend instance instance-var)
       ;; Define the variable that holds the cache's configuration at runtime.
       (defvar ,instance-var ',instance)
       ',name-sym)))

(provide 'cacheus-cache)
;;; cacheus-cache.el ends here