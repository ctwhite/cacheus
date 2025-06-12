;;; cacheus-cache.el --- A configurable, TTL-aware cache framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the `cacheus-cache!` macro, a user-friendly way to
;; define namespaced, TTL-aware, general-purpose caches in Emacs Lisp. It
;; builds upon the core framework to handle persistence, eviction, and other
;; advanced features.
;;
;; Core Features via `cacheus-cache!`:
;; - Defines a new cache instance with a unique name.
;; - Generates a specific `cl-defstruct` for cache entries, allowing custom
;;   fields via the `:fields` keyword.
;; - Sets up underlying `defvar`s for data storage.
;; - Generates a full suite of helper functions (get, put, clear, etc.).
;; - Registers the cache with the global `cacheus-global-cache-registry`.
;;
;; Customization Options:
;; See `cacheus-options` struct for a full list of keywords, including `:ttl`,
;; `:capacity`, `:version`, `:cache-file`, `:fields`, and `:tags-fn`.

;;; Code:

(require 'cl-lib)

(require 'cacheus-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module-Specific Definitions

(define-error 'cacheus-cache-error
  "Cacheus-cache specific error."
  'cacheus-error)

(cl-defstruct (cacheus-cache-instance (:include cacheus-instance))
  "A type-specific instance struct for generic caches.
This struct is functionally identical to the base `cacheus-instance` but
provides a distinct type for caches created with `cacheus-cache!`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun cacheus-cache--parse-macro-args (name-sym options-list macro-name)
  "Parse and validate the top-level arguments for `cacheus-cache!`.

Arguments:
- `NAME-SYM`: The symbol provided as the cache name.
- `OPTIONS-LIST`: The plist of options.
- `MACRO-NAME`: The name of the macro calling this helper, for error messages.

Returns:
A list `(NAME-SYM OPTIONS-PLIST)` if valid, or signals an error."
  (unless (symbolp name-sym)
    (error "%S: NAME argument must be a symbol, got %S"
           macro-name name-sym))
  (unless (cacheus-plist-valid-p options-list)
    (error "%S: Options are not a valid plist: %S"
           macro-name options-list))
  (list name-sym options-list))

(defun cacheus-cache--create-instance (name-sym options-plist)
  "Create a `cacheus-cache-instance` blueprint from macro arguments.
This function orchestrates the creation of the options and symbols
structs. It calls the core generators and then adds this module's
specific logic for handling custom `:fields` for the cache entry struct.

Arguments:
- `NAME-SYM`: The cache name symbol.
- `OPTIONS-PLIST`: The plist of options from the macro call.

Returns:
A complete `cacheus-cache-instance` struct."
  ;; 1. Get the generic options and symbols from the core generators.
  (let* ((options (cacheus-create-options name-sym options-plist))
         (symbols (cacheus-generate-symbols options)))

    ;; 2. Handle `:fields`, an option specific to `cacheus-cache!`. The user
    ;; can provide custom fields to be added to the cache entry struct. This
    ;; logic appends those fields to the default set.
    (let ((fields-data (plist-get options-plist :fields)))
      (when fields-data
        (setf (cacheus-symbols-all-struct-fields-for-entries symbols)
              (append (cacheus-symbols-all-struct-fields-for-entries symbols)
                      fields-data))))

    ;; 3. Create and return the final instance.
    (make-cacheus-cache-instance
     :options options
     :symbols symbols
     :runtime-data (make-cacheus-runtime-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macros and Functions

(defmacro cacheus-cache! (name &rest options-list)
  "Define a namespaced, general-purpose, and optionally persistent cache.
This macro generates a complete cache instance with its own data structures
and a suite of helper functions for management (e.g., `get`, `put`, `clear`).

Arguments:
- `NAME` (symbol): The base name for the cache (e.g., `my-app-data`).
  This name is used to prefix all generated functions and variables.
- `OPTIONS-LIST` (plist): Keyword arguments for customization. See the
  `cacheus-options` struct definition for a full list.

Example:
  (cacheus-cache! my-project-cache
    :capacity 100
    :ttl (* 60 60 24) ; 1 day TTL
    :cache-file \"~/.emacs.d/cache/my-project.json\"
    :logger t)

  (cacheus-my-project-cache-put \"file-status.el\" \"modified\")
  (cacheus-get! 'my-project-cache \"file-status.el\") ; => \"modified\"

Returns:
The `NAME` symbol, for convenience."
  (declare (indent 1))
  ;; 1. Parse args and create the instance blueprint.
  (let* ((parsed-args (cacheus-cache--parse-macro-args name options-list
                                                       'cacheus-cache!))
         (name-sym (car parsed-args))
         (opts-plist (cadr parsed-args))
         (instance (cacheus-cache--create-instance name-sym opts-plist)))
    ;; 2. Delegate the entire code generation to the backend function.
    `(progn
       ,(cacheus-make-cache-backend
         instance
         :instance-constructor 'make-cacheus-cache-instance
         ;; A generic cache does not have a default way to compute a value
         ;; on a cache miss, so its compute thunk is nil.
         :compute-thunk-form nil)
       ',name-sym)))

(provide 'cacheus-cache)
;;; cacheus-cache.el ends here