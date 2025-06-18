;;; cacheus-cache.el --- A configurable, TTL-aware cache framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the `cacheus-cache!` macro, a user-friendly way to
;; define namespaced, TTL-aware, general-purpose caches in Emacs Lisp. It
;; builds upon the core framework to handle persistence, eviction, and other
;; advanced features.
;;
;; Interaction with the cache is handled via the universal `cacheus-get!`
;; and `cacheus-put!` macros defined in `cacheus-core.el`.

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
  options (cacheus-options): Inherited. User-provided configuration.
  symbols (cacheus-symbols): Inherited. Generated names for vars/functions.
  runtime-data (cacheus-runtime-data): Inherited. Live cache data.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers (File-Local)

(defun cacheus-cache--parse-macro-args (name-sym options-list)
  "Parse and validate the top-level arguments for `cacheus-cache!`."
  (unless (symbolp name-sym)
    (error "cacheus-cache!: NAME argument must be a symbol, got %S" name-sym))
  (unless (cacheus-plist-valid-p options-list)
    (error "cacheus-cache!: Options are not a valid plist: %S" options-list))
  (list name-sym options-list))

(defun cacheus-cache--create-instance (name-sym options-plist)
  "Create a `cacheus-cache-instance` blueprint from macro arguments."
  ;; 1. Get the generic options and symbols from the core generators.
  (let* ((options (cacheus-create-options name-sym options-plist))
         (symbols (cacheus-generate-symbols options)))
    ;; 2. Create and return the final instance blueprint.
    (make-cacheus-cache-instance
     :options options
     :symbols symbols
     ;; NOTE: The `:runtime-data` slot is intentionally left nil. It will be
     ;; lazily initialized on first use by `cacheus-get-instance-by-name`.
     :runtime-data nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Macro

;;;###autoload
(defmacro cacheus-cache! (name &rest options-list)
  "Define a namespaced, general-purpose, and optionally persistent cache.

This macro generates all necessary data structures and helper
functions for the cache. Interaction is then handled via the
universal `cacheus-get!` and `cacheus-put!` macros.

Arguments:
  NAME (symbol): The base name for the cache (e.g., `my-app-data`).
    This symbol is used to identify the cache globally.
  OPTIONS-LIST (plist): A property list of keyword arguments for
    customization. See `cacheus-options` struct for details.

Example:
  (cacheus-cache! my-project-cache
    :capacity 100
    :ttl (* 60 60 24))

  (cacheus-put! 'my-project-cache \"file-status.el\" \"modified\")
  (cacheus-get! 'my-project-cache \"file-status.el\") ; => \"modified\"

Returns:
  (symbol): The `NAME` of the cache, for convenience."
  (declare (indent 1))
  (let* ((parsed-args (cacheus-cache--parse-macro-args name options-list))
         (name-sym (car parsed-args))
         (opts-plist (cadr parsed-args))
         ;; The instance created here is a *template* or blueprint.
         (instance (cacheus-cache--create-instance name-sym opts-plist))
         ;; The live instance is stored in a generated global variable.
         (instance-var (intern (format "cacheus--%s-instance" name-sym))))
    `(progn
       ;; Generate the backend variables and admin functions.
       ,(cacheus-make-cache-backend instance instance-var)
       ',name-sym)))

(provide 'cacheus-cache)
;;; cacheus-cache.el ends here