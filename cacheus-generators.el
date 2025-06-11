;;; cacheus-generators.el --- Generic function generators for Cacheus modules -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is the code-generation engine for the Cacheus framework. It
;; provides a collection of helper functions and a helper macro that build the
;; full `defun` and `defvar` boilerplate for any cache defined with
;; `cacheus-cache!` or `cacheus-memoize!`.
;;
;; Its primary export is `cacheus-make-cache-backend`, the internal workhorse
;; function that orchestrates all other generators in this file.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'ts)
(require 'dash)
(require 'concur nil t)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-persistence)
(require 'cacheus-storage)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primary Backend Generator

(defun cacheus-make-cache-backend (instance &rest args)
  "The central code-generation engine for the entire Cacheus framework.
This function is the internal workhorse called by high-level macros like
`cacheus-cache!` and `cacheus-memoize!`. It is not intended to be called
directly by end-users. It takes a pre-configured `INSTANCE` blueprint and
generates a single, large `(progn ...)` form containing all the Lisp code
required to create and manage a complete cache instance.

Arguments:
- `instance` (`cacheus-instance` struct): A complete blueprint of the cache.
- `args` (plist): A property list for additional macro-specific context.
  - `:instance-constructor` (symbol): The constructor function for the
    specific instance type (e.g., 'make-cacheus-cache-instance).
  - `:compute-thunk-form` (form): A Lisp form (often a lambda) that
    defines the computation to be performed on a cache miss.

Returns:
A single `(progn ...)` Lisp form ready for macro expansion."
  (let ((instance-constructor (plist-get args :instance-constructor)))
    ;; Destructure the instance blueprint into local variables for easier access.
    ;; This pulls out all options (opts) and generated symbols (syms).
    (-let-pattern*
        (((&struct :options opts :symbols syms) instance)
         ((&struct :name name :logger logger :version ver :cache-file file
                   :capacity cap :clear-hook hook) opts)
         ((&struct :all-struct-fields-for-entries all-fields
                   :struct-name-for-entries s-name
                   :make-fn-constructor-for-entries ctor
                   :get-fn get-fn :put-fn put-fn :clear-fn clear-fn
                   :save-fn save-fn :load-fn load-fn
                   :cache-var cache-var :version-id-var ver-id-var
                   :size-var size-var :inspect-cache-fn inspect-cache-fn
                   :inspect-entry-fn inspect-entry-fn
                   :invalidate-tags-fn inv-tags-fn
                   :inflight-var inflight-var) syms))

      ;; Return a single `progn` form containing all generated code.
      `(progn
         ;; 1. Define the cache-entry struct.
         ;; This struct holds the cached data (`data`), metadata (`timestamp`,
         ;; `entry-version`), and any user-defined `:fields`.
         (cl-defstruct (,s-name (:constructor ,ctor)) ,@all-fields)

         ;; 2. Define the core variables for this cache instance.
         ;; Each cache gets its own set of namespaced variables.
         (defvar ,cache-var (ht-create) ,(format "Cache for %S." name))
         (defvar ,size-var ,cap ,(format "Capacity for %S." name))
         (defvar ,ver-id-var ,ver ,(format "Functional version for %S." name))
         ;; The inflight-var is only defined if the cache supports :async.
         ,(when inflight-var `(defvar ,inflight-var (ht-create)
                                ,(format "In-flight requests for %S." name)))

         ;; 3. Register the cache in the global registry.
         ;; This makes the cache discoverable by global management functions
         ;; like `cacheus-list-all-caches` and interactive macros like `cacheus-get!`.
         (ht-set! cacheus-global-cache-registry ',name
                  (list :name ',name
                        :config (cacheus-instance-options ',instance)
                        :symbols-struct (cacheus-instance-symbols ',instance)
                        :macro-time-instance ',instance
                        :get-fn-symbol ',get-fn
                        :put-fn-symbol ',put-fn
                        :clear-fn-symbol ',clear-fn
                        :save-fn-symbol ',save-fn
                        :load-fn-symbol ',load-fn
                        :invalidate-by-tags-fn-symbol ',inv-tags-fn
                        :type (let ((type-sym (type-of ,instance)))
                                (cond ((eq type-sym 'cacheus-memoize-instance) 'memoize)
                                      ((eq type-sym 'cacheus-cache-instance) 'cache)
                                      (t (error "Unknown instance type: %S"
                                                type-sym))))))

         ;; 4. Generate the public API functions for this cache instance.
         ;; Each of these `cacheus-make-...` functions returns a `(defun ...)` form.
         ;; Only the functions relevant to the cache's configuration are generated.
         ,(cacheus-make-get-fn-form get-fn instance instance-constructor)
         ,(cacheus-make-put-fn-form put-fn instance instance-constructor)
         ,(when clear-fn
            (cacheus-make-clear-fn-form clear-fn instance instance-constructor hook))
         ,(when save-fn
            (cacheus-make-save-fn-form save-fn instance instance-constructor))
         ,(when load-fn
            (cacheus-make-load-fn-form load-fn instance instance-constructor))
         ,(when inspect-cache-fn
            (cacheus-make-inspect-cache-fn-form inspect-cache-fn instance
                                                instance-constructor))
         ,(when inspect-entry-fn
            (cacheus-make-inspect-entry-fn-form inspect-entry-fn instance
                                                instance-constructor))
         ,(when inv-tags-fn
            (cacheus-make-invalidate-by-tags-fn-form inv-tags-fn instance
                                                     instance-constructor))

         ;; 5. Set up the initial load from a persistent file, if configured.
         ;; This is wrapped in `eval-after-load` to ensure that the core Cacheus
         ;; system is fully loaded before attempting to load data from a file,
         ;; preventing startup order issues.
         ,(when (and load-fn file)
            `(eval-after-load 'cacheus
               '(lambda ()
                  (let ((logger (cacheus-resolve-logger ,logger)))
                    (when (file-exists-p ,file)
                      (funcall logger :info "[C:%S] Initial load from %s"
                               ',name ,file)
                      (condition-case-unless-debug e (,load-fn)
                        (error (funcall logger :error
                                        "[C:%S] Initial load failed: %S"
                                        ',name e :trace))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Function Generators

(defun cacheus-make-get-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for a generic cache 'get' function.
This function generates a simple wrapper that delegates all caching logic to
the central `cacheus-get-or-compute` function.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'get' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'get' function."
  (-let-pattern*
      (((&struct :options opts) instance)
       ((&struct :name name :async async) opts))
    `(defun ,fn-sym (key &optional compute-if-miss-p user-key compute-thunk)
       ,(format "Get value for KEY from the '%S' cache." name)
       ,(when async `(require 'concur))
       (cl-block ,fn-sym
         (-let*
             ((logger (cacheus-resolve-logger ,(cacheus-options-logger opts)))
              (runtime-instance
               (cacheus-get-runtime-instance-from-macro-vars
                ',instance ',instance-constructor))
              (cache-ht (cacheus-runtime-data-cache-ht
                         (cacheus-instance-runtime-data runtime-instance))))
           (if (and compute-if-miss-p compute-thunk)
               ;; On a potential write-path, delegate to the central function.
               (cacheus-get-or-compute runtime-instance key
                                       compute-thunk
                                       :user-key user-key
                                       :async ,async)
             ;; If not computing, just do a fast, read-only lookup.
             (let ((entry (ht-get cache-ht key)))
               (when (and entry (not (cacheus-is-instance-entry-stale
                                      key entry runtime-instance logger)))
                 (funcall (cacheus-symbols-data-accessor-for-entries
                           (cacheus-instance-symbols runtime-instance))
                          entry)))))
         nil))))

(defun cacheus-make-put-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for a generic cache 'put' function.
This allows for manual insertion of a key-value pair into the cache.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'put' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'put' function."
  (-let-pattern*
      (((&struct :options opts) instance)
       ((&struct :name name :logger logger :predicate pred :tags-fn tags-fn) opts))
    `(defun ,fn-sym (key value &optional explicit-tags)
       ,(format "Put VALUE for KEY into the '%S' cache." name)
       (let* ((logger (cacheus-resolve-logger ,logger))
              (runtime-instance
               (cacheus-get-runtime-instance-from-macro-vars
                ',instance ',instance-constructor))
              ;; Explicit tags provided to a `put` call override the tags-fn.
              (tags (or explicit-tags
                        ,(when tags-fn `(funcall ,tags-fn key value)))))
         (if (and ,pred (null explicit-tags) (not (funcall ,pred value)))
             (progn
               (funcall logger :info "[C:%S] Put: Skipped %S due to predicate."
                        ,name key)
               nil)
           (let ((entry (cacheus--create-entry runtime-instance
                                               key value)))
             (cacheus-store-result entry key tags runtime-instance logger)
             value))))))

(defun cacheus-make-clear-fn-form (fn-sym instance instance-constructor hook)
  "Generate the `defun` form for a 'clear cache' function.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'clear' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.
- `HOOK`: An optional hook function to run after clearing.

Returns:
A `(defun ...)` Lisp form for the 'clear' function."
  (-let-pattern*
      (((&struct :options opts) instance)
       ((&struct :name name :logger logger) opts))
    `(defun ,fn-sym ()
       ,(format "Clear all entries from the '%S' cache." name)
       (interactive)
       (let ((logger (cacheus-resolve-logger ,logger)))
         (funcall logger :info "[C:%S] Clearing cache..." ,name)
         (cacheus-clear-runtime-data
          (cacheus-get-runtime-instance-from-macro-vars
           ',instance ',instance-constructor))
         ;; If the user provided a :clear-hook, run it now.
         (when ,hook
           (condition-case-unless-debug e (funcall ,hook)
             (error (funcall logger :error "[C:%S] clear-hook error: %S"
                             ,name e :trace)))))
      ',fn-sym)))

(defun cacheus-make-save-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for a 'save cache' function.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'save' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'save' function."
  (-let-pattern*
      (((&struct :options opts) instance)
       ((&struct :name name :cache-file file :logger logger) opts))
    `(defun ,fn-sym ()
       ,(format "Save the '%S' cache to its file." name)
       (interactive)
       (let ((logger (cacheus-resolve-logger ,logger)))
         ;; This function is a no-op if no :cache-file was configured.
         (if ,file
             (progn
               (funcall logger :info "[C:%S] Saving to %s..." ,name ,file)
               (condition-case-unless-debug e
                   (cacheus-persist-cache-instance
                    (cacheus-get-runtime-instance-from-macro-vars
                     ',instance ',instance-constructor)
                    logger)
                 (error (funcall logger :error "[C:%S] Save failed: %S" ,name e)
                        (signal 'cacheus-error (list "Save failed" e)))))
           (funcall logger :warn "[C:%S] Save: No :cache-file configured."
                    ,name)))
      ',fn-sym)))

(defun cacheus-make-load-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for a 'load cache' function.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'load' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'load' function."
  (-let-pattern*
      (((&struct :options opts) instance)
       ((&struct :name name :cache-file file :logger logger) opts))
    `(defun ,fn-sym ()
       ,(format "Load the '%S' cache from its file." name)
       (interactive)
       (let ((logger (cacheus-resolve-logger ,logger)))
         ;; This function is a no-op if no :cache-file was configured.
         (if ,file
             (progn
               (funcall logger :info "[C:%S] Loading from %s..." ,name ,file)
               (cacheus-load-cache-instance
                (cacheus-get-runtime-instance-from-macro-vars
                 ',instance ',instance-constructor)
                logger))
           (funcall logger :warn "[C:%S] Load: No :cache-file configured."
                    ,name)))
      ',fn-sym)))

(defun cacheus-make-inspect-cache-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for an 'inspect-cache' function.
The generated function will display all cache entries in a new buffer.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'inspect-cache' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'inspect-cache' function."
  (-let-pattern*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :name name) opts)
       ((&struct :version-id-var ver-var
                 :data-accessor-for-entries data-acc
                 :ts-accessor-for-entries ts-acc) syms))
    `(defun ,fn-sym ()
       ,(format "Inspect all entries in the '%S' cache." name)
       (interactive)
       (-let-pattern*
           ((runtime-instance
             (cacheus-get-runtime-instance-from-macro-vars
              ',instance ',instance-constructor))
            ((&struct :cache-ht cache-ht :timestamps-ht ts-ht
                      :entry-tags-ht et-ht)
             (cacheus-instance-runtime-data runtime-instance))
            (ver (if (boundp ',ver-var) (symbol-value ',ver-var) "N/A")))
         (with-output-to-temp-buffer (format "*Cacheus Inspection: %S*" ,name)
           (princ (format "Cache: %S\nVersion: %S\nEntries: %d\n---\n"
                          ,name ver (ht-size cache-ht)))
           (if (ht-empty-p cache-ht)
               (princ "Empty.\n")
             (ht-map
              cache-ht
              (lambda (k v)
                (princ
                 (format
                  (concat "Key: %S\n  Data: %S\n  TS: %s\n"
                          "  Age: ~as\n  Tags: %S\n---\n")
                  k (funcall ',data-acc v)
                  (if-let ((ts (funcall ',ts-acc v))) (ts-to-iso8601 ts) "N/A")
                  (if-let ((rts (ht-get ts-ht k))) (ts-diff (ts-now) rts)
                    (if-let ((ts (funcall ',ts-acc v)))
                        (ts-diff (ts-now) ts) "N/A"))
                  (ht-get et-ht k "N/A")))))))))))

(defun cacheus-make-inspect-entry-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for an 'inspect-entry' function.
The generated function will display details for a single entry.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'inspect-entry' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(defun ...)` Lisp form for the 'inspect-entry' function."
  (-let-pattern*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :name name :version ver) opts)
       ((&struct :data-accessor-for-entries data-acc
                 :ts-accessor-for-entries ts-acc
                 :entry-ver-accessor-for-entries ver-acc) syms))
    `(defun ,fn-sym (user-key-str)
       ,(format "Inspect a single entry in the '%S' cache." name)
       (interactive "sUser key (Lisp form): ")
       (-let-pattern*
           ((runtime-instance
             (cacheus-get-runtime-instance-from-macro-vars
              ',instance ',instance-constructor))
            ((&struct :cache-ht cache-ht :timestamps-ht ts-ht
                      :entry-tags-ht et-ht)
             (cacheus-instance-runtime-data runtime-instance)))
         (condition-case-unless-debug e
             ;; The user-key is read from a string, which is convenient but
             ;; should be used with awareness of the potential for code evaluation.
             (let* ((user-key (read user-key-str))
                    (eff-key (if ',ver (list user-key ',ver) user-key))
                    (entry (ht-get cache-ht eff-key)))
               (if entry
                   (message
                    (format
                     (concat "Cache '%S' Entry:\n  User Key: %S\n"
                             "  Effective Key: %S\n  Version (Entry/Cache): %S / %S\n"
                             "  Data: %S\n  Timestamp: %s\n  Age: ~as\n  Tags: %S")
                     ,name user-key eff-key (funcall ',ver-acc entry) ,ver
                     (funcall ',data-acc entry)
                     (if-let ((ts (funcall ',ts-acc entry))) (ts-to-iso8601 ts) "N/A")
                     (if-let ((rts (ht-get ts-ht eff-key))) (ts-diff (ts-now) rts)
                       (if-let ((ts (funcall ',ts-acc entry)))
                           (ts-diff (ts-now) ts) "N/A"))
                     (ht-get et-ht eff-key "N/A")))
                 (message "No entry for key %S in cache %S." user-key ',name)))
           (error (message "Error inspecting %S (key: %s): %S"
                           ',name user-key-str e)))))))

(defun cacheus-make-invalidate-by-tags-fn-form (fn-sym instance instance-constructor)
  "Generate the `defun` form for an 'invalidate-by-tags' function.

Arguments:
- `FN-SYM`: The symbol to be defined as the 'invalidate-by-tags' function.
- `INSTANCE`: The macro-time `cacheus-instance` struct.
- `INSTANCE-CONSTRUCTOR`: The constructor for the specific instance type.

Returns:
A `(cl-defun ...)` Lisp form for the 'invalidate-by-tags' function."
  `(cl-defun ,fn-sym (tags &key (all-must-match nil) (run-hooks t))
     ,(format "Invalidate entries by TAGS for `%S`."
              (cacheus-options-name (cacheus-instance-options instance)))
     (interactive (list (read-from-minibuffer "Invalidate Tags (Lisp form): ")))
     (cacheus-invalidate-keys-by-tags
      (cacheus-get-runtime-instance-from-macro-vars
       ',instance ',instance-constructor)
      tags :all-must-match all-must-match :run-hooks run-hooks)
     ',fn-sym))

(provide 'cacheus-generators)
;;; cacheus-generators.el ends here