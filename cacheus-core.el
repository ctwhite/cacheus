;;; cacheus-core.el --- Core Cacheus Library and Global Management -*-
;;; lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; `cacheus-core.el` serves as the foundational library for the entire Cacheus
;; ecosystem. It defines the global cache registry, the high-level management
;; functions, and the primary user-facing macros (`cacheus:get!`,
;; `cacheus:put!`, etc.) that are shared by all other `cacheus-` modules.
;;
;; It orchestrates the metaprogramming defined in `cacheus-generators.el` and
;; manages the lifecycle of all defined caches.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(eval-when-compile (require 'cl-lib))

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-persistence)
(require 'cacheus-eviction)
(require 'cacheus-storage)
(require 'cacheus-tags)
(require 'cacheus-generators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Constants and Variables

(defcustom cacheus-default-prefix "cacheus"
  "The default string prefix used for all generated functions and variables."
  :type 'string
  :group 'cacheus)

(defcustom cacheus-save-all-on-shutdown t
  "If non-nil, automatically save all file-backed caches on Emacs shutdown."
  :type 'boolean
  :group 'cacheus)

(defvar cacheus-global-cache-registry (make-hash-table :test 'eq)
  "Global registry of all Cacheus caches defined by `defcacheus`.
This hash table maps a cache's name (a symbol) to a plist
containing its configuration, generated symbols, and a template instance.")

(define-error 'cacheus-error "Generic Cacheus error." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Generation Logic (Package-Private)

(defun cacheus-create-options (name-sym options-plist)
  "Create and validate a `cacheus-options` struct from a plist.
This is an internal setup function called by `defcacheus` to
process the user-provided options at compile-time.

Arguments:
- `NAME-SYM` (symbol): The name of the cache being created.
- `OPTIONS-PLIST` (plist): The plist of options from the macro body.

Returns:
  (cacheus-options) A validated options struct."
  (let* ((ttl (plist-get options-plist :ttl))
         (ver (plist-get options-plist :version))
         (cap (plist-get options-plist :capacity))
         (evic (plist-get options-plist :eviction-strategy))
         (file (plist-get options-plist :cache-file))
         (refresh-ttl (plist-get options-plist :refresh-ttl))
         (logger (plist-get options-plist :logger))
         (exp-hook (plist-get options-plist :expiration-hook))
         (periodic (plist-get options-plist :periodic-cleanup))
         (pred (plist-get options-plist :predicate))
         (err-handler (plist-get options-plist :error-handler))
         (async (plist-get options-plist :async))
         (dirty-p (plist-get options-plist :dirty-p))
         (clear-hook (plist-get options-plist :clear-hook))
         (prefix (plist-get options-plist :prefix))
         (fields (plist-get options-plist :fields))
         (meta-fn (plist-get options-plist :meta-fn))
         (tags-fn (plist-get options-plist :tags-fn))
         (final-evic (if (memq evic '(:lru :lfu :fifo :none nil)) evic :lru)))
    (when (and evic (not (eq evic final-evic)))
      (warn "cacheus: Invalid :eviction-strategy %S for %s. Using :lru."
            evic name-sym))
    (make-cacheus-options
     :name name-sym :logger logger :capacity cap :eviction-strategy final-evic
     :cache-file file :version ver :periodic-cleanup periodic
     :predicate pred :async async :error-handler err-handler :ttl ttl
     :refresh-ttl refresh-ttl :expiration-hook exp-hook :dirty-p dirty-p
     :clear-hook clear-hook :prefix (or prefix cacheus-default-prefix)
     :fields-data fields :meta-fn meta-fn :tags-fn tags-fn)))

(defun cacheus--generate-symbol-plist (sym-prefix)
  "Generate a plist of cache-related symbols based on a prefix."
  (let ((specs '((:cache-var           "-ht")
                  (:timestamps-var      "-timestamps-ht")
                  (:order-var           "-order-queue")
                  (:frequency-var       "-frequency-ht")
                  (:version-id-var      "-version-id")
                  (:save-fn             "-save")
                  (:clear-fn            "-clear")
                  (:inspect-fn          "-inspect")
                  (:entry-tags-var      "-entry-tags-ht")
                  (:tags-idx-var        "-tags-idx-ht")
                  (:invalidate-tags-fn  "-invalidate-by-tags")
                  (:load-fn             "-load")
                  (:inflight-var        "-inflight-ht"))))
    (cl-loop for (key suffix) in specs
             collect key and collect (intern (format "%s%s" sym-prefix suffix)))))

(defun cacheus-generate-symbols (options-struct)
  "Generate and return a `cacheus-symbols` struct based on options.
This function performs the metaprogramming step of creating all
the necessary symbol names for a new cache's functions, variables,
and struct accessors.

Arguments:
- `OPTIONS-STRUCT` (cacheus-options): The configuration for the new cache.

Returns:
  (cacheus-symbols) A struct containing all generated symbols."
  (let* ((name (cacheus-options-name options-struct))
         (prefix (cacheus-options-prefix options-struct))
         (fields (cacheus-options-fields-data options-struct))
         (sym-prefix (format "%s--%s" prefix (symbol-name name)))
         (s-name (intern (format "%s-entry" sym-prefix)))
         (ctor (intern (format "make-%s" s-name)))
         (key-acc (intern (format "%s-key" s-name)))
         (ts-acc (intern (format "%s-timestamp" s-name)))
         (data-acc (intern (format "%s-data" s-name)))
         (ver-acc (intern (format "%s-entry-version" s-name)))
         (all-fields `((key nil :read-only t) (data nil :read-only t)
                       (timestamp nil :type (or ts null))
                       (entry-version nil :read-only t)
                       ,@(cl-remove-duplicates fields :key #'car))))
    (apply #'make-cacheus-symbols
           :sym-prefix sym-prefix :struct-name-for-entries s-name
           :make-fn-constructor-for-entries ctor
           :key-accessor-for-entries key-acc :ts-accessor-for-entries ts-acc
           :data-accessor-for-entries data-acc
           :entry-ver-accessor-for-entries ver-acc
           :all-struct-fields-for-entries all-fields
           (cacheus--generate-symbol-plist sym-prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Private Helpers

(defun cacheus--initialize-runtime-data (instance)
  "Create and attach a new `cacheus-runtime-data` struct to an INSTANCE.
This performs lazy initialization of a cache's live data structures."
  (let* ((opts (cacheus-instance-options instance))
         (cap (cacheus-options-capacity opts))
         (evic (cacheus-options-eviction-strategy opts))
         (rtd (make-cacheus-runtime-data
               :cache-ht (make-hash-table :test 'equal)
               :timestamps-ht (make-hash-table :test 'equal)
               :entry-tags-ht (make-hash-table :test 'equal)
               :tags-idx-ht (make-hash-table :test 'eq)
               :inflight-ht (make-hash-table :test 'equal)
               :order-data (when (and cap (memq evic '(:lru :fifo)))
                             (ring-init cap))
               :frequency-ht (when (and cap (eq evic :lfu))
                               (make-hash-table :test 'equal)))))
    (setf (cacheus-instance-runtime-data instance) rtd)))

(defun cacheus-get-instance-by-name (cache-name)
  "Find, lazily initialize, and return the live instance for CACHE-NAME.
This is a critical function that implements lazy initialization. When a cache
is defined, a 'template' instance is stored in a `defvar`. The first time that
cache is accessed via this function, the template is copied, and its runtime
data structures (hash tables, etc.) are created. This live instance then
replaces the template in the `defvar` for all subsequent uses in the session.
This avoids allocating memory for all caches at startup.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache to retrieve.

Returns:
  (cacheus-instance) The live, initialized cache instance struct."
  (let* ((reg (gethash cache-name cacheus-global-cache-registry))
         (ivar (and reg (plist-get reg :instance-var-sym))))
    (unless (and ivar (boundp ivar))
      (error "Cacheus: No instance variable for cache '%s'" cache-name))
    (let ((instance (symbol-value ivar)))
      (unless (cacheus-instance-runtime-data instance)
        (let ((live-instance (copy-structure instance)))
          (cacheus--initialize-runtime-data live-instance)
          (set ivar live-instance)
          (setq instance live-instance)))
      instance)))

(defun cacheus--handle-hit (entry key instance)
  "Handle a cache hit for ENTRY. Returns the value."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (name (cacheus-options-name opts))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (data-acc (cacheus-symbols-data-accessor-for-entries syms))
         (val (funcall data-acc entry)))
    (funcall logger :debug "[C:%s] CACHE HIT for key: %S" name key)
    (cacheus-update-instance-on-hit key instance)
    val))

(defun cacheus--handle-miss-sync (instance key compute-thunk user-key)
  "Handle a synchronous cache miss by computing and storing the value."
  (let* ((opts (cacheus-instance-options instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         val)
    (funcall logger :debug "[C:%s] SYNC MISS for key: %S"
             (cacheus-options-name opts) user-key)
    (condition-case err (setq val (funcall compute-thunk))
      (error
       (funcall logger :error "[C:%s] Sync compute error for %S: %S"
                (cacheus-options-name opts) user-key err)
       (when-let (eh (cacheus-options-error-handler opts)) (funcall eh err))
       nil))
    (if (and val (or (null (cacheus-options-predicate opts))
                     (funcall (cacheus-options-predicate opts) val)))
        (let* ((tags (when-let (tfn (cacheus-options-tags-fn opts))
                       (funcall tfn user-key val)))
               (entry (cacheus-create-entry instance key val)))
          (cacheus-store-result entry key tags instance logger))
      (funcall logger :debug "[C:%s] Sync predicate rejected value for key: %S"
               (cacheus-options-name opts) user-key))
    val))

(defun cacheus--handle-miss-async (instance key compute-thunk user-key)
  "Handle an asynchronous cache miss, returning a promise."
  (require 'concur-chain)
  (let* ((opts (cacheus-instance-options instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
    (funcall logger :debug "[C:%s] ASYNC MISS for key: %S"
             (cacheus-options-name opts) user-key)
    (let ((promise (cacheus-async-result key compute-thunk instance logger)))
      (concur:chain promise
        (:then (lambda (val)
                 (if (and-let ((p (cacheus-options-predicate opts)))
                       (not (funcall p val)))
                     (progn (funcall logger :debug "[C:%s] Async predicate rejected"
                                     (cacheus-options-name opts))
                            (concur:rejected! (list :predicate-rejected val)))
                   val)))
        (:then (lambda (val)
                 (let* ((tags (when-let (tfn (cacheus-options-tags-fn opts))
                                (funcall tfn user-key val)))
                        (entry (cacheus-create-entry instance key val)))
                   (cacheus-store-result entry key tags instance logger)
                   val)))
        (:catch (lambda (reason)
                  (when-let (eh (cacheus-options-error-handler opts))
                    (funcall eh reason))
                  (cacheus-evict-one-entry key instance logger)
                  (concur:rejected! reason)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Functions

(cl-defun cacheus-get-or-compute (instance key compute-thunk &key user-key async)
  "Universal entry point to get a value from cache or compute it."
  (let* ((live-instance (cacheus-get-instance-by-name
                         (cacheus-options-name
                          (cacheus-instance-options instance))))
         (opts (cacheus-instance-options live-instance))
         (rtd (cacheus-instance-runtime-data live-instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (entry (gethash key (cacheus-runtime-data-cache-ht rtd)))
         (final-user-key (or user-key key)))
    (if (and entry (not (cacheus-is-entry-stale key entry live-instance logger)))
        (let ((val (cacheus--handle-hit entry key live-instance)))
          (if async (concur:resolved! val) val))
      (when entry
        (funcall logger :debug "[C:%s] STALE HIT for key: %S"
                 (cacheus-options-name opts) final-user-key)
        (cacheus-evict-one-entry key live-instance logger))
      (if async
          (cacheus--handle-miss-async live-instance key compute-thunk
                                      final-user-key)
        (cacheus--handle-miss-sync live-instance key compute-thunk
                                   final-user-key)))))

(cl-defun cacheus-put (cache-name key value &key tags)
  "Manually insert or update a VALUE for a given KEY in a cache."
  (let ((instance (cacheus-get-instance-by-name cache-name)))
    (let* ((opts (cacheus-instance-options instance))
           (p (cacheus-options-predicate opts))
           (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
      (if (and p (not (funcall p value)))
          (progn (funcall logger :info "[C:%s] Put: Skipped key %S (predicate)"
                          cache-name key) nil)
        (let ((entry (cacheus-create-entry instance key value)))
          (funcall logger :info "[C:%s] Put: Storing new value for key %S"
                   cache-name key)
          (cacheus-store-result entry key tags instance logger)
          value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Macros (Public)

;;;###autoload
(defmacro cacheus:get! (cache-name-form &rest args)
  "Retrieve a value from a cache, or compute it if missing.
This is the primary macro for interacting with Cacheus. It dispatches to
the correct underlying function based on whether the target cache is a
simple cache (taking a key and a thunk) or a memoized function (taking
arguments).

For a standard cache defined with `defcacheus`, the macro takes two arguments:
the key and a form to compute the value on a cache miss.

  (cacheus:get! 'my-cache \"some-key\"
    (lambda () (long-computation)))

For a memoized function defined with `defcacheus-memo`, you simply call it
like the original function.

  (my-memoized-function arg1 arg2) ; `cacheus:get!` is expanded by the memo macro.

Arguments:
- `CACHE-NAME-FORM` (symbol | form): The name of the cache, or a form that
  evaluates to the cache name symbol.
- `ARGS...`:
  - For a standard cache: A KEY and a COMPUTE-THUNK `(lambda () ...)`.
  - For a memoized function: The arguments to that function.

Returns:
  The cached value, the newly computed value, or a `concur-promise` for the
  value if the cache is asynchronous."
  (declare (indent 1))
  `(let* ((cache-name ,cache-name-form)
          (registry-entry (gethash cache-name cacheus-global-cache-registry)))
     (unless registry-entry
       (error "cacheus:get!: No cache registered for %S" cache-name))
     (pcase (plist-get registry-entry :type)
       ('memoize
        ;; For a memoized function, expand to a direct function call.
        (apply cache-name (list ,@args)))
       ('cache
        (let* ((key ,(car args))
               (compute-thunk ,(cadr args))
               (instance-var (plist-get registry-entry :instance-var-sym)))
          (cacheus-get-or-compute (symbol-value instance-var)
                                  key (or compute-thunk (lambda () nil))
                                  :user-key key)))
       (type (error "cacheus:get!: Unknown cache type '%s'" type)))))

;;;###autoload
(defmacro cacheus:put! (cache-name-form key value &rest kwargs)
  "Manually insert or update a VALUE for a given KEY in a cache.
This is useful for pre-populating a cache or for situations where
the value is computed outside of a `cacheus:get!` call.

Arguments:
- `CACHE-NAME-FORM` (symbol | form): The name of the cache.
- `KEY`: The key to associate with the value.
- `VALUE`: The value to store in the cache.
- `KWARGS...` (plist): Optional keyword arguments, such as `:tags`.
  - `:tags` (list): A list of symbols to tag this cache entry with.

Returns:
  The stored VALUE, or nil if the cache's predicate rejected it."
  (declare (indent 2))
  `(cacheus-put ,cache-name-form ,key ,value ,@kwargs))

;;;###autoload
(defmacro cacheus:clear! (cache-name-form)
  "Clear all entries from a specific Cacheus cache by its name.
This invokes the dynamically generated `clear` function for the cache.

Arguments:
- `CACHE-NAME-FORM` (symbol | form): The name of the cache.

Returns:
  `t` if the clear function was called, otherwise signals an error."
  (declare (indent 1))
  `(let* ((cache-name ,cache-name-form)
          (reg (gethash cache-name cacheus-global-cache-registry))
          (clear-fn (and reg (plist-get reg :clear-fn-symbol))))
     (if (and clear-fn (fboundp clear-fn))
         (funcall clear-fn)
       (error "cacheus:clear!: No clear function for cache '%S'" cache-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Cache Management (Public, Interactive)

;;;###autoload
(defun cacheus:list-all-caches ()
  "Display all registered Cacheus caches in a `tabulated-list` buffer.
This provides a dashboard for observing and managing all defined caches.
From the list buffer, you can perform actions on a cache:
  i: Inspect the cache's contents.
  c: Clear the cache.
  s: Save the cache to its file (if applicable).

Arguments:
  None.

Returns:
  None. This function's purpose is its side effect of displaying a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Cacheus Caches*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only nil)) (erase-buffer))
      (setq-local tabulated-list-format
                  [("Cache Name" 30 t) ("Type" 10 t) ("Entries" 10 :right-align)
                   ("Capacity" 10 :right-align) ("TTL (s)" 8 :right-align)
                   ("File" 35 t)])
      (setq-local tabulated-list-entries
                  (mapcar #'cacheus--cache-details-to-list-entry
                          (hash-table-values cacheus-global-cache-registry)))
      (tabulated-list-mode)
      (define-key tabulated-list-mode-map "i" #'cacheus--list-caches-inspect)
      (define-key tabulated-list-mode-map "c" #'cacheus--list-caches-clear)
      (define-key tabulated-list-mode-map "s" #'cacheus--list-caches-save)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)))

(defun cacheus--cache-details-to-list-entry (details)
  "Helper to format cache details for `tabulated-list-mode`."
  (let* ((name (plist-get details :name))
         (type (plist-get details :type))
         (opts (plist-get details :config))
         (ivar (plist-get details :instance-var-sym))
         (cap (cacheus-options-capacity opts))
         (ttl (cacheus-options-ttl opts))
         (file (cacheus-options-cache-file opts))
         (instance (and (boundp ivar) (symbol-value ivar)))
         (rtd (and instance (cacheus-instance-runtime-data instance)))
         (size (if-let (ht (and rtd (cacheus-runtime-data-cache-ht rtd)))
                   (hash-table-count ht) "N/A")))
    (list name (vector (format "%s" name) (format "%s" type) (format "%s" size)
                       (format "%s" (or cap "Inf"))
                       (format "%s" (or ttl "Inf"))
                       (format "%s" (or file "None"))))))

(defun cacheus--list-caches-operate (op-name op-fn-sym)
  "Helper to run an operation from the tabulated list."
  (let* ((name (tabulated-list-get-id))
         (reg (gethash name cacheus-global-cache-registry))
         (fn-sym (plist-get reg op-fn-sym)))
    (if (and fn-sym (fboundp fn-sym))
        (progn (funcall fn-sym)
               (message "Cacheus: Ran '%s' on '%s'" op-name name)
               (revert-buffer))
      (message "Cacheus: No '%s' function for cache '%s'" op-name name))))

(defun cacheus--list-caches-inspect () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "inspect" :inspect-fn-symbol))
(defun cacheus--list-caches-clear () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "clear" :clear-fn-symbol))
(defun cacheus--list-caches-save () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "save" :save-fn-symbol))

(defun cacheus--operate-on-all-caches (op-name op-fn-sym &optional name-filter)
  "Internal helper to perform an op on all or filtered registered caches."
  (let* ((logger (cacheus-resolve-logger nil)) (count 0)
         (caches (--filter
                  (or (not name-filter)
                      (string-match-p name-filter
                                      (symbol-name (plist-get it :name))))
                  (hash-table-values cacheus-global-cache-registry))))
    (if (null caches)
        (message "No caches found%s to %s"
                 (if name-filter (format " matching '%s'" name-filter) "") op-name)
      (-each caches
             (lambda (details)
               (let* ((name (plist-get details :name))
                      (fn-sym (plist-get details op-fn-sym)))
                 (if (and fn-sym (fboundp fn-sym))
                     (progn (funcall logger :info "Cacheus: %s-all: %s..."
                                     op-name name)
                            (condition-case e (funcall fn-sym)
                              (error (funcall logger :error
                                              "Cacheus: Failed %s %s: %S"
                                              op-name name e)))
                            (cl-incf count))
                   (funcall logger :warn "Cacheus: %s-all: Skipping %s (no fn)"
                            op-name name)))))
      (message "Attempted to %s %d cache(s)%s." op-name count
               (if name-filter (format " matching '%s'" name-filter) "")))))

;;;###autoload
(defun cacheus:clear-all-caches (&optional name-filter)
  "Clear all registered caches, with an optional regexp filter.

Arguments:
- `NAME-FILTER` (string or prefix-arg, optional): If called interactively with a
  prefix argument, prompts for a regexp to filter cache names. If provided as a
  string, it's used as a regexp directly.

Returns:
  None. This function's purpose is its side effect of clearing caches."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "clear" :clear-fn-symbol
   (if (consp name-filter)
       (read-string "Filter by cache name (regexp): ") name-filter)))

;;;###autoload
(defun cacheus:save-all-caches (&optional name-filter)
  "Save all registered, file-backed caches.

Arguments:
- `NAME-FILTER` (string or prefix-arg, optional): If called interactively with a
  prefix argument, prompts for a regexp to filter cache names. If provided as a
  string, it's used as a regexp directly.

Returns:
  None. This function's purpose is its side effect of saving caches."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "save" :save-fn-symbol
   (if (consp name-filter)
       (read-string "Filter by cache name (regexp): ") name-filter)))

;;;###autoload
(defun cacheus:load-all-caches (&optional name-filter)
  "Load all registered, file-backed caches.

Arguments:
- `NAME-FILTER` (string or prefix-arg, optional): If called interactively with a
  prefix argument, prompts for a regexp to filter cache names. If provided as a
  string, it's used as a regexp directly.

Returns:
  None. This function's purpose is its side effect of loading caches."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "load" :load-fn-symbol
   (if (consp name-filter)
       (read-string "Filter by cache name (regexp): ") name-filter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Entry Data Accessors

(defun cacheus--get-cache-symbols (cache-name)
  "Helper to retrieve the symbols struct for a given cache-name."
  (unless (symbolp cache-name)
    (error "Cache name must be a symbol: %S" cache-name))
  (let* ((reg (gethash cache-name cacheus-global-cache-registry))
         (syms (and reg (plist-get reg :symbols-struct))))
    (unless syms (error "No cache registered with name '%s'" cache-name))
    syms))

;;;###autoload
(defun cacheus:entry-data (cache-name entry)
  "Retrieve the data payload from a cache ENTRY struct for CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `ENTRY`: The cache entry struct object.

Returns:
  (any) The original data stored in the cache entry."
  (funcall (cacheus-symbols-data-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus:entry-timestamp (cache-name entry)
  "Retrieve the timestamp from a cache ENTRY struct for CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `ENTRY`: The cache entry struct object.

Returns:
  (float) The timestamp (from `float-time`) when the entry was created."
  (funcall (cacheus-symbols-ts-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus:entry-version (cache-name entry)
  "Retrieve the version from a cache ENTRY struct for CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `ENTRY`: The cache entry struct object.

Returns:
  (any) The version string or number associated with the entry."
  (funcall (cacheus-symbols-entry-ver-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus:entry-field (cache-name entry field)
  "Retrieve a custom FIELD value from a cache ENTRY struct.
This is used for caches defined with the `:fields` option.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `ENTRY`: The cache entry struct object.
- `FIELD` (symbol): The name of the custom field to access.

Returns:
  (any) The value of the custom field."
  (let* ((syms (cacheus--get-cache-symbols cache-name))
         (s-name (cacheus-symbols-struct-name-for-entries syms))
         (acc (intern (format "%s-%s" s-name field))))
    (unless (fboundp acc)
      (error "Field '%s' not found for cache '%s'" field cache-name))
    (funcall acc entry)))

;;;###autoload
(defun cacheus-get-underlying-cache-ht (cache-name)
  "Return the underlying hash table for a cache.
This function is intended for advanced introspection and debugging.
Modifying the returned hash table directly can corrupt the cache state.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache.

Returns:
  (hash-table) The raw hash table storing the cache entries."
  (let* ((instance (cacheus-get-instance-by-name cache-name))
         (rtd (cacheus-instance-runtime-data instance)))
    (and rtd (cacheus-runtime-data-cache-ht rtd))))

;;;###autoload
(defun cacheus:map (cache-name fn)
  "Apply FN to each key and entry struct in CACHE-NAME.
FN will be called with two arguments: the key and the entry struct.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache to iterate over.
- `FN` (function): A function of two arguments `(lambda (key entry) ...)`

Returns:
  None."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name))) (maphash fn ht)))

;;;###autoload
(defun cacheus:keys (cache-name)
  "Return a list of all keys in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache.

Returns:
  (list) A list of all keys currently in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-keys ht)))

;;;###autoload
(defun cacheus:values (cache-name)
  "Return a list of all raw entry structs in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache.

Returns:
  (list) A list of all `cacheus-entry` structs in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-values ht)))

;;;###autoload
(defun cacheus:size (cache-name)
  "Return the number of entries in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache.

Returns:
  (integer) The number of items currently in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-count ht)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shutdown Hook

(defun cacheus--run-save-all-caches-on-shutdown ()
  "Hook to save all file-backed caches on Emacs shutdown."
  (when cacheus-save-all-on-shutdown
    (let ((logger (cacheus-resolve-logger nil)))
      (condition-case e (cacheus:save-all-caches)
        (error (funcall logger :error "Cacheus: Error during shutdown save: %S"
                          e))))))

(add-hook 'kill-emacs-hook #'cacheus--run-save-all-caches-on-shutdown)

(provide 'cacheus-core)
;;; cacheus-core.el ends here