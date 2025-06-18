;;; cacheus-core.el --- Core Cacheus Library and Global Management -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; `cacheus-core.el` serves as the foundational library for the entire Cacheus
;; ecosystem. It defines the global cache registry, the high-level management
;; functions, and the primary user-facing macros (`cacheus-get!`,
;; `cacheus-put!`, etc.) that are shared by all other `cacheus-` modules.

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
  "Global registry of all Cacheus caches defined by various modules.
This hash table maps a cache's name (a symbol) to a plist
containing its configuration (`cacheus-options` struct), its symbols
(`cacheus-symbols` struct), and a template `cacheus-instance` struct.")

(define-error 'cacheus-error
  "Generic Cacheus error. All specific Cacheus errors inherit from this." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Generation Logic (Package-Private)

(defun cacheus-create-options (name-sym options-plist)
  "Create and validate a `cacheus-options` struct from a plist.

This is an internal setup function called by `defcacheus` to
process the user-provided options.

Arguments:
  NAME-SYM (symbol): The name of the cache being created.
  OPTIONS-PLIST (plist): The plist of options from the macro body.

Returns:
  (cacheus-options) A validated options struct."
  (cacheus-let* (((&plist :ttl ttl
                         :version ver
                         :capacity cap
                         :eviction-strategy evic
                         :cache-file file
                         :refresh-ttl refresh-ttl
                         :logger logger
                         :cache-expiration-hook exp-hook
                         :periodic-cleanup periodic
                         :predicate pred
                         :error-handler err-handler
                         :async async
                         :dirty-p dirty-p
                         :clear-hook clear-hook
                         :prefix prefix
                         :fields fields
                         :meta-fn meta-fn
                         :tags-fn tags-fn)
                 options-plist)
                 (final-evic (if (memq evic '(:lru :lfu :fifo :none nil)) evic :lru)))
    (when (and evic (not (eq evic final-evic)))
      (warn "cacheus: Invalid :eviction-strategy %S. Using :lru." evic))
    (cacheus-validate-fn-option pred :predicate)
    (cacheus-validate-fn-option err-handler :error-handler)
    (make-cacheus-options
     :name name-sym :logger logger :capacity cap :eviction-strategy final-evic
     :cache-file file :version ver :periodic-cleanup periodic
     :predicate pred :async async :error-handler err-handler :ttl ttl
     :refresh-ttl refresh-ttl :expiration-hook exp-hook :dirty-p dirty-p
     :clear-hook clear-hook :prefix (or prefix cacheus-default-prefix)
     :fields-data fields :meta-fn meta-fn :tags-fn tags-fn)))

(defun cacheus--generate-symbol-plist (sym-prefix)
  "Generate a plist of cache-related symbols based on a prefix.
This is a file-local helper for `cacheus-generate-symbols`."
  (let* ((specs '((:cache-var           "-ht")
                  (:timestamps-var      "-timestamps-ht")
                  (:order-ring-or-queue-var "-order-queue")
                  (:size-var            "-capacity-var")
                  (:frequency-var       "-frequency-ht")
                  (:version-id-var      "-version-id")
                  (:save-fn             "-save")
                  (:clear-fn            "-clear")
                  (:inspect-cache-fn    "-inspect")
                  (:inspect-entry-fn    "-inspect-entry")
                  (:entry-tags-var      "-entry-tags-ht")
                  (:tags-idx-var        "-tags-idx-ht")
                  (:invalidate-tags-fn  "-invalidate-by-tags")
                  (:load-fn             "-load")
                  (:inflight-var        "-inflight-ht")))
         (plist (cl-loop for (key suffix) in specs
                         collect key and collect
                         (intern (format "%s%s" sym-prefix suffix)))))
    (append plist nil)))

(defun cacheus-generate-symbols (options-struct)
  "Generate and return a `cacheus-symbols` struct based on options.

This function performs the metaprogramming step of creating all
the necessary symbol names for a new cache's functions, variables,
and struct accessors.

Arguments:
  OPTIONS-STRUCT (cacheus-options): The configuration for the new cache.

Returns:
  (cacheus-symbols) A struct containing all generated symbols."
  (cacheus-let* (((&struct :name name :prefix prefix :fields-data fields)
                 options-struct)
                 (sym-prefix (format "%s--%s" prefix (symbol-name name)))
                 (s-name (intern (format "%s-entry" sym-prefix)))
                 (ctor (intern (format "make-%s" s-name)))
                 (key-acc (intern (format "%s-key" s-name)))
                 (ts-acc (intern (format "%s-timestamp" s-name)))
                 (data-acc (intern (format "%s-data" s-name)))
                 (ver-acc (intern (format "%s-entry-version" s-name)))
                 (unique-fields (cl-remove-duplicates fields :key #'car :test #'eq))
                 (all-fields `((key nil :read-only t)
                               (data nil :read-only t)
                               (timestamp nil :type (or ts null))
                               (entry-version nil :read-only t)
                               ,@unique-fields)))
    (apply #'make-cacheus-symbols
           :sym-prefix sym-prefix
           :struct-name-for-entries s-name
           :make-fn-constructor-for-entries ctor
           :key-accessor-for-entries key-acc
           :ts-accessor-for-entries ts-acc
           :data-accessor-for-entries data-acc
           :entry-ver-accessor-for-entries ver-acc
           :all-struct-fields-for-entries all-fields
           (cacheus--generate-symbol-plist sym-prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Private Helpers

(defun cacheus--initialize-runtime-data (instance)
  "Create and attach a new `cacheus-runtime-data` struct to an INSTANCE.
This file-local helper performs the lazy initialization of a cache's
live data structures (hash tables, ring buffers, etc.)."
  (cacheus-let* (((&struct :options opts) instance)
                 ((&struct :capacity cap :eviction-strategy evic) opts)
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

This is a critical package-private function. It retrieves the cache
definition from the global registry. If the cache has not been used
yet in the current session, its runtime data (e.g., hash tables)
will be `nil`. In this case, this function creates those data
structures and attaches them to a live copy of the instance.

Arguments:
  CACHE-NAME (symbol): The name of the cache to retrieve.

Returns:
  (cacheus-instance) The live, initialized cache instance struct."
  (let* ((reg (gethash cache-name cacheus-global-cache-registry))
         (ivar (and reg (plist-get reg :instance-var-sym))))
    (unless (and ivar (boundp ivar))
      (error "Cacheus: No instance variable for cache '%s'" cache-name))
    (let ((instance (symbol-value ivar)))
      (unless (cacheus-instance-runtime-data instance)
        ;; The instance is a template. Make a live copy and initialize it.
        (let ((live-instance (copy-sequence instance)))
          (cacheus--initialize-runtime-data live-instance)
          ;; Replace the template with the live instance for this session.
          (setf (symbol-value ivar) live-instance)
          (setq instance live-instance)))
      instance)))

(defun cacheus--handle-hit (entry key instance)
  "Handle a cache hit for ENTRY. Returns the value."
  (cacheus-let* (((&struct :options opts :symbols syms) instance)
                 ((&struct :name name :logger logger-opt) opts)
                 ((&struct :data-accessor-for-entries data-acc) syms)
                 (logger (cacheus-resolve-logger logger-opt))
                 (val (funcall data-acc entry)))
    (funcall logger :debug "[C:%s] CACHE HIT" name)
    (cacheus-update-instance-on-hit key instance)
    val))

(defun cacheus--handle-miss-sync (instance key compute-thunk user-key)
  "Handle a synchronous cache miss by computing and storing the value."
  (cacheus-let* (((&struct :options opts) instance)
                 ((&struct :name name :logger lopt :predicate p :tags-fn tfn
                           :error-handler eh) opts)
                 (logger (cacheus-resolve-logger lopt))
                 (val nil))
    (funcall logger :debug "[C:%s] SYNC MISS for key: %S. Computing." name user-key)
    (condition-case-unless-debug err
        (setq val (funcall compute-thunk))
      (error
       (funcall logger :error "[C:%s] Sync compute error for %S: %S"
                name user-key err :trace)
       (when eh (funcall eh err))
       nil))
    (if (and val (or (null p) (funcall p val)))
        (let* ((tags (if tfn (funcall tfn user-key val)))
               (entry (cacheus-create-entry instance key val)))
          (cacheus-store-result entry key tags instance logger))
      (funcall logger :debug "[C:%s] Sync predicate rejected: %S" name user-key))
    val))

(defun cacheus--handle-miss-async (instance key compute-thunk user-key)
  "Handle an asynchronous cache miss, returning a promise."
  (require 'concur)
  (cacheus-let* (((&struct :options opts) instance)
                 ((&struct :name name :logger lopt :error-handler eh
                           :predicate p :tags-fn tfn) opts)
                 (logger (cacheus-resolve-logger lopt)))
    (funcall logger :debug "[C:%s] ASYNC MISS for key: %S. Computing." name user-key)
    (let ((promise (cacheus-async-result key compute-thunk instance logger)))
      (concur:chain promise
        (:then (lambda (val)
                 (if (and p (not (funcall p val)))
                     (progn
                       (funcall logger :debug "[C:%s] Async predicate rejected" name)
                       (concur:rejected! (list :predicate-rejected val)))
                   val)))
        (:then (lambda (val)
                 (let* ((tags (if tfn (funcall tfn user-key val)))
                        (entry (cacheus-create-entry instance key val)))
                   (cacheus-store-result entry key tags instance logger)
                   val)))
        (:catch (lambda (reason)
                  (funcall logger :error "[C:%s] Async compute error for %S: %S"
                           name user-key reason :trace)
                  (when eh (funcall eh reason))
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
         (cache-ht (cacheus-runtime-data-cache-ht rtd))
         (entry (gethash key cache-ht))
         (final-user-key (or user-key key)))
    (if (and entry (not (cacheus-is-entry-stale key entry live-instance logger)))
        (let ((val (cacheus--handle-hit entry key live-instance)))
          (if async
              (progn (require 'concur)
                     (if (concur-promise-p val) val (concur:resolved! val)))
            val))
      (progn
        (when entry
          (funcall logger :debug "[C:%s] STALE HIT for key: %S. Evicting."
                   (cacheus-options-name opts) final-user-key)
          (cacheus-evict-one-entry key live-instance logger))
        (if async
            (cacheus--handle-miss-async live-instance key compute-thunk final-user-key)
          (cacheus--handle-miss-sync live-instance key compute-thunk final-user-key))))))

(cl-defun cacheus-put (cache-name key value &key tags)
  "Manually insert or update a VALUE for a given KEY in a cache."
  (let ((instance (cacheus-get-instance-by-name cache-name)))
    (cacheus-let* (((&struct :options opts) instance)
                   (p (cacheus-options-predicate opts))
                   (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
                   (final-key key))
      (when (eq (type-of instance) 'cacheus-memoize-instance)
        (let* ((key-fn (or (cacheus-memoize-options-key-fn opts) #'list))
               (ver (cacheus-options-version opts))
               (comp-key (apply key-fn key)))
          (setq final-key (if ver (list comp-key ver) comp-key))))
      (if (and p (not (funcall p value)))
          (progn
            (funcall logger :info "[C:%s] Put: Skipped key %S (predicate)"
                     cache-name final-key)
            nil)
        (let ((entry (cacheus-create-entry instance final-key value)))
          (funcall logger :info "[C:%s] Put: Storing new value for key %S"
                   cache-name final-key)
          (cacheus-store-result entry final-key tags instance logger)
          value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Macros (Public)

;;;###autoload
(defmacro cacheus-get! (cache-name-form &rest args)
  "Retrieve a value from a cache, or compute it if missing.

This is the primary macro for interacting with Cacheus. It
dispatches to the correct underlying function based on whether the
target cache is a simple cache or a memoized function.

Arguments:
  CACHE-NAME-FORM (symbol | form): The name of the cache, or a form
    that evaluates to the cache name symbol.
  ARGS...: For a memoized function, the arguments to that function.
    For a standard cache, this should be two forms: the KEY and a
    COMPUTE-THUNK (a zero-argument lambda that computes the value).

Returns:
  The cached value, the newly computed value, or a promise if the
  cache is asynchronous."
  (declare (indent 1))
  ;; The entire body of this backquote is the macro expansion. It generates
  ;; code that will run when the user's code is executed.
  `(let* ((cache-name ,cache-name-form)
          (registry-entry (gethash cache-name cacheus-global-cache-registry)))
     (unless registry-entry
       (error "cacheus-get!: No cache registered for %S" cache-name))
     ;; This `pcase` runs at runtime, inside the expanded code.
     (pcase (plist-get registry-entry :type)
       ('memoize
        ;; For a memoized function, expand to a direct function call.
        ;; The function's name is the same as the cache's name.
        (apply cache-name (list ,@args)))
       ('cache
        (let* ((key ,(car args))
               (compute-thunk ,(cadr args))
               (instance-var (plist-get registry-entry :instance-var-sym)))
          ;; For a standard cache, expand to a call to the universal helper.
          ;; This is the location of the fix. This form is now executed
          ;; rather than being returned as a quoted list.
          (cacheus-get-or-compute
           (symbol-value instance-var)
           key
           (or compute-thunk (lambda () nil))
           :user-key key)))
       (type (error "cacheus-get!: Unknown cache type '%s'" type)))))

;;;###autoload
(defmacro cacheus-put! (cache-name-form key value &rest kwargs)
  "Manually insert or update a VALUE for a given KEY in a cache.

Arguments:
  CACHE-NAME-FORM (symbol | form): The name of the cache, or a form
    that evaluates to the cache name symbol.
  KEY: The key to associate with the value.
  VALUE: The value to store in the cache.
  KWARGS... (plist): Optional keyword arguments, such as `:tags`.

Returns:
  The stored VALUE, or nil if the cache's predicate rejected it."
  (declare (indent 2))
  `(cacheus-put ,cache-name-form ,key ,value ,@kwargs))

;;;###autoload
(defmacro cacheus-clear! (cache-name-form)
  "Clear all entries from a specific Cacheus cache by its name.

Arguments:
  CACHE-NAME-FORM (symbol | form): The name of the cache, or a form
    that evaluates to the cache name symbol.

Returns:
  t if the clear function was called, otherwise signals an error."
  (declare (indent 1))
  `(let* ((cache-name ,cache-name-form)
          (registry-entry (gethash cache-name cacheus-global-cache-registry))
          (clear-fn-sym (and registry-entry
                             (plist-get registry-entry :clear-fn-symbol))))
     (if (and clear-fn-sym (fboundp clear-fn-sym))
         (funcall clear-fn-sym)
       (error "cacheus-clear!: No clear function for cache '%S'" cache-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Cache Management (Public, Interactive)

;;;###autoload
(defun cacheus-list-all-caches ()
  "Display all registered Cacheus caches in a tabulated list.

From the list buffer, you can perform actions on a cache:
  i: Inspect the cache's contents.
  c: Clear the cache.
  s: Save the cache to its file (if applicable).

Arguments: None.
Returns: None."
  (interactive)
  (let ((buffer (get-buffer-create "*Cacheus Caches*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only nil)) (erase-buffer))
      (setq-local tabulated-list-format
                  [("Cache Name" 30 t)
                   ("Type" 10 t)
                   ("Entries" 10 :right-align)
                   ("Capacity" 10 :right-align)
                   ("TTL (s)" 8 :right-align)
                   ("File" 35 t)])
      (setq-local tabulated-list-entries
                  (--map #'cacheus--cache-details-to-list-entry
                        (let ((vals nil))
                          (maphash (lambda (k v) (push v vals))
                                   cacheus-global-cache-registry)
                          vals)))
      (tabulated-list-mode)
      (define-key tabulated-list-mode-map "i" #'cacheus--list-caches-inspect)
      (define-key tabulated-list-mode-map "c" #'cacheus--list-caches-clear)
      (define-key tabulated-list-mode-map "s" #'cacheus--list-caches-save)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)))

(defun cacheus--cache-details-to-list-entry (details)
  "File-local helper to format cache details for `tabulated-list-mode`."
  (cacheus-let* (((&plist :name name :type type :config opts :instance-var-sym ivar)
                 details)
                 ((&struct :capacity cap :ttl ttl :cache-file file) opts)
                 (instance (and (boundp ivar) (symbol-value ivar)))
                 (rtd (and instance (cacheus-instance-runtime-data instance)))
                 (size (if-let ((ht (and rtd (cacheus-runtime-data-cache-ht rtd))))
                           (hash-table-count ht) "N/A")))
    (list name (vector (format "%s" name)
                       (format "%s" type)
                       (format "%s" size)
                       (format "%s" (or cap "None"))
                       (format "%s" (or ttl "None"))
                       (format "%s" (or file "None"))))))

(defun cacheus--list-caches-operate (op-name op-fn-sym)
  "File-local helper to run an operation from the tabulated list."
  (let* ((name (tabulated-list-get-id))
         (registry-entry (gethash name cacheus-global-cache-registry))
         (fn-sym (plist-get registry-entry op-fn-sym)))
    (if (and fn-sym (fboundp fn-sym))
        (progn
          (funcall fn-sym)
          (message "Cacheus: Ran '%s' on '%s'." op-name name)
          (revert-buffer))
      (message "Cacheus: No '%s' function for cache '%s'." op-name name))))

(defun cacheus--list-caches-inspect () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "inspect" :inspect-fn-symbol))
(defun cacheus--list-caches-clear () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "clear" :clear-fn-symbol))
(defun cacheus--list-caches-save () "Interactive helper." (interactive)
  (cacheus--list-caches-operate "save" :save-fn-symbol))

(defun cacheus--operate-on-all-caches (op-name op-fn-sym &optional name-filter)
  "Internal helper to perform an op on all or filtered registered caches."
  (let* ((logger (cacheus-resolve-logger nil))
         (count 0)
         (all-details (let (vals)
                        (maphash (lambda (k v) (push v vals))
                                 cacheus-global-cache-registry)
                        vals))
         (caches (--filter (lambda (d)
                             (or (not name-filter) (string-empty-p name-filter)
                                 (string-match-p
                                  name-filter
                                  (symbol-name (plist-get d :name)))))
                           all-details)))
    (if (null caches)
        (message "No caches found%s to %s."
                 (if name-filter (format " matching '%s'" name-filter) "") op-name)
      (-each caches
             (lambda (details)
               (let* ((name (plist-get details :name))
                      (fn-sym (plist-get details op-fn-sym)))
                 (if (and fn-sym (fboundp fn-sym))
                     (progn
                       (funcall logger :info "Cacheus: %s-all: %s..." op-name name)
                       (condition-case-unless-debug e (funcall fn-sym)
                         (error
                          (funcall logger :error "Cacheus: Failed to %s %s: %S"
                                   op-name name e)))
                       (cl-incf count))
                   (funcall logger :warn "Cacheus: %s-all: Skipping %s (no valid fn)."
                            op-name name))))))
    (message "Attempted to %s %d cache(s)%s."
             op-name count (if name-filter (format " matching '%s'" name-filter) ""))))

;;;###autoload
(defun cacheus-clear-all-caches (&optional name-filter)
  "Clear all registered caches, with an optional regexp filter.

Arguments:
  NAME-FILTER (string or prefix-arg): If called interactively with a
    prefix argument, prompts for a regexp to filter cache names.
    If called programmatically, a string regexp.

Returns: None."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "clear" :clear-fn-symbol
   (if (consp name-filter) (read-string "Filter by cache name (regexp): ") name-filter)))

;;;###autoload
(defun cacheus-save-all-caches (&optional name-filter)
  "Save all registered, file-backed caches.

Arguments:
  NAME-FILTER (string or prefix-arg): If called interactively with a
    prefix argument, prompts for a regexp to filter cache names.

Returns: None."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "save" :save-fn-symbol
   (if (consp name-filter) (read-string "Filter by cache name (regexp): ") name-filter)))

;;;###autoload
(defun cacheus-load-all-caches (&optional name-filter)
  "Load all registered, file-backed caches.

Arguments:
  NAME-FILTER (string or prefix-arg): If called interactively with a
    prefix argument, prompts for a regexp to filter cache names.

Returns: None."
  (interactive "P")
  (cacheus--operate-on-all-caches
   "load" :load-fn-symbol
   (if (consp name-filter) (read-string "Filter by cache name (regexp): ") name-filter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Entry Data Accessors

(defun cacheus--get-cache-symbols (cache-name)
  "File-local helper to retrieve the symbols struct for a given cache-name."
  (unless (symbolp cache-name) (error "Cache name must be a symbol, got %S" cache-name))
  (let* ((reg (gethash cache-name cacheus-global-cache-registry))
         (syms (and reg (plist-get reg :symbols-struct))))
    (unless syms (error "No cache registered with name '%s'." cache-name))
    syms))

;;;###autoload
(defun cacheus-entry-data (cache-name entry)
  "Retrieve data from a cache ENTRY struct for CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache the entry belongs to.
  ENTRY (struct): The cache entry struct itself.

Returns:
  The raw data stored in the cache entry."
  (funcall (cacheus-symbols-data-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus-entry-timestamp (cache-name entry)
  "Retrieve timestamp from a cache ENTRY struct for CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache the entry belongs to.
  ENTRY (struct): The cache entry struct itself.

Returns:
  A `ts` struct representing the entry's timestamp."
  (funcall (cacheus-symbols-ts-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus-entry-version (cache-name entry)
  "Retrieve version from a cache ENTRY struct for CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache the entry belongs to.
  ENTRY (struct): The cache entry struct itself.

Returns:
  The version string or number associated with the entry."
  (funcall (cacheus-symbols-entry-ver-accessor-for-entries
            (cacheus--get-cache-symbols cache-name))
           entry))

;;;###autoload
(defun cacheus-entry-field (cache-name entry field)
  "Retrieve a custom FIELD value from a cache ENTRY struct.

Arguments:
  CACHE-NAME (symbol): The name of the cache the entry belongs to.
  ENTRY (struct): The cache entry struct itself.
  FIELD (symbol): The name of the custom field to access.

Returns:
  The value of the custom field."
  (let* ((syms (cacheus--get-cache-symbols cache-name))
         (s-name (cacheus-symbols-struct-name-for-entries syms))
         (acc (intern (format "%s-%s" (symbol-name s-name) (symbol-name field)))))
    (unless (fboundp acc)
      (error "Field '%s' not found for cache '%s'." field cache-name))
    (funcall acc entry)))

;;;###autoload
(defun cacheus-get-underlying-cache-ht (cache-name)
  "Return the underlying hash table for a cache.

For advanced introspection. Use with caution, do not modify directly.

Arguments:
  CACHE-NAME (symbol): The name of the cache.

Returns:
  (hash-table) The raw hash table storing the cache entries."
  (let* ((instance (cacheus-get-instance-by-name cache-name))
         (rtd (cacheus-instance-runtime-data instance)))
    (and rtd (cacheus-runtime-data-cache-ht rtd))))

;;;###autoload
(defun cacheus-map (cache-name fn)
  "Apply FN to each key and entry struct in CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache to iterate over.
  FN (function): A function of two arguments: (KEY, ENTRY-STRUCT)."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (maphash fn ht)))

;;;###autoload
(defun cacheus-keys (cache-name)
  "Return a list of all keys in CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache.

Returns:
  (list) A list of all keys currently in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-keys ht)))

;;;###autoload
(defun cacheus-values (cache-name)
  "Return a list of all raw entry structs in CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache.

Returns:
  (list) A list of all entry structs currently in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-values ht)))

;;;###autoload
(defun cacheus-size (cache-name)
  "Return the number of entries in CACHE-NAME.

Arguments:
  CACHE-NAME (symbol): The name of the cache.

Returns:
  (integer) The number of items in the cache."
  (when-let ((ht (cacheus-get-underlying-cache-ht cache-name)))
    (hash-table-count ht)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shutdown Hook

(defun cacheus--run-save-all-caches-on-shutdown ()
  "Hook to save all file-backed caches on Emacs shutdown."
  (when cacheus-save-all-on-shutdown
    (let ((logger (cacheus-resolve-logger nil)))
      (condition-case-unless-debug err (cacheus-save-all-caches)
        (error (funcall logger :error "Cacheus: Error during shutdown save: %S"
                        err :trace))))))

(add-hook 'kill-emacs-hook #'cacheus--run-save-all-caches-on-shutdown)

(provide 'cacheus-core)
;;; cacheus-core.el ends here