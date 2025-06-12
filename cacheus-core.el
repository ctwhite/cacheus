;;; cacheus-core.el --- Core Cacheus Library and Global Management -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `cacheus-core.el` serves as the foundational library for the entire Cacheus
;; ecosystem. It defines the global cache registry and the high-level
;; management functions that are shared by all other `cacheus-` modules.
;;
;; This file consolidates the global cache registry and the top-level
;; commands for managing all registered caches, making it the central point
;; for cache oversight and interaction.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)
(require 's)

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
  "The default string prefix used for all generated functions and variables.
You can `setq` this variable in your config to a different string (e.g.,
\"my-app\") to place all generated cache entities into a different
namespace by default."
  :type 'string
  :group 'cacheus)

(defcustom cacheus-save-all-on-shutdown t
  "If non-nil, automatically save all file-backed caches on Emacs shutdown."
  :type 'boolean
  :group 'cacheus)

(defvar cacheus-global-cache-registry (ht-create)
  "Global registry of all Cacheus caches defined by various modules.
Keys are the cache names (symbols). Values are plists containing cache
details, including its `:config` options struct and `:type`.")

(define-error 'cacheus-error
  "Generic Cacheus error. All specific Cacheus errors inherit from this." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Generation Logic

(defun cacheus-create-options (name-sym options-plist)
  "Create and validate a base `cacheus-options` struct from a plist.
This core function handles all options that are generic to any cache type.

Arguments:
- `NAME-SYM`: The base name symbol for the cache.
- `OPTIONS-PLIST`: A property list of user-provided cache options.

Returns:
  (cacheus-options): A validated `cacheus-options` struct instance."
  (-let-pattern*
      (((&plist :ttl ttl-opt :version version-opt :capacity capacity-opt
               :eviction-strategy eviction-strategy-opt
               :cache-file cache-file-opt
               :refresh-ttl-on-access refresh-ttl-opt :logger logger-opt
               :cache-expiration-hook expiration-hook-opt
               :periodic-cleanup periodic-cleanup-opt :predicate predicate-opt
               :error-handler error-handler-opt :async async-opt
               :dirty-p dirty-p-opt :clear-hook clear-hook-opt
               :prefix prefix-str :fields fields-data :meta-fn meta-fn-opt
               :tags-fn tags-fn-opt)
        options-plist)
       (final-eviction-strategy
        (if (memq eviction-strategy-opt '(:lru :lfu :fifo :none nil))
            eviction-strategy-opt :lru)))
    ;; Perform generic validation at macro-expansion time.
    (when (and eviction-strategy-opt
               (not (eq eviction-strategy-opt final-eviction-strategy)))
      (warn "cacheus: Invalid :eviction-strategy %S. Using :lru."
            eviction-strategy-opt))
    (when (and cache-file-opt (not (stringp cache-file-opt))
               (not (listp cache-file-opt)))
      (warn "cacheus: :cache-file should be a file path string or a form, got %S"
            cache-file-opt))
    (cacheus-validate-fn-option predicate-opt :predicate)
    (cacheus-validate-fn-option error-handler-opt :error-handler)
    (cacheus-validate-fn-option dirty-p-opt :dirty-p)
    (cacheus-validate-fn-option clear-hook-opt :clear-hook)
    (cacheus-validate-fn-option expiration-hook-opt :expiration-hook)
    (cacheus-validate-fn-option meta-fn-opt :meta-fn)
    (cacheus-validate-fn-option tags-fn-opt :tags-fn)

    (make-cacheus-options
     :name name-sym
     :logger logger-opt
     :capacity capacity-opt
     :eviction-strategy final-eviction-strategy
     :cache-file cache-file-opt
     :version version-opt
     :periodic-cleanup periodic-cleanup-opt
     :predicate predicate-opt
     :async async-opt
     :error-handler error-handler-opt
     :ttl ttl-opt
     :refresh-ttl refresh-ttl-opt
     :expiration-hook expiration-hook-opt
     :dirty-p dirty-p-opt
     :clear-hook clear-hook-opt
     :prefix (or prefix-str cacheus-default-prefix)
     :fields-data fields-data
     :meta-fn meta-fn-opt
     :tags-fn tags-fn-opt)))

(defun cacheus--generate-symbol-plist (sym-prefix ttl-opt refresh-ttl-opt
                                       capacity-opt eviction-strategy-opt
                                       file-opt tags-fn-opt)
  "Generate a plist of cache-related symbols based on enabled features."
  (let* ((symbol-specs
          '((:cache-var           "-ht" t)
            (:timestamps-var      "-timestamps-ht" (and ttl-opt refresh-ttl-opt))
            (:order-ring-or-queue-var "-order-queue"
             (and capacity-opt (memq eviction-strategy-opt '(:lru :fifo))))
            (:size-var            "-capacity-var" t)
            (:frequency-var       "-frequency-ht"
             (and capacity-opt (eq eviction-strategy-opt :lfu)))
            (:version-id-var      "-version-id" t)
            (:save-fn             "-save" file-opt)
            (:clear-fn            "-clear" t)
            (:inspect-cache-fn    "-inspect" t)
            (:inspect-entry-fn    "-inspect-entry" t)
            (:entry-tags-var      "-entry-tags-ht" tags-fn-opt)
            (:tags-idx-var        "-tags-idx-ht" tags-fn-opt)
            (:invalidate-tags-fn  "-invalidate-by-tags" tags-fn-opt)
            (:get-fn              "-get" t)
            (:put-fn              "-put" t)
            (:load-fn             "-load" file-opt)))
         (generated-syms-plist
          (cl-loop for (key suffix condition) in symbol-specs
                   when condition
                   collect key
                   and collect (intern (format "%s%s" sym-prefix suffix)))))
    (append generated-syms-plist (list :inflight-var nil))))

(defun cacheus-generate-symbols (options-struct)
 "Generate and return a `cacheus-symbols` struct.
 This is the core function for creating the unique symbols for a cache's
 internal variables, helper functions, and the complete definition of its
 entry struct (including custom fields).
 
 Arguments:
 - `OPTIONS-STRUCT`: The `cacheus-options` instance for the cache.
 
 Returns:
   (cacheus-symbols): A fully populated `cacheus-symbols` struct instance."
  (-let-pattern*
      (((&struct :name name-sym :prefix prefix-str :ttl ttl-opt
                 :refresh-ttl refresh-ttl-opt :capacity capacity-opt
                 :eviction-strategy eviction-strategy-opt
                 :cache-file file-opt :tags-fn tags-fn-opt
                 :fields-data fields-data)
        options-struct)
       (sym-prefix (format "%s--%s" prefix-str (symbol-name name-sym)))
       (s-name (intern (format "%s-entry" sym-prefix)))
       (ctor (intern (format "make-%s" s-name)))
       (ts-acc (intern (format "%s-timestamp" s-name)))
       (data-acc (intern (format "%s-data" s-name)))
       (ver-acc (intern (format "%s-entry-version" s-name)))
       (unique-fields (cl-remove-duplicates fields-data :key #'car :test #'eq))
       (all-fields `((data nil :read-only t)
                     (timestamp nil :type (or ts null))
                     (entry-version nil :read-only t)
                     ,@unique-fields)))
    (apply #'make-cacheus-symbols
           :sym-prefix sym-prefix
           :struct-name-for-entries s-name
           :make-fn-constructor-for-entries ctor
           :ts-accessor-for-entries ts-acc
           :data-accessor-for-entries data-acc
           :entry-ver-accessor-for-entries ver-acc
           :all-struct-fields-for-entries all-fields
           (cacheus--generate-symbol-plist
            sym-prefix ttl-opt refresh-ttl-opt capacity-opt
            eviction-strategy-opt file-opt tags-fn-opt))))

;;;###autoload
(defmacro cacheus-cache! (name &rest body-and-options)
  "Define a generic key-value cache named NAME.

This macro creates a new cache with a simple key-value store.
It generates all necessary backing variables and helper functions
based on the provided options.

Arguments:
- `NAME` (symbol): The unique name for the new cache.
- `BODY-AND-OPTIONS` (keyword arguments): A property list of options to
  configure the cache's behavior, such as `:ttl`, `:capacity`,
  `:eviction-strategy`, and `:cache-file`.

Returns:
  (progn ...): A set of Lisp forms that define the cache's infrastructure.

Example:
  (cacheus-cache! my-project-files
    \"A cache for project file contents.\"
    :capacity 100
    :ttl 3600
    :cache-file \"~/.emacs.d/cache/my-project-files.v1.dat\")"
  (declare (indent 1))
  (let* ((docstring (when (stringp (car body-and-options))
                      (car body-and-options)))
         (opts-plist (if docstring
                         (cdr body-and-options)
                       body-and-options)))
    (unless (cacheus-plist-valid-p opts-plist)
      (error "cacheus-cache!: Options are not a valid plist: %S" opts-plist))
    (let* ((final-opts (if (plist-member opts-plist :name)
                           opts-plist
                         (plist-put opts-plist :name name)))
           (instance (make-cacheus-cache-instance
                      :options (cacheus-create-options name final-opts)
                      :symbols (cacheus-generate-symbols
                                (cacheus-create-options name final-opts))
                      :runtime-data (make-cacheus-runtime-data))))
      (ignore docstring)
      (cacheus-make-cache-backend
       instance
       :instance-constructor 'make-cacheus-cache-instance))))

;;;###autoload
(defmacro cacheus-get! (cache-name-form &rest args)
  "Retrieve a value from any Cacheus cache by name.

For a generic cache, this returns the value for a given key, or
nil on a cache miss. For a memoized function, this triggers the
original function's execution on a miss to compute and cache
the value.

Arguments:
- `CACHE-NAME-FORM` (quoted symbol): The name of the target cache,
  e.g., `'my-cache`.
- `ARGS` (rest arguments): For a generic cache, this is a single KEY.
  For a memoized function, these are the arguments to the original
  function, which are used to compute the cache key.

Returns:
  (form): A Lisp form that, when evaluated, returns the cached value,
          or computes and returns it in the case of a memoized function miss.

Example (Generic Cache):
  (cacheus-get! 'my-project-cache \"some-key\")

Example (Memoized Function `my-expensive-func`):
  (cacheus-get! 'my-expensive-func arg1 arg2)"
  (declare (indent 1))
  (cl-block cacheus-get!
    (let* ((cache-name (when (eq 'quote (car-safe cache-name-form))
                         (cadr cache-name-form)))
           (registry-entry
            (when cache-name
              (gethash cache-name cacheus-global-cache-registry))))
      (unless cache-name
        (error "cacheus-get!: Argument must be a quoted symbol, got %S"
               cache-name-form))
      (if registry-entry
          (-let-pattern*
              (((&plist :get-fn-symbol get-fn-sym
                        :config options :type cache-type)
               registry-entry))
            (unless get-fn-sym
              (warn "cacheus-get!: No :get-fn-symbol for %S" cache-name)
              (cl-return-from cacheus-get! nil))
            (let ((key-form
                   (if (memq cache-type '(memoize memoize-fn))
                       (-let-pattern*
                           ((&struct :key-fn key-fn :arglist arglist :version ver)
                            options)
                         (let ((key-gen (or key-fn `(lambda ,arglist
                                                      (list ,@arglist)))))
                           `(let ((key (funcall #',key-gen ,@args)))
                              (if ',ver (list key ',ver) key))))
                     (if (= 1 (length args))
                         (car args)
                       (error "cacheus-get!: Generic cache expects one key")))))
              `(funcall #',get-fn-sym ,key-form t)))
          (warn "cacheus-get!: No cache registered for %S" cache-name)
        nil))))

;;;###autoload
(defmacro cacheus-put! (cache-name-form value &rest args)
  "Insert or update a VALUE in any Cacheus cache by name.

Arguments:
- `CACHE-NAME-FORM` (quoted symbol): The name of the target cache,
  e.g., `'my-cache`.
- `VALUE` (any): The value to insert into the cache.
- `ARGS` (rest arguments): For a generic cache, a single KEY. For
  a memoized function, these are the arguments used to compute the key.

Returns:
  (form): A Lisp form that, when evaluated, inserts the value
          and returns the inserted `VALUE`.

Example (Generic Cache):
  (cacheus-put! 'my-project-cache \"new-value\" \"some-key\")

Example (Memoized Function `my-expensive-func`):
  (cacheus-put! 'my-expensive-func 42 arg1 arg2)"
  (declare (indent 2))
  (cl-block cacheus-put!
    (let* ((cache-name (when (eq 'quote (car-safe cache-name-form))
                         (cadr cache-name-form)))
           (registry-entry
            (when cache-name
              (gethash cache-name cacheus-global-cache-registry))))
      (unless cache-name
        (error "cacheus-put!: Argument must be a quoted symbol, got %S"
               cache-name-form))
      (if registry-entry
          (-let-pattern*
              (((&plist :put-fn-symbol put-fn-sym
                        :config options :type cache-type)
               registry-entry))
            (unless put-fn-sym
              (warn "cacheus-put!: No :put-fn-symbol for %S" cache-name)
              (cl-return-from cacheus-put! nil))
            (let ((key-form
                   (if (memq cache-type '(memoize memoize-fn))
                       (-let-pattern*
                           ((&struct :key-fn key-fn :arglist arglist :version ver)
                            options)
                         (let ((key-gen (or key-fn `(lambda ,arglist
                                                      (list ,@arglist)))))
                           `(let ((key (funcall #',key-gen ,@args)))
                              (if ',ver (list key ',ver) key))))
                     (if (= 1 (length args))
                         (car args)
                       (error
                        "cacheus-put!: Generic cache expects one key")))))
              `(funcall #',put-fn-sym ,key-form ,value)))
          (warn "cacheus-put!: No cache registered for %S" cache-name)
        nil))))

;;;###autoload
(defmacro cacheus-clear! (cache-name-form)
  "Clear all entries from a specific Cacheus cache by its name.

Arguments:
- `CACHE-NAME-FORM` (quoted symbol): The name of the target cache
  to clear, e.g., `'my-cache`.

Returns:
  (form): A Lisp form that, when evaluated, clears the cache
          and returns the name of the cleared cache (a symbol).

Example:
  (cacheus-clear! 'my-project-cache)"
  (declare (indent 1))
  (cl-block cacheus-clear!
    (let* ((cache-name (when (eq 'quote (car-safe cache-name-form))
                         (cadr cache-name-form)))
           (registry-entry
            (when cache-name
              (gethash cache-name cacheus-global-cache-registry))))
      (unless cache-name
        (error "cacheus-clear!: Argument must be a quoted symbol, got %S"
               cache-name-form))
      (if registry-entry
          (let* ((syms (plist-get registry-entry :symbols-struct))
                 (clear-fn-sym (and syms (cacheus-symbols-clear-fn syms))))
            (unless clear-fn-sym
              (warn "cacheus-clear!: No :clear-fn-symbol for %S" cache-name)
              (cl-return-from cacheus-clear! nil))
            `(funcall #',clear-fn-sym))
        (warn "cacheus-clear!: No cache registered for %S" cache-name)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Cache Management

;;;###autoload
(defun cacheus-list-all-caches ()
  "Display all registered Cacheus caches in a new, interactive buffer.

The resulting `*Cacheus Caches*` buffer provides a tabulated
list of all caches. From this buffer, you can perform actions
on the cache at point, such as inspecting (i), clearing (c), or
saving (s).

Arguments:
  None.

Returns:
  nil. Displays a buffer as a side effect."
  (interactive)
  (let ((buffer (get-buffer-create "*Cacheus Caches*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only nil)) (erase-buffer))
      (setq-local tabulated-list-format
                  [("Cache Name" 30 t)
                   ("Type"       10 t)
                   ("Entries"    10 :right-align)
                   ("Capacity"   10 :right-align)
                   ("TTL (s)"    8 :right-align)
                   ("File"       35 t)])
      (setq-local
       tabulated-list-entries
       (--map
        (-let-pattern*
            (((name . details) it)
             ((&plist :type type :config opts :symbols syms) details)
             ((&struct :capacity cap :ttl ttl :cache-file file) opts)
             ((&struct :cache-var ht-var) syms)
             (size (if-let ((ht (and ht-var (boundp ht-var)
                                     (symbol-value ht-var))))
                       (ht-size ht) "N/A")))
          (list name
                (vector (format "%s" name)
                        (format "%s" type)
                        (format "%s" size)
                        (format "%s" (or cap "None"))
                        (format "%s" (or ttl "None"))
                        (format "%s" (or file "None")))))
        (ht->alist cacheus-global-cache-registry)))
      (tabulated-list-mode)
      (define-key tabulated-list-mode-map "i" #'cacheus--list-caches-inspect)
      (define-key tabulated-list-mode-map "c" #'cacheus--list-caches-clear)
      (define-key tabulated-list-mode-map "s" #'cacheus--list-caches-save)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)))

(defun cacheus--list-caches-operate (operation-name op-accessor-fn)
  "Internal helper to run an operation from the `tabulated-list` buffer."
  (interactive "s")
  (let* ((cache-name (tabulated-list-get-id))
         (details (when cache-name
                    (gethash cache-name cacheus-global-cache-registry)))
         (symbols (when details (plist-get details :symbols-struct)))
         (op-fn-sym (when symbols (funcall op-accessor-fn symbols))))
    (if (and op-fn-sym (fboundp op-fn-sym))
        (progn
          (funcall op-fn-sym)
          (message "Cacheus: Ran '%s' on cache '%s'."
                   operation-name cache-name)
          (revert-buffer))
      (message "Cacheus: No '%s' function available for cache '%s'."
               operation-name cache-name))))

(defun cacheus--list-caches-inspect ()
  "Interactive helper to inspect the cache on the current line."
  (interactive)
  (cacheus--list-caches-operate "inspect"
                                #'cacheus-symbols-inspect-cache-fn))

(defun cacheus--list-caches-clear ()
  "Interactive helper to clear the cache on the current line."
  (interactive)
  (cacheus--list-caches-operate "clear" #'cacheus-symbols-clear-fn))

(defun cacheus--list-caches-save ()
  "Interactive helper to save the cache on the current line."
  (interactive)
  (cacheus--list-caches-operate "save" #'cacheus-symbols-save-fn))

(defun cacheus--operate-on-all-caches (operation-name op-accessor-fn
                                       &optional name-filter-regexp)
  "Internal helper to perform an op on all or filtered registered caches."
  (let ((logger (cacheus-resolve-logger nil))
        (cnt 0)
        (matching-details
         (-filter
          (lambda (details)
            (or (not name-filter-regexp)
                (string-empty-p name-filter-regexp)
                (string-match-p
                 name-filter-regexp
                 (symbol-name (plist-get details :name)))))
          (ht-values cacheus-global-cache-registry))))
    (if (null matching-details)
        (message "No caches found%s to %s."
                 (if name-filter-regexp
                     (format " matching '%s'" name-filter-regexp) "")
                 operation-name)
      (-each matching-details
        (lambda (details)
          (-let-pattern*
              (((&plist :name cache-name :config opts :symbols syms) details))
            (when syms
              (let* ((op-fn-sym (funcall op-accessor-fn syms))
                     (cache-file (cacheus-options-cache-file opts)))
                (if (and op-fn-sym (fboundp op-fn-sym)
                         (or (not (memq operation-name '("save" "load")))
                             cache-file))
                    (progn
                      (funcall logger :info "Cacheus: %s-all: %S..."
                               operation-name cache-name)
                      (condition-case-unless-debug e (funcall op-fn-sym)
                        (error (funcall logger :error
                                        "Cacheus: Failed to %s %S: %S"
                                        operation-name cache-name e)))
                      (cl-incf cnt))
                  (funcall logger :warn
                           "Cacheus: %s-all: Skipping %S (no valid fn/file)."
                           operation-name cache-name))))))))
    (message "Attempted to %s %d cache(s)%s."
             operation-name cnt
             (if name-filter-regexp
                 (format " matching '%s'" name-filter-regexp) ""))))

;;;###autoload
(defun cacheus-clear-all-caches (&optional name-filter-regexp)
  "Clear all registered Cacheus caches.

Interactively, with a prefix argument, prompts for a regular
expression to filter caches by name.

Arguments:
- `NAME-FILTER-REGEXP` (string, optional): A regular expression.
  If provided, only caches whose names match the regexp will be
  cleared.

Returns:
  nil. Prints a message with the count of cleared caches."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter by cache name (regexp): "))))
  (cacheus--operate-on-all-caches
   "clear" #'cacheus-symbols-clear-fn name-filter-regexp))

;;;###autoload
(defun cacheus-save-all-caches (&optional name-filter-regexp)
  "Save all registered, file-backed Cacheus caches to disk.

Interactively, with a prefix argument, prompts for a regular
expression to filter caches by name.

Arguments:
- `NAME-FILTER-REGEXP` (string, optional): A regular expression.
  If provided, only file-backed caches whose names match the
  regexp will be saved.

Returns:
  nil. Prints a message with the count of saved caches."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter by cache name (regexp): "))))
  (cacheus--operate-on-all-caches
   "save" #'cacheus-symbols-save-fn name-filter-regexp))

;;;###autoload
(defun cacheus-load-all-caches (&optional name-filter-regexp)
  "Load all registered, file-backed Cacheus caches from their files.

Interactively, with a prefix argument, prompts for a regular
expression to filter caches by name.

Arguments:
- `NAME-FILTER-REGEXP` (string, optional): A regular expression.
  If provided, only file-backed caches whose names match the
  regexp will be loaded.

Returns:
  nil. Prints a message with the count of loaded caches."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter by cache name (regexp): "))))
  (cacheus--operate-on-all-caches
   "load" #'cacheus-symbols-load-fn name-filter-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry Data Accessors

(defun cacheus--get-cache-symbols (cache-name)
  "Helper to retrieve the symbols struct for a given cache-name."
  (unless (symbolp cache-name)
    (error "Cache name must be a symbol, got %S" cache-name))
  (let* ((registry-entry (gethash cache-name cacheus-global-cache-registry))
         (symbols (and registry-entry
                       (plist-get registry-entry :symbols-struct))))
    (unless symbols
      (error "No cache registered with name '%S'." cache-name))
    symbols))

;;;###autoload
(defun cacheus-entry-data (cache-name cache-entry)
  "Retrieve the main data payload from a CACHE-ENTRY.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `CACHE-ENTRY` (struct): The cache entry object itself.

Returns:
  (any): The original data that was stored in the cache entry."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (data-accessor (cacheus-symbols-data-accessor-for-entries symbols)))
    (funcall data-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-timestamp (cache-name cache-entry)
  "Retrieve the timestamp from a CACHE-ENTRY.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `CACHE-ENTRY` (struct): The cache entry object itself.

Returns:
  (time): The `current-time` timestamp when the entry was created
          or last refreshed."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (ts-accessor (cacheus-symbols-ts-accessor-for-entries symbols)))
    (funcall ts-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-version (cache-name cache-entry)
  "Retrieve the version from a CACHE-ENTRY.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `CACHE-ENTRY` (struct): The cache entry object itself.

Returns:
  (any): The cache version associated with the entry at the time
         it was created."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (ver-accessor (cacheus-symbols-entry-ver-accessor-for-entries symbols)))
    (funcall ver-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-field (cache-name cache-entry field-name)
  "Retrieve a custom FIELD-NAME's value from a CACHE-ENTRY.

The requested field must have been defined in the cache's `:fields`
option during its definition.

Arguments:
- `CACHE-NAME` (symbol): The name of the cache the entry belongs to.
- `CACHE-ENTRY` (struct): The cache entry object itself.
- `FIELD-NAME` (symbol): The name of the custom field to access.

Returns:
  (any): The value of the requested custom field."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (struct-name (cacheus-symbols-struct-name-for-entries symbols))
         (accessor-name (intern (format "%s-%s"
                                        (symbol-name struct-name)
                                        (symbol-name field-name)))))
    (unless (fboundp accessor-name)
      (error "Field '%S' not found for cache '%S'." field-name cache-name))
    (funcall accessor-name cache-entry)))

;;;###autoload
(defun cacheus-get-underlying-cache-ht (cache-name)
  "Return the underlying hash table for a given cache.

This function is for introspection or advanced manipulation. Modifying
the hash table directly may lead to an inconsistent cache state.

Arguments:
- `CACHE-NAME` (symbol): The name of the target cache.

Returns:
  (hash-table): The raw hash table used as the cache's data store."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (cache-ht-var (cacheus-symbols-cache-var symbols)))
    (unless (and cache-ht-var (boundp cache-ht-var))
      (error "Underlying cache hash table variable not found for cache '%S'."
             cache-name))
    (symbol-value cache-ht-var)))

;;;###autoload
(defun cacheus-map (cache-name fn)
  "Apply FN to each key and entry-struct in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the target cache.
- `FN` (function): A function of two arguments, `(lambda (KEY ENTRY))`,
  to apply to each element. `ENTRY` is the full cache entry struct.

Returns:
  nil."
  (ht-map (cacheus-get-underlying-cache-ht cache-name) fn))

;;;###autoload
(defun cacheus-keys (cache-name)
  "Return a list of all keys in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the target cache.

Returns:
  (list): A list of all keys currently in the cache."
  (ht-keys (cacheus-get-underlying-cache-ht cache-name)))

;;;###autoload
(defun cacheus-values (cache-name)
  "Return a list of all raw entry structs in CACHE-NAME.

Note that this returns the full `cacheus-entry` structs, not just
the data payloads. To get only the data, use `(mapcar ...)` with
`cacheus-entry-data`.

Arguments:
- `CACHE-NAME` (symbol): The name of the target cache.

Returns:
  (list): A list of all `cacheus-entry` structs in the cache."
  (ht-values (cacheus-get-underlying-cache-ht cache-name)))

;;;###autoload
(defun cacheus-size (cache-name)
  "Return the number of entries currently in CACHE-NAME.

Arguments:
- `CACHE-NAME` (symbol): The name of the target cache.

Returns:
  (integer): The total number of key-value pairs in the cache."
  (ht-size (cacheus-get-underlying-cache-ht cache-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shutdown Hook

(defun cacheus--run-save-all-caches-on-shutdown ()
  "Hook function to save all file-backed caches on Emacs shutdown."
  (when cacheus-save-all-on-shutdown
    (let ((logger (cacheus-resolve-logger nil)))
      (condition-case-unless-debug err
          (progn
            (funcall logger :info "Cacheus: Shutdown hook: Saving all caches...")
            (cacheus-save-all-caches))
        (error
         (funcall logger :error "Cacheus: Error during shutdown save: %S"
                  err :trace))))))

(add-hook 'kill-emacs-hook #'cacheus--run-save-all-caches-on-shutdown)

(provide 'cacheus-core)
;;; cacheus-core.el ends here