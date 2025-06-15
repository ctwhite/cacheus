;;; cacheus-core.el --- Core Cacheus Library and Global Management -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; `cacheus-core.el` serves as the foundational library for the entire Cacheus
;; ecosystem. It defines the global cache registry and the high-level
;; management functions that are shared by all other `cacheus-` modules.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'ht)
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

(defvar cacheus-global-cache-registry (ht-create)
  "Global registry of all Cacheus caches defined by various modules.")

(define-error 'cacheus-error
  "Generic Cacheus error. All specific Cacheus errors inherit from this." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Generation Logic

(defun cacheus-create-options (name-sym options-plist)
  "Create and validate a base `cacheus-options` struct from a plist."
  (cacheus-let* (((&plist :ttl ttl-opt :version version-opt :capacity capacity-opt
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
     :name name-sym :logger logger-opt :capacity capacity-opt
     :eviction-strategy final-eviction-strategy :cache-file cache-file-opt
     :version version-opt :periodic-cleanup periodic-cleanup-opt
     :predicate predicate-opt :async async-opt :error-handler error-handler-opt
     :ttl ttl-opt :refresh-ttl refresh-ttl-opt
     :expiration-hook expiration-hook-opt :dirty-p dirty-p-opt
     :clear-hook clear-hook-opt :prefix (or prefix-str cacheus-default-prefix)
     :fields-data fields-data :meta-fn meta-fn-opt :tags-fn tags-fn-opt)))

(defun cacheus--generate-symbol-plist (sym-prefix)
  "Generate a plist of cache-related symbols based on enabled features."
  (let* ((symbol-specs
          '(;; `get` and `put` functions are now universal and no longer generated.
            (:cache-var           "-ht" t)
            (:timestamps-var      "-timestamps-ht" t)
            (:order-ring-or-queue-var "-order-queue" t)
            (:size-var            "-capacity-var" t)
            (:frequency-var       "-frequency-ht" t)
            (:version-id-var      "-version-id" t)
            (:save-fn             "-save" t)
            (:clear-fn            "-clear" t)
            (:inspect-cache-fn    "-inspect" t)
            (:inspect-entry-fn    "-inspect-entry" t)
            (:entry-tags-var      "-entry-tags-ht" t)
            (:tags-idx-var        "-tags-idx-ht" t)
            (:invalidate-tags-fn  "-invalidate-by-tags" t)
            (:load-fn             "-load" t)
            (:inflight-var        "-inflight-ht" t)))
         (generated-syms-plist
          (cl-loop for (key suffix _condition) in symbol-specs
                   collect key
                   and collect (intern (format "%s%s" sym-prefix suffix)))))
    (append generated-syms-plist nil)))

(defun cacheus-generate-symbols (options-struct)
 "Generate and return a `cacheus-symbols` struct."
  (cacheus-let* (((&struct :name name-sym :prefix prefix-str
                         :fields-data fields-data)
                  options-struct)
                 (sym-prefix (format "%s--%s" prefix-str (symbol-name name-sym)))
                 (s-name (intern (format "%s-entry" sym-prefix)))
                 (ctor (intern (format "make-%s" s-name)))
                 (key-acc (intern (format "%s-key" s-name)))
                 (ts-acc (intern (format "%s-timestamp" s-name)))
                 (data-acc (intern (format "%s-data" s-name)))
                 (ver-acc (intern (format "%s-entry-version" s-name)))
                 (unique-fields (cl-remove-duplicates fields-data :key #'car :test #'eq))
                 (all-fields `((key nil :read-only t)
                               (data nil :read-only t)
                               (timestamp nil :type (or ts null))
                               (entry-version nil :read-only t)
                               ,@unique-fields)))
    (apply #'make-cacheus-symbols
           :sym-prefix sym-prefix :struct-name-for-entries s-name
           :make-fn-constructor-for-entries ctor
           :key-accessor-for-entries key-acc
           :ts-accessor-for-entries ts-acc :data-accessor-for-entries data-acc
           :entry-ver-accessor-for-entries ver-acc
           :all-struct-fields-for-entries all-fields
           (cacheus--generate-symbol-plist sym-prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Private Helpers

(defun cacheus--get-instance-by-name (cache-name)
  "Find, initialize, and return the live instance for CACHE-NAME."
  (let* ((registry-entry (gethash cache-name cacheus-global-cache-registry))
         (instance-var (and registry-entry (plist-get registry-entry :instance-var-sym))))
    (unless (and instance-var (boundp instance-var))
      (error "Cacheus: No instance variable found for cache '%s'" cache-name))
    (let ((instance (symbol-value instance-var)))
      (unless (cacheus-instance-runtime-data instance)
        (let* ((live-instance (copy-sequence instance))
               (opts (cacheus-instance-options live-instance))
               (capacity (cacheus-options-capacity opts))
               (eviction-strategy (cacheus-options-eviction-strategy opts))
               (rtd (make-cacheus-runtime-data
                     :cache-ht (ht-create)
                     :timestamps-ht (ht-create)
                     :entry-tags-ht (ht-create)
                     :tags-idx-ht (ht-create)
                     :inflight-ht (ht-create)
                     :order-data (when (and capacity (memq eviction-strategy '(:lru :fifo)))
                                   (ring-init capacity))
                     :frequency-ht (when (and capacity (eq eviction-strategy :lfu))
                                     (ht-create)))))
          (setf (cacheus-instance-runtime-data live-instance) rtd)
          (setf (symbol-value instance-var) live-instance)
          (setq instance live-instance)))
      instance)))

(defun cacheus--handle-hit (entry key instance async)
  "Handle a cache hit for ENTRY. Returns value or promise."
  (cacheus-let* (((&struct :options opts :symbols syms) instance)
                 ((&struct :name name :logger logger-opt) opts)
                 ((&struct :data-accessor-for-entries data-accessor) syms)
                 (logger (cacheus-resolve-logger logger-opt))
                 (cached-value (funcall data-accessor entry)))
    (funcall logger :debug "[C:%S] CACHE HIT" name)
    (cacheus-update-instance-on-hit key instance)
    (if async
        (progn (require 'concur)
               (if (concur-promise-p cached-value)
                   cached-value
                 (concur:resolved! cached-value)))
      cached-value)))

(defun cacheus--handle-miss-sync (instance key compute-thunk user-key)
  "Handle a synchronous cache miss by computing and storing the value."
  (cacheus-let* (((&struct :options opts) instance)
                 ((&struct :name name :logger logger-opt :predicate pred
                           :error-handler err-handler :tags-fn tags-fn) opts)
                 (logger (cacheus-resolve-logger logger-opt))
                 (val nil))
    (funcall logger :debug "[C:%S] SYNC MISS for key: %S. Computing." name user-key)
    (condition-case-unless-debug err (setq val (funcall compute-thunk))
      (error (funcall logger :error "[C:%S] Sync compute error for %S: %S" name user-key err :trace)
             (when err-handler (funcall err-handler err))
             (cl-return-from cacheus--handle-miss-sync nil)))
    (if (and val (or (null pred) (funcall pred val)))
        (let* ((tags (if tags-fn (funcall tags-fn user-key val)))
               (new-entry (cacheus--create-entry instance key val)))
          (cacheus-store-result new-entry key tags instance logger))
      (funcall logger :debug "[C:%S] Sync predicate rejected: %S" name user-key))
    val))

(defun cacheus--handle-miss-async (instance key compute-thunk user-key)
  "Handle an asynchronous cache miss, returning a promise."
  (require 'concur)
  (cacheus-let* (((&struct :options opts :symbols syms) instance)
                 ((&struct :name name :logger logger-opt :error-handler err-handler
                           :predicate pred :tags-fn tags-fn) opts)
                 ((&struct :inflight-var inflight-var) syms)
                 (logger (cacheus-resolve-logger logger-opt))
                 (inflight-ht (and inflight-var (boundp inflight-var) (symbol-value inflight-var))))
    (when-let ((promise (and inflight-ht (ht-get inflight-ht key))))
      (funcall logger :debug "[C:%S] IN-FLIGHT HIT for key: %S" name user-key)
      (cl-return-from cacheus--handle-miss-async promise))
    (funcall logger :debug "[C:%S] ASYNC MISS for key: %S. Computing." name user-key)
    (let ((computation-promise
           (cacheus-async-result key (lambda () (concur:make-future compute-thunk))
                                 instance logger)))
      (concur:chain computation-promise
        (:then (lambda (val)
                 (if (and pred (not (funcall pred val)))
                     (progn
                       (funcall logger :debug "[C:%S] Async predicate rejected for %S" name user-key)
                       (concur:rejected! (list :predicate-rejected val)))
                   val)))
        (:then (lambda (val)
                 (let* ((tags (if tags-fn (funcall tags-fn user-key val)))
                        (final-entry (cacheus--create-entry instance key val)))
                   (cacheus-store-result final-entry key tags instance logger)
                   val)))
        (:catch (lambda (reason)
                  (funcall logger :error "[C:%S] Async compute error for %S: %S"
                           name user-key reason :trace)
                  (when err-handler (funcall err-handler reason))
                  (cacheus-evict-one-entry key instance logger)
                  (concur:rejected! reason)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Functions

;;;###autoload
(cl-defun cacheus-get-or-compute (instance key compute-thunk &key user-key async)
  "Universal entry point to get a value from cache or compute it."
  (let* ((live-instance (cacheus--get-instance-by-name
                         (cacheus-options-name (cacheus-instance-options instance))))
         (opts (cacheus-instance-options live-instance))
         (rtd (cacheus-instance-runtime-data live-instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (cache-ht (cacheus-runtime-data-cache-ht rtd))
         (entry (ht-get cache-ht key))
         (final-user-key (or user-key key)))
    (if (and entry (not (cacheus-is-instance-entry-stale key entry live-instance logger)))
        (cacheus--handle-hit entry key live-instance async)
      (progn
        (when entry
          (funcall logger :debug "[C:%S] STALE HIT for key: %S. Evicting."
                   (cacheus-options-name opts) final-user-key)
          (cacheus-evict-one-entry key live-instance logger))
        (if async
            (cacheus--handle-miss-async live-instance key compute-thunk final-user-key)
          (cacheus--handle-miss-sync live-instance key compute-thunk final-user-key))))))

;;;###autoload
(defun cacheus-put (cache-name key value &key tags)
  "Manually insert or update a VALUE for a given KEY in a cache."
  (let ((instance (cacheus--get-instance-by-name cache-name)))
    (cacheus-let* (((&struct :options options) instance)
                   (predicate (cacheus-options-predicate options))
                   (logger (cacheus-resolve-logger (cacheus-options-logger options)))
                   (final-key key))
      (when (eq (type-of instance) 'cacheus-memoize-instance)
        (let* ((key-fn (or (cacheus-memoize-options-key-fn options) #'list))
               (version (cacheus-options-version options))
               (computed-key (apply key-fn key)))
          (setq final-key (if version (list computed-key version) computed-key))))
      (if (and predicate (not (funcall predicate value)))
          (progn
            (funcall logger :info "[C:%S] Put: Skipped key %S due to predicate."
                     cache-name final-key)
            nil)
        (let ((new-entry (cacheus--create-entry instance final-key value)))
          (funcall logger :info "[C:%S] Put: Storing new value for key %S."
                   cache-name final-key)
          (cacheus-store-result new-entry final-key tags instance logger)
          value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Cache API - Macros

;;;###autoload
(defmacro cacheus-get! (cache-name-form &rest args)
  "Retrieve a value from any Cacheus cache."
  (declare (indent 2))
  (cl-block cacheus-get!
    (let* ((cache-name (cadr cache-name-form))
           (registry-entry (gethash cache-name cacheus-global-cache-registry)))
      (unless registry-entry
        (warn "cacheus-get!: No cache registered for %S at compile time" cache-name)
        (cl-return-from cacheus-get! nil))
      (cacheus-let* (((&plist :type cache-type) registry-entry))
        (pcase cache-type
          ('memoize
           ;; For a memoized function, expand to a direct call to the function itself.
           `(,cache-name ,@args))
          ('cache
           ;; For a generic cache, expand to a call to the universal helper,
           ;; ensuring the instance is initialized first.
           (let ((key (car args))
                 (compute-thunk (cadr args)))
             `(cacheus-get-or-compute
               (cacheus--get-instance-by-name ',cache-name)
               ,key
               ,(or compute-thunk `(lambda () nil))
               :user-key ,key)))
          (_
           (warn "cacheus-get!: Unknown cache type '%s' for %S" cache-type cache-name)))))))

;;;###autoload
(defmacro cacheus-put! (cache-name-form key value &rest kwargs)
  "Manually insert or update a VALUE for a given KEY in a cache."
  (declare (indent 2))
  `(cacheus-put ',(cadr cache-name-form) ,key ,value ,@kwargs))

;;;###autoload
(defmacro cacheus-clear! (cache-name-form)
  "Clear all entries from a specific Cacheus cache by its name."
  (declare (indent 1))
  (cl-block cacheus-clear!
    (let* ((cache-name (when (eq 'quote (car-safe cache-name-form))
                         (cadr cache-name-form)))
           (registry-entry
            (when cache-name
              (gethash cache-name cacheus-global-cache-registry))))
      (if registry-entry
          (let* ((syms (plist-get registry-entry :symbols-struct))
                 (clear-fn-sym (and syms (cacheus-symbols-clear-fn syms))))
            (if clear-fn-sym
                `(funcall #',clear-fn-sym)
              (warn "cacheus-clear!: No :clear-fn-symbol for %S" cache-name)))
        (warn "cacheus-clear!: No cache registered for %S" cache-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Cache Management

;;;###autoload
(defun cacheus-list-all-caches ()
  "Display all registered Cacheus caches in a new, interactive buffer."
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
        (cacheus-let* (((name . details) it)
                       ((&plist :type type :config opts :instance-var-sym ivar) details)
                       ((&struct :capacity cap :ttl ttl :cache-file file) opts)
                       (size (if-let ((ht (and (boundp ivar)
                                               (cacheus-runtime-data-cache-ht
                                                (cacheus-instance-runtime-data (symbol-value ivar))))))
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
          (cacheus-let* (((&plist :name cache-name :config opts :symbols syms) details))
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
  "Clear all registered Cacheus caches."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter by cache name (regexp): "))))
  (cacheus--operate-on-all-caches
   "clear" #'cacheus-symbols-clear-fn name-filter-regexp))

;;;###autoload
(defun cacheus-save-all-caches (&optional name-filter-regexp)
  "Save all registered, file-backed Cacheus caches to disk."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter by cache name (regexp): "))))
  (cacheus--operate-on-all-caches
   "save" #'cacheus-symbols-save-fn name-filter-regexp))

;;;###autoload
(defun cacheus-load-all-caches (&optional name-filter-regexp)
  "Load all registered, file-backed Cacheus caches from their files."
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
  "Retrieve the main data payload from a CACHE-ENTRY."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (data-accessor (cacheus-symbols-data-accessor-for-entries symbols)))
    (funcall data-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-timestamp (cache-name cache-entry)
  "Retrieve the timestamp from a CACHE-ENTRY."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (ts-accessor (cacheus-symbols-ts-accessor-for-entries symbols)))
    (funcall ts-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-version (cache-name cache-entry)
  "Retrieve the version from a CACHE-ENTRY."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (ver-accessor (cacheus-symbols-entry-ver-accessor-for-entries symbols)))
    (funcall ver-accessor cache-entry)))

;;;###autoload
(defun cacheus-entry-field (cache-name cache-entry field-name)
  "Retrieve a custom FIELD-NAME's value from a CACHE-ENTRY."
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
  "Return the underlying hash table for a given cache."
  (let* ((symbols (cacheus--get-cache-symbols cache-name))
         (cache-ht-var (cacheus-symbols-cache-var symbols)))
    (unless (and cache-ht-var (boundp cache-ht-var))
      (error "Underlying cache hash table variable not found for cache '%S'."
             cache-name))
    (symbol-value cache-ht-var)))

;;;###autoload
(defun cacheus-map (cache-name fn)
  "Apply FN to each key and entry-struct in CACHE-NAME."
  (ht-map (cacheus-get-underlying-cache-ht cache-name) fn))

;;;###autoload
(defun cacheus-keys (cache-name)
  "Return a list of all keys in CACHE-NAME."
  (ht-keys (cacheus-get-underlying-cache-ht cache-name)))

;;;###autoload
(defun cacheus-values (cache-name)
  "Return a list of all raw entry structs in CACHE-NAME."
  (ht-values (cacheus-get-underlying-cache-ht cache-name)))

;;;###autoload
(defun cacheus-size (cache-name)
  "Return the number of entries currently in CACHE-NAME."
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