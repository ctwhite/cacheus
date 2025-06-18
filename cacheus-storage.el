;;; cacheus-storage.el --- Core cache entry storage and async operations. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the fundamental functions for managing the storage of
;; individual cache entries. This includes the logic for adding new entries,
;; which integrates with the eviction and tagging systems, and handling
;; asynchronous computations to prevent redundant work (the thundering herd
;; effect).

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)
(require 'concur nil t)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-tags)

;; Ensure `concur:make-future` is available at compile-time for the macro.
(eval-when-compile (require 'concur-future))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry Storage and Asynchronous Operations (Package-Private)

(defun cacheus-store-result (entry ekey tags instance logger)
  "Store ENTRY in INSTANCE under effective key EKEY, with TAGS.

This function is the primary entry point for adding a computed
result to the cache. It orchestrates the process of making space
for the new entry via eviction, storing the entry itself, and
updating all associated metadata like timestamps and tags.

Arguments:
  ENTRY (struct): The fully-formed cache entry struct to store.
  EKEY (any): The effective cache key (which may include a version).
  TAGS (list): A list of symbols to associate with this entry.
  INSTANCE (cacheus-instance): The live cache instance.
  LOGGER (function): The resolved logger function for this cache.

Returns:
  The stored ENTRY struct."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (hook (cacheus-options-expiration-hook opts))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (funcall logger :debug "[C:%s] store: Key:%S (Tags:%S)" name ekey tags)

    ;; 1. Prepare for the new entry by running eviction logic if needed.
    (when-let ((victim-key (cacheus-eviction-prepare-for-put ekey instance)))
      (funcall logger :debug "[C:%s] store: Victim %S chosen for eviction."
               name victim-key)
      (let ((evicted-struct (ht-get cache-ht victim-key)))
        (cacheus-evict-one-entry victim-key instance logger)
        ;; Run user-defined hook for the expired/evicted entry.
        (when (and hook evicted-struct)
          (condition-case-unless-debug e
              (funcall hook victim-key evicted-struct)
            (error
             (funcall logger :error "[C:%s] Expired hook error for %S: %S"
                      name victim-key e :trace))))))

    ;; 2. Store the new entry and its metadata.
    (ht-set! cache-ht ekey entry)
    (when-let ((ts-ht (cacheus-runtime-data-timestamps-ht rtd)))
      (ht-set! ts-ht ekey (ts-now)))
    (cacheus-update-entry-tag-info ekey tags instance logger)
    entry))

(defmacro cacheus-async-result (ekey producer-fn-form instance logger)
  "Manage an asynchronous result production for EKEY.

This macro prevents the 'thundering herd' problem for asynchronous
caches. It uses a lazy `concur:future` to ensure that for a
given key, the expensive `PRODUCER-FN-FORM` is only ever
executed once, even if multiple callers request the key
simultaneously while it is being computed. Subsequent callers
receive the same promise for the in-flight computation.

Arguments:
  EKEY (any): The effective cache key for the operation.
  PRODUCER-FN-FORM (lambda-form): A 0-arity lambda `(lambda () ...)`
    that computes the value and returns a `concur` promise.
  INSTANCE (cacheus-instance): The live instance to operate on.
  LOGGER (function): A resolved logger function.

Returns:
  (concur-promise) A promise that will resolve to the computed value."
  (declare (indent 1))
  ;; Use `gensym` to avoid variable capture conflicts inside the macro.
  (let ((name (gensym "name-"))
        (inflight-ht (gensym "inflight-ht-")))
    `(let* ((,name (cacheus-options-name (cacheus-instance-options ,instance)))
            (,inflight-ht (cacheus-runtime-data-inflight-ht
                           (cacheus-instance-runtime-data ,instance))))
       ;; 1. Check if a computation for this key is already in-flight.
       (or (ht-get ,inflight-ht ,ekey)
           ;; 2. If not, create a new computation promise.
           (let* ((future (concur:make-future ,producer-fn-form))
                  (promise (concur:force future)))
             (unless (concur-promise-p promise)
               (error "[C:%s] Producer for %S did not return a promise."
                      ,name ,ekey))
             (funcall ,logger :debug "[C:%s] async: Tracking new promise for %S."
                      ,name ,ekey)
             ;; 3. Store the promise so other callers can find it.
             (ht-set! ,inflight-ht ,ekey promise)
             ;; 4. Ensure the promise is removed from the in-flight tracker
             ;;    once it resolves or fails.
             (concur:finally
              promise
              (lambda ()
                (funcall ,logger :debug "[C:%s] async: Cleaning up promise for %S"
                         ,name ,ekey)
                (ht-remove! ,inflight-ht ,ekey)))
             promise)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Functions (Package-Private)

(defun cacheus-create-entry (instance key value)
  "Create a cache entry struct for INSTANCE with KEY and VALUE at runtime.

This function dynamically constructs the appropriate entry struct
for a given cache. It handles standard fields (key, data,
timestamp) as well as any custom fields defined by the user via
the `:fields` and `:meta-fn` options.

Arguments:
  INSTANCE (cacheus-instance): The live cache instance.
  KEY (any): The user-provided key for this entry.
  VALUE (any): The computed value to be stored.

Returns:
  (struct) A newly created cache entry struct."
  (cacheus-let*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :name name :logger lopt :meta-fn meta-fn) opts)
       ((&struct :make-fn-constructor-for-entries ctor-fn
                 :version-id-var ver-var
                 :all-struct-fields-for-entries all-fields)
        syms)
       (logger (cacheus-resolve-logger lopt))
       ;; Get field definitions, excluding the standard ones.
       (custom-fields (-remove (lambda (f) (memq (car f)
                                                 '(key data timestamp entry-version)))
                               all-fields))
       ;; Get user-provided metadata for this specific key-value pair.
       (meta (if meta-fn (funcall meta-fn key value)))
       (args nil))
    ;; Validate that the metadata function returned a plist.
    (when (and meta (not (listp meta)))
      (funcall logger :warn "[C:%s] :meta-fn returned non-plist: %S. Ignoring."
               name meta)
      (setq meta nil))
    ;; Construct the argument list for the entry struct's constructor.
    (setq args
          (list :key key
                :data value
                :timestamp (ts-now)
                :entry-version (if (boundp ver-var) (symbol-value ver-var) nil)))
    (dolist (field-def custom-fields)
      (let* ((field-name (car field-def))
             (field-kw (intern (format ":%s" field-name))))
        (setq args (append args (list field-kw (plist-get meta field-name))))))
    ;; Call the constructor with the dynamically built arguments.
    (apply ctor-fn args)))

(provide 'cacheus-storage)
;;; cacheus-storage.el ends here