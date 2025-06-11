;;; cacheus-storage.el --- Core cache entry storage and async operations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the fundamental functions for managing the storage of
;; individual cache entries. This includes the logic for adding new entries,
;; which integrates with the eviction and tagging systems, and handling
;; asynchronous computations to prevent redundant work (dog-pile effect).

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)
(require 'concur nil t)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry Storage and Asynchronous Operations

(defun cacheus-store-result (res-entry-struct ekey tags instance logger)
  "Store `RES-ENTRY-STRUCT` in `INSTANCE` under effective key `EKEY`.
This is the primary write-path function. It orchestrates eviction
logic before storing the new entry and updates tagging indexes if
`TAGS` are provided.

Arguments:
- `RES-ENTRY-STRUCT`: The fully formed entry struct to be stored.
- `EKEY`: The effective cache key for this entry.
- `TAGS`: A list of symbols to tag this entry with, or `nil`.
- `INSTANCE`: The live `cacheus-instance` to operate on.
- `LOGGER`: A resolved logger function.

Returns:
The stored `RES-ENTRY-STRUCT`."
  (-let-pattern*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name :eviction-strategy strategy :expiration-hook hook)
        opts)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :entry-tags-ht et-ht
                 :tags-idx-ht tags-idx-ht :inflight-ht inflight-ht)
        data))
    (funcall logger :debug "[C:%S] store: Key:%S (Strategy:%S, Tags:%S)"
             name ekey strategy tags)

    ;; Step 1: Evict an old entry if cache is at capacity.
    ;; `cacheus-eviction-prepare-for-put` updates eviction data structures
    ;; and returns the key of a victim to be evicted, if any.
    (when-let ((victim-key (cacheus-eviction-prepare-for-put ekey instance)))
      (funcall logger :debug "[C:%S] store: Victim %S chosen for eviction."
               name victim-key)
      (let ((evicted-struct (ht-get cache-ht victim-key)))
        (cacheus-evict-one-entry victim-key instance logger)
        ;; Call the user-provided expiration hook for the evicted entry.
        (when (and hook evicted-struct)
          (condition-case-unless-debug e (funcall hook victim-key evicted-struct)
            (error (funcall logger :error
                            "[C:%S] Expired hook error for victim %S: %S"
                            name victim-key e :trace))))))

    ;; Step 2: Store the new entry struct in the main cache hash table.
    (ht-set! cache-ht ekey res-entry-struct)

    ;; Step 3: Update external timestamp if using refresh-on-access TTL.
    ;; This timestamp tracks the last access time, distinct from the
    ;; creation timestamp stored inside the entry struct itself.
    (when ts-ht (ht-set! ts-ht ekey (ts-now)))

    ;; Step 4: Update tag indexes if tags are provided.
    (when (and tags et-ht tags-idx-ht)
      (let ((unique-tags (-distinct tags :test #'equal)))
        (when unique-tags
          ;; Associate the key with its tags.
          (ht-set! et-ht ekey unique-tags)
          ;; Update the reverse index (tag -> list of keys).
          (-each unique-tags
                 (lambda (tag)
                   (cl-pushnew ekey (ht-get tags-idx-ht tag) :test #'equal))))))

    ;; Step 5: Remove from in-flight requests table upon completion.
    (when inflight-ht (ht-remove! inflight-ht ekey))

    res-entry-struct))

(defun cacheus-async-result (ekey producer-fn instance logger)
  "Manage an asynchronous result production for `EKEY`.
This prevents the dog-pile effect by tracking pending computations in the
instance's `inflight-ht`. If a computation for `EKEY` is already
running, its promise is returned. Otherwise, `PRODUCER-FN` is called to
start a new computation, and its promise is tracked and returned.

Arguments:
- `EKEY`: The effective cache key for the operation.
- `PRODUCER-FN`: A no-argument function that must return a `concur` promise.
- `INSTANCE`: The live `cacheus-instance` to operate on.
- `LOGGER`: A resolved logger function.

Returns:
A `concur` promise that will resolve to the computed value."
  (-let-pattern*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name) opts)
       ((&struct :inflight-ht inflight-ht) data))
    ;; This `or` form is a "get or create" pattern for the promise.
    (or (ht-get inflight-ht ekey)
        ;; If not found, create and store a new promise.
        (let ((promise (funcall producer-fn)))
          (unless (concur-promise-p promise)
            (error "[C:%S] Producer for %S did not return a promise."
                   name ekey))
          (funcall logger :debug "[C:%S] async: Tracking new promise for %S."
                   name ekey)
          (ht-set! inflight-ht ekey promise)
          ;; Attach handlers to clean up the `inflight-ht` automatically
          ;; once the promise is settled (either resolved or rejected). This
          ;; prevents stale entries from blocking future computations.
          (concur:then promise
                       (lambda (_r)
                            (funcall logger :debug
                                      "[C:%S] async: %S resolved."
                                      name ekey)
                            (ht-remove! inflight-ht ekey))
                       (lambda (_e)
                            (funcall logger :error
                                      "[C:%S] async: %S rejected."
                                      name ekey)
                            (ht-remove! inflight-ht ekey)))
          promise))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Functions

(defun cacheus--create-entry (instance key value)
  "Create a cache entry struct for INSTANCE with KEY and VALUE at runtime.
This is an internal framework function.

Arguments:
- `INSTANCE`: The live `cacheus-instance` to operate on.
- `KEY`: The cache key for the entry (user-key or effective key).
- `VALUE`: The data value to be stored in the entry.

Returns:
A new cache entry struct instance."
  (-let-pattern* (((&struct :options opts :symbols syms) instance)
                   (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
                   (name (cacheus-options-name opts))
                   (meta-fn (cacheus-options-meta-fn opts))
                   (ctor-fn (symbol-function (cacheus-symbols-make-fn-constructor-for-entries syms)))
                   (ver-var (cacheus-symbols-version-id-var syms))
                   (custom-fields (-remove (lambda (f) (memq (car f) '(data timestamp entry-version)))
                                           (cacheus-symbols-all-struct-fields-for-entries syms)))
                   (meta-result (if meta-fn (funcall meta-fn key value)))
                   (constructor-args nil))
    ;; The :meta-fn can return a plist of values for custom fields.
    (when (and meta-result (not (listp meta-result)))
      (funcall logger :warn "[C:%S] :meta-fn returned non-plist: %S. Ignoring." name meta-result)
      (setq meta-result nil))
    ;; Build the argument list for the entry struct's constructor function.
    (setq constructor-args
          (list :data value
                :timestamp (ts-now)
                :entry-version (if (boundp ver-var) (symbol-value ver-var) nil)))
    ;; Dynamically add any custom fields from the :meta-fn result.
    (dolist (field custom-fields)
      (let* ((field-name (car field))
             (field-kw (intern (format ":%s" field-name))))
        (setq constructor-args
              (append constructor-args (list field-kw (plist-get meta-result field-name))))))
    (apply ctor-fn constructor-args)))

(cl-defun cacheus-get-or-compute (instance key compute-thunk &key user-key async)
  "Get value from cache or compute it. This is the central runtime cache function.
For INSTANCE with KEY, get value. If miss/stale, exec COMPUTE-THUNK.

Arguments:
- `INSTANCE`: The live `cacheus-instance` to operate on.
- `KEY`: The effective cache key (with versioning).
- `COMPUTE-THUNK`: A no-argument function that returns the value on a miss.
- `:USER-KEY`: The original key, before versioning. Used for callbacks.
- `:ASYNC`: If non-nil, `COMPUTE-THUNK` is handled in a future.

Returns:
The cached or computed value, or a `concur` promise if `:ASYNC`."
  (cl-block cacheus-get-or-compute
    (-let-pattern* (((&struct :options opts :symbols syms) instance)
                     (name (cacheus-options-name opts))
                     (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
                     (cache-ht (cacheus-runtime-data-cache-ht (cacheus-instance-runtime-data instance)))
                     (inflight-ht (when-let ((var (cacheus-symbols-inflight-var syms)))
                                   (and (boundp var) (symbol-value var))))
                     (entry (ht-get cache-ht key)))
      ;; --- HIT PATH ---
      (when (and entry (not (cacheus-is-instance-entry-stale key entry instance logger)))
        (funcall logger :debug "[C:%S] Hit key: %S" name key)
        (cacheus-update-instance-on-hit key instance)
        (cl-return-from cacheus-get-or-compute
          (funcall (cacheus-symbols-data-accessor-for-entries syms) entry)))

      ;; If an entry was found but was stale, evict it before proceeding.
      (when entry (cacheus-evict-one-entry key instance logger))

      ;; --- IN-FLIGHT / MISS PATH ---
      ;; If another thread is already computing this key, return its promise.
      (when-let ((promise (and inflight-ht (ht-get inflight-ht key))))
        (cl-return-from cacheus-get-or-compute promise))

      (let ((pred (cacheus-options-predicate opts))
            (err-handler (cacheus-options-error-handler opts))
            (tags-fn (cacheus-options-tags-fn opts)))
        (if async
            ;; --- ASYNC COMPUTE ---
            (let ((promise (cacheus-async-result key (lambda () (concur:make-future compute-thunk)) instance logger)))
              (concur:chain (concur:async! (lambda () (concur:force (concur-promise-future promise))))
                ;; Check the predicate before storing.
                :then (if (and pred (not (funcall pred <>)))
                          (progn (funcall logger :debug "[C:%S] Async predicate rejected: %S" name key)
                                 (concur:rejected! (list :predicate-rejected <>))) <>)
                ;; If valid, create entry and store it.
                :then (let* ((tags (if tags-fn (funcall tags-fn (or user-key key) <>)))
                             (final-entry (cacheus--create-entry instance key <>)))
                        (cacheus-store-result final-entry key tags instance logger) <>)
                ;; Handle any errors during computation.
                :catch (progn (funcall logger :error "[C:%S] Async compute error for %S: %S" name key <!> :trace)
                              (when err-handler (funcall err-handler <!>))
                              (cacheus-evict-one-entry key instance logger)
                              (concur:rejected! <!>))) promise)
          ;; --- SYNC COMPUTE ---
          (let (computed-val)
            (condition-case-unless-debug err (setq computed-val (funcall compute-thunk))
              (error (funcall logger :error "[C:%S] Sync compute error for %S: %S" name key err :trace)
                     (when err-handler (funcall err-handler err))
                     (cl-return-from cacheus-get-or-compute nil)))
            ;; Check the predicate before storing.
            (if (and computed-val (or (null pred) (funcall pred computed-val)))
                (let* ((tags (if tags-fn (funcall tags-fn (or user-key key) computed-val)))
                       (new-entry (cacheus--create-entry instance key computed-val)))
                  (cacheus-store-result new-entry key tags instance logger))
              (funcall logger :debug "[C:%S] Sync predicate rejected: %S" name key))
            computed-val))))))

(provide 'cacheus-storage)
;;; cacheus-storage.el ends here