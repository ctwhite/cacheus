;;; cacheus-storage.el --- Core cache entry storage and async operations
;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file provides the fundamental functions for managing the storage of
;; individual cache entries. This includes the logic for adding new entries,
;; which integrates with the eviction and tagging systems, and handling
;; asynchronous computations to prevent redundant work (the thundering herd
;; effect).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun cacheus-store-result (entry-struct ekey tags instance logger)
  "Store `entry-struct` in `instance` under effective key `ekey`.
This is the primary write-path function. It orchestrates eviction
logic before storing the new entry and updates tagging indexes if
`tags` are provided.

Arguments:
- `ENTRY-STRUCT` (struct): The fully formed entry struct to be stored.
- `EKEY` (any): The effective cache key for this entry.
- `TAGS` (list): A list of symbols to tag this entry with, or `nil`.
- `INSTANCE` (cacheus-instance): The live instance to operate on.
- `LOGGER` (function): A resolved logger function.

Returns:
The stored `ENTRY-STRUCT`."
  (let* ((opts (cacheus-instance-options instance))
         (data (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (strategy (cacheus-options-eviction-strategy opts))
         (hook (cacheus-options-expiration-hook opts))
         (cache-ht (cacheus-runtime-data-cache-ht data))
         (ts-ht (cacheus-runtime-data-timestamps-ht data))
         (et-ht (cacheus-runtime-data-entry-tags-ht data))
         (tags-idx-ht (cacheus-runtime-data-tags-idx-ht data)))
    (funcall logger :debug "[C:%S] store: Key:%S (Strategy:%S, Tags:%S)"
             name ekey strategy tags)

    ;; Step 1: Evict an old entry if cache is at capacity.
    (when-let ((victim-key (cacheus-eviction-prepare-for-put ekey instance)))
      (funcall logger :debug "[C:%S] store: Victim %S chosen for eviction."
               name victim-key)
      (let ((evicted-struct (ht-get cache-ht victim-key)))
        (cacheus-evict-one-entry victim-key instance logger)
        (when (and hook evicted-struct)
          (condition-case-unless-debug e (funcall hook victim-key evicted-struct)
            (error (funcall logger :error
                            "[C:%S] Expired hook error for victim %S: %S"
                            name victim-key e :trace))))))

    ;; Step 2: Store the new entry struct in the main cache hash table.
    (ht-set! cache-ht ekey entry-struct)

    ;; Step 3: Update external timestamp if using refresh-on-access TTL.
    (when ts-ht (ht-set! ts-ht ekey (ts-now)))

    ;; Step 4: Update tag indexes if tags are provided.
    (cacheus-update-entry-tag-info ekey tags instance logger)

    entry-struct))

(defun cacheus-async-result (ekey producer-fn instance logger)
  "Manage an asynchronous result production for `EKEY`.
This prevents the thundering herd effect by tracking pending computations in
the instance's `inflight-ht`. If a computation for `EKEY` is already
running, its promise is returned. Otherwise, `PRODUCER-FN` is called to
start a new computation, and its promise is tracked and returned.

Arguments:
- `EKEY` (any): The effective cache key for the operation.
- `PRODUCER-FN` (function): A 0-arity function that must return a `concur` future.
- `INSTANCE` (cacheus-instance): The live instance to operate on.
- `LOGGER` (function): A resolved logger function.

Returns:
A `concur` promise that will resolve to the computed value."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (name (cacheus-options-name opts))
         (inflight-var (cacheus-symbols-inflight-var syms))
         (inflight-ht (and inflight-var (boundp inflight-var)
                           (symbol-value inflight-var))))
    ;; This `or` form is a "get or create" pattern for the promise.
    (or (ht-get inflight-ht ekey)
        ;; If not found, create and store a new promise.
        (let ((promise (concur:force (funcall producer-fn))))
          (unless (concur-promise-p promise)
            (error "[C:%S] Producer for %S did not return a promise." name ekey))
          (funcall logger :debug "[C:%S] async: Tracking new promise for %S."
                   name ekey)
          (ht-set! inflight-ht ekey promise)
          ;; Attach handlers to clean up the `inflight-ht` automatically
          ;; once the promise is settled (either resolved or rejected).
          (concur:chain promise
            (:finally (lambda ()
                        (funcall logger :debug "[C:%S] async: Cleaning up promise for %S"
                                 name ekey)
                        (ht-remove! inflight-ht ekey))))
          promise))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Functions

(defun cacheus-create-entry (instance key value)
  "Create a cache entry struct for INSTANCE with KEY and VALUE at runtime.

Arguments:
- `INSTANCE` (cacheus-instance): The live instance to operate on.
- `KEY` (any): The cache key for the entry.
- `VALUE` (any): The data value to be stored in the entry.

Returns:
A new cache entry struct instance."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (name (cacheus-options-name opts))
         (meta-fn (cacheus-options-meta-fn opts))
         (ctor-fn (symbol-function (cacheus-symbols-make-fn-constructor-for-entries syms)))
         (ver-var (cacheus-symbols-version-id-var syms))
         (custom-fields (-remove (lambda (f) (memq (car f) '(key data timestamp entry-version)))
                                 (cacheus-symbols-all-struct-fields-for-entries syms)))
         (meta-result (if meta-fn (funcall meta-fn key value)))
         constructor-args)
    (when (and meta-result (not (listp meta-result)))
      (funcall logger :warn "[C:%S] :meta-fn returned non-plist: %S. Ignoring." name meta-result)
      (setq meta-result nil))
    (setq constructor-args
          (list :key key ;; FIX: Ensure the key is stored in the entry.
                :data value
                :timestamp (ts-now)
                :entry-version (if (boundp ver-var) (symbol-value ver-var) nil)))
    (dolist (field custom-fields)
      (let* ((field-name (car field))
             (field-kw (intern (format ":%s" field-name))))
        (setq constructor-args
              (append constructor-args (list field-kw (plist-get meta-result field-name))))))
    (apply ctor-fn constructor-args)))

(provide 'cacheus-storage)
;;; cacheus-storage.el ends here