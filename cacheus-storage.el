;;; cacheus-storage.el --- Core cache entry storage and async operations. -*-
;;; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the fundamental functions for managing the storage of
;; individual cache entries. This includes the logic for adding new entries,
;; which integrates with the eviction and tagging systems, and handling
;; asynchronous computations to prevent redundant work (the "thundering herd"
;; problem).

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)
(require 'concur-core nil t)
(require 'concur-chain nil t)
(require 'concur-future nil t)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-eviction)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry Storage and Asynchronous Operations (Package-Private)

(defun cacheus-store-result (entry ekey tags instance logger)
  "Store ENTRY in INSTANCE under effective key EKEY, with TAGS.
This function orchestrates the process of making space for the new
entry via eviction, storing the entry itself, and updating all
associated metadata like timestamps and tags.

Arguments:
- `ENTRY` (struct): The fully-formed cache entry struct to store.
- `EKEY` (any): The effective cache key (which may include a version).
- `TAGS` (list): A list of symbols to associate with this entry.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): The resolved logger function for this cache.

Returns:
  The stored ENTRY struct."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (hook (cacheus-options-expiration-hook opts))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (funcall logger :debug "[C:%s] store: Key:%S (Tags:%S)" name ekey tags)

    ;; 1. Prepare for new entry by running eviction logic if needed.
    (when-let ((victim-key (cacheus-eviction-prepare-for-put ekey instance)))
      (let ((evicted-struct (gethash victim-key cache-ht)))
        (cacheus-evict-one-entry victim-key instance logger)
        (when (and hook evicted-struct)
          (condition-case e (funcall hook victim-key evicted-struct)
            (error (funcall logger :error "[C:%s] Expired hook error for %S: %S"
                              name victim-key e))))))

    ;; 2. Store the new entry and its metadata.
    (puthash ekey entry cache-ht)
    (when-let ((ts-ht (cacheus-runtime-data-timestamps-ht rtd)))
      (puthash ekey (ts-now) ts-ht))
    (cacheus-update-entry-tag-info ekey tags instance logger)
    entry))

(defun cacheus-async-result (ekey producer-fn instance logger)
  "Manage an asynchronous result production for EKEY.
This function prevents the 'thundering herd' problem where multiple
callers requesting the same missing key would trigger redundant expensive
computations. It uses a lazy `concur:future` and an in-flight promise
cache (`inflight-ht`) to ensure that for any given key, the `PRODUCER-FN`
is only ever executed once.

How it works:
1. The first caller for a key finds no promise in `inflight-ht`.
2. It creates a lazy `concur:future` for the computation.
3. It `concur:force`s the future, which returns a promise for the result,
   and stores this promise in `inflight-ht`.
4. It attaches a cleanup handler to this promise and returns it.
5. Subsequent callers for the same key will find the promise in `inflight-ht`
   and receive it directly, without re-running the computation.

Arguments:
- `EKEY` (any): The effective cache key for the operation.
- `PRODUCER-FN` (lambda): A 0-arity lambda that computes the value and
  returns a `concur` promise.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): A resolved logger function.

Returns:
  (concur-promise) A promise that will resolve to the computed value."
  (let* ((name (cacheus-options-name (cacheus-instance-options instance)))
         (rtd (cacheus-instance-runtime-data instance))
         (inflight-ht (cacheus-runtime-data-inflight-ht rtd))
         (existing-promise (gethash ekey inflight-ht)))
    (if existing-promise
        (progn (funcall logger :debug "[C:%s] async: Attaching to in-flight promise for %S"
                        name ekey)
               existing-promise)
      (let* ((future (concur:make-future producer-fn
                                         :mode (cacheus-options-async instance)))
             (promise (concur:force future)))
        (funcall logger :debug "[C:%s] async: Tracking new promise for %S" name ekey)
        (puthash ekey promise inflight-ht)
        (concur:finally promise (lambda ()
                                  (funcall logger :debug "[C:%s] async: Cleaning up for %S"
                                           name ekey)
                                  (remhash ekey inflight-ht)))
        promise))))

(defun cacheus-create-entry (instance key value)
  "Create a cache entry struct for INSTANCE with KEY and VALUE at runtime.
This function dynamically constructs the appropriate entry struct,
handling standard fields as well as any custom fields defined by the
user via the `:fields` and `:meta-fn` options.

Arguments:
- `INSTANCE` (cacheus-instance): The live cache instance.
- `KEY` (any): The user-provided key for this entry.
- `VALUE` (any): The computed value to be stored.

Returns:
  (struct) A newly created cache entry struct."
  (let* ((opts (cacheus-instance-options instance))
         (syms (cacheus-instance-symbols instance))
         (name (cacheus-options-name opts))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (meta-fn (cacheus-options-meta-fn opts))
         (ctor-fn (cacheus-symbols-make-fn-constructor-for-entries syms))
         (ver-var (cacheus-symbols-version-id-var syms))
         (all-fields (cacheus-symbols-all-struct-fields-for-entries syms))
         (custom-fields (-remove (lambda (f) (memq (car f)
                                                   '(key data timestamp entry-version)))
                                 all-fields))
         (meta (if meta-fn (funcall meta-fn key value)))
         args)
    (when (and meta (not (listp meta)))
      (funcall logger :warn "[C:%s] :meta-fn returned non-plist: %S" name meta)
      (setq meta nil))
    (setq args `(:key ,key :data ,value :timestamp ,(ts-now)
                 :entry-version ,(if (boundp ver-var) (symbol-value ver-var))))
    (dolist (field-def custom-fields)
      (let* ((field-name (car field-def))
             (field-kw (intern (format ":%s" field-name))))
        (setq args (append args (list field-kw (plist-get meta field-name))))))
    (apply ctor-fn args)))

(provide 'cacheus-storage)
;;; cacheus-storage.el ends here