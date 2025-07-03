;;; cacheus-eviction.el --- Eviction and staleness management for Cacheus -*-
;;; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the core logic for managing cache entries based on
;; eviction policies, staleness criteria, and periodic cleanup routines. It
;; defines generic functions for choosing victims, updating access metadata,
;; checking entry freshness, and performing cleanup operations.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'ring)
(require 'ts)
(require 'dash)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-Level API (Package-Private)

(defun cacheus-is-entry-stale (key entry instance logger)
  "Check if a cache ENTRY for KEY in INSTANCE is stale.
This function aggregates all staleness checks: TTL, version
mismatch, and custom dirty predicates.

Arguments:
- `KEY` (any): The effective cache key.
- `ENTRY` (struct): The cache entry struct to check.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): The resolved logger function.

Returns:
  (string or nil) A string describing the reason for staleness,
  or `nil` if the entry is fresh."
  (cacheus-let*
      (((&struct :options opts :symbols syms :runtime-data rtd) instance)
       ((&struct :ttl ttl-opt :version ver-opt :dirty-p dp-opt
                 :refresh-ttl ref-opt) opts)
       ((&struct :key-accessor-for-entries key-acc
                 :ts-accessor-for-entries ts-acc
                 :entry-ver-accessor-for-entries ver-acc) syms)
       ((&struct :timestamps-ht ts-ht) rtd)
       (ttl (cacheus-resolve-option-value ttl-opt key))
       (ver (cacheus-resolve-option-value ver-opt key))
       (entry-ts (funcall ts-acc entry))
       (entry-ver (funcall ver-acc entry))
       (refresh-ts (when (and ts-ht ref-opt) (gethash key ts-ht))))
    ;; This `cond` block performs the actual staleness checks.
    (cond
     ((not entry-ts) "No timestamp")
     ;; Stale if TTL is set and has expired for both the original entry
     ;; and its last refresh time (if any).
     ((and ttl (ts-p entry-ts) (> (ts-diff (ts-now) entry-ts) ttl)
           (or (not refresh-ts) (not (ts-p refresh-ts))
               (> (ts-diff (ts-now) refresh-ts) ttl)))
      "TTL expired")
     ;; Stale if the entry's cached version doesn't match the current version.
     ((and ver entry-ver (not (equal entry-ver ver)))
      "Version mismatch")
     ;; Stale if a custom :dirty-p predicate returns true.
     ((and dp-opt (funcall dp-opt key entry))
      "Custom dirty-p")
     ;; Otherwise, the entry is considered fresh.
     (t nil))))

(defun cacheus-update-instance-on-hit (key instance)
  "Update cache metadata for KEY on a cache hit in INSTANCE.
This is called when a fresh entry is successfully retrieved. It
updates the data structures for LRU/LFU eviction policies and
refreshes the entry's TTL if configured.

Arguments:
- `KEY` (any): The effective cache key that was hit.
- `INSTANCE` (cacheus-instance): The live cache instance.

Returns:
  None. Modifies the instance in-place."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts))))
    (cacheus--eviction-update-on-hit (cacheus-options-eviction-strategy opts)
                                     key
                                     (cacheus-runtime-data-order-data rtd)
                                     (cacheus-runtime-data-frequency-ht rtd)
                                     logger)
    (when (and (cacheus-options-refresh-ttl opts)
               (cacheus-options-ttl opts)
               (cacheus-runtime-data-timestamps-ht rtd))
      (puthash key (ts-now) (cacheus-runtime-data-timestamps-ht rtd)))))

(defun cacheus-evict-one-entry (victim-key instance logger)
  "Completely remove VICTIM-KEY from all data structures in INSTANCE.
This function ensures that an entry is cleanly removed from all
relevant data structures: the main cache hash table, the
timestamps table, eviction-specific data (order ring or frequency
table), and all tag indexes.

Arguments:
- `VICTIM-KEY` (any): The key of the entry to evict.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): The resolved logger function.

Returns:
  None. Modifies the instance in-place."
  (let* ((rtd (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name (cacheus-instance-options instance))))
    (when victim-key
      (funcall logger :debug "[C:%s] Evicting key: %S" name victim-key)
      (remhash victim-key (cacheus-runtime-data-cache-ht rtd))
      (when-let (ts-ht (cacheus-runtime-data-timestamps-ht rtd))
        (remhash victim-key ts-ht))
      (when-let (freq-ht (cacheus-runtime-data-frequency-ht rtd))
        (remhash victim-key freq-ht))
      (when-let (order (cacheus-runtime-data-order-data rtd))
        (ring-remove order victim-key))
      (cacheus-remove-entry-tag-info victim-key instance logger))))

(defun cacheus-clear-runtime-data (instance)
  "Clear all live data from a cache INSTANCE without affecting its config.
Arguments:
- `INSTANCE` (cacheus-instance): The live instance to clear.

Returns:
  None. Modifies the instance in-place."
  (let ((rtd (cacheus-instance-runtime-data instance)))
    (clrhash (cacheus-runtime-data-cache-ht rtd))
    (when-let (ts-ht (cacheus-runtime-data-timestamps-ht rtd)) (clrhash ts-ht))
    (when-let (order (cacheus-runtime-data-order-data rtd)) (ring-clear order))
    (when-let (freq-ht (cacheus-runtime-data-frequency-ht rtd)) (clrhash freq-ht))
    (when-let (et-ht (cacheus-runtime-data-entry-tags-ht rtd)) (clrhash et-ht))
    (when-let (tags-idx (cacheus-runtime-data-tags-idx-ht rtd))
      (clrhash tags-idx))
    (when-let (inflight (cacheus-runtime-data-inflight-ht rtd))
      (clrhash inflight))))

(defun cacheus-eviction-prepare-for-put (key instance)
  "Prepare INSTANCE for a new entry. Returns a victim key if eviction is needed.
If the cache is at capacity and a new key is being added, this function
chooses a victim to evict based on the configured strategy. If the key
already exists, it updates its eviction metadata instead.

Arguments:
- `KEY` (any): The key for the entry about to be inserted.
- `INSTANCE` (cacheus-instance): The live cache instance.

Returns:
  (any or nil) The key of the victim to evict, or nil if no eviction is needed."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (cap (cacheus-options-capacity opts))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (when (and cap (> cap 0) (>= (hash-table-count cache-ht) cap))
      (if (gethash key cache-ht)
          ;; If key exists, we are updating, not evicting a different key.
          ;; Update its eviction metadata instead.
          (progn (cacheus-update-instance-on-hit key instance) nil)
        ;; Otherwise, choose a victim to evict.
        (cacheus--eviction-choose-victim instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Eviction Logic (File-Local)

(defun cacheus--eviction-update-on-hit (strategy key order-data freq-ht logger)
  "Update eviction metadata (LRU/LFU) for KEY upon a cache hit."
  (pcase strategy
    (:lru
     (when order-data (ring-remove order-data key) (ring-insert order-data key)))
    (:lfu
     (when freq-ht (puthash key (1+ (or (gethash key freq-ht) 0)) freq-ht)))))

(defun cacheus--eviction-choose-victim (instance)
  "Choose a victim key for eviction based on the instance's strategy."
  (let* ((opts (cacheus-instance-options instance))
         (rtd (cacheus-instance-runtime-data instance))
         (strategy (cacheus-options-eviction-strategy opts))
         (order (cacheus-runtime-data-order-data rtd))
         (freq-ht (cacheus-runtime-data-frequency-ht rtd))
         (cache-ht (cacheus-runtime-data-cache-ht rtd)))
    (pcase strategy
      ((or :lru :fifo) (when order (ring-back order)))
      (:lfu
       (when freq-ht
         (let ((min-freq most-positive-fixnum) victim)
           (maphash (lambda (k f)
                      (when (and (gethash k cache-ht) (< f min-freq))
                        (setq min-freq f victim k)))
                    freq-ht)
           victim)))
      (_ nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Helper Functions

;;;###autoload
(cl-defun cacheus-make-dirty-p-predicate (&key ttl get-timestamp
                                               get-version-for-key
                                               get-entry-version get-mtime)
  "Return a predicate function suitable for the `:dirty-p` option.
This is a constructor for creating complex staleness predicates.
You can combine multiple checks into a single function.

Arguments:
- `:TTL` (integer): A time-to-live in seconds.
- `:GET-TIMESTAMP` (function): A function that takes an entry and returns
  its `ts` struct for comparison against the TTL.
- `:GET-VERSION-FOR-KEY` (function): A function that takes a key and returns
  the *current* version of the resource.
- `:GET-ENTRY-VERSION` (function): A function that takes a cache entry and
  returns its cached version for comparison.
- `:GET-MTIME` (function): A function that takes an entry or key and returns
  its modification time as a `ts` struct for comparison.

Returns:
  (function) A predicate of the form `(lambda (key entry))` that returns
  non-nil if the entry is considered dirty (stale)."
  (lambda (key entry)
    (or (and ttl get-timestamp
             (let ((ts (funcall get-timestamp entry)))
               (and (ts-p ts) (> (ts-diff (ts-now) ts) ttl))))
        (and get-version-for-key get-entry-version
             (not (equal (funcall get-version-for-key key)
                         (funcall get-entry-version entry))))
        (and get-mtime
             (let ((res-mtime (funcall get-mtime key))
                   (entry-mtime (funcall get-mtime entry)))
               (and (ts-p res-mtime) (ts-p entry-mtime)
                    (ts> res-mtime entry-mtime)))))))

(provide 'cacheus-eviction)
;;; cacheus-eviction.el ends here