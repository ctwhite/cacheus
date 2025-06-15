;;; cacheus-eviction.el --- Eviction and staleness management for Cacheus -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file provides the core logic for managing cache entries based on
;; eviction policies, staleness criteria, and periodic cleanup routines. It
;; defines generic functions for choosing victims, updating access metadata,
;; checking entry freshness, and performing cleanup operations.
;;
;; This logic is used by higher-level modules to implement features like
;; time-to-live (TTL) expiration and capacity-based eviction (LRU, LFU, FIFO).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'ring)
(require 'ts)
(require 'dash)

(require 'cacheus-structs)
(require 'cacheus-util)
(require 'cacheus-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-Level API

(defun cacheus-is-instance-entry-stale (key entry-struct instance logger)
  "Check if a cache `ENTRY-STRUCT` for `KEY` in `INSTANCE` is stale.
This is a high-level wrapper around `cacheus--is-entry-stale-p` that
gathers and resolves the necessary parameters from the live cache `INSTANCE`.

Arguments:
- `KEY` (any): The effective key of the entry being checked.
- `ENTRY-STRUCT` (struct): The cache entry struct.
- `INSTANCE` (cacheus-instance): The live instance of the cache.
- `LOGGER` (function): The resolved logger function.

Returns:
  (string|nil): A string describing the reason if stale, or `nil` if fresh."
  (cacheus-let*
      (((&struct :options opts :symbols syms :runtime-data data) instance)
       ;; Get the raw options, which might be symbols that need resolving.
       ((&struct :ttl ttl-opt :version version-opt
                 :dirty-p dirty-p-opt :refresh-ttl refresh-opt) opts)
       ((&struct :key-accessor-for-entries key-acc
                 :ts-accessor-for-entries ts-acc
                 :entry-ver-accessor-for-entries ver-acc) syms)
       ((&struct :timestamps-ht ts-ht) data)
       ;; Resolve the options to their runtime values.
       (resolved-ttl (cacheus-resolve-option-value ttl-opt))
       (resolved-version (cacheus-resolve-option-value version-opt))
       (entry-ts (funcall ts-acc entry-struct))
       (entry-ver (funcall ver-acc entry-struct))
       (refresh-ts (when (and ts-ht refresh-opt) (ht-get ts-ht key))))
    ;; Pass the RESOLVED values to the low-level checker.
    (cacheus--is-entry-stale-p (funcall key-acc entry-struct)
                               entry-struct entry-ts resolved-ttl
                               entry-ver resolved-version
                               dirty-p-opt refresh-ts logger)))

(defun cacheus-update-instance-on-hit (key instance)
  "Update cache metadata on hit for `INSTANCE`.
This orchestrates updates for eviction metadata (LRU/LFU) and sliding TTL.

Arguments:
- `KEY` (any): The effective cache key that was hit.
- `INSTANCE` (cacheus-instance): The live instance of the cache.

Returns:
  nil."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :eviction-strategy strategy :refresh-ttl refresh-ttl
                 :ttl ttl :name name :logger logger-opt) opts)
       ((&struct :order-data order :frequency-ht freq-ht
                 :timestamps-ht ts-ht) data)
       (logger (cacheus-resolve-logger logger-opt)))
    ;; 1. Update LRU/LFU metadata.
    (cacheus-eviction-update-on-hit strategy key order freq-ht logger)
    ;; 2. Handle sliding TTL by updating the external timestamp.
    (when (and refresh-ttl ttl ts-ht)
      (ht-set! ts-ht key (ts-now))
      (funcall logger :debug "[C:%S] Refreshed TTL for key: %S" name key))))

(defun cacheus-evict-one-entry (victim-key instance logger)
  "Completely remove `VICTIM-KEY` from all data structures in `INSTANCE`.
This ensures an evicted key is removed from the main cache, timestamp and
frequency tables, the ordering ring, and its associated tag information.

Arguments:
- `VICTIM-KEY` (any): The key of the entry to evict.
- `INSTANCE` (cacheus-instance): The live instance of the cache.
- `LOGGER` (function): The resolved logger function for debug messages.

Returns:
  nil."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name) opts)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht) data))
    (when victim-key
      (funcall logger :debug "[C:%S] Evicting key: %S" name victim-key)
      (ht-remove! cache-ht victim-key)
      (when ts-ht (ht-remove! ts-ht victim-key))
      (when freq-ht (ht-remove! freq-ht victim-key))
      (when order (ring-remove order victim-key))
      (cacheus-remove-entry-tag-info victim-key instance logger))))

(defun cacheus-clear-expired-entries-instance (instance)
  "Clear all expired entries from `INSTANCE` based on its TTL.
This function is typically called periodically by a timer or cleanup process.

Arguments:
- `INSTANCE` (cacheus-instance): The live instance of the cache to clean.

Returns:
  nil."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name :ttl ttl :expiration-hook hook :logger logger-opt) opts)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht) data)
       (logger (cacheus-resolve-logger logger-opt)))
    (when ttl
      (let ((expired-keys (cacheus-get-expired-keys ts-ht ttl logger)))
        (when expired-keys
          (funcall logger :info "[C:%S] Clearing %d expired entries (TTL: %s)."
                   name (length expired-keys) ttl)
          (-each expired-keys
                 (lambda (key)
                   (let ((entry (ht-get cache-ht key)))
                     (cacheus-evict-one-entry key instance logger)
                     (when (and hook entry)
                       (condition-case-unless-debug e (funcall hook key entry)
                         (error (funcall logger :error
                                         "[C:%S] Expired hook error for %S: %S"
                                         name key e :trace))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Eviction Logic

(defun cacheus-eviction-update-on-hit (strategy key order-data frequency-ht logger)
  "Update eviction metadata (LRU/LFU) for `KEY` upon a cache hit.

Arguments:
- `STRATEGY` (keyword): Eviction strategy (`:lru`, `:lfu`, `:fifo`, or `nil`).
- `KEY` (any): The cache key that was hit.
- `ORDER-DATA` (ring): The ring buffer for LRU/FIFO.
- `FREQUENCY-HT` (hash-table): The hash table for LFU frequencies.
- `LOGGER` (function): The resolved logger function.

Returns:
  nil."
  (pcase strategy
    (:lru
     (when order-data (ring-remove order-data key) (ring-insert order-data key))
     (funcall logger :debug "evict-update: LRU updated for %S" key))
    (:lfu
     (when frequency-ht
       (ht-set! frequency-ht key (1+ (or (ht-get frequency-ht key) 0))))
     (funcall logger :debug "evict-update: LFU freq for %S is now %S"
              key (ht-get frequency-ht key)))
    (:fifo (funcall logger :debug "evict-update: FIFO no-op on hit for %S" key))
    (_ nil)))

(defun cacheus-eviction-choose-victim
    (strategy order-data frequency-ht cache-ht logger)
  "Choose a victim key for eviction based on the specified `STRATEGY`.

Arguments:
- `STRATEGY` (keyword): Eviction strategy (`:lru`, `:lfu`, `:fifo`).
- `ORDER-DATA` (ring): The ring buffer for LRU/FIFO.
- `FREQUENCY-HT` (hash-table): The hash table for LFU frequencies.
- `CACHE-HT` (hash-table): The main cache hash table.
- `LOGGER` (function): The resolved logger function.

Returns:
  (any): The key of the victim to be evicted, or `nil`."
  (pcase strategy
    ((or :lru :fifo) (when order-data (ring-back order-data)))
    (:lfu
     (when frequency-ht
       ;; NOTE: This is a simple O(n) scan. A more complex implementation
       ;; could achieve O(1) eviction but is not necessary for most use cases.
       (let ((min-freq most-positive-fixnum) (victim-key nil))
         (ht-map frequency-ht
                 (lambda (k f)
                   (when (and (ht-contains? cache-ht k) (< f min-freq))
                     (setq min-freq f victim-key k))))
         (funcall logger :debug "evict-choose: LFU victim: %S (freq: %d)"
                  victim-key min-freq)
         victim-key)))
    (_ (funcall logger :warn "evict-choose: Unknown strategy %S" strategy)
       nil)))

(defun cacheus-eviction-prepare-for-put (key instance)
  "Prepare `INSTANCE` for a new entry for `KEY`.
If the cache is at capacity and `KEY` is new, a victim key is returned.
If `KEY` already exists, its eviction metadata is simply updated.

Arguments:
- `KEY` (any): The effective cache key to be inserted.
- `INSTANCE` (cacheus-instance): The live instance to operate on.

Returns:
  (any): The key of a victim to evict, or `nil` if no eviction is needed."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :capacity cap :eviction-strategy strategy :logger logger-opt) opts)
       ((&struct :cache-ht cache-ht :order-data order :frequency-ht freq-ht) data)
       (logger (cacheus-resolve-logger logger-opt)))
    (when (and cap (> cap 0) (>= (ht-size cache-ht) cap))
      (if (ht-contains? cache-ht key)
          ;; Key exists, just update its hit metadata and don't evict.
          (progn (cacheus-eviction-update-on-hit strategy key order freq-ht logger)
                 nil)
        ;; Cache is full and key is new, so choose a victim.
        (cacheus-eviction-choose-victim strategy order freq-ht cache-ht logger)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Staleness Logic

(defun cacheus--is-entry-stale-p (key entry-struct entry-timestamp ttl
                                     entry-version current-func-version
                                     dirty-p-fn refresh-ts logger)
  "Low-level staleness check; assumes all values are provided.
This pure function determines if a cache entry is stale based on several
criteria, returning a reason string if stale, nil otherwise.

Arguments:
- `KEY` (any): The cache key.
- `ENTRY-STRUCT` (struct): The cache entry struct.
- `ENTRY-TIMESTAMP` (ts): Timestamp of the entry.
- `TTL` (number): Time-to-live in seconds.
- `ENTRY-VERSION` (any): Version of the entry.
- `CURRENT-FUNC-VERSION` (any): Current functional version of the cache.
- `DIRTY-P-FN` (function): Custom predicate for staleness.
- `REFRESH-TS` (ts): Timestamp of last refresh (for sliding TTL).
- `LOGGER` (function): The resolved logger function.

Returns:
  (string|nil): A string describing the reason if stale, or `nil` if fresh."
  (cond
   ((not entry-timestamp)
    (funcall logger :warn "stale-p: Entry %S lacks timestamp. Stale." key)
    "No timestamp")
   ((and ttl (ts-p entry-timestamp) (> (ts-diff (ts-now) entry-timestamp) ttl)
         ;; Also check sliding TTL if refresh-ts is available.
         (or (not refresh-ts) (not (ts-p refresh-ts))
             (> (ts-diff (ts-now) refresh-ts) ttl)))
    (funcall logger :debug "stale-p: TTL expired for %S (age: %s, ttl: %s)."
             key (ts-diff (ts-now) entry-timestamp) ttl)
    "TTL expired")
   ((and current-func-version entry-version
         (not (equal entry-version current-func-version)))
    (funcall logger :debug "stale-p: Version mismatch for %S (entry: %S, now: %S)."
             key entry-version current-func-version)
    "Version mismatch")
   ((and dirty-p-fn (funcall dirty-p-fn key entry-struct))
    (funcall logger :debug "stale-p: Custom dirty-p for %S returned true." key)
    "Custom dirty-p")
   (t nil)))

(defun cacheus-get-expired-keys (timestamps-ht ttl logger)
  "Return a list of keys from `TIMESTAMPS-HT` that have expired by `TTL`.

Arguments:
- `TIMESTAMPS-HT` (hash-table): A mapping keys to `ts` timestamp objects.
- `TTL` (number): The time-to-live in seconds.
- `LOGGER` (function): The resolved logger function for debug messages.

Returns:
  (list): A list of keys that are considered expired."
  (let ((expired '()))
    (when timestamps-ht
      (ht-map timestamps-ht
              (lambda (key ts-obj)
                (when (and ttl (ts-p ts-obj)
                           (> (ts-diff (ts-now) ts-obj) ttl))
                  (funcall logger :debug
                           "expired-keys: Key %S (age %s > TTL %s)."
                           key (ts-diff (ts-now) ts-obj) ttl)
                  (push key expired)))))
    expired))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Helper Functions

;;;###autoload
(cl-defun cacheus-make-dirty-p (&key ttl get-timestamp
                                     get-version-for-key get-entry-version
                                     get-mtime)
  "Return a predicate suitable for the `:dirty-p` option.
This factory function helps construct common `:dirty-p` predicates based on
TTL, version, or file modification time checks.

Arguments:
- `:ttl` (integer): Time-to-live in seconds.
- `:get-timestamp` (function): A `(lambda (entry))` -> `ts` object.
- `:get-version-for-key` (function): A `(lambda (key))` -> current version.
- `:get-entry-version` (function): A `(lambda (entry))` -> cached version.
- `:get-mtime` (function): A `(lambda (key-or-entry))` -> `ts` mtime object.

Returns:
  (function): A lambda `(lambda (key entry))` that returns `t` if dirty."
  (lambda (key entry)
    (or
     ;; TTL check
     (and ttl get-timestamp
          (let ((ts (funcall get-timestamp entry)))
            (and (ts-p ts) (> (ts-diff (ts-now) ts) ttl))))
     ;; Version check
     (and get-version-for-key get-entry-version
          (not (equal (funcall get-version-for-key key)
                      (funcall get-entry-version entry))))
     ;; Modification time check
     (and get-mtime
          (let ((resource-mtime (funcall get-mtime key))
                (entry-mtime (funcall get-mtime entry)))
            (and (ts-p resource-mtime) (ts-p entry-mtime)
                 (ts> resource-mtime entry-mtime)))))))

(provide 'cacheus-eviction)
;;; cacheus-eviction.el ends here