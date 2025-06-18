;;; cacheus-eviction.el --- Eviction and staleness management for Cacheus -*- lexical-binding: t; -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-Level API (Package-Private)

(defun cacheus-is-entry-stale (key entry instance logger)
  "Check if a cache ENTRY for KEY in INSTANCE is stale.

This function aggregates all staleness checks: TTL, version
mismatch, and custom dirty predicates.

Arguments:
  KEY (any): The effective cache key.
  ENTRY (struct): The cache entry struct to check.
  INSTANCE (cacheus-instance): The live cache instance.
  LOGGER (function): The resolved logger function.

Returns:
  (string or nil) A string describing the reason for staleness,
  or nil if the entry is fresh."
  (cacheus-let*
      (((&struct :options opts :symbols syms :runtime-data data) instance)
       ((&struct :ttl ttl-opt
                 :version ver-opt
                 :dirty-p dp-opt
                 :refresh-ttl ref-opt)
        opts)
       ((&struct :key-accessor-for-entries key-acc
                 :ts-accessor-for-entries ts-acc
                 :entry-ver-accessor-for-entries ver-acc)
        syms)
       ((&struct :timestamps-ht ts-ht) data)
       (ttl (cacheus-resolve-option-value ttl-opt))
       (ver (cacheus-resolve-option-value ver-opt))
       (entry-ts (funcall ts-acc entry))
       (entry-ver (funcall ver-acc entry))
       (refresh-ts (when (and ts-ht ref-opt) (ht-get ts-ht key))))
    (cacheus--is-entry-stale-p (funcall key-acc entry) entry-ts ttl
                               entry-ver ver dp-opt refresh-ts entry logger)))

(defun cacheus-update-instance-on-hit (key instance)
  "Update cache metadata for KEY on a cache hit in INSTANCE.

This is called when a fresh entry is successfully retrieved. It
updates the data structures for LRU/LFU eviction policies and
refreshes the entry's TTL if configured.

Arguments:
  KEY (any): The effective cache key that was hit.
  INSTANCE (cacheus-instance): The live cache instance.

Returns: None."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :eviction-strategy strategy
                 :refresh-ttl refresh-ttl
                 :ttl ttl :name name :logger lopt)
        opts)
       ((&struct :order-data order :frequency-ht freq-ht :timestamps-ht ts-ht) data)
       (logger (cacheus-resolve-logger lopt)))
    (cacheus--eviction-update-on-hit strategy key order freq-ht logger)
    (when (and refresh-ttl ttl ts-ht)
      (ht-set! ts-ht key (ts-now))
      (funcall logger :debug "[C:%s] Refreshed TTL for key: %S" name key))))

(defun cacheus-evict-one-entry (victim-key instance logger)
  "Completely remove VICTIM-KEY from all data structures in INSTANCE.

This function ensures that an entry is cleanly removed from all
relevant data structures: the main cache hash table, the
timestamps table, eviction-specific data (order ring or frequency
table), and all tag indexes.

Arguments:
  VICTIM-KEY (any): The key of the entry to evict.
  INSTANCE (cacheus-instance): The live cache instance.
  LOGGER (function): The resolved logger function.

Returns: None."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name) opts)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht)
        data))
    (when victim-key
      (funcall logger :debug "[C:%s] Evicting key: %S" name victim-key)
      (ht-remove! cache-ht victim-key)
      (when ts-ht (ht-remove! ts-ht victim-key))
      (when freq-ht (ht-remove! freq-ht victim-key))
      (when order (ring-remove order victim-key))
      ;; This function is defined in `cacheus-tags.el`, which is loaded
      ;; before this file, so no circular dependency exists at load time.
      (cacheus-remove-entry-tag-info victim-key instance logger))))

(defun cacheus-clear-runtime-data (instance)
  "Clear all live data from a cache INSTANCE without affecting its config."
  (cacheus-let*
      (((&struct :runtime-data data) instance)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht :entry-tags-ht et-ht
                 :tags-idx-ht tags-idx-ht :inflight-ht inflight-ht)
        data))
    (ht-clear! cache-ht)
    (when ts-ht (ht-clear! ts-ht))
    (when order (ring-clear order))
    (when freq-ht (ht-clear! freq-ht))
    (when et-ht (ht-clear! et-ht))
    (when tags-idx-ht (ht-clear! tags-idx-ht))
    (when inflight-ht (ht-clear! inflight-ht))))

(defun cacheus-eviction-prepare-for-put (key instance)
  "Prepare INSTANCE for a new entry. Returns victim key if needed."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :capacity cap :eviction-strategy strategy :logger lopt) opts)
       ((&struct :cache-ht cache-ht :order-data order :frequency-ht freq-ht) data)
       (logger (cacheus-resolve-logger lopt)))
    (when (and cap (> cap 0) (>= (ht-size cache-ht) cap))
      (if (ht-contains? cache-ht key)
          (progn (cacheus--eviction-update-on-hit strategy key order freq-ht logger) nil)
        (cacheus--eviction-choose-victim strategy order freq-ht cache-ht logger)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Eviction Logic (File-Local)

(defun cacheus--eviction-update-on-hit (strategy key order-data freq-ht logger)
  "Update eviction metadata (LRU/LFU) for KEY upon a cache hit."
  (pcase strategy
    (:lru
     (when order-data (ring-remove order-data key) (ring-insert order-data key))
     (funcall logger :debug "evict-update: LRU updated for %S" key))
    (:lfu
     (when freq-ht (ht-set! freq-ht key (1+ (or (ht-get freq-ht key) 0))))
     (funcall logger :debug "evict-update: LFU freq for %S is now %S"
              key (ht-get freq-ht key)))
    (:fifo
     (funcall logger :debug "evict-update: FIFO no-op on hit for %S" key))))

(defun cacheus--eviction-choose-victim (strategy order freq-ht cache-ht logger)
  "Choose a victim key for eviction based on the specified STRATEGY."
  (pcase strategy
    ((or :lru :fifo) (when order (ring-back order)))
    (:lfu
     (when freq-ht
       (let ((min-freq most-positive-fixnum) (victim nil))
         (ht-map freq-ht (lambda (k f)
                           (when (and (ht-contains? cache-ht k) (< f min-freq))
                             (setq min-freq f victim k))))
         (funcall logger :debug "evict-choose: LFU victim: %S (freq: %d)"
                  victim min-freq)
         victim)))
    (_ (funcall logger :warn "evict-choose: Unknown strategy %S" strategy) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Staleness Logic (File-Local)

(defun cacheus--is-entry-stale-p (key entry-ts ttl entry-ver
                                     current-ver dirty-p-fn refresh-ts entry logger)
  "Low-level staleness check; returns reason string if stale, nil if fresh."
  (cond
   ((not entry-ts)
    (funcall logger :warn "stale-p: Entry %S lacks timestamp." key) "No timestamp")
   ((and ttl (ts-p entry-ts) (> (ts-diff (ts-now) entry-ts) ttl)
         (or (not refresh-ts) (not (ts-p refresh-ts))
             (> (ts-diff (ts-now) refresh-ts) ttl)))
    (funcall logger :debug "stale-p: TTL expired for %S" key) "TTL expired")
   ((and current-ver entry-ver (not (equal entry-ver current-ver)))
    (funcall logger :debug "stale-p: Version mismatch for %S" key) "Version mismatch")
   ((and dirty-p-fn (funcall dirty-p-fn key entry))
    (funcall logger :debug "stale-p: Custom dirty-p for %S returned true." key)
    "Custom dirty-p")
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Helper Functions

;;;###autoload
(cl-defun cacheus-make-dirty-p-predicate (&key ttl get-timestamp get-version-for-key
                                               get-entry-version get-mtime)
  "Return a predicate function suitable for the `:dirty-p` option.

This is a constructor for creating complex staleness predicates.
You can combine multiple checks into a single function.

Arguments:
  TTL (integer): A time-to-live in seconds.
  GET-TIMESTAMP (function): A function that takes an entry struct
    and returns its timestamp (`ts` struct).
  GET-VERSION-FOR-KEY (function): A function that takes a key and
    returns the *current* version of the resource.
  GET-ENTRY-VERSION (function): A function that takes an entry
    struct and returns the version it was cached with.
  GET-MTIME (function): A function that takes an entry or a key
    and returns its modification time (`ts` struct).

Returns:
  (function) A predicate of two arguments (KEY, ENTRY) that
  returns true if the entry is dirty, nil otherwise."
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