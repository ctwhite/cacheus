;;; cacheus-structs.el --- Core data structures for the Cacheus framework -*-
;;; lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file defines all the `cl-defstruct` data structures that form the
;; backbone of the Cacheus caching framework. Separating these definitions
;; into a foundational file ensures a clear dependency hierarchy and allows
;; other modules to `require` them without circular dependencies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Data Structures

(cl-defstruct cacheus-options
  "Holds all user-configurable options for a cache instance.
This struct is created at macro-expansion time and stored within a
`cacheus-instance` to be available at runtime.

Fields:
- `name` (symbol): The unique public name of the cache.
- `logger` (function|symbol|boolean): The logger to use. Can be a function,
  a symbol naming a function, `t` for `message`, or `nil` for no-op.
- `capacity` (integer): The maximum number of entries in the cache.
- `eviction-strategy` (keyword): One of `:lru`, `:lfu`, `:fifo`, or `:none`.
- `cache-file` (string): The file path for persistence.
- `version` (any): A value used for cache invalidation. If the stored version
  of an entry does not match this, it is considered stale.
- `periodic-cleanup` (number): Interval in seconds for periodic cleanup task.
- `predicate` (function): A function `(lambda (value))` that must return
  non-nil for a newly computed value to be cached.
- `async` (boolean|symbol): If non-nil, computations are handled
  asynchronously using `concur`. Can be a `concur` mode like `:thread`.
- `error-handler` (function): A function `(lambda (error-reason))` called
  when a value computation fails.
- `ttl` (number): The default time-to-live for cache entries, in seconds.
- `refresh-ttl` (boolean): If non-nil, renews an entry's TTL on every access.
- `expiration-hook` (function): A function `(lambda (key entry))` called
  when an entry is evicted due to expiration or capacity.
- `dirty-p` (function): A predicate `(lambda (key entry))` to check if an
  entry is stale, overriding default TTL/version checks.
- `clear-hook` (function): A function `(lambda ())` called after a cache is cleared.
- `prefix` (string): The prefix for generated symbols (e.g., functions, vars).
- `fields-data` (list): A list of custom fields `(name initial-value . options)`
  to add to this cache's entry struct.
- `meta-fn` (function): A function `(lambda (key value))` that returns a
  plist of metadata to store in a cache entry's custom fields.
- `tags-fn` (function): A function `(lambda (key value))` that returns a
  list of tags to associate with a cache entry."
  name logger capacity eviction-strategy cache-file version periodic-cleanup
  predicate async error-handler ttl refresh-ttl expiration-hook dirty-p
  clear-hook prefix fields-data meta-fn tags-fn)

(cl-defstruct cacheus-symbols
  "Holds all generated symbols for a cache instance.
This struct is part of the library's metaprogramming approach, creating
all necessary names for a cache's functions and variables programmatically.

Fields:
- `sym-prefix` (string): The base prefix, e.g., `cacheus--my-cache`.
- `struct-name-for-entries` (symbol): The name of the cache's entry struct.
- `make-fn-constructor-for-entries` (symbol): Constructor for the entry struct.
- `key-accessor-for-entries` (symbol): Accessor for the `:key` slot.
- `data-accessor-for-entries` (symbol): Accessor for the `:data` slot.
- `ts-accessor-for-entries` (symbol): Accessor for the `:timestamp` slot.
- `entry-ver-accessor-for-entries` (symbol): Accessor for `:entry-version`.
- `all-struct-fields-for-entries` (list): The full slot definition alist.
- `cache-var` (symbol): The `defvar` for the main cache hash table.
- `timestamps-var` (symbol): The `defvar` for the TTL timestamp hash table.
- `order-var` (symbol): The `defvar` for LRU/FIFO ordering data.
- `frequency-var` (symbol): The `defvar` for the LFU frequency hash table.
- `version-id-var` (symbol): The `defvar` holding the cache's version.
- `inflight-var` (symbol): `defvar` for the async in-flight requests table.
- `save-fn` (symbol): The name of the generated `-save` function.
- `load-fn` (symbol): The name of the generated `-load` function.
- `clear-fn` (symbol): The name of the generated `-clear` function.
- `inspect-fn` (symbol): The name of the generated `-inspect` function.
- `entry-tags-var` (symbol): `defvar` for the entry -> tags hash table.
- `tags-idx-var` (symbol): `defvar` for the tag -> keys hash table.
- `invalidate-tags-fn` (symbol): Name of `-invalidate-by-tags` fn."
  sym-prefix struct-name-for-entries make-fn-constructor-for-entries
  key-accessor-for-entries data-accessor-for-entries ts-accessor-for-entries
  entry-ver-accessor-for-entries all-struct-fields-for-entries cache-var
  timestamps-var order-var frequency-var version-id-var inflight-var
  save-fn load-fn clear-fn inspect-fn entry-tags-var tags-idx-var
  invalidate-tags-fn)

(cl-defstruct cacheus-runtime-data
  "Holds the live, mutable data for a cache instance at runtime.
This struct is created lazily on first access to a cache.

Fields:
- `cache-ht` (hash-table): The main key -> entry-struct hash table.
- `timestamps-ht` (hash-table): The key -> timestamp hash table for TTL.
- `order-data` (ring or list): Data structure for LRU/FIFO eviction.
- `frequency-ht` (hash-table): key -> access-count table for LFU eviction.
- `entry-tags-ht` (hash-table): key -> list-of-tags hash table.
- `tags-idx-ht` (hash-table): tag -> list-of-keys reverse index.
- `inflight-ht` (hash-table): key -> promise table for async operations."
  cache-ht timestamps-ht order-data frequency-ht entry-tags-ht
  tags-idx-ht inflight-ht)

(cl-defstruct cacheus-instance
  "The base blueprint struct for any cache instance.
This struct holds the complete configuration for a cache, generated at
macro-expansion time by `defcacheus`. It is stored in a `defvar` and used
as a template for the live cache instance, which is created on first use.

Fields:
- `options` (cacheus-options): User-provided configuration.
- `symbols` (cacheus-symbols): Generated names for vars and functions.
- `runtime-data` (cacheus-runtime-data): The live data (used as a template)."
  options symbols runtime-data)

(provide 'cacheus-structs)
;;; cacheus-structs.el ends here