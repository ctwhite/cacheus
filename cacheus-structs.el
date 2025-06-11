;;; cacheus-structs.el --- Shared struct definitions for the Cacheus library -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file defines the core `cl-defstruct` types that are shared across all
;; Cacheus modules. These structs serve as the foundational data models for
;; configuring caches, tracking generated symbols, and managing runtime cache data.
;;
;; They are designed to be included (`:include`) by module-specific structs
;; to promote consistency and code reuse throughout the Cacheus ecosystem.

;;; Code:

(require 'cl-lib) ; Required for `cl-defstruct`.
(require 'ht)     ; Required for `ht-create` in default initializers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Constants

(defconst cacheus-default-capacity 100
  "The default maximum number of entries a cache can hold if not specified.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared Struct Definitions

(cl-defstruct cacheus-options
  "Holds all configuration options for any Cacheus instance.
This struct represents the user's intent and configuration. Module-specific
options structs (e.g., `cacheus-memoize-options`) should `:include` this
struct to inherit these common fields. A `nil` value for most slots
indicates that the feature is disabled.

  Slots:
  - `name` (symbol): The user-defined name of the cache/memoized function.
  - `eviction-strategy` (keyword): Eviction strategy, e.g., `:lru`, `:lfu`.
  - `capacity` (integer): The maximum number of entries the cache can hold.
  - `version` (any): A functional version identifier for invalidating entries.
  - `async` (boolean): If non-nil, enables asynchronous computation.
  - `ttl` (integer): Time-to-live in seconds for cache entries.
  - `refresh-ttl` (boolean): If non-nil, an entry's TTL is reset on access.
  - `cache-file` (string): File path for persisting the cache to disk.
  - `expiration-hook` (function): A function called when an entry expires.
  - `clear-hook` (function): A function called when the entire cache is cleared.
  - `dirty-p` (function): A custom predicate to check if an entry is stale.
  - `logger` (any): Controls logging behavior (`t`, `nil`, symbol, or lambda).
  - `error-handler` (function): A function called on a computation error.
  - `predicate` (function): A function to validate if a value is cacheable.
  - `periodic-cleanup` (integer): Interval in seconds for periodic cleanup.
  - `prefix` (string): Prefix used to generate unique function/variable names.
  - `fields-data` (list): Custom field definitions for the entry struct.
  - `meta-fn` (function): A function to populate custom fields.
  - `tags-fn` (function): A function to generate tags for an entry."
  name
  (eviction-strategy :lru)
  (capacity cacheus-default-capacity) 
  version
  async
  ttl
  refresh-ttl
  cache-file
  expiration-hook
  clear-hook
  dirty-p
  logger
  error-handler
  predicate
  periodic-cleanup
  prefix
  fields-data
  meta-fn
  tags-fn)

(cl-defstruct cacheus-symbols
  "Holds all generated symbols for any Cacheus instance.
These symbols name the `defvar`s and `defun`s that constitute a
macro-defined cache instance. They are the output of the symbol generation
process based on user-provided options.

  Slots:
  - `sym-prefix` (string): The base prefix used to generate all other symbols.
  - `struct-name-for-entries` (symbol): The generated name of the `cl-defstruct`
    that holds individual cache entries.
  - `make-fn-constructor-for-entries` (symbol): The constructor function
    symbol for the entry struct.
  - `ts-accessor-for-entries` (symbol): The accessor for the `timestamp` slot.
  - `data-accessor-for-entries` (symbol): The accessor for the `data` slot.
  - `entry-ver-accessor-for-entries` (symbol): The accessor for the
    `entry-version` slot.
  - `all-struct-fields-for-entries` (list): The complete list of field
    specifications for the entry struct.
  - `get-fn` (symbol): The generated `get` function symbol.
  - `put-fn` (symbol): The generated `put` function symbol.
  - `clear-fn` (symbol): The generated `clear` function symbol.
  - `save-fn` (symbol): The generated `save` function symbol.
  - `load-fn` (symbol): The generated `load` function symbol.
  - `inspect-cache-fn` (symbol): The generated `inspect-cache` function symbol.
  - `inspect-entry-fn` (symbol): The generated `inspect-entry` function symbol.
  - `invalidate-tags-fn` (symbol): The generated `invalidate-tags` func symbol.
  - `cache-var` (symbol): The `defvar` for the main cache hash table.
  - `timestamps-var` (symbol): The `defvar` for the timestamps hash table.
  - `order-ring-or-queue-var` (symbol): The `defvar` for the LRU/FIFO ring.
  - `size-var` (symbol): The `defvar` for the cache's capacity.
  - `frequency-var` (symbol): The `defvar` for the LFU frequency hash table.
  - `version-id-var` (symbol): The `defvar` for the cache's current version.
  - `entry-tags-var` (symbol): The `defvar` for the entry-to-tags map.
  - `tags-idx-var` (symbol): The `defvar` for the tag-to-keys index.
  - `inflight-var` (symbol): The `defvar` for the in-flight requests table."
  sym-prefix
  struct-name-for-entries
  make-fn-constructor-for-entries
  ts-accessor-for-entries
  data-accessor-for-entries
  entry-ver-accessor-for-entries
  all-struct-fields-for-entries
  get-fn
  put-fn
  clear-fn
  save-fn
  load-fn
  inspect-cache-fn
  inspect-entry-fn
  invalidate-tags-fn
  cache-var
  timestamps-var
  order-ring-or-queue-var
  size-var
  frequency-var
  version-id-var
  entry-tags-var
  tags-idx-var
  inflight-var)

(cl-defstruct cacheus-runtime-data
  "Holds the live data structures for a single Cacheus instance.
This struct's slots are initialized with fresh hash tables by default.

  Slots:
  - `cache-ht`: (hash-table) The primary data store (key -> entry-struct).
  - `timestamps-ht`: (hash-table) For eviction and TTL metadata.
  - `frequency-ht`: (hash-table) For LFU frequency counts.
  - `order-data`: (ring/queue) For LRU/FIFO ordering. Initialized at runtime.
  - `entry-tags-ht`: (hash-table) For entry-to-tags mapping.
  - `tags-idx-ht`: (hash-table) For tag-to-keys reverse index.
  - `inflight-ht`: (hash-table) For async operation management."
  (cache-ht (ht-create))
  (timestamps-ht (ht-create))
  (frequency-ht (ht-create))
  order-data ; Ring/Queue, initialized at runtime based on capacity.
  (entry-tags-ht (ht-create))
  (tags-idx-ht (ht-create))
  (inflight-ht (ht-create)))

(cl-defstruct cacheus-instance
  "Defines a complete, generic Cacheus instance.
This is the top-level container that bundles the configuration, symbol
definitions, and live runtime data for a single cache.

  Slots:
  - `options` (`cacheus-options`): The configuration for this instance.
  - `symbols` (`cacheus-symbols`): The generated symbols for this instance.
  - `runtime-data` (`cacheus-runtime-data`): Holds the live cache data."
  options
  symbols
  runtime-data)

(provide 'cacheus-structs)
;;; cacheus-structs.el ends here