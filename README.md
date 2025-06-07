# Cacheus

> A robust caching and memoization framework for Emacs Lisp with TTL, persistence, and async support.

**Cacheus** is a powerful, flexible, and feature-rich caching framework designed to bring modern caching capabilities to Emacs Lisp. It provides developers with an easy-to-use API to speed up slow functions, reduce redundant computations, and manage expensive resources like API calls.

Whether you're looking to memoize a computationally intensive function or build a persistent, TTL-aware cache for network requests, Cacheus provides the tools you need in a single, cohesive package.

## Key Features

* **Multiple Caching Strategies**:
  * **Function Memoization**: Transparently cache the results of any function using `cacheus-memoize!`.
  * **Generic Caching**: Manually `get` and `put` key-value pairs using `cacheus-cache!`.
  * **Runtime Memoization**: Wrap existing functions at runtime with `cacheus-memoize-fn`.
* **Time-To-Live (TTL)**: Automatically expire cache entries after a set duration.
* **Capacity Management**: Limit the number of items in a cache with configurable eviction strategies (`:lru`, `:lfu`, `:fifo`).
* **Persistence**: Automatically save caches to a file and load them on startup.
* **Asynchronous Computation**: Prevent UI blocking by running expensive computations in the background on cache misses.
* **Cache Invalidation**:
  * **Versioning**: Invalidate all entries by bumping a `:version` number.
  * **Tagging**: Invalidate groups of entries by tags using `cacheus-invalidate-by-tags!`.
* **Global Management**: A full suite of interactive functions (`cacheus-list-all-caches`, `cacheus-clear-all-caches`, etc.) to inspect and manage all defined caches.

## Installation

The recommended way to install Cacheus is with `use-package`:

```emacs-lisp
(use-package cacheus
  :ensure t)
```

## Core Concepts

Cacheus provides three main primitives for creating caches.

### 1. `cacheus-memoize!` - For New Functions

Use `cacheus-memoize!` to define a *new* function that has caching built-in from the start. It's perfect for your own helper functions that are computationally expensive.

```emacs-lisp
;; Imagine this function takes a long time to run
(defun my-slow-computation (x)
  (message "Computing for %S..." x)
  (sleep-for 2)
  (* x x))

;; Create a memoized version with a 10-minute (600s) TTL
(cacheus-memoize! my-cached-computation (x)
  :ttl 600
  (my-slow-computation x))

;;; Usage
(my-cached-computation 5) ;; => First call: "Computing for 5...", waits 2s, returns 25
(my-cached-computation 5) ;; => Second call: returns 25 instantly
```

### 2. `cacheus-cache!` - For Generic Data

Use `cacheus-cache!` to create a generic key-value cache. This gives you full control to `get` and `put` data manually. It's ideal for caching data from external sources like APIs.

```emacs-lisp
(require 'url-json)

(cacheus-cache! api-cache
  :ttl 3600 ; Cache results for 1 hour
  :capacity 100 ; Store up to 100 results
  :cache-file "~/.emacs.d/cache/api-cache.el") ; Persist to a file

(defun get-user-data (username)
  "Fetch user data from the network or return from cache."
  (or (cacheus-get! 'api-cache username)
      (let* ((url (format "[https://api.github.com/users/%s](https://api.github.com/users/%s)" username))
             (response (url-retrieve-synchronously url))
             (data (json-read-from-string (url-contents response))))
        ;; Put the result in the cache for next time
        (cacheus-put! 'api-cache username data))))
```

### 3. `cacheus-memoize-fn` - For Existing Functions

Use `cacheus-memoize-fn` to add caching to an *existing* function at runtime, without modifying its original definition. This is useful for optimizing functions from other libraries.

```emacs-lisp
;; Suppose `some-library-parse` is a slow function from another package.
(defun some-library-parse (text)
  (sleep-for 1)
  (length text))

;; Create a new, memoized version of it at runtime.
(defvar my-cached-parser
  (cacheus-memoize-fn #'some-library-parse
    :arglist '(text) ;; Must provide the original arglist
    :ttl 1800))

;;; Usage
(funcall my-cached-parser "hello") ;; => First call: waits 1s, returns 5
(funcall my-cached-parser "hello") ;; => Second call: returns 5 instantly
```

## Direct Cache Interaction

While wrapper functions are convenient, you can also interact with any cache directly using the `cacheus-get!`, `cacheus-put!`, and `cacheus-clear!` macros.

### `cacheus-get!`

Retrieve a value by its key. For memoized functions, you pass the arguments that form the key.

```emacs-lisp
;; For a generic cache
(cacheus-get! 'api-cache "tarsius") ;; => Returns the cached data or nil

;; For a memoized function
(cacheus-get! 'my-cached-computation 5) ;; => Returns 25 if cached, or computes and caches it
```

### `cacheus-put!`

Manually insert or update a value in a cache.

```emacs-lisp
;; For a generic cache
(cacheus-put! 'api-cache "new-user" '(:name "New" :id 123))

;; For a memoized function
(cacheus-put! 'my-cached-computation 10 100) ;; Manually sets the cache for input `10` to `100`
```

### `cacheus-clear!`

Clear all entries from a specific cache.

```emacs-lisp
(cacheus-clear! 'api-cache) ;; Removes all entries from the api-cache
(cacheus-clear! 'my-cached-computation) ;; Removes all entries from the memoized function cache
```

## Configuration Options

You can customize cache behavior with a rich set of options.

| Option                  | Description                                                                                             | Example                                    |
| ----------------------- | ------------------------------------------------------------------------------------------------------- | ------------------------------------------ |
| **`:ttl`** | **Time-To-Live (seconds)**. The duration for which a cache entry is considered fresh.                        | `:ttl 3600`                                |
| **`:refresh-ttl`**| If `t`, the TTL countdown for an entry resets on every access (a "sliding" expiration).                   | `:refresh-ttl t`                           |
| **`:capacity`** | The maximum number of items to store in the cache.                                                      | `:capacity 200`                            |
| **`:eviction-strategy`**| Algorithm to use when at capacity. Options: `:lru` (default), `:lfu`, `:fifo`.                          | `:eviction-strategy :lfu`                  |
| **`:cache-file`** | Path to a file for persistence. The cache is automatically loaded on startup and saved on shutdown.       | `:cache-file "~/.emacs.d/cache/my-cache"`  |
| **`:version`** | A value (string, number, etc.) used for bulk invalidation. Change the version to treat all old entries as stale. | `:version "my-app-v1.1"`                   |
| **`:async`** | If `t`, the computation on a cache miss runs asynchronously and returns a promise (`concur` future).     | `:async t`                                 |
| **`:predicate`** | A function `(lambda (value))` that must return non-nil for a result to be cached.                        | `:predicate 'identity` (caches non-nil)    |
| **`:tags-fn`** | A function `(lambda (key value))` that returns a list of symbols to tag the entry with for invalidation. | `:tags-fn (lambda (k v) (list 'users k))` |
| **`:key-fn`** | *(Memoize only)* A function to transform arguments into a canonical cache key.                           | `:key-fn (lambda (a b) (list a))`          |

## Global Management

Cacheus provides interactive commands to manage all your caches globally.

* **`M-x cacheus-list-all-caches`**: Open a buffer listing all registered caches. From here, you can inspect, clear, or save individual caches.
* **`M-x cacheus-clear-all-caches`**: Clear all entries from all caches. With a prefix argument, you can filter by name.
* **`M-x cacheus-save-all-caches`**: Persist all file-backed caches to disk.
* **`M-x cacheus-load-all-caches`**: Load all file-backed caches from disk.

## Contributing

Contributions are welcome! If you find a bug or have a feature request, please open an issue on GitHub. If you'd like to contribute code, please open a pull request.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
