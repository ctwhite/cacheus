;;; cacheus.el --- A memoization and caching framework for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: Christian White <christiantwhite@protonmail.com>
;; Version: 1.0.0
;; Keywords: lisp, tools, cache, memoization, performance
;; Package-Requires: ((emacs "26.1")
;;                    (cl-lib "0.5")
;;                    (dash "2.18.0")
;;                    (f "0.20.0")
;;                    (ht "2.3")
;;                    (json "1.6")
;;                    (ring "0.1")
;;                    (s "1.12.0")
;;                    (ts "0.3.0"))
;; URL: https://github.com/ctwhite/cacheus.el

;;; Commentary:
;;
;; Cacheus is a powerful, flexible framework for memoization and general-purpose
;; caching in Emacs Lisp. It is designed to improve performance in
;; performance-sensitive code where repeated, expensive computations can be
;; avoided by caching results, either in memory or on disk.
;;
;; The library provides a rich set of features including time-to-live (TTL)
;; expiration, capacity-based eviction (LRU, LFU, FIFO), file-based
;; persistence, functional versioning, and tag-based invalidation.
;;
;; Core Entry Points:
;;
;; - `cacheus:cache!`: A macro to define a generic, key-value cache with a full
;;   suite of helper functions for direct interaction.
;;
;; - `cacheus:memoize!`: A macro to transparently add caching to an Emacs Lisp
;;   function at compile-time.
;;
;; - `cacheus:memoize-fn`: A function to wrap an *existing* function with
;;   caching logic at runtime, returning a new memoized function.
;;
;; - `cacheus:get!` and `cacheus:put!`: Unified macros for directly getting or
;;   setting values in any cache, whether created by `cacheus:cache!` or
;;   `cacheus:memoize!`.
;;
;; - Global Management: Interactive commands like `cacheus:list-all-caches`
;;   and `cacheus:clear-all-caches` to manage all defined caches at once.
;;
;; This main entry point loads all necessary components of the framework.

;;; Code:

;; Provides the core framework, including all base structs, utilities,
;; and the backend generation engine.
(require 'cacheus-core)

;; Provides the `cacheus:cache!` macro for creating generic caches.
(require 'cacheus-cache)

;; Provides the `cacheus:memoize!`, `cacheus:memoize-fn`, and
;; `cacheus-macro-memoize!` features.
(require 'cacheus-memoize)

;;;###autoload
(defconst cacheus-version "1.0.0"
  "Current version number of the Cacheus framework.")

;;;###autoload
(defun cacheus-version ()
  "Display the current version of the Cacheus framework."
  (interactive)
  (message "Cacheus version %s" cacheus-version))

(provide 'cacheus)
;;; cacheus.el ends here