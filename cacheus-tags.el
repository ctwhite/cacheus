;;; cacheus-tags.el --- Core tag management utilities for Cacheus -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file centralizes the core logic for managing tags associated with cache
;; entries. Tags provide a powerful mechanism for invalidating groups of
;; related entries without needing to know their specific keys.
;;
;; This module works in conjunction with `cacheus-eviction.el` and
;; `cacheus-storage.el` to ensure that tag indexes are kept consistent as
;; entries are added, accessed, or removed from the cache.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)

(require 'cacheus-structs)
(require 'cacheus-util)

;; Declare external function to aid byte-compilation.
(declare-function cacheus-evict-one-entry "cacheus-eviction"
                  '(victim-key instance logger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag Management

(defun cacheus-remove-entry-tag-info (ekey instance logger)
  "Remove all tag information for `EKEY` from the instance's tag indexes.
This is a critical cleanup operation called during entry eviction to ensure
the tag system remains consistent. It operates by:
1. Finding all tags for `EKEY` in `entry-tags-ht`.
2. For each tag, removing `EKEY` from the key list in `tags-idx-ht`.
3. Removing `EKEY` itself from `entry-tags-ht`.

Arguments:
- `EKEY`: The effective key of the entry being removed.
- `INSTANCE`: The live `cacheus-instance` to operate on.
- `LOGGER`: A resolved logger function.

Returns:
`nil`."
  (-let-pattern*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name) opts)
       ((&struct :entry-tags-ht et-ht :tags-idx-ht tags-idx-ht) data))
    ;; Only proceed if both tagging hash tables exist.
    (when (and et-ht tags-idx-ht)
      (when-let ((tags (ht-get et-ht ekey)))
        ;; For each tag this entry has...
        (-each tags
               (lambda (tag)
                 ;; ...remove this key from the tag's reverse index.
                 (when-let* ((keys (ht-get tags-idx-ht tag))
                             (updated-keys (-remove ekey keys :test #'equal)))
                   (if (null updated-keys)
                       (ht-remove! tags-idx-ht tag)
                     (ht-set! tags-idx-ht tag updated-keys))))))
        ;; Finally, remove the key -> tags mapping itself.
        (ht-remove! et-ht ekey)
        (funcall logger :debug "[C:%S] Removed tags for %S" name ekey))))

(cl-defun cacheus-invalidate-keys-by-tags
    (runtime-instance tags &key (all-must-match nil) (run-hooks t))
  "Find and evict cache entries based on a list of `TAGS`.
This is the core implementation for tag-based invalidation.

Arguments:
- `RUNTIME-INSTANCE`: The live `cacheus-instance` object.
- `TAGS`: A single tag symbol or a list of symbols to invalidate by.
- `:all-must-match` (boolean): If non-nil, evicts entries that have ALL
  specified tags (AND logic). If nil (default), evicts entries that
  have ANY of the specified tags (OR logic).
- `:run-hooks` (boolean): If non-nil, runs the cache's `:expiration-hook`
  for each entry evicted by this operation.

Returns:
A list of the keys that were invalidated."
  (cl-block cacheus-invalidate-keys-by-tags
    (-let-pattern*
        (((&struct :options opts :runtime-data data) runtime-instance)
         ((&struct :name name :logger logger-opt :expiration-hook hook) opts)
         ((&struct :cache-ht cache-ht :tags-idx-ht tags-idx-ht) data)
         (logger (cacheus-resolve-logger logger-opt))
         (tags-to-match (if (listp tags) tags (list tags)))
         (keys-to-invalidate '()))

      (unless (seq-every-p #'symbolp tags-to-match)
        (error "Tags must be symbols or a list of symbols. Got: %S"
               tags-to-match))
      (when (null tags-to-match)
        (funcall logger :warn "[C:%S] No tags provided for invalidation." name)
        (cl-return-from cacheus-invalidate-keys-by-tags nil))

      ;; Step 1: Determine which keys to invalidate based on matching strategy.
      (if all-must-match
          ;; AND logic: find the intersection of keys for all provided tags.
          (when-let ((initial-keys (ht-get tags-idx-ht (car tags-to-match))))
            (setq keys-to-invalidate
                  (-reduce (lambda (acc tag)
                             (if (null acc)
                                 (cl-return-from cacheus-invalidate-keys-by-tags nil)
                               (cl-intersection acc (ht-get tags-idx-ht tag))))
                           (cdr tags-to-match)
                           initial-keys)))
        ;; OR logic: find the union of keys for all provided tags.
        (dolist (tag tags-to-match)
          (dolist (k (ht-get tags-idx-ht tag))
            (cl-pushnew k keys-to-invalidate :test #'equal))))
      (setq keys-to-invalidate (-distinct keys-to-invalidate :test #'equal))

      ;; Step 2: Evict all the identified keys.
      (if keys-to-invalidate
          (progn
            (funcall logger :info "[C:%S] Invalidating %d entries for tags %S"
                     name (length keys-to-invalidate) tags-to-match)
            (-each keys-to-invalidate
                   (lambda (key)
                     (let ((entry (ht-get cache-ht key)))
                       (cacheus-evict-one-entry key runtime-instance logger)
                       (when (and run-hooks hook entry)
                         (condition-case-unless-debug e (funcall hook key entry)
                           (error (funcall logger :error
                                           "[C:%S] Hook error for key %S: %S"
                                           name key e :trace))))))))
        (funcall logger :info "[C:%S] No entries found for tags %S"
                 name tags-to-match))
      keys-to-invalidate)))

(provide 'cacheus-tags)
;;; cacheus-tags.el ends here