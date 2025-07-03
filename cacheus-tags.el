;;; cacheus-tags.el --- Core tag management utilities for Cacheus -*-
;;; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file centralizes the core logic for managing tags associated with cache
;; entries. Tags provide a powerful mechanism for invalidating groups of
;; related entries without needing to know their specific keys.
;;
;; This module works in conjunction with `cacheus-eviction.el` and
;; `cacheus-storage.el` to ensure that tag indexes are kept consistent as
;; entries are added or removed from the cache.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)

(require 'cacheus-structs)
(require 'cacheus-util)

(declare-function cacheus-evict-one-entry "cacheus-eviction")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag Management (Package-Private)

(defun cacheus-update-entry-tag-info (ekey tags instance logger)
  "Update all tag indexes for EKEY with TAGS.
This function maintains two data structures:
1. `entry-tags-ht`: Maps an entry key to its list of tags.
2. `tags-idx-ht`: An inverted index mapping a tag to a list of
   keys that have that tag.

Arguments:
- `EKEY` (any): The effective key of the entry being tagged.
- `TAGS` (list): A list of tag symbols.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): The resolved logger function.

Returns:
  None. Modifies the instance's runtime data in-place."
  (let* ((data (cacheus-instance-runtime-data instance))
         (et-ht (cacheus-runtime-data-entry-tags-ht data))
         (tags-idx-ht (cacheus-runtime-data-tags-idx-ht data)))
    (when (and tags et-ht tags-idx-ht)
      (let ((unique-tags (-distinct tags)))
        (when unique-tags
          (puthash ekey unique-tags et-ht)
          (-each unique-tags
                 (lambda (tag)
                   (let ((keys (gethash tag tags-idx-ht)))
                     (pushnew ekey keys)
                     (puthash tag keys tags-idx-ht)))))))))

(defun cacheus-remove-entry-tag-info (ekey instance logger)
  "Remove all tag information for EKEY from the instance's tag indexes.
This is called during eviction to ensure the tag indexes remain consistent.

Arguments:
- `EKEY` (any): The effective key of the entry being evicted.
- `INSTANCE` (cacheus-instance): The live cache instance.
- `LOGGER` (function): The resolved logger function.

Returns:
  None. Modifies the instance's runtime data in-place."
  (let* ((name (cacheus-options-name (cacheus-instance-options instance)))
         (data (cacheus-instance-runtime-data instance))
         (et-ht (cacheus-runtime-data-entry-tags-ht data))
         (tags-idx-ht (cacheus-runtime-data-tags-idx-ht data)))
    (when (and et-ht tags-idx-ht)
      (when-let ((tags (gethash ekey et-ht)))
        (-each tags
               (lambda (tag)
                 (when-let* ((keys (gethash tag tags-idx-ht))
                             (updated (remove ekey keys)))
                   (if (null updated)
                       (remhash tag tags-idx-ht)
                     (puthash tag updated tags-idx-ht))))))
        (remhash ekey et-ht)
        (funcall logger :debug "[C:%s] Removed tags for %S" name ekey)))))

(defun cacheus-get-keys-by-tags (tags-idx-ht tags all-must-match)
  "Return a list of keys matching TAGS based on matching strategy."
  (if all-must-match
      ;; Intersect the key lists for all specified tags.
      (when-let ((initial-keys (gethash (car tags) tags-idx-ht)))
        (-reduce 'intersection (cdr tags)
                 :initial-value initial-keys
                 :key (lambda (tag) (gethash tag tags-idx-ht))))
    ;; Union the key lists for all specified tags.
    (let ((keys-ht (make-hash-table :test 'equal)))
      (dolist (tag tags)
        (dolist (k (gethash tag tags-idx-ht))
          (puthash k t keys-ht)))
      (hash-table-keys keys-ht))))

(cl-defun cacheus-invalidate-keys-by-tags
    (instance tags &key (all-must-match nil) (run-hooks t))
  "Find and evict cache entries based on a list of TAGS.

Arguments:
- `INSTANCE` (cacheus-instance): The live cache instance.
- `TAGS` (list): A list of tag symbols to match.
- `ALL-MUST-MATCH` (boolean): If non-nil, an entry must have all
  tags to be invalidated. If nil, any entry with at least one
  of the tags will be invalidated.
- `RUN-HOOKS` (boolean): If non-nil, run the `:expiration-hook` for
  each invalidated entry.

Returns:
  (list) A list of keys that were invalidated."
  (let* ((opts (cacheus-instance-options instance))
         (data (cacheus-instance-runtime-data instance))
         (name (cacheus-options-name opts))
         (logger (cacheus-resolve-logger (cacheus-options-logger opts)))
         (hook (cacheus-options-expiration-hook opts))
         (cache-ht (cacheus-runtime-data-cache-ht data))
         (tags-idx-ht (cacheus-runtime-data-tags-idx-ht data))
         (tags-to-match (if (listp tags) tags (list tags)))
         keys)
    (unless (seq-every-p #'symbolp tags-to-match)
      (error "Tags must be symbols or a list of symbols. Got: %S" tags))
    (setq keys (cacheus-get-keys-by-tags tags-idx-ht tags-to-match all-must-match))
    (if keys
        (progn
          (funcall logger :info "[C:%s] Invalidating %d entries for tags %S"
                   name (length keys) tags-to-match)
          (-each keys
                 (lambda (key)
                   (let ((entry (gethash key cache-ht)))
                     (cacheus-evict-one-entry key instance logger)
                     (when (and run-hooks hook entry)
                       (condition-case e (funcall hook key entry)
                         (error (funcall logger :error
                                         "[C:%s] Hook error for key %S: %S"
                                         name key e))))))))
      (funcall logger :info "[C:%s] No entries found for tags %S"
               name tags-to-match))
    keys))

(provide 'cacheus-tags)
;;; cacheus-tags.el ends here