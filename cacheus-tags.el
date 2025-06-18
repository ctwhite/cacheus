;;; cacheus-tags.el --- Core tag management utilities for Cacheus -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file centralizes the core logic for managing tags associated with cache
;; entries. Tags provide a powerful mechanism for invalidating groups of
;; related entries without needing to know their specific keys.
;;
;; This module works in conjunction with `cacheus-eviction.el` and
;; `cacheus-storage.el` to ensure that tag indexes are kept consistent as
;; entries are added or removed from the cache.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)

(require 'cacheus-structs)
(require 'cacheus-util)

;; This function is defined in `cacheus-eviction.el`, which is loaded before
;; this file. This forward declaration prevents a byte-compiler warning.
(declare-function cacheus-evict-one-entry "cacheus-eviction" (key instance logger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag Management (Package-Private)

(defun cacheus-update-entry-tag-info (ekey tags instance logger)
  "Update all tag indexes for EKEY with TAGS.

This function maintains two data structures:
1. `entry-tags-ht`: Maps an entry key to its list of tags.
2. `tags-idx-ht`: An inverted index mapping a tag to a list of
   keys that have that tag.

Arguments:
  EKEY (any): The effective key of the entry being tagged.
  TAGS (list): A list of tag symbols.
  INSTANCE (cacheus-instance): The live cache instance.
  LOGGER (function): The resolved logger function.

Returns: None."
  (cacheus-let*
      (((&struct :runtime-data data) instance)
       ((&struct :entry-tags-ht et-ht :tags-idx-ht tags-idx-ht) data))
    (when (and tags et-ht tags-idx-ht)
      (let ((unique-tags (-distinct tags :test #'equal)))
        (when unique-tags
          (ht-set! et-ht ekey unique-tags)
          (-each unique-tags
                 (lambda (tag)
                   ;; Get the list of keys for this tag, add the new key,
                   ;; and set it back.
                   (let ((keys (ht-get tags-idx-ht tag)))
                     (cl-pushnew ekey keys :test #'equal)
                     (ht-set! tags-idx-ht tag keys)))))))))

(defun cacheus-remove-entry-tag-info (ekey instance logger)
  "Remove all tag information for EKEY from the instance's tag indexes.

This is called during eviction to ensure the tag indexes remain
consistent.

Arguments:
  EKEY (any): The effective key of the entry being evicted.
  INSTANCE (cacheus-instance): The live cache instance.
  LOGGER (function): The resolved logger function.

Returns: None."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name) opts)
       ((&struct :entry-tags-ht et-ht :tags-idx-ht tags-idx-ht) data))
    (when (and et-ht tags-idx-ht)
      (when-let ((tags (ht-get et-ht ekey)))
        ;; For each tag the entry had, remove the entry's key
        ;; from the inverted index.
        (-each tags
               (lambda (tag)
                 (when-let* ((keys (ht-get tags-idx-ht tag))
                             (updated (-remove ekey keys :test #'equal)))
                   ;; If no more keys have this tag, remove the tag itself.
                   (if (null updated)
                       (ht-remove! tags-idx-ht tag)
                     (ht-set! tags-idx-ht tag updated))))))
        ;; Finally, remove the key -> tags mapping.
        (ht-remove! et-ht ekey)
        (funcall logger :debug "[C:%s] Removed tags for %S" name ekey))))

(defun cacheus-get-keys-by-tags (tags-idx-ht tags all-must-match)
  "Return a list of keys matching TAGS based on matching strategy."
  (if all-must-match
      ;; Intersect the key lists for all specified tags.
      (when-let ((initial-keys (ht-get tags-idx-ht (car tags))))
        (-reduce (lambda (acc tag)
                   (if (null acc)
                       acc
                     (cl-intersection acc (ht-get tags-idx-ht tag))))
                 (cdr tags)
                 initial-keys))
    ;; Union the key lists for all specified tags.
    (let ((keys-ht (ht-create)))
      (dolist (tag tags)
        (dolist (k (ht-get tags-idx-ht tag))
          (ht-set! keys-ht k t)))
      (ht-keys keys-ht))))

(cl-defun cacheus-invalidate-keys-by-tags
    (instance tags &key (all-must-match nil) (run-hooks t))
  "Find and evict cache entries based on a list of TAGS.

Arguments:
  INSTANCE (cacheus-instance): The live cache instance.
  TAGS (list): A list of tag symbols to match.
  ALL-MUST-MATCH (boolean): If non-nil, an entry must have all
    tags to be invalidated. If nil, any entry with at least one
    of the tags will be invalidated.
  RUN-HOOKS (boolean): If non-nil, run the `:expiration-hook` for
    each invalidated entry.

Returns:
  (list) A list of keys that were invalidated."
  (cacheus-let*
      (((&struct :options opts :runtime-data data) instance)
       ((&struct :name name :logger lopt :cache-expiration-hook hook) opts)
       ((&struct :cache-ht cache-ht :tags-idx-ht tags-idx-ht) data)
       (logger (cacheus-resolve-logger lopt))
       (tags-to-match (if (listp tags) tags (list tags)))
       (keys nil))
    (unless (seq-every-p #'symbolp tags-to-match)
      (error "Tags must be symbols or a list of symbols. Got: %S" tags))
    (when (null tags-to-match)
      (funcall logger :warn "[C:%s] No tags provided for invalidation." name)
      (cl-return-from cacheus-invalidate-keys-by-tags nil))

    (setq keys (cacheus-get-keys-by-tags tags-idx-ht tags-to-match all-must-match))

    (if keys
        (progn
          (funcall logger :info "[C:%s] Invalidating %d entries for tags %S"
                   name (length keys) tags-to-match)
          (-each keys
                 (lambda (key)
                   (let ((entry (ht-get cache-ht key)))
                     (cacheus-evict-one-entry key instance logger)
                     (when (and run-hooks hook entry)
                       (condition-case-unless-debug e
                           (funcall hook key entry)
                         (error
                          (funcall logger :error "[C:%s] Hook error for key %S: %S"
                                   name key e :trace))))))))
      (funcall logger :info "[C:%s] No entries found for tags %S" name tags-to-match))
    keys))

(provide 'cacheus-tags)
;;; cacheus-tags.el ends here