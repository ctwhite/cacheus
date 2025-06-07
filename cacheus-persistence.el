;;; cacheus-persistence.el --- Core persistence and loading utilities for Cacheus -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides the fundamental mechanisms for persisting Cacheus
;; instances to disk and loading them back into memory. It ensures data
;; integrity through atomic save operations and validates loaded cache files
;; against format and functional versions.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'json)
(require 'ts)
(require 'f)
(require 's)
(require 'dash)
(require 'ring)

(require 'cacheus-structs)
(require 'cacheus-util)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Constants

(defconst cacheus-file-format-version "cacheus-v1.0"
  "Version string for the Cacheus file format.
This ensures that breaking changes to the serialization format do not cause
errors with older cache files.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-Level Persistence API

(defun cacheus-persist-cache-instance (instance logger)
  "Persist the state of a Cacheus `INSTANCE` to its configured cache file.
This function serializes the cache's runtime data (entries, ordering,
metadata) into a JSON string and saves it atomically.

Arguments:
- `INSTANCE`: The live `cacheus-instance` to persist.
- `LOGGER`: The resolved logger function.

Returns:
`t` on successful persistence, `nil` if skipped (e.g., no cache file is
configured), or signals `cacheus-error` on failure."
  (-let-pattern*
      (((&struct :options opts :symbols syms :runtime-data data) instance)
       ((&struct :name name :cache-file file :version ver
                 :eviction-strategy evic-strat :capacity cap
                 :ttl ttl :refresh-ttl refresh-ttl) opts)
       ((&struct :version-id-var ver-var :size-var size-var
                 :struct-name-for-entries s-name
                 :all-struct-fields-for-entries all-fields) syms)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht) data))
    (cl-block cacheus-persist-cache-instance
      (unless file
        (funcall logger :warn "[C:%S] Persist: No :cache-file. Skipping." name)
        (cl-return-from cacheus-persist-cache-instance nil))

      (let ((data-to-persist (ht-create)))
        ;; 1. Populate Metadata block.
        (ht-set! data-to-persist 'metadata
                 (ht ('fileFormatVersion cacheus-file-format-version)
                     ('persistedAt (ts-to-iso8601 (ts-now)))
                     ('cacheFunctionalVersion
                      (or (and (boundp ver-var) (symbol-value ver-var)) ver "N/A"))
                     ('evictionStrategy (symbol-name evic-strat))
                     ('capacity (or (and (boundp size-var) (symbol-value size-var))
                                   cap "N/A"))
                     ('ttl (or ttl "N/A"))
                     ('refreshTtlOnAccess (or refresh-ttl "N/A"))
                     ('structName (symbol-name s-name))))

        ;; 2. Populate Cache Entries.
        (let ((entries-json-obj (ht-create)))
          (ht-map cache-ht
                  (lambda (key entry-struct)
                    (when-let ((skey (cacheus-stringify-key key logger)))
                      (let ((entry-detail (ht-create)))
                        ;; Serialize all fields of the entry struct.
                        (-each all-fields
                               (lambda (field-spec)
                                 (let* ((f-sym (car field-spec))
                                        (acc-fn (intern (format "%s-%s" s-name f-sym)))
                                        (val (funcall acc-fn entry-struct)))
                                   (ht-set! entry-detail (symbol-name f-sym)
                                            (if (ts-p val) (ts-to-iso8601 val) val)))))
                        (when (and (eq evic-strat :lfu) freq-ht (ht-get freq-ht key))
                          (ht-set! entry-detail "lfuFrequency" (ht-get freq-ht key)))
                        (ht-set! entries-json-obj skey entry-detail)))))
          (ht-set! data-to-persist 'entries entries-json-obj))

        ;; 3. Populate Order Data (for LRU/FIFO).
        (when order
          (let ((order-keys (-filter #'identity
                                     (-map #'cacheus-stringify-key
                                           (ring-elements order)))))
            (ht-set! data-to-persist 'order (vec order-keys))))

        ;; 4. Encode to JSON and Save Atomically.
        (condition-case-unless-debug err
            (let ((json-string (json-encode data-to-persist '(:ht-style . alist))))
              (cacheus-save-atomically file json-string logger)
              (funcall logger :info "[C:%S] Persisted to %s." name file)
              t)
          (error (error "Persistence failed for %s: %S" file err)))))))

(defun cacheus-load-cache-instance (instance logger)
  "Load the state of a Cacheus `INSTANCE` from its configured cache file.
This function validates the cache file, clears the current runtime data,
and then populates the instance by deserializing the JSON content.

Arguments:
- `INSTANCE`: The live `cacheus-instance` to load data into.
- `LOGGER`: The resolved logger function.

Returns:
`t` on successful loading, or `nil` if validation fails or an error occurs."
  (-let-pattern*
      (((&struct :options opts :symbols syms :runtime-data data) instance)
       ((&struct :name name :cache-file file :version ver :capacity cap
                 :eviction-strategy evic-strat) opts)
       ((&struct :struct-name-for-entries s-name
                 :make-fn-constructor-for-entries ctor
                 :all-struct-fields-for-entries all-fields) syms)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht) data)
       (load-result (cacheus-load-and-validate-cache-file
                     file cacheus-file-format-version ver logger)))

    (cl-block cacheus-load-cache-instance
      (unless (plist-get load-result :valid)
        (funcall logger :warn "[C:%S] Load failed validation: %s. Starting fresh."
                 name (plist-get load-result :reason))
        (cl-return-from cacheus-load-cache-instance nil))

      (condition-case-unless-debug err
          (-let* (((&plist :raw-data raw-data) load-result)
                  (metadata (cdr (assoc 'metadata raw-data)))
                  (p-struct-str (cdr (assoc 'structName metadata)))
                  (p-struct (when p-struct-str (intern-soft p-struct-str)))
                  (entries-alist (cdr (assoc 'entries raw-data)))
                  (s-order-vec (cdr (assoc 'order raw-data))))
            (unless (eq p-struct s-name)
              (error "Struct name mismatch. Expected %S, got %S in file."
                     s-name (or p-struct "N/A")))
            (cacheus-clear-runtime-data instance)

            (-each entries-alist
                   (lambda (entry-pair)
                     (-let* (((skey . details) entry-pair)
                             (key (cacheus-parse-key skey logger)))
                       (when key
                         (let* ((struct-args
                                 (-mapcat
                                  (lambda (f)
                                    (let* ((f-sym (car f)) (f-str (symbol-name f-sym))
                                           (val (cdr (assoc f-str details))))
                                      (list (intern (concat ":" f-str))
                                            (if (memq f-sym '(timestamp last-accessed))
                                                (and (stringp val)
                                                     (ignore-errors (ts-parse-iso8601 val)))
                                              val))))
                                  all-fields))
                                (new-entry (apply ctor struct-args)))
                           (ht-set! cache-ht key new-entry)
                           (when ts-ht
                             (when-let ((ts (funcall (intern (format "%s-timestamp" p-struct-str))
                                                     new-entry)))
                               (when (ts-p ts) (ht-set! ts-ht key ts))))
                           (when (and (eq evic-strat :lfu) freq-ht)
                             (when-let ((freq (cdr (assoc "lfuFrequency" details))))
                               (ht-set! freq-ht key freq))))))))

            (when (and order s-order-vec cap (> cap 0))
              (let ((loaded-count 0))
                (-each (if (vectorp s-order-vec) (cl-coerce s-order-vec 'list)
                         s-order-vec)
                       (lambda (skey)
                         (when (< loaded-count cap)
                           (when-let ((key (cacheus-parse-key skey logger)))
                             (when (ht-get cache-ht key)
                               (ring-insert order key)
                               (cl-incf loaded-count))))))))
            (funcall logger :info "[C:%S] Loaded %d entries from %s."
                     name (ht-size cache-ht) file)
            t)
        (error
         (funcall logger :error "load: Error populating from %s: %S"
                  name file err :trace)
         (funcall logger :warn "[C:%S] Cache starts fresh due to load error."
                  name)
         nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Helpers

(defun cacheus-save-atomically (file-path content logger)
  "Atomically save string `CONTENT` to `FILE-PATH`.
This uses `f-atomic-write-to-file` to prevent data corruption if Emacs
crashes or is killed during the save operation.

Arguments:
- `FILE-PATH`: The absolute path to the destination file.
- `CONTENT`: The string content to save.
- `LOGGER`: A resolved logger function.

Returns:
`t` on success, or signals `cacheus-error` on failure."
  (condition-case-unless-debug err
      (f-atomic-write-to-file file-path content)
    (error
     (funcall logger :error "atomic-save: Failed to write to %s: %S"
              file-path err)
     (signal 'cacheus-error (list "Atomic save failed" err)))))

(defun cacheus-load-and-validate-cache-file (file-path expected-format-version
                                             current-func-version logger)
  "Load JSON data from `FILE-PATH` and perform basic validation.
Validates file existence, JSON parsing, format version, and the cache's
functional version against current values.

Arguments:
- `FILE-PATH`: The path to the cache file.
- `EXPECTED-FORMAT-VERSION`: The expected file format version string.
- `CURRENT-FUNC-VERSION`: The current functional version of the cache.
- `LOGGER`: A resolved logger function.

Returns:
A plist of the form `(:valid t/nil :reason string :raw-data alist)`."
  (cl-block cacheus-load-and-validate-cache-file
    (unless (file-exists-p file-path)
      (funcall logger :warn "load: Cache file %s does not exist." file-path)
      (cl-return-from cacheus-load-and-validate-cache-file
        (list :valid nil :reason "File does not exist")))

    (condition-case-unless-debug err
        (let* ((json-string (with-temp-buffer
                              (insert-file-contents file-path)
                              (buffer-string)))
               (raw-data (json-read-from-string json-string))
               (metadata (cdr (assoc 'metadata raw-data)))
               (reason nil))
          ;; --- Perform Validations ---
          (unless metadata
            (setq reason "Missing metadata block in cache file."))
          (when-let ((file-ver (cdr (assoc 'fileFormatVersion metadata))))
            (unless (equal file-ver expected-format-version)
              (setq reason
                    (format "File format mismatch. Expected %S, got %S"
                            expected-format-version file-ver))))
          (when current-func-version
            (when-let ((persisted-ver (cdr (assoc 'cacheFunctionalVersion
                                                  metadata))))
              (unless (equal persisted-ver current-func-version)
                (setq reason
                      (format "Functional version mismatch. Expected %S, got %S"
                              current-func-version persisted-ver)))))
          ;; --- Return Result ---
          (if reason
              (progn
                (funcall logger :warn "load: Invalid cache file %s: %s"
                         file-path reason)
                (list :valid nil :reason reason :raw-data raw-data))
            (progn
              (funcall logger :debug "load: Cache file %s validated." file-path)
              (list :valid t :reason "OK" :raw-data raw-data))))
      (error
       (funcall logger :error "load: Error reading/parsing %s: %S"
                file-path err)
       (list :valid nil :reason (format "Error reading file: %S" err))))))

(provide 'cacheus-persistence)
;;; cacheus-persistence.el ends here