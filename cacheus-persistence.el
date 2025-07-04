;;; cacheus-persistence.el --- Persistence and loading utilities for Cacheus -*-
;;; lexical-binding: t; -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Constants

(defconst cacheus--file-format-version "cacheus-v1.0"
  "Version string for the Cacheus file format.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-Level Persistence API (Package-Private)

(defun cacheus-persist-cache-instance (instance logger)
  "Persist the state of a Cacheus INSTANCE to its configured cache file.
This function serializes the entire runtime state of a cache, including
its metadata, all entries, and eviction-specific data, into a JSON format
and saves it atomically to a file.

Arguments:
- `INSTANCE` (cacheus-instance): The live cache instance to persist.
- `LOGGER` (function): The resolved logger function for this cache.

Returns:
  (boolean) `t` on success, `nil` on failure."
  (cacheus-let*
      (((&struct :options opts) instance)
       ((&struct :name name :cache-file file) opts))
    (unless file
      (funcall logger :warn "[C:%s] Persist: No :cache-file configured." name)
      (cl-return-from cacheus-persist-cache-instance nil))

    (let ((persist-data (ht-create)))
      (ht-set! persist-data "metadata" (cacheus--serialize-metadata instance))
      (ht-set! persist-data "entries" (cacheus--serialize-entries instance))
      (when-let ((order-data (cacheus-runtime-data-order-data
                              (cacheus-instance-runtime-data instance))))
        (ht-set! persist-data "order"
                 (vec (-map (lambda (k) (cacheus-stringify-key k logger))
                            (ring-elements order-data)))))
      (condition-case err
          (let ((json (json-encode persist-data)))
            (cacheus-save-atomically file json logger)
            (funcall logger :info "[C:%s] Persisted to %s." name file)
            t)
        (error (error "Persistence failed for %s: %S" file err))))))

(defun cacheus-load-cache-instance (instance logger)
  "Load the state of a Cacheus INSTANCE from its configured cache file.
This function reads a previously persisted JSON file, validates its
format and version, and populates the provided live INSTANCE with the
loaded data.

Arguments:
- `INSTANCE` (cacheus-instance): The live (but empty) cache instance.
- `LOGGER` (function): The resolved logger function for this cache.

Returns:
  (boolean) `t` on success, `nil` on failure."
  (cacheus-let*
      (((&struct :options opts) instance)
       ((&struct :name name :cache-file file :version ver) opts)
       (load-result (cacheus-load-and-validate-cache-file
                     file cacheus--file-format-version ver logger)))
    (unless (plist-get load-result :valid)
      (funcall logger :warn "[C:%s] Load failed validation: %s. Starting fresh."
               name (plist-get load-result :reason))
      (cl-return-from cacheus-load-cache-instance nil))

    (condition-case err
        (-let* (((&plist :raw-data raw) load-result))
          (cacheus--populate-instance-from-data instance raw logger)
          (funcall logger :info "[C:%s] Loaded %d entries from %s."
                   name (hash-table-count
                         (cacheus-runtime-data-cache-ht
                          (cacheus-instance-runtime-data instance)))
                   file)
          t)
      (error
       (funcall logger :error "[C:%s] Error populating from %s: %S" name file err)
       (funcall logger :warn "[C:%s] Cache starts fresh due to load error." name)
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level Helpers (Package-Private & File-Local)

(defun cacheus--serialize-metadata (instance)
  "Helper to serialize cache metadata to an alist."
  (cacheus-let*
      (((&struct :options opts :symbols syms) instance)
       ((&struct :version ver :eviction-strategy evic :capacity cap
                 :ttl ttl :refresh-ttl refresh) opts)
       ((&struct :version-id-var ver-var :struct-name-for-entries s-name) syms))
    `(("fileFormatVersion" . ,cacheus--file-format-version)
      ("persistedAt" . ,(ts-format "%Y-%m-%dT%H:%M:%S" (ts-now)))
      ("cacheFunctionalVersion" . ,(or (and (boundp ver-var) (symbol-value ver-var))
                                       ver "N/A"))
      ("evictionStrategy" . ,(symbol-name evic))
      ("capacity" . ,(or cap "N/A"))
      ("ttl" . ,(or ttl "N/A"))
      ("refreshTtlOnAccess" . ,(or refresh "N/A"))
      ("structName" . ,(symbol-name s-name)))))

(defun cacheus--serialize-entries (instance)
  "Helper to serialize cache entries to an alist."
  (cacheus-let*
      (((&struct :symbols syms :runtime-data rtd) instance)
       ((&struct :cache-ht cache-ht :frequency-ht freq-ht) rtd)
       ((&struct :struct-name-for-entries s-name
                 :all-struct-fields-for-entries fields) syms)
       (logger (cacheus-resolve-logger
                (cacheus-options-logger (cacheus-instance-options instance))))
       (entries-alist nil))
    (maphash (lambda (key entry)
               (when-let ((skey (cacheus-stringify-key key logger)))
                 (let ((details nil))
                   (-each fields
                          (lambda (spec)
                            (let* ((fname (car spec))
                                   (acc (intern (format "%s-%s" s-name fname)))
                                   (val (funcall acc entry)))
                              (push (cons (symbol-name fname)
                                          (if (ts-p val) (ts-to-iso8601 val) val))
                                    details))))
                   (when (and freq-ht (gethash key freq-ht))
                     (push (cons "lfuFrequency" (gethash key freq-ht)) details))
                   (push (cons skey details) entries-alist))))
             cache-ht)
    entries-alist))

(defun cacheus--populate-instance-from-data (instance raw-data logger)
  "Helper to populate a cache instance from parsed JSON data."
  (cacheus-let*
      (((&struct :options opts :symbols syms :runtime-data rtd) instance)
       ((&struct :capacity cap :eviction-strategy evic) opts)
       ((&struct :struct-name-for-entries s-name
                 :make-fn-constructor-for-entries ctor
                 :all-struct-fields-for-entries fields) syms)
       ((&struct :cache-ht cache-ht :timestamps-ht ts-ht :order-data order
                 :frequency-ht freq-ht) rtd)
       (meta (cdr (assoc "metadata" raw-data)))
       (p-struct-str (cdr (assoc "structName" meta)))
       (p-struct (and p-struct-str (intern-soft p-struct-str)))
       (entries-alist (cdr (assoc "entries" raw-data)))
       (s-order-list (cdr (assoc "order" raw-data))))
    (unless (eq p-struct s-name)
      (error "Struct mismatch. Expected %s, got %s" s-name p-struct))

    (cacheus-clear-runtime-data instance)

    (-each entries-alist
           (lambda (pair)
             (-let* (((skey . details) pair)
                     (key (cacheus-parse-key skey logger)))
               (when key
                 (let* ((args (-mapcat
                               (lambda (f)
                                 (let* ((fsym (car f))
                                        (fstr (symbol-name fsym))
                                        (val (cdr (assoc fstr details))))
                                   (list (intern (concat ":" fstr))
                                         (if (memq fsym '(timestamp last-accessed))
                                             (and val (ts-parse-iso8601 val))
                                           val))))
                               fields))
                        (entry (apply ctor args)))
                   (puthash key entry cache-ht)
                   (when-let ((ts-str (cdr (assoc "timestamp" details))))
                     (when (and ts-ht (stringp ts-str))
                       (when-let ((ts (ignore-errors (ts-parse-iso8601 ts-str))))
                         (puthash key ts ts-ht))))
                   (when (and (eq evic :lfu) freq-ht
                              (cdr (assoc "lfuFrequency" details)))
                     (puthash key (cdr (assoc "lfuFrequency" details)) freq-ht)))))))

    (when (and order s-order-list cap (> cap 0))
      (let ((count 0))
        (-each s-order-list
               (lambda (skey)
                 (when (< count cap)
                   (when-let ((key (cacheus-parse-key skey logger)))
                     (when (gethash key cache-ht)
                       (ring-insert order key)
                       (cl-incf count))))))))))

(defun cacheus-save-atomically (file-path content logger)
  "Atomically save string CONTENT to FILE-PATH using a temporary file."
  (condition-case err
      (let* ((dir (file-name-directory file-path))
             (temp-file (make-temp-file "cacheus-tmp-" t nil content)))
        (make-directory dir t)
        (rename-file temp-file file-path t))
    (error
     (funcall logger :error "atomic-save: Failed to write to %s: %S" file-path err)
     (signal 'cacheus-error (list "Atomic save failed" err)))))

(defun cacheus-load-and-validate-cache-file (file fmt-ver func-ver logger)
  "Load JSON data from FILE and perform basic validation."
  (unless (file-readable-p file)
    (funcall logger :debug "load: Cache file %s does not exist." file)
    (cl-return-from cacheus-load-and-validate-cache-file
      (list :valid nil :reason "File does not exist")))
  (condition-case err
      (let* ((json-str (with-temp-buffer (insert-file-contents file)
                                         (buffer-string)))
             (raw (json-read-from-string json-str))
             (meta (cdr (assoc "metadata" raw)))
             reason)
        (unless meta (setq reason "Missing metadata block in cache file."))
        (when-let ((ver (cdr (assoc "fileFormatVersion" meta))))
          (unless (equal ver fmt-ver)
            (setq reason (format "File format mismatch. Expected %s, got %s"
                                 fmt-ver ver))))
        (when func-ver
          (when-let ((ver (cdr (assoc "cacheFunctionalVersion" meta))))
            (unless (equal ver func-ver)
              (setq reason (format "Functional version mismatch. Expected %s, got %s"
                                   func-ver ver)))))
        (if reason
            (progn (funcall logger :warn "load: Invalid cache file %s: %s"
                            file reason)
                   (list :valid nil :reason reason :raw-data raw))
          (list :valid t :reason "OK" :raw-data raw)))
    (error
     (funcall logger :error "load: Error reading/parsing %s: %S" file err)
     (list :valid nil :reason (format "Error reading file: %S" err)))))

(provide 'cacheus-persistence)
;;; cacheus-persistence.el ends here