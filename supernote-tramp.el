;;; supernote-tramp.el --- TRAMP integration for Supernote devices -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: files, remote, supernote, tramp, ai-generated
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides TRAMP integration for Supernote devices via WiFi.
;; It allows you to browse the Supernote filesystem through dired using
;; the Browse & Access web interface.
;;
;; Usage:
;;   (require 'supernote-tramp)
;;   (dired "/supernote:192.168.1.100:/")
;;   (dired "/supernote:192.168.1.100#8080:/")  ; custom port
;;
;; The Supernote filesystem is read-only through this interface.

;;; Code:

(require 'tramp)
(require 'json)
(require 'url)
(require 'url-http)
(require 'gnutls)

(defgroup supernote-tramp nil
  "TRAMP integration for Supernote devices."
  :group 'tramp
  :prefix "supernote-tramp-")

(defcustom supernote-tramp-default-port 8089
  "Default port for Supernote Browse & Access web interface."
  :type 'integer
  :group 'supernote-tramp)

(defcustom supernote-tramp-connection-timeout 10
  "Connection timeout in seconds for Supernote HTTP requests."
  :type 'integer
  :group 'supernote-tramp)

(defcustom supernote-tramp-cache-enabled t
  "Whether to enable caching of file listings to reduce HTTP requests.
When enabled, directory listings are cached to avoid repeated HTTP requests
for the same directory within the cache timeout period."
  :type 'boolean
  :group 'supernote-tramp)

(defcustom supernote-tramp-cache-timeout 300
  "Cache timeout in seconds for file listings.
Directory listings older than this will be refetched from the device."
  :type 'integer
  :group 'supernote-tramp)

(defcustom supernote-tramp-cache-max-size 1000
  "Maximum number of directory listings to cache.
When exceeded, oldest entries are removed first."
  :type 'integer
  :group 'supernote-tramp)

(defcustom supernote-tramp-disable-hostname-verification t
  "Whether to disable hostname verification for Supernote HTTP connections."
  :type 'boolean
  :group 'supernote-tramp)

(defconst supernote-tramp-method "supernote"
  "Method to connect to Supernote devices via WiFi.")

;; Forward declaration
(declare-function supernote-tramp-handle-copy-directory "supernote-tramp")
(declare-function supernote-tramp-copy-directory-recursively "supernote-tramp")

(defconst supernote-tramp-file-name-handler-alist
  '((access-file . ignore)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (copy-directory . supernote-tramp-handle-copy-directory)
    (copy-file . supernote-tramp-handle-copy-file)
    (delete-directory . supernote-tramp-handle-delete-directory)
    (delete-file . supernote-tramp-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . supernote-tramp-handle-directory-files)
    (directory-files-and-attributes . supernote-tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-attributes . supernote-tramp-handle-file-attributes)
    (file-directory-p . supernote-tramp-handle-file-directory-p)
    (file-executable-p . supernote-tramp-handle-file-executable-p)
    (file-exists-p . supernote-tramp-handle-file-exists-p)
    (file-local-copy . supernote-tramp-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . supernote-tramp-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . supernote-tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . supernote-tramp-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (insert-directory . supernote-tramp-handle-insert-directory)
    (insert-file-contents . supernote-tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . supernote-tramp-handle-make-directory)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-symbolic-link . supernote-tramp-handle-make-symbolic-link)
    (process-file . ignore)
    (rename-file . supernote-tramp-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . supernote-tramp-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . supernote-tramp-handle-set-file-times)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . supernote-tramp-handle-write-region))
  "Alist of handler functions for Supernote TRAMP method.")

(defvar supernote-tramp-file-cache (make-hash-table :test 'equal)
  "Cache for Supernote file listings to avoid repeated HTTP requests.
Keys are in the format 'HOST:PORT:PATH' and values are plists with:
- :data - the cached directory listing data
- :timestamp - when the data was cached
- :access-time - when the data was last accessed")

(defvar supernote-tramp-cache-access-order '()
  "List of cache keys in order of access for LRU eviction.")

(defun supernote-tramp-get-port (vec)
  "Get the port for Supernote device VEC, using default if not specified."
  (let ((port (tramp-file-name-port vec)))
    (if port
        (if (stringp port)
            (string-to-number port)
          port)
      supernote-tramp-default-port)))

(defun supernote-tramp-cache-key (vec path)
  "Generate a cache key for VEC and PATH."
  (format "%s:%s:%s" 
          (tramp-file-name-host vec)
          (supernote-tramp-get-port vec)
          (or path "/")))

(defun supernote-tramp-cache-is-expired (cache-entry)
  "Check if CACHE-ENTRY is expired based on timestamp."
  (let ((timestamp (plist-get cache-entry :timestamp))
        (now (current-time)))
    (> (time-to-seconds (time-subtract now timestamp))
       supernote-tramp-cache-timeout)))

(defun supernote-tramp-cache-update-access (cache-key)
  "Update the access time and order for CACHE-KEY."
  (let ((cache-entry (gethash cache-key supernote-tramp-file-cache)))
    (when cache-entry
      (plist-put cache-entry :access-time (current-time))
      (puthash cache-key cache-entry supernote-tramp-file-cache)
      ;; Update access order for LRU
      (setq supernote-tramp-cache-access-order
            (cons cache-key 
                  (delete cache-key supernote-tramp-cache-access-order))))))

(defun supernote-tramp-cache-evict-lru ()
  "Evict least recently used cache entries if over max size."
  (while (> (hash-table-count supernote-tramp-file-cache)
            supernote-tramp-cache-max-size)
    (let ((lru-key (car (last supernote-tramp-cache-access-order))))
      (when lru-key
        (remhash lru-key supernote-tramp-file-cache)
        (setq supernote-tramp-cache-access-order
              (delq lru-key supernote-tramp-cache-access-order))))))

(defun supernote-tramp-cache-put (cache-key data)
  "Store DATA in cache with CACHE-KEY."
  (let ((cache-entry (list :data data
                           :timestamp (current-time)
                           :access-time (current-time))))
    (puthash cache-key cache-entry supernote-tramp-file-cache)
    (setq supernote-tramp-cache-access-order
          (cons cache-key 
                (delete cache-key supernote-tramp-cache-access-order)))
    (supernote-tramp-cache-evict-lru)
    data))

(defun supernote-tramp-cache-get (cache-key)
  "Get cached data for CACHE-KEY if valid."
  (let ((cache-entry (gethash cache-key supernote-tramp-file-cache)))
    (when cache-entry
      (if (supernote-tramp-cache-is-expired cache-entry)
          (progn
            (remhash cache-key supernote-tramp-file-cache)
            (setq supernote-tramp-cache-access-order
                  (delq cache-key supernote-tramp-cache-access-order))
            nil)
        (supernote-tramp-cache-update-access cache-key)
        (plist-get cache-entry :data)))))

(defun supernote-tramp-flush-cache (vec)
  "Flush the file cache for Supernote device VEC."
  (let ((host (tramp-file-name-host vec))
        (port (supernote-tramp-get-port vec))
        (prefix (format "%s:%s:" host port)))
    (maphash (lambda (key _value)
               (when (string-prefix-p prefix key)
                 (remhash key supernote-tramp-file-cache)
                 (setq supernote-tramp-cache-access-order
                       (delq key supernote-tramp-cache-access-order))))
             supernote-tramp-file-cache)))

(defun supernote-tramp-flush-cache-path (vec path)
  "Flush cache for specific PATH on device VEC."
  (let ((cache-key (supernote-tramp-cache-key vec path)))
    (remhash cache-key supernote-tramp-file-cache)
    (setq supernote-tramp-cache-access-order
          (delq cache-key supernote-tramp-cache-access-order))))

(defun supernote-tramp-cache-stats ()
  "Return cache statistics as a plist."
  (let ((total-entries (hash-table-count supernote-tramp-file-cache))
        (expired-count 0)
        (oldest-timestamp nil)
        (newest-timestamp nil))
    (maphash (lambda (_key cache-entry)
               (let ((timestamp (plist-get cache-entry :timestamp)))
                 (when (supernote-tramp-cache-is-expired cache-entry)
                   (setq expired-count (1+ expired-count)))
                 (when (or (null oldest-timestamp) (time-less-p timestamp oldest-timestamp))
                   (setq oldest-timestamp timestamp))
                 (when (or (null newest-timestamp) (time-less-p newest-timestamp timestamp))
                   (setq newest-timestamp timestamp))))
             supernote-tramp-file-cache)
    (list :total-entries total-entries
          :expired-entries expired-count
          :valid-entries (- total-entries expired-count)
          :oldest-entry oldest-timestamp
          :newest-entry newest-timestamp
          :max-size supernote-tramp-cache-max-size
          :timeout supernote-tramp-cache-timeout)))

(defun supernote-tramp-json-to-list (obj)
  "Convert JSON object OBJ from vectors to lists recursively."
  (cond
   ((vectorp obj) 
    (mapcar #'supernote-tramp-json-to-list (append obj nil)))
   ((listp obj)
    (mapcar (lambda (item)
              (if (consp item)
                  (cons (car item) (supernote-tramp-json-to-list (cdr item)))
                (supernote-tramp-json-to-list item)))
            obj))
   (t obj)))

(defun supernote-tramp-get-file-list (vec path)
  "Get file listing for PATH on Supernote device VEC."
  (let* ((cache-key (supernote-tramp-cache-key vec path))
         (cached (when supernote-tramp-cache-enabled
                   (supernote-tramp-cache-get cache-key))))
    (if cached
        (progn
          (message "Using cached listing for %s" path)
          cached)
      (let* ((host (tramp-file-name-host vec))
             (port (supernote-tramp-get-port vec))
             (url (format "http://%s:%s%s" host port path))
             (url-request-method "GET")
             (url-request-extra-headers '(("Accept" . "text/html")))
             ;; Temporarily disable hostname verification for this request
             (old-network-security-level network-security-level)
             (old-tls-checktrust (bound-and-true-p tls-checktrust))
             (network-security-level (if supernote-tramp-disable-hostname-verification 'low network-security-level))
             (_tls-checktrust (if supernote-tramp-disable-hostname-verification nil (bound-and-true-p tls-checktrust)))
             (buffer (url-retrieve-synchronously url t nil supernote-tramp-connection-timeout)))
        (unwind-protect
            (if buffer
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t) ; Skip headers
                  (let* ((html (buffer-substring (point) (point-max)))
                         (json-start (string-match "json = '{" html))
                         (json-data (when json-start
                                      (string-match "'{\\(.*\\)}'" html json-start)
                                      (when (match-string 1 html)
                                        (concat "{" (match-string 1 html) "}")))))
                    (kill-buffer buffer)
                    (if json-data
                        (let* ((parsed-raw (json-read-from-string json-data))
                               (parsed (supernote-tramp-json-to-list parsed-raw)))
                          (when supernote-tramp-cache-enabled
                            (supernote-tramp-cache-put cache-key parsed)
                            (message "Cached listing for %s (%d entries)" 
                                    path 
                                    (length (cdr (assoc 'fileList parsed)))))
                          parsed)
                      (error "Failed to parse Supernote response"))))
              (error "Failed to connect to Supernote at %s" url))
          ;; Restore original settings
          (setq network-security-level old-network-security-level)
          (when (boundp 'tls-checktrust)
            (setq tls-checktrust old-tls-checktrust)))))))

(defun supernote-tramp-handle-file-exists-p (filename)
  "Check if FILENAME exists on Supernote."
  (let* ((vec (tramp-dissect-file-name filename))
         (path (tramp-file-name-localname vec))
         ;; Normalize path by removing trailing slash for processing
         (normalized-path (if (and (> (length path) 1) (string-suffix-p "/" path))
                             (substring path 0 -1)
                           path))
         (dir (file-name-directory normalized-path))
         (basename (file-name-nondirectory normalized-path)))
    (condition-case nil
        (cond
         ;; Root directory always exists
         ((or (string= normalized-path "/") (string= normalized-path ""))
          t)
         ;; Handle "." and ".." entries - they always exist
         ((or (string= basename ".") (string= basename ".."))
          t)
         ;; Check if file exists in directory listing
         (t
          (let ((file-list (supernote-tramp-get-file-list vec (or dir "/"))))
            (if (assoc 'fileList file-list)
                (seq-some (lambda (file)
                            (string= (cdr (assoc 'name file)) basename))
                          (cdr (assoc 'fileList file-list)))
              nil))))
      (error nil))))

(defun supernote-tramp-handle-file-directory-p (filename)
  "Check if FILENAME is a directory on Supernote."
  (let* ((vec (tramp-dissect-file-name filename))
         (path (tramp-file-name-localname vec))
         ;; Normalize path by removing trailing slash for processing
         (normalized-path (if (and (> (length path) 1) (string-suffix-p "/" path))
                             (substring path 0 -1)
                           path))
         (dir (file-name-directory normalized-path))
         (basename (file-name-nondirectory normalized-path)))
    (condition-case nil
        (cond
         ;; Root directory is always a directory
         ((or (string= normalized-path "/") (string= normalized-path ""))
          t)
         ;; Handle "." and ".." entries - they are always directories
         ((or (string= basename ".") (string= basename ".."))
          t)
         ;; Check if file exists and is a directory
         (t
          (let ((file-list (supernote-tramp-get-file-list vec (or dir "/"))))
            (if (assoc 'fileList file-list)
                (seq-some (lambda (file)
                            (and (string= (cdr (assoc 'name file)) basename)
                                 (eq (cdr (assoc 'isDirectory file)) t)))
                          (cdr (assoc 'fileList file-list)))
              nil))))
      (error nil))))

(defun supernote-tramp-handle-file-attributes (filename &optional _id-format)
  "Get file attributes for FILENAME on Supernote."
  (let* ((vec (tramp-dissect-file-name filename))
         (path (tramp-file-name-localname vec))
         ;; Normalize path by removing trailing slash for processing
         (normalized-path (if (and (> (length path) 1) (string-suffix-p "/" path))
                             (substring path 0 -1)
                           path))
         (dir (file-name-directory normalized-path))
         (basename (file-name-nondirectory normalized-path)))
    (condition-case nil
        (cond
         ;; Root directory attributes
         ((or (string= normalized-path "/") (string= normalized-path ""))
          (list t              ; 0: file type (directory)
                1              ; 1: link count
                0              ; 2: uid
                0              ; 3: gid
                '(0 0 0 0)     ; 4: access time
                '(0 0 0 0)     ; 5: modification time
                '(0 0 0 0)     ; 6: status change time
                0              ; 7: size in bytes
                "dr-xr-xr-x"   ; 8: file modes
                nil            ; 9: gid changeable
                0              ; 10: inode number
                0))            ; 11: filesystem device number
         ;; Handle "." (current directory)
         ((string= basename ".")
          (list t              ; 0: file type (directory)
                1              ; 1: link count
                0              ; 2: uid
                0              ; 3: gid
                '(0 0 0 0)     ; 4: access time
                '(0 0 0 0)     ; 5: modification time
                '(0 0 0 0)     ; 6: status change time
                0              ; 7: size in bytes
                "dr-xr-xr-x"   ; 8: file modes
                nil            ; 9: gid changeable
                0              ; 10: inode number
                0))            ; 11: filesystem device number
         ;; Handle ".." (parent directory)
         ((string= basename "..")
          (list t              ; 0: file type (directory)
                1              ; 1: link count
                0              ; 2: uid
                0              ; 3: gid
                '(0 0 0 0)     ; 4: access time
                '(0 0 0 0)     ; 5: modification time
                '(0 0 0 0)     ; 6: status change time
                0              ; 7: size in bytes
                "dr-xr-xr-x"   ; 8: file modes
                nil            ; 9: gid changeable
                0              ; 10: inode number
                0))            ; 11: filesystem device number
         ;; Regular file/directory lookup
         (t
          (let ((file-list (supernote-tramp-get-file-list vec (or dir "/"))))
          (if (assoc 'fileList file-list)
              (let ((file-info (seq-find (lambda (file)
                                           (string= (cdr (assoc 'name file)) basename))
                                         (cdr (assoc 'fileList file-list)))))
                (when file-info
                  (let* ((is-dir (eq (cdr (assoc 'isDirectory file-info)) t))
                         (size (or (cdr (assoc 'size file-info)) 0))
                         (date-str (cdr (assoc 'date file-info)))
                         (mtime (if date-str
                                   (date-to-time date-str)
                                 '(0 0 0 0)))
                         (modes (if is-dir #o555 #o444))
                         (modes-string (if is-dir "dr-xr-xr-x" "-r--r--r--")))
                    (list is-dir        ; 0: file type
                          1             ; 1: link count
                          0             ; 2: uid
                          0             ; 3: gid
                          mtime         ; 4: access time
                          mtime         ; 5: modification time
                          mtime         ; 6: status change time
                          size          ; 7: size in bytes
                          modes-string      ; 8: file modes
                          nil           ; 9: gid changeable
                          0             ; 10: inode number
                          0))))         ; 11: filesystem device number
            nil))))
      (error nil))))

(defun supernote-tramp-handle-directory-files (directory &optional full match nosort count)
  "List files in DIRECTORY on Supernote."
  (let* ((vec (tramp-dissect-file-name directory))
         (path (tramp-file-name-localname vec)))
    (condition-case nil
        (let ((file-list (supernote-tramp-get-file-list vec path)))
          (if (assoc 'fileList file-list)
              (let* ((files (mapcar (lambda (file)
                                      (cdr (assoc 'name file)))
                                    (cdr (assoc 'fileList file-list))))
                     ;; Add traditional directory entries
                     (all-files (append (list "." "..") files)))
                (when match
                  (setq all-files (seq-filter (lambda (f) (string-match match f)) all-files)))
                (when full
                  (setq all-files (mapcar (lambda (f)
                                            (cond
                                             ((string= f ".")
                                              (concat directory "."))
                                             ((string= f "..")
                                              (concat directory ".."))
                                             (t
                                              (tramp-make-tramp-file-name
                                               (tramp-file-name-method vec)
                                               (tramp-file-name-user vec)
                                               (tramp-file-name-domain vec)
                                               (tramp-file-name-host vec)
                                               (tramp-file-name-port vec)
                                               (concat path f)))))
                                          all-files)))
                (unless nosort
                  (setq all-files (sort all-files 'string<)))
                (when (and count (natnump count))
                  (setq all-files (seq-take all-files count)))
                all-files)
            nil))
      (error nil))))

(defun supernote-tramp-handle-directory-files-and-attributes (directory &optional full match nosort id-format count)
  "List files and their attributes in DIRECTORY on Supernote."
  (let ((files (supernote-tramp-handle-directory-files directory full match nosort count)))
    (when (listp files)
      (mapcar (lambda (file)
                (let ((full-path (if full file (expand-file-name file directory))))
                  (cons file (supernote-tramp-handle-file-attributes full-path id-format))))
              files))))

(defun supernote-tramp-handle-file-name-all-completions (filename directory)
  "Return completions for FILENAME in DIRECTORY on Supernote."
  (let ((files (supernote-tramp-handle-directory-files directory)))
    (all-completions filename files)))

(defun supernote-tramp-handle-file-local-copy (filename)
  "Create a local copy of FILENAME from Supernote."
  (let* ((vec (tramp-dissect-file-name filename))
         (host (tramp-file-name-host vec))
         (port (supernote-tramp-get-port vec))
         (path (tramp-file-name-localname vec))
         (url (format "http://%s:%s%s" host port path))
         (temp-file (tramp-compat-make-temp-file "supernote-tramp")))
    (with-temp-file temp-file
      (let ((url-request-method "GET"))
        (url-insert-file-contents url)))
    temp-file))

(defun supernote-tramp-handle-insert-file-contents (filename &optional visit beg end replace)
  "Insert contents of FILENAME from Supernote."
  (let ((local-copy (supernote-tramp-handle-file-local-copy filename)))
    (prog1
        (insert-file-contents local-copy visit beg end replace)
      (delete-file local-copy))))

(defun supernote-tramp-handle-insert-directory (filename &optional _switches _wildcard _full-directory-p)
  "Insert directory listing for FILENAME on Supernote."
  (let* ((vec (tramp-dissect-file-name filename))
         (path (tramp-file-name-localname vec))
         (file-list (supernote-tramp-get-file-list vec path)))
    (when (assoc 'fileList file-list)
      ;; Insert "." entry first
      (insert (format "%s %3d %s %s %8d %s %s\n"
                     "dr-xr-xr-x"   ; directory permissions
                     1              ; link count
                     "supernote"    ; owner
                     "supernote"    ; group
                     0              ; size
                     (format-time-string "%b %d %H:%M" (current-time))
                     "."))
      
      ;; Insert ".." entry second
      (insert (format "%s %3d %s %s %8d %s %s\n"
                     "dr-xr-xr-x"   ; directory permissions
                     1              ; link count
                     "supernote"    ; owner
                     "supernote"    ; group
                     0              ; size
                     (format-time-string "%b %d %H:%M" (current-time))
                     ".."))
      
      ;; Insert regular files and directories
      (dolist (file-info (cdr (assoc 'fileList file-list)))
        (let* ((name (cdr (assoc 'name file-info)))
               (is-dir (eq (cdr (assoc 'isDirectory file-info)) t))
               (size (or (cdr (assoc 'size file-info)) 0))
               (date-str (cdr (assoc 'date file-info)))
               (mtime (if date-str (date-to-time date-str) '(0 0 0 0)))
               (modes (if is-dir "dr-xr-xr-x" "-r--r--r--")))
          (insert (format "%s %3d %s %s %8d %s %s\n"
                         modes
                         1              ; link count
                         "supernote"    ; owner
                         "supernote"    ; group
                         size           ; size
                         (format-time-string "%b %d %H:%M" mtime)
                         name)))))))

;; Read-only filesystem handlers

(defun supernote-tramp-handle-file-executable-p (filename)
  "Check if file is executable on Supernote.
For Supernote, directories are executable (for navigation) but files are not."
  (file-directory-p filename))

(defun supernote-tramp-handle-file-writable-p (_filename)
  "Always return nil - Supernote filesystem is read-only."
  nil)

(defun supernote-tramp-handle-file-readable-p (filename)
  "Check if file is readable on Supernote."
  (supernote-tramp-handle-file-exists-p filename))

(defun supernote-tramp-handle-write-region (_start _end filename &optional _append _visit _lockname _confirm)
  "Signal an error - Supernote filesystem is read-only."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Supernote filesystem is read-only"))

(defun supernote-tramp-handle-delete-file (filename &optional _trash)
  "Signal an error - cannot delete files on Supernote."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Cannot delete files - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-delete-directory (directory &optional _recursive _trash)
  "Signal an error - cannot delete directories on Supernote."
  (tramp-error
   (tramp-dissect-file-name directory) 'file-error
   "Cannot delete directories - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-make-directory (dir &optional _parents)
  "Signal an error - cannot create directories on Supernote."
  (tramp-error
   (tramp-dissect-file-name dir) 'file-error
   "Cannot create directories - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-rename-file (filename _newname &optional _ok-if-already-exists)
  "Signal an error - cannot rename files on Supernote."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Cannot rename files - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-make-symbolic-link (filename _linkname &optional _ok-if-already-exists)
  "Signal an error - cannot create symbolic links on Supernote."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Cannot create symbolic links - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-set-file-modes (filename _mode)
  "Signal an error - cannot change file modes on Supernote."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Cannot change file modes - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-set-file-times (filename &optional _time)
  "Signal an error - cannot change file times on Supernote."
  (tramp-error
   (tramp-dissect-file-name filename) 'file-error
   "Cannot change file times - Supernote filesystem is read-only"))

(defun supernote-tramp-handle-copy-file (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid preserve-extended-attributes)
  "Copy file from Supernote to local system only."
  (let ((parsed (tramp-dissect-file-name filename)))
    ;; Check if destination is a TRAMP file name without generating warnings
    (if (tramp-tramp-file-p newname)
        ;; Trying to copy to another remote location
        (tramp-error parsed 'file-error
                     "Cannot copy to remote location - Supernote filesystem is read-only")
      ;; Copy from Supernote to local file
      (let ((local-copy (supernote-tramp-handle-file-local-copy filename)))
        (copy-file local-copy newname ok-if-already-exists keep-date preserve-uid-gid preserve-extended-attributes)
        (delete-file local-copy)))))

(defun supernote-tramp-handle-copy-directory (dirname newname &optional keep-date parents copy-contents)
  "Copy directory from Supernote to local system recursively."
  (let ((parsed (tramp-dissect-file-name dirname)))
    ;; Check if destination is a TRAMP file name without generating warnings
    (if (tramp-tramp-file-p newname)
        ;; Trying to copy to another remote location
        (tramp-error parsed 'file-error
                     "Cannot copy to remote location - Supernote filesystem is read-only")
      ;; Copy from Supernote to local directory
      (supernote-tramp-copy-directory-recursively dirname newname keep-date parents copy-contents))))

(defun supernote-tramp-copy-directory-recursively (dirname newname &optional keep-date parents copy-contents)
  "Recursively copy directory contents from Supernote to local filesystem.
DIRNAME is the source directory on Supernote.
NEWNAME is the destination directory on local filesystem.
KEEP-DATE, PARENTS, and COPY-CONTENTS are options for the copy operation."
  (let* ((vec (tramp-dissect-file-name dirname))
         (path (tramp-file-name-localname vec))
         ;; Normalize path by removing trailing slash
         (normalized-path (if (and (> (length path) 1) (string-suffix-p "/" path))
                             (substring path 0 -1)
                           path))
         (source-base-name (file-name-nondirectory normalized-path))
         (dest-dir (if copy-contents
                      newname
                    (let* ((source-base-name-with-slash (concat "/" source-base-name))
                           (newname-normalized (if (string-suffix-p "/" newname)
                                                  (substring newname 0 -1)
                                                newname)))
                      ;; Check if newname already ends with the source directory name
                      ;; This happens when dired pre-calculates the destination
                      (if (string-suffix-p source-base-name-with-slash newname-normalized)
                          (file-name-as-directory newname)
                        (file-name-as-directory (expand-file-name source-base-name newname)))))))
    
    ;; Create destination directory if it doesn't exist
    (unless (file-exists-p dest-dir)
      (if parents
          (make-directory dest-dir t)
        (make-directory dest-dir)))
    
    ;; Get file list from the source directory
    (let ((file-list (supernote-tramp-get-file-list vec normalized-path)))
      (when (assoc 'fileList file-list)
        (dolist (file (cdr (assoc 'fileList file-list)))
          (let* ((file-name (cdr (assoc 'name file)))
                 (is-directory (eq (cdr (assoc 'isDirectory file)) t))
                 (source-file (tramp-make-tramp-file-name
                              (tramp-file-name-method vec)
                              (tramp-file-name-user vec)
                              (tramp-file-name-domain vec)
                              (tramp-file-name-host vec)
                              (tramp-file-name-port vec)
                              (concat normalized-path "/" file-name)))
                 (dest-file (expand-file-name file-name dest-dir)))
            
            (if is-directory
                ;; Recursively copy subdirectory
                (progn
                  ;; Create the destination directory
                  (unless (file-exists-p dest-file)
                    (make-directory dest-file t))
                  ;; Copy contents into the created directory
                  (supernote-tramp-copy-directory-recursively source-file dest-file keep-date t t))
              ;; Copy regular file
              (let ((local-copy (supernote-tramp-handle-file-local-copy source-file)))
                (copy-file local-copy dest-file nil keep-date nil nil)
                (delete-file local-copy)))))))))

;;;###autoload
(defun supernote-tramp-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is a Supernote TRAMP file name."
  (let ((vec (if (tramp-file-name-p vec-or-filename)
                 vec-or-filename
               (and (stringp vec-or-filename)
                    (tramp-tramp-file-p vec-or-filename)
                    (tramp-dissect-file-name vec-or-filename)))))
    (and vec
         (string= (tramp-file-name-method vec) supernote-tramp-method))))

;;;###autoload
(defun supernote-tramp-setup ()
  "Set up TRAMP method for Supernote devices."
  (add-to-list 'tramp-methods
               `(,supernote-tramp-method
                 (tramp-login-program        "")
                 (tramp-login-args           (()))
                 (tramp-remote-shell         "")
                 (tramp-remote-shell-args    (()))
                 (tramp-connection-timeout   ,supernote-tramp-connection-timeout)
                 (tramp-session-timeout      300)
                 (tramp-default-port         ,supernote-tramp-default-port)))
  
  (tramp-set-completion-function
   supernote-tramp-method
   '((tramp-parse-rhosts "/etc/hosts")))
  
  ;; Register the file name handler for the supernote method at the front
  (push '(supernote-tramp-file-name-p . supernote-tramp-file-name-handler)
        tramp-foreign-file-name-handler-alist)
  
  (tramp-register-file-name-handlers))

;;;###autoload
(defun supernote-tramp-file-name-handler (operation &rest args)
  "TRAMP file name handler for Supernote method."
  (let ((fn (assoc operation supernote-tramp-file-name-handler-alist)))
    (if fn
        (save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###autoload
(defun supernote-tramp-flush-all-cache ()
  "Flush all cached Supernote file listings."
  (interactive)
  (let ((old-count (hash-table-count supernote-tramp-file-cache)))
    (clrhash supernote-tramp-file-cache)
    (setq supernote-tramp-cache-access-order '())
    (message "Supernote file cache cleared (%d entries removed)" old-count)))

;;;###autoload
(defun supernote-tramp-show-cache-stats ()
  "Show cache statistics in the minibuffer."
  (interactive)
  (let ((stats (supernote-tramp-cache-stats)))
    (message "Cache: %d total, %d valid, %d expired (max: %d, timeout: %ds)"
             (plist-get stats :total-entries)
             (plist-get stats :valid-entries)
             (plist-get stats :expired-entries)
             (plist-get stats :max-size)
             (plist-get stats :timeout))))

;;;###autoload
(defun supernote-tramp-toggle-cache ()
  "Toggle cache enabled/disabled."
  (interactive)
  (setq supernote-tramp-cache-enabled (not supernote-tramp-cache-enabled))
  (message "Supernote cache %s" 
           (if supernote-tramp-cache-enabled "enabled" "disabled")))

;;;###autoload
(defun supernote-tramp-clean-expired-cache ()
  "Remove expired cache entries."
  (interactive)
  (let ((removed-count 0)
        (keys-to-remove '()))
    (maphash (lambda (key cache-entry)
               (when (supernote-tramp-cache-is-expired cache-entry)
                 (push key keys-to-remove)))
             supernote-tramp-file-cache)
    (dolist (key keys-to-remove)
      (remhash key supernote-tramp-file-cache)
      (setq supernote-tramp-cache-access-order
            (delq key supernote-tramp-cache-access-order))
      (setq removed-count (1+ removed-count)))
    (message "Removed %d expired cache entries" removed-count)))

;;;###autoload
(defun supernote-tramp-flush-cache-for-path (path)
  "Flush cache for a specific PATH."
  (interactive "sSupernote path to flush: ")
  (let ((removed-count 0))
    (maphash (lambda (key _cache-entry)
               (when (string-suffix-p path key)
                 (remhash key supernote-tramp-file-cache)
                 (setq supernote-tramp-cache-access-order
                       (delq key supernote-tramp-cache-access-order))
                 (setq removed-count (1+ removed-count))))
             supernote-tramp-file-cache)
    (message "Flushed %d cache entries for path: %s" removed-count path)))

(supernote-tramp-setup)

(provide 'supernote-tramp)
;;; supernote-tramp.el ends here
