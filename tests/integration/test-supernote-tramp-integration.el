;;; test-supernote-tramp-integration.el --- Integration tests for supernote-tramp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Load test configuration
(load-file "../supernote-tramp-test-config.el")


;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: files, remote, supernote, tramp, test, integration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Integration tests for the supernote-tramp package using a real Supernote server.
;; These tests require a running Supernote server at http://192.168.1.100:8089
;;
;; Run tests with: M-x ert RET test-supernote-tramp-integration RET

;;; Code:

(require 'ert)
(require 'supernote-tramp)
(require 'dired)

(defconst test-supernote-tramp-integration-host "192.168.1.100"
  "Host for integration testing.")

(defconst test-supernote-tramp-integration-port 8089
  "Port for integration testing.")

(defconst test-supernote-tramp-integration-url
  (format "/supernote:%s#%d:/"
          test-supernote-tramp-integration-host
          test-supernote-tramp-integration-port)
  "Base URL for integration testing.")

(defun test-supernote-tramp-integration-server-available-p ()
  "Check if the test server is available."
  (condition-case nil
      (let ((url-request-method "GET")
            (url-request-extra-headers '(("Accept" . "text/html")))
            (url (format "http://%s:%s/"
                        test-supernote-tramp-integration-host
                        test-supernote-tramp-integration-port)))
        (with-current-buffer (url-retrieve-synchronously url t nil 5)
          (goto-char (point-min))
          (prog1 (search-forward "json = '{" nil t)
            (kill-buffer (current-buffer)))))
    (error nil)))

(defmacro test-supernote-tramp-integration-with-server (&rest body)
  "Execute BODY only if the test server is available."
  `(if (test-supernote-tramp-integration-server-available-p)
       (progn ,@body)
     (ert-skip "Supernote test server not available")))

;;; Integration Tests

(ert-deftest test-supernote-tramp-integration-file-exists-root ()
  "Test that the root directory exists."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (should (file-exists-p test-supernote-tramp-integration-url))))

(ert-deftest test-supernote-tramp-integration-directory-p-root ()
  "Test that the root path is recognized as a directory."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (should (file-directory-p test-supernote-tramp-integration-url))))

(ert-deftest test-supernote-tramp-integration-directory-files-root ()
  "Test directory listing of root directory."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((files (directory-files test-supernote-tramp-integration-url)))
     (should (listp files))
     (should (> (length files) 0))
     ;; Based on the server response, we should see these directories
     (should (member "Document" files))
     (should (member "EXPORT" files))
     (should (member "MyStyle" files))
     (should (member "Note" files))
     (should (member "SCREENSHOT" files))
     (should (member "INBOX" files)))))

(ert-deftest test-supernote-tramp-integration-directory-files-full ()
  "Test directory listing with full paths."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((files (directory-files test-supernote-tramp-integration-url t)))
     (should (listp files))
     (should (> (length files) 0))
     ;; All files should be full TRAMP paths
     (dolist (file files)
       (should (string-match-p "^/supernote:" file)))
     ;; Check for specific directories
     (should (seq-some (lambda (f) (string-match-p "/Document$" f)) files))
     (should (seq-some (lambda (f) (string-match-p "/Note$" f)) files)))))

(ert-deftest test-supernote-tramp-integration-directory-files-and-attributes ()
  "Test directory listing with attributes."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((files-and-attrs (directory-files-and-attributes test-supernote-tramp-integration-url)))
     (should (listp files-and-attrs))
     (should (> (length files-and-attrs) 0))
     ;; Each entry should be a cons of filename and attributes
     (dolist (entry files-and-attrs)
       (should (consp entry))
       (should (stringp (car entry)))
       (should (listp (cdr entry))))
     ;; Check that directories are properly marked
     (let ((document-entry (assoc "Document" files-and-attrs)))
       (should document-entry)
       (should (eq (car (cdr document-entry)) t))) ; is-directory should be t
     )))

(ert-deftest test-supernote-tramp-integration-file-attributes ()
  "Test file attributes for specific files."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((document-dir (concat test-supernote-tramp-integration-url "Document")))
     (should (file-exists-p document-dir))
     (let ((attrs (file-attributes document-dir)))
       (should (listp attrs))
       (should (eq (car attrs) t))  ; should be a directory
       (should (numberp (nth 7 attrs)))  ; size should be a number
       (should (listp (nth 5 attrs)))    ; modification time should be a time list
       ))))

(ert-deftest test-supernote-tramp-integration-subdirectory-access ()
  "Test accessing subdirectories."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((document-dir (concat test-supernote-tramp-integration-url "Document")))
     (should (file-directory-p document-dir))
     ;; Try to list contents of Document directory
     (let ((subfiles (condition-case nil
                         (directory-files document-dir)
                       (error nil))))
       ;; The directory should be accessible (might be empty)
       (should (listp subfiles))))))

(ert-deftest test-supernote-tramp-integration-file-name-completion ()
  "Test file name completion."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((completions (file-name-all-completions "Doc" test-supernote-tramp-integration-url)))
     (should (listp completions))
     (should (member "Document" completions)))))

(ert-deftest test-supernote-tramp-integration-dired-functionality ()
  "Test dired integration."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((dired-buffer (dired-noselect test-supernote-tramp-integration-url)))
     (should (bufferp dired-buffer))
     (with-current-buffer dired-buffer
       (should (derived-mode-p 'dired-mode))
       (should (> (count-lines (point-min) (point-max)) 1))
       ;; Check that we can see the expected directories
       (goto-char (point-min))
       (should (search-forward "Document" nil t))
       (goto-char (point-min))
       (should (search-forward "Note" nil t)))
     (kill-buffer dired-buffer))))

(ert-deftest test-supernote-tramp-integration-read-only-operations ()
  "Test that write operations fail appropriately."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((test-file (concat test-supernote-tramp-integration-url "test-file.txt"))
         (test-dir (concat test-supernote-tramp-integration-url "test-dir")))
     ;; Test that write operations fail
     (should-error (write-region "test" nil test-file))
     (should-error (make-directory test-dir))
     (should-error (delete-file test-file))
     (should-error (delete-directory test-dir))
     ;; Test that file-writable-p returns nil
     (should-not (file-writable-p test-supernote-tramp-integration-url))
     (should-not (file-writable-p test-file)))))

(ert-deftest test-supernote-tramp-integration-caching ()
  "Test caching functionality."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((supernote-tramp-cache-enabled t))
     ;; Clear cache first
     (supernote-tramp-flush-all-cache)
     (should (= (hash-table-count supernote-tramp-file-cache) 0))
     
     ;; First call should populate cache
     (let ((files1 (directory-files test-supernote-tramp-integration-url)))
       (should (> (hash-table-count supernote-tramp-file-cache) 0))
       
       ;; Second call should use cache (should be identical)
       (let ((files2 (directory-files test-supernote-tramp-integration-url)))
         (should (equal files1 files2))))
     
     ;; Clear cache again
     (supernote-tramp-flush-all-cache)
     (should (= (hash-table-count supernote-tramp-file-cache) 0)))))

(ert-deftest test-supernote-tramp-integration-error-handling ()
  "Test error handling for non-existent files."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   (let ((nonexistent-file (concat test-supernote-tramp-integration-url "nonexistent-file.txt"))
         (nonexistent-dir (concat test-supernote-tramp-integration-url "nonexistent-dir")))
     ;; Non-existent files should return nil
     (should-not (file-exists-p nonexistent-file))
     (should-not (file-directory-p nonexistent-dir))
     (should-not (file-attributes nonexistent-file))
     ;; But the operations should not signal errors
     (should-not (file-readable-p nonexistent-file))
     (should-not (file-writable-p nonexistent-file)))))

(ert-deftest test-supernote-tramp-integration-device-name ()
  "Test that we can extract device information."
  :tags '(:integration)
  (test-supernote-tramp-integration-with-server
   ;; This test validates that the JSON parsing works correctly
   (let* ((vec (tramp-dissect-file-name test-supernote-tramp-integration-url))
          (file-list (supernote-tramp-get-file-list vec "/")))
     (should (listp file-list))
     (should (assoc 'deviceName file-list))
     (should (assoc 'fileList file-list))
     (let ((device-name (cdr (assoc 'deviceName file-list))))
       (should (stringp device-name))
       (should (> (length device-name) 0))
       (message "Device name: %s" device-name)))))

;;; Test Suite Functions

(defun test-supernote-tramp-integration-run-all-tests ()
  "Run all integration tests."
  (interactive)
  (if (test-supernote-tramp-integration-server-available-p)
      (progn
        (message "Supernote test server available at %s:%s"
                 test-supernote-tramp-integration-host
                 test-supernote-tramp-integration-port)
        (ert-run-tests-batch "test-supernote-tramp-integration"))
    (message "Supernote test server not available at %s:%s"
             test-supernote-tramp-integration-host
             test-supernote-tramp-integration-port)))

(defun test-supernote-tramp-integration-run-interactive ()
  "Run integration tests interactively."
  (interactive)
  (if (test-supernote-tramp-integration-server-available-p)
      (progn
        (message "Running integration tests against Supernote server...")
        (ert "test-supernote-tramp-integration"))
    (message "Supernote test server not available at %s:%s"
             test-supernote-tramp-integration-host
             test-supernote-tramp-integration-port)))

(provide 'test-supernote-tramp-integration)

;;; test-supernote-tramp-integration.el ends here
