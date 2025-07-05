;;; test-supernote-tramp.el --- Tests for supernote-tramp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: files, remote, supernote, tramp, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the supernote-tramp package using ERT (Emacs Lisp Regression Testing).
;; Run tests with: M-x ert RET test-supernote-tramp RET

;;; Code:

(require 'ert)
(require 'supernote-tramp)

(defvar test-supernote-tramp-mock-host "192.168.1.100"
  "Mock host for testing.")

(defvar test-supernote-tramp-mock-port 8089
  "Mock port for testing.")

(defvar test-supernote-tramp-mock-json-response
  '((fileList . [((name . "test.txt") (isDirectory . :json-false) (size . 1024) (date . "2025-01-01T12:00:00Z"))
                 ((name . "folder") (isDirectory . t) (size . 0) (date . "2025-01-01T12:00:00Z"))
                 ((name . "document.pdf") (isDirectory . :json-false) (size . 2048) (date . "2025-01-01T12:00:00Z"))]))
  "Mock JSON response for testing.")

;;; Unit Tests

(ert-deftest test-supernote-tramp-method-constant ()
  "Test that the supernote method constant is defined correctly."
  (should (equal supernote-tramp-method "supernote")))

(ert-deftest test-supernote-tramp-default-port ()
  "Test that the default port is set correctly."
  (should (numberp supernote-tramp-default-port))
  (should (= supernote-tramp-default-port 8089)))

(ert-deftest test-supernote-tramp-get-port ()
  "Test port extraction from TRAMP file name vector."
  (let ((vec-with-port (tramp-dissect-file-name "/supernote:192.168.1.100#8080:/"))
        (vec-without-port (tramp-dissect-file-name "/supernote:192.168.1.100:/")))
    (should (= (supernote-tramp-get-port vec-with-port) 8080))
    (should (= (supernote-tramp-get-port vec-without-port) supernote-tramp-default-port))))

(ert-deftest test-supernote-tramp-cache-functionality ()
  "Test cache functionality."
  (let ((test-key "test-host:8089/test/path")
        (test-value '((test . "data"))))
    ;; Clear cache first
    (clrhash supernote-tramp-file-cache)
    (should (= (hash-table-count supernote-tramp-file-cache) 0))
    
    ;; Add to cache
    (puthash test-key test-value supernote-tramp-file-cache)
    (should (= (hash-table-count supernote-tramp-file-cache) 1))
    (should (equal (gethash test-key supernote-tramp-file-cache) test-value))
    
    ;; Test cache flush
    (supernote-tramp-flush-all-cache)
    (should (= (hash-table-count supernote-tramp-file-cache) 0))))

(ert-deftest test-supernote-tramp-file-name-handler-alist ()
  "Test that file name handler alist is properly defined."
  (should (listp supernote-tramp-file-name-handler-alist))
  (should (> (length supernote-tramp-file-name-handler-alist) 0))
  
  ;; Test that key operations are handled
  (should (assoc 'file-exists-p supernote-tramp-file-name-handler-alist))
  (should (assoc 'file-directory-p supernote-tramp-file-name-handler-alist))
  (should (assoc 'directory-files supernote-tramp-file-name-handler-alist))
  (should (assoc 'file-attributes supernote-tramp-file-name-handler-alist))
  (should (assoc 'file-local-copy supernote-tramp-file-name-handler-alist))
  (should (assoc 'insert-file-contents supernote-tramp-file-name-handler-alist))
  (should (assoc 'insert-directory supernote-tramp-file-name-handler-alist))
  
  ;; Test that read-only operations are handled
  (should (assoc 'file-writable-p supernote-tramp-file-name-handler-alist))
  (should (assoc 'write-region supernote-tramp-file-name-handler-alist))
  (should (assoc 'delete-file supernote-tramp-file-name-handler-alist))
  (should (assoc 'delete-directory supernote-tramp-file-name-handler-alist))
  (should (assoc 'make-directory supernote-tramp-file-name-handler-alist))
  (should (assoc 'rename-file supernote-tramp-file-name-handler-alist))
  (should (assoc 'make-symbolic-link supernote-tramp-file-name-handler-alist)))

(ert-deftest test-supernote-tramp-read-only-handlers ()
  "Test that read-only filesystem handlers work correctly."
  (let ((test-file "/supernote:192.168.1.100:/test.txt")
        (test-dir "/supernote:192.168.1.100:/testdir"))
    
    ;; Test file-writable-p always returns nil
    (should-not (supernote-tramp-handle-file-writable-p test-file))
    
    ;; Test that write operations signal errors
    (should-error (supernote-tramp-handle-write-region 1 10 test-file))
    (should-error (supernote-tramp-handle-delete-file test-file))
    (should-error (supernote-tramp-handle-delete-directory test-dir))
    (should-error (supernote-tramp-handle-make-directory test-dir))
    (should-error (supernote-tramp-handle-rename-file test-file "/tmp/newname"))
    (should-error (supernote-tramp-handle-make-symbolic-link test-file "/tmp/link"))))

(ert-deftest test-supernote-tramp-filename-parsing ()
  "Test TRAMP filename parsing for supernote URLs."
  (let ((filename-with-port "/supernote:192.168.1.100#8080:/path/to/file.txt")
        (filename-without-port "/supernote:192.168.1.100:/path/to/file.txt")
        (filename-root "/supernote:192.168.1.100:/"))
    
    ;; Test dissection works
    (should (tramp-tramp-file-p filename-with-port))
    (should (tramp-tramp-file-p filename-without-port))
    (should (tramp-tramp-file-p filename-root))
    
    ;; Test components extraction
    (let ((vec-with-port (tramp-dissect-file-name filename-with-port)))
      (should (equal (tramp-file-name-method vec-with-port) "supernote"))
      (should (equal (tramp-file-name-host vec-with-port) "192.168.1.100"))
      (should (equal (tramp-file-name-port vec-with-port) "8080"))
      (should (equal (tramp-file-name-localname vec-with-port) "/path/to/file.txt")))
    
    (let ((vec-without-port (tramp-dissect-file-name filename-without-port)))
      (should (equal (tramp-file-name-method vec-without-port) "supernote"))
      (should (equal (tramp-file-name-host vec-without-port) "192.168.1.100"))
      (should (null (tramp-file-name-port vec-without-port)))
      (should (equal (tramp-file-name-localname vec-without-port) "/path/to/file.txt")))))

(ert-deftest test-supernote-tramp-custom-variables ()
  "Test that custom variables are properly defined."
  (should (boundp 'supernote-tramp-default-port))
  (should (boundp 'supernote-tramp-connection-timeout))
  (should (boundp 'supernote-tramp-cache-enabled))
  (should (boundp 'supernote-tramp-disable-hostname-verification))
  
  ;; Test types
  (should (integerp supernote-tramp-default-port))
  (should (integerp supernote-tramp-connection-timeout))
  (should (or (eq supernote-tramp-cache-enabled t) (eq supernote-tramp-cache-enabled nil)))
  (should (or (eq supernote-tramp-disable-hostname-verification t) 
              (eq supernote-tramp-disable-hostname-verification nil))))

(ert-deftest test-supernote-tramp-setup ()
  "Test that setup function registers the method correctly."
  ;; The setup should have been called automatically when loading the package
  (should (assoc supernote-tramp-method tramp-methods))
  
  ;; Test method configuration
  (let ((method-config (assoc supernote-tramp-method tramp-methods)))
    (should method-config)
    (should (assoc 'tramp-default-port (cdr method-config)))
    (should (assoc 'tramp-connection-timeout (cdr method-config)))))

;;; Integration Tests (require network connection)

(ert-deftest test-supernote-tramp-network-functions ()
  "Test network-dependent functions (will be skipped if no network)."
  :tags '(:network)
  (skip-unless (and (boundp 'test-supernote-tramp-real-host)
                    (stringp test-supernote-tramp-real-host)))
  
  ;; These tests would require a real Supernote device
  ;; For now, we'll just test that the functions exist and can be called
  (should (fboundp 'supernote-tramp-get-file-list))
  (should (fboundp 'supernote-tramp-handle-file-exists-p))
  (should (fboundp 'supernote-tramp-handle-directory-files)))

;;; Test Suite

(defun test-supernote-tramp-run-all-tests ()
  "Run all supernote-tramp tests."
  (interactive)
  (ert-run-tests-batch "test-supernote-tramp"))

(defun test-supernote-tramp-run-unit-tests ()
  "Run only unit tests (no network required)."
  (interactive)
  (ert-run-tests-batch '(not (tag :network))))

(provide 'test-supernote-tramp)

;;; test-supernote-tramp.el ends here
