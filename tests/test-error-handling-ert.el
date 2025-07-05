;;; test-error-handling-ert.el --- ERT tests for error handling in supernote-tramp

;; Copyright (C) 2025

;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: files, remote, supernote, tramp, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive ERT tests for error handling scenarios in supernote-tramp.
;; Tests network failures, malformed responses, invalid URLs, and edge cases.

;;; Code:

(require 'ert)
(require 'supernote-tramp)

(defvar test-supernote-tramp-invalid-host "192.168.1.999"
  "Invalid host for testing connection failures.")

(defvar test-supernote-tramp-invalid-port 99999
  "Invalid port for testing connection failures.")

(ert-deftest test-supernote-tramp-handle-network-failure ()
  "Test handling of network connection failures."
  (let ((test-file (format "/supernote:%s#%d:/nonexistent.txt" 
                           test-supernote-tramp-invalid-host
                           test-supernote-tramp-invalid-port)))
    ;; Network failures should return nil gracefully, not crash
    (should-not (file-exists-p test-file))
    (should-not (file-directory-p test-file))
    (should-not (file-attributes test-file))))

(ert-deftest test-supernote-tramp-handle-malformed-json ()
  "Test handling of malformed JSON responses."
  ;; This tests the parsing logic with mock bad data
  (let ((supernote-tramp-file-cache (make-hash-table :test 'equal)))
    ;; Mock a malformed response in cache
    (puthash "/supernote:test:/" 
             '(:timestamp 0 :data "invalid-json-data")
             supernote-tramp-file-cache)
    
    ;; Should handle gracefully
    (should-not (supernote-tramp-handle-file-exists-p "/supernote:test:/test.txt"))))

(ert-deftest test-supernote-tramp-port-validation ()
  "Test port number validation and defaults."
  (let ((vec-no-port (tramp-dissect-file-name "/supernote:host:/"))
        (vec-valid-port (tramp-dissect-file-name "/supernote:host#8080:/")))
    
    ;; No port should use default
    (should (= (supernote-tramp-get-port vec-no-port) supernote-tramp-default-port))
    
    ;; Valid port should be used
    (should (= (supernote-tramp-get-port vec-valid-port) 8080))
    
    ;; Test with port extraction from valid vectors
    (let ((vec-another-port (tramp-dissect-file-name "/supernote:host#9090:/")))
      (should (= (supernote-tramp-get-port vec-another-port) 9090)))))

(ert-deftest test-supernote-tramp-invalid-filenames ()
  "Test handling of invalid or malformed filenames."
  (let ((invalid-files '("/supernote::/empty-host.txt"
                        "/supernote:host:/../../escape-attempt.txt"
                        "/supernote:host:/file with spaces.txt"
                        "/supernote:host:/файл.txt")))  ; Unicode filename
    
    ;; Should handle gracefully without crashing
    (dolist (file invalid-files)
      (should-not (file-exists-p file))
      (should-not (file-directory-p file))
      (should-not (file-attributes file)))))

(ert-deftest test-supernote-tramp-readonly-error-messages ()
  "Test that read-only operations produce appropriate error messages."
  (let ((test-file "/supernote:test:/test.txt")
        (test-dir "/supernote:test:/test-dir/"))
    
    ;; Test write-region error
    (should-error (supernote-tramp-handle-write-region 1 10 test-file)
                  :type 'file-error)
    
    ;; Test delete-file error
    (should-error (supernote-tramp-handle-delete-file test-file)
                  :type 'file-error)
    
    ;; Test delete-directory error
    (should-error (supernote-tramp-handle-delete-directory test-dir)
                  :type 'file-error)
    
    ;; Test make-directory error
    (should-error (supernote-tramp-handle-make-directory test-dir)
                  :type 'file-error)
    
    ;; Test rename-file error
    (should-error (supernote-tramp-handle-rename-file test-file "/tmp/newname")
                  :type 'file-error)
    
    ;; Test make-symbolic-link error
    (should-error (supernote-tramp-handle-make-symbolic-link test-file "/tmp/link")
                  :type 'file-error)))

(ert-deftest test-supernote-tramp-cache-key-generation ()
  "Test cache key generation for various paths."
  (let ((vec (tramp-dissect-file-name "/supernote:test:/")))
    
    ;; Normal paths
    (should (stringp (supernote-tramp-cache-key vec "/")))
    (should (stringp (supernote-tramp-cache-key vec "/Documents")))
    (should (stringp (supernote-tramp-cache-key vec "/Documents/file.txt")))
    
    ;; Edge case paths
    (should (stringp (supernote-tramp-cache-key vec "")))
    (should (stringp (supernote-tramp-cache-key vec "//")))
    (should (stringp (supernote-tramp-cache-key vec "///")))
    
    ;; Keys should be different for different paths
    (should-not (equal (supernote-tramp-cache-key vec "/")
                      (supernote-tramp-cache-key vec "/Documents")))))

(ert-deftest test-supernote-tramp-url-construction ()
  "Test URL construction for various hosts and ports."
  (let ((vec-ipv4 (tramp-dissect-file-name "/supernote:192.168.1.100#8080:/"))
        (vec-hostname (tramp-dissect-file-name "/supernote:supernote.local#8080:/"))
        (vec-default-port (tramp-dissect-file-name "/supernote:192.168.1.100:/")))
    
    ;; Test that URLs are constructed properly
    (should (stringp (tramp-file-name-host vec-ipv4)))
    (should (stringp (tramp-file-name-host vec-hostname)))
    (should (stringp (tramp-file-name-host vec-default-port)))
    
    ;; Test port extraction
    (should (= (supernote-tramp-get-port vec-ipv4) 8080))
    (should (= (supernote-tramp-get-port vec-hostname) 8080))
    (should (= (supernote-tramp-get-port vec-default-port) supernote-tramp-default-port))))

(ert-deftest test-supernote-tramp-json-conversion-edge-cases ()
  "Test JSON conversion with edge cases."
  ;; Test with empty structures
  (should (equal (supernote-tramp-json-to-list []) '()))
  (should (equal (supernote-tramp-json-to-list '()) '()))
  
  ;; Test with nested empty structures
  (let ((complex-empty '((fileList . []) (metadata . ((nested . []))))))
    (let ((converted (supernote-tramp-json-to-list complex-empty)))
      (should (listp (cdr (assoc 'fileList converted))))
      (should (null (cdr (assoc 'fileList converted))))))
  
  ;; Test with nil values
  (should (equal (supernote-tramp-json-to-list nil) nil)))

(ert-deftest test-supernote-tramp-file-attributes-edge-cases ()
  "Test file attribute parsing with edge cases."
  (let ((test-files '("/supernote:test:/nonexistent.txt"
                     "/supernote:test:/.."
                     "/supernote:test:/."
                     "/supernote:test:/.hidden")))
    
    ;; These should handle gracefully
    (dolist (file test-files)
      (let ((attrs (file-attributes file)))
        ;; Should either return nil or valid attributes
        (when attrs
          (should (listp attrs))
          (should (>= (length attrs) 10)))))))

(provide 'test-error-handling-ert)
;;; test-error-handling-ert.el ends here
