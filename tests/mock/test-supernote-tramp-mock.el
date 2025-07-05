;;; test-supernote-tramp-mock.el --- Mock tests for supernote-tramp.el file/directory distinction -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: test, tramp, supernote, mock

;;; Commentary:

;; This file provides comprehensive mock tests for supernote-tramp.el,
;; specifically focusing on file/directory distinction functionality.
;; These tests use mocked HTTP responses to ensure proper handling
;; of files and directories without requiring a live Supernote device.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'supernote-tramp)

(defvar supernote-tramp-test-mock-responses (make-hash-table :test 'equal)
  "Mock responses for supernote-tramp HTTP requests.")

(defun supernote-tramp-test-clear-mocks ()
  "Clear all mock responses."
  (clrhash supernote-tramp-test-mock-responses))

(defun supernote-tramp-test-setup-mock-response (path response)
  "Set up a mock response for PATH with RESPONSE data."
  (puthash path response supernote-tramp-test-mock-responses))

(defun supernote-tramp-test-mock-get-file-list (vec path)
  "Mock version of supernote-tramp-get-file-list for testing."
  (let* ((host (tramp-file-name-host vec))
         (port (supernote-tramp-get-port vec))
         (key (format "%s:%s%s" host port path)))
    (or (gethash key supernote-tramp-test-mock-responses)
        (gethash path supernote-tramp-test-mock-responses)
        (error "No mock response for path: %s (key: %s)" path key))))

(defmacro supernote-tramp-test-with-mocks (&rest body)
  "Execute BODY with mocked supernote-tramp-get-file-list function."
  `(let ((original-func (symbol-function 'supernote-tramp-get-file-list)))
     (unwind-protect
         (progn
           (fset 'supernote-tramp-get-file-list 'supernote-tramp-test-mock-get-file-list)
           (supernote-tramp-test-clear-mocks)
           ,@body)
       (fset 'supernote-tramp-get-file-list original-func)
       (supernote-tramp-test-clear-mocks))))

;; Test data setup

(defun supernote-tramp-test-setup-mixed-directory ()
  "Set up mock data for a directory with mixed files and directories."
  (supernote-tramp-test-setup-mock-response
   "/"
   '((fileList . [((name . "Documents")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 10:00:00"))
                  ((name . "Notes")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 11:00:00"))
                  ((name . "file1.txt")
                   (isDirectory . nil)
                   (size . 1024)
                   (date . "2025-01-10 12:00:00"))
                  ((name . "file2.pdf")
                   (isDirectory . nil)
                   (size . 2048)
                   (date . "2025-01-10 13:00:00"))
                  ((name . "hidden.file")
                   (isDirectory . nil)
                   (size . 512)
                   (date . "2025-01-10 14:00:00"))]))))

(defun supernote-tramp-test-setup-empty-directory ()
  "Set up mock data for an empty directory."
  (supernote-tramp-test-setup-mock-response
   "/empty"
   '((fileList . [])))
  ;; Also set up the version with trailing slash for file-attributes lookups
  (supernote-tramp-test-setup-mock-response
   "/empty/"
   '((fileList . []))))

(defun supernote-tramp-test-setup-files-only-directory ()
  "Set up mock data for a directory with only files."
  ;; Set up root directory with files-only entry
  (supernote-tramp-test-setup-mock-response
   "/"
   '((fileList . [((name . "files-only")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 14:00:00"))])))
  ;; Set up the files-only directory contents
  (supernote-tramp-test-setup-mock-response
   "/files-only"
   '((fileList . [((name . "document.txt")
                   (isDirectory . nil)
                   (size . 1500)
                   (date . "2025-01-10 15:00:00"))
                  ((name . "image.png")
                   (isDirectory . nil)
                   (size . 5000)
                   (date . "2025-01-10 16:00:00"))])))
  ;; Also set up the version with trailing slash for file-attributes lookups
  (supernote-tramp-test-setup-mock-response
   "/files-only/"
   '((fileList . [((name . "document.txt")
                   (isDirectory . nil)
                   (size . 1500)
                   (date . "2025-01-10 15:00:00"))
                  ((name . "image.png")
                   (isDirectory . nil)
                   (size . 5000)
                   (date . "2025-01-10 16:00:00"))]))))

(defun supernote-tramp-test-setup-directories-only ()
  "Set up mock data for a directory with only subdirectories."
  ;; Set up root directory with dirs-only entry
  (supernote-tramp-test-setup-mock-response
   "/"
   '((fileList . [((name . "dirs-only")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 16:00:00"))])))
  ;; Set up the dirs-only directory contents
  (supernote-tramp-test-setup-mock-response
   "/dirs-only"
   '((fileList . [((name . "subdir1")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 17:00:00"))
                  ((name . "subdir2")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 18:00:00"))])))
  ;; Also set up the version with trailing slash for file-attributes lookups
  (supernote-tramp-test-setup-mock-response
   "/dirs-only/"
   '((fileList . [((name . "subdir1")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 17:00:00"))
                  ((name . "subdir2")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 18:00:00"))]))))

(defun supernote-tramp-test-setup-nested-structure ()
  "Set up mock data for nested directory structure."
  (supernote-tramp-test-setup-mock-response
   "/Documents"
   '((fileList . [((name . "subfolder")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 19:00:00"))
                  ((name . "readme.txt")
                   (isDirectory . nil)
                   (size . 800)
                   (date . "2025-01-10 20:00:00"))])))
  ;; Also set up the version with trailing slash for file-attributes lookups
  (supernote-tramp-test-setup-mock-response
   "/Documents/"
   '((fileList . [((name . "subfolder")
                   (isDirectory . t)
                   (size . 0)
                   (date . "2025-01-10 19:00:00"))
                  ((name . "readme.txt")
                   (isDirectory . nil)
                   (size . 800)
                   (date . "2025-01-10 20:00:00"))]))))

;; Test cases

(ert-deftest supernote-tramp-test-mock-file-directory-p-mixed ()
  "Test file-directory-p with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test directories
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/Documents"))
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/Notes"))
   
   ;; Test files
   (should-not (supernote-tramp-handle-file-directory-p "/supernote:testhost:/file1.txt"))
   (should-not (supernote-tramp-handle-file-directory-p "/supernote:testhost:/file2.pdf"))
   (should-not (supernote-tramp-handle-file-directory-p "/supernote:testhost:/hidden.file"))
   
   ;; Test root directory
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/"))))

(ert-deftest supernote-tramp-test-mock-file-exists-p-mixed ()
  "Test file-exists-p with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test existing files and directories
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/Documents"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/Notes"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/file1.txt"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/file2.pdf"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/hidden.file"))
   
   ;; Test non-existing files
   (should-not (supernote-tramp-handle-file-exists-p "/supernote:testhost:/nonexistent.txt"))
   (should-not (supernote-tramp-handle-file-exists-p "/supernote:testhost:/NoSuchDir"))
   
   ;; Test root directory
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/"))))

(ert-deftest supernote-tramp-test-mock-file-attributes-mixed ()
  "Test file-attributes with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test directory attributes
   (let ((attrs (supernote-tramp-handle-file-attributes "/supernote:testhost:/Documents")))
     (should attrs)
     (should (eq (car attrs) t))  ; is directory
     (should (= (nth 7 attrs) 0)))  ; size is 0 for directory
   
   ;; Test file attributes
   (let ((attrs (supernote-tramp-handle-file-attributes "/supernote:testhost:/file1.txt")))
     (should attrs)
     (should (eq (car attrs) nil))  ; is not directory
     (should (= (nth 7 attrs) 1024)))  ; size is 1024
   
   (let ((attrs (supernote-tramp-handle-file-attributes "/supernote:testhost:/file2.pdf")))
     (should attrs)
     (should (eq (car attrs) nil))  ; is not directory
     (should (= (nth 7 attrs) 2048)))  ; size is 2048
   
   ;; Test non-existing file
   (should-not (supernote-tramp-handle-file-attributes "/supernote:testhost:/nonexistent.txt"))))

(ert-deftest supernote-tramp-test-mock-directory-files-mixed ()
  "Test directory-files with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test basic directory listing
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/")))
     ;; Should have 5 files plus "." and ".." entries
     (should (= (length files) 7))
     (should (member "Documents" files))
     (should (member "Notes" files))
     (should (member "file1.txt" files))
     (should (member "file2.pdf" files))
     (should (member "hidden.file" files))
     (should (member "." files))
     (should (member ".." files)))
   
   ;; Test with pattern matching
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/" nil ".*\\.txt$")))
     (should (= (length files) 1))
     (should (member "file1.txt" files)))
   
   ;; Test with full paths
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/" t)))
     ;; Should have 5 files plus "." and ".." entries
     (should (= (length files) 7))
     (should (member "/supernote:testhost:/Documents" files))
     (should (member "/supernote:testhost:/file1.txt" files))
     (should (member "/supernote:testhost:/." files))
     (should (member "/supernote:testhost:/.." files)))))

(ert-deftest supernote-tramp-test-mock-directory-files-and-attributes-mixed ()
  "Test directory-files-and-attributes with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test basic directory listing with attributes
   (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/")))
     ;; Should have 5 files plus "." and ".." entries
     (should (= (length files-attrs) 7))
     
     ;; Check Documents directory
     (let ((doc-entry (assoc "Documents" files-attrs)))
       (should doc-entry)
       (should (eq (car (cdr doc-entry)) t))  ; is directory
       (should (= (nth 7 (cdr doc-entry)) 0)))  ; size is 0
     
     ;; Check file1.txt
     (let ((file-entry (assoc "file1.txt" files-attrs)))
       (should file-entry)
       (should (eq (car (cdr file-entry)) nil))  ; is not directory
       (should (= (nth 7 (cdr file-entry)) 1024)))  ; size is 1024
     
     ;; Check file2.pdf
     (let ((file-entry (assoc "file2.pdf" files-attrs)))
       (should file-entry)
       (should (eq (car (cdr file-entry)) nil))  ; is not directory
       (should (= (nth 7 (cdr file-entry)) 2048))))))  ; size is 2048

(ert-deftest supernote-tramp-test-mock-empty-directory ()
  "Test operations on empty directory."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-empty-directory)
   
   ;; Test directory listing
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/empty")))
     ;; Empty directory should still have "." and ".." entries
     (should (= (length files) 2))
     (should (member "." files))
     (should (member ".." files)))
   
   ;; Test directory listing with attributes
   (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/empty")))
     ;; Empty directory should still have "." and ".." entries
     (should (= (length files-attrs) 2))
     (should (assoc "." files-attrs))
     (should (assoc ".." files-attrs)))))

(ert-deftest supernote-tramp-test-mock-files-only-directory ()
  "Test operations on directory with only files."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-files-only-directory)
   
   ;; Test directory listing
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/files-only")))
     ;; Should have 2 files plus "." and ".." entries
     (should (= (length files) 4))
     (should (member "document.txt" files))
     (should (member "image.png" files))
     (should (member "." files))
     (should (member ".." files)))
   
   ;; Test that all entries are properly categorized
   (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/files-only")))
     (should (= (length files-attrs) 4))
     ;; Check that "." and ".." are directories
     (should (eq (car (cdr (assoc "." files-attrs))) t))
     (should (eq (car (cdr (assoc ".." files-attrs))) t))
     ;; Check that actual files are files
     (should (eq (car (cdr (assoc "document.txt" files-attrs))) nil))
     (should (eq (car (cdr (assoc "image.png" files-attrs))) nil)))))

(ert-deftest supernote-tramp-test-mock-directories-only ()
  "Test operations on directory with only subdirectories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-directories-only)
   
   ;; Test directory listing
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/dirs-only")))
     ;; Should have 2 directories plus "." and ".." entries
     (should (= (length files) 4))
     (should (member "subdir1" files))
     (should (member "subdir2" files))
     (should (member "." files))
     (should (member ".." files)))
   
   ;; Test that all entries are directories
   (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/dirs-only")))
     (should (= (length files-attrs) 4))
     ;; All should be directories
     (dolist (file-attr files-attrs)
       (should (eq (car (cdr file-attr)) t))))))

(ert-deftest supernote-tramp-test-mock-file-completion ()
  "Test file name completion with mixed files and directories."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test completion for 'file' prefix
   (let ((completions (supernote-tramp-handle-file-name-all-completions "file" "/supernote:testhost:/")))
     (should (= (length completions) 2))
     (should (member "file1.txt" completions))
     (should (member "file2.pdf" completions)))
   
   ;; Test completion for 'D' prefix
   (let ((completions (supernote-tramp-handle-file-name-all-completions "D" "/supernote:testhost:/")))
     (should (= (length completions) 1))
     (should (member "Documents" completions)))
   
   ;; Test completion for non-matching prefix
   (let ((completions (supernote-tramp-handle-file-name-all-completions "xyz" "/supernote:testhost:/")))
     (should (= (length completions) 0)))))

(ert-deftest supernote-tramp-test-mock-nested-structure ()
  "Test nested directory structure handling."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   (supernote-tramp-test-setup-nested-structure)
   
   ;; Test that Documents directory exists in root
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/Documents"))
   
   ;; Test contents of Documents directory
   (let ((files (supernote-tramp-handle-directory-files "/supernote:testhost:/Documents")))
     ;; Should have 2 files plus "." and ".." entries
     (should (= (length files) 4))
     (should (member "subfolder" files))
     (should (member "readme.txt" files))
     (should (member "." files))
     (should (member ".." files)))
   
   ;; Test file vs directory distinction in nested structure
   (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/Documents")))
     ;; Should have 2 files plus "." and ".." entries
     (should (= (length files-attrs) 4))
     
     ;; Check "." and ".." are directories
     (let ((dot-entry (assoc "." files-attrs)))
       (should dot-entry)
       (should (eq (car (cdr dot-entry)) t)))
     (let ((dotdot-entry (assoc ".." files-attrs)))
       (should dotdot-entry)
       (should (eq (car (cdr dotdot-entry)) t)))
     
     ;; Check subfolder is a directory
     (let ((subfolder-entry (assoc "subfolder" files-attrs)))
       (should subfolder-entry)
       (should (eq (car (cdr subfolder-entry)) t)))
     
     ;; Check readme.txt is a file
     (let ((readme-entry (assoc "readme.txt" files-attrs)))
       (should readme-entry)
       (should (eq (car (cdr readme-entry)) nil))))))

(ert-deftest supernote-tramp-test-mock-edge-cases ()
  "Test edge cases in file/directory distinction."
  (supernote-tramp-test-with-mocks
   (supernote-tramp-test-setup-mixed-directory)
   
   ;; Test root directory handling
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/"))
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:"))
   
   ;; Test case sensitivity
   (should-not (supernote-tramp-handle-file-exists-p "/supernote:testhost:/documents"))  ; lowercase
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/Documents"))  ; correct case
   
   ;; Test with trailing slashes
   (should (supernote-tramp-handle-file-directory-p "/supernote:testhost:/Documents/"))
   (should (supernote-tramp-handle-file-exists-p "/supernote:testhost:/Documents/"))))

(ert-deftest supernote-tramp-test-mock-error-handling ()
  "Test error handling in file/directory operations."
  (supernote-tramp-test-with-mocks
   ;; Don't set up any mock responses
   
   ;; Test operations on non-existent paths should return nil, not error
   (should-not (supernote-tramp-handle-file-exists-p "/supernote:testhost:/nonexistent"))
   (should-not (supernote-tramp-handle-file-directory-p "/supernote:testhost:/nonexistent"))
   (should-not (supernote-tramp-handle-file-attributes "/supernote:testhost:/nonexistent"))
   
   ;; Test directory operations on non-existent paths should return nil
   (should-not (supernote-tramp-handle-directory-files "/supernote:testhost:/nonexistent"))
   (should-not (supernote-tramp-handle-directory-files-and-attributes "/supernote:testhost:/nonexistent"))))

;; Test runner function

(defun supernote-tramp-test-run-mock-tests ()
  "Run all mock tests for supernote-tramp file/directory distinction."
  (interactive)
  (let ((test-results '()))
    (dolist (test '(supernote-tramp-test-mock-file-directory-p-mixed
                    supernote-tramp-test-mock-file-exists-p-mixed
                    supernote-tramp-test-mock-file-attributes-mixed
                    supernote-tramp-test-mock-directory-files-mixed
                    supernote-tramp-test-mock-directory-files-and-attributes-mixed
                    supernote-tramp-test-mock-empty-directory
                    supernote-tramp-test-mock-files-only-directory
                    supernote-tramp-test-mock-directories-only
                    supernote-tramp-test-mock-file-completion
                    supernote-tramp-test-mock-nested-structure
                    supernote-tramp-test-mock-edge-cases
                    supernote-tramp-test-mock-error-handling))
      (let ((result (ert-run-test (ert-get-test test))))
        (push (cons test (ert-test-result-type-p result :passed)) test-results)))
    
    (let ((passed (length (seq-filter 'cdr test-results)))
          (total (length test-results)))
      (message "Mock tests completed: %d/%d passed" passed total)
      (when (< passed total)
        (message "Failed tests:")
        (dolist (test-result test-results)
          (unless (cdr test-result)
            (message "  - %s" (car test-result)))))
      (= passed total))))

(provide 'test-supernote-tramp-mock)

;;; test-supernote-tramp-mock.el ends here
