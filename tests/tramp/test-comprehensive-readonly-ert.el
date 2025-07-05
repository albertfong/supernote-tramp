;;; test-comprehensive-readonly-ert.el --- Comprehensive ERT tests for read-only operations

(require 'ert)
(require 'supernote-tramp)

;; Load test configuration
(load-file "../supernote-tramp-test-config.el")

(defvar test-server (format "%s#%d" supernote-tramp-test-server-ip supernote-tramp-test-server-port))
(defvar test-local-dir "/tmp/supernote-readonly-test/")

(ert-deftest test-comprehensive-file-exists-p ()
  "Test file-exists-p for files, directories, and root."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note"))
        (root-dir (supernote-tramp-test-path "/")))
    
    (should (file-exists-p test-file))
    (should (file-exists-p test-dir))
    (should (file-exists-p root-dir))))

(ert-deftest test-comprehensive-file-directory-p ()
  "Test file-directory-p for files and directories."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note"))
        (root-dir (supernote-tramp-test-path "/")))
    
    (should (not (file-directory-p test-file)))
    (should (file-directory-p test-dir))
    (should (file-directory-p root-dir))))

(ert-deftest test-comprehensive-file-readable-p ()
  "Test file-readable-p for files and directories."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note")))
    
    (should (file-readable-p test-file))
    (should (file-readable-p test-dir))))

(ert-deftest test-comprehensive-file-attributes ()
  "Test file-attributes for files and directories."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note")))
    
    (let ((file-attrs (file-attributes test-file))
          (dir-attrs (file-attributes test-dir)))
      
      (should (listp file-attrs))
      (should (listp dir-attrs))
      (should (not (car file-attrs))) ; file is not a directory
      (should (car dir-attrs))         ; directory is a directory
      (should (> (nth 7 file-attrs) 0)) ; file size > 0
      )))

(ert-deftest test-comprehensive-directory-files ()
  "Test directory-files functionality."
  (let ((test-dir (supernote-tramp-test-path "/Note"))
        (root-dir (supernote-tramp-test-path "/")))
    
    (let ((note-files (directory-files test-dir))
          (root-files (directory-files root-dir)))
      
      (should (listp note-files))
      (should (listp root-files))
      (should (> (length note-files) 0))
      (should (> (length root-files) 0))
      (should (member "." note-files))
      (should (member ".." note-files)))))

(ert-deftest test-comprehensive-directory-files-and-attributes ()
  "Test directory-files-and-attributes functionality."
  (let ((test-dir (supernote-tramp-test-path "/Note")))
    
    (let ((files-attrs (directory-files-and-attributes test-dir nil nil nil nil 3)))
      (should (listp files-attrs))
      (should (> (length files-attrs) 0))
      (should (cl-every (lambda (entry) 
                         (and (stringp (car entry)) 
                              (listp (cdr entry)))) 
                       files-attrs)))))

(ert-deftest test-comprehensive-blocked-write-operations ()
  "Test that all write operations are properly blocked."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note"))
        (new-file (supernote-tramp-test-path "/Note/test-new-file.note"))
        (new-dir (supernote-tramp-test-path "/Note/test-new-dir")))
    
    ;; Test write-region
    (should-error (write-region "test content" nil test-file))
    (should-error (write-region "test content" nil new-file))
    
    ;; Test delete-file
    (should-error (delete-file test-file))
    
    ;; Test rename-file
    (should-error (rename-file test-file (concat test-file ".bak")))
    
    ;; Test make-directory
    (should-error (make-directory new-dir))
    
    ;; Test delete-directory
    (should-error (delete-directory test-dir))
    
    ;; Test make-symbolic-link
    (should-error (make-symbolic-link test-file (concat test-file ".link")))))

(ert-deftest test-comprehensive-file-permissions ()
  "Test file permission checks."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note")))
    
    ;; Files should be readable but not writable
    (should (file-readable-p test-file))
    (should (not (file-writable-p test-file)))
    
    ;; Directories should be readable/executable but not writable
    (should (file-readable-p test-dir))
    (should (file-executable-p test-dir))
    (should (not (file-writable-p test-dir)))
    
    ;; Test file modes
    (should (integerp (file-modes test-file)))
    (should (integerp (file-modes test-dir)))))

(ert-deftest test-comprehensive-copy-operations ()
  "Test file copy operations to local filesystem."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (local-file "/tmp/supernote-comprehensive-copy-test.note"))
    
    ;; Clean up first
    (when (file-exists-p local-file)
      (delete-file local-file))
    
    ;; Test copy-file
    (copy-file test-file local-file)
    (should (file-exists-p local-file))
    (should (> (nth 7 (file-attributes local-file)) 0))
    
    ;; Test file-local-copy
    (let ((temp-copy (file-local-copy test-file)))
      (should (stringp temp-copy))
      (should (file-exists-p temp-copy))
      (should (> (nth 7 (file-attributes temp-copy)) 0))
      (delete-file temp-copy))
    
    ;; Clean up
    (delete-file local-file)))

(ert-deftest test-comprehensive-dired-operations ()
  "Test dired-specific operations."
  (let ((test-dir (supernote-tramp-test-path "/Note")))
    
    ;; Test insert-directory
    (with-temp-buffer
      (insert-directory test-dir "-la")
      (should (> (buffer-size) 0))
      (should (string-match "20250" (buffer-string))))
    
    ;; Test file-name-completion
    (let ((completions (file-name-all-completions "20250" test-dir)))
      (should (listp completions))
      (should (> (length completions) 0)))
    
    ;; Test expand-file-name
    (let ((expanded (expand-file-name "test.note" test-dir)))
      (should (stringp expanded))
      (should (string-match "/Note/test.note" expanded)))))

(ert-deftest test-comprehensive-navigation ()
  "Test navigation and path operations."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note")))
    
    ;; Test file-truename
    (let ((truename (file-truename test-file)))
      (should (stringp truename))
      (should (string= truename test-file)))
    
    ;; Test file-remote-p
    (should (file-remote-p test-file))
    (should (string-match "supernote:" (file-remote-p test-file)))
    
    ;; Test file-accessible-directory-p
    (should (file-accessible-directory-p test-dir))
    (should (not (file-accessible-directory-p test-file)))
    
    ;; Test substitute-in-file-name
    (let ((substituted (substitute-in-file-name test-file)))
      (should (string= substituted test-file)))))

(ert-deftest test-comprehensive-performance ()
  "Test that operations complete in reasonable time."
  (let ((test-dir (supernote-tramp-test-path "/Note")))
    
    ;; Basic operations should complete quickly
    (let ((start-time (current-time)))
      (directory-files test-dir nil nil nil 5)
      (let ((elapsed (time-to-seconds (time-subtract (current-time) start-time))))
        (should (< elapsed 10)))) ; Should complete in less than 10 seconds
    
    (let ((start-time (current-time)))
      (file-exists-p test-dir)
      (let ((elapsed (time-to-seconds (time-subtract (current-time) start-time))))
        (should (< elapsed 5)))) ; Should complete in less than 5 seconds
    
    (let ((start-time (current-time)))
      (file-directory-p test-dir)
      (let ((elapsed (time-to-seconds (time-subtract (current-time) start-time))))
        (should (< elapsed 5)))))) ; Should complete in less than 5 seconds

(provide 'test-comprehensive-readonly-ert)
;;; test-comprehensive-readonly-ert.el ends here
