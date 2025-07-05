;;; test-readonly-and-copy-ert.el --- ERT tests for read-only functionality and file copying

(require 'ert)
(require 'supernote-tramp)

;; Load test configuration
(load-file "../supernote-tramp-test-config.el")

(defvar test-server (format "%s#%d" supernote-tramp-test-server-ip supernote-tramp-test-server-port))
(defvar test-local-dir "/tmp/supernote-copy-test/")

(ert-deftest test-readonly-file-writable-p ()
  "Test that file-writable-p returns nil for read-only filesystem."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note")))
    (should (not (file-writable-p test-file)))))

(ert-deftest test-readonly-directory-writable-p ()
  "Test that directory-writable-p returns nil for read-only filesystem."
  (let ((test-dir (supernote-tramp-test-path "/Note")))
    (should (not (file-writable-p test-dir)))))

(ert-deftest test-readonly-write-region-blocked ()
  "Test that write-region is properly blocked."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note")))
    (should-error (write-region "test content" nil test-file))))

(ert-deftest test-readonly-delete-file-blocked ()
  "Test that delete-file is properly blocked."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note")))
    (should-error (delete-file test-file))))

(ert-deftest test-readonly-make-directory-blocked ()
  "Test that make-directory is properly blocked."
  (let ((test-dir (supernote-tramp-test-path "/Note/TestDir")))
    (should-error (make-directory test-dir))))

(ert-deftest test-readonly-rename-file-blocked ()
  "Test that rename-file is properly blocked."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (new-name (supernote-tramp-test-path "/Note/20250701_081047.note.bak")))
    (should-error (rename-file test-file new-name))))

(ert-deftest test-readonly-file-permissions ()
  "Test file permission functions."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (test-dir (supernote-tramp-test-path "/Note")))
    
    ;; Test file-readable-p
    (should (file-readable-p test-file))
    (should (file-readable-p test-dir))
    
    ;; Test file-executable-p for directory
    (should (file-executable-p test-dir))
    
    ;; Test file-modes
    (should (integerp (file-modes test-file)))
    (should (> (file-modes test-file) 0))))

(ert-deftest test-copy-file-to-local ()
  "Test copying files from Supernote to local filesystem."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (local-file "/tmp/supernote-copy-test.note"))
    
    ;; Clean up any existing file
    (when (file-exists-p local-file)
      (delete-file local-file))
    
    ;; Test copy-file
    (copy-file test-file local-file)
    (should (file-exists-p local-file))
    (should (> (nth 7 (file-attributes local-file)) 0))
    
    ;; Clean up
    (delete-file local-file)))

(ert-deftest test-file-local-copy ()
  "Test file-local-copy functionality."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note")))
    (let ((local-copy (file-local-copy test-file)))
      (should (stringp local-copy))
      (should (file-exists-p local-copy))
      (should (> (nth 7 (file-attributes local-copy)) 0))
      (delete-file local-copy))))

(ert-deftest test-insert-file-contents ()
  "Test insert-file-contents functionality."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note")))
    (with-temp-buffer
      (insert-file-contents test-file nil 0 1000)
      (should (> (buffer-size) 0)))))

(ert-deftest test-file-download-performance ()
  "Test file download performance with size measurement."
  (let ((test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))
        (local-file "/tmp/supernote-perf-test.note"))
    
    ;; Clean up
    (when (file-exists-p local-file)
      (delete-file local-file))
    
    ;; Measure copy performance
    (let ((start-time (current-time)))
      (copy-file test-file local-file)
      (let ((end-time (current-time))
            (file-size (nth 7 (file-attributes local-file))))
        (should (file-exists-p local-file))
        (should (> file-size 0))
        ;; Performance should be reasonable (less than 10 seconds for any file)
        (should (< (float-time (time-subtract end-time start-time)) 10.0))
        
        ;; Clean up
        (delete-file local-file)))))

(provide 'test-readonly-and-copy-ert)
;;; test-readonly-and-copy-ert.el ends here
