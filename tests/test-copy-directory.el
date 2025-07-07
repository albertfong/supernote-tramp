;;; test-copy-directory.el --- Test recursive directory copying for supernote-tramp

(require 'ert)
(require 'supernote-tramp)

;; Load test configuration
(let ((config-file (expand-file-name "supernote-tramp-test-config.el" 
                                     (file-name-directory 
                                      (or load-file-name buffer-file-name)))))
  (when (file-exists-p config-file)
    (load config-file)))

(defun test-supernote-connection ()
  "Test basic connection to Supernote device."
  (let ((test-vec (make-tramp-file-name 
                   :method "supernote" 
                   :host supernote-tramp-test-server-ip 
                   :port supernote-tramp-test-server-port)))
    (condition-case err
        (supernote-tramp-get-file-list test-vec "/Note")
      (error (ert-fail (format "Failed to connect: %s" err))))))

(ert-deftest test-supernote-tramp-copy-directory-basic ()
  "Test basic directory copying functionality."
  (skip-unless (test-supernote-connection))
  
  (let* ((source-dir (supernote-tramp-test-path "/Note/Personal/Week Notes"))
         (dest-dir (make-temp-file "supernote-test-" t))
         (expected-dest (file-name-as-directory (expand-file-name "Week Notes" dest-dir))))
    
    (unwind-protect
        (progn
          ;; Test that source directory exists and is a directory
          (should (file-directory-p source-dir))
          
          ;; Test copy operation
          (supernote-tramp-handle-copy-directory source-dir dest-dir nil t nil)
          
          ;; Verify destination directory was created
          (should (file-directory-p expected-dest))
          
          ;; Verify files were copied
          (let ((dest-files (directory-files expected-dest nil nil t)))
            (should (> (length dest-files) 2)) ; More than just . and ..
            
            ;; Check that .note files were copied
            (should (seq-some (lambda (f) (string-suffix-p ".note" f)) dest-files))
            
            ;; Check that files have content (not empty)
            (let ((sample-file (seq-find (lambda (f) (string-suffix-p ".note" f)) dest-files)))
              (when sample-file
                (let ((file-path (expand-file-name sample-file expected-dest)))
                  (should (> (file-attribute-size (file-attributes file-path)) 0)))))))
      
      ;; Cleanup
      (when (file-exists-p dest-dir)
        (delete-directory dest-dir t)))))

(ert-deftest test-supernote-tramp-copy-directory-nested ()
  "Test copying nested directories."
  (skip-unless (test-supernote-connection))
  
  (let* ((source-dir (supernote-tramp-test-path "/Note/Personal"))
         (dest-dir (make-temp-file "supernote-nested-test-" t))
         (expected-dest (file-name-as-directory (expand-file-name "Personal" dest-dir))))
    
    (unwind-protect
        (progn
          ;; Test that source directory exists and is a directory
          (should (file-directory-p source-dir))
          
          ;; Test copy operation
          (supernote-tramp-handle-copy-directory source-dir dest-dir nil t nil)
          
          ;; Verify destination directory was created
          (should (file-directory-p expected-dest))
          
          ;; Verify subdirectories were copied
          (let ((dest-files (directory-files expected-dest nil nil t)))
            (should (> (length dest-files) 2)) ; More than just . and ..
            
            ;; Check that Week Notes subdirectory was copied
            (let ((week-notes-dir (expand-file-name "Week Notes" expected-dest)))
              (if (file-directory-p week-notes-dir)
                  (let ((week-notes-files (directory-files week-notes-dir nil nil t)))
                    (should (> (length week-notes-files) 2)) ; More than just . and ..
                    (should (seq-some (lambda (f) (string-suffix-p ".note" f)) week-notes-files)))
                ;; If Week Notes directory doesn't exist, check if there are any .note files in the root
                (should (seq-some (lambda (f) (string-suffix-p ".note" f)) dest-files))))))
      
      ;; Cleanup
      (when (file-exists-p dest-dir)
        (delete-directory dest-dir t)))))

(provide 'test-copy-directory)
;;; test-copy-directory.el ends here
