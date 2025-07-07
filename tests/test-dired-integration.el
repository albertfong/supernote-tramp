;;; test-dired-integration.el --- Integration test for dired directory copying

(require 'ert)
(require 'supernote-tramp)

;; Load test configuration
(let ((config-file (expand-file-name "supernote-tramp-test-config.el" 
                                     (file-name-directory 
                                      (or load-file-name buffer-file-name)))))
  (when (file-exists-p config-file)
    (load config-file)))

(ert-deftest test-supernote-tramp-dired-integration ()
  "Test that dired directory copying works end-to-end."
  (let ((source-dir (supernote-tramp-test-path "/Note/Personal/Week Notes")))
    (skip-unless (and (file-exists-p source-dir)
                      (file-directory-p source-dir)))
    
    (let* ((dest-dir (make-temp-file "dired-integration-test-" t))
           (expected-dest (file-name-as-directory (expand-file-name "Week Notes" dest-dir))))
    
      (unwind-protect
          (progn
            ;; Test the copy-directory operation directly (what dired calls)
            (copy-directory source-dir dest-dir nil t)
            
            ;; Verify destination directory was created
            (should (file-directory-p expected-dest))
            
            ;; Verify files were copied with correct content
            (let ((dest-files (directory-files expected-dest nil "\\.note$" t)))
              (should (> (length dest-files) 0))
              
              ;; Check that at least one file has reasonable content
              (let ((sample-file (car dest-files)))
                (let ((file-path (expand-file-name sample-file expected-dest)))
                  (should (> (file-attribute-size (file-attributes file-path)) 100))
                  ;; Verify it's a real note file by checking it has some content
                  (with-temp-buffer
                    (insert-file-contents file-path nil 0 1000)
                    (should (> (buffer-size) 50)))))))
        
        ;; Cleanup
        (when (file-exists-p dest-dir)
          (delete-directory dest-dir t))))))

(provide 'test-dired-integration)
;;; test-dired-integration.el ends here
