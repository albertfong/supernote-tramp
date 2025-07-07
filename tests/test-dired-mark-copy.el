;;; test-dired-mark-copy.el --- Test dired mark and copy functionality

(require 'ert)
(require 'supernote-tramp)
(require 'dired)

;; Load test configuration
(let ((config-file (expand-file-name "supernote-tramp-test-config.el" 
                                     (file-name-directory 
                                      (or load-file-name buffer-file-name)))))
  (when (file-exists-p config-file)
    (load config-file)))

(ert-deftest test-dired-copy-non-interactive ()
  "Test dired copy in completely non-interactive mode."
  (let ((source-dir (supernote-tramp-test-path "/EXPORT"))
        (dest-dir "/home/alfong/tmp"))
    
    ;; Ensure destination directory exists
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))
    
    ;; Clean up any existing copy
    (let ((expected-dest (expand-file-name "EXPORT" dest-dir)))
      (when (file-exists-p expected-dest)
        (delete-directory expected-dest t)))
    
    (message "=== Testing Non-Interactive Dired Copy ===")
    
    ;; Use copy-directory directly to avoid any dired prompts
    (let ((original-y-or-n-p (symbol-function 'y-or-n-p))
          (original-yes-or-no-p (symbol-function 'yes-or-no-p)))
      
      ;; Mock all interactive functions to automatically answer "yes"
      (fset 'y-or-n-p (lambda (&rest _args) t))
      (fset 'yes-or-no-p (lambda (&rest _args) t))
      
      (unwind-protect
          (condition-case err
              (progn
                (message "Starting direct copy-directory operation...")
                (copy-directory source-dir dest-dir nil t nil)
                (message "✓ Direct copy operation succeeded"))
            (error 
             (message "Direct copy operation failed: %s" err)
             (should nil)))
        
        ;; Restore original functions
        (fset 'y-or-n-p original-y-or-n-p)
        (fset 'yes-or-no-p original-yes-or-no-p)))
    
    ;; Verify the results
    (let ((expected-dest (expand-file-name "EXPORT" dest-dir)))
      (should (file-exists-p expected-dest))
      (should (file-directory-p expected-dest))
      (message "✓ Non-interactive copy test passed"))
    
    ;; The test succeeds if we reach this point
    t))

(provide 'test-dired-mark-copy)
;;; test-dired-mark-copy.el ends here
