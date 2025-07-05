;;; test-tramp-parsing.el --- Test TRAMP file name parsing

(add-to-list 'load-path ".")
(require 'supernote-tramp)

(message "=== Testing TRAMP file name parsing ===")

;; Test different file name formats
(let ((test-files '("/supernote:192.168.1.100:8089:/"
                   "/supernote:192.168.1.100#8089:/"
                   "/supernote:192.168.1.100:/")))
  
  (dolist (filename test-files)
    (message "\nTesting filename: %s" filename)
    (condition-case err
        (let ((vec (tramp-dissect-file-name filename)))
          (message "  Method: %S" (tramp-file-name-method vec))
          (message "  User: %S" (tramp-file-name-user vec))
          (message "  Host: %S" (tramp-file-name-host vec))
          (message "  Port: %S" (tramp-file-name-port vec))
          (message "  Localname: %S" (tramp-file-name-localname vec))
          (message "  Port from function: %S" (supernote-tramp-get-port vec)))
      (error (message "  Error parsing: %S" err)))))

;; Test the correct format
(message "\n=== Testing with correct format ===")
(let* ((filename "/supernote:192.168.1.100#8089:/")
       (vec (tramp-dissect-file-name filename)))
  (message "Testing: %s" filename)
  (message "Vector: %S" vec)
  
  ;; Test file operations
  (message "\nTesting file operations...")
  (message "Exists: %S" (supernote-tramp-handle-file-exists-p filename))
  (message "Is directory: %S" (supernote-tramp-handle-file-directory-p filename))
  
  ;; Test directory listing
  (condition-case err
      (let ((files (supernote-tramp-handle-directory-files filename)))
        (message "Directory files: %S" files))
    (error (message "Directory files error: %S" err))))

(message "\n=== Test complete ===")
