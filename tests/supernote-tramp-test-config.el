;;; supernote-tramp-test-config.el --- Test configuration for TRAMP Supernote

;; Configuration for TRAMP Supernote tests
;; This file contains test parameters that can be customized for different environments

;;; Code:

(defvar supernote-tramp-test-server-ip "192.168.20.170"
  "IP address of the Supernote device for integration tests.
Set this to your actual Supernote device IP address.")

(defvar supernote-tramp-test-server-port 8089
  "Port number of the Supernote device HTTP server.
Supernote devices typically use port 8089 for HTTP access.")

(defvar supernote-tramp-test-timeout 30
  "Timeout in seconds for network operations in tests.")

(defvar supernote-tramp-test-enable-live-tests t
  "Whether to enable live integration tests.
Set to t to run tests against actual Supernote device.
Warning: Only enable this if you have a Supernote device available.")

(defvar supernote-tramp-test-temp-directory "/tmp/supernote-tramp-test"
  "Temporary directory for test files.")

(defvar supernote-tramp-test-verbose nil
  "Whether to enable verbose output in tests.")

(defun supernote-tramp-test-server-url ()
  "Get the full server URL for tests."
  (format "http://%s:%d" supernote-tramp-test-server-ip supernote-tramp-test-server-port))

(defun supernote-tramp-test-path (path)
  "Get a test path for the configured server."
  (format "/supernote:%s:%s" supernote-tramp-test-server-ip path))

(defun supernote-tramp-test-setup ()
  "Setup test environment."
  (when supernote-tramp-test-verbose
    (message "Setting up TRAMP Supernote test environment..."))
  
  ;; Create temp directory
  (unless (file-exists-p supernote-tramp-test-temp-directory)
    (make-directory supernote-tramp-test-temp-directory t))
  
  ;; Load supernote-tramp if not already loaded
  (unless (featurep 'supernote-tramp)
    (let ((supernote-tramp-file (expand-file-name "supernote-tramp.el" 
                                                  (file-name-directory 
                                                   (or load-file-name buffer-file-name)))))
      (if (file-exists-p supernote-tramp-file)
          (load-file supernote-tramp-file)
        (error "Could not find supernote-tramp.el"))))
  
  (when supernote-tramp-test-verbose
    (message "Test environment setup complete")))

(defun supernote-tramp-test-cleanup ()
  "Cleanup test environment."
  (when supernote-tramp-test-verbose
    (message "Cleaning up TRAMP Supernote test environment..."))
  
  ;; Clear caches
  (when (fboundp 'supernote-tramp-clear-cache)
    (supernote-tramp-clear-cache))
  
  ;; Clean up temp directory
  (when (file-exists-p supernote-tramp-test-temp-directory)
    (delete-directory supernote-tramp-test-temp-directory t))
  
  (when supernote-tramp-test-verbose
    (message "Test environment cleanup complete")))

(defun supernote-tramp-test-skip-if-no-server ()
  "Skip test if live server tests are not enabled."
  (unless supernote-tramp-test-enable-live-tests
    (ert-skip "Live server tests disabled. Set supernote-tramp-test-enable-live-tests to t to enable.")))

(defmacro supernote-tramp-test-with-server (title &rest body)
  "Execute BODY only if live server tests are enabled."
  `(ert-deftest ,(intern title) ()
     ,(format "Test %s (requires live server)" title)
     (supernote-tramp-test-skip-if-no-server)
     (supernote-tramp-test-setup)
     (unwind-protect
         (progn ,@body)
       (supernote-tramp-test-cleanup))))

(provide 'supernote-tramp-test-config)
;;; supernote-tramp-test-config.el ends here
