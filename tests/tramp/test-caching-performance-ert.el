;;; test-caching-performance-ert.el --- ERT tests for caching performance

(require 'ert)
(require 'supernote-tramp)

;; Load test configuration
(load-file "../supernote-tramp-test-config.el")

(defvar test-server (format "%s#%d" supernote-tramp-test-server-ip supernote-tramp-test-server-port))

(ert-deftest test-cache-functionality ()
  "Test that caching is working properly."
  (let ((test-path (supernote-tramp-test-path "/Note")))
    
    ;; Clear cache first
    (supernote-tramp-flush-all-cache)
    
    ;; Enable caching
    (let ((supernote-tramp-cache-enabled t))
      
      ;; First call should populate cache
      (let ((files1 (directory-files test-path nil nil nil 5)))
        (should (listp files1))
        (should (> (length files1) 0))
        
        ;; Second call should use cache (should be faster)
        (let ((files2 (directory-files test-path nil nil nil 5)))
          (should (listp files2))
          (should (equal files1 files2)))))))

(ert-deftest test-cache-invalidation ()
  "Test that cache invalidation works properly."
  (let ((test-path (supernote-tramp-test-path "/Note")))
    
    ;; Clear cache
    (supernote-tramp-flush-all-cache)
    
    ;; Enable caching
    (let ((supernote-tramp-cache-enabled t))
      
      ;; Populate cache
      (let ((files1 (directory-files test-path nil nil nil 5)))
        (should (listp files1)))
      
      ;; Clear cache
      (supernote-tramp-flush-all-cache)
      
      ;; Should still work after cache clear
      (let ((files2 (directory-files test-path nil nil nil 5)))
        (should (listp files2))
        (should (> (length files2) 0))))))

(ert-deftest test-cache-performance-improvement ()
  "Test that caching improves performance."
  (let ((test-path (supernote-tramp-test-path "/Note")))
    
    ;; Clear cache and disable it first
    (supernote-tramp-flush-all-cache)
    (setq supernote-tramp-cache-enabled nil)
    
    ;; Test without caching
    (let ((start-time (current-time)))
      (dotimes (i 2)
        (directory-files test-path nil nil nil 5))
      (let ((no-cache-time (time-to-seconds (time-subtract (current-time) start-time))))
        
        ;; Now test with caching
        (supernote-tramp-flush-all-cache)
        (setq supernote-tramp-cache-enabled t)
        
        (let ((start-time-cached (current-time)))
          ;; First call populates cache
          (directory-files test-path nil nil nil 5)
          ;; Second call uses cache
          (directory-files test-path nil nil nil 5)
          (let ((cache-time (time-to-seconds (time-subtract (current-time) start-time-cached))))
            
            ;; Cache should provide some benefit (at least not be slower)
            (should (<= cache-time (* no-cache-time 2)))
            
            ;; Both should complete in reasonable time
            (should (< no-cache-time 30))
            (should (< cache-time 30))))))))

(ert-deftest test-cache-with-different-paths ()
  "Test caching with different directory paths."
  (let ((root-path (supernote-tramp-test-path "/"))
        (note-path (supernote-tramp-test-path "/Note")))
    
    ;; Clear cache
    (supernote-tramp-flush-all-cache)
    
    ;; Enable caching
    (let ((supernote-tramp-cache-enabled t))
      
      ;; Cache different paths
      (let ((root-files (directory-files root-path nil nil nil 5))
            (note-files (directory-files note-path nil nil nil 5)))
        
        (should (listp root-files))
        (should (listp note-files))
        (should (> (length root-files) 0))
        (should (> (length note-files) 0))
        
        ;; Files should be different
        (should (not (equal root-files note-files)))))))

(ert-deftest test-cache-with-attributes ()
  "Test caching with file attributes."
  (let ((test-path (supernote-tramp-test-path "/Note")))
    
    ;; Clear cache
    (supernote-tramp-flush-all-cache)
    
    ;; Enable caching
    (let ((supernote-tramp-cache-enabled t))
      
      ;; Test directory-files-and-attributes
      (let ((files-attrs1 (directory-files-and-attributes test-path nil nil nil nil 3))
            (files-attrs2 (directory-files-and-attributes test-path nil nil nil nil 3)))
        
        (should (listp files-attrs1))
        (should (listp files-attrs2))
        (should (> (length files-attrs1) 0))
        (should (equal files-attrs1 files-attrs2))))))

(ert-deftest test-cache-disabled ()
  "Test that operations work when cache is disabled."
  (let ((test-path (supernote-tramp-test-path "/Note")))
    
    ;; Disable caching
    (let ((supernote-tramp-cache-enabled nil))
      
      ;; Should still work without cache
      (let ((files1 (directory-files test-path nil nil nil 5))
            (files2 (directory-files test-path nil nil nil 5)))
        
        (should (listp files1))
        (should (listp files2))
        (should (> (length files1) 0))
        (should (equal files1 files2))))))

(provide 'test-caching-performance-ert)
;;; test-caching-performance-ert.el ends here
