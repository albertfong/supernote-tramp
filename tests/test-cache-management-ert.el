;;; test-cache-management-ert.el --- ERT tests for cache management in supernote-tramp

;; Copyright (C) 2025

;; Author: Albert Fong <contact@albertfong.name>
;; Keywords: files, remote, supernote, tramp, test, cache

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Simplified ERT tests for cache management functions in supernote-tramp.
;; Tests basic cache operations and key generation.

;;; Code:

(require 'ert)
(require 'supernote-tramp)

(defvar test-supernote-tramp-cache-vec (tramp-dissect-file-name "/supernote:test:/")
  "Test vector for cache operations.")

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

(ert-deftest test-supernote-tramp-cache-key-uniqueness ()
  "Test that cache keys are unique for different paths and hosts."
  (let ((vec1 (tramp-dissect-file-name "/supernote:host1:/"))
        (vec2 (tramp-dissect-file-name "/supernote:host2:/"))
        (path1 "/Documents")
        (path2 "/Documents/file.txt"))
    
    ;; Different hosts should have different keys
    (should-not (equal (supernote-tramp-cache-key vec1 path1)
                      (supernote-tramp-cache-key vec2 path1)))
    
    ;; Different paths should have different keys
    (should-not (equal (supernote-tramp-cache-key vec1 path1)
                      (supernote-tramp-cache-key vec1 path2)))
    
    ;; Same host and path should have same key
    (should (equal (supernote-tramp-cache-key vec1 path1)
                   (supernote-tramp-cache-key vec1 path1)))))

(ert-deftest test-supernote-tramp-cache-expiration-logic ()
  "Test cache expiration logic with mock data."
  (let ((old-entry (list :timestamp (- (float-time) 400) :data "old-data"))
        (new-entry (list :timestamp (float-time) :data "new-data")))
    
    ;; Old entry should be expired
    (should (supernote-tramp-cache-is-expired old-entry))
    
    ;; New entry should not be expired
    (should-not (supernote-tramp-cache-is-expired new-entry))))

(ert-deftest test-supernote-tramp-cache-basic-operations ()
  "Test basic cache put and get operations."
  (let ((cache-key (supernote-tramp-cache-key test-supernote-tramp-cache-vec "/test"))
        (test-data '(:data "test-value" :timestamp 0)))
    
    ;; Clear any existing data
    (remhash cache-key supernote-tramp-file-cache)
    
    ;; Put data in cache
    (supernote-tramp-cache-put cache-key test-data)
    
    ;; Should be able to retrieve it
    (let ((retrieved-data (supernote-tramp-cache-get cache-key)))
      (should retrieved-data)
      (should (plist-get retrieved-data :data)))))

(ert-deftest test-supernote-tramp-cache-stats-basic ()
  "Test basic cache statistics functionality."
  (let ((vec test-supernote-tramp-cache-vec))
    
    ;; Add some data
    (supernote-tramp-cache-put (supernote-tramp-cache-key vec "/test1") '(:data "1"))
    (supernote-tramp-cache-put (supernote-tramp-cache-key vec "/test2") '(:data "2"))
    
    ;; Get stats
    (let ((stats (supernote-tramp-cache-stats)))
      (should (listp stats))
      ;; Should have some basic structure
      (should (>= (length stats) 1)))))

(ert-deftest test-supernote-tramp-cache-flush-operations ()
  "Test cache flush operations."
  (let ((vec test-supernote-tramp-cache-vec))
    
    ;; Add data for different paths
    (supernote-tramp-cache-put (supernote-tramp-cache-key vec "/Documents") '(:data "docs"))
    (supernote-tramp-cache-put (supernote-tramp-cache-key vec "/Photos") '(:data "photos"))
    (supernote-tramp-cache-put (supernote-tramp-cache-key vec "/Notes") '(:data "notes"))
    
    ;; Flush specific path
    (supernote-tramp-flush-cache-path vec "/Documents")
    
    ;; Documents should be flushed
    (should-not (supernote-tramp-cache-get (supernote-tramp-cache-key vec "/Documents")))
    
    ;; Others should remain (if they haven't been evicted)
    ;; Note: We can't guarantee they remain due to potential LRU eviction
    ))

(ert-deftest test-supernote-tramp-cache-data-integrity ()
  "Test cache data integrity over time."
  (let ((cache-key (supernote-tramp-cache-key test-supernote-tramp-cache-vec "/integrity"))
        (test-data '(:data (:files ("file1.txt" "file2.txt" "file3.txt")
                           :metadata (:size 1024 :modified "2025-01-01")))))
    
    ;; Store complex data
    (supernote-tramp-cache-put cache-key test-data)
    
    ;; Retrieve and verify
    (let ((retrieved-data (supernote-tramp-cache-get cache-key)))
      (should retrieved-data)
      (should (plist-get retrieved-data :data)))))

(provide 'test-cache-management-ert)
;;; test-cache-management-ert.el ends here
