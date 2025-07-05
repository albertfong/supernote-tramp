;;; test-json-conversion.el --- Test JSON vector to list conversion

(require 'ert)
(require 'json)

;; Load the main package to get the conversion function
(add-to-list 'load-path (file-name-directory (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "../supernote-tramp.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest test-supernote-tramp-json-to-list-simple-vector ()
  "Test conversion of simple JSON vector to list."
  (let* ((json-string "[1, 2, 3]")
         (parsed (json-read-from-string json-string))
         (converted (supernote-tramp-json-to-list parsed)))
    (should (vectorp parsed))  ; Original should be vector
    (should (listp converted))  ; Converted should be list
    (should (equal converted '(1 2 3)))))

(ert-deftest test-supernote-tramp-json-to-list-nested-structure ()
  "Test conversion of nested JSON structure with vectors."
  (let* ((json-string "{\"fileList\": [{\"name\": \"test\", \"isDirectory\": true}]}")
         (parsed (json-read-from-string json-string))
         (converted (supernote-tramp-json-to-list parsed)))
    ;; Check that fileList is converted from vector to list
    (let ((original-filelist (cdr (assoc 'fileList parsed)))
          (converted-filelist (cdr (assoc 'fileList converted))))
      (should (vectorp original-filelist))
      (should (listp converted-filelist))
      (should (equal (length original-filelist) (length converted-filelist))))))

(ert-deftest test-supernote-tramp-json-to-list-supernote-response ()
  "Test conversion of typical Supernote JSON response."
  (let* ((json-string "{\"deviceName\":\"TestDevice\",\"fileList\":[{\"name\":\"Document\",\"isDirectory\":true,\"size\":0},{\"name\":\"test.note\",\"isDirectory\":false,\"size\":1024}],\"totalByteSize\":1024}")
         (parsed (json-read-from-string json-string))
         (converted (supernote-tramp-json-to-list parsed)))
    ;; Verify structure
    (should (assoc 'deviceName converted))
    (should (assoc 'fileList converted))
    (should (assoc 'totalByteSize converted))
    
    ;; Verify fileList is converted to list
    (let ((filelist (cdr (assoc 'fileList converted))))
      (should (listp filelist))
      (should (= (length filelist) 2))
      
      ;; Check first file
      (let ((first-file (car filelist)))
        (should (listp first-file))
        (should (equal (cdr (assoc 'name first-file)) "Document"))
        (should (equal (cdr (assoc 'isDirectory first-file)) t))))))

(ert-deftest test-supernote-tramp-json-to-list-preserves-non-vectors ()
  "Test that non-vector data is preserved unchanged."
  (let* ((data '((name . "test") (size . 100) (nested . ((key . "value")))))
         (converted (supernote-tramp-json-to-list data)))
    (should (equal data converted))))

(provide 'test-json-conversion)
;;; test-json-conversion.el ends here
