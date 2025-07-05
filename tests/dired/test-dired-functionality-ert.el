;;; test-dired-functionality.el --- ERT tests for dired functionality

(require 'ert)
(require 'supernote-tramp)
(require 'dired)

;; Load test configuration
(load-file "../supernote-tramp-test-config.el")

(defvar test-server (format "%s#%d" supernote-tramp-test-server-ip supernote-tramp-test-server-port))
(defvar test-dir (supernote-tramp-test-path "/Note"))
(defvar test-file (supernote-tramp-test-path "/Note/20250701_081047.note"))

(ert-deftest test-dired-basic-directory-listing ()
  "Test basic directory listing functionality."
  (let ((files (directory-files test-dir)))
    (should (listp files))
    (should (> (length files) 0))
    (should (member "." files))
    (should (member ".." files))))

(ert-deftest test-dired-directory-files-and-attributes ()
  "Test directory-files-and-attributes functionality."
  (let ((files-attrs (directory-files-and-attributes test-dir nil nil nil nil 3)))
    (should (listp files-attrs))
    (should (> (length files-attrs) 0))
    (should (cl-every (lambda (entry) (and (stringp (car entry)) (listp (cdr entry)))) files-attrs))))

(ert-deftest test-dired-file-attributes ()
  "Test file-attributes functionality."
  (let ((attrs (file-attributes test-file)))
    (should (listp attrs))
    (should (not (car attrs))) ; not a directory
    (should (> (nth 7 attrs) 0)))) ; file size > 0

(ert-deftest test-dired-file-directory-p ()
  "Test file-directory-p functionality."
  (should (file-directory-p test-dir))
  (should (not (file-directory-p test-file))))

(ert-deftest test-dired-file-exists-p ()
  "Test file-exists-p functionality."
  (should (file-exists-p test-dir))
  (should (file-exists-p test-file)))

(ert-deftest test-dired-file-readable-p ()
  "Test file-readable-p functionality."
  (should (file-readable-p test-file))
  (should (file-readable-p test-dir)))

(ert-deftest test-dired-file-writable-p ()
  "Test file-writable-p functionality - should be nil for read-only filesystem."
  (should (not (file-writable-p test-file)))
  (should (not (file-writable-p test-dir))))

(ert-deftest test-dired-file-modes ()
  "Test file-modes functionality."
  (let ((modes (file-modes test-file)))
    (should (integerp modes))
    (should (> modes 0))))

(ert-deftest test-dired-file-newer-than-file-p ()
  "Test file-newer-than-file-p functionality."
  (should (not (file-newer-than-file-p test-file test-file))))

(ert-deftest test-dired-file-regular-p ()
  "Test file-regular-p functionality."
  (should (file-regular-p test-file))
  (should (not (file-regular-p test-dir))))

(ert-deftest test-dired-file-symlink-p ()
  "Test file-symlink-p functionality."
  (should (not (file-symlink-p test-file)))
  (should (not (file-symlink-p test-dir))))

(ert-deftest test-dired-copy-file ()
  "Test copy-file functionality."
  (let ((local-file "/tmp/supernote-test-copy.note"))
    (when (file-exists-p local-file)
      (delete-file local-file))
    (copy-file test-file local-file)
    (should (file-exists-p local-file))
    (should (> (nth 7 (file-attributes local-file)) 0))
    (delete-file local-file)))

(ert-deftest test-dired-file-local-copy ()
  "Test file-local-copy functionality."
  (let ((local-copy (file-local-copy test-file)))
    (should (stringp local-copy))
    (should (file-exists-p local-copy))
    (should (> (nth 7 (file-attributes local-copy)) 0))
    (delete-file local-copy)))

(ert-deftest test-dired-insert-file-contents ()
  "Test insert-file-contents functionality."
  (with-temp-buffer
    (insert-file-contents test-file nil 0 100)
    (should (> (buffer-size) 0))))

(ert-deftest test-dired-blocked-write-operations ()
  "Test that write operations are properly blocked."
  (should-error (write-region "test" nil test-file))
  (should-error (delete-file test-file))
  (should-error (rename-file test-file (concat test-file ".bak")))
  (should-error (make-directory (concat test-dir "/test-dir")))
  (should-error (make-symbolic-link test-file (concat test-file ".link")))
  (should-error (delete-directory test-dir)))

(ert-deftest test-dired-file-name-completion ()
  "Test file-name-completion functionality."
  (let ((completions (file-name-all-completions "20250" test-dir)))
    (should (listp completions))
    (should (> (length completions) 0)))
  (let ((completion (file-name-completion "20250" test-dir)))
    (should (stringp completion))))

(ert-deftest test-dired-expand-file-name ()
  "Test expand-file-name functionality."
  (let ((expanded (expand-file-name "test.note" test-dir)))
    (should (stringp expanded))
    (should (string-match "/Note/test.note" expanded))))

(ert-deftest test-dired-file-truename ()
  "Test file-truename functionality."
  (let ((truename (file-truename test-file)))
    (should (stringp truename))
    (should (string= truename test-file))))

(ert-deftest test-dired-file-remote-p ()
  "Test file-remote-p functionality."
  (should (file-remote-p test-file))
  (should (string-match "supernote:" (file-remote-p test-file))))

(ert-deftest test-dired-file-accessible-directory-p ()
  "Test file-accessible-directory-p functionality."
  (should (file-accessible-directory-p test-dir))
  (should (not (file-accessible-directory-p test-file))))

(ert-deftest test-dired-substitute-in-file-name ()
  "Test substitute-in-file-name functionality."
  (let ((substituted (substitute-in-file-name test-file)))
    (should (stringp substituted))
    (should (string= substituted test-file))))

(ert-deftest test-dired-insert-directory ()
  "Test insert-directory functionality."
  (with-temp-buffer
    (insert-directory test-dir "-la")
    (should (> (buffer-size) 0))
    (should (string-match "20250" (buffer-string)))))

(provide 'test-dired-functionality)
;;; test-dired-functionality.el ends here
