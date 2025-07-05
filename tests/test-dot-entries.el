;;; test-dot-entries.el --- Test "." and ".." entries in dired

(require 'ert)
(require 'json)

;; Load the main package
(add-to-list 'load-path (file-name-directory (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "../supernote-tramp.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest test-dot-entries-exist-in-directory-files ()
  "Test that '.' and '..' entries are included in directory-files."
  (let ((vec (tramp-dissect-file-name "/supernote:192.168.20.170:/"))
        (files nil))
    (with-temp-buffer
      ;; Mock the API response for root directory
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   '((code . "200")
                     (fileList . [((name . "Documents") (isDirectory . t) (size . 0) (date . "2024-01-01T00:00:00Z"))
                                  ((name . "Note.note") (isDirectory . nil) (size . 1024) (date . "2024-01-01T00:00:00Z"))])))))
        (setq files (supernote-tramp-handle-directory-files "/supernote:192.168.20.170:/" nil nil nil))
        (should (member "." files))
        (should (member ".." files))
        (should (member "Documents" files))
        (should (member "Note.note" files))))))

(ert-deftest test-dot-entries-file-exists-p ()
  "Test that file-exists-p returns t for '.' and '..' entries."
  (let ((vec (tramp-dissect-file-name "/supernote:192.168.20.170:/"))
        (dir "/supernote:192.168.20.170:/"))
    (with-temp-buffer
      ;; Mock the API response
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   '((code . "200")
                     (fileList . [((name . "Documents") (isDirectory . t) (size . 0) (date . "2024-01-01T00:00:00Z"))])))))
        (should (supernote-tramp-handle-file-exists-p (concat dir ".")))
        (should (supernote-tramp-handle-file-exists-p (concat dir "..")))))))

(ert-deftest test-dot-entries-file-directory-p ()
  "Test that file-directory-p returns t for '.' and '..' entries."
  (let ((vec (tramp-dissect-file-name "/supernote:192.168.20.170:/"))
        (dir "/supernote:192.168.20.170:/"))
    (with-temp-buffer
      ;; Mock the API response
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   '((code . "200")
                     (fileList . [((name . "Documents") (isDirectory . t) (size . 0) (date . "2024-01-01T00:00:00Z"))])))))
        (should (supernote-tramp-handle-file-directory-p (concat dir ".")))
        (should (supernote-tramp-handle-file-directory-p (concat dir "..")))))))

(ert-deftest test-dot-entries-file-attributes ()
  "Test that file-attributes returns proper attributes for '.' and '..' entries."
  (let ((vec (tramp-dissect-file-name "/supernote:192.168.20.170:/"))
        (dir "/supernote:192.168.20.170:/"))
    (with-temp-buffer
      ;; Mock the API response
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   '((code . "200")
                     (fileList . [((name . "Documents") (isDirectory . t) (size . 0) (date . "2024-01-01T00:00:00Z"))])))))
        (let ((dot-attrs (supernote-tramp-handle-file-attributes (concat dir ".")))
              (dotdot-attrs (supernote-tramp-handle-file-attributes (concat dir ".."))))
          (should dot-attrs)
          (should dotdot-attrs)
          ;; Both should be directories (first element should be t)
          (should (eq (car dot-attrs) t))
          (should (eq (car dotdot-attrs) t))
          ;; Both should have directory mode string
          (should (string= (nth 8 dot-attrs) "dr-xr-xr-x"))
          (should (string= (nth 8 dotdot-attrs) "dr-xr-xr-x")))))))

(ert-deftest test-dot-entries-insert-directory ()
  "Test that '.' and '..' entries appear in insert-directory output (used by dired)."
  (let ((vec (tramp-dissect-file-name "/supernote:192.168.20.170:/"))
        (output nil))
    (with-temp-buffer
      ;; Mock the API response for root directory
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   ;; Use supernote-tramp-json-to-list to convert vectors to lists
                   (supernote-tramp-json-to-list
                    '((code . "200")
                      (fileList . [((name . "Documents") (isDirectory . t) (size . 0) (date . "2024-01-01T00:00:00Z"))
                                   ((name . "Note.note") (isDirectory . nil) (size . 1024) (date . "2024-01-01T00:00:00Z"))]))))))
        ;; Call insert-directory and capture output
        (supernote-tramp-handle-insert-directory "/supernote:192.168.20.170:/")
        (setq output (buffer-string))
        
        ;; Verify dot entries are present
        (should (string-match-p "^dr-xr-xr-x.*\\.$" output))  ; "." entry
        (should (string-match-p "^dr-xr-xr-x.*\\.\\.$" output))  ; ".." entry
        
        ;; Verify regular files are also present
        (should (string-match-p "Documents" output))
        (should (string-match-p "Note.note" output))
        
        ;; Verify dot entries come first
        (let ((dot-pos (string-match "\\.$" output))
              (dotdot-pos (string-match "\\.\\.$" output))
              (docs-pos (string-match "Documents" output)))
          (should (< dot-pos docs-pos))
          (should (< dotdot-pos docs-pos)))))))

(ert-deftest test-dot-entries-live-functionality ()
  "Test dot entries functionality with live server (if configured)."
  ;; Only run if test configuration is available
  (when (and (boundp 'supernote-tramp-test-server-ip)
             (boundp 'supernote-tramp-test-server-port)
             supernote-tramp-test-server-ip
             supernote-tramp-test-server-port)
    (let ((test-dir (format "/supernote:%s#%d:/" 
                           supernote-tramp-test-server-ip 
                           supernote-tramp-test-server-port)))
      ;; Only run if we can connect to the test server
      (condition-case nil
          (when (supernote-tramp-handle-file-exists-p test-dir)
            ;; Test directory-files includes dot entries
            (let ((files (supernote-tramp-handle-directory-files test-dir)))
              (should (member "." files))
              (should (member ".." files)))
            
            ;; Test insert-directory includes dot entries
            (with-temp-buffer
              (supernote-tramp-handle-insert-directory test-dir)
              (let ((output (buffer-string)))
                (should (string-match-p "^dr-xr-xr-x.*\\.$" output))
                (should (string-match-p "^dr-xr-xr-x.*\\.\\.$" output))))
            
            ;; Test file-exists-p and file-directory-p for dot entries
            (should (supernote-tramp-handle-file-exists-p (concat test-dir ".")))
            (should (supernote-tramp-handle-file-exists-p (concat test-dir "..")))
            (should (supernote-tramp-handle-file-directory-p (concat test-dir ".")))
            (should (supernote-tramp-handle-file-directory-p (concat test-dir ".."))))
        ;; Skip test if server is not available
        (error 
         (message "Skipping live dot entries test - server not available"))))))

(ert-deftest test-dot-entries-insert-directory-output ()
  "Test that insert-directory includes dot entries in its output."
  (let ((vec (tramp-dissect-file-name "/supernote:test-host:/"))
        (output ""))
    (with-temp-buffer
      ;; Mock the API response for any directory
      (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
                 (lambda (vec path)
                   (list (cons 'fileList 
                              (supernote-tramp-json-to-list
                               [((name . "SomeFile.note") 
                                 (isDirectory . nil) 
                                 (size . 1024) 
                                 (date . "2024-01-01T00:00:00Z"))
                                ((name . "SomeDirectory") 
                                 (isDirectory . t) 
                                 (size . 0) 
                                 (date . "2024-01-01T00:00:00Z"))]))))))
        
        ;; Call insert-directory and capture output
        (supernote-tramp-handle-insert-directory "/supernote:test-host:/")
        (setq output (buffer-string))
        
        ;; Verify dot entries are present
        (should (string-match-p "\\." output))
        (should (string-match-p "\\.\\." output))
        
        ;; Verify they appear as directories (dr-xr-xr-x)
        (should (string-match-p "dr-xr-xr-x.*\\.$" output))
        (should (string-match-p "dr-xr-xr-x.*\\.\\.$" output))
        
        ;; Verify actual files are also present
        (should (string-match-p "SomeFile\\.note" output))
        (should (string-match-p "SomeDirectory" output))))))

(ert-deftest test-dot-entries-dired-integration ()
  "Test that dot entries work properly in dired context."
  (let ((vec (tramp-dissect-file-name "/supernote:test-host:/test-path/")))
    ;; Mock the API response
    (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
               (lambda (vec path)
                 (list (cons 'fileList 
                            (supernote-tramp-json-to-list
                             [((name . "file1.txt") 
                               (isDirectory . nil) 
                               (size . 500) 
                               (date . "2024-01-01T00:00:00Z"))]))))))
      
      ;; Test directory-files includes dot entries
      (let ((files (supernote-tramp-handle-directory-files "/supernote:test-host:/test-path/")))
        (should (member "." files))
        (should (member ".." files))
        (should (member "file1.txt" files)))
      
      ;; Test directory-files-and-attributes includes dot entries
      (let ((files-attrs (supernote-tramp-handle-directory-files-and-attributes "/supernote:test-host:/test-path/")))
        (should (assoc "." files-attrs))
        (should (assoc ".." files-attrs))
        (should (assoc "file1.txt" files-attrs)))
      
      ;; Test that dot entries are recognized as existing and as directories
      (should (supernote-tramp-handle-file-exists-p "/supernote:test-host:/test-path/."))
      (should (supernote-tramp-handle-file-exists-p "/supernote:test-host:/test-path/.."))
      (should (supernote-tramp-handle-file-directory-p "/supernote:test-host:/test-path/."))
      (should (supernote-tramp-handle-file-directory-p "/supernote:test-host:/test-path/..")))))

(ert-deftest test-dot-entries-with-empty-directory ()
  "Test that dot entries work even in empty directories."
  (let ((vec (tramp-dissect-file-name "/supernote:test-host:/empty/")))
    ;; Mock empty directory response
    (cl-letf (((symbol-function 'supernote-tramp-get-file-list)
               (lambda (vec path)
                 (list (cons 'fileList '())))))  ; Empty file list
      
      ;; Even empty directories should have dot entries
      (let ((files (supernote-tramp-handle-directory-files "/supernote:test-host:/empty/")))
        (should (member "." files))
        (should (member ".." files))
        (should (= (length files) 2)))  ; Only dot entries
      
      ;; Test insert-directory with empty directory
      (with-temp-buffer
        (supernote-tramp-handle-insert-directory "/supernote:test-host:/empty/")
        (let ((output (buffer-string)))
          (should (string-match-p "\\.$" output))
          (should (string-match-p "\\.\\.$" output)))))))
