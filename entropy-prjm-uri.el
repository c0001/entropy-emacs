(require 'url)

(defun entropy/prjm-uri-open (uri-string db-location &optional http-try)
  (let* ((uri (url-generic-parse-url uri-string))
         (uri-scheme (url-type uri))
         (uri-host (url-host uri))
         (uri-filename (url-filename uri)))
    (cond
     ((and (equal "file" uri-scheme)
           (equal uri-host "localhost"))
      (let ((file uri-filename))
        (if (file-exists-p file)
            (cond ((file-directory-p file)
                   (dired file))
                  (t (find-file file)))
          (error "FILE '%s' not exist." file))))
     ((and (not uri-host)
           (not uri-scheme))
      (entrop/prjm--uri-open-relative-local-file uri-filename db-location))
     ((or (equal "http" uri-scheme)
          (equal "https" uri-scheme))
      (browse-url uri-string))
     (t
      (error "<<entropy-prjm>>: Uri unsupported '%s' ." uri-string)))))


(defun entrop/prjm--uri-open-relative-local-file (uri-filename db-location)
  (unless (and (stringp uri-filename)
               (not (equal "" uri-filename)))
    (error "<<entrop/prjm--uri-open-relative-local-file>>:
This is not valid uri-filename!"))
  (let ((relative-p (or (string-match "^\\.\\./" uri-filename)
                        (string-match "^\\./" uri-filename)
                        (string-match "^~/" uri-filename)))
        (db-dir (file-name-directory db-location))
        open-func)
    (setq open-func
          (lambda (file-path)
            (if (file-exists-p file-path)
                (cond ((file-directory-p file-path)
                       (dired file-path))
                      (t
                       (find-file file-path)))
              (error "FILE '%s' not exists." file-path))))
    (unless relative-p
      (error "<<entrop/prjm--uri-open-relative-local-file>>:
Relative path value invalid '%s'." uri-filename))
    (cond ((string-match "^\\.\\./" uri-filename)
           (funcall open-func
                    (expand-file-name
                     uri-filename db-dir)))
          ((string-match "^\\./" uri-filename)
           (funcall open-func
                    (expand-file-name
                     uri-filename db-dir)))
          ((string-match "^~/" uri-filename)
           (funcall open-func
                    (expand-file-name
                     uri-filename))))))




(provide 'entropy-prjm-uri)
