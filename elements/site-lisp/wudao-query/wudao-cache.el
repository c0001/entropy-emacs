;; -*- lexical-binding: t; -*-
(require 'wudao-lib)

(defvar wudao/cache--host-dir
  (file-name-directory load-file-name))

(defvar wudao/cache--dicts-cache-tarball
  (expand-file-name
   "wudao_dict.cache.tgz"
   wudao/cache--host-dir))
(defvar wudao/cache--dicts-extract-host
  (expand-file-name
   "dict"
   wudao/cache--host-dir))

(defvar wudao/cache--cache-file-full
  (expand-file-name "query.cache" wudao/cache--dicts-extract-host))
(defvar wudao/cache--hash-file-full
  (expand-file-name "hash-full.txt" wudao/cache--dicts-extract-host))
(defvar wudao/cache--cache-file-desc
  (expand-file-name "query.cache_desc" wudao/cache--dicts-extract-host))
(defvar wudao/cache--hash-file-desc
  (expand-file-name "hash-desc.txt" wudao/cache--dicts-extract-host))

(defun wudao/cache--extract-dicts ()
  (let ((cf wudao/cache--cache-file-full)
        (cd wudao/cache--cache-file-desc))
    (unless (and (file-exists-p cf)
                 (file-exists-p cd))
      (wudao/lib-archive-dowith
       'tgz wudao/cache--dicts-cache-tarball
       wudao/cache--dicts-extract-host
       :extract))))

(defvar wudao/cache--head-regx
  (rx "**********<<"
      (group (+? ascii))
      ">>**********"))

(defvar wudao/cache--tail-regx
  (rx "<<********************>>"))

(defvar wudao/cache--dict-cache-list nil)

(defvar wudao/cache--hashed-full nil)
(defvar wudao/cache--hashed-desc nil)

(defun wudao/cache--get-region ()
  (let (word def beg end)
    (unless (looking-at wudao/cache--head-regx)
      (re-search-forward wudao/cache--head-regx)
      (forward-line 0))
    (setq word (match-string 1))
    (forward-line 1)
    (setq beg (point))
    (re-search-forward wudao/cache--tail-regx)
    (forward-line -1)
    (end-of-line)
    (setq end (point))
    (setq def
          (buffer-substring
           beg end))
    (push (cons word (if (> end beg)
                         (if (string-match-p "^No such word:" def) "===NOFOUND===\n\n" def)
                       "===Empty===\n\n"))
          wudao/cache--dict-cache-list)
    (when noninteractive
      (wudao/lib-message "pick '%s'" word))
    t))

(defun wudao/cache--print-item ()
  (insert ";; Do not modify\n")
  (insert "(\n")
  (dolist (el (reverse wudao/cache--dict-cache-list))
    (print el (current-buffer))
    (goto-char (point-max))
    (dotimes (times 3) (newline)))
  (insert ")\n"))

(defun wudao/cache--batch-extract-core (cache-file output-file)
  (unless (file-exists-p output-file)
    (wudao/cache--extract-dicts)
    (wudao/lib-message "Extract '%s'  ..." cache-file)
    (setq wudao/cache--dict-cache-list nil)
    (wudao/lib-with-temp-buffer
      (insert-file-contents cache-file)
      (goto-char (point-min))
      (while (not (null (ignore-errors (wudao/cache--get-region))))
        t))
    (when wudao/cache--dict-cache-list
      (wudao/lib-with-temp-buffer
        (goto-char (point-min))
        (wudao/cache--print-item)
        (goto-char (point-min))
        (write-file output-file))
      (wudao/lib-message "Complete!"))))

(defun wudao/cache--read-hash-core (hash-file)
  (let ((inhibit-read-only t))
    (wudao/lib-with-temp-buffer
      (insert-file-contents hash-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun wudao/cache--batch-extract-desc ()
  (wudao/cache--batch-extract-core wudao/cache--cache-file-desc wudao/cache--hash-file-desc))

(defun wudao/cache--batch-extract-full ()
  (wudao/cache--batch-extract-core wudao/cache--cache-file-full wudao/cache--hash-file-full))

(defun wudao/cache--batch-extract-all ()
  (wudao/cache--batch-extract-desc)
  (wudao/cache--batch-extract-full))

(defun wudao/cache--case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun wudao/cache--case-fold-string-hash (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'wudao/cache--case-fold-test
  'wudao/cache--case-fold-string= 'wudao/cache--case-fold-string-hash)

(defun wudao/cache--create-hash-table (alist)
  (let ((hst (make-hash-table :test 'wudao/cache--case-fold-test)))
    (dolist (el alist)
      (let ((word (car el))
            (def (cdr el)))
        (puthash word def hst)))
    hst))

;;;###autoload
(defun wudao/cache-read-hash ()
  (wudao/cache--batch-extract-all)
  (dolist (hsfv `((,wudao/cache--hash-file-desc . wudao/cache--hashed-desc)
                  (,wudao/cache--hash-file-full . wudao/cache--hashed-full)))
    (if (not (file-exists-p (car hsfv)))
        (error "Cache not extracted for '%s'!" (car hsfv))
      (wudao/lib-message "Reading '%s' ..." (symbol-name (cdr hsfv)))
      (set (cdr hsfv) (wudao/cache--read-hash-core (car hsfv)))))
  (list :short (wudao/cache--create-hash-table wudao/cache--hashed-desc)
        :full  (wudao/cache--create-hash-table wudao/cache--hashed-full)))

(provide 'wudao-cache)
