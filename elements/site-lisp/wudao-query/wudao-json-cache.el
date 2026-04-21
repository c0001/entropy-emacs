;; -*- lexical-binding: t; -*-
(require 'wudao-lib)

;; * code

;; ** defvar
(defvar wudao/json-cache--host-dir
  (file-name-directory load-file-name))

(defvar wudao/json-cache--dicts-cache-tarball
  (expand-file-name
   "wudao_dict.cache.tgz"
   wudao/json-cache--host-dir))
(defvar wudao/json-cache--dicts-extract-host
  (expand-file-name
   "dict"
   wudao/json-cache--host-dir))

(defvar wudao/json-cache--json-src-file
  (expand-file-name "query.json.cache" wudao/json-cache--dicts-extract-host))

(defvar wudao/json-cache--src-hashs-array nil)
(defvar wudao/json-cache--dict-hash nil)

;; ** common libs
(defun wudao/json-cache--string-empty-p (var)
  (if (not var)
      t
    (string-empty-p var)))

(defun wudao/json-cache--case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun wudao/json-cache--case-fold-string-hash (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'wudao/json-cache--case-fold-test
  'wudao/json-cache--case-fold-string= 'wudao/json-cache--case-fold-string-hash)

(defun wudao/json-cache--extract-dicts ()
  (let ((cf wudao/json-cache--json-src-file))
    (unless (file-exists-p cf)
      (wudao/lib-archive-dowith
       'tgz wudao/json-cache--dicts-cache-tarball
       wudao/json-cache--dicts-extract-host
       :extract))))

;; ** hash get
(defun wudao/json-cache--get-src-json-hash ()
  (wudao/json-cache--extract-dicts)
  (wudao/lib-with-temp-buffer
    (insert-file-contents wudao/json-cache--json-src-file)
    (goto-char (point-min))
    (setq wudao/json-cache--src-hashs-array
          (json-parse-buffer
           :object-type 'hash-table
           :array-type 'array))))

(defun wudao/json-cache--get-src-json-hash-from-command (query)
  (wudao/lib-with-temp-buffer
    (let ((cbk
           (shell-command-to-string
            (format "WUDAO_RETURN_JSON=1 WUDAO_INIHIBIT_ERROR_MSG=1 wd '%s'"
                    query))))
      (if (string-blank-p cbk)
          (insert "{}")
        (insert cbk))
      (goto-char (point-min))
      (json-parse-buffer
       :object-type 'hash-table
       :array-type 'array
       ))))

(defun wudao/json-cache--create-dict-hash (&optional refresh)
  (when (or (not wudao/json-cache--src-hashs-array)
            refresh)
    (wudao/lib-message "Read wudao dict json cache ...")
    (wudao/json-cache--get-src-json-hash))
  (when (or (not wudao/json-cache--dict-hash)
            refresh)
    (setq wudao/json-cache--dict-hash nil)
    (setq wudao/json-cache--dict-hash (make-hash-table :test 'wudao/json-cache--case-fold-test))
    (wudao/lib-message "Hash wudao dict json cache ...")
    (let ((src-table wudao/json-cache--src-hashs-array))
      (mapc
       (lambda (x)
         (let ((word (gethash "word" x)))
           (when word
             (unless (gethash word wudao/json-cache--dict-hash)
               (puthash word x wudao/json-cache--dict-hash)))))
       src-table))
    (wudao/lib-message "Hash wudao dict json cache successfully")
    ;; release the source hash to save system memory but set as `t' to
    ;; pass the initial check.
    (progn
      (setq wudao/json-cache--src-hashs-array t)
      (garbage-collect))))

;; ** query combine

(defun wudao/json-cache--strip-str (str)
  (let ((kill-buffer-hook nil))
    (unless (wudao/json-cache--string-empty-p str)
      (with-temp-buffer
        (insert str)
        (goto-char (point-max))
        (forward-line 0)
        (while (and (looking-at "^$")
                    (not (bobp)))
          (forward-line -1))
        (end-of-line)
        (buffer-substring (point-min) (point))))))

(defmacro wudao/json-cache--combine-query-cbk
    (&rest cbk-items)
  (let ((cbks (list 'list)))
    (dolist (el cbk-items)
      (setq cbks
            (append cbks
                    `((list ,(car el) ,@(cdr el))))))
    (macroexp-let2* ignore
        ((items nil) (rtn nil))
      `(let ((,items ,cbks)
             (,rtn ""))
         (dolist (el ,items)
           (let ((str (car el))
                 (newlines (make-string (cadr el) ?\n)))
             (when str
               (setq ,rtn (concat ,rtn str newlines)))))
         (wudao/json-cache--strip-str ,rtn)))))

;; ** renders
(defun wudao/json-cache--aref-ignore-errors (&rest args)
  (ignore-errors
    (apply 'aref args)))

;; *** face spec
(defface wudao/json-cache-face-of-query-word
  '((((class color) (background dark))
     :foreground "green" :weight bold)
    (((class color) (background light))
     :foreground "red" :weight bold))
  "")

(defface wudao/json-cache-face-of-paraphrase-type
  '((((class color) (background dark))
     :foreground "SkyBlue" :weight bold)
    (((class color) (background light))
     :foreground "blue" :weight bold))
  "")

(defface wudao/json-cache-face-of-paraphrase-value
  '((((class color) (background dark))
     :foreground "GreenYellow" :weight light :slant italic)
    (((class color) (background light))
     :foreground "SeaGreen" :weight light :slant italic))
  "")

(defface wudao/json-cache-face-of-pronunciation-country
  '((((class color) (background dark))
     :foreground "grey" :weight bold)
    (((class color) (background light))
     :foreground "grey19" :weight bold))
  "")

(defface wudao/json-cache-face-of-pronunciation-value
  '((((class color) (background dark))
     :foreground "cyan" :weight light :slant italic)
    (((class color) (background light))
     :foreground "grey19" :weight light :slant italic))
  "")

(defface wudao/json-cache-face-of-word-rank
  '((((class color) (background dark))
     :foreground "red" :weight light :underline t)
    (((class color) (background light))
     :foreground "VioletRed1" :weight light :underline t))
  "")

(defface wudao/json-cache-face-of-sentence-type
  '((((class color) (background dark))
     :foreground "green" :weight bold :slant italic)
    (((class color) (background light))
     :foreground "red" :weight bold :slant italic))
  "")

(defface wudao/json-cache-face-of-sentence-src-union
  '((((class color) (background dark))
     :foreground "DarkGrey" :weight light)
    (((class color) (background light))
     :foreground "grey19" :weight light))
  "")

(defface wudao/json-cache-face-of-sentence-examble-src
  '((((class color) (background dark))
     :foreground "white" :weight light)
    (((class color) (background light))
     :foreground "grey15" :weight light))
  "")

(defface wudao/json-cache-face-of-sentence-examble-interpretation
  '((((class color) (background dark))
     :foreground "yellow" :weight light)
    (((class color) (background light))
     :foreground "tomato" :weight light))
  "")

;; *** render funs
(defun wudao/json-cache--word-render (str)
  (if (not (wudao/json-cache--string-empty-p str))
      (propertize
       (format "%s" str)
       'face 'wudao/json-cache-face-of-query-word)))

(defun wudao/json-cache--pronunciation-render (hash)
  (let ((hash (and (hash-table-p hash)
                   (not (hash-table-empty-p hash))
                   hash))
        (proper-func
         (lambda (key val)
           (format "%s: %s"
                   (propertize key 'face 'wudao/json-cache-face-of-pronunciation-country)
                   (propertize val 'face 'wudao/json-cache-face-of-pronunciation-value))))
        rtn)
    (if hash
        (maphash
         (lambda (key val)
           (setq key (or (and (not (string-empty-p key))
                              key)
                         "英/美"))
           (if rtn
               (setq rtn
                     (format "%s, %s"
                             rtn
                             (funcall proper-func key val)))
             (setq
              rtn
              (funcall proper-func key val))))
         hash))
    rtn))

(defun wudao/json-cache--paraphrase-render (vector)
  (let ((vector (and (vectorp vector)
                   (not (= (length vector) 0))
                   vector))
        (proper-func
         (lambda (type val)
           (format "%s.: %s"
                   (propertize type 'face
                               'wudao/json-cache-face-of-paraphrase-type)
                   (propertize val 'face
                               'wudao/json-cache-face-of-paraphrase-value))))
        rtn)
    (if vector
        (mapc
         (lambda (str)
           (let* ((type (and (string-match
                               "^\\([a-zA-Z]+\\). ?\\(.+\\)$"
                               str)
                              (match-string 1 str)))
                  (val (or (and
                            type
                            (match-string 2 str))
                           str)))
             (setq type (or type "__"))
             (if rtn
                 (setq rtn
                       (format "%s\n%s"
                               rtn
                               (funcall proper-func type val)))
               (setq
                rtn
                (funcall proper-func type val)))))
         vector))
    rtn))

(defun wudao/json-cache--sentence-render (vector)
  (let* ((vector (and (vectorp vector)
                      (not (= (length vector) 0))
                      vector))
         (collins_flag (and vector (not (= 2 (length (aref vector 0))))))
         (count 1)
         rtn)
    (if vector
        (let (_)
          (mapc
           (lambda (x)
             (let ((str-deal ""))
               (cond
                (collins_flag
                 (catch :exit
                   (unless (= (length x) 3) (throw :exit nil))
                   (when (ignore-errors (or (string-empty-p (aref x 1))
                                            (= 0 (length (aref x 2)))))
                     (throw :exit nil))
                   (if (string-prefix-p "[" (aref x 1))
                       (setq str-deal
                             (format
                              "%s. %s "
                              count (propertize (aref x 1) 'face
                                                'wudao/json-cache-face-of-sentence-type)))
                     (setq str-deal
                           (format
                            "%s. %s "
                            count (propertize
                                   (concat "[" (aref x 1) "]")
                                   'face
                                   'wudao/json-cache-face-of-sentence-type))))
                   (setq str-deal
                         (concat str-deal
                                 (propertize (aref x 0)
                                             'face 'wudao/json-cache-face-of-sentence-src-union)
                                 "\n"))
                   (seq-doseq (el (wudao/json-cache--aref-ignore-errors x 2))
                     (setq str-deal
                           (concat
                            str-deal
                            (make-string (+ 2 (length (number-to-string count))) ?\ )
                            "例："
                            (propertize (aref el 0) 'face 'wudao/json-cache-face-of-sentence-examble-src)
                            "\n"
                            (make-string (+ 4 2 (length (number-to-string count))) ?\ )
                            (propertize (aref el 1) 'face 'wudao/json-cache-face-of-sentence-examble-interpretation)
                            "\n")))
                   (setq rtn (concat rtn str-deal "\n"))
                   (cl-incf count)))
                (t
                 (setq str-deal
                       (format
                        "%s. [例] %s  %s\n"
                        count
                        (propertize (aref x 0) 'face 'wudao/json-cache-face-of-sentence-examble-src)
                        (propertize (aref x 1) 'face 'wudao/json-cache-face-of-sentence-examble-interpretation)))
                 (setq rtn (concat rtn str-deal "\n"))
                 (cl-incf count)))))
           vector)))
    rtn))


(defun wudao/json-cache--word-rank-render (str)
  (if (not (wudao/json-cache--string-empty-p str))
      (propertize
       (format "%s" str)
       'face 'wudao/json-cache-face-of-word-rank)))

(defun wudao/json-cache--word-patterns-render (str)
  (if (not (wudao/json-cache--string-empty-p str))
      (propertize
       (format "%s" str)
       'face 'wudao/json-cache-face-of-word-rank)))


;; *** render funs zh-cn
(defun wudao/json-cache--word-render-zh_cn (str)
  (if (not (wudao/json-cache--string-empty-p str))
      (propertize
       (format "%s" str)
       'face 'wudao/json-cache-face-of-query-word)))

(defun wudao/json-cache--pronunciation-render_zh_cn (str)
  (if (not (wudao/json-cache--string-empty-p str))
      (propertize
       (format "%s" str)
       'face 'wudao/json-cache-face-of-pronunciation-value)))

(defun wudao/json-cache--paraphrase-render_zh_cn (vector)
  (let ((vector (and (vectorp vector)
                   (not (= (length vector) 0))
                   vector))
        (proper-func
         (lambda (type val)
           (format "%s.: %s"
                   (propertize type 'face
                               'wudao/json-cache-face-of-paraphrase-type)
                   (propertize val 'face
                               'wudao/json-cache-face-of-paraphrase-value))))
        rtn)
    (if vector
        (mapc
         (lambda (str)
           (let* ((type (and (string-match
                               "^\\([a-zA-Z]+\\). ?\\(.+\\)$"
                               str)
                              (match-string 1 str)))
                  (val (or (and
                            type
                            (match-string 2 str))
                           str)))
             (setq type (or type "__"))
             (if rtn
                 (setq rtn
                       (format "%s\n%s"
                               rtn
                               (funcall proper-func type val)))
               (setq
                rtn
                (funcall proper-func type val)))))
         vector))
    rtn))

(defun wudao/json-cache--desc-render_zh_cn (vector)
  (let* ((vector (and (vectorp vector)
                      (not (= (length vector) 0))
                      vector))
         (collins_flag (and vector (not (= 2 (length (aref vector 0))))))
         (count 1)
         (sub-count 0)
         (src-proper-func
          (lambda (str)
            (format "%s. %s"
                    count
                    (propertize
                     (replace-regexp-in-string
                      ";" "," str)
                     'face
                     'wudao/json-cache-face-of-sentence-src-union))))
         str-deal
         )

    (if vector
        (seq-doseq (x vector)
          (catch :exit
            (unless (not (= (length x) 0))
              (throw :exit nil))
            (if str-deal
                (setq str-deal (concat str-deal "\n"
                                       (funcall src-proper-func (aref x 0))))
              (setq str-deal
                    (funcall src-proper-func (aref x 0))))
            (when (= 2 (length x))
              (seq-doseq (y (aref x 1))
                (if (= 0 (% sub-count 2))
                    (progn
                      (setq y (replace-regexp-in-string
                               "^ +\\(.+\\) +$"
                               "" y)
                            y
                            (propertize
                             y
                             'face
                             'wudao/json-cache-face-of-sentence-examble-src))
                      (setq str-deal
                            (concat str-deal
                                    "\n"
                                    (make-string (+ 2 (length (number-to-string count)))
                                                 ?\ )
                                    y)))
                  (setq str-deal
                        (concat str-deal
                                " " y)))

                (cl-incf sub-count)))
            (cl-incf count)
            (setq sub-count 0))))
    str-deal))


(defun wudao/json-cache--sentence-render_zh_cn (vector)
  (let* ((vector (and (vectorp vector)
                      (not (= (length vector) 0))
                      vector))
         (collins_flag (and vector (not (= 2 (length (aref vector 0))))))
         (count 1)
         rtn)
    (if vector
        (progn
          (setq rtn "例句: ")
          (seq-doseq (x vector)
            (when (= 2 (length x))
              (setq rtn
                    (format "%s\n%s. %s %s"
                            rtn
                            count
                            (propertize (aref x 0)
                                        'face 'wudao/json-cache-face-of-sentence-examble-src)
                            (propertize (aref x 1)
                                        'face
                                        'wudao/json-cache-face-of-sentence-examble-interpretation)))
              (cl-incf count)))))
    rtn))

;; ** main

(defun wudao/json-cache--get-cbk (word-hash &optional short)
  (wudao/json-cache--combine-query-cbk
   ((wudao/json-cache--word-render
     (gethash "word" word-hash))
    1)
   ((wudao/json-cache--pronunciation-render
     (gethash "pronunciation" word-hash))
    1)
   ((wudao/json-cache--paraphrase-render
     (gethash "paraphrase" word-hash))
    1)
   ((let ((cur (wudao/json-cache--word-rank-render
                (gethash "rank" word-hash)))
          (next (wudao/json-cache--word-patterns-render
                 (gethash "pattern" word-hash))))
      (if cur
          (if next
              (concat cur " " next)
            cur)
        (when next
          next)))
    2)
   ((and (not short)
         (wudao/json-cache--sentence-render
          (gethash "sentence" word-hash)))
    0)))

(defun wudao/json-cache--get-cbk_zh_cn (word-hash &optional short)
  (wudao/json-cache--combine-query-cbk
   ((wudao/json-cache--word-render-zh_cn
     (gethash "word" word-hash))
    1)
   ((wudao/json-cache--pronunciation-render_zh_cn
     (gethash "pronunciation" word-hash))
    1)
   ((wudao/json-cache--paraphrase-render_zh_cn
     (gethash "paraphrase" word-hash))
    2)
   ((and (not short)
         (wudao/json-cache--desc-render_zh_cn
          (gethash "desc" word-hash)))
    2)
   ((and (not short)
         (wudao/json-cache--sentence-render_zh_cn
          (gethash "sentence" word-hash)))
    0)))


(defun wudao/json-cache--query-core (query backend &optional short)
  (let* ((zh_cn_p (if (string-match-p (format "\\cC\\{%s\\}" (length query)) query)
                      t
                    nil))
         (word-hash (if (eq backend 'cache)
                        (progn
                          (wudao/json-cache--create-dict-hash)
                          (gethash query wudao/json-cache--dict-hash))
                      (wudao/json-cache--get-src-json-hash-from-command query))))
    (when word-hash
      (if zh_cn_p
          (wudao/json-cache--get-cbk_zh_cn word-hash short)
        (wudao/json-cache--get-cbk word-hash short)))))

;; * provide

(defun wudao/json-cache-query-by-cache (query &optional short)
  (wudao/json-cache--query-core query 'cache short))
(defun wudao/json-cache-query-by-command (query &optional short)
  (wudao/json-cache--query-core query 'command short))

(provide 'wudao-json-cache)
