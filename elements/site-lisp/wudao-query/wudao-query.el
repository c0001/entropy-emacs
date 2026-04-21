;; -*- lexical-binding: t; -*-
;; * code
(require 'wudao-cache)
(require 'wudao-json-cache)
(require 'wudao-lib)

;; ** common libs

(defvar wudao/query--host-dir
  (expand-file-name
   (file-name-directory load-file-name)))

;; ** ansi apply style
;; *** var
(defvar wudao/query--hash-plist nil)
(defvar wudao/query-en-words-completion-tatble nil)

;; *** library
(defun wudao/query--ansi-color-apply (string)
  "The same as `ansi-color-apply', but using `face' instead of
`font-lock-faace' for make `message' colorized."
  ;; firstly we replace the blue pattern in word translation context
  ;; to magenta which be more obvious distinctly.
  (setq string
        (replace-regexp-in-string
         ;; TODO: add more conditions
         "\\[34m\\(n\\|adj\\|adv\\|art\\|vt?\\|vi\\|prep\\|abbr\\|conj\\|pron\\|misc\\)\\."
         "[95m\\1." string))
  (let ((codes (car ansi-color-context))
        (start 0) end result)
    ;; If context was saved and is a string, prepend it.
    (if (cadr ansi-color-context)
        (setq string (concat (cadr ansi-color-context) string)
              ansi-color-context nil))
    ;; Find the next escape sequence.
    (while (setq end (string-match ansi-color-control-seq-regexp string start))
      (let ((esc-end (match-end 0)))
        ;; Colorize the old block from start to end using old face.
        (when codes
          (put-text-property start end 'face
                             (ansi-color--find-face codes) string))
        (push (substring string start end) result)
        (setq start (match-end 0))
        ;; If this is a color escape sequence,
        (when (eq (aref string (1- esc-end)) ?m)
          ;; create a new face from it.
          (setq codes (ansi-color-apply-sequence
                       (substring string end esc-end) codes)))))
    ;; if the rest of the string should have a face, put it there
    (when codes
      (put-text-property start (length string)
                         'face (ansi-color--find-face codes) string))
    ;; save context, add the remainder of the string to the result
    (let (fragment)
      (if (string-match "\033" string start)
          (let ((pos (match-beginning 0)))
            (setq fragment (substring string pos))
            (push (substring string start pos) result))
        (push (substring string start) result))
      (setq ansi-color-context (if (or codes fragment) (list codes fragment))))
    (apply 'concat (nreverse result))))

(defun wudao/query--get-hash ()
  ;; restrict `gc-cons-threshold' prevents memory overflow
  (let ((gc-cons-threshold 80000))
    (if (null wudao/query--hash-plist)
        (setq wudao/query--hash-plist
              (wudao/cache-read-hash))
      wudao/query--hash-plist)))

(defun wudao/query--get-compatible-en-words (en-words-list)
  (delete
   nil
   (mapcar
    (lambda (x)
      (let ((x-1 (substring-no-properties (wudao/query-word-by-hash x))))
        (unless (string-match-p (rx (or "===Empty===" "===NOFOUND===")) x-1)
          x)))
    en-words-list)))

(defun wudao/query--make-en-words-completion-table (en-words-list)
  (wudao/query--get-hash)
  (let ((vc-reg (rx line-start (or "英/美 " "英 " "美 ")))
        (term-reg "CET[0-9]\\|TEM[0-9]\\|^( .+ )")
        (case-fold-search nil)
        tempo-register)
    (when (alist-get en-words-list wudao/query-en-words-completion-tatble
                     nil nil 'equal)
      (setq wudao/query-en-words-completion-tatble
            (delete
             nil
             (mapcar
              (lambda (x)
                (unless (equal (car x) en-words-list)
                  x))
              wudao/query-en-words-completion-tatble))))
    (redisplay t)
    (wudao/lib-message "Get \"wudao/query-en-words-completion-tatble\" ... ")
    (dolist (word (wudao/query--get-compatible-en-words en-words-list))
      (let ((cache
             (wudao/query-word-by-hash
              word
              nil))
            (cache-full
             (wudao/query-word-by-hash
              word
              t)))
        (let (sp-list final-list vc-var trans-list)
          (setq sp-list
                (split-string
                 (substring-no-properties
                  cache)
                 "\n" t))
          (setq vc-var (nth 1 sp-list)
                vc-var (and (ignore-errors (string-match-p vc-reg vc-var))
                            vc-var))
          (when vc-var
            (setq vc-var
                  (replace-regexp-in-string
                   "英" "UK>"
                   (replace-regexp-in-string
                    "美" "US>"
                    (replace-regexp-in-string
                     "英/美" "UK/US"
                     vc-var)))))
          (setq
           trans-list
           (delete
            nil
            (mapcar
             (lambda (x)
               (unless (string-match-p term-reg x)
                 (let ((prop-reg "^\\([a-z]+\\.\\) ?"))
                   (list :prop (when (string-match prop-reg x) (match-string 1 x))
                         :desc (replace-regexp-in-string prop-reg "" x)))))
             (if vc-var (cddr sp-list) (cdr sp-list))))
           trans-list
           (list :prop-list
                 (remove-duplicates
                  (delete nil
                          (mapcar (lambda (x) (plist-get x :prop)) trans-list))
                  :test 'string=)
                 :details trans-list)
           final-list
           (list word
                 :phonetic vc-var
                 :translation
                 `((chinese-simplified
                    :annotation ,trans-list
                    :short-trans ,cache
                    :full-trans ,cache-full
                    ))))
          (when final-list
            (push
             final-list
             tempo-register)))))
    (setq tempo-register
          (reverse tempo-register)
          tempo-register
          (mapcar
           (lambda (x)
             (propertize (car x)
                         'wudao/query-en-words-completion
                         (cdr x)))
           tempo-register))
    (push (cons en-words-list tempo-register)
          wudao/query-en-words-completion-tatble)
    tempo-register))

;; *** main
;;;###autoload
(defun wudao/query-word-by-hash (query &optional full)
  (let* ((hsp (wudao/query--get-hash))
         (cbk (ignore-errors
                (wudao/query--ansi-color-apply
                 (if full (gethash query (plist-get hsp :full))
                   (gethash query (plist-get hsp :short)))))))
    cbk))

(defun wudao/query-word-by-command (query &optional full)
  (let ((sh-cmd-short "wd -s \"%s\"")
        (sh-cmd-full "wd -l \"%s\"")
        cbk)
    (setq cbk
          (wudao/query--ansi-color-apply
           (shell-command-to-string
            (format (if full sh-cmd-full
                      sh-cmd-short)
                    query))))
    (when (string-match-p "\\(^No such word:\\|^Error\\)" cbk)
      (setq cbk nil))
    cbk))

(defun wudao/query-get-en-words-completion-table (en-words-list &optional force)
  (if force
      (wudao/query--make-en-words-completion-table en-words-list)
    (or (alist-get en-words-list wudao/query-en-words-completion-tatble
                   nil nil 'equal)
        (wudao/query--make-en-words-completion-table en-words-list))))

(defun wudao/query-get-en-words-property (word attr trans-lang)
  (let* ((cache (get-text-property 0 'wudao/query-en-words-completion word))
         (trans (alist-get trans-lang (plist-get cache :translation)))
         (annotation (plist-get trans :annotation))
         ;; ----------------------------------
         (phonetic '(plist-get cache :phonetic))
         (prop-list '(plist-get annotation :prop-list))
         (details '(plist-get annotation :details))
         (short-trans '(plist-get trans :short-trans))
         (full-trans '(plist-get trans :full-trans)))
    (when cache
      (cl-case attr
        (entry cache)
        (phonetic (eval phonetic))
        (prop-list (eval prop-list))
        (details (eval details))
        (short-trans (eval short-trans))
        (full-trans (eval full-trans))))))


;; ** json parse style

;;;###autoload
(defun wudao/query-word-by-hash/use-json-parse (query &optional full)
  (wudao/json-cache-query-by-cache query (not full)))

(defun wudao/query-word-by-command/use-json-parse (query &optional full)
  (wudao/json-cache-query-by-command query (not full)))

;; * provide
(provide 'wudao-query)
