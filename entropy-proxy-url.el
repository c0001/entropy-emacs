;;; File name: entropy-proxy-url.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;* Code:
(require 'proxy-mode)

(require 'w3m)
(require 'entropy-proxy-url-gfw-list)

;; ** variable declare
(defvar entropy/proxy-url-enable 'regexp
  "If none-nil enable proxy for two type:

    - symbol ‘regexp’: proxy url matched by regexp of list
      `entropy/proxy-url-regexp-list'

    - symbol ‘t’: proxy for all searching。")

(defvar entropy/proxy-url-enable-eww entropy/proxy-url-enable)
(defvar entropy/proxy-url-enable-w3m entropy/proxy-url-enable)

(defgroup entropy/proxy-url-group nil
  "group of `entropy-proxy-url'")

(defcustom entropy/proxy-url-regexp-list
  '(("google"
     "-o" "http_proxy=http://127.0.0.1:1080"
     "-o" "https_proxy=http://127.0.0.1:1080")
    ("wikipedia"
     "-o" "http_proxy=http://127.0.0.1:1080"
     "-o" "https_proxy=http://127.0.0.1:1080"))
  "The regexp list for matching url for using proxy.

The structer of this variable format was equal with
`w3m-command-arguments-alist'."
  :type '(repeat (cons :format "%v" :indent 4
		       (regexp :format "%t: %v\n")
		       (repeat :tag "Arguments passed to w3m command"
			       (string :format "Arg: %v\n"))))
  :group 'entropy/proxy-url-group)

(defcustom entropy/proxy-url-w3m-full-proxy-value  
  '(""
    "-o" "http_proxy=http://127.0.0.1:1080"
    "-o" "https_proxy=http://127.0.0.1:1080")
  "list of proxy setting for all urls visiting of `emacs-w3m'.

The fist element was the match for all urls, you do not manully modified it.

Rest elements are args for w3m command exec, you can customize it
follow the format reserved type.")

(defvar entropy/proxy-url-w3m-init nil)

;; ** macro
(defmacro entropy/proxy-url-type-form (func-regexp func-full func-clean url type-source)
  "Macro for creating auto proxy setting function accroding the
value of `entropy/proxy-url-enable'."
  `(let ((regular (symbol-value ,type-source)))
     (cond
      ((eq regular 'regexp)
       (funcall ,func-regexp ,url))
      ((eq regular 't)
       (funcall ,func-full))
      ((eq regular nil) (funcall ,func-clean)))))

(defmacro entropy/proxy-url-swith-form (type-source)
  "Macro for creating proxy type throung the way of setting each of
`entropy/proxy-url-enable-eww' and `entropy/proxy-url-enable-w3m'."
  `(let ((choice (intern (completing-read "Choose proxy method: "
                                          '("regexp" "t" "nil")))))
     (setf (symbol-value ,type-source) choice)))

;; *** Create eww group function
(defun entropy/proxy-url-regexp-for-eww (url)
  "Proxy url visiting by regexp matching by
`entropy/proxy-url-regexp-list' with eww operation."
  (let ((regexp entropy/proxy-url-regexp-list))
    (if (let ((rtn nil))
          (dolist (el regexp)
            (when (string-match-p (car el) url)
              (setq rtn t)))
          rtn)
        (when (not proxy-mode)
          (proxy-mode))
      (when proxy-mode
        (proxy-mode-disable)))))

(defun entropy/proxy-url-full-for-eww ()
  "Proxy for all urls visiting for eww operation."
  (when (not proxy-mode)
    (proxy-mode)))


(defun entropy/proxy-url-clean-for-eww ()
  "Clean all proxy setting for eww operation."
  (when proxy-mode
    (proxy-mode)))

;; ** Create w3m group function
(defun entropy/proxy-set-w3m-command-arguments ()
  "Adding proxy config one by one to `w3m-command-arguments-alist'."
  (when (and entropy/proxy-url-regexp-list
             (listp entropy/proxy-url-regexp-list))
    (dolist (el entropy/proxy-url-regexp-list)
      (add-to-list 'w3m-command-arguments-alist el))))

(defun entropy/proxy-url-regexp-for-w3m (&optional arg)
  "Proxy url visiting by regexp matching by
`entropy/proxy-url-regexp-list' with w3m operation."
  (when (not entropy/proxy-url-w3m-init)
    (when (member entropy/proxy-url-w3m-full-proxy-value
                  w3m-command-arguments-alist)
      (setq w3m-command-arguments-alist
            (delete entropy/proxy-url-w3m-full-proxy-value
                    w3m-command-arguments-alist)))
    (entropy/proxy-set-w3m-command-arguments)))

(defun entropy/proxy-url-full-for-w3m ()
  "Proxy for all urls visiting for eww operation."
  (when entropy/proxy-url-w3m-init
    (setq entropy/proxy-url-w3m-init nil))
  (when (member (car entropy/proxy-url-regexp-list)
                w3m-command-arguments-alist)
    (dolist (el entropy/proxy-url-regexp-list)
      (setq w3m-command-arguments-alist
            (delete el w3m-command-arguments-alist))))
  (when (not (member entropy/proxy-url-w3m-full-proxy-value
                     w3m-command-arguments-alist))
    (add-to-list 'w3m-command-arguments-alist
                 entropy/proxy-url-w3m-full-proxy-value)))

(defun entropy/proxy-url-clean-for-w3m ()
  "Clean all proxy setting for `emacs-w3m'."
  (when (member (car entropy/proxy-url-regexp-list)
                w3m-command-arguments-alist)
    (dolist (el entropy/proxy-url-regexp-list)
      (setq w3m-command-arguments-alist
            (delete el w3m-command-arguments-alist))))
  (when (member entropy/proxy-url-w3m-full-proxy-value
                w3m-command-arguments-alist)
    (setq w3m-command-arguments-alist
          (delete entropy/proxy-url-w3m-full-proxy-value
                  w3m-command-arguments-alist))))


;; ** main
;;;###autoload
(defun entropy/proxy-url-proxy-choice-for-eww (url &rest args)
  "Determined for whether using proxy for searching specific url or
    for all searching operation for `eww'.

    The determined condition was refer by `entropy/proxy-url-enable'."
  (entropy/proxy-url-type-form 'entropy/proxy-url-regexp-for-eww
                               'entropy/proxy-url-full-for-eww
                               'entropy/proxy-url-clean-for-eww
                               url
                               'entropy/proxy-url-enable-eww))

;;;###autoload
(defun entropy/proxy-url-proxy-choice-for-w3m (&optional url &rest args)
  "Determined for whether using proxy for searching specific url or
    for all searching operation for `emacs-w3m'.

    The determined condition was refer by `entropy/proxy-url-enable'."
  (entropy/proxy-url-type-form 'entropy/proxy-url-regexp-for-w3m
                               'entropy/proxy-url-full-for-w3m
                               'entropy/proxy-url-clean-for-w3m
                               url
                               'entropy/proxy-url-enable-w3m))


(defun entropy/proxy-url-switch-for-eww ()
  "Switching proxy type for `eww'."
  (interactive)
  (entropy/proxy-url-swith-form 'entropy/proxy-url-enable-eww))

(defun entropy/proxy-url-switch-for-w3m ()
  "Switching proxy type for `emacs-w3m'."
  (interactive)
  (entropy/proxy-url-swith-form 'entropy/proxy-url-enable-w3m))

(defun entropy/proxy-url-switch-fo-all ()
  "Unified the value of proxy type both of `eww' and `emacs-w3m'."
  (interactive)
  (let ((choice (intern (completing-read "Choose proxy method for all: " '("regexp" "t" "nil")))))
    (mapcar #'(lambda (x)
                (setf (symbol-value x) choice))
            (list 'entropy/proxy-url-enable-eww
                  'entropy/proxy-url-enable-w3m))))

;; * provide
(provide 'entropy-proxy-url)
