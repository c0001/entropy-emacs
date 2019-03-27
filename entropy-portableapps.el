;;; File name: entropy-portableapps.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Commentry:
;;
;;  This pacakage provide query and start portableapps which
;;  installed through the method provided by
;;  https://portableapps.com/.
;;
;; * Code:
;;

(require 'entropy-common-library)

(defvar entropy/poapps-root nil
  "Portableapps folder root dir using for `entropy-portableapps'
  package.")

(defvar entropy/poapps-exclude-list nil
  "List of apps' name that would be used with
  `entropy/poapps-query-open'")

(defun entropy/poapps-list-apps ()
  "List portable apps under folder `entropy/poapps-root'."
  (let ((dir (entropy/cl-list-subdir entropy/poapps-root))
        Apps)
    (if (not dir) (error (format "None apps in '%s'" entropy/poapps-root)))
    (dolist (el0 dir)
      (let ((sub (entropy/cl-list-dir-lite el0)))
        (when sub
          (dolist (el1 sub)
            (if (string-match-p "\\(\\.exe\\|\\.bat\\)$" (cdr el1))
                (push (cdr el1) Apps))))))
    (if Apps Apps nil)))

(defun entropy/poapps-make-name-alist ()
  "Make name alist for apps vector, relying on function
`entropy/cl-make-name-alist'."
  (let* ((olist (entropy/poapps-list-apps))
         (nfunc '(lambda (x) (file-name-nondirectory x)))
         (teml (entropy/cl-make-name-alist olist nfunc))
         (rtn teml))
    (if entropy/poapps-exclude-list
        (dolist (el entropy/poapps-exclude-list)
          (if (assoc el teml)
              (setq rtn (delete (assoc el teml) rtn)))))
    rtn))

;;;###autoload
(defun entropy/poapps-query-open ()
  "Query and open portableapps."
  (interactive)
  (let* ((apps (entropy/poapps-make-name-alist))
         (choice (ivy-read "Query portableapps: " apps
                           :require-match t))
         (stick (cdr (assoc choice apps))))
    (let ((default-directory (getenv "TEMP")))
      (w32-shell-execute "open" stick))))

(provide 'entropy-portableapps)
