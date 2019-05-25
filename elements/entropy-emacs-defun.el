;;; entropy-emacs-defun.el --- entropy-emacs-basic function api

;; Copyright (C) 2019-03026  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       none
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; commentary
;;
;;; Configuration:
;;
;; configuration

;; * Code:
;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)

;; ** file and directories
(defun entropy/emacs-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\.$\\|\\.\\.$\\)" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))


(defun entropy/emacs-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))


;; ** counter map list
(defun entropy/emacs-numberic-list (list-var)
  "Return list element mapped with numberic prefix which concated
with '0' as alignment state."
  (let* ((l-len (length list-var))
         (register l-len)
         (counter 0)
         (step 1)
         (zero-func
          (lambda (counter str)
            (let ((step (length str))
                   (rtn str))
              (while (< step counter)
                (setq rtn (concat "0" rtn)
                      step (+ 1 step)))
              rtn)))
         rtn)
    (while (not (eq 0 register))
      (setq register (/ register 10)
            counter (+ 1 counter)))
    (dolist (el list-var)
      (push (cons (funcall zero-func counter (number-to-string step))
                  el)
            rtn)
      (setq step (+ 1 step)))
    (reverse rtn)))


;; ** language environment set
(defun entropy/emacs-toggle-utf-8-and-locale (&optional cond)
  " This function was to toggle entire UTF-8 environment to or
back from locale.

Optional arg COND has four type:

- \"prompt\": call this function with ivy-read prompt for choosing the target encoding.
- \"UTF-8\": choose UTF-8 without prompt.
- \"LOCAL\": choose LOCAL language environment without prompt.
"
  (interactive)
  (if cond
      (cond
       ((string= cond "prompt")
        (ivy-read "Choice: " '("UTF-8" "LOCAL")
                  :require-match t
                  :action #'entropy/emacs-lang-set))
       ((string= cond "UTF-8") (entropy/emacs-lang-set "UTF-8"))
       ((string= cond "LOCAL") (entropy/emacs-lang-set "LOCAL"))
       (t (error "entropy/emacs-toggle-utf-8-and-locale: error argument type.")))
    (if (string= current-language-environment "UTF-8")
        (entropy/emacs-lang-set "LOCAL")
      (entropy/emacs-lang-set "UTF-8"))))

(defun entropy/emacs-lang-set (lang)
  (if (string-match-p
       "\\*e?shell\\*\\|\\*eshell-.*?\\*\\|\\(^\\*ansi-term-.*\\)\\|\\(\\*terminal\\)"
       (format "%s" (buffer-list)))
      (error "Can not use this function cause shell buffer exist, please kill it and try again!")
    (cond
     ((string= lang "UTF-8")
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8-unix)
      (message "Setting language environment to 'utf-8-unix'."))
     ((string= lang "LOCAL")
      (set-language-environment entropy/emacs-language-environment)
      (prefer-coding-system entropy/emacs-lang-locale)
      (setq default-file-name-coding-system 'utf-8-unix)
      (message "Setting language environment to '%s'." entropy/emacs-language-environment))
     (t (error "Invalid LANG arg")))))

(defun entropy/emacs-lang-set-utf-8 (&rest args)
  "Setting language envrionment to unix-8-unix, supported
by `entropy/emacs-lang-set'"
  (if (not (string= current-language-environment "UTF-8"))
      (entropy/emacs-lang-set "UTF-8")))

(defun entropy/emacs-revert-buffer-with-custom-language-environment ()
  "This function was designed to auto revert buffer with
language-environment you set in `entropy/emacs-language-environment'."
  (interactive)
  (if (string= current-language-environment "UTF-8")
      (progn
        (entropy/emacs-toggle-utf-8-and-locale)
        (revert-buffer t t)
        (message "Succeed revert buffer with %s" entropy/emacs-language-environment))
    (error "Have been locale setting ♘")))


;; around advice when `entropy/emacs-custom-language-environment-enable' was nil

(defun entropy/emacs-lang-set-without-enable (oldfunc &rest args)
  "Around advice for funcs:

- entropy/emacs-toggle-utf-8-and-locale
- entropy/emacs-lang-set-utf-8
- entropy/emacs-revert-buffer-with-custom-language-environment

This func will force disable each func's internal procedure when
custom variable
`entropy/emacs-custom-language-environment-enable' was nil.
 "
  (cond
   ((and entropy/emacs-custom-language-environment-enable
         (ignore-errors (stringp entropy/emacs-language-environment)))
    (apply oldfunc args))
   ((or (null entropy/emacs-custom-language-environment-enable)
        (not (ignore-errors (stringp entropy/emacs-language-environment))))
    t)))

(dolist (el '(entropy/emacs-toggle-utf-8-and-locale
              entropy/emacs-lang-set-utf-8
              entropy/emacs-revert-buffer-with-custom-language-environment))
  (advice-add el :around #'entropy/emacs-lang-set-without-enable))


;; ** provide
(provide 'entropy-emacs-defun)
