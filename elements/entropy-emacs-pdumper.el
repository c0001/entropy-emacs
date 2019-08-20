;;; entropy-emacs-pdumper.el --- procedure wrapper for pdumper process
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;; 
;; * Commentary:
;; 
;; commentary
;; 
;; * Configuration:
;; 
;; configuration
;; 
;; * Code:
;; ** require
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'rx)

;; ** defvar
(defvar entropy/emacs-pdumper--origin-load-path (copy-tree load-path))
(defvar entropy/emacs-pdumper-upstream-els nil)
(defvar entropy/emacs-pdumper-upstream-top-dir
  (progn (entropy/emacs-set-package-user-dir)
         package-user-dir))

;; timers
(defvar entropy/emacs-pdumper--rec-timer nil)

;; ** library
;; *** macro
(cl-defmacro entropy/emacs-pdumper--with-load-path (top-dir &rest body)
  (let* ((tempo-lpth (append
                      (if top-dir 
                          (entropy/emacs-list-subdir (if (symbolp top-dir)
                                                         (symbol-value top-dir)
                                                       top-dir))
                        nil)
                      entropy/emacs-origin-load-path)))
    `(let ((load-path ',tempo-lpth))
       ,@body)))

;; *** extract files
(defun entropy/emacs-pdumper--extract-upstream-packages ()
  (let* ((top-dir entropy/emacs-pdumper-upstream-top-dir)
         (sub-dirs (entropy/emacs-list-subdir top-dir))
         rtn)
    (dolist (el sub-dirs)
      (let ((files (entropy/emacs-list-dir-lite el)))
        (dolist (file files)
          (let ((ftype (car file))
                (fpath (cdr file)))
            (unless (or (equal "D" ftype)
                        (string-match-p "\\(autoloads\\.el\\|pkg\\.el\\)" fpath)
                        (not (string-match-p "\\.el$" fpath))
                        (not (string-match-p
                              (rx (or "ivy" "org" "magit" "counsel"
                                      "dired" "all-the-icon"
                                      "use-package" "diminish" "bind-key"
                                      (seq line-start "company-")))
                              (entropy/emacs-file-path-parser fpath 'file-name))))
              (push fpath rtn))))))
    (unless rtn
      (error "Non upstream installed!"))
    (setq entropy/emacs-pdumper-upstream-els rtn)))

(defun entropy/emacs-pdumper--extract-org-packages ()
  (let* ((org-dir (file-name-directory (locate-library "org")))
         (lite-list (entropy/emacs-list-dir-lite org-dir))
         (file-list (mapcar
                     (lambda (x)
                       (let ((ftype (car x)))
                         (when (equal "F" ftype)
                           (cdr x))))
                     lite-list))
         rtn)
    (dolist (file file-list)
      (when (string-match-p "\\.elc$" file)
        (push file rtn)))
    rtn))

;; *** recovery
(defun entropy/emacs-pdumper--recovery ()
  (unless (entropy/emacs-in-pdumper-procedure-p)
    (global-font-lock-mode +1)
    (transient-mark-mode +1)
    (setq entropy/emacs-custom-pdumper-do nil)
    (setq load-path entropy/emacs-pdumper-pre-lpth)
    ;; TODO ...body
    (run-hooks 'entropy/emacs-pdumper-load-hook)
    (when entropy/emacs-pdumper--rec-timer
      (cancel-timer entropy/emacs-pdumper--rec-timer))
    (defun entropy/emacs-pdumper--recovery ()
      nil)))

;; ** load-files
(defun entropy/emacs-pdumper--load-upstream ()
  (let ((els (entropy/emacs-pdumper--extract-upstream-packages)))
    (entropy/emacs-pdumper--with-load-path
     entropy/emacs-pdumper-upstream-top-dir
     (dolist (file els)
       (let* ((feature-name (file-name-base
                             (entropy/emacs-file-path-parser
                              file 'file-name)))
              (feature (intern feature-name)))
         (message "[Pdumper] load-file: %s"
                  feature-name)
         (ignore-errors (require feature)))))))

(defun entropy/emacs-pdumper--load-org ()
  (let ((els (entropy/emacs-pdumper--extract-org-packages)))
    (entropy/emacs-pdumper--with-load-path
     nil
     (dolist (file els)
       (require (intern (file-name-base file)))))))

;; ** main
(entropy/emacs-pdumper--load-org)
(entropy/emacs-pdumper--load-upstream)
(setq load-path (append
                 (entropy/emacs-list-subdir
                  entropy/emacs-pdumper-upstream-top-dir)
                 entropy/emacs-pdumper--origin-load-path)
      entropy/emacs-pdumper-pre-lpth (copy-tree load-path)
      entropy/emacs-pdumper--rec-timer
      (run-with-idle-timer 1.1 t #'entropy/emacs-pdumper--recovery))

;; * provide
(provide 'entropy-emacs-pdumper)
