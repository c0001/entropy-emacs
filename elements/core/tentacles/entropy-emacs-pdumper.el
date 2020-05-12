;;; entropy-emacs-pdumper.el --- procedure wrapper for pdumper process of entropy-emacs
;;
;; * Copyright (C) 20190821  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-pdumper.el
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "27") (cl-lib "0.5"))
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
;; Pre loading procedure for pdumper procedure of =entropy-emacs=.
;;
;; * Configuration:
;;
;; Designed for =entropy-emacs= only without inidividually using
;; warranty.
;;
;; Sets of functions used as library came from other designation of
;; =entropy-emacs=, thus correctly extracting theme from that was
;; necessary for hacking.
;;
;;
;; * Code:
;; ** require
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-defconst)
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'rx)

;; ** defvar
(defvar entropy/emacs-pdumper--upstream-els nil)
(defvar entropy/emacs-pdumper--upstream-top-dir
  (if (entropy/emacs-ext-elpkg-get-by-emacs-pkgel-p)
      (progn (entropy/emacs-set-package-user-dir)
             package-user-dir)
    (cond
     ((eq entropy/emacs-ext-elpkg-get-type 'submodules)
      (error "Pdumper can not dump extensions with submodule
archived option of `entropy/emacs-ext-elpkg-get-type'"))
     (t (error "Wrong type of extension type chosen '%s' for pdumper"
               entropy/emacs-ext-elpkg-get-type)))))

(defvar entropy/emacs-pdumper--loads-log-file
  (file-truename
   ;; using truename for opening log file for prevent link chase warning in cli
   (expand-file-name
    (format "pdumper-loads-log_%s.txt"
            (format-time-string "%Y%m%d%H%M%S"))
    entropy/emacs-stuffs-topdir)))

;; timers
(defvar entropy/emacs-pdumper--rec-timer nil
  "Timer after load pdumper session used for initialize rest
configuration.")

;; ** library
;; *** macro
(cl-defmacro entropy/emacs-pdumper--with-load-path (top-dir &rest body)
  `(let* ((load-path (append (and ,top-dir (entropy/emacs-list-dir-recursive-for-list ,top-dir))
                             entropy/emacs-origin-load-path)))
     ,@body))

;; *** extract files

(defmacro entropy/emacs-pdumper--extract-files-with-dir
    (top-dir exc-filters inc-filters &optional full-path)
  `(let* ((files (entropy/emacs-list-files-recursive-for-list ,top-dir))
          rtn)
     (dolist (file files)
       (let ((file-name (if ,full-path file (file-name-nondirectory file)))
             exc-passed inc-passed)

         (dolist (exc-rx ,exc-filters)
           (if (string-match-p exc-rx file-name)
               (push t exc-passed)
             (push nil exc-passed)))
         (setq exc-passed (not (member t exc-passed)))

         (dolist (inc-rx ,inc-filters)
           (if (string-match-p inc-rx file-name)
               (push t inc-passed)
             (push nil inc-passed)))
         (setq inc-passed (not (member nil inc-passed)))

         (when (and exc-passed inc-passed)
           (push file rtn))))
     rtn))

(defun entropy/emacs-pdumper--extract-upstream-packages ()
  (let ((exc-filters `(,(rx (or (seq "autoloads.el" line-end)
                                (seq "pkg.el" line-end)
                                (seq line-start "test.el")
                                (seq line-start "doom-themes-ext")))))
        (inc-filters `(,(rx (seq (or "ivy" "org" "magit" "transient" "counsel"
                                     "dired" "all-the-icon" "cal-china"
                                     "use-package" "diminish" "bind-key"
                                     "doom" "company" "treemacs" "entropy"
                                     "rg" "wgrep" "dashboard"
                                     "youdao" "google" "bing"
                                     "projectile" "lsp" "dap" "company")
                                 (? "-")
                                 (* any)
                                 (seq ".elc" line-end))))))
    (entropy/emacs-pdumper--extract-files-with-dir
     entropy/emacs-pdumper--upstream-top-dir
     exc-filters
     inc-filters)))

(defun entropy/emacs-pdumper--extract-internal-packages ()
  (let ((exc-filters `(,(rx (regexp (eval (regexp-quote (file-name-directory (locate-library "org"))))))
                       ,(rx (or (eval
                                 (if (not sys/win32p)
                                     "w32"
                                   "*eemacs-exlude-feature-indicator*"))))))
        (inc-filters `(,(rx (seq (or "cl" "tramp" "file" "dired" "url" "eww" "eshell" "esh" "em-")
                                 (* any)
                                 (seq ".elc" line-end))))))
    (entropy/emacs-pdumper--extract-files-with-dir
     (file-name-directory (locate-library "window"))
     exc-filters
     inc-filters)))

(defun entropy/emacs-pdumper--extract-eemacs-deps-packages ()
  (let* ((eemacs-deps-top-dir
          entropy/emacs-site-lisp-path)
         (exc-filters `(,(rx (seq line-start
                                  (or "liberime"
                                      "fakecygpty"
                                      "font-lock"
                                      "test")
                                  (* any)))))
         (inc-filters `(,(rx (seq line-start "entropy-" (* any))
                             (seq ".el" line-end)))))
    (entropy/emacs-pdumper--extract-files-with-dir
     eemacs-deps-top-dir
     exc-filters inc-filters)))

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
    (setq load-path entropy/emacs-pdumper-pre-lpth)

    ;; TODO ...body

    (entropy/emacs-message-do-message (blue "Initializing pdumper session ..."))
    ;; the pdumper session procedure
    (run-hooks 'entropy/emacs-pdumper-load-hook)
    ;; trail dealing
    (load-library "tramp")              ;reload tramp for enable `auto-sudoedit'
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (redisplay t)
    (setq entropy/emacs-fall-love-with-pdumper nil)
    ;; the very ending procedure
    (run-hooks 'entropy/emacs-pdumper-load-end-hook)
    ;; finally run start-end hook
    (run-hooks 'entropy/emacs-startup-end-hook)
    (entropy/emacs-message-do-message (green "Initialized pdumper session"))
    (when entropy/emacs-pdumper--rec-timer
      (cancel-timer entropy/emacs-pdumper--rec-timer)
      (setq gc-cons-threshold entropy/emacs-gc-threshold-basic)
      (garbage-collect))
    (defun entropy/emacs-pdumper--recovery ()
      nil)
    (setq entropy/emacs-startup-done t)))

;; ** load-files
(defmacro entropy/emacs-pdumper--load-files-core (top-dir files)
  `(entropy/emacs-pdumper--with-load-path
    ,top-dir
    (dolist (file ,files)
      (let* ((feature-name (file-name-base file))
             (feature (intern feature-name)))
        (let ((inhibit-read-only t)
              (backup-inhibited t))
          (if (not (file-exists-p entropy/emacs-pdumper--loads-log-file))
              (with-temp-buffer
                (erase-buffer)
                (goto-char (point-min))
                (insert file)
                (write-region
                 nil nil
                 entropy/emacs-pdumper--loads-log-file))
            (with-current-buffer (find-file-noselect entropy/emacs-pdumper--loads-log-file)
              (goto-char (point-max))
              (when (looking-back "^.+")
                (insert "\n"))
              (insert file)
              (save-buffer))))
        (entropy/emacs-message-do-message
         "%s %s"
         (blue "🠶 [Pdumper] load-file:")
         (yellow feature-name))
        (let ((inhibit-message t))
          (ignore-errors
            (require feature)))))))

(defun entropy/emacs-pdumper--load-files (arg-list)
  (cl-loop for (load-dir . load-files) in arg-list
           do (entropy/emacs-pdumper--load-files-core
               load-dir load-files)))

;; ** main
;; *** load-files
(defvar entropy/emacs-pdumper--load-alist
  `((,entropy/emacs-pdumper--upstream-top-dir
     . ,(entropy/emacs-pdumper--extract-upstream-packages))
    (,nil . ,(entropy/emacs-pdumper--extract-internal-packages))
    (,nil . ,(entropy/emacs-pdumper--extract-org-packages))
    (,entropy/emacs-site-lisp-path
     . ,(entropy/emacs-pdumper--extract-eemacs-deps-packages))))

(entropy/emacs-pdumper--load-files entropy/emacs-pdumper--load-alist)

;; *** idle recovery
(setq
 entropy/emacs-pdumper-pre-lpth
 (copy-tree load-path)
 entropy/emacs-pdumper--rec-timer
 (run-with-idle-timer 1.1 t #'entropy/emacs-pdumper--recovery))

;; * provide
(provide 'entropy-emacs-pdumper)
