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
  (progn (entropy/emacs-set-package-user-dir)
         package-user-dir))

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
  `(let* ((load-path
           (cond
            ((stringp ,top-dir)
             (append (entropy/emacs-list-dir-recursive-for-list ,top-dir)
                     entropy/emacs-origin-load-path))
            ((and ,top-dir
                  (listp ,top-dir))
             (let ((rtn entropy/emacs-origin-load-path))
               (dolist (path ,top-dir)
                 (setq rtn
                       (append
                        (and path
                             (entropy/emacs-list-dir-recursive-for-list path))
                        rtn)))
               rtn))
            (t
             entropy/emacs-origin-load-path))))
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
         ;; we do not allow any exclution detected
         (setq exc-passed (not (member t exc-passed)))

         (dolist (inc-rx ,inc-filters)
           (if (string-match-p inc-rx file-name)
               (push t inc-passed)
             (push nil inc-passed)))
         ;; vise versa, we allow any inclusion detected
         (setq inc-passed (member t inc-passed))

         (when (and exc-passed inc-passed)
           (push file rtn))))
     rtn))

(defun entropy/emacs-pdumper--extract-upstream-packages ()
  (let ((exc-filters `(,(rx (or (seq "autoloads.el" line-end)
                                (seq "pkg.el" (? "c") line-end)
                                (seq line-start "test.el")
                                (seq line-start "doom-themes-ext")
                                ;; vterm is an dynamic module which
                                ;; can not be load while pdumper
                                ;; session.
                                (seq line-start "vterm")
                                ;; doom-modeline has such more dirty
                                ;; hack for emacs which may not needed
                                ;; for clean emacs session init and
                                ;; for fast emacs performance issue
                                (seq line-start "doom-modeline")
                                ;; `counsel-ffdata''s defcustom for
                                ;; `counsel-ffdata-database-path' have
                                ;; bug
                                "counsel-ffdata"
                                ;; `mgit-ediff' need emacs window
                                ;; system initialized, so that we can
                                ;; not force load it until pdumper
                                ;; done.
                                "magit-ediff"

                                ;; disable it since its obsolete but
                                ;; still scratch of studying
                                (seq line-start "maple-preview")

                                ))))
        (inc-filters `(,(rx (seq (or "ivy" "org" "magit" "transient" "counsel"
                                     "dired" "all-the-icon" "cal-china"
                                     "use-package" "diminish" "bind-key"
                                     "doom" "company" "entropy"
                                     "rg" "wgrep" "dashboard"
                                     "youdao" "google" "bing"
                                     "projectile" "flycheck" "lsp" "dap" "company"
                                     (seq line-start "slime"))
                                 (? "-")
                                 (* any)
                                 (seq ".elc" line-end))))))
    (entropy/emacs-pdumper--extract-files-with-dir
     entropy/emacs-pdumper--upstream-top-dir
     exc-filters
     inc-filters)))

(defun entropy/emacs-pdumper--extract-eemacs-deps-packages ()
  (let* ((eemacs-deps-top-dir
          entropy/emacs-site-lisp-path)
         (exc-filters `(,(rx (or (seq line-start
                                      (or "liberime"
                                          "fakecygpty"
                                          "font-lock"
                                          "test")
                                      (* any)
                                      line-end)
                                 (seq "pkg.el" line-end)))))
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
  (let (
        ;; silent bookmark reload prompt prevent session load fatal.
        (bookmark-watch-bookmark-file 'silent))
    (unless (entropy/emacs-in-pdumper-procedure-p)
      (setq load-path entropy/emacs-pdumper-pre-lpth)
      (unless (catch :exit
                ;; we should judge whether the bar feature supported in
                ;; current emacs build.
                (dolist (func '(scroll-bar-mode
                                tool-bar-mode
                                menu-bar-mode))
                  (unless (fboundp func)
                    (throw :exit t))))
        (scroll-bar-mode 0)
        (tool-bar-mode 0)
        (menu-bar-mode 0)
        (redisplay t))

      ;; TODO ...body

      (entropy/emacs-message-do-message (blue "Initializing pdumper session ..."))
      ;; fast up bootstrap
      (setq gc-cons-threshold most-positive-fixnum)
      (entropy/emacs-themes-init-setup-user-theme)
      (entropy/emacs-message-do-message (green "Enabling fontlocks ..."))
      (global-font-lock-mode +1)
      (transient-mark-mode +1)
      (entropy/emacs-message-do-message (green "Enable fontlocks done"))

      (message "****** Run intial hooks *****")
      ;; the pdumper session procedure
      (run-hooks 'entropy/emacs-pdumper-load-hook)
      ;; trail dealing
      (load-library "tramp")              ;reload tramp for enable `auto-sudoedit'
      (setq entropy/emacs-fall-love-with-pdumper nil)
      (entropy/emacs-message-do-message (green "Initialized pdumper session"))
      (when entropy/emacs-pdumper--rec-timer
        (cancel-timer entropy/emacs-pdumper--rec-timer)
        (setq entropy/emacs-pdumper--rec-timer nil)
        ;; recover the gc restriction after bootstrap
        (setq gc-cons-threshold entropy/emacs-gc-threshold-basic)
        (garbage-collect))
      (defun entropy/emacs-pdumper--recovery ()
        nil)
      ;; finally run start-end hook
      (entropy/emacs-run-startup-end-hook))))

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
          (condition-case error
              (require feature)
            (error
             (let ((inhibit-message nil))
               (message "error: %s" error)))))))))

(defun entropy/emacs-pdumper--load-files (arg-list)
  (cl-loop for (load-dir . load-files) in arg-list
           do (entropy/emacs-pdumper--load-files-core
               load-dir load-files)))

;; ** main
;; *** load-files
(defvar entropy/emacs-pdumper--load-alist
  `((,entropy/emacs-pdumper--upstream-top-dir
     . ,(entropy/emacs-pdumper--extract-upstream-packages))
    (,nil . ,(entropy/emacs-pdumper--extract-org-packages))
    ((,entropy/emacs-site-lisp-path ,entropy/emacs-pdumper--upstream-top-dir)
     . ,(entropy/emacs-pdumper--extract-eemacs-deps-packages))))

(entropy/emacs-pdumper--load-files entropy/emacs-pdumper--load-alist)

;; *** idle recovery
(setq
 entropy/emacs-pdumper-pre-lpth
 (copy-tree load-path)
 entropy/emacs-pdumper--rec-timer
 (run-with-idle-timer
  0.1
  ;; NOTE: do not use repeat idle here since interaction prompt may
  ;; hold the recovery procedure thus the repeat idle call will messy
  ;; the current interaction.
  nil
  #'entropy/emacs-pdumper--recovery))

;; * provide
(provide 'entropy-emacs-pdumper)
