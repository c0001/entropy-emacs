;;; entropy-emacs-pdumper.el --- procedure wrapper for pdumper process of entropy-emacs  -*- lexical-binding: t; -*-
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
(!eemacs-require 'entropy-emacs-defvar)
(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-defun)
(!eemacs-require 'entropy-emacs-defconst)
(eval-when-compile (require 'rx))

;; ** defvar
(defvar entropy/emacs-pdumper--upstream-els nil)
(defvar entropy/emacs-pdumper--upstream-top-dir
  (progn (entropy/emacs-set-package-user-dir)
         package-user-dir))

(defvar entropy/emacs-pdumper--loads-log-file
  (file-truename
   ;; using truename for opening log file for prevent link chase warning in cli
   (expand-file-name
    (format "eemacs-pdumper-log/pdumper-loads-log/pdumper-loads-log_%s.txt"
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
             (append (entropy/emacs-list-dir-subdirs-recursively-for-list ,top-dir)
                     entropy/emacs-origin-load-path))
            ((and ,top-dir
                  (listp ,top-dir))
             (let ((rtn entropy/emacs-origin-load-path))
               (dolist (path ,top-dir)
                 (setq rtn
                       (append
                        (and path
                             (entropy/emacs-list-dir-subdirs-recursively-for-list path))
                        rtn)))
               rtn))
            (t
             entropy/emacs-origin-load-path))))
     ,(entropy/emacs-macroexp-progn body)))

;; *** libs

(let (_initp)
  (defun entropy/emacs-pdumper--recover-load-path ()
    (unless _initp
      (setq load-path entropy/emacs-pdumper-pre-lpth
            _initp t))))

(defun entropy/emacs-pdumper--recover-font-lock ()
  (unless (bound-and-true-p global-font-lock-mode)
    (entropy/emacs-message-do-message (green "Enabling fontlocks ..."))
    (global-font-lock-mode +1))
  (unless (bound-and-true-p transient-mark-mode)
    (entropy/emacs-message-do-message (green "Enabling transient ..."))
    (transient-mark-mode +1)))

;; *** extract files

(defun entropy/emacs-pdumper--extract-files-with-dir
    (top-dir exc-filters inc-filters &optional full-path)
  (let* ((files (entropy/emacs-list-dir-subfiles-recursively-for-list top-dir))
         file-name exc-passed inc-passed
         rtn)
    (dolist (file files)
      (setq file-name (if full-path file (file-name-nondirectory file))
            exc-passed t inc-passed nil)
      (catch :break
        (dolist (exc-rx exc-filters)
          (and (string-match-p exc-rx file-name)
               (throw :break (setq exc-passed nil)))))
      (catch :break
        (dolist (inc-rx inc-filters)
          (and (string-match-p inc-rx file-name)
               (throw :break (setq inc-passed t)))))
      (and exc-passed inc-passed (push file rtn)))
    (nreverse rtn)))

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
                                (seq line-start "powerline")
                                (seq line-start "spaceline")
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

                                ;; Since `lsp-mode' 20230812.1018
                                ;; ver. multi-thread is used so that
                                ;; the mutex can not be dump, so we do
                                ;; not dump `lsp-*' related packages
                                ;; anymore, see
                                ;; https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00147.html
                                (seq line-start "lsp") (seq line-start "dap-mode")

                                ))))
        (inc-filters `(,(rx (seq (or "ivy" "org" "magit" "transient" "counsel"
                                     "dired" "all-the-icon" "cal-china"
                                     "use-package" "diminish" "bind-key"
                                     "doom" "company" "entropy"
                                     "rg" "wgrep" "dashboard"
                                     "youdao" "google" "bing"
                                     "projectile" "flycheck" "company"
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
                             (seq (or ".el" ".elc") line-end))
                        ,(rx (seq line-start "company-en-words")
                             (seq (or ".el" ".elc") line-end)))))
    (entropy/emacs-pdumper--extract-files-with-dir
     eemacs-deps-top-dir
     exc-filters inc-filters)))

(defun entropy/emacs-pdumper--extract-eemacs-repack-builtin-packages ()
  (let ((files entropy/emacs-emacs-builtin-package-repack-flist)
        rtn fname-el fname-elc)
    (dolist (f files)
      (setq fname-el
            (concat f ".el")
            fname-elc
            (concat f ".elc"))
      (if (file-exists-p fname-elc)
          (push (list :load-method 'load :file fname-elc :load-path nil)
                rtn)
        (push (list :load-method 'load :file fname-el :load-path nil)
              rtn)))
    ;; FIXME: really need follow the orig order?
    (nreverse rtn)))

(defun entropy/emacs-pdumper--extract-org-packages ()
  (let* ((org-dir (file-name-directory (locate-library "org")))
         (lite-list (entropy/emacs-list-dir-lite org-dir))
         (file-list (mapcar
                     (lambda (x)
                       (let ((ftype (car x)))
                         (when (eq 'file ftype)
                           (cdr x))))
                     lite-list))
         rtn)
    (dolist (file file-list)
      (when (string-match-p "\\.elc$" file)
        (push file rtn)))
    rtn))

;; *** recovery

;; NOTE: declare customize variable first before lexical using to
;; prevent internal error like "Defining as dynamic an already lexical
;; var"
(defvar bookmark-watch-bookmark-file)
(defun entropy/emacs-pdumper--recovery ()
  (setq entropy/emacs-run-startup-pdumper-hooks-init-timestamp
        (current-time))
  (let (
        ;; silent bookmark reload prompt prevent session load fatal.
        (bookmark-watch-bookmark-file 'silent))
    (unless (entropy/emacs-in-pdumper-procedure-p)

      ;; reset main frame indicator to current session follow eemacs
      ;; internal api.
      (setq entropy/emacs-main-frame (selected-frame))

      ;; FIXME: restore the saved `load-path' sicne pdumper session
      ;; will lost the `load-path' within the dump procedure
      (entropy/emacs-pdumper--recover-load-path)

      ;; disable bar ui features before any procedur
      (entropy/emacs-ui-disable-emacs-bar-refer-uifeature)

      ;; TODO ...body

      (entropy/emacs-message-do-message (blue "Initializing pdumper session ..."))
      ;; fast up bootstrap
      (setq gc-cons-threshold most-positive-fixnum)
      (entropy/emacs-themes-init-setup-user-theme)
      (entropy/emacs-pdumper--recover-font-lock)

      (message "****** Run intial hooks *****")
      ;; the pdumper session procedure
      (run-hooks 'entropy/emacs-pdumper-load-hook)
      ;; trail dealing
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
(defun entropy/emacs-pdumper--load-files-core (top-dir files)
  (entropy/emacs-pdumper--with-load-path
   top-dir
   (let (fails)
     (dolist (file files)
       ;; file can be a string or a plist formed as
       ;;
       ;; ``` elisp
       ;; (:file file :load-method load-method :load-path load-path)
       ;; ````
       ;;
       ;; where =file= is a string, =load-method= is a function take
       ;; one arg i.e. the file to load the file into emacs lisp
       ;; runtime (default use `require' to the file basename so the
       ;; feature do not load twice, if you want to explicit load the
       ;; file use `load' instead) and the =load-path= can be a local
       ;; dir path string or a list of them which where be add to the
       ;; top of the current `load-path'.
       (let* ((file-loadpath nil)
              (file-loadmethod nil)
              (file
               (cond
                ((entropy/emacs-common-plistp file)
                 (setq file-loadpath
                       (plist-get file :load-path)
                       file-loadmethod
                       (plist-get file :load-method))
                 (or (plist-get file :file)
                     (entropy/emacs-error-without-debugger
                      "[pdumper] wrong type of load file plist obj")))
                (t file)))
              (feature-name (file-name-base file))
              (feature (intern feature-name))
              (msg
               (format ":file '%s' :file-loadmethod '%s' :file-loadpath '%s'"
                       file file-loadmethod file-loadpath)))
         (let ((inhibit-read-only t) (backup-inhibited t))
           (if (not (file-exists-p entropy/emacs-pdumper--loads-log-file))
               (with-temp-buffer
                 (erase-buffer)
                 (goto-char (point-min))
                 (insert msg)
                 (entropy/emacs-write-file
                  entropy/emacs-pdumper--loads-log-file))
             (with-current-buffer (find-file-noselect entropy/emacs-pdumper--loads-log-file)
               (goto-char (point-max))
               (when (looking-back "^.+" (line-beginning-position))
                 (insert "\n"))
               (insert msg)
               (save-buffer))))
         (entropy/emacs-message-do-message
          "%s %s"
          (blue "🠶 [Pdumper] load-file:")
          (yellow feature-name))
         (let ((inhibit-message t))
           (condition-case error
               (let ((load-path
                      (if file-loadpath
                          (if (listp file-loadpath)
                              (append
                               file-loadpath
                               load-path)
                            (cons file-loadpath load-path))
                        load-path)))
                 (cond
                  (file-loadmethod
                   (funcall file-loadmethod file))
                  (t (require feature))))
             (error
              (let ((inhibit-message nil))
                (message "error: %s" error)
                (push (cons file msg) fails)))))))
     (entropy/emacs-when-let*-first
         ((fails) file msg (cnt 0))
       (dolist (el fails)
         (setq file (car el) msg (cdr el))
         (cl-incf cnt)
         (entropy/emacs-message-do-message
          "%s%s"
          (red (format "[%d] `%s': " cnt file))
          (yellow msg)))
       (entropy/emacs-error-without-debugger
        "Pdumper procedure preloads some files with fatal, Abort!")))))

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
     . ,(entropy/emacs-pdumper--extract-eemacs-deps-packages))
    (nil
     .
     ,(entropy/emacs-pdumper--extract-eemacs-repack-builtin-packages))
    ))

(unless (bound-and-true-p entropy/emacs-do-pdumping-with-lazy-load-p)
  (entropy/emacs-pdumper--load-files entropy/emacs-pdumper--load-alist))

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
(when (boundp 'after-pdump-load-hook)
  (add-hook 'after-pdump-load-hook
            #'entropy/emacs-pdumper--recover-load-path))

;; * provide
(provide 'entropy-emacs-pdumper)
