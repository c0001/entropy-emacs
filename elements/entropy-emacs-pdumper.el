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
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'rx)

;; ** defvar
(defvar entropy/emacs-pdumper--origin-load-path (copy-tree load-path))
(defvar entropy/emacs-pdumper--upstream-els nil)
(defvar entropy/emacs-pdumper--upstream-top-dir
  (if (entropy/emacs-package-is-upstream)
      (progn (entropy/emacs-set-package-user-dir)
             package-user-dir)
    (cond
     ((eq entropy/emacs-use-extensions-type 'submodules)
      (error "Pdumper can not dump extensions with submodule
archived option of `entropy/emacs-use-extensions-type'"))
     (t (error "Wrong type of extension type chosen '%s' for pdumper"
               entropy/emacs-use-extensions-type)))))

;; timers
(defvar entropy/emacs-pdumper--rec-timer nil
  "Timer after load pdumper session used for initialize rest
configuration.")

;; ** library
;; *** macro
(cl-defmacro entropy/emacs-pdumper--with-load-path (top-dir &rest body)
  (let* ((tempo-lpth (append
                      (if (not (null (symbol-value top-dir)))
                          (entropy/emacs-list-dir-recursive-for-list
                           (entropy/emacs-eval-macro-arg top-dir))
                        nil)
                      entropy/emacs-origin-load-path)))
    `(let ((load-path ',tempo-lpth))
       ,@body)))

;; *** extract files

(defmacro entropy/emacs-pdumper--extract-files-with-dir (top-dir exc-filters inc-filters &optional full-path)
  `(let* ((files (entropy/emacs-list-files-recursive-for-list ,top-dir))
          rtn)
     (dolist (file files)
       (let ((file-name (if ,full-path file (entropy/emacs-file-path-parser file 'file-name)))
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
                                (seq line-start "test.el")))))
        (inc-filters `(,(rx (seq (or "ivy" "org" "magit" "counsel"
                                     "dired" "all-the-icon"
                                     "use-package" "diminish" "bind-key"
                                     (seq line-start "company-"))
                                 (* any)
                                 (seq ".elc" line-end))))))
    (entropy/emacs-pdumper--extract-files-with-dir
     entropy/emacs-pdumper--upstream-top-dir
     exc-filters
     inc-filters
     t)))

(defun entropy/emacs-pdumper--extract-internal-packages ()
  (let ((exc-filters `(,(rx (regexp (eval (regexp-quote (file-name-directory (locate-library "org"))))))
                       ,(rx (or (eval
                                 (if (not sys/win32p)
                                     "w32"
                                   "*eemacs-exlude-feature-indicator*"))))))
        (inc-filters `(,(rx (seq (or "cl" "tramp" "file" "dired" "url" "eww" "eshell" "esh")
                                 (* any)
                                 (or (seq ".elc" line-end)
                                     (seq ".el.gz" line-end)))))))
    (entropy/emacs-pdumper--extract-files-with-dir
     (file-name-directory (locate-library "window"))
     exc-filters
     inc-filters)))

(defun entropy/emacs-pdumper--extract-eemacs-deps-packages ()
  (let* ((eemacs-deps-files (entropy/emacs-list-files-recursive-for-list
                             (expand-file-name "elements/submodules" entropy/emacs-ext-deps-dir)))
         (exc-filter (rx (seq line-start
                              (or "liberime"
                                  "fakecygpty"
                                  "font-lock"
                                  "test")
                              (* any))))
         (inc-filter (rx (seq line-start "entropy-" (* any))
                         (seq ".el" line-end)))
         rtn)
    (dolist (file eemacs-deps-files)
      (when (and (not (string-match-p exc-filter (file-name-nondirectory file)))
                 (string-match-p inc-filter (file-name-nondirectory file)))
        (push file rtn)))
    rtn))

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
    
    (message "Initializing pdumper session ...")
    ;; the pdumper session procedure
    (run-hooks 'entropy/emacs-pdumper-load-hook)
    ;; trail dealing
    (load-library "tramp")              ;reload tramp for enable `auto-sudoedit'
    (when scroll-bar-mode
      (scroll-bar-mode 0))
    ;; the very ending procedure
    (run-hooks 'entropy/emacs-pdumper-load-end-hook)
    (message "Initialized pdumper session")
    (when entropy/emacs-pdumper--rec-timer
      (cancel-timer entropy/emacs-pdumper--rec-timer)
      (setq entropy/emacs-custom-pdumper-do nil)
      (setq gc-cons-threshold entropy/emacs-gc-threshold-basic)
      (garbage-collect))
    (defun entropy/emacs-pdumper--recovery ()
      nil)))

;; ** load-files
(defmacro entropy/emacs-pdumper--load-files-core (top-dir files)
  `(entropy/emacs-pdumper--with-load-path
    ,top-dir
    (dolist (file ,files)
      (let* ((feature-name (file-name-base file))
             (feature (intern feature-name)))
        (message "[Pdumper] load-file: %s" feature-name)
        (ignore-errors (require feature))))))

(eval-when-compile
  (defun entropy/emacs-pdumper--load-files (arg-list)
    (cl-loop for (load-dir . load-files) in arg-list
             do (entropy/emacs-pdumper--load-files-core
                 load-dir load-files))))

;; ** main

(entropy/emacs-pdumper--load-files
 `((,entropy/emacs-pdumper--upstream-top-dir . ,(entropy/emacs-pdumper--extract-upstream-packages))
   (,nil . ,(entropy/emacs-pdumper--extract-internal-packages))
   (,nil . ,(entropy/emacs-pdumper--extract-org-packages))
   (,(expand-file-name "elements/submodules" entropy/emacs-ext-deps-dir)
    . ,(entropy/emacs-pdumper--extract-eemacs-deps-packages))))


(setq load-path (append
                 (entropy/emacs-list-subdir
                  entropy/emacs-pdumper--upstream-top-dir)
                 entropy/emacs-pdumper--origin-load-path)
      entropy/emacs-pdumper-pre-lpth (copy-tree load-path)
      entropy/emacs-pdumper--rec-timer
      (run-with-idle-timer 1.1 t #'entropy/emacs-pdumper--recovery))

;; * provide
(provide 'entropy-emacs-pdumper)
