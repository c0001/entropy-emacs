;;; entropy-emacs-path.el --- entropy-emacs path setting  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190603  Enropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-path.el
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
;; `entroy-emacs' intergrated sets of shell command tool as extension
;; component for server base, communication transform etc. Emacs has
;; its owns path management as like one does of one operation system,
;; thus the path config for emacs was the aim of this configuration.
;;
;; emacs run based on system 'shell path' which all copied it into
;; emacs =env= and can be appending for other specified ones through
;; the way of using function `setenv'. Further more, emacs has its
;; other path configuration indicated  by `exec-path' for its
;; subprogress feature.
;;
;; `entropy-emacs' given various external tools enabling and path
;; sepcific customizalble variable for do both for 'shell-path' and
;; 'exec-pah', they are ordered by especially form for preventing
;; coverring type.
;;
;; In those path setting, win32 specific ones are treated specially
;; in `entropy-emacs' of using Msys2 (https://www.msys2.org/) as the
;; *NIX posix emulator, see those customizable variables doc-string
;; for detailes in group `entropy/emacs-win'.
;;
;; * Configuration:
;;
;; Loaing by `entropy-emacs' automaticaly without hacking warranty.
;;
;; * Code:
;;
;; ** require
(!eemacs-require 'entropy-emacs-defconst)
(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-defun)

;; ** variable

;; ** library
;; *** common path setting
(defun entropy/emacs-path--common-path-register ()
  (let (coworker-exec-exists-p
        coworker-env-exists-p
        (coworker-bin-path (entropy/emacs-file-path-parser
                            entropy/emacs-coworker-bin-host-path
                            'non-trail-slash))
        (env (getenv "PATH"))
        (exec-path-from-shell-variables
         entropy/emacs-willing-copy-shell-variables))
    ;; Firstly we copy the entire shell ENV
    (when exec-path-from-shell-variables
      (exec-path-from-shell-initialize))
    ;; Then we inject eemaacs spec ENV
    (when (string-match-p coworker-bin-path env)
      (setq coworker-env-exists-p t))
    (mapc
     (lambda (x)
       (when (string-match-p coworker-bin-path x)
         (setq coworker-exec-exists-p t)))
     exec-path)
    (unless coworker-exec-exists-p
      (add-to-list 'exec-path entropy/emacs-coworker-bin-host-path))
    (unless coworker-env-exists-p
      (setenv "PATH" (concat entropy/emacs-coworker-bin-host-path
                             (if sys/win32p ";" ":")
                             (getenv "PATH"))))))

;; *** w32 path set library
;; **** subrs
(defvar entropy/emacs-path-win32-shell-path-register nil)
(setq entropy/emacs-path-win32-shell-path-register
      `((:trigger entropy/emacs-win-emacs-bin-path-add
                  :path invocation-directory
                  :env-order 1
                  :exec-order 1)
        (:trigger entropy/emacs-win-fakecygpty-enable
                  :path entropy/emacs-win-fakecygpty-path
                  :env-order 2
                  :exec-order 2)
        (:trigger entropy/emacs-win-portable-mingw-enable
                  :path entropy/emacs-win-portable-mingw-bin-path
                  :env-order 3
                  :exec-order 3)
        (:trigger entropy/emacs-git-portable-enable
                  :path entropy/emacs-git-portable-path
                  :env-order 4
                  :exec-order 4)
        (:trigger entropy/emacs-win-portable-texlive-enable
                  :path entropy/emacs-win-portable-texlive-path
                  :env-order 5
                  :exec-order 5)
        (:trigger entropy/emacs-win-portable-php-enable
                  :path entropy/emacs-win-portable-php-path
                  :env-order 6
                  :exec-order 6)
        (:trigger entropy/emacs-win-portable-python-enable
                  :path entropy/emacs-win-portable-pip-host-path
                  :env-order 7
                  :exec-order 7)
        (:trigger entropy/emacs-win-portable-python-enable
                  :path entropy/emacs-win-portable-python-installation-host-path
                  :env-order 8
                  :exec-order 8)
        (:trigger entropy/emacs-win-portable-grep-enable
                  :path entropy/emacs-win-portable-grep-path
                  :env-order 9
                  :exec-order 9)
        (:trigger entropy/emacs-win-portable-ag-enable
                  :path entropy/emacs-win-portable-ag-path
                  :env-order -1
                  :exec-order -1)
        (:trigger entropy/emacs-win-portable-rg-enable
                  :path entropy/emacs-win-portable-rg-path
                  :env-order 10
                  :exec-order 10)
        (:trigger entropy/emacs-win-portable-nodejs-enable
                  :path entropy/emacs-win-portable-nodejs-installation-host-path
                  :env-order 11
                  :exec-order 11)
        (:trigger entropy/emacs-win-portable-opencc-enable
                  :path entropy/emacs-win-portable-opencc-path
                  :env-order 12
                  :exec-order 12)
        (:trigger entropy/emacs-win-portable-pandoc-enable
                  :path entropy/emacs-win-portable-pandoc-path
                  :env-order 13
                  :exec-order 13)
        (:trigger entropy/emacs-win-portable-jdk-enable
                  :path entropy/emacs-win-portable-jdk-path
                  :env-order 14
                  :exec-order 14)
        (:trigger entropy/emacs-win-portable-zeal-enable
                  :path entropy/emacs-win-portable-zeal-path
                  :env-order 15
                  :exec-order 15)
        (:trigger entropy/emacs-win-portable-putty-enable
                  :path entropy/emacs-win-portable-putty-path
                  :env-order 16
                  :exec-order 16)
        (:trigger entropy/emacs-microsoft-windows-unix-emulator-enable-extra
                  :path ,(expand-file-name "usr/bin" entropy/emacs-microsoft-windows-unix-emulator-extra-root-path)
                  :env-order 17
                  :exec-order 17
                  :predicate
                  (lambda ()
                    (when (and sys/win32p
                               entropy/emacs-microsoft-windows-unix-emulator-enable-extra
                               entropy/emacs-microsoft-windows-unix-emulator-enable)
                      (setq woman-manpath
                            `(,(concat entropy/emacs-microsoft-windows-unix-emulator-extra-root-path "usr/man")
                              ,(concat entropy/emacs-microsoft-windows-unix-emulator-extra-root-path "usr/share/man")
                              ,(concat entropy/emacs-microsoft-windows-unix-emulator-extra-root-path "usr/local/man"))))))
        (:trigger entropy/emacs-microsoft-windows-unix-emulator-enable
                  :path entropy/emacs-microsoft-windows-unix-emulator-bin-path
                  :env-order 18
                  :exec-order 18)
        ))

(defun entropy/emacs-path--w32-path-sort-predicate ()
  (let* (env-paths-positive
         env-paths-negative
         exec-paths-positive
         exec-paths-negative
         sort-func-positive
         sort-func-negative
         extract-path-func)

    (setq sort-func-positive
          (lambda (a b)
            (let ((a_order (cdr a))
                  (b_order (cdr b)))
              (if (< a_order b_order)
                  nil
                t)))
          sort-func-negative
          (lambda (a b)
            (let ((a_order (abs (cdr a)))
                  (b_order (abs (cdr b))))
              (if (< a_order b_order)
                  t
                nil)))
          extract-path-func
          (lambda (x)
            (let (rtn)
              (dolist(el x)
                (push (car el) rtn))
              (nreverse rtn))))

    (dolist (el entropy/emacs-path-win32-shell-path-register)
      (let* ((trigger (plist-get el :trigger))
             (env-order (plist-get el :env-order))
             (exec-order (plist-get el :exec-order))
             (path (plist-get el :path))
             (predicate (plist-get el :predicate))
             (env-path-pair (cons path env-order))
             (exec-path-pair (cons path exec-order)))
        (when (not (null (symbol-value trigger)))
          (if (> env-order 0)
              (push env-path-pair env-paths-positive)
            (push env-path-pair env-paths-negative))
          (if (> exec-order 0)
              (push exec-path-pair exec-paths-positive)
            (push exec-path-pair exec-paths-negative)))
        ;; do node predicate
        (when (and (not (null predicate)) (functionp predicate))
          (funcall predicate))))

    (dolist (positive-list '(env-paths-positive exec-paths-positive))
      (set positive-list
           (sort (symbol-value positive-list)
                 sort-func-positive)))
    (dolist (negative-list '(env-paths-negative exec-paths-negative))
      (set negative-list
           (sort (symbol-value negative-list)
                 sort-func-negative)))

    (list :env-paths (cons (funcall extract-path-func env-paths-positive)
                           (funcall extract-path-func env-paths-negative))
          :exec-paths (cons (funcall extract-path-func exec-paths-positive)
                            (funcall extract-path-func exec-paths-negative)))))

;; **** main
(defun entropy/emacs-path--w32-regist-path ()
  (let* ((register-var (entropy/emacs-path--w32-path-sort-predicate))
         (env-paths (plist-get register-var :env-paths))
         (env-pos-paths (car env-paths))
         (env-neg-paths (cdr env-paths))
         (exec-paths (plist-get register-var :exec-paths))
         (exec-pos-paths (car exec-paths))
         (exec-neg-paths (cdr exec-paths))
         (env-orig (getenv "PATH"))
         env-pos-concat env-neg-concat)

    ;; ==================== setenv ====================
    (when (not (null env-pos-paths))
      (dolist (el env-pos-paths)
        (setq env-pos-concat
              (if (stringp env-pos-concat)
                  (concat env-pos-concat ";" (symbol-value el))
                (symbol-value el)))))

    (when (not (null env-neg-paths))
      (dolist (el env-neg-paths)
        (setq env-neg-concat
              (if (stringp env-neg-concat)
                  (concat env-neg-concat ";" (symbol-value el))
                (symbol-value el)))))

    (setenv "PATH"
            (cond
             ((and (null env-pos-concat)
                   (null env-neg-concat))
              env-orig)
             ((and (not (null env-pos-concat))
                   (null env-neg-concat))
              (concat env-pos-concat ";" env-orig))
             ((and (null env-pos-concat)
                   (not (null env-neg-concat)))
              (concat env-orig ";" env-neg-concat))
             (t
              (concat env-pos-concat ";" env-orig ";" env-neg-concat))))

    ;; ==================== exec ====================

    (when (not (null exec-pos-paths))
      (setq exec-path
            (append
             (mapcar 'symbol-value exec-pos-paths)
             exec-path)))

    (when (not (null exec-neg-paths))
      (setq exec-path
            (append exec-path
                    (mapcar 'symbol-value exec-neg-paths))))))

;; ** main

(defun entropy/emacs-path-load-main ()
  (entropy/emacs-path--common-path-register)
  (when sys/win32p
    (entropy/emacs-path--w32-regist-path)))

(entropy/emacs-path-load-main)

;; NOTE & FIXME:
;; We must init path setting after eemacs load since pdumper will
;; re-get ENV var after dump load.
(when entropy/emacs-fall-love-with-pdumper
  (entropy/emacs-lazy-initial-advice-before
   (find-file switch-to-buffer dired ivy-mode counsel-mode entropy/shellpop-start)
   "path-register" "path-register" prompt-echo
   :pdumper-no-end nil
   (entropy/emacs-path-load-main)))

;; * provide
(provide 'entropy-emacs-path)
