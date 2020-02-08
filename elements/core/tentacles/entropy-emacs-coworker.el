;;; entropy-emacs-coworker.el --- entropy-emacs third party coworkers configuration
;;
;; * Copyright (C) 2019  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
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
;; Third-party components install library for `entropy-emacs'. Using
;; nodejs's 'npm' or 'composer' for php even for 'pip' the package
;; manager for python to install them.
;;
;; * Configuration:
;;
;; Designed for entropy-emacs only.
;; 
;; * Code:
;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-message)

;; ** library
(defun entropy/emacs-coworker--coworker-install-warn (task-name proc-buffer)
  (with-current-buffer proc-buffer
    (goto-char (point-min))
    (if (re-search-forward (rx (or (regexp "[^a-z]Error") "ERR!" "No matching"))
                           nil t)
        (let ((debug-on-error nil)
              (stdrr (buffer-substring-no-properties (point-min) (point-max))))
          (when noninteractive
            (entropy/emacs-message-do-message
             "%s"
             (red stdrr)))
          (princ "\n")
          (if (not noninteractive)
              (error
               (format 
                "%s %s %s %s %s"
                "Fatal do with coworker install task for"
                (format "<%s>" task-name)
                ", check callback buffer"
                (format "'%s'" (buffer-name (get-buffer proc-buffer)))
                "for more details"))
            (error (format "Fatal during task '%s'" task-name))))
      (entropy/emacs-message-do-message
       "%s %s %s"
       (green "Install coworker")
       (yellow (format "<%s>" task-name))
       (green "successfully")))))

(defun entropy/emacs-coworker--coworker-bins-execp (bins)
  (let (rtn)
    (catch :exit
      (dolist (el bins)
        (unless (executable-find el)
          (setq rtn t)
          (throw :exit nil))))
    (if rtn nil t)))

(defun entropy/emacs-coworker--coworker-files-existp (files)
  (let (rtn)
    (catch :exit
      (dolist (el files)
        (unless (file-exists-p el)
          (setq rtn t)
          (throw :exit nil))))
    (if rtn nil t)))

(defun entropy/emacs-coworker--coworker-alist-judge (svalist)
  (let (rtn)
    (catch :exit
      (dolist (el svalist)
        (cond ((eq 'file (car el))
               (unless (entropy/emacs-coworker--coworker-files-existp (cdr el))
                 (setq rtn t)
                 (throw :exit nil)))
              ((eq 'exec (car el))
               (unless (entropy/emacs-coworker--coworker-bins-execp (cdr el))
                 (setq rtn t)
                 (throw :exit nil))))))
    (if rtn nil t)))

(defmacro entropy/emacs-coworker--coworker-search
    (task-name-string task-buffer server-alist install-cmd
               before-install after-install proc-workdir
               &rest install-args)
  `(let ((task-buffer (get-buffer-create ,task-buffer)))
     (with-current-buffer task-buffer
       (when buffer-read-only
         (read-only-mode 0))
       (goto-char (point-min))
       (erase-buffer))
     (funcall ,before-install)
     (if (not (entropy/emacs-coworker--coworker-alist-judge ,server-alist))
         (let ((default-directory ,proc-workdir))
           (entropy/emacs-message-do-message
            "%s %s %s"
            (green "Do coworker install task")
            (yellow (format "<%s>" ,task-name-string))
            (green "..."))
           (sleep-for 2)
           (call-process ,install-cmd nil ,task-buffer t ,@install-args)
           (entropy/emacs-coworker--coworker-install-warn
            ,task-name-string task-buffer))
       (entropy/emacs-message-do-message
        "%s %s %s"
        (green "Coworker install task")
        (yellow (format "<%s>" ,task-name-string))
        (green "has been installed")))
     (funcall ,after-install)))

(defmacro entropy/emacs-coworker--coworker-install-by-npm
    (server-name-string server-bins server-repo-string)
  (if (not (or sys/win32p sys/cygwinp))
      `(entropy/emacs-coworker--coworker-search
        ,server-name-string
        (concat "*eemacs " ,server-name-string " install*")
        (list (cons 'file
                    (mapcar
                     (lambda (x)
                       (expand-file-name
                        (format "node_modules/.bin/%s" x)
                        entropy/emacs-coworker-lib-host-root))
                     ',server-bins)))
        "npm"
        (lambda ()
          (mkdir entropy/emacs-coworker-bin-host-path t)
          (mkdir (expand-file-name "node_modules" entropy/emacs-coworker-lib-host-root) t)
          (let ((lock-package-file
                 (expand-file-name "package-lock.json" entropy/emacs-coworker-lib-host-root)))
            (when (file-exists-p lock-package-file)
              (delete-file lock-package-file t))))
        (lambda ()
          (dolist (el ',server-bins)
            (make-symbolic-link (expand-file-name
                                 (format "node_modules/.bin/%s" el)
                                 entropy/emacs-coworker-lib-host-root)
                                (expand-file-name
                                 el
                                 entropy/emacs-coworker-bin-host-path)
                                t))
          (let ((lock-package-file
                 (expand-file-name "package-lock.json" entropy/emacs-coworker-lib-host-root)))
            (when (file-exists-p lock-package-file)
              (delete-file lock-package-file t))))
        entropy/emacs-coworker-lib-host-root
        "install" ,server-repo-string)
    `(entropy/emacs-coworker--coworker-search
      ,server-name-string
      (concat "*eemacs " ,server-name-string " install*")
      (list (cons 'exec ',server-bins))
      "npm" (lambda () nil) (lambda () nil)
      default-directory
      "install" "-g" ,server-repo-string)))

(defmacro entropy/emacs-coworker--coworker-install-by-pip
    (server-name-string server-bins server-repo-string)
  (if (not (or sys/win32p sys/cygwinp))
      `(entropy/emacs-coworker--coworker-search
        ,server-name-string
        (format "*eemacs %s install <pip>*" ,server-name-string)
        (list (cons 'file
                    (mapcar
                     (lambda (x)
                       (expand-file-name
                        x
                        entropy/emacs-coworker-bin-host-path))
                     ',server-bins)))
        "pip"
        (lambda () nil)
        (lambda ()
          (dolist (el ',server-bins)
            (unless (file-exists-p
                     (expand-file-name el entropy/emacs-coworker-bin-host-path))
              (make-symbolic-link
               (expand-file-name el
                                 (expand-file-name "bin" entropy/emacs-coworker-host-root))
               (expand-file-name el entropy/emacs-coworker-bin-host-path)
               t))))
        default-directory
        "--isolated"
        "install" "-I"
        ,server-repo-string "--prefix" entropy/emacs-coworker-host-root
        "--no-compile")
    `(entropy/emacs-coworker--coworker-search
      ,server-name-string
      (format "*eemacs %s install <pip>*" ,server-name-string)
      (list (cons 'exec ',server-bins))
      "pip"
      (lambda () nil)
      (lambda () nil)
      default-directory
      "install" ,server-repo-string "--no-compile")))

;; ** instances

(defun entropy/emacs-coworker-check-tern-server (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "tern-server-install" ("tern") "tern"))

(defun entropy/emacs-coworker-check-web-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "html-lsp-server" ("html-languageserver") "vscode-html-languageserver-bin")
  (entropy/emacs-coworker--coworker-install-by-npm
   "css-lsp-server" ("css-languageserver") "vscode-css-languageserver-bin"))

(defun entropy/emacs-coworker-check-js-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "typescript-base"
   ("tsc" "tsserver")
   "typescript")
  (entropy/emacs-coworker--coworker-install-by-npm
   "js-lsp-server"
   ("typescript-language-server")
   "typescript-language-server"))

(defun entropy/emacs-coworker-check-php-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "php-lsp-server" ("intelephense") "intelephense"))

(defun entropy/emacs-coworker-check-clangd-lsp (&rest _)
  (interactive)
  (if (executable-find "clangd")
      (entropy/emacs-message-do-message
       "%s %s %s"
       (green "Installed lsp server")
       (yellow "'<clangd>'")
       (green "."))
    (error "Please using system package management install '<clangd>'.")))

(defun entropy/emacs-coworker-check-python-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-pip
   "pyls-lsp" ("pyls") "python-language-server"))

(defun entropy/emacs-coworker-check-bash-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "bash-language-server"
   ("bash-language-server")
   "bash-language-server"))

(defun entropy/emacs-coworker-check-json-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "json-lsp"
   ("vscode-json-languageserver")
   "vscode-json-languageserver"))

;; * provide
(provide 'entropy-emacs-coworker)
