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
(require 'entropy-emacs-defun)

;; ** library
;; *** subroutines
;; **** individual
(defun entropy/emacs-coworker--coworker-message-install-success (task-name-string)
  (entropy/emacs-message-do-message
   "%s %s %s"
   (green "Coworker install task")
   (yellow (format "<%s>" task-name-string))
   (green "Installed successfully!")))

(defun entropy/emacs-coworker--coworker-message-existed (task-name-string)
  (entropy/emacs-message-do-message
   "%s %s %s"
   (green "Coworker install task")
   (yellow (format "<%s>" task-name-string))
   (green "has been installed")))

(defun entropy/emacs-coworker--coworker-message-do-task (task-name-string)
  (entropy/emacs-message-do-message
   "%s %s %s"
   (green "Do coworker install task")
   (yellow (format "<%s>" task-name-string))
   (green "...")))

(defun entropy/emacs-coworker--coworker-alist-judge (svalist)
  (let (rtn
        (file-judge-func
         (lambda (files)
           (let (rtn)
             (catch :exit
               (dolist (el files)
                 (unless (file-exists-p el)
                   (setq rtn t)
                   (throw :exit nil))))
             (if rtn nil t))))
        (exec-judge-func
         (lambda (bins)
           (let (rtn)
             (catch :exit
               (dolist (el bins)
                 (unless (executable-find el)
                   (setq rtn t)
                   (throw :exit nil))))
             (if rtn nil t)))))
    (catch :exit
      (dolist (el svalist)
        (cond ((eq 'file (car el))
               (unless (funcall file-judge-func (cdr el))
                 (setq rtn t)
                 (throw :exit nil)))
              ((eq 'exec (car el))
               (unless (funcall exec-funge-func (cdr el))
                 (setq rtn t)
                 (throw :exit nil))))))
    (if rtn nil t)))

(defun entropy/emacs-coworker--common-proc-warn (proc-buffer)
  (with-current-buffer proc-buffer
    (entropy/emacs-message-do-message
     "%s"
     (red
      (buffer-substring-no-properties
       (point-min)
       (point-max)))))
  (if noninteractive
      (error "")
    (switch-to-buffer "*Messages*")
    (top-level)))

;; **** Lsp callers refactory
;; ***** pypi
;; ****** patch python installed bins for import portable "sys.path"
(defun entropy/emacs-coworker--patch-python-bin-for-pythonpath (pyfile site-packages-path)
  "Patch python file PYFILE for add =sys.path= for SITE-PACKAGES-PATH"
  (unless (file-exists-p pyfile)
    (error "Pyfile '%s' not existed!" pyfile))
  (with-current-buffer (find-file-noselect pyfile)
    (let ((inhibit-read-only t)
          (inst-str (format "sys.path.insert(0,\"%s\")"
                            (expand-file-name site-packages-path)))
          (inst-entry-regexp "^import sys")
          (inst-new nil))
      (goto-char (point-min))
      (unless (re-search-forward inst-entry-regexp nil t)
        (setq inst-new t))
      (if inst-new
          (progn
            (goto-char (point-min))
            (insert "import sys\n\n")
            (insert (concat inst-str "\n\n")))
        (goto-char (line-end-position))
        (newline)
        (insert inst-str))
      (save-buffer)
      (kill-buffer))))

;; ****** generate w32 python installed bins cmd wrapper
(defun entropy/emacs-coworker--gen-python-w32-cmd-bin (cmd-bin-file bin-file-abspath site-packages-abspath)
  "Generate windows cmd batch CMD-BIN-FILE for python callers in
windows platform and using library path SITE-PAKCAGE-ABSPATH."
  (let ((template
         "@ECHO off
CALL :find_dp0

SETLOCAL

SET \"PYTHONPATH=%s;%%PYTHONPATH%%\"
cd %s
.\\%s %%*

ENDLOCAL
EXIT /b %%errorlevel%%
:find_dp0
SET dp0=%%~dp0
EXIT /b
"
         ))

    (with-current-buffer (find-file-noselect cmd-bin-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (insert
         (format template
                 site-packages-abspath
                 (file-name-directory bin-file-abspath)
                 (file-name-base bin-file-abspath)))
        (save-buffer)
        (kill-buffer)))))
;; *** package install branches
;; **** npm install
(defun entropy/emacs-coworker--coworker-install-by-npm
    (server-name-string server-bins server-repo-string)
  (let* ((server-lostp
          (not
           (entropy/emacs-coworker--coworker-alist-judge
            (list (cons 'file
                        (mapcar
                         (lambda (x)
                           (expand-file-name
                            x
                            entropy/emacs-coworker-bin-host-path))
                         server-bins))
                  (cons 'file
                        (mapcar
                         (lambda (x)
                           (expand-file-name
                            (format "node_modules/.bin/%s" x)
                            entropy/emacs-coworker-lib-host-root))
                         server-bins))))))
         (lock-package-file
          (expand-file-name "package-lock.json" entropy/emacs-coworker-lib-host-root))
         (pkg-json-del
          `(lambda ()
             (when (file-exists-p ,lock-package-file)
               (delete-file ,lock-package-file t)))))
    (if server-lostp
        (entropy/emacs-make-process
         `(:name
           ,server-name-string
           :synchronously t
           :command '("npm" "install" ,server-repo-string)
           :buffer (get-buffer-create "*eemacs-coworker-npm-install-proc*")
           :default-directory entropy/emacs-coworker-lib-host-root
           :prepare
           (entropy/emacs-coworker--coworker-message-do-task
            ,server-name-string)
           (condition-case error
               (progn
                 (mkdir entropy/emacs-coworker-bin-host-path t)
                 (mkdir (expand-file-name "node_modules" entropy/emacs-coworker-lib-host-root) t)
                 (funcall ',pkg-json-del)
                 t)
             (error
              (error "eemacs coworker init task for '%s' did with fatal!"
                     ,server-name-string)))
           :after
           (dolist (el ',server-bins)
             (message "Make symbolic for server bin '%s' ..."  el)
             (make-symbolic-link (expand-file-name
                                  (format "node_modules/.bin/%s" el)
                                  entropy/emacs-coworker-lib-host-root)
                                 (expand-file-name
                                  el
                                  entropy/emacs-coworker-bin-host-path)
                                 t)
             (message "Make symbolic for server bin '%s' done!"  el))
           (entropy/emacs-coworker--coworker-message-install-success
            ,server-name-string)
           :error
           (with-current-buffer $sentinel/destination
             (let ((msg (buffer-substring (point-min) (point-max))))
               (entropy/emacs-message-do-message
                "%s"
                (red msg))))
           :cleanup
           (when (buffer-live-p $sentinel/destination)
             (kill-buffer $sentinel/destination))))
      (entropy/emacs-coworker--coworker-message-existed
       server-name-string))))

;; **** pip install
(defun entropy/emacs-coworker--coworker-install-by-pip
    (server-name-string server-bins server-repo-string)
  (let* ((server-lostp
          (not
           (entropy/emacs-coworker--coworker-alist-judge
            (list (cons 'file
                        (mapcar
                         (lambda (x)
                           (expand-file-name
                            x
                            entropy/emacs-coworker-bin-host-path))
                         server-bins)))))))
    (if server-lostp
        (entropy/emacs-make-process
         `(:name
           ,server-name-string
           :synchronously t
           :command
           '("pip"
             "--isolated"
             "install" "-I" ,server-repo-string
             "--prefix" ,entropy/emacs-coworker-host-root
             "--no-compile")
           :buffer (get-buffer-create "*eemacs-coworker-pip-install-proc*")
           :prepare
           (entropy/emacs-coworker--coworker-message-do-task ,server-name-string)
           t
           :after
           (let ((site-packages-path
                  (if (not sys/is-win-group)
                      (expand-file-name
                       (car (file-expand-wildcards
                             (expand-file-name "python[0-9]*/site-packages"
                                               entropy/emacs-coworker-lib-host-root))))
                    (expand-file-name "Lib/site-packages" entropy/emacs-coworker-host-root)))
                 (w32-pyexecs-dir
                  (expand-file-name "Scripts" entropy/emacs-coworker-host-root)))
             (dolist (el ',server-bins)
               (let ((bin-path
                      (expand-file-name
                       el
                       entropy/emacs-coworker-bin-host-path)))
                 (cond
                  ((not sys/is-win-group)
                   (entropy/emacs-coworker--patch-python-bin-for-pythonpath
                    bin-path site-packages-path))
                  (sys/is-win-group
                   (let ((w32-exec-name
                          (expand-file-name
                           (concat el ".exe")
                           w32-pyexecs-dir)))
                     (entropy/emacs-coworker--gen-python-w32-cmd-bin
                      (concat bin-path ".cmd")
                      w32-exec-name site-packages-path)))))))
           (entropy/emacs-coworker--coworker-message-install-success ,server-name-string)
           :error
           (with-current-buffer $sentinel/destination
             (let ((msg (buffer-substring (point-min) (point-max))))
               (entropy/emacs-message-do-message
                "%s"
                (red msg))))
           :cleanup
           (when (buffer-live-p $sentinel/destination)
             (kill-buffer $sentinel/destination))))
      (entropy/emacs-coworker--coworker-message-existed
       server-name-string))))

;; **** archive download

(defun entropy/emacs-coworker--coworker-install-by-archive-get
    (server-name-string server-archive-name server-host-url server-archive-type)
  (let* (download-cbk
         (tmp-download-fname
          (format "eemacs-coworker_random_download_file_for_%s_%s" server-archive-name (random)))
         (tmp-download-file
          (expand-file-name
           tmp-download-fname
           entropy/emacs-coworker-archive-host-root))
         (server-extract-dir (expand-file-name server-archive-name entropy/emacs-coworker-archive-host-root)))
    (unless (file-exists-p entropy/emacs-coworker-archive-host-root)
      (make-directory entropy/emacs-coworker-archive-host-root t))
    (entropy/emacs-coworker--coworker-message-do-task server-name-string)
    (if (file-exists-p server-extract-dir)
        (entropy/emacs-coworker--coworker-message-existed server-name-string)
      (message "Downloading lsp archive for '%s' ..." server-name-string)
      (setq download-cbk
            (entropy/emacs-network-download-file
             server-host-url
             tmp-download-file))
      (unless (eq (symbol-value download-cbk) 'success)
        (user-error "'%s' lsp server download with fatal!" server-name-string))
      (make-directory server-extract-dir t)
      (entropy/emacs-archive-dowith
       server-archive-type
       tmp-download-file
       server-extract-dir
       :extract)
      (entropy/emacs-coworker--coworker-message-install-success
       server-name-string))))

;; ** instances
;; *** tern
(defun entropy/emacs-coworker-check-tern-server (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "tern-server-install" '("tern") "tern"))

;; *** web
(defun entropy/emacs-coworker-check-web-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "html-lsp-server" '("html-languageserver") "vscode-html-languageserver-bin")
  (entropy/emacs-coworker--coworker-install-by-npm
   "css-lsp-server" '("css-languageserver") "vscode-css-languageserver-bin"))

;; *** js
(defun entropy/emacs-coworker-check-js-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "typescript-base"
   '("tsc" "tsserver")
   "typescript")
  (entropy/emacs-coworker--coworker-install-by-npm
   "js-lsp-server"
   '("typescript-language-server")
   "typescript-language-server")
  (entropy/emacs-coworker--coworker-install-by-npm
   "vue-lsp-server"
   '("vls")
   "vls"))

;; *** php
(defun entropy/emacs-coworker-check-php-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "php-lsp-server" '("intelephense") "intelephense"))

;; *** clangd
(defun entropy/emacs-coworker-check-clangd-lsp (&rest _)
  (interactive)
  (if (executable-find "clangd")
      (entropy/emacs-message-do-message
       "%s %s %s"
       (green "Installed lsp server")
       (yellow "'<clangd>'")
       (green "."))
    (error "Please using system package management install '<clangd>'.")))

;; *** cmake

(defun entropy/emacs-coworker-check-cmake-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-pip
   "cmake-lsp" '("cmake-language-server" "cmake-format") "cmake-language-server"))

;; *** bash
(defun entropy/emacs-coworker-check-bash-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "bash-language-server"
   '("bash-language-server")
   "bash-language-server"))

;; *** json
(defun entropy/emacs-coworker-check-json-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-npm
   "json-lsp"
   '("vscode-json-languageserver")
   "vscode-json-languageserver"))

;; *** java
(defun entropy/emacs-coworker-check-java-lsp (&rest _)
  (entropy/emacs-coworker--coworker-install-by-archive-get
   "java lsp"
   "jdt-lsp"
   "http://download.eclipse.org/jdtls/milestones/0.9.0/jdt-language-server-0.9.0-201711302113.tar.gz"
   'tgz))
(when (eq (entropy/emacs-get-use-ide-type 'java-mode) 'lsp)
  (unless entropy/emacs-ext-use-eemacs-lsparc
    (setq lsp-java-server-install-dir
          (expand-file-name
           "jdt-lsp"
           entropy/emacs-coworker-archive-host-root))))

;; *** powershell
(defun entropy/emacs-coworker-check-pwsh-lsp (&rest _)
  (entropy/emacs-coworker--coworker-install-by-archive-get
   "pwsh lsp"
   "pwsh-lsp"
   "https://github.com/PowerShell/PowerShellEditorServices/releases/download/v2.1.0/PowerShellEditorServices.zip"
   'zip))
(when (eq (entropy/emacs-get-use-ide-type 'powershell-mode) 'lsp)
  (with-eval-after-load 'lsp-pwsh       ;do not using
                                        ;`entropy/emacs-lazy-load-simple',
                                        ;thats will force load 'lsp' while pdumper procedure
    (unless (file-exists-p lsp-pwsh-log-path)
      ;; ensure that the log path exist or will make pwsh-ls start fail
      (mkdir lsp-pwsh-log-path 'create-parent)))
  (unless entropy/emacs-ext-use-eemacs-lsparc
    (setq lsp-pwsh-dir
          (expand-file-name
           "pwsh-lsp/PowerShellEditorServices"
           entropy/emacs-coworker-archive-host-root))))

;; *** python
;; **** python language server types
;; ***** pyls
(defun entropy/emacs-coworker-check-pyls-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-install-by-pip
   "pyls-lsp" '("pyls") "python-language-server"))

;; ***** pyls-ms
(defvar entropy/emacs-coworker--pyls-ms-archive-dir
  (expand-file-name
   "python-language-server"
   entropy/emacs-coworker-archive-host-root))

(defvar entropy/emacs-coworker--pyls-ms-release-name
  (format "%s-x64"
          (cond (sys/macp  "osx")
                (sys/linuxp "linux")
                (sys/win32p "win")
                (t (user-error "Unsupported system: %s" system-type)))))

(defvar entropy/emacs-coworker--pyls-ms-exec-path
  (expand-file-name
   (format "output/bin/Release/%s/publish/" entropy/emacs-coworker--pyls-ms-release-name)
   entropy/emacs-coworker--pyls-ms-archive-dir))

(defun entropy/emacs-coworker--pyls-ms-existed-p ()
  (let (rtn)
    (unless (file-exists-p
           entropy/emacs-coworker--pyls-ms-archive-dir)
      (setq rtn "not-cloned"))
    (when (null rtn)
      (unless (file-exists-p
               entropy/emacs-coworker--pyls-ms-exec-path)
        (setq rtn "not-compile")))
    (if rtn
        rtn
      t)))

(defun entropy/emacs-coworker-check-pyls-ms-lsp (&rest _)
  (let ((pyls-ms-archive
         entropy/emacs-coworker--pyls-ms-archive-dir)
        (task-name "pyls-lsp-ms")
        clone-task compile-task)
    (unless (file-exists-p entropy/emacs-coworker-archive-host-root)
      (make-directory entropy/emacs-coworker-archive-host-root t))
    (setq clone-task
          `(:name
            "git clone pyls-lsp-ms"
            :default-directory entropy/emacs-coworker-archive-host-root
            :command
            '("git" "clone" "https://github.com/Microsoft/python-language-server.git")
            :synchronously t
            :buffer (get-buffer-create "eemacs clone pyls-ms")
            :prepare
            (entropy/emacs-message-do-message
             "%s"
             (green "Git clone pyls-lsp-ms ..."))
            :error
            (entropy/emacs-coworker--common-proc-warn
             $sentinel/destination))
          compile-task
          `(:name
            "dotnet compile pyls-lsp-ms"
            :default-directory
            ,(expand-file-name "src/LanguageServer/Impl/" pyls-ms-archive)
            :command
            '("dotnet" "publish" "-c" "Release" "-r"
              ,entropy/emacs-coworker--pyls-ms-release-name)
            :synchronously t
            :buffer (get-buffer-create "eemacs compile pyls-ms")
            :prepare
            (entropy/emacs-message-do-message
             "%s"
             (green "Compile pyls-lsp-ms ..."))
            :error
            (entropy/emacs-coworker--common-proc-warn
             $sentinel/destination)
            :after
            (entropy/emacs-coworker--coworker-message-install-success
             ,task-name)))
    (let ((pyls-ms-cur-status (entropy/emacs-coworker--pyls-ms-existed-p)))
      (if (eq pyls-ms-cur-status t)
          (entropy/emacs-coworker--coworker-message-existed
           task-name)
        (entropy/emacs-coworker--coworker-message-do-task
         task-name)
        (entropy/emacs-make-chained-processes
         (cond ((string= pyls-ms-cur-status "not-cloned")
                (list clone-task
                      compile-task))
               ((string= pyls-ms-cur-status "not-compile")
                (list compile-task))))))))

(setq lsp-python-ms-dir
      (expand-file-name
       "output/bin/Release/linux-x64/publish/"
       entropy/emacs-coworker--pyls-ms-archive-dir))


;; **** main

(defun entropy/emacs-coworker-check-python-lsp (&rest _)
  (cl-case entropy/emacs-codeserver-prefer-pyls-type
    (mspyls (entropy/emacs-coworker-check-pyls-ms-lsp))
    (pyls (entropy/emacs-coworker-check-pyls-lsp))))



;; * provide
(provide 'entropy-emacs-coworker)
