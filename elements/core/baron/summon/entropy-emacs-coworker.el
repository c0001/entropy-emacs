;;; entropy-emacs-coworker.el --- entropy-emacs third party coworkers configuration  -*- lexical-binding: t; -*-
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
(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-message)
(!eemacs-require 'entropy-emacs-defun)

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
                 (unless (or (file-exists-p el)
                             ;; find file with more suffix for windows platform
                             (file-exists-p (format "%s.cmd" el))
                             (file-exists-p (format "%s.exe" el)))
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
               (unless (funcall exec-judge-func (cdr el))
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
      (entropy/emacs-noninteractive-exit-with-fatal)
    (switch-to-buffer "*Messages*")
    (entropy/emacs-silent-abort)))

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

;; ***** npm
;; ****** generate w32 node callers
(defun entropy/emacs-coworker--gen-node-w32-cmd-bin (cmd-bin-file caller-path)
  (let* ((template (with-current-buffer
                       (find-file-noselect
                        (expand-file-name "node-call_template.cmd"
                                          entropy/emacs-templates-dir))
                     (buffer-substring-no-properties (point-min) (point-max))))
         (bin-content
          (replace-regexp-in-string "\\$\\$\\$\\$COMMAND\\$\\$\\$\\$"
                                    (replace-regexp-in-string "/" "\\\\\\\\" caller-path)
                                    template t))
         (inhibit-read-only t))
    (with-current-buffer (find-file-noselect cmd-bin-file)
      (erase-buffer)
      (insert bin-content)
      (save-buffer))))

;; *** package install branches
;; **** npm install
;; ***** isolate type
(defun entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
    (server-name-string server-bins server-repo-string)
  (when sys/win32p
    (unless (and entropy/emacs-win-portable-nodejs-enable
                 (file-exists-p
                  (expand-file-name
                   "node.exe"
                   entropy/emacs-win-portable-nodejs-installation-host-path))
                 (file-exists-p
                  (expand-file-name
                   "npm.cmd"
                   entropy/emacs-win-portable-nodejs-installation-host-path)))
      (entropy/emacs-error-without-debugger "You must using 'npm' of \
`entropy/emacs-win-portable-nodejs-enable' to install coworker <%s>"
             server-name-string)))
  (let* (
         ;; NOTE:
         ;; We should use independently prefix for each node pacakge
         ;; since each pacakge should has their own dependencies and
         ;; shouldn't pollute other package's dependencies.
         (this-npm-prefix
          (expand-file-name (format "eemacs-node-lsp/%s" server-name-string)
                            entropy/emacs-coworker-lib-host-root))
         (server-lostp
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
                            this-npm-prefix))
                         server-bins))))))
         ;; (lock-package-file
         ;;  (expand-file-name "package-lock.json" this-npm-prefix))
         ;; (pkg-json-del
         ;;  `(lambda ()
         ;;     (when (file-exists-p ,lock-package-file)
         ;;       (delete-file ,lock-package-file t))))
         )
    (if server-lostp
        (entropy/emacs-make-process
         `(:name
           ,server-name-string
           :synchronously t
           :command
           '("npm"
             "install" ,server-repo-string)
           :buffer (get-buffer-create "*eemacs-coworker-npm-install-proc*")
           :default-directory ,(entropy/emacs-return-as-default-directory this-npm-prefix)
           :prepare
           (entropy/emacs-coworker--coworker-message-do-task
            ,server-name-string)
           (condition-case error
               (progn
                 (unless (executable-find "npm")
                   (error "Command npm not found in your system!"))
                 (mkdir entropy/emacs-coworker-bin-host-path t)
                 (mkdir (expand-file-name "node_modules" ,this-npm-prefix) t)
                 ;; (funcall ',pkg-json-del)
                 t)
             (error
              (entropy/emacs-error-without-debugger
               "eemacs coworker init task for '%s' did with fatal: %s"
               ,server-name-string error)))
           :after
           (dolist (el ',server-bins)
             (when sys/win32p
               (setq el (format "%s.cmd" el)))
             (message "Make final executable target for server bin '%s' ..."  el)
             (cond (sys/win32p
                    (entropy/emacs-coworker--gen-node-w32-cmd-bin
                     (expand-file-name
                      el
                      entropy/emacs-coworker-bin-host-path)
                     (expand-file-name
                      (format "node_modules/.bin/%s" el)
                      ,this-npm-prefix))
                    (message "Make w32 caller for server bin '%s' done!" el))
                   (t
                    (make-symbolic-link (expand-file-name
                                         (format "node_modules/.bin/%s" el)
                                         ,this-npm-prefix)
                                        (expand-file-name
                                         el
                                         entropy/emacs-coworker-bin-host-path)
                                        t)
                    (message "Make symbolic for server bin '%s' done!"  el))))
           (entropy/emacs-coworker--coworker-message-install-success
            ,server-name-string)
           :error
           (with-current-buffer $sentinel/destination
             (let ((msg (buffer-substring (point-min) (point-max))))
               (entropy/emacs-message-do-message
                "%s\n%s"
                (red msg)
                (red
                 "[FATAL] NPM install fatal of process <%s> with package '%s'"
                 ,server-name-string
                 ,server-repo-string))))
           :cleanup
           (when (buffer-live-p $sentinel/destination)
             (kill-buffer $sentinel/destination))))
      (entropy/emacs-coworker--coworker-message-existed
       server-name-string))))

;; **** pip install
;; ***** isolate type
(defun entropy/emacs-coworker--coworker-isolate-bins-install-by-pip
    (server-name-string server-bins server-repo-string)
  (when sys/win32p
    (unless (and entropy/emacs-win-portable-python-enable
                 (file-exists-p
                  (expand-file-name
                   "pip.exe"
                   entropy/emacs-win-portable-pip-host-path)))
      (entropy/emacs-error-without-debugger "You must using 'pip' of \
`entropy/emacs-win-portable-python-enable' to install coworker <%s>"
             server-name-string)))
  (let* (
         ;; NOTE: We should use independently prefix for each pypi
         ;; pacakge since each pacakge should has their own
         ;; dependencies and shouldn't pollute other package's
         ;; dependencies.
         (this-pip-prefix
          (expand-file-name (format "eemacs-pypi-lsp/%s" server-name-string)
                            entropy/emacs-coworker-lib-host-root))
         (server-lostp
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
           '(,(if sys/win32p
                  ;; TODO: windows python release do not have the
                  ;; python3 alias to python.exe, so we should do an
                  ;; versionn check to this later.
                  "python"
                "python3")
             "-m" "pip"
             "--isolated"
             "install" "-I" ,server-repo-string
             "--prefix" ,(if sys/win32p (replace-regexp-in-string "/" "\\\\"  this-pip-prefix)
                           this-pip-prefix)
             "--no-compile")
           :buffer (get-buffer-create "*eemacs-coworker-pip-install-proc*")
           :prepare
           (entropy/emacs-coworker--coworker-message-do-task ,server-name-string)
           (progn
             (condition-case error
                 (progn
                   (unless (executable-find "pip")
                     (error "Command pip not found in your system!"))
                   (mkdir entropy/emacs-coworker-bin-host-path t)
                   (mkdir entropy/emacs-coworker-lib-host-root t)
                   (mkdir ,this-pip-prefix t)
                   t)
               (error
                (entropy/emacs-error-without-debugger
                 "eemacs coworker init task for '%s' did with fatal for mkdir: %s"
                 ,server-name-string error))))
           :after
           (let ((site-packages-path
                  (if (not sys/win32p)
                      (expand-file-name
                       (car (file-expand-wildcards
                             (expand-file-name "lib/python[0-9]*/site-packages"
                                               ,this-pip-prefix))))
                    (let ((maybe-it-0 (expand-file-name "lib/site-packages" ,this-pip-prefix))
                          (maybe-it-1 (expand-file-name "Lib/site-packages" ,this-pip-prefix)))
                      (if (file-exists-p maybe-it-0)
                          maybe-it-0
                        maybe-it-1))))
                 (w32-pyexecs-dir
                  (expand-file-name "Scripts" ,this-pip-prefix)))
             (unless (file-exists-p site-packages-path)
               (entropy/emacs-error-without-debugger
                "[%s] pypi site-pakcages dir %s not exited, this may an eemacs insternal bug!"
                ,server-name-string site-packages-path))
             (when sys/win32p
               (unless (file-exists-p site-packages-path)
                 (entropy/emacs-error-without-debugger
                  "[%s] win32 pypi bin dir %s not exited, this may an eemacs insternal bug!"
                  ,server-name-string w32-pyexecs-dir)))
             (dolist (el ',server-bins)
               (let ((bin-path
                      (expand-file-name
                       el
                       entropy/emacs-coworker-bin-host-path))
                     (native-bin-path
                      (expand-file-name
                       (format (if sys/win32p "Scripts/%s"
                                 "bin/%s")
                               el)
                       ,this-pip-prefix)))
                 (cond
                  ((not sys/win32p)
                   (make-symbolic-link native-bin-path bin-path)
                   (entropy/emacs-coworker--patch-python-bin-for-pythonpath
                    bin-path site-packages-path))
                  (sys/win32p
                   (let ((w32-pyexec-path
                          (concat native-bin-path ".exe")))
                     (entropy/emacs-coworker--gen-python-w32-cmd-bin
                      (concat bin-path ".cmd")
                      w32-pyexec-path site-packages-path)))))))
           (entropy/emacs-coworker--coworker-message-install-success ,server-name-string)
           :error
           (with-current-buffer $sentinel/destination
             (let ((msg (buffer-substring (point-min) (point-max))))
               (entropy/emacs-message-do-message
                "%s\n%s"
                (red msg)
                (red
                 "[FATAL] PIP install fatal of process <%s> with package '%s'"
                 ,server-name-string
                 ,server-repo-string))))
           :cleanup
           (when (buffer-live-p $sentinel/destination)
             (kill-buffer $sentinel/destination))))
      (entropy/emacs-coworker--coworker-message-existed
       server-name-string))))

;; **** archive download

(defun entropy/emacs-coworker--coworker-install-by-archive-get
    (server-name-string server-archive-name server-host-url server-archive-type
                        &optional no-success-msg)
  (let* (download-cbk
         (tmp-download-host
          (expand-file-name
           "eemacs-tmp-download-cache"
           entropy/emacs-coworker-archive-host-root))
         (tmp-download-basename
          (format "eemacs-coworker_random_download_file_for_%s_%s.%s"
                  server-archive-name (random)
                  server-archive-type))
         (tmp-download-file
          (expand-file-name
           tmp-download-basename
           tmp-download-host))
         (server-extdir-or-dest
          (expand-file-name
           server-archive-name
           entropy/emacs-coworker-archive-host-root)))
    ;; Firstly make stuff directory
    (unless (file-exists-p entropy/emacs-coworker-archive-host-root)
      (make-directory entropy/emacs-coworker-archive-host-root t))
    (unless (file-exists-p tmp-download-host)
      (make-directory tmp-download-host t))
    ;; begin downloading
    (entropy/emacs-coworker--coworker-message-do-task server-name-string)
    (if (and (file-exists-p server-extdir-or-dest)
             (if (eq server-archive-type 'identity) t
               ;; unless the old extract dir is empty so as not existed
               ;; since we can reuse it for the new extraction.
               (entropy/emacs-list-dir-lite server-extdir-or-dest)))
        (progn (entropy/emacs-coworker--coworker-message-existed server-name-string)
               ;; return
               'exist)
      (message "Downloading lsp archive for '%s' ..." server-name-string)
      (setq download-cbk
            (entropy/emacs-network-download-file
             server-host-url
             tmp-download-file (executable-find "curl")))
      (unless (eq (symbol-value download-cbk) 'success)
        (entropy/emacs-error-without-debugger
         "'%s' lsp server download with fatal with error type '%s'"
         server-name-string
         (get download-cbk 'error-type)))
      (cond ((eq server-archive-type 'identity)
             (rename-file tmp-download-file server-extdir-or-dest))
            (t
             (make-directory server-extdir-or-dest t)
             (entropy/emacs-archive-dowith
              server-archive-type
              tmp-download-file
              (cond ((eq server-archive-type 'gzip)
                     ;; NOTE gzip just support single file
                     ;; com/decom-press, so we recognize the
                     ;; `server-archive-name' as the sub-filename of
                     ;; the extract dir and extract that as-is.
                     (expand-file-name server-archive-name server-extdir-or-dest))
                    (t server-extdir-or-dest))
              :extract)))
      (unless no-success-msg
        (entropy/emacs-coworker--coworker-message-install-success
         server-name-string))
      ;; return
      t)))

;; **** advice them using http proxy

(dolist (install-func  '(entropy/emacs-coworker--coworker-install-by-archive-get
                         entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
                         entropy/emacs-coworker--coworker-isolate-bins-install-by-pip))
  (advice-add install-func
              :around
              #'entropy/emacs-advice-for-common-do-with-http-proxy))

;; ** instances
;; *** language server
;; **** tern
(defun entropy/emacs-coworker-check-tern-server (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "tern-server-install" '("tern") "tern"))

;; **** web
(defun entropy/emacs-coworker-check-web-lsp (&rest _)
  (interactive)
  ;; NOTE: Obsolete (public archived not mantained) vscode web referred lsp servers
  ;; (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
  ;;  "html-lsp-server" '("html-languageserver") "vscode-html-languageserver-bin")
  ;; (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
  ;;  "css-lsp-server" '("css-languageserver") "vscode-css-languageserver-bin")

  ;; use extracted (from vscode) builtin maintained ver. (see
  ;; https://github.com/hrsh7th/vscode-langservers-extracted)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "vscode-web-lsp-server"
   '("vscode-html-language-server"
     "vscode-css-language-server"
     "vscode-json-language-server"
     "vscode-eslint-language-server"
     ;; TODO: not yet published via upstream
     ;; "vscode-markdown-language-server"
     )
   "vscode-langservers-extracted"))

;; **** js
(defun entropy/emacs-coworker-check-js-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "typescript-base"
   '("tsc" "tsserver")
   "typescript")
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "js-lsp-server"
   '("typescript-language-server")
   "typescript-language-server")
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "vue-lsp-server"
   '("vls")
   "vls"))

;; **** php
(defun entropy/emacs-coworker-check-php-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "php-lsp-server" '("intelephense") "intelephense"))

;; **** clangd
(defun entropy/emacs-coworker-check-clangd-lsp (&rest _)
  (interactive)
  (if (executable-find "clangd")
      (entropy/emacs-message-do-message
       "%s %s %s"
       (green "Installed lsp server")
       (yellow "'<clangd>'")
       (green "."))
    (entropy/emacs-error-without-debugger
     "Please using system package management install '<clangd>'.")))

;; **** cmake

(defun entropy/emacs-coworker-check-cmake-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-pip
   "cmake-lsp" '("cmake-language-server") "cmake-language-server"))

;; **** bash
(defun entropy/emacs-coworker-check-bash-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "bash-language-server"
   '("bash-language-server")
   "bash-language-server"))

;; **** json
(defun entropy/emacs-coworker-check-json-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "json-lsp"
   '("vscode-json-languageserver")
   "vscode-json-languageserver"))

;; **** xml

(defun entropy/emacs-coworker-check-xml-lsp (&rest _)
  (entropy/emacs-coworker--coworker-install-by-archive-get
   "xml lsp"
   "org.eclipse.lemminx-0.13.1-uber.jar"
   "https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.13.1/org.eclipse.lemminx-0.13.1-uber.jar"
   'identity))
(setq lsp-xml-jar-file
      (expand-file-name "org.eclipse.lemminx-0.13.1-uber.jar"
                        entropy/emacs-coworker-archive-host-root))

;; **** powershell
(defun entropy/emacs-coworker-check-pwsh-lsp (&rest _)
  (entropy/emacs-coworker--coworker-install-by-archive-get
   "pwsh lsp"
   "pwsh-lsp"
   "https://github.com/PowerShell/PowerShellEditorServices/releases/latest/download/PowerShellEditorServices.zip"
   'zip))

(defvar lsp-pwsh-log-path)
(with-eval-after-load 'lsp-pwsh         ;do not using
                                        ;`entropy/emacs-lazy-load-simple',
                                        ;thats will force load 'lsp' while pdumper procedure
  (unless (file-exists-p lsp-pwsh-log-path)
    ;; ensure that the log path exist or will make pwsh-ls start fail
    (mkdir lsp-pwsh-log-path 'create-parent)))
(unless entropy/emacs-ext-use-eemacs-lsparc
  (setq lsp-pwsh-dir
        (expand-file-name
         "pwsh-lsp/PowerShellEditorServices"
         entropy/emacs-coworker-archive-host-root)))

;; **** rust-analyzer
(defun entropy/emacs-coworker-check-rust-analyzer (&rest _)
  (let (status)
    (entropy/emacs-setf-by-body status
      (entropy/emacs-coworker--coworker-install-by-archive-get
       "rust-analyzer"
       "rust-analyzer"
       (let* ((arch (cond ((string-match-p "^x86_64" system-configuration)
                           "x86_64")
                          (t
                           (user-error "unknow system architecture `%s'"
                                       system-configuration))))
              (sys (cond (sys/linuxp "unknown-linux-gnu")
                         (sys/win32p "pc-windows-msvc")
                         (t
                          (user-error "unkown system type `%s'" system-type))))
              (url (format "https://github.com/rust-lang/rust-analyzer/releases/\
latest/download/rust-analyzer-%s-%s.gz"
                           arch sys)))
         url)
       'gzip 'no-success-msg))
    (unless (eq status 'exist)
      (let ((f
             (expand-file-name "rust-analyzer/rust-analyzer"
                               entropy/emacs-coworker-archive-host-root))
            (lf (expand-file-name "rust-analyzer" entropy/emacs-coworker-bin-host-path)))
        (condition-case err
            (chmod f
                   (file-modes-symbolic-to-number
                    "u+x" (file-modes f)))
          (error
           (entropy/emacs-error-without-debugger
            "chmod `%s' as X with fatal" f)))
        (condition-case err
            (make-symbolic-link f lf 'of-if-exists)
          (error
           (entropy/emacs-error-without-debugger
            "make symbolic for `%s' to `%s' with fatal" f lf)))
        (entropy/emacs-coworker--coworker-message-install-success
         "rust-analyzer")))))

;; **** python
;; ***** python language server types
;; ****** pyls
(defun entropy/emacs-coworker-check-pyls-lsp (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-pip
   "pyls-lsp" '("pyls") "python-language-server"))

;; ****** pyls-ms
(defvar entropy/emacs-coworker--pyls-ms-archive-dir
  (expand-file-name
   "python-language-server"
   entropy/emacs-coworker-archive-host-root))

(defvar entropy/emacs-coworker--pyls-ms-release-name
  (format "%s-x64"
          (cond (sys/macp  "osx")
                (sys/linuxp "linux")
                (sys/is-win-group "win")
                (t (entropy/emacs-error-without-debugger
                    "Unsupported system: %s" system-type)))))

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
            :default-directory ,(entropy/emacs-return-as-default-directory
                                 entropy/emacs-coworker-archive-host-root)
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
            ,(entropy/emacs-return-as-default-directory
              (expand-file-name "src/LanguageServer/Impl/" pyls-ms-archive))
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
       (format "output/bin/Release/%s/publish/" entropy/emacs-coworker--pyls-ms-release-name)
       entropy/emacs-coworker--pyls-ms-archive-dir))


;; ****** pyright
(defun entropy/emacs-coworker-check-pyright-lsp ()
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "pyright-lsp" '("pyright" "pyright-langserver") "pyright"))

;; ***** main

(defun entropy/emacs-coworker-check-python-lsp (&rest _)
  (cl-case entropy/emacs-codeserver-prefer-pyls-type
    (mspyls (entropy/emacs-coworker-check-pyls-ms-lsp))
    (pyls (entropy/emacs-coworker-check-pyls-lsp))
    (pyright (entropy/emacs-coworker-check-pyright-lsp))))


;; **** java
(defun entropy/emacs-coworker-check-java-lsp (&rest _)
  (entropy/emacs-coworker--coworker-install-by-archive-get
   "java lsp"
   "jdtls"
   ;; we use 'entropy-emacs-cabinet' project's hosted jdtls which has
   ;; full project with boot/test/compiler tools integrated instead of
   ;; `lsp-java' github release stuff url which has significantly
   ;; download speed restriction.
   "https://sourceforge.net/projects/entropy-emacs-cabinet/files/LSP/lsp-java/lsp-java-v3.1_jdtls_release/jdt-language-server-1.12.0-202206011637.tar.gz"
   'tgz))

;; *** exra tools
;; **** wsl-open
(defun entropy/emacs-coworker-check-wsl-open (&rest _)
  (interactive)
  (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
   "wsl-open"
   '("wsl-open")
   "wsl-open"))

;; * provide

(defun entropy/emacs-coworker-install-all-coworkers (&optional prefix)
  "Install all eemacs coworkers."
  (interactive "P")
  (let ((count 1)
        (newhost (when prefix
                   (read-directory-name "Coworker Host: " nil nil t)))
        (task-list '((:name "tern"           :pred entropy/emacs-coworker-check-tern-server :enable (EEMACS-DT-IDENTITY t))
                     (:name "web-lsp"        :pred entropy/emacs-coworker-check-web-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "js-lsp"         :pred entropy/emacs-coworker-check-js-lsp      :enable (EEMACS-DT-IDENTITY t))
                     (:name "json-lsp"       :pred entropy/emacs-coworker-check-json-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "xml-lsp"        :pred entropy/emacs-coworker-check-xml-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "php-lsp"        :pred entropy/emacs-coworker-check-php-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "bash-lsp"       :pred entropy/emacs-coworker-check-bash-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "python-lsp"     :pred entropy/emacs-coworker-check-python-lsp  :enable (EEMACS-DT-IDENTITY t))
                     (:name "python-pyright" :pred entropy/emacs-coworker-check-pyright-lsp :enable (EEMACS-DT-IDENTITY t))
                     (:name "cmake-lsp"      :pred entropy/emacs-coworker-check-cmake-lsp   :enable (EEMACS-DT-IDENTITY t))
                     (:name "java-lsp"       :pred entropy/emacs-coworker-check-java-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "clangd-lsp"     :pred entropy/emacs-coworker-check-clangd-lsp  :enable (EEMACS-DT-IDENTITY t))
                     (:name "rust-analyzer"  :pred entropy/emacs-coworker-check-rust-analyzer :enable (EEMACS-DT-IDENTITY t))
                     (:name "wsl-open"       :pred entropy/emacs-coworker-check-wsl-open    :enable (EEMACS-DT-FORM sys/wsl2-env-p))
                     (:name "pwsh-lsp"       :pred entropy/emacs-coworker-check-pwsh-lsp    :enable (EEMACS-DT-FORM (and (executable-find "pwsh")
                                                                                                                         (executable-find "dotnet"))))
                     (:name "python-lsp-ms"  :pred entropy/emacs-coworker-check-pyls-ms-lsp :enable (EEMACS-DT-FORM
                                                                                                     (executable-find "dotnet")))
                     ))
        (entropy/emacs-message-non-popup nil))
    (entropy/emacs-with-coworker-host
      newhost
      (dolist (el task-list)
        (when (entropy/emacs-type-spec-eval (plist-get el :enable))
          (entropy/emacs-message-do-message
           "%s %s %s"
           (cyan (format ">> %s: " count))
           (yellow (format "'%s'" (plist-get el :name)))
           (green "installing ..."))
          (funcall (plist-get el :pred))
          (cl-incf count))))))

(provide 'entropy-emacs-coworker)
