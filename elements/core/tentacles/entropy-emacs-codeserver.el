;;; entropy-emacs-codeserver.el --- entropy-emacs code server configuration
;;
;; * Copyright (C) 20191014  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/core/tentacles/entropy-emacs-codeserver.el
;; Compatibility: GNU Emacs emacs-25;
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
;; The language server integrating configuration
;; 
;; * Configuration:
;; 
;; Designed for entropy-emacs only.
;; 
;; * Code:

(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-message)

;; ** library
;; *** server install procedure
(defun entropy/emacs-codeserver--server-install-warn (task-name proc-buffer)
  (with-current-buffer proc-buffer
    (goto-char (point-min))
    (if (re-search-forward (rx (or (regexp "[^a-z]Error") "ERR!" "No matching"))
                           nil t)
        (let ((debug-on-error nil))
          (error
           (format 
            "%s %s %s %s %s"
            "Fatal do with lsp-install task for"
            (format "<%s>" task-name)
            ", check callback buffer"
            (format "'%s'" (buffer-name (get-buffer proc-buffer)))
            "for more details")))
      (entropy/emacs-message-do-message
       "%s %s %s"
       (green "Install lsp-task")
       (yellow (format "<%s>" task-name))
       (green "successfully")))))

(defmacro entropy/emacs-codeserver--server-search
    (task-name task-buffer server-cons install-cmd
               before-install after-install proc-workdir
               &rest install-args)
  `(let ((server-type (car ',server-cons))
         (server-bin (eval (cdr ',server-cons)))
         (task-buffer (get-buffer-create ,task-buffer)))
     (with-current-buffer task-buffer
       (when buffer-read-only
         (read-only-mode 0))
       (goto-char (point-min))
       (erase-buffer))
     (funcall ,before-install)
     (if
         (cond ((eq 'file server-type)
                (not (file-exists-p server-bin)))
               ((eq 'exec server-type)
                (not (executable-find server-bin))))
         (let ((default-directory ,proc-workdir))
           (entropy/emacs-message-do-message
            "%s %s %s"
            (green "Do lsp server install task")
            (yellow (format "<%s>" ,task-name))
            (green "..."))
           (sleep-for 2)
           (call-process ,install-cmd nil ,task-buffer t ,@install-args)
           (entropy/emacs-codeserver--server-install-warn
            ,task-name task-buffer)
           (funcall ,after-install))
       (entropy/emacs-message-do-message
        "%s %s %s"
        (green "lsp server task")
        (yellow (format "<%s>" ,task-name))
        (green "has been installed")))))

(defmacro entropy/emacs-codeserver--server-install-by-npm
    (server-name-string server-bin-string server-repo-string)
  `(entropy/emacs-codeserver--server-search
    ,server-name-string
    ,(concat "*eemacs " server-name-string " install*")
    (file . ,(expand-file-name
              (format ".local/lib/node_modules/.bin/%s" server-bin-string)
              "~"))
    "npm"
    (lambda ()
      (mkdir (expand-file-name ".local/bin" "~") t)
      (mkdir (expand-file-name ".local/lib/node_modules/" "~") t)
      (let ((lock-package-file (expand-file-name ".local/lib/package-lock.json" "~")))
        (when (file-exists-p lock-package-file)
          (delete-file lock-package-file t))))
    (lambda ()
      (make-symbolic-link (expand-file-name
                           ,(format ".local/lib/node_modules/.bin/%s" server-bin-string)
                           "~")
                          (expand-file-name
                           ,(format ".local/bin/%s" server-bin-string)
                           "~")
                          t)
      (let ((lock-package-file (expand-file-name ".local/lib/package-lock.json" "~")))
        (when (file-exists-p lock-package-file)
          (delete-file lock-package-file t))))
    (expand-file-name ".local/lib/" "~")
    "install" ,server-repo-string))

(defmacro entropy/emacs-codeserver--server-install-by-pip
    (server-name-string server-bin-string server-repo-string)
  `(entropy/emacs-codeserver--server-search
    ,server-name-string
    ,(format "*eemacs %s install <pip>*" server-name-string)
    (file
     .
     ,(expand-file-name (format ".local/bin/%s" server-bin-string) "~"))
    "pip"
    (lambda () nil)
    (lambda () nil)
    default-directory
    "install" ,server-repo-string "--user"))


;; ** common server
;; *** individual servers
;; **** irony server
(defun entropy/emacs-codeserver-irony-load-subs ()
  (dolist (el '(irony-cdb-clang-complete
                irony-cdb-json
                irony-cdb-libclang
                irony-cdb
                irony-completion
                irony-diagnostics
                irony-iotask
                irony-snippet))
    (require el)))

(defun entropy/emacs-codeserver-irony-pipe-config ()
  "Reducing pipe-read-delay and set the pipe buffer size to
64K on Windows (from the original 4K).

It is the recommendation of irony-mode official introduction."
  (when (boundp 'w32-pipe-read-delay)
    (setq-local w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq-local irony-server-w32-pipe-buffer-size (* 64 1024))))

(defun entropy/emacs-codeserver-irony-refer-advice-around (oldfuc &rest args)
  "Prevent c group mode as `php-mode' which was the derived from
`c-mode' to load `irony-mode' and `company-irony'."
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (funcall oldfuc)
    t))

(defun entropy/emacs-codeserver-usepackage-irony ()
  "Function for enabling irony mode for c and c++ mode."
  (use-package irony
    :if (not entropy/emacs-company-lsp)
    :commands (irony-mode)
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options)
           (c-mode . entropy/emacs-codeserver-irony-pipe-config)
           (c++-mode . entropy/emacs-codeserver-irony-pipe-config))
    :config
    (entropy/emacs-codeserver-irony-load-subs)
    (advice-add 'irony-mode :around #'entropy/emacs-codeserver-irony-refer-advice-around))

  (use-package irony-eldoc
    :defines irony-mode-hook
    :commands (irony-eldoc)
    :hook (irony-mode . irony-eldoc)))

;; **** tern server
(defun entropy/emacs-codeserver-usepackage-tern ()
  (use-package tern
    :commands (tern-mode)
    :defines js2-mode-hook
    :hook (js2-mode . tern-mode)))

;; **** anaconda server
(defun entropy/emacs-codeserver-usepackage-anaconda ()
  (use-package anaconda-mode
    :diminish anaconda-mode
    :commands anaconda-mode
    :defines python-mode-hook
    :init
    (add-hook 'python-mode-hook #'anaconda-mode)))

;; *** startup
(when entropy/emacs-company-lsp
  (cond
   ((and sys/win32p
         entropy/emacs-win-portable-mingw-enable
         (file-exists-p (concat entropy/emacs-win-portable-mingw-path "libclang.dll")))
    (entropy/emacs-codeserver-usepackage-irony))
   (sys/is-posix-compatible
    (entropy/emacs-codeserver-usepackage-irony)))
  (entropy/emacs-codeserver-usepackage-tern)
  (entropy/emacs-codeserver-usepackage-anaconda))

;; ** microsoft language server
;; *** lsp-client
(use-package lsp-mode
  :if (and (>= emacs-major-version 25)
           entropy/emacs-company-lsp)
  :diminish lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :if (and (>= emacs-major-version 25)
           entropy/emacs-company-lsp)
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)))

;; *** lsp instances
;; **** lsp html&css
(when entropy/emacs-company-lsp
  (defun entropy/emacs-codeserver-check-web-lsp (&rest _)
    (interactive)
    (entropy/emacs-codeserver--server-install-by-npm
     "html-lsp-server" "html-languageserver" "vscode-html-languageserver-bin")
    (entropy/emacs-codeserver--server-install-by-npm
     "css-lsp-server" "css-languageserver" "vscode-css-languageserver-bin"))
  
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'web-mode
      (advice-add 'web-mode
                  :before
                  #'entropy/emacs-codeserver-check-web-lsp))
    (entropy/emacs-lazy-load-simple 'css-mode
      (advice-add 'css-mode
                  :before
                  #'entropy/emacs-codeserver-check-web-lsp))))

;; **** lsp javascript
(when entropy/emacs-company-lsp
  (defun entropy/emacs-codeserver-check-js-lsp (&rest _)
    (interactive)
    (entropy/emacs-codeserver--server-install-by-npm
     "js-lsp-server" "typescript-language-server" "typescript-language-server"))

  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'js2-mode
      (advice-add 'js2-mode :before #'entropy/emacs-codeserver-check-js-lsp))))

;; **** lsp php
(when entropy/emacs-company-lsp
  (defun entropy/emacs-codeserver-check-php-lsp (&rest _)
    (interactive)
    (entropy/emacs-codeserver--server-install-by-npm
     "php-lsp-server" "intelephense" "intelephense"))
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'php-mode
      (advice-add 'php-mode :before #'entropy/emacs-codeserver-check-php-lsp))))

;; **** lsp clangd
(when entropy/emacs-company-lsp
  (defun entropy/emacs-codeserver-check-clangd-lsp ()
    (interactive)
    (if (executable-find "clangd")
        (entropy/emacs-message-do-message
         "%s %s %s"
         (green "Installed lsp server")
         (yellow "'<clangd>'")
         (green "."))
      (error "Please using system package management install '<clangd>'.")))
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'cc-mode
      (advice-add 'c-mode
                  :before
                  #'entropy/emacs-codeserver-check-clangd-lsp)
      (advice-add 'c++-mode
                  :before
                  #'entropy/emacs-codeserver-check-clangd-lsp))))

;; **** lsp python
(when entropy/emacs-company-lsp
  (defun entropy/emacs-codeserver-check-python-lsp (&rest _)
    (interactive)
    (entropy/emacs-codeserver--server-install-by-pip
     "pyls-lsp" "pyls" "python-language-server"))
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'python
      (advice-add 'python-mode
                  :before
                  #'entropy/emacs-codeserver-check-python-lsp))))

;; * provide
(provide 'entropy-emacs-codeserver)
