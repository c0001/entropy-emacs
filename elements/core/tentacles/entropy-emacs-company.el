;;; entropy-emacs-company.el --- entropy emacs completion config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-company.el
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
;; Completion referrence config for `entropy-emacs'.
;;
;; `entropy-emacs' use [[http://company-mode.github.io][company-mode]] as the completion framework as
;; the completion main tool. It's the framework who provide the
;; APIS to built arbitrary completion backends for various emacs
;; major modes even for the mode independent way.
;;
;; There's two completion server type choice for `entropy-emacs':
;;
;; 1) Traditional way:
;;   
;;    The way that each backends basic on the server tool-chain are
;;    independently using its own designation, such as pyton
;;    `anaconda-mode', C `irony-mode', javascript `tern-mode'.
;;
;;    Advantage for this type is that each backend maintained
;;    individually and designed just for the single sake. this can
;;    limitting code built scope and reducing bug fixing difficulty
;;    level.
;;
;;    The weakness was that non-standard server-client communication
;;    api, which will impede the further features development who
;;    stand on the top level of all or some of them.
;;
;; 2) LSP Mode:
;;
;;    LSP (language server protocol) was brought up by Microsoft, for
;;    solving the problem caused from way '1)', it was under
;;    development. emacs melpa package 'lsp-mode' and 'elgot' was the
;;    client for thus, but be under development and with sets of
;;    bugs.
;;
;; `entropy-emacs' defaultly enable the traditional way for the sake
;; of stability.
;;
;; 
;; * Configuration:
;; 
;; configurationLoaidng by `entropy-emacs' automatically.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-message)

;; ** defvar


;; ** libraries
;; *** yas load
(defun entropy/emacs-company-use-yasnippet (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun entropy/emacs-company-start-with-yas (&rest _)
  (when (not (condition-case error
                 (symbol-value yas-global-mode)
               (error nil)))
    (when (fboundp 'yas-global-mode)
      (yas-global-mode))))

;; *** company components load
(defun entropy/emacs-company-require-subs ()
  (entropy/emacs-lazy-load-simple 'company
    (dolist (el '(company-abbrev
                  company-bbdb
                  company-capf
                  company-clang
                  company-cmake
                  company-css
                  company-dabbrev-code
                  company-dabbrev
                  company-eclim
                  company-elisp
                  company-etags
                  company-files
                  company-gtags
                  company-ispell
                  company-keywords
                  company-nxml
                  company-oddmuse
                  company-semantic
                  company-template
                  company-tempo
                  company-tng
                  company-xcode
                  company-yasnippet))
      (require el))))

;; *** server install procedure
(defun entropy/emacs-company--server-install-warn (task-name proc-buffer)
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

(defmacro entropy/emacs-company--server-search
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
           (entropy/emacs-company--server-install-warn
            ,task-name task-buffer)
           (funcall ,after-install))
       (entropy/emacs-message-do-message
        "%s %s %s"
        (green "lsp server task")
        (yellow (format "<%s>" ,task-name))
        (green "has been installed")))))

(defmacro entropy/emacs-company--server-install-by-npm
    (server-name-string server-bin-string server-repo-string)
  `(entropy/emacs-company--server-search
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

(defmacro entropy/emacs-company--server-install-by-pip
    (server-name-string server-bin-string server-repo-string)
  `(entropy/emacs-company--server-search
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

;; ** company core
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode)
;; *** bind-key  
  :bind (("M-/" . company-complete)
         ("M-\\" . company-dabbrev)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ;; ("<tab>" . company-complete-selection)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
;; *** init for load  
  :init
  (entropy/emacs-lazy-with-load-trail global-company-mode (global-company-mode t))
  (when (or (equal entropy/emacs-use-extensions-type 'submodules)
            entropy/emacs-fall-love-with-pdumper)
    (entropy/emacs-company-require-subs))
  (advice-add 'company-complete :before 'entropy/emacs-company-start-with-yas)
  
;; *** config for after-load
  :config
;; **** basic setting 
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq
   company-tooltip-limit 20  ; bigger popup window
   company-echo-delay 0      ; remove annoying blinking
   company-dabbrev-code-everywhere t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-dabbrev-char-regexp "\\sw[-_]*")

  (if entropy/emacs-company-posframe-mode
      (setq company-tooltip-offset-display 'scrollbar)
    (setq company-tooltip-offset-display 'lines))

;; **** Support yas in commpany
  (setq company-backends (mapcar #'entropy/emacs-company-use-yasnippet company-backends))

;; **** Using company-posframe to speedup company candidates window show and scrolling
  (when (and (not (version< emacs-version "26.1"))
             entropy/emacs-company-posframe-mode)
    (use-package company-posframe
      :after company
      :commands (company-posframe-mode)
      :diminish company-posframe-mode
      :init (company-posframe-mode 1))))


;; *** Popup documentation for completion candidates
(use-package company-quickhelp
  :if (and (not entropy/emacs-company-posframe-mode)
           (display-graphic-p))
  :after company
  :defines company-quickhelp-delay
  :commands (company-quickhelp-mode
             company-quickhelp-manual-begin)
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin)
              ("C-h" . nil)
              ("<f1>" . nil))
  :init
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode 1))

;; ** microsoft language server
;; *** lsp-mode
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

;; *** company-lsp
(use-package company-lsp
  :if (and (>= emacs-major-version 25)
           entropy/emacs-company-lsp)
  :init
  (add-hook 'prog-mode-hook #'entropy/emacs-company-add-lsp-backend)
  (defun entropy/emacs-company-add-lsp-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-lsp) company-backends)))

;; ** Backends
;; *** miscelloneous
;; **** englishs dict quick completion
(use-package company-en-words
  :after company
  :ensure nil
  :commands company-en-words
  :bind ("M-]" . company-en-words))

;; *** shell
(use-package company-shell
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines (sh-mode-hook)
  :commands (company-shell company-shell-env company-fish-shell)
  :init
  (add-hook 'sh-mode-hook #'entropy/emacs-company--add-shell-backend)
  (defun entropy/emacs-company--add-shell-backend ()
    (make-local-variable 'company-backends)
    (dolist (el '(company-shell company-shell-env company-fish-shell))
      (cl-pushnew (entropy/emacs-company-use-yasnippet el) company-backends))))

;; *** web refer
;; **** web/html&css
;; ***** lsp
(when entropy/emacs-company-lsp
  (defun entropy/emacs-company-check-web-lsp (&rest _)
    (interactive)
    (entropy/emacs-company--server-install-by-npm
     "html-lsp-server" "html-languageserver" "vscode-html-languageserver-bin")
    (entropy/emacs-company--server-install-by-npm
     "css-lsp-server" "css-languageserver" "vscode-css-languageserver-bin"))
  
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'web-mode
      (advice-add 'web-mode
                  :before
                  #'entropy/emacs-company-check-web-lsp))
    (entropy/emacs-lazy-load-simple 'css-mode
      (advice-add 'css-mode
                  :before
                  #'entropy/emacs-company-check-web-lsp))))

;; ***** traditional
(use-package company-web
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines (company-web-html
            company-web-jade
            company-web-slim
            web-mode-hook
            css-mode-hook)
  :commands company-web
  :init
  (add-hook 'web-mode-hook #'entropy/emacs-company-web-add-all-backends)
  (add-hook 'css-mode-hook #'entropy/emacs-company-web-add-css-backend)
  
  (defun entropy/emacs-company-web-add-html-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-html) company-backends))
  (defun entropy/emacs-company-web-add-jade-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-jade) company-backends))
  (defun entropy/emacs-company-web-add-slim-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-slim) company-backends))
  (defun entropy/emacs-company-web-add-css-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-css) company-backends))
  (defun entropy/emacs-company-web-add-all-backends ()
    (entropy/emacs-company-web-add-html-backend)
    (entropy/emacs-company-web-add-jade-backend)
    (entropy/emacs-company-web-add-slim-backend)
    (entropy/emacs-company-web-add-css-backend)))

;; **** javascript
;; ***** lsp
(when entropy/emacs-company-lsp
  (defun entropy/emacs-company-check-js-lsp (&rest _)
    (interactive)
    (entropy/emacs-company--server-install-by-npm
     "js-lsp-server" "typescript-language-server" "typescript-language-server"))

  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'js2-mode
      (advice-add 'js2-mode :before #'entropy/emacs-company-check-js-lsp))))

;; ***** traditional
(use-package tern
  :if (not entropy/emacs-company-lsp)
  :commands (tern-mode)
  :defines js2-mode-hook
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines js2-mode-hook
  :commands company-tern
  :init
  (add-hook 'js2-mode-hook #'entropy/emacs-company-tern-add-tern-backend)
  (defun entropy/emacs-company-tern-add-tern-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-tern)
                company-backends))

  (defun entropy/emacs-company-check-tern-server (&rest _)
    (interactive)
    (entropy/emacs-company--server-install-by-npm
     "tern-server-install" "tern" "tern"))

  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'js2-mode
      (advice-add 'js2-mode :before #'entropy/emacs-company-check-tern-server)))

  :config
  (defun entropy/emacs-company-create-tern-project-file (&rest _)
    "Auto create '.tern-project' file in current dir.

Notice: this automatically created file was simple, you should
modify it by personal customization.

And this automatically created file was the file within
entropy-emacs."
    (let ((tern-template (expand-file-name ".tern-project" entropy/emacs-templates-dir)))
      (when (buffer-file-name)
        (unless (file-exists-p (expand-file-name ".tern-project" default-directory))
          (if (file-exists-p tern-template)
              (progn
                (copy-file tern-template
                           (expand-file-name ".tern-project" default-directory))
                (message "Succeed to create .tern-project in this folder!"))
            (message "Can not find origin .tern-project file from %s"
                     (file-name-directory tern-template)))))))

  (advice-add 'company-tern :before #'entropy/emacs-company-create-tern-project-file))

;; **** php
;; ***** lsp
(when entropy/emacs-company-lsp
  (defun entropy/emacs-company-check-php-lsp (&rest _)
    (interactive)
    (entropy/emacs-company--server-install-by-npm
     "php-lsp-server" "intelephense" "intelephense"))
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'php-mode
      (advice-add 'php-mode :before #'entropy/emacs-company-check-php-lsp))))

;; ***** traditional
(use-package company-php
  :if (not entropy/emacs-company-lsp)
  :defines php-mode-hook
  :commands company-ac-php-backend
  :init
  (defun entropy/emacs-company-ac-php-add-acphp-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-ac-php-backend) company-backends))
  (add-hook 'php-mode-hook #'entropy/emacs-company-ac-php-add-acphp-backend))

;; *** C(PP) Java python
;; **** C(PP)
;; ***** lsp
(when entropy/emacs-company-lsp
  (defun entropy/emacs-company-check-clangd-lsp ()
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
                  #'entropy/emacs-company-check-clangd-lsp)
      (advice-add 'c++-mode
                  :before
                  #'entropy/emacs-company-check-clangd-lsp))))

;; ***** traditional
;; ****** headers
(use-package company-c-headers
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines (c-mode-hook c++-mode-hook)
  :commands company-c-headers
  :init
  (add-hook 'c-mode-hook 'entropy/emacs-company-c-headers-add-cheader-backend)
  (defun entropy/emacs-company-c-headers-add-cheader-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-c-headers) company-backends)))

;; ****** irony mode
(defun entropy/emacs-c-irony-load-subs ()
  (dolist (el '(irony-cdb-clang-complete
                irony-cdb-json
                irony-cdb-libclang
                irony-cdb
                irony-completion
                irony-diagnostics
                irony-iotask
                irony-snippet))
    (require el)))

(defun entropy/emacs-c-irony-pipe-config ()
  "Reducing pipe-read-delay and set the pipe buffer size to
64K on Windows (from the original 4K).

It is the recommendation of irony-mode official introduction."
  (when (boundp 'w32-pipe-read-delay)
    (setq-local w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq-local irony-server-w32-pipe-buffer-size (* 64 1024))))

(defun entropy/emacs-c-irony-refer-advice-around (oldfuc &rest args)
  "Prevent c group mode as `php-mode' which was the derived from
`c-mode' to load `irony-mode' and `company-irony'."
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (funcall oldfuc)
    t))

(defun entropy/emacs-c-usepackage-irony ()
  "Function for enabling irony mode for c and c++ mode."
  (use-package irony
    :if (not entropy/emacs-company-lsp)
    :commands (irony-mode)
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options)
           (c-mode . entropy/emacs-c-irony-pipe-config)
           (c++-mode . entropy/emacs-c-irony-pipe-config))
    :config
    (entropy/emacs-c-irony-load-subs)
    (advice-add 'irony-mode :around #'entropy/emacs-c-irony-refer-advice-around))

  (use-package irony-eldoc
    :defines irony-mode-hook
    :commands (irony-eldoc)
    :hook (irony-mode . irony-eldoc)))

(cond
 ((and sys/win32p
       entropy/emacs-win-portable-mingw-enable
       (file-exists-p (concat entropy/emacs-win-portable-mingw-path "libclang.dll")))
  (entropy/emacs-c-usepackage-irony))
 (sys/is-posix-compatible
  (entropy/emacs-c-usepackage-irony)))

(use-package company-irony
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines (c-mode-hook c++-mode-hook)
  :commands commpany-irony
  :init
  (add-hook 'c-mode-hook 'entropy/emacs-company-irony-add-irony-backend)
  (add-hook 'c++-mode-hook 'entropy/emacs-company-irony-add-irony-backend)
  (defun entropy/emacs-company-irony-add-irony-backend ()
    "Make local company-backends with yasnippet for irony in c
and c++ mode."
    (make-local-variable 'company-backends)
    (when  (or (eq major-mode 'c-mode)
               (eq major-mode 'c++-mode))
      (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-irony) company-backends))))

;; **** Java
;; ***** lsp
;; ***** traditional
;; **** Python
;; ***** lsp

(when entropy/emacs-company-lsp
  (defun entropy/emacs-company-check-python-lsp (&rest _)
    (interactive)
    (entropy/emacs-company--server-install-by-pip
     "pyls-lsp" "pyls" "python-language-server"))
  (when entropy/emacs-company-install-server-immediately
    (entropy/emacs-lazy-load-simple 'python
      (advice-add 'python-mode :before #'entropy/emacs-company-check-python-lsp))))

;; ***** traditional
(use-package company-anaconda
  :if (not entropy/emacs-company-lsp)
  :after company
  :defines anaconda-mode-hook
  :commands company-anaconda
  :init
  (add-hook 'anaconda-mode-hook #'entropy/emacs-company-anaconda-add-anaconda-backend)
  (defun entropy/emacs-company-anaconda-add-anaconda-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-anaconda) company-backends)))

;; * provide
(provide 'entropy-emacs-company)
