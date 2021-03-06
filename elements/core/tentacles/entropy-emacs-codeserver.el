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
(require 'entropy-emacs-coworker)

;; ** library
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
    :if (eq entropy/emacs-use-ide-type 'traditional)
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

;; **** ac-php
(defun entropy/emacs-codeserver-usepackage-ac-php ()
  (use-package ac-php
    :requires php-mode
    :defines php-mode-hook
    :commands ac-php-core-eldoc-setup
    :init
    (add-hook 'php-mode-hook '(lambda () (ac-php-core-eldoc-setup)))
    (add-hook 'php-mode-hook
              '(lambda () 
                 (setq-local indent-tabs-mode nil)
                 (setq-local c-basic-offset 4)
                 (subword-mode 1)))))

;; *** startup
(when (eq entropy/emacs-use-ide-type 'traditional)
  (cond
   ((and sys/win32p
         entropy/emacs-win-portable-mingw-enable
         (file-exists-p (concat entropy/emacs-win-portable-mingw-path "libclang.dll")))
    (entropy/emacs-codeserver-usepackage-irony))
   (sys/is-posix-compatible
    (entropy/emacs-codeserver-usepackage-irony)))
  
  (entropy/emacs-codeserver-usepackage-tern)
  (entropy/emacs-codeserver-usepackage-anaconda)
  (entropy/emacs-codeserver-usepackage-ac-php)
  
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'js2-mode
      (advice-add 'js2-mode
                  :before
                  #'entropy/emacs-coworker-check-tern-server))))

;; ** microsoft language server
;; *** lsp-client
(use-package lsp-mode
  :if (and (>= emacs-major-version 25)
           (eq entropy/emacs-use-ide-type 'lsp))
  :diminish lsp-mode
  :commands (lsp lsp-mode)
  :hook (prog-mode . lsp-deferred)
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)

  (entropy/emacs-lazy-initial-advice-before
   '(lsp)
   "lsp-enable-yas"
   "lsp-enable-yas"
   (require 'yasnippet)
   (yas-global-mode)))

(use-package lsp-ui
  :if (and (>= emacs-major-version 25)
           (eq entropy/emacs-use-ide-type 'lsp))
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)))

;; *** lsp instances
;; **** lsp html&css
(when (eq entropy/emacs-use-ide-type 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'web-mode
      (advice-add 'web-mode
                  :before
                  #'entropy/emacs-coworker-check-web-lsp))
    (entropy/emacs-lazy-load-simple 'css-mode
      (advice-add 'css-mode
                  :before
                  #'entropy/emacs-coworker-check-web-lsp))))

;; **** lsp javascript
(when (eq entropy/emacs-use-ide-type 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'js2-mode
      (advice-add 'js2-mode :before #'entropy/emacs-coworker-check-js-lsp))))

;; **** lsp php
(when (eq entropy/emacs-use-ide-type 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'php-mode
      (advice-add 'php-mode :before #'entropy/emacs-coworker-check-php-lsp))))

;; **** lsp clangd
(when (eq entropy/emacs-use-ide-type 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'cc-mode
      (advice-add 'c-mode
                  :before
                  #'entropy/emacs-coworker-check-clangd-lsp)
      (advice-add 'c++-mode
                  :before
                  #'entropy/emacs-coworker-check-clangd-lsp))))

;; **** lsp python
(when (eq entropy/emacs-use-ide-type 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple 'python
      (advice-add 'python-mode
                  :before
                  #'entropy/emacs-coworker-check-python-lsp))))

;; * provide
(provide 'entropy-emacs-codeserver)
