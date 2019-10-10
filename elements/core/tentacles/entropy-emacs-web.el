;;; entropy-emacs-web.el --- entropy-emacs web development configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-web.el
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
;; `entropy-emacs' web aspect development environment intergratioin
;; for both of front-end and backend, as that covering for =html=,
;; =css=, =javascript=, =php=.
;; 
;; * Configuration:
;; 
;; Using for `entropy-emacs' only.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))

;; ** Preparation
(entropy/emacs-lazy-load-simple 'tern
  (setq tern-command '("tern")))

;; ** main libraries
(defun entropy/emacs-web-browse-web-buffer ()
  (interactive)
  (require 'entropy-common-library-const)
  (let* (url)
    (if (and buffer-file-name
             (file-exists-p buffer-file-name))
        (progn
          (setq url
                (url-hexify-string
                 (concat "file://" buffer-file-name)
                 entropy/cl-url--allowed-chars))
          (browse-url url)))))

(defun entropy/emacs-web--web-mode-start-hook ()
  ;; Set indent and tab-width
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (entropy/emacs-lazy-load-simple 'yasnippet
    (yas-activate-extra-mode 'php-mode)
    (yas-activate-extra-mode 'js2-mode)
    (yas-activate-extra-mode 'css-mode))
  (web-mode-set-engine "php")
  ;; fake initial value for tern in `web-mode', used for
  ;; `company-tern''s subtroutine.
  (setq-local tern-mode nil))

;; ** web frontend technologies
;; *** html
;; **** web-mode
(use-package web-mode
  :commands web-mode
  :mode
  ("\\.\\(phtml\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
   .
   web-mode)
  :init
  (add-hook 'web-mode-hook
            'entropy/emacs-web--web-mode-start-hook)
  :config
  (when (display-graphic-p)
    (add-hook 'web-mode-hook
              #'(lambda ()
                  (setq-local entropy/emacs-web-development-environment
                              t)))
    (define-key web-mode-map (kbd "<C-f1>")
      'entropy/emacs-web-browse-web-buffer))

  (when (not entropy/emacs-company-lsp)
    (define-key web-mode-map (kbd "M-t") 'company-tern)
    (define-key web-mode-map (kbd "M-p") 'company-ac-php-backend)))

;; **** Emmet-mode for quick edittng
(use-package emmet-mode
  :defines (web-mode-hook html-mode-hook)
  :commands emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (unbind-key "C-c w" emmet-mode-keymap))

;; *** CSS mode
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; *** JSON mode
(use-package json-mode
  :commands (json-mode))

;; *** js-mode
;; Improved JavaScript editing mode
(use-package js2-mode
  :commands (js2-mode)
  :mode "\\.js$"
  :interpreter "node"
  :init
  (entropy/emacs-lazy-load-simple 'js2-mode
    (require 'js2-old-indent)
    (require 'js2-imenu-extras)
    (add-hook 'js2-mode-hook
              #'(lambda ()
                  (setq-local js2-basic-offset 4)
                  (js2-highlight-unused-variables-mode 1)
                  (js2-imenu-extras-mode 1)))))

;; **** tern mode
(use-package tern
  :if (not entropy/emacs-company-lsp)
  :commands (tern-mode)
  :defines js2-mode-hook
  :hook (js2-mode . tern-mode))

;; **** js2-refactor
(use-package js2-refactor
  :requires js2-mode
  :defines js2-mode-hook
  :commands (js2-refactor-mode)
  :diminish js2-refactor-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; *** tools
;; **** Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :if (executable-find "git")
  :commands (skewer-mode skewer-html-mode skewer-css-mode)
  :diminish (skewer-mode skewer-html-mode skewer-css-mode)
  :init
  (entropy/emacs-lazy-load-simple 'skewer-mode
    (dolist (el '(cache-table
                  skewer-bower
                  skewer-css
                  skewer-html
                  skewer-repl
                  skewer-setup))
      (require el)))
  (entropy/emacs-lazy-load-simple 'js2-mode
    (add-hook 'js2-mode-hook #'skewer-mode))
  (entropy/emacs-lazy-load-simple 'css-mode
    (add-hook 'css-mode-hook #'skewer-css-mode))
  (entropy/emacs-lazy-load-simple 'sgml-mode
    (add-hook 'web-mode-hook #'skewer-html-mode)))

(use-package impatient-mode
  :commands (impatient-mode)
  :preface
  (defun entropy/emacs-web-impatient-mode ()
    "Enable `impatient-mode' and http server by `httpd-start' if
server not actived and open the impatient url
\"http://localhost:8080/imp/\" with file-name of current buffer
if current file was html file."
    (interactive)
    (require 'entropy-common-library)
    (require 'impatient-mode)
    (let* ((buffer_n (buffer-name (current-buffer))))
      (cond
       ((and (boundp 'impatient-mode)
             impatient-mode)
        (impatient-mode 0))
       ((and (boundp 'impatient-mode)
             (not impatient-mode))
        (unless (ignore-errors (httpd-running-p))
          (httpd-start))
        (impatient-mode 1)
        (when (string-match-p "\\.html" buffer_n)
          (imp-visit-buffer))))))
  :init (setq impatient-mode-delayed-update nil)
  :config
  (advice-add 'impatient-mode :before 'entropy/cl-sn-buffer-p)
  (defun imp-visit-buffer (&optional arg)
    "Visit the current buffer in a browser.
If given a prefix argument, visit the buffer listing instead.

Notice: this function has been modified to patch with host name
format."
    (interactive "P")
    (unless (process-status "httpd")
      (httpd-start))
    (unless impatient-mode
      (impatient-mode))
    (let ((url (format "http://localhost:%d/imp/" httpd-port)))
      (unless arg
        (setq url (format "%slive/%s/" url (url-hexify-string (buffer-name)))))
      (browse-url url))))

;; **** Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :commands
  (web-beautify-css
   web-beautify-css-buffer
   web-beautify-html
   web-beautify-html-buffer
   web-beautify-js
   web-beautify-js-buffer)
  :init
  (entropy/emacs-lazy-load-simple 'js2-mode
    (bind-key "C-c C-b" 'web-beautify-js js2-mode-map))
  (entropy/emacs-lazy-load-simple 'json-mode
    (bind-key "C-c C-b" 'web-beautify-js json-mode-map))
  (entropy/emacs-lazy-load-simple 'sgml-mode
    (bind-key "C-c C-b" 'web-beautify-html html-mode-map))
  (entropy/emacs-lazy-load-simple 'css-mode
    (bind-key "C-c C-b" 'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

;; ** web backend technologies
;; *** php
(use-package php-mode
  :mode "\\.php$"
  :commands php-mode)
  
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
               (subword-mode 1))))


;; * provide
(provide 'entropy-emacs-web)
