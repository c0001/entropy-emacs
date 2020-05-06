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
(require 'entropy-emacs-coworker)
(require 'entropy-emacs-utils)
(require 'entropy-emacs-hydra-hollow)

;; ** Preparation
(entropy/emacs-lazy-load-simple tern
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
  (progn
    (require 'yasnippet)
    (unless yas-minor-mode
      (yas-minor-mode))
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
  :eemacs-mmphc
  (((:enable t))
   ("Basic"
    (("<f1>" entropy/emacs-web-browse-web-buffer "Preview Current Buffer"
      :enable t
      :exit t))
    "Emmet" ()
    "Navigation" ()
    ))

  :eemacs-mmphca
  (((:enable (eq (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
                 'traditional))
    (web-mode web-mode web-mode-map))
   ("Company"
    (("M-t" company-tern "Company Tern"
      :enable t
      :exit t
      :map-inject t)
     ("M-p" company-ac-php-backend "Company Ac Php"
      :enable t
      :exit t
      :map-inject t))))

  :init
  (add-hook 'web-mode-hook
            'entropy/emacs-web--web-mode-start-hook)
  :config
  (when (display-graphic-p)
    (entropy/emacs-add-hook-lambda-nil
     web-mode-enable-development-env web-mode-hook 'append
     (setq-local entropy/emacs-web-development-environment
                 t))))

;; **** Emmet-mode for quick edittng
(use-package emmet-mode
  :defines (web-mode-hook html-mode-hook)
  :commands emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (define-key emmet-mode-keymap "C-c w" nil))

;; *** CSS mode
(use-package css-mode
  :ensure nil
  :eemacs-mmphc
  (((:enable t)) nil)
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
  :eemacs-mmphc
  (((:enable t)
    (nil nil nil t (2 2)))
   ("Basic"
    (("C-c C-w" js2-mode-toggle-warnings-and-errors "Toggle the display of warnings and errors"
      :enable t :exit t :map-inject t))
    "Hide and Show"
    (("C-c C-e" js2-mode-hide-element "Fold/hide contents of a block, showing ellipses"
      :enable t :exit t :map-inject t)
     ("C-c C-s" js2-mode-show-element "Show the hidden element at current point"
      :enable t :exit t :map-inject t)
     ("C-c C-o" js2-mode-toggle-element "Hide or show the foldable element at the point"
      :enable t :exit t :map-inject t)
     ("C-c C-f" js2-mode-toggle-hide-functions "Fully hide or show buffer content"
      :enable t :exit t :map-inject t)
     ("C-c C-a" js2-mode-show-all "Show all of the text in the buffer"
      :enable t :exit t :map-inject t)
     ("C-c C-t" js2-mode-toggle-hide-comments "Folds all block comments in the buffer"
      :enable t :exit t :map-inject t))
    "Repl" nil
    "Eval" nil
    "Web Beautify" nil))
  :init
  (entropy/emacs-lazy-load-simple js2-mode
    (require 'js2-old-indent)
    (require 'js2-imenu-extras)
    (entropy/emacs-add-hook-lambda-nil
     js2-initialized-common js2-mode-hook 'append
     (setq-local js2-basic-offset 4)
     (js2-highlight-unused-variables-mode 1)
     (js2-imenu-extras-mode 1))))

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
  (entropy/emacs-lazy-load-simple skewer-mode
    (dolist (el '(cache-table
                  skewer-bower
                  skewer-css
                  skewer-html
                  skewer-repl
                  skewer-setup))
      (require el)))
  (entropy/emacs-lazy-load-simple js2-mode
    (add-hook 'js2-mode-hook #'skewer-mode))
  (entropy/emacs-lazy-load-simple css-mode
    (add-hook 'css-mode-hook #'skewer-css-mode))
  (entropy/emacs-lazy-load-simple sgml-mode
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
  :preface
  (defun entropy/emacs-web--check-js-beautify-coworker ()
    (interactive)
    (entropy/emacs-coworker--coworker-install-by-npm
     "js-beautify" ("css-beautify" "html-beautify" "js-beautify")
     "js-beautify"))

  :eemacs-mmphca
  ((((:enable t)
     (js2-mode js2-mode js2-mode-map))
    ("Web Beautify"
     (("C-c C-b" web-beautify-js "Beautify Js"
       :enable t
       :exit t
       :map-inject t))))
   (((:enable t)
     (json-mode json-mode json-mode-map))
    ("Web Beautify"
     (("C-c C-b" web-beautify-js "Beautify Json"
       :enable t
       :exit t
       :map-inject t))))
   (((:enable t)
     (web-mode web-mode web-mode-map))
    ("Web Beautify"
     (("C-c C-b" web-beautify-html "Beautify html"
       :enable t
       :exit t
       :map-inject t))))
   (((:enable t)
     (sgml-mode sgml-mode sgml-mode-map))
    ("Web Beautify"
     (("C-c C-b" web-beautify-html "Beautify Xml"
       :enable t
       :exit t
       :map-inject t))))
   (((:enable t)
     (css-mode css-mode css-mode-map))
    ("Web Beautify"
     (("C-c C-b" web-beautify-html "Beautify Css"
       :enable t
       :exit t
       :map-inject t)))))

  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-"))

  ;; install `js-beautify' coworker
  (when entropy/emacs-install-coworker-immediately
    (advice-add
     el
     :before
     #'entropy/emacs-web--check-js-beautify-coworker)))

;; ** web backend technologies
;; *** php
(use-package php-mode
  :mode "\\.php$"
  :commands php-mode)

;; * provide
(provide 'entropy-emacs-web)
