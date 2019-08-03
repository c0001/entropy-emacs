;;; entropy-emacs-themes.el --- entropy-emacs UI theme configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-themes.el
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
;; `entropy-emacs' UI theme setting and toggle features collection.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** Theme
(use-package doom-themes
  :preface (defvar region-fg nil) ; bug from: `url:https://github.com/hlissner/emacs-doom-themes/issues/166'
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  :config
  (use-package doom-themes-ext-org
    :ensure nil
    :after doom-themes
    :commands (doom-themes-org-config)
    :init
    (doom-themes-org-config)))


;; *** advice for register theme to `entropy/emacs-theme-sticker'
(when (fboundp 'entropy/emacs-theme-load-register)
  (advice-add 'load-theme :around #'entropy/emacs-theme-load-register))

;; *** Adapting to the daemon init and with customize theme choosen
;;     
;;     This issue refer to `https://github.com/hlissner/emacs-doom-themes/issues/125'.
;;
;;     For generally about, this issue brought the bad theme presentation in daemon mode of init of
;;     emacs session.
;; 
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
					    (when (eq (length (frame-list)) 2)
					      (progn
						(select-frame
						 frame)
						(load-theme entropy/emacs-theme-options)))))
  
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (when (or (display-graphic-p)
                  entropy/emacs-enable-option-theme-tty)
          (load-theme entropy/emacs-theme-options t)
          (when (fboundp 'powerline-reset)
            (powerline-reset))
          (entropy/emacs-theme-load-face-specifix (symbol-name entropy/emacs-theme-options))))
    (error "Problem loading theme %s" (symbol-name entropy/emacs-theme-options))))

;; ** page-break-lines style form Purcell
;;
;;     This Emacs library provides a global mode which displays ugly form feed characters as tidy
;;     horizontal rules.
(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode)
  :init
  (global-page-break-lines-mode))

;; * provide
(provide 'entropy-emacs-themes)
