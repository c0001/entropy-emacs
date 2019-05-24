;;; File name: init-themes ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** Theme
(use-package doom-themes
  :preface (defvar region-fg nil) ; bug from: `url:https://github.com/hlissner/emacs-doom-themes/issues/166'
  :commands (doom-themes-org-config)
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; *** other options theme
;; **** sanityinc
(use-package color-theme-sanityinc-tomorrow
  :commands (color-theme-sanityinc-tomorrow
             color-theme-sanityinc-tomorrow-night
             color-theme-sanityinc-tomorrow-day
             color-theme-sanityinc-tomorrow-bright
             color-theme-sanityinc-tomorrow-eighties
             color-theme-sanityinc-tomorrow-blue))

;; **** warming theme
(use-package birds-of-paradise-plus-theme)

;; **** ocean color theme
(use-package gotham-theme)

;; **** atom theme
(use-package atom-dark-theme)
(use-package atom-one-dark-theme)
;; **** light like github theme
(use-package doneburn-theme
  :preface (defvar doneburn-override-colors-alist 'nil))

(use-package github-theme)

;; **** klere which was mordern dark type
(use-package klere-theme)

;; **** material theme for more morden UI
(use-package material-theme)

;; **** spacemacs the most famous theme in emacs
(use-package spacemacs-theme
  :init
  (with-eval-after-load 'ivy
    (cond ((eq (car custom-enabled-themes) 'spacemacs-light)
           (set-face-attribute 'ivy-current-match nil
                               :background "salmon" :bold t))
          ((eq (car custom-enabled-themes) 'spacemacs-dark)
           (set-face-attribute 'ivy-current-match nil
                               :background "purple4" :bold t)))))
;; **** sublime like theme
(use-package sublime-themes)

;; **** darkokai-theme
(use-package darkokai-theme
  :init (setq darkokai-use-variable-pitch nil))
;; **** ujelly-theme
(use-package ujelly-theme)

;; **** srcery theme
(use-package srcery-theme)


;; **** Adding option themes to theme path
(unless (eq entropy/emacs-use-extensions-type 'origin)
  (let* ((base-dir (expand-file-name (expand-file-name "elements/submodules" entropy/emacs-ext-extensions-dir)))
         (theme-list '("color-theme-sanityinc-tomorrow"
                       "birds-of-paradise-plus-theme.el"
                       "gotham-theme"
                       "atom-dark-theme-emacs"
                       "atom-one-dark-theme"
                       "GitHub-Theme-for-Emacs"
                       "doneburn-theme"
                       "emacs-klere-theme"
                       "emacs-material-theme"
                       "spacemacs-theme"
                       "emacs-color-themes"
                       "darkokai"
                       "color-theme-ujelly"
                       "srcery-emacs")))
    (setq theme-list (mapcar #'(lambda (x)
                                 (expand-file-name x base-dir))
                             theme-list))
    (mapc #'(lambda (x)
              (add-to-list 'custom-theme-load-path x))
          theme-list)))

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
          (cond
           ((string-match-p "spacemacs-dark" (symbol-name entropy/emacs-theme-options))
            (set-face-attribute 'ivy-current-match nil
                                :background "purple4" :bold t))
           ((string-match-p "spacemacs-light)" (symbol-name entropy/emacs-theme-options))
            (set-face-attribute 'ivy-current-match nil
                                :background "salmon" :bold t))
           ((string-match-p "darkokai" (symbol-name entropy/emacs-theme-options))
            (set-face-attribute 'ivy-current-match nil
                                :background "#65a7e2"))
           ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" (symbol-name entropy/emacs-theme-options))
            (if (equal 'dark (frame-parameter nil 'background-mode))
                (set-face-attribute 'ivy-current-match nil
                                    :background "#65a7e2" :foreground "black")
              (set-face-attribute 'ivy-current-match nil
                                  :background "#1a4b77" :foreground "white")))
           ((string= "doom-solarized-light" (symbol-name entropy/emacs-theme-options))
            (with-eval-after-load 'hl-line
              (set-face-attribute 'hl-line nil :background "moccasin"))))))
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
