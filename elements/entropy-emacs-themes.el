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
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defun)

;; ** Theme
(use-package doom-themes
  :preface (defvar region-fg nil) ; bug from: `url:https://github.com/hlissner/emacs-doom-themes/issues/166'
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

(use-package doom-themes-ext-visual-bell
  :ensure nil
  :commands (doom-themes-visual-bell-config)
  :init
  (entropy/emacs-lazy-with-load-trail
   doom-visual-bell
   (doom-themes-visual-bell-config)))

(use-package doom-themes-ext-org
  :ensure nil
  :commands (doom-themes-org-config)
  :init
  (entropy/emacs-lazy-with-load-trail
   doom-org-specific
   (doom-themes-org-config)))

;; *** theme load specifix

(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-theme-load-face-specifix)
(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-theme-load-modeline-specifix)

;; *** Initialize theme and adapting to the daemon init

(entropy/emacs-lazy-with-load-trail
 enable-theme
 (redisplay t)
 (mapc #'disable-theme custom-enabled-themes)
 (condition-case nil
     (progn
       (load-theme entropy/emacs-theme-options t)
       (redisplay t))
   (error "Problem loading theme %s"
          (symbol-name entropy/emacs-theme-options)))
 (when (and (fboundp 'powerline-reset)
            (string-match-p
             "space\\|powerline"
             entropy/emacs-modeline-style))
   (powerline-reset))
 (entropy/emacs-theme-load-face-specifix
  (symbol-name entropy/emacs-theme-options))
 (when (daemonp)
   ;; This issue refer to
   ;; `https://github.com/hlissner/emacs-doom-themes/issues/125'.

   ;; For generally about, this issue brought the bad theme
   ;; presentation in daemon mode of init of emacs session.

   (add-hook 'after-make-frame-functions
             #'(lambda (frame)
	         (when (eq (length (frame-list)) 2)
		   (progn
		     (select-frame
		      frame)
		     (load-theme entropy/emacs-theme-options)))))))

;; ** solaire mode for focus visual style
(use-package solaire-mode
  :commands (solaire-mode
             solaire-mode-swap-bg
             solaire-global-mode
             turn-on-solaire-mode)
  :preface

  ;; pdumper session specification
  (defun entropy/emacs-theme--solaire-get-origin-faces ()
    (cl-loop for ((orig-face solaire-face) . judge) in solaire-mode-remap-alist
             when (eval judge)
             collect solaire-face))

  (defun entropy/emacs-theme--solaire-force-set-faces (cur-theme)
    (let ((settings (get cur-theme 'theme-settings))
          (solaire-faces
           (entropy/emacs-theme--solaire-get-origin-faces)))
      (dolist (s settings)
        (let* ((face (cadr s))
               (face-spec (cadddr s)))
          (when (member face solaire-faces)
	           (face-spec-set face face-spec))))))
  
  (defun entropy/emacs-theme--recovery-solaire-faces ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (entropy/emacs-theme--solaire-force-set-faces
       entropy/emacs-theme-sticker))
    (face-spec-set
     'default
     (entropy/emacs-get-theme-face
      entropy/emacs-theme-sticker 'default)))
  
  ;; solaire entropy-emacs configuration

  (defvar entropy/emacs-theme--solaire-is-enabled nil
    "Transient variable for indicating buffer `solaire-mode'
    status.")
  
  (defun entropy/emacs-theme--solaire-enable ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (let (is-swaped)
        (mapc
         (lambda (x)
           (when (buffer-file-name x)
             (with-current-buffer x
               (solaire-mode +1)
               (unless is-swaped
                 (solaire-mode-swap-bg)
                 (setq is-swaped t)))))
         (buffer-list)))))
  
  (defun entropy/emacs-theme--solaire-disable ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (when (bound-and-true-p solaire-global-mode)
        (solaire-global-mode -1))
      (mapc
       (lambda (x)
         (with-current-buffer x
           (when (bound-and-true-p solaire-mode)
             (solaire-mode -1))))
       (buffer-list))))

  (defun entropy/emacs-theme--solaire-enable-single ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (solaire-mode +1)))
  
  (defun entropy/emacs-theme--solaire-initial-hooks ()
    (dolist (el entropy/emacs-enable-solaire-registers)
      (dolist (hook (cdr el))
        (eval-after-load (car el)
          `(add-hook ',hook #'entropy/emacs-theme--solaire-enable-single)))))

  (defun entropy/emacs-theme--initilized-start-solaire-mode ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (with-temp-buffer
        (solaire-mode +1)
        (solaire-mode-swap-bg))
      (redisplay t))
    (entropy/emacs-theme--solaire-initial-hooks)
    (add-hook 'change-major-mode-hook
              #'(lambda ()
                  (when (bound-and-true-p solaire-mode)
                    (setq entropy/emacs-theme--solaire-is-enabled t))))
    (add-hook 'after-change-major-mode-hook
              #'(lambda ()
                  (unwind-protect
                      (when entropy/emacs-theme--solaire-is-enabled
                        (entropy/emacs-theme--solaire-enable-single))
                    (setq entropy/emacs-theme--solaire-is-enabled nil))))
    (add-hook 'entropy/emacs-theme-load-before-hook
              #'entropy/emacs-theme--solaire-disable)
    (add-hook 'entropy/emacs-theme-load-after-hook
              #'entropy/emacs-theme--solaire-enable))

  :init
  (setq solaire-mode-remap-fringe nil)
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          t))

  (entropy/emacs-lazy-with-load-trail
   solaire-mode
   (entropy/emacs-theme--initilized-start-solaire-mode)
   (when entropy/emacs-fall-love-with-pdumper
     (add-hook 'entropy/emacs-theme-load-after-hook
               #'entropy/emacs-theme--recovery-solaire-faces
               100))))

;; ** page-break-lines style form Purcell
;;
;;     This Emacs library provides a global mode which displays ugly form feed characters as tidy
;;     horizontal rules.
(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode)
  :init
  (entropy/emacs-lazy-with-load-trail
   PageBreakLines
   (global-page-break-lines-mode +1)))

;; * provide
(provide 'entropy-emacs-themes)
