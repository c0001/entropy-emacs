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
  :after doom-themes
  :commands (doom-themes-visual-bell-config)
  :init
  
  (doom-themes-visual-bell-config)
   
  :config
  (defvar doom-themes--visual-bell-old-mdl-face-remap nil
    "The bakcup modeline face for doom themes visual bell
procedure.")
  
  (defun doom-themes-visual-bell-fn-core (buff)
    (when (and (assq 'mode-line face-remapping-alist)
               doom-themes--bell-p
               (buffer-live-p buff))
      (with-current-buffer buff
        (setq-local face-remapping-alist doom-themes--visual-bell-old-mdl-face-remap)
        (setq-local doom-themes--bell-p nil)
        (force-mode-line-update))))
  
  (defun doom-themes-visual-bell-fn ()
    "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it.

Note: this function has been redefined for adapting for [entropy-emacs]

The origin face map recovery rely on the buffer (current-buffer)
actived status, otherwise for error, which may encounter the
occasion that the timer run after the buffer killed, that the
remap for recovering failed. 

This patch split the recover func to
`doom-themes-visual-bell-fn-core' and using the `buffer-alive-p'
as the trigger condition sentinal.

All face remap are buffer local binding, so that it doesn't mess
the gloabal way."
    (unless doom-themes--bell-p
      (let ((old-remap (copy-tree face-remapping-alist))
            (buff (current-buffer))
            unwind-trigger)
        (unwind-protect
            (with-current-buffer buff
              (setq-local doom-themes--visual-bell-old-mdl-face-remap old-remap)
              (setq-local doom-themes--bell-p t)
              (setq-local face-remapping-alist
                          (append (delete (assq 'mode-line face-remapping-alist)
                                          face-remapping-alist)
                                  '((mode-line doom-visual-bell))))
              (force-mode-line-update)
              (run-with-timer 0.1 nil
                              #'doom-themes-visual-bell-fn-core (current-buffer))
              (setq unwind-trigger t))
          (unless unwind-trigger
            (message "Visual Bell fatal")))))))

(use-package doom-themes-ext-org
  :ensure nil
  :after doom-themes
  :commands (doom-themes-org-config)
  :init
  (doom-themes-org-config))

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

  ;; solaire entropy-emacs configuration
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
          `(lambda () (add-hook ',hook #'entropy/emacs-theme--solaire-enable-single))))))

  (defun entropy/emacs-theme--initilized-start-solaire-mode ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (with-temp-buffer
        (solaire-mode +1)
        (solaire-mode-swap-bg))
      (redisplay t))
    (entropy/emacs-theme--solaire-initial-hooks)
    (add-hook 'entropy/emacs-theme-load-before-hook
              #'entropy/emacs-theme--solaire-disable)
    (add-hook 'entropy/emacs-theme-load-after-hook
              #'entropy/emacs-theme--solaire-enable))

  :init
  (setq solaire-mode-remap-fringe nil)
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          t))
  (cond
   (entropy/emacs-custom-pdumper-do
    ;; TODO  `solaire-mode' can not work correctly in pdumper session
    ;; referred bug of `h-86e0180b-bcf0-484d-bc21-9502d8abeb58'
    ;; (add-hook 'entropy/emacs-pdumper-load-end-hook
    ;;           #'entropy/emacs-theme--initilized-start-solaire-mode)
    )
   (t
    (add-hook (entropy/emacs-select-x-hook) #'entropy/emacs-theme--initilized-start-solaire-mode))))

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
