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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defun)

;; ** Theme
;; *** defined themes
(use-package doom-themes
  :preface (defvar region-fg nil) ; bug from: `url:https://github.com/hlissner/emacs-doom-themes/issues/166'
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        )
  ;; slide tree doom visual
  (cond ((eq entropy/emacs-tree-visual-type 'neotree)
         (entropy/emacs-lazy-load-simple neotree
           (setq doom-themes-neotree-file-icons
                 (cond
                  ((display-graphic-p)
                   t)
                  (t
                   'nerd)))
           (require 'doom-themes-ext-neotree)))
        ((eq entropy/emacs-tree-visual-type 'treemacs)
         (entropy/emacs-lazy-load-simple treemacs
           (setq doom-theme-treeemacs-theme "doom-atom")
           (require 'doom-themes-ext-treemacs)))))

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

;; ** Solaire mode for focus visual style
(use-package solaire-mode
  :commands (solaire-mode
             solaire-mode-swap-bg
             solaire-global-mode
             turn-on-solaire-mode)
  :preface
  (defvar entropy/emacs-themes-solaire-startup-timer nil)

  (defun entropy/emacs-themes--solaire-swap-bg-needed ()
    (member entropy/emacs-theme-sticker
            '(doom-dark+
              doom-molokai
              doom-horizon
              doom-Iosvkem
              doom-gruvbox
              doom-nova
              doom-solarized-light
              doom-vibrant)))

  (defun entropy/emacs-themes--solaire-swap-bg ()
    (unless (entropy/emacs-themes--solaire-swap-bg-needed)
      (solaire-mode-swap-bg)))

  (defun entropy/emacs-themes-solaire-after-load-theme-adapts (&rest _)
    (if (entropy/emacs-theme-adapted-to-solaire)
        (let (timer)
          (unless solaire-global-mode
            (solaire-global-mode t))
          (condition-case error
              (if entropy/emacs-startup-done
                  (setq timer
                        (run-with-idle-timer 0.01 nil #'entropy/emacs-themes--solaire-swap-bg))
                (setq entropy/emacs-themes-solaire-startup-timer
                      (run-with-idle-timer
                       0.01 t
                       #'(lambda ()
                           (when entropy/emacs-startup-done
                             (entropy/emacs-themes--solaire-swap-bg)
                             (cancel-timer
                              entropy/emacs-themes-solaire-startup-timer))))))
            (error
             (when (timerp timer)
               (cancel-timer timer)))))
      (solaire-global-mode 0)))

  (defun entropy/emacs-solaire-call-stuff-when-adapted
      (orig-func &rest orig-args)
     (when (entropy/emacs-theme-adapted-to-solaire)
       (apply orig-func orig-args)))

  (defun entropy/emacs-themes-solaire-swap-bg ()
    "Interacive wrapper for `solaire-mode-swap-bg'"
    (interactive)
    (solaire-mode-swap-bg))

  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :init
  (setq solaire-mode-remap-fringe nil
        solaire-mode-remap-modeline nil)
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          (or (buffer-file-name)
              (member major-mode
                      '(dired-mode
                        w3m-mode
                        eww-mode
                        elfeed-mode
                        magit-status-mode)))))
  (entropy/emacs-lazy-with-load-trail
   solaire-mode-init
   (add-hook 'entropy/emacs-theme-load-after-hook-end-1
             #'entropy/emacs-themes-solaire-after-load-theme-adapts)
   (add-hook 'entropy/emacs-theme-load-after-hook-end-2
             #'entropy/emacs-solaire-specific-for-themes)
   (solaire-global-mode))

  :config
  (advice-add 'turn-on-solaire-mode
              :around
              #'entropy/emacs-solaire-call-stuff-when-adapted)
  (advice-add 'solaire-mode-in-minibuffer
              :around
              #'entropy/emacs-solaire-call-stuff-when-adapted))

;; ** Page-break-lines style form Purcell
;;
;;     This Emacs library provides a global mode which displays ugly form feed characters as tidy
;;     horizontal rules.
(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode)
  :init

  (setq page-break-lines-modes
        '(emacs-lisp-mode
          lisp-mode
          scheme-mode
          compilation-mode outline-mode))

  (entropy/emacs-lazy-with-load-trail
   PageBreakLines
   (global-page-break-lines-mode +1)))

;; ** Initialize theme and adapting to the daemon init

;; spacemacs theme has the best tui adaptable
(unless (display-graphic-p)
  (setq entropy/emacs-theme-options 'doom-Iosvkem))

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

;; * provide
(provide 'entropy-emacs-themes)
