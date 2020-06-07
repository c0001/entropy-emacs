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
(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-adjust-org-heading-scale)

;; ** Solaire mode for focus visual style
(use-package solaire-mode
  :commands (solaire-mode
             solaire-mode-swap-bg
             solaire-global-mode
             turn-on-solaire-mode)
  :preface
  (defvar entropy/emacs-themes-solaire-startup-timer nil)

  (defun entropy/emacs-themes--solaire-swap-bg-un-needed ()
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
    (when (and (not (entropy/emacs-themes--solaire-swap-bg-un-needed))
               (entropy/emacs-theme-adapted-to-solaire))
      (solaire-mode-swap-bg)
      (message "Solaire swap bg done!")))

  (defun entropy/emacs-themes--enable-solaire-global-mode ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (unless solaire-global-mode
        (solaire-global-mode t)))
    (entropy/emacs-themes--solaire-swap-bg)
    (entropy/emacs-solaire-specific-for-themes))

  (defvar entropy/emacs-themes-solaire-after-load-theme-adapts-idle-delay 0.01)
  (defun entropy/emacs-themes-solaire-after-load-theme-adapts (&rest _)
    (when (entropy/emacs-theme-adapted-to-solaire)
      (let (timer)
        (condition-case error
            (if entropy/emacs-startup-done
                (setq timer
                      (run-with-idle-timer
                       entropy/emacs-themes-solaire-after-load-theme-adapts-idle-delay
                       nil #'entropy/emacs-themes--enable-solaire-global-mode))
              (setq entropy/emacs-themes-solaire-startup-timer
                    (run-with-idle-timer
                     entropy/emacs-themes-solaire-after-load-theme-adapts-idle-delay
                     t
                     #'(lambda ()
                         (when entropy/emacs-startup-done
                           (entropy/emacs-themes--enable-solaire-global-mode)
                           (cancel-timer
                            entropy/emacs-themes-solaire-startup-timer))))))
          (error
           (message "Error when swap solaire backgroud colors.")
           (when (timerp timer)
             (cancel-timer timer)))))))

  (defun entropy/emacs-themes-solaire-around-advice-for-make-frame
      (orig-func &rest orig-args)
    (let* ((face-reset nil)
           (frame-bg (alist-get 'background-color (car orig-args)))
           (frame-fg (alist-get 'foreground-color (car orig-args)))
           (cur-frame-bg (frame-parameter nil 'background-color))
           (cur-frame-fg (frame-parameter nil 'foreground-color))
           (new-frame (apply orig-func orig-args)))
      (unwind-protect
          (progn
            (let ()
              (mapcar
               (lambda (x)
                 (let ((map (car x))
                       (enable (cdr x)))
                   (setq enable (if (listp enable) (eval enable) (symbol-value enable)))
                   (when enable
                     (dolist (face map)
                       (when (facep face)
                         (push (cons face (face-attribute face :background))
                               face-reset))))))
               solaire-mode-remap-alist)
              (dolist (face '(ivy-current-match))
                (when (facep face)
                  (push (cons face (face-attribute face :background))
                        face-reset)))
              (dolist (pair face-reset)
                (set-face-attribute
                 (car pair)
                 new-frame
                 :background
                 (cdr pair)))
              (when (and (and frame-bg (stringp frame-bg)) (and cur-frame-bg (stringp cur-frame-bg))
                         (not (equal frame-bg cur-frame-bg)))
                (set-frame-parameter new-frame 'background-color frame-bg))
              (when (and (and frame-fg (stringp frame-fg)) (and cur-frame-fg (stringp cur-frame-fg))
                         (not (equal frame-fg cur-frame-fg)))
                (set-frame-parameter new-frame 'foreground-color frame-fg))
              new-frame))
        new-frame)))

  (defun entropy/emacs-themes--solaire-call-stuff-when-adapted
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
   :start-end t
   :body
   (add-hook 'entropy/emacs-theme-load-before-hook
             (lambda nil
               (when (bound-and-true-p solaire-global-mode)
                 (solaire-global-mode 0))))
   (add-hook 'entropy/emacs-theme-load-after-hook-end-1
             #'entropy/emacs-themes-solaire-after-load-theme-adapts)
   (advice-add 'make-frame
               :around
               #'entropy/emacs-themes-solaire-around-advice-for-make-frame)
   (entropy/emacs-themes--enable-solaire-global-mode))

  :config
  (advice-add 'turn-on-solaire-mode
              :around
              #'entropy/emacs-themes--solaire-call-stuff-when-adapted)
  (advice-add 'solaire-mode-in-minibuffer
              :around
              #'entropy/emacs-themes--solaire-call-stuff-when-adapted))

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

(defun entropy/emacs-themes-strictly-load-theme
    (theme &optional no-confirm no-enable)
  "Load theme as `load-theme' but strictly using special theme
for tui emacs session."
  (let ((theme theme))
    (unless (display-graphic-p)
      (setq theme 'spacemacs-dark))
    (load-theme theme no-confirm no-enable)))

(entropy/emacs-lazy-with-load-trail
 enable-theme
 (redisplay t)
 (mapc #'disable-theme custom-enabled-themes)

 (defun entropy/emacs-themes--init-setup ()
   (condition-case nil
       (progn
         (entropy/emacs-themes-strictly-load-theme
          entropy/emacs-theme-options t)
         (redisplay t))
     (error "Problem loading theme %s"
            (symbol-name entropy/emacs-theme-options)))
   (when (and (fboundp 'powerline-reset)
              (string-match-p
               "space\\|powerline"
               entropy/emacs-modeline-style))
     (powerline-reset))
   (entropy/emacs-theme-load-face-specifix
    (symbol-name entropy/emacs-theme-options)))

 (if (null (daemonp))
     (entropy/emacs-themes--init-setup)
   ;; This issue refer to
   ;; `https://github.com/hlissner/emacs-doom-themes/issues/125'.

   ;; For generally about, this issue brought the bad theme
   ;; presentation in daemon mode of init of emacs session.

   ;; (defvar entropy/emacs-themes--timer-for-daemon-theme-reload nil)
   (defvar entropy/emacs-themes--theme-init-setup-for-daemon-done nil)
   (defun entropy/emacs-themes--load-theme-for-daemon-client (&optional frame)
     (redisplay t)
     (let ()
       (progn
         (select-frame (or frame (selected-frame)))
         (if entropy/emacs-themes--theme-init-setup-for-daemon-done
             (when (or (not entropy/emacs-daemon-main-client-indicator)
                       ;; We must forcely reload theme to gurantee the
                       ;; gui daemon client created successfully for
                       ;; some bug of doom-themes.
                       (display-graphic-p))
               (entropy/emacs-themes-strictly-load-theme
                entropy/emacs-theme-sticker 'non-confirm))
           (entropy/emacs-themes--init-setup)
           (setq entropy/emacs-themes--theme-init-setup-for-daemon-done
                 t)))))

   (add-hook 'after-make-frame-functions
             #'entropy/emacs-themes--load-theme-for-daemon-client)))

;; * provide
(provide 'entropy-emacs-themes)
