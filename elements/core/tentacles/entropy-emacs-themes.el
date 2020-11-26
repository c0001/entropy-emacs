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
             solaire-global-mode
             turn-on-solaire-mode)
  :preface
  (defvar entropy/emacs-themes-solaire-startup-timer nil)

  (defun entropy/emacs-themes--enable-solaire-global-mode ()
    (when (entropy/emacs-theme-adapted-to-solaire)
      (unless solaire-global-mode
        (solaire-global-mode t)))
    (entropy/emacs-solaire-specific-for-themes))

  (defvar entropy/emacs-themes-solaire-after-load-theme-adapts-idle-delay 0.01)
  (defun entropy/emacs-themes-solaire-after-load-theme-adapts (&rest _)
    (when (entropy/emacs-theme-adapted-to-solaire)
      (let (timer)
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
                        entropy/emacs-themes-solaire-startup-timer)))))))))

  (defun entropy/emacs-themes-solaire-around-advice-for-make-frame
      (orig-func &rest orig-args)
    "Around advice for `make-frame' to fix bug of that solaire
face attributes and some refer is missing in the new frame."
    (let* ((face-reset nil)
           (frame-bg (alist-get 'background-color (car orig-args)))
           (frame-fg (alist-get 'foreground-color (car orig-args)))
           (cur-frame-bg (frame-parameter nil 'background-color))
           (cur-frame-fg (frame-parameter nil 'foreground-color))
           (new-frame (apply orig-func orig-args)))
      (unwind-protect
          (progn
            ;; Condition judger: commonly add exclusion of company
            ;; childframe tooltip for company-box which has its own
            ;; default frame background color face. So that any
            ;; further one can easily adding to this `or' form.
            (unless (or (and (bound-and-true-p company-candidates)
                             (or (bound-and-true-p company-box-mode)
                                 (bound-and-true-p company-posframe-mode)))
                        entropy/emacs-frame-be-made-is-child-frame)
              (let ()
                ;; map get enabled soloarized faces
                (when (bound-and-true-p solaire-mode-remap-alist)
                  (mapcar
                   (lambda (x)
                     (let ((map (car x))
                           (enable (cdr x)))
                       (setq enable (if (listp enable) (eval enable) (symbol-value enable)))
                       (when enable
                         (dolist (face map)
                           (when (facep face)
                             (push (cons face (entropy/emacs-get-face-attribute-alist face))
                                   face-reset))))))
                   solaire-mode-remap-alist))
                ;; more face specification
                (dolist (face '(
                                ;; ivy refer
                                ivy-current-match
                                ivy-yanked-word
                                ivy-completions-annotations
                                ivy-grep-line-number
                                ivy-grep-info
                                ivy-separator
                                ivy-prompt-match
                                ivy-highlight-face
                                ivy-action
                                ivy-virtual
                                ivy-remote
                                ivy-modified-outside-buffer
                                ivy-modified-buffer
                                ivy-org
                                ivy-subdir
                                ivy-match-required-face
                                ivy-confirm-face
                                ivy-minibuffer-match-face-4
                                ivy-minibuffer-match-face-3
                                ivy-minibuffer-match-face-2
                                ivy-minibuffer-match-face-1
                                ivy-minibuffer-match-highlight
                                ivy-current-match
                                ivy-cursor
                                ;; native primitive face
                                fringe))
                  (when (facep face)
                    (push (cons face (entropy/emacs-get-face-attribute-alist face))
                          face-reset)))
                ;; do face reset for new frame
                (dolist (pair face-reset)
                  (let ((face (car pair))
                        (attrs (cdr pair)))
                    (dolist (attr attrs)
                      (set-face-attribute
                       face
                       new-frame
                       (car attr)
                       (cdr attr)))))
                (when (and (and frame-bg (stringp frame-bg)) (and cur-frame-bg (stringp cur-frame-bg))
                           (not (equal frame-bg cur-frame-bg)))
                  (set-frame-parameter new-frame 'background-color frame-bg))
                (when (and (and frame-fg (stringp frame-fg)) (and cur-frame-fg (stringp cur-frame-fg))
                           (not (equal frame-fg cur-frame-fg)))
                  (set-frame-parameter new-frame 'foreground-color frame-fg))))
            new-frame)
        new-frame)))

  (defun entropy/emacs-themes--solaire-call-stuff-when-adapted
      (orig-func &rest orig-args)
     (when (entropy/emacs-theme-adapted-to-solaire)
       (apply orig-func orig-args)))

  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :init
  (setq solaire-mode-remap-fringe t
        solaire-mode-remap-modeline nil)
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          (or (buffer-file-name)
              (member major-mode
                      '(dired-mode
                        w3m-mode
                        eww-mode
                        elfeed-mode
                        magit-status-mode
                        Info-mode
                        Man-mode
                        woman-mode)))))

  (setq solaire-mode-auto-swap-bg t
        solaire-mode-themes-to-face-swap
        (let ((exclusion '(doom-dark+
                           doom-molokai
                           doom-horizon
                           doom-Iosvkem
                           doom-gruvbox
                           doom-nova
                           doom-solarized-light
                           doom-vibrant))
              (all-themes (custom-available-themes)))
          (delete nil
                  (mapcar (lambda (theme)
                            (unless (or (member theme exclusion)
                                        (not (entropy/emacs-theme-adapted-to-solaire
                                              theme)))
                              (regexp-quote (symbol-name theme))))
                          all-themes))))

  (entropy/emacs-lazy-with-load-trail
   solaire-mode-init
   :start-end t
   :body
   (add-hook 'entropy/emacs-theme-load-before-hook
             (lambda nil
               (when (bound-and-true-p solaire-global-mode)
                 (solaire-global-mode 0)
                 ;; disable remanding solair spec themes prevent from
                 ;; pollute the non solair adapted theme default bg
                 ;; colors
                 (disable-theme 'solaire-swap-bg-theme))))
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
      (setq theme 'doom-dark+))
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

   (defvar entropy/emacs-themes--theme-init-setup-for-daemon-done nil)
   (defun entropy/emacs-themes--load-theme-for-daemon-init (&optional frame)
     (redisplay t)
     (let ((frame (or frame (selected-frame))))
       (select-frame frame)
       (with-selected-frame (selected-frame)
         (unless entropy/emacs-themes--theme-init-setup-for-daemon-done
           (message "Daemon init theme ...")
           (entropy/emacs-themes--init-setup)
           (setq entropy/emacs-themes--theme-init-setup-for-daemon-done
                 1)))))

   (defvar entropy/emacs-themes--daemon-theme-reload-type nil)
   (defun entropy/emacs-themes--load-theme-for-daemon-client-new ()
     (let ()
       (if (and (frame-parameter nil 'eemacs-current-frame-is-daemon-created)
                ;; do not reload theme after dameon theme init by
                ;; `entropy/emacs-themes--load-theme-for-daemon-init'
                (eq entropy/emacs-themes--theme-init-setup-for-daemon-done 2))
           ;; prevent reload theme for same status as previous client created is
           (when (not (eq (null (display-graphic-p))
                          (null entropy/emacs-themes--daemon-theme-reload-type)))
             (message "Daemon reload theme ...")
             (entropy/emacs-themes-strictly-load-theme
              entropy/emacs-theme-sticker 'non-confirm)
             (setq entropy/emacs-themes--daemon-theme-reload-type
                   (display-graphic-p)))
         (when (and (frame-parameter nil 'eemacs-current-frame-is-daemon-created)
                    (eq entropy/emacs-themes--theme-init-setup-for-daemon-done 1))
           (setq entropy/emacs-themes--theme-init-setup-for-daemon-done 2)))))

   (add-hook 'after-make-frame-functions
             #'entropy/emacs-themes--load-theme-for-daemon-init)
   (add-hook 'entropy/emacs-daemon-server-after-make-frame-hook
             #'entropy/emacs-themes--load-theme-for-daemon-client-new)
   ))

;; * provide
(provide 'entropy-emacs-themes)
