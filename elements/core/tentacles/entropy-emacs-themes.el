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
        ))

(use-package doom-themes-ext-visual-bell
  :ensure nil
  :commands (doom-themes-visual-bell-config)
  :init
  (entropy/emacs-lazy-initial-advice-before
   (keyboard-quit)
   "doom-visual-bell" "doom-visual-bell" prompt-echo
   :pdumper-no-end t
   (doom-themes-visual-bell-config)))

(use-package doom-themes-ext-org
  :ensure nil
  :commands (doom-themes-org-config)
  :init
  (entropy/emacs-lazy-initial-advice-before
   (org-mode)
   "doom-org-specific" "doom-org-specific" prompt-echo
   :pdumper-no-end t
   (doom-themes-org-config)))

(use-package ujelly-theme
  :config
  (custom-theme-set-variables
   'ujelly
   '(ansi-color-names-vector ["#323f4e" "#fb0120" "#a1c659" "#fda331" "#6fb3d2" "#d381c3" "#6fb3d2" "#e0e0e0"])
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]))
  )

(use-package color-theme-sanityinc-tomorrow)
(use-package gotham-theme)
(use-package srcery-theme)


;; *** theme load specifix

(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-theme-load-face-specifix)
(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-theme-load-modeline-specifix)
(add-hook 'entropy/emacs-theme-load-after-hook #'entropy/emacs-adjust-org-heading-scale)

;; TODO: accomplete this hook
(defun entropy/emacs-themes-set-margin/fringe-width/style
    (&rest _)
  ;; reset the fringe style to default
  (set-fringe-style))

(add-hook 'entropy/emacs-theme-load-after-hook-end-2
          #'entropy/emacs-themes-set-margin/fringe-width/style)

;; ** Solaire mode for focus visual style

(defgroup solaire-mode nil
  "Options for solaire-mode."
  :group 'faces)

(defface solaire-default-face '((t :inherit default))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-fringe-face '((t :inherit solaire-default-face))
  "Alternative version of the `fringe' face."
  :group 'solaire-mode)

(defface solaire-line-number-face
  `((t :inherit (,(if (boundp 'display-line-numbers) 'line-number 'linum)
                 solaire-default-face)))
  "Alternative face for `line-number'.
Used by native line numbers in Emacs 26+ and `linum'."
  :group 'solaire-mode)

(defface solaire-hl-line-face '((t :inherit hl-line))
  "Alternative face for the current line, highlighted by `hl-line'."
  :group 'solaire-mode)

(defface solaire-tooltip-face '((t :inherit tooltip))
  "Alternative face for the `tooltip'."
  :group 'solaire-mode)

(defface solaire-company-tooltip-face '((t :inherit solaire-tooltip-face))
  "Alternative face for the `company-tooltip'."
  :group 'solaire-mode)

(defface solaire-org-hide-face '((t :inherit org-hide))
  "Alternative face for `org-hide'.
Used to camoflauge the leading asterixes in `org-mode' when
`org-hide-leading-stars' is non-nil."
  :group 'solaire-mode)

(defface solaire-region-face '((t :inherit region))
  "Alternative face for `region' (the active selection)."
  :group 'solaire-mode)

(defface solaire-mode-line-face '((t :inherit mode-line))
  "Alternative face for the `mode-line' face."
  :group 'solaire-mode)

(defface solaire-mode-line-inactive-face '((t :inherit mode-line-inactive))
  "Alternative face for the `mode-line-inactive' face."
  :group 'solaire-mode)

(defface solaire-header-line-face '((t :inherit header-line))
  "Alternative face for the `header-line' face."
  :group 'solaire-mode)


(defcustom entropy/emacs-solaire-remap-alist
  `((default                    . solaire-default-face)
    (hl-line                    . solaire-hl-line-face)
    (tooltip                    . solaire-tooltip-face)
    (company-tooltip            . solaire-company-tooltip-face)
    (region                     . solaire-region-face)
    (org-hide                   . solaire-org-hide-face)
    (org-indent                 . solaire-org-hide-face)
    (linum                      . solaire-line-number-face)
    (line-number                . solaire-line-number-face)
    (header-line                . solaire-header-line-face)
    (mode-line                  . solaire-mode-line-face)
    (mode-line-inactive         . solaire-mode-line-inactive-face)
    (highlight-indentation-face . solaire-hl-line-face)
    ,@(unless (version<= emacs-version "26")
        '((fringe               . solaire-fringe-face))))
  "An alist of faces to remap when enabling `entropy/emacs-solaire-mode'."
  :group 'solaire-mode
  :type '(repeat (cons (face :tag "Source face")
                       (face :tag "Destination face"))))

(defvar-local entropy/emacs-solaire-mode--local-faces-remap-cookie nil
  "The local `face-remapping-alist' generated sibling cookie for
`entropy/emacs-solaire-mode', used for
`face-remap-remove-relative'.")

(define-minor-mode entropy/emacs-solaire-mode
  "EEMACS specified `solaire-mode', simple and fast, without
buggy face pollution and messy advices since we just use
`face-remapping-alist' to do thus. The global mode is
`entrorpy/emacs-solaire-global-mode' but use
`entropy/emacs-themes-enable-solaire-global-mode-with-spec'
instead and see it for details."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (progn
    (mapc #'face-remap-remove-relative
          entropy/emacs-solaire-mode--local-faces-remap-cookie)
    (setq entropy/emacs-solaire-mode--local-faces-remap-cookie nil))
  ;; Don't kick in if the current theme doesn't support solaire-mode.
  (if (not (entropy/emacs-theme-adapted-to-solaire))
      (setq entropy/emacs-solaire-mode nil)
    (when entropy/emacs-solaire-mode
      (dolist (remap entropy/emacs-solaire-remap-alist)
        (when (cdr remap)
          (push (face-remap-add-relative (car remap) (cdr remap))
                entropy/emacs-solaire-mode--local-faces-remap-cookie))))))

(defun entropy/emacs-solaire-mode-turn-on (&rest _)
  "Turrn on `entropy/emacs-solaire-mode', the subroutine of
`entropy/emacs-solaire-global-mode'."
  (and (not entropy/emacs-solaire-mode)
       (or
        ;; NOTE: do not use solaire in minibuffer since it's may need
        ;; more works to do proper for that.
        ;; ;; (minibufferp)
        (funcall
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
                         woman-mode))))))
       (entropy/emacs-solaire-mode +1)))

(define-globalized-minor-mode
  entropy/emacs-solaire-global-mode
  entropy/emacs-solaire-mode
  entropy/emacs-solaire-mode-turn-on)

(defun entropy/emacs-themes-enable-solaire-global-mode-with-spec ()
  "Enable `entropy/emacs-solaire-global-mode' with
specification. This is the only eemacs official
`entropy/emacs-solaire-global-mode' enable caller."
  (entropy/emacs-solaire-global-mode)
  ;; downgrade `company-tooltip' face when needed
  (when (entropy/emacs-theme-adapted-to-solaire)
    (entropy/emacs-face-bg-scale-when-same
     'company-tooltip 'solaire-default-face
     (cond
      ((or (eq (frame-parameter nil 'background-mode) 'light)
           (string-match-p "\\(light\\|day\\)"
                           (symbol-name entropy/emacs-theme-sticker)))
       0.95)
      ((eq (frame-parameter nil 'background-mode) 'dark)
       0.5))
     nil
     (when (member (face-attribute 'company-tooltip :background)
                   '(unspecified nil))
       t)))
  (entropy/emacs-solaire-specific-for-themes))

(defvar
  entropy/emacs-solaire-solaire-daemon-idle-delay
  0.01
  "Delay time of timer `entropy/emacs-themes-solaire-startup-timer'.")

(defvar entropy/emacs-themes-solaire-startup-timer nil
  "Timer for as an daemon to startup
`entropy/emacs-solaire-global-mode' when eemacs startup indicated
by `entropy/emacs-startup-done'.")

(defun entropy/emacs-themes-startup-solaire-mode-as-daemon (&rest _)
  "Startup `entropy/emacs-solaire-global-mode' as the subset of
`entropy/emacs-themes-solaire-startup-timer'."
  (when (entropy/emacs-theme-adapted-to-solaire)
    (let (_)
      (if entropy/emacs-startup-done
          (entropy/emacs-themes-enable-solaire-global-mode-with-spec)
        (setq entropy/emacs-themes-solaire-startup-timer
              (run-with-idle-timer
               entropy/emacs-solaire-solaire-daemon-idle-delay
               t
               #'(lambda ()
                   (when entropy/emacs-startup-done
                     (entropy/emacs-themes-enable-solaire-global-mode-with-spec)
                     (cancel-timer
                      entropy/emacs-themes-solaire-startup-timer)))))))))

(entropy/emacs-lazy-with-load-trail
 entropy/emacs-solaire-mode-init
 :start-end t
 :body
 (add-hook 'entropy/emacs-theme-load-before-hook
           #'(lambda nil
               (entropy/emacs-solaire-global-mode 0)))
 (add-hook 'entropy/emacs-theme-load-after-hook-end-1
           #'entropy/emacs-themes-startup-solaire-mode-as-daemon))

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
          compilation-mode outline-mode)
        page-break-lines-max-width 70)

  (entropy/emacs-lazy-initial-advice-before
   (find-file)
   "PageBreakLines" "PageBreakLines" prompt-echo
   :pdumper-no-end t
   (global-page-break-lines-mode +1)))

;; ** Initialize theme and adapting to the daemon init

(defun entropy/emacs-themes-strictly-load-theme
    (theme &optional no-confirm no-enable)
  "Load theme as `load-theme' but strictly using special theme
for tui emacs session."
  (let ((theme theme))
    (unless (display-graphic-p)
      (setq theme 'ujelly))
    (load-theme theme no-confirm no-enable)))

(defun entropy/emacs-themes-init-setup-user-theme ()
  "Load theme `entropy/emacs-theme-options' in emacs initial
progress."
  (mapc #'disable-theme custom-enabled-themes)
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

(unless entropy/emacs-fall-love-with-pdumper
  (entropy/emacs-lazy-with-load-trail
   enable-theme
   (redisplay t)
   (if (null (daemonp))
       ;; common status theme load process
       (entropy/emacs-themes-init-setup-user-theme)
     ;; --------------------
     ;; Daemon theme load specifications
     ;; --------------------
     (defvar entropy/emacs-themes--theme-init-setup-for-daemon-done nil)
     (defvar entropy/emacs-themes--daemon-theme-reload-type nil)
     (defun entropy/emacs-themes--load-theme-for-daemon-client-new ()
       (cond
        ((null entropy/emacs-themes--theme-init-setup-for-daemon-done)
         (with-selected-frame (selected-frame)
           (message "Daemon init theme ...")
           (entropy/emacs-themes-init-setup-user-theme)
           (setq entropy/emacs-themes--theme-init-setup-for-daemon-done
                 'first-init
                 ;; -----------------------
                 entropy/emacs-themes--daemon-theme-reload-type
                 (display-graphic-p))))
        (t
         (cond
          ((frame-parameter nil 'eemacs-current-frame-is-daemon-created)
           ;; Reload theme since a theme is present differently from
           ;; gui to tui and vice versa generally.
           (if
               (not
                ;; prevent reload theme for same status as previous client created is
                (eq (display-graphic-p)
                    entropy/emacs-themes--daemon-theme-reload-type))
               (when (= 1 (length entropy/emacs-daemon--legal-clients))
                 (message "Daemon reload theme ...")
                 (entropy/emacs-themes-strictly-load-theme
                  entropy/emacs-theme-sticker 'non-confirm)
                 (setq entropy/emacs-themes--daemon-theme-reload-type
                       (display-graphic-p)))
             ;; In other words, we need to reset the font spec for new
             ;; created daemon frame even for the same display type
             ;; since the new frame didn't inherit the previous one's
             ;; font spec.
             (when (bound-and-true-p entropy/emacs-font-setting-enable)
               (message "Daemon enable eemacs font spec ...")
               (entropy/emacs-font-set-setfont-core))))
          (t
           nil)))))

     (add-hook 'entropy/emacs-daemon-server-after-make-frame-hook
               #'entropy/emacs-themes--load-theme-for-daemon-client-new)
     )))

;; * provide
(provide 'entropy-emacs-themes)
