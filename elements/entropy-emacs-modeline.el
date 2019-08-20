;;; entropy-emacs-modeline.el --- entropy-emacs mode-line format configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-modeline.el
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
;; `entropy-emacs' has its own specfic modeline style with bounds of
;; hacked third-party ones, the default was
;; `entropy/emacs-mode-line-origin-theme', with subs of `powerline'
;; and `spaceline' family even for =doom-modeline=.
;;
;; Sets of modeline style switching function named prefixed as
;; =entropy/emacs-mdl-*=, thus whatever modeline style startup
;; specific by customized `entropy/emacs-modeline-style', toggled for
;; that usage.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hackign warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-faces)

;; ** init setting
;;
;;     Now using three modeline style be the option for custom's choice:
;;     - spaceline
;;     - powerline
;;     - origin
;;
;;     suggestion using origin one which has the good machanism
;;     performance enhanced with emacs run time.  And if you want to
;;     using modern one , you can choose spaceline which gives you the
;;     similar sensitive as spacemacs.

;; *** eyebrowse adapting
(defvar entropy/emacs-modeline--mdl-egroup-selected-window (frame-selected-window))
(defun entropy/emacs-modeline--mdl-egroup-set-selected-window (&rest _)
  "Set `entropy/emacs-modeline--mdl-egroup' selected window indicator."
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq entropy/emacs-modeline--mdl-egroup-selected-window win)
      (force-mode-line-update))))


(defun entropy/emacs-modeline--mdl-egroup-unset-selected-window ()
  "Unset `entropy/emacs-modeline--mdl-egroup' appropriately."
  (setq entropy/emacs-modeline--mdl-egroup-selected-window nil)
  (force-mode-line-update))


(add-hook 'window-configuration-change-hook #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
(advice-add #'handle-switch-frame :after #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
(advice-add #'select-window :after #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
(with-no-warnings
  (if (not (boundp 'after-focus-change-function))
      (progn
        (add-hook 'focus-in-hook  #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
        (add-hook 'focus-out-hook #'entropy/emacs-modeline--mdl-egroup-unset-selected-window))
    (defun entropy/emacs-modeline--mdl-egroup-refresh-frame ()
      (setq entropy/emacs-modeline--mdl-egroup-selected-window nil)
      (cl-loop for frame in (frame-list)
               if (eq (frame-focus-state frame) t)
               return (setq entropy/emacs-modeline--mdl-egroup-selected-window (frame-selected-window frame)))
      (force-mode-line-update))
    (add-function :after after-focus-change-function #'entropy/emacs-modeline--mdl-egroup-refresh-frame)))


(defun entropy/emacs-modeline--mdl-egroup-face-dynamic (tag)
  (let* ((derived (if (string-match-p "\\.[[:digit:]]" tag) t nil)))
    (cond ((eq (selected-window) entropy/emacs-modeline--mdl-egroup-selected-window)
           (if derived
               'entropy/emacs-faces--modeline-mdl-eyebrowse-face-derived
             'entropy/emacs-faces--modeline-mdl-eyebrowse-face-main))
          ((not (eq (selected-window) entropy/emacs-modeline--mdl-egroup-selected-window))
           (if derived
               'entropy/emacs-faces--modeline-mdl-eyebrowse-face-derived_inactive
             'entropy/emacs-faces--modeline-mdl-eyebrowse-face-main_inactive)))))

(defun entropy/emacs-modeline--mdl-egroup ()
  "Entropy-emacs specific modeline style.

This customization mainly adding the eyebrowse slot and tagging name show function."
  (let* ((cs (eyebrowse--get 'current-slot))
         (window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc cs window-configs))
         (current-tag (nth 2 window-config))
         (mdlface (entropy/emacs-modeline--mdl-egroup-face-dynamic (number-to-string cs)))
         rtn)
    (setq rtn (concat
               ;; (propertize "<<" 'face 'font-lock-type-face)
               (propertize (concat " " (number-to-string cs) ":") 'face mdlface)
               (propertize (concat current-tag " ") 'face mdlface) " "
               ;; (propertize ">> " 'face 'font-lock-type-face)
               ))
    rtn))

;; *** init procedure
;; **** pre required

;; ***** powerline group
;; ****** powerline
(use-package powerline
  :commands (powerline-default-theme))
;; ****** spaceline
(if (eq entropy/emacs-use-extensions-type 'submodules)
    (use-package spaceline
      :init
      (use-package spaceline-config
        :commands (spaceline-spacemacs-theme)
        :config
        (setq spaceline-pre-hook #'powerline-reset) ; For changing themes
        (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)))
  (use-package spaceline
    :config
    (setq spaceline-pre-hook #'powerline-reset) ; For changing themes
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)))

;; ****** spaceline icons
(use-package spaceline-all-the-icons
  :commands (spaceline-all-the-icons-theme)
  :config
  (setq spaceline-all-the-icons-projectile-p nil)
  (setq spaceline-all-the-icons-separator-type 'none)
  (setq powerline-text-scale-factor 1.2)
  (setq spaceline-all-the-icons-icon-set-eyebrowse-slot 'square))

;; ***** origin type
(defun entropy/emacs-mode-line-origin-theme ()
  (setq-default mode-line-format
                '("%e"
                  ;; mode-line-front-space
                  (:eval (ignore-errors (entropy/emacs-modeline--mdl-egroup)))
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified "  "
                  ;;"remote:" mode-line-remote " "
                  ;; mode-line-frame-identification
                  mode-line-modes
                  (vc-mode vc-mode) " "
                  "<" mode-line-buffer-identification "> "
                  "[["mode-line-position"]]"
                  mode-line-misc-info
                  mode-line-end-spaces)))




;; ***** doom modeline
(use-package doom-modeline
  :ensure nil
  :commands (doom-modeline-mode
             doom-modeline-refresh-bars)
  :init
  (defun entropy/emacs-doom-mdlini-after-advice (&rest _rest)
    "Advice for doom-modeline-mode."
    (setq doom-modeline-buffer-file-name-style 'truncate-all)
    (doom-modeline-refresh-bars)
    ;; theme adapted
    (if (string-match-p "spolsky" (symbol-name entropy/emacs-theme-options))
        (setq doom-modeline--bar-active
              (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                       doom-modeline-bar-width
                                       doom-modeline-height))))
  (entropy/emacs-lazy-load-simple 'doom-modeline
    (advice-add 'doom-modeline-mode :after #'entropy/emacs-doom-mdlini-after-advice))
  
  :config
  
  (setq doom-modeline-height 10
        doom-modeline-bar-width 1
        doom-modeline-buffer-file-name-style 'truncate-all
        doom-modeline-major-mode-color-icon t
        doom-modeline-icon (or (display-graphic-p) entropy/emacs-custom-pdumper-do))  

  (defun entropy/emacs-modeline--dml-file-icon-around-advice (orig-func &rest orig-args)
    (apply orig-func orig-args)
    (when (equal (buffer-name) entropy/emacs-dashboard-buffer-name)
      (setq doom-modeline--buffer-file-icon (all-the-icons-octicon "eye" :height 0.8 :v-adjust 0.1))))
  (advice-add 'doom-modeline-update-buffer-file-icon
              :around
              #'entropy/emacs-modeline--dml-file-icon-around-advice)
  
  (defun doom-modeline-update-buffer-file-state-icon (&rest _)
    "Update the buffer or file state in mode-line.

Note:

This function has been modified for adapting for entropy-emacs.

entropy-emacs using all file state icos show side by side. "
    (setq doom-modeline--buffer-file-state-icon
          (concat
           (when (or (buffer-narrowed-p)
                     (and (fboundp 'fancy-narrow-active-p)
                          (fancy-narrow-active-p)))
             (doom-modeline-buffer-file-state-icon
              "vertical_align_center"
              "><"
              'doom-modeline-warning))
           (when buffer-read-only
             (doom-modeline-buffer-file-state-icon
              "lock"
              "%1*"
              'doom-modeline-warning))
           (when (buffer-modified-p)
             (doom-modeline-buffer-file-state-icon
              "save"
              "%1*"
              'doom-modeline-buffer-modified))
           (when (and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
             (doom-modeline-buffer-file-state-icon
              "do_not_disturb_alt"
              "!"
              'doom-modeline-urgent))
           "")))

  ;; narrow region adviced by doom-modeline icon dynamic features
  (advice-add #'narrow-to-defun :after #'doom-modeline-update-buffer-file-state-icon)
  (advice-add #'narrow-to-page :after #'doom-modeline-update-buffer-file-state-icon)
  
  (doom-modeline-def-segment company-indicator
    "Company mode backends indicator."
    (let ((company-lighter-base
           (propertize "✪ company" 'face 'mode-line-emphasis))
          (company-backend-abbrev-indicatior
           (lambda (x_str)
             (format "%s%s%s" 
                     (propertize "✚ ❮" 'face 'mode-line-emphasis)
                     (propertize x_str 'face 'diary)
                     (propertize "❯" 'face 'mode-line-emphasis))))
          (cur_info nil))
      (if (not (bound-and-true-p company-mode))
          ""
        (setq cur_info
              (if (consp company-backend)
                  (company--group-lighter (nth company-selection
                                               company-candidates)
                                          "company")
                (symbol-name company-backend)))
        (cond ((equal cur_info "nil")
               company-lighter-base)
              (t
               (funcall company-backend-abbrev-indicatior
                        cur_info))))))

  (doom-modeline-def-segment workspace-number
    "The current workspace name or number. Requires
`eyebrowse-mode' to be enabled.

Note:

This doom-modeline segment modified for adapting for
entropy-emacs.

entropy-emacs modified it for adapting for entropy-egroup of
eyerbowse improvement."
    (if (and (bound-and-true-p eyebrowse-mode)
             (< 0 (length (eyebrowse--get 'window-configs))))
        (let* ((num (eyebrowse--get 'current-slot))
               (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
               (str (if (and tag (< 0 (length tag)))
                        (concat (int-to-string num) ":" tag)
                      (when num (int-to-string num)))))
          (propertize (format " %s " str) 'face
                      (if (string-match-p "\\.[[:digit:]]" str)
                          (cond
                           ((doom-modeline--active)
                            'entropy/emacs-faces--modeline-mdl-eyebrowse-face-derived)
                           (t 'entropy/emacs-faces--modeline-mdl-eyebrowse-face-derived_inactive))
                        (cond ((doom-modeline--active) 'entropy/emacs-faces--modeline-mdl-eyebrowse-face-main)
                              (t 'entropy/emacs-faces--modeline-mdl-eyebrowse-face-main_inactive)))))
      ""))

  (doom-modeline-def-modeline 'main
   '(bar workspace-number window-number
         matches buffer-info remote-host buffer-position parrot
         " " company-indicator selection-info)
   '(misc-info lsp irc mu4e github debug fancy-battery minor-modes
               input-method buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'project
    '(bar workspace-number window-number buffer-default-directory)
    '(misc-info fancy-battery mu4e github debug major-mode process))

  ;; timer for after-change-function
  (defvar entropy/emacs-modeline--dml-timer nil)
  (defun entropy/emacs-modeline--dml-afc-monitior ()
    "Timer for moitor the doom-modeline feature for hook
`after-change-functions'.

Since `after-change-functions' was weak when any error occured
with emacs, see its doc-string for details."
    (let ((candis '(doom-modeline-update-buffer-file-state-icon
                    doom-modeline-update-buffer-file-name)))
      (dolist (el candis)
        (unless (member el after-change-functions)
          (add-hook 'after-change-functions el)))))

  (setq entropy/emacs-modeline--dml-timer
        (run-with-idle-timer 1 t #'entropy/emacs-modeline--dml-afc-monitior)))

;; **** load conditions
(defvar entropy/emacs-modeline--mdl-init-caller nil
  "The form for enable modeline which obtained by
`entropy/emacs-modeline--mdl-init'.")

(defun entropy/emacs-modeline--mdl-init ()
  "Init modeline style specified by `entropy/emacs-modeline-style'.

  If the specific modeline style is not compat with current emacs
  version, then warning with reset modeline style to \"origin\"
  style which defined in `entropy/emacs-modeline-style'."
  (cond
   ;; init spaceline
   ((string= entropy/emacs-modeline-style "spaceline")
    (setq powerline-default-separator (if window-system 'arrow 'utf-8))
    (setq powerline-image-apple-rgb sys/mac-x-p)
    (setq entropy/emacs-modeline--mdl-init-caller
          '(spaceline-spacemacs-theme)))

   ;; init sapceline-icons
   ((and (string= entropy/emacs-modeline-style "spaceline-icons")
         (not (string= emacs-version "25.3.1")))
    (setq entropy/emacs-modeline--mdl-init-caller
          '(spaceline-all-the-icons-theme)))

   ((and (string= entropy/emacs-modeline-style "spaceline-icons")
         (string= emacs-version "25.3.1"))
    (warn "You are in emacs veresion 25.3.1 and couldn't use
  spaceline-all-the-icons because this version can not shown
  all-the-icons-fonts correnctly.")
    (setq entropy/emacs-modeline-style "origin")
    (entropy/emacs-modeline--mdl-init))

   ;; init powerline
   ((string= entropy/emacs-modeline-style "powerline")
    (setq entropy/emacs-modeline--mdl-init-caller
          '(powerline-default-theme)))

   ;; init-origin style
   ((string= entropy/emacs-modeline-style "origin")
    (entropy/emacs-lazy-load-simple 'eyebrowse
      (setq entropy/emacs-modeline--mdl-init-caller
            '(entropy/emacs-mode-line-origin-theme))))

   ;; init doom-modeline
   ((and (string= entropy/emacs-modeline-style "doom")
         (not (version= emacs-version "25.3.1")))
    (setq entropy/emacs-modeline--mdl-init-caller
          '(doom-modeline-mode 1)))

   ;; if detective init doom-modline in emacs-25.3.1 then warning.
   ((and (string= entropy/emacs-modeline-style "doom")
         (version= emacs-version "25.3.1"))
    (warn "You are in emacs veresion 25.3.1 and couldn't use
  doom-modeline because this version can not shown
  all-the-icons-fonts correnctly.")
    (setq entropy/emacs-modeline-style "origin")
    (entropy/emacs-modeline--mdl-init))

   ;; any other type was unsupport
   (t (warn (format "entropy/emacs-modeline-style's value '%s' is invalid." entropy/emacs-modeline-style))
      (setq entropy/emacs-modeline-style "origin")
      (entropy/emacs-modeline--mdl-init)))
  (setq entropy/emacs-mode-line-sticker entropy/emacs-modeline-style))

(entropy/emacs-modeline--mdl-init)

(let ((caller `(lambda ()
                 ,entropy/emacs-modeline--mdl-init-caller)))
  (cond
   (entropy/emacs-custom-pdumper-do
    (add-hook 'entropy/emacs-pdumper-load-hook
              caller))
   (t
    (funcall caller))))


;; ** toggle function
(when entropy/emacs-enable-modeline-toggle
  (defun entropy/emacs-modeline-mdl-spaceline ()
    "Toggle modeline style to spacelien."
    (interactive)
    (setq entropy/emacs-mode-line-sticker "spaceline")
    (setq powerline-default-separator (if window-system 'arrow 'utf-8))
    (setq powerline-image-apple-rgb sys/mac-x-p)
    (spaceline-spacemacs-theme))


  (defun entropy/emacs-modeline-mdl-spaceline-all-the-icons ()
    "Toggle modeline style to spaceline-all-the-icons"
    (interactive)
    (if (version= "25.3.1" emacs-version)
        (error "You are in emacs veresion 25.3.1 and couldn't use
  spaceline-all-the-icons because this version can not shown
  all-the-icons-fonts correnctly."))
    (spaceline-all-the-icons-theme))
  
  (defun entropy/emacs-modeline-mdl-powerline ()
    "Toggle modeline style to powerline."
    (interactive)
    (setq entropy/emacs-mode-line-sticker "powerline")
    (powerline-default-theme))

  (entropy/emacs-lazy-load-simple 'eyebrowse
    (defun entropy/emacs-modeline-mdl-origin ()
      "Toggle modeline style to entropy specific origin modeline style."
      (interactive)
      (setq entropy/emacs-mode-line-sticker "origin")
      (entropy/emacs-mode-line-origin-theme)))

  (defun entropy/emacs-modeline-mdl-doom ()
    "Toggle doom-modeline."
    (interactive)
    (setq entropy/emacs-mode-line-sticker "doom")
    (doom-modeline-mode 1)))

;; ** modeline-hide feature
(use-package hide-mode-line
  :commands (hide-mode-line-mode)
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; * provide
(provide 'entropy-emacs-modeline)
