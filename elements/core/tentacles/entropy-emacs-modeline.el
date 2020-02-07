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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defface)

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
               'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived
             'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main))
          ((not (eq (selected-window) entropy/emacs-modeline--mdl-egroup-selected-window))
           (if derived
               'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived_inactive
             'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main_inactive)))))

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
  :preface
  (defvar entropy/emacs-modeline--powerline-spec-done nil)
  :commands (powerline-default-theme))

;; ****** spaceline
(defvar entropy/emacs-modeline--spaceline-spec-done nil)
(defvar entropy/emacs-modeline--spaceline-spec-list
  '())

(defun entropy/emacs-modeline--spaceline-specification ()
  ;; powerline specification
  (require 'powerline)
  (setq entropy/emacs-modeline--spaceline-spec-list nil)
  (push (cons 'powerline-default-separator
              powerline-default-separator)
        entropy/emacs-modeline--spaceline-spec-list)

  (push (cons 'powerline-image-apple-rgb
              powerline-image-apple-rgb)
        entropy/emacs-modeline--spaceline-spec-list)

  ;; self specification
  (setq powerline-default-separator (if window-system 'arrow 'utf-8))
  (setq powerline-image-apple-rgb sys/mac-x-p)
  (add-hook 'spaceline-pre-hook #'powerline-reset) ; For changing themes
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

(defun entropy/emacs-modeline--spaceline-spec-clean ()
  (dolist (el entropy/emacs-modeline--spaceline-spec-list)
    (set (car el) (cdr el))))

(if (eq entropy/emacs-use-extensions-type 'submodules)
    (use-package spaceline
      :init
      (use-package spaceline-config
        :commands (spaceline-spacemacs-theme)
        :defines spaceline-pre-hook
        :config
        (unless entropy/emacs-modeline--spaceline-spec-done
          (entropy/emacs-modeline--spaceline-specification))))
  (use-package spaceline
    :defines spaceline-pre-hook
    :config
    (unless entropy/emacs-modeline--spaceline-spec-done
      (entropy/emacs-modeline--spaceline-specification))))

;; ****** spaceline icons
(use-package spaceline-all-the-icons
  :commands (spaceline-all-the-icons-theme)
  :preface

  (defvar entropy/emacs-modeline--spaceline-icons-spec-done nil)

  (defvar entropy/emacs-modeline--spaceline-icons-spec-list '())

  (defun entropy/emacs-modeline--spaceline-icons-sepc-clean ()
    (dolist (el entropy/emacs-modeline--spaceline-icons-spec-list)
      (set (car el) (cdr el))))

  (defun entropy/emacs-modeline--spaceline-icons-specification ()
    ;; powerline specification
    (require 'powerline)
    (setq entropy/emacs-modeline--spaceline-icons-spec-list nil)
    (push (cons 'powerline-text-scale-factor powerline-text-scale-factor)
          entropy/emacs-modeline--spaceline-icons-spec-list)
    (setq powerline-text-scale-factor 1.2)    

    ;; self specification
    (setq spaceline-all-the-icons-projectile-p nil)
    (setq spaceline-all-the-icons-separator-type 'none)
    (setq spaceline-all-the-icons-icon-set-eyebrowse-slot 'square))
  
  :config
  (unless entropy/emacs-modeline--spaceline-icons-spec-done
    (entropy/emacs-modeline--spaceline-icons-specification))
  
  (defun entropy/emacs-modeline--spaceline-eyebrowse-segments-advice (orig-func &rest orig-args)
    "Ignore icon parse for float wind-num type used for
entropy-emacs' derived eyebrowse window configuration. "
    (let ((slot-number (car orig-args)))
      (or (ignore-errors (apply orig-func orig-args))
          (number-to-string slot-number))))
  (advice-add 'spaceline-all-the-icons--window-number-icon
              :around
              #'entropy/emacs-modeline--spaceline-eyebrowse-segments-advice))

;; ***** origin type
(defvar entropy/emacs-modeline--origin-spec-done nil)

(defun entropy/emacs-mode-line-origin-theme ()
  (setq-default mode-line-format
                '("%e"
                  ;; mode-line-front-space
                  (:eval (when (bound-and-true-p eyebrowse-mode)
                           (entropy/emacs-modeline--mdl-egroup)))
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
  :preface

  (defvar entropy/emacs-modeline--doom-modeline-spec-done nil)
  
  (defun entropy/emacs-modeline--doom-modeline-specification ()
    (setq doom-modeline-height 10
          doom-modeline-bar-width 1
          doom-modeline-buffer-file-name-style 'truncate-all
          doom-modeline-major-mode-color-icon t
          doom-modeline-icon
          (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))))
  :init
  (setq doom-modeline-unicode-fallback t)

  :config
  (unless entropy/emacs-modeline--doom-modeline-spec-done
    (entropy/emacs-modeline--doom-modeline-specification))
  
  (defun entropy/emacs-modeline--dml-file-icon-around-advice (orig-func &rest orig-args)
    (apply orig-func orig-args)
    (when (equal (buffer-name) entropy/emacs-dashboard-buffer-name)
      (setq doom-modeline--buffer-file-icon (all-the-icons-octicon "eye" :height 0.8 :v-adjust 0.1))))
  (advice-add 'doom-modeline-update-buffer-file-icon
              :around
              #'entropy/emacs-modeline--dml-file-icon-around-advice)


  (defun doom-modeline-icon (icon-set icon-name unicode text face &rest args)
    "Display icon of ICON-NAME with FACE and ARGS in mode-line.

  ICON-SET includes `octicon', `faicon', `material', `alltheicons'
  and `fileicon'.  UNICODE is the unicode char fallback. TEXT is the
  ASCII char fallback.

  Notice:

  This function has been patched for be compatible with CLI
  interaction."
    (let ((face (or face 'mode-line)))
      (or (when (and icon-name (not (string-empty-p icon-name)) (display-graphic-p))
            (pcase icon-set
              ('octicon
               (apply #'all-the-icons-octicon icon-name :face face args))
              ('faicon
               (apply #'all-the-icons-faicon icon-name :face face args))
              ('material
               (apply #'all-the-icons-material icon-name :face face args))
              ('alltheicon
               (apply #'all-the-icons-alltheicon icon-name :face face args))
              ('fileicon
               (apply #'all-the-icons-fileicon icon-name :face face args))))
          (when (and doom-modeline-unicode-fallback
                     unicode
                     (not (string-empty-p unicode))
                     (char-displayable-p (string-to-char unicode)))
            (propertize unicode 'face face))
          (when text (propertize text 'face face)))))
  
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
              "vertical_align_center" "↕" "><" 'doom-modeline-warning))
           (when buffer-read-only
             (doom-modeline-buffer-file-state-icon
              "lock" "🔒" "%1*" 'doom-modeline-warning))
           (when (buffer-modified-p)
             (doom-modeline-buffer-file-state-icon
              "save" "💾" "%1*" 'doom-modeline-buffer-modified))
           (when (and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
             (doom-modeline-buffer-file-state-icon
              "do_not_disturb_alt" "🚫" "!" 'doom-modeline-urgent))
           "")))

  ;; narrow region adviced by doom-modeline icon dynamic features
  (advice-add #'narrow-to-defun :after #'doom-modeline-update-buffer-file-state-icon)
  (advice-add #'narrow-to-page :after #'doom-modeline-update-buffer-file-state-icon)
  
  (doom-modeline-def-segment company-indicator
    "Company mode backends indicator."
    (let ((company-lighter-base
           (propertize "✪ company"
                       'face
                       (if (doom-modeline--active)
                           'mode-line-emphasis
                         'mode-line-inactive)))
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
                            'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived)
                           (t 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived_inactive))
                        (cond ((doom-modeline--active) 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main)
                              (t 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main_inactive)))))
      ""))

  (doom-modeline-def-modeline 'main
   '(bar workspace-number window-number
         matches buffer-info remote-host buffer-position parrot
         " " company-indicator selection-info)
   '(misc-info lsp irc mu4e github debug minor-modes
               input-method buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'project
    '(bar workspace-number window-number buffer-default-directory)
    '(misc-info mu4e github debug major-mode process))

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
  (let (cancel-branch)
    (cond
     ;; init spaceline
     ((string= entropy/emacs-modeline-style "spaceline")
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
      (setq entropy/emacs-modeline-style "origin"
            cancel-branch t)
      (entropy/emacs-modeline--mdl-init))

     ;; init powerline
     ((string= entropy/emacs-modeline-style "powerline")
      (setq entropy/emacs-modeline--mdl-init-caller
            '(powerline-default-theme)))

     ;; init-origin style
     ((string= entropy/emacs-modeline-style "origin")
      (setq entropy/emacs-modeline--mdl-init-caller
            '(entropy/emacs-mode-line-origin-theme)))

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
      (setq entropy/emacs-modeline-style "origin"
            cancel-branch t)
      (entropy/emacs-modeline--mdl-init))

     ;; any other type was unsupport
     (t (warn (format "entropy/emacs-modeline-style's value '%s' is invalid." entropy/emacs-modeline-style))
        (setq entropy/emacs-modeline-style "origin"
              cancel-branch t)
        (entropy/emacs-modeline--mdl-init)))
    (setq entropy/emacs-mode-line-sticker entropy/emacs-modeline-style)
    (unless cancel-branch
      (funcall `(lambda () ,entropy/emacs-modeline--mdl-init-caller)))))

(entropy/emacs-lazy-with-load-trail
 eemacs-modeline-init
 (redisplay t)
 (entropy/emacs-modeline--mdl-init)
 (redisplay t))

;; ** toggle function
(when entropy/emacs-enable-modeline-toggle
  
  (defun entropy/emacs-modeline--mdl-tidy-spec ()
    (pcase entropy/emacs-mode-line-sticker
      ("spaceline"
       (entropy/emacs-modeline--spaceline-spec-clean))
      ("spaceline-all-the-icons"
       (entropy/emacs-modeline--spaceline-icons-sepc-clean))
      ("doom" (doom-modeline-mode 0))
      (_ nil)))

  (defmacro entropy/emacs-modeline--define-toggle (name spec-form init-var enable-form &rest body)
    `(defun ,(intern (concat "entropy/emacs-modeline-mdl-" name "-toggle")) ()
       (interactive)
       (entropy/emacs-modeline--mdl-tidy-spec)
       (setq entropy/emacs-mode-line-sticker ,name)
       ,spec-form
       (unwind-protect
           (progn
             (setq ,init-var t)
             ,@body
             ,enable-form)
         (progn (setq ,init-var nil)))))
  
  (entropy/emacs-modeline--define-toggle
   "spaceline"
   (entropy/emacs-modeline--spaceline-specification)
   entropy/emacs-modeline--spaceline-spec-done
   (spaceline-spacemacs-theme))

  (entropy/emacs-modeline--define-toggle
   "spaceline-all-the-icons"
   (entropy/emacs-modeline--spaceline-icons-specification)
   entropy/emacs-modeline--spaceline-icons-spec-done
   (spaceline-all-the-icons-theme)
   (when (version= "25.3.1" emacs-version)
     (error "You are in emacs veresion 25.3.1 and couldn't use
  spaceline-all-the-icons because this version can not shown
  all-the-icons-fonts correnctly.")))

  (entropy/emacs-modeline--define-toggle
   "powerline"
   nil entropy/emacs-modeline--powerline-spec-done
   (powerline-default-theme))

  (entropy/emacs-modeline--define-toggle
   "doom"
   (entropy/emacs-modeline--doom-modeline-specification)
   entropy/emacs-modeline--doom-modeline-spec-done
   (doom-modeline-mode +1))

  (entropy/emacs-modeline--define-toggle
   "origin" nil entropy/emacs-modeline--origin-spec-done
   (entropy/emacs-mode-line-origin-theme)))

;; ** modeline-hide feature
(use-package hide-mode-line
  :commands (hide-mode-line-mode)
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; * provide
(provide 'entropy-emacs-modeline)
