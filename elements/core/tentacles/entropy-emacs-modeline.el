;;; entropy-emacs-modeline.el --- entropy-emacs mode-line format configuration  -*- lexical-binding: t; -*-
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

(defvar company-backend)
(defvar company-selection)
(defvar company-candidates)

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

;; *** internal mode-line specifiations
;; Show the column numberic in modeline
(setq column-number-mode t)

;; Show time on mode line
(when entropy/emacs-display-time-modeline
  (use-package time
    :ensure nil
    :init
    (entropy/emacs-lazy-with-load-trail
     display-time-mode-init
     (setq-default display-time-interval 1)
     ;; display time with date and real time infos
     (setq display-time-day-and-date t)
     ;; 24hr format
     (setq display-time-24hr-format t)
     (setq display-time-format " %e %b %Y %H:%M:%S ")
     (display-time-mode 1))))

;; A minor-mode menu for the mode line
(use-package minions
  :commands (minions-minor-modes-menu
             minions-mode)
  :init
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)
  )

;; *** eemaac-specification

;; **** core lib
(defvar entropy/emacs-modeline--subr-var->current-selected-window
  (frame-selected-window))

(defun entropy/emacs-modeline--subr-func->get-current-window ()
  ;; Get the current window but should exclude on the child frame.
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defun entropy/emacs-modeline--subr-func->set-selected-window
    (&optional orig-func &rest orig-args)
  (let ((rtn (when (and orig-func
                        (functionp orig-func))
               (apply orig-func orig-args)))
        (win
         (entropy/emacs-modeline--subr-func->get-current-window)))
    (unless (minibuffer-window-active-p win)
      (setq entropy/emacs-modeline--subr-var->current-selected-window win))
    rtn))

(defun entropy/emacs-modeline--subr-func->unset-selected-window ()
  (setq entropy/emacs-modeline--subr-var->current-selected-window nil))

(defun entropy/emacs-modeline--subr-func->judge-current-window-focus-on-p ()
  (and entropy/emacs-modeline--subr-var->current-selected-window
       (eq (entropy/emacs-modeline--subr-func->get-current-window)
           entropy/emacs-modeline--subr-var->current-selected-window)))

;; hooks for set selected window var
(add-hook 'window-configuration-change-hook
          #'entropy/emacs-modeline--subr-func->set-selected-window)
(advice-add #'handle-switch-frame
            :around
            #'entropy/emacs-modeline--subr-func->set-selected-window)
(advice-add #'select-window
            :around
            #'entropy/emacs-modeline--subr-func->set-selected-window)

(defun entropy/emacs-modeline--subr-func/->refresh-frame ()
  (setq entropy/emacs-modeline--subr-var->current-selected-window nil)
  (cl-loop for frame in (frame-list)
           if (and (eq (frame-focus-state frame) t)
                   ;; not a child frame
                   (not (frame-parent frame)))
           return (entropy/emacs-modeline--subr-func->set-selected-window)))
(add-function :after after-focus-change-function
              #'entropy/emacs-modeline--subr-func/->refresh-frame)

;; **** advices after swither

(defun entropy/emacs-modeline--set-mdlfmt-after-advice (&rest _)
  "After advice for each mode line type switcher."
  (let ((cur-mdl-fmt (default-value 'mode-line-format)))
    ;; Manually set the `mode-line-format' in some buffer since they
    ;; doesn't changed automatically after the mode line type switcher.
    (dolist (bname '("*scratch*" "*Messages*"))
      (if (buffer-live-p (get-buffer bname))
          (with-current-buffer bname
            (setq mode-line-format cur-mdl-fmt))))))

;; **** common eemacs spec eyebrowse segment
(defun entropy/emacs-modeline--mdl-common-eyebrowse-face-dynamic-gen (tag)
  (let* ((derived (if (string-match-p "\\.[[:digit:]]" tag) t nil)))
    (cond ((entropy/emacs-modeline--subr-func->judge-current-window-focus-on-p)
           (if derived
               'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived
             'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main))
          (t
           (if derived
               'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived_inactive
             'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main_inactive)))))

(defvar entropy/emacs-modeline--mdl-common-eyebrowse-segment nil)
(defun entropy/emacs-modeline--mdl-common-eyebrowse-segment ()
  "Entropy-emacs specific modeline style.

This customization mainly adding the eyebrowse slot and tagging name show function."
  (if (or (null entropy/emacs-modeline--mdl-common-eyebrowse-segment)
          (bound-and-true-p entropy/emacs-current-session-is-idle-p))
      (let* ((cs (eyebrowse--get 'current-slot))
             (window-configs (eyebrowse--get 'window-configs))
             (window-config (assoc cs window-configs))
             (current-tag (nth 2 window-config))
             (mdlface (entropy/emacs-modeline--mdl-common-eyebrowse-face-dynamic-gen
                       (number-to-string cs)))
             rtn)
        (setq rtn (concat
                   (propertize (concat " " (number-to-string cs) ":") 'face mdlface)
                   (propertize (concat current-tag " ") 'face mdlface)
                   " "))
        (setq entropy/emacs-modeline--mdl-common-eyebrowse-segment
              rtn))
    entropy/emacs-modeline--mdl-common-eyebrowse-segment))


;; *** modeline type defined
;; **** powerline group
;; ***** powerline
(entropy/emacs-usepackage-with-permanently-defer powerline
  :preface
  (defvar entropy/emacs-modeline--powerline-spec-done nil)
  (defvar entropy/emacs-modeline--powerline-enable-done nil)
  (defun entropy/emacs-modeline--powerline-spec-clean ()
    (entropy/emacs-modeline-restore-default-mdlfmt)
    (setq entropy/emacs-modeline--powerline-enable-done nil
          entropy/emacs-modeline--powerline-spec-done nil))

  (defun entropy/emacs-modeline-do-powerline-set ()
    (interactive)
    (require 'powerline)
    (powerline-default-theme)
    (let* ((cur-mdl-fmt (default-value 'mode-line-format))
           (powerline-spec (cadr cur-mdl-fmt)))
      (setq-default
       mode-line-format
       (list
        "%e"
        '(:eval
          (when (bound-and-true-p eyebrowse-mode)
            (entropy/emacs-modeline--mdl-common-eyebrowse-segment)))
        powerline-spec))))

  :commands (powerline-default-theme)
  :config
  )

;; ***** spaceline
(defvar entropy/emacs-modeline--spaceline-spec-done nil)
(defvar entropy/emacs-modeline--spaceline-enable-done nil)
(defvar entropy/emacs-modeline--spaceline-spec-list
  '())

(defvar spaceline-highlight-face-func)
(defvar spaceline-workspace-numbers-unicode)
(defun entropy/emacs-modeline--spaceline-specification ()
  ;; powerline specification
  (require 'powerline)
  (setq entropy/emacs-modeline--spaceline-spec-list nil)

  ;; backup the origin powerline configs where spaceline will modify
  ;; for.
  (push (cons 'powerline-default-separator
              powerline-default-separator)
        entropy/emacs-modeline--spaceline-spec-list)
  (push (cons 'powerline-image-apple-rgb
              powerline-image-apple-rgb)
        entropy/emacs-modeline--spaceline-spec-list)

  ;; spaceline specification for powerline config
  (setq powerline-default-separator (if window-system 'arrow 'utf-8))
  (setq powerline-image-apple-rgb sys/mac-x-p)

  ;; spaceline internal setup
  (add-hook 'spaceline-pre-hook #'powerline-reset) ; For changing themes
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

(defun entropy/emacs-modeline--spaceline-spec-clean ()
  ;; restore the origin powerline configs where spaceline has modified
  ;; for.
  (dolist (el entropy/emacs-modeline--spaceline-spec-list)
    (set (car el) (cdr el)))
  (entropy/emacs-modeline-restore-default-mdlfmt)
  (setq entropy/emacs-modeline--spaceline-enable-done nil
        entropy/emacs-modeline--spaceline-spec-done nil))

(defun entropy/emacs-modeline--spaceline-defsegment-for-workspace ()
  (spaceline-define-segment workspace-number
    "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
    (when (and (bound-and-true-p eyebrowse-mode)
               (<= 1 (length (eyebrowse--get 'window-configs))))
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      (if num
                          (concat (int-to-string num) ":" tag)
                        tag)
                    (when num (int-to-string num)))))
        (or (when spaceline-workspace-numbers-unicode
              (spaceline--unicode-number str))
            (propertize str 'face 'bold))))))

(entropy/emacs-usepackage-with-permanently-defer spaceline-config
  :ensure nil
  :commands (spaceline-spacemacs-theme))

(entropy/emacs-usepackage-with-permanently-defer spaceline
  :init
  (entropy/emacs-lazy-load-simple 'spaceline-segments
    :always-lazy-load t
    (entropy/emacs-modeline--spaceline-defsegment-for-workspace)))

;; **** origin type

(defvar entropy/emacs-modeline--origin-spec-done nil)
(defvar entropy/emacs-modeline--origin-enable-done nil)

(defun entropy/emacs-modeline--origin-spec-clean ()
  (entropy/emacs-modeline-restore-default-mdlfmt)
  (setq entropy/emacs-modeline--origin-spec-done nil
        entropy/emacs-modeline--origin-enable-done nil))

(defvar entropy/emac-modeline--origin-mdl-buffer-identification
  (propertized-buffer-identification "%6b")
  "Mode line construct for identifying the buffer being displayed.
Its default value is (\"%6b\") with some text properties added.
Major modes that edit things other than ordinary files may change this
\(e.g. Info, Dired,...)")
(put 'entropy/emac-modeline--origin-mdl-buffer-identification 'risky-local-variable t)
(make-variable-buffer-local 'entropy/emac-modeline--origin-mdl-buffer-identification)

(defun entropy/emacs-modeline--origin-mdl-propertize-face (str face)
  "Use `propertize' to decorate the STR with face FACE but
inactive when
`entropy/emacs-modeline--subr-func->judge-current-window-focus-on-p'
return nil"
  (propertize str 'face
              (if (entropy/emacs-modeline--subr-func->judge-current-window-focus-on-p)
                  face
                'mode-line-inactive)))

(defun entropy/emacs-modeline--origin-mdl-use-icon-or-plain
    (icon plain)
  "Use icon or plain text for modeline spec."
  (if (entropy/emacs-icons-displayable-p)
      icon
    plain))

(defvar entropy/emacs-modeline--simple-mode-line-format)
(setq
 entropy/emacs-modeline--simple-mode-line-format
 '("%e"
   ;; mode-line-front-space
   (:eval (if (and
               (bound-and-true-p eyebrowse-mode)
               ;; more conditions
               )
              (entropy/emacs-modeline--mdl-common-eyebrowse-segment)))
   mode-line-mule-info
   mode-line-client
   mode-line-modified " "
   (:eval
    (if (buffer-narrowed-p)
        (entropy/emacs-modeline--origin-mdl-propertize-face "><" 'warning)
      "↕"))
   ;;"remote:" mode-line-remote " "
   ;; mode-line-frame-identification
   ;; mode-line-modes
   " "
   (:eval
    (entropy/emacs-modeline--origin-mdl-use-icon-or-plain
     (cond ((string-match-p "magit" (symbol-name major-mode))
            (all-the-icons-icon-for-mode major-mode :v-adjust 0.001))
           ((and (derived-mode-p 'prog-mode)
                 (not (eq major-mode 'emacs-lisp-mode)))
            (cond
             ((and (eq major-mode 'python-mode)
                   (eq entropy/emacs-theme-sticker 'doom-1337))
              (all-the-icons-icon-for-mode
               major-mode
               :v-adjust 0.001
               :face 'all-the-icons-maroon))
             (t
              (all-the-icons-icon-for-mode major-mode :v-adjust 0.001))))
           (t
            (all-the-icons-icon-for-mode major-mode)))
     (entropy/emacs-modeline--origin-mdl-propertize-face
      (format "%s" major-mode)
      (if (member entropy/emacs-theme-sticker '(doom-1337))
          'highlight
        'success))))
   " "
   "" entropy/emac-modeline--origin-mdl-buffer-identification " "
   (:eval
    (entropy/emacs-modeline--origin-mdl-use-icon-or-plain
     (concat (all-the-icons-faicon "pencil-square-o" :face 'all-the-icons-yellow :v-adjust -0.1) " ")
     (concat (entropy/emacs-modeline--origin-mdl-propertize-face
              "POS:"
              'link)
             " ")))
   mode-line-position
   (:eval
    (when vc-mode
      (entropy/emacs-modeline--origin-mdl-use-icon-or-plain
       (format " %s%s " (all-the-icons-octicon "git-branch" :v-adjust 0.01 :face 'all-the-icons-red)
               vc-mode)
       (format "%s "
               (entropy/emacs-modeline--origin-mdl-propertize-face
                vc-mode
                'warning)))))
   mode-line-misc-info
   mode-line-end-spaces))

(defun entropy/emacs-mode-line-origin-theme ()
  (setq-default
   mode-line-format
   '(:eval
     (if (entropy/emacs-modeline--subr-func->judge-current-window-focus-on-p)
         entropy/emacs-modeline--simple-mode-line-format
       (propertize
        (make-string
         (+ (window-width) 3) ?─ t)
        'face
        'error)))))

;; **** doom modeline
(entropy/emacs-usepackage-with-permanently-defer doom-modeline
  ;; We indicate to let doom modeline always not be fully required
  ;; unless user start it since its messy of its dirty hacking
  :commands (doom-modeline-mode
             doom-modeline-refresh-bars)
  :preface

  (defvar nyan-minimum-window-width)
  (defvar poke-line-minimum-window-width)

  (defvar entropy/emacs-modeline--doom-modeline-spec-done nil)
  (defvar entropy/emacs-modeline--doom-modeline-enable-done nil)

  (defun entropy/emacs-modeline--doom-modeline-specification ()
    (setq doom-modeline-height 25
          doom-modeline-bar-width 1
          doom-modeline-buffer-file-name-style 'buffer-name
          doom-modeline-major-mode-color-icon t
          doom-modeline-icon
          (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))))

  (defun entropy/emacs-modeline--doom-modeline-spec-clean ()
    (let (_)
      (doom-modeline-mode 0)
      (entropy/emacs-modeline-restore-default-mdlfmt)
      (setq entropy/emacs-modeline--doom-modeline-spec-done nil
            entropy/emacs-modeline--doom-modeline-enable-done nil)))

;; ***** doom-modeline hydra
  :eemacs-indhc
  (((:enable t :defer t)
    (doom-modeline-dispatch))
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon :enable t)
     ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
      "unicode fallback" :toggle doom-modeline-unicode-fallback :enable t)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon :enable t)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon :enable t)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon :enable t)
     ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon :enable t)
     ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
      "modal" :toggle doom-modeline-modal-icon :enable t))
    "Segment"
    (("M" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
      "minor modes" :toggle doom-modeline-minor-modes :enable t)
     ("W" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
      "word count" :toggle doom-modeline-enable-word-count :enable t)
     ("E" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
      "encoding" :toggle doom-modeline-buffer-encoding :enable t)
     ("I" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
      "indent" :toggle doom-modeline-indent-info :enable t)
     ("L" (setq doom-modeline-lsp (not doom-modeline-lsp))
      "lsp" :toggle doom-modeline-lsp :enable t)
     ("P" (setq doom-modeline-persp-name (not doom-modeline-persp-name))
      "perspective" :toggle doom-modeline-persp-name :enable t)
     ("G" (setq doom-modeline-github (not doom-modeline-github))
      "github" :toggle doom-modeline-github :enable t)
     ("N" (setq doom-modeline-gnus (not doom-modeline-gnus))
      "gnus" :toggle doom-modeline-gnus :enable t)
     ("U" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e :enable t)
     ("R" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc :enable t)
     ("F" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
      "irc buffers" :toggle doom-modeline-irc-buffers :enable t)
     ("S" (progn
            (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
            (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
      "simple checker" :toggle doom-modeline-checker-simple-format :enable t)
     ("V" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version :enable t))
    "Style"
    (("a" (setq doom-modeline-buffer-file-name-style 'auto)
      "auto"
      :toggle (eq doom-modeline-buffer-file-name-style 'auto) :enable t)
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
      "truncate upto project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
     ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
      "truncate from project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
     ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
      "truncate with project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
     ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
      "truncate except project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
     ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
      "truncate upto root"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
     ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
      "truncate all"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :enable t :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
    "Project Detection"
    (("p f" (setq doom-modeline-project-detection 'ffip)
      "ffip"
      :enable t :toggle (eq doom-modeline-project-detection 'ffip))
     ("p t" (setq doom-modeline-project-detection 'projectile)
      "projectile"
      :enable t :toggle (eq doom-modeline-project-detection 'projectile))
     ("p p" (setq doom-modeline-project-detection 'project)
      "project"
      :enable t :toggle (eq doom-modeline-project-detection 'project))
     ("p n" (setq doom-modeline-project-detection nil)
      "disable"
      :enable t :toggle (eq doom-modeline-project-detection nil)))
    "Misc"
    (("g" (progn
            (message "Fetching GitHub notifications...")
            (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
            (browse-url "https://github.com/notifications"))
      "github notifications" :exit t :enable t)
     ("e" (if (bound-and-true-p flycheck-mode)
              (flycheck-list-errors)
            (flymake-show-diagnostics-buffer))
      "list errors" :exit t :enable t)
     ("B" (if (bound-and-true-p grip-mode)
              (grip-browse-preview)
            (message "Not in preview"))
      "browse preview" :exit t :enable t)
     ("z h" (counsel-read-setq-expression 'doom-modeline-height) "set height" :enable t)
     ("z w" (counsel-read-setq-expression 'doom-modeline-bar-width) "set bar width" :enable t)
     ("z g" (counsel-read-setq-expression 'doom-modeline-github-interval) "set github interval" :enable t)
     ("z n" (counsel-read-setq-expression 'doom-modeline-gnus-timer) "set gnus interval" :enable t))))



;; ***** init
  :init
  (setq doom-modeline-unicode-fallback t)

  ;; use `with-eval-after-load' instead of
  ;; `entropy/emacs-lazy-load-simple' since it will force requie the
  ;; pakage but we don't want it to messy emacs with its dirty
  ;; hacking injection at init time.
  (with-eval-after-load 'doom-modeline
    ;; modeline integration enhancement for `doom-modeline'
    (when (fboundp 'minions-mode)
      (add-hook 'doom-modeline-mode-hook
                #'minions-mode))
    (entropy/emacs-add-hook-lambda-nil
     dml-refresh-fc-after-eemacs-fontset
     entropy/emacs-font-set-end-hook
     (when (bound-and-true-p doom-modeline-mode)
       (doom-modeline-refresh-font-width-cache)))
    (entropy/emacs-add-hook-lambda-nil
     dml-refresh-fc-after-eemacs-startend
     entropy/emacs-startup-end-hook
     (when (bound-and-true-p doom-modeline-mode)
       (doom-modeline-refresh-font-width-cache))))

;; ***** config
  :config
  (unless entropy/emacs-modeline--doom-modeline-spec-done
    (entropy/emacs-modeline--doom-modeline-specification))

  ;; Sync doom-modeline internal default `mode-line-format' cache to
  ;; eemacs spec, it's needed becasue of that eemacs will recovery the
  ;; emacs default `mode-line-format' before any eemacs defined mode
  ;; line type switcher in which case `doom-modeline-mode' also did
  ;; that, we do not want to see different default `mode-line-format'
  ;; after the processing both of them.
  (when (boundp 'doom-modeline--default-format)
    (setq doom-modeline--default-format
          entropy/emacs-modeline-default-modeline-formt))

  ;; remove the ace-widnow hack from doom-modeline which cause some
  ;; mistake
  (advice-remove #'aw-update #'doom-modeline-aw-update)

;; ****** advices

  (defun entropy/emacs-modeline--doom-modeline-set-around-advice
      (orig-func &rest orig-args)
    "Around advice for `doom-modeline-set-modeline' which use
default variable set type for type 'main' which do not pollute
some cases."
    (let ((key (car orig-args)))
      (if (eq key 'main)
          (apply orig-func `(,key set-as-default))
        (apply orig-func orig-args))))
  (advice-add 'doom-modeline-set-modeline
              :around
              #'entropy/emacs-modeline--doom-modeline-set-around-advice)

  (defun entropy/emacs-modeline--doom-modeline-mode-around-advice
      (orig-func &rest orig-args)
    "The around advice for `doom-modeline-mode' to recovery the
`current-buffer' local `mode-line-format' when needed since
`doom-modeline' will recover the current-buffer `mode-line-format'
to its own origin `mode-line-format' of the load time which is a
internal indiscretion stick where do not think about there's
customized buffer local `mode-line-format' has been set after
enable it which do not want to be unset yet."
    (let ((current-buffer-in-special-case
           (with-current-buffer (current-buffer)
             (entropy/emacs-modeline-judge-modeline-special-p)))
          (current-local-mdlfmt
           (copy-tree (buffer-local-value
                       'mode-line-format
                       (current-buffer))))
          rtn)
      (setq rtn (apply orig-func orig-args))
      (when (and current-buffer-in-special-case
                 current-local-mdlfmt)
        (setq-local mode-line-format
                    current-local-mdlfmt))
      rtn))
  (advice-add 'doom-modeline-mode
              :around
              #'entropy/emacs-modeline--doom-modeline-mode-around-advice)

  (defvar __ya/doom-modeline--font-width-cache nil)
  (defun __adv/around/doom-modeline--font-width
      (orig-func &rest orig-args)
    "Cache the result of `doom-modeline--font-width' since it use
`face-all-attributes' on modeline to generated the key to
`doom-modeline--font-width-cache' for every modeline refresh
which lag so much."
    (if
        (and entropy/emacs-current-session-is-idle-p
             (doom-modeline--active))
        (setq __ya/doom-modeline--font-width-cache
              (apply orig-func orig-args))
      (or __ya/doom-modeline--font-width-cache
          (setq __ya/doom-modeline--font-width-cache
                (apply orig-func orig-args)))))
  (advice-add 'doom-modeline--font-width
              :around
              #'__adv/around/doom-modeline--font-width)

;; ****** eemacs doom-modeline segments spec
;; ******* company-indicator
  (doom-modeline-def-segment company-indicator
    "Company mode backends indicator."
    (let ((company-lighter-base
           (propertize "𝍎"
                       'face
                       (if (doom-modeline--active)
                           'mode-line-emphasis
                         'mode-line-inactive)))
          (company-backend-abbrev-indicatior
           (lambda (x_str)
             (format
              "%s"
              (propertize x_str 'face 'diary))))
          (cur_info nil))
      (if (not (bound-and-true-p company-mode))
          ""
        (setq cur_info
              (when entropy/emacs-current-session-is-idle-p
                ;; EEMACS_BUG:
                ;; ignore-erros for company info get since some
                ;; strange redisplay errors will occur.
                (ignore-errors
                  (if (consp company-backend)
                      (company--group-lighter
                       (nth company-selection
                            company-candidates)
                       "company")
                    (symbol-name company-backend)))))
        (cond ((or (null cur_info) (equal cur_info "nil"))
               company-lighter-base)
              ((bound-and-true-p company-candidates)
               (funcall company-backend-abbrev-indicatior
                        cur_info))
              (t
               "")))))

;; ******* workspace-number
  (doom-modeline-def-segment workspace-number
    "The current workspace name or number. Requires
`eyebrowse-mode' to be enabled.

Note:

This doom-modeline segment has been modified for adapting for
entropy-emacs."
    (if (and (bound-and-true-p eyebrowse-mode)
             (< 0 (length (eyebrowse--get 'window-configs))))
        (let* ((num (eyebrowse--get 'current-slot))
               (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
               (str (if (and tag (< 0 (length tag)))
                        (concat (int-to-string num) ":" tag)
                      (when num (int-to-string num)))))
          (propertize
           (format " %s " str) 'face
           (if (string-match-p "\\.[[:digit:]]" str)
               (cond
                ((doom-modeline--active)
                 'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived)
                (t 'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived_inactive))
             (cond ((doom-modeline--active)
                    'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main)
                   (t
                    'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main_inactive)))))
      ""))


;; ******* idle activated =buffer position= indicator

  ;; EEMACS_MAINTENANCE: update folllow upstream internal defination
  (doom-modeline-def-segment buffer-position
    "The buffer position information."
    (cond ((and entropy/emacs-current-session-is-idle-p
                (doom-modeline--active) ;EEMACS_MAINTENANCE: update folllow upstream internal defination
                (not (bound-and-true-p company-candidates)))
           (let* ((active (doom-modeline--active))
                  (lc '(line-number-mode
                        (column-number-mode
                         (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                         "%l")
                        (column-number-mode (doom-modeline-column-zero-based ":%c" ":%C"))))
                  (face (if active 'mode-line 'mode-line-inactive))
                  (mouse-face 'mode-line-highlight)
                  (local-map mode-line-column-line-number-mode-map))
             (concat
              (doom-modeline-spc)
              (doom-modeline-spc)

              (propertize (format-mode-line lc)
                          'face face
                          'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                          'mouse-face mouse-face
                          'local-map local-map)

              (cond ((and active
                          (bound-and-true-p nyan-mode)
                          (>= (window-width) nyan-minimum-window-width))
                     (concat
                      (doom-modeline-spc)
                      (doom-modeline-spc)
                      (propertize (nyan-create) 'mouse-face mouse-face)))
                    ((and active
                          (bound-and-true-p poke-line-mode)
                          (>= (window-width) poke-line-minimum-window-width))
                     (concat
                      (doom-modeline-spc)
                      (doom-modeline-spc)
                      (propertize (poke-line-create) 'mouse-face mouse-face)))
                    (t
                     (when doom-modeline-percent-position
                       (concat
                        (doom-modeline-spc)
                        (propertize (format-mode-line '("" doom-modeline-percent-position "%%"))
                                    'face face
                                    'help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu"
                                    'mouse-face mouse-face
                                    'local-map local-map)))))
              (when (or line-number-mode column-number-mode doom-modeline-percent-position)
                (doom-modeline-spc)))))
          (t
           " ......... ")))


;; ******* idle actived =matches= indicator

  (doom-modeline-def-segment matches
    "Redefined by eemacs to run while
`entropy/emacs-current-session-is-idle-p' is non-nill"
    (cond
     ((and entropy/emacs-current-session-is-idle-p
           (doom-modeline--active))
      (let ((meta (concat (doom-modeline--macro-recording)
                          (doom-modeline--symbol-overlay))))
        (or (and (not (equal meta "")) meta)
            (doom-modeline--buffer-size))))
     (t
      " ... ")))

;; ******* idle actived =buffer-info= indicator

  (doom-modeline-def-segment buffer-info
    "Combined information about the current buffer, including the
current working directory, the file name, and its
state (modified, read-only or non-existent).

NOTE: this functio has been redefined by eemacs to run while idle
while `entropy/emacs-current-session-is-idle-p' is non-nil."
    (cond
     ((or
       ;; we do not inhibit show for some non frequency status
       (or
        (eq last-command 'entropy/grom-read-only-buffer))
       (and entropy/emacs-current-session-is-idle-p
            (doom-modeline--active)))
      (concat
       (doom-modeline-spc)
       (doom-modeline--buffer-mode-icon)
       (doom-modeline--buffer-state-icon)
       (doom-modeline--buffer-name)))
     (t
      "🔃")))

;; ******* idle actived =major-mode= indictor

  (defun __ya/doom-modeline-segment--major-mode
      (orig-func &rest orig-args)
    (cond ((and entropy/emacs-current-session-is-idle-p
                (doom-modeline--active))
           (apply orig-func orig-args))
          (t
           mode-name)))
  (if (fboundp 'doom-modeline-segment--major-mode)
      (advice-add 'doom-modeline-segment--major-mode
                  :around
                  #'__ya/doom-modeline-segment--major-mode)
    (error "EEMACS_MAINTENANCE: doom-modeline-segment--major-mode \
function name has been changed, please update internal hack of \
`__ya/doom-modeline-segment--major-mode'."))

;; ****** eemacs doom-modeline type spec

  (defun __adv/around/doom-modeline-format-just-in-selected-window
      (orig-func &rest orig-args)
    "Run 'doom-modeline-formt--*' class modline format focus in
`selected-window' to reduce the perfomance issue."
    (cond
     ((and
       ;; entropy/emacs-current-session-is-idle-p
       (doom-modeline--active))
      (apply orig-func orig-args))
     (t
      ;; The window line replace the complicated modeline format while
      ;; the window is not in activation
      (propertize
       (make-string
        (+ (window-width) 3) ?─ t)
       'face
       'error))))

  (doom-modeline-def-modeline 'main
    '(bar workspace-number
          ;;window-number
          matches
          buffer-info remote-host buffer-position parrot
          " " company-indicator selection-info)
    '(misc-info lsp irc mu4e github debug minor-modes
                input-method buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'project
    '(bar
      ;; workspace-number
      window-number buffer-default-directory)
    '(misc-info mu4e github debug major-mode process))

  (dolist (fmt-func-suffix
           '(
             pdf
             vcs
             helm
             info
             main
             media
             message
             minimal
             org-src
             package
             project
             special
             dashboard
             timemachine
             ))
    (eval
     `(let* ((func ',(intern
                      (format "doom-modeline-format--%s"
                              fmt-func-suffix))))
        (when (fboundp func)
          (advice-add
           func
           :around
           #'__adv/around/doom-modeline-format-just-in-selected-window)))))

;; ****** common spec

  ;; Disable internally typo of wrong advice type for `select-window'
  ;; using `:after' keyword without WINDOW return refers to githuh
  ;; issue:
  ;; https://github.com/seagle0128/doom-modeline/issues/386#issue-734308633
  (advice-remove #'select-window #'doom-modeline-update-persp-name)

  )

;; ** toggle function

(defun entropy/emacs-modeline--query-for-messy-modeline-enable
    (modeline-name-str)
  (if (yes-or-no-p
       (format
        "Mode line type of (%s) has dirty hack which \
will messy emacs performance, really enable it?"
        modeline-name-str))
      t
    (error "Cancled enable doom-modeline!")))

(defun entropy/emacs-modeline--mdl-tidy-spec ()
  (pcase entropy/emacs-mode-line-sticker
    ("powerline"
     (entropy/emacs-modeline--powerline-spec-clean))
    ("spaceline"
     (entropy/emacs-modeline--spaceline-spec-clean))
    ("doom" (entropy/emacs-modeline--doom-modeline-spec-clean))
    ("origin" (entropy/emacs-modeline--origin-spec-clean))
    (_ nil)))

(eval-and-compile
  (defvar entropy/emacs-modeline--toggle-type-register nil))

(defmacro entropy/emacs-modeline--define-toggle
    (name spec-form spec-done-indcator enable-done-indicator
          &rest enable-form)
  ;; NOTE: if change the auto gened function name format, you must
  ;; update the doc-string corresponding part of
  ;; `entropy/emacs-theme-load-modeline-specifix'
  "Define a modeline toggling function named with NAME as a type
indicator.

Arg SPEC-FORM is a list of a form to do the specifications
procedure before the modeline type would do of which NAME
indicated.

Arg SPEC-DONE-INDICATOR is a variable name which is a variable to
indicate whether the specification did by SPEC-FORM has already
done, if thus, the SPEC-FORM will not be proceed in this time
since the duplicate specification may cause some problems.

Arg ENABLE-DONE-INDICATOR has the same meaning as what
SPEC-DONE-INDICATOR has but for tided to ENABLE-FORM only.

Arg ENABLE-FORM is the body of this macro used to do the modeline
format enabling process.
"
  (let ((func-name (intern (concat "entropy/emacs-modeline-mdl-" name "-toggle"))))
    `(progn
       (push (cons ,name ',func-name) entropy/emacs-modeline--toggle-type-register)
       (defun ,func-name ()
         ""
         (declare (interactive-only t))
         (interactive)
         ;; warn for `doom-modeline' laggy
         (defvar __doom-modeline-enabled-yet? nil)
         (if (and (string= ,name "doom")
                  (null __doom-modeline-enabled-yet?))
             (and (entropy/emacs-modeline--query-for-messy-modeline-enable
                   ,name)
                  (setq __doom-modeline-enabled-yet? t)))
         ;; fistly tidy up the remaining spec of previous modeline type
         (entropy/emacs-modeline--mdl-tidy-spec)
         (setq entropy/emacs-mode-line-sticker ,name)
         (let (_)
           (progn
             ,spec-form
             (setq ,spec-done-indcator t)
             (if ,enable-done-indicator
                 (message
                  "You modeline has been toggled to '%s' yet,\
 do not duplicate such operation" ,name)
               ,@enable-form
               (setq ,enable-done-indicator t)
               (message "Toggle modeline type to '%s' successfully"
                        ,name))))))))

;; toggle functionn for spaceline
(advice-add 'spaceline-spacemacs-theme
            :after #'entropy/emacs-modeline--set-mdlfmt-after-advice)
(entropy/emacs-modeline--define-toggle
 "spaceline"
 (entropy/emacs-modeline--spaceline-specification)
 entropy/emacs-modeline--spaceline-spec-done
 entropy/emacs-modeline--spaceline-enable-done
 (spaceline-spacemacs-theme))

;; toggle functionn for powerline
(advice-add 'entropy/emacs-modeline-do-powerline-set
            :after #'entropy/emacs-modeline--set-mdlfmt-after-advice)
(entropy/emacs-modeline--define-toggle
 "powerline"
 nil
 entropy/emacs-modeline--powerline-spec-done
 entropy/emacs-modeline--powerline-enable-done
 (entropy/emacs-modeline-do-powerline-set))

;; toggle functionn for doom-modeline
(entropy/emacs-modeline--define-toggle
 "doom"
 (entropy/emacs-modeline--doom-modeline-specification)
 entropy/emacs-modeline--doom-modeline-spec-done
 entropy/emacs-modeline--doom-modeline-enable-done
 (doom-modeline-mode +1))

;; toggle functionn for eemacs origin modeline theme
(advice-add 'entropy/emacs-mode-line-origin-theme
            :after #'entropy/emacs-modeline--set-mdlfmt-after-advice)
(entropy/emacs-modeline--define-toggle
 "origin" nil
 entropy/emacs-modeline--origin-spec-done
 entropy/emacs-modeline--origin-enable-done
 (entropy/emacs-mode-line-origin-theme))

(entropy/emacs-lazy-initial-for-hook
 '(entropy/emacs-hydra-hollow-call-before-hook)
 "modeline-hydra-hollow-init" "modeline-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'eemacs-modeline-toggle nil
  '("All"
    (("m m d t" entropy/emacs-modeline-mdl-doom-toggle
      "Toggle modeline type to [doom-mode-line] type"
      :enable t :toggle (string= entropy/emacs-mode-line-sticker "doom"))
     ("m m d d"
      (:eval
       (prog1
           '__doom-modeline--ensure-warn/hydra-func
         (defalias '__doom-modeline--ensure-warn/hydra-func
           (lambda nil
             (interactive)
             (if (bound-and-true-p doom-modeline-mode)
                 (funcall
                  (entropy/emacs-hydra-hollow-category-common-individual-get-caller
                   'doom-modeline-dispatch))
               (message "doom-modeline is no actived yet!"))))))
      "Call the dispatch for [doom-mode-line]."
      :enable t :exit t)
     ("m m s t" entropy/emacs-modeline-mdl-spaceline-toggle
      "Toggle modeline type to [spacemacs line] type"
      :enable t :toggle (string= entropy/emacs-mode-line-sticker "spaceline"))
     ("m m p t" entropy/emacs-modeline-mdl-powerline-toggle
      "Toggle modeline type to [powerline (riched modeline)] type"
      :enable t :toggle (string= entropy/emacs-mode-line-sticker "powerline"))
     ("m m o t" entropy/emacs-modeline-mdl-origin-toggle
      "Toggle modeline type to [entropy emacs origin] type"
      :enable t :toggle (string= entropy/emacs-mode-line-sticker "origin")))))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("WI&BUF"
    (("C-c m"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'eemacs-modeline-toggle))
      "Toggle mode line type"
      :enable t :eemacs-top-bind t :exit t)))))

;; ** init load conditions
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
            '(entropy/emacs-modeline-mdl-spaceline-toggle)))

     ;; init powerline
     ((string= entropy/emacs-modeline-style "powerline")
      (setq entropy/emacs-modeline--mdl-init-caller
            '(entropy/emacs-modeline-mdl-powerline-toggle)))

     ;; init-origin style
     ((string= entropy/emacs-modeline-style "origin")
      (setq entropy/emacs-modeline--mdl-init-caller
            '(entropy/emacs-modeline-mdl-origin-toggle)))

     ;; init doom-modeline
     ((string= entropy/emacs-modeline-style "doom")
      (unless (ignore-errors
                (entropy/emacs-modeline--query-for-messy-modeline-enable
                 entropy/emacs-modeline-style))
        (message "Fall back to use origin modeline ...")
        (setq cancel-branch t
              entropy/emacs-modeline-style "origin")
        (entropy/emacs-modeline--mdl-init))
      (setq entropy/emacs-modeline--mdl-init-caller
            '(entropy/emacs-modeline-mdl-doom-toggle)))

     ;; any other type was unsupport
     (t (warn (format "entropy/emacs-modeline-style's value '%s' is invalid." entropy/emacs-modeline-style))
        (setq entropy/emacs-modeline-style "origin"
              cancel-branch t)
        (entropy/emacs-modeline--mdl-init)))
    (setq entropy/emacs-mode-line-sticker entropy/emacs-modeline-style)
    (unless cancel-branch
      (funcall `(lambda () ,entropy/emacs-modeline--mdl-init-caller)))))

;; ** modeline-hide feature
(use-package hide-mode-line
  :commands (hide-mode-line-mode)
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; ** init procedure
(entropy/emacs-lazy-with-load-trail
 eemacs-modeline-init
 (redisplay t)
 (entropy/emacs-modeline--mdl-init)
 (redisplay t)
 (entropy/emacs-with-daemon-make-frame-done
  'eemacs-modeline-init nil nil
  '(progn
     (entropy/emacs-modeline--mdl-tidy-spec)
     (funcall (alist-get entropy/emacs-mode-line-sticker
                         entropy/emacs-modeline--toggle-type-register
                         nil nil 'string=)))))

;; * provide
(provide 'entropy-emacs-modeline)
