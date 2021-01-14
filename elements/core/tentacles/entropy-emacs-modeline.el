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
;; *** eemaac-specification

;; **** Advices after swither

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
    (cond ((entropy/emacs-modeline--mdl-egroup-current-window-focus-on-p?)
           (if derived
               'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived
             'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main))
          (t
           (if derived
               'entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived_inactive
             'entropy/emacs-defface-face-for-modeline-eyebrowse-face-main_inactive)))))

(defun entropy/emacs-modeline--mdl-common-eyebrowse-segment ()
  "Entropy-emacs specific modeline style.

This customization mainly adding the eyebrowse slot and tagging name show function."
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
    rtn))


;; *** modeline type defined
;; **** powerline group
;; ***** powerline
(use-package powerline
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
  (advice-add 'powerline-selected-window-active
              :around
              #'entropy/emacs-current-window-is-selected-common-around-advice))

;; ***** spaceline
(defvar entropy/emacs-modeline--spaceline-spec-done nil)
(defvar entropy/emacs-modeline--spaceline-enable-done nil)
(defvar entropy/emacs-modeline--spaceline-spec-list
  '())

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
(eval
 (delete
  nil
  `(use-package spaceline
     :init
     ,(if (eq entropy/emacs-ext-elpkg-get-type 'submodules)
          '(use-package spaceline-config
             :commands (spaceline-spacemacs-theme))
        '(ignore))
     :config
     (with-eval-after-load 'spaceline-segments
       (entropy/emacs-modeline--spaceline-defsegment-for-workspace))
     )))


;; **** origin type
;; ***** egroup core
(defvar entropy/emacs-modeline--mdl-egroup-selected-window
  (frame-selected-window))

(defun entropy/emacs-modeline--mdl-egroup-get-current-window ()
  ;; Get the current window but should exclude on the child frame.
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defun entropy/emacs-modeline--mdl-egroup-set-selected-window (&optional orig-func &rest orig-args)
  "Set `entropy/emacs-modeline--mdl-egroup' selected window indicator."
  (let ((rtn (when orig-func
               (apply orig-func orig-args)))
        (win
         (entropy/emacs-modeline--mdl-egroup-get-current-window)))
    (unless (minibuffer-window-active-p win)
      (setq entropy/emacs-modeline--mdl-egroup-selected-window win)
      (force-mode-line-update))
    rtn))

(defun entropy/emacs-modeline--mdl-egroup-unset-selected-window ()
  "Unset `entropy/emacs-modeline--mdl-egroup' appropriately."
  (setq entropy/emacs-modeline--mdl-egroup-selected-window nil)
  (force-mode-line-update))

(defun entropy/emacs-modeline--mdl-egroup-current-window-focus-on-p? ()
  (and entropy/emacs-modeline--mdl-egroup-selected-window
       (eq (entropy/emacs-modeline--mdl-egroup-get-current-window)
           entropy/emacs-modeline--mdl-egroup-selected-window)))

(advice-add 'entropy/emacs-modeline--mdl-egroup-current-window-focus-on-p?
            :around
            #'entropy/emacs-current-window-is-selected-common-around-advice)

(add-hook 'window-configuration-change-hook
          #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
(advice-add #'handle-switch-frame
            :around
            #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
(advice-add #'select-window
            :around
            #'entropy/emacs-modeline--mdl-egroup-set-selected-window)

(with-no-warnings
  (if (not (boundp 'after-focus-change-function))
      (progn
        (add-hook 'focus-in-hook  #'entropy/emacs-modeline--mdl-egroup-set-selected-window)
        (add-hook 'focus-out-hook #'entropy/emacs-modeline--mdl-egroup-unset-selected-window))

    (defun entropy/emacs-modeline--mdl-egroup-refresh-frame ()
      (setq entropy/emacs-modeline--mdl-egroup-selected-window nil)
      (cl-loop for frame in (frame-list)
               if (eq (frame-focus-state frame) t)
               return (entropy/emacs-modeline--mdl-egroup-set-selected-window)))
    (add-function :after after-focus-change-function
                  #'entropy/emacs-modeline--mdl-egroup-refresh-frame)))


;; ***** egroup main
(defvar entropy/emacs-modeline--origin-spec-done nil)
(defvar entropy/emacs-modeline--origin-enable-done nil)

(defun entropy/emacs-modeline--origin-spec-clean ()
  (entropy/emacs-modeline-restore-default-mdlfmt)
  (setq entropy/emacs-modeline--origin-spec-done nil
        entropy/emacs-modeline--origin-enable-done nil))

(defun entropy/emacs-mode-line-origin-theme ()
  (setq-default mode-line-format
                '("%e"
                  ;; mode-line-front-space
                  (:eval (when (bound-and-true-p eyebrowse-mode)
                           (entropy/emacs-modeline--mdl-common-eyebrowse-segment)))
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

;; **** doom modeline
(use-package doom-modeline
  :ensure nil
  :commands (doom-modeline-mode
             doom-modeline-refresh-bars)
  :preface

  (defvar entropy/emacs-modeline--doom-modeline-spec-done nil)
  (defvar entropy/emacs-modeline--doom-modeline-enable-done nil)

  (defun entropy/emacs-modeline--doom-modeline-specification ()
    (setq doom-modeline-height 10
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
  (((:enable t)
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

  (when (equal entropy/emacs-modeline-style "doom")
    (entropy/emacs-lazy-load-simple doom-modeline
      (add-hook 'entropy/emacs-font-set-end-hook
                #'doom-modeline-refresh-font-width-cache)
      (add-hook 'entropy/emacs-startup-end-hook
                #'doom-modeline-refresh-font-width-cache)))

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

;; ****** advices
  (advice-add 'doom-modeline--active
              :around
              #'entropy/emacs-current-window-is-selected-common-around-advice)
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

;; ******* workspace-number
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

  (doom-modeline-def-segment buffer-position
    "The buffer position information."
    (cond (entropy/emacs-current-session-is-idle
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
           " ⨂")))

;; ****** eemacs doom-modeline type spec
  (doom-modeline-def-modeline 'main
   '(bar workspace-number window-number
         matches buffer-info remote-host buffer-position parrot
         " " company-indicator selection-info)
   '(misc-info lsp irc mu4e github debug minor-modes
               input-method buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'project
    '(bar workspace-number window-number buffer-default-directory)
    '(misc-info mu4e github debug major-mode process))

;; ****** common spec

  ;; Disable internally typo of wrong advice type for `select-window'
  ;; using `:after' keyword without WINDOW return refers to githuh
  ;; issue:
  ;; https://github.com/seagle0128/doom-modeline/issues/386#issue-734308633
  (advice-remove #'select-window #'doom-modeline-update-persp-name)

  )

;; ** toggle function
(defun entropy/emacs-modeline--mdl-tidy-spec ()
  (pcase entropy/emacs-mode-line-sticker
    ("powerline"
     (entropy/emacs-modeline--powerline-spec-clean))
    ("spaceline"
     (entropy/emacs-modeline--spaceline-spec-clean))
    ("doom" (entropy/emacs-modeline--doom-modeline-spec-clean))
    ("origin" (entropy/emacs-modeline--origin-spec-clean))
    (_ nil)))

(defvar entropy/emacs-modeline--toggle-type-register nil)

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
    (push (cons name func-name) entropy/emacs-modeline--toggle-type-register)
    `(defun ,func-name ()
       (interactive)
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
                      ,name)))))))

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

;; toggle functionn for eemacs-egroup
(advice-add 'entropy/emacs-mode-line-origin-theme
            :after #'entropy/emacs-modeline--set-mdlfmt-after-advice)
(entropy/emacs-modeline--define-toggle
 "origin" nil
 entropy/emacs-modeline--origin-spec-done
 entropy/emacs-modeline--origin-enable-done
 (entropy/emacs-mode-line-origin-theme))

(entropy/emacs-hydra-hollow-common-individual-hydra-define
 'eemacs-modeline-toggle nil
 '("All"
   (("m m d t" entropy/emacs-modeline-mdl-doom-toggle
     "Toggle modeline type to [doom-mode-line] type"
     :enable t :toggle (string= entropy/emacs-mode-line-sticker "doom"))
    ("m m d d"
     (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller 'doom-modeline-dispatch))
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
     (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
             'eemacs-modeline-toggle))
     "Toggle mode line type"
     :enable t :eemacs-top-bind t :exit t))))

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
