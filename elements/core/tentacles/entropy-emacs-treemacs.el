;;; entropy-emacs-treemacs --- eemacs treemacs specifications
;;
;; * Copyright (C) 2020-02-03  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Created:       2020-02-03
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; commentary
;;
;; * Configuration:
;;
;; configuration
;;
;; * Code:

;; ** Require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)


;; ** main
(use-package treemacs
  :if (eq entropy/emacs-tree-visual-type 'treemacs)
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (("M-0"       . treemacs-select-window)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
;; *** preface
  :preface

  (defun entropy/emacs-treemacs--get-buffer-path ()
    (or (buffer-file-name (current-buffer))
        (when (eq major-mode 'dired-mode)
          (treemacs--canonical-path (dired-current-directory)))))

  (defun entropy/emacs-treemacs--buffer-in-project-p ()
    (let ((current-file (entropy/emacs-treemacs--get-buffer-path)))
      (treemacs--find-project-for-buffer current-file)))

  (defun entropy/emacs-treemacs--unwind-for-init (orig-func &rest orig-args)
    "Unwind protect to set `treemacs--ready-to-follow' t for init time."
    (unwind-protect
        (apply orig-func orig-args)
      (setq treemacs--ready-to-follow t)))

  (defvar entropy/emacs-treemacs--current-focused-buffer nil
    "Focused buffer that treemacs followed.")
  (defun entropy/emacs-treemacs--after-file-follow-do-mode-line-update
      (orig-func &rest orig-args)
    "Switch buffer to the origin buffer the buffer of before
treemacs does file follow action within the newly(may be)
`selected-window' only once compare with the focused buffer
cached in
`entropy/emacs-treemacs--after-file-follow-do-mode-line-update'.

The exists meaning for this wrapper is that the function
`treemacs--follow' will cause mode line format update with
unpredictable render bug, so that force redirect
window-configuration can fix this problem on interface, its purely
may be a emacs native bug."
    (let* ((cur-buff (current-buffer))
           (buf-fname (buffer-file-name cur-buff))
           rtn)
      (if (ignore-errors
            ;; NOTE: Suppress its file follow in `entropy/emacs-stuffs-topdir'
            ;; EEMACS_MAINTENANCE: this part may have more conditions
            (file-equal-p (expand-file-name (file-name-nondirectory buf-fname) entropy/emacs-stuffs-topdir)
                          (expand-file-name buf-fname)))
          (setq treemacs--follow-timer nil)
        (setq rtn (apply orig-func orig-args))
        (when (and (treemacs-get-local-window)
                   (not (active-minibuffer-window))
                   (not (eq major-mode 'treemacs-mode))
                   (buffer-file-name cur-buff))
          (unless (eq cur-buff entropy/emacs-treemacs--current-focused-buffer)
            (set-buffer cur-buff)
            (select-window (get-buffer-window cur-buff))
            (setq entropy/emacs-treemacs--current-focused-buffer cur-buff)))
        rtn)))

  (defun entropy/emacs-treemacs-auto-focus-on-common-buffer (&rest _)
    "Auto change focus point on common buffer (i.e. not treemacs
buffer) for some special hook."
    (let ((wlens (length (window-list))))
      (when (eq major-mode 'treemacs-mode)
        (cond
         ((> wlens 1)
          (other-window 1))
         ((= wlens 1)
          (treemacs-quit))))))

  (defun entropy/emacs-treemacs-toggle-treemacs ()
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))
      ('exists  (if (entropy/emacs-treemacs--buffer-in-project-p)
                    (treemacs-select-window)
                  (let ((current-prefix-arg t))
                    (funcall-interactively
                     'treemacs-add-project-to-workspace
                     (read-directory-name "Get project root: " nil nil t)))))
      ('none    (treemacs--init))))

  (defun entropy/emacs-treemacs-open-with (&rest _)
    "Open current node with external apps relying on
`entropy/open-with-match-open'."
    (interactive)
    (let ((file-path
           (treemacs-button-get (treemacs-current-button) :path)))
      (unless (fboundp 'entropy/open-with-match-open)
        (require 'entropy-open-with))
      (entropy/open-with-match-open
       (list file-path))))

;; *** eemacs-indhc
  :eemacs-indhc
  (((:enable t)
    (treemacs))
   ("Toggle treemacs"
    (("<f8>" entropy/emacs-treemacs-toggle-treemacs
      "Treemacs Slide Show Dwim"
      :enable t
      :exit t
      :global-bind t)
     ("C-<f8>" treemacs "Treemacs Slide Show Native"
      :enable t
      :exit t
      :global-bind t))))

;; *** eemacs-tpha
  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b t"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'treemacs))
      "Slide Filesystem Tree"
      :enable t :exit t))))

;; *** eemacs-mphc

  :eemacs-mmphc
  (((:enable t)
    (treemacs-mode
     (treemacs treemacs-mode-map) t
     ((4 :width-desc "Basic operations ")
      (4 :width-desc "Project refer and Misc."))))
   ("Navigation"
    (("n" treemacs-next-line "next line"
      :enable t :map-inject t :exit t)
     ("p" treemacs-previous-line "previous line"
      :enable t :map-inject t :exit t)
     ("M-n" treemacs-next-neighbour "next neighbour"
      :enable t :map-inject t :exit t)
     ("M-p" treemacs-previous-neighbour "previous neighbour"
      :enable t :map-inject t :exit t)
     ("u" treemacs-goto-parent-node "goto parent"
      :enable t :map-inject t :exit t)
     ("M-N" treemacs-next-line-other-window "down next window"
      :enable t :map-inject t :exit t)
     ("M-P" treemacs-previous-line-other-window "up next window"
      :enable t :map-inject t :exit t))
    "Opening Nodes "
    (("TAB" treemacs-TAB-action "dwim TAB"
      :enable t :map-inject t :exit t)
     ("RET" treemacs-RET-action "dwim RET"
      :enable t :map-inject t :exit t)
     ("M-RET" entropy/emacs-treemacs-open-with "open with external kits"
      :enable t :map-inject t :exit t)
     ("o o" treemacs-visit-node-no-split "open no split"
      :enable t :map-inject t :exit t)
     ("o h" treemacs-visit-node-horizontal-split "open horizontal"
      :enable t :map-inject t :exit t)
     ("o v" treemacs-visit-node-vertical-split "open vertical"
      :enable t :map-inject t :exit t)
     ("o a a" treemacs-visit-node-ace "open ace"
      :enable t :map-inject t :exit t)
     ("o a h" treemacs-visit-node-ace-horizontal-split "open ace horizontal"
      :enable t :map-inject t :exit t)
     ("o a v" treemacs-visit-node-ace-vertical-split "open ace vertical"
      :enable t :map-inject t :exit t)
     ("o r" treemacs-visit-node-in-most-recently-used-window "open mru window"
      :enable t :map-inject t :exit t)
     ("o x" treemacs-visit-node-in-external-application "open externally"
      :enable t :map-inject t :exit t)
     ("H" treemacs-collapse-parent-node "close parent"
      :enable t :map-inject t :exit t))
    "File Management"
    (("c f" treemacs-create-file "create file"
      :enable t :map-inject t :exit t)
     ("c d" treemacs-create-dir "create dir"
      :enable t :map-inject t :exit t)
     ("R" treemacs-rename "rename"
      :enable t :map-inject t :exit t)
     ("d" treemacs-delete "delete"
      :enable t :map-inject t :exit t)
     ("y f" treemacs-copy-file "copy"
      :enable t :map-inject t :exit t)
     ("m" treemacs-move-file "move"
      :enable t :map-inject t :exit t))
    "Toggles"
    (("t f" treemacs-follow-mode "follow mode"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs-follow-mode) t nil)
      :exit t)
     ("t a" treemacs-filewatch-mode "filewatch mode"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs-filewatch-mode) t nil)
      :exit t)
     ("t g" treemacs-git-mode "git mode"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs-git-mode) t nil)
      :exit t)
     ("t h" treemacs-toggle-show-dotfiles "show dotfiles"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs-show-hidden-files) t nil)
      :exit t)
     ("t w" treemacs-toggle-fixed-width "resizability"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs--width-is-locked) t nil)
      :exit t)
     ("t v" treemacs-fringe-indicator-mode "fringe indicator"
      :enable t :map-inject t :toggle (if (bound-and-true-p treemacs-fringe-indicator-mode) t nil)
      :exit t))
    "Projects"
    (("C-c C-p a" treemacs-add-project-to-workspace "add project"
      :enable t :map-inject t :exit t)
     ("C-c C-p d" treemacs-remove-project-from-workspace "remove project"
      :enable t :map-inject t :exit t)
     ("C-c C-p r" treemacs-rename-project "rename project"
      :enable t :map-inject t :exit t))
    "Workspaces"
    (("C-c C-w e" treemacs-edit-workspaces "Edit Workspaces"
      :enable t :map-inject t :exit t)
     ("C-c C-w a" treemacs-create-workspace "Create Workspaces"
      :enable t :map-inject t :exit t)
     ("C-c C-w d" treemacs-remove-workspace "Remove Workspaces"
      :enable t :map-inject t :exit t)
     ("C-c C-w r" treemacs-rename-workspace "Rename Workspace"
      :enable t :map-inject t :exit t)
     ("C-c C-w s" treemacs-switch-workspace "Switch Workspace"
      :enable t :map-inject t :exit t)
     ("C-c C-w f" treemacs-set-fallback-workspace "Set Fallback"
      :enable t :map-inject t :exit t))
    "Misc."
    (("g" treemacs-refresh "refresh"
      :enable t :map-inject t :exit t)
     ("w" treemacs-set-width "(re)set width"
      :enable t :map-inject t :exit t)
     ("y y" treemacs-copy-path-at-point "copy path"
      :enable t :map-inject t :exit t)
     ("y r" treemacs-copy-project-root "copy root"
      :enable t :map-inject t :exit t)
     ("s" treemacs-resort "re-sort"
      :enable t :map-inject t :exit t)
     ("b" treemacs-add-bookmark "bookmark"
      :enable t :map-inject t :exit t))))

;; *** init
  :init

  (entropy/emacs-lazy-load-simple eyebrowse
    (add-hook 'eyebrowse-pre-window-switch-hook
              #'entropy/emacs-treemacs-auto-focus-on-common-buffer))

;; *** config
  :config

  ;; set customized var after load `treemacs' so that some internal
  ;; vars are initialized
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init       t
        ;; treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   25
        treemacs-indentation             1
        treemacs-show-cursor             t
        treemacs-recenter-after-file-follow    t
        treemacs-recenter-after-project-expand t
        treemacs-recenter-after-project-jump   t
        treemacs-recenter-after-tag-follow     t
        treemacs-show-edit-workspace-help t)

  (advice-add 'treemacs--init :around #'entropy/emacs-treemacs--unwind-for-init)
  (advice-add 'treemacs--follow
              :around #'entropy/emacs-treemacs--after-file-follow-do-mode-line-update)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))


;; * provide

(provide 'entropy-emacs-treemacs)
