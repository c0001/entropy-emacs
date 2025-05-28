;; entropy-emacs-treemacs.el --- entropy emacs basic config  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20150411  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-treemacs.el
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
;; eemascs configuration for package 'treemacs'
;;
;; * Configuration:
;;
;; This file must loaded by `entorpy-emacs.el', other testing way is
;; not in the designation context.
;;
;; * Code

(defvar treemacs--themes)
(defun entropy/emacs-treemacs--init-preface nil
  (when (entropy/emacs-icons-displayable-p)
    (entropy/emacs-require-only-once 'treemacs-nerd-icons)))
(defun entropy/emacs-treemacs--init-advice (&rest _)
  (entropy/emacs-treemacs--init-preface))

(use-package treemacs
  :ensure nil
  :eemacs-adrequire
  ((:enable t :adfors (entropy/emacs-hydra-hollow-call-before-hook) :adtype hook :pdumper-no-end t))
  :eemacs-functions
  (treemacs-current-visibility
   treemacs-pulse-on-failure)
  :commands
  ;; EEMACS_MAINTENANCE: update this list since we should cover the
  ;; autoload from `package-user-dir' where non-patched treemacs
  ;; installed
  (
   treemacs
   treemacs-add-and-display-current-project
   treemacs-add-and-display-current-project-exclusively
   treemacs-add-bookmark
   treemacs-add-project-to-workspace
   treemacs-bookmark
   treemacs-create-workspace
   treemacs-create-workspace-from-project
   treemacs-decrease-width
   treemacs-edit-workspaces
   treemacs-follow-mode
   treemacs-git-mode
   treemacs-helpful-hydra
   treemacs-common-helpful-hydra
   treemacs-advanced-helpful-hydra
   treemacs-hide-gitignored-files-mode
   treemacs-indicate-top-scroll-mode
   treemacs-load-theme
   treemacs-next-workspace
   treemacs-peek-mode
   treemacs-previous-project
   treemacs-project-follow-mode
   treemacs-remove-project-from-workspace
   treemacs-remove-workspace
   treemacs-rename-project
   treemacs-rename-workspace
   treemacs-select-directory
   treemacs-select-scope-type
   treemacs-select-window
   treemacs-set-fallback-workspace
   treemacs-show-changelog
   treemacs-show-extensions
   treemacs-switch-workspace
   treemacs-tag-follow-mode
   treemacs-version
   )
  :eemacs-tpha
  (((:enable t :defer t))
   ("Utils"
    (("u t"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'treemacs-main))
      "Treemacs Commands Map"
      :enable t :exit t))))
  :eemacs-indhc
  (((:enable
     t
     :defer
     (:data (:adfors (entropy/emacs-after-startup-idle-hook) :adtype hook :pdumper-no-end t)))
    (treemacs-main nil nil (2 2 2)))
   ("Treemacs Main"
    (("<f8>" treemacs
      "Initialise or toggle treemacs."
      :enable t :global-bind t :exit t
      :toggle (eq (treemacs-current-visibility) 'visible))
     ("C-S-e" treemacs
      "Initialise or toggle treemacs. (graphic benefit key-binding)"
      :enable (or (daemonp) (display-graphic-p)) :global-bind t :exit t
      :toggle (eq (treemacs-current-visibility) 'visible))
     ("C-<f8>" treemacs-add-project-to-workspace
      "Add a project at given PATH to the current treemacs workspace"
      :enable t :global-bind t :exit t)
     ("C-S-<f8>" treemacs-select-directory
      "Add a arbitary  to open in treemacs."
      :enable t :global-bind t :exit t))))
  :eemacs-mmphc
  (((:enable
     t
     :defer
     (:data (:adfors (treemacs-mode-hook) :adtype hook :pdumper-no-end t)))
    (treemacs-mode (treemacs treemacs-mode-map) t (2 2 2)))
   ("Treemacs Help"
    ;; NOTE: we wrap each map-injected commands with
    ;; `call-interactively' since `treemacs-common-helpful-hydra' is
    ;; generated at run time whose each hydra keybinding is dragged
    ;; from `treemacs-mode-map' where we've did as below, but it use
    ;; `treemacs--find-keybind' as subroutine to find same commands'
    ;; keybingings in its keymap using their command symbol as same as
    ;; what we want to do, but its internal mechanism include a
    ;; key-code remap session that may break the `defhydra' wrapper
    ;; which made it error (ps. `defhydra' just error return nil no
    ;; force back to `top-level' so usually there's no notice for such
    ;; hydra defination failing.) with the keybinding we defined
    ;; e.g. it transfer kebinding M-<up> to M-UP but `defhydra' throw
    ;; error as it's a wrong key-stroke defination.
    (("?"            (call-interactively 'treemacs-common-helpful-hydra)
      "common helpful hydra to treemacs keymap"
      :enable t :exit t :map-inject t)
     ("M-?"          (call-interactively 'treemacs-advanced-helpful-hydra)
      "advanced helpful hydra to treemacs keymap"
      :enable t :exit t :map-inject t))
    "Treemacs Frequently Commands"
    (("M-<up>"
      (let ((_ (entropy/emacs-treemacs--goto-nearest-line t))
            (curpt (point)) ncurpt)
        (let (treemacs-pulse-on-failure)
          (treemacs-without-messages
           (treemacs-goto-parent-node)
           (and (>= (point) curpt)
                (setq ncurpt (point))
                (treemacs-previous-neighbour))))
        (and (and ncurpt (>= (point) ncurpt))
             (progn (goto-char curpt)
                    (treemacs-pulse-on-failure "No prev parent_lv sibling found!"))))
      "Goto prev parent_lv node"
      :enable t :exit t :map-inject t)
     ("M-<down>"
      (let ((_ (entropy/emacs-treemacs--goto-nearest-line)) (curpt (point)))
        (let (treemacs-pulse-on-failure)
          (treemacs-without-messages
           (treemacs-goto-parent-node) (treemacs-next-neighbour)))
        (and (<= (point) curpt) (goto-char curpt)
             (treemacs-pulse-on-failure "No next parent_lv sibling found!")))
      "Goto next parent_lv node"
      :enable t :exit t :map-inject t)
     ("C-M-<up>"  treemacs-move-project-up
      "Move project position up"
      :enable t :exit t :map-inject t)
     ("C-M-<down>" treemacs-move-project-down
      "Move project position down"
      :enable t :exit t :map-inject t))))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-show-cursor             t
        treemacs-width                   21
        treemacs-no-png-images           t)

  (setq
   treemacs-recenter-distance             0.2
   treemacs-recenter-after-tag-follow     'on-distance
   treemacs-recenter-after-file-follow    'on-distance
   treemacs-recenter-after-project-jump   'on-distance
   treemacs-recenter-after-project-expand 'on-distance
   )

  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (entropy/emacs-lazy-load-simple 'treemacs-file-management
    ;; shut down docstring width warnings
    (with-no-warnings
      (defhydra treemacs-bulk-file-actions-hydra
        ;; FIXME: this is a old hydra bug that invoke a ':exit t'
        ;; hydra from a ':exit nil' hydra probably made the transient
        ;; keymap can not be quit even though we hint the */quit key
        ;; defined in its keymap or C-g. Thus here we redefine it.
        (:exit nil :hint nil)
        ("m" #'treemacs-mark-or-unmark-path-at-point "(un)mark")
        ("u" #'treemacs-reset-marks "unmark all")
        ("s" #'treemacs-show-marked-files "show")
        ("d" #'treemacs-delete-marked-files "delete")
        ("c" #'treemacs-copy-marked-files "copy")
        ("o" #'treemacs-move-marked-files "move")
        ("q" nil "cancel")))
    ;; EEMACS_MAINTENANCE: enhance continuation usage of this hydra
    (dolist (el '(("<up>" . previous-line)
                  ("C-p" . previous-line)
                  ("<down>" . next-line)
                  ("C-n" . next-line)
                  ("<left>" . left-char)
                  ))
      (define-key treemacs-bulk-file-actions-hydra/keymap
                  (kbd (car el)) (cdr el))))

  ;; EEMACS_MAINTENANCE:
  ;; EEMACS_TEMPORALLY_HACK:
  ;; FIXME: https://github.com/Alexander-Miller/treemacs/issues/1118
  (defun entropy/emacs-treemacs--patch/apply-annotations-deferred (of &rest oargs)
    "temporarly fix issue #1118, as the treemacs bug that \='(wrong-type-argument number-or-marker-p nil)':
#+begin_quote
Since it's used as a idle timer ran after 0.5s (hard coded in
treemacs--create-branch) which wil be created fresh new one each time
treemacs--create-branch invoked, but its first arg btn is a marker
which may invisible at some time such as expand/hide repeatly
frequently speeder than the timer period, thus the inline function
treemacs-button-get can not get its depth but return nil where the
error occurred as for (let* ((depth (1+ (treemacs-button-get btn
:depth))) failed.
#+end_quote
"
    (condition-case err (apply of oargs)
      ((args-out-of-range wrong-type-argument)
       (let ((message-log-max nil))
         (with-temp-message "..." nil)))
      (t (error "%s" err))))
  (advice-add 'treemacs--apply-annotations-deferred
              :around #'entropy/emacs-treemacs--patch/apply-annotations-deferred)

  ;; made treemacs core subroutine pre-invoked eemacs treemacs init config
  (dolist (f '(treemacs-get-local-window
               treemacs-get-local-buffer))
    (advice-add f :before #'entropy/emacs-treemacs--init-advice))

  (entropy/emacs-add-hook-with-lambda
    'eemacs/treemacs--text-size-adjust (&rest _)
    "Made treemacs window exhibit things more wide based on small font
size.

Since TUI can not specified per-window text size, only GUI frame is
supported for now."
    :use-hook 'treemacs-mode-hook
    (and (display-graphic-p) (text-scale-decrease 2)))

  (defun entropy/emacs-treemacs--goto-nearest-line (&optional backward)
    (let ((op (if backward 'backward-char 'forward-char))
          (pop (if backward 'bobp 'eobp)))
      (while (and (not (funcall pop)) (not (treemacs-current-button)))
        (funcall op))))
  )

(use-package treemacs-nerd-icons
  :when (entropy/emacs-icons-displayable-p)
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config
  (progn (treemacs-load-theme "nerd-icons")
         (message "enable treemacs nerd-icons theme done")))

;; * Provide
(provide 'entropy-emacs-treemacs)
