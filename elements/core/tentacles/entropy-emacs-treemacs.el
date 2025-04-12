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

(declare treemacs--themes)
(defun entropy/emacs-treemacs-toggle-show-hide/inct nil
  (interactive)
  (when (entropy/emacs-icons-displayable-p)
    (entropy/emacs-require-only-once 'treemacs-nerd-icons))
  (call-interactively 'treemacs))

(use-package treemacs
  :commands (treemacs)
  :bind (("<f8>" . entropy/emacs-treemacs-toggle-show-hide/inct))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-show-cursor             t
        treemacs-width                   25
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
