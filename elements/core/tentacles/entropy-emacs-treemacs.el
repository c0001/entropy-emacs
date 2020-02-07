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
         ("C-x 1"     . treemacs-delete-other-windows)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
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

  :init
  (global-set-key (kbd "<f8>") #'entropy/emacs-treemacs-toggle-treemacs)
  (global-set-key (kbd "<C-f8>") #'treemacs)

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
  
  :config
  (advice-add 'treemacs--init :around #'entropy/emacs-treemacs--unwind-for-init)
  
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init       t
        ;; treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   36
        treemacs-show-cursor             t
        treemacs-recenter-after-file-follow    t
        treemacs-recenter-after-project-expand t
        treemacs-recenter-after-project-jump   t
        treemacs-recenter-after-tag-follow     t
        treemacs-show-edit-workspace-help t)

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
