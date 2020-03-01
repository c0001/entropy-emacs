;;; entropy-emacs-project.el --- eemacs projects management configuration
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Created:       2020-02-08 16:49:08
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
;; Projects management
;;
;; * Configuration:
;;
;; For eemacs specification only, no warranty for others.
;;
;; * Code:

;; ** require

(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-hydra-hollow)

;; ** projectile
(use-package projectile
  :diminish
  :commands (projectile-find-file
             projectile-command-map)
  :bind

  :eemacs-tpha
  (((:enable t))
   ("Project"
    (("C-c p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'projectile-mode))
      "Projectile Map"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t)
    (projectile-mode projectile projectile-mode-map nil (2)))
   ("projectile Switch"
    (("C-c p p p" counsel-projectile-switch-project "Switch To Other Project"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p p q" projectile-switch-open-project "Switch to a project we have currently opened"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p p d" projectile-discover-projects-in-directory "Discover any projects in directory"
      :enable t :exit t :eemacs-top-bind t))
    "Projectile Filter Open"
    (("C-c p f f" counsel-projectile-find-file "Jump to a file in the current project"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p f o" counsel-projectile-find-file-dwim "Jump to a file in the current project using completion based on context"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p f I" projectile-ibuffer "Open an IBuffer window showing all buffers in the current project"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p f d" counsel-projectile-find-dir "Jump to a directory in the current project"
      :enable t :exit t :eemacs-top-bind t))
    "Projectile Search"
    (("C-c p s a" counsel-projectile-ag "Search the current project with ag"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p s r" counsel-projectile-rg "Search the current project with rg"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p s a" counsel-projectile-grep "Search the current project with grep"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c p s g" counsel-projectile-git-grep "Search the current project with git grep"
      :enable t :exit t :eemacs-top-bind t))))

  :init

  (entropy/emacs-lazy-with-load-trail
   projectile
   (setq projectile-mode-line-prefix ""
         projectile-sort-order 'recentf
         projectile-use-git-grep t
         projectile-completion-system 'ivy
         projectile-keymap-prefix nil)
   (projectile-mode +1))

  :config

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))


(use-package counsel-projectile
  :after projectile
  :commands
  (counsel-projectile-switch-to-buffer
   counsel-projectile-switch-project
   counsel-projectile-find-dir
   counsel-projectile-git-grep
   counsel-projectile-mode
   counsel-projectile-find-file
   counsel-projectile-org-capture
   counsel-projectile-find-file-dwim
   counsel-projectile-ag
   counsel-projectile
   counsel-projectile-rg
   counsel-projectile-org-agenda
   counsel-projectile-grep)
  :init
  (entropy/emacs-lazy-with-load-trail
   counsel-projectile
   (counsel-projectile-mode +1)))

;; ** entropy-project-manager
(use-package entropy-prjm
  :ensure nil
  :commands entropy/prjm-inct-chosen-db
  :eemacs-tpha
  (((:enable t))
   ("Project"
    (("C-c M-p" entropy/prjm-inct-chosen-db "Eemacs Project Management"
      :enable t :exit t :global-bind t)))))

;; * provide
(provide 'entropy-emacs-project)
