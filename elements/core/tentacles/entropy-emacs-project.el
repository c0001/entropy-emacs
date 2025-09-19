;;; entropy-emacs-project.el --- eemacs projects management configuration  -*- lexical-binding: t; -*-
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
;; ** project (emacs builtin)

(use-package project
  :ensure nil
  :config
  (defun __ya/project-current (fn &rest args)
    "Around advice for `project-current' only when its
MAYBE-PROMPT arg is nil/omitted since it didn't handle
permission-denied path properly."
    (if (car args) (apply fn args)
      (condition-case _ (apply fn args)
        (permission-denied nil))))
  (advice-add 'project-current :around #'__ya/project-current)

  (defun eemacs//project-find-file-hook nil
    "Remember current project with project.el if it is a project."
    (when-let* ((prj (project-current)))
      (run-with-idle-timer
       0.2 nil
       (lambda nil
         (entropy/emacs-message-simple-progress-message
             (format "eemacs project: remember project %s" prj)
           :with-temp-message t
           (project-remember-project prj))))))
  (dolist (hook (list 'dired-mode-hook 'find-file-hook))
    (add-hook hook 'eemacs//project-find-file-hook))

  )

;; ** hydra hollows

(defvar entropy/emacs-ivy-counsel-git-only-list-dir)
(defun entropy/emacs-project-find-project-file (&optional find-dir)
  (interactive (list nil))
  (let* ((prj (project-current))
         (prj-root (and prj (project-root prj)))
         (prj-git-p (and prj-root (file-exists-p (expand-file-name ".git" prj-root)))))
    (if (and prj-git-p
             (eq entropy/emacs-command-completion-use-style 'ivy)
             (fboundp 'counsel-git))
        (let ((entropy/emacs-ivy-counsel-git-only-list-dir (and find-dir t)))
          (call-interactively 'counsel-git))
      (if find-dir
          (call-interactively 'project-find-file)
        (call-interactively 'project-find-dir)))))

(entropy/emacs-lazy-with-load-trail 'eemacs//project-hydra-hollow-init
  :pdumper-no-end t
  (entropy/emacs-hydra-hollow-add-for-top-dispatch
   '("Project"
     (("C-c p"
       (:eval
        (entropy/emacs-hydra-hollow-category-common-individual-get-caller
         'project-mode))
       "Project Key Map"
       :enable t :exit t))))
  (entropy/emacs-hydra-hollow-common-individual-hydra-define
   'project-mode nil
   '("Project Switch/Add/Remove"
     (("C-c p p p" project-switch-project
       "Switch To Other Project"
       :enable t :exit t :eemacs-top-bind t)
      ("C-c p p d" project-remember-projects-under
       "Discover any projects in directory"
       :enable t :exit t :eemacs-top-bind t)
      ("C-c p p r" project-forget-project
       "Remove directory from the project list"
       :enable t :exit t :eemacs-top-bind t)
      ("C-c p p R" project-forget-projects-under
       "Forget all known projects below a directory"
       :enable t :exit t :eemacs-top-bind t))
     "Project Filter Open"
     (("C-c g" entropy/emacs-project-find-project-file "Jump to a file in the current project"
       :enable t :exit t :global-bind t)
      ("C-c p f d"
       (progn
         (setq this-command 'entropy/emacs-project-find-project-file)
         (entropy/emacs-project-find-project-file t))
       "Jump to a dir in the current project"
       :enable t :exit t :eemacs-top-bind t)
      ("C-c p f l" project-list-buffers
       "List all opened buffers in the current project"
       :enable t :exit t :eemacs-top-bind t))
     "Project Powerful Search"
     (("C-c p s a"
       (if-let* ((hh (entropy/emacs-hydra-hollow-category-common-individual-get-caller
                      'powerful-searcher)))
           (call-interactively hh)
         (user-error "powerful searcher facilities not found yet."))
       "Search the current project with eemacs powerful searcher"
       :enable t :exit t :eemacs-top-bind t)))
   nil '(2 2 2)))

;; * provide
(provide 'entropy-emacs-project)
