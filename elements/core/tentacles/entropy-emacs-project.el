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

;; ** projectile
;; *** core
(use-package projectile
  :diminish
  :commands (projectile-find-file
             projectile-mode)
  :bind
  :init
  (setq
   ;; disable projectile mode-line indicator
   projectile-mode-line-prefix ""
   projectile-dynamic-mode-line nil
   ;; projectile sort spec
   projectile-sort-order 'recentf
   ;; disable projectile auto check git status for reduce lag
   projectile-use-git-grep nil
   ;; completion framework spec
   projectile-completion-system 'ivy
   ;; disable pre-setted keymap
   projectile-keymap-prefix nil
   ;; auto update detected projects and its status
   projectile-auto-update-cache t
   projectile-auto-discover t
   projectile-track-known-projects-automatically t)

  (entropy/emacs-lazy-initial-for-hook
   (entropy/emacs-after-startup-hook)
   "projectile-global-mode-init" "projectile-global-mode-init"
   prompt-echo
   :pdumper-no-end t
   (advice-add 'projectile-mode
               :after
               ;; FIXME:
               (progn
                 (defalias '__adv/after/projectile-mode/for-dired-before-readin-hook
                   (lambda
                     (&rest _)
                     "Advice for `projectile-mode' which has fatal of
using local hook injection to `dired-before-readin-hook' which
can not globally enable that hook and WHY?."
                     (cond
                      ((bound-and-true-p projectile-mode)
                       (remove-hook 'dired-before-readin-hook
                                    #'projectile-track-known-projects-find-file-hook t)
                       (add-hook 'dired-before-readin-hook
                                 #'projectile-track-known-projects-find-file-hook))
                      (t
                       (remove-hook 'dired-before-readin-hook
                                    #'projectile-track-known-projects-find-file-hook)))))
                 '__adv/after/projectile-mode/for-dired-before-readin-hook))
   (unless (bound-and-true-p counsel-projectile-mode)
     (counsel-projectile-mode t))
   (unless (bound-and-true-p projectile-mode)
     (projectile-mode t)))

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

;; *** counsel projectile
(use-package counsel-projectile
  :commands
  (counsel-projectile-mode
   counsel-projectile-switch-to-buffer
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

;; **** eemacs hydra hollow
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-after-startup-hook) :adtype hook :pdumper-no-end t))))
   ("Project"
    (("C-c p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'projectile-mode))
      "Projectile Map"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (entropy/emacs-after-startup-hook) :adtype hook :pdumper-no-end t)))
    (projectile-mode (projectile projectile-mode-map) nil (2)))
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

;; **** preface
  :preface
  (defun entropy/emacs-project--counsel-projectile-open-project-root-external
      (project-dir)
    "Open projet root dir use external explorer based on
`counsel-loate-action-extern'."
    (when (fboundp 'counsel-locate-action-extern)
      (counsel-locate-action-extern
       (expand-file-name project-dir))))

;; **** init
  :init
   ;; use ivy native matcher reduce lagging
   (setq counsel-projectile-find-file-matcher 'ivy--re-filter)
   ;; disable default counsel-projectile rebinds of
   ;; `projectile-mode-map'
   (setq counsel-projectile-key-bindings nil)

;; **** config
   :config

;; ***** extra ivy actions

  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("C-=" entropy/emacs-project--counsel-projectile-open-project-root-external
      "Open project root with external if available")))

;; ***** patches
;; ****** Make projetile candi predicates persisted
  (defmacro entropy/emacs-projectile--gen-candi-persist-rule (adv-for &optional remove)
    "Make projetile candi predicates persisted in the completion
framework to reduce lagging.

Optional arg REMOVE when non-nil, we remove all the patch by
previous set."
    (let ((loaded_var (intern (format "__cspadv_of_candi_persist_%s_loaded" adv-for)))
          (candi_var (intern (format "__cspadv_of_candi_persist_%s_canids" adv-for)))
          (adv_func (intern (format "__cspadv_of_candi_persist_adv/%s" adv-for)))
          (cancel_func (intern (format "__cspadv_of_candi_persist_reset/%s" adv-for)))
          (cspenv_p (lambda (&rest _)
                      ;; the predicate to ensure we should using
                      ;; persist candi caches. In this case we just
                      ;; using `window-minibuffer-p' to did thus
                      ;; because that if we in minibuffer to invoke
                      ;; such candi predicates that indeed we need
                      ;; persists.
                      (window-minibuffer-p))))
      `(if (not (null ',remove ))
           (progn
             (advice-remove ',adv-for #',adv_func)
             (dolist (el '(keyboard-quit ivy-done top-level))
               (advice-remove el
                              #',cancel_func))
             (dolist (el '(,loaded_var ,candi_var ,adv_func ,cancel_func))
               (unintern el)))
         (defvar ,loaded_var nil)
         (defvar ,candi_var nil)
         (defalias ',adv_func
           (lambda (orig-func &rest orig-args)
             (if (funcall #',cspenv_p)
                 (if (or (null ,candi_var)
                         (null ,loaded_var))
                     (setq ,loaded_var t
                           ,candi_var (apply orig-func orig-args))
                   ,candi_var)
               (apply orig-func orig-args))))
         (advice-add ',adv-for :around #',adv_func)
         (defalias ',cancel_func
           (lambda (&rest _)
             (setq ,candi_var nil
                   ,loaded_var nil)))
         (dolist (el '(keyboard-quit ivy-done top-level))
           (advice-add el
                       :before
                       #',cancel_func))
         )))

  ;; EEMACS_MAINTENANCE: persit common projectile list candi
  ;; predicates for reducing lag. These context may have some bug.
  (dolist (el '(counsel-projectile--project-buffers
                counsel-projectile--project-buffers-and-files
                counsel-projectile--project-directories))
    (eval
     `(entropy/emacs-projectile--gen-candi-persist-rule
       ,el)))

;; ***** simplify ivy transformer
  (defun counsel-projectile-find-file-transformer (str)
    "Transform non-visited file names with `ivy-virtual' face.

NOTE: this function has been redefined by eemacs to remove filter
condition to reduce lag since `projectile-expand-root' has low
benchmark while exhibits."
    (propertize str 'face 'ivy-virtual))

  )

;; ** entropy-project-manager
(use-package entropy-prjm
  :ensure nil
  :commands entropy/prjm-inct-chosen-db
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-after-startup-hook) :adtype hook :pdumper-no-end t))))
   ("Project"
    (("C-c M-p" entropy/prjm-inct-chosen-db "Eemacs Project Management"
      :enable t :exit t :global-bind t)))))

;; * provide
(provide 'entropy-emacs-project)
