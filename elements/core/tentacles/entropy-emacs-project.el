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

;; ** projectile
(use-package projectile
  :diminish
  :commands (projectile-find-file
             projectile-command-map)
  :bind 
  :init

  (entropy/emacs-lazy-with-load-trail
   projectile
   (setq projectile-mode-line-prefix ""
         projectile-sort-order 'recentf
         projectile-use-git-grep t
         projectile-completion-system 'ivy)
   
   (projectile-mode +1)

   (define-key projectile-mode-map (kbd "C-c p")
     #'projectile-command-map)
   )

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

;; ** entropy-project-manager
(use-package entropy-prjm
  :ensure nil
  :commands entropy/prjm-inct-chosen-db
  :bind ("C-c M-p" . entropy/prjm-inct-chosen-db))

;; * provide
(provide 'entropy-emacs-project)
