;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Modified by: Entropy
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Version control systems, e.g. Git, SVN.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** main
;; *** Git
;; **** magit
(use-package magit
  :if (executable-find "git")
  :commands
  (magit-status
   magit-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  (when (and sys/win32p entropy/emacs-git-portable)
    (setq magit-git-executable (concat entropy/emacs-git-portable-path "git.exe")))

  ;; Force using utf-8 environment to prevent causing unicode problem in git commit.
  (when entropy/emacs-custom-language-environment-enable
    (with-eval-after-load 'magit)
    (advice-add 'magit-status :before #'entropy/lang-set-utf-8)
    (advice-add 'magit-dispatch-popup :before #'entropy/lang-set-utf-8)
    (advice-add 'magit-file-popup :before #'entropy/lang-set-utf-8))

  ;; Disabled vc.el key bindings for prevent to accidental actived 
  (progn
    (define-key global-map (kbd "C-x v =") nil)
    (define-key global-map (kbd "C-x v +") nil)
    (define-key global-map (kbd "C-x v D") nil)
    (define-key global-map (kbd "C-x v G") nil)
    (define-key global-map (kbd "C-x v I") nil)
    (define-key global-map (kbd "C-x v L") nil)
    (define-key global-map (kbd "C-x v O") nil)
    (define-key global-map (kbd "C-x v P") nil)
    (define-key global-map (kbd "C-x v a") nil)
    (define-key global-map (kbd "C-x v b") nil)
    (define-key global-map (kbd "C-x v d") nil)
    (define-key global-map (kbd "C-x v g") nil)
    (define-key global-map (kbd "C-x v h") nil)
    (define-key global-map (kbd "C-x v j") nil)
    (define-key global-map (kbd "C-x v l") nil)
    (define-key global-map (kbd "C-x v m") nil)
    (define-key global-map (kbd "C-x v u") nil)
    (define-key global-map (kbd "C-x v v") nil)
    (define-key global-map (kbd "C-x v x") nil)
    (define-key global-map (kbd "C-x v ~") nil)
    (define-key global-map (kbd "C-x v i") nil))
  
  :config
  (with-eval-after-load 'magit
    (define-key magit-mode-map (kbd "M-1") nil)) ;disabled 'M-1' key-binding with conflicated with
                                                 ;`entropy/quick-readonly-global'
  (use-package ssh-agency
    :commands (ssh-agency-ensure)
    :init
    (with-eval-after-load 'magit
      (add-hook 'magit-credential-hook 'ssh-agency-ensure))
    :config
    (setenv "SSH_ASKPASS" "git-gui--askpass")))

;; **** magit-popup toolchain

(use-package magit-files
  :if (executable-find "git")
  :ensure nil
  :bind (("C-c M-g" . magit-file-popup))
  :commands
  (magit-file-popup))

(use-package magit-popup
  :if (executable-find "git")
  :commands
  (magit-define-popup-keys-deferred)
  :config
  (require 'magit)
  ;; The newest (20190214.2056) [[2019-02-15 Fri 23:07:07]] magit
  ;; doesn't provide this function, defined it from older magit
  ;; version (20190202.1535).
  (magit-define-popup magit-file-popup
    "Popup console for Magit commands in file-visiting buffers."
    :actions '((?s "Stage"     magit-stage-file)
               (?D "Diff..."   magit-diff-buffer-file-popup)
               (?L "Log..."    magit-log-buffer-file-popup)
               (?B "Blame..."  magit-blame-popup) nil
               (?u "Unstage"   magit-unstage-file)
               (?d "Diff"      magit-diff-buffer-file)
               (?l "Log"       magit-log-buffer-file)
               (?b "Blame"     magit-blame-addition)
               (?p "Prev blob" magit-blob-previous)
               (?c "Commit"    magit-commit-popup) nil
               (?t "Trace"     magit-log-trace-definition)
               (?r (lambda ()
                     (with-current-buffer magit-pre-popup-buffer
                       (and (not buffer-file-name)
                            (propertize "...removal" 'face 'default))))
                   magit-blame-removal)
               (?n "Next blob" magit-blob-next)
               (?e "Edit line" magit-edit-line-commit)
               nil nil
               (?f (lambda ()
                     (with-current-buffer magit-pre-popup-buffer
                       (and (not buffer-file-name)
                            (propertize "...reverse" 'face 'default))))
                   magit-blame-reverse)
               nil)
    :max-action-columns 5))

;; **** Gitflow externsion for Magit
(use-package magit-gitflow
  :diminish magit-gitflow-mode
  :commands (magit-gitflow-popup
             turn-on-magit-gitflow)
  :bind (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?G "GitFlow" #'magit-gitflow-popup ?!))


;; **** Git-Svn extension for Magit
(use-package magit-svn
  :diminish magit-svn-mode
  :commands (magit-svn-mode)
  :init (add-hook 'magit-mode-hook #'magit-svn-mode))
  

;; **** Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; **** Walk through git revisions of a file
(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-switch-branch
   git-timemachine-toggle))

;; **** Highlighting regions by last updated time
(use-package smeargle
  :commands (smeargle smeargle-commits smeargle-clear)
  :bind (("C-x v s" . smeargle)
         ("C-x v c" . smeargle-commits)
         ("C-x v r" . smeargle-clear)))

;; **** Git major modes
(use-package gitattributes-mode
  :commands gitattributes-mode
  :init
  (dolist (pattern '("/\\.gitattributes\\'" "/info/attributes\\'" "/git/attributes\\'"))
    (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode))))

(use-package gitconfig-mode
  :commands gitconfig-mode
  :init
  (dolist (pattern
           '("/\\.gitconfig\\'"
             "/\\.git/config\\'"
             "/modules/.*/config\\'"
             "/git/config\\'"
             "/\\.gitmodules\\'"
             "/etc/gitconfig\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode))))

(use-package gitignore-mode
  :commands gitignore-mode
  :init
  (dolist (pattern (list "/\\.gitignore\\'" "/info/exclude\\'" "/git/ignore\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode))))


;; **** Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :commands (browse-at-remote))

(provide 'entropy-emacs-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
