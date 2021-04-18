;;; entropy-emacs-vcs.el --- entropy-emacs version contol system configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-vcs.el
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
;; `entropy-emacs' version control system emacs client configuration.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs'.
;;
;; * Code:

;; ** require

;; ** main
;; *** Git
;; **** magit
(use-package magit
  :if (executable-find "git")
  :commands
  (magit-status
   magit-dispatch)
  :eemacs-tpha
  (((:enable t))
   ("Vcs"
    (("C-x g" magit-status "Show the status of the current Git repository in a buffer"
      :enable t :exit t :global-bind t)
     ("C-x M-g" magit-dispatch "Invoke a Magit command from a list of available commands"
      :enable t :exit t :global-bind t))))
  :preface
  (defun entropy/emacs-vcs--magit-init ()
    (require 'magit)
    ;; pre generate `magit-dispatch' object (used quickly)
    (transient--init-objects 'magit-dispatch nil nil)
    ;; Force using utf-8 environment to prevent causing unicode problem in git commit.
    (progn
      (advice-add 'magit-status :around #'entropy/emacs-lang-use-utf-8-ces-around-advice)
      (advice-add 'magit-dispatch-popup :around #'entropy/emacs-lang-use-utf-8-ces-around-advice)
      (advice-add 'magit-file-popup :around #'entropy/emacs-lang-use-utf-8-ces-around-advice))
    ;;disabled 'M-1' key-binding with conflicated with
    ;;`entropy/emacs-quick-readonly-global'
    (progn
      (define-key magit-mode-map (kbd "M-1") nil)
      (define-key magit-mode-map (kbd "M-0") #'magit-section-show-level-1-all)))

  :init
  (entropy/emacs-lazy-with-load-trail
   magit-init
   :pdumper-no-end t
   :body
   (entropy/emacs-vcs--magit-init))

  ;; Disabled vc.el key bindings for prevent to accidental activation
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
    (define-key global-map (kbd "C-x v i") nil)))

(use-package ssh-agency
  :commands (ssh-agency-ensure)
  :init
  (entropy/emacs-lazy-load-simple magit
    (add-hook 'magit-credential-hook 'ssh-agency-ensure))
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; **** Git-Svn extension for Magit
(use-package magit-svn
  :diminish magit-svn-mode
  :commands (magit-svn-mode)
  :init (add-hook 'magit-mode-hook #'magit-svn-mode))


;; **** Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :eemacs-tpha
  (((:enable t))
   ("Vcs"
    (("C-x v p" git-messenger:popup-message "Git Messenger"
      :enable t :exit t :global-bind t))))

  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)

  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun entropy/emacs-vcs--git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             (propertize message 'face 'warning)
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun entropy/emacs-vcs--git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (or (git-messenger:find-vcs) (user-error "Not in vcs folder!")))
             (file (or (buffer-file-name (buffer-base-buffer)) (user-error "Not a vcs file! ")))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message
              (if (git-messenger:show-detail-p commit-id)
                  (entropy/emacs-vcs--git-messenger:format-detail vcs commit-id author msg)
                (cl-case vcs
                  (git msg)
                  (svn (if (string= commit-id "-")
                           msg
                         (git-messenger:svn-message msg)))
                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                :internal-border-color (face-foreground 'default)
                                :internal-border-width 1)
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override
                #'entropy/emacs-vcs--git-messenger:popup-message)))

;; **** Walk through git revisions of a file
(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-switch-branch
   git-timemachine-toggle)
  :eemacs-indhc
  (((:enable t)
    (git-timemachine))
   ("Common histroy"
    (("t" git-timemachine "view history" :enable t :exit t)
     ("?" git-timemachine-help "Show git-timemachine help (use in thus mode enable)"
      :enable t :exit t))
    "Branch history"
    (("b" git-timemachine-switch-branch "view another branch history"
      :enable t :exit t))
    "Toggle"
    (("r" git-timemachine-toggle "Quit or re-inject timemachine"
      :enable t :exit t))))
  :eemacs-tpha
  (((:enable t))
   ("Vcs"
    (("C-x v t"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'git-timemachine))
      "git timemachine" :enable t :global-bind t :exit t)))))

;; **** Highlighting regions by last updated time
(use-package smeargle
  :commands (smeargle smeargle-commits smeargle-clear)
  :eemacs-tpha
  (((:enable t))
   ("Vcs"
    (("C-x v s" smeargle "Highlight regions by last updated time"
      :enable t :exit t :global-bind t)
     ("C-x v c" smeargle-commits "Highlight regions by age of commits"
      :enable t :exit t :global-bind t)
     ("C-x v r" smeargle-clear "Clear smeargle overlays in current buffer"
      :enable t :exit t :global-bind t)))))

;; **** Smerge mode (vcs merge tool)

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish t
  :eemacs-indhc
  (((:enable t)
    (smerge-mode))
   ("Move"
    (("n" smerge-next "next"
      :enable t :map-inject t)
     ("p" smerge-prev "previous"
      :enable t :map-inject t))
    "Keep"
    (("b" smerge-keep-base "base"
      :enable t :map-inject t)
     ("u" smerge-keep-upper "upper"
      :enable t :map-inject t)
     ("l" smerge-keep-lower "lower"
      :enable t :map-inject t)
     ("a" smerge-keep-all "all"
      :enable t :map-inject t)
     ("RET" smerge-keep-current "current"
      :enable t :map-inject t)
     ("C-m" smerge-keep-current "current"
      :enable t :map-inject t))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base"
      :enable t :map-inject t)
     ("=" smerge-diff-upper-lower "upper/lower"
      :enable t :map-inject t)
     (">" smerge-diff-base-lower "upper/lower"
      :enable t :map-inject t)
     ("R" smerge-refine "refine"
      :enable t :map-inject t)
     ("E" smerge-ediff "ediff"
      :enable t :map-inject t))
    "Other"
    (("C" smerge-combine-with-next "combine"
      :enable t :map-inject t)
     ("r" smerge-resolve "resolve"
      :enable t :map-inject t)
     ("k" smerge-kill-current "kill"
      :enable t :map-inject t)
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer"
      :enable t :map-inject t :exit t))))
  :eemacs-tpha
  (((:enable t))
   ("Vcs"
    (("C-x v m"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'smerge-mode))
      "Merge diffing"
      :enable t :global-bind t :exit t))))
  :preface
  (defun entropy/emacs-vcs--smerge-auto-open-for-buffer ()
    (require 'smerge-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))

  (defun entropy/emacs-vcs--smerge-hydra-popup ()
    (require 'smerge-mode)
    (when smerge-mode
      (funcall
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'smerge-mode))))

  :hook ((find-file . entropy/emacs-vcs--smerge-auto-open-for-buffer)
         (magit-diff-visit-file
          .
          entropy/emacs-vcs--smerge-hydra-popup)))

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
