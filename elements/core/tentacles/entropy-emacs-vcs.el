;;; entropy-emacs-vcs.el --- entropy-emacs version contol system configuration  -*- lexical-binding: t; -*-
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
  (((:enable t :defer (:data (:adfors (find-file-hook) :adtype hook :pdumper-no-end t)
                             (:adfors (switch-to-buffer) :adtype after :pdumper-no-end t))))
   ("Vcs"
    (("C-x g" magit-status "Show the status of the current Git repository in a buffer"
      :enable t :exit t :global-bind t)
     ("C-x M-g" magit-dispatch "Invoke a Magit command from a list of available commands"
      :enable t :exit t :global-bind t)
     ("C-c M-g" magit-file-dispatch "Invoke a Magit command that acts on the visited file"
      :enable t :exit t :global-bind t))))
  :preface
  (defun entropy/emacs-vcs--magit-init ()
    (with-eval-after-load 'magit
      ;; Force using utf-8 environment to prevent causing unicode problem in git commit.
      (progn
        (advice-add 'magit-status
                    :around
                    #'entropy/emacs-lang-use-utf-8-ces-around-advice))
      ;;disabled 'M-1' key-binding with conflicated with
      ;;`entropy/emacs-quick-readonly-global'
      (progn
        ;; Disable 'M-1' binding for `magit-mode-map' since we bind it
        ;; to `entropy/grom-quick-readonly-global' globally
        (define-key magit-mode-map (kbd "M-1") nil)
        (define-key magit-mode-map (kbd "M-0")
          #'magit-section-show-level-1-all)
        (define-key magit-mode-map (kbd "<M-up>")
          #'magit-section-backward-sibling)
        (define-key magit-mode-map (kbd "<M-down>")
          #'magit-section-forward-sibling))))

  :init
  (entropy/emacs-lazy-with-load-trail
   magit-init
   :pdumper-no-end t
   :body
   ;; preferred to use `magit-previous-window-configuration' to quit
   ;; windows
   (setq magit-bury-buffer-function
         'magit-restore-window-configuration)
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
    (define-key global-map (kbd "C-x v i") nil))

  :config
  ;; windows wsl2 using `pinentry-emacs' as default gpg-agent passphrase
  (when sys/wsl2-env-p
    (unless (pinentry-emacs-gpg-agent-conf-patched-p)
      (pinentry-emacs-gpg-agent-conf-patch-func)))

  )

(use-package ssh-agency
  ;; Windows only spec
  :if sys/win32p
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
  :preface

  ;; FIXME: we must declare this macro firstly before use-package
  ;; defer load the patch, otherwise, this macro will be recognized as
  ;; an void function and why?
  (defmacro entropy/emacs-vcs--git-messenger:popup-message/unwind-protect
      (&rest body)
    "The input waiting protect for
`entropy/emacs-vcs--git-messenger:popup-message' of BODY."
    `(let ((|||hydra-need-quit___|$ t))
       (unwind-protect
           (progn
             (push (read-event) unread-command-events)
             ;; when hint C-g we must exit the hydra also.
             (when (eq (car unread-command-events) 7)
               (git-messenger-hydra/lambda-\,-and-exit))
             (setq |||hydra-need-quit___|$ nil))
         ,@body
         (when |||hydra-need-quit___|$
           (hydra-keyboard-quit)))))

  :commands git-messenger:copy-message
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (find-file-hook) :adtype hook :pdumper-no-end t)
                             (:adfors (switch-to-buffer) :adtype after :pdumper-no-end t))))
   ("Vcs"
    (("C-x v p" git-messenger:popup-message "Git Messenger"
      :enable t :exit t :global-bind t))))

  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)

  :config

;; ***** patches
  (entropy/emacs-lazy-load-simple hydra
    (defhydra git-messenger-hydra (:color blue)
      ("s" git-messenger:popup-show "show")
      ("c" git-messenger:copy-commit-id "copy hash")
      ("m" git-messenger:copy-message "copy message")
      ("," (progn (catch 'git-messenger-loop (__eemacs/git-messenger:show-parent))
                  (entropy/emacs-vcs--git-messenger:popup-message t))
       "go parent")
      ("q" git-messenger:popup-close "quit")))

  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/git-messenger:popup-common (orig-func &rest orig-args)
    "Like `git-messenger:popup-common' but patch its `magit' feature
part to fix bug of wrong of start `view-mode' which cover the
magit-diff features keybinding."
    (let ((vcs (car orig-args))
          (args (cadr orig-args))
          (mode (caddr orig-args)))
      (if (eq vcs 'git)
          (progn
            (with-current-buffer (get-buffer-create "*git-messenger*")
              (view-mode -1)
              (fundamental-mode)
              (erase-buffer)
              (unless (zerop (git-messenger:execute-command vcs args t))
                (error "Failed: '%s(args=%s)'" (git-messenger:vcs-command vcs) args))
              (if git-messenger:use-magit-popup
                  (magit-show-commit git-messenger:last-commit-id)
                (pop-to-buffer (current-buffer))
                (when mode
                  (funcall mode)
                  (view-mode +1)))
              (run-hooks 'git-messenger:popup-buffer-hook)
              (goto-char (point-min)))
            (git-messenger:popup-close))
        (apply orig-func orig-args))))
  (advice-add 'git-messenger:popup-common
              :around #'__ya/git-messenger:popup-common)

  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/git-messenger:blame-arguments (vcs file line &optional commit-id)
    "Like `git-messenger:blame-arguments' but support base on
COMMIT-ID when vcs is 'git'."
    (let* ((basename (file-name-nondirectory file)))
      (if (eq vcs 'git)
          (delete nil (list "--no-pager" "blame" "-w" "-L"
                            (format "%d,+1" line)
                            "--porcelain"
                            commit-id
                            basename))
        (git-messenger:blame-arguments vcs file line))))

  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/git-messenger:commit-info-at-line (vcs file line &optional commit-id)
    "Like `git-messenger:blame-arguments' but support base on
COMMIT-ID when vcs is 'git'."
    (with-temp-buffer
      (let ((args (__ya/git-messenger:blame-arguments vcs file line commit-id)))
        (unless (zerop (git-messenger:execute-command vcs args t))
          (error "Failed: '%s blame (%s)'" (git-messenger:vcs-command vcs)
                 (buffer-substring-no-properties
                  (point-min) (point-max))))
        (goto-char (point-min))
        (cl-case vcs
          (git (git-messenger:git-commit-info-at-line))
          (t
           (git-messenger:commit-info-at-line
            vcs file line))))))

  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __eemacs/git-messenger:git-commit-arguments (commit-id)
    "Follow same return type as `git-messenger:blame-arguments' but
used for 'git' only."
    (let ((git-cmd-args
           (list "--no-pager"
                 "show"
                 commit-id)))
      (with-temp-buffer
        (git-messenger:execute-command
         'git
         git-cmd-args t)
        (goto-char (point-min))
        (if (re-search-forward
             "^Author: \\(.+\\)$" nil t)
            (cons commit-id (match-string-no-properties 1))
          (error "can not get author at commit: %s (%s)"
                 commit-id (buffer-substring-no-properties
                            (point-min) (point-max)))))))

  (defvar __ya/git-messenger:show-parent-process-output-log nil)
  (defvar __ya/git-messenger:show-parent-next-git-commit-id nil)
  (defvar __ya/git-messenger:show-parent-next-git-commit-id-1 nil)
  (defvar __ya/git-messenger:show-parent/git/current-calling-depth 0)
  (defvar __ya/git-messenger:show-parent/current-blame-arguments nil)
  (defun __eemacs/git-messenger:git-query-parent-proc-call
      (commit-id file buffer &optional line)
    "git blame for LINE of FILE based on COMMIT-ID, output to
BUFFER with `erase-buffer' firstly. Return an plist of:

#+begin_src emacs-lisp :tangle yes
(list :quit-status process-quit-status
      :git-args git-cmd-args)
#+end_src
"
    (let* ((git-cmd-args
            (delete nil
                    (list "blame"
                          ;; "-w"
                          (when line "-L")
                          (when line (format "%d,+1" line))
                          "--increment"
                          commit-id
                          "--" file)))
           (process-quit-status 0)
           (process-call-func
            (lambda ()
              (setq process-quit-status
                    (apply 'process-file "git" nil t nil
                           git-cmd-args))))
           blame-get-commit-id
           prev-commit-id-of-blame-get-commit-id
           rtn)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (funcall process-call-func))
        (when (zerop process-quit-status)
          (goto-char (point-min))
          (if (looking-at "\\([0-9a-z]+\\) ")
              (progn
                (setq blame-get-commit-id (match-string-no-properties 1))
                (when (= 40 (length blame-get-commit-id))
                  (when (re-search-forward "previous \\(\\S-+\\)" nil t)
                    (setq prev-commit-id-of-blame-get-commit-id
                          (match-string-no-properties 1))
                    (cond (
                           ;; judge whether the blamed returned commit
                           ;; id is same as the query one i.e. the
                           ;; current commit is the final changes
                           ;; which applied on this line so we need to
                           ;; check its previous commit as the blame
                           ;; to run blame again.
                           (string= blame-get-commit-id commit-id)
                           (setq rtn
                                 (__eemacs/git-messenger:git-query-parent-proc-call
                                  prev-commit-id-of-blame-get-commit-id file buffer line)))
                          (t
                           (setq
                            ;; ----
                            git-messenger:last-commit-id
                            blame-get-commit-id
                            ;; ----
                            git-messenger:last-message
                            (git-messenger:commit-message
                             'git blame-get-commit-id)
                            ;; ----
                            __ya/git-messenger:show-parent/current-blame-arguments
                            (__eemacs/git-messenger:git-commit-arguments
                             blame-get-commit-id)
                            ;; ----
                            __ya/git-messenger:show-parent-next-git-commit-id
                            prev-commit-id-of-blame-get-commit-id))))))
            (setq process-quit-status 1))))
      (unless rtn
        (setq rtn
              (list :quit-status process-quit-status
                    :git-args git-cmd-args)))
      rtn))

  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __eemacs/git-messenger:show-parent ()
    "Like `git-messenger:show-parent' but using eemacs spec process
for 'git' type and fallback to it while other types."
    (interactive)
    (when (string-match-p "^0+$" git-messenger:last-commit-id)
      (message "Can not show parent blame at an non-commit block")
      (throw 'git-messenger-loop nil))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos nil t))
           (process-succeed-p nil)
           (process-output ""))
      (cl-case git-messenger:vcs
        (git
         (let* ((query-commit-id
                 (or __ya/git-messenger:show-parent-next-git-commit-id
                     git-messenger:last-commit-id
                     (error "git-messenger:last-commit-id is emtpy")))
                proc-cbk
                (istop-warn-func
                 (lambda ()
                   (when (string= (or __ya/git-messenger:show-parent-next-git-commit-id
                                      "")
                                  (or __ya/git-messenger:show-parent-next-git-commit-id-1
                                      ""))
                     (progn
                       (message "Current commit %s is the topest parent!"
                                git-messenger:last-commit-id)
                       (throw 'git-messenger-loop nil))))))
           (setq
            __ya/git-messenger:show-parent-next-git-commit-id-1
            __ya/git-messenger:show-parent-next-git-commit-id)

           (with-temp-buffer
             (setq proc-cbk
                   (__eemacs/git-messenger:git-query-parent-proc-call
                    query-commit-id file (current-buffer) line))
             (setq process-succeed-p
                   (zerop (plist-get proc-cbk :quit-status))
                   process-output
                   (buffer-substring-no-properties
                    (point-min) (point-max)))

             ;; filter the process outpu
             (with-temp-buffer
               (insert process-output)
               ;; remove final newline
               (goto-char (point-max))
               (when (looking-at "^$")
                 (delete-char -1))
               ;; ---------
               (setq process-output
                     (buffer-substring-no-properties
                      (point-min) (point-max))))

             ;; reduce log size
             (when (> (length __ya/git-messenger:show-parent-process-output-log) 20)
               (setq __ya/git-messenger:show-parent-process-output-log
                     (butlast __ya/git-messenger:show-parent-process-output-log
                              (- (length __ya/git-messenger:show-parent-process-output-log)
                                 20))))
             ;; put the log
             (push (list
                    :file file
                    :cmd-args (plist-get proc-cbk :git-args)
                    :msg
                    process-output)
                   __ya/git-messenger:show-parent-process-output-log)
             (if process-succeed-p
                 (cl-incf __ya/git-messenger:show-parent/git/current-calling-depth)
               (if (zerop __ya/git-messenger:show-parent/git/current-calling-depth)
                   ;; The top level parent query fatal since current
                   ;; commit is the pioneer commit on this line yet!
                   ;; and we do not quit the popuped interface since
                   ;; its not an fatal.
                   (message "Current commit %s is the topest parent!"
                            git-messenger:last-commit-id)
                 ;; miscellaneous error
                 (user-error "No parent commit ID: <%s>"
                             process-output)))
             (funcall istop-warn-func)
             (throw 'git-messenger-loop nil))))
        (otherwise
         (git-messenger:show-parent)))))

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

  (defvar entropy/emacs-vcs--git-messenger:popup-message-old-message nil)
  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun entropy/emacs-vcs--git-messenger:popup-message
      (&optional calling-from-increament-command)
    "Like `git-messenger:popup-message' but popuped with `posframe',
`pos-tip', `lv' or `message', and dispatch actions with `hydra'."
    (interactive)
    (let* ((msg-buffer-name "*git-messenger*")
           (cur-defdir default-directory)
           msg-set-func
           msg-pretty-func
           popuped-message
           (cur-vcs (or (git-messenger:find-vcs)
                        (user-error "Not in vcs folder!"))))
      (when (get-buffer msg-buffer-name)
        ;; NOTE: we must kill the msg buffer when it
        ;; `default-directory' is not as the calling place since it
        ;; will cause spawn git/svn.. fatal of calling at wrong env.
        (with-current-buffer msg-buffer-name
          (unless (file-equal-p cur-defdir default-directory)
            (kill-buffer))))
      (unless calling-from-increament-command
        (setq __ya/git-messenger:show-parent-next-git-commit-id nil
              __ya/git-messenger:show-parent/current-blame-arguments nil
              entropy/emacs-vcs--git-messenger:popup-message-old-message nil
              __ya/git-messenger:show-parent/git/current-calling-depth 0))
      (setq msg-pretty-func
            (lambda (msg vcs commit-id author)
              ;; remove gpg signature header
              (with-temp-buffer
                (let ((inhibit-read-only t))
                  (insert msg)
                  (goto-char (point-min))
                  (save-match-data
                    (when (re-search-forward "-----END PGP SIGNATURE-----" nil t)
                      (progn
                        (delete-region (point-min) (point)))))
                  (setq msg (buffer-substring (point-min) (point-max)))))
              (if (git-messenger:show-detail-p commit-id)
                  (entropy/emacs-vcs--git-messenger:format-detail vcs commit-id author msg)
                (cl-case vcs
                  (git msg)
                  (svn (if (string= commit-id "-")
                           msg
                         (git-messenger:svn-message msg)))
                  (hg msg)))))
      (setq msg-set-func
            (lambda ()
              (let* ((vcs cur-vcs)
                     (file (or (buffer-file-name (buffer-base-buffer)) (user-error "Not a vcs file! ")))
                     (line (line-number-at-pos nil t))
                     (commit-info (git-messenger:commit-info-at-line vcs file line))
                     (commit-id (car commit-info))
                     (author (cdr commit-info))
                     (msg (git-messenger:commit-message vcs commit-id))
                     (rtn
                      (funcall msg-pretty-func msg vcs commit-id author)))
                (setq git-messenger:vcs vcs
                      git-messenger:last-message msg
                      git-messenger:last-commit-id commit-id)
                rtn)))
      (if (and calling-from-increament-command
               __ya/git-messenger:show-parent/current-blame-arguments)
          (setq popuped-message
                (funcall msg-pretty-func
                         git-messenger:last-message
                         git-messenger:vcs
                         git-messenger:last-commit-id
                         (cdr __ya/git-messenger:show-parent/current-blame-arguments)))
        (setq popuped-message (or
                               entropy/emacs-vcs--git-messenger:popup-message-old-message
                               (funcall msg-set-func))
              entropy/emacs-vcs--git-messenger:popup-message-old-message
              popuped-message))
      (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
      (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
             (git-messenger-hydra/body)
             (posframe-show msg-buffer-name
                            :string popuped-message
                            :left-fringe 8
                            :right-fringe 8
                            :internal-border-color (face-foreground 'default)
                            :internal-border-width 1)
             (entropy/emacs-vcs--git-messenger:popup-message/unwind-protect
              (posframe-hide msg-buffer-name)))
            ((and (fboundp 'pos-tip-show) (display-graphic-p))
             (git-messenger-hydra/body)
             (pos-tip-show popuped-message))
            ((and (fboundp 'popup-tip) t)
             (git-messenger-hydra/body)
             (let ((tip (popup-tip popuped-message
                                   :point (point)
                                   :margin 1 :truncate t
                                   :nowait t)))
               (entropy/emacs-vcs--git-messenger:popup-message/unwind-protect
                (popup-delete tip))))
            ((fboundp 'lv-message)
             (let ((hydra-hint-display-type 'message))
               (git-messenger-hydra/body))
             (lv-message popuped-message)
             (entropy/emacs-vcs--git-messenger:popup-message/unwind-protect
              (lv-delete-window)))
            (t (message "%s" popuped-message)))
      (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))

  (advice-add #'git-messenger:popup-close :override #'ignore)
  (advice-add #'git-messenger:popup-message :override
              #'entropy/emacs-vcs--git-messenger:popup-message)

;; ***** __________end__________
  )

;; **** Walk through git revisions of a file
(use-package git-timemachine
  :commands
  (git-timemachine
   git-timemachine-switch-branch
   git-timemachine-toggle)
  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (find-file-hook) :adtype hook :pdumper-no-end t)
                             (:adfors (switch-to-buffer) :adtype after :pdumper-no-end t)))
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
  (((:enable t :defer (:data (:adfors (find-file-hook) :adtype hook :pdumper-no-end t)
                             (:adfors (switch-to-buffer) :adtype after :pdumper-no-end t))))
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
  (((:enable t :defer (:data (:adfors (find-file-hook) :adtype hook :pdumper-no-end t)
                             (:adfors (switch-to-buffer) :adtype after :pdumper-no-end t))))
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
  :commands (smerge-mode)
  :eemacs-indhc
  (((:enable t :defer t)
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
  (((:enable t :defer t))
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

  :hook ((magit-diff-visit-file
          .
          entropy/emacs-vcs--smerge-hydra-popup))
  :init
  (entropy/emacs-lazy-initial-advice-before
   (find-file)
   "smerge-init-for-find-file-hook" "smerge-init-for-find-file-hook"
   prompt-echo
   :pdumper-no-end t
   (add-hook 'find-file-hook
             'entropy/emacs-vcs--smerge-auto-open-for-buffer))
  )

;; **** Git major modes
(use-package git-modes
  :init
  (add-hook 'entropy/emacs-after-startup-hook
            #'(lambda () (require 'git-modes))))


;; **** Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :commands (browse-at-remote))

(provide 'entropy-emacs-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
