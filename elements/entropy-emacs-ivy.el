;;; File name: init-ivy.el ---> for entropy-emacs
;;
;; Copyright (c) 2017 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** counsel
(use-package counsel
  :diminish counsel-mode ivy-mode
  :commands (counsel-mode ivy-mode)
;; *** bind-key
  :bind (("M-x" . counsel-M-x)
         ("C-s" . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c M-t" . entropy/counsel-load-theme)
;; **** counsel mode map
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x j" . counsel-mark-ring)
         ("C-x C-t" . counsel-find-file-extern)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-h l"   . counsel-find-library)
         ("C-c M-b" . counsel-recentf)
         ;; swiper-map
         :map swiper-map
         ("M-%" . swiper-query-replace))
;; *** init
  :init
  (add-hook 'entropy/init-X-hook #'ivy-mode)
  (add-hook 'ivy-mode-hook #'counsel-mode)

  (defun ivy-read-action ()
    "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise.

Notice: 

*This function have been modified for fixing the bug of:*

If `buffer-list' includ buffer \"Backtrace\" buffer this func will
not performed normally.

The core error founded with was the snippet: \\
~(setq key (concat key (string (read-key hint))))~

Which the part ~(read-key hint)~ will return one list as 
='(t . NUM)=  instead of the common return as one number, this
will cause func =string= sign the error:
#+BEGIN_EXAMPLE
(wrong-type-argument characterp (t . NUM))
#+END_EXAMPLE

To solve this problem was adding one condition checker for judge
which type of value be:
#+BEGIN_SRC emacs-lisp
  (or (condition-case error (string hint-value) (error nil))
      (condition-case error (string (cdr hint-value))))
  ;; hint-value was can be defined in one 'let' form for func ~(read-key hint)~.
 #+END_SRC
"
    (interactive)
    (let ((actions (ivy-state-action ivy-last)))
      (if (not (ivy--actionp actions))
          t
        (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
               (resize-mini-windows t)
               (key "")
               action-idx)
          (while (and (setq action-idx (cl-position-if
                                        (lambda (x)
                                          (string-prefix-p key (car x)))
                                        (cdr actions)))
                      (not (string= key (car (nth action-idx (cdr actions))))))
            (let* ((hint-value (read-key hint))
                   hint-string)
              (setq hint-string
                    (or
                     (condition-case error
                         (string hint-value)
                       (error nil))
                     (condition-case error
                         (string (cdr hint-value))
                       (error nil))))
              (if hint-string
                  (setq key (concat key hint-string))
                (error "Ivy hint error!"))))
          (cond ((member key '("" ""))
                 nil)
                ((null action-idx)
                 (message "%s is not bound" key)
                 nil)
                (t
                 (message "")
                 (setcar actions (1+ action-idx))
                 (ivy-set-action actions)))))))
  
  (defun entropy/ivy-read-action-after-advice (&rest args)
    "Interrupting rest process when `this-command' was
`ivy-read-action'."
    (catch 'ivy-quit
      (if (or (eq this-command 'ivy-dispatching-done)
              (eq this-command 'ivy-occur))
          (progn
            (user-error "Ivy-quit!")
            (throw 'ivy-quit "Ivy-quit!")))))
  (advice-add 'ivy-read :after #'entropy/ivy-read-action-after-advice)
;; **** escape use top-level
  (with-eval-after-load 'counsel
    (define-key counsel-mode-map (kbd "ESC ESC") 'top-level))

;; **** improve counsel-git and counsel-bookmark

  ;; counsel-git and counsel-bookmark usually using 'utf-8' encoding for searching and return data
  ;; back, so `entropy/custom-language-environment-enable' was conflicted with it, we must turn the
  ;; main encoding type while calling them.
  (if entropy/custom-language-environment-enable
      (progn
        (defun entropy/counsel-git ()
          (interactive)
          (if (not (string= current-language-environment "UTF-8"))
              (progn
                (set-language-environment "UTF-8")
                (prefer-coding-system 'utf-8-unix)
                (counsel-git))
            (counsel-git)))
        (global-set-key (kbd "C-c g") 'entropy/counsel-git)

        (defun entropy/counsel-bookmark ()
          (interactive)
          (if (not (string= current-language-environment "UTF-8"))
              (progn
                (set-language-environment "UTF-8")
                (prefer-coding-system 'utf-8-unix)
                (setq buffer-file-coding-system 'utf-8-unix)
                (setq process-coding-system 'utf-8-unix)
                (set-terminal-coding-system 'utf-8-unix)
                (set-keyboard-coding-system 'utf-8-unix)
                (counsel-bookmark))
            (counsel-bookmark)))
        (global-set-key (kbd "C-x r b") 'entropy/counsel-bookmark))
    (progn
      (global-set-key (kbd "C-c g") 'counsel-git)
      (global-set-key (kbd "C-x r b") 'counsel-bookmark)))

;; *** config
  :config
;; **** advice counsel--M-x-externs for it's bad lagging perfomance

  ;; because `counsel--M-x-externs' has the `require' function for it's contained condition context
  ;; so it will lagging like previous version of `ivy--add-face'.
  (defun entropy/counsel--M-x-externs ()
    nil)
  (advice-add 'counsel--M-x-externs :override #'entropy/counsel--M-x-externs)
;; **** disabled ivy-initial-inputs-alist

  ;; ivy initial char inserting was using for regex like searching, and it's also be '^' for ahead
  ;; searching, but when you want to searching no limited in ahead type we must force disable it.
  (setq ivy-initial-inputs-alist nil) ; disable "^" for heading begining search

;; ***** Redefine the ivy-partial-or-done to prevent double click '<tab>'

  ;; This portion give the minor changed for disabled double tab in ivy completion for preventing
  ;; accidental operation of double hint for 'TAB'.
  (defun ivy-partial-or-done ()
    "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
    (interactive)
    (if (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
             (or (and (equal ivy--directory "/")
                      (string-match "\\`[^/]+:.*\\'" ivy-text))
                 (string-match "\\`/" ivy-text)))
        (let ((default-directory ivy--directory)
              dir)
          (minibuffer-complete)
          (setq ivy-text (ivy--input))
          (when (setq dir (ivy-expand-file-if-directory ivy-text))
            (ivy--cd dir)))
      (or (ivy-partial)
          (when (or (eq this-command last-command)
                    (eq ivy--length 1))
            (message ":) evil smile ^v^")))))

;; **** windows not to use grep because there's no grep in windows
  (when sys/win32p
    (defun counsel-grep-or-swiper (&optional initial-input)
      (interactive)
      (swiper initial-input)))
  
;; **** ivy details
  (setq enable-recursive-minibuffers nil) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)

  ;; using fuzzy matching
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")
;; **** disabled counsel-find-file-at-point
  (setq counsel-find-file-at-point nil)
;; **** counsel-load-theme
  (defun entropy/ivy-org-mode-heading-reset ()
    "Stop the org-level headers from increasing in height relative to the other text."
    (with-eval-after-load 'org
      (dolist (face '(org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5
                      org-level-6
                      org-level-7
                      org-level-8))
        (set-face-attribute face nil :background nil :weight 'semi-bold :height 1.0))))
  (defun entropy/counsel-load-theme ()
    "Load theme with reset the org-headline face for disabled the
font style and height."
    (interactive)
    (counsel-load-theme)
    (entropy/ivy-org-mode-heading-reset))

  (defun entropy/clth-ivy-current-match-specific (x)
    "Advice for `counsel-load-theme-action' that setting face of
`ivy-current-match' for spacemacs themes.

Reason of this setting was that spacemacs has the un-obviouse
visual distinction of `ivy-current-match' covered upon the
`ivy-minibuffer-match-highlight'."
    (cond
     ((string-match-p "spacemacs-dark" x)
      (set-face-attribute 'ivy-current-match nil
                          :background "purple4" :bold t))
     ((string-match-p "spacemacs-light)" x)
      (set-face-attribute 'ivy-current-match nil
                          :background "salmon" :bold t))
     ((string-match-p "darkokai" x)
      (set-face-attribute 'ivy-current-match nil
                          :background "#65a7e2"))
     ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" x)
      (if (equal 'dark (frame-parameter nil 'background-mode))
          (set-face-attribute 'ivy-current-match nil
                              :background "#65a7e2" :foreground "black")
        (set-face-attribute 'ivy-current-match nil
                            :background "#1a4b77" :foreground "white")))))

  (advice-add 'counsel-load-theme-action :after #'entropy/clth-ivy-current-match-specific)

  (defun entropy/clth-doomline-specifix (arg)
    "Advice of auto refresh doom-modeline bar background color
when changing theme."
    (progn
      (cond ((and (string= entropy/mode-line-sticker "doom")
                  (string-match-p "\\(ujelly\\)" arg))
             (set-face-attribute 'doom-modeline-bar
                                 nil :background "black")
             (doom-modeline-refresh-bars))
            ((and (string= entropy/mode-line-sticker "doom")
                  (string-match-p "\\(spolsky\\)" arg))
             (setq doom-modeline--bar-active
                   (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                            doom-modeline-bar-width
                                            doom-modeline-height)))
            ((string= entropy/mode-line-sticker "doom")
             (set-face-attribute 'doom-modeline-bar
                                 nil :background (face-background 'mode-line nil t))
             (doom-modeline-refresh-bars)))))

  (advice-add 'counsel-load-theme-action :after #'entropy/clth-doomline-specifix)



  (defun entropy/clth-other-fixing (arg &rest args)
    "Advice of fixing other tiny bug of specific theme."
    (when (not (featurep 'hl-line))
      (require 'hl-line))
    (cond
     ((string= "doom-solarized-light" arg)
      (set-face-attribute 'hl-line nil :background "moccasin"))))
  (advice-add 'counsel-load-theme-action :after #'entropy/clth-other-fixing)
  

;; **** counsel-locate
  (when (and sys/win32p entropy/wsl-enable)
    (defun entropy/counsel-locate ()
      "Call counsel-locate by unicode encoding when in windows
plattform."
      (interactive)
      (unless (string= current-language-environment "UTF-8")
        (set-language-environment "UTF-8")
        (prefer-coding-system 'utf-8-unix))
      (counsel-locate))

    (defun counsel-locate (&optional initial-input)
      "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

Note: This function has been modified for transfer volum's type
of msys2 or other window-gnu-enviroment to windows origin volum
type by function `entropy/transfer-wvol'"
      (interactive)
      (ivy-read "Locate: " #'counsel-locate-function
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-locate-history
                :action (lambda (file)
                          (when file
                            (with-ivy-window
                              (entropy/transfer-wvol file))))
                :unwind #'counsel-delete-process
                :caller 'counsel-locate))

    (defun entropy/transfer-wvol (file)
      (if (string-match-p "^/[a-z]/" file)
          (let ((wvol (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:" file)))
            (find-file wvol))
        (find-file file))))

;; **** counsel-dired-jump
  (when (or sys/win32p sys/cygwinp)
    (advice-add 'counsel-dired-jump :before #'entropy/lang-set-utf-8))

;; **** use counsel css for quickly search css selector
  (use-package counsel-css
    :hook (css-mode . counsel-css-imenu-setup)
    :bind (:map css-mode-map ("C-c M-d" . counsel-css)))
  
;; **** use ivy-xref for quickly find defination and reference
  (use-package ivy-xref
    :commands (ivy-xref-show-xrefs)
    :init (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))
;; **** redefine counsel-find-file-extern

  ;; Origin this function using `interactive' prefix 'F' be as the
  ;; file choosing func, it's has a bug for what wrong with the
  ;; current diretory './' open as of redirected as current
  ;; buffer-file. Using `completing-read' instead.
  
  (defun counsel-locate-action-extern (X)
    "Pass X to `xdg-open' or equivalent command via the shell."
    (interactive
     (list (completing-read "Directory: " 'read-file-name-internal nil t)))
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (w32-shell-execute "open" X)
      (call-process-shell-command (format "%s %s"
                                          (cl-case system-type
                                            (darwin "open")
                                            (cygwin "cygstart")
                                            (t "xdg-open"))
                                          (shell-quote-argument X))
                                  nil 0))))
;; ** avy
(use-package avy
  :bind
  (("C-M-;" . avy-goto-char)
   ("M-g l" . avy-goto-line)))
;; ** use helm  ag or pt search
(defun entropy/helm-ag--edit-commit ()
  "Funciton to be redefine body of `helm-ag--edit-commit'."
  (goto-char (point-min))
  (let ((read-only-files 0)
        (saved-buffers nil)
        (regexp (helm-ag--match-line-regexp))
        (default-directory helm-ag--default-directory)
        (line-deletes (make-hash-table :test #'equal))
        (kept-buffers (buffer-list))
        open-buffers)
    (while (re-search-forward regexp nil t)
      (let* ((file (or (match-string-no-properties 1) helm-ag--search-this-file-p))
             (line (string-to-number (match-string-no-properties 2)))
             (body (match-string-no-properties 3))
             (ovs (overlays-at (line-beginning-position))))
        (with-current-buffer (find-file-noselect file)
          (message "Handling with buffer '%s' ......" (current-buffer))
          (cl-pushnew (current-buffer) open-buffers)
          (if buffer-read-only
              (progn
                (cl-incf read-only-files)
                (read-only-mode 0)))
          (goto-char (point-min))
          (let ((deleted-lines (gethash file line-deletes 0))
                (deleted (and ovs (overlay-get (car ovs) 'helm-ag-deleted))))
            (forward-line (- line 1 deleted-lines))
            (delete-region (line-beginning-position) (line-end-position))
            (if (not deleted)
                (insert body)
              (let ((beg (point)))
                (forward-line 1)
                (delete-region beg (point))
                (puthash file (1+ deleted-lines) line-deletes)))
            (cl-pushnew (current-buffer) saved-buffers)))))
    (when helm-ag-edit-save
      (dolist (buf saved-buffers)
        (with-current-buffer buf
          (save-buffer))))
    (dolist (buf open-buffers)
      (unless (memq buf kept-buffers)
        (kill-buffer buf)))
    (helm-ag--exit-from-edit-mode)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and be lift a ban." read-only-files)
      (message "Success update"))))



(if sys/win32p
;; *** Windows plattform
    (if (string= entropy/search-program "pt")
;; **** helm-pt
        (use-package helm-pt
          :commands (helm-do-pt helm-projectile-pt)
          :init
          (setq helm-pt-insert-at-point nil)
          (if entropy/custom-language-environment-enable
              ;; when customized language environment setting enabel keeping helm-pt using UTF-8 for
              ;; enchance the unicode querying.
              (progn
                (progn
                  ;; Setting unique setting for windows
                  (defun entropy/helm-do-pt-for-win ()
                    (interactive)
                    (unless (string= current-language-environment "UTF-8")
                      (set-language-environment "UTF-8")
                      (prefer-coding-system 'utf-8-unix))
                    (helm-do-pt))
                  (global-set-key (kbd "C-c j") 'entropy/helm-do-pt-for-win))

                (progn
                  (defun entropy/helm-projectile-pt-for-win ()
                    (interactive)
                    (unless (string= current-language-environment "UTF-8")
                      (set-language-environment "UTF-8")
                      (prefer-coding-system 'utf-8-unix))
                    (helm-projectile-pt))
                  (global-set-key (kbd "C-c k") 'entropy/helm-projectile-pt-for-win)))
            (progn
              (global-set-key (kbd "C-c j") 'helm-do-pt)
              (global-set-key (kbd "C-c k") 'helm-projectile-pt))))
;; **** use helm ag
      (use-package helm-ag
        :commands (helm-do-ag helm-do-ag-project-root)
        :init
        ;; Because windows ag exec was locale rely on the operation system, so keeping the ag
        ;; process using locale setting language environment.
        (if entropy/custom-language-environment-enable
            (progn
              (defun entropy/helm-do-ag ()
                (interactive)
                (if (string= current-language-environment "UTF-8")
                    (entropy/toggle-utf-8-and-locale))
                (helm-do-ag))
              (defun entropy/helm-do-ag-project-root ()
                (interactive)
                (if (string= current-language-environment "UTF-8")
                    (entropy/toggle-utf-8-and-locale))
                (helm-do-ag-project-root))
              (global-set-key (kbd "C-c j") 'entropy/helm-do-ag)
              (global-set-key (kbd "C-c k") 'entropy/helm-do-ag-project-root))
          (progn
            (defun entropy/helm-do-ag ()
              (interactive)
              (if (string= current-language-environment "UTF-8")
                  (progn
                    (set-terminal-coding-system nil)
                    (set-keyboard-coding-system nil)
                    (set-language-environment (coding-system-base locale-coding-system))))
              (helm-do-ag))
            (defun entropy/helm-do-ag-project-root ()
              (interactive)
              (if (string= current-language-environment "UTF-8")
                  (progn
                    (set-terminal-coding-system nil)
                    (set-keyboard-coding-system nil)
                    (set-language-environment (coding-system-base locale-coding-system))))
              (helm-do-ag-project-root))
            (global-set-key (kbd "C-c j") 'entropy/helm-do-ag)
            (global-set-key (kbd "C-c k") 'entropy/helm-do-ag-project-root)))
        :config
        (defun helm-ag--edit-commit ()
          "Note: this function has been re-define for compat with
entropy-emacs which force inhibit readonly mode while operating
corresponding buffer."
          (interactive)
          (funcall #'entropy/helm-ag--edit-commit))))
;; *** Unix-like plattform
  (if (string= entropy/search-program "pt")
      (use-package helm-pt
        :commands (helm-do-pt helm-projectile-pt)
        :bind
        (("C-c j" . helm-do-pt)
         ("C-c k" . helm-projectile-pt)))
    (use-package helm-ag
      :commands (helm-do-ag helm-do-ag-project-root)
      :bind (("C-c j" . helm-do-ag)
             ("C-c k" . helm-do-ag-project-root))
      :config
      (defun helm-ag--edit-commit ()
        "Note: this function has been re-define for compat with
entropy-emacs which force inhibit readonly mode while operating
corresponding buffer."
        (interactive)
        (funcall #'entropy/helm-ag--edit-commit)))))

;; ** provide
(provide 'entropy-emacs-ivy)
