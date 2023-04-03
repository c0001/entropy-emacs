;;; entropy-emacs-shell.el --- entropy emacs shell configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C)  20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-shell.el
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
;; `entropy-emacs' internal shell emulator configuration for 'eshell'
;; and 'term'.
;;
;; * Configuration:
;;
;; This file may not be used out of entroy-emacs.
;;
;;
;; * Code:
;;
;; ** require

;; ** shell config
(use-package shell
  :ensure nil
  :config
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;; *** Improve shell buffer interactive experience
  (defun entropy/emacs-shell--n-shell-mode-hook ()
    "Shell mode customizations."
    (apply
     'entropy/emacs-local-set-key-batch-do
     '(([up] . comint-previous-input)
       ([down] . comint-next-input)
       ([(shift tab)] . comint-next-matching-input-from-input)))
    (setq comint-input-sender 'entropy/emacs-shell--n-shell-simple-send))
  (add-hook 'shell-mode-hook #'entropy/emacs-shell--n-shell-mode-hook)

  (defun entropy/emacs-shell--n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer)
      )
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command)))))

;; ** eshell
(use-package eshell
  :ensure nil
  :preface
  (defvar eshell-history-file-name)
  (defvar eshell-cmpl-cycle-completions)
  (defvar eshell-aliases-file)
  (defvar eshell-prompt-function)
  (defun entropy/emacs-shell--eshell-before-advice (&rest _)
    "Delete eshell histroy file before eshell opening and prevent
open eshell on tramp-buffer when on windows platform. "
    (if (file-exists-p eshell-history-file-name)
        (delete-file eshell-history-file-name))
    (when (and (string-match "^/\\w+?:" default-directory)
               sys/win32p)
      (error "Can not using eshell in tramp location.")))

  :config
  ;; List all candidates when enter 'tab' key
  (setq eshell-cmpl-cycle-completions nil)

  ;; Value assignment
  (setq eshell-aliases-file entropy/emacs-eshell-alias-file
        eshell-history-file-name entropy/emacs-eshell-history-file)

  ;; disable eshell history preserve for local file for secure warranty
  (advice-add 'eshell :before #'entropy/emacs-shell--eshell-before-advice)
  (add-hook 'eshell-exit-hook #'(lambda ()
                                  (if (file-exists-p eshell-history-file-name)
                                      (delete-file eshell-history-file-name))))

  ;; Redefine `eshell-search-path' for preventing search executable
  ;; binary in current path automatically without prefix "./" manually
  ;; given in WINDOWS.
  (entropy/emacs--api-restriction-uniform 'eshell-search-path-patch-for-windows
      'emacs-version-incompatible
    :when sys/win32p
    :detector
    (not
     (and
      (entropy/emacs-do-error-for-emacs-version-incompatible
       '<= "29.1" 'noerr)
      (entropy/emacs-do-error-for-emacs-version-incompatible
       '> "26" 'noerr)))
    :signal
    (progn
      (entropy/emacs-do-error-for-emacs-version-incompatible
       '<= "29.1")
      (entropy/emacs-do-error-for-emacs-version-incompatible
       '> "26"))
    (defun __ya/eshell-search-path (name)
      "Patched as compat with entropy-emacs for the reason as:

FOR preventing search executable binary in current path automatically
without prefix \"./\" manually given in WINDOWS platform, as for command
'git', if command calling in the submodule path which has the git
repository redirection description file '.git' will calling it instead
search for rest path element stored in `eshell-get-path' since its
first element is current dirctory dot \".\".

In this case, each command file name was concatenated for \".\" and
\"git\" which make the filename \".git\" for search which matched by the
'.git' description file exactly. So as on, the external git command
will not be called for the instance as your expection."
      (if (file-name-absolute-p name)
          name
        (let ((list (cdr (eshell-get-path)))
              suffixes n1 n2 file)
          ;; Remove current
          (if (eshell-under-windows-p) (setq list (cdr list)))
          (while list
            (setq n1 (concat (car list) name))
            (setq suffixes eshell-binary-suffixes)
            (while suffixes
              (setq n2 (concat n1 (car suffixes)))
              (if (and (or (file-executable-p n2)
                           (and eshell-force-execution
                                (file-readable-p n2)))
                       (not (file-directory-p n2)))
                  (setq file n2 suffixes nil list nil))
              (setq suffixes (cdr suffixes)))
            (setq list (cdr list)))
          file)))
    (advice-add 'eshell-search-path
                :override #'__ya/eshell-search-path))

  ;; self-function
  (defun eshell/touch (&rest files)
    "Elisp implement 'touch' unix command."
    (when files
      (dolist (el files)
        (let ((fname el))
          (when (not (stringp fname))
            (cond
             ((symbolp fname)
              (setq fname (symbol-name fname)))
             ((numberp fname)
              (setq fname (number-to-string fname)))))
          (f-touch fname)))))

  ;; Shorten eshell prompt string
  (defun entropy/emacs-shell--eshell-prompt ()
    "Shrink emacs eshell's prompt string if feature `shrink-path'
was found."
    (let ((pwd (eshell/pwd))
          shrink-pwd)
      (when (and (or (featurep 'shrink-path)
                     (ignore-errors (require 'shrink-path)))
                 (fboundp 'shrink-path-dirs))
        (setq shrink-pwd (shrink-path-dirs pwd)))
      (if (and shrink-pwd
               (stringp shrink-pwd))
          (concat shrink-pwd " $ ")
        (concat pwd " $ "))))

  (setq eshell-prompt-function
        'entropy/emacs-shell--eshell-prompt))


;; ** term configuration
(use-package eterm-256color
  ;; enable 256 color for emacs term
  :commands (eterm-256color-mode)
  :hook (term-mode . eterm-256color-mode))

(use-package fakecygpty
  :if (and sys/win32p
           entropy/emacs-win-fakecygpty-enable
           (executable-find "fakecygpty")
           (executable-find "qkill"))
  :ensure nil
  :commands fakecygpty-activate
  :preface
  (defun entropy/emacs-shell--fakepty-cd-around-advice (old_func dir)
    (let ((wsl-root entropy/emacs-microsoft-windows-unix-emulator-root-path))
      (cond ((string-match-p "^/.[^/]+" dir)
             (setq dir (expand-file-name (replace-regexp-in-string "^/" "" dir) wsl-root)))
            ((string-match-p "^/./" dir)
             (setq dir
                   (expand-file-name
                    (replace-regexp-in-string "^/\\(.\\)/" "\\1:/" dir))))
            ((string-match-p "^/.$" dir)
             (setq dir
                   (expand-file-name
                    (replace-regexp-in-string "^/\\(.\\)" "\\1:/" dir))))
            ((string-match-p "^/$" dir)
             (setq dir wsl-root)))
      (funcall old_func dir)))

  :init
  (entropy/emacs-lazy-with-load-trail
    'fakecygpty
    (fakecygpty-activate)
    (when (and entropy/emacs-microsoft-windows-unix-emulator-enable
               (ignore-errors (file-exists-p entropy/emacs-microsoft-windows-unix-emulator-bin-path)))
      (advice-add 'cd :around 'entropy/emacs-shell--fakepty-cd-around-advice))))


;; ** vterm config

(use-package vterm
  :if (entropy/emacs-vterm-support-p)
  :defer (or entropy/emacs-fall-love-with-pdumper entropy/emacs-custom-enable-lazy-load)
  :commands (vterm vterm-mode)
  :preface
  ;; EEMACS_MAINTENANCE: add support to for-zsh and for-fishrc etc too.
  (defun entropy/emacs-shell--vterm-pwd-hack-for-current-shell ()
    "Hack for current SHELL to support vterm PWD callback. details to
see vterm's README."
    (when-let*
        ((system-valid-p
          (not (eq system-type 'windows-nt)))
         (falist  '(("bash" . "emacs-vterm-bash.sh")
                    ("zsh"  . "emacs-vterm-zsh.sh")
                    ("fish" . "emacs-vterm.fish")))
         (sfalist '(("bash" "~"              . ".bashrc")
                    ("zsh"  "~"              . ".zshrc")
                    ("fish" "~/.config/fish" . "config.fish")))
         (quried-flag (expand-file-name
                       "vterm-hack-bashrc-confirmed"
                       entropy/emacs-stuffs-topdir))
         (curshell (entropy/emacs-shell-script-get-shell-type))
         (hkf      (alist-get curshell falist nil nil 'string=))
         ((if (not (file-exists-p quried-flag)) t
            (with-current-buffer (find-file-noselect quried-flag)
              (prog1 (not (re-search-forward (regexp-quote curshell) nil t))
                (kill-buffer)))))
         (vterm-etc-host
          (expand-file-name
           "etc"
           (file-name-directory (locate-library "vterm"))))
         ((file-exists-p vterm-etc-host))
         (hkfpath        (expand-file-name hkf vterm-etc-host))
         ((file-exists-p hkfpath))
         (hack_inject?
          (unwind-protect
              (yes-or-no-p
               (format "Hack %s for support vterm pwd?\
(note: this just quried once, do not do it if you have did thus.)"
                       curshell)) nil))
         (inhibit-read-only t))
      (entropy/emacs-with-file-buffer
          (let* ((i (alist-get curshell sfalist nil nil 'string=))
                 (d (car i)) (f (cdr i)))
            (unless (file-directory-p d) (mkdir d t))
            (expand-file-name f d))
        :with-kill-visitings-pred
        (if noninteractive 'always)
        :with-save-visitings-pred
        (if noninteractive 'always)
        :with-kill-buffer-when-done t
        (goto-char (point-max))
        (insert "\n\n")
        ;; insert conditions
        (cond
         ((member curshell '("bash" "zsh"))
          (insert "if [[ \"$INSIDE_EMACS\" = 'vterm' ]]; then : ; \
else return 0; fi\n"))
         ((string= curshell "fish")
          (insert "if [ \"$INSIDE_EMACS\" = 'vterm' ]\n\
 : \nelse\n return 0\n end")))
        (insert "\n\n")
        ;; insert body
        (insert-file-contents hkfpath)
        (save-buffer))
      (entropy/emacs-with-file-buffer quried-flag
        :with-kill-visitings-pred 'always
        :without-save-visitings-pred 'always
        :with-kill-buffer-when-done t
        (goto-char (point-max))
        (if (or (save-excursion (looking-back ":" (line-beginning-position)))
                ;; buffer empty
                (= (point) 1))
            (insert (format "%s" curshell))
          (insert (format ":%s" curshell)))
        (save-buffer)) t))

  :init
  (setq
   ;; prevent the large buffer content remainin lag (possibile)
   vterm-max-scrollback 1000)

  (add-to-list 'entropy/emacs-xterm-paste-inhibit-read-only-filter
               #'(lambda (&rest _)
                   (eq major-mode 'vterm-mode)))

  (add-to-list 'entropy/emacs-xterm-paste-yank-replacement-register
               (cons (lambda () (eq major-mode 'vterm-mode))
                     #'vterm-yank))

  :config

  (defun entropy/emacs-shell--vterm-mode-around-advice (orig-func &rest orig-args)
    "prevent `vterm-mode` calling in vterm-mode from causing
segmentation fault."
    (unless (eq major-mode 'vterm-mode)
      (apply orig-func orig-args)))
  (advice-add 'vterm-mode :around #'entropy/emacs-shell--vterm-mode-around-advice)

  ;; more vterm key bounding

  (defun entropy/emacs-shell-vterm-meta-left ()
    ""
    (declare (interactive-only t))
    (interactive nil vterm-mode)
    (vterm-send-key "<left>" nil t nil))

  (defun entropy/emacs-shell-vterm-meta-right ()
    ""
    (declare (interactive-only t))
    (interactive nil vterm-mode)
    (vterm-send-key "<right>" nil t nil))

  (defun entropy/emacs-shell-vterm-ctrl-left ()
    ""
    (declare (interactive-only t))
    (interactive nil vterm-mode)
    (vterm-send-key "<left>" nil nil t))

  (defun entropy/emacs-shell-vterm-ctrl-right ()
    ""
    (declare (interactive-only t))
    (interactive nil vterm-mode)
    (vterm-send-key "<right>" nil nil t))

  (defun entropy/emacs-shell-vterm-ctrl-backspace ()
    ""
    (declare (interactive-only t))
    (interactive nil vterm-mode)
    (vterm-send-key "<backspace>" nil nil t))

  (define-key vterm-mode-map [C-left]
    #'entropy/emacs-shell-vterm-ctrl-left)
  (define-key vterm-mode-map [C-right]
    #'entropy/emacs-shell-vterm-ctrl-right)
  (define-key vterm-mode-map [M-left]
    #'entropy/emacs-shell-vterm-meta-left)
  (define-key vterm-mode-map [M-right]
    #'entropy/emacs-shell-vterm-meta-right)
  (define-key vterm-mode-map [C-backspace]
    #'entropy/emacs-shell-vterm-ctrl-backspace)

  ;; Remap `vterm-copy-mode-done' to disable `vterm-copy-mode-done'
  ;; features to disable auto kill-ring save feature while thus.
  (define-key vterm-copy-mode-map
    [remap vterm-copy-mode-done]
    #'(lambda (&rest _) (interactive) (vterm-copy-mode 0)))

  ;; Remove conflicting f12 keybind via `vterm-mode-map' and function
  ;; `entropy/emacs-tools-time-show'
  (define-key vterm-mode-map (kbd "<f12>") nil)
  ;; similarly for <f11>
  (define-key vterm-mode-map (kbd "<f11>") nil)
  ;; extra keybinding exclusions
  ;; --- FIXME: why 'M-O' conflicated with <f1> in `vterm-mode-map'?
  (define-key vterm-mode-map (kbd "M-O") nil)

  ;; enable native ime toggle for `vterm-mode'. Based on vterm updates
  ;; of
  ;; https://github.com/akermu/emacs-libvterm/commit/2b1392cb2b14ec5bd0b7355197d5f353aa5d3983
  (defun entropy/emacs-shell--vterm-set-ime-key (&optional func)
    (define-key vterm-mode-map
      (kbd entropy/emacs-internal-ime-toggling-kbd-key)
      (or func entropy/emacs-internal-IME-toggle-function)))
  ;; set at vterm first load time
  (entropy/emacs-shell--vterm-set-ime-key)
  ;; inject for future changes
  (add-hook 'entropy/emacs-internal-IME-toggle-function-set-hook
            #'entropy/emacs-shell--vterm-set-ime-key)

  (defun entropy/emacs-shell--vterm-enable-eemacs-top-key nil
    (define-key vterm-mode-map (kbd entropy/emacs-top-key) nil))
  (when (daemonp)
    (entropy/emacs-with-daemon-make-frame-done
      '__enable/eemacs-top-key-for-vterm (&rest _)
      "Remove collision of `entropy/emacs-top-key' in `vterm-mode-map'."
      :when-gui (entropy/emacs-shell--vterm-enable-eemacs-top-key)
      :when-tui (entropy/emacs-shell--vterm-enable-eemacs-top-key)))
  (entropy/emacs-shell--vterm-enable-eemacs-top-key)

  ;; finally we should auto hack vterm PWD integration
  (entropy/emacs-shell--vterm-pwd-hack-for-current-shell))

;; ** Shell Pop
(use-package entropy-shellpop
  :ensure nil
  :commands (entropy/shellpop-start)
  :preface

  (defun entropy/emacs-shell--shellpop-bindkey-for-eshell (func)
    (entropy/emacs-hydra-hollow-add-for-top-dispatch
     `("Shellpop"
       (("-" ,func "Shellpop For Eshell"
         :enable t
         :exit t
         :eemacs-top-bind t))))
    (unless (display-graphic-p)
      (define-key entropy-shellpop-mode-map
        (kbd (concat entropy/emacs-top-key " " "-"))
        func)))

  (defun entropy/emacs-shell--shellpop-bindkey-for-shell (func)
    (let ((key "s s"))
      (entropy/emacs-hydra-hollow-add-for-top-dispatch
       `("Shellpop"
         ((,key ,func "Shellpop For emacs shell-mode"
                :enable t
                :exit t
                :eemacs-top-bind t))))
      (unless (display-graphic-p)
        (define-key entropy-shellpop-mode-map
                    (kbd (concat entropy/emacs-top-key " " key))
                    func))))

  (defun entropy/emacs-shell--shellpop-bindkey-for-ansiterm (func)
    (let ((key "s a"))
      (entropy/emacs-hydra-hollow-add-for-top-dispatch
       `("Shellpop"
         ((,key ,func "Shellpop For Ansi-Term"
                :enable t
                :exit t
                :eemacs-top-bind t))))
      (unless (display-graphic-p)
        (define-key entropy-shellpop-mode-map
          (kbd (concat entropy/emacs-top-key " " key))
          func))))

  (when (and (member "MODULES" (split-string system-configuration-features nil t))
             (not sys/win32p))
    (defun entropy/emacs-shell--shellpop-bindkey-for-vterm (func)
      (entropy/emacs-hydra-hollow-add-for-top-dispatch
       `("Shellpop"
         (("=" ,func "Shellpop For Vterm"
           :enable t
           :exit t
           :eemacs-top-bind t))))
      (unless (display-graphic-p)
        (define-key entropy-shellpop-mode-map
          (kbd (concat entropy/emacs-top-key " " "="))
          func))))

  :init
  (defvar entropy/emacs-shell--shpop-types)
  (setq entropy/emacs-shell--shpop-types
        `(:ansiterm
          (:type-name
           "eemacs-ansiterm"
           :shackle-size 0.3
           :shackle-align below
           :type-keybind entropy/emacs-shell--shellpop-bindkey-for-ansiterm
           :type-body
           (ansi-term "/bin/bash"))
          :eshell
          (:type-name
           "eemacs-eshell"
           :shackle-size 0.3
           :shackle-align below
           :type-keybind entropy/emacs-shell--shellpop-bindkey-for-eshell
           :type-body
           (eshell))
          :shell
          (:type-name
           "eemacs-shell"
           :shackle-size 0.3
           :shackle-align below
           :type-keybind entropy/emacs-shell--shellpop-bindkey-for-shell
           :type-body
           (shell (current-buffer)))
          :vterm
          (:type-name
           "eemacs-vterm"
           :shackle-size 0.3
           :shackle-align bottom
           :type-keybind entropy/emacs-shell--shellpop-bindkey-for-vterm
           :type-body
           (let
               (;; prevent vterm auto rename buffer that lost register linkage
                (vterm-buffer-name-string nil))
             (vterm-mode)))))

  (entropy/emacs-lazy-with-load-trail 'shellpop-feature
    :pdumper-no-end t
    (cond ((or (not sys/win32p)
               (and sys/win32p (bound-and-true-p fakecygpty--activated)))
           (setq entropy/shellpop-pop-types
                 (list (plist-get entropy/emacs-shell--shpop-types :eshell)
                       (plist-get entropy/emacs-shell--shpop-types :ansiterm)
                       (plist-get entropy/emacs-shell--shpop-types :shell)))
           (when (and (member "MODULES" (split-string system-configuration-features nil t))
                      (not sys/win32p)
                      (let ((execs '("git" "cmake" "make" "gcc" "libtool"))
                            judge)
                        (catch :exit
                          (dolist (exec execs)
                            (unless (executable-find exec)
                              (setq judge t)
                              (throw :exit nil))))
                        (if judge nil t)))
             (add-to-list 'entropy/shellpop-pop-types
                          (plist-get entropy/emacs-shell--shpop-types :vterm))))
          (sys/win32p
           (setq entropy/shellpop-pop-types
                 (list (plist-get entropy/emacs-shell--shpop-types :eshell)
                       (plist-get entropy/emacs-shell--shpop-types :shell)))))
    (entropy/shellpop-start))

  :config

  (declare-function eyebrowse--get "ext:eyebrowse")
  (defvar entropy/emacs-shell-eemacs-shellpop--eyebrowse-curreg nil)

  (defun entropy/emacs-shell-eemacs-shellpop--eyebrowse-tag-get
      (slot)
    (when (or slot
              (setq slot
                    (eyebrowse--get 'current-slot)))
      (nth
       2
       (assoc
        slot
        (eyebrowse--get 'window-configs)))))

  (defun entropy/emacs-shell--eemacs-shellpop--eyebrowse-curreg-prune (var)
    (setq entropy/emacs-shell-eemacs-shellpop--eyebrowse-curreg
          (cl-remove-if
           `(lambda (x)
              (equal (car x) ',var))
           entropy/emacs-shell-eemacs-shellpop--eyebrowse-curreg)))

  (defun entropy/emacs-shell--eemacs-shellpop-eyebrowse-save-reg (&rest _)
    (let* ((cur-slot (eyebrowse--get 'current-slot))
           (cur-tag (entropy/emacs-shell-eemacs-shellpop--eyebrowse-tag-get
                     cur-slot)))
      (entropy/emacs-shell--eemacs-shellpop--eyebrowse-curreg-prune
       cur-slot)
      (push (list cur-slot
                  :tag cur-tag
                  :reg (entropy/shellpop-expose-type-registers-pointer))
            entropy/emacs-shell-eemacs-shellpop--eyebrowse-curreg)))

  (defun entropy/emacs-shell--eemacs-shellpop-eyebrowse-restore-reg (&rest _)
    (let* ((cur-slot (eyebrowse--get 'current-slot))
           (cur-tag (entropy/emacs-shell-eemacs-shellpop--eyebrowse-tag-get
                     cur-slot))
           (old-hist
            (alist-get
             cur-slot
             entropy/emacs-shell-eemacs-shellpop--eyebrowse-curreg
             nil nil 'equal))
           (old-tag (plist-get old-hist :tag))
           (old-reg (plist-get old-hist :reg)))
      (when old-hist
        (entropy/emacs-shell--eemacs-shellpop--eyebrowse-curreg-prune
         cur-slot)
        (when (and
               old-reg
               (or (equal cur-tag old-tag)
                   (yes-or-no-p
                    (format "Use an old shellpop register associate \
the same eyebrowse slot as current use?(tag: %s) "
                            old-tag))))
          (entropy/shellpop-replace-type-registers-pointer-as
           old-reg
           (entropy/emacs-shell-eemacs-shellpop--eyebrowse-tag-get
            nil))))))

  (with-eval-after-load 'eyebrowse
    (add-hook 'eyebrowse-pre-window-switch-hook
              #'entropy/emacs-shell--eemacs-shellpop-eyebrowse-save-reg)
    (add-hook 'eyebrowse-post-window-switch-hook
              #'entropy/emacs-shell--eemacs-shellpop-eyebrowse-restore-reg))
  )

;; ** provide
(provide 'entropy-emacs-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
