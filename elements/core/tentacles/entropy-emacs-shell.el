;;; entropy-emacs-shell.el --- entropy emacs shell configuration
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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** shell config
(use-package shell
  :ensure nil
  :config
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  
;; *** Improve shell buffer interactive experience
  (defun entropy/emacs-shell--n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
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
  (defun entropy/emacs-shell--eshell-before-advice (&rest args)
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
  ;; given.
  (defun eshell-search-path (name)
    "
Search the environment path for NAME.

NOTE: this func has been redefined as compat with entropy-emacs
for the reason as:

FOR preventing search executable binary in current path
automatically without prefix \"./\" manually given, as for command
'git', if command calling in the submodule path which has the git
repository redirection description file '.git' will calling it
instead search for rest path element stored in `eshell-path-env'.

Origin procedure mechanism useing the coding snippet embedded in:
#+BEGIN_SRC emacs-lisp
  (if (eshell-under-windows-p)
      (push \".\" list))
#+END_SRC

Which variable list is setted by:
: (eshell-parse-colon-path eshell-path-env)

Which was inherited from emacs path generated by:
: (getenv \"PATH\")

In this case, each command file name was concatenated for \".\" and
\"git\" which make the filename \".git\" for search which matched by
the '.git' description file exactly. So as on, the external git
command will not be called for the instance as your expection."
    (if (file-name-absolute-p name)
        name
      (let ((list (eshell-parse-colon-path eshell-path-env))
	    suffixes n1 n2 file)
        ;; (if (eshell-under-windows-p)
        ;;     (push "." list))
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
      (when (featurep 'shrink-path)
        (when (not (fboundp 'shrink-path-dirs))
          (require 'shrink-path))
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
  :ensure nil
  :if (and sys/win32p
           entropy/emacs-win-fakecygpty-enable
           (executable-find "fakecygpty")
           (executable-find "qkill"))
  :commands fakecygpty-activate
  :preface
  (defun entropy/emacs-shell--fakepty-cd-around-advice (old_func dir)
    (let ((wsl-root (substring (expand-file-name entropy/emacs-wsl-apps) 0 -9)))
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
   fakecygpty
   (fakecygpty-activate)
   (when (and entropy/emacs-wsl-enable
              (ignore-errors (file-exists-p entropy/emacs-wsl-apps)))
     (advice-add 'cd :around 'entropy/emacs-shell--fakepty-cd-around-advice))))

;; ** Shell Pop
(use-package entropy-shellpop
  :ensure nil
  :commands (entropy/shellpop-start)
  :preface
  
  (defun entropy/emacs-shell--shellpop-bindkey-for-eshell (func)
    (entropy/emacs-!set-key (kbd "-") func))
  (defun entropy/emacs-shell--shellpop-bindkey-for-ansiterm (func)
    (entropy/emacs-!set-key (kbd "=") func))

  :init
  (setq entropy/emacs-shell--shpop-types
        '(:ansiterm
          (:type-name
           "eemacs-ansiterm"
           :size 0.3
           :align below
           :bind entropy/emacs-shell--shellpop-bindkey-for-ansiterm
           :type-body
           (ansi-term "/bin/bash"))
          :eshell
          (:type-name
           "eemacs-eshell"
           :size 0.3
           :align below
           :bind entropy/emacs-shell--shellpop-bindkey-for-eshell
           :type-body
           (eshell))))
  
  (entropy/emacs-lazy-with-load-trail
   shellpop-feature
   (cond ((or (not sys/win32p)
              (and sys/win32p (bound-and-true-p fakecygpty--activated)))
          (setq entropy/shellpop-pop-types
                (list (plist-get entropy/emacs-shell--shpop-types :eshell)
                      (plist-get entropy/emacs-shell--shpop-types :ansiterm))))
         (sys/win32p
          (setq entropy/shellpop-pop-types
                (list (plist-get entropy/emacs-shell--shpop-types :eshell)))))
   (entropy/shellpop-start)))

;; ** provide
(provide 'entropy-emacs-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
