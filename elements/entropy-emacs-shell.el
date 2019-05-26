;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Modified By: Entropy
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Shell configurations.
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

;; ** Company mode backend for shell functions
(with-eval-after-load 'company
  (use-package company-shell
    :commands (company-shell company-shell-env company-fish-shell)
    :init (cl-pushnew '(company-shell company-shell-env company-fish-shell) company-backends)))

;; ** eshell
(use-package eshell
  :ensure nil
  :init
  (defun entropy/emacs-shell--eshell-before-advice (&rest args)
    "Delete eshell histroy file before eshell opening and prevent
open eshell on tramp-buffer when on windows platform. "
    (if (file-exists-p eshell-history-file-name)
        (delete-file eshell-history-file-name))
    (when (and (string-match "^/\\w+?:" default-directory)
               sys/win32p)
      (error "Can not using eshell in tramp location.")))
  
  (with-eval-after-load 'eshell
    (advice-add 'eshell :before #'entropy/emacs-shell--eshell-before-advice))
  
  (add-hook 'eshell-exit-hook #'(lambda ()
                                  (if (file-exists-p eshell-history-file-name)
                                      (delete-file eshell-history-file-name))))
  ;; Require eshell package forcely for reducing calling delay
  (require 'eshell)
  (require 'em-basic)
  (require 'em-alias)
  (require 'em-banner)
  (require 'em-cmpl)
  (require 'em-dirs)
  (require 'em-glob)
  (require 'em-hist)
  (require 'em-ls)
  (require 'em-prompt)
  (require 'em-script)
  (require 'em-term)
  (require 'em-unix)
  :config
  ;; List all candidates when enter 'tab' key
  (setq eshell-cmpl-cycle-completions nil)

  ;; Value assignment
  (setq eshell-aliases-file entropy/emacs-eshell-alias-file
        eshell-history-file-name entropy/emacs-eshell-history-file)



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


;; ** Multi term
(use-package multi-term)

;; ** Shell Pop
(use-package shell-pop
  :defines shell-pop-universal-key
  :bind ([f9] . entropy/emacs-shell-shell-pop-for-eshell)
  :init
  (defun entropy/emacs-shell--shell-pop-before-advice (&rest args)
    (when (string-match "^/\\w+?:" default-directory)
      (error "Can not usign eshell in tramp location.")))

  (advice-add 'entropy/emacs-shell-shell-pop-for-eshell
              :before #'entropy/emacs-shell--shell-pop-before-advice)
  
  (defun entropy/emacs-shell-shell-pop-for-eshell (arg)
    (interactive "P")
    (require 'shell-pop)
    (let* ((value '("eshell" "*eshell*" (lambda () (eshell))))
           (shell-pop-internal-mode (nth 0 value))
           (shell-pop-internal-mode-buffer (nth 1 value))
           (shell-pop-internal-mode-func (nth 2 value)))
      (shell-pop arg)))

  (defun entropy/emacs-shell--shell-pop-make-pop-ansi-term ()
    (advice-add 'entropy/emacs-shell-shell-pop-for-ansi-term-bash
                :before #'entropy/emacs-shell--shell-pop-before-advice)
    (defun entropy/emacs-shell-shell-pop-for-ansi-term-bash (arg)
      (interactive "P")
      (require 'shell-pop)
      (let* ((shell-file-name (if sys/win32p (expand-file-name "bash.exe" entropy/emacs-wsl-apps) "bash"))
             (shell-pop-term-shell shell-file-name)
             (value '("ansi-term" "*ansi-term*"
                      (lambda () (ansi-term shell-pop-term-shell))))
             (shell-pop-internal-mode (nth 0 value))
             (shell-pop-internal-mode-buffer (nth 1 value))
             (shell-pop-internal-mode-func (nth 2 value)))
        (shell-pop arg)))
    (global-set-key (kbd "<f10>") 'entropy/emacs-shell-shell-pop-for-ansi-term-bash))

  ;; ansi-term for linux-like operation system
  (when (not sys/win32p)
    (entropy/emacs-shell--shell-pop-make-pop-ansi-term))

  ;; ansi-term for windows
  (use-package fakecygpty
    :ensure nil
    :if (and sys/win32p
             entropy/emacs-win-fakecygpty-enable
             (executable-find "fakecygpty")
             (executable-find "qkill"))
    :commands fakecygpty-activate
    :init
    (fakecygpty-activate)
    :config
    (defun entropy/emacs-shell--cd-around-advice (old_func dir)
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
    (advice-add 'cd :around 'entropy/emacs-shell--cd-around-advice)
    
    (cond ((and entropy/emacs-wsl-enable
                (ignore-errors (file-exists-p entropy/emacs-wsl-apps)))
           (entropy/emacs-shell--shell-pop-make-pop-ansi-term))
          
          (t
           (advice-add 'entropy/emacs-shell-pop-for-ansi-term-cmd
                       :before #'entropy/emacs-shell--shell-pop-before-advice)
           (defun entropy/emacs-shell-pop-for-ansi-term-cmd  (arg)
             (interactive "P")
             (require 'shell-pop)
             (let* ((shell-pop-term-shell shell-file-name)
                    (value '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
                    (shell-pop-internal-mode (nth 0 value))
                    (shell-pop-internal-mode-buffer (nth 1 value))
                    (shell-pop-internal-mode-func (nth 2 value)))
               (shell-pop arg)))
           (global-set-key (kbd "<f10>") 'entropy/emacs-shell-pop-for-ansi-term-cmd)))))



;; ** provide
(provide 'entropy-emacs-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
