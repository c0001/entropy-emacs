;;; entropy-emacs-c.el --- entroy emacs C config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-c.el
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
;; This file was config of emacs major mode for =C= referrence,
;; config of C file coding style, major mode ehancement and other
;; minor tools.
;;
;; Some special tools for WIN32 platform using Msys2 *NIX environment
;; emulator, and common config for cross platform.
;;
;; * Configuration:
;;
;; There's no support to loading this file out of entropy-emacs unless
;; the hacking way.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** Preparation
(defun entropy/emacs-c-cc-mode-hook ()
  (c-set-style "bsd")
  (setq tab-width 4)
  (setq c-basic-offset 4))

(defun entropy/emacs-c-derived-mode-company-special-key (command &rest args)
  "Special key binding 'C-x M-/' for c derived modes, only using for
company referents.

This function used for do sth like:

If you want to bind sth with key 'C-x M-/' for `c-mode' but don't
let it be derived to the c derived mode like `php-mode'. Or others
oppsite."
  (interactive (list 'interactive))
  (cond
   ((or (equal major-mode 'c-mode)
        (equal major-mode 'c++-mode))
    (funcall-interactively 'company-c-headers command))
   (t (message (format "Haven't binded sth to 'C-x M-/' yet in %s ." major-mode)))))

;; ** main
(use-package cc-mode
  :ensure nil
  :init
  (add-hook 'c-mode-common-hook
            #'entropy/emacs-c-cc-mode-hook)
  :config
  (define-key c-mode-map (kbd "C-x M-/") 'entropy/emacs-c-derived-mode-company-special-key)
  (define-key c++-mode-map (kbd "C-x M-/") 'entropy/emacs-c-derived-mode-company-special-key))

;; ** benefits tools
;; *** compiler use gcc or g++ in windows opertion system
(when (and sys/win32p
           entropy/emacs-win-portable-mingw-enable
           (executable-find "x86_64-w64-mingw32-g++.exe")
           (executable-find "x86_64-w64-mingw32-gcc.exe"))
  (defun entropy/emacs-c-win-c-compiler ()
    "Compile C or Cpp file with their parameter:

1. `entropy/emacs-win-gcc-parameter'
2. `entropy/emacs-win-g++-parameter'


Til now, this function just compatibility with windows
plattform."
    (interactive)
    (let* ((file (buffer-file-name))
           (fname (file-name-base))
           (c-type (replace-regexp-in-string
                    (regexp-quote ".") "" (url-file-extension file))))
      (cond
       ((string= c-type "c")
        (shell-command
         (concat
          (concat entropy/emacs-win-portable-mingw-path "x86_64-w64-mingw32-gcc.exe")
          (concat " -o "
                  (concat (file-name-directory file)  fname ".exe") " " file
                  " " entropy/emacs-win-gcc-parameter))))
       ((or (string= c-type "cpp")
            (string= c-type "c++")
            (string= c-type "cxx"))
        (shell-command
         (concat
          (concat entropy/emacs-win-portable-mingw-path "x86_64-w64-mingw32-g++.exe")
          (concat " -o "
                  (concat (file-name-directory file)  fname ".exe") " " file
                  " " entropy/emacs-win-g++-parameter))))
       (t
        (user-error "This file was not C or Cpp file!")))))

  (defun entropy/emacs-c-win-c-open-exec ()
    "Open exec file(windows only) which compiled by current C or
Cpp file, if no such file named by like:

current-file-name.exe

will quit and message one error info.

This function must be ran after `entropy/emacs-c-win-c-compiler'.
"
    (interactive)
    (let* (exefile
           (file (buffer-file-name))
           (fname (file-name-base)))
      (if (and (file-exists-p (concat fname ".exe"))
               (or (string-match-p "\\.c$" file)
                   (string-match-p "\\.c++$" file)
                   (string-match-p "\\.cpp$" file)))
          (progn
            (setq exefile (concat fname ".exe"))
            (w32-shell-execute "open" (concat (file-name-directory (buffer-file-name)) exefile)))


        (let ((file-type (replace-regexp-in-string
                          (regexp-quote ".") "" (url-file-extension file)))
              current-type)

          (cond
           ((string= file-type "c")
            (setq current-type file-type))
           ((or (string= file-type "cpp")
                (string= file-type "c++")
                (string= file-type "cxx"))
            (setq current-type file-type))
           (t
            (setq current-type "None")))

          (if (not (string= current-type "None"))
              (user-error "There's no compiled file found with this %s file." current-type)
            (user-error "This file was not C or Cpp file, exactly be without compiled exec file."))))))

  ;; define key binding with above two function for 'c-mode' and 'c++-mode'
  (let ((mode-map `(,c-mode-map ,c++-mode-map)))
    (dolist (map mode-map)
      (define-key map (kbd "<f5>") 'entropy/emacs-c-win-c-compiler)
      (define-key map (kbd "<C-f5>") 'entropy/emacs-c-win-c-open-exec))))

;; ** provide
(provide 'entropy-emacs-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
