;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Changed by: Entropy
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Configurations for C/C++.
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
;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** Preparation
(defun entropy/emacs-c-cc-mode-hook ()
  (c-set-style "bsd")
  (setq tab-width 4)
  (setq c-basic-offset 4))

(defun entropy/emacs-c-irony-load-subs ()
  (dolist (el '(irony-cdb-clang-complete
                irony-cdb-json
                irony-cdb-libclang
                irony-cdb
                irony-completion
                irony-diagnostics
                irony-iotask
                irony-snippet))
    (require el)))

(defun entropy/emacs-c-irony-pipe-config ()
  "Reducing pipe-read-delay and set the pipe buffer size to
64K on Windows (from the original 4K).

It is the recommendation of irony-mode official introduction."
  (when (boundp 'w32-pipe-read-delay)
    (setq-local w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq-local irony-server-w32-pipe-buffer-size (* 64 1024))))


(defun entropy/c-derived-mode-company-special-key (command &rest args)
  "Special key binding 'C-M-/' for c derived modes, only using for
company referents.

This function used for do sth like:

If you want to bind sth with key 'C-M-/' for `c-mode' but don't
let it be derived to the c derived mode like `php-mode'. Or others
oppsite."
  (interactive (list 'interactive))
  (cond
   ((or (equal major-mode 'c-mode)
        (equal major-mode 'c++-mode))
    (funcall-interactively 'company-c-headers command))
   (t (message (format "Haven't binded sth to 'C-M-/' yet in %s ." major-mode)))))
  
;; ** C/C++ Mode
(use-package cc-mode
  :ensure nil
  :init
  (add-hook 'c-mode-common-hook
            #'entropy/emacs-c-cc-mode-hook)
  :config
  (define-key c-mode-map (kbd "C-M-/") 'entropy/c-derived-mode-company-special-key)
  (define-key c++-mode-map (kbd "C-M-/") 'entropy/c-derived-mode-company-special-key)


;; *** compiler use gcc or g++ in windows opertion system
  (when (and sys/win32p
             entropy/emacs-win-portable-mingw-enable
             (executable-find "x86_64-w64-mingw32-g++.exe")
             (executable-find "x86_64-w64-mingw32-gcc.exe"))
    (defun entropy/emacs-win-c-compiler ()
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

    (defun entropy/emacs-win-c-open-exec ()
      "Open exec file(windows only) which compiled by current C or
Cpp file, if no such file named by like:

current-file-name.exe

will quit and message one error info.

This function must be ran after `entropy/emacs-win-c-compiler'.
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
        (define-key map (kbd "<f5>") 'entropy/emacs-win-c-compiler)
        (define-key map (kbd "<C-f5>") 'entropy/emacs-win-c-open-exec)))))


;; ** irony mode
(defun entropy/emacs-c-irony-refer-advice-around (oldfuc &rest args)
  "Prevent c group mode as `php-mode' which was the derived from
`c-mode' to load `irony-mode' and `company-irony'."
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (funcall oldfuc)
    t))

(defun entropy/usepackage-irony ()
  "Function for enabling irony mode for c and c++ mode."
  (use-package irony
    :init (advice-add 'irony-mode :around #'entropy/emacs-c-irony-refer-advice-around)
    :commands (irony-mode)
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options)
           (c-mode . entropy/emacs-c-irony-pipe-config)
           (c++-mode . entropy/emacs-c-irony-pipe-config))
    :config
    (entropy/emacs-c-irony-load-subs))

  (use-package irony-eldoc
    :defines irony-mode-hook
    :commands (irony-eldoc)
    :hook (irony-mode . irony-eldoc)))

(cond
 ((and sys/win32p
       entropy/emacs-win-portable-mingw-enable
       (file-exists-p (concat entropy/emacs-win-portable-mingw-path "libclang.dll")))
  (entropy/usepackage-irony))
 ((or sys/linuxp 
      sys/linux-x-p
      sys/mac-x-p
      sys/macp)
  (entropy/usepackage-irony)))

;; ** provide
(provide 'entropy-emacs-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
