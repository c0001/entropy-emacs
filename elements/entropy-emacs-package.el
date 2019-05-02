;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-
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
;;             Package configurations.
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
;;set package-user-dir position
(defun entropy/set-package-dir (version)
  "Setting `package-user-dir' based on emacs version name located
for `user-emacs-directory'."
  (setq-default package-user-dir
                (expand-file-name
                 (concat "elpa-" version)
                 (expand-file-name entropy/ext-extensions-elpa-dir))))

;; Set customized package install directory
(if (and (member emacs-version '("25.2.1" "25.3.1" "26.1" "27.0.50"))
         (equal entropy/use-extensions-type 'origin))
    (entropy/set-package-dir emacs-version)
  (cond
   ((equal emacs-version "25.2.2")
    (entropy/set-package-dir "25.2.1"))))


;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(with-eval-after-load 'package
  (defun package--save-selected-packages (&optional value)
    "Set and (don't!) save `package-selected-packages' to VALUE."
    (when value
      (setq package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;;
;; ELPA: refer to https://elpa.emacs-china.org/
;;
(defvar-local package-archives-list '(melpa emacs-china tuna))

(defun entropy/set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             package-archives-list))))
  (cond
   ((eq archives 'melpa)
    (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                             ("melpa" . "http://melpa.org/packages/")
                             ("org"   . "https://orgmode.org/elpa/")
                             )))
   ((eq archives 'emacs-china)
    (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                             ("melpa" . "http://elpa.emacs-china.org/melpa/")
                             ("org"   . "http://elpa.emacs-china.org/org/")
                             )))
   ((eq archives 'tuna)
    (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                             ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                             ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                             )))
   (t
    (error "Unknown archives: '%s'" archives)))
  (message "Set package archives to '%s'." archives))

(entropy/set-package-archives entropy/package-archive-repo)

;; Initialize packages
(setq package-enable-at-startup nil)    ; To prevent initialising twice
(if (not (version< emacs-version "27"))
    (setq package-quickstart t))
(message "Packages initializing ......")
(package-initialize)
(message "Packages initializing done!")

;; Format package-gnupghome-dir format for Msys2
(when (and entropy/wsl-enable
           entropy/wsl-apps
           (executable-find "gpg")
           (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
  (let ((temp-str package-gnupghome-dir))
    (setq temp-str (replace-regexp-in-string
                    "^\\(.\\):"
                    "/\\1" temp-str)
          package-gnupghome-dir temp-str)))

;; Install entropy-emacs pre installed packages
(when (equal entropy/use-extensions-type 'origin)
  (require 'entropy-emacs-package-requirements)
  (let ((package-check-signature nil))
    (dolist (package entropy-emacs-packages)
      (unless (or (null package)
                  (package-installed-p package))
        (if (ignore-errors (assoc package package-archive-contents))
            (ignore-errors (package-install package))
          (package-refresh-contents)
          (ignore-errors (package-install package)))))))

;; Setup `use-package'
(unless (eq entropy/use-extensions-type 'submodules)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

(if (eq entropy/use-extensions-type 'submodules)
    (setq use-package-always-ensure nil)
  (setq use-package-always-ensure t))
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish
  :commands
  (diminish
   diminish-undo
   diminished-modes))
(use-package bind-key
  :commands
  (
   bind-key
   bind-key*
   bind-keys
   bind-keys*
   bind-keys-form
   unbind-key
   ))

;; Use ivy and dash (They are the basic dependencies for entropy-emacs)
(unless (not entropy/minimal-start)
  (use-package ivy)
  (use-package dash))


;; Initialization benchmark
(when entropy/initialize-benchmark-enabled
  (use-package benchmark-init
    :init
    ;; (benchmark-init/activate)
    ;; (add-hook 'after-init-hook #'benchmark-init/deactivate)
    ))

(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
