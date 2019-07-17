;;; entropy-emacs-package.el --- entropy-emacs package management configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-package.el
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
;; Setting `package.el' with `entropy-emacs' specification with
;; `use-package' delaying loading procedure. 
;;
;; * Configuration:
;;
;; Non manually configuration mentioned.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** main
;;set package-user-dir position
(defun entropy/emacs-package--set-user-package-dir (version)
  "Setting `package-user-dir' based on emacs version name located
for `user-emacs-directory'."
  (setq-default package-user-dir
                (expand-file-name
                 (concat "elpa-" version)
                 (expand-file-name entropy/emacs-ext-extensions-elpa-dir))))

;; Set customized package install directory
(if (and (member emacs-version '("25.2.1" "25.3.1" "26.1" "26.2" "27.0.50"))
         (eq entropy/emacs-use-extensions-type 'origin))
    (entropy/emacs-package--set-user-package-dir emacs-version)
  (cond
   ((and (equal emacs-version "25.2.2")
         (eq entropy/emacs-use-extensions-type 'origin))
    (entropy/emacs-package--set-user-package-dir "25.2.1"))
   ((eq entropy/emacs-use-extensions-type 'origin)
    (error "Unsupport emacs version '%s'" emacs-version))))


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
(defvar-local package-archives-list '(melpa emacs-china tuna tencent))

(defun entropy/emacs-package-set-package-archive-location (archives)
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
   ((eq archives 'tencent)
    (setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                             ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                             ("org"   . "http://mirrors.cloud.tencent.com/elpa/org/")
                             )))
   (t
    (error "Unknown archives: '%s'" archives)))
  (message "Set package archives to '%s'." archives))

(entropy/emacs-package-set-package-archive-location entropy/emacs-package-archive-repo)


;; Remove internal org-mode path
(let ((match-rexp  (concat (substring data-directory 0 -5) ".*/org$"))
      (temp_path (copy-tree load-path)))
  (catch :exit
    (dolist (el temp_path)
      (when (string-match match-rexp el)
        (setq temp_path (remove el temp_path))
        (throw :exit nil))))
  (setq load-path temp_path))


;; Initialize packages
(setq package-enable-at-startup nil)    ; To prevent initialising twice
(if (not (version< emacs-version "27"))
    (setq package-quickstart t))
(message "Packages initializing ......")
(package-initialize)
(message "Packages initializing done!")

;; Format package-gnupghome-dir format for Msys2
(when (and entropy/emacs-wsl-enable
           entropy/emacs-wsl-apps
           (executable-find "gpg")
           (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
  (setq package-gnupghome-dir nil))

;; Install entropy-emacs pre installed packages
(when (equal entropy/emacs-use-extensions-type 'origin)
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
(unless (eq entropy/emacs-use-extensions-type 'submodules)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

(if (eq entropy/emacs-use-extensions-type 'submodules)
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
(unless (not entropy/emacs-minimal-start)
  (use-package ivy)
  (use-package dash))


;; Initialization benchmark
(when entropy/emacs-initialize-benchmark-enable
  (use-package benchmark-init
    :init
    ;; (benchmark-init/activate)
    ;; (add-hook 'after-init-hook #'benchmark-init/deactivate)
    ))

(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
