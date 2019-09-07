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
(require 'entropy-emacs-defun)

;; ** Specify `package-user-dir'
(entropy/emacs-set-package-user-dir)

;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(entropy/emacs-lazy-load-simple 'package
  (defun package--save-selected-packages (&optional value)
    "Set and (don't!) save `package-selected-packages' to VALUE."
    (when value
      (setq package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;;
;; ** ELPA: refer to https://elpa.emacs-china.org/
;;
(defvar-local entropy/emacs-package--package-archives-list '(melpa emacs-china tuna tencent))

(defun entropy/emacs-package-set-package-archive-location (archives)
  "Switch to specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             entropy/emacs-package--package-archives-list))))
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

(unless (eq entropy/emacs-use-extensions-type 'submodules-melpa-local)
  (entropy/emacs-package-set-package-archive-location entropy/emacs-package-archive-repo))

;; ** Initialize packages
(when (and (entropy/emacs-package-is-upstream)
           (not entropy/emacs-fall-love-with-pdumper))
  (unless (version< emacs-version "27")
    (setq package-quickstart nil))
  (message "Custom packages initializing ......")
  (package-initialize)
  (message "Custom packages initializing done!"))

;; ** Format package-gnupghome-dir format for Msys2
(when (and entropy/emacs-wsl-enable
           entropy/emacs-wsl-apps
           (executable-find "gpg")
           (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
  (setq package-gnupghome-dir nil))

;; ** Install entropy-emacs pre installed packages
(when (entropy/emacs-package-is-upstream)
  (require 'entropy-emacs-package-requirements)
  (let ((package-check-signature nil))
    (dolist (package entropy-emacs-packages)
      (unless (or (null package)
                  (package-installed-p package))
        (if (ignore-errors (assoc package package-archive-contents))
            (ignore-errors (package-install package))
          (package-refresh-contents)
          (ignore-errors (package-install package)))))))


;; ** Required by `use-package'
(eval-when-compile
  (require 'use-package))

(if (or (eq entropy/emacs-use-extensions-type 'submodules)
        entropy/emacs-fall-love-with-pdumper)
    (setq use-package-always-ensure nil)
  (setq use-package-always-ensure t))

(setq use-package-always-defer entropy/emacs-custom-enable-lazy-load
      use-package-always-demand entropy/emacs-custom-enable-lazy-load)
(unless entropy/emacs-fall-love-with-pdumper
  (setq use-package-expand-minimally t))
(setq use-package-enable-imenu-support t)

(use-package diminish
  :commands (diminish))
(use-package bind-key
  :commands (bind-key))

;; ** Initialization benchmark
(when entropy/emacs-initialize-benchmark-enable
  (use-package benchmark-init
    :init
    ;; (benchmark-init/activate)
    ;; (add-hook 'after-init-hook #'benchmark-init/deactivate)
    ))

;; * Provide 
(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
