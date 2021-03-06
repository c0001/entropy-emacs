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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-message)

;; ** Patched for selected-packages 
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
;; ** Prepare
;; *** Package archive set 
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

(defun entropy/emacs-package--initial-package-archive ()
  (unless (eq entropy/emacs-use-extensions-type 'submodules-melpa-local)
    (entropy/emacs-package-set-package-archive-location
     entropy/emacs-package-archive-repo)))

;; *** Format package-gnupghome-dir format for Msys2
(defun entropy/emacs-package--refresh-gnupg-homedir  ()
  (when (and entropy/emacs-wsl-enable
             entropy/emacs-wsl-apps
             (executable-find "gpg")
             (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
    (setq package-gnupghome-dir nil)))

;; *** Initialize packages
(defun entropy/emacs-package--package-initialize (&optional force)
  (when (and (entropy/emacs-package-is-upstream)
             (not entropy/emacs-fall-love-with-pdumper))
    (unless (version< emacs-version "27")
      (setq package-quickstart nil))
    (when force
      (setq package--initialized nil)
      (setq load-path (copy-tree entropy/emacs-origin-load-path))
      (setq package-alist nil)
      (setq package-activated-list nil))
    (message "Custom packages initializing ......")
    (package-initialize)
    (message "Custom packages initializing done!")))

;; *** prepare main
(defvar entropy/emacs-package-prepare-done nil)

(defun entropy/emacs-package-prepare-foras (&optional force)
  (when (or (null entropy/emacs-package-prepare-done)
            force)
    (entropy/emacs-set-package-user-dir)
    (entropy/emacs-package--initial-package-archive)
    (entropy/emacs-package--refresh-gnupg-homedir)
    (entropy/emacs-package--package-initialize force)
    (unless entropy/emacs-package-prepare-done
      (setq entropy/emacs-package-prepare-done t))))

;; ** Package install subroutines
;; *** core
(defvar entropy/emacs-package-install-failed-list nil)

(defun entropy/emacs-package-package-archive-empty-p ()
  (entropy/emacs-set-package-user-dir)
  (let ((pkg-archive-dir (expand-file-name "archives" package-user-dir)))
    (if (and (file-exists-p pkg-archive-dir)
             (entropy/emacs-list-files-recursive-for-list pkg-archive-dir))
        nil
      t)))

(defun entropy/emacs-package-install-package (update &rest args)
  (let ((current-pkgs (copy-tree package-alist))
        (pkg (car args))
        install-pass)
    (when update
      (package-delete (car (alist-get pkg current-pkgs)) t))
    (if (not noninteractive)
        (apply 'package-install args)
      (entropy/emacs-message-do-message
       "[%s] package '%s' ..."
       (blue (if update "Updating" "Installing"))
       (yellow (symbol-name pkg)))
      (cond ((and (package-installed-p pkg)
                  (null update))
             (entropy/emacs-message-do-message
              (dark (white "⚠ ALREADY INSTALLED"))))
            (t
             (setq install-pass
                   (condition-case nil
                       (let ((inhibit-message t)) (apply 'package-install args))
                     (error 'notpassed)))
             (if (not (eq install-pass 'notpassed))
                 (entropy/emacs-message-do-message
                  (green "✓ DONE"))
               (entropy/emacs-message-do-message
                (red "✕ FAILED"))
               (push pkg entropy/emacs-package-install-failed-list)))))))

;; *** error prompt for failing items

(defun entropy/emacs-package-prompt-install-fails ()
  (when entropy/emacs-package-install-failed-list
    (let ((count 1))
      (dolist (pkg entropy/emacs-package-install-failed-list)
        (entropy/emacs-message-do-message
         "%s: %s '%s'"
         (yellow (number-to-string count))
         (red "failed to install pkg")
         (yellow (symbol-name pkg)))
        (cl-incf count)))
    (error "")))

;; *** install
(defun entropy/emacs-package-install-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (when (entropy/emacs-package-is-upstream)
    (entropy/emacs-message-do-message
     (blue "Checking extensions satisfied status ..."))
    (require 'entropy-emacs-package-requirements)
    (let ((package-check-signature nil))
      (dolist (package entropy-emacs-packages)
        (unless (or (null package)
                    (package-installed-p package))
          (if (ignore-errors (assoc package package-archive-contents))
              (ignore-errors (entropy/emacs-package-install-package nil package))
            (package-refresh-contents)
            (ignore-errors
              (entropy/emacs-package-install-package nil package))))))
    (entropy/emacs-package-prompt-install-fails)
    (entropy/emacs-message-do-message
     (green "All packages installed, congratulations 👏"))))

;; *** update
(defun entropy/emacs-package-update-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (when (entropy/emacs-package-is-upstream)
    (let ((current-pkgs (copy-tree package-alist))
          (new-pkgs (progn (package-refresh-contents)
                           (copy-tree package-archive-contents)))
          updates)
      (dolist (pkg current-pkgs)
        (let* ((pkg-id (car pkg))
               (pkg-desc-cur (cadr pkg))
               (pkg-desc-new (car (alist-get
                                   pkg-id
                                   new-pkgs)))
               (outdated (ignore-errors
                           (version-list-< (package-desc-version pkg-desc-cur)
                                           (package-desc-version pkg-desc-new)))))
          (when outdated
            (push pkg-id updates))))
      (if (null updates)
          (entropy/emacs-message-do-message
           (green "All packages are newest!"))
        (progn
          (entropy/emacs-message-do-message
           "%s '%s' %s"
           (green "There're")
           (yellow (number-to-string (length updates)))
           (green (concat (if (eq 1 (length updates))
                              "package"
                            "packages")
                          " will be updated after 5 seconds")))
          (sleep-for 5)
          (dolist (pkg-id updates)
            (entropy/emacs-package-install-package t pkg-id))
          (entropy/emacs-package-prompt-install-fails)
          (entropy/emacs-package--package-initialize t))))))

;; ** Use-package inititialize
;; Required by `use-package'

(defun entropy/emacs-package-init-use-package  ()
  (require 'use-package)
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

  ;; Initialization benchmark
  (when entropy/emacs-initialize-benchmark-enable
    (use-package benchmark-init
      :init
      (benchmark-init/activate)
      (add-hook 'after-init-hook #'benchmark-init/deactivate))))

;; ** common start

(defun entropy/emacs-package-common-start ()
  (entropy/emacs-package-install-all-packages)
  (entropy/emacs-package-init-use-package))

;; * Provide 
(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
