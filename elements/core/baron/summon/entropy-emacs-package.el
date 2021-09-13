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

(require 'package)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-message)

;; ** Prepare
;; *** Disable auto save `package-selected-packages'

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun __adv/override/save-selected-packages/for-disable-auto-save (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages
            :override
            #'__adv/override/save-selected-packages/for-disable-auto-save)

;; *** Package archive set
;;
(defvar-local entropy/emacs-package--package-archives-list '(melpa emacs-china tuna tencent))

(defun entropy/emacs-package-set-package-archive-repo (archives)
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
    (setq package-archives archives)))
  (entropy/emacs-message-do-message "Set package archives to '%s'." archives))

(defun entropy/emacs-package--initial-package-archive ()
  (unless (or (eq entropy/emacs-ext-elpkg-get-type
                  'entropy-emacs-extenisons-project)
              (eq entropy/emacs-ext-elpkg-get-type
                  'entropy-emacs-extensions-project-build))
    (entropy/emacs-package-set-package-archive-repo
     entropy/emacs-package-archive-repo)))

;; *** Format package-gnupghome-dir format for Msys2
(defun entropy/emacs-package--refresh-gnupg-homedir ()
  "We don't use the default `package-gnupghome-dir' when
`entropy/emacs-wsl-enable' for the reason that the gnupg exec in
`entropy/emacs-wsl-apps' can not recognize the corrent path
argument."
  (when (and entropy/emacs-wsl-enable
             entropy/emacs-wsl-apps
             (executable-find "gpg")
             (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
    (setq package-gnupghome-dir nil)))

;; *** Initialize packages
(defun entropy/emacs-package--package-initialize (&optional force)
  (unless (version< emacs-version "27")
    (setq package-quickstart nil))
  (when force
    (setq package--initialized nil)
    (setq load-path (copy-tree entropy/emacs-origin-load-path))
    (setq package-alist nil)
    (setq package-activated-list nil))
  (entropy/emacs-message-do-message "Custom packages initializing ......")
  (package-initialize)
  (entropy/emacs-message-do-message "Custom packages initializing done!"))

;; *** prepare main
(defvar entropy/emacs-package-prepare-done nil)

(defun entropy/emacs-package-prepare-foras (&optional force)
  "Prepare for package referred operation when
`entropy/emacs-package-prepare-done' was nil, or set FORCE for
the force way.

This procedure will refresh all packages status."
  (when (or (null entropy/emacs-package-prepare-done)
            force)
    (entropy/emacs-set-package-user-dir)
    (entropy/emacs-package--initial-package-archive)
    (when sys/win32p
      (entropy/emacs-package--refresh-gnupg-homedir))
    (entropy/emacs-package--package-initialize t)
    (unless entropy/emacs-package-prepare-done
      (setq entropy/emacs-package-prepare-done t)))
  ;; --- debug use ---
  ;; (print package--downloads-in-progress)
  ;; (print package-user-dir)
  ;; (print package-archives)
  )

;; ** Package install subroutines
;; *** core
(defvar entropy/emacs-package-install-failed-list nil)
(defvar entropy/emacs-package-install-success-list nil)

(defun entropy/emacs-package-package-archive-empty-p ()
  "Check the package archive dir status in `package-user-dir'.

Return t for exists status or nil for otherwise."
  (entropy/emacs-package-prepare-foras)
  (let ((pkg-archive-dir (expand-file-name "archives" package-user-dir)))
    (if (and (file-exists-p pkg-archive-dir)
             (entropy/emacs-list-files-recursive-for-list pkg-archive-dir))
        nil
      t)))

(defun entropy/emacs-package-install-package (update print-prefix &rest args)
  "Install/update package of pkg in car of ARGS.

Update package when UPDATE was non-nil.

When installing encounters the fatal error, put the pkg into
`entropy/emacs-package-install-failed-list'."
  (let ((current-pkgs (copy-tree package-alist))
        (pkg (car args))
        install-pass
        install-core-func
        error-rtn)
    (setq install-core-func
          (lambda (&optional not-prompt)
            (setq install-pass
                  (condition-case error
                      (let ((inhibit-message not-prompt))
                        (apply 'package-install args)
                        (push pkg entropy/emacs-package-install-success-list))
                    (t (prog1 'notpassed
                         (setq error-rtn error)))))
            (when (eq install-pass 'notpassed)
              (push (cons pkg error-rtn)
                    entropy/emacs-package-install-failed-list))))
    (when update
      (package-delete (car (alist-get pkg current-pkgs)) t))
    ;; install package after package archvie contents refresh
    ;; when needed.
    (unless (ignore-errors (assoc package package-archive-contents))
      (package-refresh-contents))
    (if (not noninteractive)
        (funcall install-core-func)
      (if print-prefix
          (entropy/emacs-message-do-message
           "%s [%s] package '%s' ..."
           print-prefix
           (blue (if update "Updating" "Installing"))
           (yellow (symbol-name pkg)))
        (entropy/emacs-message-do-message
         "[%s] package '%s' ..."
         (blue (if update "Updating" "Installing"))
         (yellow (symbol-name pkg))))
      (cond ((and (package-installed-p pkg)
                  (null update))
             (entropy/emacs-message-do-message
              (dark (white "⚠ ALREADY INSTALLED"))))
            (t
             (funcall install-core-func t)
             (if (not (eq install-pass 'notpassed))
                 (entropy/emacs-message-do-message
                  (green "✓ DONE"))
               (entropy/emacs-message-do-message
                "%s -- %s"
                (red "✕ FAILED")
                (cdr error-rtn))))))))

;; *** error prompt for failing items

(defun entropy/emacs-package-prompt-install-fails ()
  (when entropy/emacs-package-install-failed-list
    ;; Add stdout newline after install message when in batch-mode
    (when noninteractive
      (princ "\n")
      (princ "\n"))
    (let ((count 1))
      (dolist (pkg entropy/emacs-package-install-failed-list)
        (entropy/emacs-message-do-message
         "%s: %s '%s' because of '%s'"
         (yellow (number-to-string count))
         (red "failed to install pkg")
         (yellow (symbol-name (car pkg)))
         (cdr pkg))
        (cl-incf count)))
    (if noninteractive
        (error "")
      (switch-to-buffer "*Messages*")
      (top-level))))

;; *** install
(defun entropy/emacs-package-install-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (entropy/emacs-message-do-message
   (blue "Checking extensions satisfied status ..."))
  (require 'entropy-emacs-package-requirements)
  (let ((package-check-signature nil)
        (count 1))
    (dolist (package entropy-emacs-packages)
      (unless (or (null package)
                  (package-installed-p package))
        (ignore-errors
          (entropy/emacs-package-install-package
           nil
           (format "[%s/%s]" count (length entropy-emacs-packages))
           package
           ))
        (cl-incf count))))
  (entropy/emacs-package-prompt-install-fails)
  (entropy/emacs-message-do-message
   (green "All packages installed, congratulations 👏")))

;; *** update
(defun entropy/emacs-package-update-all-packages ()
  (entropy/emacs-package-prepare-foras)
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
          (entropy/emacs-package-install-package t nil pkg-id))
        (entropy/emacs-package-prompt-install-fails)
          ;;; FIXME: package reinitialize after updates cause error
          ;;; for `yasnippet-snippets' that for autroload deleted old
          ;;; version..
        ;;(entropy/emacs-package--package-initialize t)
        ))))

;; ** Use-package inititialize
;; Required by `use-package'

(defun entropy/emacs-package-init-use-package  ()
  (require 'use-package)
  (if entropy/emacs-fall-love-with-pdumper
      (setq use-package-always-ensure nil)
    (setq use-package-always-ensure t))

  (setq use-package-always-defer entropy/emacs-custom-enable-lazy-load
        use-package-always-demand
        (not entropy/emacs-custom-enable-lazy-load))
  (unless (or entropy/emacs-fall-love-with-pdumper
              (not entropy/emacs-custom-enable-lazy-load))
    (setq use-package-expand-minimally t))
  (setq use-package-enable-imenu-support t)

  ;; add self built `use-package' keys
  (entropy/emacs-package--use-package-add-keyword :eemacs-functions)
  (entropy/emacs-package--use-package-add-keyword :eemacs-macros)

  (use-package diminish
    :commands (diminish))
  (use-package bind-key
    :commands (bind-key)))

(defun entropy/emacs-package--use-package-add-keyword (keyword &optional precedence)
  (let ((precedence (or precedence :commands)))
    (setq use-package-keywords
          ;; should go in the same location as :bind
          (cl-loop for item in use-package-keywords
                   if (eq item precedence)
                   collect precedence and collect keyword
                   else
                   ;; don't add duplicates
                   unless (eq item keyword)
                   collect item))))

(defalias 'use-package-normalize/:eemacs-functions
  'use-package-normalize-symlist)

(defun use-package-handler/:eemacs-functions (name _keyword arg rest state)
  "The `use-package' handler for key ':eemacs-functions' which
place can host a function symbol or a list of thus. All symbols
are recognized as a normal function."
  (use-package-concat
   ;; Since we deferring load, establish any necessary autoloads, and also
   ;; keep the byte-compiler happy.
   (let ((name-string (use-package-as-string name)))
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             (unless (plist-get state :demand)
               `((unless (fboundp ',command)
                   (autoload #',command ,name-string))))
             (when (bound-and-true-p byte-compile-current-file)
               `((eval-when-compile
                   (declare-function ,command ,name-string)))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

(defalias 'use-package-normalize/:eemacs-macros
  'use-package-normalize-symlist)

(defun use-package-handler/:eemacs-macros (name _keyword arg rest state)
  "The `use-package' handler for key ':eemacs-macros' which place
can host a macro symbol or a list of thus. All symbols are
recognized as a normal macro."
  (use-package-concat
   (let ((name-string (use-package-as-string name)))
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             (unless (plist-get state :demand)
               `((unless (fboundp ',command)
                   (autoload #',command ,name-string nil nil t))))
             nil)))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))


;; ** common start

;; Disable `help-mode' auto load library for its doc render request
;; since we needed pure charge of how packages are loading for.
(setq help-enable-autoload nil
      help-enable-completion-autoload nil)

(defun entropy/emacs-package-common-start ()
  (entropy/emacs-package-install-all-packages)
  (entropy/emacs-package-init-use-package)
  (run-hooks 'entropy/emacs-package-common-start-after-hook))

;; * Provide
(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
