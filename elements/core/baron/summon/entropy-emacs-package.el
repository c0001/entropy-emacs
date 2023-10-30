;;; entropy-emacs-package.el --- entropy-emacs package management configuration  -*- lexical-binding: t; -*-
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
(defconst entropy/emacs-package-src-load-bytecode-p
  (string-match-p "\\.elc$" load-file-name))

(!eemacs-require 'entropy-emacs-defconst)
(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-defun)
(!eemacs-require 'entropy-emacs-message)

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
    (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                             ("melpa" . "http://elpa.zilongshanren.com/melpa/")
                             ("org"   . "http://elpa.zilongshanren.com/org/")
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
`entropy/emacs-microsoft-windows-unix-emulator-enable' for the reason that the gnupg exec in
`entropy/emacs-microsoft-windows-unix-emulator-bin-path' can not recognize the corrent path
argument."
  (when (and entropy/emacs-microsoft-windows-unix-emulator-enable
             entropy/emacs-microsoft-windows-unix-emulator-bin-path
             (executable-find "gpg")
             (string-match-p "^.:/.*usr/bin" (executable-find "gpg")))
    (setq package-gnupghome-dir nil)))

;; *** Initialize packages
(defvar __package-first-initialized nil)
(defun entropy/emacs-package--package-initialize (&optional force)
  (when force
    (setq package--initialized nil)
    (setq load-path (copy-tree entropy/emacs-origin-load-path))
    (setq package-alist nil)
    (setq package-activated-list nil))
  (if entropy/emacs-package-src-load-bytecode-p
      ;; enabel package qucikstart to speedup package initializing via
      ;; invoke `package-initialize' after the early-init
      (setq package-quickstart t
            entropy/emacs-package-init-with-quick-start-p t)
    ;; but not for non-bytecode startup occasion, since we should use
    ;; traditional packages founding mechanism to track the full
    ;; metadatas before `package-quickstart-refresh'.
    (when (and
           ;; in make session remove the old one
           (entropy/emacs-is-make-session)
           ;; ignore errors since invalid val of `package-quickstart-file'
           (ignore-errors (file-exists-p package-quickstart-file)))
      (delete-file package-quickstart-file 'trash)
      (when-let* ((elc (concat package-quickstart-file "c"))
                  (elcp (ignore-errors (file-exists-p elc))))
        (delete-file elc 'trash)))
    (setq package-quickstart nil
          entropy/emacs-package-init-with-quick-start-p nil))
  (entropy/emacs-message-do-message
   "Custom packages initializing ......"
   :force-message-while-eemacs-init t)
  (unless __package-first-initialized
    (setq entropy/emacs-package-initialize-init-timestamp
          (current-time)))
  (when (or force (not (bound-and-true-p __package-first-initialized)))
    (let ((package-quickstart-file
           (if package-quickstart package-quickstart-file
             ;; FIXME: bug of `package-active-all' which try to load a
             ;; readable `package-quickstart-file' even if it's
             ;; obsolete and we've disabled `package-quickstart'.
             (make-temp-name "__eemacs_fake_package-quickstart-file."))))
      (package-initialize)))
  (unless __package-first-initialized
    (setq entropy/emacs-package-initialize-done-timestamp
          (current-time)))
  (unless __package-first-initialized
    (setq __package-first-initialized t))
  (entropy/emacs-message-do-message
   "Custom packages initializing done!"
   :force-message-while-eemacs-init t))

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

(defun entropy/emacs-package-package-archive-empty-p (&optional try-get)
  "Check the package archive dir status in `package-user-dir'.

Return t for exists status or nil for otherwise.

If optional TRY-GET in non-nil then run
`package-refresh-contents' after checking if the result is nil
and the return is the rechecking result like above.

Addtionally, if TRY-GET is `eq' to `refresh' then always refresh
the contents whatever whether it was existed before, and the
return is as common usage."
  (entropy/emacs-package-prepare-foras)
  (if (eq try-get 'refresh) (progn (package-refresh-contents)
                                   (entropy/emacs-package-package-archive-empty-p))
    (let* ((pkg-archive-dir (expand-file-name "archives" package-user-dir))
           (rtn
            (if (and (file-exists-p pkg-archive-dir)
                     (entropy/emacs-list-dir-subfiles-recursively-for-list
                      pkg-archive-dir)) nil t)))
      (cond ((and try-get (not rtn))
             (package-refresh-contents)
             (entropy/emacs-package-package-archive-empty-p))
            (t rtn)))))

(defun entropy/emacs-package-pkg-installed-p (pkg &optional no-multi)
  "Like `package-installed-p' but when PKG is `package-desc',
require its version as the MIN-VERSION for `package-installed-p'.

By default, if multi version of PKG are installed, return non-nil
whatever one of their version less or equal than PKG's
version. Unless NO-MULTI is non-nil, in which case only the
minimal version is checked for judgement."
  (let* ((descp (package-desc-p pkg))
         (pkg-name (if descp (package-desc-name pkg) pkg))
         (pkg-ver (and descp (package-desc-version pkg))))
    (or
     ;; EEMACS_MAINTENANCE: ensure that `package-installed-p' is still
     ;; just check the minimal version as its "installed-p" judgement
     ;; since for now [2022-12-06 Tue 00:06:30] (emacs-29).
     (package-installed-p pkg-name pkg-ver)
     (and pkg-ver (not no-multi)
          (catch :exit
            (dolist (dc (alist-get pkg-name package-alist))
              (and (version-list-<= pkg-ver (package-desc-version dc))
                   (throw :exit t))))))))

(defun entropy/emacs-package-install-package (update print-prefix &rest args)
  "Install/update package by apply ARGS to `package-install'.

Update package when UPDATE was non-nil.

When installing encounters the fatal error, put the pkg name and
the error msg into `entropy/emacs-package-install-failed-list'."
  (let* ((current-pkgs (copy-tree package-alist))
         (pkg (car args))
         (pkg-name (if (package-desc-p pkg)
                       (package-desc-name pkg)
                     pkg))
         install-pass
         install-core-func
         error-rtn)
    (setq install-core-func
          (lambda (&optional not-prompt)
            (let (_)
              (setq install-pass
                    (condition-case error
                        (let ((inhibit-message not-prompt))
                          (apply 'package-install args)
                          (push pkg-name entropy/emacs-package-install-success-list))
                      (t (prog1 'notpassed
                           (setq error-rtn error)))))
              (when (eq install-pass 'notpassed)
                (push (cons pkg-name error-rtn)
                      entropy/emacs-package-install-failed-list)))))
    (when update
      (package-delete (car (alist-get pkg-name current-pkgs)) t))
    ;; install package after package archvie contents refresh
    ;; when needed.
    (unless (ignore-errors (assoc pkg-name package-archive-contents))
      (package-refresh-contents))
    ;; installing/updating message
    (if print-prefix
        (entropy/emacs-message-do-message
         "%s [%s] package '%s' ..."
         :popup-while-eemacs-init-with-interactive t
         print-prefix
         (blue (if update "Updating" "Installing"))
         (yellow
          (if (package-desc-p pkg)
              (format "%s ---> <%s>" pkg-name pkg)
            (format "%s" pkg-name))))
      (entropy/emacs-message-do-message
       "[%s] package '%s' ..."
       :popup-while-eemacs-init-with-interactive t
       (blue (if update "Updating" "Installing"))
       (yellow (symbol-name pkg-name))))
    ;; do installing/updating
    (cond ((and (entropy/emacs-package-pkg-installed-p pkg)
                (null update))
           (entropy/emacs-message-do-message
            (dark (white "⚠ ALREADY INSTALLED"))))
          (t
           (funcall install-core-func t)
           (if (not (eq install-pass 'notpassed))
               (entropy/emacs-message-do-message
                (green "✓ DONE")
                :popup-while-eemacs-init-with-interactive t)
             (entropy/emacs-message-do-message
              "%s -- %s"
              :popup-while-eemacs-init-with-interactive t
              (red "✕ FAILED")
              (cdr error-rtn)))))))

(defun entropy/emacs-package-compile-dynamic-module
    (pkg install-commands &optional proc-env def-dir)
  "Make dynamic module for package PKG through commands list
INSTALL-COMMANDS whose each element is a list whose car was the
command and rest of the command's arguments.

If DEF-DIR is non-nil, it should be the directory absolute path for
PKG.

If PROC-ENV is non-nil, use it replace `process-environment' for
building procedure while invoking INSTALL-COMMANDS."
  (unless def-dir
    (entropy/emacs-package-prepare-foras))
  (let ((pkg-dir
         (or def-dir
             (package-desc-dir
              (cadr (assq pkg (package--alist)))))))
    (unless (ignore-errors (file-exists-p pkg-dir))
      (error "Package '%s' not installed yet" pkg))
    (let* ((default-directory (entropy/emacs-return-as-default-directory pkg-dir))
           (process-environment (or proc-env process-environment))
           (process-format-func
            (lambda (command uid &optional defdir)
              (if (entropy/emacs-common-plistp command)
                  (setq
                   defdir
                   (or defdir
                       (when-let ((d (plist-get command :default-directory)))
                         (if (consp d)
                             (if (eq (car d) 'absolute) (cdr d)
                               (expand-file-name (cdr d) default-directory))
                           (expand-file-name d default-directory)))
                       default-directory)
                   command (plist-get command :command))
                (setq defdir (or defdir default-directory)))
              `(:name
                ,(format "eemacs-package-dynamic-module-make-for-pkg-%s/%s"
                         pkg uid)
                :default-directory ,defdir
                :command ',command
                :synchronously t
                :buffer (get-buffer-create
                         ,(format
                           "eemacs-package-dynamic-module-make-for-pkg-%s-process-buffer/%s"
                           pkg uid))
                :prepare
                (entropy/emacs-message-do-message
                 "%s"
                 (magenta "Make dynamic module for package [%s] in working dir <%s> of command '%S' ..."
                          ',pkg default-directory ',command))
                :error
                (with-current-buffer $sentinel/destination
                  (entropy/emacs-message-do-message
                   "%s"
                   (red
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max))))
                  (entropy/emacs-error-without-debugger ""))
                ;; :cleanup
                ;; (when (and
                ;;        (buffer-live-p $sentinel/destination))
                ;;   (kill-buffer $sentinel/destination))
                )))
           proc-chain-list)
      (dotimes (uid (length install-commands))
        (setq proc-chain-list
              (append proc-chain-list
                      (list
                       (funcall process-format-func
                                (nth uid install-commands)
                                uid)))))
      (entropy/emacs-make-chained-processes
       proc-chain-list)
      (entropy/emacs-message-do-message
       "%s"
       (green
        "Make dynamic module for package [%s] successfully"
        pkg)))))

;; *** error prompt for failing items

(defun entropy/emacs-package-prompt-install-fails ()
  (if (not entropy/emacs-package-install-failed-list)
      (when-let (((not entropy/emacs-package-init-with-quick-start-p))
                 ;; Only make session should refresh the quick file
                 ;; since:
                 ;;
                 ;; 1. in byte-code start session, there's indeed no
                 ;;    need to do so and shouldn't at least.
                 ;; 2. in source start session, we also need not to do
                 ;;    thus since we respect the soure session for
                 ;;    `entropy/emacs-env-with-pure-eemacs-env'.
                 (package-quickstart (entropy/emacs-is-make-session)))
        (entropy/emacs-message-simple-progress-message
         "%s"
         :with-message-color-args
         '((blue (format "Refresh `package-quickstart-file' %s"
                         package-quickstart-file)))
         (package-quickstart-refresh)))
    ;; Add stdout newline after install message when in batch-mode
    (when noninteractive
      (princ "\n")
      (princ "\n"))
    (let ((count 1))
      (dolist (pkg-err entropy/emacs-package-install-failed-list)
        (entropy/emacs-message-do-message
         "%s: %s '%s' because of '%s'"
         (yellow (number-to-string count))
         (red "failed to install pkg")
         (yellow (symbol-name (car pkg-err)))
         (cdr pkg-err))
        (cl-incf count)))
    (entropy/emacs-silent-abort)))

;; *** install
(defun entropy/emacs-package-install-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (entropy/emacs-package-package-archive-empty-p 'refresh)
  (entropy/emacs-message-do-message
   (blue "Checking extensions satisfied status ...")
   :force-message-while-eemacs-init t)
  (!eemacs-require 'entropy-emacs-package-requirements)
  (let ((package-check-signature
         ;; FIXME: shall we need to enable signature check for
         ;; non-eemacs-ext built packages and how?
         nil)
        (pkg-pre nil)
        pkg-for
        (count 1))
    ;; calulate packages need to be installing
    (dolist (pkgreqptr (bound-and-true-p entropy-emacs-packages))
      (unless (or (null pkgreqptr)
                  (entropy/emacs-package-pkg-installed-p
                   (entropy/emacs-setf-by-body pkg-for
                     (or
                      (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot
                       pkgreqptr :pkg-desc)
                      (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot
                       pkgreqptr :name)))))
        (push pkg-for pkg-pre)))
    ;; do installing
    (dolist (pkg pkg-pre)
      (entropy/emacs-without-debugger
       (entropy/emacs-package-install-package
        nil
        (format "[%s/%s(general)]" count (length pkg-pre))
        pkg))
      (cl-incf count)))
  ;; show fails
  (entropy/emacs-package-prompt-install-fails)
  (entropy/emacs-message-do-message
   (green "All packages installed, congratulations!")
   :force-message-while-eemacs-init t))

;; *** update
(defun entropy/emacs-package-update-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (entropy/emacs-package-package-archive-empty-p 'refresh)
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
          (push pkg-desc-new updates))))
    (if (null updates)
        (entropy/emacs-message-do-message
         (green "All packages are newest!")
         :force-message-while-eemacs-init t)
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
        (dolist (pkg-desc updates)
          (entropy/emacs-without-debugger
           (entropy/emacs-package-install-package t nil pkg-desc)))
        (entropy/emacs-package-prompt-install-fails)
          ;;; FIXME: package reinitialize after updates cause error
          ;;; for `yasnippet-snippets' that for autroload deleted old
          ;;; version.
        ;;(entropy/emacs-package--package-initialize t)
        ))))

;; ** Use-package inititialize
;; Required by `use-package'
(defvar entropy/emacs-package-init-use-packge-after-hook nil)
(defun entropy/emacs-package-init-use-package ()
  (entropy/emacs-package-prepare-foras)
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
  (run-hooks 'entropy/emacs-package-init-use-packge-after-hook)
  (use-package diminish
    :commands (diminish))
  (use-package bind-key
    :commands (bind-key)))

(defun entropy/emacs-package--use-package-add-keyword
    (keyword &optional precedence)
  (let ((precedence (or precedence :commands)))
    (setq use-package-keywords
          (cl-loop for item in use-package-keywords
                   if (eq item precedence)
                   collect precedence and collect keyword
                   else
                   ;; don't add duplicates
                   unless (eq item keyword)
                   collect item))))

;; *** use-package extended

(defmacro entropy/emacs--inner-use-package (use-package-name &rest use-package-body)
  "eemacs *inner only* used `use-pacakge' compatible variant to add more
keys:

- `:eemacs-with-no-require': same as `:no-require' but with pattern
  evaluation. This key exists since the `:no-require' keywords of
  `use-package' is normalized as a predicate rather than a symlist so
  that the any usual condition assignment to this keywords are always
  true for `use-package' macro expansion. (see
  `use-package-normalize-keywords' implementation, it's normally a
  Commentary confusion by `use-package's upstream for \"decades\").

- `:eemacs-with-permanently-defer': always defer if non-nil but still
  respect `:defer' if presented."
  (declare (indent defun))
  (let* ((old-use-package-defaults (copy-tree use-package-defaults))
         (pl (entropy/emacs-defun--get-body-without-keys
              use-package-body nil
              :no-require :defer
              :eemacs-with-permanently-defer
              :eemacs-with-no-require))
         (kpl (car pl))
         (no-require-p
          (if (plist-member kpl :no-require) t
            (eval (plist-get kpl :eemacs-with-no-require)
                  lexical-binding)))
         (perm-defer-p
          (if (plist-member kpl :defer)
              (eval (plist-get kpl :defer)
                    lexical-binding)
            (eval (plist-get kpl :eemacs-with-permanently-defer)
                  lexical-binding)))
         form)
    (setq form `(,@(cdr pl)))
    (if no-require-p
        (setq form `(:no-require t ,@form)))
    (if (not perm-defer-p) `(use-package ,use-package-name ,@form)
      (unwind-protect
          (progn
            (setq use-package-defaults
                  '(;; this '(t) has special meaning; see `use-package-handler/:config'
                    (:config '(t) t)
                    (:init nil t)
                    (:catch t t)
                    (:defer t t)
                    (:demand nil t)))
            (macroexpand-1 `(use-package ,use-package-name ,@form)))
        (setq use-package-defaults old-use-package-defaults)))))

;; *** extra `use-package' keywords definition
;; **** :eemacs-functions

;; same as builtin `:functions' but always autoload even in non-compile session

(entropy/emacs-add-hook-with-lambda 'use-pakcage:eemacs-functions nil
  :use-hook 'entropy/emacs-package-init-use-packge-after-hook
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-functions :if))

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
            (unless (plist-get state :demand)
              `((unless (fboundp ',command)
                  (autoload #',command ,name-string))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

;; **** :eemacs-macros

;; same as builtin `:functions' but for macaros and always autoload
;; even in non-compile session

(entropy/emacs-add-hook-with-lambda 'use-pakcage:eemacs-macros nil
  :use-hook 'entropy/emacs-package-init-use-packge-after-hook
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-macros :if))

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
            (unless (plist-get state :demand)
              `((unless (fboundp ',command)
                  (autoload #',command ,name-string nil nil t))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

;; **** :eemacs-defvars

;; same as builtin `:defines' but always defined even in non-compile session

(entropy/emacs-add-hook-with-lambda 'use-pakcage:eemacs-defvars nil
  :use-hook 'entropy/emacs-package-init-use-packge-after-hook
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-defvars :if))

(defalias 'use-package-normalize/:eemacs-defvars
  'use-package-normalize-symlist)

(defun use-package-handler/:eemacs-defvars (name _keyword arg rest state)
  "The `use-package' handler for key ':eemacs-defvars' which place
can host a macro symbol or a list of thus. All symbols are
recognized as a normal macro."
  (use-package-concat
   (let (_)
     (cl-mapcan
      #'(lambda (var)
          (unless (or (plist-get state :demand)
                      (not (symbolp var)))
            `((unless (boundp ',var) (defvar ,var)))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

;; **** :eemacs-adrequire

(entropy/emacs-add-hook-with-lambda 'use-pakcage:eemacs-adrequire nil
  :use-hook 'entropy/emacs-package-init-use-packge-after-hook
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-adrequire
   :if))

(defun use-package-eemacs-adrequire/gen-random-ad-func-prefix (use-name adtype)
  (entropy/emacs-make-new-interned-symbol
   (lambda (id)
     (format "eemacs-use-package/:eemacs-adrequire/for-%s/adtype-of-%s/func-id_%s"
             use-name adtype id)) nil t))

(defun use-package-eemacs-adrequire/gen-random-ad-judger-prefix (use-name)
  (entropy/emacs-make-new-interned-symbol
   (lambda (id)
     (format "eemacs-use-package/:eemacs-adrequire/for-%s/judger-id_%s"
             use-name id)) nil t))

(defun use-package-normalize/:eemacs-adrequire
    (use-name _key key-value)
  (let (pattern)
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (let ((elts (car key-value)))
             (dolist (ptr elts)
               (let* ((enable       (plist-get ptr :enable))
                      (adfors       (plist-get ptr :adfors))
                      (adtype       (plist-get ptr :adtype))
                      (pdump-no-end (plist-get ptr :pdumper-no-end))
                      (evfunc-0 (lambda (val)
                                  (cond ((symbolp val)
                                         val)
                                        ((consp val)
                                         (entropy/emacs-eval-with-lexical val)))))
                      (evfunc-1 (lambda (val)
                                  (cond ((symbolp val)
                                         (symbol-value val))
                                        ((consp val)
                                         (entropy/emacs-eval-with-lexical val)))))
                      (evfunc-2 (lambda (val)
                                  (cond ((symbolp val)
                                         (symbol-value val))
                                        ((listp val)
                                         (mapcar
                                          (lambda (x)
                                            (funcall evfunc-0 x))
                                          val))))))
                 (push
                  (list :enable         (funcall evfunc-1 enable)
                        :adfors         (funcall evfunc-2 adfors)
                        :adtype         (funcall evfunc-0 adtype)
                        :pdumper-no-end (funcall evfunc-1 pdump-no-end)
                        )
                  pattern))))
           (reverse pattern))
          (t
           (error
            "eemacs use-package adrequire clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun use-package-handler/:eemacs-adrequire
    (use-name _key patterns rest state)
  "Make force require USE-NAME by applying advice to the buntch
of functions according to before or after advice type and these
specification is termed of =eemacs-adrequire-patterns=.

=eemacs-adrequire-patterns= is a list of plist, valid keys of the
plist are:

- :enable :: a var or a form be evaluted as result of t or nil
  which be as an judger to perform the advice.

- :adfors :: a list of symbol of a function/hook or form which
  symbol is identically return but form will be evaluated to a
  result of a symbol as a function/hook. And the evaluated of
  this slot will be a list of function symbols.

- :adtype :: a symbol or a form which symbol is identically
  return but form will be evaluated to a result of a symbol, and
  both of those type are indicate the `advice-add' type of
  'before' or 'after' or a symbol 'hook' indicate treat :adfors
  as bunch of hooks to be injected.

- :pdumper-no-end :: when non-nil do not inject adrequire into
  `entropy/emacs-pdumper-load-hook' when
  `entropy/emacs-custom-enable-lazy-load'.
"
  (let* ((judger-var
          (intern
           (use-package-eemacs-adrequire/gen-random-ad-judger-prefix
            use-name)))
         (rest-body (use-package-process-keywords use-name rest state))
         (form
          `(progn
             (defvar ,judger-var nil
               ,(format
                 "the judger var for use-package \
:eemacs-adrequire for package '%s' which non-nil indicate that \
the :eemacs-adrequrie has been loaded and the related form is banned."
                 use-name))
             (unless (bound-and-true-p ,judger-var)
               (prog1 (require ',use-name)
                 (setq ,judger-var t)))))
         init-form)
    (dolist (ptr patterns)
      (let* ((enable       (plist-get ptr :enable))
             (adfors       (plist-get ptr :adfors))
             (adtype       (plist-get ptr :adtype))
             (pdump-no-end (plist-get ptr :pdumper-no-end))
             (ad-wrapper (cond ((eq adtype 'before)
                                'entropy/emacs-lazy-initial-advice-before)
                               ((eq adtype 'after)
                                'entropy/emacs-lazy-initial-advice-after)
                               ((eq adtype 'hook)
                                'entropy/emacs-lazy-initial-for-hook)
                               (t
                                (error "wrong type of adwrapper type '%s'"
                                       adtype))))
             (adprefix (use-package-eemacs-adrequire/gen-random-ad-func-prefix
                        use-name adtype)))
        (when enable
          (entropy/emacs-nconc-with-setvar-use-rest init-form
            (list
             (macroexpand-all
              `(,ad-wrapper
                ',adfors ',adprefix ',adprefix
                :prompt-type 'prompt-echo
                :pdumper-no-end ',pdump-no-end
                ,form)))))))
    (use-package-concat
     rest-body
     init-form)))

;; ** common start

(defun entropy/emacs-package-common-start (&optional use-full)
  (if
      ;; Do not check extensions when boot from bytecode to speedup
      ;; startup process.
      (or entropy/emacs-package-src-load-bytecode-p
          ;; Or start with eemacs daemon systemd service on which case
          ;; we should forcely consider user has installed all package
          ;; or that's a mistake by user-end since installing package
          ;; via a systemd session may be too long where may confuse
          ;; user to caught what happening.
          (entropy/emacs-getenv "EEMACS_SYSTEMD_DAEMON_SERVICE"))
      (entropy/emacs-package-prepare-foras)
    (cond
     ((entropy/emacs-is-make-all-session)
      ;; just query for install all packages once for a eemacs make
      ;; all session, since it's no need to do duplicated invocation
      ;; for non-"install" make section.
      (and (equal "Install" (entropy/emacs-is-make-session))
           (entropy/emacs-package-install-all-packages)))
     (t (entropy/emacs-package-install-all-packages))))
  (when
      ;; just init `use-package' while no installing procedure sine
      ;; `use-package' may not be detected in dependencies deficiency.
      (and (not entropy/emacs-package-install-success-list)
           ;; Also do not init `use-package' when eemacs is compiled
           ;; since in which case all use-package forms are
           ;; expanded. But FIXME that how we known all eemacs configs
           ;; are compiled? Or use a function put in every eemacs
           ;; configs head to initialize use-pacakge when it's not
           ;; compiled.
           (or (not entropy/emacs-package-src-load-bytecode-p)
               ;; but if invoking as specifying forcely
               use-full))
    (condition-case error
        (entropy/emacs-package-init-use-package)
      (error
       (if entropy/emacs-package-src-load-bytecode-p
           (progn
             ;; but still install package when there's no `use-package' checked out
             ;; while unexpected `package-user-dir' broken while some
             ;; mistake by user-end.
             (entropy/emacs-package-install-all-packages)
             (user-error
              "Please re-make-compile and re-open eemacs since we've fixed the broken dependencies (error: %s)"
              error))
         (user-error "%s" error)))))
  (run-hooks 'entropy/emacs-package-common-start-after-hook))

;; proxy support for install packages
(dolist (func '(entropy/emacs-package-compile-dynamic-module
                entropy/emacs-package-install-package
                entropy/emacs-package-update-all-packages))
  (advice-add func
              :around
              #'entropy/emacs-advice-for-common-do-with-http-proxy))

;; * Provide
(provide 'entropy-emacs-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
