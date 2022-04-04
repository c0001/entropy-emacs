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

(defvar entropy/emacs-package-src-load-file-name
  (eval 'load-file-name))

(require 'package)
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
  (unless (version< emacs-version "27")
    (setq package-quickstart nil))
  (when force
    (setq package--initialized nil)
    (setq load-path (copy-tree entropy/emacs-origin-load-path))
    (setq package-alist nil)
    (setq package-activated-list nil))
  (entropy/emacs-message-do-message
   "Custom packages initializing ......"
   :force-message-while-eemacs-init t)
  (unless __package-first-initialized
    (setq entropy/emacs-package-initialize-init-timestamp
          (current-time)))
  (when (or force (not (bound-and-true-p __package-first-initialized)))
    (package-initialize))
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
and the return is the rechecking result like above."
  (entropy/emacs-package-prepare-foras)
  (let* ((pkg-archive-dir (expand-file-name "archives" package-user-dir))
         (rtn
          (if (and (file-exists-p pkg-archive-dir)
                   (entropy/emacs-list-dir-subfiles-recursively-for-list pkg-archive-dir))
              nil
            t)))
    (cond ((and try-get
                rtn)
           (package-refresh-contents)
           (entropy/emacs-package-package-archive-empty-p))
          (t
           rtn))))

(defun entropy/emacs-package-pkg-installed-p (pkg)
  "Like `package-installed-p' but when PKG is `package-desc' and
`package-installed-p' return nil, we check the `package-desc'
with the version comparison in where `package-alist' and
`package-archive-contents' to judger whether it is installed yet
since the `package-desc' get from `package-archive-contents'
didn't have an installing host meta but `package-installed-p' so
as judge in this way for check the installing host whether
exists simply."
  (or (package-installed-p pkg)
      (let ((pkg-cur-desc
             (or
              (and (package-desc-p pkg)
                   (car (alist-get
                         (package-desc-name pkg)
                         package-alist)))
              (car (alist-get pkg package-alist))))
            pkg-archive-desc-list)
        (when (package-desc-p pkg-cur-desc)
          (setq pkg-archive-desc-list
                (alist-get (package-desc-name pkg-cur-desc)
                           package-archive-contents))
          (catch :exit
            (dolist (new-pkg-desc pkg-archive-desc-list)
              (when (version-list-=
                     (package-desc-version pkg-cur-desc)
                     (package-desc-version new-pkg-desc))
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
    (unless (ignore-errors (assoc pkg package-archive-contents))
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

(defun entropy/emacs-package-compile-dynamic-module (pkg install-commands)
  "Make dynamic module for package PKG through commands list
INSTALL-COMMANDS whose each element is a list whose car was the
command and rest of the command's arguments"
  (entropy/emacs-package-prepare-foras)
  (let ((pkg-dir
         (package-desc-dir
          (cadr (assq pkg (package--alist))))))
    (unless (ignore-errors (file-exists-p pkg-dir))
      (error "Package '%s' not installed yet" pkg))
    (let* ((default-directory pkg-dir)
           (process-format-func
            (lambda (command uid)
              `(:name
                ,(format "eemacs-package-dynamic-module-make-for-pkg-%s/%s"
                         pkg uid)
                :default-directory ,default-directory
                :command ',command
                :synchronously t
                :buffer (get-buffer-create
                         ,(format
                           "eemacs-package-dynamic-module-make-for-pkg-%s-process-buffer/%s"
                           pkg uid))
                :prepare
                (entropy/emacs-message-do-message
                 "%s"
                 (magenta "Make dynamic module for package [%s] in working dir <%s> of command '%s' ..."
                          ',pkg ,default-directory ',command))
                :error
                (with-current-buffer $sentinel/destination
                  (entropy/emacs-message-do-message
                   "%s"
                   (red
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max))))
                  (error ""))
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
  (when entropy/emacs-package-install-failed-list
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
    (let (
          ;; ensure we do not need debug for this statement
          (debug-on-error nil))
      (error ""))))

;; *** install
(defun entropy/emacs-package-install-all-packages ()
  (entropy/emacs-package-prepare-foras)
  (entropy/emacs-package-package-archive-empty-p 'try-get)
  (entropy/emacs-message-do-message
   (blue "Checking extensions satisfied status ...")
   :force-message-while-eemacs-init t)
  (!eemacs-require 'entropy-emacs-package-requirements)
  (let ((package-check-signature nil)
        (pkg-pre nil)
        (count 1))
    ;; calulate packages need to be installing
    (dolist (pkgreqptr entropy-emacs-packages)
      (unless (or (null pkgreqptr)
                  (entropy/emacs-package-pkg-installed-p
                   (or
                    (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot
                     pkgreqptr :pkg-desc)
                    (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot
                     pkgreqptr :name))))
        (push (or (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot pkgreqptr :pkg-desc)
                  (entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot pkgreqptr :name))
              pkg-pre)))
    ;; do installing
    (dolist (pkg pkg-pre)
      (ignore-errors
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
  (entropy/emacs-package-package-archive-empty-p 'try-get)
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
          (entropy/emacs-package-install-package t nil pkg-desc))
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

(defmacro entropy/emacs-usepackage-with-permanently-defer
    (&rest form)
  "Like `use-package' but always defer the package loading."
  (declare (indent 1))
  (let* ((old-use-package-defaults use-package-defaults))
    (unwind-protect
        (progn
          (setq use-package-defaults
                '(;; this '(t) has special meaning; see `use-package-handler/:config'
                  (:config '(t) t)
                  (:init nil t)
                  (:catch t t)
                  (:defer t t)
                  (:demand nil t)))
          (macroexpand-1 `(use-package ,@form)))
      (setq use-package-defaults old-use-package-defaults))))

;; *** extra `use-package' keywords definition
;; **** :eemacs-functions

(with-eval-after-load 'use-package
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-functions))

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

;; **** :eemacs-macros

(with-eval-after-load 'use-package
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-macros))

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


;; **** :eemacs-adrequire

(with-eval-after-load 'use-package
  (entropy/emacs-package--use-package-add-keyword
   :eemacs-adrequire
   :if))

(defvar use-package-eemacs-adrequire/ad-random-func-ids nil)
(defun use-package-eemacs-adrequire/gen-random-ad-func-prefix (use-name adtype)
  (let* ((id-pool use-package-eemacs-adrequire/ad-random-func-ids)
         (id (if id-pool
                 (+ (car id-pool) 1)
               0)))
    (push id use-package-eemacs-adrequire/ad-random-func-ids)
    (format "eemacs-use-package/:eemacs-adrequire/for-%s/adtype-of-%s/func-id_%s"
            use-name adtype id)))

(defvar use-package-eemacs-adrequire/ad-random-judger-ids nil)
(defun use-package-eemacs-adrequire/gen-random-ad-judger-prefix (use-name)
  (let* ((id-pool use-package-eemacs-adrequire/ad-random-judger-ids)
         (id (if id-pool
                 (+ (car id-pool) 1)
               0)))
    (push id use-package-eemacs-adrequire/ad-random-judger-ids)
    (format "eemacs-use-package/:eemacs-adrequire/for-%s/judger-id_%s"
            use-name id)))

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
                                        ((listp val)
                                         (eval val)))))
                      (evfunc-1 (lambda (val)
                                  (cond ((symbolp val)
                                         (symbol-value val))
                                        ((listp val)
                                         (eval val)))))
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
         (_ (eval `(defvar ,judger-var nil
                     ,(format
                       "the judger var for use-package \
:eemacs-adrequire for package '%s' which non-nil indicate that \
the :eemacs-adrequrie has been loaded and the related form is banned."
                       use-name))))
         (rest-body (use-package-process-keywords use-name rest state))
         (form
          `(unless (bound-and-true-p ,judger-var)
             (prog1
                 (require ',use-name)
               (setq ,judger-var t))))
         (init-form '()))
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
          (setq init-form
                (append init-form
                        `((,ad-wrapper
                           ,adfors ,adprefix ,adprefix prompt-echo
                           :pdumper-no-end ',pdump-no-end
                           ,form)))))))
    (use-package-concat
     rest-body
     init-form)))

;; ** common start

(defun entropy/emacs-package-common-start ()
  (if
      ;; Do not check extensions when boot from bytecode to speedup
      ;; startup process.
      (string-match-p
       "\\.elc$"
       entropy/emacs-package-src-load-file-name)
      (progn
        (entropy/emacs-package-prepare-foras))
    (entropy/emacs-package-install-all-packages))
  (when
      ;; just init `use-package' while no installing procedure sine
      ;; `use-package' may not be detected in dependencies deficiency.
      (not entropy/emacs-package-install-success-list)
    (condition-case error
        (entropy/emacs-package-init-use-package)
      (error
       (if (string-match-p
            "\\.elc$"
            entropy/emacs-package-src-load-file-name)
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
