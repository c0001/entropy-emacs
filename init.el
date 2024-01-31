;;; init.el --- entropy-emacs initiliaze raw  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-03-29  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; commentary
;;
;;; Configuration:
;;
;; configuration

;; * Code:
;; ** Require
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Debug emacs while hang on by send the SIGUSR2 process event
(setq debug-on-event 'sigusr2)

;; Disable emacs29's new recursively debug feature since it will still
;; warn of those problem is hot fixed yet.
(and (boundp 'debug-allow-recursive-debug)
     (setq debug-allow-recursive-debug t))

;; Native compile specs
;;
;; Disable all auto native compilation bootstraps since we use union
;; native comp procedure in the makefile's 'make native-comp' section.
(setq native-comp-always-compile nil
      native-comp-deferred-compilation nil
      ;; emacs 29.1 declared as replacement of
      ;; `native-comp-deferred-compilation'
      native-comp-jit-compilation nil)
(setq native-comp-deferred-compilation-deny-list
      '(
        ;; general exclusions
        "^.*/?\\.?dir-locals\\(\\.el\\)?$"
        "^.*/?.*-loaddefs\\.el$"
        "^.*/?loaddefs\\.el$"
        "^.*/?.*-pkg\\.el$"
        "^.*/?.*-autoloads\\.el$"
        ;; melpa specs
        "^.*/?.*\\.yas-setup\\.el$"
        ;; we must excluded eemacs code for native comp
        "^.*/?entropy-emacs-[^/]*\\.el$"
        "^.*/?liberime[^/]*\\.el$"
        "^.*/?fakecygpty[^/]*\\.el$")
      ;; emacs 29.1 declared as replacement of
      ;; `native-comp-deferred-compilation-deny-list'
      native-comp-jit-compilation-deny-list
      native-comp-deferred-compilation-deny-list)

;; Take them after basic native-comp configs so that the load
;; procedure respect what we ordered for.
(require 'cl-lib)
(require 'subr-x)

;; ** Eemacs top declares
;; *** Vars
;; set entropy-emacs lowest emacs version requirement
(defconst entropy/emacs-lowest-emacs-version-requirement
  "27.1"
  "The lowsest emacs version requirement for entropy-emacs.

It's a version string which can be used for `version<' and
`version<='.")
(defconst entropy/emacs-highest-emacs-version-requirement
  "29.2"
  "The highest emacs version requirement for entropy-emacs.

It's a version string which can be used for `version<' and
`version<='.")

(when (version< emacs-version entropy/emacs-lowest-emacs-version-requirement)
  (error "This requires Emacs %s and above!"
         entropy/emacs-lowest-emacs-version-requirement))

;; although we defined it in `early-init.el', but if we invoke
;; `init.el' as `-l' arg for emacs batch, then early-init.el will not
;; be loaded until we manully load it (see also
;; `entropy/emacs-early-init-done').
(unless (bound-and-true-p entropy/emacs-user-emacs-directory)
  (defvar entropy/emacs-user-emacs-directory
    (file-name-directory load-file-name)))

;; early init file
(defconst entropy/emacs-early-init-file
  (expand-file-name
   "early-init.el"
   entropy/emacs-user-emacs-directory)
  "The `early-init-file' file specified for =entropy-emacs=.")

;; load early init file in batch mode
(unless (bound-and-true-p entropy/emacs-early-init-done)
  (load entropy/emacs-early-init-file)
  ;; indicate its load in this context
  (setq entropy/emacs-early-init-done 'manually))

;; load custom file
(defconst entropy/emacs-custom-common-file-template
  (expand-file-name
   "custom-example.el"
   entropy/emacs-user-emacs-directory)
  "The `custom-file' template specified for =entropy-emacs=.")

(defconst entropy/emacs-custom-common-file
  (expand-file-name
   "custom.el"
   entropy/emacs-user-emacs-directory)
  "The value for `custom-file' but specified for =entropy-emacs=.")

;; *** Funcs
;; **** Multi-version emacs compatible
;; TODO ...

;; **** Eemacs init env filter

(defsubst entropy/emacs-getenv (variable &optional frame)
  "Same as `getenv' but also return nil when VARIABLE's value is an
empty string predicated by `string-empty-p'.

NOTE: for eemacs spec env vars use the alias
`entropy/emacs-getenv-eemacs-env' to distinguish context."
  (let ((env-p (getenv variable frame)))
    (if (or (null env-p) (string-empty-p env-p))
        nil env-p)))

(defalias 'entropy/emacs-getenv-eemacs-env
  'entropy/emacs-getenv
  "Alias of `entropy/emacs-getenv' to distinguish the inner usage
for eemacs defined env variable only.")

(defsubst entropy/emacs-getenv-equal
    (variable value &optional frame with-empty-string)
  "Return non-nil when the value of `getenv' of env var VARIABLE is
`equal' to VALUE, otherwise return nil.

If VALUE is a `consp' list, then the `equal' of values is treated
with `member' of value to the VALUE list.

The return is always nil when no such VARIABLE in env or value of
VARIABLE is `string-empty-p' unless WITH-EMPTY-STRING is non-nil.

NOTE: for eemacs spec env vars use the alias
`entropy/emacs-getenv-equal-eemacs-eqnv' to distinguish context."
  (when-let ((val
              (if with-empty-string
                  (getenv variable frame)
                (entropy/emacs-getenv variable frame))))
    (if (consp value)
        (member val value) (equal val value))))

(defalias 'entropy/emacs-getenv-equal-eemacs-env
  'entropy/emacs-getenv-equal
  "Alias of `entropy/emacs-getenv-equal' to distinguish the inner
usage for eemacs defined env variable only.")

(defun entropy/emacs-env-init-with-pure-eemacs-env-p (&rest _)
  (entropy/emacs-getenv-eemacs-env "EEMACS_INIT_WITH_PURE"))

(defun entropy/emacs-env-init-with-pure-eemacs-env/load-custom-file-p nil
  (and (entropy/emacs-env-init-with-pure-eemacs-env-p)
       (entropy/emacs-getenv-eemacs-env "EEMACS_INIT_WITH_PURE_LCSTF")))

;; ** Load custom

(let ((cus entropy/emacs-custom-common-file))
  (when (if (entropy/emacs-env-init-with-pure-eemacs-env-p)
            (entropy/emacs-env-init-with-pure-eemacs-env/load-custom-file-p)
          t)
    (when (not (file-exists-p cus))
      (copy-file entropy/emacs-custom-common-file-template
                 entropy/emacs-custom-common-file
                 nil t))
    (setq custom-file cus)
    (message "")
    (message "====================================")
    (message "[Loading] custom specifications ...")
    (message "====================================\n")
    (load cus)))

;; ** Top customized group
(defgroup entropy-emacs-customize-top-group nil
  "Eemacs customizable variables top group."
  :group 'extensions)

(defcustom entropy/emacs-custom-enable-lazy-load t
  "Enable lazy load for entropy-emacs when non-nil.

Notice: when `entropy/emacs-fall-love-with-pdumper' is non-nil or
in daemon session, this variable will be pressed whatever init
value assignments into."
  :type 'boolean
  :group 'entropy-emacs-customize-top-group)

;; *** Debug

(defgroup entropy/emacs-customize-group-for-DEBUG nil
  "entropy-emacs debug configurations"
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-startup-with-Debug-p
  (entropy/emacs-getenv-eemacs-env "EEMACS_DEBUG")
  "Does eemacs start with debug mode?"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-DEBUG)

(defcustom entropy/emacs-startup-benchmark-init entropy/emacs-startup-with-Debug-p
  "Benchmark eemacs startup time?"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-DEBUG)

(defcustom entropy/emacs-startup-debug-on-error entropy/emacs-startup-with-Debug-p
  "Enable `debug-on-error' at eemacs startup time?"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-DEBUG)

(when entropy/emacs-startup-debug-on-error
  (setq debug-on-error t))

(defcustom entropy/emacs-startup-jit-lock-debug-mode nil
  "Enable `jit-lock-debug-mode' at eemacs startup time?

NOTE&FIXME: eemacs defaultly disable it since
`jit-lock-debug-mode' can not render the
display? (i.e. `dired-mode' and `prog-mode' etc. has no color
renderred after init this.)"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-DEBUG)

;; *** Pdumper
(defgroup entropy/emacs-customize-group-for-pdumper nil
  "Eemacs portable dump configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-do-pdumper-in-X t
  "Whether did pdumper for gui prot."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pdumper)

;; ** Startup entropy-emacs

(defvar entropy/emacs-fall-love-with-pdumper
  (entropy/emacs-getenv-equal-eemacs-env
   "EEMACS_MAKE" '("Dump" "Compile-Dump"))
  "The emacs running type indication for pdumper.")

(defvar entropy/emacs-do-pdumping-with-lazy-load-p nil
  "Whether dump emacs with eemacs-core only i.e. all facilities will
loading after the dump file loaded.

This default value of `emacs-major-version' upper/equal than 29
is forced be non-nil since in which emacs versions the pdump
regression phenomenon is issued from
`https://lists.nongnu.org/archive/html/emacs-devel/2023-02/msg00603.html',
TLDR, for those emacs versions many more objects can not be
dumped as well as older vers any more.")
(setq entropy/emacs-do-pdumping-with-lazy-load-p
      (> emacs-major-version 28))

(defvar entropy/emacs-pdumper-load-hook nil
  "Hook for run with pdumper session startup.")
(defun entropy/emacs-suggest-startup-with-elisp-source-load-p nil
  (or
   (entropy/emacs-getenv-equal-eemacs-env "EEMACS_MAKE" '("Compile" "Compile-Dump"))
   (and noninteractive
        (not entropy/emacs-fall-love-with-pdumper)
        (not (daemonp)))
   (and entropy/emacs-startup-with-Debug-p
        ;; We just guarantee load source when in a debug
        ;; session invoked by make env, since that's clear to
        ;; show that this indeed is. In other words, we allow
        ;; debug on byte-compiled eemacs core.
        (entropy/emacs-getenv-eemacs-env "EEMACS_DEBUG"))
   (entropy/emacs-env-init-with-pure-eemacs-env-p)))

(defvar __inited-p? nil)

;; forbidden redudant load e.g. pdumper recovery session
(unless (bound-and-true-p __inited-p?)

  ;; before/after init time bind
  (defvar entropy/emacs-run-startup-beforeinit-timestamp
    nil)
  (when (daemonp)
    (setq entropy/emacs-run-startup-beforeinit-timestamp
          (current-time)))
  (defvar entropy/emacs-run-startup-afterinit-timestamp
    nil)

  (defun entropy/emacs-get-before-init-time (&rest _)
    (or entropy/emacs-run-startup-beforeinit-timestamp
        before-init-time))
  (defun entropy/emacs-get-after-init-time (&rest _)
    (or entropy/emacs-run-startup-afterinit-timestamp
        after-init-time))

  ;; load eemacs config
  (defun __init_eemacs__ (&rest _)
    "entropy-emacs loader"
    ;; require package before any procedure as top initialization
    ;; requirement.
    (require 'package)
    ;; we should reset `package-quickstart-file' since we've modified
    ;; `user-emacs-directory'
    (setq package-quickstart-file (locate-user-emacs-file "package-quickstart.el"))
    (let ((eemacs-top-init-file
           (cons 'entropy-emacs
                 (expand-file-name
                  "elements/entropy-emacs.el"
                  entropy/emacs-user-emacs-directory))))
      (cond
       ((entropy/emacs-suggest-startup-with-elisp-source-load-p)
        (require (car eemacs-top-init-file)
                 ;; confirm load the source i.e. non-compiled ver.
                 (cdr eemacs-top-init-file)))
       (t
        (add-to-list 'load-path (file-name-directory (cdr eemacs-top-init-file)))
        (require (car eemacs-top-init-file))))))

  (setq __inited-p? t)
  (cond
   ((or noninteractive
        (daemonp))
    (when (daemonp)
      (setq entropy/emacs-run-startup-afterinit-timestamp
            (current-time)))
    (funcall #'__init_eemacs__))
   (t
    (if after-init-time (funcall #'__init_eemacs__)
      (add-hook 'after-init-hook
                #'__init_eemacs__
                ;; at end of init hook
                100))))
  )
