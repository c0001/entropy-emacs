;;; init.el --- entropy-emacs initiliaze raw

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
;; ** require
(require 'cl-lib)
(require 'subr-x)

;; Debug emacs while hang on by send the SIGUSR2 process event
(setq debug-on-event 'sigusr2)

;; set entropy-emacs lowest emacs version requirement
(defconst entropy/emacs-lowest-emacs-version-requirement
  "27"
  "The lowsest emacs version requirement for entropy-emacs.

It's a version string which can be used for `version<' and
`version<='.")

;; ** eemacs top declares

(defgroup entropy-emacs-customize-top-group nil
  "Eemacs customizable variables top group."
  :group 'extensions)

;; *** Debug

(defgroup entropy/emacs-customize-group-for-DEBUG nil
  "entropy-emacs debug configurations"
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-startup-with-Debug-p (getenv "EEMACS_DEBUG")
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

;; *** Pdumper
(defgroup entropy/emacs-customize-group-for-pdumper nil
  "Eemacs portable dump configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-do-pdumper-in-X t
  "Whether did pdumper for gui prot."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pdumper)

(defcustom entropy/emacs-fall-love-with-pdumper nil
  "The emacs running type indication for pdumper."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pdumper)

;; ** prepare
;; *** multi-version emacs compatible
;; TODO ...

;; *** Warning of startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(when (version< emacs-version entropy/emacs-lowest-emacs-version-requirement)
  (error "This requires Emacs %s and above!"
         entropy/emacs-lowest-emacs-version-requirement))

;; *** eemacs init env filter

(defun entropy/emacs-env-init-with-pure-eemacs-env-p (&rest _)
  (let ((env (getenv "EEMACS_INIT_WITH_PURE")))
    (and env
         (not (string-empty-p env)))))

(defmacro entropy/emacs-env-with-pure-eemacs-env (&rest body)
  `(let ((process-environment
          (cons "EEMACS_INIT_WITH_PURE=t"
                process-environment)))
     ,@body))

;; ** startup entropy-emacs

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

  ;; top eemacs host
  (defvar entropy/emacs-user-emacs-directory
    (file-name-directory load-file-name)
    "Top eemacs host directory replaced of `user-emacs-directory'
for preventing unregular loading procedure by modification of
emacs upstream")

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

  (let ((cus entropy/emacs-custom-common-file))
    (unless (entropy/emacs-env-init-with-pure-eemacs-env-p)
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

  ;; load eemacs config
  (defun __init_eemacs__ (&rest _)
    "entropy-emacs loader"
    (let ((eemacs-top-init-file
           (cons 'entropy-emacs
                 (expand-file-name
                  "elements/entropy-emacs.el"
                  entropy/emacs-user-emacs-directory))))
      (cond
       ((or entropy/emacs-startup-with-Debug-p
            (entropy/emacs-env-init-with-pure-eemacs-env-p)
            (and noninteractive
                 (not (bound-and-true-p entropy/emacs-fall-love-with-pdumper))
                 (not (daemonp)))
            )
        (require (car eemacs-top-init-file)
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
    (add-hook 'after-init-hook
              #'__init_eemacs__
              ;; at end of init hook
              100)
    ))
  )
