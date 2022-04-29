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

;; ** prepare
;; *** multi-version emacs compatible
;; **** make `setq-local' compatible
(when (version< emacs-version "27")
  (defmacro setq-local (&rest pairs)
    "Make variables in PAIRS buffer-local and assign them the corresponding values.

PAIRS is a list of variable/value pairs.  For each variable, make
it buffer-local and assign it the corresponding value.  The
variables are literal symbols and should not be quoted.

The second VALUE is not computed until after the first VARIABLE
is set, and so on; each VALUE can use the new value of variables
set earlier in the 'setq-local'.  The return value of the
'setq-local' form is the value of the last VALUE.

\(fn [VARIABLE VALUE]...)"
    (declare (debug setq))
    (unless (zerop (mod (length pairs) 2))
      (error "PAIRS must have an even number of variable/value members"))
    (let ((expr nil))
      (while pairs
        (unless (symbolp (car pairs))
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        ;; Can't use backquote here, it's too early in the bootstrap.
        (setq expr
              (cons
               (list 'set
                     (list 'make-local-variable (list 'quote (car pairs)))
                     (car (cdr pairs)))
               expr))
        (setq pairs (cdr (cdr pairs))))
      (macroexp-progn (nreverse expr)))))

;; *** Warning of startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(when (version< emacs-version "26")
  (error "This requires Emacs 26 and above!"))

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
  (defun __init_emacs (&rest _)
    "          init emacs          "
    (require
     'entropy-emacs
     (expand-file-name
      "elements/entropy-emacs.el"
      entropy/emacs-user-emacs-directory)))

  (setq __inited-p? t)
  (cond
   ((or noninteractive
        (daemonp))
    (when (daemonp)
      (setq entropy/emacs-run-startup-afterinit-timestamp
            (current-time)))
    (funcall #'__init_emacs))
   (t
    (add-hook 'after-init-hook
              #'__init_emacs
              t)
    ))
  )
