;;; entropy-emacs.el --- entropy emacs main bridge controller
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs.el
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
;; :PROPERTIES:
;; :CUSTOM_ID: h-97799a98-4215-47dc-820d-87518a590fbe
;; :END:
;;
;; This file was the connector for other 'entropy-emacs-*' files, can
;; be as the core but mainly for bridge like role.
;;
;; For as the core position of =entropy-emacs=, the top concept
;; designation were register in this file's commentary part as the
;; brief introduction for developer or package user to understanding
;; the basic runtime logic for it.
;;
;; ** Top initialize
;; :PROPERTIES:
;; :CUSTOM_ID: h-aa9843f8-b690-444c-9537-6849afcee347
;; :END:
;;
;; =entropy-emacs= using `entropy-emacs-defcustom.el' to initialize
;; the emacs-session, provide the customizable variables, and
;; top-level APIs.
;;
;; ** Var-binds
;; :PROPERTIES:
;; :CUSTOM_ID: h-4e2936ee-4b7d-4714-8a7e-5e38118848f6
;; :END:
;;
;; This project building the variable definitions which contains both
;; of the =customizable= and =static const= cases, even for the
;; internal temporal ones, all of them categorized into following
;; files:
;;
;; 1) `entropy-emacs-defconst':  the static const variables
;; 2) `entropy-emacs-defvar':    the pacakge internal sharing variables.
;;
;; The above two variable-defined package are requiring each other in
;; the shown order, that says the =defcustom= are the top requirements
;; for =defconst=, and =defconst= was the dependency for =defvar=, in
;; the reason for as that =defconst= and =defvar='s content may be
;; initialized with user customization.
;;
;; There's another var-bind type for emacs that the 'face' type, in
;; =entropy-emacs= they are registerred in
;; `entropy-emacs-defface.el'.
;;
;; ** Func-binds
;; :PROPERTIES:
;; :CUSTOM_ID: h-9283a47f-0bcd-420c-a3f8-f718346e6288
;; :END:
;;
;; Based on the [[h-4e2936ee-4b7d-4714-8a7e-5e38118848f6][var-binds]], =entropy-emacs= buids the shared libraries
;; for [[h-c70c8556-6957-48bd-883b-25c6c5d7f594][tetacles]] for reducing duplicated coding snippets, and unified
;; internal features as provision of a framework.
;;
;; - `entropy-emacs-message.el'
;;
;;   The unified emacs-message system using both of interacive or
;;   non-interactive session.
;;
;; - `entropy-emacs-defun.el'
;;
;;   The collection of internal shared functions.
;;
;; ** Tentacles
;; :PROPERTIES:
;; :CUSTOM_ID: h-c70c8556-6957-48bd-883b-25c6c5d7f594
;; :END:
;; The final designation was building each file as the wrapper for
;; corresponding aspect of the major mode, tool-chain, or be the
;; group config for some-thing, calling them as =entropy emacs
;; tentcles=.
;;
;; * Configuration:
;; :PROPERTIES:
;; :CUSTOM_ID: h-0f8da0a0-53a9-4e8c-986c-157570a5693e
;; :END:
;;
;; Just requiring this file, you will be able to taste entropy-emacs
;; immediately.
;;
;; * Code:

(defvar entropy/emacs-run-startup-top-init-timestamp (current-time)
  "Time-stamp eemacs top init prepare")

;; ** eemacs top functions
;; Top declared functions used for eemacs.

(defun entropy/emacs-macroexp-progn (exps)
  "Return EXPS (a list of expressions) with `progn' prepended.
If EXPS is a list with a single expression, `progn' is not
prepended, but that expression is returned instead. Return nil if
EXPS is nil.

See also `entropy/emacs-macroexp-rest'."
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defsubst entropy/emacs-macroexp-rest (args)
  "Return ARGS when it's not `null' or a new list as `(nil)' otherwise.

This function exists for preventing omitting ARGS expanded in `&rest',
BODY or FORMS requested context by `,@' in `backquote' forms.

See also `entropy/emacs-macroexp-progn'."
  (or args (list nil)))

(defmacro entropy/emacs-defalias (&rest args)
  "Same as `defalias' but indeed return the SYMBOL.

\(fn SYMBOL DEFINITION &optional DOCSTRING)"
  (declare (indent 1))
  (macroexp-let2* ignore ((sym-name nil))
    `(let ((,sym-name ,(car args)))
       (defalias ,sym-name ,@(cdr args))
       ,sym-name)))

(defmacro entropy/emacs-defvar-local-with-pml (&rest args)
  "Same as `defvar-local' but also make VAR as permanent-local
variable i.e. not cleared when buffer's `major-mode' changed (see
`make-variable-buffer-local').

\(fn var val &optional docstring)"
  (declare (debug defvar) (doc-string 3) (indent defun))
  `(prog1 (defvar-local ,@args)
     (put ',(car args) 'permanent-local t)))

(defmacro entropy/emacs-without-debugger (&rest body)
  "Run BODY without emacs debugger trigger out."
  `(let ((debug-on-error nil) (debug-on-quit nil)
         (inhibit-debugger t))
     ,(entropy/emacs-macroexp-progn body)))

(defun entropy/emacs-error-without-debugger (&rest args)
  "Like `error' but never trigger the emacs debugger."
  (declare (advertised-calling-convention (string &rest args) "23.1"))
  (entropy/emacs-without-debugger
   (signal 'error (list (apply #'format-message args)))))

(defun entropy/emacs-noninteractive-exit-with-fatal ()
  "Exit current `noninteractive' emacs session with fatal exit code."
  (entropy/emacs-error-without-debugger ""))

(defun entropy/emacs-silent-abort ()
  "Abort current procedure without any debugger or prompts"
  (if noninteractive (entropy/emacs-noninteractive-exit-with-fatal)
    (entropy/emacs-error-without-debugger "")))

(defmacro entropy/emacs-sleep-while (&rest body)
  "Sleep for waiting while evaluate BODY return non-nil repeatedly
until it returns nil."
  `(while ,(entropy/emacs-macroexp-progn body)
     ;; 0.001 is the perfect val for sleep both of responsive speed
     ;; and cpu usage.
     (sleep-for 0.001)))

(defsubst entropy/emacs-nxor (cond1 cond2)
  "Like `xor' but as reverse return i.e. return non-nil when cond1
and cond2 are both of non-nil or nil."
  (not (xor cond1 cond2)))

(defsubst entropy/emacs-get-symbol-prop (maybe-sym prop)
  "Like `get' but always return nil when MAYBE-SYM is not a
`symbolp' symbol.

This function exists since user usually use `get' for
`this-command' which is not always a symbol as it declared that
its value is a COMMAND."
  (if (symbolp maybe-sym) (get maybe-sym prop)))

(defun entropy/emacs-func-is-native-comp-p (func)
  "Return non-nil when function FUNC is a native-compiled function,
nil otherwise."
  (when (and (fboundp 'subr-native-elisp-p) (functionp func))
    (cond ((symbolp func) (setq func (symbol-function func)))
          ;; FIXME: enough?
          (t nil))
    (subr-native-elisp-p func)))

(defun entropy/emacs-get-func-origin-def (func)
  "Return function FUNC's origin defination which is out of any
`nadvice' patched."
  (let* ((advised
          (and (symbolp func)
	       (advice--p (advice--symbol-function func)))))
    (or (and advised
             (advice--cd*r (advice--symbol-function func)))
	func)))

;; `compiled-function-p' is new with emacs-29, thus we should backport
;; it to lower emacs version.
(unless (fboundp 'compiled-function-p)
  (defun compiled-function-p (object)
    "Return non-nil if OBJECT is a function that has been compiled.
Does not distinguish between functions implemented in machine code
or byte-code."
    (or (subrp object) (byte-code-function-p object))))

(defun entropy/emacs-child-frame-p (&optional frame)
  "Return FRAME's parent frame if it is a child-frame (See Info node
`(elisp) Child Frames' for what is child-frame), nil if it is not
a child-frame.

If FRAME is omitted or nil use `selected-frame' as default."
  (frame-parameter frame 'parent-frame))

(defun entropy/emacs-server-client-frame-p (&optional frame)
  "FIXME&NOTE&EEMACS_MAINTENANCE: use emacs unexposed API
i.e. `client' frame parameter to detect whether FRAME is a server
client frame. Return non-nil when thus is, nil for otherwise.

Precisely say that `client' frame parameter is inherit from
`frame-inherited-parameters' which is registered by
`server-create-tty-frame' and `server-create-window-system-frame'
who are rely on the core `server--create-frame' frame creators
and set by it as well.

If FRAME is omitted or nil use `selected-frame' as default."
  (frame-parameter frame 'client))

;; *** eemacs-require-func

(defun entropy/emacs-common-require-feature
    (feature &optional filename noerror)
  "eemacs spec `require' facility , to prefer load the elisp
source rather than its compiled version in some cases.

NOTE: not support load dynamic module"
  (let (_)
    (cond
     ((or entropy/emacs-startup-with-Debug-p
          (entropy/emacs-env-init-with-pure-eemacs-env-p)
          (and noninteractive
               (not (bound-and-true-p entropy/emacs-fall-love-with-pdumper))
               (not (daemonp))))
      (require feature (or filename (format "%s.el" feature))
               noerror))
     (t (require feature filename noerror)))))

(defalias '!eemacs-require
  #'entropy/emacs-common-require-feature
  "Alias for `entropy/emacs-common-require-feature' but just used
in baron part to simplify context distinction search")

;; ** INIT
(let* ((subs-core
        '("wasteland/var-binds" "wasteland/func-binds"
          "baron/startup" "baron/batch" "baron/summon" "baron/utils" "baron/hollow"
          "baron/basic-ui"
          "tentacles"))
       (subs-dep
        '("entropy-adblockP-rule-analysis"
          "entropy-cn-dict"
          "entropy-code2org"
          "entropy-dired-cp-or-mv"
          "entropy-emacs-doc"
          "entropy-en-words"
          "entropy-global-read-only-mode"
          "entropy-open-with"
          "entropy-org-batch-refile"
          "entropy-org-export-theme-toggle"
          "entropy-org-widget"
          "entropy-portableapps"
          "entropy-proxy-url"
          "entropy-s2t"
          "entropy-sdcv"
          "entropy-shellpop"
          "benchmark-init"
          "company-tern"
          "fakecygpty"
          "undo-tree-eemacs"
          "liberime"
          "emacs-rime"
          "with-proxy.el"
          "lsp-java-simple"))
       (cur-path (expand-file-name (file-name-directory load-file-name)))
       (core-path (expand-file-name "core" cur-path))
       (deps-path (expand-file-name "site-lisp" cur-path)))
  (add-to-list 'load-path cur-path)
  (dolist (sub-core subs-core)
    (add-to-list 'load-path (expand-file-name sub-core core-path)))
  (dolist (sub-dep subs-dep)
    (add-to-list 'load-path (expand-file-name sub-dep deps-path))))

(!eemacs-require 'entropy-emacs-defcustom)
(defvar entropy/emacs-run-startup-defcustom-load-done-timestamp (current-time)
  "Time-stamp eemacs load defcustom config done done")

(defvar entropy/emacs-run-startup-defcustom-load-duration
  (float-time
   (time-subtract
    entropy/emacs-run-startup-defcustom-load-done-timestamp
    entropy/emacs-run-startup-top-init-timestamp)))

;; The eemacs specified envrionment to indicated all subprocess are
;; ran in an eemacs session, in which case all subprocess can detected
;; this variable to do some extra operations or something else.
(defun __set_eemacs_top_env_indicator ()
  (setenv "EEMACS_ENV" "TRUE"))
(__set_eemacs_top_env_indicator)
;; we should also guaranteed the pdumper reload session has this too.
(add-hook 'entropy/emacs-pdumper-load-hook #'__set_eemacs_top_env_indicator)

;; ** Start Eemacs
(cond
 ((entropy/emacs-is-make-session)
  (!eemacs-require 'entropy-emacs-batch))
 ((entropy/emacs-env-init-with-pure-eemacs-env-p)
  (!eemacs-require 'entropy-emacs-pure-env))
 (t (!eemacs-require 'entropy-emacs-start)))

;; * Provide
(provide 'entropy-emacs)
