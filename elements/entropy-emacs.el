;;; entropy-emacs.el --- entropy emacs main bridge controller  -*- lexical-binding: t; -*-
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

;; ** eemacs top APIs
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

(cl-defun entropy/emacs--get-def-body (list-var &optional with-safe)
  "Get BODY inside of plist like list LIST-VAR, commonly is the
last `keywordp' keypair's cdr or return LIST-VAR when the car of
LIST-VAR is not a `keywordp' keyword.

When WITH-SAFE is non-nil, when the real body is nil, then return
`(nil)'. Otherwise of thus case, always return nil.

This function is useful for `cl-defmacro' BODY parsing like:

#+begin_src emacs-lisp
(cl-defmacro name &rest body
             &key
             key-1
             key-2
             ...
             &allow-other-keys)
#+end_src

To get the real-body in BODY use
\(setq BODY (fn BODY))
"
  (let ((it list-var))
    (catch 'break
      (while t
        (if (keywordp (car it)) (setq it (cddr it))
          (throw 'break
                 (if (not with-safe) it
                   (or it (list nil)))))))))

(defun entropy/emacs-maybe-list (object)
  "Wrap OBJECT as a list of it only when OBJECT is not a `listp'
list.

Return the list or OBJECT when not thus."
  (and object
       (if (consp object) object (list object))))

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

(defun entropy/emacs-safety-message (format-string &rest args)
  "Like `message' but prevent user use the FORMAT-STRING as the
only argument apply to it which may cause error while the
FORMAT-STRING is actually a format-string but used as a common
string and no ARGS can be formatted."
  (if args
      (apply 'message format-string args)
    (message "%s" format-string)))

(defun entropy/emacs-time-subtract (before &optional now use-float)
  "`time-subtract' from BEFORE to NOW (defautls to `current-time').
Return that result.

If USE-FLOAT is non-nil, the result is a `floatp' number indiate that
duration in seconds."
  (let ((dr (time-subtract (or now (current-time)) before)))
    (if use-float (float-time dr) dr)))

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

But the `client' parameter is inheritance for all child-frames
created from a client frame, thus we also return nil when the
frame has a parent (but FIXME: how we ensure a child-frame is
truely a child frame that user considered as? or shall we make a
prediates hook for user registering what they recognized for?)

If FRAME is omitted or nil use `selected-frame' as default."
  (and (frame-parameter frame 'client)
       (not (entropy/emacs-child-frame-p frame))))


;; *** eemacs def*

(defvar entropy/emacs-inner-sym-for/current-defname
  (make-symbol "*eemacs-current-defname*")
  "Eemacs inner used non-`interned' symbol used for lexical bind the
current defination name for name bound definations like `defun',
`defalias', but only for those who have eemacs variant.

The valid variants are:
- `entropy/emacs-!cl-defun'
- `entropy/emacs-!defalias'

In eemacs, we use =eemacs-defn-bind= as the term for that binding
usage in context.

To use the lexical value of this symbol in context, see
`entropy/emacs-!with-cdefn'.")
(defmacro entropy/emacs-!cl-defun (&rest args)
  "Same as `cl-defun' but indeed return the symbol of NAME and also
for:

Use =eemacs-defn-bind= of symbol of NAME for BODY when
`lexical-binding' is non-nil. (see
`entropy/emacs-inner-sym-for/current-defname')

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (doc-string 3) (indent 2))
  (let ((name (car args)))
    `(let ((,entropy/emacs-inner-sym-for/current-defname
            ',name))
       (cl-defun ,@args) ',name)))

(defmacro entropy/emacs-!defalias (&rest args)
  "Same as `entropy/emacs-defalias' but also:

Use =eemacs-defn-bind= of SYMBOL for DEFINITION if it's an
`lambda' when `lexical-binding' is non-nil. (see
`entropy/emacs-inner-sym-for/current-defname')

\(fn SYMBOL DEFINITION &optional DOCSTRING)"
  (declare (indent 1))
  (macroexp-let2* ignore ((sym-name (car args)))
    `(let ((,entropy/emacs-inner-sym-for/current-defname
            ,sym-name))
       (defalias ,sym-name ,@(cdr args))
       ,sym-name)))

(cl-defmacro entropy/emacs-!with-cdefn
    (&rest body &key with-it-as &allow-other-keys)
  "Run body with binding lexical var symbol set by
WITH-IT-AS (default is `it') of value of =eemacs-defn-bind=, if
thus is void, the bind's value is nil. Return BODY's value.

(see `entropy/emacs-inner-sym-for/current-defname')"
  (declare (indent defun))
  (let ((bind-sym (or with-it-as 'it)))
    `(let ((,bind-sym
            (ignore-errors ,entropy/emacs-inner-sym-for/current-defname)))
       ,@(entropy/emacs--get-def-body body 'with-safe))))

(defmacro entropy/emacs-!message (format-string &rest args)
  "Like `message' but message with =eemacs-defn-bind= as prefix when
available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'message ,format-string ,@args)
       (message (concat "[%s] " ,format-string)
                cur-defn ,@args))))

(defmacro entropy/emacs-!error (string &rest args)
  "Like `error' but message with =eemacs-defn-bind= as prefix when
available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'error ,string ,@args)
       (error (concat "[%s] " ,string)
              cur-defn ,@args))))

(defmacro entropy/emacs-!user-error (string &rest args)
  "Like `user-error' but message with =eemacs-defn-bind= as prefix
when available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'user-error ,string ,@args)
       (user-error (concat "[%s] " ,string)
                   cur-defn ,@args))))

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

;; *** high perfomance alist

(defun entropy/emacs--make-alist-with-symbol-prop-set/core-func
    (vsym nv _op _wh keyname &optional init with-single)
  (let* ((inhibit-quit t) (oldval (symbol-value vsym))
         key keysym-p single-p nsingle-p
         decfunc)
    (entropy/emacs-setf-by-body decfunc
      (lambda (x)
        (or (and (prog1 (entropy/emacs-setf-by-body keysym-p
                          (symbolp (setq key (if (setq nsingle-p (consp x))
                                                 (car x) x))))
                   (setq single-p (not nsingle-p)))
                 with-single)
            (and keysym-p (consp x)))))
    (when (and (not init) (consp oldval))
      (dolist (el oldval)
        (if (funcall decfunc el) (put key keyname nil))))
    (setq nv (if init oldval nv))
    (when (consp nv)
      (dolist (el nv)
        (when (funcall decfunc el)
          (put key keyname
               (if (and with-single single-p) t (cdr el))))))))

(cl-defmacro entropy/emacs-make-alist-with-symbol-prop-set
    (var-sym key-sym &key with-single)
  "If a symbol VAR-SYM who is a `special-variable-p' variable and its
value is an alist (or partial of it is), then make `symbolp' key of
any of its key-pair element (i.e. the `car' of that pair) has a symbol
property key symbol KEY-SYM and make VAR-SYM's KEY-SYM slot has the
original corresponding value (i.e. the `cdr' of that pair).

If VAR-SYM's value is changed via `setq' or any other global sets
method after the initialization (i.e. after this macro's
invocation) then set all symbol's KEY-SYM slot to nil firstly and
then set them according to the new value of VAR-SYM in turn as
initialization. This automatic updates functional is supplied by
given VAR-SYM a variable watcher thru `add-variable-watcher'.

According to this macro's mechanism, each valid key's value cat via
`get' by KEY-SYM is greatly faster than `alist-get' since it is
mapping through list and will cause times of gc while VAR-SYM's value
is huge and did it frequently. But the set operation of VAR-SYM's
value is slower than the oridinary alist set method, since each set
will invoke the rearrangements for all old and new valid keys. Thus
the main usage of this macro is making a const or modification rarely
alist with high performance value cat experience.

KEY-SYM should be `keywordp' i.e. explicitly set as `:key' or the
defination is failed with error.

If WITH-SINGLE is set and return non-nil, then any symbol directly
presented in value of VAR-SYM is also used with considering to enable
as set that prop to `t' and disable as set thus to `nil'.

NOTE: this macro must be expanded in `lexical-binding' enabled
context or messy up."
  (declare (indent 2))
  (unless (bound-and-true-p lexical-binding)
    (user-error "[eemacs-hpal]: need lexical enabled environment!"))
  (let ((varsym (make-symbol "var-symbol"))
        (keysym (make-symbol "key-symbol"))
        (wssym  (make-symbol "with-single-p")))
    `(let* ((,varsym ,var-sym) (,keysym ,key-sym)
            (,wssym ,with-single)
            (var-guard-func-name
             (intern (format "__eemacs/%s/high-perfomance-alist/set-guard" ,varsym))))
       (unless (and (symbolp ,varsym) (special-variable-p ,varsym))
         (user-error "var-sym `%S' is not `symbolp' or `special-variable-p'"
                     ,varsym))
       (unless (keywordp ,keysym)
         (user-error "key-sym `%S' is not `keywordp'" ,keysym))
       ;; We should remove the old var-watcher firstly.
       (remove-variable-watcher ,varsym var-guard-func-name)
       (put ,varsym '__eemacs-alist-get__ ,keysym)
       (funcall 'entropy/emacs--make-alist-with-symbol-prop-set/core-func
                ,varsym nil nil nil ,keysym 'init ,wssym)
       (defalias var-guard-func-name
         (lambda (vsym nv op wh)
           (when (eq op 'set)
             (funcall 'entropy/emacs--make-alist-with-symbol-prop-set/core-func
                      ,varsym nv op wh ,keysym nil ,wssym)))
         (format "variable guard for high performance alist variable `%s', made
via `entropy/emacs-make-alist-with-symbol-prop-set'." ,varsym))
       (add-variable-watcher ,varsym var-guard-func-name))))

;; ** INIT

;; eemacs conventional top-level binding either NOTE emacs bind to
;; "M-ESC"
(global-set-key (kbd "ESC ESC") 'top-level)

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
