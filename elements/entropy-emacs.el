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

(defvar entropy/emacs-defconst--just-warn
  (bound-and-true-p entropy/emacs-startup-with-Debug-p))
(defmacro entropy/emacs-defconst (symbol initvalue &optional docstring)
  "Same as `defconst' but do not allow any set, local bind,
and any even modification for variable SYMBOL, if not an error is
raised up."
  (declare (indent defun) (doc-string 3))
  (setq docstring
        (concat (or (and docstring (replace-regexp-in-string "\n+$" "" docstring))
                    "A const variable.")
                "\n\n(NOTE: this variable is defined by `entropy/emacs-defconst')"))
  (macroexp-let2* ignore ((ival nil))
    `(let ((,ival ,initvalue))
       (prog1 (defconst ,symbol ,ival ,docstring)
         (add-variable-watcher
          ',symbol
          (lambda (sym &rest _)
            (and (eq sym ',symbol)
                 (funcall (if (bound-and-true-p entropy/emacs-defconst--just-warn)
                              #'warn #'error)
                          "Do not modify const variable `%s'"
                          ',symbol))))))))

(defmacro entropy/emacs-defconst/only-allow/local
    (symbol initvalue &optional docstring)
  "Same as `defconst' but do not allow any set
and any even modification for variable SYMBOL, but only local
binding in `let' refer. If not an error is raised up."
  (declare (indent defun) (doc-string 3))
  (setq docstring
        (concat (or (and docstring (replace-regexp-in-string "\n+$" "" docstring))
                    "A const variable.")
                "\n\n(NOTE: this variable is defined by \
`entropy/emacs-defconst/only-allow/local')"))
  (macroexp-let2* ignore ((ival nil))
    `(let ((,ival ,initvalue))
       (prog1 (defconst ,symbol ,ival ,docstring)
         (add-variable-watcher
          ',symbol
          (lambda (sym _nval op _wh)
            (and (eq sym ',symbol)
                 (not (memq op '(let unlet)))
                 (error "Do not modify const variable `%s'"
                        ',symbol))))))))

(entropy/emacs-defconst entropy/emacs-false-symbol (make-symbol "false")
  "A non-`intern'ed symbol which indicated semantic \"false\" for
eemacs only.

This variable exists since emacs's inner \"false\" presented symbol
\\='nil' is also a valid value for elisp variables, thus, we can not
distinguish \\='nil' of a value from when it's a \"false\" indicator.
")

(defun entropy/emacs-safety-format (string &rest objects)
  "Like `format' but prevent user use the STRINGas the
only argument apply to it which may cause error while the STRING
is actually a format-string but used as a common string and no
OBJECTS can be formatted."
  (if objects (apply 'format string objects)
    (format "%s" string)))

(defun entropy/emacs-safety-message (format-string &rest args)
  "Like `message' but prevent user use the FORMAT-STRING as the
only argument apply to it which may cause error while the
FORMAT-STRING is actually a format-string but used as a common
string and no ARGS can be formatted."
  (if args (apply 'message format-string args)
    (message "%s" format-string)))

;; ** eemacs top APIs
;; Top declared functions used for eemacs.

(defmacro entropy/emacs-with-lexical-binding-check
    (should-enable &rest body)
  "Error when BODY is (or *is not* when SHOULD-ENABLE is non-nil)
ran in an `lexical-binding' environment.

Return BODY's value if thus of OK is be."
  (declare (indent 1))
  `(if (if ,should-enable (not lexical-binding) lexical-binding)
       (error "lexical-binding %s for body:\n%S"
              (if lexical-binding "should not be enabled"
                "should be enabled") ',body)
     ,(if (cdr body) `(progn ,@body) (car body))))

;; *** compatible refine
(unless (fboundp 'always)
  ;; FIXME: invention from emacs 28 and above, so we should defined as
  ;; top.
  (defun always (&rest _arguments)
    "Do nothing and return t.
This function accepts any number of ARGUMENTS, but ignores them.
Also see `ignore'."
    t))

(if (< emacs-major-version 28)
    (defun entropy/emacs-minibufferp (&optional buffer live)
      "emacs 28 and higher `minibufferp' backport for 27 and lower where
always return non-nil when BUFFER is not lived.

BUFFER defaults to `current-buffer' if ommitted or nil."
      (if live (and (buffer-live-p (or buffer (current-buffer)))
                    (minibufferp buffer))
        (minibufferp buffer)))
  (defalias 'entropy/emacs-minibufferp #'minibufferp))

(if (< emacs-major-version 28)
    (defun entropy/emacs-get-buffer-create
        (buffer-or-name &optional _inhibit-buffer-hooks)
      "emacs 28 and higher `get-buffer-create' backport for 27 and lower
where the INHBIT-BUFFER-HOOKS is ignored since lower emacs
version doesn't has such feature.

\(fn buffer-or-name &optional inhibit-buffer-hooks)"
      (get-buffer-create buffer-or-name))
  (defalias 'entropy/emacs-get-buffer-create #'get-buffer-create))

(if (< emacs-major-version 28)
    (defun entropy/emacs-generate-new-buffer
        (buffer-or-name &optional _inhibit-buffer-hooks)
      "emacs 28 and higher `generate-new-buffer' backport for 27 and
lower where the INHBIT-BUFFER-HOOKS is ignored since lower emacs
version doesn't has such feature.

\(fn buffer-or-name &optional inhibit-buffer-hooks)"
      (generate-new-buffer buffer-or-name))
  (defalias 'entropy/emacs-generate-new-buffer #'generate-new-buffer))

(defun entropy/emacs-buffer-size
    (&optional buffer-or-name whole-buffer)
  "Return `buffer-size' for buffer BUFFER-OR-NAME if WHOLE-BUFFER is
non-nil, otherwise return the `buffer-size' for visible portion
of BUFFER-OR-NAME.

BUFFER-OR-NAME if nil or omitted, defaults to `current-buffer'."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if whole-buffer (buffer-size) (- (point-max) (point-min)))))

(if (< emacs-major-version 29)
    (defun entropy/emacs-define-key
        (&rest args)
      "emacs 28 and higher `define-key' backport for 28 and
lower where the REMOVE is ignored since lower emacs version
doesn't has such feature.

\(fn KEYMAP KEY DEF &optional REMOVE)"
      (define-key (car args) (cadr args) (caddr args)))
  (defalias 'entropy/emacs-define-key #'define-key))

;; *** subr*

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

(defmacro entropy/emacs-dynamic-let* (varlist &rest body)
  "Same as `let*' but binds `boundp' bindings
dynamicly in `lexical-binding' env of `main-thread' (which termed
as LDB i.e. lexical dynamic binding).

If LDB occurred and If `current-thread' is not `main-thread' then
error, since we can not handle collision of concurrency with
dynamic binding while `lexical-binding' enabled.

NOTE: All variables recognized as `boundp' are which
already specialized before the context compiled/defined since
this is a macro."
  (declare (indent 1))
  (if (not lexical-binding) `(let* ,varlist ,@body)
    (let (binds
          cache-binds
          (cache-binds-sym (make-symbol "var-val-cache"))
          var val)
      (dolist (el varlist)
        (if (not (listp el)) (setq var el val nil)
          (setq var (car el) val (cadr el)))
        (if (not (boundp var)) (push (list var val) binds)
          (push `(cons ',var (symbol-value ',var)) cache-binds)
          (push (list var `(set ',var ,val)) binds))
        (unless (symbolp var)
          (error "[entropy/emacs-dynamic-let*]: var `%s' is not symbolp"
                 var)))
      `(let ((,cache-binds-sym
              (list ,@(if cache-binds (nreverse cache-binds) (list nil)))))
         (when (and ,cache-binds-sym (bound-and-true-p main-thread)
                    (not (eq (current-thread) main-thread)))
           (error "[entropy/emacs-dynamic-let*] not in mainthread"))
         (unwind-protect
             (let* ,(nreverse binds) ,(entropy/emacs-macroexp-progn body))
           (when ,cache-binds-sym
             (dolist (el ,cache-binds-sym)
               (when (boundp (car el))
                 (set (car el) (cdr el))))))))))

(defmacro entropy/emacs-when-let*-first (spec &rest body)
  "Like `when-let*' but just check the `car' of SPEC whether is
nil and handled that.

See also `entropy/emacs-when-let*-firstn'."
  (declare (indent 1) (debug if-let))
  (when body
    (if (not spec) (entropy/emacs-macroexp-progn body)
      (let ((fspec (car spec)) (rspec (cdr spec)))
        `(when-let* (,fspec)
           ,@(if rspec `((let* (,@rspec) ,@body)) body))))))

(defmacro entropy/emacs-when-let*-firstn (n spec &rest body)
  "Like `entropy/emacs-when-let*-first' but check the first N
patterns of SPEC whether is nil and handled them.

Where N is a explicitly specified `natnump' number."
  (declare (indent 2) (debug if-let))
  (when body
    (if (not spec) (entropy/emacs-macroexp-progn body)
      (let ((spec-len (length spec)) wspec rspec (i 0))
        (catch :exit
          (dotimes (_ n)
            (unless (< i spec-len) (throw :exit nil))
            (push (nth i spec) wspec) (cl-incf i)))
        (setq wspec (and wspec (nreverse wspec)) rspec (nthcdr i spec))
        (if (not wspec) `(let* (,@spec) ,@body)
          `(when-let* (,@wspec)
             ,@(if rspec `((let* (,@rspec) ,@body)) body)))))))

(cl-defmacro entropy/emacs-let-it-as
    (exp &rest body &key with-it-as &allow-other-keys)
  "Bind EXP's value as `let' local var `it' then evaluate BODY and
return its value.

If WITH-IT-AS set non-nil, it should be explicitly set as a
symbol used instead of `it'."
  (declare (indent 1))
  (let ((it-sym (or with-it-as 'it)))
    `(let ((,it-sym ,exp)) ,@(entropy/emacs--get-def-body body))))

(defmacro entropy/emacs-setf-by-body (var &rest body)
  "Run BODY and using its last form's evaluated value set to a
generalized variable VAR i.e rely on `setf'. Return VAR's new
value.

The main exist reason for this macro is used to clear out the
lisp coding indentation."
  (declare (indent defun))
  (when body `(setf ,var ,(entropy/emacs-macroexp-progn body))))

(defmacro entropy/emacs-setf-by-func (var func &rest args)
  "Call FUNCTION with its ARGS and set its value to variable VAR by
`setf'. Return FUNC's evaluated value.

The main exist reason for this macro is used to clear out the
lisp coding indentation."
  (declare (indent defun))
  `(setf ,var (apply ,func ,@(entropy/emacs-macroexp-rest args))))

(defun entropy/emacs-keywordp (object &optional can-be-var can-be-func)
  "Like `keywordp' but extended with eemacs terms. Return non-nil
for true, nil for false.

Any OBJECTs can be predicated by `keywordp' is true, plus any
symbol interned in initial `obarray' is consider true unless it's
`boundp' or `fboundp'.

Optional arguments CAN-BE-VAR and CAN-BE-FUNC, either one of them
set non-nil orderred to say ignore `boundp' or `fboundp' check
respectively."
  (or (keywordp object)
      (and (symbolp object) (intern-soft object nil)
           (if can-be-var  t (not (boundp object)))
           (if can-be-func t (not (fboundp object))))))

(defun entropy/emacs-maybe-list (object)
  "Wrap OBJECT as a list of it only when OBJECT is not a `listp'
list.

Return the list or OBJECT when not thus."
  (and object
       (if (consp object) object (list object))))

(defun entropy/emacs-maybe-car (x &optional pred ignore-string)
  "Return X if it's not a `sequencep' SEQ, or return the first
element of it.

PRED if set, should be a function which take one argument X and
use it return the result instead buitin's mechanism when X is
SEQ.

Unless PRED is set, a `stringp' X is also directly returned when
IGNORE-STRING is non-nil."
  (when x
    (if (or (not (sequencep x)) (and ignore-string (stringp x))) x
      (if pred (funcall pred x)
        (if (consp x) (car x) (aref x 0))))))

(defmacro entropy/emacs-plist-put
    (plist prop val &optional predicate)
  "Same as `plist-put' but no need to perform the new value into
PLIST to ensure its value be updated even if PLIST is nil.

Plist should be a variable name or a place from where `gv-ref'
can be grabbed

NOTE: PREDICATE is ignored for `emacs-major-version' less than 29
since it's implemented only on above of thus."
  (declare (indent 1))
  (entropy/emacs-with-lexical-binding-check t
    (macroexp-let2* ignore
        ((vref `(gv-ref ,plist))
         (oval `(gv-deref ,vref)))
      `(entropy/emacs-setf-by-body (gv-deref ,vref)
         (if (< emacs-major-version 29)
             (plist-put ,oval ,prop ,val)
           (plist-put ,oval ,prop ,val ,predicate))))))

(defmacro entropy/emacs-run-body-only-once (&rest body)
  "Run BODY just once i.e. the first time invoke it, and return its
value as that once and nil as for any other time."
  (when body
    (let ((sym (make-symbol "__eemacs-temp-anchor")))
      `(unless (bound-and-true-p ,sym)
         (defvar ,sym nil)
         (prog1 (progn ,@body) (setq ,sym t))))))

(defun entropy/emacs-require-only-needed (&rest args)
  "Batch `require' features which specified via ARGS only for those
are not loaded yet.

Each element of args should be a single feature symbol or a full
argument list applied to `require'.

See also `entropy/emacs-require-only-once'."
  (let (fp falp)
    (dolist (fa args)
      (if (setq falp (listp fa)) (setq fp (car fa))
        (setq fp fa))
      (unless (memq fp features)
        (if falp (apply 'require fa)
          (require fa))))))

(defmacro entropy/emacs-require-only-once (&rest args)
  "Require features of ARGS using
`entropy/emacs-require-only-needed' only once in context."
  (macroexpand-1
   `(entropy/emacs-run-body-only-once
     (entropy/emacs-require-only-needed
      ,@args))))

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

(cl-defun entropy/emacs-get-symbol-defcustom-value
    (symbol &key with-eemacs-false)
  "Get SYMBOL standard value CUSVAL setted by `defcustom'.

If WITH-EEMACS-FALSE is set as non-nil, then return value of
`entropy/emacs-false-symbol' when SYMBOL was not defined by
`defcustom' that it doesn't has an default customized value. Otherwise
nil is returned for that case.

If CUSVAL existed and it is equalized (`equal') with the current value
i.e. `symbol-value' SYMVAL of SYMBOL, then SYMVAL is returned,
otherwise return CUSVAL if CUSVAL existed. This behaviour exists since
we want to let the return value `eq' to the original `defcustom''s
init value INITVAL as possible as we can, where the SYMVAL may be
re-structured by user specifiaction which keeping as `equal' as
INITVAL."
  (let ((exp (entropy/emacs-get-symbol-prop symbol 'standard-value))
        (symbdp (boundp symbol)) symval cusval)
    (if (null exp) (and with-eemacs-false entropy/emacs-false-symbol)
      ;; there's no need to evaluated thru judging `lexical-binding'
      ;; whether enabled since `defcustom' bind the exp as an closure
      ;; when needed internally.
      (setq cusval (eval (car exp)))
      (if (and symbdp (equal cusval (setq symval (symbol-value symbol))))
          symval cusval))))

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
               (advice--p (advice--symbol-function func))))
         fval)
    (setq fval
          (or (and advised
                   (advice--cd*r (advice--symbol-function func)))
              func))
    (if (not (symbolp fval)) fval
      (symbol-function fval))))

(defalias 'entropy/emacs-get-func-documentation
  'documentation)

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

(defun entropy/emacs-buffer-local-value (symbol &optional buffer)
  "Like `buffer-local-value' but return value of `entropy/emacs-false-symbol'
when SYMBOL doesn't have a local binding for BUFFER.

Use
    \\='(eq (entropy/emacs-buffer-local-value symbol buffer)
            entropy/emacs-false-symbol)'

to distinguish whether SYMBOL has a valid local value.

BUFFER defaults to `current-buffer' if ignored or omitted."
  (with-current-buffer (or buffer (current-buffer))
    (if (variable-binding-locus symbol)
        (buffer-local-value symbol (current-buffer))
      entropy/emacs-false-symbol)))

(defmacro entropy/emacs-alist-set
    (key alist &optional val no-add testfn)
  "Set KEY of ALIST of value VAL.

(ALIST should be a alist variable name or a PLACE which can be prepared by `gv-ref')

If KEY is not existed in ALIST than add it to the ALIST unless
NO-ADD is non-nil in which case nothing is did.

KEY member checking thru TESTFN which is used as same argument as
what named in `assoc' and default to `equal'.

If VAL is omitted, default to nil.

Return the value of ALIST after modification (or as original if
such NO-ADD is happened)."
  (declare (indent 2))
  (entropy/emacs-with-lexical-binding-check t
    (macroexp-let2* ignore
        ((alist-place-sym `(gv-ref ,alist)) (key-sym key) (testfn-sym testfn)
         (assoc-place-sym `(gv-ref (assoc ,key-sym (gv-deref ,alist-place-sym)
                                          (or ,testfn-sym 'equal))))
         (memqp-sym `(gv-deref ,assoc-place-sym))
         (val-sym val) (no-add-sym no-add))
      `(progn
         (when (if ,no-add-sym ,memqp-sym t)
           (if (not ,memqp-sym)
               (push (cons ,key-sym ,val-sym) (gv-deref ,alist-place-sym))
             (setcdr ,memqp-sym ,val-sym)))
         (gv-deref ,alist-place-sym)))))

(defun entropy/emacs-profiler-is-running-p ()
  "Return non-nil when emacs is running profiler."
  (or
   (and (fboundp 'profiler-cpu-running-p)
        (profiler-cpu-running-p))
   (profiler-memory-running-p)))

(defun entropy/emacs-debugger-is-running-p ()
  "Return non-nil when emacs is runing debugger related tasks."
  (or
   debug-on-error
   debug-on-message
   debug-on-next-call
   debug-on-quit
   debug-on-signal
   (entropy/emacs-profiler-is-running-p)
   ))

(when (< emacs-major-version 28)
  (defface ansi-color-bold
    '((t :inherit bold))
    "Face used to render bold text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-faint
    '((t :weight light))
    "Face used to render faint text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-italic
    '((t :inherit italic))
    "Face used to render italic text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-underline
    '((t :inherit underline))
    "Face used to render underlined text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-slow-blink
    '((t :box (:line-width -1)))
    "Face used to render slowly blinking text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-fast-blink
    '((t :box (:line-width -1)))
    "Face used to render rapidly blinking text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-inverse
    '((t :inverse-video t))
    "Face used to render inverted video text."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-black
    '((t :foreground "black" :background "black"))
    "Face used to render black color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-red
    '((t :foreground "red3" :background "red3"))
    "Face used to render red color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-green
    '((t :foreground "green3" :background "green3"))
    "Face used to render green color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-yellow
    '((t :foreground "yellow3" :background "yellow3"))
    "Face used to render yellow color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-blue
    '((t :foreground "blue2" :background "blue2"))
    "Face used to render blue color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-magenta
    '((t :foreground "magenta3" :background "magenta3"))
    "Face used to render magenta color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-cyan
    '((t :foreground "cyan3" :background "cyan3"))
    "Face used to render cyan color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-white
    '((t :foreground "grey90" :background "gray90"))
    "Face used to render white color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-black
    '((t :foreground "gray30" :background "gray30"))
    "Face used to render bright black color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-red
    '((t :foreground "red2" :background "red2"))
    "Face used to render bright red color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-green
    '((t :foreground "green2" :background "green2"))
    "Face used to render bright green color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-yellow
    '((t :foreground "yellow2" :background "yellow2"))
    "Face used to render bright yellow color code."
    :group 'ansi-colors)

  (defface ansi-color-bright-blue
    '((t :foreground "blue1" :background "blue1"))
    "Face used to render bright blue color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-magenta
    '((t :foreground "magenta2" :background "magenta2"))
    "Face used to render bright magenta color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-cyan
    '((t :foreground "cyan2" :background "cyan2"))
    "Face used to render bright cyan color code."
    :group 'ansi-colors
    :version "28.1")

  (defface ansi-color-bright-white
    '((t :foreground "white" :background "white"))
    "Face used to render bright white color code."
    :group 'ansi-colors
    :version "28.1")

  (defvar ansi-color-basic-faces-vector
    [nil
     ansi-color-bold
     ansi-color-faint
     ansi-color-italic
     ansi-color-underline
     ansi-color-slow-blink
     ansi-color-fast-blink
     ansi-color-inverse]
    "Faces used for SGR control sequences determining a face.
This vector holds the faces used for SGR control sequence parameters 0
to 7.

Parameter  Description
  0        default
  1        bold
  2        faint
  3        italic
  4        underlined
  5        slowly blinking
  6        rapidly blinking
  7        negative image")

  (defvar ansi-color-normal-colors-vector
    [ansi-color-black
     ansi-color-red
     ansi-color-green
     ansi-color-yellow
     ansi-color-blue
     ansi-color-magenta
     ansi-color-cyan
     ansi-color-white]
    "Faces used for SGR control sequences determining a color.
This vector holds the faces used for SGR control sequence parameters
30 to 37 (foreground colors) and 40 to 47 (background colors).

Parameter  Color
  30  40   black
  31  41   red
  32  42   green
  33  43   yellow
  34  44   blue
  35  45   magenta
  36  46   cyan
  37  47   white")

  (defvar ansi-color-bright-colors-vector
    [ansi-color-bright-black
     ansi-color-bright-red
     ansi-color-bright-green
     ansi-color-bright-yellow
     ansi-color-bright-blue
     ansi-color-bright-magenta
     ansi-color-bright-cyan
     ansi-color-bright-white]
    "Faces used for SGR control sequences determining a \"bright\" color.
This vector holds the faces used for SGR control sequence parameters
90 to 97 (bright foreground colors) and 100 to 107 (bright background
colors).

Parameter   Color
  90  100   bright black
  91  101   bright red
  92  102   bright green
  93  103   bright yellow
  94  104   bright blue
  95  105   bright magenta
  96  106   bright cyan
  97  107   bright white")

  )

(defun entropy/emacs-ansi-apply-string (&rest args)
  "Use `entropy/emacs-ansi-apply-string-1' as normal but for some
case we just strip out ansi control chars for preventing error of
invalid ansi-* face bounding such as in pdumper load procedure
uppon emacs-29.

\(fn STRING &optional NO-PROPERTIES)"
  (if (and (bound-and-true-p entropy/emacs-fall-love-with-pdumper)
           (not noninteractive)
           (not (bound-and-true-p global-font-lock-mode)))
      (replace-regexp-in-string
       ;; inspired by `ansi-color-control-seq-regexp' and
       ;; `ansi-color-parameter-regexp'.
       "\\(\e\\[[\x30-\x3F]*[\x20-\x2F]*\\|\e\\)\\(\\([0-9]*\\)[m;]\\)?"
       "" (car args))
    (apply 'entropy/emacs-ansi-apply-string-1 args)))

(defun entropy/emacs-ansi-apply-string-1 (string &optional no-properties)
  "Strip string STRING's ansi control/escape characters by replacing
them as encoding the plain string with emacs text properties.
Return thus of plain string encoded with thus text properties or
just the plain string while NO-PROPERTIES is non-nil.

This function is an extraction ver. of obsoleted emacs-27
`ansi-color-apply' but using `face' instead of `font-lock-face'."
  (if (string-empty-p string) string
    (entropy/emacs-require-only-once 'ansi-color)
    (with-temp-buffer
      (let*
          ((codes (car ansi-color-context))
           (start 0) end result rtn
           ;; obsolete libs
           (func/ansi-color-parse-sequence
            (lambda (escape-seq)
              "Return the list of all the parameters in ESCAPE-SEQ.

ESCAPE-SEQ is a SGR control sequences such as \\033[34m.  The parameter
34 is used by `ansi-color-get-face-1' to return a face definition.

Returns nil only if there's no match for `ansi-color-parameter-regexp'."
              (let ((i 0)
                    codes val)
                (while (string-match ansi-color-parameter-regexp escape-seq i)
                  (setq i (match-end 0)
                        val (string-to-number (match-string 1 escape-seq) 10))
                  ;; It so happens that (string-to-number "") => 0.
                  (push val codes))
                (nreverse codes))))
           (func/ansi-color-apply-sequence
            (lambda (escape-sequence codes)
              "Apply ESCAPE-SEQUENCE to CODES and return the new list of codes.

ESCAPE-SEQUENCE is an escape sequence parsed by
`ansi-color-parse-sequence'.

For each new code, the following happens: if it is 1-7, add it to
the list of codes; if it is 21-25 or 27, delete appropriate
parameters from the list of codes; if it is 30-37 (or 90-97) resp. 39,
the foreground color code is replaced or added resp. deleted; if it
is 40-47 (or 100-107) resp. 49, the background color code is replaced
or added resp. deleted; any other code is discarded together with the
old codes.  Finally, the so changed list of codes is returned."
              ;; (declare (obsolete ansi-color--update-face-vec "29.1"))
              (let ((new-codes (funcall func/ansi-color-parse-sequence escape-sequence)))
                (while new-codes
                  (let* ((new (pop new-codes))
                         (q (/ new 10)))
                    (setq codes
                          (pcase q
                            (0 (unless (memq new '(0 8 9))
                                 (cons new (remq new codes))))
                            (2 (unless (memq new '(20 26 28 29))
                                 ;; The standard says `21 doubly underlined' while
                                 ;; https://en.wikipedia.org/wiki/ANSI_escape_code claims
                                 ;; `21 Bright/Bold: off or Underline: Double'.
                                 (remq (- new 20) (pcase new
                                                    (22 (remq 1 codes))
                                                    (25 (remq 6 codes))
                                                    (_ codes)))))
                            ((or 3 4 9 10) (let ((r (mod new 10)))
                                             (unless (= r 8)
                                               (let (beg)
                                                 (while (and codes (/= q (/ (car codes) 10)))
                                                   (push (pop codes) beg))
                                                 (setq codes (nconc (nreverse beg) (cdr codes)))
                                                 (if (= r 9)
                                                     codes
                                                   (cons new codes))))))
                            (_ nil)))))
                codes)))
           (func/ansi-color-get-face-1
            (lambda (ansi-code &optional bright)
              "Get face definition for ANSI-CODE.
BRIGHT, if non-nil, requests \"bright\" ANSI colors, even if ANSI-CODE
is a normal-intensity color."
              ;; (declare (obsolete ansi-color--face-vec-face "29.1"))
              (when (and bright (<= 30 ansi-code 49))
                (setq ansi-code (+ ansi-code 60)))
              (cond ((<= 0 ansi-code 7)
                     (aref ansi-color-basic-faces-vector ansi-code))
                    ((<= 30 ansi-code 38)
                     (list :foreground
                           (face-foreground
                            (aref ansi-color-normal-colors-vector (- ansi-code 30))
                            nil 'default)))
                    ((<= 40 ansi-code 48)
                     (list :background
                           (face-background
                            (aref ansi-color-normal-colors-vector (- ansi-code 40))
                            nil 'default)))
                    ((<= 90 ansi-code 98)
                     (list :foreground
                           (face-foreground
                            (aref ansi-color-bright-colors-vector (- ansi-code 90))
                            nil 'default)))
                    ((<= 100 ansi-code 108)
                     (list :background
                           (face-background
                            (aref ansi-color-bright-colors-vector (- ansi-code 100))
                            nil 'default))))))
           (func/ansi-color--find-face
            (lambda (codes)
              "Return the face corresponding to CODES."
              (let (faces)
                (while codes
                  (let ((face (funcall func/ansi-color-get-face-1 (pop codes))))
                    ;; In the (default underline) face, say, the value of the
                    ;; "underline" attribute of the `default' face wins.
                    (unless (eq face 'default)
                      (push face faces))))
                ;; Avoid some long-lived conses in the common case.
                (if (cdr faces)
                    (nreverse faces)
                  (car faces))))))
        ;; If context was saved and is a string, prepend it.
        (if (cadr ansi-color-context)
            (setq string (concat (cadr ansi-color-context) string)
                  ansi-color-context nil))
        ;; Find the next escape sequence.
        (while (setq end (string-match
                          ansi-color-control-seq-regexp string start))
          (let ((esc-end (match-end 0)))
            ;; Colorize the old block from start to end using old face.
            (when codes
              (put-text-property start end 'face
                                 (funcall func/ansi-color--find-face codes)
                                 string))
            (push (substring string start end) result)
            (setq start (match-end 0))
            ;; If this is a color escape sequence,
            (when (eq (aref string (1- esc-end)) ?m)
              ;; create a new face from it.
              (setq codes (funcall
                           func/ansi-color-apply-sequence
                           (substring string end esc-end) codes)))))
        ;; if the rest of the string should have a face, put it there
        (when codes
          (put-text-property start (length string)
                             'face (funcall func/ansi-color--find-face codes)
                             string))
        ;; save context, add the remainder of the string to the result
        (let (fragment)
          (if (string-match "\033" string start)
              (let ((pos (match-beginning 0)))
                (setq fragment (substring string pos))
                (push (substring string start pos) result))
            (push (substring string start) result))
          (setq ansi-color-context
                (if (or codes fragment) (list codes fragment))))
        (setq rtn (apply 'concat (nreverse result)))
        (if (not no-properties) rtn
          (substring-no-properties rtn))))))

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

(defmacro entropy/emacs-!defgeneric (&rest args)
  "Same as `cl-defgeneric' but also:

Use =eemacs-defn-bind= of SYMBOL for DEFAULT-BODY when
`lexical-binding' is non-nil. (see
`entropy/emacs-inner-sym-for/current-defname')

\(fn NAME ARGS [DOC-STRING] [OPTIONS-AND-METHODS...] &rest DEFAULT-BODY)"
  (declare (indent 2) (doc-string 3))
  (let((sym-name (car args)))
    `(let ((,entropy/emacs-inner-sym-for/current-defname
            ',sym-name))
       (cl-defgeneric ,sym-name ,@(cdr args)))))

(defmacro entropy/emacs-!defmethod (&rest args)
  "Same as `cl-defmethod' but also:

Use =eemacs-defn-bind= of SYMBOL for BODY when `lexical-binding'
is non-nil. (see `entropy/emacs-inner-sym-for/current-defname')

\(fn NAME [EXTRA] [QUALIFIER] ARGS &rest [DOCSTRING] BODY)"
  (declare (doc-string cl--defmethod-doc-pos) (indent defun))
  (let ((sym-name (car args)))
    `(let ((,entropy/emacs-inner-sym-for/current-defname
            ',sym-name))
       (cl-defmethod ,sym-name ,@(cdr args)))))

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
  (unless args (setq args (list nil)))
  (let ((sym (make-symbol "it")))
    `(entropy/emacs-!with-cdefn
       :with-it-as ,sym
       (if (not ,sym) (message ,format-string ,@args)
         (message (concat "[%s] " ,format-string)
                  ,sym ,@args)))))

(defmacro entropy/emacs-!error (string &rest args)
  "Like `error' but message with =eemacs-defn-bind= as prefix when
available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  (unless args (setq args (list nil)))
  (let ((sym (make-symbol "it")))
    `(entropy/emacs-!with-cdefn
       :with-it-as ,sym
       (if (not ,sym) (error ,string ,@args)
         (error (concat "[%s] " ,string)
                ,sym ,@args)))))

(defmacro entropy/emacs-!user-error (string &rest args)
  "Like `user-error' but message with =eemacs-defn-bind= as prefix
when available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  (unless args (setq args (list nil)))
  (let ((sym (make-symbol "it")))
    `(entropy/emacs-!with-cdefn
       :with-it-as ,sym
       (if (not ,sym) (user-error ,string ,@args)
         (user-error (concat "[%s] " ,string)
                     ,sym ,@args)))))

;; *** making procedure
(defvar __entropy/emacs-is-make-session-check-done nil)
(defvar __entropy/emacs-is-make-session-value-cache nil)
(defun entropy/emacs-is-make-session ()
  "Obtained the 'EEMACS_MAKE' env variable value if valid
otherwise return nil.

This function commonly used to judge whether start eemacs in a make
session, where specially indicate to other subroutines to get the
eemacs make section type according to the value of entropy emacs
specified environment variable \"EEMACS_MAKE\".

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (if __entropy/emacs-is-make-session-check-done
      __entropy/emacs-is-make-session-value-cache
    (let ((env-p (entropy/emacs-getenv "EEMACS_MAKE")))
      (setq __entropy/emacs-is-make-session-value-cache
            env-p
            __entropy/emacs-is-make-session-check-done
            t)
      __entropy/emacs-is-make-session-value-cache)))

(defun entropy/emacs-is-make-all-session ()
  "Obtained the 'EEMACS_MAKE_ALL' env variable value if valid
otherwise return nil.

This function commonly used to judge whether eemacs is under a
make session which means for making all eemacs make sections.

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (entropy/emacs-getenv "EEMACS_MAKE_ALL"))

(defun entropy/emacs-is-make-with-all-yes-session ()
  "Obtained the 'EEMACS_MAKE_WITH_ALL_YES' env variable value if
valid otherwise return nil.

This function commonly used to judge whether eemacs is under a
make session without any `yes-or-no-p' validation as default to
*yes*.

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (entropy/emacs-getenv "EEMACS_MAKE_WITH_ALL_YES"))

;; *** eemacs-require-func

(defun entropy/emacs-common-require-feature
    (feature &optional filename noerror)
  "eemacs spec `require' facility , to prefer load the elisp
source rather than its compiled version in some cases.

NOTE: not support load dynamic module"
  (let (_)
    (cond
     ((entropy/emacs-suggest-startup-with-elisp-source-load-p)
      (require feature (or filename (format "%s.el" feature)) noerror))
     (t (require feature filename noerror)))))

(defalias '!eemacs-require
  #'entropy/emacs-common-require-feature
  "Alias for `entropy/emacs-common-require-feature' but just used
in baron part to simplify context distinction search")

;; *** high perfomance alist

(defun entropy/emacs--make-alist-with-symbol-prop-set/core-func
    (vsym nv _op _wh keyname &optional init with-single)
  (let* ((inhibit-quit t) (oldval (symbol-value vsym))
         alkey alkey-is-sym-p single-p nsingle-p
         decfunc)
    (entropy/emacs-setf-by-body decfunc
      (lambda (x)
        (or (and (prog1 (entropy/emacs-setf-by-body alkey-is-sym-p
                          (symbolp (setq alkey
                                         (if (setq nsingle-p (consp x))
                                             (car x) x))))
                   (setq single-p (not nsingle-p)))
                 with-single)
            (and alkey-is-sym-p nsingle-p))))
    (when (and (not init) (consp oldval))
      (dolist (el oldval)
        (if (funcall decfunc el) (put alkey keyname nil))))
    (setq nv (if init oldval nv))
    (when (consp nv)
      (dolist (el nv)
        (when (funcall decfunc el)
          (put alkey keyname
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

KEY-SYM should be `entropy/emacs-keywordp' i.e. set as `:key', `sym'
or the defination is failed with error. And it must a purely presented
symbol which not have any variable or function definations bounded (as
why we restrict this since if not is so strange right?).

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
             (intern (format "__eemacs/%s/high-perfomance-alist/set-guard"
                             ,varsym))))
       (unless (and (symbolp ,varsym) (special-variable-p ,varsym))
         (user-error "var-sym `%S' is not `symbolp' or `special-variable-p'"
                     ,varsym))
       (unless (entropy/emacs-keywordp ,keysym)
         (user-error "key-sym `%S' is not `keywordp'" ,keysym))
       ;; We should remove the old var-watcher firstly.
       (remove-variable-watcher ,varsym var-guard-func-name)
       ;; internal usage
       (put ,varsym '__eemacs-alist-get__ ,keysym)
       (funcall 'entropy/emacs--make-alist-with-symbol-prop-set/core-func
                ,varsym nil nil nil ,keysym 'init ,wssym)
       (defalias var-guard-func-name
         (lambda (vsym nv op wh)
           (when (eq op 'set)
             (funcall 'entropy/emacs--make-alist-with-symbol-prop-set/core-func
                      vsym nv op wh ,keysym nil ,wssym)))
         (format "Variable guard for high performance alist variable `%s', made
via `entropy/emacs-make-alist-with-symbol-prop-set'." ,varsym))
       (add-variable-watcher ,varsym var-guard-func-name))))

;; *** eemacs env

(cl-defmacro entropy/emacs-env-with-pure-eemacs-env
    (defdir &rest body &key load-custom-file-p &allow-other-keys)
  `(let ((default-directory (or ,defdir default-directory))
         (proc-env
          (cons "EEMACS_INIT_WITH_PURE=t"
                process-environment)))
     (let ((process-environment
            (if ,load-custom-file-p
                (cons "EEMACS_INIT_WITH_PURE_LCSTF=t" proc-env)
              proc-env)))
       ,@(entropy/emacs--get-def-body body 'with-safe))))

(defun entropy/emacs-trim-process-environment
    (&optional procenv &rest env-varnames)
  "Trim all envrionment variables named as one of ENV-VARNAMES from
PROCENV (using `process-environment' as default when it's not
specified, otherwise it should has same structure as
`process-environment'.).

Return the trimed PROCENV or nil when all is trimed out via
ENV-VARNAMESa."
  (save-match-data
    (let (rtn mvnm)
      (dolist (el (or procenv process-environment))
        (setq mvnm (car (split-string el "=")))
        (unless (member mvnm env-varnames)
          (push el rtn)))
      (and rtn (nreverse rtn)))))

;; *** hook

(entropy/emacs-!cl-defun entropy/emacs-member-hook
    (hook &optional as-hook-value reverse &rest elements)
  "Return a list of items of ELEMENTS that memberred in HOOK, or of
those not memberred in if REVERSE is non-nil. The return's order
is not changed. Return nil when not of thus found.

HOOK should be a hook symbol which acceptable by `run-hooks'
except when HOOK is a symbol and AS-HOOK-VALUE is non-nil, then
HOOK is consider as the value of a hook symbol
internally. Otherwise, any other value type of HOOK is invalid
and an error will be throwed out.

If HOOK is a hook symbol, then both its local value (obtained by
`entropy/emacs-buffer-local-value') and its global
value (`default-value') will be mapping thru out."
  (entropy/emacs-when-let*-first
      ((hval
        (cond (as-hook-value
               (list hook))
              ((symbolp hook)
               (list (entropy/emacs-buffer-local-value hook)
                     (default-value hook)))
              (t (entropy/emacs-!error "wrong type of argument: hook %s"
                                       hook))))
       mm rtn)
    (dolist (el hval)
      (unless (or (null el) (eq el entropy/emacs-false-symbol))
        (cond ((symbolp el)
               (when (setq mm (cl-position el elements :test 'eq))
                 (push mm rtn)))
              ((consp el)
               (dolist (i el)
                 (when (setq mm (cl-position i elements :test 'eq))
                   (push mm rtn)))))))
    (when rtn
      (let* (k (fn (lambda (x y)
                     (when (if reverse (not (memql x y))
                             (memql x y))
                       (push (nth x elements) k)))))
        (dotimes (i (length elements))
          (funcall fn i rtn))
        (nreverse k)))))

;; *** file

(defun entropy/emacs-file-truename (file)
  "Return the true name of FILE, which has same made procedure as
`buffer-file-truename'."
  (abbreviate-file-name (file-truename file)))

(entropy/emacs-!cl-defun entropy/emacs-touch-file
    (file &optional ok-if-file-is-directory-file-name)
  "Make a new file FILE which as *nix command `touch' does. Return
the FILE's `entropy/emacs-file-truename' FTNAME.

If FILE exists already, return FTNAME Immediately.

If FILE exists as an `file-directory-p' directory, an error will
be throwed out.

If FILE named as an `directory-name-p' filename, an error will be
throwed out unless OK-IF-FILE-IS-DIRECTORY-FILE-NAME is non-nil
in which case we will reuse the `directory-file-name' version of
that name as FILE later."
  (if (file-exists-p file)
      (if (file-directory-p file)
          (entropy/emacs-!error "file %s is a exist directory" file)
        (entropy/emacs-file-truename file))
    (if (and (directory-name-p file)
             (not ok-if-file-is-directory-file-name))
        (entropy/emacs-!error "file %s is name of a directory" file)
      (let* ((f (directory-file-name file))
             (d (file-name-directory f)))
        (unless (file-directory-p d) (mkdir d t))
        (with-temp-file file
          (entropy/emacs-file-truename file))))))

;; *** process

(defun entropy/emacs--make-process-top-advice (orig-func &rest orig-args)
  (let ((stderr (plist-get orig-args :stderr))
        (proc (apply orig-func orig-args)))
    (process-put proc '__eemacs_make_process_prop_stderr__
                 stderr)
    proc))
(advice-add 'make-process :around #'entropy/emacs--make-process-top-advice)

(defun entropy/emacs-process-stderr-object (process)
  "Return the `stderr' object of process made via `make-process'."
  (process-get process '__eemacs_make_process_prop_stderr__))

(defun entropy/emacs-process-stderr-buffer (process)
  "Like `process-buffer' but when PROCESS is made by `make-process'
then return its stderr buffer or nil if non
of thus."
  (when-let* ((buff (process-get process '__eemacs_make_process_prop_stderr__))
              ((and (bufferp buff) (buffer-live-p buff))))
    buff))

(defun entropy/emacs-process-buffer-prefer-stderr (process)
  "Like `process-buffer' but preferred return its stderr buffer when
PROCESS is made via `make-process' and
it has one of thus, otherwise same as `process-buffer'."
  (or (entropy/emacs-process-stderr-buffer process)
      (process-buffer process)))

;; ** INIT

(entropy/emacs-defconst
  entropy/emacs-inner-preload-vars-file
  (expand-file-name ".eemacs-preload.el" entropy/emacs-stuffs-topdir)
  "eemacs preload file for init some inner bootstrap/preserved variables.

See `entropy/emacs-inner-preload-vars'.

EEMACS_MAINTENANCE:

This file just be loaded for a byte-compiled session i.e. prediated
via `entropy/emacs-suggest-startup-with-elisp-source-load-p' since we
create this file intended to preserve some inner metadata that
byte-compile generated but source loading undeeded.")
(unless (entropy/emacs-suggest-startup-with-elisp-source-load-p)
  (load entropy/emacs-inner-preload-vars-file))

(defvar entropy/emacs-inner-preload-vars nil
  "A list variable symbol to be saved on
`entropy/emacs-inner-preload-vars-file'.

EEMACS_MAINTENANCE:

Each var of this list should be interned to default `obarray' since we
can not write to file with different obarray distinguished symbols
unless use byte-compile with macroexpand hack.")

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

(defun entropy/emacs--inner-setenv (&rest args)
  "eemacs internal `setenv' variant to maintain assigned env cross
session like for `entropy/emacs-pdumper-load-hook'.

NOTE: do not use this unless for eemacs inner facilities
developments."
  (prog1 (apply 'setenv args)
    (if (bound-and-true-p entropy/emacs-fall-love-with-pdumper)
        (if (bound-and-true-p entropy/emacs-pdumper-load-hook)
            (setq entropy/emacs-pdumper-load-hook
                  (nconc entropy/emacs-pdumper-load-hook
                         (list (lambda nil (apply 'setenv args)))))
          (setq entropy/emacs-pdumper-load-hook
                (list (lambda nil (apply 'setenv args))))))))

;; The eemacs specified envrionment to indicated all subprocess are
;; ran in an eemacs session, in which case all subprocess can detected
;; this variable to do some extra operations or something else.
(entropy/emacs--inner-setenv "EEMACS_ENV" "TRUE")

;; ** Start Eemacs

;; forbidden `entropy/emacs-custom-enable-lazy-load' at special
;; session.
(cond ((and entropy/emacs-custom-enable-lazy-load
            (or (and entropy/emacs-fall-love-with-pdumper
                     (not entropy/emacs-do-pdumping-with-lazy-load-p))
                ;; NOTE: We should not enable lazy load for a systemd
                ;; service session both for running and compiling
                (entropy/emacs-getenv "EEMACS_SYSTEMD_DAEMON_SERVICE")
                (daemonp)))
       (setq entropy/emacs-custom-enable-lazy-load nil))
      ((and entropy/emacs-fall-love-with-pdumper
            entropy/emacs-do-pdumping-with-lazy-load-p)
       (setq entropy/emacs-custom-enable-lazy-load t)))

(cond
 ((entropy/emacs-is-make-session)
  (!eemacs-require 'entropy-emacs-batch))
 ((entropy/emacs-env-init-with-pure-eemacs-env-p)
  (!eemacs-require 'entropy-emacs-pure-env))
 (t (!eemacs-require 'entropy-emacs-start)))

;; * Provide
(provide 'entropy-emacs)
