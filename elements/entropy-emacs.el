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
  (unless args (setq args (list nil)))
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'message ,format-string ,@args)
       (message (concat "[%s] " ,format-string)
                cur-defn ,@args))))

(defmacro entropy/emacs-!error (string &rest args)
  "Like `error' but message with =eemacs-defn-bind= as prefix when
available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  (unless args (setq args (list nil)))
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'error ,string ,@args)
       (error (concat "[%s] " ,string)
              cur-defn ,@args))))

(defmacro entropy/emacs-!user-error (string &rest args)
  "Like `user-error' but message with =eemacs-defn-bind= as prefix
when available.

(see `entropy/emacs-inner-sym-for/current-defname')"
  (unless args (setq args (list nil)))
  `(entropy/emacs-!with-cdefn
     :with-it-as cur-defn
     (if (not cur-defn) (apply 'user-error ,string ,@args)
       (user-error (concat "[%s] " ,string)
                   cur-defn ,@args))))

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
    (let ((env-p (getenv "EEMACS_MAKE")))
      (setq __entropy/emacs-is-make-session-value-cache
            (cond
             ((or (null env-p) (string-empty-p env-p)) nil)
             (t env-p))
            __entropy/emacs-is-make-session-check-done
            t)
      __entropy/emacs-is-make-session-value-cache)))

(defun entropy/emacs-is-make-all-session ()
  "Obtained the 'EEMACS_MAKE_ALL' env variable value if valid
otherwise return nil.

This function commonly used to judge whether start emacs in a
`noninteractive' status but in daemon load procedure, where
specially indicate to other subroutines to get the 'batch
run' (e.g. use entropy emacs as a shell) type according to the
value of entropy emacs specified environment variable
\"EEMACS_MAKE_ALL\".

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (let ((env-p (getenv "EEMACS_MAKE_ALL")))
    (cond
     ((or (null env-p) (string-empty-p env-p)) nil)
     (t env-p))))

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
