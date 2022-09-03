;;; entropy-emacs-defun.el --- entropy emacs pre-defined libraries  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defun.el
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
;; This file was built on the =func-binds= concept of entropy-emacs's
;; designation.
;;
;; For be as the underlying project function library hosting
;; master. Every part of this file can be sharing on the context and
;; splitting into categories individually with using outline mode
;; doc's context format.
;;
;; * Configuration:
;;
;; Just requiring it before checking the file dependencies.
;;
;; * Code:
;; ** Require
(entropy/emacs-common-require-feature 'entropy-emacs-defcustom)
(entropy/emacs-common-require-feature 'entropy-emacs-defvar)
(entropy/emacs-common-require-feature 'entropy-emacs-defconst)
(entropy/emacs-common-require-feature 'entropy-emacs-defface)
(entropy/emacs-common-require-feature 'entropy-emacs-message)
(require 'cl-lib)
(require 'rx)

;; ** internal libs
(cl-defun entropy/emacs-defun--get-real-body (list-var)
  "Get BODY inside of plist like list LIST-VAR, commonly is the
last `keywordp' keypair's cdr or return LIST-VAR when the car of
LIST-VAR is not a `keywordp' keyword.

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
        (if (keywordp (car it))
            (setq it (cddr it))
          (throw 'break it))))))

(defun entropy/emacs-eval-with-lexical (form &optional actual-lexical-binding)
  "Like `eval' but forcely enable `lexical-binding' as t.

ACTUAL-LEXICAL-BINDING when set then it is replaced the LEXICAL
arg for `eval'."
  (eval form (or actual-lexical-binding t)))

;; ** Common manipulations
;; *** Emacs internal api replacement

(defun entropy/emacs-error-without-debug (string &rest args)
  "Like `error' but always press `debug-on-error'"
  (let ((debug-on-error nil))
    (apply 'error string args)))

(defvar entropy/emacs-make-dynamic-symbol-as-same-value/heap-head-number 0)
(defun entropy/emacs-make-dynamic-symbol-as-same-value (var)
  "Make a new dynamic symbol (predicated by `special-variable-p'
and interned in `obarray') whose value is same as VAR's value
i.e. predicated by `eq'."
  (let* (sym-rtn
         (sym-make-func
          (lambda nil
            (setq sym-rtn
                  (prog1
                      (intern
                       (format "entropy/emacs-make-dynamic-symbol-as-same-value/%d"
                               entropy/emacs-make-dynamic-symbol-as-same-value/heap-head-number))
                    (cl-incf entropy/emacs-make-dynamic-symbol-as-same-value/heap-head-number))))
          ))
    (while (boundp (funcall sym-make-func)))
    (entropy/emacs-eval-with-lexical
     ;; use `defvar' to declare it as an dynamic special variable
     ;; without lexical-binding environment wrapped.
     `(defvar ,sym-rtn ',var
        "Dynamic variable defined by `entropy/emacs-make-dynamic-symbol-as-same-value'")
     ;; do not use lexical env
     nil)
    sym-rtn))

(defun entropy/emacs-make-new-function-name
    (arglist &optional docstring decl &rest body)
  "Define a function with unexisted name symbol and return the new
defined function name.

ARGLIST is the arglist for that function, and BODY is the body of
that function and decl is the declaration of that function.

Since the defination process is same as `defun', so you also can
inject `interactive' form in BODY."
  (let ((sym (entropy/emacs-make-dynamic-symbol-as-same-value
              (lambda (&rest _)
                (error "Wrong usage of this function symbol")))))
    (entropy/emacs-eval-with-lexical
     `(defun ,sym ,arglist
        ,docstring
        ,decl
        ,@body))
    sym))

(defun entropy/emacs-unintern-symbol (symbol &optional use-obarray)
  "Like `unintern' but use SYMBOL as the main arg since although
`unintern' support symbol as main arg but it may not `eq' to the
one in which obarray used, so this function use `symbol-name'
forcely get that name in USE-OBARRAY."
  (unintern (symbol-name symbol) use-obarray))

(defmacro entropy/emacs-unwind-protect-unless-success (body &rest unwindforms)
  "Like `unwind-protect' but just run UNWINDFORMS when BODY run with fatal."
  (declare (indent 1))
  (let ((sym (make-symbol "sym")))
    `(let ((,sym t))
       (unwind-protect
           (prog1
               ,body
             (setq ,sym nil))
         (if ,sym
             (progn
               ,@unwindforms))))))

(defmacro entropy/emacs-add-to-list
    (list-var element &optional append compare-fn)
  "Add ELEMENT to the top (the `car') of LIST-VAR unless APPEND is
non-nil in which case add to the tail of thus.

If ELEMENT is presented in LIST-VAR which be predicated by `member'
as default (or using COMPARE-FN when its non-nil), do nothing.

The LIST-VAR is modified unless do-nothing happened.

LIST-VAR can be a generalized varaible.
"
  `(unless (if (and ,compare-fn t)
               (funcall ,compare-fn ,element ,list-var)
             (member ,element ,list-var))
     (if ,append
         (setf ,list-var (append ,list-var (list ,element)))
       (setf ,list-var (push ,element ,list-var)))))

(defmacro entropy/emacs-setq-single-with-explicit-return (var val)
  "Like `setq' but just for the variable VAR and its value VAL and
explicit use the VAR as return finally in which case the byte
compiler doesn't throw the warning for unsed case.

Details for when you want to use a lexical binding as set with return
only like:

: (let (foo bar) (setq bar (setq foo 1)) bar)

The byte compiler will throw an error like:

#+begin_quote
Warning: Unused lexical variable `foo'
#+end_quote

And when use this macro, the expansion is look as:

: (let (foo bar) (setq bar (progn (setq foo 1) foo)) bar)

So that the lexical var FOO is used and no such error will occurred
on.
"
  (declare (indent defun))
  `(progn
     (setq ,var ,val)
     ,var))

;; **** Apis with when wrapper
(defmacro entropy/emacs-when-defun
    (name arglist condition-form &optional docstring &rest body)
  "Like `defun' but only define NAME as an function when
CONDITION-FORM is evaled return non-nil."
  (declare (indent defun))
  `(when (progn ,condition-form)
     (defun ,name ,arglist
       ,docstring
       ,@body)))

(cl-defmacro entropy/emacs-save-excursion-when
    (&rest body &key when &allow-other-keys)
  "Like `save-excursion' when WHEN is set and evaluated return
non-nil, or run BODY like `progn'."
  (let ((body (entropy/emacs-defun--get-real-body body))
        (use-when-p (not (null when))))
    `(if (and ,use-when-p ,when)
         (save-excursion ,@body)
       (progn
         ,@body))))

(cl-defmacro entropy/emacs-widen-when
    (&rest body &key when &allow-other-keys)
  "Run BODY after `widen' the `current-buffer' when WHEN is set and
evaluated return non-nil, or run BODY like `progn'."
  (let ((body (entropy/emacs-defun--get-real-body body))
        (use-when-p (not (null when))))
    `(progn
       (if (and ,use-when-p ,when) (widen))
       ,@body)))

;; *** Cl-function compatible manipulation
(defun entropy/emacs-cl-findnew-p (func)
  (let (new-func)
    (catch :exit
      (dolist (ref entropy/emacs-cl-compatible-reflects)
        (if (consp ref)
            (when (eq func (car ref))
              (setq new-func (cdr ref)))
          (when (eq ref func)
            (setq new-func
                  (intern (format "cl-%s" (symbol-name func))))))
        (when (not (null new-func))
          (throw :exit nil))))
    new-func))

(defmacro entropy/emacs-cl-compatible-apply (cl-func &rest args)
  "Macro for be forward compatibility of `cl.el' with `cl-lib.el'
in new emacs-version."
  `(let (cl-func-use)
     (cond ((and (require 'cl-lib)
                 (not (null (entropy/emacs-cl-findnew-p ',cl-func))))
            (setq cl-func-use (entropy/emacs-cl-findnew-p ',cl-func))
            (unless (fboundp cl-func-use)
              (user-error "Can not find cl function `%s'" cl-func-use)))
           ((fboundp ',cl-func)
            (setq cl-func-use ',cl-func)))
     (funcall cl-func-use ,@args)))

;; *** Sexp read and print

(defun entropy/emacs-read-base64-encoded-sexp-from-buffer
    (&optional buff)
  "Read base64 encoded sexp object getted from BUFFER which did
by `entropy/emacs-generate-base64-encoded-sexp-buffer' and return
it, use `current-buffer' when BUFFER is nil.

NOTE: the sexp must readable or throw the error."
  (let ((sexp-str
         (decode-coding-string
          (base64-decode-string
           (read (with-current-buffer (or buff (current-buffer))
                   (goto-char (point-min))
                   (current-buffer))))
          'utf-8-auto))
        ;; Parent expects UTF-8 encoded text.
        (coding-system-for-read 'utf-8-auto)
        (coding-system-for-write 'utf-8-auto))
    (read sexp-str)))

(defun entropy/emacs-generate-base64-encoded-sexp-buffer
    (sexp &optional buff)
  "Encode an sexp of any list object with base64 method and
insert into an fresh new created buffer or BUFF when
specified (NOTE: the specified buff will be tidy up), and return
the buffer with its current point at `point-min'.

You can use `entropy/emacs-read-base64-encoded-sexp-from-buffer'
to read the buffer directly to re-get the sexp in to
current emacs session."
  (let* ((inhibit-read-only t)
         (buff (or (and (bufferp buff) (buffer-live-p buff)
                        (with-current-buffer buff
                          (erase-buffer)
                          buff))
                   (generate-new-buffer " *eemacs-sexp-enc*")))
         (print-level nil)
         (print-length nil)
         (print-escape-nonascii t)
         (print-circle t))
    (prin1 sexp buff)
    (with-current-buffer buff
      ;; Just in case the string we're sending might contain EOF
      (encode-coding-region (point-min) (point-max) 'utf-8-auto)
      ;; base64 encoding
      (base64-encode-region (point-min) (point-max) t)
      ;; wrapper the result
      (goto-char (point-min)) (insert ?\")
      (goto-char (point-max)) (insert ?\" ?\n)
      ;; finally goto the bob
      (goto-char (point-min)))
    buff))

;; *** Print manipulation

(defun entropy/emacs-advice-func-around-for-print-limit
    (func-name &optional level length satisfy-func)
  "Make function func restricted by `print-level' LEVEL and
`print-length' LENGTH.

LEVEL and LENGTH are optional, if that fallback to 3 and 20 by
defautly.

Third optional arg SATISFY-FUNC is a function which used to judge
whether enable the limitation when its return is non-nil,
otherwise uses the original procedure."
  (advice-add
   func-name
   :around
   (let ((ad-name
          (intern
           (format "entropy/emacs-print-limit-advice-for-%s"
                   func-name))))
     (entropy/emacs-eval-with-lexical
      `(defun ,ad-name (orig-func &rest orig-args)
         (if (or (null ',satisfy-func)
                 (and (functionp ',satisfy-func)
                      (funcall ',satisfy-func)))
             (let ((print-level (or ,level 3))
                   (print-length (or ,length 20)))
               (apply orig-func orig-args))
           (apply orig-func orig-args))))
     ad-name)))

;; *** Place setf

(defmacro entropy/emacs-swap-two-places-value
    (place-a place-b &rest conditions)
  "Swap two places' (PLACE-A's and PLACE-B's) value using `setf'
when the last form of CONNDITIONS evaluated return non-nil."
  (declare (indent defun))
  (let ((tmpvar-sym (make-symbol "--tmpvar--")))
    `(when (progn ,@conditions)
       (let ((,tmpvar-sym ,place-b))
         (setf ,place-b ,place-a)
         (setf ,place-a ,tmpvar-sym)))))

;; *** Symbol manupulation

(defun entropy/emacs-get-symbol-defcustom-value (symbol)
  "Get SYMBOL standard value setted by `defcustom'."
  (entropy/emacs-eval-with-lexical (car (get symbol 'standard-value))))

;; *** List manipulation
;; **** Basics
;; ***** Core

(defsubst entropy/emacs-dotted-listp (list)
  "Return non-nil if LIST is a dotted end list and is not a
circular list.

In other word, a dotted list is a list whose last cdr is a
`atom'.

(see also `entropy/emacs-circular-listp' for circular list explanation.)"
  (catch :exit
    (if (proper-list-p list)
        (throw :exit nil))
    (when (entropy/emacs-circular-listp
           list :internal-without-check-proper t)
      (throw :exit nil))
    t))

(cl-defmacro entropy/emacs-list-map-cdr
    (list &rest body &key with-exit with-it-as &allow-other-keys)
  "Run BODY in mapping through each `cdr' of `listp' list LIST with
bind 'it' of the current cdr which is came from LIST to the cdr
of the last `consp' cons-cell of LIST.

Interrupted when mapping done or the BODY's last form's
evaluation return non-nil in any step of progress only when
WITH-EXIT is set.

When WITH-IT-AS is set, it should be a explicit a symbol for this
macro to use it instead of 'it' as what bind for."
  (declare (indent defun))
  (let ((rest-sym (make-symbol "rest"))
        (exit-sym (make-symbol "exit"))
        (body-rtn-sym (make-symbol "body-rtn"))
        (body (entropy/emacs-defun--get-real-body body)))
    `(let ((,rest-sym ,list)
           (,exit-sym ,with-exit)
           (,body-rtn-sym nil)
           ;; exposed internal let binding
           ,(or with-it-as 'it))
       (when ,rest-sym
         (while (and (consp ,rest-sym)
                     (if ,exit-sym
                         (not ,body-rtn-sym)
                       t))
           (setq ,(or with-it-as 'it) ,rest-sym
                 ,rest-sym (cdr ,rest-sym))
           (setq ,body-rtn-sym (progn ,@body)))))))

(cl-defmacro entropy/emacs-list-map-car
    (list &rest body &key with-exit with-tail with-it-as &allow-other-keys)
  "Run BODY in mapping through each `car' of `listp' list LIST with
bind 'it' of the current car which is came from first car of LIST
to the last car of the `consp' cons-cell' of LIST.

When WITH-TAIL is set, then also run BODY for the last cdr of a
`entropy/emacs-dotted-listp' list.

Interrupted when mapping done or the BODY's last form's
evaluation return non-nil in any step of progress only when
WITH-EXIT is set.

When WITH-IT-AS is set, it should be a explicit a symbol for this
macro to use it instead of 'it' as what bind for."
  (declare (indent defun))
  (let ((rest-sym (make-symbol "rest"))
        (exit-sym (make-symbol "exit"))
        (body-rtn-sym (make-symbol "body-rtn"))
        (body (entropy/emacs-defun--get-real-body body)))
    `(let ((,rest-sym ,list)
           (,exit-sym ,with-exit)
           (,body-rtn-sym nil)
           ;; exposed internal let binding
           ,(or with-it-as 'it))
       (when ,rest-sym
         (while (and (consp ,rest-sym)
                     (if ,exit-sym
                         (not ,body-rtn-sym)
                       t))
           (setq ,(or with-it-as 'it) (car ,rest-sym)
                 ,rest-sym (cdr ,rest-sym))
           (setq ,body-rtn-sym (progn ,@body)))
         (when (and (setq ,(or with-it-as 'it) ,rest-sym)
                    (if ,exit-sym
                        (not ,body-rtn-sym)
                      t)
                    ,with-tail)
           ,@body)))))

(cl-defun entropy/emacs-list-get-region
    (list &key start end with-destructively ignore-empty-region)
  "Return a new list which is a region of LIST from START to END.

START and END is re-calculated by
`entropy/emacs-seq-recalc-start-end' before progress.

When START is overflow, then return nil.

When START and END is same, then return the `car' of that place
unless IGNORE-EMPTY-REGION is set in which case return nil.

END when nil or omitted is indicate the place to the end of the
LIST i.e. `length' of list.

If WITH-DESTRUCTIVELY is set, the original LIST may be modified
i.e. when END is not the end of LIST, and the return is use same
storage as origin. Otherwise return the region as a new list which is
copy from origin."
  (entropy/emacs-seq-recalc-start-end list start end :with-set-end t)
  (catch :exit
    (if (and end (= start end)) (throw :exit (unless ignore-empty-region (nth start list)))
      (if (and (null end) (null (cdr list)) (= start 0)) (throw :exit (car list))
        (if (and (null (cdr list)) (> start 0)) (throw :exit nil))))
    (let ((i 0) exit beglist begpt ptdiff (end-is-end-p (null end)))
      (entropy/emacs-list-map-cdr list
        :with-exit t
        (when (= i start)
          (setq beglist it
                begpt i))
        (when begpt
          (if (and end-is-end-p)
              (setq exit t)
            (when (= (1+ i) end)
              (setq ptdiff (- i begpt))
              (unless (setq end-is-end-p (null (cdr it)))
                (if with-destructively (setcdr it nil)))
              (setq exit t))))
        (cl-incf i)
        exit)
      (unless begpt
        (throw :exit nil))
      ;; track end is overflow
      (when (and (not ptdiff) end)
        (if end-is-end-p (error "internal error"))
        (setq end-is-end-p t))
      (if with-destructively
          beglist
        (if end-is-end-p
            (copy-sequence beglist)
          (setq i 0 exit nil)
          (entropy/emacs-list-map-cdr
            (setq beglist (copy-sequence beglist))
            :with-exit t
            (when (= i ptdiff)
              (setcdr it nil)
              (setq exit t))
            (cl-incf i)
            exit)
          beglist)))))

(cl-defun entropy/emacs-circular-listp (list &key internal-without-check-proper)
  "Return the `safe-length' of LIST when its a circular list or nil
while its a `proper-list-p' list or a non-circular
`entropy/emacs-dotted-listp' list.

A circular list is a kind of list with self or portion circularity
feature, for example:

To build a self-circular list do:
#+begin_src elisp
  (let ((foo '(1 2)))
    (setcdr (cdr foo) foo)
    foo)
#+end_src

in which case the var 'foo' is circular with list whose car and `cadr'
is 1 and 2, in other word it is self-circular.

To build a portion-circular list do:
#+begin_src elisp
  (let ((foo '(1 2))
        (bar '(3 4)))
    (setcdr (cdr bar) bar)
    (setcdr (cdr foo) bar)
    foo)
#+end_src

in which case the var 'foo' is circular at `cddr' with self-circular
list 'bar', in other word foo is portion-circular.
"
  (unless (listp list)
    (signal 'wrong-type-argument `(listp ,list)))
  (catch :exit
    (when (and (not internal-without-check-proper)
               (proper-list-p list))
      (throw :exit nil))
    (let* ((llen (safe-length list))
           (mplv (1- llen))
           (i 0)
           exit rtn)
      (entropy/emacs-list-map-cdr list
        :with-exit t
        (when (= i mplv)
          (setq exit t)
          (if (consp (cdr it)) (setq rtn t)))
        (cl-incf i)
        exit)
      rtn)))

(cl-defun entropy/emacs-list-setf-nth (n replace list &key with-end-cdr)
  "Replace `nth' N of `listp' LIST with replacement REPLACE by
altered it i.e. in destructively way, do nothing when N overflow.

If WITH-END-CDR is set, the N's max value can be plus one in
which case it point to the last `atom' cdr of LIST."
  (let ((i 0) exit)
    (unless (< n 0)
      (entropy/emacs-list-map-cdr list
        :with-exit t
        (cond ((= i n)
               (setf (car it) replace
                     exit t))
              ((and with-end-cdr (= (1+ i) n)
                    ;; guarantee it is last cons-cell of LIST i.e. it
                    ;; is the last cons-cell of a dotted list LIST.
                    (atom (cdr it)))
               (setf (cdr it) replace
                     exit t))
              (t
               (cl-incf i)))
        exit))))

(cl-defun entropy/emacs-list-has-same-elements-p (lista listb &key test)
  "Return non-nil if two list is equalization as has same `length'
and same elements test by TEST or use the same test as
`cl-position' defaultly."
  (let ((alen (length lista))
        (blen (length listb)))
    (catch :exit
      (unless (= alen blen)
        (throw :exit nil))
      (dolist (el lista)
        (unless (cl-position el listb :test test)
          (throw :exit nil)))
      (throw :exit t))))

(defun entropy/emacs-list-butfirst (list &optional n)
  "Like `butlast' but return the new copy of LIST who is the
portion of LIST by removed first N elements. Removed the car of
LIST when N is nil or omitted.

If you want to make the return as the same storage as LIST, use
`nthcdr' instead."
  (declare (side-effect-free t))
  (copy-sequence
   (nthcdr (or n 1) list)))

(defun entropy/emacs-sort-list-according-to-list
    (list-to-sort list-base &rest cl-keys)
  "Return a new copy of LIST-TO-SORT with sort it as the same order
of what each elements' order in LIST-BASE.

The based LIST-BASE element's order index 'get' function using
`cl-position', so as the optional keypairs are `apply' to the
[keyword-value]... part of that function. Internally does use
[:test `equal'] defaulty since the two list's element may not be
`eq' even has same structure but we consider that is same.

(fn LIST-TO-SORT LIST-BASE [KEYWORD VALUE]...)"
  (let ((rtn (copy-sequence list-to-sort))
        (default-keys '(:test equal)))
    (setq rtn
          (sort
           rtn
           (lambda (a b)
             (let ((apos (apply 'cl-position
                                a list-base (or cl-keys default-keys)))
                   (bpos (apply 'cl-position
                                b list-base (or cl-keys default-keys))))
               (cond
                ((and apos bpos)
                 (> bpos apos))
                (t
                 nil))))))
    rtn))

;; ***** Map

(defun entropy/emacs-map-list-common (func var)
  "Map a `listp' variable VAR with function FUNC for each element
of VAR.

This function is like `mapc' but also support dotted
list (i.e. not predicted by `proper-list-p').
"
  (unless (listp var)
    (signal 'wrong-type-argument (list 'listp var)))
  (let (item (rest var))
    (while rest
      (setq item (if (listp rest) (car rest) rest)
            rest (if (listp rest) (cdr rest) nil))
      (funcall func item))))

;; **** Combinatorics
(cl-defun __entropy/emacs-gen-list-permutations
    (list-var nested-depths
              &key use-tree
              internal-not-top
              internal-top-list)
  (let ((listlen (length list-var))
        (sublist nil)
        rtn)
    (unless (or
             (= 0 nested-depths)
             (< nested-depths listlen))
      (error "nested-depths %s (i.e elements number %s) is overflow than listlen %s"
             nested-depths (+ 1 nested-depths) listlen))
    (catch :exit
      (when (= nested-depths 0)
        (throw :exit
               (if (and use-tree internal-not-top)
                   (list list-var)
                 list-var)))
      (dolist (topvar list-var)
        (setq sublist (cl-remove topvar list-var))
        (dolist (el (__entropy/emacs-gen-list-permutations
                     sublist (- nested-depths 1)
                     :use-tree use-tree
                     :internal-not-top t
                     :internal-top-list (append internal-top-list
                                                (list topvar))))
          (push (cons
                 topvar
                 (if (listp el)
                     el
                   (list el)))
                rtn)))
      (setq rtn (reverse rtn))
      (if (and use-tree internal-not-top)
          (list rtn)
        rtn))))

(cl-defun entropy/emacs-gen-list-permutations
    (list-var sample-size &key use-tree use-combination)
  "generate the =permutation-collection= of SAMPLE-SIZE of the sample space LIST-VAR.

The =permutation-collectionn= is a list of =permutation-list= whose
length is the SAMPLE-SIZE and each elements is one of the element of
LIST-VAR. The length of =permutation-collectionn= is arithmetic
arrangement of A(LIST-VAR length, SAMPLE-SIZE) i.e.  'a x (a-1) x
(a-2) ... x (a - (SIMPLE-SIZE - 1))'. The return list formed like
below list demo:

**The permutation of sample-space (1 2 3) for smaple-size 3 without tree style**
#+begin_src elisp
'((1 2 3)
  (1 3 2)
  (2 1 3)
  (2 3 1)
  (3 1 2)
  (3 2 1))
#+end_src

Which see, the order is related the origin element order of the
LIST-VAR.

If USE-TREE is non-nil then =permutation-collectionn= is a list
structured using the origin tree style of the permutations from
top to bottom which can be illustrated as below:

**The permutation of sample-space (1 2 3 4) for smaple-size 3 with tree style**

1) the tree map illustrator
#+begin_example
  level 1:            1                  2                  3                  4
                      |                  |                  |                  |
                  _________          _________          _________          _________
                 /    |    \\        /    |    \\        /    |    \\        /    |    \\
  level 2:       2    3    4        1    3    4        1    2    4        1    2    3
                /\\   /\\    /\\      /\\   /\\    /\\      /\\   /\\    /\\      /\\   /\\    /\\
  level 3:     3  4 2  4  2  3    3  4 1  4  1  3    3  4 1  4  2  1    2  3 1  3  1  2
#+end_example

2) its list represention:

#+begin_src elisp
  '((1 . (2 . (3 4))
         (3 . (2 4))
         (4 . (2 3)))
    (2 . (1 . (3 4))
         (3 . (1 4))
         (4 . (1 3)))
    (3 . (1 . (2 4))
         (2 . (1 4))
         (4 . (1 2)))
    (4 . (1 . (2 3))
         (2 . (1 3))
         (3 . (1 2))))
#+end_src

Which see, the order is also related the origin element order of the
LIST-VAR.

When USE-COMBINATION is non-nil, the USE-TREE spec is ignored and
the return is the =permutation-collectionn= without duplicates
i.e. as following arithnmetic permutation's combination term.

NOTE:

Conventionally each elements of LIST-VAR must be unique since the
sample space should not contained the duplicated elements under
arithmetic terminology. But we remove the duplicated elements
internally without modify LIST-VAR, so any case is safe. This
realized that the real used LIST-VAR is always the dups-removed
one so the result length maybe confused while thus.
"
  (let (rtn)
    (when use-combination
      (setq use-tree nil))
    (catch :exit
      (unless (listp list-var)
        (signal 'wrong-type-argument (list 'listp list-var)))
      (unless list-var
        (throw :exit nil))
      (unless (integerp sample-size)
        (signal 'wrong-type-argument (list 'integerp sample-size)))
      (unless (>= sample-size 1)
        (setq sample-size 1))
      (setq rtn
            (__entropy/emacs-gen-list-permutations
             list-var (- sample-size 1) :use-tree use-tree))
      (if use-combination
          (cl-delete-duplicates
           rtn
           ;; guarantee preserved former occurrences
           :from-end t
           :test
           (lambda (a b)
             (if (and (listp a) (listp b))
                 (entropy/emacs-list-has-same-elements-p
                  a b :test 'equal)
               (equal a b))))
        rtn))))

;; **** Numberic list
(defun entropy/emacs-numberic-list (list-var)
  "Return list element mapped with numberic prefix which concated
with '0' as alignment indicator.

For example, if a list formed as '(1 2 3 ... 100)', the returned
list will formed as:

#+BEGIN_SRC elisp
  '((\"001\" . 1)
    (\"002\" . 2)
    (\"003\" . 3)
    ...
    (\"100\" . 100))
#+END_SRC
"
  (let* ((l-len (length list-var))
         (register l-len)
         (counter 0)
         (step 1)
         (zero-func
          (lambda (counter str)
            (let ((step (length str))
                  (rtn str))
              (while (< step counter)
                (setq rtn (concat "0" rtn)
                      step (+ 1 step)))
              rtn)))
         rtn)
    (while (not (eq 0 register))
      (setq register (/ register 10)
            counter (+ 1 counter)))
    (dolist (el list-var)
      (push (cons (funcall zero-func counter (number-to-string step))
                  el)
            rtn)
      (setq step (+ 1 step)))
    (nreverse rtn)))


;; *** Sequence manipulation

(defmacro entropy/emacs-seq-recalc-start-end-common (start end)
  "Re-calculate START and END value of a `sequencep' sequence which
indicate a region of the sequence to a regular range i.e when
both of START and END is non-nil then after the re-calculation
both START and END are set as `>' 0 or `=' 0 when either of them
is `<' 0 , and END and START are swapped from `>=' sort after
calculation.

If START is `null' originally, then just set it as 0 and continue
the normal calculation.

If END is `null' originally, then it is never modified since a
nil END conventionally indicates the end of the sequence i.e. the
`length' of the sequence."
  (let ((start-sym (make-symbol "start"))
        (end-sym   (make-symbol "end")))
    `(let ((,start-sym ,start)
           (,end-sym ,end))
       (if (or (null ,start-sym) (< ,start-sym 0))
           (setf ,start 0 ,start-sym 0))
       (if (and (not (null ,end-sym)) (< ,end-sym 0))
           (setf ,end 0 ,end-sym 0))
       (when ,end-sym
         (entropy/emacs-swap-two-places-value ,start ,end
           (< ,end-sym ,start-sym))))))

(cl-defmacro entropy/emacs-seq-recalc-start-end
    (seq start end &key error-when-overflow with-set-end &allow-other-keys)
  "Like `entropy/emacs-seq-recalc-start-end-common' but also support
negative indicators and optionally support bounding check. Return the
`length' of SEQ. (tip: this macro return this value since it
internally calculate the `length' of SEQ so that the caller no need to
duplcated re-calc the `length' of SEQ for saving compute resource.)

For either START and END is negative, it is re-calculated as the index
from the end position (i.e. the `length') of SEQ negatively moved N
times where N is the `abs' value of it, before whole process.

When either START or END is less than 0, then START is set to 0 or set
length of SEQ for END, unless as below:

When ERROR-WHEN-OVERFLOW is set in which case a bound indices error is
throwed out when either START or END is overflow the `length' of SEQ's
either of its start or end index

Like `entropy/emacs-seq-recalc-start-end-common', the END is always
unmodified when it's nil unless WITH-SET-END is non-nil in which case
END is replaced with the SEQ's end postion i.e. the `length' of SEQ."
  (let ((start-sym (make-symbol "start"))
        (end-sym   (make-symbol "end"))
        (seq-sym   (make-symbol "list"))
        (seq-len-sym (make-symbol "seqlen"))
        (tmpvar-sym  (make-symbol "tmpvar"))
        (err-p-sym   (make-symbol "error-when-overflow")))
    `(let* ((,start-sym ,start)
            (,end-sym   ,end)
            (,seq-sym   ,seq)
            (,seq-len-sym (length ,seq-sym))
            (,err-p-sym ,error-when-overflow)
            (,tmpvar-sym nil))
       (unless (and (null ,start-sym) (null ,end-sym))
         (if (null ,start-sym) (setq ,start-sym 0))
         (if (null ,end-sym)   (setq ,end-sym ,seq-len-sym))
         (dolist (el (list (cons 'start ,start-sym)
                           (cons 'end   ,end-sym)))
           (setq ,tmpvar-sym (cdr el))
           (if (< ,tmpvar-sym 0)
               (setq ,tmpvar-sym (+ ,seq-len-sym ,tmpvar-sym)))
           (if (or (< ,tmpvar-sym 0) (> ,tmpvar-sym ,seq-len-sym))
               (if ,err-p-sym
                   ;; simulation as `cl-subseq's error
                   (error "Bad bounding indices: %s, %s"
                          ,start-sym ,end-sym)
                 (when (< ,tmpvar-sym 0)
                   (cond ((eq 'start (car el))
                          (setq ,start-sym 0))
                         ((eq 'end (car el))
                          (setq ,end-sym ,seq-len-sym)))))
             (cond ((eq 'start (car el))
                    (setq ,start-sym ,tmpvar-sym))
                   ((eq 'end (car el))
                    (setq ,end-sym ,tmpvar-sym))))))
       (entropy/emacs-seq-recalc-start-end-common
        ,start-sym ,end-sym)
       (setf ,start ,start-sym
             ,end   (if (and (not ,with-set-end) (= ,end-sym ,seq-len-sym))
                        nil ,end-sym))
       ;; return
       ,seq-len-sym)))

(cl-defun entropy/emacs-seq-repeat-find
    (item seq count &key find-func find-func-args)
  "Find ITEM in sequence SEQ with COUNT times with finder FIND-FUNC with
its arguments list FIND-FUNC-ARGS, return the final found `nth'
postion of SEQ or nil when not found.

FIND-FUNC should be a `cl-postion' argments list order and terms
compatible function which at least should formed like this:
: (fn elt seq &key start end)
And should return the `nth' index of found ELT in SEQ or nil for not
found. If omitted defaults to `cl-postion'.
"
  (let ((count (or count 1))
        (find-func (or find-func 'cl-position)))
    (catch :exit
      (if (= 0 count) (throw :exit nil))
      (if (= 1 count)
          (throw :exit
                 (apply find-func item seq find-func-args)))
      (let* ((start (plist-get find-func-args :start))
             (end   (plist-get find-func-args :end))
             (from-end (plist-get find-func-args :from-end))
             ;; prevent modified origin arglist
             (args     (copy-sequence find-func-args))
             (arg-repl-func
              (lambda (key value)
                ;; modify the key's value when the key is found in seq
                ;; or append them.
                (let ((mtchp (memq key args)))
                  (if mtchp (setcdr mtchp (cons value (cddr mtchp)))
                    (setq args (nconc args (list key value)))))))
             newpos)
        (entropy/emacs-seq-recalc-start-end
         seq start end :error-when-overflow t :with-set-end t)
        (if (<= end start) (throw :exit nil))
        (while (> count 0)
          (funcall arg-repl-func :start start)
          (funcall arg-repl-func :end end)
          (setq newpos
                (apply find-func item seq args))
          (if newpos
              (if from-end
                  (setq end newpos)
                (setq start (1+ newpos)))
            (throw :exit nil))
          (setq count (1- count))
          (when (> count 0)
            (if (<= end start) (throw :exit nil))))
        newpos))))

;; *** Plist manipulation

(defun entropy/emacs-strict-plistp (var)
  "Return non-nil when VAR is a strict plist.

The strict plist structed as key-value pairs appended list, the
car of it was a key, each key was a `keywordp' symbol. Each key's
value was grouped that say the place of any key's second sibling
must also be a key , thus the *strict* meaning."
  (let (llen llen-half (llp (listp var)))
    (if (not llp) nil
      (if (not (keywordp (car var))) nil
        (if (< (setq llen (length var)) 2) nil
          (if (not (= 0 (setq llen-half (% llen 2)))) nil
            (catch :exit
              (cl-loop for i from 0 to (1- llen-half)
                       unless (keywordp (nth (* i 2) var))
                       do (throw :exit nil))
              t)))))))

(defun entropy/emacs-common-plistp (var)
  "Like `entropy/emacs-strict-plistp' but allow the key's value be
omitted i.e. the next car of the key can also be a key or the end of
list."
  (let ((llp (listp var)))
    (if (not llp) nil
      (if (< (length var) 2) nil
        (if (not (keywordp (car var))) nil
          (let ((tpl var))
            (catch :exit
              (while (consp tpl)
                (unless (keywordp (car tpl)) (throw :exit nil))
                (if (keywordp (cadr tpl))
                    (setq tpl (cdr tpl))
                  (setq tpl (cddr tpl))))
              t)))))))

(defun entropy/emacs-get-plist-form
    (form-plist key &optional type no-error)
  "Like  `plist-get' but for getting the rest form of a key slot.

FORM-PLIST must be an list and no other restriction announced be, but
the key must be an `keywordp' symbol, thus all, any colon prefixed
symbol involved in is treated as an key, so as, if the rest args of an
key whose has an member of colon prefixed symbol will not be got which
must be noticed about.

Do as the same as `plist-get' when TYPE was `eq' to 't' or 'car'.

Return a `progn' form when TYPE was nil or omitted or `eq' to 'progn',
In this case the return form will be nil if the slot's rest forms are
omitted or just presented as a single 'nil', in which case we treat
this key has no rest forms.

Return a list when TYPE was `eq' to 'list'. In this case the return
list will be nil if the slot's rest form are omitted.

If NO-ERROR was non-nil, press all the error asserts, and return
nil. Otherwise when KEY can not be found in FORM-PLIST when TYPE is
not an kind of `plist-get' type described as above, throw out an
error."
  (let* ((key-match-p (memq key form-plist))
         (rest     (cdr key-match-p))
         (is-car   (memq type '(car t)))
         (is-progn (or (null type) (eq type 'progn)))
         (is-list  (eq type 'list))
         rest-car rtn)
    (catch :exit
      (when (null key-match-p)
        (when (or no-error is-car)
          (throw :exit nil))
        (error "Can not match key '%s' in form-plist!" key))
      (while (and (consp rest)
                  (not (keywordp (setq rest-car (car rest)))))
        (push rest-car rtn)
        (setq rest (cdr rest)))
      (setq rtn (nreverse rtn))
      (unless (null rtn)
        (setq rtn
              (cond
               (is-progn
                (if (and (null (car rtn)) (= (length rtn) 1)) nil
                  `(progn ,@rtn)))
               (is-car   (car rtn))
               (is-list `(,@rtn)))))
      rtn)))

(defalias 'entropy/emacs-get-plist-body 'entropy/emacs-defun--get-real-body)

(cl-defun entropy/emacs-plist-setf
    (plist key value &key append-new-key set-use-append set-use-expand)
  "Set key slot of KEY to new value VALUE of a plist PLIST
destructively. Return the altered PLIST or nil when KEY is not found
in PLIST.

KEY can also be a list of keys in which case the set place is chained
via the order of those keys in PLIST's child plist.

When APPEND-NEW-KEY is set, then when KEY is not matched in PLIST or
its child plist, then the KEY is added in the tail of the PLIST or the
corresponding child plist, and then VALUE is set there.

When SET-USE-APPEND is set, then VALUE is set append the KEY's slot's
current value or directly did when KEY's value is empty (i.e. its
sibling is also a key or its sibling is the end of corresponding
plist.)

When SET-USE-EXPAND is set as non-nil, VALUE is expanded to assosiated
place only when VALUE is a `proper-list-p' list.
"
  (unless (consp plist)
    (signal 'wrong-type-argument (list 'consp plist)))
  (unless key (signal 'wrong-type-argument (list 'keywordp key)))
  (let* ((keys (if (listp key) key (list key)))
         (place plist)
         (set-use-expand (and set-use-expand (proper-list-p value)))
         (place-get-func
          (lambda (l k)
            (let (exit fp newval)
              (entropy/emacs-list-map-cdr l
                :with-exit t
                (cond
                 ((eq (car  it) k) (setq place it       exit t))
                 ((eq (cadr it) k) (setq place (cdr it) exit t))
                 ;; append new or refuse nesting
                 ((not (consp (cdr it)))
                  (if (not append-new-key)
                      (and place (setq place nil))
                    (if (not keys)
                        (setq newval nil)
                      (setq keys (nreverse keys))
                      (while keys
                        (setq newval (cons (pop keys) (or newval nil)))
                        (unless fp (setq fp newval))))
                    (setcdr it (cons k newval))
                    (setq place (or fp (cdr it))
                          exit t))))
                exit))))
         (set-func
          (lambda nil
            (let (exit (i 0))
              (entropy/emacs-list-map-cdr place
                :with-exit t
                (if (and (= i 0) (atom (cdr it)))
                    (progn (setcdr it (if set-use-expand value (list value)))
                           (setq exit t))
                  (when (or (atom (cdr it)) (keywordp (cadr it)))
                    (cond
                     (set-use-append
                      (if set-use-expand
                          (dolist (el (reverse value))
                            (setcdr it (cons el (cdr it))))
                        (setcdr it (cons value (cdr it)))))
                     (t
                      (let ((tl (cdr it)))
                        (if set-use-expand
                            (dolist (el (reverse value))
                              (setcdr place (setq tl (cons el tl))))
                          (setcdr place (cons value tl))))))
                    (setq exit t)))
                (cl-incf i)
                exit)))))
    (catch :exit
      (let (cur-key)
        (while (setq cur-key (pop keys))
          (funcall place-get-func place cur-key)
          (unless place (throw :exit nil))
          (if (consp (cadr place))
              (if keys (setq place (cadr place))
                (funcall set-func)
                (throw :exit plist))
            (if keys (throw :exit nil)
              (funcall set-func)
              (throw :exit plist)))))
      (error "internal error"))))

;; *** String manipulation

(defun entropy/emacs-map-string-match-p (str matches)
    "Batch match a list of regexp strings of MATCHES to a
specified string STR.

Each element of MATCHES is a regexp string or a form to build a
regexp string. Internally the match subroutine use
`string-match-p'.

Return t when one of MATCHES matched the target, nil for
otherwise."
    (let ((rtn 0))
      (dolist (el matches)
        (when (string-match-p (rx (regexp (entropy/emacs-eval-with-lexical el))) str)
          (cl-incf rtn)))
      (if (eq rtn 0)
          nil
        t)))

;; *** Arithmetic manupulation
;; **** basic

(cl-defun entropy/emacs-sort-number-seq
    (number-seq &key destructive sort-func)
  "Return a sequence of numbers where all of them are member of
NUMBER-SEQ but `sort' with SORT-FUNC if specified or use `<' as
default method.

The SORT-FUNC has same term as what `sort''s PREDICATE argument
has.

Defaultly the return is a new sequence i.e. the origin NUMBER-SEQ
is not modified unless DESTRUCTIVE is specified and in where use
'(setq foo (entropy/emacs-sort-number-seq foo :destructive t))'
to be sure of correctly changing the value of a sequence
‘foo’.  "
  (let ((sort-func (or sort-func '<)))
    (unless destructive
      (setq number-seq (copy-sequence number-seq)))
    (sort number-seq sort-func)))

(defun entropy/emacs-number-member-in
    (numb numbs-list
          &optional
          ceiling numbs-list-sort-func)
  "Justify whether number NUMB is in a numbers list NUMBS-LIST,
return NUMB if thus.

If NUMB is not in NUMBS-LIST, the return is the number of NUMBS-LIST,
which is the sibling with NUMB, and the *sibling* means as:

1) If CEILING is set, the sibling is the first one of NUMBS-LIST who
   is `>' NUMB, and its previous one is `<' NUMB.

2) Otherwise, the sibling is the first one of NUMBS-LIST who is `<'
   thans NUMB and its next one is `>' NUMB.

The *first one* means obey the list order of NUMBS-LIST i.e. the `car'
sequence. If NUMBS-LIST-SORT-FUNC is set, it should be a function
which used to sort the NUMBS-LIST before query on the
sibling. NUMBS-LIST-SORT-FUNC can be shorted as follow symbol:

1) `<': use `<' to sort which let NUMBS-LIST's car be the minimal one
   and along its cdr.
2) `>': use `>' to sort which let NUMBS-LIST's car be the max one and
   along its cdr.

Any other type is a user specified function which should take the
arguments as the same as `sort' function.

NUMBS-LIST is not side-effects by NUMBS-LIST-SORT-FUNC, since the
NUMBS-LIST will be copied before thus.

If NUMB is out of range of NUMBS-LIST, then the minimal number is
return when NUMB is less than it or the max one when NUMB is
larger than thus.
"
  (when numbs-list-sort-func
    ;; ensure no side-effects to origin list
    (setq numbs-list (copy-sequence numbs-list))
    (cond
     ((eq numbs-list-sort-func '<)
      (sort numbs-list '<))
     ((eq numbs-list-sort-func '>)
      (sort numbs-list '>))
     (t
      (setq numbs-list
            (funcall numbs-list-sort-func
                     numbs-list)))))
  (let (nums-min
        nums-max)
    (cond
     ((member numb numbs-list)
      numb)
     ((< numb (setq nums-min
                    (apply 'min numbs-list)))
      nums-min)
     ((> numb (setq nums-max
                    (apply 'max numbs-list)))
      nums-max)
     (t
      (let ((prev (car numbs-list)))
        (catch :exit
          (dolist (next (cdr numbs-list))
            (if (and (> numb prev)
                     (< numb next))
                (if ceiling
                    (throw :exit next)
                  (throw :exit prev))
              (setq prev next)))
          (error
           "[internal error]: entropy/emacs-number-member-in")))))))

(cl-defun entropy/emacs-natural-number-p
    (var &key exclude-zero detect-float convert-float)
  "TEST whether VAR is an natural number i.e a `natnump' integer or
integer `=' float number if DETECT-FLOAT is enabled.

When EXCLUDE-ZERO is set, also treat VAR should be larger than 0.

Return nil if test failed or non-nil as follow:

1. a number same as VAR.
2. a integer number `=' to VAR when VAR if `floatp' and both
   DETECT-FLOAT and CONVERT-FLOAT are set.
"
  (let ((tmpvar var))
    (catch :exit
      ;; 1st: test whether var is a number object
      (unless (numberp tmpvar)
        (throw :exit nil))
      ;; 2nd: just test whether it `eq' to 0 rather than use `=' when
      ;; `exclude-zero' is enabled since the float part test is in
      ;; follow steps.
      (when exclude-zero
        (when (eq tmpvar 0)
          (throw :exit nil)))
      ;; 3rd: test whether the float `var' is a actual integer.
      (when (and detect-float
                 (floatp tmpvar))
        (unless (= var (setq tmpvar (ceiling var)))
          (throw :exit nil))
        ;; 4th: `var' is a actual integer float num, and test it with
        ;; `exclude-zero'.
        (when (and exclude-zero
                   (= 0 tmpvar))
          (throw :exit nil)))
      ;; final return
      (and
       (natnump tmpvar)
       (if (and convert-float detect-float)
           tmpvar
         var)))))

(cl-defun entropy/emacs-calc-permutations
    (permutation-size
     sample-space-size
     &key
     calc-combination)
  "Calculate and return the permutations number where choosing
PERMUTATION-SIZE in SMAPLE-SPACE-SIZE. Return nil while
calculation is overflow.

When CALC-COMBINATION is non-nil, return a `cons' of car of
permutations number and cdr of combinations number instead while
not overflow."
  (let ((cnt 0)
        (rtn 1)
        (cmb 1)
        overflow-p)
    (catch :exit
      (cond
       ((not (entropy/emacs-natural-number-p
              permutation-size
              :detect-float t
              :exclude-zero t))
        (error "permutation-size %s is not a natnum" permutation-size))
       ((not (entropy/emacs-natural-number-p
              sample-space-size
              :detect-float t
              :exclude-zero t))
        (error "sample-space-size %s is not a natnum"
               sample-space-size))
       ((not (<= permutation-size sample-space-size))
        (error "permutation-size %d is larger than sample-space-size %d"
               permutation-size sample-space-size)))
      (condition-case err
          (progn
            (while (< cnt permutation-size)
              (setq rtn
                    (* rtn (- sample-space-size cnt)))
              (cl-incf cnt))
            (when calc-combination
              (setq cnt 0)
              (while (< cnt permutation-size)
                (setq cmb
                      (* cmb (- permutation-size cnt)))
                (cl-incf cnt))))
        (error
         (setq overflow-p err)))
      (unless overflow-p
        (if calc-combination
            (cons rtn (/ rtn cmb))
          rtn)))))

;; **** coordinates system
;; ***** xy coordinates system

(defun entropy/emacs-xycrd-make-coordinate-obj (x y)
  "Create a =EEMACS-XYCRD-POINT-OBJ= i.e. a cons of two number X
and Y which as meaning of the extending on two dimensions with
zero base."
  (unless (numberp x)
    (error "=EEMACS-XYCRD-POINT-OBJ= maker: X '%d' is invalid which
should be number." x))
  (unless (numberp y)
    (error "=EEMACS-XYCRD-POINT-OBJ= maker: y '%d' is invalid which
should be number." y))
  (cons x y))

(defun entropy/emacs-xycrd-make-rectangle-obj
    (begin-crd end-crd)
  "Get a =EEMACS-XYCRD-RECTANGLE-OBJ= according to two
=EEMACS-XYCRD-POINT-OBJ= i.e. BEGIN-CRD and END-CRD.

A =EEMACS-XYCRD-RECTANGLE-OBJ= is a plist to describe the rectangle
part of a region. With follow keys returned as described below:

1) =:begin-coordinate=: a =EEMACS-XYCRD-POINT-OBJ= which user
   specified as *begin* coordinate.
2) =:begin-coordinate=: a =EEMACS-XYCRD-POINT-OBJ= which user
   specified as *end* coordinate.
3) =:begin-negative-coordinate= : a =EEMACS-XYCRD-POINT-OBJ= negative
   to =:begin-coordinate= which in same X coordinate as
   =:begin-coordinate=, used with =:begin-coordinate= to consist a
   rectangle top or bottom border.
4) =:end-negative-coordinate= : a =EEMACS-XYCRD-POINT-OBJ= negative to
   =:end-coordinate= which in same X coordinate as =:end-coordinate=,
   used with =:end-coordinate= to consist a rectangle top or bottom
   border.
5) =:coordinate-overlap-p=: non-nil when =:begin-coordinate= and
   =:end-coordinate= are same, in which case we can ensure there's no
   region rectangle, in other word the region is a point zero-base
   two-dimension coordinates system.
6) =:rectangle-columns-amount=: how many buffer columns the rectangle
   covered to. (NOTE: no meaning for non `natnump' coordinated
   rectangle)
7) =:rectangle-rows-amount=: how many buffer rows the rectangle
   covered to. (NOTE: no meaning for non `natnump' coordinated
   rectangle)
8) =:vertical-negative-p=: non-nil when =:begin-coordinate='s X is
   less than =:end-coordinate='s X.
9) =:horizontal-negative-p=: non-nil when =:begin-coordinate='s Y is
   larger than =:end-coordinate='s Y.
10) =:full-negative-p=: both =:vertical-negative-p= and
    =:horizontal-negative-p= are non-nil.
11) =:coordinate-plist=: a plist enums four corner coordinates of the
    rectangle i.e. =:top-left=, =:top-right=, =:bottom-left= and
    =:bottom-right=.
"
  (let (crd-eq-p
        (beg-crd begin-crd)   beg-crd-x  beg-crd-y
        (end-crd end-crd)     end-crd-x  end-crd-y
        beg-ncrd  beg-ncrd-x beg-ncrd-y
        end-ncrd  end-ncrd-x end-ncrd-y
        crd-plist
        w-crdlen  h-crdlen
        chamt-x   chamt-y
        vertical-negative-p horizontal-negative-p full-negative-p)
    (setq crd-eq-p   (equal beg-crd end-crd)

          beg-crd-x  (car beg-crd)  beg-crd-y (cdr beg-crd)
          end-crd-x  (car end-crd)  end-crd-y (cdr end-crd)

          beg-ncrd-x end-crd-x     beg-ncrd-y beg-crd-y
          end-ncrd-x beg-crd-x     end-ncrd-y end-crd-y

          beg-ncrd   (entropy/emacs-xycrd-make-coordinate-obj beg-ncrd-x beg-ncrd-y)
          end-ncrd   (entropy/emacs-xycrd-make-coordinate-obj end-ncrd-x end-ncrd-y)

          w-crdlen   (abs (- beg-crd-x end-crd-x))
          h-crdlen   (abs (- beg-crd-y end-crd-y))
          chamt-x    w-crdlen
          chamt-y
          (cond
           (crd-eq-p 0)
           ((= w-crdlen 0) 0)
           (t
            (1+ h-crdlen)))
          vertical-negative-p      (> beg-crd-y end-crd-y)
          horizontal-negative-p    (> beg-crd-x beg-ncrd-x)
          full-negative-p (and vertical-negative-p horizontal-negative-p)

          crd-plist
          (list :top-left      (cond (full-negative-p       end-crd)
                                     (horizontal-negative-p beg-ncrd)
                                     (vertical-negative-p   end-ncrd)
                                     (t                     beg-crd))

                :top-right     (cond (full-negative-p       end-ncrd)
                                     (horizontal-negative-p beg-crd)
                                     (vertical-negative-p   end-crd)
                                     (t                     beg-ncrd))

                :bottom-left   (cond (full-negative-p       beg-ncrd)
                                     (horizontal-negative-p end-crd)
                                     (vertical-negative-p   beg-crd)
                                     (t                     end-ncrd))

                :bottom-right  (cond (full-negative-p       beg-crd)
                                     (horizontal-negative-p end-ncrd)
                                     (vertical-negative-p   beg-ncrd)
                                     (t                     end-crd))))
    (list
     :begin-coordinate           beg-crd    :end-coordinate          end-crd
     :begin-negative-coordinate  beg-ncrd   :end-negative-coordinate end-ncrd
     :coordinate-overlap-p       crd-eq-p
     :rectangle-columns-amount   chamt-x
     :rectangle-rows-amount      chamt-y
     :vertical-negative-p        vertical-negative-p
     :horizontal-negative-p      horizontal-negative-p
     :full-negative-p            full-negative-p
     :coordinate-plist           crd-plist)))

(defvar entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-objnames
  '("a" "b"))
(defvar entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dimnames
  '("x" "y"))
(defvar entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dotnames
  '("tpl" "tpr" "btl" "btr"))
(cl-defun entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
    (objname &key
             dotname use-collection
             dimension-name)
  "Gen `let' binding var `intern'ed symbol obey rules defined by
`entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper'.

OBJNAME is a element of `entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-objnames'.

DOTNAME is a element of `entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dotnames'.

DIMENSION-NAME is a element of
`entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dimnames'.

When USE-COLLECTION, DOTNAME and DIMENSION-NAME will be ignored
and generate a symbol which describe a ':coordinate-plist' of the
OBJ.

Error when any invalid name are specified.
"
  (unless (member objname entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-objnames)
    (error "objname invalid: %s" objname))
  (when dotname
    (unless (member dotname entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dotnames)
      (error "dotname invalid: %s" dotname)))
  (when dimension-name
    (unless (member dimension-name entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym-valid-dimnames)
      (error "dimname invalid: %s" dimension-name)))
  (let (name
        (dotname-prefix "dot"))
    (cond (use-collection
           (setq name (format "%s-%ss" objname dotname-prefix))
           (setq dotname nil
                 dimension-name nil))
          ((and objname dotname)
           (setq name
                 (format "%s-%s-%s"
                         objname dotname-prefix dotname))
           (when dimension-name
             (setq name (format "%s-%s" name dimension-name)))))
    (or name (error "args not specified full"))
    (intern name)))

(defun entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper
    (a b)
  "Generate `let*' binding for list of symbols which deconstructs
the two =EEMACS-XYCRD-RECTANGLE-OBJ=.

The deconstruction is based on the order of object name,
rectangle corner dot name and the dimension name which as:

#+begin_example
<OBJECT-A>-<TOP-LEFT-DOT>-<X>
#+end_example

Unlike the example's full text naming as, this function use two simple
name char \"a\" and \"b\" to bind with object A and B. The
abbreviation is so as on the four dot cornerS'
=EEMACS-XYCRD-POINT-OBJ= of the rectangle:

- top left corner dot: \"dot-tpl\"
- top right corner dot: \"dot-tpr\"
- bottom left corner dot: \"dot-btl\"
- bottom right corner dot: \"dot-btr\"

And dimensions value as for \"x\", \"y\".

That say if we want to use a `let*' bind name, usually we need
remember a three nested naming procedure loop:

#+begin_example
  objname                   : (a b) ->
  dot corner name           :  (dot-tpl dot-tpr dot-btl dot-btr) ->
  dimenssion name (optional):   (x y)
#+end_example

Additionally there're also a 'a-dots' and 'b-dots' binds to bind with
the ':coordinate-plist' part of the =EEMACS-XYCRD-RECTANGLE-OBJ=
individually for object A and B.
"
  (let ((objnm-rel-alist `(("a" . ,a) ("b" . ,b)))
        (dotnm-keyrel-alist '(("tpl" . :top-left)
                              ("tpr" . :top-right)
                              ("btl" . :bottom-left)
                              ("btr" . :bottom-right)))
        (dimnms '(("x" . car) ("y" . cdr)))
        letbinds-list)
    (dolist (objnm-rel objnm-rel-alist)
      (let* ((objnm (car objnm-rel))
             (var (cdr objnm-rel))
             (obj-crdsym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                          objnm :use-collection t)))
        (push `(,obj-crdsym (plist-get ,var :coordinate-plist)) letbinds-list)
        (dolist (dotnm-keyrel dotnm-keyrel-alist)
          (let* ((dotnm (car dotnm-keyrel))
                 (dotkey (cdr dotnm-keyrel))
                 (obj-crd-posym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                                 objnm :dotname dotnm)))
            (push `(,obj-crd-posym (plist-get ,obj-crdsym ,dotkey))
                  letbinds-list)
            (dolist (dim dimnms)
              (let* ((dimnm (car dim))
                     (dimfunc (cdr dim))
                     (obj-crd-pos-dimsym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                                          objnm :dotname dotnm :dimension-name dimnm)))
                (push `(,obj-crd-pos-dimsym (,dimfunc ,obj-crd-posym))
                      letbinds-list)))))))
    (nreverse letbinds-list)))

(defvar entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym-valid-vhnames
  '("rect-horizontal=" "rect-vertical=" "rect-all="))
(defun entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym (vh-name)
  "Gen `let*' binding var `intern'ed symbbol ovey rules defined by
`entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-wrapper'.

VH-NAME is a element of
`entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym-valid-vhnames'.

Error when any invalid name are specified.
"
  (unless (member vh-name entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym-valid-vhnames)
    (error "invalid vh-name: %s" vh-name))
  (intern vh-name))
(defun entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-wrapper ()
  "Like and based on
`entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper', generate list of
`let*' bindings used to compare the =EEMACS-XYCRD-RECTANGLE-OBJ= A
based on B whether vertical or horizontal fully alignment.

Bind 'rect-vertical=' indicates A's top and bottom borders' left
and right dot corners' X dimension is same as B.

Bind 'rect-horizontal=' indicates A's left and right borders' top
and bottom dot corners' Y dimension is same as B.

Bind 'rect-all=' indicates A and B is fully overlapped with each
other.
"
  (let ((dim-rels '(("rect-horizontal=" . ("y" "tpl" "btl"))
                    ("rect-vertical="   . ("x" "tpl" "tpr"))))
        rtn eq-rtn)
    (dolist (dimrel dim-rels)
      (let ((opsym (intern (car dimrel)))
            (dim (cadr dimrel))
            (dotnms (cddr dimrel))
            pattern)
        (push opsym eq-rtn)
        (dolist (dotnm dotnms)
          (push `(= ,(entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                      "a" :dotname dotnm :dimension-name dim)
                    ,(entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                      "b" :dotname dotnm :dimension-name dim))
                pattern))
        (push `(,opsym (and ,@(nreverse pattern))) rtn)))
    `(,@(nreverse rtn)
      (,(intern "rect-all=") (and ,@(nreverse eq-rtn))))))

(defvar entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dimnames
  '("x" "y"))
(defvar entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dotnames
  '("tpl" "tpr" "btl" "btr"))
(defvar entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-brdnames
  '("left" "right" "top" "bottom"))
(defvar entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-cmpsyms
  '(= < > == <&= >&=))
(cl-defun entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
    (typename
     &key
     use-border
     use-negative
     compare-sym
     dimension-name)
  "Gen `let' binding var `intern'ed symbol obey rules defined by
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-wrapper'.

TYPENAME is a element of
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dotnames'
or
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-brdnames'
when USE-BORDER is non-nil.

DIMENSION-NAME is a element of
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dimnames'.

COMPARE-SYM is element of
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-cmpsyms'.

When USE-NEGATIVE is set, the generated symbol is as what
`entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-wrapper' got for
negative terms.

Error when any invalid name are specified.
"
  (let ((typelist (if use-border
                      entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-brdnames
                    entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dotnames))
        (dot-name-prefix    "dot")
        (border-name-prefix "brd")
        name)
    (unless (member typename typelist)
      (error "invalid typename: %s" typename))
    (when dimension-name
      (unless (member dimension-name entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-dimnames)
        (error "dimname invalid: %s" dimension-name)))
    (when compare-sym
      (unless (member compare-sym entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym-valid-cmpsyms)
        (error "compare func invalid: %s" compare-sym)))
    (when (and use-border dimension-name)
      (error "can not use-border with dimension-name"))
    (setq name (format "%s-%s" (if use-border border-name-prefix dot-name-prefix)
                       typename))
    (when dimension-name (setq name (format "%s-%s" name dimension-name)))
    (when compare-sym (setq name (format "%s%s" name (symbol-name compare-sym))))
    (when use-negative (setq name (concat "!" name)))
    (intern name)))

(defun entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-wrapper ()
  "Like and based on `entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper'
and `entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-wrapper', generate
list of `let*' bindings used to compare the
=EEMACS-XYCRD-RECTANGLE-OBJ= A based on B for various calculations.

The naming loops are:

1) The compare type name, one of \"dot\" for dot corner and \"brd\" for
   border.
2) The type sub names i.e.
   - for \"dot\" type: \"tpl\" \"tpr\" \"btl\" \"btr\" which is same termed as
     the parent wrapper function.
   - for \"brd\" type:
     * \"left\":   for left border which has X against dimension comparing.
     * \"right\":  for right border which has X against dimension comparing.
     * \"top\":    for top border which has Y against dimension comparing.
     * \"bottom\": for bottom border which has Y against dimension comparing.
3) Subtype for \"dot\" type, the dimension name \"x\" and \"y\" which is
   same termed as the parent wrapper function.
4) The compare type name:
   * '=': abstraction of equalization for all types even be with their
     subtypes which see:
     - when did for a \"dot\" type, return non-nil when they are the
       same coordinate. If do with the dimension subtype of the \"dot\",
       return non-nil when their corresponding same dimension value is
       `='.
     - when did for a \"brd\" type, return non-nil when the two border
       has the same against dimension comparing value e.g. (symole
       'brd-left=' indicate that A's left border and B's left border
       is in the same X dimension, so as 'brd-top=' is Y same for
       as.).
   * '<' or '>': abstraction of lesser or larger for all types even be
     with their subtypes which see:
     - when did for a \"dot\" type with its subtype, return non-nil if
       the corresponding dot corners' same dimension value is lesser
       or larger.
     - when did for a \"brd\" type, return non-nil when corresponding
       against dimension comparing is judged.
   * '&=': a subcompare type let each main compare type deliverred to
     '<&=', '>&=', '==' and just used for \"brd\" type. Note for type
     '==' is not has a same print style as others since this style is
     more significant than the rigid one.
     - '<&='or '>&=': return non-nil when the corresponding border
       aganst dimension comparing like '<' and '>' and the borders
       dot-corners' such dimension is same e.g. 'brd-left<&=' says
       that A's left border is X lesser than B's and 'dot-tpl-x=' is
       non-nil.
     - '==': return non-nil when the corresponding borders is
       coordinates same.


For each comparing above, their also has a negative comparing variant,
and named prefix with a '!' char as significantly represents the
*negative* term. The negative comparing is rules as follow:
1. for \"dot\" types:
   - A's \"tpl\" compares against B's \"tpr\"
   - A's \"tpr\" compares against B's \"tpl\"
   - A's \"btl\" compares against B's \"btr\"
   - A's \"btr\" compares against B's \"btl\"
2. for \"brd\" types:
   - A's \"left\" compares against B's \"right\"
   - A's \"right\" compares against B's \"left\"
   - A's \"top\" compares against B's \"bottom\"
   - A's \"bottom\" compares against B's \"top\"

And in negative comparing, '&=' variants' against dimension is not
changed.
"
  (let* ((objnames '("a" . "b"))
         (dimes-cons '("x" . "y"))
         (dimes (list (car dimes-cons) (cdr dimes-cons)))
         (tpl-dotnm "tpl")
         (tpr-dotnm "tpr")
         (btl-dotnm "btl")
         (btr-dotnm "btr")
         (dotnm-against-alist
          `((,tpl-dotnm . ,tpr-dotnm)
            (,tpr-dotnm . ,tpl-dotnm)
            (,btl-dotnm . ,btr-dotnm)
            (,btr-dotnm . ,btl-dotnm)))
         (lrbrd-align-v-sym (entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym "rect-vertical="))
         (tbbrd-align-h-sym (entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym "rect-horizontal="))
         ;; NOTE: key ':dotnms''s list is order required from top-to-bottom or left-to-right
         (drec-dots `(("left"     :main ,(car dimes-cons)
                       :mate ,(cdr dimes-cons) :dotnms (,tpl-dotnm ,btl-dotnm)
                       :vh-align-sym ,lrbrd-align-v-sym)
                      ("right"    :main ,(car dimes-cons)
                       :mate ,(cdr dimes-cons) :dotnms (,tpr-dotnm ,btr-dotnm)
                       :vh-align-sym ,lrbrd-align-v-sym)
                      ("top"      :main ,(cdr dimes-cons)
                       :mate ,(car dimes-cons) :dotnms (,tpl-dotnm ,tpr-dotnm)
                       :vh-align-sym ,tbbrd-align-h-sym)
                      ("bottom"   :main ,(cdr dimes-cons)
                       :mate ,(car dimes-cons) :dotnms (,btl-dotnm ,btr-dotnm)
                       :vh-align-sym ,tbbrd-align-h-sym)))
         ;; NOTE: strongly use conventionally order is tpl-tpr-btl-btr to let context clear
         (dotnames `(,tpl-dotnm ,tpr-dotnm ,btl-dotnm ,btr-dotnm))
         (align-brdnames (mapcar 'car drec-dots))

         (cmpsyms '(= < >))

         (=align-symgen-func
          (lambda (align-brdnm cmpsym &optional negative)
            (let ((cmpname (symbol-name cmpsym)))
              (if (eq cmpsym '=)
                  (setq cmpname (concat cmpname "="))
                (setq cmpname (concat cmpname "&=")))
              (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
               align-brdnm :use-border t :compare-sym (intern cmpname)
               :use-negative negative))))

         (nalign-pattern-gen-func
          (lambda (align-brdnm cmpsym &optional negative)
            (let* ((opsym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                           align-brdnm :use-border t :compare-sym cmpsym :use-negative negative))
                   (align-dots-flags
                    (alist-get align-brdnm drec-dots))
                   (pair-dots (plist-get align-dots-flags :dotnms))
                   (dotnm (car pair-dots))
                   (main-dim (plist-get align-dots-flags :main)))
              (let ((oldsym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                             dotnm :dimension-name main-dim :compare-sym cmpsym :use-negative negative)))
                `(,opsym ,oldsym)))))

         (=align-pattern-gen-func
          (lambda (align-brdnm cmpsym &optional negative)
            (let* ((opsym (funcall =align-symgen-func align-brdnm cmpsym negative))
                   (nopsym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                            align-brdnm :use-border t :compare-sym cmpsym :use-negative negative))
                   (align-dots-flags
                    (alist-get align-brdnm drec-dots))
                   ;; (pair-dots (plist-get align-dots-flags :dotnms))
                   ;; (mate-dim  (plist-get align-dots-flags :mate))
                   ;; mate-calc-form
                   (vh-alsym (plist-get align-dots-flags :vh-align-sym)))
              ;; NOTE: if no
              ;; `entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-gen-sym'
              ;; we can use below loops to generate the sub pattern
              ;; but they are duplicats accross bindings so will cost
              ;; extra non-needed CPU period.
              ;;
              ;; (dolist (dotnm pair-dots)
              ;;   (let* ((mate-sym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
              ;;                     dotnm :dimension-name mate-dim :compare-sym '=
              ;;                     ;; always disable negative since no need
              ;;                     :use-negative nil)))
              ;;     (push mate-sym mate-calc-form)))
              ;; `(,opsym (and ,nopsym ,@(nreverse mate-calc-form)))
              `(,opsym (and ,nopsym ,vh-alsym))
              )))
         ;; final return
         rtn)
    ;; positive
    (dolist (dotnm dotnames)
      (dolist (dim dimes)
        (dolist (cmp cmpsyms)
          (let* ((opsym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                         dotnm :dimension-name dim
                         :compare-sym cmp))
                 (asym  (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (car objnames)
                         :dotname dotnm :dimension-name dim))
                 (bsym  (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (cdr objnames)
                         :dotname dotnm :dimension-name dim)))
            (push `(,opsym (,cmp ,asym ,bsym)) rtn)))))
    (dolist (dotnm dotnames)
      (let* ((opsym    (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                        dotnm :compare-sym '=))
             (adots-sym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (car objnames) :dotname dotnm))
             (bdots-sym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (cdr objnames) :dotname dotnm)))
        (push `(,opsym (equal ,adots-sym ,bdots-sym)) rtn)))
    (dolist (brdnm align-brdnames)
      (dolist (cps cmpsyms)
        (push (funcall nalign-pattern-gen-func brdnm cps) rtn)))
    (dolist (brdnm align-brdnames)
      (dolist (cps cmpsyms)
        (push (funcall =align-pattern-gen-func brdnm cps) rtn)))

    ;; negative
    (dolist (dotnm dotnames)
      (dolist (dim dimes)
        (dolist (cmp cmpsyms)
          (let* ((opsym (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                         dotnm :dimension-name dim :use-negative t
                         :compare-sym cmp))
                 (asym  (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (car objnames)
                         :dotname dotnm :dimension-name dim))
                 (bsym  (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (cdr objnames)
                         :dotname (alist-get dotnm dotnm-against-alist) :dimension-name dim)))
            (push `(,opsym (,cmp ,asym ,bsym)) rtn)))))
    (dolist (dotnm dotnames)
      (let* ((opsym     (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-gen-sym
                         dotnm :compare-sym '= :use-negative t))
             (adots-sym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (car objnames) :dotname dotnm))
             (bdots-sym (entropy/emacs-xycrd-rectangle-cmp-letbinds-gen-sym
                         (cdr objnames) :dotname (alist-get dotnm dotnm-against-alist))))
        (push `(,opsym (equal ,adots-sym ,bdots-sym)) rtn)))
    (dolist (brdnm align-brdnames)
      (dolist (cps cmpsyms)
        (push (funcall nalign-pattern-gen-func brdnm cps 'neg) rtn)))
    (dolist (brdnm align-brdnames)
      (dolist (cps cmpsyms)
        (push (funcall =align-pattern-gen-func brdnm cps 'neg) rtn)))
    ;; rtn
    `(,@(nreverse rtn))))

(cl-defmacro with-eemacs-xycrd-rectangle-letbinds-wrapper
    (a b &rest body
       &key
       use-vh
       use-calc
       &allow-other-keys)
  "Macro run BODY with `entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper'.

If USE-VN is non-nil also use
`entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-wrapper'.

If USE-CALC is non-nil then USE-VH is used forcely without check
whether it is set or not, and
`entropy/emacs-xycrd-rectangle-calc-cmp-letbinds-wrapper' is
appended."
  (declare (indent defun))
  (let ((base-letbinds (entropy/emacs-xycrd-rectangle-cmp-letbinds-wrapper a b))
        (vh-letbinds (entropy/emacs-xycrd-rectangle-cmp-vh-letbinds-wrapper))
        (calc-letbinds (entropy/emacs-xycrd-rectangle-cmp-calc-letbinds-wrapper))
        (body (entropy/emacs-get-plist-body body)))
    `(let* (,@base-letbinds ,@(when use-vh vh-letbinds) ,@(when use-calc calc-letbinds))
       ,@body)))

;; *** File and directory manipulation

(defun entropy/emacs-filesystem-node-name-legal-p (filesystem-node-name-maybe)
  "Return non-nil when a user spec filesystem node name
FILESYSTEM-NODE-NAME-MAYBE is legal in current platform."
  (let ((absname (expand-file-name filesystem-node-name-maybe)))
    (file-name-absolute-p absname)))

(defun entropy/emacs-filesystem-node-exists-p (file-or-dir-name &optional file-attributes)
  "Like `file-exists-p' but apply all FILE-OR-DIR-NAME's file system node
type e.g. a broken symbolink is also treat as existed.

Return t or nil for the status.

If optional argument FILE-ATTRIBUTES is non-nil, return FILE-OR-DIR-NAME's
file attributes predicated by `file-attributes' after the existed
status checking, so return nil when file not exists as well."
  (let ((fattrs (ignore-errors (file-attributes file-or-dir-name))))
    (if file-attributes
        fattrs
      (and fattrs
           t))))

(defun entropy/emacs-get-filesystem-node-attributes (filesystem-node)
  "Like `file-attributes' but return a plist to represent its
structure so that its more human readable and easy to get its
sub-attribute.

FILESYSTEM-NODE must be existed (predicated by
`entropy/emacs-filesystem-node-exists-p') or will throw an error.

Plist keys:

- =:type=               : returned by `file-attribute-type'
- =:device-number=      : returned by `file-attribute-device-number'
- =:user-id=            : returned by `file-attribute-user-id'
- =:modification-time=  : returned by `file-attribute-modification-time'
- =:size=               : returned by `file-attribute-size'
- =:inode-number=       : returned by `file-attribute-inode-number'
- =:group-id=           : returned by `file-attribute-group-id'
- =:link-number=        : returned by `file-attribute-link-number'
- =:status-change-time= : returned by `file-attribute-status-change-time'
- =:access-time=        : returned by `file-attribute-access-time'
"
  (let ((fattrs (or (entropy/emacs-filesystem-node-exists-p filesystem-node t)
                    (user-error "[entropy/emacs-get-filesystem-node-attributes] file-not-existed: <%s>"
                                filesystem-node))))
    (if fattrs
        (list
         :type (file-attribute-type fattrs)
         :device-number (file-attribute-device-number fattrs)
         :user-id (file-attribute-user-id fattrs)
         :modification-time (file-attribute-modification-time fattrs)
         :size (file-attribute-size fattrs)
         :inode-number (file-attribute-inode-number fattrs)
         :group-id (file-attribute-group-id fattrs)
         :link-number (file-attribute-link-number fattrs)
         :status-change-time (file-attribute-status-change-time fattrs)
         :access-time (file-attribute-access-time fattrs))
      (error "[entropy/emacs-get-filesystem-node-attributes]: internal error"))))

(defun entropy/emacs-filesystem-node-is-regular-file-p
    (filesystem-node &optional symlink-also force-use-filename)
  "Return non-nil when FILESYSTEM-NODE whether is a regular file.

If optional argument SYMLINK-ALSO is non-nil, then a symbolink
(whether broken or not) is treat as a regular file also.

If optional argument FORCE-USE-FILENAME is non-nil, then use
`entropy/emacs-directory-file-name' to deal the name of
FILESYSTEM-NODE firstly."
  (when force-use-filename
    (setq filesystem-node
          (entropy/emacs-directory-file-name filesystem-node)))
  (let ((fsn-attrs (ignore-errors
                     (entropy/emacs-get-filesystem-node-attributes
                      filesystem-node)))
        fsn-type)
    (cond
     ((not fsn-attrs)
      nil)
     (t
      (setq fsn-type (plist-get fsn-attrs :type))
      (cond
       (symlink-also
        (or (null fsn-type) (stringp fsn-type)))
       (t
        (null fsn-type)))))))

(defun entropy/emacs-filesystem-node-is-symlink-p
    (filesystem-node &optional force-use-filename)
  "Return non-nil when FILESYSTEM-NODE is a symbolic link (whether
broken or existed).

If optional argument FORCE-USE-FILENAME is non-nil, then use
`entropy/emacs-directory-file-name' to deal the name of
FILESYSTEM-NODE firstly."
  (when force-use-filename
    (setq filesystem-node
          (entropy/emacs-directory-file-name filesystem-node)))
  (let ((fsn-attrs (ignore-errors
                     (entropy/emacs-get-filesystem-node-attributes
                      filesystem-node)))
        fsn-type)
    (cond
     ((not fsn-attrs)
      nil)
     (t
      (setq fsn-type (plist-get fsn-attrs :type))
      (stringp fsn-type)))))

(defun entropy/emacs-filesystem-node-is-regular-directory-p
    (filesystem-node &optional symlink-also force-use-filename)
  "Return non-nil when FILESYSTEM-NODE whether is a regular directory.

If optional argument SYMLINK-ALSO is non-nil, then when
FILESYSTEM-NODE is a symbolink (justify by
`entropy/emacs-filesystem-node-is-symlink-p' with force filename
deals) links from a existed directory treat as a regular
directory also. In this case, the link chase using
`file-truename'.

If optional argument FORCE-USE-FILENAME is non-nil, then use
`entropy/emacs-directory-file-name' to deal the name of
FILESYSTEM-NODE firstly."
  (when force-use-filename
    (setq filesystem-node
          (entropy/emacs-directory-file-name filesystem-node)))
  (let ((fsn-attrs (ignore-errors
                     (entropy/emacs-get-filesystem-node-attributes
                      filesystem-node)))
        fsn-type)
    (cond
     ((not fsn-attrs)
      nil)
     (t
      (setq fsn-type (plist-get fsn-attrs :type))
      (cond
       (symlink-also
        (if (entropy/emacs-filesystem-node-is-symlink-p
             filesystem-node 'use-fname)
            (entropy/emacs-filesystem-node-is-regular-directory-p
             (file-truename filesystem-node) nil 'use-fname)
          (eq t fsn-type)))
       (t
        (eq t fsn-type)))))))

(defun entropy/emacs-filesystem-nodes-in-same-filesystem-p (&rest filesystem-nodes)
  "Judge all file of FILESYSTEM-NODES are in the same filesystem, return t if
thus, nil otherwise.

Filesystem-Nodes must be existed (predicated by
`entropy/emacs-filesystem-node-exists-p') or will throw an error for any one
who is not existed.

Always return t when filesystem-nodes just has one file and its existed.

Be aware that the name of file or directory should be indicated
significantly since an symbolic to an another filesystem is also
an directory but its file name is an file hosted in the current
filesystem in which case you should use `file-name-as-directory'
to quote it when you treat it as an directory."
  (let (dev-ids
        remote-files
        indc)
    (catch :exit
      (dolist (f filesystem-nodes)
        (unless (entropy/emacs-filesystem-node-exists-p f)
          (user-error "[entropy/emacs-filesystem-nodes-in-same-filesystem-p]: '%s' not existed!"
                      f))
        (when (file-remote-p f)
          (push f remote-files))
        (push (file-attribute-device-number
               (file-attributes f))
              dev-ids))
      (when remote-files
        (unless (= (length filesystem-nodes) remote-files)
          (throw :exit nil)))
      (setq indc (car dev-ids))
      (unless (integerp indc)
        (error "[entropy/emacs-filesystem-nodes-in-same-filesystem-p]: internal error"))
      (mapc (lambda (x)
              (unless (= x indc)
                (throw :exit nil)))
            dev-ids)
      t)))

(defun entropy/emacs-directory-file-name (file-or-directory)
  "like `directory-file-name' but checking its type by
`directory-name-p' firstly so that both file and directory name
is supported that return the origin FILE-OR-DIRECTORY when it's
not an directory name."
  (if (directory-name-p file-or-directory)
      (directory-file-name file-or-directory)
    file-or-directory))

(defun entropy/emacs-make-relative-filename
    (file dir &optional as-list)
  "Convert FILE to a name relative to DIR.  If DIR is omitted or
nil, it defaults to `default-directory'.  If FILE is not in the
directory tree of DIR, return nil.

FILE and DIR are all expanded as file and directory before
calculating their relative relationship.

The returned rel-filename is *not* leading or tail with the
system filepath separator i.e. in Windows '\\' and in *nix system
is '/'. (i.e. are all filenames)

If optional arg AS-LIST is non-nil, return a relative path list whose
each element is the *node-name*(i.e. file name) of the relative path
string returned.

If FILE and DIR are equalized by expanded by
`entropy/emacs-directory-file-name' than the common return value is string
\".\" and the AS-LIST return type is '(\".\")'

*Always return nil of file and dir are not relatived.*
"
  (or dir (setq dir default-directory))
  (setq file (entropy/emacs-directory-file-name (expand-file-name file)))
  (setq dir (file-name-as-directory
             (entropy/emacs-directory-file-name (expand-file-name dir))))
  (let ((rtn
         (if (string-match (concat "^" (regexp-quote dir)) file)
             (substring file (match-end 0))
           nil)))
    (unless rtn
      (when (string= (entropy/emacs-directory-file-name file)
                     (entropy/emacs-directory-file-name dir))
        (setq rtn ".")))
    (when rtn
      (when as-list
        (if (string= rtn ".")
            (setq rtn (list rtn))
          (let ((cursubname (file-name-nondirectory rtn))
                (curparename (file-name-directory rtn))
                tmpvar itervar)
            (if curparename
                (progn
                  (while (or curparename (not (string-empty-p (or cursubname ""))))
                    (push cursubname tmpvar)
                    (setq itervar (and curparename (entropy/emacs-directory-file-name curparename))
                          cursubname (and itervar (file-name-nondirectory itervar))
                          curparename (and itervar (file-name-directory itervar))))
                  (setq rtn tmpvar))
              (setq rtn (list cursubname)))))))
    rtn))

(defun entropy/emacs-batch-expand-file-name
    (names &optional base-dir)
  "`expand-file-name' batch mode for list of name NAMES, based on
BASE-DIR when non-nil or `default-directory' as fallback.

If NAMES is empty, return the BASE-DIR's expanded filename.

NOTE: The NAMES is expanded using its reverse order, thus NAMES
has to be real file system path hierarchy."
  (let (form cur-name)
    (while names
      (setq cur-name (pop names))
      (if form
          (setq form
                `(expand-file-name
                  ,cur-name
                  ,form))
        (setq form
              `(expand-file-name
                ,cur-name
                ,(or base-dir default-directory)))))
    (if form
        (entropy/emacs-eval-with-lexical form)
      (expand-file-name
       (or base-dir default-directory)))))

(defun entropy/emacs-list-dir-lite (dir-root &optional not-abs)
  "Return an alist of fsystem nodes as:

#+begin_src elisp
'((dir . \"a-dir\")
  (file . \"a.txt\"))
#+end_src

where the car of each elements is the node type with follow symols to
indicate that:

- 'file': the node is an file (or an symbolic to an regular file)
- 'dir':  the node is an directory (or an symbolic to an directory)

The node sort ordered by `string-lessp'

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT.

The returned list is filtered by
`directory-files-no-dot-files-regexp' i.e. without '.' or '..'
included.
"
  (let (rtn-full rtn-lite rtn-attr)
    (setq rtn-full (directory-files dir-root (not not-abs)))
    (dolist (el rtn-full)
      ;; filter the . and ..
      (if (string-match-p
           directory-files-no-dot-files-regexp
           (if not-abs
               el
             ;; sans the directory part when filtering the absolute
             ;; path since `directory-files-no-dot-files-regexp' can
             ;; not handle the absolute path i.e. it just support
             ;; the filename.
             (file-name-nondirectory el)))
          (push el rtn-lite)))
    (if rtn-lite
        (progn
          (dolist (el rtn-lite)
            (if (file-directory-p (expand-file-name el dir-root))
                (push `(dir . ,el) rtn-attr)
              (push `(file . ,el) rtn-attr)))
          rtn-attr)
      nil)))

(defun entropy/emacs-list-dir-subdirs (dir-root &optional not-abs)
  "List subdir of root dir DIR-ROOT, ordered by `string-lessp'.

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT."
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root not-abs))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (eq 'dir (car el))
                (push (cdr el) rtn)))
          (if rtn
              (reverse rtn)
            nil))
      nil)))

(defun entropy/emacs-list-dir-subfiles (dir-root &optional not-abs)
  "Return a list of file(not directory) under directory DIR-ROOT.

The structure of return is ordered by `string-lessp'.

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT."
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root not-abs))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (when (eq 'file (car el))
              (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(cl-defun entropy/emacs-list-dir-subdirs-recursively
    (top-dir &optional not-abs
             &key
             with-attributes
             with-level
             with-filter
             map-func
             ;; remained
             remain--not-calling-at-root
             remain--top-dir-expand-of
             remain--parent-attrs
             remain--parent-subdir-nth-for-current
             remain--prev-rel-path)
  "List directory TOP-DIR's sub-dirctorys recursively, return a
=dir-spec=, whose car was a path for one dirctory i.e. a =node-name=
and the cdr was a list of =dir-spec= or nil if no sub-dir under
it. The structure of return is ordered by `string-lessp'.

If optional arg NOT-ABS is non-nil then each =node-name= is relative
to the corresponding parent path.

If optional key WITH-LEVEL is non-nil and it should be an integer to
indicate the recursively listing level for the TOP-DIR and should
larger or equal than 1. This is as the well known linux command 'tree'
does.

If optional key WITH-FILTER is specifeid, its a function which take
three arguments, i.e. a file 'type' (the car of each element of the
return of `entropy/emacs-list-dir-lite') and the absolute path who is
one of the subfile or subdir of the mapping dir and its name
(i.e. filename wihtout directory components). The function must return
nil if the node need be listed and non-nil for filtering out.

There're some inner supported filter func can be used as WITH-FILTER
when set as:
- 't': `entropy/emacs-list-dir-subdirs-recursively/filter/ignore-hidden'

If optional key WITH-ATTRIBUTES is enabled or the optional key
MAP-FUNC is set, the car of the =dir-spec= is an cons of (dir
. attributes-plist). In which case, the =attributes-plist= is
defaultly include follow keys:

#+begin_src elisp
  (list
   :dir-abspath dir-abspath
   :dir-is-root-p dir-is-root-p
   :dir-name dir-name
   :dir-subdirs-names dir-subdirs
   :dir-subfiles-names dir-subfiles
   :dir-rel-path dir-rel-path
   :dir-rel-path-level dir-rel-path-level
   :dir-nth-pos-of-parent-subdirs dir-nth-pos-of-parent-subdirs
   :dir-nth-pos-is-at-end-of-parent-subdirs dir-nth-pos-is-at-end-of-parent-subdirs
   :dir-parent-attrs dir-parent-attrs
   :dir-user-attrs dir-user-attrs)
#+end_src

The MAP-FUNC is an function used to participate with building
each =dir-spec= but not influenced the core result of thus. It
run after the current =dir-spec= has built its car place and
generated its =attributs-plist= done, thus for as, the MAP-FUNC
accept only one major argument, i.e. current =dir-spec='s
=attributes-plist= and its return will be put in place of the
DIR-USR-ATTRS of current =dir-spec='s =attributes-plist= before
generate current =dir-spec='s subdirs =dir-spec=.

The MAP-FUNC also be invoked while the recursive mapping returned from
the current =dir-spec='s subdir or just after the end of current node
dealing procedure while no subdirs found for current =dir-spec=, in
which case its optional arg END-CALL-P will be set, and we called this
operation =map-func-end-call=.

Thus the MAP-FUNC's formula is:
#+begin_src elisp
  (lambda (attributes-plist &optional end-call-p)
    (let (_)
      (cond
       (end-call-p
        (do-something-for-end-call))
       (t
        (do-something-commonly)))))
#+end_src

*ATTRIBUTES-PLIST* key description:

DIR-IS-ROOT-P is t when current =dir-spec='s dir is TOP-DIR, nil
for otherwise.

DIR-ABSPATH is the current =dir-spec='s dir's absolute path name,
DIR-NAME is the dir name, DIR-SUBDIRS-NAMES is a list of subdirs
names of current dir and so as such as DIR-SUBFILES-NAMES.

DIR-NAME, DIR-SUBDIRS-NAMES and DIR-SUBFILES-NAMES are all relative
name(s).

DIR-SUBDIRS-NAMES and DIR-SUBFILES-NAMES can be nil while no such
reflects.

DIR-REL-PATH is an list of dir names ordered as relative path
from TOP-DIR to the current =dir-spec='s dir(included) or nil
when DIR-IS-ROOT-P TRUE, and DIR-REL-PATH-LEVEL is that relative
depth integer as same as ~(length DIR-REL-PATH)~.

DIR-NTH-POS-OF-PARENT-SUBDIRS is an 0-based index integer to
indicate the current =dir-spec='s dir's pos of the parent's
DIR-SUBDIRS-NAMES. It's nil when the current =dir-spec='s dir is
TOP-DIR.

DIR-NTH-POS-IS-AT-END-OF-PARENT-SUBDIRS is 1 when
DIR-NTH-POS-OF-PARENT-SUBDIRS is the tail index and 0 for that
its not thus. When it is nil indicate that this indicator is
unusable since DIR-IS-ROOT-P is true.

DIR-PARENT-ATTRS is the =attributes-plist= of the current
=dir-spec='s parent dir's =attributes-plist=, and it is nil while
DIR-IS-ROOT-P is true. The DIR-USER-ATTRS is the current dir's
MAP-FUN operation's return which we've described earlier, so it
is always nil while the MAP-FUNC is calling on.

We involved the DIR-PARENT-ATTRS is for user to chasing the mapping
status from TOP-DIR to the current =dir-spec='s dir so that the
MAP-FUNC can be view more details thus on.

If the DIR-USER-ATTRS is an plist (which predicated by
`entropy/emacs-strict-plistp'), there're special key are meaningful
for this framwork:

- =:should-not-operate-map-func-end-call= : if set, the operation
  =map-func-end-call= is not be invoked.

- =:should-not-operate-subdirs=: if set, we will not mapping to the
  subdirs of current =dir-spec='s node path or terminated the rest
  subdirs mapping when this value has been changed by the current
  subdir mapping operation (see section *Fallback Modification*)

NOTE:

The keys:
REMAIN--NOT-CALLING-AT-ROOT,
REMAIN--TOP-DIR-EXPAND-OF,
REMAIN--PARENET-DIRNAME,
REMAIN--PREV-REL-PATH,
REMAIN--PARENT-SUBDIR-NTH-FOR-CURRENT,
REMAIN--PARENT-ATTRSARE

Are used internally, do not use it in any way.

*Fallback Modification*:

Since the top-level =attributes-plist= is accessed by any subdirs
mapping procedure, so as on recursively, thus any level mapping
procedure can modify it by side-effects which called
=fallback-modification=, this can be did by using `entropy/emacs-plist-setf'.

But we strongly just modify the DIR-USER-ATTRS, since any non user
spec slot modification may corrupt the parents rest operations.
"
  (when with-level
    (when (< with-level 1)
      (user-error "[entropy/emacs-list-dir-subdirs-recursively]: \
level restriction must larger/equal than 1")))
  (let* ((root-calling-p (not remain--not-calling-at-root))
         (this-level (if root-calling-p 0 remain--not-calling-at-root))
         (this-root
          (if remain--top-dir-expand-of
              (expand-file-name top-dir remain--top-dir-expand-of)
            (expand-file-name top-dir)))
         ;; Just gen subfiles list when specified occasion
         ;; for performance issue.
         (use-attrs-p (or with-attributes map-func))
         ;; The should do restriction based on level restriction and
         ;; the subfiles and subdirs listing and mapping must obey it.
         (this-should-do (< this-level (or with-level most-positive-fixnum)))
         (subitems (and this-should-do (entropy/emacs-list-dir-lite this-root)))
         (filter-func (lambda (&rest args)
                        (let* ((inner-assoc
                                '((t
                                   .
                                   entropy/emacs-list-dir-subdirs-recursively/filter/ignore-hidden))))
                          (if with-filter
                              (cond
                               ((functionp with-filter)
                                (not (apply with-filter args)))
                               ((assoc with-filter inner-assoc)
                                (not (apply (alist-get with-filter inner-assoc)
                                            args)))
                               (t
                                (user-error
                                 "[entropy/emacs-list-dir-subdirs-recursively]: \
wrong type of :with-filter '%s'" with-filter)))
                            t))))
         (subfiles (and
                    use-attrs-p
                    this-should-do
                    (delete
                     nil
                     (mapcar (lambda (x)
                               (let* ((filep (eq (car x) 'file))
                                      (node-abs (and filep
                                                     (cdr x)))
                                      node-name)
                                 (when node-abs
                                   (setq node-name (file-name-nondirectory node-abs))
                                   (and (funcall filter-func 'file node-abs node-name)
                                        (if not-abs
                                            node-name
                                          node-abs)))))
                             subitems))))
         (subdirs (and this-should-do
                       (delete
                        nil
                        (mapcar (lambda (x)
                                  (let* ((dirp (eq (car x) 'dir))
                                         (node-abs (and dirp
                                                        (cdr x)))
                                         node-name)
                                    (when node-abs
                                      (setq node-name (file-name-nondirectory node-abs))
                                      (and (funcall filter-func 'dir node-abs node-name)
                                           (if not-abs
                                               node-name
                                             node-abs)))))
                                subitems))))
         (get-fname-func (lambda (x)
                           (if not-abs
                               x
                             (file-name-nondirectory x))))
         (get-fnames-func (lambda (fnames)
                            (when fnames
                              (mapcar
                               get-fname-func
                               fnames))))
         this-dirname
         this-node-car
         this-rel-path
         default-attrs
         user-spec-attrs
         (should-run-map-func-for-endcall-judge-func
          (lambda ()
            (not
             (and
              (entropy/emacs-strict-plistp user-spec-attrs)
              (plist-get user-spec-attrs
                         :should-not-operate-map-func-end-call)))))
         (should-operate-subdirs-judge-func
          (lambda ()
            (not
             (and
              (entropy/emacs-strict-plistp user-spec-attrs)
              (plist-get user-spec-attrs
                         :should-not-operate-subdirs)))))
         rtn)
    (catch :exit
      (setq this-dirname
            (if root-calling-p
                "."
              (funcall get-fname-func top-dir))
            this-rel-path
            (if root-calling-p
                nil
              (append remain--prev-rel-path
                      (list this-dirname))))
      ;; this node operation
      (when use-attrs-p
        (setq default-attrs
              (list :dir-abspath this-root
                    :dir-is-root-p root-calling-p
                    :dir-name this-dirname
                    :dir-subdirs-names (funcall get-fnames-func subdirs)
                    :dir-subfiles-names (funcall get-fnames-func subfiles)
                    :dir-nth-pos-of-parent-subdirs remain--parent-subdir-nth-for-current
                    :dir-nth-pos-is-at-end-of-parent-subdirs
                    (when remain--parent-subdir-nth-for-current
                      (if
                          (eq (length (plist-get remain--parent-attrs :dir-subdirs-names))
                              (1+ remain--parent-subdir-nth-for-current))
                          1
                        0))
                    :dir-parent-attrs remain--parent-attrs
                    :dir-user-attrs nil
                    :dir-rel-path this-rel-path
                    :dir-rel-path-level this-level)))
      (when map-func
        (setq user-spec-attrs
              (funcall map-func default-attrs))
        (when user-spec-attrs
          (setq default-attrs
                (plist-put default-attrs
                           :dir-user-attrs user-spec-attrs))))
      (let ((proper-top-dir (cond ((and root-calling-p
                                        not-abs)
                                   ".")
                                  ((and root-calling-p
                                        (not not-abs))
                                   (expand-file-name top-dir))
                                  (t
                                   top-dir))))
        (if with-attributes
            (setq this-node-car
                  (cons proper-top-dir default-attrs))
          (setq this-node-car proper-top-dir)))
      (push this-node-car rtn)
      ;; run map func end call if no subdirs
      (unless subdirs
        (when (and map-func
                   (funcall should-run-map-func-for-endcall-judge-func))
          (funcall map-func default-attrs 'end-call-p))
        (throw :exit nil))
      ;; map with this node's subdirs restricted by level ristriction
      (when (and subdirs
                 (funcall should-operate-subdirs-judge-func))
        (let ((parenth 0)
              (use-level
               (1+ this-level))
              (expand-of (when not-abs
                           this-root))
              )
          (catch :exit-map-subdirs
            (dolist (sub-dir subdirs)
              (push
               (entropy/emacs-list-dir-subdirs-recursively
                sub-dir not-abs
                :with-attributes with-attributes
                :with-level with-level
                :with-filter with-filter
                :map-func map-func
                :remain--not-calling-at-root use-level
                :remain--top-dir-expand-of expand-of
                :remain--prev-rel-path this-rel-path
                :remain--parent-subdir-nth-for-current parenth
                :remain--parent-attrs default-attrs
                )
               rtn)
              (cl-incf parenth)
              (unless (funcall should-operate-subdirs-judge-func)
                (throw :exit-map-subdirs t))))))
      ;; run map func end call
      (when (and map-func
                 (funcall should-run-map-func-for-endcall-judge-func))
        (funcall map-func default-attrs 'end-call-p)))

    ;; return
    (reverse rtn)))

(defun entropy/emacs-list-dir-subdirs-recursively/filter/ignore-hidden
    (type _ node-name)
  "The hidden files or dirs node filter function used for
`entropy/emacs-list-dir-subdirs-recursively'."
  (unless (fboundp 'rx)
    (require 'rx))
  (let ((dir-ignore-regexps
         "^\\.")
        (file-ignore-regexps
         "^\\."))
    (cl-case type
      (file
       (string-match-p file-ignore-regexps node-name))
      (dir
       (string-match-p dir-ignore-regexps node-name))
      (t
       nil))))

(cl-defun entropy/emacs-list-dir-subdirs-recursively-for-list
    (top-dir &optional not-abs exclude-top-dir
             &key
             with-level
             with-filter)
  "List all sub-directories under TOP-DIR (included unless
EXCLUDE-TOP-DIR is non-nil, see below) as a list ordered by
`string-lessp' use `entropy/emacs-list-dir-subdirs-recursively'.

When optional argument EXCLUDE-TOP-DIR is non-nil, then the
TOP-DIR is not list for as. It's useful to generate a list of
pure subdirs of TOP-DIR.

Optional argument NOT-ABS and optional keys are all related to
`entropy/emacs-list-dir-subdirs-recursively' (see it for details).
"
  (let (rtn
        map-func)
    (setq map-func
          (lambda (x &optional end-call-p)
            (unless (or end-call-p
                        (and exclude-top-dir
                             (plist-get x :dir-is-root-p)))
              (let ((dir-abs-path (plist-get x :dir-abspath)))
                (if not-abs
                    (push (entropy/emacs-make-relative-filename
                           dir-abs-path top-dir)
                          rtn)
                  (push dir-abs-path rtn)))
              ;; always return nil user-spec-attrs
              nil)))
    (entropy/emacs-list-dir-subdirs-recursively
     top-dir nil
     :with-level with-level
     :with-filter with-filter
     :with-attributes nil
     :map-func map-func)
    (reverse rtn)))

(cl-defun entropy/emacs-list-dir-subfiles-recursively-for-list
    (top-dir &optional not-abs
             &key
             with-level
             with-filter)
  "List all sub-files under TOP-DIR as a list ordered by
`string-lessp' use `entropy/emacs-list-dir-subdirs-recursively'.

Optional argument NOT-ABS and optional keys are all related to
`entropy/emacs-list-dir-subdirs-recursively' (see it for details).
"
  (let (rtn
        map-func)
    (setq map-func
          (lambda (x &optional end-call-p)
            (unless end-call-p
              (let ((dir-abs-path (plist-get x :dir-abspath))
                    (dir-subfiles (plist-get x :dir-subfiles-names)))
                (when dir-subfiles
                  (dolist (el dir-subfiles)
                    (if not-abs
                        (push (entropy/emacs-make-relative-filename
                               (expand-file-name el dir-abs-path) top-dir)
                              rtn)
                      (push (expand-file-name el dir-abs-path) rtn)))))
              ;; always return nil user-spec-attrs
              nil)))
    (entropy/emacs-list-dir-subdirs-recursively
     top-dir nil
     :with-level with-level
     :with-filter with-filter
     :with-attributes nil
     :map-func map-func)
    (reverse rtn)))

(cl-defun entropy/emacs-print-dir-recursively
    (top-dir buffer &optional with-files
             &key
             dir-face file-face
             branch-str
             branch-leaf-end-str
             branch-leaf-non-end-str
             leaf-str
             use-org-style
             with-level
             with-filter
             )
  "Print the directory structure of TOP-DIR in BUFFER mapped with
`entropy/emacs-list-dir-subdirs-recursively', also print the files
when WITH-FILES is non-nil. (like the output of unix command \"tree\")

Return t if print procedure successfully done.

Optional key description:

- DIR-FACE: face used for directory print
- FILE-FACE: face used for file print
- BRANCH-STR: the contiguous bone structure representation, a string
- BRANCH-LEAF-NON-END-STR: the contiguous node's root representation, a string
- BRANCH-LEAF-END-STR: the non-contiguous node's root representation, a string
- LEAF-STR: the node's root leaf representation, a string
- WITH-LEVEL: same as :with-level key in `entropy/emacs-list-dir-subdirs-recursively'
- WITH-FILTER: same as :with-filter key in `entropy/emacs-list-dir-subdirs-recursively'

Examples

#+begin_src elisp
  (entropy/emacs-print-dir-recursively
   \"/etc/systemd\"
   t
   :with-level 2
   :with-filter t)
#+end_src

#+begin_example
  . (/etc/systemd)
  ├── network
  ├── system
  │   ├── dbus-org.bluez.service
  │   ├── dbus-org.freedesktop.nm-dispatcher.service
  │   ├── dbus-org.freedesktop.timesync1.service
  │   ├── default.target
  │   └── display-manager.service
  ├── user
  │   └── pipewire-session-manager.service
  ├── coredump.conf
  ├── homed.conf
  ├── journal-remote.conf
  ├── journal-upload.conf
  ├── journald.conf
  ├── logind.conf
  ├── networkd.conf
  ├── oomd.conf
  ├── pstore.conf
  ├── resolved.conf
  ├── sleep.conf
  ├── swap.conf
  ├── system.conf
  ├── timesyncd.conf
  └── user.conf
#+end_example

#+begin_src elisp
  (entropy/emacs-print-dir-recursively
   \"/etc/systemd\"
   nil
   :with-level 2
   :with-filter t)
#+end_src

#+begin_example
  . (/etc/systemd)
  ├── network
  ├── system
  └── user
#+end_example

When optional key USE-ORG-STYLE is non-nil print the `org-mode'
headline based tree and ignore all common tree style specifications.

The USE-ORG-STYLE defaultly output each node with its propeties as of

#+begin_src org-mode
,** java-openjdk
,:LOCATION: /etc/java-openjdk
,:DIR-REL-PATH: (etc java-openjdk)
,:DIR-ABS-PATH: /etc/java-openjdk
,:NODE-TYPE: directory
,:NODE-SIZE: 138
#+end_src
"
  (let* ((dir-face (or dir-face (progn (unless (featurep 'dired) (require 'dired)) 'dired-directory)))
         (file-face (or file-face 'default))
         (brs (or branch-str "│"))
         (brslen (length brs))
         (brlnes (or branch-leaf-non-end-str "├"))
         (brlneslen (length brlnes))
         (brles (or branch-leaf-end-str "└"))
         (brleslen (length brles))
         (lfs (or leaf-str "── "))
         (lfslen (length lfs))
         dir-indent-stack
         file-indent-tail-stack
         file-indent-tail-as-end-stack
         chase-func
         map-func
         )
    (when (or (> brslen
                 (+ brlneslen lfslen))
              (> brslen
                 (+ brleslen lfslen)))
      (user-error "[entropy/emacs-print-dir-recursively]: \
BRANCH-STR '%s' is too long!" brs))
    (let* ((node-nes (concat brlnes lfs))
           (node-nees (concat brs
                              (make-string
                               (- (length node-nes) brslen)
                               ?\ )))
           (node-es (concat brles lfs))
           (node-ees (make-string (length node-es)
                                  ?\ ))
           )
      (setq chase-func
            (lambda (x &optional level)
              (let* ((level (or level 0))
                     (dir-is-root (plist-get x :dir-is-root-p))
                     (dir-subfiles (and with-files
                                        (plist-get x :dir-subfiles-names)))
                     (dir-pare-attrs (plist-get x :dir-parent-attrs))
                     (dir-pare-subfiles (and with-files
                                             (plist-get dir-pare-attrs :dir-subfiles-names)))
                     (dir-end-of-pare-p (and (not dir-pare-subfiles)
                                             (eq 1
                                                 (plist-get
                                                  x
                                                  :dir-nth-pos-is-at-end-of-parent-subdirs))))
                     )
                (when (= level 0)
                  (setq dir-indent-stack nil
                        file-indent-tail-stack nil
                        file-indent-tail-as-end-stack nil))
                (cond
                 ((and dir-is-root (= level 0))
                  (setq dir-indent-stack '("")
                        file-indent-tail-stack `(,node-nes)
                        file-indent-tail-as-end-stack `(,node-es)))
                 ((and dir-is-root (> level 0))
                  nil)
                 ((= level 0)
                  (if dir-end-of-pare-p
                      (progn
                        (push node-es dir-indent-stack)
                        (when dir-subfiles
                          (setq file-indent-tail-stack `(,node-ees ,node-nes)
                                file-indent-tail-as-end-stack `(,node-ees ,node-es))))
                    (push node-nes dir-indent-stack)
                    (when dir-subfiles
                      (setq file-indent-tail-stack `(,node-nees ,node-nes)
                            file-indent-tail-as-end-stack `(,node-nees ,node-es))))
                  (funcall chase-func dir-pare-attrs (1+ level)))
                 (t
                  (if dir-end-of-pare-p
                      (push node-ees dir-indent-stack)
                    (push node-nees dir-indent-stack))
                  (funcall chase-func dir-pare-attrs (1+ level))))
                )))
      (setq map-func
            (lambda (x &optional end-call-p)
              (let* ((dir-is-root-p (plist-get x :dir-is-root-p))
                     (dir-rel-level (plist-get x :dir-rel-path-level))
                     (dir-rel-pathlist (plist-get x :dir-rel-path))
                     (dir-name (plist-get x :dir-name))
                     (dir-name-inst (propertize dir-name 'face dir-face))
                     (dir-abs-path (plist-get x :dir-abspath))
                     (dir-abs-path-inst (propertize dir-abs-path 'face 'shadow))
                     (dir-subfiles (and with-files
                                        (plist-get x :dir-subfiles-names)))
                     dir-indent-inst
                     file-indent-inst
                     file-indent-as-end-inst
                     org-info-func)

                (setq org-info-func
                      (lambda (type &optional fname)
                        (let* ((node-path (expand-file-name (or fname "") dir-abs-path))
                               (node-attrs (entropy/emacs-get-filesystem-node-attributes node-path)))
                          (concat
                           ":PROPERTIES:\n"
                           (format
                            ":LOCATION: %s\n"
                            node-path)
                           (apply
                            'format
                            ":%s: %s\n"
                            (cond
                             ((eq type 'dir)
                              (list "DIR-REL-PATH" dir-rel-pathlist))
                             ((eq type 'file)
                              (list "FILE-REL-PATH" (append dir-rel-pathlist (list fname))))))
                           (apply
                            'format
                            ":%s: %s\n"
                            (cond
                             ((eq type 'dir)
                              (list "DIR-ABS-PATH" node-path))
                             ((eq type 'file)
                              (list "FILE-ABS-PATH" node-path))))
                           (format ":NODE-TYPE: %s\n"
                                   (let* (_)
                                     (cond
                                      ((file-symlink-p node-path) "symlink")
                                      ((file-directory-p node-path) "directory")
                                      (t
                                       (if (> (plist-get node-attrs :link-number) 1)
                                           (format "%s(%s)" "hardlink" (plist-get node-attrs :link-number))
                                         "file")))))
                           (format ":NODE-SIZE: %s\n"
                                   (file-size-human-readable
                                    (plist-get node-attrs :size)))
                           ":END:\n"))))

                (cond
                 (use-org-style
                  (cond
                   (end-call-p
                    (when dir-subfiles
                      (dolist (f dir-subfiles)
                        (insert (format "%s %s\n"
                                        (make-string (+ 2 dir-rel-level) ?*)
                                        f))
                        (insert (funcall org-info-func 'file f)))))
                   (t
                    (insert (format "%s %s\n"
                                    (make-string (1+ dir-rel-level) ?*)
                                    (if dir-is-root-p
                                        dir-abs-path
                                      dir-name)))
                    (insert (funcall org-info-func 'dir)))))
                 (t
                  (funcall chase-func x)
                  (setq dir-indent-inst
                        (mapconcat 'identity dir-indent-stack "")
                        file-indent-inst
                        (mapconcat 'identity
                                   (append (butlast dir-indent-stack) file-indent-tail-stack)
                                   "")
                        file-indent-as-end-inst
                        (mapconcat 'identity
                                   (append (butlast dir-indent-stack) file-indent-tail-as-end-stack)
                                   ""))
                  (cond
                   (end-call-p
                    (when dir-subfiles
                      (let ((count 0)
                            (ovflow (1- (length dir-subfiles))))
                        (dolist (f dir-subfiles)
                          (if (= count ovflow)
                              (insert (format "%s%s\n" file-indent-as-end-inst
                                              (propertize f 'face file-face)))
                            (insert (format "%s%s\n" file-indent-inst
                                            (propertize f 'face file-face))))
                          (cl-incf count)))))
                   (t
                    (if dir-is-root-p
                        (insert (format "%s (%s)\n" dir-name-inst dir-abs-path-inst))
                      (insert (format "%s%s\n" dir-indent-inst dir-name-inst)))))))

                ;; we should return the null user spec attrs since we do not need it
                nil
                )))

      (with-current-buffer buffer
        (entropy/emacs-list-dir-subdirs-recursively
         top-dir t
         :map-func map-func
         :with-level with-level
         :with-filter with-filter))
      t)))

(declare-function org-shifttab "org")
(declare-function outline-on-heading-p "outline")
(define-minor-mode entropy/emacs-do-directory-mirror/log-mode
  "Minor mode for `entropy/emacs-do-directory-mirror' log buffer,
simpley enable `outline-minor-mode' and binds its interactive
commands as below keymap:

`outline-cycle': TAB
`outline-cycle-buffer' <backtab>

And full keymap as:

\\{entropy/emacs-do-directory-mirror/log-mode-map}"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap self-insert-command] 'ignore)
            (define-key map [remap ] 'ignore)
            (define-key map (kbd "TAB")
              #'(lambda (&rest _)
                  (interactive)
                  (when (outline-on-heading-p)
                    (if (fboundp 'outline-cycle)
                        ;; use `outline' internal api when emacs version
                        ;; larger than 27.
                        (outline-cycle)
                      (unless (fboundp 'org-cycle)
                        (require 'org))
                      (org-cycle)))))
            (define-key map (kbd "<backtab>")
              #'(lambda (&rest _)
                  (interactive)
                  (if (fboundp 'outline-cycle-buffer)
                      (outline-cycle-buffer)
                    (org-shifttab))))
            (define-key map " " #'next-line)
            (define-key map "\C-n" #'next-line)
            (define-key map [down] #'next-line)
            (define-key map "\C-p" #'previous-line)
            (define-key map "\C-?" #'previous-line)
            (define-key map [up] #'previous-line)
            (define-key map (kbd "RET") #'ignore)
            (define-key map (kbd "q") #'quit-window)
            map)
  (when entropy/emacs-do-directory-mirror/log-mode
    (let (_)
      (outline-minor-mode))))

(cl-defun entropy/emacs-do-directory-mirror
    (srcdir destdir
            &key
            with-level
            with-check-symlink
            with-check-hardlink
            file-mirror-func
            dir-mirror-func
            operation-symbol
            use-symbolic-link
            pop-log
            no-error-when-srcdir-is-empty-p)
  "Do directory mirror from SRCDIR to DESTDIR with =op-function= that
using default FILE-MIRROR-FUNC whose subroutine is `add-name-to-file'
for its subfiles and default DIRECORY-MIRROR-FUNC whose subroutine is
`make-directory' for subdirs, recursively.

If SRDIR is not existed, sign an error. If DESETDIR is existed sign an
error since we treat DESTDIR as the same hierachy as SRCDIR.

Sign an error when SRCDIR is empty (i.e neither files nor dirs are
found under it) unless optional key NO-ERROR-WHEN-SRCDIR-IS-EMPTY-P is
non-nil.

If DESTDIR is under SRCDIR, sign a error since it will make unlimited
recursively or messy up your file-system.

When optional key WITH-LEVEL is non-nil, we just do the recursively in
depth of that level so that its must larger than 0.

If optional key OPERATION-SYMBOL is set, it should be a symbol to
indicate that how to use this framework as customization aspect. If
not set, the anonymous symbol is set to it internally. The setted
OPERATION-SYMBOL only used when any of user spec =op-function= is set
(see section *MIRROR FUNC SPEC*), and this framework has default
OPERATION-SYMBOL for describing the default =op-function=.

* *MIRROR FUNC SPEC*

The mirror function is called =op-function= used for recursively
participates with this framework. There're two type =op-function=
i.e. FILE-MIRROR-FUNC and DIR-MIRROR-FUNC, which see below.

** =op-function= FILE-MIRROR-FUNC
If FILE-MIRROR-FUNC is customized spec for, its should take flow
arguments (and respect the order):

1. SRCSUBFILE: the source sub-file abs-path under any nested level of
   the SRCDIR

2. DESTSUBFILE : the destination sub-file abs-path under any nested
   level of the DESTDIR which intends to be mirrored to but finally
   handled by this function.

3. ATTRIBUTES-PLIST: the =attributes-plist= same as what
   `entropy/emacs-list-dir-subdirs-recursively' given from the current
   nested level of SRCDIR in where the SRCSUBFILE hosted, actually
   this framework use it as the core mapping subroutine.

4. FORCE-USE-SYMBOLIC-LINK-P: t or nil for indicate current
   SRCSUBFILE can not used to hardlinked to the DEESTSUBFILE.

5. SRCSUBFILE-TYPE: the source sub-file's filesystem-node type
   =node-type= a list of =node-sub-type= (symbol) whose car is the
   default =note-sub-type= called =node-core-type= and must be placed
   in i.e the =node-type= has at least one element and the cdr is
   called =node-rest-type=. (see *Node Type* section for details)

   + If WITH-CHECK-SYMLINK is non-nil and SRCSUBFILE is an
     symbolic-link then an =node-sub-type='symlink' is placed in
     =node-rest-type=.

   + If WITH-CHECK-HARDLINK is non-nil and SRCSUBFILE's inode number
     larger than 1, then a =node-sub-type= 'hardlink' also placed for as.


It should return a plist (the =sub-op-return=) as follow rules:

1. =:op-target-abs-path= : the new absolute DESTSUBFILE path. If the
   new DESTSUBFILE is not existed which checked by `file-exists-p' an
   error is treated as, unless the optional key
   =:without-ensure-target-existed-status= is non-nil (see below).

2. =:without-ensure-target-existed-status= : do not check the existed
   status of =:op-target-abs-path=.

3. =:op-target-node-type= : a =node-type= to indicate the generated target file type.

4. =:op-symbol= (optional) : the operation indicator, a symbol which is
   the indicator for identifying this =op-function= itself and if it
   is not set in which case this framework will fallback to the
   =defautl-op-symbol= (see the end of this section).

5. =:op-name= (optional) : the operation indicator name of =op-symbol=
   slot, is a string. If not set, fallback to =default-op-name= (see
   the end of this section).


If optional key USE-SYMBOLIC-LINK is non-nil then the default
FILE-MIRROR-FUNC use `make-symbolic-link' instead for as. This
variable will auto be set when SRCDFIR and DESTDIR is not in the same
file-system since we can not use hardlink in two different
file-system. FORCE-USE-SYMBOLIC-LINK-P will be set when
USE-SYMBOLIC-LINK is non-nil.

** =op-function= DIR-MIRROR-FUNC
If DIR-MIRROR-FUNC is customized spec for, its should take follow
arguments (and respect the order):

1. SRCSUBDIR: the source sub-dir abs-path under any nested level of the SRCDIR

2. DESTSUBDIR : the destination sub-directory abs-path under any
   nested level of the DESTDIR which intends to be mirrored but
   finally handled by this function.

3. ATTRIBUTES-PLIST: the =attributes-plist= same as what
   `entropy/emacs-list-dir-subdirs-recursively' given from the current
   nested level of SRCDIR of which the DESTSUBDIF owned, actually this
   framework use it as the core mapping subroutine.

4. SRCSUBDIR-TYPE: the source sub-dir's filesystem-node =node-type=.

   Its =node-core-type= is 'dir' and it can coexisted with below
   =node-sub-type=:

   + If WITH-CHECK-SYMLINK is non-nil and SRCSUBDIR is an symbolic
     link then 'symlink' pushed on its =node-rest-type=.

   + If WITH-CHECK-HARDLINK is non-nil and SRCSUBDIR's inode number
     larger than 1, then a =node-sub-type= 'hardlink' also placed for
     as.


It should return the an =sub-op-return= as what the FILE-MIRROR-FUNC
did but with follow mentions:

+ The key =:op-target-abs-path= is a new absolute DESTSUBDIR path used
  for generating FILE-MIRROR-FUNC's DESTSUBFILE arg. If the new
  DESTSUBDIR is not existed checked by `file-directory-p' an error is
  treated as, unless optional key
  =:without-ensure-target-existed-status= is non-nil.

+ The DIR-MIRROR-FUNC always called before FILE-MIRROR-FUNC so that be
  careful that you should respect the FILE-MIRRO-FUNC inner processing
  logic when you build your own DIR-MIRRO-FUNC, and thought this
  notice at first level. If an error catched when invoking the
  DIR-MIRROR-FUNC, the current level's FILE-MIRROR-FUNC will not be
  invoked after all and so even if it has no error catched but its
  =sub-op-return= is invalid checked internally.

** =op-function= Other Rules

The =op-function= can aslo be an plist of below structure:

: (:default-op-symbol default-op-symbol :default-op-name default-op-name :op-function op-function)

Where the =default-op-symbol= is an operation symbol like =op-symbol=
return by the =op-function= and is used for logging with this
framework's log system (see section *RETURN*) as the default operation
indicator when =op-symbol= can not getted from the =op-function= such
as an inner error occurred within it or that is not set. And
=default-op-name= is its name string and so as dealing with missing
=op-name= as thus.

If FILE-MIRROR-FUNC and DIR-MIRROR-FUNC both are symbols of functions,
they are also be the =default-op-symbol/name= i.e. the
DEFAULT-OP-SYMBOL reuse themselves as is and the DEFAULT-OP-NAME is
auto-generated based on it.

This framework automatically add the missing =default-op-symbol=
=default-op-name= internally using below algorithm:

#+begin_example
  a0: user spec default op sym
  a1: user spec default op name
  Z: user spec OPERATION-SYMBOL
  A: internal fallback default op sym
  C(sym): internal default op sym generator
  D(sym): internal default op name generator

  if a0
     set a0 = a0
  elif Z
     set a0 = C(Z)
  else
     set a0 = C(A)

  if a1
     set a1 = a1
  else
     set a1 = D(a0)
#+end_example

* *Node Type*

The =node-type= commolny has two type =node-core-type=:

1. 'file': node is not a 'dir' type filesystem node.
2. 'dir':  node is a directory filesystem node which satisfied
           `file-directory-p'.

Thus for the both 'file' and 'dir' =node-core-type= can be an symbolic
link can has an 'symlink' subsequent =node-sub-type=.

Further more an and 'file' =node-sub-type= can also be an 'hardlink',
thus the 'hardlink' is also an =node-sub-type= and it can be coexisted
with the 'symlink' since an symbolic can also be hardlinked for. Thus,
an =node-type= can both 'symlink' and 'hardlink' =node-sub-type=.

And that common occasion retrict the =node-type= for one of below five
types:

#+begin_src emacs-lisp :tangle yes
  '(file)
  '(file symlink)
  '(file hardlink)
  '(file symlink hardlink) ;; #1
  '(file hardlink symlink) ;; #2
  '(dir)
  '(dir symlink)
  '(dir hardlink symlink) ;; #3
  '(dir symlink hardlink) ;; #4

  ;; where we treat #1 and #2 is the same although they has not `equal'
  ;; but we must ignore the place order since the order of non ==

#+end_src

And we call this type =node-type= is =node-type-common=, these five
=node-type= collection is called =node-type-common-collection=. And we
do not allow any =node-type= is not an =node-type-common=.

But with this framwork, we also has an =node-type-userspec= without
any =node-sub-type= type restriction unless the car of it is a symbol
':userspec-node-type' and the rest is arbitrary. And
=node-type-userspec= just can be used as the =op-target-node-type=
since the SRCSUBFILE and SRCSUBDIR is real file and directory and we
should use =node-type-common= to describe it in any case.

This framework will check any =node-type= obtaining with above rules
and throw an error its invalid.

There's also another special =node-type= its permanently a null which
can be juged by `null' which indicate this =node-type= can not be used
and should ignore any operation on it which mean we do not throw
checker error.

* *RETURN*

Return t when succeed for all operations. And always return nil when
no operations has been did since SRCDIR has neither subfiles nor
subdirs (i.e. its empty) or has fatal operation(s) catched. And this
is the =op-common-return=.

When POP-LOG is non-nil (except when its value is 't' in which case
return =op-common-return= either but with more operations, see its
description for details) the return is a =op-rich-return= a cons whose
car was the =op-common-return= and its cdr is called =op-sub-return=
detailes by the POP-LOG value described as below list:

1. 't': Popup a log buffer without =op-sub-return=.

2. 'log': its =op-sub-return= is the log list var i.e. the =log-value= a list of
   plist of the operation details as key-pairs as:

   1) =:op-symbol=         : the operation symbol indicator and is the =op-symbol= return by
                             =op-function=.

   2) =:op-name=           : the operation name indicator (string) and is the =op-name=
                             return by =op-function=.

   3) =:src-abs-path=      : the source abs path path to the =op-function='s arg SRCSUBFILE or SRCSUBDIR.

   4) =:dest-abs-path=     : the destination abs path to which is the =op-funciton='s arg
                             DESTSUBFILE or DESTSUBDIR.

   5) =:dest-abs-path-new= : the abs path obtaianed of =sub-op-return='s =op-target-abs-path=. or nil
                             when it is `entropy/emacs-existed-filesystem-nodes-equal-p' the =:dest-abs-path=.

   6) =:src-node-type=     : obtained by SRCSUBFILE-TYPE when the =op-function= is FILE-MIRROR-FUNC
                             or SRCSUBDIR-TYPE when the =op-function= is DIR-MIRROR-FUNC.

   7) =:dest-node-type=    : the dest node filesystem-node type what opbtained by =sub-op-return='s
                             =op-target-node-type=.

   8) =:src-relative-list= : the relative path list based on SRCDIR
                             (e.g. =(\"subdir_level_1\" \"subdir_level_1-1\" \"subdir_level_1-2\")=)
                             which can be used by `entropy/emacs-batch-expand-file-name'.

   9) =:process-succeed-p= : the operation exit status that be t for indicating success and nil
                             for being \"with-fatal or error\", if it is an integer that say the
                             =op-function= is did successfully but the =sub-op-return= is invalid
                             as fully or partially.

   10) =:error-msg=        : the error message when =:process-succeed-p= is not t, automatically
                             catched by `condition-case'.

3. 'log-buffer': its =op-sub-return= is an cons cel of car of the =log-buffer=
                 and cdr of =log-value=.

                 The =log-buffer= is fontified for human readable
                 format and the buffer is enabled an mode
                 `entropy/emacs-do-directory-mirror/log-mode' which follow
                 `outline-minor-mode' benefits. So as on used, user can insert the
                 log string into arbitrary `fundamental-mode' buffer and enable that
                 mode. And the log is outline formatted based one top header so that
                 you can concatenate the log string by multi-times invoke this
                 function to generate a multi-operations log summary.

4. 'log-string': its =op-sub-return= is an cons cel of car of the =log-buffer='s
                 substring with its face properties and cdr of =log-value=.

                 It has an sub type 'log-string-with-trim-title-style'
                 which trim the title (the top head) as its children head style.


If the =op-sub-return= is null, then the =op-common-return= retured,
otherwise return the =op-rich-return=.

Sign an error when POP-LOG is not matched valied values.

"
  (setq srcdir (expand-file-name srcdir)
        destdir (expand-file-name destdir))
  (cond
   ((not (file-directory-p srcdir))
    (user-error "[entropy/emacs-do-directory-mirror] source path not existed or is not an directory '%s'"
                srcdir))
   ((file-exists-p destdir)
    (user-error "[entropy/emacs-do-directory-mirror] destination existed '%s'"
                destdir))
   ((not (file-readable-p srcdir))
    (user-error "[entropy/emacs-do-directory-mirror] source path not readable '%s'"
                srcdir))
   ((not (file-writable-p (file-name-directory
                           (entropy/emacs-directory-file-name destdir))))
    (user-error "[entropy/emacs-do-directory-mirror] destination host path not writeable '%s'"
                (file-name-directory (entropy/emacs-directory-file-name destdir))))
   ((entropy/emacs-make-relative-filename destdir srcdir)
    (user-error "[entropy/emacs-do-directory-mirror] destination '%s' is under the source directory '%s'"
                destdir srcdir)))

  ;; Always let default FILE-MIRROR-FUNC use symbolic when SRCDIR
  ;; and DESTDIR is not in the same filesytem since the hardlink is
  ;; not usable for such case.
  (unless (entropy/emacs-filesystem-nodes-in-same-filesystem-p
           (file-name-directory
            (entropy/emacs-directory-file-name destdir))
           ;; we should indicate that the SRCDIR is an directory
           (file-name-as-directory
            srcdir))
    (unless use-symbolic-link
      (setq use-symbolic-link
            '(t . "cross filesystem detected"))))

  (let* ((user-file-mirror-func file-mirror-func)
         (user-dir-mirror-func dir-mirror-func)
         use-user-mirror-func-p
         (f-type-check-func
          (lambda (x def &optional symcheck-force)
            (let ((base-type (list def))
                  (base-fattrs nil))
              ;; fistly trim the directory-name indicator since we
              ;; must treat it as a file before checking its type
              (setq x (entropy/emacs-directory-file-name x))
              (when (or with-check-symlink symcheck-force)
                (when (file-symlink-p x)
                  (push 'symlink base-type)))
              (when with-check-hardlink
                (setq base-fattrs
                      (entropy/emacs-get-filesystem-node-attributes x))
                (when (> (plist-get base-fattrs :link-number) 1)
                  (push 'hardlink base-type)))
              (reverse base-type))))
         (file-mirror-func
          (lambda (srcf destf &rest _)
            (let (_)
              (if use-symbolic-link
                  (make-symbolic-link srcf destf)
                (add-name-to-file srcf destf))
              (list :op-target-abs-path destf
                    :without-ensure-target-existed-status nil
                    :op-target-node-type
                    (funcall f-type-check-func destf 'file use-symbolic-link)
                    :op-symbol
                    (if use-symbolic-link 'symlink 'hardlink)
                    :op-name
                    (if use-symbolic-link "SYMLINK" "HARDLINK")))))
         (dir-mirror-func
          (lambda (&rest args)
            (let ((target-dir (nth 1 args)))
              (make-directory target-dir)
              (list :op-target-abs-path target-dir
                    :without-ensure-target-existed-status nil
                    :op-target-node-type '(dir)
                    :op-symbol 'mkdir
                    :op-name "MKDIR"))))
         op-type
         op-name
         (op-name-gen-func
          (lambda (x)
            (with-temp-buffer
              (insert (format "%s" x))
              (upcase-region (point-min) (point-max))
              (buffer-substring-no-properties
               (point-min) (point-max)))))
         dir-op-type
         dir-op-name
         file-op-type
         file-op-name
         op-log
         op-log-summary-msg
         op-log-buffer
         op-log-substr
         use-symbolic-link-reason-msg
         (use-log-buffer (member pop-log '(t log-buffer)))
         (use-log-string (member pop-log '(log-string log-string-with-trim-title-style)))
         (use-log-temp-buffer use-log-string)
         (use-log-value (eq pop-log 'log))
         (need-pop-log-buffer (eq pop-log t))
         map-func log-buffer-func
         (destdir-ftruename (file-truename (entropy/emacs-directory-file-name destdir)))
         (fcounts 0) (dircounts 0) (fcounts-error 0) (dircounts-error 0)

         (node-type-checker-func
          (lambda (x)
            (cond ((ignore-errors (or (eq (car x) :userspec-node-type)
                                      (null x)))
                   t)
                  (t
                   (let ((def-types '((file)
                                      (file symlink)
                                      (file hardlink)
                                      (file symlink hardlink)
                                      (file hardlink symlink)
                                      (dir)
                                      (dir symlink)
                                      (dir symlink hardlink)
                                      (dir hardlink symlink)
                                      )))
                     (if (member x def-types)
                         t
                       nil))))))

         (sub-op-return-check-func
          (lambda (x)
            (let ((op-tnodetype (plist-get x :op-target-node-type)))
              (catch :exit
                (unless (funcall node-type-checker-func op-tnodetype)
                  (throw :exit 0))
                t))))

         (all-is-success-p-func
          (lambda () (and (zerop fcounts-error) (zerop dircounts-error))))

         (fatal-or-success-string-get-func
          (lambda ()
            (if (funcall all-is-success-p-func)
                (propertize "SUCCESS" 'face 'success)
              (propertize "ERROR" 'face 'error)))))

    ;; ---------- preparation

    ;; TODO: does use-symbolic-link can be hold more information?
    (cond ((and use-symbolic-link (listp use-symbolic-link))
           (setq use-symbolic-link-reason-msg (cdr use-symbolic-link)))
          (t
           nil))

    (dolist (el `((0 . ,user-file-mirror-func)
                  (1 . ,user-dir-mirror-func)))
      (let* ((cat-slot-plist-p (entropy/emacs-strict-plistp (cdr el)))
             (func-as-slot (and cat-slot-plist-p (plist-get (cdr el) :op-function)))
             (func-as-cdr (ignore-errors (cdr el)))
             (def-op-sym-as-slot (and cat-slot-plist-p (plist-get el :op-symbol)))
             (def-op-name-as-slot (and cat-slot-plist-p (plist-get el :op-name)))
             (fbk-op-sym-file 'unknown-subfile-op)
             (fbk-op-sym-dir 'unknown-subdir-op)
             def-op-sym def-op-name
             )
        (setq def-op-sym
              (or
               ;; if set, use set
               (and def-op-sym-as-slot
                    (symbolp def-op-sym-as-slot)
                    def-op-sym-as-slot)
               ;; or if top, fallback to top
               (and operation-symbol
                    (symbolp operation-symbol)
                    (let* ((osstr (symbol-name operation-symbol)))
                      (cl-case (car el)
                        (0 (make-symbol (format "%s-file-op" osstr)))
                        (1 (make-symbol (format "%s-dir-op" osstr))))))
               ;; or use internall set
               (cl-case (car el)
                 (0 fbk-op-sym-file)
                 (1 fbk-op-sym-dir)))
              def-op-name
              (or
               ;; if set, use set
               (and def-op-name-as-slot
                    (symbolp def-op-name-as-slot)
                    def-op-name-as-slot)
               ;; or fallback to sym def since we've set it or it's
               ;; set already.
               (funcall op-name-gen-func def-op-sym)))

        (cl-case (car el)
          (0
           (setq file-op-type def-op-sym
                 file-op-name def-op-name))
          (1
           (setq dir-op-type def-op-sym
                 dir-op-name def-op-name)))

        (cond
         ((functionp func-as-cdr)
          (cl-case (car el)
            (0 (setq file-mirror-func func-as-cdr))
            (1 (setq dir-mirror-func func-as-cdr))))
         ((functionp func-as-slot)
          (cl-case (car el)
            (0 (setq file-mirror-func func-as-slot))
            (1 (setq dir-mirror-func func-as-slot))))
         (t
          (cl-case (car el)
            (0 (setq user-file-mirror-func nil))
            (1 (setq user-dir-mirror-func nil)))))))

    (setq use-user-mirror-func-p
          (if (or user-file-mirror-func user-dir-mirror-func)
              t
            nil))

    (setq op-type (cond ((and use-user-mirror-func-p
                              operation-symbol
                              (symbolp operation-symbol)
                              operation-symbol)
                         operation-symbol)
                        (use-user-mirror-func-p
                         'unknow-dir-mirror-op)
                        (t
                         (if use-symbolic-link
                             'eemacs-dir-mirror-symlink
                           'eemacs-dir-mirror-hardlink)))
          op-name (funcall op-name-gen-func op-type))


    ;; ---------- map-func instance
    (setq map-func
          (lambda (x &optional calling-end-p)
            (unless calling-end-p
              (let* ((this-map-userspec-attrs nil)
                     (dir-is-root-p (plist-get x :dir-is-root-p))
                     (this-should-do t)
                     (dir-rel-path-list
                      (plist-get x :dir-rel-path))
                     (dir-src-abspath (plist-get x :dir-abspath))
                     (dir-src-abspath-node-type
                      (funcall f-type-check-func dir-src-abspath 'dir))
                     (dir-dest-abspath
                      (entropy/emacs-batch-expand-file-name
                       dir-rel-path-list destdir))
                     dir-op-return-attrs
                     (dir-op-target-node-type nil)
                     dir-dest-abspath-new
                     dir-dest-abspath-old-equal-new-p
                     dir-dest-apspath-without-check-exist
                     (dir-subfile-names (plist-get x :dir-subfiles-names))
                     (dir-subdir-names (plist-get x :dir-subdirs-names))
                     (cur-succeed-p nil)
                     (cur-succeed-type nil)
                     cur-error-msg)

                (when (and dir-is-root-p
                           (not dir-subfile-names)
                           (not dir-subdir-names))
                  (setq this-should-do nil)
                  (when (not no-error-when-srcdir-is-empty-p)
                    (user-error "SRCDIR '%s' is empty!"
                                srcdir)))

                (when this-should-do
                  ;; initials the top condition var
                  (setq cur-succeed-p t)

                  ;; Fistly we should check whether the DESTDIR is
                  ;; under the subdirs tree if thus we should abandon
                  ;; to run the mirror process for preventing nesting
                  ;; messy.
                  (let ((src-dir-truepath
                         (entropy/emacs-directory-file-name
                          (file-truename dir-src-abspath))))
                    (when
                        (entropy/emacs-make-relative-filename
                         destdir-ftruename src-dir-truepath)
                      (setq cur-succeed-p nil
                            cur-succeed-type nil)
                      ;; notificate
                      ;; `entropy/emacs-list-dir-subdirs-recursively'
                      ;; to inhibit rest mapping operation
                      (setq this-map-userspec-attrs
                            (plist-put this-map-userspec-attrs
                                       :should-not-operate-subdirs t))
                      (setq this-map-userspec-attrs
                            (plist-put this-map-userspec-attrs
                                       :should-not-operate-map-func-end-call t))
                      (cl-incf dircounts-error)
                      (setq op-log
                            (append
                             op-log
                             `((:src-node-type ,dir-src-abspath-node-type
                                :dest-node-type dir-op-target-node-type
                                :op-symbol ,(or (plist-get dir-op-return-attrs :op-symbol) dir-op-type)
                                :op-name ,(or (plist-get dir-op-return-attrs :op-name) dir-op-name)
                                :src-abs-path ,dir-src-abspath
                                :dest-abs-path ,dir-dest-abspath
                                :src-relative-list ,dir-rel-path-list
                                :process-succeed-p ,cur-succeed-type
                                :error-msg
                                ,(format "error: prevent nested linkage (%s <-> %s)"
                                         destdir-ftruename src-dir-truepath)))))))
                  ;; do the dir mirror
                  (when cur-succeed-p
                    (condition-case error-type
                        (progn
                          (setq dir-op-return-attrs
                                (apply dir-mirror-func (list dir-src-abspath dir-dest-abspath x))
                                dir-dest-abspath-new
                                (plist-get dir-op-return-attrs :op-target-abs-path)
                                dir-dest-apspath-without-check-exist
                                (plist-get dir-op-return-attrs :without-ensure-target-existed-status)
                                dir-op-target-node-type
                                (plist-get dir-op-return-attrs :op-target-node-type)
                                dir-dest-abspath-old-equal-new-p
                                (ignore-errors
                                  (entropy/emacs-existed-filesystem-nodes-equal-p
                                   dir-dest-abspath dir-dest-abspath-new)))
                          ;; check target node manuipulation status
                          (if dir-is-root-p
                              (unless dir-dest-abspath-old-equal-new-p
                                (error "The new dest <%s> is invalid that it must be \
as the origin one <%s> at the first mirror turn."
                                       dir-dest-abspath-new dir-dest-abspath))
                            (unless dir-dest-apspath-without-check-exist
                              (if (and (stringp dir-dest-abspath-new)
                                       (not (string-empty-p dir-dest-abspath-new)))
                                  (cond (dir-dest-abspath-old-equal-new-p
                                         (unless (file-directory-p dir-dest-abspath)
                                           (error "The dest dir path %s is not existed"
                                                  dir-dest-abspath-new)))
                                        ((not (file-directory-p dir-dest-abspath-new))
                                         (error "The new dest dir path %s is not existed"
                                                dir-dest-abspath-new))
                                        ((not (entropy/emacs-make-relative-filename
                                               dir-dest-abspath-new
                                               destdir))
                                         (error "The new dest dir %s is not under the top dest host %s"
                                                dir-dest-abspath-new destdir)))
                                (error "The new dest path %s is invalid"
                                       dir-dest-abspath-new))))
                          ;; check =sub-op-return='s validation
                          (setq cur-succeed-type (funcall sub-op-return-check-func dir-op-return-attrs))
                          (setq cur-succeed-p
                                (if (eq cur-succeed-type t)
                                    t
                                  nil))
                          (cl-incf dircounts))
                      (error
                       (setq cur-succeed-p nil
                             cur-succeed-type nil)
                       (cl-incf dircounts-error)
                       (setq cur-error-msg (format "%S" error-type))))
                    (setq
                     op-log
                     (append
                      op-log
                      `((:src-node-type ,dir-src-abspath-node-type
                         :dest-node-type ,dir-op-target-node-type
                         :op-symbol ,(or (plist-get dir-op-return-attrs :op-symbol) dir-op-type)
                         :op-name ,(or (plist-get dir-op-return-attrs :op-name) dir-op-name)
                         :src-abs-path  ,dir-src-abspath
                         :dest-abs-path ,dir-dest-abspath
                         :dest-abs-path-new ,(and dir-dest-abspath-new
                                                  (not dir-dest-abspath-old-equal-new-p)
                                                  dir-dest-abspath-new)
                         :src-relative-list ,dir-rel-path-list
                         :process-succeed-p ,cur-succeed-type
                         :error-msg ,cur-error-msg)))))

                  ;; do the file mirror just when dir mirror successfully
                  (when (and cur-succeed-p dir-subfile-names)
                    (dolist (el dir-subfile-names)
                      (let* ((srcfname (expand-file-name
                                        el
                                        dir-src-abspath))
                             (srcfname-node-type (funcall f-type-check-func srcfname 'file))
                             (destfname (expand-file-name
                                         el
                                         dir-dest-abspath-new))
                             file-op-return-attrs
                             (file-op-target-node-type nil)
                             destfname-new
                             destfname-new-without-check-exist
                             destfname-old-eq-new)
                        (condition-case error-type
                            (progn
                              (setq file-op-return-attrs
                                    (apply file-mirror-func (list srcfname destfname x))
                                    destfname-new
                                    (plist-get file-op-return-attrs :op-target-abs-path)
                                    destfname-new-without-check-exist
                                    (plist-get file-op-return-attrs :without-ensure-target-existed-status)
                                    file-op-target-node-type
                                    (plist-get file-op-return-attrs :op-target-node-type)
                                    destfname-old-eq-new
                                    (ignore-errors
                                      (entropy/emacs-existed-filesystem-nodes-equal-p
                                       destfname destfname-new)))

                              ;; check the target node manupulation status
                              (unless destfname-new-without-check-exist
                                (cond (destfname-old-eq-new
                                       (unless (file-exists-p destfname)
                                         (if use-symbolic-link
                                             (if (file-attributes destfname-new)
                                                 (error "the dest file symbolic %s is broken" destfname)
                                               (error "the dest file symbolic %s is not existed" destfname))
                                           (error "The dest file name %s is not created in filesystem!"
                                                  destfname))))
                                      ((not (and (stringp destfname-new)
                                                 (not (string-empty-p destfname-new))))
                                       (error "The new dest file name %s is invalid"
                                              destfname-new))
                                      ((not (file-exists-p destfname-new))
                                       (if use-symbolic-link
                                           (if (file-attributes destfname-new)
                                               (error "the new dest file symbolic %s is broken" destfname-new)
                                             (error "the new dest file symbolic %s is not existed" destfname-new))
                                         (error "The new dest file name %s is not created in filesystem!"
                                                destfname-new)))))

                              ;; check the =sub-op-return='s validation
                              (setq cur-succeed-type (funcall sub-op-return-check-func dir-op-return-attrs))
                              (setq cur-succeed-p
                                    (if (eq cur-succeed-type t)
                                        t
                                      nil))
                              (cl-incf fcounts))
                          (error
                           (setq cur-succeed-p nil
                                 cur-succeed-type nil)
                           (cl-incf fcounts-error)
                           (setq cur-error-msg (format "%S" error-type))))
                        (setq
                         op-log
                         (append
                          op-log
                          `((:src-node-type ,srcfname-node-type
                             :dest-node-type ,file-op-target-node-type
                             :op-symbol ,(or (plist-get file-op-return-attrs :op-symbol) file-op-type)
                             :op-name ,(or (plist-get file-op-return-attrs :op-name) file-op-name)
                             :src-abs-path ,srcfname
                             :dest-abs-path ,destfname
                             :dest-abs-path-new ,(and destfname-new
                                                      (not destfname-old-eq-new)
                                                      destfname-new)
                             :src-relative-list
                             ,(append dir-rel-path-list
                                      (list el))
                             :process-succeed-p ,cur-succeed-type
                             :error-msg ,cur-error-msg)))
                         )))))

                ;; return the map spec attrs
                this-map-userspec-attrs
                ))))

    ;; ---------- log-buffer-func instance
    (setq log-buffer-func
          (lambda (log-buff)
            (unless (featurep 'org)
              (require 'org))
            (with-current-buffer log-buff
              (unless use-log-string
                (entropy/emacs-do-directory-mirror/log-mode))
              ;; insert top headline
              (let ((headop-opname-str
                     (cond
                      (use-user-mirror-func-p
                       (propertize op-name 'face 'success))
                      (t
                       (if use-symbolic-link
                           (if use-symbolic-link-reason-msg
                               (concat (propertize op-name 'face 'success)
                                       "("
                                       (propertize use-symbolic-link-reason-msg
                                                   'face 'warning)
                                       ")")
                             (propertize op-name 'face 'success))
                         (propertize op-name 'face 'warning)))))
                    (headop-success-str (funcall fatal-or-success-string-get-func)))
                (cond
                 ((eq pop-log 'log-string-with-trim-title-style)
                  (insert
                   (concat
                    ;; The simple header
                    (propertize "* " 'face 'org-level-1)
                    (format "%-8s" headop-success-str)
                    (propertize "DIRMIRROR " 'face
                                (if (funcall all-is-success-p-func)
                                    'nobreak-hyphen 'error))
                    ": "
                    (file-name-nondirectory
                     (entropy/emacs-directory-file-name srcdir))
                    ;; op indicator
                    "\nUsing operation type: "
                    headop-opname-str
                    "\n"
                    ;; Summary
                    (format "'%s' to '%s'\n(Summary: %s)"
                            srcdir destdir op-log-summary-msg)
                    "\n")))
                 (t
                  (insert
                   (format "%s Do mirror using OP [%s] for dir '%s'\nto\n'%s' %s.\n(%s)\n\n"
                           (propertize "*" 'face 'org-level-1)
                           ;; op indicator
                           headop-opname-str
                           ;; rest
                           srcdir destdir
                           headop-success-str
                           op-log-summary-msg)))))
              ;; insert sub-headers
              (let* (op-attrs
                     insop-sym
                     insop-name
                     src-node-type
                     dest-node-type
                     src-path
                     dest-path
                     dest-path-new
                     path-rellist
                     did-success
                     did-error-msg
                     (inhibit-read-only t)
                     (subitem-insert-func
                      (lambda (x success-face fatal-face)
                        (insert (concat
                                 (propertize "** " 'face 'org-level-2)
                                 (cond
                                  ((eq did-success t)
                                   (propertize "SUCCESS " 'face 'success))
                                  ((integerp did-success)
                                   (propertize "WARNING " 'face 'warning))
                                  (t
                                   (propertize "FATAL   " 'face 'error)))

                                 (propertize x 'face (if did-success success-face fatal-face))
                                 ": "
                                 (if path-rellist
                                     (mapconcat 'identity path-rellist
                                                (propertize "/" 'face 'org-macro))
                                   ".")
                                 (propertize "\n:PROPERTIES:" 'face 'org-drawer)
                                 (format "%s %s"
                                         (propertize
                                          "\n:OPERATION-NAME:"
                                          'face 'org-special-keyword)
                                         insop-name)
                                 (format "%s %s"
                                         (propertize
                                          "\n:SOURCE-NODE-TYPE:"
                                          'face 'org-special-keyword)
                                         src-node-type)
                                 (format "%s %s"
                                         (propertize
                                          "\n:SOURCE-ABSOLUTE-PATH:"
                                          'face 'org-special-keyword)
                                         src-path)
                                 (format "%s %s"
                                         (propertize
                                          "\n:DESTINATION-ABSOLUTE-PATH:"
                                          'face 'org-special-keyword)
                                         dest-path)
                                 (if dest-path-new
                                     (format "%s %s"
                                             (propertize
                                              "\n:DESTINATION-ABSOLUTE-PATH-NEW:"
                                              'face 'org-special-keyword)
                                             dest-path-new)
                                   "")
                                 (format "%s %s"
                                         (propertize
                                          "\n:DESTINATION-NODE-TYPE:"
                                          'face 'org-special-keyword)
                                         dest-node-type)
                                 (if did-error-msg
                                     (format "%s %s"
                                             (propertize
                                              "\n:ERROR-MESSAGE:"
                                              'face 'org-special-keyword)
                                             (propertize did-error-msg 'face 'error))
                                   "")
                                 (propertize "\n:END:" 'face 'org-drawer)
                                 "\n")))))
                (dolist (item op-log)
                  (setq op-attrs item
                        insop-sym (plist-get op-attrs :op-symbol)
                        insop-name (plist-get op-attrs :op-name)
                        src-node-type (plist-get op-attrs :src-node-type)
                        dest-node-type (plist-get op-attrs :dest-node-type)
                        src-path (plist-get op-attrs :src-abs-path)
                        dest-path (plist-get op-attrs :dest-abs-path)
                        dest-path-new (plist-get op-attrs :dest-abs-path-new)
                        path-rellist (plist-get op-attrs :src-relative-list)
                        did-success (plist-get op-attrs :process-succeed-p)
                        did-error-msg (plist-get op-attrs :error-msg))
                  (cond
                   ((eq (car src-node-type) 'dir)
                    (funcall subitem-insert-func "DIRMIRROR " 'nobreak-hyphen 'error))
                   ((eq (car src-node-type) 'file)
                    (funcall subitem-insert-func "FILEMIRROR" 'success 'error))
                   (t
                    (error "[entropy/emacs-do-directory-mirror]: iternal error (wrong type of src-node-type %s)"
                           src-node-type)))))

              (unless use-log-temp-buffer
                (setq buffer-read-only t))

              )))

    ;; ---------- main map process
    (entropy/emacs-list-dir-subdirs-recursively
     srcdir t
     :with-attributes t
     :with-level with-level
     :map-func map-func)

    ;; ---------- Set the summary log message
    (setq op-log-summary-msg
          (format "(%s/%s) dir created/failed, (%s/%s) files did/failed %s operation"
                  dircounts dircounts-error
                  fcounts fcounts-error
                  op-name))

    ;; ---------- generate log buffer/string
    (when op-log
      (cond
       (use-log-buffer
        (let* ((buffer (generate-new-buffer
                        (format " *[entropy/emacs-do-directory-mirror] '%s' to '%s'*"
                                srcdir destdir)
                        t)))
          (with-current-buffer buffer
            (funcall log-buffer-func (current-buffer)))
          (setq op-log-buffer buffer)
          (when need-pop-log-buffer
            (pop-to-buffer buffer))))
       (use-log-string
        (with-temp-buffer
          (funcall log-buffer-func (current-buffer))
          (setq op-log-substr
                (buffer-substring (point-min) (point-max)))))))

    ;; ---------- finally we print the summary log message to stdout
    (message "[%s] Do %s for dir '%s' to '%s' %s (%s)"
             (funcall fatal-or-success-string-get-func)
             op-name
             srcdir destdir
             (if (funcall all-is-success-p-func)
                 "successfully"
               "with fatal")
             op-log-summary-msg)

    ;; ---------- at the end, we return the =common-return=/=rich-return=
    (if op-log
        (progn
          (cond ((or (not pop-log) (eq pop-log t))
                 (funcall all-is-success-p-func))
                (use-log-string
                 (cons (funcall all-is-success-p-func) (cons op-log-substr op-log)))
                (use-log-buffer
                 (cons (funcall all-is-success-p-func) (cons op-log-buffer op-log)))
                (use-log-value
                 (cons (funcall all-is-success-p-func) op-log))
                (t
                 (error "[entropy/emacs-do-directory-mirror] wrong type pop-log type: %s"
                        pop-log))))
      nil)))

(defun entropy/emacs-file-path-parser (file-name type)
  "The file-path for 'entropy-emacs, functions for get base-name,
shrink trail slash, and return the parent(up level) dir.


type:

- 'non-trail-slash':

  Shrink the FILE-NAME path trail slash and return it.

- 'file-name':

  Return the file base name include its suffix type.

- 'parent-dir':

  Return its parent directory path."
  (let (rtn (fname (entropy/emacs-directory-file-name file-name)))
    (cl-case type
      ('non-trail-slash
       (setq rtn fname))
      ('file-name
       (setq rtn
             (file-name-nondirectory fname)))
      ('parent-dir
       (setq rtn (file-name-directory fname))))
    rtn))

(defun entropy/emacs-existed-filesystem-nodes-equal-p
    (filesystem-node1 filesystem-node2 &optional chase-link)
  "Alternative to `file-equal-p' but using `file-attributes' and
`file-remote-p' to distinguish the return.

Return t while thus. Return nil otherwise.

That's say:

Two file in same device and has same indoe number is recognized
as same node. And the same device means that two node must in the
same machine (i.e. host in local filesystem or the same remote
connection return by `file-remote-p') and located in the same
device while each node's hosted device number as same.

When optional argument CHASE-LINK is non-nil then both
FILESYTEM-NODE1 and FILESYTEM-NODE2 are expanded using
`file-truename' before file atrributes compare. So that in this
case symlinks to a same node is also recognize as same node.

Always return nil, when any of FILESYSTEM-NODE1 or
FILESYSTEM-NODE2 is not predicated by
`entropy/emacs-filesystem-node-exists-p'."
  (catch :exit
    (let ((f1-rp (file-remote-p filesystem-node1))
          (f2-rp (file-remote-p filesystem-node2)))
      (cond
       ((and f1-rp f2-rp)
        ;; if two file is all the remote file and in same remote
        ;; connection then we goto section1.
        (if (string= f1-rp f2-rp)
            nil
          ;; otherwise in different remote connection, then they are
          ;; explicitly different
          (throw :exit nil)))
       ;; only one is remote node, then they are explicit different
       ((or f1-rp f2-rp)
        (throw :exit nil)))
      ;; secton1: inode and device number compare in same filesystem
      (when chase-link
        (setq filesystem-node1 (file-truename filesystem-node1)
              filesystem-node2 (file-truename filesystem-node2)))
      (let ((f1-p (entropy/emacs-filesystem-node-exists-p filesystem-node1 t))
            (f2-p (entropy/emacs-filesystem-node-exists-p filesystem-node2 t)))
        (when (and f1-p f2-p)
          (and
           ;; same device judge
           ;;
           ;; FIXME: `file-attribute-device-number' not obey its API
           ;; docstring to return integer value for a remote file.
           (equal
            (file-attribute-device-number f1-p)
            (file-attribute-device-number f2-p))
           ;; same indoe judge
           (=
            (file-attribute-inode-number f1-p)
            (file-attribute-inode-number f2-p))))))))

(defun entropy/emacs-write-file
    (filename &optional confirm)
  "Like `write-file' but create its host place firstly when apply
FILENAME to `file-name-directory' is non-nil"
  (let ((f-host (file-name-directory filename)))
    (when (and f-host
               (not (file-exists-p f-host)))
      (make-directory f-host t))
    (write-file filename confirm)))

(defun entropy/emacs-file-secure-hash (file type hashstr &optional use-native return-curhash)
  "Verify file with hash checker with TYPE supported by
`secure-hash' compared to wanted HASHSTR, return t as verified
and nil otherwise.

If optional arg RETURN-CURHASH is non-nil return the checkout
hash string instead and in whch case the HASHSTR can be omitted.

Using spawn system caller defautly while USE-NATIVE is omitted,
otherwise use `secure-hash' instead (NOTE: do not use native
method while large file, since `secure-hash' use emacs buffer to
store file content in which case system memory will not be enough
to handle the operation.)"
  (unless (file-exists-p file)
    (user-error "Error: file <%s> is not existed!" file))
  (let* (cur-hash
         rtn
         (hashstr (or hashstr ""))
         native-method)
    (setq native-method
          (lambda (f hstr)
            (let* ((inhibit-read-only t))
              (with-temp-buffer
                (insert-file-contents-literally f)
                (setq cur-hash
                      (secure-hash type (current-buffer)))
                (setq rtn
                      (if (string= cur-hash hstr)
                          t nil))))))
    (cond
     (use-native (funcall native-method file hashstr))
     (t
      (let ((type-alist
             '((md5 .
                    (lambda (f)
                      (unless (executable-find "md5sum")
                        (error "No 'md5sum' command found in PATH of current emacs-session"))
                      (car
                       (split-string (shell-command-to-string (format "md5sum %s" (shell-quote-argument f)))
                                     " " t))))
               (sha1 .
                     (lambda (f)
                       (unless (executable-find "sha1sum")
                         (error "No 'sha1sum' command found in PATH of current emacs-session"))
                       (car
                        (split-string (shell-command-to-string (format "sha1sum %s" (shell-quote-argument f)))
                                      " " t))))
               (sha224 .
                       (lambda (f)
                         (unless (executable-find "sha224sum")
                           (error "No 'sha224sum' command found in PATH of current emacs-session"))
                         (car
                          (split-string (shell-command-to-string (format "sha224sum %s" (shell-quote-argument f)))
                                        " " t))))
               (sha256 .
                       (lambda (f)
                         (unless (executable-find "sha256sum")
                           (error "No 'sha256sum' command found in PATH of current emacs-session"))
                         (car
                          (split-string (shell-command-to-string (format "sha256sum %s" (shell-quote-argument f)))
                                        " " t))))
               (sha384 .
                       (lambda (f)
                         (unless (executable-find "sha384sum")
                           (error "No 'sha384sum' command found in PATH of current emacs-session"))
                         (car
                          (split-string (shell-command-to-string (format "sha384sum %s" (shell-quote-argument f)))
                                        " " t))))
               (sha512 .
                       (lambda (f)
                         (unless (executable-find "sha512sum")
                           (error "No 'sha512sum' command found in PATH of current emacs-session"))
                         (car
                          (split-string (shell-command-to-string (format "sha512sum %s" (shell-quote-argument f)))
                                        " " t)))))))
        ;; we must expand the filename to expand ~ like shell magick
        ;; char which will cause messy in which case spwarn process
        ;; will get the wrong(unexpanded) file path.
        (setq file (expand-file-name file))
        (setq cur-hash (funcall (alist-get type type-alist) file))
        (setq rtn
              (if (string= hashstr cur-hash)
                  t
                nil)))))
    (if return-curhash
        cur-hash
      rtn)))

(defun entropy/emacs-simple-backup-file (file-path)
  "Backup file or directory FILE-PATH with named it by the form of
\"{file-name-base}.backup_20180713_Fri_21-28-20.{file-extension-name}\"
which use `file-name-base' to generate '{file-name-base}' and use
`file-name-extension' to generate '{file-extension-name}'.

If the generated backup name exists in filesystem then add a
random suffix before the '{file-extension-name}'.

Notice there's no backup naming regexp convention guarantee! "
  (if (and (stringp file-path)
           (file-exists-p file-path))
      (let* ((file-name
              (if (file-directory-p file-path)
                  (directory-file-name file-path)
                file-path))
             (host-path (file-name-directory file-name))
             (backup-name
              (let ((rtn
                     (concat
                      (expand-file-name (file-name-base file-name) host-path)
                      ".backup_"
                      (format-time-string "%Y%m%d_%a_%H-%M-%S"))))
                (while (file-exists-p rtn)
                  (setq rtn
                        (format "%s_(random_suffix_%s)" rtn
                                (random most-positive-fixnum))))
                (concat rtn "." (file-name-extension file-name))))
             (file-base (file-name-nondirectory file-name))
             (backup-base
              (file-name-nondirectory backup-name)))
        (when (file-exists-p backup-name)
          (error "[internal error] The backup file name has duplicated in host location '%s'"
                 host-path))
        (if (file-directory-p file-path)
            (copy-directory file-path backup-name nil nil t)
          (copy-file file-path backup-name))
        (message (format "Backup '%1$s' to '%2$s'" file-base backup-base)))
    (user-error
     (format "File or directory '%s' doesn't exists, thus can no be backuped!"
             file-path))))

(defvar entropy/emacs-add-filesystem-node-watcher--id-pool 0)
(defvar entropy/emacs-filesystem-node-watcher--register nil)
(cl-defun entropy/emacs-add-filesystem-node-watcher
    (filesystem-node
     func-name idle-delay &rest body
     &key
     do-at-init
     &allow-other-keys)
  "Add a watcher to FILESYSTEM-NODE which do BODY when it is
modified after the first time checking (or did at first time also
when DO-AT-INIT is non-nil).

The =filesystem-node-watcher= is `run-with-idle-timer' repeatly
delay with IDLE-DELAY seconds.

The =filesystem-node-watcher= is named suffixed by FUNC-NAME (a
string).

Return a =filesystem-node-watcher-object= formed as:

#+begin_src elisp
(list
 :guard-id fsysnode-watcher-id
 :guard-name fsysnode-watcher-name
 :guard-for-fsysnode fsysnode-for-watched
 :guard-idle-delay fsysnode-watcher-timer-idle-delay
 :guard-func fsysnode-watcher-function-name
 :guard-timer-var fsysnode-watcher-timer-varname
 :fsysnode-modification-timestamp-var
 fsysnode-watcher-modification-cache-varname
 )
#+end_src

The =filesystem-node-watcher-object= can be used to delete
enabled filesystem-node watcher by
`entropy/emacs-remove-filesystem-node-watcher-core'. You can also
use `entropy/emacs-remove-filesystem-node-watcher' to remove all
the FILESYSTEM-NODE related =filesystem-node-watcher=.

DO-AT-INIT can either a form since its be expanded in the timer func
body.
"
  ;; Using the expanded node name since we must identify it
  ;; `entropy/emacs-filesystem-node-watcher--register'.
  (setq filesystem-node (expand-file-name filesystem-node))
  (let* ((this-id entropy/emacs-add-filesystem-node-watcher--id-pool)
         (timer-func-name
          (intern
           (format "eemacs-filesystem-node-watcher-func/%s--%d"
                   func-name this-id)))
         (modi-var-name
          (intern
           (format "eemacs-filesystem-node-watcher-modification-var/%s--%d"
                   func-name this-id)))
         (timer-var-name
          (intern
           (format "eemacs-filesystem-watcher-timer-var/%s--%d"
                   func-name this-id)))
         (this-do-at-init do-at-init)
         (this-fsysnode filesystem-node)
         (this-idle-delay idle-delay)
         (this-body (entropy/emacs-get-plist-body body))
         (rtn
          `(:guard-for-fsysnode
            ,this-fsysnode
            :guard-id ,this-id
            :guard-name ,func-name
            :guard-idle-delay ,this-idle-delay
            :guard-func ,timer-func-name
            :guard-timer-var ,timer-var-name
            :fsysnode-modification-timestamp-var
            ,modi-var-name))
         (this-body-wrapper
          `(entropy/emacs-unwind-protect-unless-success
               (progn
                 ,@this-body)
             (entropy/emacs-remove-filesystem-node-watcher-core
              ',rtn)))
         )
    (cl-incf entropy/emacs-add-filesystem-node-watcher--id-pool)
    (entropy/emacs-eval-with-lexical
     `(progn
        (defvar ,modi-var-name nil
          ,(format "Filesystem-node [%s] modification timestamp host for =filesystem-node-watcher= `%s'"
                   this-fsysnode timer-func-name))
        (defvar ,timer-var-name nil
          ,(format "Filesystem-node [%s] modification guard timer for =filesystem-node-watcher= `%s'"
                   this-fsysnode timer-func-name))
        (defun ,timer-func-name (&rest _)
          ,(format "=filesystem-node-watcher= for =filesystem-node= [%s], \
using timer `%s' and `%s' as modification timestamp."
                   this-fsysnode timer-var-name modi-var-name)
          (if (entropy/emacs-filesystem-node-exists-p
               ,this-fsysnode)
              (let* ((fattrs (entropy/emacs-get-filesystem-node-attributes
                              ,this-fsysnode))
                     (fmoditime (plist-get fattrs :modification-time)))
                (if (bound-and-true-p ,modi-var-name)
                    (unless (equal fmoditime ,modi-var-name)
                      ,this-body-wrapper)
                  (when ,this-do-at-init
                    ,this-body-wrapper))
                (setq ,modi-var-name fmoditime))
            (setq ,modi-var-name nil)))
        (setq ,timer-var-name
              (run-with-idle-timer ,this-idle-delay
                                   t
                                   #',timer-func-name))))
    (push rtn entropy/emacs-filesystem-node-watcher--register)
    rtn))

(defun entropy/emacs-remove-filesystem-node-watcher-core
    (filesystem-node-watcher-obj)
  "Remove a =filesystem-node-watcher= whose
FILESYSTEM-NODE-WATCHER-OBJ created by
`entropy/emacs-add-filesystem-node-watcher'."
  (let* ((fobj filesystem-node-watcher-obj)
         (ffunc (plist-get fobj :guard-func))
         (fmovar (plist-get fobj :fsysnode-modification-timestamp-var))
         (ftimer-var (plist-get fobj :guard-timer-var))
         (ftimer (symbol-value ftimer-var)))
    (when (timerp ftimer)
      (cancel-timer ftimer))
    (dolist (sym `(,ffunc ,fmovar ,ftimer-var))
      (entropy/emacs-unintern-symbol
       sym))
    (setq entropy/emacs-filesystem-node-watcher--register
          (delete
           fobj
           entropy/emacs-filesystem-node-watcher--register))))

(defun entropy/emacs-remove-filesystem-node-watcher
    (filesystem-node)
  "Remove all =filesystem-node-watcher= created by
`entropy/emacs-add-filesystem-node-watcher' with
FILESYSTEM-NODE."
  (let* ((fsysname (entropy/emacs-directory-file-name
                    (expand-file-name filesystem-node))))
    (dolist (fobj (copy-sequence
                   entropy/emacs-filesystem-node-watcher--register))
      (let ((ffname (plist-get fobj :guard-for-fsysnode)))
        (when (equal fsysname ffname)
          (entropy/emacs-remove-filesystem-node-watcher-core
           fobj))))))

;; *** Process manipulation

(defun entropy/emacs-process-is-running-p (process)
  "Return non-nil when PROCESS is running."
  (member (process-status process)
          '(
            ;; for non-network process
            run
            stop
            ;; for network process
            open listen connect
            ;; TODO: more precision
            )))

(defun entropy/emacs-process-exit-with-fatal-p
    (process &optional sentinel-event-string)
  "Judge whether a PROCESS is ran out with abnormal status. Return
non-nil if thus.

Optional arguments SENTINEL-EVENT-STRING is the event-status
string for normally getted from the PROCESS's sentinel."
  (require 'rx)
  (and
   ;; always return nil when process is running
   (not (entropy/emacs-process-is-running-p process))
   (let* ((proc process)
          (event sentinel-event-string)
          (event-regexp
           (rx (or "deleted" "killed" "core dumped"
                   (regex "exited abnormally with code.*")
                   (regex "failed with code.*")
                   "connection broken by remote peer"
                   ;; TODO: adding more to done as exhaustively
                   )
               (? "\n"))))
     (if event
         (or (not (= 0 (process-exit-status proc)))
             (string-match-p event-regexp event))
       (not (= 0 (process-exit-status proc)))))))

(defun entropy/emacs-process-exit-successfully-p
    (process &optional sentinel-event-string)
  "Judge whether a PROCESS is ran out successfully. Return non-nil
if thus.

Optional arguments SENTINEL-EVENT-STRING is the event-status
string for normally getted from the PROCESS's sentinel."
  (and
   ;; always return nil when process is running
   (not (entropy/emacs-process-is-running-p process))
   (let* ((proc process)
          (event sentinel-event-string)
          (event-regexp
           (rx (or "finished"
                   ;; TODO: adding more to done as exhaustively
                   )
               (? "\n"))))
     (if event
         (or (= 0 (process-exit-status proc))
             (string-match-p event-regexp event))
       (= 0 (process-exit-status proc))))))

(defun entropy/emacs-process-common-filter (&optional buffer-size-restriction without-newline)
  "Common process filter used with some benefit features.

If BUFFER-SIZE-RESTRICTION is non-nil, it must be an positive
integer to restrict the process buffer size i.e erase the buffer
content when its `buffer-size' is larger than it (default is 10M)
before insert the arrived new output. Or if it is `eq' to
'unlimit' then we don't do the default handle.

If WITHOUT-NEWLINE is non-nil, do not insert a newline at the end
of the current output when current output is not trailing with a
newline."
  `(lambda (proc proc-output)
     (let ((proc-buffer (process-buffer proc))
           (bfsr (or ',buffer-size-restriction (* 10 (expt 1024 2))))
           (with-newline (not ',without-newline)))
       (when (and (bufferp proc-buffer)
                  (buffer-live-p proc-buffer))
         (with-current-buffer proc-buffer
           (let ((moving (= (point) (process-mark proc)))
                 (inhibit-read-only t))
             (save-excursion
               ;; Insert the text, advancing the process marker.
               (goto-char (process-mark proc))
               (when (and
                      bfsr
                      (not (eq bfsr 'unlimit))
                      (> (buffer-size) bfsr))
                 (erase-buffer))
               (insert proc-output)
               ;; insert the newline when required as proper occasion
               (when with-newline
                 (unless (save-match-data
                           (looking-at "^[ \t\r\n\v\f ]*$"))
                   (insert "\n")))
               (set-marker (process-mark proc) (point)))
             (when moving (goto-char (process-mark proc)))))))))

(defun entropy/emacs-chained-eemacs-make-proc-args
    (eemacs-make-proc-args-list)
  "Chained sets of `eemacs-make-proc-args-list' one by one ordered
of a list of thus of EEMACS-MAKE-PROC-ARGS-LIST."
  (let* ((llen     (length eemacs-make-proc-args-list))
         (llmax-pt (1- llen))
         (tail-pt  llmax-pt)
         (head-pt  (1- llmax-pt))
         rtn)
    (if (< head-pt 0)
        (car eemacs-make-proc-args-list)
      (while (>= head-pt 0)
        (if tail-pt
            (setq rtn
                  (entropy/emacs-plist-setf
                   (copy-sequence (nth head-pt eemacs-make-proc-args-list))
                   :after
                   `(entropy/emacs-make-process
                     ',(nth tail-pt eemacs-make-proc-args-list))
                   :append-new-key t)
                  tail-pt nil)
          (setq rtn
                (entropy/emacs-plist-setf
                 (copy-sequence (nth head-pt eemacs-make-proc-args-list))
                 :after
                 `(entropy/emacs-make-process
                   ',rtn)
                 :append-new-key t)))
        (setq head-pt (1- head-pt)))
      rtn)))

(defun entropy/emacs-make-process
    (eemacs-make-proc-args)
  "Make a asynchronous process or a synchronous one using the
args in EEMACS-MAKE-PROC-ARGS.

Return the process object when make an asynchronous process.

Introduction of EEMACS-MAKE-PROC-ARGS:

It's an arglist whose partition as the `make-process' arglist, but
combined with `call-process' key-pair factored arglist and further
more flexible process chained key slots.

Used all `make-process' key-slots and must set the slot value
formed like _what to do for directly calling `make-process'_.

Factored `call-process' args as:
- INFILE to :infile
- DESTINATION to :destination
- DISPLAY to :display
- COMMAND to :command
- PROGRAM to use the :command slot's car of what `make-process' requests
- ARGS to use the :command slot's cdr of what `make-process'
  requests

And the slots value injecting form for `call-process' factored key
slots dealing as what mentioned in value form injection for
make-process part.

For chaining the processes in one space, there has a :after key
does for that, thus you can inject any lisp _forms_ into that
place when the head process has ran finished successfully, even
for injecting a new process, otherwise calling the error procedure
by the forms of the key :error. Finally a :prepare key can be set
to indicate whether running the process as a preparation.

If you want to specify the process working directory, set the
value of key slot of :default-directory, the place hold a atom or
a form.

Further more key :synchronously indicate whether call with
synchronously, the place hold a symbol or a single form to be
evaled and use its result to indicate turn/off as that a non-nil
result to turn on. If its result is 't' and current emacs-session
is `noninteractive', then the synchronously method using
`make-process' with spawn watchdog mechanism to emulate
synchronization , otherwise using `call-process' to did the
synchronization, this be presented since the `call-process' have
the termination without kill its spawns problem in emacs
`noninteractive' session like '--batch' mode (see its doc for
details refer the SIGINT and SIGKILL).

If you wish to do sth both for finished or errored status with
`unwind-protect', inject forms to :cleanup slot.

*Interally variable:*

For some occasions, you want to write some procedure with the proc
bindings, thus for this function provide some internally variables
can be used into your form:

1) =$sentinel/proc=
   * description: the process current running
   * limitation: just used for async process
   * Slots support: =:after=, =:cleanup=, =:sentinel=, =:error=

2) =$sentinel/event=
   * description: the process returned event string
   * limitation: just used for async process
   * Slots support: =:after=, =:cleanup=, =:sentinel=, =:error=

3) =$sentinel/destination=
   * description: its a process buffer or for the meaning for the
     of `call-process' =destination= arg when calling process
     synchronously.
   * limitation: both async and sync process calling type
   * Slots support: =:after=, =:cleanup=, =:sentinel=, =:error=
"
  (let ((prepare-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :prepare nil t)
             '(progn t)))
        (after-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :after nil t)
             '(progn t)))
        (error-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :error nil t)
             '(progn t)))
        (clean-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :cleanup nil t)
             '(progn t)))
        (synchronously
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form
           eemacs-make-proc-args :synchronously t t)))
        (default-directory
          (or (entropy/emacs-eval-with-lexical
               (entropy/emacs-get-plist-form
                eemacs-make-proc-args :default-directory t t))
              default-directory))
        ;; make-proc args
        ($make_proc_name
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :name t t)))
        ($make_proc_buffer
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :buffer t t)))
        ($make_proc_command nil)
        ($make_proc_coding
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :coding t t)))
        ($make_proc_noquery
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :noquery t t)))
        ($make_proc_stop
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :stop t t)))
        ($make_proc_connection-type
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :connection-type t t)))
        ($make_proc_filter
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :filter t t)))
        ($make_proc_sentinel
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :sentinel t t)))

        ;; call-process arg
        ($call_proc_destination nil)
        ($call_proc_infile
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :infile t t)))
        ($call_proc_display
         (entropy/emacs-eval-with-lexical
          (entropy/emacs-get-plist-form eemacs-make-proc-args :display t t)))
        ($call_proc_command nil)
        ($call_proc_args nil)

        (thiscur_sync_sym nil)
        thiscur_proc
        thiscur_proc_buffer)

    ;; firstly judge the synchronization type
    (setq thiscur_sync_sym
          (when (and (eq synchronously t)
                     ;; NOTE & FIXME: sleep waiting for async in
                     ;; interaction session may freeze emacs why? and thus
                     ;; we just used this in noninteraction session.
                     (bound-and-true-p noninteractive))
            (entropy/emacs-make-dynamic-symbol-as-same-value
             nil)))

    ;; set var-binding here to prevent duplicate eval
    (let ((cprss-args (entropy/emacs-eval-with-lexical
                       (entropy/emacs-get-plist-form eemacs-make-proc-args :command t t))))
      (setq $make_proc_command cprss-args
            $call_proc_command (car cprss-args)
            $call_proc_args (cdr cprss-args)))
    (setq $call_proc_destination
          (or (entropy/emacs-eval-with-lexical
               (entropy/emacs-get-plist-form eemacs-make-proc-args :destination t t))
              $make_proc_buffer))

    (when (entropy/emacs-eval-with-lexical prepare-form)
      (cond
       ((or (null synchronously)
            thiscur_sync_sym)
        (setq thiscur_proc
              (make-process
               :name $make_proc_name
               :buffer $make_proc_buffer
               :command $make_proc_command
               :coding $make_proc_coding
               :noquery $make_proc_noquery
               :stop $make_proc_stop
               :connection-type $make_proc_connection-type
               :filter $make_proc_filter
               :sentinel
               `(lambda ($sentinel/proc $sentinel/event)
                  (let ((orig-sentinel ,$make_proc_sentinel)
                        ($sentinel/destination (process-buffer $sentinel/proc))
                        (ran-out-p nil))
                    (unwind-protect
                        (unwind-protect
                            ;; run user spec sentinel when pure async run
                            (when (and (functionp orig-sentinel)
                                       (not ',synchronously))
                              (apply orig-sentinel
                                     $sentinel/proc $sentinel/event))
                          ;; run after/error when pure async run
                          (cond ((entropy/emacs-process-exit-successfully-p
                                  $sentinel/proc $sentinel/event)
                                 (setq ran-out-p t)
                                 (unless ',synchronously
                                   ,after-form))
                                ((entropy/emacs-process-exit-with-fatal-p
                                  $sentinel/proc $sentinel/event)
                                 (setq ran-out-p
                                       (list
                                        :exit-code (process-exit-status $sentinel/proc)))
                                 ,error-form)))
                      ;; do ran out procedures
                      (when (and (eq ',synchronously t)
                                 ran-out-p)
                        (setq ,thiscur_sync_sym ran-out-p))
                      (when (and (not ',synchronously)
                                 ran-out-p)
                        ,clean-form)))))

              thiscur_proc_buffer (process-buffer thiscur_proc))

        (when (eq synchronously t)
          (while (null (symbol-value thiscur_sync_sym)) (sleep-for 0.1))
          (entropy/emacs-eval-with-lexical
           `(let (($sentinel/destination ',thiscur_proc_buffer))
              (unwind-protect
                  (when (eq ,thiscur_sync_sym t)
                    ;; just ran after form when this process ran out successfully
                    ,after-form)
                ;; run clean form
                (unwind-protect
                    ,clean-form
                  ;; unintern the temp sync indicator symbol
                  (entropy/emacs-unintern-symbol ',thiscur_sync_sym))))))
        ;; return the processor
        thiscur_proc)
       (t
        (entropy/emacs-eval-with-lexical
         `(let (($sentinel/destination
                 ,$call_proc_destination))
            (unwind-protect
                (if (=
                     (apply 'call-process
                            ,$call_proc_command
                            ,$call_proc_infile
                            ,$call_proc_destination
                            ',$call_proc_display
                            ',$call_proc_args)
                     0)
                    ;; just ran after form when this process ran out successfully
                    ,after-form
                  ,error-form)
              ,clean-form))))))))

(defun entropy/emacs-make-chained-processes (eemacs-make-proc-args-list)
  "Chained batch of processes one by one powered by
`entropy/emacs-make-process' using a list of
=eemacs-make-proc-args= of EEMACS-MAKE-PROC-ARGS-LIST."
  (entropy/emacs-make-process
   (entropy/emacs-chained-eemacs-make-proc-args
    eemacs-make-proc-args-list)))

;; *** Form manipulation
;; **** type spec eval

(defun entropy/emacs-type-spec-eval (eemacs-type-spec)
  "Get value by evaluating the eemacs specified data structure
=eemacs-type-spec= which is an type indicating and flexible data
structure, consisted of two part in generally, i.e. the *car* of _data
type_ and *cdr* of the _structure expression_, each _data type_ can
have several different style of _structure expression_.

*data type*:

- 'EEMACS-DT-IDENTITY': the evaluated result is whatever is given
- 'EEMACS-DT-FUNC'    : the evaluated result is got from the return of specified function
- 'EEMACS-DT-FORM'    : the evaluated result is got from the return of specified elisp form

#+begin_quote
For briefly termnology says that we use *DT* as _data type_ and *exp*
as _structure expression_.
#+end_quote

Each DT's exp is not fixed while extensible but truely back
Compatible. That say the API of this function can be upgrade for
adding more exps for specified DT but not break current convention.

While the exp is the cdr of an =eemacs-type-spec= , thus so, exp
defined within cons style as an symbol or in an single symbol list
style are equivalent most of cases, in which case we treat them as an
single element as the same. Thus we called this type of exp defination
is *single-el* as the term. (e.g. (TYPE . symbol) or (TYPE symbol))

* =IDENTIFY= exp
The exp for DT 'EEMACS-DT-IDENTITY' is arbitrary since its defination, e.g. it
can be an list, an symbol, an string, an number etc. And without the
=single-el= treatment.

* =EEMACS-DT-FUNC= exp

1) If the exp of EEMACS-DT-FUNC DT is an =single-el=, thus the evaluated result
   is the return of the `funcall' of the =single-el= without any
   arguments assignment.

2) Except from (1) occasion, we treat the exp as an commonly list with
   the plist feature i.e. has colon prefixed key as the value hosted
   indicator, and extract some internal defined keys and did
   evalulation according to what is got. Valid keys are:

   - =:predicate=

     The host of an =eemacs-type-spec= to be evaluated by return
     an function which will be applied with =:args= if exists or
     just with funcall with it.

   - =:args=

     The host of the args will apply to the =:predicate=, each arg is
     an =eemacs-type-spec= which will be constructed as an list of
     evalusated value which apply to thus.


* =EEMACS-DT-FORM= exp

The exp is arbitrary and will be involved into an progn form which
will be evaluated as the result to return. But this is good context
readable convention to write the forms after an =:body= key in the
exp, and is useful for this DT further more features, see below:

If an =:sbody= is presented in the exp, then it will concated to
the tail of the =:body= and each item in =:sbody= is an
=eemacs-type-spec= which will be evaluated to be item in the tail
of the =:body= `progn' form. In this case, if no =:body= is
indicated in exp, then any other spec will be ignored.

*Restriction*:

Since we use plist like form to build the data structure in term,
thus if such an exp is used to constructed by that has element as
the same key name of any of the specified exp specification,
please using quote to wrapped it out e.g. `(quote :body)' if not
effect the expection or try use another way to avoid this.
"
  (let ((type (ignore-errors (car eemacs-type-spec)))
        (exp (ignore-errors (cdr eemacs-type-spec)))
        (symbol-mean-p
         (lambda (arg &optional get-val)
           "judge the general mean of symbol in this function"
           (let (rtn use-carp)
             (setq
              rtn
              (or (and (symbolp arg) t)
                  (and (listp arg)
                       (= 1 (length arg))
                       (symbolp (car arg))
                       (setq use-carp t))))
             (if rtn
                 (if get-val
                     (if use-carp
                         (car arg)
                       arg)
                   t)
               nil))))
        (error-type-fatal
         (lambda (type)
           (error (format
                   "\
[error] EEMACS-TYPE-SPEC of <%s> is not recognized for spec '%s'"
                   eemacs-type-spec type))))
        )
    (cond
     ;; function type
     ((eq type 'EEMACS-DT-FUNC)
      (cond
       ((or (funcall symbol-mean-p exp)
            (functionp exp))
        (if (functionp exp)
            (funcall exp)
          (funcall
           (funcall symbol-mean-p exp t))))
       ((listp exp)
        (let ((args
               (mapcar
                (lambda (eemacs-type-spec)
                  (entropy/emacs-type-spec-eval
                   eemacs-type-spec))
                (entropy/emacs-get-plist-form
                 exp :args 'list t)))
              (predicate
               (entropy/emacs-type-spec-eval
                (entropy/emacs-get-plist-form
                 exp :predicate 'car t))))
          (unless (functionp predicate)
            (error
             (format "[error] EEMACS-TYPE-SPEC of <EEMACS-DT-FUNC> \
whose predicate '%s' is not an function"
                     predicate)))
          (apply predicate args)))
       (t
        (funcall error-type-fatal 'EEMACS-DT-FUNC))))
     ;; form type
     ((eq type 'EEMACS-DT-FORM)
      (let ((body (entropy/emacs-get-plist-form exp :body 'list t))
            (sbody (entropy/emacs-get-plist-form exp :sbody 'list t))
            form-get)
        (if (and (null body)
                 (null sbody))
            (setq form-get (append (list 'progn) exp))
          (setq form-get
                (if body
                    (append (list 'progn)
                            body)))
          (when sbody
            (setq form-get
                  (append
                   form-get
                   (mapcar
                    (lambda (eemacs-type-spec)
                      (entropy/emacs-type-spec-eval
                       eemacs-type-spec))
                    sbody)))))
        (entropy/emacs-eval-with-lexical form-get)))
     ;; identity type
     ((eq type 'EEMACS-DT-IDENTITY)
      exp)
     (t
      (error
       "%s"
       (format "ERROR: EEMACS-TYPE-SPEC detected fatal of spec %s"
               eemacs-type-spec))))))

;; **** `eval-after-load' batch port
(defvar entropy/emacs--eval-after-load-log nil
  "A list of feature mapped `entropy/emacs-eval-after-load'
generated `lambda' or closures used to debug for view whether
some context is useing `lexical-binding' environemnt. (only
enabled when `entropy/emacs-startup-with-Debug-p' is set)")
(defmacro entropy/emacs-eval-after-load (feature &rest body)
  "Wrap BODY into FEATURE using `eval-after-load'.

Feature is can be a FILE arg of `eval-after-load' or a list of
that.

BODY is wrapped into a `lambda', it is defined with lexical
bindings of the stack context who calling this macro but only
when `lexical-binding' is enabled in that context, otherwise it
is just a anonymous no arguments function.

Return that BODY function"
  (declare (indent defun))
  (let ((forms-sym        (make-symbol "forms"))
        (feature-sym      (make-symbol "feature"))
        (body-func-sym    (make-symbol "body-func"))
        (extitem-func-sym (make-symbol "extract-item-func")))
    `(let (,forms-sym
           (,feature-sym ,feature)
           (,body-func-sym (list 'function (lambda nil ,@body)))
           (,extitem-func-sym
            (lambda (file)
              (if (stringp file)
                  file
                (list 'quote file)))))
       (cond ((not (listp ,feature-sym))
              (setq ,forms-sym
                    (list 'eval-after-load (funcall ,extitem-func-sym ,feature-sym)
                          ,body-func-sym)))
             ((and (listp ,feature-sym)
                   (= 1 (length ,feature-sym)))
              (setq forms
                    (list 'eval-after-load (funcall ,extitem-func-sym (car ,feature-sym))
                          ,body-func-sym)))
             ((and (listp ,feature-sym)
                   (> (length ,feature-sym) 1))
              (setq ,feature-sym (reverse ,feature-sym)
                    ,forms-sym
                    (list 'eval-after-load (funcall ,extitem-func-sym (car ,feature-sym))
                          ,body-func-sym))
              (dolist (el (cdr ,feature-sym))
                (setq ,forms-sym
                      (list 'eval-after-load (funcall ,extitem-func-sym el)
                            (list 'quote ,forms-sym))))))
       (entropy/emacs-eval-with-lexical ,forms-sym)
       (when entropy/emacs-startup-with-Debug-p
         (push (list ,feature
                     :lexical-bind-p lexical-binding
                     :functype (car (cadr ,body-func-sym))
                     :lexical (cadr (cadr ,body-func-sym))
                     :load-fname load-file-name)
               entropy/emacs--eval-after-load-log))
       ,body-func-sym)))

(defvar entropy/emacs-eval-after-load-unique-register nil)
(defmacro entropy/emacs-eval-after-load-unique (name-symbol feature &rest body)
  "Like `entropy/emacs-eval-after-load' but stick the BODY into an
function named as unique as NAME-SYMBOL so that the lazy invoking of
any FEATURE that related to NAME-SYMBOL didn't calling all lazy forms
defined for it instead of just invoke that function which be redefined
every time when BODY is different as the function's history has.

The relationship of NAME-SYMBOL and FEATURE is not bound tight fixed,
since we only stick BODY with NAME-SYMBOL, therefore any FEATURE with
the same NAME-SYMBOL is shared one lazy function defination."
  (let* ((func-name (intern
                     (format "entropy/emacs-eval-after-load-unique--/%s/funcname"
                             name-symbol)))
         (indicate-varname
          (intern
           (format "entropy/emacs-eval-after-load-unique--/%s/indc-varname"
                   name-symbol)))
         (registed-p
          (and (member func-name entropy/emacs-eval-after-load-unique-register)
               t))
         (has-ran-p
          (and (boundp indicate-varname)
               (symbol-value indicate-varname)
               t)))
    `(progn
       (if ,has-ran-p
           (progn ,@body)
         (defvar ,indicate-varname nil
           ,(format "The ran out indicator for func `%s'"
                    func-name))
         (defun ,func-name (&rest _)
           ,(format "The unique `eval-after-load' injected function for feature '%S' generated by \
`entropy/emacs-eval-after-load-unique' with name of '%s'"
                    feature name-symbol)
           (progn
             ,@body
             (setq ,indicate-varname t)
             (fmakunbound ',func-name)))
         (unless ,registed-p
           (push ',func-name entropy/emacs-eval-after-load-unique-register)
           (entropy/emacs-eval-after-load ,feature
             (funcall #',func-name)))))))

;; **** Range form generation
;; ***** generate symbol/string from range description

(defun entropy/emacs-generate-symbols-or-strings-from-range-desc
    (this-range-descs &optional make-symbol concat concat-separater)
  "Generate list of symbols or strings from RANGE-DESCS.

RANGE-DESCS is a list of RANGE-DESC which formed as one of below:

``` elisp
(:type collection
 :fmstr \"l0_%s\"
 :range-descs ((:type number_range     :range (1 . 100))
               (:type char_range       :range (65 . 90))
               (:type func             :func funcname :argslist (arg1 arg2 arg3 ...))
               (:type enum             :enum_str_list (el1 el2 el3 ...)))
 :sep \".\")

(:type number_range :fmstr \"l1_%s\" :range (10 . 90)   :sep \"...\")

(:type char_range   :fmstr \"l2_%s\" :range (115 . 200) :sep \"___\")

(:type enum
 :enum_str_list (l3_0 l3_1 l3_2 ...)
 :sep \" \")

(:type func
 :func funcname :argslist (arg1 arg2 ...)
 :sep \"---\"
```
when MAKE-SYMBOL is non-nil we export a list of symbol, otherwise list of string.

In list of string return type, when CONCAT is non-nil we return a
string concated with each string of the list of string optional with
separater CONCAT-SEPARATER when non-nil.
"
  (let* (sparse-list rtn
         (gen/from/number_range
          (lambda (range fmstr sep)
            (cl-loop for var from (car range) to (cdr range)
                     collect (format "%s%s" (format (or fmstr "%s") (number-to-string var))
                                     (or sep "")))))
         (gen/from/char_range
          (lambda (range fmstr sep)
            (cl-loop for var from (car range) to (cdr range)
                     collect (format "%s%s" (format (or fmstr "%s") (format "%c" var))
                                     (or sep "")))))
         (gen/from/func
          (lambda (func sep &rest args)
            (mapcar (lambda (str) (format "%s%s" str (or sep "")))
                    (apply func args))))

         (gen/from/enum_str_list
          (lambda (enum_str_list fmstr sep)
            (mapcar (lambda (str)
                      (format "%s%s" (format (or fmstr "%s") str) (or sep "")))
                    enum_str_list)))

         (gen/from/core-func
          (lambda (subrange)
            (let ((this-type (plist-get subrange :type)))
              (cond ((eq this-type 'number_range)
                     (funcall gen/from/number_range
                              (plist-get subrange :range)
                              (plist-get subrange :fmstr)
                              (plist-get subrange :sep)))
                    ((eq this-type 'char_range)
                     (funcall gen/from/char_range
                              (plist-get subrange :range)
                              (plist-get subrange :fmstr)
                              (plist-get subrange :sep)))
                    ((eq this-type 'func)
                     (funcall gen/from/func
                              (plist-get subrange :func)
                              (plist-get subrange :sep)
                              (plist-get subrange :argslist)))
                    ((eq this-type 'enum)
                     (funcall gen/from/enum_str_list
                              (plist-get subrange :enum_str_list)
                              (plist-get subrange :fmstr)
                              (plist-get subrange :sep)))))))

         (gen/from/collection_type
          (lambda (range-descs fmstr sep)
            (let (group-rtn)
              (dolist (subrange range-descs)
                (setq group-rtn
                      (append group-rtn
                              (funcall gen/from/core-func subrange))))
              (mapcar (lambda (str) (format "%s%s" (format (or fmstr "%s") str) (or sep "")))
                      group-rtn))))

         (concat-func
          (lambda (str-list1 str-list2)
            (let (rtn)
              (dolist (el1 str-list1)
                (dolist (el2 str-list2)
                  (setq rtn (append rtn (list (concat el1 el2))))))
              rtn))))

    (dolist (el this-range-descs)
      (setq sparse-list
            (append sparse-list
                    (list
                     (cond ((eq (plist-get el :type) 'collection)
                            (funcall gen/from/collection_type
                                     (plist-get el :range-descs)
                                     (plist-get el :fmstr)
                                     (plist-get el :sep)))
                           (t
                            (funcall gen/from/core-func el)))))))

    (let ((count 0))
      (while sparse-list
        (let ((head-group (pop sparse-list))
              sub-group)
          (when (and (= 0 count) sparse-list)
            (setq sub-group
                  (pop sparse-list)))
          (cond (sub-group
                 (setq rtn (funcall concat-func head-group sub-group)))
                (t
                 (if (null rtn)
                     (setq rtn head-group)
                   (setq rtn (funcall concat-func rtn head-group)))))
          (cl-incf count))))
    ;; return
    (if make-symbol
        (mapcar (lambda (str)
                  (make-symbol str))
                rtn)
      (cond (concat
             (let (str-rtn)
               (mapc (lambda (str)
                       (setq str-rtn
                             (concat (or str-rtn "")
                                     (if str-rtn
                                         (or concat-separater "")
                                       "")
                                     str)))
                     rtn)
               str-rtn))
            (t
             rtn)))))

;; *** Buffer manipulation
;; **** Basic

(defmacro entropy/emacs-use-marker-position (place)
  "`setf' PLACE with its position value only when PLACE is
`markerp' marker."
  (let ((val-sym (make-symbol "the-value")))
    `(let ((,val-sym ,place))
       (if (markerp ,val-sym)
           (setf ,place
                 (marker-position ,val-sym))))))

(defmacro entropy/emacs-use-markers-position (&rest places)
  "Batch place of PLACES do `entropy/emacs-use-marker-position'."
  (let (form)
    (dolist (place places)
      (push (macroexpand-1 `(entropy/emacs-use-marker-position ,place))
            form))
    `(progn ,@(nreverse form))))

(cl-defun entropy/emacs-point-min (&optional without-restriction)
  "Like `point-min' but return the `point-min' without buffer
restriction i.e. `buffer-narrowed-p' when WITHOUT-RESTRICTION is
set.

See also `entropy/emacs-point-max'."
  (if (and without-restriction
           (buffer-narrowed-p))
      (save-restriction
        (widen)
        (point-min))
    (point-min)))

(cl-defun entropy/emacs-point-max (&optional without-restriction)
  "Like `point-max' but return the `point-max' without buffer
restriction i.e. `buffer-narrowed-p' when WITHOUT-RESTRICTION is
set.

See also `entropy/emacs-point-min'."
  (if (and without-restriction
           (buffer-narrowed-p))
      (save-restriction
        (widen)
        (point-max))
    (point-max)))

(cl-defun entropy/emacs-buffer-position-p
    (position &key do-error with-range-check without-restriction)
  "Return non-nil when POSITION is a vaid buffer position of
`current-buffer'.

When DO-ERROR is set, do `error' like
`entropy/emacs-do-error-for-buffer-position-invalid' when
POSITION is invalid.

Optional keys WITH-RANGE-CHECK and WITHOUT-RESTRICTION has same
meaning of `entropy/emacs-do-error-for-buffer-position-invalid'."
  (let ((thefunc
         (lambda ()
           (funcall
            'entropy/emacs-do-error-for-buffer-position-invalid
            position
            :with-range-check with-range-check
            :without-restriction without-restriction))))
    (if do-error
        (funcall thefunc)
      (ignore-errors (funcall thefunc)))))

(cl-defun entropy/emacs-pos-within-buffer-portion-p
    (position &optional without-restriction)
  "Return non-nil when POSITION is within the BUFFER visible
portion i.e. POSITION is `>=' BUFFER's `point-min' and `<='
BUFFER's `point-max'.

POSITION is same as the arg of `goto-char' and must be prediated
by `entropy/emacs-buffer-position-p'.

BUFFER is commonly the `current-buffer' but be the
`marker-buffer' when position is a marker and the value got by
`marker-buffer' is non-nil.

When WITHOUT-PROPERTIES is set, then compare with the whole
buffer portion without any restrictions i.e. `buffer-narrowed-p'.
"
  (let* ((posmk-p (markerp position))
         (mkbuff (and posmk-p
                      (marker-buffer position)))
         (pt (if posmk-p (marker-position position) position))
         ptmin ptmax)
    (with-current-buffer (or mkbuff (current-buffer))
      (save-restriction
        (entropy/emacs-widen-when
         :when without-restriction
         (setq ptmin (point-min)
               ptmax (point-max))
         (entropy/emacs-buffer-position-p
          pt :do-error t)
         (and
          (>= pt ptmin)
          (<= pt ptmax)))))))

(defun entropy/emacs-goto-char (position)
  "Like `goto-char' but this function guaranteeing that goto the
POSITION's point in `current-buffer' even if POSITION is a maker
in another buffer.

POSITION must be prediated by `entropy/emacs-buffer-position-p'
with `current-buffer''s visible portion or an error will be
throwed out."
  (let* ((posmk-p (markerp position))
         (pt (if posmk-p (marker-position position) position))
         (_ (entropy/emacs-buffer-position-p
             pt :do-error t :with-range-check t)))
    (goto-char pt)))

(cl-defmacro entropy/emacs-with-current-buffer
    (buffer-or-name &rest body
                    &key
                    use-switch-directly
                    &allow-other-keys)
  "Same as `with-current-buffer' but run BODY after
`switch-to-buffer' and `set-buffer' to BUFFER-OR-NAME when
USE-SWITCH-DIRECTLY is non-nil, therefore do no use
USE-SWITCH-DIRECTLY when in a `noninteractive' session.

When USE-SWITCH-DIRECTLY is non-nil and other than 't', it should
be a list of rest args exclude the first argument i.e. the
BUFFER-OR-NAME of `switch-to-buffer' used to apply to
`switch-to-buffer'."
  (declare (indent 1) (debug t))
  (let ((body (entropy/emacs-get-plist-body body))
        (bforbn-sym (make-symbol "buffer-or-name"))
        (swtd-sym   (make-symbol "use-switch-directly")))
    `(let ((,bforbn-sym ,buffer-or-name)
           (,swtd-sym ,use-switch-directly))
       (if ,swtd-sym
           (progn
             (if (eq ,swtd-sym t)
                 (switch-to-buffer ,bforbn-sym)
               (apply 'switch-to-buffer ,bforbn-sym ,swtd-sym))
             (set-buffer ,buffer-or-name)
             ,@body)
         (with-current-buffer ,buffer-or-name
           ,@body)))))

(cl-defmacro entropy/emacs-with-goto-char
    (number-or-marker &rest body
                      &key save-excursion use-switch-directly
                      &allow-other-keys)
  "Run BODY after `goto-char' to NUMBER-OR-MARKER of the
BUFFER (use NUMBER-OR-MARKER's buffer when it's a marker and its
`marker-buffer' is non-nil or use `current-buffer') and the whole
procedure is with that BUFFER (i.e. run with the `current-buffer'
set to BUFFER temporarily unless USE-SWITCH-DIRECTLY is non-nil
see below). Return the result of last form of BODY.

When SAVE-EXCURSION is non-nil, try to restore the point of the
BUFFER after run BODY when possible.

When USE-SWITCH-DIRECTLY is non-nil, it has same meaning of the
same key of `entropy/emacs-with-current-buffer', otherwise
restore the former `current-buffer' which presented before whole
procedure.
"
  (declare (indent defun))
  (let ((body (entropy/emacs-get-plist-body body))
        (pt-sym     (make-symbol "number-or-marker"))
        (mkbuf-sym  (make-symbol "the-marker-buffer"))
        (swtd-sym   (make-symbol "use-switch-directly"))
        (sves-sym   (make-symbol "use-save-excursion"))
        (buff-sym   (make-symbol "the-buffer")))
    `(let* ((,pt-sym ,number-or-marker)
            (,mkbuf-sym (and (markerp ,pt-sym)
                             (marker-buffer ,pt-sym)))
            (,swtd-sym ,use-switch-directly)
            (,sves-sym ,save-excursion)
            (,buff-sym (or ,mkbuf-sym (current-buffer))))
       (entropy/emacs-with-current-buffer ,buff-sym
         :use-switch-directly ,swtd-sym
         (entropy/emacs-save-excursion-when
          :when ,sves-sym
          (goto-char ,pt-sym)
          ,@body)))))

(cl-defun entropy/emacs-buffer-goto-line
    (line-number &key buffer relative)
  "Move to line with LINE-NUMBER of buffer BUFFER (default to
`current-buffer'), if RELATIVE is non-nil move to visible line
with LINE-NUMBER of the beginning of buffer visible part.

LINE-NUMBER should be a integer or an error will be throwed out.

When buffer is narrowing, and RELATIVE is nil, then the BUFFER
will be `widen'.

If LINE-NUMBER is larger or lesser than the BUFFER's visible
part, goto the last/first line of the BUFFER visible part.

The end form point is always at the beginning of the target
buffer line."
  (unless (integerp line-number)
    (signal 'wrong-type-argument
            (list 'integerp "line-number" line-number)))
  (let ((buff (or buffer (current-buffer))))
    (with-current-buffer buff
      (entropy/emacs-widen-when
       :when relative
       (let* ((cur-lnum  (line-number-at-pos))
              (lnum      (max 0 line-number))
              (lnum-diff (- lnum cur-lnum)))
         (forward-line lnum-diff)
         (forward-line 0))))))

(defun entropy/emacs-get-buffer-region-markers
    (start end &optional buffer)
  "Make and return a cons of car of the marker of start START and
cdr of the marker of end END in buffer BUFFER (omitted for using
`current-buffer').

When START is `<' than END, the returned cons is reversed to obey
the start-to-end order.

START and END must be predicated by
`entropy/emacs-buffer-position-p' with BUFFER's visible portion
range check. When either START or END is a marker, only its
`marker-position' is used.
"
  (with-current-buffer (or buffer (current-buffer))
    (dolist (pt `(,start ,end))
      (entropy/emacs-buffer-position-p pt :do-error t :with-range-check t))
    (entropy/emacs-use-markers-position start end)
    (let (start-mk end-mk)
      (save-excursion
        (setq start-mk (set-marker (make-marker) start)
              end-mk (set-marker (make-marker) end))
        (entropy/emacs-swap-two-places-value start-mk end-mk
          (> start-mk end-mk)))
      (cons start-mk end-mk))))

(cl-defun entropy/emacs-get-buffer-region-from-pos
    (start-offset
     end-offset
     &key
     buffer position
     from-line-begin
     from-line-end
     return-as-marker)
  "Get a region cons of '(start . end) offset based on position POSITION
(omitted for using `point')' `+' the integer START-OFFSET and
END-OFFSET in buffer BUFFRE (omitted for using `current-buffer')
respectively.

POSITION must be predicated by `entropy/emacs-buffer-position-p' with
BUFFER's visible range check, or an error will be throwed out.

If FROM-LINE-BEGIN is non-nil, the based position is the
`line-beginning-position' of the line of position.

If FROM-LINE-END is non-nil, the based position is the
`line-end-position' of the line of position.

If both FROM-LINE-BEGIN and FROM-LINE-END are non-nil, the based
positions is arranged to start and end respectively of:
- start: `line-beginning-position'
- end  : `line-end-position'

If the calcualted reigon's start or end point is out of range of the
BUFFER's visible portion, an error is occurred.

If RETURN-AS-MARKER is non-nil, then the car and cdr of the return are
both a marker `=' start or end respectively.

The returned cons of start and end is reversed when start's point is
`>' then end's point."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (if position
          (entropy/emacs-goto-char position)
        (setq position (point)))
      (let* ((lnbeg-pt (line-beginning-position))
             (lnend-pt (line-end-position))
             begpt endpt)
        (cond ((and from-line-begin from-line-end)
               (setq begpt (max 1 (+ lnbeg-pt start-offset))
                     endpt (max 1 (+ lnend-pt end-offset))))
              (from-line-begin
               (setq begpt (max 1 (+ lnbeg-pt start-offset))
                     endpt (max 1 (+ lnbeg-pt end-offset))))
              (from-line-end
               (setq begpt (max 1 (+ lnend-pt start-offset))
                     endpt (max 1 (+ lnend-pt end-offset))))
              (t
               (setq begpt (max 1 (+ position start-offset))
                     endpt (max 1 (+ position end-offset)))))
        (dolist (pt (list begpt endpt))
          (entropy/emacs-buffer-position-p pt :do-error t :with-range-check t))
        (entropy/emacs-swap-two-places-value begpt endpt
          (< endpt begpt))
        (if return-as-marker
            (entropy/emacs-get-buffer-region-markers begpt endpt)
          (cons begpt endpt))))))

(cl-defun entropy/emacs-get-buffer-pos-line-content
    (&key start-offset end-offset buffer position without-properties)
  "Return substring of buffer BUFFER at the line of position
POSITION with `line-beginning-position' to `line-end-position' as
default region. Return nil when the default region is empty.

When BUFFER is omitted, use `current-buffer' as default.

When POSITION is omitted use `point' in BUFFER as
default. POSITION must be predicated by
`entropy/emacs-buffer-position-p' with BUFFER's visible portion
check or a error will be throwed out.

When WITHOUT-PROPERTIES is non-nil, trim all text properties of the
return.

When either START-OFFSET or END-OFFSET is non-nil, it should be a
integer used to calculate the position offset by
`line-beginning-position' and `line-end-position' respectively and use
that region with error when the calculated region is wider than the
line area. Also return nil when the calculated region is empty.
"
  (let ((buffer (or buffer (current-buffer)))
        (pos position)
        (extfunc (if without-properties 'buffer-substring-no-properties
                   'buffer-substring))
        begpt endpt region regbeg regend lnbegpt lnendpt)
    (with-current-buffer buffer
      (save-excursion
        (and pos
             (entropy/emacs-goto-char pos))
        (setq lnbegpt (line-beginning-position)
              lnendpt (line-end-position)
              start-offset (or start-offset 0)
              end-offset  (or end-offset  0)))
      (setq region (entropy/emacs-get-buffer-region-from-pos
                    start-offset end-offset
                    :from-line-begin t
                    :from-line-end t)
            regbeg (car region)
            regend (cdr region)
            begpt (or (and (>= regbeg lnbegpt) (<= regbeg lnendpt) regbeg)
                      (error "begin point out of current line's line-begin point"))
            endpt (or (and (>= regend lnbegpt) (<= regend lnendpt) regend)
                      (error "end point out of current line's line-end point")))
      (funcall extfunc begpt endpt))))

(cl-defun entropy/emacs-buffer-delete-line-at-pos
    (&key
     keep-relpos start-offset
     preserve-newline
     bound without-bound-when-bound-at-line-end
     return-region-string
     with-result-debug)
  "Delete the `current-buffer''s line's full content or portion at `point'.

This function defaultly do operation =delete-whole-line= when current
=buffer-line='s =line-end-point= is =newline= or do
=delete-whole-line-but-eobp= otherwise.

When some options set, do =delete-line-portion= when proper see
*OPTIONS*.

Whatever whether or not to use option, this function calculates a
default =pre-delete-region= of the =buffer-line= first. And the
default =pre-delete-region= is equal to the =region= of a non =eobp=
ended =buffer-line=.

(See *Terminogies in this commentary* for keyword explanation.)

* OPTIONS:

- START-OFFSET: when set, it should be a `natnump' number . Used to
  skip N points from the `line-beginning-position' of current line and
  set that `point' as the =region-start-point= of the defualt
  =pre-delete-region= i.e. the calculation is:
  : =region-start-point= = (+ START-OFFSET  =line-start-point=)

  When =region-start-point= is not less than =line-end-point= then
  trigger =do-nothing=.

  When it's larger than 0, always do =delete-line-portion=.

  Defautls to 0.

- PRESERVE-NEWLINE: when set, when =line-end-point= is a =newline=
  then it will never be deleted. In which case always do a
  =delete-line-portion= and it is =set-with-useless= when the current
  =buffer-line= is the last line of `current-buffer' or BOUND is
  =set-is-effective=.

- BOUND: when set, it should be a `point' in current buffer, trigger
  =do-nothing= when its overflow. It is used to restrict the default
  =pre-delete-region='s =region-end-point= which defaults to the
  =region-end-point= of the =buffer-line=. Trigger =do-nothing= when
  it is less or equal then =region-start-point= of the default
  =pre-delete-region=.

  When it's larger than the =region-end-point= of the default
  =pre-delete-region=, it is =set-with-useless=.

  When it's less than =region-end-point= of the default
  =pre-delete-region=, always do =delete-line-portion= unless
  WITHOUT-BOUND-WHEN-BOUND-AT-LINE-END is =set-is-effective=.

- WITHOUT-BOUND-WHEN-BOUND-AT-LINE-END: when set, reset the BOUND
  option to =set-default= when BOUND is same as the =line-end-point=
  of the current =buffer-line=. In which case, this function can do
  =delete-whole-line= when proper occasion instead of being effected
  by BOUND since the BOUND at =newline= is usually useless.

- RETURN-REGION-STRING: see *Return*.

- KEEP-RELPOS: when set and only with do the =delete-normal=, forward
  the `point' to the original previous =buffer-line='s
  =line-start-point= unless the current =buffer-line= is first line of
  `current-buffer' in which case it is useless without any effection
  and use the default behaviour (see *Default behaviour after
  deletion*).

- WITH-RESULT-DEBUG: EEMACS_MAINTENANCE internally debug usage, do not
  used as common.

* Default behaviour after deletion

For =delete-whole-line=, put the `point' at the =line-start-point= of
the original next =buffer-line=.

For =delete-line-portion=, put the `point' at the =region-start-point=
of the final =pre-delete-region=, also for when the
=delete-line-portion= is a =delete-whole-line-but-eobp=.

* Return

This function return a plist to describe the successful operation
result or nil when =do-nothing= is triggerred.

The successful result plist has follow valid keys:

1) ':type-of-deletion' :
   - 0: the deletion operation is =delete-whole-line=
   - 1: the deletion operation is =delete-whole-line-but-eobp=
   - 2: the deletion operation is =delete-line-portion=

2) ':type-of-operation-after-deletion' :
   - '0' : indicate the result of default final operation of
     =delete-whole-line=.
   - '1': indicate the result of final operation of
     =delete-line-portion= (include =delete-whole-line-but-eobp=
     unless when KEEP-RELPOS is set and it is in effectively).
   - '2' : indicate the result when KEEP-RELPOS is set and in
     effectively.
3) ':deleted-content-string' : when RETURN-REGION-STRING is set, it is
   the actually deleted string without trim its text properties or nil
   otherwise.

* Terminogies in this commentary:

- =newline=: A new line is a `point' whose content is a `C-j'.

- =eobp=: A `point' whose content is `eobp'. It is equal to
  `point-max'.

- =line-end-point=: A `point' who is either a =newline= or a =eobp=.

- =line-start-point=: A `point' who is not a =newline=.

- =region=: A area of the buffer which use two `point' to described
  i.e. the =region-start-point= and =region-end-point= where the
  former's is always less or equal to the latter. When the former and
  latter is `=' then we say this =region= is empty.

- =region-contents= : a number to describe how many points a =region=
  has.  A =region= has points start from =line-start-point= (include)
  and end to the =line-end-point= (exclude).

- =buffer-line=: A buffer line is a logical portion of the whole emacs
  buffer content. Usually it has a start `point' which is a
  continuation of a =line-end-point= and end with a
  =line-end-point=.

  When its =line-start-point='s position is `eq' to `point-min' then
  it is the first line of the buffer.

  When its =line-end-point= is a =eobp= then it is the last line of
  the buffer.

  Thus a =buffer-line= can be both a first and a last line of the
  buffer therefore it is the only line in the buffer.

  If a =buffer-line='s =line-start-point= is same as its
  =line-end-point= then we say this line is empty.

  Thus a =buffer-line= is also a =region= whose =region-start-point=
  is =line-start-point= and its =region-end-point= is =line-end-point=
  plus one or is =line-end-point= when its =line-end-point= is =eobp=.

- =pre-delete-region=: a =region= that prepared to be deleted.

- =delete-whole-line=: A operation delete whole points' content of a
  =buffer-line=. When the =buffer-line='s end point is `eobp' this
  =buffer-line= can not be operated by this kind of operation since we
  can not delete a `eobp' (see =delete-whole-line-but-eobp=).

- =delete-line-portion=: A operation that delete a portion of a
  =buffer-line= which is a against operation of
  =delete-whole-line=. In other words, any non empty =region= whose
  =region-start-point= larger or equal to =line-start-point= and its
  =region-end-point= is less than =line-end-point= then its contents
  can be deleted by this operation.

- =delete-whole-line-but-eobp=: a kind of =delete-line-portion=
  operation which delete all contents of a =buffer-line= unless its
  =line-end-point= is a =eobp= in which case its =line-start-point= is
  not deleted. (why of see =delete-whole-line=).

- =delete-normal=: do =delete-whole-line= or =delete-whole-line-but-eobp=.

- =do-nothing= : a operation that let this function do not do
  anything and return nil.

- =set-default=: the default value of a option.

- =set-with-useless=: indicate a option set as it is not set i.e. not
  used.

- =set-is-effective=: indicate a option set is useful i.e. in
  effectively in this function procedure.
"
  (let* (deletion-type-flag
         (deltype-alist '((0 . "delete-whole-line")
                          (1 . "delete-whole-line-but-eobp")
                          (2 . "delete-line-portion")))
         final-optype-flag
         (fnloptype-flag '((0 . "origin next line begin")
                           (1 . "at start of deletion's region")
                           (2 . "origin previous line begin")))
         reg-content
         (start-offset
          (or
           (and
            (and start-offset
                 (or (setq start-offset
                           (entropy/emacs-natural-number-p
                            start-offset :exclude-zero nil :detect-float t :convert-float t))
                     (error "start offset must be a natural number")))
            start-offset)
           0))
         (cur-pt              (point))
         (cur-lbegpt          (line-beginning-position))
         (cur-lendpt          (line-end-position))
         (cur-start-edgept    (+ start-offset cur-lbegpt))
         ;; if the calculated region start is not in the current line and it's meaningless
         (fatalp              (or (> cur-start-edgept cur-lendpt) ;its ok if beg equal to lend since we support empty line
                                  (<  cur-start-edgept cur-lbegpt))))
    (unless fatalp
      (let* ((cur-ptmin           (point-min))
             (cur-ptmax           (point-max))
             (cur-assume-edgept   (1+ cur-lendpt))
             (_                   (when (and bound
                                             without-bound-when-bound-at-line-end
                                             (= bound cur-lendpt))
                                    (setq bound nil)))
             (bound               (or bound (min cur-assume-edgept cur-ptmax)))
             (fatalp              (or
                                   ;; bound should not same/larger as/than region start pos or it's meaningless
                                   (>= cur-start-edgept bound)
                                   ;; bound should within the buffer visible portion or it's meaning less
                                   (> bound cur-ptmax))))
        (unless fatalp
          (let* (
                 ;; the region end pt must at neareast place the
                 ;; assumed end pt at and never overflow the boundary.
                 (cur-end-edgept      (if (> cur-assume-edgept bound) bound cur-assume-edgept))
                 (_                   (when (and preserve-newline (= cur-end-edgept cur-assume-edgept))
                                        (setq cur-end-edgept (1- cur-end-edgept))))
                 (cur-endeobp         (= cur-ptmax cur-end-edgept))
                 ;; if the calculated region end pt is less than the
                 ;; assumed end pt or the region start offset larger
                 ;; than 0 i.e. forward to the line begin pos then
                 ;; it's within the assumed region will be deleted so
                 ;; we do not do any further plans.
                 (reg-iswithin-p      (or (< cur-end-edgept cur-assume-edgept)
                                          (> start-offset 0)))
                 ;; or we can use assumed region to do deletion and do
                 ;; other plans.
                 (delete-wholeln-p    (not reg-iswithin-p))
                 (delete-wholeln-buteob-p (and (= start-offset 0) cur-endeobp))
                 (delete-lnportion-p  (and reg-iswithin-p
                                           (not delete-wholeln-buteob-p)))
                 (cur-firstln-p       (= (line-number-at-pos cur-pt)
                                         (line-number-at-pos cur-ptmin)))

                 (keeprelpos-effect-p (and keep-relpos
                                           (not cur-firstln-p)
                                           (or delete-wholeln-p
                                               delete-wholeln-buteob-p)))
                 (del-func            (lambda (x)
                                        (setq reg-content (and return-region-string
                                                               (buffer-substring
                                                                cur-start-edgept cur-end-edgept)))
                                        (delete-region cur-start-edgept cur-end-edgept)
                                        (setq deletion-type-flag x))))
            ;; delete region
            (cond
             (delete-wholeln-p
              (funcall del-func 0))
             (delete-wholeln-buteob-p
              (funcall del-func 1))
             (delete-lnportion-p
              (funcall del-func 2))
             (t
              (error "internal error")))
            ;; final operation
            (cond (keeprelpos-effect-p
                   (forward-line -1)
                   (setq final-optype-flag 2))
                  (delete-wholeln-p
                   (setq final-optype-flag 0))
                  (t
                   ;; FIXME: did `delete-region' guarantees for put
                   ;; point in region start?
                   (unless (= (point) cur-start-edgept)
                     (error "internal error"))
                   (setq final-optype-flag 1)))
            ))))

    ;; final return
    (when deletion-type-flag
      (list :type-of-deletion
            (if with-result-debug
                (alist-get deletion-type-flag deltype-alist)
              deletion-type-flag)
            :type-of-operation-after-deletion
            (if with-result-debug
                (alist-get final-optype-flag fnloptype-flag)
              final-optype-flag)
            :deleted-content-string
            reg-content))
    ))

;; **** buffer xy coordinates system

(defun entropy/emacs-make-eemacs-buffer-pos-coordinate-obj (x y)
  "Create a =EEMACS-POS-COORDINATE-OBJ= (see
`entropy/emacs-get-eemacs-buffer-pos-coordinate-obj') without
buffer `point' binding effectivity."
  (unless (natnump x)
    (error "=EEMACS-POS-COORDINATE-OBJ= maker: X '%d' is invalid which
should be non-negative integer." x))
  (unless (natnump y)
    (error "=EEMACS-POS-COORDINATE-OBJ= maker: y '%d' is invalid which
should be non-negative integer." y))
  (cons x y))

(defun entropy/emacs-get-eemacs-buffer-pos-coordinate-obj
    (&optional number-or-marker buffer absolute)
  "Return eemacs buffer pointer NUMBER-OR-MARKER's coordinate object
i.e. =EEMACS-POS-COORDINATE-OBJ= a inherit of =EEMACS-XYCRD-POINT-OBJ=
(see `entropy/emacs-xycrd-make-coordinate-obj' for details) but with both
of X and Y is `natnump' integer which is used to describe logical
place in a emacs buffer using `point' square left-top corner as the
point of a =EEMACS-XYCRD-POINT-OBJ=.

BUFFER is a live buffer and default to `current-buffer' but both be
covered by the NUMBER-OR-MARKER's buffer when it is a marker (i.e. can
be prediated by `markerp') and its `marker-buffer' is non-nil.

When ABSOLUTE is non-nil return the absolute coordinate which ignore
any narrowed portion.

Unlike marker or overlay, =EEMACS-POS-COORDINATE-OBJ= is not bind with
any buffer or NUMBER-OR-MARKER after got since its a abstraction of
buffer zero base two dimensions coordinates system, but we need a
buffer to get a valid `point' to calculate it since in this occasion
we need build a *effective* =EEMACS-POS-COORDINATE-OBJ= (in other
word, when the =EEMACS-POS-COORDINATE-OBJ= indicated place in the
BUFFER no longer has any characters, the got
=EEMACS-POS-COORDINATE-OBJ= can not be used to find character in its
described place any more i.e. it points to a empty non-existed
position in BUFFER, therefore the =EEMACS-POS-COORDINATE-OBJ= in this
place is *noneffective* any more unless new contents inserted into
BUFFER which filled that coordicate).

Thus two =EEMACS-POS-COORDINATE-OBJ= are same which can be predicated
by `equal' but all =EEMACS-POS-COORDINATE-OBJ= in any buffer got with
same NUMBER-OR-MARKER's `point' are usually not same since the `point'
has no defination to precisely describe a coordinate of a buffer where
its just a non-multibyte character container que's length.

Use `entropy/emacs-make-eemacs-buffer-pos-coordinate-obj' to make a
context independent =EEMACS-POS-COORDINATE-OBJ=.
"
  (let* ((mkbuff (when (markerp number-or-marker)
                   (marker-buffer number-or-marker)))
         (buffer (or mkbuff buffer (current-buffer)))
         (mkpt (when (markerp number-or-marker)
                 (marker-position number-or-marker)))
         (pt (or mkpt number-or-marker))
         lnum lnbegpt)
    (with-current-buffer buffer
      (setq pt (or pt (point)))
      (save-excursion
        (goto-char pt)
        (setq
         lnum (line-number-at-pos nil absolute)
         lnbegpt (line-beginning-position))))
    (entropy/emacs-xycrd-make-coordinate-obj
     (- pt lnbegpt)
     (- lnum 1))))

(defun entropy/emacs-buffer-get-eemacs-buffer-rectangle-obj
    (begin-pt end-pt &optional buffer absolute)
  "Return a =EEMACS-REGION-RECTANGLE-OBJ= i.e. a inherit of
=EEMACS-XYCRD-RECTANGLE-OBJ= (see
`entropy/emacs-xycrd-make-rectangle-obj') but
using =EEMACS-XYCRD-POINT-OBJ='s buffer inheritance
=EEMACS-POS-COORDINATE-OBJ=. The rectangle is according to a
region of begin point BEGIN-PT and end point END-PT in buffer
BUFFER.

The extending part of =EEMACS-XYCRD-RECTANGLE-OBJ= is adding
follow keys:

1) =:buffer= : the buffer where we used to research on.
2) =:absolute-coordinate-p= : non-nil when coordinated in a
   non-narrowed premise, inherited from when ABSOLUTE is non-nil.
3) =:buffer-narrow-region=: non-nil when BUFFER is
   `buffer-narrowed-p' and be cons of car of the region beginning
   point and cdr of the region end point.
4) =:begin-point=: the region beginning point of BUFFER.
5) =:end-point=: the region end point of BUFFER.

See also `entropy/emacs-get-eemacs-buffer-pos-coordinate-obj' for
how a coordinate is got.
"
  (let ((buffer (or buffer (current-buffer)))
        buffer-narrow-region
        beg-crd end-crd
        rtn)
    (with-current-buffer buffer
      (setq buffer-narrow-region (when (buffer-narrowed-p) (cons (point-min) (point-max))))
      (setq beg-crd    (entropy/emacs-get-eemacs-buffer-pos-coordinate-obj
                        begin-pt nil absolute)
            end-crd    (entropy/emacs-get-eemacs-buffer-pos-coordinate-obj
                        end-pt nil absolute))
      (setq rtn
            (entropy/emacs-xycrd-make-rectangle-obj
             beg-crd end-crd)
            rtn (append
                 (list :buffer buffer
                       :buffer-narrow-region buffer-narrow-region
                       :absolute-coordinate-p absolute
                       :begin-point           begin-pt
                       :end-point             end-pt)
                 rtn))
      rtn)))

;; **** overlay manipultion

(cl-defun entropy/emacs-is-valid-overlay-p (ov &key in-buffer do-error)
  "Return OV when overlay OV is valid as:

1) has alive buffer
2) has valid start and end position

Return nil when OV doesn't passed the validation.

When IN-BUFFER is set, the OV validation also need its buffer
`eq' to IN-BUFFER.

When DO-ERROR is set, error when OV is invalid.
"
  (let ((ov-buff (overlay-buffer ov))
        (ov-beg (overlay-start ov))
        (ov-end (overlay-end ov))
        (in-spec-buffer-p t)
        (wide-pass-p nil)
        rtn)
    (setq rtn
          (and (bufferp ov-buff)
               (buffer-live-p ov-buff)
               ov-beg ov-end
               (setq wide-pass-p t)
               (if in-buffer
                   (setq in-spec-buffer-p
                         (eq ov-buff in-buffer))
                 t)
               ov))
    (unless rtn
      (when do-error
        (cond ((and wide-pass-p in-buffer (null in-spec-buffer-p))
               (error "Overlay %S is an eemacs valid overlay but \
is not in specified buffer %S"
                      ov in-buffer))
              (t
               (error "Overlay %S is not a eemacs valid overlay")))))
    rtn))

(cl-defun entropy/emacs-buffer-position-within-overlay-p
    (&rest overlays
           &key position in-buffer
           without-start-edge without-end-edge
           return-all
           &allow-other-keys)
  "Return the first overlay in OVERLAYS whose area include buffer
IN-BUFFER's position POSITION or all matched ones with order obeyed
when RETURN-ALL is set. Return nil when no overlay matched for thus
condition.

POSITION must be predicated by `entropy/emacs-buffer-position-p'
or an error will be throwed out.

Commonly a position in a overlay is that:
1) Overlay's buffer is IN-BUFFER where POSITION got from.
2) Overlay's start position is less or equal (expect
   WITHOUT-START-EDGE is set) than the POSITION.
3) Overlay's end position minus 1 is larger or equal (expect
   WITHOUT-END-EDGE is set) than the POSITION.
4) Always not matched when the overlay start and end is same since the
   buffer region is empty in this occasion.
 "
  (let ((ovs (entropy/emacs-defun--get-real-body overlays))
        (pos position)
        (in-buffer (or in-buffer (current-buffer)))
        (range-func
         (lambda (x a b)
           (if (= a b)
               nil
             (and (if without-start-edge (> x a) (>= x a))
                  (if without-end-edge
                      (< x (1- b))
                    (<= x (1- b)))))))
        rtn)
    (with-current-buffer in-buffer
      (if pos
          (entropy/emacs-buffer-position-p
           pos
           :do-error t)
        (setq pos (point))))
    (catch :exit
      (dolist (ov ovs)
        (and (entropy/emacs-is-valid-overlay-p ov :in-buffer in-buffer)
             (let ((ov-beg (overlay-start ov))
                   (ov-end (overlay-end   ov)))
               (when (funcall range-func pos ov-beg ov-end)
                 (push ov rtn)
                 (unless return-all
                   (throw :exit t)))))))
    (if return-all rtn (car rtn))))

;; *** Hook manipulation

(defmacro entropy/emacs-add-hook-lambda-nil (name hook as-append &rest body)
  "Biuld auto-named function prefixed by NAME a symbol and inject
into HOOK wrapped BODY. Appended inject when AS-APPEND is non-nil.

Return the defined function symbol. "
  (let ()
    `(let* ((prefix-name-func
             (lambda ()
               (intern (format
                        "entropy/emacs-fake-lambda-nil-for-%s-/random-as-%s-function"
                        (symbol-name ',name)
                        (random most-positive-fixnum)))))
            (prefix-name (funcall prefix-name-func))
            func-define)
       ;; Prevent duplicate fboundp
       (when (fboundp prefix-name)
         (while (fboundp prefix-name)
           (setq prefix-name
                 (funcall prefix-name-func))))
       (setq func-define
             (list 'defun prefix-name '()
                   '(progn ,@body)))
       (entropy/emacs-eval-with-lexical func-define)
       (if ,as-append
           (setq ,hook
                 (append ,hook (list prefix-name)))
         (add-hook ',hook
                   prefix-name))
       prefix-name)))

(defmacro entropy/emacs-do-without-johns (!hooks-pattern &rest body)
  "The macro for inhibit some johns in hooks and run body.

!HOOKS-PATTERN is formed as:

   hookname        inhibit-johns
     |                  |
     v                  v
  ((hook0 (hook0-feature0 hook0-feature1 ...))
   (hook1 (hook1-feature1 hook1-feature1 ...))
    ...)

While inhibit-johns can be either an list of thus or a single
symbol t which means disable this hook before run BODY. "
  `(let (let-core
         (body ',body))
     (dolist (pattern ',!hooks-pattern)
       (if (eq (cadr pattern) t)
           (push `(,(car pattern) nil) let-core)
         (push `(,(car pattern)
                 (delete*
                  nil
                  (mapcar (lambda (x)
                            (unless (member x ',(cadr pattern))
                              x))
                          ,(car pattern))))
               let-core)))
     (entropy/emacs-eval-with-lexical
      `(let* ,(reverse let-core)
         ,@body))))

;; *** Color operations

(defun entropy/emacs-color-string-hex-p (color-hex-str-maybe)
  "Check a string COLOR-HEX-STR-MAYBE whether is an hex color string."
  (string-match-p "^#[0-9a-f]\\{3\\}[0-9a-f]*$"
                  color-hex-str-maybe))

(defun entropy/emacs-color-values-to-rgb (color-values)
  "Transfer COLOR-VALUES which is the result of `color-values' to a
RGB list which can be used to `color-rgb-to-hex'."
  (let ((r (car color-values))
        (g (cadr color-values))
        (b (caddr color-values))
        (full 65535.0))
    (setq r (/ r full)
          g (/ g full)
          b (/ b full))
    (list r g b)))

(defun entropy/emacs-color-get-color-hex-string (color-str &optional frame)
  "Transfer an color string COLOR-STR to hex color string,
COLOR-STR can be an color name or a hex color string.

Optional arg FRAME used for which frame specified as base to
calculate the color values. If FRAME is omitted or nil, use the
selected frame."
  (cond
   ((entropy/emacs-color-string-hex-p color-str)
    (apply 'color-rgb-to-hex
           (entropy/emacs-color-values-to-rgb
            (color-values color-str frame))))
   (t
    (apply 'color-rgb-to-hex
           (color-name-to-rgb color-str frame)))))

(defun entropy/emacs-color-same-p
    (color1 color2 &optional color1-frame color2-frame)
  "Check whether two color COLOR1 and COLOR2 are same as each
other, the color is the arg of type as the same as what
`color-values' used.

Two frame spec optional args individually spec which frame to to
use when calculate the color values for each color. If FRAME is
omitted or nil, use the selected frame.
"
  (require 'faces)
  (let ((c1 (color-values color1 color1-frame))
        (c2 (color-values color2 color2-frame)))
    (equal c1 c2)))

(defun entropy/emacs-color-scale-common (color factor &optional frame)
  "Make color values of COLOR scaled by FACTOR of frame
FRAME. Return a hex color string.

Optional arg frame when specified we calculate the color value
based the frame, nil for use selected frame."
  (let* ((cv (color-values color frame))
         (cl (entropy/emacs-color-values-to-rgb cv))
         (r (car cl))
         (g(cadr cl))
         (b (caddr cl)))
    (setq r (* r factor)
          g (* g factor)
          b (* b factor))
    (apply 'color-rgb-to-hex (list r g b))))

;; *** Face manipulation

(defun entropy/emacs-face-attribute-trim-unspecified
    (face attribute &optional frame inherit)
  "Same as `face-attribute' but resolve any unspecified or
relative values by merging with the ‘default’ face (which is
always completely specified) when the ATTRIBUTE's value is
'unspecified'."
  (let ((inherit (or inherit 'default)))
    (face-attribute face attribute frame inherit)))

(defun entropy/emacs-face-attribute-color-p
    (face attribute &optional frame inherit)
  "Same as `face-attribute' but return nil while the ATTRIBUTE's
value of FACE is not a color string which can predicated by
`entropy/emacs-color-string-hex-p' or a member of
`defined-colors' on FRAME."
  (let ((val (face-attribute face attribute frame inherit)))
    (and (stringp val)
         (or (entropy/emacs-color-string-hex-p val)
             (member val (defined-colors frame)))
         val)))

(defun entropy/emacs-get-face-attribute-alist (face &optional frame inherit)
  "Map face FACE all attributes into a alist with element formed
as '(cons attr-key attr-value)' which can be used for
`set-face-attribute' to loop did as.

If optional argument INHERIT and FRAME is non-nil, it has the
same meaning used for `face-attribute'"
  (delete nil
          (mapcar
           (lambda (attr)
             (let ((value (face-attribute face attr frame inherit)))
               (cons attr value)))
           entropy/emacs-face-attributes-list)))

(defvar entropy/emacs-set-face-attribute--internal-log-for-setted-faces nil)
(when (custom-theme-enabled-p 'eemacs-cover-theme-0)
  (disable-theme 'eemacs-cover-theme-0))
(custom-declare-theme 'eemacs-cover-theme-0 nil)
(put 'eemacs-cover-theme-0 'theme-settings nil)
;; enable `theme-immediate' to the internal cover theme which let any
;; modify of the cover theme can take effects on the display status,
;; see the function `custom-push-theme' body details.
(put 'eemacs-cover-theme-0 'theme-immediate t)

(defun entropy/emacs-defun--theme-cover-0-rest ()
  (when entropy/emacs-set-face-attribute--internal-log-for-setted-faces
    (let (log)
      (dolist (fre entropy/emacs-set-face-attribute--internal-log-for-setted-faces)
        (let ((face (car fre))
              (custom--inhibit-theme-enable nil))
          (unless (member face log)
            (custom-theme-reset-faces
             'eemacs-cover-theme-0
             `(,face nil))
            (push face log))))))
  (setq entropy/emacs-set-face-attribute--internal-log-for-setted-faces nil)
  (disable-theme 'eemacs-cover-theme-0))
;; disable the internal cover theme must before any hooks running so
(add-hook 'entropy/emacs-theme-load-before-hook-head-1
          #'entropy/emacs-defun--theme-cover-0-rest)

(defvar entropy/emacs--advice-priority-eemacs-cover-them-0-timer
  (run-with-idle-timer
   0.001 t
   #'entropy/emacs--advice-priority-eemacs-cover-them-0-timer))
(defun entropy/emacs--advice-priority-eemacs-cover-them-0-timer ()
  "Take advanced priority for the `eemacs-cover-theme-0' in
`custom-enabled-themes' to guarantee the coverage feature."
  (unless (and (custom-theme-enabled-p 'eemacs-cover-theme-0)
               (eq 'eemacs-cover-theme-0 (car custom-enabled-themes)))
    (enable-theme 'eemacs-cover-theme-0)))

(defun entropy/emacs-set-face-attribute (face frame &rest args)
  "=entropy-emacs= specified function same as `set-face-attribute'.

But using a internal declared theme take priority over the
`entropy/emacs-theme-sticker' when frame FRAME is nil. This
specification will not pollute the default attribute of face FACE
in the `selected-frame'.

NOTE: any hook include this function injected into the
'entropy/emacs-theme-load-(before/after)-hook-*' must injected
after the hook `entropy/emacs-theme-load-before-hook-head-1' in
where an internal reset function injected."
  (let* ((this-spec `(,face ((t ,@args))))
         (custom--inhibit-theme-enable nil))
    (push (cons face args) entropy/emacs-set-face-attribute--internal-log-for-setted-faces)
    (if frame
        (apply 'set-face-attribute face frame args)
      (custom-theme-reset-faces
       'eemacs-cover-theme-0
       `(,face nil))
      (custom-theme-set-faces
       'eemacs-cover-theme-0
       this-spec)
      (custom-theme-recalc-face face))))

(defun entropy/emacs-face-bg-scale-when-same
    (face1 face2 factor &optional face1-frame face1-inherit face2-frame face2-inherit)
  "Scale face FACE1's background color with FACTOR when it's bg
color is same as face FACE2's bg color.

See optional args to specified each of frame spec and whether get
the color inherited as what `face-attribute' did."
  (let* ((c1 (face-attribute face1 :background face1-frame face1-inherit))
         (c2 (face-attribute face2 :background face2-frame face2-inherit))
         new-c-hex)
    (when (entropy/emacs-color-same-p c1 c2)
      (setq new-c-hex
            (entropy/emacs-color-scale-common c1 factor face1-frame))
      (entropy/emacs-set-face-attribute
       face1 face1-frame
       :background new-c-hex))))

;; *** Theme manipulation

(defun entropy/emacs-get-theme-face (theme face)
  "Get a face from a specified theme THEME if it's enabled
i.e. membered in `custom-enabled-themes'."
  (let ((theme-settings (get theme 'theme-settings)))
    (catch :exit
      (dolist (theme-setting theme-settings)
        (when (and (eq 'theme-face (car theme-setting))
                   (eq face (cadr theme-setting)))
          (throw :exit (cadddr theme-setting)))))))

(defun entropy/emacs-theme-adapted-to-solaire-p (&optional theme)
  "Judge whether current theme loaded adapted to `entropy/emacs-solaire-mode',
return t otherwise for nil. "
  (let ((theme_cur (if theme
                       (symbol-name theme)
                     (ignore-errors
                       (symbol-name entropy/emacs-theme-sticker)))))
    ;; Condition judge for unconditional occurrence for theme loading,
    ;; seem as in pdumper session.
    (if (and (stringp theme_cur)
             (not (eql 0 (length theme_cur))))
        (catch :exit
          (dolist (regex entropy/emacs-solaire-themes-regex-list)
            (when (ignore-errors (string-match-p regex theme_cur))
              (throw :exit t))))
      nil)))

;; *** Newtork manipulation
;; **** network status checker

(defun entropy/emacs-network-url-canbe-connected-p (url &optional timeout use-curl)
  "Check URL whether can be connecated and return non-nil if canbe thus.

Optional argument TIMEOUT is an integer used to restrict the test
timeout seconds range, default to 1 second.

Throw an error when the command 'curl' not found in system PATH if
USE-CURL is non-nil (but with some restriction as below description).

About USE-CURL:

Because emacs native `url-retrieve' implementation didn't fully
support the Real TIMEOUT feature i.e its subroutine
`make-network-process' didn't allow (FIXME: maybe?) the TIMEOUT
feature. Instead it using a `while' procedure with time stamp duration
judgement as:

#+begin_src emacs-lisp
  (catch 'done
    (while (not data-buffer)
      (when (and timeout (time-less-p timeout
                                      (time-since start-time)))
        (url-debug 'retrieval \"Timed out %s (after %ss)\" url
                   (float-time (time-since start-time)))
        (throw 'done 'timeout))
        .......
#+end_src

So that, emacs will stuck while `make-network-process' return the
data buffer but wait for an connection building (as dns or other
fake data recieved by GFW) which is the realting the TIMEOUT care
about.

Thus, defaulty this function will forcely using the system 'curl'
command to be the main subroutine when the 'curl' command existed in
your system. Unless its `eq' to an symbol 'force-not'."
  (let* ((timeout (or (and (integerp timeout) timeout) 1))
         (cbksym (entropy/emacs-make-dynamic-symbol-as-same-value nil))
         (use-curl (or (and (and use-curl (not (eq use-curl 'force-not)))
                            (if (executable-find "curl")
                                t
                              (user-error "command 'curl' not found in your PATH")))
                       (and
                        (not (eq use-curl 'force-not))
                        (executable-find "curl"))))
         (native-func
          '(lambda (x tout)
             "Like `url-http-head' but be silent and no cookie used."
             (unless (featurep 'url-http)
               (require 'url-http))
             (let ((url-request-method "HEAD")
                   (url-request-data nil))
               (url-retrieve-synchronously x t t tout))))
         (cmd-args
          `("curl"
            ;; NOTE: just get the server response head so we do not stuck in an download link.
            "-I"
            "--connect-timeout" ,(number-to-string timeout)
            "-s"
            "-A"
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 \
(KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36"
            ,url
            "-o"
            ;; https://curl.se/docs/manpage.html
            ;; To suppress response bodies, you can redirect output to /dev/null:
            ;;
            ;;   curl example.com -o /dev/null
            ;; Or for Windows use nul:
            ;;
            ;;   curl example.com -o nul
            ,(if sys/win32p
                 "nul"
               "/dev/null")
            )))
    (if use-curl
        (progn
          (entropy/emacs-make-process
           `(:name
             ,(format "eemacs network url canbe connected test for 'url'" url)
             :synchronously t
             :command ',cmd-args
             :buffer nil
             :after
             (setq ,cbksym t)))
          (prog1
              (symbol-value cbksym)
            (unintern cbksym nil)))
      (message "Use `url-http-head' to test url '%s' (warn: it may stuck emacs while GFW.) ..."
               url)
      (condition-case error
          (with-current-buffer
              (funcall native-func url timeout)
            (let ((kill-buffer-hook nil))
              (kill-buffer)
              t))
        (error
         (message "%S" error)
         nil)))))

(defun entropy/emacs-network-hostname-canbe-connected-p (hostname)
  "Check web hostname (not an target url) whether can be connected
to web sever HOSTNAME, a string to represent the server web
address, e.g. \"www.google.com\".

This function use wildly integrated system kit \"ping\" as the
subroutine, throw out a error prompt when it can no be found on
your system. Return non-nil for thus, or nil otherwise."
  (let ((ping-args-core (if sys/win32p '("-n" "1" "-w" "10")
                          '("-c" "1" "-W" "10"))))
    (unless (executable-find "ping")
      (error "Can not find 'ping' command in your system"))
    (= 0 (apply 'call-process `("ping" nil nil nil ,@ping-args-core
                                ,hostname)))))

(defun entropy/emacs-network-system-online-p ()
  "Judge whether emacs can be connected to the internet. Return
non-nil for thus, or nil otherwise."
  (or (entropy/emacs-network-url-canbe-connected-p "www.baidu.com" 1)
      (entropy/emacs-network-url-canbe-connected-p "www.google.com" 1)))

;; **** download file
(defun entropy/emacs-network-download-file
    (url destination &optional use-curl async verify)
  "Download file from URL to DESTINATION via alternative
synchronous or async method using emacs native `url-retrieve' or the
\"curl\" subprocess when USE-CURL non-nil, then verify the donwload
file via the VERIFY function with a single argument the
DESTINATION file-name which return t or nil for valid or invalid
verification status.

NOTE:
   The verification process will be pressed under `condition-case'
   since we do not allow any error corrupts whole download
   procedure.

This function return a callback status of a random symbol whose
valid value are 'success' and 'failed' or nil while download
process doesn't finished. The callback symbol have special
propties while any fatals occurred while either downloading or
renaming to destination and they are:

- 'error-type': indicate which error type while whole procedure,
  valid value are:
  + 'download' : procedure fatal occurred in download time
  + 'move'     : procedure fatal occurred in move to destination time.
  + 'verify'   : verify the download file fatal in which case

- 'temp-file'  : the downloaded temp-file path, this value just
  valid while the 'error-type' is 'move'.

- 'curl-args'  : the curl subprocess spawn arguments list only
  non-nil when 'error-type' eq to 'download', used to debug.

Optional arg USE-CURL may be an plist to extended curl arguments,
so that following keys are supported:

- ':timeout' : an integer string to specified the curl timeout
  option '--connect-timeout' so that we can handle the long await
  url downloading. If not set, defaultly set with 10sec."
  (let* ((tmp-file (expand-file-name
                    (format "eemacs-download-tmpfile_[%s]_random_%s"
                            (format-time-string "%Y%m%d%H%M%S")
                            (random))
                    temporary-file-directory))
         (default-directory temporary-file-directory)
         (cbk-symbol (let ((make-sym-func
                            (lambda ()
                              (intern (format "eemacs-network-download-random-cbk_%s"
                                              (random)))))
                           sym)
                       (setq sym (funcall make-sym-func))
                       (when (boundp sym)
                         (while (boundp sym)
                           (setq sym (funcall make-sym-func))))
                       (set sym nil)
                       sym))
         (move-to-des-func
          `(lambda ()
             (condition-case error
                 (progn
                   (message "Moving to '%s' ..." ,destination)
                   (rename-file ,tmp-file (expand-file-name ,destination))
                   (message "Moving to '%s' done!" ,destination)
                   (when (file-exists-p ,tmp-file)
                     (delete-file ,tmp-file)
                     (message "Deleted temp download file!"))
                   (if (and (functionp ',verify)
                            (prog1
                                t
                              (message ">> verify the downloaded file <%s> ..."
                                       ,destination)))
                       (if (eq t
                               ;; we must ignore errors for the
                               ;; verifiction function run since it
                               ;; will corrupt whole internal download
                               ;; procedure then destroys the API
                               ;; restriction.
                               (condition-case error
                                   (funcall ',verify ,destination)
                                 (error
                                  (entropy/emacs-message-do-message
                                   "%s%s"
                                   (red "archive verify function error: ")
                                   (format "%s" error)))))
                           (setq ,cbk-symbol 'success)
                         (setq ,cbk-symbol 'failed)
                         (put ',cbk-symbol 'error-type 'verify))
                     (setq ,cbk-symbol 'success)))
               (error
                (entropy/emacs-message-do-message
                 "%s"
                 (red (format "%s" error)))
                (setq ,cbk-symbol 'failed)
                (put ',cbk-symbol 'error-type
                     'move)
                (put ',cbk-symbol 'temp-file ,tmp-file)
                (when (file-exists-p ,tmp-file)
                  (delete-file ,tmp-file)))))))
    (while (file-exists-p tmp-file)
      (setq tmp-file
            (expand-file-name
             (format "eemacs-download-tmpfile_[%s]_random_%s"
                     (format-time-string "%Y%m%d%H%M%S")
                     (random))
             temporary-file-directory)))
    (let* ((proc-buffer (get-buffer-create "*---eemacs-url-donwload---*"))
           ;; '-L' option is required so that we can follow url
           ;; redirection for such as download from sourceforge.net
           (curl-args `("-L" "--connect-timeout"
                        ,(if (and (entropy/emacs-strict-plistp use-curl)
                                  (plist-get use-curl :timeout))
                             (plist-get use-curl :timeout)
                           "10")
                        ,url "-o" ,tmp-file))
           (inhibit-read-only t)
           (success-message (format "Download from '%s' finished" url))
           (success-or-fatal-func-call-done-p-sym
            (let ((make-sym-func
                   (lambda ()
                     (intern (format "eemacs-network-download-random-internal-sync-indicator_%s"
                                     (random)))))
                  sym)
              (setq sym (funcall make-sym-func))
              (when (boundp sym)
                (while (boundp sym)
                  (setq sym (funcall make-sym-func))))
              (set sym nil)
              sym))
           (success-func `(lambda ()
                            (message ,success-message)
                            (setq ,cbk-symbol 'success)
                            (funcall ,move-to-des-func)
                            ))
           (fatal-message (format "Download file form '%s' failed!" url))
           (fatal-func `(lambda ()
                          (setq ,cbk-symbol 'failed)
                          (put ',cbk-symbol 'error-type 'download)
                          (when',use-curl
                           (put ',cbk-symbol 'curl-args ',curl-args))
                          (message ,fatal-message)
                          (when (file-exists-p ,tmp-file)
                            (delete-file ,tmp-file))
                          ))
           (common-sentinel
            `(lambda (proc status)
               (if (string= status "finished\n")
                   (funcall ,success-func)
                 (when (string-match-p "\\(exit\\|failed\\|exited\\|broken\\)" status)
                   (funcall ,fatal-func)))
               (setq ,success-or-fatal-func-call-done-p-sym t)))
           proc)
      (with-current-buffer proc-buffer
        (erase-buffer))
      (cond
       (async
        (if use-curl
            (progn
              (setq
               proc
               (make-process
                :name "eemacs url download"
                :buffer proc-buffer
                :command (cons "curl" curl-args)))
              (set-process-sentinel
               proc
               common-sentinel))
          (setq proc-buffer
            (url-retrieve
             url
             `(lambda (status &rest _)
                (let ((error-p (alist-get :error status)))
                  (if error-p
                      (funcall ,fatal-func)
                    (re-search-forward "\r?\n\r?\n")
                    (write-region (point) (point-max) ,tmp-file)
                    (funcall ,success-func)
                    )))))))
       (t
        (if use-curl
            ;; FIXME: doi not use `call-process' here since in this
            ;; case curl can not be terminated by SIGINT

            ;; (if (eq (apply 'call-process
            ;;                "curl" nil nil nil
            ;;                curl-args)
            ;;         0)
            ;;     (funcall success-func)
            ;;   (funcall fatal-func))
            (progn
              (setq
               proc
               (make-process
                :name "eemacs url download"
                :buffer proc-buffer
                :command (cons "curl" curl-args)))
              (set-process-sentinel
               proc
               common-sentinel)
              ;; wait for process done
              (while (process-live-p proc) (sleep-for 1))
              ;; wait for sentinel done
              (while (null (symbol-value success-or-fatal-func-call-done-p-sym)) (sleep-for 1))
              (sleep-for 2))
          (condition-case error
              (progn
                (url-copy-file url tmp-file)
                (funcall success-func))
            (error
             (entropy/emacs-message-do-message
              "%s"
              (red (format "%s" error)))
             (funcall fatal-func)))))))
    cbk-symbol))

;; *** Key map manipulation
(defun entropy/emacs-batch-define-key (key-obj-list)
  "Define key to keymap for batching way.

KEY-OBJ-LIST's each element forms as (keymap . ((key func) ... )), 'key' was
the string passed to `kbd'."
  (dolist (key-obj key-obj-list)
    (let ((key-map (car key-obj))
          (key-binds (cdr key-obj)))
      (when (boundp key-map)
        (dolist (key-bind key-binds)
          (define-key key-map
            (kbd (car key-bind)) (cdr key-bind)))))))

(defmacro entropy/emacs-set-key-without-remap
    (keymap key command)
  "Like `define-key' but also remove any remap of COMMAND in
KEYMAP before bind the new spec."
  (declare (indent defun))
  `(let (_)
     ;; Firstly remove the remap since the new spec may be a remap
     ;; also
     (define-key ,keymap
       (vector 'remap ,command)
       nil)
     ;; Then we injecting the spec
     (define-key ,keymap ,key ,command)))

(defmacro entropy/emacs-!set-key (key command)
  "The specified `define-key' like key builder for
`entropy/emacs-top-keymap'."
  (declare (indent defun))
  `(define-key entropy/emacs-top-keymap ,key ,command))

(defun entropy/emacs-local-set-key (key command)
  "Like `local-set-key' but always create a new keymap which copied
by `current-local-map' when it has or as what `local-set-key'
does."
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (let ((map (current-local-map)))
    (if map
        (use-local-map (setq map (copy-tree map)))
      (use-local-map (setq map (make-sparse-keymap))))
    (define-key map key command)))

(defun entropy/emacs-local-set-key-batch-do (&rest args)
  "Like `entropy/emacs-local-set-key' but in batch mode i.e. ARGS
are (key . command) paire formed list."
  (let ((map (current-local-map)))
    (if map
        (setq map (copy-tree map))
      (setq map (make-sparse-keymap)))
    (dolist (el args)
      (let ((key (car el))
            (command (cdr el)))
        (or (vectorp key) (stringp key)
            (signal 'wrong-type-argument (list 'arrayp key)))
        (define-key map key command)))
    (use-local-map map)))

;; *** Compress or decompress file

(defvar entropy/emacs-archive-dowith-alist
  '((tar
     :compress "tar -cf %o %i"
     :extract  "tar -xf %i -C %o")
    (tgz
     :compress "tar -zcf %o %i"
     :extract  "tar -zxf %i -C %o")
    (txz
     :compress "tar -Jcf %o %i"
     :extract  "tar -Jxf %i -C %o")
    (t7z
     :compress "tar -cf - %i | 7z a -si %o"
     :extract  "7z x -so %i | tar -xf - -C %o")
    (zip
     :compress "zip %o -r --filesync %i"
     :extract  "unzip -o %i -d %o")))

;; FIXME: the replace may cover the filename which include the pseudo
;; substitution
(defun entropy/emacs-gen-archive-dowith-shell-command
    (archive-type input output dowith)
  "Generate a shell command to do with an archive dealing
procedure type DOWITH for archive type ARCHIVE-TYPE of the input
file name INPUT. The shell command also want a output file name
OUTPUT.

The ARCHVE-TYPE can be one of internal support archive type that:
1) 'tar' type: in which case, INPUT was a tar file commonly named
   with \".tar\" as its suffix name.

2) 'tgz' type: in which case, INPUT was a tar file compressed with
   \"gzip\" method and commonly named with \".tgz\" or \".tar.gz\"
   as its suffix name.

3) 'txz' type: in which case, INPUT was a tar file compressed with
   \"xz\" method and commonly named with \".txz\" or \".tar.xz\"
   as its suffix name.

4) 't7z' type: in which case, INPUT was a tar file compressed with
   \"7z\" method and commonly named with \".t7z\" or \".tar.7z\"
   as its suffix name.

5) 'zip' type: in which case, INPUT was a zipper compressed file
   commonly named with \".zip\" as its suffix name.
"
  (let* ((archive-dowith-plist
          (alist-get archive-type
                     entropy/emacs-archive-dowith-alist))
         (command-fmstr
          (plist-get archive-dowith-plist dowith)))
    (replace-regexp-in-string
     "%o" (shell-quote-argument output)
     (replace-regexp-in-string
      "%i" (shell-quote-argument input)
      command-fmstr nil t)
     nil t)))

(defun entropy/emacs-archive-dowith
    (archive-type input output dowith)
  "Process a file archive procedure using the shell command
generated by `entropy/emacs-gen-archive-dowith-shell-command'
with a success message prompt (i.e. commonly non-nil return) when
command executed successfully or throw out an `user-error'
otherwise.

The arguments list is the same as thus of
`entropy/emacs-gen-archive-dowith-shell-command', see it for
their usage."
  (if (= 0 (call-process-shell-command
            (entropy/emacs-gen-archive-dowith-shell-command
             archive-type input output dowith)))
      (message "%s file %s to %s successfully"
               (if (eq dowith :compress)
                   "Compress"
                 "Uncompress")
               input output)
    (user-error "%s file %s to %s with fatal ⚠"
                (if (eq dowith :compress)
                    "Compress"
                  "Uncompress")
                input output)))

;; *** Frame manupulation
(defun entropy/emacs-frame-is-fullscreen-p (&optional frame)
  "Judge whether FRAME is fullscreen, return t for yes for as, nil
for otherwise."
  (and
   (memq (frame-parameter frame 'fullscreen)
         '(fullscreen fullboth))
   t))

(defun entropy/emacs-frame-is-maximized-p (&optional frame)
    "Judge whether FRAME is maximized, return t for yes for as, nil
for otherwise."
  (eq (frame-parameter frame 'fullscreen)
      'maximized))

;; *** Window manipulation
;; **** Window width
(defun entropy/emacs-window-no-margin-column-width (&optional window)
  "Like `window-width''s no pixel set return but remove the
`window-margins' effect, thus the return is the column with as
the WINDOW is not margined even if has been margined by
`set-window-margins'.

WINDOW must be a live window and defaults to the selected one."
  (let ((win (or window
                 (selected-window))))
    (+ (window-width win)
       (or (car (window-margins win))
           0)
       (or (cdr (window-margins win))
           0))))

(defun entropy/emacs-window-horizontally-fill-frame-p (&optional window)
  "Return non-nil if WINDOW (default as `selected-window') is
horizontally filled the frame i.e. its has no left or right
sibling."
  (let ((win (or window
                 (selected-window))))
    (= (window-total-width win)
       (window-total-size (frame-root-window) t))))

;; **** Side window
(defun entropy/emacs-overview-live-side-windows ()
  "Overview all lived side windows return an alist whose each
element is formed as (window side-type . side-slot), if non lived side
windows exist return nil."
  (let (rtn)
    (walk-window-tree
     (lambda (win)
       (let ((side-type (window-parameter win 'window-side))
             (side-slot (window-parameter win 'window-slot)))
         (when (and side-type
                    (window-live-p win))
           (push `(,win ,side-type . ,side-slot) rtn))))
     (selected-frame))
    rtn))

(defun entropy/emacs-delete-side-windows (side-types)
  "Delete all lived side windows matched the 'side-type' in a
list SIDE-TYPES, 'side-type' is enumerated of (left right top
bottom)."
  (let ((side-types (cl-delete-duplicates side-types :test 'eq))
        (side-wins (entropy/emacs-overview-live-side-windows))
        (ignore-window-parameters t))
    (unless (null side-wins)
      (dolist (side side-types)
        (mapc (lambda (side-obj)
                (when (eq side (cadr side-obj))
                  (delete-window (car side-obj))))
              side-wins)))))

;; **** split window

(defvar entropy/emacs-split-window-default-exhaustion-buffname
  "*eemacs-split-default-exhausted-buffer*")

(defun entropy/emacs-no-same-buffer-split-window
    (&optional window size side pixelwise buffname)
  "Like `split-window' but not use same buffer in the new window,
defaulty display the buffer
`entropy/emacs-split-window-default-exhaustion-buffname' if
optional argument BUFFNAME is not set.

And if BUFFNAME is not specified, we set the `default-directory'
of the new buffer on new-window which returned as the same as the
original."
  (let* ((orig-dfdir default-directory)
         (orig-win (selected-window))
         (bfname
          (or buffname
              entropy/emacs-split-window-default-exhaustion-buffname))
         (buff (get-buffer-create bfname))
         (window (apply 'split-window
                        (list window size side pixelwise))))
    (set-window-buffer window buff)
    (unless buffname
      (with-current-buffer buff
        (setq-local default-directory orig-dfdir)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert
           (format "This is the temporally buffer exhausted to
the new window splitted from origin window <%s>,
you should switch to another buffer by [C-x b] or in other way
because this buffer is auto-generated and has no meaning
unless this hints."
                   orig-win)))
        (unless (bound-and-true-p buffer-read-only)
          (setq-local buffer-read-only t))))
    ;; finally return the WINDOW same as `split-window'
    window))

(defun entropy/emacs-no-same-buffer-split-window-horizontally
    (&optional size)
  "Like `split-window-horizontally' but uhkse
`entropy/emacs-no-same-buffer-split-window' as subroutine."
  (interactive "P")
  (let ((old-window (selected-window))
        (size (and size (prefix-numeric-value size)))
        new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (entropy/emacs-no-same-buffer-split-window
                      nil size t))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
        (set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(defun entropy/emacs-no-same-buffer-split-window-vertically
    (&optional size)
  "Like `split-window-vertically' but use
`entropy/emacs-no-same-buffer-split-window' as subroutine."
  (interactive "P")
  (let ((old-window (selected-window))
        (old-point (window-point))
        (size (and size (prefix-numeric-value size)))
        moved-by-window-height moved new-window bottom)
    (when (and size (< size 0) (< (- size) window-min-height))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (entropy/emacs-no-same-buffer-split-window
                      nil size))
    (unless split-window-keep-point
      (with-current-buffer (window-buffer)
        ;; Use `save-excursion' around vertical movements below
        ;; (Bug#10971).  Note: When the selected window's buffer has a
        ;; header line, up to two lines of the buffer may not show up
        ;; in the resulting configuration.
        (save-excursion
          (goto-char (window-start))
          (setq moved (vertical-motion (window-height)))
          (set-window-start new-window (point))
          (when (> (point) (window-point new-window))
            (set-window-point new-window (point)))
          (when (= moved (window-height))
            (setq moved-by-window-height t)
            (vertical-motion -1))
          (setq bottom (point)))
        (and moved-by-window-height
             (<= bottom (point))
             (set-window-point old-window (1- bottom)))
        (and moved-by-window-height
             (<= (window-start new-window) old-point)
             (set-window-point new-window old-point)
             (select-window new-window))))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
        (set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

;; **** Window overlay judgements

(defun entropy/emacs-window-overlay-is-2-horizontal-splits-p (&optional frame other-window)
  "Return non-nil when current window overlay in frame
FRAME(default to the selected one) has just two windows and they
are horizontally split.

If optional argument OTHER-WINDOW is non-nil, return the window
which is sibling of the selected one on FRAME when core predicate
supplied."
  (let ((frame (or frame (selected-frame)))
        (frame-width-func
         (lambda (&optional frm)
           (let ((frm (or frm (selected-frame))))
             (if (display-graphic-p)
                 (frame-pixel-width frm)
               (frame-width frm)))))
        (window-width-func
         (lambda (win)
           (if (display-graphic-p)
               (window-pixel-width win)
             (window-total-width win)))))
    (with-selected-frame frame
      (and (= (length (window-list)) 2)
           (let ((rtn 0))
             (mapc (lambda (x) (setq rtn (+ rtn (funcall window-width-func x))))
                   (window-list))
             (= rtn (funcall frame-width-func)))
           (if other-window
               (catch :exit
                 (dolist (win (window-list))
                   (unless (eq win (frame-selected-window))
                     (throw :exit win)))
                 (error "internal error: can not found the other window"))
             t)))))

;; *** Test emacs with pure env

(cl-defun entropy/emacs-test-emacs-with-pure-setup-with-form
    (proc-name
     &rest forms
     &key
     use-current-package-user-dir
     emacs-invocation-name
     &allow-other-keys)
  "Invoke subprocess with process name string PROC-NAME to run
FORMS within a virginal emacs env (i.e. emacs -Q). Return the
process object.

If USE-CURRENT-PACKAGE-USER-DIR is non-nill and `eq' wit 't',
then initialize packages with current emacs session's
`package-user-dir' at the subprocess emacs initialization
time. Or it is a directory which used as thus, throw a error when
the directory not exist.

The `invocation-name' of the virginal emacs is as the same as
current emacs session if optional arg EMACS-INVOCATION-NAME is
not set, else using that as `invocation-name' for as."
  (setq forms (entropy/emacs-get-plist-body forms))
  (let* (proc
         (proc-buffer
          (generate-new-buffer
           (format "*entropy/emacs-test-emacs-with-pure-setup-with-form/%s*"
                   proc-name)))
         ;; -------------------- the read func --------------------
         (read-base64-func-name '__this-read-base64-encoded-sexp-from-buffer)
         (read-base64-func
          `(defun ,read-base64-func-name
               (base64-form-string)
             "Read base64 encoded sexp object getted from
BASE64-FORM-STRING which originally encoded by
`coding-system-for-write' using 'utf-8-auto and return the read
object which can be used for `eval'."
             (let ((sexp-str
                    (decode-coding-string
                     (base64-decode-string
                      base64-form-string)
                     'utf-8-auto))
                   ;; Parent expects UTF-8 encoded text.
                   (coding-system-for-read 'utf-8-auto)
                   (coding-system-for-write 'utf-8-auto))
               (read sexp-str))))
         ;; -------------------- the use spec form --------------------
         (form-encoded
          (with-current-buffer
              (entropy/emacs-generate-base64-encoded-sexp-buffer
               (if use-current-package-user-dir
                   `(let ((package-user-dir
                           ,(cond
                             ((not (eq t use-current-package-user-dir))
                              (let ((dir (entropy/emacs-eval-with-lexical
                                          use-current-package-user-dir)))
                                (unless (and dir
                                             (stringp dir)
                                             (not (string-empty-p dir))
                                             (file-exists-p dir))
                                  (error "the specified package-user-dir is not exist via '%S'"
                                         use-current-package-user-dir))
                                dir))
                             (t
                              package-user-dir))))
                      (progn
                        (package-initialize)
                        ,@forms))
                 `(progn
                    ,@forms)))
            (prog1
                (read (current-buffer))
              (let ((kill-buffer-hook nil))
                (kill-buffer (current-buffer))))))
         ;; -------------------- final eval form --------------------
         (proc-eval-form
          `(progn
             ,read-base64-func
             (setq __this_eval_form_str ,form-encoded)
             (setq __this_eval_form
                   (,read-base64-func-name __this_eval_form_str))
             (entropy/emacs-eval-with-lexical __this_eval_form)))
         (proc-eval-form-file
          (with-temp-buffer
            (let ((inhibit-read-only t)
                  (print-level nil)
                  (print-length nil)
                  (print-escape-nonascii t)
                  (print-circle t)
                  (tmpfile
                   (make-temp-file
                    "emacs-test-emacs-with-pure-setup-with-form.")))
              (goto-char (point-min))
              (prin1 proc-eval-form (current-buffer))
              (write-file tmpfile)
              tmpfile))))
    (setq proc
          (make-process
           :name proc-name
           :buffer proc-buffer
           :command `(
                      ;; ensure use the same emacs version as current session
                      ,(or emacs-invocation-name
                           (expand-file-name invocation-name invocation-directory))
                      "-Q" "-l" ,proc-eval-form-file)
           :sentinel
           `(lambda (proc event-str)
              (cond ((entropy/emacs-process-exit-with-fatal-p proc event-str)
                     (message
                      "eemacs viginal emacs test proc '%s' exit with fatal!"
                      ,proc-name)
                     (when (buffer-live-p ,proc-buffer)
                       (pop-to-buffer ,proc-buffer)))
                    (t
                     (unless (entropy/emacs-process-is-running-p proc)
                       (when (buffer-live-p ,proc-buffer)
                         (let ((kill-buffer-hook nil))
                           (kill-buffer ,proc-buffer)))
                       (message
                        "eemacs viginal emacs test proc '%s' exit with successfully!"
                        ,proc-name)))))))
    proc))

(declare async-start "ext:async")
(defun entropy/emacs-run-batch-with-eemacs-pure-env
    (start-form finish-form &optional defdir)
  "Invoke eemacs in batch-mode (i.e. `noninteractive' was non-nil)
with `entropy/emacs-env-init-with-pure-eemacs-env-p' asynchronously.

Return the async process object.

The START-FORM is a elisp form invoked in async request body, and
the FINISH-FORM is invoke after the START-FORM ran out within
current emacs session and optionally can use the result of
START-FORM via the internal binding variable =$ASYNC-RESULT=.

The process running `default-directory' is bind to DEFDIR if set
or will fallback to `temporary-file-directory'."
  (unless (featurep 'async)
    (require 'async))
  (entropy/emacs-env-with-pure-eemacs-env
   (or defdir temporary-file-directory)
   (async-start
    `(lambda (&rest _)
       (let ((start-file
              (expand-file-name
               "init.el" ,entropy/emacs-user-emacs-directory)))
         (load start-file)
         ,start-form))
    `(lambda ($async-result)
       ,finish-form))))

;; *** Read framework
;; **** read string enhanced
(defun entropy/emacs-read-string-repeatedly
    (prompt &optional initial-input history default-value inherit-input-method)
  "Like `read-string' but repeatly read while current input is not
predicated by `string-empty-p'.

Return as `read-string' when just one non-empty input or a list
of thus which ordered as input order."
  (let* ((this-read-str "__init__")
         (input-prompt (or prompt "Read repeat"))
         this-prompt
         rtn)
    (while (not (string-empty-p this-read-str))
      (if rtn
          (setq this-prompt
                (format (concat input-prompt " (%s): ")
                        (mapconcat #'identity (reverse rtn) ",")))
        (setq this-prompt (format "%s: " input-prompt)))
      (setq this-read-str
            (read-string this-prompt
                         initial-input history default-value inherit-input-method))

      (unless (string-empty-p this-read-str)
        (push this-read-str rtn)))
    (if (> (length rtn) 1)
        (reverse rtn)
      (if rtn
          (car rtn)
        this-read-str))))

;; **** ivy multiread framework

(defvar ivy--prompt)
(defvar ivy-last)
(defvar ivy-count-format)
(declare-function ivy-state-prompt "ext:ivy")
(defun entropy/emacs-ivy-read-repeatedly-function
    (read candidates-recorder-symbol prompt-abbrev &optional selected-shorten-function)
  "Common repeatedly read core component used for building `ivy-read''s ':action' function.

Arguments:

- READ: the current chosen entry, commonly derived by the
  ':action' function's first argument

- CANDIDATES-RECORDER-SYMBOL: chosen stored list, it's a
  symbol-name

- PROMPT-ABBREV: abbrevation of prompt for repeatedly chosen
  entry action prompt, suggested for keep consist with initial
  promot.

- SELECTED-SHORTEN-FUNCTION: function for shorten candidates's
  display, accept only one argument i.e. the READ.
"

  (when (and (not (member read (symbol-value candidates-recorder-symbol)))
             (not (string= "" read)))
    (push read (symbol-value candidates-recorder-symbol)))
  (let ((prompt (entropy/emacs--ivy-read-repeatedly-prompt-expand
                 prompt-abbrev candidates-recorder-symbol selected-shorten-function)))
    ;; NOTE: we must wrap the place holder into `eval' form since this
    ;; function is defined before the the ivy sturcture defined in
    ;; which case its `setf' general method is not defned which will
    ;; cause the `setf' macro expand with fatal.
    (entropy/emacs-eval-with-lexical `(setf (ivy-state-prompt ivy-last) ',prompt))
    (setq ivy--prompt (concat ivy-count-format " " prompt)))
  (cond
   ((eq this-command 'ivy-call)
    (with-selected-window (active-minibuffer-window)
      (delete-minibuffer-contents)))
   (t
    t)))

(defun entropy/emacs--ivy-read-repeatedly-prompt-expand
    (prompt-abbrev candidates-recorder-symbol &optional shorten-function)
  "Make incremented prompt string for function
`entropy/emacs-ivy-read-repeatedly-function', you can see the
details of arguments description on its function docstring."
  (format
   (concat prompt-abbrev " (%s) : ")
   (let ((candis-list (symbol-value candidates-recorder-symbol))
         mlist
         rtn)
     (dolist (candi-str candis-list)
       (let (decorated-candi-str)
         (unless (stringp candi-str)
           (cond
            ((symbolp candi-str)
             (setq candi-str (symbol-name candi-str)))
            (t
             (error
              "Just symbol and string type supported for candidates-recoreded (%s)."
              candi-str))))
         (setq decorated-candi-str
               (if shorten-function
                   (funcall shorten-function candi-str)
                 candi-str))
         (push decorated-candi-str mlist)))
     (setq rtn
           (let ((final-prompt-str ""))
             (dolist (el mlist)
               (setq final-prompt-str
                     (concat final-prompt-str
                             (if (string-empty-p el)
                                 ""
                               (concat el "☑; ")))))
             final-prompt-str))
     rtn)))

;; *** Operation status checker

(defun entropy/emacs-operation-status/running-auto-completion-op-p ()
  "The union judger for eemacs to indicating current operation is
company with auto completion."
  (cond
   ((and (eq entropy/emacs-auto-completion-use-backend-as 'company)
         (bound-and-true-p company-mode)
         (bound-and-true-p company-candidates))
    t)
   (t
    nil)))

(defun entropy/emacs-operation-status/auto-completion-idle-delay ()
  "Return current idle delay number for eemacs auto-completion
operation."
  (cond
   ((eq entropy/emacs-auto-completion-use-backend-as 'company)
    (or (and (bound-and-true-p company-idle-delay)
             company-idle-delay)
        (and (or (numberp entropy/emacs-company-idle-delay-default)
                 (error "`entropy/emacs-company-idle-delay-default' is not an number"))
             entropy/emacs-company-idle-delay-default)))
   (t
    (error "wrong type of eemacs auto-completion type '%s'"
           entropy/emacs-auto-completion-use-backend-as))))

;; ** eemacs specifications
;; *** Individuals

(defmacro entropy/emacs-general-run-with-gc-strict (&rest body)
  "Run BODY with restricted `gc-cons-threshold'."
  `(let* ((gc-cons-threshold entropy/emacs-gc-threshold-basic)
          (gc-cons-percentage entropy/emacs-gc-percentage-basic))
     ,@body))

(defmacro entropy/emacs-general-run-with-protect-and-gc-strict (&rest body)
  "Run BODY with `inhibit-quit' and restrict with basic
`gc-cons-threshold'."
  `(let* ((inhibit-quit t))
     (entropy/emacs-general-run-with-gc-strict
      ,@body)))

(defun entropy/emacs-transfer-wvol (file)
  "Transfer linux type root path header into windows volumn
format on windows platform."
  (if (and (string-match-p "^/[a-z]/" file)
           sys/win32p)
      (let ((wvol (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:" file)))
        (find-file wvol))
    (find-file file)))

(defun entropy/emacs-buffer-is-lisp-like-p ()
  "Justify current buffer is a lisp content buffer, any value for
true, nil for otherwise."
  (let ((lisp-file-regexp
         (progn
           (require 'rx)
           (rx (or (seq ".el" line-end)
                   (seq ".lisp" line-end)
                   )))))
    (or (and (buffer-file-name)
             (string-match-p lisp-file-regexp (buffer-file-name)))
        (or (eq major-mode 'emacs-lisp-mode)
            (eq major-mode 'lisp-mode)
            (eq major-mode 'lisp-interaction-mode)))))

(defun entropy/emacs-func-aliasp (func)
  "Return non-nil if function FN is aliased to a function symbol."
  (let ((val (symbol-function func)))
    (and val
         (symbolp val))))

(defvar entropy/emacs-icons-displayable-p--cache nil)
(defun entropy/emacs-icons-displayable-p (&optional reset)
  "Return non-nil if `all-the-icons' is displayable."
  (or (and entropy/emacs-icons-displayable-p--cache
           (not reset))
      (setq entropy/emacs-icons-displayable-p--cache
            (and entropy/emacs-use-icon
                 (display-graphic-p)
                 ;; FIXME: `find-font' can not be used in emacs batch mode.
                 (or (and entropy/emacs-fall-love-with-pdumper
                          entropy/emacs-do-pdumper-in-X)
                     (let ((rtn t))
                       (catch :exit
                         (dolist (font-name '("github-octicons"
                                              "FontAwesome"
                                              "file-icons"
                                              "Weather Icons"
                                              "Material Icons"
                                              "all-the-icons"))
                           (unless (find-font (font-spec :name font-name))
                             (setq rtn nil)
                             (throw :exit nil))))
                       rtn))))))

(defvar entropy/emacs-idle-cleanup-echo-area-timer-is-running-p nil)
(defun entropy/emacs-idle-cleanup-echo-area ()
  "Cleanup remaining echo area message when bussy done for some
tasks 'ing' refer prompts."
  (let ((inhibit-quit t))
    (unless entropy/emacs-idle-cleanup-echo-area-timer-is-running-p
      (setq entropy/emacs-idle-cleanup-echo-area-timer-is-running-p t)
      (run-with-idle-timer
       0.2 nil
       (lambda ()
         (let ((inhibit-quit t))
           (message nil)
           (setq entropy/emacs-idle-cleanup-echo-area-timer-is-running-p
                 nil)))))))

;; --------- make funciton inhibit readonly internal ---------
(defun entropy/emacs--make-function-inhibit-readonly-common
    (orig-func &rest orig-args)
  (let ((inhibit-read-only t))
    (apply orig-func orig-args)))

(defun entropy/emacs--make-function-inhibit-readonly-localalso
    (orig-func &rest orig-args)
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (apply orig-func orig-args)))

(defun entropy/emacs-make-function-inhibit-readonly
    (func &optional inhibit-local)
  "Make function FUNC adviced around by a let wrapper with
`inhibit-read-only' enabled of lexical means.

If optional argument INHIBIT-LOCAL is non-nil, its also press on
the buffer-locally variable `buffer-read-only'."
  (advice-add
   func
   :around
   (if inhibit-local
       #'entropy/emacs--make-function-inhibit-readonly-localalso
     #'entropy/emacs--make-function-inhibit-readonly-common)))

(defun entropy/emacs-version-compatible
    (version-limit version-current &optional strict)
  "Versions compatible judger for version spec basing rule of
commonly \"x.y.z\" convention to compared the required version
VERSION-LIMIT and the queried version VERSION-CURRENT.

The \"x.y.z\" version name convention defination is shown below:

- z: Quality update, API not changed
- y: Feature adding update, API not changed
- x: Compatibility destructive update, API are changed, indicating
     the fresh new version.

The compatible version comparation is rased on the two ways:
1. When optional argument STRICT is non-nil that just sub-version
   variable \"z\" can be dynamic one.
2. Vice-versa to case 1, \"y\" and \"z\" can be dynamically
   changed."
  (let* ((vl-1-regexp (regexp-quote
                       (let ((vl-list (butlast (version-to-list version-limit))))
                         (mapconcat 'number-to-string vl-list "."))))
         (vl-2-regexp (regexp-quote
                       (let ((vl-list (butlast (version-to-list version-limit) 2)))
                         (mapconcat 'number-to-string vl-list ".")))))
    (or (version= version-limit version-current)
        (and (version< version-limit version-current)
             (if strict
                 (string-match-p vl-1-regexp version-current)
               (string-match-p vl-2-regexp version-current))))))

(cl-defmacro entropy/emacs-save-excurstion-and-mark-and-match-data
    (&rest body &key turn-off &allow-other-keys)
  "Run BODY wrapped in `save-mark-and-excursion' and
`save-match-data'.

When TURN-OFF evaled non-nil, then run BODY directly."
  (declare (indent defun))
  (let ((body (entropy/emacs-get-plist-body body)))
    `(if ,turn-off
         (progn ,@body)
       (save-mark-and-excursion
         (save-match-data
           ,@body)))))

;; *** Lazy load specification
(defvar entropy/emacs--lazy-load-simple-feature-head nil)
(defvar entropy/emacs-lazy-load-simple-log-var nil)
(cl-defmacro entropy/emacs-lazy-load-simple
    (feature &rest body
             &key
             non-message
             always-lazy-load
             lexical-bindings
             &allow-other-keys)
  "Execute BODY after/require FEAURE is loaded.  FEATURE is normally a
feature name, but it can also be a file name, in case that file
does not provide any feature, further more FEATURE can be a list for
thus and autoloads them follow the order of that.

Optional key valid for:

- NON-MESSAGE:

  a form when evaluated non-nil do not show lazy loading config prompts.

- ALWAYS-LAZY-LOAD:

  a form when evalulated non-nil always lazy loading config after the
  featuer loaded without any restriction (see below). so the basic
  functional is same as `with-after-load'.

NOTE: Eventually BODY just be autoload when
`entropy/emacs-custom-enable-lazy-load' is non-nil with two
exceptions while ALWAYS-LAZY-LOAD is nil:

1. `daemonp': Since there's no need to lazy load anything while a
   daemon initialization.
2. `entropy/emacs-fall-love-with-pdumper' is non-nil, in which case
   eemacs initialization for a pdumper procedure, no need to do
   thus as case 1.

LEXICAL-BINDINGS:

Since there's not always has `lexical-binding' environemnt for
expanding this macro, thus use LEXICAL-BINDINGS to define a
var-val alist bind to BODY when it is wrapped into lazy load
scope since the BODY is ran in a lazy time out of the 'current
content', LEXICAL-BINDINGS should be formed as same as the
LEXICAL arg of `eval'.

This function should always be used preferred to maintain eemacs
internal context or API adding to thus, because any not be will
pollute eemacs internal lazy load optimization."
  (declare (indent 1) (debug t))
  (let ((body (entropy/emacs-get-plist-body body))
        (this-load-fname-sym (make-symbol "this-load-fname"))
        (feature-sym (make-symbol "feature-or-list"))
        (body-func-sym (make-symbol "body-func")))
    ;; macro main
    `(let ((,feature-sym ,feature)
           (,this-load-fname-sym
            (or
             ;; NOTE: the `load-file-name' must be evaluated by
             ;; load-time so we don't expand it at byte-compile time.
             (if load-file-name
                 (file-name-nondirectory load-file-name)
               ;; fixed the byte-compile as original
               byte-compile-current-file)
             "top-level"))
           ,body-func-sym)
       (when entropy/emacs-startup-with-Debug-p
         (push (list :feature ,feature-sym ;log the origin feature form
                     :load-in ,this-load-fname-sym
                     :body ',body)
               entropy/emacs-lazy-load-simple-log-var))
       (cond
        ((or entropy/emacs-custom-enable-lazy-load
             ,always-lazy-load)
         (when (not (null ,feature-sym))
           (cond
            ;; FIXME: not all the time this macro is expand in an
            ;; `lexical-binding' environment, so we still need to
            ;; create a artificial closure scope to handle the FEATURE
            ;; and LOAD-FNAME var bindind in runtime.
            (t
             (setq ,body-func-sym
                   (list 'function
                         (entropy/emacs-eval-with-lexical
                          '(lambda nil
                             (entropy/emacs-message-simple-progress-message
                              (unless ,non-message
                                (setq entropy/emacs--lazy-load-simple-feature-head
                                      (append entropy/emacs--lazy-load-simple-feature-head
                                              (list ,feature-sym)))
                                (format
                                 "[gened-by: %s] with lazy loading configs for feature '%s'"
                                 ,this-load-fname-sym
                                 ,feature-sym))
                              (entropy/emacs-general-run-with-protect-and-gc-strict
                               ,@body)))
                          (append
                           (list (cons ',feature-sym ,feature-sym)
                                 (cons ',this-load-fname-sym ,this-load-fname-sym))
                           ,lexical-bindings))))
             (entropy/emacs-eval-with-lexical
              (list 'entropy/emacs-eval-after-load
                    (list 'quote ,feature-sym)
                    (list 'funcall ,body-func-sym)))))))
        ((null entropy/emacs-custom-enable-lazy-load)
         (when (not (null ,feature-sym))
           (unless ,non-message
             (entropy/emacs-message-do-message
              "force load configs for feature '%s'" ,feature-sym)
             (setq entropy/emacs--lazy-load-simple-feature-head
                   (append entropy/emacs--lazy-load-simple-feature-head
                           (list ,feature-sym))))
           (cond ((listp ,feature-sym)
                  (dolist (el ,feature-sym)
                    (require el)))
                 ((symbolp ,feature-sym)
                  (require ,feature-sym)))
           (entropy/emacs-general-run-with-protect-and-gc-strict
            ,@body)))))))

(cl-defmacro entropy/emacs-lazy-with-load-trail
    (name &rest body &key
          doc-string start-end pdumper-no-end
          &allow-other-keys)
  "Wrapping BODY to a function named with suffix by NAME into
=entropy-emacs-startup-trail-hook=.

See `entropy/emacs-select-trail-hook' for details of what is
=entropy-emacs-startup-trail-hook=.

Optional keys:

- ':doc-string' :: host the function defination will be created
  for function of BODY.

- ':start-end' :: inject function of BODY into
  `entropy/emacs-startup-end-hook'. Defaultly, BODY will be
  injected into =entropy-emacs-startup-trail-hook=, but with this
  key non-nil or a form which evaluated result is non-nil.

- ':pdumper-no-end' :: specefied trail hook injection while
  pdumper according to `entropy/emacs-select-trail-hook'."
  (declare (indent defun))
  (let ((name-sym (make-symbol "name-var"))
        (func-sym (make-symbol "func-name"))
        (func-lambda-sym (make-symbol "func-body"))
        (msg-str-sym (make-symbol "msg-str"))
        (body-lambda-sym (make-symbol "body-lambda"))
        (body (entropy/emacs-defun--get-real-body body)))
    `(let* ((,body-lambda-sym (lambda nil ,@body))
            (,name-sym ,name)
            (_ (setq ,name-sym
                     (or (and (stringp ,name-sym) ,name-sym)
                         (symbol-name ,name-sym))))
            (,func-sym (intern
                        (concat "entropy/emacs-lazy-trail-to-"
                                ,name-sym)))
            (,msg-str-sym ,name-sym)
            ,func-lambda-sym)
       (setq ,func-lambda-sym
             (entropy/emacs-eval-with-lexical
              '(lambda (&rest _)
                 (entropy/emacs-message-do-message
                  "%s '%s' %s"
                  (blue "Start")
                  (yellow ,msg-str-sym)
                  (blue "..."))
                 (entropy/emacs-general-run-with-protect-and-gc-strict
                  (funcall ,body-lambda-sym))
                 (entropy/emacs-message-do-message
                  "%s '%s' %s"
                  (blue "Start")
                  (yellow ,msg-str-sym)
                  (blue "done!"))
                 (fmakunbound ,func-sym))
              (list (cons ',body-lambda-sym ,body-lambda-sym)
                    (cons ',msg-str-sym ,msg-str-sym)
                    (cons ',func-sym ,func-sym))))
       (defalias ,func-sym ,func-lambda-sym
         (or ,doc-string
             (format "`entropy/emacs-lazy-with-load-trail' for feature '%s', \
automatically self-`fmakunbound' when loaded done."
                     ,name-sym)))
       (cond
        (,start-end
         (setq entropy/emacs-startup-end-hook
               (append entropy/emacs-startup-end-hook
                       (list ,func-sym))))
        (t
         (let ((hook (entropy/emacs-select-trail-hook ,pdumper-no-end)))
           (set hook
                (append (symbol-value hook)
                        (list ,func-sym)))))))))

(cl-defmacro entropy/emacs-lazy-initial-form
    (list-var
     initial-func-suffix-name initial-var-suffix-name
     &rest body
     &key
     adder-type adder-flag
     abbrev-name prompt-type
     pdumper-no-end
     &allow-other-keys)
  "Generate a form whose functional is to wrap some forms into a
GENED-FUNTION who named with ABBREV-NAME and INITIAL-SUFFIX-NAME
(a non empty string) and add it with some *tricks* to
=entropy-emacs-startup-trail-hook= (see
`entropy/emacs-select-trail-hook') with *once calling* feature
for =entropy-emacs= lazy-load meaning.

PROMPT-TYPE can be either 'prompt-popup' or 'prompt-echo' for let
the initial form invoking do prompting in popup window type or
with origin message echo area with those specification.

The '&rest' type FORM-ARGS is orderd with ADDER-TYPE ADDER-FLAG
and BODY.

ADDER-TYPE is a symbol of either 'add-hook' or 'advice-add'.

When ADDER-FLAG is non-nil, whatever value it hosted is meaning
for a `advice-add's WHERE argument defination.

There's two tricks:

1. Directly add GENED-FUNCTION into the
   =entropy-emacs-startup-trail-hook= when
   `entropy/emacs-custom-enable-lazy-load' is non-nil. (if
   PDUMPER-NO-END is non-nil we also set the optional arg
   PDUMPER-NO-END of `entropy/emacs-select-trail-hook')

2. Using LIST-VAR and consider each element of it is a hook (when
   the ADDER-FLAG is nil) or a function (when ADDER-TYPE is
   non-nil), and add the GENED-FUNCTION into the hook or advice
   it to get the lazy load effection.

The GENED-FUNCTION has the '&rest' type argument
'_advice-orig-args' which has different meaning for different value
of ADDER-TYPE, it's meaningless when ADDER-TYPE is `add-hook',
otherwise it has the meaning for `add-function' remaining.

The GENED-FUNCTION is invocated just once that's why it is used
for lazy-loading. This functional based on the INITIAL-VAR which
named with ABBREV-NAME and INITIAL-VAR-SUFFIX-NAME (a non empty
string), BODY is wrapped into a form like:
#+begin_src emacs-lisp
  (unless INITIAL-VAR
    ,@BODY
    (setq INITIAL-VAR t))
#+end_src

So that INITIAL-FUNC-SUFFIX-NAME is the string `concat' follow
ABBREV-NAME to generate the body wrapper alias name i.e. the
function name.

Thus for all, ABBREV-NAME is feature based name string so that other
macro or function can use this naming domain to generate the
GENED-FUNCTION with the same name abbreviated space. In which case
various lazy load feature BODY wrapper can be belong to one
ABBREV-NAME but not for the INITIAL-FUNC-SUFFIX-NAME and
INITIAL-VAR-SUFFIX-NAME since they are unique for the current one, if
so the whole lazy load case will be replaced by the latter one who
invoked after."

  (let ((list-var-sym (make-symbol "list-var"))
        (func-body (entropy/emacs-defun--get-real-body body))
        (func-lambda-sym (make-symbol "func-lamba-var"))
        (func-body-lambda-sym (make-symbol "func-body-lamba-var"))
        (func-sym (make-symbol "func"))
        (var-sym (make-symbol "var"))
        (adder-type-sym (make-symbol "the-adder-type"))
        (adder-flag-sym (make-symbol "the-adder-flag"))
        (initial-func-suffix-name-sym (make-symbol "the-initial-func-suffix-name"))
        (initial-var-suffix-name-sym (make-symbol "the-initial-var-suffix-name"))
        (prompt-type-sym (make-symbol "the-prompt-type"))
        (abbrev-name-sym (make-symbol "the-abbrev-name")))
    `(let* ((,list-var-sym ,list-var)
            (,abbrev-name-sym ,abbrev-name)
            (,initial-func-suffix-name-sym ,initial-func-suffix-name)
            (,initial-var-suffix-name-sym ,initial-var-suffix-name)
            (,func-sym (intern (concat ,abbrev-name-sym "/lazy-func/" ,initial-func-suffix-name-sym)))
            (,var-sym (intern (concat ,abbrev-name-sym "/lazy-indc-var/" ,initial-var-suffix-name-sym)))
            (,adder-type-sym ,adder-type)
            (,adder-flag-sym ,adder-flag)
            (,prompt-type-sym ,prompt-type)
            (,func-body-lambda-sym (lambda (&rest _advice-orig-args) ,@func-body))
            ,func-lambda-sym
            )
       (unless (member ,adder-type-sym '(add-hook advice-add))
         (error "Wrong adder-type-sym for lazy-initial form '%s'"
                ,abbrev-name-sym))
       (when (and (eq ,adder-type-sym 'add-hook)
                  (not (null ,adder-flag-sym)))
         (error "Non-nil flag for 'add-hook' adder-type-sym for lazy-initial form '%s'"
                ,abbrev-name-sym))
       (let ((inhibit-quit t))
         (eval (list 'defvar ,var-sym nil)
               ;; we need to explicit disable the lexical binding for `defvar'
               nil)
         (setq
          ,func-lambda-sym
          (entropy/emacs-eval-with-lexical
           '(lambda (&rest ad-rest-args)
              (when (and (not (symbol-value ,var-sym))
                         ;; we just run intialform after eemacs startup
                         ;; done to speedup eemacs startup.
                         (or
                          (bound-and-true-p entropy/emacs-startup-done)
                          ;; but in pdumper dump session we want it to run
                          entropy/emacs-fall-love-with-pdumper
                          ;; but in daemon load session we want it run
                          (daemonp)
                          ;; but in non-lazy enable mode we want it run
                          (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load))
                          ))
                (let* ((inhibit-quit t)
                       (head-time (time-to-seconds))
                       (entropy/emacs-message-non-popup
                        (if (eq ,prompt-type-sym 'prompt-popup) nil t))
                       end-time
                       func-body-rtn)
                  ;; NOTE: set indicator before any body running to
                  ;; preventing recursively invoking inside of body, this
                  ;; is important for advice wrapper.
                  (setq ,var-sym t)
                  (entropy/emacs-message-do-message
                   "%s '%s' %s"
                   (blue "Loading and enable feature")
                   (yellow ,initial-func-suffix-name)
                   (blue "..."))
                  (entropy/emacs-general-run-with-protect-and-gc-strict
                   (setq func-body-rtn
                         (unwind-protect
                             (progn
                               (apply ,func-body-lambda-sym ad-rest-args))
                           ;; finally we remove the initial injection
                           (cond ((eq ,adder-type-sym 'advice-add)
                                  (dolist (adfor-it ,list-var-sym)
                                    (advice-remove adfor-it ,func-sym)))
                                 ((eq ,adder-type-sym 'add-hook)
                                  (dolist (adhfor-it ,list-var-sym)
                                    (remove-hook adhfor-it ,func-sym))))
                           ;; fake the function after evaluated it.
                           (fmakunbound ,func-sym)
                           (defalias ,func-sym #'ignore)
                           )))
                  (setq end-time (time-to-seconds))
                  (entropy/emacs-message-do-message
                   "%s '%s' %s '%s' %s"
                   (green "Load done for")
                   (yellow ,initial-func-suffix-name-sym)
                   (green "within")
                   (cyan (format "%f" (- end-time head-time)))
                   (green "seconds. (Maybe running rest tasks ...)"))
                  (entropy/emacs-idle-cleanup-echo-area)
                  func-body-rtn)))
           (list
            (cons ',func-sym ,func-sym)
            (cons ',var-sym ,var-sym)
            (cons ',list-var-sym ,list-var-sym)
            (cons ',adder-type-sym ,adder-type-sym)
            (cons ',adder-flag-sym ,adder-flag-sym)
            (cons ',func-lambda-sym ,func-lambda-sym)
            (cons ',func-body-lambda-sym ,func-body-lambda-sym)
            (cons ',prompt-type-sym ,prompt-type)
            (cons ',initial-func-suffix-name-sym ,initial-func-suffix-name-sym))))
         (entropy/emacs-eval-with-lexical
          (list 'defalias (list 'quote ,func-sym)
                (list 'function ,func-lambda-sym)))
         ;; injection
         (let (_)
           (cond
            ((not entropy/emacs-custom-enable-lazy-load)
             (let ((hook (entropy/emacs-select-trail-hook
                          ,pdumper-no-end)))
               (set hook (append (symbol-value hook) (list ,func-sym)))))
            (t
             (let (_)
               (dolist (item ,list-var-sym)
                 (if (not (null ,adder-flag-sym))
                     (funcall ,adder-type-sym item ,adder-flag-sym ,func-sym)
                   (funcall ,adder-type-sym item ,func-sym)))))))))))

(cl-defmacro entropy/emacs-lazy-initial-for-hook
    (hooks initial-func-suffix-name initial-var-suffix-name
           &rest body
           &key pdumper-no-end prompt-type
           &allow-other-keys)
  "Wrap forms collection BODY into a auto-gened function named
suffixed by INITIAL-FUNC-SUFFIX-NAME and then add it into a list
of hooks HOOKS and just enable it oncely at the next time calling
one of those hooks which commonly in usage time, this mechanism
judged by the judger i.e. the enabled status indication variable
named suffixed by INITIAL-VAR-SUFFIX-NAME.

PROMPT-TYPE can be either 'prompt-popup' or 'prompt-echo' for let
the initial form invoking do prompting in popup window type or
with origin message echo area with those specification.

If key :pdumper-no-end is non-nil then the BODY in non lazy
session is inject to the the common
`entropy/emacs-select-trail-hook' so that they are evaluated
while pdumper procedure.
"
  (let ((body-wrap (entropy/emacs-get-plist-body body)))
    `(entropy/emacs-lazy-initial-form
      ,hooks ,initial-func-suffix-name ,initial-var-suffix-name
      :abbrev-name "entropy/emacs--hook-first-enable-for"
      :prompt-type ,prompt-type
      :adder-type 'add-hook
      :pdumper-no-end ,pdumper-no-end
      ,@body-wrap)))

(cl-defmacro entropy/emacs-lazy-initial-advice-before
    (advice-fors initial-func-suffix-name initial-var-suffix-name
                 &rest body
                 &key pdumper-no-end prompt-type
                 &allow-other-keys)
  "Wrap forms collection BODY into a auto-gened function named
suffixed by INITIAL-FUNC-SUFFIX-NAME and then advice it to a list
of specific functions and just enable it oncely at the next time
calling one of those function which commonly in usage time, this
mechanism judged by the judger i.e. the enabled status indication
variable named suffixed by INITIAL-VAR-SUFFIX-NAME.

PROMPT-TYPE can be either 'prompt-popup' or 'prompt-echo' for let
the initial form invoking do prompting in popup window type or
with origin message echo area with those specification.

If key :pdumper-no-end is non-nil then the BODY in non lazy
session is inject to the the common
`entropy/emacs-select-trail-hook' so that they are evaluated
while pdumper procedure.
"
    (let ((body-wrap (entropy/emacs-get-plist-body body)))
      `(entropy/emacs-lazy-initial-form
        ,advice-fors ,initial-func-suffix-name ,initial-var-suffix-name
        :abbrev-name "entropy/emacs--BeforeADV-fisrt-enable-for"
        :prompt-type ,prompt-type
        :adder-type 'advice-add
        :adder-flag (quote :before)
        :pdumper-no-end ,pdumper-no-end
        ,@body-wrap)))

(cl-defmacro entropy/emacs-lazy-initial-advice-after
    (advice-fors initial-func-suffix-name initial-var-suffix-name
                 &rest body
                 &key pdumper-no-end prompt-type
                 &allow-other-keys)
  "Like `entropy/emacs-lazy-initial-advice-before' but for :after place."
  (let ((body-wrap (entropy/emacs-get-plist-body body)))
    `(entropy/emacs-lazy-initial-form
      ,advice-fors ,initial-func-suffix-name ,initial-var-suffix-name
      :abbrev-name "entropy/emacs--AfterADV-fisrt-enable-for"
      :prompt-type ,prompt-type
      :adder-type 'advice-add
      :adder-flag (quote :after)
      :pdumper-no-end ,pdumper-no-end
      ,@body-wrap)))

;; Disable messy commands in `M-x' list
(defun entropy/emacs-lazy-initial-read-extended-command-predicate
    (command &rest _)
  (if (and (symbolp command)
           (string-match-p
            "^\\(entropy/emacs--AfterADV-fisrt-enable-for\
\\|entropy/emacs--BeforeADV-fisrt-enable-for\
\\|entropy/emacs--hook-first-enable-for\\).+$"
            (symbol-name command)))
      nil
    t))
(add-to-list 'entropy/emacs-read-extended-command-predicates
             #'entropy/emacs-lazy-initial-read-extended-command-predicate)

;; *** Lazy execute specification
;; ***** TODO accumulation execution

;; (defmacro entropy/emacs-accumulation-execution-advice
;;     (func_name iterate_times))

;; ***** TODO idle execution
;; (defmacro entropy/emacs-idle-execution-advice
;;     (func_name iterate_times))

;; *** Package user dir specification

(defvar entropy/emacs--package-user-dir-setted nil)

(defun entropy/emacs--set-user-package-dir-common (version)
  "Setting `package-user-dir' base name based on emacs version
hosted in `entropy/emacs-ext-emacs-pkgel-get-pkgs-root'."
  (setq package-user-dir
        (expand-file-name
         (concat "elpa-" version)
         (expand-file-name entropy/emacs-ext-emacs-pkgel-get-pkgs-root))))

(defun entropy/emacs-set-package-user-dir ()
  "Set `package-user-dir' obey eemacs specified rule located in
`entropy/emacs-ext-emacs-pkgel-get-pkgs-root'.

NOTE: this is the only legal way to set `package-user-dir' in
eemacs context."
  (unless entropy/emacs--package-user-dir-setted
    (if (string-match-p "^27" emacs-version)
        (progn
          (when (and (string= emacs-version "27.1")
                     (display-graphic-p))
            (warn "Please update to emacs 27.2 at least \
since 27.1 has some fatal bug with gui session like posframe show
non comprehesive content etc.

Eemacs don't ban 27.1 version but that's strong recommendation of
thus."))
          (entropy/emacs--set-user-package-dir-common "27.1"))
      (cond
       ((string-match-p "^28" emacs-version)
        (entropy/emacs--set-user-package-dir-common "28.1"))
       ((string-match-p "^29" emacs-version)
        (entropy/emacs--set-user-package-dir-common "29.0.50"))
       (t
        (error "Unsupport emacs version '%s'" emacs-version))))
    (when (memq entropy/emacs-ext-elpkg-get-type
                '(entropy-emacs-extenisons-project
                  entropy-emacs-extensions-project-build))
      (setq package-user-dir
            (expand-file-name
             (concat (entropy/emacs-file-path-parser package-user-dir 'file-name)
                     "_MelpaLocal")
             (entropy/emacs-file-path-parser package-user-dir 'parent-dir))))
    (setq entropy/emacs--package-user-dir-setted t)))

;; *** Language environment specification

(defun entropy/emacs--lang-set (lang)
  (if (string-match-p
       "\\*e?shell\\*\\|\\*eshell-.*?\\*\\|\\(^\\*ansi-term-.*\\)\\|\\(\\*terminal\\)"
       (format "%s" (buffer-list)))
      (user-error "Can not use this function cause shell buffer exist, please kill it and try again!")
    (cond
     ((string= lang "UTF-8")
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8-unix)
      (message "Setting language environment to 'utf-8-unix'."))
     ((string= lang "LOCAL")
      (when (and (not (null entropy/emacs-custom-language-environment-enable))
                 (not (null entropy/emacs-locale-language-environment))
                 (assoc entropy/emacs-locale-language-environment language-info-alist)
                 (member entropy/emacs-locale-coding-system coding-system-list))
        (set-language-environment entropy/emacs-locale-language-environment)
        (prefer-coding-system entropy/emacs-locale-coding-system)
        (setq default-file-name-coding-system 'utf-8-unix)
        (message "Setting language environment to '%s'." entropy/emacs-locale-language-environment)))
     (t (user-error "Invalid LANG arg")))))

(defun entropy/emacs-lang-set-utf-8 (&rest _)
  "Setting language envrionment to unix-8-unix in vanilla status.

The vanilla status is for that excepts some special occasion and
this is also the functional usage restriction declaiming e.g. for
the situation when there's any process connection are built which
we forbiddingly shouldn't reset the whole language environment for
thus, if not, we will get messy with current emacs session.

For temporally usage of this functional case, see
`entropy/emacs-lang-use-utf-8-ces-around-advice'.
"
  (if (not (string= current-language-environment "UTF-8"))
      (entropy/emacs--lang-set "UTF-8")))

(defun entropy/emacs-lang-set-local (&rest _)
  "Setting language envrionment to the local matched with system
setting in vanilla status.

The vanilla status is for that excepts some special occasion and
this is also the functional usage restriction declaiming e.g. for
the situation when there's any process connection are built which
we forbiddingly shouldn't reset the whole language environment for
thus, if not, we will get messy with current emacs session.

For temporally usage of this functional case, see
`entropy/emacs-lang-use-locale-ces-around-advice'.
"
  (if (ignore-errors
        (not (string= current-language-environment
                      entropy/emacs-locale-language-environment)))
      (entropy/emacs--lang-set "LOCAL")))


(defun entropy/emacs-lang-use-utf-8-ces-around-advice (old-func &rest args)
  "Common around advice for wrapper function into utf-8
environment."
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (apply old-func args)))

(defun entropy/emacs-lang-use-locale-ces-around-advice (old-func &rest args)
  "Common around advice for wrapper funcion into locale language
environment, determined by `entropy/emacs-locale-coding-system'."
  (let ((coding-system-for-read entropy/emacs-locale-coding-system)
        (coding-system-for-write entropy/emacs-locale-coding-system))
    (apply old-func args)))

;; the 'with' macro
(defmacro entropy/emacs-lang-with-utf-8-ces (&rest body)
  "Do BODY within a utf-8 coding system environment."
  `(let* ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     ,@body))

(defmacro entropy/emacs-lang-with-locale-ces (&rest body)
  "Do BODY within a locale coding system environment determined
by `entropy/emacs-locale-coding-system'."
  `(let* ((coding-system-for-read entropy/emacs-locale-coding-system)
          (coding-system-for-write entropy/emacs-locale-coding-system))
     ,@body))

;; *** Org face specification
;; **** Cancel head face height rescale
(defvar entropy/emacs-defun--ohrsc-current-theme nil
  "Emacsc theme name for whose org or outline level face has been
backuped in faces of list of face
`entropy/emacs-defun--ohrsc-org-header-backup-faces'.")

(defconst entropy/emacs-defun--ohrsc-org-header-faces
  (list 'org-level-1
        'org-level-2
        'org-level-3
        'org-level-4
        'org-level-5
        'org-level-6
        'org-level-7
        'org-level-8
        'outline-1
        'outline-2
        'outline-3
        'outline-4
        'outline-5
        'outline-6
        'outline-7
        'outline-8))

(defconst entropy/emacs-defun--ohrsc-org-header-backup-faces
  (list 'org-level-1-backup-eemacs
        'org-level-2-backup-eemacs
        'org-level-3-backup-eemacs
        'org-level-4-backup-eemacs
        'org-level-5-backup-eemacs
        'org-level-6-backup-eemacs
        'org-level-7-backup-eemacs
        'org-level-8-backup-eemacs
        'outline-1-backup-eemacs
        'outline-2-backup-eemacs
        'outline-3-backup-eemacs
        'outline-4-backup-eemacs
        'outline-5-backup-eemacs
        'outline-6-backup-eemacs
        'outline-7-backup-eemacs
        'outline-8-backup-eemacs))

(defconst entropy/emacs-defun--ohrsc-org-header-face-spec
  '((:background . nil)
    (:weight . semi-bold)
    (:height . 1.0)))

(defun entropy/emacs-defun--ohrsc-cancel-org-header-face-scale ()
  (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
    (dolist (spc entropy/emacs-defun--ohrsc-org-header-face-spec)
      (set-face-attribute
       face nil
       (car spc) (cdr spc)))))

(defun entropy/emacs-defun--ohrsc-recovery-org-header-face-scale ()
  (let ((count 0))
    (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
      (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
        (dolist (spc entropy/emacs-defun--ohrsc-org-header-face-spec)
          (set-face-attribute
           face nil
           (car spc) (face-attribute face-bcp (car spc)))))
      (cl-incf count))))

(defun entropy/emacs-defun--ohrsc-org-header-face-backuped-p ()
  (if (eq entropy/emacs-theme-sticker entropy/emacs-defun--ohrsc-current-theme)
      (let (judge)
        (setq judge
              (catch :exit
                (dolist (face entropy/emacs-defun--ohrsc-org-header-backup-faces)
                  (unless (facep face)
                    (throw :exit 'lost)))))
        (if (eq judge 'lost)
            nil
          t))
    (setq entropy/emacs-defun--ohrsc-current-theme
          entropy/emacs-theme-sticker)
    nil))

(defun entropy/emacs-defun--ohrsc-org-header-faces-modified-p ()
  (let ((count 0)
        rtn)
    (setq rtn (catch :exit
                (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
                  (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
                    (unless (face-equal face face-bcp)
                      (throw :exit 'modified))
                    (cl-incf count)))))
    (if (eq rtn 'modified)
        t
      nil)))

(defun entropy/emacs-defun--ohrsc-backup-org-header-face ()
  (unless
   (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
   (let ((count 0))
     (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
       (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
         (copy-face face face-bcp))
       (cl-incf count)))))

(defun entropy/emacs-adjust-org-heading-scale ()
  "Stop the org-level headers from increasing in height
relative to the other text when
`entropy/emacs-disable-org-heading-scale' was non-nil."
  (require 'outline)
  (require 'org-faces)
  (when (display-graphic-p)
    (cond
     (entropy/emacs-disable-org-heading-scale
      (entropy/emacs-defun--ohrsc-backup-org-header-face)
      (entropy/emacs-defun--ohrsc-cancel-org-header-face-scale))
     ((null entropy/emacs-disable-org-heading-scale)
      (when (and
             (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
             (entropy/emacs-defun--ohrsc-org-header-faces-modified-p))
        (entropy/emacs-defun--ohrsc-recovery-org-header-face-scale))))))

;; *** MODE LINE
(defun entropy/emacs-modeline-judge-modeline-special-p ()
  "Judge whether in current status the `mode-line-format' is special
all cases is defined in
`entropy/emacs-modeline-cases-of-spec-modeline'."
  (let (rtn)
    (catch :exit
      (dolist (this-case entropy/emacs-modeline-cases-of-spec-modeline)
        (cond ((symbolp this-case)
               (setq rtn (funcall this-case)))
              ((listp this-case)
               (setq rtn (entropy/emacs-eval-with-lexical this-case)))
              (t
               (user-error "Wrong type of eemacs special modeline case, please check \
`entropy/emacs-modeline-cases-of-spec-modeline'")))
        (when rtn (throw :exit nil))))
    rtn))

(defun entropy/emacs-modeline-restore-default-mdlfmt ()
  "Restore defaut `mode-line-format' in the default-value slot of
`mode-line-format'.

This function used to clean the specification did by other
modeline swither."
  (setq-default
   mode-line-format
   entropy/emacs-modeline-default-modeline-formt)
  ;; diable the buffer local binding which will messy up other mode
  ;; line format injecting during `setq-default'.
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (unless (entropy/emacs-modeline-judge-modeline-special-p)
              (kill-local-variable 'mode-line-format))))
        (buffer-list)))

;; *** Theme loading specification

(declare-function doom-modeline-refresh-bars "ext:doom-modeline")

(defun entropy/emacs-theme-load-face-specifix (&optional theme-name-str frame)
  "Sets of specification for eemacs native themes.

This function should be invoked after a theme loaded done, and it
will automatically recongnized current theme name and do the
corresponding stuffs."
  (unless theme-name-str
    (setq theme-name-str (symbol-name entropy/emacs-theme-sticker)))
  ;; main spec
  (cond
   ((string-match-p "spacemacs-dark" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     ivy-current-match-face-hack-for-spacemacs-dark
     'ivy
     (entropy/emacs-set-face-attribute
      'ivy-current-match frame
      :background "purple4" :bold t)))
   ((string-match-p "spacemacs-light" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     ivy-current-match-face-hack-for-spacemacs-light
     'ivy
     (entropy/emacs-set-face-attribute
      'ivy-current-match frame
      :background "salmon" :bold t)))
   ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     ivy-current-match-face-hack-for-tsdh-whiteboard-adwaita-themes
     'ivy
     (if (equal 'dark (frame-parameter nil 'background-mode))
         (entropy/emacs-set-face-attribute
          'ivy-current-match frame
          :background "#65a7e2" :foreground "black")
       (entropy/emacs-set-face-attribute
        'ivy-current-match frame
        :background "#1a4b77" :foreground "white"))))
   ((string= "doom-solarized-light" theme-name-str)
    (when (not (featurep 'hl-line))
      (require 'hl-line))
    (entropy/emacs-set-face-attribute
     'hl-line frame
     :background
     "LightGoldenrod2")
    (entropy/emacs-set-face-attribute
     'solaire-hl-line-face frame
     :background "#ffffe4e4b5b5"))
   ((string= "doom-Iosvkem" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     ivy-current-match-face-hack-for-doom-Iosvkem-theme
     'ivy
     (entropy/emacs-set-face-attribute
      'ivy-current-match frame
      :background "grey8"
      :distant-foreground "grey7")))
   ((string= "doom-dark+" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     outline-3-face-hack-for-doom-dark+-theme
     'outline
     (entropy/emacs-set-face-attribute
      'outline-3 frame
      :foreground "LawnGreen")))
   ((string= "doom-1337" theme-name-str)
    (entropy/emacs-eval-after-load-unique
     ivy-current-match-face-hack-for-doom-1337-theme
     'ivy
     (entropy/emacs-set-face-attribute
      'ivy-current-match frame
      :background "#2257A0"
      :distant-foreground "#1B2229"))
    (entropy/emacs-set-face-attribute
     'highlight frame
     :foreground "grey7")
    (entropy/emacs-set-face-attribute
     'region frame
     :background "grey24"))
   ((or (string= "sanityinc-tomorrow-bright" theme-name-str)
        (string= "sanityinc-tomorrow-night" theme-name-str)
        (string= "sanityinc-tomorrow-eighties" theme-name-str))
    (entropy/emacs-set-face-attribute
     'tooltip frame
     :background "white"
     :foreground "grey21"))
   ((string= "sanityinc-tomorrow-blue" theme-name-str)
    (entropy/emacs-set-face-attribute
     'tooltip frame
     :background "white"
     :foreground "blue4"))
   ((string= "ujelly" theme-name-str)
    (dolist (el `((:face tooltip
                         :frame ,frame
                         :spec (:background "#393939393939" :foreground "white"))
                  (:face term-color-blue
                         :frame ,frame
                         :spec (:foreground "#61AFEF"))
                  (:face border
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face internal-border
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face vertical-border
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face window-divider
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face window-divider-first-pixel
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face window-divider-last-pixel
                         :frame ,frame
                         :spec (:background "grey6" :foreground "#505000000000"))
                  (:face symbol-overlay-default-face
                         :frame ,frame
                         :spec (:background "DarkOrange" :foreground "black")))
                )
      (apply 'entropy/emacs-set-face-attribute
             (plist-get el :face)
             (plist-get el :frame)
             (plist-get el :spec))))
   (t
    (entropy/emacs-set-fixed-pitch-serif-face-to-monospace)))
  ;; other spec
  ;; --- magit diff hunk highlight spec
  ;;
  ;; NOTE: in terminal magit hunk region background face is
  ;; unspecified so we should let it be visible.
  (unless (display-graphic-p)
    (entropy/emacs-eval-after-load-unique
     magit-diff-hunk-region-face-hack-for-cli-session
     'magit-diff
     (entropy/emacs-set-face-attribute
      'magit-diff-hunk-region
      frame
      :weight 'bold
      :extend t
      :background
      (let* ((color-p (lambda (x)
                        (and (stringp x)
                             (entropy/emacs-color-string-hex-p x)
                             x)))
             (cur-magit-bgc    (funcall color-p (face-attribute 'magit-diff-hunk-region :background)))
             (cur-magit-hl-bgc (funcall color-p (face-attribute 'magit-diff-context-highlight :background)))
             (cur-default-bgc  (funcall color-p (face-attribute 'default :background)))
             )
        (if cur-magit-bgc
            cur-magit-bgc
          (if cur-default-bgc
              (entropy/emacs-color-scale-common
               (or cur-magit-hl-bgc
                   cur-default-bgc)
               (if (eq (frame-parameter nil 'background-mode) 'light)
                   0.8
                 0.6))
            "#2f2f4f4f4f4f"))))))

  ;; --- company tooltip selection highlight
  ;; more visible for `company-tooltip-selection'
  (when (member theme-name-str '("doom-1337" "doom-Iosvkem"))
    (entropy/emacs-eval-after-load-unique
     company-tooltip-selection-face-hack-for-variant-themes
     'company
     (entropy/emacs-set-face-attribute
      'company-tooltip-selection
      frame
      :weight 'bold
      :extend nil
      :background
      (let ((cur-bgc (face-attribute 'company-tooltip-selection :background)))
        (unless (and (stringp cur-bgc)
                     (entropy/emacs-color-string-hex-p cur-bgc))
          (setq cur-bgc (face-attribute 'default :background)))
        (if (and (stringp cur-bgc)
                 (entropy/emacs-color-string-hex-p cur-bgc))
            (entropy/emacs-color-scale-common
             cur-bgc
             1.5)
          "#2f2f4f4f4f4f")))))
  )

(defun entropy/emacs-theme-load-modeline-specifix (&optional arg)
  "Sets of specification for eemacs native modelines.

This function should be invoked after a modeline type loaded done
by any eemacs defined modeline toggle function which always named
of abbreviated by \"entropy/emacs-modeline-mdl-NAME\" and it will
automatically recongnized current modeline type and do the
corresponding stuffs by obtained the modeline type from
`entropy/emacs-mode-line-sticker', or it will do the same of thus
but may be get the wrong context proper way that in which case,
the mode line type obtained from
`entropy/emacs-mode-line-sticker' may be not satisfied current
situation."
  (unless arg
    (setq arg (symbol-name entropy/emacs-theme-sticker)))
  (progn
    (cond ((and (string= entropy/emacs-mode-line-sticker "doom")
                (string-match-p "\\(ujelly\\)" arg))
           (entropy/emacs-set-face-attribute
            'doom-modeline-bar
            nil :background "black")
           (doom-modeline-refresh-bars))
          ((string= entropy/emacs-mode-line-sticker "doom")
           (entropy/emacs-set-face-attribute
            'doom-modeline-bar
            nil :background (face-background 'mode-line nil t))
           (doom-modeline-refresh-bars)))))

(defun entropy/emacs-solaire-specific-for-themes (&rest _)
  "Sets of specification after loaded a new theme for specified
stuffs on `entropy/emacs-solaire-mode' when
`entropy/emacs-theme-adapted-to-solaire-p' was judged."
  (when (entropy/emacs-theme-adapted-to-solaire-p)
    (require 'hl-line)
    ;; common spec
    (cond
     ((or (eq entropy/emacs-theme-sticker 'spacemacs-dark)
          (eq entropy/emacs-theme-sticker 'spacemacs-light))
      (when (facep 'solaire-hl-line-face)
        (entropy/emacs-set-face-attribute
         'solaire-hl-line-face
         nil
         :background
         (if (eq entropy/emacs-theme-sticker 'spacemacs-light)
             "#fbf8ef"
           (cond ((not (display-graphic-p))
                  "#333839")
                 ((display-graphic-p)
                  "#333340"))))))
     (t nil))
    ;; downgrade `company-tooltip' face when needed
    (entropy/emacs-face-bg-scale-when-same
     'company-tooltip 'solaire-default-face
     (cond
      ((or (eq (frame-parameter nil 'background-mode) 'light)
           (string-match-p "\\(light\\|day\\)"
                           (symbol-name entropy/emacs-theme-sticker)))
       0.95)
      ((eq (frame-parameter nil 'background-mode) 'dark)
       0.5))
     nil
     (when (member (face-attribute 'company-tooltip :background)
                   '(unspecified nil))
       t))))

;; *** Case fold search specification
(defun entropy/emacs-case-fold-focely-around-advice (old-func &rest args)
  "Wrapper function to disable `case-fold-search' functional ability."
  (let ((orig-case-type case-fold-search)
        rtn)
    (unwind-protect
        (progn (setq case-fold-search nil)
               (setq rtn (apply old-func args))
               (setq case-fold-search orig-case-type)
               rtn)
      (setq case-fold-search orig-case-type))))

;; *** Cli compatibale specification
;; **** `xterm-paste' wrappers

;; pre declaration
(defvar xclip-method)
(defvar xclip-program)
(declare-function term-send-raw-string "term")
(declare-function xterm-paste "xterm")

(defvar entropy/emacs--xterm-clipboard-head nil
  "The string of the the last event paste part of `xterm-paste',
setted by `entropy/emacs-xterm-paste-core'.")

(defvar entropy/emacs-xterm-paste-inhibit-read-only-filter nil
  "The conditions for judge whether xterm-paste with
`inhibit-read-only'.

Eacch condition is a function with one arg, the paste event and it
return non-nil for setting `inhibit-read-only' and nil for
unsetting it temporally within
`entropy/emacs-with-xterm-paste-core'.")

(defvar entropy/emacs-xterm-paste-yank-replacement-register nil
  "List of predicate pattern cons for using instead of `yank' used
within `entropy/emacs-xterm-paste'.

Which each car of the pattern was a condition, may be 'nil' or
't' or a function for be evaluated for the boolean result, and
the cdr was the replacement yank function")

(declare-function xclip-mode "xclip")

(defun entropy/emacs-xterm-cut-or-yank-sync-with-system/functional-env-statisfied-p ()
  "Return non-nil when current non-gui emacs session support sync
clipboard with native operation system."
  (let ((judger
         (or
          ;; darwin (macos platform)
          (and (and sys/macp
                    (executable-find "pbcopy"))
               'pbpaste)
          ;; cygwin (windows posix emulator)
          (and sys/cygwinp
               (executable-find "getclip")
               'getclip)
          ;; android termux emulator
          (and (executable-find "termux-clipboard-get")
               'termux-clipboard-get)
          ;; microsoft wsl env
          (and sys/wsl2-env-p
               (executable-find "powershell.exe")
               'powershell)
          ;; gnu/linux platform
          (and sys/linuxp
               (or
                (and (executable-find "xclip") 'xclip)
                (and (executable-find "xsel") 'xsel)
                (and (executable-find "wl-copy") 'wl-copy)))

          ;; NOTE: we must disable the native gui support method to
          ;; prevent it make a invisible frame to build connection with
          ;; current tui session.
          ;;
          ;;(and (fboundp 'x-create-frame) (getenv "DISPLAY") 'emacs)
          )))
    (when (and
           ;; just satisfied return when in non-gui session since the
           ;; gui session has full support for system<->emacs
           ;; clipboard sync functional
           (not (display-graphic-p))
           (fboundp 'xterm-paste)
           (when (ignore-errors (or (executable-find (symbol-name judger))
                                    ;; in windows wsl env we must use
                                    ;; the `powershell' exe name to
                                    ;; get it.
                                    (when (eq judger 'powershell)
                                      (executable-find "powershell.exe"))))
             (if (bound-and-true-p xclip-mode)
                 t
               (progn (require 'xclip)
                      (setq xclip-method judger
                            xclip-program (if (eq judger 'powershell)
                                              ;; NOTE: WSLg can not
                                              ;; found non *.exe named
                                              ;; executable
                                              (executable-find "powershell.exe")
                                            (symbol-name judger)))
                      (xclip-mode 1)
                      t))))
      judger)))

;; Inspired by
;; https://emacs-china.org/t/wsl-emacs-windows/17375/3?u=angelaneia
(defun entropy/emacs-windows-env/copy-to-clipboard-core
    (str)
  "Push string STR into WINDOWS system clipboard.

NOTE: no warranty use in other system."
  (with-temp-buffer
    (insert str)
    (call-process-region
     (point-min) (point-max)
     "clip.exe" nil 0)))

(defun entropy/emacs-windows-env/get-from-clipboard-core
    (&rest _)
  "Get last clipbaord item form WINDOWS system.

NOTE: no warranty use in other system."
  (let ((coding-system-for-read 'dos))
    ;; remove added trailing newline of CBK
    (substring
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard")
     0 -1)))

;; NOTE: there's no need to use for now Now [2022-03-03 Thu 15:00:55,
;; since WSLg can handle the system<->wsl correctly. If used will
;; cause the native clipboard transfer block timeout for
;; `gui-get-selection'
;;
;; (defun __adv/around/kill-new/windows-subsystem-env
;;     (orig-func &rest orig-args)
;;   "Kill things also put in windows system clipboard when
;; `sys/wsl2-env-p'."
;;   ;; just WSLg env since `xclip-mode' not enabled in that case
;;   (when sys/wsl2g-env-p
;;     (let ((str (car orig-args)))
;;       (entropy/emacs-windows-env/copy-to-clipboard-core
;;        str)))
;;   (apply orig-func orig-args))
;;
;; (advice-add 'kill-new
;;             :around
;;             #'__adv/around/kill-new/windows-subsystem-env)

(defun entropy/emacs-xterm-paste-core (event)
  "The eemacs kill-ring update function for monitoring
`xterm-paste' event to automatically traceback to `kill-ring'
when the last event contet doesn't change, this useful to prevent
yanking an obsolete entry from `kill-ring' when the emacs
internal cut operation has updated the kill-ring but
`xterm-paste' will still yank the previouse event content."
  (let* ((paste-str (nth 1 event)))
    (with-temp-buffer
      (unless (string= paste-str
                       entropy/emacs--xterm-clipboard-head)
        (progn (setq entropy/emacs--xterm-clipboard-head
                     paste-str)
               (xterm-paste event)))
      (yank))))

(defmacro entropy/emacs-with-xterm-paste-core (event &rest body)
  "Do BODY within the `kill-ring' update by
`entropy/emacs-xterm-paste-core' and with `inhibit-read-only' may
be set while any judger of `entropy/emacs-xterm-paste-inhibit-read-only-filter'
are triggered."
  `(let ((inhibit-read-only
          (catch :exit
            (dolist (filter entropy/emacs-xterm-paste-inhibit-read-only-filter)
              (when (and (functionp filter)
                         (funcall filter event))
                (throw :exit t))))))
     (entropy/emacs-xterm-paste-core ,event)
     ,@body))

(defun entropy/emacs-xterm-paste (event)
  "eemacs wrapper for `xterm-paste' based on the subroutine of
`entropy/emacs-xterm-paste-core' and
`entropy/emacs-with-xterm-paste-core'.

On the other hand, this function use `yank' or the specified yank
like function in
`entropy/emacs-xterm-paste-yank-replacement-register' to yank the
content in `kill-ring' to adapt any occasion which the origin
`xterm-paste' may no be proper as is.
"
  (interactive "e")
  (entropy/emacs-with-xterm-paste-core
   event
   (let (yank-func)
     (catch :exit
       (dolist (pattern entropy/emacs-xterm-paste-yank-replacement-register)
         (when
             (cond ((functionp (car pattern))
                    (funcall (car pattern)))
                   (t
                    (car pattern)))
           (setq yank-func (cdr pattern))
           (throw :exit nil))))
     (if (functionp yank-func)
         (funcall yank-func)
       (yank)))))

(defun entropy/emacs-xterm-term-S-insert (event)
  "eemacs wrapper for `term-send-raw-string' based on the
subroutine of `entropy/emacs-xterm-paste-core'.
"
  (interactive "e")
  (entropy/emacs-with-xterm-paste-core
   event
   (let* ((paste (with-temp-buffer
                   (yank)
                   (car kill-ring))))
     (when (and (stringp paste)
                (not (string= "" paste)))
       (setq paste (substring-no-properties paste))
       (term-send-raw-string paste)))))

;; *** Emacs daemon specification

(defvar entropy/emacs-with-daemon-make-frame-done-error-log nil
  "Log list error message of run of
`entropy/emacs-daemon-server-after-make-frame-hook' which each
function made by `entropy/emacs-with-daemon-make-frame-done'.")

(defun entropy/emacs-with-daemon-make-frame-done
    (name et-form ec-form &optional common-form)
  "Do sth after emacs daemon make a new frame.

- 'ET-FORM' is the form for cli emacs session
- 'EC-FORM' is the form for gui emacs-session

Optional form COMMON-FORM run directly after ET-FORM and EC-FORM
without any condition judgements.

Return the hooker symbol.

*For eemacs developer:*

We never allowed any error occurred inside of user spec forms but
stored the error log in
`entropy/emacs-with-daemon-make-frame-done-error-log'."
  (let* ((--name--
          (intern
           (format "%s-for-emacs-daemon"
                   (symbol-name name)))))
    (entropy/emacs-eval-with-lexical
     `(entropy/emacs-add-hook-lambda-nil
       ,--name--
       entropy/emacs-daemon-server-after-make-frame-hook
       'append
       (condition-case error
           (progn
             (if (display-graphic-p)
                 ,ec-form
               ,et-form)
             ,common-form)
         (error
          (push (format "[%s]: time: (%s) display-type: (%s) error: (%S)"
                        ,--name--
                        (format-time-string "%Y-%m-%d %a %H:%M:%S")
                        (display-graphic-p)
                        error)
                entropy/emacs-with-daemon-make-frame-done-error-log)))))))

(when (daemonp)
  ;; reset icon displayable cache while daemon frame makeup
  (entropy/emacs-with-daemon-make-frame-done
   'reset-icon-displayable-cache
   nil nil '(entropy/emacs-icons-displayable-p t)))

;; *** Proxy specification
;; **** process env with eemacs union internet proxy
(defvar entropy/emacs--noproxy-list-cache nil)
(defvar entropy/emacs--noproxy-string-cache nil)
(defun entropy/emacs-gen-eemacs-union-proxy-noproxy-envs (noproxy-list &optional list-return)
  "Generate comma separated no proxy patterns string from
NOPROXY-LIST which usually obtained from `entropy/emacs-union-proxy-noproxy-list'.

Return a list of thus when LIST-RETURN is non-nil."
  (catch :exit
    (cond (list-return
           (and entropy/emacs--noproxy-list-cache
                (throw :exit entropy/emacs--noproxy-list-cache)))
          (t
           (and entropy/emacs--noproxy-string-cache
                (throw :exit entropy/emacs--noproxy-string-cache))))
    (let ((noproxy-string "") list-rtn)
      (dolist (el noproxy-list)
        (cond ((and (listp el)
                    (not (null el)))
               (if list-return
                   (let ((range-list
                          (entropy/emacs-generate-symbols-or-strings-from-range-desc
                           el)))
                     (setq list-rtn (append list-rtn range-list)))
                 (let ((range-str
                        (entropy/emacs-generate-symbols-or-strings-from-range-desc
                         el nil t ",")))
                   (setq noproxy-string
                         (if (not (string-empty-p noproxy-string))
                             (format "%s,%s" noproxy-string range-str)
                           range-str)))))
              ((stringp el)
               (if list-return
                   (setq list-rtn (append list-rtn (list el)))
                 (setq noproxy-string
                       (if (string-empty-p noproxy-string)
                           (format "%s" el)
                         (format "%s,%s" noproxy-string el)))))))
      (if list-return
          (setq entropy/emacs--noproxy-list-cache list-rtn)
        (setq entropy/emacs--noproxy-string-cache noproxy-string)))))

(defvar entropy/emacs--proxy-env-cache nil)
(defun entropy/emacs-gen-eemacs-union-http-internet-proxy-envs ()
  "Generate list of http proxy env var/value paires sourced from
`entropy/emacs-union-http-proxy-plist'."
  (or entropy/emacs--proxy-env-cache
      (let* ((proxy-plist entropy/emacs-union-http-proxy-plist)
             (proxy-env
              `(,(format "http_proxy=http://%s:%s"
                         (plist-get proxy-plist :host)
                         (number-to-string (plist-get proxy-plist :port)))
                ,(format "https_proxy=http://%s:%s"
                         (plist-get proxy-plist :host)
                         (number-to-string (plist-get proxy-plist :port)))
                ,(format "HTTP_PROXY=http://%s:%s"
                         (plist-get proxy-plist :host)
                         (number-to-string (plist-get proxy-plist :port)))
                ,(format "HTTPS_PROXY=http://%s:%s"
                         (plist-get proxy-plist :host)
                         (number-to-string (plist-get proxy-plist :port))))))
        ;; inject noproxy ip addresses
        (let ((noproxy-list entropy/emacs-union-proxy-noproxy-list))
          (when noproxy-list
            (setq proxy-env
                  (append
                   proxy-env
                   (let ((noproxy-str (entropy/emacs-gen-eemacs-union-proxy-noproxy-envs noproxy-list)))
                     (list (format "no_proxy=%s" noproxy-str)
                           (format "NO_PROXY=%s" noproxy-str)))))))
        ;;return
        (setq entropy/emacs--proxy-env-cache proxy-env)
        proxy-env)))

(defvar entropy/emacs--url-proxy-services-cache nil)
(defun entropy/emacs-gen-eemacs-union-http-internet-proxy-url-proxy-services ()
  "Generate a list formed to used for set `url-proxy-services'
sourced from `entropy/emacs-union-http-proxy-plist'."
  (or entropy/emacs--url-proxy-services-cache
      (let* ((proxy-plist entropy/emacs-union-http-proxy-plist)
             (target (format "%s:%s"
                             (plist-get proxy-plist :host)
                             (number-to-string (plist-get proxy-plist :port)))))
        (setq entropy/emacs--url-proxy-services-cache
              (list (cons "http" target)
                    (cons "https" target)
                    (cons "ftp" target)
                    (cons "no_proxy" (concat
                                      "^\\("
                                      (mapconcat
                                       'identity (entropy/emacs-gen-eemacs-union-proxy-noproxy-envs
                                                  entropy/emacs-union-proxy-noproxy-list
                                                  t)
                                       "\\|")
                                      "\\)"
                                      )))))))

(defvar entropy/emacs-union-http-prroxy-internal-enable-p nil
  "The internal set for
`entropy/emacs-funcall-with-eemacs-union-http-internet-proxy'
when the proxy env wrapping enabled")

(defvar entropy/emacs-union-http-internet-proxy-extra-let-bindings nil
  "Extra `let' bindings to
`entropy/emacs-gen-eemacs-union-http-internet-proxy-let-bindings'")
(defun entropy/emacs-gen-eemacs-union-http-internet-proxy-let-bindings ()
  "Generate `let' bindings to
`entropy/emacs-funcall-with-eemacs-union-http-internet-proxy',
inject
`entropy/emacs-union-http-internet-proxy-extra-let-bindings' at
the tail of the bindings.

NOTE: this bindings just used for `let', in which case do not use
inheritance bindings as in `let*' or will make any undefined
mistakes."
  `((entropy/emacs-union-http-prroxy-internal-enable-p t)
    (url-proxy-services
     ',(entropy/emacs-gen-eemacs-union-http-internet-proxy-url-proxy-services))
    (process-environment ',(append (entropy/emacs-gen-eemacs-union-http-internet-proxy-envs)
                                   process-environment))
    ;; disable `entropy-proxy-url' patch
    (entropy/proxy-url-user-proxy-match-func (lambda (&rest _) nil))
    (entropy/proxy-url-inhbit-all-proxy t)

    ;; user specs
    ,@entropy/emacs-union-http-internet-proxy-extra-let-bindings
    ))

(defvar __ya/timer-set-function/with-url-poroxy/register nil)
(defun __ya/timer-set-function/with-url-proxy (orig-func &rest orig-args)
  "Like `timer-set-function' but patched with url proxy about of
`entropy/emacs-funcall-with-eemacs-union-http-internet-proxy' for
timer since timer is delayed call that can not inherit the
lexical binding."
  (if entropy/emacs-union-http-prroxy-internal-enable-p
      (let ((func-call (nth 1 orig-args))
            func)
        (setq func `(lambda (&rest args)
                      (let ((this-func ',func-call)
                            ,@(entropy/emacs-gen-eemacs-union-http-internet-proxy-let-bindings))
                        (apply this-func args))))
        (push (cons func-call func) __ya/timer-set-function/with-url-poroxy/register)
        (apply orig-func (car orig-args) func (cddr orig-args)))
    (apply orig-func orig-args)))
(advice-add 'timer-set-function :around #'__ya/timer-set-function/with-url-proxy)

(defun __ya/cancel-function-timers/with-url-proxy (orig-func &rest orig-args)
  "Like  `cancel-function-timer' but since `__ya/timer-set-function/with-url-proxy'."
  (let ((func (car orig-args))
        remain)
    (dolist (el __ya/timer-set-function/with-url-poroxy/register)
      (if (eq (car el) func)
          (apply orig-func (list (cdr el)))
        (push el remain)))
    (when remain
      (setq __ya/timer-set-function/with-url-poroxy/register remain))
    (apply orig-func orig-args)))
(advice-add 'cancel-function-timers :around #'__ya/cancel-function-timers/with-url-proxy)

(defun entropy/emacs-funcall-with-eemacs-union-http-internet-proxy
    (filter-func orig-func &rest orig-args)
  "Funcall ORIG-FUNC with ORIG-ARGS using
`entropy/emacs-union-http-proxy-plist' as source http_proxy
descriptor used to wrapping them in let binding for
`process-environment' when the return of FILTER-FUNC(i.e. a
function called without any arguments) is non-nil.

Additionally, the ORIG-FUNC can retrieve whether proxy wrapper enabled
by get `entropy/emacs-union-http-prroxy-internal-enable-p' non-nil."
  (if (and (plist-get entropy/emacs-union-http-proxy-plist :enable)
           (funcall filter-func))
      (entropy/emacs-eval-with-lexical
       `(let ,(entropy/emacs-gen-eemacs-union-http-internet-proxy-let-bindings)
          (apply ',orig-func ',orig-args)))
    (apply orig-func orig-args)))

(defun entropy/emacs-advice-for-common-do-with-http-proxy
    (orig-func &rest orig-args)
  "Around advice for ORIG-FUNC for let binding http_proxy with
`entropy/emacs-union-http-proxy-plist'."
  (apply
   'entropy/emacs-funcall-with-eemacs-union-http-internet-proxy
   (lambda nil t)
   orig-func orig-args))

;; ** Major-modes' eemacs extra APIs
;; *** dired mode

(eval-when-compile
  (require 'dired))

(defun __entropy/emacs-dired-movement-arg-error (arg)
  (or (entropy/emacs-natural-number-p (abs arg))
      (error "dired movement arg %s is not a non-zero integer"
             arg)))

(defun entropy/emacs-dired-move-to-filename (&rest args)
  "Move to the beginning of the filename on the current line but
without movement and change any global state when no filename in
current dired line. Return the position of the beginning of the
filename, or nil if none found.

ARGS is `apply' to `dired-move-to-filename'."
  (let ((final-pt
         (entropy/emacs-save-excurstion-and-mark-and-match-data
           (apply 'dired-move-to-filename args))))
    (when final-pt
      (goto-char final-pt)
      final-pt)))

(cl-defun entropy/emacs-dired-fname-line-p (&key position raise-error move-to-filename)
  "Return non-nil when the line at `current-buffer''s
position (default to current `point') is a dired line with
filename without movement.

POSITION must be predicated by `entropy/emacs-buffer-position-p'
with buffer visible portion check or an error will be thrown
out. If POSTION is a marker, only its `marker-position' is used.

If raise-error is set, error when the line is not thus.

This function does not move point or change global state,
including the match data defaulty unless MOVE-TO-FILENAME is set and
see below:

when MOVE-TO-FILENAME is set, move to the beginning of the
filename on the line at POSITION only when the result is non-nil,
Otherwise no movement did.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (setq position (or position (point)))
  (entropy/emacs-use-markers-position position)
  (entropy/emacs-buffer-position-p position :do-error t :with-range-check t)
  (let (rtn success-p (pt position))
    (entropy/emacs-save-excurstion-and-mark-and-match-data
      (goto-char pt)
      (setq rtn
            (when (entropy/emacs-dired-move-to-filename)
              (setq success-p t)
              (point))))
    (when (and raise-error (not success-p))
      (error "dired line at pt %d has no filename" pt))
    (catch :exit
      (unless success-p
        (throw :exit nil))
      (unless move-to-filename
        (throw :exit t))
      (goto-char rtn)
      t)))

(cl-defun entropy/emacs-dired-marked-line-p (&key position raise-error move-to-filename)
  "Return non-nil when the line at `current-buffer''s
POSITION (default to current `point') is a dired line with
marked.

POSITION must be predicated by `entropy/emacs-buffer-position-p'
with buffer visible portion check or an error will be thrown
out. If POSTION is a marker, only its `marker-position' is used.

Error while the line is not makred when RAISE-ERROR is set.

This function does not move point or change global state,
including the match data defaulty unless MOVE-TO-FILENAME is set
and see below:

when MOVE-TO-FILENAME is set, move to the beginning of the
filename on the current line after successfully movement
done. Return nil when movement can not be did without movement.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (setq position (or position (point)))
  (entropy/emacs-use-markers-position position)
  (entropy/emacs-buffer-position-p position :do-error t :with-range-check t)
  (let ((pt position)
        success-p rtn)
    (setq rtn
          (entropy/emacs-save-excurstion-and-mark-and-match-data
            (goto-char pt)
            (goto-char (line-beginning-position))
            (when (re-search-forward dired-re-mark (line-end-position) t)
              (setq success-p t)
              (point))))
    (catch :exit
      (when (and raise-error (not success-p))
        (error "dired line at pt %d is not marked" pt))
      (unless success-p
        (throw :exit nil))
      (if move-to-filename
          (progn
            (setq rtn (entropy/emacs-save-excurstion-and-mark-and-match-data
                        (goto-char rtn)
                        (when (entropy/emacs-dired-move-to-filename)
                          (point))))
            (unless rtn
              (throw :exit nil))
            (goto-char rtn)
            t)
        t))))

(cl-defun entropy/emacs-dired-next-line (arg &key move-to-line-beginning-position)
  "Move down lines then position at filename when result is non-nil
see below.

Return non-nil if the target line (i.e. the line of position
after move down) is predicated by
`entropy/emacs-dired-fname-line-p'

ARG says how many lines to move; default is one line. When ARG is
negative then move backward previous line ARG times. When visuble
portion has no enough lines to satisfied ARG then move to the
corresponding edge i.e. `point-max' when arg is positive or
`point-min' otherwise.

When MOVE-TO-LINE-BEGINNING-POSITION is non-nil, move `point' to
the `line-beginning-position' after move down done.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (__entropy/emacs-dired-movement-arg-error arg)
  (let (rtn)
    (let ((line-move-visual)
          (goal-column))
      (condition-case err
          (line-move arg nil)
        (error
         (let ((errsym (car err)))
           (cond
            ((not (member errsym '(beginning-of-buffer end-of-buffer)))
             (error "[entropy/emacs-dired-next-line]: internal error (linve-move implementation changed)"))
            (t
             (if (< arg 0)
                 (goto-char (point-min))
               (goto-char (point-max)))))))))
    ;; We never want to move point into an invisible line.
    (while (and (invisible-p (point))
                (not (if (< arg 0) (bobp) (eobp))))
      (forward-char (if (< arg 0) -1 1)))
    (setq rtn (entropy/emacs-dired-fname-line-p :move-to-filename t))
    (when move-to-line-beginning-position
      (goto-char (line-beginning-position)))
    rtn))

(cl-defun entropy/emacs-dired-jump-to-next-fname-line
    (arg &key
         use-current-line
         use-filter
         bound
         raise-error
         move-to-filename)
  "Jump to next dired fname line ARG times.

ARG says how many times to move; default is one. When ARG is
negative then move backward ARG times.

When USE-FILTER is set, it should be a function without arguments
request, run at a valid dired fname line and return non-nil to
inidicate current dired line is satisfied expection. Or it can be
set as one of follow valid values:

- 'mark': when a dired line is a fname line and predicated by
  `entropy/emacs-dired-marked-line-p'.

Defaultly this function internally just use
`entropy/emacs-dired-fname-line-p' as USE-FILTER.

BOUND is a buffer `point' restrict the search boundary.

Return nil when no next satisfied dired fname line found and
without movement. And raise a error when RAISE-ERROR is set.

Return non-nil when found next satisfied dired fname line and
move to that line.

When USE-CURRENT-LINE is set, when current dired line is a
satisfied dired fname line then return non-nil immediately and
without movement unless MOVE-TO-FILENAME is set, see below:

When MOVE-TO-FILENAME is set, move to the beginning of the
filename on the current line after successfully movement done.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (__entropy/emacs-dired-movement-arg-error arg)
  (let* ((steps (if arg arg 1))
         (bound (or bound (if (< steps 0) (point-min) (point-max))))
         (use-marked (eq use-filter 'mark))
         (filter-func (cond (use-marked
                             'entropy/emacs-dired-marked-line-p)
                            ((functionp use-filter)
                             use-filter)
                            (t
                             (lambda nil t))))
         (fln-judge-func
          (lambda ()
            (and (entropy/emacs-dired-fname-line-p)
                 (funcall filter-func))))
         (atept-func (lambda ()
                       (if (> steps 0)
                           (>= (point) bound)
                         (<= (point) bound))))
         final-pt success-p no-need-mvtf)
    (catch :exit
      (when use-current-line
        (when (funcall fln-judge-func)
          (when move-to-filename
            (entropy/emacs-dired-move-to-filename))
          (throw :exit t)))
      (entropy/emacs-save-excurstion-and-mark-and-match-data
        ;; escape current line
        (goto-char (line-end-position))
        (cond
         (use-marked
          (let ((continue-p t))
            (while (and continue-p (not (funcall atept-func)))
              (when (and (re-search-forward dired-re-mark bound 'move steps)
                         (funcall fln-judge-func))
                (setq continue-p nil
                      success-p t)))))
         (t
          (setq no-need-mvtf t)
          (let ((continue-p t)
                (succnt 0))
            (while (and continue-p (not (funcall atept-func)))
              (entropy/emacs-dired-next-line (if (< steps 0) -1 1))
              (when (funcall fln-judge-func)
                (cl-incf succnt)
                (when (= succnt (abs steps))
                  (setq continue-p nil
                        success-p t)))))))
        (when success-p
          (when (and (not no-need-mvtf) move-to-filename)
            (entropy/emacs-dired-move-to-filename))
          (setq final-pt (point))))
      (if final-pt (goto-char final-pt))
      (when (and raise-error (not success-p))
        (error "jump to next dired fname line fatal"))
      success-p)))

(cl-defun entropy/emacs-dired-map-lines (&key use-region map-func)
  "Call MAP-FUNC for all dired lines in `current-buffer' without
arguments injection.

The mapping procedure from begin and end of USE-REGION, its a
cons of car of a begin marker so as cdr, default to the markers
set by `point-min' and `point-max'.

MAP-FUNC return 'pause-iterate' is used to notify the mapping
procedure do not move to next dired line at next iteration in
which case like dired line deletion operation in MAP-FUNC will
let next dired line be current dired line in current iteration
which will cause next iteration skip this line but its not our
expection.

MAP-FUNC return 'stop-iterate' is used forcely exist whole
looping procedure.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (let* ((use-region (or use-region
                         (cons (set-marker (make-marker) (point-min))
                               (set-marker (make-marker) (point-max)))))
         (begin-marker (car use-region))
         (end-marker (cdr use-region))
         (init t) (map-return nil)
         (atept-func
          (lambda ()
            (let ((pt (point)))
              (entropy/emacs-swap-two-places-value
                begin-marker end-marker
                (< end-marker begin-marker))
              (or (>= pt end-marker)
                  (funcall (if init 'ignore '<=)
                           pt begin-marker)))))
         (map-func (or map-func 'ignore)))
    (goto-char begin-marker)
    (catch :exit
      (while (not (funcall atept-func))
        (setq init nil)
        (setq map-return (funcall map-func))
        (if (eq map-return 'stop-iterate)
            (throw :exit nil))
        (unless (eq map-return 'pause-iterate)
          (entropy/emacs-dired-next-line 1))))))

(defun entropy/emacs-dired-region-has-marked-lines-p (start end &optional return-pos)
  "Return non-nil when the region of START and END of
`current-buffer' (must be dired buffer) has
`entropy/emacs-dired-marked-line-p' dired lines.

START and END must be predicated by
`entropy/emacs-buffer-position-p' with buffer visible portion
check or an error will be thrown out. If POSTION is a marker,
only its `marker-position' is used.

If RETURN-POS is set and origin return is non-nil, return a
`point' based on first found dired markup line in that region and
it should be in one of follow types:

- symbol 'line-end'  : return `line-end-position' of that line.

- symbol 'line-begin': return `line-beginning-position' of that
  line.

- a integer : return the position which is the
  `line-beginning-position' of that line plus the integer but
  stop to `point-max' when overflow.

- default return `line-beginning-position' for any other non-nil
  object.

This function does not move point or change global state,
including the match data defaulty.
"
  (entropy/emacs-do-error-for-major-moe-incompatible 'dired-mode)
  (let (rtn)
    (save-excursion
      (entropy/emacs-dired-map-lines
       :use-region (cons (set-marker (make-marker) start)
                         (set-marker (make-marker) end))
       :map-func
       (lambda (&rest _)
         (when (entropy/emacs-dired-marked-line-p)
           (setq rtn
                 (cond
                  ((eq return-pos 'line-end)
                   (line-end-position))
                  ((eq return-pos 'line-begin)
                   (line-beginning-position))
                  ((integerp return-pos)
                   (min (point-max)
                        (+ (line-beginning-position)
                           return-pos)))
                  (return-pos
                   (line-beginning-position))
                  (t t)))
           'stop-iterate))))
    rtn))

;; * provide
(provide 'entropy-emacs-defun)
