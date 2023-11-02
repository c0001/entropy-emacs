;;; entropy-emacs-defgeneric.el --- entropy emacs generic framework  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defgeneric.el
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
;; * Code
;; ** Libs
(require 'entropy-emacs-defun)

(defun entropy/emacs/generic//func/get-cl-defmethod-args-plist
    (&rest args)
  (let* ((name (pop args))
         (extra (and (eq (car args) :extra)
                     (prog1 (cadr args)
                       (setq args (cddr args)))))
         (quli (and (keywordp (car args))
                    ;; since qualifier is a keyword so that we should
                    ;; set that plist slot via escape its keyword type
                    ;; feat to prevent ambiguous meaning for `plist'
                    ;; structure.
                    (list (pop args))))
         (arglist (pop args))
         (doc (and (stringp (car args)) (pop args)))
         (body args))
    (list :name name :extra extra :qualifier quli
          :arglist arglist :doc doc :body body)))

(defun entropy/emacs/generic//func/gen-cl-defmethod-args (ap)
  (let* ((name (plist-get ap :name))
         (extra (plist-get ap :extra))
         (quali (car (plist-get ap :qualifier)))
         (arglist (plist-get ap :arglist))
         (doc (or (plist-get ap :doc) ""))
         (body (plist-get ap :body))
         (rtn (list name)))
    (when extra (push :extra rtn)
          (push extra rtn))
    (when quali (push quali rtn))
    (push arglist rtn)
    (when doc (push doc rtn))
    (append (nreverse rtn) body)))

;; ** Defgeneric

(eval-and-compile
  (cl-defmacro entropy/emacs/generic//macro/cl-defstruct
      (name &optional doc-string &rest slots
            &key
            with-obj-name
            with-judge-macro-name
            with-judge-return-func-name
            &allow-other-keys)
    "

\(fn NAME &optional DOCSTRING &rest SLOTS &key with-obj-name \
with-judge-macro-name \
with-judge-return-func-name &allow-other-keys)"
    (declare (doc-string 2) (indent 2))
    (setq with-obj-name (or with-obj-name name))
    (let* ((oname name)
           (stopts (if (consp name) (cdr name)))
           (name (if (consp name) (car name) name))
           (type-pred-func-name
            (let ((val (alist-get :predicate stopts)))
              (or (and val (car val))
                  (intern (format "%s-p" (symbol-name name))))))
           (with-judge-macro-name
            (or with-judge-macro-name
                (intern (format "%s/macro/with-judge" (symbol-name name)))))
           (with-judge-return-func-name
            (or with-judge-return-func-name
                (intern (format "%s/pred/with-judge/return" (symbol-name name)))))

           (type-pred-func-name-sym-cons
            (cons (entropy/emacs-make-new-interned-symbol
                   (format "\
entropy/emacs/generic//var/sym/__type-pred-func-name__/%s/"
                           name))
                  type-pred-func-name))
           (type-pred-func-name-sym (car type-pred-func-name-sym-cons))

           (with-judge-macro-name-sym-cons
            (cons (entropy/emacs-make-new-interned-symbol
                   (format "\
entropy/emacs/generic//var/sym/__with-judge-macro-name__/%s/"
                           name))
                  with-judge-macro-name))
           (with-judge-macro-name-sym (car with-judge-macro-name-sym-cons))

           (with-judge-return-func-name-sym-cons
            (cons (entropy/emacs-make-new-interned-symbol
                   (format "\
entropy/emacs/generic//var/sym/__with-judge-return-func-name__/%s/"
                           name))
                  with-judge-return-func-name))
           (with-judge-return-func-name-sym
            (car with-judge-return-func-name-sym-cons)))

      (setq slots (entropy/emacs-defun--get-real-body slots))

      (let
          (_)
        `(progn
           ,@(cl-loop for el in `(,type-pred-func-name-sym-cons
                                  ,with-judge-macro-name-sym-cons
                                  ,with-judge-return-func-name-sym-cons)
                      collect `(progn
                                 (eval-when-compile (defvar ,(car el) ',(cdr el)))
                                 (entropy/emacs-defconst ,(car el) ',(cdr el))))
           (cl-defstruct ,oname ,doc-string ,@slots)
           (unless (fboundp ,type-pred-func-name-sym)
             (error "CL-X predicate function `%s' not found after `cl-defstruct' defination!"
                    ,type-pred-func-name-sym))
           (eval-and-compile
             (cl-defmacro ,with-judge-macro-name
                 (,with-obj-name &rest body &key with-error &allow-other-keys)
               ,(format-spec "\
Do BODY and return its value only when %v is a CL-X of `%f',
otherwise return nil without doing BODY.

If WITH-ERROR is non-nil then raise an error while %v is not that
type without doing BODY."
                             `((?v . ,(upcase (symbol-name with-obj-name)))
                               (?f . ,(symbol-name type-pred-func-name))))
               (declare (indent 1))
               (setq body (entropy/emacs-defun--get-real-body body t))
               (macroexp-let2* ignore
                   ((objsym ,with-obj-name)
                    (with-err-sym with-error)
                    (type-pred-func-name-sym1
                     (list 'quote ,type-pred-func-name-sym))
                    (with-judge-macro-name-sym1
                     (list 'quote ,with-judge-macro-name-sym))
                    (with-judge-return-func-name-sym1
                     (list 'quote ,with-judge-return-func-name-sym)))
                 `(progn
                    (if (funcall ,type-pred-func-name-sym1 ,objsym) (progn ,@body)
                      (when ,with-err-sym
                        (error "%s %s"
                               (format-spec
                                "Wrong type arguments: %s"
                                (list (cons ?s (symbol-name ,type-pred-func-name-sym1))))
                               ,objsym)))))))
           (eval-and-compile
             (defun ,with-judge-return-func-name (,with-obj-name &optional with-error)
               ,(format-spec "\
Return %s when %s is a CL-X of %o or nil (error when WITH-ERROR is non-nil)."
                             `((?s . ,(upcase (symbol-name with-obj-name)))
                               (?o . ,(upcase (symbol-name name)))))
               (,with-judge-macro-name
                ,with-obj-name :with-error with-error
                ,with-obj-name)))
           )))))

(eval-and-compile
  (entropy/emacs/generic//macro/cl-defstruct
      (entropy/emacs/generic//obj/generic-function
       (:constructor nil)
       (:constructor entropy/emacs/generic//obj/generic-function/pred/make)
       (:predicate   entropy/emacs/generic//obj/generic-function/pred/typep)
       (:copier      entropy/emacs/generic//obj/generic-function/pred/copy)
       (:conc-name   entropy/emacs/generic//obj/generic-function/slot/))
      "eemacs generic function object"
    :with-obj-name obj/generic-function
    :with-judge-macro-name
    entropy/emacs/generic//obj/generic-function/pred/with-judge
    :with-judge-return-func-name
    entropy/emacs/generic//obj/generic-function/pred/with-judge-return
    ;; slots
    name doc require-define
    adv-before adv-after adv-around
    )

  (entropy/emacs/generic//macro/cl-defstruct
      (entropy/emacs/generic//obj/generic-function-group
       (:constructor nil)
       (:constructor entropy/emacs/generic//obj/generic-function-group/pred/make)
       (:predicate   entropy/emacs/generic//obj/generic-function-group/pred/typep)
       (:copier      entropy/emacs/generic//obj/generic-function-group/pred/copy)
       (:conc-name   entropy/emacs/generic//obj/generic-function-group/slot/))
      "eemacs generic function group object"
    :with-obj-name obj/generic-function-group
    :with-judge-macro-name
    entropy/emacs/generic//obj/generic-function-group/pred/with-judge
    :with-judge-return-func-name
    entropy/emacs/generic//obj/generic-function-group/pred/with-judge-return
    ;; slots
    name doc
    (ring::obj/generic-function (make-ring 10))))

(defun entropy/emacs/generic//func/gen/generic-function-sym
    (generic-function-group/name generic-function/name)
  (intern (concat (symbol-name generic-function-group/name)
                  (symbol-name generic-function/name))))

(defun entropy/emacs/generic//obj/generic-function-group/pred/add
    (obj/generic-function-group obj/generic-function)
  (entropy/emacs/generic//obj/generic-function-group/pred/with-judge
      obj/generic-function-group
    :with-error t
    (let* ((r (entropy/emacs/generic//obj/generic-function-group/slot/ring::obj/generic-function
               obj/generic-function-group))
           (f (entropy/emacs/generic//obj/generic-function/pred/with-judge-return
               obj/generic-function t))
           (pf
            (lambda (x y)
              (eq (entropy/emacs/generic//obj/generic-function/slot/name x)
                  (entropy/emacs/generic//obj/generic-function/slot/name y))))
           (m (entropy/emacs-ring-member
               r obj/generic-function pf)))
      (and m (ring-remove r m))
      (entropy/emacs-ring-insert r f))))

(defun entropy/emacs/generic//obj/generic-function-group/pred/del
    (obj/generic-function-group obj/generic-function)
  (entropy/emacs/generic//obj/generic-function-group/pred/with-judge
      obj/generic-function-group
    :with-error t
    (entropy/emacs-ring-remove-all
     (entropy/emacs/generic//obj/generic-function-group/slot/ring::obj/generic-function
      obj/generic-function-group)
     (entropy/emacs/generic//obj/generic-function/pred/with-judge-return
      obj/generic-function t)
     (lambda (x y)
       (eq (entropy/emacs/generic//obj/generic-function/slot/name x)
           (entropy/emacs/generic//obj/generic-function/slot/name y))))))

(defun entropy/emacs/generic//obj/generic-function-group/pred/get
    (obj/generic-function-group generic-function/name &optional must-exist)
  (or
   (entropy/emacs/generic//obj/generic-function-group/pred/with-judge
       obj/generic-function-group
     :with-error t
     (when-let*
         ((r (entropy/emacs/generic//obj/generic-function-group/slot/ring::obj/generic-function
              obj/generic-function-group))
          (rl (ring-elements r))
          (m
           (cl-position-if
            (lambda (x)
              (entropy/emacs/generic//obj/generic-function/pred/with-judge x
                :with-error t
                (eq (entropy/emacs/generic//obj/generic-function/slot/name x)
                    generic-function/name)))
            rl)))
       (nth m rl)))
   (and must-exist
        (error "No such generic function defined in group: `%s' of group `%s'"
               generic-function/name
               (entropy/emacs/generic//obj/generic-function-group/slot/name
                obj/generic-function-group)))))

(defvar entropy/emacs/generic//var/eemacs-spec/cl-defgeneric-opts
  `((:eemacs-require-define
     require-define
     :pred
     ,(lambda (x &rest args)
        (unless args
          (eval `(progn ,@x) lexical-binding))))
    ,@(mapcar
       (lambda (el)
         (list
          (car el) (nth 1 el)
          :place 'after
          :pred
          (lambda (x &rest args)
            (let* ((ap (apply 'entropy/emacs/generic//func/get-cl-defmethod-args-plist
                              args))
                   (adv (nth 2 el)))
              `(progn
                 ,(unless (plist-get ap :qualifier)
                    `(cl-defmethod
                       ,@(progn (entropy/emacs-plist-setf ap :qualifier (list adv))
                                (entropy/emacs-plist-setf
                                 ap :body
                                 `((progn (entropy/emacs-ignore-cl-defmethod-args
                                           ,(plist-get ap :arglist))
                                          ,@x)))
                                (entropy/emacs/generic//func/gen-cl-defmethod-args
                                 ap)))))))))
       '((:eemacs-adv-before adv-before :before)
         (:eemacs-adv-after  adv-after  :after)
         (:eemacs-adv-around adv-around :around)))
    ))

(defun entropy/emacs/generic//func/check-require-defined-methods
    (obj/generic-function-group &rest generic-function/names)
  (when (entropy/emacs-debugger-is-running-p)
    (entropy/emacs/generic//obj/generic-function-group/pred/with-judge
        obj/generic-function-group
      :with-error t
      (when-let*
          ((fobs
            (ring-elements
             (entropy/emacs/generic//obj/generic-function-group/slot/ring::obj/generic-function
              obj/generic-function-group)))
           (efunc (plist-get
                   (assoc :eemacs-require-define
                          entropy/emacs/generic//var/eemacs-spec/cl-defgeneric-opts)
                   :pred))
           (rfnms (entropy/emacs-mapcar-without-orphans
                   (lambda (x)
                     (entropy/emacs/generic//obj/generic-function/pred/with-judge x
                       :with-error t
                       (when
                           (funcall
                            efunc
                            (entropy/emacs/generic//obj/generic-function/slot/require-define
                             x))
                         (entropy/emacs/generic//obj/generic-function/slot/name x))))
                   fobs nil nil)))
        (dolist (el rfnms)
          (unless (memq el generic-function/names)
            (error "Generic function `%s' has not been implemented"
                   el)))))))

(defun entropy/emacs/generic//func/gen/cl-defgeneric-exp
    (gob-sym enablep &rest cl-defgeneric-args)
  (let* ((oargs (copy-sequence cl-defgeneric-args))
         (name (pop cl-defgeneric-args)) (args (pop cl-defgeneric-args))
         (fname (car name)) (name (cdr name))
         (doc (if (stringp (car cl-defgeneric-args)) (pop cl-defgeneric-args)))
         (oslots nil)
         (slots
          (let (rtn)
            (catch :break
              (while cl-defgeneric-args
                (if (and
                     (consp (car cl-defgeneric-args))
                     (memq
                      (caar cl-defgeneric-args)
                      `(:documentation
                        declare
                        :argument-precedence-order
                        :method
                        ,@(mapcar
                           'car
                           entropy/emacs/generic//var/eemacs-spec/cl-defgeneric-opts)
                        )))
                    (push (pop cl-defgeneric-args) rtn)
                  (throw :break nil))))
            (dolist (el rtn)
              (unless (string-match-p ":eemacs-" (symbol-name (car el)))
                (push el oslots)))
            (nreverse rtn))))
    (if (not enablep) `(cl-defgeneric ,@oargs)
      (macroexp-let2* ignore
          ((slots-sym `(list ,@(mapcar (lambda (x) `(quote ,x)) slots)))
           (fob-sym `(entropy/emacs/generic//obj/generic-function/pred/make
                      :name ',name))
           (opts-alist-sym
            `(quote (,@(mapcar (lambda (x) (cons (car x) (cadr x)))
                               entropy/emacs/generic//var/eemacs-spec/cl-defgeneric-opts)))))
        `(progn
           (dolist (el ,opts-alist-sym)
             (setf (cl-struct-slot-value
                    'entropy/emacs/generic//obj/generic-function
                    (cdr el)
                    ,fob-sym)
                   (alist-get (car el) ,slots-sym))
             (setq ,slots-sym (assq-delete-all (car el) ,slots-sym)))
           (entropy/emacs/generic//obj/generic-function-group/pred/add
            ,gob-sym ,fob-sym)
           ,(if doc
                `(cl-defgeneric ,fname ,args
                   ,doc ,@oslots ,@cl-defgeneric-args)
              `(cl-defgeneric ,fname ,args
                 ,@oslots ,@cl-defgeneric-args)))))))

(defvar entropy/emacs/generic//var/generic-function-group/hub nil)
(defun entropy/emacs/generic//func/get/obj/generic-function-group
    (generic-function-group/name &optional must-exist)
  (let ((val (alist-get generic-function-group/name
                        entropy/emacs/generic//var/generic-function-group/hub)))
    (or val
        (and must-exist
             (error "No generic function group registed has name: %s"
                    generic-function-group/name)))))
(defun entropy/emacs/generic//func/gotak-generic-function-group
    (generic-function-group/name &rest other-slots)
  "Get or make if not exist."
  (let ((val (alist-get generic-function-group/name
                        entropy/emacs/generic//var/generic-function-group/hub)))
    (or val
        (prog1
            (setq val
                  (apply 'entropy/emacs/generic//obj/generic-function-group/pred/make
                         :name generic-function-group/name
                         other-slots))
          (push (cons generic-function-group/name val)
                entropy/emacs/generic//var/generic-function-group/hub)))))

;;;###autoload
(cl-defmacro entropy/emacs/generic/macro/defgenerics
    (generic-function-group/name &optional doc &rest args)
  "Define sets of `cl-defgeneric' where each GENERIC in GENERICs should
be a `cl-defgeneric' form but can be any other context related
expressions.

If GENERIC is used with GENERIC-FUNCTION-GROUP/NAME (if non-nil)
then its name is made prefixed by
that. GENERIC-FUNCTION-GROUP/NAME should be explicitly set as a
symbol name if used.

If GENERIC-FUNCTION-GROUP/NAME is used, then each GENERIC form of
`cl-defgeneric' has extra OPTIONS-AND-METHODS has meaning of listing
as below:
- `:eemacs-adv-before' : where its cdr are forms wrap to make a before
  advice (without arglist preserved binding since the arbitrary
  methods self arglist declaration) for later usage of
  `entropy/emacs/generic/macro/defmethods' to define its
  implementation with that before advice just after its main
  implements defined. (This option has no effects when the pre-defined
  method has a qualifier)

- `:eemacs-adv-after'  : as `:eemacs-adv-before' but for after advice.

- `:eemacs-adv-around' : as `:eemacs-adv-before' but for around advice
  and the forms should self-maintained to call `cl-call-next-method'
  to invoke base implements.

\(fn GENERIC-FUNCTION-GROUP/NAME &optional DOC &rest GENERICs)"
  (declare (indent 1) (doc-string 2))
  (let (fns
        (use-gob-sym (make-symbol "obj/generic-function-group"))
        )
    (dolist (el args)
      (cond
       ((eq (car el) 'cl-defgeneric)
        (if (not generic-function-group/name) (push el fns)
          (push
           (apply 'entropy/emacs/generic//func/gen/cl-defgeneric-exp
                  use-gob-sym (not (null generic-function-group/name))
                  (cons
                   (entropy/emacs/generic//func/gen/generic-function-sym
                    generic-function-group/name
                    (cadr el))
                   (cadr el))
                  (cddr el))
           fns)))
       (t (push el fns))))
    (macroexp-let2* ignore ()
      `(let (,use-gob-sym)
         ,(when generic-function-group/name
            `(progn
               (setq ,use-gob-sym
                     (entropy/emacs/generic//func/gotak-generic-function-group
                      ',generic-function-group/name
                      :doc ,doc))))
         ,@(nreverse fns)))))

;; ** Defmethod

(cl-defmacro entropy/emacs/generic//macro/cl-defmethod
    (generic-function-group/name &rest args)
  "

\(fn GENERIC-FUNCTION-GROUP/NAME NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (doc-string 4) (indent 3))
  (if (null generic-function-group/name) `(cl-defmethod ,@args)
    (let* ((ofnm (car args))
           (args
            (cons (entropy/emacs/generic//func/gen/generic-function-sym
                   generic-function-group/name (car args))
                  (cdr args)))
           (gob (entropy/emacs/generic//func/get/obj/generic-function-group
                 generic-function-group/name
                 'must-exist))
           (fob (entropy/emacs/generic//obj/generic-function-group/pred/get
                 gob ofnm 'must-exist))
           (final-exp (list `(cl-defmethod ,@args))))
      (let (slot-name slot-val pred pred-exp place)
        (dolist (el entropy/emacs/generic//var/eemacs-spec/cl-defgeneric-opts)
          (setq slot-name (nth 1 el)
                slot-val (cl-struct-slot-value 'entropy/emacs/generic//obj/generic-function
                                               slot-name fob)
                pred (plist-get el :pred)
                place (plist-get el :place))
          (when slot-val
            (setq pred-exp (apply pred slot-val args))
            (when pred-exp
              (cond ((eq place 'after)
                     (setq final-exp (append final-exp (list pred-exp))))
                    ((eq place 'before)
                     (setq final-exp (append (list pred-exp) final-exp)))
                    (t
                     (setq final-exp (append final-exp (list pred-exp)))))))))
      `(progn ,@final-exp))))

(eval-and-compile
  (defun entropy/emacs/generic//func/make-defmethod-args
      (expr spec &rest args)
    "Make `cl-defmethod's argument list ARGS with specified `&context'
of `(expr spec)', as the first context spec. Return the modified
argument list."
    (let* (rtn
           user-context-args context-set-p ccontext-set-p
           (context-set-func
            (lambda ()
              (push '&context rtn)
              (push `(,expr ,spec) rtn)
              (dolist (ex user-context-args) (push ex rtn))
              (setq context-set-p t))))
      (while args
        (cond ((and (not context-set-p)
                    (memq (car args) '(&rest &optional)))
               (funcall context-set-func))
              ((and (not context-set-p) (not ccontext-set-p)
                    (eq (car args) '&context))
               (pop args)
               (catch :break
                 (while args
                   (cond
                    ((memq (car args) '(&rest &optional))
                     (throw :break nil))
                    (t (push (pop args) user-context-args)))))
               (setq ccontext-set-p t)
               (setq user-context-args (nreverse user-context-args)))
              (t (push (pop args) rtn))))
      (unless context-set-p (funcall context-set-func))
      (nreverse rtn))))

;;;###autoload
(cl-defmacro entropy/emacs/generic/macro/defmethods
    (generic-function-group/name
     &rest args &key with-contexts &allow-other-keys)
  "Define sets of `cl-defmethod' where each METHOD in METHODS usually is
a `cl-defmethod' lisp form but can be any other context related
expressions.

If GENERIC-FUNCTION-GROUP/NAME is non-nil, it should be a explicit
eemacs GENERIC-FUNCTION-GROUP/NAME to indicate each METHOD's name is a
GENERIC-FUNCTION/NAME of a OBJ/GENERIC-FUNCTION defined via
`entropy/emacs/generic/macro/defgenerics' use
GENERIC-FUNCTION-GROUP/NAME. In which case, if no such
GENERIC-FUNCTION-GROUP/NAME is declared before ran of this macro,
error will be raised up. If one METHOD's name is not such a
GENERIC-FUNCTION/NAME in that group, error raised up either.

WITH-CONTEXTS if defined should be a explicit list of `cl-defmethod's
context (expr spec) forms used to make each METHODs' defination has
same context indicator.

\(fn GENERIC-FUNCTION-GROUP/NAME &key WITH-CONTEXTS &rest METHODS)"
  (declare (indent 1))
  (setq args (entropy/emacs-defun--get-real-body args))
  (let (fns fn-name fn-rest fn-args fn-args-index
            (gob-sym (make-symbol "obj/generic-function-group"))
            all-fnsyms)
    (when args
      (when generic-function-group/name
        (push
         `(progn
            (setq ,gob-sym
                  (entropy/emacs/generic//func/get/obj/generic-function-group
                   ',generic-function-group/name
                   'must-exist))
            (ignore ,gob-sym))
         fns))
      (dolist (fn args)
        (if (not (eq (car fn) 'cl-defmethod)) (push fn fns)
          (pop fn)
          (setq fn-name (car fn) fn-rest (cdr fn))
          (push fn-name all-fnsyms)
          (setq fn-args-index
                (or (cl-position-if 'listp fn-rest)
                    (error "Malformed `cl-defmethod' declaration: %S"
                           fn))
                fn-args (nth fn-args-index fn-rest))
          (dolist (el with-contexts)
            (entropy/emacs-setf-by-body fn-args
              (apply 'entropy/emacs/generic//func/make-defmethod-args
                     (car el) (cadr el) fn-args)))
          (entropy/emacs-list-setf-nth
           fn-args-index fn-args fn-rest :with-error t)
          (push `(entropy/emacs/generic//macro/cl-defmethod ,generic-function-group/name
                     ,fn-name ,@fn-rest) fns)))
      `(let (,gob-sym) (ignore ,gob-sym)
            ,@(reverse fns)
            (when (and ,gob-sym
                       (or (bound-and-true-p entropy/emacs-startup-with-Debug-p)
                           (entropy/emacs-debugger-is-running-p)))
              (apply 'entropy/emacs/generic//func/check-require-defined-methods
                     ,gob-sym
                     ,(progn
                        `(list ,@(mapcar (lambda (x) `(quote ,x))
                                         all-fnsyms)))))))))

;; * provide
(provide 'entropy-emacs-defgeneric)
