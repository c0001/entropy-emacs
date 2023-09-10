;; -*- lexical-binding: t; -*-

(require 'cl-lib) (require 'cl-macs)
(require 'shackle)

;; * libs

(cl-defun entropy/shellpop2/core/func/get-def-body (list-var &optional with-safe)
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

;;;###autoload
(defmacro entropy/shellpop2/core/macro/defconst (symbol initvalue &optional docstring)
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

;;;###autoload
(defun entropy/shellpop2/core/func/remove-buffer-window (buffer-or-name)
  (let ((wins (get-buffer-window-list
               buffer-or-name nil (selected-frame)))
        (win-root (window-main-window)) unwin)
    (dolist (win wins)
      (if (eq win win-root) (push win unwin)
        (condition-case nil (delete-window win)
          (error (push win unwin)))))
    (if unwin (warn "\
Some window displayed buffer %S can not remmoved since they are
either root window or with dedicated non-kill properties:
%S"
                    buffer-or-name unwin))))

(declare-function shackle-mode "ext:shackle")
;;;###autoload
(defun entropy/shellpop2/core/func/buffer/shackle-display (buffer-or-name)
  (let* ((shackle-rules
          `((,(if (stringp buffer-or-name) buffer-or-name
                (buffer-name buffer-or-name))
             :regexp nil :select t
             :align 'below :size 0.45 :autoclose t)))
         (shacklep (bound-and-true-p shackle-mode)))
    (unless shacklep (shackle-mode 1))
    (unwind-protect (display-buffer buffer-or-name)
      (unless shacklep (shackle-mode -1)))))

(defun entropy/shellpop2/core//func/ring/insert (ring item)
  (let ((rlen (ring-length ring))
        (rsize (ring-size ring)))
    (if (= rlen rsize) (ring-resize ring (1+ rsize)))
    (ring-insert ring item)))

;;;###autoload
(cl-defmacro entropy/shellpop2/core/macro/with-canbe-cdw
    (uri &rest body &key with-spec-cururi &allow-other-keys)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((uri-sym uri)
       (cur-uri-sym `(or ,with-spec-cururi default-directory)))
    `(progn
       (unless (or (file-equal-p ,uri-sym ,cur-uri-sym)
                   (not (yes-or-no-p (format "CD to `%s'? " ,uri-sym))))
         ,@(entropy/shellpop2/core/func/get-def-body body t)))))

;;;###autoload
(cl-defmacro entropy/shellpop2/core/macro/with-judge/bufferp
    (buffer &rest body &key with-livep &allow-other-keys)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((buffsym buffer))
    `(progn
       (unless (bufferp ,buffsym) (error "Not a buffer: %S" ,buffsym))
       (when ,with-livep
         (unless (buffer-live-p ,buffsym)
           (error "Not a lived buffer: %S" ,buffsym)))
       ,@(entropy/shellpop2/core/func/get-def-body body))))

;;;###autoload
(defun entropy/shellpop2/core/func/local-set-key-batch-do (&rest args)
  "Set `current-local-map' in batch mode with ARGS formed
as (key . command) pair list without pollute same `major-mode'
sharing the same local keymap."
  (let ((map (current-local-map)))
    (if map (setq map (copy-tree map))
      (setq map (make-sparse-keymap)))
    (dolist (el args)
      (let ((key (car el))
            (command (cdr el)))
        (or (vectorp key) (stringp key)
            (signal 'wrong-type-argument (list 'arrayp key)))
        (define-key map key command)))
    (use-local-map map)))

;; * tops

(defvar entropy/shellpop2/core//var/shell-type-flag-sym
  'entropy/shellpop2/core//var/shell/type/name//id:1397442003498390672)
(eval `(defvar ,entropy/shellpop2/core//var/shell-type-flag-sym nil))
(cl-defmacro entropy/shellpop2/core/macro/get/env/shell/type/name nil
  `(progn ,entropy/shellpop2/core//var/shell-type-flag-sym))
(defun entropy/shellpop2/core/func/or/shell/type/name (shell/type/name)
  (or
   shell/type/name
   (entropy/shellpop2/core/macro/get/env/shell/type/name)
   (error "No shell/type/name specifed in current context")))

(cl-defmacro entropy/shellpop2/core/macro/do-with/shell/type/name
    (shell/type/name &rest body)
  (declare (indent 1))
  `(let ((,entropy/shellpop2/core//var/shell-type-flag-sym ,shell/type/name))
     (ignore ,entropy/shellpop2/core//var/shell-type-flag-sym)
     ,(macroexp-progn body)))

(eval-and-compile
  (cl-defmacro entropy/shellpop2/core//macro/cl-defstruct
      (name &optional doc-string &rest slots
            &key
            with-obj-name
            with-judge-macro-name
            with-judge-return-func-name
            &allow-other-keys)
    "

\(fn NAME &optional DOCSTRING &rest SLOTS &key with-obj-name \
with-judge-macro-name \
with-judge-return-func-name \
with-redefine &allow-other-keys)"
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
            (cons (intern (format "\
entropy/shellpop2/core//var/sym/__type-pred-func-name__/%s"
                                  name))
                  type-pred-func-name))
           (type-pred-func-name-sym (car type-pred-func-name-sym-cons))

           (with-judge-macro-name-sym-cons
            (cons (intern (format "\
entropy/shellpop2/core//var/sym/__with-judge-macro-name__/%s"
                                  name))
                  with-judge-macro-name))
           (with-judge-macro-name-sym (car with-judge-macro-name-sym-cons))

           (with-judge-return-func-name-sym-cons
            (cons (intern (format "\
entropy/shellpop2/core//var/sym/__with-judge-return-func-name__/%s"
                                  name))
                  with-judge-return-func-name))
           (with-judge-return-func-name-sym
            (car with-judge-return-func-name-sym-cons)))

      (setq slots (entropy/shellpop2/core/func/get-def-body slots))

      (let
          (_)
        `(progn
           ,@(cl-loop for el in `(,type-pred-func-name-sym-cons
                                  ,with-judge-macro-name-sym-cons
                                  ,with-judge-return-func-name-sym-cons)
                      collect `(eval-and-compile
                                 (entropy/shellpop2/core/macro/defconst
                                   ,(car el) ',(cdr el))))
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
               (setq body (entropy/shellpop2/core/func/get-def-body body t))
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
  (defun entropy/shellpop2/core//func/make-defmethod-args
      (shell/type/name &rest args)
    (let* (rtn
           cargs context-set-p ccontext-set-p
           (context-set-func
            (lambda ()
              (push '&context rtn)
              (push `(,entropy/shellpop2/core//var/shell-type-flag-sym
                      (eql ',shell/type/name))
                    rtn)
              (dolist (ex cargs) (push ex rtn))
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
                    (t (push (pop args) cargs)))))
               (setq ccontext-set-p t) (nreverse cargs))
              (t (push (pop args) rtn))))
      (unless context-set-p
        (funcall context-set-func))
      (nreverse rtn))))

(cl-defmacro entropy/shellpop2/core//macro/defmethod (&rest args)
  (declare (indent defun))
  (let* ((defhead (car args))
         (ofargs-pos (or (cl-position-if (lambda (x) (consp x)) (cdr args))
                         (error "arglist missing")))
         (ofargs (nth ofargs-pos (cdr args)))
         (defargs (cl-loop named l
                           for i in ofargs
                           if (not (eq i '&shell-type-name))
                           collect i into l
                           else return l))
         (shell-type-pos (or (cl-position '&shell-type-name ofargs)
                             (error "shell type name flag not found")))
         shell/type/name body)
    (setq shell/type/name (or (nth (+ shell-type-pos 1) ofargs)
                              (error "shell type name is null"))
          body (nthcdr (+ ofargs-pos 1) (cdr args)))
    (let (_)
      `(cl-defmethod ,defhead
         ,(apply 'entropy/shellpop2/core//func/make-defmethod-args
                 shell/type/name defargs)
         ,@body))))

(defvar entropy/shellpop2/core//var/generic-functions nil)
(setq   entropy/shellpop2/core//var/generic-functions nil)
(cl-defmacro entropy/shellpop2/core//macro/defgeneric (&rest args)
  (declare (doc-string 3) (indent defun))
  `(progn (push (quote ,(car args))
                entropy/shellpop2/core//var/generic-functions)
          (cl-defgeneric ,@args)))

(defun entropy/shellpop2/core//func/defgeneric/append/env/shell/type/name
    (shell/type/name defargs)
  (let (rtn body)
    (catch :break
      (while defargs
        (cond ((consp (car defargs))
               (push (append (pop defargs) (list '&shell-type-name shell/type/name))
                     rtn)
               (setq body defargs) (throw :break nil))
              (t (push (pop defargs) rtn)))))
    (append (nreverse rtn) body)))

(eval-and-compile
  (defmacro entropy/shellpop2/core//macro/defapi
      (name arglist &rest body)
    "Same as `cl-defmacro' but BODY's first `cl-labels's bindings are
exposed as API delaration with fake body.

\(fn NAME ARGLIST [DOCSTRING] [DECL] BODY...)"
    (declare (indent 2) (debug cl-flet))
    (let* ((pbody body)
           (_docstring
            (when (stringp (car pbody)) (setq pbody (cdr pbody))
                  (car pbody)))
           (_declare
            (when (and (consp (car pbody)) (eq 'declare (caar pbody)))
              (setq pbody (cdr pbody)) (car pbody)))
           get-cl-bindings-func funcs)
      (setq
       get-cl-bindings-func
       (lambda (x)
         (let (rtn)
           (catch :break
             (while x
               (cond
                ((and (consp (car x)) (eq 'cl-labels (caar x)))
                 (throw :break
                        (if (listp (cadar x)) (setq rtn (cadar x))
                          (error "entropy/shellpop2/core//macro/defapi: \
cl-labels bingings invalid"))))
                ((consp (car x))
                 (setq rtn (funcall get-cl-bindings-func (car x)))
                 (if rtn (throw :break rtn))))
               (setq x (cdr x))))
           rtn)))
      (setq
       funcs (funcall get-cl-bindings-func pbody)
       funcs
       (and
        funcs
        (cl-loop
         for i in funcs
         collect
         (append
          (list 'cl-defgeneric (car i) (nth 1 i))
          (delete
           nil
           (list (if (stringp (nth 2 i)) (nth 2 i))))))))
      `(progn ,@funcs (cl-defmacro ,name ,arglist ,@body)))))

;; * objects defination

;; ** objects defination
(defvar entropy/shellpop2/core//var/register/shell/objs nil)
;; DEBUG:
;; (setq   entropy/shellpop2/core//var/register/shell/objs nil)

;;;###autoload
(defun entropy/shellpop2/core/func/get/list/shell/type/name ()
  (let ((l entropy/shellpop2/core//var/register/shell/objs))
    (if (= (length l) 0)
        (error "No any shell/type/name has been registered.")
      (mapcar (lambda (x) (car x)) l))))

(entropy/shellpop2/core//macro/cl-defstruct
    (entropy/shellpop2/core//obj/shell/buffer
     (:constructor nil)
     (:constructor entropy/shellpop2/core//obj/shell/buffer/pred/make)
     (:predicate   entropy/shellpop2/core//obj/shell/buffer/pred/typep)
     (:copier      entropy/shellpop2/core//obj/shell/buffer/pred/copy)
     (:conc-name   entropy/shellpop2/core//obj/shell/buffer/slot/))
    "entropy/shellpop2/core/shell/buffer/obj"
  :with-obj-name shell/buffer/obj
  :with-judge-macro-name entropy/shellpop2/core//obj/shell/buffer/pred/with-judge
  :with-judge-return-func-name entropy/shellpop2/core//obj/shell/buffer/pred/with-judge-return
  shell/buffer
  shell/buffer/caption)
(entropy/shellpop2/core//macro/cl-defstruct
    (entropy/shellpop2/core//obj/shell/type
     (:constructor nil)
     (:constructor entropy/shellpop2/core//obj/shell/type/pred/make)
     (:predicate   entropy/shellpop2/core//obj/shell/type/pred/typep)
     (:copier      entropy/shellpop2/core//obj/shell/type/pred/copy)
     (:conc-name   entropy/shellpop2/core//obj/shell/type/slot/))
    "entropy/shellpop2/core/shell/type/obj"
  :with-obj-name shell/type/obj
  :with-judge-macro-name entropy/shellpop2/core//obj/shell/type/pred/with-judge
  :with-judge-return-func-name entropy/shellpop2/core//obj/shell/type/pred/with-judge-return
  shell/type/name
  (shell/buffer/obj::ring (make-ring 10))
  )

(defun entropy/shellpop2/core//func/get/shell/type/obj
    (&optional shell/type/name no-error)
  (setq shell/type/name
        (entropy/shellpop2/core/func/or/shell/type/name shell/type/name))
  (let* ((l entropy/shellpop2/core//var/register/shell/objs)
         (exist-p (alist-get shell/type/name l)))
    (or (and exist-p
             (entropy/shellpop2/core//obj/shell/type/pred/typep
              exist-p)
             exist-p)
        (if (assq shell/type/name l)
            (error "\
messy `entropy/shellpop2/core//var/register/shell/objs' of key %s"
                   shell/type/name))
        (unless no-error
          (error "No shell/type/obj registed yet for shell/type/name: %s"
                 shell/type/name)))))

(defun entropy/shellpop2/core//func/regist/shell/type/obj (shell/type/obj)
  (let ((shell/type/name
         (entropy/shellpop2/core//obj/shell/type/slot/shell/type/name
          shell/type/obj)))
    (if (entropy/shellpop2/core//func/get/shell/type/obj shell/type/name 'no-error)
        (error "shell/type/name `%S' has been registerred yet" shell/type/name)
      (push (cons shell/type/name shell/type/obj)
            entropy/shellpop2/core//var/register/shell/objs))))

;; *** shell/buffer init flag
(defvar-local entropy/shellpop2/var//shell/buffer/local/flag/inited-p nil)
(defun entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p nil
  (let ((it (bound-and-true-p entropy/shellpop2/var//shell/buffer/local/flag/inited-p)))
    (and (entropy/shellpop2/core//obj/shell/buffer/pred/typep (car it))
         (entropy/shellpop2/core//obj/shell/type/pred/typep (cdr it)))))
(defun entropy/shellpop2/core//func/set/current-buffer/shell/buffer/obj::flag
    (shell/buffer/obj shell/type/obj)
  (let (_)
    (setq shell/buffer/obj
          (or (entropy/shellpop2/core//obj/shell/buffer/pred/with-judge-return
               shell/buffer/obj)
              (entropy/shellpop2/core//func/get/shell/type/obj
               (entropy/shellpop2/core/macro/get/env/shell/type/name))))
    (setq entropy/shellpop2/var//shell/buffer/local/flag/inited-p
          (cons shell/buffer/obj
                (entropy/shellpop2/core//obj/shell/type/pred/with-judge-return
                 shell/type/obj 'with-error)))))
(defun entropy/shellpop2/core//func/get/current-buffer/shell/buffer/obj::flag
    (&optional with::shell/type/obj)
  (unless (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p)
    (error "current-buffer is not a inited shell/buffer: %S" (current-buffer)))
  (let ((shell/buffer/obj
         (entropy/shellpop2/core//obj/shell/buffer/pred/with-judge-return
          (car entropy/shellpop2/var//shell/buffer/local/flag/inited-p) t))
        (shell/type/obj
         (entropy/shellpop2/core//obj/shell/type/pred/with-judge-return
          (cdr entropy/shellpop2/var//shell/buffer/local/flag/inited-p) t)))
    (if with::shell/type/obj (cons shell/buffer/obj shell/type/obj)
      shell/buffer/obj)))

;; *** shell/buffer orphans deal
(defvar entropy/shellpop2/core//var/shell/buffer::orphans nil)
(eval-and-compile
  (defmacro entropy/shellpop2/core//macro/do-with/shell/buffer::orphans
      (&rest body)
    `(progn
       (let (ln)
         (dolist (el entropy/shellpop2/core//var/shell/buffer::orphans)
           (cond ((not (buffer-live-p el)) (kill-buffer el))
                 (t (push el ln))))
         (setq entropy/shellpop2/core//var/shell/buffer::orphans
               (nreverse ln)))
       ,@body)))

(defun entropy/shellpop2/core//func/get/shell/buffer::orphans-list nil
  (entropy/shellpop2/core//macro/do-with/shell/buffer::orphans
   (copy-sequence entropy/shellpop2/core//var/shell/buffer::orphans)))
(defun entropy/shellpop2/core//func/remove/shell/buffer::orphan (shell/buffer)
  (entropy/shellpop2/core/macro/with-judge/bufferp shell/buffer
    (entropy/shellpop2/core//macro/do-with/shell/buffer::orphans
     (setq entropy/shellpop2/core//var/shell/buffer::orphans
           (delete shell/buffer
                   entropy/shellpop2/core//var/shell/buffer::orphans)))))
(defun entropy/shellpop2/core//func/regist/shell/buffer::orphan (shell/buffer)
  (entropy/shellpop2/core/macro/with-judge/bufferp shell/buffer
    (entropy/shellpop2/core//macro/do-with/shell/buffer::orphans
     (unless (memq shell/buffer entropy/shellpop2/core//var/shell/buffer::orphans)
       (push shell/buffer entropy/shellpop2/core//var/shell/buffer::orphans)))))
(defun entropy/shellpop2/core//func/member/shell/buffer::orphan (shell/buffer)
  (entropy/shellpop2/core/macro/with-judge/bufferp shell/buffer
    (entropy/shellpop2/core//macro/do-with/shell/buffer::orphans
     (memq shell/buffer entropy/shellpop2/core//var/shell/buffer::orphans))))

;; ** objects operations
;; *** generics
(entropy/shellpop2/core//macro/defgeneric
  entropy/shellpop2/core/generic/shell/buffer/op/init (shell/buffer/obj)
  "Init SHELL/BUFFER/OBJ via
`entropy/shellpop2/core/macro/do-with/shell/buffer/obj' to let
its buffer be a `shell/buffer' belong to the current
`shell/type'.")
(cl-defmethod entropy/shellpop2/core/generic/shell/buffer/op/init
  :around (shell/buffer/obj)
  (let ((inhibit-quit t))
    (ignore shell/buffer/obj)
    (cl-call-next-method)))
(entropy/shellpop2/core//macro/defgeneric
  entropy/shellpop2/core/generic/shell/buffer/op/display (shell/buffer/obj)
  "Display the `shell/buffer' of a SHELL/BUFFER/OBJ.")
(entropy/shellpop2/core//macro/defgeneric
  entropy/shellpop2/core/generic/shell/buffer/op/hide (shell/buffer/obj)
  "Reverse what
`entropy/shellpop2/core/generic/shell/buffer/op/display' did.")
(entropy/shellpop2/core//macro/defgeneric
  entropy/shellpop2/core/generic/shell/buffer/op/remove (shell/buffer/obj)
  "TODO: undefined")
(cl-defmethod entropy/shellpop2/core/generic/shell/buffer/op/remove
  :around (shell/buffer/obj)
  (let ((inhibit-quit t))
    (ignore shell/buffer/obj)
    (cl-call-next-method)))
(entropy/shellpop2/core//macro/defgeneric
  entropy/shellpop2/core/generic/shell/buffer/op/cwd (shell/buffer/obj uri)
  "Do CD for a `shell/buffer' of the SHELL/BUFFER/OBJ with path URI.

URI may be remote file path.")
(cl-defmethod entropy/shellpop2/core/generic/shell/buffer/op/cwd
  :around (shell/buffer/obj uri)
  (let ((inhibit-quit t))
    (cl-call-next-method
     shell/buffer/obj
     (expand-file-name uri))))

;; *** do with shell/buffer/obj

;;;###autoload
(entropy/shellpop2/core//macro/defapi entropy/shellpop2/core/macro/do-with/shell/buffer/obj
    (shell/type/name shell/buffer/obj &rest body &key with-it-as &allow-other-keys)
  (declare (indent 2))
  (let ((it-sym (or with-it-as 'it)))
    (macroexp-let2* ignore
        ((stn-sym `(entropy/shellpop2/core/func/or/shell/type/name
                    ,shell/type/name))
         (sto-sym `(entropy/shellpop2/core//func/get/shell/type/obj ,stn-sym))
         (sbo-sym
          `(or ,shell/buffer/obj
               (entropy/shellpop2/core//obj/shell/buffer/pred/make
                :shell/buffer
                (with-current-buffer
                    (generate-new-buffer
                     (format "*eemacs-shell-pop-%s*" (symbol-name ,stn-sym)))
                  ;; TODO: shall we do some buffer-local specs
                  (current-buffer))
                :shell/buffer/caption "anonymous"
                ))))
      `(entropy/shellpop2/core/macro/do-with/shell/type/name ,stn-sym
         (let ((,it-sym ,sbo-sym))
           (ignore ,it-sym)
           (cl-labels
               ((entropy/shellpop2/core/api/obj/shell/buffer/pred/valid-p nil
                  "Return non-nil when current `shell/buffer/obj' is valid."
                  (let ((buffer
                         (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer ,it-sym)))
                    (buffer-live-p buffer)))
                (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer nil
                  "Get the buffer of current `shell/buffer/obj'"
                  (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer ,it-sym))
                (entropy/shellpop2/core/api/obj/shell/buffer/op/set-buffer
                  (x &optional inited)
                  "Set buffer X to current `shell/buffer/obj'.

The original buffer will be killed when it's not inited or marked
as orphan."
                  (unless (buffer-live-p x) (error "wrong type of argument: bufferp %S" x))
                  (let ((obuff
                         (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer ,it-sym)))
                    (when (and obuff (buffer-live-p obuff) (not (eq obuff x)))
                      (if (with-current-buffer obuff
                            (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p))
                          (entropy/shellpop2/core//func/regist/shell/buffer::orphan obuff)
                        (or (kill-buffer obuff)
                            ;; maybe kill failed for later review
                            (entropy/shellpop2/core//func/regist/shell/buffer::orphan obuff))))
                    (setf (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer ,it-sym)
                          x)
                    (when inited
                      (with-current-buffer x
                        (entropy/shellpop2/core//func/set/current-buffer/shell/buffer/obj::flag
                         ,sbo-sym ,sto-sym)))))
                (entropy/shellpop2/core/api/obj/shell/buffer/op/get-caption nil
                  "Get the caption string of current `shell/buffer/obj'."
                  (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer/caption ,it-sym))
                (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption (x)
                  "Set caption string X to current `shell/buffer/obj'."
                  (unless (stringp x)
                    (error "buffer caption should be a string but this: %S"
                           x))
                  (setf
                   (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer/caption
                    ,it-sym)
                   (unless (string-empty-p x) x))))
             ,(macroexp-progn body)))))))

;;;###autoload
(entropy/shellpop2/core//macro/defapi entropy/shellpop2/core/macro/do-with/current/shell/buffer
    (shell/buffer &rest body)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((shbuff-sym shell/buffer)
       (sbo-sym nil) (sto-sym nil) (flag-sym nil)
       (stn-sym nil))
    `(let ((,shbuff-sym
            (or (and (buffer-live-p ,shbuff-sym)
                     (with-current-buffer ,shbuff-sym
                       (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p))
                     ,shbuff-sym)
                (error "Buffer `%S' is not a inited shell/buffer or not a lived buffer"
                       ,shbuff-sym))))
       (setq ,flag-sym
             (with-current-buffer ,shbuff-sym
               (entropy/shellpop2/core//func/get/current-buffer/shell/buffer/obj::flag
                'with::shell/type/obj))
             ,sbo-sym (car ,flag-sym) ,sto-sym (cdr ,flag-sym)
             ,stn-sym
             (entropy/shellpop2/core//obj/shell/type/slot/shell/type/name
              ,sto-sym))
       (cl-labels
           ((entropy/shellpop2/core/api/shell/buffer/op/get/shell/type/name nil
              "Get `shell/type/name' of current `shell/buffer'."
              ,stn-sym)
            (entropy/shellpop2/core/api/shell/buffer/op/get-caption nil
              "Get `shell/buffer/caption' of current `shell/buffer'."
              (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
                  ,stn-sym ,sbo-sym
                (entropy/shellpop2/core/api/obj/shell/buffer/op/get-caption)))
            (entropy/shellpop2/core/api/shell/buffer/op/set-caption (x)
              "Set `shell/type/caption' of current `shell/buffer' to X."
              (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
                  ,stn-sym ,sbo-sym
                (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption x))))
         ,(macroexp-progn body)))))

;; *** do with shell/type/obj

(defun entropy/shellpop2/core//func/shell/type/obj/op/prune (shell/type/name)
  (when-let* ((it (entropy/shellpop2/core//func/get/shell/type/obj
                   shell/type/name 'no-error))
              (it-ring
               (entropy/shellpop2/core//obj/shell/type/slot/shell/buffer/obj::ring
                it))
              (def-ring-size 10)
              ((if (and (ring-p it-ring) (not (ring-empty-p it-ring))) t
                 (setf
                  (entropy/shellpop2/core//obj/shell/type/slot/shell/buffer/obj::ring
                   it)
                  (make-ring def-ring-size))
                 nil))
              (it-ring-len (ring-length it-ring))
              (it-els (ring-elements it-ring)))
    (let (it-new-els it-new-ring elb)
      (dolist (el it-els)
        (setq elb (entropy/shellpop2/core//obj/shell/buffer/slot/shell/buffer el))
        (cond ((entropy/shellpop2/core//func/member/shell/buffer::orphan elb) nil)
              ((buffer-live-p elb) (push el it-new-els))
              ((bufferp elb) (kill-buffer elb))))
      (if (not it-new-els)
          (setf (entropy/shellpop2/core//obj/shell/type/slot/shell/buffer/obj::ring
                 it)
                (make-ring def-ring-size))
        (setq it-new-ring (make-ring (max def-ring-size (length it-new-els))))
        (dolist (el it-new-els) (ring-insert it-new-ring el))
        (setf (entropy/shellpop2/core//obj/shell/type/slot/shell/buffer/obj::ring
               it)
              it-new-ring)))))

;;;###autoload
(entropy/shellpop2/core//macro/defapi entropy/shellpop2/core/macro/do-with/shell/type/obj
    (shell/type/name &rest body &key with-it-as &allow-other-keys)
  (declare (indent 1))
  (let ((it-sym (or 'it with-it-as)))
    (macroexp-let2* ignore
        ((stn-sym `(entropy/shellpop2/core/func/or/shell/type/name
                    ,shell/type/name))
         (sto-sym `(entropy/shellpop2/core//func/get/shell/type/obj
                    ,stn-sym))
         (sto-ring-sym nil))
      `(progn
         (entropy/shellpop2/core//func/shell/type/obj/op/prune ,stn-sym)
         (setq ,sto-ring-sym
               (entropy/shellpop2/core//obj/shell/type/slot/shell/buffer/obj::ring
                ,sto-sym))
         (entropy/shellpop2/core/macro/do-with/shell/type/name ,stn-sym
           (let ((,it-sym ,sto-sym))
             (ignore ,it-sym)
             (cl-labels
                 ((entropy/shellpop2/core/api/obj/shell/type/op/get-focus nil
                    "Get current `shell/type's focused `shell/buffer/obj' or nil if
not existed."
                    (when-let ((ob
                                (and (not (ring-empty-p ,sto-ring-sym))
                                     (ring-ref ,sto-ring-sym 0))))
                      (entropy/shellpop2/core/macro/do-with/shell/buffer/obj ,stn-sym ob
                        (and (entropy/shellpop2/core/api/obj/shell/buffer/pred/valid-p)
                             it))))
                  (entropy/shellpop2/core/api/obj/shell/type/op/get-last nil
                    "Get current `shell/type's last `shell/buffer/obj' or nil if
not existed."
                    (let* ((ob ,sto-ring-sym)
                           (obmidx (1- (ring-length ob))))
                      (ring-ref ob obmidx)))
                  (entropy/shellpop2/core/api/obj/shell/type/op/set-focus (x)
                    "Set current `shell/type's focused `shell/buffer/obj' to X."
                    (entropy/shellpop2/core//obj/shell/buffer/pred/with-judge x
                      :with-error t
                      (let* ((ob ,sto-ring-sym) (idx (ring-member ob x)))
                        (and idx (ring-remove ob idx))
                        (entropy/shellpop2/core//func/ring/insert ob x))))
                  (entropy/shellpop2/core/api/obj/shell/type/op/get-at-index (&optional idx)
                    "Get current `shell/type's one `shell/buffer/obj' at index IDX, or
nil if not existed."
                    (setq idx (or idx 0))
                    (let* ((ob ,sto-ring-sym)
                           (obmidx (1- (ring-length ob))))
                      (unless (> idx obmidx) (ring-ref ob idx))))
                  (entropy/shellpop2/core/api/obj/shell/type/op/get-list nil
                    "Get current `shell/type's all `shell/buffer/obj' in list form
by ordering from last accessed time."
                    (ring-elements ,sto-ring-sym)))
               ,(macroexp-progn body))))))))

;; * objects creation

;;;###autoload
(cl-defmacro entropy/shellpop2/core/macro/make/shell/type/obj
    (shell/type/name &rest conses)
  (declare (indent 1))
  (let (_)
    `(let ((,entropy/shellpop2/core//var/shell-type-flag-sym (quote ,shell/type/name)))
       ;; regist shell/type/obj
       (entropy/shellpop2/core//func/regist/shell/type/obj
        (entropy/shellpop2/core//obj/shell/type/pred/make
         :shell/type/name (quote ,shell/type/name)))
       ;; defmethods
       ,@(cl-loop
          for i in entropy/shellpop2/core//var/generic-functions
          if (alist-get i conses)
          collect
          `(progn
             (entropy/shellpop2/core//macro/defmethod ,i
               ,@(entropy/shellpop2/core//func/defgeneric/append/env/shell/type/name
                  shell/type/name (alist-get i conses))))
          else
          do (error "generic function `%s' not implemented for shell/type/name `%s'"
                    i shell/type/name)))))

;; * provide

(provide 'entropy-shellpop2-core)
