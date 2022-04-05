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

;; ** Common manipulations
;; *** Emacs internal api replacement

(defun entropy/emacs-error-without-debug (string &rest args)
  "Like `error' but always press `debug-on-error'"
  (let ((debug-on-error nil))
    (apply 'error string args)))

(defvar entropy/emacs--unwind-protect-body-global-body-run-status-judgers nil)
(defun entropy/emacs-unwind-protect-body-gen-nested-form (forms &optional level)
  "Nested BODY `unwind-protect' form generator for
`entropy/emacs-unwind-protect-body'."
  (let* ((flag (intern
                (format "___$___$_eemacs-unwind-protect-flags_%s"
                        (length entropy/emacs--unwind-protect-body-global-body-run-status-judgers))))
         (level (or level 0))
         body
         rtn)
    (set flag nil)
    (push flag entropy/emacs--unwind-protect-body-global-body-run-status-judgers)
    (setq body `(unless (bound-and-true-p ,flag)
                  (let (forms-rtn)
                    (setq forms-rtn (progn ,@forms))
                    (setq ,flag t)
                    (throw :exit forms-rtn))))
    (cond
     ((<= level 1)
      (setq rtn
            `(unwind-protect
                 ,body
               ,body)))
     ((> level 1)
      (while (>= level 1)
        (setq
         rtn
         (if rtn
             `(unwind-protect
                  ,body
                ,rtn)
           `(unwind-protect
                ,body
              ,body)))
        (cl-decf level))))
    `(catch :exit
       ,rtn)))

(defmacro entropy/emacs-unwind-protect-body (level &rest body)
  "Like `unwind-protect' but treat the BODYFORM and UNWINDFORMS as
one as a BODY. If BODY completes normally, its value is returned.

LEVEL is an protect times, larger than 0, an integer. If LEVEL
`eql' 1, then this macro do the same as `unwind-protect' of:

#+begin_src emacs-lisp
(unwind-protect
    (progn
      ,@body)
  ,@body)
#+end_src

Or is 2 for nested the 1st level as BODY as the unwind forms protects
the old-BODY, and so on for larger level.
"
  (declare (indent defun))
  (entropy/emacs-unwind-protect-body-gen-nested-form
   body level))

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
  `(let (abbrevp cl-func-use)
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
     (eval
      `(defun ,ad-name (orig-func &rest orig-args)
         (if (or (null ',satisfy-func)
                 (and (functionp ',satisfy-func)
                      (funcall ',satisfy-func)))
             (let ((print-level (or ,level 3))
                   (print-length (or ,length 20)))
               (apply orig-func orig-args))
           (apply orig-func orig-args))))
     ad-name)))

;; *** Symbol manupulation

(defun entropy/emacs-get-symbol-defcustom-value (symbol)
  "Get SYMBOL standard value setted by `defcustom'."
  (eval (car (get symbol 'standard-value))))

;; *** List manipulation
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
    (reverse rtn)))

;; **** Nth remap with `car' and `cdr'
(defun entropy/emacs-remap-for-nth (pos list-var)
  "Since `nth' can not be used into `setf' place holder, this
function let a `nth' form remaped for a equivalent one suitable
for `setf', the argument list is the same as `nth', see its
doc-string for details."
  (let (final-form)
    (cond ((and (> pos 0)
                (= pos 1))
           (setq final-form `(cadr ',list-var)))
          ((= pos 0)
           (setq final-form `(car ',list-var)))
          ((> pos 1)
           (let ((tmp-form `(cdr ',list-var)))
             (cl-loop for ct from 2 to pos
                      do (setq tmp-form
                               (list 'cdr tmp-form)))
             (setq final-form
                   (list 'car tmp-form)))))
    final-form))

(defun entropy/emacs-setf-for-nth (pos replace list-var)
  "Setf for position POS with the replacement REPLACE in list
LIST-VAR. POS was a positive integer and indexed start at 0,
meaning as same as what in `nth' arglists."
  (let ((remap-form (entropy/emacs-remap-for-nth pos list-var)))
    (eval `(setf ,remap-form ',replace))))

;; *** Plist manipulation

(defun entropy/emacs-strict-plistp (arg)
  "Strict plist true-p checker.

The strict plist structed as key-value pairs appended list, the
car of it was key, each key was a symbol must using the ':xx'
type. Each key's value was grouped that say the key's 2-step
after place must be key as well, thus the 'strict' meaning."
  (cond
   ((or (not (listp arg))
        (null arg))
    nil)
   ((and (listp arg)
         (= (/ (length arg) 2.0) (/ (length arg) 2)))
    (let (rtn)
      (cl-loop for item from 0 to (- (/ (length arg) 2) 1)
               do (unless (and (symbolp (nth (* item 2) arg))
                               ;; using `string-match-p' prevent match
                               ;; history messy
                               (string-match-p
                                "^:"
                                (symbol-name (nth (* item 2) arg))))
                    (setq rtn t)))
      (unless rtn
        t)))
   (t
    nil)))

(defun entropy/emacs-common-plistp (arg)
  "Common plist true-p checker

The common plist structed as key-value pairs appended list, the
car of it was key, each key was a symbol must using the ':xx'
type. Each key's value can be omitted thus the 'common' meaning."
  (cond
   ((not (listp arg))
    nil)
   ((and (listp arg)
         (not (null arg))
         (symbolp (car arg)))
    (let (indicator fake-p (pos 0))
      (dolist (el arg)
        (if (and (symbolp el)
                 ;; using `string-match-p' prevent match
                 ;; history messy
                 (string-match-p
                  "^:"
                  (symbol-name el)))
            (setq indicator
                  (append indicator '(1)))
          (setq indicator
                (append indicator '(0)))))
      (catch :exit
        (while (< pos (length indicator))
          (if (= (nth pos indicator) 1)
              (let ((-pos (+ 1 pos))
                    (seq-num 0))
                (while (and (< -pos (length indicator))
                            (= (nth -pos indicator) 0))
                  (setq seq-num (+ 1 seq-num)
                        -pos (+ 1 -pos)))
                (when (> seq-num 1)
                  (setq fake-p t)
                  (throw :exit nil))
                (if (> -pos (- (- (length indicator) 1) 1))
                    (throw :exit nil)
                  (setq pos -pos)))
            (setq fake-p t)
            (throw :exit nil))))
      (if fake-p
          nil
        t)))
   (t nil)))

(defun entropy/emacs-get-plist-form
    (form-plist key &optional type no-error)
  "Like  `plist-get' but for getting the rest form of a key slot.

FORM-PLIST must be an list and no other restriction announced be,
but the key must be an symbol and formed as ':key' which start
with an colon which is what emacs plist key name convention, thus
all, any colon prefixed symbol involved in is treated as an key,
so as, if the rest args of an key whose has an member of colon
prefixed symbol will not be getted, which must be an important
restriction.

Do as the same as `plist-get' when TYPE was 't' or 'car'.

Return a `progn' form when TYPE was nil or omitted or eq 'progn',
In this case the return form will be nil if the slot's rest form
are empty, or just presented as an single nil.

Return a list when TYPE was 'list'. In this case the return list
will be nil if the slot's rest form are empty.

If NO-ERROR was non-nil, press all the error asserts, in that
case return nil, otherwise when KEY can not be found in
FORM-PLIST when type is not an 'car' type described as above,
throw out an error."
  (let* ((match (member key form-plist))
         (rest (cdr match))
         (is-car (member type '(car t)))
         (is-progn (or (null type) (eq type 'progn)))
         (is-list (eq type 'list))
         (pt 0)
         (rtn nil))
    (catch :exit
      (when (null match)
        (when (or no-error is-car)
          (throw :exit nil))
        (error "Can not match key '%s' in form-plist!" key))
      (while (and (not (null (nth pt rest)))
                  (not (and
                        (symbolp (nth pt rest))
                        (string-match-p "^:" (symbol-name (nth pt rest))))))
        (push (nth pt rest) rtn)
        (cl-incf pt))
      (unless (null rtn)
        (setq rtn
              (cond
               (is-progn
                (if (and (= (length rtn) 1)
                         (null (car rtn)))
                    nil
                  `(progn
                     ,@(reverse rtn))))
               (is-car
                (car (reverse rtn)))
               (is-list
                `(,@(reverse rtn))))))
      rtn)))

(defun entropy/emacs-inject-plist-form (form-plist key &rest inject-forms)
  "Inject forms INJECT-FORMS into a plist in place of key slot
KEY, if key can not be matched then append the KEY and thus. This
function return a form-plist."
  (let (key-pt
        key-next-pt
        (cnt 0)
        rtn)
    ;; get key slot place
    (catch :exit
      (while (<= cnt (- (length form-plist) 1))
        (when (eq (nth cnt form-plist) key)
          (setq key-pt cnt)
          (throw :exit nil))
        (cl-incf cnt)))

    (when key-pt
      ;; get next key slot place
      (setq cnt (+ key-pt 1))
      (catch :exit
        (while (<= cnt (- (length form-plist) 1))
          (when (and (symbolp (nth cnt form-plist))
                     (string-match-p "^:" (symbol-name (nth cnt form-plist))))
            (setq key-next-pt cnt)
            (throw :exit nil))
          (cl-incf cnt))))

    (cond
     ((null key-next-pt)
      (setq rtn
            (if key-pt
                (append form-plist
                        inject-forms)
              (append form-plist
                      (list key)
                      inject-forms))))
     (t
      (let* ((head-list
              (cl-loop for pt from 0 to (- key-next-pt 1)
                       collect (nth pt form-plist)))
             (tail-list
              (member (nth key-next-pt form-plist) form-plist)))
        (setq rtn
              (append head-list
                      inject-forms
                      tail-list)))))
    rtn))

(defun entropy/emacs-get-plist-body (args)
  "Get BODY inside of 'pre-plist' ARGS, commonly is the last non
key-pair cdr.

This function is useful for cl-based def* args parsing like:

#+begin_src emacs-lisp
  (name &rest body
        &key
        key-1
        key-2
        ...
        &allow-other-keys)
#+end_src

To get the real-body in BODY.
"
  (let ((it args))
    (catch 'break
      (while t
        (if (keywordp (car it))
            (setq it (cddr it))
          (throw 'break it))))))

(defun entropy/emacs-generalized-plist-get
    (plist top-place prop &optional testfn)
  "Like `plist-get' but return a cons as car of value get from
PLIST using PROP and cdr is a =eemacs-generalized-plist-get-form=
which can be used for `setf' i.e. a PLACE-FORM which can be
evaluted to return the value as `plist-get' did.

Always return nil when the prop not valid in plist or its slot is
omitted i.e. the prop has no value stored in.

TOP-PLACE is a symbol or a =eemacs-generalized-plist-get-form=
where is the PLIST evaluated from.

Example:

#+begin_src emacs-lisp
  (setq plist-var '(:a 1 :b 2 :c :d 4 :e nil :f))

  (entropy/emacs-generalized-plist-get
   plist-var 'plist-var :b)
  ;; => (2 . (car (cdr (cdr (cdr plist-var)))))

  (entropy/emacs-generalized-plist-get
   plist-var 'plist-var :e)
  ;; => (nil . (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr plist-var))))))))))

  (entropy/emacs-generalized-plist-get
   plist-var 'plist-var :c)
  ;; => (:d . (car (cdr (cdr (cdr (cdr (cdr plist-var)))))))

  (entropy/emacs-generalized-plist-get
   plist-var 'plist-var :f)
  ;; => nil

  (entropy/emacs-generalized-plist-get
   plist-var 'plist-var :invalid-prop)
  ;; => nil
#+end_src

Optional argument TESTFN is set using it as the PROP query
predicate, default is `eq'.
"
  (when plist
    (let ((pos 0)
          (mcp nil)
          (testfn (or testfn 'eq)))
      (setq mcp
            (catch :exit
              (dolist (el plist)
                (when (funcall testfn el prop)
                  (throw :exit t))
                (cl-incf pos))
              nil))
      (if (and mcp (< (1+ pos) (length plist)))
          (let ((cnt 0)
                (formrtn nil))
            (while (<= cnt pos)
              (setq formrtn
                    (if formrtn
                        `(cdr ,formrtn)
                      `(cdr ,top-place)))
              (cl-incf cnt))
            (cons
             (plist-get plist prop)
             `(car ,formrtn)))
        nil))))

(defun entropy/emacs-generalized-plist-get-batch-mode
    (plist top-place props &optional testfn)
  "The batch-mode for `entropy/emacs-generalized-plist-get' where
PROPS is a list of PROP mapping to PLIST, thus:

#+begin_src emacs-lisp
  (setq plist-var '(:a 1 :b (:a 1 :b 2 :c 3) :c 3))

  (entropy/emacs-generalized-plist-get-batch-mode
   plist-var 'plist-var '(:b :b))
  ;; => (2 . (car (cdr (cdr (cdr (car (cdr (cdr (cdr plist-var)))))))))
#+end_src

Always return nil when any mapping level's form generated failed
or PROPS is `null'. Otherwise return as same as
`entropy/emacs-generalized-plist-get'.
"
  (let (macp
        (cur_top_place top-place)
        (cur_plist plist)
        cur_log
        cur_form
        cur_value
        cur_prop)
    (cond
     (props
      (setq macp
            (catch :exit
              (while props
                (setq cur_prop (pop props))
                (setq cur_log
                      (entropy/emacs-generalized-plist-get
                       cur_plist cur_top_place cur_prop testfn)
                      cur_value (car cur_log)
                      cur_form (cdr cur_log))
                (if cur_form
                    (progn
                      (setq cur_plist cur_value
                            cur_top_place cur_form))
                  (throw :exit nil)))
              t))
      (when macp
        (cons cur_value cur_form)))
     (t
      nil))))

(defun entropy/emacs-setf-plist (eemacs-generalized-plist-get-form value)
  "Apply `setf' to a =eemacs-generalized-plist-get-form=
EEMACS-GENERALIZED-PLIST-GET-FORM with VALUE.

See `entropy/emacs-generalized-plist-get' and
`entropy/emacs-generalized-plist-get-batch-mode' for details."
  (when eemacs-generalized-plist-get-form
    (eval
     `(setf ,(cdr eemacs-generalized-plist-get-form)
            ',value))))

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
        (when (string-match-p (rx (regexp (eval el))) str)
          (cl-incf rtn)))
      (if (eq rtn 0)
          nil
        t)))

;; *** File and directory manipulation

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

(defun entropy/emacs-filesytem-nodes-in-same-filesystem-p (&rest filesystem-nodes)
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
          (user-error "[entropy/emacs-filesytem-nodes-in-same-filesystem-p]: '%s' not existed!"
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
        (error "[entropy/emacs-filesytem-nodes-in-same-filesystem-p]: internal error"))
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
        (eval form)
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
"
  (let (rtn-full rtn-lite rtn-attr)
    (setq rtn-full (directory-files dir-root (not not-abs)))
    (dolist (el rtn-full)
      ;; filter the . and ..
      (if (not (string-match-p "\\(\\\\\\|/\\)?\\(\\.\\|\\.\\.\\)$" el))
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
the current =dir-spec='s subdir or just after the end of cuarrent node
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
=fallback-modification=, this can be did by value get using
`entropy/emacs-generalized-plist-get-batch-mode' and value set using
`entropy/emacs-setf-plist'.

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
    (top-dir &optional not-abs
             &key
             with-level
             with-filter)
  "List all sub-directories under TOP-DIR as a list ordered by
`string-lessp' use `entropy/emacs-list-dir-subdirs-recursively'.

Optional argument NOT-ABS and optional keys are all related to
`entropy/emacs-list-dir-subdirs-recursively' (see it for details).
"
  (let (rtn
        map-func)
    (setq map-func
          (lambda (x &optional end-call-p)
            (unless end-call-p
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
  (unless (entropy/emacs-filesytem-nodes-in-same-filesystem-p
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
                           (setq cur-succeed-p nil)
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

(defun entropy/emacs-existed-filesystem-nodes-equal-p (filesystem-node1 filesystem-node2)
  "Alternative to `file-equal-p' but using `file-attribute-inode-number'
to distinguish the return.

Return t while thus. Return nil otherwise.

Always return nil, when any of FILESYSTEM-NODE1 or FILESYSTEM-NODE2 is
not predicated by `entropy/emacs-filesystem-node-exists-p'."
  (let ((f1-p (entropy/emacs-filesystem-node-exists-p filesystem-node1 t))
        (f2-p (entropy/emacs-filesystem-node-exists-p filesystem-node2 t)))
    (when (and f1-p f2-p)
      (= (file-attribute-inode-number f1-p)
         (file-attribute-inode-number f2-p)))))

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

;; *** Process manipulation

(defun entropy/emacs-process-exit-with-fatal-p
    (process &optional sentinel-event-string)
  "Judge whether a PROCESS is ran out with abnormal status. Return
non-nil if thus.

Optional arguments SENTINEL-EVENT-STRING is the event-status
string for normally getted from the PROCESS's sentinel."
  (require 'rx)
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
      (not (= 0 (process-exit-status proc))))))

(defun entropy/emacs-chained-eemacs-make-proc-args (eemacs-make-proc-args-list)
  "Chained sets of `eemacs-make-proc-args-list' one by one
ordered of a list of thus of EEMACS-MAKE-PROC-ARGS-LIST."
  (let* ((tail-pt (- (length eemacs-make-proc-args-list) 1))
         (head-pt (- (length eemacs-make-proc-args-list) 2))
         rtn)
    (cond ((< head-pt 0)
           (setq rtn (car eemacs-make-proc-args-list)))
          (t
           (setq )
           (while (>= head-pt 0)
             (cond ((ignore-errors (= tail-pt (- (length eemacs-make-proc-args-list) 1)))
                    (setq rtn
                          (entropy/emacs-inject-plist-form
                           (nth head-pt eemacs-make-proc-args-list)
                           :after
                           `(entropy/emacs-make-process
                             ',(nth tail-pt eemacs-make-proc-args-list))))
                    (setq tail-pt nil))
                   (t
                    (setq rtn
                          (entropy/emacs-inject-plist-form
                           (nth head-pt eemacs-make-proc-args-list)
                           :after
                           `(entropy/emacs-make-process
                             ',rtn)))))
             (setq
              head-pt (1- head-pt)))))
    rtn))

(defun entropy/emacs-make-process
    (eemacs-make-proc-args)
  "Make a asynchronous process or a synchronous one using the
args in EEMACS-MAKE-PROC-ARGS.

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
   * Slots support: =:after=, =:cleanup=, =:sentinel=

2) =$sentinel/event=
   * description: the process returned event string
   * limitation: just used for async process
   * Slots support: =:after=, =:cleanup=, =:sentinel=

3) =$sentinel/destination=
   * description: its a process buffer or for the meaning for the
     of `call-process' =destination= arg when calling process
     synchronously.
   * limitation: both async and sync process calling type
   * Slots support: =:after=, =:cleanup=, =:sentinel=
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
         (eval (entropy/emacs-get-plist-form
                eemacs-make-proc-args :synchronously t t)))
        (default-directory
          (or (eval (entropy/emacs-get-plist-form
                     eemacs-make-proc-args :default-directory t t))
              default-directory))
        ;; make-proc args
        ($make_proc_name (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :name t t)))
        ($make_proc_buffer (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :buffer t t)))
        ($make_proc_command (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :command t t)))
        ($make_proc_coding (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :coding t t)))
        ($make_proc_noquery (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :noquery t t)))
        ($make_proc_stop (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :stop t t)))
        ($make_proc_connection-type (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :connection-type t t)))
        ($make_proc_filter (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :filter t t)))
        ($make_proc_sentinel (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :sentinel t t)))

        ;; call-process arg
        ($call_proc_destination (or (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :destination t t))
                                    (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :buffer t t))))
        ($call_proc_infile (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :infile t t)))
        ($call_proc_display (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :display t t)))
        ($call_proc_command (car (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :command t t))))
        ($call_proc_args (cdr (eval (entropy/emacs-get-plist-form eemacs-make-proc-args :command t t))))

        (__this_sync_sym (let ((make-sym-func
                                (lambda ()
                                  (intern (format "eemacs-make-process_%s_%s_%s_%s"
                                                  (random) (random) (random) (random)))))
                               sym)
                           (setq sym (funcall make-sym-func))
                           (when (boundp sym)
                             (while (boundp sym)
                               (setq sym (funcall make-sym-func))))
                           (set sym nil)
                           sym))
        __this_proc
        __this_proc_buffer)
    (when (eval prepare-form)
      (cond
       ((or (null synchronously)
            (and (eq synchronously t)
                 ;; NOTE & FIXME: sleep waiting for async in
                 ;; interaction session may freeze emacs why? and thus
                 ;; we just used this in noninteraction session.
                 (bound-and-true-p noninteractive)))
        (setq __this_proc
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
                          (cond ((string-match-p "finished\n" $sentinel/event)
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
                        (setq ,__this_sync_sym ran-out-p))
                      (when (and (not ',synchronously)
                                 ran-out-p)
                        ,clean-form)))))

              __this_proc_buffer (process-buffer __this_proc))

        (when (eq synchronously t)
          (while (null (symbol-value __this_sync_sym)) (sleep-for 0.1))
          (eval `(let (($sentinel/destination ',__this_proc_buffer))
                   (unwind-protect
                       (if (eq ,__this_sync_sym t)
                           ;; just ran after form when this process ran out successfully
                           ,after-form)
                     ,clean-form)))))
       (t
        (let (($sentinel/destination
               $call_proc_destination))
          (unwind-protect
              (if (=
                   (apply 'call-process
                          $call_proc_command
                          $call_proc_infile
                          $call_proc_destination
                          $call_proc_display
                          $call_proc_args)
                   0)
                  ;; just ran after form when this process ran out successfully
                  (eval after-form)
                (eval error-form))
            (eval clean-form))))))))

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
        (eval form-get)))
     ;; identity type
     ((eq type 'EEMACS-DT-IDENTITY)
      exp)
     (t
      (error
       "%s"
       (format "ERROR: EEMACS-TYPE-SPEC detected fatal of spec %s"
               eemacs-type-spec))))))

;; **** form item replace
(defun entropy/emacs-replace-form-symbol
    (form sub-elt replace &optional parse-append)
  "Replace symbol SUB-ELT occurrences in form FORM with
replacement REPLACE recursively.

Form was a sequence, which must validated by data predicate
`sequencep', otherwise throw out a error.

If optional argument PARSE-APPEND is non-nil, the replacement is
appended injecting into the form but always do not did this way
when the REPLACE is not a LIST."
  (unless (sequencep form)
    (error "Wrong type of argument: sequencep, %s" form))
  (let (form-patch
        (vector-form-p (vectorp form)))
    (mapc
     (lambda (el)
       (cond
        ((and (symbolp el) (eq el sub-elt))
         (setq form-patch
               (append form-patch
                       (if (or (null parse-append)
                               (not (listp replace)))
                           `(,replace)
                         replace))))
        ((and (sequencep el)
              (not (stringp el)))
         (setq form-patch
               (append form-patch
                       `(,(entropy/emacs-replace-form-symbol
                           el sub-elt replace parse-append)))))
        (t
         (setq form-patch
               (append form-patch `(,el))))))
     form)
    (if vector-form-p
        (vconcat form-patch)
      form-patch)))

;; **** Dolist macro progn sequence port
(defun entropy/emacs--progn-seq-dolist-core
    (looper body-list &optional not-do parse-append)
  "Put pices of forms with same structure based on a list of
forms BODY-LIST together into a progn form, as `dolist' but
without using `while' as looper procedure, its generate plenty
forms with the looper replacement by LOOPER, its useful to
generate a non-loop batch procedure.

LOOPER is a two elements list whose car was a symbol indicate the
lexical looper var and the cdr was a quoted list of the
replacement or a symbol whose value was a list, even for a form to
generate a list.

If optional argument NOT-DO is non-nil, this function return a
form represent the procedure, so that you can evaluate later or
used in other cases.

If optional argument PARSE-APPEND is non-nil, the replacement for
the looper will inject into the form using appended way, for
example, if form '(a b c)'s looper 'b' will be replaced by '(1
2)', when this arg was non-nil, the returned form will be '(a 1 2
c)'."
  (let ((sub-elt (car looper))
        (map-seq (eval (cadr looper)))
        forms)
    (dolist (replace map-seq)
      (push
       (entropy/emacs-replace-form-symbol
        `(progn
           ,@body-list)
        sub-elt replace parse-append)
       forms))
    (setq forms
          (append '(lambda nil)
                  (list
                   (append '(progn)
                           (reverse forms)))))
    (if not-do
        forms
      (funcall forms))))

(defmacro entropy/emacs-progn-seq-dolist (looper &rest body)
  "A eemacs specified `dolist' macro, like `dolist' but mainly
has difference without `while' did and used a top `progn' form
includes all loop elements inside it separately.

The arguments list is same as `dolist' unless there's no RESULT
optional argument provided.

See `entropy/emacs-progn-seq-dolist-not-do' for non-evaluated
way.

See `entropy/emacs-progn-seq-dolist-with-parse-append' for
replacement appended way."
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body))

(defmacro entropy/emacs-progn-seq-dolist-with-parse-append
    (looper &rest body)
  "Same as `entropy/emacs-progn-seq-dolist' but loop replacement
will be append into the structure. "
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body nil :parse-append))

(defmacro entropy/emacs-progn-seq-dolist-not-do (looper &rest body)
  "Same as `entropy/emacs-progn-seq-dolist' but do not evaluate
the result, return a expanded batch progn type loop form."
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body t))

(defmacro entropy/emacs-progn-seq-dolist-not-do-with-parse-append
    (looper &rest body)
  "Same as `entropy/emacs-progn-seq-dolist-with-parse-append' but
do not evaluate the result, return a expanded batch progn type
loop form."
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body t :parse-append))

;; **** `eval-after-load' batch port
(defmacro entropy/emacs-eval-after-load (feature &rest body)
  "Wrap BODY into FEATURE using `eval-after-load'.

Feature is can be a FILE arg of `eval-after-load' or a list of
that (note that this is a macro that if feature is a list, it
means no quote needed to construct it)."
  (declare (indent defun))
  `(let (forms
         (feature ',feature)
         (body ',body)
         (extract-item-func
          (lambda (file)
            (if (stringp file)
                file
              (list 'quote file)))))
     (cond ((not (listp feature))
            (setq forms
                  `(eval-after-load ,(funcall extract-item-func feature)
                     (lambda ()
                       ,@body))))
           ((and (listp feature)
                 (= 1 (length feature)))
            (setq forms
                  `(eval-after-load ,(funcall extract-item-func (car feature))
                     (lambda ()
                       ,@body))))
           ((and (listp feature)
                 (> (length feature) 1))
            (setq feature (reverse feature)
                  forms `(eval-after-load ,(funcall extract-item-func (car feature))
                           (lambda () ,@body)))
            (dolist (el (cdr feature))
              (setq forms
                    `(eval-after-load ,(funcall extract-item-func el)
                       ',forms)))))
     (eval forms)))


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
       (eval func-define)
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
     (eval
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

(defun entropy/emacs-get-face-attribute-alist (face &optional frame)
  "Map face FACE all attributes into a alist with element formed
as '(cons attr-key attr-value)' which can be used for
`set-face-attribute' to loop did as."
  (delete nil
          (mapcar
           (lambda (attr)
             (let ((value (face-attribute face attr frame t)))
               (if (eq value 'unspecified)
                   (cond
                    ((eq attr :foreground) (cons attr nil)))
                 (cons attr value))))
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

(defun entropy/emacs-theme-adapted-to-solaire (&optional theme)
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
(defun entropy/emacs-network-canbe-connected-?-p (hostname)
  "Check hosted system whether can be connected to web sever
HOSTNAME, a string to represent the server web address,
e.g. \"www.google.com\".

This function use wildly integrated system kit \"ping\" as the
subroutine, throw out a error prompt when it can no be found on
your system. Return non-nil for thus, or nil otherwise."
  (let ((ping-args-core (if sys/win32p '("-n" "1" "-w" "10")
                          '("-c" "1" "-W" "10"))))
    (unless (executable-find "ping")
      (error "Can not find 'ping' command in your system"))
    (= 0 (apply 'call-process `("ping" nil nil nil ,@ping-args-core
                                ,hostname)))))

(defun entropy/emacs-network-connected-p ()
  "Judge whether emacs can be connected to the internet. Return
non-nil for thus, or nil otherwise."
  (or (entropy/emacs-network-canbe-connected-?-p "www.baidu.com")
      (entropy/emacs-network-canbe-connected-?-p "www.google.com")))

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


;; *** Test emacs with pure env

(defun entropy/emacs-test-emacs-with-pure-setup-with-form
    (name form &optional use-current-package-user-dir emacs-invocation-name)
  "Invoke subprocess with process name string NAME to run a FORM
within a virginal emacs env (i.e. emacs -Q). Return the process
object.

If USE-CURRENT-PACKAGE-USER-DIR is non-nill then, initialize
packages with current emacs session's `package-user-dir' at the
subprocess emacs initialization time.

The `invocation-name' of the virginal emacs is as the same as
current emacs session if optional arg EMACS-INVOCATION-NAME is
not set, else using that as `invocation-name' for as."
  (let* (proc
         (buffer (generate-new-buffer
                  (format "*entropy/emacs-test-emacs-with-pure-setup-with-form/%s*"
                          name)))
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
         (form-encoded (with-current-buffer (entropy/emacs-generate-base64-encoded-sexp-buffer
                                             (if use-current-package-user-dir
                                                 `(let ((package-user-dir ,package-user-dir))
                                                    (progn
                                                      (package-initialize)
                                                      ,form))
                                             `(progn
                                                ,form)))
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
             (eval __this_eval_form)))
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
           :name name
           :buffer buffer
           :command `(
                      ;; ensure use the same emacs version as current session
                      ,(or emacs-invocation-name
                           (expand-file-name invocation-name invocation-directory))
                      "-Q" "-l" ,proc-eval-form-file)
           :sentinel nil))
    proc))

;; ** eemacs specifications
;; *** Individuals

(defmacro entropy/emacs-general-run-with-protect-and-gc-strict (&rest body)
  "Run BODY with `inhibit-quit' and restrict with basic
`gc-cons-threshold'."
  `(let ((gc-cons-threshold entropy/emacs-gc-threshold-basic)
         (gc-cons-percentage entropy/emacs-gc-percentage-basic)
         (inhibit-quit t))
     ,@body))

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

(defun entropy/emacs-icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
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
             rtn))))

(defun entropy/emacs-idle-cleanup-echo-area ()
  "Cleanup remaining echo area message when bussy done for some
tasks 'ing' refer prompts."
  (run-with-idle-timer 0.2 nil (lambda () (message nil))))

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

;; *** Lazy load specification
(defvar entropy/emacs--lazy-load-simple-feature-head nil)
(defvar entropy/emacs-lazy-load-simple-log-var nil)
(cl-defmacro entropy/emacs-lazy-load-simple
    (feature &rest body
             &key
             non-message
             always-lazy-load
             &allow-other-keys)
  "Execute BODY after/require FEAURE is loaded.  FEATURE is normally a
feature name, but it can also be a file name, in case that file
does not provide any feature, further more FEATURE can be a list for
thus and autoloads them follow the order of that.

Optional key valid for:

- NON-MESSAGE:

  when non-nil do not show lazy loading config prompts.

- ALWAYS-LAZY-LOAD:

  when non-nil always lazy loading config after the featuer loaded
  without any restriction (see below). so the basic functional is same
  as `with-after-load'.

NOTE: Eventually BODY just be autoload when
`entropy/emacs-custom-enable-lazy-load' is non-nil with two
exceptions while ALWAYS-LAZY-LOAD is nil:

1. `daemonp': Since there's no need to lazy load anything while a
   daemon initialization.
2. `entropy/emacs-fall-love-with-pdumper' is non-nil, in which case
   eemacs initialization for a pdumper procedure, no need to do
   thus as case 1.

This function should always be used preferred to maintain eemacs
internal context or API adding to thus, because any not be will
pollute eemacs internal lazy load optimization."
  (declare (indent 1) (debug t))
  (let ((body (entropy/emacs-get-plist-body body))
        (this-load-fname
         (and entropy/emacs-startup-with-Debug-p
              (ignore-errors
                (file-name-nondirectory load-file-name)))))
    (when entropy/emacs-startup-with-Debug-p
      (push (list :feature feature
                  :load-in this-load-fname
                  :body body)
            entropy/emacs-lazy-load-simple-log-var))
    ;; macro main
    `(let ((feature-this ',feature))
       (cond
        ((or entropy/emacs-custom-enable-lazy-load
             ,always-lazy-load)
         (when (not (null feature-this))
           (entropy/emacs-eval-after-load
            ,feature
            (entropy/emacs-message-simple-progress-message
             (unless (or
                      (not (null ',non-message))
                      (member ',feature
                              (last entropy/emacs--lazy-load-simple-feature-head 3)))
               (setq entropy/emacs--lazy-load-simple-feature-head
                     (append entropy/emacs--lazy-load-simple-feature-head
                             (list ',feature)))
               (if entropy/emacs-startup-with-Debug-p
                   (format
                    "[gened-by: %s] with lazy loading configs for feature '%s'"
                    ,this-load-fname
                    ',feature)
                 (format
                  "with lazy loading configs for feature '%s'"
                  ',feature)))
             (entropy/emacs-general-run-with-protect-and-gc-strict
              ,@body)))))
        ((null entropy/emacs-custom-enable-lazy-load)
         (when (not (null feature-this))
           (unless (member feature-this (last entropy/emacs--lazy-load-simple-feature-head 3))
             (entropy/emacs-message-do-message
              "force load configs for feature '%s'" feature-this)
             (setq entropy/emacs--lazy-load-simple-feature-head
                   (append entropy/emacs--lazy-load-simple-feature-head
                           '(,feature))))
           (cond ((listp feature-this)
                  (dolist (el feature-this)
                    (require el)))
                 ((symbolp feature-this)
                  (require feature-this)))
           (entropy/emacs-general-run-with-protect-and-gc-strict
            ,@body)))))))

(defmacro entropy/emacs-lazy-with-load-trail (name &rest body)
  "Wrapping BODY to a function named with suffix by NAME into
=entropy-emacs-startup-trail-hook=.

See `entropy/emacs-select-trail-hook' for details of what is
=entropy-emacs-startup-trail-hook=.

BODY can be forms or a expanded FORM-PLIST (see
`entropy/emacs-get-plist-form') in which case there's some keys on
functional aim to:

- ':doc-string' :: host the function defination will be created
  for function of BODY.

- ':body' :: slot host forms, this key was necessary if BODY is a
  expanded FORM-PLIST.

- ':start-end' :: inject function of BODY into
  `entropy/emacs-startup-end-hook'. Defaultly, BODY will be
  injected into =entropy-emacs-startup-trail-hook=, but with this
  key non-nil or a form which evaluated result is non-nil.

- ':pdumper-no-end' :: specefied trail hook injection while
  pdumper according to `entropy/emacs-select-trail-hook'."
  (let ((func (intern
               (concat "entropy/emacs-lazy-trail-to-"
                       (symbol-name name))))
        (msg-str (symbol-name name))
        (inject-to-start-end
         (entropy/emacs-get-plist-form body :start-end t))
        (doc-string
         (entropy/emacs-get-plist-form body :doc-string t))
        (pdumper-no-end
         (entropy/emacs-get-plist-form body :pdumper-no-end t))
        (body (let ((body-form (entropy/emacs-get-plist-form body :body nil t)))
                (if body-form
                    (cdr body-form)
                  body))))
    `(progn
       (eval
        '(defun ,func ()
           ,(or doc-string "")
           (entropy/emacs-message-do-message
            "%s '%s' %s"
            (blue "Start")
            (yellow ,msg-str)
            (blue "..."))
           (entropy/emacs-general-run-with-protect-and-gc-strict
            ,@body)
           (entropy/emacs-message-do-message
            "%s '%s' %s"
            (blue "Start")
            (yellow ,msg-str)
            (blue "done!"))))
       (cond
        (,inject-to-start-end
         (setq entropy/emacs-startup-end-hook
               (append entropy/emacs-startup-end-hook
                       '(,func))))
        (t
         (set (entropy/emacs-select-trail-hook ,pdumper-no-end)
              (append (symbol-value (entropy/emacs-select-trail-hook ,pdumper-no-end))
                      '(,func))))))))

(defvar entropy/emacs-lazy-initial-form-pdumper-no-end nil)
(defun entropy/emacs-lazy-initial-form
    (list-var
     initial-func-suffix-name initial-var-suffix-name
     abbrev-name adder-name prompt-type
     &rest form_args)
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
   `entropy/emacs-lazy-initial-form-pdumper-no-end' is non-nil we also
   set the optional arg PDUMPER-NO-END of
   `entropy/emacs-select-trail-hook')

2. Using LIST-VAR and consider each element of it is a hook (when
   the ADDER-FLAG is nil) or a function (when ADDER-TYPE is
   non-nil), and add the GENED-FUNCTION into the hook or the
   advice env respectively using a ADDER-FUNCION named with the
   GENED-FUNCTION name suffixing on ADDER-NAME (a non empty
   string) for insteadly add it into =entropy-emacs startup trail
   hook= to get the lazy load effection.

The GENED-FUNCTION has the '&rest' type argument
'$_|internal-args' which has different meaning for different value
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

At last, ABBREV-NAME is feature based name string so that other
macro or function can use this function to generate the
GENED-FUNCTION with their own name abbreviated."

  (let* ((func (intern (concat abbrev-name "_" initial-func-suffix-name)))
         (adder-func (intern (concat (symbol-name func) "_" adder-name)))
         (var (intern (concat abbrev-name "_+" initial-var-suffix-name)))
         (adder-type (car form_args))
         (adder-flag (cadr form_args))
         (func-body (nth 2 form_args)))
    (unless (member adder-type '(add-hook advice-add))
      (error "Wrong adder-type for lazy-initial form '%s'"
             abbrev-name))
    (when (and (eq adder-type 'add-hook)
               (not (null adder-flag)))
      (error "Non-nil flag for 'add-hook' adder-type for lazy-initial form '%s'"
             abbrev-name))
    `(progn
       (defvar ,var nil)
       (defun ,func (&rest $_|internal-args)
         (unless (or ,var
                     ;; we just run intialform after eemacs startup
                     ;; done to speedup eemacs startup.
                     (and
                      (not (bound-and-true-p entropy/emacs-startup-done))
                      ;; but in pdumper dump session we want it to run
                      (not entropy/emacs-fall-love-with-pdumper)
                      ;; but in daemon load session we want it run
                      (not (daemonp))
                      ;; but in non-lazy enable mode we want it run
                      (not (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load)))
                      ))
           (let ((head-time (time-to-seconds))
                 (entropy/emacs-message-non-popup
                  (if (eq ',prompt-type 'prompt-popup) nil t))
                 end-time
                 func-bocy-rtn)
             (redisplay t)
             (entropy/emacs-message-do-message
              "%s '%s' %s"
              (blue "Loading and enable feature")
              (yellow ,initial-func-suffix-name)
              (blue "..."))
             (entropy/emacs-general-run-with-protect-and-gc-strict
              (setq func-bocy-rtn
                    (prog1
                        (progn
                          ,@func-body)
                      ;; FIXME: dummy for press byte-compile unused
                      ;; lexical variable warning
                      (setq $_|internal-args $_|internal-args)
                      ;; set indicator preventing duplicted invoking
                      (setq ,var t)
                      ;; fake the function after evaluated it.
                      (defun ,func (&rest _)
                        "this function has been faked since it just need to run once."
                        nil)
                      ;; finally we remove the initial injection
                      (cond ((eq ',adder-type 'advice-add)
                             (dolist (adfor-it ',list-var)
                               (advice-remove adfor-it ',func)))
                            ((eq ',adder-type 'add-hook)
                             (dolist (adhfor-it ',list-var)
                               (remove-hook adhfor-it ',func))))))
              )
             (setq end-time (time-to-seconds))
             (entropy/emacs-message-do-message
              "%s '%s' %s '%s' %s"
              (green "Load done for")
              (yellow ,initial-func-suffix-name)
              (green "within")
              (cyan (format "%f" (- end-time head-time)))
              (green "seconds. (Maybe running rest tasks ...)"))
             (redisplay t)
             (entropy/emacs-idle-cleanup-echo-area)
             func-bocy-rtn)))
       (defun ,adder-func (&rest _)
         (let ((inhibit-quit t))
           (dolist (item ',list-var)
             (if (not (null ,adder-flag))
                 (,adder-type item ,adder-flag ',func)
               (,adder-type item ',func)))
           ;; fake it after evaluated it
           (defun ,adder-func (&rest _)
             "this function has been faked since it just need to run once."
             nil)))
       (let ((hook (entropy/emacs-select-trail-hook
                    ',entropy/emacs-lazy-initial-form-pdumper-no-end)))
         (cond
          ((not entropy/emacs-custom-enable-lazy-load)
           (set hook (append (symbol-value hook) '(,func))))
          (t
           (,adder-func)))))))

(cl-defmacro entropy/emacs-lazy-initial-for-hook
    (hooks initial-func-suffix-name initial-var-suffix-name
           prompt-type &rest body
           &key pdumper-no-end
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
    (eval
     `(let ((entropy/emacs-lazy-initial-form-pdumper-no-end
             ,pdumper-no-end))
        (entropy/emacs-lazy-initial-form
         ',hooks ',initial-func-suffix-name ',initial-var-suffix-name
         "entropy/emacs--hook-first-enable-for" "hook-adder" ',prompt-type
         'add-hook nil
         ',body-wrap)))))

(cl-defmacro entropy/emacs-lazy-initial-advice-before
    (advice-fors initial-func-suffix-name initial-var-suffix-name
                 prompt-type &rest body
                 &key pdumper-no-end
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
    (eval
     `(let ((entropy/emacs-lazy-initial-form-pdumper-no-end
             ,pdumper-no-end))
        (entropy/emacs-lazy-initial-form
         ',advice-fors ',initial-func-suffix-name ',initial-var-suffix-name
         "entropy/emacs--beforeADV-fisrt-enable-for"
         "before-advice-adder" ',prompt-type
         'advice-add
         :before
         ',body-wrap)))))

(cl-defmacro entropy/emacs-lazy-initial-advice-after
    (advice-fors initial-func-suffix-name initial-var-suffix-name
                 prompt-type &rest body
                 &key pdumper-no-end
                 &allow-other-keys)
  "Like `entropy/emacs-lazy-initial-advice-before' but for :after place."
  (let ((body-wrap (entropy/emacs-get-plist-body body)))
    (eval
     `(let ((entropy/emacs-lazy-initial-form-pdumper-no-end
             ,pdumper-no-end))
        (entropy/emacs-lazy-initial-form
         ',advice-fors ',initial-func-suffix-name ',initial-var-suffix-name
         "entropy/emacs--AfterADV-fisrt-enable-for"
         "after-advice-adder" ',prompt-type
         'advice-add
         :after
         ',body-wrap)))))


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

;; *** `select-window' patch

(defun entropy/emacs--select-window-around-advice
    (orig-func &rest orig-args)
  (let ((cur-window (selected-window))
        (window (car orig-args))
        (rtn (apply orig-func orig-args)))
    (set-window-parameter cur-window 'eemacs-current-window nil)
    (when (window-live-p window)
      (set-window-parameter window 'eemacs-current-window t))
    rtn))

(advice-add 'select-window :around #'entropy/emacs--select-window-around-advice)

(defun entropy/emacs-current-window-is-selected-common-around-advice
    (orig-func &rest orig-args)
  "The common advice for function who return t or nil just be for
the sake of getting the 'current-window' status.

*DESIGNATION*:

In some situations, the `selected-window' is not what window
current use i.e. the cursor focus on in where if there's a side
window injected into current root window of current frame when
reuse thus buffer by poping out (e.g. show mpc-song buffer
replaced insome splitted window in current root window), we don't
know if this is a bug, or just a conflicted issue which may be
following on.

EEMACS_MAINTENANCE: this may be fixed by more underlying researches
"
  (let (_)
    (or (apply orig-func orig-args)
        (ignore-errors
          ;; weh use the mode-line local buffer window as review
          ;; object since the return `selected-window' has no
          ;; reference value in this case.
          (window-parameter (get-buffer-window (current-buffer))
                            'eemacs-current-window)))))

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
               (setq rtn (eval this-case)))
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
(defun entropy/emacs-theme-load-face-specifix (&optional x)
  "Sets of specification for eemacs native themes.

This function should be invoked after a theme loaded done, and it
will automatically recongnized current theme name and do the
corresponding stuffs."
  (unless x
    (setq x (symbol-name entropy/emacs-theme-sticker)))
  (cond
   ((string-match-p "spacemacs-dark" x)
    (with-eval-after-load 'ivy
      (entropy/emacs-set-face-attribute
       'ivy-current-match nil
       :background "purple4" :bold t)))
   ((string-match-p "spacemacs-light" x)
    (with-eval-after-load 'ivy
      (entropy/emacs-set-face-attribute
       'ivy-current-match nil
       :background "salmon" :bold t)))
   ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" x)
    (with-eval-after-load 'ivy
      (if (equal 'dark (frame-parameter nil 'background-mode))
          (entropy/emacs-set-face-attribute
           'ivy-current-match nil
           :background "#65a7e2" :foreground "black")
        (entropy/emacs-set-face-attribute
         'ivy-current-match nil
         :background "#1a4b77" :foreground "white"))))
   ((string= "doom-solarized-light" x)
    (when (not (featurep 'hl-line))
      (require 'hl-line))
    (entropy/emacs-set-face-attribute
     'hl-line nil
     :background
     "LightGoldenrod2")
    (entropy/emacs-set-face-attribute
     'solaire-hl-line-face nil
     :background "#ffffe4e4b5b5"))
   ((string= "doom-Iosvkem" x)
    (with-eval-after-load 'ivy
      (entropy/emacs-set-face-attribute
       'ivy-current-match nil
       :background "grey8"
       :distant-foreground "grey7")))
   ((string= "doom-dark+" x)
    (with-eval-after-load 'outline
      (entropy/emacs-set-face-attribute
       'outline-3 nil
       :foreground "LawnGreen")))
   ((string= "doom-1337" x)
    (with-eval-after-load 'outline
      (entropy/emacs-set-face-attribute
       'ivy-current-match nil
       :background "#2257A0"
       :distant-foreground "#1B2229"))
    (entropy/emacs-set-face-attribute
     'highlight nil
     :foreground "grey7")
    (entropy/emacs-set-face-attribute
     'region nil
     :background "grey24"))
   ((or (string= "sanityinc-tomorrow-bright" x)
        (string= "sanityinc-tomorrow-night" x)
        (string= "sanityinc-tomorrow-eighties" x))
    (entropy/emacs-set-face-attribute
     'tooltip nil
     :background "white"
     :foreground "grey21"))
   ((string= "sanityinc-tomorrow-blue" x)
    (entropy/emacs-set-face-attribute
     'tooltip nil
     :background "white"
     :foreground "blue4"))
   ((string= "ujelly" x)
    ;; term
    (entropy/emacs-set-face-attribute
     'term-color-blue nil
     :foreground "#61AFEF")

    ;; border margin and fringe
    (entropy/emacs-set-face-attribute
     'tooltip nil
     :background "grey6"
     :foreground "white")
    (entropy/emacs-set-face-attribute
     'border nil
     :background "grey6"
     :foreground "#505000000000")
    (entropy/emacs-set-face-attribute
     'internal-border nil
     :background "grey6"
     :foreground "#505000000000")
    (entropy/emacs-set-face-attribute
     'vertical-border nil
     :background "grey6"
     :foreground "#505000000000")
    (entropy/emacs-set-face-attribute
     'window-divider nil
     :background "grey6"
     :foreground "#505000000000")
    (entropy/emacs-set-face-attribute
     'window-divider-first-pixel nil
     :background "grey6"
     :foreground "#505000000000")
    (entropy/emacs-set-face-attribute
     'window-divider-last-pixel nil
     :background "grey6"
     :foreground "#505000000000")

    ;; symbol-overlay
    (entropy/emacs-set-face-attribute
     'symbol-overlay-default-face nil
     :background "DarkOrange"
     :foreground "black")
    )
   (t
    (entropy/emacs-set-fixed-pitch-serif-face-to-monospace))))

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
`entropy/emacs-theme-adapted-to-solaire' was judged."
  (when (entropy/emacs-theme-adapted-to-solaire)
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
(defun entropy/emacs-with-daemon-make-frame-done
    (name et-form ec-form &optional common-form)
  "Do sth after emacs daemon make a new frame.

- 'ET-FORM' is the form for cli emacs session
- 'EC-FORM' is the form for gui emacs-session

Optional form COMMON-FORM run directly after ET-FORM and EC-FORM
without any condition judgements.

Return the hooker symbol."
  (let* ((--name--
          (intern
           (format "%s-for-emacs-daemon"
                   (symbol-name name)))))
    (eval
     `(entropy/emacs-add-hook-lambda-nil
       ,--name--
       entropy/emacs-daemon-server-after-make-frame-hook
       'append
       (if (display-graphic-p)
           ,ec-form
         ,et-form)
       ,common-form))))

;; *** Proxy specification
;; **** process env with eemacs union internet proxy
(defvar __eemacs_cache_noproxy_list nil)
(defvar __eemacs_cache_noproxy_string nil)
(defun entropy/emacs-gen-eemacs-union-proxy-noproxy-envs (noproxy-list &optional list-return)
  "Generate comma separated no proxy patterns string from
NOPROXY-LIST which usually obtained from `entropy/emacs-union-proxy-noproxy-list'.

Return a list of thus when LIST-RETURN is non-nil."
  (catch :exit
    (cond (list-return
           (and __eemacs_cache_noproxy_list
                (throw :exit __eemacs_cache_noproxy_list)))
          (t
           (and __eemacs_cache_noproxy_string
                (throw :exit __eemacs_cache_noproxy_string))))
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
          (setq __eemacs_cache_noproxy_list list-rtn)
        (setq __eemacs_cache_noproxy_string noproxy-string)))))

(defvar __eemacs_cache_proxy_env nil)
(defun entropy/emacs-gen-eemacs-union-http-internet-proxy-envs ()
  "Generate list of http proxy env var/value paires sourced from
`entropy/emacs-union-http-proxy-plist'."
  (or __eemacs_cache_proxy_env
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
        (setq __eemacs_cache_proxy_env proxy-env)
        proxy-env)))

(defvar __eemacs_cache_url_proxy_services nil)
(defun entropy/emacs-gen-eemacs-union-http-internet-proxy-url-proxy-services ()
  "Generate a list formed to used for set `url-proxy-services'
sourced from `entropy/emacs-union-http-proxy-plist'."
  (or __eemacs_cache_url_proxy_services
      (let* ((proxy-plist entropy/emacs-union-http-proxy-plist)
             (target (format "%s:%s"
                             (plist-get proxy-plist :host)
                             (number-to-string (plist-get proxy-plist :port)))))
        (setq __eemacs_cache_url_proxy_services
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
      (eval
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

;; * provide
(provide 'entropy-emacs-defun)
