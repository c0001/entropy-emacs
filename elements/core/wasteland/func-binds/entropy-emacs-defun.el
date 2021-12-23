;;; entropy-emacs-defun.el --- entropy emacs pre-defined libraries
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

(defmacro entropy/emacs-with-temp-buffer (&rest body)
  "Like `with-temp-buffer' but press all hook related
`kill-buffer' after body done so that we can say it real
temporally do and have better performance than origin."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((kill-buffer-hook nil)
           (kill-buffer-query-functions nil)
           (,temp-buffer (generate-new-buffer " *temp*")))
       ;; `kill-buffer' can change current-buffer in some odd cases.
       (with-current-buffer ,temp-buffer
         (unwind-protect
             (progn ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

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
         (if (or (null ,satisfy-func)
                 (and (functionp ,satisfy-func)
                      (funcall ,satisfy-func)))
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
    (eval `(setf ,remap-form replace))))

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

The strict plist structed as key-value pairs appended list, the
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
          (when (= (nth pos indicator) 1)
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
                (setq pos -pos))))))
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
                        (string-match "^:" (symbol-name (nth pt rest))))))
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
(defun entropy/emacs-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\\\\\|/\\)\\(\\.\\|\\.\\.\\)$" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))


(defun entropy/emacs-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT, ordered by alphabetic."
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              (reverse rtn)
            nil))
      nil)))

(defun entropy/emacs-list-subfiles (dir-root)
  "Return a list of file(not directory) under directory DIR-ROOT."
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (when (equal "F" (car el))
              (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun entropy/emacs-list-dir-recursively (top-dir)
  "List directory TOP-DIR's sub-dirctorys recursively, return a
=dir-spec=, whose car was a path for one dirctory and the cdr was
a list of =dir-spec= or nil if no sub-dir under it. The structure
of return is ordered by alphabetic."
  (let ((subdirs (entropy/emacs-list-subdir top-dir))
        rtn)
    (catch :exit
      (add-to-list 'rtn top-dir)
      (unless subdirs
        (throw :exit nil))
      (dolist (sub-dir subdirs)
        (add-to-list
         'rtn
         (entropy/emacs-list-dir-recursively sub-dir))))
    (reverse rtn)))

(defun entropy/emacs-list-dir-recursive-for-list (top-dir)
  "list sub-directorys under directory TOP-DIR recursively, and
return the list. The structure of return is ordered by
alphabetic."
  (let ((dir-struct (entropy/emacs-list-dir-recursively top-dir))
        ext-func)
    (setq
     ext-func
     (lambda (node)
       (let (rtn)
         (catch :exit
           (setq rtn (list (car node)))
           (unless (cdr node)
             (throw :exit nil))
           (dolist (sub-node (cdr node))
             (setq
              rtn
              (append rtn (funcall ext-func sub-node)))))
         rtn)))
    (funcall ext-func dir-struct)))

(defun entropy/emacs-list-files-recursive-for-list (top-dir)
  "list files under directory TOP-DIR recursively, and return the
list. The structure of return is ordered by alphabetic."
  (let ((dir-list (entropy/emacs-list-dir-recursive-for-list top-dir))
        rtn)
    (dolist (dir dir-list)
      (setq rtn (append rtn (entropy/emacs-list-subfiles dir))))
    rtn))

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
  (let (rtn (fname (replace-regexp-in-string "\\(\\\\\\|/\\)$" "" file-name)))
    (cl-case type
      ('non-trail-slash
       (setq rtn fname))
      ('file-name
       (setq rtn
             (replace-regexp-in-string
              "^.*\\(\\\\\\|/\\)\\([^ /\\\\]+\\)$"
              "\\2"
              fname)))
      ('parent-dir
       (setq rtn (file-name-directory fname))))
    rtn))

(defun entropy/emacs-buffer-exists-p (buffername)
  "Judge whether buffer BUFFERNAME existed!"
  (let* ((bfl (mapcar 'buffer-name (buffer-list))))
    (if (member
         t
         (mapcar #'(lambda (bname)
                     (if (string= buffername bname) t nil))
                 bfl))
        t
      nil)))

(defun entropy/emacs-file-equal-p (file1 file2)
  "Alternative to `file-equal-p' but judge whether files existed
status before judging to prevent the unexpection internal error
hinttedd by `file-equal-p''s docstring, thus on, always return
nil if two file are not equalization or either FILE1 or file2 is
not existed."
  (let ((file1-p (file-exists-p file1))
        (file2-p (file-exists-p file2)))
    (when (and file1-p file2-p)
      (file-equal-p file1 file2))))

(defun entropy/emacs-write-file
    (filename &optional confirm)
  "Like `write-file' but create its host place firstly when apply
FILENAME to `file-name-directory' is non-nil"
  (let ((f-host (file-name-directory filename)))
    (when (and f-host
               (not (file-exists-p f-host)))
      (make-directory f-host t))
    (write-file filename confirm)))

;; *** Process manipulation
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
synchronously, the place hold a atom or a form.

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
             (lambda () t)))
        (after-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :after nil t)
             (lambda () t)))
        (error-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :error nil t)
             (lambda () t)))
        (clean-form
         (or (entropy/emacs-get-plist-form
              eemacs-make-proc-args :cleanup nil t)
             (lambda () t)))
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

        )
    (when (eval prepare-form)
      (cond
       ((null synchronously)
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
            (let ((orig-sentinel
                   ,$make_proc_sentinel)
                  ($sentinel/destination (process-buffer $sentinel/proc)))
              (unwind-protect
                  (progn
                    (when (functionp orig-sentinel)
                      (apply orig-sentinel
                             $sentinel/proc $sentinel/event))
                    (cond ((string-match-p "finished\n" $sentinel/event)
                           ,after-form)
                          ((string-match-p "\\(exit\\|failed\\|exited\\|broken\\)" $sentinel/event)
                           ,error-form)))
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
        (rtn nil))
    (cond
     ;; function type
     ((eq type 'EEMACS-DT-FUNC)
      (cond
       ((or (funcall symbol-mean-p exp)
            (functionp exp))
        (setq rtn
              (if (functionp exp)
                  (funcall exp)
                (funcall
                 (funcall symbol-mean-p exp t)))))
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
synchronous or async method using emacs native `url-retrieve' or
curl subprocess when USE-CURL non-nil, then verify the donwload
file via the VERIFY function with a single argument the
DESTINATION file-name whic return t or nil for valid or invalid
verification status.

NOTE:
   The verification process will be pressed under `conditio-case'
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
                    (format "eemacs-download-tmpfile_[%s]"
                            (format-time-string "%Y%m%d%H%M%S"))
                    temporary-file-directory))
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
                   (if (and (functionp ,verify)
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
                                   (funcall ,verify ,destination)
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
                (message (car (cdr error)))
                (setq ,cbk-symbol 'failed)
                (put ',cbk-symbol 'error-type
                     'move)
                (put ',cbk-symbol 'temp-file ,tmp-file)
                (when (file-exists-p ,tmp-file)
                  (delete-file ,tmp-file)))))))
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
           (success-func `(lambda ()
                            (message ,success-message)
                            (setq ,cbk-symbol 'success)
                            (funcall ,move-to-des-func)))
           (fatal-message (format "Download file form '%s' failed!" url))
           (fatal-func `(lambda ()
                          (setq ,cbk-symbol 'failed)
                          (put ',cbk-symbol 'error-type 'download)
                          (when',use-curl
                           (put ',cbk-symbol 'curl-args ',curl-args))
                          (message ,fatal-message)
                          (when (file-exists-p ,tmp-file)
                            (delete-file ,tmp-file))))
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
               `(lambda (proc status)
                  (if (string= status "finished\n")
                      (funcall ,success-func)
                    (when (string-match-p "\\(exit\\|failed\\|exited\\|broken\\)" status)
                      (funcall ,fatal-func))))))
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
            (if (eq (apply 'call-process
                           "curl" nil nil nil
                           curl-args)
                    0)
                (funcall success-func)
              (funcall fatal-func))
          (condition-case error
              (progn
                (url-copy-file url tmp-file)
                (funcall success-func))
            (error
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

4) 'zip' type: in which case, INPUT was a zipper compressed file
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

;; *** Window manipulation
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

;; --------- make funciton inhibit internal ---------
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
                 __func_rtn)
             (redisplay t)
             (entropy/emacs-message-do-message
              "%s '%s' %s"
              (blue "Loading and enable feature")
              (yellow ,initial-func-suffix-name)
              (blue "..."))
             (entropy/emacs-general-run-with-protect-and-gc-strict
              (setq __func_rtn
                    (prog1
                        (progn
                          ,@func-body)
                      (setq ,var t)
                      ;; fake the function after evaluated it.
                      (defun ,func (&rest $_|internal-args)
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
             __func_rtn)))
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
        (entropy/emacs--set-user-package-dir-common "28.0.50"))
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

(defun entropy/emacs-lang-set-utf-8 (&rest args)
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

(defun entropy/emacs-lang-set-local (&rest args)
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


(defun entropy/emacs-lang-use-utf-8-ces-around-advice (old-func &rest _)
  "Common around advice for wrapper function into utf-8
environment."
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (apply old-func _)))

(defun entropy/emacs-lang-use-locale-ces-around-advice (old-func &rest _)
  "Common around advice for wrapper funcion into locale language
environment, determined by `entropy/emacs-locale-coding-system'."
  (let ((coding-system-for-read entropy/emacs-locale-coding-system)
        (coding-system-for-write entropy/emacs-locale-coding-system))
    (apply old-func _)))

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
`entropy/emacs-solaire-global-mode' was non-nil."
  (when (and (entropy/emacs-theme-adapted-to-solaire)
             (bound-and-true-p entropy/emacs-solaire-global-mode))
    (require 'hl-line)
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
     (t nil))))

;; *** Case fold search specification
(defun entropy/emacs-case-fold-focely-around-advice (_old_func &rest _args)
  "Wrapper function to disable `case-fold-search' functional ability."
  (let ((_case_type case-fold-search)
        rtn)
    (unwind-protect
        (progn (setq case-fold-search nil)
               (setq rtn (apply _old_func _args))
               (setq case-fold-search _case_type)
               rtn)
      (setq case-fold-search _case_type))))

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

;; init `xclip-method' for gurantee its setting adapted to rest of
;; =entropy-emacs= context
(setq xclip-method
      (or
       (and (bound-and-true-p xclip-use-pbcopy&paste) 'pbpaste)
       (and sys/cygwinp (executable-find "getclip") 'getclip)
       (and (executable-find "xclip") 'xclip)
       (and (executable-find "xsel") 'xsel)
       (and (executable-find "wl-copy") 'wl-copy) ;github.com/bugaevc/wl-clipboard
       (and (executable-find "termux-clipboard-get") 'termux-clipboard-get)

       ;; NOTE: we must disable the native gui support method to
       ;; prevent it make a invisible frame to build connection with
       ;; current tui session.
       ;;
       ;;(and (fboundp 'x-create-frame) (getenv "DISPLAY") 'emacs)
       )
      xclip-program (symbol-name xclip-method))

(defun entropy/emacs-xterm-external-satisfied-p ()
  "Judge whether emacs to use external kits to assistant the
xterm-session yank/paste operation."
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
          ;; gnu/linux platform
          (and sys/linuxp
               (or
                (and (executable-find "xclip") 'xclip)
                (and (executable-find "xsel") 'xsel)
                (and (executable-find "wl-copy") 'wl-copy))))))
    (when (and (not (display-graphic-p))
               (fboundp 'xterm-paste)
               (when (ignore-errors (executable-find xclip-program))
                 (if (bound-and-true-p xclip-mode)
                     t
                   (progn (require 'xclip)
                          (xclip-mode 1)))))
      judger)))

;; Inspired by
;; https://emacs-china.org/t/wsl-emacs-windows/17375/3?u=angelaneia
(defun entropy/emacs-windows-env/copy-to-clipboard-core
    (str)
  "Push string STR into WINDOWS system clipboard.

NOTE: no warranty use in other system."
  (entropy/emacs-with-temp-buffer
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

(defun __adv/around/kill-new/windows-subsystem-env
    (orig-func &rest orig-args)
  "Kill things also put in windows system clipboard when
`sys/wsl-env-p'."
  (when sys/wsl-env-p
    (let ((str (car orig-args)))
      (entropy/emacs-windows-env/copy-to-clipboard-core
       str)))
  (apply orig-func orig-args))
(advice-add 'kill-new
            :around
            #'__adv/around/kill-new/windows-subsystem-env)

(defun entropy/emacs-xterm-paste-core (event)
  "The eemacs kill-ring update function for monitoring
`xterm-paste' event to automatically traceback to `kill-ring'
when the last event contet doesn't change, this useful to prevent
yanking an obsolete entry from `kill-ring' when the emacs
internal cut operation has updated the kill-ring but
`xterm-paste' will still yank the previouse event content."
  (let* ((paste-str (nth 1 event)))
    (entropy/emacs-with-temp-buffer
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
   (let* ((paste (entropy/emacs-with-temp-buffer
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

Optional form COMMON-FORM run directly without any condition
judgements.

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


;; * provide
(provide 'entropy-emacs-defun)
