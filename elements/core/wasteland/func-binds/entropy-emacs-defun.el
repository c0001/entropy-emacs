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
;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-message)
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'rx)

;; ** common library
;; *** individuals
(defun entropy/emacs-func-aliasp (func)
  "Return non-nil if function FN is aliased to a function symbol."
  (let ((val (symbol-function func)))
    (and val
         (symbolp val))))

(defun entropy/emacs-theme-adapted-to-solaire ()
  "Judge whether current theme loaded adapted to `solaire-mode',
return t otherwise for nil. "
  (let ((theme_cur (ignore-errors (symbol-name entropy/emacs-theme-sticker))))
    ;; Condition judge for unconditional occurrence for theme loading,
    ;; seem as in pdumper session.
    (if (and (stringp theme_cur)
             (not (eql 0 (length theme_cur))))
        (catch :exit
          (dolist (regex entropy/emacs-solaire-themes-regex-list)
            (when (ignore-errors (string-match-p regex theme_cur))
              (throw :exit t))))
      nil)))

(eval-when-compile
  (defmacro entropy/emacs-add-hook-lambda-nil (name hook &rest body)
    "Biuld auto-named function prefixed by NAME a symbol and
inject into HOOK wrapped BODY."
    (let ()
      `(let ((prefix-name
              (intern (format "entropy/emacs-fake-lambda-nil-%s-function"
                              (symbol-name ',name))))
             func-define)
         (setq func-define
               (list 'defun prefix-name '()
                     '(progn ,@body)))
         (eval func-define)
         (add-hook ',hook
                   prefix-name)))))

(defun entropy/emacs-strict-plistp (arg)
  "Strict plist true-p checker.

The strict plist structed as key-value pairs appended list, the
car of it was key, each key was a symbol must using the ':xx'
type. Each key's value was grouped that say the key's 2-step
after place must be key as well, thus the 'strict' meaning."
  (cond
   ((not (listp arg))
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

(defun entropy/emacs-icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and entropy/emacs-use-icon
       (display-graphic-p)
       ;; Fixme: `find-font' can not be used in emacs batch mode.
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
;; *** plist manipulation
(defun entropy/emacs-get-plist-form
    (form-plist key &optional car no-error)
  "Like  `plist-get' but for getting the rest form of a key slot.

Do as the same as `plist-get' when CAR was non-nil.

Return a `progn' form or a nil-lambda form if the slot's rest form
are empty, or a nil.

If NO-ERROR was non-nil, press all the error asserts, otherwise
when KEY can not be found in FORM-PLIST, throw out an error, in
that case return nil."
  (let ((rest (cdr (member key form-plist)))
        (pt 0)
        rtn)
    (catch :exit
      (when (null rest)
        (when no-error
          (throw :exit nil))
        (user-error "Can not match '%s's form!" key))
      (while (and (not (and
                        (symbolp (nth pt rest))
                        (string-match "^:" (symbol-name (nth pt rest)))))
                  (not (null (nth pt rest))))
        (push (nth pt rest) rtn)
        (cl-incf pt))
      (if (null rtn)
          (setq rtn
                (if (null car) (lambda () nil) nil))
        (setq rtn
              (if (null car)
                  `(progn
                     ,@(reverse rtn))
                (car (reverse rtn)))))
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

;; *** process manipulation
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

;; *** key bindings
(defmacro entropy/emacs-!set-key (key command)
  (declare (indent defun))
  `(define-key entropy/emacs-top-keymap ,key ,command))

;; *** file and directories
(defun entropy/emacs-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\.$\\|\\.\\.$\\)" el))
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
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun entropy/emacs-list-subfiles (dir-root)
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

  Return its parent directory path using `file-name-directory'"
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


;; *** counter map list
(defun entropy/emacs-numberic-list (list-var)
  "Return list element mapped with numberic prefix which concated
with '0' as alignment state."
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

;; *** cl-wrapper for higher and slower emacs version compatible
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
     (cond ((and (fboundp ',cl-func)
                 (not (null (entropy/emacs-cl-findnew-p ',cl-func))))
            (setq cl-func-use ',cl-func))
           (t
            (require 'cl-lib)
            (setq cl-func-use
                  (entropy/emacs-cl-findnew-p ',cl-func))))
     (funcall cl-func-use ,@args)))

;; *** nth remap with `car' and `cdr'
(defun entropy/emacs-remap-for-nth (pos list-var)
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
  (let ((remap-form (entropy/emacs-remap-for-nth pos list-var)))
    (eval `(setf ,remap-form replace))))

;; *** map string match
(eval-when-compile
  (defun entropy/emacs-map-string-match-p (str matches)
    (let ((rtn 0))
      (dolist (el matches)
        (when (string-match-p (rx (regexp (eval el))) str)
          (cl-incf rtn)))
      (if (eq rtn 0)
          nil
        t))))

;; *** form manipulation
(defun entropy/emacs-replace-form-symbol
    (form sub-elt replace &optional parse-append)
  (let (form-patch
        (vector-form-p (vectorp form)))
    (mapc
     (lambda (el)
       (cond
        ((and (symbolp el) (eq el sub-elt))
         (setq form-patch
               (append form-patch
                       (if (null parse-append)
                           `(,replace)
                         replace))))
        ((sequencep el)
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

;; *** dolist macro progn sequence expand type
(defun entropy/emacs--progn-seq-dolist-core
    (looper body-list &optional not-do parse-append)
  (let ((sub-elt (car looper))
        (map-seq (cadr looper))
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
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body))

(defmacro entropy/emacs-progn-seq-dolist-with-parse-append
    (looper &rest body)
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body nil :parse-append))

(defmacro entropy/emacs-progn-seq-dolist-not-do (looper &rest body)
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body t))

(defmacro entropy/emacs-progn-seq-dolist-not-do-with-parse-append
    (looper &rest body)
  `(entropy/emacs--progn-seq-dolist-core
    ',looper ',body t :parse-append))

;; *** batch `eval-after-load'
(defmacro entropy/emacs-eval-after-load (feature &rest body)
  (let (forms (cnt 1) bound)
    (cond ((symbolp feature)
           (setq forms
                 `(eval-after-load ',feature
                    (lambda ()
                      ,@body))))
          ((and (listp feature)
                (> (length feature) 1))
           (setq feature (reverse feature)
                 forms `(eval-after-load ',(car feature) (lambda () ,@body))
                 bound (length (cdr feature)))
           (dolist (load (cdr feature))
             (setq forms
                   `(eval-after-load ',load
                      ,forms))))
          ((and (listp feature)
                (= 1 (length feature)))
           (setq forms
                 `(eval-after-load ',(car feature)
                    (lambda ()
                      ,@body)))))
    forms))

;; ** lazy load branch
(defmacro entropy/emacs-lazy-load-simple (feature &rest body)
  "Execute BODY after/require FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
  (declare (indent 1) (debug t))
  (cond
   (entropy/emacs-custom-enable-lazy-load
    `(when (not (null ',feature))
       (entropy/emacs-eval-after-load ,feature
         ,@body
         (message "with lazy loading configs for feature '%s' ..."
                  (if (symbolp ',feature)
                      (symbol-name ',feature)
                    ',feature)))))
   ((null entropy/emacs-custom-enable-lazy-load)
    `(when (not (null ',feature))
       (message "force load configs for feature '%s'" (symbol-name ',feature))
       (cond ((listp ',feature)
              (dolist (el ',feature)
                (require el)))
             ((symbolp ',feature)
              (require ',feature)))
       ,@body))))

(defmacro entropy/emacs-lazy-with-load-trail (name &rest body)
  (let ((func (intern
               (concat "entropy/emacs-lazy-trail-to-"
                       (symbol-name name))))
        (msg-str (symbol-name name)))
    `(progn
       (defun ,func ()
         (entropy/emacs-message-do-message
          "%s '%s' %s"
          (blue "Start")
          (yellow ,msg-str)
          (blue "..."))
         ,@body
         (entropy/emacs-message-do-message
          "%s '%s' %s"
          (blue "Start")
          (yellow ,msg-str)
          (blue "done!")))
       (cond
        (entropy/emacs-fall-love-with-pdumper
         (setq entropy/emacs-pdumper-load-hook
               (append entropy/emacs-pdumper-load-hook
                       '(,func))))
        (t
         (set (entropy/emacs-select-x-hook)
              (append (symbol-value (entropy/emacs-select-x-hook))
                      '(,func))))))))

(defun entropy/emacs-lazy-initial-form
    (list-var initial-func-suffix-name initial-var-suffix-name abbrev-name adder-name &rest form_args)
  (let* ((func (intern (concat abbrev-name "_" initial-func-suffix-name)))
         (adder-func (intern (concat (symbol-name func) "_" adder-name)))
         (var (intern (concat abbrev-name "_+" initial-var-suffix-name)))
         (adder-type (car form_args))
         (adder-flag (cadr form_args))
         (func-body (nth 2 form_args)))
    `(progn
       (defvar ,var nil)
       (defun ,func (&rest _)
         (let ((head-time (time-to-seconds))
               end-time)
           (unless ,var
             (redisplay t)
             (entropy/emacs-message-do-message
              "%s '%s' %s"
              (blue "Loading and enable feature")
              (yellow ,initial-func-suffix-name)
              (blue "..."))
             ,@func-body
             (setq ,var t)
             (setq end-time (time-to-seconds))
             (entropy/emacs-message-do-message
              "%s '%s' %s '%s' %s"
              (green "Load done for")
              (yellow ,initial-func-suffix-name)
              (green "within")
              (cyan (format "%f" (- end-time head-time)))
              (green "seconds."))
             (redisplay t))))
       (let ((hook (entropy/emacs-select-x-hook)))
         (cond
          ((and (not entropy/emacs-custom-enable-lazy-load)
                (not entropy/emacs-fall-love-with-pdumper))
           (set hook (append (symbol-value hook) '(,func))))
          (t (defun ,adder-func ()
               (dolist (item ,list-var)
                 (if (not (null ,adder-flag))
                     (,adder-type item ,adder-flag ',func)
                   (,adder-type item ',func))))
             (set hook (append (symbol-value hook) '(,adder-func)))))))))

(defmacro entropy/emacs-lazy-initial-for-hook
    (hooks initial-func-suffix-name initial-var-suffix-name &rest body)
  (entropy/emacs-lazy-initial-form
   hooks initial-func-suffix-name initial-var-suffix-name
   "entropy/emacs--hook-first-enable-for" "hook-adder"
   'add-hook
   nil
   body))

(defmacro entropy/emacs-lazy-initial-advice-before
    (advice-fors initial-func-suffix-name initial-var-suffix-name &rest body)
  (entropy/emacs-lazy-initial-form
   advice-fors initial-func-suffix-name initial-var-suffix-name
   "entropy/emacs--beforeADV-fisrt-enable-for"
   "before-advice-adder"
   'advice-add
   :before
   body))

;; ** package user dir config

(defvar entropy/emacs--package-user-dir-setted nil)

(defun entropy/emacs--guard-ext-use-type ()
  (unless (member entropy/emacs-use-extensions-type
                  '(origin submodules submodules-melpa-local))
    (error "Invalid value for `entropy/emacs-use-extensions-type'")))

(defun entropy/emacs-package-is-upstream ()
  "Judges whether `entropy/emacs-use-extensions-type' is based on
`package.el'."
  (or (eq entropy/emacs-use-extensions-type 'origin)
      (eq entropy/emacs-use-extensions-type 'submodules-melpa-local)))

(defun entropy/emacs--set-user-package-dir-common (version)
  "Setting `package-user-dir' based on emacs version"
  (setq package-user-dir
        (expand-file-name
         (concat "elpa-" version)
         (expand-file-name entropy/emacs-ext-extensions-elpa-dir))))

(defun entropy/emacs-set-package-user-dir ()
  (unless entropy/emacs--package-user-dir-setted
    (entropy/emacs--guard-ext-use-type)
    (if (and (member emacs-version '("25.2.1" "25.3.1" "26.1" "26.2" "27.0.50" "28.0.50"))
             (entropy/emacs-package-is-upstream))
        (entropy/emacs--set-user-package-dir-common emacs-version)
      (cond
       ((and (equal emacs-version "25.2.2")
             (entropy/emacs-package-is-upstream))
        (entropy/emacs--set-user-package-dir-common "25.2.1"))
       ((and (equal emacs-version "26.3")
             (entropy/emacs-package-is-upstream))
        (entropy/emacs--set-user-package-dir-common "26.2"))
       ((and (or (equal emacs-version "27.0.60")
                 (equal emacs-version "27.0.90"))
             (entropy/emacs-package-is-upstream))
        (entropy/emacs--set-user-package-dir-common "27.0.50"))
       ((entropy/emacs-package-is-upstream)
        (error "Unsupport emacs version '%s'" emacs-version))))
    (when (eq entropy/emacs-use-extensions-type 'submodules-melpa-local)
      (setq package-user-dir
            (expand-file-name (concat (entropy/emacs-file-path-parser package-user-dir 'file-name)
                                      "_MelpaLocal")
                              (entropy/emacs-file-path-parser package-user-dir 'parent-dir))))
    (setq entropy/emacs--package-user-dir-setted t)))

;; ** language environment set

(defun entropy/emacs-lang-set (lang)
  (if (string-match-p
       "\\*e?shell\\*\\|\\*eshell-.*?\\*\\|\\(^\\*ansi-term-.*\\)\\|\\(\\*terminal\\)"
       (format "%s" (buffer-list)))
      (error "Can not use this function cause shell buffer exist, please kill it and try again!")
    (cond
     ((string= lang "UTF-8")
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8-unix)
      (message "Setting language environment to 'utf-8-unix'."))
     ((string= lang "LOCAL")
      (when (and (not (null entropy/emacs-custom-language-environment-enable))
                 (not (null entropy/emacs-locale-language-environment))
                 (stringp entropy/emacs-locale-language-environment))
        (set-language-environment entropy/emacs-locale-language-environment)
        (prefer-coding-system entropy/emacs-locale-coding-system)
        (setq default-file-name-coding-system 'utf-8-unix)
        (message "Setting language environment to '%s'." entropy/emacs-locale-language-environment)))
     (t (error "Invalid LANG arg")))))

(defun entropy/emacs-lang-set-utf-8 (&rest args)
  "Setting language envrionment to unix-8-unix, supported
by `entropy/emacs-lang-set'"
  (if (not (string= current-language-environment "UTF-8"))
      (entropy/emacs-lang-set "UTF-8")))

(defun entropy/emacs-lang-set-local (&rest args)
  "Setting language environment to `locale' specification from
`entropy/emacs-locale-language-environment'. "
  (if (and entropy/emacs-custom-language-environment-enable
           (not (null entropy/emacs-locale-language-environment)))
      (entropy/emacs-lang-set "LOCAL")))

;; common around advice for wrapper function into utf-8 environment
(defun entropy/emacs-lang-use-utf-8-ces-around-advice (old-func &rest _)
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (apply old-func _)))

;; common around advice for wrapper funcion into locale language environment
(defun entropy/emacs-lang-use-locale-ces-around-advice (old-func &rest _)
  (let ((coding-system-for-read entropy/emacs-locale-coding-system)
        (coding-system-for-write entropy/emacs-locale-coding-system))
    (apply old-func _)))

;; the 'with' macro
(defmacro entropy/emacs-lang-with-utf-8-ces (&rest body)
  `(let* ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     ,@body))

(defmacro entropy/emacs-lang-with-locale-ces (&rest body)
  `(let* ((coding-system-for-read entropy/emacs-locale-coding-system)
          (coding-system-for-write entropy/emacs-locale-coding-system))
     ,@body))



;; ** newwork manipulation
;; *** network status checker
(defun entropy/emacs-network-canbe-connected-?-p (hostname)
  (let ((ping-args-core (if sys/win32p '("-n" "1" "-w" "10")
                          '("-c" "1" "-W" "10"))))
    (= 0 (apply 'call-process `("ping" nil nil nil ,@ping-args-core
                                ,hostname)))))

(defun entropy/emacs-network-connected-p ()
  (or (entropy/emacs-network-canbe-connected-?-p "www.baidu.com")
      (entropy/emacs-network-canbe-connected-?-p "www.google.com")))

;; *** download file
(defun entropy/emacs-network-download-file
    (url destination &optional use-curl async verify)
  "Download file from URL to DESTINATION via alternative
synchronous or async method using emacs native `url-retrieve' or
curl subprocess when USE-CURL non-nil, then verify the donwload
file via the VERIFY function with a single argument the
DESTINATION file-name.

This function return a callback status of a random symbol whose
valid value are 'success' and 'failed' or nil while download
process doesn't finished."
  (let* ((tmp-file (expand-file-name
                    (format "eemacs-download-tmpfile_[%s]"
                            (format-time-string "%Y%m%d%H%M%S"))
                    temporary-file-directory))
         (cbk-symbol (let ((sym (intern (format "eemacs-network-download-random-cbk_%s"
                                                (random)))))
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
                   (if (functionp ,verify)
                       (if (funcall ,verify destination)
                           (setq ,cbk-symbol 'success)
                         (setq ,cbk-symbol 'failed))
                     (setq ,cbk-symbol 'success)))
               (error
                (message (car (cdr error)))
                (setq ,cbk-symbol 'failed)
                (when (file-exists-p ,tmp-file)
                  (delete-file ,tmp-file)))))))
    (let* ((proc-buffer (get-buffer-create "*---eemacs-url-donwload---*"))
           (inhibit-read-only t)
           (success-message (format "Download from '%s' finished" url))
           (success-func `(lambda ()
                            (message ,success-message)
                            (funcall ,move-to-des-func)))
           (fatal-message (format "Download file form '%s' failed!" url))
           (fatal-func `(lambda ()
                          (setq ,cbk-symbol 'failed)
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
                :command `("curl" "--connect-timeout" "10" ,url "-o" ,tmp-file)))
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
            (if (eq (call-process
                     "curl" nil nil nil
                     "--connect-timeout" "10"
                     url "-o" tmp-file)
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



;; ** compress or decompress file

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

(defun entropy/emacs-gen-archive-dowith-shell-command
    (archive-type input output dowith)
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

;; ** Org face reset
(defvar entropy/emacs-defun--ohrsc-previous-theme nil)

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
  (let (judge)
    (setq judge
          (catch :exit
            (dolist (face entropy/emacs-defun--ohrsc-org-header-backup-faces)
              (unless (facep face)
                (throw :exit 'lost)))))
    (if (eq judge 'lost)
        nil
      t)))

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
  (let ((count 0))
    (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
      (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
        (copy-face face face-bcp))
      (cl-incf count))))

(defun entropy/emacs-adjust-org-heading-scale ()
  "Stop the org-level headers from increasing in height
relative to the other text when
`entropy/emacs-disable-org-heading-scale' was non-nil."
  (when (null entropy/emacs-defun--ohrsc-previous-theme)
    (setq entropy/emacs-defun--ohrsc-previous-theme
          entropy/emacs-theme-sticker))
  (when (and (not (null entropy/emacs-defun--ohrsc-previous-theme))
             (not (equal entropy/emacs-theme-sticker
                         entropy/emacs-defun--ohrsc-previous-theme)))
    (entropy/emacs-defun--ohrsc-backup-org-header-face)
    (setq entropy/emacs-defun--ohrsc-previous-theme
          entropy/emacs-theme-sticker))
  (cond
   (entropy/emacs-disable-org-heading-scale
    (unless (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
      (entropy/emacs-defun--ohrsc-backup-org-header-face))
    (entropy/emacs-defun--ohrsc-cancel-org-header-face-scale))
   ((null entropy/emacs-disable-org-heading-scale)
    (when (and (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
               (entropy/emacs-defun--ohrsc-org-header-faces-modified-p))
      (entropy/emacs-defun--ohrsc-recovery-org-header-face-scale)))))

;; ** theme loading specific
(defun entropy/emacs-theme-load-face-specifix (&optional x)
  "Advice for `counsel-load-theme-action' that setting face of
`ivy-current-match' for spacemacs themes.

Reason of this setting was that spacemacs has the un-obviouse
visual distinction of `ivy-current-match' covered upon the
`ivy-minibuffer-match-highlight'."
  (unless x
    (setq x (symbol-name entropy/emacs-theme-sticker)))
  (cond
   ((string-match-p "spacemacs-dark" x)
    (with-eval-after-load 'ivy
      (set-face-attribute 'ivy-current-match nil
                          :background "purple4" :bold t)))
   ((string-match-p "spacemacs-light)" x)
    (with-eval-after-load 'ivy
     (set-face-attribute 'ivy-current-match nil
                        :background "salmon" :bold t)))
   ((string-match-p "darkokai" x)
    (with-eval-after-load 'ivy
      (set-face-attribute 'ivy-current-match nil
                          :background "#65a7e2")))
   ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" x)
    (with-eval-after-load 'ivy
      (if (equal 'dark (frame-parameter nil 'background-mode))
          (set-face-attribute 'ivy-current-match nil
                              :background "#65a7e2" :foreground "black")
        (set-face-attribute 'ivy-current-match nil
                            :background "#1a4b77" :foreground "white"))))
   ((string= "doom-solarized-light" x)
    (when (not (featurep 'hl-line))
      (require 'hl-line))
    (set-face-attribute 'hl-line nil :background "moccasin"))
   (t
    (entropy/emacs-set-fixed-pitch-serif-face-to-monospace))))

(defun entropy/emacs-theme-load-modeline-specifix (&optional arg)
  "Advice of auto refresh doom-modeline bar background color
when changing theme."
  (unless arg
    (setq arg (symbol-name entropy/emacs-theme-sticker)))
  (progn
    (cond ((and (string= entropy/emacs-mode-line-sticker "doom")
                (string-match-p "\\(ujelly\\)" arg))
           (set-face-attribute 'doom-modeline-bar
                               nil :background "black")
           (doom-modeline-refresh-bars))
          ((and (string= entropy/emacs-mode-line-sticker "doom")
                (string-match-p "\\(spolsky\\)" arg))
           (setq doom-modeline--bar-active
                 (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                          doom-modeline-bar-width
                                          doom-modeline-height)))
          ((string= entropy/emacs-mode-line-sticker "doom")
           (set-face-attribute 'doom-modeline-bar
                               nil :background (face-background 'mode-line nil t))
           (doom-modeline-refresh-bars)))))

(defun entropy/emacs-solaire-specific-for-themes ()
  (when (entropy/emacs-theme-adapted-to-solaire)
    (require 'hl-line)
    (require 'solaire-mode)
    (cond
     ((eq entropy/emacs-theme-sticker 'spacemacs-dark)
      (dolist (x '(solaire-hl-line-face hl-line))
        (set-face-attribute
         x
         nil
         :background
         (cond ((not (display-graphic-p))
                "color-236")
               ((display-graphic-p)
                "#293c44")))))
     (t nil))))

;; ** advice around for case-fold-search
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


;; ** common face setting
(defun entropy/emacs-set-fixed-pitch-serif-face-to-monospace ()
  "Set info-mode font-lock spec face `fixed-pitch-serif' to
entropy-emacs specific monospace style.

This funciton will solve the problem that the symbol pattern
display ugly and small in info-mode."
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Monospace" :slant 'italic))

;; ** key map refer
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

;; ** cli compatibale
(defvar entropy/emacs--xterm-clipboard-head nil
  "The string of the head of the last event paste part of
`xterm-paste'.")

(defvar entropy/emacs-xterm-paste-inhibit-read-only-filter nil
  "The conditions for judge whether xterm-paste with
`inhibit-read-only'.

Eacch condition is a function with one arg, the paste event.")

(defvar entropy/emacs-xterm-paste-yank-replacement-register nil
  "List of predicate pattern cons for using instead of `yank'.

Which each car of the pattern was a condition, may be 'nil' or
't' or a function for be evaluated for the boolean result, and
the cdr was the replacement yank function")

(defun entropy/emacs-xterm-external-satisfied-p ()
  "Judge whether emacs to use external kits to assistant the
xterm-session yank/paste operation."
  (let ((judger
         '(or
           ;; darwin (macos platform)
           (and (and (eq system-type 'darwin)
                     (executable-find "pbcopy"))
                'pbpaste)
           ;; cygwin (windows posix emulator)
           (and (eq system-type 'cygwin)
                (executable-find "getclip")
                'getclip)
           ;; android termux emulator
           (and (executable-find "termux-clipboard-get")
                'termux-clipboard-get)
           ;; gnu/linux platform
           (and (eq system-type 'gnu/linux)
                (or
                 (and (executable-find "xclip") 'xclip)
                 (and (executable-find "xsel") 'xsel))))))
    (when (and (not (display-graphic-p))
               (fboundp 'xterm-paste)
               (ignore-errors
                 (progn (require 'xclip)
                        (xclip-mode 1))))
      judger)))

(defun entropy/emacs-xterm-paste-core (event)
  "The eemacs subroutine for `xterm-paste' event to automatically
traceback to `kill-ring'."
  (let* ((paste-str (nth 1 event)))
    (with-temp-buffer
      (unless (equal paste-str
                     entropy/emacs--xterm-clipboard-head)
        (progn (setq entropy/emacs--xterm-clipboard-head
                     paste-str)
               (xterm-paste event)))
      (yank))))

(defmacro entropy/emacs--with-xterm-paste-core (event &rest body)
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
`entropy/emacs-xterm-paste-core'.
"
  (interactive "e")
  (entropy/emacs--with-xterm-paste-core
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
  (entropy/emacs--with-xterm-paste-core
   event
   (let* ((paste (with-temp-buffer
                   (yank)
                   (car kill-ring))))
     (when (and (stringp paste)
                (not (string= "" paste)))
       (setq paste (substring-no-properties paste))
       (term-send-raw-string paste)))))

(defun entropy/emacs-xterm-paste-sshsession ()
  (interactive)
  (let ()
    (run-with-timer 0.01 nil #'yank)
    (keyboard-quit)))

(defun entropy/emacs-basic-xterm-term-S-insert-sshsession ()
  (interactive)
  (run-with-timer
   0.01 nil
   #'(lambda ()
       (let* ((paste (with-temp-buffer
                       (yank)
                       (car kill-ring))))
         (when (stringp paste)
           (setq paste (substring-no-properties paste))
           (term-send-raw-string paste)))))
  (keyboard-quit))

;; ** miscellaneous
(defun entropy/emacs-transfer-wvol (file)
  "Transfer linux type root path header into windows volumn
format on windows platform."
  (if (and (string-match-p "^/[a-z]/" file)
           sys/win32p)
      (let ((wvol (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:" file)))
        (find-file wvol))
    (find-file file)))

(defun entropy/emacs-get-theme-face (theme face)
  (let ((theme-settings (get theme 'theme-settings)))
    (catch :exit
      (dolist (theme-setting theme-settings)
        (when (and (eq 'theme-face (car theme-setting))
                   (eq face (cadr theme-setting)))
          (throw :exit (cadddr theme-setting)))))))

(defun entropy/emacs-buffer-is-lisp-like-p ()
  "Justify current buffer is lisp like, any value for true, nil
for otherwise."
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

;; * provide
(provide 'entropy-emacs-defun)
