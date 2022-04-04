;;; entropy-emacs-hydra-hollow.el --- entropy-emacs hydra framework
;;
;; * Copyright (C) 2019  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Created:       2019-12-10
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
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

;; This package built entropy-emacs union hydra framework based on
;; =pretty-hydra= but stand on a superstructure type.

;; * Configuration:
;;
;; Designed for =entropy-emacs= only, recommend for using as reference.

;; * Todo Lists
;; ** TODO Update hydra-hollow category title patch mechnaism

;; Origin navigation and page indicator for the hydra-hollow category
;; are gernerated by the simple concatenate method, which just using
;; regexp matching the current status via its represented string, in
;; this case there's two main problems:

;; 1) It can not arrange the multi indicator concatenate sequence
;;    order when the new injection arrived.
;; 2) It do not bind with category object modification, so that its
;;    static for now which must be dynamically in full implements
;;    required for this package.

;; Plan:

;; Using the concatenate abstract form to represent the hydra body
;; title to register it into category object for as the current hydra
;; body title status which can be used to inspet and upon to the
;; provenance for modification.

;; Since we using simple `concat' function to patch the hydra body
;; title here, that in astract way we anc illustrate a patch as:
;; #+begin_example
;; (+ title flag-a flag-b ...)

;; (+ flag-c (+ title flag-a flag-b ...))

;; ...
;; #+end_example

;; Using thus flag to relect a specific patching, in this case we can
;; let =flag-a= as 'upon' navigation patching sign and =flag-b= as
;; the opposed, and let =flag-c= as the page number indicator patch
;; sign.

;; We generate a new key-slot for a =pretty-hydra-category= object,
;; the =:pretty-hydra-category-title-abstract-form= which based on
;; =:pretty-hydra-category-base-pretty-hydra-body= plist key :title
;; value as its 'title' original type. And then when patching the
;; title by a specified type handler can be modify the
;; title-abstract-form with the refer flag matched by that handler,
;; and then expand the abstract form into the instance and update the
;; abstract form for current new status lastly.

;; * Code
;; ** require
(!eemacs-require 'entropy-emacs-defun)
(!eemacs-require 'entropy-emacs-utils)
(require 'cl-lib)
(require 'use-package)

;; ** libraries
;; *** basic library
(defvar entropy/emacs-hydra-hollow-union-form
  '(lambda (&rest _))
  "A lambda form stored some predicated procedure that will run
at some proper time.")

(defvar entropy/emacs-hydra-hollow-call-before-hook nil
  "Hook before run any eemacs-hydra-hollow instances")

(defvar entropy/emacs-hydra-hollow-random-func-name-number-register nil
  "The register for the random number suffix as name for random
function naming.

Do not manually modify this variable or risking on your self.")

(defun entropy/emacs-hydra-hollow-call-union-form (&rest _)
  "Call `entropy/emacs-hydra-hollow-union-form' recursively til
it becoming one empty form."
  ;; Firstly we run the before hook before any operations
  (run-hooks 'entropy/emacs-hydra-hollow-call-before-hook)
  (let (
        ;; NOTE: this is important to prevent hydra hollow init failure
        (inhibit-quit t)
        (copy-union-form (copy-tree entropy/emacs-hydra-hollow-union-form)))
    (unless (null (cddr copy-union-form))
      (progn
        (funcall entropy/emacs-hydra-hollow-union-form)
        (unless (equal copy-union-form entropy/emacs-hydra-hollow-union-form)
          (dolist (form (cddr copy-union-form))
            (setq entropy/emacs-hydra-hollow-union-form
                  (cl-delete form entropy/emacs-hydra-hollow-union-form
                             :test 'equal)))
          (entropy/emacs-hydra-hollow-call-union-form))
        (unless (eq entropy/emacs-hydra-hollow-union-form
                    '(lambda (&rest _)))
          (setq entropy/emacs-hydra-hollow-union-form
                '(lambda (&rest _))))))))

(defvar entropy/emacs-hydra-hollow-call-union-form-adviced-list nil
  "A list of functions who has been adviced by
`entropy/emacs-hydra-hollow-advice-for-call-union-form'.

So that it can be the database for other process judging whether
need to advice as thus again.")

(defun entropy/emacs-hydra-hollow-advice-for-call-union-form (func)
  "Advice a function before by
`entropy/emacs-hydra-hollow-call-union-form' with judging whether
this operation was did that prevent do again."
  (unless (member
           func
           entropy/emacs-hydra-hollow-call-union-form-adviced-list)
    (progn (advice-add func :before #'entropy/emacs-hydra-hollow-call-union-form)
           (push func entropy/emacs-hydra-hollow-call-union-form-adviced-list))))

(defun entropy/emacs-hydra-hollow-func-version-pthydra-define
    (name body heads-plist)
  "Function version for `pretty-hydra-define' so that the origin
arguments can be evaluated before the macro expanding."
  (funcall
   `(lambda ()
      (pretty-hydra-define
        ,name ,body ,heads-plist))))

(defun entropy/emacs-hydra-hollow-func-version-pthydra-define+
    (name body heads-plist)
  "Function version for `pretty-hydra-define+' so that the origin
arguments can be evaluated before the macro expanding."
  (funcall
   `(lambda ()
      (pretty-hydra-define+
        ,name ,body ,heads-plist))))

(defun entropy/emacs-hydra-hollow-self-list-type-p (data)
  "The =self-list= data type predicate function.

A =self-list= data type is a abstract data type which served for
some data-type which either a list of *itself*.

A valid =self-list= data is a list which car must `eq' to
':self-list' and its cdr can not be nil that indicate for that the
rest must at least including one *itself*.

There's no restriction for what *itself* is."
  (let ()
    (and (listp data)
         (eq (car data) :self-list)
         (> (length data) 1))))

;; *** entropy-emacs pretty hydra superstructure

;; This section gives a riched pretty-hydra topside framework to
;; define a hydra.

;; *Main features*

;; 1) Add more hydra head key slot.
;; 2) Add category page navigability feature for restructing
;;    pretty-hydra group arrangement

;; **** core defination
;; ***** core protocols

;; This section was the core API for entropy-emacs pretty hyra
;; superstructer that define the core data type and their
;; manipulation.

;; *Data type:*

;; + =pretty-hydra-cabinet=

;;   It's an alias for pretty-hydra origin =heads-plist= data type
;;   but with more human readable renaming means.

;; + =pretty-hydra-cabinet-unit=

;;   It's a =pretty-hydra-cabinet= which just has one group.

;; + =pretty-hydra-cabinet-unit-name=

;;   The name of the one =pretty-hydra-cabinet-unit=, its a string.

;; + =pretty-hydra-cabinet-units-list=

;;   A list of =pretty-hydra-cabinet-unit=, i.e. sets of that.

;; + =pretty-hydra-body=
;; + =pretty-hydra-body-title=
;; + =pretty-hydra-body-quit-key=
;; + =pretty-hydra-body-quit-color=

;; + =pretty-hydra-head=

;;   Its an alias for =hydra-head= but be with more flexible based on
;;   pretty hydra framework and further more stands top on
;;   entropy-emacs pretty hydra supperstructure.

;; + =pretty-hydra-head-key=
;; + =pretty-hydra-head-command=
;; + =pretty-hydra-head-notation=
;; + =pretty-hydra-head-slots=

;; + =pretty-hydra-heads-list=

;;   A list of =pretty-hydra-head=, i.e. sets of that.

;; + =pretty-hydra-casket=

;;   A =pretty-hydra-cabinet= but with one restriction that its
;;   =pretty-hydra-heads-list= just has one element.

;; + =pretty-hydra-caskets-list=

;;   A list of =pretty-hydra-group-head=, i.e. sets of that.

;; + =pretty-hydra-house=

;;   A list of one =pretty-hydra-body= and one =pretty-hydra-cabinet=.



(defun entropy/emacs-hydra-hollow--common-judge-p
    (pattern)
  "Judge the PATTERN with its predicated result as the return, with
other aiming that for obtained result of sets of special PATTERN
type listed as:

- Data Type :: PATTERN is list and the car of it is a special
  symbol named as =:data=, return the cdr of PATTERN

- Eval Type :: PATTERN is list and the car of it is a special
  symbol named as =:eval=, inject the rest of it into lambda body
  and return the evaluated result.

- Eval form type :: PATTERN is a list and non special car
  indicator, recognize it as aform which will be evaluated and
  return the result.

- Symbol type :: PATTERN is a symbol and return its value using
  `symbol-value'.

- Others :: miscellaneous PATTERN type will not be judging any way
  return its self as well."
  (cond ((and (listp pattern)
              (eq (car pattern) :data))
         (cdr pattern))
        ((and (listp pattern)
              (eq (car pattern) :eval))
         (funcall `(lambda nil ,@(cdr pattern))))
        ((and (listp pattern)
              (not (null pattern)))
         (funcall `(lambda () ,pattern)))
        ((and (symbolp pattern)
              (member pattern '(nil t)))
         (unless (null pattern)
           t))
        ((symbolp pattern)
         (symbol-value pattern))
        (t
         pattern)))

(defun entropy/emacs-hydra-hollow-partion-pretty-hydra-cabinet
    (pretty-hydra-cabinet)
  "Categorized the PRETTY-HYDRA-CABINET for a list of special
 =pretty-hydra-cabinet= for that its just has one group, so that
 we called it =pretty-hydra-cabinet-unit=.

Its return value (typed name as =pretty-hydra-cabinet-units-list=)
was ordered by the same sequence of the origin groups order."
  (let (rtn (cnt 0))
    (while (< cnt (/ (length pretty-hydra-cabinet) 2))
      (push (list (nth (* cnt 2) pretty-hydra-cabinet)
                  (nth (+ 1 (* cnt 2)) pretty-hydra-cabinet))
            rtn)
      (cl-incf cnt))
    (reverse rtn)))

(defun entropy/emacs-hydra-hollow-prune-pretty-hydra-cabinet
    (pretty-hydra-cabinet)
  "Map delete the empty heads group in PRETTY-HYDRA-CABINET whose heads
was undefined, and return the new same ordered
=pretty-hydra-cabinet= or nil when all group's heads are undefined."
  (cl-loop for cnt from 0 to (- (/ (length pretty-hydra-cabinet) 2) 1)
           if (not (null (nth (+ (* cnt 2) 1) pretty-hydra-cabinet)))
           collect (nth (* cnt 2) pretty-hydra-cabinet)
           and collect (nth (+ (* cnt 2) 1) pretty-hydra-cabinet)))

(defun entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list
    (pretty-hydra-cabinet)
  "Split PRETTY-HYDRA-CABINET into a list of =pretty-hydra-cabinet=.
That each =pretty-hydra-cabinet= just has one head in its HEADS
slot. Each heads of =pretty-hydra-cabinet= can be nil. The
returned list of =pretty-hydra-cabinet= was ordered as same as the
original."
  (let (pretty-hydra-caskets-list)
    (cl-loop for cnt from 0 to (- (/ (length pretty-hydra-cabinet) 2) 1)
             do (let* ((cnt (* cnt 2))
                       (group (nth cnt pretty-hydra-cabinet))
                       (heads (nth (+ cnt 1) pretty-hydra-cabinet))
                       rtn)
                  (cond ((not (null heads))
                         (dolist (el heads)
                           (push (list group (list el)) rtn)))
                        (t
                         (push (list group '()) rtn)))
                  (when rtn
                    (setq pretty-hydra-caskets-list
                          (append pretty-hydra-caskets-list (reverse rtn))))))
    pretty-hydra-caskets-list))

(defun entropy/emacs-hydra-hollow-merge-pretty-hydra-caskets-list
    (pretty-hydra-caskets-list)
  "Merge the PRETTY-HYDRA-CASKETS-LIST into =pretty-hydra-cabinet=,
with its origin order.

PRETTY-HYDRA-CASKETS-LIST is the list of =pretty-hydra-cabinet=
whose =pretty-hydra-heads-list= has only one =pretty-hydra-head=
which can be built by
`entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list' see
its doc for more details."
  (let* ((manipulate-list (copy-tree pretty-hydra-caskets-list))
         pretty-hydra-cabinet-fake rtn)
    (while (not (null manipulate-list))
      (let ((group (caar manipulate-list))
            (rest (cdr manipulate-list))
            pointer-list collect)
        (if (eq (length manipulate-list) 1)
            (push (pop manipulate-list) pretty-hydra-cabinet-fake)
          (cl-loop for pt from 0 to (- (length rest) 1)
                   when (equal (car (nth pt rest)) group)
                   do (push pt pointer-list))
          (if (null pointer-list)
              (push (pop manipulate-list) pretty-hydra-cabinet-fake)
            (progn
              (dolist (el pointer-list)
                (push (caadr (nth el rest)) collect))
              (push (caadar manipulate-list) collect))
            (when collect
              (push (list group
                          (cl-delete nil collect))
                    pretty-hydra-cabinet-fake))
            (progn
              (dolist (pt pointer-list)
                (entropy/emacs-setf-for-nth
                 (+ pt 1) nil manipulate-list))
              (pop manipulate-list)
              (setq manipulate-list
                    (cl-delete nil manipulate-list)))))))
    (dolist (el (reverse pretty-hydra-cabinet-fake))
      (setq rtn (append rtn el)))
    rtn))

(defun entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler
    (pretty-hydra-head-notation type &optional not-beautify-notation)
  "Hydra head notation handler for patching it with faces adding
with specified indicator."
  (dolist (feature '(faces))
    (require feature))
  (let* ((nface 'entropy/emacs-defface-face-for-hydra-grey-face)
         (match-map
          `((global-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "g" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face ',nface)))
            (mode-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "m" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face ',nface)))
            (eemacs-top-keymap-inject
             :format "%s%s"
             :icon (lambda () (propertize "e" 'face 'success))
             :notation (lambda (notation) (propertize notation 'face ',nface)))))
         (matched (alist-get type match-map))
         (fmstr (plist-get matched :format))
         (icon (funcall (plist-get matched :icon)))
         (notation (or (when not-beautify-notation
                         pretty-hydra-head-notation)
                       (funcall (plist-get matched :notation) pretty-hydra-head-notation))))
    (format fmstr icon notation)))

;; ***** category framework

;; This section defined entropy-emacs main pretty-hydra *define* and
;; *define+* function for replace the ~pretty-hydra-define~ and
;; ~pretty-hydra-define+~ function for using entropy-emacs pretty
;; hydra superstructuer feature i.e. the hydra group or category
;; paging type.

;; ****** library
;; ******* core

;; This section gives the =pretty-hydra-category= prototype and their
;; method.

;; *Data-type:*
;; - =pretty-hydra-category=
;; - =pretty-hydra-category-hook=
;; - =pretty-hydra-category-name-prefix=
;; - =pretty-hydra-category-name=
;; - =pretty-hydra-category-hook-name=
;; - =pretty-hydra-category-hydra-name=
;; - =pretty-hydra-category-hydra-body=
;; - =pretty-hydra-category-hydra-body-name=
;; - =pretty-hydra-category-hydra-keymap=
;; - =pretty-hydra-category-hydra-keymap-name=
;; - =pretty-hydra-category-hydra-caller-name=
;; - =pretty-hydra-category-cabinet=
;; - =pretty-hydra-category-cabinet-unit-name=
;; - =pretty-hydra-category-cabinet-unit-names-list=
;; - =pretty-hydra-category-depth=
;; - =pretty-hydra-category-width=
;; - =pretty-hydra-category-baron-name=
;; - =pretty-hydra-category-previous-category-name=
;; - =pretty-hydra-category-next-category-name=
;; - =pretty-hydra-category-base-pretty-hydra-body=
;; - =pretty-hydra-category-width-indicator=

;; ******** category prototype

(defvar entropy/emacs-hydra-hollow-category-default-width 4
  "The default pretty-hydra-category-width.")

(cl-defun entropy/emacs-hydra-hollow-set-pretty-hydra-category-name
    (pretty-hydra-category-name
     &key
     pretty-hydra-category-name-prefix
     pretty-hydra-category-description
     pretty-hydra-category-hydra-name
     pretty-hydra-category-hydra-caller-name
     pretty-hydra-category-base-pretty-hydra-body
     pretty-hydra-category-cabinet-unit-names-list
     pretty-hydra-category-depth
     pretty-hydra-category-width
     pretty-hydra-category-baron-name
     pretty-hydra-category-previous-category-name
     pretty-hydra-category-next-category-name)
  "Create a entropy-emacs =pretty-hydra-category= by its name
CATEGORY-NAME.

A =pretty-hydra-category= is a categorized =pretty-hydra= who has
the chained feature for other =pretty-hydra-category=.

For details that:

=pretty-hydra-category= is a plist that stored sets of value to
indicating its categorized unit type. It has siblings and baron,
thus one category was also the member of one
=pretty-hydra-category= chain.

Slots description:

- =pretty-hydra-category-name-prefix=

  The prefix name to build this =pretty-hydra-category=, it's a
  symbol.

- =pretty-hydra-category-description=

  The description string for this =pretty-hydra-category=.

- =pretty-hydra-category-hydra-name=

  The =pretty-hydra-name= for this =pretty-hydra-category=, it's a
  symbol.

- =pretty-hydra-category-hydra-caller-name=

  The hydra caller function for this =pretty-hydra-category=, it's
  a symbol.

- =pretty-hydra-category-base-pretty-hydra-body=

  The base =pretty-hydra-body= for this
  =pretty-hydra-category=. It was the hydra body built in the
  =pretty-hydra= defination time, so that this slot provides a
  chance to obtained its origin =pretty-hyra-body=.

- =pretty-hydra-category-cabinet-unit-names-list=

  A list stored all =pretty-hydra-cabinet-unit-name= for this
  =pretty-hydra-category=.

- =pretty-hydra-category-depth=

  The depth (a positive non-zero integer) for this
  =pretty-hydra-category=.

- =pretty-hydra-category-width=

  The width (a positive non-zero integer) for this
  =pretty-hydra-category=. Width indicates the max cabinet slots
  can has for this =pretty-hydra-category='s
  =pretty-hydra-cabinet=, so any new cabinet slot injection will
  be automatically inject to the next chain, if next chain was
  full either, to the next chain recursively til the one that not
  overflowed or create one new chain.

- =pretty-hydra-category-baron-name=

  A symbol whose value was a =pretty-hydra-category=. This slot
  store the 'baron' navigation meaning.

- =pretty-hydra-category-previous-category-name=

  As the name meaning as.

- =pretty-hydra-category-next-category-name=

  As the name meaning as.
"
  (cond
   ((null (entropy/emacs-common-plistp (ignore-errors (symbol-value pretty-hydra-category-name))))
    (set pretty-hydra-category-name
         (list
          :pretty-hydra-category-name-prefix pretty-hydra-category-name-prefix
          :pretty-hydra-category-description pretty-hydra-category-description
          :pretty-hydra-category-name pretty-hydra-category-name
          :pretty-hydra-category-hydra-name pretty-hydra-category-hydra-name
          :pretty-hydra-category-hydra-caller-name pretty-hydra-category-hydra-caller-name
          :pretty-hydra-category-base-pretty-hydra-body pretty-hydra-category-base-pretty-hydra-body
          :pretty-hydra-category-cabinet-unit-names-list pretty-hydra-category-cabinet-unit-names-list
          :pretty-hydra-category-depth pretty-hydra-category-depth
          :pretty-hydra-category-width pretty-hydra-category-width
          :pretty-hydra-category-baron-name pretty-hydra-category-baron-name
          :pretty-hydra-category-previous-category-name pretty-hydra-category-previous-category-name
          :pretty-hydra-category-next-category-name
          pretty-hydra-category-next-category-name)))
   ((entropy/emacs-common-plistp (ignore-errors (symbol-value pretty-hydra-category-name)))
    (dolist
        (el
         '((:pretty-hydra-category-name-prefix pretty-hydra-category-name-prefix)
           (:pretty-hydra-category-description pretty-hydra-category-description)
           (:pretty-hydra-category-name pretty-hydra-category-name)
           (:pretty-hydra-category-hydra-name pretty-hydra-category-hydra-name)
           (:pretty-hydra-category-hydra-caller-name pretty-hydra-category-hydra-caller-name)
           (:pretty-hydra-category-base-pretty-hydra-body pretty-hydra-category-base-pretty-hydra-body)
           (:pretty-hydra-category-cabinet-unit-names-list pretty-hydra-category-cabinet-unit-names-list)
           (:pretty-hydra-category-depth pretty-hydra-category-depth)
           (:pretty-hydra-category-width pretty-hydra-category-width)
           (:pretty-hydra-category-baron-name pretty-hydra-category-baron-name)
           (:pretty-hydra-category-previous-category-name pretty-hydra-category-previous-category-name)
           (:pretty-hydra-category-next-category-name pretty-hydra-category-next-category-name)))
      (unless (null (symbol-value (cadr el)))
        (plist-put (symbol-value pretty-hydra-category-name)
                   (car el) (symbol-value (cadr el))))))))

;; ******** category names get
;; This section makes the core defination for =pretty-hydra-category=
;; name convention.

;; We use a customizable name-prefix as the core
;; =pretty-hydra-category-name= component, and using automatically
;; name-generator to generate sets of subroutine names.


(defvar entropy/emacs-hydra-hollow-pretty-hydra-category-name-core
  "--hydra-category-"
  "The core indicator part for the =pretty-hydra-category-name=.")

(defvar entropy/emacs-hydra-hollow-pretty-hydra-category-name-format
  (format "%%s%s%%s"
          entropy/emacs-hydra-hollow-pretty-hydra-category-name-core))

(defvar entropy/emacs-hydra-hollow-pretty-hydra-category-name-regexp
  (format "\\(.+\\)%s\\([0-9]+\\)"
          entropy/emacs-hydra-hollow-pretty-hydra-category-name-core)
  "The regexp match for =pretty-hydra-category-name=.

Where 1st match group is the =pretty-hydra-category-name-prefix=
and the 2th was for the =pretty-hydra-category-depth=.")

(defvar entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
  (format "%%s%s%%s-caller"
          entropy/emacs-hydra-hollow-pretty-hydra-category-name-core))

(defvar entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-regexp
  (format "%s-caller"
          entropy/emacs-hydra-hollow-pretty-hydra-category-name-regexp)
  "The regexp match for the =pretty-hydra-category-hydra-name=.

match group convention was obeyed by
`entropy/emacs-hydra-hollow-pretty-hydra-category-name-regexp'.")

(defun entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
    (pretty-hydra-category-name-prefix &optional pretty-hydra-category-depth)
  "Get the =pretty-hydra-category-name= through
PRETTY-HYDRA-CATEGORY-NAME-PREFIX."
  (let ((pretty-hydra-category-depth (number-to-string (or pretty-hydra-category-depth 0))))
    (intern
     (format entropy/emacs-hydra-hollow-pretty-hydra-category-name-format
             pretty-hydra-category-name-prefix pretty-hydra-category-depth))))

(defun entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-hook-name
    (pretty-hydra-category-name-prefix &optional pretty-hydra-category-depth)
  "Get the =pretty-hydra-category-hook-name= through
PRETTY-HYDRA-CATEGORY-NAME-PREFIX."
  (let ((pretty-hydra-category-depth (number-to-string (or pretty-hydra-category-depth 0))))
    (intern
     (format "%s-hook"
             (format entropy/emacs-hydra-hollow-pretty-hydra-category-name-format
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)))))

(defun entropy/emacs-hydra-hollow-category-get-hydra-branch-name
    (pretty-hydra-category-name-prefix branch &optional pretty-hydra-category-depth)
  "Get rest subroutines name of spec =pretty-hydra-category=
through PRETTY-HYDRA-CATEGORY-PREFIX.

BRANCH is a spec subroutine type symbol for this function to find
thus for. Support branch type are:

- 'pretty-hydra-keymap' :: for get
  =pretty-hydra-category-hydra-kemap= host symbol name
- 'pretty-hydra-body' :: for get
  =pretty-hydra-category-hydra-body= host symbol name
- 'pretty-hydra-cabinet' :: for get
  =pretty-hydra-category-cabinet= host symbol name
- t :: for get =pretty-hydra-category-hydra-caller-name="
  (let ((pretty-hydra-category-depth (number-to-string (or pretty-hydra-category-depth 0))))
    (cond ((null branch)
           (intern
            (format entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
                    pretty-hydra-category-name-prefix pretty-hydra-category-depth)))
          ((eq branch 'pretty-hydra-keymap)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)
             "/keymap")))
          ((eq branch 'pretty-hydra-body)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)
             "/pretty-body")))
          ((eq branch 'pretty-hydra-cabinet)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)
             "/heads-plist")))
          (t
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-format
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)
             "/body"))))))

(defun entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-p
    (maybe-pretty-hydra-category-hydra-name &optional get-type)
  "Judge a symbol MAYBE-PRETTY-HYDRA-CATEGORY-HYDRA-NAME whether
belong to a =pretty-hydra-category-hydra-name= relate subroutine,
return the hosted =pretty-hydra-category-name=.

The optional argument =GET-TYPE=, gives the same type defination
by `entropy/emacs-hydra-hollow-category-get-hydra-branch-name''s
BRANCH argument type."
  (if (and
       (symbolp maybe-pretty-hydra-category-hydra-name)
       (string-match entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-regexp
                     (symbol-name maybe-pretty-hydra-category-hydra-name)))
      (let* ((name-str (symbol-name maybe-pretty-hydra-category-hydra-name))
             (pretty-hydra-category-name-prefix (intern (match-string 1 name-str)))
             (pretty-hydra-category-depth (string-to-number (match-string 2 name-str))))
        (cond
         ((and (not (null get-type))
               (not (eq get-type 'just-prefix)))
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           pretty-hydra-category-name-prefix get-type pretty-hydra-category-depth))
         ((eq get-type 'just-prefix)
          pretty-hydra-category-name-prefix)
         (t
          (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
           pretty-hydra-category-name-prefix pretty-hydra-category-depth))))
    nil))

;; ******** category width normalize

(defun entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator
    (pretty-hydra-category-width-indicator)
  "Normalizeds PRETTY-CATEGORY-WIDTH-INDICATOR for arranging the
cabinet width for a =pretty-hydra-category= or all chained of
thus.

Basiclly a =pretty-category-width-indicator= is a *width-sign* of
t, nil or an positive integer, further more for a *rich-list* a
list which car is one of thus and the rest was a plist whom have
the slot =:width-desc= (value of a string) or omitted.

Specially a =pretty-category-width-indicator= also can be a list
of it-self internally called *self-list* type.

=====================================
Introduction for a *width-sign* type:
-------------------------------------

when it's 't', this means that there's no limitation for a
=pretty-hdyra-category='s max width restriction, otherwise while
it's 'nil' says that all the categories chained by a
=pretty-hydra-category-name-prefix= whose max width restriction
are setted with
`entropy/emacs-hydra-hollow-category-default-width'. Lastly, when
It's a integer, the significant is as same as what when it's 'nil'
on, but for the specified restriction on the value it represented.

#+begin_quote
Specially, when the =width-sign= in a *rich-list* was 't', it will
be fallback to `entropy/emacs-hydra-hollow-category-default-width'
since that the no restriction was meaning less for a *rich-list*,
see the rest sections for more details.
#+end_quote

==================================
Introduction for *rich-list* type
----------------------------------

A *rich-list* type of a =pretty-hydra-category-width-indicator= is
one a data type for extending the =width-sign= with more
attributes, there's now just one attribute given out, the
':width-desc' the description of the current
=pretty-hydra-category= or all the chained nodes when it is out of
a =self-list= type.

===================================
Introduction for *self-list* type:
-----------------------------------

Each element of this list type is a *width-sign* or a *rich-list*
type data, and either of them within this list just servered for
the current =pretty-hydra-category=, thus when a *width-sign*
within this list is value of 't' will be fallback to
`entropy/emacs-hydra-hollow-category-default-width', in the other
hand, either 't' or 'nil' is for that.
"
  (let ((ctg-width-indc pretty-hydra-category-width-indicator)
        rtn width-sign width-desc
        (rich-func
         (lambda (x)
           (and (listp x)
                (or
                 (entropy/emacs-strict-plistp (cdr x))
                 (and (eq 1 (length x))
                      (not (listp (car x))))))))
        (eval-form-p (lambda (x) (ignore-errors (eq :eval (car x))))))
    (cond
     ((funcall rich-func ctg-width-indc)
      (let ((plist (cdr ctg-width-indc)))
        (setq width-sign (entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator
                          (entropy/emacs-hydra-hollow--common-judge-p (car ctg-width-indc)))
              width-desc (entropy/emacs-hydra-hollow--common-judge-p (plist-get plist :width-desc))
              width-sign (if (eq width-sign t) entropy/emacs-hydra-hollow-category-default-width
                           width-sign)
              rtn (list width-sign :width-desc width-desc))))
     ((funcall eval-form-p ctg-width-indc)
      (setq rtn (entropy/emacs-hydra-hollow--common-judge-p ctg-width-indc)))
     ((or (member ctg-width-indc '(t nil))
          (integerp ctg-width-indc))
      (setq rtn (or ctg-width-indc entropy/emacs-hydra-hollow-category-default-width)))
     ((listp ctg-width-indc)
      (dolist (el ctg-width-indc)
        (setq rtn
              (append
               rtn
               (list (entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator
                      el)))))
      (setq rtn
            (mapcar (lambda (x)
                      (if (eq x t) entropy/emacs-hydra-hollow-category-default-width
                        x))
                    rtn))))
    rtn))

;; ******** category navigation set

(defun entropy/emacs-hydra-hollow-category-concat-title-for-nav
    (title depth &rest nav)
  (let* ((title-str (substring-no-properties (eval title)))
         (up-hint (car nav))
         (down-hint (cadr nav))
         (fmstr "[%s]: %s page")
         (fmregx-up (rx (seq "[UP]: previous page")))
         (fmregx-next (rx (seq "[DOWN]: next page"))))
    (cond
     ((and (not (null up-hint))
           (not (string-match-p fmregx-up title-str))
           (null down-hint))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "UP" "previous")
                                  'face 'warning))))
     ((and (not (null down-hint))
           (not (string-match-p fmregx-next title-str))
           (null up-hint))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "DOWN" "next")
                                  'face 'warning))))
     ((and (not (null up-hint))
           (not (string-match-p fmregx-up title-str))
           (not (null down-hint))
           (not (string-match-p fmregx-next title-str)))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "UP" "previous")
                                  'face 'warning)
                     " "
                     ,(propertize (format fmstr "DOWN" "next")
                                  'face 'warning)))))
    (when (and depth
               (or up-hint down-hint))
      (let ((num-fmstr (rx line-start "[" (group (any "0-9")) "] ")))
        (unless (string-match-p num-fmstr title-str)
          (require 'faces)
          (setq title `(concat (propertize (format "[%s]" ,depth)
                                           'face 'show-paren-mismatch)
                               " " ,title)))))
    title))

(defun entropy/emacs-hydra-hollow-category-define-nav-key
    (keymap &rest category-nav-pretty-hydra-category-hydra-caller-name)
  (let ((up-caller (car category-nav-pretty-hydra-category-hydra-caller-name))
        (down-caller (cadr category-nav-pretty-hydra-category-hydra-caller-name)))
    (when (not (null up-caller))
      (define-key keymap (kbd "<up>") up-caller))
    (when (not (null down-caller))
      (define-key keymap (kbd "<down>") down-caller))))

;; ******** category baron set

(defun entropy/emacs-hydra-hollow-category-define-rate-key
    (keymap category-baron)
  (when (and (keymapp keymap)
             (functionp category-baron))
    (define-key keymap (kbd "<up>") category-baron)))

(defun entropy/emacs-hydra-hollow-category-patch-hire-title
    (pretty-hydra-body &optional hint-string)
  (let* ((new-pretty-hydra-body (copy-tree pretty-hydra-body))
         (title (plist-get pretty-hydra-body :title))
         (stick-regexp "» Hint <up> to \\[Previous Hydra\\]")
         (stick-p (string-match-p stick-regexp (eval title)))
         new-title)
    (unless stick-p
      (setq new-title
            `(concat
              ,title
              " "
              "» "
              (propertize "Hint <up> to [Previous Hydra]"
                          'face 'error)))
      (setq new-pretty-hydra-body
            (plist-put new-pretty-hydra-body :title new-title)))
    new-pretty-hydra-body))

;; ******** category recursive match

(defun entropy/emacs-hydra-hollow-category-recursive-match-group
    (pretty-hydra-category-name-prefix pretty-hydra-category-cabinet-unit-name)
  "Recursively match a category chain spec by
PRETTY-HYDRA-CATEGORY-NAME-PREFIX, for spec
PRETTY-HYDRA-CATEGORY-CABINET-UNIT-NAME.

Return a plist as the matching report who has two slot:

- :pretty-hydra-category :: the matched =pretty-hydra-category= or nil for not mached
- :pretty-hydra-category-depth :: the matched
  =pretty-hydra-category-depth= or when :pretty-hydra-category was nil for the
  tail new pre-create category depth.
"
  (let ((pretty-hydra-category-depth 0)
        rtn)
    (catch :matched
      (while (let ((var-name
                    (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
                     pretty-hydra-category-name-prefix pretty-hydra-category-depth)))
               (funcall
                `(lambda ()
                   (bound-and-true-p ,var-name))))
        (let* ((pretty-hydra-category-name
                (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
                 pretty-hydra-category-name-prefix pretty-hydra-category-depth))
               (category (symbol-value pretty-hydra-category-name))
               (pretty-hydra-category-cabinet-unit-names-list
                (plist-get category
                           :pretty-hydra-category-cabinet-unit-names-list)))
          (when (member pretty-hydra-category-cabinet-unit-name
                        pretty-hydra-category-cabinet-unit-names-list)
            (setq rtn (list :pretty-hydra-category category
                            :pretty-hydra-category-depth pretty-hydra-category-depth))
            (throw :matched nil))
          (setq pretty-hydra-category-depth (+ 1 pretty-hydra-category-depth)))))
    (unless (not (null rtn))
      (setq rtn (list :pretty-hydra-category nil
                      :pretty-hydra-category-depth pretty-hydra-category-depth)))
    rtn))

;; ******** category manipulation

(defmacro entropy/emacs-hydra-hollow-category-with-category
    (pretty-hydra-category-name-prefix pretty-hydra-category-depth &rest body)
  "Do the BODY with the specified =pretty-hydra-category= by
PRETTY-HYDRA-CATEGORY-NAME-PREFIX.

This function will side affect the spec =pretty-hydra-category=
when finish the BODY process. Thus for that, BODY form can calling
the internally subroutines of this macro, they are:

- $internally/pretty-hydra-category
- $internally/pretty-hydra-category-name
- $internally/pretty-hydra-category-name-prefix
- $internally/pretty-hydra-category-cabinet
- $internally/pretty-hydra-category-cabinet->new :: side-effective
- $internally/pretty-hydra-category-cabinet-name
- $internally/pretty-hydra-category-cabinet-unit-names-list
- $internally/pretty-hydra-category-cabinet-unit-names-list->new :: side-effective
- $internally/pretty-hydra-category-depth
- $internally/pretty-hydra-category-width
- $internally/pretty-hydra-category-width->new :: side-effective
- $internally/pretty-hydra-category-hydra-name
- $internally/pretty-hydra-category-hydra-kemap
- $internally/pretty-hydra-category-hydra-body
- $internally/pretty-hydra-category-hydra-body-name
- $internally/pretty-hydra-category-hydra-body->new :: side-effective
- $internally/pretty-hydra-category-base-pretty-hydra-body
- $internally/pretty-hydra-category-baron-name
- $internally/pretty-hydra-category-baron-name->new :: side-effective
- $internally/pretty-hydra-category-next-category-name
- $internally/pretty-hydra-category-next-category
- $internally/pretty-hydra-category-next-category-name->new :: side-effective
- $internally/pretty-hydra-category-previous-category-name
- $internally/pretty-hydra-category-previous-category
- $internally/pretty-hydra-category-previous-category-name->new :: side-effective"
  `(let* (
          ;; orig pattern
          ($internally/pretty-hydra-category-name-prefix ,pretty-hydra-category-name-prefix)
          ($internally/pretty-hydra-category-depth ,pretty-hydra-category-depth)
          ($internally/pretty-hydra-category-hydra-keymap-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/pretty-hydra-category-name-prefix
            'pretty-hydra-keymap $internally/pretty-hydra-category-depth))

          ($internally/pretty-hydra-category-name
           (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
            $internally/pretty-hydra-category-name-prefix $internally/pretty-hydra-category-depth))

          ($internally/pretty-hydra-category
           (symbol-value $internally/pretty-hydra-category-name))

          ($internally/pretty-hydra-category-width
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-width))

          ($internally/pretty-hydra-category-baron-name
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-baron-name))

          ($internally/pretty-hydra-category-hydra-name
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-hydra-name))

          ($internally/pretty-hydra-category-hydra-body-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/pretty-hydra-category-name-prefix 'pretty-hydra-body $internally/pretty-hydra-category-depth))

          ($internally/pretty-hydra-category-hydra-body
           (symbol-value
            $internally/pretty-hydra-category-hydra-body-name))

          ($internally/pretty-hydra-category-base-pretty-hydra-body
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-base-pretty-hydra-body))

          ($internally/pretty-hydra-category-next-category-name
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-next-category-name))
          ($internally/pretty-hydra-category-next-category
           (ignore-errors (symbol-value $internally/pretty-hydra-category-next-category-name)))

          ($internally/pretty-hydra-category-previous-category-name
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-previous-category-name))
          ($internally/pretty-hydra-category-previous-category
           (ignore-errors (symbol-value $internally/pretty-hydra-category-previous-category-name)))

          ($internally/pretty-hydra-category-cabinet-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/pretty-hydra-category-name-prefix
            'pretty-hydra-cabinet $internally/pretty-hydra-category-depth))

          ($internally/pretty-hydra-category-cabinet
           (symbol-value
            $internally/pretty-hydra-category-cabinet-name))

          ($internally/pretty-hydra-category-cabinet-unit-names-list
           (plist-get $internally/pretty-hydra-category :pretty-hydra-category-cabinet-unit-names-list))

          ;; new pattern

          ($internally/pretty-hydra-category-baron-name->new
           $internally/pretty-hydra-category-baron-name)

          ($internally/pretty-hydra-category-next-category-name->new
           $internally/pretty-hydra-category-next-category-name)

          ($internally/pretty-hydra-category-previous-category-name->new
           $internally/pretty-hydra-category-previous-category-name)

          $internally/pretty-hydra-category-cabinet-unit-names-list->new
          $internally/pretty-hydra-category-width->new

          ($internally/pretty-hydra-category-hydra-body->new
           (copy-tree $internally/pretty-hydra-category-hydra-body))
          ($internally/pretty-hydra-category-cabinet->new
           (copy-tree $internally/pretty-hydra-category-cabinet))

          )

     ;; run body
     (cl-labels
         (($internally/pretty-hydra-define
           (name body heads-plist)
           (cond
            ((eq name $internally/pretty-hydra-category-hydra-name)
             (setq $internally/pretty-hydra-category-hydra-body->new
                   body)
             (setq $internally/pretty-hydra-category-cabinet->new
                   heads-plist))
            (t
             (entropy/emacs-hydra-hollow-func-version-pthydra-define
              name body heads-plist))))
          ($internally/pretty-hydra-define+
           (name body heads-plist)
           (cond
            ((eq name $internally/pretty-hydra-category-hydra-name)
             (setq $internally/pretty-hydra-category-hydra-body->new
                   body)
             (setq $internally/pretty-hydra-category-cabinet->new
                   (pretty-hydra--merge-heads
                    $internally/pretty-hydra-category-cabinet->new
                    heads-plist)))
            (t
             (entropy/emacs-hydra-hollow-func-version-pthydra-define+
              name body heads-plist)))))
       (progn
         ,@body))

     ;; redefine category pretty-hydra-body for patching with baron
     (when (not (null $internally/pretty-hydra-category-baron-name->new))
       (setq $internally/pretty-hydra-category-hydra-body->new
             (entropy/emacs-hydra-hollow-category-patch-hire-title
              $internally/pretty-hydra-category-hydra-body->new)))

     ;; redefine category pretty-hydra
     (funcall
      (list 'lambda nil
            (list 'pretty-hydra-define $internally/pretty-hydra-category-hydra-name
                  $internally/pretty-hydra-category-hydra-body->new
                  $internally/pretty-hydra-category-cabinet->new)))

     ;; reset cagegory
     (setq $internally/pretty-hydra-category-cabinet-unit-names-list->new
           (cl-loop for item in (symbol-value $internally/pretty-hydra-category-cabinet-name)
                    when (stringp item)
                    collect item))

     (entropy/emacs-hydra-hollow-set-pretty-hydra-category-name
      $internally/pretty-hydra-category-name
      :pretty-hydra-category-cabinet-unit-names-list $internally/pretty-hydra-category-cabinet-unit-names-list->new
      :pretty-hydra-category-width
      (or $internally/pretty-hydra-category-width->new
          (if (> (length $internally/pretty-hydra-category-cabinet-unit-names-list->new)
                 $internally/pretty-hydra-category-width)
              (length $internally/pretty-hydra-category-cabinet-unit-names-list->new)
            $internally/pretty-hydra-category-width))
      :pretty-hydra-category-baron-name $internally/pretty-hydra-category-baron-name->new
      :pretty-hydra-category-next-category-name $internally/pretty-hydra-category-next-category-name->new
      :pretty-hydra-category-previous-category-name $internally/pretty-hydra-category-previous-category-name->new)

     ;; remap category navigation key-binds
     (entropy/emacs-hydra-hollow-category-define-nav-key
      (symbol-value $internally/pretty-hydra-category-hydra-keymap-name)
      (plist-get
       (ignore-errors (symbol-value $internally/pretty-hydra-category-previous-category-name->new))
       :pretty-hydra-category-hydra-caller-name)
      (plist-get
       (ignore-errors (symbol-value $internally/pretty-hydra-category-next-category-name->new))
       :pretty-hydra-category-hydra-caller-name))

     ;; remap category baron key-binds
     (when $internally/pretty-hydra-category-baron-name->new
       (setq entropy/emacs-hydra-hollow-union-form
             (append
              entropy/emacs-hydra-hollow-union-form
              `((entropy/emacs-hydra-hollow-category-define-rate-key
                 (symbol-value ',$internally/pretty-hydra-category-hydra-keymap-name)
                 ',$internally/pretty-hydra-category-baron-name->new)))))
     ))


(defun entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
    (pretty-hydra-category-baron-name pretty-hydra-cabinet)
  (let ((pretty-hydra-caskets-list
         (entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list
          pretty-hydra-cabinet)))
    (dolist (head pretty-hydra-caskets-list)
      (let ((command (cadr (caadr head))))
        (when (entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-p
               command)
          (let* ((it-ctg-name-prefix
                  (entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-p
                   command 'just-prefix)))
            (setq entropy/emacs-hydra-hollow-union-form
                  (append
                   entropy/emacs-hydra-hollow-union-form
                   `((entropy/emacs-hydra-hollow-category-with-category
                      ',it-ctg-name-prefix nil
                      (setq $internally/pretty-hydra-category-baron-name->new
                            ',pretty-hydra-category-baron-name)))))))))))

;; ******** category interactive advice

(defun entropy/emacs-hydra-hollow-category-advice-for-ctg-choice
    (pretty-hydra-category-name-prefix &optional pretty-hydra-category-depth)
  (let* ((orig-caller (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                       pretty-hydra-category-name-prefix t pretty-hydra-category-depth))
         (name (intern (format "%s--|ctg-choice-around-adv"
                               orig-caller))))
    (defalias name
      `(lambda (orig-func &rest orig-args)
         (let ((init-depth (or ,pretty-hydra-category-depth 0))
               (depth (or ,pretty-hydra-category-depth 0))
               multi-p)
           (while (fboundp (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                            ',pretty-hydra-category-name-prefix t depth))
             (cl-incf depth))
           (setq depth (- depth 1))
           (setq multi-p (not (= depth init-depth)))
           (if (or (not multi-p)
                   (string-match-p entropy/emacs-hydra-hollow-pretty-hydra-category-name-regexp
                                   (symbol-name last-command))
                   (null current-prefix-arg))
               (progn
                 (setq terminated t)
                 (apply orig-func orig-args))
             (let ((cnt init-depth)
                   candis choice caller-name)
               (while (<= cnt depth)
                 (setq candis
                       (append candis
                               (list
                                (cons cnt
                                      (or
                                       (plist-get
                                        (symbol-value
                                         (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
                                          ',pretty-hydra-category-name-prefix cnt))
                                        :pretty-hydra-category-description)
                                       "None description found for this depth category")))))
                 (cl-incf cnt))

               (setq candis
                     (mapcar
                      (lambda (x)
                        (let* ((depth (car x))
                               (desc (cdr x))
                               (candi (format "%s: %s" depth desc)))
                          (cons candi depth)))
                      candis))

               (setq choice (alist-get (completing-read "Choose category slot: " candis nil t)
                                       candis nil nil 'string=)
                     caller-name
                     (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                      ',pretty-hydra-category-name-prefix t choice))
               (setq current-prefix-arg nil) ;prevent re-choose candis for thus category
               (apply (if (= choice init-depth) orig-func caller-name)
                      orig-args))))))
    (advice-add orig-caller :around name)))

;; ******* define hydra hollow category

(defun entropy/emacs-hydra-hollow-category-frame-work-define
    (pretty-hydra-category-name-prefix
     pretty-hydra-body pretty-hydra-cabinet
     &optional
     pretty-hydra-category-depth
     pretty-hydra-category-previous-category-name
     pretty-hydra-category-width-indicator)
  "=pretty-hydra-category= defination function.

Like ~pretty-hydra-define~ but used as a function and richly
featured for =pretty-hydra-category=.

Optional argument PRETTY-HYDRA-CATEGORY-WIDTH-INDICATOR is a
=pretty-hydra-category-width-indicator= type used for
`entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator'.
"
  (let* ((ctgs (entropy/emacs-hydra-hollow-partion-pretty-hydra-cabinet
                pretty-hydra-cabinet))
         (ctg-len (length ctgs))
         (ctg-indc (entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator
                    pretty-hydra-category-width-indicator))
         (body-patch (copy-tree pretty-hydra-body))
         (pretty-hydra-category-depth (or pretty-hydra-category-depth 0))
         cur-ctg-indc
         rest-ctg-inc
         cur-head-group
         rest-head-group
         pretty-hydra-category-hydra-name
         pretty-hydra-category-hydra-caller-name
         pretty-hydra-category-name
         pretty-hydra-category-description
         pretty-hydra-category-next-category-name
         cur-hydra-keymap-name
         (cur-ctg-hook-name
          (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-hook-name
           pretty-hydra-category-name-prefix
           pretty-hydra-category-depth)))

    ;; get category map
    (cond ((eq t ctg-indc)
           (setq cur-ctg-indc t
                 rest-ctg-inc t))
          ((null ctg-indc)
           (setq cur-ctg-indc
                 entropy/emacs-hydra-hollow-category-default-width
                 rest-ctg-inc
                 entropy/emacs-hydra-hollow-category-default-width))
          ((integerp ctg-indc)
           (setq cur-ctg-indc ctg-indc
                 rest-ctg-inc ctg-indc))
          ((and (listp ctg-indc)
                (entropy/emacs-strict-plistp (cdr ctg-indc)))
           (setq cur-ctg-indc (car ctg-indc)
                 pretty-hydra-category-description (plist-get (cdr ctg-indc) :width-desc)
                 rest-ctg-inc cur-ctg-indc))
          (t
           (setq cur-ctg-indc (or (ignore-errors (caar ctg-indc))
                                  (car ctg-indc))
                 pretty-hydra-category-description (ignore-errors (plist-get (cdar ctg-indc) :width-desc))
                 rest-ctg-inc (cdr ctg-indc))))

    ;; immediately throw out error of the wrong type *width-sign*
    (unless (or (member cur-ctg-indc '(t nil))
                (and (integerp cur-ctg-indc)
                     (> cur-ctg-indc 0)))
      (error "wrong type of argument: *width-sign*-p %s" cur-ctg-indc))

    ;; get manipulated heads
    (cond
     ((eq cur-ctg-indc t)
      (setq rest-head-group nil)
      (cl-loop for item in ctgs
               do (setq cur-head-group
                        (append cur-head-group item))))
     ((or (null cur-ctg-indc)
          (integerp cur-ctg-indc))
      (let ((cnt 0))
        (while (<= (+ cnt 1)
                   (or cur-ctg-indc
                       entropy/emacs-hydra-hollow-category-default-width))
          (setq cur-head-group
                (append cur-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt))
        (while (not (null (nth cnt ctgs)))
          (setq rest-head-group
                (append rest-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt)))))

    ;; patch pretty-hydra-body
    (let* ((title (plist-get body-patch :title))
           (new-title (copy-tree title)))

      (setq new-title
            (entropy/emacs-hydra-hollow-category-concat-title-for-nav
             new-title pretty-hydra-category-depth
             pretty-hydra-category-previous-category-name rest-head-group))

      (setq body-patch
            (plist-put
             body-patch
             :title
             new-title)))

    ;; generate category name
    (setq pretty-hydra-category-name
          (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
           pretty-hydra-category-name-prefix pretty-hydra-category-depth))

    ;; generate caller name
    (setq pretty-hydra-category-hydra-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           pretty-hydra-category-name-prefix nil pretty-hydra-category-depth))

    (setq pretty-hydra-category-hydra-caller-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           pretty-hydra-category-name-prefix t pretty-hydra-category-depth))

    ;; generate next pretty-hydra-category-name
    (unless (null rest-head-group)
      (setq pretty-hydra-category-next-category-name
            (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
             pretty-hydra-category-name-prefix (+ pretty-hydra-category-depth 1))))

    ;; generate hydra-keymap name
    (setq cur-hydra-keymap-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           pretty-hydra-category-name-prefix 'pretty-hydra-keymap pretty-hydra-category-depth))

    ;; define pretty hydra
    (funcall
     `(lambda ()
        (pretty-hydra-define
          ,pretty-hydra-category-hydra-name
          ,body-patch
          ,cur-head-group)))

    ;; bind rate key and patch rate title
    (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
     pretty-hydra-category-hydra-caller-name cur-head-group)

    ;; advice for multi-choice interaction
    (when (eq pretty-hydra-category-depth 0)
      (entropy/emacs-hydra-hollow-category-advice-for-ctg-choice
       pretty-hydra-category-name-prefix))

    ;; advice for calling union form
    (when (eq pretty-hydra-category-depth 0)
      (entropy/emacs-hydra-hollow-advice-for-call-union-form
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        pretty-hydra-category-name-prefix t)))

    ;; define current category nav keys
    (entropy/emacs-hydra-hollow-category-define-nav-key
     (symbol-value cur-hydra-keymap-name)
     (when pretty-hydra-category-previous-category-name
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        pretty-hydra-category-name-prefix t (- pretty-hydra-category-depth 1)))
     (when (not (null rest-head-group))
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        pretty-hydra-category-name-prefix t (+ pretty-hydra-category-depth 1))))

    ;; recursive define category hydra
    (when (not (null rest-head-group))
      (entropy/emacs-hydra-hollow-category-frame-work-define
       pretty-hydra-category-name-prefix pretty-hydra-body rest-head-group
       (+ pretty-hydra-category-depth 1) pretty-hydra-category-name rest-ctg-inc))

    ;; set this object
    (entropy/emacs-hydra-hollow-set-pretty-hydra-category-name
     pretty-hydra-category-name
     :pretty-hydra-category-name-prefix pretty-hydra-category-name-prefix
     :pretty-hydra-category-description pretty-hydra-category-description
     :pretty-hydra-category-hydra-name pretty-hydra-category-hydra-name
     :pretty-hydra-category-hydra-caller-name pretty-hydra-category-hydra-caller-name
     :pretty-hydra-category-base-pretty-hydra-body pretty-hydra-body
     :pretty-hydra-category-cabinet-unit-names-list (cl-loop for item in cur-head-group
                                                             when (not (listp item))
                                                             collect item)
     :pretty-hydra-category-depth pretty-hydra-category-depth
     :pretty-hydra-category-width cur-ctg-indc
     :pretty-hydra-category-previous-category-name pretty-hydra-category-previous-category-name
     :pretty-hydra-category-next-category-name pretty-hydra-category-next-category-name)

    ;; call its hook
    (when (not (null (ignore-errors (symbol-value cur-ctg-hook-name))))
      (run-hooks cur-ctg-hook-name))
    ))

(defun entropy/emacs-hydra-hollow-category-frame-work-define+
    (pretty-hydra-category-name-prefix pretty-hydra-body pretty-hydra-cabinet
                          &optional
                          pretty-hydra-category-width-indicator-for-inject)
  "Add hydras into a spec =pretty-hydra-category= by
PRETTY-HYDRA-CATEGORY-NAME-PREFIX.

Like ~pretty-hydra-define+~ but be with function usage and richly
for =pretty-hydra-category=.

Optional argument PRETTY-HYDRA-CATEGORY-WIDTH-INDICATOR is type
of =pretty-hydra-category-width-indicator= which used for
`entropy/emacs-hydra-hollow-normalize-pretty-hydra-category-width-indicator'."
  (let* ((top-pretty-hydra-category-name
          (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
           pretty-hydra-category-name-prefix))
         (top-category-exists-p
          (ignore-errors (not (null (symbol-value top-pretty-hydra-category-name)))))
         (ctgs
          (entropy/emacs-hydra-hollow-partion-pretty-hydra-cabinet
           pretty-hydra-cabinet)))
    (unless top-category-exists-p
      (let ((ctg-hook-name
             (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-hook-name
              pretty-hydra-category-name-prefix)))
        (unless (boundp ctg-hook-name)
          (set ctg-hook-name nil))
        (set ctg-hook-name
             (append (symbol-value ctg-hook-name)
                     `((lambda ()
                         (entropy/emacs-hydra-hollow-category-frame-work-define+
                          ',pretty-hydra-category-name-prefix ',pretty-hydra-body ',pretty-hydra-cabinet
                          ',pretty-hydra-category-width-indicator-for-inject)))))))
    (when top-category-exists-p
      (dolist (part-ctg ctgs)
        (let* ((group (car part-ctg))
               (ctg-match
                (entropy/emacs-hydra-hollow-category-recursive-match-group
                 pretty-hydra-category-name-prefix group))
               (ctg-matched-p (not
                               (null
                                (plist-get ctg-match :pretty-hydra-category)))))
          (cond
           (ctg-matched-p
            (let* ((matched-ctg
                    (plist-get ctg-match :pretty-hydra-category))
                   (matched-pretty-hydra-category-depth
                    (plist-get matched-ctg :pretty-hydra-category-depth))
                   (matched-ctg-caller
                    (plist-get matched-ctg :pretty-hydra-category-hydra-caller-name)))
              (entropy/emacs-hydra-hollow-category-with-category
               pretty-hydra-category-name-prefix matched-pretty-hydra-category-depth
               ($internally/pretty-hydra-define+
                $internally/pretty-hydra-category-hydra-name
                $internally/pretty-hydra-category-hydra-body
                part-ctg))
              (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
               matched-ctg-caller
               part-ctg)))
           (t
            (let* ((pretty-hydra-category-depth (plist-get ctg-match :pretty-hydra-category-depth))
                   (tail-pretty-hydra-category-depth (- pretty-hydra-category-depth 1))
                   (tail-pretty-hydra-category-name
                    (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
                     pretty-hydra-category-name-prefix tail-pretty-hydra-category-depth))

                   (tail-category (symbol-value tail-pretty-hydra-category-name))
                   (tail-pretty-hydra-category-hydra-caller-name (plist-get tail-category :pretty-hydra-category-hydra-caller-name))
                   (tail-category-ctg-width (plist-get tail-category :pretty-hydra-category-width))
                   (tail-pretty-hydra-category-cabinet-unit-names-list (copy-tree (plist-get tail-category :pretty-hydra-category-cabinet-unit-names-list)))

                   (tail-category-overflow-p
                    (>= (length tail-pretty-hydra-category-cabinet-unit-names-list)
                        tail-category-ctg-width)))
              (cond
               ((null tail-category-overflow-p)
                (entropy/emacs-hydra-hollow-category-with-category
                 pretty-hydra-category-name-prefix tail-pretty-hydra-category-depth
                 ($internally/pretty-hydra-define+
                  $internally/pretty-hydra-category-hydra-name
                  $internally/pretty-hydra-category-hydra-body
                  part-ctg))
                (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
                 tail-pretty-hydra-category-hydra-caller-name
                 part-ctg))

               (tail-category-overflow-p
                (let* ((new-ctg-name
                        (entropy/emacs-hydra-hollow-category-get-pretty-hydra-category-name
                         pretty-hydra-category-name-prefix pretty-hydra-category-depth)))

                  (entropy/emacs-hydra-hollow-category-with-category
                   pretty-hydra-category-name-prefix tail-pretty-hydra-category-depth
                   (let ((tail-category-used-pretty-hydra-body-title
                          (plist-get $internally/pretty-hydra-category-hydra-body->new
                                     :title)))
                     ;; add new tail category
                     (entropy/emacs-hydra-hollow-category-frame-work-define
                      pretty-hydra-category-name-prefix
                      $internally/pretty-hydra-category-hydra-body
                      part-ctg
                      (+ 1 $internally/pretty-hydra-category-depth)
                      $internally/pretty-hydra-category-name
                      pretty-hydra-category-width-indicator-for-inject)

                     ;; pop out current pretty-hydra-category-width-indicator-for-inject
                     (when (and (not (null pretty-hydra-category-width-indicator-for-inject))
                                (listp pretty-hydra-category-width-indicator-for-inject))
                       (pop pretty-hydra-category-width-indicator-for-inject))

                     (setq $internally/pretty-hydra-category-hydra-body->new
                           (plist-put
                            $internally/pretty-hydra-category-hydra-body->new
                            :title
                            (entropy/emacs-hydra-hollow-category-concat-title-for-nav
                             tail-category-used-pretty-hydra-body-title
                             $internally/pretty-hydra-category-depth
                             nil t))
                           $internally/pretty-hydra-category-next-category-name->new
                           new-ctg-name))))))))))))))


;; ****** major-mode pretty hydra core
;; This section gives the method to directed define a entropy-emacs
;; superstructure pretty hydra for a major-mode or even for a
;; arbitray mode but not recommended for the context term restriction.


(defun entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
    (mode)
  (let* ((mode-str (symbol-name mode))
         (mode-ctg-name-prefix
          (intern
           (format "eemacs-hydra-for-mode-%s"
                   mode-str))))
    mode-ctg-name-prefix))

(defun entropy/emacs-hydra-hollow-category-get-major-mode-caller (mode)
  (let (rtn)
    (setq rtn
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
            mode)
           t))
    rtn))

(defun entropy/emacs-hydra-hollow-category-major-mode-define
    (mode pretty-hydra-body pretty-hydra-cabinet
          &optional pretty-hydra-category-width-indicator)
  "Like `major-mode-hydra-define' but use
`entropy/emacs-hydra-hollow-category-frame-work-define' as
backend instead of `pretty-hydra-define'."
  (let ((ctg-name-prefix
         (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
          mode)))
    (entropy/emacs-hydra-hollow-category-frame-work-define
     ctg-name-prefix pretty-hydra-body pretty-hydra-cabinet
     nil nil pretty-hydra-category-width-indicator)))

(defun entropy/emacs-hydra-hollow-category-major-mode-define+
    (mode pretty-hydra-body pretty-hydra-cabinet
          &optional
          pretty-hydra-category-width-indicator-for-inject)
    "Like `major-mode-hydra-define+' but use
`entropy/emacs-hydra-hollow-category-frame-work-define+' as
backend instead of `pretty-hydra-define+'."
  (let ((ctg-name-prefix
         (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
          mode)))
    (entropy/emacs-hydra-hollow-category-frame-work-define+
     ctg-name-prefix pretty-hydra-body pretty-hydra-cabinet
     pretty-hydra-category-width-indicator-for-inject)))


(defun entropy/emacs-hydra-hollow-category-major-mode-hydra ()
  "Summon the hydra for given MODE (if there is one), lile
`major-mode-hydra'."
  (interactive)
  (let ((orig-mode major-mode)
        (rec-mode major-mode))
    (catch 'done
      (while rec-mode
        (let ((hydra (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                      (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
                       rec-mode)
                      t)))
          (when (fboundp hydra)
            (call-interactively hydra)
            (throw 'done t)))
        (setq rec-mode (get rec-mode 'derived-mode-parent)))
      (user-error "Major mode hydra not found for %s or its parent modes" orig-mode))))

(entropy/emacs-!set-key
  (kbd "m")
  #'entropy/emacs-hydra-hollow-category-major-mode-hydra)

;; ****** individual pretty hydra core

;; This section gives the method to define individual entropy-emacs
;; superstructure pretty hyra, with follow data type defined:

;; - =individual-hydra-name=:

;;   A symbol as =pretty-hydra-category-name-prefix= but used for
;;   this individual hydra wrapper, and expanded to thus as.


(defun entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
    (individual-hydra-name)
  (let* ((fmstr "entropy/emacs-individual-hydra--%s")
         (pretty-hydra-category-name-prefix
          (intern
           (format fmstr
                   (symbol-name individual-hydra-name)))))
    pretty-hydra-category-name-prefix))

(defun entropy/emacs-hydra-hollow-category-common-individual-get-caller
    (individual-hydra-name)
  (let ((name-prefix
         (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
          individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
     name-prefix t)))


(defun entropy/emacs-hydra-hollow-category-common-individual-define
    (individual-hydra-name pretty-hydra-body pretty-hydra-cabinet
                           &optional pretty-hydra-category-width-indicator)
  (let* ((name-prefix
          (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
           individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-frame-work-define
     name-prefix pretty-hydra-body pretty-hydra-cabinet
     nil nil
     pretty-hydra-category-width-indicator)))

(defun entropy/emacs-hydra-hollow-category-common-individual-define+
    (individual-hydra-name pretty-hydra-body pretty-hydra-cabinet
                           &optional
                           pretty-hydra-category-width-indicator-for-inject)
  (let* ((name-prefix
          (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
           individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-frame-work-define+
     name-prefix pretty-hydra-body pretty-hydra-cabinet
     pretty-hydra-category-width-indicator-for-inject)))

(defun entropy/emacs-hydra-hollow-category-common-individual-make-title-common
    (individual-hydra-name)
  (let ((title-str
         (format "%s Actions" (capitalize (symbol-name individual-hydra-name)))))
    `(:title
      (entropy/emacs-pretty-hydra-make-title
       ,title-str
       "faicon" "certificate")
      :color ambranth
      :quit-key "q"
      :separator "-")))

;; ***** base /pretty-hydra-cabinet/ normalization defination

(defun entropy/emacs-hydra-hollow-normalize-pretty-hydra-caskets-list
    (pretty-hydra-caskets-list)
  "Generized each =pretty-hydra-casket= of PRETTY-HYDRA-CASKETS-LIST.

The generization aiming for use
`entropy/emacs-hydra-hollow--common-judge-p' to expand the slot
of =pretty-hydra-casket= for group-name, key-stroke and its
notation, especially for the command slot that will preserve its
symbol type or form type when its a symbol or a native form,
otherwise as other purpose for.
"
  (let (rtn)
    (dolist (sp-head pretty-hydra-caskets-list)
      (let* ((group (car sp-head))
             (new-group (entropy/emacs-hydra-hollow--common-judge-p
                         group))
             (pattern (caadr sp-head))
             (ptt-plist (cdddr pattern))
             (key (car pattern))
             (new-key (entropy/emacs-hydra-hollow--common-judge-p
                       key))
             (command (cadr pattern))
             (new-command (or (ignore-errors
                                (when (eq :eval (car command))
                                  (entropy/emacs-hydra-hollow--common-judge-p
                                   command)))
                              command))
             (notation (caddr pattern))
             (new-notation (entropy/emacs-hydra-hollow--common-judge-p
                            notation))
             )

        ;; Notice Here: inject ':exit' key and non-nil value
        ;; to the head prevent recursive hydra map calling
        ;; bug.
        (when (and
               (symbolp new-command)
               (entropy/emacs-hydra-hollow-pretty-hydra-category-hydra-name-p
                new-command))
          (unless (plist-get ptt-plist :exit)
            (setq ptt-plist
                  (plist-put ptt-plist :exit t))))

        (setq rtn
              (append
               rtn
               (if (null new-key)
                   `((,new-group nil))
                 `((,new-group ((,new-key ,new-command ,new-notation ,@ptt-plist)))))))))
    rtn))

(defun entropy/emacs-hydra-hollow-normalize-pretty-hydra-cabinet
    (pretty-hydra-cabinet &optional no-merge)
  "Normalizes PRETTY-HYDRA-CABINET.

It return a new normalized =pretty-hydra-cabinet= or for a
=pretty-hydra-caskets-list= when optional arg NO-MERGE non-nil,
further more return nil when normalized of a 'content-empty'
PRETTY-HYDRA-CABINET.

Normalizing procedure definition:

For each =pretty-hydra-head='s :enable slot's pattern's result
evaluated by `entropy/emacs-hydra-hollow--common-judge-p' was
non-nil, or the pure empty =pretty-hydra-cabinet-unit=, gathering
them into a =pretty-hydra-caskets-list= and transfers it into
`entropy/emacs-hydra-hollow-normalize-pretty-hydra-caskets-list'
for normalizing.

Thus a 'content-empty' =pretty-hydra-cabinet= was that doesn't has
enabled =pretty-hydra-head= or any pure empty
=pretty-hydra-cabinet-unit=.
 "
  (let ((pretty-hydra-caskets-list
         (entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list
          pretty-hydra-cabinet))
        rtn)
    (dolist (sp-head pretty-hydra-caskets-list)
      (let* ((group (car sp-head))
             (head-pattern (cdddr (caadr sp-head)))
             (enable (let ((enable-slot (plist-get head-pattern :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot))))
        (if enable
            (setq rtn (append rtn `((,group ,(cadr sp-head)))))
          (when (null (cadr sp-head))
            (setq rtn (append rtn (list sp-head)))))))
    (when (not (null rtn))
      (setq rtn
            (entropy/emacs-hydra-hollow-normalize-pretty-hydra-caskets-list
             rtn))
      (unless no-merge
        (setq rtn
              (entropy/emacs-hydra-hollow-merge-pretty-hydra-caskets-list
               rtn))))
    rtn))

(defmacro entropy/emacs-hydra-hollow-with-normalized-pretty-hydra-caskets-list
    (pretty-hydra-cabinet &rest body)
  "With all normalized =pretty-hydra-head= in PRETTY-HYDRA-CABINET do
the BODY.

Its a macro with one built-in available manipulative variable
=$internally/normalized-pretty-hydra-caskets-list= which was one
=pretty-hydra-caskets-list= which each =pretty-hydra-casket= are
normalized.

The normalizing procedure provided by
`entropy/emacs-hydra-hollow-normalize-pretty-hydra-cabinet'."
  `(let (($internally/normalized-pretty-hydra-caskets-list
          (entropy/emacs-hydra-hollow-normalize-pretty-hydra-cabinet
           ,pretty-hydra-cabinet t)))
     ,@body))

;; ***** heads predicate

;; This sectioin defined the predicate handle for
;; =pretty-hydra-head=, aiming for handling more entropy-emacs
;; specified head key slot.

;; ****** library
;; ******* extra define key
(defun entropy/emacs-hydra-hollow-define-key (keymap key form)
  "Bind KEY to KEYMAP with FORM.

If FORM is not a symbol then define a function with unique random
name suffix by request from
`entropy/emacs-hydra-hollow-random-func-name-number-register'.

KEYMAP was a keymap, a keymap symbol or for some meaningful usage:

- KEYMAP `eq' 'global-map', binding KEY to global map.
- KEYMAP `eq' 'eemacs-top-map, binding KEY to
  `entropy/emacs-top-keymap'."
  (let* (command
         (func-reg-num
          (or
           (ignore-errors
            (+
             (car
              entropy/emacs-hydra-hollow-random-func-name-number-register)
             1))
           0)))
    ;; get new random func name suffix
    (while (member func-reg-num
                   entropy/emacs-hydra-hollow-random-func-name-number-register)
      (cl-incf func-reg-num))

    ;; normalize command
    (cond
     ((symbolp form)
      (setq command form))
     ((listp form)
      (let ((func-name (intern
                        (format "eemacs-hydra-hollow-random-func--/number-%s"
                                (number-to-string func-reg-num)))))
        (when (fboundp func-name)
          (error "Function '%s' has been existed, random function creating fatal!"
                 (symbol-name func-name)))
        (push func-reg-num
              entropy/emacs-hydra-hollow-random-func-name-number-register)
        (eval
         `(defun ,func-name ()
            (interactive)
            ,form))
        (setq command func-name))))

    ;; inject command to map
    (pcase keymap
      ('global-map
       (global-set-key (kbd key) command))
      ('eemacs-top-map
       (entropy/emacs-!set-key
         (kbd key) command))
      (_
       (if (listp keymap)
           (define-key keymap (kbd key) command)
         (define-key (symbol-value keymap)
           (kbd key) command))))
    ))

;; ******* hydra-injector

(defun entropy/emacs-hydra-hollow-hydra-injector-valid-p (hydra-injector)
  "Predicate function judger for HYDRA-INJECATOR of data type
=hydra-injector=.

A =hydra-injector= is a data type for host a recorde for a
external map key-binding, it can be a self-list (see
`entropy/emacs-hydra-hollow-self-list-type-p') data type.

Basiclly, it is a list of three parts:
1) *car* of a feature information served by
   `entropy/emacs-lazy-load-simple'.
2) *cadr* of a keymap information, it can be a keymap symbol or its
   value.
3) The rest is a plist which can host slot ':inject-key' and
   ':inject-command'.

   * ':inject-key': is a key-sequence used for binding commad of
     ':inject-command' into the keymap.

   * ':inject-command': is a function or a form which will be
     wrapped into a random function.
"
  (let ((judger
         (lambda (x)
           (unless (and (listp x)
                        (not (null x)))
             (error "Wrong type of argument: hydra-injector-P '%s'" x))
           (let ((feature (car x))
                 (map (cadr x)))
             (unless (or (and (symbolp feature)
                              (not (null feature)))
                         (and (listp feature)
                              (not (null feature))
                              (not (member nil feature))))
               (error "Wrong type of argument: hydra-injector-feature-P '%s'"
                      feature))
             (unless (not (null map))
               (error "Wrong type of argument: hydra-injector-map-P '%s'" map))
             t))))
    (cond
     ((entropy/emacs-hydra-hollow-self-list-type-p hydra-injector)
      (let ((value (cdr hydra-injector)))
        (dolist (var value)
          (funcall judger var))
        :self-list))
     (t
      (funcall judger hydra-injector)))))

(defun entropy/emacs-hydra-hollow-build-hydra-injector (hydra-injector key command)
  "Build a =hydra-injector= (see
`entropy/emacs-hydra-hollow-hydra-injector-valid-p' for details)
HYDRA-INJECTOR with key KEY and command COMMAND.

Both KEY and COMMAND can be nil expect that the ':inject-key' and
':inject-command' slot in the HYDRA-INJECTOR are non-nil.
"
  (let ()
    (cond
     ((eq (entropy/emacs-hydra-hollow-hydra-injector-valid-p hydra-injector)
          :self-list)
      (dolist (pair (cdr hydra-injector))
        (eval
         `(entropy/emacs-lazy-load-simple ,(car pair)
            :no-message t
            (entropy/emacs-hydra-hollow-define-key
             ',(cadr pair)
             ,(or (plist-get (cddr pair) :inject-key) key)
             #',command)))))
     ((entropy/emacs-hydra-hollow-hydra-injector-valid-p hydra-injector)
      (eval
       `(entropy/emacs-lazy-load-simple ,(car hydra-injector)
          :no-message t
          (entropy/emacs-hydra-hollow-define-key
           ',(cadr hydra-injector)
           ,(or (plist-get (cddr hydra-injector) :inject-key) key)
           #',command)))))))

(defun entropy/emacs-hydra-hollow-make-hydra-injector
    (hydra-injector-maybe feature-replace map-replace)
  "Make the data HYDRA-INJECTOR-MAYBE which maybe a
=hydra-injector= be the equality one with feature
FEATURE-REPLACCE and keymap data MAP-REPLACE."
  (let (rtn)
    (cond
     ((entropy/emacs-hydra-hollow-self-list-type-p hydra-injector-maybe)
      (dolist (pair (cdr hydra-injector-maybe))
        (cond ((and (null (car pair))
                    (not (null (cadr pair))))
               (setq rtn (append rtn (list `(,feature-replace ,(cadr pair) . ,(cddr pair))))))
              ((and (null (cadr pair))
                    (not (null (car pair))))
               (setq rtn (append rtn (list `(,(car pair) ,map-replace . ,(cddr pair))))))
              ((and (null (cadr pair))
                    (null (car pair)))
               (setq rtn (append rtn (list `(,feature-replace ,map-replace . ,(cddr pair))))))
              (t (setq rtn (append rtn (list pair)))))))
     ((listp hydra-injector-maybe)
      (cond ((and (null (car hydra-injector-maybe))
                  (not (null (cadr hydra-injector-maybe))))
             (setq rtn (append rtn (list `(,feature-replace ,(cadr hydra-injector-maybe) . ,(cddr hydra-injector-maybe))))))
            ((and (null (cadr hydra-injector-maybe))
                  (not (null (car hydra-injector-maybe))))
             (setq rtn (append rtn (list `(,(car hydra-injector-maybe) ,map-replace . ,(cddr hydra-injector-maybe))))))
            ((and (null (cadr hydra-injector-maybe))
                  (null (car hydra-injector-maybe)))
             (setq rtn (append rtn (list `(,feature-replace ,map-replace . ,(cddr hydra-injector-maybe))))))
            (t (setq rtn (append rtn (list hydra-injector-maybe))))))
     (t
      (error "Wrong type of argument: hydra-injector-p '%s'" hydra-injector-maybe)))
    (add-to-list 'rtn :self-list)
    rtn))

;; ****** predicate defination

;; *Data type*

;; - =pretty-hydra-head-predicate-func=

;;   A function parse the riched-pretty-hydra-casket and return the
;;   predicated one.

;;   A =riched-pretty-hydra-casket= is a plist of three key, =:restrict=
;;   a plist of hosted restriction status and the =:pretty-hydra-casket=
;;   was the slot host the =pretty-hydra-casket=, and the =:rest-args= a
;;   list of remaining args needed by current predicate function, thus
;;   for all, it forms as:

;;   (:restrict  (:global-bind-notation-patched t ...)
;;    :pretty-hydra-casket ("Basic" (("1" (message "yes") "test message")))
;;    :rest-args (mode feature map))

(defvar entropy/emacs-hydra-hollow-predicative-keys
  '((:global-bind . entropy/emacs-hydra-hollow-global-bind-predicate)
    (:map-inject . entropy/emacs-hydra-hollow-map-inject-predicate)
    (:eemacs-top-bind . entropy/emacs-hydra-hollow-top-keymap-inject-predicate))
  "entropy-emacs pretty-hydra superstructer predicated keys
register.

A list of cons whose car is a =pretty-hydra-head= key, and the cdr
was the predication func (i.e. =pretty-hydra-head-predicate-func=)
for that key.

The list order are meaningfully that as for the proceccing
priority.

=pretty-hydra-head-predicate-func= required one
=riched-pretty-hydra-casket=, its a plist including three slot:

- :restrict :: a plist stored restriction information for rest
  predicate noticed.
- :pretty-hydra-casket :: as its naming
- :rest-args :: spec =pretty-hydra-head-predicate-func= required
  external arguments.

=pretty-hydra-head-predicate-func= return a
=riched-pretty-hydra-casket= finally but without the :rest-args
slot so that let it be appended available for next predicate
process.
")

(defun entropy/emacs-hydra-hollow-global-bind-predicate
    (riched-pretty-hydra-casket)
  "A =pretty-hydra-head-predicate-func= for key :global-bind.

Global bind current =riched-pretty-hydra-casket='s
=pretty-hydra-head='s =pretty-hydra-head-key= when head's
':global-bind' slot's pattern's evaluated result was non-nil,
pattern is evaluated by `entropy/emacs-hydra-hollow--common-judge-p'.

Mean-while for global binding, this func will auto beautified the
head's =pretty-hydra-head-notation= with 'global-map-inject type
using `entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler'.

Restriction push:

This func will enable the :global-bind-notation-beautified
restriction.

Rest arguments:

There's no rest-arguments required by this function.
"
  (let* (
         ;; :restrict
         (restrict (plist-get riched-pretty-hydra-casket :restrict))
         ;; :pretty-hydra-casket
         (pretty-hydra-casket (plist-get riched-pretty-hydra-casket :pretty-hydra-casket))
         (group (car pretty-hydra-casket))
         (pretty-hydra-casket-pattern (caadr pretty-hydra-casket))
         (key (car pretty-hydra-casket-pattern))
         (command (cadr pretty-hydra-casket-pattern))
         (notation (caddr pretty-hydra-casket-pattern))
         (pretty-hydra-casket-plist (cdddr pretty-hydra-casket-pattern))
         (global-bind-p (entropy/emacs-hydra-hollow--common-judge-p
                         (plist-get pretty-hydra-casket-plist :global-bind)))
         ;; :rest-args
         (rest-args (plist-get riched-pretty-hydra-casket :rest-args))
         )
    (when global-bind-p
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler
             notation 'global-map-inject))
      (setq restrict (append restrict '(:global-bind-notation-beautified t)))
      (entropy/emacs-hydra-hollow-define-key
       'global-map key command))
    (list :restrict restrict
          :pretty-hydra-casket
          `(,group ((,key ,command ,notation ,@pretty-hydra-casket-plist))))))

(defun entropy/emacs-hydra-hollow-map-inject-predicate
    (riched-pretty-hydra-casket)
  "A =pretty-hydra-head-predicate-func= for the head's key :map-inject.

Bind the head's key to specified map with after load specified
feature.

Restriction:

Maybe enable restrict plist's :map-inject-notation-beautified
slot.

Rest arguments: the car was a =hydra-injector=, the rest aren't
specified yet.

Key description:

=:map-inject= slot's pattern using
`entropy/emacs-hydra-hollow--common-judge-p' to evaluated, and the
valid result type are:

1) t/nil:

   enable bind or not

2) a plist:

   All keys slot pattern will be evaluated by
   `entropy/emacs-hydra-hollow--common-judge-p'.

   Valid keys stored to this plist are:
   * :do-beautify-notation :: when non-nil, beautify the notation
     for that head.

   * :do-spec-hydra-injector :: a =hydra-injector=

   * :do-inherit-predicate :: when non-nill, inject the head's key
     to the keymap with afterload feature, keymap and feature are
     the original =:rest-args= value of current
     RICHED-PRETTY-HYDRA-CASKET.

     This type was very useful when you want to do map injection
     differed from the unified spec.
"
  (let* (
         ;; :restrict
         (restrict (plist-get riched-pretty-hydra-casket :restrict))
         ;; :pretty-hydra-casket
         (pretty-hydra-casket (plist-get riched-pretty-hydra-casket :pretty-hydra-casket))
         (group (car pretty-hydra-casket))
         (pretty-hydra-casket-pattern (caadr pretty-hydra-casket))
         (key (car pretty-hydra-casket-pattern))
         (command (cadr pretty-hydra-casket-pattern))
         (notation (caddr pretty-hydra-casket-pattern))
         (pretty-hydra-casket-plist (cdddr pretty-hydra-casket-pattern))
         (map-inject (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get pretty-hydra-casket-plist
                                 :map-inject)))
         ;; :rest-args
         (rest-args (plist-get riched-pretty-hydra-casket :rest-args))
         (hydra-injector (car rest-args)))
    (cond
     ((eq map-inject t)
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler
             notation 'mode-map-inject
             (and (member :global-bind-notation-beautified restrict)
                  (entropy/emacs-hydra-hollow--common-judge-p
                   (plist-get restrict :global-bind-notation-beautified)))))
      (setq restrict
            (append restrict '(:map-inject-notation-beautified t)))
      (entropy/emacs-hydra-hollow-build-hydra-injector
       hydra-injector
       key command))
     ((entropy/emacs-common-plistp map-inject)
      (let ((do-beautify-notation
             (entropy/emacs-hydra-hollow--common-judge-p
              (plist-get map-inject :do-beautify-notation)))
            (do-spec-hydra-injector
             (entropy/emacs-hydra-hollow--common-judge-p
              (plist-get map-inject :do-spec-hydra-injector)))
            (do-inherit-predicate
             (entropy/emacs-hydra-hollow--common-judge-p
              (plist-get map-inject :do-inherit-predicate))))
        (when do-beautify-notation
          (setq notation
                (entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler
                 notation 'mode-map-inject
                 (member :global-bind-notation-beautified restrict)))
          (setq restrict
                (append restrict '(:map-inject-notation-beautified t))))
        (when do-inherit-predicate
          (entropy/emacs-hydra-hollow-build-hydra-injector
           hydra-injector
           key command))
        (when do-spec-hydra-injector
          (entropy/emacs-hydra-hollow-build-hydra-injector
           do-spec-hydra-injector
           key command)))))
    (list :restrict restrict
          :pretty-hydra-casket
          `(,group ((,key ,command ,notation ,@pretty-hydra-casket-plist)))
          )))

(defun entropy/emacs-hydra-hollow-top-keymap-inject-predicate
    (riched-pretty-hydra-casket)
  "A =pretty-hydra-head-predicate-func= for head's key :eemacs-top-bind.

Restriction:

Maybe enable =:eemacs-topkey-notation-beautified= slot in
restriction place.

Rest arguments:

No external arguments required.

Key description:

This key indicate for inject the current =pretty-hyra-head='s
command to the `entropy/emacs-top-keymap'. The pattern of this
key slot will be evaluated by
`entropy/emacs-hydra-hollow--common-judge-p' and use its result
as for judging with 't' or 'nil'.
  "
  (let* (
         ;; :restrict
         (restrict (plist-get riched-pretty-hydra-casket :restrict))
         ;; :pretty-hydra-casket
         (pretty-hydra-casket (plist-get riched-pretty-hydra-casket :pretty-hydra-casket))
         (group (car pretty-hydra-casket))
         (pretty-hydra-casket-pattern (caadr pretty-hydra-casket))
         (key (car pretty-hydra-casket-pattern))
         (command (cadr pretty-hydra-casket-pattern))
         (notation (caddr pretty-hydra-casket-pattern))
         (pretty-hydra-casket-plist (cdddr pretty-hydra-casket-pattern))
         (top-bind-p (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get pretty-hydra-casket-plist
                                 :eemacs-top-bind)))
         ;; :rest-args
         (rest-args (plist-get riched-pretty-hydra-casket :rest-args))
         )
    (when top-bind-p
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-hydra-head-notation-handler
             notation 'eemacs-top-keymap-inject
             (or (entropy/emacs-hydra-hollow--common-judge-p
                  (plist-get restrict :global-bind-notation-beautified))
                 (entropy/emacs-hydra-hollow--common-judge-p
                  (plist-get restrict :map-inject-notation-beautified))))
            restrict
            (append restrict
                    '(:eemacs-topkey-notation-beautified t)))
      (funcall
       `(lambda ()
          (entropy/emacs-hydra-hollow-define-key
           'eemacs-top-map
           ,key
           ',command))))
    (list :restrict restrict
          :pretty-hydra-casket
          `(,group ((,key ,command ,notation ,@pretty-hydra-casket-plist))))))

;; ****** batch patch pretty-hydra-casket
;; This section provide a method to handle the =pretty-hydra-cabinet=
;; with =pretty-hydra-head-predicate-func=.

;; Thus for its a batch procedure, we need to define a new data-type:
;; =riched-pretty-hydra-casket-predicate-pattern=, an alist for
;; grouping the head's predications type. Each key of this alist was
;; the predicated key defined in
;; =entropy/emacs-hydra-hollow-predicative-keys=, and the value was a
;; list i.e. the :rest-args slot of
;; =riched-pretty-hydra-head-predicate-func='s
;; =riched-pretty-hydra-casket= argument.


(defun entropy/emacs-hydra-hollow--sort-riched-pretty-hydra-casket-predicate-pattern
    (riched-pretty-hydra-casket-predicate-pattern)
  "Sort RICHED-PRETTY-HYDRA-CASKET-PREDICATE-PATTERN with the
priority as the same as the key order in
`entropy/emacs-hydra-hollow-predicative-keys'."
  (let (rtn)
    (dolist (item entropy/emacs-hydra-hollow-predicative-keys)
      (when (assoc (car item) riched-pretty-hydra-casket-predicate-pattern)
        (setq rtn
              (append rtn
                      (list (assoc (car item) riched-pretty-hydra-casket-predicate-pattern))))))
    rtn))

(defun entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
    (pretty-hydra-cabinet riched-pretty-hydra-casket-predicate-pattern &optional not-merge)
  "Rebuild PRETTY-HYDRA-CABINET with sets of
=PRETTY-HYDRA-HEAD-PREDICATE-FUNC= required by
RICHED-PRETTY-HYDRA-CASKET-PREDICATE-PATTERN, and return a
handled =pretty-hydra-cabinet= or a =pretty-hydra-caskets-list=
if Optional arguments NOT-MERGE is non-nil. "
  (when (or (not (listp riched-pretty-hydra-casket-predicate-pattern))
            (null (cl-delete nil riched-pretty-hydra-casket-predicate-pattern)))
    (error "riched-pretty-hydra-casket-predicate-pattern was fake!"))
  (entropy/emacs-hydra-hollow-with-normalized-pretty-hydra-caskets-list
   pretty-hydra-cabinet
   (let ((sp-heads $internally/normalized-pretty-hydra-caskets-list)
         (rshpp (entropy/emacs-hydra-hollow--sort-riched-pretty-hydra-casket-predicate-pattern
                 riched-pretty-hydra-casket-predicate-pattern))
         new-pretty-hydra-caskets-list)
     (when sp-heads
       (dolist (head sp-heads)
         (let (head-of-rsh)
           (if (null (cadr head))
               (setq head-of-rsh
                     (list :restrict nil :pretty-hydra-casket head))
             (dolist (predicate-pattern rshpp)
               (let* ((key (car predicate-pattern))
                      (rest-args (cdr predicate-pattern))
                      (predicate-func (alist-get key entropy/emacs-hydra-hollow-predicative-keys))
                      (rsh (if (null head-of-rsh)
                               (list :restrict nil
                                     :pretty-hydra-casket
                                     head
                                     :rest-args
                                     rest-args)
                             (append head-of-rsh `(:rest-args ,rest-args)))))
                 (setq head-of-rsh
                       (funcall predicate-func rsh)))))
           (setq new-pretty-hydra-caskets-list
                 (append new-pretty-hydra-caskets-list
                         (list (plist-get head-of-rsh :pretty-hydra-casket))))))
       (if not-merge
           new-pretty-hydra-caskets-list
         (entropy/emacs-hydra-hollow-merge-pretty-hydra-caskets-list
          new-pretty-hydra-caskets-list))))))

;; **** core hydra builder defination
;; ***** library

(defvar entropy/emacs-hydra-hollow-pretty-hydra-cabinet-external-normalize-hook nil
  "Hook for normalize a =pretty-hydra-cabinet= with customized way.

Each function must just has one argumentm, a
=pretty-hydra-cabinet-name=, which is the symbol host that
=pretty-hydra-cabinet=.")

(defun entropy/emacs-hydra-hollow-customize-pretty-hydra-cabinet
    (pretty-hydra-cabinet-name)
  "Run hook
`entropy/emacs-hydra-hollow-pretty-hydra-cabinet-external-normalize-hook'"
  (let ()
    (run-hook-with-args
     'entropy/emacs-hydra-hollow-pretty-hydra-cabinet-external-normalize-hook
     pretty-hydra-cabinet-name)))

;; ***** hydra builder for individual

;; This section provide a method to define a somewhat hydra using
;; entropy-emacs pretty hydra supperstructure. We call this hydra
;; type =entropy/emacs-pretty-hydra-for-individual=

(defun entropy/emacs-hydra-hollow-common-individual-hydra-define
    (individual-hydra-name hydra-injector heads-plist
                           &optional pretty-hydra-body pretty-hydra-category-width-indicator)
  (entropy/emacs-hydra-hollow-customize-pretty-hydra-cabinet
   'heads-plist)
  (let ((has-defined (fboundp (entropy/emacs-hydra-hollow-category-common-individual-get-caller
                               individual-hydra-name)))
        (patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
          heads-plist
          `((:map-inject ,hydra-injector)
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or pretty-hydra-body
                  (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
                   individual-hydra-name))))
    (if (null has-defined)
        (entropy/emacs-hydra-hollow-category-common-individual-define
         individual-hydra-name body patched-heads-group
         pretty-hydra-category-width-indicator)
      (entropy/emacs-hydra-hollow-category-common-individual-define+
       individual-hydra-name body patched-heads-group
       pretty-hydra-category-width-indicator))))

(defun entropy/emacs-hydra-hollow-common-individual-hydra-define+
    (individual-hydra-name hydra-injector heads-plist
                           &optional
                           pretty-hydra-body
                           pretty-hydra-category-width-indicator-for-inject)
  (entropy/emacs-hydra-hollow-customize-pretty-hydra-cabinet
   'heads-plist)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
          heads-plist
          `((:map-inject ,hydra-injector)
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or pretty-hydra-body
                  (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
                   individual-hydra-name))))
    (entropy/emacs-hydra-hollow-category-common-individual-define+
     individual-hydra-name body patched-heads-group
     pretty-hydra-category-width-indicator-for-inject)))


(defun entropy/emacs-hydra-hollow-get-random-individual-hydra-name ()
  (let ((suffix (or (ignore-errors
                      (+ 1
                         (apply 'max
                                entropy/emacs-hydra-hollow-random-func-name-number-register)))
                    1)))
    (push suffix entropy/emacs-hydra-hollow-random-func-name-number-register)
    (intern
     (format "entropy/emacs-individual-hydra-random-name-of-%s" suffix))))

(defun entropy/emacs-hydra-hollow-build-random-individual-hydra
    (pretty-hydra-cabinet &optional pretty-hydra-category-width-indicator hydra-injector)
  "Create a individual entropy-emacs superstructure hydra using a
=pretty-hydra-cabinet=, with a random =individual-hydra-name=
created by
`entropy/emacs-hydra-hollow-get-random-individual-hydra-name'.

Optional args are a =pretty-hydra-category-width-indicator=, a
HYDRA-INJECTOR, they are used as the same meaning as for
`entropy/emacs-hydra-hollow-common-individual-hydra-define'."
  (let ((random-name (entropy/emacs-hydra-hollow-get-random-individual-hydra-name)))
    (entropy/emacs-hydra-hollow-common-individual-hydra-define
     random-name
     hydra-injector
     pretty-hydra-cabinet
     nil
     pretty-hydra-category-width-indicator)
    (entropy/emacs-hydra-hollow-category-common-individual-get-caller
     random-name)))


;; ***** hydra builder for major-mode

;; This section defines the unified major-mode dispatch used for
;; entropy-emacs for calling for arbitrary buffer with its
;; major-mode. We call this hydra type
;; =entropy/emacs-pretty-hydra-for-major-mode=

(defvar entropy/emacs-hydra-hollow-major-mode-body-register nil)

;; ******* define major mode hydra
;; This section provide the method for how to define a
;; =entropy/emacs-pretty-hydra-for-major-mode=.

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode hydra-injector body heads-plist &optional ctg-width-indc)
  (entropy/emacs-hydra-hollow-customize-pretty-hydra-cabinet
   'heads-plist)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
          heads-plist
          `((:map-inject ,hydra-injector)
            (:global-bind)
            (:eemacs-top-bind)))))
    (let ()
      (funcall
       (if (fboundp (entropy/emacs-hydra-hollow-category-get-major-mode-caller
                     mode))
           'entropy/emacs-hydra-hollow-category-major-mode-define+
         'entropy/emacs-hydra-hollow-category-major-mode-define)
       mode
       body
       patched-heads-group
       ctg-width-indc)
      (unless (alist-get mode entropy/emacs-hydra-hollow-major-mode-body-register)
        (push (cons mode body)
              entropy/emacs-hydra-hollow-major-mode-body-register)))))

;; ******* add major mode hydra
;; This section provide the method for how to add a
;; =pretty-hydr-cabinet= into
;; =entropy/emacs-pretty-hydra-for-major-mode=.

(defun entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    (mode hydra-injector heads-plist
          &optional
          pretty-hydra-body
          pretty-hydra-category-width-indicator-for-inject)
  (entropy/emacs-hydra-hollow-customize-pretty-hydra-cabinet
   'heads-plist)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
          heads-plist
          `((:map-inject ,hydra-injector)
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or pretty-hydra-body
                  (alist-get
                   mode
                   entropy/emacs-hydra-hollow-major-mode-body-register)
                  (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
                   mode))))
    (entropy/emacs-hydra-hollow-category-major-mode-define+
     mode
     body
     patched-heads-group
     pretty-hydra-category-width-indicator-for-inject)))

;; **** miscellanies

;; ***** Recursive cabinet

(defun entropy/emacs-hydra-holow-recursive-expand-pretty-hydra-cabinet
    (pretty-hydra-cabinet-name)
  "Recursively expand the nested =pretty-hydra-cabinet= of
PRETTY-HYDRA-CABINET-NAME.

A nested =pretty-hydra-cabinet= who has at least one
=pretty-hydra-head= whose =pretty-hydra-head-command= was a plist
whose car was the key =:pretty-hydra-cabinet= and its value is a
pattern prepared for evaluating by
`entropy/emacs-hydra-hollow--common-judge-p' and its return must
be a =pretty-hydra-cabinet=.

Other valid key to this plist type =pretty-hydra-head-command=:

- =:pretty-hydra-category-width-indicator=


Internal expanding mechanism:

Using `entropy/emacs-hydra-hollow-build-random-individual-hydra'
to build a =pretty-hydra= according to the evaluated value of the
=:pretty-hydra-cabinet= slot's pattern and replace current nested
=pretty-hydra-head-command= for the 'expanded' (i.e. the real
hydra body caller) =pretty-hydra-head-command=.
"
  (let* ((pretty-hydra-cabinet (symbol-value pretty-hydra-cabinet-name))
         (pretty-hydra-caskets-list
          (entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list
           pretty-hydra-cabinet))
         (command-is-nest-func
          (lambda (command)
            (and (listp command)
                 (eq (car command) :pretty-hydra-cabinet))))
         new-pretty-hydra-caskets-list)

    (dolist (pretty-hydra-casket
             pretty-hydra-caskets-list)
      (let* ((group-name (car pretty-hydra-casket))
             (casket-pattern (caadr pretty-hydra-casket))
             (command
              (nth 1 casket-pattern))
             (key (car casket-pattern))
             (notation (nth 2 casket-pattern))
             (rest-keypairs (cdddr casket-pattern)))
        (if (funcall command-is-nest-func command)
            (let* ((sub-cabinet
                    (entropy/emacs-hydra-hollow--common-judge-p
                     (plist-get command :pretty-hydra-cabinet)))
                   (sub-ctg-width-indicator (plist-get command :pretty-hydra-category-width-indicator))
                   (sub-rest-args (plist-get command :other-rest-args))
                   (sub-casket-lists
                    (entropy/emacs-hydra-hollow-make-pretty-hydra-caskets-list
                     sub-cabinet))
                   non-nested-subcabinet)
              (if (catch :exit
                    (dolist (el sub-casket-lists)
                      (let ((sub-command
                             (nth 1 (caadr el))))
                        (when (funcall command-is-nest-func sub-command)
                          (throw :exit t)))))
                  (setq non-nested-subcabinet
                        (entropy/emacs-hydra-holow-recursive-expand-pretty-hydra-cabinet
                         'sub-cabinet))
                (setq non-nested-subcabinet sub-cabinet))
              (let ((new-pretty-hydra-casket-pattern
                     (list
                      key
                      (apply 'entropy/emacs-hydra-hollow-build-random-individual-hydra
                             non-nested-subcabinet
                             sub-ctg-width-indicator
                             sub-rest-args)
                      notation)))

                (unless (null rest-keypairs)
                  (setq new-pretty-hydra-casket-pattern
                        (append new-pretty-hydra-casket-pattern
                                rest-keypairs)))

                (setq new-pretty-hydra-caskets-list
                      (append new-pretty-hydra-caskets-list
                              (list
                               (list
                                group-name
                                (list
                                 new-pretty-hydra-casket-pattern)))))))
          (setq new-pretty-hydra-caskets-list
                (append new-pretty-hydra-caskets-list
                        (list pretty-hydra-casket))))))
    (set pretty-hydra-cabinet-name
         (entropy/emacs-hydra-hollow-merge-pretty-hydra-caskets-list
          new-pretty-hydra-caskets-list))))

(add-hook 'entropy/emacs-hydra-hollow-pretty-hydra-cabinet-external-normalize-hook
          #'entropy/emacs-hydra-holow-recursive-expand-pretty-hydra-cabinet)


;; ** pre hydra hollow platform

;; This section gives entropy-emacs specified hydra defination
;; platform for other entropy-emacs config to attend in. Including
;; some customized hydras and their equivalent =use-package= feature
;; support.

;; *** top dispatcher
;; This section defines the root hydra dispatch used for
;; entropy-emacs for calling globally. We call this hydra
;; =entropy/emacs-pretty-hydra-for-top-dispatch=.

(defvar entropy/emacs-hydra-hollow-init-top-dispatch-individual-hydra-name
  'entropy/emacs-hydra-hollow-top-dispatch)

(defvar entropy/emacs-hydra-hollow-top-dispatch-register nil)
(defvar entropy/emacs-hydra-hollow-top-dispatch-init-done nil)

(defun entropy/emacs-hydra-hollow-init-top-dispatch (&optional force)
  (when (or (not entropy/emacs-hydra-hollow-top-dispatch-init-done)
            force)
    (let* ((this-individual-hydra-name entropy/emacs-hydra-hollow-init-top-dispatch-individual-hydra-name)
           (this-individual-hydra-caller
            (entropy/emacs-hydra-hollow-category-common-individual-get-caller
             this-individual-hydra-name)))
      (entropy/emacs-hydra-hollow-common-individual-hydra-define
       this-individual-hydra-name nil
       '("Basic"     ()
         "WI&BUF"    ()
         ;; ---
         "Highlight" ()
         "Utils"     ()
         ;; ---
         "Shellpop"  ()
         "Structure" ()
         "WWW"       ()
         "Rss"       ()
         ;; ---
         "Vcs"       ()
         "Tramp"     ()
         ;; ---
         "Project"   ()
         "Org"       ()
         ;; ---
         "IME"       ()
         "Misc."     ()
         )
       '(:title
         (entropy/emacs-pretty-hydra-make-title
          "eemacs top dispatch" "faicon" "toggle-on")
         :color ambranth
         :quit-key "q"
         :separator "=")
       '((2 :width-desc "Basic & window or buffer")
         (2 :width-desc "highlight or useful utils")
         (4 :width-desc "Common knifes")
         (2 :width-desc "Version controll and remote connection")
         (2 :width-desc "Projectile and Org")
         (2 :width-desc "IME and miscellaneous")))
      (unless entropy/emacs-hydra-hollow-top-dispatch-init-done
        (setq entropy/emacs-hydra-hollow-top-dispatch-init-done t)
        (entropy/emacs-!set-key
          (kbd "h")
          this-individual-hydra-caller)))))

(defun entropy/emacs-hydra-hollow-add-for-top-dispatch
    (pretty-hydra-cabinet)
  (entropy/emacs-hydra-hollow-init-top-dispatch)
  (let* ((pretty-hydra-caskets-list
          (entropy/emacs-hydra-hollow-rebuild-pretty-hydra-cabinet
           ;; use copy-tree since its has side-effect to origin one
           (copy-tree pretty-hydra-cabinet)
           '((:global-bind)
             (:eemacs-top-bind))
           t)))
    (unless (null pretty-hydra-caskets-list)
      (dolist (sp-h pretty-hydra-caskets-list)
        (setq entropy/emacs-hydra-hollow-top-dispatch-register
              (append entropy/emacs-hydra-hollow-top-dispatch-register
                      `(,sp-h))))

      (entropy/emacs-hydra-hollow-common-individual-hydra-define+
       entropy/emacs-hydra-hollow-init-top-dispatch-individual-hydra-name
       nil pretty-hydra-cabinet))))


;; *** sparse tree builder

;; This section provide a method for defining one
;; =entropy/emacs-pretty-hydra-for-major-mode= with builtin
;; =pretty-hydra-cabinet-unit=.

(defun entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-core
    (mode hydra-injector &optional pretty-hydra-category-width-indicator)
  (let ((body
         (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
          mode)))
    (progn
      (unless (alist-get mode entropy/emacs-hydra-hollow-major-mode-body-register)
        (push (cons mode
                    body)
              entropy/emacs-hydra-hollow-major-mode-body-register))
      (entropy/emacs-hydra-hollow-define-major-mode-hydra
       mode hydra-injector
       body
       '("Help"
         nil)
       pretty-hydra-category-width-indicator))))

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    (mode hydra-injector do-not-build-sparse-tree
          &optional
          heads
          pretty-hydra-category-width-indicator-for-build
          pretty-hydra-category-width-indicator-for-inject)
  (let ((has-defined
         (fboundp (entropy/emacs-hydra-hollow-category-get-major-mode-caller
                   mode)))
        (pretty-hydra-body
         (or
          (alist-get
           mode
           entropy/emacs-hydra-hollow-major-mode-body-register)
          (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
           mode))))
    (unless (or has-defined do-not-build-sparse-tree)
      (entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-core
       mode hydra-injector pretty-hydra-category-width-indicator-for-build))
    (if (and (null do-not-build-sparse-tree)
             (null has-defined))
        (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
         mode hydra-injector heads nil
         pretty-hydra-category-width-indicator-for-inject)
      (entropy/emacs-hydra-hollow-define-major-mode-hydra
       mode hydra-injector pretty-hydra-body heads
       pretty-hydra-category-width-indicator-for-build))))


;; *** use-package extended

;; This section defined the use-pacakge extended keywords based on
;; entropy-emacs pretty hydra superstructure.

;; **** library

;; ***** pattern parse
;; For give the more powerful key slot form syntax, we defined a new
;; form-type =pretty-hydra-riched-usepackage-pattern=, see below
;; function's doc-string for more details.

(defun entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
    (pattern-form)
  "A PATTERN-FORM was a ISLAND or a list of ISLANDs. The PATTERN-FORM
has its termed name =pretty-hydra-riched-usepackage-pattern=.

- Island

  consists of =Baron= and =Heads-Group=

  (list Baron Heads-Group)

- Baron

  consists of =Section= or =Section=s, each =Section= has a
  =Attribute= a plist and a =Request= a list.

  (list Attribute Request)

  or

  (list
    (list Attribute-0 Request-0)
    (list Attribute-1 Request-1)
    ...
  )

For fully naming each part of
=pretty-hydra-riched-usepackage-pattern=, we termed their as:

- =pretty-hydra-riched-usepackage-pattern-island=
- =pretty-hydra-riched-usepackage-pattern-baron=
- =pretty-hydra-riched-usepackage-pattern-section=
- =pretty-hydra-riched-usepackage-pattern-section-attribute=
- =pretty-hydra-riched-usepackage-pattern-section-request=


=Request= can be omitted or nil as element injected to the
pattern-form, for =Heads-Group= can not be ommited but for a
single 'nil' instead because the single =Island= pattern-fom with
multi =Sections= of =Baron= has same form type as the multi
=Islands= pattern-form of which the first =Island= is single
=Section= type when their's =Heads-Group= and inline =Request= are
both ommited, that as:

   1: multi =Section= of =Baron= of single =Island= as
   PATTERN-FORM with omitted =Heads-Group= and inline =Request=
   ((((:enable t)) ) b)

   2: multi =Island= PATTERN-FORM whose the first =Island= was
   single =Section= of =Baron= with omitted =Heads-Group= and
   inline =Request=
   ((((:enable t)) b) )
"
  (let* ((ptform-single-island-p
          (and (= (length pattern-form) 2)
               (or (ignore-errors (stringp (caadr pattern-form)))
                   (null (cadr pattern-form)))))
         (island-baron-section-single-p
          (lambda (island)
            (let ((baron (car island)))
              (and (or (= (length baron) 2)
                       (= (length baron) 1))
                   (symbolp (caar baron))))))
         (island-multi-sections-split-func
          (lambda (multi-secs-island)
            (let ((sections (car multi-secs-island))
                  (heads (cadr multi-secs-island))
                  split-island)
              (dolist (sec sections)
                (setq split-island
                      (append split-island (list (list sec heads)))))
              split-island)))
         (island-parse-func
          (lambda (island)
            (let (output)
              (if (funcall island-baron-section-single-p
                           island)
                  (setq output (list island))
                (setq output
                      (funcall island-multi-sections-split-func
                               island)))
              output)))
         rtn)
    (cond
     (ptform-single-island-p
      (setq rtn
            (funcall island-parse-func
                     pattern-form)))
     ((null ptform-single-island-p)
      (dolist (island pattern-form)
        (let ((obtain (funcall island-parse-func
                               island)))
          (setq rtn (append rtn obtain))))))
    rtn))

;; ***** defer parse

(defvar entropy/emacs-hydra-hollow/use-package/defer-parse-random-func-ids-pool nil)
(defun  entropy/emacs-hydra-hollow/use-package/defer-parse/gen-random-ad-funcname-prefix
    (use-name adtype)
  (let* ((id-pool
          entropy/emacs-hydra-hollow/use-package/defer-parse-random-func-ids-pool)
         (id (if id-pool
                 (+ (car id-pool) 1)
               0)))
    (push id
          entropy/emacs-hydra-hollow/use-package/defer-parse-random-func-ids-pool)
    (format "eemacs-use-package/hydra-hollow-defer-parse/for-%s/adtype-of-%s/func-id_%s"
            use-name adtype id)))

(defvar entropy/emacs-hydra-hollow/use-package/defer-parse-random-judger-ids-pool nil)
(defun  entropy/emacs-hydra-hollow/use-package/defer-parse/gen-random-ad-judger-prefix
    (use-name)
  (let* ((id-pool
          entropy/emacs-hydra-hollow/use-package/defer-parse-random-judger-ids-pool)
         (id (if id-pool
                 (+ (car id-pool) 1)
               0)))
    (push id
          entropy/emacs-hydra-hollow/use-package/defer-parse-random-judger-ids-pool)
    (format "eemacs-use-package/hydra-hollow-defer-parse/for-%s/judger-id_%s"
            use-name id)))

(defun entropy/emacs-hydra-hollow/use-package/defer-parse/rebuild-pattern
    (use-name orig-pattern)
  (let (pattern)
    (cond ((and (listp orig-pattern)
                (not (null orig-pattern)))
           (let ((elts orig-pattern))
             (dolist (ptr elts)
               (let* ((enable       (plist-get ptr :enable))
                      (adfors       (plist-get ptr :adfors))
                      (adtype       (plist-get ptr :adtype))
                      (pdump-no-end (plist-get ptr :pdumper-no-end))
                      (evfunc-0 (lambda (val)
                                  (cond ((symbolp val)
                                         val)
                                        ((listp val)
                                         (eval val)))))
                      (evfunc-1 (lambda (val)
                                  (cond ((symbolp val)
                                         (symbol-value val))
                                        ((listp val)
                                         (eval val)))))
                      (evfunc-2 (lambda (val)
                                  (cond ((symbolp val)
                                         (symbol-value val))
                                        ((listp val)
                                         (mapcar
                                          (lambda (x)
                                            (funcall evfunc-0 x))
                                          val))))))
                 (push
                  (list :enable         (funcall evfunc-1 enable)
                        :adfors         (funcall evfunc-2 adfors)
                        :adtype         (funcall evfunc-0 adtype)
                        :pdumper-no-end (funcall evfunc-1 pdump-no-end)
                        )
                  pattern))))
           (reverse pattern))
          ((symbolp orig-pattern)
           (if (eq t orig-pattern)
               t
             (if (symbol-value orig-pattern)
                 t
               nil)))
          (t
           (error
            "eemacs hydra hollow adrequire clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
    (use-name pattern form)
  "Make force require USE-NAME by applying advice to the buntch
of functions/hooks according to before or after advice type or
hook injection which use core of FORM, and these specification is
termed of =hydra-hollow-adrequire-patterns= i.e. the
PATTERN. Return a new wrapped form.

=hydra-hollow-adrequire-patterns= is a list of plist, valid keys of the
plist are:

- :adfors :: a list of symbol of a function/hook or form which
  symbol is identically return but form will be evaluated to a
  result of a symbol as a function/hook. And the evaluated of
  this slot will be a list of function/hook symbols.

- :adtype :: a symbol or a form which symbol is identically
  return but form will be evaluated to a result of a symbol, and
  both of those type are indicate the `advice-add' type of
  'before' or 'after' or a symbol 'hook' indicate treat :adfors
  as bunch of hooks to be injected.

- :pdumper-no-end :: when non-nil do not inject adrequire into
  `entropy/emacs-pdumper-load-hook' when
  `entropy/emacs-custom-enable-lazy-load'.

Further more if the PATTERN is t, we just wrapped FORM within
`entropy/emacs-lazy-load-simple' which stick to USE-NAME.

And if PATTERN is nil, then we return the form as is.
"
  (let* ((patterns
          (entropy/emacs-hydra-hollow/use-package/defer-parse/rebuild-pattern
           use-name pattern))
         (judger-var
          (intern
           (entropy/emacs-hydra-hollow/use-package/defer-parse/gen-random-ad-judger-prefix
            use-name)))
         (_ (eval `(defvar ,judger-var nil
                     ,(format
                       "the judger var for use-package \
hydra hollow instance deferred creation for package '%s' \
which non-nil indicate that \
the instance has been created and the related form is banned."
                       use-name))))
         (form-use-judge
          `(unless (bound-and-true-p ,judger-var)
             (prog1
                 ,form
               (setq ,judger-var t))))
         rtn)
    (cond
     ((and (listp patterns)
           (not (null patterns)))
      (dolist (ptr patterns)
        (let* ((adfors       (plist-get ptr :adfors))
               (adtype       (plist-get ptr :adtype))
               (pdump-no-end (plist-get ptr :pdumper-no-end))
               (ad-wrapper (cond ((eq adtype 'before)
                                  'entropy/emacs-lazy-initial-advice-before)
                                 ((eq adtype 'after)
                                  'entropy/emacs-lazy-initial-advice-after)
                                 ((eq adtype 'hook)
                                  'entropy/emacs-lazy-initial-for-hook)
                                 (t
                                  (error "wrong type of hydra-hollow adwrapper type '%s'"
                                         adtype))))
               (adprefix (entropy/emacs-hydra-hollow/use-package/defer-parse/gen-random-ad-funcname-prefix
                          use-name adtype)))
          (push
           `(,ad-wrapper
             ,adfors ,adprefix ,adprefix prompt-echo
             :pdumper-no-end ',pdump-no-end
             ,form-use-judge)
           rtn)))
      ;; progn wrap the result
      (setq rtn
            (append '(progn)
                    (reverse rtn))))
     ((eq patterns t)
      (setq rtn
            `(entropy/emacs-lazy-load-simple ,use-name
               ;; NOTE: always defer for require
               :always-lazy-load t
               ,form-use-judge)))
     (t
      (setq rtn form)))
    rtn))

;; **** :eemacs-tpha
;; The use-package key =:eemacs-tpha= indicating to add some
;; =pretty-hydra-cabinet= into
;; =entropy/emacs-pretty-hydra-for-top-dispatch=.

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :init)
                 collect :init and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-handler
    (use-name key $arg rest state)
  "The use-package =:eemacs-tpha= key handler.

Special for its
=pretty-hydra-riched-usepackage-pattern-section-request= was
without any request.

Special for its
=pretty-hydra-riched-usepackage-pattern-section-attribute= just
has one valid key =:enable=, it will be sent to
`entropy/emacs-hydra-hollow--common-judge-p' and using that's
evaluated result as its value.
"
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (requests (cadr baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (defer (let ((defer-slot (plist-get attr :defer)))
                      (entropy/emacs-hydra-hollow--common-judge-p
                       defer-slot)))
             (heads (cadr island))
             (heads-append-arg `(,heads))
             (core-caller
              `(apply
                'entropy/emacs-hydra-hollow-add-for-top-dispatch
                ',heads-append-arg))
             (main-caller
              (if defer
                  (entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
                   use-name defer
                   core-caller)
                core-caller)))
        (when enable
          (setq init-form
                (append init-form
                        `(,main-caller))))))
    (use-package-concat
     rest-body
     init-form)))

(defalias 'use-package-normalize/:eemacs-tpha
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-normalize)

(defalias 'use-package-handler/:eemacs-tpha
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-tpha)

;; **** :eemacs-mmphc
;; The use-package key =:eemacs-mmphc= indicating define a
;; =entropy/emacs-pretty-hydra-for-major-mode=.

(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :bind-keymap*)
                 collect :bind-keymap* and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log
                        (list use-name :normalize-arg key-value))
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-handler
    (use-name key $arg rest state)
  "The use-package keyword handler for =:eemacs-mmphc=

- Special for its =pretty-hydra-riched-usepackage-pattern-section-request=:

  #+begin_src emacs-lisp
    '(
      mode hydra-injector do-not-build-sparse-tree
      pretty-hydra-category-width-indicator-for-build
      pretty-hydra-category-width-indicator-for-inject)
  #+end_src

- Special for its =pretty-hydra-riched-usepackage-pattern-section-attribute=:

  Just has one valid key =:enable=, it will be sent to
  `entropy/emacs-hydra-hollow--common-judge-p' and using that's
  evaluated result as its value."

  (let* ((rest-body (use-package-process-keywords use-name rest state))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log
                 (list use-name :handle-arg $arg))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get attr :enable)))
             (defer (entropy/emacs-hydra-hollow--common-judge-p
                     (plist-get attr :defer)))
             (requests (cadr baron))
             (mode (or (car requests)
                       use-name))
             (hydra-injector (cadr requests))
             (do-not-build-sparse-tree
              (caddr requests))
             (ctg-width-indc-for-build
              (nth 3 requests))
             (ctg-width-indc-for-inject
              (nth 4 requests))
             (heads (cadr island))
             core-caller)
        (setq hydra-injector
              (entropy/emacs-hydra-hollow-make-hydra-injector
               hydra-injector
               use-name
               (intern (format "%s-map" (symbol-name use-name)))))
        (setq core-caller
              `(entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
                ',mode ',hydra-injector ',do-not-build-sparse-tree ',heads
                ',ctg-width-indc-for-build
                ',ctg-width-indc-for-inject))
        (setq
         init-form
         (append init-form
                 `((when (not (null ',enable))
                     ,(if defer
                          (entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
                           use-name defer
                           core-caller)
                        core-caller)))))))
    (use-package-concat
     init-form
     rest-body)))

(defalias 'use-package-normalize/:eemacs-mmphc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-normalize)

(defalias 'use-package-handler/:eemacs-mmphc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-add-keyword
 :eemacs-mmphc)

;; **** :eemacs-mmphca
;; The use-package key =:eemacs-mmphca= indicating to add some
;; =pretty-hydra-cabinet= into a specified
;; =entropy/emacs-pretty-hydra-for-major-mode=.

(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
        ;; should go in the same location as :eemacs-mmc
        (cl-loop for item in use-package-keywords
                 if (eq item :eemacs-mmphc)
                 collect :eemacs-mmphc and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log
                        (list use-name :normalize-arg key-value))
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mmca common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-handler
    (use-name key $arg rest state)
    "The use-package keyword handler for =:eemacs-mmphca=

- Special for its =pretty-hydra-riched-usepackage-pattern-section-request=:

  #+begin_src emacs-lisp
    '(
      mode hydra-injector
      pretty-hydra-body
      pretty-hydra-category-width-indicator-for-inject)
  #+end_src

- Special for its =pretty-hydra-riched-usepackage-pattern-section-attribute=:

  Just has one valid key =:enable=, it will be sent to
  `entropy/emacs-hydra-hollow--common-judge-p' and using that's
  evaluated result as its value."

  (let* ((rest-body (use-package-process-keywords use-name rest state))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log
                 (list use-name :handle-arg $arg))
    (setq
     init-form
     `((let (callers-for-run)
         (dolist (island ',$arg)
           (let* ((baron (car island))
                  (attr (car baron))
                  (enable (let ((enable-slot (plist-get attr :enable)))
                            (entropy/emacs-hydra-hollow--common-judge-p
                             enable-slot)))
                  (defer (let ((defer-slot (plist-get attr :defer)))
                           (entropy/emacs-hydra-hollow--common-judge-p
                            defer-slot)))
                  (requests (cadr baron))
                  (mode (car requests))
                  (hydra-injector (cadr requests))
                  (pretty-hydra-body (nth 2 requests))
                  (pretty-hydra-category-width-indicator-for-inject
                   (nth 3 requests))
                  (heads (cadr island))
                  run-call
                  core-caller)
             (setq core-caller
                   (list 'apply
                         (list 'function 'entropy/emacs-hydra-hollow-add-to-major-mode-hydra)
                         (list 'quote
                               (list mode hydra-injector heads
                                     pretty-hydra-body
                                     pretty-hydra-category-width-indicator-for-inject))))
             (when enable
               (setq run-call
                     (list 'lambda nil
                           (if defer
                               (entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
                                ',use-name defer
                                core-caller)
                             core-caller)))
               (push run-call callers-for-run))))
         (when (not (null callers-for-run))
           (dolist (caller (reverse callers-for-run))
             (funcall caller))))))
    (use-package-concat
     init-form
     rest-body)))

(defalias 'use-package-normalize/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-normalize)

(defalias 'use-package-handler/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-add-keyword
 :eemacs-mmphca)

;; **** :eemacs-indhc
;; The use-package key =:eemacs-indhc= indicating to define a
;; =entropy/emacs-pretty-hydra-for-individual=.

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :bind)
                 collect :bind and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defalias 'use-package-normalize/:eemacs-indhc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-normalize)

(defalias 'use-package-handler/:eemacs-indhc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-indhc)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-handler
    (use-name key $arg rest state)
  "The use-package keyword handler for =:eemacs-indhc=

- Special for its =pretty-hydra-riched-usepackage-pattern-section-request=:

  #+begin_src emacs-lisp
    '(
      individual-hydra-name hydra-injector
      pretty-hydra-body
      pretty-hydra-category-width-indicator
     )
  #+end_src

- Special for its =pretty-hydra-riched-usepackage-pattern-section-attribute=:

  Just has one valid key =:enable=, it will be sent to
  `entropy/emacs-hydra-hollow--common-judge-p' and using that's
  evaluated result as its value."


  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (defer (let ((defer-slot (plist-get attr :defer)))
                      (entropy/emacs-hydra-hollow--common-judge-p
                       defer-slot)))
             (heads (cadr island))
             (requests (cadr baron))
             (individual-hydra-name (car requests))
             (hydra-injector (cadr requests))
             (pretty-hydra-body (caddr requests))
             (ctg-width-indc (nth 3 requests))
             core-caller)
        (setq hydra-injector
              (entropy/emacs-hydra-hollow-make-hydra-injector
               hydra-injector
               use-name
               (intern (format "%s-map" (symbol-name use-name)))))
        (setq core-caller
              `(entropy/emacs-hydra-hollow-common-individual-hydra-define
                ',individual-hydra-name
                ',hydra-injector
                ',heads
                ',pretty-hydra-body
                ',ctg-width-indc))
        (when enable
          (setq init-form
                (append init-form
                        (if defer
                            `(,(entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
                                use-name defer
                                core-caller))
                          `(,core-caller)))))))
    (use-package-concat
     rest-body
     init-form)))

;; **** :eemacs-indhca

;; The use-package key =:eemacs-indhca= indicating to add some
;; =pretty-hydra-cabinet= into a specified
;; =entropy/emacs-pretty-hydra-for-individual=

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :eemacs-indhca)
                 collect :eemacs-indhca and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defalias 'use-package-normalize/:eemacs-indhca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-normalize)

(defalias 'use-package-handler/:eemacs-indhca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-indhca)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-handler
    (use-name key $arg rest state)
    "The use-package keyword handler for =:eemacs-indhca=

- Special for its =pretty-hydra-riched-usepackage-pattern-section-request=:

  #+begin_src emacs-lisp
    '(
      individual-hydra-name hydra-injector
      pretty-hydra-body
      pretty-hydra-category-width-indicator-for-inject)
  #+end_src

- Special for its =pretty-hydra-riched-usepackage-pattern-section-attribute=:

  Just has one valid key =:enable=, it will be sent to
  `entropy/emacs-hydra-hollow--common-judge-p' and using that's
  evaluated result as its value."

  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (defer (let ((defer-slot (plist-get attr :defer)))
                      (entropy/emacs-hydra-hollow--common-judge-p
                       defer-slot)))
             (heads (cadr island))
             (requests (cadr baron))
             (individual-hydra-name (car requests))
             (hydra-injector (cadr requests))
             (pretty-hydra-body (caddr requests))
             (ctg-width-indc-for-inject (nth 3 requests))
             (core-caller
              `(entropy/emacs-hydra-hollow-common-individual-hydra-define+
                ',individual-hydra-name
                ',hydra-injector
                ',heads
                ',pretty-hydra-body
                ',ctg-width-indc-for-inject)))
        (when enable
          (setq init-form
                (append init-form
                        (if defer
                            `(,(entropy/emacs-hydra-hollow/defer-parse/gen-wrapper
                                use-name defer
                                core-caller))
                          `(,core-caller)))))))
    (use-package-concat
     rest-body
     init-form)))

;; * provide

;; Disable messy commands in `M-x' list
(defun entropy/emacs-hydra-hollow-read-extended-command-predicate
    (command &rest _)
  (if (and (symbolp command)
           (string-match-p
            "^\\(entropy/emacs-individual-hydra\
\\|eemacs-hydra-for-mode\
\\|eemacs-hydra-hollow-random-func\\)"
            (symbol-name command)))
      nil
    t))
(add-to-list 'entropy/emacs-read-extended-command-predicates
             #'entropy/emacs-hydra-hollow-read-extended-command-predicate)

(provide 'entropy-emacs-hydra-hollow)
