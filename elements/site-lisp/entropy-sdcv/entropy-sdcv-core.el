;;; entropy-sdcv-core.el --- The core framework of entropy-sdcv  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20191102  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Created:       2019-11-02
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
;;; Commentary:

;; The core defination framework for =entropy-sdcv.el=.

;;;; Protocols:

;; We defined below four top category protocols, each of them may
;; have the subroutine protocols as the components role, but they
;; are individually for usage.

;; + DICT-BACKEND

;;   One dict type present in =entropy-sdcv= as one =DICT-BACKEND=,
;;   which functionally for query responses and given the response
;;   filter method even for propertizing its respones.

;;   A =DICT-BACKEND= is a cons consists of two aspect, car of
;;   =DICT-NAME= and cdr of =DICT-INSTANCE=.

;;   1) DICT-NAME

;;      A symbol to indicate the DICT-BACKEND as the name denotation.

;;   2) DICT-INSTANCE

;;      A plist consists of three slots, e.g. :query-function,
;;      :show-predicate and :show-face, see
;;      ~entropy/sdcv-core-query-backends-register~ doc-string for
;;      them defination.

;; + SHOW-TYPE

;;   The query-response display type, along with wild possible
;;   customizable specification, the feedback can be showed in
;;   popup-tooltip or adjacent buffer etc. any way you willing to.

;;   A =SHOW-TYPE= is a cons consists of two component, car of
;;   =SHOW-METHOD= and cdr of =SHOW-FUNCTION=.

;;   1. SHOW-METHOD

;;      A symbol as name denotation for its SHOW-TYPE.

;;   2. SHOW-FUNCTION

;;      Function which recieve one SHOW-INSTANCE (see below section)
;;      to display the resule to caller.

;; + SHOW-INSTANCE

;;   The query-response representation data-struct, abstract way
;;   include the string obtained from =DICT-BACKEND=, and the its
;;   SHOW-PREDICAT with SHOW-FACE.

;;   Consists of =FEEDBACK=, =SHOW-PREDICATE=, =SHOW-FACE=

;;   1. FEEDBACK

;;      A string as dict query-response.

;;   2. SHOW-PREDICATE

;;      A predicate to filte the FEEDBACK and did specification in
;;      the SHOW-TYPE of specified SHOW-METHOD.

;;   3. SHOW-FACE

;;      The simple propertized face for FEEDBACK.

;; + SHOW-ASPECT

;;   The fix-up representaion of dict query-response, indicated
;;   specific SHOW-METHOD, it's a cons of SHOW-MEHOD and
;;   SHOW-INSTANCE.

;;; Configuration:
;;
;; See ~entropy/sdcv-core-group~ variable group.
;;
;;; code
;;;; require

(require 'tooltip)
(condition-case nil (require 'posframe) (error (message "Warn: You haven't install posframe!")))
(require 'cl-lib)
(require 'chinese-word-at-point)

;;;; defs
(defvar entropy/sdcv-core--jieba-test-py
  (expand-file-name
   "jieba-test.py"
   (file-name-directory load-file-name))
  "jieba test feature python process file.")

(defvar entropy/sdcv-core--jieba-test-result nil
  "Result sticking for whether jieba cli tool installed.")

(defun entropy/sdcv-core--jieba-installed ()
  (unless (member entropy/sdcv-core--jieba-test-result '(installed notfound))
    (let ((sh-cbk (shell-command-to-string
                   (format "python \"%s\""
                           entropy/sdcv-core--jieba-test-py)))
          (rx-match "ModuleNotFoundError"))
      (setq entropy/sdcv-core--jieba-test-result
            (if (string-match-p rx-match sh-cbk)
                'notfound
              'installed)))))

;;;; variable declaration
;;;;; top group

(defgroup entropy-sdcv nil
  "Customizable variables group for `entropy-sdcv'."
  :group 'extensions)

;;;;; custom

(defgroup entropy/sdcv-core-group nil
  "Group for `entropy-sdcv' feature."
  :group 'entropy-sdcv)

(defcustom entropy/sdcv-core-source-language "zh-CN"
  "Mother tougue"
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-get-thing-with-lang_zh_CN-p t
  "Whether enable thing gets with zh_CN language."
  :type 'boolean
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-query-backends-register
  '()
  "List of dict type of quering for.

Each of the dict type represented as one DICT-BACKEND in
`entropy-sdcv.el', who is one alist whose car was the DICT-NAME a
symbol to indicate this dict type and the cdr is plist of three
slots, i.e. :query-function, :show-predicate and :show-face, this
plist called DICT-INSTANCE.

:query-function slotted one two arguments function, the first
argument was string as dict quering, it return the FEEDBACK, and
the second was the SHOW-METHOD.

:show-predicate was the slot whose value was one function with
two parameters i.e. FEEDBACK and SHOW-METHOD, almostly it was the
function to decorate the FEEDBACK query from current backend
according to the SHOW-MEHOD but with further more tricks and must
be matched with the corresponding SHOW-METHOD's orderings (see
referred section below) or it can be nil (or omitted) which means
do nothing in this context.

:show-face slotted the showed string (the FEEDBACK) face or
nil (or omitted) means using default setting, or it is a funciton
which return one specified face with one argument of SHOW-METHOD.

FEEDBACK was string calling back from the specific DICT-BACKEND,
commonly non-propertized, but can be fontlocked by the backend
itself, after all it must be a string which can be validated by
`stringp'.

SHOW-METHOD was a symbol for indicating how to show the FEEDBACK,
the default valid value are showed in
`entropy/sdcv-core-response-show-frontends' (see it for details).
"
  :type '(repeat :tag "Query backend object list"
                 (cons :tag "Query backend"
                       (symbol :tag "Name of current backend")
                       (plist :tag "Query backend plist" :value-type sexp)))
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-show-frontends '()
  "The alist whose each element was one query-response SHOW-TYPE
cons. The cons which car is SHOW-METHOD of a symbol and the cdr
was the FEEDBACK showing frontend function SHOW-FUNCTION whose
parameter is SHOW-INSTANCE i.e. a plist with three slots,
i.e. :feedback, :show-predicate and :show-face.

The built-in supported SHOW-METHODs are: (Notice for its predicate
usage detailes)

- 'adjacent-common': show FEEDBACK with adjacent popuped buffer
  from frame bottom. the :show-predicate function will calling
  within the popuped buffer after the FEEDBACK has be injected.

- 'posframe': show FEEDBACK with popuped tooltip around the
  current point using package `posframe'. The :show-predicate
  function will be used as the the :initialize slot of
  `posframe-show' (see the defination of `posframe-show' for
  detailes)

- 'popup': show FEEDBACK with popuped tooltip around the current
  point using package `popup'. The :show-predicate function will
  calling as the FEEDBACK decoration filter.

- 'postip': show FEEDBACK with popuped tooltip around the current
  point using package `pos-tip'. The :show-predicate function will
  does as what did in 'popup' way.

You can add more SHOW-METHODs customizable using `add-to-list'."
  :type '(alist
          :key-type (symbol :tag "Show method name")
          :value-type (function :tag "Show function"))
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-null-prompt "In the beginning, there's darkness!"
  "Feedback string for none-matching session."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-column-width-max 60
  "Query feedback info definitions string overflow width used to
fill lines destructively or display with truncated occur."
  :type 'integer
  :group 'entropy/sdcv-core-group)

;;;;; defvar

(defvar entropy/sdcv-core-query-log nil
  "Variable stored current query string for package debug using."
  )

(defvar entropy/sdcv-core-response-log nil
  "Variable stored current query response string for package debug using."
  )

(defvar entropy/sdcv-core--utf-8-backends-register '(sdcv)
  "List of query response dictionary backends which must ran with
utf-8 coding system, this is used for backends developer to ask
for the specifications.

If the backend not member in this list will be ran within the
current local coding system which is obtained from
`default-process-coding-system'.")

;;;;; default face
(defface entropy/sdcv-core-common-face '((t ()))
  "The tooltip buffer common face.")

(defun entropy/sdcv-core-common-face-bgLight-color ()
  "Tooltip buffer background color for dark-theme."
  (face-attribute 'tooltip :background))

(defun entropy/sdcv-core-common-face-bgDark-color ()
  "Tooltip buffer background color for dark-theme."
  (face-attribute 'tooltip :background))

;;;; library
;;;;; file operation

(defmacro entropy/sdcv-core-return-as-default-directory (&rest body)
  "Return a valid `default-directory' value equalized with BODY's value.

This operation exists since `default-directory' has its meaningful
special constructed contention but most of times we did not obey thus
both of our neglects and misusing.

See `default-directory' for its convention details."
  (let ((dfd-sym (make-symbol "dfd-rtn-val")))
    `(let ((,dfd-sym (progn ,@body)))
       (unless (stringp ,dfd-sym)
         (signal 'wrong-type-argument
                 (list 'stringp
                       (format "directory name: %s" ,dfd-sym))))
       (unless (or (string-empty-p ,dfd-sym)
                   (not (directory-name-p ,dfd-sym)))
         (setq ,dfd-sym (directory-file-name ,dfd-sym)))
       (file-name-as-directory ,dfd-sym))))

(defun entropy/sdcv-core-list-dir-lite (dir-root &optional not-abs)
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

(defun entropy/sdcv-core-list-dir-subdirs (dir-root &optional not-abs)
  "List subdir of root dir DIR-ROOT, ordered by `string-lessp'.

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT."
  (let ((dirlist (entropy/sdcv-core-list-dir-lite dir-root not-abs))
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

;;;;; string operation

(defun entropy/sdcv-core-truncate-string-with-length (str length &optional line-indication)
  "Truncate string STR with length LENGTH rescursively and be
without destroying string struct.

Optional argument LINE-INIDICATION formed as a list of integers
to indicate which part (i.e. line indicator for buffer render
case) to truncate."
  (let (rtn
        (str-split (split-string str "\n"))
        (count 1)
        (fill-func
         (lambda (str length)
           (let ((fill-column length))
             (with-temp-buffer
               (insert str)
               (goto-char (point-min))
               (fill-paragraph)
               (buffer-substring
                (point-min) (point-max)))))))
    (dolist (el str-split)
      (push (if line-indication
                (if (member count line-indication)
                    (funcall fill-func el length)
                  el)
              (funcall fill-func el length))
            rtn)
      (cl-incf count))
    (cond ((> (length rtn) 1)
           (setq rtn (reverse rtn))
           (setq rtn (mapconcat (lambda (x)
                                  (when (not (string-match "\n$" x))
                                    (setq x (concat x "\n"))))
                                rtn nil)))
          (t (setq rtn (car rtn))))
    rtn))

(defun entropy/sdcv-core-get-string-max-width (str &optional max-indication)
  "Get string width module by `string-width'.

Without arg MAX-INDICATION sepcification will return the default
module plist as '(:max-width max-len :match-max-lines max-lines-number-list)'

With arg MAX-INDICATION indicated for, the return plist will
append of ':match-overflow overflow-len' and
':match-overflow-lines overflow-lines-number-list'.

String width module can be illustrated as

#+BEGIN_EXAMPLE
  String buffer displayed square:

  >  We set match-overflow to 7 chars

  lines:+----------------+
      1.|abcdefg         |
      2.|agcdsdfa        |<-------------------------overflow
      3.|absdfsdfsdfsf   |<-----max-width: 14
      4.|asdfsdfsdfa     |<-------------------------overflow
      5.|asdfasdfasdff   |<-----max-width: 14
        +----------------+

  Result: (:match-width 14 :match-max-lines (3 5) :match-overflow 7 :match-overflow-lines (2 4))
#+END_EXAMPLE
"
  (let (str-len-list str-attr-list str-split max-len rtn rtn-overflow (count 1))
    (setq str-split (split-string str "\n"))
    (cond
     ((= 1 (length str-split))
      (setq rtn (cons (string-width str) (list 1))))
     ((< 1 (length str-split))
      (dolist (el str-split)
        (cl-pushnew (cons count (string-width el)) str-attr-list :test 'equal)
        (cl-incf count))
      (dolist (el str-attr-list)
        (cl-pushnew (cdr el) str-len-list :test 'equal))
      (setq max-len (apply 'max str-len-list))

      (dolist (el str-attr-list)
        (when (equal (cdr el) max-len)
          (push (car el) rtn)))
      (when max-indication
        (let (overflows)
          (dolist (el str-len-list)
            (when (> el max-indication)
              (push el overflows)))
          (dolist (el str-attr-list)
            (when (member (cdr el) overflows)
              (push (car el) rtn-overflow)))))

      (setq rtn (list :max-width max-len
                      :mactch-max-lines rtn))
      (when max-indication
        (setq rtn (append rtn (list
                               :match-overflow max-indication
                               :match-overflow-lines rtn-overflow))))))
    rtn))

(defun entropy/sdcv-core-concat-char (char-string times)
  "Concat char-string CHAR-STRING do times TIMES with.

CHAR was single type string as \"a\", and TIMES was number
positive."
  (let (rtn)
    (unless (and (stringp char-string)
                 (> times 1))
      (error
       "Char-string or times specification error!"))
    (dotimes (_ times nil)
      (if rtn
          (setq rtn (concat rtn char-string))
        (setq rtn char-string)))
    rtn))

(defun entropy/sdcv-core-chinese-string-p (string)
  "Return non-nil when string is an chinese string."
  (and (stringp string)
       (not (string-empty-p string))
       (string-match-p
        (format "\\cC\\{%s\\}" (length string))
        string)))

;;;;; theme, color refer

(defun entropy/sdcv-core-color-name-to-lab (color_name)
  "Transfer color COLOR_NAME to lab format."
  (let* ((Crgb (color-name-to-rgb color_name))
         (Clab (apply 'color-srgb-to-lab Crgb)))
    Clab))

(defun entropy/sdcv-core-frameBG-dark-or-light ()
  "Judge current emacs frame background color of dark or light type.

This function used human perception color module LAB's light
channel to analyze the background color human perception simply
dividing it as uper or lower than 50 count which the lower as
dark, otherwise as light.

For terminal ui emacs using, this function will report the error
because that some situation for those frame background was
non-detectivation."
  (let* ((CFbg (face-attribute 'default :background))
         (CFlab_l (car (entropy/sdcv-core-color-name-to-lab CFbg))))
    (if (> CFlab_l 55)
        'light
      'dark)))

;;;;; Coding system wrapper

(defmacro entropy/sdcv-core-coding-with-utf-8-ces (&rest body)
  "Do BODY within a utf-8 coding system environment."
  `(let* ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     ,@body))

(defmacro entropy/sdcv-core-coding-with-locale-ces (&rest body)
  "Do BODY within a utf-8 coding system environment."
  `(let* ((coding-system-for-read (car default-process-coding-system))
          (coding-system-for-write (cdr default-process-coding-system)))
     ,@body))

;;;;; string obtains
(defun entropy/sdcv-core-get-word-or-region (&optional not_check_region)
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (entropy/sdcv-core--jieba-installed)
  (if (and mark-active
           (not not_check_region))
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point
     (if (and entropy/sdcv-core-get-thing-with-lang_zh_CN-p
              (eq entropy/sdcv-core--jieba-test-result 'installed))
         'chinese-or-other-word
       'word)
     t)))

;;;;; query rebuit

(defun entropy/sdcv-core-query-rebuit (str)
  "Rebuilt the query string for be suits as the sdcv receive type
which can be reducing unmatching probabilities.

Note: now this func was under-development and just simply
downcase the query string."
  (let (rtn)
    (setq rtn (downcase str))
    (setq entropy/sdcv-core-query-log rtn)
    rtn))

;;;;; common face
(defun entropy/sdcv-core--automatic-faceSet ()
  (let ((Lbg_color (entropy/sdcv-core-common-face-bgLight-color))
        (Bbg_color (entropy/sdcv-core-common-face-bgDark-color))
        (rtn (list :bg nil :fg nil)))
    (cl-case (ignore-errors (entropy/sdcv-core-frameBG-dark-or-light))
      (dark (setq rtn (plist-put rtn :bg Lbg_color))
            (setq rtn (plist-put rtn :fg "goldenrod")))
      (light (setq rtn (plist-put rtn :bg Bbg_color))
             (setq rtn (plist-put rtn :fg "firebrick")))
      ((nil)
       (setq rtn (plist-put rtn :bg "black"))
       (setq rtn (plist-put rtn :fg "brightyellow"))))
    rtn))

(defun entropy/sdcv-core-gen-common-face ()
  (let ((spec (entropy/sdcv-core--automatic-faceSet))
        (inverse (face-attribute 'tooltip :inverse-video))
        bgcolor fgcolor)
    (if (or (not (eq inverse 'unspecified))
            (null inverse))
        (progn
          (setq bgcolor (face-attribute 'tooltip :foreground))
          (setq fgcolor (face-attribute 'tooltip :background)))
      (setq bgcolor (face-attribute 'tooltip :background))
      (setq fgcolor (face-attribute 'tooltip :foreground)))
    (set-face-attribute
     'entropy/sdcv-core-common-face
     nil
     :foreground (or fgcolor (plist-get spec :fg))
     :background (or bgcolor (plist-get spec :bg)))
    'entropy/sdcv-core-common-face))

(defun entropy/sdcv-core-common-propertize-feedback (feedback)
  (entropy/sdcv-core-gen-common-face)
  (propertize feedback 'face 'entropy/sdcv-core-common-face))

(defun entropy/sdcv-core-use-face (show-face &optional show-method)
  "Return a face or nil (i.e. no valid one can be return)
according to SHOW-FACE which is a valid face or a function to
return a valid face or nil. Use `tooltip' face defautly if above
filte return nil."
  (let ((solaire-p
         (and
          (or (bound-and-true-p entropy/emacs-solaire-mode)
              (bound-and-true-p solaire-mode))
          (facep 'solaire-tooltip-face))))
    (or (and (null show-face) nil)
        (and (facep show-face) show-face)
        (and (functionp show-face)
             (let ((face (funcall show-face show-method)))
               (when (facep face)
                 face)))
        ;; using solaire theme in `solaire-mode' enabled buffer
        (cond
         ((and solaire-p
               (not (member show-method
                            '(adjacent-common
                              minibuffer-common
                              popup))))
          (if (eq show-method 'pos-tip)
              'default
            'solaire-tooltip-face))
         (t
          'tooltip)))))

;;;;; query with backend

(defun entropy/sdcv-core-query-backend (query dict-backend-name show-method)
  (entropy/sdcv-core-gen-common-face)
  (let* ((backend (alist-get
                   dict-backend-name
                   entropy/sdcv-core-query-backends-register))
         (query-function (plist-get backend :query-function))
         (show-predicate (plist-get backend :show-predicate))
         (face (plist-get backend :show-face))
         (feedback (funcall query-function query show-method)))
    (list :feedback feedback
          :show-predicate show-predicate
          :show-face face)))

;;;;; show response

(defun entropy/sdcv-core-response-show (show-aspect)
  (let* ((show-instance (cdr show-aspect))
         (show-method (car show-aspect))
         (show-function
          (alist-get show-method
                     entropy/sdcv-core-response-show-frontends)))
    (funcall show-function show-instance)))

;;;;; query and show

(defun entropy/sdcv-core-query-and-show (query backend-name show-method)
  (let* ((show-instance (entropy/sdcv-core-query-backend query backend-name show-method))
         (show-aspect (cons show-method show-instance)))
    (entropy/sdcv-core-response-show show-aspect)))

;;;; modes
(defvar entropy/sdcv-core-adjacent-buffer-common-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'quit-window)
    keymap)
  "key-map for `entropy/sdcv-core-adjacent-buffer-common-mode'.")

(define-minor-mode entropy/sdcv-core-adjacent-buffer-common-mode
  "Common minor mode for adjacent `entropy-sdcv' buffer show."
  :init-value nil
  :keymap entropy/sdcv-core-adjacent-buffer-common-mode-map
  :global nil
  t)

;;; provide
(provide 'entropy-sdcv-core)
