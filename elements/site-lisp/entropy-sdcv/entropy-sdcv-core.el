;;; entropy-sdcv-core.el --- The core framework of entropy-sdcv
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
(require 'entropy-common-library)

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
;;;;; group
(defgroup entropy/sdcv-core-group nil
  "Group for `entropy-sdcv' feature."
  :group 'extensions)

;;;;; custom
(defcustom entropy/sdcv-core-source-language "zh-CN"
  "Mother tougue"
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-query-backends-register
  '()
  "List of dict type of quering for.

Each of the dict type represented as one DICT-BACKEND in
`entropy-sdcv.el', who is one alist whose car was the
DICT-BACKEND-NAME a symbol to indicate this dict type and the cdr is
plist of three slots, i.e. :query-function, :show-predicate and
:show-face, this plist called DICT-INSTANCE.

:query-function slotted one twwo argument function, the first
argument was string as dict quering, it return the FEEDBACK, and
the second was the SHOW-METHOD.

:show-predicate was the slot whose value was one function with two
parameters i.e. FEEDBACK and SHOW-METHOD.

:show-face slotted the showed string (the FEEDBACK) face or nil
means using default setting, or it is a funciton which return one
specified face with one argument of SHOW-METHOD.

FEEDBACK was string calling back from the specific DICT-BACKEND,
commonly non-propertized, but can be fontlocked by the Backend
itself, after all it must be a string which can be validated by
`stringp'.

SHOW-METHOD was a symbol for indicating how to show the FEEDBACK,
the default valid value are showed in
`entropy/sdcv-core-response-show-frontends' (see it for details).
"
  :type 'list
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
  :type 'list
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-null-prompt "In the beginning, there's darkness!"
  "Feedback string for none-matching session."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-query-log nil
  "Variable stored current query string for package debug using."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-log nil
  "Variable stored current query response string for package debug using."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-origin-lang-env (getenv "LANG")
  "Stored user origin specific env lang set."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-specific-lang "en_US.UTF-8"
  "Pre ordered system lang set used during sdcv query process."
  :type 'string
  :group 'entropy/sdcv-core-group)

(defcustom entropy/sdcv-core-response-column-width-max 60
  "Query feedback info definitions string overflow width used to
fill lines destructively or display with truncated occur."
  :type 'string
  :group 'entropy/sdcv-core-group)

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
;;;;; lang envrionment pre check
(defun entropy/sdcv-core-set-specific-lang-env ()
  (unless (equal entropy/sdcv-core-specific-lang
                 (getenv "LANG"))
    (setenv "LANG" entropy/sdcv-core-specific-lang)))

(defun entropy/sdcv-core-recovery-user-origin-lang-env ()
  (unless (equal entropy/sdcv-core-origin-lang-env
                 (getenv "LANG"))
    (setenv "LANG" entropy/sdcv-core-origin-lang-env)))

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
     (if (eq entropy/sdcv-core--jieba-test-result 'installed)
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
(defun entropy/sdcv-core-automatic-faceSet ()
  (let ((Lbg_color (entropy/sdcv-core-common-face-bgLight-color))
        (Bbg_color (entropy/sdcv-core-common-face-bgDark-color))
        (rtn (list :bg nil :fg nil)))
    (cl-case (ignore-errors (entropy/cl-frameBG-dark-or-light))
      (dark (setq rtn (plist-put rtn :bg Lbg_color))
            (setq rtn (plist-put rtn :fg "goldenrod")))
      (light (setq rtn (plist-put rtn :bg Bbg_color))
             (setq rtn (plist-put rtn :fg "firebrick")))
      (nil (setq rtn (plist-put rtn :bg "black"))
           (setq rtn (plist-put rtn :fg "brightyellow"))))
    rtn))

(defun entropy/sdcv-core-gen-common-face ()
  (let ((spec (entropy/sdcv-core-automatic-faceSet))
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
  (or (and (null show-face) nil)
      (and (facep show-face) show-face)
      (and (functionp show-face)
           (let ((face (funcall show-face show-method)))
             (when (facep face)
               face)))
      'entropy/sdcv-core-common-face))


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
