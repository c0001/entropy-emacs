;;; entropy-org-batch-refile.el --- Org batch refile simple interactivation  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-org-batch-refile
;; Package-Version: 0.1.0
;; Version:       file-version
;; Created:       2018
;; Compatibility: GNU Emacs 25;
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
;;; Commentary:
;;;; Preamble
;;
;; In org mode, internals heading taggning system give the heads tree
;; filter and grouped way for searching both be in current buffer and be
;; within searching by agenda utilities.  But be with the increasing of
;; counts of heads, heading refile (powerd by ~org-refile~) is poor of
;; the effeciency of it's manually way for the case that many of us need
;; to refile dozens of heading by the rule set of org heading properties
;; and tag.
;;
;; Thus why this package named as what you see here. This package gives
;; you the try for refile bounds of headings ruled by what your
;; specification of tag (_til now haven't do with the heading properties
;; batch refile implements_), called =batch refile=.
;;
;;;; Requirements
;;
;; =Org= only.
;;
;;;; Installation
;;
;; The =use-package= configuration managements style:
;; #+BEGIN_SRC emacs-lisp
;;   (use-package entropy-org-batch-refile
;;     :ensure nil
;;     :load-path "path-to-your-load-path"
;;     :commands entropy/org-batch-refile-tags-read-and-do)
;; #+END_SRC
;;
;;;; Interactions
;;
;; #+BEGIN_QUOTE
;; The properties matching refile will be implemented soon.
;; #+END_QUOTE
;;
;; Call func ~entropy/org-batch-refile-tags-read-and-do~.
;;
;;;; Apis
;;
;; There's several internal course can be exposed for elisp developer
;; using for. The mains of them two user_target_module and the macro
;; ~entropy/org-full-buffer-tag-match-refile~.
;;
;;
;;;;; User target model
;;
;; For the way of defining refilling target location, this package gives
;; the data structer for denoted where and how you would like to refile
;; to, that which I called =user_target_module=. The user_target_module
;; is simple and comprehensive for as is. The data type of it was one
;; cons cell which the car of it was the main file and heading location
;; introduction and the cdr of it was the instance of the car of it.
;;
;; For more detailes as one demo as: =((t/nil 1/2/3) head-string file-path)=
;;
;; - *car:* =(list t/nil 1/2/3)=
;;
;;   Car 't or nil' indicate whether using exits file as refile target
;;   file specification. Cdr '1 or 2 or 3' shows for the case of the
;;   type indication target of refiling head for as three cases:
;;
;;   1) exites org head in target file
;;   2) function auto-built head string which will be create as for
;;   3) user specified head string which will be create as for
;;
;; - *cdr:* =(list head-string file-path)=
;;
;;   The cdr of =user_target_module= value follow the car type, that the
;;   head-string required by the case that head specification case 3
;;   has been given and whatever target file existed, other wise it's
;;   must be nil. Arg file-path must be set when the file specification
;;   type was 't'.
;;
;;;;; Macro
;;
;; The core refiling course created macro
;; ~entropy/org-full-buffer-tag-match-refile~ accept two args
;; e.g. tag-matched and user_target_module, you can using it for the
;; secondary development. The lambda expression extracted by this was the
;; processing for that:
;;
;; 1. Analyzing user_target_module to located or create target location.
;; 2. Get org headings by the rule set of matching tags tag-matched
;;    specification and then refiling them all into the target.
;;
;;; TODO:
;;
;; - Improve `entropy/obr--auto-create-refile-file' auto-gen file name
;;   core function which will let sth like 'index.org<test-folder>'
;;   transfered to 'index.orgtest-folder' that we must using the
;;   further detailed category analyzation.


;;; Code:
;;;; require
(require 'org)
;;;; main function
;;;;; core libs

(defmacro entropy/org-refile--add-to-list (var value &optional append)
  `(unless (member ,value ,var)
     (if ,append
         (setq ,var (append ,var (list ,value)))
       (push ,value ,var))))

;;;;; main micro for tag match refile
(defmacro entropy/org-full-buffer-tag-match-refile (tag-regexp user-model)
  "The macro of instantiation of tag matched for `org-refile' function."
  `(if (eq major-mode 'org-mode)
       (progn
         (if buffer-read-only (setq buffer-read-only nil))
         (goto-char (point-min))
         (let* ((full-regexp (concat "^\\*+ .*" ,tag-regexp))
                (model-list (entropy/obr--target-instantiation-model ,user-model))
                (head-name (nth 1 model-list))
                (file-name (nth 0  model-list)))
           (while (re-search-forward full-regexp nil t)
             (entropy/org-refile-specific-target head-name file-name))))
     (user-error "entropy/obr: You can not use this function in %s" major-mode)))

;;;;; library for main
;;;;;; refile function
(defun entropy/org-refile-specific-target (headline file)
  "Refile headline into another file within it's specific
headline.

Idea from:
https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/
"
  (let* ((pos (save-excursion
                (find-file file)
                (setq buffer-read-only nil)
                (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

;;;;;; target model justify and return result function
;;;;;;; user-model instantiation function
(defun entropy/obr--target-instantiation-model (user-target-model)
  "
User-target-model is a list within 3 elements:

- model-type:

            this was one list contain two elements, they express
            the state of target for refilling.

            The sequence of this list were:

            + nil/t: whether specific the exists file

            + 1/2/3: whether specific the exists headline or let
                     this function automaticlly create it or
                     automaticlly create headline name with
                     headline name you specific.

- target head name (string): if the first elements was '(t 1)'
                             you can specific it, or you must set
                             it to nil.

- target file name (string): if first elements was '(t x)' you
                             can specific it, or you must set it
                             to nil.

This function return the a list of \"'(file head)\"
"
  (let ((model (car user-target-model))
        (head (nth 1 user-target-model))
        (file (nth 2 user-target-model))
        model-list)
    (cond
     ;; cond 1
     ((equal model '(t 1))
      (if (entropy/obr--justify-head-in-file head file)
          (progn (entropy/org-refile--add-to-list model-list head)
                 (entropy/org-refile--add-to-list model-list file))
        (error "entropy/obr: user-target-model invalid! model:t 1")))
     ;; cond 2
     ((equal model '(t 2))
      (if (file-exists-p file)
          (let* ((head-auto (entropy/obr--auto-create-headline-name))
                 (auto-hn (entropy/obr--headline-combine head-auto)))
            (with-current-buffer (find-file-noselect file)
              (if buffer-read-only (setq buffer-read-only nil))
              (goto-char (point-max))
              (newline 2)
              (forward-line 0)
              (insert auto-hn))
            (entropy/org-refile--add-to-list model-list head-auto)
            (entropy/org-refile--add-to-list model-list file))
        (error "entropy/obr: user-target-model invalid! model: t 2")))
     ;; cond 3
     ((equal model '(t 3))
      (if (and (file-exists-p file) head (equal (stringp head) t))
          (progn
            (with-current-buffer (find-file-noselect file)
              (if buffer-read-only (setq buffer-read-only nil))
              (goto-char (point-max))
              (newline 2)
              (forward-line 0)
              (insert (concat "* " head)))
            (entropy/org-refile--add-to-list model-list head)
            (entropy/org-refile--add-to-list model-list file))
        (error "entropy/obr: user-target-model invalid! model: t 3")))
     ;; cond 4
     ((equal model '(nil 2))
      (let ((auto-refile-model-list (entropy/obr--auto-create-refile-file t)))
        (entropy/org-refile--add-to-list model-list (car auto-refile-model-list))
        (entropy/org-refile--add-to-list model-list (nth 1 auto-refile-model-list))))
     ;; cond 5
     ((equal model '(nil 3))
      (let ((auto-refile-model-list (entropy/obr--auto-create-refile-file)))
        (if (and head (eq (stringp head) t))
            (progn
              (entropy/org-refile--add-to-list model-list head)
              (with-current-buffer (find-file-noselect (nth 1 auto-refile-model-list))
                (if buffer-read-only (setq buffer-read-only nil))
                (goto-char (point-max))
                (newline 2)
                (forward-line 0)
                (insert (concat "* " head))))
          (error "entropy/obr: user-target-model invalid! model: nil 3"))
        (entropy/org-refile--add-to-list model-list (nth 1 auto-refile-model-list))))
     ;; cond t
     (t (error "entropy/obr: user-target-model invalid! model: \"(cond t)\"")))
    model-list))

;;;;;;; justify whether head in file funciton
(defun entropy/obr--justify-head-in-file (HEAD FILE)
  "Justify whether headline HEAD in file FILE.

HEAD and FILE are both string type.

Final return t or nil accroding the state of result with
justifing.
"
  (let (return-p) ;; declare the return variable
    (if (file-exists-p FILE)
        (let ((buffer (find-file-noselect FILE)))
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (re-search-forward (regexp-quote HEAD) nil t)
                (progn (setq return-p t)
                       return-p)
              (progn (setq return-p nil)
                     return-p))))
      (progn (setq return-p nil)
             return-p))))

;;;;;;; auto create refile file function
(defun entropy/obr--auto-create-refile-file (&optional auto-insert-enable)
  " Auto create a refile file and alternatively for auto
generating refile head line.

It's return a list with head and file:

   (head file)"
  ;; adjusting file name accroding to current buffer file name
  (let* ((buffer (buffer-name))
         (invalid-regexp "\\(\\*\\|<\\|>\\)")
         refile-target-file-name
         (range-data (entropy/obr--range-data))
         (auto-headline-name (entropy/obr--auto-create-headline-name range-data))
         return-list)
    (if (string-match-p invalid-regexp buffer)
        (setq refile-target-file-name (concat (replace-regexp-in-string invalid-regexp "" buffer)
                                              "-refile"
                                              range-data
                                              ".org"))
      (setq refile-target-file-name (concat buffer "-refile" range-data ".org")))
    (with-temp-file (concat "./" refile-target-file-name)
      (if buffer-read-only (setq buffer-read-only nil))
      (if auto-insert-enable
          (progn
            (goto-char (point-max))
            (newline)
            (forward-line 0)
            (insert (entropy/obr--headline-combine auto-headline-name))
            (setq return-list `(,auto-headline-name ,refile-target-file-name)))
        (setq return-list `(nil ,refile-target-file-name))))
    return-list))

;;;;;;; auto create headline name function
(defun entropy/obr--auto-create-headline-name (&optional RANGE)
  "Generate one mechanical headline name and return it in string
type.

The optional arg RANGE was user specific range data for using as
the suffix data in head-name, nor will automatically create it
from `entropy/obr--range-data'.

The return string format like: \"* Refile-20180604_Mon_135151\""
  (let* ((range-data (entropy/obr--range-data))
         (auto-headline-name (concat "Refile" range-data))
         return-string)
    (if (and RANGE (equal t (stringp RANGE)))
        (setq return-string (concat "Refile" RANGE))
      (setq return-string auto-headline-name))
    return-string))

;;;;;;; range data function
(defun entropy/obr--range-data ()
  "Generate one ranger data for naming using and return it as
string.

The return string format is like: \"-20180604_Mon_135151\"."
  (let ((time (format-time-string "%Y%m%d_%a_%H%M%S"))
        return-data)
    (setq return-data (concat "-" time))
    return-data))

;;;;;;; headline hierachy combine
(defun entropy/obr--headline-combine (head-name &optional hierachy)
  " Combine headline name to completely full format of org's
headline sytle.

Optional arg 'hierachy' was the count of '*', and the default
count was 1.
"
  (let (return-head)
    (if (and hierachy (> hierachy 1))
        (progn
          (let ((count 0)
                (star-char ""))
            (while (<= count hierachy)
              (setq star-char (concat star-char "*"))
              (cl-incf count))
            (setq return-head (concat star-char " " head-name))
            return-head))
      (progn
        (setq return-head (concat "* " head-name))
        return-head))))

;;;; interactive funcion

;;;###autoload
(defun entropy/org-batch-refile-tags-read-and-do (tag-regexp)
  "Refile by specifying the tag matched with interactively envrionment."
  (interactive "sInput refile-tag-regexp: ")
  (entropy/org-full-buffer-tag-match-refile tag-regexp '((nil 2) nil nil)))


;;; provide
(provide 'entropy-org-batch-refile)
