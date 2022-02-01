;;; entropy-unfill --- Unfill improvements for entropy-emacs
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-unfill
;; Package-Version: 20180606
;; Version:       0.1.0
;; Created:       2018
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (org "9.1") (entropy-common-library "0.1.0") (entropy-org-widget "0.1.0"))
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
;;
;; Is there some times that you regret for filling the whole buffer
;; with the specific column setting when you want to specific the new
;; filling column number?
;;
;; In this time the first thing you want is to unfill the whole
;; buffer and then determine the next operation.
;;
;; So this packages will provide you the automatically unfill whole
;; buffer function with the subtile constraints for unfill process.
;;
;; The core concept of the mechanism of this packages function was
;; using the specific flag of points pair list to separate the
;; buffer, you can quickly understand this with following work-flow:
;;
;; #+BEGIN_EXAMPLE
;;
;;          +---------------------------------------------+
;;          | +-----------------------------------------+ |
;;          | |  part 1                           t   --+-+--------------------->  part-1 of this buffer was paragraph and it will be convert to unfill.
;;          | |     xxxxxxxxxxxxxxxxxxxxxxxx            | |                        So the state of it was 't' .
;;          | +-----------------------------------------+ |
;;          | +-----------------------------------------+ |
;;          | |  part 2                                 | |
;;          | |   #+begin_src                     nil --+-+--------------------->  part-2 was code area and do not convert it, state 'nil.
;;          | |      ......                             | |
;;          | |   #+end_src                             | |
;;          | +-----------------------------------------+ |
;;          | +-----------------------------------------+ |
;;          | |  part 3                                 | |
;;          | |    - xxxxxx                       nil --+-+--------------------->  part-3 was list , so setting converted state to nil.
;;          | |    - xxxxxx                             | |
;;          | +-----------------------------------------+ |
;;          | +-----------------------------------------+ |
;;          | |  part 4                                 | |
;;          | |                                         | |
;;          | |   | xx | xx | xx |                      | |
;;          | |   |----|----|----|                nil --+-+--------------------->  part-4 was table formal so setting converted state to nil
;;          | |   | xx | xx | xx |                      | |
;;          | |   | xx | xx | xx |                      | |
;;          | |                                         | |
;;          | +-----------------------------------------+ |
;;          | +-----------------------------------------+ |
;;          | |  part 5                                 | |
;;          | |                                         | |
;;          | |       ...............               t --+-+--------------------->  part-5 was specific area which must be converted to.
;;          | |                                         | |
;;          | +-----------------------------------------+ |
;;          +---------------------------------------------+
;;
;; #+END_EXAMPLE
;;
;; So the first thing you should do before using this pacakge was to
;; setting the variable `entropy/unfill-special-line-regexp', and you
;; can using the default value of it without manually setting, the
;; default value was used for `org-mode' special region type, like
;; code-block, plain-list, headling .....
;;
;; And then you can directively using the function
;; `entropy/unfill-full-buffer-without-special-region' for unfill
;; full buffer.
;;
;;; Code:
;;;; require
(require 'entropy-common-library)
(require 'entropy-org-widget)
;;;; var declaration
(defvar-local entropy/unfill-special-line-description nil
  "This variable temporarily holded current-line's special line
type which matching with `entropy/unfill-special-line-regexp' and
automatically operated by
`entropy/unfill-judge-special-line-type', so any manually setting
for this will not be effective.")

(defvar entropy/unfill-special-line-regexp
  '(("Return" "^ *?\\\\\\\\" "$")
    ("local-variable-prop-line" "^ *# +-\\*-" "$")
    ("local-variable" "^ *\\(;;\\|#\\) Local Variables:" "^ *\\(;;\\|#\\) End:")
    ("option" "^ *#\\+.*?:" "$")
    ("src" "^ *#\\+\\(begin\\|BEGIN\\)" "^ *#\\+\\(end\\|END\\)")
    ("prp" "^ *:PROPERTIES:" "^ *:END:")
    ("head" "^\\*+ " "$")
    ("list" "^ *\\(-\\|\\+\\|[0-9]+)?\\.?\\) " "$")
    ("table" "^ *|" "|$"))
  "
Provide the special region regexp model.

Every special region has the BEGIN and END flag, and both of them
are in regexp description (use regexp to express them).

This variable is pair of list, which contain the element are list
too. Every unit pair list is consisted by three atoms:
- model name
- begin match regexp
- end match regexp

The default value of it was the special region type of
`org-mode', you can changing or adding element of it manually ro
create one personal function to toggle the various kind of
setting of it.
")

(defvar-local entropy/unfill-feature-region-pair-points-list nil
  "Store buffer feature regions with it's begin and end point
and the state variable for judging whether unfill it.

It's coming automatically with function
`entropy/unfill-recorde-feature-region-pair-points-in-full-buffer'
so do not setting manually (it's no effection).
")


;;;; function
;;;;; judge special line type
(defun entropy/unfill-judge-special-line-type ()
  "Judge the line type and return the type description.

This function move the current-point to the beginning of line
within execution of progress and keep the point be init state in
end of progress."
  (setq entropy/unfill-special-line-description nil)
  (let ((opoint (point))
        (pe (funcall (lambda () (end-of-line) (point))))
        (count 0)
        (des-temp '()))
    (forward-line 0)
    (dolist (re entropy/unfill-special-line-regexp)
      (let ((redes (car re))
            (regexp (car (cdr re))))
        (if (re-search-forward regexp pe t)
            (progn
              (add-to-list 'des-temp redes)
              (setq count (+ 1 count))))
        (forward-line 0)))
    (if (and (> count 1) entropy/unfill-special-line-description)
        (setq entropy/unfill-special-line-description (car (reverse des-temp)))
      (setq entropy/unfill-special-line-description (car des-temp)))
    (goto-char opoint)))

;;;;; jump to special end
(defun entropy/unfill-jump-to-special-line-target-end ()
  "Jump to the end flag of the matched beginning flag of special
  line regexp."
  (let ((re-end
         (entropy/unfill-match-special-line-target-end-regexp entropy/unfill-special-line-description)))
    (re-search-forward re-end nil t)
    (when (not (string-match-p "$" re-end))
      (left-char))))

(defun entropy/unfill-match-special-line-target-end-regexp (redes)
  "According to current line special line type description which
obtain by `entropy/unfill-special-line-description' to determined
the end-flag regexp."
  (let (entropy/return)
    (dolist (re entropy/unfill-special-line-regexp)
      (let ((redes-c (car re))
            (re-end-c (nth 2 re)))
        (if (string= redes redes-c)
            (setq entropy/return re-end-c))))
    entropy/return))

;;;;; unfill region recorde
(defun entropy/unfill-recorde-feature-region-pair-points ()
  "From point of current line, detectiving the regions scope for
used by
`entropy/unfill-recorde-feature-region-pair-points-in-full-buffer'
based on current line."

  ;; judge the current line special type
  (entropy/unfill-judge-special-line-type)

  ;; return pair points of region which according current line type.
  (if entropy/unfill-special-line-description

      ;; State of line with special line type
      (let ((p1 (point))
            p2)
        (entropy/unfill-jump-to-special-line-target-end)
        (setq p2 (point))
        (add-to-list 'entropy/unfill-feature-region-pair-points-list `(,p1 ,p2 nil))
        (goto-char p1))

    ;; State of common type line
    ;;
    ;; common line type was the part of common text region which be mixed witin special region, so
    ;; when at the common line, we should to find the end flag of common text region which was just
    ;; the recently special line where is.
    (let ((p1 (point))
          p2
          ptemp
          plist
          plist-positive)
      (dolist (re entropy/unfill-special-line-regexp)
        (goto-char p1)
        (if (re-search-forward (car (cdr re)) nil t)
            (progn
              (re-search-backward (nth 1 re) nil t)
              (setq ptemp (- (point) 1))
              (add-to-list 'plist ptemp))
          (add-to-list 'plist (point-max))))
      (setq plist-positive (sort plist '<)) ;sort point sequence for finding the recently one
      (goto-char (car plist-positive))
      (setq p2 (point))
      (add-to-list 'entropy/unfill-feature-region-pair-points-list `(,p1 ,p2 t))
      (goto-char p1))))

;;;;;; recorde all unfill region in buffer
(defun entropy/unfill-recorde-feature-region-pair-points-in-full-buffer ()
  "Recorde all unfill region pair points list within buffer

  This function return the pair-points list with reversed sequenced against with buffer line number
  because if we first unfill the region on the positive way then it will destory the following
  region points records' actual meaning that the rest buffer point state has been changed thus
  previous operation."
  (setq entropy/unfill-feature-region-pair-points-list nil)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (entropy/unfill-recorde-feature-region-pair-points)
    (let* ((pre (car entropy/unfill-feature-region-pair-points-list))
           (p2 (nth 1 pre)))
      (if (equal p2 (point-max))
          (goto-char p2)
        (goto-char (+ p2 1))))))


;;;;; major-mode chagne wrapper

(defun entropy/unfill--calling-major-mode (mode)
  "Calling major-mode MODE with preserved `fill-column' from
previous `major-mode'."
  (let ((fcol fill-column))
    (funcall mode)
    (setq fill-column fcol)))

;;;;; main function
;;;###autoload
(defun entropy/unfill-full-buffer-without-special-region ()
  "Unfill full buffer with specific special region type that
except for unfilling."
  (interactive)
  (let ((cur_major-mode major-mode))
    (if buffer-read-only (setq buffer-read-only nil))
    (entropy/unfill--calling-major-mode 'fundamental-mode)
    (entropy/unfill-recorde-feature-region-pair-points-in-full-buffer)
    (dolist (el entropy/unfill-feature-region-pair-points-list)
      (let ((start (car el))
            (end (nth 1 el))
            (unfill-p (nth 2 el)))
        (if unfill-p (entropy/unfill-region start end))))
    (entropy/unfill--calling-major-mode cur_major-mode)
    (setq buffer-read-only t)))


(defun entropy/fill-full-buffer-without-special-region ()
  "fill full buffer with specific special region type that
except for unfilling."
  (interactive)
  (let ((cur_major-mode major-mode))
    (if buffer-read-only (setq buffer-read-only nil))
    (entropy/unfill--calling-major-mode 'fundamental-mode)
    (entropy/unfill-recorde-feature-region-pair-points-in-full-buffer)
    (dolist (el entropy/unfill-feature-region-pair-points-list)
      (let ((start (car el))
            (end (nth 1 el))
            (fill-p (nth 2 el)))
        (cl-case cur_major-mode
          (org-mode
           (when fill-p
             (let ((substr (buffer-substring-no-properties start end)))
               (if (string-match org-bracket-link-regexp substr)
                   (entropy/fill-buffer-replace-region-org-link start end)
                 (fill-region start end)))))
          (t (when fill-p (fill-region start end))))))
    (entropy/unfill--calling-major-mode cur_major-mode)
    (setq buffer-read-only t)))

(defface entropy/fill-buffer-org-link-indicated-face
  '((t (:foreground "white" :background "red" :bold t)))
  "Face for indicating org link for filled with.")

(defun entropy/fill-buffer-replace-region-org-link (beg end)
  "Fill buffer region which contained the org bracket link,
replacing links with it's description before filling, recoverying
for the origin link string after filling so.

This operation was effective for preventing filling procedure
break as recognizing link type as the common word slot, thus, in
this func's process replacing org bracket link as it's
description string for preventing so."
  (let ((links (entropy/ow-get-str-links (buffer-substring-no-properties beg end))))
    (entropy/unfill--calling-major-mode 'fundamental-mode)
    (when links
      (goto-char beg)
      (dolist (el links)
        (let* ((str_get (plist-get el :str))
               (desc_get (plist-get el :desc))
               (link_get (plist-get el :link))
               (cbk (entropy/cl-replace-buffer-substr-with-face
                     beg end org-bracket-link-regexp
                     (lambda (x)
                       (or desc_get
                           " "))
                     'entropy/fill-buffer-org-link-indicated-face
                     1)))
          (setq end (cdr (plist-get cbk :pos-pair)))))
      (fill-region beg end)
      (entropy/cl-buffer-substr-replace-face-el
       beg nil 'entropy/fill-buffer-org-link-indicated-face
       (lambda (p_match pmax subobj step_i)
         (let* ((subpos (plist-get subobj :subpos))
                (substr (plist-get subobj :substr))
                (link_obj (nth (1- step_i) links))
                (link (plist-get link_obj :link))
                (repstr (cond ((not (equal substr " "))
                               (concat "[[" link "][" substr "]]"))
                              ((equal substr " ")
                               (concat "[[" link "]]")))))
           (cl--set-buffer-substring (car subpos) (1+ (cdr subpos))
                                     repstr)
           (setf (symbol-value p_match) (+ (symbol-value p_match)
                                           (string-width repstr)))))
       nil))))

;;;;;; unfill region
;;;###autoload
(defun entropy/unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;;;;; unfill paragraph
;;;###autoload
(defun entropy/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


;;; provide
(provide 'entropy-unfill)
