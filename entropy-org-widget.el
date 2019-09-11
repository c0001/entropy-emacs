;;; entropy-org-widget.el --- The org widget for entropy-emacs 
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-org-widget
;; Package-Version: v0.1.0
;; Created:       2018
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (org "9.1.3"))
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
;; Chains for with Org buffer or Org files. 
;;
;; Org self provides various APIs given for emacs developer to extended
;; Org as. This package was the one as what it is and with the defaultly
;; embedded into [[https://github.com/c0001/entropy-emacs][entropy-emacs]] situation gotten.
;;
;; Org was basically for dealing with the =Org= content format e.g. the
;; outlines, heading-tag, drawers, timestamp etc. This package divided
;; the functional sitemap with the concept follow that with the source
;; content outlines seperated for. For generally of currently used
;; categories for:
;;
;; - headline
;;
;;   Functions for extending Org heads dealing with.
;;
;; - drawer
;;
;;   Functions for extending Org drawer dealing with.
;;
;; - links refer
;;
;;   Thus as above.
;;
;;;; Requirements
;;
;; Org only.
;;
;;;; Installation
;;
;; Just downloading this package and then adding it to the load-path, the
;; last ~require~ it.
;; 
;;; Code:

(require 'org)

;;;; headline
(defun entropy/ow-extract-head-alist (query)
  "Return the properties' list of current headline according to
the queries QUERY.

This functioin used org-mode api `org-entry-properties'.

Arg QUERY was formed as one strings list, each string was the
property key which you want to query about, for example:

Query: '(\"ITEM\" \"CUSTOM_ID\") 
             |
             |
             v
Rerurn: ((\"ITEM\" \"Hello World\") (\"CUSTOM_ID\" \"h-4fdbe921-ab82-4c35-abb6-35078b138826\"))

*Note:*

This function's return value was abided the origin key sequence
orderd in QUERY.

"
  (let* (rlist
         (pl (org-entry-properties)))
    (dolist (elm query)
      (dolist (el pl)
        (when (string= (car el) elm)
          (add-to-list 'rlist `(,elm ,(cdr el)) t))))
    rlist))


(defun entropy/ow-get-all-head-alist (query)
  "Return current buffer's all headline properties list according
to queries QUERY.

QUERY's form was abided by the arg of
`entropy/ow-extract-head-plist'.

Return value's sequence was abided the org headline sequence
within current buffer.
"
  (let (ret-list)
    (goto-char (point-min))
    ;; first org org headline matching
    (if (not (looking-at-p org-heading-regexp))
        (if (not (progn (org-next-visible-heading 1)
                        (if (looking-at-p org-heading-regexp) t nil)))
            (error (format "Buffer '%s' don't have any org-headline!" (current-buffer)))
          (add-to-list 'ret-list (entropy/ow-extract-head-plist query) t))
      (add-to-list 'ret-list (entropy/ow-extract-head-plist query) t))
    ;; The rest org headline matching
    (while (progn (org-next-visible-heading 1)
                  (if (looking-at-p org-heading-regexp) t nil))
      (add-to-list 'ret-list (entropy/ow-extract-head-plist query) t))
    ret-list))


(defun entropy/ow-extract-hlink ()
  "Extract headline link attribute and return that link as string.

  For example:

  headline with link
  #+BEGIN_SRC org
    ,* [[file+sys:test-link][test-link]]
  #+END_SRC

  extract as: \"file+sys:test-link\".

  If headline not full with link format which means that as:
  #+BEGIN_SRC org
    ,* this is the part as not link format, [[file+emacs:test-link][this is link format part]], this is not as also.
  #+END_SRC

  the return is just the link part \"file+emacs:test-link\".

  Note this function just extrat the first link part of headline and
  ignore the rest."
  (let* (($return nil))
    (forward-line 0)
    (if (looking-at org-heading-regexp)
        (let ((p1 (save-excursion
                    (forward-line 0)
                    (if (re-search-forward "\\(?:^\\*+?.*?\\[\\[\\)" (save-excursion (end-of-line) (point)) t)
                        (point)
                      nil)))
              (p2 (save-excursion
                    (forward-line 0)
                    (if (re-search-forward ".*?\\]\\[" (save-excursion (end-of-line) (point)) t)
                        (progn
                          (forward-char -2)
                          (point))
                      nil))))
          (if (and p1 p2)
              (setq $return (buffer-substring-no-properties p1 p2)))
          $return)
      nil)))

(defun entropy/ow-mark-headline ()
  "Return points of headline block.

  Return value was a list of points as '(p1 p2):

  Which 'p1' was the point at the beginning of current headline and
  'p2' was the point which indicate to the end of the line which was
  the -1 line forward by the next org headline."
  (let* (p1 p2 rlist)
    (forward-line 0)
    (if (looking-at org-heading-regexp)
        (save-excursion
          (forward-line 0)
          (setq p1 (point))
          (end-of-line)
          (if (re-search-forward org-heading-regexp nil t)
              (progn
                (forward-line -1)
                (end-of-line)
                (setq p2 (point)))
            (setq p2 (point-max))))
      (error "You are not at headline."))
    (setq rlist `(,p1 ,p2))
    rlist))


(defun entropy/ow-delete-headline-block ()
  "Delete headline block, used `entropy/ow-mark-headline'."
  (let* ((p-pair (entropy/ow-mark-headline))
         (start (car p-pair))
         (end (nth 1 p-pair)))
    (delete-region start end)))


;;;; drawer
(defun entropy/ow-set-drawer (drawer-list)
  "Set headline with the properties in org-mode specified by list DRAWER-LIST.

DRAWER-LIST was in construct as follow former:

'((\"CUSTOM_ID\" \"123456\")
  (\"CATEGORY\" \"TEST\")
   ...)

This function will be in turn to set properties by the sequence of DRAWER-LIST's
car position."
  (dolist (el drawer-list)
    (org-set-property (car el) (nth 1 el))))




;;;; links refer
(defun entropy/ow-get-str-links (str)
  "Get all bracket org links using regexp
`org-bracket-link-regexp' from string sorted as the raw seqeunce
oder as the occurence order of their buffer position.
 "
  (let (rtn)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (push (entropy/ow-analyze-link-simple (match-string 0))
              rtn)))
    (setq rtn (reverse rtn))
    rtn))

(defun entropy/ow-analyze-link-simple (bracket-link)
  "Simply using regexp `org-bracket-link-regexp' to analyzing the
bracket link string BRACKT-LINK, and return the plist of which
indicating it's link properties for:

  (list :str str :des description :link link)

Key :str was the substring matched for, :desc was link
description string matched or nil otherwise, :link as desc."
  (let (str desc link rtn)
    (with-temp-buffer
      (goto-char (point-min))
      (insert bracket-link)
      (goto-char (point-min))
      (re-search-forward org-bracket-link-regexp nil t)
      (setq str (match-string-no-properties 0)
            desc (when (match-end 3) (match-string-no-properties 3))
            link (match-string-no-properties 1)))
    (setq rtn (list :str str
                    :desc desc
                    :link link))
    rtn))

(defun entropy/ow-get-buffer-links (buffer)
  (let (buffer-str)
    (setq buffer-str
          (with-current-buffer buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (setq buffer-str
          (replace-regexp-in-string
           "\n" "" buffer-str))
    (entropy/ow-get-str-links buffer-str)))

;;; obsolete definatioin
(define-obsolete-function-alias
  'entropy/ow-extract-head-plist
  'entropy/ow-extract-head-alist
  "0.1.1"
  "The origing =plist= naming was typo.")

(define-obsolete-function-alias
  'entropy/ow-get-all-head-plist
  'entropy/ow-get-all-head-alist
  "0.1.1"
  "The origing =plist= naming was typo.")

;;; provide
(provide 'entropy-org-widget)
