;;; entropy-org-widget.el --- The org widget for entropy-emacs
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-org-widget
;; Package-Version: v0.1.1
;; Created:       2018
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (org "9.1"))
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
;; - Headline
;;
;;   Functions for extending Org heads dealing with.
;;
;; - Drawer
;;
;;   Functions for extending Org drawer dealing with.
;;
;; - Link
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
;;; Changelog:

;; - [2020-06-22 Mon 22:01:26] context update and bugs fix & release 0.1.1

;;; Code:

(require 'org)

;;;; Obsolete definatioin

;;;; Headline
(defun entropy/ow-extract-head-alist (&optional query)
  "Return the properties's list of current headline according to the
queries QUERY at current context in `current-buffer' of point
`point' with `save-excursion' or nil if non matches.

This functioin used org-mode api `org-entry-properties'.

Arg QUERY was formed as one strings list, each string was the
property key which you want to query about, for example:

Query: '(\"ITEM\" \"CUSTOM_ID\")
             |
             |
             v
Rerurn: ((\"ITEM\" \"Hello World\") (\"CUSTOM_ID\" \"h-4fdbe921-ab82-4c35-abb6-35078b138826\"))

QUERY was optional and return all properties when its omitted or
nil.

This function's return value was abided the origin key sequence
orderd in QUERY.
"
  (let* (rlist
         (pl (org-entry-properties)))
    (if query
        (dolist (elm query)
          (dolist (el pl)
            (when (string= (car el) elm)
              (add-to-list 'rlist `(,elm ,(cdr el)) t))))
      (dolist (el pl)
        (add-to-list 'rlist `(,(car el) ,(cdr el)) t)))
    rlist))

(defun entropy/ow-get-all-head-alist (&optional query)
  "Return current buffer's all headline properties list powered
by `entropy/ow-extract-head-alist' according to queries QUERY
with `save-excursion' or return nil otherwise.

QUERY's form was abided by the arg of
`entropy/ow-extract-head-alist' and is optional.

The returned sequence was abided the org headline sequence order
within current buffer.
"
  (let (ret-list)
    (save-excursion
      (goto-char (point-min))
      ;; first org org headline matching
      (if (not (looking-at-p org-heading-regexp))
          (if (not (progn (org-next-visible-heading 1)
                          (if (looking-at-p org-heading-regexp) t nil)))
              (progn (message (format "Buffer '%s' don't have any org-headline!"
                                      (current-buffer)))
                     (setq  ret-list nil))
            (add-to-list 'ret-list (entropy/ow-extract-head-alist query) t))
        (add-to-list 'ret-list (entropy/ow-extract-head-alist query) t))
      ;; The rest org headline matching
      (while (progn (org-next-visible-heading 1)
                    (if (looking-at-p org-heading-regexp) t nil))
        (add-to-list 'ret-list (entropy/ow-extract-head-alist query) t)))
    (delete nil ret-list)))

(defun entropy/ow-extract-hlink ()
  "Extract headline link attribute and return that link as string at
current `point' with `save-excursion' or return nil otherwise.

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

Note this function just extracts the first link part of headline
and ignore the rest."
  (let* (($return nil))
    (save-excursion
      (forward-line 0)
      (if (looking-at org-heading-regexp)
          (let ((p1 (save-excursion
                      (forward-line 0)
                      (if (re-search-forward "\\(?:^\\*+?.*?\\[\\[\\)"
                                             (save-excursion (end-of-line) (point)) t)
                          (point)
                        nil)))
                (p2 (save-excursion
                      (forward-line 0)
                      (if (re-search-forward ".*?\\]\\["
                                             (save-excursion (end-of-line) (point)) t)
                          (progn
                            (forward-char -2)
                            (point))
                        nil))))
            (if (and p1 p2)
                (setq $return (buffer-substring-no-properties p1 p2)))
            $return)
        nil))))

(defun entropy/ow-mark-headline ()
  "Return region of headline block with `save-excursion'.

Return value was a region as '(p1 p2) or throw out an error when
current point is not at a headline part i.e. the any point of the
line the headline sticks.

Which 'p1' was the point at the beginning of current headline and
'p2' was the point which indicate to the end of the line which
was the -1 line forward by the next org headline or `point-max' in
which case there's no further more rest headline below current
one."
  (let* (p1 p2 rlist)
    (save-excursion
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
      rlist)))


(defun entropy/ow-delete-headline-block ()
  "Delete headline region, used `entropy/ow-mark-headline'."
  (let* ((p-pair (entropy/ow-mark-headline))
         (start (car p-pair))
         (end (nth 1 p-pair)))
    (delete-region start end)))

;;;; Drawer
(defun entropy/ow-set-drawer (drawer-list)
  "Set headline with the properties in `org-mode' specified by
DRAWER-LIST.

DRAWER-LIST was in construct as follow former:

'((\"CUSTOM_ID\" \"123456\")
  (\"CATEGORY\" \"TEST\")
   ...)

This function will be in turn to set properties by each key of
DRAWER-LIST."
  (dolist (el drawer-list)
    (org-set-property (car el) (nth 1 el))))

;;;; Link
(defun entropy/ow-analyze-link-simple (bracket-link)
  "Simply using regexp `org-bracket-link-regexp' to analyzing the
bracket link string BRACKT-LINK, and return the plist of which
indicating it's link properties for:

  (list :str str :des description :link link)

Key :str was the substring matched for or nil if non-match, :desc
was link description string matched or nil otherwise, :link is the
uri part of thus or nil otherwise."
  (let (str desc link rtn)
    (with-temp-buffer
      (goto-char (point-min))
      (insert bracket-link)
      (goto-char (point-min))
      (re-search-forward org-bracket-link-regexp nil t)
      (setq str (match-string-no-properties 0)
            desc (match-string-no-properties 2)
            link (match-string-no-properties 1)))
    (setq rtn (list :str str
                    :desc desc
                    :link link))
    rtn))

(defun entropy/ow-get-str-links (str)
  "Get all bracket org links using regexp
`org-bracket-link-regexp' from string STR, sorted as the list of
links order as the occurence order in STR.

Each link collected as a plist return by
`entropy/ow-analyze-link-simple'.

Return nil when there's no matches in STR.
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

(defun entropy/ow-get-buffer-links (buffer)
  "Get all all bracket org link objects in current buffer using
`entropy/ow-get-str-links'."
  (let (buffer-str)
    (setq buffer-str
          (with-current-buffer buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (setq buffer-str
          (replace-regexp-in-string
           "\n" "" buffer-str))
    (entropy/ow-get-str-links buffer-str)))

;;; provide
(provide 'entropy-org-widget)
