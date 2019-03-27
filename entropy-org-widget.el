;;; File name: entropy-org-widget.el  ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentry:
;;
;; This packages was the basic library for expanding org apis and be based on
;; it. This packages provide the library for entropy-emacs's other self
;; packages.
;;
;;
;;
;;
;;
;; * Code:

(require 'org)

;; ** headline
(defun entropy/ow-extract-head-plist (query)
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


(defun entropy/ow-get-all-head-plist (query)
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


;; ** drawer
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




;; ** links refer
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

(:str xxx :desc xxx :link xxx)

Key :str was the substring matched for, :desc was link
description string matched or nil otherwise, :link as desc."
  (let (desc link rtn)
    (with-temp-buffer
      (insert bracket-link)
      (goto-char (point-min))
      (org-in-regexp org-bracket-link-regexp)
      (setq desc (when (match-end 3) (match-string-no-properties 3)))
      (setq link (match-string-no-properties 1)))
    (setq rtn (list :str (match-string-no-properties 0)
                    ;; (cond ((and desc
                    ;;             (stringp desc))
                    ;;        (concat "[[" link "][" desc "]]"))
                    ;;       (t (concat "[[" link "]]")))
                    :desc desc
                    :link link))
    rtn))


;; * provide
(provide 'entropy-org-widget)
