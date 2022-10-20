;;; entropy-emacs-syntax --- eemacs syntax parse library  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20221020  bmsac0001@gmail.com
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           null
;; Package-Version: 0.1.0
;; Version:       0.1.0
;; Created:       2022-10-20 02:36:24
;; Keywords:      syntax, entropy-emacs, eemacs,
;; Compatibility: GNU Emacs 27;
;; Package-Requires: ((emacs "27") (cl-lib "0.5"))
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
;; =entropy-emacs= syntax parse common library.
;;
;; * Configuration:
;;
;; =entropy-emacs= used only.
;;
;; * Code
;; ** Internal Libs

(defun entropy/emacs-syntax--run-buffer-pos-pred (pred &optional result-func &rest args)
  (entropy/emacs-save-excurstion-and-mark-and-match-data
    (let ((rtn (when pred (apply pred args))))
      (if (functionp result-func) (funcall result-func rtn)
        rtn))))

(defun entropy/emacs-syntax--buffer-pos-face-memq-p (faces)
  (entropy/emacs-defun--group-memq-p
   ;; use `get-text-property' for ignore overlay which is considerred
   ;; as first member in `get-char-property' used by `face-at-point'.
   (get-text-property (point) 'face) faces))

(defvar entropy/emacs-syntax--buffer-pos-at-predicates-alist nil)
(cl-defstruct
    (entropy/emacs-syntax--buffer-pos-at-predicates-struct
     (:conc-name entropy/emacs-syntax--buffer-pos-at-predicates-struct->))
  (inaccuracy-p-pred nil :read-only t)
  (common-p-pred     nil :read-only t)
  (region-get-pred   nil :read-only t))

(defun entropy/emacs-syntax--gen-buffer-pos-at-predicate-name
    (region-type-sym pred-type)
  (cl-case pred-type
    (inaccuracy-pred
     (intern (format "entropy/emacs-syntax--buffer-pos-at-%s-region-p/inaccuracy"
                     region-type-sym)))
    (common-pred
     (intern (format "entropy/emacs-syntax--buffer-pos-at-%s-region-p"
                     region-type-sym)))
    (region-pred
     (intern (format "entropy/emacs-syntax--get-buffer-pos-%s-region"
                     region-type-sym)))
    (t (signal 'wrong-type-argument
               (list 'eemacs-syntax-pos-at-pred-type-p
                     pred-type)))))

(eval-and-compile
  (cl-defmacro entropy/emacs-syntax--gen-buffer-pos-at-predicates
      (region-type-sym &rest inaccurancy-p-body &key use-faces &allow-other-keys)
    (declare (indent 1))
    (setq inaccurancy-p-body (entropy/emacs-defun--get-real-body inaccurancy-p-body))
    (macroexp-let2* ignore
        ((type-sym region-type-sym)
         (inacc-func-name
          `(entropy/emacs-syntax--gen-buffer-pos-at-predicate-name ,type-sym 'inaccuracy-pred))
         (common-func-name
          `(entropy/emacs-syntax--gen-buffer-pos-at-predicate-name ,type-sym 'common-pred))
         (region-func-name
          `(entropy/emacs-syntax--gen-buffer-pos-at-predicate-name ,type-sym 'region-pred))
         (inacc-doc
          `(format "\
Return non-nil when `point' of `current-buffer' is at a syntactic `%s'
region in inaccuracy way."
                   ,type-sym))
         (common-doc
          `(format
            "\
Return non-nil when `point' of `current-buffer' is at a syntactic
`%s' region with user specific filter function FILTER
and probe function PROBE.

FILTER and PROBE are called without any arguments with `point' in
`current-buffer', and wrapped with
`entropy/emacs-save-excurstion-and-mark-and-match-data'.

If FILTER return non-nil, then POS is definitely not at that
region. If PROBE return non-nil, then POS is definitely at that
region.

If FILTER or PROBE is omitted or nil, they are always used as return
nil." ,type-sym))
         (region-doc
          `(format "\
Return a cons of buffer positions (cons of start and end) which is the
syntactic `%s' region in where `point' of `current-buffer' be.
Or nil when `point' is not satisfied `%s'.

The region detective is for each positions around `point' using
`%s', thus using its filter
as more accuracy specific.

FILTER and PROBE are used for
`%s' in all check steps." ,type-sym ,@(make-list 3 common-func-name))))
      `(progn
         (defalias ,inacc-func-name
           (lambda nil ,(entropy/emacs-macroexp-progn inaccurancy-p-body))
           ,inacc-doc)
         (defalias ,common-func-name
           (lambda (&optional filter probe)
             (and (entropy/emacs-syntax--run-buffer-pos-pred filter 'not)
                  (or (funcall ,inacc-func-name)
                      (and (bound-and-true-p font-lock-mode)
                           (entropy/emacs-syntax--buffer-pos-face-memq-p ,use-faces))
                      (entropy/emacs-syntax--run-buffer-pos-pred probe))))
           ,common-doc)
         (defalias ,region-func-name
           (lambda (&optional filter probe)
             (when (funcall ,common-func-name filter probe)
               (entropy/emacs-save-excurstion-and-mark-and-match-data
                 (let ((orig-pt (point)) str-beg str-end
                       (first-run-p t))
                   (entropy/emacs-map-buffer-points
                    (lambda nil
                      (unless (or (entropy/emacs-use-value-once first-run-p)
                                  (funcall ,common-func-name filter probe))
                        (setq str-end (point))
                        '(:exit t))))
                   (if (and (not str-end) (eobp)) (setq str-end (point)))
                   (goto-char orig-pt) (setq first-run-p t)
                   (entropy/emacs-map-buffer-points
                    (lambda nil
                      (unless (or (entropy/emacs-use-value-once first-run-p)
                                  (funcall ,common-func-name filter probe))
                        (setq str-beg (1+ (point)))
                        '(:exit t)))
                    :with-step -1)
                   (if (and (not str-beg) (bobp)) (setq str-beg (point)))
                   (if (and str-beg str-end) (cons str-beg str-end))))))
           ,region-doc)
         (setq entropy/emacs-syntax--buffer-pos-at-predicates-alist
               (assq-delete-all ,type-sym entropy/emacs-syntax--buffer-pos-at-predicates-alist))
         (add-to-list 'entropy/emacs-syntax--buffer-pos-at-predicates-alist
                      (cons ,type-sym
                            (make-entropy/emacs-syntax--buffer-pos-at-predicates-struct
                             :inaccuracy-p-pred ,inacc-func-name
                             :common-p-pred ,common-func-name
                             :region-get-pred ,region-func-name)))))))

;; ** POS at APIs
;; *** pos at string
(entropy/emacs-syntax--gen-buffer-pos-at-predicates 'string
  :use-faces '(font-lock-string-face font-lock-doc-face)
  (nth 3 (syntax-ppss)))

;; *** pos at comment
(defvar entropy/emacs-syntax--comment-faces
  '(font-lock-comment-face
    font-lock-comment-delimiter-face
    typescript-jsdoc-tag
    typescript-jsdoc-type
    typescript-jsdoc-value
    web-mode-block-comment-face
    web-mode-comment-face
    web-mode-css-comment-face
    web-mode-javascript-comment-face
    web-mode-json-comment-face
    web-mode-part-comment-face)
  "List of comment face.")

(defcustom entropy/emacs-syntax-not-support-docstring-modes
  '(c-mode c++-mode java-mode js-mode rust-mode rustic-mode typescript-mode)
  "A list of modes not support docstring."
  :group 'entropy-emacs-syntax
  :type 'list)

(defun entropy/emacs-syntax--comment-faces ()
  "Return comment faces of current mode."
  (append entropy/emacs-syntax--comment-faces
          (when (apply #'derived-mode-p
                       entropy/emacs-syntax-not-support-docstring-modes)
            '(font-lock-doc-face))))

(defun entropy/emacs-syntax--pos-at-comment ()
  "Return non-nil (face or t) if POINT at comment."
  ;; taken form `separedit--point-at-comment' from `separedit'.
  (let* ((curpt (point))
         (face (unless (entropy/emacs-syntax-buffer-pos-at-> 'string)
                 (get-text-property curpt 'face))))
    (when face
      (or (let ((comment-faces (entropy/emacs-syntax--comment-faces)))
            (cl-loop for f in (if (consp face)
                                  (when (face-list-p face) (reverse face))
                                (list face))
                     when (or (memq f comment-faces)
                              (memq (face-attribute f :inherit) comment-faces))
                     return f))
          (save-excursion
            (let ((state (syntax-ppss)))
              (and (nth 4 state)
                   (parse-partial-sexp (point) (point-max)
                                       nil nil state 'syntax-table)
                   t)))))))

(entropy/emacs-syntax--gen-buffer-pos-at-predicates 'comment
  (entropy/emacs-syntax--pos-at-comment))

;; * provide
;;;###autoload
(cl-defun entropy/emacs-syntax-buffer-pos-at->
    (case &optional position pred-type
          &key filter probe without-restriction)
  "Return non-nil when position (use POSITION or `point' of
`current-buffer') POS of buffer BUFF (use POSITION's `marker-buffer'
when it's a marker or `current-buffer') is at syntactic case CASE, or
nil for otherwise.

POSITION must satisfied
`entropy/emacs-buffer-position-p-plus' (WITHOUT-RESTRICTION has
same meaning as thus) with range check or an error is raised up.

For less commonly, CASE is a symbol represent a syntactic feature
which covers a buffer region REGION in where POS hosted and all
positions in that REGION are all at CASE. (CASE matching is END
excluded as emacs buffer region convention). CASE is valid as one of:
1) `string': syntactic string buffer region
2) `comment': syntactic comment buffer region

PRED-TYPE is a symbol indicate which predicate did as follows:
1) `common-p': return non-nil as true for that POS is at CASE, nil
   otherwise.
2) `region-get': return REGION consists by a cons of START and END
   buffer point, or nil if pre predicated by `common-p' return nil.

If PRED-TYPE is omitted or nil, use `common-p' defaultly.

If optional arguments FILTER and PROBE is set, both of them is a
function called at POS without any arguments requested by invoking,
and run with `entropy/emacs-save-excurstion-and-mark-and-match-data'.

If FILTER return non-nil, then POS is definitely not at that CASE. If
PROBE return non-nil, then POS is definitely at that CASE.

If FILTER or PROBE is omitted or nil, they are always used as return
nil. That is only internal syntactic pass mechanism will be used in
this case.

In generally and internally as, this function use emacs inernal
low-level syntax parser `parse-partial-sexp' to justify whether CASE
is matched at POS, but it's not always accuracy, since emacs doesn't
have comprehesive syntax API implementations. Thus all, this function
externally use `font-lock-mode' related faces at POS to do as
auxiliary probes. Thus, invoking this function in a correct
`major-mode' with `font-lock-mode' enabled is at least condition to
get accuracy result or specify your own FILTER and PROBE robust."
  (declare (indent 3) (side-effect-free t))
  (entropy/emacs-buffer-with-safe-pos position
    :do-error t :with-plus-checker t :with-range-check t
    :without-restriction without-restriction
    (let ((pt (plist-get position :point))
          (buff (plist-get position :buffer))
          (struct (alist-get case entropy/emacs-syntax--buffer-pos-at-predicates-alist)))
      (with-current-buffer buff
        (save-excursion
          (goto-char pt)
          (cl-case pred-type
            ((nil common-p)
             (funcall (entropy/emacs-syntax--buffer-pos-at-predicates-struct->common-p-pred
                       struct)
                      filter probe))
            (region-get
             (funcall (entropy/emacs-syntax--buffer-pos-at-predicates-struct->region-get-pred
                       struct)
                      filter probe))
            (t (signal 'wrong-type-argument
                       (list 'eemacs-syntax-pos-at-pred-type-p
                             pred-type)))))))))

(provide 'entropy-emacs-syntax)
