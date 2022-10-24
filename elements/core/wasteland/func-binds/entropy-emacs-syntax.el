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
`%s', thus using its FILTER and PROBE
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

;; ** Common syntax APIs

(defun entropy/emacs-syntax-class-to-char-at-point (&optional as-string)
  "Return `current-buffer's current `point's syntax class character.

If AS-STRING is non-nil then make that char as string and return
that string."
  (let ((rtn (syntax-class-to-char (syntax-class (syntax-after (point))))))
    (if as-string (char-to-string rtn)
      rtn)))

(defun entropy/emacs-syntax-skip-forward (syntax &optional lim nomove)
  "Same as `skip-syntax-forward' but return a list whose car is the first
buffer position (a integer number) whose character is not matched
SYNTAX or nil otherwise, `cadr' of non-nil when the movement stoped at
`eobp' and `caddr' of what `skip-syntax-forward' return.

When NOMOVE is non-nil then all the movements is wrapped with
`save-excursion' where there's no movement after call.

See also `entropy/emacs-syntax-skip-backward'."
  (let (tr a-pt)
    (entropy/emacs-save-excursion-when
      :when nomove
      (setq tr (skip-syntax-forward syntax lim)
            a-pt (point))
      (if (eobp) (list nil t tr)
        (if (save-excursion
              (forward-char) (= 0 (skip-syntax-backward syntax)))
            (list a-pt nil tr) (list nil nil tr))))))

(defun entropy/emacs-syntax-skip-backward (syntax &optional lim nomove)
  "Same as `skip-syntax-forward' but return a list whose car is the first
buffer position (a integer number) whose character is not matched
SYNTAX or nil otherwise, `cadr' of non-nil when the movement stoped at
`bobp' and `caddr' of what `skip-syntax-forward' return.

When NOMOVE is non-nil then all the movements is wrapped with
`save-excursion' where there's no movement after call.

See also `entropy/emacs-syntax-skip-forward'."
  (let (tr d-pt)
    (entropy/emacs-save-excursion-when
      :when nomove
      (setq tr (skip-syntax-backward syntax lim))
      (if (bobp) (list nil t tr)
        (if (save-excursion
              (backward-char) (setq d-pt (point))
              (= 0 (skip-syntax-forward syntax)))
            (list d-pt nil tr) (list nil nil tr))))))

(defun entropy/emacs-syntax-buffer-point-at-list-p
    (&optional filter nomove paren prev-first)
  "Return non-nil when `point' CPT of `current-buffer' is at a
SYN-LIST-REGION (a balanced parenthetical group presented as buffer
region whose car is a `(' syntax class character LSTART-CHAR's
position LSTART and cdr is a buffer position LEND after a `)' syntax
class charater LEND-CHAR.).

SYN-LIST-REGION is using current `syntax-table' to determine what
delimiters DLS are used for searching. Since DLS may have several
parenthetical character groups (e.g. \"(\" and \")\", \"[\" and \"]\" etc. ),
thus defaully LSTART-CHAR is \"(\" and \")\" for LEND-CHAR. A cons PAREN
if set, its car and cdr is used to replace them as using. Only
characater whose syntax class matched and it's `=' one of DLS is
recognized as matched, and it's a PRCHAR.

If FILTER is set, it should be a function which accept one argumet,
the PRCHAR's buffer position number to determine whether is should be
the final used one while return non-nil for ignore that PRCHAR as a
common character. FILTER can either be `t' which means ignore any
PRCHAR in comments and strings that both of them defined by current
`syntax-table'. FILTER should not do any movements or modifications or
you should use `save-excursion' at least for just movements.

If PREV-FIRST is non-nil, search from CPT to `bobp' i.e. the PREVS
type, otherwise to `eobp' i.e. the ENDS type. This used for user to
optimze the SYN-LIST-REGION founding speed in which case if the
LEND-CHAR is too many after CPT then its suggest for backward search
first where may have nearest `(' placed in already so that the search
is did just once.

PREV-FIRST will be ignored while CPT is already at a position of a
PRCHAR, that the search direction is determined by that PRCHAR where
using PREVS when its a LEND-CHAR and reverse otherwise. This did as a
internal automatically optimization.

The `current-buffer''s point will be moved to the final searched
place, i.e. to the LSTART-CHAR when PREVS matched or `bobp' when not
matched of thus, to the LEND-CHAR when ENDS matched or `eobp' when not
matched of thus. But be without movement when NOMOVE is non-nil.

The non-nil return is SYN-LIST-REGION."
  (when (eq filter t)
    (setq filter
          (lambda (x)
            (or (entropy/emacs-syntax-buffer-pos-at-> 'string  x)
                (entropy/emacs-syntax-buffer-pos-at-> 'comment x)))))
  (let* ((origin-pt (point))
         ;; disable comments ignorance for `scan-lists' in top-level
         ;; since we should respect user interface.
         (parse-sexp-ignore-comments nil)
         ;; the main iterator for searching loop
         mt-res
         (prl     (or (car-safe paren) ?\())
         (prr     (or (cdr-safe paren) ?\)))
         prchar nprchar
         (prchar-set-func
          (lambda nil
            (setq prchar  (if prev-first prl prr))
            (setq nprchar (if prev-first prr prl))))
         (main-syntax "^()")
         (nbump-func    (lambda nil (not (if prev-first (bobp) (eobp)))))
         (char-mtp-func (lambda (x c) (= (char-after x) c)))
         (bs-nfilter-func
          (lambda (x)
            "Ignore backslashed parenthetical delimiters."
            (if (= x (point-min)) t
              (if (not (funcall char-mtp-func (1- x) ?\\)) t
                (let* ((pm (point-min))
                       (st (- x 1)) (dt (- x pm)) (ct 0))
                  (if (= dt 1) nil
                    (catch :exit
                      (dotimes (i dt)
                        (if (funcall char-mtp-func (- st i) ?\\)
                            (cl-incf ct) (throw :exit nil))))
                    (not (cl-oddp ct))))))))
         (nfilter-func
          (lambda (x)
            (and (funcall bs-nfilter-func x)
                 (if filter (not (funcall filter x)) t))))
         (synp-func
          (lambda (x)
            (let ((syn-res (car-safe (memql (syntax-class (syntax-after x)) '(4 5)))))
              (when syn-res (if (= syn-res 4) 0 1)))))
         (prp-func (lambda (x &optional no-direc syn-check)
                     "Return the delimeter type when it's passed filter and is indeed
a delimeter whose corresponding char is `=' user spec."
                     (let (isp)
                       (and (if syn-check (funcall synp-func x) t)
                            (if no-direc
                                (or (and (funcall char-mtp-func x prl) (setq isp 0))
                                    (and (funcall char-mtp-func x prr) (setq isp 1)))
                              (and (funcall char-mtp-func x prchar)
                                   (setq isp (if (= prchar prl) 0 1))))
                            (funcall nfilter-func x)
                            isp))))
         (scl-func (lambda (x)
                     (entropy/emacs-save-excurstion-and-mark-and-match-data
                       (condition-case _err
                           (scan-lists x (if prev-first -1 1) 0)
                         (scan-error nil)))))
         (nscl-func (lambda (x)
                      (entropy/emacs-save-excurstion-and-mark-and-match-data
                        (condition-case _err
                            (scan-lists x (if prev-first 1 -1) 0)
                          (scan-error nil)))))
         (at-nprchar-p-func
          (lambda (x)
            (and (funcall char-mtp-func x nprchar)
                 (funcall prp-func x 'no-direct))))
         (mt-goto-func
          (lambda (mt)
            "Goto proper pos for next searching with respecting search
direction and match type."
            (when (car mt)
              (let ((prp-p (funcall prp-func (car mt) 'no-direct)))
                (goto-char (car mt))
                (if (= (funcall synp-func (point)) 1) (forward-char 1)
                  (if (and (not prev-first) (not prp-p)) (forward-char 1)))
                (and prp-p mt)))))
         (sfunc (lambda nil
                  "search and goto proper pos suitable for looping."
                  (funcall mt-goto-func
                           (funcall
                            (if prev-first #'entropy/emacs-syntax-skip-backward
                              #'entropy/emacs-syntax-skip-forward)
                            main-syntax))))
         (lo-func
          (lambda nil
            "The main looper."
            (while (and (not (setq mt-res (funcall sfunc)))
                        (funcall nbump-func)))
            mt-res))
         (tiktok-search-func
          (lambda nil
            "Search delimeter with direction by skip negative direction siblings.

This will enlarge the search speed by skiping all non necessary
sub-groups powered by `scan-lists'."
            (let* ((scl-mt-func
                    (lambda nil (funcall prp-func (if prev-first (point) (1- (point))))))
                   (tt-func
                    (lambda nil
                      (let ((fp mt-res) nt ntp (first-jump-p t) abort)
                        (while (and (not abort) fp (funcall at-nprchar-p-func (car fp)))
                          (if (not (if (entropy/emacs-syntax-buffer-pos-at-> 'comment (car fp))
                                       (setq nt (funcall scl-func (point)))
                                     (let ((parse-sexp-ignore-comments t))
                                       (setq nt (funcall scl-func (point))))))
                              (progn (unless prev-first (forward-char 1))
                                     (setq fp (funcall lo-func) first-jump-p nil))
                            (progn (goto-char nt) (setq ntp (funcall scl-mt-func)))
                            (if (and ntp first-jump-p (= (car fp) origin-pt)) (setq abort t)
                              (setq fp (funcall lo-func) first-jump-p nil))))
                        (setq mt-res fp)))))
              (unless mt-res (funcall lo-func))
              (when mt-res (funcall tt-func))))))
    (entropy/emacs-save-excursion-when
      :when nomove
      (let* ((top-res (entropy/emacs-syntax-skip-forward main-syntax nil 'nomove))
             (top-res-p (and (car top-res) (= (nth 2 top-res) 0)))
             (start-matched-p
              (and top-res-p (funcall prp-func (car top-res) 'no-direct)))
             final-scl the-start the-end)
        ;; determin whether origin point at a valid delimeter so that
        ;; we can directly judge its region.
        (if (not start-matched-p)
            (progn (funcall prchar-set-func)
                   ;; we must skip from the valid first search since
                   ;; the `skip-syntax-forward' will start at origin
                   ;; point where will cause infinitely looping.
                   (when top-res-p (unless prev-first (forward-char 1))))
          (setq mt-res top-res
                prev-first (if (= start-matched-p 1) t))
          (funcall prchar-set-func)
          (funcall mt-goto-func top-res))
        ;; first search
        (funcall tiktok-search-func)
        ;; engaged that the search result valid or did again until yes
        ;; or return nil.
        (entropy/emacs-when-let*-first
            ((mt-res)
             (fnscl-get-func
              (lambda nil (setq final-scl (funcall nscl-func (point)))))
             (fnscl-prp-func
              (lambda nil
                (funcall prp-func (if prev-first (1- final-scl) final-scl) 'no-direct)))
             (fnscl-good-func
              (lambda nil
                (let ((parse-sexp-ignore-comments
                       (not (entropy/emacs-syntax-buffer-pos-at-> 'comment
                                (if prev-first (point) (1- (point)))))))
                  (and (funcall fnscl-get-func)
                       (funcall fnscl-prp-func))))))
          (while (and
                  (not
                   (and mt-res
                        (funcall fnscl-good-func)
                        (progn (setq the-start (point) the-end final-scl)
                               (entropy/emacs-swap-two-places-value the-start the-end
                                 (> the-start the-end)) t)
                        (<= the-start origin-pt) (> the-end origin-pt)))
                  (progn (setq the-start nil the-end nil) t)
                  (funcall nbump-func))
            (if mt-res (setq mt-res nil))
            (funcall tiktok-search-func)))
        (when (and the-start the-end) (cons the-start the-end))))))

(defun entropy/emacs-syntax-get-top-list-region-around-buffer-point
    (&optional filter probe nomove paren prev-first)
  "Return non-nil when `point' of `current-buffer' has a top-level
context SYN-LIST-REGION (see
`entropy/emacs-syntax-buffer-point-at-list-p').  Return nil otherwise.

A top-level SYN-LIST-REGION is a SYN-LIST-REGION doesn't has any outer
groups wrapped around.

Optional arguments all has same meaning as
`entropy/emacs-syntax-buffer-point-at-list-p', except that for NOMOVE
and PREV-FIRST, where PREV-FIRST is just used in first region
obtaining procedure since this function use that region as base and
using `scan-lists' to get outer region repeatly untile non of that
i.e. the top is found; And if NOMOVE is omitted or nil then stop at
the top open parenthesis or `bobp' when nil returned.

PROBE when set it should be a function accept current got context
SYN-LIST-REGION as the only argument; it should return non-nil that
indicate that that SYN-LIST-REGION is considerred as the `top-level
LIST', or nil when not. PROBE should not do any movements or
modifications or you should use `save-excursion' at least for just
movements.

The non-nil return is the SYN-LIST-REGION."
  (entropy/emacs-when-let*-first
      ((breg (entropy/emacs-syntax-buffer-point-at-list-p
              filter t paren prev-first))
       (cons-func (lambda nil (cons (point) (scan-lists (point) 1 0))))
       (probe-func
        (lambda nil (when probe (funcall probe (funcall cons-func))))))
    (entropy/emacs-save-excursion-when
      :when nomove
      (goto-char (car breg))
      (if (funcall probe-func) breg
        (while (and (condition-case _err
                        (goto-char (scan-lists (point) -1 1))
                      (scan-error nil))
                    (not (funcall probe-func))))
        (funcall cons-func)))))

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
      (entropy/emacs-with-current-buffer buff
        :unless (eq buff (current-buffer))
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
