;;; File name: entropy-common-library.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This packages provides the common library for entropy-emacs self-packages
;; creation and also for common init configs.
;; 
;; * Code:
;; ** require
(require 'dash)
(require 'cl)
(require 'ivy)

;; ** Internal Functions
;;
;;    This part defined some functions used only for this package for
;;    providing middle functional utilities to other Apis.
;;
;;    
;; ** System environment checker
(defun entropy/cl-checking-system-utf8-supply ()
  "Checking operation system envrionment lanuguage support full
'utf-8' surroundings, it's useful for emacs process starting or
shell commands calling the multibyte args.

Return non-nil as supported as."
  (let (rtn)
    (cl-case system-type
      (windows-nt
       (if (and (eq w32-ansi-code-page 65001)
                (eq w32-system-coding-system 'utf-8))
           (setq rtn t)))
      (t
       (when (ignore-errors (string-match-p "UTF-8" (getenv "LANG")))
         (setq rtn t))))
    rtn))

;; ** elisp data type operation
;; *** list
;; **** list reverse
(defun entropy/cl-reverse-list (list)
  "reverse the sequence of one list and return the result but
  without replacing the original list LIST."
  (let* (rlist)
    (dolist (el list)
      (push el rlist))
    rlist))


;; **** list uniquely creator
(defun entropy/cl-make-identify-list (list)
  "Adding identifier for each element of list LIST and return new list.

Identifier now was the amount of string value type, the return list are like
follow former:

((0 el0) (1 el1) (2 el2) ...)

Each elements can be list or any single one."
  (let* (rlist (count 0))
    (dolist (el list)
      (push `(,(number-to-string count) ,el) rlist)
      (setq count (+ 1 count)))
    (setq rlist (entropy/cl-reverse-list rlist))))

(defun entropy/cl-extract-idlist (idlist se)
  "Id list produced by `entropy/cl-make-identify-list' was aimed to make every
  element be unique even there's two element has the same value.

  This function exract the id with one abbreviated elements info which was the
  sequenced sub-element in the element enetry of origin none-identified list.

  Thus like idlist:

  '((0 (\"hello\" \"vt.\"))
    (1 (\"happy\" \"adj.\"))
    (2 (\"apple\" \"n.\"))
    (3 (\"hello\" \"vt.\")))

  was the words with words-property list.

  This function was aimed to produce one string list for some emacs completion
  function like ivy to both show the id and abbrev info like:

  '(\"0:hello\"
    \"1:happy\"
    \"2:apple\"
    \"3:hello\")"
  (let* (rlist)
    (dolist (el idlist)
      (let* ((se-id (concat (car el) ":"))
             (subel (nth se (nth 1 el)))
             (full (concat se-id subel)))
        (push full rlist)))
    (setq rlist (entropy/cl-reverse-list rlist))))


;; **** list member checker
(defun entropy/cl-unique-list (unit list)
  "Justify whether has the unit in list LIST."
  (let (($return nil))
    (dolist (el list)
      (if (equal unit el)
          (setq $return t)))
    $return))

;; **** list make name alist
(defun entropy/cl-make-name-alist (olist naming-func)
  "Make name-alist from one pure vector OLIST. 

Function NAMEING-FUNC was provided by youself which has the
single argument to accepting one element of OLIST.

Demo:

If OLIST is (1 2 3 4 5), NAMING-FUNC is '(lambda (x) (+ 1 x))
then retun name-alist:

((2 . 1) (3. 2) (4. 3) (5. 4) (6. 5))
"
  (let* (rtn)
    (dolist (el olist)
      (push `(,(funcall naming-func el) . ,el)
            rtn))
    (if rtn
        rtn
      (error "entropy/cl-make-name-alist: Occur wrong!"))))

;; **** list capture region map
(defun entropy/cl-capture-list-by-region-map (list-var region-map)
  "Capture list LIST-VAR with region map REGION-TYPE and return
the grouped pair list type value.

Arg REGION-MAP:

This variable are list of integers (can be both single element or
multi ones). Integers summer must be equal or lessan than the
length of LIST-VAR.

Demo:

List var (1 2 3 4 5 6 7 8 9) maped by (1 2 3) will return
   
    ((1) (2 3) (4 5 6))."
  (let ((map-counter (apply '+ region-map))
        (list-len (length list-var))
        (counter 0) (list-pointer 0)
        temp-var rtn)
    (unless (<= map-counter list-len)
      (error "[entropy/cl-capture-list-by-map]: region-map overflow!"))
    (dolist (el region-map)
      (setq counter list-pointer
            temp-var nil)
      (setq list-pointer (+ el list-pointer))
      (dotimes (el2 el nil)
        (push (nth counter list-var) temp-var)
        (cl-incf counter))
      (push (reverse temp-var) rtn))
    (reverse rtn)))


;; *** alist
;; **** numberic alist sorting
(defun entropy/cl-sort-numberic-alist (numberic-alist &optional reverse only-cdr)
 "Sorting numberic alist which the car of each associate element
was numberic and return the sorted alist.

Optional arg:

REVERSE: using minmal start sort ordered type.

ONLY-CDR: Return the list of whose element was the origin
association's cdr."
  (let (numberic-list max-func rtn temp_var)
    (dolist (el numberic-alist)
      (unless (numberp (car el))
        (error "<<entropy/cl-sort-numberic-alist>>: 
Wrong type of argument: numberp '%s'" (car el)))
      (push (car el) numberic-list))
    (unless (= (length numberic-alist)
               (length (remove-duplicates numberic-list :test 'eq)))
      (error "<<entropy/cl-sort-numberic-alist>>: 
Duplicated numberic order!"))
    (setq max-func
          (lambda (number-seq)
            (let (rtn)
              (push (apply 'max number-seq) rtn)
              (unless (null (delete (car rtn) number-seq))
                (setq rtn
                      (append rtn
                              (funcall max-func (delete (car rtn) number-seq)))))
              rtn)))
    (setq rtn (funcall max-func numberic-list))
    (when reverse
      (setq rtn (reverse rtn)))
    (dolist (el rtn)
      (push (assoc el numberic-alist)
            temp_var))
    (cond ((not only-cdr)
           (reverse temp_var))
          (only-cdr
           (let (rtn)
             (dolist (el temp_var)
               (push (cdr el) rtn))
             rtn)))))

;; *** plist
(defun entropy/cl-plistp (plist-var)
  "Return non-nil while PLIST-VAR whether is plist type."
  (let ((temp_var (copy-tree plist-var)))
    (if (and (ignore-errors
               (plist-put temp_var :test_put "success"))
             (not (null plist-var)))
        t
      nil)))

(defun entropy/cl-plist-batch-put (plist-var list-var &optional form)
  "Batch put variable in LIST-VAR into plist by its origin
sequence and return it.

LIST-VAR's length must be the half of the length of plist-var."
  (unless (entropy/cl-plistp plist-var)
    (error "entropy/cl-plist-batch-put: plist-var is not plist"))
  (let* ((plist-len (length plist-var))
         (list-var-len (length list-var))
         plist-clauses
         plist-putted
         (counter-max (- plist-len 2))
         (counter 0))
    (unless (= (* 2 list-var-len) plist-len)
      (error "entropy/cl-plist-batch-put: plist-var and list-var length not equalized."))
    (while (<= counter counter-max)
      (push (nth counter plist-var) plist-clauses)
      (setq counter (+ 2 counter)))
    (setq plist-clauses (reverse plist-clauses))
    (setq counter 0)
    (cond
     ((or (null form)
          (not (functionp form)))
      (dolist (el plist-clauses)
        (push (list el (nth counter list-var))
              plist-putted)
        (cl-incf counter)))
     ((functionp form)
      (dolist (el plist-clauses)
        (let ((orig-val (plist-get plist-var el))
              new-var)
          (setq new-var (apply form (list orig-val (nth counter list-var))))
          (push (list el new-var) plist-putted)
          (cl-incf counter)))))
    (setq plist-putted (reverse plist-putted))
    (apply 'append plist-putted)))
  
(defun entropy/cl-get-plist-prop-pair (plist-var &optional prop)
  "Return alist type of PlIST-VAR whose element was pair list.

Return single pair list when optional argument PROP was non-nil
for which key matched of PROP in PLIST-VAR, or return nil when
not matched. "
  (unless (entropy/cl-plistp plist-var)
    (error "[entropy/cl-get-plist-prop-pair]: wrong type of argument 'plistp'!"))
  (let ((pro-list (entropy/cl-get-plist-prop-list plist-var))
        rtn)
    (dolist (el pro-list)
      (push (list el (plist-get plist-var el)) rtn))
    (setq rtn (reverse rtn))
    (cond
     (prop
      (setq rtn (assoc prop rtn)))
     ((null prop)
      rtn))))

(defun entropy/cl-get-plist-prop-list (plist-var)
  "Return list of all props of PLIST-VAR with its origin
sequence."
  (unless (entropy/cl-plistp plist-var)
    (error "[entropy/cl-get-plist-prop-list]: wrong type of argument 'plistp'!"))
  (let* (plist-props
         (counter 0)
         (plist-len (length plist-var))
         (counter-max (- plist-len 2)))
    (while (<= counter counter-max)
      (push (nth counter plist-var) plist-props)
      (setq counter (+ 2 counter)))
    (setq plist-props (reverse plist-props))))
;; *** string operation
(defun entropy/cl-get-string-lines (str)
  "Get string STR line count while displayed in buffer."
  (let (line-count)
    (with-temp-buffer
      (insert str)
      (goto-char (point-max))
      (setq line-count(line-number-at-pos (point)))
      (kill-buffer))
    line-count))
  
(defun entropy/cl-get-string-max-width (str &optional max-indication)
  "Get string width module with plist value return.

  Without arg MAX-INDICATION sepcification will return the default
  module plist as '(:max-width max-len :match-max-lines max-lines-number-list)'

  With arg MAX-INDICATION indicated for, the return plist will
  append of ':match-overflow overflow-len' and
  ':match-overflow-lines overflow-lines-number-list'.

  String width module can be illustrated as

  #+BEGIN_EXAMPLE
      String buffer displayed square:
                                 
    lines:+-------------+                
        1.|abcdefg      |<------match-overflow: 7         
        2.|agcd         |<-------------------------overflow     
        3.|absdfsdfsdfsf| <-----max-width: 14               
        4.|asdfsdfsdf   |<-------------------------overflow    
        5.|asdfasdfasdff| <-----max-width: 14              
          +-------------+                                  
                                                         
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
        (add-to-list 'str-attr-list (cons count (string-width el)))
        (cl-incf count))
      (dolist (el str-attr-list)
        (add-to-list 'str-len-list (cdr el)))
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

 (defun entropy/cl-get-current-line-string ()
 "Return current line buffer string within current buffer. "
 (let ((p-head (line-beginning-position))
       (p-end (line-end-position))
       rtn)
   (setq rtn (buffer-substring p-head p-end))
   rtn))


 (defun entropy/cl-concat-char (char-string times)
 "Concat char-string CHAR-STRING do times TIMES with.

 CHAR was single type string as \"a\", and TIMES was number
 positive."
 (let (rtn)
   (unless (and (stringp char-string)
                (> times 1))
     (error "<entropy/cl-concat-char>: char-string or times specification error!"))
   (dotimes (el times nil)
     (if rtn
         (setq rtn (concat rtn char-string))
       (setq rtn char-string)))
   rtn))


;; **** truncate string
(defun entropy/cl-truncate-string-with-length (str length &optional line-indication)
  "Truncate string STR with length LENGTH rescursively and be without
 destroying string struct. Using func
 `entropy/cl-truncate-single-line-string-with-length' for each line
 procedural dealing with."
  (let (rtn
        (str-split (split-string str "\n"))
        (count 1))
    (dolist (el str-split)
      (push (if line-indication
                (if (member count line-indication)
                    (entropy/cl-truncate-single-line-string-with-length el length)
                  el)
              (entropy/cl-truncate-single-line-string-with-length el length))
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

(defun entropy/cl-truncate-single-line-string-with-length (str length)
  "
Recursively truncate single line string STR with specified length
LENGTH similar to buffer operation `fill-region' but used in lisp
abstract and without destroying the string struct as:

Assuming that process string arg was 'abcdefg' with truncate
length '2', the result was:

'abcdefg'

Nothing happened for, because the string as that was recognized as
the word which couldn't be truncate for, if the str arg was 'abc
de fg' then the result will be:

'abc_ 
de_ 
fg_'

In this case, truncation was visualized and the place '_' was
where space char located.

For the all as above description, word will not be truncated even
if the last word's length offset over the arg LENGTH specified,
and any word's tail with space char sequence which at end of line
will not be inherited for the next line head, instead that will be
inserted for current line ending.

Single line string means that the string do not have any newline
char e.g. '\\n' for with."
  (let* (($char-width 0)
         head-part
         rest-part
         rtn (cnt 0)
         (list/concat-func (lambda (x) x))
         (str-list (split-string str "" t))
         (str-list-len (length str-list)))

    (while (not (> (+ cnt 1) str-list-len))
      (let* ((el (nth cnt str-list))
             (el-width (string-width el)))
        (setq $char-width (+ $char-width el-width))
        (cond
         ;; current char was multibyte
         ((or (> el-width 1) (string-match "[ \t]" el))
          (if (<= $char-width length)
              (push el head-part)
            (push el rest-part)))
         ;; current char was ascii
         ((and (= el-width 1) (not (string-match "[ \t]" el)))
          (if (<= $char-width length)
              (let ((cnt_temp_char (1+ cnt))
                    char-offset_char)

                (while (and (<= (1+ cnt_temp_char) str-list-len)
                            (not (string-match "[ \t]" (nth cnt_temp_char str-list))))
                  (push (nth cnt_temp_char str-list) char-offset_char)
                  (cl-incf cnt_temp_char))
                
                (push el head-part)
                
                (let ((offset_wsp (nth cnt_temp_char str-list))
                      (cnt_temp_wsp (1+ cnt_temp_char))
                      char-offset_wsp)
                  (when (and offset_wsp
                             (string-match "[ \t]" offset_wsp))
                    (push (nth cnt_temp_char str-list) char-offset_wsp)
                    (while (and (<= (1+ cnt_temp_wsp) str-list-len)
                                (string-match "[ \t]" (nth cnt_temp_wsp str-list)))
                      (push (nth cnt_temp_wsp str-list) char-offset_wsp)
                      (cl-incf cnt_temp_wsp))
                    (when char-offset_wsp
                      (setq char-offset_char (append char-offset_wsp char-offset_char)
                            cnt_temp_char cnt_temp_wsp))))
                
                (when char-offset_char
                  (setq head-part (append char-offset_char head-part)
                        cnt (1- cnt_temp_char)
                        $char-width (+ (length char-offset_char) $char-width))))
            (push el rest-part))))
        (cl-incf cnt)))
    
    (setq head-part (reverse head-part)
          rest-part (reverse rest-part))
    
    (cond
     ((and (not head-part)
           (not rest-part))
      (setq rtn ""))
     ((and (not head-part)
           rest-part)
      (setq rtn str))
     ((and head-part
           (not rest-part))
      (setq rtn str))
     ((and head-part rest-part)
      (setq head-part (mapconcat list/concat-func head-part nil)
            rest-part (mapconcat list/concat-func rest-part nil))
      (cond ((<= (string-width rest-part) length)
             (setq rtn (concat head-part "\n" rest-part)))
            ((> (string-width rest-part) length)
             (setq rtn (concat head-part "\n"
                               (entropy/cl-truncate-single-line-string-with-length
                                rest-part length)))))))
    rtn))

;; **** string replace

(defun entropy/cl-replace-buffer-substr-with-face (beg end regexp rep indicated-face times)
  "Replace buffer substring from BEG to END matched by regexp REGEXP
of indicated face INDICATED-FACE with specific string replaced
obtaining by replaced string or function REP.

If last arg TIMES non-nil and was interger larger than zero,
searching within using `dotimes' instead of `while' til the end of
substring ending END.

If REP was a funciton, it's arg was single of the string type
which was matching of regexp REGEXP. 
"
  (let ((ctn 0)
        match-list rtn
        (core_func_repfunc
         (lambda (beg_p end_p rex ctn_i cbk-list repfunc repface times_p)
           (cond
            ((numberp times_p)
             (dotimes (el times_p nil)
               (when (re-search-forward rex (symbol-value end_p) t)
                 (cl-incf (symbol-value ctn_i))
                 (let ((match-for (match-string 0))
                       (match-replace (propertize (funcall repfunc (match-string 0))
                                                  'face repface)))
                   (setf (symbol-value end_p)
                         (+ (symbol-value end_p)
                            (- (string-width match-replace)
                               (string-width match-for))))
                   (push (list :match-order (symbol-value ctn_i)
                               :match-for match-for
                               :match-replace match-replace)
                         (symbol-value cbk-list))
                   (goto-char beg_p)
                   (re-search-forward rex (symbol-value end_p) t (symbol-value ctn_i))
                   (replace-match match-replace)))))
            ((null times_p)
             (while (re-search-forward rex (symbol-value end_p) t)
               (cl-incf (symbol-value ctn_i))
               (let ((match-for (match-string 0))
                     (match-replace (propertize (funcall repfunc (match-string 0))
                                                'face repface)))
                 (setf (symbol-value end_p)
                       (+ (symbol-value end_p)
                          (- (string-width match-replace)
                             (string-width match-for))))
                 (push (list :match-order (symbol-value ctn_i)
                             :match-for match-for
                             :match-replace match-replace)
                       (symbol-value cbk-list))
                 (goto-char beg_p)
                 (re-search-forward rex (symbol-value end_p)  t (symbol-value ctn_i))
                 (replace-match match-replace)))))))
        
        (core_func_repstr
         (lambda (end_p rex ctn_i cbk-list rep-str repface times_p)
           (cond
            ((numberp times_p)
             (dotimes (el times_p nil)
               (when (re-search-forward rex (symbol-value end_p) t)
                 (cl-incf (symbol-value ctn_i))
                 (let ((match-for (match-string 0))
                       (match-replace (propertize rep-str 'face repface)))
                   (setf (symbol-value end_p)
                         (+ (symbol-value end_p)
                            (- (string-width match-replace)
                               (string-width match-for))))
                   (push (list :match-order (symbol-value ctn_i)
                               :match-for match-for
                               :match-replace match-replace)
                         (symbol-value cbk-list))
                   (replace-match match-replace)))))
            ((null times_p)
             (while (re-search-forward rex (symbol-value end_p) t)
               (cl-incf (symbol-value ctn_i))
               (let ((match-for (match-string 0))
                     (match-replace (propertize rep-str 'face repface)))
                 (setf (symbol-value end_p)
                       (+ (symbol-value end_p)
                          (- (string-width match-replace)
                             (string-width match-for))))
                 (push (list :match-order (symbol-value ctn_i)
                             :match-for match-for
                             :match-replace match-replace)
                       (symbol-value cbk-list))
                 (replace-match match-replace))))))))
    (goto-char beg)
    (cond
     ((functionp rep)
      (funcall core_func_repfunc
               beg 'end regexp 'ctn 'match-list rep indicated-face times))
     ((stringp rep)
      (funcall core_func_repstr
               'end regexp 'ctn 'match-list rep indicated-face times)))
    (setq rtn (list :pos-pair (cons beg end)
                    :buffer-substr (buffer-substring beg end)
                    :replace-module (reverse match-list)))
    rtn))


(defun entropy/cl-buffer-substr-replace-face-el (beg end indicated-face rep-func times)
  "Replace buffer substring which has the face property specified by
face INDICATED-FACE, for the replaced string retrieving from
REP-FUNC which receive for args: current buffer point symbol,
current substring end point symbol, matched string, and match step
integer.

If last arg TIMES non-nil and was interger larger than zero,
searching within using `dotimes' instead of `while' til the end of
substring ending END.

Replaced function's point type args are all symbol delivered from
parent process which will help main procedure's loop process ran
as regular form, that's the arg of p_cur will be reset within the
REP-FUN procedure which will calculate the end of replaced
substring ending POS and set it to be thus. Point arg max point
can be nil whill will always point to the buffer `point-max', the
otherwise for should calculated within REP-FUN's procedure as pont
current arg did as well, in this case, it must always be setted to
the new end of current buffer substring changed from the origin
BEG to END. 

To giving the explicitly process for return the accurate new
whole substring end will enhancing the procedure performance
because it's will reduce the searching scope for replacing next
against the nil case which will always searching to the bound
ending of buffer ending. "
  (let* ((p_cur beg)
         (p_max (or end
                    (point-max)))
         (p_max_last p_max)
         (step 0)
         (keep-func
          (lambda (orig-p face_spec)
            (let* ((p_temp orig-p)
                   rtn)
              (when (equal (get-char-property (1+ orig-p) 'face)
                           face_spec)
                (cl-incf p_temp)
                (while (equal (get-char-property (1+ p_temp) 'face)
                              face_spec)
                  (cl-incf p_temp)))
              (if (not (equal p_temp orig-p))
                  (setq rtn (list :subpos (cons orig-p p_temp)
                                  :substr (buffer-substring-no-properties orig-p (1+ p_temp))
                                  :face face_spec))
                (setq rtn (list :subpos (cons orig-p p_temp)
                                :substr (buffer-substring-no-properties orig-p (1+ orig-p))
                                :face face_spec)))
              rtn))))
    (catch :exit
      (while (<= p_cur p_max)
        (if (equal (get-char-property p_cur 'face)
                   indicated-face)
            (progn
              (cl-incf step)
              (funcall rep-func 'p_cur 'p_max (funcall keep-func p_cur indicated-face) step)
              (when (and (not end)
                         (= p_max p_max_last))
                (setq p_max (point-max)
                      p_max_last p_max))
              (cond
               ((null times)
                t)
               ((and (numberp times)
                     (< step times))
                t)
               ((and (numberp times)
                     (>= step times))
                (throw :exit nil))))
          (cl-incf p_cur))))
    (setq rtn (buffer-string))
    rtn))
;; ** file and dir operation
;; *** dir
;; **** list dir
(defun entropy/cl-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\.$\\|\\.\\.$\\)" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))


(defun entropy/cl-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (entropy/cl-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))


(defun entropy/cl-list-dir-files-full (root-dir)
  "List all files recursively under root directory ROOT-DIR and
return the path list. "
  (let* ((cur_get (entropy/cl-list-dir-lite root-dir))
         $dirs $files $childs rtn)
    (when (not (null cur_get))
      (dolist (el cur_get)
        (cond
         ((equal (car el) "F")
          (push (cdr el) $files))
         ((equal (car el) "D")
          (push (cdr el) $dirs))))
      (when $dirs
        (dolist (el $dirs)
          (setq $childs
                (append $childs
                        (entropy/cl-list-dir-files-full el)))))
      (if $childs
          (setq rtn (append $files $childs))
        (setq rtn $files)))
    rtn))

;; **** checking duplicated files
(defun entropy/cl--sdpf-car-duplicated (cons-list)
  (let ((rtn nil)
        (rest-cons (cdr cons-list))
        (head-item (car cons-list)))
    (unless (or (equal 1 (length cons-list))
                (null cons-list))
      (dolist (el rest-cons)
        (when (equal (car head-item) (car el))
          (cond
           ((null rtn)
            (push (cons (car head-item) (list (cdr head-item) (cdr el))) rtn))
           (rtn
            (setf (cdr (car rtn))
                  (push (cdr el) (cdr (car rtn))))))))
      (setq rest-cons
            (let ((temp_rest rest-cons))
              (dolist (el rest-cons)
                (when (equal (car head-item)
                             (car el))
                  (setq temp_rest
                        (delete el temp_rest))))
              temp_rest))
      (setq rtn (append rtn
                        (entropy/cl--sdpf-car-duplicated rest-cons))))
    rtn))


(defun entropy/cl-search-duplicated-files (root-dir &optional sha_compare)
  "Search all duplictaed files under top directory ROOT-DIR and
return the Alist whose each elements car was the indicator and
the cdr was the duplicated association information.

The default duplicated searching mechanism was based the same
file-name, in that case that you don't want to using file
validation checking as the full accurate mehod. 

The optional arg SHA_COMPARE was used for checking file validation
message insteads of search duplicated as file-name based. It's
valid value are \"sha256sum\" \"sha512sum\" \"md5sum\", and the
default chosen is \"md5sum\" in the case that you set it to
Non-nil value.

*Return form:*

1. file-name base
#+BEGIN_SRC elisp
  (list
   (cons duplicated-file-name (list file-path1
                                    file-path2
                                    ...))
   ...)
#+END_SRC

2. file validation base
#+BEGIN_SRC elisp
  (list
   (cons duplicated-file-validation
         (list (cons file-name1 file-path1)
               (cons file-name2 file-path2)
               (cons file-name3 file-path3)
               ...))
   ...)
#+END_SRC"
  (let ($files
        compare-list
        rtn
        ;; increasing nested loop depth.
        (max-lisp-eval-depth 100000)
        (max-specpdl-size 100000))
    (unless (file-directory-p root-dir)
      (error (format "Root-Dir <%s> not exist!" root-dir)))
    (setq $files (entropy/cl-list-dir-files-full root-dir))
    (when $files
      (cond
       ((null sha_compare)
        (dolist (el $files)
          (push (cons (file-name-nondirectory el) el)
                compare-list))
        (entropy/cl--sdpf-car-duplicated compare-list))
       (t
        (dolist (el $files)
          (let ((shell-cbk (entropy/cl-file-validation el)))
            (push (cons shell-cbk (cons (file-name-nondirectory el) el))
                  compare-list)))
         (entropy/cl--sdpf-car-duplicated compare-list))))))

;; **** dirs relative relationship
(defvar entropy/cl-dir-relativity-log nil
  "Func `entropy/cl-dir-relativity-number''s log variable for
  debugging used.")

(defun entropy/cl-dir-relativity-number (base-dir-location external-dir-location)
  "Return the alist as relativity hierachy between two directory.


  For example:

  If BASE-DIR was '/usr/bin' and EXTERNAL-DIR was '/etc' then return
  alist (-2 . \"etc\" nil).

  If two dir was not below the root, return nil, this often occurred
  on WINDOWS plattform that basic and external dir was stored in
  different drive because WINDOWS use different drive letter for
  identifying each disk partion uniquely as 'c:' 'd:' so on.

  NOTE: both of base-dir and external-dir must be
  absolute-format and do not using other web protocol uri, this func
  only supported windows and unix-like path type.


  Details:

  The path core type on whatever plattform are, was both have the
  root node '/' or 'DRIVE-NAME:/', so the method for parse the
  relative type between two directory was really simple, that just
  be on what basic as current basdir BASE-DIR, and then compare each
  node of them respectively and then calculate parent path part, the
  last basic on the parent part, generate the relative result number
  for indicating the relative nodes interval with both positive or
  negative number for.

  The return relative result was one list contain the relative
  number (negative number indicate that external-dir was not
  deliver from current dir 'base-dir', the other wise was
  oppsite.), and the rest was the path tail with branch
  indication t or nil (t for same branch, nil for the otherwise),
  path tail was not always exist, as that the cases of same
  comparation and the same branch with negative relative number,
  it' s always the nil (as 'nil' was list with null as \"'()\").

  As that:

  BASE-DIR and EXTERNAL-DIR can in two state: 1) in same branch 2)
  in different branch

  #+BEGIN_EXAMPLE
                root                                            root
                 o                                                o
              +--+---+                                         +--+---+
              |  |   |                                         |  |   |
              |  o   | public parent path                      |  o   | public parent path
              |  |   |                                         |  |   |
              +--|---+                                         +--|---+
    external <---o                                        +-------o
     dir         |                                        |       |
                 |                                        |       |
                 o                                        |       |
                 |                              external  |       |
                 |                                 dir    o       o base-dir
                 o----->base-dir
                                                        different branch state
           same branch state
  #+END_EXAMPLE


  *same branch:*
  - base-dir: '/usr/share'
  - external-dir: '/usr/share/lib'
  - result: '(1 (\"lib\") t)

  - base-dir: '/usr/share'
  - external-dir: '/usr/share/'
  - result: '(0 nil t)

  - base-dir: '/usr/share/lib'
  - external-dir: '/usr/share'
  - result: '(-1 nil t)

  *different branch:*
  - base-dir: '/usr/share/lib'
  - external-dir: '/usr/share/applications/icons'
  - result: '(-1 (\"application\" \"icons\") nil)

  *both as equalization:*
  - base-dir: '/usr/bin'
  - external-dir: '/usr/bin'
  - result: '(0 (\"same\") t)
  "
  (setq entropy/cl-dir-relativity-log nil)
  (let* (blist
         elist
         clist
         (equal-count 0)
         (rtn nil)
         (base-dir (expand-file-name base-dir))
         (external-dir (expand-file-name external-dir-location)))
    (catch 'break
      (cond                             ;Deal with path type rely on different plattform
       ((equal system-type 'windows-nt)
        ;; Remove drive letter.
        (when (not (equal (replace-regexp-in-string "\\(^.:\\).*" "\\1" base-dir)
                          (replace-regexp-in-string "\\(^.:\\).*" "\\1" external-dir)))
          (setq rtn nil)
          (throw 'break nil))
        (setq base-dir (replace-regexp-in-string "^.:" "" base-dir))
        (setq external-dir (replace-regexp-in-string "^.:" "" external-dir)))
       (t t))

      (progn                            ;remove the tail dir slash char
        (setq base-dir (replace-regexp-in-string "/$" "" base-dir))
        (setq external-dir (replace-regexp-in-string "/$" "" external-dir)))

      ;; split dir path to list
      (setq blist (split-string base-dir "/" t))
      (setq elist (split-string external-dir "/" t))
      (add-to-list 'blist "$root")
      (add-to-list 'elist "$root")
      (setq entropy/cl-dir-relativity-log `(:path-pair (:blist ,blist :elist ,elist)))

      ;; generate combination equalized list 'clist' which was a list like '(1 1 0 0 0 1 0)
      (let ((count 0))
        (dolist (el elist)
          (if (equal el (nth count blist))
              (push 1 clist)
            (push 0 clist))
          (setq count (+ count 1)))
        (setq clist (reverse clist)))

      (setq entropy/cl-dir-relativity-log
            (append entropy/cl-dir-relativity-log
                    `(:combination ,clist)))

      ;; count the equalization count
      (catch 'equal-done
        (dolist (el clist)
          (if (equal el 0)
              (throw 'equal-done nil)
            (setq equal-count (+ 1 equal-count)))))

      (let ((blen (length blist))
            (elen (length elist))
            relative)
        (cond                           ;calculate dir relative number
         ((equal equal-count blen)
          (setq relative (- elen blen)))
         ((< equal-count blen)
          (setq relative (- equal-count blen))))
        
        (cond                           ;generate different path
         ((< relative 0)
          (cond
           ;; cond in different branch
           ((< (+ blen relative) elen)
            (let ((pos (- (+ blen relative) 1))
                  (count 0)
                  both
                  tail)
              
              (while (<= count pos)     ;make both list
                (push (nth count blist) both)
                (setq count (+ 1 count)))
              (setq both (reverse both))

              ;; make tail list
              (setq count 0)
              (dolist (el both)
                (when (equal el (nth count elist))
                  (setq count (+ 1 count))))
              (while (<= (+ count 1) (length elist))
                (push (nth count elist) tail)
                (setq count (+ count 1)))
              (setq tail (reverse tail))
              (setq rtn `(,relative ,tail nil))))

           ;; cond in same branch
           ((= (+ blen relative) elen)
            (setq rtn `(,relative nil t)))))
         ((> relative 0)
          (let* ((pos blen)
                 (tail nil)
                 (count pos))
            (while (<= count (- elen 1))
              (push (nth count elist) tail)
              (setq count (+ count 1)))
            (setq tail (reverse tail))
            (setq rtn `(,relative ,tail t))))
         ((= relative 0)
          (setq rtn `(,relative nil t))))))
    rtn))

(defun entropy/cl-make-relative-path (base-dir-location external-dir-location)
  "Make relative style path for external dir EXTERNAL-DIR which
based on base dir BASE-DIR powered on
`entropy/cl-dir-relativity-number'.

Return the relative path string if relative status exists,
otherwise for nil."
  (let* ((base-dir (expand-file-name base-dir-location))
         (external-dir (expand-file-name external-dir-location))
         (relative (entropy/cl-dir-relativity-number base-dir external-dir))
         rel-conc-func)
    (setq rel-conc-func
          (lambda (abs-rela-counter factor)
            (let ((counter 1)
                  (rtn ""))
              (while (<= counter abs-rela-counter)
                (setq rtn (concat rtn factor)
                      counter (1+ counter)))
              rtn)))
    (cond (relative
           (let ((same-branch (caddr relative))
                 (relative-number (car relative))
                 (relative-content (cadr relative)))
             (cond (same-branch
                    (cond ((> relative-number 0)
                           (let ((abbrev ".")
                                 (tail ""))
                             (dolist (el relative-content)
                               (setq tail (concat tail "/" el)))
                             (concat abbrev tail)))
                          ((< relative-number 0)
                           (funcall rel-conc-func (abs relative-number) "../"))
                          ((= relative-number 0)
                           "./")))
                   ((not same-branch)
                    (let ((abbrev (funcall rel-conc-func (abs relative-number) "../")))
                      (dolist (el relative-content)
                        (setq abbrev (concat abbrev el "/")))
                      (replace-regexp-in-string "/$" "" abbrev))))))
          ((not relative)
           nil))))


;; *** file
;; **** file modification state check
(defun entropy/cl-file-last-modification-time (file)
  "Indicated the last modification time of file FILE.

Return the list of the modification time by:
 'year', 'month', 'day', 'hour', 'minute', 'second',
and each value was number value.

Thus the example '(2018 8 21 14 47 05)."
  (let* ((time-string (format-time-string
                       "%Y-%m-%d-%H-%M-%S"
                       (if (version< emacs-version "26.1")
                           (nth 5 (file-attributes file))
                         (file-attribute-modification-time (file-attributes file)))))
         (tslist (split-string time-string "-"))
         tlist)
    (dolist (el tslist)
      (push
       (if (not (string-match-p "^0" el))
           (string-to-number el)
         (string-to-number (replace-regexp-in-string "^0" "" el)))
       tlist))
    (setq tlist (reverse tlist))))



(defun entropy/cl-file-modified (file file-time)
  "Justify whether file FILE was midified after last time
FILE-TIME.

FILE-TIME was produced by `entropy/cl-file-last-modification-time'."
  (let ((fitime file-time)
        (fctime (entropy/cl-file-last-modification-time file))
        contrast-list
        (rtn nil))
    (dolist (el '(0 1 2 3 4 5))
      (let ((oel (nth el fitime))
            (cel (nth el fctime)))
        (push `(,oel . ,cel) contrast-list)))
    (setq contrast-list (reverse contrast-list))
    (dolist (el contrast-list)
      (if (< (car el) (cdr el))
          (setq rtn t)))
    rtn))

;; **** automatically create file
(defun entropy/cl-create-file (&optional file-name target-location)
  "Create file with follow options:

- create file with named rely on `buffer-name' of `current-buffer' in `default-directory'
- create file with specific file-name in  default-directory.
- create file with named rely on `buffer-name' of `current-buffer' in specific location.
- create file with specific file-name in specific location.

Note the location must end with '/'."
  (let* ($return)
    (if (and (not file-name) (not target-location))
        (let ((crn (concat default-directory (entropy/cl-generate-attach-name (buffer-name)))))
          (if (file-exists-p crn)
              (error (format "File %s exists in %s ." crn default-directory))
            (find-file crn)
            (save-buffer)
            (kill-buffer)
            (setq $return crn)))
      (cond
       ((and file-name target-location)
        (let ((crn (concat target-location file-name)))
          (if (file-exists-p crn)
              (error (format "File %s exists in %s " file-name target-location))
            (entropy/cl-check-filename-legal crn)
            (find-file crn)
            (save-buffer)
            (kill-buffer)
            (setq $return crn))))
       ((and file-name (not target-location))
        (let ((crn (concat default-directory file-name)))
          (if (file-exists-p crn)
              (error (format "File %s exists in %s ." file-name default-directory))
            (entropy/cl-check-filename-legal crn)
            (find-file crn)
            (save-buffer)
            (kill-buffer)
            (setq $return crn))))
       ((and (not file-name) target-location)
        (let ((crn (concat target-location (entropy/cl-generate-attach-name (buffer-name)))))
          (if (file-exists-p crn)
              (error (format "File %s exists in %s ." (file-name-nondirectory crn) target-location))
            (entropy/cl-check-filename-legal crn)
            (find-file crn)
            (save-buffer)
            (kill-buffer)
            (setq $return crn))))
       (t (error "Invalid arg state"))))))



;; **** backup files
(defun entropy/cl-backup-file (FILE)
  "Backup file with named it by the form of
\"xxx-backup_20180713_Fri_21-28-20\""
  (if (and (file-exists-p FILE) FILE)
      (let* ((backup-name
              (concat FILE "-backup_" (format-time-string "%Y%m%d_%a_%H-%M-%S")))
             (backup-base (file-name-nondirectory backup-name))
             (file-base (file-name-nondirectory FILE)))
        (copy-file FILE backup-name)
        (message (format "Backup '%1$s' to '%2$s'"
                         file-base backup-base)))
    (user-error (format "Buffer '%s' is not one exists file's mirror buffer." (buffer-name)))))




;; **** check-filename legal
(defun entropy/cl-check-filename-legal (filename &optional non-error)
  "Check filename legally for current operation system platform.

This function refused the invalid file-name which un-compat for
current operation system platform like forms below:

1. For windows platform:

   * Filename begin with '/' was not allowed
   * Filename begin with undetective drive was not allowed like if
     the drives has been mounted as 'A B C' but 'D' drive, thus if
     file name begin with 'D:/' or 'D:\\' was not allowed.

2. For unix like platform 

   In commonly, unix-like system was without the whether mounted
   problem as windows, but if you use windows like path style on
   it.

This function will sign the error when filename was illegal when
optional arg NON-ERROR was nil or return t as illegal for,
otherwise return nil."
  (cond
   ((equal system-type 'windows-nt)
    (let ((path-type (entropy/cl-check-filename-w32-legal filename)))
      (if (not path-type)
          (if (not non-error)
              (error (format "Notice: Filename '%s' illegal!" filename))
            t)
        nil)))
   (t
    (let ((path-type (entropy/cl-check-filename-unix-legal filename)))
      (if (not path-type)
          (if (not non-error)
              (error (format "Notice: Filename '%s' illegal!" filename))
            t)
        nil)))))

(defun entropy/cl-check-filename-unix-legal (filename)
  "Check filename FILENAME type whether be compat for UNIX system

Notice: untill now [2018-10-15 Mon 02:05:33] just judging whether
filename type was win32 type, if as then return nil, otherwise
return t."
  (if (string-match-p "^.+?:\\(/\\|\\\\\\)" filename)
      nil
    t))


(defun entropy/cl-check-filename-w32-legal (filename)
  "Check filename FILENAEM type whether suites as windows path type.

  The return are t or nil for follow meaning:
  1. 't': filename was both of be compat for current system platform
          and can be created.
  2. 'nil': filename was one of below situation:
     * filename was not compat for current system platform.
     * filaname can not be created that was indicated that the
       drive path was not detected.
  "
  (let ((rtn nil))
    (cond
     ((string-match-p "^\\(/\\|\\\\\\)" filename)
      (setq rtn nil))
     ((string-match-p "^.:\\(/\\|\\\\\\)" filename)
      (catch 'break
        (dolist (el (entropy/cl-get-w32-drives-legal-list))
          (when (string-match-p el filename)
            (setq rtn t)
            (throw 'break nil)))))
     ((string-match-p "^..+:\\(/\\|\\\\\\)" filename)
      (setq rtn nil))
     (t
      (setq rtn t)))
    rtn))

(defun entropy/cl-get-w32-drives-legal-list ()
  "Check windows drives which are detective and return regexp
  list form like:

'(\"\\(m\\|M\\)\" \"\\(f\\|F\\)\" \"\\(z\\|Z\\)\")

That indicated that drive 'm' 'f' 'z' were detected."
  (let ((full-drive-list
         '(("a:/" . "\\(^a\\|^A\\)") ("b:/" . "\\(^b\\|^B\\)") ("c:/" . "\\(^c\\|^C\\)") ("d:/" . "\\(^d\\|^D\\)") ("e:/" . "\\(^e\\|^E\\)")
           ("f:/" . "\\(^f\\|^F\\)") ("g:/" . "\\(^g\\|^G\\)") ("h:/" . "\\(^h\\|^H\\)") ("i:/" . "\\(^i\\|^I\\)") ("j:/" . "\\(^j\\|^J\\)")
           ("k:/" . "\\(^k\\|^K\\)") ("l:/" . "\\(^l\\|^L\\)") ("m:/" . "\\(^m\\|^M\\)") ("n:/" . "\\(^n\\|^N\\)") ("o:/" . "\\(^o\\|^O\\)")
           ("p:/" . "\\(^p\\|^P\\)") ("q:/" . "\\(^q\\|^Q\\)") ("r:/" . "\\(^r\\|^R\\)") ("s:/" . "\\(^s\\|^S\\)") ("t:/" . "\\(^t\\|^T\\)")
           ("u:/" . "\\(^u\\|^U\\)") ("v:/" . "\\(^v\\|^V\\)") ("w:/" . "\\(^w\\|^W\\)") ("x:/" . "\\(^x\\|^X\\)") ("y:/" . "\\(^y\\|^Y\\)")
           ("z:/" . "\\(^z\\|^Z\\)")))
        rtn)
    (dolist (el full-drive-list)
      (when (file-exists-p (car el))
        (add-to-list 'rtn (cdr el) t)))
    rtn))
;; **** file-sha-or-md5 checking
(defun entropy/cl-file-validation (file &optional vtool)
  "Using validation tool generated the file validation codes.

Avalible validtion tool used for are:
- \"sha256sum\"
- \"sha256512\"
- \"md5sum\"

When optional arg VTOOL nil for, using md5sum as default choice.
"
  (let ((validation-tools '("sha256sum" "sha512sum" "md5sum"))
        validation)
    ;; Validation tools exists-p checking.
    (cond
     ((and vtool
           (ignore-errors (stringp vtool)))
      (unless (and (member vtool validation-tools)
                   (executable-find vtool))
        (error (format "Validation tool '%s' invalid." vtool))))
     (t
      (unless (executable-find "md5sum")
        (error (format "Validation tool '%s' invalid." "md5sum")))
      (setq vtool "md5sum")))
    ;; File validation checking
    (unless (file-exists-p file)
      (error (format "File '%s' not exit!" file)))

    (setq validation
          (replace-regexp-in-string
           "^\\([a-zA-Z0-9]+\\)\\( +.*$\\)" "\\1"
           (shell-command-to-string  
            (concat vtool " "
                    (shell-quote-argument file)))))
    (when (string-match-p "\n" validation)
      (setq validation (replace-regexp-in-string "\n" "" validation)))
    validation))

;; ** window and buffer operaion
;; *** buffer operation
;; **** buffer name uniquely checker
(defun entropy/cl-ununiquify-bfn (buffer-name)
  "ununiquify buffer name which auto produced the extension name
  components rely on `uniquify-buffer-name-style-unique'."
  (let* ((uni-rex "\\( *<.*>\\)")
         rtn)
    (setq rtn (replace-regexp-in-string uni-rex "" buffer-name))
    rtn))

(defun entropy/cl-sn-buffer-p (&optional any-args)
  "Judge whether current buffer name was unique in buffer list."
  (let* ((bufferlist (mapcar (lambda (bfn) (buffer-name bfn)) (buffer-list)))
         (oblist (delete (buffer-name) bufferlist)))
    (dolist (el oblist)
      (if (string-match-p (regexp-quote
                           (entropy/cl-ununiquify-bfn (buffer-name)))
                          el)
          (error "Buffer name not uniquify! please close other buffer which as named as current buffer.")))
    (message "Uniquely buffer-name: %s" (buffer-name))))

;; **** buffer exist check
(defun entropy/cl-buffer-exists-p (buffername)
  "Judge whether buffer BUFFERNAME existed!"
  (let* ((bfl (mapcar 'buffer-name (buffer-list))))
    (if (-filter '(lambda (bname)
                    (if (string= buffername bname) t nil))
                 bfl)
        t
      nil)))


;; **** replacing buffer string
(defun entropy/cl-replacing-buffer (re-rules-alist)
  "Replacing by regexp rule within whole buffer.

  The rule-set are formatted as bellow:

  '(\"^\\(\\.*\\)123$\" . \"\\1Hello World\")"
  (when buffer-read-only
    (read-only-mode 0))
  (goto-char (point-min))
  (while (search-forward-regexp (car re-rules-alist) nil t)
    (replace-match (cdr re-rules-alist))))



;; ** completion framework
;; *** ivy development
;; **** ivy repeatly read former
(defun entropy/cl-ivy-read-repeatedly-function (read candidates-recorded prompt-abbrev &optional selected-shorten-function)
  "Common repeatedly read action for `ivy-read'.

Arguments:

- read:                      the current chosen entry
- candidates-recorded:       chosen stored list, it's a symbol-name

- prompt-abbrev:             abbrevation of prompt for repeatedly chosen
                             entry action prompt, suggested for keep consist
                             with initial promot.

- selected-shorten-function: function for shorten candidates'
                             display. It's symbol. This variable
                             was optional.
"
  
  (if (and (not (member read (symbol-value candidates-recorded)))
           (not (string= "" read)))
      (push read (symbol-value candidates-recorded)))
  (let ((prompt (entropy/cl-ivy-read-repeatedly-prompt-expand prompt-abbrev candidates-recorded selected-shorten-function)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "(%d/%d) " prompt)))
  (cond
   ((memq this-command '(ivy-done ivy-alt-done 'ivy-immediate-done))
    t)
   ((eq this-command 'ivy-call)
    (with-selected-window (active-minibuffer-window)
      (delete-minibuffer-contents)))))

(defun entropy/cl-ivy-read-repeatedly-prompt-expand (prompt-abbrev candidates-recorded &optional shorten-function)
  "Make incremented prompt string for function `entropy/cl-ivy-read-repeatedly-function'.

You can see the details of arguments with above mentioned function docstring."
  (format (concat prompt-abbrev " (%s) : ")
          (let ((olist (symbol-value candidates-recorded))
                mlist
                rtn)
            (dolist (el olist)
              (let (prefix)
                (unless (stringp el)
                  (cond
                   ((symbolp el)
                    (setq el (symbol-name el)))
                   (t
                    (error "<<entropy/cl-ivy-read-repeatedly-prompt-expand>>:
Just symbol and string type supported for candidates-recoreded."))))
                (setq prefix (if shorten-function
                                 (funcall shorten-function el)
                               el))
                (push prefix mlist)))
            (setq rtn (let ((prompt ""))
                        (dolist (el mlist)
                          (setq prompt (concat prompt
                                               (if (not (string= "" el))
                                                   (concat el "☑; ")
                                                 ""))))
                        prompt))
            rtn)))

;; *** Internal completion method
;; **** Read string
(defun entropy/cl-repeated-read (initial-input-prompt)
  "Repeated read strings by `read-string' with typing history prompt,
after type nothing, return the string list in order to the typing
sequence."
  (let* ((read "init") rtn
         (input-prompt initial-input-prompt))
    (while (not (string= read ""))
      (if rtn
          (setq input-prompt (format (concat initial-input-prompt " (%s)")
                                     (mapconcat #'identity (entropy/cl-reverse-list rtn) ","))))
      (setq read
            (read-string (format "%s: " input-prompt)))

      (if (not (string= read ""))
          (push read rtn)))
    (if (> (length rtn) 1)
        (progn
          (setq rtn (entropy/cl-reverse-list rtn))
          rtn)
      (if rtn
          rtn
        (error "No input data!")))))




;; ** mischellaneous
(defun entropy/cl-gen-time-str (&optional format-string)
  "generate one time-string from the default style or the
customized specific format-string FORMAT-STRING."
  (let ($return)
    (if (not format-string)
        (setq $return (format-time-string "%H%M%S-%e%m%Y"))
      (setq $return (format-time-string format-string)))))

(defun entropy/cl-generate-attach-name (origin-name &optional suffix)
  "Generate one name according the origin one you specific with
  the specific suffix string SUFFIX.

If SUFFIX was nill auto use date-time-sec for suffix."
  (let ($return
        (orname (replace-regexp-in-string "\\(\\*\\|<\\|>\\)" "" origin-name)))
    (if (not suffix)
        (setq $return (concat orname "_" (entropy/cl-gen-time-str)))
      (setq $return (concat orname suffix)))))

(defun entropy/cl-read-file-name (file-type &optional existed)
 "Repeaatly read file (or dir) name from minibuffer with internal
file name completion table.

FILE-TYPE was \"file\" string or arbitrary value demoting for
directory type, using string \"dir\" by habitually.

Optional arg EXISTED indicated for require-matching for current
candidates from `read-file-name-internal', and satisfied its
FILE-TYPE.

None existed manaully read case will obey the FILE-TYPE by
following rule-set:

1) \"file\" type and read as existed one:

   while the read existed as one directory, show prompt in mode
   line and request repeatly inputtng.

2) \"file\" type and read as non-existed:
  
   while the read string matched the tail slash, repeatly
   inputting.
  
3) \"dir\" type and exsited read:

   while the read existed as one file(not directory), do repeatly
   inputting.

4) Other any read case was legal."
  
  (let* ((init-file
          (completing-read
           (if (equal file-type "file")
               "Choosing file: "
             "Choosing directory: ")
           'read-file-name-internal))
         (judge-func (if (equal file-type "file")
                         (lambda (x) (and (file-exists-p x)
                                          (not (file-directory-p x))))
                       'file-directory-p))
         (repeat-existed-func
          (lambda ()
            (while (not (funcall judge-func init-file))
              (let ((mode-line-format
                     (if (equal file-type "file")
                         (if (file-exists-p init-file)
                             (list "%e" (propertize "Please choose 'FILE' not the directory location!" 'face 'warning))
                           (list "%e" (propertize (format "File not exists '%s'" init-file) 'face 'warning)))
                       (if (file-exists-p init-file)
                           (list "%e" (propertize "Please choose 'DIR' not the file location!" 'face 'warning))
                         (list "%e" (propertize (format "Direcory not exists '%s'" init-file) 'face 'warning))))))
                (setq init-file
                      (completing-read (if (equal file-type "file")
                                           "Choosing file: "
                                         "Choosing directory: ")
                                       'read-file-name-internal))))))
         (repeat-manually-func
          (lambda ()
            (let ((slash-tail-warn (list "%e" (propertize "Please input 'FILE' not the directory location!" 'face 'warning)))
                  (file-warn (list "%e" (propertize (format "Please input 'DIR', file existed '%s'!" init-file) 'face 'warning))))
              (cond ((equal "file" file-type)
                     (while (string-match-p "/$" init-file)
                       (let ((mode-line-format slash-tail-warn))
                         (setq init-file
                               (completing-read "Input file: " 'read-file-name-internal)))))
                    (t
                     (while (and (file-exists-p init-file)
                                 (not (file-directory-p init-file)))
                       (let ((mode-line-format file-warn))
                         (setq init-file
                               (completing-read "Input directory: "
                                                'read-file-name-internal))))))))))
    (if existed
        (funcall repeat-existed-func)
      (funcall repeat-manually-func))
    init-file))

;; ** color refer
(defun entropy/cl-color-name-to-hsv (color_name)
  "Transfer color COLOR_NAME to hsv format."
  (let* ((Crgb (color-name-to-rgb color_name))
         (Chsv (apply 'color-rgb-to-hsv Crgb)))
    Chsv))

(defun entropy/cl-color-name-to-lab (color_name)
  "Transfer color COLOR_NAME to lab format."
  (let* ((Crgb (color-name-to-rgb color_name))
         (Clab (apply 'color-srgb-to-lab Crgb)))
    Clab))

(defun entropy/cl-frameBG-dark-or-light ()
  "Judge current emacs frame background color of dark or light type.

This function used human perception color module LAB's light
channel to analyze the background color human perception simply
dividing it as uper or lower than 50 count which the lower as
dark, otherwise as light.

For terminal ui emacs using, this function will report the error
because that some situation for those frame background was
non-detectivation."
  (let* ((CFbg (face-attribute 'default :background))
         (CFlab_l (car (entropy/cl-color-name-to-lab CFbg))))
    (if (> CFlab_l 55)
        'light
      'dark)))

;; ** url refer
(defun entropy/cl-url-transfer-region-entities (&optional region)
  "Transfer region-selected text into entities form and push it into
kill-ring.

You can insert it by directly use `yank'

Optionally argument REGION was used for elisp coding context,
instead of the interactive way."
  (let* ((region-selected
          (cond (region
                 region)
                ((not region)
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-selected-beginning) (region-selected-end))
                   nil))))
         rtn)
    (if region-selected
        (progn (setq rtn (url-insert-entities-in-string region-selected))
               (with-temp-buffer
                 (if buffer-read-only
                     (read-only-mode 0))
                 (let (p1 p2)
                   (insert (replace-regexp-in-string "\"" "\"" rtn))
                   (setq p1 (point-min)
                         p2 (point-max))
                   (kill-ring-save p1 p2))))
      (error "Non valid region argument given."))))


;; * provide
(provide 'entropy-common-library)
