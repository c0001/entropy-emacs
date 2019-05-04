;;; entropy-prjm-core.el --- The core prototype module for entropy project management system

;; * Copyright
;; #+BEGIN_EXAMPLE
;; Copyright (C) [2019-04-27 Sat 16:51:58]  Entropy
;;
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-project-manager/blob/master/entropy-prjm-core.el
;; Package-Version: v0.1.0
;; Version:       0.1.0
;; Created:       2019-04-27 16:53:16
;; Keywords:      project-manager
;; Compatibility: GNU Emacs emacs-version;
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
;; * Commentary:
;;
;; This file define the core prototype of entropy project
;; managerment, which gives the data type with it's method without
;; the oop defination as be instead of using the independently
;; functions grouping for as.
;;
;; The main proto for those item:
;;
;; - project object prototype:         ~entropy/prjm-prj-obj-prototype~
;; - data base object prototype:       ~entropy/prjm-db-obj-prototype~
;; - data base cache object prototype: ~entropy/prjm-db-cache-obj-prototype~
;;
;;
;; For each object manipulation, as the conveniently way for, the
;; sub-prototype of each of above main prototype called the
;; =expression= of the, designed for inter process communications as
;; the object prototype generator arguments like the ~cl-defstruct~
;; structor does as. 
;;
;; *Why not using ~cl-defstrucy~:* 
;;
;; The default of ~cl-defstruct~ gives the slots designation to
;; build the new data-structor as the hash or association list for
;; the general structrual container, thus the manipulation to each
;; elements of it has two method in my mind:
;;
;; 1. Gives the new cl-struct for each element for the complicated
;;    element struct.
;;
;; 2. Making the indiviaul function to do as.
;;
;;
;; For case =1=, if the element was structured as the complicated
;; one, building 'element' cl-struct may also have the nested
;; occasion for reserving the extending possibilities for future api
;; updating. This was terrible that if the prototype consists of
;; extremely pluralistic items, there's must be multiple sub-structor
;; defined accordingly each. This disaster for coding management.
;;
;; Thus case =2= was the actual way for the more compalicated and
;; extending way for such the project information data type
;; requirements.
;;
;;
;; *Expressions:*
;;
;; Since each entropy-project-manager object must has the
;; obj-generator, as the basic type for each genertor is common elisp
;; function, for generating the object-instance satisfied with the
;; actual arguments ordered by user or process, for generating the
;; instance who based on the abstract prototype, argument passing for
;; the generator I called =expression=, as that prj-object with
;; =prj-expression=, data-base-object with =db-expression=,
;; data-base-cache-object with =db-cache-expression=.
;;
;; Each expression has it's own prototype which pre-derived from the
;; corresponding object prototype. "pre-derived" means that its
;; designation was reflected by it's refer object prototype, or as
;; the say: /they are one-one correspondence/, it's prototype as
;; function also. To define the expression prototype instead of using
;; doc-string description to provide the data construct standard
;; intented to give the programe validation reference, as that what
;; shown is each =expression= have one validation checker, the one
;; gives the validation procedure for programmer using in his own
;; function wrapper appropriately. Thus for 'prj-expression' of
;; example, =prj-expression-validp= was the function to validate
;; each passing 'prj-expression' whether legal or not.
;;
;; *Rich expression:*
;;
;; As what did for =expression=, the =rich-expression= was the
;; briefly description for each entropy-project-manager object, but
;; for the backend which /lamely/ or /fully/ designed data construct
;; reflected to it's object prototype for the sake for simplifying
;; the data manipulation in many coding occasion.
;;
;; Each =rich-expression= builded by the refer object expression,
;; that's why it's the backend not as the same for expression as the
;; front-end for building object instance, it's derived from object
;; prototype as =expression= but was automatically generated followed
;; by the updating of object prototype, not as the =expression= that
;; it's internal mechanism must update manually to followed what as.
;;
;; The aspect of it's prototype briefly description, each
;; =rich-expression= has it's own prototype which just indicated for
;; that it's the sub-prototype of the origin refer. As what demoted
;; for, it's briefest in the case both of /lamely/ and /fully/
;; redesigned referenced with it's origin one, =rich-expression=
;; built for various kinds of forks, as that prj-object has the main
;; rich-expression =prj-rich-expression=, also has the
;; =prj-column-expression=, for meaningful as for those two ones
;; inidividually:
;;
;; - =prj-rich-expression=:
;;
;;   The fully re-construct for =prj-obj= which list all prj-obj's
;;   columns slot into the list of plist style who has the keys
;;   ~:table~, ~:colum~, ~:value~, ~:value-type~. Moreover that, it's
;;   column order is reflected fully by the prj-obj's internal
;;   construct.
;;
;; - =prj-column-expression=:
;;
;;   It's designed for represented the prj-obj's content without it's
;;   operation and table information, extracted all key-value pairs
;;   into one plist for the sake to get the prj static content as the
;;   part of origin prj-obj's construed map. Thus this
;;   rich-expression was *lamely* but useful, as that it's used in
;;   =db-cache-obj=.
;;
;;
;; *Proto map:*
;;
;; The two main (data-base refer object) object prototypese are
;; designed derived by =prj-obj-prototype=, thus this one was the
;; core prototype for those other ones. For the same reason, prj-*
;; refer expression are the fountainhead arguments expression to
;; those other object's using for it's own internal construct.
;;
;; Here's the map of each object prototype with its operations and
;; refer expressions and rich-expression.
;;
;; | proto                                 | operations                      | expressions                                    |
;; |---------------------------------------+---------------------------------+------------------------------------------------|
;; | =entropy/prjm-prj-obj-prototype=      | =entropy/prjm-gen-prj-obj=      | =entropy/prjm-prj-expression-prototype=        |
;; |                                       |                                 | =entropy/prjm-prj-rich-expression-prototype=   |
;; |                                       |                                 | =entropy/prjm-prj-column-expression-prototype= |
;; |---------------------------------------+---------------------------------+------------------------------------------------|
;; | =entropy/prjm-db-obj-prototype=       | =entropy/prjm-gen-db-obj=       | =entropy/prjm-db-expression-prototype=         |
;; |---------------------------------------+---------------------------------+------------------------------------------------|
;; | =entropy/prjm-db-cache-obj-prototype= | =entropy/prjm-gen-db-cache-obj= | =entropy/prjm-db-cache-expression-prototype=   |
;; |                                       |                                 |                                                |
;;
;; For expressions, the map for their own:
;;
;; | expression proto                               | operation                                | validator                                   |
;; |------------------------------------------------+------------------------------------------+---------------------------------------------|
;; | =entropy/prjm-prj-expression-prototype=        | nil                                      | =entropy/prjm-prj-expression-validp=        |
;; |------------------------------------------------+------------------------------------------+---------------------------------------------|
;; | =entropy/prjm-prj-rich-expression-prototype=   | =entropy/prjm-gen-prj-rich-expression=   | =entropy/prjm-prj-rich-expression-validp=   |
;; |------------------------------------------------+------------------------------------------+---------------------------------------------|
;; | =entropy/prjm-prj-column-expression-prototype= | =entropy/prjm-gen-prj-column-expression= | =entropy/prjm-prj-column-expression-validp= |
;; |                                                | =entropy/prjm-prj-column-exp-to-prj-exp= |                                             |
;;
;;
;; *Prototype details:*
;;
;; Since each object prototype has its own struct designation, ones
;; of them are complicated without the capability say shortly in code
;; file commentry block, view them in the code region.
;;
;; This file is constructed as =org-mode= headline struct as literate
;; context organization for developer for as, read it and happy
;; hacking.
;;
;; * Configuration:
;; There's nothing need to do for using this package for requring it
;; into your project.
;; 
;; * Code:

;; ** require
(require 'entropy-common-library)

;; ** defcustom
(defgroup entropy/prjm-group nil
  "entropy project manager group")

;; ** utilities 
(defun entropy/prjm--transfer-prop-to-string (prop)
  (let (rtn)
    (unless (string-match "^:" (symbol-name prop))
      (error "Prop must format as ':xxx'!"))
    (setq rtn
          (replace-regexp-in-string
           "^:" "" (symbol-name prop)))))

(defun entropy/prjm--transfer-propname-to-symbol (prop-name)
  (let (rtn)
    (unless (not (string-match-p "^:" prop-name))
      (error "Prop name can not include ':' at head place!"))
    (setq rtn
          (intern (concat ":" prop-name)))))

;; ** prjm module
;; *** prjm database module
;; **** prjm database obj prototype

;; This part for prj-db-obj and it's expression
;;
;; =db-obj= was the simple description prototype for each
;; entropy-project-manager database, it's a plist contained four base
;; key-pair and one recreational 'logo' key-pair.
;;
;; *Main key-pair:*
;;
;; 1) ~:db-name~
;;
;;    The database name indicator, does as the identification for
;;    individual database, it's the type of string, and must not be
;;    empty.
;;
;; 2) ~:db-type~
;;
;;    The database type indicator, string type of non-empty
;;    status. Arbitray value can be setted, just ruled by the kinds of
;;    database manipulation backend of =entropy-prjm-core=.
;;
;; 3) ~:db-location~
;;
;;    Demoting where the database can be find, Local-FileSystem only,
;;    string type with valid path description.
;;
;; 4) ~:db-des~
;;
;;    Description string of database, type of string without valid
;;    setting (e.g. emty or nil was allowed).
;;
;;
;; *db-expression:*
;;
;; For generating db-obj, expression =db-expression= does that, was
;; simple list type of string elements, ordered by key-seqence
;; (:db-name :db-type :db-location :db-des :db-logo), valid checking
;; func ~entropy/prjm-db-expression-validp~
;;



;; ***** prjm database obj structor
(defun entropy/prjm-db-obj-prototype ()
  (let* ((proto-func
          (lambda ()
            (list :db-name nil :db-type nil :db-location nil :db-des nil :db-logo nil)))
         (copy-proto (copy-tree (funcall proto-func))))
    copy-proto))

;; ***** prjm database obj operation
;; ****** prjm database obj generator
(defun entropy/prjm-gen-db-obj (db-expression)
  (entropy/prjm-db-expression-validp db-expression)
  (entropy/cl-plist-batch-put
   (entropy/prjm-db-obj-prototype)
   db-expression))

;; **** prjm database expression
(defun entropy/prjm-db-expression-prototype ()
  (let* ((db-proto (entropy/prjm-db-obj-prototype))
         (db-proto-len (length db-proto))
         rtn)
    (dotimes (el (/ db-proto-len 2) nil)
      (push nil rtn))
    rtn))

;; ****** prjm database expression operation
;; ******* prjm database expression valid checker
(defun entropy/prjm-db-expression-validp (db-expression &optional details)
  (let ((db-name (car db-expression))
        (db-type (cadr db-expression))
        (db-location (caddr db-expression))
        (db-des (cadddr db-expression))
        (db-logo (car (cddddr db-expression)))
        db-name-status db-type-status db-location-status db-des-status db-logo-status
        rtn details-cbk
        (db-attr-status-synopis
         '((1 . "value stringp nil")
           (2 . "value stringp but empty")
           (4 . "path not exist")
           (0 . "OK")))
        (db-attrs-valid-form
         '((0 0 0 0 1)
           (0 0 0 0 2)
           (0 0 0 0 0)
           (0 0 0 1 1)
           (0 0 0 1 2)
           (0 0 0 1 0)
           (0 0 0 2 1)
           (0 0 0 2 2)
           (0 0 0 2 0)))
        (db-attrs-lossy-form
         '((0 0 4 0 1)
           (0 0 4 0 2)
           (0 0 4 0 0)
           (0 0 4 1 1)
           (0 0 4 1 2)
           (0 0 4 1 0)
           (0 0 4 2 1)
           (0 0 4 2 2)
           (0 0 4 2 0)))
        (condition-func
         (lambda (x y &optional path-check)
           (if (and (stringp x)
                    (not (equal "" x)))
               (setf (symbol-value y) 0)
             (cond
              ((not (stringp x))
               (setf (symbol-value y) 1))
              (t
               (setf (symbol-value y) 2))))
           (when (and path-check
                      (not (eq 1 (symbol-value y)))
                      (not (eq 2 (symbol-value y))))
             (unless (ignore-errors (file-exists-p (expand-file-name x)))
               (setf (symbol-value y) 4))))))
    (let ((db-attrs (list db-name db-type db-location db-des db-logo))
          (db-status '(db-name-status db-type-status db-location-status db-des-status db-logo-status))
          (counter 0))
      (dolist (el db-status)
        (cond
         ((equal "db-location-status" (symbol-name el))
          (funcall condition-func
                   (nth counter db-attrs)
                   el t))
         (t
          (funcall condition-func
                   (nth counter db-attrs)
                   el)))
        (cl-incf counter)))
    (let ((status-map
           (list db-name-status db-type-status db-location-status db-des-status db-logo-status))
          (db-attrs-smb (list :db-name :db-type :db-location :db-des :db-logo))
          (counter 0))
      (cond
       ((member status-map db-attrs-valid-form)
        (setq rtn "OK"))
       ((member status-map db-attrs-lossy-form)
        (setq rtn "LOSSY"))
       (t
        (setq rtn "ILLEGAL")))
      (when details
        (dolist (el db-attrs-smb)
          (let ((status-info (cdr (assoc (nth counter status-map)
                                         db-attr-status-synopis))))
            (push (list el status-info) details-cbk))
          (cl-incf counter))
        (setq details-cbk
              (apply 'append
                     (reverse details-cbk)))))
    (cond
     ((equal details "FULL")
      details-cbk)
     ((equal details "BRIEF")
      rtn)
     (t
      (cond
       ((equal rtn "ILLEGAL")
        (error "<<entropy-prjm>>: db-expression illegal!"))
       ((equal rtn "LOSSY")
        (error "<<entropy-prjm>>: db-expression lossy existed db file!"))
       (t
        t))))))

;; *** prjm database cache module
;; **** prjm database cache obj prototype 
;; ***** prjm database cache obj prototype structor
(defun entropy/prjm-db-cache-obj-prototype ()
  (let* ((proto-hash (make-hash-table :test 'equal :weakness nil))
         (proto-hash-copy (copy-hash-table proto-hash))
         (proto-cons (list "" nil '()))
         (proto-cons-copy (copy-tree proto-cons)))
    (setf (cadr proto-cons-copy) proto-hash-copy)
    proto-cons-copy))

;; ***** prjm database cache obj operation
;; ****** prjm database cache obj generator
(defun entropy/prjm-gen-db-cache-obj (db-cache-expression)
  (entropy/prjm-db-cache-expression-validp db-cache-expression)
  (let* ((db-cache-obj-proto (entropy/prjm-db-cache-obj-prototype))
         (db-hash-table (cadr db-cache-obj-proto))
         (db-name (car db-cache-expression))
         (db-cache (cadr db-cache-expression))
         (counter 0)
         (prj-obj-proto (entropy/prjm-prj-obj-prototype))
         (Shaft (caar (cddddr (assoc 'Shaft prj-obj-proto)))))
    (setf (car db-cache-obj-proto) db-name)
    (while (< counter (length db-cache))
      (let* ((prj-column-exp (nth counter db-cache))
             (shaft-value (car (plist-get prj-column-exp Shaft))))
        (puthash shaft-value prj-column-exp db-hash-table))
      (cl-incf counter))
    (append (remove nil db-cache-obj-proto) (caddr db-cache-expression))))


;; **** prjm database cache expression
;; ***** prjm database cache expression prototype
;; ****** prjm database cache expression prototype structor
(defun entropy/prjm-db-cache-expression-prototype ()
  (let* ((proto-func (lambda () (list ""
                                      (list (entropy/prjm-prj-column-expression-prototype))
                                      "")))
         (proto-copy (copy-tree (funcall proto-func))))
    proto-copy))

;; ****** prjm database cache expression operation
;; ******* prjm database cache expression valid checker
(defun entropy/prjm-db-cache-expression-validp (db-cache-expression &optional details)
  (let ((shaft (car (plist-get (cdr (assoc 'Shaft (entropy/prjm-prj-obj-prototype))) :columns)))
        (db-name (car db-cache-expression))
        (db-cache (cadr db-cache-expression))
        (prj-column-exp (entropy/prjm-prj-column-expression-prototype))
        rtn (counter-main 0) illegals status)
    (if (or (not (stringp db-name))
            (equal "" db-name))
        (if details
            (push "db-name: wrong type or empty string" status)
          (error "<<entropy-prjm>>: db-cache-expression db-name wrong type or empty"))
      (if details
          (push "db-name passed checking" status)
        (setq rtn t)))
    (if (null db-cache)
        (setq illegals t)
      (dolist (el db-cache)
        (unless (ignore-errors (entropy/prjm-prj-column-expression-validp el))
          (push el illegals))))
    (if (not details)
        (if illegals
            (error "<<entropy-prjm>>: db-cache-expression cache has invalid element")
          (setq rtn t))
      (if illegals
          (push "db-cache has invalid cache element!" status)
        (push "db-cache item validation passed checking." status)))
    (let (shaft-value-list dup-status)
      (dolist (el db-cache)
        (push (plist-get el shaft) shaft-value-list))
      (if (= (length shaft-value-list)
             (length (remove-duplicates shaft-value-list)))
          (setq dup-status nil)
        (setq dup-status t))
      (if dup-status
          (if (not details)
              (error "<<entropy-prjm>>: db-cache-expression has duplicated prj shaft value items.")
            (push "db-cache items duplicated check failure!" status))
        (if (not details)
            (setq rtn t)
          (push "db-cache items duplicated check passed" status))))
    (cond
     ((not details)
      rtn)
     (details
      (reverse status)))))



;; *** prjm prj module
;; **** prjm prj object prototype
;; ***** prjm prj object strutor
;; This part was the fountainhead entropy-project-manager object
;; prototype  =prj-obj-prototype='s RFC.
;;
;; The prj-obj was designed as wildly extensible with the base
;; skeleton all of it. The skeleton shows below:
;;
;; #+BEGIN_EXAMPLE
;; '((Operation . =operation-name=)
;;   (Shaft  .  (:table =table-name= :columns (=:key01= (=key-value= . =key-valud-type=))))
;;   (Tables . ((:table =table-name= :columns (=:key01= (=key-value= . =key-valud-type=)
;;                                            =:key02= (=key-value= . =key-valud-type=)
;;                                            =:key03= (=key-value= . =key-valud-type=)
;;                                            ...))
;;              (:table =table-name= :columns (=:key01= (=key-value= . =key-valud-type=)
;;                                            =:key02= (=key-value= . =key-valud-type=)
;;                                            =:key03= (=key-value= . =key-valud-type=)
;;                                            ...))
;;              ......)))
;; #+END_EXAMPLE
;;
;; It's ideal based on sql database which using table to orgnized
;; relation-ships formed in the same dimension that called 'table',
;; and the relationship-nodes shown up as 'column' which referrenced
;; to the paper table presentation style. =prj-object= using the sql
;; table struct to initially support the database manipulation as the
;; origin projects info managerment, but it's usefule and benefit to
;; use in other database type because the sql database designation
;; was commonly or generally as going up on the database concept.
;;
;; Each '=*=' slot in the skeleton are replaceable thus the
;; extensible meaning as. =entropy-prjm= gives the default
;; replacement in this package so for be the default value for other
;; =entropy-prjm= utilities using.
;;
;; As what you seen as that the top struct of was alist, the key
;; 'Operation', 'Shaft', 'Tables' was the main rivet for it and never
;; be modified, as same for the associated plist item's key ':table',
;; ':columns'. The limited for it's instance expanding are there
;; all. For other hands, you may using arbitry table column and it's
;; value type, the columns counts of each table are unlimited.
;;
;; This prototype was a function which spitted the independent
;; =prj-obj= alist with using ~copy-tree~, so as any place resets are
;; never effecting the origin data location in the computer memory.
;;
;;

(defun entropy/prjm-prj-obj-prototype ()
  (let* ((prototype-func
          #'(lambda ()
              (list
               (cons 'Operation nil)
               (cons 'Shaft (list :table "ALL" :columns (list :Prj_ID '(nil . "VARCHAR"))))
               (cons 'Tables
                     (list
                      (list
                       :table
                       "Prjs_Index"
                       :columns (list :Prj_Name '(nil . "TEXT")
                                      :Prj_Author '(nil . "TEXT")
                                      :Prj_Date '(nil . "TEXT")
                                      :Prj_State '(nil . "TEXT")))
                      (list
                       :table
                       "Prjs_Attr"
                       :columns (list :Prj_Attr_Type '(nil . "TEXT")
                                      :Prj_Attr_Category '(nil . "TEXT")
                                      :Prj_Attr_Rfc '(nil . "TEXT")
                                      :Prj_Attr_Uri  '(nil . "TEXT")
                                      :Prj_Attr_Des  '(nil . "TEXT")))
                      (list
                       :table
                       "Prjs_Vcs"
                       :columns (list :Prj_Vcs_Type '(nil . "TEXT")
                                      :Prj_Vcs_Remote '(nil . "TEXT")
                                      :Prj_Vcs_Status  '(nil . "TEXT")
                                      :Prj_Vcs_Head '(nil . "TEXT"))))))))
         (prototype (funcall prototype-func))
         (rtn (copy-tree prototype)))
    rtn))

;; ***** prjm prj object operation
;; ****** prjm prj object genterator
(defun entropy/prjm-gen-prj-obj (prj-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((prj-obj-prototype (entropy/prjm-prj-obj-prototype))
         (prj-obj-tables (cdr (assoc 'Tables prj-obj-prototype)))
         (prj-obj-shaft-place (cdr (assoc 'Shaft prj-obj-prototype)))
         (prj-obj-shaft-key (car (plist-get prj-obj-shaft-place :columns)))
         (prj-obj-shaft-value-type (cdadr (plist-get prj-obj-shaft-place :columns)))
         (prj-obj-operation-place (assoc 'Operation prj-obj-prototype))
         (counter-map (entropy/prjm-prj-counter-region-map prj-obj-prototype))
         (region-map (cdr counter-map))
         (prjexp-maped (entropy/cl-capture-list-by-region-map
                        prj-expression
                        region-map))
         (prj-obj-shaft-new-val (car (nth 1 prjexp-maped)))
         (prj-obj-operation-new-val (car (car prjexp-maped))))

    ;; Put operation
    (setf (cdr prj-obj-operation-place) prj-obj-operation-new-val)

    ;; Put shaft
    (plist-put prj-obj-shaft-place :columns
               (list prj-obj-shaft-key
                     `(,prj-obj-shaft-new-val . ,prj-obj-shaft-value-type)))

    ;; Put table props
    (let ((counter 2))
      (dolist (el prj-obj-tables)
        (let* ((table-columns (plist-get el :columns))
               (table-columns-new
                (entropy/cl-plist-batch-put
                 table-columns
                 (nth counter prjexp-maped)
                 #'(lambda (x y)
                     (let ((val-type (cdr x))
                           rtn)
                       (setq rtn (cons y val-type))
                       rtn)))))
          (plist-put el :columns table-columns-new))
        (cl-incf counter)))
    prj-obj-prototype))

;; ****** prjm prj object counter map
(defun entropy/prjm-prj-counter-region-map (prj-obj)
  (let* (($tables (cdr (assoc 'Tables prj-obj)))
         ($tables-len (length $tables))
         $map-tables $map rtn)
    (dolist (el $tables)
      (let* (($table-name (plist-get el :table))
             ($table-columns (plist-get el :columns))
             ($table-columns-len (/ (length $table-columns) 2)))
        (push $table-columns-len $map-tables)))
    (setq $map-tables (reverse $map-tables))
    (setq $map (append '(1) '(1) $map-tables))
    (setq rtn (cons (apply '+ $map) $map))))



;; **** prjm prj expression
;; ***** prjm prj expression structor
(defun entropy/prjm-prj-expression-prototype ()
  (let* ((prj-obj-prototype (entropy/prjm-prj-obj-prototype))
         (prj-tables (cdr (assoc 'Tables prj-obj-prototype)))
         rtn
         prj-expression-prototype)
    (dolist (el prj-tables)
      (let ((columns-len (/ (length (plist-get el :columns)) 2)))
        (push columns-len rtn)))
    (setq rtn (+ 2 (apply '+ rtn)))
    (dotimes (el rtn nil)
      (push nil prj-expression-prototype))
    prj-expression-prototype))

;; ***** prjm prj expression operation
;; ****** prjm prj expression valid checker
(defun entropy/prjm-prj-expression-validp (prj-expression &optional non-error)
  (let ((prj-expression-prototype (entropy/prjm-prj-expression-prototype))
        len-satisfied rtn)
    ;; expression length checker
    (if (eq (length prj-expression-prototype) (length prj-expression))
        (setq len-satisfied t)
      (unless non-error
        (error "entropy project expresson <length> invalid!"))
      (setq len-satisfied nil))
    (unless (not len-satisfied)
      (setq rtn t))
    rtn))

;; **** prjm prj column expression
;; This part for =prj-column-expression= which the most usefully
;; brief expression for =prj-obj=.
;;
;; =prj-column-expression= was the plist consists with columns as key
;; and the value with value-type cons was the key-value. The column
;; ordered as sequenced ordering as what prj-obj given. As
;; =prj-rich-expression=, it's built automatically without manually
;; did (no warranty for manually way).
;;
;; =prj-column-expression= was used for =db-cache-obj= prototype as
;; its internally sub-datastruct.
;;


;; ***** prjm prj column expression structor
(defun entropy/prjm-prj-column-expression-prototype ()
  (let* ((prj-obj (entropy/prjm-prj-obj-prototype))
         ($tables (cdr (assoc 'Tables prj-obj)))
         ($shaft (cdr (assoc 'Shaft prj-obj)))
         rtn)
    (dolist (el $tables)
      (push (plist-get el :columns) rtn))
    (setq rtn
          (apply 'append (reverse rtn)))
    (setq rtn
          (append (plist-get $shaft :columns) rtn))))
;; ***** prjm prj column expression operation
;; ****** prjm prj column expression generator
(defun entropy/prjm-gen-prj-column-expression (prj-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((prj-column-expression (entropy/prjm-prj-column-expression-prototype))
         (counter-column 1) (counter-value 1)
         (prj-expression-len (length (cdr prj-expression)))
         (prj-column-expression-len (length prj-column-expression)))
    (unless (= (* prj-expression-len 2)
               prj-column-expression-len)
      (error "<<entropy-prjm>>: prj-expression invalid"))
    (while (and (<= counter-column (- prj-column-expression-len 1))
                (<= counter-value prj-expression-len))
      (setf (car (nth counter-column prj-column-expression))
            (nth counter-value prj-expression))
      (cl-incf counter-value)
      (setq counter-column (+ 2 counter-column)))
    prj-column-expression))

;; ****** prjm column expression validp
(defun entropy/prjm-prj-column-expression-validp (prj-column-expression &optional details)
  (let ((exp-proto (entropy/prjm-prj-column-expression-prototype))
        (counter 0)
        rtn)
    (dolist (el exp-proto)
      (cond
       ((not (= 0 (mod counter 2)))
        (let ((value-type (cdr el))
              (cur-type (ignore-errors (cdr (nth counter prj-column-expression)))))
          (push (if (equal value-type cur-type) t nil) rtn)))
       ((= 0 (mod counter 2))
        (push (if (eq el (nth counter prj-column-expression))
                  t
                nil)
              rtn)))
      (cl-incf counter))
    (setq rtn (reverse rtn))
    (if details
        (let (status (counter-step 0))
          (setq counter 0)
          (while (<= (+ counter 1) (- (length exp-proto) 1))
            (let (status-item)
              (push (symbol-value (nth counter exp-proto)) status-item)
              (cond
               ((null (nth counter rtn))
                (push "[nil] Key symbol not eq" status-item))
               ((nth counter rtn)
                (push "[ok] Key symbol eq" status-item)))
              (cond
               ((null (nth (+ counter 1) rtn))
                (push "[nil] value type not equal" status-item))
               ((nth (+ counter 1) rtn)
                (push "[ok] value type equal" status-item)))
              (push (reverse status-item) status)
              (setq counter (+ 2 counter))))
          (setq rtn (reverse status)))
      (if (member nil rtn)
          (error "prj column expression invalid")
        (setq rtn t)))
    rtn))

;; ****** prjm column expression transfer to prj expression
(defun entropy/prjm-prj-column-exp-to-prj-exp (prj-column-expression operation)
  (entropy/prjm-prj-column-expression-validp prj-column-expression)
  (let ((counter 0) rtn)
    (while (< counter (length prj-column-expression))
      (push (car (plist-get prj-column-expression (nth counter prj-column-expression)))
            rtn)
      (setq counter (+ 2 counter)))
    (append `(,operation) (reverse rtn))))

;; **** prjm prj rich expression
;; ***** prjm prj rich expressioon structor
;; This part gives the implement for prj-rich-expression, it's
;; prototype and its generator.
;;
;; =prj-rich-expression= was the fully reflection from
;; =prj-obj-prototype= but with rearranged plist of list format which
;; gives the benefit for lisp manipulation.
;;
;; The skeleton of it looks as:
;;
;; #+BEGIN_EXAMPLE
;;   (=operation-name=
;;    (:table =table-name= :column =column01= :value =value01= :value-type =value-type01=)
;;    (:table =table-name= :column =column02= :value =value02= :value-type =value-type02=)
;;    (:table =table-name= :column =column03= :value =value03= :value-type =value-type03=)
;;    ...)
;; #+END_EXAMPLE 
;;
;; As what denoted in =prj-obj-prototype=, all =*= pattern are
;; replaceable but without manually typing replacement, because this
;; expression are generateing automatically with internal prj-object
;; prototype analyzing mechanism, any manually generating was
;; unnecessary and be tight with high risk possibility.
;;
;; *Notice:*
;;
;; The ':column' value are string-type as the symbol name got by
;; origin table column key-pair's key of car place e.g. =(:key (value
;; . value-type))=. this obtained procedure powered by function
;; ~entropy/prjm--transfer-prop-to-string~ , it's not common api be
;; as the package internal utility.
;;
;; *Features:*
;;
;; As what demoted before, =prj-rich-expression= is comprehensively
;; isomer with =prj-obj=, does that the list struct for each plist
;; element are reflect to each column of the database table which
;; list as =:table :colum :value :value-type= of each column's
;; necessary information from that.
;;
;; So as that, the prj-rich-expression another important feature was
;; that each colum-plist was ordered as sequenced by as the prj-obj,
;; which was the reflected ordering feature to satisfied the 'fully
;; reflected' feature.
;;
;; *Usage scenario:*
;;
;; Using simple list of colum-plist data struct allow programmer does
;; the basically looping operation to extracted useful prj
;; information, place replacing and other fundamentally prj-obj
;; manipulation.
;;
;;
;;
;;  


(defun entropy/prjm-prj-rich-expression-prototype ()
  (let* ((prj-obj (entropy/prjm-prj-obj-prototype))
         (prj-operation (cdr (assoc 'Operation prj-obj)))
         (prj-shaft-place (cdr (assoc 'Shaft prj-obj)))
         prj-shaft-rich
         (prj-tables-list (cdr (assoc 'Tables prj-obj)))
         (prj-obj-map (cdr (entropy/prjm-prj-counter-region-map prj-obj)))
         rtn)
    ;; gen shaft
    (setq prj-shaft-rich (append (list (car prj-shaft-place) (nth 1 prj-shaft-place))
                                 (entropy/prjm--list-rich-item (cadddr prj-shaft-place))))
    
    ;; gen table rich cons
    (dolist (el prj-tables-list)
      (let ((table-name (plist-get el :table))
            (table-columns (plist-get el :columns))
            table-rich-columns (counter 0))
        (dotimes (el2 (/ (length table-columns) 2) nil)
          (push (append
                 (list :table table-name)
                 (entropy/prjm--list-rich-item
                  (list (nth counter table-columns)
                        (nth (+ 1 counter) table-columns))))
                table-rich-columns)
          (setq counter (+ 2 counter)))
        (push (reverse table-rich-columns) rtn)))
    (setq rtn (apply 'append (reverse rtn)))
    (push prj-shaft-rich rtn)
    (push prj-operation rtn)
    rtn))
;; ***** prjm prj rich expression prototype utilities
(defun entropy/prjm--list-rich-item (plist-var)
  (let ((key (car plist-var))
        (p-val (caadr plist-var))
        (p-val-type (cdadr plist-var)))
    (list :column (entropy/prjm--transfer-prop-to-string key)
          :value p-val
          :value-type p-val-type)))



;; ***** prjm prj rich expression operation
;; ****** prjm prj rich expression generator
(defun entropy/prjm-gen-prj-rich-expression (prj-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((prj-rich-expression-prototype (entropy/prjm-prj-rich-expression-prototype))
         (prj-rich-len (length prj-rich-expression-prototype))
         (prj-exp-len  (length prj-expression))
         (counter-rich 1) (counter-expression 1))
    (setf (car prj-rich-expression-prototype) (car prj-expression))
    (while (and (<= counter-rich (- prj-rich-len 1))
                (<= counter-expression (- prj-exp-len 1)))
      (plist-put (nth counter-rich prj-rich-expression-prototype)
                 :value
                 (nth counter-expression prj-expression))
      (cl-incf counter-rich)
      (cl-incf counter-expression))
    prj-rich-expression-prototype))

;; ****** prjm prj rich expression valid checker

(defun entropy/prjm-prj-rich-expression-validp (prj-rich-expression &optional details)
  (let* (column-cl-chk-func
        column-vt-chk-func
        table-chk-func
        column-chl-func-chose
        result
        (exp-proto (entropy/prjm-prj-rich-expression-prototype))
        (exp-proto-tables (cdr exp-proto))
        (exp-cur-tables (cdr prj-rich-expression))
        (counter-main 0))
    (setq
     column-chl-func-chose
     (lambda (pointer sequence)
       (let ((next-type (nth (+ 1 pointer) sequence))
             rtn)
         (if next-type
             (setq rtn 'column-vt-chk-func)
           (setq rtn 'column-cl-chk-func))
         rtn))
     
     column-cl-chk-func
     (lambda (key-proto key-cur &optional info)
       (let (rtn)
         (cond
          ((eq key-proto key-cur)
           (if (not info)
               (setq rtn t)
             (setq rtn (list (symbol-name key-proto) "[ok]: key eq"))))
          ((not (eq key-proto key-cur))
           (if (not info)
               (setq rtn nil)
             (setq rtn (list (symbol-name key-proto) "[nil]: key not eq")))))
         rtn))

     column-vt-chk-func
     (lambda (key-proto value-proto key-cur value-cur &optional info)
       (let (rtn)
         (cond
          ((eq key-proto key-cur)
           (if (not info)
               (push t rtn)
             (push (symbol-name key-proto) rtn)
             (push "[ok]: key eq" rtn)))
          ((not (eq key-proto key-cur))
           (if (not info)
               (push nil rtn)
             (push (symbol-name key-proto) rtn)
             (push "[nil]: key not eq" rtn))))
         (cond
          ((equal value-proto value-cur)
           (if (not info)
               (push t rtn)
             (push "[ok]: key value equal" rtn)))
          ((not (equal value-proto value-cur))
           (if (not info)
               (push nil rtn)
             (push "[nil]: key value not equal" rtn))))
         (if (not info)
             (if (and (car rtn) (cadr rtn))
                 (setq rtn t)
               (setq rtn nil))
           (setq rtn (reverse rtn)))
         rtn))

     table-chk-func
     (lambda (table-proto table-cur &optional info)
       (let (rtn (counter 0))
         (while (<= counter (- (length table-proto) 2))
           (let* ((sub-proto (nth counter table-proto))
                  (subv-proto (nth (+ counter 1) table-proto))
                  (sub-cur (nth counter table-cur))
                  (subv-cur (nth (+ counter 1) table-cur))
                  compare-func)
             (setq compare-func (funcall column-chl-func-chose counter table-proto))
             (cl-case compare-func
               ('column-cl-chk-func
                (push (if (not info)
                          (funcall column-cl-chk-func sub-proto sub-cur)
                        (funcall column-cl-chk-func sub-proto sub-cur t))
                      rtn))
               ('column-vt-chk-func
                (push (if (not info)
                          (funcall column-vt-chk-func sub-proto subv-proto sub-cur subv-cur)
                        (funcall column-vt-chk-func sub-proto subv-proto sub-cur subv-cur t))
                      rtn)))
             (setq counter (+ 2 counter))))
         (reverse rtn))))

    (dolist (el exp-proto-tables)
      (let ((cur-table (nth counter-main exp-cur-tables)))
        (if details
            (push (funcall table-chk-func
                           el cur-table t)
                  result)
          (push (funcall table-chk-func
                         el cur-table)
                result)))
      (cl-incf counter-main))
    (setq result (reverse result))
    (when (not details)
      (catch :exit
        (dolist (el result)
          (when (member nil el)
            (setq result nil)
            (throw :exit nil))))
      (if (null result)
          (error "[entropy-prjm]: prj rich expression invalid!")
        (setq result t)))
    result))


;; ** provide
(provide 'entropy-prjm-core)
