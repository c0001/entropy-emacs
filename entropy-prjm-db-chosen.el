;;; entropy-prjm-db-chosen.el --- One database choosing backend for entropy-prjm-interaction.el
;;
;; * Copyright (C) 20190511  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-project-manager/blob/master/entropy-prjm-db-chosen.el
;; Package-Version: v0.1.00
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
;; This package was one implementation for
;; =entropy-prjm-interaction='s
;; ~entropy/prjm-inct-db-chosen-operation-alist~.  Further more with
;; the database real-time validation status checking responses.
;;
;; *The validation responses mechanism:*
;;
;; - Initialization status:
;;
;;   In the status for every new session of emacs startup, checking
;;   all user customized db-expressions stored in customized variable
;;   =entropy/prjm-dbcs-user-db-exps= and filterring them into three
;;   status =valid=, =lossy=, and =illegal= named for literal
;;   meaning.
;;
;; - Using status:
;;
;;   Recheck the =valid= and =lossy= status db-expressions, and
;;   re-arranging them into new ones, and error while there's no
;;   valid db-expression.
;; 
;; * Configuration:
;;
;; The "get-all" implementation function is
;; ~entropy/prjm-dbcs-dbconfig-get~ and the "get-by-name"
;; implementation is ~entropy/prjm-dbcs-get-dbexp-by-name~.
;;
;; For quickly configured: 
;; #+BEGIN_SRC elisp
;;   (with-eval-after-load 'entropy-prjm
;;     (setf (alist-get  "get-all" entropy/prjm-inct-db-chosen-operation-alist)
;;           'entropy/prjm-dbcs-dbconfig-get)
;;     (setf (alist-get  "get-by-name" entropy/prjm-inct-db-chosen-operation-alist)
;;           'entropy/prjm-dbcs-get-dbexp-by-name))
;; #+END_SRC
;;
;; * Code:

(require 'entropy-prjm-core)

(defcustom entropy/prjm-dbcs-user-db-exps nil
  "db-expressions for user customized."
  :type 'sexp
  :group 'entropy-prjm-group)

;; runtime db config 
(defvar entropy/prjm--dbcs-db-exp-selected nil)
(defvar entropy/prjm--dbcs-db-valid-exps nil)
(defvar entropy/prjm--dbcs-db-lossy-exps nil)
(defvar entropy/prjm--dbcs-db-illegal-exps nil)

(defun entropy/prjm--dbcs-db-exps-checker (db-expressions)
  (let (db-names-dup-checker
        exp-valid
        exp-lossy
        exp-illegal
        rtn)

    ;; db-names dup checker
    (setq db-names-dup-checker
          (lambda (db-exps)
            (let (db-names)
              (dolist (el db-exps)
                (let ((db-obj (ignore-errors (entropy/prjm-gen-db-obj el))))
                  (when db-obj
                    (push (plist-get db-obj :db-name)
                          db-names))))
              (unless (eq (length db-names)
                          (length (remove-duplicates db-names)))
                (setq rtn t))
              rtn)))

    ;; checking duplicted db item
    (when (funcall db-names-dup-checker db-expressions)
      (error "<<entropy-prjm-db-chosen:>>db-expressions has duplicated item!"))
    
    ;; gen the valid lossy illegal map
    (dolist (el db-expressions)
      (let ((exp-status (entropy/prjm-db-expression-validp el "BRIEF")))
        (cond
         ((equal exp-status "ILLEGAL")
          (push el exp-illegal))
         ((equal exp-status "LOSSY")
          (push el exp-lossy))
         ((equal exp-status "OK")
          (push el exp-valid)))))

    (setq rtn (list :dbs-valid exp-valid
                    :dbs-lossy exp-lossy
                    :dbs-illegal exp-illegal))))

(defun entropy/prjm--dbcs-intialize-db-config ()
  (let* ((db-exps-status (entropy/prjm--dbcs-db-exps-checker entropy/prjm-dbcs-user-db-exps))
         (valid-exps (plist-get db-exps-status :dbs-valid))
         (lossy-exps (plist-get db-exps-status :dbs-lossy))
         (illegal-exps (plist-get db-exps-status :dbs-illegal)))
    (unless valid-exps
      (error "<<entropy-prjm-dbcs>>: Intial none valid db-exps
Please rechecking your speific `entropy/prjm-dbcs-user-db-exps'."))
    (setq entropy/prjm--dbcs-db-valid-exps valid-exps
          entropy/prjm--dbcs-db-lossy-exps lossy-exps
          entropy/prjm--dbcs-db-illegal-exps illegal-exps)))

(defun entropy/prjm--dbcs-dbconfig-runtime-checking ()
  (let (recheck-func
        new-db-config)
    (setq recheck-func
          (lambda (cbk)
            (let ((valid_after (entropy/prjm--dbcs-db-exps-checker
                                entropy/prjm--dbcs-db-valid-exps))
                  (lossy_after (entropy/prjm--dbcs-db-exps-checker
                                entropy/prjm--dbcs-db-lossy-exps))
                  (illegal_after (entropy/prjm--dbcs-db-exps-checker
                                  entropy/prjm--dbcs-db-illegal-exps))
                  valid-new lossy-new illegal-new)
              (setq valid-new (append (plist-get valid_after :dbs-valid)
                                      (plist-get lossy_after :dbs-valid))
                    lossy-new (append  (plist-get valid_after :dbs-lossy)
                                       (plist-get lossy_after :dbs-lossy))
                    illegal-new (append (plist-get valid_after :dbs-illegal)
                                        (plist-get lossy_after :dbs-illegal)))
              (setq valid-new (delete nil valid-new)
                    lossy-new (delete nil lossy-new)
                    illegal-new (delete nil illegal-new))
              (setf (symbol-value cbk)
                    (list
                     :dbs-valid valid-new
                     :dbs-lossy lossy-new
                     :dbs-illegal illegal-new)))))
    (funcall recheck-func 'new-db-config)
    (setq entropy/prjm--dbcs-db-valid-exps
          (plist-get new-db-config :dbs-valid)
          entropy/prjm--dbcs-db-lossy-exps
          (plist-get new-db-config :dbs-lossy)
          entropy/prjm--dbcs-db-illegal-exps
          (plist-get new-db-config :dbs-illegal))))


;;;###autoload 
(defun entropy/prjm-dbcs-dbconfig-get (&optional names-candi)
  (if (or (null entropy/prjm--dbcs-db-valid-exps)
            (= 0 (length entropy/prjm--dbcs-db-valid-exps)))
      (entropy/prjm--dbcs-intialize-db-config)
    (entropy/prjm--dbcs-dbconfig-runtime-checking))
  (when (null entropy/prjm--dbcs-db-valid-exps)
    (entropy/prjm--dbcs-dbconfig-get))
  (cond
   (names-candi
    (let (candi)
      (dolist (el entropy/prjm--dbcs-db-valid-exps)
        (push (car el) candi))
      (reverse candi)))
   (t
    entropy/prjm--dbcs-db-valid-exps)))

(defun entropy/prjm-dbcs-get-dbexp-by-name (db-name)
  (entropy/prjm-dbcs-dbconfig-get)
  (let ((db-exp (assoc db-name entropy/prjm--dbcs-db-valid-exps)))
    db-exp))

(provide 'entropy-prjm-db-chosen)
