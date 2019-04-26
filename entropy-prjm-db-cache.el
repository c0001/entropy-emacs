(require 'entropy-prjm-core)

(defcustom entropy/prjm-dbcc-operation-alist nil
  "entropy db cache operation alist
  
 '((a . afunc)
   (b . bfuncc))
"
  :type 'sexp
  :group 'entropy/prjm-group)

(defvar entropy/prjm--dbcc-dbobjs nil)

(defun entropy/prjm--dbcc-get-db-operator (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-type (plist-get db-obj :db-type))
         (db-location (plist-get db-obj :db-location))
         (db-operation (cdr (assoc db-type entropy/prjm-dbcc-operation-alist))))
    (unless (and db-operation
                 (functionp db-operation))
      (error "<<entropy-prjm>>: cache get func invalid!"))
    db-operation))

(defun entropy/prjm--dbcc-get-dbcache-exp (db-expression)
  (let* ((db-operation (entropy/prjm--dbcc-get-db-operator db-expression))
         rtn)
    (setq rtn (funcall db-operation db-expression))
    (entropy/prjm-db-cache-expression-validp rtn)
    rtn))

(defun entropy/prjm--dbcc-store-dbcache (db-expression)
  (let* ((db-name (plist-get db-expression :db-name))
         (dbc-exp (entropy/prjm--dbcc-get-dbcache-exp db-expression))
         (dbc-obj (entropy/prjm-gen-db-cache-obj dbc-exp)))
    (when (not (assoc db-name entropy/prjm--dbcc-dbobjs))
      (push dbc-obj entropy/prjm--dbcc-dbobjs))))


(defun entropy/prjm--dbcc-update-dbcache (db-expression)
  (let ((db-obj (entropy/prjm-gen-db-obj db-expression))
        (db-name (plist-get db-obj :db-name))
        (has-cached (assoc db-name entropy/prjm--dbcc-dbobjs)))
    (when has-cached
      (setq entropy/prjm--dbcc-dbobjs
            (delete has-cached entropy/prjm--dbcc-dbobjs)))
    (entropy/prjm--dbcc-store-dbcache db-expression)))

(defun entropy/prjm--dbcc-judge-dbcache-obj-obsoleted (db-expression)
  (let ((db-obj (entropy/prjm-gen-db-obj db-expression))
        (db-name (plist-get db-obj :db-name))
        (db-location (plist-get db-obj :db-location))
        (db-cache-obj (assoc db-name entropy/prjm--dbcc-dbobjs))
        dbct-cur dbct-new)
    (setq dbct-cur (cddr db-cache-obj))
    (if (entropy/cl-file-modified db-location dbct-cur)
        t
      nil)))

(defun entropy/prjm--dbcc-get-dbcache-obj (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         rtn)
    (if (assoc db-name entropy/prjm--dbcc-dbobjs)
        (setq rtn (assoc db-name entropy/prjm--dbcc-dbobjs))
      (error "<<entropy-prjm>>: none db-cache mathed as '%s'" db-name))
    rtn))

;;;###autoload
(defun entropy/prjm-dbcc-get-dbcahe (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         (db-location (plist-get db-obj :db-location))
         (db-cache-obj (assoc db-name entropy/prjm--dbcc-dbobjs)))
    (if db-cache-obj
        (when (entropy/prjm--dbcc-judge-dbcache-obj-obsoleted db-expression)
          (entropy/prjm--dbcc-update-dbcache db-expression))
      (entropy/prjm--dbcc-store-dbcache db-expression))
    (entropy/prjm--dbcc-get-dbcache-obj db-expression)))


(provide 'entropy-prjm-db-cache)


