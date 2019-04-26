(require 'entropy-prjm-core)

(defcustom entropy/prjm-prjupdate-operation-alist nil
  "alist for opertor for update prj
   '((a . afunc)
     (b . bfunc))
"
  :type 'sexp
  :group 'entropy/prjm-group)

(defun entropy/prjm--prjupdate-get-update-operator (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-type (plist-get db-obj :db-type))
         (update-operator (cdr (assoc db-type entropy/prjm-prjupdate-operation-alist))))
    (unless (and update-operator
                 (functionp update-operator))
      (error "<<entropy-prjm>>: None valid update operator matched for '%s'"
             db-type))
    update-operator))


;;;###autoload
(defun entropy/prjm-prjupdate-prj-update (prj-expression db-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-location (plist-get db-obj :db-location)))
    (funcall (entropy/prjm--prjupdate-get-update-operator db-expression)
             prj-expression db-expression)))

(provide 'entropy-prjm-prj-update)
