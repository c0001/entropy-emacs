(require 'entropy-prjm-core)

(defcustom entropy/prjm-prjdelete-operation-alist nil
  "alist for prj deleteing
    '((a . afunc)
      (b . bfunc))
"
  :type 'sexp
  :group 'entropy/prjm-group)


(defun entropy/prjm--prjdelete-get-delete-operator (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-type (plist-get db-obj :db-type))
         (delete-operator (cdr (assoc db-type entropy/prjm-prjdelete-operation-alist))))
    (unless (and delete-operator
                 (functionp delete-operator))
      (error "<<entropy-prjm>>: None valid prj delete operator matched for '%s'"
             db-type))
    delete-operator))

;;;###autoload
(defun entropy/prjm-prjdelete-prj-delete (prj-expression db-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-location (plist-get db-obj :db-location)))
    (funcall (entropy/prjm--prjdelete-get-delete-operator db-expression)
             prj-expression db-expression)))


(provide 'entropy-prjm-prj-delete)
