(require 'entropy-prjm-core)

(defcustom entropy/prjm-prjadd-operation-alist nil
  "alist for prj adding
    '((a . afunc)
      (b . bfunc))
"
  :type 'sexp
  :group 'entropy/prjm-group)


(defun entropy/prjm--prjadd-get-add-operator (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-type (plist-get db-obj :db-type))
         (add-operator (cdr (assoc db-type entropy/prjm-prjadd-operation-alist))))
    (unless (and add-operator
                 (functionp add-operator))
      (error "<<entropy-prjm>>: None valid prj add operator matched for '%s'"
             db-type))
    add-operator))

;;;###autoload
(defun entropy/prjm-prjadd-prj-add (prj-expression db-expression)
  (entropy/prjm-prj-expression-validp prj-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-location (plist-get db-obj :db-location)))
    (funcall (entropy/prjm--prjadd-get-add-operator db-expression)
             prj-expression db-expression)))


(provide 'entropy-prjm-prj-add)
