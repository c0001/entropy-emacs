(require 'entropy-prjm-core)
(require 'entropy-prjm-uri)
(require 'entropy-prjm-db-cache)
(require 'entropy-prjm-db-chosen)
(require 'entropy-prjm-prj-add)
(require 'entropy-prjm-prj-delete)
(require 'entropy-prjm-prj-update)
(require 'entropy-prjm-sqlite)
(require 'entropy-prjm-interaction)


;; ** dbcc
(setq entropy/prjm-dbcc-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-query-all-prjs)))


;; ** prj delete
(setq entropy/prjm-prjdelete-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-delete-prj)))


;; ** prj add
(setq entropy/prjm-prjadd-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-add-prj)))


;; ** prj update
(setq entropy/prjm-prjupdate-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-update-prj)))


;; ** inct
(setq entropy/prjm-inct-db-chosen-operation-alist
      '(("get-all" . entropy/prjm-dbcs-dbconfig-get)
        ("get-by-name" . entropy/prjm-dbcs-get-dbexp-by-name)))


(setq entropy/prjm-inct-prj-operation-alist
      '(("QUERY-ALL" . entropy/prjm-dbcc-get-dbcahe)
        ("ADD" . entropy/prjm-prjadd-prj-add)
        ("DELETE" . entropy/prjm-prjdelete-prj-delete)
        ("UPDATE" . entropy/prjm-prjupdate-prj-update)
        ("OPEN" . entropy/prjm-uri-open)))


(setq entropy/prjm--inct-prj-name-column :Prj_Name)
(setq entropy/prjm--inct-prj-des-column :Prj_Attr_Des)
(setq entropy/prjm--inct-prj-vcs-column :Prj_Vcs_Type)
(setq entropy/prjm--inct-prj-type-column :Prj_Attr_Type)
(setq entropy/prjm--inct-prj-uri-column :Prj_Attr_Uri)
(setq entropy/prjm--inct-prj-date-column :Prj_Date)


;; ** provide
(provide 'entropy-prjm)
