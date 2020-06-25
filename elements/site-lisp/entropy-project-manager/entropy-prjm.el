;;; entropy-prjm.el ---  The integrated package for entropy project management subs
;;
;;; Copyright (C) 20190511  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-project-manager/blob/master/entropy-prjm.el
;; Package-Version: v0.1.0
;; Package-Requires: ((entropy-common-library "0.1.0") (ivy "0.12.0"))
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
;;; Commentary:
;;
;; This package was the integrated centre for wrappering stuffs into
;; one enforceable project management, stuffs consists of default
;; utilities to =entropy-project-management=:
;;
;; *BASE:*
;; - the core: =entropy-prjm-core.el=
;; - the interaction: =entropy-prjm-interactin.el=
;;
;; *DB CHOSEN and CACHE:*
;; - db-cache: =entropy-prjm-db-cache.el=
;; - db-chosen: =entropy-prjm-db-chosen.el=
;;
;; *PRJ MANIPULATION BACKENDS:*
;; - add: =entropy-prjm-prj-add.el=
;; - delete: =entropy-prjm-prj-delete.el=
;; - update: =entropy-prjm-prj-update.el=
;;
;; *DB COMMUNICATION BACKENDS*
;; - sqlite: =entropy-prjm-sqlite.el=
;;
;;; Configuration:
;;
;; Following below simple config snippet:
;; #+BEGIN_SRC elisp
;;   ;; for traditionally config type
;;   (require 'entropy-prjm)
;;   (setq entropy/prjm-dbcs-user-db-exps
;;           '(("prjs-collection 01" "sqlite" "path-to-this-database" nil nil)
;;             ("prjs-collection 02" "sqlite" "path-to-this-database" nil nil)))
;;
;;   ;; use-package
;;   (use-package entropy-prjm
;;     :commands (entropy/prjm-inct-chosen-db)
;;     :init
;;     (setq entropy/prjm-dbcs-user-db-exps
;;           '(("prjs-collection 01" "sqlite" "path-to-this-database" nil nil)
;;             ("prjs-collection 02" "sqlite" "path-to-this-database" nil nil))))
;; #+END_SRC
;;
;; For now just support 'sqlite3' database communicatiion backends,
;; you can using =M-x entropy/prjm-sqlite-create-databse= for create
;; new database if you have none.
;;
;;; Code:


(require 'entropy-prjm-core)
(require 'entropy-prjm-uri)
(require 'entropy-prjm-db-cache)
(require 'entropy-prjm-db-chosen)
(require 'entropy-prjm-prj-add)
(require 'entropy-prjm-prj-delete)
(require 'entropy-prjm-prj-update)
(require 'entropy-prjm-sqlite)
(require 'entropy-prjm-interaction)

(defgroup entropy-prjm-group nil
  "Top customized group for `entropy-prjm'."
  :group 'extensions)

;;;; dbcc
(setq entropy/prjm-dbcc-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-query-all-prjs)))


;;;; prj delete
(setq entropy/prjm-prjdelete-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-delete-prj)))


;;;; prj add
(setq entropy/prjm-prjadd-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-add-prj)))


;;;; prj update
(setq entropy/prjm-prjupdate-operation-alist
      '(("sqlite" . entropy/prjm-sqlite-update-prj)))


;;;; inct
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


;;;; provide
(provide 'entropy-prjm)
