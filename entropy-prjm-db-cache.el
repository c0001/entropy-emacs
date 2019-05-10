;;; entropy-prjm-db-cache.el --- One entropy-prjm-interaction database query-all backend
;;
;; * Copyright (C) 20190510  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-project-manager/blob/master/entropy-prjm-db-cache.el
;; Package-Version: v0.1.0
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
;; This package designed based on ~entropy-prjm-interaction~'s api,
;; that basically satisfied the 'QUERY-ALL' operation, but with
;; data-base caching and cache updating feature.
;; 
;; For supporting for multi-type database manipulation types, this
;; package provide the api for as, customized variable
;; =entropy/prjm-dbcc-operation-alist=, which was the alist whose each
;; key was the database type e.g. =sqlite=, =org= and so on, the
;; associated value was function with single argument for
;; =db-expression=.
;; 
;; * Configuration:
;;
;; Assign function ~entropy/prjm-dbcc-get-dbcahe~ into
;; =entropy/prjm-inct-prj-operation-alist='s "QUERY-ALL" cdr clause.
;;
;; #+BEGIN_SRC elisp
;;   (with-eval-after-load 'entropy-prjm
;;     (setf (alist-get "QUERY-ALL" entropy/prjm-inct-prj-operation-alist)
;;           'entropy/prjm-dbcc-get-dbcahe))
;; #+END_SRC
;;
;; * Code:


(require 'entropy-prjm-core)

(defcustom entropy/prjm-dbcc-operation-alist nil
  "The alist stored the 'key' database-type and the 'assoc'
=query-all= function matched by the key type.

The key was string e.g. \"sqlite\", \"org\". Its associated value
was a function formed using single argument as one
_db-expression_."
  :type 'sexp
  :group 'entropy/prjm-group)

(defvar entropy/prjm--dbcc-dbcobjs nil)

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
    (when (not (assoc db-name entropy/prjm--dbcc-dbcobjs))
      (push dbc-obj entropy/prjm--dbcc-dbcobjs))))


(defun entropy/prjm--dbcc-update-dbcache (db-expression)
  (let ((db-obj (entropy/prjm-gen-db-obj db-expression))
        (db-name (plist-get db-obj :db-name))
        (has-cached (assoc db-name entropy/prjm--dbcc-dbcobjs)))
    (when has-cached
      (setq entropy/prjm--dbcc-dbcobjs
            (delete has-cached entropy/prjm--dbcc-dbcobjs)))
    (entropy/prjm--dbcc-store-dbcache db-expression)))

(defun entropy/prjm--dbcc-judge-dbcache-obj-obsoleted (db-expression)
  (let ((db-obj (entropy/prjm-gen-db-obj db-expression))
        (db-name (plist-get db-obj :db-name))
        (db-location (plist-get db-obj :db-location))
        (db-cache-obj (assoc db-name entropy/prjm--dbcc-dbcobjs))
        dbct-cur dbct-new)
    (setq dbct-cur (cddr db-cache-obj))
    (if (entropy/cl-file-modified db-location dbct-cur)
        t
      nil)))

(defun entropy/prjm--dbcc-get-dbcache-obj (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         rtn)
    (if (assoc db-name entropy/prjm--dbcc-dbcobjs)
        (setq rtn (assoc db-name entropy/prjm--dbcc-dbcobjs))
      (error "<<entropy-prjm>>: none db-cache mathed as '%s'" db-name))
    rtn))

;;;###autoload
(defun entropy/prjm-dbcc-get-dbcahe (db-expression)
  (let* ((db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         (db-location (plist-get db-obj :db-location))
         (db-cache-obj (assoc db-name entropy/prjm--dbcc-dbcobjs)))
    (if db-cache-obj
        (when (entropy/prjm--dbcc-judge-dbcache-obj-obsoleted db-expression)
          (entropy/prjm--dbcc-update-dbcache db-expression))
      (entropy/prjm--dbcc-store-dbcache db-expression))
    (entropy/prjm--dbcc-get-dbcache-obj db-expression)))


(provide 'entropy-prjm-db-cache)


