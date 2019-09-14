;;;; require
(require 'entropy-prjm-core)

;;;; defcustom

;;;; defvar
;;;;; database creation script file
(defvar entropy/prjm--sqlite-dbnew-script-file
  (expand-file-name "entropy-prjm-sqlite-initialize.sql"
                    (file-name-directory load-file-name)))

;;;;; sqlite cmd format
(defvar entropy/prjm--sqlite-cmd-format
  "sqlite3 -separator \"|||\" \"%s\" \"%s\"")

;;;;; sqlite statements
(defvar entropy/prjm--sqlite-add-prj-statement-form
  "INSERT INTO %s (%s) VALUES (%s);")

(defvar entropy/prjm--sqlite-delete-prj-statement-form
  "DELETE FROM %s WHERE %s;")

(defvar entropy/prjm--sqlite-update-prj-statement-form
  "UPDATE %s SET %s WHERE %s;")

(defvar entropy/prjm--sqlite-query-all-prj-statement-form
  "SELECT %s FROM %s WHERE %s;")

;;;;; sqlite value type
(defvar entropy/prjm--sqlite-statement-value-type
  (list
   (cons "INT" (list :db-des "Integer"
                     :method #'(lambda (str)
                                 (format "%s" (or str "")))))
   (cons "TEXT" (list :db-des "text string"
                      :method #'(lambda (str)
                                  (format "'%s'" (or str "")))))
   (cons "VARCHAR"  (list :db-des "string"
                          :method #'(lambda (str)
                                      (format "'%s'" (or str "")))))))

;;;; utilities
(defun entropy/prjm--sqlite-transfer-prop-to-string (prop)
  (let (rtn)
    (unless (string-match "^:" (symbol-name prop))
      (error "Prop must format as ':xxx'!"))
    (setq rtn
          (replace-regexp-in-string
           "^:" "" (symbol-name prop)))))
;;;; Library
;;;;; sqlite callback state filter
(defun entropy/prjm--sqlite-cbk-filter (cbk)
  (let (cbk-split-group
        cbk-split-list
        temp_var rtn
        (key-seq (entropy/prjm-prj-column-expression-prototype)))
    (setq cbk-split-list
          (split-string cbk "\n" t))
    (dolist (el cbk-split-list)
      (setq temp_var
            (split-string el "|||" nil))
      (when temp_var
        (push temp_var cbk-split-group)))
    (setq cbk-split-group (reverse cbk-split-group))
    (dolist (el cbk-split-group)
      (let ((plist-temp  (copy-tree key-seq))
            (plist-rtn (copy-tree key-seq))
            (counter 0))
        (dotimes (el2 (/ (length key-seq) 2) nil)
          (setf (car (plist-get plist-rtn (nth counter plist-temp)))
                (if (equal (nth el2 el) "")
                    nil
                  (nth el2 el)))
          (setq counter (+ 2 counter)))
        (push plist-rtn rtn)))
    (reverse rtn)))

;;;;; prj object parser
;;;;;; common library
(defun entropy/prjm--sqlite-format-value (value value-type)
  (unless (stringp value-type)
     (error "Formatting sqlite data type attributce must be string."))
  (let ((format-method
         (plist-get (cdr (assoc value-type entropy/prjm--sqlite-statement-value-type))
                    :method)))
    (unless format-method
      (error "Type '%s' doesn't matched!" value-type))
    (funcall format-method value)))

(defun entropy/prjm--sqlite-concat-list (list-var &optional separator)
  (let (rtn)
    (unless separator
      (setq separator ", "))
    (dolist (el list-var)
      (let ((item el))
        (cond
         ((numberp item)
          (setq item (number-to-string item)))
         ((not (stringp item))
          (error "[entropy/prjm--sqlite-concat-list]: wrong type of list item.")))
        (cond
         ((null rtn)
          (setq rtn item))
         (t
          (setq rtn (concat rtn separator item))))))
    rtn))

;;;;;; main utilities
;;;;;;; keyval group
(defun entropy/prjm--sqlite-gen-keyval-group-list (prj-rich-expression)
  (let* ((operation (car prj-rich-expression))
         (shaft-rich (cadr prj-rich-expression))
         (shaft-column-name (plist-get shaft-rich :column))
         (shaft-value-fmstr (entropy/prjm--sqlite-format-value
                             (plist-get shaft-rich :value)
                             (plist-get shaft-rich :value-type)))
         (prj-tables (cddr prj-rich-expression))
         (prj-tables-map (cdddr (entropy/prjm-prj-counter-region-map
                                 (entropy/prjm-prj-obj-prototype))))
         (prj-tables-mapped (entropy/cl-capture-list-by-region-map prj-tables prj-tables-map))
         group-list)
    
    (dolist (el prj-tables-mapped)
      (let ((table-name (plist-get (car el) :table))
            value-fmstr-list column-name-list)
        (dolist (el2 el)
          (let ((value-fmstr (entropy/prjm--sqlite-format-value
                              (plist-get el2 :value)
                              (plist-get el2 :value-type)))
                (column-name (plist-get el2 :column)))
            (push value-fmstr value-fmstr-list)
            (push column-name column-name-list)))
        (setq value-fmstr-list (reverse value-fmstr-list)
              column-name-list (reverse column-name-list))
        (push shaft-value-fmstr value-fmstr-list)
        (push shaft-column-name column-name-list)
        (push (cons table-name (cons (entropy/prjm--sqlite-concat-list column-name-list)
                                     (entropy/prjm--sqlite-concat-list value-fmstr-list)))
              group-list)))
    (setq group-list (reverse group-list))
    (push operation group-list)))


;;;;;;; keyval eqstm
(defun entropy/prjm--sqlite-gen-keyval-eqstm-list (prj-rich-expression)
  (let* ((operation (car prj-rich-expression))
         (shaft-rich (cadr prj-rich-expression))
         (shaft-column-name (plist-get shaft-rich :column))
         (shaft-value-fmstr (entropy/prjm--sqlite-format-value
                             (plist-get shaft-rich :value)
                             (plist-get shaft-rich :value-type)))
         (prj-tables (cddr prj-rich-expression))
         (prj-tables-map (cdddr (entropy/prjm-prj-counter-region-map
                                (entropy/prjm-prj-obj-prototype))))
         (prj-tables-mapped (entropy/cl-capture-list-by-region-map prj-tables prj-tables-map))
         eqstm-list)
    (dolist (el prj-tables-mapped)
      (let ((table-name (plist-get (car el) :table))
            eqstm-item-list concated-eqstms)
        (dolist (el2 el)
          (let ((value-fmstr (entropy/prjm--sqlite-format-value
                              (plist-get el2 :value)
                              (plist-get el2 :value-type)))
                (column-name (plist-get el2 :column)))
            (push (format "%s = %s" column-name value-fmstr)
                  eqstm-item-list)))
        (setq eqstm-item-list (reverse eqstm-item-list))
        (push (format "%s = %s" shaft-column-name shaft-value-fmstr) eqstm-item-list)
        (dolist (el3 eqstm-item-list)
          (cond
           ((null concated-eqstms)
            (setq concated-eqstms el3))
           (t
            (setq concated-eqstms (concat concated-eqstms ", " el3)))))
        (push (cons table-name concated-eqstms) eqstm-list)))
    (setq eqstm-list (reverse eqstm-list))
    (push (format "%s = %s" shaft-column-name shaft-value-fmstr) eqstm-list)
    (push operation eqstm-list)))



;;;;; prj object transfer to sqlite statement
;;;;;; insert prj statement generator
(defun entropy/prjm--sqlite-gen-insert-statement (prj-expression)
  (let* ((prj-rich-expression (entropy/prjm-gen-prj-rich-expression prj-expression))
         (prj-rich-insert-expression (entropy/prjm--sqlite-gen-keyval-group-list
                                      prj-rich-expression))
         (statement-formt entropy/prjm--sqlite-add-prj-statement-form)
         statements-list)
    (dolist (el (cdr prj-rich-insert-expression))
      (push (format statement-formt
                    (car el)
                    (nth 1 el)
                    (cddr el))
            statements-list))
    (setq statements-list (entropy/prjm--sqlite-concat-list (reverse statements-list)
                                                             " "))))
;;;;;; update prj statement generator
(defun entropy/prjm--sqlite-gen-update-statement (prj-expression)
  (let* ((prj-rich-expression (entropy/prjm-gen-prj-rich-expression prj-expression))
         (prj-rich-update-expression (entropy/prjm--sqlite-gen-keyval-eqstm-list
                                      prj-rich-expression))
         (statement-formt entropy/prjm--sqlite-update-prj-statement-form)
         (shaft-statement (cadr prj-rich-update-expression))
         statements-list)
    (dolist (el (cddr prj-rich-update-expression))
      (push (format statement-formt
                    (car el)
                    (cdr el)
                    shaft-statement)
            statements-list))
    (setq statements-list
          (entropy/prjm--sqlite-concat-list
           (reverse statements-list)
           " "))))

;;;;;; deletion prj statement
(defun entropy/prjm--sqlite-gen-delete-statement (prj-expression)
  (let* ((prj-rich-expression (entropy/prjm-gen-prj-rich-expression
                               prj-expression))
         (shaft-place (cadr prj-rich-expression))
         (shaft-value-fmstr (entropy/prjm--sqlite-format-value
                             (plist-get shaft-place :value)
                             (plist-get shaft-place :value-type)))
         (prj-obj-map (cdddr (entropy/prjm-prj-counter-region-map
                              (entropy/prjm-prj-obj-prototype))))
         (statement-format entropy/prjm--sqlite-delete-prj-statement-form)
         tables-name-list statements-list)
    (let ((index-point 2)
          (index-past-point-sum 0)
          (index-step (length prj-obj-map))
          (map-point 0))
      (dotimes (el index-step nil)
        (push (plist-get (nth index-point prj-rich-expression)
                         :table)
              tables-name-list)
        (setq index-point (ignore-errors (+ (nth map-point prj-obj-map) index-point))
              map-point (+ 1 map-point))))
    (dolist (el tables-name-list)
      (push (format statement-format
                    el (format "%s = %s"
                               (plist-get shaft-place :column)
                               shaft-value-fmstr))
            statements-list))
    (setq statements-list
          (entropy/prjm--sqlite-concat-list
           statements-list " "))))

;;;;;; query all prj statement
(defun entropy/prjm--sqlite-gen-query-all-statement ()
  (let* ((prj-obj-prototype (entropy/prjm-prj-obj-prototype))
         (shaft-place (cdr (assoc 'Shaft prj-obj-prototype)))
         (shaft-column-name (entropy/prjm--sqlite-transfer-prop-to-string
                             (car (plist-get shaft-place :columns))))
         (tables-place (cdr (assoc 'Tables prj-obj-prototype)))
         tables-name-list obj-list
         tables-name-concat obj-concat
         shaft-obj-list
         where-list
         where-statement
         (counter 0))

    (dolist (el tables-place)
      (let ((table-name (plist-get el :table))
            (columns (plist-get el :columns))
            columns-name-list
            obj-temp-list)
        (push table-name tables-name-list)
        (dolist (el2 columns)
          (when (and (symbolp el2)
                     (string-match-p "^:" (symbol-name el2)))
            (push (entropy/prjm--sqlite-transfer-prop-to-string el2) columns-name-list)))
        (dolist (el2 columns-name-list)
          (push (format "%s.%s" table-name el2)
                obj-temp-list))
        (setq obj-list (append obj-list obj-temp-list))))
    
    (setq tables-name-list (reverse tables-name-list))
    (setq tables-name-concat (entropy/prjm--sqlite-concat-list tables-name-list))
    (dolist (el tables-name-list)
      (push (format "%s.%s" el shaft-column-name)
            shaft-obj-list))
    (setq shaft-obj-list (reverse shaft-obj-list))
    (push (car shaft-obj-list) obj-list)
    (setq obj-concat (entropy/prjm--sqlite-concat-list obj-list))
    (while (and (<  (+ 1 counter) (length tables-name-list))
                (<= (+ 2 counter) (length tables-name-list)))
      (push (format "%s = %s"
                    (format "%s.%s"
                            (nth counter tables-name-list)
                            shaft-column-name)
                    (format "%s.%s"
                            (nth (+ 1 counter) tables-name-list)
                            shaft-column-name))
            where-list)
      (cl-incf counter))
    (setq where-statement
          (entropy/prjm--sqlite-concat-list
           (reverse where-list)
           " AND "))
    (format entropy/prjm--sqlite-query-all-prj-statement-form
            obj-concat tables-name-concat where-statement)))


;;;;; prj operation function
;;;;;; create empty database file
(defun entropy/prjm-sqlite-create-databse ()
  (interactive)
  (let ((db-location
         (entropy/cl-read-file-name "file"))
        (shell_cmd (concat "sqlite3 %s < " entropy/prjm--sqlite-dbnew-script-file)))
    (shell-command (format shell_cmd db-location))))

;;;;;; query all prjs
(defun entropy/prjm-sqlite-query-all-prjs (db-expression)
  (let* ((query-statement (entropy/prjm--sqlite-gen-query-all-statement))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-location (plist-get db-obj :db-location))
         (db-name (plist-get db-obj :db-name))
         (query-cmd (format entropy/prjm--sqlite-cmd-format
                            (expand-file-name db-location)
                            query-statement))
         cbk cbk-status)
    (setq cbk (shell-command-to-string query-cmd))
    (when (ignore-errors (string-match "^Error:" cbk))
      (error "Query db '%s' with error '%s'." db-name cbk))
    (setq cbk (entropy/prjm--sqlite-cbk-filter cbk))
    (setq cbk (list db-name cbk
                    (entropy/cl-file-last-modification-time db-location)))))

;;;;;; Add prj
(defun entropy/prjm-sqlite-add-prj (prj-expression db-expression &optional non-cbk-status)
  (let* ((add-statement (entropy/prjm--sqlite-gen-insert-statement prj-expression))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (sqlite-cmd (format entropy/prjm--sqlite-cmd-format
                             (expand-file-name (plist-get db-obj
                                                          :db-location))
                             add-statement))
         cbk-status)
    (setq cbk-status
          (shell-command-to-string
           sqlite-cmd))
    (unless non-cbk-status
      (cond
       ((not (string-match "^Error: " cbk-status))
        (message "Add prj '%s' successfully" prj-expression))
       ((string-match "^Error: " cbk-status)
        (message "Add prj '%s' \n =====> with error '%s'" prj-expression cbk-status))))))


;;;;;; Delete prj
(defun entropy/prjm-sqlite-delete-prj (prj-expression db-expression &optional non-cbk-status)
  (let* ((delete-statement
          (entropy/prjm--sqlite-gen-delete-statement prj-expression))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         (sqlite-cmd (format entropy/prjm--sqlite-cmd-format
                             (expand-file-name (plist-get db-obj
                                                          :db-location))
                             delete-statement))
         cbk-staus)
    (setq cbk-status (shell-command-to-string sqlite-cmd))
    (unless non-cbk-status
      (cond
       ((not (string-match "^Error: " cbk-status))
        (message "Delete prj '%s' successfully" prj-expression))
       ((string-match "^Error: " cbk-status)
        (message "Delete db '%s' \n =====> with error '%s'" prj-expression cbk-status))))))

;;;;;; Update prj 
(defun entropy/prjm-sqlite-update-prj (prj-expression db-expression &optional non-cbk-status)
  (let* ((update-statement
          (entropy/prjm--sqlite-gen-update-statement prj-expression))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (sqlite-cmd (format entropy/prjm--sqlite-cmd-format
                             (expand-file-name (plist-get db-obj
                                                          :db-location))
                             update-statement))
         cbk-status)
    (setq cbk-status (shell-command-to-string sqlite-cmd))
    (unless non-cbk-status
      (cond
       ((not (string-match "^Error: " cbk-status))
        (message "Update prj '%s' successfully" prj-expression))
       ((string-match "^Error: " cbk-status)
        (message "Update prj '%s' \n =====>with error '%s'" prj-expression cbk-status))))))



;;;; provide
(provide 'entropy-prjm-sqlite)
