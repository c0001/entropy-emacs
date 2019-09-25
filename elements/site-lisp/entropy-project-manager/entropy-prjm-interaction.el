;;; entropy-prjm-interaction.el --- The interface for entropy-project-manager
;;; Copyright
;; #+BEGIN_EXAMPLE
;; Copyright (C) 20190504  Entropy
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-project-manager/blob/master/entropy-prjm-interaction.el
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
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;; 
;;; Commentary:
;; This package was the default interface of =entropy-prjm-core=, as
;; the same prominent position as =entropy-prjm-core= does.Packages
;; gives open APIs for development to building their own 'entropy
;; project management backend'.
;;
;; OFC, as needed for every =entropy-project-management= referrenced
;; packages, this package requiring (deriving from)
;; =entropy-prjm-core=. Using its prototype and expanding them to
;; building interaction apis framework. 
;;
;; The interaction aspects of entropy-project-management are:
;; - QUERY-ALL
;; - ADD
;; - DELETE
;; - UPDATE
;; - OPEN
;;
;; All of above interaction action in this package are called
;; 'operation' as the reflection to =prj-object='s operation
;; clause. Each of them is a declaration function with respective
;; arguments and value type returning request, they are the first api
;; of this package. See the function declaration table below:
;;
;; | *operation* | *arguments*                        | *return*         |
;; |-------------+------------------------------------+------------------|
;; | QUERY-ALL   | single: =db-expression=            | =db-cache-obj=   |
;; |-------------+------------------------------------+------------------|
;; | ADD         | 0st: =prj-expression=              | operation status |
;; |             | 1st: =db-expression=               |                  |
;; |-------------+------------------------------------+------------------|
;; | DELETE      | 0st: =prj-expression=              | operation status |
;; |             | 1st: =db-expression=               |                  |
;; |-------------+------------------------------------+------------------|
;; | UPDATE      | 0st: =prj-expression=              | operation status |
;; |             | 1st: =db-expression=               |                  |
;; |-------------+------------------------------------+------------------|
;; | OPEN        | 0st: uri of one specific project   | none             |
;; |             | 1st: database location path string |                  |
;; |-------------+------------------------------------+------------------|
;;
;; All the operation interface defined in customized variable
;; ~entropy/prjm-inct-prj-operation-alist~ as the alist type, which
;; the key was the operation string.
;;
;; There's some unintelligible as of operation "OPEN", the second
;; argument =database location= may confused you, but actually easily
;; to acceptable for:
;;
;; #+BEGIN_QUOTE
;; For any project location compatible in consistency rule sets,
;; 'entropy-prjm' using the =UNIFORM RESOURCE IDENTIFIER= (the [[https://en.wikipedia.org/wiki/Uniform_Resource_Identifier][URI]])
;; to identify its location, which given the rule set for both remote
;; type resource type and local one, but for what shown for commonly
;; using for that:
;;
;;   Some status we do not using absolute path string, not to say of
;;   using the uri type local file path formular.
;;
;; Thus, the database location was used for the natively relative
;; project path relatively with the data-base location, causing for
;; some situations that user orgnize some projects relative by
;; database location for portable purpose.
;;
;; (*natively relative path format:* ../[../ ...]xxxx or ./xxx)
;; #+END_QUOTE  
;; 
;; For operation =QUERY-ALL=, the only argument it recieved is one
;; =db-expression=, but this package doesn't give the database
;; managing feature, thus for this, another api 'database retrieving
;; operation'. There's two operation type for it:
;; 
;; - "get-all" operation
;;
;;   Require non-argument, as that this function need to returns all
;;   *valid* db-expressions, as list type.
;;
;;   Notice for the emphasized word *valid*, means the the returned
;;   db-expressions must passed check individually by
;;   ~entropy/prjm-db-expression-validp~.
;;
;; - "get-by-name" operation
;;
;;   This function requried one string type argument, for matcing the
;;   corresponding db-expression as that this string was the db-name
;;   of that database. 
;;
;; For the instance for databse-retrieveing function, by the requests
;; of above database operation type list, there needs two instance
;; for calling for, all you can using the lambda for wrapping your
;; single function of which has the optioanl argument for satisfiying
;; the 'get-by-name' operation type.
;;
;; Finally as that the origin prj object prototpe is extensible, thus
;; for that this package needed the exhibited sets of generally used
;; column given for:
;;
;; - ~entropy/prjm--inct-prj-name-column~
;; - ~entropy/prjm--inct-prj-des-column~
;; - ~entropy/prjm--inct-prj-vcs-column~
;; - ~entropy/prjm--inct-prj-type-column~
;; - ~entropy/prjm--inct-prj-uri-column~
;; - ~entropy/prjm--inct-prj-date-column~
;;
;; Caused from that, commonly, each project description must have
;; it's name, short description, version control system type,
;; uri(location indication), and built date whatever data structer it
;; is. You should assign each value of above list customized
;; variable, notice for that all of them are type of plist-key
;; e.g. =:Prj_Name=.
;;
;;  
;;
;;; Configuration:
;;
;; configuration

;;; Code:

(require 'ivy)
(require 'entropy-prjm-core)
(require 'all-the-icons)
(require 'url)

;;;; defcustom
(defcustom entropy/prjm-inct-db-chosen-operation-alist
  '(("get-all" . nil)
    ("get-by-name" . nil))
  "The database manage function alist, which the associate key
was 'operation type'.

Operation type are:
- \"get-all\"
- \"get-by-name\"


The associated value of each key was one corresponding function
with speicfic interface regulation. 

*get-all* type operation:

Function with no arguments, return list of =db-expression= (see
`entropy/prjm-db-expression-prototype'). 

*get-by-name* type operation:

Function with one string type argument, the databse name, return
corresponding =db-expression=."
  :type 'sexp
  :group 'entropy/prjm-group)

(defcustom entropy/prjm-inct-prj-operation-alist
  '(("QUERY-ALL" . nil)
    ("ADD" . nil)
    ("DELETE" . nil)
    ("UPDATE" . nil)
    ("OPEN" . nil))
  "Alist of for project interaction operations.

For `entropy-prjm-interaction', there're five standard operation
for manipulating with project database:

- \"QUERY-ALL\": Query projects information of single database
- \"ADD\": Add prj into specific database. 
- \"DELETE\": Delete prj into specific database. 
- \"UPDATE\": Update prj into specific database. 
- \"OPEN\": Open project's location. 

As that, each type of value is function, and the instanced
rule-set shown in below table:

| *operation* | *arguments*                        | *return*         |
|-------------+------------------------------------+------------------|
| QUERY-ALL   | single: =db-expression=            | =db-cache-obj=   |
|-------------+------------------------------------+------------------|
| ADD         | 0st: =prj-expression=              | operation status |
|             | 1st: =db-expression=               |                  |
|-------------+------------------------------------+------------------|
| DELETE      | 0st: =prj-expression=              | operation status |
|             | 1st: =db-expression=               |                  |
|-------------+------------------------------------+------------------|
| UPDATE      | 0st: =prj-expression=              | operation status |
|             | 1st: =db-expression=               |                  |
|-------------+------------------------------------+------------------|
| OPEN        | 0st: uri of one specific project   | none             |
|             | 1st: database location path string |                  |
|-------------+------------------------------------+------------------|


There's some unintelligible as of operation \"OPEN\", the second
argument =database location= may confused you, but actually
easily to acceptable for:

#+BEGIN_QUOTE
For any project location compatible in consistency rule sets,
'entropy-prjm' using the =UNIFORM RESOURCE IDENTIFIER= (the [[https://en.wikipedia.org/wiki/Uniform_Resource_Identifier][URI]])
to identify its location, which given the rule set for both remote
type resource type and local one, but for what shown for commonly
using for that:

Some status we do not using absolute path string, not to say of
using the uri type local file path formular.

Thus, the database location was used for the natively relative
project path relatively with the data-base location, causing for
some situations that user orgnize some projects relative by
database location for portable purpose.

(*natively relative path format:* ../[../ ...]xxxx or ./xxx)
#+END_QUOTE  
"
  :type 'sexp
  :group 'entropy/prjm-group)

;;;; defvar
(defvar entropy/prjm--inct-all-the-icons (if (display-graphic-p) t nil))
(defvar entropy/prjm--inct-uri-added-log nil)


;;;;; default prj info key
(defvar entropy/prjm--inct-prj-name-column nil)
(defvar entropy/prjm--inct-prj-des-column nil)
(defvar entropy/prjm--inct-prj-vcs-column nil)
(defvar entropy/prjm--inct-prj-type-column nil)
(defvar entropy/prjm--inct-prj-uri-column nil)
(defvar entropy/prjm--inct-prj-date-column nil)


;;;;; interaction pointer icon
(defvar entropy/prjm--inct-ui-pointer-style
  (if (display-graphic-p)
      (cons (concat (all-the-icons-faicon "hand-o-right") " ") "   ")
    (cons "--> " "    ")))

;;;;; temporally db refer var
(defvar entropy/prjm--inct-temp-dbco  nil)
(defvar entropy/prjm--inct-temp-prjs-candi-alist nil)
(defvar entropy/prjm--inct-selected-db-name nil)

;;;;; prj template folder place
(defvar entropy/prjm--inct-prj-template
  (expand-file-name "prj-template" (file-name-directory load-file-name)))

;;;; library
;;;;; common library
(defun entropy/prjm--inct-get-db-prj-operator (prj-operation)
  (let* ((operator (cdr (assoc prj-operation entropy/prjm-inct-prj-operation-alist))))
    (unless (and operator
                 (functionp operator))
      (error "<<entropy-prjm>>: None valid prjm backend matched for '%s'"
             operation))
    operator))

(defun entropy/prjm--inct-do-db-prj-operation (prj-expression db-expression)
  (let* ((operation (car prj-expression))
         (operator (entropy/prjm--inct-get-db-prj-operator operaion)))
    (cond
     ((equal operation "QUERY-ALL")
      (funcall operator db-expression))
     (t
      (funcall operator prj-expression db-expression)))))


(defun entropy/prjm--inct-get-db-chosen-operator (db-chosen-type)
  (let ((operator (cdr (assoc db-chosen-type
                              entropy/prjm-inct-db-chosen-operation-alist))))
    (unless (and operator (functionp operator))
      (error "<<entropy-prjm>>: None valid prjm backend matched for '%s'"
             db-chosen-type))
    operator))

(defun entropy/prjm--inct-match-prj-icon (prj-type)
  (let (rtn)
    (if (not (display-graphic-p))
        ""
      (setq rtn (or (ignore-errors (all-the-icons-fileicon prj-type))
                    (ignore-errors (all-the-icons-alltheicon prj-type))
                    (ignore-errors (all-the-icons-faicon prj-type))
                    (ignore-errors (all-the-icons-material prj-type))
                    (ignore-errors (all-the-icons-octicon prj-type))
                    (ignore-errors (all-the-icons-wicon prj-type))))
      (cond ((not (null rtn))
             (format "%s\t%s" rtn " "))
            (t
             (concat (all-the-icons-faicon "folder") "\t "))))))

(defun entropy/prjm--inct-get-prj-attrs ()
  (let ((prj-column-exp (entropy/prjm-prj-column-expression-prototype))
        rtn (counter 0))
    (while (< counter (length prj-column-exp))
      (let* ((column (nth counter prj-column-exp))
             (column-name (replace-regexp-in-string
                           "^:" "" (symbol-name column))))
        (push (cons column-name column) rtn)
        (setq counter (+ 2 counter))))
    (reverse rtn)))

(defun entropy/prjm--inct-read-string (prompt-str &optional origin initial default candi)
  (let ((prompt (concat prompt-str
                        (if origin
                            (concat " " "(" origin ")")
                          "")
                        ": ")))
    (cond
     ((not candi)
      (read-string prompt initial 'minibuffer-history default))
     (candi
      (completing-read prompt candi nil t initial 'minibuffer-history default)))))

(defun entropy/prjm--inct-sorting-colist (column-list)
  (let ((prj-clexp (entropy/prjm-prj-column-expression-prototype))
        key-seq cur-seq)
    (dolist (el prj-clexp)
      (unless (consp el)
        (push el key-seq)))
    (setq key-seq (reverse key-seq))
    (dolist (el column-list)
      (let ((seq-pt 0)
            this-step)
        (catch :exit
          (while (< seq-pt (length key-seq))
            (when (eq el (nth seq-pt key-seq))
              (setq this-step seq-pt)
              (throw :exit nil))
            (cl-incf seq-pt)))
        (unless this-step
          (error "<<entropy/prjm--inct-sorting-colist>>:
none-matched column of prj-obj-prototype '%s'" (symbol-name el)))
        (push seq-pt cur-seq)))
    (setq cur-seq (reverse cur-seq))
    (let (seq-pairs (counter 0))
      (while (< counter (length cur-seq))
        (push (cons (nth counter cur-seq)
                    (nth counter column-list))
              seq-pairs)
        (cl-incf counter))
      (setq seq-pairs
            (entropy/cl-sort-numberic-alist
             seq-pairs
             t t)))))

(defun entropy/prjm--inct-choose-attrs ()
  (let ((attr-keys (entropy/prjm--inct-get-prj-attrs))
        selected-keys (prompt-abbrev "Choosing attrs")
        repeating-func rtn)
    (setq repeating-func
          (lambda (x)
            (unless (eq this-command 'ivy-immediate-done)
              (let ((key-str (car x)))
                (entropy/cl-ivy-read-repeatedly-function
                 key-str 'selected-keys prompt-abbrev)))))
    (ivy-read prompt-abbrev attr-keys
              :require-match t
              :action repeating-func)
    (dolist (el selected-keys)
      (push (cdr (assoc el attr-keys)) rtn))
    (entropy/prjm--inct-sorting-colist rtn)))


(defun entropy/prjm--inct-analyzing-uri (uri-string db-location &optional force-relative)
  "Analyzing URI string and return the entropy-project-managemet
needed format.

#+BEGIN_QUOTE
*For =entropy-project-management= URI specification:*

Most of uri string are acceptted by it, but for some localization
uri, i.e. wave sub-directory and the dot relative path are stored
without the header string of URI standard.
#+END_QUOTE

As for,localization uri _will be_ transfer into the wave or dot
relative format automatically while the type matched for it, it's
the function defaultly dealing method.

The wave type localization uri tranferring result based on the
=HOME= path(use ~gentenv HOME~ to see your home-path), and the dot
relative one's result based on the database-location as the
baseline.
"
  (let* ((uri (url-generic-parse-url uri-string))
         (uri-host (url-host uri))
         (uri-scheme (url-type uri))
         (uri-filename (url-filename uri)))
    (unless (file-exists-p db-location)
      (error "db-location not exist '%s'" db-location))
    (cond
     ((equal uri-scheme "file")
      (cond
       ((equal uri-host "localhost")
        (let ((file-path (if (eq system-type 'windows-nt)
                             (replace-regexp-in-string
                              "^/\\(.\\)/" "\\1:/"
                              uri-filename)
                           uri-filename)))
          (cond (force-relative
                 (let ((file-base (file-name-nondirectory (replace-regexp-in-string "/$" "" file-path)))
                       (file-dir (file-name-directory (replace-regexp-in-string "/$" "" file-path)))
                       (db-dir (file-name-directory db-location))
                       relative-abbrev rtn)
                   (catch :exit
                     (when (equal force-relative "home")
                       (let* ((home (replace-regexp-in-string
                                     "/$" "" (expand-file-name "~/")))
                              (home-relP (string-match home file-path)))
                         (when home-relP
                           (setq rtn
                                 (replace-regexp-in-string
                                  home
                                  "~" file-path))
                           (throw :exit rtn))))
                     (setq relative-abbrev
                           (entropy/cl-make-relative-path db-dir file-dir))
                     (cond (relative-abbrev
                            (if (string-match-p "/$" relative-abbrev)
                                (setq rtn (concat relative-abbrev  file-base))
                              (setq rtn (concat relative-abbrev "/" file-base))))
                           ((null relative-abbrev)
                            (setq rtn uri-string)))
                     rtn)))
                (t
                 uri-string))))
       (t
        uri-string)))
     (t
      uri-string))))

(defun entropy/prjm--inct-gen-prj-shaft ()
  (let ((tfmstr (format-time-string "%Y%m%d%H%M%S")))
    tfmstr))

(defun entropy/prjm--inct-build-local-uri (local-path)
  ;; Note: local-path must be absolute path
  (let ((head "file://localhost/") w32-transfer xnix-transfer rtn)
    (cl-case system-type
      (windows-nt
       (setq w32-transfer
             (replace "^\\(.\\):" "\\1" local-path))
       (setq rtn (concat head w32-transfer)))
      (t
       (setq xnix-transfer
             (replace-regexp-in-string
              "^/" "" local-path))
       (setq rtn (concat head xnix-transfer))))))


;;;;; column read library
(defun entropy/prjm--inct-read-column (prompt column shaft-value db-location &optional initial)
  "Read project property value.

Using `read-string' or `completing-read' to read every
column(property) of project object prototype from
`entropy/prjm-prj-obj-prototype', and return the string or plist
categorized as:

- *common read*:

  Return the string read by interaction.

- *Composited read*:

  For the special case, return the specific elisp type value
  (also can be simple string, depending on the condition case of
  the =Composited read=).

All the columns are automatically extracted from current project
object prototype session, but with some
specification(entropy-prjm-interaction preserved column type
instance) from:

1) `entropy/prjm--inct-prj-date-column'
2) `entropy/prjm--inct-prj-des-column'
3) `entropy/prjm--inct-prj-name-column'
4) `entropy/prjm--inct-prj-template'
5) `entropy/prjm--inct-prj-type-column'
6) `entropy/prjm--inct-prj-uri-column'
7) `entropy/prjm--inct-prj-vcs-column'
8) `entropy/prjm--inct-temp-prjs-candi-alist'


*Composited read details:*

Til now, there's only one composited read column i.e. the uri
column, it has the four condition cases:

| *Type case*        | *condition details*                                                   |
|--------------------+-----------------------------------------------------------------------|
| \"manually-local\" | Project in local place and input the path in manually way             |
|--------------------+-----------------------------------------------------------------------|
| \"auto-local\"     | Project in local place and automatically creat it's name unter the    |
|                    | manually inputted root place.                                         |
|--------------------+-----------------------------------------------------------------------|
| \"exists-local\"   | Project in local place and choosing it interactively                  |
|--------------------+-----------------------------------------------------------------------|
| \"remoe\"          | Project in remote site, input it manually without matching validation |

For the general '*-local' type, the read-return is the plist whose
keys are :uri and :path, function transfer the read localization
path string into URI(uniform resource identifier) to key :uri for
database stored internally powered by
`entropy/prjm--inct-analyzing-uri'. and the read local path
assigned to  key :path which used for other function to do
externals manupulation e.g. creats new project root template as
did by `entropy/prjm--inct-addprj-create-template'."
  (let ((shaft-column (car (plist-get (cdr (assoc 'Shaft (entropy/prjm-prj-obj-prototype)))
                                      :columns))))
    (cond ((or (eq column shaft-column)
               (eq column entropy/prjm--inct-prj-date-column))
           (entropy/prjm--inct-read-string
            prompt nil (or initial (entropy/prjm--inct-gen-prj-shaft))))
          ((eq column entropy/prjm--inct-prj-type-column)
           (substring-no-properties
            (entropy/prjm--inct-read-string
             prompt
             nil initial nil
             (funcall (lambda ()
                        (let (candi)
                          (dolist (el (all-the-icons--read-candidates))
                            (push (car (if (display-graphic-p)
                                           (split-string  (car el) "\t")
                                         (substring-no-properties
                                          (split-string  (car el) "\t"))))
                                  candi))
                          candi))))))
          ((eq column entropy/prjm--inct-prj-uri-column)
           (let* ((type-list '("manually-local" "auto-local" "exists-local" "remote"))
                  (type-choose (completing-read "Choosing uri type: " type-list
                                                nil t))
                  uri repeated-read-candi)
             (setq repeated-read-candi
                   (lambda (prompt-str warn-str &optional dirp reverse)
                     (let (rtn (mode-line-format
                                (list "%e" (propertize warn-str 'face 'error))))
                       (while (if (not reverse)
                                  (not (ignore-errors
                                         (if dirp
                                             (file-directory-p rtn)
                                           (file-exists-p rtn))))
                                (if dirp
                                    (condition-case error (file-directory-p rtn) (error t))
                                  (condition-case error (file-exists-p rtn) (error t))))
                         (setq rtn (entropy/prjm--inct-read-string
                                    prompt-str
                                    nil nil nil 'read-file-name-internal)))
                       rtn)))
             (if (not (equal type-choose "remote"))
                 (cond ((equal type-choose "auto-local")
                        (let ((prj-root (entropy/prjm--inct-read-string
                                         prompt nil initial nil
                                         'read-file-name-internal))
                              prj-uri prj-path
                              rtn)
                          (unless (file-directory-p prj-root)
                            (setq prj-root (funcall repeated-read-candi
                                                    prompt
                                                    "Please try to selected existed location!"
                                                    t)))
                          (setq prj-path (concat prj-root shaft-value))
                          (setq prj-uri (entropy/prjm--inct-build-local-uri prj-path)
                                prj-uri (entropy/prjm--inct-analyzing-uri prj-uri db-location "home"))
                          (list :uri prj-uri :path prj-path)))
                       ((equal type-choose "manually-local")
                        (let ((prj-archive (entropy/prjm--inct-read-string
                                            prompt nil initial nil 'read-file-name-internal))
                              prj-uri prj-path)
                          (when (file-exists-p prj-archive)
                            (setq prj-archive (funcall repeated-read-candi prompt
                                                       "Please try insert file name in root of prj!" nil t)))
                          (setq prj-path prj-archive)
                          (setq prj-uri (entropy/prjm--inct-build-local-uri prj-path)
                                prj-uri (entropy/prjm--inct-analyzing-uri prj-uri db-location "home"))
                          (list :uri prj-uri :path prj-path)))
                       ((equal type-choose "exists-local")
                        (let ((prj-archive (entropy/prjm--inct-read-string
                                            prompt nil initial nil 'read-file-name-internal))
                              prj-uri prj-path)
                          (unless (file-exists-p prj-archive)
                            (setq prj-archive (funcall repeated-read-candi prompt
                                                       "Please selected the existed locaion!")))
                          (setq prj-path prj-archive
                                prj-uri (entropy/prjm--inct-build-local-uri prj-path)
                                prj-uri (entropy/prjm--inct-analyzing-uri
                                         prj-uri db-location "home"))
                          (list :uri prj-uri :path prj-path))))
               (entropy/prjm--inct-read-string prompt nil initial))))
          (t
           (entropy/prjm--inct-read-string prompt nil initial)))))


;;;; db chosen interation
(defun entropy/prjm-inct-chosen-db ()
  (interactive)
  (ivy-read "Choose entropy prjm db: "
            (entropy/prjm--inct-csdb-get-db-names-candi)
            :require-match t
            :action (lambda (x)
                      (setq entropy/prjm--inct-selected-db-name x)
                      (let* ((candi-db-exp-get-func (entropy/prjm--inct-get-db-chosen-operator "get-by-name"))
                             (db-exp (funcall candi-db-exp-get-func x)))
                        (funcall 'entropy/prjm--inct-list-prjs db-exp)))
            :caller 'entropy/prjm-inct-chosen-db))

(ivy-set-actions
 'entropy/prjm-inct-chosen-db
 '(("a" entropy/prjm-inct-add-prj "Adding prj")))


(defun entropy/prjm--inct-csdb-get-db-names-candi ()
  (let* ((all-get-operator (entropy/prjm--inct-get-db-chosen-operator "get-all"))
         (db-exps (funcall all-get-operator))
         names-candi)
    (dolist (el db-exps)
      (let ((db-obj (entropy/prjm-gen-db-obj el)))
        (push (plist-get db-obj :db-name) names-candi)))
    names-candi))

(defun entropy/prjm--inct-csdb-db-names-candi-formatter ()
  (let ((names-candi (entropy/prjm--inct-csdb-get-db-names-candi))
        max-len formatter-string)
    (setq max-len
          (apply 'max (mapcar 'length names-candi)))
    (setq formatter-string
          (format "%%-%ds   %%s" max-len))
    formatter-string))

(defun entropy/prjm--inct-csdb-ivy-format-func (db-names)
  (let* ((candi-item-format (entropy/prjm--inct-csdb-db-names-candi-formatter))
         (candi-db-exp-get-func (entropy/prjm--inct-get-db-chosen-operator "get-by-name"))
         (candi-des-func
          (lambda (db-name)
            (let ((db-obj (entropy/prjm-gen-db-obj
                           (funcall candi-db-exp-get-func db-name))))
              (plist-get db-obj :db-des)))))
    
    (ivy--format-function-generic
     (lambda (db-name)
       (concat (car entropy/prjm--inct-ui-pointer-style)
               (format candi-item-format
                       (ivy--add-face db-name 'ivy-current-match)
                       (propertize
                        (funcall candi-des-func db-name)
                        'face 'ivy-cursor))))
     (lambda (db-name)
       (concat (cdr entropy/prjm--inct-ui-pointer-style)
               (format candi-item-format
                       db-name
                       (propertize (funcall candi-des-func db-name)
                                   'face
                                   'ivy-completions-annotations))))
     db-names "\n")))

(add-to-list 'ivy-format-functions-alist
             '(entropy/prjm-inct-chosen-db . entropy/prjm--inct-csdb-ivy-format-func))

;;;; db list all prjs
(defun entropy/prjm--inct-list-prjs (db-expression)
  (let* ((prj-operator (entropy/prjm--inct-get-db-prj-operator
                        "QUERY-ALL"))
         (db-cache-obj (funcall prj-operator db-expression))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-location (plist-get db-obj :db-location))
         (ivy-case-fold-search-default t))
    (setq entropy/prjm--inct-temp-dbco db-cache-obj
          entropy/prjm--inct-temp-prjs-candi-alist
          (entropy/prjm--inct-names-prj-column-exps db-cache-obj))
    (ivy-read "Choose prj: " entropy/prjm--inct-temp-prjs-candi-alist
              :require-match t
              :action (lambda (x) (entropy/prjm--inct-lprjs-open-prj
                                   (cdr x) db-location))
              :caller 'entropy/prjm--inct-list-prjs)))

(ivy-set-actions 'entropy/prjm--inct-list-prjs
                 '(("d" entropy/prjm--inct-delete-prj "Delete select prj")
                   ("u" entropy/prjm--inct-update-prj "Update select prj")))

(defun entropy/prjm--inct-lprjs-extract-dbco-prj-column-exps (db-cache-obj)
  (let* ((db-hash (cadr db-cache-obj))
         prj-column-exps)
    (maphash (lambda (key value)
               (push value prj-column-exps))
             db-hash)
    (reverse prj-column-exps)))

(defun entropy/prjm--inct-lprjs-list-brief-prjs-info (db-cache-obj &optional prj-names)
  (let ((prj-cl-exps (entropy/prjm--inct-lprjs-extract-dbco-prj-column-exps
                      db-cache-obj))
        (shaft-column (caar (cddddr (assoc 'Shaft (entropy/prjm-prj-obj-prototype)))))
        rtn)
    (if (not prj-names)
        (dolist (el prj-cl-exps)
          (push (list (car (plist-get el shaft-column))
                      (car (plist-get el entropy/prjm--inct-prj-name-column))
                      (car (plist-get el entropy/prjm--inct-prj-des-column)))
                rtn))
      (dolist (el prj-cl-exps)
        (push (car (plist-get el entropy/prjm--inct-prj-name-column)) rtn)))
    (reverse rtn)))

(defun entropy/prjm--inct-lprjs-get-prjs-NmId-align-format (db-cache-obj)
  (let ((brief-prjs-info (entropy/prjm--inct-lprjs-list-brief-prjs-info db-cache-obj))
        ids names id-max-len name-max-len) 
    (dolist (el brief-prjs-info)
      (push (car el) ids)
      (push (cadr el) names))
    (setq id-max-len
          (apply 'max
                 (mapcar 'length
                         ids))
          name-max-len
          (apply 'max
                 (mapcar 'length
                         names)))
    (cons name-max-len id-max-len)))

(defun entropy/prjm--inct-lprjs-candi-formatter (db-cache-obj)
  (let* ((idnm-align (entropy/prjm--inct-lprjs-get-prjs-NmId-align-format
                      db-cache-obj))
         (id-len (cdr idnm-align))
         (nm-len (car idnm-align)))
    (format "%%-%ds   %%-%ds" nm-len id-len)))

(defun entropy/prjm--inct-names-prj-column-exps (db-cache-obj)
  (let ((prj-cl-exps (entropy/prjm--inct-lprjs-extract-dbco-prj-column-exps db-cache-obj))
        rtn (counter 0) (fmstr-nmid (entropy/prjm--inct-lprjs-candi-formatter db-cache-obj))
        (shaft (caar (cddddr (assoc 'Shaft (entropy/prjm-prj-obj-prototype))))))
    (dolist (el prj-cl-exps)
      (let ((prj-name (or (car (plist-get el entropy/prjm--inct-prj-name-column))
                          "NULL-Prj-Name"))
            (shaft-name (car (plist-get el shaft))))
        (push (cons  (format fmstr-nmid prj-name shaft-name) el) rtn)))
    rtn))

(defun entropy/prjm--inct-lprjs-ivy-item-format-func (prj-candi-name &optional non-match)
  (let* ((prj-cl-exp  (cdr (assoc prj-candi-name entropy/prjm--inct-temp-prjs-candi-alist)))
         (prj-type (car (plist-get prj-cl-exp entropy/prjm--inct-prj-type-column)))
         (prj-des (car (plist-get prj-cl-exp entropy/prjm--inct-prj-des-column)))
         (prj-icon  (entropy/prjm--inct-match-prj-icon prj-type)))
    (concat (if non-match
                (cdr entropy/prjm--inct-ui-pointer-style)
              (car entropy/prjm--inct-ui-pointer-style))
            " " prj-icon " "
            (if non-match
                prj-candi-name
              (ivy--add-face prj-candi-name 'ivy-current-match))
            "\t  " (propertize (or prj-des "")
                             'face
                             (if (not non-match)
                                 'ivy-virtual
                               'ivy-completions-annotations)))))


(defun entropy/prjm--inct-lprjs-ivy-format-func (prjs-candi-alist)
  (ivy--format-function-generic
   (lambda (x) (funcall 'entropy/prjm--inct-lprjs-ivy-item-format-func x))
   (lambda (x) (funcall 'entropy/prjm--inct-lprjs-ivy-item-format-func x t))
   prjs-candi-alist "\n"))

(add-to-list 'ivy-format-functions-alist
             '(entropy/prjm--inct-list-prjs . entropy/prjm--inct-lprjs-ivy-format-func))

(defun entropy/prjm--inct-lprjs-open-prj (prj-column-exp db-location)
  (let ((open-func (entropy/prjm--inct-get-db-prj-operator "OPEN"))
        (prj-uri (car (plist-get prj-column-exp entropy/prjm--inct-prj-uri-column))))
    (funcall open-func prj-uri db-location)))



;;;; add db prj interaction
(defun entropy/prjm-inct-add-prj (db-name)
  (let ((db-exp (funcall (entropy/prjm--inct-get-db-chosen-operator "get-by-name") db-name))
        (ivy-format-function 'ivy-format-function-default))
    (funcall 'entropy/prjm--inct-add-prj-core db-exp)))

(defun entropy/prjm--inct-add-prj-core (db-expression)
  (let* (prj-exp
         (shaft-column (car (plist-get (cdr (assoc 'Shaft (entropy/prjm-prj-obj-prototype)))
                                      :columns)))
         (db-obj (entropy/prjm-gen-db-obj db-expression))
         (db-name (plist-get db-obj :db-name))
         (db-lc (plist-get db-obj :db-location))
         (keys (entropy/prjm--inct-get-prj-attrs))
         (add-operator (entropy/prjm--inct-get-db-prj-operator '"ADD"))
         (qall-operator (entropy/prjm--inct-get-db-prj-operator "QUERY-ALL"))
         uri-added path-added shaft-value-added)
    (dolist (el keys)
      (let ((prompt (concat "Input " (car el)))
            column-read)
        (setq column-read (entropy/prjm--inct-read-column prompt (cdr el) shaft-value-added db-lc))
        (cond ((eq (cdr el) entropy/prjm--inct-prj-uri-column)
               (setq uri-added (if (entropy/cl-plistp column-read)
                                   (plist-get column-read :uri)
                                 column-read)
                     path-added (if (entropy/cl-plistp column-read)
                                   (plist-get column-read :path)
                                 nil)
                     entropy/prjm--inct-uri-added-log column-read)
               (push uri-added prj-exp))
              ((eq (cdr el) shaft-column)
               (setq shaft-value-added column-read)
               (push column-read prj-exp))
              (t
               (push column-read prj-exp)))))
    (if (and (stringp (car (reverse prj-exp)))
             (not (equal "" (car (reverse prj-exp)))))
        (setq prj-exp (append '("ADD") (reverse prj-exp)))
      (error "You must insert the prj shaft!"))
    (let* ((db-cache-obj (ignore-errors (funcall qall-operator db-expression)))
           (db-hash (cadr db-cache-obj))
           (shaft-value (progn
                         (entropy/prjm-prj-expression-validp prj-exp)
                         (cadr prj-exp))))
      (cond ((null db-cache-obj)
             (message "'%s' was fresh new!" db-name))
            (t
             (when (gethash shaft-value db-hash)
               (error "<<enropy/prjm--inct-add--prj>>:
prj shaft '%s' has been existed!" shaft-value))))
      (when (and path-added
                 (not (file-exists-p path-added)))
        (entropy/prjm--inct-addprj-create-template path-added))
      (funcall add-operator prj-exp db-expression))))


(defun entropy/prjm--inct-addprj-create-template (target-abs)
  (let ((target-parent (file-name-directory (replace-regexp-in-string
                                             "/$" ""
                                             target-abs))))
    (cond ((not (file-exists-p target-parent))
           (error "Wrong target path given!"))
          ((file-exists-p target-abs)
           (error "Target file existed!"))
          (t
           (when (yes-or-no-p
                  (format "Create local prj '%s'" target-abs))
             (cond ((file-directory-p entropy/prjm--inct-prj-template)
                    (copy-directory entropy/prjm--inct-prj-template
                                    target-abs))
                   ((file-exists-p entropy/prjm--inct-prj-template)
                    (copy-file entropy/prjm--inct-prj-template
                               target-abs))))))))


;;;; delete db prj interaction
(defun entropy/prjm--inct-delete-prj (prjs-alist-item)
  (let ((db-expression (funcall (entropy/prjm--inct-get-db-chosen-operator "get-by-name") entropy/prjm--inct-selected-db-name))
        (ivy-format-function 'ivy-format-function-default))
    (funcall 'entropy/prjm--inct-delete-prj-core
             (entropy/prjm-prj-column-exp-to-prj-exp (cdr prjs-alist-item) "DELETE") db-expression)))

(defun entropy/prjm--inct-delete-prj-core (prj-exp db-expression)
  (let* ((delete-func (entropy/prjm--inct-get-db-prj-operator "DELETE"))
         (prj-cl-exp (entropy/prjm-gen-prj-column-expression prj-exp))
         (shaft (car prj-cl-exp))
         (prj-indicator (concat "\"" (car (plist-get prj-cl-exp shaft)) " "
                                (car (plist-get prj-cl-exp entropy/prjm--inct-prj-name-column))
                                "\"")))
    (when (yes-or-no-p (format "Delete project %s ? " prj-indicator))
      (funcall delete-func prj-exp db-expression))))

;;;; update db prj interaction
(defun entropy/prjm--inct-update-prj (prjs-alist-item)
  (let* ((ivy-format-function 'ivy-format-function-default)
         (key-pairs (entropy/prjm--inct-get-prj-attrs))
         (db-exp (funcall (entropy/prjm--inct-get-db-chosen-operator "get-by-name") entropy/prjm--inct-selected-db-name))
         (db-obj (entropy/prjm-gen-db-obj db-exp))
         (prj-cl-exp-rtn (copy-tree (cdr prjs-alist-item)))
         (prj-cl-exp-origin (copy-tree (cdr prjs-alist-item)))
         (shaft (car (entropy/prjm-prj-column-expression-prototype)))
         (shaft-origin (plist-get prj-cl-exp-origin shaft))
         selected-keys
         shaft-new
         keys-did)
    (when (yes-or-no-p "Choosing columns? ")
      (setq selected-keys (entropy/prjm--inct-choose-attrs))
      (let ((new-key-pairs (copy-tree key-pairs)))
        (setq key-pairs nil)
        (dolist (el selected-keys)
          (catch :exit
            (dolist (el2 new-key-pairs)
              (when (eq el (cdr el2))
                (push el2 key-pairs)
                (throw :exit nil)))))
        (setq key-pairs (reverse key-pairs))))
    (dolist (el key-pairs)
      (let* ((prompt (concat "Input " "'"(car el)"'"))
             (initial (or (car (plist-get prj-cl-exp-rtn (cdr el)))
                          (progn
                            (setq prompt
                                  (concat prompt (propertize " <origin value null>"
                                                             'face
                                                             'ivy-match-required-face)))
                            "")))
             (value-type (cdr (plist-get prj-cl-exp-rtn (cdr el))))
             column-read)
        (setq column-read
              (entropy/prjm--inct-read-column
               prompt (cdr el) shaft-origin (plist-get db-obj :db-location)
               initial))
        (cond ((eq (cdr el) entropy/prjm--inct-prj-uri-column)
               (push (cons (cdr el)
                           (cons (or (plist-get column-read :uri)
                                     column-read)
                                 value-type))
                     keys-did))
              (t
               (push (cons (cdr el)
                           (cons column-read
                                 value-type))
                     keys-did)))))
    (progn
      (when keys-did
        (dolist (el keys-did)
          (plist-put prj-cl-exp-rtn (car el) (cdr el))))

      (setq shaft-new (plist-get prj-cl-exp-rtn shaft))
      (cond
       ((equal shaft-origin shaft-new)
        (when (entropy/prjm-prj-column-expression-validp prj-cl-exp-rtn)
          (let ((prj-exp (entropy/prjm-prj-column-exp-to-prj-exp prj-cl-exp-rtn "UPDATE"))
                (update-func (entropy/prjm--inct-get-db-prj-operator "UPDATE")))
            (funcall update-func prj-exp db-exp))))
       ((not (equal shaft-origin shaft-new))
        (let ((delete-func (entropy/prjm--inct-get-db-prj-operator "DELETE"))
              (add-func (entropy/prjm--inct-get-db-prj-operator "ADD"))
              (prj-exp-rtn (entropy/prjm-prj-column-exp-to-prj-exp prj-cl-exp-rtn "ADD"))
              (prj-exp-origin (entropy/prjm-prj-column-exp-to-prj-exp prj-cl-exp-origin "DELETE")))
          (funcall delete-func prj-exp-origin db-exp)
          (funcall add-func prj-exp-rtn db-exp)))))))




;;;; provide
(provide 'entropy-prjm-interaction)
