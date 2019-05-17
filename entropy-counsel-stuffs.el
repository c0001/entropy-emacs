;;; File name: entropy-counsel-stuffs.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;
;;; Commentary:
;;
;; This packages will provide your the ability for quickly searching stuffs your
;; gatherd in your own location.
;; 
;; What is stuffs?
;; 
;; stuffs was the abstract for sth that be used for assembling into completing
;; for the unique thing which be can be used without any or just a low
;; using-threshold.
;; 
;; In this way, like books and source website can be stuffs , because you can
;; not using them for dealing with common actual task i.e. for daily using for
;; some goal. But stuff are more important for our life, the reason for it was
;; that any independant thing we used was the combination of those stuffs.
;; 
;; So this package basic on package `entropy-open-with', and the function
;; `browse-url'. The former was used to open local files like pdfs, musics,
;; videos within local gui software and the latter was used to open url stuffs
;; which foramt in local syle like `127.0.0.1/index' for searching the local
;; website you created or tracked from website remained in you local web server
;; doc container.
;; 
;; The mainly core elements for you using this package was to create one
;; database file which maintained into one =org= file, and can be both with the
;; manuly editting raw file or interactively adding records with `ivy' prompt.
;; 
;; The actual concept of this was to to using bookmarked mechanism for recording
;; or review the things your remained.
;;
;;
;;

;; * Code:
;; ** require
(require 'entropy-open-with)
(require 'entropy-org-widget)
(require 'entropy-common-library)
(require 'entropy-common-library-const)
(require 'ivy)
;; ** variable declare
(defvar entropy/cs-entry-recorde-file "~/entropy-cs.org"
  "Default stuffs recorded file used for `entropy/cs-filter-open'
  and `entropy/cs-recorde-entry'.

  File format as follow structure:

  #+BEGIN_SRC org
    ,* tile
    ,** heading 1
       :PROPERTIES:
       :CUSTOM_ID: 1234567890
       :CATEGORY: books
       :CSTYPE:   url
       :LOCATION: C:/temp/
       :END:
  #+END_SRC

  Note: Every stuffs entry don't need to manully editted this org file.")

(defvar entropy/cs-user-category '("common" "url-bookmarks" "books" "musics" "videos")
  "Default `entropy-counsel-stuffs' user customized category
  list.")



(defvar entropy/cs-cached-alist '()
  "Cached alist from `entropy/cs-entry-recorde-file'")


(defvar entropy/cs-read-temp '()
  "Temp variable for contained read candidates.")




;; ** main library
(defun entropy/cs-get-default-category (&optional cached)
  "Get all customized org categories-name of
`entropy-counsel-stuffs'.

Now just using `entropy/cs-user-category', and will plan to using
categories in all `org-agenda-files'."
  (if (not cached)
      entropy/cs-user-category

    (let* ((alist (if entropy/cs-cached-alist entropy/cs-cached-alist
                    (progn (entropy/cs-update-cached-list)
                           entropy/cs-cached-alist)))
           rlist
           rslist)
      (dolist (el alist)
        (if (entropy/cs-justify-key-p "CATEGORY" el)
            (push (entropy/cs-get-entry-key-pair-query "CATEGORY" el) rlist)))

      (dolist (el rlist)
        (let ((sp (split-string (nth 1 el) ";")))
          (dolist (el1 sp)
            (if (not (entropy/cl-unique-list el1 rslist))
                (push el1 rslist)))))
      (if rslist
          rslist
        (error "There's no category from cached!")))))



(defun entropy/cs-get-original-alist ()
  "Query ITEM, CUSTOM_ID, CSTYPE, CATEGORY, LOCATION of the all
headline of file `entropy/cs-entry-recorde-file' and return the
properties gatherd alist of all of the headline of it."
  (interactive)
  (with-temp-buffer (insert-file-contents entropy/cs-entry-recorde-file)
                    (org-mode)
                    (outline-show-all)
                    (entropy/ow-get-all-head-alist '("ITEM" "CUSTOM_ID" "CSTYPE" "CATEGORY" "LOCATION"))))


(defun entropy/cs-get-entry-key-pair-query (query entry)
  "Extract one element of one entry ENTRY of the list returned by
`entropy/cs-get-original-alist' according the the specifc key
name -- query QUERY."
  (let (rlist)
    (dolist (el entry)
      (if (string= (car el) query)
          (setq rlist el)))
    rlist))

(defun entropy/cs-return-filter-category-alist (query alist)
  "returned the `entropy-counsel-stuffs' list which the feature
of each entry of it were all have the specific category value of
category key in org drawer block using loop of
`entropy/cs-get-entry-key-pair-query'."
  (let (rlist)
    (dolist (el alist)
      (if (string-match-p (regexp-quote query) (nth 1 (entropy/cs-get-entry-key-pair-query "CATEGORY" el)))
          (add-to-list 'rlist el t)))
    rlist))

(defun entropy/cs-get-alist-name (alist)
  "Return one list of all value of ITEM key of one list returned
by `entropy/cs-get-original-alist'."
  (let (return-list)
    (dolist (el alist)
      (let ((item (nth 1 (entropy/cs-get-entry-key-pair-query "ITEM" el)))
            (cs-entry-p (entropy/cs-justify-key-p "CSTYPE" el)))
        (when cs-entry-p
          (add-to-list 'return-list item t))))
    return-list))

(defun entropy/cs-get-alist-id-name (alist)
  "Return one list of all value of both ITEM and CUSTOM_ID key of
one list returned by `entropy/cs-get-original-alist'"
  (let* (rlist)
    (dolist (el alist)
      (let* ((id-pair (if (entropy/cs-justify-key-p "CUSTOM_ID" el)
                          (nth 1 (entropy/cs-get-entry-key-pair-query "CUSTOM_ID" el))
                        nil))
             (item-pair (if (entropy/cs-justify-key-p "ITEM" el)
                            (nth 1 (entropy/cs-get-entry-key-pair-query "ITEM" el))
                          nil)))
        (if (and id-pair item-pair)
            (push `(,id-pair ,item-pair) rlist))))
    (setq rlist (entropy/cl-reverse-list rlist))))

(defun entropy/cs-justify-key-p (key ecs-entry)
  "Justify one key KEY is activated in one
`entroy-counsel-stuffs' entry.

If key exists and key's value is valid, then return that
value. Ohter wise return nil."
  (let* (justify
         (value nil))
    (dolist (el ecs-entry)
      (if (string= (car el) key)
          (setq value (nth 1 el))))
    (if (and value
             (not (string= "???" value)))
        value
      nil)))

(defun entropy/cs-get-id-entry (id alist)
  "Return the entry whose CUSTOM_ID key value was equal specific
id ID."
  (let* ((rlist nil))
    (dolist (el alist)
      (if (and (entropy/cs-justify-key-p "CUSTOM_ID" el)
               (string= id (nth 1 (entropy/cs-get-entry-key-pair-query "CUSTOM_ID" el))))
          (setq rlist el)))
    rlist))


(defun entropy/cs-update-cached-list ()
  "Update `entropy-counsel-stuffs' cached alist. And modify
`entropy/cs-cached-alist'."
  (interactive)
  (setq entropy/cs-cached-alist (entropy/cs-get-original-alist)))

(defun entropy/cs-get-cached-list ()
  "Get cached list from `entropy/cs-entry-recorde-file' without
modifying `entropy/cs-cached-alist'."
  (entropy/cs-get-original-alist))

(defun entropy/cs-read-prompt (type)
  "For repeat produce newest ivy prompt string."
  (format "%s (%s): " type
          (mapconcat #'identity entropy/cs-read-temp ";")))



;; ** Stuffs entries queried open function
;;;###autoload
(defun entropy/cs-filter-open (&optional category prompt inemacs)
  "Open one `entroy-counsel-stuffs' entry with ivy prompt.

Args description:

- CATEGORY: You can specific one category CATEGORY for limitting the scope of quering.
- PROMPT:   You can customized prompt string by setting PROMPT.
- inemacs:  Using emacs open stuff entry. This is useful API for lisp programmer."
  (interactive)
  (let* ((olist (if entropy/cs-cached-alist entropy/cs-cached-alist
                  (progn (entropy/cs-update-cached-list)
                         entropy/cs-cached-alist)))
         (q-cate (if (not category)
                     (ivy-read "Choose category：" (entropy/cs-get-default-category t)
                               :require-match t)
                   category))
         (alist (entropy/cs-return-filter-category-alist q-cate olist))
         (nlist (entropy/cs-get-alist-name alist))
         (chname (ivy-read
                  (if prompt prompt "Choose entry: ")
                  nlist
                  :require-match t))
         chentry
         chlocation)
    (dolist (el alist)
      (if (string= chname (nth 1 (entropy/cs-get-entry-key-pair-query "ITEM" el)))
          (setq chentry el)))
    (if (entropy/cs-justify-key-p "LOCATION" chentry)
        (setq chlocation (entropy/cs-justify-key-p "LOCATION" chentry))
      (error "Entry %s is not exist location key." chname))
    (if (string-match-p "^file:\\(//\\)?" chlocation)
        (setq chlocation (replace-regexp-in-string "^file:\\(//\\)?" "" chlocation)))
    (if inemacs
        (entropy/open-with-port nil chlocation t)
      (entropy/open-with-port nil chlocation))))

(defun entropy/cs-open-all ()
  "Query before open one stuff for all entry from
`entropy/cs-cached-alist'."
  (interactive)
  (let* ((olist (if entropy/cs-cached-alist entropy/cs-cached-alist
                  (progn (entropy/cs-update-cached-list)
                         entropy/cs-cached-alist)))
         (nlist (entropy/cs-get-alist-name olist))
         (choice (ivy-read "Open stuff: " nlist
                           :require-match t))
         cs-location)
    (dolist (el olist)
      (if (string= choice (nth 1 (entropy/cs-get-entry-key-pair-query "ITEM" el)))
          (if (entropy/cs-justify-key-p "LOCATION" el)
              (setq cs-location (nth 1 (entropy/cs-get-entry-key-pair-query "LOCATION" el))))))
    (if cs-location
        (entropy/open-with-port nil cs-location)
      (error (format "Invalid location query for '%s'? " choice)))))

;; ** Stuffs entries recorde function
(defun entropy/cs-recorde-insert-template (name customid type category location)
  "Insert the `entropy-counsel-stuffs' entry into
`entropy/cs-entry-recorde-file'."
  (with-current-buffer
      (find-file-noselect entropy/cs-entry-recorde-file)
    (if buffer-read-only
        (read-only-mode 0))
    (goto-char (point-max))
    (newline)
    (forward-line 0)
    (insert (format "** %s" name))
    (save-excursion 
      (insert
       (format
        "
:PROPERTIES:
:CUSTOM_ID: %s
:CSTYPE: %s
:CATEGORY: %s
:LOCATION: %s
:END:" customid type category location))
      )
    (next-line)
    (org-indent-drawer)
    (save-buffer)
    (read-only-mode 1)))

;;;###autoload
(defun entropy/cs-read-category (x)
  "Repeatedly read category candidates for
`entropy/cs-recorde-entry'."
  (if (not (member x entropy/cs-read-temp))
      (push x entropy/cs-read-temp))
  (let ((prompt (entropy/cs-read-prompt "Category-> ")))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "%-4d " prompt)))
  (cond
   ((memq this-command '(ivy-done
                         ivy-alt-done
                         ivy-immediate-done))
    t)
   ((eq this-command 'ivy-call)
    (with-selected-window (active-minibuffer-window)
      (delete-minibuffer-contents)))))

(defun entropy/cs-recorde-entry ()
  "Recorde `entropy-counsel-stuffs' entry with ivy prompt."
  (interactive)
  (let* ((name (read-string "Adding entry name: "))
         customid
         (category (progn (setq entropy/cs-read-temp nil)
                     (ivy-read "Adding category: " (entropy/cs-get-default-category t)
                               :action 'entropy/cs-read-category)
                     (let ((rtn nil))
                       (dolist (el (entropy/cl-reverse-list entropy/cs-read-temp))
                         (let* ((el-1 (replace-regexp-in-string "\\(^;+\\|;+$\\)" "" el))
                                (element (replace-regexp-in-string ";+" ";" el-1)))
                           (if rtn
                               (setq rtn (concat rtn ";" element))
                             (setq rtn element))))
                       rtn)))
         (type (ivy-read "Choose entry type: " '("url" "local")
                         :require-match t))
         (location (if (string= type "url")
                       (url-hexify-string (read-string "Adding url: ") entropy/cl-url--allowed-chars)
                     (ivy-read "Please input FILENAME: " 'read-file-name-internal
                               :history 'file-name-history))))
    (setq customid (org-id-new "entropy/cs"))
    (entropy/cs-recorde-insert-template name customid type category
                                        (if (string= type "local")
                                            (concat "file://" location)
                                          location)))
  (entropy/cs-update-cached-list))
;; ** Stuffs entries converter
(defun entropy/cs-converter (type)
  "Converting org file to the constructer of `entropy/cs-entry-recorde-file'
  format."
  (interactive
   (list (ivy-read "Choose convert type: " '("link head")
                   :require-match t)))
  (outline-show-all)
  (setq entropy/cs-read-temp nil)
  (let* ((id-prefix (read-string "Input id prefix: "))
         (cstype (ivy-read "Choose CSTYPE: " '("url" "local")))
         (category (progn
                     (ivy-read "Input category: " (entropy/cs-get-default-category t)
                               :action 'entropy/cs-read-category)
                     (let ((rtn nil))
                       (dolist (el (entropy/cl-reverse-list entropy/cs-read-temp))
                         (let* ((el-1 (replace-regexp-in-string "\\(^;+\\|;+$\\)" "" el))
                                (element (replace-regexp-in-string ";+" ";" el-1)))
                           (if rtn
                               (setq rtn (concat rtn ";" element))
                             (setq rtn element))))
                         rtn))))
    (cond
     ((string= "link head" type)
      (org-map-entries (lambda ()
                         (when (string-match "\\[\\[.*?\\]\\[.*?\\]\\]" (nth 4 (org-heading-components)))
                           (entropy/ow-set-drawer
                            `(("CUSTOM_ID" ,(org-id-new id-prefix))
                              ("CSTYPE" ,cstype)
                              ("CATEGORY" ,category)
                              ("LOCATION" ,(let (($loc (entropy/ow-extract-hlink)))
                                             (if $loc
                                                 $loc
                                               "")))))))
                       t 'file nil)
      (org-map-entries (lambda ()
                         (forward-line 0)
                         (when (string-match "\\[\\[.*?\\]\\[.*?\\]\\]" (nth 4 (org-heading-components)))
                           (let* ((level (car (org-heading-components)))
                                  (level-string "")
                                  (item (nth 4 (org-heading-components)))
                                  (name (nth 1 (split-string item "\\(\\[\\|\\]\\)" t))))
                             (let ((count 0))
                               (while (not (equal count level))
                                 (setq level-string (concat level-string "*"))
                                 (setq count (+ count 1))))
                             (kill-line)
                             (insert (concat level-string " " name)))))
                       t 'file nil))
     (t (message "Options: %s function has not been implemented." type)))))

;; ** Stuffs entries modified
;;;###autoload
(defun entropy/cs-modifiy ()
  "Modify specific entry in `entropy/cs-entry-recorde-file'."
  (interactive)
  (let* ((alist (entropy/cs-get-original-alist))
         ;; idlist
         (idlist (entropy/cs-get-alist-id-name alist))
         (iidlist (entropy/cl-make-identify-list idlist))
         (nlist (entropy/cl-extract-idlist iidlist 1))
         (choice (ivy-read "Choose which item you want to modified: " nlist))
         chidet)
    (dolist (el iidlist)
      (if (string= (car (split-string choice ":")) (car el))
          (setq chidet el)))

    (let* ((id-entry (entropy/cs-get-id-entry (car (nth 1 chidet))  alist))
           (ohname (nth 1 (entropy/cs-get-entry-key-pair-query "ITEM" id-entry)))
           (ocstype (nth 1 (entropy/cs-get-entry-key-pair-query "CSTYPE" id-entry)))
           (ocategory (nth 1 (entropy/cs-get-entry-key-pair-query "CATEGORY" id-entry)))
           (olocation (nth 1 (entropy/cs-get-entry-key-pair-query "LOCATION" id-entry)))
           (hname    (if (y-or-n-p "Changing ITEM name? ") (read-string "Input new entry name: " ohname) nil))
           (cstype   (if (y-or-n-p "Changing CSYTPE? ") (read-string "Input new cstype: " ocstype) nil))
           (category (if (y-or-n-p "Changing CATEGORY? ")
                         (progn (setq entropy/cs-read-temp nil)
                           (ivy-read "Input new category: " (entropy/cs-get-default-category t)
                                     :initial-input ocategory
                                     :action 'entropy/cs-read-category)
                           (let ((rtn nil))
                             (dolist (el (entropy/cl-reverse-list entropy/cs-read-temp))
                               (if rtn
                                   (setq rtn (concat rtn ";" el))
                                 (setq rtn el)))
                             rtn))
                       nil))
           (location (if (y-or-n-p "Changing LOCATIION? ")(read-string "Input new location: " olocation) nil)))

      (with-current-buffer (find-file-noselect entropy/cs-entry-recorde-file)
        (if buffer-read-only (read-only-mode 0))
        (if (not (equal major-mode 'org-mode))
            (org-mode))
        (outline-show-all)
        (goto-char (point-min))
        (re-search-forward (regexp-quote (car (nth 1 chidet))))
        (org-previous-visible-heading 1)
        (forward-line 0)
        (save-excursion
          (if cstype   (org-set-property "CSTYPE" cstype))
          (if category (org-set-property "CATEGORY" category))
          (if location (org-set-property "LOCATION" location)))
        (forward-line 0)
        (if hname
            (let* ((h-name (buffer-substring-no-properties (point) (save-excursion (end-of-line) (point))))
                   (nh-name (replace-regexp-in-string
                             "\\(^\\*+? +\\).*?\\(:.*?:\\)?$" (format "\\1%s     \\2" hname)
                             h-name)))
              (kill-line)
              (insert nh-name)))
        (if (or hname cstype category location)
            (save-buffer))
        (read-only-mode 1))))
  (entropy/cs-update-cached-list))

;; ** Stuffs entry deletion
;;;###autoload
(defun entropy/cs-delete ()
  "Delete one entry in `entropy/cs-entry-recorde-file'."
  (interactive)
  (let* ((alist (entropy/cs-get-original-alist))
         (idlist (entropy/cs-get-alist-id-name alist))
         (iidlist (entropy/cl-make-identify-list idlist))
         (nlist (entropy/cl-extract-idlist iidlist 1))
         (choice (ivy-read "Choose which item you want to modified: " nlist))
         chidet)
    (dolist (el iidlist)
      (if (string= (car (split-string choice ":")) (car el))
          (setq chidet el)))
    (let* ()
      (with-current-buffer (find-file-noselect entropy/cs-entry-recorde-file)
        (if buffer-read-only (read-only-mode 0))
        (if (not (equal major-mode 'org-mode))
            (org-mode))
        (outline-show-all)
        (goto-char (point-min))
        (re-search-forward (regexp-quote (car (nth 1 chidet))))
        (org-previous-visible-heading 1)
        (forward-line 0)
        (entropy/ow-delete-headline-block)
        (save-buffer)
        (read-only-mode 1))))
  (entropy/cs-update-cached-list))

;; ** Recorde or Open macro
(defmacro entropy/cs-id-open-macro (fuc-name cid)
  `(defun ,fuc-name ()
     (interactive)
     (let ((entry (entropy/cs-get-id-entry ,cid (entropy/cs-get-original-alist)))
           location
           cstype)
       (if (and entry
                (entropy/cs-justify-key-p "LOCATION" entry)
                (entropy/cs-justify-key-p "CSTYPE" entry))
           (progn
             (setq location (nth 1 (entropy/cs-get-entry-key-pair-query "LOCATION" entry)))
             (setq cstype (nth 1 (entropy/cs-get-entry-key-pair-query "CSYTPE" entry))))
         (error (format "Invalid id search: %s ." ,cid)))
       (if (string= cstype "url")
           (browse-url location)
         (entropy/open-with-port nil location)))))


(defmacro entropy/cs-recorde-macro (fuc-name cstype category)
  `(defun ,fuc-name ()
     (interactive)
     (let ((item (read-string "Create item name: "))
           (location (read-string "Insert location: "))
           (custom_id (org-id-new "entropy/cs")))
       (entropy/cs-recorde-insert-template item custom_id ,cstype ,category location))
     (entropy/cs-update-cached-list)))



;; * provide
(provide 'entropy-counsel-stuffs)
