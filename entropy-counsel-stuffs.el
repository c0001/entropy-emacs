;;; entropy-counsel-stuffs.el --- A org based stuff management
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-counsel-stuffs
;; Package-Version: 0.1.0
;; Version:       file-version
;; Created:       2018-month-date hour:min:sec
;; Compatibility: GNU Emacs 24;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (org "9.1.13"))
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
;;;; What is stuffs this case shown for?
;;
;; Stuffs was the abstract for sth that be used for assembling into
;; completing for the unique thing which be can be used without any or
;; just a low using-threshold.
;;
;; In this way, like books and source website can be stuffs , because you
;; can not using them for dealing with common actual task i.e. for daily
;; using for some goal. But stuff are more important for our life, the
;; reason for it was that any independant thing we used was the
;; combination of those stuffs.
;;
;; So this package basic on package =entropy-open-with=, and the function
;; ~browse-url~. The former was used to open local files like pdfs,
;; musics, videos within local mime relevant applications and the latter
;; was used to open url stuffs which foramt in local syle like
;; `127.0.0.1/index' for searching the local website you created or
;; tracked from website remained in you local web server doc container,
;; OFC, you also can using for web bookmarks .
;;
;; The mainly core elements for you using this package was to create one
;; database file which maintained into one =Org= file, and can be both
;; with the manuly editting raw file or interactively adding records with
;; ~ivy~ prompt.
;;
;; The actual concept of this was to to using bookmarked mechanism for
;; recording or review the things your remained.
;;
;;;; Requirements
;;
;; - entropy-common-library
;; - entropy-org-widget
;; - entropy-open-with
;; - ivy
;;
;; Required package named with prefix =entropy-= was =entropy-emacs=
;; embedded self maintained package, you can retrieve them by accessing
;; those repository.
;;
;; Required package =ivy= was elpa accredited third-party emacs extension
;; built by [[https://github.com/abo-abo][abo-abo]], you can download it from melpa directly.
;;
;;;; Installation 
;;
;; It's recommended using =use-package= to config the initialize config
;; of this file as the code snippet below shown:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (use-package entropy-counsel-stuffs
;;     :load-path "path-to-your-loadpath"
;;     :bind (("C-M-<f12>" . entropy/cs-recorde-entry)
;;            ("C-<f12>" . entropy/cs-open-all))
;;     :commands (entropy/cs-filter-open
;;                entropy/cs-recorde-entry
;;                entropy/cs-converter
;;                entropy/cs-modifiy
;;                entropy/cs-delete))
;; #+END_SRC
;;
;;
;; There's none default keybindings, keybindings shown in the initialized
;; config snippet was my own config as is, you should change it to fit
;; for your habbits if it's messy your key sequences.
;;
;;;; Database file content structer
;;
;; Each stuffs recorded in thd database file was recognized as one org
;; file heading, and all its attribute were recorded as the properties
;; under the stuff heading entry.
;;
;; The one stuff entry looks like the former shown as below:
;;
;; #+BEGIN_SRC org
;;   ,* tile
;;   ,** heading 1
;;     :PROPERTIES:
;;     :CUSTOM_ID: 1234567890
;;     :CATEGORY: books
;;     :CSTYPE:   url
;;     :LOCATION: C:/temp/
;;     :END:
;; #+END_SRC 
;;
;;; Introduction fo above tempalte:*
;;
;; The title was indicated the top level 1st info of current database
;; brief description. 
;;
;; Heading =heading 1= was one stuff entry recorde which has the the
;; attributes "custom_id","category","cstype","location" representing as
;; the heading properties key pair.
;;
;;; For the attributes description individually:*
;;
;; - CUSTOM_ID:
;;
;;   Each stuff must has the unique identifier number sequence for as the
;;   implicit name flag for preventing the duplicated human readable
;;   nature language based name string occurrence, it can be setted
;;   manually when you manipulate the database raw file manually, it's
;;   recommended to setting this id sequence using emacs org-mode
;;   internal api ~org-id-new~ or using the bash script snippet:
;;   
;;   : cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1
;;
;; - CATEGORY
;;
;;   Each stuff entry could given the category as the group classifying
;;   orgnization based as for and each of them can have multi categories
;;   filtered of.
;;
;;   The categories records string is seperated with semicolons for the
;;   multi-categories type, thus category string 'a;b;c' denoted that the
;;   stuff archived into category 'a,b,c' both of that. The designation
;;   concept for this was similar to blog site's post tag system, but
;;   warnning for that =entropy-counsel-stuff= have another type setting
;;   be for the tag post which using heading tag entry element of
;;   org-mode internal supporting with.
;;
;;   Done with category inputting could completing by both of manually or
;;   query prompting way, the interaction dealing for adding or modifying
;;   stuff entry are the automatically trench.
;;
;; - CSTYPE
;;
;;   Stuff type attribute used for indicated local or on-line stuff
;;   location generally. This attribute used for stuff open backend to
;;   filter for the correct stuff open treatment, thus local file opened
;;   by searching for the local path, url stuff open with emacs internal
;;   or external web browser etc.
;;
;;   Til now, there's just support two value =url= and =local= denoted as
;;   the it's literally meaning as is.
;;
;; - LOCATION
;;
;;   Stuff location specific string format with individual protocol
;;   string format e.g. 'file://xxxxxxxx' used for local file
;;   'https://xxxxxxxx' used for web urls. And now
;;   =entropy-counsel-stuffs= only support this two protocol string.
;;   
;;;; Interactivation
;;
;; =entropy-counsel-stuffs= using sets of interaction 'autoload' to
;; expose the user calling operation.
;;
;; 'Query open', 'Adding', 'Modification' operation were four main
;; operation in this case. All of them are designed with /query-prompt/
;; interaction based with ~completing-read~ or =ivy= emacs mordern
;; completion framework, thus you can work with it on the benefit way.
;;
;;;;; Query open
;;
;; Stuffs are usually have the dozen volumn counts stored in your local
;; database, this package designed with effective query filter for what
;; you looking for. Feature 'category' was the macro filter does for
;; located the range you care about and reducing the querying scope at
;; the initial state. the second(also the last) query was string matching.
;;
;;;;;; Query with filter
;;
;; Based on category orgnization, each stuff can be found in one specific
;; category query about, this suits for people forgotten the accurate
;; stuffs entry title string, with just the fuzzy concept about the query
;; target, thus the category was the implicit one leading your find what
;; you want.
;;
;; Even that multi-categories supported, thus you may found single stuff
;; entry under various categories, this can help you given the
;; cross-relavant stuff management function , this is improtant because
;; of that tree style node orgnization was limited with it's single
;; direction less than the wide of network theory.
;;
;; Function ~entropy/cs-filter-open~ gives the way for thus, calling it
;; for a try -v-
;;
;;;;;; Query without filter
;;
;; There's no needs to query stuff entry with category querying leading
;; for in some daily using situation and it's seems occurred
;; frequently. In this case you can calling func ~entropy/cs-open-all~
;; to query with all stuff entry candidates directly. This func was
;; suggested binding to key-sequence =<C-f12>=.
;;
;;;;; Adding stuff
;;
;; =entropy-counsel-stuffs= support adding stuff entry by the interaction
;; way manually with the =ivy= completing framework. All you need to do
;; it was just calling interactively function ~entropy/cs-recorde-entry~,
;; once while does it calling operation on it, you will done sets of
;; inputting sequenced on =cstype=, =category=, =customid=, =location=
;; properties mentioned on section [[Database file content structer][stuff-attributes]], after these
;; inputting done, the new stuff entry will be inserted into the database
;; file where variable =entropy/cs-entry-recorde-file= denoted as is.
;;
;; As mentioned in section for previous sections, the inputting behaviour
;; of multi-categories was supported within new stuff entry adding
;; interaction process, with the method both of manually seperated
;; inputted string with semicolon or with candidates (match requiring)
;; selecting repeatly with ~ivy-call~ (binding with =ivy-minibuffer-map=
;; 'C-M-m') and showing with the brief prompting string as: 
;;
;; #+attr_org: :width 600px
;; #+attr_html: :width 600px
;; [[file:img/entropy-counsel-stuffs_repeatly-category-chosen_2019-01-20_01-29-53.png]]
;;
;; As the beneficence for auto-generated entry id, there's no need to
;; given the inputting operation for manually dispaching new id string
;; for the new stuff entry.
;;
;;
;;;;; Modified stuff
;;
;; The occurence for some embarrassments state while you want to change
;; one or sets of stuffs recorded information, you get the first reaction
;; for doing with raw database file editting manually. But there's the
;; readymade stuff modication interactivation function
;; ~entropy/cs-modifiy~ for thus on. Calling it while you wish to. 
;;
;;  
;;;; Apis 
;;
;; This package can be used as the fundametal library (or dependencies)
;; for other emacs packages' developments. 
;;
;; Even though, this package gives the comprehensive independent
;; interaction functional experience, I setted the core library of it as
;; be the API type what expected for other development case.
;;
;;;;; Database pointer
;;
;; As the narration within above context, the database org file was the
;; fundamental of calling-premise, the package developer can
;; local-binding with =let= former for redirected it's value to the
;; specified location as needed even if user customized value of this in
;; the global closure has gotten off.
;;
;; Thus we can give out for one function whose aim for openning dozen of
;; database file with user selected by the completion interaction
;; interface:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (let ((stuff_files (list "/home/temp/my-stuffs01.org"
;;                           "/home/temp/my-stuffs02.org"
;;                           "/home/temp/my-stuffs03.org"))
;;         chosen_file)
;;     (setq chosen_file (completing-read "Select stuff collection: "
;;                                        stuff_files nil
;;                                        :require-match))
;;     (let ((entropy/cs-entry-recorde-file chosen_file)
;;           (entropy/cs-cached-alist  nil))
;;       (entropy/cs-open-all)))
;; #+END_SRC 
;;
;; With the obviously another notice following the 'pointer' file let
;; form, cache list =entropy/cs-cached-alist= must be cleaned out before
;; the func =entropy/cs-open-all= using. There what you must notice that
;; variabnle =entropy/cs-cached-alist= was the second part of the
;; 'database pointer' for most =entropy-counsel-stuffs= apis, that the
;; way that the 'file pointer' for database cache retrieving and stuff
;; CRUD based, and the 'cache pointer' for database query filter for.
;;
;; Almost internal api's main function based only on this two variables,
;; it's that you should always forming them in the heading of the current
;; 'let' form.
;;
;;
;;
;;;;; Database caching
;;
;; While you've get the pointer file with 'let' form, commonly used with
;; this was for transfer the file content to the =elisp= data structer
;; specified basic on the designation of =entropy-counsel-stuffs= which
;; almost one nested 'alist' type form as:
;;
;; #+BEGIN_SRC elisp
;;   (list
;;    (("ITEM" "xxx")
;;     ("CATEGORY" "xxx")
;;     ("CUSTOM_ID" "xxx")
;;     ("CSTYPE" "xxx")
;;     ("LOCATION" "xxx"))
;;    .
;;    .
;;    .)
;; #+END_SRC 
;;
;; This cache list getted by func ~entropy/cs-get-cached-list~ without
;; any arguments need for inputting in, as the narratation that it
;; retrieving the current database file as the operation object where be
;; now just specified in your 'let' form.
;;
;; OFC, you don't need to built one data structer parsing manually
;; against to this returned cache nested alist, func
;; ~entropy/cs-filter-open~ (no args required) with the temporarily
;; sticking variable =entropy/cs-cached-alist= whose value must
;; retrieving by the cache getting func ~entropy/cs-get-cached-list~.
;;
;;
;; Demo:
;; #+BEGIN_SRC elisp
;;   (let ((entropy/cs-entry-recorde-file "xxxx")
;;         (entropy/cs-cached-alist (entropy/cs-get-cached-list)))
;;     (entropy/cs-filter-open))
;; #+END_SRC
;;
;;
;; The pointer workflow's diagram:
;;
;; #+BEGIN_EXAMPLE
;;                       -----------------------
;;                      ( database-file         )
;;                       ----------+------------
;;                                 |
;;                                 |
;;                                 v
;;                       +----------------------+
;;                       | get cache func       |
;;                       |                      |
;;                       +---------+------------+
;;                                 |
;;                                 |
;;                                 |
;;                                 v
;;                        -------------------
;;                       ( cache nested alist)
;;                        ---------+---------
;;                                 |
;;                                 |
;;                                 v
;;                       +----------------------+
;;                       | filter query func    |
;;                       |                      |
;;                       +----------------------+
;;
;; #+END_EXAMPLE
;;
;;; Configuration
;;
;; The only one thing you wish to initial setted was the customized
;; variable =entropy/cs-entry-recorde-file= which denoted as the stuffs
;; collection database mentioned in the preamble section. The value of it
;; was path string of the database org file.(the default value was
;; "~/Entropy-mini/20171116235728/org/bookmarks.org") 
;;
;; By the way, while you calling any interaction func of this package
;; will check the database file if be existing as is, and auto create it
;; at the opposite, so you may not need to create the database file
;; manually unless you wish to located the database file to the specific
;; location.
;; 
;;; Code:
;;;; require
(require 'entropy-open-with)
(require 'entropy-org-widget)
(require 'entropy-common-library)
(require 'entropy-common-library-const)
(require 'ivy)
;;;; variable declare
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




;;;; main library
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



;;;; Stuffs entries queried open function
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

;;;; Stuffs entries recorde function
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
;;;; Stuffs entries converter
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

;;;; Stuffs entries modified
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

;;;; Stuffs entry deletion
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

;;;; Recorde or Open macro
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



;;; provide
(provide 'entropy-counsel-stuffs)
