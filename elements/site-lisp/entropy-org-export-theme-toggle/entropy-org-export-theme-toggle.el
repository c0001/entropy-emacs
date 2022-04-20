;;; entropy-org-export-theme-toggle.el --- toggle org export theme for html, latex ... exporting  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-11-09, 2022 Entropy
;;
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       none
;; Created:       2018-11-09
;; Keywords:      org, theme
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (org "9.1.3") (dash "2.19.1"))
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
;;
;;; Commentary:
;;
;; *The theme toggle for Org export procedure.*
;;
;;;; Preamble
;;
;; The theme meta source contained in variable or caching in local
;; file. All themes denoted as the plist data structure presented as the
;; readable type both for extracted easily for package configuration and
;; main internal manipulation function.
;;
;; Org mode have the exporting framework for using to build various file
;; format e.g. Markdown(".md") , Html, and also for latex. The
;; individually file exports procedure have the unique process treatment
;; thus, this package must given the toggle method respectively. For now,
;; this package just implemented the html exporting treatment part, thus
;; follow instruction only has the html part.
;;
;;;; Html part
;;
;; + Themes plist: =entropy/org-exptt-html-theme-plist=
;;
;;   This customizable variable was the core config place for using
;;   this package as the theme toggle functional way as an alist
;;   prototype. Each element of it was one list of whose cdr is an
;;   plist formed as four key 1) =theme_category= 2) =:theme_css= 3)
;;   =:theme_js= 4) =:theme_mischellaneous=, and its car is the
;;   string indicate the theme name.
;;
;;   1) =:theme_category=
;;
;;      Each theme must contained into one category, category was
;;      represented by the string type, there's no default common
;;      category type binding for those who doesn't have specified one
;;      category, so that you can not find it at the interaction
;;      interface.
;;
;;      The valid value of this key was one string-list, thus multi
;;      category crossover relationship is supported. Thus you can
;;      specified the refer one as:
;;
;;      #+BEGIN_SRC elisp
;;        (list "common" "index" "section")
;;      #+END_SRC
;;
;;      That's demo denoted that this theme are designed into three way
;;      as what the category name presented
;;
;;   2) =:theme_css=
;;
;;      The css style list place. Each element of this key refer value is
;;      the css type string consists of the commonly html css embedded
;;      type as "<link href="">" external type or the
;;      "<style>...</style>" type as the full string stored . Notice, the
;;      href part must been exists of the external type.
;;
;;      Demo:
;;
;;      #+BEGIN_SRC elisp
;;        :theme_css
;;        ("<link rel=\"stylesheet\" title=\"Standard\" href=\"https://orgmode.org/worg/style/worg.css\" type=\"text/css\" />"
;;         "<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"https://orgmode.org/worg/style/worg-zenburn.css\" type=\"text/css\" />"
;;         "<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"https://orgmode.org/worg/style/worg-classic.css\" type=\"text/css\" />"
;;         "
;;        <style>
;;            body #content {
;;                padding-top: 0px;
;;                width: 55%;
;;                margin: 0 auto;
;;                margin-top: 5em;
;;                background-color: white;
;;                padding: 2em;
;;                ;; /* box-shadow: 3px 3px 5px #888; */
;;            }
;;
;;            body #postamble.status {width:65%;margin:0 auto;padding:2em;border: 0px;}
;;
;;            pre.src
;;            {
;;              overflow-x: scroll;
;;              overflow-y: scroll;
;;              max-height: 400px;
;;            }
;;
;;            pre.example
;;            {
;;              overflow-x: scroll;
;;              overflow-y: scroll;
;;              max-height: 400px;
;;            }
;;
;;
;;            img{max-width: 700px}
;;
;;            h3 {
;;              margin-left: 0em
;;            }
;;
;;            h4,
;;            h5 {
;;              font-size: 1.2em;
;;              margin-left: 0em
;;            }
;;
;;            h6,
;;            h7,
;;            h8,
;;            h9,
;;            h10 {
;;              font-size: 1.1em;
;;              font-weight: bold;
;;              color: crimson;
;;              margin-left: 1.3em
;;            }
;;
;;            blockquote
;;            {
;;              background-color: azure;
;;              padding:2%;
;;              border: 2px solid;
;;              border-color: darkgrey;
;;            }
;;
;;            .org-svg
;;            {
;;              max-width: 500px
;;            }
;;        </style>")
;;      #+END_SRC
;;
;;   3) =:theme_js=
;;
;;      The js part was fully similar with the css key expection that the
;;      'style' tag replaced as 'script'.
;;
;;      Demo:
;;
;;      #+BEGIN_SRC elisp
;;        :theme_js
;;        ("<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-1.11.0.min.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-ui-1.10.2.min.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.localscroll-min.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.scrollTo-1.4.3.1-min.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.zclip.min.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/bigblow.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/hideshow.js\"></script>"
;;         "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.min.js\"></script>")
;;      #+END_SRC
;;
;;   4) =:theme_mischellaneous=
;;
;;      Using for external 'link' tag list, as the css key does, but
;;      without cached (not implemented til now but for the furter.)
;;
;;      Demo:
;;
;;      #+BEGIN_SRC elisp
;;        :theme_mischellaneous
;;        ("<link rel=\"SHORTCUT ICON\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/x-icon\" />"
;;         "<link rel=\"icon\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/ico\" />")
;;      #+END_SRC
;;
;; + Themes cache:
;;
;;   This packae support caching the remote cdn meta data both of css and
;;   js but not of some icons link(no needed as that these resource can
;;   not embed into the html file). Caching using the emacs
;;   'url-retrieve' method to retrieving meta data and write the
;;   responses to into the unique file successively.
;;
;;   For each specified html theme, caching method will create the
;;   corresponding file respectively while there's no caching refer to
;;   the current selected theme. Otherwise, this package will throw one
;;   confirmation for quering whether update the current theme caches.
;;
;;   The root dir of the caching was defined of
;;   =entropy/org-exptt-html-theme-cache-dir=, but you can defined
;;   elsewhere you wander for.
;;
;;   *Exceptions:*
;;
;;   If an theme plist has an enabled key =:theme_dontcache=, then
;;   this theme can not be cached in anyway.
;;

;;; Code:
;;;; require
(require 'cl-lib)
(require 'org)
(require 'dash)

;;;; variable declaration
(defgroup entropy/org-exp-theme-toggle nil
  "Group for entropy-org-export-theme-toggle feature."
  :group 'extensions)

;;;;; html export variable
(defcustom entropy/org-exptt-html-theme-plist '()
  "Html theme toggle plist."
  :type 'sexp
  :group 'entropy/org-exp-theme-toggle)

(defcustom entropy/org-exptt-html-theme-cache-dir "~/.org-themes"
  "Local org html theme url retrieving cached dir"
  :type 'directory
  :group 'entropy/org-exp-theme-toggle)
(unless (and (not (ignore-errors
                    (string-empty-p
                     entropy/org-exptt-html-theme-cache-dir)))
             (directory-name-p entropy/org-exptt-html-theme-cache-dir))
  (mkdir entropy/org-exptt-html-theme-cache-dir t))

;;;;; external var required
(require 'entropy-org-export-theme-toggle_varset)

;;;; library
;;;;; export to html
;;;;;; basic
(defun entropy/org-exptth-category-extract ()
  "Extract org html export theme category of
`entropy/org-exptt-html-theme-plist' and return it."
  (let ((themes-plists (-map 'cdr entropy/org-exptt-html-theme-plist))
        category-temp category-rtn)
    (dolist (el themes-plists)
      (setq category-temp (plist-get el :theme_category))
      (when (and category-temp
                 (<= 1 (length category-temp)))
        (dolist (ca category-temp)
          (cl-pushnew ca category-rtn :test 'equal))))
    category-rtn))

(defun entropy/org-exptth-show-head (theme-plist)
  "Return org html export appended heads specified by
THEME-PLIST."
  (let* ((css (plist-get theme-plist :theme_css))
         (js (plist-get theme-plist :theme_js))
         (mischellaneous (plist-get theme-plist :theme_mischellaneous))
         (head-list (append css js mischellaneous))
         (head-string nil))
    (dolist (el head-list)
      (cond
       ((not head-string)
        (setq head-string el))
       (t
        (setq head-string (concat head-string "\n" el)))))
    head-string))

(defun entropy/org-exptth-choose-category ()
  "Prompt query selects org html export theme category and return
it as string."
  (let ((categories (entropy/org-exptth-category-extract))
        choice)
    (setq choice
          (completing-read "Choose html theme category: "
                           categories nil t))
    choice))

(defun entropy/org-exptth-list-themes-by-category (category)
  "List org html exports html themes by category CATEGORY."
  (let ((themes-list entropy/org-exptt-html-theme-plist)
        rtn)
    (dolist (el themes-list)
      (when (member category (plist-get (cdr el) :theme_category))
        (cl-pushnew el rtn :test 'equal)))
    rtn))

(defun entropy/org-exptth-choose-themes (category)
  "Prompt quering and selected theme specified by category
  CATEGORY and return concated theme head."
  (let* ((themes (entropy/org-exptth-list-themes-by-category category))
         (tnames nil)
         (choice nil)
         (embedded-p nil)
         rtn)
    (dolist (el themes)
      (cl-pushnew (car el) tnames :test 'equal))
    (setq choice (completing-read "Choose html theme: " tnames nil t))
    (setq embedded-p
          (unless (plist-get
                   (--some (when (equal (car it) choice) (cdr it))
                           themes)
                   :theme_dontcache)
            (yes-or-no-p "Using embedded style? ")))
    (dolist (el themes)
      (when (equal (car el) choice)
        (if (not embedded-p)
            (setq rtn (entropy/org-exptth-show-head (cdr el)))
          (setq rtn (entropy/org-exptth-get-head-cache el)))))
    rtn))

(defun entropy/org-exptth-input-manually ()
  "Manually input org html head repeatly while not input ':quit'
and return the inputs."
  (let (rtn
        (temp-string ""))
    (while (not (equal temp-string ":quit"))
      (cond
       ((not rtn)
        (setq temp-string (read-string "Input html head: ")
              rtn (if (not (equal temp-string ":quit")) temp-string "")))
       (t
        (setq temp-string (read-string "Cotinue input head (or finished by input ':quit'): ")
              rtn (if (not (equal temp-string ":quit")) (concat rtn "\n" temp-string) rtn)))))
    rtn))


;;;;;; cached html themes
(defun entropy/org-exptth-get-head-cache (theme-object)
  "Get org html themes local cached and embedded them into export
file, if not cached for specific theme, download it using
`url-retrieve-synchronously'.

Cached file named with theme name indicated by arg theme-object's
key ':theme-name', cache stored location rely on the base dir
`entropy/org-exptt-html-theme-cache-dir.'

Arg TEMP-PLIST was the element of
`entropy/org-exptt-html-theme-object'.

See also function `entropy/org-exptth-theme-urls-combine-cache'."
  (let* ((theme-name (car theme-object))
         (theme-plist (cdr theme-object))
         (cachable (null (plist-get theme-plist :theme_dontcache)))
         (css (plist-get theme-plist :theme_css))
         (js (plist-get theme-plist :theme_js))
         (mischellaneous (plist-get theme-plist :theme_mischellaneous))
         css-snippets
         js-snippets
         css-cache
         js-cache
         rtn)
    (cond ((and (not (entropy/org-exptth-theme-cached theme-name))
                cachable)
           (dolist (el css)
             (if (string-match "href=\"\\(.*?\\)\"" el)
                 (push (list :url (match-string 1 el)) css-snippets)
               (push (list :content el) css-snippets)))
           (setq css-snippets (reverse css-snippets))

           (when (yes-or-no-p "Embedded js part? ")
             (dolist (el js)
               (if (string-match "src=\"\\(.*?\\)\"" el)
                   (push (list :url (match-string 1 el)) js-snippets)
                 (push (list :content el) js-snippets)))
             (setq js-snippets (reverse js-snippets)))

           (setq css-cache (entropy/org-exptth-theme-urls-combine-cache css-snippets 'css)
                 js-cache
                 (when js-snippets
                   (entropy/org-exptth-theme-urls-combine-cache js-snippets 'js)))
           (setq rtn (concat css-cache "\n\n" js-cache))
           (when (not js-cache)
             (dolist (el js)
               (setq rtn (concat rtn "\n" el))))
           (dolist (el mischellaneous)
             (setq rtn (concat rtn "\n" el)))
           (entropy/org-exptth-write-theme-cache theme-name rtn))
          (t (if cachable
                 (if (yes-or-no-p (format "Update %s's cache? " theme-name))
                     (progn
                       (delete-file (expand-file-name theme-name entropy/org-exptt-html-theme-cache-dir))
                       (setq rtn (entropy/org-exptth-get-head-cache theme-object))
                       (message "Update theme done"))
                   (setq rtn (entropy/org-exptth-theme-cached theme-name)))
               (error "Theme '%s' can not be cached because of theme spec"
                      theme-name))))
    rtn))


(defun entropy/org-exptth-theme-cached (theme-name)
  "Return the cached org html theme file content if corresponding
theme cache file exits or return nil otherwise.

This func used for func `entropy/org-exptth-get-head-cache'."
  (let* ((base-dir entropy/org-exptt-html-theme-cache-dir)
         (fname (expand-file-name theme-name base-dir))
         rtn)
    (if (file-exists-p fname)
        (setq rtn
              (with-temp-buffer
                (insert-file-contents-literally fname)
                (buffer-string)))
      (setq rtn nil))
    rtn))

(defun entropy/org-exptth-theme-urls-combine-cache (snippets type)
  "Using for retriving org html theme url directed css or js file
content and combined each of them into head string.

Arg SNIPPET was element of variable extract from partitial
process of func `entropy/org-exptth-get-head-cache' which was the
list consists of element consists by the car was symbol indicator
'url' (included for style as <style ...> or <script ...>) or
'content' which local string specification., car of cdr was
'url-or-content'.

Arg TYPE gives the snippets collected for css or js, each of them
set as symbol for `cl-case' judged for.

Core based was function `entropy/org-exptth-theme-urls-retrieve'."
  (let (rtn)
    (cl-case type
      ('css (setq rtn (entropy/org-exptth-theme-urls-retrieve
                       snippets
                       '("<style>" . "</style>"))))
      ('js (setq rtn (entropy/org-exptth-theme-urls-retrieve
                      snippets
                      '("<script>" . "</script>")))))
    rtn))

(defun entropy/org-exptth-theme-urls-retrieve (snippets flag-cons)
  "The entity core based process for function
`entropy/org-exptth-theme-urls-combine-cache'."
  (let ((rtn (concat (car flag-cons) "\n"))
        use-proxy-p)
    (dolist (el snippets)
      (let ((type (car el))
            (url-or-content (nth 1 el)))
        (cl-case type
          (:url
           (with-temp-buffer
             (let ((body
                    (let ((form `(url-retrieve-synchronously ,url-or-content nil t)))
                      (if (fboundp 'entropy/proxy-url-with-url-proxy)
                          (progn
                            (if (or use-proxy-p
                                    (and (yes-or-no-p "Use proxy \
(assume you have setted `entropy/proxy-url-default-http-sever-host&port-string')?"
                                                      )
                                         (setq use-proxy-p t)))
                                (entropy/proxy-url-with-url-proxy
                                  (eval form))
                              (eval form)))
                        (eval form)))))
               (when body
                 (url-insert body)
                 (setq rtn (concat rtn (buffer-string) "\n\n"))))))
          (:content (setq rtn (concat rtn
                                      (replace-regexp-in-string "<.*?>" "" url-or-content)
                                      "\n\n"))))))
    (setq rtn (concat rtn (cdr flag-cons)))
    rtn))

(defun entropy/org-exptth-write-theme-cache (theme-name content)
  "write url retriving combined string into cache file stored in
`entropy/org-exptt-html-theme-cache-dir', string value CONTENT
obtained from func `entropy/org-exptth-theme-urls-combine-cache'.

THEME-NAME specify the cached file name."
  (let* ((base-dir entropy/org-exptt-html-theme-cache-dir)
         (fname (expand-file-name theme-name base-dir)))
    (when (file-exists-p fname)
      (delete-file fname))
    (let (
          ;; using raw-text method to save the content since some
          ;; content are not readable for human meaning and we do not
          ;; need to persist the content coding-system and we should
          ;; not let user to specify the write coding system manually.
          (coding-system-for-write 'raw-text))
      (with-temp-buffer
        (insert content)
        (write-region nil nil fname)))))

;;;; main

;;;###autoload
(defun entropy/org-exptth-set-head ()
  "Setting `org-html-head' with themes specified by
`entropy-org-export-theme-toggle.el'."
  (interactive)
  (let ((setting-type (yes-or-no-p "Do you setting ox html theme manually? ")))
    (cond (setting-type
           (setq org-html-head-extra (entropy/org-exptth-input-manually)))
          ((not setting-type)
           (let ((head-string (entropy/org-exptth-choose-themes (entropy/org-exptth-choose-category))))
             (setq org-html-head-extra head-string))))))


;;; provide
(provide 'entropy-org-export-theme-toggle)
