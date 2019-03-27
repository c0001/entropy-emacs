;;; entropy-org-export-theme-toggle.el --- toggle org export theme for html, latex ... exporting

;; Copyright (C) 2018-11-09  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       none
;; Created:       2018-11-09
;; Keywords:      org, theme
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Toggle different theme setting before org export or publish process begin.
;;
;;; Configuration:
;;
;; 

;;; Code:
;; ** variable declaration
(defgroup entropy/org-exp-theme-toggle nil
  "Group for entropy-org-export-theme-toggle feature. ")

;; *** html export variable
(defcustom entropy/org-exptt-html-theme-plist '()
  "Html theme toggle plist."
  :type 'sexp
  :group 'entropy/org-exp-theme-toggle)

(defcustom entropy/org-exptt-html-theme-cache-dir
  (if (file-exists-p "~/.org-themes")
      "~/.org-themes"
    (make-directory "~/.org-themes")
    "~/.org-themes")
  "Local org html theme url retrieving cached dir"
  :type 'string
  :group 'entropy/org-exp-theme-toggle)

;; *** external var required
(require 'entropy-org-export-theme-toggle_varset)

;; ** library
;; *** export to html
;; **** basic
(defun entropy/org-exptth-category-extract ()
  "Extract org html export theme category of
`entropy/org-exptt-html-theme-plist' and return it."
  (let ((themes-list entropy/org-exptt-html-theme-plist)
        category-temp category-rtn)
    (dolist (el themes-list)
      (setq category-temp (plist-get el :theme_category))
      (when (and category-temp
                 (<= 1 (length category-temp)))
        (dolist (ca category-temp)
          (add-to-list 'category-rtn ca))))
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
    (setq choice (completing-read "Choose html theme category: " categories nil t))
    choice))


(defun entropy/org-exptth-list-themes-by-category (category)
  "List org html exports html themes by category CATEGORY."
  (let ((themes-list entropy/org-exptt-html-theme-plist)
        rtn)
    (dolist (el themes-list)
      (when (member category (plist-get el :theme_category))
        (add-to-list 'rtn el)))
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
      (add-to-list 'tnames (plist-get el :theme_name)))
    (setq choice (completing-read "Choose html theme: " tnames nil t))
    (setq embedded-p (yes-or-no-p "Using embedded style? "))
    (dolist (el themes)
      (when (equal (plist-get el :theme_name) choice)
        (if (not embedded-p)
            (setq rtn (entropy/org-exptth-show-head el))
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


;; **** cached html themes
(defun entropy/org-exptth-get-head-cache (theme-plist)
  "Get org html themes local cached and embedded them into export
file, if not cached for specific theme, download it using
`url-retrieve-synchronously'.

Cached file named with theme name indicated by arg theme-plist's
key ':theme-name', cache stored location rely on the base dir
`entropy/org-exptt-html-theme-cache-dir.'

Arg TEMP-PLIST was the element of
`entropy/org-exptt-html-theme-plist'.

See also function `entropy/org-exptth-theme-urls-combine-cache'."
  (let ((theme-name (plist-get theme-plist :theme_name))
        (css (plist-get theme-plist :theme_css))
        (js (plist-get theme-plist :theme_js))
        (mischellaneous (plist-get theme-plist :theme_mischellaneous))
        css-snippets
        js-snippets
        css-cache
        js-cache
        rtn)
    (cond ((not (entropy/org-exptth-theme-cached theme-name))
           (dolist (el css)
             (if (string-match "href=\"\\(.*?\\)\"" el)
                 (add-to-list 'css-snippets (list :url (match-string 1 el)))
               (add-to-list 'css-snippets (list :content el))))
           (setq css-snippets (reverse css-snippets))
           
           (when (yes-or-no-p "Embedded js part? ")
             (dolist (el js)
               (if (string-match "src=\"\\(.*?\\)\"" el)
                   (add-to-list 'js-snippets (list :url (match-string 1 el)))
                 (add-to-list 'js-snippets (list :content el))))
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
          (t (if (yes-or-no-p (format "Update %s's cache? " theme-name))
                 (progn
                   (delete-file (expand-file-name theme-name entropy/org-exptt-html-theme-cache-dir))
                   (setq rtn (entropy/org-exptth-get-head-cache theme-plist))
                   (message "Update theme done"))
               (setq rtn (entropy/org-exptth-theme-cached theme-name)))))
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
              (with-current-buffer (find-file-noselect fname)
                (let (content)
                  (setq content (buffer-string))
                  (kill-buffer)
                  content)))
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
      ('css (setq rtn (entropy/org-exptth-theme-urls-retrieve snippets '("<style>" . "</style>"))))
      ('js (setq rtn (entropy/org-exptth-theme-urls-retrieve snippets '("<script>" . "</script>")))))
    rtn))

(defun entropy/org-exptth-theme-urls-retrieve (snippets flag-cons)
  "The entity core based process for function
`entropy/org-exptth-theme-urls-combine-cache'."
  (let ((rtn (concat (car flag-cons) "\n")))
    (dolist (el snippets)
      (let ((type (car el))
            (url-or-content (nth 1 el)))
        (cl-case type
          (:url
           (with-temp-buffer
             (let ((body (url-retrieve-synchronously url-or-content nil t)))
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
    (with-current-buffer (find-file-noselect fname)
      (when buffer-read-only
        (read-only-mode 0))
      (insert content)
      (save-buffer)
      (kill-buffer))))

;; ** main

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


;; * provide
(provide 'entropy-org-export-theme-toggle)
