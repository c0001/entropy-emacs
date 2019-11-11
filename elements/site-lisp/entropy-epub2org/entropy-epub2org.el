;;; entropy-epub2org --- convert epub book to org files set

;; Copyright (C) 20180930  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           None
;; Package-Version: 0.1.0
;; Created:       2018-09-30 07:51:36
;; Keywords:      epub, ebook-convert, tools,
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))

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
;; For emacs user, the native vision of the getting style of reading
;; ebooks was originally tends to the method which be based on emacs it
;; self. For this pure and fully power imagination, I thought about sets
;; of hacking methods, through the way of 'sh-render' each xml or html
;; file within ebook as the melpa package [[https://github.com/wasamasa/nov.el][nov.el]] does, or using convert
;; way for as that transfer each ebook's internal ebook page to org
;; file. Obviously, the first one have the exits extension does, thus
;; this package was serving as the latter goal.  
;;
;;
;; Although kinds of ebook wrapper format are popular covering the word,
;; e.g. epub, awz3, mob ..., there's just the wrapper =epub= gets the
;; free for. Thus this package only given the try for converting the
;; epub format ebook to Org files collection ([[https://orgmode.org/][Org]] was the most popular
;; rich plain text format using in emacs).
;;
;;
;;
;; =entropy-epub2org= using four steps to process the convertting
;; procedure:
;;
;; 1) Unzip the epub file to the folder archived with sets of webpages
;;   which were the main contents host files.
;;
;; 2) Parsing the index part tree into org files.
;;
;; 3) Convert all content carrier file into org files named as is.
;;
;; 4) Tidy up all converted org files into the normal org context format.
;;
;;
;;;; Dependencies 
;;
;; this package depends on some external cli dependencies:
;; - [[https://pandoc.org/][pandoc]]
;; - unzip
;; - dos2unix
;;
;; Above two external cli tool unzip and dos2unix were common embedded
;; into various linux distribution, if not as, you should download them
;; using dist's package managements tool from the corresponding package
;; repository or from the DVD iso.
;;
;; For WINDOWS platform, I suggested using Msys2 to be the GNU
;; environment emulator which commonly embedded unzip and dos2unix as
;; well into it's installation, if not using Msys2's package management
;; =pacman= to install them.
;;
;; Pandoc was cross-platform tool using for providing various file type
;; converted each ohter, as thus support webpage source file converted
;; into Org file. In generally case that you should download and install
;; it manually from it's official website (mentioned in above list).
;;
;;
;;;; Requirements
;;
;; - entropy-common-library
;; - entropy-common-library-const
;; - entropy-unfill
;; - org
;; - files
;;
;; All the library named using the prefix =entropy-= was embedded into
;; [[https://github.com/c0001/entropy-emacs][entropy-emacs]], you can find them there and then download it into your
;; load path.
;;
;;;; Installation
;;
;; Just require it as:
;;: (require 'entropy-epub2org) 
;;
;;;; Interaction
;;
;;=entropy-epub2org= was easy to start it with calling only one func
;;~entropy/ep2o-dispatcher~ which one shabby(currently simplify used as)
;; and enough for walking through all the functionally.
;;
;; This func was interactively only for as. The once you call it, it
;; prompt you to choose the ebook location which will be chosen about,
;; and then unzip it for following steps processing mentioned in the
;; preamble.
;;
;; Just try to calling it as the first experience ^^:)
;;
;;;; Modification before/after converting
;;
;; For generally attempts for, not all content transfer from ebook source
;; to org format were comprehensively as what be along with your
;; expection. In this case, you wish to re-converted them agian for using
;; some modification rule using for ~entropy/ep2o-tidy-up-all-org-files~
;; which was the core tidy func using in step 'tidy up', of cousrse you
;; can use hook =entropy/ep2o-tidy-hook= to retry as the yet another way.
;;
;; The core aspects of 'tidy up' procedure was searching replacing the
;; unexpected converted content into the expected one following the
;; regexp rule list =entropy/ep2o-replace-rule-alist= whose each element
;; was the cons whose the car was the unexpected thing regexp matching
;; for, and the cdr otherwise. =entropy/ep2o-replacing-rule-alist= have
;; the default value setted for commonly useing in the first converted
;; processing but was absolutely limited of be that can not do with
;; widely unexpected occurrence, thus you can order extra rules by
;;~add-to-list~ as is for responding to the customized way. This
;; paragraph gives the way for ordering the specified modification before
;; converting.
;;
;; But as the human's imagination limitation, thus you couldn't get the
;; comprehensive looking for all the modification cases, you need to make
;; the modification process after the intial converting for generally
;; speaking. In this case you should using func
;; ~entropy/ep2o-src-adjusting-manually~ to modify the rest unexpected
;; thing have got. all you need was for inputting extra searching
;; replacing rule set once or repeatly for as with prompting it's
;; interface, thus til the state expected by your self.
;;
;;; Change log
;; - 2018-09-30: Version 0.1.0 release

;;   The initial release.

;;; Code:
;;;; require
(require 'entropy-common-library)
(require 'entropy-common-library-const)
(require 'entropy-unfill)
(require 'org)
(require 'files)

;;;; defvar
(defvar entropy/ep2o-ops-srcfiles-regexp "\\.\\(x?html?\\|xml\\)$"
  "Epub book splitted source file extention regexp matching rule.")
(defvar entropy/ep2o-replace-rule-alist
  `(
    ;; convert html foot note sytle to org style
    ("<<w\\([0-9]+\\)>>\\[\\[.*?\\]\\[\\^{\\[[0-9]+\\]}\\]\\]" . "[fn:\\1]")
    ("<<m\\([0-9]+\\)>>\\[\\[.*?\\]\\[\\[[0-9]+\\]\\]\\]" . "[fn:\\1]")
    ("\\(\\[fn:[0-9]+\\]\\)\\([^ [:blank:]]\\)" . "\\1 \\2")

    ;; temporarily transer hyper link foot note style to org style
    ("\\[\\[\\(.*?\\)\\(\\.x?html?\\|xml\\)\\(#.*?\\)\\]" . "[[\\1.org::\\3]")
    ("\\]\\[\\[\\([0-9]+?\\)\\]\\]\\]" . "][\\1]]")

    ;; remove cjk paragraph indentation
    ("　" . "")

    ;; replacing two back slash org newline flag to actually newline style.
    ("\\\\" . "\n")

    ;; adding space between punctuation and asterisk which be as emphasis signal
    (,(format "\\*\\(%s\\)" entropy/clconst-cjk-punc-regexp) . "* \\1")
    (,(format "\\(%s\\)\\*" entropy/clconst-cjk-punc-regexp) . "\\1 *")

    ;; replacing internal html link to org files
    ("\\(\\[\\[file:.+?\\)\\.\\(x?html?\\|xml\\)" . "\\1.org"))
  "Regexping replacing rule for converted org file.

  The doing sequence was orderd by the origin list elements sequences.")

(defvar entropy/ep2o-index-relative nil
  "Temporarily store the index-file relative path as current
  converted org src file.")

(defvar entropy/ep2o-org-after-indent-list nil
  "List stored pre-indentation org files after converting done.")

(defvar entropy/ep2o-tidy-hook nil
  "Hook for run function to customized converted org file at the
  last procedure of func `entropy/ep2o-tidy-up-all-org-files'.")

;;;; ncx file parsing
;;
;; The first thing for convert epub format files sets to org sets was to
;; extracting the summary of it and convert it to be as org index file.
;;
;; The whole index of one epub sets was contained in one 'xml' file which
;; always named with the file extention as 'ncx' like 'toc.ncx'.
;;
;; Following function or library was aimed for extract the 'DOM' object
;; as one list and then parsing them into the tree headline string that
;; as usually doing of commonly org file operation.
;;
;; The 'DOM' extracting from the 'ncx' file was really simple, and can be
;; focus on one object ---'navPoint' object.
;;
;; 'navPoint' object can be self-contained for presenting the hierarchy
;; of the book's summary which consistencies can be shown as the follow
;; diagram:
;;
;; #+BEGIN_EXAMPLE
;;              +----------+
;;              | navpoint |
;;              +----------+
;;                 -/| \-
;;               -/  |   \-
;;             -/    |     \-
;;    -------o/ -----+-----  \o
;;   (navLabel)( content   ) +----------+
;;    -------   -----------  | navpoint |
;;                           +----------+
;; #+END_EXAMPLE
;;
;; - navLabel: was the description of this summary node
;; - content: was the corresponding file this node introducted.
;; - navpoint: sub-node.
;;
;;
;;
;;
;;;;; ncx file parsing library
(defun entropy/ep2o-get-navLabel (navpoint)
  (let (rtn mlist)
    (dolist (el navpoint)
      (when (listp el)
        (when (equal 'navLabel (car el))
          (setq mlist el))))
    (dolist (el mlist)
      (when (listp el)
        (when (equal 'text (car el))
          (setq rtn (nth 2 el)))))
    rtn))

(defun entropy/ep2o-get-nvcontent (navpoint)
  (let (rtn)
    (dolist (el navpoint)
      (when (listp el)
        (when (equal 'content (car el))
          (setq mlist el))))
    (setq rtn (cdr (car (nth 1 mlist))))
    rtn))


(defun entropy/ep2o-navpoint-is-rescursive (navpoint)
  "Checking whether there's sub-node within this navpoint node."
  (let ((rtn nil))
    (when (equal (car (nth 4 navpoint))
                 'navPoint)
      (setq rtn t))
    rtn))

(defun entropy/ep2o-get-nav-info (navpoint)
  "Make one list of cons for store the navpoing dom node's self
node information.

Format like:
'((Label . content))"
  (let ((label (entropy/ep2o-get-navLabel navpoint))
        (content (entropy/ep2o-get-nvcontent navpoint))
        mlist
        rtn)
    (push (cons label content) mlist)
    (setq rtn mlist)
    rtn))


(defun entropy/ep2o-get-nav-rest (navpoint)
  "Obtaining the recursively self-sub navpoint nodes list."
  (let ((rtn navpoint)
        number)
    (dotimes (number 4 rtn)
      (setq rtn (delete (car rtn) rtn)))
    rtn))


(defun entropy/ep2o-get-nav-tree (navlist outlist &optional level)
  "Recursively espanded the navpoint nodes list NAVLIST to the
external list OUTLIST, the optional arg LEVEL was remained used
for recursively calling funciton, do not setting it manually. "
  (setf outlisit nil)
  (dolist (el navlist)
    (let ((is-re (entropy/ep2o-navpoint-is-rescursive el))
          (lv (if level level 1)))
      (add-to-list outlist `(,lv ,(entropy/ep2o-get-nav-info el)))
      (when is-re
        (let ((nav-slist (entropy/ep2o-get-nav-rest el)))
          (entropy/ep2o-get-nav-tree nav-slist outlist (+ lv 1)))))))

(defun entropy/ep2o-get-nav-positive-tree (navlist)
  "Make the tree list created by `entropy/ep2o-get-nav-tree' be
the same positively sequenced as the origin navPoint list."
  (let* (rtn
         (outlist 'rtn))
    (entropy/ep2o-get-nav-tree navlist outlist)
    (let ((mlist (symbol-value outlist)))
      (setf (symbol-value outlist) (reverse mlist)))
    rtn))



;;;;; ncx file parsing main function
(defun entropy/ep2o-get-ncx-buffer (&optional file)
  "Getting ncx file and return it's opened buffer.

Optional arg FILE was the file path string, if none-nil,
automatically parsing the ncx file without prompting query
selected ncx file"
  (let* ((file (if file file
                 (completing-read "Choose ncx file: " 'read-file-name-internal nil t)))
         (buffer (find-file-noselect file)))
    buffer))

(defun entropy/ep2o-get-ncx-dom (&optional file)
  "Return DOM object of the selected ncx file FILE if FILE was
none-nil."
  (let (rtn)
    (with-current-buffer (if file
                             (entropy/ep2o-get-ncx-buffer file)
                           (entropy/ep2o-get-ncx-buffer))
      (nxml-mode)
      (let ((start (point-min))
            (end (point-max)))
        (setq rtn (libxml-parse-xml-region start end nil t))
        (kill-buffer))
      rtn)))

(defun entropy/ep2o-get-navlist (&optional file)
  "Return navlist which was modified from ncx file's DOM list ,
the difference between them was that navlist just has the
navpoint dom node and it's self recursive."
  (let ((dom (if file
                 (entropy/ep2o-get-ncx-dom file)
               (entropy/ep2o-get-ncx-dom)))
        (mlist nil)
        (rtn nil))
    ;; navpoint node was the sub-node of navMap, extract it first.
    (dolist (el dom)
      (when  (equal (if (listp el) (car el) nil) 'navMap)
        (push el mlist)))
    (dolist (el (car mlist))
      (when (equal (if (listp el) (car el) nil) 'navPoint)
        (add-to-list 'rtn el)))
    (setq rtn (reverse rtn))
    rtn))




;;;; convertting html file to org file

(defun entropy/ep2o-list-html-src (&optional dir)
  "Return the list of html or xml or other source files of this epub book.

If arg DIR was none-nil , processing with prompting of manully
choosing the target source file directory."
  (let ((dir (if dir dir (completing-read "Choose OPS dir: " 'read-file-name-internal)))
        flist
        hlist)
    (if (and (directory-name-p dir)
             (file-exists-p dir))
        (progn
          (setq flist (entropy/cl-list-dir-lite dir)))
      (error (format "Directory %s was not ops dir." dir)))
    (if flist
        (progn
          (dolist (el flist)
            (when (and (equal "F" (car el))
                       (string-match-p entropy/ep2o-ops-srcfiles-regexp (cdr el)))
              (push (cdr el) hlist))))
      (error (format "There's nothing in %s ." dir)))
    ;; transfer path type
    (let (mlist)
      (when (equal system-type 'windows-nt)
        (dolist (el hlist)
          (push (replace-regexp-in-string "/" "\\" el t t) mlist))
        (setq hlist mlist)))
    hlist))

(defun entropy/ep2o-pandoc-html2org-and-return-orglist (html-files-list)
  "Using pandoc convert source files synchronously from the list
created by `entropy/ep2o-list-html-src'."
  (when (not (executable-find "pandoc"))
    (error "Pandoc not found in your path."))
  ;; convert html file to org format
  (let ((rtbuffer (get-buffer-create "*entropy/ep2o-pandoc*"))
        (w32-quote-process-args nil))
    (dolist (el html-files-list)
      (call-process "pandoc" nil rtbuffer nil
                    el
                    "-o"
                    (replace-regexp-in-string
                     entropy/ep2o-ops-srcfiles-regexp ".org" el))
      (message "Complete convert %s to org file." el)))

  ;; list converted org files
  (let* ((dir (file-name-directory (car html-files-list)))
         (dir-list (entropy/cl-list-dir-lite dir))
         rtn)
    (dolist (el dir-list)
      (when (and (equal "F" (car el))
                 (and (string-match-p "\\.org$" (cdr el))
                      (not (string-match-p "\\.#" (cdr el)))))
        (push (cdr el) rtn)))
    rtn))

;;;; replacing error syntax formats of org files
(defun entropy/ep2o-replacing-by-rules ()
  "Using `entropy/ep2o-replace-rule-alist' to replacing the
messing syntax produced by pandoc automatically."
  (dolist (el entropy/ep2o-replace-rule-alist)
    (entropy/cl-replacing-buffer el)))

;;;; tidy up all org files
(defun entropy/ep2o-generate-back-to-index-string (ep2o-dir-struct)
  (let* ((ops-dir (nth 1 ep2o-dir-struct))
         (index-file (car ep2o-dir-struct))
         (index-dir (file-name-directory index-file))
         (oi-relative (entropy/cl-dir-relativity-number ops-dir index-dir))
         (oi-re-count (car oi-relative))
         (oi-re-path (nth 1 oi-relative))
         (oi-re-type (nth 2 oi-relative))
         rtn)
    (cond
     ((not oi-re-type)                  ;not same branch
      (let ((count 1)
            (rtv-back "")
            (rtv-abbr "")
            rtv-full)
        (dolist (el oi-re-path)
          (if (equal "" rtv-back)
              (setq rtv-back el)
            (setq rtv-back (concat rtv-back "/" el))))
        (while (<= count (- oi-re-count))   ;generate abbr path
          (setq rtv-abbr (concat rtv-abbr "../"))
          (setq count (+ 1 count)))
        (if (not (equal "" rtv-back))
            (setq rtv-full (concat rtv-abbr rtv-back "/"
                                   (file-name-nondirectory index-file)))
          (setq rtv-full (concat rtv-abbr (file-name-nondirectory index-file))))
        (setq rtn rtv-full)))
     (oi-re-type                        ;in same branch
      (cond
       ((> oi-re-count 0)
        (let ((rtv-back ""))
          (dolist (el oi-re-path)
            (setq rtv-back (concat rtv-back el "/")))
          (setq rtn (concat rtv-back (file-name-nondirectory index-file)))))
       ((= oi-re-count 0)
        (setq rtn (file-name-nondirectory index-file)))
       ((< oi-re-count 0)
        (let ((rtv-abbr "")
              (count 1))
          (while (<= count (- oi-re-count))
            (setq rtv-abbr (concat rtv-abbr "../"))
            (setq count (+ 1 count)))
          (setq rtn (concat rtv-abbr (file-name-nondirectory index-file))))))))
    (setq rtn (concat "file:" rtn))
    rtn))


(defun entropy/ep2o-tidy-org-indentation-warning (after-indent-list)
  "When org file page lines larger than 1000 warn with frozing
behaviour caused by `org-indent-region'.

Arg after-indent-list was the except file list for dealing manually after converting.

This function dealing with buffer `current-buffer'.

This function return t or nil for judge whether do what to do for 
`entropy/ep2o-tidy-up-all-org-files'."
  
  (let ((line-count (count-lines (point-min) (point-max)))
        (do-indent t)
        rtn)
    (when (< 1000 line-count)
      (pop-to-buffer (current-buffer))
      (setq do-indent (yes-or-no-p
                       (format
                        "Long line with buffer %s, do you really indenting it?"
                        (buffer-name (current-buffer))))))
    (if (not do-indent)
        (progn
          (add-to-list after-indent-list (buffer-file-name (current-buffer)))
          (setq rtn nil))
      (setq rtn t))
    rtn))


(defun entropy/ep2o-tidy-up-all-org-files (org-files ep2o-dir-struct &optional indent-warn)
  "Tidy up the converted org file using function
`entropy/ep2o-replacing-by-rules', and doing:

- unfill the current org file buffer
- indentation current buffer
- fill column current buffer
- inserting 'back to index' link

And then run hook `entropy/ep2o-tidy-hook'."
  (dolist (el org-files)
    (with-current-buffer (find-file-noselect el)
      (entropy/ep2o-replacing-by-rules)
      (entropy/unfill-full-buffer-without-special-region)
      (read-only-mode 0)
      (if (not indent-warn)
          (when (entropy/ep2o-tidy-org-indentation-warning
                 'entropy/ep2o-org-after-indent-list)
            (org-indent-region (point-min) (point-max))
            (entropy/fill-full-buffer-without-special-region)
            (read-only-mode 0))
        (progn
          (org-indent-region (point-min) (point-max))
          (entropy/fill-full-buffer-without-special-region)
          (read-only-mode 0)))
      ;; fill foot note
      (progn
        (text-mode)
        (goto-char (point-min))
        (while (re-search-forward "^\\[fn:" nil t)
          (fill-paragraph)))
      (goto-char (point-min))
      (dotimes (el 2 nil)
        (newline))
      (goto-char (point-min))
      (insert (format "[[%s][Back To Index]]\n\n" (if entropy/ep2o-index-relative
                                                      entropy/ep2o-index-relative
                                                    (progn (setq entropy/ep2o-index-relative
                                                                 (entropy/ep2o-generate-back-to-index-string ep2o-dir-struct))
                                                           entropy/ep2o-index-relative))))
      (run-hooks 'entropy/ep2o-tidy-hook)
      (save-buffer)
      (kill-buffer))))


;;;; convert end-of-lines type
(defun entropy/ep2o-dos2unix-org-files (org-files)
  (let ((test-file (car org-files))
        (is-dos nil))
    (with-current-buffer (find-file-noselect test-file)
      (goto-char (point-min))
      (when (re-search-forward "" nil t)
        (setq is-dos t))
      (kill-buffer))
    (when is-dos
      (message "Will convernt dos to unix.")
      (when (not (executable-find "dos2unix"))
        (error "Can not find 'dos2unix' in your path"))
      (let ((buffer (get-buffer-create "*entropy/ep2o-dos2unix*")))
        (dolist (el org-files)
          (call-process "dos2unix" nil buffer nil
                        (if (equal system-type 'windows-nt)
                            (replace-regexp-in-string "/" "\\" el t t)
                          el)))))
    (when (not is-dos)
        (message "Not need to dos2nux."))))



;;;; write index org file
(defun entropy/ep2o-get-index-file (&optional file)
  (let ((fname (if file file (completing-read "Choose index file location: " 'read-file-name-internal)))
        buffer)
    (if (not (file-exists-p fname))
        (write-region "" "" fname))
    (setq buffer (find-file-noselect fname))
    buffer))

(defun entropy/ep2o-parse-index-asterisks (number)
  "Generate the asterisks string relying on the hierarchy of the
navpoint level."
  (let (mlist
        (rtn ""))
    (dotimes (el number rtn)
      (push "*" mlist))
    (dolist (el mlist)
      (setq rtn (concat rtn el)))
    rtn))

(defun entropy/ep2o-parse-index-name (title-list ep2o-dir-struct)
  "Return index head name by parsing file-link and link description respectively."
  (let* ((title (car title-list))
         (head-link (cdr title))
         (head-name (car title))
         struct
         rtn)

    ;; Dir struct parse
    (let* ((index-dir (file-name-directory (car ep2o-dir-struct)))
           (ops-dir (nth 1 ep2o-dir-struct))
           (io-relative (entropy/cl-dir-relativity-number
                         index-dir ops-dir))
           (io-re-count (car io-relative))
           (io-re-path (nth 1 io-relative))
           (io-re-type (nth 2 io-relative))
           io-string)                 ;io-string was string to insert into org headline.

      (cond
       ((equal nil io-re-type) ;cond that index file path and ops dir was not on same branch
        (let* ((rtv-abbr "")
               (count 1)
               (rtv-back ""))
          (while (<= count (- io-re-count)) ;generate '../../ ..' format abbrev
            (setq rtv-abbr (concat rtv-abbr "../"))
            (setq count (+ 1 count)))
          (dolist (el io-re-path)     ;generate relative backend path
            (if (equal "" rtv-back)
                (setq rtv-back el)
              (setq rtv-back (concat rtv-back "/" el))))
          (setq io-string (concat rtv-abbr rtv-back))))
       ((equal t io-re-type)          ;cond that index file path and ops dir was on same branch
        (cond
         ((> io-re-count 0)
          (let ((rtv-back ""))
            (dolist (el io-re-path)
              (if (equal "" rtv-back)
                  (setq rtv-back el)
                (setq rtv-back (concat rtv-back "/" el))))
            (setq io-string rtv-back)))
         ((< io-re-count 0)
          (let ((rtv-abbr "")
                (count 1))
            (while (<= count (- io-re-count))
              (setq rtv-abbr (concat rtv-abbr "../"))
              (setq count (+ 1 count)))
            (setq io-string rtv-abbr)))
         ((= io-re-count 0)
          (setq io-string "")))))
      
      (if (not (equal "" io-string))  ;final tidy up io-string for inserting
          (setq io-string (concat io-string "/"))
        (setq io-string ""))

      ;; convert file to corresponding org file (may have link style)
      (let (insert-link-file-name
            insert-link-name
            insert-link-id)
        (setq insert-link-file-name (concat (file-name-sans-extension
                                             (file-name-nondirectory head-link)) ".org"))

        (if (string-match-p "#.*$" head-link)
            (progn
              (setq insert-link-id
                    (replace-regexp-in-string
                     ".*\\(#.*$\\)" "\\1" head-link))
              (setq insert-link-name (concat insert-link-file-name
                                             "::" insert-link-id)))
          (setq insert-link-name insert-link-file-name))
        
        (setq head-link (concat "file:" io-string insert-link-name)))

      (setq rtn (format "[[%s][%s]]" head-link head-name))
      rtn)))

(defun entropy/ep2o-convert-index-to-string (index ep2o-dir-struct)
  (let* ((number (car index))
         (title-list (nth 1 index))
         (level (entropy/ep2o-parse-index-asterisks number))
         (title (entropy/ep2o-parse-index-name title-list ep2o-dir-struct))
         (rtn nil))
    (setq rtn (concat level " " title))
    rtn))


(defun entropy/ep2o-write-index-file (ncx-file index-file ops-dir)
  (interactive)
  (let* ((navlist (entropy/ep2o-get-navlist ncx-file))
         (w-buffer (entropy/ep2o-get-index-file index-file))
         (tree (entropy/ep2o-get-nav-positive-tree navlist)))
    (with-current-buffer w-buffer
      (if buffer-read-only
          (read-only-mode 0))
      (goto-char (point-min))
      (dolist (el tree)
        (insert (concat (entropy/ep2o-convert-index-to-string
                         el
                         `(,(file-name-directory index-file)
                           ,ops-dir))
                        "\n")))
      (save-buffer)
      (kill-buffer))))

;;;; unzip epub file
(defun entropy/ep2o-extract-book (book-file)
  "Using unzip extract selected epub book file BOOK-FILE into the
plain-ascii named folder and return cons of 'convert dir name'
and 'raw dir name'."
  (interactive
   (list
    (completing-read "Choose book: " 'read-file-name-internal nil t)))
  (when (or (file-directory-p book-file)
            (not (string-match-p "\\.epub$" book-file)))
    (error (format "File %s is not epub file." book-file)))
  (let* ((dir (file-name-directory book-file))
         (book-temp (concat dir "ep2o-" (format-time-string "%Y%m%d%H%M%S") ".epub"))
         (conv-dir (concat dir (replace-regexp-in-string "\\.epub$" "" (file-name-nondirectory book-temp))))
         rtn)
    (copy-file book-file book-temp)
    (when (not (executable-find "unzip"))
      (error "Can not found \"unzip\" exec."))
    (let ((back-buffer (get-buffer-create "*entropy/ep2o-unzip*")))
      (message (format "Unzip book %s ...... " book-file))
      (call-process "unzip" nil back-buffer nil
                    (if (equal system-type 'windows-nt)
                        (replace-regexp-in-string "/" "\\" book-temp t t)
                      book-temp)
                    "-d"
                    conv-dir)
      (message (format "Unzip done!")))
    (delete-file book-temp)
    (setq rtn `(,(concat conv-dir "/") . ,(concat
                                           dir
                                           (file-name-base book-file))))
    rtn))

;;;; dispatchers
;; If epub dir structer was like:
;;
;; #+BEGIN_EXAMPLE
;;   `-epub
;;    |
;;    |--OPS
;;       |
;;       |-src-file1
;;       |-src-file2
;;       |- ......
;; #+END_EXAMPLE
;;
;; Then using auto-convert mode



(defun entropy/ep2o-normal-dir-structer (dir)
  "Judge whether epub book unzip dir DIR was constructed as the
normal epub dir struct, which formed as:

dir/
|--OPS
|  |--src-file1
|  |--src-file2
|  |-- .....
|  |-- *.ncx 
|--..
"
  (let ((sub-dir-list (entropy/cl-list-subdir dir))
        is-ops
        (rtn nil))
    (dolist (el sub-dir-list)
      (when (string-match-p "OPS" el)
        (setq is-ops t)))
    (if is-ops
        (let ((ops-list (entropy/cl-list-dir-lite (concat
                                                   (if (not (string-match-p "/$" dir))
                                                       (let ((dir-s (concat dir "/"))
                                                             dir))
                                                     dir)
                                                   "OPS")))
              src-p
              ncx-p)
          (dolist (el ops-list)
            (when (string-match-p entropy/ep2o-ops-srcfiles-regexp (cdr el))
              (setq src-p t))
            (when (string-match-p "\\.ncx$" (cdr el))
              (setq ncx-p t)))
          (if (and src-p ncx-p)
              (setq rtn t)
            (setq rtn nil)))
      (setq rtn nil))
    rtn))

;;;###autoload
(defun entropy/ep2o-dispatcher ()
  (interactive)
  (setq entropy/ep2o-org-after-indent-list nil)
  (let ((dispatcher-buffer (get-buffer-create "*entropy/ep2o-dispather*")))
    (switch-to-buffer dispatcher-buffer)
    (with-current-buffer dispatcher-buffer
      ;; head info
      (insert "This dispatcher buffer for viewing the process scheldue of convert book\n")
      (insert "=========================================================================\n")
      (when (yes-or-no-p "Beginning one transfer task ? ")
        (let* ((base-cons (call-interactively #'entropy/ep2o-extract-book))
               (base-dir (car base-cons))
               (recv-dir (cdr base-cons))
               (ops-dir nil)
               (ncx-file nil)
               (index-file (concat base-dir "index.org"))
               (org-files-list nil)
               (ep2o-dir-struct nil))

          (insert (format "Convert selected book '%s.epub' . \n\n" recv-dir))

          ;; judge ebook dir struct.
          (cond
           ;; normal ops dir structer
           ((entropy/ep2o-normal-dir-structer base-dir)
            (setq ops-dir    (concat base-dir "OPS/"))
            (dolist (el (entropy/cl-list-dir-lite ops-dir))
              (when (and (equal "F" (car el))
                         (string-match-p "\\.ncx$" (cdr el)))
                (if (equal system-type 'windows-nt)
                    (setq ncx-file (replace-regexp-in-string "/" "\\" (cdr el) t t))
                  (setq ncx-file (cdr el))))))

           ;; others epub dir structer type
           ((not (entropy/ep2o-normal-dir-structer base-dir))
            (setq ops-dir (let ((rtn nil)
                                (temp nil)
                                (is-ops nil))
                            (setq temp (read-directory-name "Choose Ops dir: " base-dir nil t))
                            (catch 'break
                              (dolist (el (entropy/cl-list-dir-lite temp))
                                (when (string-match-p entropy/ep2o-ops-srcfiles-regexp (cdr el))
                                  (setq is-ops t)
                                  (throw 'break nil))))
                            (if is-ops
                                (setq rtn temp)
                              (error (format "Dir %s was not OPS dir." temp)))
                            rtn)
                  ncx-file (let ((rtn nil)
                                 (temp nil))
                             (setq temp (read-file-name "Choose ncx file: " base-dir nil t))
                             (if (string-match-p "\\.ncx$" temp)
                                 (setq rtn temp)
                               (error (format "'%s' is not ncx file." temp)))
                             rtn))))

          ;; generate ep2o-dir-struct
          (setq ep2o-dir-struct `(,index-file ,ops-dir))

          ;; convert html to org files
          (insert "---------------\n")
          (insert "process step1: \n")
          (insert "Convert html files to org-files .....\n")
          (redisplay t)
          (setq org-files-list
                (entropy/ep2o-pandoc-html2org-and-return-orglist
                 (entropy/ep2o-list-html-src ops-dir)))
          (insert "File transfer done\n\n")

          ;; dos2unix org files
          (insert "---------------\n")
          (insert "process step2: \n")
          (insert "Dos2unix file end-line-type .....\n")
          (redisplay t)
          (entropy/ep2o-dos2unix-org-files org-files-list)
          (insert "End of line type transfer done!\n\n")
          
          ;; tidy up org files
          (insert "---------------\n")
          (insert "process step3: \n")
          (insert "Tidy up all org files .....\n")
          (redisplay t)
          (setq entropy/ep2o-index-relative nil)
          (if (yes-or-no-p "Indent all files? ")
              (entropy/ep2o-tidy-up-all-org-files org-files-list ep2o-dir-struct t)
            (entropy/ep2o-tidy-up-all-org-files org-files-list ep2o-dir-struct))
          (setq entropy/ep2o-index-relative nil)
          (insert "Tidy up done!\n\n")
          
          ;; write index file
          (insert "---------------\n")
          (insert "process step4: \n")
          (insert "Generating index file .....\n")
          (redisplay t)
          (entropy/ep2o-write-index-file ncx-file index-file ops-dir)
          (insert "Write index file done!\n\n")

          ;; move folder to raw named folder
          (insert "---------------\n")
          (insert "process step5: \n")
          (insert "Rename folder as raw folder .....\n")
          (redisplay t)
          (rename-file base-dir recv-dir t)

          (insert "\n\n\nConvert finished")))
      (when (yes-or-no-p "Kill entropy ep2o dispather buffer?")
        (kill-buffer dispatcher-buffer)))))
  


;;;; ops srcs adjusting function
(defun entropy/ep2o-src-adjusting-manually ()
  "Manully adjusting ops dir for missed regexp replacing."
  (interactive)
  (setq entropy/ep2o-org-after-indent-list nil)
  (let* ((ops-dir (read-directory-name "Choosing ops dir: " nil nil t))
         (ops-dir-list (entropy/cl-list-dir-lite ops-dir))
         ops-org-files
         (is-ops nil)
         (is-ops-org nil)
         (match-rexp "")
         (replace-str "")
         (re-list '())
         (indent-all (yes-or-no-p "Indent all org files? ")))
    (catch 'break
      (dolist (el ops-dir-list)
        (when (string-match-p entropy/ep2o-ops-srcfiles-regexp (cdr el))
          (setq is-ops t)
          (throw 'break nil))))
    (when is-ops
      (catch 'break
        (dolist (el ops-dir-list)
          (when (string-match-p "\\.org$" (cdr el))
            (setq is-ops-org t)
            (throw 'break nil)))))
    (if (not is-ops-org)
        (error (format "Dir %s is not converted ops dir." ops-dir)))

    (let ((final t))
      (while final
        ;; manully inputting matching regexp
        (setq match-rexp (read-string "Inputting regexp: "))

        ;; manully inputting replacing regexp 
        (setq replace-str (read-string (format "Replacing for \"%s\" :" match-rexp)))

        (add-to-list 're-list `(,match-rexp . ,replace-str))
        (setq final (yes-or-no-p "Further more?: "))))
    (setq re-list (reverse re-list))    ;setting re-replace operation positive rely on user input.

    ;; obtained converted org files list
    (dolist (el ops-dir-list)
      (when (and (string-match-p "\\.org" (cdr el))
                 (equal (car el) "F"))
        (add-to-list 'ops-org-files (cdr el))))

    ;; matched and replace user specifiction
    (if ops-org-files
        (progn
          (dolist (el ops-org-files)
            (with-current-buffer (find-file-noselect el)
              (let ((modified nil))
                (if buffer-read-only
                    (read-only-mode 0))
                (dolist (el re-list)
                  (goto-char (point-min))
                  (while (re-search-forward (car el) nil t)
                    (replace-match (cdr el))
                    (when (not modified)
                      (setq modified t))))
                (when modified
                  (entropy/unfill-full-buffer-without-special-region)
                  (read-only-mode 0)
                  (if (not indent-all)
                      (when (entropy/ep2o-tidy-org-indentation-warning 'entropy/ep2o-org-after-indent-list)
                        (org-indent-region (point-min) (point-max))
                        (entropy/fill-full-buffer-without-special-region)
                        (read-only-mode 0))
                    (progn
                      (org-indent-region (point-min) (point-max))
                      (entropy/fill-full-buffer-without-special-region)
                      (read-only-mode 0)))
                  ;; fill foot note
                  (progn
                    (text-mode)
                    (goto-char (point-min))
                    (while (re-search-forward "^\\[fn:" nil t)
                      (fill-paragraph)))
                  (save-buffer))
                (kill-buffer))))
          (message "Processing successfully!"))
      (error "OPS dir wrong!"))))


;;; provide
(provide 'entropy-epub2org)
