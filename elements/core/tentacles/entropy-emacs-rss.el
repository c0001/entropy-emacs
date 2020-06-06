;;; entropy-emacs-rss.el --- Emacs Rss configuration
;;
;; * Copyright (C) 20190907  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-rss.el
;; Keywords:      rss, elfeed
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
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
;; Emacs rss client configuration for =entropy-emacs=.
;;
;; * Configuration:
;;
;; Using for =entropy-emacs= only.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-hydra-hollow)

;; ** elfeed feed
(use-package elfeed
  :commands (elfeed)
  :bind (:map
         elfeed-show-mode-map
         ("q" . entropy/emacs-rss-elfeed-kill-buffer))

  :eemacs-tpha
  (((:enable t))
   ("Rss"
    (("r e" elfeed "Read Rss By Elfeed"
      :enable t
      :exit t))))

  :eemacs-mmphc
  (((:enable t)
    (elfeed-search-mode (elfeed elfeed-search-mode-map) t))
   ("Feed"
    (("A" elfeed-add-feed "Add feed" :enable t :exit t)
     ("D" entropy/emacs-rss-elfeed-remove-feed "Delete feed commonly" :enable t :exit t)
     ("M-d" entropy/emacs-rss-elfeed-remove-feed-by-regexp "Delete feed by regexp"
      :enable t :exit t))
    "Filter"
    (("s" entropy/emacs-rss-elfeed-clean-filter "Clean current patched filter"
      :enable t :exit t)
     ("t" entropy/emacs-rss-elfeed-filter-by-tag "Filter feeds by tag"
      :enable t :exit t)
     ("f" entropy/emacs-rss-elfeed-filter-by-feedname "Filter feeds by name"
      :enable t :exit t)
     ("m" elfeed-search-set-filter "Filter feeds by hand" :enable t :exit t))
    "Update"
    (("u" entropy/emacs-rss-elfeed-multi-update-feeds "Update multiple feeds"
      :enable t :exit t)
     ("U" elfeed-search-fetch "Update all feeds" :enable t :exit t)
     ("g" entropy/emacs-rss-elfeed-format-feed-title "Refresh all feeds" :enable t :exit t)
     ("M-u" entropy/emacs-rss-elfeed-update-proxy "Update multiple feeds by proxy"
      :enable t :exit t)
     ("M-U" entropy/emacs-rss-elfeed-proxy-update-all-nil-feeds "Update all failed feeds with proxy"
      :enable t :exit t)
     ("M-r" entropy/emacs-rss-elfeed-update-proxyfeeds-regexp-match "Update feeds by regexp match with proxy"
      :enable t :exit t))
    "Entry"
    (("d" entropy/emacs-rss-elfeed-delete-entry "Delete current entry" :enable t :exit t)
     ("+" entropy/emacs-rss-elfeed-tag-selected "Add tag for current entry" :enable t :exit t)
     ("-" entropy/emacs-rss-elfeed-untag-selected "Delete tag for current entry" :enable t :exit t))))

  :init
  (setq elfeed-search-date-format '("%Y/%m/%d-%H:%M" 16 :left))
  (setq elfeed-curl-timeout 20)

  ;; set curl path
  (let ((mingw-curl (if (and entropy/emacs-wsl-enable
                             (file-exists-p entropy/emacs-wsl-apps))
                        (expand-file-name
                         "mingw64/bin/curl.exe"
                         (substring (directory-file-name entropy/emacs-wsl-apps) 0 -7))
                      nil))
        (msys2-curl (if (and entropy/emacs-wsl-enable
                             (file-exists-p entropy/emacs-wsl-apps))
                        (expand-file-name
                         "curl.exe" entropy/emacs-wsl-apps)))
        (w32-curl "c:/WINDOWS/system32/curl.exe")
        (unix-curl "curl"))
    (cond
     ((ignore-errors (file-exists-p w32-curl))
      (setq elfeed-curl-program-name w32-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p mingw-curl))
      (setq elfeed-curl-program-name mingw-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p msys2-curl))
      (setq elfeed-curl-program-name msys2-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (and (executable-find "curl")
                          sys/is-posix-compatible))
      (setq elfeed-use-curl t))
     (t
      (setq elfeed-use-curl nil))))


  :config
;; *** core advice

;; *** utilities
  (defun entropy/emacs-rss--elfeed-list-feeds ()
    "List feeds using for querying promt."
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          (rtn nil))
      (let ((mlist feeds))
        (dolist (el feeds)
          (if (not (member (elfeed-feed-id el) elfeed-feeds))
              (setq mlist (delete el mlist))))
        (setq feeds mlist))
      (let ((pair-list nil))
        (dolist (el feeds)
          (setq pair-list `(,(elfeed-feed-title el) . ,(elfeed-feed-id el)))
          (push pair-list rtn)))
      (let ((mlist nil))
        (dolist (el rtn)
          (if (car el)
              (push `(,(concat (car el) " ↭ " (cdr el)) . ,(cdr el)) mlist)
            (push `(,(concat "☹nil ↭ " (cdr el)) . ,(cdr el)) mlist)))
        (setq rtn mlist))
      rtn))

  (defun entropy/emacs-rss-elfeed-clean-invalid-feeds ()
    "Clean invalid feeds which can not be retrieved.

This function will remove 'as entry' both in `elfeed-db' and
`elfeed-feeds' which also will automatically stored in
`custom-file'."
    (interactive)
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          mlist)
      (dolist (el feeds)
        (when (and (not (elfeed-feed-url el))
                   (member (elfeed-feed-id el) elfeed-feeds))
          (push `(,(elfeed-feed-id el) . ,el) mlist)))
      (dolist (el mlist)
        (setq elfeed-feeds (delete (car el) elfeed-feeds)))
      (customize-save-variable 'elfeed-feeds elfeed-feeds)
      (elfeed-db-gc-empty-feeds)))

  (defvar entropy/emacs-rss--elfeed-feed-prompt-alist '()
    "Alist of feeds prompt string and url.")


  (defun entropy/emacs-rss-elfeed-kill-buffer ()
    "Automatically swtitch to elfeed-search buffer when closed
the current elfeed-show-buffer."
    (interactive)
    (if (not (equal major-mode 'elfeed-show-mode))
        (kill-buffer (current-buffer))
      (progn
        (kill-buffer (current-buffer))
        (let* ((bfl (mapcar #'(lambda (x) (buffer-name x))
                            (buffer-list))))
          (if (member "*elfeed-search*" bfl)
              (progn (switch-to-buffer "*elfeed-search*" nil t)
                     (message "Back to *elfeed-search* buffer."))
            (user-error "Couldn't found *elfeed-search* buffer."))))))


  (defun entropy/emacs-rss-elfeed-clean-filter ()
    "Clean all filter for curren elfeed search buffer."
    (interactive)
    (elfeed-search-set-filter ""))

  (defun entropy/emacs-rss--elfeed-sc-str (str)
    "Replaces space for '-' when string STR indeed of that."
    (let ((strlist (split-string str " "))
          rtn)
      (if (member "" strlist)
          (setq strlist (delete "" strlist)))
      (dolist (el strlist)
        (if (not rtn)
            (setq rtn el)
          (setq rtn (concat rtn "-" el))))
      rtn))

;; *** feeds-title config

  (eval-when-compile
    (defun entropy/emacs-rss--elfeed-string-style-hook (&rest args)
      "Hooks for replace space to '-' when save `elfeed-db'."
      (let ((feeds (if (hash-table-p elfeed-db-feeds)
                       (hash-table-values elfeed-db-feeds)
                     nil))
            did)
        (when feeds
          (dolist (el feeds)
            (let ((feed-title (elfeed-feed-title el))
                  newtitle)
              (when (and feed-title (string-match-p " " feed-title))
                (setq newtitle (entropy/emacs-rss--elfeed-sc-str feed-title))
                (setf (elfeed-feed-title el) newtitle)
                (setq did t))))
          (if did
              t
            nil)))))

  (advice-add 'elfeed-db-load :after #'entropy/emacs-rss--elfeed-string-style-hook)


  (defun entropy/emacs-rss-elfeed-format-feed-title ()
    "Interactively format feedtitle which has space."
    (interactive)
    (if (equal major-mode 'elfeed-search-mode)
        (progn
          (entropy/emacs-rss--elfeed-string-style-hook)
          (elfeed-search-update--force))
      (error "You are not in the 'elfeed-search-mode'!")))

;; *** query prompting filter function
  (defun entropy/emacs-rss--elfeed-get-all-entries ()
    "Get all entries from `elfeed-db' and return it."
    (hash-table-values (plist-get elfeed-db :entries)))

  (defun entropy/emacs-rss--elfeed-read-feedname (entry)
    "Get entry's feed name."
    (let* ((feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed)))))
      (if feed-title
          feed-title)))

  (defun entropy/emacs-rss--elfeed-tags-choice (selected-entries &optional tag return-list prompt match)
    "Query and selected tag from the list collected by
SELECTED-ENTRIES. And final return the tag string or the list of
matched entires by this tag.

Arguemnts:

- tag:         if specific it as a string, skip the step of query prompt,
               and using the value as the choice.

- return-list: if non-nil ,then this function return the matched
               entries list, otherwise return tag string

- prompt:      string for specific the query prompt.

- match:       whether require match with query prompt step."
    (if (equal major-mode 'elfeed-search-mode)
        (let (rtn entries tags-list choice)
          ;; extract entries by selected which have existed tags and make initial tags-list list.
          ;; [2018-08-30 Thu 02:32:49] this can be replaced with `elfeed-db-get-all-tags' for full
          ;; tags search.

          (let ((entl selected-entries))
            (dolist (el entl)
              (when (listp (elfeed-entry-tags el))
                (push el entries)
                (mapcar #'(lambda (x)
                            (add-to-list 'tags-list x))
                        (elfeed-entry-tags el)))))
          ;; read user choice
          (if (not tag)
              (setq choice (ivy-read (if (not prompt)
                                         "Choose tag: "
                                       prompt)
                                     tags-list
                                     :require-match (if match t nil)))
            (setq choice tag))

          (when (string-match-p " " choice)
            (setq choice (entropy/emacs-rss--elfeed-sc-str choice)))

          (if return-list
              ;; match entries of choice
              (dolist (el entries)
                (if (member (intern choice) (elfeed-entry-tags el))
                    (push el rtn)))
            (setq rtn (intern choice)))
          rtn)
      (error "Unmatched major-mode of 'elfeed-search-mode'!")))

  (defun entropy/emacs-rss--elfeed-feedname-choice (selected-entries &optional tname return-list)
    "Query with promt for choosing one feedname from the selected
entries, return the choice or one entries list wich matched this
choice.

Arguments:

- tname:       specific feedname without manully choosing one from query prompting.

- return-list: if non-nil then return the matched entries list
               instead of returning the feedname."
    (let ((entry-list selected-entries)
          feedtitle-name-list
          entries
          choice
          rtn)
      ;; make feed title name list and matched entries list
      (dolist (el entry-list)
        (let ((feed-title (entropy/emacs-rss--elfeed-read-feedname el)))
          (when feed-title
            (add-to-list 'feedtitle-name-list feed-title)
            (push el entries))))

      (if tname
          (setq choice tname)
        (setq choice (ivy-read "Choose feed title: " feedtitle-name-list)))

      (when (string-match-p " " choice)
        (setq choice (entropy/emacs-rss--elfeed-sc-str choice)))

      (if return-list
          (dolist (el entries)
            (if (equal choice (entropy/emacs-rss--elfeed-read-feedname el))
                (push el rtn)))
        (setq rtn choice))
      rtn))

  (defun entropy/emacs-rss-elfeed-filter-by-tag (tag)
    "Filter with tag choosen, powered by `entropy/emacs-rss--elfeed-tags-choice'"
    (interactive
     (list (entropy/emacs-rss--elfeed-tags-choice (entropy/emacs-rss--elfeed-get-all-entries))))
    (elfeed-search-set-filter (concat "+" (symbol-name tag))))

  (defun entropy/emacs-rss-elfeed-filter-by-feedname (feedname)
    "Filter with feedname, powered by `entropy/emacs-rss--elfeed-feedname-choice'."
    (interactive
     (list (entropy/emacs-rss--elfeed-feedname-choice (entropy/emacs-rss--elfeed-get-all-entries))))
    (if (entropy/emacs-rss--elfeed-string-style-hook)
        (elfeed-search-update--force))
    (elfeed-search-set-filter (concat "=" feedname)))

  (defun entropy/emacs-rss-elfeed-untag-selected ()
    "Untag tag for selected entries with query prompt as
requiring matched."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (choice (entropy/emacs-rss--elfeed-tags-choice entries nil nil "Choose tag for remove: " t)))
      (elfeed-untag entries choice)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun entropy/emacs-rss-elfeed-tag-selected ()
    "Adding tag for selected entries with query prompt for
selecting existing tag or input the new one instead."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (full-entrylist (entropy/emacs-rss--elfeed-get-all-entries))
           (tag (entropy/emacs-rss--elfeed-tags-choice full-entrylist nil nil "Choose tag or input one: ")))
      (elfeed-tag entries tag)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

;; *** support multibyte entry
  (defun entropy/emacs-rss-elfeed-add-feed-around (oldfunc url)
    "Addding url and hexify it when it contained multi-byte
string as CJK characters."
    (interactive (list
                  (read-string "Url: ")))
    (require 'entropy-common-library-const)
    (let ((hexi-url (url-hexify-string url entropy/cl-url--allowed-chars)))
      (funcall oldfunc hexi-url :save t)))
  (advice-add 'elfeed-add-feed :around #'entropy/emacs-rss-elfeed-add-feed-around)


;; *** delete entry
  (defun entropy/emacs-rss-elfeed-delete-entry ()
    "Delete entry of elfeed."
    (interactive)
    (if (not (use-region-p))
        (let* ((entry (elfeed-search-selected t))
               (id  (elfeed-entry-id entry))
               (n-entry elfeed-db-entries))
          (avl-tree-delete elfeed-db-index id)
          (remhash id n-entry)
          (setq elfeed-db-entries n-entry)
          (entropy/emacs-rss-elfeed-format-feed-title))
      (let* ((entries (elfeed-search-selected))
             id
             (n-entry elfeed-db-entries))
        (dolist (el entries)
          (add-to-list 'id (elfeed-entry-id el)))
        (dolist (el id)
          (avl-tree-delete elfeed-db-index el))
        (dolist (el id)
          (remhash el n-entry))
        (setq elfeed-db-entries n-entry)
        (entropy/emacs-rss-elfeed-format-feed-title))))

  (define-key elfeed-search-mode-map (kbd "d") 'entropy/emacs-rss-elfeed-delete-entry)

;; *** remove feed function
  (defvar entropy/emacs-rss--elfeed-feed-remove-list '()
    "List stored feeds url of `elfeed-feeds' to remove.")

  (defun entropy/emacs-rss--elfeed-feed-of-url (url)
    "Matching url's refer feed title name."
    (let* ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
           (rtn nil))
      (dolist (el feeds)
        (if (equal url (elfeed-feed-url el))
            (setq rtn (elfeed-feed-title el))))
      (if rtn
          rtn
        "-v-")))

  (defun entropy/emacs-rss--elfeed-remove-read-action (x)
    "Repeatedly read action for removing feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (let ((temp (cdr x)))
      (setq x temp))
    (entropy/cl-ivy-read-repeatedly-function
     x 'entropy/emacs-rss--elfeed-feed-remove-list
     "Removing:"
     #'entropy/emacs-rss--elfeed-feed-of-url))

  (defun entropy/emacs-rss-elfeed-remove-feed ()
    "Remove elfeed feeds with multi-chosen by query prompted
function of `ivy-read'."
    (interactive)
    (setq entropy/emacs-rss--elfeed-feed-remove-list nil
          entropy/emacs-rss--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-rss--elfeed-feed-prompt-alist (entropy/emacs-rss--elfeed-list-feeds))
    (ivy-read "Remove feeds:" entropy/emacs-rss--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-rss--elfeed-remove-read-action)
    (let ((rtn elfeed-feeds))
      (dolist (el entropy/emacs-rss--elfeed-feed-remove-list)
        (when (member el elfeed-feeds)
          (setq rtn (delete el rtn))))
      (customize-save-variable 'elfeed-feeds rtn)
      (when (yes-or-no-p "Do you want to remove all empty feeds? ")
        (elfeed-db-gc-empty-feeds))))

  (defun entropy/emacs-rss-elfeed-remove-feed-by-regexp ()
    "Remove feeds matched of regexp expr inputted repeatlly."
    (interactive)
    (let* ((regexp (entropy/cl-repeated-read "Input regexp"))
           (feeds (elfeed-feed-list))
           mlist
           (rtn feeds))
      (dolist (el regexp)
        (dolist (elm feeds)
          (when (string-match-p el elm)
            (add-to-list 'mlist elm))))
      (dolist (el mlist)
        (setq rtn (delete el rtn)))
      (setq elfeed-feeds rtn)
      (customize-save-variable 'elfeed-feeds elfeed-feeds)))

;; *** update specific feed through proxy

  (defvar entropy/emacs-rss--elfeed-backup-of-orig-urlproxy nil)

  (defun entropy/emacs-rss--elfeed-update-read-action (x)
    "Repeatly read action for updating feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (let ((temp (cdr x)))
      (setq x temp))
    (entropy/cl-ivy-read-repeatedly-function
     x 'entropy/emacs-elfeed-multi-update-feeds-list
     "Updating: "
     #'entropy/emacs-rss--elfeed-feed-of-url))

  (defun entropy/emacs-rss-elfeed-get-multi-update-feeds ()
    "Getting feeds needed for updating through querying with
promptings and injecting them into `entropy/emacs-elfeed-multi-update-feeds-list'."
    (interactive)
    (setq entropy/emacs-elfeed-multi-update-feeds-list nil
          entropy/emacs-rss--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-rss--elfeed-feed-prompt-alist (entropy/emacs-rss--elfeed-list-feeds))
    (ivy-read "Update feeds: " entropy/emacs-rss--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-rss--elfeed-update-read-action))

  (defun entropy/emacs-rss-elfeed-multi-update-feeds ()
    "Update feeds interactively by multiplied choicing from `entropy/emacs-rss--elfeed-feed-prompt-alist'."
    (interactive)
    (setq entropy/emacs-elfeed-multi-update-feeds-list nil
          entropy/emacs-rss--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-rss--elfeed-feed-prompt-alist (entropy/emacs-rss--elfeed-list-feeds))
    (ivy-read "Update feeds: " entropy/emacs-rss--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-rss--elfeed-update-read-action)
    (dolist (el entropy/emacs-elfeed-multi-update-feeds-list)
      (elfeed-update-feed el)))

  (defun entropy/emacs-rss--elfeed-update-curl-proxy (url-lists proxy)
    (when elfeed-curl-extra-arguments
      (let (mlist (olist elfeed-curl-extra-arguments))
        (dolist (el olist)
          (when (string-match-p "-x.*://" el)
            (setq elfeed-curl-extra-arguments (delete el elfeed-curl-extra-arguments))))))
    (setq elfeed-curl-extra-arguments (append elfeed-curl-extra-arguments
                                              (list (concat "-x" proxy))))
    (dolist (el url-lists)
      (elfeed-update-feed el)))


  (defvar entropy/emacs-rss--elfeed-update-urlretrieve-proxy-timer)
  (defun entropy/emacs-rss--elfeed-update-urlretrieve-proxy-reset-proxy-request ()
    (when (and (timerp entropy/emacs-rss--elfeed-update-urlretrieve-proxy-timer)
               (null url-queue))
      (progn (cancel-timer entropy/emacs-rss--elfeed-update-urlretrieve-proxy-timer)
             (setq entropy/emacs-rss--elfeed-update-urlretrieve-proxy-timer nil
                   entropy/emacs-rss--elfeed-proxy-for-emacs-url nil
                   url-proxy-services entropy/emacs-rss--elfeed-backup-of-orig-urlproxy))))

  (defun entropy/emacs-rss--elfeed-update-urlretrieve-proxy (url-lists)
    (unless (< (length url-lists) 5)
      (error "Too much feeds selected, it will cause lagging, reducing them under 5."))
    (let ((elfeed-use-curl nil)
          (copy-proxy (copy-tree url-proxy-services)))
      (setq entropy/emacs-rss--elfeed-backup-of-orig-urlproxy
            copy-proxy)
      (setq url-proxy-services
            (list (cons "http" entropy/emacs-elfeed-retrieve-proxy)
                  (cons "https" entropy/emacs-elfeed-retrieve-proxy)
                  (cons "ftp" entropy/emacs-elfeed-retrieve-proxy)
                  (cons "no_proxy" (concat
                                    "^\\("
                                    (mapconcat
                                     'identity entropy/emacs-elfeed-url-no-proxy
                                     "\\|") "\\)"))))
      (dolist (el url-lists)
        (elfeed-update-feed el))
      (setq entropy/emacs-rss--elfeed-update-urlretrieve-proxy-timer
            (run-with-idle-timer
             0.1 t
             #'entropy/emacs-rss--elfeed-update-urlretrieve-proxy-reset-proxy-request))))


  (defun entropy/emacs-rss-elfeed-update-proxy (&optional url-lists )
    "Update feeds using proxy."
    (interactive)
    (let ((ulist (if url-lists url-lists (progn (entropy/emacs-rss-elfeed-get-multi-update-feeds)
                                                entropy/emacs-elfeed-multi-update-feeds-list))))
      (if elfeed-use-curl
          (let ((proxy (if (not (string-match-p "^http://" entropy/emacs-elfeed-retrieve-proxy))
                           (concat "http://" entropy/emacs-elfeed-retrieve-proxy)
                         entropy/emacs-elfeed-retrieve-proxy)))
            (entropy/emacs-rss--elfeed-update-curl-proxy ulist proxy))
        (entropy/emacs-rss--elfeed-update-urlretrieve-proxy ulist))))


  (defun entropy/emacs-rss--elfeed-reset-curl-arg ()
    (let ((judge nil)
          (mlist nil))
      (when elfeed-curl-extra-arguments
        (dolist (el elfeed-curl-extra-arguments)
          (when (string-match-p "-x.*://" el)
            (setq judge t)
            (push el mlist)))
        (when judge
          (cond
           ((= 1 (length elfeed-curl-extra-arguments))
            (setq elfeed-curl-extra-arguments nil))
           (t
            (dolist (el mlist)
              (setq elfeed-curl-extra-arguments
                    (delete el elfeed-curl-extra-arguments)))))))))

  (advice-add 'elfeed-update :before #'entropy/emacs-rss--elfeed-reset-curl-arg)

  (defun entropy/emacs-rss-elfeed-proxy-update-all-nil-feeds ()
    "Update all empty feeds with proxy by `entropy/emacs-rss-elfeed-update-proxy'."
    (interactive)
    (let ((olist (entropy/emacs-rss--elfeed-list-feeds))
          mlist rlist)
      (dolist (el olist)
        (when (string-match-p "☹nil" (car el))
          (push (cdr el) mlist)))
      (if mlist
          (entropy/emacs-rss-elfeed-update-proxy mlist)
        (message "None feeds need for updating with proxy."))))


  (defun entropy/emacs-rss-elfeed-update-proxyfeeds-regexp-match ()
    "Update feeds through proxy matched by regexp stored in
`entropy/emacs-elfeed-proxyfeeds-regexp-list'.

If hasn't setting the regexp list, prompting for input them
repeatly and stored them in `cutom-file'."
    (interactive)
    (require 'entropy-common-library)
    (let ((olist (elfeed-feed-list))
          (relist entropy/emacs-elfeed-proxyfeeds-regexp-list)
          ulist)
      (unless relist
        (setq relist (entropy/cl-repeated-read "Inputting regexp"))
        (setq entropy/emacs-elfeed-proxyfeeds-regexp-list relist)
        (customize-save-variable 'entropy/emacs-elfeed-proxyfeeds-regexp-list relist))
      (when (yes-or-no-p "Do you want to adding some matching? ")
        (setq relist (append relist (entropy/cl-repeated-read "Adding regexp")))
        (setq entropy/emacs-elfeed-proxyfeeds-regexp-list relist)
        (customize-save-variable 'entropy/emacs-elfeed-proxyfeeds-regexp-list relist))
      (dolist (el olist)
        (dolist (elm relist)
          (when (string-match-p elm el)
            (push el ulist))))
      (when ulist
        (entropy/emacs-rss-elfeed-update-proxy ulist))))

;; *** default external browser for feed viewing

  (defun elfeed-search-browse-url (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'.

Note: this function has been modified by entropy-emacs for fixing
one bug of which:

Original func coding logic leaving entry update part after opening
url, this will not redraw the \"*elfeed-search*\" successfully
because that all its behavior are not witin the
\"*elfeed-search*\" buffer as it be on w3m or eww buffer when using
internal `browse-url-browser-function'.

The minor changing was compat for above."
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               do (elfeed-search-update-entry entry)
               when (elfeed-entry-link entry)
               do (if use-generic-p
                      (browse-url-generic it)
                    (browse-url it)))
      (unless (use-region-p) (forward-line))))


  (defun entropy/emacs-rss--elfeed-browse-url-around (oldfun &rest args)
    "Browse feed source by external browser with choosing."
    (let* ((choice
            (completing-read "Choose browse: " `("default"
                                                 "eww"
                                                 ,(if entropy/emacs-browse-url-function "entropy-browse-url-function" "")
                                                 ,(if (executable-find "w3m") "emacs-w3m" ""))
                             nil t))
           (browse-url-browser-function (cl-case (intern choice)
                                          ('default 'browse-url-default-browser)
                                          ('eww 'eww-browse-url)
                                          ('emacs-w3m 'entropy/emacs-textwww--w3m-browse-url)
                                          ('entropy-browse-url-function entropy/emacs-browse-url-function))))
      (funcall oldfun)))
  (advice-add 'elfeed-search-browse-url :around #'entropy/emacs-rss--elfeed-browse-url-around)
  (advice-add 'elfeed-show-visit :around #'entropy/emacs-rss--elfeed-browse-url-around)

;; *** elfeed print entry function
  (defun entropy/emacs-rss--elfeed-search-print-entry--default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) "\t")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (setq elfeed-search-print-entry-function 'entropy/emacs-rss--elfeed-search-print-entry--default)

  )

;; ** newsticker

(use-package newsticker
  :ensure nil
  :eemacs-tpha
  (((:enable t))
   ("Rss"
    (("r a" newsticker-show-news
      "Read emacs about news"
      :enable t :exit t))))
  :init
  (setq newsticker-url-list
        '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
          ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
          ("Oremacs" "https://oremacs.com/atom.xml")
          ("EmacsCast" "https://pinecast.com/feed/emacscast")
          ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss"))))


;; * provide
(provide 'entropy-emacs-rss)
