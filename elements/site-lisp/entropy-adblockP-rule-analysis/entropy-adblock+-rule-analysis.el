;;; entropy-adblock+-rule-analysis.el --- elisp extension to analyze adblock+ url rule set
;;
;;; Copyright (C) 20190530  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-adblockP-rule-analysis
;; Package-Version: v0.1.0
;; Package-Requires: ((emacs "25") (url) (memoize "1.1"))
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

;; This package gives the ability to transfer adblock-plus simple url
;; rule-set to the elisp regexp string foramt.

;; You can see all the adblock-plus rule set document [[https://adblockplus.org/en/filters][online]].

;; For more, the transforming used to for simple pac(proxy auto
;; configuration) used by [[https://github.com/FelisCatus/SwitchyOmega][Switchy-Omega]], which rely on the rule-list
;; host on [[https://github.com/gfwlist/gfwlist][GFWLIST]].

;; This package was the apis orientated designation, the rules get func
;; ~entropy/adbp-rule-get-regexp-matchs-list~ and url blacklist check
;; func ~entropy/adbp-rule-blacklist-match-url-p~ are the mainly
;; provision.

;;; Configuration:

;;; Changelog:

;; - [2020-11-25 Wed 06:30:18] Update rulesets

;;   * Add `entropy/adbp-rule-update' interaction feature.

;; - [2020-03-11 Wed 10:50:19] Version 0.1.0 release out

;;; Code:
;;;; require
(require 'url)
(require 'memoize)

;;;; variable declaration
;;;;; customized variables
(defgroup entropy/adbp-rule-group nil
  "Group variables classified identifier for
`entropy-adblock+-rule-analysis'"
  :group 'extensions)

(defcustom entropy/adbp-rule-use-upstream-rule-list nil
  "Wether to use updated upstream gfw-list.

Defautl value was nil indicated to use local cached rule-sets,
this cared for reducing some network retrieving error occurs
during emacs startup (e.g. the unobstructed net visiting ISP
attack or gnutls error in emacs batch mode)."
  :type 'boolean
  :group 'entropy/adbp-rule-group)

(defcustom entropy/adbp-custom-rule-sets-blacklist nil
  "User customized list of url regexp added to blacklist."
  :type '(repeat
          (regexp :tag "Regexp for a url added to blacklist "))
  :group 'entropy/adbp-rule-group)

(defcustom entropy/adbp-custom-rule-sets-whitelist nil
  "User customized list of url regexp added to whitelist."
  :type '(repeat
          (const :tag "Regexp for a url added to whitelist " regexp))
  :group 'entropy/adbp-rule-group)

;;;;; internal variables
(defvar entropy/adbp-rule--gfw-list-upstream
  "https://raw.githubusercontent.com/gfwlist/gfwlist/master/gfwlist.txt")
(defvar entropy/adbp-rule--gfw-list-local
  (expand-file-name "gfw-list.txt"
                    (file-name-directory load-file-name)))

(defvar entropy/adbp-rule--upstream-gfw-list-encrypted-cache nil)
(defvar entropy/adbp-rule--origin-rule-set nil)
(defvar entropy/adbp-rule--blacklist-regexps-cache nil)
(defvar entropy/adbp-rule--whitelist-regexps-cache nil)

(defvar entropy/adbp-rule--head-full-blacklist-regexp
  '("^||\\([^|].*\\)$" . "^\\\\(https?://\\\\(www\\\\.\\\\)?\\\\|ftp://\\\\)?\\1"))
(defvar entropy/adbp-rule--head-blacklist-regexp '("^|\\([^|].*\\)$" . "^\\1"))
(defvar entropy/adbp-rule--head-wild-blacklist-regexp '("^\\.\\([^\\.].*\\)$" . ".*\\1"))

(defvar entropy/adbp-rule--head-full-whitelist-regexp
  '("^@@||\\([^|].*\\)$" . "^\\\\(https?://\\\\(www\\\\.\\\\)?\\\\|ftp://\\\\)?\\1"))
(defvar entropy/adbp-rule--head-whitelist-regexp '("^@@|\\([^|].*\\)$" . "^\\1"))
(defvar entropy/adbp-rule--head-wild-whitelist-regexp '("^@@\\.\\([^\\.].*\\)$" . ".*\\1"))

;;;; libraries
(defun entropy/adbp-rule--replace-origin-rule-entry (rule-entry $rule-defination)
  (replace-regexp-in-string (car $rule-defination)
                            (cdr $rule-defination)
                            rule-entry))

(defun entropy/adbp-rule--extract-buffer-rule-entries ($rule-defination storehouse)
  (let (register)
    (goto-char (point-min))
    (while (re-search-forward (car $rule-defination) nil t)
      (setq register (match-string-no-properties 0))
      (add-to-list storehouse
                   (entropy/adbp-rule--replace-origin-rule-entry
                    register $rule-defination)))))

(defun entropy/adbp-rule--base64-decrypt (base64-code)
  (let (rtn)
    (setq rtn
          (ignore-errors
            (base64-decode-string base64-code)))
    (when (not rtn)
      (when entropy/adbp-rule-use-upstream-rule-list
        (message "Can not retrieve gfw-list on '%s'\nUsing local cached version."
                 entropy/adbp-rule--gfw-list-upstream))
      (with-temp-buffer
        (when buffer-read-only
          (read-only-mode 0))
        (insert-file-contents entropy/adbp-rule--gfw-list-local)
        (setq rtn
              (base64-decode-string
               (buffer-substring-no-properties
                (point-min) (point-max))))))
    rtn))

(defun entropy/adbp-rule--fetch-gfw-list ()
  (let ((retrieve-buffer
         (when entropy/adbp-rule-use-upstream-rule-list
           (ignore-errors
             (url-retrieve-synchronously
              entropy/adbp-rule--gfw-list-upstream)))))
    (if retrieve-buffer
        (with-current-buffer retrieve-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-line 1)
          (let ((body (cons (point) (point-max))))
            (setq entropy/adbp-rule--upstream-gfw-list-encrypted-cache
                  (buffer-substring-no-properties
                   (car body)
                   (cdr body)))))
      nil)))

(defun entropy/adbp-rule--gen-adbp-origin-rule-set ()
  (let ((cache (entropy/adbp-rule--fetch-gfw-list)))
    (setq entropy/adbp-rule--origin-rule-set
          (entropy/adbp-rule--base64-decrypt cache))))

(defun entropy/adbp-rule--extract-regexp-rules ()
  (let ((origin-rule-set (entropy/adbp-rule--gen-adbp-origin-rule-set))
        (blacklist-rule-defination-list
         (list entropy/adbp-rule--head-blacklist-regexp
               entropy/adbp-rule--head-full-blacklist-regexp
               entropy/adbp-rule--head-wild-blacklist-regexp))
        (whitelist-rule-defination-list
         (list entropy/adbp-rule--head-whitelist-regexp
               entropy/adbp-rule--head-full-whitelist-regexp
               entropy/adbp-rule--head-wild-whitelist-regexp)))
    (setq entropy/adbp-rule--blacklist-regexps-cache nil)
    (with-temp-buffer
      (when buffer-read-only
        (read-only-mode 0))
      (insert origin-rule-set)
      (dolist (el blacklist-rule-defination-list)
        (entropy/adbp-rule--extract-buffer-rule-entries
         el 'entropy/adbp-rule--blacklist-regexps-cache))
      (dolist (regexp entropy/adbp-custom-rule-sets-blacklist)
        (add-to-list 'entropy/adbp-rule--blacklist-regexps-cache
                     regexp))
      (dolist (el whitelist-rule-defination-list)
        (entropy/adbp-rule--extract-buffer-rule-entries
         el 'entropy/adbp-rule--whitelist-regexps-cache))
      (dolist (regexp entropy/adbp-custom-rule-sets-whitelist)
        (add-to-list 'entropy/adbp-rule--whitelist-regexps-cache
                     regexp)))))

;;;; main

;;;###autoload
(defun entropy/adbp-rule-get-regexp-matchs-list (&optional inct)
  (entropy/adbp-rule--extract-regexp-rules)
  (cond
   ((null inct)
    (let (rtn-blacklist rtn-whitelist)
      (dolist (el entropy/adbp-rule--blacklist-regexps-cache)
        (add-to-list 'rtn-blacklist (list el)))
      (dolist (el entropy/adbp-rule--whitelist-regexps-cache)
        (add-to-list 'rtn-whitelist (list el)))
      (list :blacklist rtn-blacklist
            :whitelist rtn-whitelist)))
   (inct
    (let ((buffer (get-buffer-create "*entropy/adbp-show-rules*")))
      (with-current-buffer buffer
        (when buffer-read-only
          (read-only-mode 0))
        (erase-buffer)
        (goto-char (point-min))
        (insert "* =====blacklist=====\n")
        (dolist (el entropy/adbp-rule--blacklist-regexps-cache)
          (insert (concat "\"" el "\"" "\n")))
        (insert "\n\n")
        (insert "* =====whitelist=====\n")
        (dolist (el entropy/adbp-rule--whitelist-regexps-cache)
          (insert (concat "\"" el "\"" "\n")))
        (org-mode))
      (switch-to-buffer buffer)))))

;;;###autoload
(defun entropy/adbp-rule-blacklist-match-url-p (url &optional debug)
  (when (or (null entropy/adbp-rule--blacklist-regexps-cache)
            (null entropy/adbp-rule--whitelist-regexps-cache))
    (entropy/adbp-rule-get-regexp-matchs-list))
  (let (blacklist-p whitelist-p)
    (catch :exit
      (dolist (rule entropy/adbp-rule--whitelist-regexps-cache)
        (when (string-match-p rule url)
          (setq whitelist-p t)
          (throw :exit nil))))
    (when (null whitelist-p)
      (catch :exit
        (dolist (rule entropy/adbp-rule--blacklist-regexps-cache)
          (when (string-match-p rule url)
            (setq blacklist-p t)
            (throw :exit nil)))))
    (cond
     (whitelist-p
      (when debug
        (message "Whitlist match for url \"%s\"" url))
      nil)
     (blacklist-p
      (when debug
        (message "Blacklist match for url \"%s\"" url))
      t)
     (t
      nil))))
(memoize #'entropy/adbp-rule-blacklist-match-url-p)

;;;###autoload

(defun entropy/adbp-rule-update ()
  "Update rulesets prompted with whether follow upstream or with
local cache only. "
  (interactive)
  (let ((match-core-memoized-p
         (get 'entropy/adbp-rule-blacklist-match-url-p
              :memoize-original-function)))
    (when match-core-memoized-p
      (memoize-restore 'entropy/adbp-rule-blacklist-match-url-p))
    (let ((entropy/adbp-rule-use-upstream-rule-list
           (yes-or-no-p "Use upstream rule list? ")))
      (entropy/adbp-rule-get-regexp-matchs-list)
      (memoize #'entropy/adbp-rule-blacklist-match-url-p))))

;;; provide
(provide 'entropy-adblock+-rule-analysis)
