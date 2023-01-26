;;; company-words.el --- English words backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 by Shuai Wu

;; Author: Shuai Wu <ws.horton@gmail.com>
;; Changed by: Entropy
;; URL:
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (cl-lib "0.5.0") (trie "0.5") (memoize "20200103.2036"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; code

(require 'cl-lib)
(require 'company)
(require 'memoize)
(require 'trie)
(require 'company-en-words-data "./company-en-words-data.el")

(defvar company-en-words/var--doc-buffer-name "*company-en-words-doc*")

(defvar company-en-words/var--candi-max-len
  (length company-en-words-data/en-words-simple-list))

(defvar company-en-words/lib--en-words-trie-obj
  (make-trie '<))

(defun company-en-words/lib--require-wudao ()
  (let* ((wd-path (executable-find "wd"))
         wd-host)
    (when wd-path
      (setq wd-host (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (car (file-attributes wd-path))))))
      (add-to-list 'load-path (expand-file-name "emacs" wd-host))
      (require 'wudao-query nil t))))

(defvar company-en-words/var--trie-inited nil)
(defun company-en-words/lib--init-trie ()
  (unless company-en-words/var--trie-inited
    ;; reset trie obj since previous aborting
    (setq company-en-words/lib--en-words-trie-obj
          (make-trie '<))
    (let* ((inhibit-message nil)        ;make sure message workable
           (pgm (prog1
                    (progn
                      (redisplay t)
                      (make-progress-reporter
                       "Make company-en-words trie table ..."
                       0 (length
                          company-en-words-data/en-words-simple-list)))
                  (redisplay t))))
      ;; ensure redisplay
      (sit-for 0.1)
      (let (
            ;; reduce memory leak
            (gc-cons-threshold 8000)
            )
        (dolist-with-progress-reporter
            (el company-en-words-data/en-words-simple-list)
            pgm
          (trie-insert company-en-words/lib--en-words-trie-obj
                       (car el) (cdr el)))
        (setq company-en-words/var--trie-inited t)))))

;; initialize trie for entropy-emacs pdumper loading or daemon or
;; eemacs non-lazy loading when detected
(when (or (bound-and-true-p entropy/emacs-fall-love-with-pdumper)
          (daemonp)
          (and (boundp 'entropy/emacs-custom-enable-lazy-load)
               (not (bound-and-true-p
                     entropy/emacs-custom-enable-lazy-load))))
  (company-en-words/lib--init-trie))

(defun company-en-words/lib--query-candis-core (word maxnum)
  (company-en-words/lib--init-trie)
  (mapcar
   (lambda (arg)
     (let ((word (car arg))
           (prop (cdr arg)))
       (propertize word
                   'en-words-simple-prop
                   prop)))
   (trie-complete
    company-en-words/lib--en-words-trie-obj
    word
    :maxnum maxnum)))

(defun company-en-words/lib--query-candis (word &optional maxnum)
  (company-en-words/lib--query-candis-core
   word (or maxnum
            company-en-words/var--candi-max-len)))

(defvar company-en-words/var--wudao-required
  (company-en-words/lib--require-wudao))

(defun company-en-words (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-en-words))
    (prefix
     (let ((word (company-grab-word)))
       word))
    (kind 'text)
    (candidates
     (when (and (stringp arg)
                (not (string-empty-p arg)))
       (if (eq company-backend 'company-en-words)
           (company-en-words/lib--query-candis arg)
         ;; reduce company tooltip laggy from reduce the candis
         ;; return while in multi backends calling status.
         (company-en-words/lib--query-candis arg 10))))
    (annotation
     ;; show annotation just when `company-prefix' length larger/equal
     ;; 2 in which case do not map thousands of candis to reduce
     ;; lagging.
     (unless (or (null arg)
                 (and company-prefix
                      (< (length company-prefix) 2)))
       (let ((props
              ;; ignore errors while arg may be nil
              (get-text-property 0 'en-words-simple-prop arg)))
         (format "%s"
                 (or props " ")))))
    (doc-buffer
     (when company-en-words/var--wudao-required
       (let ((buffer (get-buffer-create company-en-words/var--doc-buffer-name))
             (inhibit-read-only t)
             (short-trans
              (apply (if (fboundp 'wudao/query-word-by-command/use-json-parse)
                         'wudao/query-word-by-command/use-json-parse
                       'wudao/query-word-by-command)
                     (list arg))))
         (with-current-buffer buffer
           (erase-buffer)
           (insert
            (format
             "%s"
             (or short-trans
                 (propertize
                  "In the beginning there's darkness!"
                  'face 'warning)))))
         buffer)))
    (sorted t)))

(provide 'company-en-words)
