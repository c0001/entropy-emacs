;;; company-words.el --- English words backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 by Shuai Wu

;; Author: Shuai Wu <ws.horton@gmail.com>
;; Changed by: Entropy
;; URL:
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0")(cl-lib "0.5.0"))

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

;; put this file in a path, for example "~/Dropbox/Emacs/"
;; (load "~/Dropbox/Emacs/company-words")

;; See the README for more details.

(require 'cl-lib)
(require 'company)
(require 'company-en-words-data "./company-en-words-data.el")

(defvar company-en-words/var--doc-buffer-name "*company-en-words-doc*")
(defvar company-en-words/var--riched-en-words-list nil)

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

(defvar company-en-words/var--wudao-required
  (company-en-words/lib--require-wudao))

(defvar company-en-words/var--use-wudao-confirmed nil)
(defvar company-en-words/var--wudao-cached nil)

(defun company-en-words/lib--prepare-wudao-dict ()
  ;; require wudao dict to patch fully canids with fully dict property
  (when company-en-words/var--wudao-required
    (unless (or company-en-words/var--wudao-cached
                company-en-words/var--use-wudao-confirmed)
      (setq company-en-words/var--wudao-cached nil)
      (when (and (fboundp 'wudao/query-get-en-words-completion-table)
                 (let ((confirmation
                        (yes-or-no-p "Use riched company-en-words feature?(may take some time) ")))
                   (setq company-en-words/var--use-wudao-confirmed t)
                   confirmation))
        ;; restrict `gc-cons-threshold' prevents memory overflow
        (let ((gc-cons-threshold 80000))
          (message nil)
          (message "Please wait for thus done ... (be patient)")
          (setq company-en-words/var--riched-en-words-list
                (wudao/query-get-en-words-completion-table
                 company-en-words-data/en-words-simple-list)
                company-en-words/var--wudao-cached t))))))

(defun company-en-words (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-en-words))
    (prefix (company-grab-word))
    (candidates
     (company-en-words/lib--prepare-wudao-dict)
     (let ((full-candis
            (when (not (string-empty-p arg))
              (delete
               nil
               (mapcar
                (lambda (c) (and (string-prefix-p (downcase arg) c) c))
                (or company-en-words/var--riched-en-words-list
                    company-en-words-data/en-words-simple-list))))))
       (when full-candis
         (if (eq company-backend 'company-en-words)
             full-candis
           (reverse (last (reverse full-candis) 20))))))
    (annotation
     (company-en-words/lib--prepare-wudao-dict)
     (when company-en-words/var--wudao-cached
       (let ((props
              (wudao/query-get-en-words-property
               arg 'prop-list 'chinese-simplified)))
         (format "%s"
                 (or props "␀")))))
    (doc-buffer
     (company-en-words/lib--prepare-wudao-dict)
     (when company-en-words/var--wudao-cached
       (let ((buffer (get-buffer-create company-en-words/var--doc-buffer-name))
             (inhibit-read-only t)
             (short-trans
              (wudao/query-get-en-words-property
               arg 'short-trans
               'chinese-simplified)))
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
