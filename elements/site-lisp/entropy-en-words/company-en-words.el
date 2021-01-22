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

;;; code

(require 'cl-lib)
(require 'company)
(require 'company-en-words-data "./company-en-words-data.el")

(defvar company-en-words/var--doc-buffer-name "*company-en-words-doc*")

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

(defun company-en-words (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-en-words))
    (prefix (company-grab-word))
    (candidates
     (let ((full-candis
            (when (not (string-empty-p arg))
              (delete
               nil
               (mapcar
                (lambda (c) (and (string-prefix-p (downcase arg) (car c)) (car c)))
                company-en-words-data/en-words-simple-list)))))
       (when full-candis
         (if (eq company-backend 'company-en-words)
             full-candis
           ;; reduce company tooltip laggy from reduce the candis
           ;; return while in multi backends calling status.
           (reverse (last (reverse full-candis) 10))))))
    (annotation
     (let ((props
            (alist-get arg
                       company-en-words-data/en-words-simple-list
                       nil nil 'string=)))
       (format "%s"
               (or props "␀"))))
    (doc-buffer
     (when company-en-words/var--wudao-required
       (let ((buffer (get-buffer-create company-en-words/var--doc-buffer-name))
             (inhibit-read-only t)
             (short-trans
              (wudao/query-word-by-command
               arg)))
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
