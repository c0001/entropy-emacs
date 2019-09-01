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

(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'company)
(require 'company-en-words-data "./company-en-words-data.el")

(defun company-en-words--cl-compatible-for-rmifnot (&rest args)
  (if (fboundp 'cl-remove-if-not)
      (apply 'cl-remove-if-not args)
    (apply 'remove-if-not args)))

(defun company-en-words (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command 
    (interactive (company-begin-backend 'company-en-words))
    (prefix (company-grab-word))
    (candidates 
     (company-en-words--cl-compatible-for-rmifnot
      (lambda (c) (string-prefix-p (downcase arg) c))  
      en-words-completions))
    (sorted t)
    (ignore-case 'keep-prefix)))
    
(defun company-en-words-enable ()
  (interactive)
  (add-to-list 'company-backends 'company-en-words))

(defun company-en-words-disable ()
  (interactive)
  (setq company-backends (remove 'company-en-words company-backends)))

;;(add-to-list 'company-backends 'company-en-words)

(provide 'company-en-words)
