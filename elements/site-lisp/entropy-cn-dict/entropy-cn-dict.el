;;; entropy-cn-dict.el --- Simple chinese dictionary using BAIDU API  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20190911 Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-cn-dict
;; Package-Version: 0.1.0
;; Created:       2018
;; Keywords:      dictionary, translation, cn-dict
;; Compatibility: GNU Emacs 24;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (eww))
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
;;
;; A simple chinese dictionary using BAIDU API.
;;
;;; Configuration:
;;
;; Added to load-path and requiring
;;
;;; Code:

(require 'eww)

(defgroup entropy/cndt-customize-top-group nil
  "Customize group for `entropy-cn-dict'."
  :group 'extensions)

(defcustom entropy/cndt-buffer-name "*entropy/cndt-buffer*"
  "Buffer name of `entropy-cn-dict'."
  :type 'string
  :group 'entropy/cndt-customize-top-group)

(defvar entropy/cndt-engine "https://hanyu.baidu.com/s?wd="
  "Search engine for chinese string query.")

(defun entropy/cndt-get-query (url)
  "Query function of `entropy-cn-dict' for retrieve url and
process data of retrieved data for insert the result to current
buffer."
  (eww-browse-url url))

(defun entropy/cndt-search-query (url)
  "Return buffer with retrieved data using url processed by
`entropy/cndt-get-query'."
  (let* ((buffer (get-buffer-create entropy/cndt-buffer-name)))
    (with-current-buffer  buffer
      (if (not (equal major-mode 'eww-mode))
          (eww-mode))
      (entropy/cndt-get-query url)
      (bury-buffer))
    buffer))

(defun entropy/cndt-query (query)
  "`entropy-cn-dict' main interactive function.

If your mark one region of buffer then query the region, otherwise
prompt for quering.

Tips:

You can repeatly query dict witin `entropy/cndt-buffer-name'
buffer, that it will not destruct the window config."
  (interactive
   (list (let* (region)
           (cond
            ((region-active-p)
             (progn
               (setq region (buffer-substring-no-properties (region-beginning) (region-end)))
               region))
            (t
             (let ((word (read-string "Input query: ")))
               word))))))
  (let (url)
    (if (and query (not (string= "" query)))
        (setq url (concat entropy/cndt-engine (url-hexify-string query)))
      (error "cndt: Query is invalid!"))
    (if (not (equal (buffer-name) entropy/cndt-buffer-name))
        (let* ((buffer (entropy/cndt-search-query url)))
          (display-buffer buffer))
      (progn
        (kill-buffer-and-window)
        (funcall 'entropy/cndt-query query)))))


(provide 'entropy-cn-dict)
