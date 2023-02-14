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

(defconst entropy/cndt-cjk-char-regexp
  "[\u4e00-\u9fa5]"
  "CJK char regexp presentation")

(defun entropy/cndt-get-query (url)
  "Query function of `entropy-cn-dict' for retrieve url and
process data of retrieved data for insert the result to current
buffer."
  (let ((url-allow-non-local-files t)
        (inhibit-read-only t)
        ewdata)
    (fundamental-mode)
    (erase-buffer)
    (insert
     (with-current-buffer
         (condition-case-unless-debug err
             (url-retrieve-synchronously url)
           (error
            ;; just abort when `url-retrieve-synchronously' can not
            ;; handle url but in `debug-on-error' case since there's
            ;; may caused by some other influences.
            (user-error "can not retrieve `%s' (%s)" url err)))
       (set-buffer-multibyte t)
       (goto-char (point-min))
       ;; delete the http request header meta data
       (when (re-search-forward "^$" nil t)
         (delete-region (point-min) (point)))
       ;; use eww subroutine to render the retrieved DOM so that we
       ;; can treat it a eww buffer as usually did as via `eww'.
       (eww-mode)
       (eww-display-html nil url nil nil (current-buffer))
       ;; cross referrence navigation history enable via `eww-data'
       (setq ewdata eww-data)
       (prog1 (buffer-substring (point-min) (point-max))
         (kill-buffer))))
    (eww-mode)
    (setq-local eww-data ewdata)
    (goto-char (point-min))))

(defun entropy/cndt-search-query (url)
  "Return buffer with retrieved data using url processed by
`entropy/cndt-get-query'."
  (let* ((buffer (get-buffer-create entropy/cndt-buffer-name))
         (inhibit-read-only t) pt
         (del-func
          (lambda nil
            (let ((inhibit-read-only t))
              (goto-char pt)
              (delete-region (point-min) (line-beginning-position)))
            (goto-char (point-min)))))
    (with-current-buffer buffer
      (entropy/cndt-get-query url)
      (cond
       ((save-excursion (and (re-search-forward "^* 拼 音" nil t)
                             (setq pt (point))))
        (funcall del-func))
       ((save-excursion (and (re-search-forward "^* 百度首页" nil t)
                             (setq pt (point))))
        (funcall del-func)
        (when (save-excursion (and (re-search-forward "^*\n\\(^$\\)" nil t))
                              (setq pt (point)))
          (funcall del-func)))))
    buffer))

(defun entropy/cndt-query (query)
  "`entropy-cn-dict' main interactive function.

If your mark one region of buffer then query the region, otherwise
prompt for quering.

Tips:

You can repeatly query dict witin `entropy/cndt-buffer-name'
buffer, that it will not destruct the window config."
  (interactive
   (list (let* (region c)
           (cond
            ((region-active-p)
             (progn
               (setq region (buffer-substring-no-properties (region-beginning) (region-end)))
               region))
            ((and (setq c (char-to-string (following-char)))
                  (string-match-p entropy/cndt-cjk-char-regexp c))
             c)
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
