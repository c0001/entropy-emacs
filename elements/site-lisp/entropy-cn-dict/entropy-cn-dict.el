;;; entropy-cn-dict.el --- Simple chinese dictionary using moedict.el  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20261019 Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-cn-dict
;; Package-Version: 0.2.0
;; Created:       2018
;; Keywords:      dictionary, translation, cn-dict
;; Compatibility: GNU Emacs 24;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (moedict))
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

(require 'moedict)
(require 'entropy-s2t)

(defgroup entropy/cndt-customize-top-group nil
  "Customize group for `entropy-cn-dict'."
  :group 'extensions)

(defcustom entropy/cndt-buffer-name "*entropy/cndt-buffer*"
  "Buffer name of `entropy-cn-dict'."
  :type 'string
  :group 'entropy/cndt-customize-top-group)

(defvar entropy/cndt--doc-buffer nil)

(defconst entropy/cndt-cjk-char-regexp
  "[\u4e00-\u9fa5]"
  "CJK char regexp presentation")

(defvar entropy/cndt--query-pages nil)
(defvar entropy/cndt--query-pages-idx -1)
(defun entropy/cndt--save-page nil
  (with-current-buffer entropy/cndt--doc-buffer
    (let ((doc (buffer-substring (point-min) (point-max)))
          (cur-idx entropy/cndt--query-pages-idx))
      (setf (nthcdr (1+ cur-idx) entropy/cndt--query-pages)
            (cons doc (nthcdr (1+ cur-idx) entropy/cndt--query-pages)))
      (setq entropy/cndt--query-pages-idx (1+ cur-idx)))))
(defun entropy/cndt-prev-page nil
  (interactive nil entropy/cndt-mode)
  (let ((idx (1- entropy/cndt--query-pages-idx)))
    (unless (< idx 0)
      (entropy/cndt--render-doc (nth idx entropy/cndt--query-pages))
      (setq entropy/cndt--query-pages-idx idx))))
(defun entropy/cndt-next-page nil
  (interactive nil entropy/cndt-mode)
  (let ((idx (1+ entropy/cndt--query-pages-idx)))
    (unless (> idx (1- (length entropy/cndt--query-pages)))
      (entropy/cndt--render-doc (nth idx entropy/cndt--query-pages))
      (setq entropy/cndt--query-pages-idx idx))))

(defun entropy/cndt--render-doc (doc &optional save)
  (with-current-buffer entropy/cndt--doc-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert doc)
      (and save (entropy/cndt--save-page))
      (goto-char (point-min))
      (entropy/cndt-mode 1)
      (read-only-mode 1))))

(defvar entropy/cndt-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "l") #'entropy/cndt-prev-page)
    (define-key map (kbd "r") #'entropy/cndt-next-page)
    map))
(define-minor-mode entropy/cndt-mode
  "`major-mode' for `entropy/cndt-query'"
  :initial-value nil
  :keymap entropy/cndt-mode-keymap
  nil)

(defun entropy/cndt--ensure-moedict-db nil
  (unless (file-exists-p moedict-dictionary-file-path)
    (unless (executable-find "xz")
      (user-error "您的系統上沒有發現xz這個指令，請安裝後再重試一次。
Command 'xz' not found on your system. Please install it then try again"))
    (unless (file-exists-p moedict-dictionary-xz-file-path)
      (user-error "字典檔不存在"))
    (moedict-message "字典檔設定中，請稍後 ...")
    (redisplay t)
    (let* ((proc-buffer (get-buffer-create " *moedict uncompress*" t))
           (exit-code (call-process "xz" nil proc-buffer t
                                    "-kdfvv"
                                    (format "%s.xz" moedict-dictionary-file-path))))
      (if (= 0 exit-code) (moedict-message "字典檔設定完成！")
        (display-buffer proc-buffer)
        (user-error "字典檔設定失敗！")))))

(defun entropy/cndt-query (query)
  "`entropy-cn-dict' main interactive function.

If your mark one region of buffer then query the region, otherwise
prompt for quering.

Tips:

You can repeatly query dict witin `entropy/cndt-buffer-name' buffer,
with keybinding of `entropy/cndt-mode-keymap' for back/forward query
pages."
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
  (let ()
    (unless (and query (not (string= "" query)))
      (error "cndt: Query is invalid!"))
    ;; NOTE: moedict is traditional chinese dictionary which do not
    ;; support simplified chinese querying.
    (setq query (entropy/s2t-string query))
    (entropy/cndt--ensure-moedict-db)
    (let ((doc (moedict-query-vocabulary query)))
      (if doc (setq doc (moedict--render-rows doc))
        (user-error "No defs for query: %s" query))
      (setq entropy/cndt--doc-buffer
            (get-buffer-create entropy/cndt-buffer-name t))
      ;; ensure `default-directory' bind to old created doc buffer
      ;; existence or error of setting default-directory will thrown
      ;; out for new queries.
      (unless (eq (current-buffer) entropy/cndt--doc-buffer)
        (let ((defdir default-directory))
          (with-current-buffer entropy/cndt--doc-buffer
            (setq default-directory defdir))))
      (entropy/cndt--render-doc doc t)
      (display-buffer entropy/cndt--doc-buffer))))

(provide 'entropy-cn-dict)
