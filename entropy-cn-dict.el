;;; File name: entropy-cn-dict.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:

(require 'eww)
(require 'popwin)

(defcustom entropy/cndt-buffer-name "*entropy/cndt-buffer*"
  "Buffer name of `entropy-cn-dict'."
  :type 'string)

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
          (popwin:popup-buffer buffer
                               :dedicated nil :position 'bottom :noselect nil :height 0.4))
      (progn
        (kill-buffer-and-window)
        (funcall-interactively 'entropy/cndt-query query)))))


(provide 'entropy-cn-dict)
