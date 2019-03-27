;;; File name: entropy-org-batch-refile.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;
;;; Commentary:
;;
;; This package provide you the function for quickly batch refile org entry rely on the specific tag
;; matching.
;;
;;; TODO:
;;
;; - Improve `entropy/obr--auto-create-refile-file' auto-gen file name core function which will let
;;   sth like 'index.org<test-folder>' transfered to 'index.orgtest-folder' that we must using the
;;   further detailed category analyzation.


;; * Code:
;; ** require
(require 'org)
;; ** main function
;; *** main micro for tag match refile
(defmacro entropy/org-full-buffer-tag-match-refile (tag-regexp user-model)
  "The macro of instantiation of tag matched for `org-refile' function."
  `(if (eq major-mode 'org-mode)
       (progn
         (if buffer-read-only (read-only-mode 0))
         (goto-char (point-min))
         (let* ((full-regexp (concat "^\\*+ .*" ,tag-regexp))
                (model-list (entropy/obr--target-instantiation-model ,user-model))
                (head-name (nth 1 model-list))
                (file-name (nth 0  model-list)))
           (while (re-search-forward full-regexp nil t)
             (entropy/org-refile-specific-target head-name file-name))))
     (user-error "entropy/obr: You can not use this function in %s" major-mode)))
;; *** library for main
;; **** refile function
(defun entropy/org-refile-specific-target (headline file)
  "Refile headline into another file within it's specific
headline.

Idea from:
https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/
"
  (let* ((pos (save-excursion
                (find-file file)
                (read-only-mode 0)
                (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))
;; **** target model justify and return result function
;; ***** user-model instantiation function
(defun entropy/obr--target-instantiation-model (user-target-model)
  "
User-target-model is a list within 3 elements:

- model-type: 

            this was one list contain two elements, they express
            the state of target for refilling.
            
            The sequence of this list were:

            + nil/t: whether specific the exists file

            + 1/2/3: whether specific the exists headline or let
                     this function automaticlly create it or
                     automaticlly create headline name with
                     headline name you specific.

- target head name (string): if the first elements was '(t 1)'
                             you can specific it, or you must set
                             it to nil.

- target file name (string): if first elements was '(t x)' you
                             can specific it, or you must set it
                             to nil.

This function return the a list of \"'(file head)\"
"
  (let ((model (car user-target-model))
        (head (nth 1 user-target-model))
        (file (nth 2 user-target-model))
        model-list)
    (cond
     ;; cond 1
     ((equal model '(t 1))
      (if (entropy/obr--justify-head-in-file head file)
          (progn (add-to-list 'model-list head)
                 (add-to-list 'model-list file))
        (error "entropy/obr: user-target-model invalid! model:t 1")))
     ;; cond 2
     ((equal model '(t 2))
      (if (file-exists-p file)
          (let* ((head-auto (entropy/obr--auto-create-headline-name))
                 (auto-hn (entropy/obr--headline-combine head-auto)))
            (with-current-buffer (find-file-noselect file)
              (if buffer-read-only (read-only-mode 0))
              (goto-char (point-max))
              (newline 2)
              (forward-line 0)
              (insert auto-hn))
            (add-to-list 'model-list head-auto)
            (add-to-list 'model-list file))
        (error "entropy/obr: user-target-model invalid! model: t 2")))
     ;; cond 3
     ((equal model '(t 3))
      (if (and (file-exists-p file) head (equal (stringp head) t))
          (progn
            (with-current-buffer (find-file-noselect file)
              (if buffer-read-only (read-only-mode 0))
              (goto-char (point-max))
              (newline 2)
              (forward-line 0)
              (insert (concat "* " head)))
            (add-to-list 'model-list head)
            (add-to-list 'model-list file))
        (error "entropy/obr: user-target-model invalid! model: t 3")))
     ;; cond 4
     ((equal model '(nil 2))
      (let ((auto-refile-model-list (entropy/obr--auto-create-refile-file t)))
        (add-to-list 'model-list (car auto-refile-model-list))
        (add-to-list 'model-list (nth 1 auto-refile-model-list))))
     ;; cond 5
     ((equal model '(nil 3))
      (let ((auto-refile-model-list (entropy/obr--auto-create-refile-file)))
        (if (and head (eq (stringp head) t))
            (progn
              (add-to-list 'model-list head)
              (with-current-buffer (find-file-noselect (nth 1 auto-refile-model-list))
                (if buffer-read-only (read-only-mode 0))
                (goto-char (point-max))
                (newline 2)
                (forward-line 0)
                (insert (concat "* " head))))
          (error "entropy/obr: user-target-model invalid! model: nil 3"))
        (add-to-list 'model-list (nth 1 auto-refile-model-list))))
     ;; cond t
     (t (error "entropy/obr: user-target-model invalid! model: \"(cond t)\"")))
    model-list))
;; ***** justify whether head in file funciton
(defun entropy/obr--justify-head-in-file (HEAD FILE)
  "Justify whether headline HEAD in file FILE.

HEAD and FILE are both string type. 

Final return t or nil accroding the state of result with
justifing.
"
  (let (return-p) ;; declare the return variable
    (if (file-exists-p FILE)
        (let ((buffer (find-file-noselect FILE)))
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (re-search-forward (regexp-quote HEAD) nil t)
                (progn (setq return-p t)
                       return-p)
              (progn (setq return-p nil)
                     return-p))))
      (progn (setq return-p nil)
             return-p))))
;; ***** auto create refile file function
(defun entropy/obr--auto-create-refile-file (&optional auto-insert-enable)
  " Auto create a refile file and alternatively for auto
generating refile head line.

It's return a list with head and file:
  
   (head file)"
  ;; adjusting file name accroding to current buffer file name
  (let* ((buffer (buffer-name))
         (invalid-regexp "\\(\\*\\|<\\|>\\)")
         refile-target-file-name
         (range-data (entropy/obr--range-data))
         (auto-headline-name (entropy/obr--auto-create-headline-name range-data))
         return-list)
    (if (string-match-p invalid-regexp buffer)
        (setq refile-target-file-name (concat (replace-regexp-in-string invalid-regexp "" buffer)
                                              "-refile"
                                              range-data
                                              ".org"))
      (setq refile-target-file-name (concat buffer "-refile" range-data ".org")))
    (with-temp-file (concat "./" refile-target-file-name)
      (if buffer-read-only (read-only-mode 0))
      (if auto-insert-enable
          (progn
            (goto-char (point-max))
            (newline)
            (forward-line 0)
            (insert (entropy/obr--headline-combine auto-headline-name))
            (setq return-list `(,auto-headline-name ,refile-target-file-name)))
        (setq return-list `(nil ,refile-target-file-name))))
    return-list))
;; ***** auto create headline name function
(defun entropy/obr--auto-create-headline-name (&optional RANGE)
  "Generate one mechanical headline name and return it in string
type. 

The optional arg RANGE was user specific range data for using as
the suffix data in head-name, nor will automatically create it
from `entropy/obr--range-data'.

The return string format like: \"* Refile-20180604_Mon_135151\""
  (let* ((range-data (entropy/obr--range-data))
         (auto-headline-name (concat "Refile" range-data))
         return-string)
    (if (and RANGE (equal t (stringp RANGE)))
        (setq return-string (concat "Refile" RANGE))
      (setq return-string auto-headline-name))
    return-string))
;; ***** range data function
(defun entropy/obr--range-data ()
  "Generate one ranger data for naming using and return it as
string.

The return string format is like: \"-20180604_Mon_135151\"."
  (let ((time (format-time-string "%Y%m%d_%a_%H%M%S"))
        return-data)
    (setq return-data (concat "-" time))
    return-data))
;; ***** headline hierachy combine
(defun entropy/obr--headline-combine (head-name &optional hierachy)
  " Combine headline name to completely full format of org's
headline sytle.

Optional arg 'hierachy' was the count of '*', and the default
count was 1.
"
  (let (return-head)
    (if (and hierachy (> hierachy 1))
        (progn
          (let ((count 0)
                (star-char ""))
            (while (<= count hierachy)
              (setq star-char (concat star-char "*"))
              (+ count 1))
            (setq return-head (concat star-char " " head-name))
            return-head))
      (progn
        (setq return-head (concat "* " head-name))
        return-head))))
;; ** interactive funcion

;;;###autoload
(defun entropy/org-batch-refile-tags-read-and-do (tag-regexp)
  "Refile by specifying the tag matched with interactively envrionment."
  (interactive "sInput refile-tag-regexp: ")
  (entropy/org-full-buffer-tag-match-refile tag-regexp '((nil 2) nil nil)))


;; * provide
(provide 'entropy-org-batch-refile)
