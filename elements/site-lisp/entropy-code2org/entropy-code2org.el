;;; entropy-code2org --- emacs package for convert code file to org file
;;
;; * Copyright (C) 20200417  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: 0.1.0
;; Version:       0.1.0
;; Created:       2020-04-17 01:59:47
;; Keywords:      org, outline
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; * Commentary:

;; Convert your source buffer into an org context.

;; Usage:

;; - ~entropy/code2org-export-cur-to-README~ :: convert current
;;   buffer to README as markdown format.
;; - ~entropy/code2org-export-cur-to-org-file~ :: convert current
;;   buffer to a org file.
;; - ~entropy/code2org-export-cur-to-html-file~ :: convert current
;;   buffer to a html file.
;; - ~entropy/code2org-convert-current-buffer~ :: convert current
;;   buffer to a temporal buffer with org context.

;; * Configuration:
;;
;; Just ~(require 'entropy-code2org)~
;;
;; * Code:
;; ** Require
(require 'outline)
(require 'outorg)


(defun entropy/code2org--beforeadvice-for-outorg-convert-to-org (&rest _)
  "Prunning current buffer's emtpy commented line for
preventing uncommenting funciton throw out the current marker
which will make outorg's 'looping' procedure can not terminated
in correct way.

Example in lisp mode buffer:

#+BEGIN_SRC emacs-lisp
  ;;; code
  ;;
  ;; Test outorg transfer to org context
  ;;
  ;;; Foobar
  ;;
  ;; test
#+END_SRC

Line 2 and 4 even for 6, can not uncomment by
`uncomment-region-default-1' for commonly result, it will
warnning for prompt \"Beginning of buffer\" which will overwrite
the outorg convert procedure (i.e. `outorg-convert-to-org')
temporal marker which indicates the comment beginning, thus the
following marker moving operation will cause non-terminated
looping proceeding."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (format "^%s+ *$" (regexp-quote comment-start)) nil t)
        (replace-match "")))))

(advice-add 'outorg-convert-to-org
            :before #'entropy/code2org--beforeadvice-for-outorg-convert-to-org)

;; ** Customize
(defgroup entropy/code2org-customize-group nil
  "Customized variable group for `entropy-code2org'."
  :group 'outline)

(defcustom entropy/code2org-special-context-rule
  '(
    ;; *emacs-lisp-mode*
    ((lambda () (eq major-mode 'emacs-lisp-mode))
     (lambda ()
       ;; Deal with first line if it prefix with ";;;"
       (goto-char (point-min))
       (while (if (save-excursion (re-search-forward ";;[ ]?\\*+" (line-end-position) t))
                  (re-search-forward "^;;[ ]?\\*+.*---[ ]+" (line-end-position) t)
                (re-search-forward "^;;;.*---[ ]+" (line-end-position) t))
         (replace-match ";; #+TITLE: " nil t))
       ;; Remove lexical-binding string
       (goto-char (point-min))
       (while (re-search-forward "[ ]*-\\*-.*-\\*-[ ]*$"
                                 (line-end-position) t)
         (replace-match "" nil t))
       ;; Deal with ";; Local Variables:" and ";; End:"
       (goto-char (point-min))
       (while (re-search-forward "^;;+[ ]+Local[ ]+Variables: *" nil t)
         (replace-match ";; #+begin_example\n;; Local Variables:" nil t))
       (goto-char (point-min))
       (while (re-search-forward "^;;+[ ]+End: *" nil t)
         (replace-match ";; End:\n;; #+end_example" nil t)))))
  "A of content replacement predicate for specific buffer.

Each element was a list of two function subset, while both of
them has non arguments requirement, and the car's function was a
judger for judging whether using the `cadr' predicated function
for specific buffer.

Its no need to inhibit buffer read-only status because of that
all the predicate will be wrapped into thus. "
  :type 'list
  :group 'entropy/code2org-customize-group)

;; ** Internal library
(defun entropy/code2org--replace-special-context (buffer-or-name)
  (with-current-buffer buffer-or-name
    (let (predicate
          (inhibit-read-only t))
      (catch :exit
        (dolist (rule entropy/code2org-special-context-rule)
          (when (funcall (car rule))
            (setq predicate (cadr rule))
            (throw :exit nil))))
      (when predicate
        (funcall predicate))))
  buffer-or-name)

(defun entropy/code2org--handle-comment-start-regexp
    (specific-comment-start-regexp)
  (or specific-comment-start-regexp
      (format "%s+[ ]*\\*+" comment-start)))

(defun entropy/code2org--convert-code-buffer
    (buffer-or-name &optional specific-comment-start-regexp)
  (let ((inhibit-read-only t))
    (with-current-buffer buffer-or-name
      (setq outline-regexp
            (entropy/code2org--handle-comment-start-regexp
             specific-comment-start-regexp))
      (outorg-convert-to-org)))
  buffer-or-name)

(defun entropy/code2org--handle-destination
    (source-buffer-or-name destination &optional enable-mode)
  (let ((buffer-content (with-current-buffer source-buffer-or-name
                          (save-excursion
                            (buffer-substring-no-properties
                             (point-min)
                             (point-max)))))
        (enable-mode (or enable-mode (with-current-buffer source-buffer-or-name major-mode)))
        (inhibit-read-only t)
        (target-buffer (cond ((eq (car destination) 'file)
                              (find-file-noselect (cdr destination)))
                             ((eq (car destination) 'buffer)
                              (get-buffer (cdr destination))))))
    (with-current-buffer target-buffer
      (insert buffer-content)
      (funcall enable-mode))
    target-buffer))

(defun entropy/code2org-convert-which-buffer
    (buffer-or-name
     destination
     &optional
     buffer-specific-major-mode
     buffer-comment-start-regexp)
  (let ((target (entropy/code2org--handle-destination
                 buffer-or-name
                 destination
                 buffer-specific-major-mode)))
    (with-current-buffer
     (entropy/code2org--convert-code-buffer
      (entropy/code2org--replace-special-context target)
      buffer-comment-start-regexp)
     (org-mode)
     (current-buffer))))

(defmacro entropy/code2org--with-current-buffer-output (&rest body)
  `(let ((output (entropy/code2org-convert-which-buffer
                  (current-buffer)
                  (cons 'buffer
                        (let ((buffer (get-buffer-create "*entropy/code2org-temp*"))
                              (inhibit-read-only t))
                          (with-current-buffer buffer
                            (erase-buffer))
                          buffer)))))
     (with-current-buffer output
       ,@body)))

;; ** Autoloads
;;;###autoload

(defun entropy/code2org-convert-current-buffer ()
  "Convert current buffer into org-mode context and focus on the
transferred buffer."
  (interactive)
  (switch-to-buffer
   (entropy/code2org-convert-which-buffer
    (current-buffer)
    (cons 'buffer
          (let ((buffer (get-buffer-create "*entropy/code2org-temp*"))
                (inhibit-read-only t))
            (with-current-buffer buffer
              (erase-buffer))
            buffer)))))

(defun entropy/code2org-export-cur-to-org-file ()
  "Convert current buffer into a temporal org context file named
as file-name.org where file-name is the current file name or the
buffer name if its no related file visited bounding."
  (interactive)
  (let ((archive-root default-directory)
        (item-name (or (ignore-errors (file-name-nondirectory (buffer-file-name)))
                       (buffer-name))))
    (entropy/code2org--with-current-buffer-output
     (let ()
       (write-file
        (expand-file-name
         (format "%s.org" item-name)
         archive-root))))))

(defun entropy/code2org-export-cur-to-html-file ()
    "Convert current buffer into a temporal html context file
named as file-name.html where file-name is the current file name
or the buffer name if its no related file visited bounding."
  (interactive)
  (let ((archive-root default-directory)
        (item-name (or (ignore-errors (file-name-nondirectory (buffer-file-name)))
                       (buffer-name))))
    (entropy/code2org--with-current-buffer-output
     (let ()
       (org-export-to-file
           'html
           (expand-file-name
            (format "%s.html" item-name)
            archive-root))))))

(defun entropy/code2org-export-cur-to-README ()
    "Convert current buffer into a temporal markdown context file
named as file-name.md where file-name is the current file name or
the buffer name if its no related file visited bounding."
  (interactive)
  (let ((archive-root default-directory)
        (item-name (or (ignore-errors (file-name-nondirectory (buffer-file-name)))
                       (buffer-name))))
    (entropy/code2org--with-current-buffer-output
     (let ()
       (org-export-to-file
           'md
           (expand-file-name
            (format "README.md" item-name)
            archive-root))))))

;; * provide
(provide 'entropy-code2org)
