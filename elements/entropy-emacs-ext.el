;;; entropy-emacs-ext.el --- extensions detectivation

;; Copyright (C) 2019  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       noone
;; Created:       2019-04-24 06:04:03
;; Keywords:      none, none, none,
;; Compatibility: GNU Emacs 25.1;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; none
;;
;;; Configuration:
;;
;; none

;;; Code:


(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** defvar
(defvar entropy/emacs-ext--extras
  (list (list :item "entropy-emacs-deps"
              :repo-lc entropy/emacs-ext-deps-dir
              :version-lc (expand-file-name "version" entropy/emacs-ext-deps-dir)
              :version "0.1.3"
              :indicator-lc (expand-file-name "entropy-emacs-deps" entropy/emacs-ext-deps-dir)
              :inited-indicator-lc (expand-file-name "init" entropy/emacs-ext-deps-dir))
        (list :item "entropy-emacs-extensions"
              :repo-lc entropy/emacs-ext-extensions-dir
              :version-lc (expand-file-name "version" entropy/emacs-ext-extensions-dir)
              :version "0.1.1"
              :indicator-lc (expand-file-name "entropy-emacs-extensions" entropy/emacs-ext-extensions-dir)
              :inited-indicator-lc (expand-file-name "init" entropy/emacs-ext-extensions-dir))))


(defvar entropy/emacs-ext--extras-trouble-table
  '((0 . "%s repo doesn't exist.")
    (1 . "%s repo was fake.")
    (2 . "%s version indiator lost! Please repair '%s'.")
    (3 . "%s repo verion lower-than the requested! Update it first!")
    (4 . "%s repo verion upper-than the requested! Update entropy-emacs first!")
    (5 . "%s repo not initialzed, see '%s' README for as.")))


(defvar entropy/emacs-ext--extra-trouble-prompt-head
  (concat 
   (propertize
    "This buffer occurred when some entropy-emacs extras missing or
with the wrong status discovered automatically by
entropy-emacs. This occasion will occurred until you've solved
such the problem.

"

    'face 'warning)

   (propertize 
   "
There's two entropy-emacs extras may need to download by your self:
"
   'face 'bold)

   (propertize
    "
- entropy-emacs-deps (https://github.com/c0001/entropy-emacs-deps.git)

  clone it into your home dir and rename as '.entropy-emacs-deps'
  or adjusting customized variable `entropy/emacs-ext-deps-dir'.

  If the first time cloning it, please see its README and make it
  initialized.

- entropy-emacs-extensions (https://github.com/c0001/entropy-emacs-extensions.git)

  (Notices: only when `entropy/emacs-use-extensions-type' eq 'submodules)

  clone it into your home dir and rename as
  '.entropy-emacs-extension' or adjusting customized variable
  `entropy/emacs-ext-extensions-dir'.

  If the first time cloning it, please see its README and make it
  initialized.

"
    'face 'italic)))




;; ** libraries
;; *** extra status check
(defun entropy/emacs-ext--check-all-extras ()
  "Return the extra-tmaps for as extra-plists mapped as trouble
code defined in `entropy/emacs-ext--extras-trouble-table' or t."
  (let ((extras (entropy/emacs-ext--check-inuse-extras))
        rtn)
    (dolist (el extras)
      (unless (eq (entropy/emacs-ext--check-extra-status el) t)
        (push (cons (entropy/emacs-ext--check-extra-status el) el) rtn)))
    (if rtn
        rtn
      t)))


(defun entropy/emacs-ext--check-inuse-extras ()
  (let ((full-extras entropy/emacs-ext--extras))
    (if (eq entropy/emacs-use-extensions-type 'origin)
        (list (car full-extras))
      full-extras)))

(defun entropy/emacs-ext--check-extra-status (extra-plist)
  (let ((item (plist-get extra-plist :item))
        (repo_lc (plist-get extra-plist :repo-lc))
        (version_lc (plist-get extra-plist :version-lc))
        (indicator_lc (plist-get extra-plist :indicator-lc))
        (version (plist-get extra-plist :version))
        (inited-indicator (plist-get extra-plist :inited-indicator-lc)))
    (catch :exit
      (unless (file-exists-p repo_lc)
        (throw :exit 0))
      (unless (file-exists-p indicator_lc)
        (throw :exit 1))
      (if (not (file-exists-p version_lc))
          (throw :exit 2)
        (with-temp-buffer
          (insert-file-contents version_lc)
          (cond
           ((version< (buffer-substring (point-min) (point-max)) version)
            (throw :exit 3))
           ((version< version (buffer-substring (point-min) (point-max)))
            (throw :exit 4)))))
      (unless (file-exists-p inited-indicator)
        (throw :exit 5))
      t)))


;; *** trouble prompt
(defun entropy/emacs-ext--extra-prompt-troubel (extra-tmaps)
  (let ((buffer (entropy/emacs-ext--extra-create-prompt-buffer))
        troubles)
    (dolist (el extra-tmaps)
      (push (entropy/emacs-ext--extra-format-trouble el) troubles))
    (setq troubles (reverse troubles))
    (with-current-buffer buffer
      (setq troubles (entropy/emacs-numberic-list troubles))
      (dolist (el troubles)
        (insert (concat (car el) ": "
                        (cdr el) "\n\n"))))
    (switch-to-buffer buffer)
    (when (> (length (window-list)) 1)
      (delete-other-windows))))


(defun entropy/emacs-ext--extra-format-trouble (extra-tmap)
  (let* ((tcode (car extra-tmap))
         (ext-plist (cdr extra-tmap))
         (format (cdr (assoc tcode entropy/emacs-ext--extras-trouble-table))))
    (or (ignore-errors (format format (plist-get ext-plist :item)))
        (ignore-errors (format format (plist-get ext-plist :item) (plist-get ext-plist :item))))))


(defun entropy/emacs-ext--extra-create-prompt-buffer ()
  (let ((bffN "*entropy/emacs-ext*")
        buffer)
    (setq buffer (get-buffer-create bffN))
    (with-current-buffer buffer
      (when buffer-read-only
        (read-only-mode 0))
      (goto-char (point-min))
      (insert entropy/emacs-ext--extra-trouble-prompt-head)
      (insert "\n\n")
      (insert (propertize "Tourble meet:" 'face 'underline))
      (insert "\n\n"))
    buffer))

;; *** adding load path
(defun entropy/emacs-ext--add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (when (not (string-match-p "yasnippet-snippets" dir))
      (normal-top-level-add-subdirs-to-load-path))))


(defun entropy/emacs-ext--load-path (top-dir)
  (let ((subdirs (entropy/emacs-list-subdir top-dir)))
    (dolist (el subdirs)
      (add-to-list 'load-path el)
      (entropy/emacs-ext--add-subdirs-to-load-path el))))


;; ** main
(defun entropy/emacs-ext-main ()
  (let ((extras-status (entropy/emacs-ext--check-all-extras)))
    (unless (eq extras-status t)
      (entropy/emacs-ext--extra-prompt-troubel extras-status))
    (if (not (eq extras-status t))
        nil
      (entropy/emacs-ext--load-path
       (expand-file-name "elements/submodules"
                         entropy/emacs-ext-deps-dir))
      (when (eq entropy/emacs-use-extensions-type 'submodules)
        (entropy/emacs-ext--load-path
         (expand-file-name "elements/submodules"
                           entropy/emacs-ext-extensions-dir)))
      (when (and entropy/emacs-ext-user-specific-load-paths
                 (listp entropy/emacs-ext-user-specific-load-paths))
        (dolist (el entropy/emacs-ext-user-specific-load-paths)
          (when (ignore-errors (file-directory-p el))
            (entropy/emacs-ext--load-path (expand-file-name el)))))
      t)))


;; ** provide
(provide 'entropy-emacs-ext)
