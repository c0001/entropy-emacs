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
(defvar entropy/ext--extras
  (list (list :item "entropy-emacs-deps"
              :repo-lc entropy/ext-deps-dir
              :version-lc (expand-file-name "version" entropy/ext-deps-dir)
              :version "v0.1.0"
              :indicator-lc (expand-file-name "entropy-emacs-deps" entropy/ext-deps-dir))
        (list :item "entropy-emacs-extensions"
              :repo-lc entropy/ext-extensions-dir
              :version-lc (expand-file-name "version" entropy/ext-extensions-dir)
              :version "v0.1.0"
              :indicator-lc (expand-file-name "entropy-emacs-extensions" entropy/ext-extensions-dir))))


(defvar entropy/ext--extras-trouble-table
  '((0 . "%s repo doesn't exist.")
    (1 . "%s repo was fake.")
    (2 . "%s repo verion not adapt!")))


(defvar entropy/ext--extra-trouble-prompt-head
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
  or adjusting customized variable `entropy/ext-deps-dir'.

- entropy-emacs-extensions (https://github.com/c0001/entropy-emacs-extensions.git)

  (Notices: only when `entropy/use-extensions-type' eq 'submodules)

  clone it into your home dir and rename as
  '.entropy-emacs-extension' or adjusting customized variable
  `entropy/ext-extensions-dir'.
  
"
    'face 'italic)))




;; ** libraries
;; *** extra status check
(defun entropy/ext--extra-checks ()
  "Return the extra-tmaps for as extra-plists mapped as trouble
code defined in `entropy/ext--extras-trouble-table' or t."
  (let ((extras entropy/ext--extras)
        rtn)
    (dolist (el extras)
      (unless (eq (entropy/ext--extra-status el) t)
        (push (cons (entropy/ext--extra-status el) el) rtn)))
    (if rtn
        rtn
      t)))

(defun entropy/ext--extra-status (extra-plist)
  (let ((item (plist-get extra-plist :item))
        (repo_lc (plist-get extra-plist :repo-lc))
        (version_lc (plist-get extra-plist :version-lc))
        (indicator_lc (plist-get extra-plist :indicator-lc))
        (version (plist-get extra-plist :version)))
    (catch :exit
      (unless (file-exists-p repo_lc)
        (throw :exit 0))
      (unless (file-exists-p indicator_lc)
        (throw :exit 1))
      (if (not (file-exists-p version_lc))
          (throw :exit 2)
        (with-temp-buffer
          (insert-file-contents version_lc)
          (unless (string-match version (buffer-substring (point-min) (point-max)))
            (throw :exit 2))))
      t)))


;; *** trouble prompt
(defun entropy/ext--extra-prompt-troubel (extra-tmaps)
  (let ((buffer (entropy/ext--extra-create-prompt-buffer))
        troubles)
    (dolist (el extra-tmaps)
      (push (entropy/ext--extra-format-trouble el) troubles))
    (setq troubles (reverse troubles))
    (with-current-buffer buffer
      (setq troubles (entropy/numberic-list troubles))
      (dolist (el troubles)
        (insert (concat (car el) ": "
                        (cdr el) "\n\n"))))
    (switch-to-buffer buffer)
    (when (> (length (window-list)) 1)
      (delete-other-windows))))


(defun entropy/ext--extra-format-trouble (extra-tmap)
  (let* ((tcode (car extra-tmap))
         (ext-plist (cdr extra-tmap))
         (format (cdr (assoc tcode entropy/ext--extras-trouble-table))))
    (cl-case tcode
      (0 (format format (plist-get ext-plist :item)))
      (1 (format format (plist-get ext-plist :item)))
      (2 (format format (plist-get ext-plist :item))))))


(defun entropy/ext--extra-create-prompt-buffer ()
  (let ((bffN "*entropy/ext*")
        buffer)
    (setq buffer (get-buffer-create bffN))
    (with-current-buffer buffer
      (when buffer-read-only
        (read-only-mode 0))
      (goto-char (point-min))
      (insert entropy/ext--extra-trouble-prompt-head))
    buffer))

;; *** adding load path
(defun entropy/ext--add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (when (not (string-match-p "yasnippet-snippets" dir))
      (normal-top-level-add-subdirs-to-load-path))))


(defun entropy/ext--load-path (top-dir)
  (let ((subdirs (entropy/list-subdir top-dir)))
    (dolist (el subdirs)
      (add-to-list 'load-path el)
      (entropy/ext--add-subdirs-to-load-path el))))


;; ** main
(defun entropy/ext-main ()
  (let ((extras-status (entropy/ext--extra-checks)))
    (unless (eq extras-status t)
      (entropy/ext--extra-prompt-troubel extras-status))
    (if (not (eq extras-status t))
        nil
      (entropy/ext--load-path
       (expand-file-name "elements/submodules"
                         entropy/ext-deps-dir))
      (when (eq entropy/use-extensions-type 'submodules)
        (entropy/ext--load-path
         (expand-file-name "elements/submodules"
                           entropy/ext-extensions-dir)))
      t)))


;; ** provide
(provide 'entropy-emacs-ext)
