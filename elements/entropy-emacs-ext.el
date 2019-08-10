;;;  entropy-emacs-ext.el --- entropy-emacs extra dependencies configuration
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ext.el
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
;;
;; Excluded from this project i.e. the `entropy-emacs' using various
;; third-party extensions released on =github=, =elpa=, =melpa= and
;; other remote host server, thus the dependencies manamgement was
;; necessary does for checking and loading them correctly.
;;
;; `entropy-emacs' has the specific map of extensions categories
;; followed the loading priority and archiving method, variable
;; `entropy/emacs-ext-deps-dir', `entropy/emacs-ext-extensions-dir'
;; and `entropy/emacs-ext-user-specific-load-paths' are given for
;; those aim.
;;
;; Emacs has its own extensions management tool
;; i.e. =pacakge.el=. this tool has the default upstream [[https://elpa.gnu.org][elpa]] and
;; [[https://melpa.org/#/][melpa]], although there's lots of extensions registered in them host
;; and seems enoughly daily using for most of emacs user, but for
;; some rare things or some package didn't commit to those host. For
;; those case, entropy-emacs using the above three variable to cover
;; the extension hosted meta types.
;;
;; - `entropy/emacs-ext-deps-dir' indicates the project
;;   [[https://github.com/c0001/entropy-emacs-deps][entropy-emacs-deps]] location. =entroy-emacs-deps= was the the
;;   project hosted the extensions who may not registered in =melpa=
;;   or =elpa= host, and both used for project
;;   =entropy-emacs-extensions=(see below for its introduction).
;;
;; - `entropy/emacs-ext-extensions-dir' inidicates the local location
;;   of project [[https://github.com/c0001/entropy-emacs-extensions][entropy-emacs-extensions]] which was the git repo of
;;   each elpa or melpa packages archved for the sake of tracking
;;   version peer point to each extension relied by
;;   `entropy-emacs'. It can be the mirror for dependencies on melpa
;;   or elpa host but version specified for `entropy-emacs'.
;;
;; - `entropy/emacs-ext-user-specific-load-paths' was the variable
;;   for user specified extensions archive loaction which will be
;;   added to `load-path' recursively, it's a list of root of thus.
;;
;; For more package management mechanism learning, please view 'code'
;; section for the source code. 
;;
;; * Configuration:
;; 
;; Loading automatically by `entropy-emacs' but no hacking warranty.
;; 
;; * Code:


(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** defvar
(defvar entropy/emacs-ext--extras
  (list (list :item "entropy-emacs-deps"
              :repo-lc entropy/emacs-ext-deps-dir
              :version-lc (expand-file-name "version" entropy/emacs-ext-deps-dir)
              :version "0.1.6.1"
              :indicator-lc (expand-file-name "entropy-emacs-deps" entropy/emacs-ext-deps-dir)
              :inited-indicator-lc (expand-file-name "init" entropy/emacs-ext-deps-dir)
              :load-predicate (expand-file-name "entropy-emacs-deps-load.el" entropy/emacs-ext-deps-dir))
        (list :item "entropy-emacs-extensions"
              :repo-lc entropy/emacs-ext-extensions-dir
              :version-lc (expand-file-name "version" entropy/emacs-ext-extensions-dir)
              :version "0.1.8.3"
              :indicator-lc (expand-file-name "entropy-emacs-extensions" entropy/emacs-ext-extensions-dir)
              :inited-indicator-lc (expand-file-name "init" entropy/emacs-ext-extensions-dir)
              :load-predicate (expand-file-name "entropy-emacs-extensions-load.el" entropy/emacs-ext-extensions-dir))))


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


;; *** byte compile package
(defun entropy/emacs-ext--byte-compile-directory (dir)
  (let* ((dir-cur (expand-file-name dir))
         (dir-cur-P (unless (file-exists-p dir-cur)
                      (error "Directory '%s' not exists!" dir-cur)))
         (dir-list (directory-files (expand-file-name dir-cur)))
         source-dirP)
    (catch :exit
      (dolist (el dir-list)
        (when (string-match-p "\\.el$" el)
          (setq source-dirP t)
          (throw :exit nil))))
    (when source-dirP
      (byte-recompile-directory dir-cur 0 t))))


;; *** load extra load procedure
(defun entropy/emacs-ext--load-extra ()
  (let ((ext-plists (entropy/emacs-ext--check-inuse-extras)))
    (dolist (el ext-plists)
      (let ((loader (plist-get el :load-predicate)))
        (when (ignore-errors (file-exists-p loader))
          (load loader))))))


;; ** main
(defun entropy/emacs-ext-main ()
  (let ((extras-status (entropy/emacs-ext--check-all-extras)))
    (unless (eq extras-status t)
      (entropy/emacs-ext--extra-prompt-troubel extras-status))
    (if (not (eq extras-status t))
        nil
      (entropy/emacs-ext--load-extra)
      (when (and entropy/emacs-ext-user-specific-load-paths
                 (listp entropy/emacs-ext-user-specific-load-paths))
        (dolist (el entropy/emacs-ext-user-specific-load-paths)
          (when (ignore-errors (file-directory-p el))
            (entropy/emacs-ext--load-path (expand-file-name el)))))
      t)))


;; ** provide
(provide 'entropy-emacs-ext)
