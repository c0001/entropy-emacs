;;; entropy-portableapps.el --- Apps toggle based on portableapps.com
;;
;;; Copyright (C) 20190911  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-portableapps
;; Package-Version: 0.1.0
;; Created:       2018
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
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
;; [[https://portableapps.com/][Portableapps]] platform gives dozen of portale applications those for
;; using without any localization configuration pollution. This package
;; was the simple portableapps management both of query and open frontend
;; interface written by emacs lisp.
;;
;;;; Requirements
;;
;; emacs 25
;;
;;;; Installation
;;
;; The =use-package= configuration form:
;;
;; #+BEGIN_SRC elisp
;;   (use-package entropy-portableapps
;;     :ensure nil
;;     :load-path "path-to-your-load-path"
;;     :commands (entropy/poapps-query-open)
;;     :bind (("C-M-<f11>" . entropy/poapps-query-open)))
;; #+END_SRC
;;
;;;; Mechanism
;;
;; The portableapps platform gathered all portable applications into one
;; root directory, assumption here as dir =./Portableapps/=:
;;
;; #+BEGIN_EXAMPLE
;;   .
;;   ├── 7-ZipPortable
;;   ├── calibrePortable
;;   ├── CloudMusicPortable
;;   ├── CPU-ZPortable
;;   ├── FeedNotifierPortable
;;   ├── FeedRollerPortable
;;   ├── FileZillaPortable
;;   ├── FoxitReaderPortable
;;   ├── fscPortable
;;   ├── GIMPPortable
;;   ├── GoogleChromePortable
;;   ├── GPU-ZPortable
;;   ├── InkscapePortable
;;   ├── IrfanViewPortable
;;   ├── KeePassProPortable
;;   ├── LibreOfficePortable
;;   ├── LightscreenPortable
;;   ├── MPC-HCPortable
;;   ├── OBSPortable
;;   ├── PCI-ZPortable
;;   ├── PortableApps.com
;;   ├── PortableApps.comInstaller
;;   ├── PortableApps.comLauncher
;;   ├── ProcessHackerPortable
;;   ├── QuiteRSSPortable
;;   ├── RegshotPortable
;;   ├── RufusPortable
;;   ├── ScreenToGifPortable
;;   ├── SQLiteDatabaseBrowserPortable
;;   ├── TCPViewPortable
;;   ├── TelegramDesktopPortable
;;   ├── ThunderPortable
;;   ├── TimPortable
;;   └── WeChatPortable
;; #+END_EXAMPLE
;;
;;
;; Each portable application has the folder tree form as:
;; #+BEGIN_EXAMPLE
;;  .
;; ├── App
;; ├── Data
;; ├── Other
;; └── PortableApps.comInstaller.exe
;; #+END_EXAMPLE
;;
;; Indication that there's exit the top dir startup binary (the launcher)
;; which can be detected through the subdir list filter, then the
;; launcher collection was built as the query candidates basic on the
;; =Ivy= framework.
;;
;; Binaries can be file of the extension as "bat","exe".
;;
;;; Configuration
;;
;; As the mechanism section mentioned, there's one core customized
;; variable =entropy/poapps-root= to specified as the portabaleapps root
;; directory.
;;
;; Another one was =entropy/poapps-exclude-list= to give the treatment
;; for some no need binaries.
;;
;;
;;; Code:

(declare-function w32-shell-execute "w32fns")

(defgroup entropy-portableapps-group nil
  "Customized group for `entropy-portableapps'"
  :group 'extensions)

(defcustom entropy/poapps-root nil
  "Portableapps folder root dir using for `entropy-portableapps'
package."
  :type '(choice directory
                 (const nil))
  :group 'entropy-portableapps-group)

(defcustom entropy/poapps-exclude-list nil
  "List of apps' name that would be used with
`entropy/poapps-query-open'"
  :type '(choice (repeat (string :tag "AppName"))
                 (const nil))
  :group 'entropy-portableapps-group)

(defmacro entropy/poapps--return-as-default-directory (&rest body)
  "Return a valid `default-directory' value equalized with BODY's value.

This operation exists since `default-directory' has its meaningful
special constructed contention but most of times we did not obey thus
both of our neglects and misusing.

See `default-directory' for its convention details."
  (let ((dfd-sym (make-symbol "dfd-rtn-val")))
    `(let ((,dfd-sym (progn ,@body)))
       (unless (stringp ,dfd-sym)
         (signal 'wrong-type-argument
                 (list 'stringp
                       (format "directory name: %s" ,dfd-sym))))
       (unless (or (string-empty-p ,dfd-sym)
                   (not (directory-name-p ,dfd-sym)))
         (setq ,dfd-sym (directory-file-name ,dfd-sym)))
       (file-name-as-directory ,dfd-sym))))

(defun entropy/poapps--list-dir-lite (dir-root &optional not-abs)
  "Return an alist of fsystem nodes as:

#+begin_src elisp
'((dir . \"a-dir\")
  (file . \"a.txt\"))
#+end_src

where the car of each elements is the node type with follow symols to
indicate that:

- 'file': the node is an file (or an symbolic to an regular file)
- 'dir':  the node is an directory (or an symbolic to an directory)

The node sort ordered by `string-lessp'

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT.
"
  (let (rtn-full rtn-lite rtn-attr)
    (setq rtn-full (directory-files dir-root (not not-abs)))
    (dolist (el rtn-full)
      ;; filter the . and ..
      (if (not (string-match-p "\\(\\\\\\|/\\)?\\(\\.\\|\\.\\.\\)$" el))
          (push el rtn-lite)))
    (if rtn-lite
        (progn
          (dolist (el rtn-lite)
            (if (file-directory-p (expand-file-name el dir-root))
                (push `(dir . ,el) rtn-attr)
              (push `(file . ,el) rtn-attr)))
          rtn-attr)
      nil)))

(defun entropy/poapps--list-dir-subdirs (dir-root &optional not-abs)
  "List subdir of root dir DIR-ROOT, ordered by `string-lessp'.

If optional arg NOT-ABS is non-nil then each node is relative to
the DIR-ROOT."
  (let ((dirlist (entropy/poapps--list-dir-lite dir-root not-abs))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (eq 'dir (car el))
                (push (cdr el) rtn)))
          (if rtn
              (reverse rtn)
            nil))
      nil)))

(defun entropy/poapps--make-name-alist (olist &optional naming-func)
  "Make a named alist from a list OLIST.

The named alist is a alist which each car of the element is a
specified name object which defaultly is a unique order number for
current position of OLIST if optional NAMING-FUNC was unset.

Optional argument NAMEING-FUNC is a function provided by youself
which has the single argument to accepting one extracted element
of OLIST and return the name object of current element.

Demo:

If OLIST is (1 2 3 4 5), NAMING-FUNC is '(lambda (x) (+ 1 x))
then retun name-alist:

((2 . 1) (3. 2) (4. 3) (5. 4) (6. 5))
"
  (let* (rtn
         (count -1))
    (dolist (el olist)
      (push `(,(if (functionp naming-func)
                   (funcall naming-func el)
                 (cl-incf count))
              .
              ,el)
            rtn))
    (if rtn
        (reverse rtn)
      (error "[entropy/poapps--make-name-alist]: error occurred"))))

(defun entropy/poapps-list-apps ()
  "List portable apps under folder `entropy/poapps-root'."
  (let ((dirs (entropy/poapps--list-dir-subdirs entropy/poapps-root))
        Apps)
    (if (not dirs) (error (format "None apps in '%s'" entropy/poapps-root)))
    (dolist (el0 dirs)
      (let ((sub (entropy/poapps--list-dir-lite el0)))
        (when sub
          (dolist (el1 sub)
            (if (string-match-p "\\(\\.exe\\|\\.bat\\)$" (cdr el1))
                (push (cdr el1) Apps))))))
    (if Apps Apps nil)))

(defun entropy/poapps-make-name-alist ()
  "Make name alist for apps vector, relying on function
`entropy/poapps--make-name-alist'."
  (let* ((olist (entropy/poapps-list-apps))
         (nfunc '(lambda (x) (file-name-nondirectory x)))
         (teml (entropy/poapps--make-name-alist olist nfunc))
         (rtn teml))
    (if entropy/poapps-exclude-list
        (dolist (el entropy/poapps-exclude-list)
          (if (assoc el teml)
              (setq rtn (delete (assoc el teml) rtn)))))
    rtn))

;;;###autoload
(defun entropy/poapps-query-open ()
  "Query and open portableapps."
  (interactive)
  (let* ((apps (entropy/poapps-make-name-alist))
         (choice (ivy-read "Query portableapps: " apps
                           :require-match t))
         (stick (cdr (assoc choice apps))))
    (let ((default-directory
            (entropy/poapps--return-as-default-directory
             (or (getenv "TEMP") (error "Without windows TEMP env var set!")))))
      (w32-shell-execute "open" stick))))

(provide 'entropy-portableapps)
