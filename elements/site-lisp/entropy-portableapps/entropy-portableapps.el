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
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (entropy-common-library "0.1.0"))
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
;; Depended on =entropy-common-library= as various other entropy emacs
;; extensions.
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

(require 'entropy-common-library)

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

(defun entropy/poapps-list-apps ()
  "List portable apps under folder `entropy/poapps-root'."
  (let ((dir (entropy/cl-list-subdir entropy/poapps-root))
        Apps)
    (if (not dir) (error (format "None apps in '%s'" entropy/poapps-root)))
    (dolist (el0 dir)
      (let ((sub (entropy/cl-list-dir-lite el0)))
        (when sub
          (dolist (el1 sub)
            (if (string-match-p "\\(\\.exe\\|\\.bat\\)$" (cdr el1))
                (push (cdr el1) Apps))))))
    (if Apps Apps nil)))

(defun entropy/poapps-make-name-alist ()
  "Make name alist for apps vector, relying on function
`entropy/cl-make-name-alist'."
  (let* ((olist (entropy/poapps-list-apps))
         (nfunc '(lambda (x) (file-name-nondirectory x)))
         (teml (entropy/cl-make-name-alist olist nfunc))
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
    (let ((default-directory (getenv "TEMP")))
      (w32-shell-execute "open" stick))))

(provide 'entropy-portableapps)
