;;; entropy-emacs-batch.el --- entropy-emacs non-interactive toolchains
;;
;; * Copyright (C) 20190920  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-batch.el
;; Package-Version: 0.1.0
;; Version:       file-version
;; Created:       2019-09-20 18:18:18
;; Compatibility: GNU Emacs 25.2;
;; Package-Requires: ((emacs "25.2") (cl-lib "0.5"))
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
;; =entropy-emacs= native batch tools for non-interactive session,
;; usually for 'MAKE'.
;;
;; * Configuration:
;;
;; Designed for =entropy-emacs= only.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-message)
(require 'entropy-emacs-package)
(require 'entropy-emacs-coworker)
(require 'entropy-emacs-ext)

;; ** defvar
;; ** library
;; *** section prompting
(defmacro entropy/emacs-batch--prompts-for-ext-install-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for extensions installing ...")
      (blue    "=================================================="))
     ,@body))

(defmacro entropy/emacs-batch--prompts-for-dump-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "           Section for emacs dump ...")
      (blue    "=================================================="))
     (when (yes-or-no-p "Dump eemacs? ")
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-ext-update-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for extensions updating ...")
      (blue    "=================================================="))
     (when (yes-or-no-p "Do package update? ")
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-coworkers-installing-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for coworkers installing ...")
      (blue    "=================================================="))
     (when (yes-or-no-p "Install them? ")
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-native-compile
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for native compiling `package-user-dir' ...")
      (blue    "=================================================="))
     (when (yes-or-no-p "Native compile `package-user-dir'? ")
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-eemacs-ext-build-repo-install
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for install entropy-emacs-extensions stable build ...")
      (blue    "=================================================="))
     (when (yes-or-no-p "install eemacs-ext stable build? ")
       ,@body)))

;; *** dump emacs
(defun entropy/emacs-batch--dump-emacs-core ()
  (let ((dump-file (expand-file-name
                    (format "eemacs_%s.pdmp" (format-time-string "%Y%m%d%H%M%S"))
                    entropy/emacs-user-emacs-directory)))
    (setq entropy/emacs-fall-love-with-pdumper t)
    (require 'entropy-emacs-start)
    (dump-emacs-portable dump-file)))

(defun entropy/emacs-batch--dump-emacs ()
  (unless (not (version< emacs-version "27"))
    (entropy/emacs-message-do-error
     (red
      "You just can portable dump emacs while emacs version upon 27, abort")))
  (entropy/emacs-batch--dump-emacs-core))

;; *** install coworkers

(defun entropy/emacs-batch--install-coworkers (&optional prefix)
  (interactive "P")
  (let ((count 1)
        (newhost (when prefix
                   (read-directory-name "Coworker Host: " nil nil t)))
        (task-list '(("tern" . entropy/emacs-coworker-check-tern-server)
                     ("web-lsp" . entropy/emacs-coworker-check-web-lsp)
                     ("js-lsp" . entropy/emacs-coworker-check-js-lsp)
                     ("json-lsp" . entropy/emacs-coworker-check-json-lsp)
                     ("xml-lsp" . entropy/emacs-coworker-check-xml-lsp)
                     ("php-lsp" . entropy/emacs-coworker-check-php-lsp)
                     ("bash-lsp" . entropy/emacs-coworker-check-bash-lsp)
                     ("python-lsp" . entropy/emacs-coworker-check-python-lsp)
                     ("python-pyright" . entropy/emacs-coworker-check-pyright-lsp)
                     ("cmake-lsp" . entropy/emacs-coworker-check-cmake-lsp)
                     ("java-lsp"  . entropy/emacs-coworker-check-java-lsp)
                     ("pwsh-lsp" . entropy/emacs-coworker-check-pwsh-lsp)
                     ("python-lsp-ms" . entropy/emacs-coworker-check-pyls-ms-lsp)
                     ("clangd-lsp" . entropy/emacs-coworker-check-clangd-lsp))))
    (entropy/emacs-with-coworker-host
      newhost
      (dolist (el task-list)
        (entropy/emacs-message-do-message
         "%s %s %s"
         (cyan (format "🏠 %s: " count))
         (yellow (format "'%s'" (car el)))
         (green "installing ..."))
        (funcall (cdr el))
        (cl-incf count)))))

;; *** backup `package-user-dir'
(defun entropy/emacs-batch--backup-extensions ()
  (let* ((host-path (file-name-directory package-user-dir))
         (archive (file-name-nondirectory package-user-dir))
         (time-stamp (format-time-string "%Y%m%d%H%M%S"))
         (archive-bck (concat archive "_" time-stamp))
         (bcknew (expand-file-name archive-bck host-path)))
    (entropy/emacs-message-do-message
     (green "Backup `package-user-dir' ..."))
    (copy-directory package-user-dir
                    bcknew)
    (entropy/emacs-message-do-message
     "%s %s %s"
     (green "Backup to")
     (yellow (format "'%s'" bcknew))
     (green "completely!"))))

;; *** native compile  `package-user-dir'

(defun entropy/emacs-batch--around-advice-for-native-compile (orig-func &rest orig-args)
  (let ((file-or-func (caar comp-files-queue)))
    (entropy/emacs-message-do-message
     "%s%s%s"
     "↳ "
     (yellow (format "⚠ native compiling for %s :"
                     (if (functionp file-or-func)
                         "function"
                       (if (ignore-errors (file-exists-p file-or-func))
                           "file"
                         "unkown object")
                       )))
     (if (ignore-errors (file-exists-p file-or-func))
         (file-name-nondirectory file-or-func)
       (format "%s" file-or-func)))
    (apply orig-func orig-args)))

(defun entropy/emacs-batch--native-compile-package-dir ()
  (let* ((comp-verbose 0))
    (unwind-protect
        (progn
          (advice-add 'comp-run-async-workers
                      :around #'entropy/emacs-batch--around-advice-for-native-compile)
          (native-compile-async package-user-dir 'recursively)
          (while (or comp-files-queue
                     (> (comp-async-runnings) 0))
            (sleep-for 1)))
      (progn
        (advice-remove
         'comp-run-async-workers
         #'entropy/emacs-batch--around-advice-for-native-compile)))))

;; *** get entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo

(defun entrop/emacs-batch--install-eemacs-ext-stable-build-repo-core ()
  (let* ((url entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url)
         (host entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
         (dec-host
          (make-temp-name (expand-file-name
                           "eemacs-ext-stable-repo-ext_"
                           entropy/emacs-stuffs-topdir)))
         (tmp-name (make-temp-name (expand-file-name
                                    "eemacs-ext-stable-repo-archive_"
                                    entropy/emacs-stuffs-topdir)))
         download-cbk)
    ;; download archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Downloading eemacs-ext-stable repo ... "))
    (setq download-cbk
          (entropy/emacs-network-download-file
           url tmp-name
           nil nil
           (lambda (file)
             (let* ((inhibit-read-only t)
                    (stick-hash
                     entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-archive-sha256sum)
                    (buff (create-file-buffer file))
                    (cur-hash nil))
               (with-current-buffer buff
                 (erase-buffer)
                 (insert-file-contents-literally file))
               (setq cur-hash
                     (secure-hash 'sha256 buff))
               (unless (string= cur-hash stick-hash)
                 (error "Sha256 hash verify for file <%s> \
faild with hash '%s' which must match '%s'"
                        file cur-hash stick-hash))
               t))))
    (unless (eq (symbol-value download-cbk) 'success)
      (entropy/emacs-message-do-error
       "%s: %s"
       (red "Download fatal with")
       (format "%s" (get download-cbk 'error-type))))
    (entropy/emacs-message-do-message
     "%s"
     (green "Download eemacs-ext-stable repo done!"))
    ;; extract archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Extract eemacs-ext-stable repo ..."))
    (make-directory dec-host t)
    (entropy/emacs-archive-dowith
     'txz tmp-name dec-host :extract)
    (let ((dirname
           (entropy/emacs-file-path-parser
            (car
             (entropy/emacs-list-subdir
              dec-host))
            'non-trail-slash)))
      (rename-file dirname host)
      ;; generate inited indicator
      (with-temp-buffer
        (write-file
         entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path-comprehensive-indicator
         )))
    (entropy/emacs-message-do-message
     "%s"
     (green "Get eemacs-ext-stable repo done!"))))


(defun entrop/emacs-batch--install-eemacs-ext-stable-build-repo ()
  (let ((host entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
        (did-p t))
    (when (file-exists-p host)
      (setq did-p
            (yes-or-no-p
             (format "Remove old install (%s)? "
                     host)))
      (and did-p
           (rename-file
            host
            (concat host
                    (format ".bak_%s"
                            (format-time-string "%Y%m%d%H%M%S"))))))
    (if did-p
        (entrop/emacs-batch--install-eemacs-ext-stable-build-repo-core)
      (entropy/emacs-message-do-message
       "%s"
       (yellow "Abort!")))))


;; ** interactive
(when (entropy/emacs-ext-main)
  (let ((type (entropy/emacs-is-make-session)))
    (cond
     ((equal type "Install")
      (entropy/emacs-batch--prompts-for-ext-install-section
       (entropy/emacs-package-install-all-packages)))
     ((equal type "Install-Coworkers")
      (entropy/emacs-batch--prompts-for-coworkers-installing-section
       (entropy/emacs-batch--install-coworkers)))
     ((equal type "Install-Eemacs-Ext-Build")
      (entropy/emacs-batch--prompts-for-eemacs-ext-build-repo-install
       (entrop/emacs-batch--install-eemacs-ext-stable-build-repo)))
     ((equal type "Update")
      (if (entropy/emacs-package-package-archive-empty-p)
          (entropy/emacs-message-do-error
           (red "You haven't install packages, can not do updating, abort!"))
        (entropy/emacs-batch--prompts-for-ext-update-section
         (entropy/emacs-batch--backup-extensions)
         (entropy/emacs-package-update-all-packages))))
     ((equal type "Dump")
      ;; make dump file
      (entropy/emacs-batch--prompts-for-dump-section
       (entropy/emacs-batch--dump-emacs)))
     ((and (ignore-errors (native-comp-available-p))
           (equal type "native-comp"))
      (entropy/emacs-batch--prompts-for-native-compile
       (entropy/emacs-batch--native-compile-package-dir)))
     (t
      (entropy/emacs-message-do-error
       (red (format "Unknown making type '%s'" type)))))))

;; * provide
(provide 'entropy-emacs-batch)
