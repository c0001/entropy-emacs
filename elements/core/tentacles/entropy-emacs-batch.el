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
(defun entropy/emacs-batch--prompts-for-ext-install-section ()
  (entropy/emacs-message-do-message
   "\n%s\n%s\n%s\n"
   (blue    "==================================================")
   (yellow "       Section for extensions installing ...")
   (blue    "==================================================")))

(defun entropy/emacs-batch--prompts-for-dump-section ()
  (entropy/emacs-message-do-message
   "\n%s\n%s\n%s\n"
   (blue    "==================================================")
   (yellow "           Section for emacs dump ...")
   (blue    "==================================================")))

(defun entropy/emacs-batch--prompts-for-ext-update-section ()
  (entropy/emacs-message-do-message
   "\n%s\n%s\n%s\n"
   (blue    "==================================================")
   (yellow "       Section for extensions updating ...")
   (blue    "==================================================")))

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
                     ("php-lsp" . entropy/emacs-coworker-check-php-lsp)
                     ("bash-lsp" . entropy/emacs-coworker-check-bash-lsp)
                     ("python-lsp" . entropy/emacs-coworker-check-python-lsp)
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

;; ** interactive
(when (and (entropy/emacs-ext-main)
           (null entropy/emacs-make-session-make-out))
  (let ((type (entropy/emacs-is-make-session)))
    (cond
     ((equal type "All")
      ;; install packages
      (if (entropy/emacs-package-package-archive-empty-p)
          (progn
            (entropy/emacs-batch--prompts-for-ext-install-section)
            (entropy/emacs-package-install-all-packages))
        (entropy/emacs-batch--prompts-for-ext-install-section)
        (entropy/emacs-package-install-all-packages)
        (entropy/emacs-batch--prompts-for-ext-update-section)
        (when (yes-or-no-p "Update packages? ")
          (entropy/emacs-batch--backup-extensions)
          (entropy/emacs-package-update-all-packages)))
      ;; install coworkes
      (entropy/emacs-batch--prompts-for-coworkers-installing-section
       (entropy/emacs-batch--install-coworkers))
      ;; make dump file
      (setq entropy/emacs-make-session-make-out t)
      (entropy/emacs-batch--prompts-for-dump-section)
      (when (yes-or-no-p "Make pdumper file? ")
        (entropy/emacs-batch--dump-emacs)))
     
     ((equal type "Install")
      (entropy/emacs-batch--prompts-for-ext-install-section)
      (entropy/emacs-package-install-all-packages)
      (entropy/emacs-batch--prompts-for-coworkers-installing-section
       (entropy/emacs-batch--install-coworkers)))
     
     ((equal type "Update")
      (if (entropy/emacs-package-package-archive-empty-p)
          (entropy/emacs-message-do-error
           (red "You haven't install packages, can not do updating, abort!"))
        (entropy/emacs-batch--prompts-for-ext-update-section)
        (entropy/emacs-batch--backup-extensions)
        (entropy/emacs-package-update-all-packages)))
     (t
      (entropy/emacs-message-do-error
       (red (format "Unknown making type '%s'" type)))))))

;; * provide
(provide 'entropy-emacs-batch)
