(require 'entropy-emacs-message)
(require 'entropy-emacs-package)
(require 'entropy-emacs-ext)

(defvar entropy/emacs-making--making-out nil)

(defun entropy/emacs-making--dump-emacs ()
  (let ((dump-file (expand-file-name
                    (format "eemacs_%s.pdmp" (format-time-string "%Y%m%d%H%M%S"))
                    user-emacs-directory)))
    (setq entropy/emacs-fall-love-with-pdumper t
          entropy/emacs-is-make-session t)
    (require 'entropy-emacs-start)
    (dump-emacs-portable dump-file)))

(defun entropy/emacs-making-dump-emacs (&optional no-confirm)
  (unless (not (version< emacs-version "27"))
    (entropy/emacs-message-do-error
     (red
      "You just can portable dump emacs while emacs version upon 27, abort")))
  (when (or no-confirm
            (yes-or-no-p "Make pdumper file? "))
    (setq entropy/emacs-making--making-out t)
    (entropy/emacs-making--dump-emacs)))

(when (and (entropy/emacs-ext-main)
           (null entropy/emacs-making--making-out))
  (let ((type (entropy/emacs-is-make-session)))
    (cond
     ((equal type "All")
      ;; install packages
      (if (entropy/emacs-package-package-archive-empty-p)
          (entropy/emacs-package-install-all-packages)
        (entropy/emacs-package-install-all-packages)
        (when (yes-or-no-p "Update packages? ")
          (entropy/emacs-package-update-all-packages)))

      ;; make dump file
      (entropy/emacs-making-dump-emacs))
     ((equal type "Install")
      (entropy/emacs-package-install-all-packages))
     ((equal type "Update")
      (if (entropy/emacs-package-package-archive-empty-p)
          (entropy/emacs-message-do-error
           (red "You haven't install packages, can not do updating, abort!"))
        (entropy/emacs-package-update-all-packages)))
     (t
      (entropy/emacs-message-do-error
       (red (format "Unknown making type '%s'" type)))))))

(provide 'entropy-emacs-making)
