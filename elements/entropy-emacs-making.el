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

(when (and (entropy/emacs-ext-main)
           (null entropy/emacs-making--making-out))
  ;; install packages
  (require 'entropy-emacs-package)
  (if (entropy/emacs-package-package-archive-empty-p)
      (entropy/emacs-package-install-all-packages)
    (entropy/emacs-package-install-all-packages)
    (when (yes-or-no-p "Update packages? ")
      (entropy/emacs-package-update-all-packages)))

  ;; make dump file
  (when (and (not (version< emacs-version "27"))
             (yes-or-no-p "Make pdumper file? "))
    (setq entropy/emacs-making--making-out t)
    (entropy/emacs-making--dump-emacs)))

(provide 'entropy-emacs-making)
