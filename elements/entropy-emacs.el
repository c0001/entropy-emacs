(require 'entropy-emacs-custom)

(let ((preinstallp (getenv "EEMACS_INSTALL")))
  (if preinstallp
      (require 'entropy-emacs-install)
    (require 'entropy-emacs-start)))

(provide 'entropy-emacs)
