;; load the core subroutine
(entropy/emacs-common-require-feature 'entropy-emacs-defconst)
(entropy/emacs-common-require-feature 'entropy-emacs-defvar)
(entropy/emacs-common-require-feature 'entropy-emacs-defun)
(entropy/emacs-common-require-feature 'entropy-emacs-package)
(entropy/emacs-package-common-start)
;; for extra APIs
(entropy/emacs-common-require-feature 'entropy-emacs-utils)
;; hollows for extra use-package keywords defination
(entropy/emacs-common-require-feature 'entropy-emacs-hydra-hollow)

;; TODO: BODYs

;; provide
(message "eemacs pure env init done")
(provide 'entropy-emacs-pure-env)
