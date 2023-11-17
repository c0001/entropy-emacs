;; load the core subroutine
(entropy/emacs-common-require-feature 'entropy-emacs-defun)
(entropy/emacs-common-require-feature 'entropy-emacs-package)
(entropy/emacs-package-common-start)
;; hollows for extra use-package keywords defination
(entropy/emacs-common-require-feature 'entropy-emacs-hydra-hollow)

;; provide
(message "eemacs pure env init done")
(provide 'entropy-emacs-pure-env)
