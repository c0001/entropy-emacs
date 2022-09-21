;; EEMACS_MAINTENANCE: we must forbidden native-comp for polluting
;; eemacs current usage status i.e eemacs usually has its specified
;; native-comp mechanism.
(when (featurep 'comp)
  (setq native-comp-always-compile nil
        native-comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation-deny-list
        ;; we must excluded eemacs code for native comp
        '("entropy-emacs-.*"
          "liberime*"
          "fakecygpty"
          "\\.?dir-locals")))
;; load the core subroutine
(entropy/emacs-common-require-feature 'entropy-emacs-defun)
(provide 'entropy-emacs-pure-env)
