(require 'entropy-emacs-custom)

(if (entropy/emacs-is-make-session)
    (require 'entropy-emacs-making)
  (require 'entropy-emacs-start))

(provide 'entropy-emacs)
