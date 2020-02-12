(require 'entropy-emacs-defun)
(require 'entropy-emacs-utils)

(defvar entropy/emacs-hydra-top-dispatch-title
  (entropy/emacs-pretty-hydra-make-title
   "eemacs top dispatch" 'faicon "toggle-on"))

(pretty-hydra-define entropy/emacs-hydra-top-dispatch
  (:title
   entropy/emacs-hydra-top-dispatch-title
   :color  amaranth :quit-key "q")
  ("Basic"     ()
   "Highlight" ()
   "Coding"    ()
   "WWW"       ()
   "Theme"     ()))

(entropy/emacs-!set-key
  (kbd "h")
  #'entropy/emacs-hydra-top-dispatch/body)

(provide 'entropy-emacs-hydra-hollow)
