;;; File name: init-game.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** main
(use-package tetris
  :ensure nil
  :config
  (defun entropy/tetris-off-display-line-numbers ()
    "If enable `global-display-line-numbers-mode', turn it off in
`tetris-mode' ."
    (if (and (not (version< emacs-version "26")) display-line-numbers-mode)
        (display-line-numbers-mode 0)))
  (advice-add 'tetris :after #'entropy/tetris-off-display-line-numbers))

(provide 'entropy-emacs-game)
