;;; File name: init-defvar.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary
;;
;;  This package provide the init defined variable for using with
;;  entropy-emacs's main config.
;;
;; * Code:
(defvar entropy/web-development-environment nil
  "Whether using enable web-development envrionment.

This variable is mainly for the judgement button for
`entropy/browse-url-function' for determined whether to using the
specific browser to visualize current file.")


(defvar entropy/init-mini-hook ()
  "Hooks for minimal start.")


(defvar entropy/init-X-hook ()
  "Hooks of entropy-emacs X init.")


(provide 'entropy-emacs-defvar)
