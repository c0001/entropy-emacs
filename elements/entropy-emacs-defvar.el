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
(defvar entropy/emacs-web-development-environment nil
  "Whether using enable web-development envrionment.

This variable is mainly for the judgement button for
`entropy/emacs-browse-url-function' for determined whether to using the
specific browser to visualize current file.")

(defvar entropy/emacs-init-mini-hook ()
  "Hooks for minimal start.")

(defvar entropy/emacs-init-X-hook ()
  "Hooks of entropy-emacs X init.")

(defvar entropy/emacs-lang-locale (car default-process-coding-system)
  "The locale lang.")

(defvar entropy/emacs-get-dired-fpath-log nil)

(defvar entropy/emacs-window-center-integer 4)

(defvar entropy/emacs-dashboard-buffer-name  "*WELCOM TO ENTROPY-EMACS*"
  "Title of entropy-emacs initial dashboard buffer. ") 

(defvar entropy/emacs-mode-line-sticker ""
  "Sticker for current modeline style")

(provide 'entropy-emacs-defvar)
