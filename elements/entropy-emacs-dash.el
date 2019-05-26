;;; File name: init-dash.el ---> for entropy-emacs
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

;; * main
(defun entropy/emacs-dash--use-zeal-at-point ()
  "Use-package zeal-at-point."
  (if (not (executable-find "zeal"))
      (warn "Can not find 'zeal' in path.")
    (use-package zeal-at-point
      :ensure nil
      :commands (zeal-at-point
                 zeal-at-point-set-docset
                 zeal-at-point-search)
      :bind
      (("C-c o" . zeal-at-point)
       ("C-c M-o" . zeal-at-point-search)))))


(when (and sys/win32p entropy/emacs-win-portable-zeal-enable)
  (entropy/emacs-dash--use-zeal-at-point))

(when sys/linux-x-p
  (entropy/emacs-dash--use-zeal-at-point))


(provide 'entropy-emacs-dash)
