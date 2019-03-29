;;; File name: init-custom.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:

(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

(let ((nav entropy/custom-common-file)
      (cus entropy/custom-navigate-file))
  (if (file-exists-p nav)
      (load nav)
    (when (file-exists-p cus)
      (setq custom-file cus)
      (load cus))))

(provide 'entropy-emacs-custom)
