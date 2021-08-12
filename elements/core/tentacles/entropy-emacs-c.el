;;; entropy-emacs-c.el --- entroy emacs C config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-c.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;;
;; * Commentary:
;;
;; This file was config of emacs major mode for =C= referrence,
;; config of C file coding style, major mode ehancement and other
;; minor tools.
;;
;; Some special tools for WIN32 platform using Msys2 *NIX environment
;; emulator, and common config for cross platform.
;;
;; * Configuration:
;;
;; There's no support to loading this file out of entropy-emacs unless
;; the hacking way.
;;
;; * Code:

;; ** require

;; ** main
(use-package cc-mode
  :ensure nil
  :preface
  (defun entropy/emacs/c-mode/after-change-func
      (&rest _)
    ;; fontify the buffer context round at `current-point' with idle
    ;; style
    (entropy/emacs-run-at-idle-immediately
     idle-fontify-c-type-buffer
     (let ((cur-pos (point))
           (cur-line (string-to-number (format-mode-line "%l"))))
       (c-font-lock-fontify-region
        (save-excursion
          (forward-line -5)
          (point))
        (save-excursion
          (forward-line 5)
          (point))))))

  (defun entropy/emacs-c-cc-mode-common-set ()
    (c-set-style "bsd")
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    ;; EEMACS_MAINTENANCE: this patch is not under fully tested
    ;; whether influence the other functionality for `c-mode'.
    ;; ====== remove the lag core of `cc-mode' =====
    ;; ----------> 1. fistly we remove the command post triggers
    (remove-hook 'before-change-functions #'c-before-change t)
    (remove-hook 'after-change-functions #'c-after-change t)
    ;; ----------> 2. then remove the according facilities of (1)
    (remove-hook 'change-major-mode-hook #'c-leave-cc-mode-mode)
    (remove-hook 'font-lock-mode-hook #'c-after-font-lock-init t)
    (setq-local font-lock-extend-after-change-region-function
                nil
                font-lock-fontify-region-function
                'font-lock-default-fontify-region)
    ;; add idle buffer fontify hook
    (add-hook 'after-change-functions
              #'entropy/emacs/c-mode/after-change-func
              nil t))

  :init
  (add-hook 'c-mode-common-hook
            #'entropy/emacs-c-cc-mode-common-set)
  :config
  )

;; ** provide
(provide 'entropy-emacs-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
