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
  (defun entropy/emacs-c-cc-mode-common-set ()
    (c-set-style "bsd")
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4))
  :init
  (add-hook 'c-mode-common-hook
            #'entropy/emacs-c-cc-mode-common-set))

;; ** provide
(provide 'entropy-emacs-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
