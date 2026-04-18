;;; entropy-emac-java.el --- entropy-emacs Java configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-go.el
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
;; Java development configuration for `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automaticaly by `entropy-emacs' without hacking warranty.
;;
;; * Code:
;; ** Java
(entropy/emacs--inner-use-package java-ts-mode
  :ensure nil
  :eemacs-if
  (bound-and-true-p entropy/emacs-ide-is-treesit-generally-adapted-p)
  ;; escape byte-compile warning
  :eemacs-with-no-require (not (treesit-ready-p 'java t))
  :commands (java-ts-mode))

;; ** dart
(use-package dart-mode
  :commands (dart-mode)
  :mode "\\.dart$")

;; ** kotlin
(entropy/emacs--inner-use-package kotlin-ts-mode
  :eemacs-if
  (bound-and-true-p entropy/emacs-ide-is-treesit-generally-adapted-p)
  ;; escape byte-compile warning
  :eemacs-with-no-require (not (treesit-ready-p 'kotlin t))
  :commands (kotlin-ts-mode))

(use-package kotlin-mode
  :commands (kotlin-mode)
  :mode "\\.kt$")

;; * provide
(provide 'entropy-emacs-java)
