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
(use-package java-ts-mode
  :ensure nil
  :commands (java-ts-mode))
(use-package dart-mode
  :commands (dart-mode)
  :mode "\\.dart$")

;; * provide
(provide 'entropy-emacs-java)
