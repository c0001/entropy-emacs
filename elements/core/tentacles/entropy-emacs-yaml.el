;;; entropy-emacs-yaml.el --- entropy-emacs yaml file editor configuration  -*- lexical-binding: t; -*-

;; * Copyright (C) 2019067  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-yaml.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE

;; * Commentary:

;; =entropy-emacs= yaml file editor configuration

;; * Configuration:

;; loading automatically by `entropy-emacs' without hacking warranty.

;; * Code:
(use-package yaml-mode
  :mode "\\.yaml$")


;; * provide
(provide 'entropy-emacs-yaml)
