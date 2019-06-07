;;; entropy-emacs-game.el --- entropy-emacs emacs built-in games configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-game.el
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
;; `entropy-emacs' configuration for emacs built-in games.
;;
;; * Configuration:
;;
;; Loading automatcally by `entropy-emacs' without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** main
(use-package tetris
  :ensure nil
  :config
  (defun entropy/emacs-game--tetris-off-display-line-numbers ()
    "If enable `global-display-line-numbers-mode', turn it off in
`tetris-mode' ."
    (if (and (not (version< emacs-version "26")) display-line-numbers-mode)
        (display-line-numbers-mode 0)))
  (advice-add 'tetris :after #'entropy/emacs-game--tetris-off-display-line-numbers))

(provide 'entropy-emacs-game)
