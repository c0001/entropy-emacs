;;; entropy-emacs-bash.el --- entropy-emacs shell script development configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-bash.el
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
;; `entropy-emacs' bash script major-mode configuration.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require

;; ** main
(use-package sh-script
  :commands (sh-mode)
  :mode (("\\.sh$" . sh-mode)
         ("\\.bash$" . sh-mode)
         ("\\.dash$" . sh-mode))
  :config
  ;; cancel binding to 'C-c C-l' since be conflicts with
  ;; `entropy/emacs-tools-vertical-to-top' global binding
  (define-key sh-mode-map (kbd "C-c C-l") nil)

  ;; Dsiable internal map injection which may let user confuse for
  ;; using since they used un-frequently.
  (let ((binds `("\C-c("
                 "\C-c\C-w"
                 "\C-c\C-u"
                 "\C-c\C-t"
                 "\C-c\C-s"
                 "\C-c\C-r"
                 "\C-c\C-o"
                 "\C-c\C-l"
                 "\C-c\C-i"
                 "\C-c\C-f"
                 "\C-c\C-c"
                 "\C-c?"
                 "\C-c="
                 "\C-c<"
                 "\C-c>"
                 "\C-c\C-\\"
                 "\C-c+"
                 "\C-\M-x"
                 "\C-c\C-x"
                 "\C-c\C-n"
                 "\C-c\C-d"
                 "\C-c\C-z"
                 "\C-c:"
                 )))
    (dolist (bind binds)
      (define-key sh-mode-map bind nil)))

    )


;; * provide
(provide 'entropy-emacs-bash)
