;;; entropy-emacs-zeal.el --- entropy-emacs external API query config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-zeal.el
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
;; External api query config for `entropy-emacs' based on [[https://github.com/zealdocs/zeal][zeal]] the
;; mac dash open source port.;;
;; * Configuration:
;;
;; loading by `entropy-emacs' automatically without hacking warranty.
;;
;; * Code:

;; ** require

;; * main
(use-package zeal-at-point
  :if (and (executable-find "zeal")
           sys/is-graphic-support)
  :commands (zeal-at-point
             zeal-at-point-set-docset
             zeal-at-point-search)
  :bind
  (("C-c o" . zeal-at-point)
   ("C-c M-o" . zeal-at-point-search)))

;; * provide
(provide 'entropy-emacs-zeal)
