;;; entropy-emacs-defvar.el --- entropy emacs internal variable declaration
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defvar.el
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
;; This file was the collection of `entropy-emacs' internal sharing
;; variables.
;; 
;; * Configuration:
;; 
;; No individually loading designation without `entropy-emacs'.
;; 
;; * Code:

(defvar entropy/emacs-web-development-environment nil
  "Whether using enable web-development envrionment.

This variable is mainly for the judgement button for
`entropy/emacs-browse-url-function' for determined whether to using the
specific browser to visualize current file.")

(defvar entropy/emacs-init-mini-hook ()
  "Hooks for minimal start.")

(defvar entropy/emacs-init-X-hook ()
  "Hooks of entropy-emacs X init.")

(defvar entropy/emacs-lang-locale (car default-process-coding-system)
  "The locale lang.")

(defvar entropy/emacs-window-center-integer 4)

(defvar entropy/emacs-dashboard-buffer-name  "*WELCOM TO ENTROPY-EMACS*"
  "Title of entropy-emacs initial dashboard buffer. ") 

(defvar entropy/emacs-mode-line-sticker ""
  "Sticker for current modeline style")

(defvar entropy/emacs-theme-sticker ""
  "Current theme used for this session.")

(provide 'entropy-emacs-defvar)
