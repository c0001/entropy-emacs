;;; entropy-emacs-library.el --- entropy emacs underlying library for other part
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-library.el
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
;; As what mentioned in `entropy-emacs.el' the core bridge component
;; of `entropy-emacs', excluded for the top basic part i.e. the
;; public variable and function declaration files, other part of
;; `entropy-emacs' is independently. As that the case for that if
;; some part sharing one extension who roling as the sharing
;; underlying extension, this file does as so.
;; 
;; * Configuration:
;; 
;; Loading automatically by `entropy-emacs'. May be useless for other
;; usages.
;; 
;; * Code:

(require 'font-lock+)

(use-package all-the-icons
  :commands
  (all-the-icons-icon-for-file
   all-the-icons-icon-for-mode
   all-the-icons--icon-info-for-buffer
   all-the-icons-install-fonts
   all-the-icons-insert))

(use-package eldoc-eval
  :preface (defvar eldoc-in-minibuffer-mode nil)
  :commands (eldoc-in-minibuffer-mode
             eldoc-eval-expression)
  :init (setq eldoc-eval-preferred-function 'eval-expression))

(use-package shrink-path
  :commands
  (shrink-path--dirs-internal
   shrink-path--truncate
   shrink-path-dirs
   shrink-path-expand
   shrink-path-file
   shrink-path-file-expand
   shrink-path-file-mixed
   shrink-path-prompt))



(provide 'entropy-emacs-library)
