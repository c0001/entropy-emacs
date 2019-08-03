;;; entropy-emacs-structer.el --- entropy-emacs config of coding structer
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-structure.el
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
;; Coding structer context development using `org-struct-mode'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)


;; ** hs-mode

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :init
  (add-hook 'c-mode-common-hook   #'hs-minor-mode)
  (add-hook 'c++-mode-hook        #'hs-minor-mode)  
  (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  (add-hook 'java-mode-hook       #'hs-minor-mode)
  (add-hook 'lisp-mode-hook       #'hs-minor-mode)
  (add-hook 'perl-mode-hook       #'hs-minor-mode)
  (add-hook 'sh-mode-hook         #'hs-minor-mode)
  (add-hook 'js-mode-hook         #'hs-minor-mode)
  (add-hook 'css-mode-hook        #'hs-minor-mode)
  (add-hook 'php-mode-hook        #'hs-minor-mode)
  (add-hook 'python-mode-hook     #'hs-minor-mode)
  :config

  ;; function for universal code folding
  (defun entropy/emacs-structure-toggle-selective-display (column)
    "Folding coding block relied on indentation column COLUMN."
    (interactive "P")
    (set-selective-display
     (or column
	 (unless selective-display
	   (1+ (current-column))))))

  (defun entropy/emacs-structure-toggle-hiding (column)
    "Using `hs-toggle-hiding' to fold partition coding block."
    (interactive "P")
    (if hs-minor-mode
	(if (condition-case nil
		(hs-toggle-hiding)
	      (error t))
	    (hs-show-all))
      (entropy/emacs-structure-toggle-selective-display column)))

  (global-set-key (kbd "C--") 'entropy/emacs-structure-toggle-hiding)
  (global-set-key (kbd "C-+") 'entropy/emacs-structure-toggle-selective-display))

;; * provide
(provide 'entropy-emacs-structure)
