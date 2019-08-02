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


;; ** orgstruct-mode

(use-package org
  :ensure nil
  :commands (orgstruct-mode)
  :diminish orgstruct-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'orgstruct-mode)
  (add-hook 'sh-mode-hook #'orgstruct-mode)
  (add-hook 'c-mode-hook #'orgstruct-mode)
  (add-hook 'c++-mode-hook #'orgstruct-mode)
  (add-hook 'css-mode-hook #'orgstruct-mode)
  (add-hook 'python-mode-hook #'orgstruct-mode)
  (add-hook 'web-mode-hook #'orgstruct-mode)
  (add-hook 'js2-mode-hook #'orgstruct-mode)
  (add-hook 'gitignore-mode-hook #'orgstruct-mode)
  (setq orgstruct-heading-prefix-regexp
        "\\(\\( \\|	\\)*;;\\( \\|	\\)?\\|\\( \\|	\\)*#\\( \\|	\\)?\\|\\( \\|	\\)*\\/\\*\\( \\|	\\)?\\|\\( \\|	\\)*\\/\\/\\( \\|	\\)?\\|\\( \\|	\\)*<!--\\( \\|	\\)*\\)")
  

  ;; unbinding this key for give it to `eval-buffer' for `elisp-mode' and `lisp-interaction-mode'.
  (advice-add 'orgstruct-mode :after #'(lambda (&optional arg)
                                         (define-key orgstruct-mode-map (kbd "C-c C-b") nil)))

  :config

  (defvar entropy/emacs-structure--orgstruct-jumping-head-regexp
    "^\\(\\( \\|	\\)*;;\\( \\|	\\)?\\*\\|^\\( \\|	\\)*#\\( \\|	\\)?\\*\\|^\\( \\|	\\)*\\/\\*\\( \\|	\\)?\\*\\|^\\( \\|	\\)*\\/\\/\\( \\|	\\)?\\*\\)"
    "The regexp for jumping heading by:

    - `entropy/emacs-fold-org-struct'
    - `entropy/emacs-previous-orgstruct-headline'
    - `entropy/emacs-next-orgstruct-headline'
    - `entropy/emacs-up-orgstruct-headline'"
    )

  (defun entropy/emacs-structure-fold-org-struct ()
    "Fold the file with orgstruct format struture heading style
like ';; **' in elisp file"
    (interactive)
    (goto-char (point-min))
    (re-search-forward
     entropy/emacs-structure--orgstruct-jumping-head-regexp)
    (orgstruct-hijacker-org-shifttab-3 t))
  
  (defun entropy/emacs-structure-previous-orgstruct-headline ()
    "Jumping to the previous orgstruct headline."
    (interactive)
    (beginning-of-line)
    (re-search-backward
     entropy/emacs-structure--orgstruct-jumping-head-regexp))
  
  (defun entropy/emacs-structure-next-orgstruct-headline ()
    "Jumping to the next orgstruct headline."
    (interactive)
    (end-of-line)
    (re-search-forward
     entropy/emacs-structure--orgstruct-jumping-head-regexp))

  (defun entropy/emacs-structure-up-orgstruct-headline ()
    "Jumping to the up-level orgstruct headline."
    (interactive)
    (next-line)
    (beginning-of-line)
    (entropy/emacs-structure-previous-orgstruct-headline)
    (orgstruct-hijacker-outline-up-heading-1 t))
  
  (defun entropy/emacs-structure--org-struct-mode-hook ()
    "Hooks for pusing `entropy/emacs-structure-previous-orgstruct-headline',
`entropy/emacs-structure-next-orgstruct-headline',
`entropy/emacs-structure-next-orgstruct-headline' to org-struct-mode-hook."
    (define-key orgstruct-mode-map (kbd "C-<tab>") 'entropy/emacs-structure-fold-org-struct)
    (define-key orgstruct-mode-map (kbd "C-c C-p") 'entropy/emacs-structure-previous-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-n") 'entropy/emacs-structure-next-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-u") 'entropy/emacs-structure-up-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-f") nil))
  (add-hook 'orgstruct-mode-hook 'entropy/emacs-structure--org-struct-mode-hook))



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
