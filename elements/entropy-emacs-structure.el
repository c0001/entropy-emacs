;;; File name: init-structure.el ---> for entropy-emacs
;;
;; Copyright (c) 2017 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
  (add-hook 'sh-mode-hook  #'orgstruct-mode)
  (add-hook 'c-mode-hook  #'orgstruct-mode)
  (add-hook 'c++-mode-hook  #'orgstruct-mode)  
  (add-hook 'css-mode-hook  #'orgstruct-mode)
  (add-hook 'python-mode-hook #'orgstruct-mode)
  (add-hook 'web-mode-hook #'orgstruct-mode)
  (add-hook 'js2-mode-hook #'orgstruct-mode)
  (setq orgstruct-heading-prefix-regexp
        "\\(\\( \\|	\\)*;;\\( \\|	\\)?\\|\\( \\|	\\)*#\\( \\|	\\)?\\|\\( \\|	\\)*\\/\\*\\( \\|	\\)?\\|\\( \\|	\\)*\\/\\/\\( \\|	\\)?\\|\\( \\|	\\)*<!--\\( \\|	\\)*\\)")
  

  ;; unbinding this key for give it to `eval-buffer' for `elisp-mode' and `lisp-interaction-mode'.
  (advice-add 'orgstruct-mode :after #'(lambda (&optional arg)
                                         (define-key orgstruct-mode-map (kbd "C-c C-b") nil)))

  :config

  (defvar entropy/emacs-orgstruct-jumping-head-regexp
    "^\\(\\( \\|	\\)*;;\\( \\|	\\)?\\*\\|^\\( \\|	\\)*#\\( \\|	\\)?\\*\\|^\\( \\|	\\)*\\/\\*\\( \\|	\\)?\\*\\|^\\( \\|	\\)*\\/\\/\\( \\|	\\)?\\*\\)"
    "The regexp for jumping heading by:

    - `entropy/emacs-fold-org-struct'
    - `entropy/emacs-previous-orgstruct-headline'
    - `entropy/emacs-next-orgstruct-headline'
    - `entropy/emacs-up-orgstruct-headline'"
    )

  (defun entropy/emacs-fold-org-struct ()
    "Fold the file with orgstruct format struture heading style
like ';; **' in elisp file"
    (interactive)
    (goto-char (point-min))
    (re-search-forward
     entropy/emacs-orgstruct-jumping-head-regexp)
    (orgstruct-hijacker-org-shifttab-3 t))
  
  (defun entropy/emacs-previous-orgstruct-headline ()
    "Jumping to the previous orgstruct headline."
    (interactive)
    (beginning-of-line)
    (re-search-backward
     entropy/emacs-orgstruct-jumping-head-regexp))
  
  (defun entropy/emacs-next-orgstruct-headline ()
    "Jumping to the next orgstruct headline."
    (interactive)
    (end-of-line)
    (re-search-forward
     entropy/emacs-orgstruct-jumping-head-regexp))

  (defun entropy/emacs-up-orgstruct-headline ()
    "Jumping to the up-level orgstruct headline."
    (interactive)
    (next-line)
    (beginning-of-line)
    (entropy/emacs-previous-orgstruct-headline)
    (orgstruct-hijacker-outline-up-heading-1 t))
  
  (defun entropy/emacs-org-struct-mode-hook ()
    "Hooks for pusing `entropy/emacs-previous-orgstruct-headline',
`entropy/emacs-next-orgstruct-headline',
`entropy/emacs-next-orgstruct-headline' to org-struct-mode-hook."
    (define-key orgstruct-mode-map (kbd "C-<tab>") 'entropy/emacs-fold-org-struct)
    (define-key orgstruct-mode-map (kbd "C-c C-p") 'entropy/emacs-previous-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-n") 'entropy/emacs-next-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-u") 'entropy/emacs-up-orgstruct-headline)
    (define-key orgstruct-mode-map (kbd "C-c C-f") nil))
  (add-hook 'orgstruct-mode-hook 'entropy/emacs-org-struct-mode-hook))



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
  (defun entropy/emacs-toggle-selective-display (column)
    "Folding coding block relied on indentation column COLUMN."
    (interactive "P")
    (set-selective-display
     (or column
	 (unless selective-display
	   (1+ (current-column))))))

  (defun entropy/emacs-toggle-hiding (column)
    "Using `hs-toggle-hiding' to fold partition coding block."
    (interactive "P")
    (if hs-minor-mode
	(if (condition-case nil
		(hs-toggle-hiding)
	      (error t))
	    (hs-show-all))
      (entropy/emacs-toggle-selective-display column)))

  (global-set-key (kbd "C--") 'entropy/emacs-toggle-hiding)
  (global-set-key (kbd "C-+") 'entropy/emacs-toggle-selective-display))

;; * provide
(provide 'entropy-emacs-structure)
