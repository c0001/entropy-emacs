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


;; ** libraries
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

;; ** hs-mode

(use-package hideshow
  :if (eq entropy/emacs-code-folding-type 'native)
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
  (global-set-key (kbd "C--") 'entropy/emacs-structure-toggle-hiding)
  (global-set-key (kbd "C-+") 'entropy/emacs-structure-toggle-selective-display))

;; ** yafolding

(use-package yafolding
  :if (eq entropy/emacs-code-folding-type 'yafolding)
  :hook ((c-mode-common-hook   . hs-minor-mode)
         (c++-mode-hook        . hs-minor-mode)  
         (emacs-lisp-mode-hook . hs-minor-mode)
         (java-mode-hook       . hs-minor-mode)
         (lisp-mode-hook       . hs-minor-mode)
         (perl-mode-hook       . hs-minor-mode)
         (sh-mode-hook         . hs-minor-mode)
         (js-mode-hook         . hs-minor-mode)
         (css-mode-hook        . hs-minor-mode)
         (php-mode-hook        . hs-minor-mode)
         (python-mode-hook     . hs-minor-mode))
  :commands (yafolding-toggle-element
             yafolding-show-all)
  :bind (("C--" . entropy/emacs-structure-yaf-toggle)
         ("C-=" . entropy/emacs-structure-yaf-show-all))
  :config
  (defvar entropy/emacs-structure--yafolding-jumping-modes '(emacs-lisp-mode lisp-interaction-mode))
  (defun entropy/emacs-structure-yaf-toggle (column)
    (interactive "P")
    (if (member major-mode entropy/emacs-structure--yafolding-jumping-modes)
        (progn
          (hs-minor-mode 1)
          (funcall #'entropy/emacs-structure-toggle-hiding nil))
      (funcall #'yafolding-toggle-element)))

  (defun entropy/emacs-structure-yaf-show-all ()
    (interactive)
    (if (member major-mode entropy/emacs-structure--yafolding-jumping-modes)
        (progn
          (hs-minor-mode 1)
          (funcall 'hs-show-all))
      (funcall #'yafolding-show-all))))

;; ** outshine-mode

(use-package outshine
  :hook
  ((sh-mode . outshine-mode)
   (c-mode . outshine-mode)
   (c++-mode . outshine-mode)
   (css-mode . outshine-mode)
   (python-mode . outshine-mode)
   (web-mode . outshine-mode)
   (js2-mode . outshine-mode)
   (gitignore-mode . outshine-mode))
  :custom (outshine-max-level 100)
  :bind
  (("C-<tab>" . outshine-cycle-buffer)
   :map org-mode-map
   ("C-c M-t" . nil)
   ("C-c M-e" . nil))
  
  :init

  ;; lazy loading for orgstruct for elisp mode for preventing loading
  ;; org when the *scratch* buffer created with `lisp-interaction-mode'.
  (defun entropy/emacs-org--elispMode-orgstruct-enable ()
    "Enable orgstruct for `emacs-lisp-mode' in needed occasion.

Unload it when the first init is done."
    (unless (member 'outshine-mode emacs-lisp-mode-hook)
      (add-hook 'emacs-lisp-mode-hook #'outshine-mode)
      (defun entropy/emacs-org--elispMode-orgstruct-enable ()
        "Enable orgstruct for `emacs-lisp-mode' in needed occasion.

Unload it when the first init is done."
        t))
    (unless (bound-and-true-p outshine-mode)
      (outshine-mode)))

  (defun entropy/emacs-org--elispMode-orgstruct-PostCommand-hook ()
    "lazy loading the orgstruct coding style for elisp mode."
    (add-hook 'post-command-hook #'entropy/emacs-org--elispMode-orgstruct-enable nil t))

  (entropy/emacs-lazy-load-simple 'elisp-mode
    (add-hook 'emacs-lisp-mode-hook #'entropy/emacs-org--elispMode-orgstruct-PostCommand-hook))
  
  :config
  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer
    (or (outline-on-heading-p) (bobp)
        (error "Using it out of the headline was not supported."))))

;; * provide
(provide 'entropy-emacs-structure)
