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
   (java-mode . outshine-mode)
   (php-mode . outshine-mode)
   (python-mode . outshine-mode)
   (web-mode . outshine-mode)
   (css-mode . outshine-mode)      
   (js2-mode . outshine-mode)
   (gitignore-mode . outshine-mode)
   (gitconfig-mode . outshine-mode)
   (conf-colon-mode . outshine-mode)
   (conf-desktop-mode . outshine-mode)
   (conf-javaprop-mode . outshine-mode)
   (conf-ppd-mode . outshine-mode)
   (conf-space-mode . outshine-mode)
   (conf-toml-mode . outshine-mode)
   (conf-unix-mode . outshine-mode)
   (conf-windows-mode . outshine-mode)
   (conf-xdefaults-mode . outshine-mode))
  :bind
  (("C-<tab>" . outshine-cycle-buffer)
   :map org-mode-map
   ("C-c M-t" . nil)
   ("C-c M-e" . nil)
   ("C-c M-p" . nil))
  
  :init
  (setq outshine-max-level 100)
  (entropy/emacs-lazy-initial-for-hook
   '(emacs-lisp-mode-hook)
   "outshine-mode" "outshine-mode"
   (outshine-mode +1)
   (add-hook 'emacs-lisp-mode-hook 'outshine-mode))
  
  :config
  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer
    (or (outline-on-heading-p) (bobp)
        (error "Using it out of the headline was not supported.")))

  (defun outshine-set-outline-regexp-base ()
    "Return the actual outline-regexp-base.

Notice: redefined specific for entropy-emacs

Preventing recursive face rending for level keywords that loal
binding to `outshine-regexp-base-char' while using traditional
structure type for elisp."
    (if (and
         (not (outshine-modern-header-style-in-elisp-p))
         (eq major-mode 'emacs-lisp-mode))
        (progn
          (setq outshine-enforce-no-comment-padding-p t)
          (setq outshine-regexp-base
                outshine-oldschool-elisp-outline-regexp-base)
          (setq-local outshine-regexp-base-char ";"))
      (setq outshine-enforce-no-comment-padding-p nil)
      (setq outshine-regexp-base
            outshine-default-outline-regexp-base)
      (setq-local outshine-regexp-base-char
                  (default-value 'outshine-regexp-base-char))))

  (defun entropy/emacs-structure--outshine-advice-1 (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (when (string-match-p " +$" rtn)
        (setq
         rtn
         (replace-regexp-in-string
          "\\( +\\)$" "" rtn)))
      rtn))
  
  (advice-add 'outshine-calc-outline-regexp
              :around
              #'entropy/emacs-structure--outshine-advice-1)

  (defun entropy/emacs-structure--outshine-fontify-headlines (orig-func &rest orig-args)
    "Calculate heading regexps for font-lock mode."
    (let* ((outline-regexp (car orig-args))
           (outline-rgxp (substring outline-regexp 0 -7))
           (heading-1-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{1\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-2-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{2\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-3-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{3\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-4-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{4\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-5-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{5\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-6-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{6\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-7-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{7\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (heading-8-regexp
            (format "%s%s%s%s%s%s"
                    outline-rgxp
                    "\\{8\\}" (format "[^%s]" outshine-regexp-base-char) "\\(.*"
                    (if outshine-fontify-whole-heading-line "\n?" "")
                    "\\)"))
           (font-lock-new-keywords
            `((,heading-1-regexp 1 'outshine-level-1 t)
              (,heading-2-regexp 1 'outshine-level-2 t)
              (,heading-3-regexp 1 'outshine-level-3 t)
              (,heading-4-regexp 1 'outshine-level-4 t)
              (,heading-5-regexp 1 'outshine-level-5 t)
              (,heading-6-regexp 1 'outshine-level-6 t)
              (,heading-7-regexp 1 'outshine-level-7 t)
              (,heading-8-regexp 1 'outshine-level-8 t))))

      (add-to-list 'outshine-font-lock-keywords font-lock-new-keywords)
      (font-lock-add-keywords nil font-lock-new-keywords)
      (outshine-font-lock-flush)))
  (advice-add 'outshine-fontify-headlines
              :around
              #'entropy/emacs-structure--outshine-fontify-headlines))

;; * provide
(provide 'entropy-emacs-structure)
