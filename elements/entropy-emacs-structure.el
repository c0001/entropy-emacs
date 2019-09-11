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
  :hook ((c-mode-common . hs-minor-mode)
         (c++-mode . hs-minor-mode)  
         (emacs-lisp-mode . hs-minor-mode)
         (java-mode . hs-minor-mode)
         (lisp-mode . hs-minor-mode)
         (perl-mode . hs-minor-mode)
         (sh-mode . hs-minor-mode)
         (js-mode . hs-minor-mode)
         (css-mode . hs-minor-mode)
         (php-mode . hs-minor-mode)
         (python-mode . hs-minor-mode))
  :config
  (global-set-key (kbd "C--") 'entropy/emacs-structure-toggle-hiding)
  (global-set-key (kbd "C-+") 'entropy/emacs-structure-toggle-selective-display))

;; ** yafolding

(use-package yafolding
  :if (eq entropy/emacs-code-folding-type 'yafolding)
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
   ("C-c M-p" . nil)
   ("C-c M-y" . nil))
  
  :init
  (entropy/emacs-lazy-initial-for-hook
   '(emacs-lisp-mode-hook)
   "outshine-mode" "outshine-mode"
   (outshine-mode +1)
   (add-hook 'emacs-lisp-mode-hook 'outshine-mode))
  
  :config

  (setq outshine-max-level 100)
  
  (setq outshine-default-outline-regexp-base
        (format "[%s]\\{1,%d\\}"
                outshine-regexp-base-char outshine-max-level))

  (setq outshine-oldschool-elisp-outline-regexp-base
        (format "[;]\\{1,%d\\}" outshine-max-level))
  
  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer
    (or (outline-on-heading-p) (bobp)
        (error "Using it out of the headline was not supported.")))

  (defun entropy/emacs-structure--outshine-advice-for-outline-regexp-calc-of-head-stick
      (orig-func &rest orig-args)
    (concat "^"
            (apply orig-func orig-args)))

  (cl-loop for advice in '(entropy/emacs-structure--outshine-advice-for-outline-regexp-calc-of-head-stick)
           do (advice-add 'outshine-calc-outline-regexp :around advice))

  (defmacro entropy/emacs-structure--outshine-with-nontrailing-space-otreg (&rest body)
    "The specific subroutine for outshine 'demote' or 'promote' head level.

The trailing white-space was the outline 'demote' and 'promote'
bug trace core in outshine, that the origin mechinsm of outline
to re-generate the head level sign was using 'substring' function
manipulate the outline-regexp matced group of the asterisk
sequence, the trailing white space will be recognized as the
'level' sign char, thus any demote will mess as the scene.

     (defun outline-invent-heading (head up)
       \"Create a heading by using heading HEAD as a template.
     When UP is non-nil, the created heading will be one level above.
     Otherwise, it will be one level below.\"
       (save-match-data
         ;; Let's try to invent one by repeating or deleting the last char.
         (let ((new-head (if up (substring head 0 -1)
         >>>>>>>>>>>>>>>>>>>>>>>*_notice here_ 
                           (concat head (substring head -1)))))
           (if (string-match (concat \"\\`\\(?:\" outline-regexp \"\\)\")
                             new-head)
               ;; Why bother checking that it is indeed higher/lower level ?
               new-head
             ;; Didn't work, so ask what to do.
             (read-string (format-message \"%s heading for `%s': \"
                                          (if up \"Parent\" \"Demoted\") head)
                          head nil nil t)))))"
    `(let ((outline-regexp (let ((rtn outline-regexp))
                             (when (string-match-p " +$" rtn)
                               (setq
                                rtn
                                (replace-regexp-in-string
                                 "\\( +\\)$" "" rtn)))
                             rtn)))
       ,@body))
  
  (defun entropy/emacs-structure--outshine-back-to-head ()
    (entropy/emacs-structure--outshine-with-nontrailing-space-otreg
     (outline-back-to-heading)))
  
  (defun entropy/emacs-structure--outshine-demote (&optional which)
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
	            (entropy/emacs-structure--outshine-back-to-head)
	            (if current-prefix-arg nil 'subtree))))
    (entropy/emacs-structure--outshine-with-nontrailing-space-otreg
     (funcall 'outline-demote which)))

  (defun entropy/emacs-structure--outshine-promote (&optional which)
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
	            (entropy/emacs-structure--outshine-back-to-head)
	            (if current-prefix-arg nil 'subtree))))
    (entropy/emacs-structure--outshine-with-nontrailing-space-otreg
     (funcall 'outline-promote which)))

  (outshine-define-key outshine-mode-map
    (kbd "M-S-<left>") 'entropy/emacs-structure--outshine-promote
    (outline-on-heading-p))
  (outshine-define-key outshine-mode-map
    (kbd "M-S-<right>") 'entropy/emacs-structure--outshine-demote
    (outline-on-heading-p))
    
  (defun outshine-set-outline-regexp-base ()
    "Return the actual outline-regexp-base.

Notice: redefined specific for entropy-emacs

Preventing recursive face rending for level keywords that local
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

  (defun entropy/emacs-structure--outshine-gen-face-keywords (outline-regexp times)
    (let ((outline-regex-head (substring outline-regexp 0 -10))
          func rtn)
      (setq func
            (lambda (level)
              (list (format "%s%s%s%s%s%s"
                            outline-regex-head
                            (format "\\{%d\\}" level)
                            (format "[^%s#]" outshine-regexp-base-char)
                            "\\(.*"
                            (if outshine-fontify-whole-heading-line "\n?" "")
                            "\\)")
                    1
                    `(quote ,(intern (format "outshine-level-%d" level)))
                    t)))
      (cl-loop for level from 1 to times
               do (push (funcall func level) rtn))
      (nreverse rtn)))
  
  (defun entropy/emacs-structure--outshine-fontify-headlines (orig-func &rest orig-args)
    "Calculate heading regexps for font-lock mode."
    (let* ((outline-regexp (car orig-args))
           (font-lock-new-keywords
            (entropy/emacs-structure--outshine-gen-face-keywords outline-regexp 8)))
      (add-to-list 'outshine-font-lock-keywords font-lock-new-keywords)
      (font-lock-add-keywords nil font-lock-new-keywords)
      (outshine-font-lock-flush)))
  (advice-add 'outshine-fontify-headlines
              :around
              #'entropy/emacs-structure--outshine-fontify-headlines))

;; * provide
(provide 'entropy-emacs-structure)
