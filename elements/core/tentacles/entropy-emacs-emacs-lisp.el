;;; entropy-emacs-lisp.el --- entropy-emacs lisp development config
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-emacs-lisp.el
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
;; The lisp development environment config for `entropy-emacs',
;; code defination jumping, auto-completion, doc-string quick view
;; for lisp family developement cases.
;;
;; For now it just has the elisp partition, and common-lisp config
;; will arriving until when I've leanring them done.
;;
;; * Configuration:
;;
;; Loading by `entropy-emacs' automatically without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** main

;; Emacs lisp mode
;; Note: `elisp-mode' was called `emacs-lisp-mode' in <=24
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)
              ("M-\\" . company-dabbrev-code)))

(use-package lisp-interaction-mode
  :ensure nil
  :bind (:map lisp-interaction-mode-map
              ("M-\\" . company-dabbrev-code)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))


;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :init
  ;; Enable Eldoc in lisp modes in 24
  ;; `global-eldoc-mode' is enabled by default in 25.
  (unless (fboundp 'global-eldoc-mode)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    ielm-mode-hook
                    eval-expression-minibuffer-setup-hook))
      (add-hook hook #'eldoc-mode))))

;; Interactive macro expander
(use-package macrostep
  :commands (macrostep-expand)
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Make M-. and M-, work in elisp like they do in slime.
;; `xref' is perfect since 25, so only use in <=24.
(unless (fboundp 'xref-find-definitions)
  (use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
    :commands (turn-on-elisp-slime-nav-mode)
    :bind (:map elisp-slime-nav-mode-map
                ("C-h o" . elisp-slime-nav-describe-elisp-thing-at-point))
    :init (dolist (hook '(emacs-lisp-mode-hook
                          lisp-interaction-mode-hook
                          ielm-mode-hook))
            (add-hook hook #'turn-on-elisp-slime-nav-mode))))

;; Semantic code search for emacs lisp
(use-package elisp-refs
  :commands
  (elisp-refs-function
   elisp-refs-macro
   elisp-refs-variable
   elisp-refs-special
   elisp-refs-symbol))

(defun entropy/emacs-elisp-recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

;; Toggle context structer style

(defun entropy/emacs-elisp-toggle-outline-struct-style (&optional prefix)
  "Toggle outline regexp style in elisp source file, ';;;+' as
old-school type, ';; *+' as the mordern one.

PREFIX if non-nil for old-school style."
  (interactive "P")
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (re-search-forward (if prefix "^;;\\(;+\\) " "^;; \\(\\*+\\) ") nil t)
      (save-excursion
        (let* ((level-str (match-string 1))
               (level (length level-str))
               (head-str (match-string 0))
               (rep-str (concat ";;" (when prefix " ")
                                (let ((rtn ""))
                                  (dotimes (var level)
                                    (setq rtn (concat rtn (if prefix "*" ";"))))
                                  (concat rtn " ")))))
          (replace-match
           rep-str))))))


;; * provide
(provide 'entropy-emacs-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
