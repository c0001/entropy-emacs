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

;; ** library
;; *** Toggle context structer style
(defun entropy/emacs-elisp-toggle-outline-struct-style (&optional prefix silent)
  "Toggle outline regexp style in elisp source file, ';;;+' as
old-school type, ';; *+' as the mordern one.

PREFIX if non-nil for create new file copy of current buffer to
transformation.

Optional arg: SILENT was a symbol for 'modern' or 'oldschool' for
indicating style type without style type chosen interactively.

For lisp coding aim, always return the transfered buffer.
"
  (interactive "P")
  (let* ((type '("old style" "mordern style"))
         (choice (or (and silent
                          (if (eq silent 'modern)
                              t
                            nil))
                     (and (null silent)
                          (if (string= (completing-read "Choose style: " type nil t) "old style")
                              nil t))))
         (path default-directory)
         (file (format "lisp-toggle-file.[%s].el"
                       (format-time-string "%Y%m%d%H%M%S")))
         (file-create
          (lambda ()
            (with-current-buffer (current-buffer)
              (write-region (point-min) (point-max)
               (expand-file-name
                file path)))))
         (buffer (if prefix (progn (funcall file-create) (find-file-noselect file))
                   (current-buffer)))
         (enable-mode (with-current-buffer buffer major-mode)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward (if choice "^;;\\(;+\\) " "^;; \\(\\*+\\) ") nil t)
          (save-excursion
            (let* ((level-str (match-string 1))
                   (level (length level-str))
                   (head-str (match-string 0))
                   (rep-str (concat ";;" (when choice " ")
                                    (let ((rtn ""))
                                      (dotimes (var level)
                                        (setq rtn (concat rtn (if choice "*" ";"))))
                                      (concat rtn " ")))))
              (replace-match
               rep-str))))
        (when (buffer-file-name)
          (save-buffer))
        (funcall enable-mode)
        (if (interactive-p)
            (let ((judge
                   (yes-or-no-p
                    (format "Create toggled buffer '%s'\nSwitch-To? " buffer))))
              (when judge
                (switch-to-buffer buffer)))
          buffer)))))

;; ** main
;; *** Emacs lisp mode
;; Note: `elisp-mode' was called `emacs-lisp-mode' in <=24
(use-package elisp-mode
  :ensure nil
  :eemacs-mmphc
  (((:enable t)
    (emacs-lisp-mode (nil emacs-lisp-mode-map) t))
   ("IELM"
    (("C-c C-z" ielm "Open IELM"
      :enable t
      :exit t
      :map-inject t))
    "Eval"
    (("C-c C-c" eval-defun "Eval wrapping context"
      :enable t
      :exit t
      :map-inject t)
     ("C-c C-b" eval-buffer "Eval Whole buffer"
      :enable t
      :exit t
      :map-inject t)
     ("C-c M-r" eval-region "Eval Markup Region"
      :enable t
      :exit t
      :map-inject t))))
  :eemacs-mmphca
  (((:enable t)
    (lisp-interaction-mode (elisp-mode lisp-interaction-mode-map)))
   ("Eval"
    (("C-c C-c" eval-defun "Eval wrapping context"
      :enable t
      :exit t
      :map-inject t)
     ("C-c C-b" eval-buffer "Eval Whole Buffer"
      :enable t
      :exit t
      :map-inject t))))
  )

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

(use-package eldoc-eval
  :commands (eldoc-in-minibuffer-mode)
  :preface
  (defun entropy/emacs-lisp-eldoc-eval-minibuffer-map-rejected-advice
      (&rest _)
    "Rebind \"C-@\" key injected by `eldoc-in-minibuffer-mode' to
`minibuffer-local-map' to solve the conflict with
`entropy/emacs-top-key' default value."
    (define-key minibuffer-local-map (kbd "C-@") nil))

  :init
  ;; set large amount to mode line show time for preventing cut the
  ;; show state when noticed for long time, 100 sec was enough.
  (setq eldoc-show-in-mode-line-delay 100)

  (entropy/emacs-lazy-with-load-trail
   eldoc-minibuffer-show
   (with-eval-after-load 'eldoc-eval
     (advice-add 'eldoc-in-minibuffer-mode
                 :after
                 #'entropy/emacs-lisp-eldoc-eval-minibuffer-map-rejected-advice
                 ))))

;; Interactive macro expander
(use-package macrostep
  :commands (macrostep-expand)
  :eemacs-mmphca
  ((((:enable t)
     (emacs-lisp-mode (elisp-mode emacs-lisp-mode-map)))
    ((:enable t)
     (lisp-interaction-mode (elisp-mode lisp-interaction-mode-map))))
   ("Macro"
    (("C-c e" macrostep-expand "Expand Macro At Point"
      :enable t
      :exit t
      :map-inject t)))))

;; Make M-. and M-, work in elisp like they do in slime.
;; `xref' is perfect since 25, so only use in <=24.
(unless (fboundp 'xref-find-definitions)
  (use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
    :commands (turn-on-elisp-slime-nav-mode)
    :bind (:map elisp-slime-nav-mode-map
                ("C-h o" . elisp-slime-nav-describe-elisp-thing-at-point))
    :init
    (entropy/emacs-progn-seq-dolist
     (hook '(emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             ielm-mode-hook))
     (add-hook 'hook #'turn-on-elisp-slime-nav-mode))))

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

;; *** common lisp
(use-package slime
  :commands (slime slime-mode)
  :eemacs-mmphc
  (((:enable t)
    (lisp-mode (nil slime-mode-map) t))
   ("Slime"
    (("C-c p" entropy/emacs-lisp-slime-counsel-desc-symbol
      "Slime Describe Symbols"
      :enable t
      :exit t
      :map-inject t)
     ("C-c C-s" slime-describe-symbol "Slime Describe Symbol At Point"
      :enable t
      :exit t
      :map-inject t)
     ("C-c C-f" slime-describe-function "Slime Describe Func At Point"
      :enable t
      :exit t
      :map-inject t)
     ("C-c M-r" slime-repl "Slime repl" :enable t :map-inject t :exit t))))
  :init

  (setq inferior-lisp-program entropy/emacs-inferior-lisp-program)
  (setq slime-lisp-implementations
        entropy/emacs-slime-lisp-implementations)
  (add-hook 'lisp-mode-hook #'slime-mode)

  :config
  ;; Add more extensions for SLIME
  (dolist (contrib '(slime-repl slime-autodoc))
    (add-to-list 'slime-contribs contrib))

  (defun entropy/emacs-lisp--slime-env-symbols ()
    (slime-simple-completions ""))

  ;; TODO: Show brief symbol doc in ivy rich
  ;; (defun entropy/emacs-lisp--slime-filter-desc-brief (desc)
  ;;   (let (rtn)
  ;;     (when (string-match-p "Documentation:" desc)
  ;;       (with-temp-buffer
  ;;         (insert desc)
  ;;         (goto-char (point-min))
  ;;         (re-search-forward "Documentation:" nil t)
  ;;         (forward-line 1)
  ;;         (setq rtn (replace-regexp-in-string
  ;;                    "^\\s-+" ""
  ;;                    (buffer-substring (point) (progn (end-of-line) (point)))))))
  ;;     (or rtn "")))

  (defun entropy/emacs-lisp-slime-counsel-desc-symbol ()
    (interactive)
    (when (fboundp #'ivy-read)
      (ivy-read "Desc symbol: " (entropy/emacs-lisp--slime-env-symbols)
                :require-match t
                :preselect (slime-symbol-at-point)
                :action #'slime-describe-symbol
                :caller 'entropy/emacs-lisp-slime-counsel-desc-symbol)))

  (with-eval-after-load 'ivy-rich
    (when (fboundp #'entropy/emacs-ivy--ivy-rich-variable-icon)
      (setq ivy-rich-display-transformers-list
            (append
             '(entropy/emacs-lisp-slime-counsel-desc-symbol
               (:columns
                ((entropy/emacs-ivy--ivy-rich-symbol-icon)
                 (ivy-rich-candidate (:width 30)))
                :delimiter "\t"))
             ivy-rich-display-transformers-list))
      ;; renable `ivy-rich-mode' to enable new rich display rule
      (when ivy-rich-mode
        (ivy-rich-mode 0)
        (ivy-rich-mode 1)))))

;; * provide
(provide 'entropy-emacs-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
