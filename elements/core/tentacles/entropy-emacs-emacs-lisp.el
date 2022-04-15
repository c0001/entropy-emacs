;;; entropy-emacs-lisp.el --- entropy-emacs lisp development config  -*- lexical-binding: t; -*-
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
                   (rep-str (concat ";;" (when choice " ")
                                    (let ((rtn ""))
                                      (dotimes (_ level)
                                        (setq rtn (concat rtn (if choice "*" ";"))))
                                      (concat rtn " ")))))
              (replace-match
               rep-str))))
        (when (buffer-file-name)
          (save-buffer))
        (funcall enable-mode)
        (if (called-interactively-p 'any)
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
  ((((:enable t :defer (:data (:adfors (emacs-lisp-mode-hook) :adtype hook :pdumper-no-end t)))
     (emacs-lisp-mode (elisp-mode emacs-lisp-mode-map) t))
    ((:enable t :defer (:data (:adfors (emacs-lisp-mode-hook) :adtype hook :pdumper-no-end t)))
     (lisp-interaction-mode (elisp-mode lisp-interaction-mode-map) t)))
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
      :map-inject t)
     ("C-x C-e" eval-last-sexp "Evaluate sexp before point"
      :enable t
      :exit t
      :map-inject t))))
  :config
  ;; disbale `eval-last-sexp' in `ctl-x-map' for reducing mistakes
  (define-key ctl-x-map "\C-e" nil)
  )

(use-package eldoc-eval
  :commands (eldoc-in-minibuffer-mode
             eldoc-eval-expression)
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

  (setq eldoc-eval-preferred-function 'eval-expression)

  (entropy/emacs-lazy-load-simple
      eldoc-eval
    (advice-add 'eldoc-in-minibuffer-mode
                :after
                #'entropy/emacs-lisp-eldoc-eval-minibuffer-map-rejected-advice
                ))
  )

;; Interactive macro expander
(use-package macrostep
  :commands (macrostep-expand)
  :eemacs-mmphca
  ((((:enable t :defer (:data (:adfors
                               (emacs-lisp-mode
                                lisp-interaction-mode)
                               :adtype after
                               :pdumper-no-end t)))
     (emacs-lisp-mode (elisp-mode emacs-lisp-mode-map)))
    ((:enable t :defer (:data (:adfors
                               (emacs-lisp-mode
                                lisp-interaction-mode)
                               :adtype after
                               :pdumper-no-end t)))
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
     (add-hook 'hook #'elisp-slime-nav-mode))))

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

;; TODO: add unique `company-box-icons-functions' for slime
(use-package slime
  :commands (slime
             slime-mode
             entropy/emacs-lisp--slime-env-symbols
             entropy/emacs-lisp-slime-counsel-desc-symbol
             )
;; **** hydra
  :eemacs-mmphc
  (((:enable t :defer t)
    (lisp-mode (nil slime-mode-map) t))
   ("Slime Basic"
    (("b d"
      (:pretty-hydra-cabinet
       (:data
        "Describe symbol"
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
          :map-inject t)))
       :other-rest-args
       ((slime slime-mode-map)))
      "Describe lisp symbols"
      :enable t :exit t)
     ("b n"
      (:pretty-hydra-cabinet
       (:data
        "Defination Search"
        (("M-." slime-edit-definition
          "Edit the definition of the function called at point."
          :enable t :exit t :map-inject t)
         ("M-," slime-pop-find-definition-stack
          "Pop the definition stack to go back from a definition."
          :enable t :exit t :map-inject t)))
       :other-rest-args
       ((slime slime-mode-map)))
      "Defination Search"
      :enable t :exit t)
     ("C-c M-r" slime-repl "Slime repl"
      :enable t :map-inject t :exit t)
     ("C-c C-k"
      slime-compile-and-load-file
      "Compile and load the current buffer’s file."
      :enable t :exit t :map-inject t)
     ("C-c M-k"
      slime-compile-file
      "Compile (but not load) the current buffer’s file."
      :enable t :exit t :map-inject t)
     ("C-c C-c"
      slime-compile-defun
      "Compile the top-level form at point."
      :enable t :exit t :map-inject t))))

;; **** init
  :init

  (setq inferior-lisp-program entropy/emacs-inferior-lisp-program)
  (setq slime-lisp-implementations
        entropy/emacs-slime-lisp-implementations)
  (add-hook 'lisp-mode-hook #'slime-mode)

  ;; ivy-rich feature add
  (add-to-list 'entropy/emacs-ivy-rich-extra-display-transformers-list
               '(entropy/emacs-lisp-slime-counsel-desc-symbol
                 (:columns
                  (((lambda (&rest _)
                      (all-the-icons-octicon
                       "gear" :height 0.9 :v-adjust -0.05
                       :face 'success)))
                   (ivy-rich-candidate (:width 30)))
                  :delimiter "\t")))

;; **** config
  :config
;; ***** Add more extensions for SLIME
  (dolist (contrib '(slime-repl slime-autodoc))
    (add-to-list 'slime-contribs contrib))

;; ****** more emacs like interactioin
;; ******* symbol list
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
  )

;; * provide
(provide 'entropy-emacs-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
