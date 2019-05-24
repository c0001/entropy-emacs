;;; File name: init-yas.el ---> for entropy-emacs
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

;; ** yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-expand)
  :bind (("M-i" . entropy/emacs-yas-enable-or-expand))
  :init
  (defun entropy/emacs-yas-enable-or-expand (&rest args)
    "Auto enable `yas-global-mode' when not as it and call
`yas-expand'."
    (interactive)
    (require 'yasnippet)
    (cond
     ((not yas-global-mode)
      (yas-global-mode)
      (yas-expand))
     (t
      (yas-expand))))
  
  :config
  (use-package yasnippet-snippets
    :commands (yasnippet-snippets-initialize)
    :init
    (with-eval-after-load 'yasnippet
      (yasnippet-snippets-initialize)))

  (add-to-list 'yas-snippet-dirs entropy/emacs-yas-dir)

  ;; disable tab key in `yas-minor-mode' which will make conflict with `orgstruct-mode'
  ;; `https://stackoverflow.com/questions/14066526/unset-tab-binding-for-yasnippet'
  (progn
    (define-key yas-minor-mode-map [(tab)]        nil)
    (define-key yas-minor-mode-map (kbd "TAB")    nil)
    (define-key yas-minor-mode-map (kbd "<tab>")  nil)))

;; ** auto-yasnippet
;;    This is a hybrid of keyboard macros and yasnippet. You create the snippet on the go, usually
;;    to be used just in the one place. It's fast, because you're not leaving the current buffer,
;;    and all you do is enter the code you'd enter anyway, just placing ~ where you'd like yasnippet
;;    fields and mirrors to be.
;;
;;
;; --- A basic example
;;     Suppose we want to write:
;;
;;     #+BEGIN_EXAMPLE
;;     count_of_red = get_total("red");
;;     count_of_blue = get_total("blue");
;;     count_of_green = get_total("green");
;;     #+END_EXAMPLE
;;
;;     We write a template, using =~= to represent variables that we want to replace:
;;
;;     ~count_of_~red = get_total("~red");~
;;
;;     Call =aya-create= with point on this line, and the template is converted to a value we want:
;;
;;     ~count_of_red = get_total("red");~
;;
;;     Then call =aya-expand= and you can 'paste' additional instances of the template. Yasnippet is
;;     active, so you can tab between placeholders as usual.
;;
;;     #+BEGIN_EXAMPLE
;;     count_of_red = get_total("red");
;;     count_of_ = get_total("");
;;     #+END_EXAMPLE
;;
;; --- Inline text
;;
;;     ~ replaces the symbol after it. If you want to replace arbitrary text, use Emacs-style
;;     backticks:
;;
;;     ~`red'_total = get_total("`red'_values");~

(use-package auto-yasnippet
  :commands (aya-create aya-expand)
  :bind (("H-w" . aya-create)
         ("H-e" . aya-expand)))

;; * provide
(provide 'entropy-emacs-yas)
