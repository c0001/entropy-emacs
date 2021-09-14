;;; entropy-emacs-yas.el --- entropy-emacs yasnippet configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-yas.el
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
;; Yasnippet configuration for `entropy-emacs'.
;;
;; * Configuration:
;;
;; Uisng for `entropy-emacs' only.
;;
;; * Code:


;; ** require

(entropy/emacs-hydra-hollow-category-common-individual-define
 'yasnippet-uniform
 (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
  'yasnippet-uniform)
 '("Basic" nil
   "Auto yasnippet" nil))

(entropy/emacs-hydra-hollow-add-for-top-dispatch
 '("Basic"
   (("b y"
     (:eval
      (entropy/emacs-hydra-hollow-category-common-individual-get-caller
       'yasnippet-uniform))
     "Yasnippet Actions"
     :enable t :exit t))))

;; ** yasnippet

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode yas-expand)
  :eemacs-indhca
  (((:enable t)
    (yasnippet-uniform))
   ("Basic"
    (("M-i" entropy/emacs-yas-enable-or-expand
      "Expand current point content with yasnippet"
      :enable t
      :exit t
      :global-bind t)
     ("i" yas-insert-snippet "Choose a snippet to expand"
      :enable t :exit t)
     ("n" yas-new-snippet "Pop to writing a snippet"
      :enable t :exit t)
     ("v" yas-visit-snippet-file "Choose a snippet to edit"
      :enable t :exit t))))

  :init
  (defun entropy/emacs-yas-enable-or-expand (&rest args)
    "Auto enable `yas-minor-mode' when not as it and call
`yas-expand'."
    (interactive)
    (when (not (bound-and-true-p yas-minor-mode))
      (unless (featurep 'yasnippet)
        (require 'yasnippet))
      (yas-minor-mode 1))
    (yas-expand))

  :config
  ;; remove original internal const setting for 'snippets' dir
  ;; auto-created in `user-emacs-directory' since its an const set in
  ;; the `yasnippet' source but we want it be customizable, thus we
  ;; invoke `entropy/emacs-yas-dir' instead.
  (setq yas-snippet-dirs
        (remove yas--default-user-snippets-dir
                yas-snippet-dirs))
  (add-to-list 'yas-snippet-dirs 'entropy/emacs-yas-dir)
  (yas-load-directory entropy/emacs-yas-dir t)
  ;; Then we initialize the thirdparty yas collections
  (use-package yasnippet-snippets
    ;; We must declaration it within `yasnippet' package use-package after
    ;; part since we are used its API after load `yasnippet'.
    :commands (yasnippet-snippets-initialize)
    :init
    (yasnippet-snippets-initialize))

  ;; disable tab key in `yas-minor-mode' which will make conflict with `orgstruct-mode'
  ;; `https://stackoverflow.com/questions/14066526/unset-tab-binding-for-yasnippet'
  (progn
    (define-key yas-minor-mode-map [(tab)]        nil)
    (define-key yas-minor-mode-map (kbd "TAB")    nil)
    (define-key yas-minor-mode-map (kbd "<tab>")  nil)
    (define-key yas-minor-mode-map "\C-c&\C-s"    nil)
    (define-key yas-minor-mode-map "\C-c&\C-n"    nil)
    (define-key yas-minor-mode-map "\C-c&\C-v"    nil)
    ))

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
  :commands
  (aya-create
   aya-expand
   entropy/emacs-yas-aya-choose-snippet)
  :eemacs-indhca
  (((:enable t)
    (yasnippet-uniform))
   ("Auto yasnippet"
    (("M-p" aya-create "Create a snippet from the text between BEG and END"
      :enable t :exit t :global-bind t)
     ("M-e" aya-expand "Insert the last yasnippet created by 'aya-create'"
      :enable t :exit t :global-bind t)
     ("M-o" entropy/emacs-yas-aya-choose-snippet "Choose aya snippet by recorded history"
      :enable t :exit t :global-bind t))))

  :config
  (defvar entropy/emacs-yas--aya-snippets nil
    "Global variable to stored the aya-snippets")
  (defvar-local entropy/emacs-yas--aya-snippets-local nil
    "Local variable to stored the aya-snippets for
`current-buffer'.")

  (defun entropy/emacs-yas--aya-preserve (&rest _)
    (dolist (el '(entropy/emacs-yas--aya-snippets
                  entropy/emacs-yas--aya-snippets-local))
      (unless (or (string= "" aya-current)
                  (member aya-current (symbol-value el)))
        (add-to-list el aya-current))))

  (defun entropy/emacs-yas-aya-choose-snippet (prefix)
    (interactive "P")
    (let ((minibuffer-allow-text-properties t))
      (if (or entropy/emacs-yas--aya-snippets-local
              prefix)
          (setq aya-current
                (completing-read (if prefix
                                     "Select aya(global state): "
                                   "Select aya: ")
                                 (if prefix entropy/emacs-yas--aya-snippets
                                   entropy/emacs-yas--aya-snippets-local)))
        (setq aya-current
              (read-string
               (concat "Select aya "
                       (propertize "(local empty, list from global)" 'face 'warning)
                       ": "))))
      (entropy/emacs-yas--aya-preserve)))

  (defun entropy/emacs-yas--aya-create-prefix (orig-func &rest region)
    "Directly yank current short prefix string as `aya-current'.

The prefix is the region within the single line(e.g. non newline
included in), as the namespace prefix for context, for coding
benefits."
    (if (region-active-p)
        (let ((str-selected (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))))
          (if (and (string-match-p "~" str-selected)
                   (not (string-match-p "\n" str-selected)))
              (apply orig-func region)
            (setq aya-current
                  str-selected)
            (deactivate-mark)
            (message "Create name-prefix: '%s'." aya-current)))
      (apply orig-func region)))

  (advice-add 'aya-create :around #'entropy/emacs-yas--aya-create-prefix)
  (advice-add 'aya-create :after #'entropy/emacs-yas--aya-preserve))

;; * provide
(provide 'entropy-emacs-yas)
