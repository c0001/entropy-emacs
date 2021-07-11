;;; entropy-emacs-comments.el --- entropy-emacs code docstring(comments) operations
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-comments.el
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
;; `entropy-emacs' docstring(comments) operations collection
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * code

(define-key prog-mode-map
  (kbd entropy/emacs-custom-comment-dwim-prefix)
  'entropy-emacs-comment-dwim)

(defun entropy-emacs-comment-dwim (&rest args)
  (interactive)
  (cond
   ;; poporg is obsolete
   ((eq entropy/emacs-custom-comment-dwim-type
        'poporg)
    (apply 'poporg-dwim args))
   ((eq entropy/emacs-custom-comment-dwim-type
        'separedit)
    (apply 'separedit args))))

;; ** poporg :obsolete:
(use-package poporg
  :bind(:map poporg-mode-map
        ("C-c C-c" . org-table-align)
        ("C-c C-k" . poporg-update))
  :commands (poporg-dwim)
  :init

  (setq poporg-adjust-fill-column nil)

  :config

  (defun entropy/emacs-org--poporg-edit-hook ()
    "Hooks for `poporg-edit-hook' compat for entropy-emacs."
    (auto-fill-mode)
    (setq-local fill-column 66)         ;using default 70 fill-column style (66 plus commentary notation width)
    (setq-local org-adapt-indentation nil))

  (add-hook 'poporg-edit-hook
            'entropy/emacs-org--poporg-edit-hook
            t)

  (defun entropy/emacs-org--poporg-dwim-add-comment-line-head-whitespace (&rest _)
    "Add the commentary padding whitespace to each comment line
for preventing poporg make mistake to recognize whole comment
region with error throw out in region selected occasion."
    (when (and (use-region-p)
               (not (buffer-narrowed-p)))
      (let* ((orig-start (region-beginning))
             (orig-end (region-end))
             (cm-str (buffer-substring-no-properties
                      (region-beginning)
                      (region-end)))
             new-cm-str
             ;; (cur-mode major-mode)
             (ro-state buffer-read-only))
        (when buffer-read-only
          (read-only-mode 0))

        (with-temp-buffer
          (when buffer-read-only
            (read-only-mode 0))
          (erase-buffer)
          (goto-char (point-min))
          (insert cm-str)
          (let ((whadd-func
                 (lambda (x)
                   (goto-char (point-min))
                   (while (re-search-forward
                           x nil t)
                     (insert " "))
                   (setq new-cm-str
                         (buffer-substring-no-properties
                          (point-min)
                          (point-max))))))
            (funcall whadd-func
                     (rx (seq line-start (* space)
                              (or (regexp ";;?")
                                  "#" "//")
                              line-end)))))

        (when (and (not (null new-cm-str))
                   (not (equal cm-str new-cm-str)))
          (goto-char orig-start)
          (delete-region orig-start orig-end)
          (insert new-cm-str)
          ;; reset region interactively
          (funcall-interactively 'set-mark-command nil)
          (goto-char orig-start))

        (cond
          ((eq ro-state nil) (read-only-mode 0))
          ((eq ro-state t) (read-only-mode 1))))))

  (defun entropy/emacs-org--poporg-dwim-unnarrowed-buffer (&rest _)
    (with-current-buffer (current-buffer)
      (when (buffer-narrowed-p)
        (save-excursion
          (widen)))))

  (advice-add 'poporg-dwim
              :before
              #'entropy/emacs-org--poporg-dwim-add-comment-line-head-whitespace)

  (advice-add 'poporg-dwim
              :before
              #'entropy/emacs-org--poporg-dwim-unnarrowed-buffer)

  (entropy/emacs-make-function-inhibit-readonly 'poporg-edit-exit))


;; ** separedit

(use-package separedit
  :init
  (setq separedit-preserve-string-indentation t
        separedit-continue-fill-column t
        separedit-write-file-when-execute-save t
        separedit-remove-trailing-spaces-in-comment t)

  (setq separedit-comment-delimiter-alist
      '((("//+!\\(?:<\\)?" "//+\\(?:<\\)?" "\\*+")
         c-mode c++-mode csharp-mode css-mode go-mode
         java-mode js-mode objc-mode php-mode rust-mode
         rustic-mode swift-mode typescript-mode)
        (("--")
         applescript-mode haskell-mode lua-mode)
        (("//+")
         pascal-mode fsharp-mode)
        ((";+\\(?:###autoload\\)?")
         emacs-lisp-mode lisp-interaction-mode)
        ((";+")
         common-lisp racket-mode scheme-mode fennel-mode)
        (("#+")
         nix-mode python-mode ruby-mode
         ;; add missing `sh-mode' and `conf-mode'
         sh-mode conf-mode)))

  :config
  ;; Inhibit readonly when changes apply since `separedit.el' use
  ;; internal `separedit--inhibit-read-only' variable charges thus.
  ;;
  ;; EEMACS_MAINTENANCE: need update follow upstream since
  ;; `edit-indirect--commit' is not an API
  (entropy/emacs-make-function-inhibit-readonly
   'edit-indirect--commit t)

  (defun entropy/emacs-comments--separedit-org-mode-spec (&rest _)
    (setq-local org-adapt-indentation nil))

  (add-hook 'separedit-buffer-creation-hook
            #'entropy/emacs-comments--separedit-org-mode-spec)
  )

;; * provide
(provide 'entropy-emacs-comments)
