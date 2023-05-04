;;; entropy-emacs-comments.el --- entropy-emacs code docstring(comments) operations  -*- lexical-binding: t; -*-
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
;; assign extra non prog-modes with dwim comment operation
(dolist (extra-mode '((conf-mode . conf-mode-map)))
  (entropy/emacs-lazy-load-simple (car extra-mode)
    :lexical-bindings `((extra-mode . ,extra-mode))
    (define-key (symbol-value (cdr extra-mode))
      (kbd entropy/emacs-custom-comment-dwim-prefix)
      'entropy-emacs-comment-dwim)))

(defun entropy-emacs-comment-dwim (&rest args)
  "DIWM for current context's docstring or commentary boundaries."
  (declare (interactive-only t))
  (interactive)
  (cond
   ;; poporg is obsolete
   ((eq entropy/emacs-custom-comment-dwim-type
        'poporg)
    (apply 'poporg-dwim args))
   ((eq entropy/emacs-custom-comment-dwim-type
        'separedit)
    (apply 'separedit args))))

;; ** edit indirect

(entropy/emacs-defvar-local-with-pml
  entropy/emacs-edit-indirect--oldest-orig-buffer-obj nil
  "The first (oldest) meta info for a edit-indirect buffer.")
(use-package edit-indirect
  :eemacs-functions
  (entropy/emacs/edit-indirect-get-current-meta)
  :config
  (defun entropy/emacs/edit-indirect-get-current-meta
      (&optional use-oldest)
    "Return a plist (meta object) for a edit-indirect buffer including
keys for:

1. `:origin-buffer' : the buffer where edit-indirect invoked from
2. `:origin-buffer-major-mode': 1's `major-mode'.
3. `:indirect-buffer-init-timestamp': the `current-time' at which this
   edit-indirect buffer created. (initial)
4. `:indirect-buffer-init-major-mode': the initial `major-mode' of
   this Edit-indirect buffer enabled. (initial)

When USE-OLDEST non-nil then return the initial meta object for this
edit indirect buffer instead of the one generated based on current
status. But any way, the key with *initial* flag is always the oldest
ones's value.

NOTE: the oldest object is not always reliable if this function is not
the first member of `edit-indirect-after-creation-hook' either for the
local or global value or that's be `let' lexical re-binded.
"
    (entropy/emacs-when-let*-firstn 1
        ((pbuff (and (overlayp edit-indirect--overlay)
                     (overlay-buffer edit-indirect--overlay)))
         (mm (with-current-buffer pbuff major-mode))
         (oobj entropy/emacs-edit-indirect--oldest-orig-buffer-obj)
         ibuff imm rtn)
      (setq ibuff (current-buffer)
            imm (with-current-buffer ibuff major-mode))
      (with-current-buffer ibuff
        (entropy/emacs-setf-by-body rtn
          (list :origin-buffer pbuff
                :origin-buffer-major-mode mm
                :indirect-buffer-init-timestamp
                (if oobj
                    (plist-get oobj :indirect-buffer-init-timestamp)
                  (current-time))
                :indirect-buffer-init-major-mode
                (if oobj
                    (plist-get oobj :indirect-buffer-init-major-mode)
                  imm)))
        (unless entropy/emacs-edit-indirect--oldest-orig-buffer-obj
          (setq entropy/emacs-edit-indirect--oldest-orig-buffer-obj rtn)))
      (if use-oldest
          entropy/emacs-edit-indirect--oldest-orig-buffer-obj
        rtn)))
  (add-hook 'edit-indirect-after-creation-hook
            #'entropy/emacs/edit-indirect-get-current-meta))

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
          (setq buffer-read-only nil))

        (with-temp-buffer
          (when buffer-read-only
            (setq buffer-read-only nil))
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
          (deactivate-mark)
          (goto-char orig-start))

        (cond
          ((eq ro-state nil) (setq buffer-read-only nil))
          ((eq ro-state t) (setq buffer-read-only t))))))

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

;; *** config
  :config

;; **** inhibit readonly
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

;; **** `separedit--block-info' bugs fix
;; ***** DONE bug (1)
  ;; DONE: [2022-02-12 Sat 19:45:07] upstream fixed.
  ;; EEMACS_TEMPORALLY_HACK:
  ;; Patch `separedit--block-info' to ignore 'Wrong type argument:
  ;; integer-or-marker-p, nil' error mesg for some major-modes.
  ;;
  ;; EEMACS_MAINTENANCE: need update follow with upstream
  ;; (advice-patch
  ;;  'separedit--block-info
  ;;  '(when (or (derived-mode-p 'prog-mode)
  ;;             (memq major-mode
  ;;                   '(gfm-mode
  ;;                     markdown-mode org-mode
  ;;                     conf-mode
  ;;                     conf-unix-mode)))
  ;;     (condition-case nil (separedit--comment-region) (user-error nil)))
  ;;  '(when (or (derived-mode-p 'prog-mode)
  ;;             (memq major-mode '(gfm-mode markdown-mode org-mode)))
  ;;     (condition-case nil (separedit--comment-region) (user-error nil))))

;; **** `separedit--point-at-comment' bug fix

  ;; EEMACS_MAINTENANCE: `face-attribute' just can operates on a
  ;; `facep' face while many text propertize of `face' is just a plist
  ;; where error poped out. NOTE&FIXME follow with upstream updates or
  ;; taking up an issue for it?
  (advice-patch 'separedit--point-at-comment
                '(memq (entropy/emacs-ambiguous-face-attribtue f :inherit)
                       comment-faces)
                '(memq (face-attribute f :inherit) comment-faces))

;; **** indiret buffer tidy functions

  (defvar-local entropy/emacs--separedit-buffer-tidy-dispatched-done nil)

  (defvar entropy/emacs--separedit-before-abort-tidy-alist nil
    "A alist mapping a `major-mode' to a `separedit-before-abort-hook'
which should run in a edit-indirect buffer created by separedit
whose parent buffer is using that `major-mode'.

If such mapping is not matched, then call the default all mode's
compatible method where is a `(t . hook)' in this alist if
existed.")
  (defun entropy/emacs--separedit-before-abort-hook ()
    (when-let* ((mo (entropy/emacs/edit-indirect-get-current-meta))
                (mm (plist-get mo :origin-buffer-major-mode))
                (tidy-func
                 (or
                  (alist-get
                   mm
                   entropy/emacs--separedit-before-abort-tidy-alist)
                  (alist-get
                   t
                   entropy/emacs--separedit-before-abort-tidy-alist)))
                (inhibit-read-only t))
      (entropy/emacs-message-simple-progress-message
          (format "Formatting separedit indirect buffer for `%s' specs"
                  mm)
        (funcall tidy-func mo))))

  (defvar entropy/emacs--separedit-buffer-creation-tidy-alist nil
    "A alist mapping a `major-mode' to a
 `separedit-buffer-creation-hook'
which should run in a edit-indirect buffer created by separedit
whose parent buffer is using that `major-mode'.

If such mapping is not matched, then call the default all mode's
compatible method where is a `(t . hook)' in this alist if
existed.")
  (defun entropy/emacs--separedit-buffer-creation-hook ()
    (let ((ibuff (current-buffer)))
      (with-current-buffer ibuff
        (unless entropy/emacs--separedit-buffer-tidy-dispatched-done
          (setq entropy/emacs--separedit-buffer-tidy-dispatched-done t)
          ;; always inject reformatting procedure as the head since
          ;; that procedure must stands by for the equality edit
          ;; buffer content.
          (add-hook 'edit-indirect-before-commit-hook
                    'entropy/emacs--separedit-before-abort-hook
                    -100 t)
          (when-let* ((mo (entropy/emacs/edit-indirect-get-current-meta))
                      (mm (plist-get mo :origin-buffer-major-mode))
                      (tidy-func
                       (or
                        (alist-get
                         mm
                         entropy/emacs--separedit-buffer-creation-tidy-alist)
                        (alist-get
                         t
                         entropy/emacs--separedit-buffer-creation-tidy-alist)))
                      (inhibit-read-only t))
            (entropy/emacs-message-simple-progress-message
                (format "Tidy up separedit indirect buffer for `%s' specs"
                        mm)
              (funcall tidy-func mo)))))))

  (add-hook 'separedit-buffer-creation-hook
            'entropy/emacs--separedit-buffer-creation-hook)

;; ***** sh-mode

  ;; NOTE:
  ;; For old LSP bash-language-server which can not handle comment
  ;; string before a function with empty line commented on as an
  ;; entire one docstring block to display for, thus we hacked replace
  ;; each ^$ empty line with dots sequence to indicate as a empty
  ;; line.
  ;;
  ;; But for now: [2023-05-04 Thu 01:21:51] this problem is fixed by
  ;; upstream, so we commented below hacks out but remaine as the
  ;; example for such hacks for other modes.

  ;; (defun entropy/emacs-separedit--indirect-buffer/tidy/in/sh-mode
  ;;     (&rest _)
  ;;   (let ()
  ;;     (replace-regexp-in-region
  ;;      "^\\.+ *$" "" (point-min) (point-max))))
  ;; (defun entropy/emacs-separedit--indirect-buffer/tidy/out/sh-mode
  ;;     (&rest _)
  ;;   (let ()
  ;;     (replace-regexp-in-region
  ;;      "^$" "......" (point-min) (point-max))))

  ;; (dolist (mm '(sh-mode bash-ts-mode))
  ;;   (add-to-list 'entropy/emacs--separedit-buffer-creation-tidy-alist
  ;;                (cons mm 'entropy/emacs-separedit--indirect-buffer/tidy/in/sh-mode))
  ;;   (add-to-list 'entropy/emacs--separedit-before-abort-tidy-alist
  ;;                (cons mm 'entropy/emacs-separedit--indirect-buffer/tidy/out/sh-mode)))

;; *** __end__
  )

;; * provide
(provide 'entropy-emacs-comments)
