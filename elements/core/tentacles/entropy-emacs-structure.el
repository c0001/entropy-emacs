;;; entropy-emacs-structer.el --- entropy-emacs config of coding structer  -*- lexical-binding: t; -*-
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
(use-package outline
  :ensure nil
  :preface
  (defvar outline-regexp))

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
  (if (bound-and-true-p hs-minor-mode)
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
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Structure"
    (("M--" entropy/emacs-structure-toggle-hiding
      "Hide Show For Hide"
      :enable t
      :exit t
      :eemacs-top-bind t)
     ("M-=" entropy/emacs-structure-toggle-selective-display
      "Hide Show For Show"
      :enable t
      :exit t
      :eemacs-top-bind t)))))

;; ** yafolding

(use-package yafolding
  :if (eq entropy/emacs-code-folding-type 'yafolding)
  :commands (yafolding-toggle-element
             yafolding-show-all)
  :preface
  (defvar entropy/emacs-structure--yafolding-jumping-modes '(emacs-lisp-mode lisp-interaction-mode))
  (defun entropy/emacs-structure-yaf-toggle (_column)
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
      (funcall #'yafolding-show-all)))

  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Structure"
    (("M--" entropy/emacs-structure-yaf-toggle
      "yafold toggle hide/show"
      :enable t
      :exit t
      :eemacs-top-bind t)
     ("M-=" entropy/emacs-structure-yaf-show-all
      "yafold show all"
      :enable t
      :exit t
      :eemacs-top-bind t)))))


;; ** vimish
(use-package vimish-fold
  :commands
  (vimish-fold-mode
   vimish-fold-next-fold
   vimish-fold-delete
   vimish-fold
   vimish-fold-delete-all
   vimish-fold-avy
   vimish-fold-refold
   vimish-fold-refold-all
   vimish-fold-unfold
   vimish-fold-toggle
   vimish-fold-unfold-all
   vimish-fold-previous-fold
   vimish-fold-global-mode
   vimish-fold-toggle-all
   )
  :preface
  (defun entropy/emacs-structure--vimish-folded-p (beg end)
    (entropy/emacs-require-only-once 'vimish-fold)
    (let (rtn)
      (catch :exit
        (cl-destructuring-bind (beg . end) (vimish-fold--correct-region beg end)
          (dolist (overlay (overlays-in beg end))
            (when (vimish-fold--vimish-overlay-p overlay)
              (goto-char (overlay-start overlay))
              (setq rtn t)
              (throw :exit nil)))))
      rtn))

  (defun entropy/emacs-structure-vimish-toggle (&optional beg end)
    (interactive)
    (entropy/emacs-require-only-once 'vimish-fold)
    (if (entropy/emacs-structure--vimish-folded-p
         (line-beginning-position)
         (line-end-position))
        (progn
          (message "Vimish toggling ... ")
          (vimish-fold-toggle)
          (message "Vimish toggled!"))
      (let ((beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (when (and beg end)
          (vimish-fold beg end)
          (message "Vimish folded current region.")))))

  (defun entropy/emacs-structure-vimish-fold-double-quote-string ()
    "Fold lisp type doc string block using `vimish'."
    (interactive)
    (entropy/emacs-require-only-once 'vimish-fold)
    (let ((head-dquote-pt
           (save-excursion (re-search-backward "[^\\\\]\"")))
          (end-dquote-pt
           (save-excursion (re-search-forward "[^\\\\]\""))))
      (if (entropy/emacs-structure--vimish-folded-p
           (line-beginning-position)
           (line-end-position))
          (call-interactively #'entropy/emacs-structure-vimish-toggle)
        (when (and head-dquote-pt end-dquote-pt)
          (entropy/emacs-structure-vimish-toggle
           (save-excursion (goto-char head-dquote-pt) (line-beginning-position))
           (save-excursion (goto-char end-dquote-pt) (line-end-position)))))))

  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Basic"
    (("b q"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'vimish-fold))
      "Vimish Mode"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t)))
    (vimish-fold (vimish-fold vimish-fold-folded-keymap) nil (2 2)))
   ("Vimish toggle"
    (("TAB" entropy/emacs-structure-vimish-toggle
      "Automatically vimish fold/show region"
      :enable t :eemacs-top-bind t :exit t))
    "Vimish delete"
    (("d c" vimish-fold-delete "Delete fold at point"
      :enable t :exit t)
     ("d a" vimish-fold-delete-all
      "Delete all folds in current buffer"
      :enable t :exit t))
    "Vimish fold"
    (("f c" vimish-fold "Fold active region staring at BEG, ending at END"
      :enable t :exit t)
     ("f a" vimish-fold-refold-all "Refold all closed folds in current buffer"
      :enable t :exit t)
     ("f s"
      entropy/emacs-structure-vimish-fold-double-quote-string
      "Quickly vimish fold double quote string."
      :enable t :exit t))
    "Vimish unfold"
    (("u c" vimish-fold-unfold "Unfold at point"
      :enable t :exit t)
     ("u a" vimish-fold-unfold-all "Unfold all folds in current buffer"
      :enable t :exit t))))

  :config
  ;; Disable vimish native kemap that conflict with eemacs
  ;; specification
  (define-key vimish-fold-folded-keymap [67108960] nil)
  (define-key vimish-fold-unfolded-keymap [67108960] nil)

  ;; fake advice for 'vimish-fold--read-only for text replaceable
  (advice-add 'vimish-fold--read-only
              :around
              (lambda (&rest _)
                t)))

;; ** outorg
(use-package outorg
  :commands (outorg-edit-as-org
             outorg-edit-comments-and-propagate-changes
             outorg-copy-edits-and-exit)
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Structure"
    (("M-o" outorg-edit-as-org "Edit As Org Block"
      :enable t
      :exit t
      :eemacs-top-bind t))))
  :init

  (defun entropy/emacs-structure--outorg-edit-hook ()
    "Hooks for `outorg-edit-minor-mode-hook' compat for entropy-emacs."
    (auto-fill-mode)
    (setq-local fill-column 66)
    (setq-local org-adapt-indentation nil))
  (add-hook 'outorg-edit-minor-mode-hook
            #'entropy/emacs-structure--outorg-edit-hook)

  :config
  (define-key outorg-edit-minor-mode-map
    [remap outorg-save-edits-to-tmp-file] nil)
  (define-key outorg-edit-minor-mode-map
    (kbd "C-x C-s") 'outorg-copy-edits-and-exit)

  (with-eval-after-load 'outshine
    (defun outshine-hook-function ()
      (outshine-mode 1)))

  (defun entropy/emacs-structure--copied-filter (&rest _)
    "Prunning `outorg-edit-buffer-name''s emtpy commented line for
preventing uncommenting funciton throw out the current marker
which will make outorg's 'looping' procedure can not terminated in
correct way.

Example in lisp mode buffer:

#+BEGIN_SRC emacs-lisp
  ;;; code
  ;;
  ;; Test outorg transfer to org context
  ;;
  ;;; Foobar
  ;;
  ;; test
#+END_SRC

Line 2 and 4 even for 6, can not uncomment by
`uncomment-region-default-1' for commonly result, it will warnning
for prompt \"Beginning of buffer\" which will overwrite the outorg
convert procedure (i.e. `outorg-convert-to-org') temporal marker
to indicate the comment beginning, thus the following marker
moving operation will cause non-terminated looping proceeding."
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (format "^%s+ *$" (regexp-quote comment-start)) nil t)
          (replace-match "")))))
  (advice-add 'outorg-convert-oldschool-elisp-buffer-to-outshine
              :before
              #'entropy/emacs-structure--copied-filter)
  (advice-add 'outorg-convert-to-org
              :before
              #'entropy/emacs-structure--copied-filter)

  (entropy/emacs-make-function-inhibit-readonly
   'outorg-edit-as-org t)

  (entropy/emacs-make-function-inhibit-readonly
   'outorg-copy-edits-and-exit t)

  (defvar __outorg-convert-back-to-code-start nil
    "The indicator that the `outorg-convert-back-to-code' is
starting when non-nil, and will be reset to nil while finished or
any corruption occurred.")
  (defun entropy/emacs-structure--outorg-convert-back-to-code
      (orig-func &rest orig-args)
    "Around advice for `outorg-convert-back-to-code' to query
about whether comments the org source block in which case some
occasions need to do thus e.g. since *COMMETARY* header do not
want to preserve the source demo."
    (unwind-protect
        (progn
          (setq __outorg-convert-back-to-code-start t)
          (if (yes-or-no-p "Comment source block? ")
              (apply orig-func orig-args)
            (progn
              (comment-region (point-min) (point-max))
              (move-marker outorg-beginning-of-code nil)
              (move-marker outorg-end-of-code nil)
              ;; 2nd (optional) run: convert elisp headers to oldschool
              (when outorg-oldschool-elisp-headers-p
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward
                          "\\(^;;\\)\\( [*]+\\)\\( \\)" nil 'NOERROR)
                    (let* ((org-header-level (- (length (match-string-no-properties 0)) 4))
                           (replacement-string
                            (let ((strg ";"))
                              (dotimes (_ (1- org-header-level) strg)
                                (setq strg (concat strg ";"))))))
                      (replace-match replacement-string
                                     nil nil nil 2)))))))
          (message "Back to code buffer done!"))
      (setq __outorg-convert-back-to-code-start nil)))
  (advice-add 'outorg-convert-back-to-code
              :around
              #'entropy/emacs-structure--outorg-convert-back-to-code)

  )

;; ** outshine-mode

(use-package outshine
  :commands (outshine-mode)
  :hook
  ((emacs-lisp-mode . outshine-mode)
   (lisp-interaction-mode . outshine-mode)
   (sh-mode . outshine-mode)
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
   (conf-xdefaults-mode . outshine-mode)
   (makefile-mode . outshine-mode)
   (cmake-mode . outshine-mode))
  :bind
  (:map org-mode-map
   ("C-c M-t" . nil)
   ("C-c M-e" . nil)
   ("C-c M-p" . nil)
   ("C-c M-y" . nil)
   :map outshine-mode-map
   ("C-x n s" . org-narrow-to-subtree)
   ("C-<f7>" . entropy/emacs-structure-outshine-reload-major))

;; *** preface
  :preface
  (defvar-local entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode
    nil)

  (defun entropy/emacs-structure-outshine-reload-major ()
    "Reload lisp(e.g. lisp elisp or more like one) buffer's outshine
    feataure with mordern \"*\" or old-school \";\" headline visual type
    set."
    (declare (interactive-only t))
    (interactive)
    (when (and (bound-and-true-p outshine-mode)
               (entropy/emacs-buffer-is-lisp-like-p))
      (outshine-mode -1)
      (let (_)
        (with-current-buffer (current-buffer)
          (setq-local
           entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode
           (null
            entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode))
          (outshine-mode)
          ))))

  (defvar-local entropy/emacs-outshine-current-buffer-visibility-state
    nil)
  (defun entropy/emacs-outshine-cycle-buffer (&optional arg)
    "Like `outshine-cycle-bufer' but only based on `outline' so that it can
be used in `org-mode' too.

Rotate the visibility state of the buffer through 3 states:

- OVERVIEW: Show only top-level headlines matched specified header level.

- CONTENTS: Show all headlines of all levels heading, but no body text.

- SHOW ALL: Show everything.

With a numeric prefix ARG and is numberic, show all headlines up to
that level i.e. matched show type OVERVIEW, show 'SHOW ALL' if the the
prefix ARG is the pure single `C-u' type, further more show CONTENTS
if it is double prefix type, and any other prefix type fallback to
OVERVIEW status use `prefix-numeric-value' and `log' base on 4 to
recalc the specified head level specification.
"
    (interactive "P")
    (if (or (bound-and-true-p outline-mode)
            (bound-and-true-p outshine-mode)
            (bound-and-true-p outline-minor-mode)
            (derived-mode-p 'outline-mode))
        (let ((outline-level-get-func
               (cond
                ((bound-and-true-p outshine-mode)
                 'outshine-calc-outline-level)
                ((eq major-mode 'org-mode)
                 'org-outline-level)
                (t
                 (lambda ()
                   (save-excursion
                     (save-restriction
                       (widen)
                       (forward-line 0)
                       (progn (outline-back-to-heading)
                              (outline-level))))))))
              (lscmd-eq-func
               (lambda (type)
                 (and (null current-prefix-arg)
                      (eq entropy/emacs-outshine-current-buffer-visibility-state
                          type))))
              (cycle-type-msg-func
               (lambda (type &optional other-msg)
                 (let ((type-assoc
                        '((overview . "OVERVIEW")
                          (contents . "CONTENTS")
                          (all      . "SHOW ALL"))))
                   (message "[Outline cycle for type] \"%s\" done. (%s)"
                            (or (alist-get type type-assoc)
                                (error "wrong type of outline cycle type '%s'"
                                       type))
                            (or other-msg
                                "-v-"))))))
          (save-excursion
            (cond
             ;; -------------------- condition (1) with numberic prefix arg
             ((integerp arg)
              (outline-show-all)
              (outline-hide-sublevels arg)
              (setq
               entropy/emacs-outshine-current-buffer-visibility-state
               'overview)
              (funcall cycle-type-msg-func 'overview (format "Using level %s" arg)))

             ;; -------------------- condition (2) double prefix arg to CONTENTS tyep
             ((or
               (equal arg '(16))
               (funcall lscmd-eq-func 'overview))
              ;; We just created the overview - now do table of contents
              ;; This can be slow in very large buffers, so indicate action
              (funcall cycle-type-msg-func 'contents)

              ;; NOTE: firstly we must fold to top-level since the
              ;; fully expanded children will not be fold while
              ;; `outline-show-branches'
              (condition-case nil
                  (outline-hide-sublevels
                   ;; found the first head as the toplevel result since we
                   ;; can not cycle through abnormal head structure.
                   (save-excursion
                     (save-match-data
                       (goto-char (point-min))
                       (when (re-search-forward outline-regexp nil t)
                         (beginning-of-line)
                         (funcall outline-level-get-func)))))
                (error
                 (user-error
                  "No outline heading found in this buffer according to `outline-regexp': %s"
                  outline-regexp)))

              ;; Visit all headings and show their offspring
              (goto-char (point-max))
              (while (not (bobp))
                (condition-case nil
                    (progn
                      (outline-previous-visible-heading 1)
                      (outline-show-branches))
                  (error (goto-char (point-min)))))
              (funcall cycle-type-msg-func 'contents)
              (setq
               entropy/emacs-outshine-current-buffer-visibility-state
               'contents))

             ;; -------------------- condition (3) single prefix arg to show all
             ((or
               (equal arg '(4))
               (funcall lscmd-eq-func 'contents))
              ;; We just showed the table of contents - now show everything
              (outline-show-all)
              (funcall cycle-type-msg-func 'all)
              (setq
               entropy/emacs-outshine-current-buffer-visibility-state 'all))

             ;; -------------------- conditon (4) others
             (t
              ;; Default action: go to overview
              (let ((toplevel
                     (cond
                      (current-prefix-arg
                       ;; caculate `outline-level' with prefix-arg in
                       ;; which case the level is larger or equal to 3
                       ;; since 1 or 2 reflects `current-prefix-arg'
                       ;; is mapped with condition(3) and condition(2)
                       (floor (log (prefix-numeric-value current-prefix-arg) 4)))
                      ((save-excursion
                         ;; use current heading level as matches
                         (beginning-of-line)
                         (looking-at outline-regexp))
                       (max 1 (save-excursion (beginning-of-line) (funcall outline-level))))
                      (t
                       ;; NOTE:
                       ;; judge first head level if current buffer doesn't
                       ;; use 1st level for first head
                       (let ((first-level
                              (save-excursion
                                (save-match-data
                                  (goto-char (point-min))
                                  (when (re-search-forward outline-regexp nil t)
                                    (beginning-of-line)
                                    (funcall outline-level-get-func))))))
                         (or first-level 1))))))
                (outline-hide-sublevels toplevel)
                (setq
                 entropy/emacs-outshine-current-buffer-visibility-state
                 'overview)
                (funcall cycle-type-msg-func 'overview (format "Using level %s" toplevel))))))
          ;; HACK: recenter the buffer point since fully overviw or
          ;; huge fold will let the buffer visibility empty that user
          ;; need to scroll window to show the whole content.
          (recenter-top-bottom '(middle)))
      (user-error "Not in an outline based buffer!")))

  (defun entropy/outshine-previous-visible-heading (&optional arg)
    "Like `outline-previous-visible-heading' but goto to parent
heading as `entropy/emacs-org-previous-visible-heading' when
prefix arg was '(4) i.e. the single `C-u' type."
    (declare (interactive-only t))
    (interactive "P")
    (cond
     ((equal arg '(4))
      (outline-up-heading 1))
     (t
      (outline-previous-visible-heading
       (prefix-numeric-value arg)))))

  (defun entropy/emacs-structure-outshine-pop-imenu (&optional args)
    "Call `imenu' defautly unless `prefix-arg' is non-nil and
`outshine-mode' is actived in `current-buffer' in which case we
call `outshine-imenu' instead."
    (declare (interactive-only t))
    (interactive "P")
    (cond ((and (bound-and-true-p outshine-mode)
                args)
           (outshine-imenu))
          (t
           (call-interactively 'imenu))))

;; *** eemacs top key bind
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (org-mode-hook prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Structure"
    (("\\" entropy/emacs-outshine-cycle-buffer
      "Outshine Cycle"
      :enable t
      :exit t
      :eemacs-top-bind t)
     ("M-i" entropy/emacs-structure-outshine-pop-imenu
      "Outshine popup imenu"
      :enable t
      :exit t
      :eemacs-top-bind t))))

;; *** init

  :init
  (entropy/emacs-lazy-initial-advice-before
   '(switch-to-buffer find-file)
   "enable-outshine-for-opened-buffer"
   "enable-outshine-for-opened-buffer"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   ;; enable `outshine-mode' in pre-opened lisp buffer like
   ;; `*scratch*' buffer since they are opened before
   ;; `entropy/emacs-startup-done'.
   (mapc (lambda (buffer)
           (with-current-buffer buffer
             (when (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
               (outshine-mode +1))))
         (buffer-list))

   ;; let `outline-regexp' ignored as local variable since we hacked
   ;; outshine to auto generated it in each buffer and risky while the
   ;; local set.
   (add-to-list 'ignored-local-variables
                'outline-regexp)
   )

;; *** config

  :config
;; **** remove outshine remaps

  ;; we remove those laggy remap include
  ;; `outshine-self-insert-command' since we don't need them while
  ;; ever use them.
  (setq outshine-mode-map
        (assq-delete-all 'remap outshine-mode-map))

;; **** redefination

  (defvar-local entropy/emacs-structure--outline_local_spec_comment_start_str nil
    "The specified `comment-start' used for eemacs spec in
outline mode or as its minor mode.

Usually an buffer just indicate one value to the `comment-start', but
in some case we should use another when we use multi-comments style
`major-mode', thus this variable existed.")

  (defun outshine-calc-outline-level ()
    "Calculate the right outline level for the outshine-regexp.

NOTE: This function has been redefined to fix its bug for
compatible with =entropy-emacs=."
    (save-excursion
      (save-match-data
        (and
         (looking-at (outshine-calc-outline-regexp))
         (let* ((m-strg (match-string-no-properties 0))
                (comment-start
                 ;; use `entropy/emacs-structure--outline_local_spec_comment_start_str' as precedence.
                 (or (and entropy/emacs-structure--outline_local_spec_comment_start_str
                          (stringp entropy/emacs-structure--outline_local_spec_comment_start_str)
                          entropy/emacs-structure--outline_local_spec_comment_start_str)
                     comment-start
                     (error
                      "There's no valid `comment-start' indicated in this buffer.")))
                (comment-starter
                 (let (rtn)
                   (if (> comment-add 0)
                       (progn
                         (dotimes (_ (+ comment-add 1))
                           (setq rtn
                                 (concat (or rtn "") comment-start)))
                         rtn)
                     comment-start)))
                (head-indc (replace-regexp-in-string
                            (concat "^" (regexp-quote comment-starter))
                            ""
                            m-strg)))
           (length (car (split-string head-indc nil 'omit-null))))))))



;; **** eemacs outshine head regexp defination

  (setq outshine-max-level 100)

  ;; Notice here's doc, its necessary for eemacs specification
  (defvar outshine-regexp-base ""
    "Actual base for calculating the outline-regexp

The regexp form must obeyed the formular as:

   [spec-char]{1,max-level}SPC

This is required for eemacs specification.")

  (setq outshine-default-outline-regexp-base
        (format "[%s]\\{1,%d\\}"
                outshine-regexp-base-char outshine-max-level))

  (setq outshine-oldschool-elisp-outline-regexp-base
        (format "[;]\\{1,%d\\}" outshine-max-level))

;; **** eemacs outshine core subroutines

;; ***** eemacs outshine heading regexp generator
  (defun entropy/emacs-structure--outshine-modern-header-style-in-elisp-p (&optional buffer)
    "Return nil, if there is no match for a outshine-style header.
Searches in BUFFER if given, otherwise in current buffer for 0 or
1 which 0 means the traditional oushine-style header with
comment-padding (inverse of
`outshine-enforce-no-comment-padding-p'), 1 for otherwise.

This function was the replacement for
`outshine-modern-header-style-in-elisp-p' which made for be
suitable for the eemacs modified version of
`outshine-set-outline-regexp-base'."
    (let ((buf (or buffer (current-buffer)))
          (outshine-regexp-base-char
           (default-value 'outshine-regexp-base-char))
          (modern-padding-search
           (lambda ()
             (save-excursion
               (re-search-forward
                (format "^;; [%s]\\{1,%d\\} "
                        outshine-regexp-base-char outshine-max-level)
                nil 'NOERROR))))
          (modern-nonpadding-search
           (lambda ()
             (save-excursion
               (re-search-forward
                (format "^;;[%s]\\{1,%d\\} "
                        outshine-regexp-base-char outshine-max-level)
                nil 'NOERROR))))
          feature)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (cond ((funcall modern-padding-search)
                 (setq feature 0))
                ((funcall modern-nonpadding-search)
                 (setq feature 1))
                (t (setq feature nil)))))
      feature))

  (defun entropy/emacs-structure--outshine-set-outline-regexp-base (&rest _)
    "Return the actual outline-regexp-base.

Preventing recursive face rending for level keywords that local
binding to `outshine-regexp-base-char' while using traditional
structure type for elisp."

    (cond ((entropy/emacs-buffer-is-lisp-like-p)
           (let ((mordern-lisp-feature
                  (entropy/emacs-structure--outshine-modern-header-style-in-elisp-p)))
             (cond ((and (not (null mordern-lisp-feature))
                         (null entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode))
                    (cl-case mordern-lisp-feature
                      (0 (setq-local outshine-enforce-no-comment-padding-p nil))
                      (1 (setq-local outshine-enforce-no-comment-padding-p t)))
                    (setq-local outshine-regexp-base
                                outshine-default-outline-regexp-base
                                outshine-regexp-base-char
                                (default-value 'outshine-regexp-base-char)))
                   ((or (null mordern-lisp-feature)
                        entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode)
                    (setq-local outshine-regexp-base outshine-oldschool-elisp-outline-regexp-base)
                    (setq-local outshine-enforce-no-comment-padding-p t)
                    (setq-local outshine-regexp-base-char ";")))))
          (t
           (setq-local outshine-enforce-no-comment-padding-p nil)
           (setq-local outshine-regexp-base
                       outshine-default-outline-regexp-base)
           (setq-local outshine-regexp-base-char
                       (default-value 'outshine-regexp-base-char)))))

  (advice-add 'outshine-set-outline-regexp-base
              :override
              'entropy/emacs-structure--outshine-set-outline-regexp-base)

;; ***** eemacs outshine head inventing
  (defun entropy/emacs-structure--outshine-invent-heading (head up)
    "Create a heading by using heading HEAD as a template.
When UP is non-nil, the created heading will be one level above.
Otherwise, it will be one level below.

This function was a outshine specifed based on
`outline-invent-heading' that for be suitable with the
`outline-regexp' generated by `outshine-calc-outline-regexp'
which forcely using the trailing white-space as head indicator."
    (save-match-data
      ;; Let's try to invent one by repeating or deleting the last char.
      (let ((new-head (if up (concat (substring head 0 -2) " ")
                        (concat (replace-regexp-in-string "\\s-$" "" head)
                                outshine-regexp-base-char
                                " "))))
        (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                          new-head)
            ;; Why bother checking that it is indeed higher/lower level ?
            new-head
          ;; Didn't work, so ask what to do.
          (read-string (format-message "%s heading for `%s': "
                                       (if up "Parent" "Demoted") head)
                       head nil nil t)))))

;; ***** eemacs outshine head demote/promote
  (defun entropy/emacs-structure--outshine-sub-of-outline-demote (&optional which)
    "Demote headings lower down the tree.
If `transient-mark-mode' is on, and mark is active, demote
headings in the region (from a Lisp program, pass `region' for
WHICH).  Otherwise: without prefix argument, demote current
heading and all headings in the subtree (from a Lisp program,
pass `subtree' for WHICH); with prefix argument, demote just the
current heading (from a Lisp program, pass nil for WHICH, or do
not pass any argument).

This function is as the origin `outline-demote' but using
`entropy/emacs-structure--outshine-invent-heading' instead
`outline-invent-heading' for outshine specification."
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (cond
     ((eq which 'region)
      (outline-map-region 'entropy/emacs-structure--outshine-sub-of-outline-demote
                          (region-beginning) (region-end)))
     (which
      (outline-map-region 'entropy/emacs-structure--outshine-sub-of-outline-demote
                          (point)
                          (save-excursion (outline-get-next-sibling) (point))))
     (t
      (let* ((head (match-string-no-properties 0))
             (level (save-match-data (funcall outline-level)))
             (down-head
              (or (outline-head-from-level (1+ level) head)
                  (save-excursion
                    (save-match-data
                      (while (and (progn (outline-next-heading) (not (eobp)))
                                  (<= (funcall outline-level) level)))
                      (when (eobp)
                        ;; Try again from the beginning of the buffer.
                        (goto-char (point-min))
                        (while (and (progn (outline-next-heading) (not (eobp)))
                                    (<= (funcall outline-level) level))))
                      (unless (eobp)
                        (looking-at outline-regexp)
                        (match-string-no-properties 0))))
                  ;; Bummer!! There is no higher-level heading in the buffer.
                  (entropy/emacs-structure--outshine-invent-heading head nil)
                  )))

        (unless (rassoc level outline-heading-alist)
          (push (cons head level) outline-heading-alist))
        (replace-match down-head nil t)))))

  (defun entropy/emacs-structure--outshine-sub-of-outline-promote (&optional which)
    "Promote headings higher up the tree.
If `transient-mark-mode' is on, and mark is active, promote
headings in the region (from a Lisp program, pass `region' for
WHICH).  Otherwise: without prefix argument, promote current
heading and all headings in the subtree (from a Lisp program,
pass `subtree' for WHICH); with prefix argument, promote just the
current heading (from a Lisp program, pass nil for WHICH, or do
not pass any argument).

This function is as the origin `outline-promote' but using
`entropy/emacs-structure--outshine-invent-heading' instead
`outline-invent-heading' for outshine specification."
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (cond
     ((eq which 'region)
      (outline-map-region 'entropy/emacs-structure--outshine-sub-of-outline-promote
                          (region-beginning) (region-end)))
     (which
      (outline-map-region 'entropy/emacs-structure--outshine-sub-of-outline-promote
                          (point)
                          (save-excursion (outline-get-next-sibling) (point))))
     (t
      (outline-back-to-heading t)
      (let* ((head (match-string-no-properties 0))
             (level (save-match-data (funcall outline-level)))
             (up-head (or (outline-head-from-level (1- level) head)
                          ;; Use the parent heading, if it is really
                          ;; one level less.
                          (save-excursion
                            (save-match-data
                              (outline-up-heading 1 t)
                              (and (= (1- level) (funcall outline-level))
                                   (match-string-no-properties 0))))
                          ;; Bummer!! There is no lower level heading.
                          (entropy/emacs-structure--outshine-invent-heading
                           head 'up))))

        (unless (rassoc level outline-heading-alist)
          (push (cons head level) outline-heading-alist))

        (replace-match up-head nil t)))))

;; ***** eemacs outshine heading face generator

  (defun entropy/emacs-structure--outshine-gen-face-keywords (outline-regexp-spec times)
    (let ((outline-regex-head
           (substring outline-regexp-spec
                      0
                      (- 0
                         (+ 7
                            (length
                             (number-to-string
                              outshine-max-level))))))
          func rtn)
      (setq func
            (lambda (level)
              (list (format "%s%s%s%s%s%s"
                            outline-regex-head
                            (format "\\{%d\\}" level)
                            " "
                            "\\(.*"
                            (if outshine-fontify-whole-heading-line "\n?" "")
                            "\\)")
                    1
                    `(quote ,(intern (format "outshine-level-%d" level)))
                    t)))
      (cl-loop for level from 1 to times
               do (push (funcall func level) rtn))
      (nreverse rtn)))

  (defun entropy/emacs-structure--outshine-get-level-face-max-suffix ()
    "Find the max level outshine-level-NUM face's level."
    (let ((level 1))
      (while (facep (intern (format "outshine-level-%s" level)))
        (cl-incf level))
      (- level 1)))

  (defvar entropy/emacs-structure--outshine-face-level-max-level
    (entropy/emacs-structure--outshine-get-level-face-max-suffix)
    "The max level outshine-level-NUM face's level")

  (defvar entropy/emacs-structure--outshine-level-face-has-generated nil)
  (defun entropy/emacs-structure--outshine-batch-gen-outshine-level-faces ()
    "Batch generate outshine-level face according to `outshine-max-level'."
    (when (and (> outshine-max-level
                  entropy/emacs-structure--outshine-face-level-max-level)
               (null entropy/emacs-structure--outshine-level-face-has-generated))
      (let* ((max-face-level entropy/emacs-structure--outshine-face-level-max-level)
             (top-map (cl-loop for step from (+ max-face-level 1) to outshine-max-level
                               collect step))
             (cnt 0)
             (turn 1)
             maplist)
        ;; generate face suffix number maplist as
        ;; '((9 10 11 12 13 14 15 16) (17 18 19 20 21 22 23 24) ...)
        (while (<= cnt (- (length top-map) 1))
          (let (var_tmp)
            (while (and (not (= (+ 1 cnt) (- (* turn (+ max-face-level 1)) (- turn 1))))
                        (<= cnt (- (length top-map) 1)))
              (push (nth cnt top-map) var_tmp)
              (cl-incf cnt))
            (push (reverse var_tmp) maplist)
            (cl-incf turn)))
        (setq maplist (reverse maplist))
        (dolist (map maplist)
          (let ((face-inherit-indicator 1))
            (dolist (face-suffix map)
              (funcall
               `(lambda ()
                  (defface ,(intern (format "outshine-level-%s" face-suffix))
                    '((t ()))
                    ,(format "Face used for level %s headlines"
                             face-suffix))
                  (set-face-attribute
                   ',(intern (format "outshine-level-%s" face-suffix))
                   nil
                   :inherit
                   ',(intern (format "outshine-level-%s"
                                     face-inherit-indicator)))))
              (cl-incf face-inherit-indicator))))))
    (setq entropy/emacs-structure--outshine-level-face-has-generated t))

  (defun entropy/emacs-structure--outshine-fontify-headlines (_ &rest orig-args)
    "Calculate heading regexps for font-lock mode."
    (entropy/emacs-structure--outshine-batch-gen-outshine-level-faces)
    (let* ((outline-regexp-spec (car orig-args))
           (font-lock-new-keywords
            (entropy/emacs-structure--outshine-gen-face-keywords
             outline-regexp-spec
             outshine-max-level)))
      (add-to-list 'outshine-font-lock-keywords font-lock-new-keywords)
      (font-lock-add-keywords nil font-lock-new-keywords)
      (outshine-font-lock-flush)))
  (advice-add 'outshine-fontify-headlines
              :around
              #'entropy/emacs-structure--outshine-fontify-headlines)

;; **** interactive operation

  (unless (fboundp 'outshine-define-key)
    (defmacro outshine-define-key (keymap key def condition &optional mode)
      "Define key with fallback.

Binds KEY to definition DEF in keymap KEYMAP, the binding is
active when the CONDITION is true. Otherwise turns MODE off and
re-enables previous definition for KEY. If MODE is nil, tries to
recover it by stripping off \"-map\" from KEYMAP name.

DEF must be a quoted symbol of an interactive command.

This interns a named function `outshine-kbd-[key-name]' with the
appropriate docstring so that calling `describe-key' on KEY
produces a more informative output."
      (declare (indent defun))
      (let ((fn-name
             (intern (format "outshine-kbd-%s"
                             (if (eq (car-safe key) 'kbd)
                                 (cadr key)
                               key))))
            (docstring
             (format "Run the interactive command `%s' if the following condition \
is satisfied:\n\n    %s\n
Otherwise, fallback to the original binding of %s in the current mode."
                     (cadr def) ;; def is a quoted symbol (quote sym)
                     condition key))
            (mode-name
             (cond (mode mode)
                   ((string-match
                     (rx (group (1+ any)) "-map" eol) (symbol-name keymap))
                    (intern (match-string 1 (symbol-name keymap))))
                   (t (error "Could not deduce mode name from keymap name")))))
        `(progn
           (defun ,fn-name ()
             ,docstring
             (interactive)
             (call-interactively
              (if ,condition
                  ,def
                ;; turn mode off and recover the original function
                (let ((,mode-name nil))
                  (or (key-binding ,key)
                      ,(if (equal (kbd "<tab>") key)
                           (key-binding (kbd "TAB")))
                      (lambda nil (interactive) (message "`%s' can do nothing useful here." (key-description ,key))))))))
           (define-key ,keymap ,key (quote ,fn-name))))))

  (defun entropy/emacs-structure-outshine-demote-command (&optional which)
    ""
    (declare (interactive-only t))
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (funcall 'entropy/emacs-structure--outshine-sub-of-outline-demote
             which))

  (defun entropy/emacs-structure-outshine-promote-command (&optional which)
    ""
    (declare (interactive-only t))
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (funcall 'entropy/emacs-structure--outshine-sub-of-outline-promote
             which))

  (outshine-define-key outshine-mode-map
    (kbd "M-S-<left>") 'entropy/emacs-structure-outshine-promote-command
    (outline-on-heading-p))
  (outshine-define-key outshine-mode-map
    (kbd "M-S-<right>") 'entropy/emacs-structure-outshine-demote-command
    (outline-on-heading-p))

  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'entropy/emacs-outshine-cycle-buffer
    (or (outline-on-heading-p) (bobp)
        (error "Using it out of the headline was not supported.")))

  (define-key outshine-mode-map
    (kbd "C-c C-p")
    #'entropy/outshine-previous-visible-heading)
  (define-key outshine-mode-map
    [M-up]
    #'entropy/outshine-previous-visible-heading)
  (define-key outshine-mode-map
    (kbd "C-c C-n")
    #'outline-next-visible-heading)
  (define-key outshine-mode-map
    [M-down]
    #'outline-next-visible-heading)

;; **** sepcial hooks defination

  (defun entropy/emacs-structure--outshine-comment-add-specification
      ()
    "Specified `comment-add' for some special cases for outshine-mode
calculate `outline-regexp', such as elisp extension `bongo.el' use
2 repeat of `comment-add' to render its headline but default is 1
which can not be rendered correctly."
    (cond
     ((and (eq major-mode 'emacs-lisp-mode)
           (buffer-file-name)
           (string-match-p (concat (regexp-quote "bongo.el") "$")
                           (buffer-file-name)))
      (setq-local comment-add 2))
     (t
      nil)))

  (add-hook 'outline-minor-mode-hook
            #'entropy/emacs-structure--outshine-comment-add-specification)

;; **** outshine-mode advice

  (defun entropy/emacs-structure--outshine-mode-around-adv
      (orig-func &rest orig-args)
    "The `outshine-mode' specification to do some top outline specification
while in some special cases.

This function is an around advice for `outshine-mode'."
    (let (_)
      (cond
       ;; `c-mode' outline specification
       ((eq major-mode 'c-mode)
        ;; temporally set the default comment style for c-mode used as
        ;; heading line start.
        (let ((comment-start "//")
              (comment-end ""))
          ;; make it as the specified `comment-start'
          (setq-local entropy/emacs-structure--outline_local_spec_comment_start_str "//")
          (apply orig-func orig-args)))
       (t
        (apply orig-func orig-args)))))

  (advice-add 'outshine-mode
              :around
              #'entropy/emacs-structure--outshine-mode-around-adv)

  )

;; ** benefit interactively functions
;; *** subtree parse
(defun entropy/emacs-structure-parse-subtree-simple (cur-prefix &optional calc-depth-1?)
  "Parse outline subtree simply with follow two feedback:

1. Refile specific subtree into a temporal buffer
   \"*eemacs/org-subtree-counts-check*\". (we don't switch to it
   automatically, you should check it out by yourself)
2. ouput the subtree entries amounts

Prefix key is supported to specify which outline heading to be
focused on, defaultly to the current heading context, or with
\"C-u\" or multiply thus or with the numeric prefix specification
to specify its parent heading with Nth backing to.

Optional argument CALC-DEPTH-1? is only used to the second option
to restrict the entries counting behaviour, which will just
enumerate the 1 depth sub-entries of current subtree and sum them
amounts."
  (declare (interactive-only t))
  (interactive "P")
  (save-excursion
    (let* ((calc-depth-1?
            (if (called-interactively-p 'any)
                (not
                 (yes-or-no-p
                  "Count all nested subtree entries?(default to show 1 depth of subtree)"
                  ))
              calc-depth-1?))
           (temp-buffer (get-buffer-create "*eemacs/org-subtree-counts-check*"))
           (cur-mode major-mode)
           (outline-raw-p (derived-mode-p 'outline-mode))
           (cur-prefix
            (if (and (listp cur-prefix) (not (null cur-prefix)))
                (floor (log (car cur-prefix) 4))
              cur-prefix))
           (begin-pos
            (condition-case nil
                (cond
                 ((null cur-prefix)
                  (outline-back-to-heading)
                  (point))
                 (t
                  (outline-up-heading cur-prefix)
                  (point)))
              (t (point-min))))
           (see-level
            (lambda (&optional no-jump)
              (save-excursion
                (if outline-raw-p
                    (if (eq major-mode 'org-mode)
                        (let ((org-level (save-mark-and-excursion
                                           (org-outline-level))))
                          (if org-level
                              (cond
                               ((= 0 org-level)
                                (user-error "no backward org heading found"))
                               (t
                                org-level))
                            (user-error "no org heading found")))
                      (progn
                        (unless no-jump
                          (outline-back-to-heading))
                        (outline-level)))
                  (progn
                    (entropy/emacs-require-only-once 'outshine)
                    (unless (bound-and-true-p outshine-mode)
                      (outshine-mode 1))
                    (or (outshine-calc-outline-level)
                        (user-error "no org heading found")))))))
           (cur-level
            (or
             (condition-case nil
                 (funcall see-level)
               (t 0))
             0))
           (end-pos
            (condition-case nil
                (progn
                  (outline-end-of-subtree)
                  (point))
              (t (point-max))))
           (subtree-content
            (buffer-substring begin-pos end-pos)))
      (with-current-buffer temp-buffer
        (let ((inhibit-read-only t)
              (rtn 0))
          (erase-buffer)
          (insert subtree-content)
          (funcall cur-mode)
          (goto-char (point-min))
          (outline-map-region
           (if calc-depth-1?
               (lambda (&rest _)
                 (let* ((pos-level (funcall see-level 'no-jump)))
                   (when (= (+ cur-level 1) pos-level)
                     (cl-incf rtn))))
             (lambda (&rest _)
               (let* ((pos-level (funcall see-level 'no-jump)))
                 (when (< cur-level pos-level)
                   (cl-incf rtn)))))
           (point-min) (point-max))
          (message "Subtree entries counts: %s (with-prefix: %s on current level %s)"
                   rtn
                   cur-prefix
                   cur-level))))))

;; * provide
(provide 'entropy-emacs-structure)
