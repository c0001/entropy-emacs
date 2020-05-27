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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-hydra-hollow)

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
  :eemacs-tpha
  (((:enable t))
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
      (funcall #'yafolding-show-all)))

  :eemacs-tpha
  (((:enable t))
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
    (require 'vimish-fold)
    (let (rtn)
      (catch :exit
        (cl-destructuring-bind (beg . end) (vimish-fold--correct-region beg end)
          (dolist (overlay (overlays-in beg end))
            (when (vimish-fold--vimish-overlay-p overlay)
              (goto-char (overlay-start overlay))
              (setq rtn t)
              (throw :exit nil)))))
      rtn))

  (defun entropy/emacs-structure-vimish-toggle (beg end)
    (interactive "r")
    (require 'vimish-fold)
    (deactivate-mark)
    (if (entropy/emacs-structure--vimish-folded-p
         (line-beginning-position)
         (line-end-position))
        (call-interactively #'vimish-fold-toggle)
      (vimish-fold beg end)))

  (defun entropy/emacs-structure-vimish-fold-lisp-doc-string ()
    "Fold lisp type doc string block using `vimish'."
    (interactive)
    (require 'vimish-fold)
    (let ((head-dquote-pt
           (save-excursion (re-search-backward "^\\s-*\"")))
          (end-dquote-pt
           (save-excursion (re-search-forward "\"\\s-*$"))))
      (if (entropy/emacs-structure--vimish-folded-p
           (line-beginning-position)
           (line-end-position))
          (call-interactively #'entropy/emacs-structure-vimish-toggle)
        (entropy/emacs-structure-vimish-toggle
         head-dquote-pt end-dquote-pt))))

  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b q"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'vimish-fold))
      "Vimish Mode"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t)
    (vimish-fold (vimish-fold vimish-fold-folded-keymap) nil (2 2)))
   ("Vimish toggle"
    (("<C-tab>" entropy/emacs-structure-vimish-toggle
      "Automatically vimish fold/show region"
      :enable t :global-bind t :exit t))
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
      :enable t :exit t))
    "Vimish unfold"
    (("u c" vimish-fold-unfold "Unfold at point"
      :enable t :exit t)
     ("u a" vimish-fold-unfold-all "Unfold all folds in current buffer"
      :enable t :exit t))))

  :eemacs-mmphca
  (((:enable t)
    (emacs-lisp-mode (elisp-mode emacs-lisp-mode-map) nil (2)))
   ("Doc string"
    (("C-c C-o"
      entropy/emacs-structure-vimish-fold-lisp-doc-string
      "Quickly vimish fold doc string."
      :enable t :exit t :map-inject t))))

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
  (((:enable t))
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

  (defun entropy/emacs-structure--outorg-edit-unlock-buffer (&rest _)
    (when buffer-read-only
      (if (not (y-or-n-p "Buffer is read-only - make writable "))
          (error "Cannot edit read-only buffer")
        (with-current-buffer (current-buffer)
          (read-only-mode 0)))))
  (advice-add 'outorg-edit-as-org
              :before
              'entropy/emacs-structure--outorg-edit-unlock-buffer)

  (defun entropy/emacs-structure--outorg-edit-exit-unlock-code-buffer (&rest _)
    (let ((buffer (marker-buffer outorg-code-buffer-point-marker)))
      ;; unlock source code buffer
      (when (and (buffer-live-p (get-buffer buffer))
                 (with-current-buffer buffer buffer-read-only))
        (with-current-buffer buffer
          (read-only-mode 0)))

      ;; unlock edit buffer
      (when buffer-read-only
        (read-only-mode 0))))
  (advice-add 'outorg-copy-edits-and-exit :before
              #'entropy/emacs-structure--outorg-edit-exit-unlock-code-buffer))

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
   ("C-<f7>" . entropy/emacs-structure--outshine-reload-major))

;; *** preface
  :preface
  (defvar-local entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode nil)

  (defun entropy/emacs-structure--outshine-reload-major ()
    "Reload lisp(e.g. lisp elisp or more like one) buffer's outshine
    feataure with mordern \"*\" or old-school \";\" headline visual type
    set."
    (interactive)
    (when (and (bound-and-true-p outshine-mode)
               (entropy/emacs-buffer-is-lisp-like-p))
      (outshine-mode -1)
      (let ((mode major-mode))
        (with-current-buffer (current-buffer)
          (setq-local
           entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode
           (null
            entropy/emacs-structure--outshine-force-use-old-school-type-in-lisp-mode))
          (outshine-mode)
          ))))

  (defun entropy/emacs-structure--outshine-cycle-buffer (&optional arg)
    (interactive "P")
    (when (bound-and-true-p outshine-mode)
      (funcall 'outshine-cycle-buffer arg)))

  (defun entropy/emacs-structure--outshine-pop-imenu (&optional args)
    (interactive)
    (when (bound-and-true-p outshine-mode)
      (outshine-imenu)))

;; *** eemacs top key bind
  :eemacs-tpha
  (((:enable t))
   ("Structure"
    (("\\" entropy/emacs-structure--outshine-cycle-buffer
      "Outshine Cycle"
      :enable t
      :exit t
      :eemacs-top-bind t)
     ("M-i" entropy/emacs-structure--outshine-pop-imenu
      "Outshine popup imenu"
      :enable t
      :exit t
      :eemacs-top-bind t))))

;; *** init

  :init
  (entropy/emacs-lazy-with-load-trail
   enable-outshine-for-opened-buffer
   (mapc (lambda (buffer)
           (with-current-buffer buffer
             (when (member major-mode '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
               (outshine-mode +1))))
         (buffer-list)))

;; *** config

  :config

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

  (defun entropy/emacs-structure--outshine-set-outline-regexp-base (orig-func &rest orig-args)
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
              :around
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

  (defun entropy/emacs-structure--outshine-gen-face-keywords (outline-regexp times)
    (let ((outline-regex-head (substring outline-regexp
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
             (overflow (- outshine-max-level max-face-level))
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

  (defun entropy/emacs-structure--outshine-fontify-headlines (orig-func &rest orig-args)
    "Calculate heading regexps for font-lock mode."
    (entropy/emacs-structure--outshine-batch-gen-outshine-level-faces)
    (let* ((outline-regexp (car orig-args))
           (font-lock-new-keywords
            (entropy/emacs-structure--outshine-gen-face-keywords
             outline-regexp
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

  (defun entropy/emacs-structure--outshine-demote (&optional which)
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (funcall 'entropy/emacs-structure--outshine-sub-of-outline-demote
             which))

  (defun entropy/emacs-structure--outshine-promote (&optional which)
    (interactive
     (list (if (and transient-mark-mode mark-active) 'region
             (outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
    (funcall 'entropy/emacs-structure--outshine-sub-of-outline-promote
             which))

  (outshine-define-key outshine-mode-map
    (kbd "M-S-<left>") 'entropy/emacs-structure--outshine-promote
    (outline-on-heading-p))
  (outshine-define-key outshine-mode-map
    (kbd "M-S-<right>") 'entropy/emacs-structure--outshine-demote
    (outline-on-heading-p))

  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer
    (or (outline-on-heading-p) (bobp)
        (error "Using it out of the headline was not supported.")))

  (outshine-define-key outshine-mode-map
    (kbd "C-c C-p") 'outline-up-heading
    t)


  )

;; * provide
(provide 'entropy-emacs-structure)
