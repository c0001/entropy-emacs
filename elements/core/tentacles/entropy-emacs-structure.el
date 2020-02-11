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
  (entropy/emacs-!set-key (kbd "M--") 'entropy/emacs-structure-toggle-hiding)
  (entropy/emacs-!set-key (kbd "M-=") 'entropy/emacs-structure-toggle-selective-display))

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
  :init
  (entropy/emacs-!set-key (kbd "M--") #'entropy/emacs-structure-yaf-toggle)
  (entropy/emacs-!set-key (kbd "M-=") #'entropy/emacs-structure-yaf-show-all))

;; ** outorg
(use-package outorg
  :commands (outorg-edit-as-org
             outorg-edit-comments-and-propagate-changes
             outorg-copy-edits-and-exit)
  :init
  (entropy/emacs-!set-key (kbd "o") 'outorg-edit-as-org)
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
        (while (re-search-forward (format "^%s+ *$" comment-start) nil t)
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
  (:map org-mode-map
   ("C-c M-t" . nil)
   ("C-c M-e" . nil)
   ("C-c M-p" . nil)
   ("C-c M-y" . nil)
   :map outshine-mode-map
   ("C-x n s" . org-narrow-to-subtree)
   ("C-<f7>" . entropy/emacs-structure--outshine-reload-major))
  
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
  
  :init
  (entropy/emacs-!set-key
    (kbd "\\")
    'entropy/emacs-structure--outshine-cycle-buffer)

  (entropy/emacs-!set-key
    (kbd "M-i")
    'entropy/emacs-structure--outshine-pop-imenu)
  
  (entropy/emacs-lazy-initial-for-hook
   '(emacs-lisp-mode-hook lisp-interaction-mode-hook)
   "outshine-mode" "outshine-mode"
   (outshine-mode +1)
   (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
   (add-hook 'lisp-interaction-mode-hook 'outshine-mode))
  (entropy/emacs-lazy-with-load-trail
   enable-outshine-for-scratch-buffer
   (mapc (lambda (buffer)
           (when (string= (buffer-name buffer) "*scratch*")
             (with-current-buffer buffer
               (outshine-mode +1))))
         (buffer-list)))
  
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
    (let ((rtn (apply orig-func orig-args)))
      (unless (string-match-p "^\\^" rtn)
        (setq rtn (concat "^" rtn)))
      rtn))

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
  
  (defun entropy/emacs-structure--outshine-gen-face-keywords (outline-regexp times)
    (let ((outline-regex-head (substring outline-regexp 0 -10))
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
