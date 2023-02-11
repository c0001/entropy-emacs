;;; entropy-emacs-codeserver.el --- entropy-emacs code server configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20191014  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/core/tentacles/entropy-emacs-codeserver.el
;; Compatibility: GNU Emacs emacs-25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
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
;; The language server integrating configuration
;;
;; * Configuration:
;;
;; Designed for entropy-emacs only.
;;
;; * Code:

;; ** libraries

(defun entropy/emacs-codeserver--bounds-same-p
    (cur-bounds orig-bounds)
  "Judge the symbol bounds whether is same as each other
i.e. before change and after thus.

Bounds is an cons of (beg . end) point of `current-buffer'"
  (ignore-errors
    (and
     ;; current bound head is same or larger than orig bounds head
     (>= (car cur-bounds) (car orig-bounds))
     (or
      ;; -- current bound tail is witin the orig one
      (<= (cdr cur-bounds) (cdr orig-bounds))
      ;; -- out of range but on the same line and point at
      ;;    non-symbol place or the continuous of previous hints
      (and
       ;; current position is in the same line as orig-bounds at
       (< (save-excursion
            (re-search-backward "\n" nil t)
            (point))
          (car orig-bounds))
       (or
        ;; and there's on non symbol place
        (not
         (and (symbol-at-point)
              (bounds-of-thing-at-point 'symbol)))
        (string-prefix-p (buffer-substring
                          (car orig-bounds)
                          (cdr orig-bounds))
                         (buffer-substring
                          (car cur-bounds)
                          (cdr cur-bounds))))
       )))))

;; ** hydra hollow

(entropy/emacs-lazy-initial-advice-before
 entropy/emacs-ide-for-them
 "eemacs-IDE-dispatcher-hydra-hollow-init"
 "eemacs-IDE-dispatcher-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'eemacs-ide-hydra nil
  '("Server" ()
    "Diagnostics" ()))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Basic"
    (("b l"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'eemacs-ide-hydra))
      "eemacs IDE dispatcher"
      :enable t)))))

;; ** xref jumping
(use-package xref
  :ensure nil
  :defines
  (xref--marker-ring xref--history)
  :commands
  (xref-show-location-at-point
   xref-query-replace-in-results
   xref-find-definitions-other-window
   xref-prev-line
   xref-revert-buffer
   xref-find-apropos
   xref-find-references
   xref-find-definitions-at-mouse
   xref-next-line
   xref-etags-mode
   xref-find-definitions-other-frame
   xref-goto-xref
   xref-quit-and-goto-xref
   xref-pop-marker-stack
   xref-find-definitions)
;; *** eemacs hydra hollow instance
  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (prog-mode-hook) :adtype hook :pdumper-no-end t)))
    (xref-mode))
   ("Identifier find"
    (("M-." xref-find-definitions "Find the definition of the identifier at point"
      :enable t :exit t :global-bind t)
     ("M-," (:eval (cond ((= emacs-major-version 29) 'xref-go-back)
                         ((< emacs-major-version 29) 'xref-pop-marker-stack)
                         (t (error "xref-pop-marker-stack patch failed: unknow emacs version %s"
                                   emacs-version))))
      "Pop back to where M-. was last invoked"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (prog-mode-hook) :adtype hook :pdumper-no-end t))))
   ("Basic"
    (("b x"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'xref-mode))
      "Xref referrence jumping"
      :enable t :exit t))))

;; *** config
  :config

;; **** eemacs spec
;; ***** Disable xref in minibuffer prevents nesting looping unpredictable.
  (dolist (key (list "M-." "C-." "M-," "C-M-."))
    (dolist (map-ob '((ivy-minibuffer-map . ivy)
                      (minibuffer-inactive-mode-map . minibuffer)
                      ;; Although its a native map but unified for
                      ;; minibuffer feature autoloading as a fake did.
                      (minibuffer-local-map . minibuffer)))
      (entropy/emacs-lazy-load-simple (cdr map-ob)
        :lexical-bindings `((map-ob . ,map-ob)
                            (key . ,key))
        (define-key (symbol-value (car map-ob))
                    (kbd key)
                    (lambda ()
                      (interactive)
                      (message
                       "You can not using xref functions in minibuffer"
                       ))))))

;; ***** Dwim with `entropy-emacs-structure'
  (entropy/emacs-!cl-defun entropy/emacs-codeserver-xref--show-entry-after-jump (&rest _)
    "Show hidden entry after xref jump which hidden by
`entropy-emacs-structure' feature."
    (when-let* ((feature-p (featurep 'entropy-emacs-structure))
                (pt (point))
                (ovs (overlays-at (point))))
      ;; firstly show outline entry
      (when (or (bound-and-true-p outline-mode)
                (bound-and-true-p outline-minor-mode))
        (save-excursion
          (condition-case err
              (outline-show-entry)
            (outline-before-first-heading
             (entropy/emacs-!message "outline-before-first-heading"))
            (error
             (entropy/emacs-!error "%s" err)))))

      ;; then remove `hs-minor-mode' hidden overlay
      (when-let ((hsmode-p (bound-and-true-p hs-minor-mode))
                 (hs-line-is-hide-p
                  (save-excursion
                    (hs-already-hidden-p))))
        (save-excursion
          (hs-show-block)))

      ;; then remove `vimish' hidden overlay
      (when (and ovs
                 (fboundp 'vimish-fold--vimish-overlay-p)
                 (fboundp 'vimish-fold-delete)
                 (catch :exit
                   (dolist (ov ovs)
                     (when (vimish-fold--vimish-overlay-p ov)
                       (throw :exit t)))
                   nil))
        (save-excursion
          (vimish-fold-delete)))

      ;; then remove `yafolding' hidden overlay
      (when (and (fboundp 'yafolding-get-overlays)
                 (save-excursion
                   (yafolding-get-overlays
                    (line-beginning-position)
                    (+ 1 (line-end-position)))))
        (save-excursion
          (yafolding-show-element)))

      ;; Finally we push the mark for be convenience jumping with
      ;; `entropy/emacs-basic-mark-set'
      (push-mark pt)))

  (add-hook 'xref-after-jump-hook
            #'entropy/emacs-codeserver-xref--show-entry-after-jump
            100)

  ;; EEMACS_MAINTENANCE: follow upstream's internal defination.
  (let ((fname '__ya/xref-pop-marker-stack))
    (cond
     ((= emacs-major-version 29)
      (defalias fname
        (lambda (&rest _)
          "Like `xref-go-back' but use `xref--goto-char' when the
popup destination position in buffer who is narraowed where the
accessible portion is outside of that position."
          (interactive)
          (if (null (car xref--history))
              (user-error "At start of xref history")
            (let ((marker (pop (car xref--history))))
              (xref--push-forward (point-marker))
              (switch-to-buffer (or (marker-buffer marker)
                                    (user-error "The marked buffer has been deleted")))
              (xref--goto-char (marker-position marker))
              (set-marker marker nil nil)
              (run-hooks 'xref-after-return-hook)))))
      (advice-add 'xref-go-back :override fname))
     ((<= emacs-major-version 28)
      (defalias fname
        (lambda (&rest _)
          "Like `xref-pop-marker-stack' but use `xref--goto-char' when the
popup destination position in buffer who is narraowed where the
accessible portion is outside of that position."
          (interactive)
          (let ((ring
                 ;; bypass byte-compile warning in emacs-29
                 (bound-and-true-p xref--marker-ring)))
            (when (or (null ring) (ring-empty-p ring))
              (user-error "Marker stack is empty"))
            (let ((marker (ring-remove ring 0)))
              (switch-to-buffer (or (marker-buffer marker)
                                    (user-error "The marked buffer has been deleted")))
              (xref--goto-char (marker-position marker))
              (set-marker marker nil nil)
              (run-hooks 'xref-after-return-hook)))))
      (advice-add 'xref-pop-marker-stack :override fname))
     (t (error "xref-pop-marker-stack patch failed: unknow emacs version %s"
               emacs-version))))

;; *** __end__
  )


;; ** eldoc

(entropy/emacs-lazy-initial-for-hook
 '(prog-mode-hook)
 "eldoc-mode-init" "eldoc-mode-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (defun entropy/emacs-eldoc-show-eldoc-for-current-point
     (&optional inct)
   "Like `eldoc' but do not needed `eldoc-mode' enabled at
before invocation."
   (interactive '(t))
   (condition-case error
       (unless (or
                ;; do not invoking in company refer map
                (bound-and-true-p company-candidates)
                ;; TODO: more conditions
                )
         (let ((buff (current-buffer))
               (eldoc-enable-p (bound-and-true-p eldoc-mode))
               (entropy/emacs-eldoc-inhibit-in-current-buffer nil))
           (with-current-buffer buff
             (unless eldoc-enable-p
               (eldoc-mode 1)))
           (funcall
            'eldoc-print-current-symbol-info inct)
           ;; unbind temporally `eldoc-mode' enabled in source buffer
           (with-current-buffer buff
             (unless eldoc-enable-p
               (eldoc-mode 0)))))
     (error
      (message "%s" error))))

 (global-eldoc-mode 1)

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Basic"
    (("M-h" entropy/emacs-eldoc-show-eldoc-for-current-point
      "Document thing at point."
      :enable t
      :exit t
      :global-bind t)))))

;; ** Diagnostics

;; *** eemacs diagnostics framework

(defun entropy/emacs-codeserver-show-diagnostics ()
  "Show diagnostics for `current-buffer' when availalble eemacs
diagnostic feature is actived."
  (interactive)
  (cond
   ((and (bound-and-true-p flycheck-mode)
         ;; just use flycheck as diagnostic show in lsp session while
         ;; the lsp client is 'lsp-mode' since eglot use flymake by
         ;; default.
         (eq entropy/emacs-ide-use-for-all 'lsp))
    (flycheck-list-errors))
   ((bound-and-true-p flymake-mode)
    (flymake-show-diagnostics-buffer))
   (t
    (user-error "can not show diagnostics for current buffer! \
Because of no suitable backend actived yet."))))

(entropy/emacs-lazy-initial-for-hook
 '(prog-mode-hook)
 "emacs-ide-diagnostic-hydra-hollow-init"
 "emacs-ide-diagnostic-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define+
  'eemacs-ide-hydra nil
  '("Diagnostics"
    (("M-e" entropy/emacs-codeserver-show-diagnostics
      "Show diagnostics for current-buffer"
      :enable t
      :exit t
      :eemacs-top-bind t)))))

;; **** flymake
(use-package flymake
  :ensure nil
  :commands (flymake-show-diagnostics-buffer)
  :init
  ;; use eemacs union ide diagnostics idle delay
  (setq flymake-no-changes-timeout entropy/emacs-ide-diagnostic-delay)

  (setq flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t
        ;; disable the obsolete option
        ;; `flymake-start-syntax-check-on-find-file' which use
        ;; `flymake-start-on-flymake-mode' instead
        flymake-start-on-flymake-mode nil))

;; **** flycheck
(use-package flycheck
  :commands (flycheck-list-errors)
  :init
  (setq
   ;; disable flycheck status mode line indicator
   flycheck-mode-line nil
   ;; disable buffer warning messy highlight
   flycheck-highlighting-mode nil
   flycheck-idle-buffer-switch-delay 1
   ;; defautly do not echo errors which is noisily
   flycheck-display-errors-function nil
   flycheck-display-errors-delay 0.9
   ;; Use eemacs union diagnostics delay
   flycheck-idle-change-delay entropy/emacs-ide-diagnostic-delay)

  :config
  (defun __adv/around/flycheck-buffer/0
      (orig-func &rest orig-args)
    "Remove error messsage while flycheck disabled with lsp mode,
since `lsp-diagnostics--flycheck-report' will re-add
`lsp-diagnostics--flycheck-buffer' to `lsp-on-idle-hook' even if
`lsp-diagnostics-mode' is disabled while `lsp-managed-mode' is
enabled.

NOTE: its an `lsp-mode' bug."
    (if (member 'lsp-on-change after-change-functions)
        (if (bound-and-true-p flycheck-mode)
            (apply orig-func orig-args))
      (apply orig-func orig-args)))
  (advice-add 'flycheck-buffer
              :around
              '__adv/around/flycheck-buffer/0)

  ;; FIXME: this is needed while pdumper session, why?
  (defun __ya/flycheck-set-indication-mode/for-pdumper (&optional mode)
    "Like `flycheck-set-indication-mode' but for pdumper session
only which use both of margin and fringe since the fringe for
flycheck show error indication will lost in pdumper session."
    (setq mode 'left-margin)
    (setq left-fringe-width 8 right-fringe-width 4
          left-margin-width 2 right-margin-width 2)
    (setq-local flycheck-indication-mode mode)
    (flycheck-refresh-fringes-and-margins))
  (when entropy/emacs-fall-love-with-pdumper
    (defun __hook/flycheck-mode-hook/0 (&rest _)
      "Trigger margin indicator on mode init.
FIXME: this is needed while pdumper session, why?"
      (when (bound-and-true-p flycheck-mode)
        (flycheck-set-indication-mode 'left-margin)))
    (add-hook 'flycheck-mode-hook #'__hook/flycheck-mode-hook/0)
    (advice-add 'flycheck-set-indication-mode
                :override
                #'__ya/flycheck-set-indication-mode/for-pdumper))

  )

;; ** common server
;; *** individual servers
;; **** irony server
(defun entropy/emacs-codeserver-irony-load-subs ()
  (dolist (el '(irony-cdb-clang-complete
                irony-cdb-json
                irony-cdb-libclang
                irony-cdb
                irony-completion
                irony-diagnostics
                irony-iotask
                irony-snippet))
    (require el)))

(defun entropy/emacs-codeserver-irony-pipe-config ()
  "Reducing pipe-read-delay and set the pipe buffer size to
64K on Windows (from the original 4K).

It is the recommendation of irony-mode official introduction."
  (when (boundp 'w32-pipe-read-delay)
    (setq-local w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq-local irony-server-w32-pipe-buffer-size (* 64 1024))))

(defun entropy/emacs-codeserver-irony-refer-advice-around (oldfuc &rest args)
  "Prevent c group mode as `php-mode' which was the derived from
`c-mode' to load `irony-mode' and `company-irony'."
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (apply oldfuc args)
    t))

(defun entropy/emacs-codeserver-usepackage-irony ()
  "Function for enabling irony mode for c and c++ mode."
  (use-package irony
    :commands (irony-mode)
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options)
           (c-mode . entropy/emacs-codeserver-irony-pipe-config)
           (c++-mode . entropy/emacs-codeserver-irony-pipe-config))
    :config
    (entropy/emacs-codeserver-irony-load-subs)
    (advice-add 'irony-mode :around #'entropy/emacs-codeserver-irony-refer-advice-around))

  (use-package irony-eldoc
    :commands (irony-eldoc)
    :hook (irony-mode . irony-eldoc)))

;; **** tern server
(defun entropy/emacs-codeserver-usepackage-tern ()
  (use-package tern
    :commands (tern-mode)
    :hook (js2-mode . tern-mode)))

;; **** anaconda server
(defun entropy/emacs-codeserver-usepackage-anaconda ()
  (use-package anaconda-mode
    :diminish anaconda-mode
    :commands anaconda-mode
    :bind (:map anaconda-mode-map
                ("M-," . anaconda-mode-go-back))
    :init
    (add-hook 'python-mode-hook #'anaconda-mode)))

;; **** ac-php
(defun entropy/emacs-codeserver-usepackage-ac-php ()
  (use-package ac-php
    :requires php-mode
    :commands ac-php-core-eldoc-setup
    :init
    (add-hook 'php-mode-hook #'(lambda () (ac-php-core-eldoc-setup)))
    (add-hook 'php-mode-hook
              #'(lambda ()
                  (setq-local indent-tabs-mode nil)
                  (setq-local c-basic-offset 4)
                  (subword-mode 1)))))

;; *** startup

(when (eq (entropy/emacs-get-use-ide-type 'c-mode) 'traditional)
  (cond
   ((and sys/win32p
         entropy/emacs-win-portable-mingw-enable
         (file-exists-p (expand-file-name
                         "libclang.dll"
                         entropy/emacs-win-portable-mingw-bin-path)))
    (entropy/emacs-codeserver-usepackage-irony))
   (sys/is-posix-compatible
    (entropy/emacs-codeserver-usepackage-irony))))

(when (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-tern))
(when (eq (entropy/emacs-get-use-ide-type 'python-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-anaconda))
(when (eq (entropy/emacs-get-use-ide-type 'php-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-ac-php))

;; ** microsoft language server
;; *** lsp-client

;; **** uniform

;; ***** idle delay uniform
(defvar entropy/emacs-codeserver--codeserver-doc-show-timer-watch-registry nil)
(defun entropy/emacs-codeserver--codeserver-doc-show-delay-var-watcher
    (symbol newval operation _where)
  (when (eq operation 'set)
    (let ((timer-obj (alist-get symbol entropy/emacs-codeserver--codeserver-doc-show-timer-watch-registry)))
      (cond (timer-obj
             (let ((timer-var (car timer-obj))
                   (timer-func (cdr timer-obj)))
               (when (timerp (symbol-value timer-var))
                 (cancel-timer (symbol-value timer-var))
                 (set timer-var nil)
                 (cancel-function-timers timer-func)
                 (set timer-var
                      (run-with-idle-timer newval t timer-func)))))
            ((eq symbol 'entropy/emacs-ide-doc-delay)
             (let (_)
               (dolist (item entropy/emacs-codeserver--codeserver-doc-show-timer-watch-registry)
                 (let ((var (car item)))
                   (set var newval)))
               ))))))

(defmacro entropy/emacs-coceserver--codeserver-doc-show-timer-watcher-regist
    (idle-var idle-timer-var idle-timer-func)
  "Watched the IDLE-VAR modification to auto re-trigger the
IDLE-TIMER-FUNC ran as the subrotine of timer IDLE-TIMER-VAR.

This macro is an uniform for eemacs to re-trigger codeserver doc
show idle delay, thus so any modification of var
`entropy/emacs-ide-doc-delay' will set the IDLE-VAR and
re-trigger the timer IDLE-TIMER-VAR."
  `(progn
     (add-to-list 'entropy/emacs-codeserver--codeserver-doc-show-timer-watch-registry
                  '(,idle-var . (,idle-timer-var . ,idle-timer-func)))
     (add-variable-watcher
      ',idle-var #'entropy/emacs-codeserver--codeserver-doc-show-delay-var-watcher)))

(add-variable-watcher
 'entropy/emacs-ide-doc-delay
 #'entropy/emacs-codeserver--codeserver-doc-show-delay-var-watcher)


;; ***** lsp defer condition filter

(defun entropy/emacs-codeserver--codeserver-union-startjudge-filter-advice-form
    (orig-func &rest orig-args)
  "Around advice for codeserver start for get exclusions for
certain eemacs-spec conditions."
  (let* ((buff (current-buffer))
         (buff-name (buffer-name buff))
         (buff-fname (buffer-file-name buff))
         (buff-exclusions
          (rx (or (seq line-start "*Org Src")
                  (seq line-start " *org-src-fontification:")
                  (seq line-start " markdown-code-fontification:"))))
         (mode-exclusions
          '(emacs-lisp-mode
            lisp-interaction-mode
            lisp-mode
            org-mode)))
    (unless (or
             ;; just buffer with file can be start codeserver
             (not buff-fname)
             ;; `major-mode' filter
             (member major-mode mode-exclusions)
             ;; `buffer-name' filter
             (string-match-p buff-exclusions buff-name)
             ;; exclude unwriteable file since they usually are system
             ;; ones which should not be tracked and may have hudge of
             ;; files to tracking on its workspace.
             (and buff-fname
                  (not (file-writable-p buff-fname)))
             ;; a sudo tramp file since its used as fake tramp
             ;; remotion which maybe messy up system
             (and buff-fname
                  (file-remote-p buff-fname)
                  (or
                   (string-match-p "^/sudo:" buff-fname)
                   (string-match-p "^/sudoedit:" buff-fname)))
             )
      (apply orig-func orig-args))))


;; ***** ignoring watching files/directories

(entropy/emacs-!cl-defun entropy/emacs-codeserver--union-ignore-watchings
    (cache)
  (let (type var)
    (dolist (el cache)
      (setq type (car el) var (cdr el))
      (cond
       ((eq type 'file)
        (dolist (f (reverse entropy/emacs-codeserver-file-watch-ignored-files))
          (add-to-list var f)))
       ((eq type 'dir)
        (dolist (d (reverse entropy/emacs-codeserver-file-watch-ignored-directories))
          (add-to-list var d)))
       (t (entropy/emacs-!error "invalid type `%s'" type))))))

;; **** lsp-mode
;; ****** lsp-mode core
(use-package lsp-mode
  :diminish lsp-mode
  :commands (lsp lsp-mode lsp-deferred

                 lsp-disconnect
                 lsp-rename
                 lsp-document-highlight
                 lsp-signature-activate
                 lsp-organize-imports
                 lsp-execute-code-action

                 lsp-workspace-restart
                 lsp-workspace-shutdown
                 lsp-workspace-folders-add
                 lsp-workspace-folders-remove
                 lsp-workspace-blacklist-remove

                 lsp-describe-session
                 lsp-describe-thing-at-point

                 lsp-format-buffer
                 lsp-format-region

                 lsp-lens-mode
                 lsp-avy-lens

                 lsp-toggle-trace-io
                 lsp-toggle-symbol-highlight
                 lsp-toggle-signature-auto-activate
                 lsp-toggle-on-type-formatting

                 lsp-find-definition
                 lsp-find-references
                 lsp-find-implementation
                 lsp-find-type-definition
                 lsp-find-declaration)

;; ******* preface
  :preface

  (defun entropy/emacs-codeserver-lsp-mode-shutdown
      (&optional buffer)
    "Fully disable all related buffer's of `lsp-mode' feature and
shutdown those lsp client connections for the current lsp-mode
workspace when available."
    (interactive)
    (with-current-buffer (or buffer (current-buffer))
      (let (cur-wps
            enabled-buffers
            (feature-disable-func
             (lambda (this-buff)
               (when (buffer-live-p this-buff)
                 (with-current-buffer this-buff
                   (when (bound-and-true-p lsp-ui-mode)
                     (lsp-ui-mode 0))
                   (when (bound-and-true-p lsp-mode)
                     (lsp-mode 0)))))))
        (when
            (ignore-errors (setq cur-wps (lsp-workspaces)))
          ;; get wp enabed buffers
          (dolist (wp cur-wps)
            (mapc
             (lambda (wp-buff)
               (push wp-buff enabled-buffers))
             (lsp--workspace-buffers wp)))
          ;; shut down wps
          (dolist (wp cur-wps)
            (lsp-workspace-shutdown wp))
          ;; disable featured buffers' feature
          (dolist (wp-buff enabled-buffers)
            (funcall feature-disable-func wp-buff))
          ))))

  (defun entropy/emacs-codeserver-lsp-mode-shutdown-all ()
    "Fully disable all buffer's `lsp-mode' feature and shutdown
those lsp client connections.

The non-buffer associated lsp server process are not being
shutdown since it is managed by the customize variable
`lsp-keep-workspace-alive'."
    (interactive)
    (dolist (buff (buffer-list))
      (when (and (buffer-live-p buff)
                 (not (minibufferp buff)))
        (entropy/emacs-codeserver-lsp-mode-shutdown
         buff))))

;; ******* eemacs-indhc
  :eemacs-indhc
  (((:enable t :defer t)
    (lsp-mode nil nil (2 2 2 1 1)))
   ("Basic"
    (("b s" lsp "Start lsp server for current workspace"
      :enable t :exit t)
     ("b d" lsp-disconnect "Disconnect lsp server for current workspace"
      :enable t :exit t)
     ("b r" lsp-rename "Rename the symbol (and all references to it)"
      :enable t :exit t)
     ("b h" lsp-document-highlight "Highlight all relevant references at point"
      :enable t :exit t))
    "Workspace"
    (("w r" lsp-workspace-restart "Restart the workspace WORKSPACE"
      :enable t :exit t)
     ("w s" lsp-workspace-shutdown "Shut the *CURRENT* lsp WORKSPACE"
      :enable t :exit t)
     ("w k" entropy/emacs-codeserver-lsp-mode-shutdown
      "Fully disable *CURRENT* lsp WORKSPACE's buffers ‘lsp-mode’ feature and servers"
      :enable t :exit t)
     ("w c" entropy/emacs-codeserver-lsp-mode-shutdown-all
      "Disable all lsp WORKSPACEs and refer buffers 'lsp-mode' features and servers"
      :enable t :exit t)
     ("w a" lsp-workspace-folders-add "Add PROJECT-ROOT to the list of workspace folders"
      :enable t :exit t)
     ("w d" lsp-workspace-folders-remove "Remove PROJECT-ROOT from the list of workspace folders"
      :enable t :exit t)
     ("w l" lsp-workspace-blacklist-remove "Remove PROJECT-ROOT from the workspace blacklist"
      :enable t :exit t))
    "Describe"
    (("d s" lsp-describe-session "Describes current 'lsp-session'"
      :enable t :exit t)
     ("M-h" lsp-describe-thing-at-point "Display the type signature and documentation"
      :enable t :map-inject t :exit t))
    "Format"
    (("f b" lsp-format-buffer "Ask the server to format this document"
      :enable t :exit t)
     ("f r" lsp-format-region "Ask the server to format the region"
      :enable t :exit t))
    "Lens"
    (("l m" lsp-lens-mode "Toggle code-lens overlays"
      :enable t :exit t
      :toggle
      (if (bound-and-true-p lsp-lens-mode)
          t
        nil))
     ("l a" lsp-avy-lens "Click lsp lens using 'avy' package"
      :enable t :exit t))
    "Toggle"
    (("t t" lsp-toggle-trace-io "Toggle client-server protocol logging"
      :enable t :exit t)
     ("t h" lsp-toggle-symbol-highlight "Toggle symbol highlighting"
      :enable t :exit t)
     ("t s" lsp-toggle-signature-auto-activate "Toggle signature auto activate"
      :enable t :exit t)
     ("t f" lsp-toggle-on-type-formatting "Toggle on type formatting"
      :enable t :exit t))
    "Find"
    (("f d" lsp-find-definition "Find definitions of the symbol under point"
      :enable t :exit t)
     ("f r" lsp-find-references "Find references of the symbol under point"
      :enable t :exit t)
     ("f i" lsp-find-implementation "Find implementations of the symbol under point"
      :enable t :exit t)
     ("f t" lsp-find-type-definition "Find type definitions of the symbol under point"
      :enable t :exit t)
     ("f c" lsp-find-declaration "Find declarations of the symbol under point"
      :enable t :exit t))))

;; ******* eemacs-indhca
  :eemacs-indhca
  (((:enable t :defer t)
    (eemacs-ide-hydra))
   ("Server"
    (("s l"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'lsp-mode))
      "Lsp command map"
      :enable t :exit t))))

;; ******* init
  :init

  (dolist (el entropy/emacs-ide-for-them)
    (when (eq (entropy/emacs-get-use-ide-type el) 'lsp)
      (add-hook
       (intern (format "%s-hook" el))
       #'lsp-deferred)))

  (entropy/emacs-lazy-initial-advice-before
   '(lsp)
   "lsp-enable-yas"
   "lsp-enable-yas"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (require 'yasnippet)
   (unless (bound-and-true-p yas-minor-mode)
     (yas-minor-mode 1)))

  ;; fully quit `lsp-mode' charge
  (entropy/emacs-add-hook-with-lambda
    'disable-lsp-remains-when-quit (&rest _)
    :use-hook 'lsp-mode-hook
    (when (not (bound-and-true-p lsp-mode))
      (if (bound-and-true-p lsp-diagnostics-mode)
          (lsp-diagnostics-mode 0))
      (if (bound-and-true-p lsp-managed-mode)
          (lsp-managed-mode 0))))

;; ******** lsp union set
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure t)
  (setq
   ;; Disable large verbose log
   lsp-log-io nil)

  (setq
   ;; delete remaining workspace when last associate buffer is closed
   ;; to reduce memory usage and eemacs request of
   ;; `entropy/emacs-codeserver-lsp-mode-shutdown-all'
   lsp-keep-workspace-alive
   nil)

;; ******** lsp diagnostics set
  ;; Use flycheck prefer as diagnostics backend
  (defvar lsp-diagnostics-provider)
  (setq lsp-diagnostics-provider :auto
        ;; Use eemacs specific union diagnostic idle delay
        lsp-idle-delay entropy/emacs-ide-diagnostic-delay
        ;; Inhibit full workspace diagnostic clean for reduce lag
        lsp-diagnostic-clean-after-change nil)

;; ******** lsp eldoc set
  ;; Disable eldoc hover at init for reduce default lsp performance
  (setq lsp-eldoc-enable-hover nil)

;; ******** lsp signature set
  (setq lsp-signature-auto-activate nil
        ;; Set `lsp-signature-doc-lines' to 0 to restrict the echo
        ;; area lines to have more UI exps, so that only syntax line
        ;; are echoed.
        lsp-signature-doc-lines 0
        )

;; ******** lsp dap-mode set
  (setq
   ;; Forbidden auto enable `dap-mode', since we do not use it in
   ;; eemacs since its lag performance
   lsp-enable-dap-auto-configure nil)

;; ******** disable redudant functionalities to improve performance
  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-lens-enable nil
   lsp-semantic-tokens-enable nil)

  ;; Inhibit auto header insertion via lsp-cland client refer to
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2503
  (defvar lsp-clients-clangd-args)
  (setq lsp-clients-clangd-args '("--header-insertion=never"))

;; ******** lsp server init args specifications

  ;; + [2023-02-11 Sat 16:36:45]
  ;; Obsolete since typescript language server has removed
  ;; =--tsserver-log-file= option, and the *tsserver* will caught the
  ;; usage of this option as an error.
  ;; #+begin_src elisp
  ;; (defvar lsp-clients-typescript-server-args)
  ;; (setq lsp-clients-typescript-server-args
  ;;       `("--stdio"
  ;;         "--tsserver-log-file"
  ;;         ,(expand-file-name
  ;;           ".tsserver-log-file"
  ;;           entropy/emacs-temporary-file-directory)))
  ;; #+end_src
  ;;
  ;; + TODO :
  ;; Instead we use var =lsp-typescript-tsserver-log= to disable the
  ;; server inner bug verbosity to avoid create the log file in
  ;; workspace, but its the rude way that we should use the
  ;; =logDirectory= initial setting of tsserver to let user customie
  ;; the expection. But package =lsp-javascript= has harded coded its
  ;; initial option to tsserver without any custmizable setting of
  ;; =logDirectory= as what its =logVerbosity= option's defcustom var
  ;; =lsp-clients-typescript-log-verbosity= does.
  (setq lsp-typescript-tsserver-log "off")

;; ******* config
  :config

;; ******** var sets (must be set after load)

  (entropy/emacs-codeserver--union-ignore-watchings
   '((file . lsp-file-watch-ignored-files)
     (dir  . lsp-file-watch-ignored-directories)))

;; ******** add more language ids

  (entropy/emacs--api-restriction-uniform 'hahaa
      'package-version-incompatible
    :when (>= emacs-major-version 29)
    :do-error t
    :detector
    (not
     (let ((ver (substring
                 entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
                 1)))
       (and (entropy/emacs-version-compare
             '<= ver "3.0.8")
            (entropy/emacs-version-compare
             '>= ver "3.0.7"))))
    (defun __adv/lsp-bash-check-sh-shell/enable-for-bash-ts-mode
        (orig-func &rest orig-args)
      "Let `lsp-mode' known `bash-ts-mode' as same as `sh-mode' in emacs-29."
      (or (apply orig-func orig-args)
          (eq major-mode 'bash-ts-mode)))
    (with-eval-after-load 'lsp-bash
      (advice-add 'lsp-bash-check-sh-shell
                  :around
                  '__adv/lsp-bash-check-sh-shell/enable-for-bash-ts-mode))
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))
    (add-to-list 'lsp--formatting-indent-alist '(bash-ts-mode . sh-basic-offset)))

;; ******** advices
;; ********* require extra clients
  (advice-add 'lsp
              :before
              #'entropy/emacs-codeserver--lsp-mode-load-extra-clients)
  (defun entropy/emacs-codeserver--lsp-mode-load-extra-clients (&rest _)
    (dolist (feature entropy/emacs-codeserver-lsp-mode-extra-clients)
      (require feature)))

;; ********* remove the session file when close emacs
  ;; delete transient lsp session file for prevent lagging with large
  ;; amounts of folder parsing while next emacs session setup.
  (add-hook 'kill-emacs-hook
            #'entropy/emacs-codeserver--lsp-mode-remove-session-file)
  (defun entropy/emacs-codeserver--lsp-mode-remove-session-file ()
    (when (and (boundp 'lsp-session-file)
               (file-exists-p lsp-session-file))
      (delete-file lsp-session-file)))

;; ********* eemacs lsp start hack
;; ********** lsp start prompts
  (entropy/emacs-message-make-func-with-simple-progress-prompts 'lsp-deferred
    "%s <%s>"
    (green "Lsp check for buffer")
    (yellow (buffer-name)))
  (entropy/emacs-message-make-func-with-simple-progress-prompts 'lsp
    "%s"
    (green "Lsp starting"))

;; ********** start conditions filter

  (dolist (func '(lsp-deferred lsp))
    (let (_)
      (advice-add func
                  :around
                  #'entropy/emacs-codeserver--codeserver-union-startjudge-filter-advice-form
       )))

;; ********* lsp idle hook specifications
  (defvar entropy/emacs-codeserver--lsp-on-idle-cases
    '(
      ;; Remove `lsp--document-highlight' when current file is not
      ;; writeable in which case that those clients can not parse the
      ;; file in where frequently errors prompted.
      (lambda ()
        (unless (ignore-errors
                  (file-writable-p
                   (file-truename
                    (buffer-file-name (current-buffer)))))
          (remove-hook
           'lsp-on-idle-hook
           'lsp--document-highlight
           t))))
    "Cases of list of functions to run before hook `lsp-on-idle-hook' run."
    )

  (defun entropy/emacs-codeserver--run-lsp-on-idle-hooks-for-cases
      (orig-func &rest orig-args)
    "Around advice for `lsp--on-idle' for excuting further cases
predicate when run it, see
`entropy/emacs-codeserver--lsp-on-idle-cases'."
    (let* (_)
      (dolist (func entropy/emacs-codeserver--lsp-on-idle-cases)
        (when (functionp func)
          (funcall func)))
      (apply orig-func orig-args)))
  (advice-add 'lsp--on-idle
              :around
              #'entropy/emacs-codeserver--run-lsp-on-idle-hooks-for-cases)


;; ********* lsp rename without be under readonly pressure
  (entropy/emacs-make-function-inhibit-readonly 'lsp-rename t)

;; ********* fix bug of `lsp-diagnostics-flycheck-disable'
  (defun __adv/lsp-diagnostics-flycheck-disable/0
      (orig-func &rest orig-args)
    "Around advice to fix typo of `lsp-diagnostics-flycheck-disable'."
    (prog1
        (apply orig-func orig-args)
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode 0))))
  (advice-add 'lsp-diagnostics-flycheck-disable
              :around
              #'__adv/lsp-diagnostics-flycheck-disable/0)

;; ********* speed up `lsp--cur-line'

  (entropy/emacs-defvar-local-with-pml __eemacs-lsp-narrow-p nil)
  (defun __adv/around/narrow-to-region/lsp-restriction-deal
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (setq __eemacs-lsp-narrow-p t)
      rtn))

  ;; EEMACS_MAINTENANCE: need to follow lsp updates
  (defun __adv/around/lsp--cur-line/0
      (orig-func &rest orig-args)
    "Speed up `line-number-at-pos' used in `lsp--cur-line' while
emacs version lower than 28 which is implemented in elisp that
has lower performance and perforrmed in every command post to
improve buffer typing or scrolling performance in lsp actived
buffer, using modeline substitutes while ORIG-ARGS is empty, in
which case we have assumption that the lsp prober just use
`current-point' to did those detection or in buffer visual part
generally, and it is the most of cases we learn about the source
code of `lsp-mode' but no guarantees of whole mechanism, thus,
this patch may have messy on some special cases.

[Stick from]:
https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers
"
    (let* ((buff (current-buffer))
           (win (get-buffer-window buff))
           (pos (car orig-args)))
      (cond
       ((and
         (not pos)
         ;; do not patch while narrowed visualization, since the
         ;; modeline substitution may not did that correctly within
         ;; temporally `widen'
         ;; e.g. `lsp-save-restriction-and-excursion'.
         (not (bound-and-true-p __eemacs-lsp-narrow-p))
         (windowp win)
         (window-live-p win)
         ;; ensure the modeline arg expansion correct
         (eq win (selected-window)))
        (1-
         (string-to-number (format-mode-line "%l"))))
       (t
        ;; (message "use origi `lsp--cur-line'")
        (apply orig-func orig-args)))))

  (when (< emacs-major-version 28)
    (advice-add 'lsp--cur-line
                :around
                #'__adv/around/lsp--cur-line/0)
    (dolist (func '(
                    ;; FIXME: just advice to `narrow-to-region' can not
                    ;; reflect it as the role of subroutine of other
                    ;; functions?
                    narrow-to-region
                    narrow-to-defun
                    narrow-to-page
                    org-narrow-to-subtree
                    ;; TODO: add more ...
                    ))
      (advice-add func
                  :around
                  #'__adv/around/narrow-to-region/lsp-restriction-deal))
    (defun __adv/around/widen/lsp-restriction-deal
        (orig-func &rest orig-args)
      (let (_)
        (when (eq this-command 'widen)
          (setq __eemacs-lsp-narrow-p nil))
        (apply orig-func orig-args)))
    (advice-add 'widen
                :around #'__adv/around/widen/lsp-restriction-deal))


;; ******** others
  ;; Remove clients not officially included in `lsp-mode' internal
  ;; subroutine to prevent from coverring eemacs customizations.
  (setq lsp-client-packages
        (delete nil
                (mapcar (lambda (x)
                          (unless (member x '(ccls lsp-python-ms lsp-java))
                            x))
                        lsp-client-packages)))


  ;; Hack for fixing lsp memory leak for reseponse handler
  ;; suggested from:
  ;; [[https://github.com/emacs-lsp/lsp-mode/issues/3062]]
  (defun __hack/bug_fix/lsp-client-clear-leak-handlers (lsp-client)
    "Clear leaking handlers in LSP-CLIENT."
    (let ((response-handlers (lsp--client-response-handlers lsp-client))
          to-delete-keys)
      (maphash (lambda (key value)
                 (when (> (time-convert (time-since (nth 3 value)) 'integer)
                          (* 2 lsp-response-timeout))
                   (push key to-delete-keys)))
               response-handlers)
      (when to-delete-keys
        (let ((gc-keys-len (length to-delete-keys))
              (client-id (lsp--client-server-id lsp-client)))
          (message "Deleting %d handlers in %s lsp-client..."
                   gc-keys-len
                   client-id)
          (mapc (lambda (k) (remhash k response-handlers))
                to-delete-keys)
          (message "")))))
  (defun __hack/bug_fix/lsp-clear-leak ()
    "Clear all leaks for all `lsp-clients'"
    (maphash (lambda (_ client)
               (__hack/bug_fix/lsp-client-clear-leak-handlers client))
             lsp-clients))
  (defvar __hack/bug_fix/lsp-clear-leak-timer nil)
  (setq __hack/bug_fix/lsp-clear-leak-timer
        (run-with-idle-timer 5 t #'__hack/bug_fix/lsp-clear-leak))

  )

;; ****** lsp-mode UI
(use-package lsp-ui
;; ******* preface

  :preface
  (defvar entropy/emacs-codeserver-lsp-ui-doc-timer nil)

;; ******* commands and binds
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))

;; ******* eemacs-indhc
  :eemacs-indhc
  (((:enable t :defer t)
    (lsp-ui-mode nil nil (1 2 2)))
   ("Doc (Basic)"
    (("d t" lsp-ui-mode "Toggle language server UI mode on or off"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-mode) t nil))
     ("d d" lsp-ui-doc-mode "Minor mode for showing hover information in child frame"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-doc-mode) t nil))
     ("d e"
      (if (ignore-errors (member 'lsp-ui-doc-mode lsp-mode-hook))
          (progn
            (remove-hook 'lsp-mode-hook 'lsp-ui-doc-mode)
            (when (bound-and-true-p lsp-ui-doc-mode)
              (lsp-ui-doc-mode 0)))
        (add-hook 'lsp-mode-hook 'lsp-ui-doc-mode)
        (dolist (buff (buffer-list))
          (with-current-buffer buff
            (when (bound-and-true-p lsp-mode)
              (unless (bound-and-true-p lsp-ui-doc-mode)
                (lsp-ui-doc-mode 1))))))
      "global mode for showing hover information in child frame"
      :enable t :exit nil
      :toggle (ignore-errors (member 'lsp-ui-doc-mode lsp-mode-hook)))
     ("d f" lsp-ui-doc-frame-mode "Marker mode to add additional key bind for lsp-ui-doc-frame"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-doc-frame-mode) t nil))
     ("d g" lsp-ui-doc-glance "Trigger display hover information popup and hide it on next typing"
      :enable t :exit t)
     ("d s" lsp-ui-doc-show "Trigger display hover information popup"
      :enable t :exit t)
     ("d h" lsp-ui-doc-hide "Hide hover information popup"
      :enable t :exit t))
    "Imenu"
    (("i o" lsp-ui-imenu "Open ui-imenu in side window" :enable t :exit t))
    "Peek"
    (("p d" lsp-ui-peek-find-definitions "Find definitions to the IDENTIFIER at point"
      :enable t :exit t)
     ("p i" lsp-ui-peek-find-implementation "Find implementation locations of the symbol at point"
      :enable t :exit t)
     ("p r" lsp-ui-peek-find-references "Find references to the IDENTIFIER at point"
      :enable t :exit t)
     ("p s" lsp-ui-peek-find-workspace-symbol "Find symbols in the worskpace"
      :enable t :exit t))
    "Side Line"
    (("s t" lsp-ui-sideline-mode "Minor mode for showing information for current line"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-sideline-mode) t nil))
     ("s a" lsp-ui-sideline-apply-code-actions "Choose and apply code action(s) on the current line"
      :enable t :exit t)
     ("s i" lsp-ui-sideline-toggle-symbols-info "Toggle display of symbols information"
      :enable t :exit t))
    "Find"
    (("f n" lsp-ui-find-next-reference "Find next reference of the symbol at point"
      :enable t :exit t)
     ("f p" lsp-ui-find-prev-reference "Find previous reference of the symbol at point"
      :enable t :exit t)
     ("f s" lsp-ui-find-workspace-symbol "List project-wide symbols matching the query string PATTERN"
      :enable t :exit t))))

;; ******* eemacs-indhca
  :eemacs-indhca
  (((:enable t :defer t)
    (lsp-mode))
   ("Basic"
    (("b u"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'lsp-ui-mode))
      "lsp ui command map"
      :enable t :exit t))))

;; ******* init
  :init
  (setq lsp-ui-doc-enable nil           ;disable the doc show defaulty
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay entropy/emacs-ide-doc-delay
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-delay entropy/emacs-ide-doc-delay
        lsp-ui-sideline-diagnostic-max-line-length 50
        lsp-ui-sideline-diagnostic-max-lines 1)

;; ******* config
  :config
;; ******** advices

  ;; EEMACS_MAINTENANCE: these variants need to follow update with `lsp-ui' upstream.
  (defun eemacs/lsp-ui-doc--hide-frame (&optional _win)
    "Like `lsp-ui-doc--hide-frame' but for eemacs only."
    (setq lsp-ui-doc--from-mouse nil)
    (lsp-ui-util-safe-delete-overlay lsp-ui-doc--inline-ov)
    (lsp-ui-util-safe-delete-overlay lsp-ui-doc--highlight-ov)
    (when-let ((frame (lsp-ui-doc--get-frame)))
      (when (frame-visible-p frame)
        (make-frame-invisible frame))))
  (defun eemacs/lsp-ui-doc-hide ()
    "Like `lsp-ui-doc-hide' but for eemacs only."
    (interactive)
    (lsp-ui-doc-unfocus-frame) ;; In case focus is in doc frame
    (eemacs/lsp-ui-doc--hide-frame))

  (defun entropy/emacs-codeserver--lsp-ui-doc-frame-mode-disable-mouse
      (orig-func &rest orig-args)
    "Diable mouse wheel in lsp-ui-doc-frame while it has the bug of:

EEMACS_BUG: h-c02794e4-bdb8-4510-84cb-d668873b02fc
 will make lsp-ui-doc frame freeze emacs while
`mwheel-scroll' down to the eobp of lsp-doc-buffer"
    (let ((rtn (apply orig-func orig-args)))
      (unless (bound-and-true-p disable-mouse-mode)
        (entropy/emacs-require-only-once 'disable-mouse)
        (disable-mouse-mode 1))
      rtn))
  ;; since the advice we do not any mouse support by `lsp-ui'
  (setq lsp-ui-doc-show-with-mouse nil)
  (advice-add 'lsp-ui-doc-frame-mode
              :around
              #'entropy/emacs-codeserver--lsp-ui-doc-frame-mode-disable-mouse)

;; ******** Doc timer
  (defvar-local entropy/emacs-codeserver--lsp-ui-doc--bounds nil)

  (defun entropy/emacs-codeserver--lsp-ui-doc-make-request nil
    "Request the documentation to the LS."
    (let ((buf (current-buffer))
          (hide lsp-ui-doc--hide-on-next-command))
      (when (and (bound-and-true-p lsp-ui-doc-mode)
                 (not (eq this-command 'lsp-ui-doc-hide))
                 (not (eq this-command 'keyboard-quit))
                 (not (bound-and-true-p lsp-ui-peek-mode))
                 (lsp-feature? "textDocument/hover"))
        (-if-let (bounds (or (and (symbol-at-point)
                                  (bounds-of-thing-at-point 'symbol))
                             (and (looking-at "[[:graph:]]")
                                  (cons (point) (1+ (point))))))
            (unless (entropy/emacs-codeserver--bounds-same-p
                     bounds
                     entropy/emacs-codeserver--lsp-ui-doc--bounds)
              (setq entropy/emacs-codeserver--lsp-ui-doc--bounds bounds)
              (eemacs/lsp-ui-doc--hide-frame)
              (lsp-request-async
               "textDocument/hover"
               (lsp--text-document-position-params)
               (lambda (hover)
                 (when (equal buf (current-buffer))
                   (prog1
                       (lsp-ui-doc--callback hover bounds (current-buffer) hide)
                     ;; FIXME: `lsp-ui-doc--hide-frame' is hard coded in
                     ;; `lsp-ui-doc--callback' with `post-command-hook',
                     ;; it's good to hack thus.
                     (when hide
                       (remove-hook 'post-command-hook 'lsp-ui-doc--hide-frame)))))
               :mode 'tick
               :cancel-token :lsp-ui-doc-hover))
          (eemacs/lsp-ui-doc-hide)
          (setq entropy/emacs-codeserver--lsp-ui-doc--bounds nil)))))

  (defun entropy/emacs-codeserver--lsp-ui-doc-hide ()
    (let ((event last-input-event))
      (unless (or (and (car-safe event) (symbolp (car event))
                       (string-match-p "mouse" (symbol-name (car event))))
                  (eq (car-safe event) 'switch-frame))
        (unless (memq   this-command
                        ;; preserve the bounds log when these commands
                        ;; trigger since each of these commands may
                        ;; not change the bounds via
                        ;; `entropy/emacs-codeserver--bounds-same-p'
                        `(outshine-self-insert-command
                          self-insert-command
                          right-char left-char
                          forward-char backward-char
                          forward-word backward-word
                          ,(lookup-key (current-local-map) (kbd "DEL"))
                          keyboard-quit
                          ))
          (setq entropy/emacs-codeserver--lsp-ui-doc--bounds nil))
        (eemacs/lsp-ui-doc-hide))))

  (defun entropy/emacs-codeserver--lsp-ui-doc-mode-around-advice
      (orig-func &rest orig-args)
    "Remove lagging post command hooker
`lsp-ui-doc--make-request' and using eemacs sepcified popup
mechanism which purely use idle-timer to do thus.

EEMACS_MAINTENANCE: this may need update follow `lsp-ui' package
updating."
    (let ((rtn (apply orig-func orig-args)))
      (if (not (bound-and-true-p lsp-ui-doc-mode))
          (progn
            (remove-hook 'post-command-hook
                         'entropy/emacs-codeserver--lsp-ui-doc-hide t)
            (unless (let (rtn)
                      (mapc (lambda (buffer)
                              (with-current-buffer buffer
                                (when (bound-and-true-p lsp-ui-doc-mode)
                                  (setq rtn t))))
                            (buffer-list))
                      rtn)
              (when (timerp entropy/emacs-codeserver-lsp-ui-doc-timer)
                (cancel-timer entropy/emacs-codeserver-lsp-ui-doc-timer)
                (setq entropy/emacs-codeserver-lsp-ui-doc-timer nil))))
        (remove-hook 'post-command-hook 'lsp-ui-doc--make-request t)
        (add-hook 'post-command-hook 'entropy/emacs-codeserver--lsp-ui-doc-hide nil t)
        (and (timerp entropy/emacs-codeserver-lsp-ui-doc-timer)
             (cancel-timer entropy/emacs-codeserver-lsp-ui-doc-timer))
        (setq entropy/emacs-codeserver-lsp-ui-doc-timer
              (run-with-idle-timer
               lsp-ui-doc-delay t
               #'entropy/emacs-codeserver--lsp-ui-doc-make-request)))
      rtn))

  (advice-add 'lsp-ui-doc-mode
              :around
              #'entropy/emacs-codeserver--lsp-ui-doc-mode-around-advice)

  (entropy/emacs-coceserver--codeserver-doc-show-timer-watcher-regist
   lsp-ui-doc-delay
   entropy/emacs-codeserver-lsp-ui-doc-timer
   entropy/emacs-codeserver--lsp-ui-doc-make-request)

  )

;; ****** lsp-mode other extension
;; ******* lsp python ms
;; Microsoft python-language-server support
(use-package lsp-python-ms
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'mspyls)
  :defines (lsp-python-ms-python-executable-cmd)
  :init

  ;; NOTE: like the lsp-pyright config we must disable the :multi-root
  ;; lsp register but for now its hardcoded in the package.
  ;; EEMACS_MAINTENANCE: check the source updates and follow the suggestion.

  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-python-ms)
  :config
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

;; ******* lsp python pyright
(use-package lsp-pyright
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'pyright)
  :init
  ;; disable multi-root for a single pyright server connection since
  ;; python commonly use virtualenv for each project.
  ;;
  ;; NOTE: this variable must be set before load the package since its
  ;; press on the `lsp-register-client'.
  (setq lsp-pyright-multi-root nil)
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-pyright))

;; ******* lsp java

(use-package lsp-java
  ;; we use eemacs spec simple lsp-java so that do not ensure it since
  ;; we purchasing it under eemacs itself yet.
  :ensure nil
  :defines (lsp-java-boot-enabled)
  :init
  ;; EEMACS_BUG:
  ;; Currently we just use simple jdt feature because the boot server
  ;; feature has lots of bug when init at boot time.
  (setq lsp-java-boot-enabled nil)
  (setq lsp-java-autobuild-enabled nil)
  (unless entropy/emacs-ext-use-eemacs-lsparc
    (setq lsp-java-server-install-dir
          (expand-file-name "jdtls/" entropy/emacs-coworker-archive-host-root)))
  (with-eval-after-load 'lsp-mode
    (entropy/emacs-message-simple-progress-message
     "require lsp-java"
     (require 'lsp-java-boot)))

  :config

  ;; EEMACS_MAINTENANCE: follow upstream updates
  ;; JDTLS usually need to download some utils from maven or google
  ;; server to build project so we need to guarantee the user eemacs
  ;; http proxy setting for thus.
  (defun __ya/lsp-java--ls-command/with-proxy (orig-func &rest orig-args)
    (if (plist-get entropy/emacs-union-http-proxy-plist :enable)
        (let ((proxy-env (entropy/emacs-gen-eemacs-union-http-internet-proxy-envs)))
          (append
           `("env"
             ,@proxy-env)
           (apply orig-func orig-args)))
      (apply orig-func orig-args)))
  (advice-add 'lsp-java--ls-command
              :around
              #'__ya/lsp-java--ls-command/with-proxy))

;; **** Eglot


;; TODO:
;;
;; - [ ] Flex completion-at-point filter of command
;; `eglot-completion-at-point' like what lsp did through
;; `lsp-completion-filter-on-incomplete'
;;
;; - [x] make html lsp work without initial error with 'css' undefined
;; - [ ] tidy up native doc callback renderring type which can be
;;      adviced from `lsp-ui'.

(use-package eglot
  :commands (eglot eglot-ensure eglot-shutdown
                   eglot-ui-doc-mode)
  :defines (eglot-ui-doc-mode)

;; ***** eemacs indhc
  :eemacs-indhc
  (((:enable t :defer t)
    (eglot))
   ("Basic"
    (("b s" eglot "Start eglot lsp server"
      :enable t :exit t)
     ("b d" eglot-shutdown "Politely ask eglot SERVER to quit"
      :enable t :exit t)
     ("b u" eglot-ui-doc-mode "Turn on/off popuped doc ui"
      :enable t :toggle eglot-ui-doc-mode))))

;; ***** eemacs indhca
  :eemacs-indhca
  (((:enable t :defer t)
    (eemacs-ide-hydra))
   ("Server"
    (("s e"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'eglot))
      "Eglot command map"
      :enable t :exit t))))

;; ***** init & config

  :init
  (setq eglot-send-changes-idle-time entropy/emacs-ide-diagnostic-delay
        eglot-autoshutdown t
        eglot-autoreconnect 4
        ;; disale the eglot event buffer for performance issue:
        ;; https://emacs-china.org/t/eglot/21074
        eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-extend-to-xref nil)

  ;; We stay out of company setting from eglot internal consistent
  ;; specified since eemacs has its own specification.
  (entropy/emacs-lazy-load-simple 'eglot
    (dolist (item '(completion-styles company))
      (add-to-list 'eglot-stay-out-of item)))

  (defvar __elgot-not-support-yet-prompted-alist nil)
  (defun entropy/emacs-codeserver--eglot-non-support-prompt
      (&rest _)
    (unless (alist-get major-mode __elgot-not-support-yet-prompted-alist)
      (entropy/emacs-message-do-warn
       "%s: %s '%s' %s"
       (yellow "WARNING")
       (green "The major-mode")
       (yellow (symbol-name major-mode))
       (green "is not supported by eemacs eglot specification,
but you specified eglot as default lsp-ide type for this major-mode,
so that we just ignored what you specification, but you can [M-x lsp]
to enable the lsp server for this major-mode supported by `lsp-mode'.
(This warning just appeared once)."))
      (add-to-list '__elgot-not-support-yet-prompted-alist
                   (cons major-mode t))))

  ;; prog mode hook injection to start eglot
  (dolist (el entropy/emacs-ide-for-them)
    (let ((hook (intern (format "%s-hook" el))))
    (when (eq (entropy/emacs-get-use-ide-type el) 'eglot)
      ;; TODO: bellow modes are not support with eglog
      ;; yet, add supports for them in plan.
      (if (member el '(java-mode powershell-mode nxml-mode go-mode))
          (add-hook hook #'entropy/emacs-codeserver--eglot-non-support-prompt)
        (add-hook
         hook #'eglot-ensure)))))
  (advice-add 'eglot-ensure
              :around
              #'entropy/emacs-codeserver--codeserver-union-startjudge-filter-advice-form)

  ;; Disable `eldoc-mode' after eglot start
  ;; NOTE: since `eglot--connect' is the only one subroutine coverred
  ;; the all eglot start command, but it is not the exposed API which
  ;; we must follow upstream updates.
  (advice-add 'eglot--connect
              :around
              #'entropy/emacs-eldoc-inhibit-around-advice)

  :config
;; ***** mode spec framework
  (defvar entropy/emacs-codeserver--eglot-modes-spec nil)
  (defun entropy/emacs-codeserver--eglot-completion-with-placeholder
      ()
    "Add :usePlaceholders to the lsp SERVER specification which
let eglot do completion with interface argument injection."
    (mapc (lambda (eglot-pm)
            (let* ((server-item (ignore-errors (cadr eglot-pm)))
                   (server (cond
                            ((stringp server-item)
                             (make-symbol (format ":%s" server-item)))
                            (t
                             nil)))
                   (spec-exist (alist-get server eglot-workspace-configuration)))
              (when server
                (cond (spec-exist
                       (let ((spec-plist (car spec-exist)))
                         (unless (member :usePlaceholders spec-plist)
                           (plist-put spec-plist :usePlaceholders t))))
                      (t
                       (add-to-list
                        'eglot-workspace-configuration
                        `(,server (:usePlaceholders . t))))))))
          eglot-server-programs))

  (defun entropy/emacs-codeserver--eglot-top-prepare (&rest _)
    (make-local-variable 'eglot-workspace-configuration)
    (setq-local eldoc-idle-delay entropy/emacs-ide-doc-delay)
    (entropy/emacs-company-start-with-yas)
    (mapc
     (lambda (it)
       (when (ignore-errors
               (or (eq (car it) major-mode)
                   (member major-mode (car it))))
         (let ((func-or-form (cdr it)))
           (cond
            ((symbolp func-or-form)
             (funcall func-or-form))
            ((listp func-or-form)
             (entropy/emacs-eval-with-lexical func-or-form))))))
     entropy/emacs-codeserver--eglot-modes-spec)
    (entropy/emacs-codeserver--eglot-completion-with-placeholder))

  ;; we grab the eglot init subroutine as advice object since `eglot'
  ;; is an interactive function which use `eglot--guess-contact' as
  ;; its `interactive' args init utility
  (advice-add 'eglot--guess-contact :before
              #'entropy/emacs-codeserver--eglot-top-prepare)

;; ***** server spec

  (defun entropy/emacs-codeserver--eglot-server-chosen-hack
      (mode concact)
    (let* (_)
      (make-local-variable 'eglot-server-programs)
      (setq eglot-server-programs
            (--map-when
             (equal (car it) mode)
             (cons (car it)
                   concact)
             eglot-server-programs))
      (unless (alist-get mode eglot-server-programs)
        (setq eglot-server-programs
              (append `((,mode . ,concact))
                      eglot-server-programs)))))

;; ****** python
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-PYTHON ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'python-mode
     (cl-case entropy/emacs-codeserver-prefer-pyls-type
       (pyls '("pyls"))
       (pyright '("pyright-langserver" "--stdio")))))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '(python-mode
                 . entropy/emacs-codeserver--eglot-server-chosen-for-PYTHON))

;; ****** php
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-PHP ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'php-mode
     '("intelephense" "--stdio")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '(php-mode
                 . entropy/emacs-codeserver--eglot-server-chosen-for-PHP))

;; ****** C and CPP
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-C&CPP ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     '(c++-mode c-mode) '("clangd")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '((c++-mode c-mode)
                 . entropy/emacs-codeserver--eglot-server-chosen-for-C&CPP))

;; ****** JS
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-JS&TS ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     '(js-mode typescript-mode)
     `("typescript-language-server" "--tsserver-path"
       ,(executable-find "tsserver")
       "--stdio"))
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'js2-mode
     `("typescript-language-server" "--tsserver-path"
       ,(executable-find "tsserver")
       "--stdio")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '((js-mode js2-mode typescript-mode)
                 . entropy/emacs-codeserver--eglot-server-chosen-for-JS&TS))

;; ****** Html
;; ******* server defination

  (defclass eglot-html-languageserver (eglot-lsp-server) ()
    :documentation "Vscode html-languageserver class for eglot")
  (cl-defmethod eglot-initialization-options ((_server eglot-html-languageserver))
    "`eglot-html-languageserver' initialization method"
    '(:embeddedLanguages
      (:css "true" :javascript "true")
      :provideFormatte "true"
      :settings (
                 :html.customData []
                 :html.format.enable "true"
                 :html.format.wrapLineLength 120
                 :html.format.unformatted "wbr"
                 :html.format.contentUnformatted "pre,code,textarea"
                 :html.format.indentInnerHtml "false"
                 :html.format.preserveNewLines "true"
                 :html.format.maxPreserveNewLines "null"
                 :html.format.indentHandlebars "false"
                 :html.format.endWithNewline "false"
                 :html.format.extraLiners "head, body, /html"
                 :html.format.wrapAttributes "auto"
                 :html.suggest.html5 "true"
                 :html.validate.scripts "true"
                 :html.validate.styles "true"
                 :css.customData []
                 :css.completion.triggerPropertyValueCompletion "true"
                 :css.completion.completePropertyWithSemicolon "true"
                 :css.validate "true"
                 :css.lint.compatibleVendorPrefixes "ignore"
                 :css.lint.vendorPrefix "warning"
                 :css.lint.duplicateProperties "ignore"
                 :css.lint.emptyRules "warning"
                 :css.lint.importStatement "ignore"
                 :css.lint.boxModel "ignore"
                 :css.lint.universalSelector "ignore"
                 :css.lint.zeroUnits "ignore"
                 :css.lint.fontFaceProperties "warning"
                 :css.lint.hexColorLength "error"
                 :css.lint.argumentsInColorFunction "error"
                 :css.lint.unknownProperties "warning"
                 :css.lint.validProperties []
                 :css.lint.ieHack "ignore"
                 :css.lint.unknownVendorSpecificProperties "ignore"
                 :css.lint.propertyIgnoredDueToDisplay "warning"
                 :css.lint.important "ignore"
                 :css.lint.float "ignore"
                 :css.lint.idSelector "ignore"
                 :css.lint.unknownAtRules "warning"
                 :javascript.referencesCodeLens.enabled "false"
                 :javascript.referencesCodeLens.showOnAllFunctions "false"
                 :javascript.suggest.completeFunctionCalls "false"
                 :javascript.suggest.includeAutomaticOptionalChainCompletions "true"
                 :javascript.suggest.names "true"
                 :javascript.suggest.paths "true"
                 :javascript.suggest.autoImports "true"
                 :javascript.suggest.completeJSDocs "true"
                 :javascript.suggest.enabled "true"
                 :javascript.validate.enable "true"
                 :javascript.format.enable "true"
                 :javascript.format.insertSpaceAfterCommaDelimiter "true"
                 :javascript.format.insertSpaceAfterConstructor "false"
                 :javascript.format.insertSpaceAfterSemicolonInForStatements "true"
                 :javascript.format.insertSpaceBeforeAndAfterBinaryOperators "true"
                 :javascript.format.insertSpaceAfterKeywordsInControlFlowStatements "true"
                 :javascript.format.insertSpaceAfterFunctionKeywordForAnonymousFunctions "false"
                 :javascript.format.insertSpaceBeforeFunctionParenthesis "false"
                 :javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis "false"
                 :javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets "false"
                 :javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces "true"
                 :javascript.format.insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces "false"
                 :javascript.format.insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces "false"
                 :javascript.format.placeOpenBraceOnNewLineForFunctions "false"
                 :javascript.format.placeOpenBraceOnNewLineForControlBlocks "false"
                 :javascript.format.semicolons "ignore"
                 :javascript.implicitProjectConfig.checkJs "true"
                 :javascript.implicitProjectConfig.experimentalDecorators "false"
                 :javascript.suggestionActions.enabled "true"
                 :javascript.preferences.quoteStyle "single"
                 :javascript.preferences.importModuleSpecifier "auto"
                 :javascript.preferences.renameShorthandProperties "true"
                 :javascript.updateImportsOnFileMove.enabled "prompt"
                 )
      ))

;; ******* core

  (defun entropy/emacs-codeserver--eglot-server-chosen-for-HTML ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     '(web-mode html-mode)
     '(eglot-html-languageserver
       .
       ("html-languageserver"
        "--stdio"))))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '((web-mode html-mode)
                 . entropy/emacs-codeserver--eglot-server-chosen-for-HTML))

;; ****** Css

  (defun entropy/emacs-codeserver--eglot-server-chosen-for-CSS ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'css-mode
     `("css-languageserver"
       "--stdio")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '(css-mode
                 . entropy/emacs-codeserver--eglot-server-chosen-for-CSS))

;; ****** json
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-JSON ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'json-mode
     `("vscode-json-languageserver"
       "--stdio")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '(json-mode
                 . entropy/emacs-codeserver--eglot-server-chosen-for-JSON))


;; ****** Cmake

  (defun entropy/emacs-codeserver--eglot-server-chosen-for-CMAKE ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'cmake-mode
     `("cmake-language-server")))
  (add-to-list 'entropy/emacs-codeserver--eglot-modes-spec
               '(cmake-mode
                 . entropy/emacs-codeserver--eglot-server-chosen-for-CMAKE))

;; ***** doc show

  (defvar entropy/emacs-eglot-doc-buffer-name "*eglot doc*"
    "The name of eglot tooltip name."
    )

  (defvar entropy/emacs-eglot-doc-tooltip-timeout 30
    "The timeout of eglot tooltip show time, in seconds."
    )

  (defvar entropy/emacs-eglot-doc-tooltip-border-width 6
    "The border width of eglot tooltip, default is 6 px."
    )

  (defun eglot-ui--show-doc-internal (string)
    (let* (
           ;;(bg-mode (frame-parameter nil 'background-mode))
           (background-color (face-attribute 'tooltip :background)))
      (cond
       ((and (fboundp 'posframe-show) (display-graphic-p))
        (entropy/emacs-require-only-once 'posframe)
        (posframe-show
         entropy/emacs-eglot-doc-buffer-name
         :string string
         :poshandler #'posframe-poshandler-frame-top-right-corner
         :timeout entropy/emacs-eglot-doc-tooltip-timeout
         :background-color background-color
         :foreground-color (face-attribute 'default :foreground)
         :internal-border-width
         entropy/emacs-eglot-doc-tooltip-border-width
         :hidehandler (lambda (&rest _) t))
        (sit-for most-positive-fixnum t)
        (posframe-hide entropy/emacs-eglot-doc-buffer-name))
       ((fboundp 'popup-tip)
        (entropy/emacs-require-only-once 'popup)
        (let ((pop (popup-tip string :nowait t :margin 1)))
          (sit-for most-positive-fixnum t)
          (popup-delete pop)))
       (t
        (let (_)
          (switch-to-buffer-other-window entropy/emacs-eglot-doc-buffer-name)
          (with-current-buffer entropy/emacs-eglot-doc-buffer-name
            (erase-buffer)
            (insert string)
            (goto-char (point-min))
            (setq buffer-read-only t)
            (apply 'entropy/emacs-local-set-key-batch-do
                   `((,(kbd "q") . kill-buffer-and-window)
                     (,(kbd "n") . forward-line)
                     (,(kbd "j") . forward-line)
                     (,(kbd "p") . previous-line)
                     (,(kbd "k") . previous-line)))))
        ))))

  (defvar __this_eglot-show-doc--sig-showing nil)
  (defvar __this_eglot-show-doc--this-buffer nil)
  (defvar __this_eglot-show-doc--this-position-params nil)
  (defvar __this_eglot-show-doc--this-server nil)
  (defun eglot-ui-show-doc ()
    "Show documentation at point, use by `posframe'."
    (interactive)
    (let* (_)
      (setq __this_eglot-show-doc--this-buffer (current-buffer)
            __this_eglot-show-doc--this-server (eglot--current-server-or-lose)
            __this_eglot-show-doc--this-position-params (eglot--TextDocumentPositionParams)
            __this_eglot-show-doc--sig-showing nil)
      (cl-macrolet ((when-buffer-window
                     (&body body) ; notice the exception when testing with `ert'
                     `(when (or (get-buffer-window __this_eglot-show-doc--this-buffer)
                                (ert-running-test))
                        (with-current-buffer __this_eglot-show-doc--this-buffer ,@body))))
        (when (eglot--server-capable :signatureHelpProvider)
          (jsonrpc-async-request
           __this_eglot-show-doc--this-server
           :textDocument/signatureHelp __this_eglot-show-doc--this-position-params
           :success-fn
           (eglot--lambda ((SignatureHelp) signatures activeSignature activeParameter)
             (when-buffer-window
              (when (cl-plusp (length signatures))
                (setq __this_eglot-show-doc--sig-showing t)
                (eglot-ui--show-doc-internal
                 (eglot--sig-info signatures activeSignature activeParameter)))))
           :deferred :textDocument/signatureHelp))
        (when (eglot--server-capable :hoverProvider)
          (jsonrpc-async-request
           __this_eglot-show-doc--this-server
           :textDocument/hover __this_eglot-show-doc--this-position-params
           :success-fn (eglot--lambda ((Hover) contents range)
                         (unless __this_eglot-show-doc--sig-showing
                           (when-buffer-window
                            (when-let (info (and (not (seq-empty-p contents))
                                                 (eglot--hover-info contents
                                                                    range)))
                              (eglot-ui--show-doc-internal info)))))
           :deferred :textDocument/hover)))))


  (defvar eglot-ui-doc-idle-delay entropy/emacs-ide-doc-delay)
  (defvar eglot-ui-doc-idle-timer nil)
  (defvar eglot--ui-doc-enable-buffers nil)

  (defun eglot-ui-doc-display ()
    (when eglot-ui-doc-mode
      (eglot--highlight-piggyback nil)
      (eglot-ui-show-doc)))

  (define-minor-mode eglot-ui-doc-mode
    "Eglot hover doc show mode"
    :init-value nil
    :global nil
    (cond ((null eglot-ui-doc-mode)
           (progn
             (when (and (timerp eglot-ui-doc-idle-timer)
                        (null
                         (setq eglot--ui-doc-enable-buffers
                          (--filter (and (bufferp it) (buffer-live-p it))
                                    (delete*
                                     (current-buffer)
                                     eglot--ui-doc-enable-buffers)))))
               (cancel-timer eglot-ui-doc-idle-timer)
               (cancel-function-timers 'eglot-ui-doc-display)
               (setq eglot-ui-doc-idle-timer nil))
             ;; ---> we do not recovery the host native eldoc-mode since its laggy
             ;; (eldoc-mode 1)
             ))
          (eglot-ui-doc-mode
           (unless (bound-and-true-p eglot--managed-mode)
             (user-error
              "Eglot not enabled yet so that `eglot-ui-doc-mode' can not enable at once."))
           (eldoc-mode 0)
           (push (current-buffer)
                 eglot--ui-doc-enable-buffers)
           (unless (timerp eglot-ui-doc-idle-timer)
             (setq eglot-ui-doc-idle-timer
                   (run-with-idle-timer
                    eglot-ui-doc-idle-delay
                    t
                    #'eglot-ui-doc-display
                    ))))))

  (entropy/emacs-coceserver--codeserver-doc-show-timer-watcher-regist
   eglot-ui-doc-idle-delay
   eglot-ui-doc-idle-timer
   eglot-ui-doc-display)

;; ***** Others spec
  (defun eglot-shutdown--around-advice-0
      (orig-func &rest orig-args)
    "Remove symbol highlight and close idle doc show timer"
    (mapc #'delete-overlay eglot--highlights)
    (when eglot-ui-doc-mode
      (eglot-ui-doc-mode 0))
    (let ((rtn (apply orig-func orig-args)))
      rtn))
  (advice-add 'eglot-shutdown
              :around
              #'eglot-shutdown--around-advice-0)

  (defun entropy/emacs-codeserver--eldoc-shutdown-patch-of-eglot
      (orig-func &rest orig-args)
    "Cancel symbol highlight when `eldoc-mode' is set-off."
    (let ((rtn (apply orig-func orig-args)))
      (ignore-errors
        (when (and (bound-and-true-p eglot--managed-mode)
                   (null (bound-and-true-p eldoc-mode)))
          (mapc #'delete-overlay eglot--highlights)))
      rtn))
  (advice-add 'eldoc-mode
              :around #'entropy/emacs-codeserver--eldoc-shutdown-patch-of-eglot)

  (defun __ya/eglot--highlight-piggyback/disable
      (&rest _)
    "Disbale the overlay render"
    nil)
  (advice-add 'eglot--highlight-piggyback
              :override
              #'__ya/eglot--highlight-piggyback/disable)

  )


;; * provide
(provide 'entropy-emacs-codeserver)
