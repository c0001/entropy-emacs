;;; entropy-emacs-codeserver.el --- entropy-emacs code server configuration
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

;; ** hydra hollow

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
     :enable t))))

;; ** xref jumping
(use-package xref
  :ensure nil
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
  :eemacs-indhc
  (((:enable t)
    (xref-mode))
   ("Identifier find"
    (("M-." xref-find-definitions "Find the definition of the identifier at point"
      :enable t :exit t :global-bind t)
     ("M-," xref-pop-marker-stack "Pop back to where M-. was last invoked"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b x"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'xref-mode))
      "Xref referrence jumping"
      :enable t :exit t))))
  :config
  ;; Disable xref in minibuffer prevents nesting looping unpredictable.
  (dolist (key (list "M-." "C-." "M-," "C-M-."))
    (dolist (map-ob '((ivy-minibuffer-map . ivy)
                      (minibuffer-inactive-mode-map . minibuffer)
                      ;; Although its a native map but unified for
                      ;; minibuffer feature autoloading as a fake did.
                      (minibuffer-local-map . minibuffer)))
      (eval
       `(entropy/emacs-lazy-load-simple ,(cdr map-ob)
          (define-key ,(car map-ob)
            (kbd ,key)
            (lambda ()
              (interactive)
              (message
               "You can not using xref functions in minibuffer"
               ))))))))


;; ** eldoc
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands (eldoc eldoc-mode global-eldoc-mode)
  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("M-h" eldoc "Document thing at point."
      :enable t
      :exit t
      :global-bind t))))
  :init
  (entropy/emacs-lazy-with-load-trail
   eldoc-mode
   (global-eldoc-mode 1)))

;; ** Diagnostics

;; *** eemacs diagnostics framework

(defun entropy/emacs-codeserver-show-diagnostics ()
  "Show diagnostics for `current-buffer' when availalble eemacs
diagnostic feature is actived."
  (interactive)
  (cond
   ((bound-and-true-p flymake-mode)
    (flymake-show-diagnostics-buffer))
   (t
    (user-error "can not show diagnostics for current buffer! \
Because of no suitable backend actived yet."))))

(entropy/emacs-hydra-hollow-common-individual-hydra-define+
 'eemacs-ide-hydra nil
 '("Diagnostics"
   (("M-e" entropy/emacs-codeserver-show-diagnostics
     "Show diagnostics for current-buffer"
     :enable t
     :exit t
     :eemacs-top-bind t))))

;; **** flymake
(use-package flymake
  :ensure nil
  :commands (flymake-show-diagnostics-buffer))

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
      (funcall oldfuc)
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
    (add-hook 'php-mode-hook '(lambda () (ac-php-core-eldoc-setup)))
    (add-hook 'php-mode-hook
              '(lambda ()
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
                         entropy/emacs-win-portable-mingw-path)))
    (entropy/emacs-codeserver-usepackage-irony))
   (sys/is-posix-compatible
    (entropy/emacs-codeserver-usepackage-irony))))

(when (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-tern))
(when (eq (entropy/emacs-get-use-ide-type 'python-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-anaconda))
(when (eq (entropy/emacs-get-use-ide-type 'php-mode) 'traditional)
  (entropy/emacs-codeserver-usepackage-ac-php))

(when (and entropy/emacs-install-coworker-immediately
           (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional))
  (entropy/emacs-lazy-load-simple js2-mode
    (advice-add 'js2-mode
                :before
                #'entropy/emacs-coworker-check-tern-server)))

;; ** microsoft language server
;; *** lsp-client

;; **** uniform

(defvar entropy/emacs-codeserver--LSP-doc-ui-timer-register nil)
(defun entropy/emacs-codeserver--LSP-doc-ui-timer-watcher
    (symbol newval operation where)
  (when (eq operation 'set)
    (let ((timer-obj (alist-get symbol entropy/emacs-codeserver--LSP-doc-ui-timer-register)))
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
               (dolist (item entropy/emacs-codeserver--LSP-doc-ui-timer-register)
                 (let ((var (car item)))
                   (set var newval)))
               ))))))

(defmacro entropy/emacs-coceserver--LSP-doc-ui-timer-watcher-regist
    (idle-var idle-timer-var idle-timer-func)
  `(progn
     (add-to-list 'entropy/emacs-codeserver--LSP-doc-ui-timer-register
                  '(,idle-var . (,idle-timer-var . ,idle-timer-func)))
     (add-variable-watcher
      ',idle-var #'entropy/emacs-codeserver--LSP-doc-ui-timer-watcher)))

(add-variable-watcher
 'entropy/emacs-ide-doc-delay
 #'entropy/emacs-codeserver--LSP-doc-ui-timer-watcher)

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

  (defun entropy/emacs-codeserver-lsp-mode-shutdown ()
    "Fully disable all buffer's `lsp-mode' feature and shutdown
those lsp client connections for the current lsp-mode workspace
when available."
    (interactive)
    (let (cur-wps)
      (when
          (ignore-errors (setq cur-wps (lsp-workspaces)))
        (dolist (wp cur-wps)
          (mapc
           (lambda (buffer)
             (with-current-buffer buffer
               (when (bound-and-true-p lsp-ui-mode)
                 (lsp-ui-mode 0))
               (when (bound-and-true-p lsp-mode)
                 (lsp-mode 0))))
           (lsp--workspace-buffers wp)))
        (lsp-shutdown-workspace))))


;; ******* eemacs-indhc
  :eemacs-indhc
  (((:enable t)
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
     ("w s" lsp-workspace-shutdown "Shut the workspace WORKSPACE with selection"
      :enable t :exit t)
     ("w k" entropy/emacs-codeserver-lsp-mode-shutdown "Shut the *CURRENT* lsp WORKSPACE"
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
  (((:enable t)
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
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-signature-auto-activate t
        ;; Set `lsp-signature-doc-lines' to 0 to restrict the echo
        ;; area lines to have more UI exps, so that only syntax line
        ;; are echoed.
        lsp-signature-doc-lines 0
        ;; forbidden auto enable `dap-mode'
        lsp-enable-dap-auto-configure nil
        lsp-idle-delay entropy/emacs-ide-diagnostic-delay)

  ;; Inhibit auto header insertion via lsp-cland client refer to
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2503
  (setq lsp-clients-clangd-args '("--header-insertion=never"))

  (dolist (el entropy/emacs-ide-for-them)
    (when (eq (entropy/emacs-get-use-ide-type el) 'lsp)
      (add-hook
       (intern (format "%s-hook" el))
       #'lsp-deferred)))

  (entropy/emacs-lazy-initial-advice-before
   (lsp)
   "lsp-enable-yas"
   "lsp-enable-yas"
   prompt-popup
   (require 'yasnippet)
   (yas-global-mode))

;; ******* config
  :config

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

;; ********* eemacs lsp start prompting
  (advice-add 'lsp-deferred
              :before
              #'entropy/emacs-codeserver--lsp-deferred-promt)
  (defun entropy/emacs-codeserver--lsp-deferred-promt (&rest _)
    "Prompting for `lsp-deferred' starting for prevent lagging
nervous."
    (redisplay t)
    (entropy/emacs-message-do-message
     "%s <%s> %s"
     (green "Lsp check for buffer")
     (yellow (buffer-name))
     (green "..."))
    (redisplay t))

  (advice-add 'lsp
              :before
              #'entropy/emacs-codeserver--lsp-start-promt)
  (defun entropy/emacs-codeserver--lsp-start-promt (&rest _)
    "Prompting for `lsp' starting for prevent lagging nervous."
    (redisplay t)
    (entropy/emacs-message-do-message
     "%s %s"
     (green "Lsp starting")
     (green "..."))
    (redisplay t))

;; ********* exclude `major-mode' for starting `lsp-mode'
  (advice-add 'lsp
              :around
              #'entropy/emacs-codeserver--lsp-exclude)
  (defun entropy/emacs-codeserver--lsp-exclude (orig-func &rest orig-args)
    (unless (member major-mode '(emacs-lisp-mode
                                 lisp-interaction-mode
                                 lisp-mode
                                 org-mode))
      (apply orig-func orig-args)))


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

;; ******** others
  ;; Remove clients not officially included in `lsp-mode' internal
  ;; subroutine to prevent from coverring eemacs customizations.
  (setq lsp-client-packages
        (delete nil
                (mapcar (lambda (x)
                          (unless (member x '(ccls lsp-python-ms lsp-java))
                            x))
                        lsp-client-packages))))

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
  (((:enable t)
    (lsp-ui-mode nil nil (1 2 2)))
   ("Doc (Basic)"
    (("d t" lsp-ui-mode "Toggle language server UI mode on or off"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-mode) t nil))
     ("d d" lsp-ui-doc-mode "Minor mode for showing hover information in child frame"
      :enable t :exit t :toggle (if (bound-and-true-p lsp-ui-doc-mode) t nil))
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
  (((:enable t)
    (lsp-mode))
   ("Basic"
    (("b u"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'lsp-ui-mode))
      "lsp ui command map"
      :enable t :exit t))))

;; ******* init
  :init
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-delay entropy/emacs-ide-doc-delay)

;; ******* config
  :config
;; ******** advices
  ;; EEMACS_MAINTENANCE: this patch need to follow update with `lsp-ui' upstream.
  (defun lsp-ui-doc--hide-frame ()
    "Hide the frame.

NOTE: this function has been patched by eemacs to optimize performance."
    (setq lsp-ui-doc--bounds nil)
    (when (overlayp lsp-ui-doc--inline-ov)
      (delete-overlay lsp-ui-doc--inline-ov))
    (let ((doc-frame (lsp-ui-doc--get-frame)))
      (when (and (framep doc-frame)
                 ;; Judge whether the doc frame is visible instead of
                 ;; use `make-frame-invisible' directly for
                 ;; performance issue.
                 (frame-visible-p doc-frame))
        (unless lsp-ui-doc-use-webkit
          (lsp-ui-doc--with-buffer
           (erase-buffer)))
        (make-frame-invisible doc-frame))))

  (defun entropy/emacs-codeserver--lsp-ui-doc-frame-mode-disable-mounse
      (orig-func &rest orig-args)
    "Diable mouse wheel in lsp-ui-doc-frame while it has the bug of:

EEMACS_BUG: h-c02794e4-bdb8-4510-84cb-d668873b02fc
 will make lsp-ui-doc frame freeze emacs while
`mwheel-scroll' down to the eobp of lsp-doc-buffer"
    (let ((rtn (apply orig-func orig-args)))
      (unless (bound-and-true-p disable-mouse-mode)
        (unless (fboundp 'disable-mouse-mode)
          (require 'disable-mouse))
        (disable-mouse-mode 1))
      rtn))

  (advice-add 'lsp-ui-doc-frame-mode
              :around
              #'entropy/emacs-codeserver--lsp-ui-doc-frame-mode-disable-mounse)

;; ******** Doc timer
  (defvar entropy/emacs-codeserver--lsp-ui-doc--bounds nil)
  (defun entropy/emacs-codeserver--lsp-ui-doc-make-request nil
    "Request the documentation to the LS."
    (let ((buf (current-buffer)))
      (when (and (bound-and-true-p lsp-ui-doc-mode)
                 (not (eq this-command 'lsp-ui-doc-hide))
                 (not (eq this-command 'keyboard-quit))
                 (not (bound-and-true-p lsp-ui-peek-mode))
                 (lsp--capability "hoverProvider"))
        (-if-let (bounds (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                             (and (looking-at "[[:graph:]]") (cons (point) (1+ (point))))))
            (unless (equal entropy/emacs-codeserver--lsp-ui-doc--bounds
                           bounds)
              (setq entropy/emacs-codeserver--lsp-ui-doc--bounds bounds)
              (lsp-ui-doc--hide-frame)
              (lsp-request-async
                   "textDocument/hover"
                   (lsp--text-document-position-params)
                   `(lambda (hover)
                      (when (equal ,buf (current-buffer))
                        (lsp-ui-doc--callback hover ',bounds (current-buffer))))
                   :mode 'tick
                   :cancel-token :lsp-ui-doc-hover))
          (lsp-ui-doc-hide)
          (setq entropy/emacs-codeserver--lsp-ui-doc--bounds nil)))))

  (defun entropy/emacs-codeserver--lsp-ui-doc-hide ()
    (let ((event last-input-event))
      (unless (or (ignore-errors (string-match-p "mouse" (symbol-name (car event))))
                  (eq (car-safe event) 'switch-frame))
        (setq entropy/emacs-codeserver--lsp-ui-doc--bounds nil)
        (lsp-ui-doc-hide))))

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
            (remove-hook 'post-command-hook 'entropy/emacs-codeserver--lsp-ui-doc-hide t)
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
              (run-with-idle-timer lsp-ui-doc-delay t
                                   #'entropy/emacs-codeserver--lsp-ui-doc-make-request)))
      rtn))

  (advice-add 'lsp-ui-doc-mode
              :around
              #'entropy/emacs-codeserver--lsp-ui-doc-mode-around-advice)

  (entropy/emacs-coceserver--LSP-doc-ui-timer-watcher-regist
   lsp-ui-doc-delay
   entropy/emacs-codeserver-lsp-ui-doc-timer
   entropy/emacs-codeserver--lsp-ui-doc-make-request)

  )

;; ****** lsp-mode other extension
;; ******* lsp python ms
;; Microsoft python-language-server support
(use-package lsp-python-ms
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'mspyls)
  :init
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-python-ms)
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

;; ******* lsp python pyright
(use-package lsp-pyright
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'pyright)
  :init
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-pyright))


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

;; ***** eemacs indhc
  :eemacs-indhc
  (((:enable t)
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
  (((:enable t)
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
  (setq eglot-send-changes-idle-time
        entropy/emacs-ide-diagnostic-delay)

  ;; We stay out of company setting from eglot internal consistent
  ;; specified since eemacs has its own specification.
  (entropy/emacs-lazy-load-simple eglot
    (dolist (item '(completion-styles company))
      (add-to-list 'eglot-stay-out-of item)))

  (dolist (el entropy/emacs-ide-for-them)
    (when (eq (entropy/emacs-get-use-ide-type el) 'eglot)
      (add-hook
       (intern (format "%s-hook" el))
       #'eglot-ensure)))

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
             (eval func-or-form))))))
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
    (let* ((rtn (copy-tree eglot-server-programs)))
      (make-local-variable 'eglot-server-programs)
      (setq eglot-server-programs
            (--map-when
             (equal (car it) mode)
             (cons (car it)
                   concact)
             rtn))
      (unless (alist-get mode eglot-server-programs)
        (setq eglot-server-programs
              (append `((,mode . ,concact))
                      eglot-server-programs)))))

;; ****** python
  (defun entropy/emacs-codeserver--eglot-server-chosen-for-PYTHON ()
    (entropy/emacs-codeserver--eglot-server-chosen-hack
     'python-mode
     (cl-case entropy/emacs-codeserver-prefer-pyls-type
       (pyls ("pyls"))
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
  (cl-defmethod eglot-initialization-options ((server eglot-html-languageserver))
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

  (defcustom entropy/emacs-eglot-doc-buffer-name "*eglot doc*"
    "The name of eglot tooltip name."
    :type 'string)

  (defcustom entropy/emacs-eglot-doc-tooltip-timeout 30
    "The timeout of eglot tooltip show time, in seconds."
    :type 'integer)

  (defcustom entropy/emacs-eglot-doc-tooltip-border-width 6
    "The border width of eglot tooltip, default is 6 px."
    :type 'integer)

  (defun eglot-ui--show-doc-internal (string)
    (let* (
           ;;(bg-mode (frame-parameter nil 'background-mode))
           (background-color (face-attribute 'tooltip :background)))
      (cond
       ((and (fboundp 'posframe-show) (display-graphic-p))
        (require 'posframe)
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
        (require 'popup)
        (let ((pop (popup-tip string :nowait t :margin 1)))
          (sit-for most-positive-fixnum t)
          (popup-delete pop)))
       (t
        (let ((win-cfg (current-window-configuration)))
          (switch-to-buffer-other-window entropy/emacs-eglot-doc-buffer-name)
          (with-current-buffer entropy/emacs-eglot-doc-buffer-name
            (erase-buffer)
            (insert string)
            (beginning-of-buffer)
            (read-only-mode t)
            (local-set-key (kbd "q") 'kill-buffer-and-window)
            (local-set-key (kbd "n") 'forward-line)
            (local-set-key (kbd "j") 'forward-line)
            (local-set-key (kbd "p") 'previous-line)
            (local-set-key (kbd "k") 'previous-line)))))))

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
             (eldoc-mode 1)))
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

  (entropy/emacs-coceserver--LSP-doc-ui-timer-watcher-regist
   eglot-ui-doc-idle-delay
   eglot-ui-doc-idle-timer
   eglot-ui-doc-display)

;; ***** Others spec
  (with-eval-after-load 'markdown-mode
    (defun markdown-fontify-hrs (last)
      "Add text properties to horizontal rules from point to LAST.

EEMACS_TEMPORALLY_HACK:
NOTE: this function has been redefined by eemacs to temporally fix overflow hr line display in eldoc."
      (when (markdown-match-hr last)
        (let ((hr-char (markdown--first-displayable markdown-hr-display-char)))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           `(face markdown-hr-face
                  font-lock-multiline t
                  ,@(when (and markdown-hide-markup hr-char)
                      `(display ,(make-string
                                  (/ (window-body-width) 5)
                                  hr-char)))))
          t))))

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

  )


;; *** lsp instances
;; **** lsp html&css
(when (and (eq (entropy/emacs-get-use-ide-type 'web-mode) 'lsp)
           entropy/emacs-install-coworker-immediately)
  (entropy/emacs-lazy-load-simple web-mode
    (advice-add 'web-mode
                :before
                #'entropy/emacs-coworker-check-web-lsp)))

(when (and (eq (entropy/emacs-get-use-ide-type 'css-mode) 'lsp)
           entropy/emacs-install-coworker-immediately)
  (entropy/emacs-lazy-load-simple css-mode
    (advice-add 'css-mode
                :before
                #'entropy/emacs-coworker-check-web-lsp)))

;; **** lsp javascript
(when (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple js2-mode
      (advice-add 'js2-mode :before #'entropy/emacs-coworker-check-js-lsp))))

;; **** lsp json
(when (eq (entropy/emacs-get-use-ide-type 'json-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple json-mode
      (advie-add 'json-mode :before #'entropy/emacs-coworker-check-json-lsp))))

;; **** lsp bash
(when (eq (entropy/emacs-get-use-ide-type 'sh-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple sh-mode
      (advie-add 'sh-mode :before #'entropy/emacs-coworker-check-bash-lsp))))

;; **** lsp php
(when (eq (entropy/emacs-get-use-ide-type 'php-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple php-mode
      (advice-add 'php-mode :before #'entropy/emacs-coworker-check-php-lsp))))

;; **** lsp clangd
(when (eq (entropy/emacs-get-use-ide-type 'c-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple cc-mode
      (advice-add 'c-mode
                  :before
                  #'entropy/emacs-coworker-check-clangd-lsp)
      (advice-add 'c++-mode
                  :before
                  #'entropy/emacs-coworker-check-clangd-lsp))))

;; **** lsp cmake
(when (and (eq (entropy/emacs-get-use-ide-type 'cmake-mode) 'lsp)
           entropy/emacs-install-coworker-immediately)
  (entropy/emacs-lazy-load-simple cmake-mode
    (advice-add 'cmake-mode
                :before
                #'entropy/emacs-coworker-check-cmake-lsp)))

;; **** lsp python
(when (eq (entropy/emacs-get-use-ide-type 'python-mode) 'lsp)
  (when entropy/emacs-install-coworker-immediately
    (entropy/emacs-lazy-load-simple python
      (advice-add 'python-mode
                  :before
                  #'entropy/emacs-coworker-check-python-lsp))))

;; **** lsp powershell
(when (and (eq (entropy/emacs-get-use-ide-type 'powershell-mode) 'lsp)
           entropy/emacs-install-coworker-immediately)
  (entropy/emacs-lazy-load-simple powershell-mode
    (advice-add 'powershell-mode
                :before
                #'entropy/emacs-coworker-check-pwsh-lsp)))


;; * provide
(provide 'entropy-emacs-codeserver)
