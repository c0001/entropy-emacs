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
;; **** lsp-mode
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

                 lsp-treemacs-sync-mode
                 lsp-treemacs-call-hierarchy
                 lsp-treemacs-errors-list

                 lsp-find-definition
                 lsp-find-references
                 lsp-find-implementation
                 lsp-find-type-definition
                 lsp-find-declaration)
  :preface
;; ***** eemacs-indhc
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
     ("w s" lsp-workspace-shutdown "Shut the workspace WORKSPACE"
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
     ("d t" lsp-describe-thing-at-point "Display the type signature and documentation"
      :enable t :exit t))
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
    "Treemacs integrates "
    (("e m" lsp-treemacs-sync-mode "synchronizing lsp-mode workspace folders and treemacs projects"
      :enable t :exit t)
     ("e c" lsp-treemacs-call-hierarchy "Show the incoming call hierarchy for the symbol at point"
      :enable t :exit t)
     ("e e" lsp-treemacs-errors-list "Display error list in treemacs"
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
;; ***** eemacs-tphs
  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b l"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'lsp-mode))
      "Lsp command map"
      :enable t :exit t))))
;; ***** init
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-signature-auto-activate t
        ;; Set `lsp-signature-doc-lines' to 0 to restrict the echo
        ;; area lines to have more UI exps, so that only syntax line
        ;; are echoed.
        lsp-signature-doc-lines 0)

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

;; ***** config
  :config

;; ****** advices
;; ******* require extra clients
  (advice-add 'lsp
              :before
              #'entropy/emacs-codeserver--lsp-mode-load-extra-clients)
  (defun entropy/emacs-codeserver--lsp-mode-load-extra-clients (&rest _)
    (dolist (feature entropy/emacs-codeserver-lsp-mode-extra-clients)
      (require feature)))

;; ******* remove the session file when close emacs
  ;; delete transient lsp session file for prevent lagging with large
  ;; amounts of folder parsing while next emacs session setup.
  (add-hook 'kill-emacs-hook
            #'entropy/emacs-codeserver--lsp-mode-remove-session-file)
  (defun entropy/emacs-codeserver--lsp-mode-remove-session-file ()
    (when (and (boundp 'lsp-session-file)
               (file-exists-p lsp-session-file))
      (delete-file lsp-session-file)))

;; ******* eemacs lsp start prompting
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

;; ******* exclude `major-mode' for starting `lsp-mode'
  (advice-add 'lsp
              :around
              #'entropy/emacs-codeserver--lsp-exclude)
  (defun entropy/emacs-codeserver--lsp-exclude (orig-func &rest orig-args)
    (unless (member major-mode '(emacs-lisp-mode
                                 lisp-interaction-mode
                                 lisp-mode
                                 org-mode))
      (apply orig-func orig-args)))

;; ******* lsp idle hook specifications
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

;; ****** others
  ;; Remove clients not officially included in `lsp-mode' internal
  ;; subroutine to prevent from coverring eemacs customizations.
  (setq lsp-client-packages
        (delete nil
                (mapcar (lambda (x)
                          (unless (member x '(ccls lsp-python-ms lsp-java))
                            x))
                        lsp-client-packages))))

;; **** lsp-ui
(use-package lsp-ui
;; ***** preface

  :preface
  (defvar entropy/emacs-codeserver-lsp-ui-doc-timer nil)

;; ***** commands and binds
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))

;; ***** eemacs-indhc
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

;; ***** eemacs-indhca
  :eemacs-indhca
  (((:enable t)
    (lsp-mode))
   ("Basic"
    (("b u"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'lsp-ui-mode))
      "lsp ui command map"
      :enable t :exit t))))

;; ***** init
  :init
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.2)

;; ***** config
  :config
;; ****** advices
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

;; ****** Doc timer
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

  (defun entropy/emacs-codeserver--lsp-ui-doc-delay-var-wather
      (symbol newval operation where)
    "Automatically rebind
`entropy/emacs-codeserver-lsp-ui-doc-timer' when
`lsp-ui-doc-delay' has changed.

NOTE: note this function is a variable watcher, do not call it manually!"
    (when (and (eq operation 'set)
               (timerp entropy/emacs-codeserver-lsp-ui-doc-timer))
      (cancel-timer entropy/emacs-codeserver-lsp-ui-doc-timer)
      (setq entropy/emacs-codeserver-lsp-ui-doc-timer
            (run-with-idle-timer
             newval
             t
             #'entropy/emacs-codeserver--lsp-ui-doc-make-request))
      (message "Change `lsp-ui-doc-dealy' to %s" newval)))

  (add-variable-watcher
   'lsp-ui-doc-delay
   #'entropy/emacs-codeserver--lsp-ui-doc-delay-var-wather)

  )

;; **** lsp extensions
;; ***** lsp java
(use-package lsp-java
  :init
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-java)
  :config
  ;; create boot-server directory prevent `lsp-java-boot' throw out its error
  (let ((boot-server-dir
         (expand-file-name
          "boot-server"
          lsp-java-server-install-dir)))
    (unless (file-exists-p boot-server-dir)
      (make-directory boot-server-dir t))))

;; ***** lsp python ms
;; Microsoft python-language-server support
(use-package lsp-python-ms
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'mspyls)
  :init
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-python-ms)
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

;; ***** lsp python pyright
(use-package lsp-pyright
  :if (eq entropy/emacs-codeserver-prefer-pyls-type 'pyright)
  :init
  (add-to-list 'entropy/emacs-codeserver-lsp-mode-extra-clients
               'lsp-pyright))

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

;; *** lsp debug

(use-package dap-mode
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
              ;; ("<f5>" . dap-debug)
              ;; ("M-<f5>" . dap-hydra)
              )
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (_args) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))

         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         (powershell-mode . (lambda () (require 'dap-pwsh)))))

;; `lsp-mode' and `treemacs' integration
(use-package lsp-treemacs
  :after lsp-mode
  :bind (:map lsp-mode-map
              ;; ("C-<f8>" . lsp-treemacs-errors-list)
              ;; ("M-<f8>" . lsp-treemacs-symbols)
              ;; ("s-<f8>" . lsp-treemacs-java-deps-list)
              )
  :config
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
      (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))


;; * provide
(provide 'entropy-emacs-codeserver)
