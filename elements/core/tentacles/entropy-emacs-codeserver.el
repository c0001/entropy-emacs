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

(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-message)
(require 'entropy-emacs-coworker)
(require 'entropy-emacs-hydra-hollow)

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
      :enable t :exit t)))))

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
    :defines irony-mode-hook
    :commands (irony-eldoc)
    :hook (irony-mode . irony-eldoc)))

;; **** tern server
(defun entropy/emacs-codeserver-usepackage-tern ()
  (use-package tern
    :commands (tern-mode)
    :defines js2-mode-hook
    :hook (js2-mode . tern-mode)))

;; **** anaconda server
(defun entropy/emacs-codeserver-usepackage-anaconda ()
  (use-package anaconda-mode
    :diminish anaconda-mode
    :commands anaconda-mode
    :defines python-mode-hook
    :init
    (add-hook 'python-mode-hook #'anaconda-mode)))

;; **** ac-php
(defun entropy/emacs-codeserver-usepackage-ac-php ()
  (use-package ac-php
    :requires php-mode
    :defines php-mode-hook
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
         (file-exists-p (concat entropy/emacs-win-portable-mingw-path "libclang.dll")))
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
  (defun entropy/emacs-codeserver-lsp-mode-remove-session-file ()
    (when (and (boundp 'lsp-session-file)
               (file-exists-p lsp-session-file))
      (delete-file lsp-session-file)))

  (defun entropy/emacs-codeserver--lsp-deferred-promt (&rest _)
    "Prompting for `lsp-deferred' starting for prevent lagging
nervous."
    (entropy/emacs-message-do-message
     "%s <%s> %s"
     (green "Lsp check for buffer")
     (yellow (buffer-name))
     (green "...")))

  (defun entropy/emacs-codeserver--lsp-deferred-exclude (orig-func &rest orig-args)
    (unless (member major-mode '(emacs-lisp-mode
                                 lisp-interaction-mode
                                 lisp-mode))
      (apply orig-func orig-args)))

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

  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b l"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'lsp-mode))
      "Lsp command map"
      :enable t :exit t))))

  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake nil)

  ;; delete transient lsp session file for prevent lagging with large
  ;; amounts of folder parsing while next emacs session setup.
  (add-hook 'kill-emacs-hook
            #'entropy/emacs-codeserver-lsp-mode-remove-session-file)

  (dolist (el entropy/emacs-ide-for-them)
    (when (eq (entropy/emacs-get-use-ide-type el) 'lsp)
      (add-hook
       (intern (format "%s-hook" el))
       #'lsp-deferred)))

  (entropy/emacs-lazy-initial-advice-before
   (lsp)
   "lsp-enable-yas"
   "lsp-enable-yas"
   (require 'yasnippet)
   (yas-global-mode))

  :config
  (advice-add 'lsp-deferred
              :before
              #'entropy/emacs-codeserver--lsp-deferred-promt)
  (advice-add 'lsp-deferred
              :around
              #'entropy/emacs-codeserver--lsp-deferred-exclude))
;; **** lsp-ui
(use-package lsp-ui
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
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
  :eemacs-indhca
  (((:enable t)
    (lsp-mode))
   ("Basic"
    (("b u"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'lsp-ui-mode))
      "lsp ui command map"
      :enable t :exit t))))
  :init
  (setq lsp-ui-doc-position 'top))

;; **** lsp extensions
;; ***** lsp java
(use-package lsp-java
  :hook (java-mode . (lambda () (require 'lsp-java))))

;; ***** lsp python ms
;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

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
