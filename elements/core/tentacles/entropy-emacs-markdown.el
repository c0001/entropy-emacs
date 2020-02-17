;;; entropy-emacs-markdown.el --- entropy-emacs markdown-mode configuration
;;
;; * Copyright (C) 2010607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-markdown.el
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
;; Markdown preview, major-mode enhancment and other minor configs.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** main
(use-package markdown-mode
  :commands (markdown-mode)
  :hook (markdown-mode . markdown-toggle-url-hiding)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config

  ;; Change face for markdown code,pre,inline-code face for using `entropy/emacs-default-latin-font'
  (set-face-attribute 'fixed-pitch nil :family entropy/emacs-default-latin-font)

  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  ;; seting export html styl-sheet
  (setq markdown-command-needs-filename t)
  (setq markdown-content-type entropy/emacs-markdown-exp-header-context-type)
  (setq markdown-css-paths
        entropy/emacs-markdown-exp-css-paths)
  (setq markdown-xhtml-header-content
        entropy/emacs-markdown-exp-header-content))

;; ** markdown preview
;; *** simple preview
;; Render and preview via `grip'
;; you can install grip by 'pip install grip'
(defun entropy/emacs-markdown-preview-grip ()
  "Render and preview with `grip'."
  (interactive)
  (if (executable-find "grip")
      (let ((port "6419"))
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) port)
        (browse-url (format "http://localhost:%s/%s.%s"
                            port
                            (file-name-base)
                            (file-name-extension
                             (buffer-file-name)))))
    (user-error "Please install grip by 'pip install grip'.")))

(entropy/emacs-lazy-load-simple markdown-mode
  (define-key markdown-mode-command-map
    (kbd "p") #'entropy/emacs-markdown-preview-grip))

;; *** synchronization previewing
(use-package markdown-preview-mode
  :after markdown-mode
  :commands (markdown-preview-mode)
  :bind (:map markdown-mode-command-map
              ("P" . markdown-preview-mode))
  :config
  (setq markdown-preview-stylesheets
        entropy/emacs-markdown-preview-stylesheets)

  (setq markdown-preview-javascript
        entropy/emacs-markdown-preview-javascript)

  (defun entropy/emacs-markdown--mdp-before-advice (&rest args)
    "Before advice for `markdown-preview-mode' when it trigger
to disable `markdown-preview-mode' for clean up all web-sockets
to prevent ports keeping as causing to next previewing error.

This issue refer to
`https://github.com/ancane/markdown-preview-mode/issues/31'.

For more is force to set locale language coding system to utf-8,
this refer the websocket non-utf-8 cjk chars error."
    (cond
     (markdown-preview-mode
      (markdown-preview-cleanup)
      (message "Clean up all markdown preview websockets done!"))
     (t (entropy/emacs-lang-set-utf-8))))
  (advice-add 'markdown-preview-mode
              :before
              #'entropy/emacs-markdown--mdp-before-advice))

(provide 'entropy-emacs-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
