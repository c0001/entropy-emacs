;;; entropy-emacs-markdown.el --- entropy-emacs markdown-mode configuration  -*- lexical-binding: t; -*-
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

;; ** main
;; *** core
(use-package markdown-mode
  :commands (markdown-mode)
  :preface

  (defun entropy/emacs-markdown--with-eemacs-preview-browser
      (orig-func &rest orig-args)
    "Around advice for using `browse-url-function' retrieved by
`entropy/emacs-browse-url-function-get-for-web-preview' to
`browse-url'."
    (let ((browse-url-browser-function
           (entropy/emacs-browse-url-function-get-for-web-preview)))
      (apply orig-func orig-args)))

  (defun entropy/emacs-markdown-preview-before-advice (&rest _)
    (if (executable-find "multimarkdown")
        (setq markdown-command "multimarkdown")
      (setq markdown-command "markdown"))
    (unless (executable-find markdown-command)
      (error "Mardown executable can not be found, you must
either install 'markdown' or 'multimarkdown' from your system
package management!"))
    (message "Transferring current buffer using '%s' ..." markdown-command))

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :init

  (defun entropy/emacs-markdown--mdmode-core-specifed (&rest _)
    "The eemacs top `markdown-mode' specifications."
    ;; disable truncate line feature since markdown do not recognize
    ;; filled paragraph as a paragraph.
    (setq truncate-lines nil)
    ;; hide the url source link as default
    (setq markdown-hide-urls t)
    )
  (add-hook 'markdown-mode-hook
            #'entropy/emacs-markdown--mdmode-core-specifed)

  :config

  ;; Change face for markdown code,pre,inline-code face for using
  ;; `entropy/emacs-fontsets-used-latin-font'
  (when entropy/emacs-font-setting-enable
    (defface entropy/emacs-markdown-face--fixed-pitch
      '((t :inherit 'fixed-pitch))
      "")
    (set-face-attribute
     'entropy/emacs-markdown-face--fixed-pitch
     nil
     :family entropy/emacs-fontsets-used-latin-font)
    (defvar entropy/emacs-markdown--patch-fixed-pitch-face-font-remap-cache
      nil)
    (defun entropy/emacs-markdown--patch-fixed-pitch-face-font
        (&rest _)
      (if (bound-and-true-p markdown-mode)
          (when entropy/emacs-markdown--patch-fixed-pitch-face-font-remap-cache
            (face-remap-remove-relative
             entropy/emacs-markdown--patch-fixed-pitch-face-font-remap-cache))
        (setq entropy/emacs-markdown--patch-fixed-pitch-face-font-remap-cache
              (face-remap-add-relative
               'fixed-pitch 'entropy/emacs-markdown-face--fixed-pitch))))
    (add-to-list 'markdown-mode-hook
                 'entropy/emacs-markdown--patch-fixed-pitch-face-font))

  ;; prompt for markdown-command whether installed before previewer
  ;; transferring
  (advice-add 'markdown :before #'entropy/emacs-markdown-preview-before-advice)

  ;; preview using eemacs spec browser
  (advice-add 'markdown-preview :around #'entropy/emacs-markdown--with-eemacs-preview-browser)

  ;; seting export html styl-sheet
  (setq markdown-command-needs-filename t)
  (setq markdown-content-type entropy/emacs-markdown-exp-header-context-type)
  (setq markdown-css-paths
        entropy/emacs-markdown-exp-css-paths)
  (setq markdown-xhtml-header-content
        entropy/emacs-markdown-exp-header-content)

  (defun __ya/markdown-fontify-hrs (last)
    "Override advice for `makrdown-fontify-hrs' to temporally fix
overflow hr line e.g. display in eldoc."
    (when (markdown-match-hr last)
      (let ((hr-char (markdown--first-displayable markdown-hr-display-char)))
        (add-text-properties
         (match-beginning 0) (match-end 0)
         `(
           ;; face spec
           face
           markdown-hr-face
           ;; fontlock spec
           font-lock-multiline t
           ;; display spec
           ,@(when (and markdown-hide-markup hr-char)
               `(display ,(make-string
                           ;; HACK: reduce har-render width
                           (/ (window-body-width) 5)
                           hr-char)))))
        t)))
  (advice-add 'markdown-fontify-hrs
              :override
              '__ya/markdown-fontify-hrs)

  )


;; *** hydra hollow instance

;; **** markdown-mode-style hydra

;; ***** hydra heads define
(defvar entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-style-map
  '(
;; ****** Heads Insert
    "Head Insert"
    (("h" markdown-insert-header-dwim
      "Insert or replace header markup"
      :enable t :exit t)
     ("1" markdown-insert-header-atx-1
      "Insert a 1 level atx-style (hash mark) header"
      :enable t :exit t)
     ("2" markdown-insert-header-atx-2
      "Insert a 2 level atx-style (hash mark) header"
      :enable t :exit t)
     ("3" markdown-insert-header-atx-3
      "Insert a 3 level atx-style (hash mark) header"
      :enable t :exit t)
     ("4" markdown-insert-header-atx-4
      "Insert a 4 level atx-style (hash mark) header"
      :enable t :exit t)
     ("5" markdown-insert-header-atx-5
      "Insert a 5 level atx-style (hash mark) header"
      :enable t :exit t)
     ("6" markdown-insert-header-atx-6
      "Insert a 6 level atx-style (hash mark) header"
      :enable t :exit t)
     ("H" markdown-insert-header-setext-dwim
      "Insert or replace header markup, with preference for setext"
      :enable t :exit t)
     ("!" markdown-insert-header-setext-1
      "Insert a setext-style (underlined) 1 level header"
      :enable t :exit t)
     ("@" markdown-insert-header-setext-2
      "Insert a setext-style (underlined) 2 level header"
      :enable t :exit t))

;; ****** Emphasize Insert
    "Emphasize Insert"
    (("b" markdown-insert-bold
      "Insert markup to make a region or word bold"
      :enable t :exit t)
     ("i" markdown-insert-italic
      "Insert markup to make a region or word italic"
      :enable t :exit t)
     ("k" markdown-insert-kbd
      "Insert markup to wrap region or word in <kbd> tags"
      :enable t :exit t)
     ("s" markdown-insert-strike-through
      "Insert markup to make a region or word strikethrough"
      :enable t :exit t))

;; ****** Others

    "Others"
    (("C" markdown-insert-gfm-code-block
      "Insert GFM code block for language LANG"
      :enable t :exit t)
     ("[" markdown-insert-gfm-checkbox
      "Add GFM checkbox at point"
      :enable t :exit t)
     ("f" markdown-insert-footnote
      "Insert footnote with a new number and move point to footnote definition"
      :enable t :exit t)
     ("l" markdown-insert-link
      "Insert new or update an existing link, with interactive prompts"
      :enable t :exit t)
     ("w" markdown-insert-wiki-link
      "Insert a wiki link of the form [[WikiLink]]"
      :enable t :exit t)
     ("p" markdown-insert-pre
      "Start a preformatted section (or apply to the region)"
      :enable t :exit t)
     ("P" markdown-pre-region
      "Format the region as preformatted text"
      :enable t :exit t)
     ("q" markdown-insert-blockquote
      "Start a blockquote section (or blockquote the region)"
      :enable t :exit t)
     ("t" markdown-insert-table
      "Insert an empty pipe table"
      :enable t :exit t)
     ("-" markdown-insert-hr
      "Insert or replace a horizontal rule"
      :enable t :exit t))
    ))

;; ***** hydra-define

(entropy/emacs-lazy-initial-advice-before
 '(markdown-mode)
 "mdstyle-hydra-hollow-init"
 "mdstyle-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'markdown-mode-style-map nil
  entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-style-map
  nil '(2 2 2 2)))

;; **** markdown-mode-command hydra
;; ***** hydra-heads define
(defvar entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-command
  '("Basic"
    (("b m" markdown-other-window
      "Run 'markdown-command' on current buffer and display in other window"
      :enable t :exit t)
     ("b e" markdown-export
      "Run Markdown on the current buffer, save to file, and return the filename"
      :enable t :exit t)
     ("b o" markdown-open
      "Open file for the current buffer with 'markdown-open-command'"
      :enable t :exit t)
     ("b w" markdown-kill-ring-save
      "Run Markdown on file and store output in the kill ring"
      :enable t :exit t)
     ("b c" markdown-check-refs
      "Show all undefined Markdown references in current 'markdown-mode' buffer"
      :enable t :exit t)
     ("b u" markdown-unused-refs
      "Show all unused Markdown references in current 'markdown-mode' buffer"
      :enable t :exit t)
     ("b n" markdown-cleanup-list-numbers
      "Update the numbering of ordered lists"
      :enable t :exit t)
     ("b ]" markdown-complete-buffer
      "Complete markup for all objects in the current buffer"
      :enable t :exit t)
     ("b ^" markdown-table-sort-lines
      "Sort table lines according to the column at point"
      :enable t :exit t)
     ("b |" markdown-table-convert-region
      "Convert region from BEGIN to END to table with SEPARATOR"
      :enable t :exit t)
     ("b t" markdown-table-transpose
      "Transpose table at point"
      :enable t :exit t))
    "Preview"
    (("p c" markdown-preview
      "Run 'markdown-command' on the current buffer and view output in browser"
      :enable t :exit t)
     ("p e" markdown-export-and-preview
      "Export to XHTML using 'markdown-export' and browse the resulting file"
      :enable t :exit t)
     ("p l" markdown-live-preview-mode
      "Toggle native previewing on save for a specific markdown file"
      :enable t :exit t))))

;; ***** hydra-define

(entropy/emacs-lazy-initial-advice-before
 '(markdown-mode)
 "mdcmdmap-hydra-hollow-init"
 "mdcmdmaphydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'markdown-mode-command-map nil
  entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-command))

;; **** markdown-mode top dispatch hydra

;; ***** hydra heads define

(defvar entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-map
  `(
;; ****** Markup insertion & removal

    "Markup insertion & removal"
    (("C-c C-s"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'markdown-mode-style-map))
      "Markup Insertion"
      :enable t :exit t :map-inject t)

     ("C-c C-c"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'markdown-mode-command-map))
      "Common command map"
      :enable t :exit t :map-inject t)

     ("C-c C-l" markdown-insert-link
      "Insert new or update an existing link, with interactive prompts"
      :enable t :exit t :map-inject t)
     ("C-c C-k" markdown-kill-thing-at-point
      "Kill thing at point and add important text, without markup, to kill ring"
      :enable t :exit t :map-inject t))

;; ****** Promotion, demotion, and cycling

    "Promotion, demotion, and cycling"
    (("C-c C--" markdown-promote
      "Promote or move element at point to the left"
      :enable t :exit t :map-inject t)
     ("C-c C-=" markdown-demote
      "Demote or move element at point to the right"
      :enable t :exit t :map-inject t)
     ("C-c C-/" markdown-complete
      "Complete markup of object near point or in region when active"
      :enable t :exit t :map-inject t)
     )

;; ****** Following and doing things

    "Following and doing things"
    (("C-c C-o" markdown-follow-thing-at-point
      "Follow thing at point if possible, such as a reference link or wiki link"
      :enable t :exit t :map-inject t)
     ("C-c C-d" markdown-do
      "Do something sensible based on context at point"
      :enable t :exit t :map-inject t)
     ("C-c '" markdown-edit-code-block
      "Edit Markdown code block in an indirect buffer"
      :enable t :exit t :map-inject t)
     )

;; ****** Indentation

    "Indentation"
    (("C-m" markdown-enter-key
      "Handle RET depending on the context"
      :enable t :exit t :map-inject t)
     ("DEL" markdown-outdent-or-delete
      "Handle BACKSPACE by cycling through indentation points"
      :enable t :exit t :map-inject t)
     ("C-c >" markdown-indent-region
      "Indent the region from BEG to END using some heuristics"
      :enable t :exit t :map-inject t)
     ("C-c <" markdown-outdent-region
      "Call 'markdown-indent-region' on region from BEG to END with prefix"
      :enable t :exit t :map-inject t))

;; ****** Visibility cycling

    "Visibility cycling"
    (("TAB" markdown-cycle "Visibility cycling for Markdown mode"
      :enable t :exit t :map-inject t)
     ("<S-tab>" markdown-shifttab "Handle S-TAB keybinding based on context"
      :enable t :exit t :map-inject t)
     ("<backtab>" markdown-shifttab "Handle S-TAB keybinding based on context"
      :enable t :exit t :map-inject t))

;; ****** Heading and list navigation

    "Heading and list navigation"
    ((,entropy/emacs-ukrd-ouline-next-head markdown-outline-next
      "Move to next list item, when in a list, or next visible heading"
      :enable t :exit t :map-inject t)
     (,entropy/emacs-ukrd-ouline-prev-head markdown-outline-previous
      "Move to previous list item, when in a list, or previous visible heading"
      :enable t :exit t :map-inject t)
     ("C-c C-f" markdown-outline-next-same-level
      "Move to next list item or heading of same level"
      :enable t :exit t :map-inject t)
     ("C-c C-b" markdown-outline-previous-same-level
      "Move to previous list item or heading of same level"
      :enable t :exit t :map-inject t)
     ("C-c C-u" markdown-outline-up
      "Move to previous list item, when in a list, or next heading"
      :enable t :exit t :map-inject t))

;; ****** Subtree, list, and table editing

    "Subtree, list, and table editing"
    ((,entropy/emacs-ukrd-outline-move-subtree-up
      markdown-move-up "Move thing at point up"
      :enable t :exit t :map-inject t)
     (,entropy/emacs-ukrd-outline-move-subtree-down
      markdown-move-down "Move thing at point down"
      :enable t :exit t :map-inject t)
     (,entropy/emacs-ukrd-ouline-promote-sutree
      markdown-promote "Promote or move element at point to the left"
      :enable t :exit t :map-inject t)
     (,entropy/emacs-ukrd-ouline-demote-sutree
      markdown-demote "Demote or move element at point to the right"
      :enable t :exit t :map-inject t)
     ("C-c S-<up>" markdown-table-delete-row "Delete row or horizontal line at point from the table"
      :enable t :exit t :map-inject t)
     ("C-c S-<down>" markdown-table-insert-row "Insert a new row above the row at point into the table"
      :enable t :exit t :map-inject t)
     ("C-c S-<left>" markdown-table-delete-column "Delete column at point from table"
      :enable t :exit t :map-inject t)
     ("C-c S-<right>" markdown-table-insert-column "Insert a new table column"
      :enable t :exit t :map-inject t)
     ("C-c C-M-h" markdown-mark-subtree "Mark the current subtree"
      :enable t :exit t :map-inject t)
     ("C-x n s" markdown-narrow-to-subtree "Narrow buffer to the current subtree"
      :enable t :exit t :map-inject t)
     ("M-RET" markdown-insert-list-item "Insert a new list item"
      :enable t :exit t :map-inject t)
     ("C-c C-j" markdown-insert-list-item "Insert a new list item"
      :enable t :exit t :map-inject t))

;; ****** Blocks (one or more paragraphs)
    "Blocks (one or more paragraphs)"
    (("C-M-{" markdown-backward-block
      "Move the point to the start of the current Markdown block"
      :enable t :exit t :map-inject t)
     ("C-M-}" markdown-forward-block
      "Move forward to the next end of a Markdown block"
      :enable t :exit t :map-inject t)
     ("C-c M-h" markdown-mark-block
      "Put mark at end of this block, point at beginning"
      :enable t :exit t :map-inject t)
     ("C-x n b" markdown-narrow-to-block
      "Make text outside current block invisible"
      :enable t :exit t :map-inject t))

;; ****** Link Movement

    "Link Movement"
    (("M-n" markdown-next-link
      "Jump to next inline, reference, or wiki link"
      :enable t :exit t :map-inject t)
     ("M-p" markdown-previous-link
      "Jump to previous wiki link"
      :enable t :exit t :map-inject t))

;; ****** Toggling functionality

    "Toggling functionality"
    (("C-c C-x C-e" markdown-toggle-math
      "Toggle support for inline and display LaTeX math expressions"
      :enable t :exit t :map-inject t)
     ("C-c C-x C-f" markdown-toggle-fontify-code-blocks-natively
      "Toggle the native fontification of code blocks"
      :enable t :exit t :map-inject t)
     ("C-c C-x C-i" markdown-toggle-inline-images
      "Toggle inline image overlays in the buffer"
      :enable t :exit t :map-inject t)
     (,entropy/emacs-ukrd-toggle-link-display markdown-toggle-url-hiding
      "Toggle the display or hiding of URLs"
      :enable t :exit t :map-inject t)
     ("C-c C-x C-m" markdown-toggle-markup-hiding
      "Toggle the display or hiding of markup"
      :enable t :exit t :map-inject t))

;; ****** Alternative keys (in case of problems with the arrow keys)

    "Alternative keys (in case of problems with the arrow keys)"
    (;; ("C-c C-x u" markdown-move-up
     ;;  "Move thing at point up"
     ;;  :enable t :exit t :map-inject t)
     ;; ("C-c C-x d" markdown-move-down
     ;;  "Move thing at point down"
     ;;  :enable t :exit t :map-inject t)
     ;; ("C-c C-x l" markdown-promote
     ;;  "Promote or move element at point to the left"
     ;;  :enable t :exit t :map-inject t)
     ;; ("C-c C-x r" markdown-demote
     ;;  "Demote or move element at point to the right"
     ;;  :enable t :exit t :map-inject t)
     )

    ))


;; ***** hydra defiens
(entropy/emacs-lazy-initial-advice-after
 '(markdown-mode)
 "markdown-mode-hydra-hollow-init" "markdown-mode-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-define-major-mode-hydra
  'markdown-mode
  '(markdown-mode markdown-mode-map)
  (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
   'markdown-mode)
  entropy/emacs-markdown-pretty-hydra-heads-group-for-markdown-mode-map
  '((2 :width-desc "Markup insertion & removal && Promotion, demotion, and cycling")
    (2 :width-desc "Following and doing things && Indention")
    (2 :width-desc "Visibility cycle and heading or listing navigation")
    (2 :width-desc "Subtree, list and table editting && Blocks editting")
    (2 :width-desc "Link movement && Toggleing functionality ")
    (2 :width-desc "Alternatives"))))

;; ** markdown preview
;; *** simple preview
;; Render and preview via `grip'
;; you can install grip by 'pip install grip'
(defun entropy/emacs-markdown-preview-grip ()
  "Render and preview with `grip'."
  (declare (interactive-only t))
  (interactive nil markdown-mode)
  (if (executable-find "grip")
      (let ((port "6419")
            (browse-url-browser-function
             (entropy/emacs-browse-url-function-get-for-web-preview)))
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) port)
        (browse-url (format "http://localhost:%s/%s.%s"
                            port
                            (file-name-base
                             (buffer-file-name))
                            (file-name-extension
                             (buffer-file-name)))))
    (user-error "Please install grip by 'pip install grip'.")))

(entropy/emacs-lazy-initial-for-hook
 '(markdown-mode-hook)
 "markdown-grip-preview-hydra-hollow-init"
 "markdown-grip-preview-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define+
  'markdown-mode-command-map nil
  '("Preview"
    (("p g" entropy/emacs-markdown-preview-grip
      "Preview markdown buffer using python grip"
      :enable t :exit t)))))

;; *** synchronization previewing
(use-package markdown-preview-mode
  :after markdown-mode
  :preface

  (defun entropy/emacs-markdown--mdp-before-advice (&rest _)
    "Before advice for `markdown-preview-mode' when it trigger
to disable `markdown-preview-mode' for clean up all web-sockets
to prevent ports keeping as causing to next previewing error.

This issue refer to
`https://github.com/ancane/markdown-preview-mode/issues/31'.
"
    (cond
     (markdown-preview-mode
      (markdown-preview-cleanup)
      (message "Clean up all markdown preview websockets done!"))))

  :commands (markdown-preview-mode)
  :bind (:map markdown-mode-command-map
              ("P" . markdown-preview-mode))
  :eemacs-indhca
  (((:enable t :defer (:data (:adfors (markdown-mode-hook) :adtype hook :pdumper-no-end t)))
    (markdown-mode-command-map))
   ("Preview"
    (("p p" markdown-preview-mode
      "Live preview markdown buffer with external browser"
      :enable t :exit t))))

  :config
  (setq markdown-preview-stylesheets
        entropy/emacs-markdown-preview-stylesheets
        markdown-preview-javascript
        entropy/emacs-markdown-preview-javascript)

  (advice-add 'markdown-preview-mode
              :before
              #'entropy/emacs-markdown--mdp-before-advice)

  (advice-add 'markdown-preview-mode
              :around
              #'entropy/emacs-markdown--with-eemacs-preview-browser)

  (advice-add 'markdown-preview-mode
              :around #'entropy/emacs-lang-use-utf-8-ces-around-advice))

(provide 'entropy-emacs-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
