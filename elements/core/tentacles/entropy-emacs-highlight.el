;;; entropy-emacs-highlight.el --- entropy-emacs coding page visualized configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-highlight.el
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
;; `entropy-emacs' coding style for visualized aspect configuration.
;;
;; Coding parentness highlighting, coding symbole matching
;; highlighting even for file version tracking highlighting powered
;; by sets of third-party packages.
;;
;; Enable the main key for subs-config
;; i.e. `entropy/emacs-use-highlight-features', for subs, see
;; customized variable group `entropy/emacs-customize-group-for-highlight-features'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require

;; ** Highlight symbols
;;
;; Highlight symbols with overlays while providing a keymap for
;; various operations about highlighted symbols.  It was originally
;; inspired by the package `highlight-symbol`.  The fundamental
;; difference is that in `symbol-overlay` every symbol is highlighted
;; by the Emacs built-in function `overlay-put` rather than the
;; `font-lock` mechanism used in `highlight-symbol`.

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :commands (symbol-overlay-mode
             symbol-overlay-put
             symbol-overlay-remove-all
             symbol-overlay-jump-prev)
;; *** preface
  :preface

  (defun entropy/emacs-hl-symbol-overlay-toggle ()
    (interactive)
    (symbol-overlay-mode 'toggle))
  (defun entropy/emacs-hl-symbol-overlay-put ()
    (interactive)
    (unless (bound-and-true-p symbol-overlay-mode)
      (symbol-overlay-mode +1))
    (symbol-overlay-put))
  (defun entropy/emacs-hl-symbol-overlay-remove-all ()
    (interactive)
    (unless (bound-and-true-p symbol-overlay-mode)
      (symbol-overlay-mode +1))
    (symbol-overlay-remove-all))
  (defun entropy/emacs-hl-symbol-overlay-jump-next ()
    (interactive)
    (unless (bound-and-true-p symbol-overlay-mode)
      (symbol-overlay-mode +1))
    (symbol-overlay-jump-next))
  (defun entropy/emacs-hl-symbol-overlay-jump-prev ()
    (interactive)
    (unless (bound-and-true-p symbol-overlay-mode)
      (symbol-overlay-mode +1))
    (symbol-overlay-jump-prev))

;; *** eemacs hydra hollow
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h s e" entropy/emacs-hl-symbol-overlay-toggle
      "Toggle symbol overlay mode"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p symbol-overlay-mode)
          t
        nil))
     ("C-c h s r" symbol-overlay-rename
      "Rename symbol at point on all its occurrences."
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h s u" entropy/emacs-hl-symbol-overlay-put
      "Toggle all overlays of symbol at point"
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h s d" entropy/emacs-hl-symbol-overlay-remove-all
      "Remove all highlighted symbols in the buffer"
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h s p" entropy/emacs-hl-symbol-overlay-jump-prev
      "Jump to the previous location of symbol at point"
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h s n" entropy/emacs-hl-symbol-overlay-jump-next
      "Jump to the next location of symbol at point"
      :enable t
      :eemacs-top-bind t
      :exit t))))

;; *** init
  :init
  (if (and entropy/emacs-hl-sysmbol-overlay-enable-at-startup
           ;; we just enable symbol-overlay for all mode derived from
           ;; `prog-mode' when `entropy/emacs-ide-suppressed' is null
           ;; in which case all those modes doesn't have native symbol
           ;; highlight feature yet.
           (null entropy/emacs-ide-suppressed)
           ;; And not used in lsp related session, since lsp already
           ;; have thus functionality.
           (not (member entropy/emacs-ide-use-for-all
                        '(lsp eglot))))
      (add-hook 'prog-mode-hook #'symbol-overlay-mode)
    ;; emacs lisp mode doesn't have code-server support so we use
    ;; `symbol-overlay-mode' forcely enable injections for those
    ;; hooks.
    (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
      (add-hook hook 'symbol-overlay-mode)))

;; *** config
  :config

;; ***** advices
;; ****** timer reset
  (defun __adv/around/symbol-overlay-cancel-timer/0
      (orig-func &rest orig-args)
    "Forcely reset `symbol-overly-timer' for bug fix."
    (prog1
        (apply orig-func orig-args)
      ;; Force bind it to nil since the `cancel-timer' function not
      ;; care about the symbol.
      (setq-local symbol-overlay-timer nil)))
  (advice-add 'symbol-overlay-cancel-timer
              :around
              #'__adv/around/symbol-overlay-cancel-timer/0)

;; ****** after change hook modification

  ;; BUG: we don't need the refrresh globally since it is hard coded in the
  ;; source.
  (when (member 'symbol-overlay-refresh after-change-functions)
    (remove-hook 'after-change-functions 'symbol-overlay-refresh))

  (defun __adv/around/symbol-overlay-mode/0
      (orig-func &rest orig-args)
    (prog1
        (apply orig-func orig-args)
      (cond ((bound-and-true-p symbol-overlay-mode)
             ;; FIXME: NOTE: below local binding is mistake and will
             ;; messy up all buffres overlay display function related
             ;; operatin like `vr/replace' or `isearch-forward' etc. Why?
             ;; ---> (make-variable-buffer-local 'after-change-functions)
             (add-hook 'after-change-functions
                       'symbol-overlay-refresh nil t))
            (t
             (remove-hook 'after-change-functions
                          'symbol-overlay-refresh t)))))
  (advice-add 'symbol-overlay-mode
              :around
              #'__adv/around/symbol-overlay-mode/0)
  )

;; ** Highlight matching paren
(use-package paren
  :ensure nil
  :init
  (entropy/emacs-lazy-with-load-trail
   show-paren-mode
   :pdumper-no-end t
   :body
   (show-paren-mode t))
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; ** Highlight surrounding parentheses
;;
;; Highlight surrounding parentheses in Emacs, Enable the mode using
;; `M-x highlight-parentheses-mode` or by adding it to a hook.

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :commands (highlight-parentheses-mode)
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h p e" highlight-parentheses-mode
      "highlight the surrounding parentheses"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p highlight-parentheses-mode)
          t
        nil)))))
  :init
  (when entropy/emacs-hl-highlight-parentheses-mode-enable-at-startup
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
  :config (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; ** Highlight indentions
;;
;; This minor mode highlights indentation levels via font-lock. Indent
;; widths are dynamically discovered, which means this correctly
;; highlights in any mode, regardless of indent width, even in
;; languages with non-uniform indentation such as Haskell. By default,
;; this mode also inspects your theme dynamically, and automatically
;; chooses appropriate colors for highlighting. This mode works
;; properly around hard tabs and mixed indentation, and it behaves
;; well in large buffers.

(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h p i" highlight-indent-guides-mode
      "Display indent guides in a buffer"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p highlight-indent-guides-mode)
          t
        nil)))))
  :init
  (when entropy/emacs-hl-highlight-indention-enable-at-startup
    (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
  :config (setq highlight-indent-guides-method 'character))

;; ** Colorize color names in buffers
;;
;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h p r" rainbow-mode
      "Turn on Rainbow-Mode"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p rainbow-mode)
          t
        nil)))))
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  ;; Do not using rainbow mode in `help-mode' since it will messy the
  ;; default face of string context of documentation.
  ;; ;; (add-hook 'help-mode-hook #'rainbow-mode)
  (entropy/emacs-lazy-load-simple web-mode
    (add-hook 'web-mode-hook #'rainbow-mode))
  (entropy/emacs-lazy-load-simple css-mode
    (add-hook 'css-mode-hook #'rainbow-mode))

  ;; Reset `rainbow-x-colors-font-lock-keywords' when toggle daemon
  ;; session graphic type since TUI has restriction show full colors
  ;; which gui supported and thus like other wise.
  (entropy/emacs-with-daemon-make-frame-done
   'rainbow-x-color-reset
   nil nil
   '(let (enabled-buffs)
      (mapc
       (lambda (buff)
         (with-current-buffer buff
           (when (bound-and-true-p rainbow-mode)
             (rainbow-mode 0)
             (push buff enabled-buffs))))
       (buffer-list))
      (setq rainbow-x-colors-font-lock-keywords
            `((,(regexp-opt (x-defined-colors) 'words)
               (0 (rainbow-colorize-itself)))))
      (mapc
       (lambda (buff)
         (with-current-buffer buff
           (rainbow-mode 1)))
       enabled-buffs)))
  )

;; ** Highlight brackets according to their depth
;;
;; rainbow-delimiters is a "rainbow parentheses"-like mode which
;; highlights delimiters such as parentheses, brackets or braces
;; according to their depth. Each successive level is highlighted in a
;; different color. This makes it easy to spot matching delimiters,
;; orient yourself in the code, and tell which statements are at a
;; given depth.

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h p d" rainbow-delimiters-mode
      "Toggle Rainbow-Delimiters mode"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p rainbow-delimiters-mode)
          t
        nil)))))
  :init
  (when entropy/emacs-hl-rainbow-delimiters-enable-at-startup
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

;; ** Highlight TODO and similar keywords in comments and strings
;;
;; To highlight keywords turn on hl-todo-mode in individual buffers or use the the global
;; variant global-hl-todo-mode.
;;
;; This package also provides commands for moving to the next or
;; previous keyword and to invoke occur with a regexp that matches all
;; known keywords. If you want to use these commands, then you should
;; bind them in hl-todo-mode-map.

(use-package hl-todo
  :commands (global-hl-todo-mode
             hl-todo-occur
             hl-todo-previous
             hl-todo-next
             hl-todo-occur)
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h t o" hl-todo-occur
      "Find All TODO Keywords"
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h t p" hl-todo-previous
      "Jump to the previous TODO"
      :enable t
      :eemacs-top-bind t
      :exit t)
     ("C-c h t n" hl-todo-next
      "Jump to the next TODO"
      :enable t
      :eemacs-top-bind t
      :exit t))))
  :init

  (defvar entropy/emacs-hl-todo-keywords-faces
    '(("EEMACS_MAINTENANCE" . "green")
      ("EEMACS_BUG" . "red")
      ("EEMACS_TEMPORALLY_HACK" . "yellow")
      ("EEMACS_REFERENCE" . "HotPink1"))
    "Specified eemacs stick term keywords with the faces spec to
`hl-todo-mode'")

  (defun entropy/emacs-hl-todo-keywords-add-term
      ()
    "Add `entropy/emacs-hl-todo-keywords-faces' to
`hl-todo-keyword-faces'."
    (let ((eemacs-spec
           entropy/emacs-hl-todo-keywords-faces))
      (dolist (el eemacs-spec)
        (add-to-list 'hl-todo-keyword-faces
                     el))))

  (when entropy/emacs-hl-todo-enable-at-startup
    (entropy/emacs-lazy-with-load-trail
     global-hl-todo
     :body
     (progn
       (global-hl-todo-mode t))))

  :config
  (entropy/emacs-hl-todo-keywords-add-term) ;add specified term after
                                            ;load
                                            ;`hl-todo-keyword-faces'

  ;; FIXME: too ensure the term adding for pdumper session
  (unless (bound-and-true-p entropy/emacs-custom-enable-lazy-load)
    (defun __ya/hl-todo-mode (orig-func &rest orig-args)
      (progn
        (when (bound-and-true-p hl-todo-mode)
          (entropy/emacs-hl-todo-keywords-add-term))
        (apply orig-func orig-args)))
    ;; EEMACS_MAINTENANCE: internally Api may need to update with
    ;; upstream.
    (advice-add 'hl-todo--setup
                :around
                #'__ya/hl-todo-mode))
  )

;; ** Highlight uncommitted changes
;;
;; diff-hl-mode highlights uncommitted changes on the left side of
;; the window, allows you to jump between and revert them
;; selectively.
;;
;; For the usage instructions and the list of commands, see the
;; Commentary section inside the file.
;;
;; Tested with Git, Mercurial, Bazaar and SVN. May work with other VC
;; backends, too.
;;
;; The package also contains auxiliary modes:
;;
;; - diff-hl-dired-mode provides similar functionality in Dired.
;;
;; - diff-hl-margin-mode changes the highlighting function to use the
;;   margin instead of the fringe.
;;
;; - diff-hl-amend-mode shifts the reference revision back by one.
;;
;; - diff-hl-flydiff-mode implements highlighting changes on the
;;   fly. It requires Emacs 24.4 or newer.
;;
;; Check out the Commentary section in each respective file for the
;; usage instructions.

;; In windows system

(use-package diff-hl
  :if (and (or (and entropy/emacs-wsl-enable
                    sys/win32p)
               sys/is-posix-compatible)
           entropy/emacs-hl-diff-hl-enable-at-startup)
  :commands (global-diff-hl-mode
             diff-hl-mode)
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :eemacs-tpha
  (((:enable t))
   ("Hightlight"
    (("C-c h d h" diff-hl-mode
      "Toggle VC diff highlighting"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p diff-hl-mode)
          t
        nil)))))
  :init
  (entropy/emacs-lazy-with-load-trail
   global-diff-hl
   :pdumper-no-end t
   :body
   (global-diff-hl-mode t))

  ;;(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
        ;;;; This may be cause dired be crash for big git
        ;;;; repo such as linux kernel repo

  :config
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (setq diff-hl-side 'right)
    (diff-hl-margin-mode 1))

  ;; Integration with magit and psvn
  (entropy/emacs-lazy-load-simple magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; ** Highlight some operations
;;
;; This library provides minor mode volatile-highlights-mode, which
;; brings visual feedback to some operations by highlighting portions
;; relating to the operations.
;;
;; All of highlights made by this library will be removed when any
;; new operation is executed.
;;
;;
;; To toggle volatile highlighting, type:
;;
;; ~M-x volatile-highlights-mode <RET>~
;;
;; While this minor mode is on, a string `VHL’ will be displayed on
;; the modeline.
;;
;; Currently, operations listed below will be highlighted While the
;; minor mode `volatile-highlights-mode’ is on:
;;
;; - undo
;;
;;   Volatile highlights will be put on the text inserted by undo.
;;
;; - yank and yank-pop
;;
;;   Volatile highlights will be put on the text inserted by yank’ or
;;   yank-pop.
;;
;; - kill-region, kill-line, any other killing function
;;
;;   Volatile highlights will be put at the positions where the
;;   killed text used to be.
;;
;; - delete-region
;;
;;   Same as kill-region, but not as reliable since delete-region is
;;   an inline function.
;;
;; - find-tag
;;
;;   Volatile highlights will be put on the tag name which was found
;;   by find-tag.
;;
;; - occur-mode-goto-occurrence and occur-mode-display-occurrence
;;
;;   Volatile highlights will be put on the occurrence which is
;;   selected by occur-mode-goto-occurrence or
;;   occur-mode-display-occurrence.

(use-package volatile-highlights
  :commands (volatile-highlights-mode)
  :diminish volatile-highlights-mode
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h d v" volatile-highlights-mode
      "visual feedback on some operations"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p volatile-highlights-mode)
          t
        nil)))))
  :init
  (entropy/emacs-lazy-with-load-trail
   volatile-hl-mode
   :pdumper-no-end t
   :body
   (volatile-highlights-mode t)))

;; ** Visualize TAB, (HARD) SPACE, NEWLINE
;;
;; This package is a minor mode to visualize blanks (TAB, (HARD) SPACE
;; and NEWLINE).

(use-package whitespace
  :if entropy/emacs-hl-whitespace-enable-at-startup
  :ensure nil
  :diminish whitespace-mode
  :eemacs-tpha
  (((:enable t))
   ("Highlight"
    (("C-c h d w" whitespace-mode
      "Toggle whitespace visualization"
      :enable t
      :eemacs-top-bind t
      :toggle
      (if (bound-and-true-p whitespace-mode)
          t
        nil)))))
  :init
  (dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (when entropy/emacs-hl-whitespace-auto-cleanup
    (setq whitespace-action '(auto-cleanup)))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (entropy/emacs-lazy-load-simple popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

;; * provide
(provide 'entropy-emacs-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
