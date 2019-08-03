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
;; customized variable group `entropy/emacs-highlight'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

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
  :commands (symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
         ("C-M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ([M-f3] . symbol-overlay-remove-all))
  :init
  (when entropy/emacs-hl-sysmbol-overlay-enable-at-startup
    (add-hook 'prog-mode-hook #'symbol-overlay-mode)))

;; ** Highlight matching paren
(use-package paren
  :ensure nil
  :init (add-hook 'entropy/emacs-init-X-hook #'show-paren-mode)
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
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  (add-hook 'help-mode-hook #'rainbow-mode)
  (entropy/emacs-lazy-load-simple 'web-mode
    (add-hook 'web-mode-hook #'rainbow-mode))
  (entropy/emacs-lazy-load-simple 'css-mode
    (add-hook 'css-mode-hook #'rainbow-mode)))

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
  :commands (global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :init
  (when entropy/emacs-hl-todo-enable-at-startup
    (add-hook 'entropy/emacs-init-X-hook #'global-hl-todo-mode)))

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

(if sys/win32p
    ;; In windows system
    (when (and entropy/emacs-wsl-enable entropy/emacs-hl-diff-hl-enable-at-startup)
      (use-package diff-hl
        :commands (global-diff-hl-mode)
        :bind (:map diff-hl-command-map
                    ("SPC" . diff-hl-mark-hunk))
        :init
        (add-hook 'entropy/emacs-init-X-hook #'global-diff-hl-mode)

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
        (entropy/emacs-lazy-load-simple 'magit
          (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

  ;; In Not-windows system
  (when entropy/emacs-hl-diff-hl-enable-at-startup
    (use-package diff-hl
      :commands (global-diff-hl-mode)
      :bind (:map diff-hl-command-map
                  ("SPC" . diff-hl-mark-hunk))
      :init
      (add-hook 'entropy/emacs-init-X-hook #'global-diff-hl-mode)

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
      (entropy/emacs-lazy-load-simple 'magit
        (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))))

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
  :init (add-hook 'entropy/emacs-init-X-hook #'volatile-highlights-mode))

;; ** Visualize TAB, (HARD) SPACE, NEWLINE
;;
;; This package is a minor mode to visualize blanks (TAB, (HARD) SPACE
;; and NEWLINE).

(when entropy/emacs-hl-whitespace-enable-at-startup
  (use-package whitespace
    :ensure nil
    :diminish whitespace-mode
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

    (entropy/emacs-lazy-load-simple 'popup
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
            (whitespace-mode 1))))))

;; * provide
(provide 'entropy-emacs-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
