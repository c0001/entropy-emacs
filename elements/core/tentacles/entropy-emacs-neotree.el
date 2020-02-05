;;; entropy-emacs-neotree.el --- Vim neotree port for entropy-emacs
;;
;; * Copyright (C) 20190907  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-neotree.el
;; Keywords:      neotree, bar,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
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
;; Neotree side bar configuration for =entropy-emacs=.
;; 
;; * Configuration:
;; 
;; Using for =entropy-emacs= only.
;; 
;; * Code:
;; ** Require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)

;; ** neotree
(use-package neotree
  :if (eq entropy/emacs-tree-visual-type 'neotree)
  :commands (neotree-toggle
             neotree-mode
             entropy/emacs-neotree-neotree-close
             entropy/emacs-neotree-neotree-refresh-for-current
             entropy/emacs-neotree-neo-open-with)
  :bind (("<f8>" . entropy/emacs-neotree-neotree-refresh-for-current)
         ("C-<f8>" . entropy/emacs-neotree-neotree-close))

  :preface
  (defun entropy/emacs-neotree-neo-open-with (full-path &rest _)
    "Open neotree node item in external apps powered by
`entropy-open-with'."
    (interactive)
    (require 'entropy-open-with)
    (entropy/open-with-match-open (list full-path)))

  (defun entropy/emacs-neotree-neo-up-dir (&rest _)
    (interactive)
    (goto-char (point-min))
    (neotree-select-up-node))

  (defun entropy/emacs-neotree-neo-load-node (full-path &rest _)
    (interactive)
    (if (file-directory-p full-path)
        (progn
          (add-to-list 'load-path (expand-file-name full-path))
          (message "Adding load path '%s' ..." full-path))
      (load full-path)))

  :init
  (setq neo-theme (if (or (display-graphic-p) entropy/emacs-fall-love-with-pdumper) 'icons 'arrow)
        neo-autorefresh t
        neo-hidden-regexp-list nil
        neo-auto-indent-point t)

  :config

  ;; key register
  ;; Make node item execution for neotree with `entropy-open-with'
  (when (or (display-graphic-p)
            entropy/emacs-use-emacs-in-terminal-with-graphic-features)
    (define-key neotree-mode-map (kbd "M-RET")
      (neotree-make-executor
       :file-fn 'entropy/emacs-neotree-neo-open-with
       :dir-fn  'entropy/emacs-neotree-neo-open-with))
    (define-key neotree-mode-map (kbd "<M-up>")
      #'entropy/emacs-neotree-neo-up-dir)
    (define-key neotree-mode-map (kbd "+")
      #'neotree-create-node)
    (define-key neotree-mode-map (kbd "w")
      #'neotree-copy-filepath-to-yank-ring)
    (define-key neotree-mode-map (kbd "M-l")
      (neotree-make-executor
       :file-fn #'entropy/emacs-neotree-neo-load-node
       :dir-fn #'entropy/emacs-neotree-neo-load-node))
    (define-key neotree-mode-map (kbd "D")
      #'neotree-delete-node))
  
  ;; library
  (defvar entropy/emacs-neotree--neo-textscaled nil
    "Indicator for preventing further textscale for neo buffer.")
  
  (defun entropy/emacs-neotree--neo-refresh-filter ()
    (catch :exit
      (when (or (string-match-p "\\*w3m" (buffer-name))
                (bound-and-true-p lsp-ui-doc--bounds))
        (throw :exit 'stick))))

  (defun entropy/emacs-neotree--neo-refresh-conditions (orig_func &rest orig_args)
    (unless (eq 'stick (entropy/emacs-neotree--neo-refresh-filter))
      (funcall-interactively orig_func)))
  
  (defun entropy/emacs-neotree--neo-pos-hl-and-indent (&optional non_indent)
    "Highlight current neotree buffer line and goto the first
word of current-line for preventing the long line truncate view."
    (hl-line-mode 1)
    (unless non_indent
      (forward-line 0)
      (re-search-forward "\\w" (line-end-position 1) t)))
  
  ;; redefinations
  (defun neo-global--attach ()
    "Attach the global neotree buffer

Note: this function has been modified by entropy-emacs for reason
of forcely repeating the global-refresh behaviour."
    (when neo-global--autorefresh-timer
      (cancel-timer neo-global--autorefresh-timer))
    (when neo-autorefresh
      (setq neo-global--autorefresh-timer
            (run-with-idle-timer 1.2 t 'neo-global--do-autorefresh)))
    (setq neo-global--buffer (get-buffer neo-buffer-name))
    (setq neo-global--window (when (not (null neo-global--buffer))
                               (get-buffer-window
                                neo-global--buffer)))
    (neo-global--with-buffer
      (neo-buffer--lock-width))
    (when (window-live-p neo-global--window)
      (set-window-parameter neo-global--window 'no-delete-other-windows t)
      (set-window-dedicated-p neo-global--window t))
    (run-hook-with-args 'neo-after-create-hook '(window)))

  ;; Hooks
  ;; Retach neotree window when eyerbowse switched workspaces
  (add-hook 'eyebrowse-post-window-switch-hook #'neo-global--attach)

  ;; advices
  (advice-add 'neo-buffer--goto-cursor-pos :after #'entropy/emacs-neotree--neo-pos-hl-and-indent)
  (advice-add 'neo-global--do-autorefresh :around #'entropy/emacs-neotree--neo-refresh-conditions)
  
  ;; interactive
  (defun entropy/emacs-neotree-neotree-close ()
    "Globally close the neotree buffer and window."
    (interactive)
    (when neo-global--buffer (kill-buffer neo-global--buffer))
    (mapcar (lambda (x)
              (when (equal (buffer-name (window-buffer x)) neo-buffer-name)
                (delete-window-internal x)))
            (window-list))
    (setf neo-global--buffer nil
          neo-global--window nil
          entropy/emacs-neotree--neo-textscaled nil))

  (defun entropy/emacs-neotree-neotree-refresh-for-current ()
    "Open neotree with current working directory.

Globally close neotree buffer while selected window was
`neo-global--window'."
    (interactive)
    (let ((buffer_ (current-buffer))
          (bfn (ignore-errors
                 (file-name-nondirectory
                  (buffer-file-name (current-buffer)))))
          (marker (point)))
      (cond
       ((equal buffer_ neo-global--buffer)
        (entropy/emacs-neotree-neotree-close))
       (t
        (unless  (eq 'stick (entropy/emacs-neotree--neo-refresh-filter))
          (unless (neo-global--window-exists-p)
            (save-window-excursion
              (neotree-show)))
          (neo-buffer--refresh t t)
          (when (ignore-errors (stringp bfn))
            (goto-char (point-min))
            (re-search-forward bfn nil t)
            (entropy/emacs-neotree--neo-pos-hl-and-indent)
            (recenter))
          (save-excursion
            (switch-to-buffer buffer_)
            (goto-char marker))
          (neo-global--select-window)
          (unless (and entropy/emacs-neotree--neo-textscaled
                       (display-graphic-p))
            (with-current-buffer neo-buffer-name
              (cond
               ((> entropy/emacs-neotree-text-scale 0)
                (text-scale-increase entropy/emacs-neotree-text-scale))
               ((< entropy/emacs-neotree-text-scale 0)
                (text-scale-decrease (abs entropy/emacs-neotree-text-scale)))
               ((= entropy/emacs-neotree-text-scale 0)
                nil)
               ((not (integerp entropy/emacs-neotree-text-scale))
                (error "Wrong type of argument for 'entropy/emacs-neotree-text-scale'"))))
            (setq entropy/emacs-neotree--neo-textscaled t)))))))

  ;; specifications
  ;; enable doom-theme neotree visualized spec
  (when (string-match-p "^doom-" (symbol-name entropy/emacs-theme-options))
    (doom-themes-neotree-config)))

(provide 'entropy-emacs-neotree)
