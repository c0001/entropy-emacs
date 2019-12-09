;;; entropy-emacs-popwin.el --- window or buffer popuped feature for 'entropy-emacs'
;;
;; * Copyright (C) 20190821  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-popwin.el
;; Compatibility: GNU Emacs emacs-version;
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
;; Using `shackle-mode' and `popwin-mode' to manage the buffer or
;; window popuping feature, detailes specific for =entropy-emacs=.
;;
;; Let's say that, the temporally buffer or window attaching during
;; the working with emacs was frequently and indeedly useful, also
;; the 'temporally' meaning that user can benefitted burry them with
;; =C-g= keybing which was the `keyboard-quit' command built-in of
;; emacs.
;;
;; * Configuration:
;;
;; Designed for =entropy-emacs= only without inidividually using
;; warranty.
;;
;; Sets of functions used as library came from other designation of
;; =entropy-emacs=, thus correctly extracting theme from that was
;; necessary for hacking.
;; 
;; * Code:
;; ** popwin-mode
(use-package popwin
  :if (eq entropy/emacs-use-popup-window-framework 'popwin)
  :defines (popwin:keymap)
  :commands popwin-mode
  :init

  (entropy/emacs-lazy-with-load-trail
   popwin-mode
   (popwin-mode t))
  
  (entropy/emacs-!set-key (kbd "3") popwin:keymap)

  :config
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(;; Emacs
          ("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\\*Async Shell Command\\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)

          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)

          ;; Flycheck
          ("\\*flycheck errors\\*.+*$" :regexp t :position bottom :stick t :noselect nil)

          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)

          ;; Google translate
          ("*Google Translate*" :dedicated t :position bottom)

          ;; Moedict
          ("*[萌典] 查詢結果*" :dedicated t :position bottom)
          
          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)

          ;; Diff
          ("*Diff*" :dedicated t :position bottom :noselect nil)
          
          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)

          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)

          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ;; ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)

          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Magit
          ;; (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          ;; (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)

          ;; Script
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)

          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Entropy refer
          ("^\\*entropy/cpmv" :regexp t :position bottom :stick nil :noselect nil)
          ("^\\*entropy/cndt" :regexp t :position bottom :stick nil :noselect nil)
          ("^\\*entropy/sdcv" :regexp t :position bottom :stick nil :noselect nil)

          ;; sbcl-mode
          ("^\\*slime-" :regexp t :stick t :position bottom :noselect nil :height 0.4)
          ("^\\*sldb" :regexp t :stick t :position bottom :noselect nil :height 0.4)
          )))

;; ** shackle mode
(use-package shackle
  :if (eq entropy/emacs-use-popup-window-framework 'shackle)
  :commands (shackle-mode
             shackle-display-buffer
             shackle-popup-buffer
             shackle-popup-find-file)
  :preface
  (defvar shackle-popup-mode-map
    (let ((map (make-sparse-keymap)))
      map))
  :init
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (entropy/emacs-lazy-with-load-trail
   shackle-mode
   (shackle-mode t))

  (entropy/emacs-!set-key (kbd "3") shackle-popup-mode-map)
  
  :bind
  (:map shackle-popup-mode-map
   ("o" . shackle-popup-buffer)
   ("f" . shackle-popup-find-file)
   ("e" . shackle-popup-message))
  
  :config

  (defun shackle-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))
  (bind-key "C-h z" #'shackle-last-popup-buffer)

  ;; Add keyword: `autoclose'
  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list)
            (recenter-top-bottom '(middle)))))))

  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

  (defun shackle-popup-buffer ()
    (interactive)
    (let* ((buff-name (completing-read "Buffer choosing: " 'internal-complete-buffer))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (get-buffer-create buff-name)
      (display-buffer buff-name)
      (when (and (fboundp 'solaire-mode)
                 (entropy/emacs-theme-adapted-to-solaire))
        (with-current-buffer buff-name
          (solaire-mode +1)))))

  (defun shackle-popup-find-file ()
    (interactive)
    (let* ((file (completing-read "Buffer choosing: " 'read-file-name-internal))
           (buff-name (buffer-name (find-file-noselect file)))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (display-buffer buff-name)))

  (defun shackle-popup-message ()
    (interactive)
    (let* ((buff-name (buffer-name (get-buffer-create "*Messages*")))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (with-current-buffer buff-name
        (unless (eobp)
          (goto-char (point-max))))
      (display-buffer buff-name)))
  
  ;; rules
  (setq shackle-default-size 0.6)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (setq shackle-rules
        '(("*Help*" :select t :align below :autoclose t)
          ("*compilation*" :align below :autoclose t)
          ("*Completions*" :align below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align below :autoclose t)
          ("*ert*" :align below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align below)
          ("*Warnings*" :align below :autoclose t)
          ("*Messages*" :align below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :align below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align below :autoclose t)
          ("*Calendar*" :select t :align below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align below)
          (" *undo-tree*" :select t :align right)
          ("*Paradox Report*" :align below :autoclose t)
          ("*quickrun*" :select t :size 15 :align below)
          ("*tldr*" :align below :autoclose t)
          ("*Youdao Dictionary*" :align below :autoclose t)
          ("*Google Translate*" :align below :select t :size 0.5 :autoclose t)
          ("*Finder*" :select t :align below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align below :autoclose t)
          ("*lsp-help*" :align below :autoclose t)
          ("*lsp session*" :size 0.4 :align below :autoclose t)
          (" *Org todo*" :select t :size 4 :align below :autoclose t)
          ("*Org Dashboard*" :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/cpmv" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/cndt" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/sdcv" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*slime-" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*sldb" :regexp t :select t :size 0.4 :align below :autoclose t)

          (ag-mode :select t :align below)
          (grep-mode :select t :align below)
          (pt-mode :select t :align below)
          (rg-mode :select t :align below)

          (flycheck-error-list-mode :select t :align below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :align below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align below :autoclose t)
          (comint-mode :align below)
          (helpful-mode :select t :size 0.4 :align below :autoclose t)
          (process-menu-mode :select t :align below :autoclose t)
          (list-environment-mode :select t :align below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align below)
          (tabulated-list-mode :align below))))

;; * provide
(provide 'entropy-emacs-popwin)
