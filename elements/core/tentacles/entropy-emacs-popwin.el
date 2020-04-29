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
;; ** require

(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-hydra-hollow)

;; ** library
(setq entropy/emacs-popwin-union-rule-map
      '((:regexp
         :all (lambda (x rule) (list :regexp x)))
        (:select
         :popwin (lambda (x rule) (list :noselect (not x)))
         :shackle (lambda (x rule) (list :select x)))
        (:align
         :popwin (lambda (x rule) (list :position x))
         :shackle (lambda (x rule)
                    (list :align
                          (cl-case x
                            (bottom (quote 'below))
                            (top (quote 'above))
                            (t
                             (quote (quote x)))))))
        (:size
         :popwin (lambda (x rule)
                   (list (if (member (plist-get (cdr rule) :align) '(bottom top))
                             :height
                           :width)
                         x))
         :shackle (lambda (x rule) (list :size x)))
        (:autoclose
         :popwin (lambda (x rule) (list :stick x))
         :shackle (lambda (x rule) (list :autoclose x)))
        (:dedicated
         :popwin (lambda (x rule) (list :dedicated x))
         :shackle (lambda (x rule) (list :popup (null x))))))

(defun entropy/emacs-popwin-make-rule-spec
    (type eemacs-popwin-rule-list)
  (let (rtn)
    (dolist (rule eemacs-popwin-rule-list)
      (let* ((condition (car rule))
             (attrs (cdr rule))
             (cnt 0)
             (maxlen (length attrs))
             (rule-transed (list condition)))
        (while (<= cnt (- maxlen 2))
          (let* ((slot (nth cnt attrs))
                 (slot-value (nth (+ 1 cnt) attrs))
                 (trans-func (or (plist-get
                                  (alist-get slot entropy/emacs-popwin-union-rule-map)
                                  type)
                                 (plist-get
                                  (alist-get slot entropy/emacs-popwin-union-rule-map)
                                  :all))))
            (setq rule-transed
                  (append rule-transed
                          (funcall trans-func slot-value rule))
                  cnt (+ 2 cnt))))
        (setq rtn (append rtn (list rule-transed)))))
    rtn))

(setq entropy/emacs-popwin-regists
      '(;; Emacs
        ("*Help*" :dedicated t :align bottom :autoclose t :select t :size 0.4)
        ("*Messages*" :dedicated t :align bottom :autoclose t :select t :size 0.4)
        ("*compilation*" :dedicated t :align bottom :autoclose t :select nil :size 0.4)
        ("*Compile-Log*" :dedicated t :align bottom :autoclose t :select nil :size 0.4)
        ("*Warnings*" :dedicated t :align bottom :autoclose t :select nil)
        ("*Completions*" :dedicated t :align bottom :autoclose t :select nil)
        ("*Shell Command Output*" :dedicated t :align bottom :autoclose t :select nil)
        ("\\*Async Shell Command\\*.+" :regexp t :align bottom :autoclose t :select nil)
        ("^*Man.+*$" :regexp t :align bottom :autoclose nil :select t :size 0.4)
        ("^*WoMan.+*$" :regexp t :align bottom :select t :size 0.4 :autoclose t)
        ("^*Backtrace.+*$" :regexp t :dedicated t :align bottom :autoclose t :select nil)

        ;; Kill Ring
        ("*Kill Ring*" :dedicated t :align bottom :autoclose t)

        ;; Flycheck
        ("\\*flycheck errors\\*.+*$" :regexp t :align bottom :autoclose t :select nil)

        ;; Youdao dict
        ("*Youdao Dictionary*" :dedicated t :align bottom :autoclose t)

        ;; Google translate
        ("*Google Translate*" :dedicated t :align bottom :size 0.4 :autoclose t)

        ;; Moedict
        ("*[萌典] 查詢結果*" :dedicated t :align bottom :autoclose t)

        ;; Paradox
        ("*Paradox Report*" :dedicated t :align bottom :select nil :autoclose t)

        ;; Diff
        ("*Diff*" :dedicated t :align bottom :select nil :autoclose t)

        ;; List
        ("*Colors*" :dedicated t :align bottom :autoclose t)
        ("*Process List*" :dedicated t :align bottom :autoclose t)
        ("*Process-Environment*" :dedicated t :align bottom :autoclose t)

        ;; undo-tree
        (" *undo-tree*" :dedicated t :align right :autoclose t :select nil :size 60)

        ;; Search
        ("*grep*" :dedicated t :align bottom :autoclose t :select nil)
        ("*ag search*" :dedicated t :align bottom :autoclose t :select t :size 0.4)
        ("*rg*" :dedicated t :align bottom :autoclose t :select t :size 0.4)
        ("*pt-search*" :dedicated t :align bottom :autoclose t :select t :size 0.4)
        ("*Occur*" :dedicated t :align bottom :autoclose t :select t)
        ("\\*ivy-occur.+*$" :regexp t :align bottom :autoclose t :select nil)
        ("*xref*" :dedicated t :align bottom :autoclose t :select t)

        ;; VC
        ("*vc-diff*" :dedicated t :align bottom :autoclose t :select nil)
        ("*vc-change-log*" :dedicated t :align bottom :autoclose t :select nil)

        ;; Magit
        (magit-status-mode :dedicated t :align bottom :autoclose t :select t :size 0.5)
        (magit-diff-mode :dedicated t :align bottom :autoclose t :select t :size 0.5)

        ;; Script
        ("*shell*" :dedicated t :align bottom :autoclose t :select t)
        ("*Python*" :dedicated t :align bottom :autoclose t :select t)
        ("*Ruby*" :dedicated t :align bottom :autoclose t :select t)
        ("*quickrun*" :dedicated t :align bottom :autoclose t :select t)

        ;; Go
        ("^*godoc.+*$" :regexp t :align bottom :autoclose nil :select nil)
        ("*golint*" :dedicated t :align bottom :autoclose t :select nil)
        ("*govet*" :dedicated t :align bottom :autoclose t :select nil)
        ("*go-guru-output*" :dedicated t :align bottom :autoclose t :select nil)
        ("*Gofmt Errors*" :dedicated t :align bottom :autoclose t :select nil)
        ("*Go Test*" :dedicated t :align bottom :autoclose t :select nil)

        ;; Test
        ("*ert*" :dedicated t :align bottom :autoclose t :select nil)
        ("*nosetests*" :dedicated t :align bottom :autoclose t :select nil)

        ;; Entropy refer
        ("^\\*entropy/cpmv" :dedicated t :regexp t :align bottom :autoclose nil :select nil)
        ("^\\*entropy/cndt" :dedicated t :regexp t :align bottom :autoclose nil :select nil)
        ("^\\*entropy/sdcv" :dedicated t :regexp t :align bottom :autoclose nil :select nil)

        ;; sbcl-mode
        ("^\\*slime-" :regexp t :autoclose t :align bottom :select nil :size 0.4)
        ("^\\*sldb" :regexp t :autoclose t :align bottom :select nil :size 0.4)
        ))


;; ** popwin-mode
(use-package popwin
  :if (eq entropy/emacs-use-popup-window-framework 'popwin)
  :defines (popwin:keymap)
  :commands (popwin-mode
             popwin:messages
             popwin:find-file
             popwin:display-buffer)

  :eemacs-indhc
  (((:enable t)
    (popwin-dispatch))
   ("Popwin popuping "
    (("p o" popwin:display-buffer "Popup for buffers"
      :enable t :exit t :eemacs-top-bind t)
     ("p f" popwin:find-file "Popup for files"
      :enable t :exit t :eemacs-top-bind t)
     ("p e" popwin:messages "Popup message buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("p l" popwin:popup-last-buffer "Popup last popuped buffer"
      :enable t :exit t :eemacs-top-bind t))))
  :eemacs-tpha
  (((:enable t))
   ("WI&BUF"
    (("p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'popwin-dispatch))
      "Popup window or buffer"
      :enable t :exit t))))

  :init

  (entropy/emacs-lazy-with-load-trail
   popwin-mode
   (popwin-mode t))

  :config
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        (entropy/emacs-popwin-make-rule-spec
         :popwin entropy/emacs-popwin-regists)))

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

  :eemacs-indhc
  (((:enable t)
    (shackle-dispatch))
   ("Shackle popuping"
    (("p o" shackle-popup-buffer "Popup for buffers"
      :enable t :exit t :eemacs-top-bind t)
     ("p f" shackle-popup-find-file "Popup for files"
      :enable t :exit t :eemacs-top-bind t)
     ("p e" shackle-popup-message "Popup message buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("p l" shackle-last-popup-buffer "Popup last popuped buffer"
      :enable t :exit t :eemacs-top-bind t))))
  :eemacs-tpha
  (((:enable t))
   ("WI&BUF"
    (("p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'shackle-dispatch))
      "Popup window or buffer"
      :enable t :exit t))))

  :init
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (entropy/emacs-lazy-with-load-trail
   shackle-mode
   (shackle-mode t))

  :config

  (defun shackle-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

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
           (shackle-rules `((,buff-name
                             :select t
                             :align 'below
                             :autoclose t
                             :size 0.3))))
      (with-current-buffer buff-name
        (unless (eobp)
          (goto-char (point-max))))
      (display-buffer buff-name)))

  ;; rules
  (setq shackle-default-size 0.4)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (setq shackle-rules
        (entropy/emacs-popwin-make-rule-spec
         :shackle entropy/emacs-popwin-regists)))

;; * provide
(provide 'entropy-emacs-popwin)
