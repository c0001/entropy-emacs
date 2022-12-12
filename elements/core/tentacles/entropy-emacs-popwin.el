;;; entropy-emacs-popwin.el --- window or buffer popuped feature for 'entropy-emacs'  -*- lexical-binding: t; -*-
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

;; ** library
(defvar entropy/emacs-popwin-union-rule-map)
(setq entropy/emacs-popwin-union-rule-map
      `((:regexp
         :all ,(lambda (x _rule) (list :regexp x)))
        (:select
         :shackle ,(lambda (x _rule) (list :select x)))
        (:align
         :shackle
         ,(lambda (x _rule)
            (list :align
                  (cl-case x
                    (bottom 'below)
                    (top 'above)
                    (t x)))))
        (:size
         :shackle ,(lambda (x _rule) (list :size x)))
        (:autoclose
         :shackle ,(lambda (x _rule) (list :autoclose x)))
        (:dedicated
         :shackle ,(lambda (x _rule) (list :popup (null x))))))

(defun entropy/emacs-popwin-make-rule-spec
    (type eemacs-popwin-rule-list)
  (let (rtn)
    (dolist (rule eemacs-popwin-rule-list)
      (let* ((condition (car rule))
             (attrs     (cdr rule))
             (maxlen    (length attrs))
             (rule-transed (list condition))
             (cnt 0))
        (while (<= cnt (- maxlen 2))
          (let* ((slot (nth cnt attrs))
                 (slot-value (nth (+ 1 cnt) attrs))
                 (mth (alist-get slot entropy/emacs-popwin-union-rule-map))
                 (trans-func (or (plist-get mth type) (plist-get mth :all))))
            (when trans-func
              (entropy/emacs-nconc-with-setvar-use-rest rule-transed
                (funcall trans-func slot-value rule)))
            (setq cnt (+ 2 cnt))))
        (entropy/emacs-nconc-with-setvar-use-rest rtn
          (list rule-transed))))
    ;; return
    rtn))

(defvar entropy/emacs-popwin-regists)
(setq entropy/emacs-popwin-regists
      `(;; Emacs
        ("*Help*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Messages*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*compilation*"               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        (,byte-compile-log-buffer      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Warnings*"                  :regexp nil :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)
        ("*Completions*"               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Shell Command Output*"      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("\\*Async Shell Command\\*.+" :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("^\\*Man.+\\*$"               :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^\\*WoMan.+\\*$"             :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^\\*Backtrace.*\\*$"         :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^\\*eldoc"                   :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Kill Ring
        ("*Kill Ring*"                 :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Company mode
        ("\\*company-documentation"    :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select t)
        ("\\*company-en-words"         :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select t)

        ;; Youdao dict
        ("*Youdao Dictionary*"         :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Google translate
        ("*Google Translate*"          :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Moedict
        ("*[萌典] 查詢結果*"           :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Paradox
        ("*Paradox Report*"            :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Diff
        ("*Diff*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; List
        ("*Colors*"                    :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Process List*"              :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Process-Environment*"       :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Search
        ("*grep*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*ag search*"                 :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*rg*"                        :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*pt-search*"                 :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Occur*"                     :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("\\*ivy-occur"                :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*xref*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Flymake
        ("\\*Flymake diagnostics for"  :regexp t :dedicated t :align bottom :size 0.2 :autoclose t     :select t)
        ;; Flycheck
        ("\\*flycheck errors\\*.+*$"   :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)
        ("^\\*Flycheck errors\\*$"     :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)

        ;; Lsp
        ("*lsp-help*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; VC
        ("*vc-diff*"                   :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*vc-change-log*"             :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Magit
        (magit-status-mode             :regexp nil :dedicated t :align bottom :size 0.5 :autoclose t   :select t)
        (magit-diff-mode               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        (magit-revision-mode           :regexp nil :dedicated t :align bottom :size 0.5 :autoclose t   :select t)

        ;; Script
        ("*shell*"                     :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Python*"                    :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Ruby*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*quickrun*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Go
        ("^\\*godoc.+\\*$"             :regexp t   :dedicated t :align bottom :size 0.4 :autoclose nil :select nil)
        ("*golint*"                    :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*govet*"                     :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*go-guru-output*"            :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Gofmt Errors*"              :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Go Test*"                   :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Test
        ("*ert*"                       :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*nosetests*"                 :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Entropy refer
        ("^\\*entropy/cpmv"            :regexp t   :dedicated t :align bottom :size 0.4 :autoclose nil :select nil)
        ("^\\*entropy/cndt"            :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^\\*entropy/sdcv"            :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^ *\\*eemacs"                :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; sbcl-mode
        ("^\\*slime-"                  :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*inferior-lisp*"             :regexp nil :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)
        ("^\\*sldb"                    :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Bongo
        ("^\\*Bongo"                   :regexp t   :dedicated t :align left :size 0.5   :autoclose t   :select t)

        ;; Msic.
        ("*Buffer Details*"            :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Memory Explorer*"           :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^ \\*Marked Files\\*$"       :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("^\\*Error\\*$"               :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)
        ("^\\*macro expansion\\*"      :regexp t   :dedicated t :align bottom :size 0.5 :autoclose t   :select t)
        ("*Calendar*"                  :regexp nil :dedicated t :align bottom :size 0.3 :autoclose nil :select t)

        ;; proc buffers
        ;;  --- emacs async ---
        ("^\\*emacs\\(<[0-9]+>\\)?\\*" :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ;; --- others ---
        ("^ ?\\*openwith-process\\*"   :regexp t   :dedicated t :align bottom :size 0.2 :autoclose t   :select nil)
        ))

;; ** shackle mode
(use-package shackle
  :commands (shackle-mode
             shackle-display-buffer
             entropy/emacs-popwin-shackle-popup-buffer
             entropy/emacs-popwin-shackle-popup-find-file)

;; *** preface
  :preface
  (defvar shackle-popup-mode-map
    (let ((map (make-sparse-keymap)))
      map))
  (defvar entropy/emacs-popwin--shackle/beacon-blink-ignore nil)

;; *** keybindings
  :eemacs-indhc
  (((:enable t :defer t)
    (shackle-dispatch))
   ("Shackle popuping"
    (("p o" entropy/emacs-popwin-shackle-popup-buffer "Popup for buffers"
      :enable t :exit t :eemacs-top-bind t)
     ("p f" entropy/emacs-popwin-shackle-popup-find-file "Popup for files"
      :enable t :exit t :eemacs-top-bind t)
     ("p d" entropy/emacs-popwin-shackle-popup-dired "Popup for dired"
      :enable t :exit t :eemacs-top-bind t)
     ("p e" entropy/emacs-popwin-shackle-popup-message "Popup message buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("p l" entropy/emacs-popwin-shackle-show-last-popup-buffer "Popup last popuped buffer"
      :enable t :exit t :eemacs-top-bind t))))
  :eemacs-tpha
  (((:enable t :defer t))
   ("WI&BUF"
    (("p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'shackle-dispatch))
      "Popup window or buffer"
      :enable t :exit t))))

;; *** init
  :init
  (entropy/emacs-lazy-with-load-trail
    'shackle-mode
    (shackle-mode t))

;; *** config
  :config

;; **** autoclose hack

  ;; history of list of shackle popuped object formed as (buffer . buffer-window)
  (defvar entropy/emacs-popwin--shackle-popup-display-history nil)
  ;; the local variable which indicate that `current-buffer' is a
  ;; displayed by shackle when non-nil
  (entropy/emacs-defvar-local-with-pml
   entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p nil)

  (defun entropy/emacs-popwin--shackle-set-parameters-for-popup-buffer
      (buff &optional orig-window)
    (let* ((window (get-buffer-window buff))
           (orig-window (or orig-window
                            (window-parameter
                             window
                             'entropy/emacs-popwin--shackle-origin-selected-window))))
      (with-current-buffer buff
        (setq-local
         entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
         t))
      (set-window-parameter window
                            'entropy/emacs-popwin--shackle-window-is-popup-window-p
                            t)
      (set-window-parameter window
                            'entropy/emacs-popwin--shackle-origin-selected-window
                            (or
                             (and (and (windowp orig-window)
                                       (window-live-p orig-window))
                                  (and (not (eq window orig-window))
                                       orig-window))
                             (and (not (eq (selected-window) window))
                                  (selected-window))))))

;; ***** workspace switch persist for `entropy/emacs-popwin--shackle-popup-display-history'

  (defvar entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache nil)

  (defun entropy/emacs-popwin--shackle-workspace-switch-pre-hook (type)
    (let ((ecwg-slot (cond ((eq type 'eyebrowse)
                            (intern (format "eyebrowse-slot/at-%s"
                                            (eyebrowse--get 'current-slot))))
                           (t
                            (error "not supported workspace type %s" type)))))
      (setq entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache
            (assq-delete-all ecwg-slot
                             entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache))
      (push (cons ecwg-slot entropy/emacs-popwin--shackle-popup-display-history)
            entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache)))

  (defun entropy/emacs-popwin--shackle-workspace-switch-post-hook (type)
    (let ((ecwg-slot (cond ((eq type 'eyebrowse)
                            (intern (format "eyebrowse-slot/at-%s"
                                            (eyebrowse--get 'current-slot))))
                           (t
                            (error "not supported workspace type %s" type))))
          new-hist)
      (setq entropy/emacs-popwin--shackle-popup-display-history
            (alist-get ecwg-slot
                       entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache))
      (setq entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache
            (assq-delete-all ecwg-slot
                             entropy/emacs-popwin--shackle-autoclose-worksapce-group-cache))
      ;; update `entropy/emacs-popwin--shackle-popup-display-history'
      ;; proper to current workspace status since some buffer may be
      ;; killed while user operations in other workspace.
      (when entropy/emacs-popwin--shackle-popup-display-history
        (dolist (el entropy/emacs-popwin--shackle-popup-display-history)
          (let* ((buff (car el))
                 (win (get-buffer-window buff)))
            (when (and (buffer-live-p buff)
                       (window-live-p win))
              (entropy/emacs-popwin--shackle-set-parameters-for-popup-buffer
               buff (window-parameter
                     win
                     'entropy/emacs-popwin--shackle-origin-selected-window))
              (push (cons buff win) new-hist))))
        (setq entropy/emacs-popwin--shackle-popup-display-history
              new-hist))))

;; ****** eyebrowse

  (defun entropy/emacs-popwin--shackle-eyebrowse-switch-pre-hook ()
    (entropy/emacs-popwin--shackle-workspace-switch-pre-hook 'eyebrowse))
  (defun entropy/emacs-popwin--shackle-eyebrowse-switch-post-hook ()
    (entropy/emacs-popwin--shackle-workspace-switch-post-hook 'eyebrowse))

  (with-eval-after-load 'eyebrowse
    (add-hook 'eyebrowse-pre-window-switch-hook
              'entropy/emacs-popwin--shackle-eyebrowse-switch-pre-hook)
    (add-hook 'eyebrowse-post-window-switch-hook
              'entropy/emacs-popwin--shackle-eyebrowse-switch-post-hook))

;; ***** window&buffer indicator hack

  (defun entropy/emacs-popwin--shackle-display-buffer-hack
      (fn buffer alist plist)
    "Patch to `shackle-display-buffer' for =entropy-emacs=
specification."
    (let ((buff-lp (get-buffer-window buffer))
          (orig-window (selected-window))
          (window (funcall fn buffer alist plist)))
      (with-current-buffer buffer
        ;; Do not recognize displayed buffer as popuped buffer.
        (unless buff-lp
          (when (plist-get plist :autoclose)
            (entropy/emacs-popwin--shackle-set-parameters-for-popup-buffer
             buffer orig-window)
            (push (cons buffer window)
                  entropy/emacs-popwin--shackle-popup-display-history))))
      window))
  (advice-add #'shackle-display-buffer
              :around #'entropy/emacs-popwin--shackle-display-buffer-hack)

;; ***** window&buffer indicator unhack
  (defun entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
      (buffer-or-window &optional non_recursive)
    (let (_)
      (cond
       ((windowp buffer-or-window)
        (setq entropy/emacs-popwin--shackle-popup-display-history
              (rassq-delete-all
               buffer-or-window
               entropy/emacs-popwin--shackle-popup-display-history))

        ;; we should reset the parameter of whatever live or deleted
        ;; window since the deleted window may be reactivated later.
        (set-window-parameter buffer-or-window
                              'entropy/emacs-popwin--shackle-window-is-popup-window-p
                              nil)
        (set-window-parameter buffer-or-window
                              'entropy/emacs-popwin--shackle-origin-selected-window
                              nil)
        (when (window-live-p buffer-or-window)
          (unless non_recursive
            (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
             (window-buffer buffer-or-window) t))))
       ((bufferp buffer-or-window)
        (setq entropy/emacs-popwin--shackle-popup-display-history
              (assq-delete-all
               buffer-or-window
               entropy/emacs-popwin--shackle-popup-display-history))
        (when (buffer-live-p buffer-or-window)
          (with-current-buffer buffer-or-window
            (setq-local entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
                        nil))
          (unless non_recursive
            (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
             (get-buffer-window buffer-or-window) t)))))))

  (defun entropy/emacs-popwin--shackle-prunning-popup-history ()
    (let (new-hist)
      (dolist (el entropy/emacs-popwin--shackle-popup-display-history)
        (let* ((buff (car el))
               (buff-regist-win (cdr el))
               (buff-cur-win (and (bufferp buff) (get-buffer-window buff))))
          (when
              ;; we just preserved the alive buffer record
              (buffer-live-p buff)
            (if
                ;; in which case the record is efficient already
                (eq buff-regist-win buff-cur-win)
                (entropy/emacs-nconc-with-setvar-use-rest new-hist (list el))
              ;; the otherwise, the record is broken
              (if
                  ;; if the recorded buffer is still displayed as
                  ;; also, we fix its record with its new window
                  buff-cur-win
                  (progn
                    (entropy/emacs-nconc-with-setvar-use-rest new-hist
                      (list (cons buff buff-cur-win)))
                    (entropy/emacs-popwin--shackle-set-parameters-for-popup-buffer
                     buff (window-parameter
                           buff-cur-win
                           'entropy/emacs-popwin--shackle-origin-selected-window))
                    (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                     buff-regist-win t))
                ;; otherwise, we replace the buffer of the record if
                ;; its stick window displayed other buffer.
                (progn
                  (if (window-live-p buff-regist-win)
                      (progn
                        (entropy/emacs-nconc-with-setvar-use-rest new-hist
                          (list (cons (window-buffer buff-regist-win)
                                      buff-regist-win)))
                        (entropy/emacs-popwin--shackle-set-parameters-for-popup-buffer
                         (window-buffer buff-regist-win)
                         (window-parameter
                          buff-regist-win
                          'entropy/emacs-popwin--shackle-origin-selected-window))
                        (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                         buff t))
                    (progn
                      (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                       buff t)
                      ;; unhack the deletd window since its may reactivated later.
                      (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                       buff-regist-win t)))))))))
      (setq entropy/emacs-popwin--shackle-popup-display-history
            new-hist)))

;; ***** buffer popup-p judger
  (defun entropy/emacs-popwin--buffer-is-shackle-popup-p (buffer &optional strict)
    (let ((buf-win (get-buffer-window buffer)))
      (if (and (windowp buf-win)
               (eq buf-win (window-main-window (selected-frame))))
          ;; If the buffer window is the root window of
          ;; `selected-frame' then we rebind it, this may happend when
          ;; the window buffer changed to the popuped buffer or the
          ;; buffer switched to popuped window which has been the root
          ;; window of `selected-frame'.
          (progn
            (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
             buffer t)
            nil)
        (ignore-errors
          (eval
           `(,(if strict 'and 'or)
             (buffer-local-value
              'entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
              ',buffer)
             (window-parameter
              ',buf-win
              'entropy/emacs-popwin--shackle-window-is-popup-window-p)))))))

;; ***** Adjust other eemacs operations
  ;; ignore the window parameters restriction while current buffer is
  ;; an shackle popuped buffer since we temporally needed to maximize
  ;; it in most of cases.
  (add-to-list 'entropy/emacs-delete-other-windows-ignore-pms-predicates
               '(entropy/emacs-popwin--buffer-is-shackle-popup-p
                 (window-buffer)))

  ;; unhack the current window and buffer while maxmize it since we do
  ;; not want to let it be autoclose any more.
  (defun entropy/emacs-popwin--shackle-popup-buffers/current-buffer&window-unhack-when-maxmized
      (&rest _)
    (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
     (window-buffer)))
  (add-hook 'entropy/emacs-delete-other-windows-before-hook
            #'entropy/emacs-popwin--shackle-popup-buffers/current-buffer&window-unhack-when-maxmized)

  ;; do not show baecon blink when popuped buffer exists for visual
  ;; experience adjustments
  (defun entropy/emacs-popwin--shackle-popup-buffers-exist-then-ignore-beacon-blink
      ()
    (or entropy/emacs-popwin--shackle/beacon-blink-ignore
        (progn
          (entropy/emacs-popwin--shackle-prunning-popup-history)
          (not
           (null
            entropy/emacs-popwin--shackle-popup-display-history)))))
  (add-hook 'beacon-dont-blink-predicates
            #'entropy/emacs-popwin--shackle-popup-buffers-exist-then-ignore-beacon-blink)

;; ***** autoclose `\C-g' bindings
  (defun entropy/emacs-popwin--shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (entropy/emacs-popwin--shackle-prunning-popup-history)
    ;; main filter
    (let (close-done stick-buffer stick-window)
      (when (and ;; (called-interactively-p 'interactive)
                 (not (region-active-p))
                 (not (minibufferp)))
        (let (
              ;; Suppress `beacon-blink' to prevent from activating
              ;; region where effects next operation of the condition
              ;; part of this function.
              (entropy/emacs-popwin--shackle/beacon-blink-ignore t))
          (cond
           ((one-window-p)
            (setq stick-buffer (window-buffer)
                  stick-window (get-buffer-window stick-buffer))
            (when (entropy/emacs-popwin--buffer-is-shackle-popup-p stick-buffer)
              (message "Auto hide popups in <one-window type> (in window %S) ..."
                       stick-window)
              (dolist (el (list stick-buffer stick-window))
                (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                 el))
              (when (and (bound-and-true-p winner-mode)
                         (window-live-p stick-window))
                (winner-undo))))

           ((entropy/emacs-popwin--buffer-is-shackle-popup-p
             (window-buffer)
             ;; ;; WHY?:
             ;; ;; Just recognize the buffer is shackle-popuped while its
             ;; ;; window is marked either for preventing messy judging
             ;; ;; while some internal unknown window display tricks like
             ;; ;; window reuse etc.
             ;; t
             )
            (setq stick-buffer (window-buffer)
                  stick-window (get-buffer-window stick-buffer))
            (message "Auto hiding popups in <buffer-local type> (in buffer window %S) ..."
                     stick-window)
            (let ((window-prev
                   (and (windowp stick-window)
                        (window-parameter
                         stick-window
                         'entropy/emacs-popwin--shackle-origin-selected-window))))
              (dolist (el (list stick-buffer stick-window))
                (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                 el))
              (when (window-live-p stick-window)
                (delete-window stick-window))
              (when (ignore-errors (window-live-p window-prev))
                (select-window window-prev))))

           ((not (one-window-p))
            (let ((host-buffer (window-buffer))
                  window-refer)
              (setq stick-buffer (caar entropy/emacs-popwin--shackle-popup-display-history)
                    stick-window (ignore-errors (get-buffer-window stick-buffer))
                    window-refer (cdar entropy/emacs-popwin--shackle-popup-display-history))
              (dolist (el (list stick-buffer stick-window window-refer))
                (entropy/emacs-popwin--shackle-unhack-for-buffer-or-window
                 el))
              (when (ignore-errors (buffer-live-p stick-buffer))
                (when (ignore-errors (window-live-p stick-window))
                  (message
                   "Auto hiding popups in <multi-window type> (based on window %S) ..."
                   (selected-window))
                  (delete-window stick-window)
                  (setq close-done t)))
              ;; ;; recenter the `host-buffer' window but it's optional so we commented it
              (when close-done
                (with-selected-window (get-buffer-window host-buffer)
                  ;; (recenter-top-bottom '(middle))
                  ))
              )))
          ))
      ))

  (add-hook 'entropy/emacs-keyboard-quit-before-hook
            #'entropy/emacs-popwin--shackle-close-popup-window-hack)

;; **** further more pop actions

  (defun entropy/emacs-popwin-shackle-show-last-popup-buffer ()
    "View last popup buffer."
    (declare (interactive-only t))
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

  (defun entropy/emacs-popwin-shackle-popup-buffer-action (buff-name)
    (let* ((shackle-rules
            (or (and (ignore-errors (shackle-match buff-name)) shackle-rules)
                `((,buff-name :select t :size 0.4 :align below :autoclose t)))))
      (get-buffer-create buff-name)
      (display-buffer buff-name)))
  (defun entropy/emacs-popwin-shackle-popup-buffer ()
    "Display buffer with popuped behaviour powered by `shackle'."
    (declare (interactive-only t))
    (interactive)
    (let ((prompt "Buffer choosing: ")
          (table 'internal-complete-buffer)
          (action #'entropy/emacs-popwin-shackle-popup-buffer-action))
      (cond
       ((eq entropy/emacs-command-completion-use-style 'ivy)
        (ivy-read prompt table
                  :require-match nil
                  :action action
                  :caller 'entropy/emacs-popwin-shackle-popup-buffer))
       (t
        (let* ((buff-name (completing-read prompt table)))
          (funcall action buff-name))))))

  (defun entropy/emacs-popwin-shackle-popup-find-file-action (file)
    (let* ((buff-name (buffer-name (find-file-noselect file)))
           (shackle-rules
            (or (and (ignore-errors (shackle-match buff-name)) shackle-rules)
                `((,buff-name :select t :size 0.4 :align below :autoclose t)))))
      (display-buffer buff-name)))
  (defun entropy/emacs-popwin-shackle-popup-find-file ()
    "Find file with popup window powered by `shackle'."
    (declare (interactive-only t))
    (interactive)
    (let* ((prompt "File choosing: ")
           (table 'read-file-name-internal)
           (history 'file-name-history)
           (action #'entropy/emacs-popwin-shackle-popup-find-file-action))
      (cond
       ((eq entropy/emacs-command-completion-use-style 'ivy)
        (ivy-read prompt table
                  :require-match nil
                  :action action
                  :caller 'entropy/emacs-popwin-shackle-popup-find-file
                  :history history))
       (t
        (let ((file (completing-read
                     prompt table
                     nil nil nil history)))
          (funcall action file))))))

  (defun entropy/emacs-popwin-shackle-popup-dired-action (dir)
    (let* ((buff-name (buffer-name (dired-noselect dir)))
           (shackle-rules
            (or (and (ignore-errors (shackle-match buff-name)) shackle-rules)
                `((,buff-name :select t :size 0.4 :align below :autoclose t)))))
      (display-buffer buff-name)))
  (defun entropy/emacs-popwin-shackle-popup-dired ()
    "Dired with popup window powered by `shackle'."
    (declare (interactive-only t))
    (interactive)
    (let* ((prompt "Dired choosing: ")
           (table 'read-file-name-internal)
           (predicate 'file-directory-p)
           (history 'file-name-history)
           (action #'entropy/emacs-popwin-shackle-popup-dired-action))
      (cond
       ((eq entropy/emacs-command-completion-use-style 'ivy)
        (ivy-read prompt table
                  :require-match nil
                  :action action
                  :predicate predicate
                  :caller 'entropy/emacs-popwin-shackle-popup-dired
                  :history history))
       (t
        (let ((dir (completing-read prompt table
                                    predicate nil nil
                                    history)))
          (funcall action dir))))))

  (defun entropy/emacs-popwin-shackle-popup-message ()
    "Display message buffer with popup type powerd by `shackle'."
    (declare (interactive-only t))
    (interactive)
    (let* ((buff-name (buffer-name (get-buffer-create "*Messages*")))
           (shackle-rules `((,buff-name
                             :select t
                             :align below
                             :autoclose t
                             :size 0.3))))
      (with-current-buffer buff-name
        (unless (eobp)
          (goto-char (point-max))))
      (display-buffer buff-name)))

  ;; Enable ivy mode before calling these actions since the ivy-mode
  ;; maybe delayed to enable
  (dolist (func '(
                  entropy/emacs-popwin-shackle-popup-buffer
                  entropy/emacs-popwin-shackle-popup-dired
                  entropy/emacs-popwin-shackle-popup-find-file
                  entropy/emacs-popwin-shackle-popup-message
                  ))
    (advice-add func
                :before
                (lambda (&rest _)
                  "Enable some features before eemacs popup operations startup"
                  (when (and entropy/emacs-use-recentf
                             (fboundp 'recentf-mode))
                    (unless (bound-and-true-p recentf-mode)
                      (recentf-mode)))
                  (when (fboundp 'savehist-mode)
                    (unless (bound-and-true-p savehist-mode)
                      (savehist-mode)))
                  (unless (bound-and-true-p ivy-mode)
                    (when (eq entropy/emacs-command-completion-use-style 'ivy)
                      (ivy-mode t))))))

;; **** reset rule action

  (defun entropy/emacs-popwin--shackle-set-rule ()
    (setq shackle-rules
          (entropy/emacs-popwin-make-rule-spec
           :shackle entropy/emacs-popwin-regists)))

  (defun entropy/emacs-popwin-shackle-reset-rules ()
    "Reset `shackle-rules' and enable them."
    (interactive)
    (let ((init-status shackle-mode))
      (when init-status
        (shackle-mode 0))
      (entropy/emacs-popwin--shackle-set-rule)
      (when init-status
        (shackle-mode 1))))

  ;; rules
  (setq shackle-default-size 0.4)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (entropy/emacs-popwin--shackle-set-rule))

;; * provide
(provide 'entropy-emacs-popwin)
