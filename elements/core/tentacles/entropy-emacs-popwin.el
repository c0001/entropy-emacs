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
        ("*Help*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Messages*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*compilation*"               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Compile-Log*"               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Warnings*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Completions*"               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("*Shell Command Output*"      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("\\*Async Shell Command\\*.+" :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("^*Man.+*$"                   :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^*WoMan.+*$"                 :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("^*Backtrace.+*$"             :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Kill Ring
        ("*Kill Ring*"                 :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Flycheck
        ("\\*flycheck errors\\*.+*$"   :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Youdao dict
        ("*Youdao Dictionary*"         :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Google translate
        ("*Google Translate*"          :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Moedict
        ("*[萌典] 查詢結果*"           :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Paradox
        ("*Paradox Report*"            :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Diff
        ("*Diff*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

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
        ("\\*ivy-occur.+*$"            :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*xref*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; VC
        ("*vc-diff*"                   :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*vc-change-log*"             :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Magit
        (magit-status-mode             :regexp nil :dedicated t :align bottom :size 0.5 :autoclose t   :select t)
        (magit-diff-mode               :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)

        ;; Script
        ("*shell*"                     :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Python*"                    :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*Ruby*"                      :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)
        ("*quickrun*"                  :regexp nil :dedicated t :align bottom :size 0.4 :autoclose t   :select t)

        ;; Go
        ("^*godoc.+*$"                 :regexp t   :dedicated t :align bottom :size 0.4 :autoclose nil :select nil)
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

        ;; sbcl-mode
        ("^\\*slime-"                  :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ("^\\*sldb"                    :regexp t   :dedicated t :align bottom :size 0.4 :autoclose t   :select nil)
        ))


;; ** popwin-mode
(use-package popwin
  :if (eq entropy/emacs-use-popup-window-framework 'popwin)
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
             entropy/emacs-popwin-shackle-popup-buffer
             entropy/emacs-popwin-shackle-popup-find-file)
  :preface
  (defvar shackle-popup-mode-map
    (let ((map (make-sparse-keymap)))
      map))

  :eemacs-indhc
  (((:enable t)
    (shackle-dispatch))
   ("Shackle popuping"
    (("p o" entropy/emacs-popwin-shackle-popup-buffer "Popup for buffers"
      :enable t :exit t :eemacs-top-bind t)
     ("p f" entropy/emacs-popwin-shackle-popup-find-file "Popup for files"
      :enable t :exit t :eemacs-top-bind t)
     ("p e" entropy/emacs-popwin-shackle-popup-message "Popup message buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("p l" entropy/emacs-popwin-shackle-show-last-popup-buffer "Popup last popuped buffer"
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
  (entropy/emacs-lazy-with-load-trail
   shackle-mode
   (shackle-mode t))

  :config
  (defvar entropy/emacs-popwin--shackle-popup-buffer-history nil) ; all popup windows
  (defvar-local entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p nil) ; current popup window
  (put 'entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p 'permanent-local t)

  (defun entropy/emacs-popwin--shackle-display-buffer-hack
      (fn buffer alist plist)
    "Patch to `shackle-display-buffer' for adding ':autoclose'
key slot support."
    (let ((buff-lp (get-buffer-window buffer))
          (window (funcall fn buffer alist plist)))
      (with-current-buffer buffer
        ;; Do not recognize displaed buffer as popuped buffer.
        (unless buff-lp
          (setq-local
           entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
           t)
          (when (plist-get plist :autoclose)
            (add-to-list 'entropy/emacs-popwin--shackle-popup-buffer-history
                         buffer))))
      window))

  (defun entropy/emacs-popwin--shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    ;; pruning origin history list
    (setq entropy/emacs-popwin--shackle-popup-buffer-history
          (cl-loop for buffer in entropy/emacs-popwin--shackle-popup-buffer-history
                   if (window-live-p (get-buffer-window buffer))
                   collect buffer))
    ;; main filter
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               (not (active-minibuffer-window)))
      (let ((host-buffer (current-buffer)) buffer)
        (cond
         ((one-window-p)
          (message "Auto hiding popuped buffer ...")
          (progn (setq buffer (current-buffer))
                 (when  (buffer-local-value
                         'entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
                         buffer)
                   (winner-undo))))
         ((buffer-local-value
           'entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
           (current-buffer))
          (message "Auto hiding popuped buffer ...")
          (setq buffer (current-buffer))
          (delete-window))
         ((not (one-window-p))
          (setq buffer (car entropy/emacs-popwin--shackle-popup-buffer-history))
          (when (buffer-live-p buffer)
            (unless (eq buffer host-buffer)
              (let ((window (get-buffer-window buffer)))
                (when (window-live-p window)
                  (delete-window window))
                (with-selected-window (get-buffer-window host-buffer)
                  (recenter-top-bottom '(middle))))))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local entropy/emacs-popwin--shackle-buffer-is-popup-buffer-p
                        nil)))
        (setq entropy/emacs-popwin--shackle-popup-buffer-history
              (delete buffer entropy/emacs-popwin--shackle-popup-buffer-history)))))

  (advice-add #'keyboard-quit
              :before #'entropy/emacs-popwin--shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer
              :around #'entropy/emacs-popwin--shackle-display-buffer-hack)

  (defun entropy/emacs-popwin-shackle-show-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

  (defun entropy/emacs-popwin-shackle-popup-buffer ()
    (interactive)
    (let* ((buff-name (completing-read "Buffer choosing: " 'internal-complete-buffer))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (get-buffer-create buff-name)
      (display-buffer buff-name)
      (when (and (fboundp 'solaire-mode)
                 (entropy/emacs-theme-adapted-to-solaire))
        (with-current-buffer buff-name
          (solaire-mode +1)))))

  (defun entropy/emacs-popwin-shackle-popup-find-file ()
    (interactive)
    (let* ((file (completing-read "Buffer choosing: " 'read-file-name-internal))
           (buff-name (buffer-name (find-file-noselect file)))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (display-buffer buff-name)))

  (defun entropy/emacs-popwin-shackle-popup-message ()
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
