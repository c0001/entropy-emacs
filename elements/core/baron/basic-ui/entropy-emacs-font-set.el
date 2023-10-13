;;; entropy-emacs-font.el --- entropy-emacs font configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:
;; Package-Version: https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-font-set.el
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
;; `entropy-emacs' archive sets of pre-rejecting fonts family [[https://sourceforge.net/projects/entropy-emax64/files/entropy-emacs_hitch/][here]],
;; thus this file was the configuration for those fonts collection
;; used for `entropy-emacs' page render.
;;
;; According to language scripts respectively, different font package
;; is used for, `entropy-emacs' divided the language scripts into
;; three categories, i.e. =latin=, =cjk=, =symbol=. For each of those
;; categories, different font-set is enabled for them respectively,
;; `entropy-emacs' using font both of open-source and user specific
;; copyright limited ones, as what for google&adobe's NOTO fonts and
;; the using scope resticted SYMBOLA font.
;;
;; The main font set function `entropy/emacs-font-set-setfont-core' is
;; wrappered for both font status checking and font spec setting
;; procedure.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-defvar)
(!eemacs-require 'entropy-emacs-defconst)
(!eemacs-require 'entropy-emacs-defun)

(defvar entropy/emacs-font-set--warn
  "Because you enabled `entropy/emacs-font-setting-enable' , but
you didn't install all fonts file in your system, so this warning
occurring. If you want to use your own font config please disable
it.

===== Missing fonts list: =====
")

(defun entropy/emacs-font-set--plist-get (font-group key)
  (or (plist-get font-group key)
      ;; `nil' for symbol spec is meaningful
      (and (not (eq key :symbol)) "Monospace")))

(defun entropy/emacs-font-set-register ()
  (when (eq entropy/emacs-font-setting-enable t)
    (setq entropy/emacs-font-setting-enable 'fira-code))
  (let ((group (alist-get entropy/emacs-font-setting-enable
                          entropy/emacs-fontsets-fonts-collection-alias)))
    (unless group
      (user-error "Invalid arg value for `entropy/emacs-font-setting-enable' to '%s'"
                  entropy/emacs-font-setting-enable))
    (setq entropy/emacs-fontsets-used-latin-font
          (entropy/emacs-font-set--plist-get group :latin)
          entropy/emacs-fontsets-used-cjk-sc-font
          (entropy/emacs-font-set--plist-get group :sc)
          entropy/emacs-fontsets-used-cjk-tc-font
          (entropy/emacs-font-set--plist-get group :tc)
          entropy/emacs-fontsets-used-cjk-jp-font
          (entropy/emacs-font-set--plist-get group :jp)
          entropy/emacs-fontsets-used-cjk-kr-font
          (entropy/emacs-font-set--plist-get group :kr)
          entropy/emacs-fontsets-used-symbol-font
          (entropy/emacs-font-set--plist-get group :symbol)
          entropy/emacs-fontsets-used-extra-fonts
          (plist-get group :extra))
    (setq entropy/emacs-default-cjk-cn-font
          (if (eq entropy/emacs-font-chinese-type 'sc)
              entropy/emacs-fontsets-used-cjk-sc-font
            entropy/emacs-fontsets-used-cjk-tc-font))))

(defun entropy/emacs-font-set--pre-fonts-check ()
  (entropy/emacs-font-set-register)
  (let (judge (prompt "")  (count 1))
    (dolist (font-name `(,entropy/emacs-fontsets-used-latin-font
                         ,entropy/emacs-default-cjk-cn-font
                         ,entropy/emacs-fontsets-used-cjk-jp-font
                         ,entropy/emacs-fontsets-used-cjk-kr-font
                         ,entropy/emacs-fontsets-used-symbol-font))
      (when (stringp font-name)
        (unless (find-font (font-spec :name font-name))
          (push font-name judge))))
    (when entropy/emacs-fontsets-used-extra-fonts
      (dolist (font-name entropy/emacs-fontsets-used-extra-fonts)
        (when (stringp font-name)
          (unless (find-font (font-spec :name font-name))
            (push font-name judge)))))
    (when judge
      (mapc (lambda (font-name)
              (setq prompt (concat prompt (number-to-string count) ": " font-name " missing.\n"))
              (cl-incf count))
            judge)
      (warn
       (format "%s\n%s" entropy/emacs-font-set--warn prompt)))
    judge))

(eval-when-compile
  ;; nox missing this var
  (unless (boundp 'use-default-font-for-symbols)
    (defvar use-default-font-for-symbols)))

(defun entropy/emacs-font-set-setfont-core (&optional frame)
  (interactive)
  ;; it's useless to setfont for a tui session and may cause buggy
  ;; like hang the new opened daemon client when injeting to
  ;; `entropy/emacs-daemon-server-after-make-frame-hook'.
  (when (display-graphic-p)
    (entropy/emacs-font-set-setfont-core-1 frame)))

(defun entropy/emacs-font-set-setfont-core-1 (&optional frame)
  (let ((after-do (and entropy/emacs-font-setting-enable
                       (plist-get (alist-get entropy/emacs-font-setting-enable
                                             entropy/emacs-fontsets-fonts-collection-alias)
                                  :after)))
        (frame (or frame (selected-frame))))
    (cond
     ((and entropy/emacs-font-setting-enable
           (display-graphic-p)
           (not (entropy/emacs-font-set--pre-fonts-check)))

      ;; preventing unicode cusor move lagging for
      ;; windows refer to mailing list
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-11/msg00471.html
      (when sys/win32p
        (setq inhibit-compacting-font-caches t))

      ;; Setting latin Font
      (set-fontset-font nil 'latin
                        (font-spec :family entropy/emacs-fontsets-used-latin-font)
                        frame)
      (set-face-attribute
       'variable-pitch
       frame
       :family entropy/emacs-fontsets-used-latin-font)

      ;; default interface font spec
      (when (eq frame entropy/emacs-main-frame)
        ;; NOTE: We only set those standard fontset in main frame
        ;; since it will affect all frames and further more affects
        ;; other frames currnt font size.
        (mapc (lambda (type)
                (set-fontset-font
                 type 'latin
                 (font-spec :family entropy/emacs-fontsets-used-latin-font)
                 frame))
              '("fontset-default" "fontset-startup" "fontset-standard")))

      ;;Setting cjk
      (dolist (charset '(han cjk-misc kanbun bopomofo))
        (set-fontset-font nil
                          charset
                          (font-spec
                           :family
                           entropy/emacs-default-cjk-cn-font)
                          frame))

      (set-fontset-font nil
                        '(?ぁ . ?ヶ)
                        (font-spec
                         :family
                         entropy/emacs-fontsets-used-cjk-jp-font)
                        frame)
      (set-fontset-font nil
                        'kana
                        (font-spec
                         :family
                         entropy/emacs-fontsets-used-cjk-jp-font)
                        frame)

      (set-fontset-font nil
                        'hangul
                        (font-spec :family entropy/emacs-fontsets-used-cjk-kr-font)
                        frame)

      (dolist (char '(?“ ?”))
        (set-fontset-font nil
                          char
                          (font-spec :family entropy/emacs-default-cjk-cn-font)
                          frame))

      ;; setting unicode symbol font
      (if entropy/emacs-fontsets-used-symbol-font
          (progn (setq use-default-font-for-symbols nil)
                 (set-fontset-font nil
                                   'symbol
                                   (font-spec :family entropy/emacs-fontsets-used-symbol-font)
                                   frame))
        (setq use-default-font-for-symbols t))

      ;; extra spec setting
      (when (and after-do (functionp after-do))
        (funcall after-do frame))

      ;; font size scaling
      (if (memq entropy/emacs-font-setting-enable '(sarasa maple))
          (setq face-font-rescale-alist nil)
        (setq face-font-rescale-alist
              `((,entropy/emacs-fontsets-used-cjk-sc-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-tc-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-jp-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-kr-font . 1.2))))

      (if (<= entropy/emacs-font-size-default 30)
          (let ((height (ceiling (* entropy/emacs-font-size-default 10))))
            ;; FIXME: remap height with unpredictable manually try but
            ;; stable for me for the sake for make 'Latin' an 'CJK'
            ;; can be strictly be 1:2 size linetype scale. Is there
            ;; any commonly way to approach to this?
            (setq height
                  (cond
                   ((and (<= height 110)
                         (> height 100))
                    108)
                   ((and (<= height 120)
                         (> height 110))
                    117)
                   ((and (<= height 130)
                         (> height 120))
                    124)
                   ((and (<= height 140)
                         (> height 130))
                    136)
                   ((and (<= height 150)
                         (> height 140))
                    148)
                   ((and (<= height 160)
                         (> height 150))
                    154)
                   ((and (<= height 170)
                         (> height 160))
                    168)
                   ((and (<= height 180)
                         (> height 170))
                    178)
                   ((and (<= height 190)
                         (> height 180))
                    182)
                   ((and (<= height 200)
                         (> height 190))
                    198)
                   ((and (<= height 210)
                         (> height 200))
                    208)
                   ((and (<= height 220)
                         (> height 210))
                    214)
                   ((and (<= height 230)
                         (> height 220))
                    228)
                   ((and (<= height 240)
                         (> height 230))
                    238)
                   ((and (<= height 250)
                         (> height 240))
                    258)
                   ((and (<= height 260)
                         (> height 250))
                    268)
                   ((and (<= height 270)
                         (> height 260))
                    288)
                   ((and (<= height 280)
                         (> height 270))
                    298)
                   ((and (<= height 290)
                         (> height 280))
                    318)
                   ((and (<= height 300)
                         (> height 290))
                    328)
                   (t
                    nil)))
            (when height
              (set-face-attribute 'default frame :height height)))
        (warn
         "Your default font size is too large,
you must set it smaller than 24 for adapting to other entropy-emacs settings.")))
     (t
      (let ((height (ceiling (* entropy/emacs-font-size-default 10))))
        (set-face-attribute 'default frame :height height))))
    ;; run font-set after hooks
    (run-hook-with-args 'entropy/emacs-font-set-end-hook frame)))

(defvar-local entropy/emacs-font-set--prog-local-remap-cache nil)
(defun entropy/emacs-font-set--prog-font-set (&optional enable-in-any-mode)
  "Remap `prog-mode' face font using mordern programming fonts when
available. Return a face remap cache did by `face-remap-add-relative'
when the remap is actually remapped or nil otherwise.

When ENABLE-IN-ANY-MODE is non-nil, it should be `t' in where the set
can be enabled in any `major-mode' or a null arguments required
function which should return non-nil which perform the same functional
as if it is `t'."
  (when (and (display-graphic-p)
             (or
              (eq enable-in-any-mode t)
              (and (functionp enable-in-any-mode)
                   (funcall enable-in-any-mode))
              (derived-mode-p 'prog-mode))
             ;; FIXME: face remap for font spec have performance laggy
             (or (not entropy/emacs-font-setting-enable)
                 (and entropy/emacs-font-setting-enable
                      (not (eq entropy/emacs-font-setting-enable 'fira-code))
                      ;; the default font is 'fira-code'
                      (not (eq entropy/emacs-font-setting-enable t)))))
    (let ((font-familys '("Fira Code" "Source Code Pro" "JetBrains Mono")))
      (catch :exit
        (dolist (font-family font-familys)
          (when (find-font (font-spec :family font-family))
            ;; remove old remap cache before adding the new remap
            (when entropy/emacs-font-set--prog-local-remap-cache
              (face-remap-remove-relative
               entropy/emacs-font-set--prog-local-remap-cache))
            (throw
             :exit
             ;; set and return the remap COOKIE used for `face-remap-remove-relative'
             (entropy/emacs-setf-by-body entropy/emacs-font-set--prog-local-remap-cache
               (face-remap-add-relative
                (or
                 ;; remap for solaire if found
                 (and (facep 'solaire-default-face)
                      (or (bound-and-true-p solaire-mode)
                          (bound-and-true-p entropy/emacs-solaire-mode))
                      'solaire-default-face)
                 'default)
                :family font-family)))))))))

(defun entropy/emacs-font-set-modern-english-font-set
    (&optional int disable)
  "Enable mordern english font for current `major-mode' with inner
unexposed filter judgement.

When current `major-mode' is proper for as, then return the a
face remap cache which generated by `face-remap-add-relative', or
nil otherwise.

If DISABLE is non-nil (when used interactively should did with any
prefix-arg), then disable the font remap for current buffer and the
return is not specified.

The INT argument is non-nil used when interactively, otherwise when
set it non-nil, then when enable it, lesser proper status judgments
are performed."
  (interactive (list t (if current-prefix-arg t)))
  (if (and disable (not (derived-mode-p 'prog-mode)))
      (when entropy/emacs-font-set--prog-local-remap-cache
        (face-remap-remove-relative
         entropy/emacs-font-set--prog-local-remap-cache))
    (entropy/emacs-font-set--prog-font-set
     (lambda nil
       (and
        ;; not for `prog-mode' since we've use
        ;; `entropy/emacs-font-set--prog-font-set' for thus.
        (not (derived-mode-p 'prog-mode))
        (or  int
             ;; FIXME: we just allow filter on below modes since any
             ;; other `special-mode' usually use `window-width' to
             ;; calculate the char width which rely on the default
             ;; font width which may smaller than the rmapped font
             ;; width, that say the remapped buffer will appear line
             ;; width overflow in this cases.
             ;;
             ;; Or in the other hand, shall we need to hacking on
             ;; `frame-width' (the underline of `window-width') to let
             ;; those modes calculate correct 'display width' of the
             ;; remapped one?
             (memq major-mode '(Info-mode woman-mode))))))))

(defun entropy/emacs-font--smeset-add-default-hook nil
  (dolist (hook '(Info-mode-hook woman-mode-hook))
    (add-hook hook
              #'entropy/emacs-font-set-modern-english-font-set)))

(defun entropy/emacs-font-set--set-special-font-for-all-buffers (&optional reset)
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when entropy/emacs-font-set--prog-local-remap-cache
        (face-remap-remove-relative
         entropy/emacs-font-set--prog-local-remap-cache))
      (unless reset
        (if (derived-mode-p 'prog-mode)
            (entropy/emacs-font-set--prog-font-set)
          (entropy/emacs-font-set-modern-english-font-set))))))

(if (daemonp)
    (entropy/emacs-with-daemon-make-frame-done
      'enable-eemacs-special-font-set (&rest _)
      :when-gui
      (progn
        (entropy/emacs-run-body-only-once
         (add-hook 'prog-mode-hook #'entropy/emacs-font-set--prog-font-set)
         (entropy/emacs-font--smeset-add-default-hook))
        (entropy/emacs-font-set--set-special-font-for-all-buffers))
      :when-tui
      (entropy/emacs-font-set--set-special-font-for-all-buffers 'reset))
  (entropy/emacs-lazy-initial-advice-before
   '(find-file switch-to-buffer display-buffer)
   "__enable-eemacs-special-font-set__"
   "__enable-eemacs-special-font-set__"
   :prompt-type 'prompt-echo
   :pdumper-no-end nil
   (add-hook 'prog-mode-hook #'entropy/emacs-font-set--prog-font-set)
   (entropy/emacs-font--smeset-add-default-hook)
   ;; enable font set to opened buffers
   (entropy/emacs-font-set--set-special-font-for-all-buffers)))

(defun entropy/emacs--fontsize-set-guard (symbol newval operation _where)
  "`entropy/emacs-font-size-default' vairable wather guard to reset
fontset using `entropy/emacs-font-set-setfont-core'."
  (when (eq operation 'set)
    (unless (eq newval (symbol-value symbol))
      (run-with-idle-timer 0.1 nil #'entropy/emacs-font-set-setfont-core))))

(defun entropy/emacs--fontenable-set-guard (symbol newval operation _where)
  "`entropy/emacs-font-setting-enable' vairable wather guard to reset
fontset using `entropy/emacs-font-set-setfont-core'."
  (when (eq operation 'set)
    (unless (eq newval (symbol-value symbol))
      (run-with-idle-timer 0.1 nil #'entropy/emacs-font-set-setfont-core))))

(defvar entropy/emacs-font-set--var-watcher-initial-done nil)
(defun entropy/emacs-font-set--var-watcher-initial ()
  (unless entropy/emacs-font-set--var-watcher-initial-done
    (add-variable-watcher 'entropy/emacs-font-setting-enable
                          #'entropy/emacs--fontenable-set-guard)
    (add-variable-watcher 'entropy/emacs-font-size-default
                          #'entropy/emacs--fontsize-set-guard)
    (setq entropy/emacs-font-set--var-watcher-initial-done t)))

(defun entropy/emacs-font-set--setfont-initial ()
  (if (not (or (daemonp) entropy/emacs-fall-love-with-pdumper))
      (entropy/emacs-add-hook-with-lambda
        (cons t '__eemacs_initial-font-set-for-common-start) nil
        ;; use the idle timer to prevent eemacs startup redisplay lag
        :use-hook 'entropy/emacs-after-startup-idle-hook
        ;; we should initial at very end where at least be after
        ;; the initial theme load
        :use-append 100
        (let ((inhibit-quit t))
          (entropy/emacs-font-set-setfont-core)
          (add-hook 'entropy/emacs-theme-load-after-hook
                    #'entropy/emacs-font-set-setfont-core)
          (entropy/emacs-font-set--var-watcher-initial)))
    (when entropy/emacs-fall-love-with-pdumper
      (entropy/emacs-add-hook-with-lambda
        (cons t '__eemacs_initial-font-set-for-pdumper) nil
        :use-hook 'entropy/emacs-pdumper-load-hook
        (entropy/emacs-font-set-setfont-core)))
    (entropy/emacs-add-hook-with-lambda
      (cons t '__eemacs-initial-font-set-pre-sets) nil
      :use-hook 'entropy/emacs-after-startup-hook
      (add-hook 'entropy/emacs-theme-load-after-hook
                #'entropy/emacs-font-set-setfont-core)
      (entropy/emacs-font-set--var-watcher-initial))))

(provide 'entropy-emacs-font-set)
