;;; entropy-emacs-font.el --- entropy-emacs font configuration
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

(defun entropy/emacs-font-set-register ()
  (when (eq entropy/emacs-font-setting-enable t)
    (setq entropy/emacs-font-setting-enable 'fira-code))
  (let ((group (alist-get entropy/emacs-font-setting-enable
                          entropy/emacs-fontsets-fonts-collection-alias)))
    (unless group
      (user-error "Invalid arg value for `entropy/emacs-font-setting-enable' to '%s'"
                  entropy/emacs-font-setting-enable))
    (setq entropy/emacs-fontsets-used-latin-font
          (plist-get group :latin)
          entropy/emacs-fontsets-used-cjk-sc-font
          (plist-get group :sc)
          entropy/emacs-fontsets-used-cjk-tc-font
          (plist-get group :tc)
          entropy/emacs-fontsets-used-cjk-jp-font
          (plist-get group :jp)
          entropy/emacs-fontsets-used-cjk-kr-font
          (plist-get group :kr)
          entropy/emacs-fontsets-used-symbol-font
          (plist-get group :symbol)
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

(defun entropy/emacs-font-set-setfont-core (&optional frame)
  (interactive)
  (let ((after-do (plist-get (alist-get entropy/emacs-font-setting-enable
                                        entropy/emacs-fontsets-fonts-collection-alias)
                             :after)))
    (when (and (display-graphic-p)
               (not (entropy/emacs-font-set--pre-fonts-check)))

      ;; preventing unicode cusor move lagging for
      ;; windows refer to mailing list
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-11/msg00471.html
      (when sys/win32p
        (setq inhibit-compacting-font-caches t))

      ;; Setting latin Font
      (set-fontset-font nil 'latin
                        (font-spec :family entropy/emacs-fontsets-used-latin-font)
                        (or frame (selected-frame)))
      (set-face-attribute
       'variable-pitch
       (or frame (selected-frame))
       :family entropy/emacs-fontsets-used-latin-font)

      ;; default interface font spec
      (mapc (lambda (type)
              (set-fontset-font
               type 'latin
               (font-spec :family entropy/emacs-fontsets-used-latin-font)
               (or frame (selected-frame))))
            '("fontset-default" "fontset-startup" "fontset-standard"))

      ;;Setting cjk
      (dolist (charset '(han cjk-misc kana kanbun bopomofo))
        (set-fontset-font nil
                          charset
                          (font-spec
                           :family
                           entropy/emacs-default-cjk-cn-font)
                          (or frame (selected-frame))))

      (set-fontset-font nil
                        '(?ぁ . ?ヶ)
                        (font-spec
                         :family
                         entropy/emacs-fontsets-used-cjk-jp-font)
                        (or frame (selected-frame)))

      (set-fontset-font nil
                        'hangul
                        (font-spec :family entropy/emacs-fontsets-used-cjk-kr-font)
                        (or frame (selected-frame)))

      (if (eq entropy/emacs-font-setting-enable 'sarasa)
          (setq face-font-rescale-alist nil)
        (setq face-font-rescale-alist
              `((,entropy/emacs-fontsets-used-cjk-sc-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-tc-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-jp-font . 1.2)
                (,entropy/emacs-fontsets-used-cjk-kr-font . 1.2))))

      ;; setting unicode symbol font
      (if entropy/emacs-fontsets-used-symbol-font
          (progn (setq use-default-font-for-symbols nil)
                 (set-fontset-font nil
                                   'symbol
                                   (font-spec :family entropy/emacs-fontsets-used-symbol-font)
                                   (or frame (selected-frame))))
        (setq use-default-font-for-symbols t))

      (set-fontset-font nil
                        ?“
                        (font-spec :family entropy/emacs-default-cjk-cn-font)
                        (or frame (selected-frame)))

      (set-fontset-font nil
                        ?”
                        (font-spec :family entropy/emacs-default-cjk-cn-font)
                        (or frame (selected-frame)))

      ;; extra spec setting
      (when (and after-do (functionp after-do))
        (funcall after-do (or frame (selected-frame))))

      (if (< entropy/emacs-font-size-default 15)
          (let ((height (ceiling (* entropy/emacs-font-size-default 10))))
            ;; FIXME: remap height with unpredictable manually try but
            ;; stable for me for the sake for make 'Latin' an 'CJK'
            ;; can be strictly be 1:2 size linetype scale. Is there
            ;; any commonly way to approach to this?
            (setq height
                  (cond
                   ((and (<= height 110)
                         (>= height 100))
                    108)
                   ((and (<= height 120)
                         (>= height 110))
                    117)
                   ((and (<= height 130)
                         (>= height 120))
                    124)
                   ((and (<= height 140)
                         (>= height 150))
                    148)
                   ((and (<= height 150)
                         (>= height 140))
                    148)))
            (when height
              (set-face-attribute 'default (or frame (selected-frame))
                                  :height
                                  height)))
        (warn
         "Your default font size is too large,
you must set it smaller than 15 for adapting to other entropy-emacs settings."))
      ;; run font-set after hooks
      (run-hooks 'entropy/emacs-font-set-end-hook))))

(defun entropy/emacs-font-set--setfont-initial ()
  (when entropy/emacs-font-setting-enable
    (unless (or (daemonp) entropy/emacs-fall-love-with-pdumper)
      (entropy/emacs-font-set-setfont-core))
    (add-hook 'entropy/emacs-after-startup-hook
              #'(lambda ()
                  (add-hook 'entropy/emacs-theme-load-after-hook
                            #'entropy/emacs-font-set-setfont-core)))))

(defun entropy/emacs-font-set--prog-font-set ()
  "Remap `prog-mode' face font using mordern programming fonts
when available."
  (when (and (display-graphic-p)
             (derived-mode-p 'prog-mode)
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
            (face-remap-add-relative
             (or
              ;; remap for solaire if found
              (and (facep 'solaire-default-face)
                   (or (bound-and-true-p solaire-mode)
                       (bound-and-true-p entropy/emacs-solaire-mode))
                   'solaire-default-face)
              'default)
             :family font-family)
            (throw :exit nil)))))))

(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer)
 "__set-prog-mode-font-set__" "__set-prog-mode-font-set__" prompt-echo
 :pdumper-no-end nil
 (add-hook 'prog-mode-hook
           #'entropy/emacs-font-set--prog-font-set)
 ;; enable font set to opened buffers
 (dolist (buff (buffer-list))
   (with-current-buffer buff
     (when (derived-mode-p 'prog-mode)
       (entropy/emacs-font-set--prog-font-set)))))

(defun entropy/emacs--fontsize-set-guard (symbol newval operation where)
  "`entropy/emacs-font-size-default' vairable wather guard to reset
fontset using `entropy/emacs-font-set-setfont-core'."
  (when (eq operation 'set)
    (unless (eq newval (symbol-value symbol))
      (run-with-idle-timer 0.1 nil #'entropy/emacs-font-set-setfont-core))))
(add-variable-watcher 'entropy/emacs-font-size-default
                      #'entropy/emacs--fontsize-set-guard)

(provide 'entropy-emacs-font-set)
