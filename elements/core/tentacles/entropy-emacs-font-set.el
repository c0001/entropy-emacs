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

(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)

(defvar entropy/emacs-font-set--warn
  "Because you enabled `entropy/emacs-font-setting-enable' , but
you didn't install all fonts file in your system, so this warning
occurring. If you want to use your own font config please disable
it.

===== Missing fonts list: =====
")

(setq entropy/emacs-font-set-fontset-group-alias
      '((sarasa :latin "Sarasa Mono SC" :sc "Sarasa Mono SC" :tc "Sarasa Mono TC"
                :jp "Sarasa Mono J" :kr "Sarasa Mono K")
        (google :latin "Noto Mono" :sc "Noto Sans Mono CJK SC" :tc "Noto Sans Mono CJK TC"
                :jp "Noto Sans Mono CJK JP" :kr "Noto Sans Mono CJK KR"
                :symbol "Noto Sans Symbols"
                :extra ("Symbola")
                :after
                (lambda (frame)
                  (dolist (char '(?← ?→ ?↑ ?↓))
                    (set-fontset-font nil char (font-spec :family "Symbola")))
                  (when (alist-get "Symbola" face-font-rescale-alist nil nil #'string=)
                    (setq face-font-rescale-alist
                          (assoc-delete-all
                           "Symbola"
                           face-font-rescale-alist #'string=)))
                  (add-to-list 'face-font-rescale-alist (cons "Symbola" 0.85))))
        (fira-code :latin "Fira Mono" :sc "Noto Sans Mono CJK SC" :tc "Noto Sans Mono CJK TC"
                   :jp "Noto Sans Mono CJK JP" :kr "Noto Sans Mono CJK KR"
                   :symbol "Noto Sans Symbols"
                   :after
                   (lambda (frame)
                     (dolist (char '(?← ?→ ?↑ ?↓))
                       (set-fontset-font nil char (font-spec :family "Symbola")))
                     (when (alist-get "Symbola" face-font-rescale-alist nil nil #'string=)
                       (setq face-font-rescale-alist
                             (assoc-delete-all
                              "Symbola"
                              face-font-rescale-alist #'string=)))
                     (add-to-list 'face-font-rescale-alist (cons "Symbola" 0.85))))))

(defun entropy/emacs-font-set-register ()
  (when (eq entropy/emacs-font-setting-enable t)
    (setq entropy/emacs-font-setting-enable 'google))
  (let ((group (alist-get entropy/emacs-font-setting-enable
                          entropy/emacs-font-set-fontset-group-alias)))
    (unless group
      (user-error "Invalid arg value for `entropy/emacs-font-setting-enable' to '%s'"
                  entropy/emacs-font-setting-enable))
    (setq entropy/emacs-default-latin-font
          (plist-get group :latin)
          entropy/emacs-default-cjk-sc-font
          (plist-get group :sc)
          entropy/emacs-default-cjk-tc-font
          (plist-get group :tc)
          entropy/emacs-default-cjk-jp-font
          (plist-get group :jp)
          entropy/emacs-default-cjk-kr-font
          (plist-get group :kr)
          entropy/emacs-default-symbol-font
          (plist-get group :symbol)
          entropy/emacs-default-extra-fonts
          (plist-get group :extra))
    (setq entropy/emacs-default-cjk-cn-font
          (if (eq entropy/emacs-font-chinese-type 'sc)
              entropy/emacs-default-cjk-sc-font
            entropy/emacs-default-cjk-tc-font))))

(defun entropy/emacs-font-set--pre-fonts-check ()
  (entropy/emacs-font-set-register)
  (let (judge (prompt "")  (count 1))
    (dolist (font-name `(,entropy/emacs-default-latin-font
                         ,entropy/emacs-default-cjk-cn-font
                         ,entropy/emacs-default-cjk-jp-font
                         ,entropy/emacs-default-cjk-kr-font
                         ,entropy/emacs-default-symbol-font))
      (when (stringp font-name)
        (unless (find-font (font-spec :name font-name))
          (push font-name judge))))
    (when entropy/emacs-default-extra-fonts
      (dolist (font-name entropy/emacs-default-extra-fonts)
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
                                        entropy/emacs-font-set-fontset-group-alias)
                             :after)))
    (when (and (display-graphic-p)
               (not (entropy/emacs-font-set--pre-fonts-check)))

      ;; preventing unicode cusor move lagging for
      ;; windows refer to mailing list
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-11/msg00471.html
      (setq inhibit-compacting-font-caches t)

      ;; Setting latin Font
      (set-fontset-font nil 'latin
                        (font-spec :family entropy/emacs-default-latin-font)
                        (or frame (selected-frame)))
      (set-face-attribute
       'variable-pitch
       (or frame (selected-frame))
       :family entropy/emacs-default-latin-font)

      ;; default interface font spec
      (mapc (lambda (type)
              (set-fontset-font
               type 'latin
               (font-spec :family entropy/emacs-default-latin-font)
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
                         entropy/emacs-default-cjk-jp-font)
                        (or frame (selected-frame)))

      (set-fontset-font nil
                        'hangul
                        (font-spec :family entropy/emacs-default-cjk-kr-font)
                        (or frame (selected-frame)))

      (if (eq entropy/emacs-font-setting-enable 'sarasa)
          (setq face-font-rescale-alist nil)
        (setq face-font-rescale-alist
              `((,entropy/emacs-default-cjk-sc-font . 1.2)
                (,entropy/emacs-default-cjk-tc-font . 1.2)
                (,entropy/emacs-default-cjk-jp-font . 1.2)
                (,entropy/emacs-default-cjk-kr-font . 1.2))))

      ;; setting unicode symbol font
      (if entropy/emacs-default-symbol-font
          (progn (setq use-default-font-for-symbols nil)
                 (set-fontset-font nil
                                   'symbol
                                   (font-spec :family entropy/emacs-default-symbol-font)
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
            (when (eq entropy/emacs-font-setting-enable 'sarasa)
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
                      148))))
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
    (add-hook 'entropy/emacs-theme-load-after-hook
              #'entropy/emacs-font-set-setfont-core)))

(entropy/emacs-font-set--setfont-initial)

(provide 'entropy-emacs-font-set)
