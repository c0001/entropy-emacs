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
you didn't install all fonts file from
'https://sourceforge.net/projects/entropy-emax64/files/entropy-emacs_dependency/entropy-emacs_fonts.tgz/download'
so this warning occurring.

Now entropy-emacs allow you specific latin and chinese font, and
their defualt font chosen respectively were:
- latin: Liberation Mono
- han:   Droid Sans

If you want to use your own font config please disable it.
")

(defun entropy/emacs-font-set--pre-fonts-check ()
  (let (judge (prompt "")  (count 1))
    (dolist (font-name '("Noto Mono"
                         "Symbola"
                         "Noto Sans Mono CJK SC"
                         "Noto Sans Mono CJK TC"
                         "Noto Sans Mono CJK JP"
                         "Noto Sans Mono CJK KR"))
      (unless (find-font (font-spec :name font-name))
        (push font-name judge)))

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
  (let ()
    (when (and (display-graphic-p)
               (not (entropy/emacs-font-set--pre-fonts-check)))
      (setq use-default-font-for-symbols nil)
      ;; preventing unicode cusor move lagging for
      ;; windows refer to mailing list
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-11/msg00471.html
      (setq inhibit-compacting-font-caches t)

      ;; Setting English Font
      (set-fontset-font
       nil
       'latin
       (font-spec :family entropy/emacs-default-latin-font)
       frame)

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


      (setq face-font-rescale-alist `((,entropy/emacs-default-cjk-cn-font . 1.2)
                                      (,entropy/emacs-default-cjk-tc-font . 1.2)
                                      (,entropy/emacs-default-cjk-jp-font . 1.2)
                                      (,entropy/emacs-default-cjk-kr-font . 1.2)))

      ;; setting unicode symbol font
      (set-fontset-font nil
                        'symbol
                        (font-spec :family "Symbola")
                        (or frame (selected-frame)))

      (set-fontset-font nil
                        ?“
                        (font-spec :family entropy/emacs-default-cjk-cn-font)
                        (or frame (selected-frame)))

      (set-fontset-font nil
                        ?”
                        (font-spec :family entropy/emacs-default-cjk-cn-font)
                        (or frame (selected-frame)))

      (if (< entropy/emacs-font-size-default 15)
          (set-face-attribute 'default (or frame (selected-frame))
                              :height (ceiling (* entropy/emacs-font-size-default 10)))
        (warn
         "Your default font size is too large, you must set it smaller than 15 for adapting to other entropy-emacs settings.")))))

(defun entropy/emacs-font-set--setfont-initial ()
  (when entropy/emacs-font-setting-enable
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  #'entropy/emacs-font-set-setfont-core)
      (entropy/emacs-font-set-setfont-core))))

(entropy/emacs-lazy-with-load-trail
 eemacs-fontset
 (entropy/emacs-font-set--setfont-initial))

(provide 'entropy-emacs-font-set)
