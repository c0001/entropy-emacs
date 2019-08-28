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
(require 'entropy-emacs-const)
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
                         "Noto Sans Mono"
                         "Source Code Pro"
                         "Symbola"
                         "Noto Serif CJK KR"))
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
  (setq use-default-font-for-symbols nil)
  (setq inhibit-compacting-font-caches t) ;preventing unicode cusor move lagging for
                                        ;windows refer to mailing list
                                        ;https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-11/msg00471.html
  ;; Setting English Font
  (set-face-attribute 'default nil :family (intern entropy/emacs-font-face-default) :font entropy/emacs-font-face-default)
  (set-face-attribute 'variable-pitch nil :family (intern entropy/emacs-font-face-default) :font entropy/emacs-font-face-default)
  (set-fontset-font "fontset-default" 'latin
                    (font-spec :family entropy/emacs-font-face-default))
  (set-fontset-font "fontset-startup" 'latin
                    (font-spec :family entropy/emacs-font-face-default))
  (set-fontset-font "fontset-standard" 'latin
                    (font-spec :family entropy/emacs-font-face-default))            
  ;;Setting Chinese Font
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family entropy/emacs-font-chinese))
    (unless (string-match "更纱黑体" entropy/emacs-font-chinese)
      (setq face-font-rescale-alist `((,entropy/emacs-font-chinese . 1.2)))))
  ;; setting unicode symbol font
  (set-fontset-font (frame-parameter nil 'font)
                    'symbol
                    (font-spec :family "Symbola"))

  (set-fontset-font (frame-parameter nil 'font)
                    ?“
                    (font-spec :family entropy/emacs-font-chinese))
  (set-fontset-font (frame-parameter nil 'font)
                    ?”
                    (font-spec :family entropy/emacs-font-chinese))
  ;; setting korea font
  (set-fontset-font (frame-parameter nil 'font)
                    'hangul
                    (font-spec :family "Noto Serif CJK KR")))

(defun entropy/emacs-font-set--setfont-initial ()
  (when (and entropy/emacs-font-setting-enable
             (entropy/emacs-display-graphic-p))
    (let ((missing (entropy/emacs-font-set--pre-fonts-check)))
      (unless missing
        (entropy/emacs-font-set-setfont-core)
        (add-to-list 'after-make-frame-functions 'entropy/emacs-font-set-setfont-core)))))

(entropy/emacs-lazy-with-load-trail
 eemacs-fontset
 (entropy/emacs-font-set--setfont-initial))

(provide 'entropy-emacs-font-set)
