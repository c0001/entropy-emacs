;;; File name: init-font-set.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

(when (and entropy/emacs-font-setting-enable
           (display-graphic-p))
  (let (juge)
    (dolist (font-name '("Noto Mono"
                         "Noto Sans Mono"
                         "Source Code Pro"
                         "Droid Sans"
                         "Symbola"
                         "Noto Serif CJK KR"))
      (if (not (find-font (font-spec :name font-name)))
          (progn
            (warn
             "Couldn't find font \"%s\" please installed from entropy-emacs's font folder"
             font-name)
            (setq juge t))))
    (when (eq juge t)
      (warn
       " Because you enabled `entropy/emacs-font-setting-enable' , but
you didn't install all fonts file from
'https://sourceforge.net/projects/entropy-emax64/files/entropy-emacs_dependency/entropy-emacs_fonts.tgz/download'
so this warning occurring.

Now entropy-emacs allow you specific latin and chinese font, and
their defualt font chosen respectively were:
- latin: Liberation Mono
- han:   Droid Sans

If you want to use your own font config please disable it.
"))
    (if (not (eq juge t))
        (progn 
          (defun entropy/set-font (&optional frame)
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
          (entropy/set-font)
          (add-to-list 'after-make-frame-functions 'entropy/set-font)))))


(provide 'entropy-emacs-font-set)
