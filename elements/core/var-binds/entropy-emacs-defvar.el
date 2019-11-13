;;; entropy-emacs-defvar.el --- entropy emacs internal variable declaration
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defvar.el
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
;; This file was the collection of `entropy-emacs' internal sharing
;; variables.
;; 
;; * Configuration:
;; 
;; No individually loading designation without `entropy-emacs'.
;; 
;; * Code:
(require 'entropy-emacs-defconst)

;; ** individuals
(defvar entropy/emacs-top-keymap (make-sparse-keymap))

(defvar entropy/emacs-top-key
  (if (display-graphic-p)
      "C-`"
    "C-@")
  "Top key for entropy-emacs global keybind for
`entropy/emacs-top-keymap'.

It is a string used for `kbd'.")

(defvar entropy/emacs-lang-locale (car default-process-coding-system)
  "The locale lang.")

(defvar entropy/emacs-web-development-environment nil
  "Whether using enable web-development envrionment.

This variable is mainly for the judgement button for
`entropy/emacs-browse-url-function' for determined whether to using the
specific browser to visualize current file.")

(defvar entropy/emacs-window-center-integer 9)

(defvar entropy/emacs-dashboard-buffer-name  "*WELCOM TO ENTROPY-EMACS*"
  "Title of entropy-emacs initial dashboard buffer. ")

(defvar entropy/emacs-default-cjk-cn-font
  (or entropy/emacs-default-cjk-sc-font
      entropy/emacs-default-cjk-tc-font)
  "The default font for chinese lang-script, using
`entropy/emacs-default-cjk-tc-font' when SC was nil.")

;; ** cl-* compatible

(defvar entropy/emacs-cl-compatible-reflects
    '(
      (get* . cl-get)
      (random* . cl-random)
      (rem* . cl-rem)
      (mod* . cl-mod)
      (round* . cl-round)
      (truncate* . cl-truncate)
      (ceiling* . cl-ceiling)
      (floor* . cl-floor)
      (rassoc* . cl-rassoc)
      (assoc* . cl-assoc)
      (member* . cl-member)
      (delete* . cl-delete)
      (remove* . cl-remove)
      (defsubst* . cl-defsubst)
      (sort* . cl-sort)
      (function* . cl-function)
      (defmacro* . cl-defmacro)
      (defun* . cl-defun)
      (mapcar* . cl-mapcar)

      remprop
      getf
      tailp
      list-length
      nreconc
      revappend
      concatenate
      subseq
      random-state-p
      make-random-state
      signum
      isqrt
      lcm
      gcd
      notevery
      notany
      every
      some
      mapcon
      mapl
      maplist
      map
      equalp
      coerce
      tree-equal
      nsublis
      sublis
      nsubst-if-not
      nsubst-if
      nsubst
      subst-if-not
      subst-if
      subsetp
      nset-exclusive-or
      set-exclusive-or
      nset-difference
      set-difference
      nintersection
      intersection
      nunion
      union
      rassoc-if-not
      rassoc-if
      assoc-if-not
      assoc-if
      member-if-not
      member-if
      merge
      stable-sort
      search
      mismatch
      count-if-not
      count-if
      count
      position-if-not
      position-if
      position
      find-if-not
      find-if
      find
      nsubstitute-if-not
      nsubstitute-if
      nsubstitute
      substitute-if-not
      substitute-if
      substitute
      delete-duplicates
      remove-duplicates
      delete-if-not
      delete-if
      remove-if-not
      remove-if
      replace
      fill
      reduce
      compiler-macroexpand
      define-compiler-macro
      assert
      check-type
      typep
      deftype
      defstruct
      callf2
      callf
      letf*
      letf
      rotatef
      shiftf
      remf
      psetf
      (define-setf-method . define-setf-expander)
      the
      locally
      multiple-value-setq
      multiple-value-bind
      symbol-macrolet
      macrolet
      progv
      psetq
      do-all-symbols
      do-symbols
      do*
      do
      loop
      return-from
      return
      block
      etypecase
      typecase
      ecase
      case
      load-time-value
      eval-when
      destructuring-bind
      gentemp
      pairlis
      acons
      subst
      adjoin
      copy-list
      ldiff
      list*
      tenth
      ninth
      eighth
      seventh
      sixth
      fifth
      fourth
      third
      endp
      rest
      second
      first
      svref
      copy-seq
      evenp
      oddp
      minusp
      plusp
      floatp-safe
      declaim
      proclaim
      nth-value
      multiple-value-call
      multiple-value-apply
      multiple-value-list
      values-list
      values
      pushnew
      decf
      incf
      ))

;; ** garbage collection

(defvar entropy/emacs-gc-threshold-basic 20000000
  "The basic thredshold for the growth for `gc-cons-threshold'")

(defvar entropy/emacs-garbage-collect-idle-timer nil
  "The garbage collection idle timer for entropy-emacs.")

;; ** theme refer

(defvar entropy/emacs-theme-sticker ""
  "Current theme used for this session.")

(defvar entropy/emacs-theme-load-before-hook nil
  "Hook runs befor theme loading procedure built by `load-theme'.

This hook is declared within the around advice
`entropy/emacs-theme-load-advice', hook enabled when entropy-emacs
package `entropy-emacs-defvar' loaded.

The follow auto-laod wrapper may be used in `custom-file'
#+BEGIN_SRC elisp
  (with-eval-after-load 'entropy-emacs-defvar
    (add-hook 'entropy/emacs-theme-load-before-hook symbol-you-specified))
#+END_SRC
  ")

(defvar entropy/emacs-theme-load-after-hook nil
  "Hook runs after theme loading procedure built by `load-theme'.

This hook is declared within the around advice
`entropy/emacs-theme-load-advice', hook enabled when entropy-emacs
package `entropy-emacs-defvar' loaded.

The follow auto-laod wrapper may be used in `custom-file'
#+BEGIN_SRC elisp
(with-eval-after-load 'entropy-emacs-defvar
    (add-hook 'entropy/emacs-theme-load-after-hook symbol-you-specified))
#+END_SRC
  ")

(defun entropy/emacs-theme-load-advice (old-func &rest args)
  "Advice for `load-theme' which adding the before ans after hook:

- `entropy/emacs-theme-load-before-hook'
- `entropy/emacs-theme-load-after-hook'

See their docstring for more details for thus hooks usage.

The wrappered `load-theme' function will register the gloabl
dynamic variabe `entropy/emacs-theme-sticker' for current enabled
theme symbol, you can use it in the after hook
`entropy/emacs-theme-load-after-hook', which means that that hook
is ran after the registering procedure done within `progn' scope."
  (run-hooks 'entropy/emacs-theme-load-before-hook)
  (apply old-func args)
  (progn
    (let ((theme-load (car args)))
      (setq entropy/emacs-theme-sticker theme-load))
    (run-hooks 'entropy/emacs-theme-load-after-hook)))

(advice-add 'load-theme :around #'entropy/emacs-theme-load-advice)

(defvar entropy/emacs-enable-solaire-registers
  '((files . (find-file-hook))
    (magit-files . (magit-find-file-hook))
    (dired . (dired-mode-hook))
    (w3m . (w3m-mode-hook))
    (elfeed . (elfeed-search-mode-hook elfeed-show-mode-hook))
    (gnus-group . (gnus-group-mode-hook))
    (gnus-sum . (gnus-summary-mode-hook)))
  "Registers list for activing the `solaire-mode'

Each register is one cons whose car was the load feature symbol
and the cdr is the list of hook belong to that feature. ")

;; ** modeline refer

(defvar entropy/emacs-mode-line-sticker ""
  "Sticker for current modeline style")

;; ** pdumper
(defvar entropy/emacs-pdumper-pre-lpth nil
  "The fully preserved `load-path' for pdumper session, this
variable is assigned while pdumper procedure triggered, append
emacs internal load path and the subdirs of
`entropy/emacs-ext-extensions-elpa-dir' (i.e. the current
`package-user-dir' specified by what you settng for
`entropy/emacs-use-extensions-type').

Note that the entorpy-emacs just use the malpa-local type of
`entropy/emacs-use-extensions-type' to dumping as, thus the
submodules pre-loading is not supported for entropy-emacs
pdumper feature.")

;; * provide
(provide 'entropy-emacs-defvar)
