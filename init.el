;;; init.el --- entropy-emacs initiliaze raw

;; Copyright (C) 2019-03-29  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; commentary
;;
;;; Configuration:
;;
;; configuration

;; * Code:
;; ** prepare
;; *** Pre comprehensively loads org-mode

;; Org-mode is a buntch of things which will freeze emacs for a while
;; when it's first startup time, those because that =org-mode= relys
;; on and invoking sets of built-in libraris those rarely used,
;; there's also lots of hooks for run when thus, there're one major
;; issues caused that:

;; #+begin_quote
;; Sets of common hooks interned by babels loading and org-modules
;; invoking will be calling while the first enable time, you can see
;; what a hell that if sets of packages inject those hooks after
;; emacs init.
;; #+end_quote

;; Thus for all, we use preload all of stuffs that org used in a
;; clean emacs ENV i.e. the very ahead of the startup procedure, so
;; that all hooks are clean with default value injected, thus all the
;; org init preparation will be did after that.

(setq org-export-backends
      ;; set this before org.el loaded obey the order for its
      ;; docstring.
      '(ascii beamer html icalendar latex man md odt org texinfo
              confluence deck freemind groff koma-letter rss s5
              taskjuggler))

(defun entropy/emacs-org-do-load-org-babels ()
  "Load all org-babels."
  (interactive)
  (let ((prop-format (lambda (msg-str)
                       (redisplay t)
                       (message
                        (format
                         "*Lazy loading*: %s ... [this may cost some time, take a coffee -v-]"
                         (propertize msg-str
                                     'face
                                     'font-lock-type-face)))
                       (redisplay t))))
    (funcall prop-format "org-core")
    (require 'org)
    (funcall prop-format "org-ob-core")
    (require 'ob)
    (funcall prop-format "org-babels")
    (let ((ob-lang (mapcar
                    #'(lambda (x) (cons x t))
                    '(vala
                      tangle
                      table
                      stan
                      sqlite
                      sql
                      shen
                      shell
                      sed
                      screen
                      scheme
                      sass
                      ruby
                      ref
                      python
                      processing
                      plantuml
                      picolisp
                      perl
                      org
                      octave
                      ocaml
                      mscgen
                      maxima
                      matlab
                      makefile
                      lua
                      lob
                      lisp
                      lilypond
                      ledger
                      latex
                      js
                      java
                      io
                      hledger
                      haskell
                      groovy
                      gnuplot
                      fortran
                      forth
                      exp
                      eval
                      emacs-lisp
                      ebnf
                      dot
                      ditaa
                      css
                      core
                      coq
                      comint
                      clojure
                      calc
                      awk
                      asymptote
                      abc
                      R
                      J
                      C))))
      (org-babel-do-load-languages
       'org-babel-load-languages ob-lang))))

(entropy/emacs-org-do-load-org-babels)

;; Pre enable `org-mode' for forcely loading `org-modules'.
(with-temp-buffer (org-mode))

;; ** multi-version emacs compatible

;; *** make `setq-local' compatible
(when (version< emacs-version "27")
  (defmacro setq-local (&rest pairs)
    "Make variables in PAIRS buffer-local and assign them the corresponding values.

PAIRS is a list of variable/value pairs.  For each variable, make
it buffer-local and assign it the corresponding value.  The
variables are literal symbols and should not be quoted.

The second VALUE is not computed until after the first VARIABLE
is set, and so on; each VALUE can use the new value of variables
set earlier in the 'setq-local'.  The return value of the
'setq-local' form is the value of the last VALUE.

\(fn [VARIABLE VALUE]...)"
    (declare (debug setq))
    (unless (zerop (mod (length pairs) 2))
      (error "PAIRS must have an even number of variable/value members"))
    (let ((expr nil))
      (while pairs
        (unless (symbolp (car pairs))
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        ;; Can't use backquote here, it's too early in the bootstrap.
        (setq expr
              (cons
               (list 'set
                     (list 'make-local-variable (list 'quote (car pairs)))
                     (car (cdr pairs)))
               expr))
        (setq pairs (cdr (cdr pairs))))
      (macroexp-progn (nreverse expr)))))

;; ** Warning of startup


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(when (version< emacs-version "26")
  (error "This requires Emacs 26 and above!"))

;; ** Dry-land API defination

(defun entropy/emacs-version-compatible
    (version-limit version-current &optional strict)
  "Versions compatible judger for version spec basing rule of
commonly \"x.y.z\" convention to compared the required version
VERSION-LIMIT and the queried version VERSION-CURRENT.

The \"x.y.z\" version name convention defination is shown below:

- z: Quality update, API not changed
- y: Feature adding update, API not changed
- x: Compatibility destructive update, API are changed, indicating
     the fresh new version.

The compatible version comparation is rased on the two ways:
1. When optional argument STRICT is non-nil that just sub-version
   variable \"z\" can be dynamic one.
2. Vice-versa to case 1, \"y\" and \"z\" can be dynamically
   changed."
  (let* ((vl-1-regexp (regexp-quote
                       (let ((vl-list (butlast (version-to-list version-limit))))
                         (mapconcat 'number-to-string vl-list "."))))
         (vl-2-regexp (regexp-quote
                       (let ((vl-list (butlast (version-to-list version-limit) 2)))
                         (mapconcat 'number-to-string vl-list ".")))))
    (or (version= version-limit version-current)
        (and (version< version-limit version-current)
             (if strict
                 (string-match-p vl-1-regexp version-current)
               (string-match-p vl-2-regexp version-current))))))

;; ** startup entropy-emacs
(defvar entropy/emacs-user-emacs-directory
  (file-name-directory load-file-name)
  "Top eemacs host directory replaced of `user-emacs-directory'
for preventing unregular loading procedure by modification of
emacs upstream")

(let ((use-dialog-box))                 ;inhibit dialog box for
                                        ;entropy-emacs initialization
                                        ;process to prevent judgment
                                        ;interaction missing upon
                                        ;emacs-28.
  (require
   'entropy-emacs
   (expand-file-name
    "elements/entropy-emacs.el"
    entropy/emacs-user-emacs-directory)))
