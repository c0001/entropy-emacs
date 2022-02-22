;; init-const.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Constants.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * Code:
;; ** require
(entropy/emacs-common-require-feature 'entropy-emacs-defcustom)

;; ** system type
;; *** basic conditions
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a Windows-NT system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst sys/wsl-env-p
  (let ((wsl-indcf "/proc/version"))
    (and
     ;; use uname judge
     (executable-find "uname")
     (string-match-p
      "Microsoft"
      (shell-command-to-string "uname -a"))
     ;; cat /proc/version file
     (file-exists-p wsl-indcf)
     (string-match-p
      "\\(microsoft\\|Microsoft\\)"
      (with-temp-buffer
        (insert-file-contents wsl-indcf)
        (buffer-substring
         (point-min)
         (point-max))))))
  "Judge whether current env is in WSL(windows subsystem linux)
environment."
  )

(defconst sys/wslg-env-p (and sys/wsl-env-p (display-graphic-p))
  "Judge whether current env is in WSLg(windows subsystem linux
gui) environment.")

;; *** group filter
(defconst sys/is-win-group
  (or sys/win32p sys/cygwinp)
  "System type group filter for win32 platform.

Include all windows posix emulator environment, as 'Cygwin' or
'Msys2'.")

(defconst sys/is-posix-compatible
  (or sys/linuxp sys/macp sys/cygwinp)
  "System type group filter for posix comptible system
platforms even for posix emulators.

Include 'GNU/Linux', 'Darwin' or any 'UNIX' distribution.

Posix emulator supports for:

- 'Cygwin'
- 'Msys' or 'Msys2'")

(defconst sys/is-graphic-support
  (or (or sys/win32p sys/linux-x-p sys/mac-x-p)

      ;;FIXME: get DESKTOP_SESSION like env var on MACOS
      ;; (not (string-empty-p (getenv "MACOS-DISPPLAY-TYPE")))

      ;; FIXME: this windows display available judger may be not
      ;; correct, this justice used to judge whether emacs built with
      ;; a anti-`sys/win32p' type but running on a Windows-NT system
      ;; which support display-graphic feature.
      (and sys/is-win-group (string-match-p "Windows_NT" (or (getenv "OS") "")))

      (not (string-empty-p (or (getenv "DESKTOP_SESSION") "")))
      (not (string-empty-p (or (getenv "XDG_CURRENT_DESKTOP") "")))
      (display-graphic-p))
  "System group filter for graphic supported platforms.")

(defconst sys/is-linux-and-graphic-support-p
  (and sys/linuxp
       sys/is-graphic-support)
  "Systems type filter for judge current system whether is
Gnu/linux based and its environment whether support graphic
display.")

(defconst sys/is-wingroup-and-graphic-support-p
  (and sys/is-win-group
       sys/is-graphic-support)
  "Systems type filter for judge current system whether is
WINDOWS based system i.e. satisfied `sys/is-win-group' and its
environment whether support graphic display.")

(defconst sys/is-mac-and-graphic-support-p
  (and sys/macp
       sys/is-graphic-support)
  "Systems type filter for judge current system whether is Darwin
kernel based and its environment whether support graphic
display.")

;; ** others
(defconst entropy/emacs-origin-load-path (copy-tree load-path))

(defconst entropy/emacs-ecv "VERSION: 1.0.0 Tag: ONEPIECE"
  "Version of entropy-emacs.")

(defconst entropy/emacs-home-page "https://github.com/c0001/entropy-emacs"
  "The =entropy-emacs= project home page uri.")

(defconst entropy/emacs-imagemagick-feature-p
  (image-type-available-p 'imagemagick)
  "Whether support imagemagick on this emacs session.")

(require 'faces)
(defconst entropy/emacs-face-attributes-list
  (mapcar (lambda (x) (car x)) face-attribute-name-alist)
  "Emacs internally face attribte symbols list")

(defconst entropy/emacs-org-babel-featurs
  (delete nil
          `(ob-fortran
            ob-C
            ob-python
            ob-R
            ob-ruby
            ob-sass
            ob-scheme
            ob-screen
            ob-sed
            ob-shell
            ob-sql
            ob-sqlite
            ob-table
            ob-tangle
            ob-lisp
            ob-lob
            ob-lua
            ob-makefile
            ob-matlab
            ob-maxima
            ob-ocaml
            ob-octave
            ob-org
            ob-perl
            ob-plantuml
            ob-processing
            ob-ref
            ob-clojure
            ob-core
            ob-dot
            ob-emacs-lisp
            ob-eshell
            ob-eval
            ob-exp
            ob-forth
            ob-gnuplot
            ob-groovy
            ob-haskell
            ob-java
            ob-js
            ,(unless (< emacs-major-version 28)
               'ob-julia)
            ob-latex
            ob-lilypond
            ob-awk
            ob-calc
            ob-comint
            ob-css
            ob-ditaa
            ))
          "The ob-* features list")

;; * provide
(provide 'entropy-emacs-defconst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
