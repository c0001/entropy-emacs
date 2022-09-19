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

;; ** Lisp referred constant
;; *** regexp

(defconst entropy/emacs-cjk-punc-regexp
  "[\u3002\uff1b\uff0c\uff1a\u201c\u201d\uff08\uff09\u3001\uff1f\u300a\u300b]"
  "CJK punctuation regexp presentation")

(defconst entropy/emacs-cjk-char-regexp
  "[\u4e00-\u9fa5]"
  "CJK char regexp presentation")

(defconst entropy/emacs-buffer-blank-line-regexp "^\\s-*$"
  "Regexp matched a buffer blank line i.e. a line of buffer whose
contents is empty or just any whitespaces.

The usage of this regexp is via `looking-at' or
`re-search-forward' with `point' at the `beginning-of-line'.")

(defconst entropy/emacs-buffer-nblank-line-regexp "^[^\n\r]*[^\n\r\t ]\\{1\\}[^\n\r]*$"
  "Regexp matched a buffer non blank line i.e. a line of buffer
whose contents at least has an non-whitespace char.

The usage of this regexp is via `looking-at' or
`re-search-forward' with `point' at the `beginning-of-line'.")

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

(defconst sys/wsl2-env-p
  (let ((wsl-indcf "/proc/version"))
    (and
     ;; use uname judge
     (executable-find "uname")
     (string-match-p
      "Microsoft"
      (shell-command-to-string "uname -a"))
     ;; cat /proc/version file
     (file-exists-p wsl-indcf)
     (and
      (string-match-p
       "\\(microsoft\\|Microsoft\\)"
       (with-temp-buffer
         (insert-file-contents wsl-indcf)
         (buffer-substring
          (point-min)
          (point-max))))
      t)))
  "Judge whether current env is in WSL(windows subsystem linux)
ver.2 environment."
  )

(defconst sys/wsl2g-env-p (and sys/wsl2-env-p (display-graphic-p))
  "Judge whether current env is in WSLg(windows subsystem linux
ver.2 gui) environment.")

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
      ;; WAYLAND
      (not (string-empty-p (or (getenv "WAYLAND_DISPLAY") "")))
      ;; X11
      (not (string-empty-p (or (getenv "DISPLAY") "")))
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

;; ** Eemacs Error Framework

(defvar entropy/emacs-top-error-symbol
  'entropy/emacs-top-error-signal)
(define-error entropy/emacs-top-error-symbol
  "Eemacs top error" 'error)
(defun entropy/emacs-do-eemacs-top-error (msg)
  (signal
   entropy/emacs-top-error-symbol
   (list msg)))

(defvar entropy/emacs-buffer-position-invalid-error-symbol
  'entropy/emacs-buffer-position-invalid-error)
(define-error entropy/emacs-buffer-position-invalid-error-symbol
  "Eemacs wrong type of buffer position" entropy/emacs-top-error-symbol)
(cl-defun entropy/emacs-do-error-for-buffer-position-invalid
    (position &key use-any-msg with-range-check without-restriction)
  "Do error when a POSITION is not a valid buffer position value of
`current-buffer' or error with message USE-ANY-MSG directly
without any checks.

When USE-ANY-MSG is not set and when POSITION is a marker, error
when its `marker-position' doesn't be set i.e. empty. It success
in this occasion, also check the marker's integer value, see
below:

When USE-ANY-MSG is not set and POSTION is not a maker or after
the marker checking, error when POSITION is not a `natnump' or a
`zerop' object.

When USE-ANY-MSG is not set and WITH-RANGE-CHECK is set, also
error when position is not `<' the `point-min' or `>' the
`point-max'. With WITHOUT-RESTRICTION set, the buffer restriction
is temporarily disabled using `save-restriction' to get the
minimal `point-min' and max `point-marker'.

Return non-nil when no error matched.
"
  (if use-any-msg
      (format "the buffer-position var '%s' invalid since: %s."
              use-any-msg)
    (let* ((posmk-p  (markerp position))
           (pt       (if posmk-p (marker-position position) position))
           (ptwp     (and (natnump pt) (> pt 0)))
           pt-p ptmin ptmax)
      (catch :exit
        (unless ptwp
          (signal
           entropy/emacs-buffer-position-invalid-error-symbol
           (list
            (format "the buffer-position var '%s' is not a valid natural \
integer or non-empty marker."
                    position))))
        (unless with-range-check (throw :exit t))
        (let ((limit-set-func
               (lambda ()
                 (setq ptmin (point-min)
                       ptmax (point-max)))))
          (if (and without-restriction
                   (buffer-narrowed-p))
              (save-restriction
                (widen)
                (funcall limit-set-func))
            (funcall limit-set-func))
          (setq pt-p
                (and (natnump pt) (>= pt ptmin) (<= pt ptmax)))
          (unless pt-p
            (signal
             entropy/emacs-buffer-position-invalid-error-symbol
             (list
              (format "the buffer-position var '%s' is overflow for \
current buffer %S's %s."
                      position (current-buffer)
                      (if without-restriction "whole portion"
                        "visible portion"))))))
        ;; return true
        t))))

(defvar entropy/emacs-major-mode-incompatible-error-symbol
  'entropy/emacs-major-mode-incompatible-error)
(define-error entropy/emacs-major-mode-incompatible-error-symbol
  "Eemacs major mode incompatible" entropy/emacs-top-error-symbol)
(defun entropy/emacs-do-error-for-major-moe-incompatible
    (need-major-mode &optional cur-major-mode)
  (setq cur-major-mode (or cur-major-mode major-mode))
  (unless (eq cur-major-mode need-major-mode)
    (signal
     entropy/emacs-major-mode-incompatible-error-symbol
     (list
      (format "current major-mode '%s' is not '%s'"
              cur-major-mode need-major-mode)))))

(defvar entropy/emacs-emacs-version-incompatible-error-symbol
  'entropy/emacs-emacs-version-incompatible-error-signal)
(define-error entropy/emacs-emacs-version-incompatible-error-symbol
  "Eemacs emacs version incompatible" entropy/emacs-top-error-symbol)
(defun entropy/emacs-do-error-for-emacs-version-incompatible
    (emacs-version-request)
  (signal
   entropy/emacs-emacs-version-incompatible-error-symbol
   (list
    (format "emacs version '%s' request but current stands on '%s'"
            emacs-version-request emacs-version))))

(defvar entropy/emacs-package-version-incompatible-error-symbol
  'entropy/emacs-package-version-incompatible-error-signal)
(define-error entropy/emacs-package-version-incompatible-error-symbol
  "Eemacs package version incompatible" entropy/emacs-top-error-symbol)
(defun entropy/emacs-do-error-for-package-version-incompatible
    (package-name package-version-request)
  ;; the package version should obtained by `pkg-info-package-version'
  (require 'pkg-info)
  (signal
   entropy/emacs-package-version-incompatible-error-symbol
   (list
    (format "package '%s' version '%s' request but current stands on '%s'"
            package-name
            package-version-request
            (pkg-info-package-version package-name)))))

(defun entropy/emacs-api-restriction-display-warn (msg &optional do-error)
  (if do-error
      (error msg)
    (display-warning
     'eemacs-api-restriction
     msg
     :emergency)))

(defvar entropy/emacs-api-restriction-uniform-type-alist
  '((emacs-version-incompatible
     :default-detector
     (version< emacs-version entropy/emacs-lowest-emacs-version-requirement)
     :default-signal
     (signal
      entropy/emacs-emacs-version-incompatible-error-symbol
      (list
       (format "lowest emacs version '%s' request but current stands on '%s'"
               entropy/emacs-lowest-emacs-version-requirement
               emacs-version))))
    (package-version-incompatible
     :default-detector (ignore)
     :default-signal (error "package version incompatible"))
    ))
(defvar entropy/emacs-api-restriction-detection-log nil)
(cl-defmacro entropy/emacs--api-restriction-uniform
    (op-name
     type
     &rest body
     &key
     doc
     detector
     signal
     do-error
     &allow-other-keys)
  "Do BODY via an operation named by OP-NAME which refer to an checker
type.

TYPE must one of the car of element of
`entropy/emacs-api-restriction-uniform-type-alist' or will throw the
error.

EEMACS_MAINTENANCE: This macro is just used for eemacs maintainer in
where they can not guarantee some patches are valid for all
emacs-versions or purticular package version, thus for as, this macro
can throw the warning or error while the patches running on thus
situation and notice the eemacs maintainer to handle or update the
patches.

The DETETOR and SIGNAL is related to the checker instance and defaulty
provided by according to the TYPE defined by eemacs internally. Or use
the optional key slots as for manually specification and this is
suggested since the defaults are commonly used as fallback and for
demo usage.

- DOC: The details description for OP-NAME. (currently just used as a
  ready plan)

- DETECTOR:

  A form which return non-nil indicates to sign log with
  TYPE registed by OP-NAME.

- SIGNAL:

  When the DETECTOR return non-nil, use this form to `signal' the
  warning. It should have an error form to interrupt the BODY being
  Invoked via eval time since the error is ran inside a
  `condition-case' wrapper.

  We recommend to use the eemacs internal error API to do the signal:

  * `entropy/emacs-do-eemacs-top-error'
  * `entropy/emacs-do-error-for-emacs-version-incompatible'
  * `entropy/emacs-do-error-for-package-version-incompatible'

    Since they are tided with eemacs development.

- DO-ERROR:

  When non-nil Signal use `error' instead of warning to forcley
  corrupt the current emacs loop thread to avoid invoke any
  letter processes influenced by the BODY.

NOTE: all the key can be evaluated at run-time

"
  (declare (indent 2))
  (let ((body (entropy/emacs-get-plist-body body))
        (op-name-sym   (make-symbol "op-name"))
        (type-sym      (make-symbol "type"))
        (doc-sym       (make-symbol "docstring"))
        (detector-sym  (make-symbol "detector"))
        (signal-sym    (make-symbol "signal"))
        (default-sym   (make-symbol "default"))
        (warn-func-sym (make-symbol "warn-func"))
        (err-sym       (make-symbol "err-sym"))
        (err-data-sym  (make-symbol "err-data"))
        (do-body-p-sym (make-symbol "do-body-p")))
    `(let* ((,op-name-sym ,op-name)
            (,type-sym    ,type)
            (,doc-sym     ,doc)
            (,default-sym
              (alist-get ,type-sym
                         entropy/emacs-api-restriction-uniform-type-alist))
            (,detector-sym
             (or ,(if detector `(lambda nil ,detector))
                 (lambda nil (eval (plist-get ,default-sym :default-detector)))))
            (,signal-sym
             (or ,(if signal `(lambda nil ,signal))
                 (lambda nil (eval (plist-get ,default-sym :default-signal)))))
            ,err-sym ,err-data-sym ,do-body-p-sym
            (,warn-func-sym
             (lambda ()
               (entropy/emacs-api-restriction-display-warn
                (format "%s: [type: '%s' op-name: '%s' err-msg: \"%s\"], \
see `entropy/emacs-api-restriction-detection-log' for details."
                        ,err-sym ,type-sym ,op-name-sym ,err-data-sym)
                ,do-error))))
       (unless ,default-sym
         (entropy/emacs-do-eemacs-top-error
          (format "invalid eemacs api restriction type - %s"
                  ,type-sym)))
       (cond ((funcall ,detector-sym)
              (setq ,do-body-p-sym nil)
              (condition-case err
                  (funcall ,signal-sym)
                (t
                 (setq ,err-sym (car err)
                       ,err-data-sym (cdr err))
                 (add-to-list 'entropy/emacs-api-restriction-detection-log
                              (list
                               ,type-sym
                               :doc ,doc-sym
                               :operation-name ,op-name-sym
                               :error-sym ,err-sym
                               :error-data ,err-data-sym))
                 (funcall ,warn-func-sym))))
             (t
              (setq ,do-body-p-sym t)))
       (when ,do-body-p-sym
         ,@body))))

;; ** others
(defconst entropy/emacs-origin-load-path (copy-sequence load-path))

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
  "The org ob-* features list")

(defvar entropy/emacs-org-export-backends
  (delete nil
          `(ox-gfm
            ox-texinfo
            ox-odt
            ox-publish
            ox-org
            ox-md
            ox-man
            ox-latex
            ox-html
            ,(unless (< emacs-major-version 28)
               'ox-koma-letter)
            ox-icalendar
            ox-ascii
            ox-beamer
            ))
  "The org ox-* features list")

(defconst entropy/emacs-url-allowed-chars
  (progn
    (require 'url-util)
    (url--allowed-chars
     (let ((hl url-unreserved-chars))
       (dolist (el '(37 38 47 58 61 63))
         (push el hl))
       hl)))
  "Allowed chars ': / % & = ?' for url-protocal heads in function
`url-hexify-string'")

;; * provide
(provide 'entropy-emacs-defconst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
