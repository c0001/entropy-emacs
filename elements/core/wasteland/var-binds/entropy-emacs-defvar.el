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

(defvar entropy/emacs-package-common-start-after-hook nil
  "Hooks run after the entropy-emacs elisp packages initialized
 done while calling `entropy/emacs-package-common-start'.")

(defvar entropy/emacs-font-set-end-hook nil
  "Hooks run after `entropy/emacs-font-set-setfont-core'.")

(defvar entropy/emacs-top-keymap (make-sparse-keymap)
  "The top keymap for entropy-emacs holding the global
commands.")

(defvar entropy/emacs-top-key
  (if (display-graphic-p)
      (car entropy/emacs-top-prefix-key-cons)
    (cdr entropy/emacs-top-prefix-key-cons))
  "Top key for entropy-emacs global keybind for
`entropy/emacs-top-keymap'.

It is a string used for `kbd'.")

(defvar entropy/emacs-web-development-environment nil
  "Whether using enable web-development envrionment.

This variable is mainly for the judgement button for
`entropy/emacs-browse-url-function' for determined whether to using the
specific browser to visualize current file.")

(defvar entropy/emacs-window-center-integer 10
  "The number to split the `window-width' for calculating the
margin width.")

(defvar entropy/emacs-init-welcome-buffer-name  "*WELCOM TO ENTROPY-EMACS*"
  "Title of entropy-emacs initial dashboard buffer. ")

;; ** startup done refer
(defvar entropy/emacs-startup-done nil
  "while nil in startup procedure or t indicates the startup done
successfully. The meaning for startup done is that all procedure
within `entropy/emacs-startup-end-hook' are running done.")

(defun entropy/emacs-run-startup-end-hook ()
  "Run `entropy/emacs-startup-end-hook'.

Please only use this function for doing thus, do not run that
hook using `run-hooks' or any other methods or may cause some
messy."
  (run-hooks 'entropy/emacs-startup-end-hook)
  (entropy/emacs-echo-startup-done)
  (setq entropy/emacs-startup-done t))

(defvar entropy/emacs-pyim-has-initialized nil
  "variable indicate that pyim has started down for init.")

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

;; ** coworker refer
(defvar entropy/emacs-coworker-bin-host-path
  (expand-file-name "bin" entropy/emacs-coworker-host-root)
  "The coworker bin host.")

(defvar entropy/emacs-coworker-lib-host-root
  (expand-file-name "lib" entropy/emacs-coworker-host-root)
  "The default libs host root for coworker")

(defvar entropy/emacs-coworker-archive-host-root
  (expand-file-name "archive" entropy/emacs-coworker-host-root)
  "The default libs host root for coworker")

(defmacro entropy/emacs-with-coworker-host (newhost &rest body)
  (declare (indent defun))
  `(let ()
     (if (and (not (null ,newhost))
              (progn
                (make-directory ,newhost t)
                t))
         (let* ((entropy/emacs-coworker-host-root ,newhost)
                (entropy/emacs-coworker-bin-host-path
                 (expand-file-name "bin" entropy/emacs-coworker-host-root))
                (entropy/emacs-coworker-lib-host-root
                 (expand-file-name "lib" entropy/emacs-coworker-host-root))
                (entropy/emacs-coworker-archive-host-root
                 (expand-file-name "archive" entropy/emacs-coworker-host-root)))
           ,@body)
       ,@body)))

;; ** garbage collection

(defvar entropy/emacs-gc-threshold-basic 2000000
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

(defvar entropy/emacs-theme-load-after-hook-end-1 nil
  "Hooks run after hook `entropy/emacs-theme-load-after-hook'.")

(defvar entropy/emacs-theme-load-after-hook-end-2 nil
  "Hooks run after hook `entropy/emacs-theme-load-after-hook-end-1'.")

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
    (run-hooks 'entropy/emacs-theme-load-after-hook)
    (run-hooks 'entropy/emacs-theme-load-after-hook-end-1)
    (run-hooks 'entropy/emacs-theme-load-after-hook-end-2)))

(advice-add 'load-theme :around #'entropy/emacs-theme-load-advice)

;; ** modeline refer

(defvar entropy/emacs-mode-line-sticker ""
  "Sticker for current modeline style")

;; ** font refer
(defvar entropy/emacs-default-latin-font "Noto Mono"
  "Setting the default latin script font, when you enabled
`entropy/emacs-font-setting-enable'.

Defualt for \"Noto Mono\"")

(defvar entropy/emacs-default-symbol-font "Noto Sans Symbols"
  "Setting the default symbol script font, when you enabled
`entropy/emacs-font-setting-enable'.

Defualt for \"Noto Mono Symbols\"")

(defvar entropy/emacs-default-cjk-sc-font "Noto Sans Mono CJK SC"
  "Set the han(sc jp) script font, default was \"Noto Sans Mono CJK SC\"")

(defvar entropy/emacs-default-cjk-tc-font "Noto Sans Mono CJK TC"
  "Set the han(sc jp) script font, default was \"Noto Sans Mono CJK TC\"

By default `entropy/emacs-default-cjk-sc-font' preceded for this
setting to display chinese characters, unset 'sc' font setting to
enable this as default one.")

(defvar entropy/emacs-default-cjk-cn-font
  (or entropy/emacs-default-cjk-sc-font
      entropy/emacs-default-cjk-tc-font)
  "The default font for chinese lang-script, using
`entropy/emacs-default-cjk-tc-font' when
`entropy/emacs-font-chinese-type' was 'tc'.")

(defvar entropy/emacs-default-cjk-jp-font "Noto Sans Mono CJK JP"
  "Set the JP script font, default was \"Noto Sans Mono CJK JP\"")

(defvar entropy/emacs-default-cjk-kr-font "Noto Sans Mono CJK KR"
  "Set the hangul script font, default was \"Noto Sans Mono CJK KR\"")

(defvar entropy/emacs-default-extra-fonts nil
  "Extra fonts list.")

;; ** pdumper
(defvar entropy/emacs-pdumper-pre-lpth nil
  "The fully preserved `load-path' for pdumper session, this
variable is assigned while pdumper procedure triggered, append
emacs internal load path and the subdirs of
`entropy/emacs-ext-emacs-pkgel-get-pkgs-root' (i.e. the current
`package-user-dir' specified by what you settng for
`entropy/emacs-ext-elpkg-get-type').

Note that the entorpy-emacs just use the malpa-local type of
`entropy/emacs-ext-elpkg-get-type' to dumping as, thus the
submodules pre-loading is not supported for entropy-emacs
pdumper feature.")

;; ** daemon refer
(when (version< emacs-version "27")
  (defvar server-after-make-frame-hook nil
    "Hook run when the Emacs server creates a client frame.
The created frame is selected when the hook is called.

This hook was not native for current emacs version but back
patched for be compatible with 27.1.
")
  (defun entropy/emacs-server-make-frame-around-advice
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (unwind-protect
          (progn
            (run-hooks 'server-after-make-frame-hook)
            rtn)
        rtn)))
  (advice-add 'server-execute
              :around
              #'entropy/emacs-server-make-frame-around-advice))

(defvar entropy/emacs-daemon-server-after-make-frame-hook nil
  "Normal hooks run after a emacs daemon session create a client
frame.

For conventionally, you should inject any function into it by
using `entropy/emacs-with-daemon-make-frame-done', see its
docstring for details.
")

(defun entropy/emacs-daemon-create-daemon-client-plist ()
  "Create an entropy-emacs specified representation of a daemon client, a plist.

There's two key slots for this plist, ':frame' for theh current
main daemon client's frame object. ':gui-p' for whether thus is
`display-graphic-p'"
  (list :frame (selected-frame)
        :gui-p (display-graphic-p)))

(defvar entropy/emacs-daemon-main-client-indicator nil
  "Non-nil is a plist representation for current main daemon
client built by
`entropy/emacs-daemon-create-daemon-client-plist'.")

(defvar entropy/emacs-daemon-legal-clients nil
  "A list of legal daemon clients representation.")

(defun entropy/emacs-daemon-current-is-main-client (&optional frame)
  "Judge whether current frame is the frame of current daemon
main client."
  (let ((main-judge
         (eq (or frame (selected-frame))
             (plist-get
              entropy/emacs-daemon-main-client-indicator
              :frame))))
    main-judge))

(defun entropy/emacs-daemon-reset-main-client-indicator
    (orig-func &rest orig-args)
  "Reset `entropy/emacs-daemon-main-client-indicator' when the frame of
current daemon main client prepare to close, and the car of
`entropy/emacs-daemon-legal-clients' will be the assignment if
non-nil, i.e. the new main daemon client."
  (let (temp_var (frame (or (car orig-args) (selected-frame))))
    ;; pop out current daemon client from
    ;; `entropy/emacs-daemon-legal-clients'.
    (when (not (null entropy/emacs-daemon-legal-clients))
      (dolist (el entropy/emacs-daemon-legal-clients)
        (unless (eq frame (plist-get el :frame))
          (push el temp_var)))
      (setq entropy/emacs-daemon-legal-clients
            temp_var))
    ;; Reset `entropy/emacs-daemon-main-client-indicator'.
    (when (entropy/emacs-daemon-current-is-main-client frame)
      (setq entropy/emacs-daemon-main-client-indicator nil)
      (when (not (null entropy/emacs-daemon-legal-clients))
        (catch :exit
          (while entropy/emacs-daemon-legal-clients
            (if (frame-live-p
                 (plist-get
                  (car entropy/emacs-daemon-legal-clients) :frame))
                (progn
                  (setq entropy/emacs-daemon-main-client-indicator
                        (car entropy/emacs-daemon-legal-clients))
                  (throw :exit nil))
              (pop entropy/emacs-daemon-legal-clients))))))
    (apply orig-func orig-args)))

(advice-add 'delete-frame
            :around
            #'entropy/emacs-daemon-reset-main-client-indicator)

(defun entropy/emacs-daemon-client-initialize ()
  (when (daemonp)
    (set-frame-parameter (selected-frame) 'eemacs-current-frame-is-daemon-created t)
    ;; fix problem when main daemon client is dead
    (when entropy/emacs-daemon-main-client-indicator
      (unless (frame-live-p (plist-get entropy/emacs-daemon-main-client-indicator :frame))
        (setq entropy/emacs-daemon-main-client-indicator nil)))
    ;; fix problem when there's dead daemon clients in register.
    (when entropy/emacs-daemon-legal-clients
      (let (temp_var)
        (dolist (clts entropy/emacs-daemon-legal-clients)
          (when (frame-live-p (plist-get clts :frame))
            (push clts temp_var)))
        (setq entropy/emacs-daemon-legal-clients temp_var)))
    ;; startup daemon specification
    (if (or (not entropy/emacs-daemon-main-client-indicator)
            (eq (display-graphic-p)
                (plist-get entropy/emacs-daemon-main-client-indicator
                           :gui-p)))
        (let ((daemon-client-obj (entropy/emacs-daemon-create-daemon-client-plist)))
          (push daemon-client-obj entropy/emacs-daemon-legal-clients)
          (unless entropy/emacs-daemon-main-client-indicator
            (setq entropy/emacs-daemon-main-client-indicator
                  daemon-client-obj))
          (when entropy/emacs-daemon-server-after-make-frame-hook
            (run-hooks 'entropy/emacs-daemon-server-after-make-frame-hook)))
      (let ((frame (selected-frame)))
        (unless (entropy/emacs-daemon-current-is-main-client)
          (with-selected-frame frame
            (warn "
===========[*entropy emacs daemonwarn*]==============

Please close another '%s' daemon clients first before creating a
'%s' daemon client. Before did thus, some feature can not enable
properly!

That's because of the each daemon specification will just load
once for the main daemon client (the specified recognized daemon
client of entropy-emacs created by `emacsclient'), thus you
should close the origin main daemon client so that you can create
a new main daemon client instead.

For the main reason, emacs can not set frame-local variables,
that entropy-emacs forbidden create more than one daemon clients
at the same time, to prevent messing configuration up from
creating tty and gui daemon clients at same time, because tty and
gui session has huge sets of differents in entropy-emacs.

------------------------------------------------------
"
                  (if (plist-get entropy/emacs-daemon-main-client-indicator
                                 :gui-p)
                      "GUI"
                    "TTY")
                  (if (display-graphic-p)
                      "GUI"
                    "TTY"))))))))

(add-hook 'server-after-make-frame-hook
          #'entropy/emacs-daemon-client-initialize)



;; * provide
(provide 'entropy-emacs-defvar)
