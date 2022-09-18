;;; entropy-emacs-defvar.el --- entropy emacs internal variable declaration  -*- lexical-binding: t; -*-
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
(entropy/emacs-common-require-feature 'entropy-emacs-defconst)
(entropy/emacs-common-require-feature 'entropy-emacs-message)

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

;; ** individuals

(defvar entropy/emacs-package-common-start-after-hook nil
  "Hooks run after the entropy-emacs elisp packages initialized
done while calling `entropy/emacs-package-common-start'.")

(defvar entropy/emacs-font-set-end-hook nil
  "Hooks run after `entropy/emacs-font-set-setfont-core'.")

(defvar entropy/emacs-web-development-environment nil
  "Whether using enable web-development envrionment.

This varaible is a indication boolean variable who serves as a
on/off offer to some eemacs context to catch a order to treat a
procedure should be toggled into a branch which served on a sake
of web development environment.")

(defvar entropy/emacs-init-welcome-buffer-name  "*WELCOM TO ENTROPY-EMACS*"
  "Buffer name of entropy-emacs initial welcome displaying buffer.")

(defun entropy/emacs-source-directory (&rest _)
  "The `source-directory' of eemacs specification.

Return the emacs source directory when using eemacs in cloned git
repository and has submodule inited."
  (let ((dir (expand-file-name "annex/emacs-src"
                               entropy/emacs-user-emacs-directory)))
    (and (file-exists-p
          (expand-file-name "autogen.sh" dir))
         dir)))

(defvar entropy/emacs-read-extended-command-predicates
  '(
    ;; The default filter inspired by
    ;; `execute-extended-command-for-buffer', to obey the
    ;; `command-modes' predicate.
    (lambda (symbol buffer)
      (let ((modes (command-modes symbol))
            cur_mjmode
            cur_minmodes
            cur_allmodes)
        (if modes
            (progn
              (setq cur_mjmode (buffer-local-value 'major-mode buffer))
              (setq cur_minmodes (buffer-local-value 'local-minor-modes buffer))
              (setq cur_allmodes (cons cur_mjmode cur_minmodes))
              (catch :exit
                (dolist (mode modes)
                  (when (memq mode cur_allmodes)
                    (throw :exit t)))
                nil))
          t))))
  "A list of `read-extended-command-predicate' checking stop in
order while the first one return nil which used when emacs
version upper than 28.")
(defun entropy/emacs-read-extended-command-predicate-function
    (command buffer)
  "The eemacs `read-extended-command-predicate' which obey
`entropy/emacs-read-extended-command-predicates'."
  (let (_)
    (if entropy/emacs-read-extended-command-predicates
        (catch :exit
          (dolist (func entropy/emacs-read-extended-command-predicates)
            (unless (funcall func command buffer)
              (throw :exit nil)))
          t)
      t)))
(unless (< emacs-major-version 28)
  (setq read-extended-command-predicate
        'entropy/emacs-read-extended-command-predicate-function))

(defvar entropy/emacs-emacs-builtin-package-repack-flist
  (let (rtn
        fname
        (fname-exists-p
         (lambda (x)
           (or (file-exists-p (concat x ".el"))
               (file-exists-p (concat x ".elc"))))))
    (dolist (el `(("image-dired" "image-dired")))
      (let ((feature-str (car el))
            (files (cdr el)))
        (unless files
          (setq files (list feature-str)))
        (dolist (f files)
          (setq fname (expand-file-name
                       (format "%s/%s/%s"
                               feature-str
                               emacs-major-version
                               f)
                       entropy/emacs-site-lisp-path))
          (when (funcall fname-exists-p fname)
            (push fname rtn)))))
    rtn)
  "List of files who are eemacs modi ver. of the emacs internal package.

All the files are stripped their extensions so can be loaded by chosen
needed extension type.")

;; ** byte compile refer

(defvar entropy/emacs-session-in-byte-compile-emacs-core-p nil
  "whether current eemacs session is used to byte-compile eemacs
core.")

;; ** idle trigger
(defvar entropy/emacs-current-session-is-idle-p nil
  "The current emacs session idle signal, t for idle status
indicator, nil for otherwise.

Do not set this variable manually, it is assigned by
=entropy-emacs= automatically.

NOTE: this variable is useful to patch with some high performance
required function when concurrency ocation to reduce system lag
e.g. the modeline position indicator fresh function.")

(defvar entropy/emacs-session-idle-trigger-debug
  entropy/emacs-startup-with-Debug-p
  "Debug mode ran `entropy/emacs-session-idle-trigger-hook'."
  )

(defmacro entropy/emacs-session-idle--run-body-simple
    (name error-var &rest body)
  "Run BODY refer to a NAME and return the last form's result of BODY.

If `entropy/emacs-session-idle-trigger-debug' is unset, then
return nil when any error occurred in BODY and push the error msg
into ERROR-VAR."
  (declare (indent defun))
  `(if entropy/emacs-session-idle-trigger-debug
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error
        ;; escape byte-compile warning
        (eval-when-compile
          (unless (boundp ',error-var)
            (defvar ,error-var)))
        (push (cons ',name err) ,error-var)
        nil))))

(defvar entropy/emacs-session-idle-trigger-timer nil
  "The timer guard for run the
`entropy/emacs-session-idle-trigger-hook'.

NOTE: do not manually cancel it or be with your own risk.")

(defvar entropy/emacs-session-idle-trigger-hook nil
  "Hook ran immediately while current emacs-session is idle.

This hook must be injected while emacs is not idle e.g. injected
into it in `post-command-hook' or `pre-command-hook' and will be
cleaned up after the all the hooker ran up whenever what errors
happened.

NOTE: Any error occurred in this hook will be ignored unless
`entropy/emacs-session-idle-trigger-debug' is not NULL.")

(defvar entropy/emacs-session-idle-trigger-hook-error-list nil
  "The error log list of
`entropy/emacs-session-idle-trigger-hook', used when
`entropy/emacs-session-idle-trigger-debug' is null.")

(defvar entropy/emacs-current-session-idle-hook-ran-done nil
  "The indicator for indicate whether the
`entropy/emacs-session-idle-trigger-hook' has been ran out
finished done in which case the emacs is in an 'deep' sleep, t
for say done of thus and nil for otherwise.

NOTE: the indicator always be used in the idle time, since any
command will reset the idle indicator
`entropy/emacs-current-session-is-idle-p'.")

(defvar entropy/emacs-current-session-this-command-before-idle nil
  "The `this-command' run before the idle time of current emacs
session.

This variable exists because the `this-command' will be set nil
while idle time, but may hacks using
`entropy/emacs-run-at-idle-immediately' on non-idle procedure
indeed rely on the `this-command' value. Thus you can lexical
rebind the `this-command' to this variable while such hasck."
  )

(defvar entropy/emacs-current-session-last-command-before-idle nil
  "The `last-command' run before the idle time of current emacs
session.

Like `entropy/emacs-current-session-this-command-before-idle' but
for `last-command'.
"
  )

(defvar entropy/emacs-safe-idle-minimal-secs 0.1
  "The minimal idle timer SECS run with checking var
`entropy/emacs-current-session-is-idle-p' which indicates that any
specified timer function which would run with condition of thus
must setted with SECS larger than or equal of this value.")
(defvar entropy/emacs-idle-session-trigger-delay-clusters nil)

(defun entropy/emacs-get-idle-hook-refer-symbol-name-with-idle-second
    (type idle-sec)
  (pcase type
    ('hook-trigger-start-var
     (intern
      (format "entropy/emacs-current-session-is-idle-p-of-%s-sec"
              idle-sec)))
    ('hook-trigger-func
     (intern
      (format "entropy/emacs--set-idle-signal-of-%s-sec"
              idle-sec)))
    ('hook-trigger-hook-name
     (intern
      (format "entropy/emacs-session-idle-trigger-hook-of-%s-sec"
              idle-sec)))
    ('hook-trigger-timer-name
     (intern
      (format "entropy/emacs-session-idle-trigger-timer-of-%s-sec"
              idle-sec)))
    ('hook-trigger-done-var
     (intern
      (format "entropy/emacs-current-session-idle-hook-of-%s-sec-ran-done"
              idle-sec)))
    ('hook-trigger-error-list
     (intern
      (format "entropy/emacs-session-idle-trigger-hook-of-%s-sec-error-list"
              idle-sec)))
    (_
     (error "entropy/emacs-get-idle-hook-refer-symbol-name-with-idle-second: \
wrong type of type: %s"
            type))))

(defalias '__eemacs--get-idle-hook-refer-symbol-name
  'entropy/emacs-get-idle-hook-refer-symbol-name-with-idle-second)

(defun entropy/emacs--reset-idle-signal ()
  (when entropy/emacs-current-session-is-idle-p
    (let (
          ;; NOTE: protect as atomic manupulation
          (inhibit-quit t))
      ;; NOTE:
      ;; firstly we press the `inhibit-read-only' for preventing any
      ;; procedure enable this of which polluting next operation.
      (setq inhibit-read-only nil)
      (setq entropy/emacs-current-session-is-idle-p nil
            entropy/emacs-current-session-idle-hook-ran-done nil
            entropy/emacs-current-session-this-command-before-idle this-command
            entropy/emacs-current-session-last-command-before-idle last-command)
      (dolist (idle-sec entropy/emacs-idle-session-trigger-delay-clusters)
        (let ((hook-idle-trigger-start-varname
               (__eemacs--get-idle-hook-refer-symbol-name
                'hook-trigger-start-var idle-sec))
              (hook-idle-trigger-done-varname
               (__eemacs--get-idle-hook-refer-symbol-name
                'hook-trigger-done-var idle-sec)))
          (eval
           `(progn
              (when (bound-and-true-p ,hook-idle-trigger-start-varname)
                (setq ,hook-idle-trigger-start-varname nil))
              (when (bound-and-true-p ,hook-idle-trigger-done-varname)
                (setq ,hook-idle-trigger-done-varname nil)))))))))

(defun entropy/emacs--set-idle-signal ()
  (setq entropy/emacs-current-session-is-idle-p t)
  (if entropy/emacs-session-idle-trigger-debug
      (unwind-protect
          (run-hooks 'entropy/emacs-session-idle-trigger-hook)
        (setq entropy/emacs-session-idle-trigger-hook nil))
    (condition-case error
        (run-hooks 'entropy/emacs-session-idle-trigger-hook)
      (error
       (push error
             entropy/emacs-session-idle-trigger-hook-error-list)))
    (setq entropy/emacs-session-idle-trigger-hook nil))
  (setq entropy/emacs-current-session-idle-hook-ran-done t))
(defvar entropy/emacs-session-idle-trigger-timer--init-delay-sec
  (let ((idle-sec
         (- entropy/emacs-safe-idle-minimal-secs
            0.05)))
    (unless (> idle-sec 0)
      (error "[internal error]: entropy/emacs-safe-idle-minimal-secs is too small"
             ))
    idle-sec)
  "[interanal use] the initial idle delay seconds for eemacs top
idle trigger guard `entropy/emacs--set-idle-signal'"
  )
(setq entropy/emacs-session-idle-trigger-timer
      (run-with-idle-timer
       entropy/emacs-session-idle-trigger-timer--init-delay-sec
       t
       #'entropy/emacs--set-idle-signal))

;; we must ensure that this hook is first member of `pre-command-hook'
;; since the referred var will reflect any functions rest of thus.
;;
;; FIXME: does the depth set can reflect the local variable binding?
;; i.e. whether the local-binding of `pre-command-hook' is obey this
;; global order?
;;
;; EEMACS_MAINTENANCE: follow the FIXME seciton to hack any other hook
;; injecting before this one.
(add-hook 'pre-command-hook #'entropy/emacs--reset-idle-signal -100)
(defun entropy/emacs--idle-var-guard (_symbol newval _operation _where)
  (unless (null newval)
    (force-mode-line-update)))
(add-variable-watcher 'entropy/emacs-current-session-is-idle-p
                      #'entropy/emacs--idle-var-guard)

(defun entropy/emacs-def-idle-hook-refer-context (idle-sec)
  (let ((hook-idle-trigger-start-varname
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-start-var idle-sec))
        (func-idle-trigger-name
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-func idle-sec))
        (hook-idle-trigger-hookname
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-hook-name idle-sec))
        (hook-idle-trigger-timer-varname
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-timer-name idle-sec))
        (hook-idle-trigger-done-varname
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-done-var idle-sec))
        (hook-idle-trigger-error-list
         (__eemacs--get-idle-hook-refer-symbol-name
          'hook-trigger-error-list idle-sec)))
    (entropy/emacs-eval-with-lexical
     `(progn
        (defvar ,hook-idle-trigger-start-varname nil
          (format "Like `entropy/emacs-current-session-is-idle-p' \
but for idle with %ss." ,idle-sec))

        (defvar ,hook-idle-trigger-hookname nil
          (format "Like `entropy/emacs-session-idle-trigger-hook' \
but for idle with %ss." ,idle-sec))

        (defvar ,hook-idle-trigger-done-varname nil
          (format "Like `entropy/emacs-current-session-idle-hook-ran-done' \
but used for hook `%s'"
                  ',hook-idle-trigger-hookname))

        (defvar ,hook-idle-trigger-error-list nil
          (format "Like `entropy/emacs-session-idle-trigger-hook-error-list' \
but used for hook `%s'."
                  ',hook-idle-trigger-hookname))

        (defvar ,hook-idle-trigger-timer-varname nil
          (format "Like `entropy/emacs-session-idle-trigger-timer' \
but used for hook `%s'."
                  ',hook-idle-trigger-hookname))

        (defun ,func-idle-trigger-name (&rest _)
          ,(format "Like `entropy/emacs--set-idle-signal' but latency of %s seconds."
                   idle-sec)
          (setq ,hook-idle-trigger-start-varname t)
          (if entropy/emacs-session-idle-trigger-debug
              (unwind-protect
                  (run-hooks ',hook-idle-trigger-hookname)
                (setq ,hook-idle-trigger-hookname nil))
            (condition-case error
                (run-hooks ',hook-idle-trigger-hookname)
              (error
               (push error ,hook-idle-trigger-error-list)))
            (setq ,hook-idle-trigger-hookname nil))
          (setq ,hook-idle-trigger-done-varname t))

        (push ,idle-sec
              entropy/emacs-idle-session-trigger-delay-clusters)
        ))))

(defun entropy/emacs-current-session-is-idle
    (&optional idle-sec)
  "Judge whether eemacs is idle.

Optional IDEL-SEC indicate the idle seconds maybe logged by
`entropy/emacs-idle-session-trigger-delay-clusters' or using
`current-idle-time' to comparing thus. Defaultly return the
topset trigger status which equality the point just idle
indicator `entropy/emacs-current-session-is-idle-p'."
  (if (null idle-sec)
      entropy/emacs-current-session-is-idle-p
    (and
     entropy/emacs-current-session-is-idle-p
     (let ((var (__eemacs--get-idle-hook-refer-symbol-name
                 'hook-trigger-start-var idle-sec)))
       (if (boundp var)
           (symbol-value var)
         ;; instead of generate a new idle trigger instance, we just
         ;; use `current-idle-time' to judge the idleness duration to
         ;; reduce unnecessary timer resource for performance concern.
         (let ((curidtm (current-idle-time)))
           (and curidtm
                (>= (time-to-seconds curidtm)
                    idle-sec))))))))

(defun entropy/emacs-idle-session-trigger-hooks-prunning
    (name)
  "Remove NAME in all eemacs idle trigger hooks both of
`entropy/emacs-session-idle-trigger-hook' and the others based on
`entropy/emacs-idle-session-trigger-delay-clusters'"
  (setq entropy/emacs-session-idle-trigger-hook
        (delete name
                entropy/emacs-session-idle-trigger-hook))
  (dolist (idle-sec entropy/emacs-idle-session-trigger-delay-clusters)
    (let ((hook
           (__eemacs--get-idle-hook-refer-symbol-name
            'hook-trigger-hook-name idle-sec)))
      (when (entropy/emacs-bound-and-true-p hook)
        (set hook
             (delete
              name
              (symbol-value hook)))))))

(defmacro entropy/emacs-run-at-idle-immediately--append-hook
    (hook-name func)
  `(progn
     ;; escape byte-compile warning
     (eval-when-compile
       (unless (boundp ',hook-name)
         (defvar ,hook-name)))
     (entropy/emacs-nconc-with-setvar-use-rest ,hook-name
       (list ,func))))

(cl-defmacro entropy/emacs-run-at-idle-immediately
    (name &rest body
          &key
          ((:idle-when idle-p) t)
          ((:when should-run) t)
          which-hook
          current-buffer
          &allow-other-keys)
  "Run BODY defination as NAME while current emacs session ran
into idle status immediately or just run as progn while the
trigger of this macro is in idle time i.e. the
`entropy/emacs-current-session-is-idle-p' is non-nil.

Optional key slot support:

- idle-when:

  Form for evaluated to non-nil from judging wherther to injection to
  idle trigger or just directly do BODY like `progn'.

- when:

  Form for evaluated to judge whether should run the BODY.

- which-hook:

  Number(can be float) of seconds of to idicate which trigger
  hook to run. (must larger(or equal) than
  `entropy/emacs-safe-idle-minimal-secs')

  The set should be exactly did since this macro use it to found
  referred context.

- current-buffer:

  Form when evaluated non-nil before BODY to wrap BODY in
  `current-buffer' in the runtime, and ignore BODY when the
  buffer is not lived in runtime.

  When the form evaluated return a `bufferp' buffer, then use that
  buffer as `current-buffer'.

  When the form evaluated return a `windowp' window, then use that
  window's displayed buffer as `current-buffer'.

When `entropy/emacs-session-idle-trigger-debug' is null, then any
body is error ignored.

NOTE: each NAME in que is uniquely i.e. duplicated injection will
remove the oldest one and then injecting new one.

NOTE: this macro must expanded in an `lexical-binding' enabled
context."
  (when which-hook
    (unless (>= which-hook entropy/emacs-safe-idle-minimal-secs)
      (error "[%s] idle seconds is less than the safe value %s"
             name entropy/emacs-safe-idle-minimal-secs)))
  (let* ((hook
          (if which-hook
              (__eemacs--get-idle-hook-refer-symbol-name
               'hook-trigger-hook-name which-hook)
            'entropy/emacs-session-idle-trigger-hook))
         (hook-idle-sec
          (if which-hook
              which-hook
            entropy/emacs-session-idle-trigger-timer--init-delay-sec))
         (hook-error-list
          (if which-hook
              (__eemacs--get-idle-hook-refer-symbol-name
               'hook-trigger-error-list which-hook)
            'entropy/emacs-session-idle-trigger-hook-error-list))
         (hook-timer-func
          (if which-hook
              (__eemacs--get-idle-hook-refer-symbol-name
               'hook-trigger-func which-hook)
            'entropy/emacs--set-idle-signal))
         (hook-timer-varname
          (if which-hook
              (__eemacs--get-idle-hook-refer-symbol-name
               'hook-trigger-timer-name which-hook)
            'entropy/emacs-session-idle-trigger-timer))
         (with-buff-stick-form current-buffer)
         (body (entropy/emacs-get-plist-body body))
         (body-wrapper
          `(entropy/emacs-session-idle--run-body-simple
             ,name
             ,hook-error-list
             (when ,should-run
               ,@body)))
         (with-buff-stick-form-p-sym (make-symbol "buffer-stick-p-2"))
         (cur-buff-sym (make-symbol "current-used-buffer")))

    `(let (_)
       (if (or
            ;; forcely run without idle since is idle yet
            (bound-and-true-p entropy/emacs-current-session-is-idle-p)
            (entropy/emacs-session-idle--run-body-simple
              ,name
              ,hook-error-list
              (not ,idle-p)))

           (progn
             ,body-wrapper)

         ;; intitial the context when not defined
         (unless (fboundp ',hook-timer-func)
           (entropy/emacs-def-idle-hook-refer-context
            ,hook-idle-sec))

         ;; define the new hook function
         (let* ((,with-buff-stick-form-p-sym
                 ,(when with-buff-stick-form
                    `(entropy/emacs-session-idle--run-body-simple
                       ,name
                       ,hook-error-list
                       ,with-buff-stick-form)))
                (,cur-buff-sym
                 (when ,with-buff-stick-form-p-sym
                   (or (and (windowp ,with-buff-stick-form-p-sym)
                            (or (window-buffer ,with-buff-stick-form-p-sym)
                                t))
                       (or (and (bufferp ,with-buff-stick-form-p-sym)
                                ,with-buff-stick-form-p-sym)
                           (current-buffer))))))
           (defalias ',name
             (lambda (&rest _)
               (if ,with-buff-stick-form-p-sym
                   (when (and (bufferp ,cur-buff-sym)
                              (buffer-live-p ,cur-buff-sym))
                     (with-current-buffer ,cur-buff-sym
                       ,body-wrapper))
                 ,body-wrapper))))

         ;; remove all NAME in hooks
         (entropy/emacs-idle-session-trigger-hooks-prunning
          ',name)
         ;; We should append the hook to the tail since follow the time
         ;; order.
         (entropy/emacs-run-at-idle-immediately--append-hook
          ,hook #',name)

         ;; Intial the trigger timer when not bound
         (unless (bound-and-true-p ,hook-timer-varname)
           ;; escape byte-compile warning
           (eval-when-compile
             (unless (boundp ',hook-timer-varname)
               (defvar ,hook-timer-varname)))
           (setq ,hook-timer-varname
                 (run-with-idle-timer
                  ,hook-idle-sec
                  t ',hook-timer-func)))
         ))))

;; ** eemacs top keymap refer
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

;; ** startup done refer
(defvar entropy/emacs-startup-done nil
  "while nil in startup procedure or t indicates the startup done
successfully. The meaning for startup done is that all procedure
within `entropy/emacs-startup-end-hook' are running done.")

(defvar entropy/emacs-run-startup-duration nil
  "The object represent eemacs startup time duration")

(defvar entropy/emacs-run-startup-config-load-init-timestamp nil
  "Time-stamp eemacs init load for all configs")
(defvar entropy/emacs-run-startup-trail-hooks-init-timestamp nil
  "Time-stamp eemacs init run trail hook")
(defvar entropy/emacs-run-startup-pdumper-hooks-init-timestamp nil
  "Time-stamp eemacs init run `entropy/emacs-pdumper-load-hook'")
(defvar entropy/emacs-run-startup-trail-hooks-init-done-timestamp nil
  "Time-stamp eemacs ran trail hook done")
(defvar entropy/emacs-package-initialize-init-timestamp nil
  "Time-stamp before run `package-initialize'")
(defvar entropy/emacs-package-initialize-done-timestamp nil
  "Time-stamp after ran `package-initialize'")

(declare-function benchmark-init/deactivate "ext:benchmark-init")
(declare-function benchmark-init/show-durations-tabulated "ext:benchmark-init-modes")
(declare-function benchmark-init/show-durations-tree "ext:benchmark-init-modes")
(declare-function benchmark-init/activate "ext:benchmark-init")
(defun entropy/emacs-run-startup-end-hook ()
  "Run `entropy/emacs-startup-end-hook' just before mark
`entropy/emacs-startup-done' as done.

   NOTE:
   Please only use this function for doing thus, do not run that
   hook using `run-hooks' or any other methods or may cause some
   messy.

And then make statistics of the startup duration to
`entropy/emacs-run-startup-duration'.

After that we run `entropy/emacs-after-startup-hook' to
initialize the default non-lazy configs.
"
  (entropy/emacs-message-do-message
   "==================== eemacs trail hooks ran out ===================="
   :force-message-while-eemacs-init t)
  (run-hooks 'entropy/emacs-startup-end-hook)
  (entropy/emacs-message-do-message
   "==================== eemacs end hooks ran out ===================="
   :force-message-while-eemacs-init t)
  (setq entropy/emacs-startup-done t)
  ;; NOTE: we must hide popup after set `entropy/emacs-startup-done'
  ;; since its API commentary.
  (entropy/emacs-message-hide-popup t)
  ;; ========== startup done hints
  (let* ((entropy/emacs-message-non-popup t)
         (this-time (current-time))
         ;; -----------------------
         (emacs-pre-time:before
          (entropy/emacs-get-before-init-time))
         (emacs-pre-time:after
          (entropy/emacs-get-after-init-time))
         (emacs-pre-time
          (string-to-number
           (format "%.2f"
                   (float-time
                    (time-subtract emacs-pre-time:after
                                   emacs-pre-time:before)))))
         ;; -------------------------
         (pkg-init-time:before entropy/emacs-package-initialize-init-timestamp)
         (pkg-init-time:after  entropy/emacs-package-initialize-done-timestamp)
         (pkg-init-time
          (string-to-number
           (format "%.2f"
                   (float-time
                    (time-subtract pkg-init-time:after
                                   pkg-init-time:before)))))
         ;; -------------------------
         (real-startup-time:before (entropy/emacs-get-after-init-time))
         (real-startup-time:after this-time)
         (real-startup-time
          (string-to-number
           (format "%.2f"
                   (float-time
                    (time-subtract real-startup-time:after
                                   real-startup-time:before)))))
         ;; --------------------------
         (eemacs-init-time:before entropy/emacs-run-startup-top-init-timestamp)
         (eemacs-init-time:after  entropy/emacs-run-startup-config-load-init-timestamp)
         (eemacs-init-time
          (string-to-number
           (format "%.2f"
                   (float-time (time-subtract
                                eemacs-init-time:after
                                eemacs-init-time:before
                                )))))
         ;; -------------------------
         (config-load-time:before entropy/emacs-run-startup-config-load-init-timestamp)
         (config-load-time:after  entropy/emacs-run-startup-trail-hooks-init-timestamp)
         (config-load-time
          (string-to-number
           (format "%.2f"
                   (float-time (time-subtract
                                config-load-time:after
                                config-load-time:before)))))
         ;; ----------------------------
         (lazy-hook-time:before entropy/emacs-run-startup-trail-hooks-init-timestamp)
         (lazy-hook-time:after  entropy/emacs-run-startup-trail-hooks-init-done-timestamp)
         (lazy-hook-time
          (string-to-number
           (format "%.2f"
                   (+
                    (float-time (time-subtract
                                 lazy-hook-time:after
                                 lazy-hook-time:before))
                    (or (when entropy/emacs-run-startup-pdumper-hooks-init-timestamp
                          (float-time
                           (time-subtract
                            this-time
                            entropy/emacs-run-startup-pdumper-hooks-init-timestamp)))
                        0)))))
         (base-str "Inited")
         (msgstr
          (entropy/emacs-message--do-message-ansi-apply
           "%s (using %s seconds \
/ %s for package initialization \
/ %s for do eemacs-tentacles-load \
/ %s for run eemacs-trail-hook \
/ %s for eemacs-init \
/ %s for Emacs-init)"
           (green base-str)
           (yellow real-startup-time)
           (cyan pkg-init-time)
           (yellow config-load-time)
           (yellow lazy-hook-time)
           (yellow eemacs-init-time)
           (red emacs-pre-time)
           )))
    (setq entropy/emacs-run-startup-duration
          `((eemacs-startup :duration ,real-startup-time)
            (pakage-initialize
             :before ,pkg-init-time:before
             :after ,pkg-init-time:after
             :duration ,pkg-init-time)
            (emacs-init
             :before ,emacs-pre-time:before
             :after ,emacs-pre-time:after
             :duration ,emacs-pre-time)
            (eemacs-init
             :before ,eemacs-init-time:before
             :after ,eemacs-init-time:after
             :duration ,eemacs-init-time)
            (eemacs-config-load
             :before ,config-load-time:before
             :after ,config-load-time:after
             :duration ,config-load-time)
            (eemacs-trail-hook
             :before ,lazy-hook-time:before
             :after ,lazy-hook-time:after
             :extra ,(if entropy/emacs-run-startup-pdumper-hooks-init-timestamp
                         (list :before this-time
                               :after entropy/emacs-run-startup-pdumper-hooks-init-timestamp
                               ))
             :duratioin ,lazy-hook-time)))
    (unless (daemonp)
      (with-selected-window (selected-window)
        (with-current-buffer (window-buffer)
          (setq-local
           mode-line-format
           msgstr))))
    (entropy/emacs-message-do-message
     "%s --- %s %s"
     (bold base-str)
     (magenta "Happy hacking")
     "('C-h v entropy/emacs-run-startup-duration' see startup time)"
     ))
  ;; ========== Debug facilities
  (when entropy/emacs-startup-benchmark-init
    (benchmark-init/show-durations-tree)
    (split-window-below)
    (benchmark-init/show-durations-tabulated)
    (benchmark-init/deactivate))
  ;; ========== ran `entropy/emacs-after-startup-hook'
  (run-hooks 'entropy/emacs-after-startup-hook)
  )

(defvar entropy/emacs-IME-specs-initialized nil
  "Variable indicate that eemacs IME specified features has started
down for init.

When eq 't' indicates that the whole pyim initialization process
ran with fully successful, or any non-nil value is a warning
status existed

When eq 'nil' that say the initialization is going on.")

;; ** coworker refer
(defvar entropy/emacs-coworker-bin-host-path
  (expand-file-name "bin" entropy/emacs-coworker-host-root)
  "The default bins host root for coworker based on
`entropy/emacs-coworker-host-root'.")

(defvar entropy/emacs-coworker-lib-host-root
  (expand-file-name "lib" entropy/emacs-coworker-host-root)
  "The default libs host root for coworker based on
`entropy/emacs-coworker-host-root'.")

(defvar entropy/emacs-coworker-archive-host-root
  (expand-file-name "archive" entropy/emacs-coworker-host-root)
  "The default libs host root for coworker based on
`entropy/emacs-coworker-host-root'.")

(defmacro entropy/emacs-with-coworker-host (newhost &rest body)
  "Do BODY within the newest host root path specification of
`entropy/emacs-coworker-host-root' temporally with NEWHOST, in
which case variable: `entropy/emacs-coworker-bin-host-path',
`entropy/emacs-coworker-lib-host-root',
`entropy/emacs-coworker-archive-host-root' will be temporally set
also."
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

;; ** codeserver refer
(defvar entropy/emacs-codeserver-lsp-mode-extra-clients nil
  "Extra `lsp-mode' compatible language server feature.

Each element of this list is a feature name of which emacs
extansion was a third-party extension for `lsp-mode'.

All of them will be required by `lsp-mode' be its fistly starting
in current emacs-session.

NOTE: This variable used for `lsp-mode' be customized for
replacing extra clients of internal officially registered ones or
add the missing ones e.g. we may want to take high priority for
`lsp-python-ms' with the internal registerred `lsp-pyls' to get
more powerful features and add `lsp-java' to let `lsp-mode'
support java support while it doesn't place it internally.")


;; ** window refer

;; *** delete other window extension
(defvar entropy/emacs-delete-other-windows-before-hook nil
  "The hook run before `delete-other-windows', this hook was
invoked by =entropy-emacs= so that the efficiency just worked
after =entropy-emacs= has loading done i.e. while
`entropy/emacs-startup-done' is non-nil.")

(defvar entropy/emacs-delete-other-windows-after-hook nil
  "The hook run after `delete-other-windows', this hook was
invoked by =entropy-emacs= so that the efficiency just worked
after =entropy-emacs= has loading done i.e. while
`entropy/emacs-startup-done' is non-nil.")

(defvar entropy/emacs-origin-window-configuration-before-delete-other-windows nil
  "The 'window-configuration' before operating
`delete-other-windows'. this variable was invoked by
=entropy-emacs= so that the efficiency just worked after
=entropy-emacs= has loading done i.e. while
`entropy/emacs-startup-done' is non-nil.")

(defvar entropy/emacs-delete-other-windows-ignore-pms-predicates nil
  "List of functions called when judgeing whether ignore all
window-parameters to `delete-other-windows', similar to use
`delete-other-windows-internal'.

Each function can be either a function or a form (i.e. not match
`functionp'), and each call to them is wrapped with
`ignore-errors' to prevent the messy by. Each function get an
argument the WINDOW of `delete-other-windows' optional slot.")

(defun entropy/emacs-defvar--delete-other-windows-around-advice
    (orig-func &rest orig-args)
  "Delete other window eemacs specification

This function is a around advice for `delete-other-windows' and
externally add below features:

* hooks:
- `entropy/emacs-delete-other-windows-before-hook'
- `entropy/emacs-delete-other-windows-after-hook'

* predicates: `entropy/emacs-delete-other-windows-ignore-pms-predicates'
* Constant variable: `entropy/emacs-origin-window-configuration-before-delete-other-windows'
"
  (let* (this-rtn
         (window (window-normalize-window (car orig-args)))
         (ignore-wmpmts-p
          (and (not (null entropy/emacs-delete-other-windows-ignore-pms-predicates))
               (catch :exit
                 (let (judge)
                   (dolist (func entropy/emacs-delete-other-windows-ignore-pms-predicates)
                     (ignore-errors
                       (if (functionp func)
                           (setq judge (funcall func window))
                         (setq judge (eval func))))
                     (when judge
                       (throw :exit judge)))
                   judge))))
         (ignore-window-parameters ignore-wmpmts-p))
    (run-hooks 'entropy/emacs-delete-other-windows-before-hook)
    (setq entropy/emacs-origin-window-configuration-before-delete-other-windows
          (current-window-configuration))
    (setq this-rtn (apply orig-func orig-args))
    (run-hooks 'entropy/emacs-delete-other-windows-after-hook)
    this-rtn))
(advice-add 'delete-other-windows
            :around
            #'entropy/emacs-defvar--delete-other-windows-around-advice)

;; *** window center mode

(defvar entropy/emacs-window-force-inhibit-auto-center nil
  "Force inhibit `entropy/emacs-window-center-mode' auto mode even
if `entropy/emacs-window-center-auto-mode-enable-p' is non-nil.")

(defvar entropy/emacs-window-auto-center-require-enable-p nil
  "`let' non-nil binding idicator for indicate that the env is
derived from the operation of `entropy/emacs-window-center-mode'
auto mode.")

(defun entropy/emacs-window-center-calc-margin-width (&optional window)
  "Calcualte the pre centerred window margin column width for
WINDOW (defaultly by `selected-window') with assumed that WINDOW
has no margin set rely on
`entropy/emacs-window-no-margin-column-width'."
  (let ((window (or window (selected-window))))
    (round
     (/ (entropy/emacs-window-no-margin-column-width window)
        (float entropy/emacs-window-center-integer)))))

(defun entropy/emacs-window-center-calc-body-width (&optional window)
  "Calcualte the pre centerred window body column width for
WINDOW (defaultly by `selected-window') with assumed that WINDOW
has no margin set rely on
`entropy/emacs-window-no-margin-column-width'.

Strictly say that this function is compatible with
`entropy/emacs-window-center-calc-margin-width' which say that
the return plus 2 times of the margin width calculated by that is
equalized with
`entropy/emacs-window-no-margin-column-width'."
  (let ((window (or window (selected-window))))
    (- (entropy/emacs-window-no-margin-column-width window)
       (* 2 (entropy/emacs-window-center-calc-margin-width)))))

(defun entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge
    ()
  "Judge whether current `entropy/emacs-window-center-mode'
auto-mode can be triggerred in generalized meaning in which case
use `entropy/emacs-wc-window-auto-center-mode-turn-on-judger' for
more accurated.

Return t or nil for commonly manner."
  (and entropy/emacs-window-center-auto-mode-enable-p
       (not entropy/emacs-window-force-inhibit-auto-center)
       t))

(defvar entropy/emacs-window-center-enable-before-hook nil
  "The hook run before `entropy/emacs-window-center-mode' enable")
(defvar entropy/emacs-window-center-enable-after-hook nil
  "The hook run after `entropy/emacs-window-center-mode' enabled")
(defvar entropy/emacs-window-center-disable-before-hook nil
  "The hook run before `entropy/emacs-window-center-mode' disable")
(defvar entropy/emacs-window-center-disable-after-hook nil
  "The hook run after `entropy/emacs-window-center-mode' disabled")

(defvar entropy/emacs-window-center-mode-turn-on-filter-list
  '((lambda (buffer-or-name)
      (with-current-buffer buffer-or-name
        (catch :exit
          (let* ((buff-name (if (bufferp buffer-or-name)
                                (buffer-name buffer-or-name)
                              buffer-or-name)))
            (when (or (string-equal entropy/emacs-init-welcome-buffer-name buff-name)
                      (and (bound-and-true-p dashboard-buffer-name)
                           (string-equal dashboard-buffer-name buff-name)))
              (throw :exit 'in-eemacs-welcom-buffer))
            (when (string-equal " *LV*" buff-name)
              (throw :exit 'lv-window))
            (when (and (bound-and-true-p transient--buffer-name)
                       (string-equal transient--buffer-name buff-name))
              (throw :exit 'transient-window))
            (when (and (bound-and-true-p which-key-buffer-name)
                       (string-equal which-key-buffer-name buff-name))
              (throw :exit 'whickey-window))
            (when (or (and (bound-and-true-p image-dired-display-image-buffer)
                           (string-equal image-dired-display-image-buffer buff-name))
                      (and (bound-and-true-p image-dired-thumbnail-buffer)
                           (string-equal image-dired-thumbnail-buffer buff-name)))
              (throw :exit 'image-dired-refer-buffer))
            (when (string-match-p "^\\*Messages\\*" buff-name)
              (throw :exit 'message-buffer))
            (when (string-match-p "^ \\*Echo Area" buff-name)
              (throw :exit 'echo-area-buffer))
            (when (string-match-p "magit.*:" buff-name)
              (throw :exit 'in-magit-refer-buffer))
            (when (string-match-p "magit-.*-mode" (format "%s" major-mode))
              (throw :exit 'in-magit-refer-modes))
            (when (or (member major-mode '(shell-mode vterm-mode term-mode eshell-mode))
                      (string-match-p "*eemacs-\\(eshell\\|vterm\\|shell\\|ansiterm\\)-[0-9]+*"
                                      buff-name))
              (throw :exit 'in-term-mode))
            )
          t))))
  "List of filter function with one argument BUFFER-OR-NAME, and
always return non-nil, t for should enable
`entropy/emacs-window-center-mode', use other symbol to
indicate false meaning (see the default function listed in this
list for details).")

(defun entropy/emacs-window-center-mode-turn-on-judger
    (buffer-or-name)
  "Run the `entropy/emacs-window-center-mode-turn-on-filter-list' one
by one, return t when all filters matched, return other symbol to
indicate the false meaning."
  (or
   (catch :exit
     (progn
       (dolist (func entropy/emacs-window-center-mode-turn-on-filter-list)
         (let (rtn)
           (unless (eq (setq rtn (funcall func buffer-or-name)) t)
             (throw :exit rtn))))
       t))
   (error "window center window judgements return error")))

(defvar entropy/emacs-window-auto-center-mode-turn-on-filter-list
  '(
    ;; ---------- default filter
    (lambda (buffer-or-name)
      (catch :exit
        (let* ((win (get-buffer-window buffer-or-name))
               (win-live-p (and (windowp win)
                                (window-live-p win)))
               (buff-buff (get-buffer buffer-or-name))
               (buff-name (when-let ((buff (get-buffer buffer-or-name)))
                            (buffer-name buff))))
          (unless (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
            (throw :exit 'auto-center-base-condition-not-satisfied))
          (unless (not (minibufferp buff-buff))
            (throw :exit 'minibufferp))
          (unless win-live-p
            (throw :exit 'no-live-win))
          (unless (or (entropy/emacs-frame-is-fullscreen-p)
                      (entropy/emacs-frame-is-maximized-p))
            (when (and (not noninteractive) (not (display-graphic-p)))
              ;; forcely disable auto window center mode in cli
              ;; session since we can not judge the maximized status
              ;; of the term.
              (setq entropy/emacs-window-center-auto-mode-enable-p nil)
              (throw :exit 'in-cli-session))
            (throw :exit 'frame-not-fullscreen))
          (with-current-buffer buffer-or-name
            ;; should not auto center window for elfeed search buffer
            ;; since it has long title line.
            (when (eq major-mode 'elfeed-search-mode)
              (throw :exit 'elfeed-search-mode)))
          (unless (entropy/emacs-window-horizontally-fill-frame-p win)
            (throw :exit 'window-not-horizontally-fill-frame))
          (when (string-match-p "^\\*\\(Proced\\|Process List\\).*$"
                                buff-name)
            (throw :exit 'process-buffer-detected))
          (when (string= "*eemacs-minor-tools/print-var*" buff-name)
            (throw :exit 'eemacs-var-print-buffer))
          t)))

    ;; ---------- Others ...

    )
  "Like `entropy/emacs-window-center-mode-turn-on-filter-list', a list of
filter function with one argument BUFFER-OR-NAME, and always
return non-nil, t for should enable
`entropy/emacs-window-center-mode' in auto mode (see
`entropy/emacs-window-center-auto-mode-enable-p' for details),
use other symbol to indicate false meaning (see the default
function listed in this list for details).")

(defun entropy/emacs-wc-window-auto-center-mode-turn-on-judger
    (buffer-or-name)
  "Run the `entropy/emacs-window-auto-center-mode-turn-on-filter-list' one
by one, return t when all filters matched, return other symbol to
indicate the false meaning."
  (or
   (catch :exit
     (progn
       (dolist (func entropy/emacs-window-auto-center-mode-turn-on-filter-list)
         (let (rtn)
           (unless (eq (setq rtn (funcall func buffer-or-name)) t)
             (throw :exit rtn))))
       ;; append non-auto filters
       (let ((rtn (entropy/emacs-window-center-mode-turn-on-judger buffer-or-name)))
         (unless (eq rtn t)
           (throw :exit rtn)))
       t))
   (error "Auto window center window judgements return error")))

(defun entropy/emacs-window-center-emulate-window-column-width-as-enabled
    (&optional use-filter use-window)
  "caculate `window-width' by emulating when
`entropy/emacs-window-center-mode' init up in window USE-WINDOW or the
`selected-window' by default.

It's as same as `entropy/emacs-window-center-calc-body-width'
expect that:

If USE-FILTER is non-nil, apply
`entropy/emacs-wc-window-auto-center-mode-turn-on-judger' when
`entropy/emacs-window-center-auto-mode-enable-p' is enabled or
`entropy/emacs-window-center-mode-turn-on-judger' to pre-judge whether
use the emulated result or use the `window-width'."
  (let* ((use-window (or use-window (selected-window)))
         (use-buff (window-buffer use-window))
         use-width-func filter-judge-p)
    ;; error when in a splitted root window
    (unless use-buff
      (error "Window %s has no buffer displayed in it!"))
    (setq use-width-func
          (lambda ()
            (entropy/emacs-window-center-calc-body-width use-window)))
    (cond
     (use-filter
      (setq filter-judge-p
            (funcall
             (if (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
                 'entropy/emacs-wc-window-auto-center-mode-turn-on-judger
               'entropy/emacs-window-center-mode-turn-on-judger)
             use-buff))
      (if (eq filter-judge-p t)
          (funcall use-width-func)
        (window-width use-window)))
     (t
      (funcall use-width-func)))))

;; ** dired refer

;; *** `dired-goto-file' extending
(defvar entropy/emacs-dired-goto-file-extend-processors nil
  "Extra processors applied for `dired-goto-file' when its return
is nil for the sake of find the line matching the file when
origin process can not did as. Did as extending role.

Escape when first processor return non-nil.

Each processor must accept the same arguments and must return as the
same type as what described in `dired-goto-file'.

NOTE:

Using `entropy/emacs-dired-goto-file-extend-processors-regist' to
regist your own extending function, do not use `add-to-list',
`push' although its an list. Since we do other things
internally.")

(defun entropy/emacs-dired-goto-file-extend-processors-regist (func)
  "Regist FUNC to
`entropy/emacs-dired-goto-file-extend-processors'."
  (unless (member func entropy/emacs-dired-goto-file-extend-processors)
    (setq entropy/emacs-dired-goto-file-extend-processors
          (append entropy/emacs-dired-goto-file-extend-processors
                  (list func)))))

(defun entropy/emacs-dired-goto-file-with-extended-processors
    (orig-func &rest orig-args)
  "Extending the ability of `dired-goto-file' using
`entropy/emacs-dired-goto-file-extend-processors' (see it for
details)."
  (let ((file (car orig-args))
        (rtn (apply orig-func orig-args)))
    (catch :exit
      (when rtn
        (throw :exit rtn))
      (dolist (func entropy/emacs-dired-goto-file-extend-processors)
        (when (setq rtn (funcall func file))
          (throw :exit rtn)))
      nil)))
(advice-add 'dired-goto-file :around
            #'entropy/emacs-dired-goto-file-with-extended-processors)

(defun entropy/emacs-dired-goto-file-use-re-search-forward (file)
  "The extending to `dired-goto-file' using `re-search-forward'
with the base file name of FILE to speedup in most of cases."
  (let ((fbname (file-name-nondirectory (directory-file-name file)))
        pt cur-fname)
    (save-excursion
      (goto-char (point-min))
      (catch :exit
        (while (re-search-forward (regexp-quote fbname) nil t)
          (setq cur-fname (ignore-errors (dired-get-filename)))
          (when (and cur-fname
                     (string-equal cur-fname file))
            (dired-move-to-filename)
            (setq pt (point))
            (throw :exit t)))))
    (if pt
        (progn
          (goto-char pt)
          pt)
      nil)))
(entropy/emacs-dired-goto-file-extend-processors-regist
 #'entropy/emacs-dired-goto-file-use-re-search-forward)

;; ** garbage collection refer

(defvar entropy/emacs-gc-threshold-basic (* 1 1024 1024)
  "The basic thredshold for the growth for `gc-cons-threshold'")

(defvar entropy/emacs-gc-percentage-basic 0.3
  "The basic portion for the growth for `gc-cons-percentage'")

(defvar entropy/emacs-garbage-collect-idle-timer nil
  "The garbage collection idle timer for entropy-emacs.")

(defvar entropy/emacs-garbage-collect-restrict-commands nil
  "List of commands (i.e. interactive functions) that need to
restrict their runtime `gc-cons-threshold' while invoked in any
keymap.")

;; ** theme refer

(defvar entropy/emacs-theme-sticker nil
  "Current theme used for this session.

It's a symbol of the theme feature, auto-assigned within
eemacs-context, you shouldn't modify it manually.

Also see `entropy/emacs-theme-load-advice'. ")

(defvar entropy/emacs-theme-load-before-hook-head-1 nil
  "Hook runs before `entropy/emacs-theme-load-before-hook'.")

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

(defvar entropy/emacs-theme-load-after-hook-head-1 nil
  "Hooks run before hook `entropy/emacs-theme-load-after-hook'.")

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
  (run-hooks 'entropy/emacs-theme-load-before-hook-head-1)
  (run-hooks 'entropy/emacs-theme-load-before-hook)
  (progn
    (when entropy/emacs-theme-sticker
      ;; Disable origin theme before load a new theme to prevent from
      ;; messing up the new one
      (when (member entropy/emacs-theme-sticker custom-enabled-themes)
        (disable-theme entropy/emacs-theme-sticker)))
    (let ((theme-load (car args)))
      (setq entropy/emacs-theme-sticker theme-load)
      (apply old-func args))
    (run-hooks 'entropy/emacs-theme-load-after-hook-head-1)
    (run-hooks 'entropy/emacs-theme-load-after-hook)
    (run-hooks 'entropy/emacs-theme-load-after-hook-end-1)
    (run-hooks 'entropy/emacs-theme-load-after-hook-end-2)))

(advice-add 'load-theme :around #'entropy/emacs-theme-load-advice)

(defvar entropy/emacs-solaire-mode-extra-buffer-filters nil
  "Extra buffer filter functions which return non-nil indicating
the buffer should not enable `entropy/emacs-solaire-mode'.

Each function take only one arg the BUFFER and return immediatly
when one of the filters return non-nil.")

(defun entropy/emacs-solaire-mode-run-extra-buffer-filters (buffer)
  "The predicate for
`entropy/emacs-solaire-mode-extra-buffer-filters'."
  (catch :exit
    (dolist (func entropy/emacs-solaire-mode-extra-buffer-filters)
      (when (funcall func buffer)
        (throw :exit t)))))

;; ** modeline refer

(defvar entropy/emacs-mode-line-sticker ""
  "Sticker for current modeline style, used for eemacs context to
recognize which mode line type is used for now in current emacs
session. It's a string, such as
\"doom\",\"spaceline\",\"powerline\" etc.

For conventionally, we gives each pre-defined name-reflect to its
mode-line feature, to enhance the namespace unified ability and
be good for maintenance:

- \"doom\"      <---> feature: `doom-modeline'
- \"spaceline\" <---> feature: `spaceline'
- \"powerline\" <---> feature: `powerline'.")

(defvar entropy/emacs-modeline-default-modeline-formt
  (copy-tree mode-line-format)
  "The emacs default `mode-line-format'.")

(defvar entropy/emacs-modeline-cases-of-spec-modeline
  '(
    ;; special major-mode enabled in `current-buffer'
    (member major-mode '(treemacs-mode
                         neotree-mode
                         mpc-mode
                         mpc-tagbrowser-mode
                         mpc-songs-mode
                         mpc-status-mode
                         mpc-tagbrowser-dir-mode))
    ;; special minor-mode enabled in current-buffer
    (or (bound-and-true-p entropy-shellpop-mode))
    )
  "list of predicates for compute to judge whether in this case
the `mode-line-format' is special when the return is non-nil.

Each predicate can be a form or a function name.")

;; ** font refer
(defvar entropy/emacs-fontsets-used-latin-font "Noto Mono"
  "The latin script font used in eemacs context, set
automatically followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-used-symbol-font "Noto Sans Symbols"
  "The symbol script font name in eemacs context, set
automatically followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-used-cjk-sc-font "Noto Sans Mono CJK SC"
  "The han(sc jp) script font in eemacs context, set
automatically followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-used-cjk-tc-font "Noto Sans Mono CJK TC"
  "The han(tc jp) script font in eemacs context, set
automatically followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-default-cjk-cn-font
  (or entropy/emacs-fontsets-used-cjk-sc-font
      entropy/emacs-fontsets-used-cjk-tc-font)
  "The font for chinese lang-script in eemacs context, set
atutomatically followed by `entropy/emacs-font-setting-enable',
using `entropy/emacs-fontsets-used-cjk-tc-font' when
`entropy/emacs-font-chinese-type' was 'tc' or for using
`entropy/emacs-fontsets-used-cjk-sc-font'.")

(defvar entropy/emacs-fontsets-used-cjk-jp-font "Noto Sans Mono CJK JP"
  "The JP script font in eemacs context, set automatically
followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-used-cjk-kr-font "Noto Sans Mono CJK KR"
  "The hangul script font in eemacs context, set automatically
followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-used-extra-fonts nil
  "Extra fonts list in eemacs conttext, set automatically
followed by `entropy/emacs-font-setting-enable'.")

(defvar entropy/emacs-fontsets-fonts-collection-alias
  '((sarasa :latin "Sarasa Mono SC" :sc "Sarasa Mono SC" :tc "Sarasa Mono TC"
            :jp "Sarasa Mono J" :kr "Sarasa Mono K")
    (google :latin "Noto Mono" :sc "Noto Sans Mono CJK SC" :tc "Noto Sans Mono CJK TC"
            :jp "Noto Sans Mono CJK JP" :kr "Noto Sans Mono CJK KR"
            :symbol "Noto Sans Symbols")
    (fira-code :latin "Fira Code" :sc "Noto Sans Mono CJK SC" :tc "Noto Sans Mono CJK TC"
               :jp "Noto Sans Mono CJK JP" :kr "Noto Sans Mono CJK KR"
               :symbol "Noto Sans Symbols"))
  "Alist of each fontset group for =entropy-emacs=.

 Each element is a cons of a type symbol and a group instance
 plist with following valid keys:

 - ':latin': latin script font family name string.
 - ':sc': simplified chinese font family name string.
 - ':tc': traditional chinese font family name string.
 - ':jp': japanese font family name string.
 - ':kr': korean font family name string.
 - ':symbol': symbol script font family name string.
 - ':extra': needed extra font family name strings list.
 - ':after': a function with one argument of a frame selected be
   called when this fontset type group is set.")

;; ** pdumper
(defvar entropy/emacs-pdumper-pre-lpth nil
  "The fully preserved `load-path' before pdumper dump procedure
invoking. Used to get the copy of the normal eemacs `load-path'
prepared for some emergency occasion.")

;; ** frame refer

(defvar entropy/emacs-main-frame (selected-frame)
  "The =main-operated-frame= (see below for details) used for
=entropy-emacs= of current emacs-session predicated by `framep'
or nil when it not set.

=main-operated-frame= is the frame user sticked in, its commonly
is the current `selected-frame', not an child frame or any other
emacs server maked one, its an eemacs generally term and just a
frame basically.

NOTE: do not manually modify this variable, since eemacs auto set
it internally.")

(defvar entropy/emacs-delete-frame-functions nil
  "Like `delete-frame-functions' but ran before it and any function
in this list should not kill the frame internally which is
restricted by thus.")

(defun entropy/emacs--run-eemacs-delete-frame-functions
    (&optional frame _force)
  "The running process for `entropy/emacs-delete-frame-functions'.

NOTE: do not run it in any cases since its an `delete-frame' advice."
  (let ((frame (or frame (selected-frame))))
    (when entropy/emacs-delete-frame-functions
      (dolist (func entropy/emacs-delete-frame-functions)
        (funcall func frame)))))

(advice-add 'delete-frame
            :before
            #'entropy/emacs--run-eemacs-delete-frame-functions)

;; ** daemon refer
(defvar entropy/emacs-daemon-server-init-done nil
  "When non-nil indicate the daemon server has been initialized
i.e. has been first called done by open from 'emacsclient'
command, that's say this daemon has been used at thus.")

(defvar entropy/emacs-daemon-server-after-make-frame-hook nil
  "Normal hooks run after a emacs daemon session create a client
frame. The created frame is selected when the hook is called.

For conventionally, you should inject any function into it by
using `entropy/emacs-with-daemon-make-frame-done', see its
docstring for details.
")

(defun entropy/emacs-daemon--create-daemon-client-plist ()
  "Create an entropy-emacs specified representation of a daemon client, a plist.

There's two key slots for this plist, ':frame' for theh current
main daemon client's frame object. ':gui-p' for whether thus is
`display-graphic-p'"
  (list :frame (selected-frame)
        :gui-p (display-graphic-p)))

(defvar entropy/emacs-daemon--main-client-indicator nil
  "Non-nil is a plist representation for current main daemon
client built by
`entropy/emacs-daemon--create-daemon-client-plist'.

Do not set it with `setq' or other commonly lisp variable set
api, using `entropy/emacs-daemon--set-main-client-indictor' to
set an `entropy/emacs-daemon--reset-main-client-indictor' to
reset.")

(defvar entropy/emacs-daemon--legal-clients nil
  "A list of legal daemon clients representation.")

(defun entropy/emacs-daemon-multi-gui-clients-p ()
  "Indicate whether current daemon clients has 2 or more gui
clients."
  (when (daemonp)
    (let ((cnt 0) frame gui-p)
      (dolist (el entropy/emacs-daemon--legal-clients)
        (setq frame (plist-get el :frame)
              gui-p (plist-get el :gui-p))
        (when (and gui-p
                   (frame-live-p frame))
          (cl-incf cnt)))
      (if (> cnt 1)
          t
        nil))))

(defun entropy/emacs-daemon-frame-is-daemon-client-p (frame)
  "Return non-nil when frame is an daemon client."
  (when (daemonp)
    (catch :exit
      (dolist (el entropy/emacs-daemon--legal-clients)
        (when (eq (plist-get el :frame) frame)
          (throw :exit t))))))

(defun entropy/emacs-daemon-current-is-main-client (&optional frame)
  "Judge whether current frame is the frame of current daemon
main client."
  (let ((main-judge
         (eq (or frame (selected-frame))
             (plist-get
              entropy/emacs-daemon--main-client-indicator
              :frame))))
    main-judge))

(defun entropy/emacs-daemon--set-main-client-indictor (x)
  (setq entropy/emacs-daemon--main-client-indicator
        x
        entropy/emacs-main-frame
        (plist-get entropy/emacs-daemon--main-client-indicator
                   :frame)))

(defun entropy/emacs-daemon--reset-main-client-indicator ()
  (setq entropy/emacs-daemon--main-client-indicator
        nil
        entropy/emacs-main-frame
        nil))

(defun entropy/emacs-daemon--delete-frame-function
    (frame-predel)
  "Delete the frame FRAME-PREDEL when it is an daemon client i.e. judged
by `entropy/emacs-daemon-frame-is-daemon-client-p'.

If FRAME-PREDEL is `entropy/emacs-daemon--main-client-indicator' , it
will be rest and the car of `entropy/emacs-daemon--legal-clients' will
be the assignment to be the new main one if non-nil.

Otherwise FRAME-PREDEL will be delete from
`entropy/emacs-daemon--legal-clients', so that it will not be an legal
frame predicated by `entropy/emacs-daemon-frame-is-daemon-client-p'
any more.
"
  (let (temp_var (frame frame-predel))
    (when (entropy/emacs-daemon-frame-is-daemon-client-p frame)
      ;; pop out current daemon client from
      ;; `entropy/emacs-daemon--legal-clients'.
      (when (not (null entropy/emacs-daemon--legal-clients))
        (dolist (el entropy/emacs-daemon--legal-clients)
          (unless (eq frame (plist-get el :frame))
            (push el temp_var)))
        (setq entropy/emacs-daemon--legal-clients
              temp_var))
      ;; Reset `entropy/emacs-daemon--main-client-indicator'.
      (when (entropy/emacs-daemon-current-is-main-client frame)
        (entropy/emacs-daemon--reset-main-client-indicator)
        (when (not (null entropy/emacs-daemon--legal-clients))
          (catch :exit
            (while entropy/emacs-daemon--legal-clients
              (if (frame-live-p
                   (plist-get
                    (car entropy/emacs-daemon--legal-clients) :frame))
                  (progn
                    (entropy/emacs-daemon--set-main-client-indictor
                     (car entropy/emacs-daemon--legal-clients))
                    (throw :exit nil))
                (pop entropy/emacs-daemon--legal-clients)))))))))

(when (daemonp)
  (add-to-list 'delete-frame-functions
               #'entropy/emacs-daemon--delete-frame-function))

(defvar entropy/emacs-daemon--dont-init-client nil
  "Forbidden eemacs server client initialization specification
when non-nil.

EEMACS_MAINTENANCE:
Please just use in debug way as may not work in lexical way.")

(defun entropy/emacs-daemon--client-initialize ()
  "Initial daemon client instance with eemacs specification
creation procedure.

NOTE: Do not use it in any entropy-emacs context, its just a
unified specifically hook for `server-after-make-frame-hook'.

EEMACS_MAINTENANCE:

Please just leave it in `entropy-emacs-defvar' file as its
internal functional."
  (when (and (daemonp)
             (null entropy/emacs-daemon--dont-init-client)
             (and
              ;; must an visisble frame, NOTE: we can not use
              ;; `frame-visible-p' or the frame parameter `visibility'
              ;; since they just return non-nil when frame is
              ;; displayed even if the frame is created used to
              ;; display, so we must use the underlying mechanism
              (frame-parameter nil 'display)
              ;; must not an existed client frame since
              ;; `server-process-filter' use current frame like some
              ;; server based package such as `with-editor' to make
              ;; pseudo frame which reused current one.
              (not (entropy/emacs-daemon-frame-is-daemon-client-p
                    (selected-frame)))
              ;; must in interaction mode
              (null noninteractive)
              ;; TODO: add more filters to make explicit judge whether
              ;; the frame is maked from command line
              ;; i.e. 'emacsclient ...'
              ))
    (set-frame-parameter (selected-frame) 'eemacs-current-frame-is-daemon-created t)
    ;; fix problem when main daemon client is dead
    (when entropy/emacs-daemon--main-client-indicator
      (unless (frame-live-p (plist-get entropy/emacs-daemon--main-client-indicator :frame))
        (entropy/emacs-daemon--reset-main-client-indicator)))
    ;; fix problem when there's dead daemon clients in register.
    (when entropy/emacs-daemon--legal-clients
      (let (temp_var)
        (dolist (clts entropy/emacs-daemon--legal-clients)
          (when (frame-live-p (plist-get clts :frame))
            (push clts temp_var)))
        (setq entropy/emacs-daemon--legal-clients temp_var)))
    ;; startup daemon specification
    (if (or (not entropy/emacs-daemon--main-client-indicator)
            ;; in same display type
            (eq (display-graphic-p)
                (plist-get entropy/emacs-daemon--main-client-indicator
                           :gui-p)))
        (let ((daemon-client-obj (entropy/emacs-daemon--create-daemon-client-plist)))
          (push daemon-client-obj entropy/emacs-daemon--legal-clients)
          (unless entropy/emacs-daemon--main-client-indicator
            (entropy/emacs-daemon--set-main-client-indictor
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
                  (if (plist-get entropy/emacs-daemon--main-client-indicator
                                 :gui-p)
                      "GUI"
                    "TTY")
                  (if (display-graphic-p)
                      "GUI"
                    "TTY"))))))
    ;; finally we sign the init done for daemon for fist time.
    (unless entropy/emacs-daemon-server-init-done
      (setq entropy/emacs-daemon-server-init-done t))))

(when (daemonp)
  (add-hook 'server-after-make-frame-hook
            #'entropy/emacs-daemon--client-initialize))

;; ** ime refer

(defvar entropy/emacs-internal-IME-toggle-function
  (lambda (&optional type)
    (interactive)
    (let ((error-msg
           "Non defination found for current union internal ime toggle function."
           ))
      (message "%s" error-msg)
      (if type
          nil
        error-msg)))
  "Eemacs union emacs internal IME toggle function , is a
`interactive' function.

This function must be `ignore-errors' like wrapped i.e. never
throw error, and return t for indicate internal IME toggle enable
and nil for disable, and any other return is indicate toggle with
fatal.

And this function must can optionally accept one argument
i.e. the toggle type, 'enable' or 'disable'. Which when type is
enable, then theh function should enable the corresponding
internal input method and 'disable' to disable thus, and the
return means has changed to use t for succeed and any other
return for fatal.

If use a string as the fatal return, it should be the error message
string for conventionally did.")

;; ** package patches refer

(defvar entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-conditions nil
  "List of functions to call without any args as an condition
judger to judge whether inhibit the
`ivy-dynamic-exhibit-delay-ms'.

If one of the condition return non-nil then ignore rest
conditions and consider that should be inhibited.")

(defun entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-p ()
  "Return non-nil when we should inhibit
`ivy-dynamic-exhibit-delay-ms' rely on
`entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-conditions'."
  (catch :exit
    (dolist (func entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-conditions)
      (when (funcall func)
        (throw :exit t)))))

;; ** coding sytle

(defvar-local entropy/emacs-inhibit-simple-whitespace-clean nil
  "Non-nil inhibit `entropy/emacs-basic-simple-whitespace-clean'
did before current buffer saving procedure.")

;; * provide
(provide 'entropy-emacs-defvar)
