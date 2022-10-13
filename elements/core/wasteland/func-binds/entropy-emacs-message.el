;;; entropy-emacs-message.el --- entropy-emacs top function library for 'message' refer  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 201909  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-message.el
;; Package-Version: 0.1.0
;; Version:       file-version
;; Created:       2019
;; Compatibility: GNU Emacs 25.2;
;; Package-Requires: ((emacs "25.2") (cl-lib "0.5") (ansi-color "3"))
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
;; This file was built on the =func-binds= concept of entropy-emacs's
;; designation.
;;
;; The ansi-color wrapper for emacs message-system, aim for unifying
;; non-interaction (e.g. emacs batch-mode ) or interaction session's
;; messaging system, inspired from [[https://github.com/hlissner/doom-emacs][doom-emacs]]'s `message.el'
;;
;; * Configuration:
;;
;; Design for =entropy-emacs= only, non-warranty for grafting.
;;
;; * Code:
;; ** require
(require 'ansi-color)
(entropy/emacs-common-require-feature 'entropy-emacs-defconst)

;; ** const define
(defconst entropy/emacs-message-message-buffname
  "*Eemacs Messages*")

(defconst entropy/emacs-message--message-fg
  '((reset      . 0)
    (black      . 30)
    (red        . 31)
    (green      . 32)
    (yellow     . 33)
    (blue       . 34)
    (magenta    . 35)
    (cyan       . 36)
    (white      . 37))
  "List of text colors.")

(defconst entropy/emacs-message--message-bg
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst entropy/emacs-message--message-fx
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

;; ** variable declaration
(defvar entropy/emacs-message-non-popup t
  "Do not message with popup window, mostly used with lexical
binding and be under suppressed by
`entropy/emacs-message-non-popup-permanently'.

Defaulty we disable popup message since it is not good idea in
most of cases.")

(defvar entropy/emacs-message-non-popup-permanently nil
  "Permanently suppress popup window type for messaging, this
setting will suppress `entropy/emacs-message-non-popup'
functional whenever what it is.")

(entropy/emacs-defvar-local-with-pml entropy/emacs-message--cur-buf-is-popup-p nil)
(defvar entropy/emacs-message--idle-timer-for-hide-popup nil)

;; ** library

(defun entropy/emacs-message--safety-format (string &rest objects)
  "Like `format' but wrap when no objects presented so as safety
thus."
  (if objects
      (apply 'format string objects)
    (format "%s" string)))

(defun entropy/emacs-message--in-daemon-load-p ()
  "Judge whether current env is a daemon silence status e.g both
satisficed `daemonp' and in emacs init time.

This function used to give out a notation that in thus time we
can not use popup message style where the emacs session do not
have window feature started up."
  (and (daemonp) (null after-init-time)))

(defun entropy/emacs-message--get-plist-body (args)
  "Get BODY inside of 'pre-plist' ARGS, commonly is the last non
key-pair cdr.

This function is useful for cl-based def* args parsing like:

#+begin_src emacs-lisp
  (name &rest body
        &key
        key-1
        key-2
        ...
        &allow-other-keys)
#+end_src

To get the real-body in BODY.
"
  (let ((it args))
    (catch 'break
      (while t
        (if (keywordp (car it))
            (setq it (cddr it))
          (throw 'break it))))))

;; *** top advice
(defun entropy/emacs-message-quit (&rest _)
  (ignore-errors
    (entropy/emacs-message-hide-popup t)))
(advice-add 'keyboard-quit :before #'entropy/emacs-message-quit)

;; *** common ansi message wrapper core

;; define `ansi-color--find-face' while not found since its may obsolete while emacs-29
(unless (fboundp 'ansi-color--find-face)
  (defun ansi-color--find-face (codes)
    "Return the face corresponding to CODES."
    (let (faces)
      (while codes
        (let ((face (ansi-color-get-face-1 (pop codes))))
          ;; In the (default underline) face, say, the value of the
          ;; "underline" attribute of the `default' face wins.
          (unless (eq face 'default)
            (push face faces))))
      ;; Avoid some long-lived conses in the common case.
      (if (cdr faces)
          (nreverse faces)
        (car faces)))))

(defun entropy/emacs-message--ansi-color-apply-for-face (string)
  "The same as `ansi-color-apply', but using `face' instead of
`font-lock-faace' for make `message' colorized."
  (let ((codes (car ansi-color-context))
        (start 0) end result)
    ;; If context was saved and is a string, prepend it.
    (if (cadr ansi-color-context)
        (setq string (concat (cadr ansi-color-context) string)
              ansi-color-context nil))
    ;; Find the next escape sequence.
    (while (setq end (string-match ansi-color-control-seq-regexp string start))
      (let ((esc-end (match-end 0)))
        ;; Colorize the old block from start to end using old face.
        (when codes
          (put-text-property start end 'face
                             (ansi-color--find-face codes) string))
        (push (substring string start end) result)
        (setq start (match-end 0))
        ;; If this is a color escape sequence,
        (when (eq (aref string (1- esc-end)) ?m)
          ;; create a new face from it.
          (setq codes (ansi-color-apply-sequence
                       (substring string end esc-end) codes)))))
    ;; if the rest of the string should have a face, put it there
    (when codes
      (put-text-property start (length string)
                         'face (ansi-color--find-face codes) string))
    ;; save context, add the remainder of the string to the result
    (let (fragment)
      (if (string-match "\033" string start)
          (let ((pos (match-beginning 0)))
            (setq fragment (substring string pos))
            (push (substring string start pos) result))
        (push (substring string start) result))
      (setq ansi-color-context (if (or codes fragment) (list codes fragment))))
    (apply 'concat (nreverse result))))

(defun entropy/emacs-message--ansi-format (code format &rest args)
  (let ((rule (or (assq code entropy/emacs-message--message-fg)
                  (assq code entropy/emacs-message--message-bg)
                  (assq code entropy/emacs-message--message-fx))))
    (format "\e[%dm%s\e[%dm"
            (cdr rule)
            ;; we shouldn't use `format' with only string type since
            ;; any string contain format notaion will cause it
            ;; corrupt.
            (apply 'entropy/emacs-message--safety-format
                   format args)
            0)))

(defmacro entropy/emacs-message-format-message (message &rest args)
  "An alternative to `format' that strips out ANSI codes if used in an
interactive session."
  `(cl-flet*
       (,@(cl-loop for rule
                   in (append entropy/emacs-message--message-fg
                              entropy/emacs-message--message-bg
                              entropy/emacs-message--message-fx)
                   collect
                   `(,(car rule)
                     (lambda (message &rest args)
                       (apply #'entropy/emacs-message--ansi-format
                              ',(car rule) message args))))
        (color
         (lambda (code format &rest args)
           (apply #'entropy/emacs-message--ansi-format
                  code format args))))
     (funcall 'entropy/emacs-message--safety-format ,message ,@args)))

(defmacro entropy/emacs-message--do-message-ansi-apply (message &rest args)
  `(let* ((echo-string nil)
          (inhibit-read-only t)
          (rtn-1 (entropy/emacs-message-format-message ,message ,@args))
          (rtn
           (entropy/emacs-message--ansi-color-apply-for-face
            rtn-1)))
     (setq echo-string
           (if (or noninteractive (entropy/emacs-message--in-daemon-load-p))
               (if (or (entropy/emacs-message--in-daemon-load-p)
                       sys/win32p)
                   (substring-no-properties rtn)
                 rtn-1)
             rtn))
     echo-string))

(defun entropy/emacs-message--format-message-2 (message &optional args)
  (eval
   `(entropy/emacs-message--do-message-ansi-apply
     ,message ,@args)))

(cl-defmacro entropy/emacs-message--do-message-popup
    (message &rest args &key without-log-message-before-eemacs-init-done
             &allow-other-keys)
  (setq args (entropy/emacs-message--get-plist-body args))
  `(let* ((message-str
           (entropy/emacs-message--do-message-ansi-apply
            ,message ,@args))
          (break-p
           ;; TODO: add conditions
           nil)
          (buff-get
           (lambda nil
             (let ((buff (get-buffer-create entropy/emacs-message-message-buffname)))
               (and (or (and (bufferp buff) (buffer-live-p buff))
                        (error "eemacs message buffer can not be create!"))
                    buff))))
          the-buff the-window)
     (unless break-p
       (with-current-buffer (setq the-buff (funcall buff-get))
         (setq mode-line-format nil cursor-type nil
               ;; we should always consider this buffer read-only as message
               ;; did for.
               buffer-read-only t))
       (unless (window-live-p (get-buffer-window the-buff))
         (save-selected-window
           (display-buffer
            the-buff
            `((display-buffer-at-bottom)
              .
              ((inhibit-switch-frame . t)
               ;; let this window be dedicated to the
               ;; `entropy/emacs-message-message-buffname' so that any
               ;; display buffer action before hide this window doesn't
               ;; operation on this window.
               (dedicated . t)
               (align . below)
               (window-height
                .
                ,(if (bound-and-true-p entropy/emacs-startup-done)
                     0.15 0.22))))
            (selected-frame))))
       (if (window-live-p (setq the-window (get-buffer-window the-buff)))
           ;; we must do insert withins window selected since we can
           ;; denote the current line of that window.
           (with-selected-window the-window
             (set-window-parameter
              the-window 'entropy/emacs-message--cur-win-is-popup-p t)
             (let ((inhibit-read-only t))
               (with-current-buffer the-buff
                 (unless (eq the-buff (window-buffer))
                   ;; FIXME: why this happened in some case?
                   (error "[internal error] eemacs popup buffer is not dedicated to its window!"))
                 (let* ((fn-msgstr (concat "-> " message-str))
                        (fn-insertion (concat fn-msgstr "\n")))
                   (goto-char (point-max))
                   ;; also log startup procedures meta into message
                   ;; buffer so that we can see whole sequenced
                   ;; init context.
                   (unless (or entropy/emacs-startup-done
                               ,without-log-message-before-eemacs-init-done)
                     (message "%s" fn-msgstr))
                   (insert fn-insertion)
                   (redisplay t)
                   (setq entropy/emacs-message--cur-buf-is-popup-p t)))))
         (error "Can not create an `entropy/emacs-message-message-buffname' window."))
       ;; run an timer guard to force hide popuped message window
       (unless (timerp entropy/emacs-message--idle-timer-for-hide-popup)
         (setq entropy/emacs-message--idle-timer-for-hide-popup
               (run-with-idle-timer 0.05 t #'entropy/emacs-message-hide-popup))))))

(defmacro entropy/emacs-message-do-message-1 (message &rest args)
  "An alternative to `message' that strips out ANSI codes."
  `(let (_)
     ;; (redisplay t)

     ;; we shouldn't use `message' directly to touch the message in
     ;; this place since any string contain format notaion will cause
     ;; it corrupt, thus we use single `%s' to format the result.
     (prog1
         (message "%s"
                  (entropy/emacs-message--do-message-ansi-apply
                   ,message ,@args))
       ;; (redisplay t)
       )))

(defmacro entropy/emacs-message-do-warn-1 (message &rest args)
  "An alternative to `message' that strips out ANSI codes."
  `(warn
    (entropy/emacs-message--do-message-ansi-apply
     ,message ,@args)))

;; *** common progress message wrapper core


;; ** auto load
;; *** common ansi message wrapper APIs
;;;###autoload
(cl-defmacro entropy/emacs-message-do-message
    (message &rest args
             &key (popup-while-eemacs-init-with-interactive nil)
             (force-message-while-eemacs-init nil)
             &allow-other-keys)
  "An alternative to `message' that strips out ANSI codes, with
popup window if in a interaction session when
`entropy/emacs-message-non-popup' is `nil' and follow the
restriction as below:

Disable popup feature forcly when in minibuffer window and any
`noninteractive' session or daemon init procedure.

Suppress any heavy message using simple prompts before
`entropy/emacs-startup-done' (i.e. the emacs init duration) is
set unless `entropy/emacs-startup-with-Debug-p' was non-nil in
interactive session or
   - in a daemon init session
   - in a make session
   - in a interactive session and
     `entropy/emacs-custom-enable-lazy-load' is disabled in which
     case we should see the heavy load procedure with explicitly
     information.

Optional key:

- popup-while-eemacs-init-with-interactive :: using popup type for message forcely
when in an interactive session and before eemacs startup done
without respect `entropy/emacs-message-non-popup'.

- force-message-while-eemacs-init :: forcely show the message
  rather than the suppressed one while eemacs startup duration.
"
  (let ((args (entropy/emacs-message--get-plist-body args)))
    `(cond (
            ;; ========== Simplifying the startup hints
            (and
             ;; only used in eemacs startup time
             (not (bound-and-true-p entropy/emacs-startup-done))
             ;; -- not when key :force-message-while-eemacs-init is set while eemacs init
             (not ,force-message-while-eemacs-init)
             ;; BUT:
             ;; -- not in debug mode
             (not entropy/emacs-startup-with-Debug-p)
             ;; -- not when non-lazy-mode enabled in interactive session
             ;;    since we should see the long terms of init.
             (not
              (and (null noninteractive)
                   (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load))))
             ;; -- not in daemon init type
             (not
              (and (not (bound-and-true-p entropy/emacs-daemon-server-init-done))
                   (daemonp)))
             ;; -- not in make session
             (not (entropy/emacs-is-make-session))
             ;; -- not when key :popup-while-eemacs-init-with-interactive is set while eemacs init
             (not (and (not (bound-and-true-p entropy/emacs-startup-done))
                       ,popup-while-eemacs-init-with-interactive))
             )
            (message "%s" "Loading ..."))
           (
            ;; ========== Disable the popup feature when needed
            (or
             ;; always disbale popup in `noninteractive' mode
             noninteractive
             ;; always disbale popup when daemon loading
             (entropy/emacs-message--in-daemon-load-p)
             ;; always disbale popup when in minibuffer window
             (minibuffer-window-active-p (selected-window))
             )
            (entropy/emacs-message-do-message-1 ,message ,@args))
           (
            ;; ========== Use popup feature in interactive session with some conditions
            (and (null noninteractive)
                 (or
                  ;; allow popup when eemacs startup done
                  (and (bound-and-true-p entropy/emacs-startup-done)
                       (null entropy/emacs-message-non-popup)
                       (null entropy/emacs-message-non-popup-permanently))
                  ;; allow popup in debug init
                  (and (not (bound-and-true-p entropy/emacs-startup-done))
                       entropy/emacs-startup-with-Debug-p)
                  ;; allow popup when non-lazy interative init session
                  (and (null noninteractive)
                       (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load))
                       (not (bound-and-true-p entropy/emacs-startup-done)))
                  ;; allow popup when key :popup-while-eemacs-init-with-interactive is set
                  ;; while eemacs init time
                  (and (not (bound-and-true-p entropy/emacs-startup-done))
                       (not (null ,popup-while-eemacs-init-with-interactive)))))
            (entropy/emacs-message--do-message-popup
             ,message ,@args))
           (
            ;; ========== otherwise use echo area defaultly
            t
            (entropy/emacs-message-do-message-1 ,message ,@args)))))

(defun entropy/emacs-message-focus-on-popup-window ()
  "Focus on popuped `entropy/emacs-message-message-buffname'
window if available, return non-nil for success, nil otherwise."
  (let ((buff
         (get-buffer-create
          entropy/emacs-message-message-buffname)))
    (when (and (buffer-live-p buff)
               (window-live-p
                (get-buffer-window buff)))
      (select-window (get-buffer-window buff))
      (with-selected-window (get-buffer-window buff)
        (setq cursor-type t))
      t)))

(defmacro entropy/emacs-message-do-warn (message &rest args)
  "An alternative `warn' that strips out ANSI codes. "
  `(let (_)
     (entropy/emacs-message-do-warn-1 ,message ,@args)))

(defmacro entropy/emacs-message-do-error (message &rest args)
  "An alternative to `user-error' that strips out ANSI codes.

Enable debugger when `entropy/emacs-startup-with-Debug-p' is
enable, suppressed whenever possible in otherwise.

NOTE: Just use it in `noninteractive' session."
  `(let ((debug-on-error entropy/emacs-startup-with-Debug-p)
         (inhibit-debugger (not entropy/emacs-startup-with-Debug-p)))
     (entropy/emacs-message-do-message
      ,message ,@args)
     (user-error "")))

(defun entropy/emacs-message-hide-popup (&optional force)
  "Hide popup window which display `entropy/emacs-message-message-buffname'."
  (let* ((buf-name entropy/emacs-message-message-buffname)
         (win (get-buffer-window buf-name)))
    (when (and (and (bound-and-true-p entropy/emacs-startup-done)
                    (windowp win))
               (or force
                   (window-parameter win 'entropy/emacs-message--cur-win-is-popup-p)
                   (and (buffer-live-p (get-buffer buf-name))
                        (with-current-buffer (get-buffer buf-name)
                          entropy/emacs-message--cur-buf-is-popup-p))))
      (let ((ignore-window-parameters t))
        (delete-window win))
      (when (buffer-live-p (get-buffer buf-name))
        (with-current-buffer (get-buffer buf-name)
          (setq entropy/emacs-message--cur-buf-is-popup-p nil)))
      (when (timerp entropy/emacs-message--idle-timer-for-hide-popup)
        (cancel-timer entropy/emacs-message--idle-timer-for-hide-popup)
        (setq entropy/emacs-message--idle-timer-for-hide-popup
              nil))
      )))

;; *** common progress message wrapper APIs

(cl-defmacro entropy/emacs-message-simple-progress-message
    (message &rest body
             &key with-temp-message ignore-current-messages
             with-either-popup
             with-message-color-args
             &allow-other-keys)
  "Do BODY and return its result with progress prompt message MESSAGE
using `make-progress-reporter'.

Inhibit any prompts while message is nil in which case we just run
BODY.

If WITH-TEMP-MESSAGE is set and return non-nil, then all the progress
messages are not logged into `messages-buffer'.

The `current-message' is restored in echo area after BODY ran out if
it is non-nil only when WITH-TEMP-MESSAGE is set and return non-nil
and `current-message' is non-nil and is not `member' in
IGNORE-CURRENT-MESSAGES or call IGNORE-CURRENT-MESSAGES with
`current-message' and return non-nil if it is a function.

This function do not do any restriction for any messages produced by
BODY.

If WITH-MESSAGE-COLOR-ARGS is set, it should return a list of args
used with MESSAGE together apply to
`entropy/emacs-message-format-message' in which case MESSAGE should
set as a format string. Used to build a colored MESSAGE.

If WITH-EITHER-POPUP is set and return non-nil, then MESSAGE is also
displayed via `entropy/emacs-message--do-message-popup'."
  (let ((body (entropy/emacs-message--get-plist-body body))
        (with-tmpmsg-sym (make-symbol "with-temp-message-p"))
        (message-sym (make-symbol "message"))
        (progress-reporter-sym (make-symbol "progress-reporter"))
        (curmsg-sym (make-symbol "curmsg"))
        (ignmsgs-sym (make-symbol "ignmsgs"))
        (msg-max-ov-sym (make-symbol "origin-message-log-max-value"))
        (use-popup-p-sym (make-symbol "with-popup-p"))
        (msg-color-args-sym (make-symbol "message-color-args")))
    `(let* ((,with-tmpmsg-sym ,with-temp-message)
            (,curmsg-sym (current-message))
            (,ignmsgs-sym ,ignore-current-messages)
            (,message-sym ,message)
            (,use-popup-p-sym ,with-either-popup)
            (,msg-color-args-sym ,with-message-color-args)
            (_ (when (and ,message-sym ,msg-color-args-sym)
                 (setq ,message-sym
                       (entropy/emacs-message--format-message-2
                        ,message-sym ,msg-color-args-sym))))
            ;; restrict messages
            (,msg-max-ov-sym message-log-max)
            (message-log-max
             (if ,with-tmpmsg-sym nil ,msg-max-ov-sym))
            (,progress-reporter-sym
             (when ,message-sym
               (make-progress-reporter (format "%s ... " ,message-sym)))))
       (when (and ,message-sym ,use-popup-p-sym
                  entropy/emacs-startup-with-Debug-p
                  (not entropy/emacs-startup-done))
         (entropy/emacs-message--do-message-popup
          (format "%s ..." ,message-sym)
          :without-log-message-before-eemacs-init-done t))
       (prog1 (let ((message-log-max ,msg-max-ov-sym))
                ,(entropy/emacs-macroexp-progn body))
         (when ,progress-reporter-sym
           (progress-reporter-done ,progress-reporter-sym))
         (when (and ,message-sym ,curmsg-sym)
           (let (
                 ;; we has no reason to log the old msg again since we
                 ;; just need to let be shown in echo area.
                 (message-log-max nil))
             (when (and ,with-tmpmsg-sym
                        (if (functionp ,ignmsgs-sym)
                            (not (funcall ,ignmsgs-sym ,curmsg-sym))
                          (not (member ,curmsg-sym ,ignmsgs-sym))))
               (message "%s" ,curmsg-sym))))))))

(defmacro entropy/emacs-message-make-func-with-simple-progress-prompts
    (func-name &optional message &rest args)
  "Make function named by FUNC-NAME executing return result with
progress prompt using
`entropy/emacs-message-simple-progress-message'."
  (declare (indent 1))
  (let ((func-name-sym     (make-symbol "func-name"))
        (func-adv-name-sym (make-symbol "func-acv-name"))
        (message-sym       (make-symbol "message")))
    `(let* ((,func-name-sym ,func-name)
            (,func-adv-name-sym
             (intern (format "__adv/around/%s/for-progress-promtps__" ,func-name-sym)))
            (,message-sym ,message))
       (defalias ,func-adv-name-sym
         (lambda (orig-func &rest orig-args)
           (let (_)
             (entropy/emacs-message-simple-progress-message
              (if (not (null ,message-sym))
                  ;; append user specified message with function name
                  (format "[`%s']: %s"
                          ,func-name-sym
                          (entropy/emacs-message--do-message-ansi-apply
                           ,message-sym ,@args))
                ;; use plain simplify message type
                (format "Do `%s' executing" ,func-name-sym))
              (apply orig-func orig-args)))))
       (advice-add ,func-name-sym :around ,func-adv-name-sym))))

;; * provide
(provide 'entropy-emacs-message)
