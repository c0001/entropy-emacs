;;; entropy-emacs-message.el --- entropy-emacs top function library for 'message' refer
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

;; ** const define
(defconst entropy/emacs-message-message-buffer
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
(defvar entropy/emacs-message-non-popup nil
  "Do not message with popup window, mostly used with lexical
binding and be under suppressed by
`entropy/emacs-message-non-popup-permanently'.")

(defvar entropy/emacs-message-non-popup-permanently nil
  "Permanently suppress popup window type for messaging, this
setting will suppress `entropy/emacs-message-non-popup'
functional whenever what it is.")

(defvar-local entropy/emacs-message--cur-buf-is-popup-p nil)
(defvar entropy/emacs-message--idle-timer-for-hide-popup nil)

;; ** library

(defun entropy/emacs-message--in-daemon-load-p ()
  "Judge whether current env is a daemon silence status e.g both
satisficed `daemonp' and in emacs init time.

This function used to give out a notation that in thus time we
can not use popup message style where the emacs session do not
have window feature started up."
  (and (daemonp) (null after-init-time)))

;; *** top advice
(defun entropy/emacs-message-quit (&rest args)
  (entropy/emacs-message-hide-popup t))

(advice-add 'keyboard-quit :before #'entropy/emacs-message-quit)

;; *** core
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
            (apply #'format format args)
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
     (format ,message ,@args)))

(defmacro entropy/emacs-message--do-message-ansi-apply (message &rest args)
  `(let* (echo-string
          (inhibit-read-only t)
          (rtn-1 (entropy/emacs-message-format-message ,message ,@args))
          (rtn
           (entropy/emacs-message--ansi-color-apply-for-face
            rtn-1)))
     (setq echo-string
           (if (or noninteractive (entropy/emacs-message--in-daemon-load-p))
               (if (entropy/emacs-message--in-daemon-load-p)
                   (substring-no-properties rtn)
                 rtn-1)
             rtn))))

(defmacro entropy/emacs-message--do-message-popup (message &rest args)
  `(let ((buf (get-buffer-create entropy/emacs-message-message-buffer))
         (message-str
          (entropy/emacs-message--do-message-ansi-apply
           ,message ,@args)))
     (with-current-buffer buf
       (setq-local mode-line-format nil
                   cursor-type nil))
     (redisplay t)
     (unless (ignore-errors (get-buffer-window buf)) ;Prevent make redudant popup window
       (display-buffer-at-bottom
        buf
        `((align . below)
          (window-height
           .
           ,(if (bound-and-true-p entropy/emacs-startup-done)
                0.2
              0.3)))))
     ;;(redisplay t)
     (with-selected-window (get-buffer-window buf)
       (set-window-parameter
        (selected-window)
        'entropy/emacs-message--cur-win-is-popup-p t)
       (with-current-buffer buf
         (setq-local entropy/emacs-message--cur-buf-is-popup-p t)
         (goto-char (point-max))
         (insert "-> ")
         (insert message-str)
         (insert "\n")))
     ;;(redisplay t)

     ;; force hide window when message in idle time
     (when (current-idle-time)
       (entropy/emacs-message-hide-popup t))

     (unless (timerp entropy/emacs-message--idle-timer-for-hide-popup)
       (setq entropy/emacs-message--idle-timer-for-hide-popup
             (run-with-idle-timer
              0.15 t
              #'entropy/emacs-message-hide-popup
              )))))

(defmacro entropy/emacs-message-do-message-1 (message &rest args)
  "An alternative to `message' that strips out ANSI codes."
  `(message
    (entropy/emacs-message--do-message-ansi-apply
     ,message ,@args)))

;; ** auto load
;;;###autoload
(defmacro entropy/emacs-message-do-message (message &rest args)
  "An alternative to `message' that strips out ANSI codes, with popup
window if in a interaction session and
`entropy/emacs-message-non-popup' is `null'."
  `(if (or noninteractive
           (entropy/emacs-message--in-daemon-load-p)
           entropy/emacs-message-non-popup
           entropy/emacs-message-non-popup-permanently)
       (entropy/emacs-message-do-message-1 ,message ,@args)
     (entropy/emacs-message--do-message-popup
      ,message ,@args)))

(defmacro entropy/emacs-message-do-error (message &rest args)
  "An alternative to `user-error' that strips out ANSI codes.

NOTE: Just use it in `noninteractive' session."
  `(progn
     (entropy/emacs-message-do-message
      ,message ,@args)
     (user-error "")))

(defun entropy/emacs-message-hide-popup (&optional force)
  "Hide popup window which display `entropy/emacs-message-message-buffer'."
  (let* ((buf-name entropy/emacs-message-message-buffer)
         (win (ignore-errors
                (get-buffer-window buf-name))))
    (when (and (and (bound-and-true-p entropy/emacs-startup-done)
                    win)
               (or force
                   (window-parameter win 'entropy/emacs-message--cur-win-is-popup-p)
                   (with-current-buffer buf-name
                     entropy/emacs-message--cur-buf-is-popup-p)))
      (delete-window win)
      ;; FIXME: prevent remaining thus exist, why need this?
      (unless (ignore-errors
                (get-buffer-window buf-name))
        (entropy/emacs-message-hide-popup t))
      (with-current-buffer buf-name
        (setq-local entropy/emacs-message--cur-buf-is-popup-p nil))
      (message
       (entropy/emacs-message--do-message-ansi-apply
        "%s %s %s"
        (magenta "♥")
        "Happy hacking"
        (magenta "♥"))))))

;; * provide
(provide 'entropy-emacs-message)
