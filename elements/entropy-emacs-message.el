;;; -*- lexical-binding: t; -*-
(require 'ansi-color)

(defconst entropy/emacs-message-message-buffer
  " *eemacs messages*")

(defconst entropy/emacs-message-message-fg
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

(defconst entropy/emacs-message-message-bg
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst entropy/emacs-message-message-fx
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

(defun entropy/emacs-message--ansi-apply (code format &rest args)
  (let ((rule (or (assq code entropy/emacs-message-message-fg)
                  (assq code entropy/emacs-message-message-bg)
                  (assq code entropy/emacs-message-message-fx))))
    (format "\e[%dm%s\e[%dm"
            (cdr rule)
            (apply #'format format args)
            0)))

(defmacro entropy/emacs-message--format-message (message &rest args)
  "An alternative to `format' that strips out ANSI codes if used in an
interactive session."
  `(cl-flet*
       (,@(cl-loop for rule
                   in (append entropy/emacs-message-message-fg
                              entropy/emacs-message-message-bg
                              entropy/emacs-message-message-fx)
                   collect
                   `(,(car rule)
                     (lambda (message &rest args)
                       (apply #'entropy/emacs-message--ansi-apply ',(car rule) message args))))
        (color
         (lambda (code format &rest args)
           (apply #'entropy/emacs-message--ansi-apply code format args))))
     (format ,message ,@args)))

(defun entropy/emacs-message-quit (&rest args)
  (let ((echo-wd (get-buffer-window entropy/emacs-message-message-buffer)))
    (when echo-wd
      (delete-window echo-wd))))

(advice-add 'keyboard-quit :before #'entropy/emacs-message-quit)

;;;###autoload

(defmacro entropy/emacs-message-do-message (message &rest args)
  "An alternative to `message' that strips out ANSI codes if used in an
interactive session."
  `(if noninteractive
       (message (entropy/emacs-message--format-message ,message ,@args))
     (let ((buf (get-buffer-create entropy/emacs-message-message-buffer))
           echo-string)
       (with-current-buffer buf
         (goto-char (point-max))
         (let ((beg (point))
               end)
           (insert (entropy/emacs-message--format-message ,message ,@args))
           (insert "\n")
           (setq end (point))
           (ansi-color-apply-on-region beg end)))
       (with-selected-window (display-buffer-at-bottom
                              buf '((align . below)
                                    (window-height . 0.3)))
         (goto-char (point-max))))))

(provide 'entropy-emacs-message)
