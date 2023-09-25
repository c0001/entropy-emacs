;; -*- lexical-binding: t; -*-

(!eemacs-require 'entropy-emacs-defun)
(!eemacs-require 'entropy-emacs-utils)

(entropy/emacs-!cl-defun entropy/emacs-sys-simple-notify
    (msg &optional
         (expire-time 3000)
         (app-name "eemacs")
         (logo entropy/emacs-fancy-splash-logo-file)
         &key with-error)
  "Do a system notification use message MSG, the display is expired
after EXPIRE-TIME (milliseconds).

Optionally the notification is branded as APP-NAME which is a
string to indicate which one is sending it. If LOGO is set, it
should be an icon filename used with APP-NAME as for be used as
visible identity function.

The notification may not be implemented in your system by eemacs
yet, an error will be throwed out if WITH-ERROR is non-nil in
this case. Also for any error responsed by the spawns will be
throwed out if WITH-ERROR is non-nil. Otherwise, any error is
suppressed."
  (let* ((exec (cond (sys/linuxp
                      (when (executable-find "notify-send")
                        `("notify-send" "-t" ,(number-to-string expire-time)
                          "-a" ,app-name
                          "-i" ,logo
                          ,(or msg "undefined"))))
                     ;; not implement yet
                     (t nil))))
    (if (not exec)
        (when with-error
          (entropy/emacs-!error
           "No eemacs system notify implemented in your %s system yet"
           system-type))
      (entropy/emacs-with-make-process
       :name "eemacs-sys-simple-notify" :command exec
       :error
       (when with-error
         (entropy/emacs-!error
          "Fatal with notify args: %s" exec))))))

(provide 'entropy-emacs-sys)
