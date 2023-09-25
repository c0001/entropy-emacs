;; the elisp impl for `eemacs-termsafe.sh'  -*- lexical-binding: t; -*-
(require 'entropy-emacs-sys)
(defvar __this_var_rate 1)
(defvar __this_var_limit 5242880)
(defvar __this_xclip_judge nil)
(defvar __this_clip_str nil)
(defvar __this_clip_last_str "")
(defvar __this_clip_str_size nil)
(defvar __this_proc_output nil)
(defvar __this_xclip_alist
  (list
   (list "wl-copy" :env
         (lambda nil
           (and (entropy/emacs-getenv "WAYLAND_DISPLAY")
                (executable-find "wl-copy")
                (executable-find "wl-paste")))
         :clean
         (lambda nil (call-process "wl-copy" nil t nil "-c")))
   (list "xclip" :env
         (lambda nil
           (and (entropy/emacs-getenv "DISPLAY")
                (executable-find "bash")
                (executable-find "xclip")))
         :clean
         (lambda nil
           (let ((ec 0))
             (catch :exit
               (dolist (el '("primary" "clipboard" "secondary"))
                 (setq ec
                       (call-process "bash" nil
                                     ;; FIXME: use stdout the proc never exit,
                                     ;; thus we just use stderr
                                     (list nil t)
                                     nil
                                     "-c"
                                     (format "xclip -selection '%s' -i <(echo -n '')"
                                             el)
                                     ))
                 (unless (zerop ec) (throw :exit ec))))
             ec)))
   (list "xsel" :env
         (lambda nil
           (and (entropy/emacs-getenv "DISPLAY")
                (executable-find "xsel")))
         :clean
         (lambda nil
           (let ((ec 0))
             (catch :exit
               (dolist (el '("p" "b" "s"))
                 (setq ec
                       (call-process "xsel" nil (list nil t) nil "--delete" el)))
               (unless (zerop ec) (throw :exit ec)))
             ec)))))

(unless (setq __this_xclip_judge
              (entropy/emacs-xterm-cut-or-yank-sync-with-system/functional-env-statisfied-p))
  (xclip-mode 1))
(message "Meta:
interprogram-paste-function: %s
interprogram-cut-function:   %s
xclip-judge:                 %s
xclip-mode:                  %s
xclip-method:                %s

Env:
WAYLAND_DISPLAY: %s
DISPLAY: %s"
         (bound-and-true-p interprogram-paste-function)
         (bound-and-true-p interprogram-cut-function)
         (bound-and-true-p __this_xclip_judge)
         (bound-and-true-p xclip-mode)
         (bound-and-true-p xclip-method)
         (entropy/emacs-getenv "WAYLAND_DISPLAY")
         (entropy/emacs-getenv "DISPLAY"))
(while (progn (sleep-for __this_var_rate) t)
  (setq __this_clip_str (ignore-errors (current-kill 0)))
  (if (and (stringp __this_clip_str)
           (string= __this_clip_str __this_clip_last_str))
      (setq __this_clip_str nil)
    (and (stringp __this_clip_str)
         (setq __this_clip_last_str __this_clip_str)))
  (when (and (stringp __this_clip_str)
             (message "[%s]: checking ..." (format-time-string "%Y-%m-%d %a %H:%M:%S"))
             (> (setq __this_clip_str_size (string-bytes __this_clip_str))
                __this_var_limit))
    (entropy/emacs-sys-simple-notify
     (format "Encounter %d huge size clipboard content, clearing ..."
             __this_clip_str_size))
    (let (pl exit-code)
      (dolist (i __this_xclip_alist)
        (setq pl (cdr i))
        (when (funcall (plist-get pl :env))
          (with-temp-buffer
            (setq exit-code
                  (funcall (plist-get pl :clean))
                  __this_proc_output
                  (buffer-substring
                   (point-min) (point-max))))
          (if (zerop exit-code)
              (progn
                (entropy/emacs-sys-simple-notify
                 (format "%s clean clipboard successfully" (car i)))
                (setq kill-ring nil) (garbage-collect))
            (user-error "%s clean clipboard with fatal exit code %s: %s"
                        (car i) exit-code __this_proc_output)))))))
