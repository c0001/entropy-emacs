;; -*- lexical-binding: t; -*-

(require 'entropy-shellpop2-core)
(declare-function eshell "eshell")
(declare-function eshell-reset "esh-mode")
(declare-function eshell-process-interact "esh-proc")

(defun entropy/shellpop2/eshell//get-uri (uri)
  (let ((remote-id (file-remote-p uri)))
    (if remote-id
        (string-remove-prefix
         remote-id uri)
      uri)))

(entropy/shellpop2/core/macro/make/shell/type/obj eshell
  (entropy/shellpop2/core/generic/shell/buffer/op/init
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption
      (read-string "Set this eshell caption: " nil nil nil t))
     (let (nbuff)
       (with-current-buffer
           (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)
         (setq nbuff (save-window-excursion (eshell 'new-session)))
         (or (and (buffer-live-p nbuff) nbuff)
             (error "eshell api changed: \
command `shell' do not return buffer"))))))
  (entropy/shellpop2/core/generic/shell/buffer/op/cwd
   (shell/buffer/obj uri)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
       (with-current-buffer buff
         (entropy/shellpop2/core/macro/with-canbe-cdw uri
           (if (eshell-process-interact 'process-live-p)
               (message "Won't change CWD because of running process.")
             (setq default-directory (file-name-as-directory uri))
             (eshell-reset)))))))
  (entropy/shellpop2/core/generic/shell/buffer/op/display
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
       (entropy/shellpop2/core/func/buffer/shackle-display buff))))
  (entropy/shellpop2/core/generic/shell/buffer/op/hide
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (when-let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
       (entropy/shellpop2/core/func/remove-buffer-window buff))))
  (entropy/shellpop2/core/generic/shell/buffer/op/remove
   (_shell/buffer/obj)
   ;; TODO
   nil))

(provide 'entropy-shellpop2-shell-type-eshell)
