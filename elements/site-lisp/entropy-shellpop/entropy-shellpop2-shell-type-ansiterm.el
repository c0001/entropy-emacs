;; -*- lexical-binding: t; -*-

(require 'entropy-shellpop2-core)
(require 'term)

(entropy/shellpop2/core/macro/make/shell/type/obj ansiterm
  (entropy/shellpop2/core/generic/shell/buffer/op/init
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption
      (read-string "Set this ansiterm caption: " nil nil nil t))
     (let ((otbfnm term-ansi-buffer-name))
       (save-window-excursion
         (ansi-term shell-file-name)
         (when (equal otbfnm term-ansi-buffer-name)
           (error "shellpop inner error: ansiterm not create a new buffer"))
         (with-current-buffer (get-buffer term-ansi-buffer-name)
           (let (_)
             (apply
              'entropy/shellpop2/core/func/local-set-key-batch-do
              (cl-loop for key in '("C-g" "A-C-g" "C-s-g" "C-M-g" "C-M-s-g")
                       collect (cons (kbd key) #'keyboard-quit)))
             (current-buffer)))))))
  (entropy/shellpop2/core/generic/shell/buffer/op/cwd
   (shell/buffer/obj uri)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
       (with-current-buffer buff
         (entropy/shellpop2/core/macro/with-canbe-cdw uri
           (unless
               ;; emacs-term not support remote auto cd
               (file-remote-p uri)
             (term-send-raw-string "\C-u")
             (term-send-raw-string "\C-k")
             (term-send-raw-string
              (concat "cd "
                      (shell-quote-argument
                       uri)
                      "\n"))))))))
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

(provide 'entropy-shellpop2-shell-type-ansiterm)
