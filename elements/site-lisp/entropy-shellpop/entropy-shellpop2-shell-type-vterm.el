;; -*- lexical-binding: t; -*-

(require 'entropy-shellpop2-core)
(defvar vterm-buffer-name-string)
(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-check-proc "vterm")
(declare-function vterm-copy-mode "vterm")
(declare-function vterm-delete-region "vterm")
(declare-function vterm--get-end-of-line "vterm")
(declare-function vterm--get-prompt-point "vterm")

(entropy/shellpop2/core/macro/make/shell/type/obj vterm
  (entropy/shellpop2/core/generic/shell/buffer/op/init
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption
      (read-string "Set this vterm caption: " nil nil nil t))
     (let (
           ;; prevent vterm auto rename buffer that lost register linkage
           (vterm-buffer-name-string nil))
       (with-current-buffer
           (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)
         (vterm-mode)
         (current-buffer)))))
  (entropy/shellpop2/core/generic/shell/buffer/op/cwd
   (shell/buffer/obj uri)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer))
            prbegpt prendpt)
       (with-current-buffer buff
         (when (and (not (file-remote-p uri)) (file-directory-p uri)
                    (not (file-equal-p uri default-directory))
                    (not (bound-and-true-p vterm-copy-mode))
                    (eq major-mode 'vterm-mode)
                    (vterm-check-proc (current-buffer))
                    (setq prbegpt (or (vterm--get-prompt-point)
                                      (error "vterm can not found prompt begin point"))
                          prendpt (or (vterm--get-end-of-line)
                                      (error "vterm can not found prompt end point")))
                    (= prbegpt (point)) (= prbegpt prendpt))
           (vterm-delete-region prbegpt prendpt)
           (vterm-send-string
            (concat "cd " (shell-quote-argument uri)))
           (vterm-send-return))))))
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

(provide 'entropy-shellpop2-shell-type-vterm)
