;; -*- lexical-binding: t; -*-

(require 'entropy-shellpop2-core)
(require 'shell)
(defun entropy/shellpop2/emacs-shell//func/get-comint-mode-bol-pos ()
  (let ((proc (or (get-buffer-process (current-buffer))
                  (user-error "Current buffer has no process")))
        rtn)
    (setq rtn (ignore-errors (marker-position (process-mark proc))))
    (unless (integerp rtn)
      (error "Comint subprocess are missing!"))
    rtn))

(defun entropy/shellpop2/emacs-shell//func/delete-char-for-shell-mode
    (n &optional killflag)
  (interactive "p\nP")
  (if (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p)
      (let ((comint-bol-pos (entropy/shellpop2/emacs-shell//func/get-comint-mode-bol-pos))
            pos-offset)
        (if (> comint-bol-pos (point))
            (delete-char (- n) killflag)
          (setq pos-offset (- (point) comint-bol-pos))
          (when (< pos-offset 1) (user-error "Beginning of shell prompt line!"))
          (when (> n pos-offset) (setq n pos-offset))
          (delete-char (- n) killflag)))
    (delete-char (- n) killflag)))

(defun entropy/shellpop2/emacs-shell//func/comint-send-input ()
  (interactive)
  (let ((comint-bol-pos (entropy/shellpop2/emacs-shell//func/get-comint-mode-bol-pos)))
    (if (>= (point) comint-bol-pos)
        (comint-send-input)
      (user-error "Can not operate `comint-send-input' in this point!"))))

(defvar shell-mode-map)
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "DEL")
              #'entropy/shellpop2/emacs-shell//func/delete-char-for-shell-mode)
  (define-key shell-mode-map (kbd "RET")
              #'entropy/shellpop2/emacs-shell//func/comint-send-input))

(defun entropy/shellpop2/emacs-shell//get-uri (uri)
  (let ((remote-id (file-remote-p uri)))
    (if remote-id
        (string-remove-prefix
         remote-id uri)
      uri)))

(entropy/shellpop2/core/macro/make/shell/type/obj emacs-shell
  (entropy/shellpop2/core/generic/shell/buffer/op/init
   (shell/buffer/obj)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption
      (read-string "Set this emacs-shell caption: " nil nil nil t))
     (let (nbuff)
       (with-current-buffer
           (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)
         (setq nbuff
               (save-window-excursion
                 (shell (current-buffer))))
         (or (and (buffer-live-p nbuff) nbuff)
             (error "emacs-shell api changed: \
command `shell' do not return buffer"))))))
  (entropy/shellpop2/core/generic/shell/buffer/op/cwd
   (shell/buffer/obj uri)
   (entropy/shellpop2/core/macro/do-with/shell/buffer/obj nil shell/buffer/obj
     (let* ((buff (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
       (with-current-buffer buff
         (entropy/shellpop2/core/macro/with-canbe-cdw uri
           (goto-char (point-max))
           (comint-kill-input)
           (insert
            (concat
             "cd "
             (shell-quote-argument
              (entropy/shellpop2/emacs-shell//get-uri uri))))
           (let ((comint-process-echoes t)) (comint-send-input))
           (recenter 1))))))
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

(provide 'entropy-shellpop2-shell-type-emacs-shell)
