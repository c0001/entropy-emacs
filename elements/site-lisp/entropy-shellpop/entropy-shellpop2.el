;; -*- lexical-binding: t; -*-
(require 'entropy-shellpop2-core)
(require 'entropy-shellpop2-shell-type-vterm)
(require 'entropy-shellpop2-shell-type-eshell)
(require 'entropy-shellpop2-shell-type-emacs-shell)

(defvar entropy/shellpop2/main//var/show/shell/buffer/after/hook nil)

;; * lib

;;;###autoload
(defun entropy/shellpop2/main/show/shell/buffer (shell/type/name &optional index new)
  (let ((defdir default-directory) newp)
    (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
      (let ((buffobj-focus
             (if new nil
               (if (or (not index) (= index 0))
                   (entropy/shellpop2/core/api/obj/shell/type/op/get-focus)
                 (or (entropy/shellpop2/core/api/obj/shell/type/op/get-at-index
                      index)
                     (entropy/shellpop2/core/api/obj/shell/type/op/get-last)))))
            nbuff)
        (unless buffobj-focus
          (setq buffobj-focus
                (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
                    shell/type/name nil it)
                newp t)
          (setq nbuff
                (entropy/shellpop2/core/generic/shell/buffer/op/init
                 buffobj-focus))
          (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
              shell/type/name buffobj-focus
            (entropy/shellpop2/core/api/obj/shell/buffer/op/set-buffer
             nbuff 'inited))
          (entropy/shellpop2/core/api/obj/shell/type/op/set-focus
           buffobj-focus))
        (if (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
                shell/type/name buffobj-focus
              (let ((buff
                     (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer)))
                (and (buffer-live-p buff)
                     (get-buffer-window buff t))))
            (entropy/shellpop2/core/generic/shell/buffer/op/hide buffobj-focus)
          (entropy/shellpop2/core/generic/shell/buffer/op/display
           buffobj-focus))
        (unless newp
          (entropy/shellpop2/core/generic/shell/buffer/op/cwd
           buffobj-focus defdir))
        (when newp
          (with-current-buffer
              (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
                  shell/type/name buffobj-focus
                (entropy/shellpop2/core/api/obj/shell/buffer/op/get-buffer))
            (run-hooks 'entropy/shellpop2/main//var/show/shell/buffer/after/hook)))
        ))))

;;;###autoload
(defun entropy/shellpop2/main/hide/shell/buffer (shell/type/name)
  (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
    (when-let ((buffobj-focus
                (entropy/shellpop2/core/api/obj/shell/type/op/get-focus)))
      (entropy/shellpop2/core/generic/shell/buffer/op/hide buffobj-focus))))

;;;###autoload
(defun entropy/shellpop2/main/rename/shell/buffer
    (shell/type/name &optional index caption-string)
  (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
    (let ((buffobj
           (if (or (not index) (= index 0))
               (entropy/shellpop2/core/api/obj/shell/type/op/get-focus)
             (or (entropy/shellpop2/core/api/obj/shell/type/op/get-at-index
                  index)
                 (entropy/shellpop2/core/api/obj/shell/type/op/get-last)))))
      (unless buffobj (error "No shell buffer object found at index: %S" index))
      (entropy/shellpop2/core/macro/do-with/shell/buffer/obj shell/type/name buffobj
        (let ((old-capt (entropy/shellpop2/core/api/obj/shell/buffer/op/get-caption)))
          (entropy/shellpop2/core/api/obj/shell/buffer/op/set-caption
           (or caption-string
               (read-string
                (format "Input new caption (old is '%s'): " old-capt)
                nil nil nil t))))))))

;;;###autoload
(defun entropy/shellpop2/main/getfous (shell/type/name)
  (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
    (entropy/shellpop2/core/api/obj/shell/type/op/get-focus)))

;;;###autoload
(defun entropy/shellpop2/main/getfous/all ()
  (let (shell/type/name shell/buffer/obj rtn)
    (dolist (el (entropy/shellpop2/core/func/get/list/shell/type/name))
      (setq shell/type/name el
            shell/buffer/obj (entropy/shellpop2/main/getfous el))
      (push (cons shell/type/name shell/buffer/obj) rtn))
    rtn))

;;;###autoload
(defun entropy/shellpop2/main/setfocus
    (shell/type/name &optional shell/buffer/obj)
  (when shell/buffer/obj
    (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
      (entropy/shellpop2/core/macro/do-with/shell/buffer/obj shell/type/name shell/buffer/obj
        (unless (entropy/shellpop2/core/api/obj/shell/buffer/pred/valid-p)
          (error "shell/buffer/obj is invalid: %S" shell/buffer/obj)))
      (entropy/shellpop2/core/api/obj/shell/type/op/set-focus
       shell/buffer/obj))))

;;;###autoload
(defun entropy/shellpop2/main/setfous/all (shell/buffer/obj::alist)
  (let (shell/type/name shell/buffer/obj)
    (dolist (el shell/buffer/obj::alist)
      (setq shell/type/name (car el) shell/buffer/obj (cdr el))
      (entropy/shellpop2/main/setfocus
       shell/type/name shell/buffer/obj))))

;;;###autoload
(defun entropy/shellpop2/main/choose/shell/buffer/obj (shell/type/name)
  (entropy/shellpop2/core/macro/do-with/shell/type/obj shell/type/name
    (let ((l (entropy/shellpop2/core/api/obj/shell/type/op/get-list)) ll (i 0))
      (unless l
        (error "No shell/buffer/obj found for current shell/type/name `%S'"
               shell/type/name))
      (mapc
       (lambda (x)
         (entropy/shellpop2/core/macro/do-with/shell/buffer/obj
             shell/type/name x
           (push (cons (format
                        "%d: %s"
                        (cl-incf i)
                        (entropy/shellpop2/core/api/obj/shell/buffer/op/get-caption))
                       x)
                 ll))) l)
      (alist-get
       (or
        (completing-read (format "[%s] Choose buffer: " shell/type/name)
                         ll nil t "1" nil nil t)
        (user-error "No matched, Abort!"))
       ll nil nil 'string=))))

(defun entropy/shellpop2/main//get/current/shell/buffer/caption nil
  (and (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p)
       (entropy/shellpop2/core/macro/do-with/current/shell/buffer (current-buffer)
         (entropy/shellpop2/core/api/shell/buffer/op/get-caption))))

(defun entropy/shellpop2/main//get/current/shell/buffer/shell/type/name nil
  (and (entropy/shellpop2/core//func//current-buffer-is-inited/shell/buffer::p)
       (entropy/shellpop2/core/macro/do-with/current/shell/buffer (current-buffer)
         (entropy/shellpop2/core/api/shell/buffer/op/get/shell/type/name))))

;; * commands
;; ** core commands

(eval-and-compile
  (defmacro entropy/shellpop2/main//make-core-command-1 (shell/type/name)
    (macroexp-let2* ignore
        ((stn-sym shell/type/name)
         (fnm-sty-sym `(format "entropy/shellpop2/main/%s/command/core" ,stn-sym))
         (fsym-sym `(intern ,fnm-sty-sym)))
      `(prog1 ,fsym-sym
         (defalias ,fsym-sym
           (lambda (narg)
             (interactive "p")
             (cond ((eql narg 4)
                    (let ((shell/buffer/obj
                           (entropy/shellpop2/main/choose/shell/buffer/obj ,stn-sym)))
                      (entropy/shellpop2/core/macro/do-with/shell/type/obj ,stn-sym
                        (entropy/shellpop2/core/api/obj/shell/type/op/set-focus shell/buffer/obj)))
                    (entropy/shellpop2/main/show/shell/buffer ,stn-sym))
                   ((eql narg 16)
                    (entropy/shellpop2/main/show/shell/buffer ,stn-sym nil t))
                   (t
                    (entropy/shellpop2/main/show/shell/buffer ,stn-sym)))))))))

;;;###autoload
(defun entropy/shellpop2/main/make-core-command (shell/type/name)
  (entropy/shellpop2/main//make-core-command-1 shell/type/name))

;;;###autoload
(defun entropy/shellpop2/main/make-core-commands ()
  (let ((tl (entropy/shellpop2/core/func/get/list/shell/type/name)) rtn)
    (mapc (lambda (x)
            (push (cons x (entropy/shellpop2/main/make-core-command x)) rtn))
          tl)
    (nreverse rtn)))

;; ** rename command

(defun entropy/shellpop2/main/rename/current::shell/buffer/caption nil
  (interactive)
  (entropy/shellpop2/core/macro/do-with/current/shell/buffer (current-buffer)
    (entropy/shellpop2/core/api/shell/buffer/op/set-caption
     (read-string "Rename current shell buffer caption: "
                  (entropy/shellpop2/core/api/shell/buffer/op/get-caption)
                  nil nil t))))

;; * minor mode
;; ** mode line
(defface entropy/shellpop2/main//face/modeline-highlight-basic
  '((((class color) (min-colors 88) (background light))
     :weight light
     :box (:line-width -1 :color "grey75" :style nil))
    (((class color) (min-colors 88) (background dark) )
     :weight light
     :box (:line-width -1 :color "grey40" :style nil)))
  "")

(defface entropy/shellpop2/main//face/modeline-highlight-major-mode
  '((default
     :inherit
     entropy/shellpop2/main//face/modeline-highlight-basic)
    (((class color) (min-colors 88) (background light))
     :weight bold :foreground "red"
     :box (:line-width -1 :color "grey75" :style nil))
    (((class color) (min-colors 88) (background dark) )
     :weight bold :foreground "yellow"
     :box (:line-width -1 :color "grey40" :style nil)))
  "")

(defface entropy/shellpop2/main//face/modeline-highlight-working-on
  '((default
     :inherit
     entropy/shellpop2/main//face/modeline-highlight-basic)
    (((class color) (min-colors 88) (background light))
     :slant italic :weight bold)
    (((class color) (min-colors 88) (background dark) )
     :slant italic :weight bold))
  "")

(defface entropy/shellpop2/main//face/modeline-highlight-default-directory
  '((default
     :inherit
     entropy/shellpop2/main//face/modeline-highlight-basic)
    (((class color) (min-colors 88) (background light))
     :slant italic :weight light)
    (((class color) (min-colors 88) (background dark) )
     :slant italic :weight light))
  "")

(defun entropy/shellpop2/main//func/set-modeline-format ()
  (let* ((lhs
          '(format
            "%s %s%s %s%s %s%s%s"
            (propertize "*Shellpop*" 'face 'tty-menu-enabled-face)
            (propertize "Desc: " 'face 'success)
            (let ((orig-desc (entropy/shellpop2/main//get/current/shell/buffer/caption))
                  unset-p)
              (when (or (null orig-desc)
                        (string-empty-p orig-desc))
                (setq orig-desc "[No desc yet]"
                      unset-p t))
              (if unset-p
                  orig-desc
                (propertize orig-desc 'face 'warning)))
            (propertize "type: " 'face 'success)
            (propertize
             (format "%s"
                     (or (entropy/shellpop2/main//get/current/shell/buffer/shell/type/name)
                         "anonymous")
                     'face 'bold-italic))
            (propertize (format "@%s " major-mode)
                        'face 'entropy/shellpop2/main//face/modeline-highlight-major-mode)
            (propertize "Working on: " 'face 'entropy/shellpop2/main//face/modeline-highlight-working-on)
            (propertize default-directory
                        'face
                        'entropy/shellpop2/main//face/modeline-highlight-default-directory)))
         (rhs "")
         (mid-str (propertize " "
                              'display
                              `((space
                                 :align-to
                                 (- (+ right right-fringe right-margin)
                                    ,(string-width
                                      (format-mode-line (list :eval rhs)))
                                    ))))))
    (setq-local mode-line-format
                `("%e"
                  (:eval ,lhs)
                  ,mid-str
                  (:eval ,rhs)))))

;; ** def
(defvar entropy/shellpop2/main//keymap::minor-mode/core-mode
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-:") #'eval-expression)
    (define-key map (kbd "<f1>")
                #'entropy/shellpop2/main/rename/current::shell/buffer/caption)
    map))

(define-minor-mode entropy/shellpop2/main//minor-mode/core-mode
  "Popup shell buffer."
  :initial-value nil
  :keymap entropy/shellpop2/main//keymap::minor-mode/core-mode
  (if entropy/shellpop2/main//minor-mode/core-mode
      (entropy/shellpop2/main//func/set-modeline-format)
    nil))

(add-hook 'entropy/shellpop2/main//var/show/shell/buffer/after/hook
          #'entropy/shellpop2/main//minor-mode/core-mode)

;; * provide
(provide 'entropy-shellpop2)
