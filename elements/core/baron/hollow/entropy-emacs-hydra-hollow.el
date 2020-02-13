;; * code
;; ** require
(require 'entropy-emacs-defun)
(require 'entropy-emacs-utils)
(require 'cl-lib)

;; ** defvar
(defvar entropy/emacs-hydra-hollow-top-dispatch-register nil)
(defvar entropy/emacs-hydra-hollow-top-dispatch-init-done nil)

;; ** libraries
(defun entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
    (pretty-heads-groups)
  (let (group-extract-func
        group-element-parse-func
        return-var)
    (setq group-element-parse-func
          (lambda (x)
            (let ((command (cadr x))
                  (key (car x)))
              (unless (symbolp command)
                (setq command `(lambda () ,command)))
              `(:command ,command :key ,key)))
          group-extract-func
          (lambda (x)
            (let (rtn)
              (dolist (el x)
                (when (and (listp el)
                           (not (null (car el)))
                           (not (stringp el)))
                  (push el rtn)))
              rtn)))
    (dolist (el (funcall group-extract-func pretty-heads-groups))
      (dolist (elt el)
        (push (funcall group-element-parse-func elt)
              return-var)))
    return-var))

(defun entropy/emacs-hydra-hollow--merge-pretty-hydra-sparse-heads
    (pretty-heads-list)
  (let* ((manipulate-list (copy-tree pretty-heads-list))
         pretty-heads-group-fake rtn)
    (while (not (null manipulate-list))
      (let ((group (caar manipulate-list))
            (rest (cdr manipulate-list))
            pointer-list collect)
        (if (eq (length manipulate-list) 1)
            (push (pop manipulate-list) pretty-heads-group-fake)
          (cl-loop for pt from 0 to (- (length rest) 1)
                   when (equal (car (nth pt rest)) group)
                   do (push pt pointer-list))
          (if (null pointer-list)
              (push (car manipulate-list) pretty-heads-group-fake)
            (progn
             (dolist (el pointer-list)
               (push (cadr (nth el rest)) collect))
             (push (cadar manipulate-list) collect))
            (when collect
              (push (list group collect) pretty-heads-group-fake))
            (progn
              (dolist (pt pointer-list)
                (entropy/emacs-setf-for-nth
                 (+ pt 1) nil manipulate-list))
              (pop manipulate-list)
              (setq manipulate-list
                    (delete* nil manipulate-list)))))))
    (dolist (el pretty-heads-group-fake)
      (if (listp (caadr el))
          (setq rtn (append rtn el))
        (setq rtn (append rtn (list (car el) (cdr el))))))
    rtn))

;; ** apis
;; *** top dispatcher

(defun entropy/emacs-hydra-hollow-init-top-dispatch (&optional force)
  (when (or (not entropy/emacs-hydra-hollow-top-dispatch-init-done)
            force)
    (pretty-hydra-define entropy/emacs-hydra-hollow-top-dispatch
      (:title
       (entropy/emacs-pretty-hydra-make-title
        "eemacs top dispatch" 'faicon "toggle-on")
       :color ambranth
       :quit-key "q")
      ("Basic"     ()
       "Highlight" ()
       "Coding"    ()
       "WWW"       ()
       "Theme"     ()
       "Misc."     ()))
    (unless entropy/emacs-hydra-hollow-top-dispatch-init-done
      (setq entropy/emacs-hydra-hollow-top-dispatch-init-done t)
      (entropy/emacs-!set-key
        (kbd "h")
        #'entropy/emacs-hydra-hollow-top-dispatch/body))))

(defun entropy/emacs-hydra-hollow-remap-top-dispatch ()
  (interactive)
  (entropy/emacs-hydra-hollow-init-top-dispatch t)
  (dolist (head entropy/emacs-hydra-hollow-top-dispatch-register)
    (let (form)
      (setq form
            `(lambda ()
               (pretty-hydra-define+ entropy/emacs-hydra-hollow-top-dispatch
                 nil
                 ,head)))
      (funcall form))))

(cl-defmacro entropy/emacs-hydra-hollow-add-for-top-dispatch
    (group &key notation key command toggle exit)
  (declare (indent 1))
  `(progn
     (entropy/emacs-hydra-hollow-init-top-dispatch)
     (add-to-list
      'entropy/emacs-hydra-hollow-top-dispatch-register
      '(,group
        ((,key ,command ,notation
               :toggle ,toggle :exit ,exit
               :face entropy/emacs-defface-face-for-hydra-heads-orange-face))))
     (pretty-hydra-define+ entropy/emacs-hydra-hollow-top-dispatch
       nil
       (,group
        ((,key ,command ,notation
               :toggle ,toggle :exit ,exit
               :face entropy/emacs-defface-face-for-hydra-heads-orange-face))))
     (when (or (string= ,group "Basic")
               (string= ,group "WWW"))
       (global-set-key (kbd ,key) #',command))))

;; *** majro mode dispacher

(cl-defmacro entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode mode-map body heads-plist)
  `(let ((binds (entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
                 ',heads-plist)))
     (major-mode-hydra-define ,mode
       ,body
       ,heads-plist)
     (dolist (el binds)
       (let ((command (plist-get el :command))
             (key (plist-get el :key)))
         (define-key ,mode-map (kbd key) command)))))

;; * provide
(provide 'entropy-emacs-hydra-hollow)
