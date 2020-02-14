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
    (pretty-heads-group)
  (let (group-extract-func
        group-element-parse-func
        return-var)
    (setq group-element-parse-func
          (lambda (x)
            (let ((command (cadr x))
                  (key (car x))
                  (map-inject (plist-get (cdddr x) :map-inject)))
              (unless (symbolp command)
                (setq command `(lambda () ,command)))
              `(:command ,command :key ,key :map-inject ,map-inject)))
          group-extract-func
          (lambda (x)
            (let (rtn)
              (dolist (el x)
                (when (and (listp el)
                           (not (null (car el)))
                           (not (stringp el)))
                  (push el rtn)))
              rtn)))
    (dolist (el (funcall group-extract-func pretty-heads-group))
      (dolist (elt el)
        (push (funcall group-element-parse-func elt)
              return-var)))
    return-var))

(defun entropy/emacs-hydra-hollow--split-pretty-hydra-group-heads
    (pretty-heads-group)
  (let (split-heads)
    (cl-loop for cnt from 0 to (- (/ (length pretty-heads-group) 2) 1)
             do (let* ((cnt (* cnt 2))
                       (group (nth cnt pretty-heads-group))
                       (heads (nth (+ cnt 1) pretty-heads-group))
                       rtn)
                  (dolist (el heads)
                    (push (list group (list el)) rtn))
                  (when rtn
                    (setq split-heads (append split-heads (reverse rtn))))))
    split-heads))

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
              (push (pop manipulate-list) pretty-heads-group-fake)
            (progn
             (dolist (el pointer-list)
               (push (caadr (nth el rest)) collect))
             (push (caadar manipulate-list) collect))
            (when collect
              (push (list group collect) pretty-heads-group-fake))
            (progn
              (dolist (pt pointer-list)
                (entropy/emacs-setf-for-nth
                 (+ pt 1) nil manipulate-list))
              (pop manipulate-list)
              (setq manipulate-list
                    (cl-delete nil manipulate-list)))))))
    (dolist (el (reverse pretty-heads-group-fake))
      (setq rtn (append rtn el)))
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
       "Pyim"      ()
       "Highlight" ()
       "WWW"       ()
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
    (group &key notation key command toggle exit global-bind)
  (declare (indent 1))
  (let ((notation
         (if global-bind
             (format "%s (g)" notation)
           notation)))
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
       (when ,global-bind
         (global-set-key (kbd ,key) #',command)))))

;; *** majro mode dispacher

(cl-defmacro entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode feature mode-map body heads-plist)
  (let ((split-heads (copy-tree
                      (entropy/emacs-hydra-hollow--split-pretty-hydra-group-heads
                       heads-plist)))
        patched-heads-group)
    
    (dolist (head split-heads)
      (let* ((head-plist (caadr head))
             (head-attr (cdddr head-plist))
             (map-inject (plist-get head-attr :map-inject))
             (notation (caddr head-plist)))
        (push 
         (list
          (car head)
          (list
           (append
            (list
             (car head-plist)
             (cadr head-plist)
             (if map-inject
                 (format "%s (m)" notation)
               notation))
            (cdddr head-plist))))
         patched-heads-group)))

    (setq patched-heads-group (reverse patched-heads-group)
          patched-heads-group
          (entropy/emacs-hydra-hollow--merge-pretty-hydra-sparse-heads
           patched-heads-group))           

    `(let ()
       (entropy/emacs-lazy-load-simple ',feature
         (let ((binds (entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
                       ',heads-plist)))

           (major-mode-hydra-define ,mode
             ,body
             ,patched-heads-group)              
           
           (dolist (el binds)
             (let ((command (plist-get el :command))
                   (key (plist-get el :key))
                   (map-inject (plist-get el :map-inject)))
               (when map-inject
                 (message "Binding key '%s' of command '%s' to map '%s' ..."
                          key (symbol-name command) (symbol-name ',mode-map))
                 (define-key ,mode-map (kbd key) command)))))))))

(cl-defmacro entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    (mode mode-map heads-plist)
  `(let ((binds (entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
                 ',heads-plist)))
     (major-mode-hydra-define+ ,mode
       nil
       ,heads-plist)
     (dolist (el binds)
       (let ((command (plist-get el :command))
             (key (plist-get el :key))
             (map-inject (plist-get el :map-inject)))
         (when map-inject
           (define-key ,mode-map (kbd key) command))))))

;; * provide
(provide 'entropy-emacs-hydra-hollow)
