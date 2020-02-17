;; * code
;; ** require
(require 'entropy-emacs-defun)
(require 'entropy-emacs-utils)
(require 'cl-lib)
(require 'use-package)

;; ** defvar
(defvar entropy/emacs-hydra-hollow-top-dispatch-register nil)
(defvar entropy/emacs-hydra-hollow-top-dispatch-init-done nil)

(defvar entropy/emacs-hydra-hollow-major-mode-body-register nil)

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
                  (cond ((not (null heads))
                         (dolist (el heads)
                           (push (list group (list el)) rtn)))
                        (t
                         (push (list group '()) rtn)))
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
        "eemacs top dispatch" "faicon" "toggle-on")
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
;; **** library
;; ***** patch hint doc
(defun entropy/emacs-hydra-hollow-patch-mode-inject-group-heads
    (pretty-heads-group)
  (let ((split-heads (copy-tree
                      (entropy/emacs-hydra-hollow--split-pretty-hydra-group-heads
                       pretty-heads-group)))
        patched-heads-group)
    (dolist (head split-heads)
      (let* ((head-plist (caadr head))
             (head-attr (cdddr head-plist))
             (map-inject (plist-get head-attr :map-inject))
             (notation (caddr head-plist))
             new-heads-plist)
        (setq new-heads-plist
              (append
               (list
                (car head-plist)
                (cadr head-plist)
                (if map-inject
                    (format "%s (m)" notation)
                  notation))
               (cdddr head-plist)))
        (if (not (null head-plist))
            (push
             (list
              (car head)
              (list
               new-heads-plist))
             patched-heads-group)
          (push (list (car head) nil) patched-heads-group))))
    (setq patched-heads-group (reverse patched-heads-group)
          patched-heads-group
          (entropy/emacs-hydra-hollow--merge-pretty-hydra-sparse-heads
           patched-heads-group))
    patched-heads-group))

;; **** define major mode hydra

(cl-defmacro entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode feature mode-map body heads-plist)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-patch-mode-inject-group-heads
          heads-plist)))
    `(let ()
       ;; Define major-mode-hydra before lazy loading feature prevent
       ;; hydra adding cover its body
       (major-mode-hydra-define ,mode
         ,body
         ,patched-heads-group)

       (unless (alist-get  ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register))

       (entropy/emacs-lazy-load-simple ,feature
         (let ((binds (entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
                       ',heads-plist)))
           (dolist (el binds)
             (let ((command (plist-get el :command))
                   (key (plist-get el :key))
                   (map-inject (plist-get el :map-inject)))
               (when (and map-inject command key)
                 (message "Binding key '%s' of command '%s' to map '%s' ..."
                          key (symbol-name command) (symbol-name ',mode-map))
                 (define-key ,mode-map (kbd key) command)))))))))

;; **** add major mode hydra
(cl-defmacro entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    (mode feature mode-map heads-plist &optional hydra-body)
  (let ((binds (entropy/emacs-hydra-hollow--gets-pretty-hydra-heads-keybind
                heads-plist))
        (patched-heads-group
         (entropy/emacs-hydra-hollow-patch-mode-inject-group-heads
          heads-plist))
        (body (or hydra-body
                  (alist-get
                   mode
                   entropy/emacs-hydra-hollow-major-mode-body-register
                   ))))
    `(let ()
       (entropy/emacs-lazy-load-simple ,feature
         ;; add hydra for feature with lazy load prevent covering the
         ;; major defination
         (major-mode-hydra-define+ ,mode
           ,body
           ,patched-heads-group)
         (dolist (el ',binds)
           (let ((command (plist-get el :command))
                 (key (plist-get el :key))
                 (map-inject (plist-get el :map-inject)))
             (when map-inject
               (define-key ,mode-map (kbd key) command))))))))

;; **** sparse tree builder

(cl-defmacro entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    (mode feature mode-map &optional heads)
  (let ((body
         `(:title
           (entropy/emacs-pretty-hydra-make-title-for-major-mode-common
            ',mode (format "%s Actions" (symbol-name ',mode)))
           :color ambranth
           :quit-key "q")))
    `(progn
       (unless (alist-get ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode
                     ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register))
       (entropy/emacs-hydra-hollow-define-major-mode-hydra
        ,mode ,feature ,mode-map
        ,body
        ("Baisc"      ()
         "Company"    ()
         "IDE"        ()
         "Navigation" ()
         "Misc."      ()))
       (when (not (null ',heads))
         ;; We forced tranferred the 'body' arg to
         ;; `entropy/emacs-hydra-hollow-add-to-major-mode-hydra'
         ;; prevent from that its a macro will expand while this
         ;; procedure process
         (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
          ,mode ,feature ,mode-map ,heads ,body))
       )))

;; **** use-package extended
;; ***** :eemacs-mmc
(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :bind-keymap*)
                 collect :bind-keymap* and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log
                        (list use-name :normalize-arg key-value))
           (car key-value))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (enable (plist-get $arg :enable))
         (mode (or (plist-get $arg :mode)
                   use-name))
         (map (or (plist-get $arg :map)
                  (intern (format "%s-map" (symbol-name use-name)))))
         (feature (or (plist-get $arg :feature)
                      use-name))
         (heads (plist-get $arg :heads))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log
                 (list use-name :handle-arg $arg))
    (when (listp enable)
      (setq enable (funcall `(lambda () ,enable))))
    (setq
     init-form
     `((when (not (null ',enable))
         (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
          ,mode ,feature ,map ,heads))))
    (use-package-concat
     rest-body
     init-form)))

(defalias 'use-package-normalize/:eemacs-mmc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-def-normalize)

(defalias 'use-package-handler/:eemacs-mmc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmc-add-keyword
 :eemacs-mmc)


;; ***** :eemacs-mmca

(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmca-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
        ;; should go in the same location as :eemacs-mmc
        (cl-loop for item in use-package-keywords
                 if (eq item :eemacs-mmc)
                 collect :eemacs-mmc and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-mmca-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log
                        (list use-name :normalize-arg key-value))
           (let ((arg-list (car key-value))
                 rtn split-func)
             (setq split-func
                   (lambda (x)
                     (let ((patterns (car x))
                           (heads (cadr x))
                           output)
                       (dolist (el patterns)
                         (push (list el heads) output))
                       output)))

             (dolist (el arg-list)
               (let ((pattern-group-car (ignore-errors (caar el))))
                 (cond
                  ((and pattern-group-car
                        (listp pattern-group-car))
                   (setq rtn (append rtn (reverse (funcall split-func el)))))
                  (t
                   (setq rtn (append rtn (list el)))))))
             rtn))
          (t
           (error
            "eemacs mmca common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-mmca-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log
                 (list use-name :handle-arg $arg))
    (setq
     init-form
     `((let (_callers)
         (dolist (item ',$arg)
           (let* ((pattern (car item))
                  (heads (cadr item))
                  (mode (car pattern))
                  (feature (cadr pattern))
                  (map (caddr pattern))
                  run-call)
             (setq run-call
                   (list 'lambda '()
                         (list 'entropy/emacs-hydra-hollow-add-to-major-mode-hydra
                               mode feature map heads)))
             (push run-call _callers)))
         (dolist (caller (reverse _callers))
           (funcall caller)))))
    (use-package-concat
     rest-body
     init-form)))

(defalias 'use-package-normalize/:eemacs-mmca
  #'entropy/emacs-hydra-hollow--usepackage-mmca-def-normalize)

(defalias 'use-package-handler/:eemacs-mmca
  #'entropy/emacs-hydra-hollow--usepackage-mmca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmca-add-keyword
 :eemacs-mmca)



;; * provide
(provide 'entropy-emacs-hydra-hollow)
