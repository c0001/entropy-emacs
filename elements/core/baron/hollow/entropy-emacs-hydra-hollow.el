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
;; *** pretty hydra heads manipulation
;; **** core
(defun entropy/emacs-hydra-hollow--delete-empty-pretty-hydra-head-group
    (pretty-heads-group)
  (cl-loop for cnt from 0 to (- (/ (length pretty-heads-group) 2) 1)
           if (not (null (nth (+ (* cnt 2) 1) pretty-heads-group)))
           collect (nth (* cnt 2) pretty-heads-group)
           and collect (nth (+ (* cnt 2) 1) pretty-heads-group)))

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
              (push (list group
                          (cl-delete nil collect))
                    pretty-heads-group-fake))
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


;; **** wapper

(defun entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
    (heads-group)
  (let ((split-heads
         (entropy/emacs-hydra-hollow--split-pretty-hydra-group-heads
          heads-group))
        rtn)
    (dolist (sp-head split-heads)
      (let* ((group (car sp-head))
             (head-pattern (cdddr (caadr sp-head)))
             (enable (let ((enable-slot (plist-get head-pattern :enable)))
                       (if (listp enable-slot)
                           (funcall `(lambda () ,enable-slot))
                         (if (null enable-slot)
                             nil
                           t)))))
        (if enable
            (setq rtn (append rtn `((,group ,(cadr sp-head)))))
          (setq rtn (append rtn `((,group nil)))))))
    (setq rtn
          (entropy/emacs-hydra-hollow--delete-empty-pretty-hydra-head-group
           (entropy/emacs-hydra-hollow--merge-pretty-hydra-sparse-heads
            rtn)))))


;; *** heads notation handler

(defun entropy/emacs-hydra-hollow-pretty-head-notation-handler
    (head-notation type)
  (dolist (feature '(faces))
    (require feature))
  (let* ((match-map
          `((global-map-inject
             :format "%s %s"
             :icon (lambda () (propertize "⭮" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face 'link)))
            (mode-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "⭥" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face 'link)))))
         (matched (alist-get type match-map))
         (fmstr (plist-get matched :format))
         (icon (funcall (plist-get matched :icon)))
         (notation (funcall (plist-get matched :notation) head-notation)))
    (format fmstr icon notation)))


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
             (entropy/emacs-hydra-hollow-pretty-head-notation-handler
              notation 'global-map-inject)
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
                    (entropy/emacs-hydra-hollow-pretty-head-notation-handler
                     notation 'mode-map-inject)
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
                   )
                  (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
                   mode))))
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
         (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
          mode)))
    `(progn
       (unless (alist-get ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode
                     ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register))
       (entropy/emacs-hydra-hollow-define-major-mode-hydra
        ,mode ,feature ,mode-map
        ,body
        ("Help"
         (("C-h M-m" discover-my-major "Show Keybinds For Current Major Mode"
           :exit t)
          ("C-h M-M" discover-my-mode "Show Keybinds For Enabled Minor Mode"
           :exit t))))
       (when (not (null ',heads))
         ;; We forced tranferred the 'body' arg to
         ;; `entropy/emacs-hydra-hollow-add-to-major-mode-hydra'
         ;; prevent from that its a macro will expand while this
         ;; procedure process
         (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
          ,mode ,feature ,mode-map ,heads ,body))
       )))

;; *** use-package extended
;; **** :eemacs-tpha

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :config)
                 collect :config and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-normalize
    (use-name key key-value)
  (let ()
    (cond ((and (listp key-value)
                (= 1 (length key-value)))
           (car key-value))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (item $arg)
      (let* ((condition (car item))
             (enable (let ((enable-slot (plist-get condition :enable)))
                       (cond ((listp enable-slot)
                              (funcall `(lambda () ,enable-slot)))
                             (t
                              (if (null enable-slot)
                                  nil
                                t)))))
             (spec (cadr item)))
        (when enable
          (setq init-form
                (append init-form
                        `((lambda ()
                            ,(append '(entropy/emacs-hydra-hollow-add-for-top-dispatch)
                                     spec))))))))
    (use-package-concat
     rest-body
     init-form)))

(defalias 'use-package-normalize/:eemacs-tpha
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-normalize)

(defalias 'use-package-handler/:eemacs-tpha
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-tpha)



;; **** :eemacs-mmphc
(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-add-keyword (keyword)
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

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-normalize
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

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-handler
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

(defalias 'use-package-normalize/:eemacs-mmphc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-normalize)

(defalias 'use-package-handler/:eemacs-mmphc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-add-keyword
 :eemacs-mmphc)

;; **** :eemacs-mmphca

(defvar entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log nil)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
        ;; should go in the same location as :eemacs-mmc
        (cl-loop for item in use-package-keywords
                 if (eq item :eemacs-mmphc)
                 collect :eemacs-mmphc and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-normalize
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
                     (let ((barons (car x))
                           (heads (cadr x))
                           output)
                       (dolist (el barons)
                         (push (list el heads) output))
                       output)))

             (dolist (el arg-list)
               (let ((pattern-group-car (ignore-errors (caaar el))))
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

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmca-arg-log
                 (list use-name :handle-arg $arg))
    (setq
     init-form
     `((let (_callers)
         (dolist (item ',$arg)
           (let* ((baron (car item))
                  (condition (car baron))
                  (enable (let ((enable-slot (plist-get condition :enable)))
                            (cond ((listp enable-slot)
                                   (funcall `(lambda () ,enable-slot)))
                                  (t
                                   (if (null enable-slot)
                                       nil
                                     t)))))
                  (pattern (cadr baron))
                  (mode (car pattern))
                  (feature (cadr pattern))
                  (map (caddr pattern))
                  (heads (cadr item))
                  run-call)
             (when enable
               (setq run-call
                     (list 'lambda '()
                           (list 'entropy/emacs-hydra-hollow-add-to-major-mode-hydra
                                 mode feature map heads)))
               (push run-call _callers))))
         (when (not (null _callers))
           (dolist (caller (reverse _callers))
             (funcall caller))))))
    (use-package-concat
     rest-body
     init-form)))

(defalias 'use-package-normalize/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-normalize)

(defalias 'use-package-handler/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-add-keyword
 :eemacs-mmphca)



;; * provide
(provide 'entropy-emacs-hydra-hollow)
