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

(defun entropy/emacs-hydra-hollow--common-judge-p
    (pattern)
  (cond ((listp pattern)
         (funcall `(lambda () ,pattern)))
        ((symbolp pattern)
         (unless (null pattern)
           t))
        (t
         (error "Judge pattern can be only a form or a symbol!"))))

(defun entropy/emacs-hydra-hollow-delete-empty-pretty-hydra-head-group
    (pretty-heads-group)
  (cl-loop for cnt from 0 to (- (/ (length pretty-heads-group) 2) 1)
           if (not (null (nth (+ (* cnt 2) 1) pretty-heads-group)))
           collect (nth (* cnt 2) pretty-heads-group)
           and collect (nth (+ (* cnt 2) 1) pretty-heads-group)))

(defun entropy/emacs-hydra-hollow-gets-pretty-hydra-heads-keybind
    (pretty-heads-group refer-key)
  (let (group-extract-func
        group-element-parse-func
        return-var)
    (setq group-element-parse-func
          (lambda (x)
            (let ((command (cadr x))
                  (key (car x))
                  (refer-match (entropy/emacs-hydra-hollow--common-judge-p
                                (plist-get (cdddr x) refer-key))))
              (unless (symbolp command)
                (setq command `(lambda () ,command)))
              `(:command ,command :key ,key ,refer-key ,refer-match)))
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

(defun entropy/emacs-hydra-hollow-split-pretty-hydra-group-heads
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

(defun entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
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
         (entropy/emacs-hydra-hollow-split-pretty-hydra-group-heads
          heads-group))
        rtn)
    (dolist (sp-head split-heads)
      (let* ((group (car sp-head))
             (head-pattern (cdddr (caadr sp-head)))
             (enable (let ((enable-slot (plist-get head-pattern :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot))))
        (if enable
            (setq rtn (append rtn `((,group ,(cadr sp-head)))))
          (setq rtn (append rtn `((,group nil)))))))
    (setq rtn
          (entropy/emacs-hydra-hollow-delete-empty-pretty-hydra-head-group
           (entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
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

(defun entropy/emacs-hydra-hollow-patch-pretty-hydra-heads-group-notations
    (pretty-heads-group refer-key notation-type)
  (let ((split-heads (copy-tree
                      (entropy/emacs-hydra-hollow-split-pretty-hydra-group-heads
                       pretty-heads-group)))
        patched-heads-group)
    (dolist (head split-heads)
      (let* ((head-plist (caadr head))
             (head-attr (cdddr head-plist))
             (refer-match (entropy/emacs-hydra-hollow--common-judge-p
                           (plist-get head-attr refer-key)))
             (notation (caddr head-plist))
             new-heads-plist)
        (setq new-heads-plist
              (append
               (list
                (car head-plist)
                (cadr head-plist)
                (if refer-match
                    (entropy/emacs-hydra-hollow-pretty-head-notation-handler
                     notation notation-type)
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
          (entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
           patched-heads-group))
    patched-heads-group))

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

(defun entropy/emacs-hydra-hollow-patch-global-inject-group-heads-notations
    (pretty-heads-group)
  (entropy/emacs-hydra-hollow-patch-pretty-hydra-heads-group-notations
   pretty-heads-group :global-bind 'global-map-inject))

(defun entropy/emacs-hydra-hollow-add-for-top-dispatch
    (pretty-heads-group)
  (let* ((notation-patched-heads-group
          (entropy/emacs-hydra-hollow-patch-global-inject-group-heads-notations
           pretty-heads-group))
         (enabled-heads-group
          (entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
           notation-patched-heads-group)))
    (unless (null enabled-heads-group)
      (entropy/emacs-hydra-hollow-init-top-dispatch)
      (dolist (split-head (entropy/emacs-hydra-hollow-split-pretty-hydra-group-heads
                           enabled-heads-group))
        (setq entropy/emacs-hydra-hollow-top-dispatch-register
              (append entropy/emacs-hydra-hollow-top-dispatch-register
                      `(,split-head))))
      (funcall
       `(lambda ()
          (pretty-hydra-define+ entropy/emacs-hydra-hollow-top-dispatch
            nil
            ,enabled-heads-group)))
      (let ((global-binds (entropy/emacs-hydra-hollow-gets-pretty-hydra-heads-keybind
                           enabled-heads-group :global-bind)))
        (dolist (bind global-binds)
          (let ((key (plist-get bind :key))
                (command (plist-get bind :command))
                (inject-p (plist-get bind :global-bind)))
            (when inject-p
              (global-set-key (kbd key) command))))))))

;; *** majro mode dispacher
;; **** library
;; ***** patch hint doc
(defun entropy/emacs-hydra-hollow-patch-mode-inject-group-heads-notations
    (pretty-heads-group)
  (entropy/emacs-hydra-hollow-patch-pretty-hydra-heads-group-notations
   pretty-heads-group :map-inject 'mode-map-inject))

;; **** define major mode hydra

(cl-defmacro entropy/emacs-hydra-hollow--define-major-mode-hydra-macro
    (mode feature mode-map body heads-plist)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-patch-mode-inject-group-heads-notations
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
         (let ((binds (entropy/emacs-hydra-hollow-gets-pretty-hydra-heads-keybind
                       ',heads-plist :map-inject)))
           (dolist (el binds)
             (let ((command (plist-get el :command))
                   (key (plist-get el :key))
                   (map-inject (plist-get el :map-inject)))
               (when (and map-inject command key)
                 (message "Binding key '%s' of command '%s' to map '%s' ..."
                          key (symbol-name command) (symbol-name ',mode-map))
                 (define-key ,mode-map (kbd key) command)))))))))

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode feature mode-map body heads-plist)
  (let ((heads-plist
         (entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
          heads-plist)))
    (unless (null heads-plist)
      (funcall
       `(lambda ()
          (entropy/emacs-hydra-hollow--define-major-mode-hydra-macro
           ,mode ,feature ,mode-map ,body ,heads-plist))))))

;; **** add major mode hydra
(cl-defmacro entropy/emacs-hydra-hollow--add-to-major-mode-hydra-macro
    (mode feature mode-map heads-plist &optional hydra-body)
  (let ((binds (entropy/emacs-hydra-hollow-gets-pretty-hydra-heads-keybind
                heads-plist :map-inject))
        (patched-heads-group
         (entropy/emacs-hydra-hollow-patch-mode-inject-group-heads-notations
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

(defun entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    (mode feature mode-map heads-plist &optional hydra-body)
  (let ((heads-plist
         (entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
          heads-plist)))
    (unless (null heads-plist)
      (funcall
       `(lambda ()
          (entropy/emacs-hydra-hollow--add-to-major-mode-hydra-macro
           ,mode ,feature ,mode-map ,heads-plist ,hydra-body))))))

;; **** sparse tree builder

(cl-defmacro entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-macro
    (mode feature mode-map)
  (let ((body
         (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
          mode)))
    `(progn
       (unless (alist-get ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode
                     ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register))
       (entropy/emacs-hydra-hollow-define-major-mode-hydra
        ',mode ',feature ',mode-map
        ',body
        '("Help"
          (("C-h M-m" discover-my-major "Show Keybinds For Current Major Mode"
            :exit t
            :enable t)
           ("C-h M-M" discover-my-mode "Show Keybinds For Enabled Minor Mode"
            :exit t
            :enable t))))
       )))

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    (mode feature mode-map &optional heads)
  (let ((heads
         (entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
          heads)))
    (funcall
     `(lambda ()
        (entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-macro
         ,mode ,feature ,mode-map)))
    (when heads
      (funcall
       `(lambda ()
          (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
           ',mode ',feature ',mode-map ',heads))))))

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
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (spec (cadr item)))
        (when enable
          (setq init-form
                (append init-form
                        `((lambda ()
                            ,(append
                              '(apply)
                              '('entropy/emacs-hydra-hollow-add-for-top-dispatch)
                              (list (list 'quote spec))))))))))
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
         (enable (entropy/emacs-hydra-hollow--common-judge-p
                  (plist-get $arg :enable)))
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
    (setq
     init-form
     `((when (not (null ',enable))
         (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
          ',mode ',feature ',map ',heads))))
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
                            (entropy/emacs-hydra-hollow--common-judge-p
                             enable-slot)))
                  (pattern (cadr baron))
                  (mode (car pattern))
                  (feature (cadr pattern))
                  (map (caddr pattern))
                  (heads (cadr item))
                  run-call)
             (when enable
               (setq run-call
                     (list 'lambda '()
                           (list 'apply
                                 (list 'function 'entropy/emacs-hydra-hollow-add-to-major-mode-hydra)
                                 (list 'quote (list mode feature map heads)))))
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
