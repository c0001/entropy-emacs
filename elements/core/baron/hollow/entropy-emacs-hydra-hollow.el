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
  (cond ((and (listp pattern)
              (not (null pattern)))
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
    (heads-group &optional no-merge)
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
        (when enable
          (setq rtn (append rtn `((,group ,(cadr sp-head))))))))
    (when (not (null rtn))
      (unless no-merge
        (setq rtn
              (entropy/emacs-hydra-hollow-delete-empty-pretty-hydra-head-group
               (entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
                rtn)))))
    rtn))

(defmacro entropy/emacs-hydra-hollow-with-enabled-split-heads
    (pretty-heads-group &rest body)
  `(let ((enabled-split-pretty-heads
          (entropy/emacs-hydra-hollow-get-enabled-pretty-group-heads
           ,pretty-heads-group t)))
     ,@body))

;; **** heads predicate
;; ***** predicate defination

;; - =Predicate Functionn:=

;;   A function parse the riched-split-pretty-head and return the
;;   predicated one.

;;   A =riched-split-pretty-head= is a plist of three key, =:restrict=
;;   a list of symbols and the =:split-pretty-head= was the slot host
;;   the =split-pretty-head=, and the =:rest-args= a list of remaining
;;   args needed by current predicate function, thus for all, it forms
;;   as:

;;   (:restrict  (global-bind-notation-patched ...)
;;    :split-pretty-head ("Basic" (("1" (message "yes") "test message")))
;;    :rest-args (mode feature map))

(defvar entropy/emacs-hydra-hollow-predicate-union-form
  '(lambda ()))

(defvar entropy/emacs-hydra-hollow-predicative-keys
  '((:global-bind . entropy/emacs-hydra-hollow-global-bind-predicate)
    (:map-inject . entropy/emacs-hydra-hollow-map-inject-predicate)))

(defun entropy/emacs-hydra-hollow-global-bind-predicate
    (riched-split-pretty-head)
  (let* (
         ;; :restrict
         (restrict (plist-get riched-split-pretty-head :restrict))
         ;; :split-pretty-head
         (split-pretty-head (plist-get riched-split-pretty-head :split-pretty-head))
         (group (car split-pretty-head))
         (split-pretty-head-pattern (caadr split-pretty-head))
         (key (car split-pretty-head-pattern))
         (command (cadr split-pretty-head-pattern))
         (notation (caddr split-pretty-head-pattern))
         (split-pretty-head-plist (cdddr split-pretty-head-pattern))
         (global-bind-p (entropy/emacs-hydra-hollow--common-judge-p
                         (plist-get split-pretty-head-plist :global-bind)))
         ;; :rest-args
         (rest-args (plist-get riched-split-pretty-head :rest-args))
         )
    (when global-bind-p
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-head-notation-handler
             notation 'global-map-inject))
      (setq restrict (append restrict '(:global-bind-notation-beautified)))
      (setq entropy/emacs-hydra-hollow-predicate-union-form
            (append entropy/emacs-hydra-hollow-predicate-union-form
                    `((global-set-key (kbd ,key) #',command))))
      )
    (list :restrict restrict
          :split-pretty-head
          `(,group ((,key ,command ,notation ,@split-pretty-head-plist))))))

(defun entropy/emacs-hydra-hollow-map-inject-predicate
    (riched-split-pretty-head)
  (let* (
         ;; :restrict
         (restrict (plist-get riched-split-pretty-head :restrict))
         ;; :split-pretty-head
         (split-pretty-head (plist-get riched-split-pretty-head :split-pretty-head))
         (group (car split-pretty-head))
         (split-pretty-head-pattern (caadr split-pretty-head))
         (key (car split-pretty-head-pattern))
         (command (cadr split-pretty-head-pattern))
         (notation (caddr split-pretty-head-pattern))
         (split-pretty-head-plist (cdddr split-pretty-head-pattern))
         (map-inject (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get split-pretty-head-plist
                                 :map-inject)))
         ;; :rest-args
         (rest-args (plist-get riched-split-pretty-head :rest-args))
         (mode (car rest-args))
         (feature (cadr rest-args))
         (map (caddr rest-args))
         )
    (when map-inject
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-head-notation-handler
             notation 'mode-map-inject
             (member :global-bind-notation-beautified restrict)))
      (setq restrict
            (append restrict '(:map-inject-notation-beautified)))
      (setq entropy/emacs-hydra-hollow-predicate-union-form
            (append entropy/emacs-hydra-hollow-predicate-union-form
                    `((entropy/emacs-lazy-load-simple ,feature
                        (define-key ,map (kbd ,key) #',command)))))
      )

    (list :restrict restrict
          :split-pretty-head
          `(,group ((,key ,command ,notation ,@split-pretty-head-plist)))
          )))

;; ***** batch patch split-head

(defun entropy/emacs-hydra-hollow--sort-riched-split-head-predicate-pattern
    (riched-split-head-predicate-pattern)
  (let (rtn)
    (dolist (item entropy/emacs-hydra-hollow-predicative-keys)
      (when (assoc (car item) riched-split-head-predicate-pattern)
        (setq rtn
              (append rtn
                      (list (assoc (car item) riched-split-head-predicate-pattern))))))
    rtn))

(defun entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
    (pretty-heads-group riched-split-head-predicate-pattern &optional not-merge)
  (when (or (not (listp riched-split-head-predicate-pattern))
            (null (cl-delete nil riched-split-head-predicate-pattern)))
    (error "riched-split-head-predicate-pattern was fake!"))
  (entropy/emacs-hydra-hollow-with-enabled-split-heads
   pretty-heads-group
   (let ((sp-heads enabled-split-pretty-heads)
         (rshpp (entropy/emacs-hydra-hollow--sort-riched-split-head-predicate-pattern
                 riched-split-head-predicate-pattern))
         new-split-heads)
     (when sp-heads
       (dolist (head sp-heads)
         (let (head-of-rsh)
           (dolist (predicate-pattern rshpp)
             (let* ((key (car predicate-pattern))
                    (rest-args (cdr predicate-pattern))
                    (predicate-func (alist-get key entropy/emacs-hydra-hollow-predicative-keys))
                    (rsh (if (null head-of-rsh)
                             (list :rest nil
                                   :split-pretty-head
                                   head
                                   :rest-args
                                   rest-args)
                           (append head-of-rsh `(:rest-args ,rest-args)))))
               (setq head-of-rsh
                     (funcall predicate-func rsh))))
           (setq new-split-heads
                 (append new-split-heads
                         (list (plist-get head-of-rsh :split-pretty-head))))))
       (if not-merge
           new-split-heads
         (entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
          new-split-heads))))))

;; *** heads notation handler

(defun entropy/emacs-hydra-hollow-pretty-head-notation-handler
    (head-notation type &optional not-beautify-notation)
  (dolist (feature '(faces))
    (require feature))
  (let* ((match-map
          `((global-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "g" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face 'link)))
            (mode-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "m" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face 'link)))))
         (matched (alist-get type match-map))
         (fmstr (plist-get matched :format))
         (icon (funcall (plist-get matched :icon)))
         (notation (or (when not-beautify-notation
                         head-notation)
                       (funcall (plist-get matched :notation) head-notation))))
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
;; **** library

(defun entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
    (pattern-form)
  "A PATTERN-FORM was a ISLAND or a list of ISLANDs.

- Island

  consists of =Baron= and =Heads-Group=

  (list Baron Heads-Group)

- Baron

  consists of =Section= or =Section=s, each =Section= has a
  =Attribute= a plist and a =Request= a list.

  (list Attribute Request)

  or

  (list
    (list Attribute-0 Request-0)
    (list Attribute-1 Request-1)
    ...
  )


=Request= can be omitted or nil as element injected to the
pattern-form, for =Heads-Group= can not be ommited but for a
single 'nil' instead because the single =Island= pattern-fom with
multi =Sections= of =Baron= has same form type as the multi
=Islands= pattern-form of which the first =Island= is single
=Section= type when their's =Heads-Group= and inline =Request= are
both ommited, that as:

   1: multi =Section= of =Baron= of single =Island= as
   PATTERN-FORM with omitted =Heads-Group= and inline =Request=
   ((((:enable t)) ) b)

   2: multi =Island= PATTERN-FORM whose the first =Island= was
   single =Section= of =Baron= with omitted =Heads-Group= and
   inline =Request=
   ((((:enable t)) b) )
"
  (let* ((ptform-single-island-p
          (and (= (length pattern-form) 2)
               (or (ignore-errors (stringp (caadr pattern-form)))
                   (null (cadr pattern-form)))))
         (island-baron-section-single-p
          (lambda (island)
            (let ((baron (car island)))
              (and (or (= (length baron) 2)
                       (= (length baron) 1))
                   (symbolp (caar baron))))))
         (island-multi-sections-split-func
          (lambda (multi-secs-island)
            (let ((sections (car multi-secs-island))
                  (heads (cadr multi-secs-island))
                  split-island)
              (dolist (sec sections)
                (setq split-island
                      (append split-island (list (list sec heads)))))
              split-island)))
         (island-parse-func
          (lambda (island)
            (let (output)
              (if (funcall island-baron-section-single-p
                           island)
                  (setq output (list island))
                (setq output
                      (funcall island-multi-sections-split-func
                               island)))
              output)))
         rtn)
    (cond
     (ptform-single-island-p
      (setq rtn
            (funcall island-parse-func
                     pattern-form)))
     ((null ptform-single-island-p)
      (dolist (island pattern-form)
        (let ((obtain (funcall island-parse-func
                               island)))
          (setq rtn (append rtn obtain))))))
    rtn))


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
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (requests (cadr baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (heads (cadr island))
             (heads-append-arg `(,heads))
             (core-caller
              `(apply
                'entropy/emacs-hydra-hollow-add-for-top-dispatch
                ',heads-append-arg)))
        (when enable
          (setq init-form
                (append init-form
                        `(,core-caller))))))
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
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
          (t
           (error
            "eemacs mm common use-package clause form wrong type for '%s' def!"
            (symbol-name use-name))))))

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-mmphc-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         init-form)
    (add-to-list 'entropy/emacs-hydra-hollow--usepackage-eemamcs-mmc-arg-log
                 (list use-name :handle-arg $arg))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get attr :enable)))
             (requests (cadr baron))
             (mode (or (car requests)
                       use-name))
             (map (or (caddr requests)
                      (intern (format "%s-map" (symbol-name use-name)))))
             (feature (or (cadr requests)
                          use-name))
             (heads (cadr island)))
        (setq
         init-form
         (append init-form
                 `((when (not (null ',enable))
                     (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
                      ',mode ',feature ',map ',heads)))))))
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
           (entropy/emacs-hydra-hollow--usepackage-common-pattern-parse
            (car key-value)))
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
         (dolist (island ',$arg)
           (let* ((baron (car island))
                  (attr (car baron))
                  (enable (let ((enable-slot (plist-get attr :enable)))
                            (entropy/emacs-hydra-hollow--common-judge-p
                             enable-slot)))
                  (requests (cadr baron))
                  (mode (car requests))
                  (feature (cadr requests))
                  (map (caddr requests))
                  (heads (cadr island))
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
