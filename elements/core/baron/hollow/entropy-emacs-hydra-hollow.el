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

(defvar entropy/emacs-hydra-hollow-union-form
  '(lambda (&rest _)))

(defvar entropy/emacs-hydra-hollow-call-union-form-adviced-list nil)

;; ** libraries

(defun entropy/emacs-hydra-hollow-call-union-form (&rest _)
  (let ((copy-union-form (copy-tree entropy/emacs-hydra-hollow-union-form)))
    (unless (null (cddr copy-union-form))
      (progn
        (funcall entropy/emacs-hydra-hollow-union-form)
        (unless (equal copy-union-form entropy/emacs-hydra-hollow-union-form)
          (dolist (form (cddr copy-union-form))
            (setq temp/x1 2)
            (setq entropy/emacs-hydra-hollow-union-form
                  (cl-delete form entropy/emacs-hydra-hollow-union-form
                             :test 'equal)))
          (entropy/emacs-hydra-hollow-call-union-form))
        (unless (eq entropy/emacs-hydra-hollow-union-form
                    '(lambda (&rest _)))
          (setq entropy/emacs-hydra-hollow-union-form
                '(lambda (&rest _))))))))

(defun entropy/emacs-hydra-hollow-advice-for-call-union-form (func)
  (unless (member
           func
           entropy/emacs-hydra-hollow-call-union-form-adviced-list)
    (progn (advice-add func :before #'entropy/emacs-hydra-hollow-call-union-form)
           (push func entropy/emacs-hydra-hollow-call-union-form-adviced-list))))

(defun entropy/emacs-hydra-hollow-func-version-pthydra-define
    (name body heads-plist)
  (funcall
   `(lambda ()
      (pretty-hydra-define
        ,name ,body ,heads-plist))))

(defun entropy/emacs-hydra-hollow-func-version-pthydra-define+
    (name body heads-plist)
  (funcall
   `(lambda ()
      (pretty-hydra-define+
        ,name ,body ,heads-plist))))

;; *** pretty hydra heads manipulation
;; **** core

(defun entropy/emacs-hydra-hollow--common-judge-p
    (pattern)
  (cond ((and (listp pattern)
              (eq (car pattern) :data))
         (cdr pattern))
        ((and (listp pattern)
              (not (null pattern)))
         (funcall `(lambda () ,pattern)))
        ((and (symbolp pattern)
              (member pattern '(nil t)))
         (unless (null pattern)
           t))
        ((symbolp pattern)
         (symbol-value pattern))
        (t
         pattern)))

(defun entropy/emacs-hydra-hollow-partion-pretty-heads-group
    (pretty-heads-group)
  (let (rtn (cnt 0))
    (while (< cnt (/ (length pretty-heads-group) 2))
      (push (list (nth (* cnt 2) pretty-heads-group)
                  (nth (+ 1 (* cnt 2)) pretty-heads-group))
            rtn)
      (cl-incf cnt))
    (reverse rtn)))

(defun entropy/emacs-hydra-hollow-delete-empty-pretty-hydra-head-group
    (pretty-heads-group)
  (cl-loop for cnt from 0 to (- (/ (length pretty-heads-group) 2) 1)
           if (not (null (nth (+ (* cnt 2) 1) pretty-heads-group)))
           collect (nth (* cnt 2) pretty-heads-group)
           and collect (nth (+ (* cnt 2) 1) pretty-heads-group)))

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

(defun entropy/emacs-hydra-hollow-pretty-head-notation-handler
    (head-notation type &optional not-beautify-notation)
  (dolist (feature '(faces))
    (require feature))
  (let* ((nface 'entropy/emacs-defface-face-for-hydra-grey-face)
         (match-map
          `((global-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "g" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face ',nface)))
            (mode-map-inject
             :format "%s%s"
             :icon (lambda () (propertize "m" 'face 'error))
             :notation (lambda (notation) (propertize notation 'face ',nface)))
            (eemacs-top-keymap-inject
             :format "%s%s"
             :icon (lambda () (propertize "e" 'face 'success))
             :notation (lambda (notation) (propertize notation 'face ',nface)))))
         (matched (alist-get type match-map))
         (fmstr (plist-get matched :format))
         (icon (funcall (plist-get matched :icon)))
         (notation (or (when not-beautify-notation
                         head-notation)
                       (funcall (plist-get matched :notation) head-notation))))
    (format fmstr icon notation)))

;; **** wapper

(defun entropy/emacs-hydra-hollow-generalized-split-heads-common
    (pretty-split-heads)
  (let (rtn)
    (dolist (sp-head pretty-split-heads)
      (let* ((group (car sp-head))
             (new-group (entropy/emacs-hydra-hollow--common-judge-p
                         group))
             (pattern (caadr sp-head))
             (ptt-plist (cdddr pattern))
             (key (car pattern))
             (new-key (entropy/emacs-hydra-hollow--common-judge-p
                       key))
             (command (cadr pattern))
             (new-command (or (ignore-errors
                                (when (eq :eval (car command))
                                  (eval (cadr command))))
                              command))
             (notation (caddr pattern))
             (new-notation (entropy/emacs-hydra-hollow--common-judge-p
                            notation))
             )

        (setq rtn
              (append
               rtn
               `((,new-group ((,new-key ,new-command ,new-notation ,@ptt-plist))))))))
    rtn))

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
      (setq rtn
            (entropy/emacs-hydra-hollow-generalized-split-heads-common
             rtn))
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

;; **** category framework
;; ***** library

;; ****** core

;; ******* category prototype

(defvar entropy/emacs-hydra-hollow-category-default-width 4)

(cl-defun entropy/emacs-hydra-hollow-set-category-name
    (category-name
     &key
     category-name-prefix
     category-hydra-name
     category-hydra-caller
     caller-base-pretty-body
     category-groups
     category-depth
     category-width
     category-baron-name
     previous-category-name
     next-category-name)
  (cond
   ((null (entropy/emacs-common-plistp (ignore-errors (symbol-value category-name))))
    (set category-name
         (list
          :category-name-prefix category-name-prefix
          :category-name category-name
          :category-hydra-name category-hydra-name
          :category-hydra-caller category-hydra-caller
          :caller-base-pretty-body caller-base-pretty-body
          :category-groups category-groups
          :category-depth category-depth
          :category-width category-width
          :category-baron-name category-baron-name
          :previous-category-name previous-category-name
          :next-category-name
          next-category-name)))
   ((entropy/emacs-common-plistp (ignore-errors (symbol-value category-name)))
    (dolist
        (el
         '((:category-name-prefix category-name-prefix)
           (:category-name category-name)
           (:category-hydra-name category-hydra-name)
           (:category-hydra-caller category-hydra-caller)
           (:caller-base-pretty-body caller-base-pretty-body)
           (:category-groups category-groups)
           (:category-depth category-depth)
           (:category-width category-width)
           (:category-baron-name category-baron-name)
           (:previous-category-name previous-category-name)
           (:next-category-name next-category-name)))
      (unless (null (symbol-value (cadr el)))
        (plist-put (symbol-value category-name)
                   (car el) (symbol-value (cadr el))))))))

;; ******* category names get

(defvar entropy/emacs-hydra-hollow-category-name-core
  "--hydra-category-")

(defvar entropy/emacs-hydra-hollow-category-name-format
  (format "%%s%s%%s"
          entropy/emacs-hydra-hollow-category-name-core))

(defvar entropy/emacs-hydra-hollow-category-name-regexp
  (format "\\(.+\\)\\(%s\\)\\([0-9]+\\)"
          entropy/emacs-hydra-hollow-category-name-core))

(defvar entropy/emacs-hydra-hollow-category-hydra-name-format
  (format "%%s%s%%s-caller"
          entropy/emacs-hydra-hollow-category-name-core))

(defvar entropy/emacs-hydra-hollow-category-hydra-name-regexp
  (format "%s-caller"
          entropy/emacs-hydra-hollow-category-name-regexp))

(defun entropy/emacs-hydra-hollow-category-get-category-name
    (category-name-prefix &optional category-depth)
  (let ((category-depth (number-to-string (or category-depth 0))))
    (intern
     (format entropy/emacs-hydra-hollow-category-name-format
             category-name-prefix category-depth))))

(defun entropy/emacs-hydra-hollow-category-get-hydra-branch-name
    (category-name-prefix branch &optional category-depth)
  (let ((category-depth (number-to-string (or category-depth 0))))
    (cond ((null branch)
           (intern
            (format entropy/emacs-hydra-hollow-category-hydra-name-format
                    category-name-prefix category-depth)))
          ((eq branch 'hydra-map)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-category-hydra-name-format
                     category-name-prefix category-depth)
             "/keymap")))
          ((eq branch 'hydra-pretty-body)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-category-hydra-name-format
                     category-name-prefix category-depth)
             "/pretty-body")))
          ((eq branch 'hydra-group-heads)
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-category-hydra-name-format
                     category-name-prefix category-depth)
             "/heads-plist")))
          (t
           (intern
            (concat
             (format entropy/emacs-hydra-hollow-category-hydra-name-format
                     category-name-prefix category-depth)
             "/body"))))))

(defun entropy/emacs-hydra-hollow-category-hydra-name-p
    (maybe-category-hydra-name &optional get-type)
  (if (string-match entropy/emacs-hydra-hollow-category-hydra-name-regexp
                    (symbol-name maybe-category-hydra-name))
      (let* ((name-str (symbol-name maybe-category-hydra-name))
             (category-name-prefix (intern (match-string 1 name-str)))
             (category-depth (string-to-number (match-string 3 name-str))))
        (cond
         ((and (not (null get-type))
               (not (eq get-type 'just-prefix)))
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix get-type category-depth))
         ((eq get-type 'just-prefix)
          category-name-prefix)
         (t
          (entropy/emacs-hydra-hollow-category-get-category-name
           category-name-prefix category-depth))))
    nil))


;; ******* category width normalize

(defun entropy/emacs-hydra-hollow-normalize-category-width-indicator
    (category-width-indicator)
  (let (rtn)
    (cond ((and (listp category-width-indicator)
                (not (null category-width-indicator)))
           (dolist (el category-width-indicator)
             (let ((el-var (entropy/emacs-hydra-hollow--common-judge-p
                            el)))
               (cond ((and (integerp el-var)
                           (> el-var 0))
                      (push el-var rtn))
                     ((null el-var)
                      (push entropy/emacs-hydra-hollow-category-default-width
                            rtn))
                     (t
                      (push t rtn)))))
           (setq rtn (reverse rtn)))
          ((null category-width-indicator)
           (setq rtn nil))
          (t
           (setq rtn t)))
    rtn))

;; ******* category navigation set

(defun entropy/emacs-hydra-hollow-category-concat-title-for-nav
    (title &rest nav)
  (let* ((up-hint (car nav))
         (down-hint (cadr nav))
         (fmstr "[%s]: %s page"))
    (cond
     ((and (not (null up-hint))
           (null down-hint))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "UP" "previous")
                                  'face 'warning))))
     ((and (not (null down-hint))
           (null up-hint))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "DOWN" "next")
                                  'face 'warning))))
     ((and (not (null up-hint))
           (not (null down-hint)))
      (setq title
            `(concat ,title " "
                     ,(propertize (format fmstr "UP" "previous")
                                  'face 'warning)
                     " "
                     ,(propertize (format fmstr "DOWN" "next")
                                  'face 'warning)))))
    title))

(defun entropy/emacs-hydra-hollow-category-define-nav-key
    (keymap &rest category-nav-category-hydra-caller)
  (let ((up-caller (car category-nav-category-hydra-caller))
        (down-caller (cadr category-nav-category-hydra-caller)))
    (when (not (null up-caller))
      (define-key keymap (kbd "<up>") up-caller))
    (when (not (null down-caller))
      (define-key keymap (kbd "<down>") down-caller))))

;; ******* categor baron set

(defun entropy/emacs-hydra-hollow-category-define-rate-key
    (keymap category-baron)
  (when (and (keymapp keymap)
             (functionp category-baron))
    (define-key keymap (kbd "<up>") category-baron)))

(defun entropy/emacs-hydra-hollow-category-patch-hire-title
    (pretty-hydra-body &optional hint-string)
  (let* ((new-pretty-hydra-body (copy-tree pretty-hydra-body))
         (title (plist-get pretty-hydra-body :title))
         (stick-regexp "» Hint <up> to \\[Previous Hydra\\]")
         (stick-p (string-match-p stick-regexp (eval title)))
         new-title)
    (unless stick-p
      (setq new-title
            `(concat
              ,title
              " "
              "» "
              (propertize "Hint <up> to [Previous Hydra]"
                          'face 'error)))
      (setq new-pretty-hydra-body
            (plist-put new-pretty-hydra-body :title new-title)))
    new-pretty-hydra-body))

;; ******* category recursive match

(defun entropy/emacs-hydra-hollow-category-recursive-match-group
    (category-name-prefix group-match)
  (let ((category-depth 0)
        rtn)
    (catch :matched
      (while (funcall
              `(lambda ()
                 (bound-and-true-p
                  ,(entropy/emacs-hydra-hollow-category-get-category-name
                    category-name-prefix category-depth))))
        (let* ((category-name
                (entropy/emacs-hydra-hollow-category-get-category-name
                 category-name-prefix category-depth))
               (category (symbol-value category-name))
               (category-groups (plist-get category :category-groups)))
          (when (member group-match category-groups)
            (setq rtn (list :category category :category-depth category-depth))
            (throw :matched nil))
          (setq category-depth (+ 1 category-depth)))))
    (unless (not (null rtn))
      (setq rtn (list :category nil :category-depth category-depth)))
    rtn))

;; ******* category manipulation

(defmacro entropy/emacs-hydra-hollow-category-with-category
    (category-name-prefix category-depth &rest body)
  `(let* (                              ;
          ;; orig pattern
          ($internally/category-name-prefix ,category-name-prefix)
          ($internally/category-depth ,category-depth)

          ($internally/category-name
           (entropy/emacs-hydra-hollow-category-get-category-name
            $internally/category-name-prefix $internally/category-depth))

          ($internally/category-value
           (symbol-value $internally/category-name))

          ($internally/category-hydra-name
           (plist-get $internally/category-value :category-hydra-name))

          ($internally/category-caller-base-pretty-body-value
           (plist-get $internally/category-value :caller-base-pretty-body))

          ($internally/category-baron-name
           (plist-get $internally/category-value :category-baron-name))

          ($internally/next-category-name
           (plist-get $internally/category-value :next-category-name))
          ($internally/next-category-value
           (ignore-errors (symbol-value $internally/next-category-name)))

          ($internally/previous-category-name
           (plist-get $internally/category-value :previous-category-name))
          ($internally/previous-category-value
           (ignore-errors (symbol-value $internally/previous-category-name)))

          ($internally/category-pretty-hydra-body-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/category-name-prefix 'hydra-pretty-body $internally/category-depth))

          ($internally/category-pretty-hydra-body-value
           (symbol-value
            $internally/category-pretty-hydra-body-name))

          ($internally/category-pretty-heads-group-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/category-name-prefix
            'hydra-group-heads $internally/category-depth))

          ($internally/category-pretty-heads-group-value
           (symbol-value
            $internally/category-pretty-heads-group-name))

          ($internally/category-hydra-map-name
           (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
            $internally/category-name-prefix
            'hydra-map $internally/category-depth))

          ($internally/category-groups-value
           (plist-get $internally/category-value :category-groups))

          ($internally/category-width-value
           (plist-get $internally/category-value :category-width))

          ;; new pattern

          ($internally/new-category-baron-name
           $internally/category-baron-name)

          ($internally/new-next-category-name
           $internally/next-category-name)

          ($internally/new-previous-category-name
           $internally/previous-category-name)

          $internally/new-category-groups-value
          $internally/new-category-width-value

          ($internally/new-category-pretty-hydra-body-value
           (copy-tree $internally/category-pretty-hydra-body-value))
          ($internally/new-category-pretty-heads-group-value
           (copy-tree $internally/category-pretty-heads-group-value))

          )

     ;; run body
     (cl-labels
         (($internally/pretty-hydra-define
           (name body heads-plist)
           (cond
            ((eq name $internally/category-hydra-name)
             (setq $internally/new-category-pretty-hydra-body-value
                   body)
             (setq $internally/new-category-pretty-heads-group-value
                   heads-plist))
            (t
             (entropy/emacs-hydra-hollow-func-version-pthydra-define
              name body heads-plist))))
          ($internally/pretty-hydra-define+
           (name body heads-plist)
           (cond
            ((eq name $internally/category-hydra-name)
             (setq $internally/new-category-pretty-hydra-body-value
                   body)
             (setq $internally/new-category-pretty-heads-group-value
                   (pretty-hydra--merge-heads
                    $internally/new-category-pretty-heads-group-value
                    heads-plist)))
            (t
             (entropy/emacs-hydra-hollow-func-version-pthydra-define+
              name body heads-plist)))))
       (progn
         ,@body))

     ;; redefine category pretty-hydra-body for patching with baron
     (when (not (null $internally/new-category-baron-name))
       (setq $internally/new-category-pretty-hydra-body-value
             (entropy/emacs-hydra-hollow-category-patch-hire-title
              $internally/new-category-pretty-hydra-body-value)))

     ;; redefine category pretty-hydra
     (funcall
      (list 'lambda nil
            (list 'pretty-hydra-define $internally/category-hydra-name
                  $internally/new-category-pretty-hydra-body-value
                  $internally/new-category-pretty-heads-group-value)))

     ;; reset cagegory
     (setq $internally/new-category-groups-value
           (cl-loop for item in (symbol-value $internally/category-pretty-heads-group-name)
                    when (stringp item)
                    collect item))

     (entropy/emacs-hydra-hollow-set-category-name
      $internally/category-name
      :category-groups $internally/new-category-groups-value
      :category-width
      (or $internally/new-category-width-value
          (if (> (length $internally/new-category-groups-value)
                 $internally/category-width-value)
              (length $internally/new-category-groups-value)
            $internally/category-width-value))
      :category-baron-name $internally/new-category-baron-name
      :next-category-name $internally/new-next-category-name
      :previous-category-name $internally/new-previous-category-name)

     ;; remap category navigation key-binds
     (entropy/emacs-hydra-hollow-category-define-nav-key
      (symbol-value $internally/category-hydra-map-name)
      (plist-get
       (ignore-errors (symbol-value $internally/new-previous-category-name))
       :category-hydra-caller)
      (plist-get
       (ignore-errors (symbol-value $internally/new-next-category-name))
       :category-hydra-caller))

     ;; remap category baron key-binds
     (when $internally/new-category-baron-name
       (setq entropy/emacs-hydra-hollow-union-form
             (append
              entropy/emacs-hydra-hollow-union-form
              `((entropy/emacs-hydra-hollow-category-define-rate-key
                 (symbol-value ',$internally/category-hydra-map-name)
                 ',$internally/new-category-baron-name)))))
     ))


(defun entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
    (category-baron-name pretty-hydra-heads-group)
  (let ((split-heads
         (entropy/emacs-hydra-hollow-split-pretty-hydra-group-heads
          pretty-hydra-heads-group)))
    (dolist (head split-heads)
      (let ((command (cadr (caadr head))))
        (when (entropy/emacs-hydra-hollow-category-hydra-name-p
               command)
          (let* ((it-ctg-name-prefix
                  (entropy/emacs-hydra-hollow-category-hydra-name-p
                   command 'just-prefix)))
            (setq entropy/emacs-hydra-hollow-union-form
                  (append
                   entropy/emacs-hydra-hollow-union-form
                   `((entropy/emacs-hydra-hollow-category-with-category
                      ',it-ctg-name-prefix nil
                      (setq $internally/new-category-baron-name
                            ',category-baron-name)))))))))))


;; ****** define hydra hollow category

(defun entropy/emacs-hydra-hollow-category-frame-work-define
    (category-name-prefix pretty-body pretty-heads-group
                          &optional category-depth previous-category-name category-width-indicator)
  (let* ((ctgs (entropy/emacs-hydra-hollow-partion-pretty-heads-group
                pretty-heads-group))
         (ctg-len (length ctgs))
         (ctg-indc (entropy/emacs-hydra-hollow-normalize-category-width-indicator
                    category-width-indicator))
         (body-patch (copy-tree pretty-body))
         (category-depth (or category-depth 0))
         cur-ctg-indc
         rest-ctg-inc
         cur-head-group
         rest-head-group
         category-hydra-name
         category-hydra-caller
         category-name
         next-category-name
         cur-hydra-keymap-name
         )

    ;; get category map
    (cond ((eq t ctg-indc)
           (setq cur-ctg-indc t
                 rest-ctg-inc t))
          ((null ctg-indc)
           (setq cur-ctg-indc
                 entropy/emacs-hydra-hollow-category-default-width
                 rest-ctg-inc nil))
          ((integerp ctg-indc)
           (setq cur-ctg-indc ctg-indc
                 rest-ctg-inc ctg-indc))
          (t
           (setq cur-ctg-indc (car ctg-indc)
                 rest-ctg-inc (cdr ctg-indc))))

    ;; get manipulated heads
    (cond
     ((eq cur-ctg-indc t)
      (setq rest-head-group nil)
      (cl-loop for item in ctgs
               do (setq cur-head-group
                        (append cur-head-group item))))
     ((or (null cur-ctg-indc)
          (and (integerp cur-ctg-indc)
               (<= cur-ctg-indc 0))
          (floatp cur-ctg-indc))
      (let ((cnt 0))
        (while (<= (+ cnt 1)
                   entropy/emacs-hydra-hollow-category-default-width)
          (setq cur-head-group
                (append cur-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt))
        (while (not (null (nth cnt ctgs)))
          (setq rest-head-group
                (append rest-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt))))
     ((and (integerp cur-ctg-indc)
           (> cur-ctg-indc 0))
      (let ((cnt 0))
        (while (<= (+ cnt 1)
                   cur-ctg-indc)
          (setq cur-head-group
                (append cur-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt))
        (while (not (null (nth cnt ctgs)))
          (setq rest-head-group
                (append rest-head-group
                        (nth cnt ctgs)))
          (cl-incf cnt)))))

    ;; patch pretty-body
    (let* ((title (plist-get body-patch :title))
           (new-title (copy-tree title)))

      (setq new-title
            (entropy/emacs-hydra-hollow-category-concat-title-for-nav
             new-title previous-category-name rest-head-group))

      (setq body-patch
            (plist-put
             body-patch
             :title
             new-title)))

    ;; generate category name
    (setq category-name
          (entropy/emacs-hydra-hollow-category-get-category-name
           category-name-prefix category-depth))

    ;; generate caller name
    (setq category-hydra-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix nil category-depth))

    (setq category-hydra-caller
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix t category-depth))

    ;; generate next category-name
    (unless (null rest-head-group)
      (setq next-category-name
            (entropy/emacs-hydra-hollow-category-get-category-name
             category-name-prefix (+ category-depth 1))))

    ;; generate hydra-keymap name
    (setq cur-hydra-keymap-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix 'hydra-map category-depth))

    ;; define pretty hydra
    (funcall
     `(lambda ()
        (pretty-hydra-define
          ,category-hydra-name
          ,body-patch
          ,cur-head-group)))

    ;; bind rate key and patch rate title

    (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
     category-hydra-caller cur-head-group)

    ;; advice for calling union form
    (when (eq category-depth 0)
      (entropy/emacs-hydra-hollow-advice-for-call-union-form
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        category-name-prefix t)))

    ;; define current category nav keys
    (entropy/emacs-hydra-hollow-category-define-nav-key
     (symbol-value cur-hydra-keymap-name)
     (when previous-category-name
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        category-name-prefix t (- category-depth 1)))
     (when (not (null rest-head-group))
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        category-name-prefix t (+ category-depth 1))))

    ;; recursive define category hydra
    (when (not (null rest-head-group))
      (entropy/emacs-hydra-hollow-category-frame-work-define
       category-name-prefix pretty-body rest-head-group
       (+ category-depth 1) category-name rest-ctg-inc))

    ;; return category plist
    (entropy/emacs-hydra-hollow-set-category-name
     category-name
     :category-name-prefix category-name-prefix
     :category-hydra-name category-hydra-name
     :category-hydra-caller category-hydra-caller
     :caller-base-pretty-body pretty-body
     :category-groups (cl-loop for item in cur-head-group
                               when (not (listp item))
                               collect item)
     :category-depth category-depth
     :category-width cur-ctg-indc
     :previous-category-name previous-category-name
     :next-category-name next-category-name)

    ))

(defun entropy/emacs-hydra-hollow-category-frame-work-define+
    (category-name-prefix pretty-body pretty-heads-group
                          &optional
                          category-width-indicator-for-build
                          category-width-indicator-for-inject)
  (let* ((top-category-name
          (entropy/emacs-hydra-hollow-category-get-category-name
           category-name-prefix))
         (top-category-exists-p
          (ignore-errors (not (null (symbol-value top-category-name)))))
         (ctgs
          (entropy/emacs-hydra-hollow-partion-pretty-heads-group
           pretty-heads-group)))
    (unless top-category-exists-p
      (entropy/emacs-hydra-hollow-category-frame-work-define
       category-name-prefix pretty-body pretty-heads-group
       nil nil category-width-indicator-for-build))
    (when top-category-exists-p
      (dolist (part-ctg ctgs)
        (let* ((group (car part-ctg))
               (ctg-match
                (entropy/emacs-hydra-hollow-category-recursive-match-group
                 category-name-prefix group))
               (ctg-matched-p (not
                               (null
                                (plist-get ctg-match :category)))))
          (cond
           (ctg-matched-p
            (let* ((matched-ctg
                    (plist-get ctg-match :category))
                   (matched-category-depth
                    (plist-get matched-ctg :category-depth))
                   (matched-ctg-caller
                    (plist-get matched-ctg :category-hydra-caller)))
              (entropy/emacs-hydra-hollow-category-with-category
               category-name-prefix matched-category-depth
               ($internally/pretty-hydra-define+
                $internally/category-hydra-name
                $internally/category-pretty-hydra-body-value
                part-ctg))
              (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
               matched-ctg-caller
               part-ctg)))
           (t
            (let* ((category-depth (plist-get ctg-match :category-depth))
                   (tail-category-depth (- category-depth 1))
                   (tail-category-name
                    (entropy/emacs-hydra-hollow-category-get-category-name
                     category-name-prefix tail-category-depth))

                   (tail-category (symbol-value tail-category-name))
                   (tail-category-hydra-caller (plist-get tail-category :category-hydra-caller))
                   (tail-category-ctg-width (plist-get tail-category :category-width))
                   (tail-category-groups (copy-tree (plist-get tail-category :category-groups)))

                   (tail-category-overflow-p
                    (>= (length tail-category-groups)
                        tail-category-ctg-width)))
              (cond
               ((null tail-category-overflow-p)
                (entropy/emacs-hydra-hollow-category-with-category
                 category-name-prefix tail-category-depth
                 ($internally/pretty-hydra-define+
                  $internally/category-hydra-name
                  $internally/category-pretty-hydra-body-value
                  part-ctg))
                (entropy/emacs-hydra-hollow-category-try-bind-rate-to-baron
                 tail-category-hydra-caller
                 part-ctg))

               (tail-category-overflow-p
                (let* ((new-ctg-name
                        (entropy/emacs-hydra-hollow-category-get-category-name
                         category-name-prefix category-depth)))

                  (entropy/emacs-hydra-hollow-category-with-category
                   category-name-prefix tail-category-depth
                   (let ((tail-category-used-pretty-body-title
                          (plist-get $internally/new-category-pretty-hydra-body-value
                                     :title)))
                     ;; add new tail category
                     (entropy/emacs-hydra-hollow-category-frame-work-define
                      category-name-prefix
                      $internally/category-caller-base-pretty-body-value
                      part-ctg
                      (+ 1 $internally/category-depth)
                      $internally/category-name
                      category-width-indicator-for-inject)

                     ;; pop out current category-width-indicator-for-inject
                     (when (and (not (null category-width-indicator-for-inject))
                                (listp category-width-indicator-for-inject))
                       (pop category-width-indicator-for-inject))

                     (setq $internally/new-category-pretty-hydra-body-value
                           (plist-put
                            $internally/new-category-pretty-hydra-body-value
                            :title
                            (entropy/emacs-hydra-hollow-category-concat-title-for-nav
                             tail-category-used-pretty-body-title
                             nil t))
                           $internally/new-next-category-name
                           new-ctg-name))))))))))))))


;; ***** major-mode pretty hydra core

(defun entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
    (mode)
  (let* ((mode-str (symbol-name mode))
         (mode-ctg-name-prefix
          (intern
           (format "eemacs-hydra-for-mode-%s"
                   mode-str))))
    mode-ctg-name-prefix))

(defun entropy/emacs-hydra-hollow-category-get-major-mode-caller (mode)
  (let (rtn)
    (setq rtn
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
            mode)
           t))
    rtn))

(defun entropy/emacs-hydra-hollow-category-major-mode-define
    (mode pretty-body pretty-heads-group &optional category-width-indicator)
  (let ((ctg-name-prefix
         (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
          mode)))
    (entropy/emacs-hydra-hollow-category-frame-work-define
     ctg-name-prefix pretty-body pretty-heads-group
     nil nil category-width-indicator)))

(defun entropy/emacs-hydra-hollow-category-major-mode-define+
    (mode pretty-body pretty-heads-group
          &optional
          category-width-indicator-for-build
          category-width-indicator-for-inject)
  (let ((ctg-name-prefix
         (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
          mode)))
    (entropy/emacs-hydra-hollow-category-frame-work-define+
     ctg-name-prefix pretty-body pretty-heads-group
     category-width-indicator-for-build
     category-width-indicator-for-inject)))


(defun entropy/emacs-hydra-hollow-category-major-mode-hydra ()
  "Summon the hydra for given MODE (if there is one)."
  (interactive)
  (let ((orig-mode major-mode)
        (rec-mode major-mode))
    (catch 'done
      (while rec-mode
        (let ((hydra (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                      (entropy/emacs-hydra-hollow-category-get-major-mode-name-prefix
                       rec-mode)
                      t)))
          (when (fboundp hydra)
            (call-interactively hydra)
            (throw 'done t)))
        (setq rec-mode (get rec-mode 'derived-mode-parent)))
      (user-error "Major mode hydra not found for %s or its parent modes" orig-mode))))

(entropy/emacs-!set-key
  (kbd "m")
  #'entropy/emacs-hydra-hollow-category-major-mode-hydra)

;; ***** individual pretty hydra core


(defun entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
    (individual-hydra-name)
  (let* ((fmstr "entropy/emacs-individual-hydra--%s")
         (category-name-prefix
          (format fmstr
                  (symbol-name individual-hydra-name))))
    category-name-prefix))

(defun entropy/emacs-hydra-hollow-category-common-individual-get-caller
    (individual-hydra-name)
  (let ((name-prefix
         (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
          individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
     name-prefix t)))


(defun entropy/emacs-hydra-hollow-category-common-individual-define
    (individual-hydra-name pretty-body pretty-heads-group
                           &optional category-width-indicator)
  (let* ((name-prefix
          (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
           individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-frame-work-define
     name-prefix pretty-body pretty-heads-group
     nil nil
     category-width-indicator)))

(defun entropy/emacs-hydra-hollow-category-common-individual-define+
    (individual-hydra-name pretty-body pretty-heads-group
                           &optional
                           category-width-indicator-for-build
                           category-width-indicator-for-inject)
  (let* ((name-prefix
          (entropy/emacs-hydra-hollow-category-common-individual-get-name-prefix
           individual-hydra-name)))
    (entropy/emacs-hydra-hollow-category-frame-work-define+
     name-prefix pretty-body pretty-heads-group
     category-width-indicator-for-build
     category-width-indicator-for-inject)))

(defun entropy/emacs-hydra-hollow-category-common-individual-make-title-common
    (individual-hydra-name)
  (let ((title-str
         (format "%s Actions" (capitalize (symbol-name individual-hydra-name)))))
    `(:title
      (entropy/emacs-pretty-hydra-make-title
       ,title-str
       "faicon" "certificate")
      :color ambranth
      :quit-key "q"
      :separator "═")))


;; **** heads predicate
;; ***** predicate defination

;; - =Predicate Functionn:=

;;   A function parse the riched-split-pretty-head and return the
;;   predicated one.

;;   A =riched-split-pretty-head= is a plist of three key, =:restrict=
;;   a plist of hosted restriction status and the =:split-pretty-head=
;;   was the slot host the =split-pretty-head=, and the =:rest-args= a
;;   list of remaining args needed by current predicate function, thus
;;   for all, it forms as:

;;   (:restrict  (global-bind-notation-patched ...)
;;    :split-pretty-head ("Basic" (("1" (message "yes") "test message")))
;;    :rest-args (mode feature map))

(defvar entropy/emacs-hydra-hollow-predicative-keys
  '((:global-bind . entropy/emacs-hydra-hollow-global-bind-predicate)
    (:map-inject . entropy/emacs-hydra-hollow-map-inject-predicate)
    (:eemacs-top-bind . entropy/emacs-hydra-hollow-top-keymap-inject-predicate)))

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
      (setq restrict (append restrict '(:global-bind-notation-beautified t)))
      (global-set-key (kbd key) command))
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
         (feature (car rest-args))
         (map (cadr rest-args))
         )
    (when (and feature map)
      (cond
       ((eq map-inject t)
        (setq notation
              (entropy/emacs-hydra-hollow-pretty-head-notation-handler
               notation 'mode-map-inject
               (and (member :global-bind-notation-beautified restrict)
                    (entropy/emacs-hydra-hollow--common-judge-p
                     (plist-get restrict :global-bind-notation-beautified)))))
        (setq restrict
              (append restrict '(:map-inject-notation-beautified t)))
        (funcall
         `(lambda nil
            (entropy/emacs-lazy-load-simple ,feature
              (define-key ,map (kbd ,key) #',command)))))
       ((entropy/emacs-common-plistp map-inject)
        (let ((do-beautify-notation
               (entropy/emacs-hydra-hollow--common-judge-p
                (plist-get map-inject :do-beautify-notation)))
              (do-inject-spec-maps
               (entropy/emacs-hydra-hollow--common-judge-p
                (plist-get map-inject :do-inject-spec-maps)))
              (do-inherit-predicate
               (entropy/emacs-hydra-hollow--common-judge-p
                (plist-get map-inject :do-inherit-predicate))))
          (when do-beautify-notation
            (setq notation
                  (entropy/emacs-hydra-hollow-pretty-head-notation-handler
                   notation 'mode-map-inject
                   (member :global-bind-notation-beautified restrict)))
            (setq restrict
                  (append restrict '(:map-inject-notation-beautified t))))
          (when do-inherit-predicate
            (funcall
             `(lambda nil
                (entropy/emacs-lazy-load-simple ,feature
                  (define-key ,map (kbd ,key) #',command)))))
          (when do-inject-spec-maps
            (when (and (listp do-inject-spec-maps)
                       (listp (car do-inject-spec-maps)))
              (dolist (item do-inject-spec-maps)
                (let ((feature (car item))
                      (map (cadr item))
                      (key-for (or (nth 2 item) key)))
                  (funcall
                   `(lambda nil
                      (entropy/emacs-lazy-load-simple ,feature
                        (define-key ,map (kbd ,key-for) #',command))))
                  ))))))))

    (list :restrict restrict
          :split-pretty-head
          `(,group ((,key ,command ,notation ,@split-pretty-head-plist)))
          )))

(defun entropy/emacs-hydra-hollow-top-keymap-inject-predicate
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
         (top-bind-p (entropy/emacs-hydra-hollow--common-judge-p
                      (plist-get split-pretty-head-plist
                                 :eemacs-top-bind)))
         ;; :rest-args
         (rest-args (plist-get riched-split-pretty-head :rest-args))
         )
    (when top-bind-p
      (setq notation
            (entropy/emacs-hydra-hollow-pretty-head-notation-handler
             notation 'eemacs-top-keymap-inject
             (or (entropy/emacs-hydra-hollow--common-judge-p
                  (plist-get restrict :global-bind-notation-beautified))
                 (entropy/emacs-hydra-hollow--common-judge-p
                  (plist-get restrict :map-inject-notation-beautified))))
            restrict
            (append restrict
                    '(:eemacs-topkey-notation-beautified t)))
      (funcall
       `(lambda ()
          (entropy/emacs-!set-key
            (kbd ,key)
            #',command))))
    (list :restrict restrict
          :split-pretty-head
          `(,group ((,key ,command ,notation ,@split-pretty-head-plist))))))

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


;; ** apis
;; *** top dispatcher

(defvar entropy/emacs-hydra-hollow-init-top-dispatch-ctg-name-prefix
  'entropy/emacs-hydra-hollow-top-dispatch)

(defun entropy/emacs-hydra-hollow-init-top-dispatch (&optional force)
  (when (or (not entropy/emacs-hydra-hollow-top-dispatch-init-done)
            force)
    (let* ((ctg-name-prefix entropy/emacs-hydra-hollow-init-top-dispatch-ctg-name-prefix)
           (ctg-top-category-hydra-caller
            (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
             ctg-name-prefix t)))
      (entropy/emacs-hydra-hollow-category-frame-work-define
       ctg-name-prefix
       '(:title
         (entropy/emacs-pretty-hydra-make-title
          "eemacs top dispatch" "faicon" "toggle-on")
         :color ambranth
         :quit-key "q"
         :separator "═")
       '("Basic"     ()
         "WI&BUF"    ()
         ;; ---
         "Highlight" ()
         "Utils"     ()
         ;; ---
         "Shellpop"  ()
         "Structure" ()
         "WWW"       ()
         "Rss"       ()
         ;; ---
         "Vcs"       ()
         "Tramp"     ()
         ;; ---
         "Project"   ()
         "Org"       ()
         ;; ---
         "Pyim"      ()
         "Misc."     ()
         )
       nil nil
       '(2 2 4 2 2))

      (unless entropy/emacs-hydra-hollow-top-dispatch-init-done
        (setq entropy/emacs-hydra-hollow-top-dispatch-init-done t)
        (entropy/emacs-!set-key
          (kbd "h")
          ctg-top-category-hydra-caller)
        (entropy/emacs-hydra-hollow-advice-for-call-union-form
         ctg-top-category-hydra-caller)))))

(defun entropy/emacs-hydra-hollow-add-for-top-dispatch
    (pretty-heads-group)
  (entropy/emacs-hydra-hollow-init-top-dispatch)
  (let* ((split-heads
          (entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
           pretty-heads-group
           '((:global-bind)
             (:eemacs-top-bind))
           t)))
    (unless (null split-heads)
      (dolist (sp-h split-heads)
        (setq entropy/emacs-hydra-hollow-top-dispatch-register
              (append entropy/emacs-hydra-hollow-top-dispatch-register
                      `(,sp-h))))

      (entropy/emacs-hydra-hollow-category-frame-work-define+
       entropy/emacs-hydra-hollow-init-top-dispatch-ctg-name-prefix
       nil
       (entropy/emacs-hydra-hollow-merge-pretty-hydra-sparse-heads
        split-heads)))))

;; *** majro mode dispacher

(entropy/emacs-hydra-hollow-advice-for-call-union-form
 #'entropy/emacs-hydra-hollow-category-major-mode-hydra)

;; **** define major mode hydra

(cl-defmacro entropy/emacs-hydra-hollow--define-major-mode-hydra-macro
    (mode feature mode-map body heads-plist &optional ctg-width-indc)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
          heads-plist
          `((:map-inject . (,feature ,mode-map))
            (:global-bind)
            (:eemacs-top-bind)))))
    `(let ()
       ;; Define major-mode-hydra before lazy loading feature prevent
       ;; hydra adding cover its body
       (,(if (fboundp (entropy/emacs-hydra-hollow-category-get-major-mode-caller
                       mode))
             'entropy/emacs-hydra-hollow-category-major-mode-define+
           'entropy/emacs-hydra-hollow-category-major-mode-define)
        ',mode
        ',body
        ',patched-heads-group
        ',ctg-width-indc)

       (unless (alist-get  ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register)))))

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra
    (mode feature mode-map body heads-plist &optional ctg-width-indc)
  (let ()
    (funcall
     `(lambda ()
        (entropy/emacs-hydra-hollow--define-major-mode-hydra-macro
         ,mode ,feature ,mode-map ,body ,heads-plist ,ctg-width-indc)))))

;; **** add major mode hydra
(cl-defmacro entropy/emacs-hydra-hollow--add-to-major-mode-hydra-macro
    (mode feature mode-map heads-plist
          &optional
          hydra-body
          category-width-indicator-for-build
          category-width-indicator-for-inject)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
          heads-plist
          `((:map-inject . (,feature ,mode-map))
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or hydra-body
                  (alist-get
                   mode
                   entropy/emacs-hydra-hollow-major-mode-body-register
                   )
                  (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
                   mode))))
    `(let ()
       ;; add hydra for feature with lazy load prevent covering the
       ;; major defination
       (entropy/emacs-hydra-hollow-category-major-mode-define+
        ',mode
        ',body
        ',patched-heads-group
        category-width-indicator-for-build
        category-width-indicator-for-inject)
       )))

(defun entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    (mode feature mode-map heads-plist
          &optional
          hydra-body
          category-width-indicator-for-build
          category-width-indicator-for-inject)
  (let ()
    (funcall
     `(lambda ()
        (entropy/emacs-hydra-hollow--add-to-major-mode-hydra-macro
         ,mode ,feature ,mode-map ,heads-plist ,hydra-body
         ,category-width-indicator-for-build
         ,category-width-indicator-for-inject)))))

;; **** sparse tree builder

(cl-defmacro entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-macro
    (mode &optional category-width-indicator)
  (let ((body
         (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
          mode)))
    `(progn
       (unless (alist-get ',mode entropy/emacs-hydra-hollow-major-mode-body-register)
         (push (cons ',mode
                     ',body)
               entropy/emacs-hydra-hollow-major-mode-body-register))
       (entropy/emacs-hydra-hollow-category-major-mode-define
        ',mode
        ',body
        '("Help"
          nil)
        ',category-width-indicator))))

(defun entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    (mode feature mode-map do-not-build-sparse-tree
          &optional
          heads
          category-width-indicator-for-build
          category-width-indicator-for-inject)
  (let ((has-defined (fboundp (entropy/emacs-hydra-hollow-category-get-major-mode-caller
                               mode)))
        (hydra-body
         (or
          (alist-get
           mode
           entropy/emacs-hydra-hollow-major-mode-body-register)
          (entropy/emacs-pretty-hydra-make-body-for-major-mode-union
           mode))))
    (unless (or has-defined do-not-build-sparse-tree)
      (funcall
       `(lambda ()
          (entropy/emacs-hydra-hollow--define-major-mode-hydra-common-sparse-tree-macro
           ,mode ,category-width-indicator-for-build))))
    (funcall
     (if (and (null do-not-build-sparse-tree)
              (null has-defined))
         `(lambda ()
            (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
             ',mode ',feature ',mode-map ',heads
             nil nil
             ',category-width-indicator-for-inject))
       `(lambda ()
          (entropy/emacs-hydra-hollow-define-major-mode-hydra
           ',mode ',feature ',mode-map ',hydra-body ',heads
           ',category-width-indicator-for-build))))))

;; **** individual common hydra define&define+


(defun entropy/emacs-hydra-hollow-common-individual-hydra-define
    (individual-hydra-name feature keymap heads-plist
                           &optional hydra-body category-width-indicator)
  (let ((has-defined (fboundp (entropy/emacs-hydra-hollow-category-common-individual-get-caller
                               individual-hydra-name)))
        (patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
          heads-plist
          `((:map-inject . (,feature ,keymap))
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or hydra-body
                  (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
                   individual-hydra-name))))
    (if (null has-defined)
        (entropy/emacs-hydra-hollow-category-common-individual-define
         individual-hydra-name body patched-heads-group
         category-width-indicator)
      (entropy/emacs-hydra-hollow-category-common-individual-define+
       individual-hydra-name body patched-heads-group
       category-width-indicator))))

(defun entropy/emacs-hydra-hollow-common-individual-hydra-define+
    (individual-hydra-name feature keymap heads-plist
                           &optional
                           hydra-body
                           category-width-indicator-for-build
                           category-width-indicator-for-inject)
  (let ((patched-heads-group
         (entropy/emacs-hydra-hollow-rebuild-pretty-heads-group
          heads-plist
          `((:map-inject . (,feature ,keymap))
            (:global-bind)
            (:eemacs-top-bind))))
        (body (or hydra-body
                  (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
                   individual-hydra-name))))
    (entropy/emacs-hydra-hollow-category-common-individual-define+
     individual-hydra-name body patched-heads-group
     category-width-indicator-for-build
     category-width-indicator-for-inject)))


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
                 if (eq item :init)
                 collect :init and collect keyword
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
             (feature (or (cadr requests)
                          use-name))
             (map (or (caddr requests)
                      (intern (format "%s-map" (symbol-name use-name)))))
             (do-not-build-sparse-tree
              (cadddr requests))
             (ctg-width-indc-for-build
              (nth 4 requests))
             (ctg-width-indc-for-inject
              (nth 5 requests))
             (heads (cadr island)))
        (setq
         init-form
         (append init-form
                 `((when (not (null ',enable))
                     (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
                      ',mode ',feature ',map ',do-not-build-sparse-tree ',heads
                      ',ctg-width-indc-for-build
                      ',ctg-width-indc-for-inject)))))))
    (use-package-concat
     init-form
     rest-body)))

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
                  (pretty-body (nth 3 requests))
                  (category-width-indicator-for-build
                   (nth 4 requests))
                  (category-width-indicator-for-inject
                   (nth 5 requests))
                  (heads (cadr island))
                  run-call)
             (when enable
               (setq run-call
                     (list 'lambda '()
                           (list 'apply
                                 (list 'function 'entropy/emacs-hydra-hollow-add-to-major-mode-hydra)
                                 (list 'quote
                                       (list mode feature map heads
                                             pretty-body
                                             category-width-indicator-for-build
                                             category-width-indicator-for-inject)))))
               (push run-call _callers))))
         (when (not (null _callers))
           (dolist (caller (reverse _callers))
             (funcall caller))))))
    (use-package-concat
     init-form
     rest-body)))

(defalias 'use-package-normalize/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-normalize)

(defalias 'use-package-handler/:eemacs-mmphca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-mmphca-add-keyword
 :eemacs-mmphca)



;; **** :eemacs-indhc
(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :bind)
                 collect :bind and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defalias 'use-package-normalize/:eemacs-indhc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-normalize)

(defalias 'use-package-handler/:eemacs-indhc
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-indhc)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-normalize
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

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhc-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (heads (cadr island))
             (requests (cadr baron))
             (individual-hydra-name (car requests))
             (feature (cadr requests))
             (keymap (caddr requests))
             (hydra-body (cadddr requests))
             (ctg-width-indc (nth 4 requests)))
        (when enable
          (setq init-form
                (append init-form
                        `((entropy/emacs-hydra-hollow-common-individual-hydra-define
                           ',individual-hydra-name
                           ',feature
                           ',keymap
                           ',heads
                           ',hydra-body
                           ',ctg-width-indc)))))))
    (use-package-concat
     rest-body
     init-form)))

;; **** :eemacs-indhca


(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-add-keyword (keyword)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        (cl-loop for item in use-package-keywords
                 if (eq item :eemacs-indhca)
                 collect :eemacs-indhca and collect keyword
                 else
                 ;; don't add duplicates
                 unless (eq item keyword)
                 collect item)))

(defalias 'use-package-normalize/:eemacs-indhca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-normalize)

(defalias 'use-package-handler/:eemacs-indhca
  #'entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-handler)

(entropy/emacs-hydra-hollow--usepackage-eemacs-tpha-add-keyword
 :eemacs-indhca)

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-normalize
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

(defun entropy/emacs-hydra-hollow--usepackage-eemacs-indhca-def-handler
    (use-name key $arg rest state)
  (let* ((rest-body (use-package-process-keywords use-name rest state))
         (init-form '()))
    (dolist (island $arg)
      (let* ((baron (car island))
             (attr (car baron))
             (enable (let ((enable-slot (plist-get attr :enable)))
                       (entropy/emacs-hydra-hollow--common-judge-p
                        enable-slot)))
             (heads (cadr island))
             (requests (cadr baron))
             (individual-hydra-name (car requests))
             (feature (cadr requests))
             (keymap (caddr requests))
             (hydra-body (cadddr requests))
             (ctg-width-indc-for-build (nth 4 requests))
             (ctg-width-indc-for-inject (nth 5 requests)))
        (when enable
          (setq init-form
                (append init-form
                        `((entropy/emacs-hydra-hollow-common-individual-hydra-define+
                           ',individual-hydra-name
                           ',feature
                           ',keymap
                           ',heads
                           ',hydra-body
                           ',ctg-width-indc-for-build
                           ',ctg-width-indc-for-inject)))))))
    (use-package-concat
     rest-body
     init-form)))

;; * provide
(provide 'entropy-emacs-hydra-hollow)
