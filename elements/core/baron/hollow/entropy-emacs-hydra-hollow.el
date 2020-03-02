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

(defvar entropy/emacs-hydra-hollow-category-default-width 4)

(defun entropy/emacs-hydra-hollow-partion-pretty-heads-group
    (pretty-heads-group)
  (let (rtn (cnt 0))
    (while (< cnt (/ (length pretty-heads-group) 2))
      (push (list (nth (* cnt 2) pretty-heads-group)
                  (nth (+ 1 (* cnt 2)) pretty-heads-group))
            rtn)
      (cl-incf cnt))
    (reverse rtn)))

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

(defun entropy/emacs-hydra-hollow-category-get-category-name
    (category-name-prefix &optional depth)
  (let ((depth (number-to-string (or depth 0))))
    (intern
     (format "%s--hydra-category-%s"
             category-name-prefix depth))))

(defun entropy/emacs-hydra-hollow-category-get-hydra-branch-name
    (category-name-prefix branch &optional depth)
  (let ((depth (number-to-string (or depth 0))))
    (cond ((null branch)
           (intern
            (format "%s--hydra-category-caller-%s"
                    category-name-prefix depth)))
          ((eq branch 'hydra-map)
           (intern
            (format "%s--hydra-category-caller-%s/keymap"
                    category-name-prefix depth)))
          ((eq branch 'hydra-pretty-body)
           (intern
            (format "%s--hydra-category-caller-%s/pretty-body"
                    category-name-prefix depth)))
          ((eq branch 'hydra-group-heads)
           (intern
            (format "%s--hydra-category-caller-%s/heads-plist"
                    category-name-prefix depth)))
          (t
           (intern
            (format "%s--hydra-category-caller-%s/body"
                    category-name-prefix depth))))))

(defun entropy/emacs-hydra-hollow-category-define-nav-key
    (keymap &rest category-nav-caller-body)
  (let ((up-caller (car category-nav-caller-body))
        (down-caller (cadr category-nav-caller-body)))
    (when (not (null up-caller))
      (define-key keymap (kbd "<up>") up-caller))
    (when (not (null down-caller))
      (define-key keymap (kbd "<down>") down-caller))))

(defun entropy/emacs-hydra-hollow-category-recursive-match-group
    (category-name-prefix group-match)
  (let ((depth 0)
        rtn)
    (catch :matched
      (while (funcall
              `(lambda ()
                 (bound-and-true-p
                  ,(entropy/emacs-hydra-hollow-category-get-category-name
                    category-name-prefix depth))))
        (let* ((category-name
                (entropy/emacs-hydra-hollow-category-get-category-name
                 category-name-prefix depth))
               (category (symbol-value category-name))
               (groups (plist-get category :groups)))
          (when (member group-match groups)
            (setq rtn (list :category category :depth depth))
            (throw :matched nil))
          (setq depth (+ 1 depth)))))
    (unless (not (null rtn))
      (setq rtn (list :category nil :depth depth)))
    rtn))

(defun entropy/emacs-hydra-hollow-category-frame-work-define
    (category-name-prefix pretty-body pretty-heads-group
                          &optional depth previous-category-name category-width-indicator)
  (let* ((ctgs (entropy/emacs-hydra-hollow-partion-pretty-heads-group
                pretty-heads-group))
         (ctg-len (length ctgs))
         (ctg-indc (entropy/emacs-hydra-hollow-normalize-category-width-indicator
                    category-width-indicator))
         (body-patch (copy-tree pretty-body))
         (depth (or depth 0))
         cur-ctg-indc
         rest-ctg-inc
         cur-head-group
         rest-head-group
         caller-name
         caller-name-body
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
     ((null cur-ctg-indc)
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
           category-name-prefix depth))

    ;; generate caller name
    (setq caller-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix nil depth))

    (setq caller-name-body
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix t depth))

    ;; generate next category-name
    (unless (null rest-head-group)
      (setq next-category-name
            (entropy/emacs-hydra-hollow-category-get-category-name
             category-name-prefix (+ depth 1))))

    ;; generate hydra-keymap name
    (setq cur-hydra-keymap-name
          (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
           category-name-prefix 'hydra-map depth))

    ;; define pretty hydra
    (funcall
     `(lambda ()
        (pretty-hydra-define
          ,caller-name
          ,body-patch
          ,cur-head-group)))

    ;; define current category nav keys
    (entropy/emacs-hydra-hollow-category-define-nav-key
     (symbol-value cur-hydra-keymap-name)
     (when previous-category-name
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        category-name-prefix t (- depth 1)))
     (when (not (null rest-head-group))
       (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
        category-name-prefix t (+ depth 1))))

    ;; recursive define category hydra
    (when (not (null rest-head-group))
      (entropy/emacs-hydra-hollow-category-frame-work-define
       category-name-prefix pretty-body rest-head-group
       (+ depth 1) category-name rest-ctg-inc))

    ;; return category plist
    (set category-name
         (list :category-name-prefix category-name-prefix
               :category-name category-name
               :caller-name caller-name
               :caller-body caller-name-body
               :caller-base-pretty-body pretty-body
               :groups (cl-loop for item in cur-head-group
                                when (not (listp item))
                                collect item)
               :depth depth
               :category-width cur-ctg-indc
               :previous-category-name previous-category-name
               :next-category-name next-category-name))

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
                   (matched-depth
                    (plist-get matched-ctg :depth))
                   (matched-hydra-key-map-name
                    (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                     category-name-prefix 'hydra-map matched-depth))
                   (matched-previous-caller
                    (plist-get
                     (symbol-value
                      (plist-get matched-ctg
                                 :previous-category-name))
                     :caller-body))
                   (matched-next-caller
                    (plist-get
                     (symbol-value
                      (plist-get matched-ctg
                                 :next-category-name))
                     :caller-body))
                   (caller-name (plist-get
                                 matched-ctg
                                 :caller-name)))
              ;; inject heads
              (funcall
               `(lambda ()
                  (pretty-hydra-define+ ,caller-name
                    nil
                    ,part-ctg)))

              ;; reset nav key
              (entropy/emacs-hydra-hollow-category-define-nav-key
               (symbol-value matched-hydra-key-map-name)
               matched-previous-caller
               matched-next-caller)
              ))
           (t
            (let* ((depth (plist-get ctg-match :depth))
                   (tail-category-depth (- depth 1))
                   (tail-category-name
                    (entropy/emacs-hydra-hollow-category-get-category-name
                     category-name-prefix tail-category-depth))

                   (tail-category (symbol-value tail-category-name))

                   (tail-category-previous-ctg-caller-body
                    (plist-get
                     (symbol-value
                      (plist-get tail-category
                                 :previous-category-name))
                     :caller-body))

                   (tail-category-caller-name (plist-get tail-category :caller-name))

                   (tail-category-hydra-keymap-name
                    (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                     category-name-prefix 'hydra-map tail-category-depth))

                   (tail-category-used-pretty-body-name
                    (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                     category-name-prefix 'hydra-pretty-body tail-category-depth))

                   (tail-category-used-pretty-body
                    (copy-tree
                     (symbol-value
                      tail-category-used-pretty-body-name)))

                   (tail-category-hydra-heads-plist
                    (symbol-value
                     (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                      category-name-prefix 'hydra-group-heads tail-category-depth)))

                   (tail-category-used-pretty-body-title
                    (plist-get tail-category-used-pretty-body
                               :title))

                   (tail-category-ctg-width (plist-get tail-category :category-width))
                   (tail-category-groups (copy-tree (plist-get tail-category :groups)))

                   (tail-category-overflow-p
                    (>= (length tail-category-groups)
                        tail-category-ctg-width))

                   (inherit-base-pretty-body
                    (plist-get tail-category
                               :caller-base-pretty-body)))
              (cond
               ((null tail-category-overflow-p)
                ;; inject heads
                (funcall
                 `(lambda ()
                    (pretty-hydra-define+ ,tail-category-caller-name
                      nil
                      ,part-ctg)))

                ;; patch tail category groups slot
                (set tail-category-caller-name
                     (plist-put tail-category
                                :groups
                                (append tail-category-groups
                                        (list group))))

                ;; reset previous body key
                (entropy/emacs-hydra-hollow-category-define-nav-key
                 (symbol-value tail-category-hydra-keymap-name)
                 tail-category-previous-ctg-caller-body))

               (tail-category-overflow-p
                (let* ((new-ctg-name
                        (entropy/emacs-hydra-hollow-category-get-category-name
                         category-name-prefix depth))
                       (new-ctg-caller-body
                        (entropy/emacs-hydra-hollow-category-get-hydra-branch-name
                         category-name-prefix t depth)))
                  ;; add new tail category
                  (entropy/emacs-hydra-hollow-category-frame-work-define
                   category-name-prefix inherit-base-pretty-body part-ctg
                   depth tail-category-name category-width-indicator-for-inject)

                  ;; pop out current category-width-indicator-for-inject
                  (when (not (null category-width-indicator-for-inject))
                    (pop category-width-indicator-for-inject))

                  ;; modify tail category's pretty-body for add 'down' nav prompt
                  (let* ((tail-category-used-pretty-body-new-title
                          (entropy/emacs-hydra-hollow-category-concat-title-for-nav
                           tail-category-used-pretty-body-title
                           nil t)))
                    (setq tail-category-used-pretty-body
                          (plist-put
                           tail-category-used-pretty-body
                           :title
                           tail-category-used-pretty-body-new-title))
                    (set tail-category-used-pretty-body-name
                         tail-category-used-pretty-body)

                    (funcall
                     `(lambda ()
                        (pretty-hydra-define ,tail-category-caller-name
                          ,tail-category-used-pretty-body
                          ,tail-category-hydra-heads-plist))))

                  ;; set tail category next-category-name
                  (set tail-category-name
                       (plist-put
                        tail-category :next-category-name
                        new-ctg-name))

                  ;; reset nav key for tail category hydra keymap
                  (entropy/emacs-hydra-hollow-category-define-nav-key
                   (symbol-value tail-category-hydra-keymap-name)
                   tail-category-previous-ctg-caller-body
                   new-ctg-caller-body)

                  )))))))))))


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

(defvar entropy/emacs-hydra-hollow-predicate-union-form
  '(lambda ()))

(defvar entropy/emacs-hydra-hollow--predicate-union-form-temp nil)

(defun entropy/emacs-hydra-hollow-predicate-call-union-form (&rest _)
  (unless (equal entropy/emacs-hydra-hollow--predicate-union-form-temp
                 entropy/emacs-hydra-hollow-predicate-union-form)
    (setq
     entropy/emacs-hydra-hollow--predicate-union-form-temp
     entropy/emacs-hydra-hollow-predicate-union-form)
    (funcall entropy/emacs-hydra-hollow-predicate-union-form)))

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
        (setq entropy/emacs-hydra-hollow-predicate-union-form
              (append entropy/emacs-hydra-hollow-predicate-union-form
                      `((entropy/emacs-lazy-load-simple ,feature
                          (define-key ,map (kbd ,key) #',command))))))
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
            (setq entropy/emacs-hydra-hollow-predicate-union-form
                  (append entropy/emacs-hydra-hollow-predicate-union-form
                          `((entropy/emacs-lazy-load-simple ,feature
                              (define-key ,map (kbd ,key) #',command))))))
          (when do-inject-spec-maps
            (when (and (listp do-inject-spec-maps)
                       (listp (car do-inject-spec-maps)))
              (dolist (item do-inject-spec-maps)
                (let ((feature (car item))
                      (map (cadr item))
                      (key-for (or (nth 2 item) key)))
                  (setq entropy/emacs-hydra-hollow-predicate-union-form
                        (append entropy/emacs-hydra-hollow-predicate-union-form
                                `((entropy/emacs-lazy-load-simple ,feature
                                    (define-key ,map (kbd ,key-for) #',command)))))
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
      (setq entropy/emacs-hydra-hollow-predicate-union-form
            (append entropy/emacs-hydra-hollow-predicate-union-form
                    `((entropy/emacs-!set-key
                        (kbd ,key)
                        #',command)))))
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

;; *** heads notation handler

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

(defvar entropy/emacs-hydra-hollow-init-top-dispatch-ctg-name-prefix
  'entropy/emacs-hydra-hollow-top-dispatch)

(defun entropy/emacs-hydra-hollow-init-top-dispatch (&optional force)
  (when (or (not entropy/emacs-hydra-hollow-top-dispatch-init-done)
            force)
    (let* ((ctg-name-prefix entropy/emacs-hydra-hollow-init-top-dispatch-ctg-name-prefix)
           (ctg-top-caller-body
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
          ctg-top-caller-body)
        (advice-add ctg-top-caller-body
                    :before
                    #'entropy/emacs-hydra-hollow-predicate-call-union-form)))))

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

(advice-add 'entropy/emacs-hydra-hollow-category-major-mode-hydra
            :before
            #'entropy/emacs-hydra-hollow-predicate-call-union-form)

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
