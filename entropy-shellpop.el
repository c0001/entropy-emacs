;; * code

;; ** require
(require 'shackle)
(require 'cl-lib)

;; ** defcustom
(setq entropy/shellpop-pop-types
  '((:type-name
     "eemacs-ansiterm"
     :size 0.3
     :align below
     :bind "<f10>"
     :type-body
     ((ansi-term "/bin/bash")))
    (:type-name
     "eemacs-eshell"
     :size 0.3
     :align below
     :bind "<f9>"
     :type-body
     ((eshell)))))

;; ** defvar

(defvar entropy/shellpop-type-register nil)

;; ** library

(defun entropy/shellpop--gen-buffn-regexp (shellpop-type-name)
  (concat "^\\*"
          (regexp-quote shellpop-type-name)
          "-\\[\\([0-9]+\\)\\]\\*$"))

(defun entropy/shellpop--gen-buffn-fmstr (shellpop-type-name)
  (concat "*" shellpop-type-name "-[%s]*"))

(defun entropy/shellpop--gen-type-func-name (shellpop-type-name func-type)
  (cl-case func-type
    (core (concat "entropy/shellpop-user-" shellpop-type-name "-shellpop-core"))
    (t (concat "entropy/shellpop-user-" shellpop-type-name "-shellpop"))))

(defun entropy/shellpop--get-type-buffer-indexs (shellpop-type-name)
  (let* ((buffns (mapcar (lambda (buff) (buffer-name buff)) (buffer-list)))
         (buffn-regexp (entropy/shellpop--gen-buffn-regexp shellpop-type-name))
         (index-list '()))
    (dolist (buffn buffns)
      (when (string-match buffn-regexp buffn)
        (push (string-to-number (match-string 1 buffn))
              index-list)))
    index-list))

(defun entropy/shellpop--get-type-free-indexs (index-list)
  (cl-loop for slot from 0 to (apply 'max index-list)
           when (not (member slot index-list))
           collect slot))

(defun entropy/shellpop--get-type-buffer-obj (shellpop-type-name &optional index)
  (let* ((type-buffer-indexs (entropy/shellpop--get-type-buffer-indexs shellpop-type-name))
         (buffn-fmstr (entropy/shellpop--gen-buffn-fmstr shellpop-type-name))
         (free-indexs (if type-buffer-indexs
                          (entropy/shellpop--get-type-free-indexs
                           type-buffer-indexs)
                        nil)))
    (cond
     ((null type-buffer-indexs)
      (let ((buffn (format buffn-fmstr 0)))
        (list :isnew t :activep (entropy/shellpop--buffer-active-p buffn)
              :index 0 :buffer-name buffn)))
     (index
      (cond ((> index (apply 'max type-buffer-indexs))
             (let ((buffn (format buffn-fmstr index)))
               (list :isnew t :activep (entropy/shellpop--buffer-active-p buffn)
                     :index index :buffer-name buffn)))
            ((< index 0)
             (entropy/shellpop--get-type-buffer-obj shellpop-type-name))
            ((and free-indexs (member index free-indexs))
             (let ((buffn (format buffn-fmstr index)))
               (list :isnew t :activep (entropy/shellpop--buffer-active-p buffn)
                     :index index :buffer-name buffn)))
            (t
             (let ((buffn (format buffn-fmstr index)))
               (list :isnew nil
                     :activep (entropy/shellpop--buffer-active-p buffn)
                     :index index :buffer-name buffn)))))
     (t
      (let* ((index-pick (or (and free-indexs (car free-indexs))
                             (+ 1 (apply 'max type-buffer-indexs))))
             (buffn (format buffn-fmstr index-pick)))
        (list :isnew t :activep (entropy/shellpop--buffer-active-p buffn)
              :index index-pick :buffer-name buffn))))))

(defun entropy/shellpop--buffer-active-p (buffer-name)
  (get-buffer-window buffer-name))

(defun entropy/shellpop--prune-type-register-core (shellpop-type-register)
  (let* ((type-name (car shellpop-type-register))
         (type-plist (cdr shellpop-type-register))
         (type-indexs (plist-get type-plist :indexs))
         (type-pointer (plist-get type-plist :pointer))
         (type-pointer-alivep (if (integerp type-pointer)
                                  (not (plist-get (entropy/shellpop--get-type-buffer-obj
                                                   type-name type-pointer)
                                                  :isnew))
                                nil))
         ret)
    ;; pointer reset
    (unless type-pointer-alivep
      (setf type-plist (plist-put type-plist :pointer nil)))
    ;; indexs reset
    (when (not (null type-indexs))
      (dolist (index type-indexs)
        (let ((buffer-obj (entropy/shellpop--get-type-buffer-obj type-name (car index))))
          (when (plist-get buffer-obj :isnew)
            (push index ret))))
      (dolist (index ret)
        (setq type-indexs (delete index type-indexs)))
      (setf type-plist
            (plist-put type-plist :indexs type-indexs)))))

(defun entropy/shellpop--prune-type-register ()
  (dolist (shellpop-type-register entropy/shellpop-type-register)
    (entropy/shellpop--prune-type-register-core shellpop-type-register)))

(defun entropy/shellpop--type-index-member (index shellpop-type-register-index)
  (let ((cur-indexs shellpop-type-register-index))
    (catch :exit
      (dolist (entry cur-indexs)
        (when (eq index (car entry))
          (throw :exit t))))))

(defun entropy/shellpop--put-index (shellpop-type-name buff-index)
  (let* ((type-name shellpop-type-name)
         (shellpop-type-register (assoc type-name entropy/shellpop-type-register))
         (cur-type-plist (cdr shellpop-type-register))
         (cur-type-indexs (plist-get cur-type-plist :indexs))
         (cur-type-pointer (plist-get cur-type-plist :pointer)))
    (unless (entropy/shellpop--type-index-member buff-index cur-type-indexs)
      (setf cur-type-plist
            (plist-put cur-type-plist :indexs
                       (append (list (cons buff-index (read-string "Type slot 'DES': ")))
                               cur-type-indexs))))
    (unless (eq cur-type-pointer buff-index)
      (setf cur-type-plist
            (plist-put cur-type-plist
                       :pointer buff-index)))))

(defun entropy/shellpop--make-prompt (shellpop-type-register-index)
  (let* ((name-list (entropy/cl-make-name-alist
                     shellpop-type-register-index
                     (lambda (x) (concat (number-to-string (car x))
                                         ": "
                                         (cdr x)))))
         (choice (completing-read "Select slot: " name-list)))
    (cadr (assoc choice name-list))))

(defun entropy/shellpop--make-type-core (shellpop-type)
  (let* ((type-name (plist-get shellpop-type :type-name))
         (func-name-core (entropy/shellpop--gen-type-func-name
                          type-name 'core))
         (func-name (entropy/shellpop--gen-type-func-name
                     type-name t))
         (type-size (plist-get shellpop-type :size))
         (type-align (plist-get shellpop-type :align))
         (type-bind (plist-get shellpop-type :bind))
         (type-body (plist-get shellpop-type :type-body))
         (buffern-regexp (entropy/shellpop--gen-buffn-regexp type-name)))
    (list
     `(defun ,(intern func-name-core) (&optional index)
        (let* ((shackle-rules '((,buffern-regexp :regexp t
                                                 :select t
                                                 :align ,type-align
                                                 :size ,type-size)))
               (buffer-ob (entropy/shellpop--get-type-buffer-obj ,type-name index))
               (buffn (plist-get buffer-ob :buffer-name))
               (buff-activep (plist-get buffer-ob :activep))
               (buff-isnew (plist-get buffer-ob :isnew))
               (buff-index (plist-get buffer-ob :index))
               (buff (get-buffer-create buffn))
               (old-type-register (copy-tree (assoc ,type-name entropy/shellpop-type-register)))
               unwind-trigger)
          (unwind-protect
              (progn 
                (entropy/shellpop--put-index ,type-name buff-index)
                (if buff-activep
                    (delete-window (get-buffer-window buffn))
                  (display-buffer buff)
                  (when buff-isnew
                    ,@type-body
                    (unless (equal buffn (buffer-name))
                      (kill-buffer buff)
                      (rename-buffer buffn))))
                (setq unwind-trigger t))
            (unless unwind-trigger
              (setf (alist-get ,type-name entropy/shellpop-type-register)
                    (cdr old-type-register))
              (kill-buffer buff)))))

     `(defun ,(intern func-name) (prompt)
        (interactive "P")
        (entropy/shellpop--prune-type-register)
        (let* ((type-reg (assoc ,type-name entropy/shellpop-type-register))
               (type-plist (cdr type-reg))
               (type-pointer (plist-get type-plist :pointer))
               (type-indexs (plist-get type-plist :indexs)))
          (cond (prompt
                 (cond
                  ((not (null type-indexs))
                   (cond ((and (listp prompt)
                               (eq (car prompt) 16))
                          (,(intern func-name-core)))
                         ((listp prompt)
                          (if (null (cdr type-indexs))
                              (,(intern func-name-core) type-pointer)
                            (let ((choice (entropy/shellpop--make-prompt type-indexs)))
                              (,(intern func-name-core) choice))))
                         ((integerp prompt)
                          (,(intern func-name-core) prompt))))
                  (t
                   (,(intern func-name-core) type-pointer))))
                (t (,(intern func-name-core) type-pointer)))))
     `(when (stringp ,type-bind)
        (global-set-key (kbd ,type-bind) #',(intern func-name))))))

(defun entropy/shellpop--make-types ()
  (dolist (shellpop-type entropy/shellpop-pop-types)
    (let* ((type-name (plist-get shellpop-type :type-name))
           (type-func-core (intern (entropy/shellpop--gen-type-func-name type-name 'core)))
           (type-func (intern (entropy/shellpop--gen-type-func-name type-name t)))
           (type-def (entropy/shellpop--make-type-core shellpop-type)))
      (funcall `(lambda () ,@type-def))
      (push (cons type-name
                  (copy-tree
                   `(:type-func ,(list type-func-core type-func)
                                :indexs nil :pointer nil)))
            entropy/shellpop-type-register))))


