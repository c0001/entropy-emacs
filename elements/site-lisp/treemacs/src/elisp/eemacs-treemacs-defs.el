;; -*- lexical-binding: t; -*-
(defvar eemacs-treemacs/var/func-indicator nil)
(defmacro eemacs-treemacs/macro/defun-with-op-indc (&rest args)
  (declare (doc-string 3) (indent 2))
  (let* ((fsym (car args))
         (arg-list (cadr args))
         (doc  (and (stringp (nth 2 args)) (nth 2 args)))
         (decl (if doc (and (listp (nth 3 args)) (eq 'declare (car (nth 3 args))) (nth 3 args))
                 (and (listp (nth 2 args)) (eq 'declare (car (nth 2 args))) (nth 2 args))))
         (inct (if (and doc decl) (and (listp (nth 4 args)) (eq 'interactive (car (nth 4 args))) (nth 4 args))
                 (if (or doc decl)
                     (and (listp (nth 3 args)) (eq 'interactive (car (nth 3 args))) (nth 3 args))
                   (and (listp (nth 2 args)) (eq 'interactive (car (nth 2 args))) (nth 2 args)))))
         (body (if doc (if decl (if inct (nthcdr 5 args) (nthcdr 4 args))
                         (if inct (nthcdr 4 args) (nthcdr 3 args)))
                 (if decl (if inct (nthcdr 4 args) (nthcdr 3 args))
                   (if inct (nthcdr 3 args) (nthcdr 2 args)))))
         (hbody (delete nil `(,doc ,decl ,inct))))
    `(defun ,fsym ,arg-list ,@hbody
            (let ((eemacs-treemacs/var/func-indicator ',fsym))
              (ignore eemacs-treemacs/var/func-indicator)
              ,@body))))
(defalias 'etm/defun 'eemacs-treemacs/macro/defun-with-op-indc)

(defvar eemacs-treemacs/var/catch-sym (make-symbol ":eemacs-treemacs-catch-sym"))
(defmacro eemacs-treemacs/macro/catch (&rest body)
  `(catch (quote ,eemacs-treemacs/var/catch-sym) ,@body))
(defmacro eemacs-treemacs/macro/return (rtn)
  `(throw (quote ,eemacs-treemacs/var/catch-sym) ,rtn))

(defmacro eemacs-treemacs/macro/let* (&rest args)
  (declare (indent 1))
  `(eemacs-treemacs/macro/catch (let* ,@args)))

(defun eemacs-treemacs/func/treemacs-maybe-init ()
  (let ((treemacs-select-when-already-in-treemacs 'stay))
    (pcase (treemacs-current-visibility)
      ('exists  (treemacs-select-window))
      ('none    (treemacs-select-window))
      ('visible (treemacs-select-window)))))

(defmacro etm/wbf (button &rest body)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((bt button) (btf `(and (markerp ,bt) (marker-buffer ,bt))))
    `(if ,btf (with-current-buffer ,btf ,@body) ,@body)))
(defmacro etm/let* (button &rest args)
  (declare (indent 1))
  `(etm/wbf ,button (let* ,@args)))

(defmacro eemacs-treemacs/sleep-while (out &rest main)
  (declare (indent 2))
  (macroexp-let2* ignore
      ((cnt 0))
    `(progn
       (while (and (progn ,@main)
                   (if (<= ,cnt 30) t ,out nil))
         (sleep-for 0.1) (cl-incf ,cnt)))))
(defvar-local eemacs-treemacs/var/lsp-mode-imenu-init-done nil)
(defun eemacs-treemacs/func/imenu--make-index-alist (&rest _)
  (progn
    (cond
     ;; synchronously grab lsp-mode imenu data to prevent from mading
     ;; difference between treemacs tag expanding data and the exact
     ;; imenu data in buffer since lsp-mode grab data asynchronously
     ;; on where the initial imenu data retrieved by treemacs is not
     ;; the final one.
     ((and (bound-and-true-p lsp-mode) (bound-and-true-p lsp-enable-imenu)
           (not (bound-and-true-p lsp--document-symbols))
           (not (bound-and-true-p eemacs-treemacs/var/lsp-mode-imenu-init-done)))
      (message "waitting lsp document symbols grabbing ...")
      (eemacs-treemacs/sleep-while
          (unless eemacs-treemacs/var/lsp-mode-imenu-init-done
            (setq eemacs-treemacs/var/lsp-mode-imenu-init-done 'err)
            (user-error "can not grab lsp document symbols, just done!"))
          (null (let ((lsp--document-symbols-request-async t))
                  (condition-case e
                      (or (lsp--get-document-symbols)
                          (setq eemacs-treemacs/var/lsp-mode-imenu-init-done
                                t))
                    (error nil))))))
     (t nil))
    (imenu--make-index-alist t)))

(defun eemacs-treemacs/func/find-file-noselect (file)
  ;; use `find-file' with window and buffer restored against
  ;; `find-file-noselect' since it pressed all mode hooks ran on where
  ;; we expected.
  (save-excursion
    (save-window-excursion
      (message "etm ffn: %s" file)
      (find-file file) (current-buffer))))

(provide 'eemacs-treemacs-defs)
