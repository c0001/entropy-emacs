;; -*- lexical-binding: t; -*-
;; * code
;; ** require
(eval-when-compile
  (require 'eieio)
  (require 'rx))

(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (require 'treesit)
  (defvar treesit-language-source-alist))

;; ** libs
(cl-defmacro eemacs/lang/macro/oset (obj &rest slots)
  (declare (indent 1))
  (let (key val form)
    (while slots
      (setq key (pop slots) val (pop slots))
      (when (eq key :list) (setq val `(ensure-list ,val)))
      (push `(oset ,obj ,key ,val) form))
    (when form
      (cons 'progn (nreverse form)))))

(cl-defmacro eemacs/lang/macro/oref (obj &rest slots)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((oobj nil) (ooform nil) (obj obj))
    (let (slts form sltmp)
      (dolist (slt slots)
        (if (not (keywordp slt))
            (push slt slts)
          (push (intern (substring (symbol-name slt) 1)) slts)))
      (and slts (setq slts (nreverse slts)))
      (while slts
        (setq sltmp (pop slts))
        (setq form (if (not form)
                       `(let ((,oobj (and (eieio-object-p ,obj)
                                          (slot-boundp ,obj ',sltmp)
                                          (oref ,obj ,sltmp))))
                          ,oobj)
                     `(let* ((,ooform ,form)
                             (,oobj (and (eieio-object-p ,ooform)
                                         (slot-boundp ,ooform ',sltmp)
                                         (oref ,ooform ,sltmp))))
                        ,oobj))))
      form)))

(cl-defmacro eemacs/lang/func/define-general-probe
    (&rest body &key with-this-as
           &allow-other-keys)
  (let* ((body (entropy/emacs--get-def-body body 'with-safe))
         (bofarg (make-symbol "buffer-or-file"))
         (it (or with-this-as 'probe))
         (probe/var/buffer (intern (concat (symbol-name it) "/var/buffer")))
         (probe/var/file (intern (concat (symbol-name it) "/var/file")))
         (probe/var/fext (intern (concat (symbol-name it) "/var/fext")))
         (bop (make-symbol "bop")))
    `(let ((fnm (make-symbol "eemacs/lang/probe-func")))
       (entropy/emacs-defalias fnm
         (lambda (&optional ,bofarg)
           (let* ((,probe/var/buffer
                   (and (bufferp ,bofarg)
                        (buffer-live-p ,bofarg)
                        ,bofarg))
                  (,probe/var/file
                   (if ,probe/var/buffer (buffer-file-name ,probe/var/buffer)
                     (and (stringp ,bofarg)
                          (file-exists-p ,bofarg)
                          ,bofarg)))
                  (,probe/var/fext
                   (and ,probe/var/file
                        (file-name-extension ,probe/var/file)))
                  ,bop)
             (ignore ,probe/var/buffer ,probe/var/file ,probe/var/fext)
             (let ((,probe/var/buffer (or ,probe/var/buffer
                                          (when ,probe/var/file
                                            (or (find-buffer-visiting ,probe/var/file)
                                                (prog1 (find-file-noselect ,probe/var/file)
                                                  (setq ,bop t)))))))
               (unwind-protect (progn ,@body)
                 (when ,bop
                   (let ((inhibit-quit t))
                     (kill-buffer ,probe/var/buffer))))))))
       (put fnm :is-eemacs/lang/probe-func-p t)
       fnm)))

(defun eemacs/lang/func//cond-match-p (cond cond-map)
  (let (extractor elt)
    (when (setq elt (alist-get cond cond-map nil nil 'equal))
      (setq
       extractor (or (plist-get elt :extractor) 'identity)
       elt       (plist-get elt :val))
      (funcall extractor elt))))
(cl-defmacro eemacs/lang/macro/define-probe
    (&key
     with-this-as
     with-conds-pattern
     &allow-other-keys)
  (let* ((it (or with-this-as 'probe))
         (probe/var/buffer (intern (concat (symbol-name it) "/var/buffer")))
         ;; (probe/var/file (intern (concat (symbol-name it) "/var/file")))
         (probe/var/fext (intern (concat (symbol-name it) "/var/fext"))))
    (macroexp-let2* ignore ((cods nil))
      `(eemacs/lang/func/define-general-probe
        :with-this-as ,with-this-as
        (let ((,cods ,with-conds-pattern) extp)
          (catch :exit
            (dolist (cod ,cods)
              (entropy/emacs-setf-by-body extp
                (cond
                 ((and (eq (car cod) 'major-mode) ,probe/var/buffer)
                  (with-current-buffer ,probe/var/buffer
                    (eemacs/lang/func//cond-match-p
                     major-mode (cdr cod))))
                 ((and (eq (car cod) 'file-ext) ,probe/var/fext)
                  (eemacs/lang/func//cond-match-p
                   ,probe/var/fext
                   (cdr cod)))
                 ((eq (car cod) '_) (cdr cod))
                 ((eq (car cod) 'function) (funcall (cdr cod)))))
              (and extp (throw :exit nil))))
          extp)))))

;; ** classes
(defun eemacs/lang/probe-func-type-p (sym)
  (or
   (and (symbolp sym) (functionp sym)
        (eq (get sym :is-eemacs/lang/probe-func-p) t))))
(defclass eemacs/lang/class/core ()
  ((name          :initarg :name :type string)
   (fnm-regexp    :initarg :fnm-regexp :type string)))
(defclass eemacs/lang/class//list-probe ()
  ((_))
  :abstract t)
(defun eemacs/lang/class//list-probe/method/call
    (obj &optional buffer-or-file)
  (if (or (not (eieio-object-p obj))
          (not (memq (eieio-object-class obj)
                     (eieio-class-children
                      'eemacs/lang/class//list-probe))))
      nil
    (let ((l  (and (slot-boundp obj 'list)
                   (slot-value  obj 'list)))
          (pb (and (slot-boundp obj 'probe)
                   (slot-value  obj 'probe))))
      (if pb (funcall pb buffer-or-file)
        (and (consp l) (not (cdr l)) (car l))))))
(defclass eemacs/lang/class/modes (eemacs/lang/class//list-probe)
  ((list          :initarg :list :type (or null (satisfies consp))
                  :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/lang/class/ids (eemacs/lang/class//list-probe)
  ((list          :initarg :list :type (or null (satisfies consp))
                  :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/lang/class/treesit ()
  ((id            :initarg :id            :type string)
   (repo-type     :initarg :repo-type     :type (or string null)
                  :initform nil)
   (repo-url      :initarg :repo-url      :type (or string null)
                  :initform nil)
   (repo-revision :initarg :repo-revision :type (or string null)
                  :initform nil)
   (repo-src-dir  :initarg :repo-src-dir  :type (or string null)
                  :initform nil)
   (compile-cc    :initarg :compile-cc    :type (or string null)
                  :initform nil)
   (compile-c++   :initarg :compile-c++   :type (or string null)
                  :initform nil)
   (modes         :initarg :modes
                  :type (or eemacs/lang/class//list-probe null)
                  :initform nil)
   (installer     :initarg :installer :type (or null (satisfies functionp))
                  :initform nil)))

(defun eemacs/lang/class/subrecipes/pred/list-type-p (x)
  (and (listp x)
       (catch :exit
         (dolist (y x)
           (unless (eemacs/lang/class/recipe-p y)
             (throw :exit nil)))
         t)))
(defclass eemacs/lang/class/subrecipes (eemacs/lang/class//list-probe)
  ((list :initarg :list
         :type (or
                null
                (and
                 (satisfies consp)
                 (satisfies eemacs/lang/class/subrecipes/pred/list-type-p)))
         :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/lang/class/recipe ()
  ((core    :initarg :core    :type eemacs/lang/class/core)
   (modes   :initarg :modes   :type eemacs/lang/class/modes)
   (ids     :initarg :ids     :type eemacs/lang/class/ids)
   (treesit :initarg :treesit :type (or eemacs/lang/class/treesit null))
   (subrecipes :initarg :subrecipes
               :type (or eemacs/lang/class/subrecipes
                         null)
               :initform nil)))

(defvar eemacs/lang/var/recipe-alist nil)
(defun eemacs/lang/func/get-recipe-modes (lang-recipe &optional type)
  (if (not (eemacs/lang/class/recipe-p lang-recipe)) nil
    (cl-case type
      (treesit (eemacs/lang/macro/oref lang-recipe :treesit :modes :list))
      (prog    (eemacs/lang/macro/oref lang-recipe :modes :list))
      (all     (append (eemacs/lang/func/get-recipe-modes lang-recipe 'prog)
                       (eemacs/lang/func/get-recipe-modes lang-recipe 'treesit)))
      (t (eemacs/lang/func/get-recipe-modes lang-recipe 'prog)))))
(defun eemacs/lang/func/get-recipes-modes (&optional type)
  (let (rtn mds)
    (dolist (rec eemacs/lang/var/recipe-alist)
      (setq mds (eemacs/lang/func/get-recipe-modes (cdr rec) type))
      (and  mds (setq rtn (append rtn mds))))
    rtn))
(defun eemacs/lang/func/bof/lang-recipe (buffer-or-file)
  (let (;; lang-name
        lang-rec lang-fnm-regexp)
    (funcall
     (eemacs/lang/func/define-general-probe
      :with-this-as the
      (catch :exit
        (dolist (rec eemacs/lang/var/recipe-alist)
          (setq ;; lang-name (car rec)
                lang-rec (cdr rec)
                lang-fnm-regexp
                (eemacs/lang/macro/oref lang-rec :core :fnm-regexp))
          (when
              (and the/var/file
                   lang-fnm-regexp
                   (string-match-p
                    lang-fnm-regexp
                    (file-name-nondirectory the/var/file)))
            (throw :exit rec))
          (when (and the/var/buffer
                     (memq (buffer-local-value 'major-mode the/var/buffer)
                           (eemacs/lang/macro/oref lang-rec :modes :list)))
            (throw :exit rec)))
        nil))
     buffer-or-file)))
(defun eemacs/lang/func/bof/lang-name (buffer-or-file)
  (when-let ((rec (eemacs/lang/func/bof/lang-recipe buffer-or-file)))
    (car rec)))
(defun eemacs/lang/func/bof/modes (buffer-or-file &optional type)
  (let ((rec (eemacs/lang/func/bof/lang-recipe buffer-or-file)))
    (when rec
      (setq rec (cdr rec))
      (eemacs/lang/class//list-probe/method/call
       (cl-case type
         (treesit (eemacs/lang/macro/oref rec :treesit :modes))
         (t (eemacs/lang/macro/oref rec :modes)))
       buffer-or-file))))
(defun eemacs/lang/func/mode/treesit-mode-p (mode)
  (let (;; lang-name
        lang-rec lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-ts-modes
              (eemacs/lang/macro/oref lang-rec
                :treesit :modes :list))
        (if (memq mode lang-ts-modes) (throw :exit t)))
      nil)))
(defun eemacs/lang/func/mode/prog-mode-p (mode)
  (let (;; lang-name
        lang-rec lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/lang/macro/oref lang-rec :modes :list))
        (if (memq mode lang-modes) (throw :exit t))
        (setq lang-ts-modes
              (eemacs/lang/macro/oref lang-rec
                :treesit :modes :list))
        (if (memq mode lang-ts-modes) (throw :exit nil)))
      ;; FIXME: fine-tune default judgement
      (and (fboundp mode)
           (string-match-p "-mode\\'" (symbol-name mode))))))
(defun eemacs/lang/func/mode/prog-modes (mode &optional buffer-or-file)
  (let (;; lang-name
        lang-rec lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/lang/macro/oref lang-rec
                :treesit :modes :list))
        (when (memq mode (append lang-modes lang-ts-modes))
          (throw :exit (if buffer-or-file
                           (eemacs/lang/class//list-probe/method/call
                            (eemacs/lang/macro/oref lang-rec :modes) buffer-or-file)
                         lang-modes))))
      nil)))
(defun eemacs/lang/func/mode/treesit-modes (mode &optional buffer-or-file)
  (let (;; lang-name
        lang-rec lang-ts-obj lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/lang/macro/oref
                  (setq lang-ts-obj (eemacs/lang/macro/oref lang-rec :treesit))
                :modes :list))
        (when (memq mode lang-ts-modes)
          (throw :exit (if buffer-or-file
                           (eemacs/lang/class//list-probe/method/call
                            (eemacs/lang/macro/oref lang-ts-obj :modes)
                            buffer-or-file)
                         lang-ts-modes)))
        (when (memq mode lang-modes)
          (and (not lang-ts-obj) (throw :exit nil))
          (throw :exit
                 (if buffer-or-file
                     (eemacs/lang/class//list-probe/method/call
                      (eemacs/lang/macro/oref lang-ts-obj :modes)
                      buffer-or-file)
                   lang-ts-modes))))
      nil)))
(defun eemacs/lang/func/mode/treesit-id (mode)
  (let (;; lang-name
        lang-rec lang-ts-obj lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/lang/macro/oref
                  (setq lang-ts-obj (eemacs/lang/macro/oref lang-rec :treesit))
                :modes :list))
        (when (and lang-ts-obj (memq mode (append lang-modes lang-ts-modes)))
          (throw :exit
                 (eemacs/lang/macro/oref lang-ts-obj :id))))
      nil)))

(defvar eemacs/lang/var//treesit-parser-install-ok-exists nil)
(cl-defmacro eemacs/lang/macro/with-make-recipe
    (name &rest slots &key with-this-as with-modes-assoc-plist &allow-other-keys)
  (declare (indent 1))
  (let* ((this (or with-this-as 'this))
         (body (entropy/emacs--get-def-body slots 'with-safe))
         (this/obj/core    (intern (concat (symbol-name this) "/obj/core")))
         (this/obj/modes   (intern (concat (symbol-name this) "/obj/modes")))
         (this/obj/ids     (intern (concat (symbol-name this) "/obj/ids")))
         (this/obj/treesit (intern (concat (symbol-name this) "/obj/treesit")))
         (this/obj/subrecipes  (intern (concat (symbol-name this) "/obj/subrecipes")))
         (this/var/modes-assoc-plist (intern (concat (symbol-name this) "/var/modes-assoc-plist")))
         (this/var/prog-modes  (intern (concat (symbol-name this) "/var/prog-modes")))
         (this/var/ts-modes    (intern (concat (symbol-name this) "/var/treesit-modes")))
         (this/var/all-modes   (intern (concat (symbol-name this) "/var/all-modes"))))
    (macroexp-let2* ignore ((name name) (mal with-modes-assoc-plist))
      `(let* ((,this (eemacs/lang/class/recipe
                      :core    (make-instance 'eemacs/lang/class/core)
                      :modes   (make-instance 'eemacs/lang/class/modes)
                      :ids     (make-instance 'eemacs/lang/class/ids)
                      :treesit (make-instance 'eemacs/lang/class/treesit)
                      :subrecipes (make-instance 'eemacs/lang/class/subrecipes)))
              (,this/obj/core    (oref ,this core))
              (,this/obj/modes   (oref ,this modes))
              (,this/obj/ids     (oref ,this ids))
              (,this/obj/treesit (oref ,this treesit))
              (,this/obj/subrecipes (oref ,this subrecipes))
              (,this/var/modes-assoc-plist ())
              (,this/var/prog-modes ())
              (,this/var/ts-modes   ())
              (,this/var/all-modes  ()))
         (ignore ,this ,this/obj/core ,this/obj/modes ,this/obj/ids ,this/obj/treesit
                 ,this/obj/subrecipes
                 ,this/var/prog-modes ,this/var/ts-modes ,this/var/all-modes
                 ,this/var/modes-assoc-plist)
         (oset ,this/obj/core :name ,name)
         (when ,mal
           (setq ,this/var/modes-assoc-plist ,mal)
           (let (mds ts-mds)
             (while ,mal
               (setq mds (pop ,mal)
                     ts-mds (plist-get mds :treesit-modes)
                     mds    (plist-get mds :prog-modes))
               (setq ,this/var/prog-modes (append ,this/var/prog-modes (ensure-list mds)))
               (setq ,this/var/ts-modes   (append ,this/var/ts-modes (ensure-list ts-mds))))
             (setq ,this/var/all-modes (append ,this/var/prog-modes ,this/var/ts-modes))))
         (progn ,@body)
         (if (assoc ,name eemacs/lang/var/recipe-alist 'string=)
             (setf (alist-get ,name eemacs/lang/var/recipe-alist
                              nil nil 'string=)
                   ,this)
           (push (cons ,name ,this) eemacs/lang/var/recipe-alist))
         (let
             ((func-sym (intern (format "eemacs/lang/interact/install/treesit-parser/%s"
                                        (replace-regexp-in-string "[    ]+" "-" ,name))))
              core-func)
           (entropy/emacs-setf-by-body core-func
             (lambda ()
               (entropy/emacs-when-let*-firstn 3
                   ((trobj (oref ,this treesit))
                    (id (eemacs/lang/macro/oref trobj id))
                    (ids (ensure-list id))
                    (libnames
                     (let (l (tmpvar ids)
                             (soext
                              (or (car dynamic-library-suffixes)
                                  (signal 'treesit-error
                                          '("Emacs cannot figure out the file extension \
for dynamic libraries for this system, because `dynamic-library-suffixes' is nil"
                                            )))))
                       (while tmpvar
                         (push
                          (let ((the-id (pop tmpvar)))
                            (cons
                             the-id
                             (concat "libtree-sitter-" the-id
                                     soext)))
                          l))
                       (nreverse l)))
                    (libdir entropy/emacs-treesit-libs-default-load-path)
                    (subrecs (oref (oref ,this subrecipes) list))
                    (should-not-install-list nil)
                    nid)
                 (when libnames
                   (dolist (lb libnames)
                     (setq nid (car lb) lb (expand-file-name (cdr lb) libdir))
                     (when (file-exists-p lb)
                       (if eemacs/lang/var//treesit-parser-install-ok-exists
                           (push nid should-not-install-list)
                         (let ((inhibit-quit t)
                               (lb-bk (make-backup-file-name lb)))
                           (and (file-exists-p lb-bk) (delete-file lb-bk))
                           (rename-file lb lb-bk))))))
                 (if (memq id should-not-install-list)
                     (message "warn: treesit-parser '%s' already installed." id)
                   (let ((treesit-language-source-alist
                          `((,(intern id)
                             ,(oref trobj repo-url)
                             ,(oref trobj repo-revision)
                             ,(oref trobj repo-src-dir)
                             ,(oref trobj compile-cc)
                             ,(oref trobj compile-c++))))
                         (inhibit-quit t))
                     (entropy/emacs-with-eemacs-union-http-internet-proxy
                      (entropy/emacs-message-simple-progress-message
                          (format "Installing treesit grammer parser '%s'" ,name)
                        :with-maybe-modeline-msg 'force
                        (if (< emacs-major-version 30)
                            ;; FIXME: emacs 29's
                            ;; `treesit-install-language-grammar' not
                            ;; support customized outdir spec
                            (apply 'treesit--install-language-grammar-1
                                   libdir
                                   (assoc (intern id) treesit-language-source-alist))
                          (treesit-install-language-grammar
                           (intern id) libdir))))))
                 (when subrecs
                   (dolist (sc subrecs)
                     (funcall (oref (oref sc treesit) installer)))))))
           (defalias func-sym
             (lambda nil (interactive) (funcall core-func)))
           (oset ,this/obj/treesit :installer func-sym))
         ,this))))

(defun eemacs/lang/func/install/all-treesit-parsers nil
  (when (file-directory-p entropy/emacs-treesit-libs-default-load-path)
    (when-let ((files (directory-files entropy/emacs-treesit-libs-default-load-path
                                       t "\\`libtree-sitter-")))
      (dolist (el files) (delete-file el))))
  (let ((eemacs/lang/var//treesit-parser-install-ok-exists t))
    (dolist (rec eemacs/lang/var/recipe-alist)
      (when-let ((func
                  (eemacs/lang/macro/oref (cdr rec)
                    :treesit :installer)))
        (entropy/emacs-message-simple-progress-message
            (format "Install treesit parsers for lang %s" (car rec))
          (funcall func))))))

(defun eemacs/lang/assoc-plist/func/match
    (assoc-plist key-match match-val key-against)
  (let (mval aval rtn)
    (catch :exit
      (dolist (el assoc-plist)
        (when (and (setq mval (ensure-list (plist-get el key-match)))
                   (setq aval (plist-get el key-against))
                   (member match-val mval))
          (throw :exit (setq rtn aval)))))
    rtn))

;; ** recipes
;; *** Clojure
(eemacs/lang/macro/with-make-recipe "Clojure"
  :with-modes-assoc-plist
  '((:prog-modes
     (clojure-mode
      clojurescript-mode clojurec-mode
      clojuredart-mode jank-mode joker-mode)
     :treesit-modes clojure-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.cljc?s?d?\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes
    :probe
    (eemacs/lang/macro/define-probe
     :with-conds-pattern
     `((function
        .
        ,(lambda nil
           (when probe/var/buffer
             (car-safe (memq (buffer-local-value 'major-mode probe/var/buffer)
                             this/var/prog-modes)))))
       (file-ext
        ("clj"  :val clojure-mode)
        ("cljc" :val clojurec-mode)
        ("cljs" :val clojurescript-mode)
        ("cljd" :val clojuredart-mode)
        ("jank" :val jank-mode)
        ("joke" :val joker-mode)))))
  (eemacs/lang/macro/oset this/obj/ids :list "clojure")
  (eemacs/lang/macro/oset this/obj/treesit
    :id
    (car-safe (oref this/obj/ids list))
    :repo-url
    "https://github.com/sogaiu/tree-sitter-clojure"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes
     :probe
     (eemacs/lang/macro/define-probe
      :with-conds-pattern
      '((_ . clojure-ts-mode))))))

;; *** Python

(eemacs/lang/macro/with-make-recipe "Python"
  :with-modes-assoc-plist
  '((:prog-modes python-mode :treesit-modes python-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.py[iw]?\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "python")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "python"
    :repo-url "https://github.com/tree-sitter/tree-sitter-python"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes)))

;; *** Html
(eemacs/lang/macro/with-make-recipe "HTML"
  :with-modes-assoc-plist
  '((:prog-modes
     (html-mode web-mode mhtml-mode sgml-mode)
     :treesit-modes html-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.html\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "html")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "html"
    :repo-url "https://github.com/tree-sitter/tree-sitter-html"
    :modes
    (eemacs/lang/class/modes :list this/var/treesit-modes))
  (eemacs/lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** CSS
     (eemacs/lang/macro/with-make-recipe "CSS"
       :with-this-as this-css
       :with-modes-assoc-plist
       '((:prog-modes css-mode :treesit-modes css-ts-mode))
       (eemacs/lang/macro/oset this-css/obj/core
         :fnm-regexp "\\.css\\'")
       (eemacs/lang/macro/oset this-css/obj/modes
         :list this-css/var/prog-modes)
       (eemacs/lang/macro/oset this-css/obj/ids
         :list "css")
       (eemacs/lang/macro/oset this-css/obj/treesit
         :id "css"
         :repo-url "https://github.com/tree-sitter/tree-sitter-css"
         :modes (eemacs/lang/class/modes :list this-css/var/treesit-modes)))

;; **** XML
     (eemacs/lang/macro/with-make-recipe "XML"
       :with-this-as this-xml
       :with-modes-assoc-plist
       '((:prog-modes (xml-mode nxml-mode)))
       (eemacs/lang/macro/oset this-xml/obj/core
         :fnm-regexp "\\.xml\\'")
       (eemacs/lang/macro/oset this-xml/obj/modes
         :list this-xml/var/prog-modes)
       (eemacs/lang/macro/oset this-xml/obj/ids
         :list "xml"))
     )))

;; *** Javascript
(eemacs/lang/macro/with-make-recipe "JavaScript"
  :with-modes-assoc-plist
  `((:prog-modes
     (js-mode javascript-mode js2-mode)
     :treesit-modes
     js-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.js\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes
    :probe
    (eemacs/lang/macro/define-probe
     :with-conds-pattern
     `((function
        .
        ,(lambda nil
           (when probe/var/buffer
             (car-safe (memq (buffer-local-value 'major-mode probe/var/buffer)
                             this/var/prog-modes)))))
       (_ . js-mode))))
  (eemacs/lang/macro/oset this/obj/ids :list "javascript")
  (eemacs/lang/macro/oset this/obj/treesit
    :id (car-safe (oref this/obj/ids list))
    :repo-url "https://github.com/tree-sitter/tree-sitter-javascript"
    :repo-revision "master"
    :repo-src-dir  "src"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** JSON
     (eemacs/lang/macro/with-make-recipe "JSON"
       :with-this-as this-json
       :with-modes-assoc-plist
       '((:prog-modes (json-mode js-json-mode) :treesit-modes json-ts-mode))
       (eemacs/lang/macro/oset this-json/obj/core
         :fnm-regexp "\\.json\\'")
       (eemacs/lang/macro/oset this-json/obj/modes
         :list this-json/var/prog-modes)
       (eemacs/lang/macro/oset this-json/obj/ids
         :list "json")
       (eemacs/lang/macro/oset this-json/obj/treesit
         :id "json"
         :repo-url "https://github.com/tree-sitter/tree-sitter-json"
         :modes (eemacs/lang/class/modes :list this-json/var/treesit-modes)))

;; **** Typescript
     (eemacs/lang/macro/with-make-recipe "TypeScript"
       :with-this-as this-ts
       :with-modes-assoc-plist
       '((:prog-modes typescript-mode :treesit-modes typescript-ts-mode))
       (eemacs/lang/macro/oset this-ts/obj/core
         :fnm-regexp "\\.ts\\'")
       (eemacs/lang/macro/oset this-ts/obj/modes
         :list this-ts/var/prog-modes
         :probe
         (eemacs/lang/macro/define-probe
          :with-conds-pattern
          `((function
             .
             ,(lambda nil
                (when probe/var/buffer
                  (car-safe (memq (buffer-local-value 'major-mode probe/var/buffer)
                                  this-ts/var/prog-modes))))))))
       (eemacs/lang/macro/oset this-ts/obj/ids :list "typescript")
       (eemacs/lang/macro/oset this-ts/obj/treesit
         :id (car-safe (oref this-ts/obj/ids list))
         :repo-url "https://github.com/tree-sitter/tree-sitter-typescript"
         :repo-revision "master"
         :repo-src-dir "typescript/src"
         :modes
         (eemacs/lang/class/modes
          :list this-ts/var/treesit-modes))
       (eemacs/lang/macro/oset this-ts/obj/subrecipes
         :list
         (list
;; **** TypeScript JSX
          (eemacs/lang/macro/with-make-recipe "TypeScript JSX"
            :with-this-as this-jsx
            :with-modes-assoc-plist
            `((:prog-modes typescript-tsx-mode :treesit-modes tsx-ts-mode))
            (eemacs/lang/macro/oset this-jsx/obj/core
              :fnm-regexp "\\.tsx\\'")
            (eemacs/lang/macro/oset this-jsx/obj/modes
              :list this-jsx/var/prog-modes)
            (eemacs/lang/macro/oset this-jsx/obj/ids
              :list "tsx")
            (eemacs/lang/macro/oset this-jsx/obj/treesit
              :id (car-safe (oref this-jsx/obj/ids list))
              :repo-url "https://github.com/tree-sitter/tree-sitter-typescript"
              :repo-revision "master"
              :repo-src-dir "tsx/src"
              :modes
              (eemacs/lang/class/modes :list this-jsx/var/treesit-modes))))))

;; **** Vue
     (eemacs/lang/macro/with-make-recipe "Vue"
       :with-this-as this-vue
       :with-modes-assoc-plist
       `((:prog-modes vue-mode :treesit-modes vue-ts-mode))
       (eemacs/lang/macro/oset this-vue/obj/core
         :fnm-regexp "\\.vue\\'")
       (eemacs/lang/macro/oset this-vue/obj/modes
         :list this-vue/var/prog-modes)
       (eemacs/lang/macro/oset this-vue/obj/ids
         :list "vue")
       (eemacs/lang/macro/oset this-vue/obj/treesit
         :id (car-safe (oref this-vue/obj/ids list))
         :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-vue"
         :modes
         (eemacs/lang/class/modes :list this-vue/var/treesit-modes)))
     )))

;; *** ShellScript

(eemacs/lang/macro/with-make-recipe "Shell Script"
  :with-modes-assoc-plist
  '((:prog-modes sh-mode :treesit-modes bash-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp (rx "." (or "sh" "bash" "bashrc" "bash_profile" "fish" "zsh") line-end))
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list '("sh" "bash" "zsh" "fish")
    :probe
    (eemacs/lang/macro/define-probe
     :with-conds-pattern
     `((major-mode
        (bash-ts-mode . bash))
       (function
        .
        ,(lambda nil
           (when (and (fboundp 'entropy/emacs-shell-script-get-shell-type)
                      probe/var/buffer)
             (with-current-buffer probe/var/buffer
               (when (buffer-file-name)
                 (entropy/emacs-shell-script-get-shell-type))))))
       (file-ext
        ("sh" . sh)
        ("bash" . bash)
        ("bashrc" . bash)
        ("bash_profile" . bash)
        ("zsh" . zsh)
        ("fish" . fish)))))
  (eemacs/lang/macro/oset this/obj/treesit
    :id "bash"
    :repo-url "https://github.com/tree-sitter/tree-sitter-bash"
    :modes
    (eemacs/lang/class/modes
     :list '(bash-ts-mode)))
  (eemacs/lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** PowerShell
     (eemacs/lang/macro/with-make-recipe "PowerShell"
       :with-modes-assoc-plist
       '((:prog-modes powershell-mode :treesit-modes powershell-ts-mode))
       (eemacs/lang/macro/oset this/obj/core
         :fnm-regexp "\\.ps[dm]?1\\'")
       (eemacs/lang/macro/oset this/obj/modes
         :list this/var/prog-modes)
       (eemacs/lang/macro/oset this/obj/ids
         :list "powershell")
       (eemacs/lang/macro/oset this/obj/treesit
         :id "powershell"
         :repo-url "https://github.com/airbus-cert/tree-sitter-powershell"
         :modes (eemacs/lang/class/modes :list this/var/treesit-modes))))))

;; *** C

(eemacs/lang/macro/with-make-recipe "C"
  :with-modes-assoc-plist
  '((:prog-modes c-mode :treesit-modes c-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.c\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "c")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "c"
    :repo-url "https://github.com/tree-sitter/tree-sitter-c"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** CPP
     (eemacs/lang/macro/with-make-recipe "C++"
       :with-this-as this-cpp
       :with-modes-assoc-plist
       '((:prog-modes c++-mode :treesit-modes c++-ts-mode))
       (eemacs/lang/macro/oset this-cpp/obj/core
         :fnm-regexp "\\.cpp\\'")
       (eemacs/lang/macro/oset this-cpp/obj/modes
         :list this-cpp/var/prog-modes)
       (eemacs/lang/macro/oset this-cpp/obj/ids
         :list "cpp")
       (eemacs/lang/macro/oset this-cpp/obj/treesit
         :id "cpp"
         :repo-url "https://github.com/tree-sitter/tree-sitter-cpp"
         :modes
         (eemacs/lang/class/modes
          :list this-cpp/var/treesit-modes)))

;; **** CSHARP
     (eemacs/lang/macro/with-make-recipe "C#"
       :with-this-as this-csharp
       :with-modes-assoc-plist
       '((:prog-modes csharp-mode :treesit-modes csharp-ts-mode))
       (eemacs/lang/macro/oset this-csharp/obj/core
         :fnm-regexp "\\.cs\\'")
       (eemacs/lang/macro/oset this-csharp/obj/modes
         :list this-csharp/var/prog-modes)
       (eemacs/lang/macro/oset this-csharp/obj/ids
         :list "csharp")
       (eemacs/lang/macro/oset this-csharp/obj/treesit
         :id "c-sharp"
         :repo-url "https://github.com/tree-sitter/tree-sitter-c-sharp"
         :modes
         (eemacs/lang/class/modes
          :list this-csharp/var/treesit-modes)))

;; **** CMAKE
     (eemacs/lang/macro/with-make-recipe "CMAKE"
       :with-this-as this-cmake
       :with-modes-assoc-plist
       '((:prog-modes cmake-mode :treesit-modes cmake-ts-mode))
       (eemacs/lang/macro/oset this-cmake/obj/core
         :fnm-regexp "\\.cmake\\'")
       (eemacs/lang/macro/oset this-cmake/obj/modes
         :list this-cmake/var/prog-modes)
       (eemacs/lang/macro/oset this-cmake/obj/ids
         :list "cmake")
       (eemacs/lang/macro/oset this-cmake/obj/treesit
         :id "cmake"
         :repo-url "https://github.com/uyha/tree-sitter-cmake"
         :modes
         (eemacs/lang/class/modes
          :list this-cmake/var/treesit-modes)))
     )))

;; *** Rust
(eemacs/lang/macro/with-make-recipe "Rust"
  :with-modes-assoc-plist
  '((:prog-modes rust-mode :treesit-modes rust-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.rs\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "rust")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "rust"
    :repo-url "https://github.com/tree-sitter/tree-sitter-rust"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes)))

;; *** Go
(eemacs/lang/macro/with-make-recipe "Go"
  :with-modes-assoc-plist
  '((:prog-modes go-mode :treesit-modes go-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.go\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "go")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "go"
    :repo-url "https://github.com/tree-sitter/tree-sitter-go"
    :modes
    (eemacs/lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** Go Mod
     (eemacs/lang/macro/with-make-recipe "Go Mod"
       :with-this-as this-gomod
       :with-modes-assoc-plist
       '((:prog-modes go-mod-mode :treesit-modes go-mod-ts-mode))
       (eemacs/lang/macro/oset this-gomod/obj/core
         :fnm-regexp "go\\.mod\\'")
       (eemacs/lang/macro/oset this-gomod/obj/modes
         :list this-gomod/var/prog-modes)
       (eemacs/lang/macro/oset this-gomod/obj/ids
         :list "gomod")
       (eemacs/lang/macro/oset this-gomod/obj/treesit
         :id "gomod"
         :repo-url "https://github.com/camdencheek/tree-sitter-go-mod"
         :modes
         (eemacs/lang/class/modes
          :list this-gomod/var/treesit-modes)))
     )))

;; *** Java

(eemacs/lang/macro/with-make-recipe "Java"
  :with-modes-assoc-plist
  '((:prog-modes java-mode :treesit-modes java-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.java\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "java")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "java"
    :repo-url "https://github.com/tree-sitter/tree-sitter-java"
    :modes (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; *** PHP
(eemacs/lang/macro/with-make-recipe "PHP"
  :with-modes-assoc-plist
  '((:prog-modes php-mode :treesit-modes php-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.php\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "php")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "php"
    :repo-url "https://github.com/tree-sitter/tree-sitter-php"
    :repo-src-dir "php/src"
    :modes
    (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; *** Perl
(eemacs/lang/macro/with-make-recipe "Perl"
  :with-modes-assoc-plist
  '((:prog-modes perl-mode :treesit-modes perl-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.pl6?\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list (list "perl" "perl6")
    :probe
    (eemacs/lang/macro/define-probe
     :with-conds-pattern
     `((file-ext
        ("perl" "perl")
        ("perl" "perl6"))
       (_ . "perl"))))
  (eemacs/lang/macro/oset this/obj/treesit
    :id "perl"
    :repo-url "https://github.com/ganezdragon/tree-sitter-perl"
    :modes
    (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; *** Ruby
(eemacs/lang/macro/with-make-recipe "Ruby"
  :with-modes-assoc-plist
  '((:prog-modes ruby-mode :treesit-modes ruby-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp
    "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\
\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\
\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "ruby")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "ruby"
    :repo-url "https://github.com/tree-sitter/tree-sitter-ruby"
    :modes
    (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; *** YAML
(eemacs/lang/macro/with-make-recipe "YAML"
  :with-modes-assoc-plist
  '((:prog-modes yaml-mode :treesit-modes yaml-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.ya?ml\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "yaml")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "yaml"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
    :modes (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; *** TOML
(eemacs/lang/macro/with-make-recipe "TOML"
  :with-modes-assoc-plist
  '((:prog-modes (conf-toml-mode toml-mode) :treesit-modes toml-ts-mode))
  (eemacs/lang/macro/oset this/obj/core
    :fnm-regexp "\\.toml\\'")
  (eemacs/lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/lang/macro/oset this/obj/ids
    :list "toml")
  (eemacs/lang/macro/oset this/obj/treesit
    :id "toml"
    :repo-url "https://github.com/tree-sitter/tree-sitter-toml"
    :modes
    (eemacs/lang/class/modes :list this/var/treesit-modes)))

;; * provide
(provide 'entropy-emacs-lang)
