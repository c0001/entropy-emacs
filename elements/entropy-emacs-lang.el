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
(cl-defmacro eemacs/prog-lang/macro/oset (obj &rest slots)
  (declare (indent 1))
  (let (key val form)
    (while slots
      (setq key (pop slots) val (pop slots))
      (if (not (keywordp key)) (setq key (intern (format ":%s") (symbol-name key))))
      (when (eq key :list) (setq val `(ensure-list ,val)))
      (push `(oset ,obj ,(intern (substring (symbol-name key) 1))
                   ,val) form))
    (when form
      (cons 'progn (nreverse form)))))

(cl-defmacro eemacs/prog-lang/macro/oref (obj &rest slots)
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

(cl-defmacro eemacs/prog-lang/macro/define-general-probe
    (&rest body &key with-this-as
           &allow-other-keys)
  (let* ((body (entropy/emacs--get-def-body body 'with-safe))
         (bofarg (make-symbol "buffer-or-file"))
         (it (or with-this-as 'probe))
         (probe/var/buffer (intern (concat (symbol-name it) "/var/buffer")))
         (probe/var/file (intern (concat (symbol-name it) "/var/file")))
         (probe/var/fext (intern (concat (symbol-name it) "/var/fext")))
         (bop (make-symbol "bop")))
    `(let ((fnm (make-symbol "eemacs/prog-lang/probe-func")))
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
             (and ,probe/var/fext (string-empty-p ,probe/var/fext)
                  (setq ,probe/var/fext nil))
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
       (put fnm :is-eemacs/prog-lang/probe-func-p t)
       fnm)))

(defun eemacs/prog-lang/func//cond-match-p (cond cond-map)
  (let (extractor elt)
    (when (setq elt (alist-get cond cond-map nil nil 'equal))
      (setq
       extractor (or (plist-get elt :extractor) 'identity)
       elt       (plist-get elt :val))
      (funcall extractor elt))))
(cl-defmacro eemacs/prog-lang/macro/define-probe
    (&key
     with-this-as
     with-conds-pattern
     &allow-other-keys)
  (let* ((it (or with-this-as 'probe))
         (probe/var/buffer (intern (concat (symbol-name it) "/var/buffer")))
         ;; (probe/var/file (intern (concat (symbol-name it) "/var/file")))
         (probe/var/fext (intern (concat (symbol-name it) "/var/fext"))))
    (macroexp-let2* ignore ((cods nil))
      `(eemacs/prog-lang/macro/define-general-probe
        :with-this-as ,with-this-as
        (let ((,cods ,with-conds-pattern) extp)
          (catch :exit
            (dolist (cod ,cods)
              (entropy/emacs-setf-by-body extp
                (cond
                 ((and (eq (car cod) 'major-mode) ,probe/var/buffer)
                  (with-current-buffer ,probe/var/buffer
                    (eemacs/prog-lang/func//cond-match-p
                     major-mode (cdr cod))))
                 ((and (eq (car cod) 'file-ext) ,probe/var/fext)
                  (eemacs/prog-lang/func//cond-match-p
                   ,probe/var/fext
                   (cdr cod)))
                 ((eq (car cod) '_) (cdr cod))
                 ((eq (car cod) 'function) (funcall (cdr cod)))))
              (and extp (throw :exit nil))))
          extp)))))

;; ** classes
(defun eemacs/prog-lang/probe-func-type-p (sym)
  (or
   (and (symbolp sym) (functionp sym)
        (eq (get sym :is-eemacs/prog-lang/probe-func-p) t))))
(defclass eemacs/prog-lang/class/core ()
  ((name          :initarg :name :type string
                  :documentation
                  "Language Name standard via language server
protocol (See: https://code.visualstudio.com/docs/languages/identifiers).

For those langauge has no defined in which case, please following
programer forum convention i.e. as conventional as possible")
   (fnm-regexp    :initarg :fnm-regexp :type string)))
(defclass eemacs/prog-lang/class//list-probe ()
  ((_))
  :abstract t
  :documentation
  "The empty top abstraction of a eemacs specified language object class
for defined a list of possible value and a probe function to extract an
accelerated one for a buffer or file from unified api
`eemacs/prog-lang/class//list-probe/method/call'.

This at least, any children class inherit from this interface should
declare two slots, i.e. the :list and :probe where the value of :list
should be a list of elements, and a function defined via
`eemacs/prog-lang/macro/define-probe' for :probe.")
(defun eemacs/prog-lang/class//list-probe/method/call
    (obj &optional buffer-or-file)
  (if (or (not (eieio-object-p obj))
          (not (memq (eieio-object-class obj)
                     (eieio-class-children
                      'eemacs/prog-lang/class//list-probe))))
      nil
    (let ((l  (and (slot-boundp obj 'list)
                   (slot-value  obj 'list)))
          (pb (and (slot-boundp obj 'probe)
                   (slot-value  obj 'probe))))
      (if pb (funcall pb buffer-or-file)
        (and (consp l) (not (cdr l)) (car l))))))
(defclass eemacs/prog-lang/class/modes (eemacs/prog-lang/class//list-probe)
  ((list          :initarg :list :type (or null (satisfies consp))
                  :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/prog-lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/prog-lang/class/ids (eemacs/prog-lang/class//list-probe)
  ((list          :initarg :list :type (or null (satisfies consp))
                  :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/prog-lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/prog-lang/class/treesit ()
  ((id            :initarg :id            :type string
                  :documentation
                  "NOTE: The language id for the LANG, which should indeed grabbed from LSP
standard defs of *language identifier*
(https://microsoft.github.io/language-server-protocol/specifications/)
or if any not presented in LSP then named as history usage convention
and updating it while presents.")
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
                  :type (or eemacs/prog-lang/class//list-probe null)
                  :initform nil)
   (installable   :initarg :installable :type (or null t)
                  :initform t)
   (installer     :initarg :installer :type (or null (satisfies functionp))
                  :initform nil)))

(defun eemacs/prog-lang/class/subrecipes/pred/list-type-p (x)
  (and (listp x)
       (catch :exit
         (dolist (y x)
           (unless (eemacs/prog-lang/class/recipe-p y)
             (throw :exit nil)))
         t)))
(defclass eemacs/prog-lang/class/subrecipes (eemacs/prog-lang/class//list-probe)
  ((list :initarg :list
         :type (or
                null
                (and
                 (satisfies consp)
                 (satisfies eemacs/prog-lang/class/subrecipes/pred/list-type-p)))
         :initform nil)
   (probe         :initarg :probe :type (or (satisfies eemacs/prog-lang/probe-func-type-p) null)
                  :initform nil)))
(defclass eemacs/prog-lang/class/recipe ()
  ((core    :initarg :core    :type eemacs/prog-lang/class/core)
   (modes   :initarg :modes   :type eemacs/prog-lang/class/modes)
   (ids     :initarg :ids     :type eemacs/prog-lang/class/ids)
   (treesit :initarg :treesit :type (or eemacs/prog-lang/class/treesit null))
   (subrecipes :initarg :subrecipes
               :type (or eemacs/prog-lang/class/subrecipes
                         null)
               :initform nil)
   (parent  :initarg :parent :type (or null string))))

(defvar eemacs/prog-lang/var/recipe-alist nil)
(defun eemacs/prog-lang/func/get-recipe-modes (lang-recipe &optional type)
  (if (not (eemacs/prog-lang/class/recipe-p lang-recipe)) nil
    (cl-case type
      (treesit-modes  (eemacs/prog-lang/macro/oref lang-recipe :treesit :modes :list))
      (prog-modes     (eemacs/prog-lang/macro/oref lang-recipe :modes :list))
      (all            (append (eemacs/prog-lang/func/get-recipe-modes lang-recipe 'prog-modes)
                              (eemacs/prog-lang/func/get-recipe-modes lang-recipe 'treesit-modes)))
      (t (eemacs/prog-lang/func/get-recipe-modes lang-recipe 'prog-modes)))))
(defun eemacs/prog-lang/func/get-recipes-modes (&optional type)
  (let (rtn mds)
    (dolist (rec eemacs/prog-lang/var/recipe-alist)
      (setq mds (eemacs/prog-lang/func/get-recipe-modes (cdr rec) type))
      (and  mds (setq rtn (append rtn mds))))
    rtn))
(defun eemacs/prog-lang/func/bof/lang-recipe (buffer-or-file)
  "Get the `eemaca/lang/class/recipe' for BUFFER-OR-FILE, the return is a
cons of LANG-NAME and LANG-RECIPE, or nil that not found."
  (let (lang-rec lang-fnm-regexp rtn)
    (when (or (and (stringp buffer-or-file)
                   (file-exists-p buffer-or-file))
              (and (bufferp buffer-or-file)
                   (buffer-file-name buffer-or-file)))
      (entropy/emacs-setf-by-body rtn
        (funcall
         (eemacs/prog-lang/macro/define-general-probe
          (catch :exit
            (dolist (rec eemacs/prog-lang/var/recipe-alist)
              (setq lang-rec (cdr rec))
              (when (and probe/var/fext probe/var/file
                         (setq lang-fnm-regexp
                               (eemacs/prog-lang/macro/oref lang-rec
                                 :core :fnm-regexp))
                         (string-match-p lang-fnm-regexp probe/var/file))
                (throw :exit rec)))
            nil))
         buffer-or-file)))
    ;; NOTE: we preferred the fname regexp match, then use mode
    ;; matching, since a regular named file may be opened with wrong
    ;; `prog-mode'.
    (or rtn
        (and (bufferp buffer-or-file)
             (funcall
              (eemacs/prog-lang/macro/define-general-probe
               (catch :exit
                 (dolist (rec eemacs/prog-lang/var/recipe-alist)
                   (setq lang-rec (cdr rec))
                   (when (memq (buffer-local-value 'major-mode probe/var/buffer)
                               (eemacs/prog-lang/func/get-recipe-modes lang-rec 'all))
                     (throw :exit rec)))
                 nil))
              buffer-or-file)))))

(defun eemacs/prog-lang/func/bof/lang-name (buffer-or-file)
  (when-let* ((rec (eemacs/prog-lang/func/bof/lang-recipe buffer-or-file)))
    (car rec)))
(defun eemacs/prog-lang/func/bof/treesit-id (buffer-or-file)
  (when-let* ((rec (eemacs/prog-lang/func/bof/lang-recipe buffer-or-file)))
    (eemacs/prog-lang/macro/oref (cdr rec) :treesit :id)))
(defun eemacs/prog-lang/func/bof/modes (buffer-or-file &optional type)
  (let ((rec (eemacs/prog-lang/func/bof/lang-recipe buffer-or-file)))
    (when rec
      (setq rec (cdr rec))
      (eemacs/prog-lang/class//list-probe/method/call
       (cl-case type
         (treesit-modes (eemacs/prog-lang/macro/oref rec :treesit :modes))
         (t (eemacs/prog-lang/macro/oref rec :modes)))
       buffer-or-file))))
(defun eemacs/prog-lang/func/mode/treesit-mode-p (mode)
  (let (;; lang-name
        lang-rec lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/prog-lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-ts-modes
              (eemacs/prog-lang/macro/oref lang-rec
                :treesit :modes :list))
        (if (memq mode lang-ts-modes) (throw :exit t)))
      nil)))
(defun eemacs/prog-lang/func/mode/prog-mode-p (mode)
  (let (;; lang-name
        lang-rec lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/prog-lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/prog-lang/macro/oref lang-rec :modes :list))
        (if (memq mode lang-modes) (throw :exit t))
        (setq lang-ts-modes
              (eemacs/prog-lang/macro/oref lang-rec
                :treesit :modes :list))
        (if (memq mode lang-ts-modes) (throw :exit nil)))
      ;; FIXME: fine-tune default judgement
      (and (fboundp mode)
           (string-match-p "-mode\\'" (symbol-name mode))))))
(defun eemacs/prog-lang/func/mode/prog-modes (mode &optional buffer-or-file)
  (let (;; lang-name
        lang-rec lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/prog-lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/prog-lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/prog-lang/macro/oref lang-rec
                :treesit :modes :list))
        (when (memq mode (append lang-modes lang-ts-modes))
          (throw :exit (if buffer-or-file
                           (eemacs/prog-lang/class//list-probe/method/call
                            (eemacs/prog-lang/macro/oref lang-rec :modes) buffer-or-file)
                         lang-modes))))
      nil)))
(defun eemacs/prog-lang/func/mode/treesit-modes (mode &optional buffer-or-file)
  (let (;; lang-name
        lang-rec lang-ts-obj lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/prog-lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/prog-lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/prog-lang/macro/oref
                  (setq lang-ts-obj (eemacs/prog-lang/macro/oref lang-rec :treesit))
                :modes :list))
        (when (memq mode lang-ts-modes)
          (throw :exit (if buffer-or-file
                           (eemacs/prog-lang/class//list-probe/method/call
                            (eemacs/prog-lang/macro/oref lang-ts-obj :modes)
                            buffer-or-file)
                         lang-ts-modes)))
        (when (memq mode lang-modes)
          (and (not lang-ts-obj) (throw :exit nil))
          (throw :exit
                 (if buffer-or-file
                     (eemacs/prog-lang/class//list-probe/method/call
                      (eemacs/prog-lang/macro/oref lang-ts-obj :modes)
                      buffer-or-file)
                   lang-ts-modes))))
      nil)))
(defun eemacs/prog-lang/func/mode/treesit-id (mode)
  (let (;; lang-name
        lang-rec lang-ts-obj lang-modes lang-ts-modes)
    (catch :exit
      (dolist (rec eemacs/prog-lang/var/recipe-alist)
        (setq ;; lang-name (car rec)
              lang-rec (cdr rec))
        (setq lang-modes (eemacs/prog-lang/macro/oref lang-rec :modes :list)
              lang-ts-modes
              (eemacs/prog-lang/macro/oref
                  (setq lang-ts-obj (eemacs/prog-lang/macro/oref lang-rec :treesit))
                :modes :list))
        (when (and lang-ts-obj (memq mode (append lang-modes lang-ts-modes)))
          (throw :exit
                 (eemacs/prog-lang/macro/oref lang-ts-obj :id))))
      nil)))

(entropy/emacs-defconst/only-allow/local
  eemacs/prog-lang/var//treesit-parser-install-ok-exists nil)
(entropy/emacs-defconst/only-allow/local
  eemacs/prog-lang/var//lang-recpe-parent nil)
(cl-defmacro eemacs/prog-lang/macro/with-make-recipe
    (name &rest slots &key with-this-as with-modes-assoc-plist &allow-other-keys)
  "Define a `eemacs/prog-lang/class/recipe' use let bounded sets of interned
symbols THIS-BINDING prefixed by WITH-THIS-AS (defaults to `this') that
is a explicit symbol name.

NAME should indeed grabbed from LSP standard defs of
*language* (https://microsoft.github.io/language-server-protocol/specifications/)
or if any not presented in LSP then named as history usage convention
and updating it while presents.

WITH-MODES-ASSOC-PLIST used as meaning as `eemacs/prog-lang/assoc-plist/func/match'.
"
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
      `(let* ((,this (eemacs/prog-lang/class/recipe
                      :core    (make-instance 'eemacs/prog-lang/class/core)
                      :modes   (make-instance 'eemacs/prog-lang/class/modes)
                      :ids     (make-instance 'eemacs/prog-lang/class/ids)
                      :treesit (make-instance 'eemacs/prog-lang/class/treesit)
                      :subrecipes (make-instance 'eemacs/prog-lang/class/subrecipes)))
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
         (eemacs/prog-lang/macro/oset ,this/obj/core :name ,name)
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
         (eemacs/prog-lang/macro/oset ,this :parent eemacs/prog-lang/var//lang-recpe-parent)
         (let ((eemacs/prog-lang/var//lang-recpe-parent ,name)) ,@body)
         (if (assoc ,name eemacs/prog-lang/var/recipe-alist 'string=)
             (setf (alist-get ,name eemacs/prog-lang/var/recipe-alist
                              nil nil 'string=)
                   ,this)
           (push (cons ,name ,this) eemacs/prog-lang/var/recipe-alist))
         (let
             ((func-sym (intern (format "eemacs/prog-lang/interact/install/treesit-parser/%s"
                                        (replace-regexp-in-string "[    ]+" "-" ,name))))
              core-func)
           (entropy/emacs-setf-by-body core-func
             (lambda ()
               (entropy/emacs-when-let*-firstn 4
                   ((trobj (oref ,this treesit))
                    (id (eemacs/prog-lang/macro/oref trobj :id))
                    (ids (ensure-list id))
                    ((or (eemacs/prog-lang/macro/oref trobj :installable)
                         (prog1 nil
                           (display-warning
                            'treesit
                            (format "The treesit parser '%s' isn't installable \
since its source currently not support eemacs auto installation mechanism! SKIP!!!"
                                    id)))))
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
                       (if eemacs/prog-lang/var//treesit-parser-install-ok-exists
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
           (eemacs/prog-lang/macro/oset ,this/obj/treesit :installer func-sym))
         ,this))))

(defun eemacs/prog-lang/func/install/all-treesit-parsers nil
  (when (file-directory-p entropy/emacs-treesit-libs-default-load-path)
    (when-let* ((files (directory-files entropy/emacs-treesit-libs-default-load-path
                                        t "\\`libtree-sitter-")))
      (dolist (el files) (delete-file el))))
  (let ((eemacs/prog-lang/var//treesit-parser-install-ok-exists t))
    (dolist (rec eemacs/prog-lang/var/recipe-alist)
      (when-let* ((func
                   (eemacs/prog-lang/macro/oref (cdr rec)
                     :treesit :installer))
                  ;; NOTE: no dups invocation since parent auto
                  ;; recursively install subrecipes
                  ((not (eemacs/prog-lang/macro/oref (cdr rec) :parent))))
        (entropy/emacs-message-simple-progress-message
            (format "Install treesit parsers for lang %s" (car rec))
          (funcall func))))))

(defun eemacs/prog-lang/assoc-plist/func/match
    (assoc-plists key-match match-member key-against)
  "Find value based on key KEY-MATCH according to the wanted from an
ASSOC-PLIST from list of thus in ASSOC-PLISTS.

ASSOC-PLIST is a plist whose each key's value treated as a list as
default, `ensure-list' it if not thus. In general any key's value of it
are considerred as equal rights, in which case if MATCH-MEMBER is a
member of KEY-MATCH's value, then the key KEY-AGAINST's value in this
ASSOC-PLIST is returned, and ensured as a list.

If matches occurrence, returned immediately, thus the order of
ASSOC-PLISTS is respected."
  (let (mval aval rtn)
    (catch :exit
      (dolist (el assoc-plists)
        (when (and (setq mval (ensure-list (plist-get el key-match)))
                   (setq aval (plist-get el key-against))
                   (member match-member mval))
          (throw :exit (setq rtn (ensure-list aval))))))
    rtn))

;; ** recipes

;; NOTE: metas of recipes can be grabbed from package `treesit-auto'.

;; *** Common Lisp

(eemacs/prog-lang/macro/with-make-recipe "Common Lisp"
  :with-modes-assoc-plist
  '((:prog-modes common-lisp-mode :treesit-modes commonlisp-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.cl\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "commonlisp")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "commonlisp"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** Clojure
     (eemacs/prog-lang/macro/with-make-recipe "Clojure"
       :with-modes-assoc-plist
       '((:prog-modes
          (clojure-mode
           clojurescript-mode clojurec-mode
           clojuredart-mode jank-mode joker-mode)
          :treesit-modes clojure-ts-mode))
       (eemacs/prog-lang/macro/oset this/obj/core
         :fnm-regexp "\\.cljc?s?d?\\'")
       (eemacs/prog-lang/macro/oset this/obj/modes
         :list this/var/prog-modes
         :probe
         (eemacs/prog-lang/macro/define-probe
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
       (eemacs/prog-lang/macro/oset this/obj/ids :list "clojure")
       (eemacs/prog-lang/macro/oset this/obj/treesit
         :id
         (car-safe (oref this/obj/ids list))
         :repo-url
         "https://github.com/sogaiu/tree-sitter-clojure"
         :modes
         (eemacs/prog-lang/class/modes
          :list this/var/treesit-modes
          :probe
          (eemacs/prog-lang/macro/define-probe
           :with-conds-pattern
           '((_ . clojure-ts-mode)))))))))

;; *** Python

(eemacs/prog-lang/macro/with-make-recipe "Python"
  :with-modes-assoc-plist
  '((:prog-modes python-mode :treesit-modes python-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.py[iw]?\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "python")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "python"
    :repo-url "https://github.com/tree-sitter/tree-sitter-python"
    :modes
    (eemacs/prog-lang/class/modes
     :list this/var/treesit-modes)))

;; *** Html
(eemacs/prog-lang/macro/with-make-recipe "HTML"
  :with-modes-assoc-plist
  '((:prog-modes
     (html-mode web-mode mhtml-mode sgml-mode)
     :treesit-modes html-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.html\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "html")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "html"
    :repo-url "https://github.com/tree-sitter/tree-sitter-html"
    :modes
    (eemacs/prog-lang/class/modes :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** CSS
     (eemacs/prog-lang/macro/with-make-recipe "CSS"
       :with-this-as this-css
       :with-modes-assoc-plist
       '((:prog-modes css-mode :treesit-modes css-ts-mode))
       (eemacs/prog-lang/macro/oset this-css/obj/core
         :fnm-regexp "\\.css\\'")
       (eemacs/prog-lang/macro/oset this-css/obj/modes
         :list this-css/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-css/obj/ids
         :list "css")
       (eemacs/prog-lang/macro/oset this-css/obj/treesit
         :id "css"
         :repo-url "https://github.com/tree-sitter/tree-sitter-css"
         :modes (eemacs/prog-lang/class/modes :list this-css/var/treesit-modes)))

;; **** XML
     (eemacs/prog-lang/macro/with-make-recipe "XML"
       :with-this-as this-xml
       :with-modes-assoc-plist
       '((:prog-modes (xml-mode nxml-mode)))
       (eemacs/prog-lang/macro/oset this-xml/obj/core
         :fnm-regexp "\\.xml\\'")
       (eemacs/prog-lang/macro/oset this-xml/obj/modes
         :list this-xml/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-xml/obj/ids
         :list "xml"))
     )))

;; *** Javascript
(eemacs/prog-lang/macro/with-make-recipe "JavaScript"
  :with-modes-assoc-plist
  `((:prog-modes
     (js-mode javascript-mode js2-mode)
     :treesit-modes
     js-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.js\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes
    :probe
    (eemacs/prog-lang/macro/define-probe
     :with-conds-pattern
     `((function
        .
        ,(lambda nil
           (when probe/var/buffer
             (car-safe (memq (buffer-local-value 'major-mode probe/var/buffer)
                             this/var/prog-modes)))))
       (_ . js-mode))))
  (eemacs/prog-lang/macro/oset this/obj/ids :list "javascript")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id (car-safe (oref this/obj/ids list))
    :repo-url "https://github.com/tree-sitter/tree-sitter-javascript"
    :repo-revision "master"
    :repo-src-dir  "src"
    :modes
    (eemacs/prog-lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** JSON
     (eemacs/prog-lang/macro/with-make-recipe "JSON"
       :with-this-as this-json
       :with-modes-assoc-plist
       '((:prog-modes (json-mode js-json-mode) :treesit-modes json-ts-mode))
       (eemacs/prog-lang/macro/oset this-json/obj/core
         :fnm-regexp "\\.json\\'")
       (eemacs/prog-lang/macro/oset this-json/obj/modes
         :list this-json/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-json/obj/ids
         :list "json")
       (eemacs/prog-lang/macro/oset this-json/obj/treesit
         :id "json"
         :repo-url "https://github.com/tree-sitter/tree-sitter-json"
         :modes (eemacs/prog-lang/class/modes :list this-json/var/treesit-modes)))

;; **** JSDOC
     (eemacs/prog-lang/macro/with-make-recipe "JSDoc"
       :with-this-as this-jsdoc
       (eemacs/prog-lang/macro/oset this-jsdoc/obj/treesit
         :id "jsdoc"
         :repo-url "https://github.com/tree-sitter/tree-sitter-jsdoc"))

;; **** Typescript
     (eemacs/prog-lang/macro/with-make-recipe "TypeScript"
       :with-this-as this-ts
       :with-modes-assoc-plist
       '((:prog-modes typescript-mode :treesit-modes typescript-ts-mode))
       (eemacs/prog-lang/macro/oset this-ts/obj/core
         :fnm-regexp "\\.ts\\'")
       (eemacs/prog-lang/macro/oset this-ts/obj/modes
         :list this-ts/var/prog-modes
         :probe
         (eemacs/prog-lang/macro/define-probe
          :with-conds-pattern
          `((function
             .
             ,(lambda nil
                (when probe/var/buffer
                  (car-safe (memq (buffer-local-value 'major-mode probe/var/buffer)
                                  this-ts/var/prog-modes))))))))
       (eemacs/prog-lang/macro/oset this-ts/obj/ids :list "typescript")
       (eemacs/prog-lang/macro/oset this-ts/obj/treesit
         :id (car-safe (oref this-ts/obj/ids list))
         :repo-url "https://github.com/tree-sitter/tree-sitter-typescript"
         :repo-revision "master"
         :repo-src-dir "typescript/src"
         :modes
         (eemacs/prog-lang/class/modes
          :list this-ts/var/treesit-modes))
       (eemacs/prog-lang/macro/oset this-ts/obj/subrecipes
         :list
         (list
;; **** TypeScript JSX
          (eemacs/prog-lang/macro/with-make-recipe "TypeScript JSX"
            :with-this-as this-jsx
            :with-modes-assoc-plist
            `((:prog-modes typescript-tsx-mode :treesit-modes tsx-ts-mode))
            (eemacs/prog-lang/macro/oset this-jsx/obj/core
              :fnm-regexp "\\.tsx\\'")
            (eemacs/prog-lang/macro/oset this-jsx/obj/modes
              :list this-jsx/var/prog-modes)
            (eemacs/prog-lang/macro/oset this-jsx/obj/ids
              :list "tsx")
            (eemacs/prog-lang/macro/oset this-jsx/obj/treesit
              :id (car-safe (oref this-jsx/obj/ids list))
              :repo-url "https://github.com/tree-sitter/tree-sitter-typescript"
              :repo-revision "master"
              :repo-src-dir "tsx/src"
              :modes
              (eemacs/prog-lang/class/modes :list this-jsx/var/treesit-modes))))))

;; **** Vue
     (eemacs/prog-lang/macro/with-make-recipe "Vue"
       :with-this-as this-vue
       :with-modes-assoc-plist
       `((:prog-modes vue-mode :treesit-modes vue-ts-mode))
       (eemacs/prog-lang/macro/oset this-vue/obj/core
         :fnm-regexp "\\.vue\\'")
       (eemacs/prog-lang/macro/oset this-vue/obj/modes
         :list this-vue/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-vue/obj/ids
         :list "vue")
       (eemacs/prog-lang/macro/oset this-vue/obj/treesit
         :id (car-safe (oref this-vue/obj/ids list))
         :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-vue"
         :modes
         (eemacs/prog-lang/class/modes :list this-vue/var/treesit-modes)))
     )))

;; *** ShellScript

(eemacs/prog-lang/macro/with-make-recipe "Shell Script"
  :with-modes-assoc-plist
  '((:prog-modes sh-mode :treesit-modes bash-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp (rx "." (or "sh" "bash" "bashrc" "bash_profile" "fish" "zsh") line-end))
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list '("sh" "bash" "zsh" "fish")
    :probe
    (eemacs/prog-lang/macro/define-probe
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
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "bash"
    :repo-url "https://github.com/tree-sitter/tree-sitter-bash"
    :modes
    (eemacs/prog-lang/class/modes
     :list '(bash-ts-mode)))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** PowerShell
     (eemacs/prog-lang/macro/with-make-recipe "PowerShell"
       :with-modes-assoc-plist
       '((:prog-modes powershell-mode :treesit-modes powershell-ts-mode))
       (eemacs/prog-lang/macro/oset this/obj/core
         :fnm-regexp "\\.ps[dm]?1\\'")
       (eemacs/prog-lang/macro/oset this/obj/modes
         :list this/var/prog-modes)
       (eemacs/prog-lang/macro/oset this/obj/ids
         :list "powershell")
       (eemacs/prog-lang/macro/oset this/obj/treesit
         :id "powershell"
         :repo-url "https://github.com/airbus-cert/tree-sitter-powershell"
         :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; **** AWK
     (eemacs/prog-lang/macro/with-make-recipe "AWK"
       :with-modes-assoc-plist
       '((:prog-modes awk-mode :treesit-modes awk-ts-mode))
       (eemacs/prog-lang/macro/oset this/obj/core
         :fnm-regexp "\\.awk\\'")
       (eemacs/prog-lang/macro/oset this/obj/modes
         :list this/var/prog-modes)
       (eemacs/prog-lang/macro/oset this/obj/ids
         :list "awk")
       (eemacs/prog-lang/macro/oset this/obj/treesit
         :id "awk"
         :repo-url "https://github.com/Beaglefoot/tree-sitter-awk"
         :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

     )))

;; *** C

(eemacs/prog-lang/macro/with-make-recipe "C"
  :with-modes-assoc-plist
  '((:prog-modes c-mode :treesit-modes c-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.c\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "c")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "c"
    :repo-url "https://github.com/tree-sitter/tree-sitter-c"
    :modes
    (eemacs/prog-lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** CPP
     (eemacs/prog-lang/macro/with-make-recipe "C++"
       :with-this-as this-cpp
       :with-modes-assoc-plist
       '((:prog-modes c++-mode :treesit-modes c++-ts-mode))
       (eemacs/prog-lang/macro/oset this-cpp/obj/core
         :fnm-regexp "\\.cpp\\'")
       (eemacs/prog-lang/macro/oset this-cpp/obj/modes
         :list this-cpp/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-cpp/obj/ids
         :list "cpp")
       (eemacs/prog-lang/macro/oset this-cpp/obj/treesit
         :id "cpp"
         :repo-url "https://github.com/tree-sitter/tree-sitter-cpp"
         :modes
         (eemacs/prog-lang/class/modes
          :list this-cpp/var/treesit-modes)))

;; **** CSHARP
     (eemacs/prog-lang/macro/with-make-recipe "C#"
       :with-this-as this-csharp
       :with-modes-assoc-plist
       '((:prog-modes csharp-mode :treesit-modes csharp-ts-mode))
       (eemacs/prog-lang/macro/oset this-csharp/obj/core
         :fnm-regexp "\\.cs\\'")
       (eemacs/prog-lang/macro/oset this-csharp/obj/modes
         :list this-csharp/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-csharp/obj/ids
         :list "csharp")
       (eemacs/prog-lang/macro/oset this-csharp/obj/treesit
         :id "c-sharp"
         :repo-url "https://github.com/tree-sitter/tree-sitter-c-sharp"
         :modes
         (eemacs/prog-lang/class/modes
          :list this-csharp/var/treesit-modes)))

;; **** CMAKE
     (eemacs/prog-lang/macro/with-make-recipe "CMAKE"
       :with-this-as this-cmake
       :with-modes-assoc-plist
       '((:prog-modes cmake-mode :treesit-modes cmake-ts-mode))
       (eemacs/prog-lang/macro/oset this-cmake/obj/core
         :fnm-regexp "\\.cmake\\'")
       (eemacs/prog-lang/macro/oset this-cmake/obj/modes
         :list this-cmake/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-cmake/obj/ids
         :list "cmake")
       (eemacs/prog-lang/macro/oset this-cmake/obj/treesit
         :id "cmake"
         :repo-url "https://github.com/uyha/tree-sitter-cmake"
         :modes
         (eemacs/prog-lang/class/modes
          :list this-cmake/var/treesit-modes)))
     )))

;; *** Rust
(eemacs/prog-lang/macro/with-make-recipe "Rust"
  :with-modes-assoc-plist
  '((:prog-modes rust-mode :treesit-modes rust-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.rs\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "rust")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "rust"
    :repo-url "https://github.com/tree-sitter/tree-sitter-rust"
    :modes
    (eemacs/prog-lang/class/modes
     :list this/var/treesit-modes)))

;; *** Go
(eemacs/prog-lang/macro/with-make-recipe "Go"
  :with-modes-assoc-plist
  '((:prog-modes go-mode :treesit-modes go-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.go\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "go")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "go"
    :repo-url "https://github.com/tree-sitter/tree-sitter-go"
    :modes
    (eemacs/prog-lang/class/modes
     :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** Go Mod
     (eemacs/prog-lang/macro/with-make-recipe "Go Mod"
       :with-this-as this-gomod
       :with-modes-assoc-plist
       '((:prog-modes go-mod-mode :treesit-modes go-mod-ts-mode))
       (eemacs/prog-lang/macro/oset this-gomod/obj/core
         :fnm-regexp "go\\.mod\\'")
       (eemacs/prog-lang/macro/oset this-gomod/obj/modes
         :list this-gomod/var/prog-modes)
       (eemacs/prog-lang/macro/oset this-gomod/obj/ids
         :list "gomod")
       (eemacs/prog-lang/macro/oset this-gomod/obj/treesit
         :id "gomod"
         :repo-url "https://github.com/camdencheek/tree-sitter-go-mod"
         :modes
         (eemacs/prog-lang/class/modes
          :list this-gomod/var/treesit-modes)))
     )))

;; *** Java

(eemacs/prog-lang/macro/with-make-recipe "Java"
  :with-modes-assoc-plist
  '((:prog-modes java-mode :treesit-modes java-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.java\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "java")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "java"
    :repo-url "https://github.com/tree-sitter/tree-sitter-java"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes))
  (eemacs/prog-lang/macro/oset this/obj/subrecipes
    :list
    (list
;; **** Kotlin
     (eemacs/prog-lang/macro/with-make-recipe "Kotlin"
       :with-modes-assoc-plist
       '((:prog-modes kotlin-mode :treesit-modes kotlin-ts-mode))
       (eemacs/prog-lang/macro/oset this/obj/core
         :fnm-regexp "\\.kts?\\'")
       (eemacs/prog-lang/macro/oset this/obj/modes
         :list this/var/prog-modes)
       (eemacs/prog-lang/macro/oset this/obj/ids
         :list "kotlin")
       (eemacs/prog-lang/macro/oset this/obj/treesit
         :id "kotlin"
         :repo-url "https://github.com/fwcd/tree-sitter-kotlin"
         :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; **** Dart
     (eemacs/prog-lang/macro/with-make-recipe "Dart"
       :with-modes-assoc-plist
       '((:prog-modes dart-mode :treesit-modes dart-ts-mode))
       (eemacs/prog-lang/macro/oset this/obj/core
         :fnm-regexp "\\.dart\\'")
       (eemacs/prog-lang/macro/oset this/obj/modes
         :list this/var/prog-modes)
       (eemacs/prog-lang/macro/oset this/obj/ids
         :list "dart")
       (eemacs/prog-lang/macro/oset this/obj/treesit
         :id "dart"
         :repo-url "https://github.com/ast-grep/tree-sitter-dart"
         :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))
     )))

;; *** PHP
(eemacs/prog-lang/macro/with-make-recipe "PHP"
  :with-modes-assoc-plist
  '((:prog-modes php-mode :treesit-modes php-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.php\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "php")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "php"
    :repo-url "https://github.com/tree-sitter/tree-sitter-php"
    :repo-src-dir "php/src"
    :modes
    (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Perl
(eemacs/prog-lang/macro/with-make-recipe "Perl"
  :with-modes-assoc-plist
  '((:prog-modes perl-mode :treesit-modes perl-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.pl6?\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list (list "perl" "perl6")
    :probe
    (eemacs/prog-lang/macro/define-probe
     :with-conds-pattern
     `((file-ext
        ("perl" "perl")
        ("perl" "perl6"))
       (_ . "perl"))))
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "perl"
    :repo-url "https://github.com/ganezdragon/tree-sitter-perl"
    :modes
    (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Ruby
(eemacs/prog-lang/macro/with-make-recipe "Ruby"
  :with-modes-assoc-plist
  '((:prog-modes ruby-mode :treesit-modes ruby-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp
    "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\
\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\
\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "ruby")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "ruby"
    :repo-url "https://github.com/tree-sitter/tree-sitter-ruby"
    :modes
    (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Lua
(eemacs/prog-lang/macro/with-make-recipe "Lua"
  :with-modes-assoc-plist
  '((:prog-modes lua-mode :treesit-modes lua-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.lua\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "lua")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "lua"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** YAML
(eemacs/prog-lang/macro/with-make-recipe "YAML"
  :with-modes-assoc-plist
  '((:prog-modes yaml-mode :treesit-modes yaml-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.ya?ml\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "yaml")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "yaml"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** TOML
(eemacs/prog-lang/macro/with-make-recipe "TOML"
  :with-modes-assoc-plist
  '((:prog-modes (conf-toml-mode toml-mode) :treesit-modes toml-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.toml\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "toml")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "toml"
    :repo-url "https://github.com/tree-sitter/tree-sitter-toml"
    :modes
    (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Dockerfile

(eemacs/prog-lang/macro/with-make-recipe "Dockerfile"
  :with-modes-assoc-plist
  '((:prog-modes dockerfile-mode :treesit-modes dockerfile-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "dockerfile")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "dockerfile"
    :repo-url "https://github.com/camdencheek/tree-sitter-dockerfile"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** SQL
(eemacs/prog-lang/macro/with-make-recipe "SQL"
  :with-modes-assoc-plist
  '((:prog-modes sql-mode :treesit-modes sql-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.sql\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "sql")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "sql"
    :repo-url "https://github.com/DerekStride/tree-sitter-sql"
    :repo-revision "gh-pages"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Org
(eemacs/prog-lang/macro/with-make-recipe "Org"
  :with-modes-assoc-plist
  '((:prog-modes org-mode :treesit-modes org-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.org\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "org")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "org"
    :repo-url "https://github.com/milisims/tree-sitter-org"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** LaTeX
(eemacs/prog-lang/macro/with-make-recipe "LaTeX"
  :with-modes-assoc-plist
  '((:prog-modes latex-mode :treesit-modes latex-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.tex\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "latex")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "latex"
    ;; FIXME: current latex treesit soure does not include a parser.c
    ;; in its src dir and no plan to do so, thus the compiling is
    ;; always failed, see:
    ;; https://github.com/latex-lsp/tree-sitter-latex/pull/168 &&
    ;; https://github.com/latex-lsp/tree-sitter-latex/issues/172
    :repo-url "https://github.com/latex-lsp/tree-sitter-latex"
    :installable
    ;; FIXME: due to above issue, the emacs-29 treesit.el internal
    ;; installation mechanism does error instead of warning while such
    ;; case failing occurred.
    nil
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Makefile
(eemacs/prog-lang/macro/with-make-recipe "Makefile"
  :with-modes-assoc-plist
  '((:prog-modes makefile-mode :treesit-modes makefile-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\([Mm]akefile\\|.*\\.\\(mk\\|make\\)\\)\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "makefile")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "make"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-make"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; *** Markdown
(eemacs/prog-lang/macro/with-make-recipe "Markdown"
  :with-modes-assoc-plist
  '((:prog-modes (poly-markdown-mode markdown-mode) :treesit-modes markdown-ts-mode))
  (eemacs/prog-lang/macro/oset this/obj/core
    :fnm-regexp "\\.md\\'")
  (eemacs/prog-lang/macro/oset this/obj/modes
    :list this/var/prog-modes)
  (eemacs/prog-lang/macro/oset this/obj/ids
    :list "markdown")
  (eemacs/prog-lang/macro/oset this/obj/treesit
    :id "markdown"
    :repo-url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :repo-src-dir "tree-sitter-markdown/src"
    :modes (eemacs/prog-lang/class/modes :list this/var/treesit-modes)))

;; * provide
(provide 'entropy-emacs-lang)
