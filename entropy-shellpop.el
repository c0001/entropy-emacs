;;; entropy-shellpop.el --- popup shell buffer for transient
;;
;; * Copyright (C) 20190829  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;; 
;; * Commentary:
;;
;; The shell popuped feature provision what seems like the =vscode
;; shellpopup aspect=.
;;
;; Allow =multi-shell-type= coexistence, that the mode-type of
;; =eshell-mode=, =shell-mode=, =term-mode= can be popuped together
;; with keybinding customization.
;;
;; Allow =multi-shell-buffer= coexistence independently, with
;; completion query prompt manangement.
;;
;; This package was inspired by [[http://github.com/kyagi/shell-pop-el][shell-pop-el]], but built based on
;; purely fundamental, for optimizing features' detailes and
;; restructed the popup feature rely on [[https://github.com/wasamasa/shackle][shackle]].
;;
;; * Configuration:
;;
;; Just cloning this repo under the path sepcified on your wish, and
;; added it to your ~load-path~, using ~require~ or ~use-package~ to
;; manage the configuration for this by calling the main function
;; ~entropy/shellpop-start~. Traditionally minor snippet as:
;;
;; #+BEGIN_SRC elisp
;;   (require 'entropy-shellpop)
;;   (entropy/shellpop-start)
;; #+END_SRC
;;
;; The internal builtin shell popup types are:
;;
;; - for eshell: <f9>
;; - for ansi-term: <f10>
;;
;; You may customize variable =entropy/shellpop-pop-types= for more
;; specification, see its doc-string for more.
;; 
;; * Code:
;; 
;; ** require
(require 'cl-lib)
(require 'shackle)
(require 'entropy-common-library)

;; ** defcustom
(defgroup entropy/shellpop-customized-group nil
  "entropy-shellpop customized variable group.")

(defcustom entropy/shellpop-pop-types
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
     ((eshell))))
  "Shell pop types defination.

It's a list of which each element is plist structed form called
SHELLPOP-TYPE of `entropy-shellpop'.

Slot description:

1) :type-name

   Uniquely shellpop-type name traversing
   `entropy/shellpop-pop-types', it will be the name component for
   the generated FUNC name, SHELLPOP-TYPE-REGISTER key, and also
   the popuped shell BUFFER-NAME.

2) :size

   The popuped shell-buffer window size, it's the float number
   between 0 to 1.

3) :align

   Popuped position specification, a symbol, valid are:

     'below 'above 'right 'left and t means 'default'.

4) :bind

   The keybinding for this shellpop-type, it's the string
   transfered to function `kbd'.

5) :type-body

   The list of form to enable shell entity, for example:
   '((eshell))' or '((ansi-term \"/bin/bash\"))'

   All the form must host within the top parentheses that all of
   them will apply into the caller body slot."
  
  :type 'sexp
  :group 'entropy/shellpop-customized-group)

;; ** defvar

(defvar entropy/shellpop--type-register nil)

;; ** library
;; *** cdw functions
(defmacro entropy/shellpop--cd-to-cwd-with-judge (path-given &rest body)
  `(unless (equal (expand-file-name default-directory)
                  (expand-file-name ,path-given))
     ,@body))

(defun entropy/shellpop--cd-to-cwd-eshell (cwd)
  (if (eshell-process-interact 'process-live-p)
      (message "Won't change CWD because of running process.")
    (entropy/shellpop--cd-to-cwd-with-judge
     cwd
     (setq-local default-directory cwd)
     (eshell-reset))))

(defun entropy/shellpop--cd-to-cwd-shell (cwd)
  (entropy/shellpop--cd-to-cwd-with-judge
   cwd
   (goto-char (point-max))
   (comint-kill-input)
   (insert (concat "cd " (shell-quote-argument cwd)))
   (let ((comint-process-echoes t))
     (comint-send-input))
   (recenter 0)))

(defun entropy/shellpop--cd-to-cwd-term (cwd)
  (entropy/shellpop--cd-to-cwd-with-judge
   cwd
   (term-send-raw-string (concat "cd " (shell-quote-argument cwd) "\n"))
   (term-send-raw-string "\C-l")))

(defun entropy/shellpop--cd-to-cwd (cwd buff)
  (with-current-buffer buff
    (let ((abspath (expand-file-name cwd)))
      (cond ((eq major-mode 'eshell-mode)
             (entropy/shellpop--cd-to-cwd-eshell abspath))
            ((eq major-mode 'shell-mode)
             (entropy/shellpop--cd-to-cwd-shell abspath))
            ((eq major-mode 'term-mode)
             (entropy/shellpop--cd-to-cwd-term abspath))
            (t
             (message "Shell type not supported for 'entropy-shellpop' to CDW"))))))


;; *** shellpop type name generator
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

(defun entropy/shellpop--parse-buffer-name-type (buffer-name)
  (let* ((type-names (mapcar (lambda (x) (car x)) entropy/shellpop--type-register))
         (type-name-regexs (mapcar (lambda (x)
                                     (cons x (entropy/shellpop--gen-buffn-regexp x)))
                                   type-names))
         rtn)
    (catch :exit
      (dolist (regex type-name-regexs)
        (when (string-match (cdr regex) buffer-name)
          (setq rtn
                (cons (car regex)
                      (string-to-number
                       (match-string 1 buffer-name))))
          (throw :exit nil))))
    rtn))

;; *** buffer bunches parses
(defun entropy/shellpop--get-type-buffer-indexs (shellpop-type-name &optional buffn-list-rtn)
  (let* ((buffns (mapcar (lambda (buff) (buffer-name buff)) (buffer-list)))
         (buffn-regexp (entropy/shellpop--gen-buffn-regexp shellpop-type-name))
         (index-list '())
         (buffn-list '()))
    (dolist (buffn buffns)
      (when (string-match buffn-regexp buffn)
        (push (string-to-number (match-string 1 buffn))
              index-list)
        (when buffn-list-rtn
          (push buffn buffn-list))))
    (if buffn-list-rtn
        (list :indexs index-list :buffns buffn-list)
      index-list)))

(defun entropy/shellpop--get-type-free-indexs (index-list)
  (cl-loop for slot from 0 to (apply 'max index-list)
           when (not (member slot index-list))
           collect slot))

;; *** shellpop type entity object generator
(defun entropy/shellpop--buffer-active-p (buffer-name)
  (get-buffer-window buffer-name))

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

(defun entropy/shellpop--close-all-active-shellpop-window ()
  (let* ((type-names (mapcar (lambda (x) (car x)) entropy/shellpop--type-register))
         shellpop-buffns
         closed)
    (dolist (type-name type-names)
      (let* ((buffns (plist-get (entropy/shellpop--get-type-buffer-indexs
                                 type-name t)
                                :buffns)))
        (setq shellpop-buffns (append shellpop-buffns buffns))))
    (dolist (buffn shellpop-buffns)
      (when (entropy/shellpop--buffer-active-p buffn)
        (delete-window (get-buffer-window buffn)))
      (push buffn closed))
    closed))

;; *** prunning registered shellpop type entity 
(defun entropy/shellpop--prune-type-register-core (shellpop-type-register)
  (let* ((type-name (car shellpop-type-register))
         (type-plist (cdr shellpop-type-register))
         (type-indexs (plist-get type-plist :indexs))
         (type-buffer-indexs (entropy/shellpop--get-type-buffer-indexs type-name))
         (type-pointer (plist-get type-plist :pointer))
         (type-pointer-alivep (if (integerp type-pointer)
                                  (not (plist-get (entropy/shellpop--get-type-buffer-obj
                                                   type-name type-pointer)
                                                  :isnew))
                                nil))
         ret)
    ;; pointer reset
    (unless type-pointer-alivep
      (setq type-plist (plist-put type-plist :pointer nil))
      (setf (cdr shellpop-type-register) type-plist))
    
    ;; indexs reset
    (when (not (null type-indexs))
      (dolist (index type-indexs)
        (let ((buffer-obj (entropy/shellpop--get-type-buffer-obj type-name (car index))))
          (when (plist-get buffer-obj :isnew)
            (push index ret))))
      (dolist (index ret)
        (setq type-indexs (delete index type-indexs))))

    ;; register non-registered exists buffer
    (when (not (null type-buffer-indexs))
      (let ((indexed (if (not (null type-indexs))
                         (mapcar (lambda (x) (car x)) type-indexs)
                       nil))
            supplements)
        (if indexed
            (dolist (rested-index type-buffer-indexs)
              (unless (member rested-index indexed)
                (push rested-index supplements)))
          (setq supplements type-buffer-indexs))
        (when (not (null supplements))
          (dolist (item supplements)
            (setq type-indexs (append (list (cons item "not described"))
                                      type-indexs))))))

    (setf (cdr shellpop-type-register)
          (plist-put type-plist :indexs type-indexs))))

(defun entropy/shellpop--prune-type-register ()
  (dolist (shellpop-type-register entropy/shellpop--type-register)
    (entropy/shellpop--prune-type-register-core shellpop-type-register)))

;; *** registering shellpop type
(defun entropy/shellpop--type-index-member (index shellpop-type-register-index)
  (let ((cur-indexs shellpop-type-register-index))
    (catch :exit
      (dolist (entry cur-indexs)
        (when (eq index (car entry))
          (throw :exit t))))))

(defun entropy/shellpop--put-index (shellpop-type-name buff-index)
  (let* ((type-name shellpop-type-name)
         (shellpop-type-register (assoc type-name entropy/shellpop--type-register))
         (cur-type-plist (cdr shellpop-type-register))
         (cur-type-indexs (plist-get cur-type-plist :indexs))
         (cur-type-pointer (plist-get cur-type-plist :pointer)))
    (unless (entropy/shellpop--type-index-member buff-index cur-type-indexs)
      (plist-put cur-type-plist :indexs
                 (append (list (cons buff-index (read-string "Type slot 'DES': ")))
                         cur-type-indexs)))
    (unless (eq cur-type-pointer buff-index)
      (plist-put cur-type-plist
                 :pointer buff-index))))

;; *** index overview
(defun entropy/shellpop--make-prompt (shellpop-type-register-index)
  (let* ((name-list (entropy/cl-make-name-alist
                     shellpop-type-register-index
                     (lambda (x) (concat (number-to-string (car x))
                                         ": "
                                         (cdr x)))))
         (choice (completing-read "Select slot: " name-list)))
    (cadr (assoc choice name-list))))

;; *** shellpop type generator

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
               (cur-workdir (expand-file-name default-directory))
               (buffer-ob (entropy/shellpop--get-type-buffer-obj ,type-name index))
               (buffn (plist-get buffer-ob :buffer-name))
               (buff-activep (plist-get buffer-ob :activep))
               (buff-isnew (plist-get buffer-ob :isnew))
               (buff-index (plist-get buffer-ob :index))
               (buff (get-buffer-create buffn))
               (old-type-register (copy-tree (assoc ,type-name entropy/shellpop--type-register)))
               unwind-trigger buffn-not-eq)
          (unwind-protect
              (progn
                (entropy/shellpop--put-index ,type-name buff-index)
                (if buff-activep
                    (delete-window (get-buffer-window buffn))
                  (entropy/shellpop--close-all-active-shellpop-window)
                  (display-buffer buff)
                  (when buff-isnew
                    (with-current-buffer buff
                      ,@type-body
                      (unless (equal buffn (buffer-name))
                        (setq buffn-not-eq (current-buffer))))
                    (when buffn-not-eq
                      (kill-buffer buff)
                      (with-current-buffer buffn-not-eq
                        (rename-buffer buffn)
                        (setq buff (get-buffer buffn))))
                    (with-current-buffer buff
                      (entropy-shellpop-mode t)))
                  (entropy/shellpop--cd-to-cwd cur-workdir buff))
                (setq unwind-trigger t))
            (unless unwind-trigger
              (setf (alist-get ,type-name entropy/shellpop--type-register)
                    (cdr old-type-register))
              (kill-buffer buff)))))

     `(defun ,(intern func-name) (prompt)
        (interactive "P")
        (entropy/shellpop--prune-type-register)
        (let* ((type-reg (assoc ,type-name entropy/shellpop--type-register))
               (type-plist (cdr type-reg))
               (type-pointer (plist-get type-plist :pointer))
               (type-indexs (plist-get type-plist :indexs)))
          (when prompt (entropy/shellpop--close-all-active-shellpop-window))
          (cond (prompt
                 (cond ((eq (car prompt) 16)
                        (,(intern func-name-core)))
                       ((null type-indexs)
                        (error "None regestered popuped shell buffer found!"))
                       ((not (null type-indexs))
                        (,(intern func-name-core)
                         (entropy/shellpop--make-prompt type-indexs)))))
                ((null prompt)
                 (cond ((not (null type-pointer))
                        (,(intern func-name-core) type-pointer))
                       ((null type-pointer)
                        (cond ((not (null type-indexs))
                               (,(intern func-name-core)
                                (entropy/shellpop--make-prompt type-indexs)))
                              ((null type-indexs)
                               (,(intern func-name-core))))))))))
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
                   `(:type-func ,(list :core type-func-core :interact type-func)
                                :indexs nil :pointer nil)))
            entropy/shellpop--type-register))))


;; *** shellpop minor mode
;; **** minor mode
(defvar entropy-shellpop-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode entropy-shellpop-mode
  "Popup shell buffer."
  :initial-value t
  :keymap entropy-shellpop-mode-map
  (if entropy-shellpop-mode
      t
    nil))

(define-key entropy-shellpop-mode-map (kbd "<f1>")
  #'entropy/shellpop--rename-index-desc-within-mode)

;; **** desc modefified

(defun entropy/shellpop--rename-index-desc-core (index shellpop-type-register)
  (let* ((type-name (car shellpop-type-register))
         (type-plist (cdr shellpop-type-register))
         (type-indexs (plist-get type-plist :indexs))
         (old-desc (alist-get index type-indexs nil nil 'equal))
         (prompt (format "Input new slot[%s] desc (old-is: '%s'): " index old-desc))
         (new-desc (read-string prompt)))
    (setf (alist-get index type-indexs)
          new-desc)))

(defun entropy/shellpop--rename-index-desc-within-mode ()
  (interactive)
  (let* ((buffn (buffer-name))
         (buffn-parse (entropy/shellpop--parse-buffer-name-type buffn))
         (type-register (assoc (car buffn-parse) entropy/shellpop--type-register)))
    (entropy/shellpop--rename-index-desc-core
     (cdr buffn-parse)
     type-register)))

;; ** Auto Load

;;;###autoload
(defun entropy/shellpop-start ()
  (interactive)
  (entropy/shellpop--make-types)
  (message "Intialized shellpop feature"))

;; * provide
(provide 'entropy-shellpop)
