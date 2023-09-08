;;; entropy-shellpop.el --- popup shell buffer for transient  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20190829  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac0001@gmail.com>
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
;;
;; Package-Version: 20220517
;; Version:       0.1.0
;; Created:       2019-08-29
;; Keywords:      shell-pop, shell
;; Compatibility: GNU Emacs emacs-version 26.1;
;; Package-Requires: ((cl-lib "1.0") (shackle "1.0.3") (vterm "0.0.1"))
;; #+END_EXAMPLE
;;
;;; Commentary:
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
;;; Configuration:

;; Just cloning this repo under the path sepcified on your wish, and
;; added it to your ~load-path~, using ~require~ or ~use-package~ to
;; manage the configuration for this by calling the main function
;; ~entropy/shellpop-start~. Traditionally minor snippet as:

;; #+BEGIN_SRC elisp
;;   (require 'entropy-shellpop)
;;   (entropy/shellpop-start)
;; #+END_SRC

;; The internal builtin shell popup types are:

;; - for shell-mode: =<f8>=
;; - for eshell: =<f9>=
;; - for ansi-term: =<f10>= (windows native exclude)
;; - for vterm: =<f12>= (windows native exclude)

;; You may customize variable =entropy/shellpop-pop-types= for more
;; specification, see its doc-string for more.

;;; Development

;; For PR and extented aiming for, =entropy-shellpop= provide its own
;; code context map, a illustration for thus as below sections:

;;;; =shellpop-type-register-list= data structure

;; This is a global var as a list of =shellpop-type-register= which be
;; designed for 'entropy-shellpop' to get the overview for all
;; shellpop buffers in current emacs session of arbitrary shell-type
;; (yes =entropy-shellpop= was multi-shell coexist possibly). It group
;; the shellpop buffers into two-step tree, the top was referred to
;; the =shell-type-name= and the subtree was for index of those
;; buffers, that:

;; #+BEGIN_EXAMPLE
;;                             =========================================
;;                                shellpop type register list illustration
;;                             =========================================


;;                                    +-------------------------------+
;;                                    |  shellpop-type-register-list  |
;;                                    +-------------+-----------------+
;;                                ---------/        |        \---------
;;              eshell  ---------/             ansi | term             \---------  shell
;;          +----------/---------------+     +------v------------------+      +--\----------------------+
;;          | shellpop-type-register 1 |     | shellpop-type-register 2|      | shellpop-type-register 3|
;;          +------+-------------------+     +------+------------------+      +------+------------------+
;;                 |                                |                                |
;;       +---------+---------+            +---------+---------+            +---------+---------+
;;       |         |         |            |         |         |            |         |         |
;;       v         v         v            v         v         v            v         v         v
;;   buffer 0   buffer 1  buffer 2    buffer 0   buffer 1  buffer 2    buffer 0   buffer 1  buffer 2
;;       ^                                          ^                                          ^
;;       |                                          |                                          |
;;       |                                          |                                          |
;;    pointer                                    pointer                                    pointer

;; #+END_EXAMPLE

;; With the illustration shown above, =shellpop-type-register= also has
;; the *pointer* to indicate which shellpop buffer of specific shell-type
;; are used currently, this *pointer* was used to briefly popping out the
;; shellpop buffer without items chosen operation.

;; Var =entropy/shellpop--type-registers= was one implementation instance
;; used for thus, its a alist which key was the =shellpop-type-name= a
;; string, and the cdr was the plist whose slots are:

;; + =:type-func=

;;   This slot hosts the one plist to show the current shell-type caller
;;   and its core function.

;;   1) =:core=

;;      Each shellpop-type has one underline function mainly for showing
;;      the shellpop buffer according to this shell-type
;;      specification. This slot was on that.

;;   2) =:interact=

;;      The main caller for this shellpop-type, a function with
;;      interacive features.

;; + =:indexs=

;;   Alist to represent the current shellpop-type shellpop buffers,
;;   formed as
;;   : '((0  . tag-name_0) (1 . tag-name_1) (2 . tag-name_2))'

;;   *tag-name* was the human readable description for its shellpop
;;   buffer.

;; + =:pointer=

;;   *Interger* to indicate which shellpop buffer of this shellpop-type
;;   is used currently.

;;;; =shellpop-buffer-object= data structure

;; As that metioned for section for *shellpop-type-register*, each
;; shellpop buffer of one shellpop-type was mapped in one indexed alist,
;; thus for term, the index space can be unlimited restricted by the
;; emacs's threshold for integer. Says in the other hand, in imagination
;; case, there's infinite shellpop buffers exists in the same time,
;; exists or inexists as the top level concept.

;; Each shellpop buffer registed in =entropy/shellpop--type-registers= was
;; a index with its description, but without any status recorded, and for
;; that the register were not background upate in real-time as a service
;; running for watching as 'watch-dog'. For thus, =entropy-sdcv= gives a
;; shellpop buffer status checking probe function
;; ~entropy/shellpop--get-type-buffer-obj~.

;; The probe function recieve two arguments i.e. the =shellpop-type-name=
;; and the buffer-index (ps: optionally), return one plist strucured of
;; =shellpop-buffer-object= struct. This data structure stored the
;; various buffer status information, include:

;; 1) Whether this index of buffer was exists, key =:isnew= indicated
;;    that.
;; 2) Whether the indexed exists buffer shown in the current frame, key
;;    =:activep= indicated that.
;; 3) The index of this shellpop buffer, key =:index= indicated that.
;; 4) The buffer-naem of the indexed shellpop buffer, key =:buffer-name=
;;    indicated that.

;;;; Extensible developing

;; As for compability case thought for, I think the
;; =shellpop-type-register= and =shellpop-buffer-object= provision was
;; enoughly for you to write some tools to enhance the shellpop behavior,
;; and given the rich way to experience thus. Just do with your flying
;; mind.

;;; Changelog
;; - [2022-05-17 Tue 18:30:06]
;;
;;   Add :pointer reflect preserve API

;; - [2020-11-04 Wed 18:18:29]

;;   Add `entropy-shellpop-mode' specific modeline

;; - [2020-11-04 Wed 12:42:27]

;;   * Comprehensively add `shell-mode' as a type of shellpop
;;     operation

;;   * Fix bug of cdw operation which do not clear current input
;;     before did thus

;; - [2020-05-01 Fri 16:54:34] Do not require vterm at load-time.

;;   * Declare vterm functions.
;;   * Save window configuration when compile vterm module

;; - [2020-01-18] bug fixed

;;   Remove ansiterm and vterm shellpop types enabled in windows.

;; - [2020-01-09] bug fixed

;;   Remove vterm feature on non `--with-modules` feature emacs session

;; - [2020-01-08] Add support for `vterm`

;; - [2019-11-13] *v0.1.0* release out.

;;   The first release of =entropy-sdcv=

;;; Code:
;;
;;;; preface
(defun entropy/shellpop--vterm-supported ()
  (and (member "MODULES" (split-string system-configuration-features nil t))
       (not (eq system-type 'windows-nt))
       (let ((execs '("cmake" "make" "gcc" "libtool" "git"))
             judge)
         (catch :exit
           (dolist (exec execs)
             (unless (executable-find exec)
               (setq judge t)
               (throw :exit nil))))
         (if judge nil t))))

;; Prevent vterm compiling time modify the popup window configuration
(advice-add
 'vterm-module-compile
 :around
 (lambda (orig-func &rest orig-args)
   (save-excursion
     (save-window-excursion
       (apply orig-func orig-args)))))

(defun entropy/shellpop--get-comint-mode-bol-pos ()
  (let ((proc (or (get-buffer-process (current-buffer))
                  (user-error "Current buffer has no process")))
        rtn)
    (setq rtn (ignore-errors (marker-position (process-mark proc))))
    (unless (integerp rtn)
      (error "Comint subprocess are missing!"))
    rtn))

(defun entropy/shellpop--delete-char-for-shell-mode
    (n &optional killflag)
  (interactive "p\nP")
  (if (bound-and-true-p entropy-shellpop-mode)
      (let ((comint-bol-pos (entropy/shellpop--get-comint-mode-bol-pos))
            pos-offset)
        (if (> comint-bol-pos (point))
            (delete-char (- n) killflag)
          (setq pos-offset (- (point) comint-bol-pos))
          (when (< pos-offset 1) (user-error "Beginning of shell prompt line!"))
          (when (> n pos-offset) (setq n pos-offset))
          (delete-char (- n) killflag)))
    (delete-char (- n) killflag)))

(defun entropy/shellpop--comint-send-input ()
  (interactive)
  (let ((comint-bol-pos (entropy/shellpop--get-comint-mode-bol-pos)))
    (if (>= (point) comint-bol-pos)
        (comint-send-input)
      (user-error "Can not operate `comint-send-input' in this point!"))))

(defvar shell-mode-map)
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "DEL")
    #'entropy/shellpop--delete-char-for-shell-mode)
  (define-key shell-mode-map (kbd "RET")
    #'entropy/shellpop--comint-send-input))

;;;; require
(require 'cl-lib)
(require 'shackle)

(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function shrink-path-dirs "shrink-path")


;;;; defcustom
(defgroup entropy/shellpop-customized-group nil
  "entropy-shellpop customized variable group."
  :group 'extensions)

(defcustom entropy/shellpop-shell-popup-key
  (lambda (func) (global-set-key (kbd "<f8>") func))
  "Default key sequence for popup eshell."
  :type 'sexp
  :group 'entropy/shellpop-customized-group)

(defcustom entropy/shellpop-eshell-popup-key
  (lambda (func) (global-set-key (kbd "<f9>") func))
  "Default key sequence for popup eshell."
  :type 'sexp
  :group 'entropy/shellpop-customized-group)

(defcustom entropy/shellpop-ansiterm-popup-key
  (lambda (func) (global-set-key (kbd "<f10>") func))
  "Default key sequence for popup ansi-term."
  :type 'sexp
  :group 'entropy/shellpop-customized-group)

(defcustom entropy/shellpop-vterm-popup-key
  (lambda (func) (global-set-key (kbd "<f12>") func))
  "Default key sequence for popup vterm."
  :type 'sexp
  :group 'entropy/shellpop-customized-group)

(defcustom entropy/shellpop-pop-types
  (let (register
        (types
         `((ansi-term
            :type-name
            "eemacs-ansiterm"
            :shackle-size 0.3
            :shackle-align below
            :type-keybind ,entropy/shellpop-ansiterm-popup-key
            :type-body
            (ansi-term "/bin/bash"))
           (eshell
            :type-name
            "eemacs-eshell"
            :shackle-size 0.3
            :shackle-align below
            :type-keybind ,entropy/shellpop-eshell-popup-key
            :type-body
            (eshell))
           (shell
            :type-name
            "eemacs-shell"
            :shackle-size 0.3
            :shackle-align below
            :type-keybind ,entropy/shellpop-shell-popup-key
            :type-body
            (shell (current-buffer)))
           `((vterm
              :type-name
              "eemacs-vterm"
              :shackle-size 0.3
              :shackle-align bottom
              :type-keybind ,entropy/shellpop-vterm-popup-key
              :type-body
              (let
                  (;; prevent vterm auto rename buffer that lost register linkage
                   (vterm-buffer-name-string nil))
                (vterm-mode)))))))
    (setq register (append register (list (alist-get 'eshell types))))
    (setq register (append register (list (alist-get 'shell types))))
    (when (entropy/shellpop--vterm-supported)
      (setq register (append register (list (alist-get 'vterm types)))))
    (when (not (eq system-type 'windows-nt))
      (setq register (append register (list (alist-get 'ansi-term types)))))
    register)
    "Shell pop types defination.

It's a list of which each element is plist structed form called
SHELLPOP-TYPE of `entropy-shellpop'.

Slot description:

1) :type-name

   Uniquely shellpop-type name traversing
   `entropy/shellpop-pop-types', it will be the name component for
   the generated FUNC name, SHELLPOP-TYPE-REGISTER key, and also
   the popuped shell BUFFER-NAME.

2) :shackle-size

   The popuped shell-buffer window size, it's the float number
   between 0 to 1(do not set it to 1 that will cause messure).

3) :shackle-align

   Popuped position specification, a symbol, valid are:

     'below 'above 'right 'left and t means 'default'.

4) :type-keybind

   The keybinding for this shellpop-type, it's one function to
   bind the interactive function for this SHELL-TYPE, one argument
   slot for that func.

5) :type-body

   The forms to enable shell entity, for example: '(eshell)' or
   '(ansi-term \"/bin/bash\")'.
"

  :type 'sexp
  :group 'entropy/shellpop-customized-group)

;;;; defvar

(defvar entropy/shellpop--type-registers nil)

(defvar entropy/shellpop--top-wcfg-register nil
  "The window configuratio register for recovering for maximized
shellpop type")

;;;; library

;;;;; common libs

(defmacro entropy/shellpop--return-as-default-directory (&rest body)
  "Return a valid `default-directory' value equalized with BODY's value.

This operation exists since `default-directory' has its meaningful
special constructed contention but most of times we did not obey thus
both of our neglects and misusing.

See `default-directory' for its convention details."
  (let ((dfd-sym (make-symbol "dfd-rtn-val")))
    `(let ((,dfd-sym (progn ,@body)))
       (unless (stringp ,dfd-sym)
         (signal 'wrong-type-argument
                 (list 'stringp
                       (format "directory name: %s" ,dfd-sym))))
       (unless (or (string-empty-p ,dfd-sym)
                   (not (directory-name-p ,dfd-sym)))
         (setq ,dfd-sym (directory-file-name ,dfd-sym)))
       (file-name-as-directory ,dfd-sym))))

(defun entropy/shellpop--make-name-alist (olist &optional naming-func)
  "Make a named alist from a list OLIST.

The named alist is a alist which each car of the element is a
specified name object which defaultly is a unique order number for
current position of OLIST if optional NAMING-FUNC was unset.

Optional argument NAMEING-FUNC is a function provided by youself
which has the single argument to accepting one extracted element
of OLIST and return the name object of current element.

Demo:

If OLIST is (1 2 3 4 5), NAMING-FUNC is '(lambda (x) (+ 1 x))
then retun name-alist:

((2 . 1) (3. 2) (4. 3) (5. 4) (6. 5))
"
  (let* (rtn
         (count -1))
    (dolist (el olist)
      (push `(,(if (functionp naming-func)
                   (funcall naming-func el)
                 (cl-incf count))
              .
              ,el)
            rtn))
    (if rtn
        (reverse rtn)
      (error "[entropy/shellpop--make-name-alist]: occur wrong"))))

(defun entropy/shellpop--plist-get-rest (plist-var key)
  "Get the whole plist PLIST-VAR values-seq after the key KEY and
return them into list ordered as original case."
  (let (nth-key nth-rest-tail (count 0) rtn)
    (catch :exit
      (dolist (el plist-var)
        (when (eq el key)
          (setq nth-key count)
          (throw :exit nil))
        (cl-incf count)))
    (catch :exit
      (cl-loop for pos from (+ nth-key 1) to (- (length plist-var) 1)
               when (and (symbolp (nth pos plist-var))
                         (string-match-p "^:" (symbol-name (nth pos plist-var))))
               do (progn (setq nth-rest-tail (- pos 1))
                         (throw :exit nil))))
    (unless (integerp nth-rest-tail)
      (setq nth-rest-tail (- (length plist-var) 1)))
    (cl-loop for pos from (+ 1 nth-key) to nth-rest-tail
             do (push (nth pos plist-var) rtn))
    (reverse rtn)))

(defun entropy/shellpop--remote-files-same-host-p (&rest files)
  (if files
      (let (remote-ids)
        (dolist (f files)
          (cl-pushnew (file-remote-p f)
                      remote-ids
                      :test 'equal)
          )
        (= 1 (length remote-ids)))
    (error "[entropy-shellpop]: arguments empty")))

(defun entropy/shellpop--remote-fname-unquote (frname)
  (let ((remote-id (file-remote-p frname)))
    (if remote-id
        (string-remove-prefix
         remote-id frname)
      frname)))

;;;;; specific delete window function

(defun entropy/shellpop--delete-window (window)
  (catch :exit
    (unless (one-window-p)
      (delete-window window)
      (throw :exit nil))
    (progn
      (cond ((window-configuration-p entropy/shellpop--top-wcfg-register)
             (set-window-configuration
              entropy/shellpop--top-wcfg-register)
             (delete-window window))
            (t
             (message
              "can not restore window-configuration \
for current maximized pop-shell."))))))

;;;;; specific confirm function

(defun entropy/shellpop--confirm (prompt)
  (condition-case nil
      (y-or-n-p prompt)
    ((quit error) nil)))

;;;;; cdw functions
(defmacro entropy/shellpop--cd-to-cwd-with-judge (path-given prompt &rest body)
  (let ((path-sym (make-symbol "path-specified")))
    `(let ((,path-sym ,path-given))
       (unless (or
                ;; when the current shell host not as same as the new one,
                ;; we ignore the auto CDW feature, since its may cause
                ;; current shellpop slot unreliable for user common
                ;; operation which is usually not the user's expection.
                (not (entropy/shellpop--remote-files-same-host-p
                      default-directory ,path-sym))
                (eq
                 ;; FIXME: the non-nil return is not extremely indicate
                 ;; TRUE which is described in docstring of `file-equal-p'.
                 (file-equal-p
                  (expand-file-name default-directory)
                  (expand-file-name ,path-sym))
                 t))
         (cond (,prompt
                (when (entropy/shellpop--confirm
                       (format "CD to '%s'" ,path-sym))
                  ,@body))
               ((null ,prompt)
                ,@body))))))

(declare-function eshell-reset "esh-mode")
(defun entropy/shellpop--cd-to-cwd-eshell (cwd)
  (if (eshell-process-interact 'process-live-p)
      (message "Won't change CWD because of running process.")
    (entropy/shellpop--cd-to-cwd-with-judge
     cwd nil
     (setq default-directory
           (entropy/shellpop--return-as-default-directory cwd))
     (eshell-reset))))

(defun entropy/shellpop--cd-to-cwd-shell (cwd)
  (entropy/shellpop--cd-to-cwd-with-judge
   cwd t
   (goto-char (point-max))
   (comint-kill-input)
   (insert
    (concat
     "cd "
     (shell-quote-argument
      (entropy/shellpop--remote-fname-unquote
       cwd))))
   (let ((comint-process-echoes t))
     (comint-send-input))
   (recenter 1)))

(defun entropy/shellpop--cd-to-cwd-term (cwd)
  (unless
      ;; emacs-term not support remote auto cd
      (file-remote-p cwd)
    (entropy/shellpop--cd-to-cwd-with-judge
     cwd t
     (term-send-raw-string "\C-u")
     (term-send-raw-string "\C-k")
     (term-send-raw-string
      (concat "cd "
              (shell-quote-argument
               cwd)
              "\n"))
     (term-send-raw-string "\C-l"))))

(defun entropy/shellpop--cd-to-cwd-vterm (cwd)
  (entropy/shellpop--cd-to-cwd-with-judge
   cwd t
   (vterm-send-C-u)
   (vterm-send-C-k)
   (vterm-send-string
    (concat "cd "
            (shell-quote-argument
             (entropy/shellpop--remote-fname-unquote
              cwd))))
   (vterm-send-return)
   (vterm-send-C-l)))

(defun entropy/shellpop--cd-to-cwd (cwd buff)
  (with-current-buffer buff
    (let ((abspath (expand-file-name cwd)))
      (cond ((eq major-mode 'eshell-mode)
             (entropy/shellpop--cd-to-cwd-eshell abspath))
            ((eq major-mode 'shell-mode)
             (entropy/shellpop--cd-to-cwd-shell abspath))
            ((eq major-mode 'term-mode)
             (entropy/shellpop--cd-to-cwd-term abspath))
            ((eq major-mode 'vterm-mode)
             (entropy/shellpop--cd-to-cwd-vterm abspath))
            (t
             (message "Shell type not supported for 'entropy-shellpop' to CDW"))))))


;;;;; shellpop type name generator
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
  (let* ((type-names (mapcar (lambda (x) (car x)) entropy/shellpop--type-registers))
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

;;;;; buffer bunches parses
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

;;;;; shellpop type entity object generator
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

;;;;; close all actived shellpop window
(defun entropy/shellpop--close-all-active-shellpop-window ()
  (let* ((type-names (mapcar (lambda (x) (car x)) entropy/shellpop--type-registers))
         shellpop-buffns
         closed)
    (dolist (type-name type-names)
      (let* ((buffns (plist-get (entropy/shellpop--get-type-buffer-indexs
                                 type-name t)
                                :buffns)))
        (setq shellpop-buffns (append shellpop-buffns buffns))))
    (dolist (buffn shellpop-buffns)
      (when (entropy/shellpop--buffer-active-p buffn)
        (entropy/shellpop--delete-window (get-buffer-window buffn)))
      (push buffn closed))
    closed))

;;;;; prunning registered shellpop type entity
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
  (dolist (shellpop-type-register entropy/shellpop--type-registers)
    (entropy/shellpop--prune-type-register-core shellpop-type-register)))

;;;;; registering shellpop type
(defun entropy/shellpop--type-index-member (index shellpop-type-register-index)
  (let ((cur-indexs shellpop-type-register-index))
    (catch :exit
      (dolist (entry cur-indexs)
        (when (equal index (car entry))
          (throw :exit t))))))

(defun entropy/shellpop--put-index
    (shellpop-type-name buff-index &optional when-available no-new-desc)
  (let* ((type-name shellpop-type-name)
         (shellpop-type-register (assoc type-name entropy/shellpop--type-registers))
         (cur-type-plist (cdr shellpop-type-register))
         (cur-type-indexs (plist-get cur-type-plist :indexs))
         (cur-type-pointer (plist-get cur-type-plist :pointer))
         (available (entropy/shellpop--type-index-member buff-index cur-type-indexs)))
    (catch :exit
      (when (and when-available (not available))
        (throw :exit nil))
      (unless available
        (setq cur-type-plist
              (plist-put cur-type-plist :indexs
                         (append
                          (list
                           (cons
                            buff-index
                            (or
                             (and no-new-desc
                                  (stringp no-new-desc)
                                  (not (string-empty-p no-new-desc))
                                  no-new-desc)
                             (and no-new-desc
                                  (format "anonymous new for '%s' at [%s]"
                                          shellpop-type-name
                                          buff-index))
                             (read-string "Type slot 'DES': "))))
                          cur-type-indexs)))
        (setf (cdr shellpop-type-register) cur-type-plist))
      (unless (eq cur-type-pointer buff-index)
        (setq cur-type-plist
              (plist-put cur-type-plist
                         :pointer buff-index))
        (setf (cdr shellpop-type-register) cur-type-plist))
      t)))

;;;;; index overview
(defun entropy/shellpop--make-prompt (shellpop-type-register-index)
  (let* ((name-list (entropy/shellpop--make-name-alist
                     shellpop-type-register-index
                     (lambda (x) (concat (number-to-string (car x))
                                         ": "
                                         (cdr x)))))
         (choice (completing-read "Select slot: " name-list)))
    (cadr (assoc choice name-list))))

;;;;; shellpop type generator

(defun entropy/shellpop--make-type-core (shellpop-type)
  (let* ((type-name (plist-get shellpop-type :type-name))
         (func-name-core (entropy/shellpop--gen-type-func-name
                          type-name 'core))
         (func-name (entropy/shellpop--gen-type-func-name
                     type-name t))
         (type-size (plist-get shellpop-type :shackle-size))
         (type-align (plist-get shellpop-type :shackle-align))
         (type-bind (plist-get shellpop-type :type-keybind))
         (type-body (entropy/shellpop--plist-get-rest shellpop-type :type-body))
         (buffern-regexp (entropy/shellpop--gen-buffn-regexp type-name)))
    (list
     `(defun ,(intern func-name-core) (&optional index)
        (unless shackle-mode
          (shackle-mode t))
        (let* ((shackle-rules '((,buffern-regexp :regexp t
                                                 :select t
                                                 :align ,type-align
                                                 :size ,type-size
                                                 :autoclose t)))
               (cur-workdir (expand-file-name default-directory))
               (buffer-ob (entropy/shellpop--get-type-buffer-obj ,type-name index))
               (buffn (plist-get buffer-ob :buffer-name))
               (buff-activep (plist-get buffer-ob :activep))
               (buff-isnew (plist-get buffer-ob :isnew))
               (buff-index (plist-get buffer-ob :index))
               (buff (get-buffer-create buffn))
               (old-type-register (copy-tree (assoc ,type-name entropy/shellpop--type-registers)))
               unwind-trigger buffn-not-eq)
          (unwind-protect
              (progn
                (unless (and (< ,type-size 1)
                             (> ,type-size 0))
                  (error "Unsupport shellpop window size '%s'" ,type-size))
                (entropy/shellpop--put-index ,type-name buff-index)
                (if buff-activep
                    (entropy/shellpop--delete-window (get-buffer-window buffn))
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
              (setf (alist-get ,type-name entropy/shellpop--type-registers)
                    (cdr old-type-register))
              (when buff-isnew
                (kill-buffer buff))))))

     `(defun ,(intern func-name) (prompt)
        (interactive "P")
        (entropy/shellpop--prune-type-register)
        (let* ((type-reg (assoc ,type-name entropy/shellpop--type-registers))
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
     `(when (functionp ',type-bind)
        (funcall ',type-bind #',(intern func-name))))))

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
            entropy/shellpop--type-registers))))


;;;;; shellpop minor mode
;;;;;; minor mode
(defvar entropy-shellpop-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode entropy-shellpop-mode
  "Popup shell buffer."
  :initial-value t
  :keymap entropy-shellpop-mode-map
  (if entropy-shellpop-mode
      (entropy/shellpop--set-modeline-format)
    nil))

(defun entropy/shellpop--define-key (key-maps)
  (cl-loop for (key-bind . func) in key-maps
           do (define-key entropy-shellpop-mode-map
                (kbd key-bind) func)))

(entropy/shellpop--define-key
 '(("<f1>" . entropy/shellpop-rename-index-desc-within-mode)
   ("C-x 1" . entropy/shellpop-delete-other-widnow)
   ("\C-u" . universal-argument)
   ("C-h k" . describe-key)
   ("M-:" . eval-expression)
   ("C-g" . keyboard-quit)))

;;;;;; shellpop description

(defun entropy/shellpop--rename-index-desc-core (index shellpop-type-register)
  (let* ((_type-name (car shellpop-type-register))
         (type-plist (cdr shellpop-type-register))
         (type-indexs (plist-get type-plist :indexs))
         (old-desc (alist-get index type-indexs nil nil 'equal))
         (prompt (format "Input new slot[%s] desc (old-is: '%s'): " index old-desc))
         (new-desc (read-string prompt)))
    (setf (alist-get index type-indexs)
          new-desc)))

(defun entropy/shellpop--get-index-desc-core (index shellpop-type-register)
  (let* ((_type-name (car shellpop-type-register))
         (type-plist (cdr shellpop-type-register))
         (type-indexs (plist-get type-plist :indexs))
         (old-desc (alist-get index type-indexs nil nil 'equal)))
    old-desc))

(defun entropy/shellpop-rename-index-desc-within-mode ()
  (interactive)
  (let* ((buffn (buffer-name))
         (buffn-parse (entropy/shellpop--parse-buffer-name-type buffn))
         (type-register (assoc (car buffn-parse) entropy/shellpop--type-registers)))
    (entropy/shellpop--rename-index-desc-core
     (cdr buffn-parse)
     type-register)))

(defun entropy/shellpop-get-index-desc-within-mode ()
  (let* ((buffn (buffer-name))
         (buffn-parse (entropy/shellpop--parse-buffer-name-type buffn))
         (type-register (assoc (car buffn-parse) entropy/shellpop--type-registers)))
    (entropy/shellpop--get-index-desc-core
     (cdr buffn-parse)
     type-register)))


;;;;;; register for delete other window

(defun entropy/shellpop-delete-other-widnow ()
  (interactive)
  (unless (one-window-p)
    (let ((wfg-cur (current-window-configuration)))
      (setq entropy/shellpop--top-wcfg-register wfg-cur)
      (delete-other-windows))))

;;;;;; shellpop modeline format

(defface entropy/shellpop--modeline-highlight-basic-face
  '((((class color) (min-colors 88) (background light))
     :weight light
     :box (:line-width -1 :color "grey75" :style nil))
    (((class color) (min-colors 88) (background dark) )
     :weight light
     :box (:line-width -1 :color "grey40" :style nil)))
  "")

(defface entropy/shellpop--modeline-highlight-major-mode-face
  '((default
      :inherit
      entropy/shellpop--modeline-highlight-basic-face)
    (((class color) (min-colors 88) (background light))
     :weight bold :foreground "red"
     :box (:line-width -1 :color "grey75" :style nil))
    (((class color) (min-colors 88) (background dark) )
     :weight bold :foreground "yellow"
     :box (:line-width -1 :color "grey40" :style nil)))
  "")

(defface entropy/shellpop--modeline-highlight-working-on-face
  '((default
      :inherit
      entropy/shellpop--modeline-highlight-basic-face)
    (((class color) (min-colors 88) (background light))
     :slant italic :weight bold)
    (((class color) (min-colors 88) (background dark) )
     :slant italic :weight bold))
  "")

(defface entropy/shellpop--modeline-highlight-default-directory-face
  '((default
      :inherit
      entropy/shellpop--modeline-highlight-basic-face)
    (((class color) (min-colors 88) (background light))
     :slant italic :weight light)
    (((class color) (min-colors 88) (background dark) )
     :slant italic :weight light))
  "")

(defun entropy/shellpop--set-modeline-format ()
  (let* ((lhs '(format "%s %s%s %s%s %s%s%s"
                       (propertize "*Shellpop*" 'face 'tty-menu-enabled-face)
                       (propertize "Desc: " 'face 'success)
                       (let ((orig-desc (entropy/shellpop-get-index-desc-within-mode))
                             unset-p)
                         (when (or (null orig-desc)
                                   (string-empty-p orig-desc))
                           (setq orig-desc "[No desc yet]"
                                 unset-p t))
                         (if unset-p
                             orig-desc
                           (propertize orig-desc 'face 'warning)))
                       (propertize "type: " 'face 'success)
                       (propertize
                        (car (entropy/shellpop--parse-buffer-name-type
                              (buffer-name)))
                        'face 'bold-italic)
                       (propertize (format "@%s " major-mode)
                                   'face 'entropy/shellpop--modeline-highlight-major-mode-face)
                       (propertize "Working on: " 'face 'entropy/shellpop--modeline-highlight-working-on-face)
                       (propertize default-directory
                                   'face
                                   'entropy/shellpop--modeline-highlight-default-directory-face)))
         (rhs '(format "%s%s "
                       (propertize "Index: " 'face 'success)
                       (ignore-errors
                         (number-to-string
                          (cdr (entropy/shellpop--parse-buffer-name-type
                                (buffer-name)))))))
         (mid-str (propertize " "
                              'display
                              `((space
                                 :align-to
                                 (- (+ right right-fringe right-margin)
                                    ,(string-width
                                      (format-mode-line (list :eval rhs)))
                                    ))))))
    (setq-local mode-line-format
                `("%e"
                  (:eval ,lhs)
                  ,mid-str
                  (:eval ,rhs)))))

;;;; Auto Load

;;;###autoload
(defun entropy/shellpop-start ()
  (interactive)
  (entropy/shellpop--make-types)
  (message "Intialized shellpop feature"))

;;;###autoload
(defun entropy/shellpop-expose-type-registers-pointer ()
  "Return an object reflect each :pointer to a
=shellpop-type-register=, just used for
`entropy/shellpop-replace-type-registers-pointer-as'."
  (let (rtn type-name pointer)
    (dolist (el entropy/shellpop--type-registers)
      (setq type-name (car el)
            pointer (plist-get (cdr el) :pointer))
      (when pointer
        (push (list type-name
                    :pointer pointer
                    :wcfg entropy/shellpop--top-wcfg-register)
              rtn)))
    rtn))

(defun entropy/shellpop-replace-type-registers-pointer-as
    (var &optional desc)
  "Reset current =shellpop-type-register-list= with the new
:pointer patch obtained by
`entropy/shellpop-expose-type-registers-pointer'."
  (or desc (setq desc t))
  (dolist (el var)
    (let* ((type-name (car el))
           (attrs (cdr el))
           (pointer (plist-get attrs :pointer))
           (wcfg (plist-get attrs :wcfg)))
      (when pointer
        (entropy/shellpop--put-index
         type-name pointer t desc)
        (when (and wcfg
                   (window-configuration-p wcfg))
          (setq entropy/shellpop--top-wcfg-register
                wcfg))))))

;;; provide
(provide 'entropy-shellpop)