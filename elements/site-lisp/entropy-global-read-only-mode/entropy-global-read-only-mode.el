;;; entropy-global-readonly-mode --- Simple global read-only mode  -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 20200221  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-global-read-only-mode
;; Package-Version: 0.1.2
;; Created:       2018
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
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
;;; Commentary:

;; Whether the time you want let the buffer opening behaviour be
;; read-only defaultly?

;; This package giving you the try for as.

;; As it's name 'global read only' meaning, the main feature concept was
;; as that but with some custome rule base. Some buffer
;; e.g. compiling refers can not do so unless you want to get the
;; unexpected things out.

;; The rule base for is follow the below two way:

;; 1) Modes respective

;;    Let file start up with read-only mode follow it's buffer major mode
;;    specification.

;; 2) Lock files at startup for general view.

;;    Single the way for each file opening about, but the special
;;    buffer list regexp matching for.

;;;; Requirements

;; The only one extra melpa extension [[https://orgmode.org/][org]] is required. Org mode utilies
;; need treating for specially read-only setting way, thus this package
;; will give some re-defun coding snippets for the ones member of those
;; utilies. But all the re-defun procedure are just enabled when =org=
;; loaded, there's no need to require =org= with the manually way.

;;;; Installation

;; Download main [[file:entropy-global-read-only-mode.el][source file]] to your load-path and ~require~ it as
;; the most easy way.

;;;; Configuration

;; The based-rule set mentioned above was given by the customize variable
;; =entropy/grom-readonly-type= which gives list of valid internal string
;; type value for as:

;; - "modes" :

;;   Initializing read-only type for the major-modes list in
;;   =entropy/grom-mode-hooks-list= and it's default value is:
;;   #+BEGIN_EXAMPLE
;;     emacs-lisp-mode-hook
;;     c-mode-hook
;;     php-mode-hook
;;     web-mode-hook
;;     python-mode-hook
;;     js2-mode-hook
;;     css-mode-hook
;;     org-mode-hook
;;     json-mode-hook
;;     markdown-mode-hook
;;     bat-mode-hook
;;     text-mode-hook
;;   #+END_EXAMPLE

;;   This variable was customized, you may want to specified it along
;;   with your own benefit.


;; - "all" :

;;   Initialize all file opening read-only type based on the wide
;;   rule set of the buffer name filters
;;   =entropy/grom-customizable-special-buffer-name-regexp-list=. Further
;;   more, there's also an non-special exception option
;;   =entropy/grom-customizable-nonspecial-buffer-name-regexp-list=
;;   which let you add some exclusion that the special sepcification
;;   wild included.


;; You can select one of them be the global-read-only-type for as.

;; The =use-packge= configure management type demo as:
;; #+BEGIN_SRC emacs-lisp
;;   (use-package entropy-global-read-only-mode
;;     :ensure nil
;;     :load-path "path-to-your-load-path"
;;     :commands (entropy-grom-mode)
;;     :init (add-hook 'after-init-hook #'entropy-grom-mode))
;; #+END_SRC
;;
;;;; Interaction

;; - Function: ~entropy-grom-mode~

;;   Mainly global read only mode enable or disable function. Enabling
;;   obeying the rule set =entropy/grom-readonly-type=.

;; - Function: ~entropy/grom-toggle-read-only~

;;   Toggle global buffers read-only status in =buffer-list= basic on
;;   the buffer name regexp matching regexp rule set of that one is
;;   =entropy/grom--internal-specified-special-bfregexp-list= the basically core
;;   native builtin one and what you can customizable one
;;   =entropy/grom-customizable-special-buffer-name-regexp-list=.

;; - Function: ~entropy/grom-read-only-buffer~

;;   Quickly lock current buffer or the otherwise as the emacs internal
;;   func ~read-only-mode~ but with this package specification.

;; - Function: ~entropy/grom-quick-readonly-global~

;;   Quickly lock all active buffers using the rule set of func
;;   ~entropy/grom-toggle-read-only~.

;;;; Redefine functions and advices tracking

;; There's some necessary case for redefining some packages refered
;; functions, the majority occurrence one of them is that they
;; operated buffer without buffer read-only status checking, thus
;; they thrown out errors of unexpect process interrupted.

;; Til now in this package, all redefined function are all the
;; utilities of =org-mode=. Most of org buffer operation are not
;; checking the buffer locked status and for the unlocking
;; automatically way.Thus, the redefined core reason is to embed the
;; unlock codes into them respectively. The defination will be
;; recovered when =entropy/grom-mode= disabled.

;;; Changelog:
;;
;; - [2022-04-10 Sun 17:48:35] Bug fix
;;
;;   For now we will auto detect the `grep' like buffer which commonly
;;   use `wgrep' to handle the read-only, and use `wgrep' refer
;;   commands simulate the readonly operations since the commonly
;;   `read-only-mode' is not suitable for that buffers and will
;;   corrupt the emacs-sessioin, e.g. incorrectly unlock with user
;;   modification will cause 'Text is readonly' error cover whole
;;   emacs session.
;;
;;   Others:
;;
;;   1. `entropy/grom--buffer-special-p' is migrate to
;;      `entropy/grom-buffer-special-p' which be an exposed API.
;;
;;   2. Refer to the main bug fix, command
;;      `entropy-global-read-only-mode.el' also check the special
;;      buffer when unlock operation.
;;
;; - [2021-01-29 Fri 23:34:17] Support org patch to "modes" grom type

;;   Others:

;;   * Require features before patching them instead of autoloads with
;;     `eval-after-load' so that we can disable it from patcher
;;     register directly while disable grom-mode.

;; - [2021-01-11 Mon 19:18:25] Use internal readonly routine
;;   `entropy/grom--readonly-1&0' instead of commonly used
;;   `read-only-mode' to have more condition cases filter.

;; - [2020-06-24 Wed 13:51:37] Add nonspecial customizable option and context update

;; - [2020-05-29 Fri 20:13:23] Optimize namespace

;;   * Make internal librariese follow conventions by emacs
;;     non-provided named specification rule.

;;   * Fix logical bug.

;;   * Add treemacs-persist file match rule detection.


;; - [2020-04-04 Sat 08:05:27] Bump to version 0.1.1

;;   1) Subroutines mechnism update

;;   2) Bug fix

;; - [2020-02-21] disable prompt for unlock buffer

;;   "for simply and quickly toggle"

;; - [2018-08-01] version 0.1.0 release

;;; Code:

;;;; require
(require 'cl-lib)

(declare-function wgrep-exit "ext:wgrep")
(declare-function wgrep-change-to-wgrep-mode "ext:wgrep")
(declare-function wdired-exit "ext:wdired")
(declare-function wdired-change-to-wdired-mode "ext:wdired")

(defvar org-agenda-files)
(defvar org-agenda-mode-map)
(defvar org-capture-plist)

;;;; declare variable
(defvar entropy/grom--internal-specified-special-bfregexp-list
  '(
    ;; Some important special buffer
    "magit-process:"
    "magit:"
    "CAPTURE-.*\\.org$"
    "treemacs-persist"
    "newsrc"
    "autoloads\\.el"
    "loaddefs\\.el"
    "COMMIT_EDITMSG"
    "treemacs-persist"
    "_archive"
    ;; for all special buffers which start with wildards follow emacs
    ;; internal conventions
    "^ *\\*.*?\\* *$"
    )
  "The list is used by `entropy/grom-toggle-read-only' for excluding
buffer with BUFFER-NAME REGXP in this list, basiclly this variable
was the core buffer name regexp matching list for \"all\" type of
`entropy/grom-readonly-type'.

Do not modify it manually, or will messy up with risky on
your-self way. If need to do that, add rules into customizable
variable `entropy/grom-customizable-special-buffer-name-regexp-list' instead."
  )

(defvar entropy/grom-buffer-true-name-pair-list nil
  "This list contains the FILENAME of the all buffer name
BUFFER-NAME in current frame and obtained by
`entropy/grom--get-true-buffer-name-pair-list', which is a list of
cons cell of (file_truename . buffer).")

(defvar entropy/grom--advice-register nil
  "Register for adivce information built by `entropy/grom--add-advice'.")
(defvar entropy/grom--add-hook-register nil
  "Register for adivce information built by `entropy/grom--add-hook'.")
(defvar entropy/grom--external-patch-register nil
  "Register of sets of grom-patch information.

A grom-patch is a plist with bellow falid key slots:
- ':init' : a function to enable the patcher or nil.
- ':setoff' : a function to disable the patcher or nil.

Each grom-patch will enabled by `entropy/grom--enable-patcher' and
disabled by `entropy/grom--disable-patcher'.")

;;;; custom variable
(defgroup entropy-grom nil
  "Customizable group for `entropy-grom-mode'."
  :group 'extensions)

(defcustom entropy/grom-readonly-type "all"
  "Choose the type of init-read-only method:

There's two choice:
1. 'all':   Let find-file be default the read-only mode.
2. 'modes': Use the list of major-modes' hooks to embend it in.
            The list variable is `entropy/grom-mode-hooks-list'
"
  :type '(choice (string :tag "For find file all" "all")
                 (string :tag "For mode hook specified" "modes"))
  :group 'entropy-grom)

(defcustom entropy/grom-mode-hooks-list
  '(emacs-lisp-mode-hook
    c-mode-hook
    php-mode-hook
    web-mode-hook
    python-mode-hook
    js2-mode-hook
    css-mode-hook
    org-mode-hook
    json-mode-hook
    markdown-mode-hook
    bat-mode-hook
    text-mode-hook)
  "The list of which major mode hook to be injected read-only
functional hook at start-up."
  :type '(repeat (symbol :tag "Mode hook symbol"))
  :group 'entropy-grom)

(defcustom entropy/grom-customizable-special-buffer-name-regexp-list nil
  "External except buffer name regexp list excluding when enable
read-only mode while `entropy-grom-mode' enabled.

Its defaultly empty of no problem or added as your own
specification, all rules list in here will be combined with
`entropy/grom--internal-specified-special-bfregexp-list' for
expection dealing with."
  :type '(choice (const nil)
                 (repeat (regexp :tag "Special buffer name regexp")))
  :group 'entropy-grom)

(defcustom entropy/grom-customizable-nonspecial-buffer-name-regexp-list
  `(,(rx "*scratch*"))
  "Specified permanently non-special meaningful buffer name regexp
list."
  :type '(choice (const nil)
                 (repeat (regexp :tag "Non-special buffer name regexp")))
  :group 'entropy-grom)

;;;; library

(defun entropy/grom--readonly-1&0 (type)
  "=entropy-grom= internal `read-only-mode' routine which specified
read-only functions in cases dependent, and return an signal
symbol to show which routine type used.

For now valid signal are:
- 'wdired' : which use wdired-mode to lock(save) and unlock(edit)
- 'wgrep'  : which use wgrep to lock(save) and unlock(edit)
- 'nil'    : commonly use `read-only-mode'"
  (let (func-enable func-disable rtn)
    (cond ((or (derived-mode-p 'dired-mode)
               (eq major-mode 'wdired-mode))
           (setq func-enable 'wdired-exit
                 func-disable 'wdired-change-to-wdired-mode
                 rtn 'wdired))
          ((member major-mode
                   '(rg-mode
                     ivy-occur-grep-mode
                     ;; TODO: add more major-mode use `wgrep' to
                     ;; simulate the readonly operation.
                     ))
           (setq func-enable 'wgrep-exit
                 func-disable 'wgrep-change-to-wgrep-mode
                 rtn 'wgrep))
          (t
           (setq func-enable #'(lambda () (setq buffer-read-only t))
                 func-disable #'(lambda () (setq buffer-read-only nil)))))
    (cond ((= type 1)
           (funcall func-enable))
          ((= type 0)
           (funcall func-disable))
          (t
           (error "[entropy/grom--readonly-1&0] Invalid mode type %s" type)))
    rtn))

(defun entropy/grom--get-special-buffer-name-regexp-list ()
  "Combined `entropy/grom-customizable-special-buffer-name-regexp-list' with
`entropy/grom--internal-specified-special-bfregexp-list' to generating the
whole expections rules."
  (let ((whole-init (copy-tree entropy/grom--internal-specified-special-bfregexp-list)))
    (dolist (el entropy/grom-customizable-special-buffer-name-regexp-list)
      (unless (member el whole-init)
        (setq whole-init
              (append whole-init (list el)))))
    whole-init))

(defun entropy/grom-buffer-special-p (buffer-or-name)
  (if (equal entropy/grom-readonly-type "all")
      (let ((buffer-name
             (or (and (stringp buffer-or-name) buffer-or-name)
                 (and (bufferp buffer-or-name)
                      (buffer-name buffer-or-name))
                 (error "Wrong type of argument: bufferp--> '%s'" buffer-or-name))))
        (catch :exit
          (dolist (rule (entropy/grom--get-special-buffer-name-regexp-list))
            (when (and (string-match-p rule buffer-name)
                       (not (catch :exit-0
                              (dolist (el entropy/grom-customizable-nonspecial-buffer-name-regexp-list)
                                (when (string-match-p el buffer-name)
                                  (throw :exit-0 t))))))
              (throw :exit t)))))
    nil))

(defun entropy/grom--get-true-buffer-name-pair-list ()
  "Getting current frame's actived buffer true name with
`buffer-file-name' and push them into
`entropy/grom-buffer-true-name-pair-list'
"
  (setq entropy/grom-buffer-true-name-pair-list nil)
  (dolist (buffer (mapcar (function buffer-name) (buffer-list)))
    (let ((tbname (if (with-current-buffer buffer buffer-file-truename)
                      (file-truename
                       (with-current-buffer buffer buffer-file-truename))
                    nil)))
      (add-to-list 'entropy/grom-buffer-true-name-pair-list `(,tbname . ,buffer)))))

(cl-defun entropy/grom--enable-read-only (&key message buffer no-check)
  (let ((buffer (or buffer (current-buffer)))
        signal)
    (if (null no-check)
        (if (entropy/grom-buffer-special-p buffer)
            (when message
              (user-error "You can not make '%s' read only, because this
operation may cause some risk. ('M-x read-only-mode' for forcely)"
                          (buffer-name)))
          (with-current-buffer buffer
            (setq signal (entropy/grom--readonly-1&0 1))))
      (entropy/grom--readonly-1&0 1))
    (when message
      (message "Lock %s%s successfully"
               (if (eq buffer (current-buffer))
                   "current-buffer"
                 buffer)
               (if signal
                   (format " use type [%s] " signal)
                 "")))))

(cl-defun entropy/grom--disable-read-only (&key message buffer no-check)
  (let ((buffer (or buffer (current-buffer)))
        signal)
    (if (null no-check)
        (if (entropy/grom-buffer-special-p buffer)
            (when message
              (user-error "You can not unlock '%s', because this operation may cause some
risk. ('M-x read-only-mode' for forcely)"
                          (buffer-name)))
          (with-current-buffer buffer
            (setq signal (entropy/grom--readonly-1&0 0))))
      (entropy/grom--readonly-1&0 0))
    (when (and message
               ;; do not cover the message come from those type.
               (not (member signal '(wdired wgrep))))
      (message "Unlock %s%s successfully"
               (if (eq buffer (current-buffer))
                   "current-buffer"
                 buffer)
               (if signal
                   (format " use type [%s] " signal)
                 "")))))

(defun entropy/grom--add-hook (hook function)
  (add-to-list 'entropy/grom--add-hook-register
               (list hook function))
  (add-hook hook function))

(defun entropy/grom--add-advice (symbol where function)
  (add-to-list 'entropy/grom--advice-register
               (list symbol function))
  (advice-add symbol where function))

(defun entropy/grom--remove-hook-and-advice ()
  (mapc (lambda (grom-hook-registor)
          (apply 'remove-hook grom-hook-registor))
        entropy/grom--add-hook-register)
  (setq entropy/grom--add-hook-register nil)
  (mapc (lambda (grom-advice-registor)
          (apply 'advice-remove grom-advice-registor))
        entropy/grom--advice-register)
  (setq entropy/grom--advice-register nil))

(defmacro entropy/grom--with-require-feature
    (feature &rest body)
  "Do body after FEATURE required if available, FEATURE can be a
single one or a list of thus. "
  (declare (indent defun))
  (macroexp-let2* ignore
      ((fts-sym nil)
       (ftlp-sym nil)
       (fatp-sym nil))
    `(let* ((,fts-sym ,feature) ,fatp-sym
            (_ (unless (consp ,fts-sym)
                 (setq ,fts-sym (list ,fts-sym)))))
       (mapc (lambda (fr)
               (unless (or (featurep fr)
                           (ignore-errors (require fr)))
                 (setq ,fatp-sym t))) ,fts-sym)
       (unless ,fatp-sym ,(macroexp-progn body)))))

(defun entropy/grom--enable-patcher ()
  (dolist (patcher entropy/grom--external-patch-register)
    (let ((init (plist-get patcher :init)))
      (when (functionp init)
        (funcall init)))))

(defun entropy/grom--disable-patcher ()
  (dolist (patcher entropy/grom--external-patch-register)
    (let ((setoff (plist-get patcher :setoff)))
      (when (functionp setoff)
        (funcall setoff)))))

;;;; Read-only minor tools
;;;;; Buffer lock status toggle quick way
;;;###autoload
(defun entropy/grom-read-only-buffer ()
  (interactive)
  (if buffer-read-only
      (entropy/grom--disable-read-only :message t)
    (entropy/grom--enable-read-only :message t)))

;;;;; Global read only toggle function
;;;###autoload
(defun entropy/grom-toggle-read-only (&optional readonly editted current-buffer-ndwp cury)
  "Toggle readonly-or-not for all buffers except for the
buffer-name witin the full buffer name regexp matching by
`entropy/grom-buffer-special-p'.

There's four optional argument for this function:

- *readonly and editted:* If set one of them to 't' then means
  'toggle read only' or 'toggle editted' independently

- *current-buffer-ndwp:* This set for prompting whether do the
  main specific function in current buffer when it set to nil . It
  means that not do with current buffer and be with prompt. This
  set will cover cury setting when it's be nil.

- *cury:* Do with current buffer without prompt if set it and
  =current-buffer-ndwp= to t,
  opposite means that not do current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-buffer-name (buffer-name current-buffer))
         (candidate '("global read only" "global editted"))
         (read
          (if (not (or readonly editted))
              (completing-read "Please choose read-only or editted:" candidate nil t)
            (cond
             ((and (eq readonly t)
                   (eq editted nil))
              "global read only")
             ((and (eq readonly nil)
                   (eq editted t))
              "global editted")
             ((and (eq readonly t)
                   (eq editted t))
              (error "Can not double true for readonly and editted!"))))))
    (let ((buffer-name-list (mapcar (function buffer-name) (buffer-list))))
      (dolist (value (copy-tree buffer-name-list))
        (when (entropy/grom-buffer-special-p value)
          (setq buffer-name-list (delete value buffer-name-list))))
      (let ((cury-did-for-p
             (if current-buffer-ndwp cury
               (yes-or-no-p "Whether do with current buffer?"))))
        (dolist (buffname buffer-name-list)
          (with-current-buffer (get-buffer buffname)
            (when
                (not (eq major-mode 'dired-mode))
              (cond
               ((string= read "global read only")
                (when (not buffer-read-only)
                  (if (not (string= current-buffer-name buffname))
                      (entropy/grom--enable-read-only :no-check t)
                    (when cury-did-for-p
                      (entropy/grom--enable-read-only :no-check t)))))
               ((string= read "global editted")
                (when buffer-read-only
                  (if (not (string= current-buffer-name buffname))
                      (entropy/grom--disable-read-only :no-check t)
                    (when cury-did-for-p
                      (entropy/grom--disable-read-only :no-check t)))))))))))
    ))

;;;###autoload
(defun entropy/grom-quick-readonly-global ()
  "Do readonly for all buffers include current-buffer.

This function are basically rely on `entropy/grom-toggle-read-only'."
  (interactive)
  (entropy/grom-toggle-read-only t nil t t)
  (message "All buffers have been read-only!"))

;;;; Global read only excepted feature
;;;;; Adjust org mode
;;;;;; library

(defvar entropy/grom--org-agenda-files-true-name-list nil
  "This list contains the FILENAME of `org-agenda-files' and
obtained by `entropy/grom-get-true-agenda-file-name-list'")

(defvar entropy/grom--org-agenda-actived-buffer-list nil
  "This list contains buffer name BUFFERNAME which mirrored with
`org-agenda-files' in current frame, and optained by
`entropy/grom--get-actived-agenda-buffers-list'")


(defvar entropy/grom--current-agenda-buffer nil
  "Current manipulation agenda buffer with `org-agenda'.

Note: Don't manually assign value to this variable.

This variable will be auto-clean when agenda manipulation
finished.")

(defun entropy/grom--get-true-agenda-files-name-list (&rest _)
  "Getting FILENAME of `org-agenda-files' and push them in
`entropy/grom--org-agenda-files-true-name-list'

This function was used for
`entropy/grom--get-actived-agenda-buffers-list'.
"
  (setq entropy/grom--org-agenda-files-true-name-list nil)
  (let ((agenda-files org-agenda-files))
    (dolist (oname agenda-files)
      (let((nname (file-truename oname)))
        (add-to-list 'entropy/grom--org-agenda-files-true-name-list nname)))))

(defun entropy/grom--get-actived-agenda-buffers-list (&rest _)
  "Getting activated `org-agenda-files' true name of `buffer-list'
in current frame."
  (setq entropy/grom--org-agenda-actived-buffer-list nil)
  (entropy/grom--get-true-agenda-files-name-list)
  (entropy/grom--get-true-buffer-name-pair-list)
  (dolist (aname entropy/grom--org-agenda-files-true-name-list)
    (dolist (bname entropy/grom-buffer-true-name-pair-list)
      (when (stringp (car bname))
        (when (string-match-p (regexp-quote aname) (car bname))
          (add-to-list 'entropy/grom--org-agenda-actived-buffer-list (cdr bname)))))))

(defun entropy/grom--unlock-actived-agenda-file-buffers (&rest _)
  "Unlock all agenda files"
  (entropy/grom--get-actived-agenda-buffers-list)
  (dolist (buffer entropy/grom--org-agenda-actived-buffer-list)
    (with-current-buffer buffer
      (if buffer-read-only
          (entropy/grom--readonly-1&0 0)))))

(defun entropy/grom--agenda-unlock-current-entry (&rest _)
  "Unlock current entry in agenda view panel when global
readonly mode is on."
  (if (and
       (string= entropy/grom-readonly-type "all")
       (equal major-mode 'org-agenda-mode))
      (let ((etb (or (if (org-get-at-bol 'org-hd-marker)
                         (buffer-name
                          (marker-buffer (org-get-at-bol 'org-hd-marker))))
                     entropy/grom--current-agenda-buffer)))
        (if (not etb)
            (progn
              (org-agenda-redo-all)
              (setq etb (buffer-name (marker-buffer (org-get-at-bol 'org-hd-marker))))))
        (save-excursion
          (with-current-buffer etb
            (if buffer-read-only
                (progn
                  (entropy/grom--readonly-1&0 0)
                  (message "Unlock buffer '%s' !" etb))
              (message "No need to unlock buffer '%s' -v-" etb))))
        (setq entropy/grom--current-agenda-buffer etb))))

(defun entropy/grom--agenda-lock-current-entry (&rest _)
  "Relock current agenda entry by manipulation according to
`entropy/grom--current-agenda-buffer'."
  (when (and (equal major-mode 'org-agenda-mode)
             (not (null entropy/grom--current-agenda-buffer))
             (buffer-live-p (get-buffer entropy/grom--current-agenda-buffer)))
    (let ((etb entropy/grom--current-agenda-buffer))
      (with-current-buffer etb
        (if (not buffer-read-only)
            (entropy/grom--enable-read-only))))
    (setq entropy/grom--current-agenda-buffer nil)))

;;;;;; enable org grom patch
;;;;;;; main
(defun entropy/grom--org-patch-init ()
;;;;;;;; agenda function advice for unlock current entry
  (when
      (or (string= entropy/grom-readonly-type "all")
          (string= entropy/grom-readonly-type "modes"))
    (entropy/grom--with-require-feature 'org-agenda
      (define-key org-agenda-mode-map (kbd "C-d")
        #'entropy/grom--unlock-actived-agenda-file-buffers)

      ;; org-agenda-todo advice
      (entropy/grom--add-advice
       'org-agenda-todo
       :before #'entropy/grom--agenda-unlock-current-entry)
      (entropy/grom--add-advice
       'org-agenda-todo
       :after #'entropy/grom--agenda-lock-current-entry)


      ;; ============note behaviour ==============
      ;; -----------------------------------------
      ;; org-agenda-add-not advice
      (entropy/grom--add-advice
       'org-agenda-add-note :before #'entropy/grom--agenda-unlock-current-entry)

      ;; org-add-log-note
      (entropy/grom--add-advice
       'org-add-log-note :before #'entropy/grom--agenda-unlock-current-entry)

      ;; org-sore-log-note
      (entropy/grom--add-advice
       'org-store-log-note :after #'entropy/grom--agenda-lock-current-entry)))


;;;;;;;; Redefine org-capture about function for adapting for global-readonly-mode
  (when
      (or (string= entropy/grom-readonly-type "all")
          (string= entropy/grom-readonly-type "modes"))
    (entropy/grom--with-require-feature 'org-capture
      (defun entropy/grom--org-capture-place-template (&optional inhibit-wconf-store)
        "Insert the template at the target location, and display
the buffer.  When `inhibit-wconf-store', don't store the window
configuration, as it may have been stored before. (This function
was the override function for `org-capture-place-template' by
`entropy-global-read-only-mode')"
        (unless inhibit-wconf-store
          (org-capture-put :return-to-wconf (current-window-configuration)))
        (delete-other-windows)
        (org-switch-to-buffer-other-window
         (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
        (widen)
        (outline-show-all)
        (goto-char (org-capture-get :pos))
        (setq-local outline-level 'org-outline-level)

        ;; Force un-readonly buffer
        (if buffer-read-only
            (entropy/grom--readonly-1&0 0))

        (pcase (org-capture-get :type)
          ((or `nil `entry) (org-capture-place-entry))
          (`table-line (org-capture-place-table-line))
          (`plain (org-capture-place-plain-text))
          (`item (org-capture-place-item))
          (`checkitem (org-capture-place-item)))
        (org-capture-mode 1)
        (setq-local org-capture-current-plist org-capture-plist))

      (entropy/grom--add-advice 'org-capture-place-template :override
                                #'entropy/grom--org-capture-place-template)

      (defun entropy/grom--org-capture-put-target-region-and-position-after-advice
          (&rest _)
        "Adding buffer unlock for narrowed org capture buffer.

This func was advice for func
`org-capture-put-target-region-and-position'."
        (when buffer-read-only
          (entropy/grom--readonly-1&0 0)))

      (entropy/grom--add-advice
       'org-capture-put-target-region-and-position
       :after #'entropy/grom--org-capture-put-target-region-and-position-after-advice))

    (entropy/grom--with-require-feature 'org-datetree
      (defun entropy/grom--org-datetree-before-advice ()
        (if buffer-read-only
            (entropy/grom--readonly-1&0 0)))
      (entropy/grom--add-advice
       'org-datetree--find-create
       :before
       #'entropy/grom--org-datetree-before-advice))))


;;;;;;; add to external patch

(add-to-list 'entropy/grom--external-patch-register
             '(:init
               entropy/grom--org-patch-init
               :setoff nil))

;;;; mode defination
(defun entropy/grom--mode-enable ()
  "Enable entropy-grom-mode."
  (progn
    (cond
     ((string= entropy/grom-readonly-type "modes")
      (dolist (mode-hook entropy/grom-mode-hooks-list)
        (entropy/grom--add-hook mode-hook #'entropy/grom--enable-read-only)))
     ((string= entropy/grom-readonly-type "all")
      (entropy/grom--add-hook 'find-file-hook 'entropy/grom--enable-read-only)))
    (entropy/grom--enable-patcher)
    (message "Global read only mode enable!")))

(defun entropy/grom--mode-disable ()
  "Disable entropy-grom-mode."
  (progn
    (entropy/grom--remove-hook-and-advice)
    (entropy/grom--disable-patcher)
    (message "Global read only mode disable!")))

;;;###autoload
(define-minor-mode entropy-grom-mode
  "Global minor mode for buffer-readonly for all."
  :init-value nil
  :lighter "GROM"
  :global t
  (if entropy-grom-mode
      (entropy/grom--mode-enable)
    (entropy/grom--mode-disable)))

;;; provide
(provide 'entropy-global-read-only-mode)
