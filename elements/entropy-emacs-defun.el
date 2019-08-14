;;; entropy-emacs-defun.el --- entropy emacs pre-defined libraries
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defun.el
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
;; This file was built on the top level concept of entropy-emacs's
;; designation, as what do with `entropy-emacs-defcustom' and
;; `entropy-emacs-defconst'.
;;
;; For be as the underlying project function library hosting
;; master. Every part of this file can be sharing on the context and
;; splitting into categories individually with using outline mode
;; doc's context format.
;; 
;; * Configuration:
;; 
;; Just requiring it before checking the file dependencies. 
;; 
;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-defcustom)

;; ** lazy load branch
(defun entropy/emacs-select-x-hook ()
  (if entropy/emacs-minimal-start
      'entropy/emacs-init-mini-hook
    'entropy/emacs-init-X-hook))

(defmacro entropy/emacs-lazy-load-simple (file &rest body)
  "Execute BODY after/require FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
  (declare (indent 1) (debug t))
  (cond
   (entropy/emacs-custom-enable-lazy-load
    `(eval-after-load ,file (lambda () ,@body)))
   ((null entropy/emacs-custom-enable-lazy-load)
    `(progn
       (require ,file)
       ,@body))))


(defun entropy/emacs-lazy-initial-form
    (list-var initial-func-suffix-name initial-var-suffix-name abbrev-name adder-name &rest form_args)
  (let* ((func (intern (concat abbrev-name "_" initial-func-suffix-name)))
         (adder-func (intern (concat (symbol-name func) "_" adder-name)))
         (var (intern (concat abbrev-name "_+" initial-var-suffix-name)))
         (adder-type (car form_args))
         (adder-flag (cadr form_args))
         (func-body (nth 2 form_args)))
    `(progn
       (defvar ,var nil)
       (defun ,func (&rest _)
         (unless ,var
           ,@func-body
           (setq ,var t)))
       (defun ,adder-func ()
         (dolist (item ,list-var)
           (if (not (null ,adder-flag))
               (,adder-type item ,adder-flag ',func)
             (,adder-type item ',func))))
       (add-hook ',(entropy/emacs-select-x-hook) ',adder-func))))


(defmacro entropy/emacs-lazy-initial-for-hook
    (hooks initial-func-suffix-name initial-var-suffix-name &rest body)
  (entropy/emacs-lazy-initial-form
   hooks initial-func-suffix-name initial-var-suffix-name
   "entropy/emacs--hook-first-enable-for" "hook-adder"
   'add-hook
   nil
   body))

(defmacro entropy/emacs-lazy-initial-advice-before
    (advice-fors initial-func-suffix-name initial-var-suffix-name &rest body)
  (entropy/emacs-lazy-initial-form
   advice-fors initial-func-suffix-name initial-var-suffix-name
   "entropy/emacs--beforeADV-fisrt-enable-for"
   "before-advice-adder"
   'advice-add
   :before
   body))

;; ** file and directories
(defun entropy/emacs-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\.$\\|\\.\\.$\\)" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))


(defun entropy/emacs-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun entropy/emacs-file-path-parser (file-name type)
  "The file-path for 'entropy-emacs, functions for get base-name,
shrink trail slash, and return the parent(up level) dir."
  (let (rtn (fname (replace-regexp-in-string "\\(\\\\\\|/\\)$" "" file-name)))
    (cl-case type
      ('non-trail-slash
       (setq rtn fname))
      ('file-name
       (setq rtn
             (replace-regexp-in-string
              "^.*\\(\\\\\\|/\\)\\([^ /\\\\]+\\)$"
              "\\2"
              fname)))
      ('parent-dir
       (setq rtn (file-name-directory fname))))
    rtn))

(defun entropy/emacs-buffer-exists-p (buffername)
  "Judge whether buffer BUFFERNAME existed!"
  (let* ((bfl (mapcar 'buffer-name (buffer-list))))
    (if (-filter '(lambda (bname)
                    (if (string= buffername bname) t nil))
                 bfl)
        t
      nil)))


;; ** counter map list
(defun entropy/emacs-numberic-list (list-var)
  "Return list element mapped with numberic prefix which concated
with '0' as alignment state."
  (let* ((l-len (length list-var))
         (register l-len)
         (counter 0)
         (step 1)
         (zero-func
          (lambda (counter str)
            (let ((step (length str))
                   (rtn str))
              (while (< step counter)
                (setq rtn (concat "0" rtn)
                      step (+ 1 step)))
              rtn)))
         rtn)
    (while (not (eq 0 register))
      (setq register (/ register 10)
            counter (+ 1 counter)))
    (dolist (el list-var)
      (push (cons (funcall zero-func counter (number-to-string step))
                  el)
            rtn)
      (setq step (+ 1 step)))
    (reverse rtn)))


;; ** language environment set
(defun entropy/emacs-toggle-utf-8-and-locale (&optional cond)
  " This function was to toggle entire UTF-8 environment to or
back from locale.

Optional arg COND has four type:

- \"prompt\": call this function with ivy-read prompt for choosing the target encoding.
- \"UTF-8\": choose UTF-8 without prompt.
- \"LOCAL\": choose LOCAL language environment without prompt.
"
  (interactive)
  (if cond
      (cond
       ((string= cond "prompt")
        (ivy-read "Choice: " '("UTF-8" "LOCAL")
                  :require-match t
                  :action #'entropy/emacs-lang-set))
       ((string= cond "UTF-8") (entropy/emacs-lang-set "UTF-8"))
       ((string= cond "LOCAL") (entropy/emacs-lang-set "LOCAL"))
       (t (error "entropy/emacs-toggle-utf-8-and-locale: error argument type.")))
    (if (string= current-language-environment "UTF-8")
        (entropy/emacs-lang-set "LOCAL")
      (entropy/emacs-lang-set "UTF-8"))))

(defun entropy/emacs-lang-set (lang)
  (if (string-match-p
       "\\*e?shell\\*\\|\\*eshell-.*?\\*\\|\\(^\\*ansi-term-.*\\)\\|\\(\\*terminal\\)"
       (format "%s" (buffer-list)))
      (error "Can not use this function cause shell buffer exist, please kill it and try again!")
    (cond
     ((string= lang "UTF-8")
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8-unix)
      (message "Setting language environment to 'utf-8-unix'."))
     ((string= lang "LOCAL")
      (unless (and (not (null entropy/emacs-language-environment))
                   (stringp entropy/emacs-language-environment))
        (error "Wrong arg type for 'entropy/emacs-language-environment'."))
      (set-language-environment entropy/emacs-language-environment)
      (prefer-coding-system entropy/emacs-lang-locale)
      (setq default-file-name-coding-system 'utf-8-unix)
      (message "Setting language environment to '%s'." entropy/emacs-language-environment))
     (t (error "Invalid LANG arg")))))

(defun entropy/emacs-lang-set-utf-8 (&rest args)
  "Setting language envrionment to unix-8-unix, supported
by `entropy/emacs-lang-set'"
  (if (not (string= current-language-environment "UTF-8"))
      (entropy/emacs-lang-set "UTF-8")))

(defun entropy/emacs-lang-set-local (&rest args)
  "Setting language environment to `locale' specification from
`entropy/emacs-language-environment'. "
  (if (and entropy/emacs-custom-language-environment-enable
           (not (null entropy/emacs-language-environment)))
      (entropy/emacs-lang-set "LOCAL")))

(defun entropy/emacs-revert-buffer-with-custom-language-environment ()
  "This function was designed to auto revert buffer with
language-environment you set in `entropy/emacs-language-environment'."
  (interactive)
  (if (string= current-language-environment "UTF-8")
      (progn
        (entropy/emacs-toggle-utf-8-and-locale)
        (revert-buffer t t)
        (message "Succeed revert buffer with %s" entropy/emacs-language-environment))
    (error "Have been locale setting ♘")))


;; around advice when `entropy/emacs-custom-language-environment-enable' was nil

(defun entropy/emacs-lang-set-without-enable (oldfunc &rest args)
  "Around advice for funcs:

- entropy/emacs-toggle-utf-8-and-locale
- entropy/emacs-lang-set-utf-8
- entropy/emacs-revert-buffer-with-custom-language-environment

This func will force disable each func's internal procedure when
custom variable
`entropy/emacs-custom-language-environment-enable' and
`entropy/emacs-language-environment' was nil.
 "
  (cond
   ((and entropy/emacs-custom-language-environment-enable
         (ignore-errors (stringp entropy/emacs-language-environment)))
    (apply oldfunc args))
   ((or (null entropy/emacs-custom-language-environment-enable)
        (not (ignore-errors (stringp entropy/emacs-language-environment))))
    t)))

(dolist (el '(entropy/emacs-toggle-utf-8-and-locale
              entropy/emacs-revert-buffer-with-custom-language-environment
              entropy/emacs-lang-set))
  (advice-add el :around #'entropy/emacs-lang-set-without-enable))

;; common around advice for wrapper function into utf-8 environment
(defun entropy/emacs-lang-set-utf-8-around-wrapper (old-func &rest _)
  (unless (string= current-language-environment "UTF-8")
    (entropy/emacs-lang-set-utf-8))
  (apply old-func _))

;; common around advice for wrapper funcion into locale language environment
(defun entropy/emacs-lang-set-local-around-wrapper (old-func &rest _)
  (when (and entropy/emacs-custom-language-environment-enable)
    (entropy/emacs-lang-set-local))
  (apply old-func _))


;; ** miscellaneous
(defun entropy/emacs-transfer-wvol (file)
  "Transfer linux type root path header into windows volumn
format on windows platform."
  (if (and (string-match-p "^/[a-z]/" file)
           sys/win32p)
      (let ((wvol (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:" file)))
        (find-file wvol))
    (find-file file)))

(defun entropy/emacs-package-is-upstream ()
  "Judges whether `entropy/emacs-use-extensions-type' is based on
`package.el'."
  (or (eq entropy/emacs-use-extensions-type 'origin)
      (eq entropy/emacs-use-extensions-type 'submodules-melpa-local)))

;; ** Org face reset
(defvar entropy/emacs-defun--ohrsc-previous-theme nil)

(defconst entropy/emacs-defun--ohrsc-org-header-faces
  (list 'org-level-1
        'org-level-2
        'org-level-3
        'org-level-4
        'org-level-5
        'org-level-6
        'org-level-7
        'org-level-8))

(defconst entropy/emacs-defun--ohrsc-org-header-backup-faces
  (list 'org-level-1-backup-eemacs
        'org-level-2-backup-eemacs
        'org-level-3-backup-eemacs
        'org-level-4-backup-eemacs
        'org-level-5-backup-eemacs
        'org-level-6-backup-eemacs
        'org-level-7-backup-eemacs
        'org-level-8-backup-eemacs))

(defconst entropy/emacs-defun--ohrsc-org-header-face-spec
  '((:background . nil)
    (:weight . semi-bold)
    (:height . 1.0)
    (:inherit . nil)))

(defun entropy/emacs-defun--ohrsc-cancel-org-header-face-scale ()
  (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
    (dolist (spc entropy/emacs-defun--ohrsc-org-header-face-spec)
      (set-face-attribute
       face nil
       (car spc) (cdr spc)))))

(defun entropy/emacs-defun--ohrsc-recovery-org-header-face-scale ()
  (let ((count 0))
    (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
      (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
        (dolist (spc entropy/emacs-defun--ohrsc-org-header-face-spec)
          (set-face-attribute
           face nil
           (car spc) (face-attribute face-bcp (car spc)))))
      (cl-incf count))))

(defun entropy/emacs-defun--ohrsc-org-header-face-backuped-p ()
  (let (judge)
    (setq judge
          (catch :exit
            (dolist (face entropy/emacs-defun--ohrsc-org-header-backup-faces)
              (unless (facep face)
                (throw :exit 'lost)))))
    (if (eq judge 'lost)
        nil
      t)))

(defun entropy/emacs-defun--ohrsc-org-header-faces-modified-p ()
  (let ((count 0)
        rtn)
    (setq rtn (catch :exit
                (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
                  (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
                    (unless (face-equal face face-bcp)
                      (throw :exit 'modified))
                    (cl-incf count)))))
    (if (eq rtn 'modified)
        t
      nil)))

(defun entropy/emacs-defun--ohrsc-backup-org-header-face ()
  (let ((count 0))
    (dolist (face entropy/emacs-defun--ohrsc-org-header-faces)
      (let ((face-bcp (nth count entropy/emacs-defun--ohrsc-org-header-backup-faces)))
        (copy-face face face-bcp))
      (cl-incf count))))

(defun entropy/emacs-adjust-org-heading-scale ()
  "Stop the org-level headers from increasing in height
relative to the other text when
`entropy/emacs-disable-org-heading-scale' was non-nil."
  (when (null entropy/emacs-defun--ohrsc-previous-theme)
    (setq entropy/emacs-defun--ohrsc-previous-theme
          entropy/emacs-theme-sticker))
  (when (and (not (null entropy/emacs-defun--ohrsc-previous-theme))
             (not (equal entropy/emacs-theme-sticker
                         entropy/emacs-defun--ohrsc-previous-theme)))
    (entropy/emacs-defun--ohrsc-backup-org-header-face)
    (setq entropy/emacs-defun--ohrsc-previous-theme
          entropy/emacs-theme-sticker))
  (cond
   (entropy/emacs-disable-org-heading-scale
    (unless (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
      (entropy/emacs-defun--ohrsc-backup-org-header-face))
    (entropy/emacs-defun--ohrsc-cancel-org-header-face-scale))
   ((null entropy/emacs-disable-org-heading-scale)
    (when (and (entropy/emacs-defun--ohrsc-org-header-face-backuped-p)
               (entropy/emacs-defun--ohrsc-org-header-faces-modified-p))
      (entropy/emacs-defun--ohrsc-recovery-org-header-face-scale)))))

;; ** theme loading specific
(defun entropy/emacs-theme-load-face-specifix (&optional x)
  "Advice for `counsel-load-theme-action' that setting face of
`ivy-current-match' for spacemacs themes.

Reason of this setting was that spacemacs has the un-obviouse
visual distinction of `ivy-current-match' covered upon the
`ivy-minibuffer-match-highlight'."
  (unless x
    (setq x (symbol-name entropy/emacs-theme-sticker)))
  (cond
   ((string-match-p "spacemacs-dark" x)
    (set-face-attribute 'ivy-current-match nil
                        :background "purple4" :bold t))
   ((string-match-p "spacemacs-light)" x)
    (set-face-attribute 'ivy-current-match nil
                        :background "salmon" :bold t))
   ((string-match-p "darkokai" x)
    (set-face-attribute 'ivy-current-match nil
                        :background "#65a7e2"))
   ((string-match-p "\\(tsdh\\|whiteboard\\|adwaita\\)" x)
    (if (equal 'dark (frame-parameter nil 'background-mode))
        (set-face-attribute 'ivy-current-match nil
                            :background "#65a7e2" :foreground "black")
      (set-face-attribute 'ivy-current-match nil
                          :background "#1a4b77" :foreground "white")))
   ((string= "doom-solarized-light" x)
    (when (not (featurep 'hl-line))
      (require 'hl-line))
    (set-face-attribute 'hl-line nil :background "moccasin"))
   (t
    (entropy/emacs-set-fixed-pitch-serif-face-to-monospace))))

(defun entropy/emacs-theme-load-modeline-specifix (&optional arg)
  "Advice of auto refresh doom-modeline bar background color
when changing theme."
  (unless arg
    (setq arg (symbol-name entropy/emacs-theme-sticker)))
  (progn
    (cond ((and (string= entropy/emacs-mode-line-sticker "doom")
                (string-match-p "\\(ujelly\\)" arg))
           (set-face-attribute 'doom-modeline-bar
                               nil :background "black")
           (doom-modeline-refresh-bars))
          ((and (string= entropy/emacs-mode-line-sticker "doom")
                (string-match-p "\\(spolsky\\)" arg))
           (setq doom-modeline--bar-active
                 (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                          doom-modeline-bar-width
                                          doom-modeline-height)))
          ((string= entropy/emacs-mode-line-sticker "doom")
           (set-face-attribute 'doom-modeline-bar
                               nil :background (face-background 'mode-line nil t))
           (doom-modeline-refresh-bars)))))

;; ** advice around for case-fold-search
(defun entropy/emacs-case-fold-focely-around-advice (_old_func &rest _args)
  "Wrapper function to disable `case-fold-search' functional ability."
  (let ((_case_type case-fold-search)
        rtn)
    (unwind-protect
        (progn (setq case-fold-search nil)
               (setq rtn (apply _old_func _args))
               (setq case-fold-search _case_type)
               rtn)
      (setq case-fold-search _case_type))))


;; ** common face setting
(defun entropy/emacs-set-fixed-pitch-serif-face-to-monospace ()
  "Set info-mode font-lock spec face `fixed-pitch-serif' to
entropy-emacs specific monospace style.

This funciton will solve the problem that the symbol pattern
display ugly and small in info-mode."
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Monospace" :slant 'italic))

;; ** solaire-mode adapted themes judgement
(defun entropy/emacs-theme-adapted-to-solaire ()
  "Judge whether current theme loaded adapted to `solaire-mode',
return t otherwise for nil. "
  (let ((theme_cur (symbol-name entropy/emacs-theme-sticker)))
    (catch :exit
      (dolist (regex entropy/emacs-solaire-themes-regex-list)
        (when (ignore-errors (string-match-p regex theme_cur))
          (throw :exit t))))))

;; ** provide
(provide 'entropy-emacs-defun)
