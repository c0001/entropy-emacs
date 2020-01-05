;;; entropy-emacs-basic.el --- entropy emacs basic config
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-basic.el
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
;; This file was the `entropy-emacs' basic config file, config of set
;; of emacs internal features and using some extra extensions to
;; enhance the basic emacs experience.
;;
;; Also given the simple workspace management based on `eyebrowse'
;; and hacks of entropy-emacs internally for derived workspace
;; environment.
;;
;; File using `outline mode' to manage the context level and the
;; categories. 
;;
;; This file were enabled for 'Mini mode' (specified of
;; `entropy/emacs-minimal-start') and well for the full startup
;; type. Set of auto-start feature in this file are injecting into
;; hook `entropy/emacs-init-mini-hook' checking this hook for viewing
;; the features loading at start-up time.
;;
;; * Configuration:
;;
;; This file must loaded by `entorpy-emacs.el', other testing way is
;; not in the designation context.
;;
;; 
;; * Code:
;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defface)

(entropy/emacs-lazy-with-load-trail
 top-keybinding
 (define-key (current-global-map)
   (kbd entropy/emacs-top-key)
  entropy/emacs-top-keymap))

;; ** Temporal bug revert
;; *** gnutls bug for emacs version upper than '26.1'
;; 
;; Bug refer emacs `url.el' bug or possible for the gnutls bug override.
;;
;; Refer:
;; @see https://github.com/magit/ghub/issues/81#issuecomment-488660597
;; For now [2019-08-08 Thu 19:23:42] it seems occur on w32 port only
(when (and (version< "26.1" emacs-version)
           sys/win32p)
  (advice-add #'gnutls-available-p :override #'ignore))

;; ** Personal infomation
(when (and entropy/emacs-user-full-name
           entropy/emacs-user-mail-address)
  (setq user-full-name entropy/emacs-user-full-name)
  (setq user-mail-address entropy/emacs-user-mail-address))


;; *** Show the column numberic in modeline
(setq column-number-mode t)

;; *** Set default cursor style
(setq-default cursor-type t)

(defun entropy/emacs-basic-toggle-cursor-type ()
  (interactive)
  (if (string= (symbol-name cursor-type) "t")
      (setq cursor-type 'bar)
    (setq cursor-type t)))

;; *** Global display line number mode
(if (>= emacs-major-version 26)
    (progn
      (setq-default display-line-numbers-width-start t)
      (when entropy/emacs-init-display-line-mode
        (global-display-line-numbers-mode))))

;; ** Backup setting
(setq-default auto-save-default nil)    ;disable it for preventing typing lagging
(setq make-backup-files nil)

;; ** Scratch buffer corresponding file
;; 
;;     Amounts of company backend function can not functional
;;     auto-completion in none file buffer, so corresponding one file
;;     to *scratch* buffer.

(defun entropy/emacs-basic--scratch-buffer-file-binding ()
  "Corresponded *scratch* buffer to one temp-file.

  Filename are \".scratch_entropy\" in HOME.
  "
  (let ((bfn "*scratch*"))
    (if (entropy/emacs-buffer-exists-p "*scratch*")
        (kill-buffer "*scratch*"))
    
    (let ((fname (expand-file-name ".scratch_entropy" entropy/emacs-stuffs-topdir)))
      (if (not (file-exists-p fname))
          (progn
            (write-region "" "" fname)
            (with-current-buffer (find-file-noselect fname)
              (if buffer-read-only (read-only-mode 0))
              (auto-save-mode 0)
              (erase-buffer)
              (rename-buffer "*scratch*")
              (lisp-interaction-mode)
              (insert initial-scratch-message)))
        (with-current-buffer (find-file-noselect fname)
          (if buffer-read-only (read-only-mode 0))
          (auto-save-mode 0)
          (rename-buffer "*scratch*")
          (lisp-interaction-mode)
          (goto-char (point-min))
          (unless (re-search-forward (regexp-quote initial-scratch-message) nil t)
            (insert initial-scratch-message)))))
    bfn))

(unless entropy/emacs-fall-love-with-pdumper
  (entropy/emacs-lazy-load-simple 'entropy-emacs-structure
    (entropy/emacs-basic--scratch-buffer-file-binding)))

;; Create a new scratch buffer
(defun entropy/emacs-tools-create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (entropy/emacs-basic--scratch-buffer-file-binding))
  (lisp-interaction-mode)
  (message "Create *scratch* buffer"))

;; ** Highlight current line
(defun entropy/emacs-basic--dhl-judge-state ()
  (let ((hlmp (ignore-errors hl-line-mode))
        (dlmp display-line-numbers)
        rtn)
    (setq rtn (list hlmp dlmp))
    (cond
     ((equal rtn '(nil nil))
      (setq rtn 1))
     ((equal rtn '(nil t))
      (setq rtn 2))
     ((equal rtn '(t nil))
      (setq rtn 3))
     ((equal rtn '(t t))
      (setq rtn 4)))
    rtn))

(defun entropy/emacs-basic-dhl-toggle ($Prefix)
  (interactive "P")
  (if (not (version< emacs-version "26.1"))
      (if (not $Prefix)
          (cl-case (entropy/emacs-basic--dhl-judge-state)
            (1
             (hl-line-mode 1))
            (2
             (hl-line-mode 1))
            (3
             (hl-line-mode 0))
            (4
             (hl-line-mode 0)))
        (cl-case (entropy/emacs-basic--dhl-judge-state)
          (1
           (hl-line-mode 1)
           (display-line-numbers-mode 1))
          (2
           (hl-line-mode 1))
          (3
           (display-line-numbers-mode 1))
          (4
           (hl-line-mode 0)
           (display-line-numbers-mode 0))))
    (if (not (ignore-errors hl-line-mode))
        (hl-line-mode 1)
      (hl-line-mode 0))))
      
(global-set-key (kbd "<f2>") 'entropy/emacs-basic-dhl-toggle)

;; ** Smooth scrolling
(defvar entropy/emacs-basic-smooth-scrolling-mode nil
  "Indicated whether smooth-scrolling enable.

Note:

Manually edit this variable will not be any effection.")

(setq scroll-step 0)
(setq scroll-conservatively 0)

(defun entropy/emacs-basic-smooth-scrolling ()
  "Toggle smooth-scrolling buffer scrolling view."
  (interactive)
  (setq redisplay-dont-pause t
        scroll-margin 0
        scroll-step   (if (equal scroll-step 0) 1 0)
        scroll-conservatively (if (equal scroll-conservatively 0) 10000 0))
  (if (and (equal scroll-step 1)
           (equal scroll-conservatively 10000))
      (progn
        (setq entropy/emacs-basic-smooth-scrolling-mode t)
        (message "Smooth scrolling enabled!"))
    (progn
      (setq entropy/emacs-basic-smooth-scrolling-mode nil)
      (message "Smooth scrolling disabled!"))))

(entropy/emacs-basic-smooth-scrolling)

;; ** Kill-buffer-and-window function
(defvar entropy/emacs-basic--kill-buffer-excluded-buffer-list nil
  "Buffer name regexp list stored excluded buffer name match for
  func `entropy/emacs-kill-buffer-and-window'.")

(defvar entropy/emacs-basic--kill-buffer-special-buffer-list
  '(("\\*eshell-?" . (lambda ()
                       (kill-this-buffer)))
    ("\\*anaconda-" . (lambda ()
                        (error
                         "You couldn't kill this buffer, as it will cause some problem.")))
    (("\\.cp?p?" "\\.h" "\\.el" "\\.py" "\\.php" "\\.js" "\\.html" "\\.css" "\\.sh" "\\.org"
      "\\.txt" "\\.md")
     .
     (lambda ()
       (let ((buffn (buffer-name))
             (base-dir default-directory)
             (fname (buffer-file-name)))
         (kill-buffer buffn)
         (when (and (ignore-errors (stringp fname))
                    (file-writable-p fname))
           (dired base-dir)))))
    ("ansi-term"
     .
     (lambda ()
       "The patch for the bug of error after kill ansi-term
buffer and its popup window refer to bug
#h-0c3ab89e-a470-42d2-946e-4f217ea2f20c in entropy-emacs bug
collection."
       (if sys/linuxp
           (let* ((_buff (current-buffer))
                  (_proc (get-buffer-process _buff)))
             (when _proc
               (when (yes-or-no-p
                      (format "Buffer %S has a running process; kill it? "
                              (buffer-name _buff)))
                 (set-process-filter _proc nil)
                 (kill-process _proc)
                 (let ((kill-buffer-query-functions '((lambda () t))))
                   (if (not (one-window-p))
                       (kill-buffer-and-window)
                     (kill-this-buffer))))))
         (kill-this-buffer)))))
  "Special buffer alist which the car can be regexp string or
  list of regexp string, the cdr was buffer killing specific
  func. ")

(defun entropy/emacs-basic-kill-buffer-and-window ()
  "Kill buffer and window following rule by
`entropy/emacs-basic--kill-buffer-excluded-buffer-list' and
`entropy/emacs-basic--kill-buffer-special-buffer-list'.

Using func `entropy/emacs-basic--buffer-close' be the default func."
  (interactive)
  (let ((excluded entropy/emacs-basic--kill-buffer-excluded-buffer-list)
        (special entropy/emacs-basic--kill-buffer-special-buffer-list)
        excl_p special_p
        (buffn (buffer-name)))
    (when excluded
      (mapc (lambda (regx)
              (when (not excl_p)
                (when (string-match-p regx buffn)
                  (setq excl_p t))))
            excluded))
    (cond
     (excl_p
      (entropy/emacs-basic--buffer-close))
     (t
      (mapc (lambda (mod_l)
              (let ((regx (car mod_l))
                    (predicate (cdr mod_l)))
                (when (not special_p)
                  (cond
                   ((listp regx)
                    (catch :exit
                      (dolist (el regx)
                        (when (string-match-p el buffn)
                          (setq special_p
                                (cons el predicate))
                          (throw :exit nil)))))
                   (t
                    (when (string-match-p regx buffn)
                      (setq special_p
                            (cons regx predicate))))))))
            special)
      (if special_p
          (funcall (cdr special_p))
        (entropy/emacs-basic--buffer-close))))))

(global-set-key (kbd "C-x k") 'entropy/emacs-basic-kill-buffer-and-window)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

(defun entropy/emacs-basic--buffer-close ()
  "Kill buffer and close it's host window if windows conuts
retrieve from `window-list' larger than 1."
  (let ((buflist (window-list)))
    (if (> (length buflist) 1)
        (cond
         ((and (eq (length buflist) 2)
               (let (rtn)
                 (mapc (lambda (window)
                         (let ((buff (window-buffer window)))
                           (when (window-parameter
                                  (get-buffer-window buff)
                                  'no-delete-other-windows)
                             (setq rtn t))))
                       buflist)
                 rtn))
          (kill-buffer))
         (t
          (kill-buffer-and-window)))
      (kill-buffer))))

;; ** Kill-other-buffers
(defun entropy/emacs-basic-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; ** kill-other-windows

(defun entropy/emacs-basic-kill-other-window ()
  "Delete other window and do again using `delete-other-windows-internal' if non-effect.

This affected by `neotree' window sticking with `eyebrowse'
layout switching conflicts."
  (interactive)
  (let ((wdc (length (window-list)))
        neo-exist)
    (unless (eq wdc 1)
      (delete-other-windows)
      (when (and (= wdc (length (window-list)))
                 (bound-and-true-p neo-buffer-name)
                 (not (let (_var rtn)
                        (setq _var (mapcar
                                    (lambda (x)
                                      (equal neo-buffer-name
                                             (buffer-name (window-buffer x))))
                                    (window-list)))
                        (dolist (elt _var)
                          (unless (null elt)
                            (setq rtn t)))
                        rtn)))
        (delete-other-windows-internal)))))
(global-set-key (kbd "C-x 1") #'entropy/emacs-basic-kill-other-window)

;; ** kill redundant buffer
(defun entropy/emacs-basic-kill-large-process-buffer ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p (regexp-quote "*eww") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*[萌典]") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*eshell") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*helm") (format "%s" buffer))
      (kill-buffer buffer))    
    )
  (dolist (process (process-list))
    (when (and (not (string-match-p (regexp-quote "shell") (format "%s" process)))
               (not (string-match-p (regexp-quote "ansi-term") (format "%s" process)))
               (not (string-match-p (regexp-quote "terminal") (format "%s" process))))
               (delete-process process))))
(entropy/emacs-!set-key (kbd "0") 'entropy/emacs-basic-kill-large-process-buffer)

;; ** Set defualt tab size
(if entropy/emacs-custom-tab-enable
    (setq-default tab-width entropy/emacs-custom-tab-width)
  (setq-default indent-tabs-mode nil))

;; ** Setting language encoding environment
(setq system-time-locale "C") ;Use english format time string
;; *** Default using UTF-8 encoding for basic environment when `entropy/emacs-custom-language-environment-enable' was nil
(unless (and entropy/emacs-custom-language-environment-enable
             (ignore-errors (stringp entropy/emacs-language-environment)))
  (progn
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8-unix)
    (set-default-coding-systems 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)))

;; *** When `entropy/emacs-custom-language-environment-enable' was t using customized basic encoding system.
(when (and entropy/emacs-custom-language-environment-enable
           (ignore-errors (stringp entropy/emacs-language-environment)))
  (set-language-environment entropy/emacs-language-environment)
  (setq default-file-name-coding-system 'utf-8-unix))

;; setting w32 shell lang env
(when sys/win32p
  (when entropy/emacs-win-env-lang-enable
    (setenv "LANG" entropy/emacs-win-env-lang-set)))

;; **** Specific cases to forceing using UTF-8 encoding environment
;; ***** Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ***** Force setting specific file type which must be opened with utf-8-unix encoding system. 
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.md\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.html\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.css\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.c\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.php\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.js\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.sh\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.el\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.bat\\'" 'utf-8-unix)
;; ========================================================================
;; Prompt: If you want all file to be one coding system you should do below
;;(modify-coding-system-alist 'file "" 'utf-8-unix)
;; ========================================================================


;; ***** let diff-buffer-with-file force run with unicode language environment
(advice-add 'diff-buffer-with-file :before #'entropy/emacs-lang-set-utf-8)

;; ** Auto wrap line
(setq-default truncate-lines t)
(global-set-key(kbd "C-<f9>") 'toggle-truncate-lines)
(setq org-startup-truncated t)

;; ** Dired config
;; *** dired basic
(use-package dired
  :ensure nil
  :init
;; **** Delete directory with force actions
  (setq entropy/emacs-basic--dired-delete-file-mode-map (make-sparse-keymap))
  (define-minor-mode entropy/emacs-basic--dired-delete-file-mode
    "Minor mode for func `entropy/emacs-basic-dired-delete-file-recursive'."
    :keymap 'entropy/emacs-basic--dired-delete-file-mode-map
    :global nil)

  (defvar entropy/emacs-basic--dired-file-current-delete nil
    "Current file pre deleted by
`entropy/emacs-basic-dired-delete-file-recursive'.")
  
  (defvar entropy/emacs-basic--dired-delete-file-refer-files nil
    "Files buffer killed by
`entropy/emacs-basic-dired-delete-file-recursive' log variable.")

  (defvar entropy/emacs-basic--dired-delete-file-refer-dired-buffers nil
    "Dired buffer killed by `entropy/emacs-basic--dired-delete-file-rescursie'
log variable.")
  
  (defun entropy/emacs-basic--dired-redelete-file ()
    "Redeletting file specified by variable
`entropy/emacs-basic--dired-file-current-delete'."
    (interactive)
    (kill-buffer)
    (entropy/emacs-basic-dired-delete-file-recursive entropy/emacs-basic--dired-file-current-delete))

  (defun entropy/emacs-basic--dired-delete-file-prompt (files-list)
    "popup buffer to deleting with prompting and return the
condition state for whether be continuing rest process."
    (if (dired-mark-pop-up " *Deletions*"
                           'delete
                           files-list
                           dired-deletion-confirmer
                           (format "%s %s " "Deleting" (dired-mark-prompt nil files-list)))
        t
      (error "Cancel deleting files!")))

  (defun entropy/emacs-basic-dired-delete-file-recursive (&optional pre-files just-kill-refers)
    "Delete file recursively with refer buffer
cleaned (i.e. files under this dir will cleaned their linked
opened buffer within current emacs session.)

This func was binding to 'D' in `dired-mode-map' which be the
replacement for func `dired-do-delete', this func's advantageous
than it was given the deletion failed handle for responding to
some directory.

Error handle will switching to special buffer ‘*[w32-resmon]*’
buffer with minor mode `entropy/emacs-basic--dired-delete-file-mode' for
prompting for how to resolving deletions problems.

In win32 platform using 'resmon' for conflicates resolve tool.  "
    (interactive)
    (let ((base-files (cond ((and (equal major-mode 'dired-mode)
                                  (not pre-files))
                             (dired-get-marked-files))
                            ((and (not (null pre-files))
                                  (listp pre-files))
                             pre-files)
                            (t (error "Dir list invalid!"))))
          _file-type)

      (unless just-kill-refers
        (entropy/emacs-basic--dired-delete-file-prompt base-files))
      
      (dolist (file base-files)

        ;; killed refer buffers.
        (dolist (el (buffer-list))
          (let* ((buffer-file (buffer-file-name el)))
            (when (and buffer-file
                       (string-match (regexp-quote file) buffer-file))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-files
                           (cons (buffer-name el) (current-time-string)))
              (kill-buffer el))))

        ;; killed refer dired buffers
        (dolist (el dired-buffers)
          (when (string-match (regexp-quote file) (car el))
            (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-dired-buffers
                         (cons el (current-time-string)))
            (kill-buffer (cdr el))))
        
        (condition-case nil
            (when (not just-kill-refers)
              (progn
                (setq entropy/emacs-basic--dired-file-current-delete (list file))
                (cond ((f-symlink-p file)
                       (setq _file-type 'symbol_link)
                       (delete-file file))
                      ((f-file-p file)
                       (setq _file-type 'file)
                       (delete-file file))
                      ((f-directory-p file)
                       (setq _file-type 'directory)
                       (delete-directory file t)))
                (when (equal major-mode 'dired-mode)
                  (revert-buffer))
                (cl-case _file-type
                  ('symbol_link
                   (message (format "Delete symbolink '%s' done! -v-" file)))
                  ('file
                   (message (format "Delete file '%s' done! -v-" file)))
                  ('directory
                   (message (format "Delete directory '%s' done! -v-" file))))))
          (error
           (cond ((eq system-type 'windows-nt)
                  (let ((prompt-buffer (get-buffer-create "*[w32-resmon]*")))
                    (start-process-shell-command
                     "windows resource monitor"
                     prompt-buffer
                     "resmon")
                    (switch-to-buffer prompt-buffer)
                    (goto-char (point-min))
                    (entropy/emacs-basic--dired-delete-file-mode)
                    (insert
                     (format
                      (concat
                       "==============Prompt Info=================\n\n"
                       "Kill all handle refer file:\n %s\n"
                       "with filter with windows resmon!\n\n" 
                       "And try again with answer minibuffer prompts delete again\n\n"
                       "===============Prompt End=================")
                      file))
                    (entropy/emacs-basic--dired-redelete-file)))
                 (t (message "Kill all refer task and try again!"))))))))

  (defun entropy/emacs-basic-dired-delete-file-refers ()
    "Kill all refers of dired markd file of directories."
    (interactive)
    (entropy/emacs-basic-dired-delete-file-recursive nil t))
  
  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "D") 'entropy/emacs-basic-dired-delete-file-recursive)
    (define-key dired-mode-map (kbd "M-d") 'entropy/emacs-basic-dired-delete-file-refers))
  
  :config
;; **** Set unit of dired inode for human readable
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  
  (if (not sys/is-win-group)
      ;; because of windows dired list is too long so just let it in linux
      (setq dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

;; **** Always delete and copy resursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  
;; **** bind 'M-<up>' for dired updir
  (define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)
  
;; **** Improve dired files operation experience for kill opened refer buffers.
  (defun entropy/emacs-basic--kill-redundant-buffer (&rest rest-args)
    "Delete file refer redundant buffer which will cause dired
'delete' or 'rename' failed."
    (dolist (el (mapcar 'buffer-name (buffer-list)))
      (dolist (re-el '("\\*Echo Area"
                       "\\*RNC Input\\*"))
        (when (string-match-p re-el el)
          (kill-buffer el)))))
  (advice-add 'dired-do-rename :before #'entropy/emacs-basic--kill-redundant-buffer)
  (advice-add 'dired-do-flagged-delete :before #'entropy/emacs-basic--kill-redundant-buffer)
  
;; **** get both UNIX and WINDOWS style path string
  (defvar entropy/emacs-basic--get-dired-fpath-log nil)

  (defun entropy/emacs-basic-get-dired-fpath (type)
    (interactive
     (list (completing-read "Choose path string type: "
                            '("unix" "win32"))))
    (let (rtn
          (files_get (or (dired-get-marked-files)
                         (list default-directory))))
      (dolist (el files_get)
        (cond
         ((string= type "win32")
          (push (replace-regexp-in-string
                 "/"
                 "\\\\"
                 el)
                rtn))
         (t
          (push el rtn))))
      
      (cond
       ((= (length rtn) 1)
        (with-temp-buffer
          (when buffer-read-only
            (read-only-mode 0))
          (goto-char (point-min))
          (insert (car rtn))
          (kill-ring-save (point-min) (point-max))
          (message (format "Save '%s' to kill-ring." (car rtn)))))
       (t
        (setq rtn (reverse rtn)
              entropy/emacs-basic--get-dired-fpath-log rtn)
        (message "Save all path string to log variable 'entropy/emacs-basic--get-dired-fpath-log'.")))))
  
  (define-key dired-mode-map (kbd "0 w") 'entropy/emacs-basic-get-dired-fpath)

;; **** dired add load path
  (defun entropy/emacs-basic--dired-add-to-load-path ()
    (interactive)
    (let ((dir (completing-read "Choose load path adding item: "
                                'read-file-name-internal
                                nil t)))
      (unless (file-directory-p dir)
        (setq dir (file-name-directory dir)))
      (add-to-list 'load-path dir)))

  (define-key dired-mode-map (kbd "M-l") 'entropy/emacs-basic--dired-add-to-load-path)

;; **** dired backup file

  (defun entropy/emacs-basic-backup-files ()
    (interactive)
    (when (not (fboundp 'entropy/cl-backup-file))
      (require 'entropy-common-library))
    (let ((files (dired-get-marked-files)))
      (dolist (el files)
        (when (file-exists-p el)
          (entropy/cl-backup-file el)))
      (revert-buffer)))
  (define-key dired-mode-map (kbd "B") #'entropy/emacs-basic-backup-files)

;; **** dired auto revert after some operations

  (defun entropy/emacs-basic--dired-revert-advice (&rest _)
    (revert-buffer))
  (dolist (el '(dired-do-rename
                dired-do-rename-regexp
                dired-do-copy
                dired-do-copy-regexp
                dired-do-compress
                dired-do-compress-to
                dired-do-touch))
    (advice-add el :after #'entropy/emacs-basic--dired-revert-advice)))

;; **** Use dired-aux to enable dired-isearch
(use-package dired-aux :ensure nil)

;; **** Quick sort dired buffers via hydra
  ;;; bind key: `S'
(when (not sys/win32p)
  (use-package dired-quick-sort
    :if (or (executable-find "gls") (executable-find "ls"))
    :commands (dired-quick-sort-setup)
    :init (add-hook 'dired-mode-hook 'dired-quick-sort-setup)))

;; **** Use coloful dired ls
(defun entropy/emacs-basic--dired-visual-init ()
  "Init dired colorful visual featuer."
  (cond ((or (string= entropy/emacs-dired-visual-type "simple-rainbow")
             (version= emacs-version "25.3.1"))
         (use-package dired-rainbow
           :commands dired-rainbow-define dired-rainbow-define-chmod
           :init
           (dired-rainbow-define dotfiles "gray" "\\..*")

           (dired-rainbow-define web "#4e9a06" ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
					        "json" "asp" "aspx" "haml" "php" "jsp" "ts"
					        "coffee" "scss" "less" "phtml"))
           (dired-rainbow-define prog "yellow3" ("el" "l" "ml" "py" "rb" "pl" "pm" "c"
					         "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
					         "m" "cs" "mk" "make" "swift" "go" "java"
					         "asm" "robot" "yml" "yaml" "rake" "lua"))
           (dired-rainbow-define sh "green yellow" ("sh" "bash" "zsh" "fish" "csh" "ksh"
					            "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
           (dired-rainbow-define text "yellow green" ("txt" "md" "org" "ini" "conf" "rc"
					              "vim" "vimrc" "exrc"))
           (dired-rainbow-define doc "spring green" ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
					             "csv" "rtf" "wps" "pdf" "texi" "tex"
					             "odt" "ott" "odp" "otp" "ods" "ots"
					             "odg" "otg"))
           (dired-rainbow-define misc "gray50" ("DS_Store" "projectile" "cache" "elc"
					        "dat" "meta"))
           (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
					          "wmv" "mov" "3gp" "avi" "mpg" "mkv"
					          "flv" "ogg" "rm" "rmvb"))
           (dired-rainbow-define picture "purple3" ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
					            "ico" "svg" "psd" "pcd" "raw" "exif"
					            "BMP" "JPG" "PNG"))
           (dired-rainbow-define archive "saddle brown" ("zip" "tar" "gz" "tgz" "7z" "rar"
						         "gzip" "xz" "001" "ace" "bz2" "lz"
						         "lzma" "bzip2" "cab" "jar" "iso"))

           ;; boring regexp due to lack of imagination
           (dired-rainbow-define log (:inherit default :italic t) ".*\\.log"))
         (when (string= entropy/emacs-dired-visual-type "all-the-icons")
           (warn " Because you are in emacs 25.3.1, just can
using simple dired visual type, although you have seting it to
\"all-the-icons\".")))
        ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
              (not (version= emacs-version "25.3.1"))
              (or (display-graphic-p)
                  entropy/emacs-fall-love-with-pdumper))
         (when sys/win32p
           (require 'font-lock+))
         (use-package all-the-icons-dired
           :commands (all-the-icons-dired-mode)
           :hook (dired-mode . all-the-icons-dired-mode)
           :config
           (defun entropy/emacs-basic--all-the-icons-dired-display ()
             "Display the icons of files without colors in a dired buffer."
             ;; Don't display icons after dired commands (e.g insert-subdir, create-directory)
             ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/11
             (all-the-icons-dired--reset)

             (when (and (not all-the-icons-dired-displayed) dired-subdir-alist
                        (not (> (save-excursion (goto-char (point-max)) (line-number-at-pos)) 100)))
               (setq-local all-the-icons-dired-displayed t)
               (let ((inhibit-read-only t)
                     (remote-p (and (fboundp 'tramp-tramp-file-p)
                                    (tramp-tramp-file-p default-directory))))
                 (save-excursion
                   ;; TRICK: Use TAB to align icons
                   (setq-local tab-width 1)
                   (goto-char (point-min))
                   (while (not (eobp))
                     (when (dired-move-to-filename nil)
                       (insert " ")
                       (let ((file (dired-get-filename 'verbatim t)))
                         (unless (member file '("." ".."))
                           (let ((filename (file-local-name (dired-get-filename nil t))))
                             (if (file-directory-p filename)
                                 (let ((icon (cond
                                              (remote-p
                                               (all-the-icons-octicon
                                                "file-directory"
                                                :v-adjust all-the-icons-dired-v-adjust
                                                :face 'all-the-icons-dired-dir-face))
                                              ((file-symlink-p filename)
                                               (all-the-icons-octicon
                                                "file-symlink-directory"
                                                :v-adjust all-the-icons-dired-v-adjust
                                                :face 'all-the-icons-dired-dir-face))
                                              ((all-the-icons-dir-is-submodule filename)
                                               (all-the-icons-octicon
                                                "file-submodule"
                                                :v-adjust all-the-icons-dired-v-adjust
                                                :face 'all-the-icons-dired-dir-face))
                                              ((file-exists-p (format "%s/.git" filename))
                                               (all-the-icons-octicon
                                                "repo"
                                                :height 1.1 :v-adjust all-the-icons-dired-v-adjust
                                                :face 'all-the-icons-dired-dir-face))
                                              (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                                   (apply (car matcher)
                                                          (list (cadr matcher)
                                                                :face 'all-the-icons-dired-dir-face
                                                                :v-adjust all-the-icons-dired-v-adjust)))))))
                                   (insert icon))
                               (insert (all-the-icons-icon-for-file
                                        file
                                        :v-adjust all-the-icons-dired-v-adjust))))
                           (insert "\t"))))
                     (forward-line 1))))))
           (advice-add #'all-the-icons-dired--display :override #'entropy/emacs-basic--all-the-icons-dired-display)))
        ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
              (not (or (display-graphic-p)
                       entropy/emacs-fall-love-with-pdumper)))
         (setq entropy/emacs-dired-visual-type "simple-rainbow")
         (warn "
You are in terminal emacs session, can not enable
'dired-all-the-icons', enable simple-rainbow instead now.")
         (entropy/emacs-basic--dired-visual-init))
        (t (error "entropy/emacs-dired-visual-type invalid"))))

(entropy/emacs-basic--dired-visual-init)

;; **** dired-x
(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-size-limit nil)
  (setq dired-omit-extensions nil))
  
;; ** Image-mode
(use-package image-mode
  :ensure nil
  :config
  (defun entropy/emacs-basic-image-gif-warn (&optional args)
    "Warn that gif animation by large gif file will freeze
emacs."
    (if (string-match-p "\\.gif" (buffer-name))
        (if (not (y-or-n-p "Do you want to animated it? "))
            (error "Please open it with external apps!"))))
  (advice-add 'image-toggle-animation :before #'entropy/emacs-basic-image-gif-warn))

;; ** Set transparenct of emacs frame
(global-set-key (kbd "<f6>") 'entropy/emacs-basic-loop-alpha)

(defun entropy/emacs-basic-loop-alpha ()    
  (interactive)    
  (let ((h (car entropy/emacs-loop-alpha-value)))
    (funcall
     (lambda (a ab)    
       (set-frame-parameter (selected-frame) 'alpha (list a ab))    
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h)
     (car (cdr h)))
    (setq entropy/emacs-loop-alpha-value (cdr (append entropy/emacs-loop-alpha-value (list h))))))

(when entropy/emacs-init-loop-alpha
  (entropy/emacs-lazy-with-load-trail
   loop-alpha
   (entropy/emacs-basic-loop-alpha)))

;; ** Paragraph fill size
(setq-default fill-column entropy/emacs-fill-paragraph-width)

;; ** Show time on mode line
(when entropy/emacs-display-time-modeline
  ;; enable time show when  
  (display-time-mode 1)
  (setq-default display-time-interval 1)
  ;; display time with date and real time infos
  (setq display-time-day-and-date t)
  ;; 24hr format
  (setq display-time-24hr-format t)
  (setq display-time-format "%e %b %Y %H:%M:%S")
  (display-time))

;; ** Input time into buffer
(defun entropy/emacs-basic-now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))


(defun entropy/emacs-basic-today ()
  "Insert string for today's date nicely formatted in American style,
 e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; ** Read-Only-About
(use-package entropy-global-read-only-mode
  :ensure nil
  :commands (entropy-grom-mode)
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file push-button find-library-name)
   "entropy-grom"
   "entropy-grom"
   (entropy-grom-mode +1)))

;; ** Revert buffer automatically

(entropy/emacs-lazy-initial-for-hook
 '(find-file-hook)
 "GlbAutoRevertMode"
 "GlbAutoRevertMode-enabled"
 (global-auto-revert-mode +1))

;; ** Use which-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (entropy/emacs-lazy-with-load-trail which-key (which-key-mode t))
  (setq which-key-popup-type 'minibuffer))

;; ** Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :bind (("C-x u" . entropy/emacs-basic-undo-tree))
  :hook ((undo-tree-mode . (lambda () (define-key undo-tree-map (kbd "C-x u") nil))))
  :init
  (unless entropy/emacs-fall-love-with-pdumper
    (entropy/emacs-lazy-load-simple 'undo-tree
      (global-undo-tree-mode +1)))
  
  :config
  (defun entropy/emacs-basic-undo-tree ()
    (interactive)
    (if (car (window-margins))
        (progn
          (setq entropy/emacs-basic-undo-tree-margin-detective t)
          (entropy/emacs-basic-center-text-clear)
          (undo-tree-visualize))
      (progn
        (setq entropy/emacs-basic-undo-tree-margin-detective nil)
        (undo-tree-visualize))))

  (defun undo-tree-visualizer-quit ()
    "Quit the undo-tree visualizer. 

Note:

This function has redefined for adapting to
`entropy/emacs-basic-center-text'."
    (interactive)
    (unless (eq major-mode 'undo-tree-visualizer-mode)
      (user-error "Undo-tree mode not enabled in buffer"))
    (undo-tree-clear-visualizer-data buffer-undo-tree)
    ;; remove kill visualizer hook from parent buffer
    (unwind-protect
        (with-current-buffer undo-tree-visualizer-parent-buffer
          (remove-hook 'before-change-functions 'undo-tree-kill-visualizer t))
      ;; kill diff buffer, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-hide-diff))
      (let ((parent undo-tree-visualizer-parent-buffer)
            window)
        ;; kill visualizer buffer
        (kill-buffer nil)
        ;; switch back to parent buffer
        (unwind-protect
            ;; (if (setq window (get-buffer-window parent))
            ;;     (select-window window))
            (if entropy/emacs-basic-undo-tree-margin-detective
                (progn
                  (switch-to-buffer-other-window parent)
                  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                                      (/ (window-width) entropy/emacs-window-center-integer)
                                      (/ (window-width) entropy/emacs-window-center-integer)))
              (switch-to-buffer parent)))))))

;; ** Auto-sudoedit
(use-package auto-sudoedit
  :commands (auto-sudoedit-mode)
  :if (not sys/is-win-group)
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file dired)
   "autosudoedit"
   "autosudoedit"
   (auto-sudoedit-mode +1)))

;; ** Clear killring
;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/emacs-basic-clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

;; ** Mark-sexp
(entropy/emacs-!set-key (if (display-graphic-p) (kbd "C-`") (kbd "C-@")) 'set-mark-command)
(defun entropy/emacs-basic-mark-set ()
  (interactive)
  (save-excursion
    (push-mark)
    (push-mark)))
(entropy/emacs-!set-key (kbd "1") 'entropy/emacs-basic-mark-set)

;; ** Windows forbidden view-hello-file
(when sys/is-win-group
  (defun view-hello-file ()
    "Prompt emacs user do not use view-hello-file in windows operation system"
    (interactive)
    (message "Do not use view-hello-file in windows because of it will jamm windows and emacs")))

;; ** Delete file to trash
(if entropy/emacs-dired-enable-trash
    (setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder but with some
                                               ; performance bug on windows plattform.
  )

;; ** History && Recentf
(use-package saveplace
  :ensure nil
  :init
  ;; Emacs 25 has a proper mode for `save-place'
  (if (fboundp 'save-place-mode)
      (save-place-mode)
    (setq save-place t)))

(use-package recentf
  :if entropy/emacs-use-recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)
  (add-hook 'find-file-hook '(lambda () (unless recentf-mode
					  (recentf-mode)
					  (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :ensure nil
  :init (entropy/emacs-lazy-with-load-trail savehist (savehist-mode t))
  :config
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60))

;; ** Bookmarks autosave
(setq bookmark-save-flag 1)

;; *** Bookmark utf-8
(when entropy/emacs-custom-language-environment-enable
  (dolist (hook '(bookmark-edit-annotation-mode-hook
                  bookmark-bmenu-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (unless (string= current-language-environment "UTF-8")
                    (entropy/emacs-lang-set-utf-8)))))

  (use-package bookmark
    :ensure nil
    :config
    (defun bookmark-set (&optional name no-overwrite)
      "Set a bookmark named NAME at the current location.
If NAME is nil, then prompt the user.

With a prefix arg (non-nil NO-OVERWRITE), do not overwrite any
existing bookmark that has the same name as NAME, but instead push the
new bookmark onto the bookmark alist.  The most recently set bookmark
with name NAME is thus the one in effect at any given time, but the
others are still there, should the user decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)

Note: This function has been redefined for forcing using utf-8
coding-system to save bookmark infos"
      (interactive (list nil current-prefix-arg))
      (unless (string= current-language-environment "UTF-8")
        (entropy/emacs-lang-set-utf-8))
      (let ((prompt
             (if no-overwrite "Set bookmark" "Set bookmark unconditionally")))
        (bookmark-set-internal prompt name (if no-overwrite 'push 'overwrite))))))
                  
;; ** Major mode reload
(defun entropy/emacs-basic-major-mode-reload ()
  "Reload current `major-mode'.

  This function was usable for some occurrence that current mode
  functional part was missed or be without working on the proper
  way. "
  (interactive)
  (if (not (string-match-p "^CAPTURE-" (buffer-name)))
      (let ((point (point))
            (mode major-mode))
        (funcall mode)
        (when (eq major-mode 'org-mode)
          (outline-show-all))
        (goto-char point)
        (message "Reloaded current major mode '%s'!" (symbol-name major-mode)))
    (error "You can not refresh %s in this buffer, if did may cause some bug." (symbol-name major-mode))))
(global-set-key (kbd "<f7>") 'entropy/emacs-basic-major-mode-reload)

;; ** Disable-mouse-wheel and more
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :commands (global-disable-mouse-mode)
  :init
  (entropy/emacs-lazy-with-load-trail disable-mouse (global-disable-mouse-mode t)))

;; ** Artist-mode
(use-package artist
  :ensure nil
  :init
  ;; Init disable rubber-banding for reducing performance requirements.
  (add-hook 'artist-mode-hook
            #'(lambda () 
                (if artist-rubber-banding
                    (setq-local artist-rubber-banding nil)))))

(defun entropy/emacs-basic-ex-toggle-artist-and-text ()
  (interactive)
  "Toggle mode between `text-mode' & `artist-mode'."
  (cond ((eq major-mode 'picture-mode)
         (text-mode))
        ((eq major-mode 'text-mode)
         (yes-or-no-p "Really for that? (maybe you don't want to change to artist!) ")
         (artist-mode))))
(entropy/emacs-lazy-load-simple 'artist
  (define-key artist-mode-map (kbd "<f5>") 'entropy/emacs-basic-ex-toggle-artist-and-text))
(entropy/emacs-lazy-load-simple 'text-mode
  (define-key text-mode-map (kbd "<f5>") 'entropy/emacs-basic-ex-toggle-artist-and-text))

;; Disabled '<' and '>' keybinding function.
(entropy/emacs-lazy-load-simple 'artist
  (define-key artist-mode-map (kbd ">") nil)
  (define-key artist-mode-map (kbd "<") nil))

(defun entropy/emacs-basic-artist-mode ()
  "Open one temp-file with artist-mode.
Temp file was \"~/~entropy-artist.txt\""
  (interactive)
  (find-file "~/~entropy-artist.txt")
  (artist-mode))

;; ** Disable auto-window-vscroll -----> the resean from
;; `https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag'
(setq auto-window-vscroll nil)

;; ** Use chinese pyim
;; *** extra dependencies
;; **** librime for pyim
(use-package liberime-config
  :ensure nil
  :commands (liberime-load)
  :preface
  (defun entropy/emacs-basic-pyim-load-rime ()
    (liberime-load)
    (liberime-select-schema "luna_pinyin_simp"))
  :init
  (setq liberime-shared-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-scheme-data)
        liberime-user-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-cache-dir)))

;; **** pyim-basic
(use-package pyim-basedict
  :ensure nil
  :commands (pyim-basedict-enable))

;; **** simple chinese to traditional chinese
(use-package entropy-s2t
  :ensure nil
  :commands entropy/s2t-string)

;; *** preface
(use-package pyim
  :diminish chinese-pyim-mode
  :commands (pyim-restart-1
             pyim-start
             entropy/emacs-basic-pyim-toggle
             entropy/emacs-basic-toggle-pyim-s2t
             entropy/emacs-basic-toggle-pyim-punctuation-half-or-full
             pyim-convert-string-at-point)
  :bind
  (("M-j" . pyim-convert-string-at-point))
  
  :preface
  (defun entropy/emacs-basic-pyim-start ()
    (interactive)
    (require 'pyim)
    (cond ((and (eq entropy/emacs-pyim-use-backend 'internal)
                (not entropy/emacs-pyim-dicts))
           (pyim-basedict-enable))
          ((eq entropy/emacs-pyim-use-backend 'internal)
           (setq pyim-dicts entropy/emacs-pyim-dicts))
          ((eq entropy/emacs-pyim-use-backend 'liberime)
           (entropy/emacs-basic-pyim-load-rime)))
    (set-input-method "pyim")

    ;; keybinding reflect

    (global-set-key (kbd "C-\\") 'entropy/emacs-basic-pyim-toggle)
    (entropy/emacs-!set-key (kbd "2") 'entropy/emacs-basic-toggle-pyim-s2t)
    (entropy/emacs-!set-key (kbd "1") 'entropy/emacs-basic-toggle-pyim-punctuation-half-or-full))

;; *** init  
  :init

  ;; If didn't use pyim set input method to nil
  (unless entropy/emacs-enable-pyim
    (setq default-input-method nil))

  ;;  Setting pyim as the default input method
  (when entropy/emacs-enable-pyim
    (setq default-input-method "pyim"))

  ;;  pyim backend chosen
  (cl-case entropy/emacs-pyim-use-backend
    (internal (setq pyim-default-scheme 'quanpin))
    (liberime (setq pyim-default-scheme 'rime-quanpin)))
  
  ;;  use popup or posframe for pyim tooltip show

  (if (version< emacs-version "26")
      (if (or (eq entropy/emacs-pyim-tooltip 'posframe)
              (not entropy/emacs-pyim-tooltip))
          (setq pyim-page-tooltip 'popup)
        (setq pyim-page-tooltip entropy/emacs-pyim-tooltip))
    (progn
      (if entropy/emacs-pyim-tooltip
          (setq pyim-page-tooltip entropy/emacs-pyim-tooltip)
        (setq pyim-page-tooltip 'posframe))))

  (setq pyim-dcache-directory entropy/emacs-pyim-cached-dir)

  ;; 5 candidates shown for pyim tooltip
  (setq pyim-page-length 8)

;; *** config
  :config
;; **** toggle input method
  (defun entropy/emacs-basic-pyim-toggle ()
    (interactive)
    (if (string= current-input-method "pyim")
        (set-input-method "rfc1345")
      (progn
        (set-input-method "pyim")
        (setq pyim-punctuation-escape-list nil))))
  
;; **** using 'C-g' to cancling any pyim manipulation
  (if (not (version< emacs-version "26"))
      (define-key pyim-mode-map (kbd "C-g") 'pyim-quit-clear))
  
;; **** s2t&t2s convertor
  (defun entropy/emacs-basic-toggle-pyim-s2t ()
    (interactive)
    (if pyim-magic-converter
        (setq pyim-magic-converter nil)
      (setq pyim-magic-converter 'entropy/s2t-string)))

;; **** toglle punctuation between half and full way.
  (defun entropy/emacs-basic-toggle-pyim-punctuation-half-or-full ()
    (interactive)
    (if (eq (car pyim-punctuation-translate-p) 'no)
        (setq pyim-punctuation-translate-p '(yes no auto))
      (setq pyim-punctuation-translate-p '(no yes auto)))))

;; ** Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; ** Key modification
;; *** xclip activation
(use-package xclip
  :if (and (not (display-graphic-p)) (executable-find "xclip"))
  :commands
  (xclip-mode)
  :init
  (entropy/emacs-lazy-with-load-trail
   xclip-mode
   (xclip-mode 1)))

;; *** key re-mapping
;; Binding 'super' and 'hyper' on win32 and mac.
;;   the idea form `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'
(cond
 (sys/win32p
  (setq w32-apps-modifier 'hyper) ; Menu/App key
  (global-set-key (kbd "C-M-g") 'keyboard-quit) ; when unintended active this, using 'QUIT' as 'C-g'
  (global-set-key (kbd "C-s-g") 'keyboard-quit) ; same as above of super key intended active
  (global-set-key (kbd "A-C-g") 'keyboard-quit) ; same as above of super key intended active
  )
 
 (sys/macp
  ;; set keys for Apple keyboard, for emacs in OS X
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  ))

;; *** event re-bind
;; **** xterm re-bind
(entropy/emacs-lazy-with-load-trail
 xterm-rebind
 (when (and (not (display-graphic-p))
            (fboundp #'xterm-paste))
   (if (not (entropy/emacs-is-ssh-session))
       (if (not (executable-find "xclip"))
           (define-key global-map [xterm-paste]
             #'entropy/emacs-xterm-paste)
         (define-key global-map [xterm-paste]
           #'yank))
     (define-key global-map [xterm-paste]
       #'entropy/emacs-xterm-paste-sshsession))

   (defun entropy/emacs-xterm-paste-sshsession ()
     (interactive)
     (let ()
       (run-with-timer 0.01 nil #'yank)
       (keyboard-quit)))   

   (defun entropy/emacs-basic-xterm-term-S-insert-sshsession ()
     (interactive)
     (run-with-timer
      0.01 nil
      #'(lambda ()
          (let* ((paste (with-temp-buffer
                          (yank)
                          (car kill-ring))))
            (when (stringp paste)
              (setq paste (substring-no-properties paste))
              (term-send-raw-string paste)))))
     (keyboard-quit))

   (entropy/emacs-lazy-load-simple 'term
     (cond
      ((not (entropy/emacs-is-ssh-session))
       (define-key term-raw-map
         [S-insert]
         #'entropy/emacs-xterm-term-S-insert)
       (define-key term-raw-map
         [xterm-paste]
         #'entropy/emacs-xterm-term-S-insert))
      (t
       (define-key term-raw-map
         [S-insert]
         #'entropy/emacs-basic-xterm-term-S-insert-sshsession)
       (define-key term-raw-map
         [xterm-paste]
         #'entropy/emacs-basic-xterm-term-S-insert-sshsession))))))

;; ** Adding advice for `y-or-n-p' for emacs 26 and higher in widnows plattform
(when (and sys/win32p (not (version< emacs-version "26.1")))
  (defun entropy/emacs-basic-y-or-n-p (prompt)
    (let ((judge (completing-read prompt '("yes" "no") nil t)))
      (if (string= judge "yes")
          t
        nil)))
  
  ;; adding advice ro y-or-n-p for temporarily fix bug of that can not
  ;; using any key-bindings when active "C-<lwindow>-g" in windows
  (advice-add 'y-or-n-p :override #'entropy/emacs-basic-y-or-n-p))

;; ** Epa (emacs gpg assistant)
(use-package epa
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-for-hook
   '(dired-mode-hook find-file-hook)
   "epa-mode" "epa-mode"
   (epa-file-enable))
  
  (when (and entropy/emacs-wsl-enable
             (file-exists-p entropy/emacs-wsl-apps))
    (entropy/emacs-lazy-load-simple 'custom
      (custom-set-variables
       '(epg-gpg-program (expand-file-name "gpg.exe" entropy/emacs-wsl-apps))
       '(epg-gpgconf-program (expand-file-name "gpgconf.exe" entropy/emacs-wsl-apps))
       '(epg-gpgsm-program (expand-file-name "gpgsm.exe" entropy/emacs-wsl-apps))))))

;; ** Emacs process and system proced manager hacking
;; *** process
(global-set-key (kbd "C-c s s") 'list-processes)

;; *** proced
(use-package proced
  :ensure nil
  :commands (proced-process-attributes)
  :preface
  (defun entropy/emacs-basic-proced-processP (processName)
    "Return one alist collected the proced info of procssName,
otherwise returns nil."
    (let ((procedList (proced-process-attributes))
          rtn)
      (dolist (el procedList)
        (when (equal (cdr (assq 'comm el))
                     processName)
          (push el rtn)))
      rtn))

  (defun entropy/emacs-basic-proced-auto-startwith (processName $executable)
    (unless (entropy/emacs-basic-proced-processP processName)
      (cl-case system-type
        (windows-nt
         (when (fboundp 'w32-shell-execute)
           (let ((default-directory temporary-file-directory))
             (w32-shell-execute
              "open" $executable)
             (message (format "Start with '%s'."
                              $executable)))))
        (t (message
            "`entropy/emacs-basic-proced-auto-startwith' are just used in w32 platform")))))

  :init
  (setq-default proced-format 'medium)
  (entropy/emacs-lazy-with-load-trail
   auto-start-exec
   (dolist (el entropy/emacs-startwith-apps)
     (when (executable-find (cdr el))
       (entropy/emacs-basic-proced-auto-startwith (car el) (cdr el))))))

;; ** Improve captialize function

;; Due to the convention while want to capitalize or uper-case the
;; word just has been done, building follow two function to enhance
;; the origin function `capitalize-word' and `upercase-word' and
;; `down-case'.

(defmacro entropy/emacs-basic--build-case-toggle (type-name main-func)
  `(defun ,(intern (concat "entropy/emacs-basic-toggle-case-for-" type-name))
       (arg)
     (interactive "P")
     (left-word)
     (call-interactively ',main-func t (vector arg))))

(entropy/emacs-basic--build-case-toggle "capitalize" capitalize-word)
(entropy/emacs-basic--build-case-toggle "upcase" upcase-word)
(entropy/emacs-basic--build-case-toggle "downcase" downcase-word)

(global-set-key (kbd "M-c") 'entropy/emacs-basic-toggle-case-for-capitalize)
(global-set-key (kbd "M-u") 'entropy/emacs-basic-toggle-case-for-upcase)
(global-set-key (kbd "M-l") 'entropy/emacs-basic-toggle-case-for-downcase)

;; ** autocompression moode

;; Force refresh autocompression mode enabling status as that the
;; initialization for its refers procedure can not cover fully
;; functional of `auto-compression-mode'.
(cond (entropy/emacs-fall-love-with-pdumper
       (add-hook 'entropy/emacs-pdumper-load-hook
                 #'(lambda ()
                     (auto-compression-mode 0)
                     (auto-compression-mode 1))))
      (t
       (entropy/emacs-lazy-initial-advice-before
        '(push-button load-library find-library)
        "autocompression-mode"
        "autocompression-mode"
        (auto-compression-mode 0)
        (auto-compression-mode 1))))

;; * provide
(provide 'entropy-emacs-basic)
