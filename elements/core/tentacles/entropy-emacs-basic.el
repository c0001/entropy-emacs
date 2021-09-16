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
;; * Code
;; ** require

(when (and entropy/emacs-indicate-sshd-session
           (display-graphic-p))
  (setq entropy/emacs-indicate-sshd-session nil))

(defun entropy/emacs-basic-set-mark-command ()
  (entropy/emacs-!set-key
    (kbd entropy/emacs-top-key)
    'set-mark-command))

(defun entropy/emacs-basic-mark-set ()
  "Mark the current point and push it to mark ring so that this
place can be easily found by other interactive command."
  (interactive)
  (save-excursion
    (push-mark)))

(entropy/emacs-lazy-with-load-trail
 top-keybinding
 :pdumper-no-end t
 :body
 (if (null (daemonp))
     (progn
       (define-key (current-global-map)
         (kbd entropy/emacs-top-key)
         entropy/emacs-top-keymap)
       (entropy/emacs-basic-set-mark-command))
   ;;------------ set basic key-binds for daemon session------------
   ;;; redefine kill emacs bindings to frame delete that follow the
   ;;; default daemon session convention.
   (define-key (current-global-map)
     [remap save-buffers-kill-terminal] #'delete-frame)
   ;;; remap `entropy/emacs-top-key' for daemon session
   (entropy/emacs-with-daemon-make-frame-done
    'top-key-bind nil nil
    '(progn
       (setq entropy/emacs-top-key
             (if (display-graphic-p)
                 (car entropy/emacs-top-prefix-key-cons)
               (cdr entropy/emacs-top-prefix-key-cons)))
       (define-key
         (current-global-map)
         (kbd
          entropy/emacs-top-key)
         entropy/emacs-top-keymap)))
   ;; after set the top key, rebind the `set-mark-command' key-bind
   (entropy/emacs-with-daemon-make-frame-done
    'set-mark-command nil nil
    '(entropy/emacs-basic-set-mark-command))))

;; EEMACS_MAINTENANCE
(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer ivy-read)
 "disable-gvfs" "disable-gvfs" prompt-echo
 :pdumper-no-end t
 ;; Since tramp archive using simple magick filename regexp
 ;; matching, and its internal
 ;; `tramp-archive-file-name-handler-alist''s each corresponding
 ;; implementation can not follow the equalization API defination of
 ;; origin function, thus for most of tramp internal errors will
 ;; pollute the thread operations. (e.g. local directory naming as an
 ;; archive name will also invoking tramp-archive methods which throw
 ;; out many more problems while its magick filename I/O deals)
 (setq tramp-archive-enabled nil))

;; ** Basic major-modes spec
;; *** Dired config
;; **** dired basic
(use-package dired
  :ensure nil
;; ***** init
  :init

;; ****** pretty-hydra
  (defvar entropy/emacs-basic-dired-hydra-hollow-cabinets
    '("Basic"
      (("RET" dired-find-file "Open item dwim"
        :enable t
        :exit t
        :map-inject t)
       ("M-o" dired-find-file-other-window "Open item in other window"
        :enable t
        :exit t
        :map-inject t)
       ("R" dired-do-rename "Rename current file or all marked (or next ARG) files"
        :enable t :map-inject t :exit t)
       ("D" entropy/emacs-basic-dired-delete-file-recursive "Delete recursive"
        :enable t
        :map-inject t
        :exit t)
       ("M-d" entropy/emacs-basic-dired-delete-file-refers "Delete refers"
        :enable t
        :map-inject t
        :exit t)
       ("+" dired-create-directory "Create directory"
        :enable t
        :map-inject t
        :exit t)
       ("S" hydra-dired-quick-sort/body "Sort dired with hydra dispatch"
        :enable (not sys/win32p) :map-inject t :exit t)
       ("M-<up>" dired-up-directory "Up directory"
        :enable t
        :map-inject t
        :exit t)
       ("B" entropy/emacs-basic-backup-files "Common Backup"
        :enable t
        :map-inject t
        :exit t))
      "Useful"
      (("g" revert-buffer "Refresh current dired buffer"
        :enable t :map-inject t :exit t)
       ("m" (:pretty-hydra-cabinet
             (:data
              "Dired basic mark commands"
              (("m" dired-mark "Mark the file at point in the Dired buffer"
                :enable t :map-inject t :exit t)
               ("t" dired-toggle-marks "Marked files become unmarked, and vice versa"
                :enable t :map-inject t :exit t)
               ("u" dired-unmark "Unmark the file at point in the Dired buffer"
                :enable t :map-inject t :exit t)
               ("U" dired-unmark-all-marks "Remove all marks from all files in the Dired buffer"
                :enable t :map-inject t :exit t))
              "Dired rich mark commands"
              (("* %" dired-mark-files-regexp "Mark all files matching REGEXP for use in later commands"
                :enable t :map-inject t :exit t)
               ("* *" dired-mark-executables "Mark all executable files"
                :enable t :map-inject t :exit t)
               ("* ." dired-mark-extension "Mark all files with a certain EXTENSION for use in later commands"
                :enable t :map-inject t :exit t)
               ("* /" dired-mark-directories "Mark all directory file lines except ‘.’ and ‘..’"
                :enable t :map-inject t :exit t)
               ("* @" dired-mark-symlinks "Mark all symbolic links"
                :enable t :map-inject t :exit t)
               ("* N" dired-number-of-marked-files "Display the number and total size of the marked files"
                :enable t :map-inject t :exit t)))
             :other-rest-args
             ((dired dired-mode-map)))
        "Dired mark commands"
        :enable t :exit t)
       ("w" dired-copy-filename-as-kill "Copy names of marked (or next ARG) files into the kill ring"
        :enable t :map-inject t :exit t)
       ("y" dired-show-file-type "Print the type of FILE, according to the ‘file’ command"
        :enable t :map-inject t :exit t)
       ("C" dired-do-copy "Copy all marked (or next ARG) files, or copy the current file"
        :enable t :map-inject t :exit t)
       ("L" dired-do-load "Load the marked (or next ARG) Emacs Lisp files"
        :enable t :map-inject t :exit t)
       ("Z" dired-do-compress "Compress or uncompress marked (or next ARG) files"
        :enable t :map-inject t :exit t)
       ("M-(" dired-mark-sexp "Mark files for which PREDICATE returns non-nil"
        :enable t :map-inject t :exit t)
       (":" (:pretty-hydra-cabinet
             (:data
              "Dired EPA(gnupg elisp Binding) commands"
              ((":d" epa-dired-do-decrypt "Decrypt marked files"
                :enable t :map-inject t :exit t)
               (":v" epa-dired-do-verify "Verify marked files"
                :enable t :map-inject t :exit t)
               (":s" epa-dired-do-sign "Sign marked files"
                :enable t :map-inject t :exit t)
               (":e" epa-dired-do-encrypt "Encrypt marked files"
                :enable t :map-inject t :exit t)))
             :other-rest-args
             ((dired dired-mode-map)))
        "Dired EPA(gnupg elisp Binding) commands"
        :enable t :exit t)
       ("i" (:pretty-hydra-cabinet
             (:data
              "dired image viewer commands"
              (("C-t d" image-dired-display-thumbs "Display thumbnails of all marked files"
                :enable t :map-inject t :exit t)
               ("C-t t" image-dired-tag-files "Tag marked file(s) in dired"
                :enable t :map-inject t :exit t)
               ("C-t r" image-dired-delete-tag "Remove tag for selected file(s)"
                :enable t :map-inject t :exit t)
               ("C-t j" image-dired-jump-thumbnail-buffer "Jump to thumbnail buffer"
                :enable t :map-inject t :exit t)
               ("C-t i" image-dired-dired-display-image "Display current image file"
                :enable t :map-inject t :exit t)
               ("C-t x" image-dired-dired-display-external "Display file at point using an external viewer"
                :enable t :map-inject t :exit t)
               ("C-t a" image-dired-display-thumbs-append "Append thumbnails to ‘image-dired-thumbnail-buffer’"
                :enable t :map-inject t :exit t)
               ("C-t ." image-dired-display-thumb "Shorthand for ‘image-dired-display-thumbs’ with prefix argument"
                :enable t :map-inject t :exit t)
               ("C-t c" image-dired-dired-comment-files "Add comment to current or marked files in dired"
                :enable t :map-inject t :exit t)
               ("C-t f" image-dired-mark-tagged-files "Use regexp to mark files with matching tag"
                :enable t :map-inject t :exit t)
               ("C-t C-t" image-dired-dired-toggle-marked-thumbs
                "Toggle thumbnails in front of file names in the dired buffer"
                :enable t :map-inject t :exit t)
               ("C-t e" image-dired-dired-edit-comment-and-tags
                "Edit comment and tags of current or marked image files"
                :enable t :map-inject t :exit t)))
             :other-rest-args
             ((dired dired-mode-map)))
        "Image dired commands"
        :enable t :exit t))
      "Misc."
      (("0" entropy/emacs-basic-get-dired-fpath "Get Node Path"
        :enable t
        :map-inject t
        :exit t)
       ("M-l" entropy/emacs-basic--dired-add-to-load-path "Add path"
        :enable t
        :map-inject t
        :exit t))))

  (entropy/emacs-lazy-initial-advice-before
   (dired)
   "hydra-hollow-init-for-dired"
   "hydra-hollow-init-for-dired"
   prompt-echo
   :pdumper-no-end t
   (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    'dired-mode '(dired dired-mode-map) t
    entropy/emacs-basic-dired-hydra-hollow-cabinets
    '(2 2)))

;; ***** config
  :config
;; ****** Delete directory with force actions
  (setq entropy/emacs-basic--dired-delete-file-mode-map (make-sparse-keymap))

  ;; TODO : comlete `entropy/emacs-basic--dired-delete-file-mode'
  (define-minor-mode entropy/emacs-basic--dired-delete-file-mode
    "Minor mode for func `entropy/emacs-basic-dired-delete-file-recursive'."
    :keymap entropy/emacs-basic--dired-delete-file-mode-map
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

  (defun entropy/emacs-basic--dired-delete-file-prompt (files-list)
    "popup buffer to deleting with prompting and return the
condition state for whether be continuing rest process."
    (if (dired-mark-pop-up " *Deletions*"
                           'delete
                           files-list
                           dired-deletion-confirmer
                           (format "%s %s " "Deleting"
                                   (dired-mark-prompt nil files-list)))
        t
      (error "Cancel deleting files!")))

  (defun entropy/emacs-basic-dired-delete-file-recursive
      (&optional pre-files just-kill-refers)
    "Delete file recursively with refer buffer cleaned (i.e. files
under this dir will cleaned their corresponding buffers (file
buffer and dir host dired buffer within current emacs session.)

This func is the replacement for func `dired-do-delete', take
advantage by giving the deletion failed handle for each fatal by
showing up the failing error detailes and referred transaction
suggestions.

Error handle will switching to special buffer '*eemacs delete file error*'
buffer with minor mode
`entropy/emacs-basic--dired-delete-file-mode'.

For lisp code, optional args:

- PRE-FILES: file list to deletion
- JUST-KILL-REFERS: just kill file referred file buffer or hosted
  dir's dired buffers.
"
    (interactive)
    (let* ((base-files (cond ((and (equal major-mode 'dired-mode)
                                   (not pre-files))
                              (dired-get-marked-files))
                             ((and (not (null pre-files))
                                   (listp pre-files))
                              pre-files)
                             (t (error "Dir list invalid!"))))
           (did-times (length base-files))
           _file-type
           (pbufname "*eemacs delete file error*")
           (prompt-buffer (get-buffer-create
                           pbufname))
           error-occurred
           (count 1))

      (unless just-kill-refers
        (entropy/emacs-basic--dired-delete-file-prompt base-files))

      (dolist (file base-files)

        ;; killed refer buffers.
        (dolist (el (buffer-list))
          (let* ((buffer-file (buffer-file-name el)))
            (when (and buffer-file
                       (entropy/emacs-file-equal-p file buffer-file))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-files
                           (cons (buffer-name el) (current-time-string)))
              (kill-buffer el))))

        ;; killed refer dired buffers
        (dolist (el dired-buffers)
          (let ((dir (car el))
                (buffer (cdr el)))
            (when (and
                   (buffer-live-p buffer)
                   (if (eq major-mode 'dired-mode)
                       (not (eq buffer (current-buffer)))
                     t)
                   (entropy/emacs-file-equal-p
                    (if (or (file-directory-p file)
                            ;; if file not exist then we match the name string
                            (string-match-p "/\\|\\\\$" file))
                        ;; NOTE: we must delete the trailing slash of
                        ;; the dired directory container retrieved by
                        ;; each car of the element of `dired-buffer'
                        ;; since it may cause handle magick filenames
                        ;; using `tramp-archive-file-name-handler'
                        ;; such as directory name includes '.*.tar/'
                        ;; etc. , which may cause the loop break with
                        ;; specified handle error by gvfs.
                        (replace-regexp-in-string "/$" "" dir)
                      (expand-file-name
                       (file-name-nondirectory file) dir))
                    file))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-dired-buffers
                           (cons el (current-time-string)))
              (kill-buffer buffer))))
        (when (not just-kill-refers)
          (condition-case this-error
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
                (when (and (equal major-mode 'dired-mode)
                           (= count did-times))
                  (revert-buffer))
                (cl-case _file-type
                  ('symbol_link
                   (message (format "Delete symbolink '%s' done! -v-" file)))
                  ('file
                   (message (format "Delete file '%s' done! -v-" file)))
                  ('directory
                   (message (format "Delete directory '%s' done! -v-" file))))
                (cl-incf count))
            (error
             (setq error-occurred t)
             (let* ((inhibit-read-only t))
               (unless (buffer-live-p prompt-buffer)
                 (error "buffer creation error for %s" pbufname))
               (with-current-buffer prompt-buffer
                 (entropy/emacs-basic--dired-delete-file-mode)
                 (goto-char (point-min))
                 (erase-buffer)
                 (when (= count 1)
                   (insert
                    "========== File Deletion Error Handle Prompt Buffer ==========\n"))
                 (insert
                  (format "[%s] %s: %s deletion failed by %s \n"
                          count _file-type file (cdr this-error)))))))))
      (when error-occurred
        (with-current-buffer prompt-buffer
          (insert "========== Prompt End =========="))
        (pop-to-buffer prompt-buffer))))

  (defun entropy/emacs-basic-dired-delete-file-refers ()
    "Kill all refers of dired markd file of directories."
    (interactive)
    (entropy/emacs-basic-dired-delete-file-recursive nil t))


;; ****** unbind some unusual keys which may cause mistakes or its dangerous

  (dolist (key (list
                ;; letter bounds
                "a" "c" "d" "e" "f" "h" "i" "j" "k"
                "l" "n" "p" "q" "s" "v" "x"
                ;; case letter bounds
                "A" "F" "G" "H" "I" "M" "N" "O" "P"
                "T" "V" "W" "X" "Y"
                ;; special key-sequence
                "M-)" "~" "=" "C-M-d" "C-o" "M-s a C-s" "M-s a M-C-s"
                "M-s f C-s" "M-s f M-C-s" "\M-\C-?" "\M-\C-d" "\M-\C-u"
                "\M-\C-n" "\M-\C-p" "\M-{" "\M-}" "%"))
    (define-key dired-mode-map (kbd key) nil))

;; ****** Set unit of dired inode for human readable
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (if (not sys/is-win-group)
      ;; because of windows dired list is too long so just let it in linux
      (setq dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

;; ****** Always delete and copy resursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

;; ****** Improve dired files operation experience for kill opened refer buffers.
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

;; ****** get both UNIX and WINDOWS style path string
  (defvar entropy/emacs-basic-dired-fpath-get-log nil
    "The log list stored file path temporarily, will be reset
when you call `entropy/emacs-basic-get-dired-fpath'.")

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
              entropy/emacs-basic-dired-fpath-get-log nil
              entropy/emacs-basic-dired-fpath-get-log rtn)
        (message "Save all path string to log variable 'entropy/emacs-basic-dired-fpath-get-log'.")))))

;; ****** dired add load path
  (defun entropy/emacs-basic--dired-add-to-load-path ()
    (interactive)
    (let ((dir (completing-read "Choose load path adding item: "
                                'read-file-name-internal
                                nil t)))
      (unless (file-directory-p dir)
        (setq dir (file-name-directory dir)))
      (add-to-list 'load-path dir)))

;; ****** dired backup file

  (defun entropy/emacs-basic-backup-files ()
    (interactive)
    (when (not (fboundp 'entropy/cl-backup-file))
      (require 'entropy-common-library))
    (let ((files (dired-get-marked-files)))
      (dolist (el files)
        (when (file-exists-p el)
          (entropy/cl-backup-file el)))
      (revert-buffer)))

;; ****** dired auto revert after some operations

  (defun entropy/emacs-basic--dired-revert-advice (orig-func &rest orig-args)
    "Dired buffer revert around advice for various file create operation.

Currently just auto revert the 'from' dired buffer after thus
operations.

This function temporally inhibits the `dired-omit-verbose' option
to avoid the omitting status verbose messave while new file
injection into the 'to' dired buffer(if it is visited which means
the appropriate dired buffer is opened).

TODO:

- [] Support newfile created target dired buffer revert feature

  This will be very hard to do that because the internal api
  `dired-create-files' doesn't expose the target dired buffer
  which we could not retrieve thus unless we dirty hacky on the
  api, but it will lost the compatible ability later or may meet
  some unexpected fatals."
    (let* ((dired-omit-verbose nil)
           (cur-buffer (current-buffer))
           (rtn (apply orig-func orig-args)))
      (with-current-buffer cur-buffer
        (revert-buffer))
      rtn))
  (dolist (el '(dired-do-rename
                dired-do-rename-regexp
                dired-do-copy
                dired-do-copy-regexp
                dired-do-compress
                dired-do-compress-to
                dired-do-touch))
    (advice-add el :around #'entropy/emacs-basic--dired-revert-advice))

;; ****** patch for `dired-mark-pop-up'

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun dired-mark-pop-up (buffer-or-name op-symbol files function &rest args)
    "Return FUNCTION's result on ARGS after showing which files are marked.
Displays the file names in a window showing a buffer named
BUFFER-OR-NAME; the default name being \" *Marked Files*\".  The
window is not shown if there is just one file, `dired-no-confirm'
is t, or OP-SYMBOL is a member of the list in `dired-no-confirm'.

By default, Dired shrinks the display buffer to fit the marked files.
To disable this, use the Customization interface to add a new rule
to `display-buffer-alist' where condition regexp is \"^ \\*Marked Files\\*$\",
action argument symbol is `window-height' and its value is nil.

FILES is the list of marked files.  It can also be (t FILENAME)
in the case of one marked file, to distinguish that from using
just the current file.

FUNCTION should not manipulate files, just read input (an
argument or confirmation).

NOTE:

This function has been modified to be compat with =entropy-emacs=.

Originally, this funciton using fully widnow size to fit the
buffer content by using `fit-window-to-buffer', and thus this is
annoying when buffer size was large which will make some
conflicates to other UI experience like `ivy-read'. Yeap, the
modifcation is to remove this feature.
"
    (if (or (eq dired-no-confirm t)
            (memq op-symbol dired-no-confirm)
            ;; If FILES defaulted to the current line's file.
            (= (length files) 1))
        (apply function args)
      (let ((buffer (get-buffer-create (or buffer-or-name " *Marked Files*")))
            ;; Mark *Marked Files* window as softly-dedicated, to prevent
            ;; other buffers e.g. *Completions* from reusing it (bug#17554).
            (display-buffer-mark-dedicated 'soft))
        (with-displayed-buffer-window
            buffer
            (cons 'display-buffer-below-selected
                  ;; HACK: just preserve size
                  '((preserve-size . (nil . t))))
            #'(lambda (window _value)
                (with-selected-window window
                  (unwind-protect
                      (apply function args)
                    (when (window-live-p window)
                      (quit-restore-window window 'kill)))))
          ;; Handle (t FILE) just like (FILE), here.  That value is
          ;; used (only in some cases), to mean just one file that was
          ;; marked, rather than the current line file.
          (with-current-buffer buffer
            (dired-format-columns-of-files
             (if (eq (car files) t) (cdr files) files))
            (remove-text-properties (point-min) (point-max)
                                    '(mouse-face nil help-echo nil))
            (setq tab-line-exclude nil))))))


  )

;; **** Use dired-aux to enable dired-isearch
(entropy/emacs-lazy-initial-advice-before
 (dired) "dired-aux-init-for-dired" "dired-aux-init-for-dired"
 prompt-echo
 :pdumper-no-end t
 (require 'dired-aux)
 ;; disable '.' key binding with `dired-clean-directory' for dired
 ;; mode for inadvertently press.
 (define-key dired-mode-map (kbd ".") nil))

;; **** Quick sort dired buffers via hydra
  ;;; bind key: `S'
(when (not sys/win32p)
  (use-package dired-quick-sort
    :if (or (executable-find "gls") (executable-find "ls"))
    :commands (dired-quick-sort-setup)
    :init (add-hook 'dired-mode-hook 'dired-quick-sort-setup)))

;; **** Use coloful dired ls

(use-package dired-rainbow
  :eemacs-macros (dired-rainbow-define)
  :commands (dired-rainbow-define-chmod)
  :init
  (entropy/emacs-lazy-initial-advice-before
   (dired)
   "dired-rainbow-init-for-dired" "dired-rainbow-init-for-dired"
   prompt-echo
   :pdumper-no-end t
   (require 'dired-rainbow)
   (dired-rainbow-define dotfiles "gray" "\\..*")
   (dired-rainbow-define
    web "#4e9a06"
    ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
     "json" "asp" "aspx" "haml" "php" "jsp" "ts"
     "coffee" "scss" "less" "phtml"))
   (dired-rainbow-define
    prog "yellow3"
    ("el" "l" "ml" "py" "rb" "pl" "pm" "c"
     "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
     "m" "cs" "mk" "make" "swift" "go" "java"
     "asm" "robot" "yml" "yaml" "rake" "lua"))
   (dired-rainbow-define
    sh "green yellow"
    ("sh" "bash" "zsh" "fish" "csh" "ksh"
     "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
   (dired-rainbow-define
    text "yellow green"
    ("txt" "md" "org" "ini" "conf" "rc"
     "vim" "vimrc" "exrc"))
   (dired-rainbow-define
    doc "spring green"
    ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
     "csv" "rtf" "wps" "pdf" "texi" "tex"
     "odt" "ott" "odp" "otp" "ods" "ots"
     "odg" "otg"))
   (dired-rainbow-define
    misc "gray50"
    ("DS_Store" "projectile" "cache" "elc"
     "dat" "meta"))
   (dired-rainbow-define
    media "#ce5c00"
    ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
     "wmv" "mov" "3gp" "avi" "mpg" "mkv"
     "flv" "ogg" "rm" "rmvb"))
   (dired-rainbow-define
    picture "purple3"
    ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
     "ico" "svg" "psd" "pcd" "raw" "exif"
     "BMP" "JPG" "PNG"))
   (dired-rainbow-define
    archive "saddle brown"
    ("zip" "tar" "gz" "tgz" "7z" "rar"
     "gzip" "xz" "001" "ace" "bz2" "lz"
     "lzma" "bzip2" "cab" "jar" "iso"))
   ;; boring regexp due to lack of imagination
   (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")))

(use-package diredfl
  :commands (diredfl-global-mode)
  :init
  (entropy/emacs-lazy-initial-advice-before
   (dired)
   "diredfl-init-for-dired" "diredfl-init-for-dired"
   prompt-echo
   :pdumper-no-end t
   (diredfl-global-mode 1)))

;; **** dired-x
(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :eemacs-mmphca
  (((:enable t)
    (dired-mode (dired dired-mode-map)))
   ("Misc."
    (("o" dired-omit-mode "Toggle omission of uninteresting files in Dired (Dired-Omit mode)."
      :enable t :toggle dired-omit-mode :map-inject t))))
  :config
  (setq dired-omit-size-limit nil)
  (setq dired-omit-extensions nil)
  ;; Just omit the current node point
  (setq dired-omit-files "\\`[[:space:]]*[.][.]?\\'")
  ;; remap `dired-omit-mode' command in `dired-mode-map' because of
  ;; conflicted with `ace-window'
  (define-key dired-mode-map "\C-x\M-o" nil))

;; **** dired-subtree
;; Org mode like dired subtree fold/expand
(use-package dired-subtree
  :after dired
  :commands
  (dired-subtree-toggle
   dired-subtree-cycle)
  :eemacs-mmphca
  (((:enable t)
    (dired-mode (dired dired-mode-map)))
   ("Basic"
    (("TAB" dired-subtree-toggle
      "Insert subtree at point (vice versa)."
      :enable t :map-inject t :exit t)
     ("<backtab>" dired-subtree-cycle
      "Org-mode like cycle visibilitya"
      :enable t :map-inject t :exit t))))
  :config
;; ***** patch `dired-up-directory'

  (defun entropy/emacs-basic--dired-subtree-advice-for-dired-up-directory
      (orig-func &rest orig-args)
    "Move point to the parent node of current node in dired
buffer without buffer content refreshing.

NOTE:

This function is a around advice for function `dired-up-directory'."
    (let ((current-node (ignore-errors (dired-get-filename)))
          cur-node-parent)
      (setq cur-node-parent
            (ignore-errors
              (file-name-directory
               (directory-file-name current-node))))
      (if (or (null current-node)
              (entropy/emacs-file-equal-p default-directory cur-node-parent))
          (if (and
               (save-excursion
                 (forward-line 0)
                 (let ((line-str (buffer-substring (point) (progn (end-of-line) (point)))))
                   (if (string-empty-p line-str)
                       t
                     nil)))
               (and (bound-and-true-p dired-subtree-overlays)
                    (catch :exit
                      (dolist (ov dired-subtree-overlays)
                        (let ((ov-beg (overlay-start ov))
                              (ov-end (overlay-end ov)))
                          (when (and
                                 ov-beg ov-end  ;; prevent from null overlay instance
                                 (and (<= (point) ov-end)
                                      (>= (point) ov-beg)))
                            (throw :exit t)))))))
              ;; if current point in the subree overlay but not at an dired node
              (progn (forward-line -1) (dired-move-to-filename))
            ;; Apply orig func filtered by checking whether the up-dir exist?
            (let* ((dir (dired-current-directory))
                   (up (file-name-directory (directory-file-name dir))))
              (if (file-directory-p up)
                  (apply orig-func orig-args)
                ;; kill other dired buffer as is or as subtree of thus dir
                (let ((asoc-ups (dired-buffers-for-dir up)))
                  (message "Up dir '%s' is not exsit, Abort!" up)
                  (dolist (buff asoc-ups)
                    (kill-buffer buff))))))
        (let* ((search-node
                (file-name-nondirectory
                 (directory-file-name cur-node-parent))))
          (while (not (entropy/emacs-file-equal-p (dired-get-filename) cur-node-parent))
            (re-search-backward
             (regexp-quote search-node)
             nil t))))))

  (advice-add 'dired-up-directory
              :around
              #'entropy/emacs-basic--dired-subtree-advice-for-dired-up-directory)

;; ***** patch `dired-subtree--readin'

  (defun entropy/emacs-basic--dired-subtree-readin-around-advice
      (orig-func &rest orig-args)
    "Around advice for `dired-subtree--readin' to put the
'dired-filename' text property to the filename correctly.

That origin function just listing the directory files with
verbatim style which doesn't has ability to distinguish some
filename start with space char, this is buggly that if thus,
`dired' will using basically `dired-move-to-filename' to get the
beginning of the filename position which will prompt a warning
that show that filename wasn't exsited any more."
    (let* ((readin (apply orig-func orig-args))
           (dirname (car orig-args))
           fname-ps-list
           rtn
           (at-start t)
           (inhibit-read-only t))
      (if (string-empty-p readin)
          readin
        (with-temp-buffer
          (insert readin)

          ;; Pruning the dir list of '.' and '..'
          ;;
          ;; EEMACS_MAINTENANCE: some times the function
          ;; `insert-directory' doesn't list the '.' '..' at top of
          ;; the buffer, this need to investigating
          (save-excursion
            (goto-char (point-min))
            (let (this-func)
              (setq this-func
                    (lambda ()
                      (let ((fname-beg (save-excursion (dired-move-to-filename)))
                            (fname-end (save-excursion (end-of-line) (point)))
                            (kill-whole-line t))
                        (when (integerp fname-beg)
                          (when (or (string= (buffer-substring-no-properties fname-beg fname-end)
                                             ".")
                                    (string= (buffer-substring-no-properties fname-beg fname-end)
                                             ".."))
                            (forward-line 0)
                            (kill-line)
                            (funcall this-func))))))
              (funcall this-func)
              (while (= 0 (forward-line))
                (funcall this-func))
              ;; remove eob's blank line
              (goto-char (point-max))
              (when (looking-at "^$")
                ;; ignore error when buffer is empty now
                (ignore-errors
                  (delete-char -1)))))

          (goto-char (point-min))
          (while (or at-start (= (forward-line) 0))
            (setq at-start nil)
            (let* ((fname-beg (save-excursion (dired-move-to-filename) (point)))
                   (fname-end (save-excursion (end-of-line) (point)))
                   (line-beg (save-excursion (forward-line 0) (point)))
                   (fname-end1 fname-end)
                   fname)
              (setq fname (buffer-substring-no-properties fname-beg fname-end))
              (while (and
                      (not (file-exists-p (expand-file-name fname dirname)))
                      (or (> fname-beg line-beg)
                          (error "Fatal read current filename correctly in subtree routine")))
                (while (and
                        (> fname-end1 fname-beg)
                        (not (file-exists-p (expand-file-name fname dirname))))
                  (setq fname (buffer-substring-no-properties fname-beg fname-end1))
                  (unless (file-exists-p (expand-file-name fname dirname))
                    (setq fname-end1 (1- fname-end1))))
                (if (file-exists-p (expand-file-name fname dirname))
                    (setq fname-end fname-end1)
                  (setq fname-beg (1- fname-beg)
                        fname-end1 fname-end)))
              (push (cons fname-beg fname-end) fname-ps-list)))
          (dolist (item fname-ps-list)
            (put-text-property (car item) (cdr item) 'dired-filename t))
          (setq rtn (buffer-substring (point-min) (point-max)))
          rtn))))
  (advice-add 'dired-subtree--readin
              :around
              #'entropy/emacs-basic--dired-subtree-readin-around-advice)
  )


;; **** wdired

(use-package wdired
  :ensure nil
  :commands
  (wdired-mode
   wdired-finish-edit)
  :config
  (defun __adv/around/wdired-finish-edit/for-window-recenter
      (orig-func &rest orig-args)
    "Recenter the window after `wdired-finish-edit' since it
always revert buffer after change dired buffer in which case the
window point not shown in nice place e.g. at window bottom."
    (prog1
        (apply orig-func orig-args)
      (recenter-top-bottom '(middle))))
  (advice-add 'wdired-finish-edit
              :around
              #'__adv/around/wdired-finish-edit/for-window-recenter)
  )

;; *** Image-mode
(use-package image-mode
  :ensure nil
  :eemacs-mmphc
  (((:enable t)
    (nil
     nil
     t
     ((1 :width-desc "Navigation")
      (1 :width-desc "Resize")
      (1 :width-desc "Animation"))))
   ("Navigation"
    (("gn" image-next-file "Visit the next image in the same directory"
      :enable t :map-inject t :exit t)
     ("gN" image-previous-file "Visit the preceding image in the same directory"
      :enable t :map-inject t :exit t))

    "Resize"
    (("t+" image-increase-size "Increase the image size by a factor of N."
      :enable t :map-inject t :exit t)
     ("t-" image-decrease-size "Decrease the image size by a factor of N."
      :enable t :map-inject t :exit t)
     ("tf" image-mode-fit-frame "Fit FRAME to the current image."
      :enable t :map-inject t :exit t)
     ("tr" image-transform-reset "Display the current image with the default size and rotation."
      :enable t :map-inject t :exit t)
     ("th" image-transform-fit-to-height "Fit the current image to the height of the current window."
      :enable t :map-inject t :exit t)
     ("tw" image-transform-fit-to-width "Fit the current image to the width of the current window."
      :enable t :map-inject t :exit t)
     ("ts" image-transform-set-scale "Prompt for a number, and resize the current image by that amount."
      :enable t :map-inject t :exit t))

    "Animation"
    (("aa" image-toggle-animation "Start or stop animating the current image."
      :enable t :map-inject t :exit t)
     ("a+" image-increase-speed "Increase the speed of current animated image by a factor of 2."
      :enable t :map-inject t :exit t)
     ("a-" image-decrease-speed "Decrease the speed of current animated image by a factor of 2."
      :enable t :map-inject t :exit t)
     ("ar" image-reset-speed "Reset the animation speed of the current image."
      :enable t :map-inject t :exit t))
    ))

  :config
  (defun entropy/emacs-basic-image-gif-warn (&optional args)
    "Warn that gif animation by large gif file will freeze
emacs."
    (if (string-match-p "\\.gif" (buffer-name))
        (if (not (y-or-n-p "Do you want to animated it? "))
            (error "Please open it with external apps!"))))
  (advice-add 'image-toggle-animation :before #'entropy/emacs-basic-image-gif-warn))


;; *** Artist-mode
(use-package artist
  :ensure nil
  :init
  ;; Init disable rubber-banding for reducing performance requirements.
  (add-hook 'artist-mode-hook
            #'(lambda ()
                (if artist-rubber-banding
                    (setq-local artist-rubber-banding nil))))

  :config

  ;; Disabled '<' and '>' keybinding function.
  (entropy/emacs-lazy-load-simple artist
    (define-key artist-mode-map (kbd ">") nil)
    (define-key artist-mode-map (kbd "<") nil))
  )

(defun entropy/emacs-basic-artist-mode ()
  "Open one temp-file with artist-mode.
Temp file was \"~/~entropy-artist.txt\""
  (interactive)
  (find-file "~/~entropy-artist.txt")
  (artist-mode))


;; *** Man-mode
(use-package man
  :ensure nil
  :commands (man
             entropy/emacs-Man-mode-fit-to-window)
  :eemacs-mmphc
  (((:enable t)
    (Man-mode (man Man-mode-map) t (1 2 2)))
   ("Basic"
    (("k"    Man-kill
      "Kill the buffer containing the manpage."
      :enable t :exit t :map-inject t)
     ("u"    Man-update-manpage
      "Reformat current manpage by calling the man command again synchronously."
      :enable t :exit t :map-inject t)
     ("f"    entropy/emacs-Man-mode-fit-to-window
      "Fit current manpage to `window-width'."
      :enable t :exit t :map-inject t)
     ("m"    man
      "Get a Un*x manual page and put it in a buffer."
      :enable t :exit t :map-inject t))
    "Navigation"
    (("n"    Man-next-section "Next section"
      :enable t :exit t :map-inject t)
     ("p"    Man-previous-section "Previous section"
      :enable t :exit t :map-inject t)
     ("M-n"  Man-next-manpage "Next manpage"
      :enable t :exit t :map-inject t)
     ("M-p"  Man-previous-manpage "Previous Manpage"
      :enable t :exit t :map-inject t)
     ("."    beginning-of-buffer "Goto top"
      :enable t :exit t :map-inject t))
    "Jump referrence"
    (("r"    Man-follow-manual-reference
      "Get one of the manpages referred to in the \"SEE ALSO\" section"
      :enable t :exit t :map-inject t)
     ("g"    Man-goto-section
      "Move point to SECTION."
      :enable t :exit t :map-inject t)
     ("s"    Man-goto-see-also-section
      "Move point to the \"SEE ALSO\" section."
      :enable t :exit t :map-inject t))))

  :init
  (setq Man-width-max nil)

  :config
  (defun __adv/around/Man-mode/disable-auto-window-fit
      (orig-func &rest orig-args)
    (prog1
        (apply orig-func orig-args)
      (remove-hook 'window-state-change-functions
                   #'Man--window-state-change t)))
  (advice-add 'Man-mode
              :around
              #'__adv/around/Man-mode/disable-auto-window-fit)

  (defun entropy/emacs-Man-mode-fit-to-window
      (&rest _)
    "Manually fit current manpage to `window-width' like
`Man-fit-to-window' and just support in an buffer that is
`current-buffer' on which enabled `Man-mode'."
    (interactive)
    (with-current-buffer (current-buffer)
      (when (eq major-mode 'Man-mode)
        (Man-fit-to-window
         (get-buffer-window
          (current-buffer))))))

  )

(use-package woman
  :ensure nil
  :defer entropy/emacs-fall-love-with-pdumper
  :commands woman
  :init (setq woman-fill-column 100
              woman-dired-keys nil)
  :config
  (defun entropy/emacs-basic-woman--no-warning-around-advice
      (orig-func &rest orig-args)
    "Suppress defalias warning for woman defined for."
    (with-no-warnings
      (apply orig-func orig-args)))
  (advice-add 'woman
              :around
              #'entropy/emacs-basic-woman--no-warning-around-advice))

;; ** Basic global settings:
;; *** Temporal bug revert
;; **** gnutls bug for emacs version upper than '26.1'
;;
;; Bug refer emacs `url.el' bug or possible for the gnutls bug override.
;;
;; Refer:
;; @see https://github.com/magit/ghub/issues/81#issuecomment-488660597
;; For now [2019-08-08 Thu 19:23:42] it seems occur on w32 port only
(when (and (or (version= "26.2" emacs-version)
               (version= "26.3" emacs-version))
           sys/win32p)
  (advice-add #'gnutls-available-p :override #'ignore))


;; **** Disable `auto-mode-alist' case fold search

;; Disable case insensitive search mode for `auto-mode-case-fold' when
;; possible
;;
;; The reason for that this one charge the behaviour of `find-file'
;; where how to did with `auto-mode-alist', that each judgement of
;; thus was a filename regexp match, for those special filename will
;; encounter ridiculous conflict operation mistake such of which file
;; named as 'ebrowse' will opened automatically by `ebrowse-tree-mode'
;; becasue there's two injection from that within `auto-mode-alist'
;; i.e. '("BROWSE\\'" . ebrowse-tree-mode) and '("\\.ebrowse\\'"
;; . ebrowse-tree-mode), so funny.
;;
;; But this is not a bug, because on some platform (e.g. WINDOWS),
;; file name is native case insensitive as is and there's no treatment
;; for those situations.
(setq auto-mode-case-fold nil)

;; *** Personal infomation
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
(defun entropy/emacs-basic--dspln-mode-around-advice
    (orig-func &rest orig-args)
  "Filters for `display-line-numbers-mode' to press it for some
occasions. "
  (unless (or (member major-mode
                      '(vterm-mode
                        shell-mode
                        eshell-mode
                        term-mode
                        treemacs-mode
                        neotree-mode
                        dashboard-mode
                        dired-mode
                        eww-mode
                        w3m-mode))
              (bound-and-true-p entropy/emacs-ui-init-welcom-mode))
    (apply orig-func orig-args)))

(advice-add 'display-line-numbers-mode
            :around
            #'entropy/emacs-basic--dspln-mode-around-advice)

(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer)
 "global-display-line-numbers-mode"
 "global-display-line-numbers-mode"
 prompt-echo
 :pdumper-no-end nil
 (setq-default display-line-numbers-width-start t)
 (when entropy/emacs-init-display-line-numbers-mode
   (global-display-line-numbers-mode)))

;; *** Global hl-line mode
(defun entropy/emacs-turn-on-hl-line-mode ()
  "The filter for `hl-line-mode' available option in triggered
buffer, in that case any conditions don't match the filter then
`hl-line-mode' will be enabled."
  (unless (or (member major-mode
                      '(vterm-mode
                        shell-mode
                        eshell-mode
                        term-mode
                        dashboard-mode))
              (bound-and-true-p entropy/emacs-ui-init-welcom-mode)
              (eq major-mode 'eww-mode)
              (let ((rtn
                     (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
                (> rtn 1000))
              (minibufferp))
    (hl-line-mode 1)))

;; Build `hl-line-mode' based global mode, that it is different from
;; `global-hl-line-mode' which use `global-hl-line-highlight-all' as
;; the subroutine. So that we can toggle line highlight feature
;; without global status restriction anti from what
;; `global-hl-line-mode' did.
(define-globalized-minor-mode entropy/emacs-hl-line-global-mode
  hl-line-mode
  entropy/emacs-turn-on-hl-line-mode)

(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer)
 "global-hl-line-mode" "global-hl-line-mode" prompt-echo
 :pdumper-no-end nil
 (when entropy/emacs-init-hl-line-mode
   (entropy/emacs-hl-line-global-mode 1)))

;; *** Backup setting
(setq-default auto-save-default nil)    ; disable it for preventing typing lagging
(setq make-backup-files nil)

;; *** Scratch buffer corresponding file
;;
;;     Amounts of company backend function can not functional
;;     auto-completion in none file buffer, so corresponding one file
;;     to *scratch* buffer.

(defun entropy/emacs-basic--scratch-buffer-file-binding ()
  "Corresponded *scratch* buffer to one temp-file.

Filename are \".scratch_entropy\" host in
`entropy/emacs-stuffs-topdir'.
"
  (let ((bfn "*scratch*"))
    (if (entropy/emacs-buffer-exists-p "*scratch*")
        (kill-buffer "*scratch*"))
    (let ((fname (expand-file-name ".scratch_entropy" entropy/emacs-stuffs-topdir))
          ;; inhibit `find-file-hook' for speedup file create
          (find-file-hook nil))
      (if (not (file-exists-p fname))
          (progn
            (write-region "" "" fname)
            (with-current-buffer (find-file-noselect fname)
              (if buffer-read-only (read-only-mode 0))
              (auto-save-mode 0)
              (rename-buffer "*scratch*")
              (unless (eq major-mode 'lisp-interaction-mode)
                (lisp-interaction-mode))
              (insert initial-scratch-message)))
        (with-current-buffer (find-file-noselect fname)
          (if buffer-read-only (read-only-mode 0))
          (auto-save-mode 0)
          (rename-buffer "*scratch*")
          (unless (eq major-mode 'lisp-interaction-mode)
            (lisp-interaction-mode))
          (save-excursion
            (goto-char (point-min))
            (unless (string-match-p
                     (concat
                      (car (split-string (regexp-quote initial-scratch-message) "^$" t))
                      ".*")
                     (buffer-substring-no-properties
                      (point-min) (point-max)))
              (insert initial-scratch-message))))))
    bfn))

(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer ivy-read)
 "init-eemamcs-scratch-buffer" "init-eemamcs-scratch-buffer"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-basic--scratch-buffer-file-binding))

;; Create a new scratch buffer
(defun entropy/emacs-basic-create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (entropy/emacs-basic--scratch-buffer-file-binding))
  (lisp-interaction-mode)
  (message "Create *scratch* buffer"))

;; *** Highlight current line and display line numbers
(defun entropy/emacs-basic--dhl-judge-state ()
  (let ((hlmp (bound-and-true-p hl-line-mode))
        (dlmp (bound-and-true-p display-line-numbers))
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
  "Dwim enable/disable `hl-line-mode' while be on
`display-line-numbers-mode' with prefix key enable."
  (interactive "P")
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
       (display-line-numbers-mode 0)))))

(defvar-local eemacs-hl-line-mode-enable nil)
(defun entropy/emacs-basic--hl-line-mode-patcher-0
    (orig-func &rest orig-args)
  "Use `eemacs-hl-line-mode-enable' as the further indicator of
`hl-line-mode'."
  (let ((rtn (apply orig-func orig-args)))
    (if (bound-and-true-p hl-line-mode)
        (progn
          (setq-local eemacs-hl-line-mode-enable t)
          ;; since we use the idle timer to show hl-line, so we
          ;; disable below unneeded hooks in `post-command-hook' for
          ;; reduce redudant perfomance leak

          ;; preserve this for hl-line overlay move action
          ;; (remove-hook 'post-command-hook #'hl-line-highlight t)
          (remove-hook 'post-command-hook #'hl-line-maybe-unhighlight t))
      (setq-local eemacs-hl-line-mode-enable nil))
    rtn))
(with-eval-after-load 'hl-line
  (advice-add 'hl-line-mode
              :around #'entropy/emacs-basic--hl-line-mode-patcher-0))

(defun entropy/emacs-basic--hl-line-mode-recover nil
  "Timer function to recovery the `hl-line-mode' status when
temporally shutdown by
`entropy/emacs-basic--hl-line-disable-wrapper'."
  (when (and (bound-and-true-p eemacs-hl-line-mode-enable)
             (null (bound-and-true-p hl-line-mode))
             entropy/emacs-current-session-is-idle)
    (hl-line-mode 1)))
(run-with-idle-timer
 (max entropy/emacs-safe-idle-minimal-secs 0.25)
 t #'entropy/emacs-basic--hl-line-mode-recover)

(defun entropy/emacs-basic--hl-line-disable-wrapper
    (orig-func &rest orig-args)
  "Temporally disable `hl-line-mode' to satisfied more
performance requests while ORIG-FUNC is called up.

NOTE: this is a advice wrapper for any function."
  (let (rtn)
    (unwind-protect
        (when (bound-and-true-p hl-line-mode)
          (hl-line-mode 0)
          (setq-local eemacs-hl-line-mode-enable t))
      (setq rtn (apply orig-func orig-args)))
    rtn))

;; ---> Default wrapped for:
(dolist (func '(previous-line next-line newline))
  (advice-add func
              :around #'entropy/emacs-basic--hl-line-disable-wrapper))
(with-eval-after-load 'dired
  (advice-add 'dired-next-line
              :around #'entropy/emacs-basic--hl-line-disable-wrapper))

;; *** Smooth scrolling
;; Force smooth mouse scroll experience
(when (display-graphic-p)
  (setq
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
   mouse-wheel-progressive-speed nil))

(defvar entropy/emacs-basic--next-screen-context-lines-orig-value next-screen-context-lines)
(defvar entropy/emacs-basic--redisplay-dont-pause-orig-value redisplay-dont-pause)
(defvar entropy/emacs-basic--scroll-margin-orig-value scroll-margin)
(defvar entropy/emacs-basic--scroll-conservatively-orig-value scroll-conservatively)

(define-minor-mode entropy/emacs-basic-smooth-scrolling-mode
  "Toggle smooth-scrolling buffer scrolling view."
  :init-value nil
  :lighter "SM"
  :global t
  (if entropy/emacs-basic-smooth-scrolling-mode
      (progn
        (setq
         next-screen-context-lines 0
         redisplay-dont-pause      t
         scroll-margin             0
         scroll-conservatively     100)
        (message "Smooth scrolling enabled!"))
    (progn
      (setq
       next-screen-context-lines entropy/emacs-basic--next-screen-context-lines-orig-value
       redisplay-dont-pause      entropy/emacs-basic--redisplay-dont-pause-orig-value
       scroll-margin             entropy/emacs-basic--scroll-margin-orig-value
       scroll-conservatively     entropy/emacs-basic--scroll-conservatively-orig-value)
      (message "Smooth scrolling disabled!"))))

(entropy/emacs-basic-smooth-scrolling-mode 1)


;; *** Kill-buffer-and-window spec

(defun entropy/emacs-basic-kill-ansiterm-buffer ()
  "Kill `ansi-term' buffer with proper way.

This function mainly gives a patch for the bug of error after
kill ansi-term buffer and its popup window refer to bug
#h-0c3ab89e-a470-42d2-946e-4f217ea2f20c in entropy-emacs bug
collection."
  (interactive)
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
    (kill-this-buffer)))

(defun entropy/emacs-basic-kill-buffer-and-show-its-dired ()
  "Kill buffer, swtich to its hosted location `dired' buffer when
its a exists file's buffer."
  (interactive)
  (let ((buffn (buffer-name))
        (base-dir default-directory)
        (fname (buffer-file-name)))
    (kill-buffer buffn)
    (when (and (ignore-errors (stringp fname))
               (file-writable-p fname))
      (dired base-dir))))

(defun entropy/emacs-basic-kill-buffer-and-its-window-when-grids ()
  "Kill buffer and close it's host window if windows conuts
retrieve from `window-list' larger than 1."
  (interactive)
  (let ((buflist (window-list)))
    (if (> (length buflist) 1)
        (cond
         ((or
           (and (eq (length buflist) 2)
                (let (rtn)
                  (mapc (lambda (window)
                          (let ((buff (window-buffer window)))
                            (when (window-parameter
                                   (get-buffer-window buff)
                                   'no-delete-other-windows)
                              (setq rtn t))))
                        buflist)
                  rtn))
           ;; when the window is the root window of current frame we
           ;; just kill it.  NOTE: in some cases the main window may
           ;; not be the only live window showed in current frame as
           ;; messy.
           (eq (window-main-window) (get-buffer-window (current-buffer))))
          (kill-buffer))
         (t
          (kill-buffer-and-window)))
      (kill-buffer))))

(defun entropy/emacs-basic-kill-buffer ()
  "Entropy emacs specified `kill-buffer' method, used for replace
as thus."
  (interactive)
  (cond
   ((eq major-mode 'term-mode)
    (call-interactively #'entropy/emacs-basic-kill-ansiterm-buffer))
   ((buffer-file-name)
    (call-interactively #'entropy/emacs-basic-kill-buffer-and-show-its-dired))
   (t
    (call-interactively
     #'entropy/emacs-basic-kill-buffer-and-its-window-when-grids))))

(global-set-key (kbd "C-x k") #'entropy/emacs-basic-kill-buffer)
(global-set-key (kbd "C-x M-k") #'kill-buffer)

;; *** Set defualt tab size
;; Do not use `indent-tabs-mode' by default for compatibility meaning
;; that tabs visualization are not unified accorss editor.

(if entropy/emacs-custom-tab-enable
    (setq-default tab-width entropy/emacs-custom-tab-width)
  (setq-default indent-tabs-mode nil))

;; *** disable `electric-indent-mode' by default

;; Remove it globally since we do not use it frequently in non prog
;; mode and it well inject to `post-self-insert-hook' to increase
;; performance latency.
(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer)
 "__disable-electric-indent-mode" "__disable-electric-indent-mode"
 prompt-echo
 :pdumper-no-end t
 (when (bound-and-true-p electric-indent-mode)
   (electric-indent-mode 0))
 ;; add it locally to prog referred mode hook
 (let ((spec-modes (append entropy/emacs-ide-for-them
                           (list 'emacs-lisp-mode
                                 'lisp-interaction-mode))))
   ;; use `electric-indent-local-mode' for spec modes
   (dolist (mode spec-modes)
     (add-hook (intern (format "%s-hook" mode))
               'electric-indent-local-mode))
   ;; make exist opened buffer enable `electric-indent-local-mode'
   (dolist (buff (buffer-list))
     (with-current-buffer buff
       (when (memq major-mode spec-modes)
         (electric-indent-local-mode 1))))))

;; *** Setting language encoding environment
(setq system-time-locale "C") ;Use english format time string

;; **** Default using UTF-8 encoding for basic environment
;; when `entropy/emacs-custom-language-environment-enable' was nil

(unless entropy/emacs-custom-language-environment-enable
  (entropy/emacs-lang-set-utf-8))

;; **** Using customized basic encoding system
;; When `entropy/emacs-custom-language-environment-enable' was t

(when (and entropy/emacs-custom-language-environment-enable
           (stringp entropy/emacs-locale-language-environment))
  ;; Customize language environment with user specification
  (entropy/emacs-lang-set-local))

;; ***** Specific cases to forceing using UTF-8 encoding environment

;; ****** Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ****** Force setting specific file type which must be opened with utf-8-unix encoding system.
(dolist (suffix
         '(;; document file
           "\\.org$" "\\.md$"
           ;; source file
           "\\.html" "\\.css$" "\\.php$" "\\.js$" "\\.ts$"
           "\\.c\\(p+\\)?$" "\\.py$" "\\.lisp$" "\\.el$"
           "\\.sh$" "\\.bat$"
           ))
  (modify-coding-system-alist 'file suffix 'utf-8-unix))

;; ========================================================================
;; Prompt: If you want all file to be one coding system you should do below
;;(modify-coding-system-alist 'file "" 'utf-8-unix)
;; ========================================================================

;; ****** let diff-buffer-with-file force run with unicode language environment
(advice-add 'diff-buffer-with-file
            :around #'entropy/emacs-lang-use-utf-8-ces-around-advice)

;; *** Auto wrap line
(setq-default truncate-lines t)

;; *** Auto clean whitespace after save buffer
(use-package whitespace
  :ensure nil
  :commands (whitespace-cleanup)
  :preface
  (defun entropy/emacs-basic-simple-whitespace-clean ()
    "Clean whitespace with the default `whitspace-style'."
    (interactive)
    (require 'whitespace)
    (let ((whitespace-style (default-value 'whitespace-style)))
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (whitespace-cleanup)))))
  :init
  (add-hook 'before-save-hook
            #'entropy/emacs-basic-simple-whitespace-clean))


;; *** Set transparenct of emacs frame

(defvar entropy/emacs-basic--loop-alpha-did nil)

(defun entropy/emacs-basic-loop-alpha (&optional prefix)
  "Toggle frame transparent status with specified transparent
value as optional interaction while `PREFIX' is non-nil."
  (interactive "P")
  (setq entropy/emacs-basic--loop-alpha-did
        (or prefix (null entropy/emacs-basic--loop-alpha-did)))
  (let ((bgtr-default entropy/emacs-loop-alpha-value)
        (bgtr
         (when prefix
           (string-to-number (read-string "Input bg trransparent var (75-95): ")))))
    ;; Restrict transparent integer value be between 75 to 95 where is
    ;; the best customization
    (setq bgtr
          (let ()
            (if (not (integerp bgtr))
                bgtr-default
              (if (> bgtr 95)
                  95
                (if (<= bgtr 0)
                    75
                  bgtr)))))
    (funcall
     (lambda (a)
       (unless entropy/emacs-basic--loop-alpha-did
         (setq a 100))
       (let ((alpha-items (mapcar (lambda (x) (when (eq (car x) 'alpha) x))
                                  default-frame-alist)))
         (dolist (el alpha-items)
           (unless (null el)
             (setq default-frame-alist
                   (delete el default-frame-alist)))))
       (set-frame-parameter (selected-frame) 'alpha (list a 100))
       (add-to-list 'default-frame-alist (cons 'alpha (list a 100))))
     bgtr)
    (setq entropy/emacs-loop-alpha-value
          bgtr)))

(when (and entropy/emacs-start-with-frame-transparent-action
           (display-graphic-p))
  (entropy/emacs-lazy-with-load-trail
   loop-alpha
   (entropy/emacs-basic-loop-alpha)))

;; *** Paragraph fill size
(setq-default fill-column entropy/emacs-fill-paragraph-width)

;; *** Show time on mode line
(when entropy/emacs-display-time-modeline
  ;; enable time show when
  (display-time-mode 1)
  (setq-default display-time-interval 1)
  ;; display time with date and real time infos
  (setq display-time-day-and-date t)
  ;; 24hr format
  (setq display-time-24hr-format t)
  (setq display-time-format " %e %b %Y %H:%M:%S ")
  (display-time))

;; *** Input time into buffer
(defun entropy/emacs-basic-now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))

(defun entropy/emacs-basic-today ()
  "Insert string for today's date nicely formatted in American style,
 e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; *** Global read only mode
(use-package entropy-global-read-only-mode
  :ensure nil
  :commands (entropy-grom-mode
             entropy/grom-read-only-buffer
             entropy/grom-quick-readonly-global)
  :eemacs-indhc
  (((:enable t)
    (entropy-grom-mode))
   ("Basic"
    (("<f1>" entropy/grom-read-only-buffer "Toggle buffer read-only status"
      :enable t :global-bind t :exit t
      :toggle buffer-read-only)
     ("t" entropy/grom-toggle-read-only "Toggle global buffers read-only status"
      :enable t :exit t)
     ("M-1" entropy/grom-quick-readonly-global
      "Quickly lock all buffers in current emacs session with internal rules matched"
      :enable t :global-bind t :exit t))))
  :eemacs-tpha
  (((:enable t))
   ("WI&BUF"
    (("L"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'entropy-grom-mode))
      "Buffer locker both for single or global buffer-list"
      :enable t :exit t))))

  :init
  (entropy/emacs-lazy-initial-advice-before
   (find-file push-button find-library-name)
   "entropy-grom"
   "entropy-grom"
   prompt-echo
   :pdumper-no-end t
   (entropy-grom-mode +1))

  :config
  (dolist (rule `(,(rx "*outorg-edit-buffer*")
                  ,(rx "*Buffer Details*")
                  ,(rx "*Memory Explorer*")
                  ,(rx "*poporg: ")))
    (add-to-list 'entropy/grom-customizable-nonspecial-buffer-name-regexp-list
                 rule)))

;; *** Revert buffer automatically

(entropy/emacs-lazy-initial-for-hook
 (find-file-hook)
 "GlbAutoRevertMode"
 "GlbAutoRevertMode-enabled"
 prompt-echo
 (global-auto-revert-mode +1))

(defun entropy/emacs-basic-toggle-global-auto-revert-mode ()
  "Toggle auto-revert global bounds of feature on or off.

NOTE: e.g. `global-auto-revert-mode' and `magit-auto-revert-mode'."
  (interactive)
  (let ((gav-p (bound-and-true-p global-auto-revert-mode))
        gav-on gav-off
        (magit-gav-p (bound-and-true-p magit-auto-revert-mode))
        mgav-on mgav-off)
    (when (or gav-p magit-gav-p)
      (when gav-p
        (global-auto-revert-mode 0)
        (setq gav-off t))
      (when magit-gav-p
        (magit-auto-revert-mode 0)
        (setq mgav-off t)))
    (when (and (null gav-p)
               (null magit-gav-p))
      (global-auto-revert-mode +1)
      (setq gav-on t)
      (when (fboundp 'magit-auto-revert-mode)
        (magit-auto-revert-mode +1)
        (setq mgav-on t)))
    (cond
     ((and gav-on mgav-on)
      (message "Toggle `global-auto-revert-mode' and `magit-auto-revert-mode' on."))
     (gav-on
      (message "Toggle `global-auto-revert-mode' on."))
     (mgav-on
      (message "Toggle `magit-auto-revert-mode' on."))
     ((and gav-off mgav-off)
      (message "Toggle `global-auto-revert-mode' and `magit-auto-revert-mode' off."))
     (gav-off
      (message "Toggle `global-auto-revert-mode' off."))
     (magav-off
      (message "Toggle `magit-auto-revert-mode' off.")))))

;; *** Popup key stroking prompt
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (entropy/emacs-lazy-with-load-trail
   which-key
   :pdumper-no-end t
   :body
   (which-key-mode t))
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay nil
        which-key-side-window-max-height 0.15
        which-key-echo-keystrokes 0.005
        which-key-separator "->"
        which-key-show-remaining-keys t
        which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  :config
  (entropy/emacs-lazy-load-simple help
    (global-set-key (kbd "C-h C-h") nil))

;; **** config
  :config
;; ***** Disable `after-make-frame-functions' when popup which-key dim using frame feature.
  (defun entropy/emacs-basic--which-key-inhibit-stuffs
      (orig-func &rest orig-args)
    (let ((after-make-frame-functions nil))
      (apply orig-func orig-args)))
  (dolist (func '(which-key--show-buffer-new-frame
                  which-key--show-buffer-reuse-frame))
    (advice-add func
                :around
                #'entropy/emacs-basic--which-key-inhibit-stuffs))

  )

;; *** Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure nil
  :commands (global-undo-tree-mode undo-tree-visualize)
  :preface

  (defvar entropy/emacs-basic--undo-tree-stick-window-configuration nil
    "The window configuration before calling `undo-tree-visualize'.")

  (defun entropy/emacs-basic--save-window-cfg-for-undo-tree
      (orig-func &rest orig-args)
    (setq entropy/emacs-basic--undo-tree-stick-window-configuration
          (current-window-configuration))
    (apply orig-func orig-args))

  (defun entropy/emacs-basic--restore-window-cfg-for-undo-tree
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (when (window-configuration-p
             entropy/emacs-basic--undo-tree-stick-window-configuration)
        (set-window-configuration
         entropy/emacs-basic--undo-tree-stick-window-configuration))
      (setq entropy/emacs-basic--undo-tree-stick-window-configuration nil)
      rtn))

  :init
  (entropy/emacs-lazy-initial-advice-before
   (switch-to-buffer find-file)
   "undo-tree-enable-init"
   "undo-tree-enable-init"
   prompt-echo
   ;; undo-tree can not enabled while pdumper
   :pdumper-no-end nil
   (global-undo-tree-mode t)
   (global-set-key (kbd "C-x u") #'undo-tree-visualize))

  :config

  (advice-add 'undo-tree-visualize
              :around
              #'entropy/emacs-basic--save-window-cfg-for-undo-tree)

  (advice-add 'undo-tree-visualizer-quit
              :around
              #' entropy/emacs-basic--restore-window-cfg-for-undo-tree))

;; *** Rectangle manipulation

(use-package rect
  :ensure nil
  :eemacs-indhc
  (((:enable t)
    (rectangle-mode))
   ("Move"
    (("h" backward-char "←"
      :enable t)
     ("j" next-line "↓"
      :enable t)
     ("k" previous-line "↑"
      :enable t)
     ("l" forward-char "→"
      :enable t))
    "Action"
    (("C-x r w" copy-rectangle-as-kill "copy"
      :enable t :global-bind t :exit t)
     ("C-x r y" yank-rectangle "yank"
      :enable t :global-bind t :exit t)
     ("C-x r t" string-rectangle "string"
      :enable t :global-bind t :exit t)
     ("C-x r d" kill-rectangle "kill"
      :enable t :global-bind t :exit t)
     ("C-x r c" clear-rectangle "clear"
      :enable t :global-bind t :exit t)
     ("C-x r o" open-rectangle "open"
      :enable t :global-bind t :exit t)
     ("C-x r N" rectangle-number-lines "number lines"
      :enable t :global-bind t :exit t))))
  :eemacs-tpha
  (((:enable t))
   ("Utils"
    (("u r"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'rectangle-mode))
      "Rectangle region manipulation"
      :enable t :exit t)))))


;; *** Auto-sudoedit

(use-package auto-sudoedit
  :commands (entropy/emacs-sudoedit-current-path-maybe)
  :config
  (defun entropy/emacs-sudoedit-current-path-maybe (curr-path)
    "Get su privileges for CURR-PATH if need to so."
    (interactive (list (auto-sudoedit-current-path)))
    (let ((path (auto-sudoedit-path curr-path)))
      (if path
          (find-file path)
        (entropy/emacs-message-do-message
         "no need to get sudo permission to edit path %s"
         (green (format "%s" curr-path)))))))

;; *** Kill ring config

(defvar entropy/emacs-basic-kill-ring-persist-lock-file
  (expand-file-name
   ".kill-ring-persist.lock"
   entropy/emacs-stuffs-topdir)
  "The persist kill ring file used indicator flag file.")

;; restrict `kill-ring' length for reducing `browse-kill-ring' lag
(setq kill-ring-max 300)

(defun entropy/emacs-basic-kill-ring-persist-backup ()
  (unless (fboundp 'entropy/cl-backup-file)
    (require 'entropy-common-library))
  (when (file-exists-p entropy/emacs-kill-ring-persist-file)
    (entropy/cl-backup-file
     entropy/emacs-kill-ring-persist-file)))

;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/emacs-basic-clear-kill-ring ()
  (interactive)
  (when (yes-or-no-p "Clean kill ring? ")
    (entropy/emacs-basic-kill-ring-persist-backup)
    (setq kill-ring nil) (garbage-collect)))

(defun entropy/emacs-basic-kill-ring-persist (&optional remove-lock)
  "Save `kill-ring' to persist file `entropy/emacs-kill-ring-persist-file'.

Optional argument REMOVE-LOCK when non-nil will delete the persist
kill-ring flag file
`entropy/emacs-basic-kill-ring-persist-lock-file'.

NOTE: do not do the REMOVE-LOCK operation unless you dont care
another eemacs session covers the current kill-ring persist
file."
  (interactive)
  (let* ((find-file-suppress-same-file-warnings t)
         (file entropy/emacs-kill-ring-persist-file)
         (to-buffer (find-file-noselect file))
         (inhibit-read-only t)
         ;; the `kill-ring' cache file header
         (top-message
          (format ";; this file is auto-generated by entropy-emacs \
for kill-ring persistent at %s, do not edit it manually"
                  (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))
         ;; print and coding restriction
         (coding-system-for-read 'utf-8-auto)
         (coding-system-for-write 'utf-8-auto)
         (print-level nil)
         (print-length nil)
         (print-escape-nonascii t)
         (print-circle t)
         ;; the error temp host
         print-error
         ;; perdicatge filter judge
         (can-do-it t)
         ;; function for check `kill-ring' item's printed
         ;; representation whether readable
         (printable-judge
          (lambda (value output-sym)
            "Judge whether the print type of VALUE can be read
correctly, if thus return t otherwise for nil and transferred
error type to output symbol OUTPUT-SYM."
            (with-temp-buffer
              (condition-case error
                  (let (_)
                    ;; Print the value into a buffer...
                    (prin1 value (current-buffer))
                    ;; ...and attempt to read it.
                    (read (point-min-marker))
                    ;; The attempt worked: the object is printable.
                    t)
                ;; The attempt failed: the object is not printable.
                (error (set output-sym error) nil))))))
    (unless (buffer-live-p to-buffer)
      (message "kill ring persist file can not be opened!")
      (setq can-do-it nil))
    (if can-do-it
        (let (_)
          (with-current-buffer to-buffer
            (erase-buffer)
            (insert top-message)
            (insert "\n\n")
            (insert "(")
            (dolist (item kill-ring)
              (let ((item (substring-no-properties item)))
                (if (funcall printable-judge
                             item
                             'print-error)
                    (progn (prin1 item to-buffer)
                           (insert ?\n))
                  ;; Warn that item is not readable
                  (entropy/emacs-message-do-message
                   "%s%s%s%s"
                   (red "⚠: ")
                   "kill ring item "
                   (yellow (format "'%s'" item))
                   (format " can not be saved because of [%s] !"
                           print-error)))))
            (insert ")")
            (with-temp-message ""
              (let ((inhibit-message t)
                    ;; tidy up buffer write context
                    (write-contents-functions nil)
                    (local-write-file-hooks nil)
                    (write-file-functions nil)
                    (before-save-hook nil))
                (save-buffer))))
          (when remove-lock
            (f-delete entropy/emacs-basic-kill-ring-persist-lock-file)))
      (entropy/emacs-message-do-message
       "%s"
       (red "WARN: can not open the persist kill ring file!")))))

(defalias 'entropy/emacs-basic-kill-ring-persist-after-kill-emacs
  (lambda (&rest _)
    "Save kill ring persist file and remove its lock file.

NOTE: this function just injected into `kill-emacs-hook', do not
used as common usage."
    (entropy/emacs-basic-kill-ring-persist t)))

(defun entropy/emacs-basic-read-kill-ring-from-persist ()
  "Restore `kill-ring' from persist file
`entropy/emacs-kill-ring-persist-file' when possible (which means
fallback to origin when restoring with fatal, if thus backup the
original one in the same of host place).

NOTE: this function run init at eemacs startup time, and do not
use it within any time after that.

the return value is t or nil while t indicates the read procedure
successfully both of situation of read persisit of create an new."
  (interactive)
  (if (file-exists-p entropy/emacs-basic-kill-ring-persist-lock-file)
      (when (and (yes-or-no-p "It seems kill ring persist file is locked by another emacs session\
, unlock it?(NOTE: it may cover the another emacs session's kill-ring persistence!)")
                 (progn (delete-file entropy/emacs-basic-kill-ring-persist-lock-file)
                        t))
        (entropy/emacs-basic-read-kill-ring-from-persist))
    (let* ((file entropy/emacs-kill-ring-persist-file)
           (find-file-suppress-same-file-warnings t) ;suppress the noisy same file warning
           (buffer (find-file-noselect file))
           (inhibit-read-only t)
           kill-ring-read
           (kill-ring-old (copy-tree kill-ring))
           (rtn t))
      (with-current-buffer buffer
        (goto-char (point-min))
        (condition-case error
            (let ((coding-system-for-read 'utf-8-auto)
                  (coding-system-for-write 'utf-8-auto))
              (setq kill-ring-read (read (current-buffer)))
              (setq kill-ring kill-ring-read))
          (error
           (setq kill-ring kill-ring-old
                 rtn nil)
           (unless (= (buffer-size) 0)
             (warn (format "Read persit kill-ring fatal of [%s], fallen back to origin done!"
                           error))
             (entropy/emacs-basic-kill-ring-persist-backup)))))
      (f-touch entropy/emacs-basic-kill-ring-persist-lock-file)
      rtn)))

;; run it when init emacs
(defvar entropy/emacs-basic-timer-of-kill-ring-persist nil)
(defun entropy/emacs-basic--init-kill-ring-persist ()
  "Initialize =entropy-emacs= `kill-ring' persist feature."
  (interactive)
  (when (entropy/emacs-basic-read-kill-ring-from-persist)
    (setq entropy/emacs-basic-timer-of-kill-ring-persist
          (run-with-idle-timer 10 t #'entropy/emacs-basic-kill-ring-persist))
    (add-hook 'kill-emacs-hook
              #'entropy/emacs-basic-kill-ring-persist-after-kill-emacs)))

(entropy/emacs-lazy-initial-advice-before
 (find-file switch-to-buffer ivy-read)
 "kill-ring-persist-init" "kill-ring-persist-init"
 :pdumper-no-end nil
 (entropy/emacs-basic--init-kill-ring-persist))

(defalias 'entropy/emacs-basic-start-kill-ring-persist
  #'entropy/emacs-basic--init-kill-ring-persist)

(defun entropy/emacs-basic-disable-kill-ring-persist ()
  "Disable =entropy-emacs= `kill-ring' persist feature."
  (interactive)
  (entropy/emacs-basic-kill-ring-persist)
  (when (timerp entropy/emacs-basic-timer-of-kill-ring-persist)
    (cancel-timer entropy/emacs-basic-timer-of-kill-ring-persist)
    (setq entropy/emacs-basic-timer-of-kill-ring-persist nil))
  (f-delete entropy/emacs-basic-kill-ring-persist-lock-file)
  (remove-hook 'kill-emacs-hook
               #'entropy/emacs-basic-kill-ring-persist-after-kill-emacs))

;; *** Forbidden view-hello-file for W32 platform

;; `view-hello-file' will freeze WINDOWS emacs session, override it!
(when sys/is-win-group
  (defun view-hello-file ()
    "Prompt emacs user do not use view-hello-file in windows
operation system"
    (interactive)
    (message "Do not use view-hello-file in windows because of it will jamm windows and emacs")))

;; *** Delete file to trash
(if entropy/emacs-dired-enable-trash
    (setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder but with some
                                               ; performance bug on windows plattform.
  )

;; *** History && Recentf
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
  :preface
  :init
  (entropy/emacs-lazy-initial-advice-before
   (find-file switch-to-buffer)
   "recentf-init" "recentf-init" prompt-echo
   ;; injects into pdumper recovery session since the recentf is
   ;; dynamic
   :pdumper-no-end nil
   (recentf-mode))

  :config
  (setq recentf-max-saved-items 1000
        recentf-exclude
        `("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
          ,(expand-file-name recentf-save-file)
          (lambda (file) (file-in-directory-p file package-user-dir)))))

(use-package savehist
  :ensure nil
  :init

  (entropy/emacs-lazy-initial-advice-before
   (find-file ivy-switch-buffer dired)
   "savehist-init-for-find-file-refer"
   "savehist-init-for-find-file-refer"
   prompt-echo
   :pdumper-no-end t
   (savehist-mode t))

  :config
  (setq
   history-length 10000
   savehist-additional-variables
   '(mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history)
   savehist-ignored-variables
   '(kill-ring)
   savehist-autosave-interval 60))

;; *** Bookmarks
(setq bookmark-save-flag 1)

;; *** Major mode reload
(defun entropy/emacs-basic-major-mode-reload ()
  "Reload current `major-mode'.

This function was usable for some occurrence that current mode
functional part was missed or be without working on the proper
way. "
  (interactive)
  (if (not (string-match-p "^CAPTURE-" (buffer-name)))
      (let ((point (point))
            (mode major-mode))
        (progn
          (cond
           ((derived-mode-p 'prog-mode)
            ;; switch to `fundamental-mode' firstly for preventing
            ;; some unexpected messy while prog modes is major-mode
            ;; FIXME: why?
            (fundamental-mode))
           (t
            t))
          (funcall mode))
        (when (eq major-mode 'org-mode)
          (outline-show-all))
        (goto-char point)
        (message "Reloaded current major mode '%s'!" (symbol-name major-mode)))
    (error "You can not refresh %s in this buffer, if did may cause some bug."
           (symbol-name major-mode))))

;; *** Disable-mouse-wheel and more
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :commands (global-disable-mouse-mode)
  :init
  (when entropy/emacs-disable-mouse-at-init-time
    (entropy/emacs-lazy-with-load-trail
     disable-mouse
     (global-disable-mouse-mode t))))

;; *** Disable auto-window-vscroll

;; see
;; `https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag'
;; for details.
;;
;; This is the main lag causer for the line navigation lag. Especially
;; that use the powerline or some modeline type based on it which
;; oftenly provide the buffer 'v-scroll' indicator that for thus.

(setq auto-window-vscroll nil)

;; *** Use chinese pyim
;; **** extra dependencies
;; ***** librime for pyim
(use-package liberime
  :if (not sys/is-win-group)
  :ensure nil
  :defer (or entropy/emacs-fall-love-with-pdumper entropy/emacs-custom-enable-lazy-load)
  :preface
  (defvar entropy/emacs-basic-pyim-liberime-load-timer nil)

  (defun entropy/emacs-basic--pyim-set-rime-schema ()
    "Set rime input schema to 'luna_pinyin_simp' as that is the
only schema supported in entropy-emacs.

Notice:

If your input schema was traditional chinese, try to create
'luna_pinyin.custom.yaml' in
`entropy/emacs-pyim-liberime-cache-dir' with the content to:

#+begin_example
patch:
  switches:                   # 注意縮進
    - name: ascii_mode
      reset: 0                # reset 0 的作用是當從其他輸入方案切換到本方案時，
      states: [ 中文, 西文 ]  # 重設爲指定的狀態，而不保留在前一個方案中設定的狀態。
    - name: full_shape        # 選擇輸入方案後通常需要立即輸入中文，故重設 ascii_mode = 0；
      states: [ 半角, 全角 ]  # 而全／半角則可沿用之前方案中的用法。
    - name: simplification
      reset: 1                # 增加這一行：默認啓用「繁→簡」轉換。
      states: [ 漢字, 汉字 ]
#+end_example

See [[https://github.com/rime/home/wiki/\
CustomizationGuide#\
%E4%B8%80%E4%BE%8B%E5%AE%9A\
%E8%A3%BD%E7%B0%A1%E5%8C%96\
%E5%AD%97%E8%BC%B8%E5%87%BA][the rime wiki]] for details.
"
    (liberime-try-select-schema
     "luna_pinyin_simp"))

  (defun entropy/emacs-basic--pyim-first-build-timer (&optional is-with-building)
    (setq entropy/emacs-basic-pyim-liberime-load-timer
          (run-with-timer
           0 1
           `(lambda ()
              (let* ((proc (get-process "liberime-build"))
                     (proc-status (when proc (process-status proc)))
                     (proc-exited (when proc (eq proc-status 'exit)))
                     proc-exit-signal)
                (when (timerp entropy/emacs-basic-pyim-liberime-load-timer)
                  (cancel-timer entropy/emacs-basic-pyim-liberime-load-timer)
                  (setq entropy/emacs-basic-pyim-liberime-load-timer nil))
                (if (ignore-errors
                      (entropy/emacs-basic--pyim-set-rime-schema))
                    (progn
                      (setq entropy/emacs-pyim-has-initialized t)
                      (when (or proc ',is-with-building)
                        (entropy/emacs-message-do-message
                         "%s"
                         (green "liberime build successfully"))))
                  (cond
                   (proc-exited
                    (setq proc-exit-signal (process-exit-status proc))
                    (if (= 0 proc-exit-signal)
                        ;; in this case although the build is done but
                        ;; the dynamic module has not been loaded done
                        ;; so that we must wait for turns.
                        (entropy/emacs-basic--pyim-first-build-timer
                         t)
                      (entropy/emacs-message-do-message
                       "%s"
                       (red (format "liberime build faild with exit status %s, \
please check buffer '*liberime build*' for details"
                                    proc-exit-signal)))
                      (setq entropy/emacs-pyim-has-initialized 'liberime-build-failed)
                      ))
                   (t
                    (entropy/emacs-basic--pyim-first-build-timer
                     ',is-with-building)))))))))

  (defun entropy/emacs-basic-pyim-load-rime ()
    ;; load liberim just needed to require it. Set
    ;; `liberime-auto-build' to t so that we do not get the build
    ;; prompt messeges and auto-build the liberime-core so that we do
    ;; not manually run `liberime-build' here directly.
    (setq liberime-auto-build t)
    (require 'liberime)
    ;; set liberime schema and check dynamic module status and build
    ;; it when needed.
    (let (building)
      (if
          (not
           (ignore-errors
             (entropy/emacs-basic--pyim-set-rime-schema)))
          (setq building t)
        (setq entropy/emacs-pyim-has-initialized t))
      (when building
        (entropy/emacs-basic--pyim-first-build-timer))))

  :init
  (setq liberime-shared-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-scheme-data)
        liberime-user-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-cache-dir)))

;; ***** pyim-basic
(use-package pyim-basedict
  :ensure nil
  :commands (pyim-basedict-enable))

;; ***** simple chinese to traditional chinese
(use-package entropy-s2t
  :ensure nil
  :commands entropy/s2t-string)

;; **** pyim main
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

  (entropy/emacs-hydra-hollow-add-for-top-dispatch
   '("Pyim"
     (("c c" entropy/emacs-basic-pyim-start
       "Enable Pyim"
       :enable t
       :toggle entropy/emacs-pyim-has-initialized
       :exit t))))

  (defun entropy/emacs-basic-pyim-start ()
    (interactive)
    (unless entropy/emacs-pyim-has-initialized
      (require 'pyim)
      (cond ((eq entropy/emacs-pyim-use-backend 'internal)
             (setq pyim-dicts entropy/emacs-pyim-dicts)
             (setq entropy/emacs-pyim-has-initialized t))
            ((and (eq entropy/emacs-pyim-use-backend 'liberime)
                  (not sys/win32p))
             (require 'pyim-liberime)          ;needed for load liberime for pyim
             (entropy/emacs-basic-pyim-load-rime))
            (t
             (pyim-basedict-enable)
             (setq entropy/emacs-pyim-has-initialized t)))

      ;; init pyim at temp buffer for preventing polluting
      ;; current-input-method in current buffer.
      (with-temp-buffer
        (set-input-method "pyim"))

      ;; keybinding reflect
      (entropy/emacs-hydra-hollow-add-for-top-dispatch
       '("Pyim"
         (("C-\\" entropy/emacs-basic-pyim-toggle
           "Set Inputmethod 'Pyim'"
           :enable t
           :toggle (string= current-input-method "pyim")
           :global-bind t)

          ("c t" entropy/emacs-basic-toggle-pyim-s2t
           "'Pyim' use traditional chinese"
           :enable t
           :toggle (eq pyim-magic-converter 'entropy/s2t-string))

          ("c f" entropy/emacs-basic-toggle-pyim-punctuation-half-or-full
           "'Pyim' toggle punct full/half"
           :enable t
           :toggle (eq (car pyim-punctuation-translate-p) 'yes)))))

      ))

;; ***** init
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

  ;; customized dcache directory
  (setq pyim-dcache-directory entropy/emacs-pyim-cached-dir)

  ;; 5 candidates shown for pyim tooltip
  (setq pyim-page-length 8)

  ;; Using thread for loading dache
  (when (version< "26" emacs-version)
    (setq pyim-prefer-emacs-thread t))

;; ***** config
  :config
;; ****** toggle input method
  (defun entropy/emacs-basic-pyim-toggle ()
    (interactive)
    (if (string= current-input-method "pyim")
        (set-input-method nil)
      (progn
        (set-input-method "pyim")
        (setq pyim-punctuation-escape-list nil))))

;; ****** using 'C-g' to cancling any pyim manipulation
  (if (not (version< emacs-version "26"))
      (define-key pyim-mode-map (kbd "C-g") 'pyim-quit-clear))

;; ****** s2t&t2s convertor
  (defun entropy/emacs-basic-toggle-pyim-s2t ()
    (interactive)
    (if pyim-magic-converter
        (setq pyim-magic-converter nil)
      (setq pyim-magic-converter 'entropy/s2t-string)))

;; ****** toglle punctuation between half and full way.
  (defun entropy/emacs-basic-toggle-pyim-punctuation-half-or-full ()
    (interactive)
    (if (or (eq (car pyim-punctuation-translate-p) 'no)
            (eq (car pyim-punctuation-translate-p) 'auto))
        (setq pyim-punctuation-translate-p '(yes no auto))
      (setq pyim-punctuation-translate-p '(no yes auto)))))

;; *** Key modification

;; **** more conventions
(global-set-key (kbd "C-M-g") 'keyboard-quit) ; when unintended active this, using 'QUIT' as 'C-g'
(global-set-key (kbd "C-s-g") 'keyboard-quit) ; same as above of super key intended active
(global-set-key (kbd "A-C-g") 'keyboard-quit) ; same as above of super key intended active

;; **** key re-mapping
;; Binding 'super' and 'hyper' on win32 and mac.
;;   the idea form `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'
(cond
 (sys/win32p
  (setq w32-apps-modifier 'hyper) ; Menu/App key
  )
 (sys/macp
  ;; set keys for Apple keyboard, for emacs in OS X
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  ))

;; **** xterm re-bind

;; Rebind "insert" refer key in terminal emacs to support yank&cut
;; communication with GUI.

(entropy/emacs-lazy-with-load-trail
 xterm-rebind
 :pdumper-no-end t
 :body
 (let* ((cli-enable-func
         (lambda ()
           (define-key global-map [xterm-paste]
             #'entropy/emacs-xterm-paste)))
        (cli-disable-func
         (lambda ()
           (define-key global-map [xterm-paste]
             #'xterm-paste)))
        (term-enable-func
         (lambda ()
           (require 'term)
           (define-key term-raw-map
             [S-insert]
             #'entropy/emacs-xterm-term-S-insert)
           (define-key term-raw-map
             [xterm-paste]
             #'entropy/emacs-xterm-term-S-insert)))
        (term-disable-func
         (lambda ()
           (require 'term)
           (define-key term-raw-map
             [S-insert]
             #'term-paste)
           (define-key term-raw-map
             [xterm-paste]
             #'term-paste)))
        (enable-func
         `(lambda ()
            (funcall ,cli-enable-func)
            (funcall ,term-enable-func)))
        (disable-func
         `(lambda ()
            (funcall ,cli-disable-func)
            (funcall ,term-disable-func))))
   (if (null (daemonp))
       (when (entropy/emacs-xterm-external-satisfied-p)
         (funcall enable-func))
     (defvar entropy/emacs-basic--xterm-paste-rebinded nil)
     (entropy/emacs-with-daemon-make-frame-done
      'xterm-paste-bind
      `(when (entropy/emacs-xterm-external-satisfied-p)
         (funcall ,enable-func)
         (setq entropy/emacs-basic--xterm-paste-rebinded t))
      `(when entropy/emacs-basic--xterm-paste-rebinded
         (funcall ,disable-func)
         (setq entropy/emacs-basic--xterm-paste-rebinded nil))))))

;; *** Adding advice for `y-or-n-p' for emacs 26 and higher in widnows plattform
(when (and sys/win32p (not (version< emacs-version "26.1")))
  (defun entropy/emacs-basic-y-or-n-p (prompt)
    (let ((judge (completing-read prompt '("yes" "no") nil t)))
      (if (string= judge "yes")
          t
        nil)))

  ;; adding advice ro y-or-n-p for temporarily fix bug of that can not
  ;; using any key-bindings when active "C-<lwindow>-g" in WINDOWS
  (advice-add 'y-or-n-p :override #'entropy/emacs-basic-y-or-n-p))

;; *** Epa (emacs gpg assistant)
(use-package epa
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-for-hook
   (dired-mode-hook find-file-hook)
   "epa-mode" "epa-mode" prompt-echo
   (epa-file-enable))

  (when (and entropy/emacs-wsl-enable
             (file-exists-p entropy/emacs-wsl-apps))
    (entropy/emacs-lazy-load-simple custom
      (custom-set-variables
       '(epg-gpg-program (expand-file-name "gpg.exe" entropy/emacs-wsl-apps))
       '(epg-gpgconf-program (expand-file-name "gpgconf.exe" entropy/emacs-wsl-apps))
       '(epg-gpgsm-program (expand-file-name "gpgsm.exe" entropy/emacs-wsl-apps))))))

;; *** Emacs process and system proced manager hacking
;; **** process
(entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
 'process-menu-mode '(simple process-menu-mode-map) t
 '("Basic"
   (("S" tabulated-list-sort "Sort Tabulated List entries by the column at point"
     :enable t :exit t :map-inject t)
    ("d" process-menu-delete-process "Kill process at point in a 'list-processes' buffer."
     :enable t :exit t :map-inject t)
    ("g" revert-buffer "Refresh process buffer"
     :enable t :exit t :map-inject t)
    ("h" describe-mode "Display documentation of current major mode and minor modes."
     :enable t :exit t :map-inject t)
    ("q" quit-window "Quit WINDOW and bury its buffer."
     :enable t :exit t :map-inject t)
    ("{" tabulated-list-narrow-current-column "Narrow the current tabulated list column by N chars."
     :enable t :exit t :map-inject t)
    ("}" tabulated-list-widen-current-column "Widen the current tabulated-list column by N chars."
     :enable t :exit t :map-inject t))))

;; **** proced
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

  :eemacs-mmphc
  (((:enable t)
    (proced-mode (proced proced-mode-map) t (2 2 2 2)))
   ("Marking"
    (("m" proced-mark "Mark the current (or next COUNT) processes"
      :enable t :exit t :map-inject t)
     ("u" proced-unmark "Unmark the current (or next COUNT) processes"
      :enable t :exit t :map-inject t)
     ("M" proced-mark-all "Mark all processes (or with region support)."
      :enable t :exit t :map-inject t)
     ("U" proced-unmark-all "Unmark all processes (or with region support)."
      :enable t :exit t :map-inject t)
     ("t" proced-toggle-marks "Toggle marks: marked processes become unmarked, and vice versa"
      :enable t :exit t :map-inject t)
     ("C" proced-mark-children "Mark child processes of process PPID."
      :enable t :exit t :map-inject t)
     ("P" proced-mark-parents "Mark parent processes of process CPID."
      :enable t :exit t :map-inject t))
    "Filtering"
    (("f" proced-filter-interactive "Filter Proced buffer using SCHEME"
      :enable t :exit t :map-inject t)
     ("RET" proced-refine
      "Refine Proced listing by comparing with the attribute value at point."
      :enable t :exit t :map-inject t))
    "Sorting"
    (("s c" proced-sort-pcpu "Sort Proced buffer by percentage CPU TIME."
      :enable t :exit t :map-inject t)
     ("s m" proced-sort-pmem "Sort Proced buffer by percentage MEMORY USAGE."
      :enable t :exit t :map-inject t)
     ("s p" proced-sort-pid "Sort Proced buffer by PID."
      :enable t :exit t :map-inject t)
     ("s s" proced-sort-start "Sort Proced buffer by time the command STARTED."
      :enable t :exit t :map-inject t)
     ("s S" proced-sort-interactive "Sort Proced buffer using SCHEME."
      :enable t :exit t :map-inject t)
     ("s t" proced-sort-time "Sort Proced buffer by CPU TIME."
      :enable t :exit t :map-inject t)
     ("s u" proced-sort-user "Sort Proced buffer by USER."
      :enable t :exit t :map-inject t))
    "Tree"
    (("T" proced-toggle-tree
      "Toggle the display of the process listing as process tree."
      :enable t :exit t :map-inject t))
    "Formatting"
    (("F" proced-format-interactive
      "Format Proced buffer using SCHEME."
      :enable t :exit t :map-inject t))
    "Operate"
    (("o" proced-omit-processes
      "Omit marked processes."
      :enable t :exit t :map-inject t)
     ("x" proced-send-signal
      "Send a SIGNAL to processes in PROCESS-ALIST."
      :enable t :exit t :map-inject t)
     ("k" proced-send-signal
      "Send a SIGNAL to processes in PROCESS-ALIST."
      :enable t :exit t :map-inject t)
     ("r" proced-renice
      "Renice the processes in PROCESS-ALIST to PRIORITY."
      :enable t :exit t :map-inject t))
    ))
  :hook
  (proced-mode . hl-line-mode)
  :init
  (setq-default proced-format 'medium)
  (entropy/emacs-lazy-with-load-trail
   auto-start-exec
   (when sys/win32p
     (dolist (el entropy/emacs-startwith-apps)
       (when (executable-find (cdr el))
         (entropy/emacs-basic-proced-auto-startwith
          (car el) (cdr el)))))))

;; *** Improve captialize function

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

(entropy/emacs-hydra-hollow-common-individual-hydra-define
 'words-manipulation nil
 '("Basic"
   (("M-c" entropy/emacs-basic-toggle-case-for-capitalize
     "Captalize Word"
     :enable t
     :exit t
     :global-bind t)
    ("M-l" entropy/emacs-basic-toggle-case-for-downcase
     "Down Case Word"
     :enable t
     :exit t
     :global-bind t)
    ("M-u" entropy/emacs-basic-toggle-case-for-upcase
     "Upcase Word"
     :enable t
     :exit t
     :global-bind t))))

(entropy/emacs-hydra-hollow-add-for-top-dispatch
 '("Basic"
   (("b w"
     (:eval
      (entropy/emacs-hydra-hollow-category-common-individual-get-caller
       'words-manipulation))
     "Words manipulation"
     :enable t
     :exit t))))

;; *** Autocompression moode

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
        (push-button load-library find-library)
        "autocompression-mode"
        "autocompression-mode"
        prompt-echo
        :pdumper-no-end t
        (auto-compression-mode 0)
        (auto-compression-mode 1))))

;; *** Replace follow input in region
;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-advice-before
   (yank xterm-paste)
   "delsel-mode-init" "delsel-mode-init" prompt-echo
   :pdumper-no-end t
   (delete-selection-mode 1)))

;; *** Inhibit gui dialog
(setq use-file-dialog nil
      use-dialog-box nil)

;; *** A minor-mode menu for the mode line
(use-package minions
  :commands (minions-minor-modes-menu
             minions-mode)
  :init
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)
  )

;; *** Description | Help mode improvement
;; **** restriction print level and length for help buffer

(entropy/emacs-advice-func-around-for-print-limit
 'describe-variable
 nil 100
 (lambda (&rest _)
   t))

;; **** lagging prompts

(defun entropy/emacs-basic--help-doc-lagging-prompt (&rest _)
  (message "Prepare for help documents ...")
  (redisplay t))
(advice-add 'describe-variable
            :before
            #'entropy/emacs-basic--help-doc-lagging-prompt)

;; *** Minibuffer Setup

;; Forcely rest plain input method while active minibuffer
(defun entropy/emacs-basic--reset-ime-in-minibuffer
    (&rest _)
  (unless (null current-input-method)
    (set-input-method nil)))
(add-hook 'minibuffer-setup-hook
          #'entropy/emacs-basic--reset-ime-in-minibuffer)

;; *** Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; ** eemacs basic hydra-hollow instances

(entropy/emacs-hydra-hollow-common-individual-hydra-define
 'eemacs-basic-config-core nil
 '("Eemacs Basic Core"
   (("f f"
     (:pretty-hydra-cabinet
      (:data
       "Frequently used commands"
       (("C-x 1" delete-other-windows
         "delete-other-window"
         :enable t :exit t :global-bind t)
        ("<f2>" entropy/emacs-basic-dhl-toggle "hl line"
         :enable t
         :exit t
         :global-bind t)
        ("<f6>" entropy/emacs-basic-loop-alpha
         "Frame Alpha"
         :enable t
         :toggle entropy/emacs-basic-loop-alpha-did
         :global-bind t)
        ("<f7>" entropy/emacs-basic-major-mode-reload
         "Reload Major"
         :enable t :exit t :global-bind t)
        ("C-<f9>" toggle-truncate-lines "toggle truncate"
         :enable t :toggle truncate-lines :global-bind t)
        ("SPC" entropy/emacs-basic-mark-set
         "Mark Set"
         :enable t :eemacs-top-bind t :exit t)
        ("C-c s s" list-processes "List Process"
         :enable t :exit t :global-bind t))))
     "Frequently used commands"
     :enable t :exit t)))
 nil '(2 2 2))

(entropy/emacs-hydra-hollow-add-for-top-dispatch
 '("Basic"
   (("b m"
     (:eval
      (entropy/emacs-hydra-hollow-category-common-individual-get-caller
       'eemacs-basic-config-core))
     "Core Operations"
     :enable t :exit t))))

;; ** Basic minor interaction commands

(defvar entropy/emacs-basic-print-variable-history nil)

(defun entropy/emacs-basic-print-variable (variable)
  "Print a variable into a transient buffer and popup to display
it with focus on."
  (interactive
   (list
    (intern
     (completing-read "Chosen variable symbol: "
                      obarray
                      (lambda (x)
                        (and (boundp x)
                             x))
                      t
                      nil
                      'entropy/emacs-basic-print-variable-history))))
  (let ((buffer (get-buffer-create "*eemacs-minor-tools/print-var*"))
        (inhibit-read-only t)
        (variable (symbol-value variable))
        (print-level nil)
        (print-length nil))
    (with-current-buffer buffer
      (fundamental-mode)
      (local-set-key (kbd "q") 'quit-window)
      (erase-buffer)
      (cond
       ((stringp variable)
        (print variable (current-buffer)))
       ((sequencep variable)
        (mapc
         (lambda (x)
           (print x (current-buffer)))
         variable))
       (t
        (print variable (current-buffer))))

      ;; truncate column while long line detected prevents lagging and
      ;; freezing and restore theh overflow content to button help
      ;; echo
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-width (- (line-end-position)
                             (line-beginning-position)))
              (over-contents ""))
          (when (> line-width (frame-width))
            (goto-char (+ (point) (frame-width)))
            (setq over-contents
                  (buffer-substring (- (point) 3)
                                    (line-end-position)))
            (replace-region-contents
             (- (point) 3)
             (line-end-position)
             (lambda (&rest _) ""))
            (insert-button "..."
                           'action
                           `(lambda (&rest _)
                              (print ,over-contents))
                           'help-echo
                           (concat "mouse-2, RET: "
                                   "Follow this link")
                           'follow-link t))
          (forward-line 1)))
      (read-only-mode 1))
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))))

(defvar entropy/emacs-basic--desc-current-var nil)
(defun entropy/emacs-basic--desc-var-preserve-var
    (orig-func &rest orig-args)
  (let ((var (car orig-args)))
    (setq entropy/emacs-basic--desc-current-var
          var)
    (apply orig-func orig-args)))

(advice-add 'describe-variable
            :around
            #'entropy/emacs-basic--desc-var-preserve-var)

(define-key help-mode-map
  (kbd "p")
  (lambda ()
    (interactive)
    (entropy/emacs-basic-print-variable
     entropy/emacs-basic--desc-current-var)))


;; * provide
(provide 'entropy-emacs-basic)
