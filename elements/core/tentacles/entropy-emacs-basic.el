;;; entropy-emacs-basic.el --- entropy emacs basic config  -*- lexical-binding: t; -*-
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
;; ** Require

(when (and entropy/emacs-indicate-sshd-session
           (display-graphic-p))
  (setq entropy/emacs-indicate-sshd-session nil))

(defun entropy/emacs-basic-set-mark-command ()
  (entropy/emacs-!set-key
    (kbd entropy/emacs-top-key)
    'set-mark-command))

(defun entropy/emacs-basic-mark-set ()
  "Mark the current point and push it to mark ring so that this
place can be easily found by other interactive command.

With prefix argument binds, jump to the previous mark place."
  (declare (interactive-only t))
  (interactive)
  (if current-prefix-arg
      (let* ((mkr (cl-delete-duplicates
                   mark-ring
                   :test 'equal))
             (mkr-top (car mkr))
             (mkr-bottom (car (last mkr)))
             (mkr-body (nbutlast (cdr mkr))))
        (when mkr
          (catch :exit
            (when (eq mkr-top mkr-bottom)
              (goto-char (marker-position mkr-top))
              (throw :exit t))
            (setq mark-ring
                  (append (cons mkr-bottom mkr-body)
                          (list mkr-top)))
            (goto-char (marker-position mkr-bottom)))))
    (save-excursion
      (push-mark))))

(entropy/emacs-lazy-with-load-trail 'top-keybinding
  :pdumper-no-end t
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

;; ** Personal infomation
(when (and entropy/emacs-user-full-name
           entropy/emacs-user-mail-address)
  (setq user-full-name entropy/emacs-user-full-name)
  (setq user-mail-address entropy/emacs-user-mail-address))

;; ** Temporal bug revert
;; *** gnutls bug for emacs version upper than '26.1'
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


;; *** Disable `auto-mode-alist' case fold search

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

;; *** Disable gvfs

;; EEMACS_MAINTENANCE
;; Since tramp archive using simple magick filename regexp
;; matching, and its internal
;; `tramp-archive-file-name-handler-alist''s each corresponding
;; implementation can not follow the equalization API defination of
;; origin function, thus for most of tramp internal errors will
;; pollute the thread operations. (e.g. local directory naming as an
;; archive name will also invoking tramp-archive methods which throw
;; out many more problems while its magick filename I/O deals)
(if (> emacs-major-version 27)
    (with-eval-after-load
        ;; FIXME: we need to do this after load `tramp-archive' or may
        ;; cause the invalid file-name-handler error in emacs-28 and
        ;; why?
        ;;
        ;; Try below elisp snippets:
        ;;
        ;; #+begin_src elisp
        ;;   (entropy/emacs-test-emacs-with-pure-setup-with-form
        ;;    "tramp-archive-enabled-bug-reproduce"
        ;;    :use-current-package-user-dir nil
        ;;    :emacs-invocation-name "emacs-28.1"
        ;;    '(progn
        ;;       (setq debug-on-error t)
        ;;       (setq tramp-archive-enabled nil)
        ;;       (file-directory-p
        ;;        "/home/entropy/.config/entropy-config/\
        ;;   entropy-emacs/entropy-emacs/annex/emacs-src/\
        ;;   test/lisp/net/tramp-archive-resources/foo.iso/foo")
        ;;       ))
        ;; #+end_src
        'tramp-archive
      (setq tramp-archive-enabled nil))
  (setq tramp-archive-enabled nil))

;; ** Basic major-modes spec
;; *** Dired config
;; **** dired basic
(use-package dired
  :ensure nil
  :defines (dired-do-revert-buffer)
;; ***** init
  :init

  ;; Enable `truncate-lines' in dired buffer
  (add-hook 'dired-mode-hook
            #'(lambda () (setq truncate-lines t)))

;; ****** pretty-hydra
  (defvar entropy/emacs-basic-dired-hydra-hollow-cabinets
    '("Basic"
      (("RET" dired-find-file "Open item dwim"
        :enable t
        :exit t
        :map-inject t)
       ("M-o" entropy/emacs-dired-find-file-other-window "Open item in other window"
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
       ("M-<up>" entropy/emacs-dired-Meta-up "Go/move point to parent dired-buffer/dir-node"
        :enable t
        :map-inject t
        :exit t)
       ("M-<down>" entropy/emacs-dired-Meta-down "Go/move point to next upgrade sibling dired-buffer/dir-node"
        :enable t
        :map-inject t
        :exit t)
       ("B" entropy/emacs-basic-dired-backup-files "Common Backup"
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
                :enable t :map-inject t :exit t)
               ("C-<up>" dired-prev-marked-file "Move to the ARGth previous marked file."
                :enable t :map-inject t :exit t)
               ("C-<down>" dired-next-marked-file "Move to the ARGth next marked file."
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
                :enable t :map-inject t :exit t))
              "Eemacs special mark commands"
              (("s o"
                entropy/emacs-basic-dired-mark-special-nodes/dir-has-onlyone-subdir-recursively
                "Mark one subdir nesting nodes"
                :enable t :map-inject nil :exit t)))
             :pretty-hydra-category-width-indicator
             (2 2)
             :other-rest-args
             ((dired dired-mode-map)))
        "Dired mark commands"
        :enable t :exit t)
       ("w" dired-copy-filename-as-kill "Copy names of marked (or next ARG) files into the kill ring"
        :enable t :map-inject t :exit t)
       ("y" dired-show-file-type "Print the type of FILE, according to the ‘file’ command"
        :enable t :map-inject t :exit t)
       ("x" entropy/emacs-basic-dired-show-file-xdg-mime-type
        "Print the xdg-mime type of FILE, according to the 'xdg-mime' command."
        :enable sys/is-linux-and-graphic-support-p :map-inject t :exit t)
       ("C" dired-do-copy "Copy all marked (or next ARG) files, or copy the current file"
        :enable t :map-inject t :exit t)
       ("M" entropy/emacs-dired-do-hard-or-symbolic-links-union-processor
        "Mirror(symlink/hardlink) all marked files, or mirror the current file"
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
       ("i" entropy/emacs-image-dired-init
        "Inital `image-dired' in current dired buffer"
        :enable t :exit t))
      "Misc."
      (("p" entropy/emacs-basic-get-dired-fpath "Get Node Path"
        :enable t
        :map-inject t
        :exit t)
       ("M-l" entropy/emacs-basic--dired-add-to-load-path "Add path"
        :enable t
        :map-inject t
        :exit t)
       ("T" entropy/emacs-basic-dired/print-tree "Show/Print marked nodes tree"
        :enable t
        :map-inject t
        :exit t)
       ("A" entropy/emacs-dired-do-counts-marked-files-recursively
        "Count marked items recursively"
        :enable t
        :map-inject t
        :exit t)
       )))

  (entropy/emacs-lazy-initial-advice-before
   '(dired-mode)
   "hydra-hollow-init-for-dired"
   "hydra-hollow-init-for-dired"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
    'dired-mode '(dired dired-mode-map) t
    entropy/emacs-basic-dired-hydra-hollow-cabinets
    '(2 2)))

;; ***** config
  :config
;; ****** Basic configs
;; ******* Unbind some unusual keys which may cause mistakes or its dangerous

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

;; ******* Set unit of dired inode for human readable

  (if (not sys/is-win-group)
      ;; because of windows dired list is too long so just let it in linux
      (setq dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

;; ******* Hide details defautly
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; ******* Always delete and copy resursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

;; ****** Core advice
;; ******* Patch for `dired-mark-pop-up'

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (eval
   '(cond
     ((version< emacs-version "28")
      (defun __ya/dired-mark-pop-up (buffer-or-name op-symbol files function &rest args)
        "Like `dired-mark-pop-up' but has been modified to be compat with =entropy-emacs=.

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
                (setq tab-line-exclude nil)))))))
     (t
      (defun __ya/dired-mark-pop-up (buffer-or-name op-symbol files function &rest args)
        "like `dired-mark-pop-up' but has been modified to be compat with =entropy-emacs=.

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
            (with-current-buffer-window
                buffer
                `(display-buffer-below-selected
                  ;; HACK: we just want to preserve the size
                  ;; (window-height . fit-window-to-buffer)
                  (preserve-size . (nil . t))
                  (body-function
                   . ,#'(lambda (_window)
                          ;; Handle (t FILE) just like (FILE), here.  That value is
                          ;; used (only in some cases), to mean just one file that was
                          ;; marked, rather than the current line file.
                          (dired-format-columns-of-files
                           (if (eq (car files) t) (cdr files) files))
                          (remove-text-properties (point-min) (point-max)
                                                  '(mouse-face nil help-echo nil))
                          (setq tab-line-exclude nil))))
                #'(lambda (window _value)
                    (with-selected-window window
                      (unwind-protect
                          (apply function args)
                        (when (window-live-p window)
                          (quit-restore-window window 'kill))))))))))))

  (advice-add 'dired-mark-pop-up
                  :override
                  #'__ya/dired-mark-pop-up)

;; ****** Eemacs spec `dired' commands
;; ******* Dired sibling directory navigation dwim

  (defun entropy/emacs-dired-Meta-up ()
    "Goto parent `dired' buffer of `dired-current-directory' or goto
the parent subtree sibling using `dired-sutree-up'."
    (declare (interactive-only t))
    (interactive)
    (let ((in-dired-subtree-p
           (and (fboundp 'dired-subtree--get-ov)
                (dired-subtree--get-ov))))
      (cond
       (in-dired-subtree-p
        (dired-subtree-up))
       (t
        (dired-up-directory)))))

  (defun entropy/emacs-dired-Meta-down ()
    "Downup to parent next sibling `dired' buffer of
`dired-current-directory' or goto the next upgrade level 1
subtree sibling using `dired-subtree''s subroutine."
    (declare (interactive-only t))
    (interactive)
    (let ((in-dired-subtree-p
           (and (fboundp 'dired-subtree--get-ov)
                (dired-subtree--get-ov)))
          orig-pt)
      (cond
       (in-dired-subtree-p
        (goto-char (overlay-end in-dired-subtree-p))
        (dired-move-to-filename))
       (t
        (dired-up-directory)
        (setq orig-pt (point))
        (unless
            (catch :exit
              (while (and (= 0 (forward-line 1))
                          (not (eobp)))
                (when (and
                       ;; We've replaced `file-directory-p' with the
                       ;; regexp test to speed up filters over TRAMP.
                       ;; So long as dired/ls format doesn't change,
                       ;; we're good.  'd' for directories, 'l' for
                       ;; potential symlinks to directories.
                       (looking-at "..[dl]")
                       (file-directory-p (dired-get-filename)))
                  (dired-find-file)
                  (throw :exit t)))
              nil)
          (goto-char orig-pt)
          (user-error "No more subdirectories for dired host <%s>"
                      (dired-current-directory)))
        ))))

;; ******* Yet another `narrow-to-region' to suite for `dired'

  (defun entropy/emacs-dired-narrow-to-region ()
    "Like `narrow-to-region' but with `dired' specfications and
adapting thus and just enable when `region-active-p' is
predicated."
    (declare (interactive-only t))
    (interactive)
    (when (region-active-p)
      (let ((regbeg (region-beginning))
            (regend (region-end)))
        (deactivate-mark)
        (when (save-excursion (goto-char regend)
                              (= 1 (line-number-at-pos nil t)))
          (user-error "Region just includ top line of current dired-buffer!"))
        (cond
         (
          ;; when at the top of buffer line, we should escase all
          ;; title matching for convention.
          (and (not (buffer-narrowed-p))
               (= (line-number-at-pos) 1))
          (setq regbeg (line-end-position)))
         (t
          ;; defaulty we should take the region begin at the previous
          ;; line end point thus the region can include all text
          ;; properties of current-line.
          (unless (eq (progn (goto-char regbeg) (line-beginning-position))
                      regbeg)
            (setq regbeg (line-end-position 0)))))
        ;; for the region end is like for the default case of region
        ;; beg but using reversed thoughts.
        (setq regend
              (progn (goto-char regend)
                     (if (= regend (line-beginning-position))
                         regend
                       (forward-line 1)
                       (point))))
        (narrow-to-region regbeg regend))))
  (define-key dired-mode-map [remap narrow-to-region]
    #'entropy/emacs-dired-narrow-to-region)

  (cl-defun entropy/emacs-dired-narrow--to-marked-files-within-region
      (&key begin-pt end-pt
            jump-to-first-marked-file
            use-region-selection)
    "Remove all unmarked valid dired lines in `dired-mode' within
region of BEGIN-PT to END-PT (default to `point-min' and
`point-max').

Re-pos to the first marked dired line when
JUMP-TO-FIRST-MARKED-FILE is non-nil.

When USE-REGION-SELECTION is set, using `region-beginning' and
`region-end' when `region-active-p' return non-nil, in which case
cover the begin and end point sets."
    (entropy/emacs-do-error-for-major-mode-incompatible 'dired-mode)
    (if (and use-region-selection
             (region-active-p))
        (setq begin-pt (region-beginning)
              end-pt (region-end))
      (setq begin-pt (or begin-pt (point-min))
            end-pt (or end-pt (point-max))))
    (let* ((begin-pt (entropy/emacs-save-excurstion-and-mark-and-match-data
                       (goto-char begin-pt)
                       (goto-char (line-beginning-position))))
           (end-pt (entropy/emacs-save-excurstion-and-mark-and-match-data
                     (goto-char end-pt)
                     (goto-char (line-end-position))))
           (begin-marker (set-marker (make-marker) begin-pt))
           (end-marker (set-marker (make-marker) end-pt))
           (judge-func nil)
           (njudge-func nil))
      (setq judge-func
            (lambda nil
              (and (entropy/emacs-dired-fname-line-p)
                   (entropy/emacs-dired-marked-line-p)))
            njudge-func
            (lambda ()
              (or
               ;; the line display top dir or top subdir
               (not (entropy/emacs-dired-fname-line-p))
               ;; dired subtree related parents
               (let* ((ovs (bound-and-true-p dired-subtree-overlays))
                      curpt-lnum
                      curpt-nextline-start-pt
                      curpt-is-dsubt-p
                      curpt-dsubt-ov curpt-dsubt-next-line-ov
                      cur-dsubt-ov-depth
                      dsubt-markpos dsubt-markpos-ov dsubt-markpos-ov-depth
                      dsubt-markpos-get-func)
                 (setq dsubt-markpos-get-func
                       (lambda (ov)
                         (when (entropy/emacs-is-valid-overlay-p
                                ov
                                :in-buffer (current-buffer))
                           (setq dsubt-markpos
                                 (entropy/emacs-dired-region-has-marked-lines-p
                                  (point) (overlay-end ov) 'line-beg))
                           dsubt-markpos)))
                 (when (and ovs
                            ;; pre conditions
                            (not
                             (and
                              (string-match-p
                               "^ *$"
                               (entropy/emacs-get-buffer-pos-line-content
                                :without-properties t))
                              (entropy/emacs-dired-subtree-api-get-overlay-at-pt)))
                            )
                   (setq curpt-dsubt-ov
                         (entropy/emacs-dired-subtree-api-get-overlay-at-pt)
                         curpt-is-dsubt-p curpt-dsubt-ov
                         curpt-nextline-start-pt (1+ (line-end-position))
                         curpt-lnum (line-number-at-pos))
                   (when (> curpt-nextline-start-pt end-marker)
                     (setq curpt-nextline-start-pt nil))
                   (when
                       (or
                        (and curpt-is-dsubt-p
                             (funcall dsubt-markpos-get-func curpt-dsubt-ov)
                             (< (point) dsubt-markpos)
                             (< (entropy/emacs-setq-single-with-explicit-return
                                  cur-dsubt-ov-depth
                                  (overlay-get curpt-dsubt-ov 'dired-subtree-depth))
                                (entropy/emacs-setq-single-with-explicit-return
                                  dsubt-markpos-ov-depth
                                  (overlay-get
                                   (setq dsubt-markpos-ov
                                         (entropy/emacs-dired-subtree-api-get-overlay-at-pt
                                          dsubt-markpos))
                                   'dired-subtree-depth)))
                             (or
                              (= (+ curpt-lnum 1)
                                 (line-number-at-pos (overlay-start dsubt-markpos-ov)))
                              (catch :exit
                                (let ((parent-ov dsubt-markpos-ov))
                                  (while (setq
                                          parent-ov
                                          (entropy/emacs-dired-subtree-api-get-overlay-at-pt
                                           (1- (overlay-start parent-ov))))
                                    (when (= (line-number-at-pos (1- (overlay-start parent-ov)))
                                             curpt-lnum)
                                      (throw :exit t)))))))
                        (and (not curpt-is-dsubt-p)
                             curpt-nextline-start-pt
                             (setq curpt-dsubt-next-line-ov
                                   (entropy/emacs-dired-subtree-api-get-overlay-at-pt
                                    curpt-nextline-start-pt))
                             (funcall dsubt-markpos-get-func
                                      curpt-dsubt-next-line-ov)))
                     ;; finally return non-nil
                     t))))))
      (entropy/emacs-dired-map-lines
       :use-region (cons begin-marker end-marker)
       :map-func
       (lambda ()
         (let (rtn)
           (catch :exit
             (when (funcall judge-func)
               (throw :exit nil))
             (unless (funcall njudge-func)
               (let ((inhibit-read-only t))
                 (when (entropy/emacs-buffer-delete-line-at-pos
                        :bound end-marker)
                   (setq rtn 'pause-iterate))))
             rtn))))
      (when jump-to-first-marked-file
        (goto-char begin-marker)
        (entropy/emacs-dired-jump-to-next-fname-line
         1
         :use-current-line t
         :bound (marker-position end-marker)
         :use-filter 'mark
         :move-to-filename t))))

  (defun entropy/emacs-dired-narrow-to-marked-files ()
    "Narrow `dired' buffer contents to only marked files with
necessary extra informations preserved like dir header (or subdir
header) and `dired-subtree' parent context's hierarchy.

Type `\\[revert-buffer]' to exhibit original full buffer
contents."
    (declare (interactive-only t))
    (interactive nil dired-mode)
    (entropy/emacs-do-error-for-major-mode-incompatible 'dired-mode)
    (entropy/emacs-dired-narrow--to-marked-files-within-region
     :jump-to-first-marked-file t
     :use-region-selection t))

;; ******* Yet another `dired-find-file-other-window'

  (defun entropy/emacs-dired-find-file-other-window ()
    "Like `dired-find-file-other-window' but tied toi eemacs spec."
    (declare (interactive-only t))
    (interactive nil dired-mode)
    (unless (eq major-mode 'dired-mode)
      (user-error "Can not invoke this command out of `dired-mode'."))
    (let* ((current-relfname (or (let ((f (dired-get-filename nil t)))
                                   (ignore-errors
                                     (entropy/emacs-make-relative-filename
                                      f
                                      ;; NOTE: we can not use
                                      ;; `dired-current-directory' since
                                      ;; it has been hacked by
                                      ;; `dired-subtree' which also
                                      ;; recognize the subtree item path
                                      default-directory)))
                                 (user-error "No on an dired file line!")))
           (file-use-external-p (and
                                 (not (file-directory-p current-relfname))
                                 (entropy/emacs-find-file-judge-filename-need-open-with-external-app-p
                                  current-relfname))))
      (if file-use-external-p
          (find-file current-relfname)
        (unless (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
          (when (bound-and-true-p entropy/emacs-window-center-mode)
            (entropy/emacs-window-center-mode 0)))
        (if (> (length (window-list)) 1) (delete-other-windows))
        (progn
          (entropy/emacs-no-same-buffer-split-window-horizontally
           (ceiling (* 0.18
                       (frame-width))))
          (other-window 1)
          (if (file-directory-p current-relfname)
              (dired current-relfname)
            (find-file current-relfname))))))

;; ******* Delete directory with force actions

  (defvar entropy/basic--dired-recursive-delete-prompt-buffer-name
    "*eemacs delete file error*")

  (defvar entropy/emacs-basic--dired-delete-file-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q") #'delete-window)
      map)
    "Key map for `entropy/emacs-basic--dired-delete-file-mode'.")

  ;; TODO : comlete `entropy/emacs-basic--dired-delete-file-mode'
  (define-minor-mode entropy/emacs-basic--dired-delete-file-mode
    "Minor mode for func `entropy/emacs-basic-dired-delete-file-recursive'."
    :keymap entropy/emacs-basic--dired-delete-file-mode-map
    :global nil
    (setq buffer-read-only t))

  (defvar entropy/emacs-basic--dired-file-current-delete nil
    "Current file pre deleted by
`entropy/emacs-basic-dired-delete-file-recursive'.")

  (defvar entropy/emacs-basic--dired-files-deleted-history nil
    "Actually file deleted by
`entropy/emacs-basic-dired-delete-file-recursive', as an history
log variable.")

  (defvar entropy/emacs-basic--dired-delete-file-refer-files-buffer-history nil
    "Files buffer killed by
`entropy/emacs-basic-dired-delete-file-recursive' history
variable.")

  (defvar entropy/emacs-basic--dired-delete-file-refer-dired-buffers-history nil
    "Dired buffers killed by `entropy/emacs-basic--dired-delete-file-rescursie'
history log.")

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
           cur-file-type
           (pbufname entropy/basic--dired-recursive-delete-prompt-buffer-name)
           (prompt-buffer (with-current-buffer (get-buffer-create
                                                pbufname)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert
                               "========== File Deletion Error Handle Prompt Buffer ==========\n"))
                            (current-buffer)))
           error-occurred
           (count 0))

      (unless just-kill-refers
        (entropy/emacs-basic--dired-delete-file-prompt base-files))

      (dolist (file base-files)
        (cl-incf count)

        ;; killed refer buffers.
        (dolist (el (buffer-list))
          (let* ((buffer-file (buffer-file-name el)))
            (when (and (buffer-live-p el)
                       buffer-file
                       (entropy/emacs-existed-filesystem-nodes-equal-p file buffer-file))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-files-buffer-history
                           (cons (buffer-name el) (current-time-string)))
              (message "[eemacs-dired-delete-file]: killed file <%s> referred buffer '%s'"
                       file el)
              (kill-buffer el))))

        ;; killed refer dired buffers
        (dolist (el dired-buffers)
          (let* ((dir (car el))
                 (dirname-sans (entropy/emacs-directory-file-name dir))
                 (fname-sans (entropy/emacs-directory-file-name file))
                 (fname-dirp (or (file-directory-p file)
                                 ;; if file not exist then we match the name string
                                 (directory-name-p file)))
                 (buffer (cdr el)))
            (when (and
                   (buffer-live-p buffer)
                   (if (eq major-mode 'dired-mode)
                       (not (eq buffer (current-buffer)))
                     t)
                   (entropy/emacs-existed-filesystem-nodes-equal-p
                    (if fname-dirp
                        ;; NOTE: we must delete the trailing slash of
                        ;; the dired directory container retrieved by
                        ;; each car of the element of `dired-buffer'
                        ;; since it may cause handle magick filenames
                        ;; using `tramp-archive-file-name-handler'
                        ;; such as directory name includes '.*.tar/'
                        ;; etc. , which may cause the loop break with
                        ;; specified handle error by gvfs.
                        dirname-sans
                      ;; fake file as under the curret dired dir
                      (expand-file-name
                       (file-name-nondirectory fname-sans) dir))
                    fname-sans
                    'chase-link))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-dired-buffers-history
                           (cons el (current-time-string)))
              (message "[eemacs-dired-delete-file]: killed file <%s> referred dired buffer '%s'"
                       file buffer)
              (kill-buffer buffer))))

        ;; Do deletion
        (when (not just-kill-refers)
          (condition-case this-error
              (progn
                (setq entropy/emacs-basic--dired-file-current-delete (list file))
                (cond ((f-symlink-p file)
                       (setq cur-file-type 'symbol_link)
                       (delete-file file))
                      ((f-file-p file)
                       (setq cur-file-type 'file)
                       (delete-file file))
                      ((f-directory-p file)
                       (setq cur-file-type 'directory)
                       (delete-directory file t)))
                (push (list :file-type cur-file-type
                            :file-path file
                            :date (current-time-string))
                      entropy/emacs-basic--dired-files-deleted-history)
                (cl-case cur-file-type
                  (symbol_link
                   (entropy/emacs-safety-message (format "Delete symbolink '%s' done! -v-" file)))
                  (file
                   (entropy/emacs-safety-message (format "Delete file '%s' done! -v-" file)))
                  (directory
                   (entropy/emacs-safety-message (format "Delete directory '%s' done! -v-" file)))))
            (error
             (setq error-occurred t)
             (let* ((inhibit-read-only t))
               (unless (buffer-live-p prompt-buffer)
                 (error "buffer creation error for %s" pbufname))
               (with-current-buffer prompt-buffer
                 (insert
                  (format "[At %sth operation] %s: %s %s (%s) \n\tsince %s\n"
                          count
                          cur-file-type
                          (propertize "deletion failed" 'face 'error)
                          (file-name-nondirectory file)
                          file
                          (cdr this-error)))))))))

      ;; update dired buffer after deletion
      (when (and (not just-kill-refers)
                 (equal major-mode 'dired-mode)
                 (= count did-times))
        (revert-buffer))
      ;; error prompt
      (when error-occurred
        (with-current-buffer prompt-buffer
          (let ((inhibit-read-only t))
            (insert "========== Prompt End ==========")
            (entropy/emacs-basic--dired-delete-file-mode)))
        (pop-to-buffer prompt-buffer))))

  (defun entropy/emacs-basic-dired-delete-file-refers ()
    "Kill all refers of dired markd file of directories."
    (interactive)
    (entropy/emacs-basic-dired-delete-file-recursive nil t))


;; ******* Get both UNIX and WINDOWS style path string
  (defvar entropy/emacs-basic-dired-fpath-get-log nil
    "The log list stored file path temporarily, will be reset
when you call `entropy/emacs-basic-get-dired-fpath'.")

  (defun entropy/emacs-basic-get-dired-fpath (type)
    "Get marked files path and push them to the `kill-ring'."
    (declare (interactive-only t))
    (interactive
     (list (completing-read "Choose path string type: "
                            '("unix" "win32")))
     dired-mode)
    (let (rtn
          (files_get (or (dired-get-marked-files)
                         (list default-directory)))
          ;; (curl-marked-p
          ;;  (save-excursion
          ;;    (forward-line 0)
          ;;    (save-match-data
          ;;      (re-search-forward (dired-marker-regexp)
          ;;                         (line-end-position) t))))
          (curl-fname (dired-get-filename))
          (killring-save-func
           (lambda (&rest strs)
             (dolist (el strs)
               (with-temp-buffer
                 (when buffer-read-only
                   (setq buffer-read-only nil))
                 (goto-char (point-min))
                 (insert el)
                 (kill-ring-save (point-min) (point-max))
                 (entropy/emacs-safety-message
                  "Save '%s' to kill-ring."
                  el))))))
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
        (apply killring-save-func rtn))
       (t
        (setq rtn (reverse rtn)
              entropy/emacs-basic-dired-fpath-get-log nil
              entropy/emacs-basic-dired-fpath-get-log rtn)

        (funcall killring-save-func curl-fname)
        (message "Save all path string to log variable \
'entropy/emacs-basic-dired-fpath-get-log' and the current line filename to the `kill-ring'.")
        ))))

;; ******* Dired add load path
  (defun entropy/emacs-basic--dired-add-to-load-path ()
    "Add markd directories (or directory of files) to `load-path'."
    (declare (interactive-only t))
    (interactive nil dired-mode)
    (let ((dirs (or (dired-get-marked-files)
                    (list (dired-current-directory)))))
      (dolist (el dirs)
        (unless (file-directory-p el)
          (setq el (file-name-directory el)))
        (add-to-list 'load-path el))))

;; ******* Dired backup file

  (defun entropy/emacs-basic-dired-backup-files ()
    "Backup current file or dircectory in current host dir."
    (declare (interactive-only t))
    (interactive nil dired-mode)
    (let ((files (dired-get-marked-files)))
      (dolist (el files)
        (when (file-exists-p el)
          (entropy/emacs-simple-backup-file el)))
      (revert-buffer)))

;; ******* Dired mime query

  ;; like `dired-show-file-type' but show xdg mime type.
  (defun entropy/emacs-basic-dired-show-file-xdg-mime-type (file)
    "Print the xdg mime type of FILE and its default desktop
application handler when exist, according to the `xdg-mime'
command.

With prefix arg, also save the result to `kill-ring'
respectively."
    (interactive (list (dired-get-filename t)))
    (let (process-file-side-effects
          (kill-ring-save-p current-prefix-arg)
          (show-default-app-p
           ;; TODO: shell we need to set this as an option?
           t)
          the-mime-type the-default-app)
      (condition-case err
          (with-temp-buffer
            (process-file "xdg-mime" nil t t
                          "query" "filetype"
                          (expand-file-name file))
            (when (bolp)
              (backward-delete-char 1))
            (setq the-mime-type (buffer-string))
            (when kill-ring-save-p
              (kill-new the-mime-type))
            (when show-default-app-p
              (erase-buffer)
              (process-file "xdg-mime" nil t t
                            "query" "default"
                            the-mime-type)
              (setq the-default-app (buffer-string))
              (if (string-empty-p the-default-app)
                  (setq the-default-app nil)
                (setq the-default-app
                      (replace-regexp-in-string
                       "\n" "" the-default-app))
                (when kill-ring-save-p
                  (kill-new the-default-app))))
            (cond
             (the-default-app
              (message "mime-type: %s default-appliction: %s"
                       (propertize
                        the-mime-type   'face
                        'entropy/emacs-defface-simple-color-face-yellow)
                       (propertize
                        the-default-app 'face
                        'entropy/emacs-defface-simple-color-face-green)))
             (t
              (message "%s"
                       the-mime-type))))
        (error
         (user-error "%s" err)))))

;; ******* Dired mark special nodes
;; ******** Mark one-subdir nesting structure nodes
  (defun entropy/emacs-basic-dired-mark-special-nodes/dir-has-onlyone-subdir-recursively
      (arg)
    "Mark dired nodes whose sublevel nesting structure as:

#+begin_example
dir0
|_dir1
  |_dir2
    |_dir3
      ...
#+end_example

Thus on that each sub-level of the node just has one dir node and so
on recursively based on depth level ARG. And we also disallow any
files on each level.

In interaction mode, ARG is caluated by `prefix-numeric-value' on
`current-prefix-arg', so that both 'C-u C-u .. ' and 'C-u NUM' are
acceptable. If `current-prefix-arg' is nil or ARG less than 1, we
fallbackk to 1."
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (unless (eq major-mode 'dired-mode)
      (user-error "[Error]: current buffer is not an 'dired' buffer"))
    ;; NOTE: firstly set off the region since `dired-mark' may marked
    ;; all nodes within region?
    (deactivate-mark)
    ;; NOTE: firstly we need to unmark all marks
    (let ((inhibit-message t))
      (dired-unmark-all-marks))
    (let ((level (cond
                  ((null arg)
                   1)
                  ((listp arg)
                   (floor
                    (log (car arg) 4)))
                  (t
                   (if (> arg 0)
                       arg
                     1))))
          judge-func
          cur-dir
          satisfied-stack)

      (setq judge-func
            (lambda (dir restrict-level)
              (let* ((cur-level 0)
                     curdir-subitems
                     curdir-subfiles
                     curdir-subdirs
                     (nodes-get-func (lambda (type)
                                       (delete nil
                                               (mapcar
                                                (lambda (x)
                                                  (and (eq type (car x))
                                                       (cdr x)))
                                                curdir-subitems))))
                     curdir-subdirs-len)
                (catch :exit
                  (while t
                    ;; Do not list dir while out restriction although
                    ;; its has non-effects but its useless and may
                    ;; cause performance issue since large diretory
                    ;; listing take long time.
                    (unless (= cur-level restrict-level)
                      (setq curdir-subitems
                            (entropy/emacs-list-dir-lite
                             dir)
                            curdir-subfiles
                            (funcall nodes-get-func 'file)
                            curdir-subdirs
                            (funcall nodes-get-func 'dir)
                            curdir-subdirs-len
                            (length
                             curdir-subdirs)))
                    (cond
                     ;; Top condition while loop to throw t since all filters
                     ;; missing matching thus.
                     ((= cur-level restrict-level)
                      (throw :exit t))
                     ;; No subdirs found
                     ((= curdir-subdirs-len 0)
                      (throw :exit nil))
                     ;; subdirlen larger than restiction
                     ((> curdir-subdirs-len 1)
                      (throw :exit nil))
                     ;; should not has any subfiles
                     (curdir-subfiles
                      (throw :exit nil))
                     )
                    (setq dir (car curdir-subdirs))
                    (cl-incf cur-level))))))

      ;; main
      (goto-char (point-min))
      (catch :exit
        (while t
          (and
           (dired-move-to-filename)
           (throw :exit t))
          (forward-line 1)
          (when (eobp)
            (user-error
             "[Error] No valid node found in current 'dired' buffer!"))))
      (catch :exit
        (while (not (eobp))
          (if (and (or (setq cur-dir (dired-get-filename))
                       (error "[Debug] inner error"))
                   (file-directory-p cur-dir))
              (if (funcall judge-func
                           cur-dir level)
                  (progn (dired-mark 1)
                         (push (file-name-nondirectory cur-dir)
                               satisfied-stack))
                (unless (dired-next-line 1)
                  (throw :exit nil)))
            (unless (dired-next-line 1)
              (throw :exit nil)))))
      (if satisfied-stack
          (progn
            (goto-char (point-min))
            ;; goto the first marked
            (dired-next-marked-file 1)
            (message "[OK] Marked %s dirs that satisfied %s level single subdir structure"
                     (length satisfied-stack)
                     level))
        (user-error "No dirs matched %s level single subdir structure"
                    level))))


;; ******* Dired tree print

  (defun entropy/emacs-basic-dired/print-tree (arg)
    "Print dired nodes as shell command 'tree' like thus with depth
restriction ARG, and popup those buffers.

In interaction mode, ARG is caluated by `prefix-numeric-value' on
`current-prefix-arg', so that both 'C-u C-u .. ' and 'C-u NUM'
are acceptable. If `current-prefix-arg' is nil we fallbackk to 1,
or if 0 we use `nil' which is for non level restriction.

This function also can print the tree as `org-mode' style with a
interaction hint."
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (unless (eq major-mode 'dired-mode)
      (user-error "[Error]: current buffer is not an 'dired' buffer"))
    (let ((with-level
           (cond
            ((null arg)
             1)
            ((listp arg)
             (floor
              (log (car arg) 4)))
            (t
             (if (> arg 0)
                 arg
               nil))))
          (nodes (or (dired-get-marked-files)
                     (list (dired-current-directory))))
          (use-org-style
           (yes-or-no-p "Use org style? "))
          do-func
          buffer
          did-items)
      (setq do-func
            (lambda (dirs level)
              (let ((buffer (generate-new-buffer
                             (format "*eemacs-dired-print-tree (using level %s)*"
                                     level)))
                    (inhibit-read-only t))
                (with-current-buffer buffer
                  (erase-buffer)
                  (dolist (node dirs)
                    (when (file-directory-p node)
                      (push node did-items)
                      (unless use-org-style
                        (insert (format "* %s\n" node))
                        (insert "#+begin_example\n"))
                      (entropy/emacs-print-dir-recursively
                       node buffer
                       t
                       :with-level level
                       :use-org-style use-org-style)
                      (unless use-org-style
                        (insert "#+end_example"))
                      (insert "\n")))
                  (if did-items
                      (progn (org-mode)
                             ;; NOTE: we must put the `org-mode'
                             ;; specifiction after the mode enabled
                             ;; since some sets maybe covered by its
                             ;; hooks.
                             (setq-local org-pretty-entities nil
                                         org-pretty-entities-include-sub-superscripts nil)
                             (setq buffer-read-only t))
                    (kill-buffer buffer)))
                buffer)))
      (setq buffer (funcall do-func nodes with-level))
      (when (and (bufferp buffer)
                 (buffer-live-p buffer))
        (pop-to-buffer buffer))
      (unless did-items
        (user-error "[Error]: can not found any nodes can be print its tree!"))))

;; ******* Dired do hardlinks (can be used for directory as well) or symbolic links

  (defvar-local entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list nil)

  (defun entropy/emacs-basic--dired-do-hard-or-symbolic-link-prompt
      (op-type files-list)
    "A alternative `dired-mark-pop-up' for
`entropy/emacs-dired-do-hard-or-symbolic-links-union-processor'
predicating files with prompting and throw an error for quit when
user refuse the prompts."
    (let ((op-name
           (cond
            ((eq op-type 'hardlink)
             "Hardlink")
            ((eq op-type 'symlink)
             "Symbolic-link")
            (t
             (error "wrong type of op-type: %s"
                    op-type)))))
      (if (dired-mark-pop-up
           (format " *eemacs dired do %s to*" op-name)
           (cond
            ((eq op-type 'hardlink)
             'hardlink)
            ((eq op-type 'symbolic-link)
             'symlink))
           files-list
           'yes-or-no-p
           (format "Do eemacs files mirror %s %s "
                   op-name
                   (dired-mark-prompt nil files-list)))
          t
        (error "Cancel %s files!"
               op-name))))

  (defun entropy/emacs-dired-do-hard-or-symbolic-links-union-processor
      (use-hardlink)
    "Do files mirror using (symlink/hardlink) powered by
`entropy/emacs-do-directory-mirror' for `dired-get-marked-files'
in current `dired' buffer. Use symbolic link type defautly unless
`current-prefix-arg' is non-nil."
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (unless (eq major-mode 'dired-mode)
      (user-error "Not in dired buffer"))
    (setq entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list nil)
    (let* ((op-type (if use-hardlink 'hardlink 'symlink))
           (base-dir-abs-path (expand-file-name default-directory))
           (nodes (let ((dfiles (dired-get-marked-files)))
                    (unless dfiles
                      (user-error "No files get from this dired buffer!"))
                    (or (and (entropy/emacs-basic--dired-do-hard-or-symbolic-link-prompt
                              op-type dfiles)
                             dfiles)
                        (user-error "Operation cancled"))))
           (host-target (read-file-name "Choose target host: "
                                   nil nil t nil 'file-directory-p))
           (file-target nil)
           (log-success-files 0)
           (log-success-dirs 0)
           (log-fatal-files 0)
           (log-fatal-dirs 0)
           (log-allop-success-p-func
            (lambda ()
              (and (= 0 log-fatal-files)
                   (= 0 log-fatal-dirs))))
           (log-buffer (generate-new-buffer
                        (format "*eemacs-dired-do-hardlink-<%s>*"
                                default-directory)))
           log-string-list
           ;; functions
           (node-msg-func
            (lambda (ftype src dest)
              (message "Do %s for %s type from '%s' to '%s' ..."
                       op-type ftype src dest)))
           (summary-msg-info-get-func
            (lambda ()
              (message "%s (%s/%s) dir created/failed, (%s/%s) files did/failed for %s operation"
                       (if (funcall log-allop-success-p-func)
                           (propertize "SUCCESS" 'face 'success)
                         (propertize "ERROR" 'face 'error))
                       log-success-dirs
                       log-fatal-dirs
                       log-success-files
                       log-fatal-files
                       op-type)))
           summary-msg-info-str
           (summary-msg-message-func
            (lambda ()
              (message "%s" summary-msg-info-str)))
           (node-type-get-func
            (lambda (x def)
              (let ((base-type (list def))
                    (base-fattrs nil))
                ;; fistly trim the directory-name indicator since we
                ;; must treat it as a file before checking its type
                (setq x (entropy/emacs-directory-file-name x))
                (when (file-symlink-p x)
                  (push 'symlink base-type))
                (setq base-fattrs
                      (entropy/emacs-get-filesystem-node-attributes x))
                (when (> (plist-get base-fattrs :link-number) 1)
                  (push 'hardlink base-type))
                (reverse base-type))))
           (log-append-func
            (lambda (x group-log-p)
              (if group-log-p
                  (setq entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list
                        (append
                         entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list
                         x))
                (setq entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list
                      (append
                       entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list
                       (list x))))))
           (log-get-func
            (lambda (op-type src-abs-path src-abs-path-node-type
                     dest-abs-path success error-msg)
              (let ((op-name
                     (with-temp-buffer
                       (insert (format "%s" op-type))
                       (upcase-region (point-min) (point-max))
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
                    (dest-abs-path-node-type
                     (when success
                       (funcall node-type-get-func
                                dest-abs-path
                                (car src-abs-path-node-type)))))
                (list
                 :op-symbol op-type
                 :op-name op-name
                 :src-abs-path src-abs-path
                 :src-node-type src-abs-path-node-type
                 :dest-abs-path dest-abs-path
                 :dest-node-type dest-abs-path-node-type
                 :process-succeed-p success
                 :error-msg error-msg
                 :log-string
                 (concat
                  (format "%s %s %s: %s \n"
                          (propertize "*" 'face 'org-level-1)
                          (cond
                           ((eq success t)
                            (propertize "SUCCESS" 'face 'success))
                           ((integerp success)
                            (propertize "WARNING" 'face 'warning))
                           (t
                            (propertize "FATAL  " 'face 'error)))
                          (if (eq (car src-abs-path-node-type) 'dir)
                              (propertize
                               "DIRMIRROR "
                               'face
                               (if (eq success t) 'nobreak-hyphen 'error))
                            (propertize
                             "FILEMIRROR"
                             'face (if (eq success t) 'success 'error)))
                          (entropy/emacs-make-relative-filename
                           src-abs-path
                           base-dir-abs-path))
                  (propertize ":PROPERTIES:" 'face 'org-drawer)
                  (format "%s %s"
                          (propertize
                           "\n:OPERATION-NAME:"
                           'face 'org-special-keyword)
                          op-name)
                  (format "%s %s"
                          (propertize
                           "\n:SOURCE-ABSOLUTE-PATH:"
                           'face 'org-special-keyword)
                          src-abs-path)
                  (format "%s %s"
                          (propertize
                           "\n:SOURCE-NODE-TYPE:"
                           'face 'org-special-keyword)
                          src-abs-path-node-type)
                  (format "%s %s"
                          (propertize
                           "\n:DESTINATION-ABSOLUTE-PATH:"
                           'face 'org-special-keyword)
                          dest-abs-path)
                  (format "%s %s"
                          (propertize
                           "\n:DESTINATION-NODE-TYPE:"
                           'face 'org-special-keyword)
                          dest-abs-path-node-type)
                  (if error-msg
                      (format "%s %s"
                              (propertize
                               "\n:ERROR-MESSAGE:"
                               'face 'org-special-keyword)
                              (propertize error-msg 'face 'error))
                    "")
                  (propertize "\n:END:" 'face 'org-drawer)
                  )))))
           (log-filter-func
            (lambda ()
              (when entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list
                (dolist (el entropy/emacs-basic--dired-do-hard-or-symbolic-link-log-list)
                  (let ((ftype (car (plist-get el :src-node-type)))
                        (success (plist-get el :process-succeed-p))
                        (log-string (plist-get el :log-string)))
                    ;; iterator increasement
                    (cond
                     ((eq ftype 'file)
                      (if (eq success t)
                          (cl-incf log-success-files)
                        (cl-incf log-fatal-files)))
                     ((eq ftype 'dir)
                      (if (eq success t)
                          (cl-incf log-success-dirs)
                        (cl-incf log-fatal-dirs)))
                     (t
                      (error "[eemacs-dired-do-hardlink] internal error")))
                    (when log-string
                      (setq log-string-list
                            (append log-string-list
                                    (list log-string)))))))))
           )
      ;; Firstly we must check the target host is not under any marked
      ;; directories to ensure no unlimited mirror.
      (let (_)
        (dolist (el nodes)
          (when (and
                 (file-directory-p el)
                 (entropy/emacs-make-relative-filename
                  host-target el))
            (user-error "Target host '%s' is under current dired directory's '%s' subdir!"
                        host-target
                        (file-name-nondirectory
                         (entropy/emacs-directory-file-name
                          el))))))
      (when nodes
        (dolist (node nodes)
          (setq node (expand-file-name node))
          (setq file-target
                (expand-file-name
                 (file-name-nondirectory
                  (directory-file-name
                   node))
                 host-target))
          (cond
           ((file-directory-p node)
            (funcall node-msg-func 'dir node file-target)
            (let* ((mirror-log
                    ;; we also need to trace the native error for
                    ;; `entropy/emacs-do-directory-mirror'.
                    (condition-case error-type
                        (entropy/emacs-do-directory-mirror
                         node file-target
                         :use-symbolic-link (not use-hardlink)
                         :no-error-when-srcdir-is-empty-p t
                         :pop-log 'log-string-with-trim-title-style)
                      (error
                       (funcall log-append-func
                                (funcall log-get-func
                                         op-type
                                         node '(dir)
                                         file-target
                                         nil
                                         (format "%S" error-type))
                                nil)
                       ;; must return nil since we should ignore
                       ;; next procedure in this condition context
                       nil)))
                   (mirror-log-var (cddr mirror-log))
                   (mirror-log-string (cadr mirror-log)))
              (when mirror-log-var
                (funcall log-append-func
                         mirror-log-var
                         t))
              (when mirror-log-string
                (setq log-string-list
                      (append log-string-list
                              (list mirror-log-string))))))
           ((entropy/emacs-filesystem-node-exists-p node)
            (let ((srcnodetype (funcall node-type-get-func node 'file)))
              (funcall node-msg-func 'file node file-target)
              (condition-case error-type
                  (progn
                    (if use-hardlink
                        (add-name-to-file node file-target)
                      (make-symbolic-link node file-target))
                    (funcall log-append-func
                             (funcall log-get-func
                                      op-type
                                      node srcnodetype
                                      file-target
                                      t
                                      nil)
                             nil))
                (error
                 (funcall log-append-func
                          (funcall log-get-func
                                   op-type
                                   node srcnodetype
                                   file-target
                                   nil
                                   (format "%S" error-type))
                          nil)))))))
        (funcall log-filter-func)
        (setq summary-msg-info-str
              (funcall summary-msg-info-get-func))
        (funcall summary-msg-message-func)
        (when log-string-list
          (with-current-buffer log-buffer
            (let ((inhibit-read-only t))
              (insert (format "%s\n%s\n%s\n\n"
                              "=============================="
                              summary-msg-info-str
                              "=============================="
                              ))
              (mapc (lambda (x)
                      (insert x)
                      (insert "\n"))
                    log-string-list))
            (entropy/emacs-do-directory-mirror/log-mode)
            (setq buffer-read-only t)
            (pop-to-buffer log-buffer))))))

;; ******* Dired count marked files recursively

  (defun entropy/emacs-dired-do-counts-marked-files-recursively (arg)
    "Do count files recursively for marked items in `dired-mode'
buffer with level restriction by ARG.

In interaction mode, ARG is caluated by `prefix-numeric-value' on
`current-prefix-arg', so that both 'C-u C-u .. ' and 'C-u NUM'
are acceptable. If `current-prefix-arg' is nil we fallbackk to 1,
or if 0 we use `nil' which for non level restriction.

Any symbolic link target to a directory is recognized as an
directory.
"
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (unless (eq major-mode 'dired-mode)
      (user-error "[Error]: current buffer is not an 'dired' buffer"))
    (let ((with-level
           (cond
            ((null arg)
             1)
            ((listp arg)
             (floor
              (log (car arg) 4)))
            (t
             (if (> arg 0)
                 arg
               nil))))
          (nodes (or (dired-get-marked-files)
                     (list (dired-current-directory))))
          (all-files-counts 0))
      (dolist (el nodes)
        (if (file-directory-p el)
            (setq all-files-counts
                  (+ all-files-counts
                     (length (entropy/emacs-list-dir-subfiles-recursively-for-list
                              el nil
                              :with-level with-level))))
          (cl-incf all-files-counts)))
      (message (format "%s files in %s marked items"
                       all-files-counts (length nodes)))))

;; ******* Dired auto revert after some operations

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
    (let* ((cur-buffer (current-buffer))
           (rtn (apply orig-func orig-args)))
      (with-current-buffer cur-buffer
        (revert-buffer))
      rtn))

  ;; emacs 28 has an `dired-do-revert-buffer' option to auto revert
  ;; buffer opterations of 'dired-do-copy', 'dired-do-rename',
  ;; 'dired-do-symlink', 'dired-do-hardlink'.
  (when (not (version< emacs-version "28"))
    (setq dired-do-revert-buffer t))
  (dolist (el (delete nil `(,(when (version< emacs-version "28") 'dired-do-rename)
                            dired-do-rename-regexp
                            ,(when (version< emacs-version "28") 'dired-do-copy)
                            dired-do-copy-regexp
                            dired-do-compress
                            dired-do-compress-to
                            dired-do-touch
                            ,(when (version< emacs-version "28") 'dired-do-symlink)
                            ,(when (version< emacs-version "28") 'dired-do-hardlink))))
    (advice-add el :around #'entropy/emacs-basic--dired-revert-advice))

;; ***** __end
  )

;; **** Use dired-aux to enable dired-isearch
(entropy/emacs-lazy-initial-advice-before
 '(dired) "dired-aux-init-for-dired" "dired-aux-init-for-dired"
 :prompt-type 'prompt-echo
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
  :eemacs-adrequire
   ((:enable t :adfors (dired-mode-hook) :adtype hook :pdumper-no-end t))
  :commands (dired-rainbow-define-chmod)
  :init
  (defvar entropy/emacs-dired-rainbow-spec
    '(
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
  :config
  (eval
   `(progn
      ,@entropy/emacs-dired-rainbow-spec)))

(use-package diredfl
  :commands (diredfl-global-mode)
  :init
  (entropy/emacs-lazy-initial-advice-after
   '(dired-mode)
   "diredfl-init-for-dired" "diredfl-init-for-dired"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (diredfl-global-mode 1)))

;; **** dired-x
(use-package dired-x
  :ensure nil
  :eemacs-adrequire ((:enable t :adfors (dired-mode) :adtype after :pdumper-no-end t))
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :eemacs-mmphca
  (((:enable t :defer t)
    (dired-mode (dired dired-mode-map)))
   ("Misc."
    (("o" dired-omit-mode "Toggle omission of uninteresting files in Dired (Dired-Omit mode)."
      :enable t :toggle dired-omit-mode :map-inject t))))
  :config
  (setq dired-omit-size-limit nil)
  (setq dired-omit-extensions nil)
  (setq dired-omit-verbose nil)
  ;; Just omit the current node point and some windows volumn auto
  ;; created stuffs.
  (setq dired-omit-files
        "\\`\\([[:space:]]*[.][.]?\
\\|System Volume Information\
\\|\\$RECYCLE.BIN\\)\\'")
  ;; remap `dired-omit-mode' command in `dired-mode-map' because of
  ;; conflicted with `ace-window'
  (define-key dired-mode-map "\C-x\M-o" nil))

;; **** dired-subtree
;; Org mode like dired subtree fold/expand
(use-package dired-subtree
  :commands
  (dired-subtree-toggle
   dired-subtree-cycle
   entropy/emacs-dired-subtree-cycle)
;; ***** eemacs mmphca
  :eemacs-mmphca
  (((:enable t :defer (:data (:adfors (dired-mode-hook) :adtype hook :pdumper-no-end t)))
    (dired-mode (dired dired-mode-map)))
   ("Basic"
    (("TAB" dired-subtree-toggle
      "Insert subtree at point (vice versa)."
      :enable t :map-inject t :exit t)
     ("<backtab>" entropy/emacs-dired-subtree-cycle
      "Org-mode like cycle visibilitya"
      :enable t :map-inject t :exit t))))
;; ***** config
  :config
;; ****** patch
;; ******* patch core libs

  ;; FIXME: report to updtream.
  (defun __ya/dired-subtree--get-all-ovs-at-point (&optional p)
    "Fix typo of dired-subtree--get-all-ovs-at-point."
    (setq p (or p (point)))
    (--filter (overlay-get it 'dired-subtree-depth)
              (overlays-at p)))
  (advice-add 'dired-subtree--get-all-ovs-at-point
              :override
              #'__ya/dired-subtree--get-all-ovs-at-point)

;; ******* patch `dired-subtree-up'

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/dired-subtree-up (&optional _arg)
    "Like `dired-subtree-up' but fix bug when current subtree's
parent is at the first node in curren dired buffer which
`dired-previous-line' jump out the `invisible-p' target place as
its description:

#+begin_quote
We never want to move point into an invisible line.
#+end_quote

So jumping from the first node's subtree will visually at the top of
the buffer.
"
    (interactive "p")
    (-when-let (ov (dired-subtree--get-ov))
      (goto-char (overlay-start ov))
      (if (and
           ;; we must using absolute line number to exclude the
           ;; narrow case since its not suit for that case.
           (not (buffer-narrowed-p))
           (=
            (line-number-at-pos
             (save-excursion (forward-line -1) (point)))
            3))
          (progn (forward-line -1) (dired-move-to-filename))
        (dired-previous-line 1))))
  (advice-add 'dired-subtree-up
              :override
              #'__ya/dired-subtree-up)

;; ******* patch `dired-subtree--readin'

  (defun entropy/emacs-basic--dired-subtree-readin-core
      (buffer dirpath user-spec-dired-actual-switches)
    "Use `dired-insert-directory' to list DIRPATH's directory list to
BUFFER with list switch USER-SPEC-DIRED-ACTUAL-SWITCHES which
names to suggest using `dired-actual-switches' to do thus since
`dired' finally use it internally for draw the list and we should
obey it as consistency thought."
    (let* ((inhibit-read-only t))
      (with-current-buffer buffer
        ;; insert dir list
        (dired-insert-directory
         dirpath
         (or user-spec-dired-actual-switches (error "empty `dired-actual-switches'")))
        ;; remove invisble properties since we should let it be raw output
        (remove-text-properties (point-min) (point-max) '(invisible))
        ;; Pruning the dir list of '.' and '..'
        (save-excursion
          (let ((this-func
                 (lambda ()
                   (let* ((inhibit-point-motion-hooks t)
                          (inhibit-field-text-motion t)
                          lnendpt
                          fname-beg
                          fname)
                     (setq lnendpt (line-end-position))
                     (setq fname-beg (save-excursion
                                       (dired-move-to-filename)))
                     (when (integerp fname-beg)
                       (setq fname (buffer-substring-no-properties fname-beg lnendpt))
                       (when (or (string= fname ".")
                                 (string= fname ".."))
                         (forward-line 0)
                         (delete-region (point) (1+ lnendpt))
                         t))))))
            (goto-char (point-min))
            ;; remove the details title
            (unless (dired-move-to-filename)
              (goto-char (point-min))
              (condition-case err
                  (delete-region (point) (1+ (line-end-position)))
                (args-out-of-range
                 ;; FIXME: directory like '/proc/1/fdinfo' has no
                 ;; normally directory structure?
                 (user-error "Special directory detected '%s'"
                             dirpath))
                (error
                 (error "%s" err))))
            (while (not (eobp))
              (unless (funcall this-func)
                (forward-line 1))
              (funcall this-func))
            ;; remove eob's blank line
            (goto-char (point-max))
            (when (looking-at "^$")
              ;; ignore error when buffer is empty now
              (ignore-errors
                (delete-char -1)))))
        ;; Add leading space using for `dired-subtree--dired-line-is-directory-or-link-p'
        (let ((this-func
               (lambda ()
                 (let ((cur-spc-len 0))
                   (when (looking-at "\\( *\\)")
                     (setq cur-spc-len
                           (length (match-string 1))))
                   (insert (make-string
                            (max (- 2 cur-spc-len) 0)
                            ?\ ))))))
          (goto-char (point-min))
          (funcall this-func)
          (while (= (forward-line 1) 0)
            (funcall this-func))
          (delete-char -2)))))

  (defun entropy/emacs-basic--dired-subtree-readin-around-advice
      (&rest orig-args)
    "Override advice for `dired-subtree--readin' to put the
'dired-filename' text property to the filename correctly.

That origin function just listing the directory files with
verbatim style which doesn't has ability to distinguish some
filename start with space char, this is buggly that if thus,
`dired' will using `dired-move-to-filename-regexp' for
`dired-move-to-filename' to guess the beginning of the filename
position which will prompt a warning that show that filename
wasn't exsited any more."
    (let (
          ;; we must get `dired-actual-switches' before the
          ;; `with-temp-buffer' statement since its a buffer locl
          ;; varaible.
          (dir-ls-switches dired-actual-switches))
      (with-temp-buffer
        (entropy/emacs-basic--dired-subtree-readin-core
         (current-buffer) (car orig-args) dir-ls-switches)
        (buffer-substring (point-min) (point-max)))))
  (advice-add 'dired-subtree--readin
              :override
              #'entropy/emacs-basic--dired-subtree-readin-around-advice)

;; ******* patch `dired-subtree-insert'

  (defun __ya/dired-subtree--dired-line-is-directory-or-link-p
      (orig-func &rest orig-args)
    "Extended the simple line regexp match for
`dired-subtree--dired-line-is-directory-or-link-p' after its origin
speedup method so that we can recongnize more unregular directory
listing format return by 'ls' in some cases like permission denied
listing for some items under an readable dirctory e.g.

#+begin_example
  /proc/387:
  /usr/bin/ls: cannot read symbolic link '/proc/387/cwd': Permission denied
  /usr/bin/ls: cannot read symbolic link '/proc/387/root': Permission denied
  /usr/bin/ls: cannot read symbolic link '/proc/387/exe': Permission denied
    total used in directory 0 available 0 B
    dr-xr-xr-x   2 root root 0 Jun 23 04:28 attr
    dr-x------   2 root root 0 Jun 23 04:28 fd
    dr-xr-xr-x   2 root root 0 Jun 23 04:28 fdinfo
    dr-x------   2 root root 0 Jun 23 04:28 map_files
    dr-xr-xr-x  59 root root 0 Jun 23 04:28 net
    dr-x--x--x   2 root root 0 Jun 23 04:28 ns
    dr-xr-xr-x   3 root root 0 Jun 22 22:28 task
    -r--r--r--   1 root root 0 Jun 23 04:28 arch_status
    -rw-r--r--   1 root root 0 Jun 23 04:28 autogroup
    -r--------   1 root root 0 Jun 23 04:28 auxv
    -r--r--r--   1 root root 0 Jun 23 04:28 cgroup
    --w-------   1 root root 0 Jun 23 04:28 clear_refs
    -r--r--r--   1 root root 0 Jun 23 04:16 cmdline
    -rw-r--r--   1 root root 0 Jun 23 04:28 comm
    -rw-r--r--   1 root root 0 Jun 23 04:28 coredump_filter
    -r--r--r--   1 root root 0 Jun 23 04:28 cpuset
    -r--r--r--   1 root root 0 Jun 23 04:28 cpu_resctrl_groups
    lrwxrwxrwx   1 root root 0 Jun 23 04:28 cwd
#+end_example

In which case the leading space chars amount is larger or less than 2
which is hardcoded in the ORIGIN-FUNC.
"
    (or (apply orig-func orig-args)
        (let ((file (ignore-errors
                      ;; using ignore errors to treat some defined
                      ;; error msg from `dired-get-filename' as nil
                      ;; return since we can not distinguish the '.'
                      ;; and '..' while using its optional arg
                      ;; NO-ERROR-IF-NOT-FILEP.
                      (dired-get-filename))))
          (and file
               (file-directory-p file)))))
  (advice-add 'dired-subtree--dired-line-is-directory-or-link-p
              :around
              #'__ya/dired-subtree--dired-line-is-directory-or-link-p)

  (defun __ya/dired-subtree-insert (&rest _args)
    "Like `dired-subtree-insert' but with bug fix.

EEMACS_MAINTENANCE: Updpate with upstream's updates.
"
    (when (and (dired-subtree--dired-line-is-directory-or-link-p)
               (not (dired-subtree--is-expanded-p)))
      (let* ((dir-name (dired-get-filename nil))
             (listing (dired-subtree--readin (file-name-as-directory dir-name)))
             ;; EEMACS_TEMPORALLY_HACK inhibit point motion features
             ;; to ensure plain motion operations did correctly.
             (inhibit-field-text-motion t)
             (inhibit-point-motion-hooks t)
             ;; EEMACS_TEMPORALLY_HACK: using top `inhibit-read-only'
             ;; instead of using interactive `read-only-mode' which is
             ;; messy.
             (inhibit-read-only t)
             beg end)
        (move-end-of-line 1)
        ;; this is pretty ugly, I'm sure it can be done better
        (save-excursion
          (insert listing)
          (setq end (+ (point) 2)))
        ;; EEMACS_TEMPORALLY_HACK: don't use (newline) since its an
        ;; iteractive func which not designed for lisp programe. And it
        ;; can not be did what we want in an 'invisible' property
        ;; setted region like in a line of an symbolic with
        ;; `dired-hide-details-mode' enabled buffer.
        (insert "\n")
        (setq beg (point))
        (remove-text-properties (1- beg) beg '(dired-filename))
        (let* ((ov (make-overlay beg end))
               (parent (dired-subtree--get-ov (1- beg)))
               (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth)))
                          1))
               (face (intern (format "dired-subtree-depth-%d-face" depth))))
          (when dired-subtree-use-backgrounds
            (overlay-put ov 'face face))
          ;; refactor this to some function
          (overlay-put ov 'line-prefix
                       (if (stringp dired-subtree-line-prefix)
                           (if (not dired-subtree-use-backgrounds)
                               (apply 'concat (-repeat depth dired-subtree-line-prefix))
                             (cond
                              ((eq nil dired-subtree-line-prefix-face)
                               (apply 'concat
                                      (-repeat depth dired-subtree-line-prefix)))
                              ((eq 'subtree dired-subtree-line-prefix-face)
                               (concat
                                dired-subtree-line-prefix
                                (propertize
                                 (apply 'concat
                                        (-repeat (1- depth) dired-subtree-line-prefix))
                                 'face face)))
                              ((eq 'parents dired-subtree-line-prefix-face)
                               (concat
                                dired-subtree-line-prefix
                                (apply 'concat
                                       (--map
                                        (propertize dired-subtree-line-prefix
                                                    'face
                                                    (intern (format "dired-subtree-depth-%d-face" it)))
                                        (number-sequence 1 (1- depth))))))))
                         (funcall dired-subtree-line-prefix depth)))
          (overlay-put ov 'dired-subtree-name dir-name)
          (overlay-put ov 'dired-subtree-parent parent)
          (overlay-put ov 'dired-subtree-depth depth)
          (overlay-put ov 'evaporate t)
          (push ov dired-subtree-overlays))
        (goto-char beg)
        (dired-move-to-filename)
        (run-hooks 'dired-subtree-after-insert-hook))))
  (advice-add 'dired-subtree-insert
              :override
              #'__ya/dired-subtree-insert)


;; ****** dired extending
;; ******* extending `dired-goto-file'
  (defun entropy/emacs-basic-dired-subtree--dired-goto-file (file)
    "Addon for `dired-goto-file' to find the line matching file."
    (let (ov-start ov-end cur-fname pt)
      (when dired-subtree-overlays
        (setq
         pt
         (catch :exit
           (dolist (ov
                    ;; using from minimal pt from ov list
                    (reverse dired-subtree-overlays))
             (when (overlayp ov)
               (setq ov-start (overlay-start ov)
                     ov-end (overlay-end ov))
               (when
                   ;; FIXME: why the ov start and end can be nil while
                   ;; dired revert from deletion operations?
                   (and (integer-or-marker-p ov-start)
                        (integer-or-marker-p ov-end))
                 (save-excursion
                   (goto-char ov-start)
                   (while (and (< (point) ov-end)
                               (not (eobp)))
                     (setq cur-fname
                           (ignore-errors (dired-get-filename)))
                     (when (and cur-fname
                                (string-equal cur-fname file))
                       (dired-move-to-filename)
                       (throw :exit (point)))
                     (dired-next-line 1))))))))
        (when pt
          (goto-char pt)
          pt))))
  (entropy/emacs-dired-goto-file-extend-processors-regist
   'entropy/emacs-basic-dired-subtree--dired-goto-file)

;; ****** eemacs spec apis

  (defun entropy/emacs-dired-subtree-api-get-overlay-at-pt (&optional pt)
    "Get nearest subtree overlay at point PT (defaults to `point'),
return the ov or nil when not found."
    (dired-subtree--get-ov pt))

  (defun entropy/emacs-dired-subtree-api-get-overlays-with-depth-sort
      (&optional pt reverse)
    "Return a list of subtree overlays at `point' PT (defaults to `point').

The return is sorted from wide to minimal which means that the
car is the parent of PT. Or REVERSE the order which means the car
is the minimal nested one from PT."
    (--sort (funcall
             (if reverse '< '>)
             (overlay-get it 'dired-subtree-depth)
             (overlay-get other 'dired-subtree-depth))
            (if pt
                (dired-subtree--get-all-ovs-at-point pt)
              (copy-sequence dired-subtree-overlays))))

;; ****** eemacs spec commands
;; ******* multi `dired-subtree-cycle'

  (defun entropy/emacs-dired-subtree-cycle (&optional max-depth)
    "Like `dired-subtree-cycle' but mapped with
`dired-get-marked-files' when marked directories found."
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (let ((mkitems (dired-get-marked-files)))
      (cond
       ((null mkitems)
        (funcall 'dired-subtree-cycle max-depth))
       (t
        (dired-map-over-marks
         (let* ((cur-fname (dired-get-filename))
                (cur-item-p
                 (and cur-fname
                      (dired-subtree--dired-line-is-directory-or-link-p))))
           (when cur-item-p
             (funcall 'dired-subtree-cycle max-depth)))
         nil t)))))

;; ***** __end__
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
;; **** union framework

(use-package image-file
  :ensure nil
  :commands (image-file-name-regexp
             auto-image-file-mode)
  :init

  ;; Support webp file
  (add-to-list 'image-file-name-extensions "webp")

  )

(defun entropy/emacs-image-mode-external-view-union (image-file)
  "Use `entropy/open-with-port' to open IMAGE-FILE in external app.

The IMAGE-FILE must match regexp of `image-file-name-regexp' or
an error thrown out."
  (unless (featurep 'entropy-open-with)
    (require 'entropy-open-with))
  (unless (string-match-p (image-file-name-regexp) image-file)
    (user-error "'%s' is not an image file!"))
  (entropy/open-with-port nil image-file))


(defun entropy/emacs-image-mode-copy-image-as-jpeg
    (srcfname destfname &optional ok-if-exists)
  "Copy any source image file SRCFNAME to an jpeg image file
DESTFNAME using imagemagick to convert thus when SRCFNAME is not
an jpeg image file or just copy it using `copy-file'.

Return t if the process did done or a cons to describe the failed
status which formed as:

: (fatal-status-symbol . error-msg)

if =fatal-status-symbol= is 'nil', it indicate dedicated fatal
occurs, other wise they are:

- 'file-exists' : the target file exists
- 'convert-not-found' : couvert executable not found
- 'convert-failed' : the convert procedure failed

If optional argument OK-IF-EXISTS is non-nil, the DESTFNAME will be
overwrited.
"
  (let ((src-proper-p
         (string-match-p "^jpeg\\|jpg\\|JPEG\\|JPG$"
                         (file-name-extension srcfname)))
        (error-symbol nil))
    (condition-case error
        (progn
          (cond
           (src-proper-p
            (copy-file srcfname destfname ok-if-exists))
           (t
            (when (file-exists-p destfname)
              (if ok-if-exists
                  (delete-file destfname)
                (setq error-symbol 'file-exists)
                (user-error "Target file '%s' is existed!")))
            (unless (executable-find "convert")
              (setq error-symbol 'convert-not-found)
              (user-error "imagemgick convert not found"))
            (let ((default-directory (entropy/emacs-return-as-default-directory
                                      temporary-file-directory)))
              (message "convert '%s' to '%s' jpeg file ..."
                       srcfname destfname)
              (unless (zerop
                       (if (executable-find "gm")
                           (call-process "gm" nil nil nil
                                         "convert"
                                         srcfname
                                         "-strip"
                                         (format "jpeg:%s" destfname))
                         (call-process "convert" nil nil nil
                                       srcfname
                                       "-strip"
                                       (format "jpeg:%s" destfname))))
                (setq error-symbol 'convert-failed)
                (user-error "Convert '%s' to '%s' failed!"
                            srcfname destfname))
              (message "convert '%s' to '%s' jpeg file done"
                       srcfname destfname))))
          t)
      (error
       (cons error-symbol
             (format "%s" error))))))

;; **** image main mode
(use-package image-mode
  :ensure nil
  :functions (image-set-window-vscroll
              image-set-window-hscroll
              image-mdoe)
  :eemacs-mmphc
  (((:enable t :defer t)
    (nil
     nil
     t
     ((1 :width-desc "Navigation")
      (1 :width-desc "Resize")
      (1 :width-desc "Animation"))))
   ("Navigation & View"
    (("gn" image-next-file "Visit the next image in the same directory"
      :enable t :map-inject t :exit t)
     ("gN" image-previous-file "Visit the preceding image in the same directory"
      :enable t :map-inject t :exit t)
     ("C-<return>" entropy/emacs-image-mode-open-as-external
      "Display current image with external app"
      :enable t :map-inject t :exit t)
     ("M-RET" entropy/emacs-image-mode-open-as-external
      "Display current image with external app"
      :enable t :map-inject t :exit t))

    "Resize Temporally"
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

  (defun entropy/emacs-basic-image-gif-warn (&rest _)
    "Warn that gif animation by large gif file will freeze
emacs."
    (if (string-match-p "\\.gif" (buffer-name))
        (if (not (y-or-n-p "Do you want to animated it? "))
            (error "Please open it with external apps!"))))
  (advice-add 'image-toggle-animation :before #'entropy/emacs-basic-image-gif-warn)

  (defun entropy/emacs-image-mode-open-as-external ()
    "Display image with external app of file in current `image-mode'
buffer."
    (declare (interactive-only t))
    (interactive)
    (unless (eq major-mode 'image-mode)
      (user-error "Not in image-mode"))
    (let ((file (buffer-file-name)))
      (unless (file-exists-p file)
        (user-error "No original file name found"))
      (entropy/emacs-image-mode-external-view-union
       file)))
  )

;; **** image dired

(defmacro entropy/emacs-basic-image-dired-use-package (&rest body)
  "The use-packag patch for `image-dired' version related patches
ver. load.

This macro existed because of that each version of image-dird has
fatal bug of using filename as part of the regexp string. And
hence further more enhancement we can add also via this
mechanism."
  (declare (indent 1))
  (let* ((path
          (expand-file-name
           (format "image-dired/%s" emacs-major-version)
           entropy/emacs-site-lisp-path))
         (body-patch (cons (car body)
                           (cons :load-path
                                 (cons path (cdr body))))))
    (if (= emacs-major-version 28)
        `(use-package
           ,@body-patch)
      `(use-package
         ,@body))))

;; ***** core
(defvar-local __ya/image-dired-display-image-buffer-image-file nil)

(entropy/emacs-basic-image-dired-use-package image-dired
  :ensure nil
  :commands (image-dired)
;; ****** preface
  :preface

  (defun entropy/emacs-image-dired-init (&optional arg)
    "Initial `image-dired' automatically when proper.

With prefix argument ARG is set, convert the thumbnails from
whatever marked files getted from
`dired-get-marked-files'. Otherwise, unmark all marked items
firstly and then marked files through `image-file-name-regexp',
convert the thumbnails from these marked items.

If not marked files found (user spec mode), or non image files
can be found in this dired buffer, cancel the operation and throw
an error."
    (declare (interactive-only t))
    (interactive "P" dired-mode)
    (unless (featurep 'image-dired)
      (require 'image-dired))
    (unless (eq major-mode 'dired-mode)
      (user-error "Not in an dired buffer"))
    (let ((cur-buffer (current-buffer))
          (img-fmarked (or
                        (and arg (dired-get-marked-files))
                        (let (effective-marked-files)
                          ;; we must unmark all items firstly since any marked item not an
                          ;; image will be a mistake for create thumbnails. Unless user
                          ;; specifed marked files with prefix arg in which case we ignore
                          ;; the messy protection.
                          (dired-unmark-all-marks)
                          (dired-mark-files-regexp (image-file-name-regexp))
                          (setq effective-marked-files
                                (dired-get-marked-files
                                 nil nil nil
                                 ;; distinguish the only one marked
                                 ;; status so that we can jduge
                                 ;; whether there's actual marked
                                 ;; files has get from regexp marking
                                 ;; process.
                                 t))
                          (if (null (cdr effective-marked-files))
                              nil
                            effective-marked-files)))))
      (unless img-fmarked
        (user-error "No images found!"))
      (unwind-protect
          (let ((files img-fmarked))
            (if (or (<= (length files) image-dired-show-all-from-dir-max-files)
                    (and (> (length files) image-dired-show-all-from-dir-max-files)
                         (y-or-n-p
                          (format
                           "Directory contains more than %d image files.  Proceed? "
                           image-dired-show-all-from-dir-max-files))))
                (progn
                  ;; FIXME:
                  ;; Firstly we should clean up the active jobs stack
                  ;; since the `image-dired-queue' is globalized and
                  ;; its stack pop procedure is not safe of which will
                  ;; be corrupt when any error within the queue run so
                  ;; that the `image-dired-queue-active-jobs' may not
                  ;; be `cl-decf' properly which will cause the new
                  ;; queue do not run while it hint the
                  ;; `image-dired-queue-active-limit'.
                  (setq image-dired-queue-active-jobs 0
                        image-dired-queue nil)
                  (image-dired-display-thumbs)
                  (with-current-buffer image-dired-thumbnail-buffer
                    (when (bound-and-true-p hl-line-mode)
                      ;; display `hl-line-mode' since its cover the
                      ;; mark highliting face for thumbnails and will
                      ;; drag the image dired origin file track
                      ;; performance.
                      (hl-line-mode -1)))
                  (pop-to-buffer image-dired-thumbnail-buffer))
              (message "Canceled.")))
        (with-current-buffer cur-buffer
          (dired-unmark-all-marks)))))

;; ****** config
  :config

  (setq image-dired-cmd-create-thumbnail-program
        (if (executable-find "gm") "gm" "convert"))
  (setq image-dired-cmd-create-temp-image-program
        (if (executable-find "gm") "gm" "convert"))
  (setq image-dired-cmd-create-thumbnail-options
        (let ((opts image-dired-cmd-create-thumbnail-options))
          (if (equal image-dired-cmd-create-thumbnail-program "gm")
              (if (equal (car opts) "convert")
                  opts
                (cons "convert" opts))
            opts)))
  (setq image-dired-cmd-create-standard-thumbnail-options
        (let ((opts image-dired-cmd-create-standard-thumbnail-options))
          (if (equal image-dired-cmd-create-thumbnail-program "gm")
              (if (equal (car opts) "convert")
                  opts
                (cons "convert" opts))
            opts)))
  (setq image-dired-cmd-create-temp-image-options
        (let ((opts image-dired-cmd-create-temp-image-options))
          (if (equal image-dired-cmd-create-temp-image-program "gm")
              (if (equal (car opts) "convert")
                  opts
                (cons "convert" opts))
            opts)))

;; ****** __end__
  )

;; ***** image-dired-thumbnail-mode

;; FIXME: Def macro must be out of the `use-package' body why?
(defmacro entropy/emacs-image-dired-thumbnail-with-mapping-images
    (&rest body)
  "Do BODY at every point at an image thumbnail in
`image-dired-thumbnail-buffer' pretending with
`with-silent-modifications'."
  `(with-current-buffer image-dired-thumbnail-buffer
     (save-mark-and-excursion
       (goto-char (point-min))
       (let ((inhibit-read-only t))
         (while (not (eobp))
           (with-silent-modifications
             (when (image-dired-image-at-point-p)
               ,@body))
           (forward-char))))))

(entropy/emacs-basic-image-dired-use-package image-dired
  :ensure nil
  :commands (image-dired)
;; ****** eemacs hydra hollow instance
  :eemacs-mmphc
  (((:enable t :defer t)
    (image-dired-thumbnail-mode
     (image-dired image-dired-thumbnail-mode-map)
     t
     (2 2 2)))
   ("Basic"
    (("RET" image-dired-display-thumbnail-original-image
      "Display current thumbnail’s original image"
      :enable t :map-inject t :exit t)
     ("C-<return>" entropy/emacs-image-dired-thumbnail-mode-open-as-external
      "Display current thumbnail’s original image with external app"
      :enable t :map-inject t :exit t)
     ("M-RET" entropy/emacs-image-dired-thumbnail-mode-open-as-external
      "Display current thumbnail’s original image with external app"
      :enable t :map-inject t :exit t)
     ("v" (:pretty-hydra-cabinet
           (:data
            "Grid buffer navigation"
            (("<right>" entropy/emacs-image-dired-forward-image
              "Move to next image and display properties"
              :enable t :map-inject t :exit t)
             ("<left>" entropy/emacs-image-dired-backward-image
              "Move to prev image and display properties"
              :enable t :map-inject t :exit t)
             ("<up>" entropy/emacs-image-dired-previous-line
              "Move to previous line and display properties"
              :enable t :map-inject t :exit t)
             ("<down>" entropy/emacs-image-dired-next-line
              "Move to next line and display properties"
              :enable t :map-inject t :exit t)))
           :other-rest-args ((image-dired image-dired-thumbnail-mode-map)))
      "Navigation on thumbnails"
      :enable t :exit t)
     ("V" (:pretty-hydra-cabinet
           (:data
            "display buffer navigation"
            (("C-<right>" entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/right
              "Scroll image display buffer arrow <right>"
              :enable t :map-inject t :exit t)
             ("C-<left>" entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/left
              "Scroll image display buffer arrow <left>"
              :enable t :map-inject t :exit t)
             ("C-<up>" entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/up
              "Scroll image display buffer arrow <up>"
              :enable t :map-inject t :exit t)
             ("C-<down>" entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/down
              "Scroll image display buffer arrow <down>"
              :enable t :map-inject t :exit t)))
           :other-rest-args ((image-dired image-dired-thumbnail-mode-map)))
      "Navigation on display buffer"
      :enable t :exit t)
     ("SPC" image-dired-display-next-thumbnail-original
      "Move to next thumbnail and display the image."
      :enable t :exit t :map-inject t)
     ("DEL" image-dired-display-previous-thumbnail-original
      "Move to previous thumbnail and display image."
      :enable t :exit t :map-inject t))
    "Mark&Track"
    (("m" entropy/emacs-image-dired-mark-thumb-original-file
      "Mark original image file in associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("u" entropy/emacs-image-dired-unmark-thumb-original-file
      "Unmark original image file in associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("M" entropy/emacs-image-dired-mark-all-thumbs
      "Mark all original image file in associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("U" entropy/emacs-image-dired-unmark-all-thumbs
      "Unmark all original image file in associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("G" (progn (message "Sync mark status from associated dired buffer ...")
                 (image-dired-thumb-update-marks)
                 (message "Sync mark status from associated dired buffer done"))
      "Sync mark status from associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("." image-dired-track-original-file
      "Track the original file in the associated Dired buffer."
      :enable t :map-inject t :exit t)
     ("d" entropy/emacs-image-dired-thumbnail-mode-pop-assoc-dired
      "Popup associated dired buffer in current image dired thumbnail buffer."
      :enable t :map-inject t :exit t)
     ("<tab>" entropy/emacs-image-dired-jump-original-dired-buffer
      "Jump to the Dired buffer associated with the current image file"
      :enable t :map-inject t :exit t)
     ("g" (:pretty-hydra-cabinet
           (:data
            "Display Modification"
            (("g" image-dired-line-up-dynamic
              "Line up thumbnails images dynamically."
              :enable t :exit t)
             ("f" image-dired-line-up
              "Line up thumbnails according to ‘image-dired-thumbs-per-row’"
              :enable t :exit t)
             ("i" image-dired-line-up-interactive
              "Line up thumbnails interactively."
              :enable t :exit t))))
      "Update thumbnails grid arrangement"
      :enable t :map-inject t :exit t)
     ("t" (:pretty-hydra-cabinet
           (:data
            "viewc/modify thumbnails tag"
            (("t" image-dired-tag-thumbnail
              "Tag current or marked thumbnails."
              :enable t :exit t)
             ("r" image-dired-tag-thumbnail-remove
              "Remove tag from current or marked thumbnails."
              :enable t :exit t))))
      "viewc/modify thumbnails tag"
      :enable t :map-inject t :exit t))
    "Image Rotate"
    (("l" image-dired-rotate-thumbnail-left
      "Rotate thumbnail left (counter clockwise) 90 degrees"
      :enable t :exit t :map-inject t)
     ("r" image-dired-rotate-thumbnail-right
      "Rotate thumbnail counter right (clockwise) 90 degrees."
      :enable t :exit t :map-inject t)
     ("L" image-dired-rotate-original-left
      "Rotate original image left (counter clockwise) 90 degrees."
      :enable t :exit t :map-inject t)
     ("R" image-dired-rotate-original-right
      "Rotate original image right (clockwise) 90 degrees"
      :enable t :exit t :map-inject t))
    "Image Info Modifiction"
    (("D" image-dired-thumbnail-set-image-description
      "Set the ImageDescription EXIF tag for the original image."
      :enable t :exit t :map-inject t)
     ("C-d" image-dired-delete-char
      "Remove current thumbnail from thumbnail buffer and line up."
      :enable t :exit t :map-inject t)
     ("c" image-dired-comment-thumbnail
      "Add comment to current thumbnail in thumbnail buffer."
      :enable t :exit t :map-inject t))
    ))
;; ****** init
  :init

;; ****** config
  :config
;; ******* core lib

  (defun entropy/emacs-image-dired-thumb-set-mark-properties-at-point (type)
    "Set the mark face at point of `image-dired-thumbnail-buffer'.

Valid TYPE are 'mark' 'flag' and 'unmark' and 'toggle'."
    (when image-dired-thumb-visible-marks
      (with-current-buffer image-dired-thumbnail-buffer
        (save-mark-and-excursion
          (let ((inhibit-read-only t))
            (with-silent-modifications
              (cond ((member type '(mark flag))
                     (add-face-text-property
                      (point) (1+ (point))
                      'image-dired-thumb-mark))
                    ((eq type 'unmark)
                     (remove-text-properties
                      (point) (1+ (point))
                      '(face image-dired-thumb-mark)))
                    ((eq type 'toggle)
                     (if (eq (get-text-property (point) 'face)
                             'image-dired-thumb-mark)
                         (entropy/emacs-image-dired-thumb-set-mark-properties-at-point
                          'unmark)
                       (entropy/emacs-image-dired-thumb-set-mark-properties-at-point
                          'mark)))
                    (t
                     (user-error "wrong type of mark property set type: %s"
                                 type)))))))))

  (defun entropy/emacs-image-dired-modify-mark-on-thumb-original-file (command)
    "Modify mark in Dired buffer.
COMMAND is one of `mark' for marking file in Dired, `unmark' for
unmarking file in Dired or `flag' for flagging file for delete in
Dired.

Like `image-dired-modify-mark-on-thumb-original-file' but without
update all thumbnails mark status which just image at current
point."
    (let* ((file-name (image-dired-original-file-name))
           (dired-buf (image-dired-associated-dired-buffer))
           (dired-buf-live-p (and dired-buf (buffer-live-p dired-buf))))
      (if (not (and dired-buf-live-p file-name))
          (cond ((not dired-buf-live-p)
                 (message "No associated dired-buffer found!"))
                (t
                 (message "No image, or image with correct properties, at point.")))
        (with-current-buffer dired-buf
          (message "%s" file-name)
          (when (dired-goto-file file-name)
            (cond ((eq command 'mark) (dired-mark 1))
                  ((eq command 'unmark) (dired-unmark 1))
                  ((eq command 'toggle)
                   (if (image-dired-dired-file-marked-p)
                       (dired-unmark 1)
                     (dired-mark 1)))
                  ((eq command 'flag) (dired-flag-file-deletion 1)))
            (entropy/emacs-image-dired-thumb-set-mark-properties-at-point
             command))))))

  (defvar entropy/emacs-image-dired-idle-track-orig-file-timer nil)
  (defun entropy/emacs-image-dired-idle-track-orig-file--core (&rest _)
    (when (and (eq (current-buffer)
                   (get-buffer image-dired-thumbnail-buffer))
               t)
      (image-dired-track-original-file)))
  (defun entropy/emacs-image-dired-idle-track-orig-file ()
    "Like `image-dired-track-original-file' but run with idle timer."
    (when image-dired-track-movement
      (when (timerp entropy/emacs-image-dired-idle-track-orig-file-timer)
        (cancel-timer entropy/emacs-image-dired-idle-track-orig-file-timer)
        (setq entropy/emacs-image-dired-idle-track-orig-file-timer
              nil))
      (setq entropy/emacs-image-dired-idle-track-orig-file-timer
            (run-with-idle-timer
             ;; using one second is proper since any lower idle delay
             ;; may hint the key repeat frequency which will not show
             ;; the idle delay effects.
             1
             nil
             #'entropy/emacs-image-dired-idle-track-orig-file--core))))

;; ******* patch
;; ******** core

  (defvar __ya/image-dired-display-image-stick-fit-type nil)

  (eval-and-compile
    (defmacro entropy/emacs-basic-image-dired--display-size-with-dwim-p (original-size)
      `(or
        ;; explicit should use dwim
        (when (not ,original-size)
          ;; inherit history dwim type when history is set
          (when __ya/image-dired-display-image-stick-fit-type
            (setq ,original-size
                  __ya/image-dired-display-image-stick-fit-type))
          t)
        (cond
         ;; user specified dwim type
         ((member ,original-size '(2 3))
          ;; then we update the history dwim type
          (setq __ya/image-dired-display-image-stick-fit-type
                ,original-size)
          t)
         ;; manually clear the hisrtoy type and fallback to use dwim
         ((and (listp ,original-size)
               (not (equal ,original-size
                           ;; the single prefix treat as use origin size.
                           '(4))))
          ;; then we clear the history dwim type
          (setq
           __ya/image-dired-display-image-stick-fit-type
           nil)
          t)
         ;; otherwise disable dwim i.e. use single `C-u' type
         ;; which is same as origin mechanism
         (t
          nil)))))

  ;; EEMACS_MAINTENANCE follow upstream updates
  (entropy/emacs-when-defun __ya/image-dired-display-image (file &optional original-size)
    (version< emacs-version "29")
    "Like `image-dired-display-image' but expand the ORIGINAL-SIZE
means as DWIM that:

- `=' 2:      fit to height (stick to window height using the origin image width)
- `=' 3:      fit to width (stick to window width using the origin image height)
- `eq' '(4):  use origin size which is same as origin mechanism the single 'C-u' prefix.

The dwim is memoized via history variable
`__ya/image-dired-display-image-stick-fit-type'.

Any other prefix type is treat as clear/reset the stick history
dwim memory and use both height and width fit display type."
    (image-dired--check-executable-exists
     'image-dired-cmd-create-temp-image-program)
    (let* ((new-file (expand-file-name image-dired-temp-image-file))
           (window (image-dired-display-window))
           (window-height (image-dired-display-window-height window))
           (window-width (image-dired-display-window-width window))
           (image-type 'jpeg)
           (should-use-dwim-p
            (entropy/emacs-basic-image-dired--display-size-with-dwim-p
             original-size)))

      (setq file (expand-file-name file))
      (if should-use-dwim-p
          (let* ((spec
                  (list
                   (cons ?p image-dired-cmd-create-temp-image-program)
                   (cons ?w (or
                             ;; height stick tyep expand width
                             (and (eq original-size 2)
                                  "")
                             window-width))
                   (cons ?h (or
                             ;; width stick type expand height
                             (and (eq original-size 3)
                                  "")
                             window-height))
                   (cons ?f file)
                   (cons ?t new-file)))
                 (ret
                  (apply #'call-process
                         image-dired-cmd-create-temp-image-program nil nil nil
                         (mapcar
                          (lambda (arg) (format-spec arg spec))
                          image-dired-cmd-create-temp-image-options))))
            (when (not (zerop ret))
              (user-error "Could not resize image")))
        (let ((cpstatus
               (entropy/emacs-image-mode-copy-image-as-jpeg
                file new-file t)))
          (unless (eq t cpstatus)
            (user-error "Copy as orig file failed type '%s' as ['%s']"
                        (car cpstatus) (cdr cpstatus)))))
      (with-current-buffer (image-dired-create-display-image-buffer)
        ;; let buffer known the current display image origin file
        (setq __ya/image-dired-display-image-buffer-image-file file)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (clear-image-cache)
          (image-dired-insert-image new-file image-type 0 0)
          (goto-char (point-min))
          (image-dired-update-property 'original-file-name file)
          (with-selected-window window
            (unless (string= (buffer-name) image-dired-display-image-buffer)
              (error "internal error <current window is not matched display image buffer>"))
            ;; FIXME: We must use `image-mode' subroutines to set the
            ;; scroll pos since orign `set-window-vscroll' and
            ;; `set-window-hscroll' is not work when previously has
            ;; did scrolled operation using `image-next-line' etc. and
            ;; why? (maybe since `image-next-line' and refer use
            ;; `setf' i.e. `gv' functionality and why this does?)
            (image-set-window-vscroll 0)
            (image-set-window-hscroll 0))))))

  (entropy/emacs-when-defun __ya/image-dired-display-image (file &optional original-size)
    (version< "29" emacs-version)
    "Like `image-dired-display-image' but expand the ORIGINAL-SIZE
means as DWIM that:

- `=' 2:      fit to height (stick to window height using the origin image width)
- `=' 3:      fit to width (stick to window width using the origin image height)
- `eq' '(4):  use origin size which is same as origin mechanism the single 'C-u' prefix.

The dwim is memoized via history variable
`__ya/image-dired-display-image-stick-fit-type'.

Any other prefix type is treat as clear/reset the stick history
dwim memory and use both height and width fit display type."
    (declare (advertised-calling-convention (file) "29.1"))
    (setq file (expand-file-name file))
    (when (not (file-exists-p file))
      (error "No such file: %s" file))
    (let ((buf (get-buffer-create image-dired-display-image-buffer))
          (cur-win (selected-window))
          (should-use-dwim-p
           (entropy/emacs-basic-image-dired--display-size-with-dwim-p
            original-size))
          (inhibit-read-only t))
      (unless (get-buffer-window buf)
        (display-buffer buf))
      (with-current-buffer buf
        (when (eq major-mode 'image-dired-display-image-mode)
          (with-selected-window (get-buffer-window buf)
            (image-set-window-vscroll 0)
            (image-set-window-hscroll 0)))
        (erase-buffer)
        (insert-file-contents-literally file)
        (goto-char (point-min))
        ;; let buffer known the current display image origin file
        (setq __ya/image-dired-display-image-buffer-image-file file)
        (let ((image-auto-resize
               (or (if should-use-dwim-p
                       (cond
                        ((eq original-size 2)
                         'fit-height)
                        ((eq original-size 3)
                         'fit-width))
                     (when (equal original-size '(4))
                       1))
                   t)))
          (image-dired-display-image-mode)))
      (select-window cur-win)))

  (advice-add 'image-dired-display-image
              :override #'__ya/image-dired-display-image)

;; ******* eemacs spec commands
;; ******** open with external app

  (defun entropy/emacs-image-dired-thumbnail-mode-open-as-external ()
    "Open thumbnails origin image file at point with external app."
    (declare (interactive-only t))
    (interactive nil image-dired-thumbnail-mode)
    (let ((file (image-dired-original-file-name)))
      (if (not (string-equal major-mode "image-dired-thumbnail-mode"))
          (message "Not in image-dired-thumbnail-mode")
        (if (not (image-dired-image-at-point-p))
            (message "No thumbnail at point")
          (if (not file)
              (message "No original file name found")
            (entropy/emacs-image-mode-external-view-union
             file))))))

;; ******** popup associated dired buffer

  ;; declare it as dynamic binding firstly
  (defvar shackle-rules)
  (defun entropy/emacs-image-dired-thumbnail-mode-pop-assoc-dired ()
    "Popup associated dired buffer in current image dired thumbnail buffer."
    (declare (interactive-only t))
    (interactive nil image-dired-thumbnail-mode)
    (unless (string-equal major-mode "image-dired-thumbnail-mode")
      (user-error "Not in image-dired-thumbnail-mode"))
    (let ((assoc-dired-buffer (image-dired-associated-dired-buffer)))
      (unless (and (bufferp assoc-dired-buffer)
                   (buffer-live-p assoc-dired-buffer))
        (user-error "No associated dired-buffer found!"))
      (when image-dired-track-movement
        (with-current-buffer image-dired-thumbnail-buffer
          ;; NOTE: firstly we should forcely sync the track point
          ;; since we use
          ;; `entropy/emacs-image-dired-idle-track-orig-file' to idle
          ;; tracking orig file in most of case which may not did done
          ;; yet.
          (image-dired-track-original-file)))
      (let ((buffer-name (buffer-name assoc-dired-buffer)))
        (if (bound-and-true-p shackle-mode)
            (let* ((shackle-rules
                    (or (and (ignore-errors (shackle-match buffer-name)) shackle-rules)
                        `((,buffer-name :select t :size 0.4 :align below :autoclose t)))))
              (display-buffer assoc-dired-buffer)
              (message "Shackle popup to dired buffer <%s>" buffer-name))
          (pop-to-buffer assoc-dired-buffer)
          (message "Native popup to dired buffer <%s>" buffer-name)))))

;; ******** jump to associated dired buffer

  (defun entropy/emacs-image-dired-jump-original-dired-buffer ()
    "Like `image-dired-jump-original-dired-buffer' but try to display
the origin dired buffer first so that we commonly can did
successfully for this operation."
    (declare (interactive-only t))
    (interactive)
    (let* ((buff (image-dired-associated-dired-buffer))
           (win (image-dired-get-buffer-window buff))
           frame)
      (unless (and (windowp win)
                   (window-live-p win))
        (setq win nil)
        (when buff
          (entropy/emacs-image-dired-thumbnail-mode-pop-assoc-dired)
          (setq win (get-buffer-window buff))))
      (if win
          (progn
            (if (not (equal (selected-frame) (setq frame (window-frame win))))
                (select-frame-set-input-focus frame))
            (select-window win))
        (message "Associated dired buffer not visible"))))

;; ******** scroll sync
  (defvar entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/redraw-timer nil)
  (defun entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer
      (arrow n)
    "Scroll image in `image-dired-display-image-buffer' while current
buffer is `image-dired-thumbnail-mode' with ARROW.

ARROW is valid in 'up' 'down' 'left' 'right'."
    (unless (string-equal major-mode "image-dired-thumbnail-mode")
      (user-error "Not in image-dired-thumbnail-mode"))
    (let* ((buffer (get-buffer image-dired-display-image-buffer))
           (buffer-win (get-buffer-window buffer)))
      (unless (and (buffer-live-p buffer)
                   buffer-win)
        (user-error "No lived image display buffer or window!"))
      (with-selected-window buffer-win
        (cond
         ((equal arrow 'up)     (funcall-interactively 'image-previous-line n))
         ((equal arrow 'down)   (funcall-interactively 'image-next-line n))
         ((equal arrow 'left)   (funcall-interactively 'image-backward-hscroll n))
         ((equal arrow 'right)  (funcall-interactively 'image-forward-hscroll n))
         (t
          (user-error "invalid arrow type: %s" arrow)))
        ;; FIXME: why we should redraw the display frame to force visualized movitation?
        (unless (or entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/redraw-timer
                    (eq arrow 'down))
          (setq entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/redraw-timer
                (run-with-timer
                 0.01 nil
                 `(lambda ()
                    (let ((frame (window-frame ',buffer-win)))
                      (unwind-protect
                          (when (frame-live-p frame)
                            (redraw-frame frame))
                        (setq entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/redraw-timer
                              nil)))))))
        )))
  (defun entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/up
      (&optional n)
    "Use `entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer' with 'up'."
    (declare (interactive-only t))
    (interactive "p" image-dired-thumbnail-mode)
    (entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer
     'up n))
  (defun entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/down
      (&optional n)
    "Use `entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer' with 'down'."
    (declare (interactive-only t))
    (interactive "p" image-dired-thumbnail-mode)
    (entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer
     'down n))
  (defun entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/left
      (&optional n)
    "Use `entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer' with 'left'."
    (declare (interactive-only t))
    (interactive "p" image-dired-thumbnail-mode)
    (entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer
     'left n))
  (defun entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer/right
      (&optional n)
    "Use `entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer' with 'right'."
    (declare (interactive-only t))
    (interactive "p" image-dired-thumbnail-mode)
    (entropy/emacs-image-dired-thumbnail-mode-scroll-display-buffer
     'right n))

;; ******** basic navigation

  (defun entropy/emacs-image-dired-forward-image (&optional arg)
    "Like `image-dired-forward-image' but using
`entropy/emacs-image-dired-idle-track-orig-file' as subroutine."
    (declare (interactive-only t))
    (interactive "p")
    (let (pos (steps (or arg 1)))
      (dotimes (_ steps)
        (if (and (not (eobp))
                 (save-excursion
                   (forward-char)
                   (while (and (not (eobp))
                               (not (image-dired-image-at-point-p)))
                     (forward-char))
                   (setq pos (point))
                   (image-dired-image-at-point-p)))
            (goto-char pos)
          (user-error "At last image"))))
    (when image-dired-track-movement
      (entropy/emacs-image-dired-idle-track-orig-file))
    (image-dired-display-thumb-properties))

  (defun entropy/emacs-image-dired-backward-image (&optional arg)
    "Like `image-dired-backward-image' but using
`entropy/emacs-image-dired-idle-track-orig-file' as subroutine."
    (declare (interactive-only t))
    (interactive "p")
    (let (pos (steps (or arg 1)))
      (dotimes (_ steps)
        (if (and (not (bobp))
                 (save-excursion
                   (backward-char)
                   (while (and (not (bobp))
                               (not (image-dired-image-at-point-p)))
                     (backward-char))
                   (setq pos (point))
                   (image-dired-image-at-point-p)))
            (goto-char pos)
          (user-error "At first image"))))
    (when image-dired-track-movement
      (entropy/emacs-image-dired-idle-track-orig-file))
    (image-dired-display-thumb-properties))

  (defun entropy/emacs-image-dired-next-line ()
    "Like `image-dired-next-line' but using
`entropy/emacs-image-dired-idle-track-orig-file' as subroutine."
    (declare (interactive-only t))
    (interactive)
    (let ((goal-column (current-column)))
      (forward-line 1)
      (move-to-column goal-column))
    ;; If we end up in an empty spot, back up to the next thumbnail.
    (if (not (image-dired-image-at-point-p))
        (entropy/emacs-image-dired-backward-image))
    (if image-dired-track-movement
        (entropy/emacs-image-dired-idle-track-orig-file))
    (image-dired-display-thumb-properties))

  (defun entropy/emacs-image-dired-previous-line ()
    "Like `image-dired-previous-line' but using
`entropy/emacs-image-dired-idle-track-orig-file' as subroutine."
    (declare (interactive-only t))
    (interactive)
    (let ((goal-column (current-column)))
      (forward-line -1)
      (move-to-column goal-column))
    ;; If we end up in an empty spot, back up to the next
    ;; thumbnail. This should only happen if the user deleted a
    ;; thumbnail and did not refresh, so it is not very common. But we
    ;; can handle it in a good manner, so why not?
    (if (not (image-dired-image-at-point-p))
        (entropy/emacs-image-dired-backward-image))
    (if image-dired-track-movement
        (entropy/emacs-image-dired-idle-track-orig-file))
    (image-dired-display-thumb-properties))

;; ******** mark/unmark

  (defun entropy/emacs-image-dired-mark-thumb-original-file (&optional no-forward)
    "Mark original image file in associated Dired buffer.

Like `image-dired-mark-thumb-original-file' but using
`entropy/emacs-image-dired-modify-mark-on-thumb-original-file' as
subroutine and support region mark."
    (interactive)
    (if (region-active-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (deactivate-mark)
          (goto-char beg)
          (while (< (point) end)
            (when (image-dired-image-at-point-p)
              (entropy/emacs-image-dired-modify-mark-on-thumb-original-file
               'mark))
            (forward-char 1)))
      (entropy/emacs-image-dired-modify-mark-on-thumb-original-file
       'mark))
    (unless no-forward
      (ignore-errors
        (image-dired-forward-image))))

  (defun entropy/emacs-image-dired-unmark-thumb-original-file (&optional no-forward)
    "Unmark original image file in associated Dired buffer.

Like `image-dired-unmark-thumb-original-file' but using
`entropy/emacs-image-dired-modify-mark-on-thumb-original-file' as
subroutine and support region unmark."
    (interactive)
    (if (region-active-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (deactivate-mark)
          (goto-char beg)
          (while (< (point) end)
            (when (image-dired-image-at-point-p)
              (entropy/emacs-image-dired-modify-mark-on-thumb-original-file
               'unmark))
            (forward-char 1)))
      (entropy/emacs-image-dired-modify-mark-on-thumb-original-file
       'unmark))
    (unless no-forward
      (ignore-errors
        (image-dired-forward-image))))

  (defun entropy/emacs-image-dired-mark-all-thumbs ()
    "Mark all image thumbs with syncing with associated
dired buffer."
    (declare (interactive-only t))
    (interactive)
    ;; cancel the regio first since it's meaningful for
    ;; `entropy/emacs-image-dired-mark-thumb-original-file'
    (deactivate-mark)
    (entropy/emacs-image-dired-thumbnail-with-mapping-images
     (let (_)
       (entropy/emacs-image-dired-mark-thumb-original-file t))))

  (defun entropy/emacs-image-dired-unmark-all-thumbs ()
    "Unmark all image thumbs with syncing with associated
dired buffer."
    (declare (interactive-only t))
    (interactive)
    ;; cancel the regio first since it's meaningful for
    ;; `entropy/emacs-image-dired-unmark-thumb-original-file'
    (deactivate-mark)
    (entropy/emacs-image-dired-thumbnail-with-mapping-images
     (let (_)
       (entropy/emacs-image-dired-unmark-thumb-original-file t))))

;; ****** __end__
  )

;; ***** image-dired-display-image-mode
(entropy/emacs-basic-image-dired-use-package image-dired
  :ensure nil
;; ****** eemacs hydra hollow instance
  :eemacs-mmphc
  (((:enable t :defer t)
    (image-dired-display-image-mode
     (image-dired image-dired-display-image-mode-map)
     t
     (2 2 2)))
   ("Basic"
    (("RET" entropy/emacs-image-dired-display-image-buffer-redo-command
      "Redisplay the current buffer image."
      :enable t :exit t :map-inject t)
     ("C-<return>" entropy/emacs-image-dired-buffer-view-with-external-app
      "Display current displayed image’s original image with external app"
      :enable t :map-inject t :exit t)
     ("M-RET" entropy/emacs-image-dired-buffer-view-with-external-app
      "Display current displayed image’s original image with external app"
      :enable t :map-inject t :exit t)
     ("I" entropy/emacs-image-dired-enable-image-mode-in-display-buffer
      "Enable `image-mode' in current buffer."
      :enable t :exit t :map-inject t))))

;; ****** config
  :config
  (defun entropy/emacs-image-dired-buffer-view-with-external-app ()
    "View current displayed image in external app."
    (declare (interactive-only t))
    (interactive nil image-dired-display-image-mode)
    (unless (eq major-mode 'image-dired-display-image-mode)
      (user-error "Not in an dired image display spec buffer!"))
    (let ((file __ya/image-dired-display-image-buffer-image-file))
      (unless (and file (file-exists-p file))
        (user-error "No image displayed in this buffer"))
      (entropy/emacs-image-mode-external-view-union file)))

  (defun entropy/emacs-image-dired-display-image-buffer-redo-command (arg)
    "Redisplay the current `image-dired-display-image-buffer'
displayed image as same operated mechanism as
`image-dired-display-thumbnail-original-image'."
    (declare (interactive-only t))
    (interactive "P" image-dired-display-image-mode)
    (unless (eq major-mode 'image-dired-display-image-mode)
      (user-error "Not in an dired image display spec buffer!"))
    (let ((file __ya/image-dired-display-image-buffer-image-file))
      (unless (and file (file-exists-p file))
        (user-error "No image displayed in this buffer"))
      (image-dired-display-image file arg)))

  (defun entropy/emacs-image-dired-enable-image-mode-in-display-buffer ()
    "Enable `image-mode' in `image-dired-display-image-buffer'"
    (declare (interactive-only t))
    (interactive nil image-dired-display-image-mode)
    (unless (eq major-mode 'image-dired-display-image-mode)
      (user-error "Not in an dired image display spec buffer!"))
    (let ((inhibit-read-only t)
          (file __ya/image-dired-display-image-buffer-image-file)
          (modified (buffer-modified-p)))
      (unless (and file (file-exists-p file))
        (user-error "No image displayed in this buffer"))
      (unless (featurep 'image-mode)
        (require 'image-mode))
      (erase-buffer)
      (insert-file-contents-literally file)
      (image-mode)
      (restore-buffer-modified-p modified)))

;; ****** __end__
  )

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
  (entropy/emacs-lazy-load-simple 'artist
    (define-key artist-mode-map (kbd ">") nil)
    (define-key artist-mode-map (kbd "<") nil))
  )

(defun entropy/emacs-basic-do-artist-temporally ()
  "Display one temp-buffer with `artist-mode' enabled."
  (declare (interactive-only t))
  (interactive)
  (let ((buffer (generate-new-buffer "*eemacs-artist-mode-temp-buffer*")))
    (with-current-buffer buffer
      (artist-mode)
      (insert (make-string 100 ?\n))
      (goto-char (point-min)))
    (display-buffer buffer)))


;; *** Man-mode
(use-package man
  :ensure nil
  :commands (man
             entropy/emacs-Man-mode-fit-to-window)
  :eemacs-mmphc
  (((:enable t :defer t)
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

  (defvar entropy/emacs-Man-mode-fit-to-window-timer nil)
  (defun entropy/emacs-Man-mode-fit-to-window-with-idle (window)
    (unwind-protect
        (when (window-live-p window)
          (Man-fit-to-window window))
      (setq entropy/emacs-Man-mode-fit-to-window-timer
            nil)))
  (defun entropy/emacs-Man-mode-fit-to-window
      (&optional buffer inct)
    "Like `Man-fit-to-window' but with eemacs spec."
    (interactive
     (list (current-buffer) t)
     Man-mode)
    (when-let* ((buffer (or buffer (current-buffer)))
                (buffobj-p (and buffer (buffer-live-p buffer)))
                (window (get-buffer-window
                         (current-buffer)))
                (winobj-p (and window (window-live-p window))))
      (with-current-buffer buffer
        (let (_)
          (when (and (eq major-mode 'Man-mode)
                     (window-live-p window))
            (if
                ;; Use direct progn did in these conditions
                (or inct
                    (not
                     (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)))
                (unless (and entropy/emacs-Man-mode-fit-to-window-timer
                             (timerp entropy/emacs-Man-mode-fit-to-window-timer)
                             (eq (car
                                  (timer--args
                                   entropy/emacs-Man-mode-fit-to-window-timer))
                                 window))
                  (Man-fit-to-window window)))
            ;; FIXME: why in wc auto center mode the progn did not
            ;; work? (so we use idle timer do as instead)
            (unless entropy/emacs-Man-mode-fit-to-window-timer
              (setq entropy/emacs-Man-mode-fit-to-window-timer
                    (run-with-idle-timer
                     0.2 nil
                     #'entropy/emacs-Man-mode-fit-to-window-with-idle
                     window))))))))

  (add-hook 'entropy/emacs-window-center-enable-after-hook
            #'entropy/emacs-Man-mode-fit-to-window)
  (add-hook 'entropy/emacs-window-center-disable-after-hook
            #'entropy/emacs-Man-mode-fit-to-window)

  )

(use-package woman
  :ensure nil
  :defer entropy/emacs-fall-love-with-pdumper
  :commands woman
  :init
  (setq woman-fill-column 100
        woman-dired-keys nil)
  ;; Disable the internal warning
  (with-eval-after-load 'warnings       ;FIXME: why? is it not preloading?
    (add-to-list 'warning-suppress-types
                 '((defvaralias losing-value woman-topic-history)))
    (add-to-list 'warning-suppress-log-types
                 '((defvaralias losing-value woman-topic-history))))
  :config

  (defun __ya/woman-really-find-file (filename compressed bufname)
    "Like `woman-really-find-file' but respect eemacs `shackle-mode'
specified."
    (let ((WoMan-current-file filename))	; used for message logging
      (if woman-use-own-frame
          (select-frame
           (or (and (frame-live-p woman-frame) woman-frame)
               (setq woman-frame (make-frame)))))
      (with-current-buffer (get-buffer-create bufname)
        (buffer-disable-undo)
        (let ((inhibit-read-only nil))
          (erase-buffer)			; NEEDED for reformat
          (woman-insert-file-contents filename compressed)
          ;; Set buffer's default directory to that of the file.
          (entropy/emacs-set-default-directory (file-name-directory filename))
          (setq-local backup-inhibited t)
          (set-visited-file-name "")
          (woman-process-buffer)
          ;; EEMACS_TEMPORALLY_HACK: scroll to the top of buffer since
          ;; the orig-func cause to the tail of buffer
          (goto-char (point-min)))
        ;; EEMACS_TEMPORALLY_HACK: Finally popup the buffer since we
        ;; do not want to see the buffer processing procedure.
        (if woman-use-own-frame
            (condition-case nil
                (pop-to-buffer-same-window (current-buffer))
              (error (pop-to-buffer (current-buffer))))
          (pop-to-buffer (current-buffer))))))
  (advice-add 'woman-really-find-file
              :override
              #'__ya/woman-really-find-file)


  )

;; ** Basic global settings:

;; *** Editor spec
;; **** Buffer default UI
;; ***** Set default cursor style
(setq-default cursor-type t)

(defun entropy/emacs-basic-toggle-cursor-type ()
  "Toggle cursor display type."
  (declare (interactive-only t))
  (interactive)
  (if (string= (symbol-name cursor-type) "t")
      (setq cursor-type 'bar)
    (setq cursor-type t)))

;; ***** Hl-line And Line Numbers display

(use-package hl-line
  :ensure nil
  :commands (hl-line-mode
             global-hl-line-mode
             hl-line-overlay)
  :defines (hl-line-mode
            global-hl-line-mode))

(use-package display-line-numbers
  :ensure nil
  :commands (display-line-numbers-mode)
  :defines (display-line-numbers-mode))

;; ****** Global display line number mode
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
              (bound-and-true-p entropy/emacs-ui-init-welcom-mode)
              ;; we should always restirct
              ;; `global-display-line-numbers-mode' in `prog-mode' or
              ;; its derived modes.
              (and (bound-and-true-p global-display-line-numbers-mode)
                   (not (derived-mode-p 'prog-mode))))
    (apply orig-func orig-args)))

(advice-add 'display-line-numbers-mode
            :around
            #'entropy/emacs-basic--dspln-mode-around-advice)

(entropy/emacs-lazy-initial-advice-before
 '(find-file switch-to-buffer)
 "global-display-line-numbers-mode"
 "global-display-line-numbers-mode"
 :prompt-type 'prompt-echo
 :pdumper-no-end nil
 (setq-default display-line-numbers-width-start t)
 (when entropy/emacs-init-display-line-numbers-mode
   (global-display-line-numbers-mode)))

;; ****** Global hl-line mode
(defun entropy/emacs-turn-on-hl-line-mode ()
  "The filter for `hl-line-mode' available option in triggered
buffer, in that case any conditions don't match the filter then
`hl-line-mode' will be enabled."
  (unless (or
           ;; special modes and buffers
           (member major-mode
                   '(vterm-mode
                     shell-mode
                     eshell-mode
                     term-mode
                     dashboard-mode))
           (bound-and-true-p entropy/emacs-ui-init-welcom-mode)
           (eq major-mode 'eww-mode)
           (minibufferp)
           ;; large buffers
           (let ((rtn
                  (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos))))
             (> rtn 1000))
           ;; magit related buffers, since it has its own line highlight methods
           (string-match-p "magit" (symbol-name major-mode))
           ;; TODO: add more expections
           )
    (hl-line-mode 1)))

;; Build `hl-line-mode' based global mode, that it is different from
;; `global-hl-line-mode' which use `global-hl-line-highlight-all' as
;; the subroutine. So that we can toggle line highlight feature
;; without global status restriction anti from what
;; `global-hl-line-mode' did.
(define-globalized-minor-mode entropy/emacs-hl-line-global-mode
  hl-line-mode
  entropy/emacs-turn-on-hl-line-mode
  :group 'hl-line)

(entropy/emacs-lazy-initial-advice-before
 '(find-file switch-to-buffer)
 "global-hl-line-mode" "global-hl-line-mode"
 :prompt-type 'prompt-echo
 :pdumper-no-end nil
 (when entropy/emacs-init-hl-line-mode
   (entropy/emacs-hl-line-global-mode 1)))

;; ****** Highlight current line and display line numbers
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
  (declare (interactive-only t))
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

(defun __bugfix/hl-line-unhighlight/for-timer-var-clean ()
  "Bug fix for `hl-line-unhighlight' missing timer unset procedure."
  (when hl-line-overlay
    (delete-overlay hl-line-overlay)
    (setq hl-line-overlay nil)))
(advice-add 'hl-line-overlay
            :override
            #'__bugfix/hl-line-unhighlight/for-timer-var-clean)

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
             entropy/emacs-current-session-is-idle-p)
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
(entropy/emacs-lazy-initial-advice-before
 '(dired-mode)
 "hl-line-spec-for-dired-init" "hl-line-spec-for-dired-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (dolist (el '(dired-previous-line dired-next-line))
   (advice-add el
               :around
               #'entropy/emacs-basic--hl-line-disable-wrapper)))

;; ***** Smooth scrolling
;; Force smooth mouse scroll experience
(when (display-graphic-p)
  (setq
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
   mouse-wheel-progressive-speed nil))

(defvar-local entropy/emacs-basic--next-screen-context-lines-orig-value next-screen-context-lines)
(defvar-local entropy/emacs-basic--scroll-margin-orig-value scroll-margin)
(defvar-local entropy/emacs-basic--scroll-conservatively-orig-value scroll-conservatively)

(defun entropy/emacs-basic-smooth-scrolling-mode-turn-on ()
  (unless (and (functionp entropy/emacs-unreadable-buffer-judge-function)
               (funcall entropy/emacs-unreadable-buffer-judge-function (current-buffer)))
    (entropy/emacs-basic-smooth-scrolling-mode 1)))

(define-minor-mode entropy/emacs-basic-smooth-scrolling-mode
  "Toggle smooth-scrolling buffer scrolling view."
  :lighter "SM"
  :group 'scrolling
  (if entropy/emacs-basic-smooth-scrolling-mode
      (progn
        (setq entropy/emacs-basic--next-screen-context-lines-orig-value next-screen-context-lines)
        (setq entropy/emacs-basic--scroll-margin-orig-value scroll-margin)
        (setq entropy/emacs-basic--scroll-conservatively-orig-value scroll-conservatively)
        (setq-local
         next-screen-context-lines 0
         scroll-margin             0
         scroll-conservatively     100))
    (progn
      (setq-local
       next-screen-context-lines entropy/emacs-basic--next-screen-context-lines-orig-value
       scroll-margin             entropy/emacs-basic--scroll-margin-orig-value
       scroll-conservatively     entropy/emacs-basic--scroll-conservatively-orig-value)
      )))

(define-globalized-minor-mode
  entropy/emacs-basic-smooth-scrolling-global-mode
  entropy/emacs-basic-smooth-scrolling-mode
  entropy/emacs-basic-smooth-scrolling-mode-turn-on
  :group 'scrolling)

(entropy/emacs-lazy-with-load-trail
  'eemacs-smooth-scrolling-mode-init
  (entropy/emacs-basic-smooth-scrolling-global-mode 1))


;; ***** Tab default visualization

;; Do not use `indent-tabs-mode' by default for compatibility meaning
;; that tabs visualization are not unified accorss editor.
(if entropy/emacs-custom-tab-enable
    (setq-default tab-width entropy/emacs-custom-tab-width)
  (setq-default indent-tabs-mode nil))

;; **** Buffer operations
;; ***** Initiative operations
;; ****** Input time into buffer
(defun entropy/emacs-basic-now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (declare (interactive-only t))
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))

(defun entropy/emacs-basic-today ()
  "Insert string for today's date nicely formatted in American style,
 e.g. Sunday, September 17, 2000."
  (declare (interactive-only t))
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; ****** Undo and Redo
;; ******* Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure nil
  :commands (global-undo-tree-mode undo-tree-visualize)
  :preface

  (defvar-local entropy/emacs-basic--undo-tree-stick-window-configuration nil
    "The window configuration before calling `undo-tree-visualize'.")

  (defvar-local entropy/emacs-basic--undo-tree-stick-autocenter-mode nil
    "Whether `entropy/emacs-window-center-mode' is enabled in
    `current-buffer'.")

  (defun entropy/emacs-basic--save-window-cfg-for-undo-tree
      (orig-func &rest orig-args)
    ;; firstly reset the indicator (clean the previous set)
    (setq entropy/emacs-basic--undo-tree-stick-autocenter-mode nil)
    (when (bound-and-true-p entropy/emacs-window-center-mode)
      (unless (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
        (entropy/emacs-window-center-mode 0))
      (setq entropy/emacs-basic--undo-tree-stick-autocenter-mode t))
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
      (when entropy/emacs-basic--undo-tree-stick-autocenter-mode
        (unless (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
          (entropy/emacs-window-center-mode)))
      rtn))

  :init
  (entropy/emacs-lazy-initial-advice-before
   '(switch-to-buffer find-file)
   "undo-tree-enable-init"
   "undo-tree-enable-init"
   :prompt-type 'prompt-echo
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

;; ****** Case type toggle
;; ******* Improve captialize function
(defun entropy/emacs-basic-toggle-case-core (case-type)
  "Toggle case type of region begin and end by CASE-TYPE while
`region-active-p' return non nil, or use `backward-word' with arg
1 as begin and the `current-point' as end.

CASE-TYPE can be one of 'capitalize' 'downcase' 'upcase'."
  (let (region func)
    ;; region set
    (cond
     ((region-active-p)
      (setq region
            (list (region-beginning)
                  (region-end))))
     (t
      (setq region
            (list (save-excursion
                    (forward-word -1)
                    (point))
                  (point)))))
    (pcase case-type
      ('capitalize (setq func #'capitalize-region))
      ('downcase (setq func #'downcase-region))
      ('upcase (setq func #'upcase-region))
      (_
       (error "Wrong type of caes-type '%s'"
              case-type)))
    (apply func region)))

(defun entropy/emacs-basic-toggle-case-for-capitalize ()
  "Toggle current region or thing capitalized."
  (declare (interactive-only t))
  (interactive)
  (entropy/emacs-basic-toggle-case-core 'capitalize))
(defun entropy/emacs-basic-toggle-case-for-downcase ()
  "Toggle current region or thing downcase."
  (declare (interactive-only t))
  (interactive)
  (entropy/emacs-basic-toggle-case-core 'downcase))
(defun entropy/emacs-basic-toggle-case-for-upcase ()
  "Toggle current region or thing upcase."
  (declare (interactive-only t))
  (interactive)
  (entropy/emacs-basic-toggle-case-core 'upcase))

(entropy/emacs-lazy-initial-advice-after
 '(find-file switch-to-buffer prog-mode fundamental-mode)
 "entropy-toggle-case-hydra-hollow-init"
 "entropy-toggle-case-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'words-manipulation nil
  '("Basic"
    (("M-c" entropy/emacs-basic-toggle-case-for-capitalize
      "Captalize Word/Region"
      :enable t
      :exit t
      :global-bind t)
     ("M-l" entropy/emacs-basic-toggle-case-for-downcase
      "Down Case Word/Region"
      :enable t
      :exit t
      :global-bind t)
     ("M-u" entropy/emacs-basic-toggle-case-for-upcase
      "Upcase Word/Region"
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
      :exit t)))))

;; ****** Auto-sudoedit

(use-package auto-sudoedit
  :commands (entropy/emacs-sudoedit-current-path-maybe)
  :config
  (defun entropy/emacs-sudoedit-current-path-maybe (curr-path)
    "Get su privileges for CURR-PATH if need to so."
    (declare (interactive-only t))
    (interactive (list (auto-sudoedit-current-path)))
    (let* ((cur-buff (current-buffer))
           (cur-buff-fname (buffer-file-name cur-buff))
           (autosudo-obj (auto-sudoedit-path curr-path))
           (type (car autosudo-obj))
           (path (cdr autosudo-obj)))
      (if (and type path)
          (progn
            (let (_)
              (find-file path))
            ;; kill the current file buffer firstly to ensure the
            ;; tramped one is the only one visited the file so that we
            ;; do not need to do some file steal operation any more.
            (when (and cur-buff-fname
                       (not (file-remote-p cur-buff-fname)))
              (kill-buffer cur-buff)))
        (entropy/emacs-message-do-message
         "no need to get sudo permission to edit path %s"
         (green (format "%s" curr-path)))))))


;; ****** Rectangle manipulation

(use-package rect
  :ensure nil
  :eemacs-adrequire
  ((:enable t
    :adfors (transient-mark-mode-hook)
    :adtype hook
    :pdumper-no-end t))
  :eemacs-indhc
  (((:enable t :defer t)
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
  (((:enable t :defer t))
   ("Utils"
    (("u r"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'rectangle-mode))
      "Rectangle region manipulation"
      :enable t :exit t)))))

;; ****** Major mode reload
(defun entropy/emacs-basic-major-mode-reload ()
  "Reload current `major-mode'.

This function was usable for some occurrence that current mode
functional part was missed or be without working on the proper
way. "
  (declare (interactive-only t))
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

;; ****** Kill-buffer-and-window spec

(defun entropy/emacs-basic-kill-ansiterm-buffer ()
  "Kill `ansi-term' buffer with proper way.

This function mainly gives a patch for the bug of error after
kill ansi-term buffer and its popup window refer to bug
#h-0c3ab89e-a470-42d2-946e-4f217ea2f20c in entropy-emacs bug
collection."
  (interactive)
  (if sys/linuxp
      (let* ((buff (current-buffer))
             (proc (get-buffer-process buff)))
        (when proc
          (when (yes-or-no-p
                 (format "Buffer %S has a running process; kill it? "
                         (buffer-name buff)))
            (set-process-filter proc nil)
            (kill-process proc)
            (let ((kill-buffer-query-functions '((lambda () t))))
              (if (not (one-window-p))
                  (kill-buffer-and-window)
                (kill-this-buffer))))))
    (kill-this-buffer)))

(defun entropy/emacs-basic-kill-buffer-and-show-its-dired ()
  "Kill buffer, swtich to its hosted location `dired' buffer when
its a exists file's buffer."
  (interactive)
  (let ((buff (current-buffer))
        (fname (buffer-file-name)))
    (kill-buffer buff)
    (when (file-exists-p fname)
      (dired (file-name-directory fname)))))

(defun entropy/emacs-basic-kill-buffer-and-its-window-when-grids ()
  "Kill buffer and close it's host window if windows conuts
retrieve from `window-list' larger than 1."
  (interactive)
  (let ((winlist (window-list)))
    (if (> (length winlist) 1)
        ;; -------------------- grid case
        (cond
         (
          ;; === Just kill buffer even if is grid case
          (or
           ;; Two windows displayed and one is dedicated with
           ;; `no-delete-other-windows'.
           (and (= (length winlist) 2)
                (let (rtn)
                  (catch :exit
                    (mapc (lambda (win)
                            (let (_)
                              (when (window-parameter
                                     win
                                     'no-delete-other-windows)
                                (setq rtn t)
                                (throw :exit nil))))
                          winlist))
                  rtn))
           ;; when the window is the root window of current frame we
           ;; just kill it, since in some cases the main window
           ;; may not be the only live window showed in current frame
           ;; as messy and why?.
           (eq (window-main-window) (get-buffer-window (current-buffer))))
          (kill-buffer))
         ;; === default grid case
         (t
          (kill-buffer-and-window)))
      ;; -------------------- default case
      (kill-buffer))))

(defun entropy/emacs-basic-kill-buffer ()
  "Entropy emacs specified `kill-buffer' method, used for replace
as thus."
  (declare (interactive-only t))
  (interactive)
  (cond
   ((eq major-mode 'term-mode)
    (call-interactively #'entropy/emacs-basic-kill-ansiterm-buffer)
    (message "Kill current ansi-term buffer done."))
   ((and (buffer-file-name)
         ;; not the 2 window horizontal overlay with the other one
         ;; (not selected one) is the dired which match the dir of
         ;; current `default-directory'.
         (let ((current-defdir default-directory)
               (win-is-2hp (entropy/emacs-window-overlay-is-2-horizontal-splits-p nil t)))
           (if win-is-2hp
               (with-current-buffer (window-buffer win-is-2hp)
                 (if (and
                      (eq major-mode 'dired-mode)
                      (entropy/emacs-existed-filesystem-nodes-equal-p
                       default-directory current-defdir))
                     nil
                   t))
             t)))
    (call-interactively #'entropy/emacs-basic-kill-buffer-and-show-its-dired)
    (message "Revert to the dired of current buffer's `default-directory' done."))
   (t
    (call-interactively
     #'entropy/emacs-basic-kill-buffer-and-its-window-when-grids)
    (message "Kill current buffer and its window done."))))

(global-set-key (kbd "C-x k") #'entropy/emacs-basic-kill-buffer)
(global-set-key (kbd "C-x M-k") #'kill-buffer)

;; ***** Implicit trigger
;; ****** Disable `electric-indent-mode' by default

;; Remove it globally since we do not use it frequently in non prog
;; mode and it well inject to `post-self-insert-hook' to increase
;; performance latency.
(entropy/emacs-lazy-initial-advice-before
 '(find-file switch-to-buffer)
 "__disable-electric-indent-mode" "__disable-electric-indent-mode"
 :prompt-type 'prompt-echo
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

;; ****** Replace follow input in region
;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(yank xterm-paste)
   "delsel-mode-init" "delsel-mode-init"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (delete-selection-mode 1))
  :config
  ;; EEMACS_MAINTENANCE:
  ;;
  ;; Command without 'delete-selection' property will not delete the
  ;; region before insertion.
  (put 'entropy/emacs-xterm-paste 'delete-selection t))

;; ****** Auto wrap line
(setq-default truncate-lines t)

;; ****** Paragraph fill size
(setq-default fill-column entropy/emacs-fill-paragraph-width)

;; ****** Auto clean whitespace after save buffer
(use-package whitespace
  :ensure nil
  :commands (whitespace-cleanup)
  :preface
  (defun entropy/emacs-basic-simple-whitespace-clean ()
    "Clean whitespace with the default `whitspace-style' (i.e. as the same
as official initialized `defcustom' one) unless
`entropy/emacs-inhibit-simple-whitespace-clean' is non-nil.

NOTE: for eemacs maintainer may confused with the `whitespace'
configuratioi in `entropy-emacs-highlight.el', the difference is in
the letter file is configured for display style and the former is for
coding style."
    (interactive)
    (with-current-buffer (current-buffer)
      (if (or entropy/emacs-inhibit-simple-whitespace-clean
              ;; just used in interactive emacs session since we don't
              ;; want to fluence the batch-mode while user use elisp
              ;; as a shell or do non-ui related operations.
              noninteractive)
          (ignore)
        (require 'whitespace)
        (let ((whitespace-style (entropy/emacs-get-symbol-defcustom-value
                                 'whitespace-style)))
          (let ((inhibit-read-only t))
            (whitespace-cleanup)
            (message "whitespace cleanup for current buffer <%s> done"
                     (current-buffer)))))))
  :init
  (add-hook 'before-save-hook
            #'entropy/emacs-basic-simple-whitespace-clean))


;; ****** Global read only mode
(use-package entropy-global-read-only-mode
  :ensure nil
  :commands (entropy-grom-mode
             entropy/grom-read-only-buffer
             entropy/grom-quick-readonly-global)
  :eemacs-indhc
  (((:enable t :defer t)
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
  (((:enable t :defer t))
   ("WI&BUF"
    (("L"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'entropy-grom-mode))
      "Buffer locker both for single or global buffer-list"
      :enable t :exit t))))

  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file push-button find-library-name switch-to-buffer)
   "entropy-grom"
   "entropy-grom"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (entropy-grom-mode +1))

  :config
  (dolist (rule `(,(rx "*outorg-edit-buffer*")
                  ,(rx "*Buffer Details*")
                  ,(rx "*Memory Explorer*")
                  ,(rx "*poporg: ")
                  ,(rx "*edit-indirect ")
                  ,(rx "*Org Src test org")))
    (add-to-list 'entropy/grom-customizable-nonspecial-buffer-name-regexp-list
                 rule))

  ;; treat more functions `inhibit-read-only'
  (dolist (func '(add-dir-local-variable))
    (entropy/emacs-make-function-inhibit-readonly
     func))
  )

;; ****** Auto-Save
(setq-default auto-save-default nil)    ; disable it for preventing typing lagging
(setq make-backup-files nil)

;; ****** Auto Revert buffer

(use-package autorevert
  :ensure nil
  :commands
  (global-auto-revert-mode
   auto-revert-mode)
  :init
  (setq
   ;; immediatly revert to prevent user messy but not set too small
   ;; delay timer since system drives I/O is time expensive.
   auto-revert-interval 5
   ;; but not disturb normal user inputs
   auto-revert-stop-on-user-input t
   ;; ignore modes check since we want to check every visited file
   global-auto-revert-ignore-modes nil
   ;; we should see what heppens to track bugs
   auto-revert-verbose t
   )
  ;; disable lagging since vc check is time expensive
  (setq auto-revert-check-vc-info nil))

(entropy/emacs-lazy-initial-for-hook
 '(find-file-hook)
 "GlbAutoRevertMode"
 "GlbAutoRevertMode-enabled"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (global-auto-revert-mode +1))

(defun entropy/emacs-basic-toggle-global-auto-revert-mode ()
  "Toggle auto-revert global bounds of feature on or off.

NOTE: e.g. `global-auto-revert-mode' and `magit-auto-revert-mode'."
  (declare (interactive-only t))
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
     (mgav-off
      (message "Toggle `magit-auto-revert-mode' off.")))))

;; *** Emacs-wide spec
;; **** emacs source dir
;; Set `source-directory' to eemacs specified location
(entropy/emacs-lazy-initial-for-hook
 '(entropy/emacs-after-startup-hook)
 "init-emacs-source-dir-set" "init-emacs-source-dir-set"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (setq source-directory
       (entropy/emacs-source-directory))
 ;; We must reeset this var since its preloaded before emacs startup.
 ;; EEMACS_MAINTENANCE: follow emacs version
 (defvar find-function-C-source-directory)
 (setq find-function-C-source-directory
       (let ((dir (expand-file-name "src" source-directory)))
         (if (file-accessible-directory-p dir) dir))))

;; **** Scratch buffer corresponding file
;;
;;     Amounts of company backend function can not functional
;;     auto-completion in none file buffer, so corresponding one file
;;     to *scratch* buffer.

(defvar magit-inhibit-refresh-save)
(defun entropy/emacs-basic--scratch-buffer-file-binding ()
  "Bind *scratch* buffer to one persist file.

The persist file is \".scratch_entropy\" host in `entropy/emacs-stuffs-topdir'.

Return the new scratch buffer.
"
  (let* ((bfn "*scratch*")
         (main-func
          (lambda (&rest _)
            (with-current-buffer (current-buffer)
              ;; make scratch buffer as un-readonly defaultly
              (if buffer-read-only (setq buffer-read-only nil))
              ;; insert the `initial-scratch-message'
              (save-excursion
                (goto-char (point-min))
                (unless (looking-at (regexp-quote initial-scratch-message))
                  (insert initial-scratch-message)))
              ;; disable `auto-save-mode' since we do not want to auto
              ;; backup the scratch contents
              (when (bound-and-true-p buffer-auto-save-file-name)
                (auto-save-mode 0))
              ;; rename to scratch buffer name
              (rename-buffer bfn)
              ;; follow basic scratch buffer major-mode spec
              (unless (eq major-mode 'lisp-interaction-mode)
                (lisp-interaction-mode))
              ;; ignore scratch buffer file for `magit-save-repository-buffers' checker.
              (setq-local magit-inhibit-refresh-save t)
              ;; final procedures
              (goto-char (point-min))))))
    ;; kill the origin one firstly
    (when (get-buffer bfn)
      (kill-buffer (get-buffer bfn)))
    ;; main
    (let ((fname (expand-file-name ".scratch_entropy" entropy/emacs-stuffs-topdir))
          ;; inhibit `find-file-hook' for speedup file create
          (find-file-hook nil))
      (unless (file-exists-p fname)
        (write-region "" nil fname))
      (with-current-buffer (find-file-noselect fname)
        (funcall main-func)))
    ;; return the buffer
    (get-buffer bfn)))

(entropy/emacs-lazy-initial-advice-before
 '(find-file switch-to-buffer ivy-read)
 "init-eemamcs-scratch-buffer" "init-eemamcs-scratch-buffer"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-basic--scratch-buffer-file-binding))

;; Create a new scratch buffer
(defun entropy/emacs-basic-create-scratch-buffer ()
  "Create a scratch buffer."
  (declare (interactive-only t))
  (interactive)
  (switch-to-buffer (entropy/emacs-basic--scratch-buffer-file-binding))
  (message "Create *scratch* buffer done"))

;; **** Kill ring config
;; ***** basic

;; Cutting and pasting uses the system clipboard
(setq select-enable-clipboard t)

;; save system existing clipboard text into kill ring before replacing
;; it and clipboard data is only saved to the ‘kill-ring’ when its
;; string length shorter than 2000.
(setq save-interprogram-paste-before-kill 2000)

;; Disable duplicates filter since we always want to see the 'cut' in
;; last of `kill-ring'
(setq kill-do-not-save-duplicates nil)

;; restrict `kill-ring' length for reducing `browse-kill-ring' lag
(setq kill-ring-max 300)

;; Don't change the system clipboard when call yank refers in emacs
(setq yank-pop-change-selection nil)

;; ***** persist kill ring
(defvar entropy/emacs-basic-kill-ring-persist-lock-file
  (expand-file-name
   ".kill-ring-persist.lock"
   entropy/emacs-stuffs-topdir)
  "The persist kill ring file used indicator flag file.")

(defun entropy/emacs-basic-kill-ring-persist-backup ()
  (when (file-exists-p entropy/emacs-kill-ring-persist-file)
    (entropy/emacs-simple-backup-file
     entropy/emacs-kill-ring-persist-file)))

;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/emacs-basic-clear-kill-ring ()
  "Clear `kill-ring' with confirmation."
  (declare (interactive-only t))
  (interactive)
  (when (yes-or-no-p "Clean kill ring? ")
    (entropy/emacs-basic-kill-ring-persist-backup)
    (setq kill-ring nil)
    (garbage-collect)))

(defvar entropy/emacs-basic-kill-ring-persist-error-log nil)
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
         (to-buffer (progn
                      ;; create the kill-ring persist host root firstly
                      (let ((file-host (file-name-directory file)))
                        (unless (file-exists-p file-host)
                          (make-directory file-host t)))
                      (let ((large-file-warning-threshold
                             ;; disable the large file restriction
                             ;; since we do not modify it manually.
                             most-positive-fixnum))
                        (find-file-noselect file))))
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
                             'entropy/emacs-basic-kill-ring-persist-error-log)
                    (progn (prin1 item to-buffer)
                           (insert ?\n))
                  ;; Warn that item is not readable
                  (entropy/emacs-message-do-message
                   "%s%s%s%s"
                   (red "⚠: ")
                   "kill ring item "
                   (yellow (format "'%s'" item))
                   (format " can not be saved because of [%s] !"
                           entropy/emacs-basic-kill-ring-persist-error-log)))))
            (insert ")")
            (with-temp-message ""
              (let ((inhibit-message t)
                    ;; tidy up buffer write context
                    (write-contents-functions nil)
                    (write-file-functions nil)
                    (before-save-hook nil))
                (save-buffer))))
          (when remove-lock
            (when (file-exists-p entropy/emacs-basic-kill-ring-persist-lock-file)
              (f-delete entropy/emacs-basic-kill-ring-persist-lock-file))))
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
           (buffer (let ((large-file-warning-threshold most-positive-fixnum))
                     (find-file-noselect file)))
           (inhibit-read-only t)
           kill-ring-read
           (kill-ring-old (copy-sequence kill-ring))
           (rtn t))
      (with-current-buffer buffer
        (goto-char (point-min))
        (if (or (string-empty-p (buffer-substring-no-properties
                                 (point) (point-max)))
                ;; just has comments content in this buffer
                (null
                 (save-excursion
                   (re-search-forward "^[^;]" nil t))))
            (progn
              (message "Empty persist file")
              (save-buffer)
              (setq rtn t))
          (condition-case error
              (let ((coding-system-for-read 'utf-8-auto)
                    (coding-system-for-write 'utf-8-auto))
                (setq kill-ring-read (read (current-buffer)))
                ;; append the current `kill-ring' to the persist one
                ;; as we need the new to be insert as.
                (setq kill-ring (append kill-ring kill-ring-read)))
            (error
             (setq kill-ring kill-ring-old
                   rtn nil)
             (unless (= (buffer-size) 0)
               (warn (format "Read persit kill-ring fatal of [%s], fallen back to origin done!"
                             error))
               (entropy/emacs-basic-kill-ring-persist-backup))))))
      ;; just generate the lock file while kill-ring init successfully
      (when rtn
        (f-touch entropy/emacs-basic-kill-ring-persist-lock-file))
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
 '(
   ;; yank query operations
   yank-pop
   counsel-yank-pop
   ;; kill-ring side effects operations
   kill-ring-save kill-line backward-kill-word
   ;; yank operations
   yank xterm-paste)
 "kill-ring-persist-init" "kill-ring-persist-init"
 :prompt-type 'prompt-echo
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
  (when (file-exists-p entropy/emacs-basic-kill-ring-persist-lock-file)
    (f-delete entropy/emacs-basic-kill-ring-persist-lock-file t))
  (remove-hook 'kill-emacs-hook
               #'entropy/emacs-basic-kill-ring-persist-after-kill-emacs))


;; ***** remove redundant kill-ring save operation

(defun __ya/backward-kill-word (arg)
    "Alternative for `backward-kill-word' but not trigger
`kill-ring-save'."
    (declare (interactive-only t))
    (interactive "p")
    (delete-region
     (point)
     (progn (forward-word (- arg)) (point))))

(global-set-key [remap backward-kill-word]
                #'__ya/backward-kill-word)

;; **** Delete file to trash
(if entropy/emacs-dired-enable-trash
    (setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder but with some
                                               ; performance bug on windows plattform.
  )

;; **** Epa (emacs gpg assistant)
(use-package epa
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-for-hook
   '(dired-mode-hook find-file-hook)
   "epa-mode" "epa-mode"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (epa-file-enable))

  (when (and entropy/emacs-microsoft-windows-unix-emulator-enable
             (file-exists-p entropy/emacs-microsoft-windows-unix-emulator-bin-path))
    (entropy/emacs-lazy-load-simple 'custom
      (custom-set-variables
       '(epg-gpg-program
         (expand-file-name
          "gpg.exe"
          entropy/emacs-microsoft-windows-unix-emulator-bin-path))
       '(epg-gpgconf-program
         (expand-file-name
          "gpgconf.exe"
          entropy/emacs-microsoft-windows-unix-emulator-bin-path))
       '(epg-gpgsm-program
         (expand-file-name
          "gpgsm.exe"
          entropy/emacs-microsoft-windows-unix-emulator-bin-path))))))

;; **** Autocompression moode

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
        :prompt-type 'prompt-echo
        :pdumper-no-end t
        (auto-compression-mode 0)
        (auto-compression-mode 1))))


;; **** History && Recentf
(use-package saveplace
  :ensure nil
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file switch-to-buffer dired counsel-dired)
   "save-place-init" "save-place-init"
   :prompt-type 'prompt-echo
   ;; injects into pdumper recovery session since the save-place is
   ;; dynamic
   :pdumper-no-end nil
   (save-place-mode)))

(use-package recentf
  :if entropy/emacs-use-recentf
  :commands (recentf-mode)
  :ensure nil
  :preface
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file switch-to-buffer dired counsel-dired)
   "recentf-init" "recentf-init"
   :prompt-type 'prompt-echo
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
   '(find-file ivy-switch-buffer dired counsel-dired)
   "savehist-init-for-find-file-refer"
   "savehist-init-for-find-file-refer"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (savehist-mode t))

  :config
  (setq
   history-length 100  ; NOTE: we strongly use shorter length to
                       ; preserve current emacs session performance
                       ; for long time sicne some package internal
                       ; binding huge stakcs rely on this variable
                       ; e.g. `ivy--sessions'.
   savehist-additional-variables
   '(mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history)
   savehist-ignored-variables
   '(kill-ring)
   savehist-autosave-interval 60))

;; **** Disable-mouse-wheel and more
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :commands (global-disable-mouse-mode)
  :init
  (when entropy/emacs-disable-mouse-at-init-time
    (entropy/emacs-lazy-with-load-trail
      'disable-mouse
      (global-disable-mouse-mode t))))

;; **** Disable auto-window-vscroll

;; see
;; `https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag'
;; for details.
;;
;; This is the main lag causer for the line navigation lag. Especially
;; that use the powerline or some modeline type based on it which
;; oftenly provide the buffer 'v-scroll' indicator that for thus.

(setq auto-window-vscroll nil)

;; **** Popup key stroking prompt
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (entropy/emacs-lazy-with-load-trail 'which-key
    :pdumper-no-end t
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

;; ***** config
  :config
;; ****** Disable `after-make-frame-functions' when popup which-key dim using frame feature.
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


;; **** Inhibit gui dialog
(setq use-file-dialog nil
      use-dialog-box nil)

;; **** Key modification

;; ***** more conventions

;; arrange multi easy to mis-hints keyboard shortcuts to `keyboard-quit'
(defvar ivy-minibuffer-map)
(let ((keys '(
              ;; when unintended active this, using 'QUIT' as 'C-g'
              "C-M-g"
              ;; same as above of super key intended active
              "C-s-g"
              ;; same as above of super key intended active
              "A-C-g"
              ;; same as above of super key intended active
              "C-M-s-g"
              )))
  ;; for global mode map
  (dolist (key keys)
    (global-set-key (kbd key) 'keyboard-quit))

  ;; also define the minibuffer specified `keboard-quit' function
  (eval-after-load 'ivy
    `(dolist (key ',keys)
       (define-key ivy-minibuffer-map
         (kbd key) 'minibuffer-keyboard-quit))))

;; ***** key re-mapping
;; Binding 'super' and 'hyper' on win32 and mac.
;;   the idea form `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'

(defvar w32-apps-modifier)
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-control-modifier)
(defvar ns-function-modifier)
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

;; ***** xterm re-bind

;; Rebind "insert" refer key in terminal emacs to support yank&cut
;; communication with GUI.

(defvar term-raw-map)
(entropy/emacs-lazy-initial-advice-before
 '(xterm-paste kill-ring-save yank)
 "xterm-rebind-init" "xterm-rebind-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
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
            (funcall ',cli-enable-func)
            (funcall ',term-enable-func)))
        (disable-func
         `(lambda ()
            (funcall ',cli-disable-func)
            (funcall ',term-disable-func))))
   (if (null (daemonp))
       (when (entropy/emacs-xterm-cut-or-yank-sync-with-system/functional-env-statisfied-p)
         (funcall enable-func))
     (defvar entropy/emacs-basic--xterm-paste-rebinded nil)
     (entropy/emacs-with-daemon-make-frame-done
      'xterm-paste-bind
      `(when (entropy/emacs-xterm-cut-or-yank-sync-with-system/functional-env-statisfied-p)
         (funcall ,enable-func)
         (setq entropy/emacs-basic--xterm-paste-rebinded t))
      `(when entropy/emacs-basic--xterm-paste-rebinded
         (funcall ,disable-func)
         (setq entropy/emacs-basic--xterm-paste-rebinded nil))))))

;; **** Bookmarks
(use-package bookmark
  :init
  (setq bookmark-save-flag 1))

;; **** Description | Help mode improvement
;; ***** EEMACS_MAINTENANCE - Bug fix

(entropy/emacs--api-restriction-uniform 'describe-key/bug/xterm-paste/advice-patch
    'emacs-version-incompatible
  :doc "Fix the `help--analyze-key' error while terminal emacs session can not
read the event correctly since the pseudo terminal instance's
implementation.

We use the \"xterm refer bug fix\" as name since it's the first time
that we found this bug i.e. in the `describe-key' operation for
[xterm-paste] keystroke in emacs terminal session which has below
backtrace:

#+begin_example
  Debugger entered--Lisp error: (wrong-type-argument listp \"hello world\")
    posn-set-point(\"hello world\")
    help--analyze-key([(xterm-paste \"hello world\")] [27 91 50 48 48 126] nil)
    #f(compiled-function (x) #<bytecode -0x8df78d18b617c1b>)(([(xterm-paste \"hello world
    describe-key((([(xterm-paste \"hello world\")] . [27 91 50 48 48 126])))
    funcall-interactively(describe-key (([(xterm-paste \"hello world\")] . [27 91 50 48 4$
    command-execute(describe-key)
#+end_example
"
  :detector (version< "29" (number-to-string emacs-major-version))
  :signal (entropy/emacs-do-error-for-emacs-version-incompatible
           '<= "29")
  (when (or (not (display-graphic-p)) (daemonp))
    (advice-patch 'help--analyze-key
                  '(save-excursion
                     (let ((evpt (event-end event)))
                       (and (number-or-marker-p evpt)
                            (posn-set-point evpt))
                       (key-binding key t)))
                  '(save-excursion (posn-set-point (event-end event))
                                   (key-binding key t)))))

;; ***** Restriction print level and length for help buffer

(entropy/emacs-advice-func-around-for-print-limit
 'describe-variable
 nil 100
 (lambda (&rest _)
   t))

;; ***** Print variable
(defvar entropy/emacs-basic-print-variable-history nil)

(defvar entropy/emacs-basic--desc-current-var nil
  "The current query on variable symbol of `describe-variable'.")

(defvar entropy/emacs-basic--desc-current-var-default-value nil
  "The current query on variable symbol's `default-value' of
`describe-variable'.")

(defun entropy/emacs-basic-print-variable-core-func
    (var &optional depth)
  "Print VAR's value recursively according to its type in
`current-buffer'"
  (unless depth
    (setq depth 0))
  (let* ((insert-func
          '(lambda (obj)
             (insert (format "%s" obj))))
         (indent-insert-func
          `(lambda (&optional str offset)
             (or offset (setq offset 0))
             (insert (make-string (+ ,depth offset) ?\ ))
             (when str
               (funcall ',insert-func str))))
         (insert-newline
          (lambda (&optional times)
            (unless (save-match-data (looking-at "^$"))
              (unless times
                (setq times 1))
              (dotimes (_ times)
                (insert "\n"))))))
    (cond
     ((stringp var)
      `(string
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func)
          (funcall (if (> ,depth 0)
                       'prin1
                     'princ)
                   x (current-buffer)))))
     ((vectorp var)
      `(vector
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func "[\n")
          (mapc
           (lambda (y)
             (funcall
              (plist-get
               (cdr (funcall
                     #'entropy/emacs-basic-print-variable-core-func
                     y (1+ ,depth)))
               :print-func)
              y)
             (funcall ',insert-newline))
           x)
          (funcall ',indent-insert-func "]"))))
     ((listp var)
      `(list
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (if (null x)
              (funcall ',indent-insert-func "nil")
            (funcall ',indent-insert-func "(\n")
            (entropy/emacs-list-mapc
             (lambda (y)
               (funcall
                (plist-get
                 (cdr (funcall
                       #'entropy/emacs-basic-print-variable-core-func
                       y (1+ ,depth)))
                 :print-func)
                y)
               (insert "\n"))
             x)
            (funcall ',indent-insert-func ")")))))
     ((hash-table-p var)
      `(hash
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func "#<hash-table\n")
          (maphash
           (lambda (yk yv)
             (funcall ',insert-newline)
             (funcall ',indent-insert-func ":key\n" 1)
             (funcall ',indent-insert-func (pp-to-string yk) 1)
             (funcall ',insert-newline)
             (funcall ',indent-insert-func ":val\n" 1)
             (funcall
              (plist-get
               (cdr (funcall
                     #'entropy/emacs-basic-print-variable-core-func
                     yv (1+ ,depth)))
               :print-func)
              yv))
           x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func ">"))))
     ((cl-struct-p var)
      `(cl-struct
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func "#s(")
          (let* ((class (cl-find-class (type-of x)))
                 (slots (cl--struct-class-slots class))

                 (len (length slots)))
            (funcall ',insert-func (cl--struct-class-name class))
            (funcall ',insert-newline)
            (dotimes (i len)
              (let ((slot (aref slots i))
                    (val (aref x (1+ i))))
                (funcall ',indent-insert-func ":" 1)
                (funcall ',insert-func (cl--slot-descriptor-name slot))
                (funcall ',insert-newline)
                (funcall
                 (plist-get
                  (cdr (funcall
                        #'entropy/emacs-basic-print-variable-core-func
                        val (1+ ,depth)))
                  :print-func)
                 val)
                (funcall ',insert-newline))))
          (funcall ',indent-insert-func ")"))))
     (t
      `(unknown
        :print-func
        (lambda (x)
          (funcall ',insert-newline)
          (funcall ',indent-insert-func (pp-to-string x))
          ))))))

(defun entropy/emacs-basic-print-variable (var-sym)
  "Print a variable into a transient buffer and popup to display
it with focus on.

Show the `default-value' while non-nil `current-prefix-arg' is
detected or using `buffer-local-value' with buffer where invoked
from."
  (interactive
   (list
    (let ((form-str
           (completing-read "Chosen variable symbol or evaluate a form: "
                            obarray
                            (lambda (symbol)
                              (or (and (boundp symbol)
                                       (not (keywordp symbol)))
                                  (get symbol 'variable-documentation)))
                            nil
                            nil
                            'entropy/emacs-basic-print-variable-history)))
      (read form-str))))
  (let* ((orig-buffer-temp-p nil)
         (help-mode-p (and (eq major-mode 'help-mode)
                           (eq (car help-xref-stack-item) 'describe-variable)))
         (orig-buffer
          (if current-prefix-arg
              (prog1
                  (generate-new-buffer "*temp*" t)
                (setq orig-buffer-temp-p t))
            (let (rtn)
              (if help-mode-p
                  (setq rtn (nth 2 help-xref-stack-item))
                (setq rtn (current-buffer)))
              (unless (and rtn (bufferp rtn) (buffer-live-p rtn))
                (user-error "Orig buffer <%s> invalid" rtn))
              rtn)))
         (print-buffer (get-buffer-create "*eemacs-minor-tools/print-var*"))
         (inhibit-read-only t)
         (variable
          (with-current-buffer orig-buffer
            (prog1
                (cond ((symbolp var-sym)
                       (if orig-buffer-temp-p
                           (if help-mode-p
                               ;; FIXME: why we can not get the
                               ;; correct default value of variable
                               ;; such as `post-command-hook' in the
                               ;; *help* buffer when using the map
                               ;; binding command
                               ;; `entropy/emacs-basic--desc-var-preserve-var'?
                               ;; (so as on this bug we get the
                               ;; default value before invoke it.)
                               entropy/emacs-basic--desc-current-var-default-value
                             (default-value var-sym))
                         (symbol-value var-sym)))
                      (t
                       (condition-case err
                           (entropy/emacs-eval-with-lexical var-sym)
                         (error (user-error "fatal: %s" err)))))
              (when orig-buffer-temp-p
                (kill-buffer orig-buffer)))))
         (variable-type-get-func 'entropy/emacs-basic-print-variable-core-func)
         (print-level nil)
         (print-length nil)
         print-window)

    (with-current-buffer print-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (fundamental-mode)
      (entropy/emacs-local-set-key (kbd "q") 'quit-window))
    (display-buffer print-buffer)
    (setq print-window (get-buffer-window print-buffer))
    (select-window print-window)

    (sit-for 0)
    (message "print value of `%S' ..." var-sym)
    (sit-for 0)

    (with-current-buffer print-buffer
      (entropy/emacs-unwind-protect-unless-success
          (let ((var-type-obj
                 (funcall variable-type-get-func variable)))
            (insert (format ";; ========== print for [%s] type %s '%S' %s ==========\n"
                            (car var-type-obj)
                            (if (symbolp var-sym) "variable" "value of expression")
                            var-sym
                            (if (and orig-buffer-temp-p (symbolp var-sym))
                                "with default value"
                              (format "in buffer local <%s>" orig-buffer))))
            (funcall (plist-get (cdr var-type-obj) :print-func)
                     variable))
        (when (yes-or-no-p "Abort and droped the already printed contents?")
          (delete-window print-window)
          (kill-buffer print-buffer))
        (user-error "Abort!"))

      ;; truncate column while long line detected prevents lagging and
      ;; freezing and restore theh overflow content to button help
      ;; echo
      (goto-char (point-min))
      (let ((win-len (window-width print-window)))
        (while (not (eobp))
          (let* ((lbegpt (line-beginning-position))
                 (lendpt (line-end-position))
                 (line-width (- lendpt lbegpt))
                 (over-contents "")
                 (lwrenpt nil))
            (when (> line-width entropy/emacs-unreadable-buffer-so-long-threshold)
              (goto-char (+ (1- (point)) win-len))
              (setq lwrenpt (- (point) 3))
              (setq over-contents
                    (buffer-substring lwrenpt
                                      (line-end-position)))
              (replace-region-contents
               lwrenpt
               (line-end-position)
               (lambda (&rest _) ""))
              (goto-char lwrenpt)
              (insert-button
               "..."
               'action
               `(lambda (&rest _)
                  (print ,over-contents))
               'help-echo
               (concat "mouse-2, RET: "
                       "Follow this link")
               'follow-link t))
            (forward-line 1))))
      (lisp-data-mode)
      (set-buffer-modified-p nil)
      (entropy/emacs-local-set-key (kbd "q") 'quit-window)
      (setq buffer-read-only t)
      (goto-char (point-min)))

    (message "print value of `%S' done (type 'q' to quit window)"
             var-sym)
    ))

(defun entropy/emacs-basic--desc-var-preserve-var
    (orig-func &rest orig-args)
  (let ((var (car orig-args)))
    (setq entropy/emacs-basic--desc-current-var
          var
          entropy/emacs-basic--desc-current-var-default-value
          (default-value var))
    (apply orig-func orig-args)))

(advice-add 'describe-variable
            :around
            #'entropy/emacs-basic--desc-var-preserve-var)

(defun entropy/emacs-basic-describe-variable-print-var ()
  "Using `entropy/emacs-basic-print-variable' to show the current
described variable."
  (interactive nil help-mode)
  (entropy/emacs-basic-print-variable
   (if (and (eq major-mode 'help-mode)
            (eq (car help-xref-stack-item) 'describe-variable))
       entropy/emacs-basic--desc-current-var
     (user-error "Not in an `describe-variable' help buffer"))))

(entropy/emacs-lazy-initial-advice-after
 '(help-mode)
 "help-print-var-bind-init" "help-print-var-bind-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (define-key help-mode-map
   (kbd "p")
   #'entropy/emacs-basic-describe-variable-print-var))

;; ***** Lagging prompts

(defun entropy/emacs-basic--help-doc-lagging-prompt (&rest _)
  (message "Prepare for help documents ...")
  (when (display-graphic-p)
    (redisplay t)))
(advice-add 'describe-variable
            :before
            #'entropy/emacs-basic--help-doc-lagging-prompt)

;; ***** Misc.
(unless (version< emacs-version "28")
  ;; Enable outline view mode for long line keybindings description
  ;; help when emacs version -le emacs 28.
  (setq describe-bindings-outline t))

;; **** Minibuffer Setup

;; Forcely rest plain input method while active minibuffer
(defun entropy/emacs-basic--reset-ime-in-minibuffer
    (&rest _)
  (unless (null current-input-method)
    (set-input-method nil)))
(add-hook 'minibuffer-setup-hook
          #'entropy/emacs-basic--reset-ime-in-minibuffer)

;; **** Misc.
;; ***** Forbidden view-hello-file for W32 platform

;; `view-hello-file' will freeze WINDOWS emacs session, override it!
(when sys/is-win-group
  (defun view-hello-file ()
    "Prompt emacs user do not use view-hello-file in windows
operation system"
    (interactive)
    (message "Do not use view-hello-file in windows because of it will jamm windows and emacs")))

;; ***** Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; ***** Disable some annoying internal-keybindings

;; Disable `view-emacs-FAQ' global keybinding
(global-set-key (kbd "C-h C-f") nil)
(global-set-key (kbd "C-h C-h") nil)

;; ***** Disable messy commands in `M-x' list
(defun entropy/emacs-baisc-general-read-extended-command-predicate
    (command &rest _)
  (if (and (symbolp command)
           (string-match-p
            "^\\(\\*table--\
\\|__\
\\).+$"
            (symbol-name command)))
      nil
    t))
(add-to-list 'entropy/emacs-read-extended-command-predicates
             #'entropy/emacs-baisc-general-read-extended-command-predicate)

;; ***** proper password cache expire time

(setq password-cache-expiry (* 30 60))

;; *** Emacs core patches

;; **** `insert-for-yank' patch

(defun entropy/emacs-insert-for-yank/warning (orig-func &rest orig-args)
  "Like `insert-for-yank' but with eemacs restirictions."
  (let* ((str (car orig-args))
         (str-size (length str))
         (str-size-limit 70)
         (str-newline-p (string-match-p "\n" str))
         (prompt-buffer (get-buffer-create "*eemacs-yank-warning*"))
         prompt-window
         confirm-do-p
         confirm-did-p
         (prompt-kill-func
          (lambda ()
            (when (buffer-live-p prompt-buffer)
              (when (and (windowp prompt-window)
                         (window-live-p prompt-window)
                         (not (eq (window-main-window) prompt-window)))
                (delete-window prompt-window))
              (kill-buffer prompt-buffer)))))
    (cond
     ;; 1) Dired restriction for insertion
     ((and (not (bound-and-true-p buffer-read-only))
           (or (derived-mode-p 'dired-mode)
               (eq major-mode 'wdired-mode)))
      (when (or (> str-size str-size-limit)
                str-newline-p)
        (with-current-buffer prompt-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert str)
            (goto-char (point-min))))
        (pop-to-buffer prompt-buffer)
        (setq prompt-window (get-buffer-window prompt-buffer))
        (unwind-protect
            (progn
              (setq confirm-do-p
                    (yes-or-no-p
                     "It's dangerous to insert this string into current dired buffer, really do?")
                    confirm-did-p t)
              )
          (funcall prompt-kill-func)
          (unless confirm-do-p
            (when confirm-did-p
              (user-error "Abort"))))))
     ;; TODO: add more restrictions
     )
    (apply orig-func orig-args)))
(advice-add 'insert-for-yank :around #'entropy/emacs-insert-for-yank/warning)

;; *** System-wide spec
;; **** Coding environment
(setq system-time-locale "C") ;Use english format time string

;; ***** Default using UTF-8 encoding for basic environment
;; when `entropy/emacs-custom-language-environment-enable' was nil

(unless entropy/emacs-custom-language-environment-enable
  (entropy/emacs-lazy-initial-for-hook
   '(entropy/emacs-after-startup-hook)
   "init-lang-set-for-utf8" "init-lang-set-for-utf8"
   :prompt-type 'prompt-echo
   ;; we must ensure the lang set ran by pdumper session load time
   ;; since te LANG can not be dump.
   :pdumper-no-end nil
   (entropy/emacs-lang-set-utf-8)))

;; ***** Using customized basic encoding system
;; When `entropy/emacs-custom-language-environment-enable' was t

(when (and entropy/emacs-custom-language-environment-enable
           (stringp entropy/emacs-locale-language-environment))
  ;; Customize language environment with user specification
  (entropy/emacs-lazy-initial-for-hook
   '(entropy/emacs-after-startup-hook)
   "init-lang-set-for-user-locale" "init-lang-set-for-user-locale"
   :prompt-type 'prompt-echo
   ;; we must ensure the lang set ran by pdumper session load time
   ;; since te LANG can not be dump.
   :pdumper-no-end nil
   (entropy/emacs-lang-set-local)))

;; ****** Specific cases to forceing using UTF-8 encoding environment

;; ******* Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ******* Force setting specific file type which must be opened with utf-8-unix encoding system.
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

;; ******* let diff-buffer-with-file force run with unicode language environment
(advice-add 'diff-buffer-with-file
            :around #'entropy/emacs-lang-use-utf-8-ces-around-advice)

;; **** internal IME
;; ***** Unified define

(defvar entropy/emacs-basic-intenal-ime-unified-caller-register nil
  "The unified startup caller register for =eemacs-internal-ime=

Each element is formed as an name cons plist of:

#+begin_src #+begin_src emacs-lisp
'(name
  :imestr      ime-registed-name-string
  :enable      enable-var-sym
  :prepare     prepare-func
  :startup     startup-func
  :toggle      toggle-func
  :punctuation (:toggle          punctuation-toggle-func
                :indictor        punctuation-type-indictor)
  :chinese     (:s2t-toggle      s2t-toggles-func
                :s2t-indicator   s2t-type-inidicator))
#+end_src

The =ime-registed-name-string= is the actual name registed to emacs by `register-input-method'.

The =name= is the internal IME name as an symbol.

The =enable-var-sym= is an symbol of a variable which indicate whether
need to start the specfied IME.

The =prepare-func= is an function to be called to prepare the
specified internal IME env which may not be obeyed the
=enable-var-sym='s value.

The =startup-func= is function to start the ime of =name= which obeyed the
=enable-var-sym=.

The =toggle-func= is an function to toggle the non-ime and the specified ime =name=

In =:punctuation= slot, is an plist for:
1) =:toggle=: an function to toggle the status of punctuation status of full/half
2) =:indicator=: an variable symbol whose value is non-nil when using
   full type of punctuation inputs, or an form evaled to return as thus.

In =:chinese= slot, is an plist only specifed for chinese input methodg:
1) =:s2t-toggle=: an function to toggle the simplified or traditional
   chinese inputs.
2) =:s2t-indicator=: an variable symbol whose value is non-nil when
   current using symplified chinese inputs, or an form evaled to return as thus.

")

;; ***** Use chinese pyim
;; ****** extra dependencies
;; ******* librime for pyim
(use-package liberime
  :if (not sys/is-win-group)
  :ensure nil
  :defer (or entropy/emacs-fall-love-with-pdumper entropy/emacs-custom-enable-lazy-load)
  :preface

  (defun entropy/emacs-basic--pyim-set-rime-schema ()
    "Start and set rime input schema to
`entropy/emacs-internal-ime-use-rime-default-schema'"
    (require 'liberime)
    ;; load liberim just needed to require it. Set
    ;; `liberime-auto-build' to t so that we do not get the build
    ;; prompt messeges and auto-build the liberime-core so that we do
    ;; not manually run `liberime-build' here directly.
    (setq liberime-auto-build t)
    (liberime-load)
    (liberime-try-select-schema
     entropy/emacs-internal-ime-use-rime-default-schema)
    t)

  (defun entropy/emacs-basic-pyim-load-rime ()
    "Load `pyim' specified elisp rime binding `liberime' and set the
schema by `entropy/emacs-basic--pyim-set-rime-schema'.

This function will store the `liberime' loading callback to
`entropy/emacs-IME-specs-initialized'(see it for details.)"
    (let (building)
      (if
          (not
           (ignore-errors
             (entropy/emacs-basic--pyim-set-rime-schema)))
          (setq building t)
        (setq entropy/emacs-IME-specs-initialized t))
      (when building
        (warn "You need to build liberime firstly \
when your `entropy/emacs-pyim-use-backend' is 'liberime' \
by run command \"make liberime\" in eemacs root place")
        (setq entropy/emacs-IME-specs-initialized 'liberime-no-build))))

  :init
  (setq liberime-shared-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-scheme-data)
        liberime-user-data-dir
        (expand-file-name entropy/emacs-pyim-liberime-cache-dir)))

;; ******* pyim-basic
(use-package pyim-basedict
  :ensure nil
  :commands (pyim-basedict-enable))

;; ******* simple chinese to traditional chinese
(use-package entropy-s2t
  :ensure nil
  :commands entropy/s2t-string)

;; ****** pyim main
(use-package pyim
  :diminish chinese-pyim-mode
  :commands (pyim-restart-1
             pyim-start
             entropy/emacs-basic-pyim-toggle
             entropy/emacs-basic-toggle-pyim-s2t
             entropy/emacs-basic-toggle-pyim-punctuation-half-or-full
             pyim-convert-string-at-point)

;; ******* preface
  :preface
  (defun entropy/emacs-basic-pyim-start-pyim ()
    "Start `pyim' with specified backend of `entropy/emacs-pyim-use-backend'.

This function will store the loading callback to
`entropy/emacs-IME-specs-initialized'(see it for details.)"
    (require 'pyim)
    (cond ((eq entropy/emacs-pyim-use-backend 'internal)
           (if entropy/emacs-pyim-dicts
               (setq pyim-dicts entropy/emacs-pyim-dicts)
             (pyim-basedict-enable))
           (setq entropy/emacs-IME-specs-initialized t))
          ((and (eq entropy/emacs-pyim-use-backend 'liberime)
                (not sys/win32p))
           (require 'pyim-liberime)          ;needed for load liberime for pyim
           (entropy/emacs-basic-pyim-load-rime))
          (t
           (error "Invalid `entropy/emacs-pyim-use-backend' value '%s'"
                  entropy/emacs-pyim-use-backend)))

    ;; init pyim at temp buffer for preventing polluting
    ;; current-input-method in current buffer.
    (with-temp-buffer
      (set-input-method "pyim")))

;; ******* init
  :init

  (defun entropy/emacs-basic--pyim-prepare ()
    "Prepare env for `pyim'"
    ;;  Setting pyim as the default input method
    (when entropy/emacs-enable-pyim
      (setq default-input-method "pyim"))
    ;;  pyim backend chosen
    (cl-case entropy/emacs-pyim-use-backend
      (internal (setq pyim-default-scheme 'quanpin))
      (liberime (setq pyim-default-scheme 'rime-quanpin)))
    ;;  use popup or posframe for pyim tooltip show
    (setq pyim-page-tooltip entropy/emacs-pyim-tooltip)
    (add-variable-watcher
     'entropy/emacs-pyim-tooltip
     (lambda  (symbol newval operation _where)
       (when (eq operation 'set)
         (unless (eq newval (symbol-value symbol))
           (setq pyim-page-tooltip newval)))))

    ;; customized dcache directory
    (setq pyim-dcache-directory entropy/emacs-pyim-dcache-host-path)
    ;; 5 candidates shown for pyim tooltip
    (setq pyim-page-length 8))

  (add-to-list 'entropy/emacs-basic-intenal-ime-unified-caller-register
               '(pyim
                 :imestr "pyim"
                 :enable entropy/emacs-enable-pyim
                 :prepare entropy/emacs-basic--pyim-prepare
                 :startup entropy/emacs-basic-pyim-start-pyim
                 :toggle entropy/emacs-basic-pyim-toggle
                 :punctuation (:toggle
                               entropy/emacs-basic-toggle-pyim-punctuation-half-or-full
                               :indicator
                               (eq (car pyim-punctuation-translate-p) 'yes))
                 :chinese (:s2t-toggle
                           entropy/emacs-basic-toggle-pyim-s2t
                           :s2t-indicator
                           (eq pyim-magic-converter 'entropy/s2t-string))))

;; ******* config
  :config
;; ******** core advice

;; ********* patch for `pyim-indicator-daemon-function'
  ;; FIXME
  ;; EEMACS_BUG
  ;; EEMACS_TEMPORALLY_HACK
  ;; EEMACS_MAINTENANCE: follow upstream
  (defun __ya/pyim-indicator-daemon-function ()
    "Like `pyim-indicator-daemon' but remove the `redisplay'
procedure since it may cause emacs `font-lock-mode' pause for the
current displayed buffer area wile
`pyim-indicator-use-post-command-hook' is set and WHY?"
    (while-no-input
      ;;(redisplay) <---- disable this
      (ignore-errors
        (let ((chinese-input-p
               (and (functionp pyim-indicator-daemon-function-argument)
                    (funcall pyim-indicator-daemon-function-argument))))
          (dolist (indicator pyim-indicator-list)
            (when (functionp indicator)
              (funcall indicator current-input-method chinese-input-p)))))))

  (advice-add 'pyim-indicator-daemon-function :override
              #'__ya/pyim-indicator-daemon-function)

;; ******** toggle input method
  (defun entropy/emacs-basic-pyim-toggle (&optional type)
    (interactive)
    "The eemacs pyim ime toggler which obey the
`entropy/emacs-internal-IME-toggle-function' api."
    (condition-case error
        (let (disable-p)
          (cond ((and (or (string= current-input-method "pyim")
                          (eq type 'disable))
                      (not (eq type 'enable))
                      (setq disable-p t))
                 (set-input-method nil))
                ((and (or (eq type 'enable)
                          (null type)))
                 (set-input-method "pyim")
                 (setq pyim-punctuation-escape-list nil))
                (t
                 (error "invalid ime toggle type '%s'" type)))
          (if type
              t
            (if disable-p
                nil
              t)))
      (error
       (let ((error-msg (format "%s" error)))
         (message "%s" error-msg)
         error-msg))))

;; ******** using 'C-g' to cancling any pyim manipulation
  (if (not (version< emacs-version "26"))
      (define-key pyim-mode-map (kbd "C-g") 'pyim-quit-clear))

;; ******** s2t&t2s convertor
  (defun entropy/emacs-basic-toggle-pyim-s2t ()
    (interactive)
    (if pyim-magic-converter
        (progn
          (setq pyim-magic-converter nil)
          (message "【简体中文】"))
      (setq pyim-magic-converter 'entropy/s2t-string)
      (message "【繁体中文】")))
  ;; Quickly change 简体 <-> 繁体
  (define-key pyim-mode-map (kbd "C-r") #'entropy/emacs-basic-toggle-pyim-s2t)

;; ******** toglle punctuation between half and full way.
  (defun entropy/emacs-basic-toggle-pyim-punctuation-half-or-full ()
    (interactive)
    (if (or (eq (car pyim-punctuation-translate-p) 'no)
            (eq (car pyim-punctuation-translate-p) 'auto))
        (setq pyim-punctuation-translate-p '(yes no auto))
      (setq pyim-punctuation-translate-p '(no yes auto)))))

;; ***** Use emacs rime

(use-package rime
  :eemacs-functions (rime-activate
                     entropy/emacs-basic-emacs-rime-s2t-toggle
                     entropy/emacs-basic-emacs-rime-CNschema-is-simplified)
  :ensure nil
;; ****** preface
  :preface

  (defun entropy/emacs-basic--emacs-rime-set-posframe-parameter ()
    (let* ((rime-buff (ignore-errors (get-buffer rime-posframe-buffer)))
           (rime-posframe (and rime-buff
                               (posframe--find-existing-posframe
                                rime-buff)))
           ;; repos
           )
      (when (and rime-posframe (frame-live-p rime-posframe))
        (posframe-delete-frame rime-buff)
        ;; FIXME: find a way to reopen the rime posframe
        ;; (setq repos t)
        )
      (setq rime-posframe-properties
            `(:internal-border-width
              10
              :font
              ,(frame-parameter (selected-frame) 'font)))))

  (defun entropy/emacs-basic-emacs-rime-start ()
    "Load `rime' and set the schema by
`entropy/emacs-basic--pyim-set-rime-schema'.

This function will store the `rime' loading callback to
`entropy/emacs-IME-specs-initialized'(see it for details.)"
    (unless entropy/emacs-IME-specs-initialized
      (require 'rime)
      (let ((build-func (lambda ()
                          (let ((env (rime--build-compile-env))
                                (process-environment (copy-sequence process-environment))
                                (default-directory (entropy/emacs-return-as-default-directory
                                                    rime--root)))
                            (cl-loop for pair in env
                                     when pair
                                     do (push pair process-environment))
                            (if (zerop (shell-command "make lib"))
                                (prog1 t
                                  (message "Compile succeed!"))
                              (entropy/emacs-message-do-warn
                               "Compile Rime dynamic module failed")
                              'emacs-rime-build-fatal)))))
        (if (file-exists-p rime--module-path)
            (setq entropy/emacs-IME-specs-initialized
                  t)
          (setq entropy/emacs-IME-specs-initialized
                (funcall build-func)))
        (when (eq entropy/emacs-IME-specs-initialized t)
          (with-temp-buffer
            (entropy/emacs-basic--emacs-rime-set-posframe-parameter)
            (add-hook 'entropy/emacs-font-set-end-hook
                      #'entropy/emacs-basic--emacs-rime-set-posframe-parameter)
            (rime-activate nil)
            (rime-lib-select-schema entropy/emacs-internal-ime-use-rime-default-schema)
            (setq default-input-method "rime"))))))

  (defun entropy/emacs-basic-emacs-rime-toggle (&optional type)
    (interactive)
    "The eemacs emacs-rime ime toggler which obey the
`entropy/emacs-internal-IME-toggle-function' api."
    (condition-case error
        (let (disable-p)
          (cond ((and (or (string= current-input-method "rime")
                          (eq type 'disable))
                      (not (eq type 'enable))
                      (setq disable-p t))
                 (set-input-method nil))
                ((and (or (eq type 'enable)
                          (null type)))
                 (set-input-method "rime")
                 (setq pyim-punctuation-escape-list nil))
                (t
                 (error "invalid ime toggle type '%s'" type)))
          (if type
              t
            (if disable-p
                nil
              t)))
      (error
       (let ((error-msg (format "%s" error)))
         (message "%s" error-msg)
         error-msg))))

;; ****** init
  :init

  (defun __eemacs-rime-poptype-translate (type)
    "Translate popup type of `entropy/emacs-emacs-rime-tooltip' to
`rime-show-candidate' defination with eemacs spec."
    (if (eq type 'minibuffer)
        ;; NOTE&FIXME: we translate the 'minibuffre' type to 'message'
        ;; since the previous one may not work properly when the echo
        ;; area has content.(why?)
        'message
      type))

  (defun entropy/emacs-basic--emacs-rime-prepare ()
    "Prepare env for `emacs-rime'"
    (when entropy/emacs-enable-emacs-rime
      (setq default-input-method "rime"))
    (setq rime-user-data-dir
          entropy/emacs-internal-ime-rime-user-data-host-path
          rime-show-candidate
          (__eemacs-rime-poptype-translate
           entropy/emacs-emacs-rime-tooltip))

    (add-variable-watcher
     'entropy/emacs-emacs-rime-tooltip
     (lambda  (symbol newval operation _where)
       (when (eq operation 'set)
         (unless (eq newval (symbol-value symbol))
           (setq rime-show-candidate
                 (__eemacs-rime-poptype-translate
                  newval)))))))

  (add-to-list 'entropy/emacs-basic-intenal-ime-unified-caller-register
               '(emacs-rime
                 :imestr "rime"
                 :enable entropy/emacs-enable-emacs-rime
                 :prepare entropy/emacs-basic--emacs-rime-prepare
                 :startup entropy/emacs-basic-emacs-rime-start
                 :toggle entropy/emacs-basic-emacs-rime-toggle
                 :punctuation (:toggle
                               ;; TODO:
                               #'(lambda () (interactive) t)
                               :indicator
                               ;; TODO:
                               nil)
                 :chinese (:s2t-toggle
                           entropy/emacs-basic-emacs-rime-s2t-toggle
                           :s2t-indicator
                           (not
                            (funcall
                             'entropy/emacs-basic-emacs-rime-CNschema-is-simplified)))))

;; ****** config
  :config

  (add-to-list 'entropy/emacs-solaire-mode-extra-buffer-filters
               #'(lambda (buff)
                   (string-match-p
                    (regexp-quote
                     rime-posframe-buffer)
                    (buffer-name buff))))

;; ******* core advice
  (defvar __emacs-rime-current-schema nil
    "The current `rime' schema selected.")

  (defun __ya/rime-lib-select-schema (orig-func &rest orig-args)
    "Like `rime-lib-select-schema' but memory its schema selection
stored in `__emacs-rime-current-schema'."
    (let ((schema (car orig-args)))
      (prog1
          (apply orig-func orig-args)
        (setq __emacs-rime-current-schema schema))))
  (advice-add 'rime-lib-select-schema
              :around #'__ya/rime-lib-select-schema)

;; ******* candi show patch

  (defun __rime--message-content-sit-for (secs)
    "Wrapper for using `sit-for' inside of
`rime--message-display-content' for the sake of fix display hints
conflicts."
    (if (memq last-input-event
              `(
                ;; EEMACS_TEMPORALLY_HACK: the `sit-for' will block
                ;; the modline status refresh when toggle input
                ;; method. And FIXME: why it will also block the
                ;; following rime candi show with message?
                ,(car (listify-key-sequence
                       (kbd entropy/emacs-internal-ime-toggling-kbd-key)))))
        (run-with-idle-timer
         0 nil
         'sit-for secs)
      (sit-for secs)))
  (defun __ya/rime--message-display-content (content)
    "Like `rime--message-display-content', Fix bugs."
    (let ((message-log-max nil))
      (if
          ;; Idle `sit-for' while empty candi msg responsed on since
          ;; there's also no point pre-post display while the candi
          ;; msg is empty which indicate it's an non-mapped key for
          ;; the rime engine.
          (and (string-empty-p content)
               ;; FIXME & NOTE: we should stick this patch only when
               ;; `rime-mode' enabled, if not will cause the imcoming
               ;; `message' lost and why?
               rime-mode)
          (run-with-idle-timer
           0.1
           nil
           #'(lambda (msg)
               (let ((message-log-max nil))
                 (save-window-excursion
                   (with-temp-message msg
                     (__rime--message-content-sit-for most-positive-fixnum)))))
           content)
        (save-window-excursion
          (with-temp-message content
            (__rime--message-content-sit-for most-positive-fixnum))))))
  (advice-add 'rime--message-display-content
              :override #'__ya/rime--message-display-content)

;; ******* eemacs spec apis
  ;; FIXME:
  ;; TODO: we must use an unified wrapper to judge the result since
  ;; rime support various chinese ime further than just 'luna_pyinyin'
  ;; like 'cangjie' 'zhuyin' etc.
  (defun __emacs-rime-is-use-cn-schema-p ()
    (string-match-p "luna_pinyin" __emacs-rime-current-schema))
  (defun entropy/emacs-basic-emacs-rime-CNschema-is-simplified ()
    (string-match-p "luna_pinyin_simp" __emacs-rime-current-schema))
  (defun entropy/emacs-basic-emacs-rime-s2t-toggle ()
    (interactive)
    (when (__emacs-rime-is-use-cn-schema-p)
      (if (entropy/emacs-basic-emacs-rime-CNschema-is-simplified)
          (prog1
              (rime-lib-select-schema "luna_pinyin")
            (message "【繁体中文】"))
        (message "【简体中文】")
        (rime-lib-select-schema "luna_pinyin_simp"))))
  ;; Quickly change 简体 <-> 繁体
  (define-key rime-active-mode-map (kbd "C-r") #'entropy/emacs-basic-emacs-rime-s2t-toggle)
  )

;; ***** _union setup

;; FIXME: set input method to nil because emacs will change
;; `default-input-method''s default value WHY?
(setq default-input-method nil)


;; FIXME: when `ivy-dynamic-exhibit-delay-ms' is enabled, emacs-rime
;; will cause eemacs bug of =h-1c9af04d-403f-4050-a8eb-778fc47ff8de=
(defvar __eemacs-internal-ime-union-registed-name-strings nil)
(setq __eemacs-internal-ime-union-registed-name-strings
      (mapcar
       (lambda (x)
         (plist-get (cdr x) :imestr))
       entropy/emacs-basic-intenal-ime-unified-caller-register))
(add-to-list 'entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-conditions
             #'(lambda ()
                 (member current-input-method
                         __eemacs-internal-ime-union-registed-name-strings)))

(defun entropy/emacs-internal-ime-toggle-popup-type ()
  "Toggle eemacs internal IME candi show type."
  (declare (interactive-only t))
  (interactive)
  (let ((chosen (completing-read "Choose popup type: "
                                 '("minibuffer"
                                   "popup"
                                   "posframe"
                                   )
                                 nil t)))
    (setq entropy/emacs-internal-ime-popup-type (intern chosen))
    (entropy/emacs-internal-ime-popup-type-autoset)))

(defun entropy/emacs-internal-ime-starter (&optional no-error)
  "The =eemacs-intenal-IME= startup caller as the union wrapper for
any of the `entropy/emacs-internal-ime-use-backend'.

Throw an error while any core init procedure startup with
fatal (i.e. `entropy/emacs-IME-specs-initialized' is get as
whatever but 't') when optional argument NO-ERROR is unset.

This function does nothing when
`entropy/emacs-IME-specs-initialized' is t as already yet since
we do not want to init as duplicated which will cause messy."
  (interactive)
  (unless entropy/emacs-IME-specs-initialized
    (let ((core-init-juger
           (lambda ()
             (if (eq entropy/emacs-IME-specs-initialized t)
                 t
               (unless no-error
                 (error "Enable eemacs internal IME spec [%s] with fatal of [%s]"
                        entropy/emacs-internal-ime-use-backend
                        entropy/emacs-IME-specs-initialized)))))
          non-startup
          register ime-enable ime-name ime-plist
          hydra-heads hydra-group)

      ;; Set `entropy/emacs-internal-ime-use-backend' when not enabled at eemacs startup time
      (unless entropy/emacs-internal-ime-use-backend
        (setq non-startup t)
        (setq entropy/emacs-internal-ime-use-backend
              (intern (completing-read "choose =eemacs-intenal-IME= backend"
                                       '("pyim" "emacs-rime")
                                       nil t))))
      (setq register (assoc entropy/emacs-internal-ime-use-backend
                            entropy/emacs-basic-intenal-ime-unified-caller-register)
            ime-name (car register)
            ime-plist (cdr register)
            ime-enable (or (symbol-value (plist-get ime-plist :enable))
                           (when non-startup
                             (set (plist-get ime-plist :enable)
                                  t))))
      (when ime-enable
        ;; auto reset the popup type while user messy setting
        (entropy/emacs-internal-ime-popup-type-autoset)
        ;; also reset popup type while emacs daemon type change
        (entropy/emacs-with-daemon-make-frame-done
         'eemacs-internal-ime-popuptype-autoreeset
         nil nil
         '(entropy/emacs-internal-ime-popup-type-autoset))

        ;; union function set
        (setq entropy/emacs-internal-IME-toggle-function
              (plist-get ime-plist :toggle))

        ;; union hydra hollow set
        (setq hydra-heads
              `((,entropy/emacs-internal-ime-toggling-kbd-key
                 ,entropy/emacs-internal-IME-toggle-function
                 ,(format "Set Inputmethod '%s'" ime-name)
                 :enable t
                 :toggle (string= current-input-method ,(plist-get ime-plist :imestr))
                 :global-bind t)
                ,(when (plist-get ime-plist :chinese)
                   (list
                    "c t" (plist-get (plist-get ime-plist :chinese) :s2t-toggle)
                    (format "'%s' use traditional chinese (quckly use'C-r' within input)"
                            ime-name)
                    :enable t
                    :toggle (plist-get (plist-get ime-plist :chinese) :s2t-indicator)))
                ("c f" ,(plist-get (plist-get ime-plist :punctuation) :toggle)
                 ,(format "'%s' toggle punct full/half" ime-name)
                 :enable t
                 :toggle ,(plist-get (plist-get ime-plist :punctuation) :indicator))
                ("c p" entropy/emacs-internal-ime-toggle-popup-type
                 "Change =eemacs-intenal-IME= popup type"
                 :enable t :exit t))
              hydra-group
              (list "IME" (delete nil hydra-heads)))

        (funcall (plist-get ime-plist :prepare))
        (funcall (plist-get ime-plist :startup))
        (when (funcall core-init-juger)
          (entropy/emacs-hydra-hollow-add-for-top-dispatch
           hydra-group))))))

(entropy/emacs-lazy-initial-for-hook
 '(entropy/emacs-hydra-hollow-call-before-hook)
 "eemacs-IMEspec-hydra-hollow-init" "eemacs-IMEspec-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("IME"
    (("c c" entropy/emacs-internal-ime-starter
      "Enable eemacs internal IME spec"
      :enable t
      :toggle entropy/emacs-IME-specs-initialized
      :exit t)))))

;; **** Emacs process and system proced manager hacking
;; ***** process

(entropy/emacs-lazy-initial-advice-after
 '(process-menu-mode)
 "process-menu-mode-hydra-hollow-init" "process-menu-mode-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
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
      :enable t :exit t :map-inject t)))))

;; ***** proced
(use-package proced
  :ensure nil
  :commands (proced-process-attributes)
;; ****** preface
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
           (let ((default-directory (entropy/emacs-return-as-default-directory
                                     temporary-file-directory)))
             (w32-shell-execute
              "open" $executable)
             (message (format "Start with '%s'."
                              $executable)))))
        (t (message
            "`entropy/emacs-basic-proced-auto-startwith' are just used in w32 platform")))))

;; ****** hydra hollow
  :eemacs-mmphc
  (((:enable t :defer t)
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

;; ****** hook
  :hook
  (proced-mode . hl-line-mode)

;; ****** init
  :init
  (setq-default proced-format 'medium)
  (entropy/emacs-lazy-with-load-trail
    'auto-start-exec
    (when sys/win32p
      (dolist (el entropy/emacs-startwith-apps)
        (when (executable-find (cdr el))
          (entropy/emacs-basic-proced-auto-startwith
           (car el) (cdr el))))))

;; ****** config
  :config

;; ******* size unit indicator patch
  (defface __proced-size-gb-unit-face
    '((t (:foreground "Red"))) "")
  (defface __proced-size-mb-unit-face
    '((t (:foreground "yellow"))) "")
  (defface __proced-size-kb-unit-face
    '((t (:foreground "green"))) "")
  (defun __proced-size-kb-to-dwim-human-readable (size)
    (let* ((gb (/ size (expt 1024.0 2)))
           (mb (/ size 1024.0))
           (mbp (>= mb 1))
           (gbp (>= gb 1)))
      (cond
       (gbp
        (format "%.2f%s" (/ size (expt 1024.0 2))
                (propertize "G" 'face
                            '__proced-size-gb-unit-face)))
       (mbp
        (format "%.2f%s" (/ size 1024.0)
                (propertize "M" 'face
                            '__proced-size-mb-unit-face)))
       (t
        (format "%d%s" size
                (propertize "K" 'face
                            '__proced-size-kb-unit-face))))))

  (dolist (size-ptr '(vsize rss))
    (when (cadr (alist-get size-ptr proced-grammar-alist))
      (setf (cadr (alist-get size-ptr proced-grammar-alist))
            '__proced-size-kb-to-dwim-human-readable)))

;; ****** end
  )

;; ** Eemacs basic hydra-hollow instances

(entropy/emacs-lazy-initial-for-hook
 '(entropy/emacs-after-startup-hook)
 "eemacs-basic-core-hydra-hollow-init"
 "eemacs-basic-core-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'eemacs-basic-config-core nil
  '("Eemacs Basic Core"
    (("f f"
      (:pretty-hydra-cabinet
       (:data
        "Window split/delete"
        (("C-x 1" delete-other-windows
          "delete-other-window"
          :enable t :exit t :global-bind t)
         ("C-x 2" entropy/emacs-no-same-buffer-split-window-vertically
          "delete-other-window"
          :enable t :exit t :global-bind t)
         ("C-x 3" entropy/emacs-no-same-buffer-split-window-horizontally
          "delete-other-window"
          :enable t :exit t :global-bind t))
        "Frequently used Misc. commands"
        (("<f2>" entropy/emacs-basic-dhl-toggle "hl line"
          :enable t
          :exit t
          :global-bind t)
         ("<f6>" entropy/emacs-ui-loop-alpha-selected-frame
          "Frame Alpha"
          :enable t
          :toggle entropy/emacs-ui--loop-alpha-selected-frame-is-did
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
      :enable t :exit t)))))

;; * provide
(provide 'entropy-emacs-basic)
