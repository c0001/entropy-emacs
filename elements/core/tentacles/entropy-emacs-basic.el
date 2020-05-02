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
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defface)
(require 'entropy-emacs-utils)
(require 'entropy-emacs-hydra-hollow)

(when (and entropy/emacs-indicate-sshd-session
           (display-graphic-p))
  (setq entropy/emacs-indicate-sshd-session nil))

(entropy/emacs-lazy-with-load-trail
 top-keybinding
 (define-key (current-global-map)
   (kbd entropy/emacs-top-key)
  entropy/emacs-top-keymap))

;; ** Basic major-modes spec
;; *** Dired config
;; **** dired basic
(use-package dired
  :ensure nil

;; ***** pretty-hydra
  :eemacs-mmphc
  (((:enable t)
    (dired-mode dired dired-mode-map t (3 3 3)))
   ("Basic"
    (("RET" dired-find-file "Open item dwim"
      :enable t
      :exit t
      :map-inject t)
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
    "Misc."
    (("0" entropy/emacs-basic-get-dired-fpath "Get Node Path"
      :enable t
      :map-inject t
      :exit t)
     ("M-l" entropy/emacs-basic--dired-add-to-load-path "Add path"
      :enable t
      :map-inject t
      :exit t))))

;; ***** init
  :init
;; ****** Delete directory with force actions
  (setq entropy/emacs-basic--dired-delete-file-mode-map (make-sparse-keymap))

  ;; TODO : comlete `entropy/emacs-basic--dired-delete-file-mode'
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

;; ***** config
  :config
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

  (defun entropy/emacs-basic--dired-revert-advice (&rest _)
    (revert-buffer))
  (dolist (el '(dired-do-rename
                dired-do-rename-regexp
                dired-do-copy
                dired-do-copy-regexp
                dired-do-compress
                dired-do-compress-to
                dired-do-touch))
    (advice-add el :after #'entropy/emacs-basic--dired-revert-advice))
  )

;; **** Use dired-aux to enable dired-isearch
(entropy/emacs-lazy-load-simple dired
  (require 'dired-aux))

;; **** Quick sort dired buffers via hydra
  ;;; bind key: `S'
(when (not sys/win32p)
  (use-package dired-quick-sort
    :if (or (executable-find "gls") (executable-find "ls"))
    :commands (dired-quick-sort-setup)
    :init (add-hook 'dired-mode-hook 'dired-quick-sort-setup)))

;; **** Use coloful dired ls

(use-package dired-rainbow
  :commands
  (dired-rainbow-define dired-rainbow-define-chmod)
  :init
  (entropy/emacs-lazy-load-simple dired
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
  :init
  (entropy/emacs-lazy-with-load-trail
   diredfl-colorful-ini
   (diredfl-global-mode 1)))

(use-package all-the-icons-dired
  :if
  (and (display-graphic-p)
       (entropy/emacs-icons-displayable-p))
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode)
  :init
  (when sys/win32p
    (require 'font-lock+))
  :config
  (with-no-warnings
    (defun entropy/emacs-basic--all-the-icons-dired-refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons it too many items
      (if (<= (count-lines (point-min) (point-max)) 300)
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)

            (goto-char (point-min))
            (while (not (eobp))
              (let ((file (dired-get-filename 'verbatim t)))
                (when file
                  (let ((icon (if (file-directory-p file)
                                  (all-the-icons-icon-for-dir file nil "")
                                (all-the-icons-icon-for-file
                                 file
                                 :v-adjust all-the-icons-dired-v-adjust))))
                    (if (member file '("." ".."))
                        (all-the-icons-dired--add-overlay (point) " \t")
                      (all-the-icons-dired--add-overlay (point) (concat icon "\t"))))))
              (dired-next-line 1)))
        (message "Not display icons because of too many items.")))
    (advice-add #'all-the-icons-dired--refresh
                :override #'entropy/emacs-basic--all-the-icons-dired-refresh)))

;; **** dired-x
(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-size-limit nil)
  (setq dired-omit-extensions nil))

;; **** dired-subtree
;; Org mode like dired subtree fold/expand
(use-package dired-subtree
  :after dired
  :commands
  (dired-subtree-toggle
   dired-subtree-cycle)
  :eemacs-mmphca
  (((:enable t)
    (dired-mode dired dired-mode-map))
   ("Basic"
    (("TAB" dired-subtree-toggle
      "Insert subtree at point or remove it if it was not present."
      :enable t :map-inject t :exit t)
     ("<backtab>" dired-subtree-cycle
      "Org-mode like cycle visibilitya"
      :enable t :map-inject t :exit t)))))

;; *** Image-mode
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
(when (>= emacs-major-version 26)
  (progn
    (setq-default display-line-numbers-width-start t)
    (when entropy/emacs-init-display-line-mode
      (global-display-line-numbers-mode))))

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

Filename are \".scratch_entropy\" host in `entropy/emacs-stuffs-topdir'.
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

(entropy/emacs-lazy-with-load-trail
 init-eemamcs-scratch-buffer
 (entropy/emacs-lazy-load-simple entropy-emacs-structure
   (redisplay t)
   (entropy/emacs-basic--scratch-buffer-file-binding)))

;; Create a new scratch buffer
(defun entropy/emacs-basic-create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (entropy/emacs-basic--scratch-buffer-file-binding))
  (lisp-interaction-mode)
  (message "Create *scratch* buffer"))

;; *** Highlight current line
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

;; *** Smooth scrolling

;; Force smooth mouse scroll experience
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

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
        scroll-conservatively (if (equal scroll-conservatively 0) 100000 0))
  (if (and (equal scroll-step 1)
           (equal scroll-conservatively 100000))
      (progn
        (setq entropy/emacs-basic-smooth-scrolling-mode t)
        (message "Smooth scrolling enabled!"))
    (progn
      (setq entropy/emacs-basic-smooth-scrolling-mode nil)
      (message "Smooth scrolling disabled!"))))

(entropy/emacs-basic-smooth-scrolling)

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


;; *** Kill-other-windowss spec

(defun entropy/emacs-basic-kill-other-window ()
  "Delete other window and do again using
`delete-other-windows-internal' if non-effect.

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

;; *** Set defualt tab size
;; Do not use `indent-tabs-mode' by default for compatibility meaning
;; that tabs visualization are not unified accorss editor.

(if entropy/emacs-custom-tab-enable
    (setq-default tab-width entropy/emacs-custom-tab-width)
  (setq-default indent-tabs-mode nil))

;; *** Setting language encoding environment
(setq system-time-locale "C") ;Use english format time string

;; **** Default using UTF-8 encoding for basic environment
;; when `entropy/emacs-custom-language-environment-enable' was nil

(unless (and entropy/emacs-custom-language-environment-enable
             (ignore-errors (stringp entropy/emacs-locale-language-environment)))
  (entropy/emacs-lang-set-utf-8))

;; **** Using customized basic encoding system
;; When `entropy/emacs-custom-language-environment-enable' was t

(when (and entropy/emacs-custom-language-environment-enable
           (ignore-errors (stringp entropy/emacs-locale-language-environment)))
  ;; Customize language environment with user specification
  (entropy/emacs-lang-set-local))

;; setting w32 shell lanuguage environment
(when sys/win32p
  (when entropy/emacs-win-env-lang-enable
    (setenv "LANG" entropy/emacs-win-env-lang-set)))

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

(defvar entropy/emacs-basic-loop-alpha-did nil)

(defun entropy/emacs-basic-loop-alpha (&optional prefix)
  (interactive "P")
  (setq entropy/emacs-basic-loop-alpha-did
        (null entropy/emacs-basic-loop-alpha-did))
  (let ((h (car entropy/emacs-loop-alpha-value))
        (bgtr
         (when prefix
           (string-to-number (read-string "Input bg trransparent var (0-100): ")))))
    (setq bgtr
          (let ()
            (if (not (integerp bgtr))
                (car h)
              (if (> bgtr 95)
                  95
                (if (<= bgtr 0)
                    75
                  bgtr)))))
    (funcall
     (lambda (a ab)
       (let ((alpha-items (mapcar (lambda (x) (when (eq (car x) 'alpha) x))
                                  default-frame-alist)))
         (dolist (el alpha-items)
           (unless (null el)
             (setq default-frame-alist
                   (delete el default-frame-alist)))))
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     bgtr
     (car (cdr h)))
    (setq entropy/emacs-loop-alpha-value
          (cdr (append entropy/emacs-loop-alpha-value
                       (list (list bgtr (nth 1 h))))))))

(when (and entropy/emacs-init-loop-alpha
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
  (setq display-time-format "%e %b %Y %H:%M:%S")
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
  :commands (entropy-grom-mode)
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file push-button find-library-name)
   "entropy-grom"
   "entropy-grom"
   (entropy-grom-mode +1))
  :config
  (add-to-list 'entropy/grom-find-file-except-bfregexp-list
               "treemacs-persist"))

;; *** Revert buffer automatically

(entropy/emacs-lazy-initial-for-hook
 '(find-file-hook)
 "GlbAutoRevertMode"
 "GlbAutoRevertMode-enabled"
 (global-auto-revert-mode +1))

;; *** Popup key stroking prompt
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (entropy/emacs-lazy-with-load-trail which-key (which-key-mode t))
  (setq which-key-popup-type 'side-window))

;; *** Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure nil
  :commands (global-undo-tree-mode undo-tree-visualize)
  :defines (entropy/emacs-basic-undo-tree-margin-detective)
  :hook ((undo-tree-mode . (lambda () (define-key undo-tree-map (kbd "C-x u") nil))))
  :init
  (entropy/emacs-lazy-with-load-trail
   undo-tree-enable
   (global-undo-tree-mode t))

  (if (eq entropy/emacs-align-window-center-with? 'basic)
      (global-set-key (kbd "C-x u") #'entropy/emacs-basic-undo-tree)
    (global-set-key (kbd "C-x u") #'undo-tree-visualize))

  :config
  (when (eq entropy/emacs-align-window-center-with? 'basic)
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
                    (switch-to-buffer parent)
                    (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                                        (/ (window-width) entropy/emacs-window-center-integer)
                                        (/ (window-width) entropy/emacs-window-center-integer)))
                (switch-to-buffer parent))))))))

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


;; *** Clear killring
;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/emacs-basic-clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

;; *** Mark-sexp
(entropy/emacs-!set-key
  ;; Set the default region mark enable key stroke to the double of
  ;; `entropy/emacs-top-key' i.e. double hints that.
  ;;
  ;; This is key-stroke was easy to remeber, greate!
  (kbd entropy/emacs-top-key)
  'set-mark-command)

(defun entropy/emacs-basic-mark-set ()
  "Mark the current point and push it to mark ring so that this
place can be easily found by other interactive command."
  (interactive)
  (save-excursion
    (push-mark)))

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
  (setq recentf-max-saved-items 300)
  (entropy/emacs-lazy-with-load-trail
   recentf-init
   (recentf-mode))
  :config
  (setq recentf-max-saved-items 300
        recentf-exclude
        `("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
          ,(expand-file-name recentf-save-file)
          (lambda (file) (file-in-directory-p file package-user-dir)))))

(use-package savehist
  :ensure nil
  :init (entropy/emacs-lazy-with-load-trail savehist-init (savehist-mode t))
  :config
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)
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
        (funcall mode)
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
  (entropy/emacs-lazy-with-load-trail
   disable-mouse
   (global-disable-mouse-mode t)))

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
  :commands (liberime-load)
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

See [[https://github.com/rime/home/wiki/CustomizationGuide#%E4%B8%80%E4%BE%8B%E5%AE%9A%E8%A3%BD%E7%B0%A1%E5%8C%96%E5%AD%97%E8%BC%B8%E5%87%BA][the rime wiki]] for details.

"
    (liberime-select-schema
     "luna_pinyin_simp"))

  (defun entropy/emacs-basic--pyim-first-build-timer ()
    (setq entropy/emacs-basic-pyim-liberime-load-timer
          (run-with-timer
           2 1
           (lambda ()
             (cancel-timer entropy/emacs-basic-pyim-liberime-load-timer)
             (unless
                 (ignore-errors
                   (entropy/emacs-basic--pyim-set-rime-schema))
               (entropy/emacs-basic--pyim-first-build-timer))))))

  (defun entropy/emacs-basic-pyim-load-rime ()
    ;; load liberim just needed to require it.
    (require 'liberime)
    ;; set liberime schema and check dynamic module status and build
    ;; it when needed.
    (let (building)
      (when
          (not
           (ignore-errors
             (entropy/emacs-basic--pyim-set-rime-schema)))
        (liberime-build)
        (setq building t))
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

  (defvar entropy/emacs-basic-pyim-has-initialized nil)

  (entropy/emacs-hydra-hollow-add-for-top-dispatch
   '("Pyim"
     (("c c" entropy/emacs-basic-pyim-start
       "Enable Pyim"
       :enable t
       :toggle entropy/emacs-basic-pyim-has-initialized
       :exit t))))

  (defun entropy/emacs-basic-pyim-start ()
    (interactive)
    (unless entropy/emacs-basic-pyim-has-initialized
      (require 'pyim)
      (cond ((eq entropy/emacs-pyim-use-backend 'internal)
             (setq pyim-dicts entropy/emacs-pyim-dicts))
            ((and (eq entropy/emacs-pyim-use-backend 'liberime)
                  (not sys/win32p))
             (entropy/emacs-basic-pyim-load-rime))
            (t
             (pyim-basedict-enable)))
      (set-input-method "pyim")

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

      (setq entropy/emacs-basic-pyim-has-initialized t)))

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

;; *** Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; *** Key modification

;; **** key re-mapping
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

;; **** xterm re-bind

;; Rebind "insert" refer key in terminal emacs to support yank&cut
;; communication with GUI.

(entropy/emacs-lazy-with-load-trail
 xterm-rebind
 (entropy/emacs-xterm-external-satisfied-p)
 (define-key global-map [xterm-paste]
   #'entropy/emacs-xterm-paste)

 (entropy/emacs-lazy-load-simple term
   (define-key term-raw-map
     [S-insert]
     #'entropy/emacs-xterm-term-S-insert)
   (define-key term-raw-map
     [xterm-paste]
     #'entropy/emacs-xterm-term-S-insert)))

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
   '(dired-mode-hook find-file-hook)
   "epa-mode" "epa-mode"
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
 'process-menu-mode 'simple process-menu-mode-map t
 '("Basic"
   (("S" tabulated-list-sort "Sort Tabulated List entries by the column at point"
     :enable t :exit t :map-inject t)
    ("d" process-menu-delete-process "Kill process at point in a ‘list-processes’ buffer."
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
    (proced-mode proced proced-mode-map t (2 2 2 2)))
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
 'words-manipulation nil nil
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
        '(push-button load-library find-library)
        "autocompression-mode"
        "autocompression-mode"
        (auto-compression-mode 0)
        (auto-compression-mode 1))))

;; *** Replace follow input in region
;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :init
  (entropy/emacs-lazy-with-load-trail
   delsel-mode-init
   (delete-selection-mode 1)))

;; *** Inhibit gui dialog
(setq use-file-dialog nil
      use-dialog-box nil)

;; *** A minor-mode menu for the mode line
(use-package minions
  :commands (minions-minor-modes-menu)
  :init
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)
  (entropy/emacs-lazy-load-simple doom-modeline
    (add-hook 'doom-modeline-mode-hook
              #'minions-mode)))

;; ** eemacs basic hydra-hollow instances

(entropy/emacs-hydra-hollow-common-individual-hydra-define
 'eemacs-basic-config-core nil nil
 '("Eemacs Basic Core"
   (("C-x 1" entropy/emacs-basic-kill-other-window
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

(entropy/emacs-hydra-hollow-add-for-top-dispatch
 '("Basic"
   (("b m"
     (:eval
      (entropy/emacs-hydra-hollow-category-common-individual-get-caller
       'eemacs-basic-config-core))
     "Core Operations"
     :enable t :exit t))))

;; * provide
(provide 'entropy-emacs-basic)
