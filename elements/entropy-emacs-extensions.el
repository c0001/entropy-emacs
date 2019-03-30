;;; init-extensions --- entropy-emacs submodule extensions init config

;; Copyright (C) 2019-01-07  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This elisp source file was part of entropy-emacs, used to
;; initialize emacs extensions specified with entropy-emacs with git
;; submodule type.
;;
;; The main purpose of this elisp source file do was used to compile
;; and load extensions' doc info file into emacs self-doc system.
;;
;;; Configuration:
;;
;; none

;;; Code:

;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'cl)
;; ** varaibale defination
(defvar entropy/iniext-lost-cmd nil)
(defvar entropy/iniext-submodule-dir
  (expand-file-name "elements/submodules" entropy/iniext-extensions-dir))
(defvar entropy/iniext-lost-extern-prompt-buffer "*entropy/lost-extern*")
(defvar entropy/iniext-subinit-buffer "*entropy/subinit*")
(defvar entropy/iniext-subupdate-buffer "*entropy/subupdate*")
(defvar entropy/iniext-clone-extensions-prompt-buffer "*entropy/iniext-cleb*")
(defvar entropy/iniext-clone-extensions-url "https://github.com/c0001/entropy-emacs-extensions.git")

(defvar entropy/iniext-pre-update-submodules nil)
(defvar entropy/iniext-submodules-with-doc-list
  '("magit/Documentation"
    "swiper/doc"
    "with-editor"
    "w3m/doc"))


(defvar entropy/iniext-deps
  '(("entropy-cn-dict" . "https://github.com/c0001/entropy-cn-dict.git")
    ("entropy-common-library" . "https://github.com/c0001/entropy-common-library.git")
    ("entropy-counsel-prj" . "https://github.com/c0001/entropy-counsel-prj.git")
    ("entropy-counsel-stuffs" . "https://github.com/c0001/entropy-counsel-stuffs.git")
    ("entropy-dired-cp-or-mv" . "https://github.com/c0001/entropy-dired-cp-or-mv.git")
    ("entropy-epub2org" . "https://github.com/c0001/entropy-epub2org.git")
    ("entropy-global-read-only-mode" . "https://github.com/c0001/entropy-global-read-only-mode.git")
    ("entropy-open-with" . "https://github.com/c0001/entropy-open-with.git")
    ("entropy-org-batch-refile" . "https://github.com/c0001/entropy-org-batch-refile.git")
    ("entropy-org-export-theme-toggle" . "https://github.com/c0001/entropy-org-export-theme-toggle.git")
    ("entropy-org-widget" . "https://github.com/c0001/entropy-org-widget.git")
    ("entropy-portableapps" . "https://github.com/c0001/entropy-portableapps.git")
    ("entropy-proxy-mode" . "https://github.com/c0001/entropy-proxy-mode.git")
    ("entropy-proxy-url" . "https://github.com/c0001/entropy-proxy-url.git")
    ("entropy-s2t" . "https://github.com/c0001/entropy-s2t.git")
    ("entropy-sdcv" . "https://github.com/c0001/entropy-sdcv.git")
    ("entropy-unfill" . "https://github.com/c0001/entropy-unfill.git")
    ("entropy-en-words" . "https://github.com/c0001/entropy-en-words.git")
    ("entropy-font-lock-plus" . "https://github.com/c0001/entropy-font-lock-plus.git")))

(defvar entropy/iniext-deps-counter nil)

(defvar entropy/iniext-deps-lossy nil)

(defvar entropy/iniext-deps-lossy-prompt-buffer "*entropy-emacs-deps-lossy*")

(defvar entropy/iniext-deps-lossy-get-buffer "*entropy-emacs-deps-get*")


;; ** preparation
;; *** pre-cmd check
(defun entropy/iniext-check-extern ()
  "Checking entropy-emacs submodule initialization external
dependencies satisfies as.

Non-nil return indicated of satisfaction."
  (unless entropy/iniext-lost-cmd
    (setq entropy/iniext-lost-cmd nil))
  (let ((pre-cmd '("git"))
        (rtn t))
    (dolist (el pre-cmd)
      (unless (executable-find el)
        (push el entropy/iniext-lost-cmd)))
    (when entropy/iniext-lost-cmd
      (setq rtn nil))
    rtn))

(defun entropy/iniext-extern-prompt ()
  (when entropy/iniext-lost-cmd
    (with-current-buffer (get-buffer-create
                          entropy/iniext-lost-extern-prompt-buffer)
      (goto-char (point-min))
      (setq-local buffer-read-only nil)
      (dolist (el entropy/iniext-lost-cmd)
        (insert (format "External \"%s\" command lost!" el) "\n")))

    (switch-to-buffer entropy/iniext-lost-extern-prompt-buffer)
    (delete-other-windows)
    (setq rtn nil)
    (message "External feature unsatisfied!")))


;; ** core library
;; *** common library
(defun entropy/iniext-add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (when (not (string-match-p "yasnippet-snippets" dir))
      (normal-top-level-add-subdirs-to-load-path))))

;; *** submodule refer
;; **** clone extension repo
(defun entropy/iniext-clone-extensions ()
  (let ((pbuffer (get-buffer-create entropy/iniext-clone-extensions-prompt-buffer))
        (cmd (list "git" "clone" entropy/iniext-clone-extensions-url
                   (expand-file-name entropy/iniext-extensions-dir)))
        proc-name)
    (with-current-buffer pbuffer
      (unless (not buffer-read-only)
        (read-only-mode 0))
      (insert "Now cloning entropy-emacs extensions ..."))
    (switch-to-buffer pbuffer)
    (delete-other-windows)
    (setq proc-name
          (make-process
           :name "entropy/iniext-extension-clone"
           :buffer pbuffer 
           :command cmd
           :sentinel 'entropy/iniext-clone-extensions_senital
           :filter 'entropy/iniext-clone-extensions_filter))))


(defun entropy/iniext-clone-extensions_senital (proc cbk)
  (when (string-match "finished$" cbk)
    (bury-buffer entropy/iniext-clone-extensions-prompt-buffer)
    (about-emacs)
    (let ((init-subs (yes-or-no-p "Cloning entropy emacs extensions repo done! init-submodules?")))
      (when init-subs
        (entropy/iniext-extract-submodule)))))

(defun entropy/iniext-clone-extensions_filter (proc cbk)
  (switch-to-buffer (process-buffer proc))
  (display-buffer (process-buffer proc))
  (when (> (length (window-list)) 1)
    (delete-other-windows)))
;; **** submodule status check

(defun entropy/iniext-substatus ()
  "Judging whether entropy-emacs's submodules has been
initialized which Non-nill renturns that has been initialized,
otherwise for as."
  (unless (not entropy/iniext-pre-update-submodules)
    (setq entropy/iniext-pre-update-submodules nil))
  (let* ((subdirs (ignore-errors (directory-files entropy/iniext-submodule-dir)))
         _temp_var rtn)
    (unless subdirs
      (message (format "Now clone entropy-emacs-extenstion repo first!"))
      (setq rtn 'not-clone))
    (when subdirs
      (setq subdirs (delete "." subdirs))
      (setq subdirs (delete ".." subdirs))
      (dolist (el subdirs)
        (setq _temp_var
              (expand-file-name el entropy/iniext-submodule-dir))
        (when (entropy/iniext-empty-submodule-p _temp_var)
          (push _temp_var entropy/iniext-pre-update-submodules)))
      (if entropy/iniext-pre-update-submodules
          (setq rtn nil)
        (setq rtn t)))
    rtn))

(defun entropy/iniext-empty-submodule-p (subm-path)
  (let ((dir-list (directory-files subm-path))
        rtn)
    (setq dir-list (delete "." dir-list))
    (setq dir-list (delete ".." dir-list))
    (if (not dir-list)
        (setq rtn t)
      (setq rnt nil))
    rtn))
;; **** submodule extracting funcs
(defun entropy/iniext-init-submodules ()
  (let* ((default-directory (expand-file-name entropy/iniext-submodule-dir))
         (cmd-args '("submodule" "init"))
         _process)
    (with-current-buffer (get-buffer-create entropy/iniext-subinit-buffer)
      (goto-char (point-min))
      (insert "Init submodules ...... \n\n"))
    (set-buffer entropy/iniext-subinit-buffer)
    (delete-other-windows)
    (setq _process
          (apply 'start-process
                 "entropy/iniext-subinit"
                 entropy/iniext-subinit-buffer
                 "git" cmd-args))
    (set-process-sentinel _process 'entropy/iniext-subinit-sentinel)
    (set-process-filter _process 'entropy/iniext-subgot-cbk-filter)))

(defun entropy/iniext-subinit-sentinel (process cbk)
  (when (string-match "finished$" cbk)
    (kill-buffer entropy/iniext-subinit-buffer)
    (delete-other-windows)
    (when (yes-or-no-p "Update submodules?")
      (entropy/iniext-update-submodules))))


(defun entropy/iniext-update-submodules ()
  (let* ((default-directory (expand-file-name entropy/iniext-submodule-dir))
         (cmd "git submodule update")
         _process)
    (with-current-buffer (get-buffer-create
                          entropy/iniext-subupdate-buffer)
      (goto-char (point-min))
      (insert "Update submodules ...... \n"))
    (switch-to-buffer entropy/iniext-subupdate-buffer)
    (delete-other-windows)
    (setq _process
          (start-process-shell-command "entropy/iniext-subupdate"
                                       entropy/iniext-subupdate-buffer
                                       cmd))
    (set-process-filter
     _process
     'entropy/iniext-subgot-cbk-filter)
    (set-process-sentinel
     _process
     'entropy/iniext-subupdate-sentiniel)))

(defun entropy/iniext-subupdate-sentiniel (process cbk)
  (when (string-match "finished$" cbk)
    (with-current-buffer (process-buffer process)
      (kill-buffer))
    (delete-other-windows)
    (let ((buff (get-buffer-create "*after-update-submodules*")))
      (display-buffer buff)
      (with-current-buffer buff
        (goto-char (point-min))
        (insert "Successfully update submodules, please reboot emacs -v-.")))))

(defun entropy/iniext-subgot-cbk-filter (proc string)
  "Process filter func excerpt from
http://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html"
  (when (buffer-live-p (process-buffer proc))
    (display-buffer (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;; **** submodule extract main function
(defun entropy/iniext-extract-submodule ()
  "Extracting entropy-emacs submodules and return the
initialization status with 't or 'nil.

This func return Non-nil when the submodules had been
initialized (both of git for initing and updating.), so that can
be the conditions sticker for the following procedures.

For the status when the submodule had been initialized, it adding
the extensions info file path to `Info-additional-directory-list'.
"
  (interactive)
  (let ((init-envP (entropy/iniext-check-extern))
        (sub_status (entropy/iniext-substatus))
        rtn)
    (cond
     ((not init-envP)
      (entropy/iniext-extern-prompt)
      (setq rtn nil))
     ((eq sub_status t)
      (let* ((base-dir (expand-file-name
                        entropy/iniext-submodule-dir))
             (submodule-dirs (entropy/list-subdir base-dir)))
        (dolist (el submodule-dirs)
          (add-to-list 'load-path el)
          (entropy/iniext-add-subdirs-to-load-path el)))
      (entropy/iniext-add-subdoc)
      (setq rtn t))
     ((eq sub_status 'not-clone)
      (entropy/iniext-clone-extensions)
      (setq rtn nil))
     ((not sub_status)
      (entropy/iniext-init-submodules)
      (setq rtn nil)))
    rtn))

;; **** submodule make
;; ***** info load
(defun entropy/iniext-add-subdoc ()
  (with-eval-after-load 'info
    (let ((info-dirs entropy/iniext-submodules-with-doc-list))
      (dolist (el info-dirs)
        (add-to-list 'Info-additional-directory-list
                     (expand-file-name
                      el
                      entropy/iniext-submodule-dir))))))


;; *** force dependencies refer
(defun entropy/iniext-deps-status ()
  (let (rtn lossy)
    (dolist (el entropy/iniext-deps)
      (let ((dirN (expand-file-name
                   (car el) entropy/iniext-deps-dir)))
        (unless (file-directory-p dirN)
          (push el lossy))))
    (if lossy
        (setq entropy/iniext-deps-lossy lossy
              rtn nil)
      (setq rtn t))))

(defun entropy/iniext-deps-lossy-prompt  ()
  (with-current-buffer (get-buffer-create entropy/iniext-deps-lossy-prompt-buffer)
    (unless (not buffer-read-only)
      (read-only-mode 0))
    (goto-char (point-min))
    (dolist (el entropy/iniext-deps-lossy)
      (insert (concat (format "Losing '%s' ." (car el)) "\n"))))
  (let (rtn)
    (switch-to-buffer entropy/iniext-deps-lossy-prompt-buffer)
    (delete-other-windows)
    (setq rtn (yes-or-no-p
               "Please get them now:"))
    rtn))

(defun entropy/iniext-deps-get ()
  (when (yes-or-no-p
         "Getting entropy-emamcs-deps?")
    (let (cmds cmds_len (count 0) get-dep-process)
      (dolist (el entropy/iniext-deps-lossy)
        (cl-incf count)
        (let* ((item (concat (car el) "-git_clone"))
               (item-remote (cdr el))
               (item-clone-path (expand-file-name
                                 (car el)
                                 (expand-file-name entropy/iniext-deps-dir))))
          (push (list :cmd (list "clone" item-remote item-clone-path)
                      :seq (number-to-string count)
                      :item item)
                cmds)))
      (setq cmds (reverse cmds))
      (setq cmds_len (length cmds))
      (with-current-buffer
          (get-buffer-create entropy/iniext-deps-lossy-get-buffer)
        (unless (not buffer-read-only)
          (read-only-mode 0))
        (goto-char (point-min))
        (dolist (el cmds)
          (insert
           (concat (plist-get el :seq) ". Item: <" (plist-get el :item) "> cloning ...\n=====\n\n"))))
      (switch-to-buffer entropy/iniext-deps-lossy-get-buffer)
      (delete-other-windows)
      (setq count 0)
      (dolist (el cmds)
        (cl-incf count)
        (setq get-dep-process
              (apply 'start-process
                     (plist-get el :item)
                     entropy/iniext-deps-lossy-get-buffer
                     "git" (plist-get el :cmd)))
        (set-process-filter get-dep-process 'entropy/iniext-deps-get-cbk-filter)
        (set-process-sentinel get-dep-process 'entropy/iniext-deps-get-sentinel)))))


(defun entropy/iniext-deps-get-status-out (proc status)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (when (re-search-forward (process-name proc) nil t)
      (next-line 2)
      (forward-line 0)
      (cond
       ((string-match "exited abnormally with code \\([0-9]+\\)" status)
        (insert (format "Git clone '%s' fatal with code <%s> .\n"
                        (process-name proc)
                        (match-string 1 status)))
        (push nil entropy/iniext-deps-counter))
       ((string-match "finished$" status)
        (insert (format "Git clone '%s' success!\n"
                        (process-name proc)))
        (push t entropy/iniext-deps-counter))))))

(defun entropy/iniext-deps-get-sentinel (proc cbk)
  (entropy/iniext-deps-get-status-out proc cbk)
  (entropy/iniext-deps-get-final-func))

(defun entropy/iniext-deps-get-cbk-filter (proc string)
  (switch-to-buffer (process-buffer proc))
  (delete-other-windows))

(defun entropy/iniext-deps-get-final-func ()
  (when (and (not (null entropy/iniext-deps-lossy))
             (not (null entropy/iniext-deps-counter))
             (eq (length entropy/iniext-deps-lossy) (length entropy/iniext-deps-counter)))
    (cond ((member nil entropy/iniext-deps-counter)
           (entropy/iniext-deps-get-final-prompt "Get deps with error!"))
          (t (entropy/iniext-deps-get-final-prompt "Get deps successfully! Reboot emacs!")))))

(defun entropy/iniext-deps-get-final-prompt (string)
  (with-current-buffer (get-buffer-create "entropy-emacs-deps_stdout")
    (when buffer-read-only
      (read-only-mode 0))
    (goto-char (point-min))
    (insert string))
  (switch-to-buffer "entropy-emacs-deps_stdout")
  (delete-other-windows))

(defun entropy/iniext-extract-deps ()
  (interactive)
  (let ((init-envP (entropy/iniext-check-extern))
        (deps_status (entropy/iniext-deps-status))
        rtn)
    (cond
     ((not init-envP)
      (entropy/iniext-extern-prompt)
      (setq rtn nil))
     (deps_status
      (let* ((base-dir (expand-file-name
                        entropy/iniext-deps-dir))
             (subdeps-dirs (entropy/list-subdir base-dir)))
        (dolist (el subdeps-dirs)
          (add-to-list 'load-path el)
          (entropy/iniext-add-subdirs-to-load-path el)))
      (setq rtn t))
     ((not deps_status)
      (entropy/iniext-deps-get)
      (setq rtn nil)))))



;; ** main
(defun entropy/iniext-main ()
  (let ((deps_extractP (entropy/iniext-extract-deps))
        submodule-extractP
        rtn)
    (if (not deps_extractP)
        (setq rtn nil)
      (cl-case entropy/use-extensions-type
        (submodules
         (setq submodule-extractP (entropy/iniext-extract-submodule))
         (when submodule-extractP
           (setq rtn t)))
        (origin
         (setq rtn t))))
    rtn))


;; * provide
(provide 'entropy-emacs-extensions)
