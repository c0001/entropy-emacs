;;; entropy-emacs-batch.el --- entropy-emacs non-interactive toolchains
;;
;; * Copyright (C) 20190920  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-batch.el
;; Package-Version: 0.1.0
;; Version:       file-version
;; Created:       2019-09-20 18:18:18
;; Compatibility: GNU Emacs 25.2;
;; Package-Requires: ((emacs "25.2") (cl-lib "0.5"))
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
;; =entropy-emacs= native batch tools for non-interactive session,
;; usually for 'MAKE'.
;;
;; * Configuration:
;;
;; Designed for =entropy-emacs= only.
;;
;; * Code:

;; ** require

;; do not auto native-comp while we did batch make
(setq native-comp-deferred-compilation nil)

(defun entropy/emacs-batch-require-prefer-use-source
    (feature)
  (require feature (format "%s.el" feature)))

(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-message)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-defun)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-package)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-coworker)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-ext)

;; ** defvar
;; ** library
;; *** common library

;; *** section prompting
(defmacro entropy/emacs-batch--prompts-for-ext-install-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for extensions installing ...")
      (blue    "=================================================="))
     ,@body))

(defmacro entropy/emacs-batch--prompts-for-dump-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "           Section for emacs dump ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "Dump eemacs? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-ext-update-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for extensions updating ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "Do package update? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-coworkers-installing-section
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for coworkers installing ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "Install them? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-native-compile
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for native compiling `package-user-dir' ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "Native compile `package-user-dir'? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-eemacs-ext-build-repo-install
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for install entropy-emacs-extensions stable build ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "install eemacs-ext stable build? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-eemacs-fonts-install
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for install eemacs-fonts ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "install eemacs-fonts? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-byte-compile-eemacs-internal
    (&rest body)
  `(let ()
     (entropy/emacs-message-do-message
      "\n%s\n%s\n%s\n"
      (blue    "==================================================")
      (yellow "       Section for byte-compile eemacs internal ...")
      (blue    "=================================================="))
     (when (or (entropy/emacs-is-make-all-session) (yes-or-no-p "Compile? "))
       ,@body)))

;; *** make sections
;; **** dump emacs
(defun entropy/emacs-batch--dump-emacs-core ()
  (let ((dump-file (expand-file-name
                    (format "eemacs_%s.pdmp" (format-time-string "%Y%m%d%H%M%S"))
                    entropy/emacs-user-emacs-directory)))
    (setq entropy/emacs-fall-love-with-pdumper t)
    (entropy/emacs-batch-require-prefer-use-source
     'entropy-emacs-start)
    (dump-emacs-portable dump-file)))

(defun entropy/emacs-batch--dump-emacs ()
  (unless (not (version< emacs-version "27"))
    (entropy/emacs-message-do-error
     (red
      "You just can portable dump emacs while emacs version upon 27, abort")))
  (entropy/emacs-batch--dump-emacs-core))

;; **** install coworkers

(defun entropy/emacs-batch--install-coworkers (&optional prefix)
  (interactive "P")
  (let ((count 1)
        (newhost (when prefix
                   (read-directory-name "Coworker Host: " nil nil t)))
        (task-list '((:name "tern"           :pred entropy/emacs-coworker-check-tern-server :enable (EEMACS-DT-IDENTITY t))
                     (:name "web-lsp"        :pred entropy/emacs-coworker-check-web-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "js-lsp"         :pred entropy/emacs-coworker-check-js-lsp      :enable (EEMACS-DT-IDENTITY t))
                     (:name "json-lsp"       :pred entropy/emacs-coworker-check-json-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "xml-lsp"        :pred entropy/emacs-coworker-check-xml-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "php-lsp"        :pred entropy/emacs-coworker-check-php-lsp     :enable (EEMACS-DT-IDENTITY t))
                     (:name "bash-lsp"       :pred entropy/emacs-coworker-check-bash-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "python-lsp"     :pred entropy/emacs-coworker-check-python-lsp  :enable (EEMACS-DT-IDENTITY t))
                     (:name "python-pyright" :pred entropy/emacs-coworker-check-pyright-lsp :enable (EEMACS-DT-IDENTITY t))
                     (:name "cmake-lsp"      :pred entropy/emacs-coworker-check-cmake-lsp   :enable (EEMACS-DT-IDENTITY t))
                     (:name "java-lsp"       :pred entropy/emacs-coworker-check-java-lsp    :enable (EEMACS-DT-IDENTITY t))
                     (:name "clangd-lsp"     :pred entropy/emacs-coworker-check-clangd-lsp  :enable (EEMACS-DT-IDENTITY t))
                     (:name "wsl-open"       :pred entropy/emacs-coworker-check-wsl-open    :enable (EEMACS-DT-FORM sys/wsl2-env-p))
                     (:name "pwsh-lsp"       :pred entropy/emacs-coworker-check-pwsh-lsp    :enable (EEMACS-DT-FORM (and (executable-find "pwsh")
                                                                                                                         (executable-find "dotnet"))))
                     (:name "python-lsp-ms"  :pred entropy/emacs-coworker-check-pyls-ms-lsp :enable (EEMACS-DT-FORM
                                                                                                     (executable-find "dotnet")))

                     )))
    (entropy/emacs-with-coworker-host
      newhost
      (dolist (el task-list)
        (when (entropy/emacs-type-spec-eval (plist-get el :enable))
          (entropy/emacs-message-do-message
           "%s %s %s"
           (cyan (format ">> %s: " count))
           (yellow (format "'%s'" (plist-get el :name)))
           (green "installing ..."))
          (funcall (plist-get el :pred))
          (cl-incf count))))))

;; **** backup `package-user-dir'
(defun entropy/emacs-batch--backup-extensions ()
  (let* ((host-path (file-name-directory package-user-dir))
         (archive (file-name-nondirectory package-user-dir))
         (time-stamp (format-time-string "%Y%m%d%H%M%S"))
         (archive-bck (concat archive "_" time-stamp))
         (bcknew (expand-file-name archive-bck host-path)))
    (entropy/emacs-message-do-message
     (green "Backup `package-user-dir' ..."))
    (copy-directory package-user-dir
                    bcknew)
    (entropy/emacs-message-do-message
     "%s %s %s"
     (green "Backup to")
     (yellow (format "'%s'" bcknew))
     (green "completely!"))))

;; **** native compile `package-user-dir'

(defun entropy/emacs-batch--around-advice-for-native-compile (orig-func &rest orig-args)
  (let ((file-or-func (caar comp-files-queue)))
    (entropy/emacs-message-do-message
     "%s%s%s"
     "↳ "
     (yellow (format "⚠ native compiling for %s :"
                     (if (functionp file-or-func)
                         "function"
                       (if (ignore-errors (file-exists-p file-or-func))
                           "file"
                         "unkown object")
                       )))
     (if (ignore-errors (file-exists-p file-or-func))
         (file-name-nondirectory file-or-func)
       (format "%s" file-or-func)))
    (apply orig-func orig-args)))

(defun entropy/emacs-batch--native-compile-package-dir ()
  (let* ((comp-verbose 0)
         (pkg-dirs `(,package-user-dir
                     ,(file-name-directory
                       (locate-library "subr")))))
    (dolist (pkg-dir pkg-dirs)
      (unwind-protect
          (progn
            (entropy/emacs-message-do-message
             "%s '%s' %s"
             (green "Do native compile for package dir")
             (yellow  (format "%s" pkg-dir))
             (green  "......"))
            (sleep-for 3)
            (advice-add 'comp-run-async-workers
                        :around #'entropy/emacs-batch--around-advice-for-native-compile)
            (native-compile-async pkg-dir 'recursively)
            (while (or comp-files-queue
                       (> (comp-async-runnings) 0))
              (sleep-for 1)))
        (progn
          (advice-remove
           'comp-run-async-workers
           #'entropy/emacs-batch--around-advice-for-native-compile))))))

;; **** get entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo

(defun entropy/emacs-batch--install-eemacs-ext-stable-build-repo-core ()
  (let* ((url entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url)
         (host entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
         (stuff-dir
          (make-temp-name (expand-file-name
                           (format
                            "eemacs-ext-stable-build-cache\
/eemacs-ext-build-%s_%s_"
                            entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
                            (format-time-string "%Y%m%d%H%M%S"))
                           entropy/emacs-stuffs-topdir)))
         (dec-host
          (make-temp-name (expand-file-name
                           "eemacs-ext-stable-repo-decompress_"
                           stuff-dir)))
         (tmp-name
          (make-temp-name (expand-file-name
                           "eemacs-ext-stable-repo-archive.txz_"
                           stuff-dir)))
         download-cbk)
    (make-directory stuff-dir t)
    ;; download archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Downloading eemacs-ext-stable repo ... "))
    (setq download-cbk
          (entropy/emacs-network-download-file
           url tmp-name
           (when (executable-find "curl") t) nil
           (lambda (file)
             (let* ((inhibit-read-only t)
                    (stick-hash
                     entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-archive-sha256sum)
                    (buff (create-file-buffer file))
                    (cur-hash nil))
               (with-current-buffer buff
                 (erase-buffer)
                 (insert-file-contents-literally file))
               (setq cur-hash
                     (secure-hash 'sha256 buff))
               (unless (string= cur-hash stick-hash)
                 (error "Sha256 hash verify for file <%s> \
faild with hash '%s' which must match '%s'"
                        file cur-hash stick-hash))
               t))))
    (unless (eq (symbol-value download-cbk) 'success)
      (entropy/emacs-message-do-error
       "%s: %s"
       (red "Download fatal with")
       (format "%s" (get download-cbk 'error-type))))
    (entropy/emacs-message-do-message
     "%s"
     (green "Download eemacs-ext-stable repo done!"))
    ;; extract archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Extract eemacs-ext-stable repo ..."))
    (make-directory dec-host t)
    (entropy/emacs-archive-dowith
     'txz tmp-name dec-host :extract)
    (let ((dirname
           (entropy/emacs-file-path-parser
            (car
             (entropy/emacs-list-subdir
              dec-host))
            'non-trail-slash)))
      (rename-file dirname host)
      ;; generate inited indicator
      (entropy/emacs-with-temp-buffer
        (write-file
         entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path-comprehensive-indicator
         )))
    (entropy/emacs-message-do-message
     "%s"
     (green "Get eemacs-ext-stable repo done!"))))

(defun entropy/emacs-batch--install-eemacs-ext-stable-build-repo ()
  (let ((host entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
        (did-p t))
    (when (file-exists-p host)
      (setq did-p
            (yes-or-no-p
             (format "Remove old install (%s)? "
                     host)))
      (and did-p
           (rename-file
            host
            (concat host
                    (format ".bak_%s"
                            (format-time-string "%Y%m%d%H%M%S"))))))
    (if did-p
        (entropy/emacs-batch--install-eemacs-ext-stable-build-repo-core)
      (entropy/emacs-message-do-message
       "%s"
       (yellow "Abort!")))))

(advice-add 'entropy/emacs-batch--install-eemacs-ext-stable-build-repo
            :around
            #'entropy/emacs-advice-for-common-do-with-http-proxy)

;; **** get eemacs-font

(defun entropy/emacs-batch--install-eemacs-fonts ()
  (let* ((url entropy/emacs-ext-eemacs-fonts-archive-url)
         (stuff-dir
          (make-temp-name
           (expand-file-name
            (format
             "eemacs-exts-fonts-cache/eemacs-ext-fonts-temp-host_%s_"
             (format-time-string "%Y%m%d%H%M%S"))
            entropy/emacs-stuffs-topdir)))
         (dec-host
          (make-temp-name (expand-file-name
                           "eemacs-ext-fonts-extract_"
                           stuff-dir)))
         (tmp-name
          (make-temp-name (expand-file-name
                           "eemacs-ext-fonts-archive.txz_"
                           stuff-dir)))
         download-cbk)
    (make-directory stuff-dir t)
    ;; download archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Downloading eemacs-fonts archive ... "))
    (setq download-cbk
          (entropy/emacs-network-download-file
           url tmp-name
           (when (executable-find "curl") t) nil
           (lambda (file)
             (let* ((stick-hash
                     entropy/emacs-ext-eemacs-fonts-archive-sha256sum)
                    (cur-hash (entropy/emacs-file-secure-hash file 'sha256 stick-hash nil t)))
               (unless (string= cur-hash stick-hash)
                 (error "Sha256 hash verify for file <%s> \
faild with hash '%s' which must match '%s'"
                        file cur-hash stick-hash))
               t))))
    (unless (eq (symbol-value download-cbk) 'success)
      (entropy/emacs-message-do-error
       "%s: %s"
       (red "Download fatal with")
       (format "%s" (get download-cbk 'error-type))))
    (entropy/emacs-message-do-message
     "%s"
     (green "Download eemacs-fonts repo done!"))
    ;; -------------------- extract archive
    (entropy/emacs-message-do-message
     "%s"
     (green "Extract eemacs-fonts ..."))
    (make-directory dec-host t)
    (entropy/emacs-archive-dowith
     'txz tmp-name dec-host :extract)
    (entropy/emacs-message-do-message
     "%s"
     (green "Get eemacs-fonts done!"))
    ;; -------------------- install fonts
    (entropy/emacs-message-do-message
     "%s"
     (green "Install eemacs-fonts  ..."))
    (let ((repo-path (car (entropy/emacs-list-subdir
                           dec-host))))
      (if (file-directory-p repo-path)
          (entropy/emacs-message-do-message
           "%s"
           (green (format "--> default-directory: %s"
                          repo-path)))
        (error "Internal fatal: <%s> not exists!"
               repo-path))
      (entropy/emacs-make-process
       `(:name
         " *install eemacs-fonts* "
         :synchronously t
         :buffer (get-buffer-create " *install eemacs-fonts process buffer* ")
         :command '("sh" "-c" "./install.sh")
         :default-directory ,repo-path
         :after
         (with-current-buffer $sentinel/destination
           (let ((msg (buffer-substring-no-properties (point-min) (point-max))))
             (entropy/emacs-message-do-message
              "%s"
              (blue msg)))
           (entropy/emacs-message-do-message
            "%s"
            (green "Install eemacs-fonts done!")))
         :error
         (with-current-buffer $sentinel/destination
           (let ((msg (buffer-substring-no-properties (point-min) (point-max))))
             (entropy/emacs-message-do-message
              "%s\n%s"
              (yellow "Install eemacs-fonts ecounter fatal!")
              (red msg)))
           (entropy/emacs-message-do-error
            "%s"
            (red "Fatal abort!")))))
      )
    ))

(advice-add 'entropy/emacs-batch--install-eemacs-fonts
            :around
            #'entropy/emacs-advice-for-common-do-with-http-proxy)

;; **** byte compile tentacles

(defun entropy/emacs-batch--byte-compile-dir (dir)
  (let* ((dir-cur (expand-file-name dir))
         (dir-cur-P (unless (file-exists-p dir-cur)
                      (error "Directory '%s' not exists!" dir-cur)))
         (dir-list (directory-files (expand-file-name dir-cur)))
         source-dirP)
    (catch :exit
      (dolist (el dir-list)
        (when (string-match-p "\\.el$" el)
          (setq source-dirP t)
          (throw :exit nil))))
    (if source-dirP
        (byte-recompile-directory dir-cur 0 t)
      (error "Dir %s is not an elisp source dir"
             dir))))

(defvar entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/pkg-init-p
  nil)
(defvar entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/timestamp
  (format-time-string "%Y%m%d%H%M%S"))
(defun entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks
    (name host require-features &optional clean)
  (let* ((dir (expand-file-name
               (format "elements/core/%s" host)
               entropy/emacs-user-emacs-directory))
         (elcs (directory-files dir nil ".*\\.elc$"))
         (log-file (expand-file-name
                    (format "%s/date-%s/eemacs-%s-compile-%s.log"
                            "eemacs-core-bytecompile-log"
                            entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/timestamp
                            name
                            (format-time-string "%Y%m%d%H%M%S"))
                    entropy/emacs-stuffs-topdir)))
    (cond (clean
           (if (null elcs)
               (entropy/emacs-message-do-message
                "%s: %s"
                (yellow "[WARN]")
                (yellow "no compiled files need to be cleaned for dir %s"
                        (blue "%s" dir)))
             (dolist (file elcs)
               (delete-file (expand-file-name file dir) t))
             (entropy/emacs-message-do-message
              "%s"
              (green "Clean compile files for dir '%s' done"
                     (blue "%s" dir)))))
          (t
           ;; declare this session status
           (setq entropy/emacs-session-in-byte-compile-emacs-core-p t)
           ;; (setq debug-on-error t)
           (entropy/emacs-message-do-message
            "\n==================== compile eemacs core/%s ... ===================="
            (green host))
           (unless entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/pkg-init-p
             (entropy/emacs-package-common-start)
             (setq entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/pkg-init-p
                   t))
           (dolist (feature require-features)
             ;; require the feature in source
             (entropy/emacs-batch-require-prefer-use-source feature))
           (entropy/emacs-batch--byte-compile-dir dir)
           (let* ((log-buff-name "*Compile-Log*")
                  (log-buff (get-buffer log-buff-name)))
             (when (bufferp log-buff)
               (with-current-buffer log-buff
                 (entropy/emacs-write-file log-file t)
                 (kill-buffer log-buff))))))))

(defvar entropy/emacs-batch--bytecompile-item-register
  '(
    (eemacs-baron-wasteland-var-binds
     "wasteland/var-binds"
     nil)

    (eemacs-baron-wasteland-func-binds
     "wasteland/func-binds"
     nil)

    (eemacs-baron-startup
     "baron/startup"
     nil)

    (eemacs-baron-basic-ui
     "baron/basic-ui"
     nil)

    (eemacs-baron-summon
     "baron/summon"
     nil)

    (eemacs-baron-utils
     "baron/utils"
     nil)

    (eemacs-baron-hollow
     "baron/hollow"
     nil)

    (eemacs-tentacles
     "tentacles"
     (entropy-emacs-utils
      entropy-emacs-path
      entropy-emacs-window-parameter-memory
      entropy-emacs-hydra-hollow))

    ))

(defun entropy/emacs-batch--do-bytecompile-eemacs-core
    (&optional clean)
  (let ((module-pkg-incs
         (and (not clean)
              (delete nil
                      `(,(when (entropy/emacs-vterm-support-p)
                           '(vterm ("cmake" ".") ("make"))))))))
    ;; Compile dynamic modules firstly since the byte-compile process
    ;; will load the module as well.
    (when module-pkg-incs
      (dolist (spec module-pkg-incs)
        (entropy/emacs-package-compile-dynamic-module
         (car spec) (cdr spec))))
    (dolist (item entropy/emacs-batch--bytecompile-item-register)
      (apply 'entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks
             (if clean
                 (append item '(t))
               item)))))

;; ** interactive
(when (entropy/emacs-ext-main)
  (let ((type (entropy/emacs-is-make-session)))
    (cond
     ((equal type "Install")
      (entropy/emacs-batch--prompts-for-ext-install-section
       (entropy/emacs-package-install-all-packages)))
     ((equal type "Compile")
      ;; we must check all depedencies firstly while compile
      (entropy/emacs-package-install-all-packages)
      (when entropy/emacs-package-install-success-list
        (entropy/emacs-message-do-error
         (red "Please re-do current make operation since we solved deps broken")))
      (entropy/emacs-batch--prompts-for-byte-compile-eemacs-internal
       (entropy/emacs-batch--do-bytecompile-eemacs-core)))
     ((equal type "Compile-Clean")
      (entropy/emacs-batch--prompts-for-byte-compile-eemacs-internal
       (entropy/emacs-batch--do-bytecompile-eemacs-core t)))
     ((equal type "Install-Coworkers")
      (entropy/emacs-batch--prompts-for-coworkers-installing-section
       (entropy/emacs-batch--install-coworkers)))
     ((equal type "Install-Eemacs-Ext-Build")
      (entropy/emacs-batch--prompts-for-eemacs-ext-build-repo-install
       (entropy/emacs-batch--install-eemacs-ext-stable-build-repo)))
     ((equal type "Install-Eemacs-Fonts")
      (entropy/emacs-batch--prompts-for-eemacs-fonts-install
       (entropy/emacs-batch--install-eemacs-fonts)))
     ((equal type "Update")
      (if (entropy/emacs-package-package-archive-empty-p)
          (entropy/emacs-message-do-error
           (red "You haven't install packages, can not do updating, abort!"))
        (entropy/emacs-batch--prompts-for-ext-update-section
         (entropy/emacs-batch--backup-extensions)
         (entropy/emacs-package-update-all-packages))))
     ((equal type "Dump")
      ;; make dump file
      (entropy/emacs-batch--prompts-for-dump-section
       (entropy/emacs-batch--dump-emacs)))
     ((and (ignore-errors (native-comp-available-p))
           (equal type "Native-Comp"))
      ;; we must prepare for `package-user-dir' before native-comp
      (entropy/emacs-package-install-all-packages)
      (entropy/emacs-batch--prompts-for-native-compile
       (entropy/emacs-batch--native-compile-package-dir)))
     (t
      (entropy/emacs-message-do-error
       (red (format "Unknown making type '%s'" type)))))))

;; * provide
(provide 'entropy-emacs-batch)
