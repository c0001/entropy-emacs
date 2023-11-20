;;; entropy-emacs-batch.el --- entropy-emacs non-interactive toolchains  -*- lexical-binding: t; -*-
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

(defconst entropy/emacs-batch--make-env-type
  (entropy/emacs-is-make-session)
  "The `entropy/emacs-is-make-session' return for this emacs batch
session"
  )

;; do not auto native-comp while we did batch make
(setq native-comp-deferred-compilation nil
      ;; emacs 29.1 declared as replacement of
      ;; `native-comp-deferred-compilation'
      native-comp-jit-compilation      nil)

(defun entropy/emacs-batch-require-prefer-use-source
    (feature)
  (if (equal "Dump" entropy/emacs-batch--make-env-type)
      ;; let dump session using byte-compile procedure
      (require feature)
    (require feature (format "%s.el" feature))))

;; ** defvar

(defvar entropy/emacs-batch-help-string
  (format
   "
                   Entropy-Emacs Make

Welcom to Entropy-Emacs Make procedure, there's two 'make' options
provision that 'install', 'install-coworkers', for each as install all
elisp packges, install external depedencies.

#+begin_quote
  NOTE: =entropy-emacs= currently support lowest emacs version is
  =emacs-%s=.
#+end_quote

Before invoking 'make', you should setting env var of:
- EMACS_MAJOR_VERSION: default 28

  In which case, set it properly in which emacs session you will
  use. (note: type =C-h v emacs-major-version= to see what entity
  should be set on.)

- EMACS: default the first matched 'emacs' caller in =$PATH= and its
  major-version must match the EMACS_MAJOR_VERSION or will make messy
  yourself.

Make targets:

       make all
       make install
       make compile
       make liberime
       make install-coworkers
       make install-eemacs-ext-build
       make install-eemacs-fonts
       make update
       make dump
       make native-comp

- 'update' option update existed installed packages.

- 'dump' option dump the emacs using pdumper function
  `dump-emacs-portable' (need emacs 27 version or above).

  The dumped emacs binary file is stored in
  'entropy/emacs-user-emacs-directory' (i.e. the entropy-emacs
  specified `user-emacs-directory' and be equivalent to it in most of
  cases) with name-space as 'eemacs_YearDateTime.pdmp', start it with:

  -------
         emacs --dump-file=eemacs_YearDateTime.pdmp
  -------

- 'native-comp' option will using gccemacs 'native-compile' func to
  compile all packages hosted in `package-user-dir'. It's useful when
  you use gccemacs.

- 'install-coworkers' option will install all system-wide emacs
  dependencies where entropy-emacs specified, you will do it when
  you're missing any of libraries prompt internally of entropy-emacs
  using.

- 'install-eemacs-ext-build' option download specified
  =entropy-emacs-extensions-project-build= to use as local melpa
  mirror according current entropy-emacs version in which case after
  successfully installed thus, entropy-emacs will use internal
  specified packages version to get best experience for
  entropy-emacs. (If the old build installed detected, we backup it in
  the same place and install the new one.)

  Currently =entropy-emacs-extensions-project-build= in used version
  is =%s=. (you can review it by manually download in via [[%s]])

- 'install-eemacs-fonts' option download and install entropy-emacs
  suggested free copyright fonts (NOTE: auto-install in *nix system
  but WINDOWS etc. on which show the prompts for manually installing.)

- 'compile' option compile entropy-emacs to get better startup speed
  and running performance. Use 'compile-clean' to clean the
  bytecompile files.

- 'liberime' option compile 'liberime' emacs-module based on your
  system, note that you should have 'librime' and its develop headers
  installed on your system e.g. in debian `apt install librime-dev'.

  On windows platform or looing for more details please see the README
  file under \"elements/site-lisp/liberime\".

- 'all' option combine the [install, install-coworkers, liberime,
  compile] sections together and export env varaible EEMACS_MAKE_ALL
  with 1 before any operation did.

- 'install-systemd-service' option install a user space sysemd service
  for current user only, which can use a service session name via env
  variable EEMACS_SESSION (defaults to \"main\").

MAINTAINS:

        make debug: startup entropy-emacs with debug facilities


In used emacs version is: %s
"
   entropy/emacs-lowest-emacs-version-requirement
   entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
   entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url
   emacs-version
   ))

(when (equal entropy/emacs-batch--make-env-type "Help")
  (message "%s" entropy/emacs-batch-help-string)
  (kill-emacs))

;; ** library

(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-message)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-defun)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-package)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-coworker)
(entropy/emacs-batch-require-prefer-use-source 'entropy-emacs-ext)

;; *** common library

;; *** section prompting

(defmacro entropy/emacs-batch--with-prompts-msg (msg &rest body)
  (declare (indent 1))
  `(progn (entropy/emacs-message-do-message
           "\n%s\n%s\n%s\n"
           (blue    "==================================================")
           (yellow "       %s ..." ,msg)
           (blue    "=================================================="))
          ,@body))

(defmacro entropy/emacs-batch--prompts-for-ext-install-section
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg "Section for extensions installing"
     ,@body))

(defmacro entropy/emacs-batch--prompts-for-dump-section
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg "Section for emacs dump"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "Dump eemacs? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-ext-update-section
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg "Section for extensions updating"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "Do package update? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-coworkers-installing-section
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg "Section for coworkers installing"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "Install them? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-native-compile
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg
       "Section for native compiling `package-user-dir'"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "Native compile `package-user-dir'? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-eemacs-ext-build-repo-install
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg
       "Section for install entropy-emacs-extensions stable build"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "install eemacs-ext stable build? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-eemacs-fonts-install
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg
       "Section for install eemacs-fonts"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "install eemacs-fonts? "))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-byte-compile-eemacs-internal
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg
       "Section for byte-compile eemacs internal"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (or (bound-and-true-p entropy/emacs-fall-love-with-pdumper)
                   (yes-or-no-p "Compile? ")))
       ,@body)))

(defmacro entropy/emacs-batch--prompts-for-byte-compile-clean-eemacs-internal
    (&rest body)
  `(entropy/emacs-batch--with-prompts-msg
       "Section for byte-compile eemacs internal"
     (when (or (entropy/emacs-is-make-all-session)
               (entropy/emacs-is-make-with-all-yes-session)
               (yes-or-no-p "Clean compilations? "))
       ,@body)))

;; *** make sections
;; **** dump emacs
(defun entropy/emacs-batch--dump-emacs-core ()
  (let ((dump-file (expand-file-name
                    (format "eemacs_%s.pdmp" (format-time-string "%Y%m%d%H%M%S"))
                    entropy/emacs-user-emacs-directory)))
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

(defalias 'entropy/emacs-batch--install-coworkers
  #'entropy/emacs-coworker-install-all-coworkers)

;; **** backup `package-user-dir'
(defun entropy/emacs-batch--backup-extensions ()
  (let* ((host-path (file-name-directory package-user-dir))
         (archive (file-name-nondirectory package-user-dir))
         (time-stamp (format-time-string "%Y%m%d%H%M%S"))
         (archive-bck (concat archive "_" time-stamp))
         (bcknew (expand-file-name archive-bck host-path)))
    (entropy/emacs-message-do-message
     (green "Backup `package-user-dir' ..."))
    (copy-directory package-user-dir bcknew)
    (entropy/emacs-message-do-message
     "%s %s %s"
     (green "Backup to")
     (yellow (format "'%s'" bcknew))
     (green "completely!"))))

;; **** native compile `package-user-dir'

(defun entropy/emacs-batch--native-comp-cu-done (file)
  (entropy/emacs-message-do-message
   "%s native compiled for file `%s'"
   (green "✓ DONE")
   file))

(defun entropy/emacs-batch--native-compile-package-dir ()
  (require 'comp)
  (let* ((native-comp-verbose 0)
         (pkg-dirs `(,package-user-dir ,(file-name-directory (locate-library "subr"))))
         (native-comp-async-cu-done-functions '(entropy/emacs-batch--native-comp-cu-done))
         (ignore-rexps
          (if (version< emacs-version "29.1")
              (bound-and-true-p native-comp-deferred-compilation-deny-list)
            (bound-and-true-p native-comp-jit-compilation-deny-list)))
         files)
    (dolist (pkg-dir pkg-dirs)
      (entropy/emacs-setf-by-body files
        (entropy/emacs-mapcar-without-orphans
         (lambda (x) (unless (entropy/emacs-string-match-p x ignore-rexps) x))
         (directory-files-recursively pkg-dir comp-valid-source-re)
         nil nil))
      (entropy/emacs-message-do-message
       "%s%s%s `%s' %s"
       (green "Do native compile")
       (format " (%d files) " (length files))
       (green "for package dir")
       (yellow  (format "%s" pkg-dir))
       (green  "......"))
      (sleep-for 0.001)
      (native-compile-async files)
      (entropy/emacs-sleep-while
       (or comp-files-queue (> (comp-async-runnings) 0))))))

;; **** get entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo

(defun entropy/emacs-batch--install-eemacs-ext-stable-build-repo-core ()
  (let* ((url entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url)
         (host entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
         (_ (when-let (((file-exists-p host))
                       (fname (directory-file-name host))
                       (date  (format-time-string "%Y%m%d%H%M%S"))
                       (fname-old (format "%s.old_%s" fname date)))
              (while (file-exists-p fname-old)
                (setq fname-old
                      (concat fname-old (format "_%s" (random most-positive-fixnum)))))
              (unless (yes-or-no-p (format "Backup old Installation (mv `%s' to `%s') ? "
                                           fname fname-old))
                (entropy/emacs-message-do-error
                 "%s" (yellow "Abort!")))
              (condition-case err
                  (rename-file fname fname-old)
                (error
                 (entropy/emacs-message-do-error
                  "%s" (red (format "Error: backup old eemacs-ext-build \
'%s' to '%s' with fatal of (%s)" fname fname-old err)))))))
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
    (dolist (d `(,stuff-dir
                 ,(file-name-directory (directory-file-name host))))
      (make-directory d t))
    ;; download archive
    (entropy/emacs-message-do-message
     "%s %s %s"
     (green "Downloading eemacs-ext-stable repo")
     (yellow (format "%s" url))
     (green "..."))
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
                 (entropy/emacs-error-without-debugger
                  "Sha256 hash verify for file <%s> \
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
             (entropy/emacs-list-dir-subdirs
              dec-host))
            'non-trail-slash)))
      (rename-file dirname host)
      ;; generate inited indicator
      (with-temp-buffer
        (write-file
         entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path-comprehensive-indicator
         )))
    (entropy/emacs-message-do-message
     "%s"
     (green "Get eemacs-ext-stable repo done!"))))

(defun entropy/emacs-batch--install-eemacs-ext-stable-build-repo ()
  (entropy/emacs-batch--install-eemacs-ext-stable-build-repo-core))

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
                 (entropy/emacs-error-without-debugger
                  "Sha256 hash verify for file <%s> \
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
    (let ((repo-path dec-host))
      (if (file-directory-p repo-path)
          (entropy/emacs-message-do-message
           "%s"
           (green (format "--> default-directory: %s"
                          repo-path)))
        (entropy/emacs-error-without-debugger
         "Internal fatal: <%s> not exists!"
         repo-path))
      (entropy/emacs-make-process
       `(:name
         " *install eemacs-fonts* "
         :synchronously t
         :buffer (get-buffer-create " *install eemacs-fonts process buffer* ")
         :command '("sh" "./install.sh")
         :default-directory ,(entropy/emacs-return-as-default-directory repo-path)
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

;; FIXME: supress large coding neglects' byte-compile warning in
;; eemacs which we should fix in future.
(setq byte-compile-warnings
      '(not
        unresolved
        callargs
        docstrings
        make-local
        ))

(defun entropy/emacs-batch--byte-compile-dir (dir)
  (let* ((dir-cur (expand-file-name dir))
         (_ (unless (file-exists-p dir-cur)
              (entropy/emacs-error-without-debugger
               "Directory '%s' not exists!" dir-cur)))
         (dir-list (directory-files (expand-file-name dir-cur)))
         source-dirP)
    (catch :exit
      (dolist (el dir-list)
        (when (string-match-p "\\.el$" el)
          (setq source-dirP t)
          (throw :exit nil))))
    (if source-dirP
        (dolist (f (entropy/emacs-list-dir-subfiles dir-cur))
          (unless (string-match-p "^.*-pkg\\.el$" f)
            (when (string-match-p "^.*\\.el$" f)
              (byte-recompile-file f t 0))))
      (entropy/emacs-error-without-debugger
       "Dir %s is not an elisp source dir"
       dir))))

(defvar entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks/timestamp
  (format-time-string "%Y%m%d%H%M%S"))
(defun entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks
    (name host require-features &optional clean)
  (let* ((label "")
         (dir (expand-file-name
               (cond
                ((string-prefix-p "-" host)
                 (prog1
                     (progn
                       (setq host (replace-regexp-in-string
                                   "^-" ""
                                   host))
                       (format "elements/site-lisp/%s" host))
                   (setq label "site-lisp")))
                ((string= "top" host)
                 (prog1
                     (progn
                       (setq host "")
                       (format "elements/%s" host))
                   (setq label "eemacs-top-libs")))
                (t
                 (prog1
                     (format "elements/core/%s" host)
                   (setq label "core"))))
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
            "\n==================== compile eemacs %s/%s ... ===================="
            (green label)
            (green host))
           (dolist (feature require-features)
             ;; require the feature in source
             (entropy/emacs-batch-require-prefer-use-source feature))
           (entropy/emacs-batch--byte-compile-dir dir)
           (let* ((log-buff-name byte-compile-log-buffer)
                  (log-buff (get-buffer log-buff-name)))
             (when (buffer-live-p log-buff)
               (with-current-buffer log-buff
                 (when (> (buffer-size (current-buffer)) 0)
                   (entropy/emacs-write-file log-file t))
                 (kill-buffer log-buff))))))))

(defvar entropy/emacs-batch--bytecompile-item-register
  `(
    (eemacs-top-declare
     "top"
     nil)

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

    (eemacs-site-lisp_entropy-adblockP-rule-analysis
     "-entropy-adblockP-rule-analysis"
     nil)
    (eemacs-site-lisp_entropy-cn-dict
     "-entropy-cn-dict"
     nil)
    (eemacs-site-lisp_entropy-dired-cp-or-mv
     "-entropy-dired-cp-or-mv"
     nil)
    (eemacs-site-lisp
     "-entropy-en-words"
     nil)
    (eemacs-site-lisp_entropy-global-read-only-mode
     "-entropy-global-read-only-mode"
     nil)
    (eemacs-site-lisp_entropy-open-with
     "-entropy-open-with"
     nil)
    (eemacs-site-lisp_entropy-org-batch-refile
     "-entropy-org-batch-refile"
     nil)
    (eemacs-site-lisp_entropy-org-export-theme-toggle
     "-entropy-org-export-theme-toggle"
     nil)
    (eemacs-site-lisp_entropy-org-widget
     "-entropy-org-widget"
     nil)
    (eemacs-site-lisp_entropy-portableapps
     "-entropy-portableapps"
     nil)
    (eemacs-site-lisp_entropy-proxy-url
     "-entropy-proxy-url"
     nil)
    (eemacs-site-lisp_entropy-s2t
     "-entropy-s2t"
     nil)
    (eemacs-site-lisp_entropy-sdcv
     "-entropy-sdcv"
     nil)
    (eemacs-site-lisp_entropy-shellpop
     "-entropy-shellpop"
     nil)
    ,(when (= emacs-major-version 28)
       '(eemacs-site-lisp_image-dired-28-patch
         "-image-dired/28"
         nil))
    (eemacs-site-lisp_lsp-java-simple
     "-lsp-java-simple"
     nil)
    (eemacs-site-lisp_undo-tree-eemacs
     "-undo-tree-eemacs"
     nil)
    (eemacs-site-lisp_emacs-rime
     "-emacs-rime"
     nil)

    ))
(setq entropy/emacs-batch--bytecompile-item-register
      (delete nil entropy/emacs-batch--bytecompile-item-register))

(defun entropy/emacs-batch--do-bytecompile-eemacs-core
    (&optional clean)
  (let ((module-pkg-incs
         (and (not clean)
              (delete
               nil
               `(,(when (entropy/emacs-vterm-support-p)
                    `(vterm
                      (("mkdir" "-p" "build")
                       (:default-directory
                        "build"
                        :command
                        ("sh" "-c" "cmake -G 'Unix Makefiles' ..;"))
                       (:default-directory
                        "build"
                        :command ("make")))))))))
        pkg cmds penv defdir)
    ;; Compile dynamic modules firstly since the byte-compile process
    ;; will load the module as well.
    (when module-pkg-incs
      (dolist (spec module-pkg-incs)
        (setq pkg (car spec) cmds (cadr spec)
              penv (nth 2 spec) defdir (nth 3 spec))
        (entropy/emacs-package-compile-dynamic-module
         pkg cmds penv defdir)))
    (dolist (item
             (let ((val entropy/emacs-batch--bytecompile-item-register)
                   (extra-cleans
                    (list '(eemacs-baron-utils
                            "baron/batch"
                            nil))))
               (if (not clean) val
                 (append extra-cleans val))))
      (apply 'entropy/emacs-batch--bytecompile-eemacs-core-utils-frameworks
             (if clean (append item '(t)) item)))
    (unless clean
      (entropy/emacs-with-file-buffer entropy/emacs-inner-preload-vars-file
        :with-kill-visitings-pred 'always
        :without-save-visitings-pred 'always
        :with-kill-buffer-when-done t
        :with-find-file-function 'pure
        (setq-local auto-save-default nil)
        (setq-local make-backup-files nil)
        (setq-local before-save-hook nil)
        (erase-buffer)
        (dolist (el entropy/emacs-inner-preload-vars)
          (insert (format "(setq %S %S)\n" el (symbol-value el))))
        (save-buffer)))))

;; **** compile extra facilities

(defun entropy/emacs-batch--make-extra-utils-wrapper
    (name path command-list)
  (let ((cmdstr (mapconcat 'identity command-list " ")))
    (entropy/emacs-with-make-process
     :name name
     :synchronously t
     :command command-list
     :buffer (entropy/emacs-generate-new-buffer
              (format " *eemacs/batch--make-extra-utils/%s*" name))
     :default-directory path
     :after
     (entropy/emacs-message-do-message
      "[%s]: %s%s%s%s%s%s"
      (green  "OK")
      (green "Make ")
      (yellow name)
      (green " successfully with cmd ")
      (yellow "`%s' " cmdstr)
      (green "in path: ")
      (yellow path))
     :error
     (entropy/emacs-message-do-error
      "[%s]: %s%s%s%s%s%s\n%s"
      (red  "ERR")
      (red "Make ")
      (yellow name)
      (red " fatal with cmd ")
      (yellow "`%s' " cmdstr)
      (red "in path: ")
      (yellow path)
      (red (with-current-buffer $sentinel/destination
             (buffer-substring-no-properties
              (point-min) (point-max)))))
     :cleanup
     (when (buffer-live-p $sentinel/destination)
       (kill-buffer $sentinel/destination)))))

;; ***** liberime

(defun entropy/emacs-batch--make-dylib/liberime ()
  (let ((emver (format "EMACS_MAJOR_VERSION=%s" emacs-major-version))
        (path  (expand-file-name "liberime"
                                 entropy/emacs-site-lisp-path)))
    (entropy/emacs-message-simple-progress-message
        "Making eemacs liberime"
      (entropy/emacs-batch--make-extra-utils-wrapper
       "liberime:clean" path
       (list "make" emver "EMACS=" "clean"))
      (entropy/emacs-batch--make-extra-utils-wrapper
       "liberime:all" path
       ;; we should disable EMACS macro since if set the makefile of
       ;; liberime will not use the self-maintained `emacs-module.h'
       ;; headers.
       (list "make" emver "EMACS=" "all")))))

;; ***** emacs-rime

(defun entropy/emacs-batch--make-dylib/emacs-rime ()
  (let ((emhroot (format "EMACS_MODULE_HEADER_ROOT=%s"
                         (expand-file-name
                          (number-to-string emacs-major-version)
                          (expand-file-name
                           "annex/emacs-module"
                           entropy/emacs-user-emacs-directory))))
        (path  (expand-file-name "emacs-rime"
                                 entropy/emacs-site-lisp-path)))
    (entropy/emacs-message-simple-progress-message
        "Making eemacs emacs-rime"
      (entropy/emacs-batch--make-extra-utils-wrapper
       "emacs-rime:clean" path
       (list "make" emhroot "clean"))
      (entropy/emacs-batch--make-extra-utils-wrapper
       "emacs-rime:lib" path
       (list "make" emhroot "lib")))))

;; ** interactive

(defvar entropy/emacs-batch--check-packages-done-p nil)
(defun entropy/emacs-batch--check-packages nil
  (if (or entropy/emacs-batch--check-packages-done-p
          ;; NOTE: we have no need to calling pkgs checker under a
          ;; eemacs make-all session, since we've always made "Intall"
          ;; make section be the top of the make-all chains head
          (entropy/emacs-is-make-all-session))
      (unless entropy/emacs-batch--check-packages-done-p
        ;; but we should always init eemacs `use-package' additionals
        ;; at least for macro expansion.
        (entropy/emacs-package-init-use-package))
    (entropy/emacs-package-common-start 'use-full)
    (when entropy/emacs-package-install-success-list
      (entropy/emacs-message-do-error
       (red "Please re-do current make operation \
since we solved deps broken")))))

(when (and noninteractive
           (bound-and-true-p entropy/emacs-batch--make-env-type)
           (entropy/emacs-ext-main))
  (let ((type entropy/emacs-batch--make-env-type))
    (cond
     ((equal type "Install")
      (entropy/emacs-batch--prompts-for-ext-install-section
       (entropy/emacs-package-common-start 'use-full)))
     ((member type (list "Compile" "Compile-Dump"))
      ;; we must check all depedencies firstly while compile
      (entropy/emacs-batch--check-packages)
      (entropy/emacs-batch--prompts-for-byte-compile-eemacs-internal
       (unless (and (entropy/emacs-getenv-eemacs-env "EEMACS_MAKE_ALL")
                    (not (equal type "Compile-Dump")))
         (entropy/emacs-batch--do-bytecompile-eemacs-core t))
       (entropy/emacs-batch--do-bytecompile-eemacs-core)))
     ((equal type "Compile-Clean")
      (entropy/emacs-batch--prompts-for-byte-compile-clean-eemacs-internal
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
      (entropy/emacs-batch--check-packages)
      (entropy/emacs-batch--prompts-for-native-compile
       ;; we should take eemacs eln path as top to store the generations
       (entropy/emacs-native-comp-eln-load-path-set 'reset)
       (entropy/emacs-batch--native-compile-package-dir)))
     ((equal type "Liberime")
      (entropy/emacs-batch--with-prompts-msg
          "Section for compiling emacs rime facilities"
        (entropy/emacs-batch--make-dylib/emacs-rime)
        (entropy/emacs-batch--make-dylib/liberime)))
     (t
      (entropy/emacs-message-do-error
       (red (format "Unknown making type '%s'" type)))))))

;; * provide
(provide 'entropy-emacs-batch)
