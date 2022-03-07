;;; entropy-emacs-tools.el --- entropy-emacs toolbox
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-tools.el
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
;; Tool-box for `entropy-emacs', include web-search, web-viewer,
;; rss-feed and dict translation features, also of project manager
;; utilities.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:
;; ** require
(require 'subr-x)

;; ** gatherd for minor tools
;; *** openwith external apps
;; **** openwith config
(use-package openwith
  :if (and sys/is-graphic-support
           (if sys/linuxp
               (executable-find "setsid")
             t))
  :commands openwith-make-extension-regexp
  :init

  (entropy/emacs-lazy-initial-advice-before
   (find-file switch-to-buffer)
   "enable-native-openwith-mode" "enable-native-openwith-mode"
   prompt-echo
   :pdumper-no-end t
   (openwith-mode))

  :config

  (eval-when-compile (require 'rx))
  (eval-and-compile
    (setq openwith-associations
          (list
           (list
            (concat "^.*\\.\\("
                    (eval
                     `(rx
                       (or
                        ,@'(
                            ;; audio & video files
                            "mpg" "mpeg" "mp3" "mp4" "rmvb"
                            "avi" "wmv" "wav" "mov" "flv"
                            "ogm" "ogg" "mkv" "m4a" "flac" "aac" "ape"
                            ;; documents
                            "pdf" "djvu" "odt"
                            (regex "docx?") (regex "xslx?") (regex "pptx?")
                            ;; archive type
                            "zip" "7z" "xz" "rar" "bzip2"
                            "tgz" "txz" "t7z" "tbz"
                            ;; NOTE: do not set 'tar\..+' regex since
                            ;; the 'tar.sig' suffix is used for emacs
                            ;; internal IO like `package-intall'.
                            (regex "tar\\.\\(gz\\|xz\\|bz\\)")
                            "bak"
                            ))))
                    "\\)$")
            ;; we use xdg-open(linux) and start(windows) as default mime handler
            (cond (sys/is-linux-and-graphic-support-p
                   "xdg-open")
                  (sys/is-wingroup-and-graphic-support-p
                   "start"))
            '(file)))))

  (defun __ya/openwith-open-unix (command arglist)
    "like `openwith-open-unix' but use `start-process' to open the
external COMMAND with ARGLIST."
    (let* ((process-connection-type t)
           (proc-name (format "openwith-process_%s:%s"
                              command
                              (mapconcat 'identity arglist "_")))
           (proc-buff (generate-new-buffer "*openwith-process*"))
           (proc-sentinel
            (lambda (proc _event)
              (let ((proc-name (process-name proc))
                    (proc-buffer (process-buffer proc))
                    (proc-status (process-status proc))
                    (error-pred (lambda (proc-name proc-buffer)
                                  (pop-to-buffer proc-buffer)
                                  (let ((debug-on-error nil))
                                    (error "openwith file handler for process <%s> exited with fatal"
                                           proc-name)))))
                (cond ((and (eq 'exit proc-status)
                            (not (= 0 (process-exit-status proc))))
                       (funcall error-pred proc-name proc-buffer))
                      ((member proc-status '(stop signal))
                       (funcall error-pred proc-name proc-buffer))
                      ((and (eq 'exit proc-status)
                            (= 0 (process-exit-status proc)))
                       (when (buffer-live-p proc-buffer)
                         (let ((kill-buffer-hook nil))
                           (kill-buffer proc-buffer)))
                       (message "openwith file handler for process <%s> open sucessfully"
                                proc-name)))))))
      (set-process-sentinel
       (apply 'start-process
              proc-name proc-buff
              "setsid" "-w" command
              arglist)
       proc-sentinel)))
  (advice-add 'openwith-open-unix :override #'__ya/openwith-open-unix)

  (defun __ya/openwith-open-windows (command arglist)
    "Like `openwith-open-windows' bug use support external COMMAND
with ARGLIST."
    (let* (_)
      (w32-shell-execute
       "open"
       command
       (mapconcat 'shell-quote-argument arglist " "))))
  (advice-add 'openwith-open-windows :override #'__ya/openwith-open-windows)

  (setq entropy/emacs-find-file-judge-fllename-need-open-with-external-app-core-filter
        (lambda (filename)
          (let ((assocs openwith-associations)
                (file filename)
                oa)
            (catch :exit
              ;; do not use `dolist' here, since some packages (like cl)
              ;; temporarily unbind it
              (while assocs
                (setq oa (car assocs)
                      assocs (cdr assocs))
                (when (let
                          ;; we must ensure `case-fold-search' since the
                          ;; extenxion have uppercaes variants
                          ((case-fold-search t))
                        (string-match-p (car oa) file))
                  (throw :exit t)))))))

  (defun __ya/openwith-file-handler (operation &rest args)
    "Like `openwith-file-handler' but enhanced"
    (catch :exit
      (when (and openwith-mode (not (buffer-modified-p)) (zerop (buffer-size)))
        (let ((assocs openwith-associations)
              (file (car args))
              oa)
          ;; do not use `dolist' here, since some packages (like cl)
          ;; temporarily unbind it
          (while assocs
            (setq oa (car assocs)
                  assocs (cdr assocs))
            (when (let
                      ;; we must ensure `case-fold-search' since the
                      ;; extenxion have uppercaes variants
                      ((case-fold-search t))
                    (string-match-p (car oa) file))
              (let ((params (mapcar (lambda (x) (if (eq x 'file) file x))
                                    (nth 2 oa))))
                (when (or (not openwith-confirm-invocation)
                          (y-or-n-p (format "%s %s? " (cadr oa)
                                            (mapconcat #'identity params " "))))
                  (if (eq system-type 'windows-nt)
                      (openwith-open-windows (cadr oa) params)
                    (openwith-open-unix (cadr oa) params))
                  (kill-buffer nil)
                  (when (bound-and-true-p recentf-mode)
                    (recentf-add-file file))
                  ;; quit procedure while matched rules
                  (top-level)))))))
      ;; when no association was found, relay the operation to other handlers
      (let ((inhibit-file-name-handlers
             (cons 'openwith-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args))))
  (advice-add 'openwith-file-handler :override #'__ya/openwith-file-handler)

  )

;; **** Function manually
;; ***** Open in desktop manager
(when sys/is-graphic-support
  (defun entropy/emacs-tools-show-in-desktop (&optional dpath)
    "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
 This command be called when in a file or in `dired'.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2017-12-23"
    (interactive)
    (let (($path (if (not dpath)
                     (if (buffer-file-name)
                         (file-name-nondirectory (buffer-file-name))
                       default-directory )
                   dpath)))
      (cond
       (sys/is-wingroup-and-graphic-support-p
        (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
       (sys/is-mac-and-graphic-support-p
        (if (eq major-mode 'dired-mode)
            (let (($files (dired-get-marked-files )))
              (if (eq (length $files) 0)
                  (shell-command
                   (concat "open " default-directory ))
                (shell-command
                 (concat "open -R " (car (dired-get-marked-files))))))
          (shell-command
           (concat "open -R " $path))))
       (sys/is-linux-and-graphic-support-p
        (shell-command "gio open .")
        ;; gio open was the suggested command for now [2018-01-03 Wed 04:23:17]
        ;;
        ;; 2013-02-10 (shell-command "xdg-open .")  sometimes froze
        ;; emacs till the folder is closed. eg with nautilus
        ))))
  )

;; ***** Open in terminal
(defun entropy/emacs-tools-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2017-10-09"
  (interactive)
  (let ((wcdir
         (expand-file-name default-directory))
        proc-obj
        (proc-sentinel
         (lambda (proc _event)
           (let ((proc-name (process-name proc))
                 (proc-buffer (process-buffer proc))
                 (proc-status (process-status proc))
                 (error-pred (lambda (proc-name proc-buffer)
                               (pop-to-buffer proc-buffer)
                               (error "eemacs-linux-terminal-popup <%s> exited with fatal"
                                      proc-name))))
             (cond ((and (eq 'exit proc-status)
                         (not (= 0 (process-exit-status proc))))
                    (funcall error-pred proc-name proc-buffer))
                   ((member proc-status '(stop signal))
                    (funcall error-pred proc-name proc-buffer))
                   ((and (eq 'exit proc-status)
                         (= 0 (process-exit-status proc)))
                    (message "eemacs-linux-terminal-popup <%s> open sucessfully"
                             proc-name)
                    (when (buffer-live-p proc-buffer)
                      (let ((kill-buffer-hook nil))
                        (kill-buffer proc-buffer)))))))))
    (cond
     (sys/win32p
      ;;(message "Microsoft Windows not supported bash shell, and we use cmd instead")
      (let* (($path-o (if (string-match-p "^~/" wcdir)
                          (replace-regexp-in-string
                           "^~"
                           (expand-file-name "~")
                           wcdir)
                        wcdir))
             ($path-backslash (replace-regexp-in-string "/" "\\" $path-o t t))
             ($path (concat "\"" $path-backslash "\"")))
        (if entropy/emacs-microsoft-windows-unix-emulator-terminal-enable
            (if (string-match-p "msys2_shell" entropy/emacs-microsoft-windows-unix-emulator-terminal)
                ;; using msys2 mintty
                (w32-shell-execute
                 "open"
                 entropy/emacs-microsoft-windows-unix-emulator-terminal
                 (concat
                  (completing-read "Choosing shell type: "
                                   '("-mingw32"
                                     "-mingw64"
                                     "-msys2")
                                   nil t)
                  " -where "
                  $path))
              ;; using git-for-windows terminal
              (w32-shell-execute "open" entropy/emacs-microsoft-windows-unix-emulator-terminal))

          ;; using cmd
          (w32-shell-execute "open" "cmd" $path))))

     (sys/is-mac-and-graphic-support-p
      (let ((process-connection-type nil))
        (start-process
         "" nil
         "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal"
         wcdir)))

     (sys/is-linux-and-graphic-support-p
      (let ((time-str
             ;; unique process id
             (format "%s.%s"
                     (format-time-string "%Y%m%d%H%M%S")
                     (random 10000)))
            ;; preserve `process-connection-type' to t since while
            ;; nil some unexpected occasion occurred like env
            ;; inherited issue.
            (process-connection-type t)
            (exec-and-arg
             (or
              ;; ----- use gpu accelerated terminal
              (and (executable-find "kitty")
                   `("kitty" "-d" ,wcdir))
              ;; EEMACS_BUG: alacritty may cause multi process spwan invoked why?
              ;; - obviously in debian 11?
              ;; - no bug in archlinux?
              (and (executable-find "alacritty")
                   `("alacritty" "--working-directory" ,wcdir))

              ;; ----- use DE based terminal
              (and (executable-find "gnome-terminal")
                   `("gnome-terminal" ,wcdir))
              (and (executable-find "konsole")
                   `("konsole "))

              ;; ----- fallback to use xterm
              (and (executable-find "uxterm")
                   ("uxterm"))
              (and (executable-find "xterm")
                   ("xterm"))
              )))
        (unless exec-and-arg
          (error "Can not find proper terminal emulator on your system."))
        (setq proc-obj
              (apply 'start-process
                     (format "eemacs-linux-terminal-popup_<%s>_%s"
                             wcdir
                             time-str)
                     (get-buffer-create
                      (format " *eemacs-linux-terminal-popup-proc-buffer_<%s>.%s* "
                              wcdir
                              time-str))
                     ;; use setsid to creat a new controlling terminal so that
                     ;; emacs not kill it while open since: gvfs-open and
                     ;; xdg-open return before their children are done
                     ;; working. Emacs might kill their controlling terminal when
                     ;; this happens, killing the children, and stopping refer
                     ;; application from opening properly. (see
                     ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
                     ;; for more details)
                     (append '("setsid" "-w") exec-and-arg)))
        (set-process-sentinel proc-obj proc-sentinel))
      )
     )
    ))

(when sys/win32p
  (defun entropy/emacs-tools-cmd()
    (interactive)
    (if entropy/emacs-Cmder-enable
        (let (($path default-directory))
          (w32-shell-execute "open" entropy/emacs-Cmder-path
                             (replace-regexp-in-string "/" "\\" $path t t)))
      (let (($path default-directory))
        (w32-shell-execute "open" "cmd"
                           (replace-regexp-in-string "/" "\\" $path t t))))))

;; ***** hydra hollow instance

(entropy/emacs-lazy-initial-advice-before
 (dired-mode) "eemacs-tools-open-in-extapp-hydra-hollow-init"
 "eemacs-tools-open-in-extapp-hydra-hollow-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
  'dired-mode '(dired dired-mode-map)
  '("Misc."
    (("M-=" entropy/emacs-tools-show-in-desktop
      "Show current file in desktaop"
      :enable sys/is-graphic-support :exit t :map-inject t)))))

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "eemacs-tools-external-terminal-hydra-hollow-init"
 "eemacs-tools-external-terminal-hydra-hollow-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define+
  'eemacs-basic-config-core nil
  '("Eemacs Basic Core"
    (("C-;" entropy/emacs-tools-open-in-terminal
      "Open the current location in a new terminal window"
      :enable sys/is-graphic-support :exit t :global-bind t)
     ("C--" entropy/emacs-tools-cmd
      "Open the current location in a new windows cmdproxy"
      :enable sys/win32p :exit t :global-bind t)))))

;; **** entropy-open-with
(use-package entropy-open-with
  :if (and sys/is-graphic-support
           (if (or sys/linuxp
                   sys/wsl2-env-p)
               (executable-find "setsid")
             t))
  :ensure nil
  :commands (entropy/open-with-dired-open
             entropy/open-with-buffer
             entropy/open-with-dired-open/with-wsl2-native)

  :eemacs-tpha
  (((:enable
     sys/is-graphic-support
     :defer (:data
             (:adfors
              (dired-mode)
              :adtype after
              :pdumper-no-end t))))
   ("WI&BUF"
    (("M-1" entropy/open-with-buffer "Buffer open with portable apps"
      :enable t :exit t :eemacs-top-bind t))))

  :eemacs-mmphca
  (((:enable
     (or sys/is-graphic-support
         sys/wsl2-env-p)
     :defer (:data
             (:adfors
              (dired-mode)
              :adtype after
              :pdumper-no-end t)))
    (dired-mode (dired dired-mode-map)))
   ("Misc."
    (("M-RET" entropy/open-with-dired-open "Dired open with portable apps"
      :enable
      (when (or sys/is-graphic-support
                sys/wsl2-env-p)
        t)
      :exit t :map-inject t)
     ("C-<return>" entropy/open-with-dired-open/with-wsl2-native
      "Dired open with portable apps with windows native apps in wsl2 env."
      :enable
      sys/wsl2-env-p
      :exit t :map-inject t))))

  :config
  (defun entropy/emacs-tools--open-with-port-stuffs-around (oldfunc &rest arg-rest)
    "when in `entropy/emacs-web-development-environment' advice
`entropy/open-with-port' for prevent open url with specific
development web-browser."
    (let ((entropy/emacs-web-development-environment nil))
      (apply oldfunc arg-rest)))
  (entropy/emacs-lazy-load-simple entropy-open-with
    (advice-add 'entropy/open-with-port
                :around
                #'entropy/emacs-tools--open-with-port-stuffs-around))

  (defun entropy/open-with-dired-open/with-wsl2-native (&optional prefix)
    "Like `entropy/open-with-dired-open' but wrapped with
`entropy/open-with-microsoft-native-when-wsl2-p' binding as t."
    (interactive "P")
    (let ((entropy/open-with-microsoft-native-when-wsl2-p
           t))
      (funcall 'entropy/open-with-dired-open
               prefix)))
  )

;; *** vertical center display
(defun entropy/emacs-tools-vertical-center ()
  "Just vertical-center buffer without further operation supplied
like `recenter-top-bottom'."
  (interactive)
  (recenter-top-bottom '(middle)))

(defun entropy/emacs-tools-vertical-to-bottom (&optional arg)
  "Just vertical-bottom buffer without further operation supplied
like `recenter-top-bottom', if optional arg is non-nil then we
vertical-top buffer."
  (interactive "P")
  (if arg
      (recenter-top-bottom 0)
    (recenter-top-bottom -1)))

(entropy/emacs-lazy-initial-advice-before
 (switch-to-buffer find-file)
 "eemacs-buffer-window-recenter-hydra-hollow-init"
 "eemacs-buffer-window-recenter-hydra-hollow-init"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'eemacs-center-line-position nil
  '("Eemacs Center Line"
    (("C-l" entropy/emacs-tools-vertical-center "Vertical center buffer"
      :enable t :exit t :global-bind t)
     ("C-M-l" entropy/emacs-tools-vertical-to-bottom "Recenter to window bottom (use prefix to top)"
      :enable t :exit t :global-bind t))))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("WI&BUF"
    (("i l"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'eemacs-center-line-position))
      "Center buffer line"
      :enable t :exit t)))))

;; *** beacon cursor blanking
(use-package beacon
  :preface
  (defvar entropy/emacs-tools-beacon-blink-ignore nil
    "Non-nill to give out an ignore condition signal to
`beacon-blink' which press it off any way.")

  (defvar entropy/emacs-tools-beacon-blink-top-hook nil
    "List of functions to run before `beacon-blink', you may want
to use it as enable `entropy/emacs-tools-beacon-blink-ignore'
dynamically.")
  :commands (beacon-mode beacon-blink)
  :init
  (defun entropy/emacs-tools--beacon-blink-advice
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      ;; reset the ignore pattern since previous set by
      ;; `entropy/emacs-tools-beacon-blink-top-hook'
      (setq entropy/emacs-tools-beacon-blink-ignore nil)
      (run-hooks 'entropy/emacs-tools-beacon-blink-top-hook)
      (unless (or (not (fboundp 'beacon-blink))
                  entropy/emacs-tools-beacon-blink-ignore
                  ;; Disable beacon blink in mpc referred buffer for performance issue
                  (member major-mode '(mpc-mode
                                       mpc-tagbrowser-mode
                                       mpc-songs-mode
                                       mpc-status-mode
                                       mpc-tagbrowser-dir-mode
                                       )))
        (beacon-blink)
        rtn)))
  (when (bound-and-true-p entropy/emacs-init-beacon-blink)
    (advice-add 'windmove-do-window-select :around
                #'entropy/emacs-tools--beacon-blink-advice)
    (advice-add 'recenter-top-bottom :around
                #'entropy/emacs-tools--beacon-blink-advice)))

;; *** visual-regexp
;;
;; Visual-regexp for Emacs is like replace-regexp, but with live
;; visual feedback directly in the buffer.
(use-package visual-regexp
  :commands (vr/replace vr/query-replace)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (switch-to-buffer find-file)
                        :adtype
                        after
                        :pdumper-no-end t)))
    (visual-regexp))
   ("Basic"
    (("C-c r" vr/replace "Regexp-replace with live visual feedback"
      :enable t :exit t :global-bind t)
     ("C-c q" vr/query-replace "Use vr/query-replace like you would use query-replace-regexp"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (switch-to-buffer find-file)
                        :adtype
                        after
                        :pdumper-no-end t))))
   ("Utils"
    (("u v"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'visual-regexp))
      "Visual regexp query/replace"
      :enable t :exit t))))
  :config
  (dolist (el '(vr--do-replace vr--perform-query-replace vr--interactive-get-args))
    (advice-add el :around #'entropy/emacs-case-fold-focely-around-advice)))


;; *** ialign
(use-package ialign
  :commands (ialign)
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (find-file switch-to-buffer)
                        :adtype
                        after
                        :pdumper-no-end t))))
   ("Utils"
    (("C-c i" ialign "Interactively align region"
      :enable t :exit t :global-bind t)))))

;; *** Firefox edit use emacs
;;
;;    An extension for Google Chrome browser that allows you to edit
;;    text areas of the browser in Emacs. It's similar to Edit with
;;    Emacs, but has some advantages as below with the help of
;;    websocket.
;;
;;    The input on Emacs is reflected to the browser instantly and
;;    continuously.
;;
;;    You can use both the browser and Emacs at the same time. They
;;    are updated to the same content bi-directionally.
;;
;;    Since v2.0.0, Atomic Chrome for Emacs supports Ghost Text as
;;    browser extension, bringing compatibility with Firefox, too.
(use-package atomic-chrome
  :commands (atomic-chrome-start-server)
  :preface
  (defun entropy/emacs-tools-toggle-atomic-chrome ()
    (interactive)
    (require 'atomic-chrome)
    (if atomic-chrome-server-atomic-chrome
        (atomic-chrome-stop-server)
      (atomic-chrome-start-server)))

  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype
                        hook
                        :pdumper-no-end t))))
   ("Utils"
    (("u a" entropy/emacs-tools-toggle-atomic-chrome
      "Toggle websocket server for atomic-chrome"
      :enable t :exit t
      :toggle atomic-chrome-server-atomic-chrome))))

  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode))))


;; *** Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (after-change-major-mode-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (discover-my-major))
   ("Discover major-mode map"
    (("C-h M-m" discover-my-major "Create a makey popup listing all major-mode"
      :enable t :exit t :global-bind t)
     ("C-h M-M" discover-my-mode "Create a makey popup listing all minor-mode"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (after-change-major-mode-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Basic"
    (("b d"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'discover-my-major))
      "Discover modes key-bindings"
      :enable t))))

  :config

  (defvar entropy/emacs-tools--dmm-sections-log nil)

  (defun entropy/emacs-tools--dmm-prune-sections (dmm-sections)
    (setq entropy/emacs-tools--dmm-sections-log nil)
    (let ((dmm-sections-copy (copy-sequence dmm-sections))
          (rules '("next-line" "previous-line" "left-char" "right-char"
                   "beginning-of-line" "end-of-line"))
          rtn)
      (setq entropy/emacs-tools--dmm-sections-log dmm-sections-copy)
      (dolist (section dmm-sections-copy)
        (let* ((group-name (car section))
               (bindings (cdr section))
               (cpbdins (copy-sequence bindings))
               cache)
          (dolist (binding cpbdins)
            (unless (or (entropy/emacs-map-string-match-p
                         (cdr binding) rules)
                        (entropy/emacs-map-string-match-p
                         (car binding) rules))
              (push binding cache)))
          (push (append (list group-name) (nreverse cache)) rtn)))
      (nreverse rtn)))

  (defun entropy/emacs-tools--dmm-adv-for-section-builder (orig-func &rest orig-args)
    (let ((dmm-sections (apply orig-func orig-args)))
      (entropy/emacs-tools--dmm-prune-sections dmm-sections)))

  (advice-add 'dmm/descbinds-all-sections
              :around
              #'entropy/emacs-tools--dmm-adv-for-section-builder))

;; *** Self functions
;; **** split window horizontally for comfortable width setting
(defun entropy/emacs-tools-horizonal-split-window ()
  "Split the single window to two windows with different size
which determined by the scale count 0.3 "
  (interactive)
  (when (bound-and-true-p entropy/emacs-wc-center-window-mode)
    (when (fboundp 'entropy/emacs-wc-center-window-mode)
      (entropy/emacs-wc-center-window-mode 0)))
  (if (> (length (window-list)) 1) (delete-other-windows))
  (progn
    (entropy/emacs-no-same-buffer-split-window-horizontally
     (ceiling (* 0.18
                 (frame-width))))
    (other-window 1)))

;; **** show time
(defun entropy/emacs-tools-time-show ()
  "Show current time with date information also."
  (interactive)
  (let ((time (format-time-string "%Y-%m-%d %a %H:%M:%S")))
    (message "Now is %s" time)))

;; **** hydra hollow instance

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "eemacs-tools-self-functions-hydra-hollow-init"
 "eemacs-tools-self-functions-hydra-hollow-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define+
  'eemacs-basic-config-core nil
  '("Eemacs Basic Core"
    (("<f12>" entropy/emacs-tools-time-show
      "Show current time with date information also"
      :enable t :exit t :global-bind t))))
 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("WI&BUF"
    (("C-x M-1" entropy/emacs-tools-horizonal-split-window
      "Split the single window to two windows with different size"
      :enable t :exit t :global-bind t))))
 )

;; *** encoding and end-of-line conversation
(defun entropy/emacs-tools-dos2unix-internal ()
  "Exchange the buffer end-of-line type to unix sytle."
  (interactive)
  (entropy/cl-backup-file (buffer-file-name))
  (revert-buffer-with-coding-system 'dos t)
  (set-buffer-file-coding-system 'unix)
  (if buffer-read-only
      (setq buffer-read-only nil))
  (save-buffer)
  (revert-buffer nil 'revert-without-query)
  (setq buffer-read-only t))

(defun entropy/emacs-tools-save-buffer-as-utf8-internal (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (entropy/cl-backup-file (buffer-file-name))
  (revert-buffer-with-coding-system coding-system)
  (if (yes-or-no-p (format "Does encoding with '%s' display correctly? " coding-system))
      (progn
        (set-buffer-file-coding-system 'utf-8-unix)
        (if buffer-read-only
            (setq buffer-read-only nil))
        (save-buffer)
        (revert-buffer nil 'revert-without-query)
        (setq buffer-read-only t))
    (user-error "Please try corrected encoding! ")))

(defun entropy/emacs-tools-dos2unix-external (&optional no-backup)
  "Exchange the buffer end-of-line type to unix sytle."
  (interactive)
  (if (executable-find "dos2unix")
      (progn
        (setq entropy/cl-dos2unix-shell-command
              (concat "dos2unix " (concat " " "\"" buffer-file-name "\"")))
        (unless no-backup
          (entropy/cl-backup-file (buffer-file-name)))
        (shell-command entropy/cl-dos2unix-shell-command))
    (message "error: Can't find dos2unix executeble program in your PATH")))

(defun entropy/emacs-tools-save-buffer-as-utf8-external (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (if (yes-or-no-p (format "Does encoding with '%s' display correctly? " coding-system))
      (if (and (executable-find "iconv")
               (executable-find "mv"))
          (let* ((fname (if (buffer-file-name) (buffer-file-name) (error "Buffer without exist file!")))
                 ($dir (file-name-directory fname))
                 ($item (file-name-nondirectory fname))
                 ($trans-item (concat $item ".entropy-with-utf8"))
                 (trans-file (expand-file-name $trans-item $dir))
                 (iconv_cmd (concat "iconv -f " (symbol-name coding-system)
                                    " "
                                    "-t utf-8"
                                    " "
                                    (shell-quote-argument fname)
                                    " > "
                                    (shell-quote-argument trans-file)))
                 (rm-cmd (concat "rm " (shell-quote-argument fname)))
                 (mv-cmd (concat "mv " (shell-quote-argument trans-file) " " (shell-quote-argument fname)))
                 iconv-cbk)
            (if (yes-or-no-p (format "Do you confirm transfer this file to '%s' ?" "utf-8-unix"))
                (progn
                  (entropy/cl-backup-file fname)
                  (setq iconv-cbk (shell-command-to-string iconv_cmd))
                  (if (and (file-exists-p trans-file)
                           (equal iconv-cbk ""))
                      (progn
                        (kill-buffer)
                        (shell-command rm-cmd)
                        (shell-command mv-cmd)
                        (find-file fname))
                    (error "Iconv failed!")))
              (message "Bye Bye -v- ✌")))
        (message "error: Can't find 'iconv' or 'mv' executable program in your path."))
    (user-error "Please try corrected encoding! ")))


(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-hydra-hollow-call-before-hook)
 "eemacs-dos2unix-hydra-hollow-init" "eemacs-dos2unix-hydra-hollow-init"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'tools-coding-refactor nil
  '("DOS2UNIX"
    (("d i" entropy/emacs-tools-dos2unix-internal
      "Exchange the buffer end-of-line type to unix sytle internally"
      :enable t :exit t)
     ("d e" entropy/emacs-tools-dos2unix-external
      "Exchange the buffer end-of-line type to unix sytle externally"
      :enable t :exit t))
    "Convert To Utf-8"
    (("u i" entropy/emacs-tools-save-buffer-as-utf8-internal
      "Revert a buffer with 'CODING-SYSTEM' and save as UTF-8 internally"
      :enable t :exit t)
     ("u e" entropy/emacs-tools-save-buffer-as-utf8-external
      "Revert a buffer with 'CODING-SYSTEM' and save as UTF-8 externally"
      :enable t :exit t))))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Utils"
    (("u b"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'tools-coding-refactor))
      "Dos2unix and UTF-8 convertor"
      :enable t :exit t)))))

;; *** Foreign language realtime translation

(defvar entropy/emacs-tools-dict-sticker entropy/emacs-dictionary-backend)

(defun entropy/emacs-tools-dict-search-at-point ()
  (interactive)
  (let ()
    (pcase entropy/emacs-tools-dict-sticker
      ('sdcv (call-interactively 'entropy/sdcv-search-at-point-tooltip))
      ('youdao (call-interactively 'entropy/emacs-tools-youdao-search-at-point))
      ('google (call-interactively 'entropy/emacs-tools-google-translate-at-point-direct-en-CN))
      ('bing (call-interactively 'entropy/emacs-tools-bing-dict-brief-direct)))))

(defun entropy/emacs-tools-dict-search-with-prompt ()
  (interactive)
  (let ()
    (pcase entropy/emacs-tools-dict-sticker
      ('sdcv (call-interactively 'entropy/sdcv-search-input-adjacent))
      ('youdao (call-interactively 'youdao-dictionary-search-from-input))
      ('google (call-interactively 'entropy/emacs-tools-google-translate-prompt-direct-en-CN))
      ('bing (call-interactively 'entropy/emacs-tools-bing-dict-brief-prompt)))))

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "entropy-dict-search-hydra-hollow-init"
 "entropy-dict-search-hydra-hollow-init"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'eemacs-dict-search nil
  '("Basic"
    (("C-f" entropy/emacs-tools-dict-search-at-point
      "Search dict for thing at current point"
      :enable t :eemacs-top-bind t :exit t)
     ("M-f" entropy/emacs-tools-dict-search-with-prompt
      "Search dict for user specified with prompts"
      :enable t :eemacs-top-bind t :exit t)
     ("C-x y" entropy/cndt-query "Simple Translate Chinese at point"
      :enable t :exit t :global-bind t)
     ("t"
      (let* ((candis '("sdcv" "youdao" "bing" "google"))
             (chosen (completing-read
                      "Toggle dict backend to: "
                      candis nil t "sdcv")))
        (setq entropy/emacs-tools-dict-sticker (intern chosen)))
      "Toggle entropy-emacs dict search backend"
      :enable t :exit t))
    "Entropy Sdcv"
    (("s t" entropy/sdcv-toggle-backend "Toggle dict-backends"
      :enable t :exit t)
     ("s s" entropy/sdcv-toggle-show-tooltip-method "Toggle show method"
      :enable t :exit t)
     ("s a" entropy/sdcv-autoshow-mode "Auto translate"
      :enable t :exit nil
      :toggle
      (if (bound-and-true-p entropy/sdcv-autoshow-mode)
          t
        nil)))))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Basic"
    (("b o"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'eemacs-dict-search))
      "Dict search with sets of backends "
      :enable t :exit t)))))

;; **** yoaudao-dictionary
(use-package youdao-dictionary
  :commands
  (youdao-dictionary-mode
   youdao-dictionary-play-voice-at-point
   youdao-dictionary-play-voice-from-input
   youdao-dictionary-play-voice-of-current-word
   youdao-dictionary-search
   youdao-dictionary-search-and-replace
   youdao-dictionary-search-at-point
   youdao-dictionary-search-at-point+
   youdao-dictionary-search-at-point-tooltip
   youdao-dictionary-search-at-point-posframe
   youdao-dictionary-search-from-input)

  :preface
  (defun entropy/emacs-tools-youdao-search-at-point ()
    (interactive)
    (call-interactively
     (if (entropy/emacs-posframe-adapted-p)
         'youdao-dictionary-search-at-point-posframe
       'youdao-dictionary-search-at-point+)))

  :init
  (setq url-automatic-caching t)
  (defalias 'ydi 'youdao-dictionary-search-from-input))

;; **** google-translate
(use-package google-translate
  :commands (google-translate-translate
             entropy/emacs-tools-google-translate-at-point-direct-en-CN
             entropy/emacs-tools-google-translate-prompt-direct-en-CN)
  :init
  (when entropy/emacs-google-translate-toggle-patched-in-china
    ;;    Because google-translate has been block in china, so can use below variable for preventing
    ;;    this problem. And this solution was from `https://emacs-china.org/t/topic/2808/17'
    (eval-after-load 'google-translate-core
      '(setq google-translate-base-url "http://translate.google.cn/translate_a/single"
             google-translate-listen-url "http://translate.google.cn/translate_tts"))
    (eval-after-load 'google-translate-tk
      '(setq google-translate--tkk-url "http://translate.google.cn/"))

    ;; Since we use google china ip, we can directly use curl to
    ;; retrieve the result to speedup qury search which do not need to
    ;; use interal proxy any way.
    (when (executable-find "curl")
      (setq google-translate-backend-method
            'curl)))

  :config

  ;; HACK
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun google-translate-json-suggestion (json)
    "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion.

Notice: this function has redefined for fix the bug for json
parsing bug of 'Args out of range: [], 1'.

Patching method getted from
https://github.com/atykhonov/google-translate/issues/98#issuecomment-562870854
"
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil)))

  ;; HACK
  ;; EEMACS_BUG: https://github.com/atykhonov/google-translate/issues/137
  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (defun entropy/emacs-tools-google-translate-at-point-direct-en-CN ()
    (interactive)
    (let* ((langs '("auto" "zh-CN"))
           (source-language (car langs))
           (target-language (cadr langs))
           (bounds nil))
      (google-translate-translate
       source-language target-language
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (or (and (setq bounds (bounds-of-thing-at-point 'word))
                  (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (error "No word at point."))))))

  (defun entropy/emacs-tools-google-translate-prompt-direct-en-CN ()
    (interactive)
    (setq google-translate-translation-direction-query
          (if (use-region-p)
              (google-translate--strip-string
               (buffer-substring-no-properties (region-beginning) (region-end)))
            (current-word t t)))

    (setq google-translate-current-translation-direction 0)

    (let* ((text (let ((rtn
                        (read-string
                         (if google-translate-translation-direction-query
                             (format "Input text (default-> %s): " google-translate-translation-direction-query)
                           "Input text: "))))
                   (if (string= "" rtn)
                       google-translate-translation-direction-query
                     rtn)))
           (source-language "auto")
           (target-language "zh-CN"))
      (when (null source-language)
        (setq source-language (google-translate-read-source-language)))
      (when (null target-language)
        (setq target-language (google-translate-read-target-language)))
      (google-translate-translate source-language target-language text))))


;; **** bing-dict
(use-package bing-dict
  :commands (bing-dict-brief
             bing-dict-brief-cb
             entropy/emacs-tools-bing-dict-brief-prompt
             entropy/emacs-tools-bing-dict-brief-direct)
  :config
  (defun entropy/emacs-tools-bing-dict-brief-prompt (word)
    "Show the explanation of WORD from Bing in the echo area."
    (interactive
     (let* ((default (if (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))
                       (let ((text (thing-at-point 'word)))
                         (if text (substring-no-properties text)))))
            (prompt (if (stringp default)
                        (format "Search Bing dict (default \"%s\"): " default)
                      "Search Bing dict: "))
            (string (read-string prompt nil 'bing-dict-history default)))
       (list string)))
    (save-match-data
      (url-retrieve (concat bing-dict--base-url
                            (url-hexify-string word))
                    'bing-dict-brief-cb
                    `(,(decode-coding-string word 'utf-8))
                    t
                    t)))

  (defun entropy/emacs-tools-bing-dict-brief-direct (word)
    (interactive
     (let* ((default (if (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))
                       (let ((text (thing-at-point 'word)))
                         (if text
                             (substring-no-properties text)
                           (error "No point word found!"))))))
       (list default)))
    (save-match-data
      (url-retrieve (concat bing-dict--base-url
                            (url-hexify-string word))
                    'bing-dict-brief-cb
                    `(,(decode-coding-string word 'utf-8))
                    t
                    t))))
;; **** sdcv
(use-package entropy-sdcv
  :ensure nil
  :commands (entropy/sdcv-search-at-point-tooltip
             entropy/sdcv-search-input-adjacent
             entropy/sdcv-autoshow-mode)
  :init
  (if (null (daemonp))
      (unless (display-graphic-p)
        (setq entropy/sdcv-default-show-tooltip-method 'popup))
    (entropy/emacs-with-daemon-make-frame-done
     'entropy-sdcv
     '(setq entropy/sdcv-default-show-tooltip-method 'popup)
     '(when (entropy/emacs-posframe-adapted-p)
        (setq entropy/sdcv-default-show-tooltip-method 'posframe))))

  (dolist (item '((eww  . eww-mode-hook)  (w3m . w3m-mode-hook)
                  (info . Info-mode-hook) (markdown-mode . markdown-mode-hook)
                  (man  . Man-mode-hook)  (woman . woman-mode-hook)))
    (eval-after-load (car item)
      `(add-hook ',(cdr item) #'entropy/sdcv-autoshow-mode)))

  (add-to-list 'entropy/emacs-solaire-mode-extra-buffer-filters
               #'(lambda (buff)
                   (string-match-p
                    (regexp-quote (buffer-name buff))
                    entropy/sdcv-show-tooltip-buffer-name)))

  :config
  (cond ((executable-find "wd")
         (setq entropy/sdcv-default-query-backend-name 'wudao-command))
        ((or (not (executable-find "sdcv"))
             (not (entropy/sdcv-backends--sdcv-auto-search-dicts)))
         (setq entropy/sdcv-default-query-backend-name 'youdao))))

;; **** chinese dict
(use-package entropy-cn-dict
  :ensure nil
  :commands entropy/cndt-query)

;; *** Log keyboard commands to buffer
;;     Show event history and command history of some or all buffers.
(use-package command-log-mode
  :diminish (command-log-mode . "¢")
  :commands (command-log-mode)
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Utils"
    (("u l" entropy/emacs-tools-command-log-mode
      "Toggle keyboard command logging"
      :enable t :toggle command-log-mode))))
  :init
  (setq command-log-mode-auto-show t)
  (defun entropy/emacs-tools-command-log-mode ()
    (interactive)
    (require 'command-log-mode)
    (if (not (bound-and-true-p command-log-mode))
        (progn
          (command-log-mode 1)
          (unless (window-live-p
                   (get-buffer-window
                    (get-buffer-create " *command-log*")))
            (clm/open-command-log-buffer)))
      (command-log-mode 0)))

  :config
  ;; Remove the initial unneeded hook injector since they should do in
  ;; the mode enable procedure.
  (remove-hook 'post-command-hook 'clm/zap-recent-history)
  (remove-hook 'pre-command-hook 'clm/log-command)
  (remove-hook 'post-self-insert-hook 'clm/recent-history)

  (defun __adv/around/command-log-mode (orig-func &rest orig-args)
    "Around advice for inject/remove proper hook referred to
`command-log-mode'."
    (prog1
        (apply orig-func orig-args)
      (if (bound-and-true-p command-log-mode)
          (progn
            (add-hook 'post-command-hook 'clm/zap-recent-history)
            (add-hook 'pre-command-hook 'clm/log-command)
            (add-hook 'post-self-insert-hook 'clm/recent-history))
        (remove-hook 'post-command-hook 'clm/zap-recent-history)
        (remove-hook 'pre-command-hook 'clm/log-command)
        (remove-hook 'post-self-insert-hook 'clm/recent-history))))
  (advice-add 'command-log-mode
              :around
              #'__adv/around/command-log-mode))

;; *** pomidor A simple and cool pomodoro timer
(use-package pomidor
  :commands (pomidor)
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors
                              (entropy/emacs-after-startup-hook)
                              :adtype hook
                              :pdumper-no-end t))))
   ("Utils"
    (("C-c <f12>" pomidor
      "A simple and cool pomodoro technique timer"
      :enable t :exit t :global-bind t)))))

;; ** Misc
;; *** copy path, url, etc.
(use-package copyit
  :commands
  (copyit-file-as-data-uri
   copyit-file-content
   copyit-file-exif-information
   copyit-file-pathname
   copyit-ssh
   copyit-variable)
  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (entropy/emacs-hydra-hollow-call-before-hook) :adtype hook :pdumper-no-end t)))
    (copyit))
   ("File Copyit"
    (("u" copyit-file-as-data-uri
      "Copy 'FILE-PATH' content as Data URI format"
      :enable t :exit t)
     ("c" copyit-file-content
      "Copy 'FILE-PATH' content"
      :enable t :exit t)
     ("e" copyit-file-exif-information
      "Copy exif-information by 'FILE-PATH'"
      :enable t :exit t)
     ("p" copyit-file-pathname
      "Copy 'FILE-PATH'"
      :enable t :exit t))
    "Misc."
    (("s" copyit-ssh
      "Copy ssh file"
      :enable t :exit t)
     ("v" copyit-variable
      "Copy pretty-printed value 'SYMBOL's variable"
      :enable t :exit t))))
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-hydra-hollow-call-before-hook) :adtype hook :pdumper-no-end t))))
   ("Utils"
    (("u p"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'copyit))
      "Copyit stuff"
      :enable t :exit t)))))

;; *** Emacs startup profiler
(use-package esup
  :commands (esup))

;; *** for generate elisp source file's commentry structure to org file
(use-package entropy-code2org
  :ensure nil
  :commands (entropy/code2org-export-cur-to-README
             entropy/code2org-export-cur-to-html-file
             entropy/code2org-export-cur-to-org-file)
  :preface
  (defun entropy/emacs-tools--before-advice-for-code2org (orig-func &rest _)
    (if (member
         major-mode
         '(emacs-lisp-mode lisp-interaction-mode lisp-mode))
        (with-current-buffer
            (entropy/emacs-elisp-toggle-outline-struct-style t 'modern)
          (funcall orig-func))
      (funcall orig-func)))

  :eemacs-indhca
  (((:enable t :defer (:data
                       (:adfors
                        (prog-mode)
                        :adtype after
                        :pdumper-no-end t)))
    (eemacs-basic-config-core))
   ("Export outline style code buffer into other kinds"
    (("c o" entropy/code2org-export-cur-to-org-file "Generate org file from current code buffer"
      :enable t :exit t)
     ("c h" entropy/code2org-export-cur-to-html-file "Generate html file from current code buffer"
      :enable t :exit t)
     ("c r" entropy/code2org-export-cur-to-README "Generate readme file from current code buffer"
      :enable t :exit t))))

  :config
  (dolist (func '(entropy/code2org-export-cur-to-README
                  entropy/code2org-export-cur-to-html-file
                  entropy/code2org-export-cur-to-org-file))
    (advice-add
     func
     :around
     #'entropy/emacs-tools--before-advice-for-code2org)))


;; ****  require by el2org for generate source to readme which be with the github style md file
(use-package ox-gfm)


;; *** bell ring refer
(setq ring-bell-function 'ignore)

;; *** Goto home dir
(defun entropy/emacs-tools-goto-sys-home ()
  "Open system home folder.

  It's usefully for windows user to quickly switching to 'c:/.../user-name'."
  (interactive)
  (cond
   (sys/win32p
    (let ((home (getenv "USERPROFILE")))
      (dired home)))
   (sys/is-posix-compatible
    (dired "~/"))))

(defalias 'ehome
  'entropy/emacs-tools-goto-sys-home
  "Alias for entropy/emacs-tools-goto-sys-home.")

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-hydra-hollow-call-before-hook)
 "ehome-alias-hydra-hollow-init"
 "ehome-alias-hydra-hollow-init"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Basic"
    (("b h" ehome "Open HOME Directory"
      :enable t :exit t)))))

;; *** visual-ascii-mode

  ;; Show key-binding with readable style instead of ascii numberic
  ;; sequence

(use-package visual-ascii-mode
  :commands (global-visual-ascii-mode visual-ascii-mode)
  :eemacs-mmphca
  (((:enable t :defer (:data
                       (:adfors
                        (help-mode)
                        :adtype after
                        :pdumper-no-end t)))
    (help-mode (help-mode help-mode-map)))
   ("Visual"
    (("v" visual-ascii-mode "Visualize ascii code on buffer"
      :enable t :exit t :map-inject t)))))

;; *** view emacs memory map
(use-package memory-usage
  :commands (memory-usage memory-usage-find-large-variables)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (memory-usage))
   ("Basic"
    (("u" memory-usage "Show current emacs session memory map"
      :enable t :exit t)
     ("v" memory-usage-find-large-variables
      "Find variables whose printed representation takes over 100KB"
      :enable t :exit t))))
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Utils"
    (("u m"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'memory-usage))
      "Emacs memory usage view"
      :enable t :exit t))))
  :init
  :config
  ;; readonly for popuped buffer and other hacking
  (defun __adv/around/memory-usage-with-exploer-buffer
      (orig-func &rest orig-args)
    (let ((inhibit-read-only t))
      (with-current-buffer
          (get-buffer-create "*Memory Explorer*")
        (setq buffer-read-only t)
        (condition-case error
            (entropy/emacs-message-simple-progress-message
             "Memory find large variable"
             (apply orig-func orig-args))
          (error
           (sort-numeric-fields 1 (point-min) (point-max))
           (message "%s" error))))))
  (advice-add 'memory-usage-find-large-variables
              :around
              #'__adv/around/memory-usage-with-exploer-buffer)
  (defun __adv/around/memory-usage-with-Buffer-Details-buffer
      (orig-func &rest orig-args)
    (let ((inhibit-read-only t))
      (with-current-buffer
          (get-buffer-create "*Buffer Details*")
        (setq buffer-read-only t)
        (entropy/emacs-message-simple-progress-message
         "Memory general overview"
         (apply orig-func orig-args)))))
  (advice-add 'memory-usage
              :around
              #'__adv/around/memory-usage-with-Buffer-Details-buffer))

;; ** entropy-emacs self packages

;; *** entropy-proxy-url
(use-package entropy-proxy-url
  :ensure nil
  :if (plist-get entropy/emacs-union-http-proxy-plist :enable)
  :eemacs-macros
  (entropy/proxy-url-with-url-proxy
   entropy/proxy-url-with-w3m-proxy
   entropy/proxy-url-with-socks-proxy
   entropy/proxy-url-with-shell-proxy)
  :commands (entropy/proxy-url-make-builtin-recipes
             entropy/proxy-url-make-recipes)
  :preface

  (defvar entropy/emacs-proxy-url-loaded nil)
  (when entropy/emacs-fall-love-with-pdumper
    ;; prevent non-interactive procedure loading w3m as fatally status
    ;; for `entropy-proxy-url' w3m loading status checker
    (when (executable-find "w3m")
      (setq entropy/proxy-url--w3m-load-effectively
            t)))

  :eemacs-mmphca
  ((((:enable t :defer (:data
                        (:adfors
                         (eww-mode w3m-mode)
                         :adtype after
                         :pdumper-no-end t)))
     (eww-mode (eww eww-mode-map)))
    ("Proxy"
     (("p" entropy/proxy-url-switch-proxy-for-eww-group
       "Toggle proxy type"
       :enable t :map-inject t :exit t))))
   (((:enable t :defer (:data
                        (:adfors
                         (eww-mode w3m-mode)
                         :adtype after
                         :pdumper-no-end t)))
     (w3m-mode (w3m w3m-mode-map)))
    ("Proxy"
     (("p" entropy/emacs-tools-w3m-toggle-proxy
       "Toggle proxy type"
       :enable t :map-inject t :exit t)))))

  :init

  (let ((http_proxy_host (or (plist-get entropy/emacs-union-http-proxy-plist :host) "127.0.0.1"))
        (http_proxy_port (or (plist-get entropy/emacs-union-http-proxy-plist :port) 7890)))
    (setq entropy/proxy-url-default-proxy-server-alist
          `((emacs-url "http://" ,http_proxy_host ,http_proxy_port)
            (shell-http  "http://" ,http_proxy_host ,http_proxy_port)
            (emacs-w3m "http://" ,http_proxy_host ,http_proxy_port)))

    (setq entropy/proxy-url-default-no-proxy-regexp-list
          (mapcar (lambda (ip) (regexp-quote ip))
                  (entropy/emacs-gen-eemacs-union-proxy-noproxy-envs
                   (or entropy/emacs-union-proxy-noproxy-list
                       '("127.0.0.1" "localhost"))
                   t))))

  (when (plist-get entropy/emacs-union-socks-proxy-plist :enable)
    (add-to-list 'entropy/proxy-url-default-proxy-server-alist
                 `(emacs-socks
                   ""
                   (or (plist-get entropy/emacs-union-socks-proxy-plist :host) "127.0.0.1")
                   (or (plist-get entropy/emacs-union-socks-proxy-plist :port) 7890)
                   (or (number-to-string
                        (plist-get entropy/emacs-union-socks-proxy-plist
                                   :socks-version))
                       "5")
                   )))

  (defun entropy/emacs-tools--proxy-url-w3m-specific ()
    ;; recorde current retrieve url
    (defvar entropy/emacs-tools--w3m-retrieve-url nil)
    (defun entropy/emacs-tools--w3m-recorde-retrieve-url (url &rest args)
      (setq entropy/emacs-tools--w3m-retrieve-url url))
    (defun entropy/emacs-tools-w3m-toggle-proxy ()
      "Toggle proxy using `entropy-proxy-url' and refresh current
web page buffer. It's typically using with the statement that you
can't visit one page suddenly."
      (interactive)
      (require 'entropy-proxy-url)
      (let ((url entropy/emacs-tools--w3m-retrieve-url))
        (entropy/proxy-url-switch-proxy-for-w3m-group)
        (call-interactively 'w3m-process-stop)
        (w3m-goto-url url)))
    (advice-add 'w3m-retrieve :before
                #'entropy/emacs-tools--w3m-recorde-retrieve-url))

  (entropy/emacs-lazy-load-simple w3m
    (unless (eq entropy/emacs-proxy-url-loaded t)
      (require 'entropy-proxy-url)
      (entropy/proxy-url-make-builtin-recipes)
      (setq entropy/emacs-proxy-url-loaded t))
    (when (executable-find "w3m")
      (entropy/emacs-tools--proxy-url-w3m-specific)))

  (entropy/emacs-lazy-load-simple eww
    (unless (eq entropy/emacs-proxy-url-loaded t)
      (require 'entropy-proxy-url)
      (entropy/proxy-url-make-builtin-recipes)
      (setq entropy/emacs-proxy-url-loaded t)))
  )


;; *** entropy-unfill
(use-package entropy-unfill
  :ensure nil
  :commands (entropy/unfill-full-buffer-without-special-region
             entropy/unfill-paragraph
             entropy/fill-full-buffer-without-special-region)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (entropy-unfill))
   ("Unfill"
    (("u" entropy/unfill-full-buffer-without-special-region
      "Unfill full buffer without specific special region"
      :enable t :exit t)
     ("p" entropy/unfill-paragraph
      "Takes paragraph into a single line of text"
      :enable t :exit t))
    "Fill"
    (("f" entropy/fill-full-buffer-without-special-region
      "Fill full buffer without specific special region"
      :enable t :exit t))))

  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Utils"
    (("u x"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'entropy-unfill))
      "Entropy Unfill"
      :enable t :exit t)))))

;; *** entropy-org-batch-refile
(use-package entropy-org-batch-refile
  :ensure nil
  :commands entropy/org-batch-refile-tags-read-and-do
  :eemacs-mmphca
  (((:enable t :defer (:data
                       (:adfors
                        (org-mode-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (org-mode (org org-mode-map)))
   ("Org Refile"
    (("r b" entropy/org-batch-refile-tags-read-and-do
      "Refile by specifying the tag matched"
      :enable t :exit t)))))

;; *** entropy-cp-or-mv
(use-package entropy-dired-cp-or-mv
  :ensure nil
  :commands (entropy/cpmv-dired-get-files-list
             entropy/cpmv-to-current)
  :eemacs-mmphca
  (((:enable t :defer (:data (:adfors (dired-mode-hook) :adtype hook :pdumper-no-end t)))
    (dired-mode (dired dired-mode-map)))
   ("Misc."
    (("c c" entropy/cpmv-dired-get-files-list
      "Get files list for cp or mv"
      :enable t :exit t)
     ("c t" entropy/cpmv-to-current
      "Cp or Mv fils or directory"
      :enable t :exit t)))))

;; *** entropy-portableapps
(use-package entropy-portableapps
  :if sys/is-win-group
  :ensure nil
  :commands (entropy/poapps-query-open)
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-after-startup-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Misc."
    (("<M-up>" entropy/poapps-query-open
      "Query and open portableapps"
      :enable t
      :exit t
      :eemacs-top-bind t)))))

;; *** entropy-epub2org
(use-package entropy-epub2org
  :ensure nil
  :commands (entropy/ep2o-dispatcher
             entropy/ep2o-src-adjusting-manually)
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (entropy/emacs-hydra-hollow-call-before-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Misc."
    (("m e"
      (:pretty-hydra-cabinet
       (:data
        "Convert ebook to org files"
        (("m e d" entropy/ep2o-dispatcher
          "Convert epub book to org-files"
          :enable t :exit t)
         ("m e m" entropy/ep2o-src-adjusting-manually
          "Re-adjust epub book org version"
          :enable t :exit t))))
      "Convert ebook to org files"
      :enable t :exit t))))
  :config
  (defun entropy/emacs-tools--ep2o-tidy-up-image-width-defaut ()
    (unless (equal major-mode 'org-mode)
      (org-mode)
      (outline-show-all)
      (org-show-all))
    (add-file-local-variable 'org-image-actual-width 500))
  (add-hook 'entropy/ep2o-tidy-hook #'entropy/emacs-tools--ep2o-tidy-up-image-width-defaut))


;; * provide
(provide 'entropy-emacs-tools)
