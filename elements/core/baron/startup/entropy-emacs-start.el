;;; entropy-emacs.el --- entropy emacs main bridge controller  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs.el
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
;; This file was the starter for other 'entropy-emacs-*' files, can
;; be as the core but mainly for bridge like role.
;;
;;
;; * Configuration:
;;
;; Just requiring this file, you will be able to taste entropy-emacs
;; immediately.
;;
;; * Code:
(defvar entropy/emacs-start-src-load-file-name
  (eval 'load-file-name))

(defun entropy/emacs-start-bytecode-boot-p ()
  (string-match
   "\\.elc$"
   entropy/emacs-start-src-load-file-name))

;; ** Require
(when entropy/emacs-startup-debug-on-error
  (setq debug-on-error t))
(when entropy/emacs-startup-jit-lock-debug-mode
  (require 'jit-lock)
  (jit-lock-debug-mode t))

(defvar entropy/emacs-start--load-duration-log nil)
(defmacro entropy/emacs-start--run-with-duration-log
    (name &rest body)
  `(if (bound-and-true-p entropy/emacs-startup-with-Debug-p)
       (let* ((inhibit-quit t)
              (before-time (current-time)))
         (prog1
             (progn
               ,@body)
           (push (cons (float-time
                        (time-subtract
                         (current-time)
                         before-time))
                       ;; strip quote of name pattern from require arg
                       (if (and (listp ',name)
                                (eq 2 (length ',name))
                                (eq (car ',name) 'quote))
                           (cadr ',name)
                         ',name))
                 entropy/emacs-start--load-duration-log)))
     ,@body))

(defmacro entropy/emacs-start--require-with-duration-log
    (&rest args)
  `(entropy/emacs-start--run-with-duration-log
    ,(car args)
    (entropy/emacs-common-require-feature ,@args)))

(defun entropy/emacs-start--sort-duration-log
    (&rest _)
  (let ((all-sec
         (apply '+
                (mapcar
                 (lambda (x) (car x))
                 entropy/emacs-start--load-duration-log
                 ))))
    (setq entropy/emacs-start--load-duration-log
          (list
           :sum all-sec
           :details
           (sort entropy/emacs-start--load-duration-log
                 (lambda (el1 el2)
                   (let ((el1-sec (car el1))
                         (el2-sec (car el2)))
                     (> el1-sec el2-sec))))))))

(when (bound-and-true-p entropy/emacs-startup-with-Debug-p)
  (add-hook 'entropy/emacs-after-startup-hook
            #'entropy/emacs-start--sort-duration-log))

;; *** load wasteland
;; **** var binds
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defconst)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defface)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defvar)

;; forbidden `entropy/emacs-custom-enable-lazy-load' at special
;; session.
(progn
  (when (and (or entropy/emacs-fall-love-with-pdumper
                 (daemonp))
             entropy/emacs-custom-enable-lazy-load)
    (setq entropy/emacs-custom-enable-lazy-load nil)))

;; **** func binds
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-message)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defun)

;; *** load ui
;; load fontset fistly prevents ui position calculating bug.
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-font-set)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-ui)
(redisplay t)

;; *** load baron
;; **** summons
;; ***** elisp packages
;; ****** require eemacs packages library
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-package)

;; ****** Quit emacs with installing new packages
(defvar entropy/emacs-start--is-init-with-install nil
  "Judgement of whether X start emacs with installing new packages")

(defun entropy/emacs-start--check-init-with-install-p ()
  "When start emacs with installing, prompt user to reboot emacs.
and save the compiling log into `entropy/emacs-stuffs-topdir'
named as 'compile_$date.log'."
  (let ((buflist (mapcar #'buffer-name (buffer-list))))
    (when (> (length entropy/emacs-package-install-success-list) 0)
      (setq entropy/emacs-start--is-init-with-install t))
    ;; persist save compile log for debugging
    (when (member "*Compile-Log*" buflist)
      ;; First recorde compiling log
      (let ((inhibit-read-only t)
            ($f (expand-file-name
                 (concat "compile_" (format-time-string "%Y-%m-%d_%a_%H%M%S") ".log")
                 (expand-file-name "eemacs-package-install-compile-logs"
                                   entropy/emacs-stuffs-topdir))))
        (with-current-buffer (get-buffer "*Compile-Log*")
          (entropy/emacs-write-file $f)))))
  ;; fake defun
  (defun entropy/emacs-start--check-init-with-install-p ()
    "This function has been unloaded."
    nil)
  (when entropy/emacs-start--is-init-with-install
    (run-with-timer 300 nil #'kill-emacs)))

(defun entropy/emacs-start--warn-with-pkg-install ()
  (entropy/emacs-start--check-init-with-install-p)
  (when entropy/emacs-start--is-init-with-install
    (entropy/emacs-message-do-message
     "%s"
     :popup-while-eemacs-init-with-interactive t
     (underscore
      (magenta
       "Remaining procedure can not loaded in this
session because of you have installed some stuffs in this
session, please restart thus, and it will be well.")))
    (when (not noninteractive)
      (entropy/emacs-message-do-message
       "%s%s"
       :popup-while-eemacs-init-with-interactive t
       (yellow (bold "Warn: "))
       (yellow
        "You init with installing new packages, please reopen emacs!
Emacs will auto close after 5 minutes \
or manually do 'C-x C-c' immediately.")))
    (entropy/emacs-start--require-with-duration-log 'cl-macs)
    (cl-assert (entropy/emacs-message-focus-on-popup-window))))

;; breaking remaining procedure while new package intalled within this
;; session, because some messy.
(unless (entropy/emacs-start-bytecode-boot-p)
  (add-hook 'entropy/emacs-package-common-start-after-hook
            #'entropy/emacs-start--warn-with-pkg-install))

(entropy/emacs-start--require-with-duration-log 'entropy-emacs-ext)
(defvar entropy/emacs-start-ext-available-p
  (entropy/emacs-start--run-with-duration-log
   func/entropy/emacs-ext-main
   (entropy/emacs-ext-main)))
(when entropy/emacs-start-ext-available-p
  (entropy/emacs-start--run-with-duration-log
   func/entropy/emacs-package-common-start
   (entropy/emacs-package-common-start)))

;; ***** Then require top facilities
(unless (or entropy/emacs-start--is-init-with-install
            (not entropy/emacs-start-ext-available-p))
  ;; coworker
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-coworker)

  ;; top utils
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-utils)
  (when entropy/emacs-startup-benchmark-init
    (benchmark-init/activate))

  ;; startup
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-gc)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-path)

  ;; hollows
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-window-parameter-memory)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-hydra-hollow))

;; *** preface advice
(defun entropy/emacs-start--require-prompt (feature)
  (when
      ;; reducing duplicated feature loading prompts
      (not (featurep feature))
    (let ((f-str (symbol-name feature)))
      (when (if entropy/emacs-fall-love-with-pdumper
                ;; reduce duplicated feature load prompts
                (string-match-p "^entropy-" f-str)
              t)
        (entropy/emacs-message-do-message
         "(%s) %s %s"
         (blue "require")
         (green "Loading:")
         (yellow f-str))))))

(defun entropy/emacs-start--advice-for-require-prompt
    (orig-func &rest orig-args)
  (let ((feature (car orig-args)))
    (when (if (not entropy/emacs-fall-love-with-pdumper)
              t
            (if (and
                 (not (eq feature 'vterm))
                 (not (eq feature 'liberime)))
                t
              (user-error "--> Skip for load C module <%s> file while pdumper!"
                          feature)))
      (entropy/emacs-start--require-prompt feature)
      (apply orig-func orig-args))))

;; ** Trail
;; *** Windows IME enable function
(when sys/win32p
  (defun entropy/emacs-start-w32-ime-enable (&optional silence)
    (interactive)
    (when (> (car (w32-version)) 9)
      (if silence
          (w32-send-sys-command #xF000)
        (if (yes-or-no-p "Do you really want to enable w32? ")
            (let (buff)
              (setq buff (get-buffer-create "*entropy/emacs-w32-prompt*"))
              (switch-to-buffer buff)
              (with-current-buffer "*entropy/emacs-w32-prompt*"
                (org-mode)
                (insert
                 "

Quote:

Because of the mechanism of win10-18030, emacs couldn't to using
IME before you do the action that moving the frame of current
emacs session.


When you see this buffer, it proving that you are in the state for
enable w32-ime, please enter 'ENTER' key to enable it.


Futher-prompt:

- when you can not using UPCASE in win10, please type <lwindow> ,
  because this caused after lock-window which activated by
  <lwindow-l>.

- On win10_1809 the possibilities for keys typing messed up of
  that all key stroking will default binding with 'ctrl' prefix
  which caused all normal cammand internally calling as messing
  state, this situation occurred when typing 'C-g' frequently or
  using windows multi-workspace switching 'C-M-<arrow-key>' key
  combination.

  The fixing (temperally recover method) was double typing
  'Control' key as the unlocking way.


                                             Happy your life -v- !


Trying insert some words in below are:
-------------------------------------


")
                (w32-send-sys-command #xF000)))))))
  (when entropy/emacs-microsoft-windows-native-ime-enhancement-enable
    (entropy/emacs-lazy-with-load-trail
      'patch-and-enable-w32-ime
      (entropy/emacs-start-w32-ime-enable))))


;; *** Diable linux tty swiching keybinding
;;
;; Disable virtual terminal tty switching keybinding on linux for
;; release more keybinding possibilitie.(see
;; <https://unix.stackexchange.com/questions/34158/rebinding-disabling-ctrlaltf-virtual-terminal-console-switching>)
  ;; (entropy/emacs-start--run-with-duration-log
  ;;  form/disable-virtual-terminal-tty-sh-command
  ;;  (when sys/linux-x-p
  ;;    (entropy/emacs-lazy-with-load-trail
  ;;     'setxkbmap
  ;;     (shell-command "setxkbmap -option srvrkeys:none")
  ;;     (entropy/emacs-message-do-message
  ;;      (yellow "Diable tty switching keybinding done! You can run shell-command \"setxkbmap -option ''\" manually"
  ;;              :force-message-while-eemacs-init t)))))

;; *** Resetting browse-url-function in fancy-startup-screen

(defun entropy/emacs-start--startup-screen-after-advice (&rest _)
  "The advice when `entropy/emacs-browse-url-function' was detectived.

The main goal for this advice function was to chanage startup
screen's `browse-url-browse-function' to
`entropy/emacs-browse-url-function'."
  (if (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
      (setq-local browse-url-browser-function entropy/emacs-browse-url-function)
    (setq-local browse-url-browser-function 'browse-url-default-browser)))


(defun entropy/emacs-start--about-emacs-after-advice (&rest _)
  "The advice when `entropy/emacs-browse-url-function' was detectived.

The main goal for this advice function was to chanage \"About
Emacs\" buffer's local `browse-url-browse-function' to
`entropy/emacs-browse-url-function'."
  (with-current-buffer "*About GNU Emacs*"
    (if (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
        (setq-local browse-url-browser-function entropy/emacs-browse-url-function)
      (setq-local browse-url-browser-function 'browse-url-default-browser))))

(advice-add 'fancy-startup-screen :after #'entropy/emacs-start--startup-screen-after-advice)

(advice-add 'about-emacs :after #'entropy/emacs-start--about-emacs-after-advice)

;; ** start

(defvar entropy/emacs-start--init-after-load-hook nil
  "hooks injected to `entropy/emacs-after-startup-hook' after inited eemacs.")

;; *** status of internal IME loading
(defvar entropy/emacs-start--internalIME-startup-timer nil)

(defun entropy/emacs-start--internalIME-startup-init-status-guard ()
  "Trace =eemacs-intenal-IME= loading thread til it's done as giving the
notation.

(specific for emacs version uper than '26' or included '26'.)"
  (let ((timer-stop-func
         (lambda ()
           (when (timerp entropy/emacs-start--internalIME-startup-timer)
             (cancel-timer entropy/emacs-start--internalIME-startup-timer))
           (setq entropy/emacs-start--internalIME-startup-timer nil)
           (cancel-function-timers #'entropy/emacs-start--internalIME-startup-init-status-guard))))
    (cond ((bound-and-true-p entropy/emacs-IME-specs-initialized)
           (funcall timer-stop-func)
           (if (eq entropy/emacs-IME-specs-initialized t)
               (entropy/emacs-message-do-message (green "=eemacs-intenal-IME= loading down."))
             (entropy/emacs-message-do-message
              "%s: =eemacs-intenal-IME= loading with status: %s, so that it's no been initialized!"
              (yellow "warning")
              (cyan (format "%s" entropy/emacs-IME-specs-initialized))))
           (defun entropy/emacs-start--internalIME-startup-init-status-guard ()
             (entropy/emacs-message-do-message
              (yellow "This function has been unloaded."))))
          (t
           (funcall timer-stop-func)
           (setq entropy/emacs-start--internalIME-startup-timer
                 (run-with-timer
                  1 nil
                  #'entropy/emacs-start--internalIME-startup-init-status-guard))))))

(defun entropy/emacs-start--internalIME-startup-initialize ()
  "Make prompt when loading and loded =eemacs-intenal-IME= for emacs init time."
  ;; preparation prompt loading eemacs intenal IME.
  (let (_)
    (entropy/emacs-message-do-message
     "%s"
     (green "Loading =eemacs-intenal-IME=, please waiting ......"))
    (redisplay t))
  ;; initialize =eemacs-intenal-IME=
  (entropy/emacs-internal-ime-starter t)
  ;; prompt for loading =eemacs-intenal-IME= done.
  (setq entropy/emacs-start--internalIME-startup-timer
        (run-with-idle-timer
         0.4 nil
         #'entropy/emacs-start--internalIME-startup-init-status-guard))
  ;; reset function
  (defun entropy/emacs-start--internalIME-startup-initialize ()
    (message "This function has been unloaded.")))

(when entropy/emacs-internal-ime-use-backend
  (cond
   ;; FIXME: intenal ime enabel procedure will block daemon init
   ;; process so we arrange it into the server frame create hook.
   ((daemonp)
    (defvar entropy/emacs-start--internalIME-daemon-init-guard-func nil)
    (setq entropy/emacs-start--internalIME-daemon-init-guard-func
          (entropy/emacs-with-daemon-make-frame-done
           'startup-with-internal-ime
           nil nil
           '(unless entropy/emacs-IME-specs-initialized
              (entropy/emacs-start--internalIME-startup-initialize)
              (remove-hook 'entropy/emacs-daemon-server-after-make-frame-hook
                           entropy/emacs-start--internalIME-daemon-init-guard-func)))))
   (t
    (add-hook 'entropy/emacs-start--init-after-load-hook
              #'entropy/emacs-start--internalIME-startup-initialize))))

;; *** after load procedure

(defun entropy/emacs-start--init-after-load-initialze-process ()
  (let (_)
    ;; run after init hooks
    (unless entropy/emacs-start--is-init-with-install
      (entropy/emacs-message-do-message
       "==================== eemacs trail hooks running ===================="
       :force-message-while-eemacs-init t)
      (entropy/emacs-message-do-message
       (cyan "After load initilizing ...")
       :force-message-while-eemacs-init t)
      (setq entropy/emacs-run-startup-trail-hooks-init-timestamp
            (current-time))
      (run-hooks (entropy/emacs-select-trail-hook t))
      (setq entropy/emacs-run-startup-trail-hooks-init-done-timestamp
            (current-time))
      (entropy/emacs-message-do-message
       (green "After load initilized")
       :force-message-while-eemacs-init t))
    ;; append startup hook when there's no any installation detected
    (when (not entropy/emacs-start--is-init-with-install)
      (setq entropy/emacs-after-startup-hook
            (append entropy/emacs-after-startup-hook
                    entropy/emacs-start--init-after-load-hook)))))

;; *** start up branch

;; **** mini start
(defun entropy/emacs-start-M-enable ()
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞")
   (blue "Loading minimal ...... "))
  (advice-add 'require :around #'entropy/emacs-start--advice-for-require-prompt)

  ;; external depedencies scan and loading
  (when entropy/emacs-fall-love-with-pdumper
    (!eemacs-require 'entropy-emacs-pdumper))

  ;; basic feature defination
  (entropy/emacs-common-require-feature 'entropy-emacs-basic)

  ;; ============================
  ;; mainly ui configuration
  (entropy/emacs-common-require-feature 'entropy-emacs-modeline)

  ;; loading theme configuration after the modeline for loading theme
  ;; specifics for some mode-line adaption
  (entropy/emacs-common-require-feature 'entropy-emacs-themes)
  ;; FIXME: we must set up font initially after theme spec config load
  ;; to take font set effectively WHY?
  (entropy/emacs-font-set--setfont-initial)

  (entropy/emacs-common-require-feature 'entropy-emacs-wc)
  (entropy/emacs-common-require-feature 'entropy-emacs-popwin)
  ;; =============================

  ;; ineractive
  (cond ((eq entropy/emacs-command-completion-use-style 'ivy)
         (entropy/emacs-common-require-feature 'entropy-emacs-ivy))
        (t
         nil))
  ;; org
  (entropy/emacs-common-require-feature 'entropy-emacs-org)
  ;; code folding
  (entropy/emacs-common-require-feature 'entropy-emacs-structure)

  ;; ends for minimal start
  (advice-remove 'require #'entropy/emacs-start--advice-for-require-prompt)

  (defun entropy/emacs-start-M-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞")
   (green "Minimal start completed.")))

;; **** x enable
(defun entropy/emacs-start-X-enable ()
  (interactive)
  (advice-add 'require :around #'entropy/emacs-start--advice-for-require-prompt)
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞") (blue "Loading rest ......"))
  ;; highlight
  (when entropy/emacs-use-highlight-features
    ;; highlight package will cause the low performance of emacs interpretering, so this be the
    ;; choiced option
    (entropy/emacs-common-require-feature 'entropy-emacs-highlight))

  ;; For code IDE
  (entropy/emacs-common-require-feature 'entropy-emacs-yas)
  (entropy/emacs-common-require-feature 'entropy-emacs-codeserver)
  (entropy/emacs-common-require-feature 'entropy-emacs-company)
  (entropy/emacs-common-require-feature 'entropy-emacs-comments)
  ;; For useful tools
  (entropy/emacs-common-require-feature 'entropy-emacs-shell)
  (entropy/emacs-common-require-feature 'entropy-emacs-ibuffer)
  (entropy/emacs-common-require-feature 'entropy-emacs-textwww)
  (entropy/emacs-common-require-feature 'entropy-emacs-rss)
  (entropy/emacs-common-require-feature 'entropy-emacs-gnus)
  (entropy/emacs-common-require-feature 'entropy-emacs-project)
  (entropy/emacs-common-require-feature 'entropy-emacs-tools)
  (entropy/emacs-common-require-feature 'entropy-emacs-music)
  (entropy/emacs-common-require-feature 'entropy-emacs-vcs)

  ;; init-calendar was not adapt emacs 27 or higher and just adapt emacs 25 or earlier
  ;; [2018-03-09 Fri 13:35:45] it's be fixed by xwl (the maintainer)
  ;; issue url `https://github.com/xwl/cal-china-x/issues/12'
  (entropy/emacs-common-require-feature 'entropy-emacs-calendar)

  ;; For programing language
  (entropy/emacs-common-require-feature 'entropy-emacs-bash)
  (entropy/emacs-common-require-feature 'entropy-emacs-web)
  (entropy/emacs-common-require-feature 'entropy-emacs-python)
  (entropy/emacs-common-require-feature 'entropy-emacs-c)
  (entropy/emacs-common-require-feature 'entropy-emacs-markdown)
  (entropy/emacs-common-require-feature 'entropy-emacs-emacs-lisp)
  (entropy/emacs-common-require-feature 'entropy-emacs-lua)
  (entropy/emacs-common-require-feature 'entropy-emacs-zeal)
  (entropy/emacs-common-require-feature 'entropy-emacs-go)
  (entropy/emacs-common-require-feature 'entropy-emacs-rust)
  (entropy/emacs-common-require-feature 'entropy-emacs-yaml)
  ;; For tramp
  (entropy/emacs-common-require-feature 'entropy-emacs-tramp)
  ;; For game
  (entropy/emacs-common-require-feature 'entropy-emacs-game)
  ;; end
  (advice-remove 'require #'entropy/emacs-start--advice-for-require-prompt)

  (defun entropy/emacs-start-X-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞")
   (green "Full start completed.")))

;; *** start up warns
;; **** linux DE IME warning
(defun entropy/emacs-start-linux-DE-IME-warning (&optional force pop-warn)
  "Warnning for linux desktop IME bug of eemacs bug
[h-12379f67-4311-4433-86e3-a6fdfd886112].

Return t for messy env detected, nil for otherwise. If optional
argument POP-WARN is non-nil, throw out the warning.

If optional argument FORCE is non-nil, we also judged in linux
non-X env."
  (when (or sys/linux-x-p (and sys/linuxp force))
    (let ((envars '("GTL_IM_MODULE"
                    "GTK_IM_MODULE"
                    "QT_IM_MODULE"
                    "XMODIFIERS"))
          (warn-head "\nLinux desktop IME framework detected, they may freeze gui emacs in\n\
fast hint occation follow the\neemacs bug notaion of [h-12379f67-4311-4433-86e3-a6fdfd886112]:

Some IME (input method editor) hang emacs without any response in GUI
mode in Gnome shell while fast scroll screen using arrow down/up
(i.e. ~next-line~ / ~previous-line~) with fast repeat key typing
enable in system setting or keyboard hardware embedded utils.

The only way currently to fix this is to disable the IME in whole
emacs session's environment:
#+begin_src shell
unset GTL_IM_MODULE
unset GTK_IM_MODULE
unset QT_IM_MODULE
unset XMODIFIERS
exec emacs \"$@\"
#+end_src

Put above be the emacs.sh or other caller format with example or just
unseet these environment variable.

Or modify the desktop file or the desktop custom keyboard shortcut for
as ~EXEC=env GTL_IM_MODULE= GTK_IM_MODULE= QT_IM_MODULE= XMODIFIERS= emacs %F~ or
~env GTL_IM_MODULE= GTK_IM_MODULE= QT_IM_MODULE= XMODIFIERS= emacs~.

Currently detected env variables:")
          (msg ""))
      (dolist (env envars)
        (let ((val (getenv env)))
          (when (and val
                     (not (string-empty-p val)))
            (setq msg (format "%s\n%s=%s" msg env val)))))
      (unless (string-empty-p msg)
        (when pop-warn
          (warn "%s" (concat warn-head msg)))
        t))))

(defvar entropy/emacs-start-linux-DE-IME-warning-idle-timer nil)
(defvar entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p nil)

(defun entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset
    (&optional frame force)
  (when (and entropy/emacs-start-linux-DE-IME-warning-idle-timer
             (if (timerp entropy/emacs-start-linux-DE-IME-warning-idle-timer)
                 t
               ;; prevent miscellaneous value injection
               (when entropy/emacs-start-linux-DE-IME-warning-idle-timer
                 (setq entropy/emacs-start-linux-DE-IME-warning-idle-timer nil))
               nil)
             (if (framep frame)
                 ;; justify whether the frame is an gui daemon client
                 (and
                  (entropy/emacs-daemon-frame-is-daemon-client-p frame)
                  (frame-live-p frame)
                  (with-selected-frame frame (display-graphic-p)))
               t)
             (if force
                 t
               ;; just disable the timer when this client is the last gui client
               (not (entropy/emacs-daemon-multi-gui-clients-p))))
    (cancel-timer entropy/emacs-start-linux-DE-IME-warning-idle-timer)
    (setq entropy/emacs-start-linux-DE-IME-warning-idle-timer nil)))

(when (daemonp)
  (add-to-list 'delete-frame-functions
               #'entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset))

(cond
 ((daemonp)
  (entropy/emacs-with-daemon-make-frame-done
   'eemacs-linux-de-ime-warning
   nil
   '(progn
      (when (and (not entropy/emacs-start-linux-DE-IME-warning-idle-timer)
                 (entropy/emacs-start-linux-DE-IME-warning t))
        (setq entropy/emacs-start-linux-DE-IME-warning-idle-timer
              (run-with-idle-timer
               3
               t
               (lambda (&rest _)
                 (unless entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p
                   (setq entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p t)
                   (let ((cur_wcfg (current-window-configuration))
                         (cur_frame (selected-frame)))
                     (unwind-protect
                         (progn
                           (entropy/emacs-start-linux-DE-IME-warning t t)
                           (switch-to-buffer "*Warnings*")
                           (delete-other-windows)
                           (sit-for 0.1)        ;enuser rediplay
                           (when (yes-or-no-p "Did you know that?")
                             (entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset nil t)))
                       (when (and (frame-live-p cur_frame)
                                  (frame-visible-p cur_frame))
                         (with-selected-frame cur_frame
                           (set-window-configuration cur_wcfg)))
                       (setq entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p nil))))))))
      )))

 ((bound-and-true-p entropy/emacs-fall-love-with-pdumper)
  (add-hook 'entropy/emacs-pdumper-load-hook
            #'(lambda (&rest _)
                (entropy/emacs-start-linux-DE-IME-warning nil t))))
 (t
  (entropy/emacs-start-linux-DE-IME-warning nil t)))

;; *** startup main function
(defun entropy/emacs-start--init-X ()
  (entropy/emacs-start-M-enable)
  (entropy/emacs-start-X-enable))

(defun entropy/emacs-start--init-M ()
  (entropy/emacs-start-M-enable))

(defun entropy/emacs-start--init-bingo ()
  (cond
   ((not entropy/emacs-minimal-start)
    (entropy/emacs-start--init-X))
   (entropy/emacs-minimal-start
    (entropy/emacs-start--init-M))))

;; On win10 there's default global utf-8 operation system based
;; language environment setting supporting option. As when it enable,
;; forcing reset emacs internal w32-code page setting to utf-8 which
;; can support `shell-command' or other process calling API support
;; multibyte filename as args.
(defvar w32-ansi-code-page)
(defvar w32-system-coding-system)
(when (and sys/win32p
           (eq w32-ansi-code-page 65001))
  (setq w32-system-coding-system 'utf-8)
  (define-coding-system-alias 'cp65001 'utf-8))

(defun entropy/emacs-start-do-load ()
  (let ((inhibit-quit t))
    (when (and entropy/emacs-start-ext-available-p
               (not entropy/emacs-start--is-init-with-install))
      (entropy/emacs-message-do-message
       "==================== %s ===================="
       :force-message-while-eemacs-init t
       (yellow "Cat's eye opening"))
      (setq entropy/emacs-run-startup-config-load-init-timestamp
            (current-time))
      (entropy/emacs-start--init-bingo)
      (entropy/emacs-start--init-after-load-initialze-process)
      (unless entropy/emacs-fall-love-with-pdumper
        (entropy/emacs-run-startup-end-hook)))))

(entropy/emacs-start--run-with-duration-log
 form/start-tentacles
 (redisplay t)
 (entropy/emacs-start-do-load))

;; * provide
(provide 'entropy-emacs-start)
