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
(defconst entropy/emacs-start-bytecode-boot-p
  (string-match-p "\\.elc$" load-file-name))

;; ** Require
(when entropy/emacs-startup-jit-lock-debug-mode
  (jit-lock-debug-mode t))

(defvar entropy/emacs-start--load-duration-log nil)
(eval-when-compile
  (defmacro entropy/emacs-start--run-with-duration-log
      (name &rest body)
    (macroexp-let2* ignore
        ((do-name name))
      `(if (or (bound-and-true-p entropy/emacs-log-startup-duration-details)
               (bound-and-true-p entropy/emacs-startup-with-Debug-p))
           (let* ((inhibit-quit t)
                  (before-time (current-time)))
             (prog1 ,(entropy/emacs-macroexp-progn body)
               (push (cons (entropy/emacs-time-subtract before-time nil t)
                           ;; strip quote of do-name pattern from require arg
                           (if (and (listp ,do-name)
                                    (eq 2 (length ,do-name))
                                    (eq (car ,do-name) 'quote))
                               (cadr ,do-name) ,do-name))
                     entropy/emacs-start--load-duration-log)))
         ,(entropy/emacs-macroexp-progn body)))))

(eval-when-compile
  (defmacro entropy/emacs-start--require-with-duration-log
      (&rest args)
    (macroexp-let2* ignore
        ((car-args (car args))
         (cdr-args (cdr args)))
      (macroexpand-1
       `(entropy/emacs-start--run-with-duration-log
         ,car-args
         (entropy/emacs-common-require-feature ,@args))))))

(defun entropy/emacs-start--sort-duration-log
    (&rest _)
  (let ((all-sec
         (apply #'+ (mapcar #'car entropy/emacs-start--load-duration-log))))
    (setq entropy/emacs-start--load-duration-log
          (list
           :sum all-sec
           :details
           (sort entropy/emacs-start--load-duration-log
                 #'(lambda (el1 el2)
                     (let ((el1-sec (car el1))
                           (el2-sec (car el2)))
                       (> el1-sec el2-sec))))))))

;; *** load wasteland
;; **** var binds
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defconst)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defface)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defvar)

;; **** func binds
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-message)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-defun)

;; *** load ui
;; load fontset fistly prevents ui position calculating bug.
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-font-set)
(entropy/emacs-start--require-with-duration-log 'entropy-emacs-ui)
;;this redisplay is indeed needed
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
named as 'compile_$date.log'.

Return non-nil while there's indeed installed or failed installing
some packages. Or nil without did anything."
  (when (or entropy/emacs-package-install-success-list
            entropy/emacs-package-install-failed-list)
    (setq entropy/emacs-start--is-init-with-install t)
    (when-let
        ((logbuff (and (bound-and-true-p byte-compile-log-buffer)
                       (get-buffer byte-compile-log-buffer))))
      ;; persist save compile log for debugging
      (let ((file
             (expand-file-name
              (concat "compile_" (format-time-string "%Y-%m-%d_%a_%H%M%S") ".log")
              (expand-file-name "eemacs-package-install-compile-logs/"
                                entropy/emacs-stuffs-topdir))))
        (with-current-buffer logbuff (entropy/emacs-write-file file))))
    ;; fake defun
    (defalias 'entropy/emacs-start--check-init-with-install-p #'ignore
      "This function has been unloaded.")
    (when entropy/emacs-start--is-init-with-install
      (run-with-timer 300 nil #'kill-emacs))
    entropy/emacs-start--is-init-with-install))

(defun entropy/emacs-start--warn-with-pkg-install ()
  (when (entropy/emacs-start--check-init-with-install-p)
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
    (unless (memq 'cl-macs features)
      (entropy/emacs-start--require-with-duration-log 'cl-macs))
    (cl-assert (entropy/emacs-message-focus-on-popup-window))))

;; breaking remaining procedure while new package intalled within this
;; session, because some messy.
(unless entropy/emacs-start-bytecode-boot-p
  (add-hook 'entropy/emacs-package-common-start-after-hook
            #'entropy/emacs-start--warn-with-pkg-install))

(entropy/emacs-start--require-with-duration-log 'entropy-emacs-ext)
(defvar entropy/emacs-start-ext-available-p
  (entropy/emacs-start--run-with-duration-log
   'func/entropy/emacs-ext-main
   (entropy/emacs-ext-main)))
(when entropy/emacs-start-ext-available-p
  (entropy/emacs-start--run-with-duration-log
   'func/entropy/emacs-package-common-start
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
      (not (memq feature features))
    (let ((f-str (symbol-name feature)))
      (when (if entropy/emacs-fall-love-with-pdumper
                ;; reduce duplicated feature load prompts
                (string-match-p "^entropy-" f-str)
              t)
        (entropy/emacs-message-do-message
         "(%s) %s %s (from: \"%s\")"
         (blue "require")
         (green "Loading:")
         (yellow f-str)
         load-file-name)))))

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
  ;;  'form/disable-virtual-terminal-tty-sh-command
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
         #'(lambda ()
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
           (entropy/emacs-with-lambda 'entropy/emacs-start--internalIME-startup-init-status-guard
             (&rest _)
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
  (entropy/emacs-message-do-message
   "%s"
   (green "Loading =eemacs-intenal-IME=, please waiting ......"))
  ;; initialize =eemacs-intenal-IME=
  (entropy/emacs-internal-ime-starter t)
  ;; prompt for loading =eemacs-intenal-IME= done.
  (setq entropy/emacs-start--internalIME-startup-timer
        (run-with-idle-timer
         0.4 nil
         #'entropy/emacs-start--internalIME-startup-init-status-guard))
  ;; reset function
  (entropy/emacs-with-lambda 'entropy/emacs-start--internalIME-startup-initialize ()
    (message "This function has been unloaded.")))

(when entropy/emacs-internal-ime-use-backend
  (cond
   ;; FIXME: intenal ime enabel procedure will block daemon init
   ;; process so we arrange it into the server frame create hook.
   ((daemonp)
    (defvar entropy/emacs-start--internalIME-daemon-init-guard-func nil)
    (setq entropy/emacs-start--internalIME-daemon-init-guard-func
          (entropy/emacs-with-daemon-make-frame-done
            'startup-with-internal-ime (&rest _)
            (unless entropy/emacs-IME-specs-initialized
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
      (unless (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumping-with-lazy-load-p)
        (run-hooks (entropy/emacs-select-trail-hook t)))
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
   (blue "Loading minimal ......"))

  ;; external depedencies scan and loading
  (when entropy/emacs-fall-love-with-pdumper
    (!eemacs-require 'entropy-emacs-pdumper))

  ;; basic feature defination
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-basic)

  ;; ============================
  ;; mainly ui configuration
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-modeline)

  ;; loading theme configuration after the modeline for loading theme
  ;; specifics for some mode-line adaption
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-themes)
  ;; FIXME: we must set up font initially after theme spec config load
  ;; to take font set effectively WHY?
  (entropy/emacs-font-set--setfont-initial)

  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-wc)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-popwin)
  ;; =============================

  ;; ineractive
  (cond ((eq entropy/emacs-command-completion-use-style 'ivy)
         (entropy/emacs-start--require-with-duration-log 'entropy-emacs-ivy))
        (t
         nil))
  ;; org
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-org)
  ;; code folding
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-structure)

  ;; ends for minimal start
  (fmakunbound 'entropy/emacs-start-M-enable)
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞")
   (green "Minimal start completed.")))

;; **** x enable
(defun entropy/emacs-start-X-enable ()
  (interactive)
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞") (blue "Loading rest ......"))
  ;; highlight
  (when entropy/emacs-use-highlight-features
    ;; highlight package will cause the low performance of emacs interpretering, so this be the
    ;; choiced option
    (entropy/emacs-start--require-with-duration-log 'entropy-emacs-highlight))

  ;; For code IDE
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-yas)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-codeserver)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-company)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-comments)
  ;; For useful tools
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-shell)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-ibuffer)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-textwww)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-rss)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-gnus)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-project)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-tools)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-music)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-vcs)

  ;; init-calendar was not adapt emacs 27 or higher and just adapt emacs 25 or earlier
  ;; [2018-03-09 Fri 13:35:45] it's be fixed by xwl (the maintainer)
  ;; issue url `https://github.com/xwl/cal-china-x/issues/12'
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-calendar)

  ;; For programing language
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-shell-script)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-web)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-python)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-c)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-markdown)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-emacs-lisp)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-lua)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-zeal)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-go)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-rust)
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-yaml)
  ;; For tramp
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-tramp)
  ;; For game
  (entropy/emacs-start--require-with-duration-log 'entropy-emacs-game)
  ;; end
  (fmakunbound 'entropy/emacs-start-X-enable)
  (entropy/emacs-message-do-message
   "%s %s"
   :force-message-while-eemacs-init t
   (white "⮞")
   (green "Full start completed.")))

;; *** start up warns
;; **** linux DE IME warning

(defvar entropy/emacs-start--linux-DE-IME-last-check-rtn nil)
(defvar entropy/emacs-start--linux-DE-IME-checked-p nil)
(defun entropy/emacs-start-linux-DE-IME-warning (&optional force pop-warn)
  ;; we just check those envs at startup time since after that there's
  ;; no problem for this emacs main process as what we can use those
  ;; envs in spawn processes.
  (prog1 (entropy/emacs-start-linux-DE-IME-warning-1
          force pop-warn
          (and
           ;; if pdumper indicator is signed then we shouldn't use the
           ;; cache result since its just a dump for the env where
           ;; dumping ran in but not the actual usage session.
           (not entropy/emacs-fall-love-with-pdumper)
           entropy/emacs-start--linux-DE-IME-checked-p))
    (setq entropy/emacs-start--linux-DE-IME-checked-p t)))

(defun entropy/emacs-start-linux-DE-IME-warning-1
    (&optional force pop-warn use-last-result)
  "Warnning for linux desktop IME bug of eemacs bug
[h-12379f67-4311-4433-86e3-a6fdfd886112].

Return t for messy env detected, nil for otherwise. If optional
argument POP-WARN is non-nil, either throw out the warning before
the return.

If optional argument FORCE is non-nil, we also judged in linux
non-X env, this useful for daemon session with X expectation."
  (when (and (or sys/linux-x-p (and force sys/is-linux-and-graphic-support-p))
             ;; Disable warn when in non-XIM options enabled emacs
             ;; build, since it disabled any X IME features natively.
             (not (and system-configuration-options
                       (stringp system-configuration-options)
                       (string-match-p "--without-xim" system-configuration-options))))
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
          msg rtn)
      (if use-last-result
          (setq msg entropy/emacs-start--linux-DE-IME-last-check-rtn
                rtn (not (null msg)))
        (dolist (env envars)
          (let ((val (getenv env)))
            (when (and val (not (string-empty-p val)))
              (setq msg (format "%s\n%s=%s" (or msg "") env val)
                    rtn t))))
        (setq entropy/emacs-start--linux-DE-IME-last-check-rtn msg))
      (prog1 rtn
        (if (and rtn pop-warn) (warn "%s" (concat warn-head msg)))))))

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
                  (entropy/emacs-daemon-frame-is-legal-daemon-client-p frame)
                  (frame-live-p frame)
                  (with-selected-frame frame (display-graphic-p)))
               t)
             (if force
                 t
               ;; just disable the timer when this client is the last gui client
               (not (entropy/emacs-daemon-multi-gui-clients-p))))
    (entropy/emacs-cancel-timer-var
     entropy/emacs-start-linux-DE-IME-warning-idle-timer)))

(when (daemonp)
  (add-to-list 'delete-frame-functions
               #'entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset))

(defun entropy/emacs-start--linux-DE-IME-warning-guard nil
  (unless (or entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p
              ;; in occasion such as daemon without any clients connected
              noninteractive)
    (setq entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p t)
    (let ((cur_wcfg (current-window-configuration))
          (cur_frame (selected-frame))
          nis-p)
      (unwind-protect
          (if (setq nis-p (not (entropy/emacs-start-linux-DE-IME-warning t t)))
              (entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset nil t)
            (switch-to-buffer "*Warnings*")
            (delete-other-windows)
            (sit-for 0.1)        ;enuser rediplay
            (when (yes-or-no-p "Did you know that?")
              (entropy/emacs-start-linux-DE-IME-warning-idle-timer/reset nil t)))
        (when (and (not nis-p)
                   (frame-live-p cur_frame)
                   (frame-visible-p cur_frame))
          (with-selected-frame cur_frame
            (set-window-configuration cur_wcfg)))
        (setq entropy/emacs-start-linux-DE-IME-warning-idle-is-prompting-p
              nil)))))

;; init detection
(when (or
       ;; NOTE: we should always add init checker for pdumper session
       ;; since it's ran with environment independent
       entropy/emacs-fall-love-with-pdumper
       (entropy/emacs-start-linux-DE-IME-warning 'force))
  ;; further popup warn
  (cond
   ((daemonp)
    (entropy/emacs-with-daemon-make-frame-done
      'eemacs-linux-de-ime-warning (&rest _)
      :when-gui
      (when (and (not entropy/emacs-start-linux-DE-IME-warning-idle-timer)
                 (entropy/emacs-start-linux-DE-IME-warning t))
        (setq entropy/emacs-start-linux-DE-IME-warning-idle-timer
              (run-with-idle-timer
               3 t #'entropy/emacs-start--linux-DE-IME-warning-guard)))))
   ((bound-and-true-p entropy/emacs-fall-love-with-pdumper)
    (add-hook 'entropy/emacs-pdumper-load-hook
              #'(lambda (&rest _)
                  (entropy/emacs-start-linux-DE-IME-warning nil t))))
   (t (entropy/emacs-start-linux-DE-IME-warning nil t))))

;; *** startup main function
(defun entropy/emacs-start--init-X ()
  (entropy/emacs-start-M-enable)
  (entropy/emacs-start-X-enable))

(defun entropy/emacs-start--init-M ()
  (entropy/emacs-start-M-enable))

(defun entropy/emacs-start--init-bingo ()
  (advice-add 'require :around #'entropy/emacs-start--advice-for-require-prompt)
  (unwind-protect
      (cond
       ((not entropy/emacs-minimal-start)
        (entropy/emacs-start--init-X))
       (entropy/emacs-minimal-start
        (entropy/emacs-start--init-M)))
    (advice-remove 'require #'entropy/emacs-start--advice-for-require-prompt)))

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
       "%s ..."
       :force-message-while-eemacs-init t
       (yellow "Cat's eye opening"))
      (setq entropy/emacs-run-startup-config-load-init-timestamp
            (current-time))
      (entropy/emacs-start--init-bingo)
      (entropy/emacs-start--init-after-load-initialze-process)
      (unless entropy/emacs-fall-love-with-pdumper
        (entropy/emacs-run-startup-end-hook)))))

(entropy/emacs-start--run-with-duration-log
 'form/start-tentacles-and-all-hooks
 (entropy/emacs-start-do-load)
 (when (or (bound-and-true-p entropy/emacs-log-startup-duration-details)
           (bound-and-true-p entropy/emacs-startup-with-Debug-p))
   (run-with-idle-timer
    0.1 nil
    #'entropy/emacs-start--sort-duration-log)))

;; * provide
(provide 'entropy-emacs-start)
