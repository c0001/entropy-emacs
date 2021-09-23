;;; entropy-emacs.el --- entropy emacs main bridge controller
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
;; ** Require
(when entropy/emacs-startup-debug-on-error
  (setq debug-on-error t))

;; *** load wasteland
;; **** var binds
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defface)
(require 'entropy-emacs-defvar)

;; forbidden `entropy/emacs-custom-enable-lazy-load' at special
;; session.
(progn
  (when (and (or entropy/emacs-fall-love-with-pdumper
                 (daemonp))
             entropy/emacs-custom-enable-lazy-load)
    (setq entropy/emacs-custom-enable-lazy-load nil)))

;; **** func binds
(require 'entropy-emacs-message)
(require 'entropy-emacs-defun)

;; *** load ui
;; load fontset fistly prevents ui position calculating bug.
(require 'entropy-emacs-font-set)
(require 'entropy-emacs-ui)
(redisplay t)

;; *** load baron
;; **** summons
;; ***** elisp packages
;; ****** require eemacs packages library
(require 'entropy-emacs-package)

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
                 entropy/emacs-stuffs-topdir))
            buff_content)
        (with-current-buffer (get-buffer "*Compile-Log*")
            (setq buff_content
                  (buffer-substring-no-properties
                   (point-min) (point-max))))
        (with-current-buffer (find-file-noselect $f)
          (goto-char (point-min))
          (insert buff_content)
          (save-buffer)
          (kill-buffer)))))
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
     (underscore
      (magenta
       "Remaining procedure can not loaded in this
session because of you have installed some stuffs in this
session, please restart thus, and it will be well.")))
    (when (not noninteractive)
      (entropy/emacs-message-do-message
       "%s%s"
       (yellow (bold "Warn: "))
       (yellow
        "You init with installing new packages, please reopen emacs!
Emacs will auto close after 5 minutes \
or manually do 'C-x C-c' immediately.")))
    (require 'cl-macs)
    (cl-assert (entropy/emacs-message-focus-on-popup-window))))

;; breaking remaining procedure while new package intalled within this
;; session, because some messy.
(add-hook 'entropy/emacs-package-common-start-after-hook
          #'entropy/emacs-start--warn-with-pkg-install)

(require 'entropy-emacs-ext)
(defvar entropy/emacs-start-ext-available-p
  (entropy/emacs-ext-main))
(when entropy/emacs-start-ext-available-p
  (entropy/emacs-package-common-start))

;; ***** Then require top facilities
(unless entropy/emacs-start--is-init-with-install
  ;; coworker
  (require 'entropy-emacs-coworker)

  ;; top utils
  (require 'entropy-emacs-utils)
  (when entropy/emacs-startup-benchmark-init
    (benchmark-init/activate))

  ;; startup
  (require 'entropy-emacs-gc)
  (require 'entropy-emacs-path)

  ;; hollows
  (require 'entropy-emacs-window-parameter-memory)
  (require 'entropy-emacs-hydra-hollow))

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
  (when entropy/emacs-win-init-ime-enable
    (entropy/emacs-lazy-with-load-trail
     patch-and-enable-w32-ime
     (entropy/emacs-start-w32-ime-enable))))


;; *** Diable linux tty swiching keybinding
;;
;; Disable virtual terminal tty switching keybinding on linux for
;; release more keybinding possibilitie.(see
;; <https://unix.stackexchange.com/questions/34158/rebinding-disabling-ctrlaltf-virtual-terminal-console-switching>)
(when sys/linux-x-p
  (entropy/emacs-lazy-with-load-trail
   setxkbmap
   (shell-command "setxkbmap -option srvrkeys:none")
   (entropy/emacs-message-do-message
    (yellow "Diable tty switching keybinding done! You can run shell-command \"setxkbmap -option ''\" manually"))))

;; *** Resetting browse-url-function in fancy-startup-screen

(defun entropy/emacs-start--startup-screen-after-advice (&rest arg-rest)
  "The advice when `entropy/emacs-browse-url-function' was detectived.

The main goal for this advice function was to chanage startup
screen's `browse-url-browse-function' to
`entropy/emacs-browse-url-function'."
  (if (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
      (setq-local browse-url-browser-function entropy/emacs-browse-url-function)
    (setq-local browse-url-browser-function 'browse-url-default-browser)))


(defun entropy/emacs-start--about-emacs-after-advice (&rest arg-rest)
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

;; *** status of pyim loading
(defvar entropy/emacs-start--pyim-timer nil)

(defun entropy/emacs-start--pyim-init-status-guard ()
  "Trace pyim loading thread til it's done as giving the
notation.

(specific for emacs version uper than '26' or included '26'.)"
  (let ((timer-stop-func
         (lambda ()
           (when (timerp entropy/emacs-start--pyim-timer)
             (cancel-timer entropy/emacs-start--pyim-timer))
           (setq entropy/emacs-start--pyim-timer nil)
           (cancel-function-timers #'entropy/emacs-start--pyim-init-status-guard))))
    (cond ((bound-and-true-p entropy/emacs-pyim-has-initialized)
           (funcall timer-stop-func)
           (if (eq entropy/emacs-pyim-has-initialized t)
               (entropy/emacs-message-do-message (green "pyim loading down."))
             (entropy/emacs-message-do-message
              "%s: Pyim loading with status: %s, so that it's no been initialized!"
              (yellow "warning")
              (cyan (format "%s" entropy/emacs-pyim-has-initialized))))
           (defun entropy/emacs-start--pyim-init-status-guard ()
             (entropy/emacs-message-do-message
              (yellow "This function has been unloaded."))))
          (t
           (funcall timer-stop-func)
           (setq entropy/emacs-start--pyim-timer
                 (run-with-timer
                  1 nil
                  #'entropy/emacs-start--pyim-init-status-guard))))))

(defun entropy/emacs-start--pyim-initialize ()
  "Make prompt when loading and loded pyim cache for emacs init time."
  ;; preparation prompt loading pyim cache.
  (let (_)
    (entropy/emacs-message-do-message
     "%s"
     (green "Loading pyim cache, please waiting ......"))
    (redisplay t))
  ;; initialize pyim
  (entropy/emacs-basic-pyim-start)
  ;; prompt for loading pyim cache done.
  (setq entropy/emacs-start--pyim-timer
        (run-with-idle-timer
         0.4 nil
         #'entropy/emacs-start--pyim-init-status-guard))
  ;; reset function
  (defun entropy/emacs-start--pyim-initialize ()
    (message "This function has been unloaded.")))

(when entropy/emacs-enable-pyim
  (add-hook 'entropy/emacs-start--init-after-load-hook
            #'entropy/emacs-start--pyim-initialize))

;; *** after load procedure

(defun entropy/emacs-start--init-after-load-initialze-process (enable aft-hook)
  (when enable
    ;; run after init hooks
    (unless entropy/emacs-start--is-init-with-install
      (entropy/emacs-message-do-message
       (cyan "After load initilizing ..."))
      (setq entropy/emacs-run-startup-trail-hooks-init-timestamp
            (current-time))
      (run-hooks aft-hook)
      (entropy/emacs-message-do-message
       (green "After load initilized"))
      (setq entropy/emacs-run-startup-trail-hooks-init-done-timestamp
            (current-time)))
    ;; append startup hook when there's no any installation detected
    (when (not entropy/emacs-start--is-init-with-install)
      (setq entropy/emacs-after-startup-hook
            (append entropy/emacs-after-startup-hook
                    entropy/emacs-start--init-after-load-hook)))))

;; *** start up branch

(defun entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe
    (single-feature)
  "For eemacs tentacles `require' prefer to use *.el file instead
of founding its *.elc prior file in some special cases."
  (let ((fname (format "%s.el" single-feature)))
    (if (or (bound-and-true-p entropy/emacs-fall-love-with-pdumper)
            (daemonp)
            (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load)))
        (require single-feature fname)
      (require single-feature))))

;; **** mini start
(defun entropy/emacs-start-M-enable ()
  (entropy/emacs-message-do-message
   "%s %s"
   (white "⮞")
   (blue "Loading minimal ...... "))
  (advice-add 'require :around #'entropy/emacs-start--advice-for-require-prompt)

  ;; external depedencies scan and loading
  (when entropy/emacs-fall-love-with-pdumper
    (require 'entropy-emacs-pdumper))

  ;; basic feature defination
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-basic)

  ;; ============================
  ;; mainly ui configuration
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-modeline)

  ;; loading theme configuration after the modeline for loading theme
  ;; specifics for some mode-line adaption
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-themes)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-wc)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-popwin)
  ;; =============================

  ;; ineractive
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-ivy)
  ;; org
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-org)
  ;; code folding
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-structure)

  ;; ends for minimal start
  (advice-remove 'require #'entropy/emacs-start--advice-for-require-prompt)

  ;; after loading eemacs mini
  (entropy/emacs-start--init-after-load-initialze-process
   entropy/emacs-minimal-start 'entropy/emacs-init-mini-hook)

  (defun entropy/emacs-start-M-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (entropy/emacs-message-do-message
   "%s %s"
   (white "⮞")
   (green "Minimal start completed.")))

;; **** x enable
(defun entropy/emacs-start-X-enable ()
  (interactive)
  (advice-add 'require :around #'entropy/emacs-start--advice-for-require-prompt)
  (entropy/emacs-message-do-message
   "%s %s" (white "⮞") (blue "Loading rest ......"))
  ;; highlight
  (when entropy/emacs-use-highlight-features
    ;; highlight package will cause the low performance of emacs interpretering, so this be the
    ;; choiced option
    (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-highlight))

  ;; For code IDE
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-yas)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-codeserver)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-company)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-comments)
  ;; For useful tools
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-shell)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-ibuffer)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-textwww)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-rss)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-gnus)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-project)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-tools)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-music)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-vcs)

  ;; init-calendar was not adapt emacs 27 or higher and just adapt emacs 25 or earlier
  ;; [2018-03-09 Fri 13:35:45] it's be fixed by xwl (the maintainer)
  ;; issue url `https://github.com/xwl/cal-china-x/issues/12'
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-calendar)

  ;; For programing language
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-bash)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-web)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-python)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-c)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-markdown)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-emacs-lisp)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-lua)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-zeal)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-go)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-rust)
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-yaml)
  ;; For tramp
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-tramp)
  ;; For game
  (entropy/emacs-start/require/for/tentacles-requiring-use-elc-maybe 'entropy-emacs-game)
  ;; end
  (advice-remove 'require #'entropy/emacs-start--advice-for-require-prompt)
  (entropy/emacs-start--init-after-load-initialze-process
   (null entropy/emacs-minimal-start)
   'entropy/emacs-init-X-hook)

  (defun entropy/emacs-start-X-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (entropy/emacs-message-do-message
   "%s %s"
   (white "⮞")
   (green "Full start completed.")))

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
(when (and sys/win32p
           (eq w32-ansi-code-page 65001))
  (setq w32-system-coding-system 'utf-8)
  (define-coding-system-alias 'cp65001 'utf-8))

(defun entropy/emacs-start-do-load ()
  (let (_)
    (when (and entropy/emacs-start-ext-available-p
               (not entropy/emacs-start--is-init-with-install))
      (entropy/emacs-message-do-message (yellow "Cat's eye opening ..."))
      (setq entropy/emacs-run-startup-config-load-init-timestamp
            (current-time))
      (entropy/emacs-start--init-bingo)
      (unless entropy/emacs-fall-love-with-pdumper
        (entropy/emacs-run-startup-end-hook)))))

(if (or entropy/emacs-fall-love-with-pdumper
        (daemonp))
    (entropy/emacs-start-do-load)
  (run-with-idle-timer
   ;; EEMACS_BUG: we can not set delay to 0 since bug:
   ;; h-6d28b926-88c0-4286-a0de-9ee7b4a7516c
   0.00001 nil
   #'entropy/emacs-start-do-load))

;; * provide
(provide 'entropy-emacs-start)
