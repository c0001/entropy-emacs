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

;; *** customization read
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-message)

(progn (when (and entropy/emacs-fall-love-with-pdumper
                  entropy/emacs-custom-enable-lazy-load)
         (setq entropy/emacs-custom-enable-lazy-load nil))

       (when (and entropy/emacs-fall-love-with-pdumper
                  entropy/emacs-enable-pyim)
         (setq entropy/emacs-enable-pyim nil)
         (entropy/emacs-message-do-message
          (red "You can not enable pyim in pdumper procedure, but
we inject it into pdumper session initialize procedure. "))
         (add-hook 'entropy/emacs-pdumper-load-end-hook
                   #'entropy/emacs-basic-pyim-start)))

(when (daemonp)
  (when entropy/emacs-custom-enable-lazy-load
    (setq entropy/emacs-custom-enable-lazy-load nil)))

;; *** load core library
(require 'entropy-emacs-defun)

;; Increase the default gc-cons-percentage for more smooth typing
;; experience
(require 'entropy-emacs-gc)

(require 'entropy-emacs-defface)

;; *** Enable entropy emacs UI configuration

(cond (entropy/emacs-fall-love-with-pdumper
       ;; Load fontset specification before UI initilization prevent
       ;; drawer bug for pdumper session
       (require 'entropy-emacs-font-set)
       (require 'entropy-emacs-ui))
      (t
       (require 'entropy-emacs-ui)
       (require 'entropy-emacs-font-set)))
(redisplay t)

;; *** preface advice
(defun entropy/emacs-start--require-prompt (feature)
  (let ((f-str (symbol-name feature)))
    (entropy/emacs-message-do-message
     "%s %s"
     (green "Loading:")
     (yellow f-str))))

(defun entropy/emacs-start--require-loading (feature &rest rest)
  (when (not (featurep feature))
    (entropy/emacs-start--require-prompt feature)))

(defun entropy/emacs-start--initial-redisplay-advice (orig-func &rest orig-args)
  (if (or entropy/emacs-fall-love-with-pdumper
          (daemonp))
      (entropy/emacs-message-do-message
       (red "Redisplay disabled in pdumper procedure."))
    (apply orig-func orig-args)))

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
;; *** starting emacs with installing new packages
(defvar entropy/emacs-start--is-init-with-install nil
  "Judgement of whether X start emacs with installing new packages")

(defun entropy/emacs-start--check-init-with-install-p ()
  "When start emacs with installing, prompt user to reboot emacs.
and save the compiling log into `entropy/emacs-stuffs-topdir'
named as 'compile_$date.log'."
  (let ((buflist (mapcar #'buffer-name (buffer-list))))
    (when (member "*Compile-Log*" buflist)
      ;; First recorde compiling log
      (let (($f (expand-file-name (concat "compile_" (format-time-string "%Y-%m-%d_%a_%H%M%S") ".log")
                                  entropy/emacs-stuffs-topdir))
            buff_content)
        (with-current-buffer (find-file-noselect $f)
          (with-current-buffer "*Compile-Log*"
            (setq buff_content (buffer-substring-no-properties (point-min) (point-max))))
          (read-only-mode 0)
          (goto-char (point-min))
          (insert buff_content)
          (save-buffer)
          (kill-buffer)))
      (setq entropy/emacs-start--is-init-with-install t)))
  (defun entropy/emacs-start--check-init-with-install-p ()
    "This function has been unloaded."
    nil)
  (when entropy/emacs-start--is-init-with-install
    (run-with-timer 6 nil #'kill-emacs)))

;; *** start type choice
;; **** status of pyim loading
(when entropy/emacs-enable-pyim
  ;; for emacs 26 and higher version
  (defvar entropy/emacs-start--pyim-timer-26+ nil)
  (defvar entropy/emacs-start--pyim-init-done-26+ nil)

  (defvar entropy/emacs-start--pyim-init-prompt-buffer "*pyim-cache-loading*")

  (defun entropy/emacs-start--pyim-init-after-loaded-cache-26+ ()
    "Trace pyim loading thread til it's done as giving the
notation.

(specific for emacs version uper than '26' or included '26'.)"
    (when (< (length (all-threads)) 2)
      (setq entropy/emacs-start--pyim-init-done-26+ t)
      (when (bound-and-true-p entropy/emacs-start--pyim-timer-26+)
        (cancel-function-timers #'entropy/emacs-start--pyim-init-after-loaded-cache-26+))
      (when (buffer-live-p entropy/emacs-start--pyim-init-prompt-buffer)
        (switch-to-buffer entropy/emacs-start--pyim-init-prompt-buffer)
        (kill-buffer-and-window))
      (entropy/emacs-message-do-message (green "pyim loading down."))
      (defun entropy/emacs-start--pyim-init-after-loaded-cache-26+ ()
        (entropy/emacs-message-do-message
         (yellow "This function has been unloaded.")))))

  ;; for emacs 25 and lower version
  (defun entropy/emacs-start--pyim-init-after-loaded-cache-26- ()
    "Giving the notation when loaded pyim cache.

(specific for emacs version under '26')"
    (entropy/emacs-message-do-message (green "pyim loading down."))
    (defun entropy/emacs-start--pyim-init-after-loaded-cache-26- ()
      (entropy/emacs-message-do-message
       (yellow "This function has been unloaded."))))

  (defun entropy/emacs-start--pyim-initialize ()
    "Make prompt when loading and loded pyim cache for emacs init time.

For different emacs version using different tracing method:
- for 26+: `entropy/emacs-start--pyim-init-after-loaded-cache-26+'
- for 26-: `entropy/emacs-pyim-init-after-loaded-cache-26_'

It's for that emacs version uper than 26 as pyim using thread for loading cache."
    ;; preparation prompt loading pyim cache.
    (cond
     ((not (version< emacs-version "26"))
      (let ((buffer (get-buffer-create entropy/emacs-start--pyim-init-prompt-buffer)))
        (setq entropy/emacs-start--pyim-init-prompt-buffer buffer)
        (with-current-buffer buffer
          (insert (propertize "Loading pyim cache, please waiting ......" 'face 'warning)))
        (split-window-vertically (- (window-total-height) 4))
        (other-window 1)
        (switch-to-buffer buffer)
        (redisplay t)))
     ((version< emacs-version "26")
      (entropy/emacs-message-do-message
       (blue (bold "Loading pyim cache ......")))))

    ;; initialize pyim
    (entropy/emacs-basic-pyim-start)

    ;; prompt for loading pyim cache done.
    (cond
     ((not (version< emacs-version "26"))
      (setq entropy/emacs-start--pyim-timer-26+
            (run-with-idle-timer
             1 200
             #'entropy/emacs-start--pyim-init-after-loaded-cache-26+)))
     ((version< emacs-version "26")
      (entropy/emacs-start--pyim-init-after-loaded-cache-26-)))
    ;; reset function
    (defun entropy/emacs-start--pyim-initialize ()
      (message "This function has been unloaded."))))


;; **** after load procedure

(defun entropy/emacs-start--init-after-load-initialze-process (enable aft-hook)
  (when enable
    ;; check start with package install status
    (when (not (entropy/emacs-is-make-session))
      (entropy/emacs-start--check-init-with-install-p)
      (when entropy/emacs-start--is-init-with-install
        (warn "You init with installing new packages, please reopen emacs!

Emacs will auto close after 6s ......")))
    ;; run after init hooks
    (unless entropy/emacs-start--is-init-with-install
      (entropy/emacs-message-do-message
       (cyan "After load initilizing ..."))
      (run-hooks aft-hook)
      (entropy/emacs-message-do-message
       (green "After load initilized")))
    ;; start pyim
    (when (and entropy/emacs-enable-pyim
               (not entropy/emacs-start--is-init-with-install))
      (entropy/emacs-start--pyim-initialize))))

;; **** start up branch
;; ***** mini start
(defun entropy/emacs-start-M-enable ()
  (entropy/emacs-message-do-message
   "%s %s"
   (white "⮞")
   (blue "Loading minimal ...... "))
  (advice-add 'require :before #'entropy/emacs-start--require-loading)
  (advice-add 'redisplay :around #'entropy/emacs-start--initial-redisplay-advice)

  ;; parse installed packages
  (require 'entropy-emacs-package)
  (entropy/emacs-package-common-start)

  ;; external depedencies scan and loading
  (when entropy/emacs-fall-love-with-pdumper
    (require 'entropy-emacs-pdumper))

  (require 'entropy-emacs-hydra-hollow)

  ;; basic feature defination
  (require 'entropy-emacs-basic)

  ;; ============================
  ;; mainly ui configuration
  (redisplay t)
  (require 'entropy-emacs-modeline)
  (redisplay t)
  ;; loading theme configuration after the modeline for loading theme
  ;; specifics for some mode-line adaption
  (require 'entropy-emacs-themes)
  (redisplay t)

  (require 'entropy-emacs-wc)
  (redisplay t)
  (require 'entropy-emacs-popwin)
  (redisplay t)
  ;; =============================

  ;; ineractive
  (require 'entropy-emacs-ivy)
  ;; org
  (require 'entropy-emacs-org)
  ;; code folding
  (require 'entropy-emacs-structure)

  ;; ends for minimal start
  (advice-remove 'require #'entropy/emacs-start--require-loading)
  (advice-remove 'redisplay #'entropy/emacs-start--initial-redisplay-advice)

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

;; ***** x enable
(defun entropy/emacs-start-X-enable ()
  (interactive)
  (advice-add 'require :before #'entropy/emacs-start--require-loading)
  (advice-add 'redisplay :around #'entropy/emacs-start--initial-redisplay-advice)
  (entropy/emacs-message-do-message
   "%s %s" (white "⮞") (blue "Loading rest ......"))
  ;; highlight
  (when entropy/emacs-use-highlight-features
    ;; highlight package will cause the low performance of emacs interpretering, so this be the
    ;; choiced option
    (require 'entropy-emacs-highlight))

  ;; For code IDE
  (require 'entropy-emacs-yas)
  (require 'entropy-emacs-codeserver)
  (require 'entropy-emacs-company)
  ;; For useful tools
  (require 'entropy-emacs-shell)
  (require 'entropy-emacs-ibuffer)
  (require 'entropy-emacs-textwww)
  (require 'entropy-emacs-rss)
  (require 'entropy-emacs-gnus)
  (require 'entropy-emacs-neotree)
  (require 'entropy-emacs-treemacs)
  (require 'entropy-emacs-project)
  (require 'entropy-emacs-tools)
  (require 'entropy-emacs-music)
  (require 'entropy-emacs-vcs)

  ;; init-calendar was not adapt emacs 27 or higher and just adapt emacs 25 or earlier
  ;; [2018-03-09 Fri 13:35:45] it's be fixed by xwl (the maintainer)
  ;; issue url `https://github.com/xwl/cal-china-x/issues/12'
  (require 'entropy-emacs-calendar)

  ;; For programing language
  (require 'entropy-emacs-web)
  (require 'entropy-emacs-python)
  (require 'entropy-emacs-c)
  (require 'entropy-emacs-markdown)
  (require 'entropy-emacs-emacs-lisp)
  (require 'entropy-emacs-lua)
  (require 'entropy-emacs-zeal)
  (require 'entropy-emacs-go)
  ;; For tramp
  (require 'entropy-emacs-tramp)
  ;; For game
  (require 'entropy-emacs-game)
  ;; end
  (advice-remove 'require #'entropy/emacs-start--require-loading)
  (advice-remove 'redisplay #'entropy/emacs-start--initial-redisplay-advice)

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

;; **** startup main function
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

(require 'entropy-emacs-ext)
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
  (when (entropy/emacs-ext-main)
    (require 'entropy-emacs-path)
    (entropy/emacs-start--init-bingo)
    (unless entropy/emacs-fall-love-with-pdumper
      (setq entropy/emacs-startup-done t))))

(if (or entropy/emacs-fall-love-with-pdumper
        (daemonp))
    (entropy/emacs-start-do-load)
  (run-with-idle-timer
   0.1 nil
   #'entropy/emacs-start-do-load))

;; * provide
(provide 'entropy-emacs-start)
