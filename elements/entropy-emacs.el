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
;; This file was the connector for other 'entropy-emacs-*' files, can
;; be as the core but mainly for bridge like role.
;;
;; For as the core position of =entropy-emacs=, the top concept
;; designation were register in this file's commentary part as the
;; brief introduction for developer or package user to understanding
;; the basic runtime logic for it.
;;
;; This project fistly building the variable definitions which
;; contains both of the =customizable= and =static const= cases, even
;; for the internal temporal ones, all of them categorized into
;; following files:
;;
;; 1) `entropy-emacs-defcustom': the customizable variables declaration
;; 2) `entropy-emacs-defconst':  the static const variables
;; 3) `entropy-emacs-defvar':    the pacakge internal sharing variables.
;;
;; And the another one file belongs to this top (basic) designation
;; was the `entropy-emacs-defun' file which was built basic on the
;; above variable declaraion and be as the basic library for other
;; package internal files using. 
;;
;; The final coding designation was building each file as the wrapper
;; for corresponding aspect of the major mode, tool-chain, or be the
;; group config for some-thing.
;;
;; * Configuration:
;; 
;; Just requiring this file, you will be able to taste entropy-emacs
;; immediately.
;; 
;; * Code:
;; ** Require
;; *** For basic setting
(require 'entropy-emacs-custom)
(require 'entropy-emacs-defvar)
(require 'entropy-emacs-const)

(require 'entropy-emacs-path)
(require 'entropy-emacs-ui)
(require 'entropy-emacs-font-set)
(redisplay t)


;; *** require advice
(defvar entropy/emacs--required-features nil)

(defface entropy/emacs--require-head-prompt-face
  '((t :weight semi-bold :foreground "yellow1"))
  "Face for the head prompt requiring advice
`entropy/emacs-require-loadding")

(defface entropy/emacs--require-tail-prompt-face
  '((t :weight semi-bold :foreground "linen" :underline t))
  "Face for the tail prompt requiring advice
`entropy/emacs-require-loadding")


(defun entropy/emacs--require-prompt (feature)
  (let ((f-str (symbol-name feature))
        (head (lambda (x) (propertize x 'face 'entropy/emacs--require-head-prompt-face)))
        (tail (lambda (x) (propertize x 'face 'entropy/emacs--require-tail-prompt-face))))
    (message
     (format
      "%s %s"
      (funcall head "Loading:")
      (funcall tail f-str)))))

(defun entropy/emacs--require-loading (feature &rest rest)
  (when (not (member feature entropy/emacs--required-features))
    (entropy/emacs--require-prompt feature)
    (add-to-list 'entropy/emacs--required-features feature)))

;; * Trail
;; ** Windows IME enable function
(when sys/win32p
  (defun entropy/emacs-w32-ime-enable (&optional silence)
    (interactive)
    (if (> (car (w32-version)) 9)
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
  (when entropy/emacs-win-init-ime-enable ()
        (add-hook 'emacs-startup-hook #'(lambda () (entropy/emacs-w32-ime-enable t)))
        (global-set-key (kbd "C-\\") #'(lambda ()
                                         (interactive)
                                         (entropy/emacs-w32-ime-enable t)))))



;; ** Resetting browse-url-function in fancy-startup-screen

(defun entropy/emacs--startup-screen-after-advice (&rest arg-rest)
  "The advice when `entropy/emacs-browse-url-function' was detectived.

The main goal for this advice function was to chanage startup
screen's `browse-url-browse-function' to
`entropy/emacs-browse-url-function'."
  (if (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
      (setq-local browse-url-browser-function entropy/emacs-browse-url-function)
    (setq-local browse-url-browser-function 'browse-url-default-browser)))


(defun entropy/emacs--about-emacs-after-advice (&rest arg-rest)
  "The advice when `entropy/emacs-browse-url-function' was detectived.

The main goal for this advice function was to chanage \"About
Emacs\" buffer's local `browse-url-browse-function' to
`entropy/emacs-browse-url-function'."
  (with-current-buffer "*About GNU Emacs*"
    (if (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
        (setq-local browse-url-browser-function entropy/emacs-browse-url-function)
      (setq-local browse-url-browser-function 'browse-url-default-browser))))

(advice-add 'fancy-startup-screen :after #'entropy/emacs--startup-screen-after-advice)

(advice-add 'about-emacs :after #'entropy/emacs--about-emacs-after-advice)

;; * start
;; ** starting emacs with installing new packages 
(defvar entropy/emacs--X-is-init-with-install nil
  "Judgement of whether X start emacs with installing new packages")

(defun entropy/emacs--X-init-with-install-prompt ()
  "When X start emacs with installing, prompt user to reboot emacs.
and save the compiling log into `user-emacs-dir' named as
'compile_$date.log'."
  (let ((buflist (mapcar #'buffer-name (buffer-list))))
    (when (member "*Compile-Log*" buflist)
      ;; First recorde compiling log
      (let (($f (expand-file-name (concat "compile_" (format-time-string "%Y-%m-%d_%a_%H%M%S") ".log")
                                  user-emacs-directory))
            buff_content)
        (with-current-buffer (find-file-noselect $f)
          (with-current-buffer "*Compile-Log*"
            (setq buff_content (buffer-substring-no-properties (point-min) (point-max))))
          (read-only-mode 0)
          (goto-char (point-min))
          (insert buff_content)
          (save-buffer)
          (kill-buffer)))
      (warn "You init with installing new packages, please reopen emacs! 

Emacs will auto close after 6s ......")
      (setq entropy/emacs--X-is-init-with-install t)))

  (defun entropy/emacs--X-init-with-install-prompt ()
    "This function has been unloaded."
    nil)
  (when entropy/emacs--X-is-init-with-install
    (run-with-timer 6 nil #'kill-emacs)))

;; ** x enable
(defun entropy/emacs-X-enable ()
  (interactive)
  (advice-add 'require :before #'entropy/emacs--require-loading)  
  (message "Loading rest ......")
  ;; core
  (require 'entropy-emacs-tools)
  (require 'entropy-emacs-org)
  (require 'entropy-emacs-structure)
  (require 'entropy-emacs-ivy)
  (require 'entropy-emacs-modeline)
  ;; theme
  (require 'entropy-emacs-themes)
  (redisplay t)
  ;; highlight
  (when entropy/emacs-use-highlight-features
    ;; highlight package will cause the low performance of emacs interpretering, so this be the
    ;; choiced option
    (require 'entropy-emacs-highlight))

  ;; For auto complete
  (require 'entropy-emacs-yas)
  (require 'entropy-emacs-company)
  (require 'entropy-emacs-lsp)
  ;; For useful tools
  (require 'entropy-emacs-shell)
  (require 'entropy-emacs-ibuffer)

  (when sys/linux-x-p
    ;; when in windows environment, mpvplayer can not be load by emms 
    (require 'entropy-emacs-emms))

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
  (require 'entropy-emacs-dash)
  (require 'entropy-emacs-go)
  ;; For calender 
  (require 'cal-china-x)
  (setq mark-holidays-in-calendar t)
  ;; For tramp
  (require 'entropy-emacs-tramp)
  ;; For game
  (require 'entropy-emacs-game)
  ;; end
  (advice-remove 'require #'entropy/emacs--require-loading)
  (run-hooks 'entropy/emacs-init-X-hook)
  (defun entropy/emacs-X-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (message "Full start completed.")
  (run-with-timer 1 nil #'entropy/emacs--X-init-with-install-prompt))


;; ** mini start
(defun entropy/emacs-M-enable ()
  (message "Loading minimal ...... ")
  (advice-add 'require :before #'entropy/emacs--require-loading)
  (require 'entropy-emacs-package)
  (require 'entropy-emacs-basic)
  (require 'entropy-emacs-library)
  (redisplay t)
  ;; end
  (advice-remove 'require #'entropy/emacs--require-loading)
  (run-hooks 'entropy/emacs-init-mini-hook)
  (defun entropy/emacs-M-enable ()
    (interactive)
    (message "This function has been unloaded."))
  (message "Minimal start completed."))

;; ** start type choice
;; *** status of pyim loading
(when entropy/emacs-enable-pyim
  ;; for emacs 26 and higher version
  (defvar entropy/emacs--pyim-timer-26+ nil)
  (defvar entropy/emacs--pyim-init-done-26+ nil)
  
  (defun entropy/emacs--pyim-init-after-loaded-cache-26+ ()
    "Trace pyim loading thread til it's done as giving the
notation.

(specific for emacs version uper than '26' or included '26'.)"
    (when (< (length (all-threads)) 2)
      (setq entropy/emacs--pyim-init-done-26+ t)
      (when (bound-and-true-p entropy/emacs--pyim-timer-26+)
        (cancel-function-timers #'entropy/emacs--pyim-init-after-loaded-cache-26+))
      (kill-buffer-and-window)
      (message "pyim loading down.")
      (defun entropy/emacs--pyim-init-after-loaded-cache-26+ ()
        (message "This function has been unloaded."))))

  ;; for emacs 25 and lower version
  (defun entropy/emacs--pyim-init-after-loaded-cache-26- ()
    "Giving the notation when loaded pyim cache.

(specific for emacs version under '26')"
    (message "pyim loading down.")
    (defun entropy/emacs--pyim-init-after-loaded-cache-26- ()
      (message "This function has been unloaded.")))
  
  (defun entropy/emacs--pyim-init-prompt ()
    "Make prompt when loading and loded pyim cache for emacs init time.

For different emacs version using different tracing method:
- for 26+: `entropy/emacs--pyim-init-after-loaded-cache-26+'
- for 26-: `entropy/emacs-pyim-init-after-loaded-cache-26_'

It's for that emacs version uper than 26 as pyim using thread for loading cache."
    ;; preparation prompt loading pyim cache.
    (cond
     ((not (version< emacs-version "26"))
      (let ((buffer (get-buffer-create "*pyim-cache-loading*")))
        (with-current-buffer buffer
          (insert (propertize "Loading pyim cache, please waiting ......" 'face 'warning)))
        (split-window-vertically (- (window-total-height) 4))
        (other-window 1)
        (switch-to-buffer buffer)))
     ((version< emacs-version "26")
      (message "Loading pyim cache ......")))
    
    (pyim-restart-1 t)                  ;loading cache main function.
    
    ;; prompt for loading pyim cache done.
    (cond 
     ((not (version< emacs-version "26"))
      (setq entropy/emacs--pyim-timer-26+
            (run-with-timer
             1 200
             #'entropy/emacs--pyim-init-after-loaded-cache-26+)))
     ((version< emacs-version "26")
      (entropy/emacs--pyim-init-after-loaded-cache-26-)))))


;; *** startup main function
(defun entropy/emacs--init-X ()
  (entropy/emacs-M-enable)
  (entropy/emacs-X-enable)
  (when (and entropy/emacs-enable-pyim
             ;; judgement of whether first init entropy-emacs
             (not
              (let ((buflist (mapcar #'buffer-name (buffer-list)))
                    (rtn nil))
                (when (member "*Compile-Log*" buflist)
                  (setq rtn t))
                rtn)))
    (entropy/emacs--pyim-init-prompt)
    (defun entropy/emacs--pyim-init-prompt ()
      (message "This function has been unloaded."))))

(defun entropy/emacs--init-M ()
  (entropy/emacs-M-enable)
  (when entropy/emacs-enable-pyim
    (entropy/emacs--pyim-init-prompt)
    (defun entropy/emacs--pyim-init-prompt ()
      (message "This function has been unloaded."))))


(defun entropy/emacs--init-bingo ()
  (cond
   ((not entropy/emacs-minimal-start)
    (entropy/emacs--init-X))
   (entropy/emacs-minimal-start
    (entropy/emacs--init-M))))


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

(run-with-idle-timer 0.1 nil
                     #'(lambda ()
                         (when (entropy/emacs-ext-main)
                           (entropy/emacs--init-bingo))))

(provide 'entropy-emacs)
