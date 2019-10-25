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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'subr-x)

;; ** gatherd for minor tools
;; *** openwith external apps
;; **** openwith config
(use-package openwith
  :if (display-graphic-p)
  :commands openwith-make-extension-regexp
  :init
  (add-hook 'dired-mode-hook #'openwith-mode)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv" "m4a" "flac" "aac"))
               ;;"mpv --audio-display=attachment"
               (cond (sys/linuxp
                      "xdg-open")
                     (t
                      nil))
               '(file))
         
         (list (openwith-make-extension-regexp
                '("pdf" "djvu"))
               ;;"evince"
               (cond (sys/linuxp
                      "xdg-open")
                     (t
                      nil))
               '(file))))
  :config
  (defun openwith-open-unix (command arglist)
    "Run external command COMMAND, in such a way that it is
disowned from the parent Emacs process.  If Emacs dies, the
process spawned here lives on.  ARGLIST is a list of strings,
each an argument to COMMAND."
    (let ((shell-file-name "/bin/sh")
          (process-connection-type nil))
      (start-process-shell-command
       "openwith-process" nil
       (concat
        "exec nohup " command " " 
        (mapconcat 'shell-quote-argument arglist " ")
        " >/dev/null")))))

;; **** Function manually
;; ***** open in external apps
(when  (display-graphic-p)
  (defun entropy/emacs-tools-open-in-external-app (&optional files)
    "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.  URL
`http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
    (interactive)
    (let* (($file-list
            (if (not files)
                (if (and (string-equal major-mode "dired-mode"))
                    (dired-get-marked-files)
                  (list (buffer-file-name)))
              files))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files?"))))
      (when $do-it-p
        (cond
         ((string-equal system-type "windows-nt")
          (mapc
           (lambda ($fpath)
             (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t)))
           $file-list))
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (shell-command
              (concat "open " (shell-quote-argument $fpath))))
           $file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda ($fpath)
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" $fpath)))
           $file-list))))))
  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "<C-return>") 'entropy/emacs-tools-open-in-external-app)))

;; ***** Open in desktop manager
(when (display-graphic-p)
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
       ((string-equal system-type "windows-nt")
        (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
       ((string-equal system-type "darwin")
        (if (eq major-mode 'dired-mode)
            (let (($files (dired-get-marked-files )))
              (if (eq (length $files) 0)
                  (shell-command
                   (concat "open " default-directory ))
                (shell-command
                 (concat "open -R " (car (dired-get-marked-files ))))))
          (shell-command
           (concat "open -R " $path))))
       ((string-equal system-type "gnu/linux")
        (shell-command "gio open .")
        ;; gio open was the suggested command for now [2018-01-03 Wed 04:23:17]
        ;;
        ;; 2013-02-10 (shell-command "xdg-open .")  sometimes froze
        ;; emacs till the folder is closed. eg with nautilus
        ))))

  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "C-=") 'entropy/emacs-tools-show-in-desktop)))

;; ***** Open in terminal
(when (display-graphic-p)
  (defun entropy/emacs-tools-open-in-terminal ()
    "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2017-10-09"
    (interactive)
    (cond
     ((string-equal system-type "windows-nt")
      ;;(message "Microsoft Windows not supported bash shell, and we use cmd instead")
      (let* (($path-o (if (string-match-p "^~/" default-directory)
                          (replace-regexp-in-string "^~"
                                                    (expand-file-name "~")
                                                    default-directory)
                        default-directory))
             ($path-backslash (replace-regexp-in-string "/" "\\" $path-o t t))
             ($path (concat "\"" $path-backslash "\"")))
        (if entropy/emacs-wsl-terminal-enable
            (if (string-match-p "msys2_shell" entropy/emacs-wsl-terminal)
                ;; using msys2 mintty
                (w32-shell-execute "open" entropy/emacs-wsl-terminal
                                   (concat
                                    (completing-read "Choosing shell type: "
                                                     '("-mingw32"
                                                       "-mingw64"
                                                       "-msys2")
                                                     nil t)

                                    " -where "
                                    $path))
              ;; using git-for-windows terminal
              (w32-shell-execute "open" entropy/emacs-wsl-terminal))

          ;; using cmd
          (w32-shell-execute "open" "cmd" $path))))
     
     ((string-equal system-type "darwin")
      (let ((process-connection-type nil))
        (start-process "" nil "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal" default-directory)))
     
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil))
        (start-process "" nil "gnome-terminal" default-directory)))))
  (global-set-key (kbd "C-;") 'entropy/emacs-tools-open-in-terminal)

  (when sys/win32p
    (defun entropy/emacs-tools-cmd()
      (interactive)
      (if entropy/emacs-Cmder-enable
          (let (($path  default-directory ))
            (w32-shell-execute "open" entropy/emacs-Cmder-path (replace-regexp-in-string "/" "\\" $path t t)))
        (let (($path  default-directory ))
          (w32-shell-execute "open" "cmd" (replace-regexp-in-string "/" "\\" $path t t)))))))

;; **** entropy-open-with
(use-package entropy-open-with
  :if (display-graphic-p)
  :ensure nil
  :commands (entropy/open-with-dired-open
             entropy/open-with-buffer)
  :bind (:map entropy/emacs-top-keymap
         ("M-1" . entropy/open-with-buffer))
  :init
  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "M-RET") 'entropy/open-with-dired-open))
  :config
  (defun entropy/emacs-tools--open-with-port-stuffs-around (oldfunc &rest arg-rest)
    "when in `entropy/emacs-web-development-environment' advice
`entropy/open-with-port' for prevent open url with specific
development web-browser."
    (let ((entropy/emacs-web-development-environment nil))
      (apply oldfunc arg-rest)))
  (entropy/emacs-lazy-load-simple 'entropy-open-with
    (advice-add 'entropy/open-with-port :around #'entropy/emacs-tools--open-with-port-stuffs-around)))


;; *** vertical center display
(defun entropy/emacs-tools-vertical-center ()
  "Just vertical-center buffer without further operation supplied
like `recenter-top-bottom'."
  (interactive)
  (recenter-top-bottom '(middle)))

(global-set-key (kbd "C-l")  'entropy/emacs-tools-vertical-center)

(defun entropy/emacs-tools-vertical-to-bottom ()
  "Just vertical-bottom buffer without further operation supplied
like `recenter-top-bottom'."
  (interactive)
  (recenter-top-bottom -1))

(entropy/emacs-!set-key (kbd "C-l") 'entropy/emacs-tools-vertical-to-bottom)

;; *** beacon cursor blanking
(use-package beacon
  :commands (beacon-mode beacon-blink)
  :init
  (defun entropy/emacs-tools--beacon-blink-advice (&rest _)
    (unless (not (fboundp 'beacon-blink))
      (beacon-blink)))
  (advice-add 'windmove-do-window-select :after
              #'entropy/emacs-tools--beacon-blink-advice)
  (advice-add 'recenter-top-bottom :after
              #'entropy/emacs-tools--beacon-blink-advice))

;; *** visual-regexp
;; 
;; Visual-regexp for Emacs is like replace-regexp, but with live
;; visual feedback directly in the buffer.
(use-package visual-regexp
  :commands (vr/replace vr/query-replace)
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace))
  :config
  (dolist (el '(vr--do-replace vr--perform-query-replace vr--interactive-get-args))
    (advice-add el :around #'entropy/emacs-case-fold-focely-around-advice)))


;; *** ialign
(use-package ialign
  :commands (ialign))

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
  ;; you can active follow code block for start atomic server with startup of emacs.
  ;;      :init (atomic-chrome-start-server)
  :commands (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode))))


;; *** Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode))
  :config

  (defun entropy/emacs-tools--dmm-prune-sections (dmm-sections)
    (let ((dmm-sections-copy (copy-sequence dmm-sections))
          (rules '("next-line" "previous-line" "left-char" "right-char"))
          rtn)
      (dolist (section dmm-sections-copy)
        (let* ((group-name (car section))
               (bindings (cdr section))
               (cpbdins (copy-sequence bindings))
               cache)
          (dolist (binding bindings)
            (unless (member (cdr binding) rules)
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
  (when (window-margins)
    (when (fboundp 'entropy/emacs-basic-center-text-clear)
      (entropy/emacs-basic-center-text-clear)))
  (if (> (length (window-list)) 1) (delete-other-windows))
  (progn
    (split-window-horizontally
     (ceiling (* 0.3
                 (frame-width))))
    (other-window 1)))
(global-set-key (kbd "C-x M-3") 'entropy/emacs-tools-horizonal-split-window)

;; **** Configure network proxy
;; **** entropy-emacs version show
(defun entropy/emacs-tools-entropy-emacs-version ()
  "Show entropy-emacs version."
  (interactive)
  (message entropy/emacs-ecv))

;; **** show time
(defun entropy/emacs-tools-time-show ()
  "Show current time with date information also."
  (interactive)
  (let ((time (format-time-string "%Y-%m-%d %a %H:%M:%S")))
    (message "Now is %s" time)))
(global-set-key (kbd "<f12>") 'entropy/emacs-tools-time-show)

;; *** encoding and end-of-line conversation
(defun entropy/emacs-tools-dos2unix-internal ()
  "Exchange the buffer end-of-line type to unix sytle."
  (interactive)
  (entropy/cl-backup-file (buffer-file-name))
  (revert-buffer-with-coding-system 'dos t)
  (set-buffer-file-coding-system 'unix)
  (if buffer-read-only
      (read-only-mode 0))
  (save-buffer)
  (revert-buffer nil 'revert-without-query)
  (read-only-mode 1))

(defun entropy/emacs-tools-save-buffer-as-utf8-internal (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (entropy/cl-backup-file (buffer-file-name))
  (revert-buffer-with-coding-system coding-system)
  (if (yes-or-no-p (format "Does encoding with '%s' display correctly? " coding-system))
      (progn
        (set-buffer-file-coding-system 'utf-8-unix)
        (if buffer-read-only
            (read-only-mode 0))
        (save-buffer)
        (revert-buffer nil 'revert-without-query)
        (read-only-mode 1))
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


;; *** Foreign language realtime translation
(defun entropy/emacs-tools-toggle-dict (&optional default)
  "Toggle foreign language translate proxy engine interactively.

Note:

For now, there's three choices for you:

1) bingdict

   Bing dict proxy was really simple and be prompt just using
   minibuffer.

   It's really benefit for instantly know what you want for the
   current word.

2) youdaodict

   Perfectly with child-frame display just like company-mode's
   candidates.

3) google-translation

   Google translation translation engine was known as it's wild
   coverage for variant foreign languages."
  (interactive)
  (let* ((choice
          (if (not default)
              (ivy-read "Choose your choice: " '("youdao" "bing" "google" "sdcv")
                        :require-match t
                        :initial-input "sdcv")
            default)))
    (cond
     ((string= choice "youdao")
      (progn
        (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point-tooltip)
        (global-set-key (kbd "C-c M-y") 'youdao-dictionary-search-from-input)))
     ((string= choice "bing")
      (progn
        (global-set-key (kbd "C-c y") 'entropy/emacs-tools-bing-dict-brief-direct)
        (global-set-key (kbd "C-c M-y") 'entropy/emacs-tools-bing-dict-brief-prompt)))
     ((string= choice "google")
      (progn
        (global-set-key (kbd "C-c y") 'entropy/emacs-tools-google-translate-at-point-direct-en-CN)
        (global-set-key (kbd "C-c M-y") 'entropy/emacs-tools-google-translate-prompt-direct-en-CN)))
     ((string= choice "sdcv")
      (global-set-key (kbd "C-c y") 'entropy/sdcv-search-at-point-tooltip)
      (global-set-key (kbd "C-c M-y") 'entropy/sdcv-search-input-adjacent)))))

(entropy/emacs-tools-toggle-dict (symbol-name entropy/emacs-dictionary-backend))

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
   youdao-dictionary-search-from-input)
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
      '(setq google-translate--tkk-url "http://translate.google.cn/")))
  :config
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
  ;; :bind  (("C-c y" . entropy/emacs-tools-bing-dict-brief-direct)
  ;;         ("C-c M-y" . entropy/emacs-tools-bing-dict-brief-prompt))
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
             entropy/sdcv-search-input-adjacent)
  :config
  (entropy/emacs-lazy-load-simple 'entropy-sdcv
    (defun entropy/sdcv--lang-advice (&rest args)
      (entropy/emacs-lang-set-utf-8))
    (advice-add 'entropy/sdcv-search-at-point-tooltip :before #'entropy/sdcv--lang-advice)
    (advice-add 'entropy/sdcv-search-input-adjacent :before #'entropy/sdcv--lang-advice)))

;; *** chinese dict
(use-package entropy-cn-dict
  :ensure nil
  :commands entropy/cndt-query
  :bind (("C-x y" . entropy/cndt-query)))

;; *** Log keyboard commands to buffer
;;     Show event history and command history of some or all buffers.
(use-package command-log-mode
  :diminish (command-log-mode . "¢")
  :commands (command-log-mode)
  :init
  (setq command-log-mode-auto-show t)
  (defvar command-log-mode nil)
  (defun entropy/emacs-tools-command-log-mode ()
    (interactive)
    (if (not command-log-mode)
        (progn (command-log-mode)
               (entropy/emacs-lazy-load-simple 'command-log-mode
                 (clm/toggle-command-log-buffer)))
      (command-log-mode 0))))

;; *** pomidor A simple and cool pomodoro timer
(use-package pomidor
  :commands (pomidor)
  :bind (("C-c <f12>" . pomidor)))

;; *** maple preview
(when (equal entropy/emacs-use-extensions-type 'submodules)
  (use-package maple-preview
    :ensure nil
    :commands (maple-preview-mode)
    :init
    (setq maple-preview:allow-modes '(org-mode markdown-mode gfm-mode html-mode web-mode))
    :config
    (defun entropy/emacs-tools-maple-preview:schema-auto-hooks ()
      (dolist ($el '(windmove-left
                     windmove-right
                     windmove-up
                     windmove-down))
        (advice-add $el :after #'maple-preview:send-to-server))
      (when (featurep 'eyebrowse)
        (entropy/emacs-lazy-load-simple 'eyebrowse
          (advice-add 'eyebrowse-switch-to-window-config
                      :after
                      #'maple-preview:send-to-server)))
      (advice-add 'other-window :after #'maple-preview:send-to-server)
      (when (featurep 'markdown-mode)
        (entropy/emacs-lazy-load-simple 'markdown-mode
          (advice-add 'markdown-outdent-or-delete
                      :after
                      #'maple-preview:send-to-server)))
      (advice-add 'backward-delete-char-untabify
                  :after #'maple-preview:send-to-server))

    (defun entropy/emacs-tools--maple-preview:schema-finialize-hooks ()
      (dolist ($el '(windmove-left
                     windmove-right
                     windmove-up
                     windmove-down))
        (advice-remove $el #'maple-preview:send-to-server))
      (when (and (featurep 'eyebrowse)
                 (fboundp 'eyebrowse-switch-to-window-config))
        (advice-remove 'eyebrowse-switch-to-window-config
                       #'maple-preview:send-to-server))
      (advice-remove 'other-window #'maple-preview:send-to-server)
      (when (and (featurep 'markdown-mode)
                 (fboundp 'markdown-outdent-or-delete))
        (advice-remove 'markdown-outdent-or-delete
                       #'maple-preview:send-to-server))
      (advice-remove 'backward-delete-char-untabify
                     #'maple-preview:send-to-server))

    (add-hook 'maple-preview:auto-hook #'entropy/emacs-tools-maple-preview:schema-auto-hooks)
    (add-hook 'maple-preview:finialize-hook #'entropy/emacs-tools--maple-preview:schema-finialize-hooks)))

;; *** display world clock
(use-package counsel-world-clock
  :commands  (counsel-world-clock)
  :bind ("C-x <f12>" . counsel-world-clock))

;; ** Misc
;; *** copy path, url, etc.
(use-package copyit
  :commands
  (copyit-file-as-data-uri
   copyit-file-content
   copyit-file-exif-information
   copyit-file-pathname
   copyit-ssh
   copyit-variable))

;; *** Emacs startup profiler
(use-package esup
  :commands (esup))

;; *** for generate elisp source file's commentry structure to org file
(use-package el2org
  :commands (el2org-generate-html
             el2org-generate-org
             el2org-generate-readme))


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

(defalias 'ehome 'entropy/emacs-tools-goto-sys-home "Alias for entropy/emacs-tools-goto-sys-home.")

;; *** firefox bookmarks and history query and open
(use-package counsel-ffdata
  :commands (counsel-ffdata-firefox-bookmarks
             counsel-ffdata-firefox-history)
  :init
  (setq counsel-ffdata-database-path
        (ignore-errors
          (cl-case system-type
            ((gnu gnu/linux gnu/kfreebsd)
             (expand-file-name
              (car (file-expand-wildcards
                    "~/.mozilla/firefox/*.default-release/places.sqlite"))))
            (windows-nt
             (car (file-expand-wildcards
                   (expand-file-name "Mozilla/Firefox/Profiles/*/places.sqlite"
                                     (getenv "APPDATA")))))))))

;; *** visual-ascii-mode

  ;; Show key-binding with readable style instead of ascii numberic
  ;; sequence

(use-package visual-ascii-mode
  :commands (global-visual-ascii-mode visual-ascii-mode)
  :bind (:map help-mode-map
              ("v" . visual-ascii-mode)))

;; ** entropy-emacs self packages

;; *** entropy-proxy-url
(use-package entropy-proxy-url
  :ensure nil
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
  
  :init

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
    (advice-add 'w3m-retrieve :before #'entropy/emacs-tools--w3m-recorde-retrieve-url)
    (define-key w3m-mode-map (kbd "p") #'entropy/emacs-tools-w3m-toggle-proxy))
  
  (entropy/emacs-lazy-load-simple 'w3m
    (unless (eq entropy/emacs-proxy-url-loaded t)
      (entropy/proxy-url-make-builtin-recipes)
      (when (executable-find "w3m")
        (entropy/emacs-tools--proxy-url-w3m-specific))
      (setq entropy/emacs-proxy-url-loaded t)))

  (entropy/emacs-lazy-load-simple 'eww
    (unless (eq entropy/emacs-proxy-url-loaded t)
      (entropy/proxy-url-make-builtin-recipes)
      (setq entropy/emacs-proxy-url-loaded t))))

;; *** entropy-projec-manager
(use-package entropy-prjm
  :ensure nil
  :commands entropy/prjm-inct-chosen-db
  :bind ("C-c M-p" . entropy/prjm-inct-chosen-db))

;; *** entropy-unfill
(use-package entropy-unfill
  :ensure nil
  :commands (entropy/unfill-full-buffer-without-special-region
             entropy/unfill-paragraph
             entropy/fill-full-buffer-without-special-region))

;; *** entropy-org-batch-refile
(use-package entropy-org-batch-refile
  :ensure nil
  :commands entropy/org-batch-refile-tags-read-and-do)

;; *** entropy-cp-or-mv
(use-package entropy-dired-cp-or-mv
  :ensure nil
  :commands (entropy/cpmv-dired-get-files-list
             entropy/cpmv-to-current))

;; *** entropy-counsel-stuffs
(use-package entropy-counsel-stuffs
  :ensure nil
  :bind (("M-<f12>" . entropy/cs-recorde-entry)
         ("C-<f12>" . entropy/cs-open-all))
  :commands (entropy/cs-filter-open
             entropy/cs-recorde-entry
             entropy/cs-converter
             entropy/cs-modifiy
             entropy/cs-delete))

;; *** entropy-portableapps
(use-package entropy-portableapps
  :if sys/is-win-group
  :ensure nil
  :commands (entropy/poapps-query-open)
  :bind (:map entropy/emacs-top-keymap
         ("<M-up>" . entropy/poapps-query-open)))

;; *** entropy-epub2org
(use-package entropy-epub2org
  :ensure nil
  :commands (entropy/ep2o-dispatcher
             entropy/ep2o-src-adjusting-manually)
  :config
  (defun entropy/emacs-tools--ep2o-tidy-up-image-width-defaut ()
    (unless (equal major-mode 'org-mode)
      (org-mode)
      (outline-show-all)
      (org-show-block-all))
    (add-file-local-variable 'org-image-actual-width 500))
  (add-hook 'entropy/ep2o-tidy-hook #'entropy/emacs-tools--ep2o-tidy-up-image-width-defaut))


;; * provide
(provide 'entropy-emacs-tools)
