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
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'subr-x)

;; ** openwith external apps
;; *** openwith config
(use-package openwith
  :if (or sys/win32p sys/linux-x-p sys/mac-x-p)
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

;; *** Function manually
;; **** open in external apps
(when  (or sys/win32p sys/linux-x-p sys/mac-x-p)
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

;; **** Open in desktop manager
(when (or sys/win32p sys/linux-x-p sys/mac-x-p)
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

;; **** Open in terminal
(when (or sys/win32p sys/linux-x-p sys/mac-x-p)
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

;; *** entropy-open-with
(use-package entropy-open-with
  :if (or sys/win32p sys/linux-x-p sys/mac-x-p)
  :ensure nil
  :commands (entropy/open-with-dired-open
             entropy/open-with-buffer)
  :bind (("C-M-1" . entropy/open-with-buffer))
  :init
  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "<C-M-return>") 'entropy/open-with-dired-open)))


;; ** vertical center display
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

(global-set-key (kbd "C-M-l") 'entropy/emacs-tools-vertical-to-bottom)

;; ** beacon cursor blanking
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

;; ** visual-regexp
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


;; ** ialign
(use-package ialign
  :commands (ialign))

;; ** Firefox edit use emacs
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


;; ** entropy-proxy-url
(use-package entropy-proxy-url
  :ensure nil
  :commands (entropy/proxy-url-proxy-choice-for-eww
             entropy/proxy-url-proxy-choice-for-w3m)
  :init
  (advice-add 'eww-browse-url :before #'entropy/proxy-url-proxy-choice-for-eww)
  (advice-add 'w3m-goto-mailto-url :before #'entropy/proxy-url-proxy-choice-for-w3m)
  (advice-add 'w3m-goto-ftp-url :before #'entropy/proxy-url-proxy-choice-for-w3m)
  (advice-add 'w3m--goto-url--valid-url :before #'entropy/proxy-url-proxy-choice-for-w3m))

;; ** eww config
;; *** eww search engine
(if entropy/emacs-eww-search-engine-customize
    (setq eww-search-prefix entropy/emacs-eww-search-engine))

;; *** disable eww image animation for reducing the performance lagging
(setq-default shr-image-animate nil)

;; *** get image url
(entropy/emacs-lazy-load-simple 'shr
  (defun entropy/emacs-tools-get-eww-url (choice)
    "Get image or point url at eww or it's derived modes."
    (interactive
     (list (completing-read "Choose url copy type" '("image-url" "shr-url"))))
    (let ((url (cond ((equal choice "image-url") (get-text-property (point) 'image-url))
                     ((equal choice "shr-url") (get-text-property (point) 'shr-url)))))
      (if url
          (progn
            (kill-new url)
            (message "Copy %s: '%s' ." choice url)
            url)
        (error (format "Can not find %s here!" choice)))))

  (entropy/emacs-lazy-load-simple 'eww
    (define-key eww-mode-map (kbd "m") #'entropy/emacs-tools-get-eww-url))
  (entropy/emacs-lazy-load-simple 'elfeed-show
    (define-key elfeed-show-mode-map (kbd "m") #'entropy/emacs-tools-get-eww-url)))


;; *** eww proxy toggle
(entropy/emacs-lazy-load-simple 'entropy-proxy-url
  (entropy/emacs-lazy-load-simple 'eww
    (define-key eww-mode-map (kbd "p") #'entropy/proxy-url-switch-for-eww)))


;; *** eww open url outside
(defun entropy/emacs-tools-eww-open-url-external ()
  "Open current eww web page on external browser.

Browser chosen based on variable
`browse-url-browser-function'. In entropy-emacs they can be:

1. w3m cli backend fronts to emacs-w3m
2. personal browser specifiction rely on `entropy/emacs-browse-url-function'.
3. default browser detecting as `browse-url-default-browser'.
"
  (interactive)
  (unless (not (equal major-mode 'eww-mode))
    (let ((url (eww-copy-page-url))
          (browse-url-browser-function
           (let (rtn choices)
             (when (functionp entropy/emacs-browse-url-function)
               (add-to-list 'choices `("personal" ,entropy/emacs-browse-url-function)))
             (when (executable-find "w3m")
               (add-to-list 'choices `("w3m" ,(lambda (url &rest args)
                                              (w3m-goto-url url)))))
             (add-to-list 'choices '("default" browse-url-default-browser))
             (setq rtn (completing-read "Choice external browser:  "
                                        choices nil t))
             (setq rtn (nth 1 (assoc rtn choices)))
             rtn)))
      (browse-url url))))

(entropy/emacs-lazy-load-simple 'eww
  (define-key eww-mode-map (kbd "M") #'entropy/emacs-tools-eww-open-url-external))

;; ** Rss feed
(use-package elfeed
  :commands (elfeed)
  :bind (:map elfeed-search-mode-map
              ("A" . entropy/emacs-tools-elfeed-filter-by-feedname)
              ("B" . entropy/emacs-tools-elfeed-filter-by-tag)
              ("-" . entropy/emacs-tools-elfeed-untag-selected)
              ("+" . entropy/emacs-tools-elfeed-tag-selected)
              ("g" . entropy/emacs-tools-elfeed-format-feed-title)
              ("s" . entropy/emacs-tools-elfeed-clean-filter)
         :map elfeed-show-mode-map
              ("q" . entropy/emacs-tools-elfeed-kill-buffer))

  :init
  (defvar entropy/emacs-tools--elfeed-init nil
    "whether the firt time calling `elfeed'.")
  
  (defun entropy/emacs-tools--elfeed-db-load (&rest rest)
    "'BEFORE' advice for elfeed for fix bug of `elfeed-db-load'
read wrong buffer stirng for `elfeed-db' at init time.

This may be the problem of `use-package' lazy loading of elfeed
and macro expand of it."
    (when (not entropy/emacs-tools--elfeed-init)
      (let ((index (expand-file-name "index" elfeed-db-directory))
            (enable-local-variables nil))
        (when (file-exists-p index) 
          (with-current-buffer (find-file-noselect index :nowarn)
            (fundamental-mode)
            (goto-char (point-min))
            (require 'elfeed-db)
            (if (eql elfeed-db-version 4)
                ;; May need to skip over dummy database
                (let ((db-1 (read (current-buffer)))
                      (db-2 (ignore-errors (read (current-buffer)))))
                  (setf elfeed-db (or db-2 db-1)))
              ;; Just load first database
              (setf elfeed-db (read (current-buffer))))
            (kill-buffer))
          (setf elfeed-db-feeds (plist-get elfeed-db :feeds)
                elfeed-db-entries (plist-get elfeed-db :entries)
                elfeed-db-index (plist-get elfeed-db :index))))
      (setq entropy/emacs-tools--elfeed-init t)))

  (advice-add 'elfeed :before #'entropy/emacs-tools--elfeed-db-load)
  
  (setq elfeed-search-date-format '("%Y/%m/%d-%H:%M" 16 :left))
  (setq elfeed-curl-timeout 20)

  ;; set curl path
  (let ((mingw-curl (if (and entropy/emacs-wsl-enable
                             (file-exists-p entropy/emacs-wsl-apps))
                        (expand-file-name
                         "mingw64/bin/curl.exe"
                         (substring (directory-file-name entropy/emacs-wsl-apps) 0 -7))
                      nil))
        (msys2-curl (if (and entropy/emacs-wsl-enable
                             (file-exists-p entropy/emacs-wsl-apps))
                        (expand-file-name
                         "curl.exe" entropy/emacs-wsl-apps)))
        (w32-curl "c:/WINDOWS/system32/curl.exe")
        (unix-curl "curl"))
    (cond
     ((ignore-errors (file-exists-p w32-curl))
      (setq elfeed-curl-program-name w32-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p mingw-curl))
      (setq elfeed-curl-program-name mingw-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p msys2-curl))
      (setq elfeed-curl-program-name msys2-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (and (executable-find "curl")
                          (or sys/linuxp sys/macp)))
      (setq elfeed-use-curl t))
     (t
      (setq elfeed-use-curl nil))))


  :config
;; *** feeds-title config
  (defun entropy/emacs-tools--elfeed-sc-str (str)
    "Replaces space for '-' when string STR indeed of that."
    (let ((strlist (split-string str " "))
          rtn)
      (if (member "" strlist)
          (setq strlist (delete "" strlist)))
      (dolist (el strlist)
        (if (not rtn)
            (setq rtn el)
          (setq rtn (concat rtn "-" el))))
      rtn))

  (eval-when-compile
    (defun entropy/emacs-tools--elfeed-string-style-hook (&rest args)
      "Hooks for replace space to '-' when save `elfeed-db'."
      (let ((feeds (if (hash-table-p elfeed-db-feeds)
                       (hash-table-values elfeed-db-feeds)
                     nil))
            did)
        (when feeds
          (dolist (el feeds)
            (let ((feed-title (elfeed-feed-title el))
                  newtitle)
              (when (and feed-title (string-match-p " " feed-title))
                (setq newtitle (entropy/emacs-tools--elfeed-sc-str feed-title))
                (setf (elfeed-feed-title el) newtitle)
                (setq did t))))
          (if did
              t
            nil)))))

  (advice-add 'elfeed-db-load :after #'entropy/emacs-tools--elfeed-string-style-hook)


  (defun entropy/emacs-tools-elfeed-format-feed-title ()
    "Interactively format feedtitle which has space."
    (interactive)
    (if (equal major-mode 'elfeed-search-mode)
        (progn
          (entropy/emacs-tools--elfeed-string-style-hook)
          (elfeed-search-update--force))
      (error "You are not in the 'elfeed-search-mode'!")))
  
;; *** Query prompting filter function
  (defun entropy/emacs-tools--elfeed-get-all-entries ()
    "Get all entries from `elfeed-db' and return it."
    (hash-table-values (plist-get elfeed-db :entries)))
  
  (defun entropy/emacs-tools--elfeed-read-feedname (entry)
    "Get entry's feed name."
    (let* ((feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed)))))
      (if feed-title
          feed-title)))
  
  (defun entropy/emacs-tools--elfeed-tags-choice (selected-entries &optional tag return-list prompt match)
    "Query and selected tag from the list collected by
SELECTED-ENTRIES. And final return the tag string or the list of
matched entires by this tag.

Arguemnts:

- tag:         if specific it as a string, skip the step of query prompt,
               and using the value as the choice.

- return-list: if non-nil ,then this function return the matched
               entries list, otherwise return tag string

- prompt:      string for specific the query prompt.

- match:       whether require match with query prompt step."
    (if (equal major-mode 'elfeed-search-mode)
        (let (rtn entries tags-list choice)
          ;; extract entries by selected which have existed tags and make initial tags-list list.
          ;; [2018-08-30 Thu 02:32:49] this can be replaced with `elfeed-db-get-all-tags' for full
          ;; tags search.

          (let ((entl selected-entries))
            (dolist (el entl)
              (when (listp (elfeed-entry-tags el))
                (push el entries)
                (mapcar #'(lambda (x)
                            (add-to-list 'tags-list x))
                        (elfeed-entry-tags el)))))
          ;; read user choice
          (if (not tag)
              (setq choice (ivy-read (if (not prompt)
                                         "Choose tag: "
                                       prompt)
                                     tags-list
                                     :require-match (if match t nil)))
            (setq choice tag))

          (when (string-match-p " " choice)
            (setq choice (entropy/emacs-tools--elfeed-sc-str choice)))

          (if return-list
              ;; match entries of choice
              (dolist (el entries)
                (if (member (intern choice) (elfeed-entry-tags el))
                    (push el rtn)))
            (setq rtn (intern choice)))
          rtn)
      (error "Unmatched major-mode of 'elfeed-search-mode'!")))

  (defun entropy/emacs-tools--elfeed-feedname-choice (selected-entries &optional tname return-list)
    "Query with promt for choosing one feedname from the selected
entries, return the choice or one entries list wich matched this
choice.

Arguments:

- tname:       specific feedname without manully choosing one from query prompting.

- return-list: if non-nil then return the matched entries list
               instead of returning the feedname."
    (let ((entry-list selected-entries)
          feedtitle-name-list
          entries
          choice
          rtn)
      ;; make feed title name list and matched entries list
      (dolist (el entry-list)
        (let ((feed-title (entropy/emacs-tools--elfeed-read-feedname el)))
          (when feed-title
            (add-to-list 'feedtitle-name-list feed-title)
            (push el entries))))

      (if tname
          (setq choice tname)
        (setq choice (ivy-read "Choose feed title: " feedtitle-name-list)))
      
      (when (string-match-p " " choice)
        (setq choice (entropy/emacs-tools--elfeed-sc-str choice)))
      
      (if return-list
          (dolist (el entries)
            (if (equal choice (entropy/emacs-tools--elfeed-read-feedname el))
                (push el rtn)))
        (setq rtn choice))
      rtn))

  (defun entropy/emacs-tools-elfeed-filter-by-tag (tag)
    "Filter with tag choosen, powered by `entropy/emacs-tools--elfeed-tags-choice'"
    (interactive
     (list (entropy/emacs-tools--elfeed-tags-choice (entropy/emacs-tools--elfeed-get-all-entries))))
    (elfeed-search-set-filter (concat "+" (symbol-name tag))))
  
  (defun entropy/emacs-tools-elfeed-filter-by-feedname (feedname)
    "Filter with feedname, powered by `entropy/emacs-tools--elfeed-feedname-choice'."
    (interactive
     (list (entropy/emacs-tools--elfeed-feedname-choice (entropy/emacs-tools--elfeed-get-all-entries))))
    (if (entropy/emacs-tools--elfeed-string-style-hook)
        (elfeed-search-update--force))
    (elfeed-search-set-filter (concat "=" feedname)))

  (defun entropy/emacs-tools-elfeed-untag-selected ()
    "Untag tag for selected entries with query prompt as
requiring matched."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (choice (entropy/emacs-tools--elfeed-tags-choice entries nil nil "Choose tag for remove: " t)))
      (elfeed-untag entries choice)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun entropy/emacs-tools-elfeed-tag-selected ()
    "Adding tag for selected entries with query prompt for
selecting existing tag or input the new one instead."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (full-entrylist (entropy/emacs-tools--elfeed-get-all-entries))
           (tag (entropy/emacs-tools--elfeed-tags-choice full-entrylist nil nil "Choose tag or input one: ")))
      (elfeed-tag entries tag)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

;; *** other improvement
  (defun entropy/emacs-tools-elfeed-kill-buffer ()
    "Automatically swtitch to elfeed-search buffer when closed
the current elfeed-show-buffer."
    (interactive)
    (if (not (equal major-mode 'elfeed-show-mode))
        (kill-buffer (current-buffer))
      (progn
        (kill-buffer (current-buffer))
        (let* ((bfl (mapcar #'(lambda (x) (buffer-name x))
                            (buffer-list))))
          (if (member "*elfeed-search*" bfl)
              (progn (switch-to-buffer "*elfeed-search*" nil t)
                     (message "Back to *elfeed-search* buffer."))
            (user-error "Couldn't found *elfeed-search* buffer."))))))


  (defun entropy/emacs-tools-elfeed-clean-filter ()
    "Clean all filter for curren elfeed search buffer."
    (interactive)
    (elfeed-search-set-filter ""))
  
;; **** utilities
  (defun entropy/emacs-tools--elfeed-list-feeds ()
    "List feeds using for querying promt."
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          (rtn nil))
      (let ((mlist feeds))
        (dolist (el feeds)
          (if (not (member (elfeed-feed-id el) elfeed-feeds))
              (setq mlist (delete el mlist))))
        (setq feeds mlist))
      (let ((pair-list nil))
        (dolist (el feeds)
          (setq pair-list `(,(elfeed-feed-title el) . ,(elfeed-feed-id el)))
          (push pair-list rtn)))
      (let ((mlist nil))
        (dolist (el rtn)
          (if (car el)
              (push `(,(concat (car el) " ↭ " (cdr el)) . ,(cdr el)) mlist)
            (push `(,(concat "☹nil ↭ " (cdr el)) . ,(cdr el)) mlist)))
        (setq rtn mlist))
      rtn))

  (defun entropy/emacs-tools-elfeed-clean-invalid-feeds ()
    "Clean invalid feeds which can not be retrieved.

This function will remove 'as entry' both in `elfeed-db' and
`elfeed-feeds' which also will automatically stored in
`custom-file'."
    (interactive)
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          mlist)
      (dolist (el feeds)
        (when (and (not (elfeed-feed-url el))
                   (member (elfeed-feed-id el) elfeed-feeds))
          (push `(,(elfeed-feed-id el) . ,el) mlist)))
      (dolist (el mlist)
        (setq elfeed-feeds (delete (car el) elfeed-feeds)))
      (customize-save-variable 'elfeed-feeds elfeed-feeds)
      (elfeed-db-gc-empty-feeds)))

  (defvar entropy/emacs-tools--elfeed-feed-prompt-alist '()
    "Alist of feeds prompt string and url.")
  
;; **** add feed advice
  (defun entropy/emacs-tools-elfeed-add-feed-around (oldfunc url)
    "Addding url and hexify it when it contained multi-byte
string as CJK characters."
    (interactive (list
                  (read-string "Url: ")))
    (require 'entropy-common-library-const)
    (let ((hexi-url (url-hexify-string url entropy/cl-url--allowed-chars)))
      (funcall oldfunc hexi-url :save t)))
  (advice-add 'elfeed-add-feed :around #'entropy/emacs-tools-elfeed-add-feed-around)


;; **** delete entry
  (defun entropy/emacs-tools-elfeed-delete-entry ()
    "Delete entry of elfeed."
    (interactive)
    (if (not (use-region-p))
        (let* ((entry (elfeed-search-selected t))
               (id  (elfeed-entry-id entry))
               (n-entry elfeed-db-entries))
          (avl-tree-delete elfeed-db-index id)
          (remhash id n-entry)
          (setq elfeed-db-entries n-entry)
          (entropy/emacs-tools-elfeed-format-feed-title))
      (let* ((entries (elfeed-search-selected))
             id
             (n-entry elfeed-db-entries))
        (dolist (el entries)
          (add-to-list 'id (elfeed-entry-id el)))
        (dolist (el id)
          (avl-tree-delete elfeed-db-index el))
        (dolist (el id)
          (remhash el n-entry))
        (setq elfeed-db-entries n-entry)
        (entropy/emacs-tools-elfeed-format-feed-title))))

  (define-key elfeed-search-mode-map (kbd "d") 'entropy/emacs-tools-elfeed-delete-entry)

;; **** remove feed function
  (defvar entropy/emacs-tools--elfeed-feed-remove-list '()
    "List stored feeds url of `elfeed-feeds' to remove.")
  
  (defun entropy/emacs-tools--elfeed-feed-of-url (url)
    "Matching url's refer feed title name."
    (let* ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
           (rtn nil))
      (dolist (el feeds)
        (if (equal url (elfeed-feed-url el))
            (setq rtn (elfeed-feed-title el))))
      (if rtn
          rtn
        "-v-")))

  (defun entropy/emacs-tools--elfeed-remove-read-action (x)
    "Repeatedly read action for removing feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (let ((temp (cdr x)))
      (setq x temp))
    (entropy/cl-ivy-read-repeatedly-function
     x 'entropy/emacs-tools--elfeed-feed-remove-list
     "Removing:"
     #'entropy/emacs-tools--elfeed-feed-of-url))
  
  (defun entropy/emacs-tools-elfeed-remove-feed ()
    "Remove elfeed feeds with multi-chosen by query prompted
function of `ivy-read'."
    (interactive)
    (setq entropy/emacs-tools--elfeed-feed-remove-list nil
          entropy/emacs-tools--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-tools--elfeed-feed-prompt-alist (entropy/emacs-tools--elfeed-list-feeds))
    (ivy-read "Remove feeds:" entropy/emacs-tools--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-tools--elfeed-remove-read-action)
    (let ((rtn elfeed-feeds))
      (dolist (el entropy/emacs-tools--elfeed-feed-remove-list)
        (when (member el elfeed-feeds)
          (setq rtn (delete el rtn))))
      (customize-save-variable 'elfeed-feeds rtn)
      (when (yes-or-no-p "Do you want to remove all empty feeds? ")
        (elfeed-db-gc-empty-feeds))))

  (defun entropy/emacs-tools-elfeed-remove-feed-by-regexp ()
    "Remove feeds matched of regexp expr inputted repeatlly."
    (interactive)
    (let* ((regexp (entropy/cl-repeated-read "Input regexp"))
           (feeds (elfeed-feed-list))
           mlist
           (rtn feeds))
      (dolist (el regexp)
        (dolist (elm feeds)
          (when (string-match-p el elm)
            (add-to-list 'mlist elm))))
      (dolist (el mlist)
        (setq rtn (delete el rtn)))
      (setq elfeed-feeds rtn)
      (customize-save-variable 'elfeed-feeds elfeed-feeds)))
  
;; **** update specific feed through proxy
  (defvar entropy/emacs-tools-elfeed-multi-update-feeds-list '()
    "Feeds for update.")

  (defun entropy/emacs-tools--elfeed-update-read-action (x)
    "Repeatly read action for updating feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (let ((temp (cdr x)))
      (setq x temp))
    (entropy/cl-ivy-read-repeatedly-function
     x 'entropy/emacs-tools-elfeed-multi-update-feeds-list
     "Updating: "
     #'entropy/emacs-tools--elfeed-feed-of-url))

  (defun entropy/emacs-tools-elfeed-get-multi-update-feeds ()
    "Getting feeds needed for updating through querying with
promptings and injecting them into `entropy/emacs-tools-elfeed-multi-update-feeds-list'."
    (interactive)
    (setq entropy/emacs-tools-elfeed-multi-update-feeds-list nil
          entropy/emacs-tools--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-tools--elfeed-feed-prompt-alist (entropy/emacs-tools--elfeed-list-feeds))
    (ivy-read "Update feeds: " entropy/emacs-tools--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-tools--elfeed-update-read-action))
  
  (defun entropy/emacs-tools-elfeed-multi-update-feeds ()
    "Update feeds interactively by multiplied choicing from `entropy/emacs-tools--elfeed-feed-prompt-alist'."
    (interactive)
    (setq entropy/emacs-tools-elfeed-multi-update-feeds-list nil
          entropy/emacs-tools--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-tools--elfeed-feed-prompt-alist (entropy/emacs-tools--elfeed-list-feeds))
    (ivy-read "Update feeds: " entropy/emacs-tools--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-tools--elfeed-update-read-action)
    (dolist (el entropy/emacs-tools-elfeed-multi-update-feeds-list)
      (elfeed-update-feed el)))

  (defun entropy/emacs-tools--elfeed-update-curl-proxy (url-lists proxy)
    (when elfeed-curl-extra-arguments
      (let (mlist (olist elfeed-curl-extra-arguments))
        (dolist (el olist)
          (when (string-match-p "-x.*://" el)
            (setq elfeed-curl-extra-arguments (delete el elfeed-curl-extra-arguments))))))
    (setq elfeed-curl-extra-arguments (append elfeed-curl-extra-arguments
                                              (list (concat "-x" proxy))))
    (dolist (el url-lists)
      (elfeed-update-feed el)))


  (defun entropy/emacs-tools--elfeed-update-urlretrieve-proxy (url-lists)
    (if (not (boundp 'entropy/proxy-mode))
        (entropy/proxy-mode)
      (if (not entropy/proxy-mode)
          (entropy/proxy-mode)))
    (unless (< (length url-lists) 5)
      (error "Too much feeds selected, it will cause lagging, reducing them under 5."))
    (let ((elfeed-use-curl nil))
      (dolist (el url-lists)
        (elfeed-update-feed el)))
    (entropy/proxy-mode-disable))

  (defun entropy/emacs-tools-elfeed-update-proxy (&optional url-lists )
    "Update feeds using proxy."
    (interactive)
    (let ((ulist (if url-lists url-lists (progn (entropy/emacs-tools-elfeed-get-multi-update-feeds)
                                                entropy/emacs-tools-elfeed-multi-update-feeds-list))))
      (if elfeed-use-curl
          (let ((proxy (ivy-read "Choose or input your proxy: "
                                 '("http://127.0.0.1:1080"))))
            (entropy/emacs-tools--elfeed-update-curl-proxy ulist proxy))
        (entropy/emacs-tools--elfeed-update-urlretrieve-proxy ulist))))


  (defun entropy/emacs-tools--elfeed-reset-curl-arg ()
    (let ((judge nil)
          (mlist nil))
      (when elfeed-curl-extra-arguments
        (dolist (el elfeed-curl-extra-arguments)
          (when (string-match-p "-x.*://" el)
            (setq judge t)
            (push el mlist)))
        (when judge
          (cond
           ((= 1 (length elfeed-curl-extra-arguments))
            (setq elfeed-curl-extra-arguments nil))
           (t
            (dolist (el mlist)
              (setq elfeed-curl-extra-arguments
                    (delete el elfeed-curl-extra-arguments)))))))))

  (advice-add 'elfeed-update :before #'entropy/emacs-tools--elfeed-reset-curl-arg)

  (defun entropy/emacs-tools-elfeed-proxy-update-all-nil-feeds ()
    "Update all empty feeds with proxy by `entropy/emacs-tools-elfeed-update-proxy'."
    (interactive)
    (let ((olist (entropy/emacs-tools--elfeed-list-feeds))
          mlist rlist)
      (dolist (el olist)
        (when (string-match-p "☹nil" (car el))
          (push (cdr el) mlist)))
      (if mlist
          (entropy/emacs-tools-elfeed-update-proxy mlist)
        (message "None feeds need for updating with proxy."))))

  
  (defun entropy/emacs-tools-elfeed-update-proxyfeeds-regexp-match ()
    "Update feeds through proxy matched by regexp stored in
`entropy/emacs-elfeed-proxyfeeds-regexp-list'.

If hasn't setting the regexp list, prompting for input them
repeatly and stored them in `cutom-file'."
    (interactive)
    (require 'entropy-common-library)
    (let ((olist (elfeed-feed-list))
          (relist entropy/emacs-elfeed-proxyfeeds-regexp-list)
          ulist)
      (unless relist
        (setq relist (entropy/cl-repeated-read "Inputting regexp"))
        (setq entropy/emacs-elfeed-proxyfeeds-regexp-list relist)
        (customize-save-variable 'entropy/emacs-elfeed-proxyfeeds-regexp-list relist))
      (when (yes-or-no-p "Do you want to adding some matching? ")
        (setq relist (append relist (entropy/cl-repeated-read "Adding regexp")))
        (setq entropy/emacs-elfeed-proxyfeeds-regexp-list relist)
        (customize-save-variable 'entropy/emacs-elfeed-proxyfeeds-regexp-list relist))
      (dolist (el olist)
        (dolist (elm relist)
          (when (string-match-p elm el)
            (push el ulist))))
      (when ulist
        (entropy/emacs-tools-elfeed-update-proxy ulist))))

;; **** default external browser for feed viewing

  (defun elfeed-search-browse-url (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'.

Note: this function has been modified by entropy-emacs for fixing
one bug of which:

Original func coding logic leaving entry update part after opening
url, this will not redraw the \"*elfeed-search*\" successfully
because that all its behavior are not witin the
\"*elfeed-search*\" buffer as it be on w3m or eww buffer when using 
internal `browse-url-browser-function'.

The minor changing was compat for above."
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               do (elfeed-search-update-entry entry)
               when (elfeed-entry-link entry)
               do (if use-generic-p
                      (browse-url-generic it)
                    (browse-url it)))
      (unless (use-region-p) (forward-line))))

  
  (defun entropy/emacs-tools--elfeed-browse-url-around (oldfun &rest args)
    "Browse feed source by external browser with choosing."
    (let* ((choice
            (completing-read "Choose browse: " `("default"
                                                 "eww"
                                                 ,(if entropy/emacs-browse-url-function "entropy-browse-url-function" "")
                                                 ,(if (executable-find "w3m") "emacs-w3m" ""))
                             nil t))
           (browse-url-browser-function (cl-case (intern choice)
                                          ('default 'browse-url-default-browser)
                                          ('eww 'eww-browse-url)
                                          ('emacs-w3m 'entropy/emacs-tools--w3m-browse-url)
                                          ('entropy-browse-url-function entropy/emacs-browse-url-function))))
      (funcall oldfun)))
  (advice-add 'elfeed-search-browse-url :around #'entropy/emacs-tools--elfeed-browse-url-around)
  (advice-add 'elfeed-show-visit :around #'entropy/emacs-tools--elfeed-browse-url-around)

;; **** elfeed print entry function
  (defun entropy/emacs-tools--elfeed-search-print-entry--default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) "\t")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (setq elfeed-search-print-entry-function 'entropy/emacs-tools--elfeed-search-print-entry--default))

;; ** gnus
(use-package gnus
  :ensure nil
  :config
  ;; gnus home setting
  (setq gnus-home-directory (plist-get entropy/emacs-gnus-init-config :gnus-home))
  ;; gnus news dir
  (setq gnus-directory (plist-get entropy/emacs-gnus-init-config :gnus-news-dir))
  (setq gnus-kill-files-directory (plist-get entropy/emacs-gnus-init-config :gnus-news-dir))
  ;; gnus mail dir
  (setq mail-source-directory (plist-get entropy/emacs-gnus-init-config :mail-dir))
  (setq mail-default-directory (plist-get entropy/emacs-gnus-init-config :mail-temp-dir)) ;setting mail source library 'sendmail' default-directory
  (setq message-directory (plist-get entropy/emacs-gnus-init-config :mail-dir))
  (setq nnfolder-directory (plist-get entropy/emacs-gnus-init-config :mail-dir))
  ;; gnus-init config file
  (setq gnus-init-file (plist-get entropy/emacs-gnus-init-config :init-file))
  ;; newrc source file
  (setq gnus-startup-file (plist-get entropy/emacs-gnus-init-config :startup-file))
  (setq gnus-read-newsrc-file (plist-get entropy/emacs-gnus-init-config :read-newsrc))
  (setq gnus-save-newsrc-file (plist-get entropy/emacs-gnus-init-config :save-newsrc))
  ;; dribble file (The update cache)
  (setq gnus-use-dribble-file (plist-get entropy/emacs-gnus-init-config :use-dribble))
  ;; fetch updat sources type, defualt be 'some' recommended set it to 't'
  (setq gnus-read-active-file (plist-get entropy/emacs-gnus-init-config :read-active-file))


  ;; Unbind `message-kill-address' when in gnus mail `message-mode-map'.
  ;; Because of that this will cover `browse-kill-ring' default keybinding.
  (entropy/emacs-lazy-load-simple 'gnus-msg
    (define-key message-mode-map (kbd "C-c M-k") nil)))


;; ** search-web

;; Post web search queries using `browse-url'.
(use-package search-web
  :ensure nil
  :commands (search-web search-web-region)
  :bind (("C-c w" . entropy/emacs-tools-search-web-toggle)
         ("C-c W" . entropy/emacs-tools-search-web-region-toggle))
  :config

;; *** default config

  ;; redefine search-web for compat with entropy-emacs
  (defun search-web (engine word)
    "Note this function has been modified for compating with entropy-emacs.

The original one can't recovering default browser function,
fixing it as thus. "
    (interactive (list
                  (search-web-query-engine)
                  (read-string "Search Word: " nil 'search-web-word-history)))
    (destructuring-bind (engine url render)
        (assoc engine search-web-engines)
      (let* ((render
              (case render
                ((nil) search-web-default-browser)
                (In-Emacs search-web-in-emacs-browser)
                (External search-web-external-browser)
                (t render))))
        (setq browse-url-browser-function render)
        (browse-url (format url (url-hexify-string word)))
        (setq-default browse-url-browser-function search-web-default-browser))))

  
  ;; redefine search query engine for force input comprehensive data
  (defun entropy/emacs-tools--search-web-query-egine (type)
    (let* ((prompt "Search Engine: "))
      (completing-read prompt search-web-engines nil t
                       (if (string= "External" type)
                           (let ((result nil))
                             (dolist (el entropy/emacs-search-web-engines-external)
                               (if (string= "google" (car el))
                                   (setq result t)))
                             (if result
                                 "google"
                               nil))
                         nil))))

  ;; Optional choosing internal or external browser to follow the searching.
  (defun entropy/emacs-tools-search-web-toggle ()
    (interactive)
    (let ((type (completing-read "Internal or External: " '("Internal" "External") nil t)))
      (let* ((search-web-engines (cond 
                                  ((equal type "Internal") entropy/emacs-search-web-engines-internal)
                                  ((equal type "External") entropy/emacs-search-web-engines-external)))
             (engine (entropy/emacs-tools--search-web-query-egine type))
             (word (read-string "Searching for?: ")))
        (search-web engine word))))

  (defun entropy/emacs-tools-search-web-region-toggle ()
    (interactive)
    (let ((type (completing-read "Internal or External: " '("Internal" "External") nil t)))
      (let* ((search-web-engines (cond 
                                  ((equal type "Internal") entropy/emacs-search-web-engines-internal)
                                  ((equal type "External") entropy/emacs-search-web-engines-external)))
             (engine (entropy/emacs-tools--search-web-query-egine type)))
        (search-web-region engine))))
  
  (setq search-web-in-emacs-browser 'eww-browse-url)

  (defun entropy/emacs-tools--search-web--around (oldfun &rest arg-rest)
    "Partially cancel `entropy/emacs-web-development-environment' if
    it's actived."
    (let* ((entropy/emacs-web-development-environment nil))
      (funcall oldfun)))

  (advice-add 'entropy/emacs-tools-search-web-toggle :around #'entropy/emacs-tools--search-web--around)
  (advice-add 'entropy/emacs-tools-search-web-region-toggle :around #'entropy/emacs-tools--search-web--around))

;; ** emacs-w3m interface
(when (executable-find "w3m")
  (use-package w3m
    :commands
    (w3m
     w3m-search
     w3m-goto-url
     entropy/emacs-tools--w3m-set-proxy
     entropy/emacs-tools--w3m-browse-url)
    :bind
    (:map w3m-mode-map
          ("<down>" . next-line)
          ("<up>" . previous-line)
          ("<left>" . left-char)
          ("<right>" . right-char)
          ("p" . entropy/emacs-tools-w3m-toggle-proxy)
          ("b" . w3m-view-url-with-browse-url)
          ("l" . w3m-view-previous-page)
          ("q" . bury-buffer))
    :config
    (entropy/emacs-lazy-load-simple 'w3m-search
      (add-to-list 'w3m-search-engine-alist
                   '("bing" "https://www.bing.com/search?q=%s" utf-8))
      (setq w3m-search-default-engine "bing"))
    (setq w3m-confirm-leaving-secure-page nil)
    (setq w3m-image-no-idle-timer t)
    (setq w3m-image-animate-seconds nil)
    (setq w3m-show-graphic-icons-in-header-line nil)
    (setq w3m-use-favicon nil)
    (setq w3m-use-refresh nil)
    (setq w3m-use-tab nil)
    (setq w3m-use-tab-menubar nil)
    (setq w3m-process-timeout 5)
    (setq w3m-pop-up-windows nil)
    (when (and (eq system-type 'windows-nt)
               (file-exists-p (concat invocation-directory "convert.exe")))
      (setq w3m-imagick-convert-program (concat invocation-directory "convert.exe")))

    ;; session configuration
    (setq w3m-session-autosave nil)
    (setq w3m-session-deleted-save nil)
    (setq w3m-session-crash-recovery nil)

;; *** recorde current retrieve url
    (defvar entropy/emacs-tools--w3m-retrieve-url nil)
    (defun entropy/emacs-tools--w3m-recorde-retrieve-url (url &rest args)
      (setq entropy/emacs-tools--w3m-retrieve-url url))
    (advice-add 'w3m-retrieve :before #'entropy/emacs-tools--w3m-recorde-retrieve-url)

;; *** w3m proxy toggle
    (defun entropy/emacs-tools-w3m-toggle-proxy ()
      "Toggle proxy using `entropy-proxy-url' and refresh current
web page buffer. It's typically using with the statement that you
can't visit one page suddenly."
      (interactive)
      (require 'entropy-proxy-url)
      (let ((url entropy/emacs-tools--w3m-retrieve-url))
        (entropy/proxy-url-switch-for-w3m)
        (call-interactively 'w3m-process-stop)
        (w3m-goto-url url)))
    
;; *** w3m external browser setting
    (defun entropy/emacs-tools--w3m-external-advice (oldfunc &rest args)
      (let ((browse-url-browser-function
             (if entropy/emacs-browse-url-function
                 entropy/emacs-browse-url-function
               'browse-url-default-browser)))
        (call-interactively oldfunc)))
    (advice-add 'w3m-view-url-with-browse-url :around #'entropy/emacs-tools--w3m-external-advice))
    
    
;; *** w3m personal browse url function
  (defun entropy/emacs-tools--w3m-browse-url (url &rest args)
    (w3m-goto-url url)))


;; ** toggle the default browse in emacs
(defun entropy/emacs-tools--setting-default-browser (browser)
  "Setting the default browser for `search-web' and all the
url-open about function."

  (setq-default search-web-default-browser browser)
  (setq-default search-web-external-browser browser)
  (setq-default search-web-in-emacs-browser 'eww-browse-url)
  (setq-default browse-url-browser-function browser)
  (setq shr-external-browser browser))

(defun entropy/emacs-tools-toggle-default-browser ()
  "Toggle browse-url defualt browse function for all url-open
about function.

Default option were: 
- eww: `eww-browse-url'
- default: `browse-url-default-browser'

If `entropy/emacs-enable-personal-browse-url-function' was 't' and `entropy/emacs-browse-url-function' was
effective then adding option of personal browse url function that be in ordered by
`entropy/emacs-browse-url-function'
"
  (interactive)
  (let* ((list-of-choice (cond ((and entropy/emacs-enable-personal-browse-url-function
                                     entropy/emacs-browse-url-function
                                     (not (executable-find "w3m")))
                                '("personal"
                                  "eww"
                                  "default"))
                               ((and entropy/emacs-enable-personal-browse-url-function
                                     entropy/emacs-browse-url-function
                                     (executable-find "w3m"))
                                '("personal"
                                  "w3m"
                                  "eww"
                                  "default"))
                               ((executable-find "w3m")
                                '("w3m"
                                  "eww"
                                  "default"))
                               (t '("eww" "default"))))
         (choice
          (ivy-read "Choose the function you want: " list-of-choice)))
        (cond
         ((string= choice "eww")
          (entropy/emacs-tools--setting-default-browser 'eww-browse-url))
         ((string= choice "default")
          (entropy/emacs-tools--setting-default-browser 'browse-url-default-browser))
         ((string= choice "personal")
          (entropy/emacs-tools--setting-default-browser entropy/emacs-browse-url-function))
         ((string= choice "w3m")
          (entropy/emacs-tools--setting-default-browser 'entropy/emacs-tools--w3m-browse-url))
         (t
          (error "Please choose the correct choice!")))))


;; init setting
(when (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
  (if (not (executable-find "w3m"))
      (entropy/emacs-tools--setting-default-browser entropy/emacs-browse-url-function)
    (if (display-graphic-p)
        (entropy/emacs-tools--setting-default-browser entropy/emacs-browse-url-function)
      (entropy/emacs-tools--setting-default-browser 'entropy/emacs-tools--w3m-browse-url))))

;; ** Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; ** Self functions
;; *** split window horizontally for comfortable width setting
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

;; *** Configure network proxy
;; *** entropy-emacs version show
(defun entropy/emacs-tools-entropy-emacs-version ()
  "Show entropy-emacs version."
  (interactive)
  (message entropy/emacs-ecv))

;; *** show time
(defun entropy/emacs-tools-time-show ()
  "Show current time with date information also."
  (interactive)
  (let ((time (format-time-string "%Y-%m-%d %a %H:%M:%S")))
    (message "Now is %s" time)))
(global-set-key (kbd "<f12>") 'entropy/emacs-tools-time-show)

;; ** encoding and end-of-line conversation
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


;; ** Foreign language realtime translation
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

(entropy/emacs-tools-toggle-dict "sdcv")

;; *** yoaudao-dictionary
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

;; *** google-translate
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


;; *** bing-dict
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
;; *** sdcv
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

;; ** chinese dict
(use-package entropy-cn-dict
  :ensure nil
  :commands entropy/cndt-query
  :bind (("C-x y" . entropy/cndt-query)))

;; ** Log keyboard commands to buffer
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

;; ** pomidor A simple and cool pomodoro timer
(use-package pomidor
  :commands (pomidor)
  :bind (("C-c <f12>" . pomidor)))

;; ** maple preview
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

;; ** display world clock
(use-package counsel-world-clock
  :commands  (counsel-world-clock)
  :bind ("C-x <f12>" . counsel-world-clock))

;; ** entropy-emacs self packages
;; *** entropy-proxy-mode
(use-package entropy-proxy-mode
  :ensure nil
  :commands entropy/proxy-mode
  :config
  (setq url-gateway-local-host-regexp
        (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'")))

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
             entropy/cs-delete)
  :config
  (defun entropy/emacs-tools--open-with-port-stuffs-around (oldfunc &rest arg-rest)
    "when in `entropy/emacs-web-development-environment' advice
`entropy/open-with-port' for prevent open url with specific
development web-browser."
    (let ((entropy/emacs-web-development-environment nil))
      (apply oldfunc arg-rest)))
  (entropy/emacs-lazy-load-simple 'entropy-open-with
    (advice-add 'entropy/open-with-port :around #'entropy/emacs-tools--open-with-port-stuffs-around)))

;; *** entropy-portableapps
(use-package entropy-portableapps
  :if sys/win32p
  :ensure nil
  :commands (entropy/poapps-query-open)
  :bind (("C-M-<f11>" . entropy/poapps-query-open)))


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
   ((or sys/linuxp sys/linux-x-p sys/macp sys/mac-x-p)
    (dired "~/"))))

(defalias 'ehome 'entropy/emacs-tools-goto-sys-home "Alias for entropy/emacs-tools-goto-sys-home.")

;; *** firefox bookmarks and history query and open
(use-package counsel-ffdata
  :commands (counsel-ffdata-firefox-bookmarks
             counsel-ffdata-firefox-history)
  :init
  (setq counsel-ffdata-database-path
        (cl-case system-type
          ((gnu gnu/linux gnu/kfreebsd)
           (expand-file-name
            (car (file-expand-wildcards
                  "~/.mozilla/firefox/*.default-release/places.sqlite"))))
          (windows-nt
           (car (file-expand-wildcards
                 (expand-file-name "Mozilla/Firefox/Profiles/*/places.sqlite"
                                   (getenv "APPDATA"))))))))

;; *** neotree
(use-package neotree
  :commands (neotree-toggle
             neotree-mode
             entropy/emacs-tools-neotree--close
             entropy/emacs-tools-neotree-refresh-for-current
             entropy/emacs-tools-neo-open-with)
  :bind (("<f8>" . entropy/emacs-tools-neotree-refresh-for-current)
         ("C-<f8>" . entropy/emacs-tools-neotree--close))

  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-autorefresh t
        neo-hidden-regexp-list nil)

  :config

  ;; Make node item execution for neotree with `entropy-open-with'
  (define-key neotree-mode-map (kbd "<C-M-return>")
    (neotree-make-executor
     :file-fn 'entropy/emacs-tools-neo-open-with
     :dir-fn  'entropy/emacs-tools-neo-open-with))

  (defun entropy/emacs-tools-neo-open-with (full-path &rest _)
    "Open neotree node item in external apps powered by
`entropy-open-with'."
    (interactive)
    (require 'entropy-open-with)
    (entropy/open-with-match-open (list full-path)))

  (defun entropy/emacs-tools--neo-pos-hl-and-indent (&rest _)
    "Highlight current neotree buffer line and goto the first
word of current-line for preventing the long line truncate view."
    (hl-line-mode 1)
    (forward-line 0)
    (re-search-forward "\\w" nil t))

  (advice-add 'neo-buffer--goto-cursor-pos
              :after
              #'entropy/emacs-tools--neo-pos-hl-and-indent)

  (defun neo-global--attach ()
    "Attach the global neotree buffer

Note: this function has been modified by entropy-emacs for reason
of forcely repeating the global-refresh behaviour."
    (when neo-global--autorefresh-timer
      (cancel-timer neo-global--autorefresh-timer))
    (when neo-autorefresh
      (setq neo-global--autorefresh-timer
            (run-with-idle-timer 1.2 t 'neo-global--do-autorefresh)))
    (setq neo-global--buffer (get-buffer neo-buffer-name))
    (setq neo-global--window (get-buffer-window
                              neo-global--buffer))
    (neo-global--with-buffer
      (neo-buffer--lock-width))
    (run-hook-with-args 'neo-after-create-hook '(window)))

  (defun entropy/emacs-tools-neotree--close ()
    "Globally close the neotree buffer and window."
    (interactive)
    (when neo-global--buffer (kill-buffer neo-global--buffer))
    (mapcar (lambda (x)
              (when (equal (buffer-name (window-buffer x)) neo-buffer-name)
                (delete-window-internal x)))
            (window-list))
    (setf neo-global--buffer nil
          neo-global--window nil))

  (defun entropy/emacs-tools-neotree-refresh-for-current ()
    "Open neotree with current working directory.

Globally close neotree buffer while selected window was
`neo-global--window'."
    (interactive)
    (let ((buffer_ (current-buffer))
          (bfn (ignore-errors
                 (file-name-nondirectory
                  (buffer-file-name (current-buffer)))))
          (marker (point)))
      (cond
       ((equal buffer_ neo-global--buffer)
        (entropy/emacs-tools-neotree--close))
       (t
        (unless (neo-global--window-exists-p)
          (save-window-excursion
            (neotree-show)))
        (neo-buffer--refresh t t)
        (when (ignore-errors (stringp bfn))
          (goto-char (point-min))
          (re-search-forward bfn nil t)
          (entropy/emacs-tools--neo-pos-hl-and-indent))
        (save-excursion
          (switch-to-buffer buffer_)
          (goto-char marker))
        (neo-global--select-window)))))

  (add-hook 'eyebrowse-post-window-switch-hook  #'neo-global--attach))

;; * provide
(provide 'entropy-emacs-tools)
