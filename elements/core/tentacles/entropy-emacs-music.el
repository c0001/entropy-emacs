;;; entropy-emacs-music --- Music client for emacs
;;
;; * Copyright (C) 20200316  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Version:       file-version
;; Created:       2019-03-16 18:09:51
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs 24;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; entropy emacs music client config
;;
;; * Configuration:
;;
;; eemac specification, no warrantry for others.
;;
;; * code

;; ** require
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defface)
(require 'entropy-emacs-utils)
(require 'entropy-emacs-hydra-hollow)


;; ** prepare

(setenv "MPD_HOST" entropy/emacs-mpd-host-url)
(setenv "MPD_PORT" entropy/emacs-mpd-host-port)

;; ** mpc

(use-package mpc
  :ensure nil
  :eemacs-functions mpc-songs-buf
  :commands
  (mpc
   mpc-seek-current)
;; *** preface
  :preface

  (defvar entropy/emacs-music-mpc--orig-window-configuration nil)

  (defun entropy/emacs-music-mpc--patch-popuped-window-balance
      (&rest _)
    "Patch the origin mpc initialized windows layout for eemacs
specification."
    (let* ((buff (mpc-songs-buf))
           (buff-win (ignore-errors (get-buffer-window buff))))
      (when (and (buffer-live-p buff)
                 buff-win)
        (message "Balance mpc layout ... ")
        (with-selected-window buff-win
          (enlarge-window 10)
          (goto-char (point-min)))
        (select-window buff-win)
        (message ""))))

  (defun entropy/emacs-music-mpc--patch-popuped-window-around-advice
      (orig-func &rest orig-args)
    (let* ((wfg-orig (current-window-configuration)))
      (setq entropy/emacs-music-mpc--orig-window-configuration
            wfg-orig)
      (delete-other-windows-internal)
      (apply orig-func orig-args)
      (entropy/emacs-music-mpc--patch-popuped-window-balance)))

  (defun entropy/emacs-music-mpc--patch-quit-around-advice (orig-func &rest orig-args)
    (let* ()
      (apply orig-func orig-args)
      (set-window-configuration
       entropy/emacs-music-mpc--orig-window-configuration)))

  (defun entropy/emacs-music-mpc-auto-add-and-play ()
    "Play current music in `mpc-songs-mode'.

Add current music to queue when its not in thus."
    (interactive)
    (condition-case nil
        (call-interactively #'mpc-songs-jump-to)
      (error
       (when (null mpc-select)
         (call-interactively #'mpc-select))
       (mpc-playlist-add)
       (call-interactively #'mpc-songs-jump-to)
       (call-interactively #'mpc-select-toggle))))

  (defun entropy/emacs-music-mpc-increae-volume ()
    (interactive)
    (let* ((curvol (string-to-number (cdr (assq 'volume mpc-status))))
           (newvol (+ curvol 5))
           (newvol-str (number-to-string newvol)))
      (if (< newvol 100)
          (mpc-proc-cmd (list "setvol" newvol-str)
                        'mpc-status-refresh)
        (message "Warn: mpc vol was loudest!"))))

  (defun entropy/emacs-music-mpc-decrease-volume ()
    (interactive)
    (let* ((curvol (string-to-number (cdr (assq 'volume mpc-status))))
           (newvol (- curvol 5))
           (newvol-str (number-to-string (- curvol 5))))
      (if (> newvol 0)
          (mpc-proc-cmd (list "setvol" newvol-str)
                        'mpc-status-refresh)
        (message "Warn: mpc vol was mute!"))))

;; *** eemacs mmphc
  :eemacs-mmphc
  ((((:enable t)
     (mpc-songs-mode (mpc mpc-songs-mode-map) t (3 2 2)))
    ((:enable t)
     (mpc-tagbrowser-mode (mpc mpc-tagbrowser-mode-map) t (3 2 2)))
    ((:enable t)
     (mpc-status-mode (mpc mpc-status-mode-map) t (3 2 2))))
   ("Common"
    (("P" mpc-pause "Pause playing" :enable t :exit t :map-inject t)
     ("n" mpc-next "next song" :enable t :exit t :map-inject t)
     ("p" (mpc-proc-cmd "previous") "previous song"
      :enable t :exit t :map-inject t)
     ("t t" mpc-toggle-play "Toggle between play and pause"
      :enable t :exit t :map-inject nil)
     ("t r" mpc-toggle-repeat "Toggle repeat play"
      :enable t :exit nil :map-inject nil
      :toggle (if (string= "0" (cdr (assq 'repeat (mpc-cmd-status)))) nil t))
     ("t a" mpc-toggle-single "Toggle single play for repeat mode"
      :enable t :exit nil :map-inject nil
      :toggle (if (string= "0" (cdr (assq 'single (mpc-cmd-status)))) nil t))
     ("t s" mpc-toggle-shuffle "Toggle shuffle play"
      :enable t :exit nil :map-inject nil
      :toggle (if (string= "0" (cdr (assq 'random (mpc-cmd-status)))) nil t)))

    "Seek&volume"
    ((">" (mpc-seek-current "+10") "Seek forward 10s"
      :enable t :map-inject t)
     ("<" (mpc-seek-current "-10") "Seek backward 10s"
      :enable t :map-inject t)
     ("+" entropy/emacs-music-mpc-increae-volume "increase volume"
      :enable t :map-inject t)
     ("-" entropy/emacs-music-mpc-decrease-volume "decrease volume"
      :enable t :map-inject t)
     ("m m"
      (mpc-proc-cmd
       (list "setvol" "100")
       'mpc-status-refresh)
      "Maximize volume to 100"
      :enable t :exit t)
     ("m 0"
      (mpc-proc-cmd
       (list "setvol" "0")
       'mpc-status-refresh)
      "Mute volume to 0"
      :enable t :exit t))

    "Search"
    (("s" mpc-songs-search
      "Filter songs to those who include STRING in their metadata"
      :enable t :exit t :map-inject t)
     ("S" mpc-songs-kill-search
      "Turn off the current search restriction"
      :enable t :exit t :map-inject t))

    "Playlist"
    (("g" mpc-playlist "Show the current playlist"
      :enable t :exit t :map-inject t)
     ("a" mpc-playlist-add "Add the selection to the playlist"
      :enable t :exit t)
     ("c" mpc-playlist-create "Save current playlist under name NAME"
      :enable t :exit t)
     ("r" mpc-playlist-rename "Rename playlist OLDNAME to NEWNAME"
      :enable t :exit t)
     ("d" mpc-playlist-delete "Remove the selected songs from the playlist"
      :enable t :exit t)
     ("D" mpc-playlist-destroy "Delete playlist named NAME"
      :enable t :exit t))))

;; *** eemacs mmphca
  :eemacs-mmphca
  ((((:enable t)
     (mpc-tagbrowser-mode (mpc mpc-tagbrowser-mode-map)))
    ("Common"
     (("RET" mpc-select "Select the tag value at point"
       :enable t :map-inject t :exit t))))
   (((:enable t)
     (mpc-songs-mode (mpc mpc-songs-mode-map)))
    ("Common"
     (("RET" entropy/emacs-music-mpc-auto-add-and-play
       "Play current music."
       :enable t :exit t :map-inject t)))))

;; *** hook
  :hook
  ((mpc-songs-mode      . hl-line-mode)
   (mpc-status-mode     . hl-line-mode)
   (mpc-tagbrowser-mode . hl-line-mode))

;; *** init
  :init
  (setq
   mpc-host
   (format "%s:%s"
           entropy/emacs-mpd-host-url
           entropy/emacs-mpd-host-port)
   mpc-songs-format
   "%-5{Time} %25{Title} %20{Album} %20{Artist} %5{Date}"
   mpc-browser-tags
   '(Genre
     Artist|Composer|Performer
     Album|Playlist)
   mpc-status-buffer-format
   '("%-5{Time} / %{Duration} %2{Disc--}%4{Track}"
     "Title:  %{Title}"
     "Album:  %{Album}"
     "Artist: %{Artist}"
     "%128{Cover}"))

;; *** config
  :config
  ;; RET in mpc-status-mode is meaningless and will messy the visual
  ;; experience.
  (define-key mpc-status-mode-map
    (kbd "RET") nil)

  (advice-add 'mpc
              :around
              #'entropy/emacs-music-mpc--patch-popuped-window-around-advice)
  (advice-add 'mpc-quit
              :around
              #'entropy/emacs-music-mpc--patch-quit-around-advice))


;; ** bongo
(use-package bongo
;; *** defines
  :commands
  (bongo-switch-to-buffer
   bongo-switch-buffers)
  :eemacs-functions
  (bongo-buffer
   bongo-library-buffer
   bongo-playlist-buffer)
  :eemacs-macros
  (with-bongo-library-buffer)

;; *** init
  :init

  (entropy/emacs-lazy-load-simple dired
    (with-no-warnings
      (defun entropy/emacs-music-bongo-add-dired-files ()
        "Add marked files to the Bongo library and then popup the
`bongo-library-buffer' which the buffer point position has been
jumped to the main context."
        (interactive)
        (let ((buffer (bongo-library-buffer)))
          (let (file (files nil))
            (dired-map-over-marks
             (setq file (dired-get-filename)
                   files (append files (list file)))
             nil t)
            (with-bongo-library-buffer
              (mapc 'bongo-insert-file files)
              (goto-char (point-min))
              ;; go to the head of the library content which will skip
              ;; the bongo library header
              (re-search-forward
               (regexp-quote "  Report bugs to <bongo-devel@nongnu.org>."))
              (next-line 2)))
          (display-buffer buffer)))

      (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
       'dired-mode '(dired dired-mode-map)
       '("Misc."
         (("m b" entropy/emacs-music-bongo-add-dired-files
           "Add marked files to the Bongo library."
           :enable t :exit t))))))

;; *** config
  :config

  ;; focely disable bongo mode line indictor at startup time, because
  ;; it may cause modeline format pollution and be with unstable xpm
  ;; indictor image render function.
  ;;
  ;; We must set it after the `bongo.el' loaded to override what it
  ;; will enable at the load time.
  (setq bongo-mode-line-indicator-mode nil)

  )

;; * provide
(provide 'entropy-emacs-music)
