;;; entropy-emacs-music --- Music client for emacs  -*- lexical-binding: t; -*-
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

;; ** prepare

(entropy/emacs--inner-setenv "MPD_HOST" entropy/emacs-mpd-host-url)
(entropy/emacs--inner-setenv "MPD_PORT" entropy/emacs-mpd-host-port)

;; ** mpc

(use-package mpc
  :ensure nil
  :eemacs-functions mpc-songs-buf
  :commands
  (mpc entropy/emacs-music-mpc-mini
   mpc-seek-current)
;; *** eemacs mmphc
  :eemacs-mmphc
  ((((:enable t :defer t)
     (mpc-songs-mode (mpc mpc-songs-mode-map) t (2 2 2)))
    ((:enable t :defer t)
     (mpc-tagbrowser-mode (mpc mpc-tagbrowser-mode-map) t (2 2 2)))
    ((:enable t :defer t)
     (mpc-status-mode (mpc mpc-status-mode-map) t (2 2 2))))
   ("Common"
    (("P" mpc-toggle-play "Toggle between play and pause"
      :enable t :exit t :map-inject t)
     ("n" mpc-next "next song" :enable t :exit t :map-inject t)
     ("p" (mpc-proc-cmd "previous") "previous song"
      :enable t :exit t :map-inject t)
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
     ("f" (mpc-seek-current "+10") "Seek forward 10s"
      :enable t :map-inject t)
     ("<" (mpc-seek-current "-10") "Seek backward 10s"
      :enable t :map-inject t)
     ("b" (mpc-seek-current "-10") "Seek backward 10s"
      :enable t :map-inject t)
     ("+" entropy/emacs-music-mpc-increae-volume "increase volume"
      :enable t :map-inject t)
     ("-" entropy/emacs-music-mpc-decrease-volume "decrease volume"
      :enable t :map-inject t)
     ("v m"
      (mpc-proc-cmd
       (list "setvol" "100")
       'mpc-status-refresh)
      "Maximize volume to 100"
      :enable t :exit t)
     ("v 0"
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
      :enable t :exit t :map-inject t)
     ("g"
      entropy/emacs-music-mpc-songs-buffer-refresh
      "Show the current played Song with refresh playlist buffer"
      :enable t :exit t :map-inject t))

    "Playlist"
    (("p" entropy/emacs-music-mpc-jump-to-playlist
      "Jump to specified playlist"
      :enable t :exit t :map-inject t)
     ("c" mpc-playlist-create "Create new playlist under name NAME"
      :enable t :exit t :map-inject t)
     ("r" mpc-playlist-rename "Rename playlist OLDNAME to NEWNAME"
      :enable t :exit t)
     ("D" mpc-playlist-destroy "Delete playlist named NAME"
      :enable t :exit t))))

;; *** eemacs mmphca
  :eemacs-mmphca
  ((((:enable t :defer t)
     (mpc-tagbrowser-mode (mpc mpc-tagbrowser-mode-map)))
    ("Common"
     (("RET" mpc-select "Select the tag value at point"
       :enable t :map-inject t :exit t))))
   (((:enable t :defer t)
     (mpc-songs-mode (mpc mpc-songs-mode-map)))
    ("Common"
     (("RET" entropy/emacs-music-mpc-auto-add-and-play
       "Play current music."
       :enable t :exit t :map-inject t))

     "Select"
    (("m" entropy/emacs-music-mpc-select-single "Signle select"
      :enable t :exit t :map-inject t)
     ("M" entropy/emacs-music-mpc-select-region "Region select"
      :enable t :exit t :map-inject t)
     ("u" entropy/emacs-music-mpc-unselect-single "Signle unselect"
      :enable t :exit t :map-inject t)
     ("U" entropy/emacs-music-mpc-unselect-region "Region unselect"
      :enable t :exit t :map-inject t)
     ("R" entropy/emacs-music-mpc-unselect-all "Unselect all"
      :enable t :exit t :map-inject t))

     "Playlist"
     (("a" entropy/emacs-music-mpc-add-to-playlist
      "Add the selection to the playlist"
      :enable t :exit t)
      ("d" entropy/emacs-music-mpc-remove-songs-from-playlist
       "Delete selections from current playlist"
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
   "%4{Disc} %25{Title} %20{Album} %20{Artist} %5{Date} %-5{Time}"
   mpc-browser-tags
   '(Artist
     Album
     Playlist)
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

;; **** advices
  (advice-add 'mpc
              :around
              #'entropy/emacs-music-mpc--initialize-patch)
  (advice-add 'mpc
              :around
              #'__mpc/disable-window-configuration-restore)

  ;; patched to not kill `entropy/emacs-main-frame'
  (advice-patch
   'mpc-quit
   '(unless (eq frame entropy/emacs-main-frame) (ignore-errors (delete-frame frame)))
   '(ignore-errors (delete-frame frame)))
  (advice-add 'mpc-quit
              :around
              #'entropy/emacs-music-mpc--patch-quit-around-advice)

  (defun entropy/emacs-music--mpc-around-advice-for-mpc-volum-refresh
      (orig-func &rest orig-args)
    (condition-case nil
        (apply orig-func orig-args)
      (error
       (message "Warn: mpc callback with some fatal for `mpc-volumn-refresh'"))))

  (advice-add 'mpc-volume-refresh
              :around
              #'entropy/emacs-music--mpc-around-advice-for-mpc-volum-refresh)

  (defvar entropy/emacs-music--mpc-auto-add-and-play-sinal nil)
  (defun entropy/emacs-music--mpc-around-advice-for-mpc--status-callback
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (prog1
          rtn
        (when entropy/emacs-music--mpc-auto-add-and-play-sinal
          (entropy/emacs-music--mpc-goto-current-pos)
          (setq entropy/emacs-music--mpc-auto-add-and-play-sinal nil)))))

  (advice-add 'mpc--status-callback
              :around
              #'entropy/emacs-music--mpc-around-advice-for-mpc--status-callback)

;; **** redefine
  ;; EEMACS_MAINTENANCE: For prevent from multi-same items pos jump,
  ;; we disable the 'other' handle but there's may have a more
  ;; excellent way?
  (defun mpc-songpointer-refresh ()
    (let ((buf (mpc-proc-buffer (mpc-proc) 'songs)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((pos (text-property-any
                       (point-min) (point-max)
                       'mpc-file (mpc-songs-hashcons
                                  (cdr (assq 'file mpc-status)))))
                 ;; (other (when pos
                 ;;          (save-excursion
                 ;;            (goto-char pos)
                 ;;            (text-property-any
                 ;;             (line-beginning-position 2) (point-max)
                 ;;             'mpc-file (mpc-songs-hashcons
                 ;;                        (cdr (assq 'file mpc-status)))))))
                 )
            ;; (if other
            ;;     ;; The song appears multiple times in the buffer.
            ;;     ;; We need to be careful to choose the right occurrence.
            ;;     (mpc-proc-cmd "playlist" 'mpc-songpointer-refresh-hairy)
            ;;   (mpc-songpointer-set pos))
            (mpc-songpointer-set pos))))))

  (defun mpc--status-timer-run ()
    "Refresh mpc status.

NOTE: this function has been modified to continue
`mpc--status-idle-timer' when the the status buffer or window has
been killed."
    (with-demoted-errors "MPC: %S"
      (when (process-get (mpc-proc) 'ready)
        (let* ((buf (mpc-proc-buffer (mpc-proc) 'status))
               (win (get-buffer-window buf t))
               (win-songs (get-buffer-window
                           (mpc-proc-buffer (mpc-proc) 'songs))))
          (if (and (not win)
                   (not win-songs))
              (mpc--status-timer-stop)
            (with-local-quit (mpc-status-refresh)))))))

;; **** enhancement

;; ***** initial UI
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

  (defun entropy/emacs-music-mpc--exchage-window-buffers-init ()
    (let* ((playlist-buf (mpc-proc-buffer (mpc-proc) 'Playlist))
           (playlist-win (get-buffer-window playlist-buf))
           (status-buf (mpc-proc-buffer (mpc-proc) 'status))
           (status-win (get-buffer-window status-buf)))
      (set-window-buffer status-win playlist-buf)
      (set-window-buffer playlist-win status-buf)))

  (defun entropy/emacs-music-mpc--status-buffer-create ()
    "Create mpc status buffer with connection did on"
    (let* ((proc (mpc-proc))
           (buf (mpc-proc-buffer proc 'status)))
      (unless (buffer-live-p buf)
        (setq buf (get-buffer-create "*MPC-Status*"))
        (with-current-buffer buf
          (mpc-status-mode))
        (mpc-proc-buffer proc 'status buf))
      buf))

  (defvar entropy/emacs-music--mpc-mini-mode nil)
  (defun entropy/emacs-music-mpc--initialize-patch
      (orig-func &rest orig-args)
    (let* ((wfg-orig (current-window-configuration)))
      (cond ((null entropy/emacs-music--mpc-mini-mode)
             (setq entropy/emacs-music-mpc--orig-window-configuration
                   wfg-orig)
             (delete-other-windows-internal)
             (apply orig-func orig-args)
             (entropy/emacs-music-mpc--patch-popuped-window-balance)
             (entropy/emacs-music-mpc--exchage-window-buffers-init))
            (entropy/emacs-music--mpc-mini-mode
             (entropy/emacs-delete-side-windows '(left))
             ;; connect mpc daemon and create core buffers so that we
             ;; do not need to use mpc internal window creation
             ;; procedure to both reduce init time and prevent display
             ;; extra non-need buffers
             (progn
               (entropy/emacs-music-mpc--status-buffer-create)
               (mpc-status-refresh)
               (mpc-songs-buf))
             (let* ((status-buf (mpc-proc-buffer (mpc-proc) 'status))
                    (songs-buf (mpc-proc-buffer (mpc-proc) 'songs))
                    (bottom-enlarge
                     (let ((height-top (frame-height))
                           rtn)
                       (setq rtn
                             (- (* 0.7 height-top)
                                (/ height-top 2)))
                       (floor rtn)))
                    win-above win-below)
               (unless (and (buffer-live-p status-buf)
                            (buffer-live-p songs-buf))
                 (user-error "Mpc daemon is not running!"))
               (setq win-above
                     (display-buffer-in-side-window
                      status-buf
                      `((slot . 0)
                        (side . left)
                        (window-width
                         .
                         ,(ceiling (* (frame-width) 0.15))))))
               (setq win-below
                     (display-buffer-in-side-window
                      songs-buf
                      `((slot . 1)
                        (side . left))))
               (window-resize win-below bottom-enlarge)
               (dolist (win (list win-above win-below))
                 (set-window-parameter win 'no-delete-other-windows t)
                 ;; inidcate those window is used for mpc-mini-mode
                 ;; specially on.
                 (set-window-parameter win 'mpc-mini-mode t))
               ;; dedicated buffer with its window so that any buffer
               ;; display can not reuse that window which is
               ;; necessarily needed in this case
               (dolist (win `(,win-above ,win-below))
                 (set-window-dedicated-p win t))
               (with-current-buffer songs-buf
                 (entropy/emacs-music-mpc-songs-buffer-refresh)))))

      ;; patch `mode-line-format' for performance consideration
      (let ((song-buf (mpc-proc-buffer (mpc-proc) 'songs)))
        (when (buffer-live-p song-buf)
          (with-current-buffer song-buf
            (setq-local
             mode-line-format
             (--map-when
              (eq it 'mode-line-position)
              '(:eval (if (entropy/emacs-current-session-is-idle 0.8)
                          mode-line-position
                        " <x> "))
              mode-line-format)))))))

  (defun entropy/emacs-music-mpc-mini ()
    "The `mpc' mini tpype which just display side window group
for the songs list and status callback."
    (declare (interactive-only t))
    (interactive)
    (let ((entropy/emacs-music--mpc-mini-mode t))
      (mpc)))

  (defun __mpc/disable-window-configuration-restore (&optional orig-func &rest orig-args)
    (let (rtn)
      (when (functionp orig-func)
        (setq rtn (apply orig-func orig-args)))
      ;; remove all mpc internal window configuration memory in which
      ;; case we use
      ;; `entropy/emacs-music-mpc--orig-window-configuration' mechanism instead.
      (mapc (lambda (buff)
              (with-current-buffer buff
                (when (buffer-local-value 'mpc-previous-window-config buff)
                  (kill-local-variable 'mpc-previous-window-config))))
            (buffer-list))
      rtn))

  (defun entropy/emacs-music-mpc--patch-quit-around-advice (orig-func &rest orig-args)
    (let* ((select-window-mpc-mini-mode-p
            (window-parameter (selected-window) 'mpc-mini-mode)))
      (__mpc/disable-window-configuration-restore)
      (apply orig-func orig-args)
      (when (and (window-configuration-p entropy/emacs-music-mpc--orig-window-configuration)
                 ;; we do not recover origin window configuration when
                 ;; current mpc in mini mode.
                 (not select-window-mpc-mini-mode-p))
        (set-window-configuration
         entropy/emacs-music-mpc--orig-window-configuration)
        (setq entropy/emacs-music-mpc--orig-window-configuration nil))))

  (defun entropy/emacs-music-mpc--window-configuration-reset-hook
      (&rest _)
    "Reset `entropy/emacs-music-mpc--orig-window-configuration'
as hook for commonly situation."
    (setq
     entropy/emacs-music-mpc--orig-window-configuration nil))

  ;; the window configurtion can not be used cross workspace
  (with-eval-after-load 'eyebrowse
    (add-hook 'eyebrowse-pre-window-switch-hook
              #'entropy/emacs-music-mpc--window-configuration-reset-hook))

;; ***** Utils
;; ****** mpc select
  (defun entropy/emacs-music-mpc-unselect-all (&optional event)
    "Unselect the tag value at point."
    (interactive (list last-nonmenu-event) mpc-songs-mode)
    (mpc-event-set-point event)
    (if (and (bolp) (eobp)) (forward-line -1))
    (mapc 'delete-overlay mpc-select)
    (setq mpc-select nil))

  (defun entropy/emacs-music-mpc-unselect-single (&optional event)
    (interactive (list last-nonmenu-event) mpc-songs-mode)
    (mpc-event-set-point event)
    (save-excursion
      (when (get-char-property (point) 'mpc-select)
        (let ((ols nil))
          (dolist (ol mpc-select)
            (if (and (<= (overlay-start ol) (point))
                     (> (overlay-end ol) (point)))
                (delete-overlay ol)
              (push ol ols)))
          (cl-assert (= (1+ (length ols)) (length mpc-select)))
          (setq mpc-select ols))))
    (forward-line 1))

  (defun entropy/emacs-music-mpc-unselect-region ()
    (interactive nil mpc-songs-mode)
    (let ((this-start (line-number-at-pos (region-beginning)))
          (this-end (line-number-at-pos (region-end))))
      (entropy/emacs-buffer-goto-line this-start)
      (while (and (<= (line-number-at-pos (point))
                      this-end)
                  (not (eobp)))
        (call-interactively
         #'entropy/emacs-music-mpc-unselect-single))
      (let (select-active-regions)
        (deactivate-mark))))

  (defun entropy/emacs-music-mpc-select-single (&optional event)
    (interactive (list last-nonmenu-event) mpc-songs-mode)
    (mpc-event-set-point event)
    (if (and (bolp) (eobp)) (forward-line -1))
    (if (mpc-tagbrowser-all-p)
        nil
      (mpc-select-make-overlay))
    (when mpc-tag
      (mpc-tagbrowser-all-select)
      (mpc-selection-refresh))
    (forward-line 1))

  (defun entropy/emacs-music-mpc-select-region ()
    (interactive nil mpc-songs-mode)
    (let ((this-start (region-beginning))
          (this-end (region-end)))
      (progn
        (goto-char this-start)
        (call-interactively #'mpc-select)
        (goto-char this-end)
        (and (eobp)
             (forward-line -1))
        (call-interactively #'mpc-select-extend)
        (let (select-active-regions)
          (deactivate-mark)))))

;; ****** mpc goto current pos
  (defvar entropy/emacs-music--mpc-goto-current-pos-fake nil)
  (defun entropy/emacs-music--mpc-goto-current-pos ()
    ;; return nil when fatal or a point that goes to
    (unless entropy/emacs-music--mpc-goto-current-pos-fake
      (ignore-errors
        (with-current-buffer (mpc-proc-buffer (mpc-proc) 'songs)
          (goto-char
           (marker-position overlay-arrow-position))
          (recenter-top-bottom '(middle))))))

;; ***** Playlist

  (defun entropy/emacs-music--mpc-gen-default-que-playlist-name ()
    (let* ((playlists (mpc-cmd-list 'Playlist))
          (default-name "----*Que*----")
          (rtn default-name))
      (while (member rtn playlists)
        (setq rtn (format "%s(%s)" default-name (random 1000))))
      rtn))

  (defun entropy/emacs-music--mpc-get-current-playlist-constraints ()
    (let ((playlist (assoc 'Playlist (mpc-constraints-get-current))))
      (if playlist
          (list playlist)
        nil)))

  (defun entropy/emacs-music--mpc-playlist ()
    "sama as `mpc-playlist' but for current-playlist only
i.e. not jump to que playlist."
    (mpc-constraints-push 'noerror)
    (mpc-constraints-restore
     (entropy/emacs-music--mpc-get-current-playlist-constraints)))

  (defun entropy/emacs-music-mpc-songs-buffer-refresh ()
    (interactive nil mpc-songs-mode)
    (entropy/emacs-music--mpc-playlist)
    (let* ((song-buff (get-buffer "*MPC-Songs*"))
           (song-win (ignore-errors (get-buffer-window song-buff))))
      (when (and song-win
                 (with-current-buffer song-buff
                   (eq major-mode 'mpc-songs-mode)))
        (with-selected-window song-win
          (when (with-current-buffer song-buff
                  (entropy/emacs-music--mpc-goto-current-pos))
            (recenter-top-bottom '(middle)))))))

  (defun entropy/emacs-music--mpc-choose-playlist-constraints ()
    (interactive nil mpc-songs-mode mpc-tagbrowser-mode mpc-status-mode)
    (let ((playlists (mpc-cmd-list 'Playlist))
          candis
          constraints)
      (dolist (el playlists)
        (push (cons el (list 'Playlist el)) candis))
      (setq candis (append (list
                            (list
                             (entropy/emacs-music--mpc-gen-default-que-playlist-name)
                             ))
                           candis))
      (setq constraints
            (alist-get
             (completing-read "Choose playlist to view: "
                              candis
                              nil t)
             candis
             nil nil 'string=)
            constraints (unless (null constraints) (list constraints)))
      constraints))

  (defun entropy/emacs-music-mpc-jump-to-playlist ()
    (interactive nil mpc-songs-mode mpc-tagbrowser-mode mpc-status-mode)
    (let ((constraints
           (entropy/emacs-music--mpc-choose-playlist-constraints)))
      (mpc-constraints-restore constraints)))

  (defun entropy/emacs-music--mpc-choose-playlist-name
      (prompt)
    (let ((quename (entropy/emacs-music--mpc-gen-default-que-playlist-name))
          (playlists (mpc-cmd-list 'Playlist))
          rtn)
      (setq playlists (append (list quename) playlists)
            rtn
            (completing-read (format "Choose playlist (%s): " prompt)
                             playlists nil t))
      (if (string= rtn quename)
          nil
        rtn)))

  (defun entropy/emacs-music-mpc-add-to-playlist ()
    (interactive nil mpc-songs-mode)
    (let ((songs (mapcar #'car (mpc-songs-selection)))
          (plchosen (entropy/emacs-music--mpc-choose-playlist-name
                     "to add")))
      (mpc-cmd-add songs plchosen)
      (message "Appended %d songs" (length songs))
      ;; Return the songs added.  Used in `mpc-play'.
      songs))

;; ***** Song play and add/delete from playlist

  (defun entropy/emacs-music-mpc-auto-add-and-play ()
    "Play current music in `mpc-songs-mode'.

Add current music to queue when its not in thus."
    (interactive nil mpc-songs-mode)
    (condition-case nil
        (progn
          (call-interactively #'mpc-songs-jump-to))
      (error
       (let (_)
         (when (null mpc-select)
           (call-interactively #'mpc-select))
         (let ((entropy/emacs-music--mpc-goto-current-pos-fake t))
           (mpc-playlist-add))
         (call-interactively #'mpc-songs-jump-to)
         (call-interactively #'entropy/emacs-music-mpc-unselect-all))))
    (setq entropy/emacs-music--mpc-auto-add-and-play-sinal t))

  (defun entropy/emacs-music-mpc-remove-songs-from-playlist
      (&optional non-selection-clear)
    (interactive "P" mpc-songs-mode)
    (let ((entropy/emacs-message-non-popup t)
          (deleted-counts 0)
          cur-select
          cur-select-pos
          (cur-play-pos (alist-get 'Pos mpc-status))
          (cur-line (line-number-at-pos))
          (clear-selections
           (lambda ()
             (unless non-selection-clear
               (call-interactively
                #'entropy/emacs-music-mpc-unselect-all)))))
      (if (not (eq major-mode 'mpc-songs-mode))
          (entropy/emacs-message-do-message
           "%s%s"
           (red "Warn: ")
           (yellow "You are not in *MPC-Songs* buffer!"))
        ;; grab current play song pos
        (when (null mpc-select)
          (call-interactively #'mpc-select))

        ;; batch delete songs from current playlist
        (dolist (el (mapcar #'cdr (mpc-songs-selection)))
          (setq cur-select (list el)
                cur-select-pos el)
          ;; warn for delete current playing track
          (when (and (ignore-errors (= cur-select-pos (string-to-number cur-play-pos)))
                     (not (stringp mpc-songs-playlist)))
            (unwind-protect
                (user-error
                 "Can not delete current tracked on song from playlist Aborted operation!"
                 )
              (call-interactively #'entropy/emacs-music-mpc-unselect-all)))
          ;; delete main procedure
          (if (and (not (null (car cur-select)))
                   (numberp (car cur-select)))
              (progn
                (mpc-cmd-delete cur-select mpc-songs-playlist)
                (cl-incf deleted-counts))
            (funcall clear-selections)
            (cond
             ((null cur-select)
              (user-error "Playlist is empty"))
             (t
              (user-error
               "Can not get call back from mpc daemon, please take a while and redo thus"
               )))))

        ;; end procedure
        (funcall clear-selections)
        (entropy/emacs-music--mpc-playlist)
        (progn
          (entropy/emacs-buffer-goto-line cur-line)
          (recenter-top-bottom '(middle)))
        (message "Deleted %s songs from current playlist <%s>"
                 deleted-counts
                 (if (stringp mpc-songs-playlist)
                     mpc-songs-playlist
                   "Default")))))

;; ***** Volume increase/downcase
  (defun entropy/emacs-music-mpc-increae-volume ()
    (interactive nil mpc-songs-mode mpc-tagbrowser-mode mpc-status-mode)
    (let* ((curvol (string-to-number (cdr (assq 'volume mpc-status))))
           (newvol (+ curvol 5))
           (newvol-str (number-to-string newvol)))
      (if (< newvol 100)
          (mpc-proc-cmd (list "setvol" newvol-str)
                        'mpc-status-refresh)
        (message "Warn: mpc vol was loudest!"))))

  (defun entropy/emacs-music-mpc-decrease-volume ()
    (interactive nil mpc-songs-mode mpc-tagbrowser-mode mpc-status-mode)
    (let* ((curvol (string-to-number (cdr (assq 'volume mpc-status))))
           (newvol (- curvol 5))
           (newvol-str (number-to-string (- curvol 5))))
      (if (> newvol 0)
          (mpc-proc-cmd (list "setvol" newvol-str)
                        'mpc-status-refresh)
        (message "Warn: mpc vol was mute!"))))

;; **** end
  )


;; ** bongo
(use-package bongo
;; *** preface

;; *** defines
  :commands
  (bongo-switch-to-buffer
   bongo-switch-buffers
   bongo-dired-library-mode
   entropy/emacs-music-bongo-add-dired-files)
  :eemacs-functions
  (bongo-buffer
   bongo-library-buffer
   bongo-playlist-buffer
   bongo-track-line-p)
  :eemacs-macros
  (with-bongo-library-buffer)

;; *** init
  :init

  (entropy/emacs-lazy-initial-advice-before
   '(dired-mode)
   "bongo-dired-init" "bongo-dired-init"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (entropy/emacs-hydra-hollow-add-to-major-mode-hydra
    'dired-mode '(dired dired-mode-map)
    '("Bongo"
      (("b" (if (bound-and-true-p bongo-dired-library-mode)
                (bongo-dired-library-mode -1)
              (bongo-dired-library-mode 1))
        "Toggle Bongo library mode in current dired buffer"
        :enable t :exit nil :map-inject t
        :toggle (bound-and-true-p bongo-dired-library-mode))
       ("C-b a" entropy/emacs-music-bongo-add-dired-files
        "Add marked files to the Bongo library."
        :enable t :exit t)
       ("C-b C-a" (bongo-insert-directory default-directory)
        "Add current dired directory to the Bongo library."
        :enable t :exit t)
       ("C-b M-a" (bongo-insert-directory-tree default-directory)
        "Add current dired directory recursively to the Bongo library."
        :enable t :exit t)))))

;; *** config
  :config

  ;; focely disable bongo mode line indictor at startup time, because
  ;; it may cause modeline format pollution and be with unstable xpm
  ;; indictor image render function.
  ;;
  ;; We must set it after the `bongo.el' loaded to override what it
  ;; will enable at the load time.
  (setq bongo-mode-line-indicator-mode nil)
  (entropy/emacs-defconst bongo-mode-line-indicator-mode nil)

  (when (not (entropy/emacs-custom-var-is-customized-p 'bongo-enabled-backends))
    ;; prefer use mplayer since it's the most compatible way for bongo
    ;; in eemacs test.
    (setq bongo-enabled-backends '(mplayer)))
  (setq bongo-mpv-extra-arguments '("--mute=no" "-vo" "null")
        bongo-mplayer-extra-arguments '("-novideo"))

  (defun entropy/emacs-music-bongo-add-dired-files ()
    "Add marked files to the Bongo library and then popup the
`bongo-library-buffer' which the buffer point position has been
jumped to the main context."
    (interactive nil dired-mode)
    (let (lbuf files)
      (dired-map-over-marks
       (push (dired-get-filename) files)
       nil t)
      (setq files (nreverse files))
      (with-bongo-library-buffer
        (setq lbuf (current-buffer))
        (save-excursion
          (goto-char (point-max))
          (mapc 'bongo-insert-file files))
        (unless (bongo-track-line-p)
          (goto-char (point-min))
          (while (and (car (entropy/emacs-forward-line))
                      (not (bongo-track-line-p))))))
      (and (buffer-live-p lbuf) (display-buffer lbuf))))

  (defun entropy/emacs--bongo-play-next-or-backto-first (&optional n)
    "Maybe start playing the next track in the nearest playlist buffer.
If there is no next track to play, just stop playback, if current played
the last item in the playlist then fallback to the first one replay the
playlist.

With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
progressive playback.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
    (interactive "P")
    (condition-case err
        (bongo-play-next n)
      (error
       (bongo-stop)
       (let ((lps (bongo-point-at-last-track-line))
             (cps (bongo-point-at-current-track-line)))
         (if (and (number-or-marker-p lps) (number-or-marker-p cps)
                  (= cps lps))
             (bongo-play-line (bongo-point-at-first-track-line))
           (error "%s" err))))))
  (setq-default bongo-next-action 'entropy/emacs--bongo-play-next-or-backto-first)

  (and (fboundp 'bongo-repeating-playlist-mode)
       (entropy/emacs-!error-as-eemacs-internal-error
        "Bongo has builtin implements func: bongo-repeating-playlist-mode"))
  (defun bongo-repeating-playlist-mode (&optional default)
    "Switch to repeating playlist mode in the nearest playlist buffer.
In repeating playback mode, the current playlist is played over and over.
With prefix argument DEFAULT, make repeating playlist the default mode.
This function sets the buffer-local or global value of `bongo-next-action'."
    (interactive "P")
    (if (not default)
        (with-bongo-playlist-buffer
          (setq bongo-next-action 'entropy/emacs--bongo-play-next-or-backto-first)
          (message "Switched to repeating playback mode."))
      (setq-default bongo-next-action 'entropy/emacs--bongo-play-next-or-backto-first)
      (message "Repeating playback is now the default mode."))
    (force-mode-line-update))
  (put 'entropy/emacs--bongo-play-next-or-backto-first
       'bongo-playback-mode-indicator "repeat playlist")

  (entropy/emacs-api-restriction/elpkg-eemacs-ext-stable-build-repo-version
      'redefine/bongo-default-track
    :do-error t
    :elpkg-eemacs-ext-stable-build-repo-version "3.2.0"

    (entropy/emacs-defconst/only-allow/local eemacs//bongo-buffer-p-either-dired-buffer-p nil)
    (defun eemacs//bongo-buffer-p (ofunc &rest oargs)
      "Bongo's sets of insertion should not in dired buffer."
      (unless (and (not eemacs//bongo-buffer-p-either-dired-buffer-p)
                   (entropy/emacs-derived-cur-major-mode-p 'dired-mode))
        (apply ofunc oargs)))
    (advice-add 'bongo-buffer-p :around #'eemacs//bongo-buffer-p)
    (defun eemacs//bongo-switch-buffer (ofunc &rest oargs)
      (let ((eemacs//bongo-buffer-p-either-dired-buffer-p t))
        (apply ofunc oargs)))
    (dolist (func '(bongo-switch-to-buffer bongo-switch-buffers))
      (advice-add func :around #'eemacs//bongo-switch-buffer))

    ;; FIXME: this is a bug of `bongo-player-times-last-updated' which
    ;; will never trigger body of `bongo-player-times-changed' when
    ;; its hint the top value of the LOW part of (current-time) value
    ;; which is standardized as always less than 65536, in which case
    ;; there's impossible for any current LOW part less than 65535.
    ;;
    ;; Obviously this bug will made the `bongo-track-length' never the
    ;; updated via `bongo-redisplay-line' in which case since the
    ;; track length is never obtained by `bongo-player-times-changed'
    ;; in any backend's filter part.
    (advice-patch 'bongo-player-times-changed
                  '(> current-seconds
                      (let ((ot bongo-player-times-last-updated))
                        (if (= ot 65535) (setq bongo-player-times-last-updated 0)
                          ot)))
                  '(> current-seconds bongo-player-times-last-updated))

    (entropy/emacs-setf-by-body bongo-infoset-from-file-name-function
      (entropy/emacs-!cl-defun
          eemacs//bongo-simple-infoset-from-file-name (file-name)
        (let ((track-length-part
               (when (and (boundp 'bongo-track-length) bongo-track-length)
                 `((length . ,bongo-track-length)))))
          `((track (title . ,(file-name-sans-extension
                              (file-name-nondirectory
                               (if (bongo-uri-p file-name)
                                   (bongo-unescape-uri file-name)
                                 file-name))))
                   ,@track-length-part)))))

    (defun eemacs//bongo-default-track ()
      (let* ((other-fields-width
              (with-temp-buffer
                (insert (bongo-format-infoset
                         (entropy/emacs-mapcar-without-orphans
                          (lambda (x) (unless (eq (car x) 'track) x))
                          bongo-infoset nil 'nil)))
                (current-column)))
             (indentation-width
              (* (length bongo-indentation-string)
                 (bongo-line-indentation bongo-line)))
             (index-str (when bongo-index
                          (concat bongo-index ". ")))
             (title-width (- bongo-track-length-column
                             (+ indentation-width other-fields-width
                                (or (and index-str (string-width index-str)) 0))))
             (notruncate-p
              (not
               (or (bongo-playlist-buffer-p bongo-target)
                   (bongo-library-buffer-p  bongo-target)))))
        (concat
         index-str
         (cl-labels ((trstr-func (str &rest args)
                       (if notruncate-p str
                         (apply 'truncate-string-to-width str args))))
           (let (str (elp
                      (cond
                       ((bound-and-true-p truncate-string-ellipsis)
                        (bound-and-true-p truncate-string-ellipsis))
                       ((char-displayable-p ?…) "…")
                       ("..."))))
             (entropy/emacs-setf-by-body str
               (let* ((file (bongo-line-file-name))
                      (fdname (and file
                                   (file-name-nondirectory
                                    (directory-file-name (file-name-directory file))))))
                 (when fdname
                   (setq fdname (trstr-func
                                 fdname (floor (/ bongo-track-length-column 2.0))
                                 nil nil elp)))
                 (if (not fdname) bongo-title
                   (format "%s/%s"
                           (propertize fdname 'face 'bongo-album-title)
                           (file-name-nondirectory file)))))
             (trstr-func
              str
              (if (> title-width 0) (min title-width (string-width str))
                (string-width str))
              nil nil elp)))
         (when-let (((and bongo-display-track-lengths bongo-length
                          (bongo-playlist-buffer-p bongo-target))))
           (concat
            (when-let (((> title-width 0))
                       (lw (+
                            ;; fine tune padding
                            20
                            ;; current SPC char
                            1
                            title-width)))
              (if (fboundp 'string-pixel-width)
                  (propertize
                   " "
                   'display
                   `(space
                     :align-to
                     (,(string-pixel-width (make-string lw 32)))))
                (propertize " " 'display `(space :align-to ,lw))))
            bongo-length)))))
    (setq bongo-track-function 'eemacs//bongo-default-track)

    ;; mpv backend refine
    (defvar-local eemacs//bongo-mpv-pause/resume-current-op nil)
    (defun eemacs//bongo-mpv-player-pause/resume (player)
      "refine due to `eemacs//bongo--mpv-socket-filter'"
      (if (bongo-player-paused-p player)
          (progn (bongo--run-mpv-command player
                                         "pause"
                                         "set_property_string"
                                         "pause"
                                         "no")
                 (setq eemacs//bongo-mpv-pause/resume-current-op 'unpause))
        (bongo--run-mpv-command player
                                "pause"
                                "set_property"
                                "pause"
                                t)
        (setq eemacs//bongo-mpv-pause/resume-current-op 'pause)))
    (advice-add 'bongo-mpv-player-pause/resume
                :override #'eemacs//bongo-mpv-player-pause/resume)

    (defun eemacs//bongo--mpv-socket-filter (process output)
      "Refine to adapt new mpv version pause/resume jsonipc format"
      (let ((player (process-get process 'bongo-player))
            request_id_val)
        (dolist (parsed-response (mapcar #'json-read-from-string
                                         (split-string output "\n" t)))
          (cond
           ;; EEMACS_MAINTENANCE: the new mpv jsonrpc pause/resume format doesn't use event slot
           ((progn (setq request_id_val (bongo-alist-get parsed-response 'request_id))
                   (member request_id_val '("pause" "unpause")))
            (if (with-bongo-playlist-buffer
                  (eq eemacs//bongo-mpv-pause/resume-current-op 'pause))
                (bongo-player-put player 'paused t)
              (bongo-player-put player 'paused nil)))
           ;; EEMACS_MAINTENANCE: the original old mpv jsonrpc pause/resume format
           ((assoc 'event parsed-response)
            (pcase (bongo-alist-get parsed-response 'event)
              (`"pause" (bongo-player-put player 'paused t))
              (`"unpause" (bongo-player-put player 'paused nil))))
           (t
            (pcase (bongo-alist-get parsed-response 'request_id)
              (`"time-pos"
               (progn
                 (bongo-player-update-elapsed-time
                  player (bongo-alist-get parsed-response 'data))
                 (bongo-player-times-changed player)))
              (`"duration"
               (progn
                 (bongo-player-update-total-time
                  player (bongo-alist-get parsed-response 'data))
                 (bongo-player-times-changed player)))
              (`"metadata"
               (let* ((data  (bongo-alist-get parsed-response 'data))
                      (album (bongo-alist-get data 'album))
                      (title (bongo-alist-get data 'title))
                      (genre (bongo-alist-get data 'genre)))
                 (bongo-player-put player 'metadata-fetched t)
                 (when album
                   (bongo-player-put player 'stream-name album))
                 (when title
                   (bongo-player-put player 'stream-part-title title))
                 (when genre
                   (bongo-player-put player 'stream-genre genre))
                 (when (or album title genre)
                   (bongo-player-metadata-changed player))))))))))
    (advice-add 'bongo--mpv-socket-filter
                :override #'eemacs//bongo--mpv-socket-filter)

    (defalias 'bongo-goto-current-playing-track-line 'bongo-recenter
      "eemacs alias for `bongo-recenter' to obviously hint via minibuffer promption.")

    )

  (defconst eemacs//bongo-modeline-indicator/keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1]
        (lambda nil (interactive)
          (bongo-pause/resume)))
      map))
  (defvar eemacs//bongo-modeline-indicator/cache-str nil)
  (defun eemacs//bongo-modeline-indicator/core nil
    (let* ((icp (entropy/emacs-icons-displayable-p))
           (player (with-bongo-playlist-buffer (bound-and-true-p bongo-player)))
           (upp (and player (bongo-player-paused-p player)))
           (ppp (and player (not upp) (bongo-player-running-p player)))
           (stp (or (null player) (bongo-player-explicitly-stopped-p player)))
           (str
            (if ppp
                (if icp (nerd-icons-faicon "nf-fa-pause_circle_o")
                  "bongo-playing")
              (if stp nil
                (if icp (nerd-icons-faicon "nf-fa-play_circle_o")
                  "bongo-paused")))))
      (if (not str) (setq str "")
        (entropy/emacs-setf-by-body str
          (propertize
           str
           'mouse-face 'mode-line-highlight
           'help-echo
           (format
            "Bongo is %s (%s)%s"
            (if ppp "playing" "paused")
            (format "current item '%s'"
                    (ignore-errors
                      (bongo-alist-get
                       (bongo-alist-get (bongo-player-get player 'infoset) 'track)
                       'title)))
            "\nmouse-1: pause/resume")
           'local-map eemacs//bongo-modeline-indicator/keymap)))
      (setq eemacs//bongo-modeline-indicator/cache-str
            (replace-regexp-in-string "%" "%%" str))))
  (defun eemacs//bongo-modeline-indicator nil
    (if (not entropy/emacs-current-session-is-idle-p)
        (or eemacs//bongo-modeline-indicator/cache-str "")
      (eemacs//bongo-modeline-indicator/core)
      eemacs//bongo-modeline-indicator/cache-str))
  (add-to-list
   'mode-line-misc-info
   (list t '(:eval (eemacs//bongo-modeline-indicator))))

  ;; trash unsafe command directly used in keymap
  (dolist (map (list bongo-mode-map bongo-playlist-mode-map bongo-library-mode-map))
    (keymap-substitute map 'bongo-rename-line 'ignore))

;; *** end
  )

;; * provide
(provide 'entropy-emacs-music)
