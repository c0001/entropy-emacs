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

;; **** initial UI
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
           (buf (mpc-proc-buffer proc 'status))
           (songs-buf (mpc-proc-buffer proc 'songs))
           (songs-win (if songs-buf (get-buffer-window songs-buf 0))))
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
              '(:eval (if entropy/emacs-current-session-is-idle
                          mode-line-position
                        " ⨂"))
              mode-line-format)))))))

  (defun entropy/emacs-music-mpc-mini ()
    "The `mpc' mini tpype which just display side window group
for the songs list and status callback."
    (interactive)
    (let ((entropy/emacs-music--mpc-mini-mode t))
      (mpc)))

  (defun entropy/emacs-music-mpc--patch-quit-around-advice (orig-func &rest orig-args)
    (let* ((select-window-mpc-mini-mode-p
            (window-parameter (selected-window) 'mpc-mini-mode)))
      ;; remove all mpc internal window configuration memory in which
      ;; case we use
      ;; `entropy/emacs-music-mpc--orig-window-configuration' mechanism instead.
      (mapc (lambda (buff)
              (with-current-buffer buff
                (when (buffer-local-value 'mpc-previous-window-config buff)
                  (kill-local-variable 'mpc-previous-window-config))))
            (buffer-list))
      (apply orig-func orig-args)
      (when (and (window-configuration-p entropy/emacs-music-mpc--orig-window-configuration)
                 ;; we do not recover origin window configuration when
                 ;; current mpc in mini mode.
                 (not select-window-mpc-mini-mode-p))
        (set-window-configuration
         entropy/emacs-music-mpc--orig-window-configuration)
        (setq entropy/emacs-music-mpc--orig-window-configuration nil))))

;; **** Utils
;; ***** mpc select
  (defun entropy/emacs-music-mpc-unselect-all (&optional event)
    "Unselect the tag value at point."
    (interactive (list last-nonmenu-event))
    (mpc-event-set-point event)
    (if (and (bolp) (eobp)) (forward-line -1))
    (mapc 'delete-overlay mpc-select)
    (setq mpc-select nil))

  (defun entropy/emacs-music-mpc-unselect-single (&optional event)
    (interactive (list last-nonmenu-event))
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
    (interactive)
    (let ((this-start (line-number-at-pos (region-beginning)))
          (this-end (line-number-at-pos (region-end))))
      (goto-line this-start)
      (while (and (<= (line-number-at-pos (point))
                      this-end)
                  (not (eobp)))
        (call-interactively
         #'entropy/emacs-music-mpc-unselect-single))
      (let (select-active-regions)
        (deactivate-mark))))

  (defun entropy/emacs-music-mpc-select-single (&optional event)
    (interactive (list last-nonmenu-event))
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
    (interactive)
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

;; ***** mpc goto current pos
  (defvar entropy/emacs-music--mpc-goto-current-pos-fake nil)
  (defun entropy/emacs-music--mpc-goto-current-pos ()
    ;; return nil when fatal or a point that goes to
    (unless entropy/emacs-music--mpc-goto-current-pos-fake
      (ignore-errors
        (with-current-buffer (mpc-proc-buffer (mpc-proc) 'songs)
          (goto-char
           (marker-position overlay-arrow-position))
          (recenter-top-bottom '(middle))))))

;; **** Playlist

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
    (interactive)
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
    (interactive)
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
    (interactive)
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
    (interactive)
    (let ((songs (mapcar #'car (mpc-songs-selection)))
          (plchosen (entropy/emacs-music--mpc-choose-playlist-name
                     "to add")))
      (mpc-cmd-add songs plchosen)
      (message "Appended %d songs" (length songs))
      ;; Return the songs added.  Used in `mpc-play'.
      songs))

;; **** Song play and add/delete from playlist
  (defvar entropy/emacs-music--mpc-auto-add-and-play-sinal nil)
  (defun entropy/emacs-music-mpc-auto-add-and-play ()
    "Play current music in `mpc-songs-mode'.

Add current music to queue when its not in thus."
    (interactive)
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
    (interactive "P")
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
        (when (ignore-errors
                (goto-line cur-line))
          (recenter-top-bottom '(middle)))
        (message "Deleted %s songs from current playlist <%s>"
                 deleted-counts
                 (if (stringp mpc-songs-playlist)
                     mpc-songs-playlist
                   "Default")))))

;; **** Volume increase/downcase
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
     (mpc-songs-mode (mpc mpc-songs-mode-map) t (2 2 2)))
    ((:enable t)
     (mpc-tagbrowser-mode (mpc mpc-tagbrowser-mode-map) t (2 2 2)))
    ((:enable t)
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
     ("<" (mpc-seek-current "-10") "Seek backward 10s"
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
   "%-5{Time} %25{Title} %20{Album} %20{Artist} %5{Date}"
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
  (advice-add 'mpc-quit
              :around
              #'entropy/emacs-music-mpc--patch-quit-around-advice)

  (defun entropy/emacs-music--mpc-around-advice-for-mpc-volum-refresh
      (orig-func &rest orig-args)
    (condition-case error
        (apply orig-func orig-args)
      (error
       (message "Warn: mpc callback with some fatal for `mpc-volumn-refresh'"))))

  (advice-add 'mpc-volume-refresh
              :around
              #'entropy/emacs-music--mpc-around-advice-for-mpc-volum-refresh)

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


  )


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
         (("c b" entropy/emacs-music-bongo-add-dired-files
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
