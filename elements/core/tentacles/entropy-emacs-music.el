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
  :commands
  (mpc
   mpc-seek-current)
  :preface
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

  :eemacs-mmphc
  ((((:enable t)
     (mpc-songs-mode mpc mpc-songs-mode-map t (3 2 2)))
    ((:enable t)
     (mpc-tagbrowser-mode mpc mpc-tagbrowser-mode-map t (3 2 2))))
   ("Common"
    (("P" mpc-pause "Pause playing" :enable t :exit t :map-inject t)
     ("s" mpc-toggle-play "Toggle between play and pause"
      :enable t :exit t :map-inject t)
     ("n" mpc-next "next song" :enable t :exit t :map-inject t)
     ("p" (mpc-proc-cmd "previous") "previous song"
      :enable t :exit t :map-inject t)
     ("RET" mpc-select "Select the tag value at point"
      :enable t :map-inject t :exit t))
    "Seek"
    ((">" (mpc-seek-current "+10") "Seek forward 10s"
      :enable t :map-inject t)
     ("<" (mpc-seek-current "-10") "Seek backward 10s"
      :enable t :map-inject t))
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

  :eemacs-mmphca
  (((:enable t)
    (mpc-songs-mode mpc mpc-songs-mode-map t))
   ("Common"
    (("RET" entropy/emacs-music-mpc-auto-add-and-play
      "Play current music."
      :enable t :exit t :map-inject t))))

  :init
  (setq
   mpc-host
   (format "%s:%s"
           entropy/emacs-mpd-host-url
           entropy/emacs-mpd-host-port)
   mpc-songs-format
   "%-5{Time} %25{Title} %20{Album} %20{Artist} %5{Date}"
   mpc-browser-tags
   '(Artist|Composer|Performer
     Album|Playlist)))


;; * provide
(provide 'entropy-emacs-music)
