;;; entropy-emacs-gnus.el --- GNUS configuration for entropy-emacs  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190907  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-gnus.el
;; Keywords:      gnus, news
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
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
;; GNUS configuration for =entropy-emacs=.
;;
;; * Configuration:
;;
;; Using for =entropy-emacs= only.
;;
;; * Code:
;; ** require

(defvar mail-source-directory)
(defvar message-directory)
(defvar nnfolder-directory)
(defvar gnus-init-file)
(defvar gnus-startup-file)
(defvar gnus-read-newsrc-file)
(defvar gnus-save-newsrc-file)
(defvar gnus-use-dribble-file)
(defvar gnus-read-active-file)

;; ** gnus
(use-package gnus
  :ensure nil
  :config
  (entropy/emacs-setf-from-plist entropy/emacs-gnus-init-config nil
    ;; gnus home setting
    :gnus-home        gnus-home-directory
    ;; gnus news dir
    :gnus-news-dir    gnus-directory
    :gnus-news-dir    gnus-kill-files-directory
    ;; gnus mail dir
    :mail-dir         mail-source-directory
    :mail-dir         message-directory
    :mail-dir         nnfolder-directory
    :mail-temp-dir    mail-default-directory
    ;; gnus-init config file
    :init-file        gnus-init-file
    ;; newrc source file
    :startup-file     gnus-startup-file
    :read-newsrc      gnus-read-newsrc-file
    :save-newsrc      gnus-save-newsrc-file
    ;; dribble file (The update cache)
    :use-dribble      gnus-use-dribble-file
    ;; fetch updat sources type, defualt be 'some' recommended set it to 't'
    :read-active-file gnus-read-active-file)

  ;; Unbind `message-kill-address' when in gnus mail `message-mode-map'.
  ;; Because of that this will cover `browse-kill-ring' default keybinding.
  (defvar message-mode-map)
  (entropy/emacs-lazy-load-simple 'gnus-msg
    (define-key message-mode-map (kbd "C-c M-k") nil)))


;; * provide
(provide 'entropy-emacs-gnus)
