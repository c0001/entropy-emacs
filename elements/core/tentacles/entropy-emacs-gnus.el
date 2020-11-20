;;; entropy-emacs-gnus.el --- GNUS configuration for entropy-emacs
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
  (setq mail-default-directory (plist-get entropy/emacs-gnus-init-config :mail-temp-dir))
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
  (entropy/emacs-lazy-load-simple gnus-msg
    (define-key message-mode-map (kbd "C-c M-k") nil)))


;; * provide
(provide 'entropy-emacs-gnus)
