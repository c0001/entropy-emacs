;;; entropy-emacs-emms.el --- entropy-emacs emms configuration
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-emms.el
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
;; Simple emms config for `entropy-emacs'.
;; 
;; * Configuration:
;; 
;; Just used for `entropy-emacs', loading automatically.
;; 
;; * Code:
;; 
;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** main
(use-package emms
;; *** bind key
  :bind
  (("C-c M-e g" . emms-play-directory)
   ("C-c M-e d" . emms-play-dired)
   ("C-c M-e v" . emms-playlist-mode-go)
   ("C-c M-e x" . emms-start)
   ("C-c M-e SPC" . emms-pause)
   ("C-c M-e s" . emms-stop)
   ("C-c M-e n" . emms-next)
   ("C-c M-e p" . emms-previous)
   ("C-c M-e e" . emms-play-file)
   ("C-c M-e h" . emms-shuffle)
   ("C-c M-e r" . emms-toggle-repeat-track)
   ("C-c M-e R" . emms-toggle-repeat-playlist)
   ("C-c M-e >" . emms-seek-forward)
   ("C-c M-e <" . emms-seek-backward)
   ("C-c M-e f" . emms-show))
;; *** init
  :init
;; **** set variable
  (setq emms-repeat-playlist nil
	emms-lyrics-coding-system nil)
  
;; *** config
  :config
  (emms-standard))

;; * provide
(provide 'entropy-emacs-emms)

