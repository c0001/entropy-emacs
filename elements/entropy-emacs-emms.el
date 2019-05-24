;;; File name: init-emms.el ---> for entropy-emacs
;;
;; Copyright (c) 2017 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
;; ** require
(require 'entropy-emacs-const)
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
	emms-source-file-default-directory "~/Music/" 
	emms-lyrics-dir "~/Music/"
	emms-lyrics-coding-system nil)
;; *** config
  :config
  (emms-standard)
;; **** use mpv be the emms's player
  (use-package emms-player-mpv
    :init
    (progn
      (require 'emms-player-mpv)
      (add-to-list 'emms-player-list 'emms-player-mpv)))
;; **** display emms-modeline  
  (if entropy/emacs-use-emms-mode-line
      (emms-mode-line 1)
    (emms-mode-line -1))
  (if entropy/emacs-use-emms-mode-line
      (emms-playing-time 1)
    (emms-playing-time-disable-display))
;; ***** Cycle mode line
  (when entropy/emacs-use-emms-mode-line
    (use-package emms-mode-line-cycle
      :init
      (custom-set-variables '(emms-mode-line-cycle-use-icon-p t))
      (add-hook 'after-init-hook (lambda () (emms-mode-line-cycle 1))))))




;; * provide
(provide 'entropy-emacs-emms)

