;;; entropy-emacs-ibuffer.el --- entropy-emacs ibuffer configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ibuffer.el
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
;; IBUFFER configuration of `entropy-emacs'. 
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** Group ibuffer's list by project root
(if entropy/emacs-enable-ibuffer-projectitle
;; Because projectitle in ibuffer will Straining system resources ，so let projectitle insert in
;; ibuffer be optional, and let all buffer switcher function be kill-buffer function cause if not do
;; this it will cause other opertion in other buffer be lagging a lot.
;;
;; ** use ibuffer-projectitle
    (use-package ibuffer-projectile
      :bind ("C-x C-b" . ibuffer)
      :init
      (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
      (add-hook 'ibuffer-hook
                (lambda ()
                  (ibuffer-auto-mode 1)
                  (ibuffer-projectile-set-filter-groups)
                  (unless (eq ibuffer-sorting-mode 'alphabetic)
                    (ibuffer-do-sort-by-alphabetic))))
      (add-hook 'ibuffer-hook '(lambda () (eyebrowse-mode 0) (winner-mode 0)))

      :config
      (defun entropy/emacs-ibuffer-kill-ibuffer ()
        (interactive)
        (if (not (string= major-mode 'ibuffer-mode))
            (message "This function is used in Ibuffer!")
          (progn
            (entropy/emacs-kill-buffer-and-window)
            (eyebrowse-mode 1)
            (winner-mode 1))))
      (define-key ibuffer-mode-map (kbd "q") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x b") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x <left>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x <right>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x <up>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x <down>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x C-<left>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x C-<right>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x C-<up>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x C-<down>") 'entropy/emacs-ibuffer-kill-ibuffer)
      (define-key ibuffer-mode-map (kbd "C-x o") 'entropy/emacs-ibuffer-kill-ibuffer))
  
;; ** none ibuffer projectitle
  (progn
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (add-hook 'ibuffer-hook
              #'(lambda ()
                  (progn
                    (ibuffer-set-filter-groups-by-mode)

;; *** size readable form EmacsWiki `https://www.emacswiki.org/emacs/IbufferMode'
                    ;; Use human readable Size column instead of original one
                    (define-ibuffer-column size-h
                      (:name "Size" :inline t)
                      (cond
                       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
                       ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
                       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
                       (t (format "%8d" (buffer-size)))))

                    ;; Modify the default ibuffer-formats
                    (setq ibuffer-formats
                          '((mark modified read-only " "
                                  (name 18 18 :left :elide)
                                  " "
                                  (size-h 9 -1 :right)
                                  " "
                                  (mode 16 16 :left :elide)
                                  " "
                                  filename-and-process))))))))

;; * provide
(provide 'entropy-emacs-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
