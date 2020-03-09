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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** ibuffer core
(use-package ibuffer
  :ensure nil
  :eemacs-tpha
  (((:enable t))
   ("WI&BUF"
    (("C-x C-b" ibuffer "Begin using Ibuffer to edit a list of buffers"
      :enable t
      :exit t
      :global-bind t))))

  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (entropy/emacs-lazy-load-simple counsel
    (with-no-warnings
      (defun entropy/emacs-ibuffer-find-file ()
        (interactive)
        (let ((default-directory
                (let ((buf (ibuffer-current-buffer)))
                  (if (buffer-live-p buf)
                      (with-current-buffer buf
                        default-directory)
                    default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file
                  :override
                  #'entropy/emacs-ibuffer-find-file))))


;; ** ibuffer all the icons feature
(use-package all-the-icons-ibuffer
  :if (display-graphic-p)
  :init (all-the-icons-ibuffer-mode 1))

;; ** ibuffer-projectitle display
(use-package ibuffer-projectile
  :if entropy/emacs-enable-ibuffer-projectitle
  :preface
  (defun entropy/emacs-ibuffer--ibprjt-init ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            #'entropy/emacs-ibuffer--ibprjt-init)
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon
              "file-directory"
              :face ibuffer-filter-group-name-face
              :v-adjust -0.05
              :height 1.25)
             " ")
          "Project: ")))

;; ** common ibuffer display
(when (not entropy/emacs-enable-ibuffer-projectitle)
  (defun entropy/emacs-ibuffer--init-common ()
    (progn
      (ibuffer-set-filter-groups-by-mode)

      ;; size readable form EmacsWiki `https://www.emacswiki.org/emacs/IbufferMode'
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
                    filename-and-process)))))
  (add-hook 'ibuffer-hook
            #'entropy/emacs-ibuffer--init-common))

;; * provide
(provide 'entropy-emacs-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
