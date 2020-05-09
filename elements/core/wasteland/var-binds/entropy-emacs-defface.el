;;; entropy-emacs-faces.el --- entropy emacs defined faces package
;;
;; * Copyright (C) 20190730  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/elements/entropy-emacs-faces.el
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
;; Pre defined faces for other entropy-emacs components using.
;;
;; * Configuration:
;;
;; Just requiring it commonly.
;;
;; * Code:
;; ** init faces
(defface entropy/emacs-defface-face-for-require-faces-head-prompt
  '((t :weight semi-bold :foreground "yellow1"))
  "Face for the head prompt requiring advice
`entropy/emacs-require-loadding")

(defface entropy/emacs-defface-face-for-require-face-tail-prompt
  '((t :weight semi-bold :foreground "linen" :underline t))
  "Face for the tail prompt requiring advice
`entropy/emacs-require-loadding")

(defun entropy/emacs-set-fixed-pitch-serif-face-to-monospace ()
  "Set info-mode font-lock spec face `fixed-pitch-serif' to
entropy-emacs specific monospace style.

This funciton will solve the problem that the symbol pattern
display ugly and small in info-mode."
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Monospace" :slant 'italic))

(entropy/emacs-lazy-load-simple faces
  (entropy/emacs-set-fixed-pitch-serif-face-to-monospace))

;; ** basic faces
(defface entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_body '((t ()))
  "Face for message body area with func `entropy/emacs-basic-eyebrowse-switch-top'")

(set-face-attribute 'entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_body
                    nil :foreground "yellow")

(defface entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_content '((t ()))
  "Face for message content area with func `entropy/emacs-basic-eyebrowse-switch-top'")

(set-face-attribute 'entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_content
                    nil :foreground "green2")


;; ** ui faces
(defface entropy/emacs-defface-face-for-ui-dashboard-title-face '((t ()))
  "Face for entropy-emacs initial buffer title.")

(set-face-attribute 'entropy/emacs-defface-face-for-ui-dashboard-title-face
                    nil :height 2.5 :bold t :underline t :overline t)
;; ** modeline faces
(defface entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main '((t ()))
  "Face for eyebrowse main workspace segment of origin modeline")

(set-face-attribute 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main nil
                    :foreground "DarkGoldenrod2" :background "black" :bold t)

(defface entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main_inactive '((t ()))
  "Face for eyebrowse main workspace segment while window
inactive of origin modeline")

(set-face-attribute 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-main_inactive nil
                    :foreground "white" :background "brown")

(defface entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived '((t ()))
  "Face for eyebrowse derived workspace segment of origin modeline.")

(set-face-attribute 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived nil
                    :background "#deaa00" :foreground "purple4" :bold t)

(defface entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived_inactive '((t ()))
  "Face for eyebrowse derived workspace segment while window
inactive of origin modeline ")

(set-face-attribute 'entropy/emacs-defface-face-for-modeline-mdl-eyebrowse-face-derived_inactive nil
                    :foreground "white" :background "DarkOrange4")


;; ** ivy faces
(defface entropy/emacs-defface-face-for-swiper-dired-candi-inactive-face  '((t ()))
  "Faces for swiper inactive entries  for dired-buffer.")


;; ** hydra face

(defface entropy/emacs-defface-face-for-hydra-red-face '((t ()))
  "face for hydra head 'red'")

(set-face-attribute 'entropy/emacs-defface-face-for-hydra-red-face nil
                    :foreground "red"
                    :weight 'bold)


(defface entropy/emacs-defface-face-for-hydra-blue-face '((t ()))
  "face for hydra head 'blue'")

(set-face-attribute 'entropy/emacs-defface-face-for-hydra-blue-face nil
                    :foreground "blue"
                    :weight 'bold)


(defface entropy/emacs-defface-face-for-hydra-orange-face '((t ()))
  "face for hydra head 'orange'")

(set-face-attribute 'entropy/emacs-defface-face-for-hydra-orange-face nil
                    :foreground "orange"
                    :weight 'bold)

(defface entropy/emacs-defface-face-for-hydra-grey-face '((t ()))
  "face for hydra head 'grey'")

(set-face-attribute 'entropy/emacs-defface-face-for-hydra-grey-face nil
                    :foreground "grey"
                    :weight 'bold)


;; * provide
(provide  'entropy-emacs-defface)
