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
;; ** preparation

(defun entropy/emacs-set-fixed-pitch-serif-face-to-monospace ()
  "Set face `fixed-pitch-serif' to entropy-emacs specific monospace
style.

This funciton will solve the problem that the symbol pattern
display ugly and small in `info-mode' or other simulate occasions."
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Monospace" :slant 'italic))

(with-eval-after-load 'faces
  (entropy/emacs-set-fixed-pitch-serif-face-to-monospace))

(defgroup entropy/emacs-defface-group nil
  "Eemacs customizable faces group"
  :group 'entropy-emacs-customize-top-group)

;; ** eyebrowse faces
(defface entropy/emacs-defface-face-for-eyebrowse-back-top-wg-message-face_body
  '((t :foreground "yellow"))
  "Face for message body area with func `entropy/emacs-basic-eyebrowse-switch-top'"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-eyebrowse-back-top-wg-message-face_content
  '((t :foreground "green2"))
  "Face for message content area with func `entropy/emacs-basic-eyebrowse-switch-top'"
  :group 'entropy/emacs-defface-group)

;; ** welcome buffer
(defface entropy/emacs-defface-face-for-welcome-buffer-title-face
  '((t :height 2.5 :bold t :underline t :overline t))
  "Face for entropy-emacs initial buffer title."
  :group 'entropy/emacs-defface-group)

;; ** modeline faces
;; *** eyebrowse modeline indicator faces
(defface entropy/emacs-defface-face-for-modeline-eyebrowse-face-main
  '((t :foreground "DarkGoldenrod2" :background "black" :bold t))
  "Face for eyebrowse main workspace segment of origin modeline"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-modeline-eyebrowse-face-main_inactive
  '((t :foreground "white" :background "brown"))
  "Face for eyebrowse main workspace segment while window
inactive of origin modeline"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived
  '((t :background "#deaa00" :foreground "purple4" :bold t))
  "Face for eyebrowse derived workspace segment of origin modeline."
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-modeline-eyebrowse-face-derived_inactive
  '((t :foreground "white" :background "DarkOrange4"))
  "Face for eyebrowse derived workspace segment while window
inactive of origin modeline."
  :group 'entropy/emacs-defface-group)

;; ** ivy faces
(defface entropy/emacs-defface-face-for-swiper-dired-candi-inactive-face
  '((t ()))
  "Faces for swiper inactive entries for dired-buffer."
  :group 'entropy/emacs-defface-group)

;; ** hydra face

(defface entropy/emacs-defface-face-for-hydra-red-face
  '((t :foreground "red" :weight bold))
  "Face for hydra head 'red'"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-hydra-blue-face
  '((t :foreground "blue" :weight bold))
  "Face for hydra head 'blue'"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-hydra-orange-face
  '((t :foreground "orange" :weight bold))
  "Face for hydra head 'orange'"
  :group 'entropy/emacs-defface-group)

(defface entropy/emacs-defface-face-for-hydra-grey-face
  '((t :foreground "grey" :weight bold))
  "Face for hydra head 'grey'"
  :group 'entropy/emacs-defface-group)

;; * provide
(provide  'entropy-emacs-defface)
