;;; entropy-emacs-faces.el --- entropy emacs defined faces package  -*- lexical-binding: t; -*-
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
;; ** require
(require 'color)

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

;; ** common face

(defgroup entropy/emacs-defface-simple-color-face-group nil
  "Eemacs customizable simple color faces group"
  :group 'entropy/emacs-defface-group)

(defun entropy/emacs-defface-gen-simple-color-face (color-name color &rest other-faceattrs)
  "Generate a simple color face using `defface' which belong to the
face group
`entropy/emacs-defface-simple-color-face-group'. Return the FACE.

COLOR-NAME is the specified common emacs supported color name
such as red,yellow,green etc or use arbitrary string but in this
case the COLOR must be specified see below:

COLOR is the hex representation of COLOR-NAME, if nil use
COLOR-NAME (assume its a valid emacs color alias without
validation) to generate one internally but just in interactively
session since the translation must use GUI features other wise
use the COLOR-NAME directly as COLOR without type validation so
that the usage must be carefully and we always suggested
specified the COLOR argument.

Commonly FACE just has an :foreground attribute set to COLOR, but
you can specified more attribute as what `set-face-attribute' did
through specifying OTHER-FACEATTRS.
"
  (let* ((face-sym
          (intern
           (format "entropy/emacs-defface-simple-color-face-%s"
                   color-name)))
         (color
          (or color
              (and noninteractive color-name)
              (apply 'color-rgb-to-hex
                     (color-name-to-rgb
                      color-name))))
         form)
    (setq
     form
     `(defface ,face-sym
        '((t :foreground ,color
             ,@other-faceattrs))
        ,(if other-faceattrs
             (format "eemacs's common color face for color '%s' (%s).

With face attribtues:

'%s'"
                     color-name color other-faceattrs)
           (format "eemacs's common color face for color '%s' (%s)."
                   color-name color))
        :group 'entropy/emacs-defface-simple-color-face-group))
    (eval form)
    ;; return the face symbol
    face-sym))

(dolist (color-spec
         '((:color-name "red"           :color "red")
           (:color-name "red-bold"      :color "red"    :attrs (:bold t))
           (:color-name "red-italic"    :color "red"    :attrs (:slant italic))
           (:color-name "green"         :color "green")
           (:color-name "green-bold"    :color "green"  :attrs (:bold t))
           (:color-name "green-italic"  :color "green"  :attrs (:slant italic))
           (:color-name "yellow"        :color "yellow")
           (:color-name "yellow-bold"   :color "yellow" :attrs (:bold t))
           (:color-name "yellow-italic" :color "yellow" :attrs (:slant italic))
           (:color-name "blue"          :color "blue")
           (:color-name "blue-bold"     :color "blue"   :attrs (:bold t))
           (:color-name "blue-italic"   :color "blue"   :attrs (:slant italic))
           (:color-name "cyan"          :color "cyan")
           (:color-name "cyan-bold"     :color "cyan"   :attrs (:bold t))
           (:color-name "cyan-italic"   :color "cyan"   :attrs (:slant italic))))
  (apply
   'entropy/emacs-defface-gen-simple-color-face
   (plist-get color-spec :color-name)
   (plist-get color-spec :color)
   (plist-get color-spec :attrs)))

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
  '((((class color) (background dark))
     :foreground "grey" :weight bold)
    (((class color) (background light))
     :foreground "grey18" :weight bold))
  "Face for hydra head 'grey'"
  :group 'entropy/emacs-defface-group)

;; * provide
(provide  'entropy-emacs-defface)
