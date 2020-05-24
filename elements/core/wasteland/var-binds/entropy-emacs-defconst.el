;; init-const.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Constants.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * Code:
;; ** require
(require 'entropy-emacs-defcustom)

;; ** system type
;; *** basic conditions
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

;; *** group filter
(defconst sys/is-win-group
  (or sys/win32p sys/cygwinp)
  "System type group filter for win32 platform.

Include all windows posix emulator environment, as 'Cygwin' or
'Msys2'.")

(defconst sys/is-posix-compatible
  (or sys/linux-x-p sys/linuxp sys/mac-x-p sys/macp)
  "System type group filter for posix comptible system
platforms even for posix emulators.

Include 'GNU/Linux', 'Darwin' or any 'UNIX' distribution.

Posix emulator supports for:

- 'Cygwin'
- 'Msys' or 'Msys2'")

(defconst sys/is-graphic-support
  (or (or sys/is-win-group sys/linux-x-p sys/mac-x-p) ;FIXME: get DESKTOP_SESSION like env var on MACOS
      (not (string-empty-p (getenv "DESKTOP_SESSION")))
      (not (string-empty-p (getenv "XDG_CURRENT_DESKTOP"))))
  "System group filter for graphic supported platforms.")

;; ** others
(defconst entropy/emacs-origin-load-path (copy-tree load-path))

(defconst entropy/emacs-ecv "VERSION: 0.1.0 Tag: ONEPIECE"
  "Version of entropy-emacs.")

(defconst entropy/emacs-home-page "https://github.com/c0001/entropy-emacs"
  "The =entropy-emacs= project home page uri.")

;; * provide
(provide 'entropy-emacs-defconst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
