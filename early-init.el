;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2010-2021 Entropy

;; Author: Entropy <bmsac0001@gmail.com>

;; This file is not part of GNU Emacs.
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

;;; Commentary:
;; Suggested by : https://github.com/seagle0128/.emacs.d
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:
;;;; early defvar

;; top eemacs host
(defconst entropy/emacs-user-emacs-directory
  (or (bound-and-true-p entropy/emacs-user-emacs-directory)
      (file-name-directory load-file-name))
  "Top eemacs host directory replaced of `user-emacs-directory'
for preventing unregular loading procedure by modification of
emacs upstream")

;; We should sync `user-emacs-directory' with eemacs specification so
;; that all emacs internal behaviour is compatible with eemacs
(setq user-emacs-directory entropy/emacs-user-emacs-directory)

(defconst entropy/emacs-stuffs-topdir
  (expand-file-name "stuffs" entropy/emacs-user-emacs-directory)
  "The stuffs collection host path, for as `savehist-file',
`bookmark-file' cache host. This setting mainly for cleanup
`entropy/emacs-user-emacs-directory'.")
(defconst entropy/emacs-eln-cache-directory
  (expand-file-name "emacs/emacs-eln-cache" entropy/emacs-stuffs-topdir))

(defun entropy/emacs-native-comp-eln-load-path-set (&optional reset)
  ;; redirect `native-comp-eln-load-path' to store elns to alternative path
  (when (boundp 'native-comp-eln-load-path)
    ;; emacs-29 and above support redirect internally so we do not
    ;; need to hack
    (if (fboundp 'startup-redirect-eln-cache)
        (unless (equal (car native-comp-eln-load-path)
                       entropy/emacs-eln-cache-directory)
          (startup-redirect-eln-cache entropy/emacs-eln-cache-directory))
      (let (tmp)
        (if reset
            (if (not (member entropy/emacs-eln-cache-directory native-comp-eln-load-path))
                (push entropy/emacs-eln-cache-directory native-comp-eln-load-path)
              (dolist (el native-comp-eln-load-path)
                (if (not (equal el entropy/emacs-eln-cache-directory))
                    (push el tmp)))
              (setq native-comp-eln-load-path (nreverse tmp))
              (push entropy/emacs-eln-cache-directory native-comp-eln-load-path))
          ;; emacs-28's eln-cache directory is hardcoded in source file and
          ;; must be the first eln load path for smallest primitive
          ;; initialization
          (setq native-comp-eln-load-path
                (cons (car native-comp-eln-load-path)
                      (cons entropy/emacs-eln-cache-directory
                            (cdr native-comp-eln-load-path)))))))))
(entropy/emacs-native-comp-eln-load-path-set)

;;;; Basic
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Inhibit dialog box for entropy-emacs initialization process to
;; prevent judgment interaction missing upon emacs-28.
(setq use-dialog-box nil)

;; Enlarge log max for log full startup verbose diagnostics
(setq message-log-max 3000)

;; Disable `help-mode' auto load library for its doc render request
;; since we needed pure charge of how packages are loading for.
(setq help-enable-autoload nil
      help-enable-completion-autoload nil
      help-enable-symbol-autoload nil
      )

;;;; Process performance tweaking

(setq read-process-output-max (* 1024 1024))
;; Disable `process-adaptive-read-buffering' for *nix platform for
;; enlarge process output read performance in emacs (especially for
;; eshell) according to:
;;
;; - https://emacs-china.org/t/emacs/21053/13
;; - https://www.reddit.com/r/emacs/comments/usghki/living_the_eshell_dream_a_reduction_in_latency/
;; - https://www.reddit.com/r/emacs/comments/usghki/comment/i9sehrv/?utm_source=share&utm_medium=web2x&context=3
;;
(when (memq system-type '(gnu gnu/linux darwin))
  (setq process-adaptive-read-buffering nil))

;;;; Font lock mode config

;; ;; Globally downgrade font-lock decoration to improve performance
;; (setq font-lock-maximum-decoration
;;       '((t . 2)))

;; Optimize jit-lock-mode default configuration
(setq jit-lock-defer-time
      ;; FIXME: emacs upper than 28 seems always defer jit-lock? and 0
      ;; may cause font-lock not flush while idle?
      (if (< emacs-major-version 28) 0 nil)

      ;; jit-lock-stealth-time 2
      ;; jit-lock-chunk-size 100
      ;; jit-lock-stealth-load 50
      ;; jit-lock-stealth-nice 3
      ;; jit-lock-contextually 'syntax-driven
      ;; jit-lock-context-time 0.5
      ;; jit-lock-antiblink-grace 2
      )

;; inhibit fontlock render while fast hints
(cond ((version< emacs-version "28")
       (setq fast-but-imprecise-scrolling t))
      (t
       (setq redisplay-skip-fontification-on-input t)))


;; inhibit mouse highlight while typing
;;
;; inspired by :
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Avoidance.html
(setq make-pointer-invisible t
      mouse-highlight 1)

;;;; FIXME Disable tramp archive (gvfs) handler

;; EEMACS_MAINTENANCE: Refer to eemacs bug =h:02882923-4531-4775-9ae4-27c809f90f6e=

;; FIXME: we need to do set it after load `tramp-archive' or may cause
;; the invalid file-name-handler error in emacs-28 and why?
;;
;; DONE [20220921]: this bug has been fix in emacs-28.2
(setq tramp-archive-enabled nil)

;;;; Fix pgtk frame visible/invisible performance issue
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58556

(setq pgtk-wait-for-event-timeout 0
      pgtk-selection-timeout 0)

;;; early-init.el loaded done indicator

(defvar entropy/emacs-early-init-done t
  "Indicator for showing up that eemacs has loaded its \"early-init.el\".

This variable exists since emacs just load that file in normal startup
procedure, thus it has no effects on batch mode. Thus we manually load
it in \"init.el\" when this variable is not detected for non-nil.

When the value is `t', then indicating that \"early-init.el\" is loaded
by emacs automatically, otherwise as `manually' then indicating that
\"early-init.el\" is loaded by \"init.el\" context in which case emacs not
load it automatically.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
