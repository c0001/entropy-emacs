;;; entropy-emacs-gc.el --- Automaitcally garbage-collection configurations for 'entropy-emacs'
;;
;; * Copyright (C) 20190809  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-gc.el
;; Package-Requires: ((emacs "25") (cl-lib "1.0"))
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
;; The garbage collection mechanism was fragile when emacs session
;; ran with lots of extensions, the memory increasing swiftly while
;; soome doing some riched feature's requesting and looping nested
;; for a huge demands, then the emacs garbage-collection will going
;; up frequently. GC procedure is commonly non-sensitively but only
;; for single occasion, lagging performed while times swiftly gc
;; demands as for some scale data processing.
;;
;; This package injecting the specifics gc functions to hooks with
;; the finger typed-increased `gc-threshold' assignment mechanism for
;; rejecting the gc occurred in typing-time, and actives the 'gc' in
;; idle time and the focus-out scene.
;;
;; * Configuration:
;;
;; Binding tightly for =entropy-emacs=, non-warranty for
;; individual usage. 
;; 
;; * Code:
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defvar)

(defun entropy/emacs-gc--increase-cons-threshold ()
  (setq gc-cons-threshold
        (+ 2000000
           gc-cons-threshold)))

(defun entropy/emacs-gc--idle-time-recovery ()
  (garbage-collect)
  (setq gc-cons-threshold
        entropy/emacs-gc-threshold-basic))

(defun entropy/emacs-gc--init-idle-gc (&optional sec)
  (setq entropy/emacs-garbage-collect-idle-timer
        (run-with-idle-timer (if sec sec entropy/emacs-garbage-collection-delay)
                             t #'entropy/emacs-gc--idle-time-recovery)))

(defun entropy/emacs-gc--focus-in-reset ()
  (entropy/emacs-gc--init-idle-gc entropy/emacs-garbage-collection-delay))

(defun entropy/emacs-gc--focus-out-hook ()
  (garbage-collect)
  (setq gc-cons-threshold entropy/emacs-gc-threshold-basic)
  (garbage-collect)
  (when (timerp entropy/emacs-garbage-collect-idle-timer)
    (cancel-timer entropy/emacs-garbage-collect-idle-timer)
    (setq entropy/emacs-garbage-collect-idle-timer nil)))

(defun entropy/emacs-gc-set-idle-gc (secs)
  "Re-set the garbage collecton timer
`entropy/emacs-garbage-collect-idle-timer' with specific idle
delay seconds SECS."
  (interactive
   (list (let ((read-delay (string-to-number (read-string "Idle delay seconds: "))))
           (if (eq read-delay 0)
               (error "Input idle delay not valid!")
             read-delay))))
  (when (timerp entropy/emacs-garbage-collect-idle-timer)
    (cancel-timer entropy/emacs-garbage-collect-idle-timer)
    (setq entropy/emacs-garbage-collect-idle-timer nil))
  (entropy/emacs-gc--init-idle-gc secs))

(defun entropy/emacs-gc--enter-minibuffer-wmaster ()
  (setq garbage-collection-messages nil))

(defun entropy/emacs-gc--exit-minibuffer-wmaster ()
  (setq garbage-collection-messages entropy/emacs-garbage-collection-message-p)
  (setq gc-cons-threshold entropy/emacs-gc-threshold-basic)
  (garbage-collect))

(entropy/emacs-lazy-with-load-trail
 gc-message
 (setq garbage-collection-messages entropy/emacs-garbage-collection-message-p)
 (add-hook 'minibuffer-setup-hook #'entropy/emacs-gc--enter-minibuffer-wmaster)
 (add-hook 'minibuffer-exit-hook #'entropy/emacs-gc--exit-minibuffer-wmaster)
 (add-hook 'focus-out-hook #'entropy/emacs-gc--focus-out-hook)
 (add-hook 'focus-in-hook #'entropy/emacs-gc--focus-in-reset)
 (add-hook 'post-command-hook #'entropy/emacs-gc--increase-cons-threshold)
 (entropy/emacs-gc--init-idle-gc))

(when entropy/emacs-fall-love-with-pdumper
  ;; upper gc threshold for pdumper procedure
  (setq gc-cons-threshold 50000000))

(provide 'entropy-emacs-gc)
