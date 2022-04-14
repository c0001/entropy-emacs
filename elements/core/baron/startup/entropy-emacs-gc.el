;;; entropy-emacs-gc.el --- Automaitcally garbage-collection configurations for 'entropy-emacs'  -*- lexical-binding: t; -*-
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
;; idle time.
;;
;; * Configuration:
;;
;; Binding tightly for =entropy-emacs=, non-warranty for
;; individual usage.
;;
;; * Code:

(defvar entropy/emacs-gc-records nil)

(defmacro entropy/emacs-gc--with-record (&rest body)
  (declare (indent defun))
  `(let* (--duration--
          (--cur-time-- (current-time))
          (--cur-time-human-- (format-time-string "[%Y-%m-%d %a %H:%M:%S]"))
          (--start-- --cur-time--))

     (when (> (length entropy/emacs-gc-records) 1000)
       (setq entropy/emacs-gc-records nil))

     (prog1
         (progn
           ,@body)
       (setq --duration--
             (float-time
              (time-subtract
               (current-time) --start--)))
       (push (list :stamp --cur-time-human-- :duration --duration--
                   :idle-delay entropy/emacs-garbage-collection-delay)
             entropy/emacs-gc-records))))

(defun entropy/emacs-gc-wrapper (orig-func &rest orig-args)
  (let (_)
    (entropy/emacs-message-simple-progress-message
     (if entropy/emacs-garbage-collection-message-p
         "[gc]: Garbage-collecting")
     (entropy/emacs-gc--with-record
       (apply orig-func orig-args)))))

(advice-add 'garbage-collect
            :around
            #'entropy/emacs-gc-wrapper)

(defmacro  __ya/gc-threshold_setq (symbol value)
  "yet another `setq' but spec for garbage collection referred
variable with newvar set while the VALUE is not equal to the
origin, since each set to the `gc-threshold' or
`gc-cons-percentage' will make gc subrotine analysis(?)"
  `(let ((newval ,value))
     (unless (= ,symbol newval)
       (setq ,symbol newval))))

(defun entropy/emacs-gc--adjust-cons-threshold ()
  (cond (
         ;; -------------------- restrict status --------------------
         ;; condition orderred by the performance sort from low to
         ;; high for preventing the judge performance issue
         (or
          ;; ----------
          ;; ;; company frontend will leak memory
          ;; (bound-and-true-p company-candidates)
          ;; ----------

          ;; we hope all procedure during `eval-expression' are gc
          ;; restricted
          (or (member this-command
                      '(eval-last-sexp
                        eval-region
                        eval-defun
                        eval-expression
                        eval-print-last-sexp
                        eval-buffer
                        )))
          )
         ;; restrict the gc threshold when matching above condidtions
         (__ya/gc-threshold_setq
          gc-cons-threshold
          (* 2 1024 1024)))
        ;; -------------------- prog-mode --------------------
        ((derived-mode-p 'prog-mode)
         (__ya/gc-threshold_setq
          gc-cons-threshold
          (* 100 1024 1024)))
        ;; -------------------- default status --------------------
        (t
         (__ya/gc-threshold_setq
          gc-cons-threshold
          (* 50 1024 1024)))))

(defun entropy/emacs-gc--init-idle-gc (&optional sec)
  (setq entropy/emacs-garbage-collect-idle-timer
        (run-with-idle-timer (if sec sec entropy/emacs-garbage-collection-delay)
                             t #'entropy/emacs-gc--idle-time-recovery)))

(defun entropy/emacs-gc--idle-time-recovery ()
  (garbage-collect)
  (setq gc-cons-threshold
        entropy/emacs-gc-threshold-basic
        gc-cons-percentage
        entropy/emacs-gc-percentage-basic)
  ;; remove duplicate timemr when detected
  (let (duplicate-timerp)
    (dolist (timer timer-idle-list)
      (let ((timer-func (aref timer 5)))
        (when (eq timer-func 'entropy/emacs-gc--idle-time-recovery)
          (push timer duplicate-timerp))))
    (when (and duplicate-timerp
               (> (length duplicate-timerp) 1))
      (dolist (timer duplicate-timerp)
        (cancel-timer timer))
      (entropy/emacs-gc--init-idle-gc))))

(defun entropy/emacs-gc-set-idle-gc (secs)
  "Re-set the garbage collecton timer
`entropy/emacs-garbage-collect-idle-timer' with specific idle
delay seconds SECS."
  (interactive
   (list (let ((read-delay (string-to-number (read-string "Idle delay seconds: "))))
           (if (or (not (numberp read-delay))
                   (<= read-delay 0))
               (error "Input idle delay not valid!")
             read-delay))))
  (when (timerp entropy/emacs-garbage-collect-idle-timer)
    (cancel-timer entropy/emacs-garbage-collect-idle-timer)
    (setq entropy/emacs-garbage-collect-idle-timer nil))
  (setq entropy/emacs-garbage-collection-delay secs)
  (entropy/emacs-gc--init-idle-gc
   entropy/emacs-garbage-collection-delay))


;; --------------------------------------------------
;; after init for eemacs gc init
(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "eemacs-gc-optimization" "eemacs-gc-optimization" prompt-echo
 :pdumper-no-end t
 (setq garbage-collection-messages nil)
 (add-hook 'post-command-hook #'entropy/emacs-gc--adjust-cons-threshold)
 (entropy/emacs-gc--init-idle-gc)
 (setq read-process-output-max (* 1024 1024)))
;; --------------------------------------------------
;; init gc set
(cond ((or entropy/emacs-fall-love-with-pdumper
           (daemonp)
           (not (bound-and-true-p entropy/emacs-custom-enable-lazy-load)))
       ;; restrict gc threshold for pdumper progress and daemon init
       ;; progress even for non lazy init mode.
       (setq gc-cons-threshold (* 2 1024 1024)))
      (t
       ;; enlarge the `gc-cons-threshold' for speedup startup progress
       ;; while normal init mode.

       ;; NOTE: do not use `most-positive-fixnum' here since its may make emacs hang
       (setq gc-cons-threshold (* 100 (expt 1024 2)))))

(provide 'entropy-emacs-gc)
