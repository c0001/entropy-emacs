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

(eval-and-compile
  (defvar entropy/emacs-gc-thread-max
    (if (< emacs-major-version 29) (* 100 (expt 1024 2))
      ;; FIXME: emacs-29's gc inner optimization has collision with the
      ;; traditional enlarging value, thus we attempt to use a tiny
      ;; value approaching to the default one to reduce gc time
      ;; duration. Is this theory right?
      (* 2 (expt 1024 2)))))

(defmacro entropy/emacs-gc--with-record (&rest body)
  (declare (indent defun))
  `(let* (--duration--
          (--cur-time-- (current-time))
          (--cur-time-human-- (format-time-string "[%Y-%m-%d %a %H:%M:%S]"))
          (--start-- --cur-time--))

     (when (> (length entropy/emacs-gc-records) 1000)
       (setq entropy/emacs-gc-records nil))

     (prog1
         ,(entropy/emacs-macroexp-progn body)
       (setq --duration--
             (float-time
              (time-subtract
               (current-time) --start--)))
       (push (list :stamp --cur-time-human-- :duration --duration--
                   :idle-delay entropy/emacs-garbage-collection-delay
                   :gc-cons-threshold gc-cons-threshold
                   :gc-cons-percentage gc-cons-percentage)
             entropy/emacs-gc-records))))

(defmacro __ya/gc-threshold_setq (symbol value)
  "yet another `setq' but spec for garbage collection referred
variable with newvar set while the VALUE is not equal to the
origin, since each set to the `gc-threshold' or
`gc-cons-percentage' will make gc subrotine analysis(?)"
  `(let ((newval ,value))
     (unless (= ,symbol newval)
       (let ((gcval gc-cons-threshold))
         (setq ,symbol newval)
         (when (and garbage-collection-messages
                    (not (= gcval gc-cons-threshold)))
           (message
            "[%s] gc-cons-threshold change from %s to %s"
            this-command gcval gc-cons-threshold))))))

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
          (entropy/emacs-get-symbol-prop this-command 'eemacs-gc-special-cmd-p)
          )
         ;; restrict the gc threshold when matching above condidtions
         (__ya/gc-threshold_setq
          gc-cons-threshold
          entropy/emacs-gc-threshold-basic))
        ;; -------------------- high performance mode --------------------
        (t
         (__ya/gc-threshold_setq
          gc-cons-threshold entropy/emacs-gc-thread-max))))

(defun entropy/emacs-gc--init-idle-gc (&optional sec)
  (entropy/emacs-cancel-timer-var entropy/emacs-garbage-collect-idle-timer)
  (setq entropy/emacs-garbage-collect-idle-timer
        (run-with-idle-timer (if sec sec entropy/emacs-garbage-collection-delay)
                             t #'entropy/emacs-gc--idle-time-recovery)))

(defun entropy/emacs-gc--idle-time-recovery ()
  (setq gc-cons-threshold
        entropy/emacs-gc-threshold-basic
        gc-cons-percentage
        entropy/emacs-gc-percentage-basic)
  (let ((msg "[gc]: Garbage-collecting")
        ;; disable gc message temporarily since we use self spec one
        (garbage-collection-messages nil))
    (entropy/emacs-message-simple-progress-message
     msg
     :with-temp-message t
     :ignore-current-messages (lambda (x) (string-match-p (regexp-quote msg) x))
     (entropy/emacs-gc--with-record (garbage-collect)))))

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
  (setq entropy/emacs-garbage-collection-delay secs)
  (entropy/emacs-gc--init-idle-gc))


;; --------------------------------------------------
;; after init for eemacs gc init
(entropy/emacs-lazy-initial-for-hook
 '(entropy/emacs-after-startup-hook)
 "eemacs-gc-optimization" "eemacs-gc-optimization"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (setq garbage-collection-messages entropy/emacs-garbage-collection-message-p)
 (add-hook 'pre-command-hook #'entropy/emacs-gc--adjust-cons-threshold 100)
 (entropy/emacs-gc--init-idle-gc)
 (setq read-process-output-max (* 1024 1024)))
;; --------------------------------------------------
;; init gc set
(cond ((or entropy/emacs-fall-love-with-pdumper
           (not entropy/emacs-custom-enable-lazy-load)
           (daemonp))
       ;; restrict gc threshold for pdumper progress and daemon init
       ;; progress even for non lazy init mode.
       (setq gc-cons-threshold (* 2 1024 1024)))
      (t
       ;; enlarge the `gc-cons-threshold' for speedup startup progress
       ;; while normal init mode.

       ;; NOTE: do not use `most-positive-fixnum' here since its may make emacs hang
       (let ((emtn-p (= emacs-major-version 29)))
         ;; FIXME: [2022-10-30 Sun 06:11:57] emacs-29.0.50's gc is so
         ;; frequently than 28, so I find a sweet point for thus.
         (setq gc-cons-threshold (* (if emtn-p 50 100) (expt 1024 2)))
         ;; FIXME: [2022-10-30 Sun 06:11:57] emacs-29.0.50's gc
         ;; percentage is suggested to 1.0?
         (if emtn-p (setq gc-cons-percentage 1.0)))))

(provide 'entropy-emacs-gc)
