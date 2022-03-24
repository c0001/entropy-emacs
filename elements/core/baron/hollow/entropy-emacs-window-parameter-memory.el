;;; entropy-emacs-window-parameter-memory.el --- A cookie for emacs window conifgurations
;;
;; * Copyright (C) 20201121  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           nul
;; Package-Version: null
;; Version:       null
;; Created:       2020-11-21 02:42:54
;; Keywords:      window-parameters, window-configuration, entropy-emacs,
;; Compatibility: GNU Emacs 26.1;
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
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
;; The cookie framework for save or restgore current `window-list''s
;; window parameters based on buffer of each window of thus.
;;
;; * Configuration:
;;
;; eemacs used only
;;
;; * Code:

(defvar entropy/emacs-wpamemory--memory-cookies nil
  "A list of eemacs-wpamemory-regist-obj

*Description of 'eemacs-wpamemory-regist-obj':*

This is an object consists of three part, i.e. an type symbol
MEMORY-TYPE , an =eemacs-type-spec= MEMORY-TYPE-ROLE and an
MEMORY-PLIST to host what is save.

MEMORY-TYPE is an indicator specified whom regist this.

MEMORY-PLIST has one register key :register in where the
MEMORY-TYPE saved the window parameters of current window-list.

The value of the key :register is a list of plist known as
'wpam-item' which has two key i.e. =:buff= (the buffer on that
window) and =:win-pms= (the parameters of that window getted from
`window-parameters').

eemacs-wpam (abbreviation of this
=entropy-emacs-window-parameter-memory.el=) treat each memory
stick on buffers not on those windows becasue of that buffer is
constant of an 'window-configuration' but windows can be re-built
when recovery thus, In the other hand, the memory mechnism of
focusing on buffer can reduce the restoring conflicts in which
case if a buffer is not displayed any more, we can ignore it's
window parameter memory so that do not make side-effects on the
new buffer displayed in that window which generated by other
operations.

Each MEMORY-TYPE can regist the memory into here multi-times,
because each MEMORY-TYPE can have various feature represented by
the MEMORY-TYPE-ROLE, but each MEMORY-TYPE-ROLE just can stored
once in here which means that each type with the same role can
not be duplicated, if so the new one replace the old one.")

(defvar entropy/emacs-wpamemory-ignored-window-parameters
  '(eemacs-current-window
    internal-region-overlay)
  "Window parameters do not memorize

Those window parameter restoration causing some mistakes or just
user specified.")

(defvar entropy/emacs-wpamemory-default-type-role '(EEMACS-DT-IDENTITY . nil))

(defun entropy/emacs-wpamemory-pruning-memory
    (memory-type &optional memory-type-role)
  "Prunning the =eemacs-wpamemory-regist-obj= with MEMORY-TYPE of
its role as MEMORY-TYPE-ROLE from
`entropy/emacs-wpamemory--memory-cookies', use
`entropy/emacs-wpamemory-default-type-role' while the role is not
specified."
  (let ((cur-cache entropy/emacs-wpamemory--memory-cookies)
        (memory-type-role
         (or memory-type-role
             entropy/emacs-wpamemory-default-type-role)))
    (when cur-cache
      (dolist (obj cur-cache)
        (when (eq (car obj) memory-type)
          (when (equal (entropy/emacs-type-spec-eval
                        (cadr obj))
                       (entropy/emacs-type-spec-eval
                        memory-type-role))
            (setq entropy/emacs-wpamemory--memory-cookies
                  (delete
                   obj
                   entropy/emacs-wpamemory--memory-cookies))))))))

(defun entropy/emacs-wpamemory-get-memory
    (memory-type memory-type-role &optional pop-it)
    "Get the =eemacs-wpamemory-regist-obj= with MEMORY-TYPE
of its role as MEMORY-TYPE-ROLE from
`entropy/emacs-wpamemory--memory-cookies'"
  (let* ((cache entropy/emacs-wpamemory--memory-cookies)
         (memory-type-role
          (or memory-type-role
              entropy/emacs-wpamemory-default-type-role))
         rtn)
    (setq rtn
          (catch :exit
            (dolist (obj cache)
              (when (eq (car obj) memory-type)
                (when (equal (entropy/emacs-type-spec-eval
                              (cadr obj))
                             (entropy/emacs-type-spec-eval
                              memory-type-role))
                  (throw :exit obj))))))
    (when (and rtn pop-it)
      (entropy/emacs-wpamemory-pruning-memory
       memory-type memory-type-role))
    rtn))

(defun entropy/emacs-wpamemory-regist-memory
    (memory-type &optional memory-type-role pm-pair-filter)
  "Regist an =eemacs-wpamemory-regist-obj= to
`entropy/emacs-wpamemory--memory-cookies' by MEMORY-TYPE of role
as MEMORY-TYPE-ROLE, filter `window-parameters' keypair with
function PM-PAIR-FILTER which accept the item of list of thus and
return non-nil indicating to use the parameter, null for
otherwise.

No item regist when there's no valid window parameter key got.

Use `entropy/emacs-wpamemory-default-type-role' while the role is
not specified.
"
  (let* ((win-list (window-list))
         (memory-type-role
          (or memory-type-role entropy/emacs-wpamemory-default-type-role))
         cache)
    (dolist (win win-list)
      (let* ((buff (window-buffer win))
             (win-pms (window-parameters win))
             ;; whether need to regist the cookie
             (need-to-regist t))
        ;; don't memorize some special keys
        (dolist (item entropy/emacs-wpamemory-ignored-window-parameters)
          (setq win-pms
                (delete (assoc item win-pms)
                        win-pms)))
        (if (null win-pms)
            (setq need-to-regist nil))

        ;; user spec window parameter filter
        (when (and pm-pair-filter (functionp pm-pair-filter) need-to-regist)
          (let ((spec-inc-func pm-pair-filter)
                inc-tempvar)
            (dolist (pm-pair win-pms)
              (when (apply spec-inc-func pm-pair)
                (push pm-pair inc-tempvar)))
              (setq win-pms inc-tempvar)))
        (if (null win-pms)
            (setq need-to-regist nil))

        ;; window filter
        (unless (or
                 (null need-to-regist)
                 (and
                  (with-current-buffer buff
                    (if (not (minibufferp buff))
                        nil
                      t))
                  (prog1 t (setq need-to-regist nil))))
          (push (list :buff buff :win-pms win-pms)
                cache))))

    ;; get the cookie
    (when cache
      (setq entropy/emacs-wpamemory--memory-cookies
            (append
             (list
              (let (_)
                (entropy/emacs-wpamemory-pruning-memory
                 memory-type memory-type-role)
                (append
                 (list memory-type memory-type-role)
                 (list :register cache))))
             entropy/emacs-wpamemory--memory-cookies)))))

(defun entropy/emacs-wpamemory-restore-memory
    (memory-type &optional memory-type-role)
    "Resotre an =eemacs-wpamemory-regist-obj= from
`entropy/emacs-wpamemory--memory-cookies' by MEMORY-TYPE of role
as MEMORY-TYPE-ROLE if available, and prunning the one in thus.

Use `entropy/emacs-wpamemory-default-type-role' while the role is
not specified."
  (let* ((cache entropy/emacs-wpamemory--memory-cookies)
         (res-obj (entropy/emacs-get-plist-form
                   (entropy/emacs-wpamemory-get-memory
                    memory-type memory-type-role 'pop-it)
                   :register
                   'car t)))
    (when res-obj
      (dolist (obj res-obj)
        (let* ((buff (plist-get obj :buff))
               (buff-win (get-buffer-window buff))
               (buff-lp (ignore-errors
                          (and (buffer-live-p buff)
                               (window-live-p
                                buff-win))))
               (win-pms (plist-get obj :win-pms)))
          (when buff-lp
            (dolist (pm win-pms)
              (set-window-parameter
               buff-win
               (car pm) (cdr pm)))))))))

;; * provide
(provide 'entropy-emacs-window-parameter-memory)
