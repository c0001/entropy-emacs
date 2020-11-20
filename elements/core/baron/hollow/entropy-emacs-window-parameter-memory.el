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
;; The cookie framework for save or restgore current window
;; configurationwindow parameters.
;;
;; * Configuration:
;;
;; eemacs used only
;;
;; * Code:

(defvar entropy/emacs-wpamemory--memory-cookies nil
  "A list of eemacs-wpamemory-regist-obj

 *Description of 'eemacs-wpamemory-regist-obj':*

 This is an object consists of two part, i.e. an type symbol TYPE
 and a Plist.

 TYPE is an indicator specified whom regist this.

 Plist has a feature key specified by the TYPE known as a
 'wpam-type-feature-key' and one register key :register in where the
 TYPE saved the window parameters of current window-list.

 The value of the key :register is a list of plist known as
 'wpam-item' which has two key i.e. :buff (the buffer on that
 window) and :win-pms (the parameters of that window).

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

 Each TYPE can regist the memory into here multi-times, because
 each TYPE can have various feature represented by the
 'wpamemory-type-feature-key', but each feature value of a
 TYPE just can stored once in here which means that each type with
 the same feature keypair can not be duplicated, if so, the new one
 replaced the old one.")

(defvar entropy/emacs-wpamemory-ignored-window-parameters
  '(eemacs-current-window
    internal-region-overlay)
  "Window parameters do not memorize

Those window parameter restoration causing some mistakes or just
user specified.")

(defun entropy/emacs-wpamemory-pruning-memory (memory-type &optional feature-keypair)
  (let ((cur-cache entropy/emacs-wpamemory--memory-cookies))
    (when cur-cache
      (cond ((eq memory-type 'common)
             (mapc (lambda (obj)
                     (when (equal (car obj) 'common)
                       (setq entropy/emacs-wpamemory--memory-cookies
                             (delete obj
                                     entropy/emacs-wpamemory--memory-cookies))))
                   cur-cache))
            (feature-keypair
             (dolist (obj cur-cache)
               (when (eq (car obj) 'eyebrowse)
                 (when (equal  (plist-get (cdr obj) (car feature-keypair))
                               (cdr feature-keypair))
                   (setq entropy/emacs-wpamemory--memory-cookies
                         (delete
                          obj
                          entropy/emacs-wpamemory--memory-cookies))))))))))

(defun entropy/emacs-wpamemory-get-memory (memory-type feature-keypair &optional pop-it)
  (let* ((cache entropy/emacs-wpamemory--memory-cookies)
         rtn)
    (setq rtn
          (catch :exit
            (cond (feature-keypair
                   (dolist (obj cache)
                     (when (eq (car obj) memory-type)
                       (when (equal (plist-get (cdr obj) (car feature-keypair))
                                    (cdr feature-keypair))
                         (throw :exit obj)))))
                  (t
                   (assoc 'common cache)))))
    (when (and rtn pop-it)
      (entropy/emacs-wpamemory-pruning-memory
       memory-type feature-keypair))
    rtn))

(defun entropy/emacs-wpamemory-regist-memory (memory-type &optional feature-keypair)
  (let* ((win-list (window-list))
         (cur-cache entropy/emacs-wpamemory--memory-cookies)
         cache)
    (dolist (win win-list)
      (let* ((buff (window-buffer win))
             (win-pms (window-parameters win)))
        ;; don't memorize some special keys
        (dolist (item entropy/emacs-wpamemory-ignored-window-parameters)
          (setq win-pms
                (delete (assoc item win-pms)
                        win-pms)))
        (unless (or (with-current-buffer buff
                      (if (not (minibufferp buff))
                          nil
                        t)))
          (push (list :buff buff :win-pms win-pms)
                cache))))
    (setq entropy/emacs-wpamemory--memory-cookies
          (append
           (list
            (cond (feature-keypair
                   (let ((key (car feature-keypair))
                         (var (cdr feature-keypair)))
                     (entropy/emacs-wpamemory-pruning-memory memory-type feature-keypair)
                     (append
                      (list memory-type key var)
                      (list :register cache))))
                  (t (entropy/emacs-wpamemory-pruning-memory 'common)
                     (list 'common :register cache))))
           entropy/emacs-wpamemory--memory-cookies))))

(defun entropy/emacs-wpamemory-restore-memory (memory-type &optional feature-keypair)
  (let* ((cache entropy/emacs-wpamemory--memory-cookies)
         (res-obj (plist-get
                   (cdr (entropy/emacs-wpamemory-get-memory
                         memory-type feature-keypair 'pop-it))
                   :register)))
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

(defun entropy/emacs-wpamemory-default-register (&optional orig-func &rest orig-args)
  (entropy/emacs-wpamemory-regist-memory 'common)
  (when (functionp orig-func)
    (apply orig-func orig-args)))

(defun entropy/emacs-wpamemory-default-restore (&optional orig-func &rest orig-args)
  (let ((rtn (when (functionp orig-func)
               (apply orig-func orig-args))))
    (entropy/emacs-wpamemory-restore-memory 'common)
    rtn))

(advice-add 'display-buffer :around #'entropy/emacs-wpamemory-default-register)
(advice-add 'delete-window :around #'entropy/emacs-wpamemory-default-restore)



;; * provide
(provide 'entropy-emacs-window-parameter-memory)
