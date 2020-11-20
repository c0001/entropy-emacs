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

(defvar entropy/emacs-wpamemory--memory-cookies nil)

(defun entropy/emacs-wpamemory-pruning-memory (memory-type &optional feature-keypair)
  (let ((cur-cache entropy/emacs-wpamemory--memory-cookies))
    (when cur-cache
      (cond ((eq memory-type 'common)
             (mapc (lambda (obj)
                     (when (eq (car obj) 'common)
                       (delete obj entropy/emacs-wpamemory--memory-cookies)))
                   entropy/emacs-wpamemory--memory-cookies))
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
      (setq entropy/emacs-wpamemory--memory-cookies
            (delete rtn entropy/emacs-wpamemory--memory-cookies)))
    rtn))

(defun entropy/emacs-wpamemory-regist-memory (memory-type &optional feature-keypair)
  (let* ((win-list (window-list))
         (cur-cache entropy/emacs-wpamemory--memory-cookies)
         cache)
    (dolist (win win-list)
      (let* ((buff (window-buffer win))
             (win-pms (window-parameters win)))
        (setq win-pms
              (delete (assoc 'internal-region-overlay win-pms)
                      win-pms))
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
                  (t (list 'common :register cache))))
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

;; * provide
(provide 'entropy-emacs-window-parameter-memory)
