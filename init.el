;;; init.el --- entropy-emacs initiliaze raw

;; Copyright (C) 2019-03-29  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; commentary
;;
;;; Configuration:
;;
;; configuration

;; * Code:

;; ** Warning of startup


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(when (version< emacs-version "24.4")
  (error "This requires Emacs 24.4 and above!"))

;; ** Optimize gc performance and loading speed:
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Gc hook for minibuffer (using high gc threshold for ivy)
(defun entropy/minibuffer-attend-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun entropy/minibuffer-quit-hook ()
  (if (eq system-type 'windows-nt)
      (setq gc-cons-threshold 80000000)
    (cond ((not (display-graphic-p))
           (setq gc-cons-threshold 800000))
          ((display-graphic-p)
           (setq gc-cons-threshold 30000000)))))
(add-hook 'minibuffer-setup-hook #'entropy/minibuffer-attend-hook)
(add-hook 'minibuffer-exit-hook #'entropy/minibuffer-quit-hook)

(setq garbage-collection-messages nil)
(setq gc-cons-threshold 100000000)
(setq gc-cons-percentage 0.1)

;; Setting gc space limit to smaller one for fasting init time.
;;    NOTE: this will cause frequency gc because of the low limit of gc value. Thus this will cause
;;    other lagging experience like counsel-M-x scrolling down with it's candidates.
(add-hook 'emacs-startup-hook #'entropy/minibuffer-quit-hook)

;; ** load path
;; *** load-path for entropy-emacs
(let ((entropy-emacs_path
       (expand-file-name "elements/"
                         user-emacs-directory)))
  (add-to-list 'load-path entropy-emacs_path))

;; ** startup entropy-emacs
(require 'entropy-emacs)

