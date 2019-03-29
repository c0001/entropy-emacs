;;; init-library.el --- general library config for sub-init files

;; Copyright (C) 20181223  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           nil
;; Package-Version: nil
;; Version:       nil
;; Created:       2018-12-23 11:42:28
;; Keywords:      nil
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

;;; Code:

(require 'font-lock+)

(use-package all-the-icons
  :commands
  (all-the-icons-icon-for-file
   all-the-icons-icon-for-mode
   all-the-icons--icon-info-for-buffer
   all-the-icons-install-fonts
   all-the-icons-insert))

(use-package eldoc-eval
  :preface (defvar eldoc-in-minibuffer-mode nil)
  :commands (eldoc-in-minibuffer-mode
             eldoc-eval-expression)
  :init (setq eldoc-eval-preferred-function 'eval-expression))

(use-package shrink-path
  :commands
  (shrink-path--dirs-internal
   shrink-path--truncate
   shrink-path-dirs
   shrink-path-expand
   shrink-path-file
   shrink-path-file-expand
   shrink-path-file-mixed
   shrink-path-prompt))



(provide 'entropy-emacs-library)
