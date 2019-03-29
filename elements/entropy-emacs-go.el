;;; init-go.el --- entropy-emacs main config for go language

;; Copyright (C) 20181217  entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       none
;; Created:       2018-12-17 15:30:11
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs 26.1;
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
;; none
;;
;;; Configuration:
;;
;; none

;; * Code:

(use-package go-mode
  :commands (go-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; * provide
(provide 'entropy-emacs-go)
