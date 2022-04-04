;;; entropy-emacs-python.el --- entropy-emacs python development configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           url
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; Python mode configuration for `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require

;; ** main
;; Python Mode
(use-package python
  :ensure nil
  :eemacs-mmphc
  (((:enable t :defer (:data (:adfors (python-mode-hook) :adtype hook :pdumper-no-end t)))
    (python-mode (python python-mode-map) t (2 1)))
   ("Basic"
    nil
    "Repl"
    (("C-c C-p" run-python "Run an inferior Python process"
      :enable t :exit t :map-inject t))
    "Eval"
    (("C-c C-e" python-shell-send-statement
      "Send the statement at point to inferior Python process"
      :enable t :exit t :map-inject t)
     ("C-x C-e" python-shell-send-defun
      "Send the statement at point to inferior Python process"
      :enable t :exit t :map-inject t)
     ("C-c C-r" python-shell-send-region
      "Send the region to inferior Python process"
      :enable t :exit t :map-inject t))
    "IDE"
    nil))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(provide 'entropy-emacs-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
