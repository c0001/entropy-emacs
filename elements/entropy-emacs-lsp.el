;;; entropy-emacs-lsp.el --- entropy-emacs lanugae server protocol configuration
;;
;; * Copyright (C) 201990607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           uhttps://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-lsp.el
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
;; Microsoft Language-server-protocol configuration for
;; `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;; 
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** main
(use-package lsp-mode
  :if (and (>= emacs-major-version 25)
           entropy/emacs-company-lsp)
  :diminish lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil))
  
(use-package lsp-ui
  :if (and (>= emacs-major-version 25)
           entropy/emacs-company-lsp)
  :commands (lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-imenu)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)))

(provide 'entropy-emacs-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
