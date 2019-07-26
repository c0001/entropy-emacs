;;; entropy-emacs-custom.el ---  entropy-emacs user specific loading
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-custom.el
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
;; `entropy-emacs' using specific user customized file for allowing
;; more complicated usage cases, but for preserve the emacs internal
;; customized mechanism i.e. the 'custom.el' file. 
;;
;; There's two file used for above session:
;;
;; 1) `entropy/emacs-custom-common-file':
;;
;;    The same setting as `custom-file'.
;;
;; 2) `entropy/emacs-custom-navigate-file'
;;
;;    Loading further more user custom load files.
;;
;;
;; The origin customized file `custom-file' used by emacs internal
;; will oftenly inject some nested variable setting or other
;; temperaly snippets, thus for that,
;; `entropy/emacs-custom-navigate-file' was the pretty charged by
;; user-self which do not write any snippets into automatically, as
;; what said that its used for you preferentially.
;;
;; 
;; * Configuration:
;; 
;; Loading by `entropy-emacs' automatically.
;; 
;; * Code:

(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

(let ((cus entropy/emacs-custom-common-file)
      (nav entropy/emacs-custom-navigate-file))
  (if (file-exists-p cus)
      (progn (load cus)
             (setq custom-file cus))
    (when (file-exists-p nav)
      (setq custom-file nav)
      (load nav))))

(provide 'entropy-emacs-custom)
