;;; entropy-emacs-path.el --- entropy-emacs path setting
;;
;; * Copyright (C) 20190603  Enropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-path.el
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
;; `entroy-emacs' intergrated sets of shell command tool as extension
;; component for server base, communication transform etc. Emacs has
;; its owns path management as like one does of one operation system,
;; thus the path config for emacs was the aim of this configuration.
;;
;; emacs run based on system 'shell path' which all copied it into
;; emacs =env= and can be appending for other specified ones through
;; the way of using function `setenv'. Further more, emacs has its
;; other path configuration indicated  by `exec-path' for its
;; subprogress feature.
;;
;; `entropy-emacs' given various external tools enabling and path
;; sepcific customizalble variable for do both for 'shell-path' and
;; 'exec-pah', they are ordered by especially form for preventing
;; coverring type.
;;
;; In those path setting, win32 specific ones are treated specially
;; in `entropy-emacs' of using Msys2 (https://www.msys2.org/) as the
;; *NIX posix emulator, see those customizable variables doc-string
;; for detailes in group `entropy/emacs-win'.
;;
;; * Configuration:
;;
;; Loaing by `entropy-emacs' automaticaly without hacking warranty. 
;; 
;; * Code:
;; 
;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)

;; ** shell path
(when sys/win32p
;; *** emacs bin folder
  (if entropy/emacs-win-emacs-bin-path-add
      (setenv "PATH" (concat invocation-directory ";" (getenv "path"))))
;; *** wsl-apps
  (if entropy/emacs-wsl-enable
      (setenv "PATH" (concat entropy/emacs-wsl-apps ";" (getenv "PATH"))))

;; *** fakecygpty
  (if entropy/emacs-win-fakecygpty-enable
      (setenv "PATH" (concat entropy/emacs-win-fakecygpty-path ";" (getenv "PATH"))))
  
;; *** gcc for win 
  (if entropy/emacs-win-portable-mingw-enable
      (setenv "PATH" (concat entropy/emacs-win-portable-mingw-path ";" (getenv "PATH"))))  
;; *** git-portable
  (if entropy/emacs-git-portable
      (setenv "PATH" (concat entropy/emacs-git-portable-path ";" (getenv "PATH"))))
;; *** windows texlive
  (if entropy/emacs-win-portable-texlive-enable (setenv "PATH" (concat entropy/emacs-win-portable-texlive-path ";" (getenv "PATH"))))

;; *** windows php
  (if entropy/emacs-win-portable-php-enable (setenv "PATH" (concat entropy/emacs-win-portable-php-path ";" (getenv "PATH"))))
;; *** windows python about
;; **** windows pip-path setting
  (if entropy/emacs-win-portable-pip-enable (setenv "PATH" (concat entropy/emacs-win-portable-pip-path ";" (getenv "PATH"))))
;; **** windows python-path setting
  (if entropy/emacs-win-portable-python-enable (setenv "PATH" (concat entropy/emacs-win-portable-python-path ";" (getenv "PATH"))))
;; *** windows grep path setting
  (if entropy/emacs-win-portable-grep-enable (setenv "PATH" (concat entropy/emacs-win-portable-grep-path ";" (getenv "PATH"))))
;; *** windows ag-path setting
  (if entropy/emacs-win-portable-ag-enable (setenv "PATH" (concat (getenv "PATH") ";" entropy/emacs-win-portable-ag-path)))
;; *** windows rg-path setting
  (if entropy/emacs-win-portable-rg-enable (setenv "PATH" (concat entropy/emacs-win-portable-rg-path ";" (getenv "PATH"))))
;; *** windows pt-path setting
  (if entropy/emacs-win-portable-pt-enable (setenv "PATH" (concat entropy/emacs-win-portable-pt-path ";" (getenv "PATH"))))
;; *** windows nodejs-path setting
  (if entropy/emacs-win-portable-nodejs-enable (setenv "PATH" (concat entropy/emacs-win-portable-nodejs-path ";" (getenv "PATH"))))
;; *** windows-opencc
  (if entropy/emacs-win-portable-opencc-enable (setenv "PATH" (concat entropy/emacs-win-portable-opencc-path ";" (getenv "PATH"))))
;; *** windows-pandoc
  (if entropy/emacs-win-portable-pandoc-enable (setenv "PATH" (concat entropy/emacs-win-portable-pandoc-path ";" (getenv "PATH"))))
;; *** windows-portable-jdk
  (if entropy/emacs-win-portable-jdk-enable (setenv "PATH" (concat entropy/emacs-win-portable-jdk-path ";" (getenv "PATH"))))
;; *** windows-zeal
  (if entropy/emacs-win-portable-zeal-enable (setenv "PATH" (concat entropy/emacs-win-portable-zeal-path ";" (getenv "PATH"))))
;; *** windows portable putty
  (if entropy/emacs-win-portable-putty-enable
      (setenv "PATH" (concat entropy/emacs-win-portable-putty-path
                             ";"
                             (getenv "PATH")))))


;; ** exec path
(when sys/win32p
;; *** emacs bin folder
  (if entropy/emacs-win-emacs-bin-path-add
      (add-to-ordered-list 'exec-path invocation-directory 19))
  
;; *** wsl exec path setting
  (if entropy/emacs-wsl-enable
      (progn
        (add-to-ordered-list 'exec-path entropy/emacs-wsl-apps 12)
        (when entropy/emacs-wsl-enable-extra
          (add-to-ordered-list 'exec-path (concat entropy/emacs-wsl-apps-extra "usr/bin/") 18)
          (setq woman-manpath
                `(,(concat entropy/emacs-wsl-apps-extra "usr/man")
                  ,(concat entropy/emacs-wsl-apps-extra "usr/share/man")
                  ,(concat entropy/emacs-wsl-apps-extra "usr/local/man"))))))

;; *** fakecygpty for windows term and ansi-term
  (if entropy/emacs-win-fakecygpty-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-fakecygpty-path 21))
  
;; *** clang for company-clang
  (if entropy/emacs-win-portable-clang-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-clang-path 15))
  
;; *** gcc for win exec path setting
  (if entropy/emacs-win-portable-mingw-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-mingw-path 10))
  
;; **** windows git-portable exec path setting
  (if entropy/emacs-git-portable
      (add-to-ordered-list 'exec-path entropy/emacs-git-portable-path 9))
;; *** windows texlive path setting
  (if entropy/emacs-win-portable-texlive-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-texlive-path 8))

;; *** windows php
  (if entropy/emacs-win-portable-php-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-php-path 7))
;; *** windows python about
;; **** windows pip exec path setting
  (if entropy/emacs-win-portable-pip-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-pip-path 6))
;; **** windows python exec path setting
  (if entropy/emacs-win-portable-python-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-python-path 5))
  
;; *** windows grep exec path setting
  (if entropy/emacs-win-portable-grep-enable
      (add-to-ordered-list 'exec entropy/emacs-win-portable-grep-path 4))
;; *** windnows ag exec path setting
  (if entropy/emacs-win-portable-ag-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-ag-path 11))
;; *** windows rg exec path setting
  (if entropy/emacs-win-portable-rg-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-rg-path 3))
;; *** windows pt exec path setting
  (if entropy/emacs-win-portable-pt-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-pt-path 2))
;; *** windows nodejs exec path setting
  (if entropy/emacs-win-portable-nodejs-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-nodejs-path 1))

;; *** windows opencc exec path setting
  (if entropy/emacs-win-portable-opencc-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-opencc-path 13))

;; *** windows pandoc exec path setting
  (if entropy/emacs-win-portable-pandoc-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-pandoc-path 14))

;; *** windows portable jdk exec path setting
  (if entropy/emacs-win-portable-jdk-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-jdk-path 16))

;; *** windows portable zeal path setting
  (if entropy/emacs-win-portable-zeal-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-zeal-path 17))
;; *** windows portable putty path setting
  (if entropy/emacs-win-portable-putty-enable
      (add-to-ordered-list 'exec-path entropy/emacs-win-portable-putty-path 20)))

;; * provide
(provide 'entropy-emacs-path)
