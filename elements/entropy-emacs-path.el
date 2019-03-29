;;; File name: init-path.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)

;; ** shell path
(when sys/win32p
;; *** emacs bin folder
  (if entropy/win-emacs-bin-path-add
      (setenv "PATH" (concat invocation-directory ";" (getenv "path"))))
;; *** wsl-apps
  (if entropy/wsl-enable
      (setenv "PATH" (concat entropy/wsl-apps ";" (getenv "PATH"))))
;; *** gcc for win 
  (if entropy/win-portable-mingw-enable
      (setenv "PATH" (concat entropy/win-portable-mingw-path ";" (getenv "PATH"))))  
;; *** git-portable
  (if entropy/git-portable
      (setenv "PATH" (concat entropy/git-portable-path ";" (getenv "PATH"))))
;; *** windows texlive
  (if entropy/win-portable-texlive-enable (setenv "PATH" (concat entropy/win-portable-texlive-path ";" (getenv "PATH"))))

;; *** windows php
  (if entropy/win-portable-php-enable (setenv "PATH" (concat entropy/win-portable-php-path ";" (getenv "PATH"))))
;; *** windows python about
;; **** windows pip-path setting
  (if entropy/win-portable-pip-enable (setenv "PATH" (concat entropy/win-portable-pip-path ";" (getenv "PATH"))))
;; **** windows python-path setting
  (if entropy/win-portable-python-enable (setenv "PATH" (concat entropy/win-portable-python-path ";" (getenv "PATH"))))
;; *** windows grep path setting
  (if entropy/win-portable-grep-enable (setenv "PATH" (concat entropy/win-portable-grep-path ";" (getenv "PATH"))))
;; *** windows ag-path setting
  (if entropy/win-portable-ag-enable (setenv "PATH" (concat (getenv "PATH") ";" entropy/win-portable-ag-path)))
;; *** windows rg-path setting
  (if entropy/win-portable-rg-enable (setenv "PATH" (concat entropy/win-portable-rg-path ";" (getenv "PATH"))))
;; *** windows pt-path setting
  (if entropy/win-portable-pt-enable (setenv "PATH" (concat entropy/win-portable-pt-path ";" (getenv "PATH"))))
;; *** windows nodejs-path setting
  (if entropy/win-portable-nodejs-enable (setenv "PATH" (concat entropy/win-portable-nodejs-path ";" (getenv "PATH"))))
;; *** windows-opencc
  (if entropy/win-portable-opencc-enable (setenv "PATH" (concat entropy/win-portable-opencc-path ";" (getenv "PATH"))))
;; *** windows-pandoc
  (if entropy/win-portable-pandoc-enable (setenv "PATH" (concat entropy/win-portable-pandoc-path ";" (getenv "PATH"))))
;; *** windows-portable-jdk
  (if entropy/win-portable-jdk-enable (setenv "PATH" (concat entropy/win-portable-jdk-path ";" (getenv "PATH"))))
;; *** windows-zeal
  (if entropy/win-portable-zeal-enable (setenv "PATH" (concat entropy/win-portable-zeal-path ";" (getenv "PATH"))))
;; *** windows portable putty
  (if entropy/win-portable-putty-enable
      (setenv "PATH" (concat entropy/win-portable-putty-path
                             ";"
                             (getenv "PATH")))))


;; ** exec path
(when sys/win32p
;; *** emacs bin folder
  (if entropy/win-emacs-bin-path-add
      (add-to-ordered-list 'exec-path invocation-directory 19))
;; *** wsl exec path setting
  (if entropy/wsl-enable
      (progn
        (add-to-ordered-list 'exec-path entropy/wsl-apps 12)
        (when entropy/wsl-enable-extra
          (add-to-ordered-list 'exec-path (concat entropy/wsl-apps-extra "usr/bin/") 18)
          (setq woman-manpath
                `(,(concat entropy/wsl-apps-extra "usr/man")
                  ,(concat entropy/wsl-apps-extra "usr/share/man")
                  ,(concat entropy/wsl-apps-extra "usr/local/man"))))))
;; *** clang for company-clang
  (if entropy/win-portable-clang-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-clang-path 15))
;; *** gcc for win exec path setting
  (if entropy/win-portable-mingw-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-mingw-path 10))  
;; **** windows git-portable exec path setting
  (if entropy/git-portable
      (add-to-ordered-list 'exec-path entropy/git-portable-path 9))
;; *** windows texlive path setting
  (if entropy/win-portable-texlive-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-texlive-path 8))

;; *** windows php
  (if entropy/win-portable-php-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-php-path 7))
;; *** windows python about
;; **** windows pip exec path setting
  (if entropy/win-portable-pip-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-pip-path 6))
;; **** windows python exec path setting
  (if entropy/win-portable-python-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-python-path 5))
  
;; *** windows grep exec path setting
  (if entropy/win-portable-grep-enable
      (add-to-ordered-list 'exec entropy/win-portable-grep-path 4))
;; *** windnows ag exec path setting
  (if entropy/win-portable-ag-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-ag-path 11))
;; *** windows rg exec path setting
  (if entropy/win-portable-rg-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-rg-path 3))
;; *** windows pt exec path setting
  (if entropy/win-portable-pt-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-pt-path 2))
;; *** windows nodejs exec path setting
  (if entropy/win-portable-nodejs-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-nodejs-path 1))

;; *** windows opencc exec path setting
  (if entropy/win-portable-opencc-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-opencc-path 13))

;; *** windows pandoc exec path setting
  (if entropy/win-portable-pandoc-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-pandoc-path 14))

;; *** windows portable jdk exec path setting
  (if entropy/win-portable-jdk-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-jdk-path 16))

;; *** windows portable zeal path setting
  (if entropy/win-portable-zeal-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-zeal-path 17))
;; *** windows portable putty path setting
  (if entropy/win-portable-putty-enable
      (add-to-ordered-list 'exec-path entropy/win-portable-putty-path 20)))

;; * provide
(provide 'entropy-emacs-path)
