;;; entropy-emacs-library.el --- entropy emacs underlying library for other part
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-library.el
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
;; As what mentioned in `entropy-emacs.el' the core bridge component
;; of `entropy-emacs', excluded for the top basic part i.e. the
;; public variable and function declaration files, other part of
;; `entropy-emacs' is independently. As that the case for that if
;; some part sharing one extension who roling as the sharing
;; underlying extension, this file does as so.
;; 
;; * Configuration:
;; 
;; Loading automatically by `entropy-emacs'. May be useless for other
;; usages.
;; 
;; * Code:

(require 'entropy-emacs-defconst)

(when sys/win32p
  (require 'font-lock+))

;; ** all-the-icons
(use-package all-the-icons
  :commands
  (all-the-icons-icon-for-file
   all-the-icons-icon-for-mode
   all-the-icons--icon-info-for-buffer
   all-the-icons-install-fonts
   all-the-icons-insert)
  :config
;; *** icons specification  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss" :v-adjust 0.2))

  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-material "web" :v-adjust 0.0))

  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :v-adjust 0.2))

  (add-to-list 'all-the-icons-mode-icon-alist
               '(w3m-mode all-the-icons-faicon "chrome" :v-adjust -0.2))

  (add-to-list 'all-the-icons-mode-icon-alist
               '(gitignore-mode all-the-icons-alltheicon "git" :v-adjust 0.2))
  
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-toml-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  
  (add-to-list 'all-the-icons-icon-alist
               '("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue)))

;; ** eldoc-eval
(use-package eldoc-eval
  :defines eldoc-in-minibuffer-mode
  :commands (eldoc-in-minibuffer-mode
             eldoc-eval-expression)
  :config (setq eldoc-eval-preferred-function 'eval-expression))

;; ** shrink-path
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

;; ** posframe
(use-package posframe
  :commands
  (posframe-arghandler-default
   posframe-auto-delete
   posframe-delete
   posframe-delete-all
   posframe-delete-frame
   posframe-funcall
   posframe-hide
   posframe-hide-all
   posframe-poshandler-absolute-x-y
   posframe-poshandler-frame-bottom-left-corner
   posframe-poshandler-frame-bottom-right-corner
   posframe-poshandler-frame-center
   posframe-poshandler-frame-top-center
   posframe-poshandler-frame-top-left-corner
   posframe-poshandler-frame-top-right-corner
   posframe-poshandler-point-bottom-left-corner
   posframe-poshandler-point-top-left-corner
   posframe-poshandler-window-bottom-left-corner
   posframe-poshandler-window-bottom-right-corner
   posframe-poshandler-window-center
   posframe-poshandler-window-top-left-corner
   posframe-poshandler-window-top-right-corner
   posframe-run-poshandler
   posframe-show
   posframe-workable-p))

;; ** hydra

(use-package hydra
  :commands (defhydra))

;; * provide
(provide 'entropy-emacs-library)
