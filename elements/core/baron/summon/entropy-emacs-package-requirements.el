;;; entropy-emacs-package-requirements.el --- entropy-emacs extensions pre-defination  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 2019067  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-package-requirements.el
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
;; List of `entropy-emacs' required extension, used for quick
;; indexing by `package.el'.
;;
;; * Configuration:
;;
;; loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

(!eemacs-require 'entropy-emacs-defcustom)

;; NOTE: ensure `package-archive-contents' obtained before loading
;; this package, in where `entropy-emacs-package' autoload it thus we
;; comment it with notation.
;;
;; (require 'package) (package-refresh-contents)

(defvar entropy-emacs-packages nil
  "List of packages which should installing/checking in list order.")

(defun __eemacs/pkg-reqs/get-newest-pkg-desc (pkg-name)
  (let ((pkg_descs (copy-sequence (alist-get pkg-name package-archive-contents))))
    (car (sort pkg_descs
               (lambda (x y)
                 (let ((x-v (package-desc-version x))
                       (y-v (package-desc-version y)))
                   (version-list-< y-v x-v)))))))

(eval-when-compile
  (defmacro __eemacs/pkg-reqs-list-items (&rest pkgs)
    (let ((arglist nil))
      (dolist (el pkgs)
        (if (symbolp el)
            (push (list 'quote el) arglist)
          (push el arglist)))
      `(delete nil (list ,@arglist)))))

;; EEMACS_MAINTENANCE: don't forget to update this list when the
;; eemacs site-lisp add new components
(defconst entropy/emacs--self-maintained-packges
  '(treemacs benchmark-init))
(defvar entropy/emacs--base-packges)
(setq entropy/emacs--base-packges
      (__eemacs/pkg-reqs-list-items
       ac-php
       ac-php-core
       ace-window
       advice-patch
       ag
       alert
       anaconda-mode
       async
       atomic-chrome
       auto-complete
       auto-sudoedit
       auto-yasnippet
       autothemer
       avy
       beacon
       bing-dict
       bongo
       browse-at-remote
       browse-kill-ring
       buffer-move
       cal-china-x
       chinese-word-at-point
       cmake-mode
       color-theme-sanityinc-tomorrow
       command-log-mode
       company
       company-anaconda
       company-box
       company-c-headers
       company-irony
       company-php
       company-prescient
       company-quickhelp
       company-shell
       company-web
       conda
       copyit
       counsel
       counsel-css
       counsel-ffdata
       counsel-world-clock
       cfrs
       dap-mode
       dash
       dashboard
       dash-functional
       diff-hl
       diminish
       dired-hacks-utils
       dired-quick-sort
       dired-subtree
       diredfl
       disable-mouse
       discover-my-major
       doom-modeline
       doom-themes
       edit-indirect
       editorconfig
       eglot
       external-completion
       eldoc
       eldoc-eval
       elfeed
       elisp-refs
       elisp-slime-nav
       emacsql
       emmet-mode
       emms
       epl
       esup
       eterm-256color
       exec-path-from-shell
       eyebrowse
       f
       find-file-in-project
       flycheck
       flymake
       ;; magit/ghub require emacs version upon 29 now [2024-12-23 Mon 17:59:06]
       (unless (< emacs-major-version 29) 'ghub)
       git-messenger
       git-timemachine
       git-modes
       gntp
       go-mode
       google-translate
       gotham-theme
       heap
       helm
       helm-ag
       helm-core
       hide-mode-line
       highlight-indent-guides
       highlight-parentheses
       hl-todo
       ht
       htmlize
       hydra
       ialign
       ibuffer-project
       impatient-mode
       irony
       irony-eldoc
       ivy
       ivy-hydra
       ivy-prescient
       ivy-rich
       ivy-xref
       js2-mode
       js2-refactor
       json-mode
       json-reformat
       json-snatcher
       log4e
       loop
       lsp-mode
       lsp-java
       lsp-pyright
       lsp-python-ms
       lsp-ui
       lua-mode
       lv
       macrostep
       magit
       magit-svn
       major-mode-hydra
       makey
       markdown-mode
       memoize
       memory-usage
       minions
       multi-term
       multiple-cursors
       names
       neotree
       nerd-icons
       nerd-icons-ivy-rich
       nerd-icons-ibuffer
       olivetti
       openwith
       org-bullets
       org-download
       org-pomodoro
       outorg
       outshine
       ox-gfm
       ox-reveal
       page-break-lines
       paradox
       pfuture
       php-mode
       pkg-info
       pomidor
       poporg
       popup
       pos-tip
       powerline
       powershell
       (unless (version< emacs-version "26") 'posframe)
       prescient
       pretty-hydra
       project
       pyim
       pyim-basedict
       pythonic
       queue
       rainbow-delimiters
       rainbow-mode
       request
       rg
       ripgrep
       rust-mode
       s
       search-web
       shackle
       shell-pop
       shrink-path
       simple-httpd
       skewer-mode
       slime
       slime-company
       smeargle
       spaceline
       spacemacs-theme
       separedit
       spinner
       srcery-theme
       ssh-agency
       swiper
       symbol-overlay
       tern
       tNFA
       toc-org
       transient
       treemacs-nerd-icons
       treepy
       trie
       ujelly-theme
       use-package
       vimish-fold
       visual-ascii-mode
       visual-regexp
       ;; --> obsolete
       ;; volatile-highlights
       (when (entropy/emacs-vterm-support-p) 'vterm)
       vundo
       w3m
       web-beautify
       web-completion-data
       web-mode
       web-server
       websocket
       wgrep
       which-key
       windresize
       with-editor
       xclip
       xcscope
       xr
       xterm-color
       yafolding
       yaml-mode
       yasnippet
       yasnippet-snippets
       youdao-dictionary
       zeal-at-point
       ))

(defvar entropy/emacs-pkr--builtin-packages)
(defvar entropy/emacs-pkr--packages)
(progn
  (setq entropy/emacs-pkr--builtin-packages nil)
  (setq entropy/emacs-pkr--packages nil)
  (let ((sort-func
         (lambda (x y)
           (condition-case err
               (assq (car x) (package-desc-reqs (cdr y)))
             (error (error "pkg %s -- %s" y err))))))
    (cl-loop
     for pkg in entropy/emacs--base-packges
     if (package-built-in-p pkg)
     do
     (push (cons pkg (__eemacs/pkg-reqs/get-newest-pkg-desc pkg))
           entropy/emacs-pkr--builtin-packages)
     else
     do
     (push (cons pkg (__eemacs/pkg-reqs/get-newest-pkg-desc pkg))
           entropy/emacs-pkr--packages))
    (setq entropy/emacs-pkr--builtin-packages
          (sort entropy/emacs-pkr--builtin-packages sort-func))
    (setq entropy/emacs-pkr--packages
          (sort entropy/emacs-pkr--packages sort-func))))

(let ((use-extras (unless (eq entropy/emacs-ext-elpkg-get-type 'origin)
                    (list
                     ;; disable maple preview as pre-request since its obsolete
                     ;; maple-preview
                     )))
      ;; NOTE: ensure bultin-packages shadowing so that any other
      ;; packages required those shadowed builtin-packages using ones
      ;; coming from `package-archives' at compile and load time,
      ;; where guaranteeing consist macro expansion and API stability.
      (pkgs-rtn (append entropy/emacs-pkr--builtin-packages
                        entropy/emacs-pkr--packages)))
  (when use-extras
    (setq pkgs-rtn (append pkgs-rtn use-extras)))
  (setq entropy-emacs-packages pkgs-rtn))

(provide 'entropy-emacs-package-requirements)
