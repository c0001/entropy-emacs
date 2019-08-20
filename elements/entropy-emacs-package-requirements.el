;;; entropy-emacs-package-requirements.el --- entropy-emacs extensions pre-defination
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

(defvar entropy-emacs-packages nil)

(setq entropy-emacs-packages
      `(go-mode
        all-the-icons
        eldoc-eval
        shrink-path
        ibuffer-projectile
        symbol-overlay
        highlight-parentheses
        highlight-indent-guides
        rainbow-mode
        rainbow-delimiters
        hl-todo
        volatile-highlights
        counsel
        counsel-css
        ivy-xref
        avy
        all-the-icons-ivy
        ivy-rich
        helm-pt
        helm-ag
        find-file-in-project
        markdown-mode
        markdown-preview-mode
        lsp-mode
        lsp-ui
        lua-mode
        powerline
        spaceline
        spaceline-all-the-icons
        doom-modeline
        hide-mode-line
        diminish
        bind-key
        ivy
        dash
        benchmark-init
        htmlize
        org-bullets
        ox-reveal
        org-pomodoro
        org-download
        toc-org
        poporg
        py-autopep8
        anaconda-mode
        yafolding
        outshine
        eterm-256color
        shell-pop
        openwith
        beacon
        visual-regexp
        ialign
        atomic-chrome
        elfeed
        search-web
        w3m
        discover-my-major
        youdao-dictionary
        google-translate
        bing-dict
        command-log-mode
        pomidor
        ,(when (not (eq entropy/emacs-use-extensions-type 'origin)) 'maple-preview)
        counsel-world-clock
        copyit
        esup
        el2org
        ox-gfm
        counsel-ffdata
        neotree
        magit
        ssh-agency
        magit-popup
        magit-gitflow
        magit-svn
        git-messenger
        git-timemachine
        smeargle
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        browse-at-remote
        solaire-mode
        page-break-lines
        web-mode
        emmet-mode
        json-mode
        js2-mode
        tern
        js2-refactor
        skewer-mode
        impatient-mode
        web-beautify
        php-mode
        ac-php
        yasnippet
        yasnippet-snippets
        auto-yasnippet
        irony
        irony-eldoc
        window-number
        eyebrowse
        windresize
        buffer-move
        dired-quick-sort
        dired-rainbow
        all-the-icons-dired
        shackle
        which-key
        undo-tree
        auto-sudoedit
        disable-mouse
        pyim
        pyim-basedict
        ,(unless (version< emacs-version "26") 'posframe)
        cal-china-x
        company
        ,(unless (version< emacs-version "26") 'company-posframe)
        company-quickhelp
        company-lsp
        company-shell
        company-web
        company-tern
        company-php
        company-c-headers
        company-irony
        company-anaconda
        zeal-at-point
        macrostep
        elisp-slime-nav
        elisp-refs
        emms
        visual-ascii-mode

        ;; themes
        doom-themes
        atom-dark-theme
        atom-one-dark-theme
        autothemer
        birds-of-paradise-plus-theme
        color-theme-sanityinc-tomorrow
        chocolate-theme
        darkokai-theme
        doneburn-theme
        github-theme
        gotham-theme
        klere-theme
        material-theme
        spacemacs-theme
        srcery-theme
        sublime-themes
        ujelly-theme
        use-package))

;; filter packages required tree to remove all nil entry
(let (rtn)
  (dolist (el entropy-emacs-packages)
    (unless (eq nil el)
      (push el rtn)))
  (setq entropy-emacs-packages rtn))

(provide 'entropy-emacs-package-requirements)
