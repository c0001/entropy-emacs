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

(require 'entropy-emacs-defcustom)

(defvar entropy-emacs-packages nil)

(setq entropy-emacs-packages
      `(
        ;; elpa&melpa packags
        ,(unless (version< emacs-version "26") 'company-posframe)
        ,(unless (version< emacs-version "26") 'posframe)
        ac-php
        ac-php-core
        ace-window
        ag
        alert
        all-the-icons
        all-the-icons-dired
        all-the-icons-ibuffer
        all-the-icons-ivy
        all-the-icons-ivy-rich
        anaconda-mode
        async
        atom-dark-theme
        atom-one-dark-theme
        atomic-chrome
        auto-complete
        auto-sudoedit
        auto-yasnippet
        autothemer
        avy
        beacon
        benchmark-init
        bind-key
        bing-dict
        birds-of-paradise-plus-theme
        bongo
        browse-at-remote
        browse-kill-ring
        buffer-move
        cal-china-x
        chinese-word-at-point
        chocolate-theme
        cmake-mode
        color-theme-sanityinc-tomorrow
        command-log-mode
        company
        company-anaconda
        company-c-headers
        company-irony
        company-lsp
        company-php
        company-prescient
        company-quickhelp
        company-shell
        company-tern
        company-web
        copyit
        counsel
        counsel-css
        counsel-ffdata
        counsel-projectile
        counsel-world-clock
        dap-mode
        darkokai-theme
        dash
        dash-functional
        diff-hl
        diminish
        dired-hacks-utils
        dired-quick-sort
        dired-rainbow
        dired-subtree
        diredfl
        disable-mouse
        discover-my-major
        doneburn-theme
        doom-modeline
        doom-themes
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
        eyebrowse
        f
        find-file-in-project
        ghub
        git-commit
        git-messenger
        git-timemachine
        gitattributes-mode
        gitconfig-mode
        github-theme
        gitignore-mode
        gntp
        go-mode
        google-translate
        gotham-theme
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
        ibuffer-projectile
        impatient-mode
        irony
        irony-eldoc
        ivy
        ivy-hydra
        ivy-rich
        ivy-xref
        js2-mode
        js2-refactor
        json-mode
        json-reformat
        json-snatcher
        klere-theme
        log4e
        loop
        lsp-java
        lsp-mode
        lsp-python-ms
        lsp-treemacs
        lsp-ui
        lua-mode
        lv
        macrostep
        magit
        magit-gitflow
        magit-popup
        magit-svn
        major-mode-hydra
        makey
        markdown-mode
        markdown-preview-mode
        material-theme
        memoize
        memory-usage
        minions
        multi-term
        multiple-cursors
        names
        neotree
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
        popwin
        pos-tip
        powerline
        powershell
        prescient
        pretty-hydra
        projectile
        pyim
        pyim-basedict
        pythonic
        rainbow-delimiters
        rainbow-mode
        request
        rg
        ripgrep
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
        solaire-mode
        spaceline
        spaceline-all-the-icons
        spacemacs-theme
        spinner
        srcery-theme
        ssh-agency
        sublime-themes
        swiper
        symbol-overlay
        tern
        toc-org
        transient
        treemacs
        treemacs-projectile
        treepy
        ujelly-theme
        use-package
        vimish-fold
        visual-ascii-mode
        visual-regexp
        volatile-highlights
        vterm
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
        yasnippet
        yasnippet-snippets
        youdao-dictionary
        zeal-at-point

        ;; eemacs-ext archived packages
        ,(when (not (eq entropy/emacs-use-extensions-type 'origin)) 'maple-preview)
        ))

;; filter packages required tree to remove all nil entry
(let (rtn)
  (dolist (el entropy-emacs-packages)
    (unless (eq nil el)
      (push el rtn)))
  (setq entropy-emacs-packages rtn))


(provide 'entropy-emacs-package-requirements)
