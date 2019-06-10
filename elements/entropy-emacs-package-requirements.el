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

(defvar entropy-emacs-packages
  `(ac-php
    apel
    atom-dark-theme
    atom-one-dark-theme
    atomic-chrome
    auto-yasnippet
    avy
    beacon
    bing-dict
    birds-of-paradise-plus-theme
    browse-at-remote
    browse-kill-ring
    buffer-move
    cal-china-x
    color-theme-sanityinc-tomorrow
    command-log-mode
    company-anaconda
    anaconda-mode
    company-c-headers
    company-irony
    ,(unless (version< emacs-version "26") 'company-posframe)
    company-quickhelp
    company-shell
    company-tern
    company-web
    company
    copyit
    counsel-css
    counsel
    darkokai-theme
    dash-functional
    diminish
    dired-rainbow
    dired-hacks-utils
    disable-mouse
    discover-my-major
    doneburn-theme
    doom-modeline
    doom-themes
    eterm-256color
    el2org
    eldoc-eval
    elfeed
    elisp-refs
    emmet-mode
    esup
    eyebrowse
    git-messenger
    git-timemachine
    gitattributes-mode
    gitconfig-mode
    github-theme
    gitignore-mode
    go-mode
    google-translate
    gotham-theme
    helm-ag
    helm
    helm-core
    highlight-indent-guides
    highlight-parentheses
    hl-todo
    ialign
    impatient-mode
    htmlize
    irony-eldoc
    irony
    ivy-xref
    js2-refactor
    json-mode
    json-reformat
    json-snatcher
    klere-theme
    loop
    lua-mode
    macrostep
    magit-gitflow
    magit-popup
    magit-svn
    magit
    git-commit
    makey
    markdown-preview-mode
    markdown-mode
    material-theme
    multi-term
    multiple-cursors
    openwith
    org-bullets
    org-download
    org-pomodoro
    ox-gfm
    page-break-lines
    php-mode
    pomidor
    alert
    log4e
    gntp
    poporg
    popwin
    ,(unless (version< emacs-version "26") 'posframe)
    py-autopep8
    pyim
    pyim-basedict
    pythonic
    rainbow-delimiters
    rainbow-mode
    shell-pop
    shrink-path
    f
    skewer-mode
    js2-mode
    simple-httpd
    smeargle
    spaceline-all-the-icons
    spaceline
    s
    powerline
    all-the-icons
    all-the-icons-dired
    memoize
    spacemacs-theme
    srcery-theme
    ssh-agency
    sublime-themes
    swiper
    ivy
    symbol-overlay
    tern
    toc-org
    transient
    dash
    ujelly-theme
    undo-tree
    use-package
    bind-key
    visual-regexp
    volatile-highlights
    w3m
    web-beautify
    web-mode
    web-server
    websocket
    which-key
    window-number
    windresize
    with-editor
    async
    yasnippet-snippets
    yasnippet
    youdao-dictionary
    names
    chinese-word-at-point
    pos-tip
    popup
    zeal-at-point
    search-web))

(provide 'entropy-emacs-package-requirements)
