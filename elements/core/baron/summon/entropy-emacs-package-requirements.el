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

(defvar entropy-emacs-packages nil)

(defun __entropy/emacs-requirements/pkgs_desc_get_statble (pkg-name)
  (let ((pkg_descs (alist-get pkg-name package-archive-contents)))
    (car (sort pkg_descs
               (lambda (x y)
                 (let ((x-v (package-desc-version x))
                       (y-v (package-desc-version y)))
                   (version-list-<= x-v y-v)))))))

(defun entropy/emacs-pkgreq-get-pkgreqptr-pkg-slot
    (pkgreqptr slot)
  (let ((rtn
         (if (symbolp pkgreqptr)
             (when (and slot
                        (eq slot :name))
               pkgreqptr)
           (plist-get pkgreqptr slot))))
    (when (and rtn
               (eq slot :pkg-desc))
      (setq rtn
            (if (functionp rtn)
                (funcall rtn)
              rtn)))
    rtn))

(defvar entropy/emacs--base-packges
  `(
    ac-php
    ac-php-core
    ace-window
    advice-patch
    ag
    alert
    all-the-icons
    all-the-icons-ibuffer
    all-the-icons-ivy
    all-the-icons-ivy-rich
    anaconda-mode
    async
    atomic-chrome
    auto-complete
    auto-sudoedit
    auto-yasnippet
    autothemer
    avy
    beacon
    bind-key
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
    counsel-projectile
    counsel-world-clock
    dash
    dashboard
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
    doom-modeline
    doom-themes
    edit-indirect
    ;; force use new version of `eglot'
    ,(list
      :name 'eglot
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'eglot)))
    ;; force use new version of `eldoc' for new version of `eglot'
    ,(list
      :name 'eldoc
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'eldoc)))
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
    ;; force use new version of `flymake' for new version of `eglot'
    ,(list
      :name 'flymake
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'flymake)))
    ghub
    git-commit
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
    ibuffer-projectile
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
    markdown-preview-mode
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
    pos-tip
    powerline
    powershell
    prescient
    pretty-hydra
    ;; forcely install newer version of `project' since newer version flymake needed
    ,(list
      :name 'project
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'project)))
    projectile
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
    spaceline-all-the-icons
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
    treepy
    trie
    ujelly-theme
    use-package
    vimish-fold
    visual-ascii-mode
    visual-regexp
    ;; --> obsolete
    ;; volatile-highlights
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

(setq entropy-emacs-packages
      (let ((use-posframe (unless (version< emacs-version "26") 'posframe))
            (use-vterm (when (entropy/emacs-vterm-support-p) 'vterm))
            (use-extras (unless (eq entropy/emacs-ext-elpkg-get-type 'origin)
                          (list
                           ;; disable maple preview as pre-request since its obsolete
                           ;; maple-preview
                           )))
            (pkgs-rtn entropy/emacs--base-packges))
        (when use-posframe
          (setq pkgs-rtn (append pkgs-rtn (list use-posframe))))
        (when use-vterm
          (setq pkgs-rtn (append pkgs-rtn (list use-vterm))))
        (when use-extras
          (setq pkgs-rtn (append pkgs-rtn use-extras)))
	(setq entropy-emacs-packages pkgs-rtn)))


(provide 'entropy-emacs-package-requirements)
