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
  (let ((pkg_descs (copy-sequence (alist-get pkg-name package-archive-contents))))
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

(cl-defun entropy/emacs-pkgreq-make-pkgreqptr
    (&key under name pkg-desc)
  (unless (and under (>= emacs-major-version under))
    (list :name name :pkg-desc pkg-desc)))

(eval-when-compile
  (defmacro __eemac/pkg-reqs-list-items (&rest pkgs)
    (let ((arglist nil))
      (dolist (el pkgs)
        (if (symbolp el)
            (push (list 'quote el) arglist)
          (push el arglist)))
      `(list ,@arglist))))

;; EEMACS_MAINTENANCE: don't forget to update this list when the
;; eemacs site-lisp add new components
(defconst entropy/emacs--self-maintained-packges
  '(treemacs benchmark-init))
(defvar entropy/emacs--base-packges
  (__eemac/pkg-reqs-list-items
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
    ;; force use new version of `eglot'
    (entropy/emacs-pkgreq-make-pkgreqptr
      :under 30
      :name 'eglot
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'eglot)))
    (entropy/emacs-pkgreq-make-pkgreqptr
      :under 29
      :name 'external-completion
      :pkg-desc (lambda ()
                  (__entropy/emacs-requirements/pkgs_desc_get_statble
                   'external-completion)))
    ;; force use new version of `eldoc' for new version of `eglot'
    (entropy/emacs-pkgreq-make-pkgreqptr
      :under 29
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
    (entropy/emacs-pkgreq-make-pkgreqptr
      :under 29
      :name 'flymake
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'flymake)))
    ;; magit/ghub require emacs version upon 29 now [2024-12-23 Mon 17:59:06]
    (when (version<= "29.1" emacs-version) 'ghub)
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
    prescient
    pretty-hydra
    ;; forcely install newer version of `project' since newer version flymake needed
    (entropy/emacs-pkgreq-make-pkgreqptr
      :under 29
      :name 'project
      :pkg-desc (lambda () (__entropy/emacs-requirements/pkgs_desc_get_statble 'project)))
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
    (when (< emacs-major-version 29) 'use-package)
    vimish-fold
    visual-ascii-mode
    visual-regexp
    ;; --> obsolete
    ;; volatile-highlights
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

(setq entropy-emacs-packages
      (let ((use-posframe (unless (version< emacs-version "26") 'posframe))
            (use-vterm (when (entropy/emacs-vterm-support-p) 'vterm))
            (use-extras (unless (eq entropy/emacs-ext-elpkg-get-type 'origin)
                          (list
                           ;; disable maple preview as pre-request since its obsolete
                           ;; maple-preview
                           )))
            (pkgs-rtn (delete nil entropy/emacs--base-packges)))
        (when use-posframe
          (setq pkgs-rtn (append pkgs-rtn (list use-posframe))))
        (when use-vterm
          (setq pkgs-rtn (append pkgs-rtn (list use-vterm))))
        (when use-extras
          (setq pkgs-rtn (append pkgs-rtn use-extras)))
        (setq entropy-emacs-packages pkgs-rtn)))


(provide 'entropy-emacs-package-requirements)
