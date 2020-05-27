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
(require 'cl-lib)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)

(when sys/win32p
  (require 'font-lock+))
;; ** memoize
(use-package memoize
  :commands
  (memoize
   memoize-restore
   memoize-by-buffer-contents--wrap
   memoize--wrap
   memoize-by-buffer-contents))

;; ** all-the-icons
(use-package all-the-icons
  :commands
  (all-the-icons-octicon-family
   all-the-icons-octicon
   all-the-icons-icon-for-buffer
   all-the-icons-icon-family-for-buffer
   all-the-icons-icon-family
   all-the-icons-fileicon-data
   all-the-icons-insert-alltheicon
   all-the-icons-wicon-data
   all-the-icons-icon-for-url
   all-the-icons-insert-material
   all-the-icons-install-fonts
   all-the-icons-insert-wicon
   all-the-icons-faicon
   all-the-icons-material-family
   all-the-icons-material
   all-the-icons-icon-for-weather
   all-the-icons-fileicon-family
   all-the-icons-insert
   all-the-icons-icon-for-mode
   all-the-icons-icon-for-file
   all-the-icons-match-to-alist
   all-the-icons-wicon
   all-the-icons-alltheicon-family
   all-the-icons-icon-family-for-mode
   all-the-icons-icon-family-for-file
   all-the-icons-wicon-family
   all-the-icons-faicon-family
   all-the-icons-octicon-data
   all-the-icons-insert-octicon
   all-the-icons-insert-icons-for
   all-the-icons-alltheicon-data
   all-the-icons-insert-fileicon
   all-the-icons-insert-faicon
   all-the-icons-ivy-setup
   all-the-icons-dir-is-submodule
   all-the-icons-faicon-data
   all-the-icons-material-data
   all-the-icons-auto-mode-match?
   all-the-icons-fileicon
   all-the-icons-alltheicon
   all-the-icons-icon-for-dir)

  :init
  ;; Patching dir icon list for fix some icon filter bugs
  (setq all-the-icons-dir-icon-alist
        '(("^[Tt]rash$" all-the-icons-faicon "trash-o" :height 1.2 :v-adjust -0.1)
          ("dropbox" all-the-icons-faicon "dropbox" :height 1.0 :v-adjust -0.1)
          ("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 1.0 :v-adjust -0.1)
          ("^atom$" all-the-icons-alltheicon "atom" :height 1.2 :v-adjust -0.1)
          ("^[Dd]ocuments$" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1)
          ("^[Dd]ownload$" all-the-icons-faicon "cloud-download" :height 0.9 :v-adjust -0.1)
          ("^[dD]esktop$" all-the-icons-octicon "device-desktop" :height 1.0 :v-adjust -0.1)
          ("^[pP]ictures$" all-the-icons-faicon "picture-o" :height 0.9 :v-adjust -0.2)
          ("^[pP]hotos$" all-the-icons-faicon "camera-retro" :height 1.0 :v-adjust -0.1)
          ("^[mM]usic$" all-the-icons-faicon "music" :height 1.0 :v-adjust -0.1)
          ("^[mM]ovies$" all-the-icons-faicon "film" :height 0.9 :v-adjust -0.1)
          ("code" all-the-icons-octicon "code" :height 1.1 :v-adjust -0.1)
          ("workspace" all-the-icons-octicon "code" :height 1.1 :v-adjust -0.1)
          ("^[Tt]est$" all-the-icons-fileicon "test-dir" :height 0.9)
          ("\\.git" all-the-icons-alltheicon "git" :height 1.0)
          (".?" all-the-icons-octicon "file-directory" :height 1.0 :v-adjust -0.1)))

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
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue))

  (dolist (music-mode '(mpc-mode
                        mpc-mode-menu
                        mpc-tagbrowser-mode
                        mpc-songs-mode
                        mpc-status-mode
                        mpc-tagbrowser-dir-mode
                        ))
    (add-to-list 'all-the-icons-mode-icon-alist
                 `(,music-mode all-the-icons-faicon "music" :face all-the-icons-blue)))

;; *** memoize missing funcs

  (dolist (func '(all-the-icons-icon-for-dir))
    (unless (get func :memoize-original-function)
      (memoize func))))

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

;; ** htmlize
(use-package htmlize
  :commands
  (htmlize-next-face-change
   htmlize-font-body-tag
   htmlize-fstruct-underlinep
   htmlize-decode-face-prop
   htmlize-fstruct-background
   htmlize-face-background
   htmlize-delete-tmp-overlays
   htmlize-many-files-dired
   htmlize-shadow-form-feeds
   htmlize-concat
   htmlize-decode-invisibility-spec
   htmlize-faces-in-buffer
   htmlize-ensure-fontified
   htmlize-face-css-name
   htmlize-region
   htmlize-inline-css-body-tag
   htmlize-memoize
   htmlize-copy-attr-if-set
   htmlize-font-text-markup
   htmlize-make-file-name
   htmlize-default-doctype
   htmlize-color-to-rgb
   htmlize-font-pre-tag
   htmlize-fstruct-overlinep
   htmlize-display-prop-to-html
   htmlize-fstruct-foreground
   htmlize-overlay-faces-at
   htmlize-face-foreground
   htmlize-match-inv-spec
   htmlize-fstruct-boldp
   htmlize-generate-image
   htmlize-next-change
   htmlize-default-body-tag
   htmlize-extract-text
   htmlize-fstruct-css-name
   htmlize-faces-at-point
   htmlize-css-specs
   htmlize-unstringify-face
   htmlize-untabify-string
   htmlize-inline-css-text-markup
   htmlize-buffer-1
   htmlize-region-for-paste
   htmlize-create-auto-links
   htmlize-fstruct-size
   htmlize-face-to-fstruct
   htmlize-get-text-with-display
   htmlize-method-function
   htmlize-with-fontify-message
   htmlize-buffer
   htmlize-face-size
   htmlize-face-set-from-keyword-attr
   htmlize-file
   htmlize-attrlist-to-fstruct
   htmlize-make-tmp-overlay
   htmlize-fstruct-italicp
   htmlize-buffer-substring-no-invisible
   htmlize-default-transform-image
   htmlize-sorted-overlays-at
   htmlize-make-face-map
   htmlize-get-color-rgb-hash
   htmlize-defang-local-variables
   htmlize-lexlet
   htmlize-add-before-after-strings
   htmlize-css-insert-head
   htmlize-face-color-internal
   htmlize-inline-css-pre-tag
   htmlize-make-link-overlay
   htmlize-escape-or-link
   htmlize-string-to-html
   htmlize-attr-escape
   htmlize-region-save-screenshot
   htmlize-trim-ellipsis
   htmlize-fstruct-strikep
   htmlize-fstruct-p
   htmlize-get-override-fstruct
   htmlize-merge-faces
   htmlize-method
   htmlize-despam-address
   htmlize-alt-text
   htmlize-many-files
   htmlize-css-text-markup
   htmlize-merge-size
   htmlize-protect-string
   htmlize-face-to-fstruct-1
   htmlize-default-pre-tag
   htmlize-merge-two-faces
   htmlize-format-link
   htmlize-copy-prop
   ))

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
   posframe-workable-p)
  :preface
  (defun entropy/emacs-posframe-adapted-p ()
    "Judge whether posframe can be used in current emacs session."
    (and (not (version< emacs-version "26.1"))
         (display-graphic-p))))

;; ** hydra
;; *** hydra core
(use-package hydra
  :commands (defhydra)
  :init
  ;; Fix '[]' as key stroke in hydra doc init refer to
  ;; https://github.com/abo-abo/hydra/issues/365#issue-574484394
  ;;
  ;; This will remove when update hydra package.
  (setq hydra-key-regex
        "[][\\[:alnum:] ~.,;:/|?<>={}*+#%@!&^↑↓←→⌫⌦⏎'`()\"$-]+?"))

;; *** pretty-hydra

(use-package pretty-hydra
  :commands
  (
   pretty-hydra-define
   pretty-hydra-define+
   pretty-hydra-toggle
   pretty-hydra--merge-heads
   )
  :init
  (setq pretty-hydra-enable-use-package t)

  :config
  (defun entropy/emacs-pretty-hydra--patch-1 (orig-func &rest orig-args)
    "The around advice for inhibit any restriction for
`prin1-to-string' while generate pretty-hydra doc-string, thus
for that there's some un-investigated causes during the pretty
hydra docstring title generation that pollute the sexp printing
format which caused by set the restriction for thus."
    (let* ((print-level nil)
           (print-length nil))
      (apply orig-func orig-args)))

  (advice-add 'pretty-hydra--generate
              :around
              #'entropy/emacs-pretty-hydra--patch-1))

;; *** major-hydra

(use-package major-mode-hydra
  :commands
  (
   major-mode-hydra-define
   major-mode-hydra
   major-mode-hydra-bind
   major-mode-hydra-define+
   ))

;; *** def APIs
;; **** pretty hydra title making
(with-no-warnings
  (cl-defun entropy/emacs-pretty-hydra-make-title
      (title &optional icon-type icon-name
             &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face 'entropy/emacs-defface-face-for-hydra-orange-face))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (entropy/emacs-icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(cl-defun entropy/emacs-pretty-hydra-make-title-for-major-mode-common
    (mode title-str &key face height v-adjust)
  (require 'faces)
  (let* ((icon-display-p (entropy/emacs-icons-displayable-p))
         (face (or face 'entropy/emacs-defface-face-for-hydra-orange-face))
         (icon (if (fboundp 'all-the-icons-icon-for-mode)
                   (ignore-errors
                     (all-the-icons-icon-for-mode
                      mode
                      :face face :height (or height 1)
                      :v-adjust (or v-adjust 0)))
                 (when icon-display-p
                   (error "Function <all-the-icons-icon-for-mode> not found!")))))
    (concat
     (when icon-display-p
       (if (not (stringp icon))
           "[unmached icon]"
         icon))
     " "
     (propertize title-str 'face face))))

(defun entropy/emacs-pretty-hydra-make-body-for-major-mode-union (mode)
  `(:title
    (let* ((mode-str (capitalize (symbol-name ',mode)))
           (title (entropy/emacs-pretty-hydra-make-title-for-major-mode-common
                   ',mode (format "%s Actions" mode-str))))
      title)
    :color ambranth
    :quit-key "q"
    :separator "═"))

;; ** xclip activation
(use-package xclip
  :commands
  (xclip-mode))

;; * provide
(provide 'entropy-emacs-utils)
