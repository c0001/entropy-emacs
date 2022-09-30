;;; entropy-emacs-library.el --- entropy emacs underlying library for other part  -*- lexical-binding: t; -*-
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
(!eemacs-require 'entropy-emacs-defconst)
(!eemacs-require 'entropy-emacs-defun)

;; ** dash
(use-package dash :init (require 'dash)
  :config
  (entropy/emacs-lazy-initial-for-hook
   '(emacs-lisp-mode-hook)
   "dash-fontify-mode-init" "dash-fontify-mode-init"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (global-dash-fontify-mode)))

;; ** f
(use-package f :init (require 'f))

;; ** advice-patch
;; This package builds on `advice-add' but instead of letting you add code
;; before/after/around the body of the advised function, it lets you directly
;; patch the inside of that function.
;;
;; This is inspired from [el-patch](https://github.com/raxod502/el-patch),
;; but stripped down to its barest essentials.  `el-patch' provides many more
;; features, especially to be notified when the advised function is modified
;; and to help you update your patches accordingly.

(use-package advice-patch
  :eemacs-functions (advice--patch advice-patch))

;; ** memoize
(use-package memoize
  :commands
  (memoize
   memoize-restore
   memoize-by-buffer-contents--wrap
   memoize--wrap
   memoize-by-buffer-contents))


;; ** trie

(use-package trie
  :commands
  (make-trie trie-insert trie-complete))

;; ** prescient
(use-package prescient
  :commands
  (prescient-sort
   prescient-persist-mode
   prescient-filter
   prescient-remember
   prescient-with-group
   prescient-split-query
   prescient-sort-compare
   prescient-fuzzy-regexp
   prescient-prefix-regexp
   prescient-regexp-regexp
   prescient-filter-regexps
   prescient-literal-regexp
   prescient-anchored-regexp
   prescient-initials-regexp
   prescient-literal-prefix-regexp
   )
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(ivy-mode counsel-mode company-mode)
   "prescient-mode-init"
   "prescient-mode-init"
   :prompt-type 'prompt-echo
   :pdumper-no-end t
   (prescient-persist-mode 1)))

;; ** exec-path-from-shell
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-copy-env
             exec-path-from-shell-initialize
             exec-path-from-shell-getenv
             exec-path-from-shell-getenvs))

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
        '(("^[Tt]rash$"       all-the-icons-faicon     "trash-o"        :height 1.2 :v-adjust -0.1)
          ("dropbox"          all-the-icons-faicon     "dropbox"        :height 1.0 :v-adjust -0.1)
          ("google[ _-]drive" all-the-icons-alltheicon "google-drive"   :height 1.0 :v-adjust -0.1)
          ("^atom$"           all-the-icons-alltheicon "atom"           :height 1.2 :v-adjust -0.1)
          ("^[Dd]ocuments$"   all-the-icons-faicon     "book"           :height 1.0 :v-adjust -0.1)
          ("^[Dd]ownload$"    all-the-icons-faicon     "cloud-download" :height 0.9 :v-adjust -0.1)
          ("^[dD]esktop$"     all-the-icons-octicon    "device-desktop" :height 1.0 :v-adjust -0.1)
          ("^[pP]ictures$"    all-the-icons-faicon     "picture-o"      :height 0.9 :v-adjust -0.2)
          ("^[pP]hotos$"      all-the-icons-faicon     "camera-retro"   :height 1.0 :v-adjust -0.1)
          ("^[mM]usic$"       all-the-icons-faicon     "music"          :height 1.0 :v-adjust -0.1)
          ("^[mM]ovies$"      all-the-icons-faicon     "film"           :height 0.9 :v-adjust -0.1)
          ("^[Cc]ode$"        all-the-icons-octicon    "code"           :height 1.1 :v-adjust -0.1)
          ("workspace"        all-the-icons-octicon    "code"           :height 1.1 :v-adjust -0.1)
          ("^[Tt]est$"        all-the-icons-fileicon   "test-dir"       :height 0.9)
          ("\\.git"           all-the-icons-alltheicon "git"            :height 1.0)
          (".?"               all-the-icons-octicon    "file-directory" :height 1.0 :v-adjust -0.1)))

  :config
;; *** icons specification

  (dolist (el
           `(("\\.xpm$"     all-the-icons-octicon  "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen)
             ("\\.toml$"    all-the-icons-octicon  "settings"    :v-adjust 0.0 :face all-the-icons-dyellow)
             ("\\.lua$"     all-the-icons-fileicon "lua"         :face all-the-icons-dblue)
             ("\\.go$"      all-the-icons-fileicon "go"          :face all-the-icons-blue)
             ("NEWS$"       all-the-icons-faicon   "newspaper-o" :height 0.9   :v-adjust -0.2)
             ("Cask$"       all-the-icons-fileicon "elisp"       :height 1.0   :v-adjust -0.2 :face all-the-icons-blue)
             (".*\\.ipynb$" all-the-icons-fileicon "jupyter"     :height 1.2   :face all-the-icons-orange)
             ("\\.epub$"    all-the-icons-faicon   "book"        :height 1.0   :v-adjust -0.1 :face all-the-icons-green)))
    (add-to-list 'all-the-icons-icon-alist
                 el))

  (dolist (el
           `((cask-mode                   all-the-icons-fileicon   "elisp"       :height 1.0   :v-adjust -0.2 :face all-the-icons-blue)
             (lua-mode                    all-the-icons-fileicon   "lua"         :face all-the-icons-dblue)
             (conf-toml-mode              all-the-icons-octicon    "settings"    :v-adjust 0.0 :face all-the-icons-dyellow)
             (nov-mode                    all-the-icons-faicon     "book"        :height 1.0   :v-adjust -0.1 :face all-the-icons-green)
             (gfm-mode                    all-the-icons-octicon    "markdown"    :face all-the-icons-blue)
             (vterm-mode                  all-the-icons-octicon    "terminal"    :v-adjust 0.2)
             (elfeed-search-mode          all-the-icons-faicon     "rss"         :v-adjust 0.2)
             (elfeed-show-mode            all-the-icons-material   "web"         :v-adjust 0.0)
             (Info-mode                   all-the-icons-faicon     "info-circle" :v-adjust 0.2)
             (w3m-mode                    all-the-icons-faicon     "chrome"      :v-adjust -0.2)
             (gitignore-mode              all-the-icons-alltheicon "git"         :v-adjust 0.2)
             (ein:notebooklist-mode       all-the-icons-faicon     "book"        :face all-the-icons-orange)
             (ein:notebook-mode           all-the-icons-fileicon   "jupyter"     :height 1.2   :face all-the-icons-orange)
             (ein:notebook-multilang-mode all-the-icons-fileicon   "jupyter"     :height 1.2   :face all-the-icons-orange)
             (go-mode                     all-the-icons-fileicon   "go"          :face all-the-icons-blue)
             (help-mode                   all-the-icons-faicon     "info-circle" :height 1.1   :v-adjust -0.1 :face all-the-icons-purple)
             (Info-mode                   all-the-icons-faicon     "info-circle" :height 1.1   :v-adjust -0.1)
             (conf-mode                   all-the-icons-octicon    "settings"    :v-adjust 0.0 :face all-the-icons-dyellow)
             (conf-unix-mode              all-the-icons-octicon    "settings"    :v-adjust 0.0 :face all-the-icons-dyellow)
             ,@(mapcar
                (lambda (music-mode)
                  (list music-mode 'all-the-icons-faicon "music" :face 'all-the-icons-blue))
                '(mpc-mode
                  mpc-mode-menu
                  mpc-tagbrowser-mode
                  mpc-songs-mode
                  mpc-status-mode
                  mpc-tagbrowser-dir-mode
                  ))
             ;; add more rules
             ))

    (add-to-list 'all-the-icons-mode-icon-alist
                 el))

;; *** memoize internal icon render

  ;; ;; FIXME: memoize all-the-icons-* casue emacs input lag
  ;; (dolist (func '(all-the-icons-material
  ;;                 all-the-icons-faicon
  ;;                 all-the-icons-octicon
  ;;                 all-the-icons-material
  ;;                 all-the-icons-alltheicon
  ;;                 ))
  ;;   (unless (get func :memoize-original-function)
  ;;     (memoize func)))
  )

;; ** eldoc
;; *** Core
(use-package eldoc
  :ensure nil
  :commands (eldoc eldoc-mode global-eldoc-mode)
  :init

  ;; Use union IDE like doc delay setting
  (setq eldoc-idle-delay entropy/emacs-ide-doc-delay)

  ;; ---------- Temporally eldoc-mode patch
  (defvar-local entropy/emacs-eldoc-inhibit-in-current-buffer nil)
  (defun entropy/emacs-eldoc-inhibit-around-advice
      (orig-func &rest orig-args)
    "Around advice for disable `eldoc-mode' with the idlle time
of `eldoc-idle-delay' after excute the ORIG-FUNC."
    (unwind-protect
        (apply orig-func orig-args)
      (prog1
          nil
        (setq
         entropy/emacs-eldoc-inhibit-in-current-buffer
         t))))

  :config
  ;; ---------- Truncate lines for eldoc message when in some occasions
  (defun __adv/around/eldoc-minibuffer-message
      (orig-func &rest orig-args)
    "Around advice for `eldoc-minibuffer-message' by eemacs."
    (let ((message-truncate-lines
           ;; truncate message lines in some cases
           (or (bound-and-true-p lsp-mode))))
      (apply orig-func orig-args)))
  (advice-add 'eldoc-minibuffer-message
              :around
              #'__adv/around/eldoc-minibuffer-message)

  ;; ---------- Disable eldoc idle trigger in some occasions
  (defun __adv/around/eldoc-schedule-timer/filter-run
      (orig-func &rest orig-args)
    "Around advice for `eldoc-schedule-timer' to disable
`eldoc-mode' while filter the by eemacs internal spec."
    (let (_)
      (cond ((bound-and-true-p
              entropy/emacs-eldoc-inhibit-in-current-buffer)
             (eldoc-mode 0)
             nil)
            (t
             (apply orig-func orig-args)))))
  (advice-add 'eldoc-schedule-timer
              :around
              #'__adv/around/eldoc-schedule-timer/filter-run)
  )

;; *** Use new version of `eldoc'

(when (version< emacs-version "28")
  (defvar __new_pkg/eldoc
    (expand-file-name
     "eldoc.elc"
     (package-desc-dir
      (cadr (assq 'eldoc (package--alist)))))
    "The elisp file of the new `eldoc' version")
  (defvar __ya/eldoc-newpkg-load-p nil)
  (cond ((and (bound-and-true-p entropy/emacs-custom-enable-lazy-load)
              t)
         (entropy/emacs-lazy-initial-for-hook
          '(entropy/emacs-after-startup-hook)
          "eldoc-new-version-load" "eldoc-new-version-load"
          :prompt-type 'prompt-echo
          (unless (bound-and-true-p __ya/eldoc-newpkg-load-p)
            (eval
             `(load ',__new_pkg/eldoc))
            (setq __ya/eldoc-newpkg-load-p t))))
        (t
         (load __new_pkg/eldoc))))

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

;; ** popup

(use-package popup
  :commands (popup-tip))

;; ** transient

(use-package transient)

;; ** hydra
;; *** hydra core
(use-package hydra
  :eemacs-macros (defhydra)
  :eemacs-functions (hydra-default-pre)
  :init
  ;; Fix '[]' as key stroke in hydra doc init refer to
  ;; https://github.com/abo-abo/hydra/issues/365#issue-574484394
  ;;
  ;; This will remove when update hydra package.
  (setq hydra-key-regex
        "[][\\[:alnum:] ~.,;:/|?<>={}*+#%@!&^↑↓←→⌫⌦⏎'`()\"$-]+?")
  :config
;; **** core patch
;; ****** `hydra--make-defun'

  (defun hydra--make-defun (name body doc head
                                 keymap body-pre body-before-exit
                                 &optional body-after-exit)
    "Make a defun wrapper, using NAME, BODY, DOC, HEAD, and KEYMAP.
NAME and BODY are the arguments to `defhydra'.
DOC was generated with `hydra--doc'.
HEAD is one of the HEADS passed to `defhydra'.
BODY-PRE is added to the start of the wrapper.
BODY-BEFORE-EXIT will be called before the hydra quits.
BODY-AFTER-EXIT is added to the end of the wrapper.

NOTE: this function has been redefined by eemacs by suggested by
`entropy/emacs-top-improvements-for-require'.

EEMACS_MAINTENANCE: follow upstream updates and just copy the new
version src and remove the redudant require statements in this
function."
    (let* ((cmd-name (hydra--head-name head name))
           (cmd (when (car head)
                  (hydra--make-callable
                   (cadr head))))
           (doc (if (car head)
                    (format "Call the head `%S' in the \"%s\" hydra.\n\n%s"
                            (cadr head) name doc)
                  (format "Call the body in the \"%s\" hydra.\n\n%s"
                          name doc)))
           (hint (intern (format "%S/hint" name)))
           (body-foreign-keys (hydra--body-foreign-keys body))
           (body-timeout (plist-get body :timeout))
           (idle (or (and (eq (cadr head) 'body) (plist-get body :idle))
                     (plist-get (nthcdr 3 head) :idle)))
           (curr-body-fn-sym (intern (format "%S/body" name)))
           (body-on-exit-t
            `((hydra-keyboard-quit)
              (setq hydra-curr-body-fn ',curr-body-fn-sym)
              ,@(if body-after-exit
                    `((unwind-protect
                          ,(when cmd
                             (hydra--call-interactively cmd (cadr head)))
                        ,body-after-exit))
                  (when cmd
                    `(,(hydra--call-interactively cmd (cadr head)))))))
           (body-on-exit-nil
            (delq
             nil
             `((let ((hydra--ignore ,(not (eq (cadr head) 'body))))
                 (hydra-keyboard-quit)
                 (setq hydra-curr-body-fn ',curr-body-fn-sym))
               ,(when cmd
                  `(condition-case err
                       ,(hydra--call-interactively cmd (cadr head))
                     ((quit error)
                      (message (error-message-string err)))))
               ,(if idle
                    `(hydra-idle-message ,idle ,hint ',name)
                  `(hydra-show-hint ,hint ',name))
               (hydra-set-transient-map
                ,keymap
                (lambda () (hydra-keyboard-quit) ,body-before-exit)
                ,(when body-foreign-keys
                   (list 'quote body-foreign-keys)))
               ,body-after-exit
               ,(when body-timeout
                  `(hydra-timeout ,body-timeout))))))
      `(defun ,cmd-name ()
         ,doc
         (interactive)
         ;; (require 'hydra)
         (hydra-default-pre)
         ,@(when body-pre (list body-pre))
         ,@(cond ((eq (hydra--head-property head :exit) t)
                  body-on-exit-t)
                 ((eq (hydra--head-property head :exit) nil)
                  body-on-exit-nil)
                 (t
                  `((if ,(hydra--head-property head :exit)
                        (progn
                          ,@body-on-exit-t)
                      ,@body-on-exit-nil)))))))

  )

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

;; **** Patch
;; ***** core def
  (defvar entropy/emacs-pretty-hydra-posframe-visible-p nil)
  (defvar entropy/emacs-pretty-hydra-defined-indcator nil)
  (defvar entropy/emacs-pretty-hydra-posframe-boder-color "red")
  (defun  entropy/emacs-pretty-hydra-posframe-canbe-use ()
    "Return non-nil when current emacs-session can use
`posframe-show' to show the pretty hydra."
    (and
     (display-graphic-p)
     (fboundp 'posframe-show)))
  (defvar entropy/emacs-pretty-hydra-inhibt-use-posframe nil
    "Inhibit use `posframe-show' to show the hydra hints even if
`entropy/emacs-pretty-hydra-posframe-canbe-use' is satisfied.")
  (defvar entropy/emacs-pretty-hydra--hydra-hints-let-env
    '((hydra-hint-display-type
       (if (and (entropy/emacs-pretty-hydra-posframe-canbe-use)
                (not entropy/emacs-pretty-hydra-inhibt-use-posframe))
           'posframe
         'lv))
      (hydra-posframe-show-params
       ;; EEMACS_MAINTENANCE: follow `hydra' updates
       (when (eq hydra-hint-display-type 'posframe)
         (list
          ;; let font same as parent frame
          :font (frame-parameter nil 'font)
          :internal-border-width 1
          :internal-border-color entropy/emacs-pretty-hydra-posframe-boder-color
          ;; truncate line always in hydra posframe
          :lines-truncate t
          ;; stick on frame center always while show hydra posframe
          :poshandler 'posframe-poshandler-frame-center
          ))))
    "The hydra referred `let*' bindings as an eemacs pretty hydra pre
bindings for as.")


;; ***** hydra refer patch
;; ****** hydra posframe patch
;; ******* hydra posframe show patch

  ;; EEMACS_MAINTENANCE: follow `hydra-posframe-show' internal buffer name updates
  (defvar __hydra-posframe-buff-name " *hydra-posframe*")
  (defun __hydra-posframe-buffer-live-p (&rest _)
    (let ((buff (get-buffer __hydra-posframe-buff-name)))
      (and (bufferp buff)
           (buffer-live-p buff))))

  (defun __adv/around/hydra-posframe-show
      (orig-func &rest orig-args)
    "Reset the posframe `internal-border' face background color
since the posframe resuse the invisible old created
`posframe--frame' to speed reason, but it's parameter can be
easily modified by others."
    (let ((rtn (apply orig-func orig-args)))
      (unless (framep rtn)
        (error "Update the pretty-hydra hack on \
`hydra-posframe-show' since internal api is changed"))
      (set-face-background 'internal-border
                           entropy/emacs-pretty-hydra-posframe-boder-color
                           rtn)
      rtn))
  (advice-add 'hydra-posframe-show
              :around
              #'__adv/around/hydra-posframe-show)
  ;; FIXME: Recreate the hydra-posframe before load a new theme since
  ;; the new theme may cover someting patched yet e.g. the border
  ;; face?
  (add-hook 'entropy/emacs-theme-load-before-hook
            #'(lambda (&rest _)
                (when (and (__hydra-posframe-buffer-live-p)
                           (fboundp 'posframe-delete-frame))
                  (posframe-delete-frame
                   __hydra-posframe-buff-name))))

;; ******* hydra posframe hide patch

  (defun __adv/around/hydra-posframe-hide/close-eemacs-pretty-hydra
      (&rest _)
    ;; EEMACS_MAINTENANCE: follow `hydra' updates
    "Unset `entropy/emacs-pretty-hydra-posframe-visible-p' after
close hydra posframe."
    (require 'posframe)
    (unless hydra--posframe-timer
      (setq hydra--posframe-timer
            (run-with-idle-timer
             0 nil
             (lambda ()
               (setq hydra--posframe-timer nil)
               (posframe-hide __hydra-posframe-buff-name)
               (setq entropy/emacs-pretty-hydra-posframe-visible-p
                     nil))))))
  (advice-add 'hydra-posframe-hide
              :override
              #'__adv/around/hydra-posframe-hide/close-eemacs-pretty-hydra)
  (defun __adv/around/hydra-keyboard-quit/close-eemacs-pretty-hydra
      (orig-func &rest orig-args)
    "Bound `hydra-hint-display-type' to posframe when
`entropy/emacs-pretty-hydra-posframe-visible-p' non-nil."
    (let (_)
      (if entropy/emacs-pretty-hydra-posframe-visible-p
          (let ((hydra-hint-display-type 'posframe))
            (apply orig-func orig-args))
        (apply orig-func orig-args))))
  (advice-add 'hydra-keyboard-quit
              :around
              #'__adv/around/hydra-keyboard-quit/close-eemacs-pretty-hydra)

;; ******* hydra posframe make-defun patch

  (defun __adv/around/hydra--make-defun/for-pretty-hydra-patch
      (orig-func &rest orig-args)
    "Let the \"sub\" hydra defined by `pretty-hydra-define' be
forcely follow the `entropy/emacs-pretty-hydra--hydra-hints-let-env'
env."
    (if (not (bound-and-true-p
              entropy/emacs-pretty-hydra-defined-indcator))
        (apply orig-func orig-args)
      (let ((rtn (apply orig-func orig-args)))
        (unless (eq (car rtn) 'defun)
          (error "Update the pretty-hydra hack on \
`hydra--make-defun' since internal api is changed"))
        (let* ((name (cadr rtn))
               (name-adv (intern
                          (format "__adv/around/%s/with-pretty-hydra-hack"
                                  name))))
          (setq rtn
                `(prog1
                     (progn
                       ,rtn)
                   (defun ,name-adv (orig-func &rest orig-args)
                     ,(format "pretty-hydra hacked around advice for `%s'."
                              name)
                     (let* (,@entropy/emacs-pretty-hydra--hydra-hints-let-env)
                       (apply orig-func orig-args)))
                   (advice-add ',name :around #',name-adv)))
          rtn))))
  (advice-add 'hydra--make-defun
              :around
              #'__adv/around/hydra--make-defun/for-pretty-hydra-patch)

;; ***** patch 1
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
              #'entropy/emacs-pretty-hydra--patch-1)

;; ***** patch 2
  (defun entropy/emacs-pretty-hydra--patch-2
      (orig-func &rest orig-args)
    "Let all hydra defined by `pretty-hydra-define' show with
posframe when available."
    (let* ((name (car orig-args))
           (body-func-name-str
            (format "%S/body" name))
           (body-adfunc-name
            (intern
             (format "__adv/around/%s/__use-posframe-show"
                     body-func-name-str)))
           (body-func-name nil)
           rtn)
      (progn
        (setq rtn (apply orig-func orig-args))
        ;; we just intern the adfunc when the hydra generator return
        ;; success for preventing obarray messy.
        (setq body-func-name
              (intern body-func-name-str)))
      ;; inject advice after the origin macro
      (setq rtn
            `(prog1
                 (let ((entropy/emacs-pretty-hydra-defined-indcator
                        t))
                   ,rtn)
               (let (_)
                 (defun ,body-adfunc-name
                     (orig-func &rest orig-args)
                   ,(format "Around advice for `%s' to show with posframe if available."
                            body-adfunc-name)
                   (let* (,@entropy/emacs-pretty-hydra--hydra-hints-let-env)
                     (setq entropy/emacs-pretty-hydra-posframe-visible-p t)
                     (apply orig-func orig-args)))
                 (advice-add ',body-func-name
                             :around
                             ',body-adfunc-name))))
      rtn))
  (advice-add 'pretty-hydra--generate
              :around
              #'entropy/emacs-pretty-hydra--patch-2)

;; **** end
  )

;; *** major-hydra

(use-package major-mode-hydra
  :commands
  (
   major-mode-hydra-define
   major-mode-hydra-define+
   major-mode-hydra
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
    :foreign-keys warn
    :color ambranth
    :quit-key "q"
    :separator "."))

;; ** wgrep

(use-package wgrep
  :commands
  (wgrep-save-all-buffers
   wgrep-exit
   wgrep-change-to-wgrep-mode
   wgrep-finish-edit
   wgrep-toggle-readonly-area
   wgrep-remove-all-change
   wgrep-mark-deletion
   wgrep-remove-change
   wgrep-abort-changes)
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; ** edit-indirect
(use-package edit-indirect
  :commands
  (edit-indirect-save
   edit-indirect-abort
   edit-indirect-commit
   edit-indirect-region)
  :config
  (entropy/emacs-make-function-inhibit-readonly
   'edit-indirect-commit))


;; ** Benchmark

(use-package benchmark-init
  :ensure nil
  :commands
  (benchmark-init/activate
   benchmark-init/deactivate))
(use-package benchmark-init-modes
  :ensure nil
  :commands
  (benchmark-init/show-durations-tabulated
   benchmark-init/show-durations-tree))

;; * provide
(provide 'entropy-emacs-utils)
