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

;; ** xdg
(use-package xdg
  :ensure nil
  :eemacs-functions
  (xdg-config-home
   xdg-cache-home))

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
  :eemacs-functions (advice--patch advice-patch)
  :config
  ;; we should kill the src buffer for preventing user's misbehave on
  ;; that src file after emacs inited for thus.
  (defun __ya/advice-patch/kill-def-buffer-after-patched
      (fn &rest args)
    (unwind-protect (apply fn args)
      (when-let* ((fb (ignore-errors
                        (find-function-noselect (car args) 'lisp-only)))
                  (buff (car fb)))
        (entropy/emacs-dynamic-let* (kill-buffer-hook)
          (kill-buffer buff)))))
  (advice-add 'advice-patch
              :around
              #'__ya/advice-patch/kill-def-buffer-after-patched))

;; ** async

(use-package async
  :eemacs-functions (async-start)
  :commands (async-byte-compile-file
             async-bytecomp-package-mode
             async-shell-command)
  :init
  ;; FIXME: disable below option since it use
  ;; `tramp-password-prompt-regexp' to match the process buffer
  ;; content in process filter which may cause the stack overflow for
  ;; regex matching while thus of piece of content is huge.
  (setq async-prompt-for-password nil)
  :config
  (defun __ya/async-start-process/suggest-read-only-for-proc-buffer
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (when-let* ((proc rtn)
                  ((and (processp proc) (process-live-p proc)))
                  (proc-buff (process-buffer proc))
                  ((and (bufferp proc-buff) (buffer-live-p proc-buff))))
        (with-current-buffer proc-buff
          (setq entropy/emacs-should-be-read-only t))
        (when-let* ((proc-buff-stderr (entropy/emacs-process-stderr-buffer proc))
                    ((and (bufferp proc-buff-stderr)
                          (buffer-live-p proc-buff-stderr))))
          (with-current-buffer proc-buff-stderr
            (setq entropy/emacs-should-be-read-only t))))
      ;; orig return
      rtn))
  (advice-add 'async-start-process
              :around
              #'__ya/async-start-process/suggest-read-only-for-proc-buffer))

;; ** memoize
(use-package memoize
  :commands
  (memoize
   memoize-restore
   memoize-by-buffer-contents--wrap
   memoize--wrap
   memoize-by-buffer-contents))


;; ** trie

(entropy/emacs--inner-use-package trie
  ;; inhibit require when byte-compile since it relying on an old
  ;; compat package `tNFA' which use obsolete cl-* functions, while
  ;; many obsolete warnings will popup when in compilation.
  :eemacs-with-no-require t
  :eemacs-with-permanently-defer t
  :eemacs-functions
  (make-trie
   ;; FIXME: we need to auto load `trie--create' since it's not edited
   ;; as autoloaded by `trie' maintainer in source, thus emacs can not
   ;; find api `make-trie(-*)?' autoloads definations since they are
   ;; alias of `trie--create' which is an `defstruct' auto generated
   ;; function. (i.e. the if we autoload `make-trie' but it's still
   ;; not work since its just declared as an alias in autoload file.)
   ;;
   ;; EEMACS_MAINTENANCE: push a bug issue for upstream.
   trie--create
   trie-insert trie-complete))

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

;; ** icon
;; *** nerd icons

(use-package nerd-icons
  :commands
  (nerd-icons-icon-for-dir
   nerd-icons-auto-mode-match?
   nerd-icons-faicon-data
   nerd-icons-dir-is-submodule
   nerd-icons-insert-faicon
   nerd-icons-insert-icons-for
   nerd-icons-insert-octicon
   nerd-icons-octicon-data
   nerd-icons-faicon-family
   nerd-icons-wicon-family
   nerd-icons-wicon
   nerd-icons-match-to-alist
   nerd-icons-icon-for-file
   nerd-icons-icon-for-mode
   nerd-icons-insert
   nerd-icons-icon-for-weather
   nerd-icons-faicon
   nerd-icons-insert-wicon
   nerd-icons-install-fonts
   nerd-icons-icon-for-url
   nerd-icons-wicon-data
   nerd-icons-icon-for-buffer
   nerd-icons-octicon
   nerd-icons-octicon-family
   )
  :config
  ;; Set tree-sitter variant prog-modes' icon as what their traditional modes have
  (let (tr-fnm oc)
    (dolist (m entropy/emacs-ide-for-them)
      (unless (alist-get m nerd-icons-mode-icon-alist)
        (when (entropy/emacs-setf-by-body tr-fnm
                (and (eemacs/prog-lang/func/mode/treesit-mode-p m)
                     (ensure-list (eemacs/prog-lang/func/mode/prog-modes m))))
          (catch :exit
            (dolist (el tr-fnm)
              (when (setq oc (alist-get el nerd-icons-mode-icon-alist))
                (push (cons m oc) nerd-icons-mode-icon-alist)
                (throw :exit nil))))))))
  )

;; ** eldoc
;; *** Core
(use-package eldoc
  :ensure nil
  :commands (eldoc eldoc-mode global-eldoc-mode)
  :eemacs-functions (eldoc-minibuffer-message)
  :init

  ;; Use union IDE like doc delay setting
  (setq eldoc-idle-delay entropy/emacs-ide-doc-delay)

  ;; ---------- Temporally eldoc-mode patch
  (entropy/emacs-defvar-local-with-pml entropy/emacs-eldoc-inhibit-in-current-buffer nil)
  (defun entropy/emacs-eldoc-inhibit-around-advice
      (orig-func &rest orig-args)
    "Around advice for disable `eldoc-mode' with the idlle time
of `eldoc-idle-delay' after excute the ORIG-FUNC."
    (unwind-protect
        (apply orig-func orig-args)
      (and (setq entropy/emacs-eldoc-inhibit-in-current-buffer t)
           nil)))

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
  (cond ((and (entropy/emacs-custom-enable-lazy-load/val)
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
  (defun entropy/emacs-posframe-adapted-p (&optional frame)
    "Judge whether posframe can be used in current emacs session."
    (or
     (> emacs-major-version 30)
     (and (not (version< emacs-version "26.1"))
          (display-graphic-p frame))))
  (entropy/emacs-with-daemon-make-frame-done
    'delete-all-posframe-frames-for-cli nil
    "Remove all posframe child-frames for cli session since they may
hang thus (i.e. focus missed in)"
    :when-tui
    (posframe-delete-all)))

;; ** popup

(use-package popup
  :commands (popup-tip))

;; ** transient

(use-package transient)

;; ** lv

(use-package lv
  :config
  (entropy/emacs-api-restriction/elpkg-eemacs-ext-stable-build-repo-version
      'lv--patch
    :do-error t
    :elpkg-eemacs-ext-stable-build-repo-version "3.2.0"
    (defun entropy/emacs--lv-delete-window ()
      "Advice for `lv-delete-window' for undeletable lv buffer window when it
was split from a side window such as from a treemacs scope window."
      (when (window-live-p lv-wnd)
        (let ((buf (window-buffer lv-wnd)))
          (condition-case err (delete-window lv-wnd)
            (error
             (let (ok)
               (when (and (window-live-p lv-wnd) (window-parameter lv-wnd 'window-side))
                 (set-window-parameter lv-wnd 'window-side nil)
                 (setq ok t))
               (if ok (delete-window lv-wnd)
                 (signal 'error err)))))
          (and (buffer-live-p buf) (kill-buffer buf)))))
    (advice-add 'lv-delete-window
                :override 'entropy/emacs--lv-delete-window))

  (entropy/emacs-api-restriction/elpkg-eemacs-ext-stable-build-repo-version
      'lv-window-redef
    :do-error t :elpkg-eemacs-ext-stable-build-repo-version "3.2.0"
    (entropy/emacs-!cl-defun entropy/emacs--lv-window-override ()
      "The override advice for `lv-window' since its buggy of using
`switch-to-buffer' to set buffer of `lv-wnd' which may cause origin
`selected-window' not lived any more since it uses `pop-to-buffer' to
handle display actions which not guarantee the original window
layout. In other hand, `switch-to-buffer' is prefer to be used
interactively which is not a good taste to used in progs."
      (if (window-live-p lv-wnd) lv-wnd
        (let ((ori (selected-window)) buf)
          (prog1 (setq lv-wnd
                       (select-window
                        (let ((ignore-window-parameters t))
                          (split-window
                           (frame-root-window) -1 'below))
                        'norecord))
            (if (setq buf (get-buffer " *LV*"))
                (set-window-buffer lv-wnd buf)
              (set-window-buffer
               lv-wnd (setq buf (get-buffer-create " *LV*" t))))
            (with-current-buffer buf
              (fundamental-mode)
              (set-window-hscroll lv-wnd 0)
              (setq window-size-fixed t)
              (setq mode-line-format nil)
              (setq header-line-format nil)
              (setq tab-line-format nil)
              (setq cursor-type nil)
              (setq display-line-numbers nil)
              (setq display-fill-column-indicator nil)
              (set-window-dedicated-p lv-wnd t)
              (set-window-parameter lv-wnd 'no-other-window t)
              (run-hooks 'lv-window-hook))
            (unless (window-live-p ori)
              (entropy/emacs-!error-as-eemacs-internal-error
               "orig win %s not lived anymore" ori))
            (select-window ori 'norecord)))))
    (advice-add 'lv-window :override #'entropy/emacs--lv-window-override))

  ;; EEMACS_MAINTENANCE: inhibit wrapper for lv core subroutine for preventing from session messy/stuck/buggy
  (defun entropy/emacs--lv-window-inhibit-quit (ofunc &rest oargs)
    (let ((inhibit-quit t)) (apply ofunc oargs)))
  (advice-add 'lv-window :around #'entropy/emacs--lv-window-inhibit-quit)

  (defun entropy/emacs--lv-message-inhibit-quit (ofunc &rest oargs)
    (let ((inhibit-quit t))
      (entropy/emacs-unwind-protect-unless-success
          (apply ofunc oargs)
        (lv-delete-window))))
  (advice-add 'lv-message :around #'entropy/emacs--lv-message-inhibit-quit)

  )

;; ** hydra
;; *** hydra core
(entropy/emacs--inner-use-package hydra
  :eemacs-macros (defhydra)
  :eemacs-functions (hydra-default-pre
                     entropy/emacs-utils-hdyra-displayed-p
                     entropy/emacs-utils-cmd-call-from-hydra-p)
  :defines entropy/emacs-utils--cmd-call-from-hydra-p
  :init
  ;; Fix '[]' as key stroke in hydra doc init refer to
  ;; https://github.com/abo-abo/hydra/issues/365#issue-574484394
  ;;
  ;; This will remove when update hydra package.
  ;;
  ;; (setq hydra-key-regex
  ;;       "[][\\[:alnum:] ~.,;:/|?<>={}*+#%@!&^↑↓←→⌫⌦⏎'`()\"$-]+?")

  :config
;; **** core patch
;; ***** display/delete indicator

  (defvar entropy/emacs-utils--hydra-dlpi-alist nil)
  (defun entropy/emacs-utils-hdyra-displayed-p (&optional frame)
    "Return non-nil when FRAME has a displayed `hydra' dashboard.

FRAME defaults to `selected-frame'."
    (let (prn)
      (dolist (el entropy/emacs-utils--hydra-dlpi-alist)
        (if (frame-live-p (car el)) (push el prn)))
      (when prn
        (setq entropy/emacs-utils--hydra-dlpi-alist (nreverse prn))
        (alist-get (or frame (selected-frame))
                   entropy/emacs-utils--hydra-dlpi-alist))))
  (defun entropy/emacs-utils--hydra-set-display-indicator (&rest _)
    (let (_)
      (entropy/emacs-alist-set (selected-frame)
          entropy/emacs-utils--hydra-dlpi-alist
        t)))
  (advice-add 'hydra-show-hint
              :before #'entropy/emacs-utils--hydra-set-display-indicator)

  (defvar entropy/emacs-utils--hydra-reset-display-indicator/is-ran-p nil)
  (defun entropy/emacs-utils--hydra-reset-display-indicator (&rest args)
    (entropy/emacs-when-let*-first
        (((not entropy/emacs-utils--hydra-reset-display-indicator/is-ran-p))
         (frame (selected-frame))
         ;; prevent nested invocation since messy usage of `advice'
         (entropy/emacs-utils--hydra-reset-display-indicator/is-ran-p t)
         func)
      (prog1 (when args (apply (car args) (cdr args)))
        (entropy/emacs-setf-by-body func
          (lambda nil
            (when (entropy/emacs-utils-hdyra-displayed-p frame)
              (entropy/emacs-alist-set frame
                  entropy/emacs-utils--hydra-dlpi-alist
                nil))))
        (if (eq this-command 'keyboard-quit)
            ;; arrange the rester into a idle state to prevent occasions
            ;; from while this not the last processor of a command loop
            ;; i.e. set after any steps ran out in this thread so that we
            ;; can gurantee that the
            ;; `entropy/emacs-utils-hdyra-displayed-p''s judgement is valid
            ;; in all of that time.
            (run-with-idle-timer 0.01 nil func)
          (funcall func)))))
  (advice-add 'hydra-keyboard-quit :around
              #'entropy/emacs-utils--hydra-reset-display-indicator)
  (entropy/emacs--api-restriction-uniform 'hydra--clearfun-hack
      'package-version-incompatible
    :do-error t
    :detector
    (not (version=
          "3.2.0"
          (substring
           entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           1)))
    :signal
    (signal
     entropy/emacs-package-version-incompatible-error-symbol
     (list 'entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           "require: v3.2.0"))
    (defun __ya/hydra--clearfun/eemacs-hydra-display-indicator-reset ()
      "The overriden advice for `hydra--clearfun' but take same codec
content as origin but inject eemacs hydra display inidcator reset
procedure."
      (unless (eq this-command 'hydra-pause-resume)
        (when (or
               (memq this-command '(handle-switch-frame keyboard-quit))
               (null overriding-terminal-local-map)
               (not (or (eq this-command
                            (lookup-key hydra-curr-map (this-single-command-keys)))
                        (cl-case hydra-curr-foreign-keys
                          (warn (setq this-command 'hydra-amaranth-warn))
                          (run t) (t nil)))))
          (prog1 (hydra-disable)
            (entropy/emacs-utils--hydra-reset-display-indicator)))))
    (advice-add 'hydra--clearfun
                :override
                #'__ya/hydra--clearfun/eemacs-hydra-display-indicator-reset))

  (define-advice hydra-disable
      (:around (ofunc &rest oargs) eemacs-advice//hidra-disable-in-emacs-TUI-posframe)
    "Advice for follow `entropy/emacs-select-frame/insist-invisible-state'."
    (let ((entropy/emacs-select-frame/insist-invisible-state t))
      (apply ofunc oargs)))

;; ***** call interactively indicator

  (entropy/emacs-defconst/only-allow/local
    entropy/emacs-utils--cmd-call-from-hydra-p nil)
  (defun entropy/emacs-utils-cmd-call-from-hydra-p ()
    "Return non-nil when in context within an command call from a hydra
keymap."
    (and entropy/emacs-utils--cmd-call-from-hydra-p t))
  (defun entropy/emacs-utils--cmd-call-from-hydra-adv (ofunc &rest oargs)
    (let ((form (apply ofunc oargs)))
      (entropy/emacs-setf-by-body form
        `(let ((entropy/emacs-utils--cmd-call-from-hydra-p t))
           ,form))))
  (advice-add 'hydra--call-interactively
              :around
              #'entropy/emacs-utils--cmd-call-from-hydra-adv)

;; **** __end___
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
     (entropy/emacs-posframe-adapted-p)
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

  (defun __adv/around/hydra-posframe-show/reset-internal-border
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
              #'__adv/around/hydra-posframe-show/reset-internal-border)

  (defun __adv/around/hydra-posframe-show/set-eemacs-pretty-hdydra-posframe-indicator
      (ofunc &rest oargs)
    (setq entropy/emacs-pretty-hydra-posframe-visible-p t)
    (apply ofunc oargs))
  (advice-add 'hydra-posframe-show
              :around
              #'__adv/around/hydra-posframe-show/set-eemacs-pretty-hdydra-posframe-indicator)

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
    (entropy/emacs-require-only-once 'posframe)
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
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(cl-defun entropy/emacs-pretty-hydra-make-title-for-major-mode-common
    (mode title-str &key face height v-adjust)
  (entropy/emacs-require-only-once 'faces)
  (let* ((icon-display-p (entropy/emacs-icons-displayable-p))
         (face (or face 'entropy/emacs-defface-face-for-hydra-orange-face))
         (icon (if (fboundp 'nerd-icons-icon-for-mode)
                   (ignore-errors
                     (nerd-icons-icon-for-mode
                      mode
                      :face face :height (or height 1)
                      :v-adjust (or v-adjust 0)))
                 (when icon-display-p
                   (error "Function <nerd-icons-icon-for-mode> not found!")))))
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


;; ** editorconfig

(use-package editorconfig
  :commands
  (editorconfig-version
   editorconfig-mode
   editorconfig-conf-mode
   editorconfig-format-buffer
   editorconfig-apply)
  :eemacs-functions (editorconfig-get-properties)
  :preface
  (entropy/emacs-!cl-defun
      entropy/emacs--editorconfig-get-properties (fname)
    (cond
     ;; EEMACS_MAINTENANCE: for `editorconfig' version >= 0.11.0
     ((fboundp 'editorconfig-call-get-properties-function)
      (editorconfig-call-get-properties-function fname))
     ((fboundp 'editorconfig-get-properties)
      (editorconfig-get-properties fname))
     (t
      (entropy/emacs-!error-as-eemacs-internal-error
       "no `editorconfig-get-properties' or \
`editorconfig-call-get-properties-function' fboundp"))))
  :init
  (entropy/emacs-lazy-with-load-trail 'editorconf-mode-init
    :start-end t :pdumper-no-end t
    (editorconfig-mode t))

  (dolist (prop entropy/emacs-editor-convention-properties)
    (entropy/emacs-editor-convention-register-property-value
     prop
     (entropy/emacs-defalias
         (intern (format "entropy/emacs-editorconfig-get-prop-value/%s" prop))
       (lambda (&optional buffer)
         (when-let* ((buff (or buffer (current-buffer)))
                     (buff-fname (buffer-file-name buff))
                     (hash (entropy/emacs--editorconfig-get-properties buff-fname))
                     ((not (zerop (hash-table-size hash))))
                     (val (gethash prop hash))) val))
       (format "Return `editorconfig-mode' prop `%s' value for \
buffer BUFFER (defaults to `current-buffer'), or nil if not has
one of so." prop))))

  :config
  ;; add missing indentation guides
  ;; (unless (alist-get 'js-ts-mode editorconfig-indentation-alist)
  ;;   (add-to-list 'editorconfig-indentation-alist
  ;;                '(js-ts-mode js-indent-level)))
  )

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

;; ** simple-httpd
(use-package simple-httpd
  :init
  ;; always use servlets to enlarge its greatest functions
  (setq httpd-servlets t)
  :config
  (defvar entropy/emacs-httpd-stop-anyway nil
    "Cause `httpd-stop' without warning prompt and interactive
confirmation for existence.")
  (defun __ya/httpd-stop/safe (orig-func &rest orig-args)
    "Advice for `httpd-stop' adapting to eemacs spec where aim to
safely stop the existed running server.

Var `entropy/emacs-httpd-stop-anyway' when non-nil, origin
function used anyway."
    (when (and (not entropy/emacs-httpd-stop-anyway)
               (not noninteractive) (httpd-running-p))
      (unless (yes-or-no-p "there's one running httpd existed, really stop it? \
(which will breaking other outer connection to this server instance.)")
        (user-error "Abort!")))
    (apply orig-func orig-args))
  (advice-add 'httpd-stop :around '__ya/httpd-stop/safe)

  (defvar entropy/emacs-httpd-start-anyway nil
    "Cause `httpd-start' without warning prompt and interactive
confirmation for existence.")
  (defun __ya/httpd-start/safe (orig-func &rest orig-args)
    "Advice for `httpd-start' adapting to eemacs spec where aim to
safely stop the existed running server before start the new,
where the safety warning only occurred in `interactive' case,
otherwise do nothing when running httpd server existed.

Eemacs advicing for behaving as this, since package
`simple-httpd' is capable of using all as servelets and its root
directory can be dynamic binding whenever user want to
change. Thus there's no need to frequently restart a new server
in current emacs session, that's meaningless that most directly
invocation of `http-start' is just need for when
`httpd-running-p' is return nil.

Var `entropy/emacs-httpd-start-anyway' when non-nil, origin
function used anyway."
    (let ((rn (httpd-running-p))
          (ct entropy/emacs-httpd-start-anyway))
      (when (and (not ct)
                 (called-interactively-p 'interactive) rn)
        (unless (setq ct (yes-or-no-p "there's one running httpd existed, \
really stop it before start a new one? \
(which will breaking other outer connection to this server instance.)"))
          (user-error "Abort!")))
      (when (or ct (not rn))
        (let ((entropy/emacs-httpd-stop-anyway t))
          (apply orig-func orig-args)))))
  (advice-add 'httpd-start :around '__ya/httpd-start/safe)

  (defun entropy/emacs-utils--httpd-start (ofunc &rest oargs)
    (unless (httpd-running-p)
      (setq httpd-port (entropy/emacs-get-available-sys-network-port httpd-port)))
    (apply ofunc oargs))
  (advice-add 'httpd-start :around #'entropy/emacs-utils--httpd-start)

  )

;; ** xclip-mode

(use-package xclip
  :commands (xclip-mode)
  :eemacs-functions
  (xclip-set-selection
   xclip-get-selection)
  :config
  (defun __adv/xclip-with-tmpdir-as-default-directory
      (fn &rest args)
    "Advice for xclip's `call-process' and `start-process' to handle
invocation with system tmpfs as `default-directory' to avoid
after while the process looking for a non-exited directory any
more."
    (let ((default-directory
           entropy/emacs-system-temporary-file-directory))
      (apply fn args)))
  (dolist (fn (list 'xclip-set-selection 'xclip-get-selection))
    (advice-add fn :around
                #'__adv/xclip-with-tmpdir-as-default-directory)))

;; ** treesit

(entropy/emacs--inner-use-package treesit
  :eemacs-if (>= emacs-major-version 30)
  :ensure nil
  :defines (outline-regexp
            outline-search-function)
  :config
  (setq-default
   treesit-outline-predicate
   (entropy/emacs-!cl-defun eemacs//default-treesit-outline-predicate (node)
     "The default `treesit-outline-predicate' in eemacs.

This function exists to implement more accurately
`outline-search-function' for treesit variant `major-mode's other than
using the emacs internal one i.e. `treesit-simple-imenu-settings' which
is not too much suitable for eemacs outline integration."
     (and (equal "comment" (treesit-node-type node))
          (entropy/emacs-save-excurstion-and-mark-and-match-data
            (goto-char (pos-bol))
            (re-search-forward outline-regexp (pos-eol) t))
          t)))
  (define-advice outline-next-visible-heading
      (:around (ofunc &rest oargs)
               eemacs//treesit-outline-next-visible-heading)
    "EEMACS_TEMPORALLY_HACK: Temporarily disable `outline-search-function' of
treesit variants mode for hacking its infinitely looping searching next
outline heads start at a invisible heading line"
    (if
        (and (eemacs/prog-lang/func/bof/treesit-id (current-buffer))
             (or
              (entropy/emacs-save-excurstion-and-mark-and-match-data
                (goto-char (pos-bol))
                (not (looking-at-p outline-regexp)))
              (entropy/emacs-save-excurstion-and-mark-and-match-data
                (goto-char (pos-eol))
                (outline-invisible-p))))
        (let ((outline-search-function nil))
          (apply ofunc oargs))
      (apply ofunc oargs)))
  )

;; * provide
(provide 'entropy-emacs-utils)
