;;; entropy-emacs-ivy.el ---  entropy emacs 'M-x' enhancement
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ivy.el
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
;; `entropy-emacs' using [[https://github.com/abo-abo/swiper][ivy]] framework to enhance the emacs command
;; completion interface 'M-x'.
;;
;; Based on this framework, some usefule implementation for
;; sub-feature also used in `entroy-emacs', the main one was
;; =counsel= the collection of ivy stuffs.
;;
;; The other 'M-x' ehancement framework was [[https://github.com/emacs-helm/helm][helm]], using independent
;; buffer to show the command completion unlike what did in ivy which
;; uses the mini-buffer to do so. `entropy-emacs' using =helm= to do
;; the project greping/replacing, the main reason for that the
;; mini-bufer was not adapt to do heavy things due to ivy
;; designation. As thus, the 'ag' and 'pt' eventhough the ripgrep
;; 'rg' are using helm interface in `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-utils)

;; ** ivy
(use-package ivy
  :defines (ivy-mode-hook)
  :diminish ivy-mode
  :commands (ivy-mode ivy-switch-buffer ivy-resume)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))

;; *** ivy init
  :init

  ;;  disabled ivy-initial-inputs-alist

  ;; ivy initial char inserting was using for regex like searching,
  ;; and it's also be '^' for ahead searching, but when you want to
  ;; searching no limited in ahead type we must force disable it.
  (setq ivy-initial-inputs-alist nil) ; disable "^" for heading begining search

  ;; ivy details
  (setq enable-recursive-minibuffers nil) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-dynamic-exhibit-delay-ms 2) ;prevent immediacy dnynamic process fetching crash.

  ;; using fuzzy matching
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          ;; using elisp regex match candidates
          (t . ivy--regex)))

  (setq swiper-action-recenter t)

;; *** ivy config
  :config

  (defun entropy/emacs-ivy--ivy-read-action-after-advice (&rest args)
    "Interrupting rest process when `this-command' was
`ivy-read-action'."
    (catch 'ivy-quit
      (if (or (eq this-command 'ivy-dispatching-done)
              (eq this-command 'ivy-occur)
              (string-match-p "^ivy-read-action/" (symbol-name this-command)))
          (progn
            (user-error "Ivy-quit!")
            (throw 'ivy-quit "Ivy-quit!")))))
  (advice-add 'ivy-read :after #'entropy/emacs-ivy--ivy-read-action-after-advice)

  ;; top level for ivy-mode-map
  (entropy/emacs-lazy-load-simple ivy
    (define-key ivy-mode-map (kbd "ESC ESC") 'top-level))


  ;;; *Redefine the ivy-partial-or-done to prevent double click '<tab>'*
  ;; This portion give the minor changed for disabled double tab in
  ;; ivy completion for preventing accidental operation of double hint
  ;; for 'TAB'.

  (defun ivy-partial-or-done ()
    "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
    (interactive)
    (cond
     ((and completion-cycle-threshold (< (length ivy--all-candidates) completion-cycle-threshold))
      (let ((ivy-wrap t))
        (ivy-next-line)))
     ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (or (and (equal ivy--directory "/")
                    (string-match-p "\\`[^/]+:.*\\'" ivy-text))
               (= (string-to-char ivy-text) ?/)))
      (let ((default-directory ivy--directory)
            dir)
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (setq dir (ivy-expand-file-if-directory ivy-text))
          (ivy--cd dir))))
     (t
      (or (ivy-partial)
          (when (or (eq this-command last-command)
                    (eq ivy--length 1))
            (message ":) evil smile ^v^"))))))

  )



;; ** ivy hydra
;; Additional key bindings for Ivy
(use-package ivy-hydra
  :init
  (when (version< "26" emacs-version)
    ;; `ivy-dispatching-done' can not display minibuffer hint prompt
    ;; when emacs version upper than 26. see
    ;; https://github.com/abo-abo/swiper/issues/2397 for details
    (entropy/emacs-lazy-load-simple ivy
      (require 'ivy-hydra)
      (when (fboundp 'ivy-hydra-read-action)
        ;; using new ivy hydra api in latest ivy update version
        (setq ivy-read-action-function #'ivy-hydra-read-action)))))

;; ** swiper

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-M-s" . swiper-all)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :init
  (entropy/emacs-lazy-load-simple swiper
    (ivy-mode +1))

  :config
  ;; ==assign format-function to swiper for fix some-bug==

  ;; when using `all-the-icons-dired' with swiper, icons displayed in
  ;; associate dired-buffer are mapped with all-the-icons spec fonts
  ;; which can not rendered correctly for other fonts. ivy's defaut
  ;; format function `ivy-format-function-default' used for `swiper'
  ;; which using `identify' to format inactive candis which using the
  ;; default face as font-lock atribtue, it will corrupts the
  ;; correctly font displaying when set spec font to this default
  ;; face.

  (entropy/emacs-lazy-load-simple all-the-icons-dired
    (defun entropy/emacs-ivy--swiper-format-function-for-dired (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (ivy--add-face str 'ivy-current-match))
       (lambda (str)
         (cond
          ((eq major-mode 'dired-mode)
           (ivy--add-face str 'entropy/emacs-defface-face-for-swiper-dired-candi-inactive-face))
          (t
           (identity str))))
       cands
       "\n"))
    (add-to-list 'ivy-format-functions-alist
                 (cons 'swiper #'entropy/emacs-ivy--swiper-format-function-for-dired))))

;; ** counsel
(use-package counsel
  :diminish counsel-mode
  :commands (counsel-mode
             counsel-linux-app)

;; *** bind-key
  :bind (("M-x"     . counsel-M-x)
         ("C-c M-t" . entropy/emacs-ivy-counsel-load-theme)
         ("C-c g"   . counsel-git)
         ("C-x d"   . counsel-dired)
         ("C-x C-f" . counsel-find-file)
         ("C-h a"   . counsel-apropos)
         ("C-h P"   . counsel-package)
         ("C-h v"   . counsel-describe-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-h l"   . counsel-find-library)
         ("C-x j"   . counsel-mark-ring)
         ("C-x C-t" . counsel-find-file-extern)
         ("C-c M-b" . counsel-recentf)
         ("C-c M-k" . counsel-yank-pop)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper))

;; *** entropy/emacs-top-keymap

  :eemacs-tpha
  (((:enable t))
   ("Utils"
    (("u c"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'counsel-mode))
      "Counsel Stuffs Map"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t)
    (counsel-mode (counsel counsel-mode-map) nil (2 2 2)))
   (
    ;; FILE group -- prefix 'f'
    "Counsel File Manipulation"
    (("C-c c f b" counsel-buffer-or-recentf "Open Recentf buffer or File"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f f" counsel-fzf "Open a file using the fzf shell command"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f l" counsel-locate "Call the "locate" shell command."
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f m b" counsel-bookmark "View Emacs Bookmarks"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f m d" counsel-bookmarked-directory "View Emacs Bookmarked Directories"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f o" counsel-switch-buffer-other-window "Switch buffer to other window"
      :enable t :exit t :eemacs-top-bind t))

    ;; GIT group -- prefix 'g'
    "Counsel Git Manipulation"
    (("C-c c g c" counsel-git-checkout "Call the \"git checkout\" command"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c g g" counsel-git-grep "Grep for a string in the current Git repository"
      :enable t :exit t :eemacs-top-bind t))

    ;; GREP group -- prefix 's'
    "Counsel Grep Manipulation"
    (("C-c c s g" counsel-grep "Grep for a string in the file visited by the current buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s a" counsel-ag "Grep for a string in the current directory using ag"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s p" counsel-pt "Grep for a string in the current directory using pt"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s r" counsel-rg "Grep for a string in the current directory using rg"
      :enable t :exit t :eemacs-top-bind t))

    ;; Misc.Emacs group -- prefix 'e'
    "Counsel Emacs Manipulation"
    (("C-c c e /" counsel-el "Elisp completion at point"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e c" counsel-faces "Complete faces with preview"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e f" counsel-fonts "Show a list of all supported font families"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e h" counsel-command-history "Show the history of commands"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e i" counsel-imenu "Jump to a buffer position indexed by imenu"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e l" counsel-load-library "Load a selected the Emacs Lisp library"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e m" counsel-minor "Enable or disable minor mode"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e p" counsel-package "Install or delete packages"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e s" counsel-list-processes "Offer completion for 'process-list'"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e t" entropy/emacs-ivy-counsel-load-theme "Load specific emacs theme"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e u" counsel-unicode-char "Insert COUNT copies of a Unicode character at point"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e v" counsel-set-variable "Set a variable SYM, with completion"
      :enable t :exit t :eemacs-top-bind t))

    ;; TODO Misc.OS group -- prefix 'o'
    ;; --------------------------------
    ;; TODO org grep
    ;; -------------
    ;; MISCELLANEOUS group
    "Counsel Miscellaneous"
    (("C-c c m e" counsel-colors-emacs
      "Show a list of all supported colors for a particular frame"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m m" counsel-minibuffer-history "Browse minibuffer history"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m o" counsel-outline "Jump to an outline heading with completion"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m w" counsel-colors-web "Show a list of all W3C web colors for use in CSS"
      :enable t :exit t :eemacs-top-bind t))))

;; *** hooks
  :hook ((ivy-mode . counsel-mode))

;; *** init
  :init

  (setq counsel-find-file-at-point nil)
  (setq counsel-yank-pop-separator
        "\n────────\n")

  (entropy/emacs-lazy-load-simple counsel
    (ivy-mode +1))

  (unless sys/win32p
    (entropy/emacs-!set-key (kbd "<M-up>") #'counsel-linux-app))

;; **** counsel-git with utf-8

  (advice-add 'counsel-git :around
              #'entropy/emacs-lang-use-utf-8-ces-around-advice)

;; *** config
  :config
;; **** reset `ivy-initial-inputs-alist'
  (setq ivy-initial-inputs-alist nil)

;; **** advice counsel--M-x-externs for it's bad lagging perfomance

  ;; because `counsel--M-x-externs' has the `require' function for
  ;; it's contained condition context so it will lagging like previous
  ;; version of `ivy--add-face'.
  (defun entropy/emacs-ivy--counsel--M-x-externs ()
    nil)
  (advice-add 'counsel--M-x-externs
              :override
              #'entropy/emacs-ivy--counsel--M-x-externs)

;; **** windows not to use grep because there's no grep in windows
  (when sys/win32p
    (defun entropy/emacs-ivy-counsel-grep-or-swiper (orig-func &rest orig-args)
      (interactive)
      (apply 'swiper orig-args))
    (advice-add 'counsel-grep-or-swiper
                :around
                #'entropy/emacs-ivy-counsel-grep-or-swiper))

;; **** counsel-load-theme
  (defun entropy/emacs-ivy-counsel-load-theme ()
    "Load theme with some stuffs patching."
    (interactive)
    (counsel-load-theme))

;; **** counsel-locate
  (when (and sys/win32p entropy/emacs-wsl-enable)
    (defun counsel-locate (&optional initial-input)
      "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

Note: This function has been modified for transfer volum's type
of msys2 or other window-gnu-enviroment to windows origin volum
type by function `entropy/emacs-transfer-wvol'"
      (interactive)
      (ivy-read "Locate: " #'counsel-locate-function
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-locate-history
                :action (lambda (file)
                          (when file
                            (with-ivy-window
                              (entropy/emacs-transfer-wvol file))))
                :unwind #'counsel-delete-process
                :caller 'counsel-locate)))

;; **** redefine counsel-git

  (setq counsel-git-cmd "git ls-files --full-name --")

  (defvar entropy/emacs-ivy-counsel-git-root nil
    "Temporally variable storing git repository root dir,
this variable used to patching for origin `counsel-git'.")

  (defun counsel-git-cands (&rest _)
    (let ((default-directory (counsel-locate-git-root)))
      (setq entropy/emacs-ivy-counsel-git-root default-directory)
      (split-string
       (shell-command-to-string counsel-git-cmd)
       "\n"
       t)))

  (defun counsel-git-action (x)
    "Find file X in current Git repository.

    Note: this function has been modified by entropy-emacs because of:

    ivy version 0.11.0 and counsel version 0.11.0 has the bug that
    using wrong root-dir for find git repo's file that will cause file
    not existed error and creating new buffer with that actual name,
    this problem caused by origin `counsel-git-action' using
    `ivy-last''s directory slot as the default diretory on the 0.11.0
    version of ivy framework updating.
    "
    (with-ivy-window
      (let ((default-directory entropy/emacs-ivy-counsel-git-root))
        (find-file x)))))

;; *** use counsel css for quickly search css selector
(use-package counsel-css
  :after css-mode
  :hook (css-mode . counsel-css-imenu-setup)
  :eemacs-mmphca
  (((:enable t)
    (css-mode (css-mode css-mode-map)))
   ("Basic"
    (("C-c M-d" counsel-css "Jump to a css selector"
      :enable t
      :exit t
      :map-inject t))))

  :init
  (when entropy/emacs-fall-love-with-pdumper
    (require 'css-mode)))


;; *** use ivy-xref for quickly find defination and reference
(use-package ivy-xref
  :commands (ivy-xref-show-xrefs)
  :init (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

;; *** use display world clock
(use-package counsel-world-clock
  :commands  (counsel-world-clock)
  :eemacs-indhca
  (((:enable t)
    (counsel-mode (counsel counsel-mode-map)))
   ("Counsel Miscellaneous"
    (("C-c c m c" counsel-world-clock "Display time in different time zone in echo area"
      :enable t :exit t :eemacs-top-bind t)))))

;; *** use firefox bookmarks and history query and open
(use-package counsel-ffdata
  :commands (counsel-ffdata-firefox-bookmarks
             counsel-ffdata-firefox-history)

  :eemacs-indhca
  (((:enable t)
    (counsel-mode (counsel counsel-mode-map)))
   ("Counsel Miscellaneous"
    (("C-c c m f b" counsel-ffdata-firefox-bookmarks "Search your Firefox bookmarks"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m f h" counsel-ffdata-firefox-history "Search your Firefox history"
      :enable t :exit t :eemacs-top-bind t))))

  :init
  (setq counsel-ffdata-database-path
        (ignore-errors
          (cl-case system-type
            ((gnu gnu/linux gnu/kfreebsd)
             (expand-file-name
              (car (file-expand-wildcards
                    "~/.mozilla/firefox/*.default-release/places.sqlite"))))
            (windows-nt
             (car (file-expand-wildcards
                   (expand-file-name "Mozilla/Firefox/Profiles/*/places.sqlite"
                                     (getenv "APPDATA")))))))))

;; ** avy
(use-package avy
  :commands
  (avy-goto-line avy-goto-char)
  :eemacs-indhc
  (((:enable t)
    (avy))
   ("Avy Core Actions"
    (("M-g l" avy-goto-line "Jump to a line start in current buffer"
      :enable t :exit t :global-bind t)
     ("M-g c" avy-goto-char "Jump to the currently visible CHAR"
      :enable t :exit t :global-bind t))))

  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b a"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'avy))
      "Avy Pos Jump"
      :enable t :exit t)))))

;; ** Ivy UI Enhancement
;; *** all the icons ivy
(use-package all-the-icons-ivy
  :if (and (eq entropy/emacs-ivy-rich-type 'all-the-icons-ivy)
           (entropy/emacs-icons-displayable-p))
  :commands (all-the-icons-ivy-setup)
  :init
  (entropy/emacs-lazy-load-simple ivy
    (all-the-icons-ivy-setup)))

;; *** ivy rich mode

;; **** core
;; config inspired by the ivy config of *centaur-emacs* (https://github.com/seagle0128/.emacs.d)

(use-package ivy-rich
  :if (eq entropy/emacs-ivy-rich-type 'ivy-rich-mode)
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :commands (ivy-rich-mode)
  :init

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (defun entropy/emacs-ivy--enable-ivy-rich-common ()
    (require 'ivy)
    (require 'all-the-icons)
    (ivy-rich-mode +1)
    (ivy-mode +1)
    (setq ivy-virtual-abbreviate
          (or (and ivy-rich-mode 'abbreviate) 'name)))

  (cond
   (entropy/emacs-fall-love-with-pdumper
    (add-hook 'entropy/emacs-pdumper-load-hook
              #'entropy/emacs-ivy--enable-ivy-rich-common))
   (t
    (entropy/emacs-lazy-initial-advice-before
     (ivy-read)
     "ivy-rich" "ivy-rich"
     (entropy/emacs-ivy--enable-ivy-rich-common))))

  :config
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Initial eemacs specific ivy-rich columns
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-switch-buffer-size
             (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode
             (:width 12 :face warning)))
           :predicate
           (lambda
             (cand)
             (get-buffer cand)))
          counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary
             (:face font-lock-doc-face)))))))

;; **** all the icons ivy rich
(use-package all-the-icons-ivy-rich
  :commands all-the-icons-ivy-rich-mode
  :if
  (eq entropy/emacs-ivy-rich-type 'ivy-rich-mode)
  :preface
  (defun entropy/emacs-ivy--all-the-icon-ivy-rich-common-dir-icon
      (candi)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun entropy/emacs-ivy--all-the-icons-ivy-rich-common-file-icon (candidate)
    "Display file icon from CANDIDATE in `ivy-rich'."
    (let* ((path (concat ivy--directory candidate))
           (file (file-name-nondirectory path))
           (icon (cond
                  ((file-directory-p path)
                   (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                  ((string-match-p "^/.*:$" path)
                   (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                  ((not (string-empty-p file))
                   (all-the-icons-icon-for-file file :v-adjust -0.05)))))
      (all-the-icons-ivy-rich--format-icon
       (if (symbolp icon)
           (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :v-adjust 0.0)
         icon))))

  :init
  (entropy/emacs-lazy-with-load-trail
   all-the-icons-ivy-rich-mode
   (memoize 'entropy/emacs-ivy--all-the-icons-ivy-rich-common-file-icon)
   (let* ((enable-func
           (lambda ()
             (entropy/emacs-lazy-load-simple ivy
               (all-the-icons-ivy-rich-mode 1)))))
     (if (daemonp)
         (entropy/emacs-with-daemon-make-frame-done
          'all-the-icon-ivy-rich-mode
          '(when (bound-and-true-p all-the-icons-ivy-rich-mode)
             (all-the-icons-ivy-rich-mode 0))
          `(funcall ,enable-func))
       (when (entropy/emacs-icons-displayable-p)
         (funcall enable-func)))))

  :config
  ;; patch `all-the-icons-ivy-rich-display-transformers-list' for
  ;; reducing performace lagging
  (let ((orig-ivy-rich-trans-list
         (copy-tree all-the-icons-ivy-rich-display-transformers-list)))
    (dolist
        (item
         '((ivy-switch-buffer
            (:columns
             ((all-the-icons-ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t"))
           (counsel-projectile-switch-project
            (:columns
             ((entropy/emacs-ivy--all-the-icon-ivy-rich-common-dir-icon)
              (ivy-rich-candidate))
             :delimiter "\t"))
           (counsel-projectile-find-file
            (:columns
             ((entropy/emacs-ivy--all-the-icons-ivy-rich-common-file-icon)
              (counsel-projectile-find-file-transformer))
             :delimiter "\t"))
           (counsel-projectile-find-dir
            (:columns
             ((all-the-icons-ivy-rich-project-icon)
              (counsel-projectile-find-dir-transformer))
             :delimiter "\t"))))
      (setq orig-ivy-rich-trans-list
            (plist-put orig-ivy-rich-trans-list
                       (car item)
                       (cadr item))))
    (setq all-the-icons-ivy-rich-display-transformers-list
          orig-ivy-rich-trans-list)))

;; ** Powerful searcher
;; *** helm ag
(defun entropy/emacs-ivy--helm-ag--edit-commit ()
  "Funciton to be redefine body of `helm-ag--edit-commit'.

Adding buffer unlock and wind narrowed region feature."
  (goto-char (point-min))
  (let ((read-only-files 0)
        (saved-buffers nil)
        (regexp (helm-ag--match-line-regexp))
        (default-directory helm-ag--default-directory)
        (line-deletes (make-hash-table :test #'equal))
        (kept-buffers (buffer-list))
        open-buffers)
    (while (re-search-forward regexp nil t)
      (let* ((file (or (match-string-no-properties 1) helm-ag--search-this-file-p))
             (line (string-to-number (match-string-no-properties 2)))
             (body (match-string-no-properties 3))
             (ovs (overlays-at (line-beginning-position))))
        (with-current-buffer (find-file-noselect file)
          (message "Handling with buffer '%s' ......" (current-buffer))
          (cl-pushnew (current-buffer) open-buffers)
          (when buffer-read-only
            (progn
              (cl-incf read-only-files)
              (read-only-mode 0)))
          (when (buffer-narrowed-p)
            (widen))
          (goto-char (point-min))
          (let ((deleted-lines (gethash file line-deletes 0))
                (deleted (and ovs (overlay-get (car ovs) 'helm-ag-deleted))))
            (forward-line (- line 1 deleted-lines))
            (delete-region (line-beginning-position) (line-end-position))
            (if (not deleted)
                (insert body)
              (let ((beg (point)))
                (forward-line 1)
                (delete-region beg (point))
                (puthash file (1+ deleted-lines) line-deletes)))
            (cl-pushnew (current-buffer) saved-buffers)))))
    (when helm-ag-edit-save
      (dolist (buf saved-buffers)
        (with-current-buffer buf
          (save-buffer))))
    (dolist (buf open-buffers)
      (unless (memq buf kept-buffers)
        (kill-buffer buf)))
    (helm-ag--exit-from-edit-mode)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and be lift a ban." read-only-files)
      (message "Success update"))))

(use-package helm-ag
  :if (string= entropy/emacs-search-program "ag")
  :commands (helm-do-ag helm-do-ag-project-root)
  :init
  ;; case-sensitive for ag search command.
  (setq helm-ag-base-command
        "ag --nocolor --nogroup --case-sensitive"
        helm-ag-use-grep-ignore-list t)
  :config
  (dolist (el '(helm-do-ag helm-do-ag-project-root))
    (advice-add el :around
                #'entropy/emacs-lang-use-utf-8-ces-around-advice))

  (defun helm-ag--edit-commit ()
    "Note: this function has been re-define for compat with
entropy-emacs which force inhibit readonly mode while operating
corresponding buffer, and forcely `widen' the reflected buffers so
that the replacement POS can be find absolutely."
    (interactive)
    (funcall #'entropy/emacs-ivy--helm-ag--edit-commit)))

;; *** rg

(use-package rg
  :if (string= entropy/emacs-search-program "rg")
  :commands
  (rg
   rg-project)
  :config
  (dolist (func '(rg rg-project))
    (advice-add func
                :around
                #'entropy/emacs-lang-use-utf-8-ces-around-advice)))

;; *** hydra for searcher

(entropy/emacs-hydra-hollow-common-individual-hydra-define
 'powerful-searcher nil
 (if (eq entropy/emacs-search-program "ag")
     '("Powerful Searcher"
       (("C-c j" helm-do-ag "Helm AG Search"
         :enable t :exit t :global-bind t)
        ("C-c k" helm-do-ag-project-root "Helm AG search for project root"
         :enable t :exit t :global-bind t)))
   '("Powerful Searcher"
     (("C-c j" rg "Ripgrep for location selected"
       :enable t :exit t :global-bind t)
      ("C-c k" rg-project "Ripgrep for current project"
       :enable t :exit t :global-bind t)))))

(entropy/emacs-hydra-hollow-add-for-top-dispatch
 '("Utils"
   (("u s"
     (:eval
      (entropy/emacs-hydra-hollow-category-common-individual-get-caller
       'powerful-searcher))
     "Powerful searcher"
     :enable t :exit t))))
;; ** Powerful find-file
(use-package find-file-in-project
  :defines (ffip-project-root)
  :commands (ffip-find-files
             entropy/emacs-ivy-ffip
             entropy/emacs-ivy-ffip-directory-only)
  :eemacs-indhc
  (((:enable t)
    (recursive-find-file))
   ("Recursive file system search (find-file recursive match)"
    (("C-x M-f" entropy/emacs-ivy-ffip "Fuzzy Open File"
      :enable t :exit t :global-bind t)
     ("C-x M-d" entropy/emacs-ivy-ffip-directory-only "Fuzzy Open File Under Directory"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t))
   ("Utils"
    (("u f"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'recursive-find-file))
      "Find file recursively in specified root"
      :enable t :exit t))))
  :init
  ;; Using rust 'fd' for perfomance improving
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))
  :config
  (defun entropy/emacs-ivy-ffip (_interaction)
    "Find file using `find-file-in-project' in place.

Just find directory when _INTERACTION was non-nil (the prefix
with `C-u')."
    (interactive "P")
    (let (prompt-func)
      (setq prompt-func
            (lambda ()
              (let (target)
                (setq target
                      (read-directory-name
                       "Choose Place Root: "
                       nil nil t))
                (unless (file-directory-p target)
                  (setq target (file-name-directory target)))
                target)))
      (let ((ffip-project-root (funcall prompt-func))
            (ffip-ignore-filenames nil))
        (if _interaction
            (ffip-find-files "" nil t)
          (ffip-find-files nil nil)))))

  (defun entropy/emacs-ivy-ffip-directory-only ()
    "Find directory using `find-file-in-project' in place."
    (interactive)
    (funcall-interactively 'entropy/emacs-ivy-ffip t)))

;; ** provide
(provide 'entropy-emacs-ivy)
