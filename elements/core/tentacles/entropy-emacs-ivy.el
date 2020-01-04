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
  
  ;; using fuzzy matching
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          ;; using elisp regex match candidates
          (t . ivy--regex)))

  (setq swiper-action-recenter t)
  
;; *** ivy config
  :config

  ;; redefines `ivy-read-action' for fix some bug
  (defun ivy-read-action ()
    "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise.

Notice: 

*This function have been modified for fixing the bug of:*

If `buffer-list' includ buffer \"Backtrace\" buffer this func will
not performed normally.

The core error founded with was the snippet: \\
~(setq key (concat key (string (read-key hint))))~

Which the part ~(read-key hint)~ will return one list as 
='(t . NUM)=  instead of the common return as one number, this
will cause func =string= sign the error:
#+BEGIN_EXAMPLE
(wrong-type-argument characterp (t . NUM))
#+END_EXAMPLE

To solve this problem was adding one condition checker for judge
which type of value be:
#+BEGIN_SRC emacs-lisp
  (or (condition-case error (string hint-value) (error nil))
      (condition-case error (string (cdr hint-value))))
  ;; hint-value was can be defined in one 'let' form for func ~(read-key hint)~.
 #+END_SRC
"
    (interactive)
    (let ((actions (ivy-state-action ivy-last)))
      (if (not (ivy--actionp actions))
          t
        (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
               (resize-mini-windows t)
               (key "")
               action-idx)
          (while (and (setq action-idx (cl-position-if
                                        (lambda (x)
                                          (string-prefix-p key (car x)))
                                        (cdr actions)))
                      (not (string= key (car (nth action-idx (cdr actions))))))
            (let* ((hint-value (read-key hint))
                   hint-string)
              (setq hint-string
                    (or
                     (condition-case error
                         (string hint-value)
                       (error nil))
                     (condition-case error
                         (string (cdr hint-value))
                       (error nil))))
              (if hint-string
                  (setq key (concat key hint-string))
                (error "Ivy hint error!"))))
          (cond ((member key '("" ""))
                 nil)
                ((null action-idx)
                 (message "%s is not bound" key)
                 nil)
                (t
                 (message "")
                 (setcar actions (1+ action-idx))
                 (ivy-set-action actions)))))))

  (defun entropy/emacs-ivy--ivy-read-action-after-advice (&rest args)
    "Interrupting rest process when `this-command' was
`ivy-read-action'."
    (catch 'ivy-quit
      (if (or (eq this-command 'ivy-dispatching-done)
              (eq this-command 'ivy-occur))
          (progn
            (user-error "Ivy-quit!")
            (throw 'ivy-quit "Ivy-quit!")))))
  (advice-add 'ivy-read :after #'entropy/emacs-ivy--ivy-read-action-after-advice)

  ;; top level for ivy-mode-map
  (entropy/emacs-lazy-load-simple 'ivy
    (define-key ivy-mode-map (kbd "ESC ESC") 'top-level))


  ;; Redefine the ivy-partial-or-done to prevent double click '<tab>'

  ;; This portion give the minor changed for disabled double tab in
  ;; ivy completion for preventing accidental operation of double hint
  ;; for 'TAB'.
  (defun ivy-partial-or-done ()
    "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
    (interactive)
    (if (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
             (or (and (equal ivy--directory "/")
                      (string-match "\\`[^/]+:.*\\'" ivy-text))
                 (string-match "\\`/" ivy-text)))
        (let ((default-directory ivy--directory)
              dir)
          (minibuffer-complete)
          (setq ivy-text (ivy--input))
          (when (setq dir (ivy-expand-file-if-directory ivy-text))
            (ivy--cd dir)))
      (or (ivy-partial)
          (when (or (eq this-command last-command)
                    (eq ivy--length 1))
            (message ":) evil smile ^v^"))))))


;; ** swiper

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :init
  (entropy/emacs-lazy-load-simple 'swiper
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
  
  (entropy/emacs-lazy-load-simple 'all-the-icons-dired
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
         ("C-x d"   . counsel-dired)
         ("C-x C-f" . counsel-find-file)
         ("C-h P"   . counsel-package)
         ("C-h v"   . counsel-describe-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-h l"   . counsel-find-library)
         ("C-x j"   . counsel-mark-ring)
         ("C-x C-t" . counsel-find-file-extern)
         ("C-c M-b" . counsel-recentf)
         ("C-c M-k" . counsel-yank-pop)

;; **** entropy/emacs-top-keymap
         :map entropy/emacs-top-keymap
         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)
         ("C-c f" . counsel-faces)
         ("C-c a" . counsel-apropos)
         ("C-c u" . counsel-unicode-char)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c w" . counsel-colors-web)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c a" . counsel-ag)
         
;; **** counsel mode map
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper))
  
;; *** hooks
  :hook ((ivy-mode . counsel-mode))
  
;; *** init
  :init

  (setq counsel-find-file-at-point nil)
  (setq counsel-yank-pop-separator "\n-------\n")
  
  (entropy/emacs-lazy-load-simple 'counsel
    (ivy-mode +1))
  
  (unless sys/win32p
    (entropy/emacs-!set-key (kbd "<M-up>") #'counsel-linux-app))
  
;; **** improve counsel-git and counsel-bookmark

  ;; counsel-git and counsel-bookmark usually using 'utf-8' encoding for searching and return data
  ;; back, so `entropy/emacs-custom-language-environment-enable' was conflicted with it, we must turn the
  ;; main encoding type while calling them.
  (if entropy/emacs-custom-language-environment-enable
      (progn
        (defun entropy/emacs-ivy-counsel-git ()
          (interactive)
          (if (not (string= current-language-environment "UTF-8"))
              (progn
                (entropy/emacs-lang-set-utf-8)
                (counsel-git))
            (counsel-git)))
        (global-set-key (kbd "C-c g") 'entropy/emacs-ivy-counsel-git)

        (defun entropy/emacs-ivy-counsel-bookmark ()
          (interactive)
          (if (not (string= current-language-environment "UTF-8"))
              (progn
                (entropy/emacs-lang-set-utf-8)
                (counsel-bookmark))
            (counsel-bookmark)))
        (global-set-key (kbd "C-x r b") 'entropy/emacs-ivy-counsel-bookmark))
    (progn
      (global-set-key (kbd "C-c g") 'counsel-git)
      (global-set-key (kbd "C-x r b") 'counsel-bookmark)))

;; *** config
  :config
;; **** reset `ivy-initial-inputs-alist'  
  (setq ivy-initial-inputs-alist nil)
  
;; **** advice counsel--M-x-externs for it's bad lagging perfomance

  ;; because `counsel--M-x-externs' has the `require' function for it's contained condition context
  ;; so it will lagging like previous version of `ivy--add-face'.
  (defun entropy/emacs-ivy--counsel--M-x-externs ()
    nil)
  (advice-add 'counsel--M-x-externs :override #'entropy/emacs-ivy--counsel--M-x-externs)

;; **** windows not to use grep because there's no grep in windows
  (when sys/win32p
    (defun counsel-grep-or-swiper (&optional initial-input)
      (interactive)
      (swiper initial-input)))
  
;; **** counsel-load-theme
  (defun entropy/emacs-ivy-counsel-load-theme ()
    "Load theme with reset the org-headline face for disabled the
font style and height."
    (interactive)
    (counsel-load-theme)
    (ignore-errors
      (entropy/emacs-adjust-org-heading-scale)))

;; **** counsel-locate
  (when (and sys/win32p entropy/emacs-wsl-enable)
    (defun entropy/emacs-ivy--counsel-locate ()
      "Call counsel-locate by unicode encoding when in windows
plattform."
      (interactive)
      (unless (string= current-language-environment "UTF-8")
        (entropy/emacs-lang-set-utf-8))
      (counsel-locate))

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

;; **** counsel-dired-jump
  (when sys/win32p
    (advice-add 'counsel-dired-jump :before #'entropy/emacs-lang-set-utf-8))
  
;; **** redefine counsel-find-file-extern

  ;; Origin this function using `interactive' prefix 'F' be as the
  ;; file choosing func, it's has a bug for what wrong with the
  ;; current diretory './' open as of redirected as current
  ;; buffer-file. Using `completing-read' instead.
  
  (defun entropy/emacs-ivy--counsel-locate-action-extern (X)
    "Pass X to `xdg-open' or equivalent command via the shell."
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (w32-shell-execute "open" X)
      (call-process-shell-command (format "%s %s"
                                          (cl-case system-type
                                            (darwin "open")
                                            (cygwin "cygstart")
                                            (t "xdg-open"))
                                          (shell-quote-argument X))
                                  nil 0)))

  (defun counsel-locate-action-extern ()
    (interactive)
    (ivy-read "Choose file open with extern: "
              'read-file-name-internal
              :require-match t
              :action #'entropy/emacs-ivy--counsel-locate-action-extern
              :history 'file-name-history
              :caller 'counsel-locate-action-extern))
  
;; **** redefine counsel-git
  
  (setq counsel-git-cmd "git ls-files --full-name --")
  
  (defvar entropy/emacs-ivy-counsel-git-root nil
    "Temporally variable storing git repository root dir,
this variable used to patching for origin `counsel-git'.")

  (defun counsel-git-cands ()
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
  :bind (:map css-mode-map ("C-c M-d" . counsel-css))
  :init
  (when entropy/emacs-fall-love-with-pdumper
    (require 'css-mode)))
  

;; *** use ivy-xref for quickly find defination and reference
(use-package ivy-xref
  :commands (ivy-xref-show-xrefs)
  :init (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

;; ** avy
(use-package avy
  :bind
  (("M-g l" . avy-goto-line)
   :map entropy/emacs-top-keymap
   ("M-;" . avy-goto-char)))

;; ** ivy all the icons
;; *** all the icons ivy
(use-package all-the-icons-ivy
  :if (eq entropy/emacs-ivy-rich-type 'all-the-icons-ivy)
  :commands (all-the-icons-ivy-setup)
  :init
  (entropy/emacs-lazy-load-simple 'ivy
    (all-the-icons-ivy-setup)))

;; *** ivy rich mode

;; config inspired by the ivy config of *centaur-emacs* (https://github.com/seagle0128/.emacs.d)

(use-package ivy-rich
  :if (eq entropy/emacs-ivy-rich-type 'ivy-rich-mode)
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :commands (ivy-rich-mode)

;; **** preface
  :preface
;; ***** ivy rich icons
  (defun entropy/emacs-ivy--ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun entropy/emacs-ivy--ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun entropy/emacs-ivy--ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (let* ((path (file-local-name (expand-file-name candidate ivy--directory)))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun entropy/emacs-ivy--ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun entropy/emacs-ivy--ivy-rich-variable-icon (_candidate)
    "Display variable icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun entropy/emacs-ivy--ivy-rich-symbol-icon (_candidate)
    "Display symbol icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

  (defun entropy/emacs-ivy--ivy-rich-theme-icon (_candidate)
    "Display theme icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun entropy/emacs-ivy--ivy-rich-keybinding-icon (_candidate)
    "Display keybindings icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (defun entropy/emacs-ivy--ivy-rich-library-icon (_candidate)
    "Display library icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun entropy/emacs-ivy--ivy-rich-package-icon (_candidate)
    "Display package icons in `ivy-rich'."
    (when (or (display-graphic-p)
              (and entropy/emacs-fall-love-with-pdumper
                   entropy/emacs-do-pdumper-in-X))
      (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

  (defun entropy/emacs-ivy--ivy-rich-font-icon (_candidate)
    "Display the font icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))
  
  (when (or (display-graphic-p)
            (and entropy/emacs-fall-love-with-pdumper
                 entropy/emacs-do-pdumper-in-X))
    (defun entropy/emacs-ivy--ivy-rich-bookmark-type-plus (candidate)
      (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type :override #'entropy/emacs-ivy--ivy-rich-bookmark-type-plus))

;; **** initialize
  :init

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))
  
;; ***** lazy load
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
     '(ivy-read)
     "ivy-rich" "ivy-rich"
     (entropy/emacs-ivy--enable-ivy-rich-common))))

;; ***** ivy-rich register
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((entropy/emacs-ivy--ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((entropy/emacs-ivy--ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((entropy/emacs-ivy--ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((entropy/emacs-ivy--ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((entropy/emacs-ivy--ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((entropy/emacs-ivy--ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((entropy/emacs-ivy--ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
           :delimiter "\t")
          counsel-describe-variable
          (:columns
           ((entropy/emacs-ivy--ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))
           :delimiter "\t")
          counsel-apropos
          (:columns
           ((entropy/emacs-ivy--ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-info-lookup-symbol
          (:columns
           ((entropy/emacs-ivy--ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((entropy/emacs-ivy--ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-locate-action-extern
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fzf
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((entropy/emacs-ivy--ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (entropy/emacs-ivy--ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-package
          (:columns
           ((entropy/emacs-ivy--ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-library
          (:columns
           ((entropy/emacs-ivy--ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-library
          (:columns
           ((entropy/emacs-ivy--ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-theme
          (:columns
           ((entropy/emacs-ivy--ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fonts
          (:columns
           ((entropy/emacs-ivy--ivy-rich-font-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-major
          (:columns
           ((entropy/emacs-ivy--ivy-rich-function-icon)
            (ivy-rich-candidate))
           :delimiter "\t")))
  
;; **** after load
  :config
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))



;; ** use helm ag or pt search
;; *** Preparation
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


;; *** libraries
(defun entropy/emacs-ivy--use-pt-common ()
  (use-package helm-pt
    :commands (helm-do-pt helm-projectile-pt)
    :bind
    (("C-c j" . helm-do-pt)
     ("C-c k" . helm-projectile-pt))
    :config
    (dolist (el '(helm-do-pt helm-projectile-pt))
      (advice-add el :around #'entropy/emacs-lang-set-local-around-wrapper))))

(defun entropy/emacs-ivy--use-ag-common ()
  (use-package helm-ag
    :commands (helm-do-ag helm-do-ag-project-root)
    :bind (("C-c j" . helm-do-ag)
           ("C-c k" . helm-do-ag-project-root))
    :init
    ;; case-sensitive for ag search command.
    (setq helm-ag-base-command
          "ag --nocolor --nogroup --case-sensitive"
          helm-ag-use-grep-ignore-list t)
    :config
    (dolist (el '(helm-do-ag helm-do-ag-project-root))
      (advice-add el :around #'entropy/emacs-lang-set-utf-8-around-wrapper))
    
    (defun helm-ag--edit-commit ()
      "Note: this function has been re-define for compat with
entropy-emacs which force inhibit readonly mode while operating
corresponding buffer."
      (interactive)
      (funcall #'entropy/emacs-ivy--helm-ag--edit-commit))))

;; *** Main
(cond
 ((string= entropy/emacs-search-program "pt")
  (entropy/emacs-ivy--use-pt-common))
 ((string= entropy/emacs-search-program "ag")
  (entropy/emacs-ivy--use-ag-common)))


;; ** using find-file with counsel
(use-package find-file-in-project
  :defines (ffip-project-root)
  :commands (ffip-find-files
             entropy/emacs-ivy-ffip
             entropy/emacs-ivy-ffip-directory-only)
  :bind (("C-x M-f" . entropy/emacs-ivy-ffip)
         ("C-x M-d" . entropy/emacs-ivy-ffip-directory-only))
  :config
  (defun entropy/emacs-ivy-ffip (_interaction)
    (interactive "P")
    (let (prompt-func)
      (setq prompt-func
            (lambda ()
              (let (target)
                (setq target
                      (completing-read
                       "Choose Place Root: "
                       'read-file-name-internal))
                (unless (file-directory-p target)
                  (setq target (file-name-directory target)))
                target)))
      (let ((ffip-project-root (funcall prompt-func))
            (ffip-ignore-filenames nil))
        (if _interaction
            (ffip-find-files "" nil t)
          (ffip-find-files nil nil)))))

  (defun entropy/emacs-ivy-ffip-directory-only ()
    (interactive)
    (funcall-interactively 'entropy/emacs-ivy-ffip t)))


;; ** provide
(provide 'entropy-emacs-ivy)
