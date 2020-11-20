;;; entropy-emacs-company.el --- entropy emacs completion config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-company.el
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
;; Completion referrence config for `entropy-emacs'.
;;
;; `entropy-emacs' use [[http://company-mode.github.io][company-mode]] as the completion framework as
;; the completion main tool. It's the framework who provide the
;; APIS to built arbitrary completion backends for various emacs
;; major modes even for the mode independent way.
;;
;; There's two completion server type choice for `entropy-emacs':
;;
;; 1) Traditional way:
;;
;;    The way that each backends basic on the server tool-chain are
;;    independently using its own designation, such as pyton
;;    `anaconda-mode', C `irony-mode', javascript `tern-mode'.
;;
;;    Advantage for this type is that each backend maintained
;;    individually and designed just for the single sake. this can
;;    limitting code built scope and reducing bug fixing difficulty
;;    level.
;;
;;    The weakness was that non-standard server-client communication
;;    api, which will impede the further features development who
;;    stand on the top level of all or some of them.
;;
;; 2) LSP Mode:
;;
;;    LSP (language server protocol) was brought up by Microsoft, for
;;    solving the problem caused from way '1)', it was under
;;    development. emacs melpa package 'lsp-mode' and 'elgot' was the
;;    client for thus, but be under development and with sets of
;;    bugs.
;;
;; `entropy-emacs' defaultly enable the traditional way for the sake
;; of stability.
;;
;;
;; * Configuration:
;;
;; configurationLoaidng by `entropy-emacs' automatically.
;;
;; * Code:

;; ** require

;; ** defvar
(defvar entropy/emacs-company-elisp-top-backends
  '(company-files company-capf :with company-en-words)
  "Basic top company-backend for all situations.")

(entropy/emacs-lazy-load-simple company
  (setq-default company-backends
                (append (list entropy/emacs-company-elisp-top-backends)
                        company-backends)))

;; ** libraries
;; *** yas load
(defun entropy/emacs-company-use-yasnippet (backend &optional reverse)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (if reverse
        (append
         '(company-yasnippet) '(:separate)
         (if (consp backend) backend (list backend)))
      (append
       (if (consp backend) backend (list backend))
       '(:separate company-yasnippet)))))

(defvar entropy/emacs-company-start-with-yas-done nil)
(defun entropy/emacs-company-start-with-yas (&rest _)
  (unless (or (bound-and-true-p yas-global-mode)
              entropy/emacs-company-start-with-yas-done)
    (when (fboundp 'yas-global-mode)
      (yas-global-mode)
      (setq entropy/emacs-company-start-with-yas-done t))))

;; *** company for docs modes

(defun entropy/emacs-company-privilege-yas-for-docs ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends
               '(company-files company-yasnippet :with company-en-words)))

(defun entropy/emacs-company-yas-for-docs-init ()
  (let (macros)
    (dolist (el '((org-mode . org-mode-hook)
                  (mardown-mode . markdown-mode-hook)
                  (text-mode . text-mode-hook)))
      (add-to-list
       'macros
       `(lambda ()
          (with-eval-after-load ',(car el)
            (add-hook ',(cdr el)
                      #'entropy/emacs-company-privilege-yas-for-docs)))))
    (dolist (macro macros)
      (funcall macro))))

;; ** company core
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode
             company-mode
             company-complete)

;; *** preface
  :preface

  (defun entropy/emacs-company-toggle-idledelay (&optional prefix)
    (interactive "P")
    (if (bound-and-true-p company-idle-delay)
        (progn (setq company-idle-delay nil)
               (message "turn off `company-idle-delay'"))
      (setq company-idle-delay
            (if prefix
                (let ((secs (string-to-number
                             (read-string "Input Company delay secs: "))))
                  (if (and (numberp secs)
                           (> secs 0))
                      secs
                    (message "Invalid company-delay secs '%s'" secs)
                    entropy/emacs-company-idle-delay-default))
              entropy/emacs-company-idle-delay-default))
      (let ((entropy/emacs-message-non-popup t))
        (entropy/emacs-message-do-message
         "%s '%s' to '%s'"
         (blue "Set")
         (yellow (symbol-name 'company-idle-delay))
         (red (number-to-string company-idle-delay))))))

  (defun entropy/emacs-company--gc-optimize ()
    "Maximize `gc-cons-threshold' when company session is
actived, as the rest that next garbage-collect operation til
`entropy/emacs-gc--idle-time-recovery' triggered."
    (when (and (bound-and-true-p company-emulation-alist)
               (not (equal company-emulation-alist '((t . nil)))))
      (setq gc-cons-threshold most-positive-fixnum)))

  (defun entropy/emacs-company-files (command)
    (interactive (list 'interactive))
    (unless (or buffer-read-only
                (equal (buffer-name)
                       entropy/emacs-init-welcome-buffer-name))
      (company-files command)))

;; *** bind-key
  :bind (:map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ;; ("<tab>" . company-complete-selection)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))

  :eemacs-indhc
  (((:enable t)
    (company-auto-completion))
   ("Basic complete"
    (("M-/" company-complete
      "Insert the common part of all candidates or the current selection"
      :enable t :global-bind t :exit t)
     ("M-\\" company-dabbrev "dabbrev-like 'company-mode' completion backend"
      :enable t :global-bind t :exit t)
     ("C-c C-y" company-yasnippet "'company-mode' backend for 'yasnippet'"
      :enable t :global-bind t :exit t)
     ("]" entropy/emacs-company-files "Auto complete file path at point"
      :enable t :eemacs-top-bind t :exit t)
     ("M-]" company-en-words "Auto complete english word at point."
      :enable t :global-bind t :exit t)
     ("M-p" entropy/emacs-company-toggle-idledelay
      "Turn on/off automatically company completion (prefix key for set idle delay)."
      :enable t :eemacs-top-bind t :exit t))))

  :eemacs-tpha
  (((:enable t))
   ("Basic"
    (("b c"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'company-auto-completion))
      "Auto completion operations"
      :enable t :exit t))))

;; *** init for load
  :init
  (entropy/emacs-lazy-with-load-trail
   global-company-mode
   (global-company-mode t)
   (dolist (func '(company-idle-begin company-complete))
     (advice-add func
                 :before 'entropy/emacs-company-start-with-yas))
   (entropy/emacs-company-yas-for-docs-init))

  (entropy/emacs-lazy-load-simple (company counsel)
    (define-key company-active-map (kbd "M-o") 'counsel-company))

;; *** config for after-load
  :config
;; **** basic setting
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; optimized gc while company session for reducing lags
  (add-hook 'post-command-hook #'entropy/emacs-company--gc-optimize)

  ;; common internal customization
  (setq
   company-tooltip-limit 20  ; bigger popup window
   company-echo-delay 0      ; remove annoying blinking
   company-tooltip-maximum-width 70
   company-tooltip-minimum-width 55
   company-tooltip-align-annotations t
   company-tooltip-offset-display 'lines
   company-idle-delay entropy/emacs-company-idle-delay-default
   company-dabbrev-code-everywhere t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-dabbrev-char-regexp "\\sw[-_]*")
  )

;; *** company components function autoload
(use-package company-dabbrev   :ensure nil :after company :commands company-dabbrev)
(use-package company-files     :ensure nil :after company :commands company-files)
(use-package company-yasnippet :ensure nil :after company :commands company-yasnippet)

;; ** company enhancement
;; *** Popup documentation for completion candidates
(use-package company-quickhelp
  :if (eq entropy/emacs-company-tooltip-use-type 'default)
  :after company
  :commands (company-quickhelp-mode
             company-quickhelp-manual-begin)
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin)
              ("C-h" . nil)
              ("<f1>" . nil))
  :init
  (setq company-quickhelp-delay
        entropy/emacs-company-quickhelp-delay-default)
  (if (daemonp)
      (entropy/emacs-with-daemon-make-frame-done
       'company-quickhelp-mode
       '(company-quickhelp-mode 0)
       '(company-quickhelp-mode 1))
    (when (display-graphic-p)
      (company-quickhelp-mode 1))))

;; *** Company-posframe config

(use-package company-posframe
  :if (eq entropy/emacs-company-tooltip-use-type 'company-posframe)
  :after company
  :commands (company-posframe-mode)
  :diminish company-posframe-mode
  :init
  (setq company-posframe-quickhelp-delay
        entropy/emacs-company-quickhelp-delay-default)
  (if (null (daemonp))
      (when (entropy/emacs-posframe-adapted-p)
        (company-posframe-mode 1))
    (entropy/emacs-with-daemon-make-frame-done
     'company-posframe-mode
     '(when (bound-and-true-p company-posframe-mode)
        (company-posframe-mode 0))
     `(unless (bound-and-true-p company-posframe-mode)
        (company-posframe-mode 1)))))

;; *** Company-box config

(use-package company-box
  :if (eq entropy/emacs-company-tooltip-use-type 'company-box)
  :after company
  :commands (company-box-mode)

;; **** preface
  :preface

;; **** init
  :init
  (setq company-box-doc-delay
        ;; FIXME: company box idle delay smaller than 0.42 will cause
        ;; first candi doc not show
        (max 0.42 entropy/emacs-company-quickhelp-delay-default)
        company-box-max-candidates (if (boundp 'x-gtk-resize-child-frames) 100 20)
        company-box-show-single-candidate 'always)

  (if (null (daemonp))
      (add-hook 'company-mode-hook
                #'company-box-mode)
    (entropy/emacs-with-daemon-make-frame-done
     'company-box-mode
     '(progn
        (remove-hook 'company-mode-hook
                     #'company-box-mode)
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (when (bound-and-true-p company-box-mode)
                    (company-box-mode 0))))
              (buffer-list)))
     '(progn
        (add-hook 'company-mode-hook
                  #'company-box-mode)
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (unless (bound-and-true-p company-box-mode)
                    (company-box-mode 1))))
              (buffer-list)))))

;; **** config
  :config

;; ***** common setting
  (add-hook 'entropy/emacs-theme-load-after-hook
            #'entropy/emacs-company--company-box-recover-bg)
  (defun entropy/emacs-company--company-box-recover-bg ()
    "Recovery background color for company-box frame after theme
loading.

The exists meaning for this function is that the
company-box-frame and company-box-doc-frame was built tied to the
orignal selected frame at once persistently, which also take
affection of the theme loading which will make all frame's
default background color changes so that also to change the
initial frame background face sets did by
`company-box--make-frame'."
    (let ((main-bg (face-background 'company-box-background nil t)))
      (dolist (frame (list (ignore-errors (company-box--get-frame))
                           (frame-parameter nil 'company-box-doc-frame)))
        (when (frame-live-p frame)
          (message "company-box rec for frame '%s'" frame)
          (with-selected-frame frame
            (unless (equal main-bg (frame-parameter nil 'background-color))
              (set-frame-parameter nil 'background-color
                                   main-bg)
              (message "company box bg for frame '%s' is set to %s" frame
                       (frame-parameter nil 'background-color))))))))

;; ***** show mechanism patch
  (when sys/linuxp
    ;; Fix child-frame resize/reposition bug on linux
    (if (boundp 'x-gtk-resize-child-frames)
        ;; FIXME: `x-gtk-resize-child-frames' option was one temporal
        ;; patch method invoking from emacs-devel commit c49d379f17bcb
        ;; which will eliminated in the future emacs version.
        (setq x-gtk-resize-child-frames 'hide)))

  (defun entropy/emacs-company--company-box-content-length-restrict-advice (orig-func &rest orig-args)
    "Restrict  company box content length to reduce lagging feels."
    (if (bound-and-true-p company-box-mode)
        (let* ((company-candidates (-take company-box-max-candidates company-candidates))
               (company-candidates-length (length company-candidates)))
          (apply orig-func orig-args))
      (apply orig-func orig-args)))

  (dolist (el '(company-box-show company-box--update company-set-selection))
    (advice-add el
                :around
                #'entropy/emacs-company--company-box-content-length-restrict-advice))

;; ***** icons patch

  (with-no-warnings
    ;; Prettify icons
    (defun entropy/emacs-company--company-box-icons-elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'entropy/emacs-company--company-box-icons-elisp))

  (progn
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq entropy/emacs-company--company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'entropy/emacs-company--company-box-icons-all-the-icons))
  )

;; *** Better sorting and filtering
(use-package company-prescient
  :after company
  :commands (company-prescient-mode)
  :init
  (company-prescient-mode 1))

;; *** Company in minibuffer

;; Stolen from https://gist.github.com/Bad-ptr/7787596#file-company-minibuffer-el
(entropy/emacs-lazy-load-simple company
  (defvar-local entropy/emacs-company--minibuffer-command nil)

  (defun entropy/emacs-company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion back-end for Emacs Lisp in the
minibuffer."
    (interactive (list 'interactive))
    (case command
      ('prefix (and (minibufferp)
                    (company-grab-symbol)))
      ('candidates
       (case entropy/emacs-company--minibuffer-command
         ('execute-extended-command (all-completions arg obarray 'commandp))
         (t (all-completions arg obarray))))))

  (defun entropy/emacs-active-minibuffer-company-elisp ()
    "Active `company-mode' in minibuffer only for elisp
completion when calling: 'execute-extended-command' or
'eval-expression'."
    (company-mode 0)
    (when (and global-company-mode (or
                                    (eq this-command #'execute-extended-command)
                                    (eq this-command #'eval-expression)
                                    (eq this-command #'eldoc-eval-expression)))

      (setq-local entropy/emacs-company--minibuffer-command this-command)

      (setq-local completion-at-point-functions
                  (list (if (fboundp 'elisp-completion-at-point)
                            #'elisp-completion-at-point
                          #'lisp-completion-at-point)
                        t))

      (setq-local company-show-numbers nil)
      (setq-local company-backends '((entropy/emacs-company-elisp-minibuffer
                                      company-capf)))
      (setq-local company-tooltip-limit 6)
      (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                      company-preview-if-just-one-frontend))
      (company-mode 1)

      ;; We just use overlay render tooltip type because other
      ;; child-frame ones can not show those frame at point of oneline
      ;; height minibuffer window
      (when (bound-and-true-p company-box-mode)
        (company-box-mode 0))
      (when (bound-and-true-p company-posframe-mode)
        (company-posframe-mode 0))))

  (add-hook 'minibuffer-setup-hook
            #'entropy/emacs-active-minibuffer-company-elisp)
  )

;; ** company-lsp
(entropy/emacs-lazy-load-simple lsp-mode
  (advice-add 'lsp
              :after
              #'entropy/emacs-company-add-lsp-backend))

(defun entropy/emacs-company-add-lsp-backend (&rest args)
  (make-local-variable 'company-backends)
  (setq-local company-backends (remove 'company-lsp company-backends))
  (add-to-list 'company-backends
               '(company-files
                 company-capf
                 :separate company-dabbrev-code company-keywords
                 :with company-yasnippet)))

;; ** Individual backends
(defun entropy/emacs-company--default-traditional-backends-generator
    (stick-backends)
  (make-local-variable 'company-backends)
  (setq-local
   company-backends
   `((company-files
      ,@stick-backends
      :separate
      company-dabbrev-code
      company-gtags
      company-etags
      company-keywords
      :with company-yasnippet
      ))))

;; *** miscelloneous
;; **** englishs dict quick completion
(use-package company-en-words
  :after company
  :ensure nil
  :commands company-en-words
  :init
  (with-eval-after-load 'company-box
    (defun entropy/emacs-company--company-en-words-icons-for-company-box
        (candi)
      "Common text icon view for non-matched candi of dev env so
that for en-words candi recognized "
      'Unknown)
    (setq company-box-icons-functions
          (append company-box-icons-functions
                  '(entropy/emacs-company--company-en-words-icons-for-company-box)))
    ;; add specific backends face in box
    (add-to-list 'company-box-backends-colors
                 '(company-en-words
                   :all "DarkOrange"
                   :selected
                   (:background "gray" :foreground "black")
                   :annotation
                   (:foreground "yellow")))
    ))

;; *** shell
(use-package company-shell
  :if (or (eq (entropy/emacs-get-use-ide-type 'sh-mode) 'traditional)
          entropy/emacs-ide-suppressed)
  :after company
  :commands (company-shell company-shell-env company-fish-shell)
  :init
  (entropy/emacs-lazy-load-simple sh-script
    (defun entropy/emacs-company--set-company-backends-for-sh-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-shell company-shell-env)))
    (add-hook
     'sh-mode-hook
     #'entropy/emacs-company--set-company-backends-for-sh-mode-in-traditional-way)))

;; *** web refer
;; **** web/html&css
(use-package company-web
  :after company
  :commands company-web
  :init

  (autoload (function company-web-html) "company-web-html" nil t)
  (autoload (function company-web-jade) "company-web-jade" nil t)
  (autoload (function company-web-slim) "company-web-slim" nil t)

  (entropy/emacs-lazy-load-simple web-mode
    (when (eq (entropy/emacs-get-use-ide-type 'web-mode) 'traditional)
      (defun entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way
          ()
        (entropy/emacs-company--default-traditional-backends-generator
         '(company-web-html)))
      (add-hook
       'web-mode-hook
       #'entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way)))

  (entropy/emacs-lazy-load-simple css-mode
    (when (eq (entropy/emacs-get-use-ide-type 'css-mode) 'traditional)
      (defun entropy/emacs-company--set-company-backends-for-css-mode-in-traditional-way
          ()
        (entropy/emacs-company--default-traditional-backends-generator
         '(company-css)))
      (add-hook
       'css-mode-hook
       #'entropy/emacs-company--set-company-backends-for-css-mode-in-traditional-way)))

  )

;; **** javascript
(use-package company-tern
  :ensure nil
  :if (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
  :after company
  :commands company-tern
  :init
  (entropy/emacs-lazy-load-simple js2-mode
    (defun entropy/emacs-company--set-company-backends-for-js2-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-tern)))
    (add-hook
     'js2-mode-hook
     #'entropy/emacs-company--set-company-backends-for-js2-mode-in-traditional-way))

  :config
  (defun entropy/emacs-company-create-tern-project-file (&rest _)
    "Auto create '.tern-project' file in current dir.

Notice: this automatically created file was simple, you should
modify it by personal customization.

And this automatically created file was the file within
entropy-emacs."
    (let ((tern-template (expand-file-name ".tern-project" entropy/emacs-templates-dir)))
      (when (buffer-file-name)
        (unless (file-exists-p (expand-file-name ".tern-project" default-directory))
          (if (file-exists-p tern-template)
              (progn
                (copy-file tern-template
                           (expand-file-name ".tern-project" default-directory))
                (message "Succeed to create .tern-project in this folder!"))
            (message "Can not find origin .tern-project file from %s"
                     (file-name-directory tern-template)))))))

  (advice-add 'company-tern :before
              #'entropy/emacs-company-create-tern-project-file))

;; **** php
(use-package company-php
  :if (eq (entropy/emacs-get-use-ide-type 'php-mode) 'traditional)
  :commands company-ac-php-backend
  :init
  (entropy/emacs-lazy-load-simple php-mode
    (defun entropy/emacs-company--set-company-backends-for-php-mode-in-traditional-way ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-ac-php-backend)))
    (add-hook
     'php-mode-hook
     #'entropy/emacs-company--set-company-backends-for-php-mode-in-traditional-way)))

;; *** C(PP) Java python
;; **** C(PP)
;; ***** headers
(use-package company-c-headers
  :after company
  :commands company-c-headers)

;; ***** company irony
(use-package company-irony
  :after company
  :commands commpany-irony)

;; ***** main

(defun entropy/emacs-company--set-company-backends-for-C-mode-in-traditional-way
    ()
  (entropy/emacs-company--default-traditional-backends-generator
   '(company-c-headers company-irony)))

(entropy/emacs-lazy-load-simple cc-mode
  (dolist (item '((c-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c-mode) 'traditional))
                  (c++-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c++-mode) 'traditional))))
    (when (eval (cdr item))
      (add-hook
       (car item)
       #'entropy/emacs-company--set-company-backends-for-C-mode-in-traditional-way
       ))))


;; **** Java
;; **** Python
(use-package company-anaconda
  :if (eq (entropy/emacs-get-use-ide-type 'python-mode) 'traditional)
  :after company
  :commands company-anaconda
  :init
  (entropy/emacs-lazy-load-simple python
    (defun entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-anaconda)))
    (add-hook
     'anaconda-mode-hook
     #'entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way)))

;; *** common lisp
;; slime repl completion
(use-package slime-company
  :after slime
  :commands (company-slime slime-company-doc-mode)
  :init
  (add-to-list 'slime-contribs 'slime-company)
  (add-hook 'slime-mode-hook
            #'entropy/emacs-company-slime-add-company-slime-backend)
  (add-hook 'slime-repl-mode-hook
            #'entropy/emacs-company-slime-add-company-slime-backend)
  (defun entropy/emacs-company-slime-add-company-slime-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-slime)
                company-backends)))

;; * provide
(provide 'entropy-emacs-company)
