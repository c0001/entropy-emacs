;;; entropy-emacs-web.el --- entropy-emacs web development configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-web.el
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
;; `entropy-emacs' web aspect development environment intergratioin
;; for both of front-end and backend, as that covering for =html=,
;; =css=, =javascript=, =php=.
;;
;; * Configuration:
;;
;; Using for `entropy-emacs' only.
;;
;; * Code:

;; ** require

;; ** Preparation

(defvar tern-command)
(entropy/emacs-lazy-load-simple 'tern
  (setq tern-command '("tern")))

;; ** main libraries
(defun entropy/emacs-web-browse-html-buffer ()
  "Browse current html buffer using `browse-url-browse-function'
set of `entropy/emacs-browse-url-function-get-for-web-preview'."
  (interactive nil web-mode html-mode)
  (let* (url
         tmp-file
         (browse-url-browser-function
          (entropy/emacs-browse-url-function-get-for-web-preview)))
    (if (and buffer-file-name
             (file-exists-p buffer-file-name))
        (progn
          (setq url
                (url-hexify-string
                 (concat "file://" buffer-file-name)
                 entropy/emacs-url-allowed-chars))
          (browse-url url))
      (setq tmp-file
            (make-temp-file "eemacs-web-preview_"
                            nil
                            ".html"
                            (buffer-substring (point-min) (point-max))))
      (setq url
            (url-hexify-string
             (concat "file://" tmp-file)
             entropy/emacs-url-allowed-chars))
      (browse-url url))))

(defvar web-mode-markup-indent-offset)
(defvar web-mode-css-indent-offset)
(defvar web-mode-code-indent-offset)
(defvar tern-mode)

;; ** web frontend technologies
;; *** html
;; **** web-mode
(use-package web-mode
  :commands web-mode
;; ***** mode declaration
  :mode
  ("\\.\\(phtml\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
   .
   web-mode)
;; ***** eemacs mmphc
  :eemacs-mmphc
  (((:enable t :defer t)
    (web-mode (web-mode web-mode-map) t (1 2 1 1 2)))
   ("Basic"
    (("<f1>" entropy/emacs-web-browse-html-buffer "Preview Current Buffer"
      :enable t
      :exit t))
    "Attribute"
    (("C-c C-a b" web-mode-attribute-beginning "Fetch html attribute beginning"
      :enable t :map-inject t :exit t)
     ("C-c C-a e" web-mode-attribute-end "Fetch html attribute end"
      :enable t :map-inject t :exit t)
     ("C-c C-a i" web-mode-attribute-insert "Insert an attribute inside current tag"
      :enable t :map-inject t :exit t)
     ("C-c C-a n" web-mode-attribute-next "Fetch next attribute"
      :enable t :map-inject t :exit t)
     ("C-c C-a s" web-mode-attribute-select "Select the current html attribute"
      :enable t :map-inject t :exit t)
     ("C-c C-a k" web-mode-attribute-kill "Kill the current html attribute"
      :enable t :map-inject t :exit t)
     ("C-c C-a p" web-mode-attribute-previous "Fetch previous attribute"
      :enable t :map-inject t :exit t)
     ("C-c C-a t" web-mode-attribute-transpose "Transpose the current html attribute"
      :enable t :map-inject t :exit t))
    "Block"
    (("C-c C-b b" web-mode-block-beginning "Move point to the beginning of the current block"
      :enable t :map-inject t :exit t)
     ("C-c C-b c" web-mode-block-close "Close the first unclosed control block"
      :enable t :map-inject t :exit t)
     ("C-c C-b e" web-mode-block-end "Move point to the end of the current block"
      :enable t :map-inject t :exit t)
     ("C-c C-b k" web-mode-block-kill "Kill the current block"
      :enable t :map-inject t :exit t)
     ("C-c C-b n" web-mode-block-next "Move point to the beginning of the next block"
      :enable t :map-inject t :exit t)
     ("C-c C-b p" web-mode-block-previous "Move point to the beginning of the previous block"
      :enable t :map-inject t :exit t)
     ("C-c C-b s" web-mode-block-select "Select the current block"
      :enable t :map-inject t :exit t))
    "DOM"
    (("C-c C-d a" web-mode-dom-apostrophes-replace "Replace char(') with its symbol type in the html contents of the buffer."
      :enable t :map-inject t :exit t)
     ("C-c C-d d" web-mode-dom-errors-show "Show unclosed tags"
      :enable t :map-inject t :exit t)
     ("C-c C-d e" web-mode-dom-entities-replace "Replace html entities (e.g. &eacute; &#233; or &#x00E9; become é)"
      :enable t :map-inject t :exit t)
     ("C-c C-d n" web-mode-dom-normalize "Normalize buffer"
      :enable t :map-inject t :exit t)
     ("C-c C-d q" web-mode-dom-quotes-replace "Replace dumb quotes"
      :enable t :map-inject t :exit t)
     ("C-c C-d t" web-mode-dom-traverse "Traverse html dom tree"
      :enable t :map-inject t :exit t)
     ("C-c C-d x" web-mode-dom-xpath "Display html path"
      :enable t :map-inject t :exit t))
    "Element"
    (("C-c C-e /" web-mode-element-close "Close html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e a" web-mode-element-content-select "Select the content of a html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e b" web-mode-element-beginning "Move to beginning of element"
      :enable t :map-inject t :exit t)
     ("C-c C-e c" web-mode-element-clone "Clone the current html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e d" web-mode-element-child "Fetch child element"
      :enable t :map-inject t :exit t)
     ("C-c C-e e" web-mode-element-end "Move to end of element"
      :enable t :map-inject t :exit t)
     ("C-c C-e f" web-mode-element-children-fold-or-unfold "Fold/Unfold all the children of the current html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e i" web-mode-element-insert "Insert an html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e I" web-mode-element-insert-at-point "Replace the word at point with a html tag of it"
      :enable t :map-inject t :exit t)
     ("C-c C-e k" web-mode-element-kill "Kill the current html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e m" web-mode-element-mute-blanks "Mute blanks"
      :enable t :map-inject t :exit t)
     ("C-c C-e n" web-mode-element-next "Fetch next element"
      :enable t :map-inject t :exit t)
     ("C-c C-e p" web-mode-element-previous "Fetch previous element"
      :enable t :map-inject t :exit t)
     ("C-c C-e r" web-mode-element-rename "Rename the current html element"
      :enable t :map-inject t :exit t)
     ("C-c C-e s" web-mode-element-select "Select the current html element (including opening and closing tags)"
      :enable t :map-inject t :exit t)
     ("C-c C-e t" web-mode-element-transpose "Transpose two html elements"
      :enable t :map-inject t :exit t)
     ("C-c C-e u" web-mode-element-parent "Fetch parent element"
      :enable t :map-inject t :exit t)
     ("C-c C-e v" web-mode-element-vanish "Vanish the current html element. The content of the element is kept"
      :enable t :map-inject t :exit t)
     ("C-c C-e w" web-mode-element-wrap "Wrap current REGION with start and end tags"
      :enable t :map-inject t :exit t)
     ("C-c C-e +" web-mode-element-extract "Flatten element"
      :enable t :map-inject t :exit t)
     ("C-c C-e -" web-mode-element-contract "Flatten elements"
      :enable t :map-inject t :exit t))
    "Tag"
    (("C-c C-t a" web-mode-tag-attributes-sort "Sort the attributes inside the current html tag"
      :enable t :map-inject t :exit t)
     ("C-c C-t b" web-mode-tag-beginning "Fetch current html tag beg"
      :enable t :map-inject t :exit t)
     ("C-c C-t e" web-mode-tag-end "Fetch current html tag end"
      :enable t :map-inject t :exit t)
     ("C-c C-t m" web-mode-tag-match "Move point to the matching opening/closing tag"
      :enable t :map-inject t :exit t)
     ("C-c C-t n" web-mode-tag-next "Fetch next tag. Might be html comment or server tag (e.g. jsp)"
      :enable t :map-inject t :exit t)
     ("C-c C-t p" web-mode-tag-previous "Fetch previous tag"
      :enable t :map-inject t :exit t)
     ("C-c C-t s" web-mode-tag-select "Select the current html tag"
      :enable t :map-inject t :exit t))
    "Misc."
    (("C-c C-f" web-mode-fold-or-unfold "Toggle folding on an html element or a control block"
      :enable t :map-inject t :exit t)
     ("C-c C-i" web-mode-buffer-indent "Indent all buffer"
      :enable t :map-inject t :exit t)
     ("C-c C-j" web-mode-jshint "Run JSHint on all the JavaScript parts"
      :enable t :map-inject t :exit t)
     ("C-c C-l" web-mode-file-link "Insert a link to a file in html document"
      :enable t :map-inject t :exit t)
     ("C-c C-m" web-mode-mark-and-expand "Mark and expand"
      :enable t :map-inject t :exit t)
     ("C-c C-n" web-mode-navigate "Move point to the matching opening/closing tag/block"
      :enable t :map-inject t :exit t)
     ("C-c C-r" web-mode-reload "Reload web-mode"
      :enable t :map-inject t :exit t)
     ("C-c C-s" web-mode-snippet-insert "Insert a snippet"
      :enable t :map-inject t :exit t)
     ("C-c C-w" web-mode-whitespaces-show "Toggle whitespaces"
      :enable t :map-inject t :exit t))))

;; ***** eemacs mmphca
  :eemacs-mmphca
  (((:enable (eq (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
                 'traditional)
     :defer t)
    (web-mode (web-mode web-mode-map)))
   ("Company"
    (("M-t" company-tern "Company Tern"
      :enable t
      :exit t
      :map-inject t)
     ("M-p" company-ac-php-backend "Company Ac Php"
      :enable t
      :exit t
      :map-inject t))))

;; ***** init
  :init

  (entropy/emacs-editor-convention/op/add-mode-hook
    (cons t 'entropy/emacs-web--web-mode-start-hook) nil
    :use-hook 'web-mode-hook :use-timer-cond 'with-current-buffer
    :use-append t
    ;; Set indent and tab-width
    (entropy/emacs-editor-convention/wrapper/do-unless-prop-is-set
      'indent_size nil
      (setq-local web-mode-markup-indent-offset 2)
      (setq-local web-mode-css-indent-offset 4)
      (setq-local web-mode-code-indent-offset 2))
    (entropy/emacs-editor-convention/wrapper/do-unless-prop-is-set
      'indent_style nil
      (setq-local indent-tabs-mode nil))
    (entropy/emacs-editor-convention/wrapper/do-unless-prop-is-set
      'tab_width nil
      (setq-local tab-width 2))
    (progn
      (entropy/emacs-require-only-once 'yasnippet)
      (unless yas-minor-mode
        (yas-minor-mode 1))
      (yas-activate-extra-mode 'php-mode)
      (yas-activate-extra-mode 'js2-mode)
      (yas-activate-extra-mode 'css-mode))
    (web-mode-set-engine "php")
    ;; fake initial value for tern in `web-mode', used for
    ;; `company-tern''s subtroutine.
    (setq-local tern-mode nil))

;; ***** config
  :config
  (when (display-graphic-p)
    (entropy/emacs-add-hook-with-lambda
      'web-mode-enable-development-env (&rest _)
      :use-hook 'web-mode-hook
      :use-append t
      (setq-local entropy/emacs-web-development-environment
                  t))))

;; **** Emmet-mode for quick edittng
(entropy/emacs--inner-use-package emmet-mode
  ;; FIXME: since it use obsolete `cl' package which will cause
  ;; byte-compiler warning
  :eemacs-with-no-require t
  :commands emmet-mode
  :defines emmet-mode-keymap
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :eemacs-mmphca
  (((:enable t :defer t)
    (web-mode (emmet-mode emmet-mode-keymap)))
   ("Basic"
    (("C-j" emmet-expand-line "Emmet expanding (intelligent)"
      :enable t :map-inject t :exit t))))
  :config
  (define-key emmet-mode-keymap (kbd "C-c w") nil))

;; *** CSS mode
(use-package css-mode
  :ensure nil
  :eemacs-mmphc
  (((:enable t :defer t)
    (nil nil t))
   ("Basic"
    (("C-c C-f" css-cycle-color-format "Cycle the color at point between different CSS color formats"
      :enable t :map-inject t :exit t)
     ("C-c C-o" css-lookup-symbol "Display the CSS documentation for SYMBOL, as found on MDN"
      :enable t :map-inject t :exit t))))
  :init
  (entropy/emacs-editor-convention/op/add-mode-hook
    (cons t 'entropy/emacs-web--css-mode-common-setup) nil
    :use-hook 'css-mode-hook :use-timer-cond 'with-current-buffer
    :use-append t
    (entropy/emacs-editor-convention/wrapper/do-unless-prop-is-set
      'indent_size nil
      (setq-local css-indent-offset 2))))

;; *** JSON mode
(use-package json-mode
  :commands (json-mode)
  :eemacs-mmphc
  (((:enable t :defer t)
    (nil nil t))
   ("Basic"
    (("C-c C-p" json-mode-show-path
      "Print the path to the node at point to the minibuffer, and yank to the kill ring"
      :enable t :map-inject t :exit t)
     ("C-c P" json-mode-kill-path "Yank current object key"
      :enable t :map-inject t :exit t)
     ("C-c C-f" json-mode-beautify
      "Beautify / pretty-print the active region (or the entire buffer if no active region)"
      :enable t :map-inject t :exit t)
     ("C-c C-t" json-toggle-boolean
      "If point is on 'true' or 'false', toggle it"
      :enable t :map-inject t :exit t)
     ("C-c C-k" json-nullify-sexp
      "Replace the sexp at point with 'null'"
      :enable t :map-inject t :exit t)
     ("C-c C-i" json-increment-number-at-point
      "Add DELTA to the number at point; DELTA defaults to 1"
      :enable t :map-inject t :exit t)
     ("C-c C-d" json-decrement-number-at-point "Decrement the number at point"
      :enable t :map-inject t :exit t)))))

;; *** js-mode


(defvar entropy/emacs-web--js-suffix-regexp
  "\\.\\(tsx?\\|m?jsx?\\)\\'")

;; emacs builtin `js-mode'
(use-package js
  :ensure nil
  :init
  (unless entropy/emacs-prog/javascript/use-major-mode/js2-mode/p
    (add-to-list 'auto-mode-alist `(,entropy/emacs-web--js-suffix-regexp . js-mode)))
  :config
  ;; using union xref specs
  (dolist (k (list "M-." "M-,"))
    (define-key js-mode-map (kbd k) nil)))

;; Improved JavaScript editing mode
(use-package js2-mode
  :commands (js2-mode)
  :interpreter "node"
  :eemacs-mmphc
  (((:enable
     entropy/emacs-prog/javascript/use-major-mode/js2-mode/p
     :defer t)
    (nil nil t (1 1 3)))
   ("Basic"
    (("C-c C-w" js2-mode-toggle-warnings-and-errors "Toggle the display of warnings and errors"
      :enable t :exit t :map-inject t))
    "Hide and Show"
    (("C-c C-e" js2-mode-hide-element "Fold/hide contents of a block, showing ellipses"
      :enable t :exit t :map-inject t)
     ("C-c C-s" js2-mode-show-element "Show the hidden element at current point"
      :enable t :exit t :map-inject t)
     ("C-c C-o" js2-mode-toggle-element "Hide or show the foldable element at the point"
      :enable t :exit t :map-inject t)
     ("C-c C-f" js2-mode-toggle-hide-functions "Fully hide or show buffer content"
      :enable t :exit t :map-inject t)
     ("C-c C-a" js2-mode-show-all "Show all of the text in the buffer"
      :enable t :exit t :map-inject t)
     ("C-c C-t" js2-mode-toggle-hide-comments "Folds all block comments in the buffer"
      :enable t :exit t :map-inject t))
    "Repl" nil
    "Eval" nil
    "Web Beautify" nil))
  :init
  ;; disable the parse error messy hightlighting to avoid visual
  ;; disturbance.
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (entropy/emacs-lazy-load-simple 'js2-mode
    ;; for backport compatible of js Indentation context parser and
    ;; inntenter for emacs 24 and lower
    (if (< emacs-major-version 25) (require 'js2-old-indent))
    (require 'js2-imenu-extras)
    (entropy/emacs-editor-convention/op/add-mode-hook
      (cons t 'entropy/emacs-web--js2-common-setup) nil
      :use-hook 'js2-mode-hook :use-timer-cond 'with-current-buffer
      :use-append t
      (entropy/emacs-editor-convention/wrapper/do-unless-prop-is-set
        'indent_size nil
        (setq-local js2-basic-offset 4))
      (js2-imenu-extras-mode 1)))

  (if entropy/emacs-prog/javascript/use-major-mode/js2-mode/p
      (add-to-list 'auto-mode-alist
                   `(,entropy/emacs-web--js-suffix-regexp . js2-mode))
    ;; Enable js2 facilities for mordern javascript major-modes which
    ;; for some utilities which rely on `js2-mode' such as
    ;; `skewer-mode', or react jsx/tsx syntax parser.
    (dolist (hook '(js-mode-hook js-ts-mode-hook))
      (add-hook hook #'js2-minor-mode)))

  :config
  ;; using union xref specs
  (dolist (k (list "M-." "M-,"))
    (define-key js2-mode-map (kbd k) nil)))

;; **** js2-refactor
(use-package js2-refactor
  :requires js2-mode
  :commands (js2-refactor-mode)
  :diminish js2-refactor-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; **** Patch
;; ***** [[https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-08/msg00471.html][bug#65134]]
(defvar js--treesit-indent-rules)
(when (and
       (bound-and-true-p
        entropy/emacs-ide-is-treesit-generally-adapted-p)
       (version= emacs-version "29.1"))
  (entropy/emacs-lazy-load-simple 'js
    (let* ((oval (copy-tree (car js--treesit-indent-rules))) rtn)
      (dolist (el oval)
        (if (not (and (listp el) (entropy/emacs-lonely-listp el)
                      (eq (car el) 'js-jsx--treesit-indent-compatibility-bb1f97b)
                      ))
            (push el rtn)
          (dolist (pv (js-jsx--treesit-indent-compatibility-bb1f97b))
            (push pv rtn))))
      (setcar js--treesit-indent-rules (nreverse rtn)))))

;; *** tools
;; **** Live browser JavaScript, CSS, and HTML interaction
;; ***** skewer-mode
(use-package skewer-mode
  :if (executable-find "git")
  :commands (skewer-mode skewer-html-mode skewer-css-mode)
  :diminish (skewer-mode skewer-html-mode skewer-css-mode)
;; ****** config
  :config
;; ******* main patch

  (defun entropy/emacs-web--skewer-has-valid-clients-p (&optional reset)
    (let (vp proc)
      (dolist (el skewer-clients)
        (when (skewer-client-p el)
          (setq proc (skewer-client-proc el))
          (if (and (processp proc) (process-live-p proc))
              (push el vp))))
      (if reset (setq skewer-clients (nreverse vp))
        (nreverse vp))))

  (defun entropy/emacs-web--run-skewer (orig-func &rest orig-args)
    (if (entropy/emacs-web--skewer-has-valid-clients-p 'reset)
        (progn
          (if current-prefix-arg
              ;; when user prefix detected then we respect original
              ;; function
              (apply orig-func orig-args)
            (when (called-interactively-p 'interactive)
              (message "No new clients created \
since existed live skewer client(s) detected.")))
          nil)
      (apply orig-func orig-args)))
  (advice-add 'run-skewer :around #'entropy/emacs-web--run-skewer)

  (defvar entropy/emacs-web--skewer-mode-selector-inner-p nil)
  (defvar-local entropy/emacs-web--skewer-mode-selector-enabled-p nil)
  (defun entropy/emacs-web--skewer-mode-selector
      (orig-func &rest orig-args)
    "Treat all `skewer-mode' variant as the same by filter by
which `major-mode' current is on."
    (if (or entropy/emacs-web--skewer-mode-selector-inner-p
            ;; when disable mode, we treat as origin
            (let ((arg (car orig-args)))
              (and entropy/emacs-web--skewer-mode-selector-enabled-p
                   (eq arg 'toggle))
              (and (numberp arg) (< arg 0))))
        (apply orig-func orig-args)
      (let* ((entropy/emacs-web--skewer-mode-selector-inner-p t)
             (enable-js2
              (lambda nil (and (not (derived-mode-p 'js2-mode))
                               (not (bound-and-true-p js2-minor-mode))
                               (js2-minor-mode))))
             (enable-func
              (lambda (main-func)
                (funcall enable-js2) (funcall main-func)
                (run-skewer))))
        (cl-case
            (entropy/emacs-ide-get-lang-mode-info-plist-attr :lang)
          (javascript
           (funcall enable-func 'skewer-mode)
           (setq entropy/emacs-web--skewer-mode-selector-enabled-p t))
          (html
           (funcall enable-func 'skewer-html-mode)
           (setq entropy/emacs-web--skewer-mode-selector-enabled-p t))
          (css
           (funcall enable-func 'skewer-css-mode)
           (setq entropy/emacs-web--skewer-mode-selector-enabled-p t))
          (t (warn
              "No suitable skewer minor mode can be used for `%s'"
              major-mode))))))
  (dolist (f '(skewer-mode skewer-html-mode skewer-css-mode))
    (advice-add f :around #'entropy/emacs-web--skewer-mode-selector))

  (defun entropy/emacs-web--skewer-return-proper-mode (&optional mode)
    (let ((mode (or mode major-mode)))
      (cl-case
          (entropy/emacs-ide-get-lang-mode-info-plist-attr :lang)
        (javascript 'skewer-mode)
        (html       'skewer-html-mode)
        (css        'skewer-css-mode)
        (t (user-error
            "No suitable skewer minor mode can be used for `%s'"
            mode)))))

;; ******* keymap bind

  (defun entropy/emacs-skewer--load-buffer ()
    (interactive)
    (let ((md (entropy/emacs-web--skewer-return-proper-mode)))
      (cl-case md
        (skewer-mode
         (call-interactively 'skewer-load-buffer))
        (skewer-html-mode
         (user-error "No skewer buffer eval spec for `%s'" md))
        (skewer-css-mode
         (call-interactively 'skewer-css-eval-buffer)))))

  (defun entropy/emacs-skewer--eval-last-expression ()
    (interactive)
    (let ((md (entropy/emacs-web--skewer-return-proper-mode)))
      (cl-case md
        (skewer-mode
         (call-interactively 'skewer-eval-last-expression))
        (skewer-html-mode
         (user-error "No skewer last exp eval spec for `%s'" md))
        (skewer-css-mode
         (call-interactively 'skewer-css-eval-current-declaration)))))

  (defun entropy/emacs-skewer--eval-defun ()
    (interactive)
    (let ((md (entropy/emacs-web--skewer-return-proper-mode)))
      (cl-case md
        (skewer-mode
         (call-interactively 'skewer-eval-defun))
        (skewer-html-mode
         (call-interactively 'skewer-html-eval-tag))
        (skewer-css-mode
         (call-interactively 'skewer-css-eval-current-rule)))))

  (dolist (mp '((skewer-mode . skewer-mode-map)
                (skewer-css  . skewer-css-mode-map)
                (skewer-html . skewer-html-mode-map)))
    (with-eval-after-load (car mp)
      (let ((map (symbol-value (cdr mp))))
        (dolist (kb '(("C-c C-c" . entropy/emacs-skewer--eval-defun)
                      ("C-c C-b" . entropy/emacs-skewer--load-buffer)
                      ("C-x C-e" . entropy/emacs-skewer--eval-last-expression)))
          (define-key map (kbd (car kb)) (cdr kb))))))

;; ****** end
  )

;; ***** impatient-mode
(use-package impatient-mode
  :commands (impatient-mode)
  :init
  (setq impatient-mode-delay 0.3)
;; ****** config
  :config
;; ******* core patch
  (defun entropy/emacs-web--impatient-mode (&rest _)
    "Hook for `impatient-mode' where inovke `imp-visit-buffer' after
enabled that."
    (when (bound-and-true-p impatient-mode)
      (unless (httpd-running-p) (httpd-start))
      (when (derived-mode-p 'web-mode 'html-mode)
        (imp-visit-buffer))))
  (add-hook 'impatient-mode-hook 'entropy/emacs-web--impatient-mode)

;; ****** end

  )

;; **** Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :commands
  (web-beautify-css
   web-beautify-css-buffer
   web-beautify-html
   web-beautify-html-buffer
   web-beautify-js
   web-beautify-js-buffer
   entropy/emacs-web/web-beautify-file)
  :preface
  (defun entropy/emacs-web--check-js-beautify-coworker ()
    (interactive)
    (entropy/emacs-coworker--coworker-isolate-bins-install-by-npm
     "js-beautify" '("css-beautify" "html-beautify" "js-beautify")
     "js-beautify"))

  :eemacs-mmphca
  ((((:enable t :defer t)
     (js2-mode (js2-mode js2-mode-map)))
    ("Web Beautify"
     (("C-c C-b" web-beautify-js "Beautify Js"
       :enable (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
       :exit t
       :map-inject t))))
   (((:enable t)
     (json-mode (json-mode json-mode-map)))
    ("Web Beautify"
     (("C-c C-b" web-beautify-js "Beautify Json"
       :enable (eq (entropy/emacs-get-use-ide-type 'json-mode) 'traditional)
       :exit t
       :map-inject t))))
   (((:enable t)
     (web-mode (web-mode web-mode-map)))
    ("Web Beautify"
     (("C-c C-0" web-beautify-html "Beautify html"
       :enable (eq (entropy/emacs-get-use-ide-type 'web-mode) 'traditional)
       :exit t
       :map-inject t))))
   (((:enable t)
     (nxml-mode (nxml-mode nxml-mode-map)))
    ("Web Beautify"
     (("C-c C-b" web-beautify-html "Beautify Xml"
       :enable (eq (entropy/emacs-get-use-ide-type 'nxml-mode) 'traditional)
       :exit t
       :map-inject t))))
   (((:enable t)
     (css-mode (css-mode css-mode-map)))
    ("Web Beautify"
     (("C-c C-b" web-beautify-html "Beautify Css"
       :enable (eq (entropy/emacs-get-use-ide-type 'css-mode) 'traditional)
       :exit t
       :map-inject t)))))

  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-"))

  (defun entropy/emacs/web-beautify--interactive-warn (&rest _)
    "Prompt user when use `web-beautify-*' interactive command in
non-proper buffer."
    (let ((cmd this-command) (buf (current-buffer)))
      (when (and (called-interactively-p 'interactive)
                 (not (derived-mode-p 'css-mode 'js-mode 'js2-mode 'web-mode)))
        (unless (yes-or-no-p
                 (format "Really invoke `%s' in buffer `%s' \
whose major-mode is `%s' which may make messy?"
                         cmd buf major-mode))
          (user-error "Abort command `%s' for buffer `%s'"
                      cmd buf)))))

  (dolist (func (list 'web-beautify-html 'web-beautify-css 'web-beautify-js))
    (advice-add func :before #'entropy/emacs/web-beautify--interactive-warn))

  (defun entropy/emacs-web/web-beautify-file (file)
    "Do web-beautify-* for FILE."
    (interactive
     (list
      (entropy/emacs-read-file-name-only-for
       "choose file: "
       :with-only-for
       (lambda (x)
         (if (and (file-exists-p x) (not (file-directory-p x))) t
           "(not a existed file) choose file: ")))))
    (let ((f file)
          (inhibit-read-only t)
          (make-backup-files t)
          (write-contents-functions nil)
          (write-file-functions nil)
          (before-save-hook nil)
          ftype)
      (cond
       ((string-match-p "\\.\\(js\\|ts\\|json\\)$" f)
        (setq ftype "js"))
       ((string-match-p "\\.\\(css\\|sass\\)$" f)
        (setq ftype "css"))
       ((string-match-p "\\.\\(html?\\|xml\\)$" f)
        (setq ftype "html"))
       (t (setq ftype (completing-read
                       "Which type of this file is?: "
                       (list "css" "html" "jss")
                       nil t))))
      (setq ftype (cond
                   ((string= "js" ftype) 'web-beautify-js-buffer)
                   ((string= "css" ftype) 'web-beautify-css-buffer)
                   ((string= "html" ftype) 'web-beautify-html-buffer)))
      (entropy/emacs-message-simple-progress-message
       (format "do `%s' for file %s" ftype f)
       :with-temp-message t
       (with-current-buffer (find-file-noselect f)
         (funcall ftype) (save-buffer) (kill-buffer)))))

  )

;; ** web backend technologies
;; *** php
(entropy/emacs--inner-use-package php-mode
  ;; FIXME: since its dups byte-compile warning : Function provided is already compiled
  :eemacs-with-no-require t
  :mode "\\.php$"
  :commands php-mode)

;; * provide
(provide 'entropy-emacs-web)
