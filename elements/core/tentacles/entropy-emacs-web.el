;;; entropy-emacs-web.el --- entropy-emacs web development configuration
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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))
(require 'entropy-emacs-coworker)
(require 'entropy-emacs-utils)
(require 'entropy-emacs-hydra-hollow)

;; ** Preparation
(entropy/emacs-lazy-load-simple tern
  (setq tern-command '("tern")))

;; ** main libraries
(defun entropy/emacs-web-browse-web-buffer ()
  (interactive)
  (require 'entropy-common-library-const)
  (let* (url)
    (if (and buffer-file-name
             (file-exists-p buffer-file-name))
        (progn
          (setq url
                (url-hexify-string
                 (concat "file://" buffer-file-name)
                 entropy/cl-url--allowed-chars))
          (browse-url url)))))

(defun entropy/emacs-web--web-mode-start-hook ()
  ;; Set indent and tab-width
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (progn
    (require 'yasnippet)
    (unless yas-minor-mode
      (yas-minor-mode))
    (yas-activate-extra-mode 'php-mode)
    (yas-activate-extra-mode 'js2-mode)
    (yas-activate-extra-mode 'css-mode))
  (web-mode-set-engine "php")
  ;; fake initial value for tern in `web-mode', used for
  ;; `company-tern''s subtroutine.
  (setq-local tern-mode nil))

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
  (((:enable t)
    (web-mode (web-mode web-mode-map) t (1 2 1 1 2)))
   ("Basic"
    (("<f1>" entropy/emacs-web-browse-web-buffer "Preview Current Buffer"
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
                 'traditional))
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
  (add-hook 'web-mode-hook
            'entropy/emacs-web--web-mode-start-hook)

;; ***** config
  :config
  (when (display-graphic-p)
    (entropy/emacs-add-hook-lambda-nil
     web-mode-enable-development-env web-mode-hook 'append
     (setq-local entropy/emacs-web-development-environment
                 t))))

;; **** Emmet-mode for quick edittng
(use-package emmet-mode
  :commands emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :eemacs-mmphca
  (((:enable t)
    (web-mode (emmet-mode emmet-mode-keymap)))
   ("Basic"
    (("C-j" emmet-expand-line "Emmet expanding (intelligent)"
      :enable t :map-inject t :exit t))))
  :config
  (define-key emmet-mode-keymap "C-c w" nil))

;; *** CSS mode
(use-package css-mode
  :ensure nil
  :eemacs-mmphc
  (((:enable t)
    (nil nil t))
   ("Basic"
    (("C-c C-f" css-cycle-color-format "Cycle the color at point between different CSS color formats"
      :enable t :map-inject t :exit t)
     ("C-c C-o" css-lookup-symbol "Display the CSS documentation for SYMBOL, as found on MDN"
      :enable t :map-inject t :exit t))))
  :init (setq css-indent-offset 2))

;; *** JSON mode
(use-package json-mode
  :commands (json-mode)
  :eemacs-mmphc
  (((:enable t)
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
;; Improved JavaScript editing mode
(use-package js2-mode
  :commands (js2-mode)
  :mode "\\.js$"
  :interpreter "node"
  :eemacs-mmphc
  (((:enable t)
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
  (entropy/emacs-lazy-load-simple js2-mode
    (require 'js2-old-indent)
    (require 'js2-imenu-extras)
    (entropy/emacs-add-hook-lambda-nil
     js2-initialized-common js2-mode-hook 'append
     (setq-local js2-basic-offset 4)
     (js2-highlight-unused-variables-mode 1)
     (js2-imenu-extras-mode 1))))

;; **** js2-refactor
(use-package js2-refactor
  :requires js2-mode
  :commands (js2-refactor-mode)
  :diminish js2-refactor-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; *** tools
;; **** Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :if (executable-find "git")
  :commands (skewer-mode skewer-html-mode skewer-css-mode)
  :diminish (skewer-mode skewer-html-mode skewer-css-mode)
  :init
  (entropy/emacs-lazy-load-simple skewer-mode
    (dolist (el '(cache-table
                  skewer-bower
                  skewer-css
                  skewer-html
                  skewer-repl
                  skewer-setup))
      (require el)))
  (entropy/emacs-lazy-load-simple js2-mode
    (add-hook 'js2-mode-hook #'skewer-mode))
  (entropy/emacs-lazy-load-simple css-mode
    (add-hook 'css-mode-hook #'skewer-css-mode))
  (entropy/emacs-lazy-load-simple sgml-mode
    (add-hook 'web-mode-hook #'skewer-html-mode)))

(use-package impatient-mode
  :commands (impatient-mode)
  :preface
  (defun entropy/emacs-web-impatient-mode ()
    "Enable `impatient-mode' and http server by `httpd-start' if
server not actived and open the impatient url
\"http://localhost:8080/imp/\" with file-name of current buffer
if current file was html file."
    (interactive)
    (require 'entropy-common-library)
    (require 'impatient-mode)
    (let* ((buffer_n (buffer-name (current-buffer))))
      (cond
       ((and (boundp 'impatient-mode)
             impatient-mode)
        (impatient-mode 0))
       ((and (boundp 'impatient-mode)
             (not impatient-mode))
        (unless (ignore-errors (httpd-running-p))
          (httpd-start))
        (impatient-mode 1)
        (when (string-match-p "\\.html" buffer_n)
          (imp-visit-buffer))))))
  :init (setq impatient-mode-delayed-update nil)
  :config
  (advice-add 'impatient-mode :before 'entropy/cl-sn-buffer-p)
  (defun imp-visit-buffer (&optional arg)
    "Visit the current buffer in a browser.
If given a prefix argument, visit the buffer listing instead.

Notice: this function has been modified to patch with host name
format."
    (interactive "P")
    (unless (process-status "httpd")
      (httpd-start))
    (unless impatient-mode
      (impatient-mode))
    (let ((url (format "http://localhost:%d/imp/" httpd-port)))
      (unless arg
        (setq url (format "%slive/%s/" url (url-hexify-string (buffer-name)))))
      (browse-url url))))

;; **** Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :commands
  (web-beautify-css
   web-beautify-css-buffer
   web-beautify-html
   web-beautify-html-buffer
   web-beautify-js
   web-beautify-js-buffer)
  :preface
  (defun entropy/emacs-web--check-js-beautify-coworker ()
    (interactive)
    (entropy/emacs-coworker--coworker-install-by-npm
     "js-beautify" ("css-beautify" "html-beautify" "js-beautify")
     "js-beautify"))

  :eemacs-mmphca
  ((((:enable t)
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

  ;; install `js-beautify' coworker
  (when entropy/emacs-install-coworker-immediately
    (advice-add
     el
     :before
     #'entropy/emacs-web--check-js-beautify-coworker)))

;; ** web backend technologies
;; *** php
(use-package php-mode
  :mode "\\.php$"
  :commands php-mode)

;; * provide
(provide 'entropy-emacs-web)
