;;; File name: init-web.el ---> for entropy-emacs
;;
;; Copyright (c) 2017 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'cl)

;; ** main
(defun entropy/creat-js2mode-tern-project-file ()
  "Auto create '.tern-project' file in current dir.

  Notice: this automatically created file was simple, you should
  modify it by personal customization.

  And this automatically created file was the file within
  entropy-emacs."
  (interactive)
  (if (file-exists-p ".tern-project")
      (message "Already have .tern-project :)")
    (if (file-exists-p (expand-file-name "annex/.tern-project" user-emacs-directory))
        (progn
          (copy-file (expand-file-name "annex/.tern-project" user-emacs-directory) "./.tern-project")
          (message "Succeed to create .tern-project in this folder!"))
      (message "Can not find origin .tern-project file from your .emacs.d/annex folder"))))

(defun entropy/url-transfer-region-entities ()
  "Transfer region text into entities form and push it into
kill-ring.

You can insert it by directly use `yank'"
  (interactive)
  (let* ((region (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   nil))
         rtn)
    (if region
        (progn (setq rtn (url-insert-entities-in-string region))
               (with-temp-buffer
                 (if buffer-read-only
                     (read-only-mode 0))
                 (let (p1 p2)
                   (insert (replace-regexp-in-string "\"" "\"" rtn))
                   (setq p1 (point-min)
                         p2 (point-max))
                   (kill-ring-save p1 p2)))))))

(with-eval-after-load 'tern
  (setq tern-command '("tern")))

;; ** Front
;; *** html
;; **** web-mode
(use-package web-mode
  :commands (web-mode)
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
;; ***** init
  :init
  (add-hook 'web-mode-hook
            '(lambda ()
               ;; Set indent and tab-width
               (setq web-mode-markup-indent-offset 4)
               (setq web-mode-css-indent-offset 4)
               (setq web-mode-code-indent-offset 4)
               (setq indent-tabs-mode nil)
               (setq tab-width 4)
               (with-eval-after-load 'yasnippet
                 (yas-activate-extra-mode 'php-mode)
                 (yas-activate-extra-mode 'js2-mode)
                 (yas-activate-extra-mode 'css-mode))
               (web-mode-set-engine "php")
               (setq-local tern-mode nil)))
;; ***** config
  :config
  (if (or sys/win32p sys/linux-x-p sys/mac-x-p)
      (progn
        (add-hook 'web-mode-hook #'(lambda () (setq-local entropy/web-development-environment t)))
	(defun entropy/browse-url-of-buffer ()
	  (interactive)
          (require 'entropy-common-library-const)
          (let* (url)
            (if (and buffer-file-name
                     (file-exists-p buffer-file-name))
                (progn
                  (setq url (url-hexify-string (concat "file://" buffer-file-name)
                                               entropy/cl-url--allowed-chars))
                  (browse-url url)))))

        ;; Preview the html file in external browser
        (define-key web-mode-map (kbd "<C-f1>") 'entropy/browse-url-of-buffer)))

;; ****** company config
;; ******* js company-config for using tern or lsp
  (defun entropy/wm-company-tern (command)
    "This function was to automaticly init `tern-mode' and pop-up
`company-tern'."
    (interactive (list 'interactive))
    (when (or (not (boundp 'tern-mode))
              (not tern-mode))
      (use-package tern
        :commands (tern-mode))
      (use-package company-tern
        :commands (company-tern))
      ;; (require 'tern)
      ;; (require 'company-tern)
      )
    (if (not tern-mode)
        (progn
          (entropy/creat-js2mode-tern-project-file)
          (tern-mode t))
      (funcall 'company-tern command)))
  (when (not entropy/company-lsp)
    (define-key web-mode-map (kbd "M-t") 'entropy/wm-company-tern))

;; ******* php company config and toggle function of between web or php mode
  (use-package ac-php-core)
  (use-package company-php
    :commands (company-ac-php-backend))
  (define-key web-mode-map (kbd "M-p") 'company-ac-php-backend)
 
  (define-key web-mode-map (kbd "<f5>") 'entropy/toggle-php-flavor-mode)
;; ******* Use company-web
  (add-hook 'web-mode-hook
            (lambda ()
	      (make-local-variable 'company-backends)
	      (with-eval-after-load 'company
		(use-package company-web
                  :commands (company-web)
		  :init
		  (cl-pushnew (company-backend-with-yas 'company-web-html) company-backends)
		  (cl-pushnew (company-backend-with-yas 'company-web-jade) company-backends)
		  (cl-pushnew (company-backend-with-yas 'company-web-slim) company-backends)
                  (cl-pushnew (company-backend-with-yas 'company-css) company-backends))))))

;; **** Use emmet-mode for folding code
(use-package emmet-mode
  ;; :after (web-mode)
  :commands (emmet-mode)
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (define-key emmet-mode-keymap (kbd "C-c w") nil))

;; *** CSS mode
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; *** JSON mode
(use-package json-mode
  :commands (json-mode))

;; *** js-mode
;; Improved JavaScript editing mode
(use-package js2-mode
  :commands (js2-mode)
  :mode "\\.js$"
  :interpreter "node"
  :init
  (with-eval-after-load 'js2-mode
    (require 'js2-old-indent)
    (require 'js2-imenu-extras)
    (add-hook 'js2-mode-hook
              (lambda ()
                (setq js2-basic-offset 4)
                (js2-highlight-unused-variables-mode 1)
                (js2-imenu-extras-mode 1))))
  :config
;; **** js2-refactor
  (use-package js2-refactor
    :commands (js2-refactor-mode)
    :diminish js2-refactor-mode
    :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m"))
  
;; **** js2 company config

  ;; If not use company-lsp then use company-tern
  (if (not entropy/company-lsp)
      (progn
        (use-package company-tern
          :commands (company-tern))
        (use-package tern
          :commands (tern-mode))
        (add-hook
         'js2-mode-hook
         #'(lambda ()
             (entropy/creat-js2mode-tern-project-file)
             (tern-mode t)
             (make-local-variable 'company-backends)
             (with-eval-after-load 'company
               (cl-pushnew (company-backend-with-yas 'company-tern) company-backends)))))))

;; *** tools
;; **** Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :if (executable-find "git")
  :commands (skewer-mode skewer-html-mode skewer-css-mode)
  :diminish (skewer-mode skewer-html-mode skewer-css-mode)
  :init
  (with-eval-after-load 'skewer-mode
    (dolist (el '(cache-table
                  skewer-bower
                  skewer-css
                  skewer-html
                  skewer-repl
                  skewer-setup))
      (require el)))
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook #'skewer-mode))
  (with-eval-after-load 'css-mode
    (add-hook 'css-mode-hook #'skewer-css-mode))
  (with-eval-after-load 'sgml-mode
    (add-hook 'web-mode-hook #'skewer-html-mode)))

(use-package impatient-mode
  :commands (impatient-mode entropy/impatient-mode)
  :config
  (require 'entropy-common-library)

  (defun entropy/impatient-mode ()
    "Enable `impatient-mode' and http server by `httpd-start' if
    server not actived and open the impatient url
    \"http://localhost:8080/imp/\" with file-name of current
    buffer if current file was html file."
    (interactive)
    (let* ((buffer (buffer-name (current-buffer))))
      (if (and (boundp 'impatient-mode)
               (not impatient-mode))
          (progn
            (if (not (httpd-running-p))
                (httpd-start))
            (impatient-mode)))
      (if (string-match-p "\\.html" buffer)
          (browse-url (concat "http://localhost:8080/imp/live/"
                              (file-name-nondirectory (buffer-name)))))))
  (advice-add 'impatient-mode :before 'entropy/cl-sn-buffer-p))
;; **** Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :commands
  (web-beautify-css
   web-beautify-css-buffer
   web-beautify-html
   web-beautify-html-buffer
   web-beautify-js
   web-beautify-js-buffer)
  :init
  (with-eval-after-load 'js2-mode
    (bind-key "C-c C-b" 'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c C-b" 'web-beautify-js json-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c C-b" 'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c C-b" 'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

;; ** End
;; *** php
(use-package php-mode
  :mode "\\.php$"
  :commands (php-mode)
  :config
  (use-package ac-php
    :commands (ac-php-core-eldoc-setup)
    :init
    (add-hook 'php-mode-hook '(lambda () (ac-php-core-eldoc-setup)))
    (add-hook 'php-mode-hook
  	      '(lambda () 
  	         (setq indent-tabs-mode nil)
  	         (setq c-basic-offset 4)
  	         ;;(setq php-template-compatibility nil)
  	         (subword-mode 1)))

    (with-eval-after-load 'company
      (use-package company-php
        :commands (company-ac-php-backend))
      (define-key php-mode-map (kbd "M-p") 'company-ac-php-backend)
      (define-key php-mode-map (kbd "<f5>") 'entropy/toggle-php-flavor-mode))))

;; **** php-web-exchage-flavour
(defun entropy/toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string-match-p "PHP/" mode-name)
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))



;; * provide
(provide 'entropy-emacs-web)
