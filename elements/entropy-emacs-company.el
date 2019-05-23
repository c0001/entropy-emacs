;;; File name: init-company.el ---> for entropy-emacs
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

;; ** defvar


;; ** libraries
(defun entropy/emacs-company-use-yasnippet (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun entropy/emacs-company-start-with-yas (&rest _)
  (if (not (condition-case error
               (symbol-value yas-global-mode)
             (error nil)))
      (when (fboundp 'yas-global-mode)
        (yas-global-mode))))

(defun entropy/emacs-company-require-subs ()
  (with-eval-after-load 'company
    (dolist (el '(company-abbrev
                  company-bbdb
                  company-capf
                  company-clang
                  company-cmake
                  company-css
                  company-dabbrev-code
                  company-dabbrev
                  company-eclim
                  company-elisp
                  company-etags
                  company-files
                  company-gtags
                  company-ispell
                  company-keywords
                  company-nxml
                  company-oddmuse
                  company-semantic
                  company-template
                  company-tempo
                  company-tng
                  company-xcode
                  company-yasnippet))
      (require el))))


;; ** company core
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode)
;; *** bind-key  
  :bind (("M-/" . company-complete)
         ("M-\\" . company-dabbrev)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ;; ("<tab>" . company-complete-selection)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
;; *** init for load  
  :init
  (add-hook 'entropy/init-X-hook #'global-company-mode)
  (entropy/emacs-company-require-subs)
  (advice-add 'company-complete :before 'entropy/emacs-company-start-with-yas)
  
;; *** config for after-load
  :config
;; **** basic setting 
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq
   company-tooltip-limit 20  ; bigger popup window
   company-echo-delay 0      ; remove annoying blinking
   company-dabbrev-code-everywhere t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-dabbrev-char-regexp "\\sw\\(-\\w\\|_\\w\\)?*\\(\\.\\w\\)?*")

  (if entropy/company-posframe-mode
      (setq company-tooltip-offset-display 'scrollbar)
    (setq company-tooltip-offset-display 'lines))

;; **** Support yas in commpany
  (setq company-backends (mapcar #'entropy/emacs-company-use-yasnippet company-backends))

;; **** Using company-posframe to speedup company candidates window show and scrolling
  (when (and (not (version< emacs-version "26.1"))
             entropy/company-posframe-mode)
    (use-package company-posframe
      :after company
      :commands (company-posframe-mode)
      :diminish company-posframe-mode
      :init (company-posframe-mode 1))))


;; *** Popup documentation for completion candidates
  (use-package company-quickhelp
    :if (and (not entropy/company-posframe-mode)
             (display-graphic-p))
    :after company
    :defines company-quickhelp-delay
    :commands (company-quickhelp-mode
               company-quickhelp-manual-begin)
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin)
                ("c-h" . nil)
                ("<f1>" . nil))
    :init
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))

;; ** company lsp
(use-package company-lsp
  :if (and (>= emacs-major-version 25)
           entropy/company-lsp)
  :init
  (add-hook 'prog-mode-hook #'entropy/emacs-company-add-lsp-backend)
  (defun entropy/emacs-company-add-lsp-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-lsp) company-backends)))

;; ** Backends
;; *** miscelloneous
;; **** englishs dict quick completion
(use-package company-en-words
  :after company
  :ensure nil
  :commands company-en-words
  :bind ("M-]" . company-en-words))

;; *** web refer
;; **** web/html&css
;; ***** lsp
;; ***** traditional
(use-package company-web
  :if (not entropy/company-lsp)
  :after company
  :defines (company-web-html
            company-web-jade
            company-web-slim
            web-mode-hook
            css-mode-hook)
  :commands company-web
  :init
  (add-hook 'web-mode-hook #'entropy/emacs-company-web-add-all-backends)
  (add-hook 'css-mode-hook #'entropy/emacs-company-web-add-css-backend)
  
  (defun entropy/emacs-company-web-add-html-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-html) company-backends))
  (defun entropy/emacs-company-web-add-jade-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-jade) company-backends))
  (defun entropy/emacs-company-web-add-slim-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-web-slim) company-backends))
  (defun entropy/emacs-company-web-add-css-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-css) company-backends))
  (defun entropy/emacs-company-web-add-all-backends ()
    (entropy/emacs-company-web-add-html-backend)
    (entropy/emacs-company-web-add-jade-backend)
    (entropy/emacs-company-web-add-slim-backend)
    (entropy/emacs-company-web-add-css-backend)))

;; **** javascript
;; ***** lsp
;; ***** traditional
(use-package company-tern
  :if (not entropy/company-lsp)
  :after company
  :defines js2-mode-hook
  :commands company-tern
  :init
  (add-hook 'js2-mode-hook #'entropy/emacs-company-tern-add-tern-backend)
  (defun entropy/emacs-company-tern-add-tern-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-tern) company-backends)))

;; **** php
;; ***** lsp
;; ***** traditional
(use-package company-php
  :if (not entropy/company-lsp)
  :defines php-mode-hook
  :commands company-ac-php-backend
  :init
  (defun entropy/emacs-company-ac-php-add-acphp-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-ac-php-backend) company-backends))
  (add-hook 'php-mode-hook #'entropy/emacs-company-ac-php-add-acphp-backend))

;; *** C(PP) Java python
;; **** C(PP)
;; ***** lsp
;; ***** traditional
(use-package company-c-headers
  :if (not entropy/company-lsp)
  :after company
  :defines (c-mode-hook c++-mode-hook)
  :commands company-c-headers
  :init
  (add-hook 'c-mode-hook 'entropy/emacs-company-c-headers-add-cheader-backend)
  (add-hook 'c-mode-hook 'entropy/emacs-company-c-headers-add-cheader-backend)
  (defun entropy/emacs-company-c-headers-add-cheader-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-c-headers) company-backends)))


(use-package company-irony
  :if (not entropy/company-lsp)
  :after company
  :defines (c-mode-hook c++-mode-hook)
  :commands commpany-irony
  :init
  (add-hook 'c-mode-hook 'entropy/emacs-company-irony-add-irony-backend)
  (add-hook 'c++-mode-hook 'entropy/emacs-company-irony-add-irony-backend)
  (defun entropy/emacs-company-irony-add-irony-backend ()
    "Make local company-backends with yasnippet for irony in c
and c++ mode."
    (make-local-variable 'company-backends)
    (when  (or (eq major-mode 'c-mode)
               (eq major-mode 'c++-mode))
      (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-irony) company-backends))))

;; ****Java
;; ***** lsp
;; ***** traditional
;; **** Python
;; ***** lsp
;; ***** traditional
(use-package company-anaconda
  :if (not entropy/company-lsp)
  :after company
  :defines anaconda-mode-hook
  :commands company-anaconda
  :init
  (add-hook 'anaconda-mode-hook #'entropy/emacs-company-anaconda-add-anaconda-backend)
  (defun entropy/emacs-company-anaconda-add-anaconda-backend ()
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-anaconda) company-backends)))




;; * provide
(provide 'entropy-emacs-company)
