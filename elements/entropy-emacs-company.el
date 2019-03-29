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

;; ** main
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode)
;; ** bind-key  
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
;; ** init for load  
  :init
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
      (require el)))
  
  (add-hook 'entropy/init-X-hook #'global-company-mode)
  (advice-add 'company-complete :before #'(lambda (&rest args)
                                            (if (not (condition-case error
                                                         (symbol-value yas-global-mode)
                                                       (error nil)))
                                                (yas-global-mode))))
;; ** config for after-load
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq
   ;; company-idle-delay nil
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

;; ** Popup documentation for completion candidates
  (with-eval-after-load 'company
    (use-package company-quickhelp
      :if (and (not entropy/company-posframe-mode)
               (display-graphic-p))
      :commands (company-quickhelp-mode
                 company-quickhelp-manual-begin)
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :init (company-quickhelp-mode 1)
      :config (setq company-quickhelp-delay 1))
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "<f1>") nil))

;; ** Support yas in commpany

  ;;  Note: Must be the last to involve all backends
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (with-eval-after-load 'company
    (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

;; ** using company-words

  ;; Note: this pakcages has the bug of error when be completion for words longer than some chars
  (use-package company-en-words
    :ensure nil
    :commands company-en-words
    :bind ("M-]" . company-en-words))

;; ** Using company-posframe to speedup company candidates window show and scrolling
  (when (and (not (version< emacs-version "26.1"))
             entropy/company-posframe-mode)
    (with-eval-after-load 'company
      (use-package company-posframe
        :commands (company-posframe-mode)
        :diminish company-posframe-mode
        :init (company-posframe-mode 1)))))


;; * provide
(provide 'entropy-emacs-company)
