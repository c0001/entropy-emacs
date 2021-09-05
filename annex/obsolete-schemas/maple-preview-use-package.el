(use-package maple-preview
  :if (and
       ;; disable defaulty since its obsolete almostly about 3 years
       ;; since 2018, but still can be used to scratch studying.
       nil
       (equal entropy/emacs-ext-elpkg-get-type 'entropy-emacs-extenisons-project))
  :ensure nil
  :commands (maple-preview-mode)
  :init
  (setq maple-preview:allow-modes '(org-mode markdown-mode gfm-mode html-mode web-mode))
  :config
  (defun entropy/emacs-tools-maple-preview:schema-auto-hooks ()
    (dolist ($el '(windmove-left
                   windmove-right
                   windmove-up
                   windmove-down))
      (advice-add $el :after #'maple-preview:send-to-server))
    (entropy/emacs-lazy-load-simple eyebrowse
      (advice-add 'eyebrowse-switch-to-window-config
                  :after
                  #'maple-preview:send-to-server))
    (advice-add 'other-window :after #'maple-preview:send-to-server)
    (entropy/emacs-lazy-load-simple markdown-mode
      (advice-add 'markdown-outdent-or-delete
                  :after
                  #'maple-preview:send-to-server))
    (advice-add 'backward-delete-char-untabify
                :after #'maple-preview:send-to-server))

  (defun entropy/emacs-tools--maple-preview:schema-finialize-hooks ()
    (dolist ($el '(windmove-left
                   windmove-right
                   windmove-up
                   windmove-down))
      (advice-remove $el #'maple-preview:send-to-server))
    (when (fboundp 'eyebrowse-switch-to-window-config)
      (advice-remove 'eyebrowse-switch-to-window-config
                     #'maple-preview:send-to-server))
    (advice-remove 'other-window #'maple-preview:send-to-server)
    (when (fboundp 'markdown-outdent-or-delete)
      (advice-remove 'markdown-outdent-or-delete
                     #'maple-preview:send-to-server))
    (advice-remove 'backward-delete-char-untabify
                   #'maple-preview:send-to-server))

  (add-hook 'maple-preview:auto-hook
            #'entropy/emacs-tools-maple-preview:schema-auto-hooks)
  (add-hook 'maple-preview:finialize-hook
            #'entropy/emacs-tools--maple-preview:schema-finialize-hooks))
