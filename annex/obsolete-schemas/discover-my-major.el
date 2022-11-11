;; Discover key bindings and their meaning for the current Emacs major mode

(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (after-change-major-mode-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (discover-my-major))
   ("Discover major-mode map"
    (("C-h M-m" discover-my-major "Create a makey popup listing all major-mode"
      :enable t :exit t :global-bind t)
     ("C-h M-M" discover-my-mode "Create a makey popup listing all minor-mode"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (after-change-major-mode-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Basic"
    (("b d"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'discover-my-major))
      "Discover modes key-bindings"
      :enable t))))

  :config

  (defvar entropy/emacs-tools--dmm-sections-log nil)

  (defun entropy/emacs-tools--dmm-prune-sections (dmm-sections)
    (setq entropy/emacs-tools--dmm-sections-log nil)
    (let ((dmm-sections-copy (copy-sequence dmm-sections))
          (rules '("next-line" "previous-line" "left-char" "right-char"
                   "beginning-of-line" "end-of-line"))
          rtn)
      (setq entropy/emacs-tools--dmm-sections-log dmm-sections-copy)
      (dolist (section dmm-sections-copy)
        (let* ((group-name (car section))
               (bindings (cdr section))
               (cpbdins (copy-sequence bindings))
               cache)
          (dolist (binding cpbdins)
            (unless (or (entropy/emacs-string-match-p
                         (cdr binding) rules)
                        (entropy/emacs-string-match-p
                         (car binding) rules))
              (push binding cache)))
          (push (nconc (list group-name) (nreverse cache)) rtn)))
      (nreverse rtn)))

  (defun entropy/emacs-tools--dmm-adv-for-section-builder (orig-func &rest orig-args)
    (let ((dmm-sections (apply orig-func orig-args)))
      (entropy/emacs-tools--dmm-prune-sections dmm-sections)))

  (advice-add 'dmm/descbinds-all-sections
              :around
              #'entropy/emacs-tools--dmm-adv-for-section-builder))
