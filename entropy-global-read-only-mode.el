;;; File name: entropy-global-read-only-mode.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; * Code:
;; ** declare variable
(defvar entropy/grom-toggle-except-bfregexp-list
  '(
    "\\*Minibuf.*\\*"
    "\\*Echo Area"
    "\\*code-conversion-work\\*"
    "\\*Kill Ring"
    "\\*Help\\*"
    "\\*Backtrace\\*"
    "\\*Messages\\*"
    "\\*scratch\\*"
    "\\*which-key"
    "\\*tip\\*"
    "\\*Agenda Commands\\*"
    "\\*urlparse-temp\\*"
    "\\*Org todo\\*"
    "\\*Org Agenda\\*"
    "magit-process:"
    "magit:"
    "\\*clang-output\\*"
    "\\*shell"
    "\\*terminal\\*"
    "\\*ansi-term"
    "CAPTURE-.*\\.org$"
    "\\*company-posframe-buffer\\*"
    "\\*entity\\*"
    "\\*w3m-"
    "\\*nntp"
    "newsrc"
    "\\*gnus"
    "COMMIT_EDITMSG"
    "_archive"
    "^ *\\*.*?\\* *$"
    )
  "The list is used by `entropy/grom-toggle-read-only'
for excluding buffer with BUFFER-NAME REGXP in this list.")

(defvar entropy/grom-agenda-true-name-list nil
  "This list contains the FILENAME of `org-agenda-files' and
    obtained by `entropy/grom-get-true-agenda-file-name-list'")

(defvar entropy/grom-buffer-true-name-pair-list nil
  "This list contains the FILENAME of the all buffer name
    BUFFER-NAME in current frame and obtained by
    `entropy/grom-get-true-buffer-name-pair-list'")


(defvar entropy/grom-org-unlock-file-list nil
  "This list contains buffer name BUFFERNAME which mirrored
    with `org-agenda-files' in current frame, and optained by
    `entropy/grom-get-unlock-agenda-buffers-list'")


(defvar entropy/grom-current-agenda-buffer nil
  "Current manipulation agenda buffer with `org-agenda'.

Note: Don't manually assign value to this variable. 

This variable will be auto-clean when agenda manipulation
finished.")


;; ** custom variable
(defgroup entropy-global-read-only-mode nil
  "group for global-readonly-mode.")

(defcustom entropy/grom-enable nil
  "Enable global-readonly-mode"
  :type 'boolean
  :group 'entropy-global-read-only-mode)

(defcustom entropy/grom-readonly-type "all"
  "
Choose the type of init-read-only method:

There's two choice:
1. 'all':   Let find-file be default the read-only mode.
2. 'modes': Use the list of major-modes' hooks to embend it in.
            The list variable is `entropy/grom-mode-list'
"
  :type 'string
  :group 'entropy-global-read-only-mode)

(defcustom entropy/grom-mode-list
  '(emacs-lisp-mode-hook
    c-mode-hook
    php-mode-hook
    web-mode-hook
    python-mode-hook
    js2-mode-hook
    css-mode-hook
    org-mode-hook
    json-mode-hook
    markdown-mode-hook
    bat-mode-hook
    text-mode-hook)
  "The list of which major mode to be read-only at start-up"
  :type 'sexp
  :group 'entropy-global-read-only-mode)

(defcustom entropy/grom-find-file-except-bfregexp-list
  `(,(regexp-quote "autoloads.el")
    "\\*Compile-.*\\*"
    "loaddefs\\.el"
    "COMMIT_EDITMSG"
    "_archive"
    "^ *\\*.*?\\* *$")
  "Except buffer name list for `entropy/grom-find-file-hook'."
  :type 'sexp
  :group 'entropy-global-read-only-mode)


;; ** Read-only minor tools
;; *** Buffer lock quick way with <f1>
(global-set-key (kbd "<f1>") 'entropy/grom-read-only-buffer)
(defun entropy/grom-read-only-buffer ()
  (interactive)
  (if buffer-read-only
      (if (yes-or-no-p "Do you really want to quit <Readonly Mode> ?")
          (read-only-mode 0))
    (if (and (not (string-match-p "\\*Minibuf.*\\*" (buffer-name)))
             (not (string-match-p "CAPTURE-.*.org$" (buffer-name))))
        (progn
          (read-only-mode 1)
          (message "Lock this buffer succesfully"))
      (message "You are in %s , don't read-only it!" (buffer-name)))))


;; *** Global read only toggle function
(defun entropy/grom-toggle-read-only (&optional readonly editted current-buffer-ndwp cury)
  "Toggle readonly-or-not for all buffers except for the buffer-name witin `entropy/grom-toggle-except-bfregexp-list'

There's four optional argument for this function:

- *readonly and editted:* If set one of them to 't' then means 'toggle read only' or 'toggle
  editted' independently

- *current-buffer-ndwp:* This set for prompting whether do the main specific function in current
  buffer when it set to nil . It means that not do with current buffer and be with prompt. This
  set will cover cury setting when it's be nil.

- *cury:* Do with current buffer without prompt if set it to t and =current-buffer-ndwp= was t,
  opposite means that not do current buffer."
  (interactive)
  (let* ((current-buffer (buffer-name (current-buffer)))
         (candidate '("global read only" "global editted"))
         (read
          (if (not (or readonly editted))
              (completing-read "Please choose read-only or editted:" candidate nil t)
            (cond
             ((and (eq readonly t)
                   (eq editted nil))
              "global read only")
             ((and (eq readonly nil)
                   (eq editted t))
              "global editted")
             ((and (eq readonly t)
                   (eq editted t))
              (error "Can not double true for readonly and editted!"))))))
    (let ((p-buffer-list (mapcar (function buffer-name) (buffer-list))))
      (dolist (value p-buffer-list)
        (dolist (cache entropy/grom-toggle-except-bfregexp-list)
          (if (string-match-p cache value)
              (delete value p-buffer-list))))
      (if (not current-buffer-ndwp)
          (let ((lock-cur (yes-or-no-p "Whether do with current buffer?")))
            (dolist (buffer p-buffer-list)
              (with-current-buffer buffer
                (cond
                 ((string= read "global read only")
                  (if (and
                       (not buffer-read-only)
                       (not (string-match-p
                             (regexp-quote "dired-mode")
                             (format "%s" major-mode))))
                      (if (not (string= current-buffer buffer))
                          (read-only-mode 1)
                        (if (eq lock-cur t)
                            (read-only-mode 1)))))
                 ((string= read "global editted")
                  (if (and buffer-read-only
                           (not (string-match-p
                                 (regexp-quote "dired-mode")
                                 (format "%s" major-mode))))
                      (if (not (string= current-buffer buffer))
                          (read-only-mode 0)
                        (if (eq lock-cur t)
                            (read-only-mode 0)))))))))
        (dolist (buffer p-buffer-list)
          (with-current-buffer buffer
            (cond
             ((string= read "global read only")
              (if (and
                   (not buffer-read-only)
                   (not (string-match-p
                         (regexp-quote "dired-mode")
                         (format "%s" major-mode))))
                  (if (not (string= current-buffer buffer))
                      (read-only-mode 1)
                    (if (eq cury t)
                        (read-only-mode 1)))))
             ((string= read "global editted")
              (if (and buffer-read-only
                       (not (string-match-p
                             (regexp-quote "dired-mode")
                             (format "%s" major-mode))))
                  (if (not (string= current-buffer buffer))
                      (read-only-mode 0)
                    (if (eq cury t)
                        (read-only-mode 0))))))))))))

(defun entropy/grom-quick-readonly-global ()
  "Do readonly for all buffers include current-buffer.

This function are basically rely on `entropy/grom-toggle-read-only'."
  (interactive)
  (if (and (not (string-match-p "CAPTURE-.*\\.org$" (buffer-name (current-buffer))))
           (not (string-match-p "\\*Minibuf.*\\*" (buffer-name (current-buffer)))))
      (progn
        (entropy/grom-toggle-read-only t nil t nil)
        (let ((judge nil)
              (bn (buffer-name)))
          (dolist (el entropy/grom-toggle-except-bfregexp-list)
            (if (string-match-p bn el)
                (setq judge t)))
          (if (not judge)
              (read-only-mode 1)
            (error "Current buffer %s can no be locked" bn))))
    (user-error "Can not use quick-readonly-global in %s!" (buffer-name)))
  (message "All buffers have been read-only!"))
(global-set-key (kbd "M-1") 'entropy/grom-quick-readonly-global)


;; ** Global-readonly-mode
;; *** enable function
;; **** Global-readonly-type-filter
(defun entropy/grom-init ()
  (cond
   ((string= entropy/grom-readonly-type "modes")
    (dolist (mode-hook entropy/grom-mode-list)
      (add-hook mode-hook #'(lambda ()
                              (when (not (string= (buffer-name) "*scratch*"))
                                (read-only-mode 1))))))
   ((string= entropy/grom-readonly-type "all")
    (defun entropy/grom-find-file-hook ()
      "Hooks for find-file with global readonly mode type \"all\"
      using except buffer-name list
      `entropy/grom-find-file-except-bfregexp-list'."
      (let ((p nil))
        (dolist (buffer entropy/grom-find-file-except-bfregexp-list)
          (if (string-match-p buffer (buffer-name))
              (setq p t)))
        (if (not p)
            (read-only-mode 1))))
    (add-hook 'find-file-hook 'entropy/grom-find-file-hook))

   ((string= entropy/grom-readonly-type "convert")
    (with-eval-after-load dired
      (defun dired-find-file ()
        "*Note:* This function has been redefined by
      global-read-only-mode

In Dired, visit the file or directory named on this line."
        (interactive)
        ;; Bind `find-file-run-dired' so that the command works on directories
        ;; too, independent of the user's setting.
        (let ((find-file-run-dired t))
          (find-file-read-only (dired-get-file-for-visit))))))))


;; *** disable function
(defun entropy/grom-setoff ()
  "Setoff global-readonly-mode."
  (cond
   ((string= entropy/grom-readonly-type "modes")
    (dolist (mode-hook entropy/grom-mode-list)
      (remove-hook mode-hook #'(lambda ()
                                 (when (not (string= (buffer-name) "*scratch*"))
                                   (read-only-mode 1))))))
   ((string= entropy/grom-readonly-type "all")
    (remove-hook 'find-file-hook #'entropy/grom-find-file-hook))
   ((string= entropy/grom-readonly-type "convert")
    (defun dired-find-file ()
      "In Dired, visit the file or directory named on this line."
      (interactive)
      (let ((find-file-run-dired t)
            (switch-to-buffer-preserve-window-point
             (if dired-auto-revert-buffer
                 nil
               switch-to-buffer-preserve-window-point)))
        (find-file (dired-get-file-for-visit))))))
  (defun entropy/grom-toggle-read-only ()
    (message "Function has been removed."))
  (defun entropy/grom-quick-readonly-global ()
    (message "Function has been removed.")))

;; ** Global read only excepted feature
;; *** Unlock library
;; **** library for org
(defun entropy/grom-error-toggle-readonly ()
  (interactive)
  (user-error "Can not use quick-global-only or toggle-readonly in %s" major-mode))

(defun entropy/grom-get-true-agenda-file-name-list ()
  "Getting FILENAME of `org-agenda-files' and push them in
`entropy/grom-agenda-true-name-list'

This function was used for `entropy/grom-get-unlock-agenda-buffers-list'.
"
  (interactive)
  (setq entropy/grom-agenda-true-name-list nil)
  (let ((agenda-files org-agenda-files))
    (dolist (oname agenda-files)
      (let((nname (file-truename oname)))
        (add-to-list 'entropy/grom-agenda-true-name-list nname)))))

(defun entropy/grom-get-true-buffer-name-pair-list ()
  "Getting current frame's actived buffer true name with
`buffer-file-name' and push them into `entropy/grom-buffer-true-name-pair-list'

This function was used for `entropy/grom-get-unlock-agenda-buffers-list'.
"
  (interactive)
  (setq entropy/grom-buffer-true-name-pair-list nil)
  (dolist (buffer (mapcar (function buffer-name) (buffer-list)))
    (let ((tbname (if (with-current-buffer buffer buffer-file-truename)
                      (file-truename
                       (with-current-buffer buffer buffer-file-truename))
                    nil)))
      (add-to-list 'entropy/grom-buffer-true-name-pair-list `(,tbname ,buffer)))))

(defun entropy/grom-get-unlock-agenda-buffers-list ()
  "Getting activated `org-agenda-fils' true name of buffer-list
in current frame."
  (interactive)
  (setq entropy/grom-org-unlock-file-list nil)
  (entropy/grom-get-true-agenda-file-name-list)
  (entropy/grom-get-true-buffer-name-pair-list)
  (dolist (aname entropy/grom-agenda-true-name-list)
    (dolist (bname entropy/grom-buffer-true-name-pair-list)
      (if (stringp (car bname))
          (if (string-match-p (regexp-quote aname) (car bname))
              (add-to-list 'entropy/grom-org-unlock-file-list (nth 1 bname)))))))

(defun entropy/grom-unlock-agenda-files ()
  "Unlock all agenda files when `entropy/grom-enable' was enabled"
  (interactive)
  (entropy/grom-get-unlock-agenda-buffers-list)
  (dolist (buffer entropy/grom-org-unlock-file-list)
    (with-current-buffer buffer
      (if buffer-read-only
          (read-only-mode 0)))))


(defun entropy/grom-agenda-unlock-current-entry (&rest arg-reset)
  "Unlock current entry in agenda view panel when global
readonly mode."
  (interactive)
  (if (and (or (string= entropy/grom-readonly-type "convert")
               (string= entropy/grom-readonly-type "all"))
           (equal major-mode 'org-agenda-mode))
      (let ((etb (or (if (org-get-at-bol 'org-hd-marker)
                         (buffer-name
                          (marker-buffer (org-get-at-bol 'org-hd-marker))))
                     entropy/grom-current-agenda-buffer)))
        (if (not etb)
            (progn
              (org-agenda-redo-all)
              (setq etb (buffer-name (marker-buffer (org-get-at-bol 'org-hd-marker))))))
        (save-excursion
          (with-current-buffer etb
            (if buffer-read-only
                (progn
                  (read-only-mode 0)
                  (message "Unlock buffer '%s' !" etb))
              (message "No need to unlock buffer '%s' -v-" etb))))
        (setq entropy/grom-current-agenda-buffer etb))))

(defun entropy/grom-agenda-lock-current-entry (&rest arg-rest)
  "Relock current agenda entry by manipulation according to
`entropy/grom-current-agenda-buffer'."
  (interactive)
  (when (and (equal major-mode 'org-agenda-mode)
             entropy/grom-current-agenda-buffer)
    (let ((etb entropy/grom-current-agenda-buffer))
      (with-current-buffer etb
        (if (not buffer-read-only)
            (read-only-mode 1))))
    (setq entropy/grom-current-agenda-buffer nil)))

;; *** Adjust org mode
(defun entropy/grom-org-init ()
;; ***** agenda function advice for unlock current entry
  (when (or (string= entropy/grom-readonly-type "convert")
            (string= entropy/grom-readonly-type "all"))
    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "C-d") #'entropy/grom-unlock-agenda-files)

      ;; org-agenda-todo advice
      (advice-add 'org-agenda-todo :before #'entropy/grom-agenda-unlock-current-entry)
      (advice-add 'org-agenda-todo :after #'entropy/grom-agenda-lock-current-entry)


      ;; ============note behaviour ==============
      ;; -----------------------------------------
      ;; org-agenda-add-not advice 
      (advice-add 'org-agenda-add-note :before #'entropy/grom-agenda-unlock-current-entry)
      
      ;; org-add-log-note
      (advice-add 'org-add-log-note :before #'entropy/grom-agenda-unlock-current-entry)

      ;; org-sore-log-note
      (advice-add 'org-store-log-note :after #'entropy/grom-agenda-lock-current-entry)))


  
;; ***** add error-readonly hook for agenda
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "<f1>") 'entropy/grom-error-toggle-readonly)
    (define-key org-agenda-mode-map (kbd "M-1") 'entropy/grom-error-toggle-readonly))
  
;; ***** Redefine org-capture about function for adapting for global-readonly-mode
  (when (or (string= entropy/grom-readonly-type "convert")
            (string= entropy/grom-readonly-type "all"))
    (with-eval-after-load 'org-capture
      (defun org-capture-place-template (&optional inhibit-wconf-store)
        "*Note:* This function has been modified by globla-read-only-mode

Insert the template at the target location, and display the buffer.
When `inhibit-wconf-store', don't store the window configuration, as it
may have been stored before."
        (unless inhibit-wconf-store
          (org-capture-put :return-to-wconf (current-window-configuration)))
        (delete-other-windows)
        (org-switch-to-buffer-other-window
         (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
        (widen)
        (outline-show-all)
        (goto-char (org-capture-get :pos))
        (setq-local outline-level 'org-outline-level)

        ;; Force un-readonly buffer
        (if buffer-read-only
            (read-only-mode 0))
        
        (pcase (org-capture-get :type)
          ((or `nil `entry) (org-capture-place-entry))
          (`table-line (org-capture-place-table-line))
          (`plain (org-capture-place-plain-text))
          (`item (org-capture-place-item))
          (`checkitem (org-capture-place-item)))
        (org-capture-mode 1)
        (setq-local org-capture-current-plist org-capture-plist))

      (defun entropy/org-capture-put-unlock (&rest args)
        "Adding buffer unlock for narrowed org capture buffer.

This func was advice for func
`org-capture-put-target-region-and-position'."
        (when buffer-read-only
          (read-only-mode 0)))

      (advice-add 'org-capture-put-target-region-and-position
                  :after #'entropy/org-capture-put-unlock))

    (with-eval-after-load 'org-datetree
      (defun org-datetree--find-create (regex year &optional month day insert)
        " *Note:* this function has been modified by
        global-read-only mode

Find the datetree matched by REGEX for YEAR, MONTH, or DAY.
REGEX is passed to `format' with YEAR, MONTH, and DAY as
arguments.  Match group 1 is compared against the specified date
component.  If INSERT is non-nil and there is no match then it is
inserted into the buffer."

        ;; Fore un-readonly buffer
        (if buffer-read-only
            (read-only-mode 0))

        (when (or month day)
          (org-narrow-to-subtree))
        (let ((re (format regex year month day))
              match)
          (goto-char (point-min))
          (while (and (setq match (re-search-forward re nil t))
                      (goto-char (match-beginning 1))
                      (< (string-to-number (match-string 1)) (or day month year))))
          (cond
           ((not match)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (org-datetree-insert-line year month day insert))
           ((= (string-to-number (match-string 1)) (or day month year))
            (beginning-of-line))
           (t
            (beginning-of-line)
            (org-datetree-insert-line year month day insert))))))))

(defun entropy/grom-org-setoff ()
  "Setoff all org config of global-readonly-mode."
  (with-eval-after-load 'org-agenda
    (advice-remove 'org-agenda-todo #'entropy/grom-agenda-unlock-current-entry)
    (advice-remove 'org-agenda-todo #'entropy/grom-agenda-lock-current-entry)
    (advice-remove 'org-agenda-add-note #'entropy/grom-agenda-unlock-current-entry))
  (with-eval-after-load 'org
    (advice-remove 'org-add-log-note #'entropy/grom-agenda-unlock-current-entry)
    (advice-remove 'org-store-log-note #'entropy/grom-agenda-lock-current-entry)))

;; ** mode defination
(defun entropy/grom-mode-enable ()
  "Enable entropy-grom-mode."
  (progn
    (entropy/grom-init)
    (entropy/grom-org-init)
    (message "Global read only mode enable!")))

(defun entropy/grom-mode-disable ()
  "Disable entropy-grom-mode."
  (progn
    (entropy/grom-setoff)
    (entropy/grom-org-setoff)
    (message "Global read only mode disable!")))

;;;###autoload
(define-minor-mode entropy-grom-mode
  "Global minor mode for buffer-readonly for all."
  :init-value nil
  :lighter "GROM"
  :global t
  (if entropy-grom-mode
      (entropy/grom-mode-enable)
    (entropy/grom-mode-disable)))

;; * provide
(provide 'entropy-global-read-only-mode)
