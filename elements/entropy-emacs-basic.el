;;; File name: init-basic.el ---> for entropy-emacs
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

;; ** Temporal bug revert
;; *** gnutls bug for emacs version upper than '26.1'
;; 
;; Bug refer emacs `url.el' bug or possible for the gnutls bug override.
;;
;; Refer:
;; @see https://github.com/magit/ghub/issues/81#issuecomment-488660597 
(unless (version< emacs-version "26.2")
  (advice-add #'gnutls-available-p :override #'ignore))

;; ** Personal infomation
(when (and entropy/emacs-user-full-name
           entropy/emacs-user-mail-address)
  (setq user-full-name entropy/emacs-user-full-name)
  (setq user-mail-address entropy/emacs-user-mail-address))


;; *** Show the column numberic in modeline
(setq column-number-mode t)
;; *** Set default cursor style
(setq-default cursor-type t)

(defun entropy/toggle-cursor-type ()
  (interactive)
  (if (string= (symbol-name cursor-type) "t")
      (setq cursor-type 'bar)
    (setq cursor-type t)))

;; *** global display line number mode
(if (>= emacs-major-version 26)
    (progn
      (setq-default display-line-numbers-width-start t)
      ;; (setq-default display-line-numbers-grow-only t)
      (when entropy/emacs-init-display-line-mode
        (global-display-line-numbers-mode))))

;; ** Set the default dired directory
(setq default-directory "~/")
;; ** Backup setting
(setq-default auto-save-default t) ;;auto-save buffer with '#...#' syntax of file name on disk
(setq make-backup-files nil)
;; ** scratch buffer corresponding file
;; 
;;     Amounts of company backend function can not functional
;;     auto-completion in none file buffer, so corresponding one file
;;     to *scratch* buffer.

(defun entropy/scratch-buffer-file-binding ()
  "Corresponded *scratch* buffer to one temp-file.

  Filename are \".scratch_entropy\" in `user-emacs-directory'.
  "
  (let ((bfn "*scratch*"))
    (require 'entropy-common-library)
    (if (entropy/cl-buffer-exists-p "*scratch*")
        (kill-buffer "*scratch*"))
    
    (let ((fname "~/.scratch_entropy"))
      (if (not (file-exists-p fname))
          (progn
            (write-region "" "" fname)
            (with-current-buffer (find-file-noselect fname)
              (if buffer-read-only (read-only-mode 0))
              (auto-save-mode 0)
              (erase-buffer)
              (rename-buffer "*scratch*")
              (emacs-lisp-mode)
              (insert initial-scratch-message)))
        (with-current-buffer (find-file-noselect fname)
          (if buffer-read-only (read-only-mode 0))
          (auto-save-mode 0)
          (rename-buffer "*scratch*")
          (emacs-lisp-mode)
          (goto-char (point-min))
          (if (not (re-search-forward (regexp-quote initial-scratch-message) nil t))
              (insert initial-scratch-message)))))
    bfn))

(with-eval-after-load 'entropy-emacs-structure
  (entropy/scratch-buffer-file-binding))

;; ** Highlight current line
(defun entropy/dhl-judge-state ()
  (let ((hlmp (ignore-errors hl-line-mode))
        (dlmp display-line-numbers)
        rtn)
    (setq rtn (list hlmp dlmp))
    (cond
     ((equal rtn '(nil nil))
      (setq rtn 1))
     ((equal rtn '(nil t))
      (setq rtn 2))
     ((equal rtn '(t nil))
      (setq rtn 3))
     ((equal rtn '(t t))
      (setq rtn 4)))
    rtn))

(defun entropy/dhl-toggle ($Prefix)
  (interactive "P")
  (if (not (version< emacs-version "26.1"))
      (if (not $Prefix)
          (cl-case (entropy/dhl-judge-state)
            (1
             (hl-line-mode 1))
            (2
             (hl-line-mode 1))
            (3
             (hl-line-mode 0))
            (4
             (hl-line-mode 0)))
        (cl-case (entropy/dhl-judge-state)
          (1
           (hl-line-mode 1)
           (display-line-numbers-mode 1))
          (2
           (hl-line-mode 1))
          (3
           (display-line-numbers-mode 1))
          (4
           (hl-line-mode 0)
           (display-line-numbers-mode 0))))
    (if (not (ignore-errors hl-line-mode))
        (hl-line-mode 1)
      (hl-line-mode 0))))
      
(global-set-key (kbd "<f2>") 'entropy/dhl-toggle)

;; ** smooth scrolling
(defvar entropy/smooth-scrolling-mode nil
  "Indicated whether smooth-scrolling enable.

Note:

Manually edit this variable will not be any effection.")

(defun entropy/smooth-scrolling ()
  "Toggle smooth-scrolling buffer scrolling view."
  (interactive)
  (setq redisplay-dont-pause t
        scroll-margin (if (equal scroll-margin 0) 1 0)
        scroll-step   (if (equal scroll-step 0) 1 0)
        scroll-conservatively (if (equal scroll-conservatively 0) 101 0)
        scroll-preserve-screen-position (if scroll-preserve-screen-position 1 nil))
  (if (and (equal scroll-margin 1)
           (equal scroll-step 1)
           (equal scroll-conservatively 101)
           (equal scroll-preserve-screen-position nil))
      (progn
        (message "Smooth scrolling enabled!")
        (setq entropy/smooth-scrolling-mode t))
    (progn
      (message "Smooth scrolling disabled!")
      (setq entropy/smooth-scrolling-mode nil))))

(entropy/smooth-scrolling)

;; ** Window-setting
;; *** Window switch
(use-package window-number
  :commands (window-number-switch)
  :bind
  ("C-x o" . window-number-switch)
  :config
  (window-number-mode 1))

;; **** use windmove function stolen :) from `https://github.com/troydm/emacs-stuff/blob/master/windcycle.el'
(defun entropy/emacs-windmove-up-cycle()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)
	          (error (condition-case nil (windmove-right) (error (condition-case nil (windmove-left) (error (windmove-up))))))))))

(defun entropy/emacs-windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)
	          (error (condition-case nil (windmove-left) (error (condition-case nil (windmove-right) (error (windmove-down))))))))))

(defun entropy/emacs-windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)
	          (error (condition-case nil (windmove-up) (error (condition-case nil (windmove-down) (error (windmove-right))))))))))

(defun entropy/emacs-windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)
	          (error (condition-case nil (windmove-down) (error (condition-case nil (windmove-up) (error (windmove-left))))))))))

(global-set-key (kbd "C-x <up>") 'entropy/emacs-windmove-up-cycle)
(global-set-key (kbd "C-x <down>") 'entropy/emacs-windmove-down-cycle)
(global-set-key (kbd "C-x <right>") 'entropy/emacs-windmove-right-cycle)
(global-set-key (kbd "C-x <left>") 'entropy/emacs-windmove-left-cycle)

;; ***** disable buffer reverse and turn by =C-x C-left= =C-x C-right=
(global-set-key (kbd "C-x C-<left>") nil)
(global-set-key (kbd "C-x C-<right>") nil)

;; (global-set-key (kbd "M-<up>") 'entropy/emacs-windmove-up-cycle)
;; (global-set-key (kbd "M-<down>") 'entropy/emacs-windmove-down-cycle)
;; (global-set-key (kbd "M-<right>") 'entropy/emacs-windmove-right-cycle)
;; (global-set-key (kbd "M-<left>") 'entropy/emacs-windmove-left-cycle)

;; Window Dedicated Toggle Function
(defun toggle-dedicated-window ()
  (interactive)
  (let ((d (window-dedicated-p (selected-window))))
    (progn (set-window-dedicated-p (selected-window) (not d))
	   (if d (message "Window is not dedicated") (message "Window is now dedicated")))))

;; *** window config
;; **** eyebrowse ----> for save the window config(workspace group)
(use-package eyebrowse
  :commands (eyebrowse-mode)
  :init (add-hook 'entropy/init-mini-hook #'eyebrowse-mode)
  :bind (("C-c C-w C-e" . entropy/eyebrowse-create-workspaces)
         ("C-c C-w M-e" . entropy/eyebrowse-delete-workspace)
         ("C-c C-w C-`" . entropy/eyebrowse-switch-top)
         ("C-c v" . entropy/eyebrowse-create-derived)
         ("C-c M-v" . entropy/eyebrowse-switch-derived)
         :map eyebrowse-mode-map
         ("C-c C-w C-c" . entropy/eyebrowse-create-window-config)
         ("C-c C-w c" . entropy/eyebrowse-create-window-config)
         ("C-c C-w ." . entropy/eye-switch-basic-window)
         ("C-c C-w a" . eyebrowse-switch-to-window-config))
  :config
  (setq eyebrowse-mode-line-style nil)
  (if entropy/emacs-enable-eyebrowse-new-workspace-init-function
      (setq eyebrowse-new-workspace entropy/emacs-eyebrowse-new-workspace-init-function)
    (setq eyebrowse-new-workspace t))

  ;; debug for improving eyebrowse's user experience
  (with-eval-after-load 'eyebrowse
    (defun eyebrowse--read-slot ()
      "Read in a window config SLOT to switch to.
  A formatted list of window configs is presented as candidates.
  If no match was found, the user input is interpreted as a new
  slot to switch to.

  Note: 

  This function has been modified for be compat with entropy-emacs
  for reasons as below:

  #+BEGIN_QUOTE
  Origin eyebrowse slots switching prompt showing all slots include
  current work-space slot, this was messy as this will producing the
  probility for switching to current work-space while operator occur
  the mistake, thus this wasting the time.

  The minor improve for this was remove the current work-space slot
  in switching prompt's candidates.
  #+END_QUOTE

  This minor improve refer to the github issue
  https://github.com/wasamasa/eyebrowse/issues/77"
      (let* ((current-slot (eyebrowse--get 'current-slot))
             (candidates (--keep (and (/= (car it) current-slot)
                                      (cons (eyebrowse-format-slot it)
                                            (car it)))
				 (eyebrowse--get 'window-configs)))
             (candidate (completing-read "Enter slot: " candidates))
             (choice (cdr (assoc candidate candidates))))
	(or choice (eyebrowse--string-to-number candidate)
            (user-error "Invalid slot number")))))


  (defun entropy/eyebrowse-create-window-config ()
    "Creates a window config at a yet unoccupied slot and named
    this work space."
    (interactive)
    (funcall #'eyebrowse-create-window-config)
    (let ((slot (eyebrowse--get 'current-slot))
          (tag (read-string "Tag: ")))
      (apply #'eyebrowse-rename-window-config `(,slot ,tag))))
  
  (defun entropy/current-slot ()
    "Show current eyebrowse workspace slot and tag info."
    (interactive)
    (let* ((entropy/eyebrowse-slot-result (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (window-config (assoc (eyebrowse--get 'current-slot) window-configs))
           (current-tag (nth 2 window-config)))
      (message "Slot:%s  Tag:%s" entropy/eyebrowse-slot-result current-tag)))
  (global-set-key (kbd "C-c M-s") 'entropy/current-slot)

  (defun entropy/eyebrowse-kill-all-group ()
    "Kill all eyebrowse window config"
    (interactive)
    (dolist (item (eyebrowse--get 'window-configs))
      (eyebrowse--delete-window-config (car item)))
    (eyebrowse-init))

  (defun entropy/eyebrowse-create-workspaces (&optional ws-list $confirm)
    "Batch create eyebrowse workspace with name input prompt
powered by `entropy/cl-repeated-read'.

You can insert optional arg WS-LIST for do it within lisp
programming code. WS-LIST was list with string elements like:

'(\"basic\" \"main\" \"temp\" \"eww\")'

The second optional arg $confirm will trigger the process
confirmation when sets it to 't'."
    (interactive)
    (require 'entropy-common-library)
    (when $confirm
      (unless (yes-or-no-p "Do you want to clean all workspace and buiding new workspaces? ")
        (error "Canceld rebuild workspaces.")))
    (entropy/eyebrowse-kill-all-group)
    (let ((current-slot (eyebrowse--get 'current-slot ))
          (ws (if ws-list
                  ws-list
                (entropy/cl-repeated-read "work-space name"))))
      (dolist (el ws)
        (if (equal 1 current-slot)
            (progn
              (eyebrowse-rename-window-config 1 el)
              (setq current-slot (+ 1 current-slot)))
          (progn
            (eyebrowse-switch-to-window-config current-slot)
            (eyebrowse-rename-window-config current-slot el)
            (setq current-slot (+ 1 current-slot)))))
      (eyebrowse-switch-to-window-config-1)))


  (defvar entropy/eyebrowse-config-selected '()
    "Contained selected eyebrowse workspace config.")

  (defun entropy/eyebrowse-read-prompt ()
    "Produce the prompt string for repeated selected eyebrowse
window configs."
    (format "WS (%s) : "
            (let ((olist entropy/eyebrowse-config-selected)
                  mlist
                  rtn)
              (dolist (el olist)
                (let (prefix)
                  (setq prefix (car (split-string el ":")))
                  (push prefix mlist)))
              (setq rtn (let ((prompt ""))
                          (dolist (el mlist)
                            (setq prompt (concat prompt (if (not (string= el ""))
                                                            (concat el "☑; ")
                                                          ""))))
                          prompt))
              rtn)))
  
  (eval-and-compile
    (defun entropy/eyebrowse-read-config-repeated (x)
      "Used in repeated selected eyebrowse config with `ivy-call'.

This was the one action in `ivy-read'."
      (require 'ivy)
      (if (not (member x entropy/eyebrowse-config-selected))
          (push x entropy/eyebrowse-config-selected))
      (let ((prompt (entropy/eyebrowse-read-prompt)))
        (setf (ivy-state-prompt ivy-last) prompt)
        (setq ivy--prompt (concat "(%d/%d) " prompt)))
      (cond
       ((memq this-command '(ivy-done
                             ivy-alt-done
                             ivy-immediate-done))
        t)
       ((eq this-command 'ivy-call)
        (with-selected-window (active-minibuffer-window)
          (delete-minibuffer-contents))))))
  
  (defun entropy/eyebrowse-delete-workspace ()
    "Delete eyebrowse workspace with prompt."
    (interactive)
    (require 'entropy-common-library)
    (require 'ivy)
    (setq entropy/eyebrowse-config-selected nil)
    (let* ((wcon (eyebrowse--get 'window-configs))
           sanm
           candi
           candin)
      (dolist (el wcon)
        (when (not (= (car el) (eyebrowse--get 'current-slot)))
          (push `(,(concat (number-to-string (car el)) ":" (nth 2 el)) . ,(car el)) candi)))
      (setq candi (entropy/cl-reverse-list candi))
      (setq candin (mapcar 'car candi))
      (ivy-read "Delete worksapce (%d/%d): " candin
                :require-match t
                :action 'entropy/eyebrowse-read-config-repeated)
      (dolist (el entropy/eyebrowse-config-selected)
        (eyebrowse--delete-window-config (cdr (assoc el candi))))))


  (defun eyebrowse-free-slot (slots)
    "Returns a yet unoccupied slot.
The specific behaviour is tmux-like.

Note: this function has been redefine for
`entropy/eyebrowse-create-derived'."
    (let ((min (car slots)))
      (if (> min 1)
          1
        (let (last cur done)
          (while (and slots (not done))
            (setq last (car slots)
                  cur (cadr slots))
            (when (and last cur
                       (> (- cur last) 1))
              (setq done t))
            (setq slots (cdr slots)))
          (floor (1+ last))))))

  
  (defun entropy/eyebrowse-create-derived ()
    "Create derived workspace basic from the current main workspace.

The main workspace was whom have the slot without float point,
1,2,3,4.....


The derived workspace was whom have the float slot point, '1.1'
'1.2' '5.6' .....


For now the which main workspace just can have nine derived
workspaces. Thus the core cause of this was the slot recording
function just can manipulate one decimal place for one main
workspace. 

The reason for this limit was that two points follow:

- For complicated arithmetic coding
- For trying to reducing the recongnization for operation.

"
    (interactive)
    (let* ((dot-list '(
                       0.1 0.2 0.3
                       0.4 0.5 0.6
                       0.7 0.8 0.9))
           derived-list
           derived-dot
           derived-dot-ac
           new-slot
           (window-configs (eyebrowse--get 'window-configs)))
      
      (let* ((slots (mapcar 'car window-configs))
             (current-slot (eyebrowse--get 'current-slot))
             (floor-tag (nth 2 (assoc (floor current-slot) window-configs)))
             (floor-slot (floor current-slot))
             (up-slot (+ 1 floor-slot)))

        ;; make derived-list
        (dolist (el slots)
          (if (and (< el up-slot)
                   (>= el floor-slot))
              (push el derived-list)))

        ;; make derived-dot list
        (dolist (el derived-list)
          (push (- el floor-slot) derived-dot))
        (setq derived-dot (delete 0 derived-dot))

        ;; producing accurated derived-dot list.
        (dolist (el1 derived-dot)
          (dolist (el2 dot-list)
            (if (< (abs (- el1 el2)) 0.00000000000000995)
                (push el2 derived-dot-ac))))

        ;; judgement of whether derived number fulled
        (if (> (length derived-dot-ac) 8)
            (error "Derived number fill out!"))

        (let ((temp-dot dot-list))
          ;; produce remaining accessed usable derived number
          (dolist (el1 dot-list)
            (dolist (el2 derived-dot-ac)
              (if (= el1 el2)
                  (setq temp-dot (delete el1 temp-dot)))))

          ;; choose the least usable drived number
          (let ((lengths (- (length temp-dot) 1))
                (count 0)
                (forward 0))
            (while (< forward lengths)
              (if (= forward 0) (setq forward (+ count 1)))
              (if (< (nth count temp-dot)
                     (nth forward temp-dot))
                  (setq forward (+ forward 1))
                (progn (setq count forward)
                       (setq forward 0))))
            (setq new-slot (+ floor-slot (nth count temp-dot)))))

        (let* ((custr (read-string "Entering the tag: ")))
          ;; switch to derived work-space and named it
          (eyebrowse-switch-to-window-config new-slot)
          (eyebrowse-rename-window-config
           new-slot
           (concat "⛓"  (number-to-string floor-slot) ":"
                   (if (and floor-tag (not (string= floor-tag "")))
                                  (concat "\"" floor-tag "\"") "[]")
                   (if (and custr (not (string= "" custr)))
                       (concat "☛" custr)
                     "")))))))

  (defun entropy/eyebrowse-switch-derived ()
    "Switch to derived workspace rely on current basic workspace."
    (interactive)
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (slots (mapcar 'car window-configs))
           (floor-slot (floor (eyebrowse--get 'current-slot)))
           (top-slot (+ 1 floor-slot))
           derived-list
           derived-named-list
           choice)
      (dolist (el slots)
        (if (and (< el top-slot)
                 (> el floor-slot))
            (if (and (not (= el (eyebrowse--get 'current-slot)))
                     (not (= el floor-slot)))
                (push el derived-list))))
      (if derived-list
          (dolist (el derived-list)
            (push
             (cons
              (concat
               (number-to-string el) ":"
               (nth 2 (assoc el window-configs)))
              el)
             derived-named-list))
        (cond
         ((string-match-p "\\.[[:digit:]]" (number-to-string (eyebrowse--get 'current-slot)))
          (error "You are in the only one derived workspace."))
         (t (error "No derived work-space."))))
      (setq choice (completing-read "Choose derived: " derived-named-list nil t))
      (eyebrowse-switch-to-window-config (cdr (assoc choice derived-named-list)))))


  (defun entropy/eye-switch-basic-window ()
    "Switch to basic workspace which has the prompt candidates
without derived slot."
    (interactive)
    (let* ((wcfgs (eyebrowse--get 'window-configs))
           (slots (mapcar 'car wcfgs))
           cons-slots
           s-and-name
           choice)

      (dolist (el slots)                ;make cons list for candi with slot
        (if (and (not (> el (floor el)))
                 (not (= el (eyebrowse--get 'current-slot))))
            (push `(,(concat (number-to-string el) ":" (nth 2 (assoc el wcfgs)))
                    . ,el)
                  cons-slots)))
      (dolist (el cons-slots)           ;make candi-name list
        (push (car el) s-and-name))

      (setq choice (ivy-read "Switch to WS: " s-and-name
                             :require-match t))
      (eyebrowse-switch-to-window-config (cdr (assoc choice cons-slots)))))


  (defface entropy/eyebrowse-back-top-wg-message-face_body '((t ()))
    "Face for message body area with func `entropy/eyebrowse-switch-top'")

  (set-face-attribute 'entropy/eyebrowse-back-top-wg-message-face_body
                      nil :foreground "yellow")

  (defface entropy/eyebrowse-back-top-wg-message-face_content '((t ()))
    "Face for message content area with func `entropy/eyebrowse-switch-top'")

  (set-face-attribute 'entropy/eyebrowse-back-top-wg-message-face_content
                      nil :foreground "green2")

  
  (defun entropy/eyebrowse-switch-top ()
    "Back to the top workspace from current derived workspace."
    (interactive)
    (let* ((cslot (eyebrowse--get 'current-slot))
           (top-slot (floor cslot))
           (top-tag (nth 2 (assoc top-slot (eyebrowse--get 'window-configs)))))
      (cond
       ((not (equal cslot top-slot))
        (eyebrowse-switch-to-window-config top-slot)
        (message (concat (propertize "You've been back to top wg: "
                                     'face 'entropy/eyebrowse-back-top-wg-message-face_body)
                         (propertize (if (and (not (equal top-tag ""))
                                              (not (equal top-tag nil)))
                                         (format "[%s]: %s " top-slot top-tag)
                                       (format "[%s] " top-slot))
                                     'face 'entropy/eyebrowse-back-top-wg-message-face_content)
                         (propertize "." 'face 'entropy/eyebrowse-back-top-wg-message-face_body))))
       (t (error "You've at top wg!"))))))

;; **** winner mode for recover previous window config faster
(use-package winner
  :ensure nil
  :commands (winner-mode)
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
  (add-hook 'entropy/init-mini-hook #'winner-mode))
;; **** desktop mode
(when entropy/emacs-desktop-enable
  (use-package desktop
    :ensure nil
    :commands (desktop-save-mode)
    :init (desktop-save-mode 1)
    :config
    ;; Restore frames into their original displays (if possible)
    (setq desktop-restore-in-current-display nil)

    ;; Load custom theme
    (add-hook 'desktop-after-read-hook
	      (lambda ()
		(dolist (theme custom-enabled-themes)
		  (load-theme theme t))))

    ;; Don't save/restore frames in tty
    (unless (display-graphic-p)
      (setq desktop-restore-frames nil))))

;; *** Kill-buffer-and-window function
(defvar entropy/kill-buffer-excluded-buffer-list nil
  "Buffer name regexp list stored excluded buffer name match for
  func `entropy/kill-buffer-and-window'.")

(defvar entropy/kill-buffer-special-buffer-list
  '(("\\*eshell-?" . (lambda ()
                       (kill-this-buffer)))
    ("\\*anaconda-" . (lambda ()
                        (error
                         "You couldn't kill this buffer, as it will cause some problem.")))
    (("\\.cp?p?" "\\.h" "\\.el" "\\.py" "\\.php" "\\.js" "\\.html" "\\.css" "\\.sh" "\\.org"
      "\\.txt" "\\.md")
     .
     (lambda ()
       (let ((buffn (buffer-name))
             (base-dir default-directory))
         (kill-buffer buffn)
         (dired base-dir)))))
  "Special buffer alist which the car can be regexp string or
  list of regexp string, the cdr was buffer killing specific
  func. ")

(defun entropy/kill-buffer-and-window ()
  "Kill buffer and window following rule by
`entropy/kill-buffer-excluded-buffer-list' and
`entropy/kill-buffer-special-buffer-list'.

Using func `entropy/buffer-close' be the default func."
  (interactive)
  (let ((excluded entropy/kill-buffer-excluded-buffer-list)
        (special entropy/kill-buffer-special-buffer-list)
        excl_p special_p
        (buffn (buffer-name)))
    (when excluded
      (mapc (lambda (regx)
              (when (not excl_p)
                (when (string-match-p regx buffn)
                  (setq excl_p t))))
            excluded))
    (cond
     (excl_p
      (entropy/buffer-close))
     (t
      (mapc (lambda (mod_l)
              (let ((regx (car mod_l))
                    (predicate (cdr mod_l)))
                (when (not special_p)
                  (cond
                   ((listp regx)
                    (catch :exit
                      (dolist (el regx)
                        (when (string-match-p el buffn)
                          (setq special_p
                                (cons el predicate))
                          (throw :exit nil)))))
                   (t
                    (when (string-match-p regx buffn)
                      (setq special_p
                            (cons regx predicate))))))))
            special)
      (if special_p
          (funcall (cdr special_p))
        (entropy/buffer-close))))))

(global-set-key (kbd "C-x k") 'entropy/kill-buffer-and-window)
(global-set-key (kbd "C-x M-k") 'kill-this-buffer)

(defun entropy/buffer-close ()
  "Kill buffer and close it's host window if windows conuts
retrieve from `window-list' larger than 1."
  (let ((buflist (window-list)))
    (if (> (length buflist) 1)
        (kill-buffer-and-window)
      (kill-buffer))))
;; *** Buffer window size setting
(use-package windresize
  :commands (windresize)
  :bind
  ("C-<f10>" . windresize))

;; *** Kill-other-buffers
(defun entropy/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(defun entropy/kill-large-process-buffer ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p (regexp-quote "*eww") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*[萌典]") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*eshell") (format "%s" buffer))
      (kill-buffer buffer))
    (when (string-match-p (regexp-quote "*helm") (format "%s" buffer))
      (kill-buffer buffer))    
    )
  (dolist (process (process-list))
    (when (and (not (string-match-p (regexp-quote "shell") (format "%s" process)))
               (not (string-match-p (regexp-quote "ansi-term") (format "%s" process)))
               (not (string-match-p (regexp-quote "terminal") (format "%s" process))))
               (delete-process process))))
(global-set-key (kbd "C-0") 'entropy/kill-large-process-buffer)


;; *** Exchange window
(use-package buffer-move
  :commands (buf-move-up
             buf-move-down
             buf-move-left
             buf-move-right)
  :bind
  (("C-c <C-up>"   .  buf-move-up)
   ("C-c <C-down>" .  buf-move-down)
   ("C-c <C-left>" .  buf-move-left)
   ("C-c <C-right>" . buf-move-right)))

;; *** Centered-window
;; **** Manully method
(defun entropy/center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (if (car (window-margins))
      (entropy/center-text-clear)
    (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        (/ (window-width) 6)
                        (/ (window-width) 6))))

(defun entropy/center-text-clear ()
  (interactive)
  (set-window-margins
   (car (get-buffer-window-list (current-buffer) nil t))
   nil))
(global-set-key (kbd "C-c M-<up>")     'entropy/center-text)
(global-set-key (kbd "C-c M-<down>")     'entropy/center-text-clear)

;; **** centered-window
;; (use-package centered-window
;;   :bind(("C-c M-<up>" . centered-window-mode)))

;; *** window divider
(window-divider-mode)
;; ** Set defualt tab size
(if entropy/emacs-custom-tab-enable
    (setq-default tab-width entropy/emacs-custom-tab-width)
  (setq-default indent-tabs-mode nil))

;; ** Setting language encoding environment
(setq system-time-locale "C") ;Use english format time string
;; *** Default using UTF-8 encoding for basic environment when `entropy/emacs-custom-language-environment-enable' was nil
(unless (and entropy/emacs-custom-language-environment-enable
             (ignore-errors (stringp entropy/emacs-language-environment)))
  (progn
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8-unix)
    (set-default-coding-systems 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)))

;; *** When `entropy/emacs-custom-language-environment-enable' was t using customized basic encoding system.
(when (and entropy/emacs-custom-language-environment-enable
           (ignore-errors (stringp entropy/emacs-language-environment)))
  (set-language-environment entropy/emacs-language-environment)
  (setq default-file-name-coding-system 'utf-8-unix))

;; setting w32 shell lang env
(when sys/win32p
  (when entropy/emacs-win-env-lang-enable
    (setenv "LANG" entropy/emacs-win-env-lang-set)))

;; **** Specific state to forceing using UTF-8 encoding environment
;; ***** Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ***** Force setting specific file type which must be opened with utf-8-unix encoding system. 
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.md\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.html\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.css\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.c\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.php\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.js\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.sh\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.el\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.bat\\'" 'utf-8-unix)
;; ========================================================================
;; Prompt: If you want all file to be one coding system you should do below
;;(modify-coding-system-alist 'file "" 'utf-8-unix)
;; ========================================================================



;; **** Function for toggle UTF-8 and locale adaption environment
(defvar entropy/lang-locale (car default-process-coding-system)
  "The locale lang.")

(defun entropy/toggle-utf-8-and-locale (&optional cond)
  " This function was to toggle entire UTF-8 environment to or
back from locale.

Optional arg COND has four type:

- \"prompt\": call this function with ivy-read prompt for choosing the target encoding.
- \"UTF-8\": choose UTF-8 without prompt.
- \"LOCAL\": choose LOCAL language environment without prompt.
"
  (interactive)
  (if cond
      (cond
       ((string= cond "prompt")
        (ivy-read "Choice: " '("UTF-8" "LOCAL")
                  :require-match t
                  :action #'entropy/lang-set))
       ((string= cond "UTF-8") (entropy/lang-set "UTF-8"))
       ((string= cond "LOCAL") (entropy/lang-set "LOCAL"))
       (t (error "entropy/toggle-utf-8-and-locale: error argument type.")))
    (if (string= current-language-environment "UTF-8")
        (entropy/lang-set "LOCAL")
      (entropy/lang-set "UTF-8"))))

(defun entropy/lang-set (lang)
  (if (string-match-p "\\*e?shell\\*\\|\\*eshell-.*?\\*" (format "%s" (buffer-list)))
      (error "Can not use this function cause shell buffer exist, please kill it and try again!")
    (cond
     ((string= lang "UTF-8")
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8-unix)
      (message "Setting language environment to 'utf-8-unix'."))
     ((string= lang "LOCAL")
      (set-language-environment entropy/emacs-language-environment)
      (prefer-coding-system entropy/lang-locale)
      (setq default-file-name-coding-system 'utf-8-unix)
      (message "Setting language environment to '%s'." entropy/emacs-language-environment))
     (t (error "Invalid LANG arg")))))

(defun entropy/lang-set-utf-8 (&rest args)
  "Setting language envrionment to unix-8-unix, supported
by `entropy/lang-set'"
  (if (not (string= current-language-environment "UTF-8"))
      (entropy/lang-set "UTF-8")))

(defun entropy/revert-buffer-with-custom-language-environment ()
  "This function was designed to auto revert buffer with
language-environment you set in `entropy/emacs-language-environment'."
  (interactive)
  (if (string= current-language-environment "UTF-8")
      (progn
        (entropy/toggle-utf-8-and-locale)
        (revert-buffer t t)
        (message "Succeed revert buffer with %s" entropy/emacs-language-environment))
    (error "Have been locale setting ♘")))

;; ***** let diff-buffer-with-file force run with unicode language environment
(advice-add 'diff-buffer-with-file :before #'entropy/lang-set-utf-8)

;; ***** around advice when `entropy/emacs-custom-language-environment-enable' was nil

(defun entropy/lang-set-without-enable (oldfunc &rest args)
  "Around advice for funcs:

- entropy/toggle-utf-8-and-locale
- entropy/lang-set-utf-8
- entropy/revert-buffer-with-custom-language-environment

This func will force disable each func's internal process when
custom variable `entropy/emacs-custom-language-environment-enable' was
nil.
 "
  (cond
   ((and entropy/emacs-custom-language-environment-enable
         (ignore-errors (stringp entropy/emacs-language-environment)))
    (apply oldfunc args))
   ((or (null entropy/emacs-custom-language-environment-enable)
        (not (ignore-errors (stringp entropy/emacs-language-environment))))
    t)))

(dolist (el '(entropy/toggle-utf-8-and-locale
              entropy/lang-set-utf-8
              entropy/revert-buffer-with-custom-language-environment))
  (advice-add el :around #'entropy/lang-set-without-enable))

;; ** Auto wrap line
(setq-default truncate-lines t)
(global-set-key(kbd "C-<f9>") 'toggle-truncate-lines)
(setq org-startup-truncated t)
;; ** remove overwrite-mode
(advice-add 'overwrite-mode
            :override
            #'(lambda (&rest args) (message "Overwrite-mode has been removed from entropy-emacs.")))
;; ** Dired config
(use-package dired
  :ensure nil
  :init
;; *** Delete directory with force actions

  (setq entropy/dired-delete-file-mode-map (make-sparse-keymap))
  (define-minor-mode entropy/dired-delete-file-mode
    "Minor mode for func `entropy/dired-delete-file-recursive'."
    :keymap 'entropy/dired-delete-file-mode-map
    :global nil)

  (defvar entropy/dired-file-current-delete nil
    "Current file pre deleted by
    `entropy/dired-delete-file-recursive'.")
  
  (defvar entropy/dired-delete-file-refer-files nil
    "Files buffer killed by `entropy/dired-delete-file-recursive' log
    variable.")
  (defvar entropy/dired-delete-file-refer-dired-buffers nil
    "Dired buffer killed by `entropy/dired-delete-file-rescursie'
    log variable.")
  
  (defun entropy/dired-redelete-file ()
    "Redeletting file specified by variable
`entropy/dired-file-current-delete'."
    (interactive)
    (kill-buffer)
    (entropy/dired-delete-file-recursive entropy/dired-file-current-delete))


  (defun entropy/dired-delete-file-prompt (files-list)
    "popup buffer to deleting with prompting and return the
condition state for whether be continuing rest process."
    (if (dired-mark-pop-up " *Deletions*"
                           'delete
                           files-list
                           dired-deletion-confirmer
                           (format "%s %s " "Deleting" (dired-mark-prompt nil files-list)))
        t
      (error "Cancel deleting files!")))

  

  (defun entropy/dired-delete-file-recursive (&optional pre-files just-kill-refers)
    "Delete file recursively with refer buffer
cleaned (i.e. files under this dir will cleaned their linked
opened buffer within current emacs session.)

This func was binding to 'D' in `dired-mode-map' which be the
replacement for func `dired-do-delete', this func's advantageous
than it was given the deletion failed handle for responding to
some directory.

Error handle will switching to special buffer ‘*[w32-resmon]*’
buffer with minor mode `entropy/dired-delete-file-mode' for
prompting for how to resolving deletions problems.

In win32 platform using 'resmon' for conflicates resolve tool.  "
    (interactive)
    (let ((base-files (cond ((and (equal major-mode 'dired-mode)
                                  (not pre-files))
                             (dired-get-marked-files))
                            ((and (not (null pre-files))
                                  (listp pre-files))
                             pre-files)
                            (t (error "Dir list invalid!")))))


      (unless just-kill-refers
        (entropy/dired-delete-file-prompt base-files))
      
      (dolist (file base-files)

        ;; killed refer buffers.
        (dolist (el (buffer-list))
          (let* ((buffer-file (buffer-file-name el)))
            (when (and buffer-file
                       (string-match (regexp-quote file) buffer-file))
              (add-to-list 'entropy/dired-delete-file-refer-files
                           (cons (buffer-name el) (current-time-string)))
              (kill-buffer el))))

        ;; killed refer dired buffers
        (dolist (el dired-buffers)
          (when (string-match (regexp-quote file) (car el))
            (add-to-list 'entropy/dired-delete-file-refer-dired-buffers
                         (cons el (current-time-string)))
            (kill-buffer (cdr el))))
        
        (condition-case nil
            (when (not just-kill-refers)
              (progn
                (setq entropy/dired-file-current-delete (list file))
                (cond ((f-directory-p file)
                       (delete-directory file t))
                      ((f-file-p file)
                       (delete-file file)))
                (when (equal major-mode 'dired-mode)
                  (revert-buffer))
                (message (format "Delete file '%s' done! -v-" file))))
          (error
           (cond ((eq system-type 'windows-nt)
                  (let ((prompt-buffer (get-buffer-create "*[w32-resmon]*")))
                    (start-process-shell-command
                     "windows resource monitor"
                     prompt-buffer
                     "resmon")
                    (switch-to-buffer prompt-buffer)
                    (goto-char (point-min))
                    (entropy/dired-delete-file-mode)
                    (insert
                     (format
                      (concat
                       "==============Prompt Info=================\n\n"
                       "Kill all handle refer file:\n %s\n"
                       "with filter with windows resmon!\n\n" 
                       "And try again with answer minibuffer prompts delete again\n\n"
                       "===============Prompt End=================")
                      file))
                    (entropy/dired-redelete-file)))
                 (t (message "Kill all refer task and try again!"))))))))


  (defun entropy/dired-delete-file-refers ()
    "Kill all refers of dired markd file of directories."
    (interactive)
    (entropy/dired-delete-file-recursive nil t))
  
  
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "D") 'entropy/dired-delete-file-recursive)
    (define-key dired-mode-map (kbd "M-d") 'entropy/dired-delete-file-refers))


  
;; *** init hide details
  :config
  (add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode)))
;; *** Set unit of dired inode for human readable
  (if (and (not sys/win32p)
           (not sys/cygwinp))
      ;; because of windows dired list is too long so just let it in linux
      (setq dired-listing-switches "-alFh --group-directories-first")
    (setq dired-listing-switches "-alh"))

;; *** Always delete and copy resursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
;; *** Use dired-aux to enable dired-isearch
  (use-package dired-aux :ensure nil)
;; *** Quick sort dired buffers via hydra
  ;;; bind key: `S'
  (when (not sys/win32p)
    (use-package dired-quick-sort
      :if (or (executable-find "gls") (executable-find "ls"))
      :commands (dired-quick-sort-setup)
      :init (dired-quick-sort-setup)))

;; *** Use coloful dired ls
  (defun entropy/dired-visual-init ()
    "Init dired colorful visual featuer."
    (cond ((or (string= entropy/emacs-dired-visual-type "simple-rainbow")
               (version= emacs-version "25.3.1"))
           (use-package dired-rainbow
             :commands dired-rainbow-define dired-rainbow-define-chmod
             :init
             (dired-rainbow-define dotfiles "gray" "\\..*")

             (dired-rainbow-define web "#4e9a06" ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
					          "json" "asp" "aspx" "haml" "php" "jsp" "ts"
					          "coffee" "scss" "less" "phtml"))
             (dired-rainbow-define prog "yellow3" ("el" "l" "ml" "py" "rb" "pl" "pm" "c"
					           "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
					           "m" "cs" "mk" "make" "swift" "go" "java"
					           "asm" "robot" "yml" "yaml" "rake" "lua"))
             (dired-rainbow-define sh "green yellow" ("sh" "bash" "zsh" "fish" "csh" "ksh"
					              "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
             (dired-rainbow-define text "yellow green" ("txt" "md" "org" "ini" "conf" "rc"
					                "vim" "vimrc" "exrc"))
             (dired-rainbow-define doc "spring green" ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
					               "csv" "rtf" "wps" "pdf" "texi" "tex"
					               "odt" "ott" "odp" "otp" "ods" "ots"
					               "odg" "otg"))
             (dired-rainbow-define misc "gray50" ("DS_Store" "projectile" "cache" "elc"
					          "dat" "meta"))
             (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
					            "wmv" "mov" "3gp" "avi" "mpg" "mkv"
					            "flv" "ogg" "rm" "rmvb"))
             (dired-rainbow-define picture "purple3" ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
					              "ico" "svg" "psd" "pcd" "raw" "exif"
					              "BMP" "JPG" "PNG"))
             (dired-rainbow-define archive "saddle brown" ("zip" "tar" "gz" "tgz" "7z" "rar"
						           "gzip" "xz" "001" "ace" "bz2" "lz"
						           "lzma" "bzip2" "cab" "jar" "iso"))

             ;; boring regexp due to lack of imagination
             (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")

             ;; highlight executable files, but not directories
             (dired-rainbow-define-chmod executable-unix "green" "-[rw-]+x.*"))
           (if (string= entropy/emacs-dired-visual-type "all-the-icons")
               (warn " Because you are in emacs 25.3.1, just can
using simple dired visual type, although you have seting it to
\"all-the-icons\".")))
          ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
                (not (version= emacs-version "25.3.1"))
                (display-graphic-p))
           (require 'font-lock+)
           (use-package all-the-icons-dired
             :commands (all-the-icons-dired-mode)
             :hook (dired-mode . all-the-icons-dired-mode)))
          ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
                (not (display-graphic-p)))
           (setq entropy/emacs-dired-visual-type "simple-rainbow")
           (warn "You are in terminal emacs session, can not
           enable 'dired-all-the-icons', enable simple-rainbow
           instead now. ")
           (entropy/dired-visual-init))
          (t (error "entropy/emacs-dired-visual-type invalid"))))
  (entropy/dired-visual-init)
  
;; *** bind 'M-<up>' for dired updir
  (define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)
;; *** Improve dired files operation experience for kill opened refer buffers.
  (defun entropy/kill-redundant-buffer (&rest rest-args)
    "Delete file refer redundant buffer which will cause dired
'delete' or 'rename' failed."
    (dolist (el (mapcar 'buffer-name (buffer-list)))
      (dolist (re-el '("\\*Echo Area"
                       "\\*RNC Input\\*"))
        (when (string-match-p re-el el)
          (kill-buffer el)))))
  (advice-add 'dired-do-rename :before #'entropy/kill-redundant-buffer)
  (advice-add 'dired-do-flagged-delete :before #'entropy/kill-redundant-buffer)
;; *** get both UNIX and WINDOWS style path string
  (defvar entropy/get-dired-fpath-log nil)
  (defun entropy/get-dired-fpath (type)
    (interactive
     (list (completing-read "Choose path string type: "
                            '("unix" "win32"))))
    (let (rtn
          (files_get (dired-get-marked-files)))
      (dolist (el files_get)
        (cond
         ((string= type "win32")
          (push (replace-regexp-in-string
                 "/"
                 "\\\\"
                 el)
                rtn))
         (t
          (push el rtn))))
      
      (cond
       ((= (length rtn) 1)
        (with-temp-buffer
          (when buffer-read-only
            (read-only-mode 0))
          (goto-char (point-min))
          (insert (car rtn))
          (kill-ring-save (point-min) (point-max))
          (message (format "Save '%s' to kill-ring." (car rtn)))))
       (t
        (setq rtn (reverse rtn)
              entropy/get-dired-fpath-log rtn)
        (message "Save all path string to log variable 'entropy/get-dired-fpath-log'.")))))
  (define-key dired-mode-map (kbd "0 w") 'entropy/get-dired-fpath)

;; *** dired add load path
  (defun entropy/dired-add-to-load-path ()
    (interactive)
    (let ((dir (completing-read "Choose load path adding item: "
                                'read-file-name-internal
                                nil t)))
      (unless (file-directory-p dir)
        (setq dir (file-name-directory dir)))
      (add-to-list 'load-path dir)))
  (define-key dired-mode-map (kbd "M-l") 'entropy/dired-add-to-load-path))


;; ** image-mode
(use-package image-mode
  :ensure nil
  :config
  (defun entropy/image-gif-warn (&optional args)
    "Warn that gif animation by large gif file will freeze
emacs."
    (if (string-match-p "\\.gif" (buffer-name))
        (if (not (y-or-n-p "Do you want to animated it? "))
            (error "Please open it with external apps!"))))
  (advice-add 'image-toggle-animation :before #'entropy/image-gif-warn))

;; ** Set transparenct of emacs frame
(global-set-key (kbd "<f6>") 'entropy/loop-alpha)  ;;注意这行中的F8 , 可以改成你想要的按键    

(defun entropy/loop-alpha ()    
  (interactive)    
  (let ((h (car entropy/emacs-loop-alpha-value)))
    (funcall
     (lambda (a ab)    
       (set-frame-parameter (selected-frame) 'alpha (list a ab))    
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h)
     (car (cdr h)))
    (setq entropy/emacs-loop-alpha-value (cdr (append entropy/emacs-loop-alpha-value (list h))))))

(when entropy/emacs-init-loop-alpha
    (entropy/loop-alpha))

;; ** Paragraph fill size
(setq-default fill-column entropy/emacs-fill-paragraph-width)
;; ** Show time on mode line
(when entropy/emacs-display-time-modeline
  ;; 启用时间显示设置
  (display-time-mode 1)
  (setq-default display-time-interval 1)
  ;; 时间显示包括日期和具体时间
  (setq display-time-day-and-date t)
  ;; 时间使用24小时制
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-format "%I:%M %p %e %b %y %_5j")
  (setq display-time-format "%e %b %Y %H:%M:%S")
  (display-time))
;; ** Input time into buffer
;; (defun now ()
;;   "Insert string for the current time formatted like '2:34 PM'."
;;   (interactive)                 ; permit invocation in minibuffer
;;   (insert (format-time-string "%D %-I:%M %p")))

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))


(defun today ()
  "Insert string for today's date nicely formatted in American style,
 e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; ** Read-Only-About
(use-package entropy-global-read-only-mode
  :ensure nil
  :commands (entropy-grom-mode)
  :init (add-hook 'entropy/init-mini-hook #'entropy-grom-mode))
;; ** Revert buffer
(global-auto-revert-mode t)
;; ** Use popwin mode
(use-package popwin
  :commands popwin-mode
  :init (add-hook 'entropy/init-mini-hook #'popwin-mode)
  :config
  (bind-key "C-3" popwin:keymap)

  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(;; Emacs
          ("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)

          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)

          ;; Flycheck
          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)

          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)

          ;; Google translate
          ("*Google Translate*" :dedicated t :position bottom)

          ;; Moedict
          ("*[萌典] 查詢結果*" :dedicated t :position bottom)
          
          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)

          ;; Diff
          ("*Diff*" :dedicated t :position bottom :noselect nil)
          
          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)

          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)

          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ;; ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)

          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Magit
          ;; (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          ;; (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)

          ;; Script
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)

          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil))))
;; ** Use which-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (add-hook 'entropy/init-mini-hook #'which-key-mode)
  (setq which-key-popup-type 'minibuffer))

;; ** Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :init
  (add-hook 'entropy/init-mini-hook #'global-undo-tree-mode)
  (add-hook 'undo-tree-mode-hook
	    '(lambda () (define-key undo-tree-map (kbd "C-x u") nil)))
  :config
  (defun undo-tree-visualizer-quit ()
    "Quit the undo-tree visualizer."
    (interactive)
    (unless (eq major-mode 'undo-tree-visualizer-mode)
      (user-error "Undo-tree mode not enabled in buffer"))
    (undo-tree-clear-visualizer-data buffer-undo-tree)
    ;; remove kill visualizer hook from parent buffer
    (unwind-protect
        (with-current-buffer undo-tree-visualizer-parent-buffer
          (remove-hook 'before-change-functions 'undo-tree-kill-visualizer t))
      ;; kill diff buffer, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-hide-diff))
      (let ((parent undo-tree-visualizer-parent-buffer)
            window)
        ;; kill visualizer buffer
        (kill-buffer nil)
        ;; switch back to parent buffer
        (unwind-protect
            ;; (if (setq window (get-buffer-window parent))
            ;;     (select-window window))
            (if entropy/undo-tree-margin-detective
                (progn
                  (switch-to-buffer-other-window parent)
                  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                                      (/ (window-width) 4)
                                      (/ (window-width) 4)))
              (switch-to-buffer parent)))))))

(defun entropy/undo-tree ()
  (interactive)
  (if (car (window-margins))
      (progn
        (setq entropy/undo-tree-margin-detective t)
        (entropy/center-text-clear)
        (undo-tree-visualize))
    (progn
      (setq entropy/undo-tree-margin-detective nil)
      (undo-tree-visualize))))
(global-set-key (kbd "C-x u") 'entropy/undo-tree)

;; ** Auto-sudoedit
(when (and (not sys/win32p)
           (not sys/cygwinp))
  (use-package auto-sudoedit
    :commands (auto-sudoedit-mode)
    :init
    (auto-sudoedit-mode 1)))

;; *** clear killring
;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

;; ** Windows garbage
;; (when (eq system-type 'windows-nt)
;;   (setq gc-cons-threshold (* 5 1024))
;;   (setq gc-cons-percentage 0.5)
;;   (run-with-idle-timer 5 t #'garbage-collect)
;;   ;; 显示垃圾回收信息，这个可以作为调试用
;;   ;; (setq garbage-collection-messages t)
;;   )
;; ** Windows mark-sexp
(when sys/win32p
  (global-set-key (kbd "C-`") 'set-mark-command))
(defun entropy/mark-set ()
  (interactive)
  (save-excursion
    (push-mark)
    (push-mark)))
(global-set-key (kbd "C-2") 'entropy/mark-set)
;; ** Windows forbidden view-hello-file
(when (or sys/win32p sys/cygwinp)
  (defun view-hello-file ()
    "Prompt emacs user do not use view-hello-file in windows operation system"
    (interactive)
    (message "Do not use view-hello-file in windows because of it will jamm windows and emacs")))
;; ** Delete file to trash
(if entropy/emacs-dired-enable-trash
    (setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder but with some
                                               ; performance bug on windows plattform.
  )

;; ** History && Recentf
(use-package saveplace
  :ensure nil
  :init
  ;; Emacs 25 has a proper mode for `save-place'
  (if (fboundp 'save-place-mode)
      (save-place-mode)
    (setq save-place t)))

(use-package recentf
  :if entropy/emacs-use-recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)

  ;; lazy load recentf
  ;;(add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'find-file-hook '(lambda () (unless recentf-mode
					  (recentf-mode)
					  (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode))
;; ** Bookmarks autosave
(setq bookmark-save-flag 1)
;; *** bookmark utf-8
(when entropy/emacs-custom-language-environment-enable
  (dolist (hook '(bookmark-edit-annotation-mode-hook
                  bookmark-bmenu-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (if (not (string= current-language-environment "UTF-8"))
                      (progn
                        (set-language-environment "UTF-8")
                        (prefer-coding-system 'utf-8-unix)
                        (setq buffer-file-coding-system 'utf-8-unix)
                        (setq process-coding-system 'utf-8-unix)
                        (set-terminal-coding-system 'utf-8-unix)
                        (set-keyboard-coding-system 'utf-8-unix)
                        )))))

  (use-package bookmark
    :ensure nil
    :config
    (defun bookmark-set (&optional name no-overwrite)
      "Set a bookmark named NAME at the current location.
If NAME is nil, then prompt the user.

With a prefix arg (non-nil NO-OVERWRITE), do not overwrite any
existing bookmark that has the same name as NAME, but instead push the
new bookmark onto the bookmark alist.  The most recently set bookmark
with name NAME is thus the one in effect at any given time, but the
others are still there, should the user decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)"
      (interactive (list nil current-prefix-arg))
      (progn
        (set-language-environment "UTF-8")
        (prefer-coding-system 'utf-8-unix)
        (setq buffer-file-coding-system 'utf-8-unix)
        (setq process-coding-system 'utf-8-unix)
        (set-terminal-coding-system 'utf-8-unix)
        (set-keyboard-coding-system 'utf-8-unix)
        )
      (let ((prompt
             (if no-overwrite "Set bookmark" "Set bookmark unconditionally")))
        (bookmark-set-internal prompt name (if no-overwrite 'push 'overwrite))))
    ))
                  
;; ** Major mode reload
(defun entropy/major-mode-reload ()
  "Reload current `major-mode'.

  This function was usable for some occurrence that current mode
  functional part was missed or be without working on the proper
  way. "
  (interactive)
  (if (not (string-match-p "^CAPTURE-" (buffer-name)))
      (let ((point (point))
            (mode major-mode))
        (funcall mode)
        (when (eq major-mode 'org-mode)
          (outline-show-all))
        (goto-char point)
        (message "Reloaded current major mode '%s'!" (symbol-name major-mode)))
    (error "You can not refresh %s in this buffer, if did may cause some bug." (symbol-name major-mode))))
(global-set-key (kbd "<f7>") 'entropy/major-mode-reload)
;; ** disable-mouse-wheel and more
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :commands (global-disable-mouse-mode)
  :init
  (add-hook 'entropy/init-mini-hook #'global-disable-mouse-mode))
;; ** Artist-mode
(use-package artist-mode
  :ensure nil
  :init
  ;; Init disable rubber-banding for reducing performance requirements.
  (add-hook 'artist-mode-hook
            #'(lambda () 
                (if artist-rubber-banding
                    (setq-local artist-rubber-banding nil)))))

(defun entropy/ex-toggle-artist-and-text ()
  (interactive)
  "Toggle mode between `text-mode' & `artist-mode'."
  (cond ((eq major-mode 'picture-mode)
         (text-mode))
        ((eq major-mode 'text-mode)
         (yes-or-no-p "Really for that? (maybe you don't want to change to artist!) ")
         (artist-mode))))
(with-eval-after-load 'artist
  (define-key artist-mode-map (kbd "<f5>") 'entropy/ex-toggle-artist-and-text))
(with-eval-after-load 'text-mode
  (define-key text-mode-map (kbd "<f5>") 'entropy/ex-toggle-artist-and-text))

;; Disabled '<' and '>' keybinding function.
(with-eval-after-load 'artist
  (define-key artist-mode-map (kbd ">") nil)
  (define-key artist-mode-map (kbd "<") nil))

(defun entropy/artist-mode ()
  "Open one temp-file with artist-mode.
Temp file was \"~/~entropy-artist.txt\""
  (interactive)
  (find-file "~/~entropy-artist.txt")
  (artist-mode))

;; ** disable auto-window-vscroll -----> the resean from
;; `https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag'
(setq auto-window-vscroll nil)
;; ** use chinese pyim
(if (and entropy/emacs-enable-pyim
         (or sys/win32p sys/linux-x-p sys/mac-x-p))
    (use-package pyim
      :bind
      (("M-j" . pyim-convert-code-at-point))
      :diminish chinese-pyim-mode
      :demand t
      :commands (pyim-restart-1)
      :config
;; *** 词库设置
      (if (not entropy/emacs-pyim-dicts)
          (use-package pyim-basedict
            :ensure nil
            :config (pyim-basedict-enable))
        (setq pyim-dicts entropy/emacs-pyim-dicts))
;; *** 默认输入法设置
      (setq default-input-method "pyim")

;; *** 使用全拼
      (setq pyim-default-scheme 'quanpin)
;; *** use popup or posframe for pyim tooltip show
      ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
      ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
      ;; 手动安装 posframe 包。
      (if (version< emacs-version "26")
          (if (or (eq entropy/emacs-pyim-tooltip 'posframe)
                  (not entropy/emacs-pyim-tooltip))
              (setq pyim-page-tooltip 'popup)
            (setq pyim-page-tooltip entropy/emacs-pyim-tooltip))
        (progn
          (use-package posframe)
          (if entropy/emacs-pyim-tooltip
              (setq pyim-page-tooltip entropy/emacs-pyim-tooltip)
            (setq pyim-page-tooltip 'posframe))))

      (when entropy/emacs-pyim-cached-dir
        (setq pyim-dcache-directory entropy/emacs-pyim-cached-dir))

;; *** 选词框显示5个候选词
      (setq pyim-page-length 5)

;; *** toggle input method
      (defun entropy/pyim-toggle ()
        (interactive)
        (if (string= current-input-method "pyim")
            (set-input-method "rfc1345")
          (progn
            (set-input-method "pyim")
            (setq pyim-punctuation-escape-list nil))))
      (global-set-key (kbd "C-\\") 'entropy/pyim-toggle)
;; *** 使用C-g来取消输入
      (if (not (version< emacs-version "26"))
          (define-key pyim-mode-map (kbd "C-g") 'pyim-quit-clear))
;; *** 简繁转换
      (use-package entropy-s2t
        :ensure nil
        :commands entropy/s2t-string)
      (defun entropy/toggle-pyim-s2t ()
        (interactive)
        (if pyim-magic-converter
            (setq pyim-magic-converter nil)
          (setq pyim-magic-converter 'entropy/s2t-string)))
      (global-set-key (kbd "C-M-\\") 'entropy/toggle-pyim-s2t)

;; *** 切换全角半角标点符号
      (defun entropy/toggle-pyim-punctuation-half-or-full ()
        (interactive)
        (if (eq (car pyim-punctuation-translate-p) 'no)
            (setq pyim-punctuation-translate-p '(yes no auto))
          (setq pyim-punctuation-translate-p '(no yes auto))))
      (global-set-key (kbd "C-1") 'entropy/toggle-pyim-punctuation-half-or-full))
  
;; *** If didn't use pyim set input method to nil
  (setq default-input-method nil))


;; ** enable disabled commands
(put 'narrow-to-region 'disabled nil)
;; ** 'super' and 'hyper' key
(progn
  ;; Binding 'super' and 'hyper' on win32 and mac.
  ;;   the idea form `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'
  (cond
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on
    ;; Windows.

    ;; (setq w32-pass-apps-to-system nil)
    ;; (setq w32-pass-rwindow-to-system nil)
    ;; (setq w32-pass-lwindow-to-system nil)

    ;; (setq w32-rwindow-modifier 'meta)
                                         ; Right Windows key
    ;; (setq w32-lwindow-modifier 'alt)
                                     ; Left Windows key setting to alt key for tipical freezing
                                     ; emacs the bug of win 10 that capslock key can not functional
                                     ; when unlock screensaver.
    
    (setq w32-apps-modifier 'hyper) ; Menu/App key

    (global-set-key (kbd "C-M-g") 'keyboard-quit) ; when unintended active this, using 'QUIT' as 'C-g'
    (global-set-key (kbd "C-s-g") 'keyboard-quit) ; same as above of super key intended active
    (global-set-key (kbd "A-C-g") 'keyboard-quit) ; same as above of super key intended active

    ;; actived for `aya-create' and `aya-expand'
    (w32-register-hot-key [h-e])
    (w32-register-hot-key [h-w]))
   
   (sys/macp
    ;; set keys for Apple keyboard, for emacs in OS X
    (setq mac-command-modifier 'meta) ; make cmd key do Meta
    (setq mac-option-modifier 'super) ; make opt key do Super
    (setq mac-control-modifier 'control) ; make Control key do Control
    (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
    )))
;; ** adding advice for `y-or-n-p' for emacs 26 and higher in widnows plattform
(when (and sys/win32p (not (version< emacs-version "26.1")))
  (defun entropy/y-or-n-p (prompt)
    (let ((judge (completing-read prompt '("yes" "no") nil t)))
      (if (string= judge "yes")
          t
        nil)))
  
  ;; adding advice ro y-or-n-p for temporarily fix bug of that can not using any key-bindings when
  ;; active "C-<lwindow>-g" in windows
  (advice-add 'y-or-n-p :override #'entropy/y-or-n-p))

;; ** epa (emacs gpg assistant)
(use-package epa
  :ensure nil
  :init
  (epa-file-enable)
  (when (and entropy/emacs-wsl-enable
             (file-exists-p entropy/emacs-wsl-apps))
    (with-eval-after-load 'custom
      (custom-set-variables
       '(epg-gpg-program (expand-file-name "gpg.exe" entropy/emacs-wsl-apps))
       '(epg-gpgconf-program (expand-file-name "gpgconf.exe" entropy/emacs-wsl-apps))
       '(epg-gpgsm-program (expand-file-name "gpgsm.exe" entropy/emacs-wsl-apps))))))

;; ** process refer
;; *** process
(global-set-key (kbd "C-c s s") 'list-processes)
;; *** proced
(use-package proced
  :ensure nil
  :commands (proced-process-attributes)
  :init
  (setq-default proced-format 'medium)

  (defun entropy/proced-processP (processName)
    "Return one alist collected the proced info of procssName,
otherwise returns nil."
    (let ((procedList (proced-process-attributes))
          rtn)
      (dolist (el procedList)
        (when (equal (cdr (assq 'comm el))
                     processName)
          (push el rtn)))
      rtn))

  (defun entropy/proced-auto-startwith (processName $executable)
    (unless (entropy/proced-processP processName)
      (cl-case system-type
        (windows-nt
         (when (fboundp 'w32-shell-execute)
           (let ((default-directory temporary-file-directory))
             (w32-shell-execute
              "open" $executable)
             (message (format "Start with '%s'."
                              $executable)))))
        (t (message
            "`entropy/proced-auto-startwith' are just used in w32 platform")))))

  (dolist (el entropy/emacs-startwith-apps)
    (when (executable-find (cdr el))
      (entropy/proced-auto-startwith (car el) (cdr el)))))

;; ** improve captialize function

;; Due to the convention while want to capitalize or uper-case the word just has been done, building
;; follow two function to enhance the origin function `capitalize-word' and `upercase-word'.
(defun entropy/capitalize-word (arg)
  "Automatically go ahead of previous word before call `capitalize-word'."
  (interactive "P")
  (left-word)
  (call-interactively 'capitalize-word t (vector arg)))

(defun entropy/upcase-word (arg)
  "automatically go ahead of previous word before call `upcase-word'."
  (interactive "P")
  (left-word)
  (call-interactively 'upcase-word t (vector arg)))

(global-set-key (kbd "M-c") 'entropy/capitalize-word)
(global-set-key (kbd "M-u") 'entropy/upcase-word)

;; * provide
(provide 'entropy-emacs-basic)
