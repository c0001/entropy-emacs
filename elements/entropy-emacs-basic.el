;;; entropy-emacs-basic.el --- entropy emacs basic config
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-basic.el
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
;; This file was the `entropy-emacs' basic config file, config of set
;; of emacs internal features and using some extra extensions to
;; enhance the basic emacs experience.
;;
;; Also given the simple workspace management based on `eyebrowse'
;; and hacks of entropy-emacs internally for derived workspace
;; environment.
;;
;; File using `outline mode' to manage the context level and the
;; categories. 
;;
;; This file were enabled for 'Mini mode' (specified of
;; `entropy/emacs-minimal-start') and well for the full startup
;; type. Set of auto-start feature in this file are injecting into
;; hook `entropy/emacs-init-mini-hook' checking this hook for viewing
;; the features loading at start-up time.
;;
;; * Configuration:
;;
;; This file must loaded by `entorpy-emacs.el', other testing way is
;; not in the designation context.
;;
;; 
;; * Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-faces)

;; ** Temporal bug revert
;; *** gnutls bug for emacs version upper than '26.1'
;; 
;; Bug refer emacs `url.el' bug or possible for the gnutls bug override.
;;
;; Refer:
;; @see https://github.com/magit/ghub/issues/81#issuecomment-488660597
;; For now [2019-08-08 Thu 19:23:42] it seems occur on w32 port only
(when (and (version< "26.1" emacs-version)
           sys/win32p)
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

(defun entropy/emacs-basic-toggle-cursor-type ()
  (interactive)
  (if (string= (symbol-name cursor-type) "t")
      (setq cursor-type 'bar)
    (setq cursor-type t)))

;; *** Global display line number mode
(if (>= emacs-major-version 26)
    (progn
      (setq-default display-line-numbers-width-start t)
      (when entropy/emacs-init-display-line-mode
        (global-display-line-numbers-mode))))

;; ** Set the default dired directory
(setq default-directory "~/")

;; ** Backup setting
(setq-default auto-save-default nil)    ;disable it for preventing typing lagging
(setq make-backup-files nil)

;; ** Scratch buffer corresponding file
;; 
;;     Amounts of company backend function can not functional
;;     auto-completion in none file buffer, so corresponding one file
;;     to *scratch* buffer.

(defun entropy/emacs-basic--scratch-buffer-file-binding ()
  "Corresponded *scratch* buffer to one temp-file.

  Filename are \".scratch_entropy\" in `user-emacs-directory'.
  "
  (let ((bfn "*scratch*"))
    (if (entropy/emacs-buffer-exists-p "*scratch*")
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
              (lisp-interaction-mode)
              (insert initial-scratch-message)))
        (with-current-buffer (find-file-noselect fname)
          (if buffer-read-only (read-only-mode 0))
          (auto-save-mode 0)
          (rename-buffer "*scratch*")
          (lisp-interaction-mode)
          (goto-char (point-min))
          (if (not (re-search-forward (regexp-quote initial-scratch-message) nil t))
              (insert initial-scratch-message)))))
    bfn))

(entropy/emacs-lazy-load-simple 'entropy-emacs-structure
  (entropy/emacs-basic--scratch-buffer-file-binding))

;; Create a new scratch buffer
(defun entropy/emacs-tools-create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (entropy/emacs-basic--scratch-buffer-file-binding))
  (lisp-interaction-mode)
  (message "Create *scratch* buffer"))

;; ** Highlight current line
(defun entropy/emacs-basic--dhl-judge-state ()
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

(defun entropy/emacs-basic-dhl-toggle ($Prefix)
  (interactive "P")
  (if (not (version< emacs-version "26.1"))
      (if (not $Prefix)
          (cl-case (entropy/emacs-basic--dhl-judge-state)
            (1
             (hl-line-mode 1))
            (2
             (hl-line-mode 1))
            (3
             (hl-line-mode 0))
            (4
             (hl-line-mode 0)))
        (cl-case (entropy/emacs-basic--dhl-judge-state)
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
      
(global-set-key (kbd "<f2>") 'entropy/emacs-basic-dhl-toggle)

;; ** Smooth scrolling
(defvar entropy/emacs-basic-smooth-scrolling-mode nil
  "Indicated whether smooth-scrolling enable.

Note:

Manually edit this variable will not be any effection.")

(setq scroll-step 0)
(setq scroll-conservatively 0)

(defun entropy/emacs-basic-smooth-scrolling ()
  "Toggle smooth-scrolling buffer scrolling view."
  (interactive)
  (setq redisplay-dont-pause t
        scroll-margin 0
        scroll-step   (if (equal scroll-step 0) 1 0)
        scroll-conservatively (if (equal scroll-conservatively 0) 10000 0))
  (if (and (equal scroll-step 1)
           (equal scroll-conservatively 10000))
      (progn
        (setq entropy/emacs-basic-smooth-scrolling-mode t)
        (message "Smooth scrolling enabled!"))
    (progn
      (setq entropy/emacs-basic-smooth-scrolling-mode nil)
      (message "Smooth scrolling disabled!"))))

(entropy/emacs-basic-smooth-scrolling)

;; ** Window-setting
;; *** Window switch
(use-package window-number
  :commands (window-number-switch
             window-number-mode)
  :bind
  ("C-x o" . window-number-switch)
  :init
  (window-number-mode 1))

;; **** Use windmove function stolen :) from `https://github.com/troydm/emacs-stuff/blob/master/windcycle.el'
(defun entropy/emacs-basic-windmove-up-cycle ()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)
             (error (condition-case nil (windmove-right)
                      (error (condition-case nil (windmove-left)
                               (error (windmove-up))))))))))

(defun entropy/emacs-basic-windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)
             (error (condition-case nil (windmove-left)
                      (error (condition-case nil (windmove-right)
                               (error (windmove-down))))))))))

(defun entropy/emacs-basic-windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)
             (error (condition-case nil (windmove-up)
                      (error (condition-case nil (windmove-down)
                               (error (windmove-right))))))))))

(defun entropy/emacs-basic-windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)
             (error (condition-case nil (windmove-down)
                      (error (condition-case nil (windmove-up)
                               (error (windmove-left))))))))))

(global-set-key (kbd "C-x <up>") 'entropy/emacs-basic-windmove-up-cycle)
(global-set-key (kbd "C-x <down>") 'entropy/emacs-basic-windmove-down-cycle)
(global-set-key (kbd "C-x <right>") 'entropy/emacs-basic-windmove-right-cycle)
(global-set-key (kbd "C-x <left>") 'entropy/emacs-basic-windmove-left-cycle)

;; ***** Disable buffer reverse and turn by =C-x C-left= =C-x C-right=
(global-set-key (kbd "C-x C-<left>") nil)
(global-set-key (kbd "C-x C-<right>") nil)


;; *** window config
;; **** eyebrowse ----> for save the window config(workspace group)
(use-package eyebrowse
  :commands (eyebrowse--current-window-config
             eyebrowse--delete-window-config
             eyebrowse--dotted-list-p
             eyebrowse--fixup-window-config
             eyebrowse--get
             eyebrowse--insert-in-window-config-list
             eyebrowse--load-window-config
             eyebrowse--read-slot
             eyebrowse--rename-window-config-buffers
             eyebrowse--set
             eyebrowse--string-to-number
             eyebrowse--update-window-config-element
             eyebrowse--walk-window-config
             eyebrowse--window-config-present-p
             eyebrowse-close-window-config
             eyebrowse-create-window-config
             eyebrowse-format-slot
             eyebrowse-free-slot
             eyebrowse-init
             eyebrowse-last-window-config
             eyebrowse-mode
             eyebrowse-mode-line-indicator
             eyebrowse-next-window-config
             eyebrowse-prev-window-config
             eyebrowse-rename-window-config
             eyebrowse-setup-evil-keys
             eyebrowse-setup-opinionated-keys
             eyebrowse-switch-to-window-config
             eyebrowse-switch-to-window-config-0
             eyebrowse-switch-to-window-config-1
             eyebrowse-switch-to-window-config-2
             eyebrowse-switch-to-window-config-3
             eyebrowse-switch-to-window-config-4
             eyebrowse-switch-to-window-config-5
             eyebrowse-switch-to-window-config-6
             eyebrowse-switch-to-window-config-7
             eyebrowse-switch-to-window-config-8
             eyebrowse-switch-to-window-config-9)
  
  :bind (("C-c C-w C-e" . entropy/emacs-basic-eyebrowse-create-workspaces)
         ("C-c C-w M-e" . entropy/emacs-basic-eyebrowse-delete-workspace)
         ("C-c C-w C-`" . entropy/emacs-basic-eyebrowse-switch-top)
         ("C-c v" . entropy/emacs-basic-eyebrowse-create-derived)
         ("C-c M-v" . entropy/emacs-basic-eyebrowse-switch-derived)
         :map eyebrowse-mode-map
         ("C-c C-w C-c" . entropy/emacs-basic-eyebrowse-create-window-config)
         ("C-c C-w c" . entropy/emacs-basic-eyebrowse-create-window-config)
         ("C-c C-w ." . entropy/emacs-basic-eyebrowse-switch-basic-window)
         ("C-c C-w a" . eyebrowse-switch-to-window-config))
  :init
  (setq eyebrowse-mode-line-style nil)
  
  (entropy/emacs-lazy-load-simple 'eyebrowse
    (unless (fboundp 'pdumper-stats)
      ;; disable eyebrowse-mode initilized when use pdumper emacs feature
      (eyebrowse-mode))
    (if entropy/emacs-enable-eyebrowse-new-workspace-init-function
        (setq eyebrowse-new-workspace entropy/emacs-basic--eyebrowse-new-workspace-init-function)
      (setq eyebrowse-new-workspace t)))

  :config
  ;; debug for improving eyebrowse's user experience
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
          (user-error "Invalid slot number"))))

  (defun entropy/emacs-basic-eyebrowse-create-window-config ()
    "Creates a window config at a yet unoccupied slot and named
    this work space."
    (interactive)
    (funcall #'eyebrowse-create-window-config)
    (let ((slot (eyebrowse--get 'current-slot))
          (tag (read-string "Tag: ")))
      (apply #'eyebrowse-rename-window-config `(,slot ,tag))))
  
  (defun entropy/emacs-basic--eyebrowse-show-current-slot ()
    "Show current eyebrowse workspace slot and tag info."
    (interactive)
    (let* ((entropy/emacs-basic--eyebrowse-slot-result (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (window-config (assoc (eyebrowse--get 'current-slot) window-configs))
           (current-tag (nth 2 window-config)))
      (message "Slot:%s  Tag:%s" entropy/emacs-basic--eyebrowse-slot-result current-tag)))
  (global-set-key (kbd "C-c M-s") 'entropy/emacs-basic--eyebrowse-show-current-slot)

  (defun entropy/emacs-basic--eyebrowse-kill-all-group ()
    "Kill all eyebrowse window config"
    (interactive)
    (dolist (item (eyebrowse--get 'window-configs))
      (eyebrowse--delete-window-config (car item)))
    (eyebrowse-init))

  (defun entropy/emacs-basic-eyebrowse-create-workspaces (&optional ws-list $confirm)
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
    (entropy/emacs-basic--eyebrowse-kill-all-group)
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


  (defvar entropy/emacs-basic--eyebrowse-config-selected '()
    "Contained selected eyebrowse workspace config.")

  (defun entropy/emacs-basic--eyebrowse-read-prompt ()
    "Produce the prompt string for repeated selected eyebrowse
window configs."
    (format "WS (%s) : "
            (let ((olist entropy/emacs-basic--eyebrowse-config-selected)
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
    (defun entropy/emacs-basic--eyebrowse-read-config-repeated (x)
      "Used in repeated selected eyebrowse config with `ivy-call'.

This was the one action in `ivy-read'."
      (require 'ivy)
      (if (not (member x entropy/emacs-basic--eyebrowse-config-selected))
          (push x entropy/emacs-basic--eyebrowse-config-selected))
      (let ((prompt (entropy/emacs-basic--eyebrowse-read-prompt)))
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
  
  (defun entropy/emacs-basic-eyebrowse-delete-workspace ()
    "Delete eyebrowse workspace with prompt."
    (interactive)
    (require 'entropy-common-library)
    (require 'ivy)
    (setq entropy/emacs-basic--eyebrowse-config-selected nil)
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
                :action 'entropy/emacs-basic--eyebrowse-read-config-repeated)
      (dolist (el entropy/emacs-basic--eyebrowse-config-selected)
        (eyebrowse--delete-window-config (cdr (assoc el candi))))))


  (defun eyebrowse-free-slot (slots)
    "Returns a yet unoccupied slot.
The specific behaviour is tmux-like.

Note: this function has been redefine for
`entropy/emacs-basic-eyebrowse-create-derived'."
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

  
  (defun entropy/emacs-basic-eyebrowse-create-derived ()
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

  (defun entropy/emacs-basic-eyebrowse-switch-derived ()
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


  (defun entropy/emacs-basic-eyebrowse-switch-basic-window ()
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
  
  (defun entropy/emacs-basic-eyebrowse-switch-top ()
    "Back to the top workspace from current derived workspace."
    (interactive)
    (let* ((cslot (eyebrowse--get 'current-slot))
           (top-slot (floor cslot))
           (top-tag (nth 2 (assoc top-slot (eyebrowse--get 'window-configs)))))
      (cond
       ((not (equal cslot top-slot))
        (eyebrowse-switch-to-window-config top-slot)
        (message (concat (propertize "You've been back to top wg: "
                                     'face 'entropy/emacs-faces--basic-eyebrowse-back-top-wg-message-face_body)
                         (propertize (if (and (not (equal top-tag ""))
                                              (not (equal top-tag nil)))
                                         (format "[%s]: %s " top-slot top-tag)
                                       (format "[%s] " top-slot))
                                     'face 'entropy/emacs-faces--basic-eyebrowse-back-top-wg-message-face_content)
                         (propertize "." 'face 'entropy/emacs-faces--basic-eyebrowse-back-top-wg-message-face_body))))
       (t (error "You've at top wg!"))))))

;; **** winner mode for recover previous window config faster
(use-package winner
  :ensure nil
  :commands (winner-mode)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
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
  (entropy/emacs-lazy-initial-advice-before
   '(switch-to-buffer find-file delete-other-windows)
   "winner-mode" "winner-mode"
   (winner-mode +1))
  
  :config
  (winner-mode +1))

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
(defvar entropy/emacs-basic--kill-buffer-excluded-buffer-list nil
  "Buffer name regexp list stored excluded buffer name match for
  func `entropy/emacs-kill-buffer-and-window'.")

(defvar entropy/emacs-basic--kill-buffer-special-buffer-list
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
             (base-dir default-directory)
             (fname (buffer-file-name)))
         (kill-buffer buffn)
         (when (and (ignore-errors (stringp fname))
                    (file-writable-p fname))
           (dired base-dir)))))
    ("ansi-term"
     .
     (lambda ()
       "The patch for the bug of error after kill ansi-term
buffer and its popup window refer to bug
#h-0c3ab89e-a470-42d2-946e-4f217ea2f20c in entropy-emacs bug
collection."
       (if sys/linuxp
           (let* ((_buff (current-buffer))
                  (_proc (get-buffer-process _buff)))
             (when _proc
               (when (yes-or-no-p
                      (format "Buffer %S has a running process; kill it? "
                              (buffer-name _buff)))
                 (set-process-filter _proc nil)
                 (kill-process _proc)
                 (let ((kill-buffer-query-functions '((lambda () t))))
                   (if (not (one-window-p))
                       (kill-buffer-and-window)
                     (kill-this-buffer))))))
         (kill-this-buffer)))))
  "Special buffer alist which the car can be regexp string or
  list of regexp string, the cdr was buffer killing specific
  func. ")

(defun entropy/emacs-basic-kill-buffer-and-window ()
  "Kill buffer and window following rule by
`entropy/emacs-basic--kill-buffer-excluded-buffer-list' and
`entropy/emacs-basic--kill-buffer-special-buffer-list'.

Using func `entropy/emacs-basic--buffer-close' be the default func."
  (interactive)
  (let ((excluded entropy/emacs-basic--kill-buffer-excluded-buffer-list)
        (special entropy/emacs-basic--kill-buffer-special-buffer-list)
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
      (entropy/emacs-basic--buffer-close))
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
        (entropy/emacs-basic--buffer-close))))))

(global-set-key (kbd "C-x k") 'entropy/emacs-basic-kill-buffer-and-window)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

(defun entropy/emacs-basic--buffer-close ()
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
(defun entropy/emacs-basic-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun entropy/emacs-basic-kill-large-process-buffer ()
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
(global-set-key (kbd "C-0") 'entropy/emacs-basic-kill-large-process-buffer)


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
(defun entropy/emacs-basic-center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (if (car (window-margins))
      (entropy/emacs-basic-center-text-clear)
    (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        (/ (window-width) entropy/emacs-window-center-integer)
                        (/ (window-width) entropy/emacs-window-center-integer))))

(defun entropy/emacs-basic-center-text-clear ()
  (interactive)
  (set-window-margins
   (car (get-buffer-window-list (current-buffer) nil t))
   nil))
(global-set-key (kbd "C-c M-<up>")     'entropy/emacs-basic-center-text)
(global-set-key (kbd "C-c M-<down>")     'entropy/emacs-basic-center-text-clear)


;; *** Window divider
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

;; **** Specific cases to forceing using UTF-8 encoding environment
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


;; ***** let diff-buffer-with-file force run with unicode language environment
(advice-add 'diff-buffer-with-file :before #'entropy/emacs-lang-set-utf-8)

;; ** Auto wrap line
(setq-default truncate-lines t)
(global-set-key(kbd "C-<f9>") 'toggle-truncate-lines)
(setq org-startup-truncated t)

;; ** remove overwrite-mode
(advice-add 'overwrite-mode
            :override
            #'(lambda (&rest args) (message "Overwrite-mode has been removed from entropy-emacs.")))

;; ** Dired config
;; *** dired basic
(use-package dired
  :ensure nil
  :init
;; **** Delete directory with force actions
  (setq entropy/emacs-basic--dired-delete-file-mode-map (make-sparse-keymap))
  (define-minor-mode entropy/emacs-basic--dired-delete-file-mode
    "Minor mode for func `entropy/emacs-basic-dired-delete-file-recursive'."
    :keymap 'entropy/emacs-basic--dired-delete-file-mode-map
    :global nil)

  (defvar entropy/emacs-basic--dired-file-current-delete nil
    "Current file pre deleted by
`entropy/emacs-basic-dired-delete-file-recursive'.")
  
  (defvar entropy/emacs-basic--dired-delete-file-refer-files nil
    "Files buffer killed by
`entropy/emacs-basic-dired-delete-file-recursive' log variable.")

  (defvar entropy/emacs-basic--dired-delete-file-refer-dired-buffers nil
    "Dired buffer killed by `entropy/emacs-basic--dired-delete-file-rescursie'
log variable.")
  
  (defun entropy/emacs-basic--dired-redelete-file ()
    "Redeletting file specified by variable
`entropy/emacs-basic--dired-file-current-delete'."
    (interactive)
    (kill-buffer)
    (entropy/emacs-basic-dired-delete-file-recursive entropy/emacs-basic--dired-file-current-delete))

  (defun entropy/emacs-basic--dired-delete-file-prompt (files-list)
    "popup buffer to deleting with prompting and return the
condition state for whether be continuing rest process."
    (if (dired-mark-pop-up " *Deletions*"
                           'delete
                           files-list
                           dired-deletion-confirmer
                           (format "%s %s " "Deleting" (dired-mark-prompt nil files-list)))
        t
      (error "Cancel deleting files!")))

  (defun entropy/emacs-basic-dired-delete-file-recursive (&optional pre-files just-kill-refers)
    "Delete file recursively with refer buffer
cleaned (i.e. files under this dir will cleaned their linked
opened buffer within current emacs session.)

This func was binding to 'D' in `dired-mode-map' which be the
replacement for func `dired-do-delete', this func's advantageous
than it was given the deletion failed handle for responding to
some directory.

Error handle will switching to special buffer ‘*[w32-resmon]*’
buffer with minor mode `entropy/emacs-basic--dired-delete-file-mode' for
prompting for how to resolving deletions problems.

In win32 platform using 'resmon' for conflicates resolve tool.  "
    (interactive)
    (let ((base-files (cond ((and (equal major-mode 'dired-mode)
                                  (not pre-files))
                             (dired-get-marked-files))
                            ((and (not (null pre-files))
                                  (listp pre-files))
                             pre-files)
                            (t (error "Dir list invalid!"))))
          _file-type)

      (unless just-kill-refers
        (entropy/emacs-basic--dired-delete-file-prompt base-files))
      
      (dolist (file base-files)

        ;; killed refer buffers.
        (dolist (el (buffer-list))
          (let* ((buffer-file (buffer-file-name el)))
            (when (and buffer-file
                       (string-match (regexp-quote file) buffer-file))
              (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-files
                           (cons (buffer-name el) (current-time-string)))
              (kill-buffer el))))

        ;; killed refer dired buffers
        (dolist (el dired-buffers)
          (when (string-match (regexp-quote file) (car el))
            (add-to-list 'entropy/emacs-basic--dired-delete-file-refer-dired-buffers
                         (cons el (current-time-string)))
            (kill-buffer (cdr el))))
        
        (condition-case nil
            (when (not just-kill-refers)
              (progn
                (setq entropy/emacs-basic--dired-file-current-delete (list file))
                (cond ((f-symlink-p file)
                       (setq _file-type 'symbol_link)
                       (delete-file file))
                      ((f-file-p file)
                       (setq _file-type 'file)
                       (delete-file file))
                      ((f-directory-p file)
                       (setq _file-type 'directory)
                       (delete-directory file t)))
                (when (equal major-mode 'dired-mode)
                  (revert-buffer))
                (cl-case _file-type
                  ('symbol_link
                   (message (format "Delete symbolink '%s' done! -v-" file)))
                  ('file
                   (message (format "Delete file '%s' done! -v-" file)))
                  ('directory
                   (message (format "Delete directory '%s' done! -v-" file))))))
          (error
           (cond ((eq system-type 'windows-nt)
                  (let ((prompt-buffer (get-buffer-create "*[w32-resmon]*")))
                    (start-process-shell-command
                     "windows resource monitor"
                     prompt-buffer
                     "resmon")
                    (switch-to-buffer prompt-buffer)
                    (goto-char (point-min))
                    (entropy/emacs-basic--dired-delete-file-mode)
                    (insert
                     (format
                      (concat
                       "==============Prompt Info=================\n\n"
                       "Kill all handle refer file:\n %s\n"
                       "with filter with windows resmon!\n\n" 
                       "And try again with answer minibuffer prompts delete again\n\n"
                       "===============Prompt End=================")
                      file))
                    (entropy/emacs-basic--dired-redelete-file)))
                 (t (message "Kill all refer task and try again!"))))))))


  (defun entropy/emacs-basic-dired-delete-file-refers ()
    "Kill all refers of dired markd file of directories."
    (interactive)
    (entropy/emacs-basic-dired-delete-file-recursive nil t))
  
  (entropy/emacs-lazy-load-simple 'dired
    (define-key dired-mode-map (kbd "D") 'entropy/emacs-basic-dired-delete-file-recursive)
    (define-key dired-mode-map (kbd "M-d") 'entropy/emacs-basic-dired-delete-file-refers))
  
  :config
;; **** Set unit of dired inode for human readable
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  
  (if (and (not sys/win32p)
           (not sys/cygwinp))
      ;; because of windows dired list is too long so just let it in linux
      (setq dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

;; **** Always delete and copy resursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  
;; **** Use dired-aux to enable dired-isearch
  (use-package dired-aux :ensure nil)
  
;; **** Quick sort dired buffers via hydra
  ;;; bind key: `S'
  (when (not sys/win32p)
    (use-package dired-quick-sort
      :if (or (executable-find "gls") (executable-find "ls"))
      :commands (dired-quick-sort-setup)
      :init (add-hook 'dired-mode-hook 'dired-quick-sort-setup)))

;; **** Use coloful dired ls
  (defun entropy/emacs-basic--dired-visual-init ()
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
             (dired-rainbow-define log (:inherit default :italic t) ".*\\.log"))
           (when (string= entropy/emacs-dired-visual-type "all-the-icons")
             (warn " Because you are in emacs 25.3.1, just can
using simple dired visual type, although you have seting it to
\"all-the-icons\".")))
          ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
                (not (version= emacs-version "25.3.1"))
                (display-graphic-p))
           (when sys/win32p
             (require 'font-lock+))
           (use-package all-the-icons-dired
             :commands (all-the-icons-dired-mode)
             :hook (dired-mode . all-the-icons-dired-mode)
             :config
             (defun entropy/emacs-basic--all-the-icons-dired-display ()
               "Display the icons of files without colors in a dired buffer."
               ;; Don't display icons after dired commands (e.g insert-subdir, create-directory)
               ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/11
               (all-the-icons-dired--reset)

               (when (and (not all-the-icons-dired-displayed) dired-subdir-alist
                          (not (> (save-excursion (goto-char (point-max)) (line-number-at-pos)) 100)))
                 (setq-local all-the-icons-dired-displayed t)
                 (let ((inhibit-read-only t)
                       (remote-p (and (fboundp 'tramp-tramp-file-p)
                                      (tramp-tramp-file-p default-directory))))
                   (save-excursion
                     ;; TRICK: Use TAB to align icons
                     (setq-local tab-width 1)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (when (dired-move-to-filename nil)
                         (insert " ")
                         (let ((file (dired-get-filename 'verbatim t)))
                           (unless (member file '("." ".."))
                             (let ((filename (file-local-name (dired-get-filename nil t))))
                               (if (file-directory-p filename)
                                   (let ((icon (cond
                                                (remote-p
                                                 (all-the-icons-octicon
                                                  "file-directory"
                                                  :v-adjust all-the-icons-dired-v-adjust
                                                  :face 'all-the-icons-dired-dir-face))
                                                ((file-symlink-p filename)
                                                 (all-the-icons-octicon
                                                  "file-symlink-directory"
                                                  :v-adjust all-the-icons-dired-v-adjust
                                                  :face 'all-the-icons-dired-dir-face))
                                                ((all-the-icons-dir-is-submodule filename)
                                                 (all-the-icons-octicon
                                                  "file-submodule"
                                                  :v-adjust all-the-icons-dired-v-adjust
                                                  :face 'all-the-icons-dired-dir-face))
                                                ((file-exists-p (format "%s/.git" filename))
                                                 (all-the-icons-octicon
                                                  "repo"
                                                  :height 1.1 :v-adjust all-the-icons-dired-v-adjust
                                                  :face 'all-the-icons-dired-dir-face))
                                                (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                                     (apply (car matcher)
                                                            (list (cadr matcher)
                                                                  :face 'all-the-icons-dired-dir-face
                                                                  :v-adjust all-the-icons-dired-v-adjust)))))))
                                     (insert icon))
                                 (insert (all-the-icons-icon-for-file
                                          file
                                          :v-adjust all-the-icons-dired-v-adjust))))
                             (insert "\t"))))
                       (forward-line 1))))))
             (advice-add #'all-the-icons-dired--display :override #'entropy/emacs-basic--all-the-icons-dired-display)))
          ((and (string= entropy/emacs-dired-visual-type "all-the-icons")
                (not (display-graphic-p)))
           (setq entropy/emacs-dired-visual-type "simple-rainbow")
           (warn "You are in terminal emacs session, can not
           enable 'dired-all-the-icons', enable simple-rainbow
           instead now. ")
           (entropy/emacs-basic--dired-visual-init))
          (t (error "entropy/emacs-dired-visual-type invalid"))))
  
  (entropy/emacs-basic--dired-visual-init)
  
;; **** bind 'M-<up>' for dired updir
  (define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)
  
;; **** Improve dired files operation experience for kill opened refer buffers.
  (defun entropy/emacs-basic--kill-redundant-buffer (&rest rest-args)
    "Delete file refer redundant buffer which will cause dired
'delete' or 'rename' failed."
    (dolist (el (mapcar 'buffer-name (buffer-list)))
      (dolist (re-el '("\\*Echo Area"
                       "\\*RNC Input\\*"))
        (when (string-match-p re-el el)
          (kill-buffer el)))))
  (advice-add 'dired-do-rename :before #'entropy/emacs-basic--kill-redundant-buffer)
  (advice-add 'dired-do-flagged-delete :before #'entropy/emacs-basic--kill-redundant-buffer)
  
;; **** get both UNIX and WINDOWS style path string
  (defvar entropy/emacs-basic--get-dired-fpath-log nil)

  (defun entropy/emacs-basic-get-dired-fpath (type)
    (interactive
     (list (completing-read "Choose path string type: "
                            '("unix" "win32"))))
    (let (rtn
          (files_get (or (dired-get-marked-files)
                         (list default-directory))))
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
              entropy/emacs-basic--get-dired-fpath-log rtn)
        (message "Save all path string to log variable 'entropy/emacs-basic--get-dired-fpath-log'.")))))
  
  (define-key dired-mode-map (kbd "0 w") 'entropy/emacs-basic-get-dired-fpath)

;; **** dired add load path
  (defun entropy/emacs-basic--dired-add-to-load-path ()
    (interactive)
    (let ((dir (completing-read "Choose load path adding item: "
                                'read-file-name-internal
                                nil t)))
      (unless (file-directory-p dir)
        (setq dir (file-name-directory dir)))
      (add-to-list 'load-path dir)))

  (define-key dired-mode-map (kbd "M-l") 'entropy/emacs-basic--dired-add-to-load-path)

;; **** dired backup file

  (defun entropy/emacs-basic-backup-files ()
    (interactive)
    (when (not (fboundp 'entropy/cl-backup-file))
      (require 'entropy-common-library))
    (let ((files (dired-get-marked-files)))
      (dolist (el files)
        (when (file-exists-p el)
          (entropy/cl-backup-file el)))
      (revert-buffer)))
  (define-key dired-mode-map (kbd "B") #'entropy/emacs-basic-backup-files)

;; **** dired auto revert after some operations

  (defun entropy/emacs-basic--dired-revert-advice (&rest _)
    (revert-buffer))
  (dolist (el '(dired-do-rename
                dired-do-rename-regexp
                dired-do-copy
                dired-do-copy-regexp
                dired-do-compress
                dired-do-compress-to
                dired-do-touch))
    (advice-add el :after #'entropy/emacs-basic--dired-revert-advice)))

;; *** dired-x
(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-size-limit nil)
  (setq dired-omit-extensions nil))
  
;; ** Image-mode
(use-package image-mode
  :ensure nil
  :config
  (defun entropy/emacs-basic-image-gif-warn (&optional args)
    "Warn that gif animation by large gif file will freeze
emacs."
    (if (string-match-p "\\.gif" (buffer-name))
        (if (not (y-or-n-p "Do you want to animated it? "))
            (error "Please open it with external apps!"))))
  (advice-add 'image-toggle-animation :before #'entropy/emacs-basic-image-gif-warn))

;; ** Set transparenct of emacs frame
(global-set-key (kbd "<f6>") 'entropy/emacs-basic-loop-alpha)

(defun entropy/emacs-basic-loop-alpha ()    
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
    (entropy/emacs-basic-loop-alpha))

;; ** Paragraph fill size
(setq-default fill-column entropy/emacs-fill-paragraph-width)

;; ** Show time on mode line
(when entropy/emacs-display-time-modeline
  ;; enable time show when  
  (display-time-mode 1)
  (setq-default display-time-interval 1)
  ;; display time with date and real time infos
  (setq display-time-day-and-date t)
  ;; 24hr format
  (setq display-time-24hr-format t)
  (setq display-time-format "%e %b %Y %H:%M:%S")
  (display-time))

;; ** Input time into buffer
(defun entropy/emacs-basic-now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))


(defun entropy/emacs-basic-today ()
  "Insert string for today's date nicely formatted in American style,
 e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; ** Read-Only-About
(use-package entropy-global-read-only-mode
  :ensure nil
  :commands (entropy-grom-mode)
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file push-button)
   "entropy-grom"
   "entropy-grom"
   (entropy-grom-mode +1)))

;; ** Revert buffer automatically

(entropy/emacs-lazy-initial-for-hook
 '(find-file-hook)
 "GlbAutoRevertMode"
 "GlbAutoRevertMode-enabled"
 (global-auto-revert-mode +1))

;; ** Use popup window framework
;; *** popwin-mode
(use-package popwin
  :if (eq entropy/emacs-use-popup-window-framework 'popwin)
  :commands popwin-mode
  :init (add-hook (entropy/emacs-select-x-hook) #'popwin-mode)
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
          ("\\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
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
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Entropy refer
          ("^\\*entropy/cpmv" :regexp t :position bottom :stick nil :noselect nil)
          ("^\\*entropy/cndt" :regexp t :position bottom :stick nil :noselect nil)
          ("^\\*entropy/sdcv" :regexp t :position bottom :stick nil :noselect nil))))

;; *** shackle mode
(use-package shackle
  :if (eq entropy/emacs-use-popup-window-framework 'shackle)
  :commands (shackle-mode
             shackle-display-buffer
             shackle-popup-buffer
             shackle-popup-find-file)
  :init
  (defvar shackle-popup-mode-map
    (let ((map (make-sparse-keymap)))
      map))
  (global-set-key (kbd "C-3") shackle-popup-mode-map)
  (add-hook (entropy/emacs-select-x-hook) #'shackle-mode)
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)
  :bind
  (:map shackle-popup-mode-map
   ("o" . shackle-popup-buffer)
   ("f" . shackle-popup-find-file)
   ("e" . shackle-popup-message))
  :config
  (eval-and-compile
    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))
    (bind-key "C-h z" #'shackle-last-popup-buffer)

    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list)
              (recenter-top-bottom '(middle)))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  (defun shackle-popup-buffer ()
    (interactive)
    (let* ((buff-name (completing-read "Buffer choosing:" 'internal-complete-buffer))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (get-buffer-create buff-name)
      (display-buffer buff-name)
      (when (and (fboundp 'solaire-mode)
                 (entropy/emacs-theme-adapted-to-solaire))
        (with-current-buffer buff-name
          (solaire-mode +1)))))

  (defun shackle-popup-find-file ()
    (interactive)
    (let* ((file (completing-read "Buffer choosing:" 'read-file-name-internal))
           (buff-name (buffer-name (find-file-noselect file)))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (display-buffer buff-name)))

  (defun shackle-popup-message ()
    (interactive)
    (let* ((buff-name (buffer-name (get-buffer-create "*Messages*")))
           (shackle-rules `((,buff-name :select t :align 'below :autoclose t))))
      (with-current-buffer buff-name
        (unless (eobp)
          (goto-char (point-max))))
      (display-buffer buff-name)))
  
  ;; rules
  (setq shackle-default-size 0.6)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (setq shackle-rules
        '(("*Help*" :select t :align below :autoclose t)
          ("*compilation*" :align below :autoclose t)
          ("*Completions*" :align below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align below :autoclose t)
          ("*ert*" :align below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align below)
          ("*Warnings*" :align below :autoclose t)
          ("*Messages*" :align below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :align below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align below :autoclose t)
          ("*Calendar*" :select t :align below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align below)
          (" *undo-tree*" :select t :align right)
          ("*Paradox Report*" :align below :autoclose t)
          ("*quickrun*" :select t :size 15 :align below)
          ("*tldr*" :align below :autoclose t)
          ("*Youdao Dictionary*" :align below :autoclose t)
          ("*Google Translate*" :align below :select t :size 0.5 :autoclose t)
          ("*Finder*" :select t :align below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align below :autoclose t)
          ("*lsp-help*" :align below :autoclose t)
          ("*lsp session*" :size 0.4 :align below :autoclose t)
          (" *Org todo*" :select t :size 4 :align below :autoclose t)
          ("*Org Dashboard*" :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/cpmv" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/cndt" :regexp t :select t :size 0.4 :align below :autoclose t)
          ("^\\*entropy/sdcv" :regexp t :select t :size 0.4 :align below :autoclose t)

          (ag-mode :select t :align below)
          (grep-mode :select t :align below)
          (pt-mode :select t :align below)
          (rg-mode :select t :align below)

          (flycheck-error-list-mode :select t :align below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :align below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align below :autoclose t)
          (comint-mode :align below)
          (helpful-mode :select t :size 0.4 :align below :autoclose t)
          (process-menu-mode :select t :align below :autoclose t)
          (list-environment-mode :select t :align below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align below)
          (tabulated-list-mode :align below))))

;; ** Use which-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (add-hook (entropy/emacs-select-x-hook) #'which-key-mode)
  (setq which-key-popup-type 'minibuffer))

;; ** Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :bind (("C-x u" . entropy/emacs-basic-undo-tree))
  :init
  (entropy/emacs-lazy-load-simple 'undo-tree
    (global-undo-tree-mode +1)
    (add-hook 'undo-tree-mode-hook
              '(lambda () (define-key undo-tree-map (kbd "C-x u") nil))))
  :config
  (defun entropy/emacs-basic-undo-tree ()
    (interactive)
    (if (car (window-margins))
        (progn
          (setq entropy/emacs-basic-undo-tree-margin-detective t)
          (entropy/emacs-basic-center-text-clear)
          (undo-tree-visualize))
      (progn
        (setq entropy/emacs-basic-undo-tree-margin-detective nil)
        (undo-tree-visualize))))

  (defun undo-tree-visualizer-quit ()
    "Quit the undo-tree visualizer. 

Note:

This function has redefined for adapting to
`entropy/emacs-basic-center-text'."
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
            (if entropy/emacs-basic-undo-tree-margin-detective
                (progn
                  (switch-to-buffer-other-window parent)
                  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                                      (/ (window-width) entropy/emacs-window-center-integer)
                                      (/ (window-width) entropy/emacs-window-center-integer)))
              (switch-to-buffer parent)))))))

;; ** Auto-sudoedit
(use-package auto-sudoedit
  :commands (auto-sudoedit-mode)
  :if (and (not sys/win32p)
           (not sys/cygwinp))
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(find-file dired)
   "autosudoedit"
   "autosudoedit"
   (auto-sudoedit-mode +1)))

;; ** Clear killring
;;     From the forum of stackexchange
;;     `https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs'
;;     Or you just can use (setq kill-ring nil) only.
(defun entropy/emacs-basic-clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

;; ** mark-sexp
(global-set-key (kbd "C-`") 'set-mark-command)
(defun entropy/emacs-basic-mark-set ()
  (interactive)
  (save-excursion
    (push-mark)
    (push-mark)))
(global-set-key (kbd "C-2") 'entropy/emacs-basic-mark-set)

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
  (add-hook 'find-file-hook '(lambda () (unless recentf-mode
					  (recentf-mode)
					  (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :ensure nil
  :init (add-hook (entropy/emacs-select-x-hook) #'savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60))

;; ** Bookmarks autosave
(setq bookmark-save-flag 1)

;; *** Bookmark utf-8
(when entropy/emacs-custom-language-environment-enable
  (dolist (hook '(bookmark-edit-annotation-mode-hook
                  bookmark-bmenu-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (unless (string= current-language-environment "UTF-8")
                    (entropy/emacs-lang-set-utf-8)))))

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
the list of bookmarks.)

Note: This function has been redefined for forcing using utf-8
coding-system to save bookmark infos"
      (interactive (list nil current-prefix-arg))
      (unless (string= current-language-environment "UTF-8")
        (entropy/emacs-lang-set-utf-8))
      (let ((prompt
             (if no-overwrite "Set bookmark" "Set bookmark unconditionally")))
        (bookmark-set-internal prompt name (if no-overwrite 'push 'overwrite))))))
                  
;; ** Major mode reload
(defun entropy/emacs-basic-major-mode-reload ()
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
(global-set-key (kbd "<f7>") 'entropy/emacs-basic-major-mode-reload)

;; ** Disable-mouse-wheel and more
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :commands (global-disable-mouse-mode)
  :init
  (add-hook (entropy/emacs-select-x-hook) #'global-disable-mouse-mode))

;; ** Artist-mode
(use-package artist
  :ensure nil
  :init
  ;; Init disable rubber-banding for reducing performance requirements.
  (add-hook 'artist-mode-hook
            #'(lambda () 
                (if artist-rubber-banding
                    (setq-local artist-rubber-banding nil)))))

(defun entropy/emacs-basic-ex-toggle-artist-and-text ()
  (interactive)
  "Toggle mode between `text-mode' & `artist-mode'."
  (cond ((eq major-mode 'picture-mode)
         (text-mode))
        ((eq major-mode 'text-mode)
         (yes-or-no-p "Really for that? (maybe you don't want to change to artist!) ")
         (artist-mode))))
(entropy/emacs-lazy-load-simple 'artist
  (define-key artist-mode-map (kbd "<f5>") 'entropy/emacs-basic-ex-toggle-artist-and-text))
(entropy/emacs-lazy-load-simple 'text-mode
  (define-key text-mode-map (kbd "<f5>") 'entropy/emacs-basic-ex-toggle-artist-and-text))

;; Disabled '<' and '>' keybinding function.
(entropy/emacs-lazy-load-simple 'artist
  (define-key artist-mode-map (kbd ">") nil)
  (define-key artist-mode-map (kbd "<") nil))

(defun entropy/emacs-basic-artist-mode ()
  "Open one temp-file with artist-mode.
Temp file was \"~/~entropy-artist.txt\""
  (interactive)
  (find-file "~/~entropy-artist.txt")
  (artist-mode))

;; ** Disable auto-window-vscroll -----> the resean from
;; `https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag'
(setq auto-window-vscroll nil)

;; ** Use chinese pyim
(use-package pyim
  :if entropy/emacs-enable-pyim
  :diminish chinese-pyim-mode
  :commands (pyim-restart-1
             pyim-start
             entropy/emacs-basic-pyim-toggle
             entropy/emacs-basic-toggle-pyim-s2t
             entropy/emacs-basic-toggle-pyim-punctuation-half-or-full
             pyim-convert-string-at-point)
  :bind
  (("M-j" . pyim-convert-string-at-point))
  :init

;; *** pyim user dictionaries specification
  (cond ((and (eq entropy/emacs-pyim-use-backend 'internal)
              (not entropy/emacs-pyim-dicts))
         (use-package pyim-basedict
           :ensure nil
           :config (pyim-basedict-enable)))
        ((eq entropy/emacs-pyim-use-backend 'internal)
         (setq pyim-dicts entropy/emacs-pyim-dicts))
        ((eq entropy/emacs-pyim-use-backend 'liberime)
         (use-package liberime-config
           :ensure nil
           :commands (liberime-load)
           :init (liberime-load))
         (liberime-start (expand-file-name entropy/emacs-pyim-liberime-scheme-data)
                         (expand-file-name entropy/emacs-pyim-liberime-cache-dir))
         (liberime-select-schema "luna_pinyin_simp")))
  
;; *** Setting pyim as the default input method
  (setq default-input-method "pyim")

;; *** pyim backend chosen
  (cl-case entropy/emacs-pyim-use-backend
    (internal (setq pyim-default-scheme 'quanpin))
    (liberime (setq pyim-default-scheme 'rime-quanpin)))
  
;; *** use popup or posframe for pyim tooltip show

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

;; *** 5 candidates shown for pyim tooltip
  (setq pyim-page-length 8)

;; *** config
  :config
;; **** toggle input method
  (defun entropy/emacs-basic-pyim-toggle ()
    (interactive)
    (if (string= current-input-method "pyim")
        (set-input-method "rfc1345")
      (progn
        (set-input-method "pyim")
        (setq pyim-punctuation-escape-list nil))))
  (global-set-key (kbd "C-\\") 'entropy/emacs-basic-pyim-toggle)
  
;; **** using 'C-g' to cancling any pyim manipulation
  (if (not (version< emacs-version "26"))
      (define-key pyim-mode-map (kbd "C-g") 'pyim-quit-clear))
  
;; **** s2t&t2s convertor
  (use-package entropy-s2t
    :ensure nil
    :commands entropy/s2t-string)
  (defun entropy/emacs-basic-toggle-pyim-s2t ()
    (interactive)
    (if pyim-magic-converter
        (setq pyim-magic-converter nil)
      (setq pyim-magic-converter 'entropy/s2t-string)))
  (global-set-key (kbd "C-M-\\") 'entropy/emacs-basic-toggle-pyim-s2t)

;; **** toglle punctuation between half and full way.
  (defun entropy/emacs-basic-toggle-pyim-punctuation-half-or-full ()
    (interactive)
    (if (eq (car pyim-punctuation-translate-p) 'no)
        (setq pyim-punctuation-translate-p '(yes no auto))
      (setq pyim-punctuation-translate-p '(no yes auto))))
  (global-set-key (kbd "C-1") 'entropy/emacs-basic-toggle-pyim-punctuation-half-or-full))

;; **** If didn't use pyim set input method to nil
(unless entropy/emacs-enable-pyim
  (setq default-input-method nil))


;; ** Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; ** 'super' and 'hyper' key
(progn
  ;; Binding 'super' and 'hyper' on win32 and mac.
  ;;   the idea form `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'
  (cond
   (sys/win32p
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


;; ** Adding advice for `y-or-n-p' for emacs 26 and higher in widnows plattform
(when (and sys/win32p (not (version< emacs-version "26.1")))
  (defun entropy/emacs-basic-y-or-n-p (prompt)
    (let ((judge (completing-read prompt '("yes" "no") nil t)))
      (if (string= judge "yes")
          t
        nil)))
  
  ;; adding advice ro y-or-n-p for temporarily fix bug of that can not
  ;; using any key-bindings when active "C-<lwindow>-g" in windows
  (advice-add 'y-or-n-p :override #'entropy/emacs-basic-y-or-n-p))

;; ** Epa (emacs gpg assistant)
(use-package epa
  :ensure nil
  :init
  (epa-file-enable)
  (when (and entropy/emacs-wsl-enable
             (file-exists-p entropy/emacs-wsl-apps))
    (entropy/emacs-lazy-load-simple 'custom
      (custom-set-variables
       '(epg-gpg-program (expand-file-name "gpg.exe" entropy/emacs-wsl-apps))
       '(epg-gpgconf-program (expand-file-name "gpgconf.exe" entropy/emacs-wsl-apps))
       '(epg-gpgsm-program (expand-file-name "gpgsm.exe" entropy/emacs-wsl-apps))))))

;; ** Emacs process and system proced manager hacking
;; *** process
(global-set-key (kbd "C-c s s") 'list-processes)

;; *** proced
(use-package proced
  :ensure nil
  :commands (proced-process-attributes)
  :init
  (setq-default proced-format 'medium)

  (defun entropy/emacs-basic-proced-processP (processName)
    "Return one alist collected the proced info of procssName,
otherwise returns nil."
    (let ((procedList (proced-process-attributes))
          rtn)
      (dolist (el procedList)
        (when (equal (cdr (assq 'comm el))
                     processName)
          (push el rtn)))
      rtn))

  (defun entropy/emacs-basic-proced-auto-startwith (processName $executable)
    (unless (entropy/emacs-basic-proced-processP processName)
      (cl-case system-type
        (windows-nt
         (when (fboundp 'w32-shell-execute)
           (let ((default-directory temporary-file-directory))
             (w32-shell-execute
              "open" $executable)
             (message (format "Start with '%s'."
                              $executable)))))
        (t (message
            "`entropy/emacs-basic-proced-auto-startwith' are just used in w32 platform")))))

  (dolist (el entropy/emacs-startwith-apps)
    (when (executable-find (cdr el))
      (entropy/emacs-basic-proced-auto-startwith (car el) (cdr el)))))

;; ** Improve captialize function

;; Due to the convention while want to capitalize or uper-case the word just has been done, building
;; follow two function to enhance the origin function `capitalize-word' and `upercase-word'.
(defun entropy/emacs-basic-capitalize-word (arg)
  "Automatically go ahead of previous word before call `capitalize-word'."
  (interactive "P")
  (left-word)
  (call-interactively 'capitalize-word t (vector arg)))

(defun entropy/emacs-basic-upcase-word (arg)
  "automatically go ahead of previous word before call `upcase-word'."
  (interactive "P")
  (left-word)
  (call-interactively 'upcase-word t (vector arg)))

(global-set-key (kbd "M-c") 'entropy/emacs-basic-capitalize-word)
(global-set-key (kbd "M-u") 'entropy/emacs-basic-upcase-word)

;; ** autocompression moode

;; Force refresh autocompression mode enabling status as that the
;; initialization for its refers procedure can not cover fully
;; functional of `auto-compression-mode'.
(entropy/emacs-lazy-initial-advice-before
 '(push-button load-library find-library)
 "autocompression-mode"
 "autocompression-mode"
 (auto-compression-mode 0)
 (auto-compression-mode 1))

;; * provide
(provide 'entropy-emacs-basic)
