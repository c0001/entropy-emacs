;;; entropy-emacs-wc.el --- entropy emacs window configuration
;;
;; * Copyright (C) 20190821 Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-wc.el
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; Emacs window referred configuration sepcified for =entropy-emacs=.
;;
;; This configuration aimed to provide batch of window operation
;; optimized and injecting the window group for work-around.
;;
;; * Configuration:
;;
;; Designed for =entropy-emacs= only without inidividually using
;; warranty.
;;
;; Sets of functions used as library came from other designation of
;; =entropy-emacs=, thus correctly extracting theme from that was
;; necessary for hacking.
;; 
;; * Code:

;; ** Window switch
;; *** window numberic indicator
(use-package window-number
  :commands (window-number-switch
             window-number-mode)
  :bind
  ("C-x o" . window-number-switch)
  :init
  (cond (entropy/emacs-fall-love-with-pdumper
         (add-hook 'entropy/emacs-pdumper-load-hook
                   #'window-number-mode))
        (t
         (entropy/emacs-lazy-load-simple 'window-number
           (window-number-mode +1)))))

;; *** Use windmove function stolen :) from `https://github.com/troydm/emacs-stuff/blob/master/windcycle.el'
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

;; **** Disable buffer reverse and turn by =C-x C-left= =C-x C-right=
(global-set-key (kbd "C-x C-<left>") nil)
(global-set-key (kbd "C-x C-<right>") nil)

;; ** window config
;; *** eyebrowse ----> for save the window config(workspace group)
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
  
  :bind (("C-c v" . entropy/emacs-basic-eyebrowse-create-derived)
         ("C-c M-v" . entropy/emacs-basic-eyebrowse-switch-derived))
  :init

  (entropy/emacs-lazy-with-load-trail
   eyebrowse-enable
   (eyebrowse-mode +1)

   (setq eyebrowse-mode-map
     (let ((map (current-global-map))
           (prefix-map (make-sparse-keymap)))
       (define-key prefix-map (kbd "<") 'eyebrowse-prev-window-config)
       (define-key prefix-map (kbd ">") 'eyebrowse-next-window-config)
       (define-key prefix-map (kbd "'") 'eyebrowse-last-window-config)
       (define-key prefix-map (kbd "\"") 'eyebrowse-close-window-config)
       (define-key prefix-map (kbd ",") 'eyebrowse-rename-window-config)
       (define-key prefix-map (kbd ".") 'eyebrowse-switch-to-window-config)
       (define-key prefix-map (kbd "0") 'eyebrowse-switch-to-window-config-0)
       (define-key prefix-map (kbd "1") 'eyebrowse-switch-to-window-config-1)
       (define-key prefix-map (kbd "2") 'eyebrowse-switch-to-window-config-2)
       (define-key prefix-map (kbd "3") 'eyebrowse-switch-to-window-config-3)
       (define-key prefix-map (kbd "4") 'eyebrowse-switch-to-window-config-4)
       (define-key prefix-map (kbd "5") 'eyebrowse-switch-to-window-config-5)
       (define-key prefix-map (kbd "6") 'eyebrowse-switch-to-window-config-6)
       (define-key prefix-map (kbd "7") 'eyebrowse-switch-to-window-config-7)
       (define-key prefix-map (kbd "8") 'eyebrowse-switch-to-window-config-8)
       (define-key prefix-map (kbd "9") 'eyebrowse-switch-to-window-config-9)
       (define-key prefix-map (kbd "c") 'eyebrowse-create-window-config)
       (define-key prefix-map (kbd "C-c") 'eyebrowse-create-window-config)
       (define-key map eyebrowse-keymap-prefix prefix-map)))

   (global-set-key (kbd "C-c C-w") eyebrowse-mode-map)
   
   (dolist (bind '(("C-e" . entropy/emacs-basic-eyebrowse-create-workspaces)
                   ("M-e" . entropy/emacs-basic-eyebrowse-delete-workspace)
                   ("C-o" . entropy/emacs-basic-eyebrowse-switch-top)
                   ("C-c" . entropy/emacs-basic-eyebrowse-create-window-config)
                   ("c" . entropy/emacs-basic-eyebrowse-create-window-config)
                   ("." . entropy/emacs-basic-eyebrowse-switch-basic-window)
                   ("a" . eyebrowse-switch-to-window-config)))
     (define-key eyebrowse-mode-map
       (kbd (car bind)) (cdr bind))))

  :config
  (setq eyebrowse-mode-line-style nil
        eyebrowse-new-workspace
        (if (functionp entropy/emacs-enable-eyebrowse-new-workspace-init-function)
            entropy/emacs-enable-eyebrowse-new-workspace-init-function
          t))
  
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
                                     'face 'entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_body)
                         (propertize (if (and (not (equal top-tag ""))
                                              (not (equal top-tag nil)))
                                         (format "[%s]: %s " top-slot top-tag)
                                       (format "[%s] " top-slot))
                                     'face 'entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_content)
                         (propertize "." 'face 'entropy/emacs-defface-face-for-basic-eyebrowse-back-top-wg-message-face_body))))
       (t (error "You've at top wg!"))))))

;; *** winner mode for recover previous window config faster
(use-package winner
  :ensure nil
  :commands (winner-mode)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
  :init

  (cond
   (entropy/emacs-fall-love-with-pdumper
    (add-hook 'entropy/emacs-pdumper-load-hook
              #'winner-mode))
   (t
    (entropy/emacs-lazy-initial-advice-before
     '(switch-to-buffer find-file delete-other-windows)
     "winner-mode" "winner-mode"
     (winner-mode +1))))
  
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"
          "*Backtrace*")
        winner-boring-buffers-regexp
        (rx (or (seq line-start "magit: ")))))

;; *** desktop mode

(use-package desktop
  :if entropy/emacs-desktop-enable
  :ensure nil
  :commands (desktop-save-mode)
  :init
  (cond
   (entropy/emacs-fall-love-with-pdumper
    (add-hook 'entropy/emacs-pdumper-load-hook
              #'desktop-save-mode))
   (t (desktop-save-mode +1)))
  
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
    (setq desktop-restore-frames nil)))

;; ** Buffer window size setting
(use-package windresize
  :commands (windresize)
  :bind
  ("C-<f10>" . windresize))

;; ** Exchange window
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

;; ** Centered-window
;; *** Manully method
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


;; ** Window divider

(entropy/emacs-lazy-with-load-trail
 win-divider
 (window-divider-mode t))

;; * provide
(provide 'entropy-emacs-wc)
