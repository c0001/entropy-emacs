;;; entropy-emacs-wc.el --- entropy emacs window configuration  -*- lexical-binding: t; -*-
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

;; ** require

(entropy/emacs-lazy-initial-for-hook
 (window-configuration-change-hook)
 "rich-window-config-hydra-hollow-top-init"
 "rich-window-config-hydra-hollow-top-init" prompt-echo
 :pdumper-no-end t
 (let ((ind-hydra-name 'eemacs-window-config))
   (entropy/emacs-hydra-hollow-category-common-individual-define
    ind-hydra-name
    (entropy/emacs-hydra-hollow-category-common-individual-make-title-common
     ind-hydra-name)
    '("Move Window" nil
      "Jump To Window" nil
      "Align Buffer" nil))
   (entropy/emacs-hydra-hollow-add-for-top-dispatch
    `("WI&BUF"
      (("M-w"
        (:eval
         (entropy/emacs-hydra-hollow-category-common-individual-get-caller
          ',ind-hydra-name))
        "Rich command for (window buffer) Dwim"
        :enable t :exit t))))))

;; ** Window switch
;; *** window numberic indicator
(use-package ace-window
  :commands
  (ace-delete-other-windows
   ace-delete-window
   ace-select-window
   ace-swap-window
   ace-window-display-mode
   ace-window)
  :eemacs-tpha
  (((:enable
     t
     :defer
     (:data (:adfors
             (window-configuration-change-hook)
             :adtype hook
             :pdumper-no-end t))))
   ("WI&BUF"
    (("C-x M-o" ace-window "Switch to Another Window"
      :enable t
      :exit t
      :global-bind t))))
  :init
  ;; (entropy/emacs-lazy-with-load-trail
  ;;  ace-window-init
  ;;  ;; NOTE: (FIXME) this mode will make each frame created be visible
  ;;  ;; by `aw--after-make-frame', its may be a compatible problem.
  ;;  (ace-window-display-mode +1))
  )

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


(entropy/emacs-lazy-initial-for-hook
 (window-configuration-change-hook)
 "window-jump-extra-hydra-hollow-init"
 "window-jump-extra-hydra-hollow-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define+
  'eemacs-window-config nil
  '("Jump To Window"
    (("C-x <up>" entropy/emacs-basic-windmove-up-cycle
      "Move To Up Window"
      :enable t
      :exit t
      :global-bind t)
     ("C-x <down>" entropy/emacs-basic-windmove-down-cycle
      "Move To Below Window"
      :enable t
      :exit t
      :global-bind t)
     ("C-x <right>" entropy/emacs-basic-windmove-right-cycle
      "Move To Right Window"
      :enable t
      :exit t
      :global-bind t)
     ("C-x <left>" entropy/emacs-basic-windmove-left-cycle
      "Move To Left Window"
      :enable t
      :exit t
      :global-bind t)
     ))))

;; **** Disable buffer reverse and turn by =C-x C-left= =C-x C-right=
(global-set-key (kbd "C-x C-<left>") nil)
(global-set-key (kbd "C-x C-<right>") nil)

;; ** window config
;; *** eyebrowse ----> for save the window config(workspace group)
(use-package eyebrowse
;; **** commands
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

;; **** preface
  :preface

  (setq entropy/emacs-wc-eyebrowse-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "<") 'eyebrowse-prev-window-config)
          (define-key map (kbd ">") 'eyebrowse-next-window-config)
          (define-key map (kbd "'") 'eyebrowse-last-window-config)
          (define-key map (kbd "\"") 'eyebrowse-close-window-config)
          (define-key map (kbd ",") 'eyebrowse-rename-window-config)
          (define-key map (kbd ".") 'eyebrowse-switch-to-window-config)
          (define-key map (kbd "0") 'eyebrowse-switch-to-window-config-0)
          (define-key map (kbd "1") 'eyebrowse-switch-to-window-config-1)
          (define-key map (kbd "2") 'eyebrowse-switch-to-window-config-2)
          (define-key map (kbd "3") 'eyebrowse-switch-to-window-config-3)
          (define-key map (kbd "4") 'eyebrowse-switch-to-window-config-4)
          (define-key map (kbd "5") 'eyebrowse-switch-to-window-config-5)
          (define-key map (kbd "6") 'eyebrowse-switch-to-window-config-6)
          (define-key map (kbd "7") 'eyebrowse-switch-to-window-config-7)
          (define-key map (kbd "8") 'eyebrowse-switch-to-window-config-8)
          (define-key map (kbd "9") 'eyebrowse-switch-to-window-config-9)
          (define-key map (kbd "c") 'eyebrowse-create-window-config)
          map))

  (dolist (bind '(("c" . entropy/emacs-basic-eyebrowse-create-window-config)
                  ("a" . eyebrowse-switch-to-window-config)))
    (define-key entropy/emacs-wc-eyebrowse-mode-map
      (kbd (car bind)) (cdr bind)))

;; **** eemacs-tpha
  :eemacs-tpha
  (((:enable t :defer t))
   ("WI&BUF"
    (("W"
      (:pretty-hydra-cabinet
       (:data
        "Eyebrowse Keymap"
        (("C-c C-w"
          (:eval
           (entropy/emacs-hydra-hollow-category-common-individual-get-caller
            'eyebrowse-mode))
          "Eyerbowse key Map bind of <\C-c \C-w>"
          :enable t
          :exit t
          :eemacs-top-bind t)
         ("C-c w"
          (:eval
           (entropy/emacs-hydra-hollow-category-common-individual-get-caller
            'eyebrowse-mode))
          "Eyerbowse key Map bind of <\C-cw>"
          :enable t
          :exit t
          :eemacs-top-bind t))))
      "Eyebrowe Dispatch Keymap"
      :enable t
      :exit t))))

;; **** eemacs-indhc
  :eemacs-indhc
  (((:enable t :defer t)
    (eyebrowse-mode
     (eyebrowse entropy/emacs-wc-eyebrowse-mode-map)
     nil
     ((3 :width-desc "Common switch && A/R workspace && Derived workspace manipulation")
      (1 :width-desc "Quick briefly numeric switch keys"))))
   ("Common Switch"
    (("C-o" entropy/emacs-basic-eyebrowse-switch-top
      "Switch to Workspace Top"
      :enable t
      :exit t
      :map-inject t)
     ("."   eyebrowse-switch-to-window-config
      "Choose Work-Space And Jump into"
      :enable t
      :exit t
      :map-inject t)
     (","   eyebrowse-rename-window-config
      "Rename or add workspace tag"
      :enable t
      :exit t
      :map-inject t))

    "Create Or Delete Workspace"
    (("C-e" entropy/emacs-basic-eyebrowse-create-workspaces
      "Batch create workspace"
      :enable t
      :exit t
      :map-inject t)
     ("M-e" entropy/emacs-basic-eyebrowse-delete-workspace
      "Delete workspace"
      :enable t
      :exit t
      :map-inject t)
     ("C-c c" entropy/emacs-basic-eyebrowse-create-window-config
      "Create One Work-Space "
      :enable t
      :exit t
      :map-inject t))

    "Derived Workspace"
    (("C-c v" entropy/emacs-basic-eyebrowse-create-derived
      "Create Derived Work-Space"
      :enable t
      :exit t
      :global-bind t)
     ("C-c M-v" entropy/emacs-basic-eyebrowse-switch-derived
      "Switch To Derived Work-Space"
      :enable t
      :exit t
      :global-bind t))

    "Digital switch"
    (("0" eyebrowse-switch-to-window-config-0
      "Switch to Work Space 0"
      :enable t
      :exit t
      :map-inject t)
     ("1" eyebrowse-switch-to-window-config-1
      "Switch to Work Space 1"
      :enable t
      :exit t
      :map-inject t)
     ("2" eyebrowse-switch-to-window-config-2
      "Switch to Work Space 2"
      :enable t
      :exit t
      :map-inject t)
     ("3" eyebrowse-switch-to-window-config-3
      "Switch to Work Space 3"
      :enable t
      :exit t
      :map-inject t)
     ("4" eyebrowse-switch-to-window-config-4
      "Switch to Work Space 4"
      :enable t
      :exit t
      :map-inject t)
     ("5" eyebrowse-switch-to-window-config-5
      "Switch to Work Space 5"
      :enable t
      :exit t
      :map-inject t)
     ("6" eyebrowse-switch-to-window-config-6
      "Switch to Work Space 6"
      :enable t
      :exit t
      :map-inject t)
     ("7" eyebrowse-switch-to-window-config-7
      "Switch to Work Space 7"
      :enable t
      :exit t
      :map-inject t)
     ("8" eyebrowse-switch-to-window-config-8
      "Switch to Work Space 8"
      :enable t
      :exit t
      :map-inject t)
     ("9" eyebrowse-switch-to-window-config-9
      "Switch to Work Space 9"
      :enable t
      :exit t
      :map-inject t))))

;; **** init
  :init

  (setq eyebrowse-keymap-prefix (kbd "s-w"))

  (entropy/emacs-lazy-with-load-trail
   eyebrowse-enable
   (eyebrowse-mode +1))

  (add-hook 'eyebrowse-post-window-switch-hook
            #'entropy/emacs-wc-window-auto-center-mode-enable-for-all-displayed-windows)

;; **** config
  :config
  (setq eyebrowse-mode-line-style nil
        eyebrowse-new-workspace
        (if (and entropy/emacs-enable-eyebrowse-new-workspace-init-function
                 (functionp entropy/emacs-eyebrowse-new-workspace-init-function))
            entropy/emacs-eyebrowse-new-workspace-init-function
          t))

;; ***** window parameter memory
  (defun entropy/emacs-wc--eyebrowse-regist-wpamemory ()
    (let ((cur-eslot (when (bound-and-true-p eyebrowse-mode)
                       (eyebrowse--get 'current-slot))))
      (entropy/emacs-wpamemory-regist-memory
       'eyebrowse
       `(EEMACS-DT-IDENTITY
         .
         (:eyebrowse-slot . ,cur-eslot)))))

  (defun entropy/emacs-wc--eyebrowse-resotre-wpamemory ()
    (let ((cur-eslot (when (bound-and-true-p eyebrowse-mode)
                       (eyebrowse--get 'current-slot))))
      (entropy/emacs-wpamemory-restore-memory
       'eyebrowse
       `(EEMACS-DT-IDENTITY
         .
         (:eyebrowse-slot . ,cur-eslot)))))

  (add-hook 'eyebrowse-pre-window-switch-hook #'entropy/emacs-wc--eyebrowse-regist-wpamemory)
  (add-hook 'eyebrowse-post-window-switch-hook #'entropy/emacs-wc--eyebrowse-resotre-wpamemory)

;; ***** Debugs for improving eyebrowse's user experience
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

;; ***** Create window config
  (defun entropy/emacs-basic-eyebrowse-create-window-config ()
    "Creates a window config at a yet unoccupied slot and named
    this work space."
    (interactive)
    (funcall #'eyebrowse-create-window-config)
    (let ((slot (eyebrowse--get 'current-slot))
          (tag (read-string "Tag: ")))
      (apply #'eyebrowse-rename-window-config `(,slot ,tag))))

;; ***** Show slot information
  (defun entropy/emacs-basic-eyebrowse-show-current-slot ()
    "Show current eyebrowse workspace slot and tag info."
    (interactive)
    (let* ((entropy/emacs-basic--eyebrowse-slot-result (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (window-config (assoc (eyebrowse--get 'current-slot) window-configs))
           (current-tag (nth 2 window-config)))
      (message "Slot:%s  Tag:%s" entropy/emacs-basic--eyebrowse-slot-result current-tag)))

;; ***** kill all eyebrowse window configs
  (defun entropy/emacs-basic-eyebrowse-kill-all-group ()
    "Kill all eyebrowse window config"
    (interactive)
    (dolist (item (eyebrowse--get 'window-configs))
      (eyebrowse--delete-window-config (car item)))
    (eyebrowse-init))

;; ***** Batch create eyerbrowse window configs
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
    (entropy/emacs-basic-eyebrowse-kill-all-group)
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

;; ***** Batch remove eyebrowse window configs
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
      (setq candi (reverse candi))
      (setq candin (mapcar 'car candi))
      (ivy-read "Delete worksapce (%d/%d): " candin
                :require-match t
                :action 'entropy/emacs-basic--eyebrowse-read-config-repeated)
      (dolist (el entropy/emacs-basic--eyebrowse-config-selected)
        (eyebrowse--delete-window-config (cdr (assoc el candi))))))

;; ***** Derived eyebrowse window config feature
;; ****** Library
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

;; ****** Create derived eyebrowse window configs
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
           (if (and custr (not (string= "" custr)))
               custr
             ""))))))

;; ****** Switch derived eyebrowse window configs
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
                                     'face 'entropy/emacs-defface-face-for-eyebrowse-back-top-wg-message-face_body)
                         (propertize (if (and (not (equal top-tag ""))
                                              (not (equal top-tag nil)))
                                         (format "[%s]: %s " top-slot top-tag)
                                       (format "[%s] " top-slot))
                                     'face 'entropy/emacs-defface-face-for-eyebrowse-back-top-wg-message-face_content)
                         (propertize "." 'face 'entropy/emacs-defface-face-for-eyebrowse-back-top-wg-message-face_body))))
       (t (error "You've at top wg!"))))))

;; *** winner mode for recover previous window config faster
(use-package winner
  :ensure nil
  :commands (winner-mode)
  :eemacs-tpha
  (((:enable t :defer t))
   ("WI&BUF"
    (("i w"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'winner-mode))
      "Winner Mode"
      :enable t :exit t))))
  :eemacs-indhc
  (((:enable t :defer t)
    (winner-mode))
   ("Basic"
    (("C-c <left>" winner-undo
      "Winner Undo"
      :enable t
      :exit t
      :global-bind t)
     ("C-c <right>" winner-redo
      "Winner Redo"
      :enable t
      :exit t
      :global-bind t))))

  :init

  (cond
   (entropy/emacs-fall-love-with-pdumper
    (add-hook 'entropy/emacs-pdumper-load-hook
              #'winner-mode))
   (t
    ;; Force init after load for prevent other frame window setup mess
    (entropy/emacs-lazy-with-load-trail
     enable-winner-mode
     (winner-mode +1))))

  (setq winner-boring-buffers
        `("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"
          "*Backtrace*"
          "*rg*"
          "*Messages*"
          "*Buffer Details*"
          "*Memory Explorer*"
          "*grep*"
          "*ag search*"
          "*rg*"
          "*pt-search*"
          "*Occur*"
          "*xref*"
          "*compilation*"
          "*Compile-Log*"
          "*Warnings*"
          "*Completions*"
          "*Shell Command Output*"
          "*golint*"
          "*govet*"
          "*go-guru-output*"
          "*Gofmt Errors*"
          "*Go Test*"
          "*shell*"
          "*Python*"
          "*Ruby*"
          "*quickrun*"
          "*Diff*"
          "*Colors*"
          "*Process List*"
          "*Process-Environment*"
          ,entropy/emacs-message-message-buffname
          )
        winner-boring-buffers-regexp
        (rx (or
             "*eemacs-"
             "*entropy/"
             (regexp "\\*Async Shell Command\\*.+" )
             (regexp "^*Man.+*$")
             (regexp "^*WoMan.+*$")
             (regexp "^*Backtrace.+*$")
             (regexp "^\\*vc-")
             (regexp "\\*ivy-occur.+*$" )
             (regexp "^*godoc.+*$")
             (regexp
              "^magit[-]?\\([a-z]+\\)?:"))))

  :config
  (defun __adv/around/winner-save-old-configurations/post-command-idle-trigger
      (orig-func &rest orig-args)
    "Trigger `winner-mode' `post-command-hook'
`winner-save-old-configurations' with idle timer for perfomance
issue."
    (let (_)
      (eval
       `(entropy/emacs-run-at-idle-immediately
         __idle/winner-save-old-config
         :which-hook 0.4
         (apply ',orig-func ',orig-args)))))
  (advice-add 'winner-save-old-configurations
              :around
              #'__adv/around/winner-save-old-configurations/post-command-idle-trigger
              )
  )

;; *** desktop mode

(use-package desktop
  :if entropy/emacs-desktop-enable
  :ensure nil
  :commands (desktop-save-mode)
  :init
  (entropy/emacs-lazy-with-load-trail
   desktop-save-mode
   (desktop-save-mode 1))

  :config
  ;; Restore frames into their original displays (if possible)
  (setq desktop-restore-in-current-display nil)

  ;; Don't save/restore frames in tty
  (unless (display-graphic-p)
    (setq desktop-restore-frames nil)))

;; ** Buffer window size setting
(use-package windresize
  :commands (windresize)
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (window-configuration-change-hook) :adtype hook :pdumper-no-end t))))
   ("WI&BUF"
    (("C-<f10>" windresize "Resize Window"
      :enable t
      :global-bind t
      :exit t))))
  :config
  ;; define missing <return> key as `windresize-exit' that prevent
  ;; that key stroken calling from current major-mode map e.g. vterm
  ;; has bind it.
  (define-key windresize-map
    (kbd "<return>") #'windresize-exit))

;; ** Exchange window
(use-package buffer-move
  :commands (buf-move-up
             buf-move-down
             buf-move-left
             buf-move-right)
  :eemacs-indhca
  (((:enable t :defer (:data (:adfors (window-configuration-change-hook) :adtype hook :pdumper-no-end t)))
    (eemacs-window-config))
   ("Move Window"
    (("C-c <C-up>"     buf-move-up
      "Move Buffer To Upstairs Window"
      :enable t :global-bind t :exit t)
     ("C-c <C-down>"   buf-move-down
      "Move Buffer To Down-stairs Window"
      :enable t :global-bind t :exit t)
     ("C-c <C-left>"   buf-move-left
      "Move Buffer To Left Window"
      :enable t :global-bind t :exit t)
     ("C-c <C-right>"  buf-move-right
      "Move Buffer To Right Window"
      :enable t :global-bind t :exit t)))))

;; ** Centered-window

;; *** -------------------- union declarations --------------------

(defvar entropy/emacs-wc--center-window-function nil)
(defvar entropy/emacs-wc--uncenter-window-function nil)

(defvar entropy/emacs-wc--expand-center-window-function nil)
(defvar entropy/emacs-wc--shrink-center-window-function nil)

(defun entropy/emacs-wc--center-window ()
  (interactive)
  (when (and t
             (functionp entropy/emacs-wc--center-window-function))
    (run-hooks 'entropy/emacs-window-center-enable-before-hook)
    (message "Centering buffer window for buffer %s ..." (current-buffer))
    (funcall entropy/emacs-wc--center-window-function)
    (run-hooks 'entropy/emacs-window-center-enable-after-hook)))

(defun entropy/emacs-wc--uncenter-window ()
  (interactive)
  (when (and t
             (functionp entropy/emacs-wc--uncenter-window-function))
    (run-hooks 'entropy/emacs-window-center-disable-before-hook)
    (message "Uncentering buffer window for buffer %s ..." (current-buffer))
    (funcall entropy/emacs-wc--uncenter-window-function)
    (run-hooks 'entropy/emacs-window-center-disable-after-hook)))

(defun entropy/emacs-wc--expand-center-window ()
  (interactive)
  (when (and t
             (functionp entropy/emacs-wc--expand-center-window-function))
    (funcall entropy/emacs-wc--expand-center-window-function)))

(defun entropy/emacs-wc--shrink-center-window ()
  (interactive)
  (when (and t
             (functionp entropy/emacs-wc--shrink-center-window-function))
    (funcall entropy/emacs-wc--shrink-center-window-function)))

(defvar-local entropy/emacs-window-center-mode--is-set-p nil
  "The `entropy/emacs-window-center-mode' enable internal indicator
used for `entropy/emacs-window-center-mode' internally only.")
(define-minor-mode entropy/emacs-window-center-mode
  "Center buffer window init with raito of
`entropy/emacs-window-center-integer'."
  :init-value nil
  (let* ((this-buff (current-buffer))
         (buff-lp (and this-buff (buffer-live-p this-buff)))
         (buff-win (and buff-lp (get-buffer-window this-buff)))
         (enabled-yet (with-current-buffer this-buff
                        (bound-and-true-p entropy/emacs-window-center-mode--is-set-p)))
         (log t))
    ;; just judge filters when not invoke by auto-mode since the
    ;; auto-mode invoke the enable process after its own judge, so we
    ;; acquiesce we should do everything in auto mode env detected.
    (unless entropy/emacs-window-auto-center-require-enable-p
      (setq log (entropy/emacs-window-center-mode-turn-on-judger
                 this-buff))
      (push (list 'common
                  :state (list :do (if entropy/emacs-window-center-mode 'enable 'disable)
                               :log log
                               :buff-live-p buff-lp
                               :buff-win-p (and buff-win t)
                               :enabled-yet enabled-yet)
                  :buffer this-buff
                  :buffer-win buff-win)
            entropy/emacs-wc-window-auto-center-mode--log))
    ;; main
    (if entropy/emacs-window-center-mode
        (when (eq log t)
          (funcall-interactively
           #'entropy/emacs-wc--center-window)
          (setq entropy/emacs-window-center-mode--is-set-p t))
      (funcall-interactively
       #'entropy/emacs-wc--uncenter-window)
      (setq entropy/emacs-window-center-mode--is-set-p nil))))

;; *** Manully method
(when (eq entropy/emacs-window-center-mode-use-backend 'basic)

  (defvar-local entropy/emacs-wc-centerwindow-basic--currentbuffer-centerred-p nil)
  (defvar-local entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin nil)
  (defvar       entropy/emacs-wc-centerwindow-basic--currentbuffer-center-step 1)

  (defun entropy/emacs-wc-centerwindow-basic--do-center ()
    (let* ((ratio entropy/emacs-window-center-integer)
           this-set)
      (setq this-set (/ (entropy/emacs-window-no-margin-column-width) ratio))
      (set-window-margins
       nil
       this-set
       this-set)
      (setq entropy/emacs-wc-centerwindow-basic--currentbuffer-centerred-p t
            entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin
            this-set)))

  (defun entropy/emacs-wc-centerwindow-basic--do-clear ()
    (set-window-margins
     nil
     nil)
    (setq entropy/emacs-wc-centerwindow-basic--currentbuffer-centerred-p nil
          entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin nil))

  (defun entropy/emacs-wc-centerwindow-basic--do-expand ()
    (let* ((calc (- entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin
                    entropy/emacs-wc-centerwindow-basic--currentbuffer-center-step))
           (step (and (> calc 0)
                      calc))
           this-set)
      (when step
        (setq this-set step)
        (set-window-margins
         nil
         this-set
         this-set)
        (setq entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin
              this-set))))

  (defun entropy/emacs-wc-centerwindow-basic--do-shrink ()
    (let* ((calc (+ entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin
                    entropy/emacs-wc-centerwindow-basic--currentbuffer-center-step))
           (step (and (< calc (/ (entropy/emacs-window-no-margin-column-width)
                                 2))
                      calc))
           this-set)
      (when step
        (setq this-set step)
        (set-window-margins
         nil
         this-set
         this-set)
        (setq entropy/emacs-wc-centerwindow-basic--currentbuffer-window-margin
              this-set))))

  (setq entropy/emacs-wc--center-window-function
        #'entropy/emacs-wc-centerwindow-basic--do-center)
  (setq entropy/emacs-wc--uncenter-window-function
        #'entropy/emacs-wc-centerwindow-basic--do-clear)
  (setq entropy/emacs-wc--expand-center-window-function
        #'entropy/emacs-wc-centerwindow-basic--do-expand)
  (setq entropy/emacs-wc--shrink-center-window-function
        #'entropy/emacs-wc-centerwindow-basic--do-shrink)
  )

;; *** Using olivetti

(use-package olivetti
  :if (eq entropy/emacs-window-center-mode-use-backend 'olivetti)
  :commands
  (olivetti-mode
   olivetti-set-width
   olivetti-shrink
   olivetti-expand)
  :preface
  (defun entropy/emacs-wc--calc-olivetti-body-width ()
    "Calculate the center alignment percentage rely on
`entropy/emacs-window-center-integer'."
    (let ((partial
           (/ (- (window-width)
                 (* 2 (/ (window-width)
                         entropy/emacs-window-center-integer)))
              (float (window-width)))))
      (setq olivetti-body-width partial)))

  :init
  (setq olivetti-minimum-body-width 10)
  (setq-default olivetti-body-width 0.8)
  (add-hook 'olivetti-mode-hook
            #'entropy/emacs-wc--calc-olivetti-body-width)

  (setq entropy/emacs-wc--center-window-function
        #'(lambda ()
            (let ((tcl truncate-lines))
              (prog1
                  (olivetti-mode 1)
                ;; FIXME: why enalble `olivetti-mode' will reset
                ;; `truncate-lines' var even we can not find any
                ;; setting in olivetti source code.
                (unless (equal tcl truncate-lines)
                  (setq truncate-lines tcl))))))
  (setq entropy/emacs-wc--uncenter-window-function #'(lambda () (olivetti-mode 0)))
  (setq entropy/emacs-wc--expand-center-window-function #'olivetti-expand)
  (setq entropy/emacs-wc--shrink-center-window-function #'olivetti-shrink)

  :config
  (defun entropy/emacs-wc--olivetti-around-advice-for-window-toggle-side-windows
      (orig-func &rest orig-args)
    "The around advice for `window-toggle-side-windows' placed by
`olivetti-mode' which may cause its bug of polluting
`window-state-put-list' when the olivetti-mode is actived for
displayed buffers in where its 'window' is nil setted in some item
in `window-state-put-list' which will cause error popup for the
its subroutine `window--state-put-2'.

EEMACS_BUG: This may be an internal emacs bug or caused by
olivetti-mode itself responsibility."
    (let* ((win-list (window-list))
           (win-buffers (mapcar 'window-buffer win-list))
           olive-cache)
      ;; cancel all window's `olivetti-mode'
      (dolist (buff win-buffers)
        (with-current-buffer buff
          (when (bound-and-true-p olivetti-mode)
            (push (cons buff (buffer-local-value 'olivetti-body-width (current-buffer)))
                  olive-cache)
            (olivetti-mode 0))))
      (unwind-protect
          (apply orig-func orig-args)
        ;; recover those buffer origin olivetti status
        (dolist (cache olive-cache)
          (let ((buff (car cache))
                (body-width (cdr cache)))
            ;; ensure origin buffer displayed as before
            (when (and (buffer-live-p buff)
                       (get-buffer-window buff))
              (with-current-buffer buff
                (olivetti-mode 1)
                (olivetti-set-width body-width))))))))
  (advice-add 'window-toggle-side-windows
              :around #'entropy/emacs-wc--olivetti-around-advice-for-window-toggle-side-windows))

;; *** Auto center window

(defvar entropy/emacs-wc-window-auto-center-mode--log nil)

(defun entropy/emacs-wc-window-auto-center-mode-diwm
    (&optional buffer-or-name)
  "Enable/Disable `entropy/emacs-window-center-mode' for
`selected-window' based on filter
`entropy/emacs-wc-window-auto-center-mode-turn-on-judger' while
`entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge'
is satisfied firstly."
  (when (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
    (let* ((buff (or (and buffer-or-name (get-buffer buffer-or-name))
                     (window-buffer)))
           (buff-win (ignore-errors (get-buffer-window buff)))
           log)
      (when buff-win
        (setq log
              (entropy/emacs-wc-window-auto-center-mode-turn-on-judger
               buff))
        (with-selected-window buff-win
          (if (eq log t)
              (unless (bound-and-true-p entropy/emacs-window-center-mode)
                (entropy/emacs-window-center-mode))
            (when (bound-and-true-p entropy/emacs-window-center-mode)
              (entropy/emacs-window-center-mode 0))))))))

(defun entropy/emacs-wc-window-auto-center-mode-diwm-idle
    (&rest _)
  "Funcall `entropy/emacs-wc-window-auto-center-mode-diwm' in idle
status without repeat and stick to `current-buffer'."
  (run-with-idle-timer
   0.1 nil
   #'entropy/emacs-wc-window-auto-center-mode-diwm
   (current-buffer)))

(defun entropy/emacs-wc-window-auto-center-mode-enable-for-all-displayed-windows (&rest _)
  "Auto enable/disable `entropy/emacs-window-center-mode' for
windows in current `window-list' mapping of
`entropy/emacs-wc-window-auto-center-mode-diwm'
while
`entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge'
is satisfied firstly."
  (when (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge)
    (funcall
     (lambda (&rest _)
       (let ((wins (window-list)))
         (dolist (win wins)
           (with-selected-window win
             (entropy/emacs-wc-window-auto-center-mode-diwm))))))))

(defmacro entropy/emacs-wc-auto-center-window-mode--wrap-core-subroutine
    (adfor buff-arg-nth)
  "The macro to generate the after advice for function like:

> =(lambda (arg1 buffer-or-nam arg3 ... &rest ...) body ...)=
buffer display function to let them has the ability to center the
window automaticaly according to
`entropy/emacs-wc-window-auto-center-mode-turn-on-judger' and
suffer from whether
`entropy/emacs-window-auto-center-require-enable-p' is local
binded.

NOTE: this is an internal macro, do not use it in else where but here."
  (let ((adfunc-name
         (intern
          (format
           "entropy/emacs-wc-center-window--auto-turn-on/after-advice-for_%s"
           adfor))))
    `(let (_)
       (defun ,adfunc-name
           (&rest orig-args)
         ,(format "After advie to emacs internal func `%s' to adapt to \
`entropy/emacs-window-auto-center-require-enable-p'."
                  adfor)
         (when entropy/emacs-window-auto-center-require-enable-p
           (let* ((buffer-or-name (nth ,buff-arg-nth orig-args))
                  (buff-lp (ignore-errors (buffer-live-p (get-buffer buffer-or-name))))
                  (buff-win (and buff-lp (get-buffer-window buffer-or-name)))
                  (enabled-yet (and buff-lp
                                    (with-current-buffer buffer-or-name
                                      (bound-and-true-p entropy/emacs-window-center-mode))))
                  log)
             (when (and buff-lp buff-win)
               (if enabled-yet
                   (when (and
                          (prog1 t
                            (setq log
                                  (entropy/emacs-wc-window-auto-center-mode-turn-on-judger
                                   buffer-or-name)))
                          (not (eq log t)))
                     (with-selected-window buff-win
                       (entropy/emacs-window-center-mode 0)))
                 (when (and
                        (prog1 t
                          (setq log1
                                (entropy/emacs-wc-window-auto-center-mode-turn-on-judger
                                 buffer-or-name)))
                        (eq log1 t))
                   (with-selected-window buff-win
                     (entropy/emacs-window-center-mode)))))
             (push (list ',adfor
                         :state (list :do (if enabled-yet 'enable 'disable)
                                      :log log
                                      :buff-live-p buff-lp
                                      :buff-win-p (and buff-win t)
                                      :enabled-yet enabled-yet)
                         :buffer buffer-or-name
                         :buffer-win buff-win)
                   entropy/emacs-wc-window-auto-center-mode--log))))
       (advice-add ',adfor
                   :after
                   ',adfunc-name))))

;; Patch auto center functional to emacs internal API, currently we
;; just patching for `display-buffer' and `switch-to-buffer', since
;; `display-buffer' is the most core buffer display mechanism for
;; emacs to handle the 'display' request, but for `set-window-buffer'
;; cluster buffer replacement API like `switch-to-buffer'. We just
;; patch the `switch-to-buffer' API for that since `set-window-buffer'
;; patching is so dangerous and has no sense to ensuer the sagety.
(entropy/emacs-wc-auto-center-window-mode--wrap-core-subroutine
 display-buffer 0)
(entropy/emacs-wc-auto-center-window-mode--wrap-core-subroutine
 switch-to-buffer 0)

(add-hook 'entropy/emacs-after-startup-hook
          #'(lambda ()
              (dolist (inct-func entropy/emacs-window-auto-center-commands-list)
                (advice-add inct-func
                            :around
                            #'entropy/emacs-window-auto-center-around-advice))
              (add-hook 'entropy/emacs-delete-other-windows-after-hook
                        #'entropy/emacs-wc-window-auto-center-mode-diwm)
              (add-hook 'after-change-major-mode-hook
                        #'entropy/emacs-wc-window-auto-center-mode-diwm-idle)))

;; *** key bind
(defvar entropy/emacs-window-center-mode-hydra-hollow-is-built-p nil)
(defun entropy/emacs-window-center-mode-hydra-hollow-build (&rest _)
  (unless entropy/emacs-window-center-mode-hydra-hollow-is-built-p
    (entropy/emacs-hydra-hollow-common-individual-hydra-define+
     'eemacs-window-config nil
     '("Align Buffer"
       (("C-c M-<up>"
         (entropy/emacs-window-center-mode 1)
         "Center Window"
         :enable t
         :exit t
         :global-bind t)
        ("C-c M-<down>"
         (entropy/emacs-window-center-mode 0)
         "Clear Center Window"
         :enable t
         :exit t
         :global-bind t)
        ("{" entropy/emacs-wc--shrink-center-window
         "Shrink align width"
         :enable t)
        ("}" entropy/emacs-wc--expand-center-window
         "Expand align width"
         :enable t))))
    (setq entropy/emacs-window-center-mode-hydra-hollow-is-built-p t)
    (advice-remove 'entropy/emacs-window-center-mode
                   #'entropy/emacs-window-center-mode-hydra-hollow-build)))

(advice-add 'entropy/emacs-window-center-mode
            :before
            #'entropy/emacs-window-center-mode-hydra-hollow-build)

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-hydra-hollow-call-before-hook)
 "align-buffer-hydra-hollow-extra-init"
 "align-buffer-hydra-hollow-extra-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-window-center-mode-hydra-hollow-build))

;; ** Window divider

(entropy/emacs-lazy-initial-for-hook
 (window-configuration-change-hook)
 "window-divider-mode-init" "window-divider-mode-init" prompt-echo
 :pdumper-no-end t
 (window-divider-mode)
 (defun entropy/emacs-wc-auto-toggle-window-divider-mode ()
   (let ((need-to-disable
          (memq entropy/emacs-theme-sticker
                '(spacemacs-dark
                  spacemacs-light))))
     (if (and (bound-and-true-p window-divider-mode)
              need-to-disable)
         (window-divider-mode 0)
       (unless (or (bound-and-true-p window-divider-mode)
                   need-to-disable)
         (window-divider-mode)))))
 (run-with-idle-timer
  0.2
  t
  #'entropy/emacs-wc-auto-toggle-window-divider-mode))

;; * provide
(provide 'entropy-emacs-wc)
