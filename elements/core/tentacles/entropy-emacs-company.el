;;; entropy-emacs-company.el --- entropy emacs completion config  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-company.el
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
;; Completion referrence config for `entropy-emacs'.
;;
;; `entropy-emacs' use [[http://company-mode.github.io][company-mode]] as the completion framework as
;; the completion main tool. It's the framework who provide the
;; APIS to built arbitrary completion backends for various emacs
;; major modes even for the mode independent way.
;;
;; There's two completion server type choice for `entropy-emacs':
;;
;; 1) Traditional way:
;;
;;    The way that each backends basic on the server tool-chain are
;;    independently using its own designation, such as pyton
;;    `anaconda-mode', C `irony-mode', javascript `tern-mode'.
;;
;;    Advantage for this type is that each backend maintained
;;    individually and designed just for the single sake. this can
;;    limitting code built scope and reducing bug fixing difficulty
;;    level.
;;
;;    The weakness was that non-standard server-client communication
;;    api, which will impede the further features development who
;;    stand on the top level of all or some of them.
;;
;; 2) LSP Mode:
;;
;;    LSP (language server protocol) was brought up by Microsoft, for
;;    solving the problem caused from way '1)', it was under
;;    development. emacs melpa package 'lsp-mode' and 'elgot' was the
;;    client for thus, but be under development and with sets of
;;    bugs.
;;
;; `entropy-emacs' defaultly enable the traditional way for the sake
;; of stability.
;;
;;
;; * Configuration:
;;
;; configurationLoaidng by `entropy-emacs' automatically.
;;
;; * Code:

;; ** require

;; ** defvar

;; declare firtly for lexical proper
(defvar company-backends)
(defvar company-candidates)
(defvar company-candidates-length)
(defvar company-frontends)
(defvar company-backends)
(defvar company-dabbrev-code-everywhere)
(defvar company-dabbrev-ignore-case)
(defvar company-dabbrev-downcase)
(defvar company-dabbrev-char-regexp)

(defvar entropy/emacs-company-top-initial-backends
  '(company-yasnippet
    company-capf
    :separate
    company-dabbrev
    company-en-words)
  "Basic top company-backend for all situations.")

(defvar entropy/emacs-company-internal-pseudo-frontends
  '(company-pseudo-tooltip-frontend
    company-pseudo-tooltip-unless-just-one-frontend
    company-pseudo-tooltip-unless-just-one-frontend-with-delay)
  "The `company-mode' internal popup frontends collection.

NOTE: update with `company' upstream.")

(entropy/emacs-lazy-load-simple 'company
  (setq-default
   company-backends
   (list (copy-sequence entropy/emacs-company-top-initial-backends))))

;; ** libraries

(defun entropy/emacs-company--set-backends (&rest backends)
  (setq-local
   company-backends
   (mapcar #'(lambda (x) (if (consp x) (copy-sequence x) x))
           backends)))

(eval-and-compile
  (defmacro entropy/emacs-company--make-auto-handy-backend
      (backend-name)
    "Let a interactively defined `company-backend' abort current
company status when it is invoked while handy hint
i.e. interactvely.

This is useful for user decide to begin a backend manually when
current auto began listed `company-candidates' is not proper for
using."
    (declare (indent defun))
    (let ((name (intern (format "__ya/%s/as-auto-handy-company-backend"
                                backend-name))))
      `(progn
         (defalias ',name
           (lambda (orig-func &rest orig-args)
             (if (and (eq this-command ',backend-name)
                      ;; only for interatively backend call
                      (called-interactively-p 'interactive)
                      (entropy/emacs-operation-status/running-auto-completion-op-p))
                 (progn (company-abort)
                        (call-interactively #',backend-name)))
             (apply orig-func orig-args))
           (format "The handy began advice for the company-backend `%s'.

Created by `entropy/emacs-company--make-auto-handy-backend'."
                   ',backend-name))
         (unless (compiled-function-p (indirect-function ',name))
           (byte-compile ',name))
         (advice-add ',backend-name :around #',name)))))

;; *** yas load
(defun entropy/emacs-company-start-with-yas (&optional force)
  (when (or force (bound-and-true-p company-mode))
    (unless (bound-and-true-p yas-minor-mode)
      (entropy/emacs-require-only-once 'yasnippet)
      (yas-minor-mode 1))))

;; *** company for docs modes

(defun entropy/emacs-company-privilege-yas-for-docs ()
  "Make `company-backends' suitable for documents edits with
yasnippet support *locally*."
  (entropy/emacs-company--set-backends
   '(company-yasnippet
     ;; we should make spell candis take prior before dabbrev backend
     ;; since this is used for documentation mode.
     company-en-words
     :separate
     company-dabbrev)))

(defun entropy/emacs-company-yas-for-docs-init ()
  (let (funcs)
    (dolist (el '((org-mode . org-mode-hook)
                  (mardown-mode . markdown-mode-hook)
                  (text-mode . text-mode-hook)))
      (push
       (lambda ()
         (with-eval-after-load (car el)
           (add-hook (cdr el) #'entropy/emacs-company-privilege-yas-for-docs)))
       funcs))
    (dolist (func funcs) (funcall func))))

;; *** prunning `company-frontends'

(defun entropy/emacs-company--set-only-one-frontend
    (only-one &rest others)
  (setq company-frontends
        (if others
            (append (list only-one) others)
          (list only-one))))

(defun entropy/emacs-company--set-only-one-frontend-all-buffers
    (only-one &rest others)
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (bound-and-true-p company-mode)
        (apply 'entropy/emacs-company--set-only-one-frontend
               only-one others)))))

;; ** company core
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode
             company-mode
             company-complete
             company-files
             entropy/emacs-company-toggle-idledelay)

;; *** preface
  :preface

  (defun entropy/emacs-company-files (command)
    "Like `company-files' but using as individual eemacs command with
eemacs specifications"
    (interactive (list 'interactive) company-mode global-company-mode)
    (unless
        ;; some filters not to triggered
        (or buffer-read-only
            ;; TODO: more
            )
      (unless (bound-and-true-p company-mode)
        (company-mode))
      (company-files command)))
  (entropy/emacs-company--make-auto-handy-backend
    entropy/emacs-company-files)

  (defun entropy/emacs-company--core-subr-is-nacomp-p nil
    "return non-nil when current use company package with
native-compiled subr."
    (entropy/emacs-func-is-native-comp-p
     (entropy/emacs-get-func-origin-def 'company-call-backend)))

;; *** bind-key
  :bind (:map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ;; ("<tab>" . company-complete-selection)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))

  :eemacs-indhc
  (((:enable t :defer t)
    (company-auto-completion))
   ("Basic complete"
    (("M-/" company-complete
      "Insert the common part of all candidates or the current selection"
      :enable t :global-bind t :exit t)
     ("M-\\" entropy/emacs-company-dabbrev "dabbrev-like 'company-mode' completion backend"
      :enable t :global-bind t :exit t)
     ("C-c C-y" company-yasnippet "'company-mode' backend for 'yasnippet'"
      :enable t :global-bind t :exit t)
     ("]" entropy/emacs-company-files "Auto complete file path at point"
      :enable t :eemacs-top-bind t :exit t)
     ("M-]" company-en-words "Auto complete english word at point."
      :enable t :global-bind t :exit t)
     ("M-p" entropy/emacs-company-toggle-idledelay
      "Turn on/off automatically company completion (prefix key for set idle delay)."
      :enable t :eemacs-top-bind t :exit t))))

  :eemacs-tpha
  (((:enable t :defer t))
   ("Basic"
    (("b c"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'company-auto-completion))
      "Auto completion operations"
      :enable t :exit t))))

;; *** init for load
  :init
  (defun entropy/emacs-company-global-init ()
    (global-company-mode t)
    ;;reduce lagging on increase company echo idle delay
    (setq company-echo-delay 1)
    (add-hook 'company-mode-hook #'entropy/emacs-company-start-with-yas)
    (entropy/emacs-company-yas-for-docs-init)
    (entropy/emacs-company-toggle-frontend
     entropy/emacs-company-tooltip-use-type))
  (entropy/emacs-lazy-initial-advice-before
   '(find-file switch-to-buffer)
   "global-company-mode-init" "global-company-mode-init"
   :prompt-type 'prompt-echo
   ;; We must ensure the `global-company-mode' enabled in pdumper
   ;; recovery hook since the `company-mode-on' need `noninteractive'
   ;; is null.
   :pdumper-no-end nil
   (entropy/emacs-company-global-init))
  (entropy/emacs-lazy-load-simple '(company counsel)
    (define-key company-active-map (kbd "M-o") 'counsel-company)
    (entropy/emacs-add-hook-with-lambda
      (cons t '__eemacs-abort-company-when-enter-in-minibuffer) nil
      "Abort company status when entering minibuffer since the
`company-active-map' will influence the minibuffer keymap."
      :use-hook 'minibuffer-setup-hook
      (when-let*
          ((inhibit-quit t)
           ((not (eq real-this-command 'counsel-company)))
           (orig-win (minibuffer-selected-window))
           (orig-buff (window-buffer orig-win)))
        (with-current-buffer orig-buff
          (when (bound-and-true-p company-candidates)
            (company-abort))))))

;; *** config for after-load
  :config
;; **** basic setting

  ;; common internal customization
  (setq
   ;; just enable company in some prog referred modes
   company-global-modes (append '(emacs-lisp-mode
                                  lisp-mode
                                  lisp-interaction-mode
                                  text-mode
                                  org-mode
                                  )
                                entropy/emacs-ide-for-them)
   company-tooltip-width-grow-only nil
   company-show-quick-access nil
   company-format-margin-function nil   ;disable icon show with pseudo tooltip
   company-tooltip-limit 20  ; bigger popup window
   company-tooltip-maximum-width 70
   company-tooltip-minimum-width 20
   company-tooltip-align-annotations t
   company-tooltip-offset-display nil   ;reducing selection fast hints laggy
   company-minimum-prefix-length 2
   company-require-match nil
   )

  ;; disable common command before which the company begun to run
  ;; EEMACS_MAINTENANCE: the variable `company--bein-inhibit-commands'
  ;; is not the common API!
  (dolist (command '(previous-line
                     next-line
                     left-char
                     right-char))
    (add-to-list 'company--begin-inhibit-commands
                 command))

  ;; make `company-frontends' buffer-local permanently since we want
  ;; it be thus so we can control each buffers local specifications.
  (make-variable-buffer-local 'company-frontends)
  ;; set the default company frontend with eemacs spec
  (setq-default company-frontends '(company-pseudo-tooltip-frontend))

  ;; Reset `company-emulation-alist' for two reason:
  ;; 1) origin set of `company-emulation-alist' is a const var `((t
  ;;    . nil))' in which case its a company internal typo or neglect
  ;;    since it modifies it via install or uninstall the overriding
  ;;    keymap thru `company-uninstall-map' or `company-install-map'.
  ;; 2) the default `t' of that override's indicator will cause every
  ;;    buffer use `company-emulation-alist' as the overriding when
  ;;    it's set in an `company-backend' activated buffer, thus the
  ;;    buffer or window swiches will make keymap usage messy. Thus we
  ;;    use `company-candidates' for indicator since it's a
  ;;    buffer-local variable and default in nil for those reverse
  ;;    occasions.
  ;;
  ;; FIXME: shall we need to push a bug for company upstream?
  (setq company-emulation-alist
        (entropy/emacs-double-list 'company-candidates))

;; **** advices
;; ***** `company-post-command' idle trigger

  (defvar entropy/emacs-company--company-special-keys
    '(
      ;; TODO: common navigation hints
      right-char
      left-char
      next-line
      previous-line
      scroll-up-command
      move-end-of-line
      move-beginning-of-line
      forward-char
      forward-word
      backward-char
      backward-word
      ;; FIXME: why backward kill can not trigger the eemacs idle port correctly
      __ya/backward-kill-word
      backward-kill-word
      ;; `company-active-map' hints
      company-abort
      company-select-next
      company-select-previous
      company-select-previous-or-abort
      company-select-next-or-abort
      company-next-page
      company-previous-page
      company-complete-mouse
      company-select-mouse
      company-complete-selection
      company-quickhelp-manual-begin
      company-show-doc-buffer
      company-show-location
      company-search-candidates
      company-filter-candidates
      company-complete-number
      company-complete-quick-access
      ;; `company-search-map' hints
      company-search-other-char
      company-search-delete-char
      company-search-abort
      company-select-next-or-abort
      company-select-previous-or-abort
      company--select-next-and-warn
      company--select-previous-and-warn
      company-search-repeat-forward
      company-search-repeat-backward
      company-search-toggle-filtering
      company-search-printing-char
      ;; company internal fake set
      company-idle-begin
      company-complete-common
      company-complete-selection
      company-complete
      company-abort
      ))
  ;; Use symbol property instead of using `memq' to reduce lag
  (dolist (cmd entropy/emacs-company--company-special-keys)
    (put cmd 'eemacs-company-special-key t))

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defvar-local __ya/company-post-command/current-point nil)
  (defvar       __ya/company-post-command/current-buffer nil)
  (defvar-local __ya/company-post-command/previous-point nil)
  (defvar-local __ya/company-post-command/idle-cancel-p nil)

  (defvar __ya/company-post-command/orig-func (symbol-function 'company-post-command)
    "The origin function defination of `company-post-command'.")

  (defvar __ya/company-post-command/idle-port)
  (defun entropy/emacs-company--define-idle-post-command ()
    (progn
      (setq company-idle-delay 0)
      (if (not entropy/emacs-company-idle-delay-internal)
          (setq __ya/company-post-command/idle-port
                __ya/company-post-command/orig-func)
        (entropy/emacs-eval-with-lexical
         `(entropy/emacs-define-idle-function __ya/company-post-command/idle-port
            ,entropy/emacs-company-idle-delay-internal
            "The idle port of `company-post-command' obeyed `company''s origin
mechanism.

This function is useless unless emacs idle reached
`entropy/emacs-company-idle-delay-internal'."
            (entropy/emacs-when-let*-firstn 2
                (((eq __ya/company-post-command/current-buffer (current-buffer)))
                 ((not __ya/company-post-command/idle-cancel-p))
                 (this-command entropy/emacs-current-session-this-command-before-idle)
                 (last-command entropy/emacs-current-session-last-command-before-idle))
              (unless (or (eq this-command 'company-abort)
                          (eq this-command 'company-complete))
                (funcall __ya/company-post-command/orig-func)))))
        (when (eq __ya/company-post-command/orig-func
                  __ya/company-post-command/idle-port)
          (error "eemacs company inernal error: byte-compile origin post command defination"))
        (byte-compile __ya/company-post-command/idle-port)
        (byte-compile '__ya/company-post-command/idle-port))))
  (entropy/emacs-company--define-idle-post-command)

  (defun entropy/emacs-company--company-idle-delay-reset-guard
      (_varsym newval op _wh)
    "We should always ensure that `company-idle-delay' is zero since
we use eemacs specified idle trigger mechanism."
    (when (and (eq op 'set) (not (eq 0 newval)))
      (user-error "Should not manually modifie `company-idle-delay' to %s"
                  newval)))
  (add-variable-watcher 'company-idle-delay
                        #'entropy/emacs-company--company-idle-delay-reset-guard)

  (defun entropy/emacs-company-toggle-idledelay (&optional prefix)
    "Toggle `entropy/emacs-company-idle-delay-internal' on/off.

When PREFIX, set the specfied idle delay seconds for as or use
`entropy/emacs-company-idle-delay-default' as default."
    (interactive "P")
    (let ((def-safe-idle-secs
            (max (+ 0.1 entropy/emacs-safe-idle-minimal-secs)
                 entropy/emacs-company-idle-delay-default)))
      (if (and (not prefix)
               (bound-and-true-p entropy/emacs-company-idle-delay-internal))
          (progn (setq entropy/emacs-company-idle-delay-internal nil)
                 (entropy/emacs-company--define-idle-post-command)
                 (message "turn off `entropy/emacs-company-idle-delay-internal'"))
        (setq entropy/emacs-company-idle-delay-internal
              (if prefix
                  (let ((secs (string-to-number
                               (read-string "Input Company delay secs: "))))
                    (if (and (numberp secs)
                             (> secs entropy/emacs-safe-idle-minimal-secs))
                        secs
                      (user-error "Invalid company-delay secs '%s'" secs)))
                def-safe-idle-secs))
        (entropy/emacs-company--define-idle-post-command)
        (let ((entropy/emacs-message-non-popup t))
          (entropy/emacs-message-do-message
           "%s '%s' to '%s'"
           (blue "Set")
           (yellow (symbol-name 'entropy/emacs-company-idle-delay-internal))
           (red (number-to-string entropy/emacs-company-idle-delay-internal)))))))

  (defun __ya/company-post-command (orig-func &rest orig-args)
    "Yet another `company-post-command' which run with idle timer so
that the sequentially fast hints not laggy by `candidates'
re-calculation."
    (setq __ya/company-post-command/previous-point __ya/company-post-command/current-point
          __ya/company-post-command/current-point (point)
          __ya/company-post-command/current-buffer (current-buffer)
          __ya/company-post-command/idle-cancel-p nil)
    ;; FIXME: prevent duplicated timer delay show since it may cause
    ;; eemacs bug h:c5b6bd90-0662-4daa-877f-5be88c04ce2a.
    (entropy/emacs-cancel-timer-var company-timer)
    (if (or entropy/emacs-current-session-is-idle-p
            (not company-candidates)
            (entropy/emacs-get-symbol-prop this-command 'eemacs-company-special-key)
            (entropy/emacs-get-symbol-prop last-command 'eemacs-company-special-key)
            (entropy/emacs-get-symbol-prop real-this-command 'eemacs-company-special-key)
            (entropy/emacs-get-symbol-prop real-last-command 'eemacs-company-special-key)
            (current-idle-time)
            ;; FIXME: if we using idle in `delete' char cases, company
            ;; will not working properly and may cause emacs hang?
            (and __ya/company-post-command/previous-point
                 (< __ya/company-post-command/current-point
                    __ya/company-post-command/previous-point)
                 ;; trigger origin func when delete more with human typing habits
                 (> (- __ya/company-post-command/previous-point
                       __ya/company-post-command/current-point)
                    3)))
        (let ((company-idle-delay entropy/emacs-company-idle-delay-internal))
          (apply orig-func orig-args)
          (setq __ya/company-post-command/idle-cancel-p t))
      (funcall __ya/company-post-command/idle-port)
      ;; EEMACS_MAINTENANCE&TODO: ensure no duplicate for above idle
      ;; progress but seemes company has its own preventing condition
      ;; wrapped already.
      (company-install-map)))

  (advice-add 'company-post-command :around #'__ya/company-post-command)

;; ***** fly on type for `delete-char'
  (defvar-local __company-delc-time-host nil)
  (defun __company-delc-time-fly-p ()
    "Judge whether the `delete-char' hit on flying while company
activated status. Default time during set is less than 70ms."
    (catch :exit
      (let* ((time-old (or __company-delc-time-host (throw :exit nil)))
             (time-now (current-time)))
        (and
         (eq (cadr time-now) (cadr time-old))
         (<= (entropy/emacs-time-subtract time-old time-now t)
             entropy/emacs-company-delete-char-on-the-fly-duration)))))
  (defun __company-delete-char (orig-func &rest orig-args)
    (let (rtn)
      (when (and (bound-and-true-p company-mode)
                 (bound-and-true-p company-candidates))
        (when (or
               ;; always abort while using the post-command respeced
               ;; frontend e.g. pseudo tooltip, since
               ;; `company-post-command' will trigger the
               ;; ':post-command' slot of the frontend body in every
               ;; hinted for delete refer command which will cause the
               ;; delete char period time judge be useless.
               (memq (car company-frontends)
                     entropy/emacs-company-internal-pseudo-frontends)
               (__company-delc-time-fly-p))
          (company-abort)))
      (setq rtn (apply orig-func orig-args)
            __company-delc-time-host
            (current-time))
      rtn))
  ;; FIXME: usually adviced for `delete-backward-char' is enough since
  ;; it's the subroutine for most of backspace commands binding for
  ;; major-modes but shall we build a list commands that we can cover
  ;; them all?
  (advice-add 'delete-backward-char :around #'__company-delete-char)

;; ***** pseudo tooltip optimization
;; ****** remove annotation perform on pseudo frontend
  (defun entropy/emacs-company--pseudo-no-annotation
      (orig-func &rest orig-args)
    "Force disable annotation for pseudo tooltip for performance
consideration.

Reason:

`company--create-lines' will call annotation query for each
canididates which makes emacs laggy for each post-command while
`company-pseudo-tooltip-unhide'."
    (unless (and
             (eq (car orig-args) 'annotation)
             (memq (car company-frontends)
                   entropy/emacs-company-internal-pseudo-frontends))
      (apply orig-func orig-args)))
  ;; we just escape annotation for not native compiled origin def
  ;; since the letter is performance satisfied.
  (unless (entropy/emacs-company--core-subr-is-nacomp-p)
    (advice-add 'company-call-backend
                :around
                #'entropy/emacs-company--pseudo-no-annotation))

;; ****** pseudo frontend subroutine performance optimization
;; ******* simplify the pseudo candi line overlay generator
  (defun __ya/company-fill-propertize (value _annotation width selected left right)
    "The simplify `company-fill-propertize'.

EEMACS_MAINTENANCE: stick to upstream udpate"
    (let* ((margin (length left))
           (_ (setq value (company-reformat (company--pre-render value))))
           (line (concat left
                         (company-safe-substring
                          value 0 width)
                         right)))
      (setq width (+ width margin (length right)))
      (when selected
        (add-face-text-property 0 width 'company-tooltip-selection t line))
      (add-face-text-property 0 width 'company-tooltip t line)
      line))
  (unless (entropy/emacs-company--core-subr-is-nacomp-p)
    (advice-add 'company-fill-propertize
                :override #'__ya/company-fill-propertize))

;; ******* optimization the internal substring routine
  (defun __ya/company-safe-substring (orig-func &rest orig-args)
    "High performance version of `company-safe-substring'.
EEMACS_MAINTENANCE: stick to upstream udpate"
    (let ((str (car orig-args))
          (from (cadr orig-args))
          (to (caddr orig-args)))
      (if (string-match-p "\n" str)
          (apply orig-func orig-args)
        ;; the single line string can be hacked as manupulation with
        ;; below high-performance replacements.
        (let* ((str-width (string-width str)))
          (if (> from str-width)
              ""
            (if to
                (concat
                 (if (<= str-width to)
                     str
                   (substring str from to))
                 (let ((padding (- to (string-width str))))
                   (when (> padding 0)
                     (company-space-string padding))))
              (substring str from)))))))
  (advice-add 'company-safe-substring
              :around
              #'__ya/company-safe-substring)

;; ******* `company--posn-col-row' optimization
  ;; EEMACS_MAINTENANCE: this may update with upstream
  (defun __ya/company--posn-col-row (posn)
    "NOTE: this function has been redefined by eemacs to get more
efficiently way."
    (let* (;; calculate `posn-col-row' just once for reducing I/O periods.
           (nominal-pos (posn-col-row posn))
           (col (car nominal-pos))
           ;; `posn-col-row' doesn't work well with lines of different height.
           ;; `posn-actual-col-row' doesn't handle multiple-width characters.
           (row (cdr (or (posn-actual-col-row posn)
                         ;; When position is non-visible for some reason.
                         nominal-pos))))
      (when (bound-and-true-p display-line-numbers)
        (cl-decf col (+ 2 (line-number-display-width))))
      (cons (+ col (window-hscroll)) row)))

  (advice-add 'company--posn-col-row
              :override
              #'__ya/company--posn-col-row)

;; ***** `company-show-doc-buffer' conflicts hack

  (defun __ya/company-show-doc-buffer ()
    "Redefine of `company-show-doc-buffer' since its conflicted
with `shackle'."
    (interactive)
    (let* ((orig-win (selected-window))
           (_ (when (minibuffer-window-active-p orig-win)
                (setq orig-win (minibuffer-selected-window))))
           (other-window-scroll-buffer nil)
           (selection-index (or company-selection 0))
           (selected-candi-str (nth selection-index company-candidates))
           (doc-buffer (company-call-backend 'doc-buffer selected-candi-str))
           (_ (progn
                (when (consp doc-buffer)
                  (setq doc-buffer (car doc-buffer)))
                (cond ((stringp doc-buffer)
                       (setq doc-buffer (get-buffer doc-buffer)))
                      ((bufferp doc-buffer)))
                (unless doc-buffer
                  (user-error
                   "can not fetch doc of current selection '%s'"
                   selected-candi-str))
                (unless (and (bufferp doc-buffer)
                             (buffer-live-p doc-buffer))
                  (error "company doc buffer '%s' is not a buffer"
                         doc-buffer))))
           (doc-buffer-mode (and doc-buffer
                                 (with-current-buffer doc-buffer
                                   major-mode)))
           (doc-contents (and doc-buffer
                              (with-current-buffer doc-buffer
                                (buffer-substring (point-min) (point-max)))))
           (doc-buffer-new (get-buffer-create "*eemacs-company-doc*")))

      ;; NOTE: abort the origin window buffer company-status when used
      ;; since its emulation keymap is still activated.
      (with-selected-window orig-win
        (when (bound-and-true-p company-mode)
          (company-abort)))
      ;; show the eemacs specified doc buffer
      (with-current-buffer doc-buffer-new
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert doc-contents)
          (goto-char (point-min))
          (setq buffer-read-only t)
          (eval `(,doc-buffer-mode))
          (entropy/emacs-local-set-key
           (kbd "q")
           ;; using quote to disable closure binding
           (lambda ()
             (interactive)
             (kill-buffer-and-window)))))
      (display-buffer doc-buffer-new)))
  (advice-add 'company-show-doc-buffer
              :override
              #'__ya/company-show-doc-buffer)

;; *** __end__
  )

;; ** company components function autoload

(use-package company-dabbrev
  :ensure nil :after company
  :defines
  (company-dabbrev-code-everywhere
   company-dabbrev-code-other-buffers
   company-dabbrev-code-time-limit
   company-dabbrev-ignore-case
   company-dabbrev-downcase
   company-dabbrev-char-regexp)
  :commands (company-dabbrev entropy/emacs-company-dabbrev)
  :init
  (setq
   ;; performance restrict
   company-dabbrev-code-everywhere nil
   company-dabbrev-code-other-buffers nil
   ;; tiny small search time suppied for prevent lag
   company-dabbrev-code-time-limit 0.01
   company-dabbrev-ignore-case t
   company-dabbrev-downcase nil
   ;; simplify the regexp search for reduce lag
   company-dabbrev-char-regexp "\\sw")

  :config
  (defun entropy/emacs-company-dabbrev
      (command &optional arg &rest ignored)
    "Same as `company-dabbrev' but enlarge its restriction since this
command is used interactively only (i.e. not a
`company-backends') in which case no need to restrict its
performance."
    (declare (interactive-only t))
    (interactive (list 'interactive))
    (let (;; TODO: enlarge company-dabbrev limitations
          (company-dabbrev-code-everywhere t)
          (company-dabbrev-code-other-buffers t)
          (company-dabbrev-code-time-limit 0.5)
          ;; FIXME: related to bug of h-f551b679-908f-4b64-b08e-e7074d17581e
          ;; NOTE: Do not set this var too complicated which will make emacs
          ;; lag since `company-dabbrev' use `looking-back' to search
          ;; matching.
          (company-dabbrev-char-regexp "[-_/a-zA-Z0-9.><]"))
      (apply 'company-dabbrev command arg ignored)))
  (entropy/emacs-company--make-auto-handy-backend
    entropy/emacs-company-dabbrev))

(use-package company-files     :ensure nil :after company :commands company-files)
(use-package company-yasnippet :ensure nil :after company :commands company-yasnippet)

;; ** company enhancement
;; *** toggle framework

(eval-and-compile
  (defvar entropy/emacs-company--frontend-register nil
    "The register for the type of `company-frontends'.

It is a list of alist which formed as car as the frontend type
name symbol and cdr of a plist consists of:

- =:enable= : a function without argument required to enable TYPE
- =:disable= : a function without argument required to disable TYPE
- =:daemon-init=: a function (or a form build the function) which will
  be inject into `entropy/emacs-daemon-server-after-make-frame-hook'
  used to manage the daemon refer specifications, usually build by
  `entropy/emacs-with-daemon-make-frame-done'.
")
  (defvar entropy/emacs-company--frontend-daemon-current-hook nil
    "The internally used variable to store the =:daemon-init=
function for `entropy/emacs-company-frontend-sticker'.")
  (defvar entropy/emacs-company-frontend-sticker nil
    "The current used eemacs `company-frontends' type which has its
instance in `entropy/emacs-company--frontend-register' or is nil
indicates that there's no type is enabled."))

(defun entropy/emacs-company-toggle-frontend (type)
  "Set or toggle the `company-frontends' using type TYPE.

When invoked from interaction call, the type completion prompts
is popuped out.

This function get the TYPE instance from
`entropy/emacs-company--frontend-register' and save the user spec
in `entropy/emacs-company-frontend-sticker'."
  (interactive
   (list (intern
          (completing-read "Choose valid company frontend: "
                           (mapcar (lambda (x) (symbol-name (car x)))
                                   entropy/emacs-company--frontend-register)))))
  (let* ((cur-type entropy/emacs-company-frontend-sticker)
         (cur-disable
          (or
           (plist-get
            (alist-get
             ;; NOTE: just get the disable function from the registry
             ;; form `entropy/emacs-company-frontend-sticker' since we
             ;; shouldn't use the disable function when it is the
             ;; first time call the toggle function.
             cur-type
             entropy/emacs-company--frontend-register)
            :disable)
           ;; fall back to the fake one
           (lambda (&rest _) nil)))
         (chose-enable
          (plist-get
           (alist-get type entropy/emacs-company--frontend-register)
           :enable))
         (daemon-init
          (plist-get
           (alist-get type entropy/emacs-company--frontend-register)
           :daemon-init))
         (daemon-init-inject-func
          (lambda ()
            ;; remove the old one
            (when entropy/emacs-company--frontend-daemon-current-hook
              (remove-hook 'entropy/emacs-daemon-server-after-make-frame-hook
                           entropy/emacs-company--frontend-daemon-current-hook))
            ;; generate the daemon guard function
            (unless (symbolp daemon-init)
              (setq daemon-init (entropy/emacs-eval-with-lexical daemon-init)))
            (unless (functionp daemon-init)
              (user-error
               "[company-toggle-register '%s']: daemon init is not an function or can not build an function!"
               cur-type))
            ;; regist the new one
            (setq entropy/emacs-company--frontend-daemon-current-hook
                  daemon-init)
            (unless (memq daemon-init
                          entropy/emacs-daemon-server-after-make-frame-hook)
              (add-hook 'entropy/emacs-daemon-server-after-make-frame-hook
                        daemon-init)))))
    (when (not (functionp cur-disable))
      (error "Can not find associated disable function for company frontend '%s'"
             cur-type))
    (unless (functionp chose-enable)
      (error "Can not find associated enable function for company frontend '%s'"
             type))

    (progn
      ;; Firstly we should disable current frontend injections
      (funcall cur-disable)

      ;; Finally invoke new type
      (cond (
             ;; is daemon and inited
             (and entropy/emacs-daemon-server-init-done (daemonp))
             (message "Enable company frontend type '%s' after daemon make frame ..."
                      type)
             (funcall chose-enable)
             (funcall daemon-init-inject-func))
            (
             ;; is daemon but not inited
             (and (not entropy/emacs-daemon-server-init-done) (daemonp))
             (message "Planning to enable company frontend type '%s' after daemon make frame ..."
                      type)
             (funcall daemon-init-inject-func))
            (t
             (message "Enable company frontend type '%s' ..."
                      type)
             (funcall chose-enable)))
      (setq entropy/emacs-company-frontend-sticker type))
    (message "OK: change company frontend from %s to %s done!" cur-type type)))

;; *** default company frontend
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :after company
  :commands (company-quickhelp-mode
             company-quickhelp-manual-begin)
  :eemacs-functions (company-quickhelp-frontend)
  :bind (:map company-active-map
              ("<f1>" . nil))
  :preface

  (defun entropy/emacs-company--default-frontend-set-hook
      (&rest _)
    (apply 'entropy/emacs-company--set-only-one-frontend
           'company-pseudo-tooltip-frontend
           (when (display-graphic-p)
             (list 'company-quickhelp-frontend))))

  (defun entropy/emacs-company--default-enable ()
    ;; just use one frontends for reduce lagging
    (add-hook 'company-mode-hook
              #'entropy/emacs-company--default-frontend-set-hook)
    ;; travel all buffers with `company-mode' enabled to set frontend.
    (apply 'entropy/emacs-company--set-only-one-frontend-all-buffers
           'company-pseudo-tooltip-frontend
           (when (display-graphic-p)
             (list 'company-quickhelp-frontend)))
    (if (entropy/emacs-company--core-subr-is-nacomp-p)
        (entropy/emacs-setf-by-body company-format-margin-function
          (cond ((display-graphic-p) 'company-vscode-dark-icons-margin)
                (t 'company-text-icons-margin)))
      (setq company-format-margin-function nil))
    (cond ((display-graphic-p)
           (company-quickhelp-mode 1)
           (entropy/emacs-set-key-without-remap
             company-active-map
             (kbd "C-h")
             'company-quickhelp-manual-begin))
          (t
           (message
            "NOTE: Can not enable company-quickhelp in non-gui session"))))

  (defun entropy/emacs-company--default-disable ()
    (remove-hook 'company-mode-hook
                 #'entropy/emacs-company--default-frontend-set-hook)
    ;; travel all buffers with `company-mode' enabled to set default frontend
    (entropy/emacs-company--set-only-one-frontend-all-buffers
     'company-pseudo-tooltip-frontend)
    (when (bound-and-true-p company-quickhelp-mode)
      (company-quickhelp-mode 0)
      (entropy/emacs-set-key-without-remap
        company-active-map
        (kbd "C-h")
        'company-show-doc-buffer)))

  (add-to-list 'entropy/emacs-company--frontend-register
               '(default
                  :enable entropy/emacs-company--default-enable
                  :disable entropy/emacs-company--default-disable
                  :daemon-init
                  (entropy/emacs-with-daemon-make-frame-done
                    'company-default-mode (&rest _)
                    :when-tui
                    (entropy/emacs-company--default-disable)
                    :when-gui
                    (entropy/emacs-company--default-enable))))

  :init
  (setq company-quickhelp-delay
        entropy/emacs-company-quickhelp-delay-default
        company-quickhelp-use-propertized-text t))

;; *** Company-box config

(use-package company-box
  :after company
  :commands (company-box-mode)
  :eemacs-functions (entropy/emacs-company-box-delete-all-child-frames)
;; **** preface
  :preface

  (defun entropy/emacs-company--company-box-frontend-set-hook
      (&rest _)
    (entropy/emacs-company--set-only-one-frontend
     'company-box-frontend))

  (defun __auto_enable_or_disable_company-box-mode
      ()
    (if (bound-and-true-p company-mode)
        (company-box-mode 1)
      (company-box-mode 0)))

  (defun entropy/emacs-company--box-disable ()
    (progn
      (remove-hook 'company-mode-hook
                   #'company-box-mode)
      (remove-hook 'company-mode-hook
                   #'entropy/emacs-company--company-box-frontend-set-hook)
      (remove-hook
       'company-mode-hook
       '__auto_enable_or_disable_company-box-mode)
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (when (bound-and-true-p company-box-mode)
                  (company-box-mode 0))))
            (buffer-list))
      (entropy/emacs-set-key-without-remap
        company-active-map
        (kbd "C-h")
        'company-show-doc-buffer)
      ;; We must remove all unvisible company-box frame since it may
      ;; make cli session hang (i.e. the focus missed in)
      (entropy/emacs-company-box-delete-all-child-frames)))

  (defun entropy/emacs-company--box-enable ()
    (cond ((entropy/emacs-posframe-adapted-p)
           ;; just use one frontends for reduce lagging
           (add-hook 'company-mode-hook
                     #'entropy/emacs-company--company-box-frontend-set-hook)
           ;; travel all buffers with `company-mode' enabled to set frontend.
           (entropy/emacs-company--set-only-one-frontend-all-buffers
            'company-box-frontend)
           (add-hook
            'company-mode-hook
            '__auto_enable_or_disable_company-box-mode)
           (mapc (lambda (buffer)
                   (with-current-buffer buffer
                     (when (bound-and-true-p company-mode)
                       (unless (bound-and-true-p company-box-mode)
                         (company-box-mode 1)))))
                 (buffer-list))
           (entropy/emacs-set-key-without-remap
             company-active-map
             (kbd "C-h")
             'company-box-doc-manually))
          (t
           (let (_)
             (entropy/emacs-message-do-message
              "%s"
              (red
               "Can not enable company-box in non-gui session, did nothing here."
               ))
             (entropy/emacs-company--default-enable)))))

  (add-to-list 'entropy/emacs-company--frontend-register
               '(company-box :enable entropy/emacs-company--box-enable
                             :disable entropy/emacs-company--box-disable
                             :daemon-init
                             (entropy/emacs-with-daemon-make-frame-done
                               'company-box-mode (&rest _)
                               :when-tui
                               (entropy/emacs-company--box-disable)
                               :when-gui
                               (entropy/emacs-company--box-enable))))

;; **** init
  :init
  (setq
   company-box-doc-enable (if entropy/emacs-company-quickhelp-delay-default t)
   company-box-doc-delay
   ;; FIXME: company box idle delay smaller than 0.42 will cause
   ;; first candi doc not show
   (max 0.42 (or entropy/emacs-company-quickhelp-delay-default 0)))

  (setq
   company-box-show-single-candidate 'always
   company-box-enable-icon t
   company-box-icons-alist 'company-box-icons-images
   company-box-icon-right-margin 1)

;; **** config
  :config

;; ***** show mechanism patch
  (when sys/linuxp
    ;; Fix child-frame resize/reposition bug on linux
    (if (boundp 'x-gtk-resize-child-frames)
        ;; FIXME: `x-gtk-resize-child-frames' option was one temporal
        ;; patch method invoking from emacs-devel commit c49d379f17bcb
        ;; which will eliminated in the future emacs version.
        (if (and
             (> emacs-major-version 26)
             (string-match-p "GTK3" system-configuration-features)
             (let ((value (getenv "XDG_CURRENT_DESKTOP")))
               (and (stringp value)
                    ;; It can be "ubuntu:GNOME".
                    (string-match-p "GNOME" value))))
            ;; Not future-proof, but we can use it now.
            (setq x-gtk-resize-child-frames 'resize-mode)
          (if ;; FIXME: in emacs-29 gui session
              ;; `x-gtk-resize-child-frames' eq to `hide' will make
              ;; emacs broken and non-usable as error msg like:
              ;; `(circular-list (("29.0.50") . #2))' when quick
              ;; delete chars and reinput those chars and lead the
              ;; next show causing this bug.
              ;;
              ;; This seems to a eemacs internal bug caused since we
              ;; can not reproduce in vanilla emacs-29. And just
              ;; appeared in `company-box'.
              ;;
              ;; see eemacs bug h:c5b6bd90-0662-4daa-877f-5be88c04ce2a
              (<= emacs-major-version 29)
              (setq x-gtk-resize-child-frames nil)
            (setq x-gtk-resize-child-frames 'hide)))))

;; ***** Icons specified patch

  (with-no-warnings
    ;; Prettify icons
    (defun entropy/emacs-company--company-box-icons-elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp
                :override
                #'entropy/emacs-company--company-box-icons-elisp))

;; ***** advices
;; ****** make `company-box--set-mode' proper

  ;; EEMACS_MAINTENANCE: should follow upstream updates
  (defun __ya/company-box--set-mode (&optional frame)
    (cond
     ((and (bound-and-true-p company-box-mode) (not (display-graphic-p frame)))
      (company-box-mode -1))
     ((bound-and-true-p company-box-mode)
      (company-box--tweak-external-packages)
      (remove-hook 'after-make-frame-functions 'company-box--set-mode t)
      (add-hook 'delete-frame-functions 'company-box--kill-buffer)
      (add-hook 'buffer-list-update-hook 'company-box--handle-window-changes t)
      (make-local-variable 'company-frontends)
      ;; HACK: Make `company-frontends' removed all internal frontends
      ;; since origin procedure doesn't enum the all internal
      ;; frontends.
      (mapc (lambda (x) (setq company-frontends
                              (delete x company-frontends)))
            entropy/emacs-company-internal-pseudo-frontends)
      (add-to-list 'company-frontends 'company-box-frontend)
      (unless (assq 'company-box-frame frameset-filter-alist)
        (push '(company-box-doc-frame . :never) frameset-filter-alist)
        (push '(company-box-frame . :never) frameset-filter-alist)))
     ((memq 'company-box-frontend company-frontends)
      (setq company-frontends (delq 'company-box-frontend company-frontends))
      ;; HACK: eemacs should support only one candi show even if use
      ;; internal frontend.
      (add-to-list 'company-frontends 'company-pseudo-tooltip-frontend))))

  (advice-add 'company-box--set-mode
              :override
              #'__ya/company-box--set-mode)

;; ****** company-box child-frame patch

;; ******* bug fix
  (defun __ya/company-box--get-frame
      (orig-func &rest orig-args)
    "Like `company-box--get-frame' but we should guarantee the
returned FRAME is lived since we can not do anything in a dead
frame as that will cause error."
    (let ((frame (apply orig-func orig-args)))
      (when (and (framep frame) (frame-live-p frame))
        frame)))
  (advice-add 'company-box--get-frame
              :around
              #'__ya/company-box--get-frame)

;; ******* core patch
;; ******** unified core patch

  (defvar entropy/emacs-company-box-silent-error-log nil)
  (defun entropy/emacs-company--cmpbox-silent-error (fmtstr &rest args)
    (let ((msg (format-message fmtstr args)))
      (push (cons (format-time-string "[%Y-%m-%d %a %H:%M:%S]")
                  msg)
            entropy/emacs-company-box-silent-error-log)
      (message "%s" msg)))
  (eval-and-compile
    (defmacro entropy/emacs-company--cmpbox-with-demoted-errors
        (format &rest body)
      "Same as `with-demoted-errors' with use
`entropy/emacs-company--cmpbox-silent-error' instead of `message'
as subroutine."
      (declare (debug t) (indent 1))
      (let* ((err (make-symbol "err"))
             (orig-body body)
             (format (if (and (stringp format) body) format
                       (prog1 "Error: %S"
                         (if format (push format body)))))
             (exp
              `(condition-case-unless-debug ,err
                   ,(macroexp-progn body)
                 (error (entropy/emacs-company--cmpbox-silent-error ,format ,err)
                        nil))))
        (if (eq orig-body body) exp
          ;; The use without `format' is obsolete, let's warn when we bump
          ;; into any such remaining uses.
          (macroexp-warn-and-return "Missing format argument" exp nil nil format)))))

  (defvar __company-box-doc-hided-p nil)
  (defvar __company-box-doc-need-force-hide-p nil)
  (defvar __company-box-main-frame-hided-p nil)
  (defvar __company-box-main-frame-need-force-hide-p nil)

;; ********* set child frame indicator
  (defun __company-box-make-child-frame-with-frame-indicator
      (orig-func &rest orig-args)
    "Let `company-box--make-frame' be explicitly with frame indicator
as its `frame-parameter'."
    (let ((company-box-frame-parameters
           (append company-box-frame-parameters
                   `((this-company-box-frame-p . t)))))
      (apply orig-func orig-args)))
  (advice-add 'company-box--make-frame
              :around
              #'__company-box-make-child-frame-with-frame-indicator)

  (defun __company-box-doc-make-frame-with-frame-indicator
      (orig-func &rest orig-args)
    "Let `company-box-doc--make-frame' be explicitly with frame
indicator as its `frame-parameter'."
    (let ((company-box-doc-frame-parameters
           (append company-box-doc-frame-parameters
                   `((this-company-box-frame-p . t)
                     (this-company-box-doc-frame-p . t)))))
      (apply orig-func orig-args)))
  (advice-add 'company-box-doc--make-frame
              :around
              #'__company-box-doc-make-frame-with-frame-indicator)

;; ********* delete company box frames

  (defun entropy/emacs-company--cmpbox-del-frames nil
    ;; abort all activated company activities firstly
    (dolist (buff (buffer-list))
      (with-current-buffer buff
        (when (bound-and-true-p company-candidates)
          (company-abort))))
    ;; delete all `company-box' (and doc) frames with reset the frame
    ;; local var.
    (dolist (frame (frame-list))
      (let ((cmpbox-frame-p (frame-parameter frame 'this-company-box-frame-p))
            lfm)
        (if cmpbox-frame-p (delete-frame frame t)
          (when (setq lfm (frame-local-getq company-box-frame frame))
            (if (frame-live-p lfm) (delete-frame lfm t))
            (frame-local-setq company-box-frame nil frame))
          (when (setq lfm (frame-local-getq company-box-doc-frame frame))
            (if (frame-live-p lfm) (delete-frame lfm t))
            (frame-local-setq company-box-doc-frame nil frame)))))
    ;; rest eemacs specs set
    (setq __company-box-doc-hided-p nil))

;; ********* frame font spec
  (defun __company-box-make-child-frame-with-fontspec
      (orig-func &rest orig-args)
    "Let `company-box--make-frame' use same font from its parent
frame."
    (let ((company-box-frame-parameters
           (append company-box-frame-parameters
                   `((font . ,(frame-parameter nil 'font))))))
      (apply orig-func orig-args)))
  (advice-add 'company-box--make-frame
              :around #'__company-box-make-child-frame-with-fontspec)

  (defun __company-box-make-doc-child-frame-with-fontspec
      (orig-func &rest orig-args)
    "Let `company-box-doc--make-frame' use same font from its parent
frame."
    (let ((company-box-doc-frame-parameters
           (append company-box-doc-frame-parameters
                   `((font . ,(frame-parameter nil 'font))))))
      (apply orig-func orig-args)))
  (advice-add 'company-box-doc--make-frame
              :around #'__company-box-make-doc-child-frame-with-fontspec)

;; ********* recreate child frame after theme load
  (defun __company-box-remove-child-frame/before-load-theme ()
    "Remove company-box's frame before load a new theme since the we
don't set a proper method to let preserved old frame use the new
theme face spces."
    (when (bound-and-true-p company-candidates)
      (company-abort))
    (entropy/emacs-company--cmpbox-del-frames))
  (add-hook 'entropy/emacs-theme-load-before-hook
            #'__company-box-remove-child-frame/before-load-theme)

;; ******** company box doc core patch
;; ********* company box doc hide when fly on editing

  (defun __company-box-doc-show-timer-func (selection frame)
    "`company-box-doc' delay show core subroutine.

EEMACS_MAINTENANCE: may need update with company-box upstream."
    (company-box-doc--show selection frame)
    (company-ensure-emulation-alist))
  (defun __ya/company-box-doc (selection frame)
    "Patched for: Run doc frame hide just once time when consecutively fast
hints.

The origin `company-box-doc' has preparation with doc frame visible
checking with function `frame-local-get' which mapped local frame
obarray to search local varaible which has bad benchmark performance
as an scroll lagging reason when toggle on `company-box-doc-enable'.

EEMACS_MAINTENANCE: need patching updately with `company-box'
upstream."
    (when-let* ((company-box-doc-enable))
      (unless (or __company-box-doc-need-force-hide-p
                  __company-box-doc-hided-p)
        (let ((doc-frame
               (frame-local-getq company-box-doc-frame
                                 frame)))
          (when (and doc-frame
                     (framep doc-frame)
                     (frame-live-p doc-frame)
                     (frame-visible-p doc-frame)
                     ;; to ensuer is not the root frame
                     (framep (frame-parent doc-frame)))
            (make-frame-invisible doc-frame))
          (setq __company-box-doc-hided-p t)))
      (when (timerp company-box-doc--timer)
        (cancel-timer company-box-doc--timer)
        (cancel-function-timers '__company-box-doc-show-timer-func)
        (setq company-box-doc--timer nil))
      (setq company-box-doc--timer
            (run-with-timer
             company-box-doc-delay nil
             #'__company-box-doc-show-timer-func
             selection frame))))
  (advice-add 'company-box-doc :override #'__ya/company-box-doc)

  (defun __local-company-box-doc-hided-rest
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (setq __company-box-doc-hided-p nil)
      rtn))
  (advice-add 'company-box-doc--show
              :around #'__local-company-box-doc-hided-rest)

  (defun __company-box-doc-hide--with-local-judge
      (orig-func &rest orig-args)
    (unless (or __company-box-doc-need-force-hide-p
                __company-box-doc-hided-p)
      (let ((rtn (apply orig-func orig-args)))
        (setq __company-box-doc-hided-p t)
        rtn)))
  (advice-add 'company-box-doc--hide
              :around
              #'__company-box-doc-hide--with-local-judge)

  (defun __ya_company-box-doc-hide (&rest _)
    (company-box-doc--hide (selected-frame)))
  (defun entropy/emacs-company-box-hide-on-the-fly (&rest _)
    "Hide company-box-doc frame when flying on the hints when the
older doc show is actived since we delayed company update command
while in `company-box-mode'."
    (if (bound-and-true-p company-box-mode)
        (add-hook 'pre-command-hook
                  #'__ya_company-box-doc-hide nil t)
      (remove-hook 'pre-command-hook
                   #'__ya_company-box-doc-hide t)))
  (add-hook 'company-box-mode-hook
            #'entropy/emacs-company-box-hide-on-the-fly)

;; ********* `company-box-doc--show' neglect patch

  (defun __ya/company-box-doc--show/neglect-patch/0 (&rest _)
    "If the doc frame is manipulated with multi buffer display
operation then the `frame-root-window' of that frame will be
inner one in which case made collision between
`company-box-doc''s origin usage/designation and the new
occasion.

This patch delete the origin doc frame so that let the subroutine
recreates a new one for usage so that remove that collision."
    (when-let* ((frame (frame-local-getq company-box-doc-frame))
                ((frame-live-p frame)))
      (unless (window-live-p (frame-root-window frame))
        (let (
              ;; NOTE: we should ensure that no run frame deletion
              ;; hooks since we're just kill it. For more precisely
              ;; that `company-box--kill-buffer' will be invoked via
              ;; that way which is not what we expecting for.
              delete-frame-functions)
          (delete-frame frame t)
          (frame-local-setq company-box-doc-frame nil)))))
  (advice-add 'company-box-doc--show
              :before #'__ya/company-box-doc--show/neglect-patch/0)

;; ******** company box main core patch
;; ********* company box hide/show patch

  (defun __ya/company-box-hide/light-ver (&optional no-run-hook)
    "light version of `company-box-hide' but without make frame
invisible.

This function exists since we want to let company-box known that the
frame is hided so that it can create new frame position cache without
collisions caused by eemacs company's idle port modification and
reducing lag caused by `make-frame-invisible'."
    ;; EEMACS_MAINTENANCE: follow `company-box-hide's updates
    (setq company-box--bottom nil
          company-box--x nil
          company-box--prefix-pos nil
          company-box--last-start nil
          company-box--edges nil)
    ;; we should do this since this is a fake hiding method
    (setq __company-box-main-frame-hided-p nil)
    (with-current-buffer (company-box--get-buffer)
      (setq company-box--last-start nil))
    (remove-hook 'window-scroll-functions 'company-box--handle-scroll-parent t)
    (unless no-run-hook
      (run-hook-with-args
       'company-box-hide-hook
       (or (frame-parent) (selected-frame)))))

  (defun __ya/company-box-hide (orig-func &rest orig-args)
    (when (or __company-box-main-frame-need-force-hide-p
              (not __company-box-main-frame-hided-p))
      (prog1
          (apply orig-func orig-args)
        (setq __company-box-main-frame-hided-p t)
        (if __company-box-main-frame-need-force-hide-p
            (let ((__company-box-doc-need-force-hide-p t))
              (company-box-doc--hide))))))
  (advice-add 'company-box-hide :around #'__ya/company-box-hide)

  (defun __ya/company-box-show (orig-func &rest orig-args)
    (unless __company-box-main-frame-hided-p
      (__ya/company-box-hide/light-ver))
    ;; NOTE: we should whatever do this firstly since for preventing
    ;; messy.
    (setq __company-box-main-frame-hided-p nil)
    (apply orig-func orig-args))
  (advice-add 'company-box-show :around #'__ya/company-box-show)

  (defun __company-box-main-frame-displayed-flag-set (&rest _)
    (setq __company-box-doc-hided-p nil))
  (dolist (func '(company-box--update company-box--select-mouse))
    (advice-add func :after #'__company-box-main-frame-displayed-flag-set))

;; ******* frame hide/show robust

  (defun __ya/company-box-hide-or-show/robust (orig-func &rest orig-args)
    (let ((inhibit-quit t))
      (condition-case-unless-debug err
          (apply orig-func orig-args)
        (error
         (let ((inhibit-quit t))
           (entropy/emacs-company--cmpbox-with-demoted-errors
               "[__ya/company-box-hide-or-show/robust internal] [1] %S"
             (__ya/company-box-hide/light-ver t))
           (entropy/emacs-company--cmpbox-with-demoted-errors
               "[__ya/company-box-hide-or-show/robust internal] [2] %S"
             (when (fboundp 'company-box--get-buffer)
               (let ((buff (company-box--get-buffer)))
                 (with-current-buffer buff
                   (setq company-box--last-start nil)))))
           (entropy/emacs-company--cmpbox-del-frames)
           (dolist (var '(__company-box-doc-hided-p __company-box-main-frame-hided-p))
             (set var nil))
           (entropy/emacs-company--cmpbox-with-demoted-errors
               "[__ya/company-box-hide-or-show/robust internal] [3] %S"
             (dolist (buff (buffer-list))
               (with-current-buffer buff
                 (when (and (entropy/emacs-operation-status/running-auto-completion-op-p)
                            (memq 'company-box-frontend company-frontends))
                   (company-abort)))))
           (entropy/emacs-company--cmpbox-silent-error
            "[company-box hide/show] error: %S" err))))))
  (dolist (func '(company-box-hide
                  company-box-show
                  company-box--update
                  company-box--move-selection
                  company-box-doc--show
                  company-box-doc--hide
                  ;; the internal handles
                  company-box--handle-scroll
                  company-box--handle-window-changes
                  company-box--handle-scroll-parent
                  company-box--handle-theme-change))
    (advice-add func
                :around #'__ya/company-box-hide-or-show/robust))

;; ******* Debug
  (defun entropy/emacs-company-box-delete-all-child-frames
      (&optional _arg)
    "[Debug only] Delete all company-box child frames."
    (interactive "P")
    (entropy/emacs-company--cmpbox-del-frames))

;; ***** loading patch

  ;; HACK: disable remapping since we bind it manually and Because
  ;; EEMACS_BUG: `company-box-doc' permanently remap
  ;; `company-show-doc-buffer' to its spec, so that any
  ;; `company-show-doc-buffer' binding will redirect to the
  ;; `company-box-doc-manually' which is a messy.
  (when (equal
         (lookup-key company-active-map
                     [remap company-show-doc-buffer])
         'company-box-doc-manually)
    (define-key company-active-map
      [remap company-show-doc-buffer]
      nil))
  )

;; *** Better sorting and filtering

;; TODO: hack its performance to fit fast typing with `company-mode'.
(use-package company-prescient
  :after company
  :commands (company-prescient-mode)
  :init
  ;; EEMACS_BUG: 2021-09-17 Fri 18:12:03
  ;; we disable it temporally since its performance issue.
  ;; (company-prescient-mode 1)
  )

;; *** Company in minibuffer

;; Stolen from https://gist.github.com/Bad-ptr/7787596#file-company-minibuffer-el
(entropy/emacs-lazy-load-simple 'company
  (defvar-local entropy/emacs-company--minibuffer-command nil)

  (defun entropy/emacs-company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion backend for Emacs Lisp in the
minibuffer."
    (interactive (list 'interactive) company-mode)
    (cond
     ((eq command 'prefix)
      (and (minibufferp)
           (company-grab-symbol)))
     ((eq command 'candidates)
      (cl-case entropy/emacs-company--minibuffer-command
        (execute-extended-command (all-completions arg obarray 'commandp))
        (t (apply 'company-capf 'candidates arg ignored))))
     ((not (eq entropy/emacs-company--minibuffer-command 'execute-extended-command))
      (apply 'company-capf command arg ignored))))

  (defun entropy/emacs-active-minibuffer-company-elisp ()
    "Active `company-mode' in minibuffer only for elisp
completion when calling: 'execute-extended-command' or
'eval-expression'."
    (when (bound-and-true-p company-mode)
      (company-mode 0))
    (when (and global-company-mode
               (or
                ;; commands filters to use `company-mode' in minibuffer
                (eq this-command #'execute-extended-command)
                (eq this-command #'eval-expression)
                (eq this-command #'eldoc-eval-expression)
                ;; TODO: add more filters
                ))

      (setq-local entropy/emacs-company--minibuffer-command
                  this-command)

      (setq-local completion-at-point-functions
                  (list (if (fboundp 'elisp-completion-at-point)
                            #'elisp-completion-at-point
                          #'lisp-completion-at-point)
                        t))

      (entropy/emacs-company--set-backends
       (cond ((eq entropy/emacs-company--minibuffer-command 'execute-extended-command)
              '(entropy/emacs-company-elisp-minibuffer))
             (t '(entropy/emacs-company-elisp-minibuffer company-dabbrev))))
      (setq-local company-tooltip-limit 6)
      (company-mode 1)

      ;; We just use overlay render tooltip type because other
      ;; child-frame ones can not show those frame at point of oneline
      ;; height minibuffer window
      (when (bound-and-true-p company-box-mode)
        (company-box-mode 0))
      (when (bound-and-true-p company-posframe-mode)
        (company-posframe-mode 0))

      ;; set internal tooltip
      (setq-local company-frontends
                  '(company-pseudo-tooltip-frontend))
      ))

  (add-hook 'minibuffer-setup-hook
            #'entropy/emacs-active-minibuffer-company-elisp)
  )

;; ** company-lsp
(entropy/emacs-lazy-load-simple 'lsp-mode
  (advice-add 'lsp
              :after
              #'entropy/emacs-company-add-lsp-backend))
(entropy/emacs-lazy-load-simple 'eglot
  (dolist (func '(eglot eglot-ensure))
    (advice-add func
                :after
                #'entropy/emacs-company-add-lsp-backend)))

(defun entropy/emacs-company-add-lsp-backend (&rest _)
  (setq-local completion-ignore-case t
              completion-styles (list 'basic 'partial-completion 'emacs22))
  (entropy/emacs-company--set-backends
   '(company-capf
     :separate company-dabbrev-code company-keywords
     :with company-yasnippet)))

;; ** Individual backends
(defun entropy/emacs-company--default-traditional-backends-generator
    (stick-backends)
  (entropy/emacs-company--set-backends
   `(company-capf
     ,@stick-backends
     :separate
     company-dabbrev-code
     company-gtags
     company-etags
     company-keywords
     :with company-yasnippet
     )))

;; *** miscelloneous
;; **** englishs dict quick completion
(use-package company-en-words
  :after company
  :ensure nil
  :commands company-en-words
  :init
  (with-eval-after-load 'company-box
    (defun entropy/emacs-company--company-en-words-icons-for-company-box
        (_candi)
      "Common text icon view for non-matched candi of dev env so
that for en-words candi recognized "
      'Text)
    (setq company-box-icons-functions
          (append company-box-icons-functions
                  '(entropy/emacs-company--company-en-words-icons-for-company-box)))
    ;; add specific backends face in box
    (add-to-list 'company-box-backends-colors
                 '(company-en-words
                   :all "DarkOrange"
                   :selected
                   (:background "gray" :foreground "black")
                   :annotation
                   (:foreground "yellow")))
    )
  :config
  (entropy/emacs-company--make-auto-handy-backend
    company-en-words))

;; *** shell
(use-package company-shell
  :if (or (eq (entropy/emacs-get-use-ide-type 'sh-mode) 'traditional)
          entropy/emacs-ide-suppressed)
  :after company
  :commands (company-shell company-shell-env company-fish-shell)
  :init
  (entropy/emacs-lazy-load-simple 'sh-script
    (defun entropy/emacs-company--set-company-backends-for-sh-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-shell company-shell-env)))
    (add-hook
     'sh-mode-hook
     #'entropy/emacs-company--set-company-backends-for-sh-mode-in-traditional-way)))

;; *** web refer
;; **** web/html&css
(use-package company-web
  :after company
  :commands company-web
  :init

  (autoload (function company-web-html) "company-web-html" nil t)
  (autoload (function company-web-jade) "company-web-jade" nil t)
  (autoload (function company-web-slim) "company-web-slim" nil t)

  (entropy/emacs-lazy-load-simple 'web-mode
    (when (eq (entropy/emacs-get-use-ide-type 'web-mode) 'traditional)
      (defun entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way
          ()
        (entropy/emacs-company--default-traditional-backends-generator
         '(company-web-html)))
      (add-hook
       'web-mode-hook
       #'entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way)))

  (entropy/emacs-lazy-load-simple 'css-mode
    (when (eq (entropy/emacs-get-use-ide-type 'css-mode) 'traditional)
      (defun entropy/emacs-company--set-company-backends-for-css-mode-in-traditional-way
          ()
        (entropy/emacs-company--default-traditional-backends-generator
         '(company-css)))
      (add-hook
       'css-mode-hook
       #'entropy/emacs-company--set-company-backends-for-css-mode-in-traditional-way)))

  )

;; **** javascript
(use-package company-tern
  :ensure nil
  ;; FIXME: it using `dash-functional' which will cause obsolete
  ;; warning within byte-compiling
  :no-require t
  :if (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
  :after company
  :commands company-tern
  :init
  (entropy/emacs-lazy-load-simple 'js2-mode
    (defun entropy/emacs-company--set-company-backends-for-js2-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-tern)))
    (add-hook
     'js2-mode-hook
     #'entropy/emacs-company--set-company-backends-for-js2-mode-in-traditional-way))

  :config
  (defun entropy/emacs-company-create-tern-project-file (&rest _)
    "Auto create '.tern-project' file in current dir.

Notice: this automatically created file was simple, you should
modify it by personal customization.

And this automatically created file was the file within
entropy-emacs."
    (let ((tern-template (expand-file-name ".tern-project" entropy/emacs-templates-dir)))
      (when (buffer-file-name)
        (unless (file-exists-p (expand-file-name ".tern-project" default-directory))
          (if (file-exists-p tern-template)
              (progn
                (copy-file tern-template
                           (expand-file-name ".tern-project" default-directory))
                (message "Succeed to create .tern-project in this folder!"))
            (message "Can not find origin .tern-project file from %s"
                     (file-name-directory tern-template)))))))

  (advice-add 'company-tern :before
              #'entropy/emacs-company-create-tern-project-file))

;; **** php
(use-package company-php
  :if (eq (entropy/emacs-get-use-ide-type 'php-mode) 'traditional)
  :commands company-ac-php-backend
  :init
  (entropy/emacs-lazy-load-simple 'php-mode
    (defun entropy/emacs-company--set-company-backends-for-php-mode-in-traditional-way ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-ac-php-backend)))
    (add-hook
     'php-mode-hook
     #'entropy/emacs-company--set-company-backends-for-php-mode-in-traditional-way)))

;; *** C(PP) Java python
;; **** C(PP)
;; ***** headers
(use-package company-c-headers
  :after company
  :commands company-c-headers)

;; ***** company irony
(use-package company-irony
  :after company
  :commands commpany-irony)

;; ***** main

(defun entropy/emacs-company--set-company-backends-for-C-mode-in-traditional-way
    ()
  (entropy/emacs-company--default-traditional-backends-generator
   '(company-c-headers company-irony)))

(entropy/emacs-lazy-load-simple 'cc-mode
  (dolist (item '((c-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c-mode) 'traditional))
                  (c++-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c++-mode) 'traditional))))
    (when (entropy/emacs-eval-with-lexical (cdr item))
      (add-hook
       (car item)
       #'entropy/emacs-company--set-company-backends-for-C-mode-in-traditional-way
       ))))


;; **** Java
;; **** Python
(use-package company-anaconda
  :if (eq (entropy/emacs-get-use-ide-type 'python-mode) 'traditional)
  :after company
  :commands company-anaconda
  :init
  (entropy/emacs-lazy-load-simple 'python
    (defun entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-anaconda)))
    (add-hook
     'anaconda-mode-hook
     #'entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way))
  :config

  ;; Use proxy for bootstrap anaconda-mode for the sake for installing
  ;; server specs
  (with-eval-after-load 'anaconda-mode
    (advice-add 'anaconda-mode-bootstrap
                :around
                #'entropy/emacs-advice-for-common-do-with-http-proxy)))

;; *** common lisp
;; slime repl completion
(use-package slime-company
  :after slime
  :commands (company-slime slime-company-doc-mode)
  :init
  (add-to-list 'slime-contribs 'slime-company)
  (add-hook 'slime-mode-hook
            #'entropy/emacs-company-slime-add-company-slime-backend)
  (add-hook 'slime-repl-mode-hook
            #'entropy/emacs-company-slime-add-company-slime-backend)
  (defun entropy/emacs-company-slime-add-company-slime-backend ()
    (entropy/emacs-company--set-backends
     (cons 'company-slime entropy/emacs-company-top-initial-backends))))

;; * provide
(provide 'entropy-emacs-company)
