;;; entropy-emacs-company.el --- entropy emacs completion config
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
(defvar entropy/emacs-company-top-initial-backends
  '(company-files company-capf :separate company-en-words)
  "Basic top company-backend for all situations.")

(defvar entropy/emacs-company-max-candidates 12
  "The companys max canids length restriction.")

(defvar entropy/emacs-company-internal-pseudo-frontends
  '(company-pseudo-tooltip-frontend
    company-pseudo-tooltip-unless-just-one-frontend
    company-pseudo-tooltip-unless-just-one-frontend-with-delay)
  "The `company-mode' internal popup frontends collection.

NOTE: update with `company' upstream.")

(entropy/emacs-lazy-load-simple company
  (setq-default company-backends
                (append (list entropy/emacs-company-top-initial-backends)
                        company-backends)))

;; ** libraries
;; *** yas load
(defun entropy/emacs-company-use-yasnippet (backend &optional reverse)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (if reverse
        (append
         '(company-yasnippet) '(:separate)
         (if (consp backend) backend (list backend)))
      (append
       (if (consp backend) backend (list backend))
       '(:separate company-yasnippet)))))

(defun entropy/emacs-company-start-with-yas (&rest _)
  (unless (bound-and-true-p yas-minor-mode)
    (unless (fboundp 'yas-minor-mode)
      (require 'yasnippet))
    (yas-minor-mode 1)))

;; *** company for docs modes

(defun entropy/emacs-company-privilege-yas-for-docs ()
  "Make `company-backends' suitable for documents edits with
yasnippet support *locally*."
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends
               '(company-files company-yasnippet company-dabbrev
                               :separate company-en-words)))

(defun entropy/emacs-company-yas-for-docs-init ()
  (let (macros)
    (dolist (el '((org-mode . org-mode-hook)
                  (mardown-mode . markdown-mode-hook)
                  (text-mode . text-mode-hook)))
      (add-to-list
       'macros
       `(lambda ()
          (with-eval-after-load ',(car el)
            (add-hook ',(cdr el)
                      #'entropy/emacs-company-privilege-yas-for-docs)))))
    (dolist (macro macros)
      (funcall macro))))


;; *** Candidates length restriction

(defvar-local entropy/company--this-candi-length nil)

(defun entropy/emacs-company--company-candis-restrict-advice
    (orig-func &rest orig-args)
  "Restrict company content to reduce lagging feels.

NOTE: this function is an around advice wrapper."
  (let* ((company-candidates
          (-take entropy/emacs-company-max-candidates
                 company-candidates))
         (company-candidates-length
          (setq entropy/company--this-candi-length
                (length company-candidates))))
    (apply orig-func orig-args)))

(defun entropy/emacs-company--company-candis-length-follow-advice
    (orig-func &rest orig-args)
  "Follow candis length via `entropy/company--this-candi-length'.

NOTE: this function is an around advice wrapper."
  (let* ((company-candidates-length
          entropy/company--this-candi-length))
    (apply orig-func orig-args)))


;; *** prunning `company-frontends'

(defun entropy/emacs-company--set-only-one-frontend
    (only-one)
  (setq company-frontends (list only-one)))

(defun entropy/emacs-company--set-only-one-frontend-all-buffers
    (only-one)
  (dolist (buff (buffer-list))
    (when (bound-and-true-p company-mode)
      (entropy/emacs-company--set-only-one-frontend
       only-one))))

;; ** company core
(use-package company
  ;; :diminish company-mode  ;;; This comment to diminish the modline
  :commands (global-company-mode
             company-mode
             company-complete
             company-files)

;; *** preface
  :preface

  (defun entropy/emacs-company-toggle-idledelay (&optional prefix)
    "Toggle `company-idle-delay' on/off.

When PREFIX, set the specfied idle delay seconds for as or use
`entropy/emacs-company-idle-delay-default' as default."
    (interactive "P")
    (let ((def-safe-idle-secs
            (max (+ 0.1 entropy/emacs-safe-idle-minimal-secs)
                 entropy/emacs-company-idle-delay-default)))
      (if (and (not prefix)
               (bound-and-true-p company-idle-delay))
          (progn (setq company-idle-delay nil)
                 (message "turn off `company-idle-delay'"))
        (setq company-idle-delay
              (if prefix
                  (let ((secs (string-to-number
                               (read-string "Input Company delay secs: "))))
                    (if (and (numberp secs)
                             (> secs entropy/emacs-safe-idle-minimal-secs))
                        secs
                      (message "Invalid company-delay secs '%s'" secs)
                      def-safe-idle-secs))
                def-safe-idle-secs))
        (let ((entropy/emacs-message-non-popup t))
          (entropy/emacs-message-do-message
           "%s '%s' to '%s'"
           (blue "Set")
           (yellow (symbol-name 'company-idle-delay))
           (red (number-to-string company-idle-delay)))))))

  (defun entropy/emacs-company-files (command)
    "Like `company-files' but using as individual eemacs command with
eemacs specifications"
    (interactive (list 'interactive))
    (unless
        ;; some filters not to triggered
        (or buffer-read-only
            ;; TODO: more
            )
      (unless (bound-and-true-p company-mode)
        (company-mode))
      (company-files command)))

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
     ("M-\\" company-dabbrev "dabbrev-like 'company-mode' completion backend"
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
    (dolist (func '(company-idle-begin company-complete))
      (advice-add func
                  :before 'entropy/emacs-company-start-with-yas))
    (entropy/emacs-company-yas-for-docs-init)
    (entropy/emacs-company-toggle-frontend
     entropy/emacs-company-tooltip-use-type))
  (entropy/emacs-lazy-initial-advice-before
   (find-file switch-to-buffer)
   "global-company-mode-init" "global-company-mode-init"
   prompt-echo
   ;; We must ensure the `global-company-mode' enabled in pdumper
   ;; recovery hook since the `company-mode-on' need `noninteractive'
   ;; is null.
   :pdumper-no-end nil
   (entropy/emacs-company-global-init))
  (entropy/emacs-lazy-load-simple (company counsel)
    (define-key company-active-map (kbd "M-o") 'counsel-company))

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
   company-tooltip-limit 20  ; bigger popup window
   company-tooltip-maximum-width 70
   company-tooltip-minimum-width 20
   company-tooltip-align-annotations t
   company-tooltip-offset-display nil   ;reducing selection fast hints laggy
   company-idle-delay entropy/emacs-company-idle-delay-default
   company-dabbrev-code-everywhere t    ;NOTE: this may make emacs lag
   company-dabbrev-ignore-case t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   )

  ;; FIXME: related to bug of h-f551b679-908f-4b64-b08e-e7074d17581e
  ;; NOTE: Do not set this var too complicated which will make emacs
  ;; lag since `company-dabbrev' use `looking-back' to search
  ;; matching.
  (setq company-dabbrev-char-regexp "[-_/a-zA-Z0-9.><]")

  ;; disable common command before which the company begun to run
  ;; EEMACS_MAINTENANCE: the variable `company--bein-inhibit-commands'
  ;; is not the common API!
  (dolist (command '(previous-line
                     next-line
                     left-char
                     right-char))
    (add-to-list 'company--begin-inhibit-commands
                 command))

;; **** advices
;; ***** restrict company candidates length
  (advice-add 'company-pseudo-tooltip-frontend
              :around
              #'entropy/emacs-company--company-candis-restrict-advice)
  (advice-add 'company-set-selection
              :around
              #'entropy/emacs-company--company-candis-length-follow-advice)


;; ***** `company-post-command' idle trigger

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/company-post-command ()
    "Yet another `company-post-command' which run with
`entropy/emacs-run-at-idle-immediately' so that the sequentially
fast hints not laggy by `candidates' re-calculation."
    (when (or (and company-candidates
                   (null this-command))
              (eq this-command 'keyboard-quit))
      ;; Happens when the user presses `C-g' while inside
      ;; `flyspell-post-command-hook', for example.
      ;; Or any other `post-command-hook' function that can call `sit-for',
      ;; or any quittable timer function.
      (company-abort)
      (setq this-command 'company-abort))
    (entropy/emacs-run-at-idle-immediately
     __idle/company-post-command
     :which-hook 0.3
     :idle-when
     ;; TODO: complete the precise conditions
     (let ((special_key_p (member this-command
                                  '(
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
                                    ;; `company-search-map' hints
                                    company-search-other-char
                                    company-search-delete-char
                                    company-search-abort
                                    company-search-repeat-forward
                                    company-search-repeat-backward
                                    company-search-toggle-filtering
                                    company-search-printing-char
                                    )))
           (candi_exist_p (bound-and-true-p company-candidates)))
       (cond
        (special_key_p nil)
        (candi_exist_p t)
        (t nil)))
     (let ((this-command (if (bound-and-true-p entropy/emacs-current-session-is-idle-p)
                             entropy/emacs-current-session-this-command-before-idle
                           this-command))
           (last-command (if (bound-and-true-p entropy/emacs-current-session-is-idle-p)
                             entropy/emacs-current-session-last-command-before-idle
                           last-command)))
       (unless (company-keep this-command)
         ;; we just use `condition-case' since run within idle trigger
         ;; wrapper as it has its own debug facilities.
         (condition-case err
             (progn
               (unless (equal (point) company-point)
                 (let (company-idle-delay) ; Against misbehavior while debugging.
                   (company--perform)))
               (if company-candidates
                   (company-call-frontends 'post-command)
                 (let ((delay (company--idle-delay)))
                   (and (numberp delay)
                        (not defining-kbd-macro)
                        (company--should-begin)
                        (if (current-idle-time)
                            (funcall
                             'company-idle-begin
                             (current-buffer) (selected-window)
                             (buffer-chars-modified-tick) (point))
                          (when (timerp company-timer)
                            (cancel-timer company-timer))
                          (setq company-timer nil)
                          (setq company-timer
                                (run-with-timer
                                 delay nil
                                 'company-idle-begin
                                 (current-buffer) (selected-window)
                                 (buffer-chars-modified-tick) (point))))
                        ))))
           (error (message "Company: An error occurred in post-command")
                  (message "%s" (error-message-string err))
                  (company-cancel))))
       (company-install-map))))

  (advice-add 'company-post-command
              :override
              #'__ya/company-post-command)



;; ***** fly on type for `delete-char'
  (defvar-local __company-delc-time-host nil)
  (defun __company-delc-time-fly-p ()
    "Judge whether the `delete-char' hit on flying while company
activated status. Default time during set is less than 70ms."
    (catch :exit
      (let* ((time-old (or __company-delc-time-host
                           (throw :exit nil)))
             (time-now (current-time))
             (duration (abs
                        (- (caddr time-now)
                           (caddr time-old)))))
        (and
         (equal (cadr time-now) (cadr time-old))
         (< duration
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
          (when (bound-and-true-p company-mode)
            (company-abort))))
      (setq rtn (apply orig-func orig-args)
            __company-delc-time-host
            (current-time))
      rtn))
  (advice-add 'delete-char :around #'__company-delete-char)

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
  (advice-add 'company-call-backend
              :around
              #'entropy/emacs-company--pseudo-no-annotation)

;; ****** pseudo frontend subroutine performance optimization
;; ******* simplify the pseudo candi line overlay generator
  (defun __ya/company-fill-propertize (value annotation width selected left right)
    "The simplify `company-fill-propertize'.

EEMACS_MAINTENANCE: stick to upstream udpate"
    (let* ((margin (length left))
           (common (or (company-call-backend 'match value)
                       (if company-common
                           (string-width company-common)
                         0)))
           (_ (setq value (company-reformat (company--pre-render value))
                    ;; annotation nil
                    ))
           (line (concat left
                         (company-safe-substring
                          value 0 width)
                         right)))
      (setq width (+ width margin (length right)))
      (when selected
        (add-face-text-property 0 width 'company-tooltip-selection t line))
      (add-face-text-property 0 width 'company-tooltip t line)
      line))

  (advice-add
   'company-fill-propertize
   :override
   #'__ya/company-fill-propertize)

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
      (when (and header-line-format (version< emacs-version "24.3.93.3"))
        ;; http://debbugs.gnu.org/18384
        (cl-decf row))
      (when (bound-and-true-p display-line-numbers)
        (cl-decf col (+ 2 (line-number-display-width))))
      (cons (+ col (window-hscroll)) row)))

  (advice-add
   'company--posn-col-row
   :override
   #'__ya/company--posn-col-row)

;; ***** `company-show-doc-buffer' conflicts hack

  (defun __ya/company-show-doc-buffer ()
    "Redefine of `company-show-doc-buffer' since its conflicted
with `shackle'."
    (interactive)
    (let ((other-window-scroll-buffer)
          (selection (or company-selection 0))
          (orig-win (selected-window)))
      (let* ((selected (nth selection company-candidates))
             (doc-buffer (company-call-backend 'doc-buffer selected)))
        (when (consp doc-buffer)
          (setq doc-buffer (car doc-buffer)))
        (if (or (and (bufferp doc-buffer)
                     (buffer-live-p doc-buffer))
                ;; or the buffer is `buffer-name' then we also getted
                (and (stringp doc-buffer)
                     (bufferp (get-buffer doc-buffer))))
            (let (_)
              (with-current-buffer doc-buffer
                (goto-char (point-min)))
              (with-selected-window orig-win
                (company-abort))
              (display-buffer doc-buffer))
          (message "can not fetch doc of current selection '%s'"
                   selected)))))
  (advice-add 'company-show-doc-buffer
              :override
              #'__ya/company-show-doc-buffer)

  )

;; *** company components function autoload
(use-package company-dabbrev   :ensure nil :after company :commands company-dabbrev)
(use-package company-files     :ensure nil :after company :commands company-files)
(use-package company-yasnippet :ensure nil :after company :commands company-yasnippet)

;; ** company enhancement
;; *** toggle framework

(eval-and-compile
  (defvar entropy/emacs-company--frontend-register nil)
  (defvar entropy/emacs-company--frontend-daemon-current-hook nil)
  (defvar entropy/emacs-company-frontend-sticker nil))

(defun entropy/emacs-company-toggle-frontend (type)
  (interactive
   (list (intern
          (completing-read "Choose valid company frontend: "
                           (mapcar (lambda (x) (symbol-name (car x)))
                                   entropy/emacs-company--frontend-register)))))
  (let* ((cur-type entropy/emacs-company-frontend-sticker)
         (cur-disable
          (plist-get
           (alist-get cur-type entropy/emacs-company--frontend-register)
           :disable))
         (chose-enable
          (plist-get
           (alist-get type entropy/emacs-company--frontend-register)
           :enable))
         (daemon-init
          (plist-get
           (alist-get type entropy/emacs-company--frontend-register)
           :daemon-init)))
    (when (and cur-type (not (functionp cur-disable)))
      (error "Can not find associated disable function for company frontend '%s'"
             cur-type))
    (unless (functionp chose-enable)
      (error "Can not find associated enable function for company frontend '%s'"
             type))

    (when (symbolp daemon-init)
      ;; wrap daemon init to a form
      (setq daemon-init `(funcall ,daemon-init)))

    (progn
      (when cur-type
        ;; disable current frontend injections
        (funcall cur-disable)
        ;; remove daemon init hook for current frontend type
        (when entropy/emacs-company--frontend-daemon-current-hook
          (remove-hook 'entropy/emacs-daemon-server-after-make-frame-hook
                       entropy/emacs-company--frontend-daemon-current-hook)))
      (cond (
             ;; is daemon and inited
             (and entropy/emacs-daemon-server-init-done (daemonp))
             (message "Enable company frontend type '%s' after daemon make frame ..."
                      type)
             (funcall chose-enable)
             (setq entropy/emacs-company--frontend-daemon-current-hook
                   (eval daemon-init)))
            (
             ;; is daemon but not inited
             (and (not entropy/emacs-daemon-server-init-done) (daemonp))
             (message "Planning to enable company frontend type '%s' after daemon make frame ..."
                      type)
             (setq entropy/emacs-company--frontend-daemon-current-hook
                   (eval daemon-init)))
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
  :bind (:map company-active-map
              ("<f1>" . nil))
  :preface

  (defun entropy/emacs-company--default-frontend-set-hook
      (&rest _)
    (entropy/emacs-company--set-only-one-frontend
     'company-pseudo-tooltip-frontend))

  (defun entropy/emacs-company--default-enable ()
    ;; just use one frontends for reduce lagging
    (add-hook 'company-mode-hook
              #'entropy/emacs-company--default-frontend-set-hook)
    ;; travel all buffers with `company-mode' enabled to set frontend.
    (entropy/emacs-company--default-frontend-set-hook)
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
                   'company-default-mode
                   '(entropy/emacs-company--default-disable)
                   '(entropy/emacs-company--default-enable))))

  :init
  (setq company-quickhelp-delay
        entropy/emacs-company-quickhelp-delay-default
        company-quickhelp-use-propertized-text t))

;; *** Company-box config

(use-package company-box
  :after company
  :commands (company-box-mode)

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
        'company-show-doc-buffer)))

  (defun entropy/emacs-company--box-enable ()
    (cond ((entropy/emacs-posframe-adapted-p)
           ;; just use one frontends for reduce lagging
           (add-hook 'company-mode-hook
                     #'entropy/emacs-company--company-box-frontend-set-hook)
           ;; travel all buffers with `company-mode' enabled to set frontend.
           (entropy/emacs-company--set-only-one-frontend
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
                              'company-box-mode
                              '(entropy/emacs-company--box-disable)
                              '(entropy/emacs-company--box-enable))))

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
          (setq x-gtk-resize-child-frames 'hide))))

  (advice-add 'company-box-show
              :around
              #'entropy/emacs-company--company-candis-restrict-advice)
  (advice-add 'company-box--update
              :around
              #'entropy/emacs-company--company-candis-length-follow-advice)

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

;; ***** company box doc hide when fly on editing

  (defvar-local __local-company-box-doc-hided nil)
  (defun __company-doc-show-timer-func (selection frame)
    "`company-box-doc' delay show core subroutine.

EEMACS_MAINTENANCE: may need update with company-box upstream."
    (company-box-doc--show selection frame)
    (company-ensure-emulation-alist))
  (defun company-box-doc (selection frame)
    "NOTE: this function has been redefined for adapting to
eemacs.

Patched for: Run doc frame hide just one time when consecutively
fast hints.

The origin `company-box-doc' has preparation with doc frame
visible checking with function `frame-local-get' which mapped
local frame obarray to search local varaible which has bad
benchmark performance as an scroll lagging reason when toggle on
`company-box-doc-enable'.

EEMACS_MAINTENANCE: need patching updately with `company-box'
upstream."
    (when company-box-doc-enable
      (eval
       `(progn
          (unless __local-company-box-doc-hided
            (let ((doc-frame
                   (frame-local-getq company-box-doc-frame
                                     ,frame)))
              (when (and doc-frame
                         (framep doc-frame)
                         (frame-live-p frame)
                         (frame-visible-p doc-frame)
                         ;; to ensuer is not the root frame
                         (framep (frame-parent doc-frame)))
                (make-frame-invisible doc-frame))
              (setq __local-company-box-doc-hided t)))
          (when (timerp company-box-doc--timer)
            (cancel-timer company-box-doc--timer)
            (cancel-function-timers '__company-doc-show-timer-func)
            (setq company-box-doc--timer nil))
          (setq company-box-doc--timer
                (run-with-timer
                 company-box-doc-delay nil
                 #'__company-doc-show-timer-func
                 ',selection ,frame))))))

  (defun __local-company-box-doc-hided-rest
      (orig-func &rest orig-args)
    (let ((rtn (apply orig-func orig-args)))
      (setq __local-company-box-doc-hided nil)
      rtn))
  (advice-add 'company-box-doc--show
              :around #'__local-company-box-doc-hided-rest)

  (defun __company-box-doc-hide--with-local-judge
      (orig-func &rest orig-args)
    (unless __local-company-box-doc-hided
      (let ((rtn
             (apply orig-func orig-args)))
        (setq __local-company-box-doc-hided t)
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
        (cond
         ((bound-and-true-p company-box-mode)
          (add-hook 'pre-command-hook
                    #'__ya_company-box-doc-hide nil t))
         (t
          (add-hook 'pre-command-hook
                    #'__ya_company-box-doc-hide nil t)))
      (remove-hook 'pre-command-hook
                   #'__ya_company-box-doc-hide t)))
  (add-hook 'company-box-mode-hook
            #'entropy/emacs-company-box-hide-on-the-fly)

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
      ;; HACK: do not make `company-frontends' local
      ;; (make-local-variable 'company-frontends)
      (mapc (lambda (x) (setq company-frontends
                              (delete x company-frontends)))
            entropy/emacs-company-internal-pseudo-frontends)
      (add-to-list 'company-frontends 'company-box-frontend)
      (unless (assq 'company-box-frame frameset-filter-alist)
        (push '(company-box-doc-frame . :never) frameset-filter-alist)
        (push '(company-box-frame . :never) frameset-filter-alist)))
     ((memq 'company-box-frontend company-frontends)
      (setq company-frontends (delq 'company-box-frontend company-frontends))
      (add-to-list 'company-frontends 'company-pseudo-tooltip-unless-just-one-frontend))))

  (advice-add 'company-box--set-mode
              :override
              #'__ya/company-box--set-mode)

;; ****** company-box child-frame patch

;; ******* bug fix
  (defun __ya/company-box--get-frame
      (orig-func &rest orig-args)
    (let ((frame (apply orig-func orig-args)))
      (when (ignore-errors (frame-live-p frame))
        frame)))
  (advice-add 'company-box--get-frame
              :around
              #'__ya/company-box--get-frame)

;; ******* core patch

;; ******* recreate child frame after theme load
  (defun __company-box-remove-child-frame/before-load-theme ()
    "Remove company-box's frame before load a new theme since the we
don't set a proper method to let preserved old frame use the new
theme face spces."
    (let ((frame (company-box--get-frame)))
      (when (frame-live-p frame)
        (when (bound-and-true-p company-candidates)
          (company-abort))
        (delete-frame frame)
        (frame-local-setq company-box-frame nil))))
  (add-hook 'entropy/emacs-theme-load-before-hook
            #'__company-box-remove-child-frame/before-load-theme)

;; ******* set child frame indicator
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

;; ******* frame font spec
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

;; ******* Debug
  (defun entropy/emacs-company-box-delete-all-child-frames
      (&optional arg)
    "[Debug only] Delete all company-box child frames."
    (interactive "P")
    (let ((frame-list (frame-list)))
      (dolist (frame frame-list)
        (when (and (frame-live-p frame)
                   (frame-parameter
                    frame
                    'this-company-box-frame-p))
          (entropy/emacs-message-do-message
           "Deleting company-box child-frame <%s> ..."
           (yellow frame))
          (delete-frame frame t)))))

;; ***** loading patch

  ;; HACK: disable remapping since we bind it manually and
  ;; Because
  ;; EEMACS_BUG: `company-box-doc' permanently remap
  ;; `company-show-doc-buffer' to its spec.
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
(entropy/emacs-lazy-load-simple company
  (defvar-local entropy/emacs-company--minibuffer-command nil)

  (defun entropy/emacs-company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion back-end for Emacs Lisp in the
minibuffer."
    (interactive (list 'interactive))
    (cl-case command
      ('prefix (and (minibufferp)
                    (company-grab-symbol)))
      ('candidates
       (cl-case entropy/emacs-company--minibuffer-command
         ('execute-extended-command (all-completions arg obarray 'commandp))
         (t (all-completions arg obarray))))))

  (defun entropy/emacs-active-minibuffer-company-elisp ()
    "Active `company-mode' in minibuffer only for elisp
completion when calling: 'execute-extended-command' or
'eval-expression'."
    (company-mode 0)
    (when (and global-company-mode (or
                                    (eq this-command #'execute-extended-command)
                                    (eq this-command #'eval-expression)
                                    (eq this-command #'eldoc-eval-expression)))

      (setq-local entropy/emacs-company--minibuffer-command this-command)

      (setq-local completion-at-point-functions
                  (list (if (fboundp 'elisp-completion-at-point)
                            #'elisp-completion-at-point
                          #'lisp-completion-at-point)
                        t))

      (setq-local company-show-numbers nil)
      (setq-local company-backends '((entropy/emacs-company-elisp-minibuffer
                                      company-capf)))
      (setq-local company-tooltip-limit 6)
      (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend))
      (company-mode 1)

      ;; We just use overlay render tooltip type because other
      ;; child-frame ones can not show those frame at point of oneline
      ;; height minibuffer window
      (when (bound-and-true-p company-box-mode)
        (company-box-mode 0))
      (when (bound-and-true-p company-posframe-mode)
        (company-posframe-mode 0))))

  (add-hook 'minibuffer-setup-hook
            #'entropy/emacs-active-minibuffer-company-elisp)
  )

;; ** company-lsp
(entropy/emacs-lazy-load-simple lsp-mode
  (advice-add 'lsp
              :after
              #'entropy/emacs-company-add-lsp-backend))
(entropy/emacs-lazy-load-simple eglot
  (dolist (func '(eglot eglot-ensure))
    (advice-add func
                :after
                #'entropy/emacs-company-add-lsp-backend)))

(defun entropy/emacs-company-add-lsp-backend (&rest args)
  (make-local-variable 'company-backends)
  (setq-local company-backends (remove 'company-lsp company-backends)
              completion-ignore-case t
              completion-styles '(basic partial-completion emacs22))
  (add-to-list 'company-backends
               '(company-files
                 company-capf
                 :separate company-dabbrev-code company-keywords
                 :with company-yasnippet)))

;; ** Individual backends
(defun entropy/emacs-company--default-traditional-backends-generator
    (stick-backends)
  (make-local-variable 'company-backends)
  (setq-local
   company-backends
   `((company-files
      ,@stick-backends
      :separate
      company-dabbrev-code
      company-gtags
      company-etags
      company-keywords
      :with company-yasnippet
      ))))

;; *** miscelloneous
;; **** englishs dict quick completion
(use-package company-en-words
  :after company
  :ensure nil
  :commands company-en-words
  :init
  (with-eval-after-load 'company-box
    (defun entropy/emacs-company--company-en-words-icons-for-company-box
        (candi)
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
    ))

;; *** shell
(use-package company-shell
  :if (or (eq (entropy/emacs-get-use-ide-type 'sh-mode) 'traditional)
          entropy/emacs-ide-suppressed)
  :after company
  :commands (company-shell company-shell-env company-fish-shell)
  :init
  (entropy/emacs-lazy-load-simple sh-script
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

  (entropy/emacs-lazy-load-simple web-mode
    (when (eq (entropy/emacs-get-use-ide-type 'web-mode) 'traditional)
      (defun entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way
          ()
        (entropy/emacs-company--default-traditional-backends-generator
         '(company-web-html)))
      (add-hook
       'web-mode-hook
       #'entropy/emacs-company--set-company-backends-for-web-mode-in-traditional-way)))

  (entropy/emacs-lazy-load-simple css-mode
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
  :if (eq (entropy/emacs-get-use-ide-type 'js2-mode) 'traditional)
  :after company
  :commands company-tern
  :init
  (entropy/emacs-lazy-load-simple js2-mode
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
  (entropy/emacs-lazy-load-simple php-mode
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

(entropy/emacs-lazy-load-simple cc-mode
  (dolist (item '((c-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c-mode) 'traditional))
                  (c++-mode-hook . (eq (entropy/emacs-get-use-ide-type 'c++-mode) 'traditional))))
    (when (eval (cdr item))
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
  (entropy/emacs-lazy-load-simple python
    (defun entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way
        ()
      (entropy/emacs-company--default-traditional-backends-generator
       '(company-anaconda)))
    (add-hook
     'anaconda-mode-hook
     #'entropy/emacs-company--set-company-backends-for-python-mode-in-traditional-way)))

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
    (make-local-variable 'company-backends)
    (cl-pushnew (entropy/emacs-company-use-yasnippet 'company-slime)
                company-backends)))

;; * provide
(provide 'entropy-emacs-company)
