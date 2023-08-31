;;; entropy-emacs-ivy.el ---  entropy emacs 'M-x' enhancement  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ivy.el
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
;; `entropy-emacs' using [[https://github.com/abo-abo/swiper][ivy]] framework to enhance the emacs command
;; completion interface 'M-x'.
;;
;; Based on this framework, some usefule implementation for
;; sub-feature also used in `entroy-emacs', the main one was
;; =counsel= the collection of ivy stuffs.
;;
;; The other 'M-x' ehancement framework was [[https://github.com/emacs-helm/helm][helm]], using independent
;; buffer to show the command completion unlike what did in ivy which
;; uses the mini-buffer to do so. `entropy-emacs' using =helm= to do
;; the project greping/replacing, the main reason for that the
;; mini-bufer was not adapt to do heavy things due to ivy
;; designation. As thus, the 'ag' and 'pt' eventhough the ripgrep
;; 'rg' are using helm interface in `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require

(defvar company-backend)
(defvar company-common)
(defvar company-point)
(defvar company-prefix)
(defvar company-selection)
(defvar company-candidates)
(defvar company-candidates-cache)
(defvar company-candidates-length)
(defvar company-search-string)

;; ** isearch
(use-package isearch
  :ensure nil
  :init
  ;; Never wrap searching so that we can stick on top/bottom match
  ;; place precisely.
  (setq isearch-wrap-pause nil)
  :config
  (defvar __eemacs/isearch_prefix_added_p__ nil)
  (eval-when-compile
    (defmacro eemacs/isearch--with-edit-mode-spec (&rest body)
      `(let ((isearch-message-prefix-add
              (propertize "[edit] " 'face 'warning))
             (__eemacs/isearch_prefix_added_p__ t))
         ,(entropy/emacs-macroexp-progn body))))

  (defun __ya/isearch-message-prefix (fn &rest args)
    (if (bound-and-true-p __eemacs/isearch_prefix_added_p__) (apply fn args)
      (let ((isearch-message-prefix-add
             (if (eq this-command 'isearch-edit-string)
                 (propertize
                  (substitute-command-keys "\
[type \\<minibuffer-local-isearch-map>\\[exit-minibuffer] to finish edit] ")
                  'face 'warning)
               (propertize
                ;; FIXME: `substitute-command-keys' can not choose a
                ;; specified key-binding to show while various key
                ;; binding to a same defination?
                ;;
                ;; (substitute-command-keys
                ;;  "[type \\<isearch-mode-map>\\[isearch-edit-string] to edit-string] ")
                "[type M-e to edit-string] "
                'face 'warning))))
        (apply fn args))))
  (advice-add 'isearch-message-prefix
              :around
              #'__ya/isearch-message-prefix)

  (defvar __ya/isearch-mode-for-winprop nil)
  (defun __ya/isearch-mode/main-func (fn &rest args)
    ;; only init for the top invocation of `isearch-*' commands so
    ;; that the prop not changed via incremental search.
    (unless (bound-and-true-p isearch-mode)
      (setq __ya/isearch-mode-for-winprop
            (list
             :win (selected-window)
             :win-start (window-start)
             :win-point (window-point))))
    (apply fn args))
  (advice-add 'isearch-mode
              :around #'__ya/isearch-mode/main-func)

  (defun eemacs/isearch--edit-string-with-eemacs-internal-ime ()
    (interactive)
    (eemacs/isearch--with-edit-mode-spec
     (let ((entropy/emacs-inhibit-minibuffer-setup-reset-internal-IME t))
       (minibuffer-with-setup-hook entropy/emacs-internal-IME-toggle-function
         (isearch-edit-string)))))

  (defun eemacs/isearch--Left-Char ()
    (interactive)
    (eemacs/isearch--with-edit-mode-spec
     (isearch-edit-string)))

  (defun eemacs/isearch--Right-Char ()
    (interactive)
    (eemacs/isearch--with-edit-mode-spec
     (isearch-edit-string)))

  (defun eemacs/isearch--Ctril-A ()
    (interactive)
    (eemacs/isearch--with-edit-mode-spec
     (isearch-edit-string)))

  (defun eemacs/isearch--Ctril-E ()
    (interactive)
    (eemacs/isearch--with-edit-mode-spec
     (isearch-edit-string)))

  (defun eemacs/isearch--exit-minibuffer ()
    (interactive)
    (unwind-protect (and (minibufferp) (abort-minibuffers))
      (run-with-idle-timer
       0.001 nil
       (lambda nil
         (when isearch-mode
           ;;  FIXME: why needed this?
           (discard-input)
           (unwind-protect (isearch-cancel)
             (unwind-protect
                 (when (consp __ya/isearch-mode-for-winprop)
                   (let ((win (plist-get __ya/isearch-mode-for-winprop :win))
                         (win-start (plist-get __ya/isearch-mode-for-winprop :win-start))
                         (win-pt (plist-get __ya/isearch-mode-for-winprop :win-point)))
                     (when (window-live-p win)
                       (with-selected-window win
                         (and (>= win-pt (point-max)) (<= win-pt (point-max))
                              (goto-char win-pt)))
                       (and (>= win-start (point-max)) (<= win-start (point-max))
                            (set-window-start win win-start)))))
               (setq __ya/isearch-mode-for-winprop nil))
             (if (minibufferp) (minibuffer-keyboard-quit)
               (keyboard-quit))))))))

  (define-key isearch-mode-map (kbd "ESC ESC") '#'abort-minibuffers)
  (entropy/emacs-define-key-for-C-g-and-its-malwares
   isearch-mode-map 'eemacs/isearch--exit-minibuffer)
  ;; edit string mode for arbitrary search pattern modifiation
  (define-key isearch-mode-map (kbd "M-e") 'isearch-edit-string)
  (define-key isearch-mode-map (kbd entropy/emacs-internal-ime-toggling-kbd-key)
              'eemacs/isearch--edit-string-with-eemacs-internal-ime)
  ;; either rediret common buffer operation to edit string mode to
  ;; make interaction be consistent with ivy
  (define-key isearch-mode-map (kbd "<left>")  'eemacs/isearch--Left-Char)
  (define-key isearch-mode-map (kbd "C-a")     'eemacs/isearch--Ctril-A)
  (define-key isearch-mode-map (kbd "<right>") 'eemacs/isearch--Right-Char)
  (define-key isearch-mode-map (kbd "C-e")     'eemacs/isearch--Ctril-E)
  ;; make up/down be selection jump
  (define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
  ;; either for more:
  (define-key isearch-mode-map (kbd "M-<backspace>")
              (lambda nil (interactive)
                (let ((this-command 'isearch-edit-string))
                  (isearch-edit-string))))

  (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)
  (define-key isearch-mode-map "\M-c" 'isearch-toggle-case-fold)
  (define-key isearch-mode-map "\M-r" 'isearch-toggle-regexp)

  (defun eemacs/isearch--query-replace ()
    "Using `isearch-query-replace' or `isearch-query-replace-regexp'
based on `isearch-regexp' as filter."
    (interactive)
    (let ((inhibit-read-only t)
          (set-message-functions
           ;; disable mark set message to confuse user while long time
           ;; of substitution
           (append (list (lambda (x) (if (string= "Mark set" x) t)))
                   set-message-functions))
          (fun
           (if isearch-regexp 'isearch-query-replace-regexp
             'isearch-query-replace)))
      (message "Do %s ..." fun)
      (call-interactively fun)))
  (define-key isearch-mode-map (kbd "M-q") 'eemacs/isearch--query-replace)

  (let ((map  minibuffer-local-isearch-map))
    (define-key map (kbd "RET")     #'exit-minibuffer)
    (define-key map (kbd "M-TAB")   #'isearch-complete-edit)
    (define-key map (kbd "C-s")     #'isearch-forward-exit-minibuffer)
    (define-key map (kbd "<down>")  #'isearch-forward-exit-minibuffer)
    (define-key map (kbd "C-r")     #'isearch-reverse-exit-minibuffer)
    (define-key map (kbd "<up>")    #'isearch-reverse-exit-minibuffer)
    (define-key map (kbd "C-f")     #'isearch-yank-char-in-minibuffer)
    (define-key map (kbd "<right>") #'right-char)
    (define-key map (kbd "ESC ESC") #'abort-minibuffers)
    (entropy/emacs-define-key-for-C-g-and-its-malwares
     map #'eemacs/isearch--exit-minibuffer))

  )

;; ** ivy
(use-package ivy
  :diminish ivy-mode
  :commands (ivy-mode ivy-switch-buffer ivy-resume)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         (("C-c o" . entropy/emacs-ivy-use-re-builer)))

;; *** ivy init
  :init

  ;;  disabled ivy-initial-inputs-alist

  ;; ivy initial char inserting was using for regex like searching,
  ;; and it's also be '^' for ahead searching, but when you want to
  ;; searching no limited in ahead type we must force disable it.
  (setq ivy-initial-inputs-alist nil)
  (entropy/emacs-lazy-load-simple 'counsel
    ;; we must set it nil again when loaded `counsel' which will
    ;; inject its own types but with some eemacs specified which need
    ;; to use initial inputs.
    (setq ivy-initial-inputs-alist nil)
    (dolist (cmd '(Man-completion-table woman))
      (add-to-list 'ivy-initial-inputs-alist
                   (cons cmd "^")))
    ;; printable M-x candi sort default type by string less
    (entropy/emacs-alist-set
        'counsel-M-x ivy-sort-functions-alist
      'ivy-string<)
    ;; always show recent added the-mark in head chosen of candidates
    ;; list, where the original counsel defined reversed.
    (entropy/emacs-alist-set
        'counsel-mark-ring ivy-sort-functions-alist
      'ignore))

  ;; ivy details
  (setq enable-recursive-minibuffers t) ;Allow commands in minibuffers
  (setq ivy-use-selectable-prompt nil)
  (setq ivy-use-virtual-buffers t)      ;Enable bookmarks and recentf
  (setq ivy-count-format "%-5d ")
  (setq ivy-on-del-error-function 'ignore)  ;Disable back-delete exit when empty input.
  (setq ivy-dynamic-exhibit-delay-ms 2) ;prevent immediacy dnynamic process fetching crash.

  ;; Disable fancy highlight to the matched candidate for performance issue
  (setq ivy-display-style nil)

  ;; using fuzzy matching
  (setq ivy-re-builders-alist
        '(;; (read-file-name-internal . ivy--regex-plus)
          (counsel-M-x . ivy--regex-ignore-order)
          (counsel-describe-function . ivy--regex-ignore-order)
          (counsel-describe-variable . ivy--regex-ignore-order)
          (counsel-describe-symbol . ivy--regex-ignore-order)
          ;; eemacs union spec for all procedure directly using
          ;; `completing-read' instead of `ivy-read'.
          (t . entropy/emacs-ivy-common-re-builder)))

  ;; prompt newline set
  ;;
  ;; NOTE: eemacs has its inner auto prompt newline adding patch for
  ;; `ivy--insert-prompt' so we just comment it.
  ;;
  ;; (setq ivy-add-newline-after-prompt t)

  ;; GC restrictions:
  ;;
  ;; Since the selection line visual movement is implemented by
  ;; re-generate minibuffer contents, so each move command posted will
  ;; generated large heap of datas even that `ivy' already restrict
  ;; the rener part just with lenght of candis where `ivy-height'
  ;; indicated for.
  (dolist (el '((ivy-next-line nil . 0.3)
                (ivy-previous-line nil . 0.3)))
    (add-to-list 'entropy/emacs-garbage-collect-restrict-commands
                 el))

;; *** ivy config
  :config
;; **** more re-builders hack

  (defun entropy/emacs-ivy-use-re-builer nil
    "Set `ivy--regex-function' via eemacs collections."
    (interactive)
    (let ((sel (completing-read
                "Choose re-buider: "
                (list "identity" "pure" "plus" "fuzzy" "ignore-order")
                nil t)))
      (setq
       ;; FIXME: or we should respect caller's :re-buler specs or may
       ;; be messy but `ivy--reset-state' seems just ran at each
       ;; caller invoke time so that this spec will not messy like
       ;; what `ivy-rotate-preferred-builders' also set
       ;; `ivy--regex-function' directly.
       ivy--regex-function
       (pcase sel
         ('"identity" 'regexp-quote)
         ('"pure" 'identity)
         ('"plus" 'ivy--regex-plus)
         ('"fuzzy" 'ivy--regex-fuzzy)
         ('"ignore-order" 'ivy--regex-ignore-order)))
      (message "Set current ivy re-builder to `%s'" ivy--regex-function)))

;; **** exit ivy procedure immediatly after the dispatch actions
  ;; FIXME: do not use for all ivy caller, add filter for specified caller
  (defun entropy/emacs-ivy--ivy-read-quit-after-dispatch-actions
      (&rest _)
    "Interrupting rest process when `this-command' was
`ivy-dispatching-done' or `ivy-occur' or any extra ivy read
actions.

Originally, after each ivy-action did done from the ivy-dispatch
will go on for the rest procedure if exists, it originally was the
designation logic what ivy suggested that each function based on
ivy-framework should end with the `ivy-read' point i.e. there
shouldn't have any rest part after thus, which indicates that any
action after the `ivy-read' candi match one must be transferred
into the ':action' slot of `ivy-read' so that the procedure can be
entirely quit just after `ivy-read' done.

But not all the function builder follow this convention, and a
effect does by enabling `ivy-mode' will enhance the internal
`completing-read' experience and also can using some native
ivy extra actions throw ivy-dispatch, but almost all function
which invoking the `completing-read' just use it as the candi
chosen interaction, and injects rest functional part follow that,
that meaning that when we calling any extra ivy actions after that
part of the ivy enhanced `completing-read' procedure, the rest
will go on doing, that's dangerous because we usually just want
call those extra ivy actions to finish some minor work and do not
want do the origin process what the function designed.

Thus for all, this after type advice for `ivy-read' will focely
quit to the top-level when thus done.

Notice that this wrapper designation was not suggested by the
upstream and may be make risky follow the ivy updates.
"
    (let ((this-caller (ivy-state-caller ivy-last))
          ;; NOTE:
          ;; `this-command' and `this-caller' is not always a symbol
          ;; even though be commonly does as, so we need to wrapper
          ;; thus.
          (symname-func (lambda (x) (if (symbolp x )
                                        (symbol-name x)
                                      ""))))
      (when (and (or (eq this-command 'ivy-dispatching-done)
                     (eq this-command 'ivy-occur)
                     (string-match-p "^ivy-read-action/"
                                     (funcall symname-func this-command)))
                 ;; TODO: filters
                 (not (or
                       ;; FIXME: Since kill-new's
                       ;; `interprogram-cut-function' may not be ran
                       ;; completely (why?) when emacs error
                       ;; immediately. We must banned it.
                       (eq this-caller 'counsel-yank-pop)

                       ;; Other filters for common counsel commands,
                       ;; since common counsel commands are maintained
                       ;; by `ivy' author who has no wrong uses for
                       ;; `ivy-read'.
                       (string-match-p "^counsel-.*$"
                                       (funcall symname-func this-caller))
                       )))
        (progn
          (user-error "Ivy quit for caller '%s'" this-caller)))))
  (advice-add 'ivy-read :after #'entropy/emacs-ivy--ivy-read-quit-after-dispatch-actions)

;; **** inhibit double tab trigger
  ;; Patch the `ivy-alt-done' to prevent double click '<tab>'
  ;; triggered in `ivy-partial-or-done' i.e. This portion give the
  ;; minor changed for disabled double tab in ivy completion for
  ;; preventing accidental operation of double hint for 'TAB'.

  (defun __adv/around/ivy-alt-done (orig-func &rest orig-args)
    (let (_)
      (if (eq this-command 'ivy-partial-or-done)
          (message
           (propertize
            "⚠: no other accurate completion can be auto TAB"
            'face 'warning))
        (apply orig-func orig-args))))
  (advice-add 'ivy-alt-done :around '__adv/around/ivy-alt-done)

;; **** idle post for `ivy--queue-exhibit'

  (defvar __idle/ivy-queue-exhited-ivy-done-like-cmds
    '(ivy-alt-done
      ivy-mouse-done
      ivy-immediate-done
      ivy-done
      ivy-partial-or-done
      ivy-mouse-dispatching-done
      ivy-dispatching-done
      ))
  (dolist (cmd __idle/ivy-queue-exhited-ivy-done-like-cmds)
    (put cmd '__eemacs-ivy-done-like-cmd-p t))

  (defvar-local __idle/ivy-queue-exhited-done nil)
  ;; EEMACS_MAINTENANCE: patching follow upstream please!
  (entropy/emacs-define-idle-function __ya/ivy--queue-exhibit/idle-func 0.12
    (when (entropy/emacs-minibufferp nil t)
      (let* (
             ;; binding `this-command' to the
             ;; `entropy/emacs-current-session-this-command-before-idle'
             ;; since `this-command' is nil while idle time but
             ;; ivy subroutine need it to set.
             (this-command
              ;; bind to `self-insert-command' while `ivy-done' like trigger
              ;; since its an user messy.
              (if (entropy/emacs-get-symbol-prop
                   entropy/emacs-current-session-this-command-before-idle
                   '__eemacs-ivy-done-like-cmd-p)
                  (if (entropy/emacs-get-symbol-prop
                       entropy/emacs-current-session-last-command-before-idle
                       '__eemacs-ivy-done-like-cmd-p)
                      'self-insert-command
                    entropy/emacs-current-session-last-command-before-idle)
                entropy/emacs-current-session-this-command-before-idle)))
        (ivy--exhibit))))

  (defvar __eemacs-ivy-dynamic-commands
    '(self-insert-command
      ivy-backward-delete-char
      ivy-backward-kill-word
      ivy-forward-char
      backward-char))
  (dolist (cmd __eemacs-ivy-dynamic-commands)
    (put cmd '__eemacs-ivy-dynamic-command-p t))

  (defun __ya/ivy--exhibit (&rest _)
    (setq __idle/ivy-queue-exhited-done t))
  (advice-add 'ivy--exhibit :after #'__ya/ivy--exhibit)
  (defun __ya/ivy--queue-exhibit ()
    "The override advice for `ivy--queue-exhibit' to exhibit the
canidates idle after input event done by 0.2 seconds."
    (condition-case err
        (progn
          (setq __idle/ivy-queue-exhited-done nil)
          (let ((eemacs-dym-p
                 (not (entropy/emacs-ivy-patch/inhibit-dynamic-exhibit-p)))
                (ivy-dym-p
                 (and
                  ;; NOTE: prevent mistake set of `ivy-dynamic-exhibit-delay-ms'
                  (numberp ivy-dynamic-exhibit-delay-ms)
                  (> ivy-dynamic-exhibit-delay-ms 0)
                  (ivy-state-dynamic-collection ivy-last)))
                ;; since it's a post-command that `inhibit-debugger'
                ;; is always pressed, so we should reopen the debug
                ;; for debug convention.
                inhibit-debugger)
            (and ivy-dym-p
                 (entropy/emacs-cancel-timer-var ivy--exhibit-timer))
            (if (and
                 eemacs-dym-p
                 (entropy/emacs-get-symbol-prop
                  this-command '__eemacs-ivy-dynamic-command-p))
                (funcall __ya/ivy--queue-exhibit/idle-func) (ivy--exhibit))))
      ;; debug
      ((debug error) (signal (car err) (cdr err)))))

  (advice-add 'ivy--queue-exhibit :override #'__ya/ivy--queue-exhibit)

  (defun __adv/around/ivy-done/for-idle-trigger (orig-func &rest orig-args)
    "prompts for waiting for `ivy--exhibit' done while trigger
`ivy-done' before thus."
    (let (_)
      (if __idle/ivy-queue-exhited-done
          (apply orig-func orig-args)
        (let ((prompt-str " (waiting for exhibit done)"))
          (setq ivy--prompt-extra prompt-str)))))
  (advice-add 'ivy-done :around #'__adv/around/ivy-done/for-idle-trigger)

  (defun __ya/ivy-backward-kill-word (arg)
    "Alternative for `ivy-bakward-kill-word' but not trigger
`ivy--exhibit'."
    (interactive "p")
    (delete-region (point) (progn (forward-word (- arg)) (point))))
  (advice-add 'ivy-backward-kill-word :override #'__ya/ivy-backward-kill-word)

;; **** fix bug of `ivy-reverse-i-search'

  ;; EEMACS_MAINTENANCE: the patch must follow each update of `ivy'.
  (defun __ya/ivy-reverse-i-search ()
    "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibuffer.
\\<ivy-reverse-i-search-map>
You can also delete an element from history with \\[ivy-reverse-i-search-kill].

NOTE: this function has been patched with always require match
since the non-match candi action will cause ivy internal unknown
bug of (EEMACS_BUG: h-17036bdc-c6e9-4ac2-bac8-1c55bd8ecda4)."
    (interactive)
    (cond
     ((= (minibuffer-depth) 0)
      (user-error
       "This command is intended to be called from within `ivy-read' only"))
     ;; don't recur
     ((and (> (minibuffer-depth) 1)
           (eq (ivy-state-caller ivy-last) 'ivy-reverse-i-search)))
     (t
      (let ((enable-recursive-minibuffers t)
            (old-last ivy-last))
        (ivy-read "Reverse-i-search: "
                  (ivy-history-contents (ivy-state-history ivy-last))
                  :require-match t       ;EEMACS_BUG h-17036bdc-c6e9-4ac2-bac8-1c55bd8ecda4
                  :keymap ivy-reverse-i-search-map
                  :action (lambda (x)
                            (ivy--reset-state
                             (setq ivy-last old-last))
                            (delete-minibuffer-contents)
                            (insert (substring-no-properties (car x)))
                            (ivy--cd-maybe))
                  :caller 'ivy-reverse-i-search)))))

  (advice-add 'ivy-reverse-i-search :override #'__ya/ivy-reverse-i-search)

;; **** fix lag on `ivy-thing-at-point'

  (defvar __eemacs-ivy-thing-at-pt-nignore-cmds
    '(
      ;; let `swiper' like function using origin since its
      ;; internal needed in ivy source filea
      swiper
      swiper-thing-at-point
      swiper-all
      swiper-all-thing-at-point
      __ya/swiper-all-thing-at-point
      swiper-isearch
      swiper-isearch-thing-at-point
      counsel-grep-or-swiper
      __eemacs/ivy-minibuffer-common-thing-at-point
      ))
  (dolist (cmd __eemacs-ivy-thing-at-pt-nignore-cmds)
    (put cmd '__eemacs-ivy-thing-at-pt-nignore-cmd-p t))

  (defvar __eemacs-ivy-thing-at-pt-nignore-mmodes
    '(emacs-lisp-mode
      lisp-interaction-mode
      lisp-data-mode
      help-mode))
  (dolist (cmd __eemacs-ivy-thing-at-pt-nignore-mmodes)
    (put cmd '__eemacs-ivy-thing-at-pt-nignore-mmode-p t))

  (defvar __eemacs-ivy-thing-at-pt-nignore-mmodes-cmds
    '(counsel-describe-variable
      counsel-describe-function
      ))
  (dolist (cmd __eemacs-ivy-thing-at-pt-nignore-mmodes-cmds)
    (put cmd '__eemacs-ivy-thing-at-pt-nignore-mmodes-cmd-p t))

  (defun __ya/ivy-thing-at-point (orig-func &rest orig-args)
    "Ignore `ivy-thing-at-point' to return emtpy string but with some
occasions since the internal `thing-at-point' is so laggy with
large buffer."
    (cond ((or
            ;; TODO: could we use this: `current-prefix-arg' ?

            ;; FIXME: shall we need to use `with-ivy-window' to
            ;; stat the status in origin buffer since, but I test
            ;; that there's no need and why?
            (entropy/emacs-get-symbol-prop
             real-this-command '__eemacs-ivy-thing-at-pt-nignore-cmd-p)
            (and (entropy/emacs-get-symbol-prop
                  major-mode '__eemacs-ivy-thing-at-pt-nignore-mmode-p)
                 (entropy/emacs-get-symbol-prop
                  this-command '__eemacs-ivy-thing-at-pt-nignore-mmodes-cmd-p))
            ;; enable get thing when region actived unless the
            ;; region content is not thing like
            (and (region-active-p)
                 (let ((str (buffer-substring (region-beginning) (region-end))))
                   (and (not (string-match-p "[ \n\t\r]" str))
                        (not (> (length str) 100))))))
           (let (rtn)
             (prog1 (setq rtn (apply orig-func orig-args))
               (unless (string-empty-p rtn)
                 (message "ivy-thing-at-pt: invoke by command <%s> at thing '%s'"
                          this-command rtn)))))
          (t "")))
  (advice-add 'ivy-thing-at-point :around #'__ya/ivy-thing-at-point)


;; **** TODO `ivy--dynamic-collection-cands' bugs notation

;; For `swiper-isearch', `ivy--dynamic-collection-cands's error part
;; will be triggerred for as my "fixme" part description:
;; #+begin_src elisp
;;   (defun ivy--dynamic-collection-cands (input)
;;     (let ((coll (condition-case nil
;;                     (funcall (ivy-state-collection ivy-last) input)
;;                   (error
;;                    ;; FIXME: why ivy pass three args that func as that
;;                    ;; the above normal invocation is just use one, does
;;                    ;; its a protol for &rest'?
;;                    ;;
;;                    ;; if not, then this is horrible since both of the
;;                    ;; expection and the error debugging information is
;;                    ;; messed up.
;;                    (funcall (ivy-state-collection ivy-last) input nil nil)))))
;;       (if (listp coll)
;;           (mapcar (lambda (x) (if (consp x) (car x) x)) coll)
;;         coll)))
;; #+end_src
;; Thus for `swiper-isearch-function' will throw the
;; `wrong-type-arguments' error since it just declared as accepting
;; one arg.

;; **** TODO workaround for ivy window height

;; **** workaround for `ivy--insert-prompt'

  (entropy/emacs--api-restriction-uniform 'ivy--insert-prompt-hack
      'package-version-incompatible
    :when (> emacs-major-version 27)
    :do-error t
    :detector
    (not (version=
          "3.0.9"
          (substring
           entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           1)))
    :signal
    (signal
     entropy/emacs-package-version-incompatible-error-symbol
     (list 'entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
           "require: v3.0.9"))

    (defvar eemacs/ivy--prompt-cache nil)
    (defvar eemacs/ivy--insert-prompt-lazy-commands
      (list 'ivy-next-line 'ivy-previous-line 'left-char 'right-char
            'ivy-forward-char))
    (defun __ya/ivy--insert-prompt-1 ()
      "EEMACS_MAINTENANCE&FIXME Overriden advice for
`ivy--insert-prompt'.

For:

inhibit very long line (exceed for minibuffer's `window-width') prompt
string while use `ivy--directory' which is usually used when the
collection table is `read-file-name-internal'. Since in TUI session,
thus prompt will not be displayed and the exhibition of thus is messy
upon emacs 28.2 due to emacs new minibuffer resize mechanism while
`ivy' seems dropped its following step for."
      (when (setq ivy--prompt (ivy-prompt))
        ;; FIXME:
        ;; we should always truncate the drawer since we've patched
        ;; this function for fixing sets of bugs but the problem for
        ;; mini window height display messy when not truncate lines in
        ;; TUI
        (setq-local truncate-lines t)
        (unless (memq this-command '(ivy-done ivy-alt-done ivy-partial-or-done
                                              counsel-find-symbol))
          (setq ivy--prompt-extra ""))
        (let (head tail tail-not-empty-p (winww (window-width))
                   (ellipse-str "…"))
          (if (string-match "\\(.*?\\)\\(:? ?\\)\\'" ivy--prompt)
              (progn
                (setq head (match-string 1 ivy--prompt))
                (setq tail (match-string 2 ivy--prompt)))
            (setq head ivy--prompt)
            (setq tail ""))
          (setq tail-not-empty-p (not (string-empty-p tail)))
          (let ((inhibit-read-only t)
                (std-props '(front-sticky t rear-nonsticky t field t read-only t))
                (n-str
                 (concat
                  (if (and (bound-and-true-p minibuffer-depth-indicate-mode)
                           (> (minibuffer-depth) 1))
                      (format "[%d] " (minibuffer-depth))
                    "")
                  (concat
                   (if (string-match "%d.*%d" ivy-count-format)
                       (format head
                               (1+ ivy--index)
                               (or (and (ivy-state-dynamic-collection ivy-last)
                                        ivy--full-length)
                                   ivy--length))
                     (format head
                             (or (and (ivy-state-dynamic-collection ivy-last)
                                      ivy--full-length)
                                 ivy--length)))
                   ivy--prompt-extra
                   tail)))
                n-str-end-with-newline-p
                d-str)
            (when-let* (((not (display-graphic-p)))
                        (n-str-len (string-width n-str))
                        ((> n-str-len winww))
                        ;; FIXME: why tui emacs can not display
                        ;; equaled window-width string like some one
                        ;; include CJK chars as usual which may have
                        ;; extra newlines followed after? (thus why we
                        ;; need to `substring' more chars)
                        (lov (- winww n-str-len))
                        (winww-pp (- winww 10)))
              (unless (> winww-pp 0) (setq winww-pp winww))
              (setq n-str
                    (concat
                     (truncate-string-to-width
                      n-str winww-pp nil nil
                      (concat
                       ellipse-str
                       (cond ((and ivy--directory
                                   (string-match-p "Copy .+ to:" n-str))
                              "to: ")
                             (t
                              (if tail-not-empty-p tail " : "))))))))
            ;; ensure comfortable input experience
            (when-let* ((n-str-len (string-width n-str))
                        ((or (>= n-str-len winww)
                             (> (/ n-str-len (* winww 1.0)) 0.25))))
              (setq-local ivy-add-newline-after-prompt t)
              (setq n-str-end-with-newline-p t)
              (setq n-str (concat n-str "\n")))
            (save-excursion
              (goto-char (point-min))
              (delete-region (point-min) (minibuffer-prompt-end))
              (let ((wid-n (string-width n-str))
                    (wid-d nil)
                    (ww winww))
                (setq d-str
                      (if ivy--directory
                          (let* ((sdir (file-name-as-directory
                                        (file-name-nondirectory
                                         (directory-file-name ivy--directory))))
                                 (mdir (abbreviate-file-name ivy--directory))
                                 (wid-df (string-width ivy--directory))
                                 (wid-ds (string-width sdir))
                                 (wid-dm (string-width mdir))
                                 (wid-prefix (if n-str-end-with-newline-p 0 wid-n)))
                            (if (> (+ wid-prefix wid-df) ww)
                                (if (> (+ wid-prefix wid-dm) ww)
                                    (if (not (> (+ wid-prefix wid-ds) ww)) sdir
                                      (truncate-string-to-width
                                       sdir (- ww 2) nil nil (concat ellipse-str "/")))
                                  mdir) ivy--directory))
                        "")
                      wid-d (string-width d-str))
                (setq n-str
                      (cond ((and (not n-str-end-with-newline-p)
                                  (> (+ wid-n wid-d) ww))
                             (concat n-str "\n" d-str "\n"))
                            ((and (not n-str-end-with-newline-p)
                                  (> (+ wid-n wid-d (string-width ivy-text)) ww))
                             (concat n-str d-str "\n"))
                            (t (concat n-str d-str)))))
              (when ivy-pre-prompt-function
                (setq n-str (concat (funcall ivy-pre-prompt-function) n-str)))
              (when (and ivy-add-newline-after-prompt (not (string-match-p "\n$" n-str)))
                (setq n-str (concat n-str "\n")))
              (setq n-str (ivy--break-lines n-str (window-width)))

              ;; TODO&&EEMACS_TEMPORALLY_HACK: ensure enough mini
              ;; window height given, although it's the job of
              ;; `ivy--minibuffer-setup' but it's buggy.
              (when-let* ((n-str-llen (length (split-string n-str "\n")))
                          ((> n-str-llen 1)))
                (setq-local max-mini-window-height
                            (+ (- ivy-height
                                  ;; FIXME: we remain further more one
                                  ;; line since when as equal for thus
                                  ;; (i.e. theory as minus extra 1
                                  ;; line which is the origin prompt
                                  ;; line place should be also enough
                                  ;; for drawing what we given), the
                                  ;; mini buffer not show the first
                                  ;; prompt line?
                                  ;;(if ivy-add-newline-after-prompt 2 1)
                                  (if ivy-add-newline-after-prompt 1 0))
                               n-str-llen)))
              (set-text-properties 0 (length n-str)
                                   `(face minibuffer-prompt ,@std-props)
                                   n-str)
              (setq n-str (funcall ivy-set-prompt-text-properties-function
                                   n-str std-props))
              (insert (setq eemacs/ivy--prompt-cache n-str)))
            ;; Mark prompt as selected if the user moves there or it is the only
            ;; option left.  Since the user input stays put, we have to manually
            ;; remove the face as well.
            (when ivy--use-selectable-prompt
              (if (= ivy--index -1)
                  (add-face-text-property
                   (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
                (remove-list-of-text-properties
                 (minibuffer-prompt-end) (line-end-position) '(face))))
            ;; get out of the prompt area
            (constrain-to-field nil (point-max))))))

    (entropy/emacs-make-alist-with-symbol-prop-set
        'eemacs/ivy--insert-prompt-lazy-commands
        '__eemacs-command-is-ivy-insert-prompt-lazy-p
      :with-single t)
    (defun __ya/ivy--insert-prompt ()
      "See `__ya/ivy--insert-prompt-1' for details."
      (if (and eemacs/ivy--prompt-cache
               this-command
               (entropy/emacs-get-symbol-prop
                this-command
                '__eemacs-command-is-ivy-insert-prompt-lazy-p)
               (setq ivy--prompt (ivy-prompt)))
          (progn
            (save-excursion
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (delete-region (point-min) (minibuffer-prompt-end))
                (insert eemacs/ivy--prompt-cache)))
            ;; Mark prompt as selected if the user moves there or it is the only
            ;; option left.  Since the user input stays put, we have to manually
            ;; remove the face as well.
            (when ivy--use-selectable-prompt
              (if (= ivy--index -1)
                  (add-face-text-property
                   (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
                (remove-list-of-text-properties
                 (minibuffer-prompt-end) (line-end-position) '(face))))
            ;; get out of the prompt area
            (constrain-to-field nil (point-max)))
        (__ya/ivy--insert-prompt-1)))

    (advice-add 'ivy--insert-prompt :override '__ya/ivy--insert-prompt))

;; **** __end___
  )

;; ** ivy hydra
;; Additional key bindings for Ivy
(use-package ivy-hydra
  :init
  (when (version< "26" emacs-version)
    ;; `ivy-dispatching-done' can not display minibuffer hint prompt
    ;; when emacs version upper than 26. see
    ;; https://github.com/abo-abo/swiper/issues/2397 for details
    ;; ---------------------------------------------------------
    ;; [2020-06-03 Wed 18:06:44] for now above issue seems be fixed,
    ;; but still using the patch for more enhancement.
    (entropy/emacs-lazy-load-simple 'ivy
      (require 'ivy-hydra)
      (when (fboundp 'ivy-hydra-read-action)
        ;; using new ivy hydra api in latest ivy update version
        (setq ivy-read-action-function #'ivy-hydra-read-action)))))

;; ** swiper

(use-package swiper
  :commands (swiper swiper-all swiper-isearch)
  :preface

  :bind (("C-M-s" . swiper-all)
         :map swiper-map
         ("M-q" . swiper-query-replace)
         :map ivy-minibuffer-map
         (("M-p" . entropy/emacs-basic-pyim-toggle-cregexp-ivy-enable)
          ("M-n" . __eemacs/ivy-minibuffer-common-thing-at-point))
         :map swiper-all-map
         ("M-n" . __ya/swiper-all-thing-at-point))

;; *** init
  :init
  (setq swiper-action-recenter t)       ;recenter buffer after swiper jumping to the match

;; *** config
  :config

;; **** core advice
;; ***** enhance `swiper-*-thing-at-point'

  (defun __eemacs/swiper-call-with-thing-at-point (name func)
    "entropy-emacs union ivy framework inline `thing-at-point' core
subroutine."
    (let (_)
      (if (window-minibuffer-p)
          (let (bnd str regionp)
            ;; Do not duplicated insert the match when reinvoke
            (delete-region (line-beginning-position)
                           (line-end-position))
            (with-ivy-window
              (setq bnd
                    (if (setq regionp (region-active-p))
                        (prog1 (cons (region-beginning) (region-end))
                          (deactivate-mark))
                      (bounds-of-thing-at-point 'symbol)))
              (setq str
                    ;; EEMACS_TEMPORALLY_HACK: ignore erros when bound
                    ;; are nil since thing can not get in empty area
                    (ignore-errors
                      (buffer-substring-no-properties (car bnd) (cdr bnd)))))
            (if (not str)
                (message "WARN: Noting at point!")
              (insert str)
              (unless regionp
                ;; EEMACS_TEMPORALLY_HACK: just add ivy symbol
                ;; boundaries when prefix arg since it can not search
                ;; all matches while boundary is add, i.e. the
                ;; boundary is used to restrict without part match
                ;; like match 'abc' not match '0-abc-1' when boundary
                ;; added through `swiper--re-builder'
                (if current-prefix-arg (ivy--insert-symbol-boundaries)
                  (unless (eq real-last-command this-command)
                    (message
                     (format "%s (with 'C-u' and invoking again to insert boundary)"
                             name)))))))
        (let (thing)
          (if (use-region-p)
              (progn
                (setq thing (buffer-substring-no-properties
                             (region-beginning) (region-end)))
                (goto-char (region-beginning))
                (deactivate-mark))
            (let ((bnd (bounds-of-thing-at-point 'symbol)))
              (if bnd (goto-char (car bnd)))
              (setq thing (ivy-thing-at-point))))
          (funcall-interactively func thing)))))

  (defun __ya/swiper-isearch-thing-at-point ()
    "Like `swiper-isearch-thing-at-point' but with some eemacs patch
and bug fix."
    (interactive)
    (__eemacs/swiper-call-with-thing-at-point
     "Swiper-isearch-thing-at-pt" 'swiper-isearch))
  (advice-add 'swiper-isearch-thing-at-point
              :override #'__ya/swiper-isearch-thing-at-point)

  (defun __ya/swiper-all-thing-at-point ()
    "Like `swiper-isearch-thing-at-point' but for `swiper-all' inline
usage."
    (interactive)
    (__eemacs/swiper-call-with-thing-at-point
     "Swiper-all-thing-at-pt" 'swiper-all))

  (defun __eemacs/ivy-minibuffer-common-thing-at-point ()
    "Like `swiper-isearch-thing-at-point' but for
`ivy-minibuffer-map' inline usage for those commands has no
grabing thing feature."
    (interactive)
    (__eemacs/swiper-call-with-thing-at-point
     "ivy-common-thing-at-pt" 'swiper-all))

;; ***** swiper all patch
;; ****** core

;; ******* More filters for `swiper-all-buffer-p'

  (defvar __swiper-all-current-all-buffers nil)
  (defvar __swiper-all-current-restrictions nil)
  (defvar __swiper-all-eemacs-spec-filters
    ;; each function must return t for buffer need to select
    `((:name
       "mode restrictions"
       :no-qurey nil
       :func
       ,(lambda ()
          (let* ((buff-list __swiper-all-current-all-buffers)
                 (modes-get
                  (delete-dups
                   (delete
                    nil
                    (mapcar
                     (lambda (x)
                       (unless (minibufferp x)
                         (with-current-buffer x
                           major-mode)))
                     buff-list))))
                 mode)
            (setq mode
                  (completing-read "Which mode to swipr all?: "
                                   modes-get nil t)
                  mode (intern mode))
            (lambda (buff)
              (with-current-buffer buff
                (eq major-mode mode))))))))

  (defun __swiper-all-call-eemacs-spec-filters ()
    (setq __swiper-all-current-all-buffers (buffer-list))
    (let ((filters (copy-sequence __swiper-all-eemacs-spec-filters))
          (rtn-funcs nil))
      (catch :exit
        (while filters
          (let* ((fplist (pop filters))
                 (fname (plist-get fplist :name))
                 (ffunc (plist-get fplist :func))
                 (no-query (plist-get fplist :no-qurey)))
            (when (or no-query
                      (yes-or-no-p (format "do with <%s> " fname)))
              (push (funcall ffunc) rtn-funcs)))))
      (setq __swiper-all-current-restrictions
            (reverse rtn-funcs))))

  (defun __ya/swiper-all/eemacs-core-patch (orig-func &rest orig-args)
    (setq __swiper-all-current-restrictions nil)
    (let (_)
      (when current-prefix-arg
        (__swiper-all-call-eemacs-spec-filters))
      (apply orig-func orig-args)))
  (advice-add 'swiper-all :around #'__ya/swiper-all/eemacs-core-patch)

  (defun __ya/swiper-all-buffer-p (orig-func &rest orig-args)
    "Like `swiper-all-buffer-p' but with more filters adapted to eemacs."
    (let ((orig-judge (apply orig-func orig-args))
          (buffer (car orig-args)))
      (when orig-judge
        (let ((buff-name (buffer-name buffer))
              (buff-fname (buffer-file-name buffer)))
          (and
           ;; basic restricks
           (cond (
                  ;; ignore kill-ring persist file
                  (and buff-fname
                       (ignore-errors
                         (file-equal-p
                          buff-fname
                          entropy/emacs-kill-ring-persist-file)))
                  nil)
                 ;; ignore any pre non visible buffer
                 ((string-match-p "^ +\\*.+\\*.*" buff-name)
                  nil)
                 ;; otherwise for orig return
                 (t
                  orig-judge))
           ;; eemacs union filters
           (if (not __swiper-all-current-restrictions) t
             (catch :exit
               (dolist (func __swiper-all-current-restrictions)
                 (unless (funcall func buffer)
                   (throw :exit nil)))
               t)))))))
  (advice-add 'swiper-all-buffer-p
              :around
              #'__ya/swiper-all-buffer-p)

;; ****** window configuration restore when quit from `swiper-all'
  (defvar entropy/emacs-ivy--swiper-all-complete-did-p nil
    "Indicator for that `swiper-all' was did complete which not
unwind occasion.")
  (defun entropy/emacs-ivy--swiper-all-restore-wfg (orig-func &rest orig-args)
    "Restore origin window-configuration when unwind of `swiper-all'."
    (let (rtn
          (cur-wfg (current-window-configuration))
          (cur-pt (point)))
      (unwind-protect
          (progn
            (setq entropy/emacs-ivy--swiper-all-complete-did-p
                  nil)
            (setq rtn (apply orig-func orig-args)
                  entropy/emacs-ivy--swiper-all-complete-did-p
                  t)
            rtn)
        (unless entropy/emacs-ivy--swiper-all-complete-did-p
          (set-window-configuration cur-wfg)
          (goto-char cur-pt)
          ;; if `ivy-occur' occurred, then we should display that
          ;; buffer since we restored wcfg which coverred last wcfg
          ;; which displayed that occur buffer.
          ;;
          ;; EEMACS_MAINTENANCE: the occur buffer is retrieved by
          ;; `next-error-last-buffer' which is set by `ivy-occur'
          ;; function's internal procedure where is not an exposed
          ;; API, so we should track the updates with upstream.
          (when (and (eq this-command 'ivy-occur)
                     (buffer-live-p next-error-last-buffer)
                     (string-match-p "\\*ivy-occur.*swiper-all"
                                     (buffer-name next-error-last-buffer)))
            (display-buffer next-error-last-buffer))))))

  ;; make `swiper-all' restore origin window-configuration when unwind
  (advice-add 'swiper-all :around #'entropy/emacs-ivy--swiper-all-restore-wfg)

;; ***** messy handles

  ;; EEMACS_MAINTENANCE: follow upstreams updates

  (defvar __ya/swiper-match-usable-p/no-hack-p nil)
  (defun __ya/swiper-match-usable-p (orig-func &rest orig-args)
    ;; since messy search via `re-search-*' may be cross line which is
    ;; unusable.
    (when (or __ya/swiper-match-usable-p/no-hack-p
              (not (looking-at-p "^$")))
      (apply orig-func orig-args)))
  (advice-add 'swiper-match-usable-p
              :around #'__ya/swiper-match-usable-p)
  ;; Since `swiper' relying on line-number pos offset from `point-min'
  ;; to counting the preselection line to display at the caller
  ;; initialization, we should respect each empty line which will be
  ;; counted via `count-screen-lines' in body of `swiper'.
  (defun __ya/swiper--candidates (orig-func &rest orig-args)
    (let ((__ya/swiper-match-usable-p/no-hack-p t))
      (apply orig-func orig-args)))
  (advice-add 'swiper--candidates
              :around
              #'__ya/swiper--candidates)

  (defun __ya/swiper-isearch-function (orig-func &rest orig-args)
    (condition-case err
        (funcall orig-func
                 ;; FIXME: why `ivy--dynamic-collection-cands' will try
                 ;; another (nil nil) two external arguments for failed
                 ;; retry in `condition-case'?
                 (car orig-args))
      ;; since `re-search-*' throw error when invaid regexp detected
      ;; even if the NOERROR arg is set.
      (invalid-regexp
       (message "[%s] [swiper-isearch-function]: %s"
                (format-time-string "%Y-%m-%d %a %H:%M:%S") err)
       ;; ensure null return which let ivy consider the collection is
       ;; empty since `swiper-isearch-function' the collection
       ;; function for `ivy-read' used for caller `swiper-isearch'.
       nil)
      (error (signal (car err) (cdr err)))))
  (advice-add 'swiper-isearch-function
              :around
              #'__ya/swiper-isearch-function)

  (defun __ya/swiper-all-function (orig-func &rest orig-args)
    (condition-case err
        (funcall orig-func
                 ;; FIXME: why `ivy--dynamic-collection-cands' will try
                 ;; another (nil nil) two external arguments for failed
                 ;; retry in `condition-case'?
                 (car orig-args))
      ;; Since `re-search-*' throw error when invaid regexp detected
      ;; even if the NOERROR arg is set.  Also since
      ;; `put-text-property' can not operated on an empty string where
      ;; above wrong regexp caused.
      (error
       (message "[%s] [swiper-all-function]: %s"
                (format-time-string "%Y-%m-%d %a %H:%M:%S") err)
       ;; ensure null return which let ivy consider the collection is
       ;; empty since `swiper-all-function' the collection
       ;; function for `ivy-read' used for caller `swiper-isearch'.
       nil)))
  (advice-add 'swiper-all-function
              :around
              #'__ya/swiper-all-function)

  ;; (defun __ya/swiper--isearch-same-line-p (orig-func &rest orig-args)
  ;;   ;; since the orig func doesn't deal with non `point' prop occasion
  ;;   ;; on `count-lines' usage.
  ;;   (condition-case err
  ;;       (apply orig-func orig-args)
  ;;     (error
  ;;      (message "[swiper--isearch-same-line-p]: %s" err)
  ;;      ;; ensure null return which to emulate null boolean return
  ;;      ;; since we are failed to judge
  ;;      nil)))
  ;; (advice-add 'swiper--isearch-same-line-p
  ;;             :around
  ;;             #'__ya/swiper--isearch-same-line-p)

  (defun __ya/swiper--isearch-candidate-pos (orig-func &rest orig-args)
    (let ((cand (car orig-args)))
      ;; prevent args-out-of-range
      (unless (and (integer-or-marker-p cand)
                   (with-ivy-window (>= cand (point-max))))
        (apply orig-func orig-args))))
  (advice-add 'swiper--isearch-candidate-pos
              :around
              #'__ya/swiper--isearch-candidate-pos)

  ;; (defun __ya/swiper--isearch-occur-cands (cands)
  ;;   (let* ((last-pt (get-text-property 0 'point (car cands)))
  ;;          line res pt)
  ;;     (unless last-pt
  ;;       (catch :break
  ;;         (dolist (cand cands)
  ;;           (when (setq last-pt (get-text-property 0 'point cand))
  ;;             (throw :break nil)))))
  ;;     (when last-pt
  ;;       (setq line (1+ (line-number-at-pos last-pt)))
  ;;       (dolist (cand cands)
  ;;         (when (setq pt (get-text-property 0 'point cand))
  ;;           (cl-incf line (1- (count-lines last-pt pt)))
  ;;           (push (cons line cand) res)
  ;;           (setq last-pt pt)))
  ;;       (nreverse res))))
  ;; (advice-add 'swiper--isearch-occur-cands
  ;;             :override #'__ya/swiper--isearch-occur-cands)

;; *** __end___
  )


;; ** ivy-prescient
(use-package ivy-prescient
  :commands (ivy-prescient-mode)
  :init
  (defun ivy-prescient-non-fuzzy (str)
    "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq
   ivy-prescient-retain-classic-highlighting t
   ivy-prescient-enable-sorting t
   ivy-prescient-sort-commands
   '(:not swiper swiper-isearch ivy-switch-buffer
          lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
          counsel-grep counsel-git-grep counsel-rg counsel-ag
          counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-org-capture counsel-outline counsel-org-goto
          counsel-load-theme counsel-yank-pop
          counsel-recentf counsel-buffer-or-recentf
          centaur-load-theme)
   ivy-prescient-enable-filtering nil)

  ;; EEMACS_MAINTENANCE: temporally disable auto init
  ;; `ivy-prescient-mode' since its laggy.
  ;; TODO: hack on its laggy performance
  ;; (entropy/emacs-lazy-initial-advice-before
  ;;  '(ivy-mode)
  ;;  "ivy-prescient-init" "ivy-prescient-init"
  ;;  :prompt-type 'prompt-echo
  ;;  :pdumper-no-end t
  ;;  (ivy-prescient-mode 1))

  ;; (entropy/emacs-lazy-initial-advice-after
  ;;  '(counsel-mode)
  ;;  "ivy-prescient-rebuilder-init"
  ;;  "ivy-prescient-rebuilder-init"
  ;;  :prompt-type 'prompt-echo
  ;;  :pdumper-no-end t
  ;;  (dolist (re-builder
  ;;           '((counsel-ag . ivy-prescient-non-fuzzy)
  ;;             (counsel-rg . ivy-prescient-non-fuzzy)
  ;;             (counsel-pt . ivy-prescient-non-fuzzy)
  ;;             (counsel-grep . ivy-prescient-non-fuzzy)
  ;;             (counsel-fzf . ivy-prescient-non-fuzzy)
  ;;             (counsel-imenu . ivy-prescient-non-fuzzy)
  ;;             (counsel-yank-pop . ivy-prescient-non-fuzzy)
  ;;             (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
  ;;             (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
  ;;             (insert-char . ivy-prescient-non-fuzzy)
  ;;             (counsel-unicode-char . ivy-prescient-non-fuzzy)
  ;;             (t . ivy-prescient-re-builder)
  ;;             ;; NOTE:
  ;;             ;; use default rebulder for swiper clusters since we use
  ;;             ;; it native regexp builder for query-replace better
  ;;             ;; than what provided by `prescient' which has multibyte
  ;;             ;; unicode groups injection.
  ;;             (swiper . ivy--regex)
  ;;             (swiper-isearch . ivy--regex)
  ;;             (swiper-all . ivy--regex)
  ;;             ))
  ;;    (let ((caller (car re-builder))
  ;;          (builder (cdr re-builder)))
  ;;      (cond
  ;;       ((assoc caller ivy-re-builders-alist)
  ;;        (setf (alist-get caller ivy-re-builders-alist) builder))
  ;;       (t
  ;;        (add-to-list
  ;;         'ivy-re-builders-alist
  ;;         re-builder))))))
  )

;; ** counsel
(use-package counsel
  :eemacs-adrequire
  ((:enable t :adfors (entropy/emacs-hydra-hollow-call-before-hook) :adtype hook :pdumper-no-end t))
  :diminish counsel-mode
  :commands (counsel-mode
             counsel-linux-app
             counsel-grep-or-swiper)

;; *** bind-key
  :bind (("M-x"     . counsel-M-x)
         ("C-s"     . counsel-grep-or-swiper)
         ("C-c M-t" . counsel-load-theme)
         ("C-c g"   . counsel-git)
         ("C-x d"   . counsel-dired)
         ("C-x C-f" . counsel-find-file)
         ("C-h a"   . counsel-apropos)
         ("C-h b"   . counsel-descbinds)
         ("C-h P"   . counsel-package)
         ("C-h v"   . counsel-describe-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-h l"   . counsel-find-library)
         ("C-x j"   . counsel-mark-ring)
         ("C-x C-t" . counsel-find-file-extern)
         ("C-c M-b" . counsel-recentf)
         ("C-c M-k" . counsel-yank-pop)
         ([remap yank-pop] . counsel-yank-pop))

;; *** entropy/emacs-top-keymap

  :eemacs-tpha
  (((:enable t :defer t))
   ("Utils"
    (("u c"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'counsel-mode))
      "Counsel Stuffs Map"
      :enable t :exit t))))

  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (entropy/emacs-after-startup-idle-hook)
                                      :adtype hook :pdumper-no-end t)))
    (counsel-mode (counsel counsel-mode-map) nil (2 2 2)))
   (
    ;; FILE group -- prefix 'f'
    "Counsel File Manipulation"
    (("C-c c f b" counsel-buffer-or-recentf "Open Recentf buffer or File"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f f" counsel-fzf "Open a file using the fzf shell command"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f l" counsel-locate "Call the \"locate\" shell command."
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f m b" counsel-bookmark "View Emacs Bookmarks"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f m d" counsel-bookmarked-directory "View Emacs Bookmarked Directories"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c f o" counsel-switch-buffer-other-window "Switch buffer to other window"
      :enable t :exit t :eemacs-top-bind t))

    ;; GIT group -- prefix 'g'
    "Counsel Git Manipulation"
    (("C-c c g c" counsel-git-checkout "Call the \"git checkout\" command"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c g g" counsel-git-grep "Grep for a string in the current Git repository"
      :enable t :exit t :eemacs-top-bind t))

    ;; GREP group -- prefix 's'
    "Counsel Grep Manipulation"
    (("C-c c s g" counsel-grep "Grep for a string in the file visited by the current buffer"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s a" counsel-ag "Grep for a string in the current directory using ag"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s p" counsel-pt "Grep for a string in the current directory using pt"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c s r" counsel-rg "Grep for a string in the current directory using rg"
      :enable t :exit t :eemacs-top-bind t))

    ;; Misc.Emacs group -- prefix 'e'
    "Counsel Emacs Manipulation"
    (("C-c c e c" counsel-faces "Complete faces with preview"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e f" counsel-fonts "Show a list of all supported font families"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e h" counsel-command-history "Show the history of commands"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e i" counsel-imenu "Jump to a buffer position indexed by imenu"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e k" counsel-kmacro "Interactively choose and run a keyboard macro."
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e l" counsel-load-library "Load a selected the Emacs Lisp library"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e m" counsel-minor "Enable or disable minor mode"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e p" counsel-package "Install or delete packages"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e s" counsel-list-processes "Offer completion for 'process-list'"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e t" counsel-load-theme "Load specific emacs theme"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e u" counsel-unicode-char "Insert COUNT copies of a Unicode character at point"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c e v" counsel-set-variable "Set a variable SYM, with completion"
      :enable t :exit t :eemacs-top-bind t))

    ;; TODO Misc.OS group -- prefix 'o'
    ;; --------------------------------
    ;; TODO org grep
    ;; -------------
    ;; MISCELLANEOUS group
    "Counsel Miscellaneous"
    (("C-c c m e" counsel-colors-emacs
      "Show a list of all supported colors for a particular frame"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m m" counsel-minibuffer-history "Browse minibuffer history"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m o" counsel-outline "Jump to an outline heading with completion"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m w" counsel-colors-web "Show a list of all W3C web colors for use in CSS"
      :enable t :exit t :eemacs-top-bind t)
     ("<M-up>" counsel-linux-app "Launch a Linux desktop application"
      :enable sys/is-linux-and-graphic-support-p :exit t :eemacs-top-bind t))))


;; *** init
  :init

  (setq counsel-find-file-at-point nil)
  (setq counsel-yank-pop-separator
        "\n────────\n")

  ;; increasing swiper limit to has more convention
  (setq counsel-grep-swiper-limit 10000000)

  ;; - let counsel grep base command treat some file as text, thus add
  ;;   '-a' option as thus for. This specify exist because of some large
  ;;   text file will treated as an binary file as mistake as grep does.
  ;;
  ;; - use perl regex match
  (setq counsel-grep-base-command "grep -a -P -n -e %s %s")

;; *** config
  :config
  ;; enable `ivy-mode‘ firstly before enable `counsel-mode'
  (unless (bound-and-true-p ivy-mode)
    (ivy-mode +1))
  (unless (bound-and-true-p counsel-mode)
    (counsel-mode +1))

;; **** advice counsel--M-x-externs for it's bad lagging perfomance

  ;; because `counsel--M-x-externs' has the `require' function for
  ;; it's contained condition context so it will lagging like previous
  ;; version of `ivy--add-face'.
  (defun entropy/emacs-ivy--counsel--M-x-externs (&rest _)
    nil)
  (advice-add 'counsel--M-x-externs
              :override
              #'entropy/emacs-ivy--counsel--M-x-externs)

;; **** override `counsel-load-theme-action'
  (defun __ya/counsel-load-theme-action (x)
    "Redefined by overriden to remove the all-theme-disable preface in the
origin procedure which will casue some error in eemacs-specification."
    (condition-case nil
        (progn
          ;;(mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x) t)
          (when (fboundp 'powerline-reset)
            (powerline-reset)))
      (error "Problem loading theme %s" x)))
  (advice-add 'counsel-load-theme-action
              :override
              #'__ya/counsel-load-theme-action)

;; **** do not active `counsel-grep' function when not grep exec found

  (defun __ya/counsel-grep-or-swiper (&optional initial-input)
    "Call `swiper-isearch' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern."
    (interactive)
    (cond
     ;; big buffer always use `isearch' to reduce lag and gc
     ((or current-prefix-arg
          (> (entropy/emacs-buffer-size) (* 5 (expt 1024 2))))
      (isearch-forward t t))
     ((or (not buffer-file-name)
          (buffer-narrowed-p)
          ;; although is a file but without truely local file
          ;; association in which case the buffer is also a virtual
          ;; one, so we still use `swiper-isearch' to deal with.
          (and buffer-file-name (file-remote-p buffer-file-name))
          (and buffer-file-name
               (jka-compr-get-compression-info buffer-file-name))
          (funcall counsel-grep-use-swiper-p))
      ;; FIXME: [2023-08-01 Tue 23:35:31] temporarily fallback to use
      ;; `swiper' instead of `swiper-isearch' since it stucked via
      ;; `ivy-next/previous-line' i.e. can not move to begin/end of
      ;; candi list, is this related to our hackings on swiper stuffs?
      (swiper initial-input))
     ((buffer-file-name)
      ;; save the buffer first
      (when (and (buffer-modified-p)
                 (and buffer-file-name (file-writable-p buffer-file-name))
                 (or (yes-or-no-p "Save buffer before `counsel-grep'?")
                     (user-error "Abort!")))
        (save-buffer))
      (counsel-grep initial-input))
     (t (isearch-forward t t))))
  (advice-add 'counsel-grep-or-swiper :override #'__ya/counsel-grep-or-swiper)

;; **** patches for `counsel-grep' and referred

  (defun entropy/emacs-ivy--counsel-grep-patch-1 (orig-func &rest orig-args)
    "The around advice for `counsel-grep' to widen the current
buffer for preventing ivy framework highlights the wrong positon
in current buffer."
    (when (buffer-narrowed-p) (widen))
    (apply orig-func orig-args))

  (advice-add 'counsel-grep :around #'entropy/emacs-ivy--counsel-grep-patch-1)

  (defun entropy/emacs-ivy--counsel-grep-function (string)
    "Grep in the current directory for STRING.

NOTE:

This is the override advice for `counsel-grep-function' to
directly identified the input regexp string which do not be with
`ivy--regex' feature."
    (or
     (ivy-more-chars)
     (let ((regex (identity string)))
       (counsel--async-command
        (format counsel-grep-command (shell-quote-argument regex)))
       nil)))

  (advice-add 'counsel-grep-function :override #'entropy/emacs-ivy--counsel-grep-function)


;; **** enhance counsel company

  (defvar entropy/emacs-ivy--counsel-company-candidates nil)
  (defvar entropy/emacs-ivy--counsel-company-candidates-cache nil)
  (defvar entropy/emacs-ivy--counsel-company-candidates-length nil)
  (defvar entropy/emacs-ivy--counsel-company-backend nil)
  (defvar entropy/emacs-ivy--counsel-company-common nil)
  (defvar entropy/emacs-ivy--counsel-company-prefix nil)
  (defvar entropy/emacs-ivy--counsel-company-selection nil)
  (defvar entropy/emacs-ivy--counsel-company-point nil)
  (defvar entropy/emacs-ivy--counsel-company-search-string nil)
  (defvar entropy/emacs-ivy--counsel-orig-buff nil)
  (defun __ya/counsel-company ()
    "Lile `counsel-company' but hacked with eemacs prope."
    (interactive)
    ;; EEMACS_TEMPORALLY_HACK: do not duplicate enable company-mode
    ;; since each run will call its hook/
    (unless (bound-and-true-p company-mode)
      (company-mode 1))
    ;; EEMACS_TEMPORALLY_HACK:
    (if company-candidates
        (progn
          (setq entropy/emacs-ivy--counsel-company-candidates
                company-candidates
                entropy/emacs-ivy--counsel-company-candidates-cache
                company-candidates-cache
                entropy/emacs-ivy--counsel-company-backend
                company-backend
                entropy/emacs-ivy--counsel-company-candidates-length
                company-candidates-length
                entropy/emacs-ivy--counsel-company-common
                company-common
                entropy/emacs-ivy--counsel-company-selection
                company-selection
                entropy/emacs-ivy--counsel-company-point
                company-point
                entropy/emacs-ivy--counsel-company-search-string
                company-search-string
                entropy/emacs-ivy--counsel-company-prefix
                company-prefix
                entropy/emacs-ivy--counsel-orig-buff
                (current-buffer))
          (company-call-frontends 'hide))
      (user-error "No company candidates found"))
    (let ((len (cond ((let ((l (length company-common)))
                        (and company-common
                             (string= company-common
                                      (buffer-substring
                                       ;; ensure not
                                       ;; `arg-out-of-range' since the
                                       ;; `company-common' is not
                                       ;; always the string user seen
                                       ;; as.
                                       (max (point-min) (- (point) l))
                                       (point)))
                             l)))
                     (company-prefix
                      (length company-prefix)))))
      (when len
        (setq ivy-completion-beg (- (point) len))
        (setq ivy-completion-end (point))
        (ivy-read "Candidate: " company-candidates
                  :action #'ivy-completion-in-region-action
                  :caller 'counsel-company))))

  (advice-add 'counsel-company
              :override
              #'__ya/counsel-company)

  (defun entropy/emacs-ivy--counsel-company-show-doc-action (candi)
    (let* ((selection candi))
      (with-current-buffer entropy/emacs-ivy--counsel-orig-buff
        (let* ((company-selection
                (- (length company-candidates)
                   (length (member selection company-candidates)))))
          (company-show-doc-buffer)))))

  (ivy-add-actions 'counsel-company
                   '(("M-h" entropy/emacs-ivy--counsel-company-show-doc-action
                      "Show document for current selection if available")))

;; **** counsel-locate
  (when (and sys/win32p entropy/emacs-microsoft-windows-unix-emulator-enable)
    (defun __ya/counsel-locate (&optional initial-input)
      "Redefined for transfer volum's type of msys2 or other
window-gnu-enviroment to windows origin volum type by function
`entropy/emacs-transfer-wvol'"
      (interactive)
      (counsel--locate-updatedb)
      (ivy-read "Locate: " #'counsel-locate-function
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-locate-history
                :action (lambda (file)
                          (when file
                            (with-ivy-window
                              (entropy/emacs-transfer-wvol file))))
                :unwind #'counsel-delete-process
                :caller 'counsel-locate))
    (advice-add 'counsel-locate :around #'__ya/counsel-locate))

;; **** redefine counsel-git

  (setq counsel-git-cmd "git ls-files --full-name --")

  (defvar entropy/emacs-ivy-counsel-git-root nil
    "Temporally variable storing git repository root dir,
this variable used to patching for origin `counsel-git'.")

  (defun __ya/counsel-git-cands (&rest _)
    "Patched with eemacs used for `__ya/counsel-git-action'."
    (let ((default-directory (entropy/emacs-return-as-default-directory
                              (counsel-locate-git-root))))
      (setq entropy/emacs-ivy-counsel-git-root default-directory)
      (split-string
       (shell-command-to-string counsel-git-cmd)
       "\n"
       t)))
  (advice-add 'counsel-git-cands :override #'__ya/counsel-git-cands)

  (defun __ya/counsel-git-action (x)
    "Redefined by entropy-emacs because of:

ivy version 0.11.0 and counsel version 0.11.0 has the bug that
using wrong root-dir for find git repo's file that will cause
file not existed error and creating new buffer with that actual
name, this problem caused by origin `counsel-git-action' using
`ivy-last''s directory slot as the default diretory on the 0.11.0
version of ivy framework updating."
    (with-ivy-window
      (let ((default-directory entropy/emacs-ivy-counsel-git-root))
        (find-file x))))
  (advice-add 'counsel-git-action :override #'__ya/counsel-git-action)

  ;; counsel-git with utf-8
  (advice-add 'counsel-git :around
              #'entropy/emacs-lang-use-utf-8-ces-around-advice)

;; **** redefine counsel-kmacro
  (defun __ya/counsel-kmacro-action-run (x)
    "Redefined to compat with eemacs.

Since we chosen the kmacro from ring, we set it as the
`last-kbd-macro' for reputation later."
    (let* ((actual-kmacro (cdr x))
           (kmacro-keys (nth 0 actual-kmacro))
           (kmacro-counter-format-start (nth 2 actual-kmacro)))
      ;; EEMACS_TEMPORALLY_HACK: set current chosen as `last-kbd-macro'.
      (kmacro-push-ring)
      (kmacro-split-ring-element actual-kmacro)
      ;; With prefix argument, call the macro that many times.
      (kmacro-call-macro (or current-prefix-arg 1) t nil kmacro-keys)))
  (advice-add 'counsel-kmacro-action-run
              :override #'__ya/counsel-kmacro-action-run)

;; **** redefine `counsel-fonts'

  (defun __ya/counsel-fonts (orig-func &rest orig-args)
    "Same as `counsel-fonts', but ignored in non-GUI session
since `font-family-list' always return nil in TUI session."
    (when (eq (face-attribute 'default :font) 'unspecified)
      (user-error "We can not detecting any system fonts in this emacs session"))
    (apply orig-func orig-args))
  (advice-add 'counsel-fonts :around #'__ya/counsel-fonts)

;; *** __end___
  )

;; *** use counsel css for quickly search css selector
(use-package counsel-css
  :hook (css-mode . counsel-css-imenu-setup)
  :eemacs-mmphca
  (((:enable t :defer (:data (:adfors (css-mode-hook) :adtype hook :pdumper-no-end t)))
    (css-mode (css-mode css-mode-map)))
   ("Basic"
    (("C-c M-d" counsel-css "Jump to a css selector"
      :enable t
      :exit t
      :map-inject t)))))

;; *** use ivy-xref for quickly find defination and reference
(use-package ivy-xref
  :after ivy
  :commands (ivy-xref-show-xrefs)
  :init
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions
  ;; (e.g. project-find-regexp) as well
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
  (when (>= emacs-major-version 27)
    ;; xref initialization is different in Emacs 27 - there are two
    ;; different variables which can be set rather than just one
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))

;; *** use display world clock
(use-package counsel-world-clock
  :commands  (counsel-world-clock)
  :eemacs-indhca
  (((:enable t :defer (:data (:adfors (counsel-mode-hook) :adtype hook :pdumper-no-end t)))
    (counsel-mode (counsel counsel-mode-map)))
   ("Counsel Miscellaneous"
    (("C-c c m c" counsel-world-clock "Display time in different time zone in echo area"
      :enable t :exit t :eemacs-top-bind t)))))

;; ** avy
(use-package avy
  :commands
  (avy-goto-line avy-goto-char)
  :eemacs-indhc
  (((:enable t :defer (:data (:adfors (counsel-mode-hook window-configuration-change-hook) :adtype hook :pdumper-no-end t)))
    (avy))
   ("Avy Core Actions"
    (("M-g l" avy-goto-line "Jump to a line start in current buffer"
      :enable t :exit t :global-bind t)
     ("C-t" avy-goto-line "Jump to the line start in current buffer"
      :enable t :exit t :global-bind t)
     ("M-g c" avy-goto-char "Jump to the currently visible CHAR"
      :enable t :exit t :global-bind t)
     ("C-r" avy-goto-char "Jump to the currently visible CHAR"
      :enable t :exit t :global-bind t))))

  :eemacs-tpha
  (((:enable t :defer t))
   ("Basic"
    (("b a"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'avy))
      "Avy Pos Jump"
      :enable t :exit t))))
  :config
  ;; misty keys touched for `avy-goto-char' which not explicit show to
  ;; user but useful for preventing unmentionable hints
  (dolist (misty-key '("C-s-r" "C-s-t" "C-s-e"))
    (global-set-key (kbd misty-key) #'avy-goto-char))

  ;; FIXME: if invoke `avy-isearch' not in activated `isearch-mode'
  ;; then will let emacs hang on with memory increased crazily in some
  ;; occasions or be inner error since `avy-isearch' is just desinged
  ;; for using in that case, thus shall we need to put a issue for
  ;; upstream?
  (entropy/emacs-!cl-defun __ya/avy-isearch/judge-isearch-enabled
      (orig-func &rest orig-args)
    (if (bound-and-true-p isearch-mode)
        (apply orig-func orig-args)
      (entropy/emacs-!user-error
       "You are not in isearch-mode")))
  (advice-add 'avy-isearch
              :around
              #'__ya/avy-isearch/judge-isearch-enabled)

  )

;; ** Ivy UI Enhancement
;; *** all the icons ivy
(use-package all-the-icons-ivy
  :commands (all-the-icons-ivy-setup)
  :init
  (if (and (eq entropy/emacs-ivy-rich-type 'all-the-icons-ivy)
           (entropy/emacs-icons-displayable-p))
      (entropy/emacs-lazy-load-simple 'ivy
        (all-the-icons-ivy-setup))))

;; *** ivy rich mode

;; **** all the icons ivy rich
(use-package all-the-icons-ivy-rich
  :commands
  (all-the-icons-ivy-rich-mode)
  :eemacs-functions
  (all-the-icons-ivy-rich-world-clock-icon
   all-the-icons-ivy-rich-file-icon
   all-the-icons-ivy-rich-mode-icon
   all-the-icons-ivy-rich-buffer-icon
   all-the-icons-ivy-rich-company-icon
   all-the-icons-ivy-rich-git-branch-icon
   all-the-icons-ivy-rich-package-icon
   all-the-icons-ivy-rich-process-icon
   all-the-icons-ivy-rich-font-icon
   all-the-icons-ivy-rich-dir-icon
   all-the-icons-ivy-rich-face-icon
   all-the-icons-ivy-rich-bookmark-icon
   all-the-icons-ivy-rich-function-icon
   all-the-icons-ivy-rich-imenu-icon
   all-the-icons-ivy-rich-tramp-icon
   all-the-icons-ivy-rich-library-icon
   all-the-icons-ivy-rich-keybinding-icon
   all-the-icons-ivy-rich-symbol-icon
   all-the-icons-ivy-rich-theme-icon
   all-the-icons-ivy-rich-variable-icon
   all-the-icons-ivy-rich--format-icon)
  :preface

  (defvar entropy/emacs-ivy-all-the-icon-ivy-rich-icon-funcs
    '(all-the-icons-ivy-rich-world-clock-icon
      all-the-icons-ivy-rich-file-icon
      all-the-icons-ivy-rich-mode-icon
      all-the-icons-ivy-rich-buffer-icon
      all-the-icons-ivy-rich-company-icon
      all-the-icons-ivy-rich-git-branch-icon
      all-the-icons-ivy-rich-package-icon
      all-the-icons-ivy-rich-process-icon
      all-the-icons-ivy-rich-font-icon
      all-the-icons-ivy-rich-dir-icon
      all-the-icons-ivy-rich-face-icon
      all-the-icons-ivy-rich-bookmark-icon
      all-the-icons-ivy-rich-function-icon
      all-the-icons-ivy-rich-imenu-icon
      all-the-icons-ivy-rich-tramp-icon
      all-the-icons-ivy-rich-library-icon
      all-the-icons-ivy-rich-keybinding-icon
      all-the-icons-ivy-rich-symbol-icon
      all-the-icons-ivy-rich-theme-icon
      all-the-icons-ivy-rich-variable-icon))
  (defun ya/all-the-icon-ivy-rich-common-dir-icon
      (_candi)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "file-directory"
                            :height 1.0 :v-adjust 0.01)))

  (defun ya/all-the-icons-ivy-rich-common-file-icon (candidate)
    "Display file icon from CANDIDATE in `ivy-rich'."
    (let* ((path (expand-file-name candidate ivy--directory))
           (file (file-name-nondirectory path))
           (icon (cond
                  ((file-directory-p path)
                   (all-the-icons-octicon "file-directory"
                                          :height 1.0 :v-adjust 0.01))
                  ((string-match-p "^/.*:$" path)
                   (all-the-icons-octicon "radio-tower"
                                          :height 1.0 :v-adjust 0.01))
                  ((not (string-empty-p file))
                   (all-the-icons-icon-for-file file :v-adjust -0.05)))))
      (all-the-icons-ivy-rich--format-icon
       (if (symbolp icon)
           (all-the-icons-faicon "file-o"
                                 :face 'all-the-icons-dsilver :v-adjust 0.0)
         icon))))

  (defun __adv/around/icon-func-empty-display
      (orig-func &rest orig-args)
    "Around advice for such icon retrieve function to dynamic
display icon or empty string while
`entropy/emacs-icons-displayable-p' return non-nil."
    (if (entropy/emacs-icons-displayable-p)
        (apply orig-func orig-args)
      ""))

  :init
  ;; (entropy/emacs-lazy-with-load-trail
  ;;  'all-the-icons-ivy-rich-mode
  ;;  :pdumper-no-end t
  ;;  (memoize
  ;;   'ya/all-the-icons-ivy-rich-common-file-icon))

  :config
  ;; advice and memoize icon format functions
  (dolist (icon-func
           `(ya/all-the-icon-ivy-rich-common-dir-icon
             ya/all-the-icons-ivy-rich-common-file-icon
             ,@entropy/emacs-ivy-all-the-icon-ivy-rich-icon-funcs
             ))
    (advice-add icon-func
                :around
                #'__adv/around/icon-func-empty-display))

  )

;; **** core
;; config inspired by the ivy config of *centaur-emacs* (https://github.com/seagle0128/.emacs.d)

(use-package ivy-rich
  :commands (ivy-rich-mode)
  :init

  (defun entropy/emacs-ivy--enable-ivy-rich-common ()
    (entropy/emacs-require-only-once
     'ivy
     ;; ensure load the :config part
     'ivy-rich
     'all-the-icons)
    ;; we must ensure that initial `ivy-rich-display-transformers-list'
    ;; after all specification are injected into
    ;; `entropy/emacs-ivy-rich-extra-display-transformers-list'.
    (setq
     ivy-rich-display-transformers-list
     (entropy/ivy--ivy-rich-set-transformers-list))
    (ivy-rich-mode +1)
    (unless (bound-and-true-p ivy-mode)
      (ivy-mode +1)))

  (when (eq entropy/emacs-ivy-rich-type 'ivy-rich-mode)
    ;; Setting tab size to 1, to insert tabs as delimiters
    (add-hook 'minibuffer-setup-hook
              (entropy/emacs-defalias '__eemacs-minibuffer-tab-with-set-for-ivy
                (lambda nil
                  (when (and (eq entropy/emacs-command-completion-use-style 'ivy)
                             (bound-and-true-p ivy-mode))
                    (setq tab-width
                          (or (and (bound-and-true-p ivy-rich-mode) 1)
                              (entropy/emacs-get-symbol-defcustom-value 'tab-width)))))
                "Set minibuffer `tab-width' proprly with `ivy-mode' customization."))
    (add-hook 'ivy-rich-mode-hook
              (lambda (&rest _)
                (setq ivy-virtual-abbreviate
                      (or (and ivy-rich-mode 'abbreviate) 'name))))
    (cond
     (entropy/emacs-fall-love-with-pdumper
      (add-hook 'entropy/emacs-pdumper-load-hook
                #'entropy/emacs-ivy--enable-ivy-rich-common))
     (t
      (entropy/emacs-lazy-initial-for-hook
       '(ivy-mode-hook)
       "ivy-rich-init" "ivy-rich-init"
       :prompt-type 'prompt-echo
       :pdumper-no-end t
       (entropy/emacs-ivy--enable-ivy-rich-common)))))

  :config
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Initial eemacs specific ivy-rich columns
  (defvar entropy/ivy--ivy-rich-display-transformers-list nil)
  (defvar entropy/ivy--ivy-rich-candi-width/non-docstring nil
    "Default candi width for non-doc show, nil for disable.")
  (defvar entropy/ivy--ivy-rich-candi-width/with-docstring 60
    "Default candi width for doc show, nil for disable.")
  (defvar entropy/emacs-ivy-rich-show-docstring-p nil
    "")
  (defun _ivy-rich-use-doc-width ()
    (if entropy/emacs-ivy-rich-show-docstring-p
        entropy/ivy--ivy-rich-candi-width/with-docstring
      entropy/ivy--ivy-rich-candi-width/non-docstring))
  (defun _ivy-rich-use-doc-func-elisp-var ()
    (if entropy/emacs-ivy-rich-show-docstring-p
        'ivy-rich-counsel-variable-docstring
      (lambda (&rest _) "")))
  (defun _ivy-rich-use-doc-func-elisp-func ()
    (if entropy/emacs-ivy-rich-show-docstring-p
        'ivy-rich-counsel-function-docstring
      (lambda (&rest _) "")))
  (defun _ivy-rich-use-icon-func (icon-func &optional empty)
    (if (or empty
            (not (eq entropy/emacs-ivy-rich-type 'ivy-rich-mode)))
        `(,(lambda (&rest _) ""))
      `(,icon-func)))

  (defun __ya/ivy-rich-normalize-width (str len &optional right-aligned)
    "Like `ivy-rich-normalize-width' but using ascii dot seqs instead
of the \"…\" since its font width may not be noarmalized with
currnt fontset."
    (let ((str-len (string-width str)))
      (cond ((< str-len len)
             (if right-aligned
                 (concat (make-string (- len str-len) ? ) str)
               (concat str (make-string (- len str-len) ? ))))
            ((<= len (- str-len)) "")
            ((> str-len len)
             (truncate-string-to-width str len 0 nil "..."))
            (t str))))
  (advice-add 'ivy-rich-normalize-width
              :override #'__ya/ivy-rich-normalize-width)

  (defun entropy/ivy--ivy-rich-set-transformers-list ()
    (let* ((dcw-w entropy/ivy--ivy-rich-candi-width/with-docstring)
           ;; (dcw-n entropy/ivy--ivy-rich-candi-width/non-docstring)
           (nwth-func
            (lambda (str)
              (ivy-rich-normalize-width
               str
               (max (- (window-width (minibuffer-window)) 32)
                    dcw-w))))
           (npred-func
            (lambda (cand) (get-buffer cand)))
           rtn)
      (entropy/emacs-setf-by-body rtn
        `(ivy-switch-buffer
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-buffer-icon)
            (ivy-rich-candidate
             (:width ,nwth-func))
            (ivy-rich-switch-buffer-size
             (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align left))
            (ivy-rich-switch-buffer-major-mode
             (:width 12 :face warning)))
           :predicate ,npred-func
           :delimiter "\t")
          ;; --------------------
          entropy/emacs-popwin-shackle-popup-buffer
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-buffer-icon)
            (ivy-rich-candidate
             (:width ,nwth-func))
            (ivy-rich-switch-buffer-size
             (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align left))
            (ivy-rich-switch-buffer-major-mode
             (:width 12 :face warning)))
           :predicate ,npred-func
           :delimiter "\t")
          ;; --------------------
          counsel-find-file
          (:columns
           (,(_ivy-rich-use-icon-func 'ya/all-the-icons-ivy-rich-common-file-icon)
            (ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          entropy/emacs-popwin-shackle-popup-find-file
          (:columns
           (,(_ivy-rich-use-icon-func 'ya/all-the-icons-ivy-rich-common-file-icon)
            (ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-M-x
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-function-icon)
            (counsel-M-x-transformer
             (:width ,(_ivy-rich-use-doc-width)))
            (,(_ivy-rich-use-doc-func-elisp-func)
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-describe-function
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-function-icon)
            (counsel-describe-function-transformer
             (:width ,(_ivy-rich-use-doc-width)))
            (,(_ivy-rich-use-doc-func-elisp-func)
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-describe-variable
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-variable-icon)
            (counsel-describe-variable-transformer
             (:width ,(_ivy-rich-use-doc-width)))
            (,(_ivy-rich-use-doc-func-elisp-var)
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-recentf
          (:columns
           (,(_ivy-rich-use-icon-func 'ya/all-the-icons-ivy-rich-common-file-icon)
            (ivy-rich-candidate
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-list-processes
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-process-icon)
            (ivy-rich-candidate (:width 25))
            (all-the-icons-ivy-rich-process-id (:width 7 :face all-the-icons-ivy-rich-process-id-face))
            (all-the-icons-ivy-rich-process-status (:width 7 :face all-the-icons-ivy-rich-process-status-face))
            (all-the-icons-ivy-rich-process-buffer-name (:width 25 :face all-the-icons-ivy-rich-process-buffer-face))
            (all-the-icons-ivy-rich-process-tty-name (:width 12 :face all-the-icons-ivy-rich-process-tty-face))
            (all-the-icons-ivy-rich-process-thread (:width 12 :face all-the-icons-ivy-rich-process-thread-face))
            (all-the-icons-ivy-rich-process-command (:face all-the-icons-ivy-rich-process-command-face)))
           :delimiter "\t")
          ;; --------------------
          counsel-package
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-package-icon)
            (ivy-rich-candidate (:width 0.3))
            (all-the-icons-ivy-rich-package-version (:width 16 :face all-the-icons-ivy-rich-version-face))
            (all-the-icons-ivy-rich-package-archive-summary (:width 7 :face all-the-icons-ivy-rich-archive-face))
            (all-the-icons-ivy-rich-package-install-summary (:face all-the-icons-ivy-rich-package-status-installed-face)))
           :delimiter "\t")
          ;; --------------------
          package-install
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-package-icon)
            (ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary
             (:face font-lock-doc-face)))
           :delimiter "\t")
          ;; --------------------
          package-reinstall
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-package-icon)
            (ivy-rich-candidate (:width 0.3))
            (ivy-rich-package-version (:width 16 :face all-the-icons-ivy-rich-version-face))
            (ivy-rich-package-archive-summary (:width 7 :face all-the-icons-ivy-rich-archive-face))
            (ivy-rich-package-install-summary (:face all-the-icons-ivy-rich-package-status-installed-face)))
           :delimiter "\t")
          ;; --------------------
          package-delete
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-projectile-switch-project
          (:columns
           ((ya/all-the-icon-ivy-rich-common-dir-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-projectile-find-file
          (:columns
           ((ya/all-the-icons-ivy-rich-common-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          ;; --------------------
          counsel-projectile-find-dir
          (:columns
           ((all-the-icons-ivy-rich-dir-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          ;; --------------------
          counsel-major
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-mode-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-minor
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-mode-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-imenu
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-imenu-icon)
            (ivy-rich-candidate))
           :delimiter "\t")

          ;; --------------------
          ;;
          ;; ;; EEMACS_MAINTENANCE:  need to use newer version of `company-box'
          ;; counsel-company
          ;; (:columns
          ;;  (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-company-icon)
          ;;   (ivy-rich-candidate))
          ;;  :delimiter "\t")

          ;; --------------------
          counsel-fonts
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-font-icon)
            (ivy-rich-candidate
             (:width 30)
             (:delimiter " "))
            ;; inspired by `counsel--font-with-sample'.
            (,(lambda (font-name)
                (format
                 "%s"
                 (propertize "世界你好abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             'face (list :family font-name))))))
           :delimiter "\t")
          ;; --------------------
          counsel-find-library
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-load-library
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-load-theme
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-world-clock
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-world-clock-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-tramp
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-tramp-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          ;; --------------------
          counsel-git-checkout
          (:columns
           (,(_ivy-rich-use-icon-func 'all-the-icons-ivy-rich-git-branch-icon)
            (ivy-rich-candidate))
           :delimiter "\t")

          ))
      ;; append customized specification
      (dolist (cus-spec entropy/emacs-ivy-rich-extra-display-transformers-list)
        (entropy/emacs-nconc-with-setvar-use-rest rtn cus-spec))
      rtn))

  (defun entropy/emacs-ivy-rich-toggle-doc-show (&optional type)
    (interactive)
    (progn
      (ivy-rich-mode 0)
      (setq entropy/emacs-ivy-rich-show-docstring-p
            (if type
                (cond ((eq type 'enable)
                       t)
                      ((eq type 'disable)
                       nil))
              (not entropy/emacs-ivy-rich-show-docstring-p)))
      (setq
       ivy-rich-display-transformers-list
       (entropy/ivy--ivy-rich-set-transformers-list))
      (ivy-rich-mode 1)))
  )


;; ** Powerful searcher
;; *** helm ag
(use-package helm-ag
  :if (string= entropy/emacs-search-program "ag")
  :commands (helm-do-ag helm-do-ag-project-root)
  :defines (helm-ag-base-command
            helm-ag-use-grep-ignore-list)
  :config
  ;; case-sensitive for ag search command.
  (setq helm-ag-base-command
        "ag --nocolor --nogroup --case-sensitive"
        helm-ag-use-grep-ignore-list t)
  (dolist (el '(helm-do-ag helm-do-ag-project-root))
    (advice-add el :around
                #'entropy/emacs-lang-use-utf-8-ces-around-advice)))

;; *** rg

(use-package rg
  :if (string= entropy/emacs-search-program "rg")
  :commands
  (rg rg-project)
  :init
  (setq rg-command-line-flags (list "--pcre2"))
  :config
  (dolist (func '(rg rg-project))
    (advice-add func
                :around
                #'entropy/emacs-lang-use-utf-8-ces-around-advice)))

;; *** hydra for searcher

(entropy/emacs-lazy-initial-advice-after
 '(find-file switch-to-buffer)
 "powerful-searcher-hydra-hollow-init"
 "powerful-searcher-hydra-hollow-init"
 :prompt-type 'prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-common-individual-hydra-define
  'powerful-searcher nil
  (if (string= entropy/emacs-search-program "ag")
      '("Powerful Searcher"
        (("C-c j" helm-do-ag "Helm AG Search"
          :enable t :exit t :global-bind t)
         ("C-c k" helm-do-ag-project-root "Helm AG search for project root"
          :enable t :exit t :global-bind t)))
    '("Powerful Searcher"
      (("C-c j" rg "Ripgrep for location selected"
        :enable t :exit t :global-bind t)
       ("C-c k" rg-project "Ripgrep for current project"
        :enable t :exit t :global-bind t)))))

 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Utils"
    (("u s"
      (:eval
       (entropy/emacs-hydra-hollow-category-common-individual-get-caller
        'powerful-searcher))
      "Powerful searcher"
      :enable t :exit t)))))

;; ** Powerful find-file
(use-package find-file-in-project
  :commands (ffip-find-files
             entropy/emacs-ivy-ffip
             entropy/emacs-ivy-ffip-directory-only
             entropy/emacs-ivy-ffip-symbolink-only)
  :eemacs-indhc
  (((:enable t :defer (:data
                       (:adfors
                        (counsel-mode-hook
                         ivy-mode-hook
                         window-configuration-change-hook)
                        :adtype hook
                        :pdumper-no-end t)))
    (recursive-find-file))
   ("Recursive file system search (find-file recursive match)"
    (("C-x M-f" entropy/emacs-ivy-ffip "Fuzzy Open File"
      :enable t :exit t :global-bind t)
     ("C-x M-d" entropy/emacs-ivy-ffip-directory-only "Fuzzy Open File Under Directory"
      :enable t :exit t :global-bind t)
     ("C-x M-l" entropy/emacs-ivy-ffip-symbolink-only "Fuzzy Open symbolinks Under Directory"
      :enable t :exit t :global-bind t))))
  :eemacs-tpha
  (((:enable t :defer (:data
                       (:adfors
                        (counsel-mode-hook
                         ivy-mode-hook
                         window-configuration-change-hook)
                        :adtype hook
                        :pdumper-no-end t))))
   ("Utils"
    (("u f"
      (:eval (entropy/emacs-hydra-hollow-category-common-individual-get-caller
              'recursive-find-file))
      "Find file recursively in specified root"
      :enable t :exit t))))
  :init
  ;; Using rust 'fd' for perfomance improving
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))
  ;; always show the executed command and debug information
  (setq ffip-debug t)
  ;; always disable `ffip-find-files-history' since its large when
  ;; locating for large location which will enlarge emacs RAM usage.
  (setq ffip-find-files-history-max-items 0)

  :config

  (defvar __ffip-find-symbolic-only-p nil)
  ;; EEMACS_MAINTENANCE: follow upstream update
  (defun __ffip-create-shell-command-for-symbolic (keyword find-directory-p)
    "Like `ffip-create-shell-command' but for symolic only."
    (let* (cmd fmt tgt)
      (cond
       (ffip-use-rust-fd
        ;; `-H` => search hidden files
        ;; `-E` => exclude pattern
        ;; `-c` => color
        ;; `-i` => case insensitive
        ;; `-t` => directory (d) or file (f)
        ;; `-p` => match full path
        (setq fmt (concat "%s %s -c never -H -i -t %s %s %s %s"
                          (if ffip-rust-fd-respect-ignore-files "" " -I")
                          (if ffip-match-path-instead-of-filename " -p" "")
                          " "
                          ffip-rust-fd-extra-opts
                          " %s"))
        ;; fd use regular expression for target pattern (but glob pattern when excluding, sigh)
        (setq tgt (if keyword (format "\".*%s\"" keyword) "")))
       (t
        (setq tgt
              (if find-directory-p (format "-iwholename \"*%s\"" keyword)
                (ffip--create-filename-pattern-for-gnufind keyword)))
        (setq fmt (concat "%s "
                          ffip-find-pre-path-options
                          " . \\( %s \\) -prune -o -type %s %s %s %s %s -print"))))

      (setq cmd (format fmt
                        (ffip--executable-find)
                        (ffip--prune-patterns)
                        "l"
                        (ffip--ignore-file-names)
                        ffip-find-options
                        (ffip--join-patterns ffip-patterns)
                        tgt))
      cmd))
  (defun __ya/ffip-create-shell-command (orig-func &rest orig-args)
    (if __ffip-find-symbolic-only-p
        (apply '__ffip-create-shell-command-for-symbolic orig-args)
      (apply orig-func orig-args)))
  (advice-add 'ffip-create-shell-command :around #'__ya/ffip-create-shell-command)

  ;; EEMACS_MAINTENANCE: follow upstream update
  (defun __ya/ffip--prune-patterns (orig-func &rest orig-args)
    "Bug fix when use rust fd as that `ffip-use-rust-fd' is non-nil
which can not set -E with \"\" empty arg for find cause nothing
to find as well."
    (if (and (bound-and-true-p ffip-use-rust-fd)
             (and (string-match-p
                   "^ *$"
                   (mapconcat (lambda (p)
                                p)
                              ffip-prune-patterns " "))))
        ""
      (apply orig-func orig-args)))
  (advice-add 'ffip--prune-patterns :around #'__ya/ffip--prune-patterns)


  (defun ffip-completing-read (prompt collection &optional action)
    "Read a string in minibuffer, with completion.

PROMPT is a string with same format parameters in `ido-completing-read'.
COLLECTION is a list of strings.

ACTION is a lambda function to call after selecting a result.

This function returns the selected candidate or nil.

NOTE: this function has been redefined by =entropy-emacs=."
    (let* (selected)
      (cond
       ;;; NOTE: We comment this block for force check only one candi in
       ;;; collection so that we can view the candi length in ivy
       ;;; frame-work for more beneficent.
       ;;; |||||
       ;;; vvvv
       ;; ((and action (= 1 (length collection)))
       ;;  ;; select the only candidate immediately
       ;;  (setq selected (car collection)))
       (ffip-prefer-ido-mode
        ;; ido can only handle list of strings
        (setq selected (ido-completing-read prompt (mapcar 'car collection))))
       (t
        (setq selected (completing-read prompt collection))
        (setq selected (or (assoc selected collection) selected))))
      (when selected
        ;; make sure only the string/file is passed to action
        (let* ((default-directory
                 (entropy/emacs-return-as-default-directory
                  (ffip-get-project-root-directory)))
               (result (if (consp selected) (cdr selected) selected)))
          (if action (funcall action result) result)))))

  (defun entropy/emacs-ivy-ffip (&optional pseudo-interaction)
    "Find file using `find-file-in-project' in place.

Just find directory when PSEUDO-INTERACTION was non-nil (the prefix
with `C-u').

NOTE: this function ignores all /ignore-patterns/ both included
in `ffip-ignore-filenames' and `ffip-prune-patterns' even for
`ffip-rust-fd-respect-ignore-files', which means that all file
under place will be detected!"
    (interactive "P")
    (let (prompt-func
          (ffip-use-rust-fd (bound-and-true-p current-prefix-arg)))
      (setq prompt-func
            (lambda ()
              (let (target)
                (setq target
                      (read-directory-name
                       (format
                        "[%s] Choose Place Root: "
                        (if ffip-use-rust-fd "fd-find" "find"))
                       nil nil t))
                (unless (file-directory-p target)
                  (setq target (file-name-directory target)))
                target)))
      (let ((ffip-project-root (funcall prompt-func))
            (ffip-ignore-filenames nil)
            (ffip-rust-fd-respect-ignore-files nil)
            (ffip-prune-patterns '("")))
        (cond ((eq pseudo-interaction 'dir)
               (ffip-find-files "" nil t))
              ((eq pseudo-interaction 'symlnk)
               (let ((__ffip-find-symbolic-only-p t))
                 (ffip-find-files nil nil)))
              (t
               (ffip-find-files nil nil))))))

  (defun entropy/emacs-ivy-ffip-directory-only ()
    "Find directory using `find-file-in-project' in place."
    (declare (interactive-only t))
    (interactive)
    (funcall 'entropy/emacs-ivy-ffip 'dir))

  (defun entropy/emacs-ivy-ffip-symbolink-only ()
    "Find directory using `find-file-in-project' in place."
    (declare (interactive-only t))
    (interactive)
    (funcall 'entropy/emacs-ivy-ffip 'symlnk))

  )

;; ** provide
(provide 'entropy-emacs-ivy)
