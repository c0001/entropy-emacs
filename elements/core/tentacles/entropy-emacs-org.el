;;; entropy-emacs-org.el --- entropy emacs org-mode configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-org.el
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
;; Sets of configuration based on emacs `Org-Mode' which be of the
;; rich plain-text file management-system.
;;
;; In this file, some hacking and patches for `Org-Mode' was
;; defaultly and needed by `entropy-emacs's designation. Thus,
;; `entropy-emacs' using specific Org version release to keep the
;; config efficiently, for now it is "v9.1.9".
;;
;; For that features adding based on org-mode are all tools tiding
;; for thus , `org-download', `org-bullet', stuffs of extensions
;; based on it etc.
;;
;; * Configuration:
;;
;; Loasing by `entropy-emacs' automatically without hacking warranty.
;;
;; * Code:

;; ** require

;; ** Pre advice
(defun entropy/emacs-org--export-panel-around-advice (old-func &rest args)
  (unless (fboundp 'org-reveal-export-to-html)
    (require 'ox-reveal))
  (let ((entropy/emacs-web-development-environment t))
    (apply old-func args)))

(entropy/emacs-lazy-load-simple ox
  (advice-add 'org-export-dispatch
              :around #'entropy/emacs-org--export-panel-around-advice))


;; ** main
;; *** core
(use-package org
  :ensure nil
  :commands (org-mode
             org-store-link
             org-agenda
             org-capture
             org-switchb
             org-previous-item
             org-next-item
             org-toggle-link-display
             org-babel-result-hide-all)

;; **** eemacs top binding keys

  :eemacs-tpha
  (((:enable t :defer t))
   ("Org"
    (("C-c a" org-agenda "Dispatch agenda commands to collect entries to the agenda buffer"
      :enable t
      :exit t
      :global-bind t)
     ("C-c c" org-capture "Capture something via Org-Mode"
      :enable t
      :exit t
      :global-bind t)
     ("C-c b" org-switchb "Switch between Org buffers"
      :enable t
      :exit t
      :global-bind t))))

;; **** hook
  :hook ((org-mode . org-babel-result-hide-all))

;; **** preface
  :preface


;; **** init
  :init

  (entropy/emacs-lazy-initial-advice-before
   (org-mode)
   "org-mode-init" "org-mode-init" prompt-echo
   :pdumper-no-end t
   ;; Patch org-mode first customized invoking procedure with more
   ;; prompts for reducing nervous motion
   (entropy/emacs-run-hooks-with-prompt
    ;; require all ob-* features
    ;; FIXME: use org API `org-babel-do-load-languages' instead?
    (dolist (feature entropy/emacs-org-babel-featurs)
      (require feature))))

;; **** configs
  :config
;; ***** UI
;; ****** startup UI
  ;; Forcely trucate line for `org-mode' buffer
  (setq org-startup-truncated t)

  ;; Hiding block details at init-time
  (setq-default org-hide-block-startup t)

;; ****** entities

  ;; ;; Show entities as UTF8 characters defautly
  ;; (setq org-pretty-entities t
  ;;       org-pretty-entities-include-sub-superscripts t)

;; ***** Cycle

  ;; Set the empty line number between the headline without visible
  (setq org-cycle-separator-lines 2)

  ;; Vixual cycle with plain list
  (setq-default org-cycle-include-plain-lists 'integrate)

;; ***** Todo
;; ******* todo priority
  ;; lowset prioty was 'Z'
  (setq org-lowest-priority 90)
  ;; default prioty was 'C'
  (setq org-default-priority 67)

;; ******* todo log type

  ;; Insert time-stamp when toggle 'TODO' to 'DONE'
  (setq org-log-done 'time)

;; ***** Link
  ;; Do not hexify url link
  (when (boundp 'org-url-hexify-p)
    ;; disable it when org-version under than 9.3
    (setq org-url-hexify-p nil))

  ;; Using relative path insert type defauly so that we can attachs
  ;; things baed on org file.
  (setq-default org-link-file-path-type 'relative)

  ;; Using fuzzy match for org external link
  ;; which support the link type:
  ;; 'file:xxx.org::str'
  (setq org-link-search-must-match-exact-headline nil)

;; ***** Export

  ;; Export heading level counts same as raw org that maximum for 8
  (defvar org-export-headline-levels)
  (setq org-export-headline-levels 8)

  ;; Interpret entities when exporting
  (defvar org-export-with-entities)
  (setq org-export-with-entities t)

;; ***** Latex
;; ****** formula convertor
  ;; Choosing org formula convertor
  (cond
   ((and sys/is-graphic-support
         entropy/emacs-imagemagick-feature-p
         (not sys/is-win-group))
    (setq org-preview-latex-default-process 'imagemagick)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
    (defun entropy/emacs-org--latex-preview-fragment-with-imagemagick-warn
        (orig-func &rest orig-args)
      (let (rtn indc)
        (unwind-protect
            (progn
              (setq rtn (apply orig-func orig-args))
              (setq indc t)
              rtn)
          (unless indc
            (warn "
=============== org latex formula previewer warning =============

You are using imagemagick to convert latex formula snippet which
must satisfied follow requirements:

- installed Imagemagick7 and pdflatex command (this can be
  installed by a latex distribution fully)

- Imagemagick7's policy are enabled of 'all' type, which you can
  see the =policy.xml= under it configuration dir, follow the
  following modification:

``` xml
<policy domain=\"delegate\" rights=\"all\" pattern=\"gs\" />
                             <!-- ^^^  origin maybe 'none' -->
```
"
                  )))))
    (advice-add 'org-latex-preview
                :around
                #'entropy/emacs-org--latex-preview-fragment-with-imagemagick-warn))

   (sys/win32p
    (setq org-preview-latex-default-process 'dvisvgm)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))))

;; ***** Refile

  (defvar org-refile-targets)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  ;; Refile in a single go defautly since we use fuzzy
  ;; `completing-read' framework that no need to search via the
  ;; hierarchy which is low efficiency.
  (defvar org-outline-path-complete-in-steps)
  (setq org-outline-path-complete-in-steps nil)

  ;; allow refile to the root of an org file
  (defvar org-refile-use-outline-path)
  (setq org-refile-use-outline-path 'file)

  ;; Make `org-refile' inhibiting the read only status
  (entropy/emacs-make-function-inhibit-readonly
   'org-refile)

  ;; Make refile cache enabled defaultly.
  (defvar org-refile-use-cache)
  (setq org-refile-use-cache t)

;; ***** Imenu integration

  ;; The default depth shown for integrating org heading line to imenu
  ;; while be within org-mode
  (setq org-imenu-depth 8)

;; ***** Hack
;; ****** disable native TAB behaviour in src block

  ;; No effect of TAB in a code block is as if it were issued in the language major mode buffer and
  ;; fix yas <src expanding error for `nil-mode'

  ;; 1. Firstly we do not want to emulate TAB in src block in as where
  ;; as the major-mode taken buffer.
  ;;
  ;; 2. According to
  ;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
  ;;
  ;; Issues With Yasnippet In Emacs 27.2 (Lisp error: (error "No suchlanguage mode: nil-mode"))
  ;;
  ;; The problem here is that Yasnippet tries to indent code after
  ;; snippet expansion, and to indent code in the #+BEGIN_SRC block,
  ;; Org mode needs to enter this block and execute language
  ;; appropriate indent rules. Org tries to enter this block, but
  ;; there's no language specified, hence the error. You can see this
  ;; in the stacktrace:
  ;;
  ;; #+begin_example
  ;; error("No such language mode: %s" nil-mode)
  ;; org-edit-src-code() ;; Org decides to temporarely enter the indirect buffer
  ;; org-babel-do-key-sequence-in-edit-buffer("\11") ;; Org tries to indent
  ;; org-indent-line() ;; asks Org to indent
  ;; indent-according-to-mode() ;; tries to meaningfully indent
  ;; yas--indent-region(...) ;; tries to indent
  ;;
  ;; #+end_example
  ;;
  (setq org-src-tab-acts-natively nil)

;; ****** define 'end' key to `org-end-of-line' to adapt to org specifications
  (define-key org-mode-map (kbd "<end>") 'org-end-of-line)

;; ****** `org-open-at-point' enhanced
  (defun entropy/emacs-org-open-at-point ()
    "Like `org-open-at-point' but:

Change the find-file method of org-open-at-point instead of
find-file-other-window"
    (declare (interactive-only t))
    (interactive nil org-mode)
    (let ((org-link-frame-setup
           (entropy/emacs-cl-compatible-apply
            acons
            'file
            'find-file org-link-frame-setup)))
      (org-open-at-point)))

  ;; using entropy-open-with to open org link
  (defun entropy/emacs-org-eow ()
    "Open link in org-mode using `entropy/open-with-port'."
    (declare (interactive-only t))
    (interactive nil org-mode)
    (require 'entropy-open-with)
    (let* ((link (nth 1 (org-element-lineage
                         (org-element-context)
                         '(link) t)))
           (link-type (plist-get link :type))
           (path (plist-get link :path)))
      (entropy/open-with-port
       nil
       (cond ((string= "file" link-type)
              (expand-file-name (url-unhex-string path)))
             ((string-match-p "https?" link-type)
              (concat link-type ":" path))
             (t (error (format "Invalid file type '%s'!" link-type)))))))

;; ****** org inline image toggle enhancement
  (when entropy/emacs-imagemagick-feature-p
    (progn
      (setq org-image-actual-width nil)
      (defun entropy/emacs-org--otii-before-advice (orig-func &rest orig-args)
        "Advice for `org-toggle-inline-images' when emacs was
build with imagemagick, because of that org-mode will have the
ability to display GIF type image whatever it's size be, so it
will cause the large lagging performance when the file link
directed to large gif file when willing display images in current
buffer."
        (if (not (yes-or-no-p
                  "Toggle inline images in org-mode will spend sometime and causing lagging performance, continue? "))
            (message "Abort displaying inline images")
          (apply orig-func orig-args)))
      (advice-add 'org-display-inline-images
                  :around #'entropy/emacs-org--otii-before-advice)))

;; ****** org head line 'CUSTOM-ID' generator

  ;; Inspired by:
  ;; https://writequit.org/articles/emacs-org-mode-generate-ids.html#h-cf29e5e7-b456-4842-a3f7-e9185897ac3b

;; ******* baisc function
  (defun entropy/emacs-org--custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will be passed through to `org-id-new'. In any case, the
CUSTOM_ID of the entry is returned."
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

;; ******* interactive function

  (defun entropy/emacs-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive nil org-mode)
    (org-map-entries
     (lambda ()
       (entropy/emacs-org--custom-id-get (point) 'create))
     t nil))

  ;; Auto detected '#+OPTIOINS: auto-id:t' and maping adding id for heads
  (defun entropy/emacs-org-auto-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one. Only adds ids if the
`auto-id' option is set to `t' in the file somewhere. ie,
#+OPTIONS: auto-id:t"
    (interactive nil org-mode)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries
         (lambda () (entropy/emacs-org--custom-id-get (point) 'create))
         t nil))))

;; ******* auto insert function

  (defun entropy/emacs-org-auto-add-org-ids-before-save ()
    "Auto add id into all heads of current org buffer before save
it to file when the org option key notation ':autoclose' has
enabled at current org buffer. "
    (when (and (eq major-mode 'org-mode)
               (eq buffer-read-only nil))
      (entropy/emacs-org-auto-add-ids-to-headlines-in-file)))
  (add-hook 'before-save-hook
            #'entropy/emacs-org-auto-add-org-ids-before-save)

;; ****** org file apps patches
;; ******* backends
;; ******** html open app
  (defun entropy/emacs-org--hexpt-function (&optional path link)
    "Function embeded into `org-file-apps', used for
`entropy/emacs-org--hexpt-advice' to open html file using
`browse-url-browser-function' based on
`entropy/emacs-browse-url-function-get-for-web-preview'."
    (if (and (stringp link)
             (stringp path)
             (file-exists-p path))
        (let ((browse-url-browser-function
               (entropy/emacs-browse-url-function-get-for-web-preview)))
          (browse-url
           (concat "file:///"
                   (url-hexify-string link entropy/emacs-url-allowed-chars))))
      (error "Invalid link '%s' with path '%s'" link path)))

;; ******* main
  (defun entropy/emacs-org--specified-file-open-operations ()
    "Patch `org-file-apps' with some specification locally."
    (let ((map-list
           '(("\\.x?m?html?\\'" . entropy/emacs-org--hexpt-function)
             ("\\.[gG]if\\'" . system)
             ("\\.pdf\\'" . system))))
      (setq-local org-file-apps
                  (append map-list org-file-apps))))

  (add-hook 'org-mode-hook
            #'entropy/emacs-org--specified-file-open-operations)

;; ****** inhibit org defaultly open directory link using `explorer.exe' in windows

  (when sys/is-win-group
    ;; Remove the `directory' entries fistly
    (setq org-file-apps
          (delete nil
                  (mapcar (lambda (x)
                            (unless (eq (car x) 'directory)
                              x))
                          org-file-apps)))
    ;; then inject the defautly one
    (add-to-list 'org-file-apps '(directory . emacs)))

;; **** ___end___
  )

;; *** org-agenda
(use-package org-agenda
  :ensure nil
  :after org
  :commands
  (org-agenda)
  :hook
  ((org-agenda-mode . hl-line-mode))
  :init

  ;; Set agenda daily show as defaulta
  (setq-default org-agenda-span 'day)

  ;; Gloable tags match
  (setq-default org-complete-tags-always-offer-all-agenda-tags t)

  ;; Agenda prefix visual type
  (if sys/is-graphic-support
      (progn
        (setq org-agenda-prefix-format
              '((agenda . " ✪ %?-12t% s")
                (todo . " ✈ ")
                (tags . " ☯ ")
                (search . " ✔ "))))
    (setq org-agenda-prefix-format
          '((agenda . " agenda: %?-12t% s")
            (todo . " Plan: ")
            (tags . " Tags-Match: ")
            (search . " Search: "))))

  ;; org-tags-match-list-sublevels
  (setq org-tags-match-list-sublevels nil)
  (defun entropy/emacs-org-toggle-agenda-subshow-and-heading-levels ()
    "Toggle `org-agenda' tag exhibited visual type."
    (interactive nil org-mode)
    (if (and (string= "*Org Agenda*" (buffer-name))
             (eq major-mode 'org-agenda-mode))
        (progn
          (if (eq org-tags-match-list-sublevels nil)
              (setq org-tags-match-list-sublevels 'indented)
            (setq org-tags-match-list-sublevels nil))
          (org-agenda-redo-all))
      (error "You must use it in org agenda buffer!")))

;; **** config
  :config
  ;; agenda tag set interaction enhancement
  ;; TODO: migrate to eemacs hydra
  (when (and (or (featurep 'counsel)
                 (ignore-errors (require 'counsel)))
             (fboundp 'counsel-org-tag)
             (fboundp 'counsel-org-tag-agenda))
    (define-key org-agenda-mode-map
      (kbd "C-c C-q") #'counsel-org-tag-agenda))

  ;; TODO: add `org-agenda' eemacs hydra


  )

;; *** org-capture
(use-package org-capture
  :ensure nil
  :after org
  :commands (org-capture)
  :config
;; **** org-capture about
;; ***** hook for org-capture
  (defun entropy/emacs-org--capture-indent-buffer (&rest _)
    "Indent org capture buffer when finished capture editting."
    (when org-adapt-indentation
      (let ((pm (point-min))
            (pb (point-max)))
        (org-indent-region pm pb)
        (goto-char (point-max))
        (insert "\n"))))
  (add-hook 'org-capture-prepare-finalize-hook
            #'entropy/emacs-org--capture-indent-buffer)

  (defun entropy/emacs-org--capture-set-tags (&rest _)
    "Adding org tags using `counsel-org-tag' after placed org capture template.

This function was the after advice for `org-capture-template'."
    (condition-case nil
        (when (fboundp 'counsel-org-tag)
          (save-excursion
            (funcall #'counsel-org-tag)))
      ((error quit)
       (message "Cancel adding headline tags."))))

  (advice-add 'org-capture-place-template
              :after #'entropy/emacs-org--capture-set-tags
              '((depth . -100)))        ;added it at most outsiede of
                                        ;advice list so that prevent
                                        ;any other override advice
                                        ;covered it

;; ***** Do not using `org-toggle-link-display' in capture buffer.

  ;; Because of that if do this will lost the buffer font-lock
  ;; effecting(all buffer be non-font-lock visual) and do not have the
  ;; recovery method unless reopen capture operation.w

  (defun entropy/emacs-org--capture-forbidden-toggle-link-display (&rest _)
    "Advice for `org-toggle-link-display' for forbidden it when in capture buffer.

Because of that if do this will lost the buffer font-lock
effecting(all buffer be non-font-lock visual) and do not have the
recovery method unless reopen capture operation.w
"
    (when (and (string-match-p "^CAPTURE-" (buffer-name))
               (eq major-mode 'org-mode))
      (user-error "Do not toggle link display in 'org capture' buffer.")))

  (advice-add 'org-toggle-link-display
              :before
              #'entropy/emacs-org--capture-forbidden-toggle-link-display)

;; **** ___end___
  )

;; *** org-babel
(use-package ob
  :ensure nil
  :after org
  :config
;; **** org babel evaluate confirm
  (entropy/emacs-lazy-load-simple org
    (when (not (version< org-version "9.1.9"))
      (defvar entropy/emacs-org--src-info nil
        "Current org babel info using for `entropy/emacs-org--babel-comfirm-evaluate'.")

      (defun entropy/emacs-org--set-src-info (old-func &rest args)
        "Around advice for func `org-babel-get-src-block-info'
for obtain current src block info for redistrict into
`entropy/emacs-org--src-info'.

This func built for the reason the unknown mechanism that clean
the block info name part while transport into babel confirmation
func so that the confirming prompt string will not given the
babel name, this will confusing for the user to distinguish which
block current prompting is and then how to judge whether to
evaluate it.. "
        (let ((info (apply old-func args)))
          (setq entropy/emacs-org--src-info info)
          info))

      (advice-add 'org-babel-get-src-block-info
                  :around #'entropy/emacs-org--set-src-info)

      (defun entropy/emacs-org--babel-comfirm-evaluate (old-func _info)
        "This function was the around advice func for
`org-babel-confirm-evaluate' func.

For forcing query prompt whether evaluating src block when dealing
with process of exporting.


============================
For Org version be 9.1.13 or 9.1.19:

Original mechanism for org export will ignore all src evaluating
confirm prompting while src language was indicated in
`org-babel-load-languages' and be loaded by
`org-babel-load-languages'.

The core reason was that the variable `org-export-use-babel' of
these org version was the pointer for controlling whether export
with evaluating src (inline or block) and the default value of it
was 't'. These org version modified lots of internal process
logical which not as that prompt for evaluating just rely on
variable `org-confirm-babel-evaluate' value in 8.xx version.

#+BEGIN_QUOTE
 You can see functon's top branch diversion condition of func
 =org-babel-exp-process-buffer= for proving what I expression.
#+END_QUOTE

This will cause some serious problem when evaluating some
dangerous src block which not indicated the block arg ':eval
query' which variable `org-export-use-babel' declared.

This func forcing setting variable `org-confirm-babel-evaluate'
to t during the comfirmation process, because the core problem
indicated above is that whomever clean the comfirmation value to
nil of this variable before the comfirming process so that the
func `org-babel-confirm-evaluate' was suppressed while evaluating
the block, I think the whom was the variable
`org-export-use-babel' setting state according to the above
problem description.

This function also using `entropy/emacs-org--src-info' passing
for sub-func instead of the origin derived 'info' value as the
reason, please see the docstring refer."
        (let ((org-confirm-babel-evaluate t))
          (funcall old-func entropy/emacs-org--src-info)))

      (advice-add 'org-babel-confirm-evaluate
                  :around #'entropy/emacs-org--babel-comfirm-evaluate)))

;; **** org global export macro
  (defvar org-export-global-macros)
  (entropy/emacs-lazy-load-simple ox
    (add-to-list 'org-export-global-macros
                 '("kbd" . "@@html:<code>$1</code>@@")))

;; **** org babel src mode engines

;; ***** web mode for edit html refer src block
  ;; Method 1: patch `org-src-lang-modes'
  ;; ---------with the bug of none highlites in org mode src html block
  ;; ---------and the issue with none aspiration of web-mode maintainer with link
  ;; ----------------------------->`https://github.com/fxbois/web-mode/issues/636'
  ;; (add-to-list 'org-src-lang-modes '("html" . web))

  ;; Method 2: patch with advice
  (defun entropy/emacs-org--patch-org-src-edit-element-for-web-mode
      (orig-func &rest orig-args)
    "Patch `org-src--edit-element' to calling `web-mode' to edit
some LANGs if `web-mode' is featured."
    (let ((datum (car orig-args))
          (name (nth 1 orig-args))
          (init-func (nth 2 orig-args))
          (rest (cdddr orig-args)))
      (when (member init-func '(html-mode))
        (when (fboundp 'web-mode)
          (setq init-func 'web-mode)))
      (apply orig-func datum name init-func rest)))
  (entropy/emacs-lazy-load-simple org-src
    (advice-add
     'org-src--edit-element
     :around
     #'entropy/emacs-org--patch-org-src-edit-element-for-web-mode))

;; **** ___end___

  )

;; *** org-export
;; **** core
(use-package ox
  :ensure nil
  :after org
;; ***** config
  :config
;; ****** init all ox-* features

  (dolist (feature entropy/emacs-org-export-backends)
    (require feature))

;; ****** html export
;; ******* html exported head coding
  (setq org-html-coding-system 'utf-8-unix)

  ;; inhibit org self default html style
  (setq org-html-head-include-scripts nil
        org-html-head-include-default-style nil)

;; ******* fix the bug when cjk paragraph exported to html has the auto newline space char including
  ;; This hacking refer to https://emacs-china.org/t/org-mode-html/7174#breadcrumb-0
  ;; and raw from spacemac layer: https://github.com/syl20bnr/spacemacs/blob/develop/layers/+intl/chinese/packages.el#L104
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))


;; ******* org ignore broken links
  (setq org-export-with-broken-links 'mark)

;; ***** ___end___
  )

;; **** ox-reveal
(use-package ox-reveal
  :after ox
  :defines (org-reveal-width
            org-reveal-height
            org-reveal-margin
            org-reveal-min-scale
            org-reveal-max-scale
            org-reveal-control
            org-reveal-center
            org-reveal-progress
            org-reveal-history
            )
  :init
  ;; adding the backends to the general variable
  (add-to-list 'entropy/emacs-org-export-backends
               'ox-reveal)
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-theme "black")
  (setq org-reveal-width 1200)
  (setq org-reveal-height 1000)
  (setq org-reveal-margin "0.1")
  (setq org-reveal-min-scale "0.5")
  (setq org-reveal-max-scale "2.5")
  (setq org-reveal-transition "cube")
  (setq org-reveal-plugins '(classList markdown zoom notes))
  (setq org-reveal-control t)
  (setq org-reveal-center t)
  (setq org-reveal-progress t)
  (setq org-reveal-history nil))

;; *** org-publish
(use-package ox-publish
  :ensure nil
  :after org
  :commands
  (org-publish-current-file
   org-publish-all
   org-publish
   org-publish-project
   org-publish-current-project
   )
  :config
  ;; Force using the utf-8 coding system while publish process
  (advice-add 'org-publish :around #'entropy/emacs-lang-use-utf-8-ces-around-advice)

  (defun entropy/emacs-org--publish-check-timestamp-around_advice
      (oldfun &rest args)
    "The advice around the org publish cache file timestamp check
function `org-publish-cache-file-needs-publishing'.

Patching for adding query whether force re publishing unmodified
cached refer file.
"
    (let* ((check-result (apply oldfun args))
           (fname (car args))
           (manually-judgement nil))
      (if (not check-result)
          (setq manually-judgement
                (yes-or-no-p (format "Do you want to Re-publish '%s' ?" fname)))
        (setq manually-judgement check-result))
      manually-judgement))

  (advice-add 'org-publish-cache-file-needs-publishing
              :around #'entropy/emacs-org--publish-check-timestamp-around_advice)

  )

;; *** org-ctags
(use-package org-ctags
  :after org
  :ensure nil
  :preface
  (defun entropy/emacs-org--ctags-disable ()
    "Disable `org-ctags' for reset some pollution from here."
    (setplist
     'org-mode
     (let (rtn
           (pt 0)
           (sym-plist (symbol-plist 'org-mode)))
       (while (< pt (- (length sym-plist) 1))
         (when (not (eq (nth pt sym-plist) 'find-tag-default-function))
           (setq rtn (append rtn (list (nth pt sym-plist))))
           (unless (null (nth (+ 1 pt) sym-plist))
             (setq rtn (append rtn (list (nth (+ 1 pt) sym-plist))))))
         (setq pt (+ 2 pt)))
       rtn))
    (setq org-ctags-enabled-p nil)
    (dolist (fn org-ctags-open-link-functions)
      (remove-hook 'org-open-link-functions fn)))
  :init
  (entropy/emacs-lazy-load-simple (org org-ctags)
    (entropy/emacs-org--ctags-disable)))

;; *** org-id
(use-package org-id
  :ensure nil
  :commands org-id-new
  :config

  (defun entropy/emacs-org-id-add-location-around-advice
      (orig-func &rest orig-args)
    "Around advice for `org-id-add-location' for preventing error
popup when in non-file buffer so that contiguous procedures run
normally while that."
    (if (buffer-file-name (buffer-base-buffer))
        (apply orig-func orig-args)
      (ignore-errors
        (message "`org-id-get' expects a file-visiting buffer but for <%s>"
                 (current-buffer))
        (apply orig-func orig-args))))
  (advice-add 'org-id-add-location
              :around
              #'entropy/emacs-org-id-add-location-around-advice)

  )

;; *** org keymap eemacs hydra specifications
;; **** org-mode
;; ***** sub-groups
;; ****** org basic manipulation
;; ******* sub-groups
;; ******** common group
;; ********* sub-groups
;; ********** common insert

(defvar entropy/emacs-org-keymap-group-$common-insert
  '("Common Insert"
    (("C-<return>" org-insert-heading-respect-content
      "Insert heading with 'org-insert-heading-respect-content' set to t"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-," org-insert-structure-template
      "Insert a block structure of the type which babel defined so"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x f" org-footnote-action
      "Do the right thing for footnotes"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-f" org-emphasize
      "Insert or change an emphasis, i.e. a font like bold or italic"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-<return>" org-meta-return
      "Insert a new heading or wrap a region in a table"
      :enable t :map-inject t :exit t) ;; RET/<return> key with modifiers
     )))

;; ********** common copy

(defvar entropy/emacs-org-keymap-group-$common-copy&paste
  '("Common copy and paste"
    (("C-c M-w" org-copy "Like 'org-refile', but copy"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-w" org-refile "Move the entry or entries at point to another heading"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x M-w" org-copy-special
      "Copy region in table or copy current subtree"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-w" org-cut-special "Cut region in table or cut current subtree"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-y" org-paste-special
      "Paste rectangular region into table, or past subtree relative to level"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x v" org-copy-visible "Copy the visible parts of the region"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** common move

(defvar entropy/emacs-org-keymap-group-$common-move
  '("Common move"
    (("C-M-t" org-transpose-element
      "Transpose current and previous elements, keeping blank lines between"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-<down>" org-metadown
      "Move subtree down or move table row down"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-<up>" org-metaup
      "Move subtree up or move table row up"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-<left>" org-metaleft
      "Promote heading, list item at point or move table column left"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-<right>" org-metaright
      "Demote heading, list item at point or move table column right"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("C-c C->" org-demote-subtree
      "Demote the entire subtree."
      :enable t :map-inject t :exit t)
     ("C-c C-<" org-promote-subtree
      "Promote the entire subtree"
      :enable t :map-inject t :exit t))))

;; ********** common archive

(defvar entropy/emacs-org-keymap-group-$common-archive
  '("Common archive"
    (("C-c $" org-archive-subtree
      "Move the current subtree to the archive"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-s" org-archive-subtree
      "Move the current subtree to the archive"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-a" org-archive-subtree-default
      "Archive the current subtree with the default command"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x A" org-archive-to-archive-sibling
      "Archive the current heading by moving it under the archive sibling"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-<tab>" org-force-cycle-archived
      "Cycle subtree even if it is archived"
      :enable t :map-inject t :exit t) ;; TAB key with modifiers
     ("C-c C-x a" org-toggle-archive-tag
      "Toggle the archive tag for the current headline"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** common toggle

(defvar entropy/emacs-org-keymap-group-$common-toggle
  '("Common toggle"
    (("C-c C-x C-b" org-toggle-checkbox
      "Toggle the checkbox in the current line"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c ;" org-toggle-comment
      "Change the COMMENT state of an entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c :" org-toggle-fixed-width
      "Toggle fixed-width markup"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x \\" org-toggle-pretty-entities
      "Toggle the composition display of entities as UTF8 characters"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-*" org-list-make-subtree
      "Convert the plain list at point into a subtree"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C--" org-toggle-item
      "Convert headings or normal lines to items, items to normal lines"
      :enable t :map-inject t :exit t)
     ("C-c -" org-ctrl-c-minus
      "Insert separator line in table or modify bullet status of line"
      :enable t :map-inject t :exit t))))

;; ********** common attachments

(defvar entropy/emacs-org-keymap-group-$common-attachments
  '("Common attachments"
    (("C-c C-a" org-attach "The dispatcher for attachment commands"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** common sort
(defvar entropy/emacs-org-keymap-group-$common-sort
    '("Common sort"
      (("C-c ^" org-sort
        "Call 'org-sort-entries', 'org-table-sort-lines' or 'org-sort-list'"
        :enable t :map-inject t :exit t) ;; All the other keys
       )))

;; ********** common open
(defvar entropy/emacs-org-keymap-group-$common-open
  '("Common open"
    (("C-c C-o" entropy/emacs-org-open-at-point
      "Open link, timestamp, footnote or tags at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c M-o" entropy/emacs-org-eow
      "Open link in org-mode using 'entropy/open-with-port"
      :enable t :exit t :map-inject t)
     )))

;; ********** common edit
(defvar entropy/emacs-org-keymap-group-$common-edit
  '("Common edit"
    (("C-c '" org-edit-special "Call a special editor for the element at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-M-S-<left>" org-decrease-number-at-point
      "Decrement the number at point"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("C-M-S-<right>" org-increase-number-at-point
      "Increment the number at point"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("C-c C-r" org-reveal
      "Show current entry, hierarchy above it, and the following headline"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********* define

(defvar entropy/emacs-org-keymap-group-$common
  '("Common Manipulations"
    (("c i" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-insert :other-rest-args ((org org-mode-map))) "Common Insert"
      :enable t :exit t)
     ("c e" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-edit :other-rest-args ((org org-mode-map))) "Common Edit"
      :enable t :exit t)
     ("c y" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-copy&paste :other-rest-args ((org org-mode-map))) "Common copy and paste"
      :enable t :exit t)
     ("c m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-move :other-rest-args ((org org-mode-map))) "Common Move"
      :enable t :exit t)
     ("c o" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-open :other-rest-args ((org org-mode-map))) "Common Open"
      :enable t :exit t)
     ("c s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-sort :other-rest-args ((org org-mode-map))) "Common Sort"
      :enable t :exit t)
     ("c t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-toggle :other-rest-args ((org org-mode-map))) "Common Toggle"
      :enable t :exit t)
     ("c a" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-archive :other-rest-args ((org org-mode-map))) "Common Archive"
      :enable t :exit t)
     ("c n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-attachments :other-rest-args ((org org-mode-map))) "Common attachments"
      :enable t :exit t))))

;; ******** babel
(defvar entropy/emacs-org-keymap-group-$babel
  '("Babel"
    (("C-c C-x [" org-reftex-citation
      "Use reftex-citation to insert a citation into the buffer"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** time stamp
(defvar entropy/emacs-org-keymap-group-$time-stamp
  '("Time stamp"
    (("C-c ." org-time-stamp "Prompt for a date/time and insert a time stamp"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c !" org-time-stamp-inactive
      "Insert an inactive time stamp"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-t" org-toggle-time-stamp-overlays
      "Toggle the use of custom time stamp formats"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c <" org-date-from-calendar
      "Insert time stamp corresponding to cursor date in *Calendar* buffer"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("S-<down>" org-shiftdown
      "Decrease the date item at the cursor by one"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<up>" org-shiftup
      "Increase the date item at the cursor by one"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     )))

;; ******** headline tag
(defvar entropy/emacs-org-keymap-group-$headline-tag
  '("Headline tag"
    (("C-c C-x q" org-toggle-tags-groups
      "Toggle support for group tags"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-q"
      (call-interactively
       (if (fboundp 'counsel-org-tag)
           'counsel-org-tag
         'org-set-tags-command))
      "Set the tags for the current visible entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-c" org-ctrl-c-ctrl-c
      "Set tags in headline, or update according to changed information at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))


;; ******** plain list
(defvar entropy/emacs-org-keymap-group-$plain-list
  '("Plain list"
    (("M-S-<right>" org-shiftmetaright
      "Indent a local list item including its children"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-S-<left>" org-shiftmetaleft
      "Outdent a local list item including its children"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     )))

;; ******** headline drawer property
(defvar entropy/emacs-org-keymap-group-$drawer&property
  '("Drawer and property"
    (("C-c C-x d" org-insert-drawer
      "Insert a drawer at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x o" org-toggle-ordered-property
      "Toggle the ORDERED property of the current entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x p" org-set-property
      "In the current entry, set PROPERTY to VALUE"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x P" org-set-property-and-value
      "Allow to set [PROPERTY]: [value] direction from prompt"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x e" org-set-effort
      "Set the effort property of the current entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x E" org-inc-effort
      "Increment the value of the effort property in the current entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** inline image

(defvar entropy/emacs-org-keymap-group-$inline-image
  '("Inline image"
    (("C-c C-x C-v" org-toggle-inline-images
      "Toggle the display of inline images"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-M-v" org-redisplay-inline-images
      "Refresh the display of inline images"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** table
;; ********* subgroups
;; ********** table create&covert

(defvar entropy/emacs-org-keymap-group-$table-create&convert
  '("Table create&convert"
    (("C-c |" org-table-create-or-convert-from-region
      "Convert region to table, or create an empty table"
      :enable t :map-inject t :exit t
      ) ;; All the other keys
     ("C-c ~" org-table-create-with-table.el
      "Use the table.el package to insert a new table"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** table edit

(defvar entropy/emacs-org-keymap-group-$table-edit
  '("Table edit"
    (("C-c `" org-table-edit-field
      "Edit table field in a different window"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c ?" org-table-field-info
      "Show info about the current field, and highlight any reference at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-#" org-table-rotate-recalc-marks
      "Rotate the recalculation mark in the first column"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c SPC" org-table-blank-field
      "Blank the current table field or active region"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("S-RET" org-table-copy-down
      "Copy the value of the current field one row below"
      :enable t :map-inject t :exit t) ;; RET/<return> key with modifiers
     ("C-c \" a" orgtbl-ascii-plot
      "Draw an ASCII bar plot in a column"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c \" g" org-plot/gnuplot
      "Plot table using gnuplot"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-S-<down>" org-shiftmetadown
      "Insert an empty row at the current line"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-S-<up>" org-shiftmetaup
      "kill the current row"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-S-<right>" org-shiftmetaright
      "Demote subtree or insert table column"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("M-S-<left>" org-shiftmetaleft
      "Promote subtree or delete table column"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     )))

;; ********** table eval
(defvar entropy/emacs-org-keymap-group-$table-eval
  '("Table eval"
    (("C-c =" org-table-eval-formula
      "Replace the table field value at the cursor by the result of a calculation"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c +" org-table-sum
      "Sum numbers in region of current table column"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c *" org-ctrl-c-star
      "Compute table, or change heading status of lines"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c {" org-table-toggle-formula-debugger
      "Toggle the formula debugger in tables"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** table view
(defvar entropy/emacs-org-keymap-group-$table-view
  '("Table view"
    (("C-c }" org-table-toggle-coordinate-overlays
      "Toggle the display of Row/Column numbers in tables"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c TAB" org-ctrl-c-tab
      "Toggle columns width in a table, or show children"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ********** table move

(defvar entropy/emacs-org-keymap-group-$table-move
  '("Table move "
    (("S-<down>" org-shiftdown
      "Move a single cell down in a table"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<left>" org-shiftleft
      "move a single cell left"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<right>" org-shiftright
      "move a single cell right") ;; Cursor keys with modifiers
     ("S-<up>" org-shiftup
      "Move a single cell up in a table"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<tab>" org-shifttab
      "Go to the previous field in the table"
      :enable t :map-inject t :exit t) ;; TAB key with modifiers
     )))

;; ********* define

(defvar entropy/emacs-org-keymap-group-$table
  '("Table"
    (("c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-create&convert :other-rest-args ((org org-mode-map)))
      "Create&Convert Table"
      :enable t :exit t)
     ("e" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-edit :other-rest-args ((org org-mode-map))) "Edit Table "
      :enable t :exit t)
     ("m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-move :other-rest-args ((org org-mode-map))) "Move Elements"
      :enable t :exit t)
     ("f" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-eval :other-rest-args ((org org-mode-map))) "Table Evaluate"
      :enable t :exit t)
     ("v" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-view :other-rest-args ((org org-mode-map))) "Table rich view"
      :enable t :exit t))))

;; ******** dynamic block
(defvar entropy/emacs-org-keymap-group-$dynamic-block
  '("Dynamic block"
    (("C-c C-x x" org-dynamic-block-insert-dblock
      "Insert a dynamic block of type TYPE"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-u" org-dblock-update
      "User command for updating dynamic blocks"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** link

(defvar entropy/emacs-org-keymap-group-$link
  '("Link"
    (("C-c C-l" org-insert-link
      "Insert a link.  At the prompt, enter the link"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-M-l" org-insert-all-links
      "Insert all links in 'org-stored-links'"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c M-l" org-insert-last-stored-link
      "Insert the last link stored in 'org-stored-links'"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-n" org-next-link
      "Move forward to the next link"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-p" org-previous-link
      "Move backward to the previous link"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("<f2>" org-toggle-link-display
      "Toggle the literal or descriptive display of links"
      :enable t :map-inject nil :exit t)
     )))


;; ******** note
(defvar entropy/emacs-org-keymap-group-$note
  '("Note"
    (("C-c C-z" org-add-note
      "Add a note to the current entry"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-k" org-kill-note-or-show-branches
      "Abort storing current note, or show just branches"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******* define

(defvar entropy/emacs-org-keymap-group-$basic-manipulation
  '("Basic"
    (("b c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common :other-rest-args ((org org-mode-map))) "Common Manipulation"
      :enable t :exit t)
     ("b u" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$link :other-rest-args ((org org-mode-map))) "Link Manipulation"
      :enable t :exit t)
     ("b t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table :other-rest-args ((org org-mode-map))) "Table Manipulation"
      :enable t :exit t)
     ("b l" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$plain-list :other-rest-args ((org org-mode-map))) "Plain List Manipulation"
      :enable t :exit t)
     ("b n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$note :other-rest-args ((org org-mode-map))) "Note Manipulation"
      :enable t :exit t))
    "Handy"
    (("b g" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$headline-tag :other-rest-args ((org org-mode-map))) "Tag Manipulation"
      :enable t :exit t)
     ("b d" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$drawer&property :other-rest-args ((org org-mode-map))) "Drawer&Property Manipulation"
      :enable t :exit t)
     ("b s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$time-stamp :other-rest-args ((org org-mode-map))) "Time Stamp Manipulation"
      :enable t :exit t)
     ("b i" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$inline-image :other-rest-args ((org org-mode-map))) "Inline Image manipulation"
      :enable t :exit t)
     ("b y" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$dynamic-block :other-rest-args ((org org-mode-map))) "Dynamic Block manipulation"
      :enable t :exit t)
     ("b b" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$babel :other-rest-args ((org org-mode-map))) "Babel Manipulation"
      :enable t :exit t))))

;; ****** org buffer navigation (guide)
;; ******* sub-groups
;; ******** sparse tree
(defvar entropy/emacs-org-keymap-group-$sparse-tree
  '("Sparse tree"
    (("C-c \\" org-match-sparse-tree
      "Create a sparse tree according to tags string MATCH"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c /" org-sparse-tree
      "Create a sparse tree, prompt for the details"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-c" org-columns
      "Turn on column view on an Org mode file"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x M-c" org-columns-remove-overlays
      "Remove all currently active column overlays"
      :enable t :map-inject t :exit t)
     ("C-c C-x b" org-tree-to-indirect-buffer
      "Create indirect buffer and narrow it to current subtree"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** cycle
(defvar entropy/emacs-org-keymap-group-$cycle
  '("Cycle"
    (("<tab>" org-cycle
      "TAB-action and visibility cycling for Org mode"
      :enable t :map-inject t :exit t) ;; TAB key with modifiers
     ("C-i" org-cycle
      "TAB-action and visibility cycling for Org mode"
      :enable t :map-inject t :exit t) ;; TAB key with modifiers
     ("C-'" org-cycle-agenda-files
      "Cycle through the files in 'org-agenda-files'"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-," org-cycle-agenda-files
      "Cycle through the files in 'org-agenda-files'"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** goto

(defun entropy/emacs-org-previous-visible-heading (&optional arg)
  "Like `org-previous-visible-heading' but goto to parent heading
when prefix arg was '(4) i.e. the single `C-u' type."
  (declare (interactive-only t))
  (interactive "P" org-mode)
  (cond
   ((equal arg '(4))
    (outline-up-heading 1))
   (t
    (org-previous-visible-heading
     (prefix-numeric-value arg)))))

(defvar entropy/emacs-org-keymap-group-$goto
  '("Goto"
    (("C-c C-j" org-goto
      "Look up a different location in the current file, keeping current visibility"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c >" org-goto-calendar
      "Go to the Emacs calendar at the current date"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-p" entropy/emacs-org-previous-visible-heading
      "Move to the previous visible heading"
      :enable t :map-inject t :exit t)
     ("<M-up>" entropy/emacs-org-previous-visible-heading
      "Move to the previous visible heading"
      :enable t :map-inject t :exit t)
     ("C-c C-n" org-next-visible-heading
      "Move to the next visible heading"
      :enable t :map-inject t :exit t)
     ("<M-down>" org-next-visible-heading
      "Move to the next visible heading"
      :enable t :map-inject t :exit t)
     ("C-<up>" org-previous-item "Move to the beginning of the previous item"
      :enable t :exit t :map-inject t)
     ("C-<down>" org-next-item "Move to the beginning of the next item"
      :enable t :exit t :map-inject t)
     ("M-{" org-backward-element
      "Move to the previous element at the same level, when possible"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-}" org-forward-element
      "Move to the next element at the same level, when possible"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-b" org-backward-heading-same-level
      "Move backward to the ARG’th subheading at same level as this one"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-f" org-forward-heading-same-level
      "Move forward to the ARG’th subheading at same level as this one"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-/" org-down-element
      "Move to inner element"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-^" org-up-element
      "Move to upper element"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))


;; ******** mark
(defvar entropy/emacs-org-keymap-group-$mark
  '("Mark"
    (("ESC h" org-mark-element
      "Put point at beginning of this element, mark at end"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-h" org-mark-element
      "Put point at beginning of this element, mark at end"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c &" org-mark-ring-goto
      "Jump to the previous position in the mark ring"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c %" org-mark-ring-push
      "Put the current position into the mark ring and rotate it"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c @" org-mark-subtree
      "Mark the current subtree"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))


;; ******** narrow
(defvar entropy/emacs-org-keymap-group-$narrow
  '("Narrow"
    (("C-x n b" org-narrow-to-block
      "Narrow buffer to the current block"
      :enable t :map-inject t :exit t) ;; Narrowing bindings
     ("C-x n e" org-narrow-to-element
      "Narrow buffer to current element"
      :enable t :map-inject t :exit t) ;; Narrowing bindings
     ("C-x n s" org-narrow-to-subtree
      "Narrow buffer to the current subtree"
      :enable t :map-inject t :exit t) ;; Narrowing bindings
     )))

;; ******** block jump
(defvar entropy/emacs-org-keymap-group-$block-jump
  '("Block Jump"
    (("C-c M-f" org-next-block
      "Jump to the next block"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c M-b" org-previous-block
      "Jump to the previous block"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******* define

(defvar entropy/emacs-org-keymap-group-$buffer-navigation
  '("Buffer Navigation"
    (("n s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$sparse-tree :other-rest-args ((org org-mode-map))) "Sparse Tree Viewer"
      :enable t :exit t)
     ("n c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$cycle :other-rest-args ((org org-mode-map))) "Cycle Through Buffer"
      :enable t :exit t)
     ("n g" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$goto :other-rest-args ((org org-mode-map))) "Goto Buffer POS"
      :enable t :exit t)
     ("n m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$mark :other-rest-args ((org org-mode-map))) "Mark Up Org Buffer"
      :enable t :exit t)
     ("n n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$narrow :other-rest-args ((org org-mode-map))) "Narrow Org Buffer"
      :enable t :exit t)
     ("n j" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$block-jump :other-rest-args ((org org-mode-map))) "Block Jump"
      :enable t :exit t))))

;; ****** org task function
;; ******* sub-groups
;; ******** timer
(defvar entropy/emacs-org-keymap-group-$timer
  '("Timer"
    (("C-c C-x ." org-timer
      "Insert a H:MM:SS string from the timer into the buffer"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x -" org-timer-item
      "Insert a description-type item with the current timer value"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x ," org-timer-pause-or-continue
      "Pause or continue the relative or countdown timer"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x ;" org-timer-set-timer
      "Prompt for a duration in minutes or hh:mm:ss and set a timer"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x 0" org-timer-start
      "Set the starting time for the relative timer to now"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x /" org-timer-stop
      "Stop the relative or countdown timer"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** clock
(defvar entropy/emacs-org-keymap-group-$clock
  '("Clock"
    (("C-c C-x C-q" org-clock-cancel
      "Cancel the running clock by removing the start timestamp"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-d" org-clock-display
      "Show subtree times in the entire buffer"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-j" org-clock-goto
      "Go to the currently clocked-in entry, or to the most recently clocked one"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-i" org-clock-in
      "Start the clock on the current item"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-x" org-clock-in-last
      "Clock in the last closed clocked item"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-o" org-clock-out
      "Stop the currently running clock"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-z" org-resolve-clocks
      "Resolve all currently open Org clocks"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-y" org-evaluate-time-range
      "Evaluate a time range by computing the difference between start and end"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-S-<down>" org-shiftcontroldown
      "Change timestamps synchronously down in CLOCK log lines"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("C-S-<up>" org-shiftcontrolup
      "Change timestamps synchronously up in CLOCK log lines"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     )))

;; ******** todo

(defvar entropy/emacs-org-keymap-group-$todo
  '("Todo"
    (("C-c C-t" org-todo
      "Change the TODO state of an item"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("M-S-<return>" org-insert-todo-heading
      "Insert a new heading with the same level and TODO state as current heading"
      :enable t :map-inject t :exit t) ;; RET/<return> key with modifiers
     ("M-S-RET" org-insert-todo-heading
      "Insert a new heading with the same level and TODO state as current heading"
      :enable t :map-inject t :exit t) ;; RET/<return> key with modifiers
     ("C-S-<return>" org-insert-todo-heading-respect-content
      "Insert TODO heading with 'org-insert-heading-respect-content' set to t"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c #" org-update-statistics-cookies
      "Update the statistics cookie, either from TODO or from checkboxes"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c ," org-priority
      "Change the priority of an item"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x c" org-clone-subtree-with-time-shift
      "Clone the task (subtree) at point N times"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-S-<left>" org-shiftcontrolleft
      "Switch to previous TODO set"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("C-S-<right>" org-shiftcontrolright
      "Switch to next TODO set"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<down>" org-shiftdown
      "Decrease the priority of the current item"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<up>" org-shiftup
      "Increase the priority of the current item"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<left>" org-shiftleft
      "switch to the previous TODO keyword"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     ("S-<right>" org-shiftright
      "switch to the next TODO keyword"
      :enable t :map-inject t :exit t) ;; Cursor keys with modifiers
     )))

;; ******** agenda
(defvar entropy/emacs-org-keymap-group-$agenda
  '("Agenda"
    (("C-c [" org-agenda-file-to-front
      "Move/add the current file to the top of the agenda file list"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x >" org-agenda-remove-restriction-lock
      "Remove agenda restriction lock"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x <" org-agenda-set-restriction-lock
      "Set restriction lock for agenda to current subtree or file"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c ]" org-remove-file
      "Remove current file from the list of files in variable 'org-agenda-files'"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** schedule

(defvar entropy/emacs-org-keymap-group-$schedule
  '("Schedule"
    (("C-c C-d" org-deadline
      "Insert the "DEADLINE:" string with a timestamp to make a deadline"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-s" org-schedule
      "Insert the SCHEDULED: string with a timestamp to schedule a TODO item"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))


;; ******* define

(defvar entropy/emacs-org-keymap-group-$task-manipulation
  '("Task Manipulation"
    (("t t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$todo :other-rest-args ((org org-mode-map))) "Todo manipulation"
      :enable t :exit t)
     ("t c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$clock :other-rest-args ((org org-mode-map))) "Clock Operation"
      :enable t :exit t)
     ("t a" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$agenda :other-rest-args ((org org-mode-map))) "Agenda Refer"
      :enable t :exit t)
     ("t m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$timer :other-rest-args ((org org-mode-map))) "Timer manipulation"
      :enable t :exit t))))


;; ****** org rss
(defvar entropy/emacs-org-keymap-group-$org-rss
  '("Rss"
    (("C-c C-x G" org-feed-goto-inbox
      "Go to the inbox that captures the feed named FEED"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x g" org-feed-update-all
      "Get inbox items from all feeds in 'org-feed-alist'"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ****** org export and preview

(defvar entropy/emacs-org-keymap-group-$export-and-preview
  '("Export&Preview"
    (("C-c C-e" org-export-dispatch
      "Export dispatcher for Org mode"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-l" org-latex-preview
      "Toggle preview of the LaTeX fragment at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ****** org misc.

(defvar entropy/emacs-org-keymap-group-$misc
  '("Misc"
    (("C-c C-x I" org-info-find-node
      "Find Info documentation NODENAME or Org documentation according context"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x !" org-reload
      "Reload all Org Lisp files"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ***** define-hydra

(entropy/emacs-lazy-initial-advice-before
 (org-mode)
 "hydra-hollow-init-for-org"
 "hydra-hollow-init-for-org"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
  'org-mode '(org org-mode-map) t
  (append
   entropy/emacs-org-keymap-group-$basic-manipulation
   entropy/emacs-org-keymap-group-$buffer-navigation
   entropy/emacs-org-keymap-group-$task-manipulation
   entropy/emacs-org-keymap-group-$org-rss
   entropy/emacs-org-keymap-group-$export-and-preview
   entropy/emacs-org-keymap-group-$misc)
  '(4 2 2)
  '(4 2 2)))

;; ** entropy-emacs additional function
;; *** tags align
(defun entropy/emacs-org-tags-align (&optional all)
  "Align all tags in one org-mode buffer with align column prompt
inputting, the align number must be positive as that it will be
automatically transferred to the value adapted to
`org-tags-column'."
  (declare (interactive-only t))
  (interactive "P" org-mode)
  (let ((org-tags-column (string-to-number
                          (concat "-"
                                  (read-string
                                   "please insert the tag align column: ")))))
    (if (and (not all) (org-at-heading-p))
        (org--align-tags-here org-tags-column)
      (save-excursion
        (if all
            (progn
              (goto-char (point-min))
              (while (re-search-forward org-tag-line-re nil t)
                (org--align-tags-here org-tags-column)))
          (org-back-to-heading t)
          (org--align-tags-here org-tags-column))))))

;; *** org file images management
;; **** extract all images from org file
(defun entropy/emacs-org-ocii-extract-file-imgs-main ()
  "Extracting all images from one org file to the target location
chosen as prompted query location state."
  (interactive nil org-mode)
  (let* ((target-file (completing-read "choosing org file: "
                                       'read-file-name-internal
                                       nil t))
         (target-imgs-dir
          (completing-read "Choosing imgs extract location: "
                           'read-file-name-internal
                           nil t))
         imgs-paths imgs-lost imgs-copied)
    (cond
     ((not (file-exists-p target-file))
      (error (format "File '%s' not existed." target-file)))
     ((or (file-directory-p target-file)
          (not (equal (file-name-extension target-file) "org")))
      (error (format "File '%s' must be org file" target-file))))
    (unless (file-directory-p target-imgs-dir)
      (error (format "Directory '%s' not existed.")))
    (setq imgs-paths (entropy/emacs-org--ocii-extract-file-imgs-links target-file))
    (when imgs-paths
      (dolist (el imgs-paths)
        (if (file-exists-p el)
            (push el imgs-copied)
          (push el imgs-lost)))
      (when imgs-lost
        (entropy/emacs-org--ocii-extract-prompt-lost imgs-lost))
      (if imgs-copied
          (unless (not (yes-or-no-p
                        (format "Extract imgs of '%s' to '%s'?"
                                (file-name-nondirectory target-file)
                                target-imgs-dir)))
            (dolist (el imgs-copied)
              (copy-file el
                         (expand-file-name (file-name-nondirectory el)
                                           (expand-file-name target-imgs-dir)))
              (message (format "Copy '%s' to '%s' done."
                               el target-imgs-dir))))
        (message (format "No valid imgs of '%s' to extracted!"
                         (file-name-nondirectory target-file)))))))

(defun entropy/emacs-org--ocii-extract-prompt-lost (imgs-list)
  (dolist (el imgs-list)
    (message (format "Image file '%s' not existed!" el))))

(defun entropy/emacs-org--ocii-extract-file-imgs-links (org-file)
  "extracting all images links from one org-file ORG-FILE without
source images file existed status checking.

NOTE:

Now just supply localization image file analyzing."
  (require 'entropy-org-widget)
  (let ((link-objs (entropy/ow-get-buffer-links (find-file-noselect org-file)))
        links_temp links
        (base-dir (file-name-directory org-file)))
    (when link-objs
      (dolist (el link-objs)
        (push (plist-get el :link) links_temp)))
    (when links_temp
      (dolist (el links_temp)
        (when (string-match "\\(svg\\|imagemagick\\|png\\|gif\\|tiff\\|jpeg\\|jpg\\|xpm\\|xbm\\|pbm\\)$"
                            el)
          (let ((non-abbrev (replace-regexp-in-string "^file:" "" el)))
            (cond
             ((string-match-p "^\\.+" non-abbrev)
              (let ((default-directory base-dir))
                (push (expand-file-name non-abbrev) links)))
             (t
              (when (entropy/emacs-filesystem-node-name-legal-p non-abbrev)
                (push non-abbrev links))))))))
    links))


;; ** org-bullets
(use-package org-bullets
  :commands (org-bullets-mode)
  :init
  (entropy/emacs-lazy-with-load-trail
   org-bullet-mode-init
   :pdumper-no-end t
   :body
   (if (daemonp)
       (entropy/emacs-with-daemon-make-frame-done
        'org-bullet-mode-init
        '(progn
           (remove-hook 'org-mode-hook #'org-bullets-mode)
           (mapc
            (lambda (buffer)
              (with-current-buffer buffer
                (when (and (eq major-mode 'org-mode)
                           (bound-and-true-p org-bullets-mode))
                  (org-bullets-mode 0))))
            (buffer-list)))
        '(progn
           (add-hook 'org-mode-hook #'org-bullets-mode)
           (mapc
            (lambda (buffer)
              (with-current-buffer buffer
                (when (and (eq major-mode 'org-mode)
                           (null (bound-and-true-p org-bullets-mode)))
                  (org-bullets-mode 1))))
            (buffer-list))))
     (add-hook 'org-mode-hook #'org-bullets-mode)
     ;; update opened org-buffer bullet status
     (mapc
      (lambda (buffer)
        (with-current-buffer buffer
          (when (eq major-mode 'org-mode)
            (unless (bound-and-true-p org-bullets-mode)
              (org-bullets-mode 1)))))
      (buffer-list))))

  :config
  (if (not (string= entropy/emacs-org-bullets-type "roman"))
      (setq org-bullets-bullet-list '("⓪" "①" "②" "③"
                                      "④" "⑤" "⑥" "⑦"
                                      "⑧" "⑨" "⑩" "⑪"
                                      "⑫" "⑬" "⑭"
                                      "⑮" "⑯" "⑰"
                                      "⑱" "⑲" "⑳"))
    (setq org-bullets-bullet-list '("●" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ"
                                    "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ"
                                    "Ⅹ" "Ⅺ" "Ⅻ"))))

;; ** org-pomodoro
(use-package org-pomodoro)

;; ** org html export extra themes
(use-package entropy-org-export-theme-toggle
  :ensure nil
  :commands (entropy/org-exptth-set-head))

;; ** org-download
(use-package org-download
  :commands (org-download-image
             org-download-screenshot
             org-download-delete
             org-download-enable)
  :after org
  :bind
  (:map org-mode-map
        ("\C-cp" . entropy/emacs-org-download-screenshot))
  :init

  ;; Init setting for changing the default annotation method and support unicode dir-name
  (progn
    (setq-default org-download-image-dir "./img/")
    (setq-default org-download-heading-lvl nil)	;; Cancel taxonomy make image directory

    ;; Support unicode filename
    (defun entropy/emacs-org--custom-download-method (link)
      (org-download--fullname (org-link-unescape link)))
    (setq org-download-method 'entropy/emacs-org--custom-download-method)
                                        ; notice, this field can not using lambda expression

    ;; Donwload annotation specifiction
    (setq org-download-annotate-function
          #'(lambda (link)
              (org-download-annotate-default (org-link-unescape link)))))

;; *** config
  :config

;; **** Org-download-dnd about

  ;; DND was the handle of emacs for handling OS-to-Emacs interactive
  ;; behaviour abbreviated by 'Drag-And-Drop'.
  ;;
  ;; For handling this, emacs provide variable `dnd-protocol-alist'
  ;; to store the alist of numerous method for handling this.
  ;;
  ;; Org-download package's internal functions `org-download-dnd' was
  ;; added to `dnd-protocol-alist' while enable it, and it's function
  ;; was handle the dnd operator in org-mode buffer with calling
  ;; `org-download-image' for insert image into current org-mode
  ;; buffer.

;; **** Patch org-download-screenshot

  (defun entropy/emacs-org-download-screenshot ()
    "Capture screenshot and insert the resulting file.
The screenshot tool is determined by
`org-download-screenshot-method'.

If you are in WINDOWS plattform, we fallback to use
=SnippingTool= to be the download method when default way
occurred with error. And on that case in WINDOWS, the
=SnippingTool= tool will save the capture image by interactived
way so that we can not tranfer any command line arguments to it
for returning the image path saved to, in this case we make a
convention that pre-set a capture path
`entropy/emacs-win-org-download-file-name' so that we can using
`org-download-image' to do thus.

Note: this function was derived and extended from
org-download-screenshot, and patched with support auto org-indent
current inserted annotation when `org-adapt-indentation' non-nil.
"
    (interactive nil org-mode)
    (let* ((inhibit-read-only t)
           success-p
           (win-method "SnippingTool.exe")
           (indent-func
            (lambda ()
              (beginning-of-line)
              (org-indent-line)))
           (windows-use-fallback-p
            (lambda ()
              (and sys/is-win-group
                   (executable-find win-method))))
           (windows-fallback-func
            (lambda ()
              (let ((link entropy/emacs-win-org-download-file-name))
                (if (not (ignore-errors (process-lines win-method)))
                    (error "Capture image using '%s' failed!" win-method)
                  (org-download-image link)
                  ;; delete the tempo capture image
                  (delete-file link))))))
      (condition-case nil
          (progn
            (call-interactively #'org-download-screenshot)
            (setq success-p t))
        (error
         (cond
          ((funcall windows-use-fallback-p)
           (funall windows-fallback-func)
           (setq success-p t)))))
      (when (and success-p
                 org-adapt-indentation)
        (let ((prev (+ 0
                       (if org-download-annotate-function       1 0)
                       (if (= org-download-image-html-width 0)  0 1)
                       (if (= org-download-image-latex-width 0) 0 1))))
          (unless (= 0 prev)
            (dotimes (i prev)
              (forward-line (- i prev))
              (funcall indent-func)
              (forward-line (- prev i))))
          (funcall indent-func)))))

;; **** enhance org-download-insert-link
  (defun entropy/emacs-org--odl-judgement-whether-capture-name (buffname)
    "Judgement whether using 'org-download' in capture mode, if
indeed then auto-transfer buffer-name to origin one and return
FILENAME.

FILENAME transfer depending on the value of
`uniquify-buffer-name-style' which uniquify `buffer-name'
duplicated in `buffer-list', thus each type of uniquify name style
corresponding to the specific transfer method."
    (if (string-match-p "^CAPTURE-" buffname)
        (let ((tname (replace-regexp-in-string "^CAPTURE-" "" buffname))
              (forward-re "\\(.*?/\\)+")
              (reverse-re "\\(.*?\\\\\\)+")
              (post-forward-re "\\|.*?/?.*?$")
              (post-angle-re "<.*?>$"))
          (cond
           ;; forward uniquify buffer name type
           ((string-match-p forward-re tname)
            (setq tname (file-name-nondirectory tname)))
           ((string-match-p reverse-re tname)
            (setq tname (file-name-nondirectory tname)))
           ;; post-forward-angle-brackets uniquify buffer name type
           ((string-match-p post-angle-re tname)
            (setq tname (replace-regexp-in-string post-angle-re "" tname)))
           ;; post-forward uniquify buffer name type
           ((string-match-p post-forward-re tname)
            (setq tname (replace-regexp-in-string post-forward-re "" tname)))
           ;; non-duplicated files
           (t tname))
          (concat default-directory tname))
      buffer-file-name))

  (defun org-download-insert-link (link filename)
    "This function has been redefine for the bug of using
`buffer-name' in `file-name-directory' and automatically
adjusting the link insert position follow the rules below:

- Point at beginning of empty line:

  Insert link in current point.

- Point at middle or end of empty line:

  Insert link in current point.

- Point at end of none-empty line:

  Insert the link under of current line with newline creating.

- Point at partition case of none-empty line:

  Insert the link under of current line with newline creating.
"
    (cond
     ((and (equal (point) (line-beginning-position)) ;point at beginning of empty line
           (looking-at " *$"))
      t)
     ((and (looking-back "^ +" (line-beginning-position)) ;point at middle or end of empty line
           (looking-at " *$"))
      t)
     ((and (looking-at " *$")       ;point at end of none-empty line
           (looking-back ".+" (line-beginning-position)))
      (newline))
     ((and (looking-at ".+$")       ;point at part or beginning of none-empty line
           (looking-back ".*" (line-beginning-position)))
      (end-of-line)
      (newline)))
    (insert
     (concat
      (funcall org-download-annotate-function link)
      (if (eq org-download-annotate-function 'ignore) "" "\n")
      (if (= org-download-image-html-width 0)
          ""
        (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
      (if (= org-download-image-latex-width 0)
          ""
        (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
      (format org-download-link-format
              (file-relative-name
               filename
               (file-name-directory
                (entropy/emacs-org--odl-judgement-whether-capture-name (buffer-name)))))))
    (org-indent-line)))

;; ** toc-org
(use-package toc-org
  :commands toc-org-insert-toc
  :init (setq toc-org-hrefify-default "gh"))

;; * provide
(provide 'entropy-emacs-org)
