;;; entropy-emacs-org.el --- entropy emacs org-mode configuration
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
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)
(require 'entropy-emacs-utils)
(require 'entropy-emacs-hydra-hollow)

;; ** Pre advice
(defun entropy/emacs-org--export-panel-around-advice (old-func &rest args)
  (unless (fboundp 'org-reveal-export-to-html)
    (when (featurep 'ox-reveal)
      (load-library "ox-reveal")))
  (let ((entropy/emacs-web-development-environment t))
    (apply old-func args)))

(entropy/emacs-lazy-load-simple ox
  (advice-add 'org-export-dispatch
              :around #'entropy/emacs-org--export-panel-around-advice))


;; ** main
;; *** core
(use-package org
  :ensure nil
  :defines  (org-mode-map)
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
  (((:enable t))
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

  (entropy/emacs-lazy-load-simple org
    (unless (or entropy/emacs-fall-love-with-pdumper
                (not entropy/emacs-custom-enable-lazy-load))
      (defvar entropy/emacs-org--org-mode-fist-calling-done nil)
      (advice-add 'org-mode
                  :around
                  #'(lambda (orig-func &rest orig-args)
                      (if entropy/emacs-org--org-mode-fist-calling-done
                          (progn (apply orig-func orig-args))
                        (entropy/emacs-run-hooks-with-prompt
                         (setq entropy/emacs-org--org-mode-fist-calling-done t)
                         (apply orig-func orig-args)))))))

;; **** configs
  :config
;; ***** basic setting

  ;;Insert time-stamp when toggle 'TODO' to 'DONE'
  (setq org-log-done 'time)

  ;; Using fuzzy match for org external link
  ;; which support the link type:
  ;; 'file:xxx.org::str'
  (setq org-link-search-must-match-exact-headline nil)

  ;; Forcely trucate line for `org-mode' buffer
  (setq org-startup-truncated t)

  ;; Choosing org formula convertor
  (when (and sys/is-graphic-support (not sys/is-win-group))
    (setq org-preview-latex-default-process 'imagemagick)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))
  (when sys/win32p
    (setq org-preview-latex-default-process 'dvisvgm)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))


  (setq-default org-hide-block-startup t)                         ;Hiding block details at init-time
  (setq org-export-headline-levels 8)                             ;Export heading level counts same as raw org that maximum for 8
  (setq org-cycle-separator-lines 30)                             ;Set the empty line number between the headline without visible
  (setq-default org-cycle-include-plain-lists 'integrate)         ;Vixual cycle with plain list
  (add-to-list 'org-export-backends 'md)                          ;Adding org export file type - markdown

  (setq org-imenu-depth 8) ; The default depth shown for integrating
                           ; org heading line to imenu while be within
                           ; org-mode

;; ***** define 'end' key to `org-end-of-line'
  (define-key org-mode-map (kbd "<end>") 'org-end-of-line)

;; ***** org open at point enhanced

  ;; change the find-file method of org-open-at-point instead of find-file-other-window
  (defun entropy/emacs-org-open-at-point ()
    (interactive)
    (let ((org-link-frame-setup
           (entropy/emacs-cl-compatible-apply acons 'file 'find-file org-link-frame-setup))
          (process-connection-type
           (cond
            ((eq system-type 'gnu/linux)
             nil)
            (t t))))
      (org-open-at-point)))

  ;; using entropy-open-with to open org link
  (defun entropy/emacs-org-eow ()
    "Open link in org-mode using `entropy/open-with-port'."
    (interactive)
    (require 'entropy-open-with)
    (let* ((link (nth 1 (org-element-lineage
                         (org-element-context)
                         '(link) t)))
           (link-type (plist-get link :type))
           (path (plist-get link :path)))
      (entropy/open-with-port nil (cond ((string= "file" link-type)
                                         (expand-file-name (url-unhex-string path)))
                                        ((string-match-p "https?" link-type)
                                         (concat link-type ":" path))
                                        (t (error (format "Invalid file type '%s'!" link-type)))))))

;; ***** org-priority-setting
  (setq org-lowest-priority 90)         ;lowset prioty was 'Z'
  (setq org-default-priority 67)        ;default prioty was 'C'

;; ***** org-refile gloable and 9 depths
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  (setq org-url-hexify-p nil)
  (setq-default org-link-file-path-type (quote relative))

;; ***** org-counsel-set-tag

  (when (featurep 'counsel)
    (entropy/emacs-lazy-load-simple org
      (bind-key "C-c C-q" #'counsel-org-tag org-mode-map))
    (entropy/emacs-lazy-load-simple org-agenda
      (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map)))

;; ***** org-inline-image size
  (when (image-type-available-p 'imagemagick)
    (progn
      (setq org-image-actual-width nil)
      (defun entropy/emacs-org--otii-before-advice (&rest arg-rest)
        "Advice for `org-toggle-inline-images' when emacs was
build with imagemagick, because of that org-mode will have the
ability to display GIF type image whatever it's size be, so it
will cause the large lagging performance when the file link
directed to large gif file when willing display images in current
buffer."
        (if (not (yes-or-no-p "This will spend sometime and causing lagging performance, cotinue? "))
            (error "Abort displaying inline images")))
      (advice-add 'org-display-inline-images :before #'entropy/emacs-org--otii-before-advice)))

;; ***** org-auto-insert 'CUSTOM-ID'
  ;;      which source code from the bloag@
  ;;      `https://writequit.org/articles/emacs-org-mode-generate-ids.html#h-cf29e5e7-b456-4842-a3f7-e9185897ac3b'
;; ****** baisc function
  (defun entropy/emacs-org--custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    ;; (interactive)
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

;; ****** interactive function
  ;; Originally function that can not auto detected '#+OPTIOINS: auto-id:t'
  (defun entropy/emacs-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive)
    (org-map-entries
     (lambda ()
       (entropy/emacs-org--custom-id-get (point) 'create))
     t nil))

  (defun entropy/emacs-org-auto-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one. Only adds ids if the
`auto-id' option is set to `t' in the file somewhere. ie,
#+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries
         (lambda () (entropy/emacs-org--custom-id-get (point) 'create))
         t nil))))

;; ****** autamatically add ids to saved org-mode headlines

  (defun entropy/emacs-org-auto-add-org-ids-before-save ()
    (when (and (eq major-mode 'org-mode)
               (eq buffer-read-only nil))
      (entropy/emacs-org-auto-add-ids-to-headlines-in-file)))
  (add-hook 'before-save-hook
            #'entropy/emacs-org-auto-add-org-ids-before-save)
;; ***** browser url with system app
  (add-hook 'org-mode-hook
            '(lambda ()
               (delete '("\\.x?html?\\'" . default) org-file-apps)
               (add-to-list 'org-file-apps '("\\.x?html?\\'" . system))))

  (add-hook 'org-mode-hook
            '(lambda ()
               (delete '("\\.[gG]if\\'" . default) org-file-apps)
               (add-to-list 'org-file-apps '("\\.[gG]if\\'" . system))))

  (add-hook 'org-mode-hook
            '(lambda ()
               (delete '("\\.pdf\\'" . default) org-file-apps)
               (add-to-list 'org-file-apps '("\\.pdf\\'" . system))))


;; ***** fix bugs of open directory using external apps in windows
  (when sys/win32p
    (add-to-list 'org-file-apps '(directory . emacs)))


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
    (interactive)
    (if (string= "*Org Agenda*" (buffer-name))
        (progn
          (if (eq org-tags-match-list-sublevels nil)
              (setq org-tags-match-list-sublevels 'indented)
            (setq org-tags-match-list-sublevels nil))
          (org-agenda-redo-all))
      (error "You must use it in org agenda buffer!"))))

;; *** org-capture
(use-package org-capture
  :ensure nil
  :after org
  :commands (org-capture)
  :config
;; **** org-capture about
;; ***** hook for org-capture
  (defun entropy/emacs-org--capture-indent-buffer (&optional arg)
    "Indent org capture buffer when finished capture editting."
    (when org-adapt-indentation
      (let ((pm (point-min))
            (pb (point-max)))
        (org-indent-region pm pb)
        (goto-char (point-max))
        (insert "\n"))))
  (add-hook 'org-capture-prepare-finalize-hook #'entropy/emacs-org--capture-indent-buffer)

  (defun entropy/emacs-org--capture-set-tags (&rest args)
    "Adding org tags using `counsel-org-tag' and save point where current it is.

This function was the after advice for `org-capture-template'."
    (condition-case error
        (let ((pi (point)))
          (funcall #'counsel-org-tag)
          (goto-char pi))
      ((error quit)
       (message "Cancel adding headline tags."))))

  (advice-add 'org-capture-place-template
              :after #'entropy/emacs-org--capture-set-tags)

;; ***** Do not using `org-toggle-link-display' in capture buffer.

  ;; Because of that if do this will lost the buffer font-lock effecting(all buffer be non-font-lock
  ;; visual) and do not have the recovery method unless reopen capture operation.w

  (defun entropy/emacs-org--capture-forbidden-toggle-link-display (&rest rest-args)
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

      (advice-add 'org-babel-get-src-block-info :around #'entropy/emacs-org--set-src-info)

      (defun entropy/emacs-org--babel-comfirm-evaluate (old-func info)
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

      (advice-add 'org-babel-confirm-evaluate :around #'entropy/emacs-org--babel-comfirm-evaluate)))

;; **** org global export macro
  (entropy/emacs-lazy-load-simple ox
    (add-to-list 'org-export-global-macros
                 '("kbd" . "@@html:<code>$1</code>@@")))

;; **** org babel src mode engines
  ;; ---------with the bug of none highlites in org mode src html block
  ;; ---------and the issue with none aspiration of web-mode maintainer with link
  ;; ----------------------------->`https://github.com/fxbois/web-mode/issues/636'
  ;; (add-to-list 'org-src-lang-modes '("html" . web))

  )

;; *** org-export

(use-package ox
  :ensure nil
  :after org
  :config
;; **** html export
;; ***** html exported head coding
  (setq org-html-coding-system 'utf-8-unix)

  ;; inhibit org self default html style
  (setq org-html-head-include-scripts nil
        org-html-head-include-default-style nil)

;; ***** solve the bug when cjk paragraph exported to html has the auto newline space char including.
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


;; ***** org export html open function

  ;; If `entropy/emacs-browse-url-function' is detectived then open
  ;; exported html file with it instead of using default apps or
  ;; system type.

  (defun entropy/emacs-org--hexpt-use-external-type ()
    "
returning the type of exec for open exported html file, they are:

- \"personal\": using `entropy/emacs-browse-url-function' to open the
  exported html file.

- \"automatic\": using the way by `org-open-file' to automatilly
  open exported file."
    (if (and entropy/emacs-enable-personal-browse-url-function
             (functionp entropy/emacs-browse-url-function))
        "personal"
      "automatic"))

  (defun entropy/emacs-org--hexpt-function (&optional path link)
    "Function embeded into `org-file-apps', used for
`entropy/emacs-org--hexpt-advice' to open html file using
`entropy/emacs-browse-url-function'."
    (require 'entropy-common-library-const)
    (if link
        (if (yes-or-no-p "Open html file with entropy/emacs-browse-url-function ? ")
            (funcall entropy/emacs-browse-url-function
                     (concat "file:///" (url-hexify-string link entropy/cl-url--allowed-chars)))
          (org-open-file path 'system))
      (error "Invalid link!")))

  (defun entropy/emacs-org--hexpt-advice (orig-func &rest orig-args)
    "Advice for `org-open-file' for changing the \"html\"
    associtated function when exporting html file from org file
    or open html file link in org-mode.

    This function use `entropy/emacs-org--hexpt-function' to judge the exec
    type for chosen the way whether embeded it into `org-file-apps'."
    (let* ((embeded '("\\.\\(x\\|m\\)?html?\\'" . entropy/emacs-org--hexpt-function))
           (type (entropy/emacs-org--hexpt-use-external-type))
           (process-connection-type nil))
      (when (string-match "\\.\\(x\\|m\\)?html?$" (car orig-args))
        (cond ((string= "personal" type)
               (when (not (member embeded org-file-apps))
                 (add-to-list 'org-file-apps embeded)
                 (message "Using `entropy/emacs-browse-url-function' to open exported html file.")))
              ("automatic"
               (message
                "Using automatic method to open exported html file! "))))
      (unwind-protect
          (apply orig-func orig-args)
        (when (member embeded org-file-apps)
          (setq org-file-apps (delete embeded org-file-apps))))))

  (advice-add 'org-open-file :around #'entropy/emacs-org--hexpt-advice)

;; ***** org ignore broken links
  (setq org-export-with-broken-links 'mark))


;; *** org-publish
(use-package org-publish
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

  ;; Around advice for `org-publish-cache-file-needs-publishing'
  ;; for adding query whether force re publishing unmodified
  ;; cached refer file.
  (defun entropy/emacs-org--publish-check-timestamp-around_advice
      (oldfun &rest args)
    "The advice around the org publish cache file timestamp check
    function `org-publish-cache-file-needs-publishing'."
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
  (when entropy/emacs-fall-love-with-pdumper
    (entropy/emacs-lazy-with-load-trail
     disable-org-ctags
     (entropy/emacs-org--ctags-disable))))

;; *** keymap hydra reflect
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
      "Insert heading with ‘org-insert-heading-respect-content’ set to t"
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
    (("C-c M-w" org-copy "Like ‘org-refile’, but copy"
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
        "Call ‘org-sort-entries’, ‘org-table-sort-lines’ or ‘org-sort-list’"
        :enable t :map-inject t :exit t) ;; All the other keys
       )))

;; ********** common open
(defvar entropy/emacs-org-keymap-group-$common-open
  '("Common open"
    (("C-c C-o" entropy/emacs-org-open-at-point
      "Open link, timestamp, footnote or tags at point"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c M-o" entropy/emacs-org-eow
      "Open link in org-mode using ‘entropy/open-with-port"
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
    (("c i" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-insert :other-rest-args (org org-mode-map)) "Common Insert"
      :enable t :exit t)
     ("c e" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-edit :other-rest-args (org org-mode-map)) "Common Edit"
      :enable t :exit t)
     ("c y" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-copy&paste :other-rest-args (org org-mode-map)) "Common copy and paste"
      :enable t :exit t)
     ("c m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-move :other-rest-args (org org-mode-map)) "Common Move"
      :enable t :exit t)
     ("c o" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-open :other-rest-args (org org-mode-map)) "Common Open"
      :enable t :exit t)
     ("c s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-sort :other-rest-args (org org-mode-map)) "Common Sort"
      :enable t :exit t)
     ("c t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-toggle :other-rest-args (org org-mode-map)) "Common Toggle"
      :enable t :exit t)
     ("c a" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-archive :other-rest-args (org org-mode-map)) "Common Archive"
      :enable t :exit t)
     ("c n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common-attachments :other-rest-args (org org-mode-map)) "Common attachments"
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
    (("c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-create&convert :other-rest-args (org org-mode-map))
      "Create&Convert Table"
      :enable t :exit t)
     ("e" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-edit :other-rest-args (org org-mode-map)) "Edit Table "
      :enable t :exit t)
     ("m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-move :other-rest-args (org org-mode-map)) "Move Elements"
      :enable t :exit t)
     ("f" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-eval :other-rest-args (org org-mode-map)) "Table Evaluate"
      :enable t :exit t)
     ("v" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table-view :other-rest-args (org org-mode-map)) "Table rich view"
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
      "Insert all links in ‘org-stored-links’"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c M-l" org-insert-last-stored-link
      "Insert the last link stored in ‘org-stored-links’"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-n" org-next-link
      "Move forward to the next link"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x C-p" org-previous-link
      "Move backward to the previous link"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("<f2>" org-toggle-link-display
      "Toggle the literal or descriptive display of links"
      :enable t :eemacs-top-bind t :exit t)
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
    (("b c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$common :other-rest-args (org org-mode-map)) "Common Manipulation"
      :enable t :exit t)
     ("b u" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$link :other-rest-args (org org-mode-map)) "Link Manipulation"
      :enable t :exit t)
     ("b t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$table :other-rest-args (org org-mode-map)) "Table Manipulation"
      :enable t :exit t)
     ("b l" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$plain-list :other-rest-args (org org-mode-map)) "Plain List Manipulation"
      :enable t :exit t)
     ("b n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$note :other-rest-args (org org-mode-map)) "Note Manipulation"
      :enable t :exit t)
     ("b g" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$headline-tag :other-rest-args (org org-mode-map)) "Tag Manipulation"
      :enable t :exit t)
     ("b d" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$drawer&property :other-rest-args (org org-mode-map)) "Drawer&Property Manipulation"
      :enable t :exit t)
     ("b s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$time-stamp :other-rest-args (org org-mode-map)) "Time Stamp Manipulation"
      :enable t :exit t)
     ("b i" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$inline-image :other-rest-args (org org-mode-map)) "Inline Image manipulation"
      :enable t :exit t)
     ("b y" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$dynamic-block :other-rest-args (org org-mode-map)) "Dynamic Block manipulation"
      :enable t :exit t)
     ("b b" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$babel :other-rest-args (org org-mode-map)) "Babel Manipulation"
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
      "Cycle through the files in ‘org-agenda-files’"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-," org-cycle-agenda-files
      "Cycle through the files in ‘org-agenda-files’"
      :enable t :map-inject t :exit t) ;; All the other keys
     )))

;; ******** goto
(defvar entropy/emacs-org-keymap-group-$goto
  '("Goto"
    (("C-c C-j" org-goto
      "Look up a different location in the current file, keeping current visibility"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c >" org-goto-calendar
      "Go to the Emacs calendar at the current date"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-p" org-previous-visible-heading
      "Move to the previous visible heading"
      :enable t :map-inject t :exit t)
     ("C-c C-n" org-next-visible-heading
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
    (("n s" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$sparse-tree :other-rest-args (org org-mode-map)) "Sparse Tree Viewer"
      :enable t :exit t)
     ("n c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$cycle :other-rest-args (org org-mode-map)) "Cycle Through Buffer"
      :enable t :exit t)
     ("n g" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$goto :other-rest-args (org org-mode-map)) "Goto Buffer POS"
      :enable t :exit t)
     ("n m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$mark :other-rest-args (org org-mode-map)) "Mark Up Org Buffer"
      :enable t :exit t)
     ("n n" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$narrow :other-rest-args (org org-mode-map)) "Narrow Org Buffer"
      :enable t :exit t)
     ("n j" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$block-jump :other-rest-args (org org-mode-map)) "Block Jump"
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
      "Insert TODO heading with ‘org-insert-heading-respect-content’ set to t"
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
      "Remove current file from the list of files in variable ‘org-agenda-files’"
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
    (("t t" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$todo :other-rest-args (org org-mode-map)) "Todo manipulation"
      :enable t :exit t)
     ("t c" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$clock :other-rest-args (org org-mode-map)) "Clock Operation"
      :enable t :exit t)
     ("t a" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$agenda :other-rest-args (org org-mode-map)) "Agenda Refer"
      :enable t :exit t)
     ("t m" (:pretty-hydra-cabinet entropy/emacs-org-keymap-group-$timer :other-rest-args (org org-mode-map)) "Timer manipulation"
      :enable t :exit t))))


;; ****** org rss
(defvar entropy/emacs-org-keymap-group-$org-rss
  '("Rss"
    (("C-c C-x G" org-feed-goto-inbox
      "Go to the inbox that captures the feed named FEED"
      :enable t :map-inject t :exit t) ;; All the other keys
     ("C-c C-x g" org-feed-update-all
      "Get inbox items from all feeds in ‘org-feed-alist’"
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

(entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
 'org-mode 'org 'org-mode-map t
 (append
  entropy/emacs-org-keymap-group-$basic-manipulation
  entropy/emacs-org-keymap-group-$buffer-navigation
  entropy/emacs-org-keymap-group-$task-manipulation
  entropy/emacs-org-keymap-group-$org-rss
  entropy/emacs-org-keymap-group-$export-and-preview
  entropy/emacs-org-keymap-group-$misc)
 '(3 2 2)
 '(3 2 2))

;; ** org-id
(use-package org-id
  :ensure nil
  :commands org-id-new
  :config

  (defun entropy/emacs-org-id-add-location-around-advice
      (orig-func &rest orig-args)
    "Around advice for `org-id-add-location' for preventing error
popup when in non-file buffer."
    (if (buffer-file-name (current-buffer))
        (apply orig-func orig-args)
      (ignore-errors (apply orig-func orig-args))))
  (advice-add 'org-id-add-location
              :around
              #'entropy/emacs-org-id-add-location-around-advice)

  ;; Redefun the org-id-new for use '-' instead of ':'
  (defun org-id-new (&optional prefix)
    "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org-4nd91V40HI\".

Note: this function has been redefined to use '-' instead of ':'
as the hypenation."

    (let* ((prefix (if (eq prefix 'none)
                       ""
                     (concat (or prefix org-id-prefix) "-")))
           unique)
      (if (equal prefix "-") (setq prefix ""))
      (cond
       ((memq org-id-method '(uuidgen uuid))
        (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
        (unless (org-uuidgen-p unique)
          (setq unique (org-id-uuid))))
       ((eq org-id-method 'org)
        (let* ((etime (org-reverse-string (org-id-time-to-b36)))
               (postfix (if org-id-include-domain
                            (progn
                              (require 'message)
                              (concat "@" (message-make-fqdn))))))
          (setq unique (concat etime postfix))))
       (t (error "Invalid `org-id-method'")))
      (concat prefix unique))))

;; ** entropy-emacs additional function
;; *** tags align
(defun entropy/emacs-org-tags-align (&optional all)
  "Align all tags in one org-mode buffer with align column prompt
inputting, the align number must be positive as that it will be
automatically transferred to the value adapted to
`org-tags-column'."
  (interactive "P")
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

;; *** org file images checking
;; **** extract all images from org file
(defun entropy/emacs-org-ocii-extract-file-imgs-main ()
  "Extracting all images from one org file to the target location
chosen as prompted query location state."
  (interactive)
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
  (require 'entropy-common-library)
  (require 'entropy-org-widget)
  (let ((link-objs (entropy/ow-get-buffer-links (find-file-noselect org-file)))
        links_temp links
        (base-dir (file-name-directory org-file)))
    (when link-objs
      (dolist (el link-objs)
        (push (plist-get el :link) links_temp)))
    (when links_temp
      (dolist (el links_temp)
        (when (string-match "\\(svg\\|imagemagick\\|png\\|gif\\|tiff\\|jpeg\\|xpm\\|xbm\\|pbm\\)$"
                            el)
          (let ((non-abbrev (replace-regexp-in-string "^file:" "" el)))
            (cond
             ((string-match-p "^\\.+" non-abbrev)
              (let ((default-directory base-dir))
                (push (expand-file-name non-abbrev) links)))
             (t
              (unless (entropy/cl-check-filename-legal non-abbrev t)
                (push non-abbrev links))))))))
    links))


;; ** org-bullets
(use-package org-bullets
  :commands (org-bullets-mode)
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :init
  (entropy/emacs-lazy-with-load-trail
   org-bullet-mode-init
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
     (add-hook 'org-mode #'org-bullets-mode)))
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

;; ** ox-reveal
(use-package ox-reveal
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

;; ** org-pomodoro
(use-package org-pomodoro)

;; ** org export setting
(use-package entropy-org-export-theme-toggle
  :ensure nil
  :commands (entropy/org-exptth-set-head))

;; ** org-download
(use-package org-download
  :commands (org-download-image
             org-download-screenshot
             org-download-delete
             org-download-enable)
  :bind
  (:map org-mode-map
        ("\C-cp" . entropy/emacs-org-download-screenshot))
  :init

  ;; Init setting for changing the default annotation method and support unicode dir-name
  (progn
    ;; (require 'org-download)

    (setq-default org-download-image-dir "../annex/img/")
    (setq-default org-download-heading-lvl nil)	;; Cancel taxonomy make image directory

    ;; Support unicode filename
    (defun entropy/emacs-org--custom-download-method (link)
      (org-download--fullname (org-link-unescape link)))
    (setq org-download-method 'entropy/emacs-org--custom-download-method) ; notice, this field can not using lambda expression

    ;; Donwload annotation specifiction
    (setq org-download-annotate-function
          '(lambda (link)
             (org-download-annotate-default (org-link-unescape link)))))

  (setq org-download-annotate-function 'ignore)

;; *** config
  :config

;; **** Redefine or-download-dnd about

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

  (if (image-type-available-p 'imagemagick)
      (defun org-download-dnd-fallback (uri action)
        "Note: This function has been pseudo maked because of
the unneeded feature that fall-back to using internal originally
type to deal with drop event of draging image to drop it in
`org-mode'."
        (let ((dnd-protocol-alist
               (rassq-delete-all
                'org-download-dnd
                (copy-alist dnd-protocol-alist))))
          ;; (dnd-handle-one-url nil action uri)
          nil)))


  (defun org-download-dnd (uri action)
    "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch.

Note: This function has been redifined to adding
`org-display-inline-images'."
    (cond ((eq major-mode 'org-mode)
           (condition-case nil
               (org-download-image uri)
             (error
              (org-download-dnd-fallback uri action))))
          ((eq major-mode 'dired-mode)
           (org-download-dired uri))
          ;; redirect to someone else
          (t
           (org-download-dnd-fallback uri action)))
    (org-display-inline-images))


;; **** Redefine org-download-screenshot function to support auto org-indent current inserted annotation.
  (if (and sys/is-win-group
           (executable-find
            (car (split-string entropy/emacs-win-org-download-screenshot-method))))
      (progn
        (defun entropy/emacs-org-download-screenshot ()
          "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'.

And Now you are in windows plattform we defualt use SnippingTool
to be stuff. And you can change it's value by the variable
`entropy/emacs-win-org-download-screenshot-method' it will pass itself
to `org-download-screenshot-method' .
"
          (interactive)
          (if buffer-read-only
              (read-only-mode 0))
          (let ((link entropy/emacs-win-org-download-file-name))
            (shell-command (format org-download-screenshot-method link))
            (org-download-image link)
            (beginning-of-line)
            (org-indent-line)
            (delete-file link)
            (org-display-inline-images)))
        (setq org-download-screenshot-method entropy/emacs-win-org-download-screenshot-method))
    (defun entropy/emacs-org-download-screenshot ()
      "
Note: this function was derived and extended from
org-download-screenshot

Capture screenshot and insert the resulting file.
The screenshot tool is determined by
`org-download-screenshot-method'.
"
      (interactive)
      (org-download-screenshot)
      (beginning-of-line)
      (org-indent-line)
      (org-display-inline-images)))


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
              (post-foward-re "\\|.*?/?.*?$")
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

;; ** Dwimin commenting code region with org mode
(use-package poporg
  :bind(("C-c \"" . poporg-dwim)
        :map poporg-mode-map
        ("C-c C-c" . org-table-align)
        ("C-c C-k" . poporg-update))
  :commands (poporg-dwim)
  :config
  (setq poporg-adjust-fill-column nil)
  (defun entropy/emacs-org--poporg-edit-hook ()
    "Hooks for `poporg-edit-hook' compat for entropy-emacs."
    (auto-fill-mode)
    (setq-local fill-column 66)
    (setq-local org-adapt-indentation nil))

  (add-hook 'poporg-edit-hook
            'entropy/emacs-org--poporg-edit-hook
            t)

  (defun entropy/emacs-org--poporg-dwim-add-comment-line-head-whitespace (&rest _)
    (when (and (use-region-p)
               (not (buffer-narrowed-p)))
      (let* ((orig-start (region-beginning))
             (orig-end (region-end))
             (cm-str (buffer-substring-no-properties
                      (region-beginning)
                      (region-end)))
             new-cm-str
             (cur-mode major-mode)
             (ro-state buffer-read-only))
        (when buffer-read-only
          (read-only-mode 0))

        (with-temp-buffer
          (when buffer-read-only
            (read-only-mode 0))
          (erase-buffer)
          (goto-char (point-min))
          (insert cm-str)
          (let ((whadd-func
                 (lambda (x)
                   (goto-char (point-min))
                   (while (re-search-forward
                           x nil t)
                     (insert " "))
                   (setq new-cm-str
                         (buffer-substring-no-properties
                          (point-min)
                          (point-max))))))
            (cl-case cur-mode
              ((lisp-interaction-mode emacs-lisp-mode)
               (funcall whadd-func "^\\s-*;;$"))
              (sh-mode
               (funcall whadd-func "^\\s-*#$")))))

        (when (and (not (null new-cm-str))
                   (not (equal cm-str new-cm-str)))
          (goto-char orig-start)
          (delete-region orig-start orig-end)
          (insert new-cm-str)
          (funcall-interactively 'set-mark-command nil)
          (goto-char orig-start))

        (cond
          ((eq ro-state nil) (read-only-mode 0))
          ((eq ro-state t) (read-only-mode 1))))))

  (defun entropy/emacs-org--poporg-dwim-unnarrowed-buffer (&rest _)
    (with-current-buffer (current-buffer)
      (when (buffer-narrowed-p)
        (save-excursion
          (widen)))))

  (defun entropy/emacs-org--poporg-dwim-unlock-orig-buffer (&rest _)
    (with-current-buffer (poporg-orig-buffer)
      (when buffer-read-only
        (read-only-mode 0))))

  (advice-add 'poporg-dwim
              :before
              #'entropy/emacs-org--poporg-dwim-add-comment-line-head-whitespace)
  (advice-add 'poporg-dwim
              :before
              #'entropy/emacs-org--poporg-dwim-unnarrowed-buffer)

  (advice-add 'poporg-edit-exit
              :before
              #'entropy/emacs-org--poporg-dwim-unlock-orig-buffer))

;; * provide
(provide 'entropy-emacs-org)
