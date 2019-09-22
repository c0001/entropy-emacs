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

;; ** Pre advice
(defun entropy/emacs-org--export-panel-around-advice (old-func &rest args)
  (unless (fboundp 'org-reveal-export-to-html)
    (when (featurep 'ox-reveal)
      (load-library "ox-reveal")))
  (let ((entropy/emacs-web-development-environment t))
    (apply old-func args)))

(entropy/emacs-lazy-load-simple 'ox
  (advice-add 'org-export-dispatch
              :around #'entropy/emacs-org--export-panel-around-advice))


;; ** main

(use-package org
;; *** init
  :ensure nil
  :defines  (org-mode-map)
  :commands (orgstruct-mode
             org-mode
             org-store-link
             org-agenda
             org-capture
             org-switchb
             org-previous-item
             org-next-item
             org-toggle-link-display
             org-babel-result-hide-all)
  :diminish orgstruct-mode
;; *** binding keys
  :bind
  (("\C-cl" . org-store-link)
   ("\C-ca" . org-agenda)
   ("\C-cc" . org-capture)
   ("\C-cb" . org-switchb)
   :map org-mode-map
   ("C-<up>" . org-previous-item)
   ("C-<down>" . org-next-item)
   ("<C-f2>" . org-toggle-link-display))

;; *** hook
  :hook ((org-mode . org-babel-result-hide-all))

  :preface
  (defun entropy/emacs-org--do-load-org ()
    (let ((prop-format (lambda (msg-str)
                         (redisplay t)
                         (message
                          (format
                           "*Lazy loading*: %s ... [this may cost some time, take a coffee -v-]"
                           (propertize msg-str
                                       'face
                                       'font-lock-type-face)))
                         (redisplay t))))
      (funcall prop-format "org-core")
      (require 'org)
      (funcall prop-format "org-ob-core")
      (require 'ob)
      (funcall prop-format "org-babels")
      (let ((ob-lang (mapcar
                      #'(lambda (x) (cons x t))
                      '(vala
                        tangle
                        table
                        stan
                        sqlite
                        sql
                        shen
                        shell
                        sed
                        screen
                        scheme
                        sass
                        ruby
                        ref
                        python
                        processing
                        plantuml
                        picolisp
                        perl
                        org
                        octave
                        ocaml
                        mscgen
                        maxima
                        matlab
                        makefile
                        lua
                        lob
                        lisp
                        lilypond
                        ledger
                        latex
                        keys
                        js
                        java
                        io
                        hledger
                        haskell
                        groovy
                        gnuplot
                        fortran
                        forth
                        exp
                        eval
                        emacs-lisp
                        ebnf
                        dot
                        ditaa
                        css
                        core
                        coq
                        comint
                        clojure
                        calc
                        awk
                        asymptote
                        abc
                        R
                        J
                        C))))
        (org-babel-do-load-languages
         'org-babel-load-languages ob-lang))))

  (entropy/emacs-lazy-initial-advice-before
   '(org-mode)
   "org-mode"
   "org-mode"
   (entropy/emacs-org--do-load-org))
  
;; *** configs
  :config
;; **** basic setting  
  (setq-default org-agenda-span (quote day)) ;Set agenda daily show as default
  (setq org-log-done 'time)                  ;insert time-stamp when toggle 'TODO' to 'DONE'
  (setq org-link-search-must-match-exact-headline nil) ;using fuzzy match for org external link
                                                       ;which support the link type:
                                                       ;'file:xxx.org::str'

  
  ;; Choosing org formula convertor
  (when (or sys/linux-x-p sys/mac-x-p)
    (setq org-preview-latex-default-process 'imagemagick)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))
  (when sys/win32p
    (setq org-preview-latex-default-process 'dvisvgm)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)))

  
  (setq-default org-hide-block-startup t) ;Hiding block details at init-time
  (setq org-export-headline-levels 8)     ;Export heading level counts same as raw org that maximum for 8
  (setq org-cycle-separator-lines 30) ;Set the empty line number between the headline without visible
  (setq-default org-cycle-include-plain-lists 'integrate)         ;Vixual cycle with plain list
  (setq-default org-complete-tags-always-offer-all-agenda-tags t) ;Gloable tags match
  (add-to-list 'org-export-backends 'md) ;Adding org export file type - markdown

  (setq org-imenu-depth 8) ; The default depth shown for integrating org heading line to imenu while be within org-mode
;; **** org-capture about
;; ***** hook for org-capture
  (defun entropy/emacs-org--capture-indent-buffer (&optional arg)
    "Indent org capture buffer when finished capture editting."
    (let ((pm (point-min))
          (pb (point-max)))
      (org-indent-region pm pb)
      (goto-char (point-max))
      (insert "\n")))
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
    (when (string-match-p "^CAPTURE-" (buffer-name))
      (user-error "Do not toggle link display in 'org capture' buffer.")))

  (advice-add 'org-toggle-link-display :before #'entropy/emacs-org--capture-forbidden-toggle-link-display)

  
;; **** let org-heading insert without new blank always
  ;; ==================================================
  ;; The principle for follow function was like below code block:
  ;; (setq org-blank-before-new-entry
  ;;       '((headding . nil)
  ;;         (plain-list-item . nil)))
  ;; ==================================================

  (defun entropy/emacs-org--call-rebinding-org-blank-behaviour (fn)
    (let ((org-blank-before-new-entry
           (copy-tree org-blank-before-new-entry)))
      (rplacd (assoc 'heading org-blank-before-new-entry) nil)
      (rplacd (assoc 'plain-list-item org-blank-before-new-entry) nil)
      (call-interactively fn)))

  (defun entropy/emacs-org-smart-meta-return-dwim ()
    (interactive)
    (entropy/emacs-org--call-rebinding-org-blank-behaviour 'org-meta-return))

  (defun entropy/emacs-org-smart-insert-todo-heading-dwim ()
    (interactive)
    (entropy/emacs-org--call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

  (define-key org-mode-map (kbd "M-<return>") 'entropy/emacs-org-smart-meta-return-dwim)

  
;; **** define 'end' key to `org-end-of-line'
  (define-key org-mode-map (kbd "<end>") 'org-end-of-line)
;; **** org open at point enhanced

  ;; change the find-file method of org-open-at-point instead of find-file-other-window
  (defun entropy/emacs-org-open-at-point ()
    (interactive)
    (let ((org-link-frame-setup
           (acons 'file 'find-file org-link-frame-setup))
          (process-connection-type
           (cond
            ((eq system-type 'gnu/linux)
             nil)
            (t t))))
      (org-open-at-point)))
  (define-key org-mode-map (kbd "C-c C-o") 'entropy/emacs-org-open-at-point)

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
  (define-key org-mode-map (kbd "C-c M-o") 'entropy/emacs-org-eow)
  
;; **** clear key-map of 'C-c C-w'
  
  ;; delete keybind `C-c C-w' of org-capture-refile for fix the contradiction with eyebrowse.
  (entropy/emacs-lazy-load-simple 'org-capture
    (define-key org-capture-mode-map (kbd "C-c C-w") nil))

  ;; delete keybind of org-mode
  (define-key org-mode-map (kbd "C-c C-w") nil)

  ;; delete keybind of org-agenda
  (entropy/emacs-lazy-load-simple 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c C-w") nil))

;; **** org-priority-setting
  (setq org-lowest-priority 90)         ;lowset prioty was 'Z'
  (setq org-default-priority 67)        ;default prioty was 'C'
  
;; **** org-agenda-setting
;; ***** org-agenda-prefix
  (if (or sys/linux-x-p sys/mac-x-p sys/win32p)
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
  (if (not (version< emacs-version "26.1"))
      (add-hook 'org-agenda-mode-hook #'(lambda () (display-line-numbers-mode 0))))

;; ***** org-tags-match-list-sublevels
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
      (error "You must use it in org agenda buffer!")))
  

;; **** org heading face
  
  ;; Some emacs-theme will adjust heading height for obtain better visual sense, but it will break
  ;; the text align state, so using follow function to avoid it.
  
  (add-hook 'org-mode-hook 'entropy/emacs-adjust-org-heading-scale)


;; **** org-refile gloable and 9 depths
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  (setq org-url-hexify-p nil)
  (setq-default org-link-file-path-type (quote relative))
  
;; ***** org babel src mode engines
  ;; ---------with the bug of none highlites in org mode src html block
  ;; ---------and the issue with none aspiration of web-mode maintainer with link
  ;; ----------------------------->`https://github.com/fxbois/web-mode/issues/636'
  ;; (add-to-list 'org-src-lang-modes '("html" . web))


;; ***** org babel evaluate confirm
  (entropy/emacs-lazy-load-simple 'org
    (when (not (version< org-version "9.1.9"))
      (defvar entropy/emacs-org--src-info nil
        "Current org babel info using for `entropy/emacs-org--babel-comfirm-evaluate'.")

      (defun org-babel-exp-src-block ()
        "Process source block for export.
Depending on the \":export\" header argument, replace the source
code block like this:

both ---- display the code and the results

code ---- the default, display the code inside the block but do
          not process

results - just like none only the block is run on export ensuring
          that its results are present in the Org mode buffer

none ---- do not display either code or results upon export

Assume point is at block opening line.

Note: This func has been modified for compat with entropy-emacs.

      Adding part of the export 'info' as the current value of
      variabel `entropy/emacs-org--src-info'."
        (interactive)
        (save-excursion
          (let* ((info (org-babel-get-src-block-info 'light))
	         (lang (nth 0 info))
	         (raw-params (nth 2 info))
	         hash)
            ;; bail if we couldn't get any info from the block
            (unless noninteractive
	      (message "org-babel-exp process %s at position %d..."
		       lang
		       (line-beginning-position)))
            (setq entropy/emacs-org--src-info info)
            (when info
	      ;; if we're actually going to need the parameters
	      (when (member (cdr (assq :exports (nth 2 info))) '("both" "results"))
	        (let ((lang-headers (intern (concat "org-babel-default-header-args:"
					            lang))))
	          (org-babel-exp--at-source
		      (setf (nth 2 info)
		            (org-babel-process-params
		             (apply #'org-babel-merge-params
			            org-babel-default-header-args
			            (and (boundp lang-headers)
				         (symbol-value lang-headers))
			            (append (org-babel-params-from-properties lang)
				            (list raw-params)))))))
	        (setf hash (org-babel-sha1-hash info)))
	      (org-babel-exp-do-export info 'block hash)))))

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

;; ***** org global export macro
  (entropy/emacs-lazy-load-simple 'ox
    (add-to-list 'org-export-global-macros
                 '("kbd" . "@@html:<code>$1</code>@@")))
  
;; ***** html export
;; ****** html exported head coding
  (setq org-html-coding-system 'utf-8-unix)
  
  ;; inhibit org self default html style
  (setq org-html-head-include-scripts nil
        org-html-head-include-default-style nil)
  
;; ****** solve the bug when cjk paragraph exported to html has the auto newline space char including.
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

  
;; ****** org export html open function

  ;; If `entropy/emacs-browse-url-function' is detectived then open
  ;; exported html file with it instead of using default apps or
  ;; system type.
  
  (defun entropy/emacs-org--hexpt-function (&optional path link judge)
    "Function embeded into `org-file-apps', used for
`entropy/emacs-org--hexpt-advice'.

If judge t, use the first section part of this function for
returning the type of exec for open exported html file, they are:

- \"personal\": using `entropy/emacs-browse-url-function' to open the
  exported html file.

- \"automatic\": using the way by `org-open-file' to automatilly
  open exported file."
    (require 'entropy-common-library-const)
    (if judge
        (let (rtn)
          (if (and entropy/emacs-enable-personal-browse-url-function
                   entropy/emacs-browse-url-function)
              (setq rtn "personal")
            (setq rtn "automatic"))
          rtn)
      (if link
          (if (yes-or-no-p "Open html file with entropy/emacs-browse-url-function ? ")
              (progn
                (funcall entropy/emacs-browse-url-function
                         (concat "file:///" (url-hexify-string link entropy/cl-url--allowed-chars)))
                (message "Using entropy/emacs-browse-url-function to open exported html file."))
            (org-open-file path 'system))
        (error "Invalid link!"))))


  (defun entropy/emacs-org--hexpt-advice (&rest arg-rest)
    "Advice for `org-open-file' for changing the \"html\"
    associtated function when exporting html file from org file
    or open html file link in org-mode.

    This function use `entropy/emacs-org--hexpt-function' to judge the exec
    type for chosen the way whether embeded it into `org-file-apps'."
    (let* ((embeded '("\\.\\(x\\|m\\)?html?\\'" . entropy/emacs-org--hexpt-function))
           (type (entropy/emacs-org--hexpt-function nil nil t)))
      (if (string= "personal" type)
          (progn
            (if (not (member embeded org-file-apps))
                (add-to-list 'org-file-apps embeded)))
        (when (string-match "\\.\\(x\\|m\\)?html?$" (car arg-rest))
          (message
           "Using automatic method to open exported html file! ")))))


  (advice-add 'org-open-file :before #'entropy/emacs-org--hexpt-advice)

  (defun entropy/emacs-org--hexpt-after-advice (&rest arg-rest)
    "Delete embeded file apps from `org-file-apps'."
    (let ((embeded '("\\.\\(x\\|m\\)?html?\\'" . entropy/emacs-org--hexpt-function)))
      (if (member embeded org-file-apps)
          (setq org-file-apps (delete embeded org-file-apps)))))

  (advice-add 'org-open-file :after #'entropy/emacs-org--hexpt-after-advice)
  
;; ****** org ignore broken links
  (setq org-export-with-broken-links 'mark)

;; ***** org publish config

  ;; Force using the utf-8 coding system while publish process
  (advice-add 'org-publish :before #'entropy/emacs-lang-set-utf-8)

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
    
;; **** org-counsel-set-tag
  (entropy/emacs-lazy-load-simple 'org
    (bind-key "C-c C-q" #'counsel-org-tag org-mode-map))
  (entropy/emacs-lazy-load-simple 'org-agenda
    (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))

;; **** org-inline-image size
  (if (image-type-available-p 'imagemagick)
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
  
;; **** org-auto-insert 'CUSTOM-ID'
  ;;      which source code from the bloag@
  ;;      `https://writequit.org/articles/emacs-org-mode-generate-ids.html#h-cf29e5e7-b456-4842-a3f7-e9185897ac3b'
;; ***** baisc function
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
  
;; ***** interactive function
  ;; Originally function that can not auto detected '#+OPTIOINS: auto-id:t'
  (defun entropy/emacs-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive)
    (org-map-entries (lambda () (entropy/emacs-org--custom-id-get (point) 'create)) t nil))

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
        (org-map-entries (lambda () (entropy/emacs-org--custom-id-get (point) 'create)) t nil))))

;; ***** autamatically add ids to saved org-mode headlines
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         (eq buffer-read-only nil))
                (entropy/emacs-org-auto-add-ids-to-headlines-in-file))))
  
;; **** fix problem for browse cached url
  
  ;;	This problem was that cann't open loacal cached url for xdg-open in linux both in fedora and
  ;;	archlinux but windows and can not to find the nature of this issue til now [2018-02-01 Thu
  ;;	14:27:08]

;; ***** first mehtod set the `process-connection-type' to nill   
  ;; Use pipes for subprocess communication
  ;; 
  ;;	This config will cause shell-mode error upon emacs 27 and little mistake for emacs 25 or
  ;;	earlier,you could search the variable for mor info.
  ;; 
  ;;(setq process-connection-type nil)

;; ***** second method use external browser for GNU/Linux opertion system
  ;; (when sys/linuxp
  ;;   (cond
  ;;    ((eq entropy/emacs-default-external-browser 'chrome)
  ;;     (add-hook 'org-mode-hook
  ;; 		'(lambda ()
  ;; 		   (delete '("\\.x?html?\\'" . default) org-file-apps)
  ;; 		   (add-to-list 'org-file-apps '("\\.x?html?\\'" . "google-chrome-stable %s")))
  ;; 		))

  ;;    ((eq entropy/emacs-default-external-browser 'firefox)
  ;;     (add-hook 'org-mode-hook
  ;; 		'(lambda ()
  ;; 		   (delete '("\\.x?html?\\'" . default) org-file-apps)
  ;; 		   (add-to-list 'org-file-apps '("\\.x?html?\\'" . "firefox %s")))
  ;; 		))))

;; ***** third method to use the system default apps 
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

  
;; **** fix bugs of open directory using external apps in windows
  (when sys/win32p
    (add-to-list 'org-file-apps '(directory . emacs))))
;; ** Redefun the org-id-new for use '-' instead of ':'
(use-package org-id
  :ensure nil
  :commands org-id-new
  :config
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
(defun entropy/emacs-org-tags-align ()
  "Align all tags in one org-mode buffer with align column prompt
inputting."
  (interactive)
  (let ((org-tags-column (string-to-number
                          (read-string "please insert the tag align column: "))))
    (org-set-tags 'universal-argument t)))

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
    (setq imgs-paths (entropy/emacs-org--ocii-extract-file-imgs-links target-file)
          temp/x3 imgs-paths)
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
    (setq temp/xx links_temp)
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

;; ** org-htmlize for export code block with coloful sensitive which suitable with programe
;;    languages
(use-package htmlize)

;; ** org-bullets
(when (and (display-graphic-p)
           entropy/emacs-enable-org-bullets)
  (use-package org-bullets
    :commands (org-bullets-mode)
    :hook (org-mode . (lambda () (org-bullets-mode 1)))
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
                                      "Ⅹ" "Ⅺ" "Ⅻ")))))

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
  (if (and (or sys/win32p sys/cygwinp)
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
  
  (advice-add 'poporg-dwim
              :before
              #'entropy/emacs-org--poporg-dwim-add-comment-line-head-whitespace))

;; * provide
(provide 'entropy-emacs-org)
