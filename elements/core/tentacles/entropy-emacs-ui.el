;;; entropy-emacs-ui.el --- entropy-emacs UI basic configuration
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ui.el
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
;; `entropy-emacs' UI basic configuration.
;;
;; * Configuration:
;;
;; Using for `entropy-emacs' only, be without hacking warranty.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defface)

(when
    (not entropy/emacs-fall-love-with-pdumper)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (redisplay t))

;; ** theme in loading progress
(defun entropy/emacs-ui--load-basic-theme-core ()
  (when (display-graphic-p)
    (let ((initial-theme-path entropy/emacs-initial-theme-path))
      (add-to-list 'custom-theme-load-path initial-theme-path)
      (add-to-list 'load-path initial-theme-path)
      (load-theme 'entropy-base16-theme-bright t))))

(if entropy/emacs-fall-love-with-pdumper
    (entropy/emacs-lazy-with-load-trail
     load-basic-theme
     (entropy/emacs-ui--load-basic-theme-core))
  (entropy/emacs-ui--load-basic-theme-core))

;; ** init-frame position and width and height

(defun entropy/emacs-ui-set-frame-position ()
  "Reset frame's position and size by the constraint of
`entropy/emacs-init-frame-width-scale',
`entropy/emacs-init-frame-height-scale'

And the aim effection for it is that let frame be in the middle
of current displayer you be in with the specific height and width
determined by above variable you setted."
  (interactive)
  (let (x y width height)
    ;; widht height, coordinate calculate.
    (setq width (ceiling (* (x-display-pixel-width) entropy/emacs-init-frame-width-scale)))
    (setq height (ceiling (* (x-display-pixel-height) entropy/emacs-init-frame-height-scale)))
    (setq x (ceiling (/ (- (x-display-pixel-width) width) 2)))
    (setq y (if entropy/emacs-init-fpos-y entropy/emacs-init-fpos-y 0))
    ;; frame position setting
    (set-frame-width (selected-frame) width nil t)
    (set-frame-height (selected-frame) height nil t)
    (set-frame-position (selected-frame) x y)))

(defvar entropy/emacs-ui--init-frame-maximized nil)
(when (and (display-graphic-p)
           (not entropy/emacs-fall-love-with-pdumper))
  ;; gurantee all init promting showed available since the small
  ;; startup frame size was ugly.
  (toggle-frame-maximized)
  (setq entropy/emacs-ui--init-frame-maximized t)
  ;; gurantee for toggled done!
  (sleep-for 0.1)
  (redisplay t))

(entropy/emacs-lazy-with-load-trail
 frame-reset-size&width
 :start-end t
 :body
 (when (display-graphic-p)
   (when (and entropy/emacs-init-fpos-enable
              (< entropy/emacs-init-frame-width-scale 1))
     (when entropy/emacs-ui--init-frame-maximized
       (toggle-frame-maximized))
     (sleep-for 0.1)
     (redisplay t)
     (entropy/emacs-ui-set-frame-position)
     (redisplay t))))

;; ** elisp show parent
(unless entropy/emacs-use-highlight-features
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'lisp-interaction-mode-hook 'show-paren-mode))

;; ** Don't use GTK+ tooltip

;; Follow the emacs warning by starting up with daemon mode, adding follow codes to avoid it.
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; ** Logo
(setq fancy-splash-image entropy/emacs-fancy-splash-logo-file)

;; ** initial buffer
;; *** entropy init welcom screen
(when (and (not (null entropy/emacs-enable-initial-dashboard))
           (not (daemonp)))

;; **** varaible defination
  (defvar entropy/emacs-ui--init-welcom-last-width nil
    "Remain the window size of previous (the last) buffer
    `entropy/emacs-init-welcom-buffer-name''s widnow.")
  (defvar entropy/emacs-ui--init-welcom-width (window-width)
    "Default entropy emacs initial dashboard width. ")

;; **** initial buffer minor mode
  (defvar entropy/emacs-ui-init-welcom-mode-map
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap
        (kbd "q")
        (lambda (&rest _)
          (interactive)
          (when (buffer-live-p (get-buffer "*scratch*"))
            (switch-to-buffer
             "*scratch*"))))
      keymap))

  (define-minor-mode entropy/emacs-ui-init-welcom-mode
    "Minor mode for `entropy/emacs-init-welcom-buffer-name'."
    :init-value nil
    :global nil)

;; **** libraries

  (defun entropy/emacs-ui--init-welcom-gen-widget-entry-info-list ()
    `(((:str "- Read ")
       (:str "entropy-emacs introduction"
             :link-type file
             :link ,(plist-get entropy/emacs-doc-path :org))
       (:str ".")
       (:str "(view ")
       (:str "html version"
             :link-type web
             :link ,(concat "file://"
                            (plist-get entropy/emacs-doc-path :html)))
       (:str " go.) "))

      ((:str "- Get ")
       (:str "entropy-emax64 encapsulation"
             :link-type web
             :link "https://sourceforge.net/projects/entropy-emax64/")
       (:str "."))

      ((:str "- View ")
       (:str "emacs tutorial"
             :link-type help
             :link (help-with-tutorial))
       (:str "."))))

  (defvar entropy/emacs-ui--init-welcom-widget-entry-info-list
    (entropy/emacs-ui--init-welcom-gen-widget-entry-info-list)
    "The default entropy-emacs dashbaord widget infos const. Each
entry string displayed must be single line style without any
newline partition.")

  (defun entropy/emacs-ui--init-welcom-gen-widget-entries ()
    "Generate entropy-emacs initial dashboard buffer widget's
source cons which the car was the list contained the element as
the format proper with widiget entry insert func
`entropy/emacs-ui--init-welcom-insert-widget-entry' and the cdr was the
widget width calc by the lagest entry string width plus two (two
char \"|\")."
    (let ((str-obj-list entropy/emacs-ui--init-welcom-widget-entry-info-list)
          str-counts-list
          str-fancy-list)
      ;; Generate the string length list of each widiget entry using for generate the align-width
      ;; (the max string width among on the list elements) for func
      ;; `entropy/emacs-ui--init-welcom-widget-expand-str-obj'.
      (let ((counter 0))
        (dolist (el str-obj-list)
          (dolist (el2 el)
            (setq counter (+ counter (length (plist-get el2 :str)))))
          (push counter str-counts-list)
          (setq counter 0))
        (setq str-counts-list (reverse str-counts-list)))
      ;; Return the widget entries list used for the insert func
      ;; `entropy/emacs-ui--init-welcom-create-widget'. list contains the car as list of str-fancy-obj format
      ;; relied on func `entropy/emacs-ui--init-welcom-insert-widget-entry' and the cdr as align-width for the
      ;; widget width specific.
      (let (temp-fancy
            (align-width (apply 'max str-counts-list))
            (n 0))
        (dolist (el str-obj-list)
          (let ((current-str-width (nth n str-counts-list)))
            (push (entropy/emacs-ui--init-welcom-widget-expand-str-obj el align-width current-str-width) str-fancy-list)
            (setq n (1+ n))))
        (setq str-fancy-list (reverse str-fancy-list)
              str-fancy-list (cons str-fancy-list (+ 2 align-width))))
      str-fancy-list))

  (defun entropy/emacs-ui--init-welcom-widget-expand-str-obj (str-obj align-width current-str-width)
    "Expand string object used by entropy emacs dashboard widget
to the programmable list format used by func
`entropy/emacs-ui--init-welcom-insert-widget-entry'.

NOTICE: each string object was defualt as single line entry
without any newline partition which wil be display correctly as:

\"| Read emacs tutorial. |\"

left and right vertical char was the decoration for each widget
entry."
    (let (atom-list rtn)
      (dolist (el str-obj)
        (push (entropy/emacs-ui--init-welcom-widget-expand-str-obj-atom el) atom-list))
      (dolist (el atom-list)
        (setq rtn (append el rtn)))
      (setq rtn (append '(:face default "|") rtn))
      (when (< current-str-width align-width)
        (setq rtn (entropy/emacs-ui--init-welcom-widget-align-str-space rtn align-width current-str-width)))
      (setq rtn (append rtn '(:face default "|\n")))
      rtn))

  (defun entropy/emacs-ui--init-welcom-widget-expand-str-obj-atom (str-obj-atom)
    "Expand entropy emacs dashboard widget string object's
element str to the elemet will be append by
`entropy/emacs-ui--init-welcom-widget-expand-str-obj'. "
    (let ((str (plist-get str-obj-atom :str))
          (link-type (plist-get str-obj-atom :link-type))
          link
          rtn)
      (if link-type
          (cond ((eq link-type 'file)
                 (setq link (plist-get str-obj-atom :link))
                 (setq rtn (list :face 'default
                                 :link `(,str ,(list 'lambda '(_button)
                                                     `(find-file ,link))))))
                ((eq link-type 'web)
                 (setq link (plist-get str-obj-atom :link))
                 (setq rtn (list :face 'default
                                 :link `(,str ,(list 'lambda '(_botton)
                                                     `(browse-url ,link))))))
                ((eq link-type 'help)
                 (setq link (plist-get str-obj-atom :link))
                 (setq rtn (list :face 'default
                                 :link `(,str ,(list 'lambda '(_botton) link))))))
        (setq rtn `(:face default ,str)))
      rtn))

  (defun entropy/emacs-ui--init-welcom-widget-align-str-space (str-fancy-obj align-width current-str-width)
    "Align widget entry width to the align width ALIGN-WIDTH by
fill the space to trailling of it."
    (let ((n (- align-width current-str-width))
          (rtn ""))
      (while (not (equal n 0))
        (setq rtn (concat " " rtn))
        (setq n (1- n)))
      (setq rtn (append str-fancy-obj (list rtn)))
      rtn))

  (defun entropy/emacs-ui--init-welcom-create-widget ()
    "Create entropy-emacs dashboard widget using widget entry
insert func `entropy/emacs-ui--init-welcom-insert-widget-entry'."
    (let* ((widget (entropy/emacs-ui--init-welcom-gen-widget-entries))
           (widget-entries (car widget))
           (widget-width (cdr widget))
           (left-margin (make-string (max 0
                                          (floor (/ (- entropy/emacs-ui--init-welcom-width widget-width) 2)))
                                     ?\ )))
      (dolist (el widget-entries)
        (insert left-margin)
        (entropy/emacs-ui--init-welcom-insert-widget-entry el))))

  (defun entropy/emacs-ui--init-welcom-insert-widget-entry (args)
    " NOTE: this function was the fork of func
`fancy-splash-insert' and be modified for compating with
entropy-emacs initial buffer creater
`entropy/emacs-ui--init-welcom-initial-buffer'.

Insert text into the current buffer, with faces.
Arguments from ARGS should be either strings; functions called
with no args that return a string; pairs `:face FACE', where FACE
is a face specification usable with `put-text-property'; or pairs
`:link LINK' where LINK is a list of arguments to pass to
`insert-button', of the form (LABEL ACTION [HELP-ECHO]), which
specifies the button's label, `action' property and help-echo string.
FACE and LINK can also be functions, which are evaluated to obtain
a face or button specification."
    (let ((current-face nil))
      (while args
        (cond ((eq (car args) :face)
               (setq args (cdr args) current-face (car args))
               (if (functionp current-face)
                   (setq current-face (funcall current-face))))
              ((eq (car args) :link)
               (setq args (cdr args))
               (let ((spec (car args)))
                 (if (functionp spec)
                     (setq spec (funcall spec)))
                 (insert-button (car spec)
                                'face (list 'link current-face)
                                'action (cadr spec)
                                'help-echo (concat "mouse-2, RET: "
                                                   (or (nth 2 spec)
                                                       "Follow this link"))
                                'follow-link t)))
              (t (insert (propertize (let ((it (car args)))
                                       (if (functionp it)
                                           (funcall it)
                                         it))
                                     'face current-face
                                     'help-echo (startup-echo-area-message)))))
        (setq args (cdr args)))))

  (defun entropy/emacs-ui--init-welcom-extract-text-logo (logo_id)
    "Extract text logo stored in file
`entropy/emacs-fancy-splash-text-logo-file' and return the text logo
module (see `entropy/emacs-ui--init-welcom-text-logo-align').
"
    (let (($f entropy/emacs-fancy-splash-text-logo-file)
          rtn)
      (with-current-buffer (find-file-noselect $f)
        (goto-char (point-min))
        (while (re-search-forward "^<begin>" nil t)
          (next-line)
          (forward-line 0)
          (let ((beg (point))
                end)
            (re-search-forward "\\(?:^</begin>\\)" nil t)
            (backward-char 9)
            (setq end (point))
            (push (buffer-substring beg end) rtn))))
      (setq rtn (reverse rtn))
      (unless (<= logo_id (length rtn))
        (error "Text logo id overflow!"))
      (let* ((str-choice (nth (- logo_id 1) rtn))
             (str-list (split-string str-choice "\n"))
             max_len)
        (setq rtn nil)
        (setq max_len (apply 'max
                             (mapcar (lambda (str_line)
                                       (string-width str_line))
                                     str-list)))
        (setq rtn `((:str ,(split-string str-choice "\n") :face nil :max_len ,max_len))))))

;; **** main function

  (defun entropy/emacs-ui--init-welcom-text-logo-align (text-logo)
    "Align the text logo TEST-LOGO to the window align style,
text logo module was one plist which has three keys:

- str: the string list sperated by newline.
- face: string face
- max_len: the square lenght of string's buffer presentaion."
    (let (rtn)
      (dolist (el text-logo)
        (push
         (mapconcat
          (lambda (x) x)
          (reverse
           (let (align-list
                 (align_str
                  (make-string
                   (max 0 (floor
                           (/
                            (- entropy/emacs-ui--init-welcom-width
                               ;; Adjusting string width with it's display visual
                               ;; face width from it's height.
                               (* (plist-get el :max_len)
                                  (if (plist-get el :face)
                                      (let ((height (face-attribute (cdr el) :height)))
                                        (if height height 1))
                                    1)))
                            2)))
                   ?\ )))
             (dolist (substr (plist-get el :str))
               (push (concat
                      align_str
                      substr)
                     align-list))
             align-list))
          "
")
         rtn))
      rtn))

  (defun entropy/emacs-ui--init-welcom-initial-buffer ()
    "Create entroy-emacs initial buffer.

First insert entropy-emacs logo into initial buffer
`entropy/emacs-init-welcom-buffer-name', and then insert 'welcom' title
and entropy-emacs version with tag description. Last to insert
widget used func `entropy/emacs-ui--init-welcom-create-widget'."
    (let ((buffer (get-buffer-create entropy/emacs-init-welcom-buffer-name))
          (img (ignore-errors (create-image entropy/emacs-fancy-splash-logo-file)))
          (title " WELCOME TO ENTROPY-EMACS ")
          (version entropy/emacs-ecv))
      (with-current-buffer buffer
        (read-only-mode 0)
        (goto-char (point-min))
        (insert "\n\n\n")
        (forward-line 0)
        (if (and img (display-graphic-p))
            (progn
              (insert (propertize " " 'display
                                  `(space :align-to (+ center (-0.5 . ,img)))))
              (make-button (prog1 (point) (insert-image img)) (point)
                           'face 'default
                           'help-echo "mouse-2, RET: Browse https://www.gnu.org/"
                           'action (lambda (_button) (browse-url "https://www.gnu.org/"))
                           'follow-link t))
          (insert
           (car
            (entropy/emacs-ui--init-welcom-text-logo-align
             (entropy/emacs-ui--init-welcom-extract-text-logo 1)))))
        (insert "\n\n\n\n")
        (insert (make-string
                 (max 0 (floor
                         (/
                          (- entropy/emacs-ui--init-welcom-width
                             ;; Adjusting title string width with it's display visual face width
                             ;; from it's height.
                             (* (length title)
                                (if (display-graphic-p)
                                    (let ((height (face-attribute 'entropy/emacs-defface-face-for-ui-dashboard-title-face
                                                                  :height)))
                                      (if height height 1))
                                  1)))
                          2)))
                 ?\ ))
        (insert (propertize title 'face 'entropy/emacs-defface-face-for-ui-dashboard-title-face))
        (insert "\n")
        (insert (make-string (floor (/ (- entropy/emacs-ui--init-welcom-width (length version)) 2)) ?\ ))
        (insert entropy/emacs-ecv)
        (insert "\n\n\n\n\n\n")
        (entropy/emacs-ui--init-welcom-create-widget)
        (setq entropy/emacs-ui--init-welcom-last-width (window-width))
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
            (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))
        (read-only-mode 1)
        (unless (display-graphic-p)
          (setq-local browse-url-browser-function
                      'eww-browse-url))
        (entropy/emacs-ui-init-welcom-mode))
      buffer))

  (defun entropy/emacs-ui--init-welcom-wc-change-func ()
    "Erase and recreate initial buffer when origin window size
was changed, the window modification detector was the variable
`entropy/emacs-ui--init-welcom-last-width' which stored the lates initial
buffer window size."
    (let ((buffer-exists (buffer-live-p (get-buffer entropy/emacs-init-welcom-buffer-name))))
      (when (or (not (eq entropy/emacs-ui--init-welcom-last-width (window-width)))
                (not buffer-exists))
        (setq entropy/emacs-ui--init-welcom-width (window-width)
              entropy/emacs-ui--init-welcom-last-width entropy/emacs-ui--init-welcom-width)
        (with-current-buffer (get-buffer-create entropy/emacs-init-welcom-buffer-name)
          (let ((buffer-read-only nil))
            (erase-buffer)
            (entropy/emacs-ui--init-welcom-initial-buffer))))))

  (defun entropy/emacs-ui--init-welcom-resize-hook (&optional _)
    "Hook useing the core func `entropy/emacs-ui--init-welcom-wc-change-func'
for adding to variable `window-size-change-functions' and hook
`window-setup-hook'."
    (let ((win-spec (get-buffer-window entropy/emacs-init-welcom-buffer-name))
          (frame-spec (frame-selected-window)))
      (when (and win-spec
                 (not (window-minibuffer-p frame-spec)))
        (with-selected-window win-spec
          (entropy/emacs-ui--init-welcom-wc-change-func)))))

;; **** welcom initialize

  (defun entropy/emacs-ui--init-welcom-resize-run ()
    (unless (member 'entropy/emacs-ui--init-welcom-resize-hook
                    window-size-change-functions)
      (add-hook 'window-size-change-functions 'entropy/emacs-ui--init-welcom-resize-hook))
    (entropy/emacs-ui--init-welcom-resize-hook))

  (defun entropy/emacs-ui--init-welcom-init-core ()
    (setq initial-buffer-choice #'entropy/emacs-ui--init-welcom-initial-buffer)
    (entropy/emacs-ui--init-welcom-resize-run))

  (if entropy/emacs-fall-love-with-pdumper
      (entropy/emacs-lazy-with-load-trail
       welcome-buffer
       (setq entropy/emacs-ui--init-welcom-width (window-width))
       (setq entropy/emacs-ui--init-welcom-widget-entry-info-list
             (entropy/emacs-ui--init-welcom-gen-widget-entry-info-list))
       (entropy/emacs-ui--init-welcom-init-core)
       (entropy/emacs-ui--init-welcom-resize-run)
       (let ((buffer (entropy/emacs-ui--init-welcom-initial-buffer)))
         (unless (eq entropy/emacs-enable-initial-dashboard 'rich)
           (switch-to-buffer buffer))))
    (entropy/emacs-ui--init-welcom-init-core)
    (entropy/emacs-lazy-with-load-trail
     welcome-buffer-refresh
     (kill-buffer entropy/emacs-init-welcom-buffer-name)
     (setq entropy/emacs-ui--init-welcom-widget-entry-info-list
           (entropy/emacs-ui--init-welcom-gen-widget-entry-info-list))
     (let ((buffer (entropy/emacs-ui--init-welcom-initial-buffer)))
       (unless (eq entropy/emacs-enable-initial-dashboard 'rich)
         (switch-to-buffer buffer))))))

;; *** emacs dashboard

(when (and (eq entropy/emacs-enable-initial-dashboard 'rich)
           (not (daemonp)))

  (defun entropy/emacs-rich-dashboard-init ()
    (require 'dashboard)
    (setq dashboard-startup-banner entropy/emacs-fancy-splash-logo-file
          dashboard-center-content t
          dashboard-banner-logo-title "Welcom To Entropy-Emacs"
          dashboard-page-separator "\n\n")
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (agenda . 5)
                            (projects . 5)
                            (registers . 5)))
    (when (display-graphic-p)
      (setq dashboard-set-heading-icons t)
      (setq dashboard-set-file-icons t))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer "*dashboard*")
    (goto-char (point-min))
    (hl-line-mode 1)
    (redisplay t))

  (if entropy/emacs-fall-love-with-pdumper
      (progn
        (entropy/emacs-lazy-with-load-trail
         rich-dashboard
         (entropy/emacs-rich-dashboard-init)
         (add-hook 'window-size-change-functions
                   'dashboard-resize-on-hook)))
    (with-eval-after-load 'entropy-emacs-package
      (add-hook 'entropy/emas-package-common-start-after-hook
                #'entropy/emacs-rich-dashboard-init)
      (add-hook 'window-size-change-functions
                'dashboard-resize-on-hook))))

;; ** Title
(entropy/emacs-lazy-with-load-trail
 frame-title-set
 (setq frame-title-format
       '("GNU Emacs " emacs-version "@" user-login-name " : "
         (:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b"))))
 (setq icon-title-format frame-title-format))

;; ** Misc
;; *** minor misc
(size-indication-mode 1)                ;Toggle buffer size display in the mode line (Size Indication mode).
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

(defun entropy/emacs-show-col-row ()
  "Display buffer-position."
  (interactive)
  (let* ((line   (number-to-string (+ (count-lines 1 (point))
                                      (if (bolp) 1 0))))
         (row   (number-to-string (current-column))))
    (setq row (propertize row 'face 'epa-mark))
    (setq line (propertize line 'face 'epa-mark))
    (message "current-line:%s current-row:%s" line row)))

;; * provide
(provide 'entropy-emacs-ui)
