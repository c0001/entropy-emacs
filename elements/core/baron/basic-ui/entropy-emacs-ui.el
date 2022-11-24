;;; entropy-emacs-ui.el --- entropy-emacs UI basic configuration  -*- lexical-binding: t; -*-
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
(!eemacs-require 'entropy-emacs-defconst)
(!eemacs-require 'entropy-emacs-defcustom)
(!eemacs-require 'entropy-emacs-defface)

(let ((this-init-done nil))
  (defun entropy/emacs-ui-disable-emacs-bar-refer-uifeature
      (&optional force)
    "Disable emacs bar refer ui features.

Defautly this function just can be invoked once, unless FORCE is
non-nil."
    (unless (or
             (and this-init-done
                  (not force))
             (catch :exit
               ;; we should judge whether the bar feature supported in
               ;; current emacs build.
               (dolist (func '(scroll-bar-mode
                               tool-bar-mode
                               menu-bar-mode))
                 (unless (fboundp func)
                   (throw :exit t)))))
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (setq this-init-done t)
      ;; (redisplay t)
      )))

(cond
 (entropy/emacs-fall-love-with-pdumper
  ;; NOTE: we do not deal thus for pdumper session here since we
  ;; inject `entropy/emacs-ui-disable-emacs-bar-refer-uifeature' to
  ;; `entropy/emacs-pdumper--recovery' directly.
  ;;
  ;; Q: why not inject to `entropy/emacs-pdumper-load-hook'
  ;;
  ;; A: since `entropy/emacs-pdumper-load-hook' is not run firstly
  ;; when pdumper session ui startup, so that we can not disable bar
  ;; refer ui feature before the frame show up immediately.
  nil)
 ((daemonp)
  (entropy/emacs-with-daemon-make-frame-done
    'disable-emacs-bar-refer-ui-features (&rest _)
    (entropy/emacs-ui-disable-emacs-bar-refer-uifeature)))
 (t
  (entropy/emacs-ui-disable-emacs-bar-refer-uifeature)))

;; ** theme in loading progress
(defun entropy/emacs-ui--load-basic-theme-core ()
  (when (display-graphic-p)
    (let ((initial-theme-path entropy/emacs-initial-theme-path))
      (add-to-list 'custom-theme-load-path initial-theme-path)
      (add-to-list 'load-path initial-theme-path)
      (load-theme 'entropy-base16-theme-bright t))))

(unless (or entropy/emacs-fall-love-with-pdumper (daemonp))
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
  ;; FIXME: frame position set not effectually when in maximized or
  ;; fullscreen status, so we must undo 'full' status.
  (cond ((entropy/emacs-frame-is-maximized-p)
         (toggle-frame-maximized))
        ((entropy/emacs-frame-is-fullscreen-p)
         (toggle-frame-fullscreen)
         (when (entropy/emacs-frame-is-maximized-p)
           (toggle-frame-maximized))))
  (let (x y width height)
    ;; widht height, coordinate calculate.
    (setq width
          (ceiling
           (* (x-display-pixel-width)
              entropy/emacs-init-frame-width-scale)))
    (setq height
          (ceiling
           (* (x-display-pixel-height)
              entropy/emacs-init-frame-height-scale)))
    (setq x (ceiling (/ (- (x-display-pixel-width) width) 2)))
    (setq y (if entropy/emacs-init-fpos-y entropy/emacs-init-fpos-y 0))
    ;; frame position setting
    (set-frame-width (selected-frame) width nil t)
    (set-frame-height (selected-frame) height nil t)
    (set-frame-position (selected-frame) x y)))

(when (and entropy/emacs-init-fpos-enable
           (< entropy/emacs-init-frame-width-scale 1))
  ;; use the idle timer to prevent eemacs startup redisplay lag
  (run-with-idle-timer
   0.2 nil #'(lambda () (when (display-graphic-p)
                          (entropy/emacs-ui-set-frame-position)))))

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
  (defvar entropy/emacs-ui--init-welcom-width (window-width)
    "Default entropy emacs initial dashboard width. ")

;; **** initial buffer minor mode

  (defun entropy/emacs-ui-init-welcom-mode-switch-to-scratch-buffer
      ()
    "Switch to scratch buffer."
    (interactive)
    (when (and (buffer-live-p (get-buffer "*scratch*"))
               (bound-and-true-p entropy/emacs-ui-init-welcom-mode))
      (switch-to-buffer "*scratch*")))

  (defvar entropy/emacs-ui-init-welcom-mode-map
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap
        (kbd "q")
        #'entropy/emacs-ui-init-welcom-mode-switch-to-scratch-buffer)
      keymap))

  (define-minor-mode entropy/emacs-ui-init-welcom-mode
    "Minor mode for `entropy/emacs-init-welcome-buffer-name'."
    :init-value nil
    :keymap entropy/emacs-ui-init-welcom-mode-map
    :global nil)

;; **** libraries

  (defun entropy/emacs-ui--init-welcom-gen-widget-entry-info-list ()
    `(((:str "- Read ")
       (:str "entropy-emacs"
             :link-type file
             :link ,(plist-get
                     entropy/emacs-core-doc-file-archives-plist
                     :org))
       (:str " introduction.")
       (:str " (view ")
       (:str "html version"
             :link-type web
             :link ,(concat "file://"
                            (plist-get
                             entropy/emacs-core-doc-file-archives-plist
                             :html)))
       (:str " go.) "))

      ((:str "- Customize ")
       (:str "entropy-emacs"
             :link-type custom-group
             :link entropy-emacs-customize-top-group)
       (:str " interactively."))

      ((:str "- Get ")
       (:str "entropy-emacs"
             :link-type web
             :link ,entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url)
       (:str " stable elpa/melpa packages."))

      ((:str "- Get ")
       (:str "entropy-emacs"
             :link-type web
             :link ,entropy/emacs-ext-eemacs-fonts-archive-url)
       (:str " suggested fonts."))

      ((:str "- Learn ")
       (:str "\"Structure and Interpretation of Computer Programs\""
             :link-type info
             :link "SICP")
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
        (setq str-counts-list (nreverse str-counts-list)))
      ;; Return the widget entries list used for the insert func
      ;; `entropy/emacs-ui--init-welcom-create-widget'. list contains the car as list of str-fancy-obj format
      ;; relied on func `entropy/emacs-ui--init-welcom-insert-widget-entry' and the cdr as align-width for the
      ;; widget width specific.
      (let ((align-width (apply 'max str-counts-list))
            (n 0))
        (dolist (el str-obj-list)
          (let ((current-str-width (nth n str-counts-list)))
            (push (entropy/emacs-ui--init-welcom-widget-expand-str-obj
                   el align-width current-str-width)
                  str-fancy-list)
            (setq n (1+ n))))
        (setq str-fancy-list (nreverse str-fancy-list)
              str-fancy-list (cons str-fancy-list (+ 2 align-width))))
      str-fancy-list))

  (defun entropy/emacs-ui--init-welcom-widget-expand-str-obj
      (str-obj align-width current-str-width)
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
        (push (entropy/emacs-ui--init-welcom-widget-expand-str-obj-atom
               el)
              atom-list))
      (dolist (el atom-list)
        (setq rtn (append el rtn)))
      (setq rtn (append '(:face default "|") rtn))
      (when (< current-str-width align-width)
        (setq rtn
              (entropy/emacs-ui--init-welcom-widget-align-str-space
               rtn align-width current-str-width)))
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
                                 :link `(,str ,(list 'lambda '(_botton) link)))))
                ((eq link-type 'info)
                 (setq link (plist-get str-obj-atom :link))
                 (setq rtn (list :face 'default
                                 :link `(,str ,(list 'lambda '(_botton)
                                                     `(info ,link))))))
                ((eq link-type 'custom-group)
                 (setq link (plist-get str-obj-atom :link))
                 (setq rtn (list :face 'default
                                 :link `(,str ,(list 'lambda '(_botton)
                                                     `(customize-group ',link))))))
                )
        (setq rtn `(:face default ,str)))
      rtn))

  (defun entropy/emacs-ui--init-welcom-widget-align-str-space
      (str-fancy-obj align-width current-str-width)
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
           (left-margin
            (make-string
             (max
              0
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
          (forward-line 1)
          ;; ensure at the beginning of the line
          (forward-line 0)
          (let ((beg (point))
                end)
            (re-search-forward "\\(?:^</begin>\\)" nil t)
            (backward-char 9)
            (setq end (point))
            (push (buffer-substring beg end) rtn))))
      (setq rtn (nreverse rtn))
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
        (setq rtn `((:str ,(split-string str-choice "\n")
                          :face nil :max_len ,max_len))))))

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
          (nreverse
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

  (defun entropy/emacs-ui--init-welcom-initial-buffer (&optional use-buffer)
    "Create entroy-emacs initial buffer.

First insert entropy-emacs logo into initial buffer
`entropy/emacs-init-welcome-buffer-name', and then insert 'welcom' title
and entropy-emacs version with tag description. Last to insert
widget used func `entropy/emacs-ui--init-welcom-create-widget'."
    (let ((buffer
           (or use-buffer
               (get-buffer-create entropy/emacs-init-welcome-buffer-name)))
          img
          (title " WELCOME TO ENTROPY-EMACS ")
          (version entropy/emacs-ecv))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (insert "\n\n\n")
        (forward-line 0)
        (if (and (display-graphic-p)
                 (entropy/emacs-setf-by-body img
                   (ignore-errors
                     (create-image entropy/emacs-fancy-splash-logo-file))))
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
                                    (let ((height
                                           (face-attribute
                                            'entropy/emacs-defface-face-for-welcome-buffer-title-face
                                            :height)))
                                      (if height height 1))
                                  1)))
                          2)))
                 ?\ ))
        (insert (propertize title 'face 'entropy/emacs-defface-face-for-welcome-buffer-title-face))
        (insert "\n")
        (insert (make-string (floor (/ (- entropy/emacs-ui--init-welcom-width (length version)) 2)) ?\ ))
        (insert entropy/emacs-ecv)
        (insert "\n\n\n\n\n\n")
        (entropy/emacs-ui--init-welcom-create-widget)
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
            (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (unless (display-graphic-p)
          (setq-local browse-url-browser-function
                      'eww-browse-url))
        (entropy/emacs-ui-init-welcom-mode))
      buffer))

  (defun entropy/emacs-ui--init-welcom-wc-change-func (&optional use-buffer)
    "Erase and recreate initial buffer when origin window size was
changed."
    (when-let* ((buff (or use-buffer
                          (get-buffer entropy/emacs-init-welcome-buffer-name)))
                (buff-win (get-buffer-window buff))
                (buff-lp (and buff buff-win))
                (buff-win-curw (with-selected-window buff-win (window-width))))
      (unless (eq entropy/emacs-ui--init-welcom-width buff-win-curw)
        (setq entropy/emacs-ui--init-welcom-width buff-win-curw)
        (with-current-buffer buff
          (let ((buffer-read-only nil))
            (erase-buffer)
            (entropy/emacs-ui--init-welcom-initial-buffer buff))))))

  (defun entropy/emacs-ui--init-welcom-resize-hook (&optional _)
    "Hook using the core func
`entropy/emacs-ui--init-welcom-wc-change-func' for adding to
variable `window-size-change-functions' with dups same displayed
window removed since we just need one welcom buffer."
    (entropy/emacs-when-let*-firstn 2
        (((not (minibufferp)))
         (wins (get-buffer-window-list
                entropy/emacs-init-welcome-buffer-name
                'nomini 'visible))
         (selframe (selected-frame))
         (ignore-window-parameters t)
         win-spec)
      (when (cdr wins)
        ;; just remain one welcom window in current `selected-frame'
        ;; if exists, and remove others in any other frame.
        (entropy/emacs-setf-by-body wins
          (entropy/emacs-mapcar-without-orphans
           (lambda (win)
             (if (eq (window-frame win) selframe) win
               (if (frame-root-window-p win)
                   (set-window-buffer win (get-buffer-create "*scratch*"))
                 (delete-window win)) nil))
           wins nil nil))
        (if wins (mapc #'delete-window (cdr wins))))
      (when (and (setq win-spec (car wins)) (window-live-p win-spec))
        (entropy/emacs-ui--init-welcom-wc-change-func))))

;; **** welcom initialize

  (defun entropy/emacs-ui--init-welcom-init-core (&optional use-initial)
    (when-let*
        ((buff
          (if use-initial
              (let ((buffgen (get-buffer-create entropy/emacs-init-welcome-buffer-name)))
                (setq initial-buffer-choice
                      #'(lambda () (entropy/emacs-ui--init-welcom-initial-buffer buffgen)
                          buffgen))
                buffgen)
            (entropy/emacs-ui--init-welcom-initial-buffer)))
         ((buffer-live-p buff)))
      (with-current-buffer buff
        (add-hook 'window-size-change-functions
                  'entropy/emacs-ui--init-welcom-resize-hook
                  nil t))
      (unless use-initial (set-window-buffer (frame-root-window) buff))))

  (setq inhibit-startup-screen t)

  (if entropy/emacs-fall-love-with-pdumper
      (entropy/emacs-lazy-with-load-trail
        'welcome-buffer
        (setq entropy/emacs-ui--init-welcom-width (window-width))
        (entropy/emacs-ui--init-welcom-init-core))
    (entropy/emacs-ui--init-welcom-init-core)))

;; *** emacs dashboard
(defvar dashboard-buffer-name)
(defvar dashboard-init-info)
(defvar dashboard-navigator-buttons)
(defvar dashboard-startup-banner)
(defvar dashboard-center-content)
(defvar dashboard-banner-logo-title)
(defvar dashboard-page-separator)
(defvar dashboard-set-navigator)
(defvar dashboard-items)
(defvar dashboard-set-heading-icons)
(defvar dashboard-set-file-icons)

(when (and (eq entropy/emacs-enable-initial-dashboard 'rich)
           (not (daemonp)))

  (with-eval-after-load 'dashboard-widgets
    (setq dashboard-init-info
          (let ((package-count 0)
                (time (float-time
                       (time-subtract entropy/emacs-package-initialize-done-timestamp
                                      entropy/emacs-package-initialize-init-timestamp))))
            (when (bound-and-true-p package-alist)
              (setq package-count (length package-activated-list)))
            (if (zerop package-count)
                (format "Emacs started in %s" time)
              (format "%d packages loaded in %s" package-count time))))

    (advice-add #'dashboard-get-banner-path
                :override
                (lambda (&rest _)
                  (let* ((logo-obj
                          (entropy/emacs-ui--init-welcom-extract-text-logo 3))
                         logo-txt
                         (file (expand-file-name "entropy-emacs-temp-txt-banner.txt"
                                                 entropy/emacs-stuffs-topdir)))
                    (dolist (str (plist-get (car logo-obj) :str))
                      (setq logo-txt (concat logo-txt str "\n")))
                    (with-current-buffer
                        (find-file-noselect file t t)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert logo-txt)
                        (insert "\n")
                        (save-buffer)
                        (kill-buffer)))
                    file))))

  (defun entropy/emacs-rich-dashboard-set-button ()
    (setq
     dashboard-navigator-buttons
     `(((,(when (entropy/emacs-icons-displayable-p)
            (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
         "Homepage" "Browse homepage"
         (lambda (&rest _) (browse-url entropy/emacs-home-page)))

        (,(when (entropy/emacs-icons-displayable-p)
            (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
         "Settings" "Open custom file"
         (lambda (&rest _) (find-file custom-file)))

        (,(if (entropy/emacs-icons-displayable-p)
              (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
            "?")
         "" "Help (?/h)"
         (lambda (&rest _)
           (info (plist-get entropy/emacs-core-doc-file-archives-plist :texinfo)))
         font-lock-string-face)))))

  (defun entropy/emacs-rich-dashboard-init ()
    (message "========== init rich dashboard ==========")
    (require 'dashboard)
    (setq dashboard-startup-banner entropy/emacs-fancy-splash-logo-file
          dashboard-center-content t
          dashboard-banner-logo-title "Welcome To Entropy-Emacs"
          dashboard-page-separator "\n\n"
          dashboard-set-navigator t)
    (entropy/emacs-rich-dashboard-set-button)
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5)))
    (when (display-graphic-p)
      (setq dashboard-set-heading-icons t)
      (setq dashboard-set-file-icons t))
    (dashboard-insert-startupify-lists)
    ;; FIXME: why this is not enough? (i.e. still remained after init)
    (remove-hook 'window-size-change-functions 'dashboard-resize-on-hook)
    (let ((win (or (get-buffer-window entropy/emacs-init-welcome-buffer-name)
                   (error "eemacs internal fatal of no welcom buffer window getted")))
          (dbuff (or (get-buffer dashboard-buffer-name)
                     (error "eemacs internal fatal of failed init dashboard buffer"))))
      (set-window-buffer win dbuff)
      (with-selected-window win
        (goto-char (point-min))
        (hl-line-mode 1)))
    ;; FIXME:
    ;; kill dashboard org pre load opened buffers for let them
    ;; encounter rest org specification in `entropy-emacs-org.el'
    (let (kill-buffer-hook)
      (mapc
       (lambda (buffer)
         (with-current-buffer buffer
           (when (eq major-mode 'org-mode)
             (kill-buffer buffer))))
       (buffer-list)))
    ;; (redisplay t)
    )

  (defun __eemacs/remove-dashboard-messy-injection (&rest _)
    (remove-hook 'window-size-change-functions 'dashboard-resize-on-hook)
    (when-let ((dbuff (get-buffer dashboard-buffer-name)))
      (with-current-buffer dbuff
        (remove-hook 'window-size-change-functions 'dashboard-resize-on-hook t)
        (when (eq major-mode 'dashboard-mode)
          (add-hook 'window-size-change-functions 'dashboard-resize-on-hook
                    nil t)))))
  (advice-add 'dashboard-mode :after #'__eemacs/remove-dashboard-messy-injection)

  (entropy/emacs-add-hook-with-lambda 'rich-dashboard-init
    (&rest _)
    "Initialize eemacs specified `dashboard' configs and its UI."
    ;; we must inject it after startup since it may cause some
    ;; features load at eemacs init time where all lazy config are
    ;; not allowed to run in, so that some collisions may happened.
    :use-hook 'entropy/emacs-after-startup-hook
    :use-append t
    (entropy/emacs-rich-dashboard-init)
    (run-with-idle-timer
     0.1 nil #'__eemacs/remove-dashboard-messy-injection))

  )

;; ** Frame spec
;; *** Title
(entropy/emacs-lazy-with-load-trail
  'frame-title-set
  (setq frame-title-format
        (list "GNU Emacs " emacs-version "@" user-login-name))
  (setq icon-title-format frame-title-format))

;; *** Transparence

(defvar entropy/emacs-ui--loop-alpha-selected-frame-is-did nil)

(defun entropy/emacs-ui-loop-alpha-selected-frame (&optional prefix)
  "Toggle frame transparent status with specified transparent
value as optional interaction while `PREFIX' is non-nil."
  (interactive "P")
  (setq entropy/emacs-ui--loop-alpha-selected-frame-is-did
        (or prefix (null entropy/emacs-ui--loop-alpha-selected-frame-is-did)))
  (let ((bgtr-default entropy/emacs-loop-alpha-value)
        (bgtr
         (when prefix
           (string-to-number
            (read-string
             "Input bg trransparent var (75-95): "))))
        (real-alpha-supported-p
         (or (bound-and-true-p eemacs-emacs-built-with-support-true-alpha-background-p)
             (version< "29" emacs-version)))
        key)
    (setq key (if real-alpha-supported-p 'alpha-background 'alpha))
    ;; Restrict transparent integer value be between 75 to 95 where is
    ;; the best customization
    (setq bgtr
          (let (_)
            (if (not (integerp bgtr))
                bgtr-default
              (if (> bgtr 95)
                  95
                (if (<= bgtr 0)
                    75
                  bgtr)))))
    (funcall
     (lambda (a)
       (unless entropy/emacs-ui--loop-alpha-selected-frame-is-did
         (setq a 100))
       (let ((alpha-items
              (entropy/emacs-mapcar-without-orphans
               (lambda (x) (when (eq (car x) key) x))
               default-frame-alist nil nil))
             (val (if real-alpha-supported-p a (list a 100))))
         (dolist (el alpha-items)
           (setq default-frame-alist (delete el default-frame-alist)))
         (set-frame-parameter (selected-frame) key val)
         (add-to-list 'default-frame-alist (cons key val))))
     bgtr)
    (setq entropy/emacs-loop-alpha-value bgtr)))

(when (and entropy/emacs-start-with-frame-transparent-action
           (display-graphic-p))
  (entropy/emacs-lazy-with-load-trail
    'loop-alpha
    (run-with-idle-timer
     0.2 nil
     #'entropy/emacs-ui-loop-alpha-selected-frame)))

;; ** Misc
;; *** minor misc
;; **** indication mode
;; Toggle buffer size display in the mode line (Size Indication mode).
(run-with-idle-timer 0.2 nil #'size-indication-mode)

(defun entropy/emacs-show-col-row ()
  "Display buffer-position."
  (interactive)
  (let* ((line
          (number-to-string (+ (count-lines 1 (point))
                               (if (bolp) 1 0))))
         (row
          (number-to-string (current-column))))
    (setq row (propertize row 'face 'epa-mark))
    (setq line (propertize line 'face 'epa-mark))
    (message "current-line:%s current-row:%s" line row)))

;; **** line move visual type
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq-default line-move-visual nil)     ; Defaultly disable it for performance issue

;; * provide

(provide 'entropy-emacs-ui)
