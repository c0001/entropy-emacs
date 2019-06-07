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
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)


(when (or sys/win32p sys/linux-x-p sys/mac-x-p)
  (customize-set-variable 'scroll-bar-mode nil)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (redisplay t))

;; ** theme in loading progress
(when (display-graphic-p)
  (let ((initial-theme-path (expand-file-name "startup-theme" (file-name-directory load-file-name))))
    (add-to-list 'custom-theme-load-path initial-theme-path)
    (add-to-list 'load-path initial-theme-path)
    (load-theme 'entropy-base16-theme-bright t)))

;; ** init-frame position and width and height
(if (< entropy/emacs-init-frame-width-scale 1)
    (when (or sys/win32p sys/linux-x-p sys/mac-x-p)
      (if (< entropy/emacs-font-size-default 15)
          (set-face-attribute 'default nil :height (ceiling (* entropy/emacs-font-size-default 10)))
        (error "Your default font size is too large, you must set it smaller than 15."))

      (defun entropy/emacs-ui-set-frame-position ()
        "Reset frame's position and size by the constraint of `entropy/emacs-init-frame-width-scale',
`entropy/emacs-init-frame-height-scale'

And the aim effection for it is that let frame be in the middle of current displayer you be in with
the specific height and width determined by above variable you setted."
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
      (when entropy/emacs-init-fpos-enable
        (entropy/emacs-ui-set-frame-position)))
  
  (when (or sys/win32p sys/linux-x-p sys/mac-x-p)
    (setq initial-frame-alist (quote ((fullscreen . maximized))))
    (setq default-frame-alist initial-frame-alist)))

;; ** elisp show parent
(unless entropy/emacs-use-highlight-features
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'lisp-interaction-mode-hook 'show-paren-mode))
;; ** Don't use GTK+ tooltip

;; Follow the emacs warning by starting up with daemon mode, adding follow codes to avoid it.
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; ** Logo
(setq fancy-splash-image entropy/emacs-fancy-splash-logo)

;; ** initial buffer
(when entropy/emacs-enable-initial-dashboard
;; *** varaible defination  
  (defvar entropy/emacs-ui--dashboard-last-width nil
    "Remain the window size of previous (the last) buffer
    `entropy/emacs-dashboard-buffer-name''s widnow.")
  (defvar entropy/emacs-ui--dashboard-width (window-width)
    "Default entropy emacs initial dashboard width. ")
  (defface entropy/emacs-ui--dashboard-title-face '((t ()))
    "Face for entropy-emacs initial buffer title.")
  (set-face-attribute 'entropy/emacs-ui--dashboard-title-face
                      nil :height 2.5 :bold t :underline t :overline t)

  (defvar entropy/emacs-dashboard-text-logo-file
    (expand-file-name "logo/logo.txt"
                      (file-name-directory load-file-name))
    "Text logo file.")

;; *** libraries
  (defconst entropy/emacs-ui--dashboard-widget-entry-info-list
    `(((:str "- Read ")
       (:str "entropy-emacs introduction"
        :link-type file
        :link ,(expand-file-name
                "elements/submodules/entropy-emacs-doc/org/entropy-emacs_introduction.org"
                entropy/emacs-ext-deps-dir))
       (:str ".")
       (:str "(view ")
       (:str "html version"
        :link-type web
        :link ,(concat "file://"
                (expand-file-name
                 "entropy-emacs-doc/org/entropy-emacs_introduction.html"
                 entropy/emacs-ext-deps-dir)))
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
       (:str ".")))
    "The default entropy-emacs dashbaord widget infos const. Each
    entry string displayed must be single line style without any
    newline partition.")

  (defun entropy/emacs-ui--dashboard-gen-widget-entries ()
    "Generate entropy-emacs initial dashboard buffer widget's
source cons which the car was the list contained the element as
the format proper with widiget entry insert func
`entropy/emacs-ui--dashboard-insert-widget-entry' and the cdr was the
widget width calc by the lagest entry string width plus two (two
char \"|\")."
    (let ((str-obj-list entropy/emacs-ui--dashboard-widget-entry-info-list)
          str-counts-list
          str-fancy-list)
      ;; Generate the string length list of each widiget entry using for generate the align-width
      ;; (the max string width among on the list elements) for func
      ;; `entropy/emacs-ui--dashboard-widget-expand-str-obj'.
      (let ((counter 0))
        (dolist (el str-obj-list)
          (dolist (el2 el)
            (setq counter (+ counter (length (plist-get el2 :str)))))
          (push counter str-counts-list)
          (setq counter 0))
        (setq str-counts-list (reverse str-counts-list)))
      ;; Return the widget entries list used for the insert func
      ;; `entropy/emacs-ui--dashboard-create-widget'. list contains the car as list of str-fancy-obj format
      ;; relied on func `entropy/emacs-ui--dashboard-insert-widget-entry' and the cdr as align-width for the
      ;; widget width specific.
      (let (temp-fancy
            (align-width (apply 'max str-counts-list))
            (n 0))
        (dolist (el str-obj-list)
          (let ((current-str-width (nth n str-counts-list)))
            (push (entropy/emacs-ui--dashboard-widget-expand-str-obj el align-width current-str-width) str-fancy-list)
            (setq n (1+ n))))
        (setq str-fancy-list (reverse str-fancy-list)
              str-fancy-list (cons str-fancy-list (+ 2 align-width))))
      str-fancy-list))

  (defun entropy/emacs-ui--dashboard-widget-expand-str-obj (str-obj align-width current-str-width)
    "Expand string object used by entropy emacs dashboard widget
to the programmable list format used by func
`entropy/emacs-ui--dashboard-insert-widget-entry'.

NOTICE: each string object was defualt as single line entry
without any newline partition which wil be display correctly as:

\"| Read emacs tutorial. |\"

left and right vertical char was the decoration for each widget
entry."
    (let (atom-list rtn)
      (dolist (el str-obj)
        (push (entropy/emacs-ui--dashboard-widget-expand-str-obj-atom el) atom-list))
      (dolist (el atom-list)
        (setq rtn (append el rtn)))
      (setq rtn (append '(:face default "|") rtn))
      (when (< current-str-width align-width)
        (setq rtn (entropy/emacs-ui--dashboard-widget-align-str-space rtn align-width current-str-width)))
      (setq rtn (append rtn '(:face default "|\n")))
      rtn))
  
  (defun entropy/emacs-ui--dashboard-widget-expand-str-obj-atom (str-obj-atom)
    "Expand entropy emacs dashboard widget string object's
element str to the elemet will be append by
`entropy/emacs-ui--dashboard-widget-expand-str-obj'. "
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

  (defun entropy/emacs-ui--dashboard-widget-align-str-space (str-fancy-obj align-width current-str-width)
    "Align widget entry width to the align width ALIGN-WIDTH by
fill the space to trailling of it."
    (let ((n (- align-width current-str-width))
          (rtn ""))
      (while (not (equal n 0))
        (setq rtn (concat " " rtn))
        (setq n (1- n)))
      (setq rtn (append str-fancy-obj (list rtn)))
      rtn))


  (defun entropy/emacs-ui--dashboard-create-widget ()
    "Create entropy-emacs dashboard widget using widget entry
insert func `entropy/emacs-ui--dashboard-insert-widget-entry'."
    (let* ((widget (entropy/emacs-ui--dashboard-gen-widget-entries))
           (widget-entries (car widget))
           (widget-width (cdr widget))
           (left-margin (make-string (max 0
                                          (floor (/ (- entropy/emacs-ui--dashboard-width widget-width) 2)))
                                     ?\ )))
      (dolist (el widget-entries)
        (insert left-margin)
        (entropy/emacs-ui--dashboard-insert-widget-entry el))))

  
  (defun entropy/emacs-ui--dashboard-insert-widget-entry (args)
    " NOTE: this function was the fork of func
`fancy-splash-insert' and be modified for compating with
entropy-emacs initial buffer creater
`entropy/emacs-ui--dashboard-initial-buffer'.

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


  (defun entropy/emacs-ui--dashboard-extract-text-logo (logo_id)
    "Extract text logo stored in file
`entropy/emacs-dashboard-text-logo-file' and return the text logo
module (see `entropy/emacs-ui--dashboard-text-logo-align').
"
    (let (($f entropy/emacs-dashboard-text-logo-file)
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

  
;; *** main function

  (defun entropy/emacs-ui--dashboard-text-logo-align (text-logo)
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
                            (- entropy/emacs-ui--dashboard-width
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


  
  (defun entropy/emacs-ui--dashboard-initial-buffer ()
    "Create entroy-emacs initial buffer.

First insert entropy-emacs logo into initial buffer
`entropy/emacs-dashboard-buffer-name', and then insert 'welcom' title
and entropy-emacs version with tag description. Last to insert
widget used func `entropy/emacs-ui--dashboard-create-widget'."
    (let ((buffer (get-buffer-create entropy/emacs-dashboard-buffer-name))
          (img (ignore-errors (create-image entropy/emacs-fancy-splash-logo)))
          (title " WELCOME TO ENTROPY-EMACS ")
          (version entropy/emacs-ecv))
      (with-current-buffer buffer
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
            (entropy/emacs-ui--dashboard-text-logo-align
             (entropy/emacs-ui--dashboard-extract-text-logo 1)))))
        (insert "\n\n\n\n")
        (insert (make-string
                 (max 0 (floor
                         (/
                          (- entropy/emacs-ui--dashboard-width
                             ;; Adjusting title string width with it's display visual face width
                             ;; from it's height.
                             (* (length title)
                                (if (display-graphic-p)
                                    (let ((height (face-attribute 'entropy/emacs-ui--dashboard-title-face
                                                                  :height)))
                                      (if height height 1))
                                  1)))
                          2)))
                 ?\ ))
        (insert (propertize title 'face 'entropy/emacs-ui--dashboard-title-face))
        (insert "\n")
        (insert (make-string (floor (/ (- entropy/emacs-ui--dashboard-width (length version)) 2)) ?\ ))
        (insert entropy/emacs-ecv)
        (insert "\n\n\n\n\n\n")
        (entropy/emacs-ui--dashboard-create-widget)
        (setq entropy/emacs-ui--dashboard-last-width (window-width))
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
	    (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))
        (read-only-mode 1)
        (unless (display-graphic-p)
          (setq-local browse-url-browser-function
                      'eww-browse-url)))
      buffer))
  (setq initial-buffer-choice #'entropy/emacs-ui--dashboard-initial-buffer)


  (defun entropy/emacs-ui--dashboard-wc-change-func ()
    "Erase and recreate initial buffer when origin window size
was changed, the window modification detector was the variable
`entropy/emacs-ui--dashboard-last-width' which stored the lates initial
buffer window size."
    (let ((buffer-exists (buffer-live-p (get-buffer entropy/emacs-dashboard-buffer-name))))
      (when (or (not (eq entropy/emacs-ui--dashboard-last-width (window-width)))
                (not buffer-exists))
        (setq entropy/emacs-ui--dashboard-width (window-width)
              entropy/emacs-ui--dashboard-last-width entropy/emacs-ui--dashboard-width)
        (with-current-buffer (get-buffer-create entropy/emacs-dashboard-buffer-name)
          (let ((buffer-read-only nil))
            (erase-buffer)
            (entropy/emacs-ui--dashboard-initial-buffer))))))

  (defun entropy/emacs-ui--dashboard-resize-hook (&optional _)
    "Hook useing the core func `entropy/emacs-ui--dashboard-wc-change-func'
for adding to variable `window-size-change-functions' and hook
`window-setup-hook'."
    (let ((win-spec (get-buffer-window entropy/emacs-dashboard-buffer-name))
          (frame-spec (frame-selected-window)))
      (when (and win-spec
                 (not (window-minibuffer-p frame-spec)))
        (with-selected-window win-spec
          (entropy/emacs-ui--dashboard-wc-change-func)))))

  (add-hook 'window-setup-hook
            (lambda ()
              (add-hook 'window-size-change-functions 'entropy/emacs-ui--dashboard-resize-hook)
              (entropy/emacs-ui--dashboard-resize-hook))))



;; ** Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

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
