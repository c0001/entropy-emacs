;;; code  -*- lexical-binding: t; -*-
;;;; require
(require 'popup)
(require 'pos-tip)
(condition-case nil (require 'posframe) (error (message "Warn: You haven't install posframe!")))

;;;; defcustom
(defgroup entropy/sdcv-show-group nil
  "Customizable variable group of `entropy-sdcv-show'."
  :group 'entropy-sdcv)

(defcustom entropy/sdcv-show-tooltip-buffer-name "*entropy/sdcv-show-tooltip*"
  "Entropy sdcv tooltip default buffer name. Both using for
posframe or popup shown mechanism."
  :type 'string
  :group 'entropy/sdcv-show-group)

(defcustom entropy/sdcv-show-showed-buffer-name "*entropy/sdcv-show-showed-buffer*"
  "Entropy sdcv adjacent feedback presenting buffer name."
  :type 'string
  :group 'entropy/sdcv-show-group)

;;;; library
;;;;; show predicate wrapper
(defmacro entropy/sdcv-show--response-predicate-gen (show-predicate feedback show-method)
  `(lambda ()
     (if (functionp ,show-predicate)
         (funcall ,show-predicate ,feedback ',show-method)
       ,feedback)))

;;;;; kill buffer and window
(defun entropy/sdcv-show--get-buffer-create (buffer)
  (let ((buffer-window (get-buffer-window buffer))
        rtn)
    (when buffer-window
      (delete-window buffer-window))
    (setq rtn
          (or (and (stringp buffer) buffer)
              (buffer-name buffer)))
    (ignore-errors (kill-buffer buffer))
    (get-buffer-create rtn)))

;;;; response adjacent buffer show
;;;;; adjacent-common
(defun entropy/sdcv-show--show-with-buffer-common (show-instance)
  (let ((buffer (entropy/sdcv-show--get-buffer-create entropy/sdcv-show-showed-buffer-name))
        (feedback (plist-get show-instance :feedback))
        (show-predicate (plist-get show-instance :show-predicate))
        (face (entropy/sdcv-core-use-face (plist-get show-instance :show-face) 'adjacent-common)))
    (when (and (facep face)
               ;; do not use tooltip face to render buffer content
               (not (eq face 'tooltip)))
      (setq feedback
            (propertize feedback 'face face)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert feedback)
      (goto-char (point-min))
      (funcall (entropy/sdcv-show--response-predicate-gen
                show-predicate feedback adjacent-common))
      (entropy/sdcv-core-adjacent-buffer-common-mode 1)
      (setq buffer-read-only t))
    (display-buffer buffer)))

;;;; response tooltip show
;;;;; posframe
(defcustom entropy/sdcv-show-posframe-border-width 10
  "Tooltip width for dict query callbacking show as."
  :type 'integer
  :group 'entropy/sdcv-show-group)

(defvar entropy/sdcv-show--posframe-last-point nil
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar entropy/sdcv-show--posframe-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

(defun entropy/sdcv-show--show-with-posframe (show-instance)
  (let* ((feedback (plist-get show-instance :feedback))
         (show-predicate (plist-get show-instance :show-predicate))
         (face (entropy/sdcv-core-use-face (plist-get show-instance :show-face) 'posframe))
         (predicate (entropy/sdcv-show--response-predicate-gen show-predicate feedback posframe))
         (buffer (entropy/sdcv-show--get-buffer-create entropy/sdcv-show-tooltip-buffer-name))
         ;; (common-fg (face-attribute 'entropy/sdcv-core-common-face :foreground))
         ;; (common-bg (face-attribute 'entropy/sdcv-core-common-face :background))
         (color-func
          ;; posframe color spec can not be `undefined' in which case
          ;; it will fallback to unspec which will not follow font
          ;; spec in `:font'
          (lambda (x p)
            (let ((color (face-attribute x p)))
              (and (stringp color)
                   color)))))
    (posframe-show buffer
                   :string feedback
                   :position (point)
                   :background-color (if (eq (face-attribute face :inverse-video) t)
                                         (funcall color-func face :foreground)
                                       (funcall color-func face :background))
                   :foreground-color (if (eq (face-attribute face :inverse-video) t)
                                         (funcall color-func face :background)
                                       (funcall color-func face :foreground))
                   :font (frame-parameter nil 'font)
                   :internal-border-width entropy/sdcv-show-posframe-border-width
                   :initialize predicate)
    (setq entropy/sdcv-show--posframe-last-point (point)
          entropy/sdcv-show--posframe-last-scroll-offset (window-start))
    ;; add to global scope so that we can deletion any posfram related
    (add-hook 'post-command-hook
              'entropy/sdcv-show--posframe-hide-after-move
              )))

(defun entropy/sdcv-show--posframe-hide-after-move ()
  "Quit and delete `entropy/sdcv-show-tooltip-buffer-name' of posframe
show-type whatever keys touching with.

This func was automatically added into `post-command-hook' by
`entropy/sdcv-show--show-with-posframe'."
  (ignore-errors
    (when (get-buffer entropy/sdcv-show-tooltip-buffer-name)
      (let ((del-func
             (lambda (&rest _)
               (posframe-hide entropy/sdcv-show-tooltip-buffer-name)
               ;; (kill-buffer entropy/sdcv-show-tooltip-buffer-name)
               ))
            (del-p nil))
        (cond ((eq
                ;; we should use `real-this-command' since some post
                ;; command may modify the `this-command'.
                real-this-command 'keyboard-quit)
               (funcall del-func)
               (setq del-p t))
              ((not (and
                     (equal (point) entropy/sdcv-show--posframe-last-point)
                     (equal (window-start)
                            entropy/sdcv-show--posframe-last-scroll-offset)))
               (funcall del-func)
               (setq del-p t)))
        ;; ensure the deletion predicated then we can remove the hook
        ;; for guaranteeing the performance
        (when del-p
          (remove-hook 'post-command-hook
                       'entropy/sdcv-show--posframe-hide-after-move
                       ))))))

;;;;; pos-tip
(defcustom entropy/sdcv-show-pos-tip-height-stretch 1.5
  "The height scale for `pos-tip-show-no-propertize'.

Cause for the bug for `pos-tip-show-no-propertize' can not
caculate the multibyte string block height, this var will do the
proper stretching."
  :type 'number
  :group 'entropy/sdcv-show-group)

(defun entropy/sdcv-show--pos-tip-show (show-instance)
  (let* ((window (selected-window))
         (frame (window-frame window))
         (feedback (plist-get show-instance :feedback))
         (w-h (pos-tip-string-width-height feedback))
         (show-predicate (plist-get show-instance :show-predicate))
         (face (entropy/sdcv-core-use-face
                (plist-get show-instance :show-face)
                'pos-tip)))
    (setq feedback (funcall (entropy/sdcv-show--response-predicate-gen
                             show-predicate feedback pos-tip)))
    (pos-tip-show-no-propertize
     feedback
     face
     (point) nil -1
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (ceiling (* (pos-tip-tooltip-height
                  (cdr w-h)
                  (frame-char-height frame) frame)
                 entropy/sdcv-show-pos-tip-height-stretch)))))

(defun entropy/sdcv-show--show-with-postip (show-instance)
  (let ((pos-tip-internal-border-width 12))
    (entropy/sdcv-show--pos-tip-show show-instance)))

;;;;; popup
(defun entropy/sdcv-show--show-with-popup (show-instance)
  (let (($pface_temp (copy-tree
                      (cons (face-attribute 'popup-tip-face :foreground)
                            (face-attribute 'popup-tip-face :background))))
        (popup-tip-max-width entropy/sdcv-core-response-column-width-max)
        (feedback (plist-get show-instance :feedback))
        (show-predicate (plist-get show-instance :show-predicate))
        (face (entropy/sdcv-core-use-face (plist-get show-instance :show-face) 'popup)))
    (setq feedback (funcall (entropy/sdcv-show--response-predicate-gen
                             show-predicate feedback popup)))
    (set-face-attribute 'popup-tip-face nil
                        :foreground (face-attribute face :foreground)
                        :background (face-attribute face :background))
    (popup-tip feedback :point (point) :margin 1 :truncate t)
    (set-face-attribute 'popup-tip-face nil
                        :foreground (car $pface_temp)
                        :background (cdr $pface_temp))))


;;; minibuffer show

(defun entropy/sdcv-show--show-with-minibuffer-common (show-instance)
  (let ((feedback (plist-get show-instance :feedback))
        (show-predicate (plist-get show-instance :show-predicate))
        (face (entropy/sdcv-core-use-face
               (plist-get show-instance :show-face)
               'minibuffer-common)))
    (setq feedback (funcall
                    (entropy/sdcv-show--response-predicate-gen
                     show-predicate feedback minibuffer-common)))
    (when (and (facep face)
               ;; do not use `tool-tip' render content in minibuffer
               (not (eq face 'tooltip)))
      (setq feedback
            (propertize feedback 'face face)))
    (message feedback)))

;;; provide

(dolist (el '((adjacent-common . entropy/sdcv-show--show-with-buffer-common)
              (posframe . entropy/sdcv-show--show-with-posframe)
              (popup . entropy/sdcv-show--show-with-popup)
              (pos-tip . entropy/sdcv-show--show-with-postip)
              (minibuffer-common . entropy/sdcv-show--show-with-minibuffer-common)))
  (add-to-list 'entropy/sdcv-core-response-show-frontends
               el))

(provide 'entropy-sdcv-show)
