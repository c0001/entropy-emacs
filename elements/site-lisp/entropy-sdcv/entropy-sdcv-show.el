;;; code
;;;; require
(require 'popup)
(require 'pos-tip)
(condition-case nil (require 'posframe) (error (message "Warn: You haven't install posframe!")))

;;;; defcustom
(defcustom entropy/sdcv-show-group nil
  "Customizable variable group of `entropy-sdcv-show'.")

(defcustom entropy/sdcv-show-tooltip-buffer "*entropy/sdcv-show-tooltip*"
  "Entropy sdcv tooltip default buffer name. Both using for
posframe or popup shown mechanism."
  :type 'string
  :group 'entropy/sdcv-show-group)

(defcustom entropy/sdcv-show-showed-buffer "*entropy/sdcv-show-showed-buffer*"
  "Entropy sdcv adjacent feedback presenting buffer name."
  :type 'string
  :group 'entropy/sdcv-show-group)

;;;; library
;;;;; show predicate wrapper
(defmacro entropy/sdcv-show--response-predicate-gen (show-predicate feedback show-method)
  `(lambda ()
     (if (functionp ,show-predicate)
         (funcall ,show-predicate ,feedback ,show-method)
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
  (let ((buffer (entropy/sdcv-show--get-buffer-create entropy/sdcv-show-showed-buffer))
        (feedback (plist-get show-instance :feedback))
        (show-predicate (plist-get show-instance :show-predicate))
        (face (entropy/sdcv-core-use-face (plist-get show-instance :show-face) 'adjacent-common)))
    (when (facep face)
      (setq feedback
            (propertize feedback 'face face)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert feedback)
      (goto-char (point-min))
      (funcall (entropy/sdcv-show--response-predicate-gen
                show-predicate feedback 'adjacent-common))
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
         (predicate (entropy/sdcv-show--response-predicate-gen show-predicate feedback 'posframe))
         (buffer (entropy/sdcv-show--get-buffer-create entropy/sdcv-show-tooltip-buffer)))
    (cond ((eq face nil)
           (posframe-show buffer
                          :string feedback
                          :position (point)
                          :internal-border-width entropy/sdcv-show-posframe-border-width
                          :initialize predicate))
          ((facep face)
           (posframe-show buffer
                          :string feedback
                          :position (point)
                          :background-color (face-attribute face :background)
                          :foreground-color (face-attribute face :foreground)
                          :internal-border-width entropy/sdcv-show-posframe-border-width
                          :initialize predicate)))
    (setq entropy/sdcv-show--posframe-last-point (point)
          entropy/sdcv-show--posframe-last-scroll-offset (window-start))
    (add-hook 'post-command-hook 'entropy/sdcv-show--posframe-hide-after-move)))

(defun entropy/sdcv-show--posframe-hide-after-move ()
  "Quit and delete `entropy/sdcv-show-tooltip-buffer' of posframe
show-type whatever keys touching with. 

This func was automatically added into `post-command-hook' by
`entropy/sdcv-show--show-with-posframe'."
  (ignore-errors
    (when (get-buffer entropy/sdcv-show-tooltip-buffer)
      (unless (and
               (equal (point) entropy/sdcv-show--posframe-last-point)
               (not (eq this-command 'keyboard-quit))
               (equal (window-start) entropy/sdcv-show--posframe-last-scroll-offset))
        (posframe-delete entropy/sdcv-show-tooltip-buffer)
        (kill-buffer entropy/sdcv-show-tooltip-buffer)))))

;;;;; pos-tip
(defcustom entropy/sdcv-show-pos-tip-height-stretch 1.5
  "The height scale for `pos-tip-show-no-propertize'.

Cause for the bug for `pos-tip-show-no-propertize' can not
caculate the multibyte string block height, this var will do the
proper stretching."
  :type 'sexp
  :group 'entropy/sdcv-show-group)

(defun entropy/sdcv-show--pos-tip-show (show-instance)
  (let* ((window (selected-window))
         (frame (window-frame window))
         (feedback (plist-get show-instance :feedback))
         (w-h (pos-tip-string-width-height feedback))
         (show-predicate (plist-get show-instance :show-predicate))
         (face (entropy/sdcv-core-use-face (plist-get show-instance :show-face) 'pos-tip)))
    (setq feedback (funcall (entropy/sdcv-show--response-predicate-gen
                             show-predicate feedback 'pos-tip)))
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
                             show-predicate feedback 'popup)))
    (if (null (facep face))
        (popup-tip feedback :point (point) :margin 1 :truncate t)
      (set-face-attribute 'popup-tip-face nil
                          :foreground (face-attribute face :foreground)
                          :background (face-attribute face :background))
      (popup-tip feedback :point (point) :margin 1 :truncate t)
      (set-face-attribute 'popup-tip-face nil
                          :foreground (car $pface_temp)
                          :background (cdr $pface_temp)))))


;;; provide

(dolist (el '((adjacent-common . entropy/sdcv-show--show-with-buffer-common)
              (posframe . entropy/sdcv-show--show-with-posframe)
              (popup . entropy/sdcv-show--show-with-popup)
              (pos-tip . entropy/sdcv-show--show-with-postip)))
  (add-to-list 'entropy/sdcv-core-response-show-frontends
               el))

(provide 'entropy-sdcv-show)
