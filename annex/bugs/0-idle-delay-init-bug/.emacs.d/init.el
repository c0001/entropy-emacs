(setq use-dialog-box nil)
(defun _start-test ()
  (yes-or-no-p "See if there's any pre-typed key here?"))
(run-with-idle-timer
 0 nil #'_start-test)
