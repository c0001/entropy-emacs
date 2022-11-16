(toggle-debug-on-quit)
(toggle-frame-maximized)
;; (package-initialize)
(require 'files)
(require 'help)
(with-temp-message
    (substitute-command-keys
     (format
      "emacs %s is hang of `commandp' loop via advice to `find-file', \
please hint `\\[keyboard-quit]' to see the backtrace."
      emacs-major-version))
  (dotimes (cnt 20)
    (let ((sym (intern (format "test-advice/cnt-%s" cnt))))
      (eval
       `(progn
          (defun ,sym (&rest _)
            nil)
          (advice-add 'find-file :before ',sym)))))
  (commandp 'find-file))
