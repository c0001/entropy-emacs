;; Bug reproduce emacs init.el related to
;; https://github.com/emacs-lsp/lsp-ui/issues/530
;;
;; USAGE: emacs -q -l _path_of_this_file
;;
;; Please hover the mounse on the symbol `scanf' or `main' on test.c
;; buffer thens scroll the lsp-ui-doc frame window by mounse-wheel,
;; then the bug will be occurred with freezing session, and beeping
;; sequentially while emacs bell ring is not null.
;;
;; If the bug not occurred try enter <f11> let emacs judging into
;; fullscreen and redo above section, this may occurred as well.
;;

(package-initialize)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ))
(package-refresh-contents)
(setq inhibit-startup-screen t)

(defalias '_buginit
  (lambda (&rest _)
    (when (display-graphic-p)
      (let* ((testdir "~/TEST/lsp-ui-bug_530")
             (test-c-file
              (expand-file-name "test.c" testdir))
             (inhibit-read-only t)
             buffer)
        (dolist (pkg '(lsp-mode lsp-ui))
          (package-install pkg))
        (mkdir testdir t)
        (setq
         mouse-wheel-scroll-amount '(1 ((shift) . 1))
         mouse-wheel-progressive-speed nil)

        (with-current-buffer
            (setq buffer (find-file test-c-file))
          (erase-buffer)
          (insert
           "#include <stdio.h>

int main(int argc, char *argv[])
{
    char c;
    scanf(\"%c\", &c);
    return 0;
}
"
           )
          (save-buffer)
          (unless (eq major-mode 'c-mode)
            (c-mode))
          (require 'lsp)
          (lsp))
        (switch-to-buffer buffer)))))

(_buginit)
