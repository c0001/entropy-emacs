;; Bug reproduce related to eemacs bug of h-1c9af04d-403f-4050-a8eb-778fc47ff8de
;;
;; USAGE: emacs -Q -l _path_of_this_file
;;
;; Please Hint C-s and C-\ to enable emacs-rime and typing several
;; chars (larger than 5) and typing DEL two or more times to see the
;; bug.

;; * code
;; ** prepare
(setq inhibit-startup-screen t)
(setq debug-on-error t)

;; ** pakcage install

;; *** package initialize
(setq package-user-dir
      (expand-file-name
       "elpa-test"
       user-emacs-directory))
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ))
(package-initialize)
(package-refresh-contents)

;; *** package install
(dolist (ft '(swiper ivy counsel rime))
  (package-install ft))

;; ** init env
(require 'ivy)
(require 'counsel)
(ivy-mode)
(setq ivy-dynamic-exhibit-delay-ms 2)
(counsel-mode)
(setq ivy-add-newline-after-prompt t)
(define-key global-map (kbd "C-s") 'swiper-isearch)

(require 'rime)
(setq rime-user-data-dir (expand-file-name
                          "rime-user-data-test"
                          user-emacs-directory)
      rime-show-candidate 'posframe)
(setq default-input-method "rime")

;; * __end
(provide 'reproduce-bug-of-h-1c9af04d-403f-4050-a8eb-778fc47ff8de)
