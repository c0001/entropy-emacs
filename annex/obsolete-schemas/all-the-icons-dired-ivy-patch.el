;; ==assign format-function to swiper for fix some-bug==
;; when using `all-the-icons-dired' with swiper, icons displayed in
;; associate dired-buffer are mapped with all-the-icons spec fonts
;; which can not rendered correctly for other fonts. ivy's defaut
;; format function `ivy-format-function-default' used for `swiper'
;; which using `identify' to format inactive candis which using the
;; default face as font-lock atribtue, it will corrupts the
;; correctly font displaying when set spec font to this default
;; face.
(entropy/emacs-lazy-load-simple 'all-the-icons-dired
  (defun entropy/emacs-ivy--swiper-format-function-for-dired (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face str 'ivy-current-match))
     (lambda (str)
       (cond
        ((eq major-mode 'dired-mode)
         (ivy--add-face str 'entropy/emacs-defface-face-for-swiper-dired-candi-inactive-face))
        (t
         (identity str))))
     cands
     "\n"))
  (add-to-list 'ivy-format-functions-alist
               (cons 'swiper #'entropy/emacs-ivy--swiper-format-function-for-dired)))
