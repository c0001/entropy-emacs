;;; entropy-emacs-testwww.el --- Plain text browser configuration for entropy-emacs  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190907  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-textwww.el
;; Keywords:      browser, eww, w3m
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;; Text plain web browser configuration for =entropy-emacs= .
;;
;; Using =pre-uniform= to designed the commands map for =eww= and
;; =w3m= and will be for more.
;;
;; * Configuration:
;;
;; Using for =entropy-emacs= only.
;;
;; * Code:
;; ** require

;; ** uniform

(cl-defmacro entropy/emacs-textwww--hydra-uniform
    (&key mode feature mode-map
          bookmark-library-view bookmark-add
          browse-with-external
          toggle-inline-image toggle-image
          current-page-url current-link-url previous-page next-page
          search-query
          search-engine
          open-history)
  (let ()
    `(entropy/emacs-lazy-initial-for-hook
      (eww-mode-hook w3m-mode-hook)
      ,(format"text-www-hydra-hollow-init/for-%s" mode)
      ,(format"text-www-hydra-hollow-init/for-%s" mode)
      prompt-echo
      :pdumper-no-end t
      (entropy/emacs-hydra-hollow-define-major-mode-hydra-common-sparse-tree
       ',mode '(,feature ,mode-map) t
       '("Basic"
         (("s" ,search-query "Search"
           :enable (not (null ',search-query)) :map-inject (not (null ',mode-map))
           :exit t)
          ("S" ,search-engine "Toggle search engine"
           :enable (not (null ',search-engine)) :map-inject (not (null ',mode-map))
           :exit t)
          ("t" ,toggle-inline-image "Toggle display current image"
           :enable (not (null ',toggle-inline-image)) :map-inject (not (null ',mode-map))
           :exit t)
          ("T" ,toggle-image "Toggle display all images "
           :enable (not (null ',toggle-image)) :map-inject (not (null ',mode-map))
           :exit t))
         "Page Move"
         (("l" ,previous-page "Previous Page"
           :enable (not (null ',previous-page)) :map-inject (not (null ',mode-map))
           :exit t)
          ("n" ,next-page "Next Page"
           :enable (not (null ',next-page)) :map-inject (not (null ',mode-map))
           :exit t)
          ("e" ,browse-with-external "Browse Externally"
           :enable (not (null ',browse-with-external)) :map-inject (not (null ',mode-map))
           :exit t))
         "Link Retrieve"
         (("c" ,current-page-url "Copy Current Page Url"
           :enable (not (null ',current-page-url)) :map-inject (not (null ',mode-map))
           :exit t)
          ("u" ,current-link-url "Copy Current Link Url"
           :enable (not (null ',current-link-url)) :map-inject (not (null ',mode-map))
           :exit t))
         "Bookmark Operation"
         (("b" ,bookmark-library-view "View Bookmarks"
           :enable (not (null ',bookmark-library-view)) :map-inject (not (null ',mode-map))
           :exit t)
          ("a" ,bookmark-add "Add Bookmark"
           :enable (not (null ',bookmark-add)) :map-inject (not (null ',mode-map))
           :exit t)
          ("h" ,open-history "Open browse history"
           :enable (not (null ',open-history)) :map-inject (not (null ',mode-map))
           :exit t)))))))

;; ** browsers
;; *** emacs-w3m interface
(use-package w3m
  :if (executable-find "w3m")
  :commands
  (w3m
   w3m-search
   w3m-goto-url)

  :defines (w3m-bookmark-file)

  :bind
  (:map w3m-mode-map
        ("<down>" . next-line)
        ("<up>" . previous-line)
        ("<left>" . left-char)
        ("<right>" . right-char))

;; **** init
  :init
  ;; basic initial value
  (setq w3m-confirm-leaving-secure-page nil
        w3m-image-no-idle-timer t
        w3m-image-animate-seconds nil

        ;; header line icon disable
        ;; -- emacs < 27 compatible
        ;; w3m-show-graphic-icons-in-header-line nil
        w3m-show-graphic-icons-in-tab-line nil

        w3m-use-favicon nil
        w3m-use-refresh nil
        w3m-use-tab nil
        w3m-use-tab-menubar nil
        w3m-process-timeout 5
        w3m-pop-up-windows nil
        )

  ;; disable the cursor move hook for reduce lagging
  ;; FIXME: we ensure set it after load w3m to take effecient, why?
  (if entropy/emacs-fall-love-with-pdumper
      (entropy/emacs-lazy-with-load-trail
       unset-w3m-after-cursor-move-hook
       :pdumper-no-end t
       :body
       (setq w3m-after-cursor-move-hook nil))
    (entropy/emacs-lazy-load-simple w3m
      (setq w3m-after-cursor-move-hook nil)))

  ;; disable this to prevent its lagging on because of that
  ;; `w3m-redisplay-pages-automatically' is a hook arranged into
  ;; `window-configuration-change-hook'.
  (setq w3m-redisplay-pages-automatically-p nil)

  ;; session configuration
  (defvar w3m-session-autosave)
  (setq w3m-session-autosave nil)
  (defvar w3m-session-deleted-save)
  (setq w3m-session-deleted-save nil)
  (defvar w3m-session-crash-recovery)
  (setq w3m-session-crash-recovery nil)

  ;; shied windows internal synonyms 'convert.exe' with emacs internal
  ;; imagemagick "convert.exe".
  (when (and sys/win32p
             (file-exists-p (concat invocation-directory "convert.exe")))
    (setq w3m-imagick-convert-program
          (concat invocation-directory "convert.exe")))

  ;; w3m personal browse url function
  (defun entropy/emacs-textwww--w3m-browse-url (url &rest _)
    (if (or (bound-and-true-p w3m-make-new-session)
            ;; TODO: more conditions to use new session
            (or (memq major-mode
                      '(elfeed-show-mode elfeed-search-mode))))
        (w3m-goto-url-new-session url)
      (w3m-goto-url url)))

  ;; uniform
  (entropy/emacs-textwww--hydra-uniform
   :mode w3m-mode
   :feature w3m
   :mode-map w3m-mode-map
   :toggle-image w3m-toggle-inline-images
   :toggle-inline-image w3m-toggle-inline-image
   :bookmark-library-view w3m-bookmark-view
   :bookmark-add w3m-bookmark-add-current-url
   :open-history w3m-history
   :browse-with-external w3m-view-url-with-browse-url
   :current-page-url w3m-print-current-url
   :current-link-url w3m-print-this-url
   :previous-page entropy/emacs-textwww-w3m-view-previous-page
   :next-page w3m-view-next-page
   :search-query w3m-search
   :search-engine entropy/emacs-textwww-w3m-toggle-search-engine)

  ;; coding system specified to utf-8 when `current-language-environment' is thus.
  (defvar w3m-bookmark-file-coding-system)
  (when (string= current-language-environment "UTF-8")
    (setq w3m-coding-system 'utf-8
          w3m-default-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-bookmark-file-coding-system 'utf-8
          w3m-url-coding-system-alist '((nil . utf-8))
          ))

  (defun __ya/w3m-bookmark-buffer ()
    "Use exist bookmark visiting file buffer so that we can use user
spec `buffer-file-coding-system' where `w3m' oftenly throw the
error like \"Specified title includes unsafe character(s): %s\"."
    (let ((bbuffn " *w3m bookmark*"))
      (if (and (get-buffer bbuffn)
               (file-equal-p
                (buffer-file-name bbuffn)
                w3m-bookmark-file))
          ;; defaulty resuse the buffer for perfomance issue.
          nil
        ;; NOTE: Fistly we need to close the origin bookmark buffer using
        ;; `w3m' internal API which is used to update its internal status.
        (w3m-kill-buffer bbuffn)
        (let ((large-file-warning-threshold
               most-positive-fixnum))
          (with-current-buffer (find-file-noselect w3m-bookmark-file)
            (when buffer-read-only
              ;; NOTE: its must be an unread only buffer
              (setq buffer-read-only nil)))))))
  (with-eval-after-load 'w3m-bookmark
    (advice-add 'w3m-bookmark-buffer
                :before
                #'__ya/w3m-bookmark-buffer)
    (entropy/emacs-make-function-inhibit-readonly
     #'w3m-bookmark-write-file))

  ;; disable w3m update warning before w3m loading
  (advice-add 'w3m-fix-melpa-installation
              :around
              (lambda (&rest _) nil))

;; **** config
  :config
;; ***** contents decoding patch

  ;; Add missing 'brotli' data compression algorithm for "br" encoder
  ;; type web content transfer format like
  ;;
  ;; https://en.wikipedia.org/wiki/Brotli
  ;;
  ;; #+begin_src emacs-lisp
  ;;   (w3m-attributes "https://archlinux.org/packages/community/any/drawing" t)
  ;;   ;; -> ("text/html" "utf-8" nil "br" nil "https://archlinux.org/packages/community/any/drawing/")
  ;; #+end_src
  ;;
  ;; `w3m' just internally given the 'gzip' 'bzip2' 'deflate' method
  ;; for web data decoding usage, so that `w3m' can not decoding the
  ;; type of "br" when the transferring data is compressed by 'brotli'.
  ;;
  ;; FIXME: we should given an pull request to emacs-w3m upstream.
  ;;
  (dolist (encoder '(("br"     . brotli)
                     ("brotli" . brotli)))
    (add-to-list 'w3m-encoding-alist
                 encoder))
  (add-to-list 'w3m-decoder-alist
               '(brotli "brotli" ("-d")))

  (defun __ya/w3m-which-command (orig-func &rest orig-args)
    "Throw `user-error' for COMMAND not found.

So that we can notify user as thus and preventing ugly procedure
remained."
    (let ((cmd (apply orig-func orig-args)))
      (unless cmd
        (user-error "can not found cmd <%S> in your system!"
                    orig-args))
      cmd))
  (advice-add 'w3m-which-command
              :around
              #'__ya/w3m-which-command)

;; ***** Search engine specified
  (defvar w3m-search-engine-alist)
  (defvar w3m-search-default-engine)
  (entropy/emacs-lazy-load-simple w3m-search
    ;; Default use Microsoft 'bing' search engine for compatible of
    ;; proxy wild problem especially for chinese user.
    (add-to-list 'w3m-search-engine-alist
                 '("bing" "https://www.bing.com/search?q=%s" utf-8))
    (setq w3m-search-default-engine "bing"))

  ;; As an implicit search engine toggle
  (defun entropy/emacs-textwww-w3m-toggle-search-engine ()
    "Choose with prompt for `w3m-search-default-engine'."
    (interactive)
    (let ((choice (completing-read "Choose search engine: "
                                   w3m-search-engine-alist
                                   nil t)))
      (setq w3m-search-default-engine choice)))

;; ***** w3m external browser setting
  (defun entropy/emacs-textwww--w3m-external-advice (oldfunc &rest args)
    (let ((browse-url-browser-function
           (if entropy/emacs-browse-url-function
               entropy/emacs-browse-url-function
             'browse-url-default-browser)))
      (apply oldfunc args)))
  (advice-add 'w3m-view-url-with-browse-url
              :around #'entropy/emacs-textwww--w3m-external-advice)

;; ***** Auto adjusting w3m page width while `text-scale-mode' is on
  (defun entropy/emacs-textwww--w3m-calc-max-cols ()
    (let* ((wwp (window-width nil t))
           (fw (window-font-width)))
      (floor (/ wwp fw))))

  (defun entropy/emacs-textwww--w3m-page-text-scale-hook ()
    "Adjusting w3m buffer page width adapted to the window max
columns width whens entering in or out of `text-scale-mode'.

EEMACS_MAINTENANCE:

Variable `w3m-fill-column' can not be local binding, it seems be
used in process handler which has its own individual buffer whose
value of it is not relavant to current buffer value."
    (cond
     ((and (eq major-mode 'w3m-mode)
           (bound-and-true-p text-scale-mode))
      ;; disable window center mode since there's some visual bug
      (when (bound-and-true-p entropy/emacs-window-center-mode)
        (entropy/emacs-window-center-mode 0))
      (setq w3m-fill-column
            (entropy/emacs-textwww--w3m-calc-max-cols))
      (w3m-redisplay-this-page))
     ((and (eq major-mode 'w3m-mode)
           (not (bound-and-true-p text-scale-mode)))
      (setq w3m-fill-column
            (entropy/emacs-get-symbol-defcustom-value
             'w3m-fill-column))
      (w3m-redisplay-this-page))))
  (add-hook 'text-scale-mode-hook
            #'entropy/emacs-textwww--w3m-page-text-scale-hook)

;; ***** UI tweak
;; ****** `w3m-popup-buffer' tweak

  (defun __ya/w3m-popup-buffer (orig-func &rest orig-args)
    "The eemacs spec to `w3m-popup-buffer'."
    (let ((rtn (apply orig-func orig-args)))
      ;; auto center `w3m' initial welcom buffer window
      (when (eq real-this-command 'w3m)
        (when (eq (entropy/emacs-wc-window-auto-center-mode-turn-on-judger
                   (current-buffer))
                  t)
          (entropy/emacs-window-center-mode)))
      rtn))
  (advice-add 'w3m-popup-buffer
              :around
              #'__ya/w3m-popup-buffer)

;; ****** Loading message center method tweak
  ;; EEMACS_MAINTENANCE: follow upstream
  (defun __ya/w3m-display-progress-message (url)
    "Like `w3m-display-progress-message' but respect
`entropy/emacs-window-center-mode' and enhance the message
alignment."
    (let* ((ecwidth (entropy/emacs-window-center-emulate-window-column-width-as-enabled
                     t))
           (wwidth (window-width))
           ;; ----- prompts string-----
           (info-str
            (substitute-command-keys
             (concat
              "\
Reading " url " ...\n\n"
              "\
Reading " (w3m-url-readable-string (w3m-url-strip-authinfo url)) " ...\n\n"
              "\
`\\<w3m-mode-map>\\[w3m-process-stop]' to abort this operation, or\n"
              "\
`\\<w3m-mode-map>\\[w3m-search-new-session]' to perform a search in a new buffer, or\n"
              "\
`\\<w3m-mode-map>\\[w3m-goto-url-new-session]' to visit a URL in a new buffer, or\n"
              "\
do any emacs work in any other buffer, or just wait ... ")))
           ;; -------------------------
           (max-line-len (save-mark-and-excursion
                           (with-temp-buffer
                             (let ((inhibit-read-only t)
                                   cur-head-pt
                                   len-stack)
                               (insert info-str)
                               (goto-char (point-min))
                               (while (and (not (eobp)) t)
                                 (setq cur-head-pt (point))
                                 (end-of-line)
                                 (push (- (point) cur-head-pt) len-stack)
                                 (forward-line 1))
                               (apply 'max len-stack)))))
           (indent (make-string
                    (max 0 (/ (- (cond
                                  ;; set align width respect `entropy/emacs-window-center-mode'
                                  ((or (bound-and-true-p entropy/emacs-window-center-mode)
                                       (entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge))
                                   ecwidth)
                                  (t
                                   wwidth))
                                 max-line-len)
                              2))
                    ? ))
           msg)
      (setq msg
            (save-mark-and-excursion
              (let ((inhibit-read-only t))
              (with-temp-buffer
                (insert info-str)
                (goto-char (point-min))
                (while (and (not (eobp)) t)
                  (unless (looking-at "^$")
                    (insert indent))
                  (forward-line 1))
                (buffer-substring (point-min) (point-max))))))
      ;; align window vertically
      (insert-char ?\n (max 0 (- (/ (window-height) 2) 3)))
      (insert msg)
      ;; NOTE: w3m internal required to do this
      (put-text-property (point-min) (point-max) 'w3m-progress-message t)
      ;; ensure display
      (sit-for 0)))

  (advice-add 'w3m-display-progress-message
              :override
              #'__ya/w3m-display-progress-message)

;; ***** eemacs spec command
;; ****** union
  (defvar entropy/emacs-textwww--w3m-inhibit-log-url nil
    "When non-nil indicate that do not log current canonical
`w3m-current-url' to `entropy/emacs-textwww--w3m-url-history'. It
should always be `let' binding to use."
    )
  (defvar-local entropy/emacs-textwww--w3m-url-history nil
    "The canonical url `w3m-current-url' (FIXME: does it always the
return of `w3m-canonicalize-url'? if not we should update
`entropy/emacs-textwww--w3m-log-current-url') history log for
current w3m buffer.

Each element of of this log list is a cons which car is the url
and the cdr is the point which is the last visit for page
content."
    )

  (defun entropy/emacs-textwww--w3m-log-current-url
      (orig-func &rest orig-args)
    (unless entropy/emacs-textwww--w3m-inhibit-log-url
      (let (_)
        (push (cons w3m-current-url
                    (car w3m-current-position))
              entropy/emacs-textwww--w3m-url-history)))
    (apply orig-func orig-args))

  (advice-add 'w3m-goto-url
              :around
              #'entropy/emacs-textwww--w3m-log-current-url)

;; ****** previous page restore
  (defun entropy/emacs-textwww-w3m-view-previous-page ()
    "Like `w3m-view-previous-page' but use the eemacs spec history
revision mechanism since `w3m-view-previous-page' has bug (or
mistake) for restore the previous in 'about:.*' buffer i.e `w3m'
internally did not log the 'about:.*' url as history so that it
rotate the `w3m-history' to the previous page of the page we want
to restore."
    (declare (interactive-only t))
    (interactive nil w3m-mode)
    (let* ((hist (pop entropy/emacs-textwww--w3m-url-history))
           (url (car hist))
           (pos (cdr hist)))
      (if (and (stringp url)
               (w3m-url-valid url))
          (let ((entropy/emacs-textwww--w3m-inhibit-log-url t)
                ;; do not create new history
                (w3m-history-reuse-history-elements t)
                ;; check the cache available
                (cache-available (not
                                  (eq (gethash url w3m-cache-hashtb 'void)
                                      'void)))
                ;; FIXME:
                ;; use cached page resources instead of re-request
                ;; even if the content is not cached predicated by
                ;; `w3m-cache-available-p' since the content is cached
                ;; even if it show as non-cached?
                (w3m-prefer-cache t))
            (w3m-goto-url url)
            (when (and cache-available
                       (integer-or-marker-p pos))
              (goto-char pos)
              (recenter)))
        ;; fallback to use `w3m-view-previous-page'.
        (w3m-view-previous-page))))

;; ****** make new session

  (defun entropy/emacs-w3m-make-new-session ()
    "Make a new `w3m' session."
    (interactive)
    (let (_)
      (w3m nil t)))

  (defun entropy/emacs-w3m-view-bookmark-in-new-session ()
    "View w3m bookmark in a new `w3m' session."
    (interactive)
    (progn
      (entropy/emacs-w3m-make-new-session)
      (w3m-bookmark-view)))

;; ****** quit w3m session dwim

  (defun entropy/emacs-w3m-quit-window-dwim ()
    "eemacs specified w3m window quit command"
    (interactive nil w3m-mode)
    (let* ((win-list (window-list))
           (win-list-len (length win-list)))
      (cond
       ((> win-list-len 1)
        (let* ((item
                ;; FIXME: is w3m modified the buffer history order
                ;; i.e. always let the car of thus be an already w3m
                ;; buffer?
                (let (prev-buff-obj)
                  (catch :exit
                    (dolist (el (window-prev-buffers))
                      (let* ((this-buff (car el))
                             (this-buff-livep
                              (and (bufferp this-buff)
                                   (buffer-live-p this-buff))))
                        (when (and this-buff-livep
                                   (not (eq
                                         (buffer-local-value 'major-mode this-buff)
                                         'w3m-mode)))
                          (throw :exit el))))
                    nil)))
               (buff (car item))
               (pos (caddr item)))
          (if (and buff
                   (bufferp buff)
                   (buffer-live-p buff))
              (progn (switch-to-buffer buff)
                     (goto-char pos))
            (kill-buffer))))
       (t
        (w3m-close-window)))))
  (define-key w3m-mode-map (kbd "q") #'entropy/emacs-w3m-quit-window-dwim)

;; **** __end__
  )


;; *** eww config
(use-package eww
  :preface
;; **** get image url
  (entropy/emacs-lazy-load-simple shr
    (defun entropy/emacs-textwww-get-eww-url ()
      "Get image or point url at eww or it's derived modes."
      (interactive nil eww-mode)
      (let* (choice
             (url (or (and (setq choice "image-url") (get-text-property (point) 'image-url))
                      (and (setq choice "common-url") (get-text-property (point) 'shr-url)))))
        (if url
            (progn
              (kill-new url)
              (message "Copy %s: '%s' ." choice url)
              url)
          (error (format "Can not find %s here!" choice))))))
  :init
;; **** uniform

  (entropy/emacs-textwww--hydra-uniform
   :mode eww-mode
   :feature eww
   :mode-map eww-mode-map
   :toggle-image entropy/emacs-textwww--eww-toggle-show-image-whole-page
   :bookmark-library-view eww-list-bookmarks
   :bookmark-add eww-add-bookmark
   :open-history eww-list-histories
   :browse-with-external entropy/emacs-textwww-eww-open-url-external
   :current-page-url eww-copy-page-url
   :current-link-url entropy/emacs-textwww-get-eww-url
   :previous-page eww-back-url
   :next-page eww-next-url
   :search-query eww
   :search-engine entropy/emacs-textwww-eww-toggle-default-search-engine)

  ;; sicne `eww' invoke its `major-mode' after its buffer popuped, the
  ;; `entropy/emacs-window-center-mode' will be disabled after
  ;; that, so we must manullay reinjected into that.
  (add-hook 'eww-mode-hook
            #'(lambda (&rest _)
                (entropy/emacs-wc-window-auto-center-mode-diwm)))

;; **** eww search engine
  (if entropy/emacs-enable-eww-search-engine-customize
      (setq eww-search-prefix entropy/emacs-eww-search-engine))

;; **** disable eww image animation for reducing the performance lagging
  (setq-default shr-image-animate nil
                shr-inhibit-images t)
  :config
;; **** eww open url outside
  (defun entropy/emacs-textwww-eww-open-url-external ()
    "Open current eww web page on external browser.

Browser chosen based on variable
`browse-url-browser-function'. In entropy-emacs they can be:

1. w3m cli backend fronts to emacs-w3m
2. personal browser specifiction rely on `entropy/emacs-browse-url-function'.
3. default browser detecting as `browse-url-default-browser'.
"
    (interactive nil eww-mode)
    (unless (not (equal major-mode 'eww-mode))
      (let ((url (eww-copy-page-url))
            (browse-url-browser-function
             (let (rtn choices)
               (when (functionp entropy/emacs-browse-url-function)
                 (cl-pushnew `("personal" ,entropy/emacs-browse-url-function)
                             choices))
               (when (executable-find "w3m")
                 (cl-pushnew `("w3m" ,(lambda (url &rest _)
                                        (w3m-goto-url url)))
                             choices))
               (cl-pushnew '("default" browse-url-default-browser) choices)
               (setq rtn (completing-read "Choice external browser:  "
                                          choices nil t))
               (setq rtn (nth 1 (assoc rtn choices)))
               rtn)))
        (browse-url url))))

;; **** eww toggle image display

  (defun entropy/emacs-textwww--eww-toggle-show-image-whole-page ()
    "Toggle whether display inline images in current eww buffer
in whole page."
    (interactive nil eww-mode)
    (let ()
      (when (eq major-mode 'eww-mode)
        (setq-local shr-inhibit-images (null shr-inhibit-images))
        (eww-reload))))

  ;; EEMACS_MAINTENANCE: Find way to toggle single inline image
  ;; displayable at point within a eww buffer like what did in
  ;; `w3m-mode'.

;; **** toggle search engine

  (defun entropy/emacs-textwww-eww-toggle-default-search-engine ()
    (interactive)
    (let ((choice
           (replace-regexp-in-string
            "%s.*$" ""
            (car
             (alist-get
              (completing-read "Choose search engine: "
                               entropy/emacs-search-web-engines-internal
                               nil t)
              entropy/emacs-search-web-engines-internal
              nil nil 'string=)))))
      (setq eww-search-prefix choice)))

  )

;; ** caller
;; *** search-web

;; Post web search queries using `browse-url'.
(use-package search-web
  :ensure nil
  :commands (search-web
             search-web-region
             entropy/emacs-textwww-search-web-region-toggle
             entropy/emacs-textwww-search-web-toggle)
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-after-startup-hook) :adtype hook :pdumper-no-end t))))
   ("WWW"
    (("C-c w" entropy/emacs-textwww-search-web-toggle
      "Search For Web With Hint"
      :enable t
      :global-bind t
      :exit t)
     ("C-c W" entropy/emacs-textwww-search-web-region-toggle
      "Search For Web With Region"
      :enable t
      :global-bind t
      :exit t))))

  :config

;; **** default config

  ;; dynamic declare
  (defvar w3m-make-new-session)
  ;; redefine search-web for compat with entropy-emacs
  (defun __ya/search-web (engine word)
    "Like `search-web' but patch for:

- the original one can't recovering default browser function,
fixing it as thus.

- let `w3m' always make new session to open the search result."
    (interactive (list
                  (search-web-query-engine)
                  (read-string "Search Word: " nil 'search-web-word-history)))
    (let* ((pattern (assoc engine search-web-engines 'string=))
           (url (cadr pattern))
           (render-get (caddr pattern))
           (render
            (cl-case render-get
              (nil search-web-default-browser)
              (In-Emacs search-web-in-emacs-browser)
              (External search-web-external-browser)
              (t 'browse-url-default-browser)))
           (orig-render browse-url-browser-function))
      (unwind-protect
          (progn
            (cond ((eql (car current-prefix-arg) 16)
                   (let ((brs (completing-read
                               "Choice browser:"
                               '("eww" "w3m")
                               nil t)))
                     (setq browse-url-browser-function
                           (cond
                            ((string= brs "eww")
                             'eww-browse-url)
                            ((string= brs "w3m")
                             'w3m-goto-url-new-session)
                            ))))
                  (t
                   (setq browse-url-browser-function render)))
            (let ((w3m-make-new-session t))
              (browse-url (format url (url-hexify-string word)))))
        (setq-default browse-url-browser-function orig-render))))
  (advice-add 'search-web :override #'__ya/search-web)

  ;; redefine search query engine for force input comprehensive data
  (defun entropy/emacs-textwww--search-web-query-egine (type)
    (let* ((prompt "Search Engine: "))
      (completing-read prompt search-web-engines nil t
                       (if (string= "External" type)
                           (let ((result nil))
                             (dolist (el entropy/emacs-search-web-engines-external)
                               (if (string= "google" (car el))
                                   (setq result t)))
                             (if result
                                 "google"
                               nil))
                         nil))))

  ;; Optional choosing internal or external browser to follow the searching.

  (defvar entropy/emacs-textwww--search-web-default-engine-list nil)
  (defun entropy/emacs-textwww--search-web-toggle-core (&optional region)
    (let ((type (or (and
                     ;; if prefix of 4, will reset the engine list
                     (not (eql (car current-prefix-arg) 4))
                     entropy/emacs-textwww--search-web-default-engine-list)
                    (setq entropy/emacs-textwww--search-web-default-engine-list
                          (completing-read "Internal or External: "
                                           '("Internal"
                                             "External")
                                           nil t)))))
      (let* ((search-web-engines (cond
                                  ((equal type "Internal")
                                   entropy/emacs-search-web-engines-internal)
                                  ((equal type "External")
                                   entropy/emacs-search-web-engines-external)))
             (engine (entropy/emacs-textwww--search-web-query-egine type))
             (word (if region
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (read-string
                      (format "Searching for?[%s]: "
                              ;; indicator browser use
                              (if (not (eql (car current-prefix-arg) 16))
                                  entropy/emacs-textwww--search-web-default-engine-list
                                "with choose extra browser"))))))
        (search-web engine word))))

  (defun entropy/emacs-textwww-search-web-toggle (&optional _prefix)
    "Trigger `search-web'.

Prefix meaning:

- When single prefix, forcely query for which engine list to use,
  default to use
  `entropy/emacs-textwww--search-web-default-engine-list'.

- When double prefix, query for external browser to use."
    (interactive "P")
    (entropy/emacs-textwww--search-web-toggle-core))

  (defun entropy/emacs-textwww-search-web-region-toggle (&optional _prefix)
    "Like `entropy/emacs-textwww-search-web-toggle' but trigger
`search-web-region' instead."
    (interactive "P")
    (entropy/emacs-textwww--search-web-toggle-core t))

  (setq search-web-in-emacs-browser 'eww-browse-url)

  (defun entropy/emacs-textwww--search-web--around (orig-func &rest orig-args)
    "Partially cancel `entropy/emacs-web-development-environment' if
    it's actived."
    (let* ((entropy/emacs-web-development-environment nil))
      (apply orig-func orig-args)))

  (advice-add 'entropy/emacs-textwww-search-web-toggle
              :around #'entropy/emacs-textwww--search-web--around)
  (advice-add 'entropy/emacs-textwww-search-web-region-toggle
              :around #'entropy/emacs-textwww--search-web--around))

;; *** toggle the default browse in emacs
(defun entropy/emacs-textwww--setting-default-browser (browser)
  "Setting the default browser for `search-web' and all the
url-open about function."

  (setq-default search-web-default-browser browser)
  (setq-default search-web-external-browser browser)
  (setq-default search-web-in-emacs-browser 'eww-browse-url)
  (setq-default browse-url-browser-function browser)
  (setq browse-url-secondary-browser-function browser)
  )

(defun entropy/emacs-textwww-toggle-default-browser ()
  "Toggle browse-url defualt browse function for all url-open
about function.

Default option were:
- eww: `eww-browse-url'
- default: `browse-url-default-browser'

If `entropy/emacs-enable-personal-browse-url-function' was 't' and `entropy/emacs-browse-url-function' was
effective then adding option of personal browse url function that be in ordered by
`entropy/emacs-browse-url-function'
"
  (interactive)
  (let* ((list-of-choice
          (cond ((and entropy/emacs-enable-personal-browse-url-function
                      entropy/emacs-browse-url-function
                      (not (executable-find "w3m")))
                 '("personal"
                   "eww"
                   "default"))
                ((and entropy/emacs-enable-personal-browse-url-function
                      entropy/emacs-browse-url-function
                      (executable-find "w3m"))
                 '("personal"
                   "w3m"
                   "eww"
                   "default"))
                ((executable-find "w3m")
                 '("w3m"
                   "eww"
                   "default"))
                (t '("eww" "default"))))
         (choice
          (completing-read "Choose the function you want: " list-of-choice)))
        (cond
         ((string= choice "eww")
          (entropy/emacs-textwww--setting-default-browser 'eww-browse-url))
         ((string= choice "default")
          (entropy/emacs-textwww--setting-default-browser 'browse-url-default-browser))
         ((string= choice "personal")
          (entropy/emacs-textwww--setting-default-browser entropy/emacs-browse-url-function))
         ((string= choice "w3m")
          (entropy/emacs-textwww--setting-default-browser 'entropy/emacs-textwww--w3m-browse-url))
         (t
          (error "Please choose the correct choice!")))))


;; init setting

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "default-browser-toggle-hydra-hollow-init"
 "default-browser-toggle-hydra-hollow-init" prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("WWW"
    (("C-c M-w" entropy/emacs-textwww-toggle-default-browser
      "Toggle default browser."
      :enable t
      :eemacs-top-bind t
      :exit t)))))

;; advantage of using w3m as default browser
(when (and entropy/emacs-enable-personal-browse-url-function
           (functionp entropy/emacs-browse-url-function))
  (if (not (executable-find "w3m"))
      (entropy/emacs-textwww--setting-default-browser entropy/emacs-browse-url-function)
    (if sys/is-graphic-support
        (entropy/emacs-textwww--setting-default-browser
         entropy/emacs-browse-url-function)
      (entropy/emacs-textwww--setting-default-browser
       'entropy/emacs-textwww--w3m-browse-url))))


;; * provide
(provide 'entropy-emacs-textwww)
