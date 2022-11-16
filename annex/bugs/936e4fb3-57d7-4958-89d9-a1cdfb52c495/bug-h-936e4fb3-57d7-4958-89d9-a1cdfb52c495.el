;; Bug reproduce emacs init.el
;; USAGE: emacs -Q -l _path_of_this_file
;;
;; Please hover the mounse on the symbol `scanf' or `main' on test.c
;; buffer thens scroll the lsp-ui-doc frame window by mounse-wheel,
;; then the bug will be occurred with freezing session, and beeping
;; sequentially while emacs bell ring is not null.
;;
;; If the bug not occurred try enter <f11> let emacs judging into
;; fullscreen and redo above section, this may occurred as well.
;;

(setq inhibit-startup-screen t)

(require 'newsticker)

(setq newsticker-url-list-defaults nil
      newsticker-url-list
      '(
        ;; global emacs forums and blogs
        ("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
        ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")
        ("Emacs Stackexchange" "https://emacs.stackexchange.com/feeds")
        ("EmacsTalk.github.io" "https://emacstalk.github.io/podcast/index.xml")
        ;; chinese emacs forums and blogs
        ("EmacsChina Posts" "https://emacs-china.org/posts.rss")
        ("EmacsChina latest" "https://emacs-china.org/latest.rss")
        ("Manateelazycat Blog" "https://manateelazycat.github.io/feed.xml")
        ))

(newsticker-show-news)

(sit-for 1)

(setq frame-title-format "Waiting for feeds getted ...")
(run-with-idle-timer
 10 nil
 (lambda nil
   (setq frame-title-format "See bug in minibuffer")
   (with-current-buffer "*Newsticker Tree*"
     (search-forward "All items" nil t)
     (funcall-interactively 'newsticker-treeview-tree-do-click))))
