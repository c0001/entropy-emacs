;;; entropy-emacs-rss.el --- Emacs Rss configuration
;;
;; * Copyright (C) 20190907  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-rss.el
;; Keywords:      rss, elfeed
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
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
;; Emacs rss client configuration for =entropy-emacs=.
;;
;; * Configuration:
;;
;; Using for =entropy-emacs= only.
;;
;; * Code:

;; ** require

;; ** elfeed feed
(use-package elfeed
  :commands (elfeed)
  :bind (:map
         elfeed-show-mode-map
         ("q" . entropy/emacs-rss-elfeed-kill-buffer))

;; *** hydra hollow
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-hydra-hollow-call-before-hook)
                                      :adtype hook :pdumper-no-end t))))
   ("Rss"
    (("r e" elfeed "Read Rss By Elfeed"
      :enable t
      :exit t))))

  :eemacs-mmphc
  (((:enable t :defer t)
    (elfeed-search-mode (elfeed elfeed-search-mode-map) t))
   ("Feed"
    (("A" elfeed-add-feed "Add feed" :enable t :exit t :map-inject t)
     ("D" entropy/emacs-rss-elfeed-remove-feed "Delete feed commonly" :enable t :exit t :map-inject t)
     ("M-d" entropy/emacs-rss-elfeed-remove-feed-by-regexp "Delete feed by regexp"
      :enable t :exit t :map-inject t))
    "Filter"
    (("s" entropy/emacs-rss-elfeed-clean-filter "Clean current patched filter"
      :enable t :exit t :map-inject t)
     ("t" entropy/emacs-rss-elfeed-filter-by-tag "Filter feeds by tag"
      :enable t :exit t :map-inject t)
     ("f" entropy/emacs-rss-elfeed-filter-by-feedname "Filter feeds by name"
      :enable t :exit t :map-inject t)
     ("m" elfeed-search-set-filter "Filter feeds by hand" :enable t :exit t :map-inject t))
    "Update"
    (("u" entropy/emacs-rss-elfeed-multi-update-feeds "Update multiple feeds"
      :enable t :exit t :map-inject t)
     ("U" entropy/emacs-rss-elfeed-update "Update all feeds"
      :enable t :exit t :map-inject t)
     ("g" entropy/emacs-rss-elfeed-update-search-list-inct "Refresh elfeed status"
      :enable t :exit t :map-inject t)
     ("G" (entropy/emacs-rss-elfeed-update-search-list-inct t) "Refresh elfeed status strongly"
      :enable t :exit t :map-inject t))
    "Entry"
    (("d" entropy/emacs-rss-elfeed-delete-entry "Delete current entry"
      :enable t :exit t :map-inject nil)
     ("+" entropy/emacs-rss-elfeed-tag-selected "Add tag for current entry"
      :enable t :exit t :map-inject t)
     ("-" entropy/emacs-rss-elfeed-untag-selected "Delete tag for current entry"
      :enable t :exit t :map-inject t))))

;; *** init
  :init
  ;; show all feeds retrieve heath informations
  (setq elfeed-log-level 'debug)

  ;; Restrict curl connections for performance thoughts
  (setq elfeed-curl-max-connections 5)

  ;; shorten timeout for impatient
  (setq elfeed-curl-timeout 5)

  ;; center window for more comfortable visualization
  (with-eval-after-load 'elfeed
    (defun entropy/emacs-elfeed-show-log-buffer ()
      "Display `elfeed-log-bfufer'."
      (interactive)
      (pop-to-buffer (elfeed-log-buffer))))

  ;; Nice time format string
  (setq elfeed-search-date-format '("%Y/%m/%d-%H:%M" 16 :left))

  ;; auto center window mode
  (dolist (cmd '(
                 ;; We don't patch for elfeed since elfeed change the
                 ;; major-mode after popup the buffer which killed all
                 ;; local settings. We advice for `elfeed' directly
                 ;; for thus.
                 ;;
                 ;; elfeed

                 elfeed-search-show-entry))
    (add-to-list 'entropy/emacs-window-auto-center-commands-list
                 cmd))

  ;; set curl path
  (let ((mingw-curl (if (and entropy/emacs-win-portable-mingw-enable
                             (file-exists-p entropy/emacs-win-portable-mingw-bin-path))
                        (expand-file-name
                         "curl.exe"
                         entropy/emacs-win-portable-mingw-bin-path)
                      nil))
        (msys2-curl (if (and entropy/emacs-microsoft-windows-unix-emulator-enable
                             (file-exists-p entropy/emacs-microsoft-windows-unix-emulator-bin-path))
                        (expand-file-name
                         "curl.exe" entropy/emacs-microsoft-windows-unix-emulator-bin-path)))
        (w32-curl "c:/WINDOWS/system32/curl.exe")
        (unix-curl "curl"))
    (cond
     ((ignore-errors (file-exists-p mingw-curl))
      (setq elfeed-curl-program-name mingw-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p msys2-curl))
      (setq elfeed-curl-program-name msys2-curl)
      (setq elfeed-use-curl t))
     ((ignore-errors (and (executable-find "curl")
                          sys/is-posix-compatible))
      (setq elfeed-use-curl t))
     ((ignore-errors (file-exists-p w32-curl))
      (setq elfeed-curl-program-name w32-curl)
      (setq elfeed-use-curl t))
     (t
      (setq elfeed-use-curl nil))))


  :config
;; *** core advice

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/elfeed (orig-func &rest orig-args)
    "Like `elfeed' but adapt for eemacs specifications."
    (prog1
        (apply orig-func orig-args)
      (entropy/emacs-wc-center-window-auto-mode-for-current-selected-window)
      ;; truncate long lines in this summary buffer since some feeds
      ;; title so long.
      (setq truncate-lines t)))
  (advice-add 'elfeed :around #'__ya/elfeed)

  (defun entropy/emacs-rss--elfeed-url-hexify (url)
    (require 'entropy-common-library-const)
    (let ((hexi-url (url-hexify-string url entropy/cl-url--allowed-chars)))
      hexi-url))

  (defun entropy/emacs-rss--elfeed-search-update ()
    (elfeed-search-update--force))

  (defvar entropy/emamcs-rss--elfeed-save-custom t) ;default to `t' to suitable for subroutines patches
  (defvar entropy/emacs-rss--elfeed-use-proxy-p nil)
  (defvar entropy/emacs-rss--elfeed-gen-feeds-done nil
    "NOTE: Internal indicator for eemacs elfeed config initial, do
not modify it manually.")
  (defvar entropy/emacs-rss--elfeed-use-prroxy-feeds nil)
  (defvar entropy/emacs-rss--elfeed-non-prroxy-feeds nil)

  (defun entropy/emacs-elfeed-feeds--gen-feeds (&optional regen user-feeds-plists)
    "Generate `entropy/emacs-rss--elfeed-non-prroxy-feeds' and
`entropy/emacs-rss--elfeed-non-prroxy-feeds' from
USER-FEEDS-PLIST when `entropy/emacs-rss--elfeed-gen-feeds-done'
is nil unless REGEN is set. Retun a undefined plist.

When USER-FEEDS-PLISTS is not set, use
`entropy/emacs-elfeed-feeds' instead in which case the
`elfeed-feeds' will be synced as it.
"
    (when (or (not entropy/emacs-rss--elfeed-gen-feeds-done)
              regen
              user-feeds-plists)
      (let (user-common-feeds user-proxy-feeds new-fp-rtn)
        (unless user-feeds-plists
          (setq entropy/emacs-rss--elfeed-use-prroxy-feeds nil)
          (setq entropy/emacs-rss--elfeed-non-prroxy-feeds nil))
        (mapc
         (lambda (x)
           (let* ((url (if entropy/emacs-rss--elfeed-gen-feeds-done
                           (car x)
                         (entropy/emacs-rss--elfeed-url-hexify (car x))))
                  (attrs (cdr x))
                  (new-fp (unless entropy/emacs-rss--elfeed-gen-feeds-done
                            (cons url attrs)))
                  (proxy? (plist-get attrs :use-proxy)))
             (if proxy?
                 (push url (if user-feeds-plists
                               user-proxy-feeds
                             entropy/emacs-rss--elfeed-use-prroxy-feeds))
               (push url (if user-feeds-plists
                             user-common-feeds
                           entropy/emacs-rss--elfeed-non-prroxy-feeds)))
             (when new-fp
               (push new-fp new-fp-rtn))))
         (or user-feeds-plists entropy/emacs-elfeed-feeds))
        (unless (or entropy/emacs-rss--elfeed-gen-feeds-done
                    user-feeds-plists)
          (setq entropy/emacs-rss--elfeed-gen-feeds-done t))
        (unless user-feeds-plists
          (setq elfeed-feeds
                (append entropy/emacs-rss--elfeed-non-prroxy-feeds
                        entropy/emacs-rss--elfeed-use-prroxy-feeds)))
        (when new-fp-rtn
          (setq entropy/emacs-elfeed-feeds new-fp-rtn))
        (if user-feeds-plists
            (list :proxy-feeds user-proxy-feeds
                  :common-feeds user-common-feeds)
          (list :proxy-feeds entropy/emacs-rss--elfeed-use-prroxy-feeds
                :common-feeds entropy/emacs-rss--elfeed-non-prroxy-feeds)))))

  (defun entropy/emacs-elfeed-feeds-variable-watcher (symbol newval operation where)
    "`entropy/emacs-elfeed-feeds' vairable wather guard to auto sync
to `elfeed-feeds'."
    (when (eq operation 'set)
      (unless (eq newval (symbol-value symbol))
        (let ((tmpvar newval))
          (unless entropy/emacs-rss--elfeed-gen-feeds-done
            (setq tmpvar nil)
            (dolist (el newval)
              (push (cons (entropy/emacs-rss--elfeed-url-hexify (car el))
                          (cdr el))
                    tmpvar)))
          (setq entropy/emacs-elfeed-feeds tmpvar)
          (when entropy/emamcs-rss--elfeed-save-custom
            (customize-save-variable 'entropy/emacs-elfeed-feeds
                                     tmpvar)))
        (entropy/emacs-elfeed-feeds--gen-feeds t))))
  (add-variable-watcher 'entropy/emacs-elfeed-feeds
                        #'entropy/emacs-elfeed-feeds-variable-watcher)
  (entropy/emacs-elfeed-feeds--gen-feeds) ;init `elfeed-feeds'

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (cl-defun __ya/elfeed-add-feed-around (url &key save)
    "Like `elfeed-add-feed' but add to `entropy/emacs-elfeed-feeds'
instead."
    (interactive
     (list
      (let* ((feeds (elfeed-candidate-feeds))
             (prompt (if feeds (concat "URL (default " (car feeds)  "): ")
                       "URL: "))
             (input (read-from-minibuffer prompt nil nil nil nil feeds))
             (result (elfeed-cleanup input)))
        (cond ((not (zerop (length result))) result)
              (feeds (car feeds))
              ((user-error "No feed to add"))))
      :save t))
    (let* ((hexi-url (entropy/emacs-rss--elfeed-url-hexify url))
           (use-proxy-p (yes-or-no-p "With proxy?"))
           (new-feeds-plists (list (list url :use-proxy use-proxy-p)))
           (entropy/emamcs-rss--elfeed-save-custom save))
      (setq entropy/emacs-elfeed-feeds
            (append entropy/emacs-elfeed-feeds
                    new-feeds-plists))
      ;; Fetch the new feed
      (progn
        (elfeed-db-ensure)
        (entropy/emacs-rss--elfeed-fetch-feeds
         (entropy/emacs-rss--elfeed-arrange-feeds-plist
          `(,url))))
      ))
  (advice-add 'elfeed-add-feed :override
              #'__ya/elfeed-add-feed-around)


  (defun entropy/emacs-rss--elfeed-prun-feeds-plist (feeds &optional feeds-plist-name)
    "Destructively prune FEEDS-PLIST-NAME's value by FEEDS, and
return the pruned 'feeds-plist'.

Optional arg FEEDS-PLIST-NAME if nil, pruning
`entropy/emacs-elfeed-feeds' by default."
    (let (
          ;; disable `lexical-binding' since we need to use
          ;; `symbol-value' inside for whatever type of symbol given.
          (lexical-binding nil))
      (let* (tmpvar
             (feeds-plist-name (or feeds-plist-name
                                  'entropy/emacs-elfeed-feeds))
             (feeds-plist (symbol-value feeds-plist-name)))
        (dolist (el feeds)
          (when (setq tmpvar (assoc el feeds-plist 'string=))
            (setq feeds-plist (delete tmpvar feeds-plist))))
        (set feeds-plist-name feeds-plist)
        (symbol-value feeds-plist-name))))

  (defun entropy/emacs-rss--elfeed-arrange-feeds-plist (feeds)
    "Gen an ELFEED-FEEDS-RICH from feeds"
    (let ((user-common-feeds nil)
          (user-proxy-feeds nil))
      (dolist (el feeds)
        (if (plist-get (alist-get el entropy/emacs-elfeed-feeds nil nil 'string=)
                       :use-proxy)
            (push el user-proxy-feeds)
          (push el user-common-feeds)))
      (list :proxy-feeds user-proxy-feeds
            :common-feeds user-common-feeds)))

  (defun entropy/emacs-rss--elfeed-process-stop-all ()
    (let (_)
      (when elfeed-use-curl
        ;; do more times sincee elfeed recall `elfeed-curl--run-queue'
        ;; in the curl cbk.
        (dotimes (var 2)
          (sleep-for 0.2)
          (mapc (lambda (x) (when (string-match-p "elfeed-curl" x)
                              (delete-process x)))
                (mapcar #'process-name (process-list)))))
      (elfeed-unjam)))

  (defun entropy/emacs-rss--elfeed-process-running-p ()
    "Return non-nil when `elfeed' has remaining running process."
    (let ((elfeed-indicator
           (not
            ;; Bug of `elfeed-queue-count-active' &
            ;; `elfeed-queue-count-total' which may
            ;; cause less than zero
            (<=
             ;; NOTE FIXME: do not use `elfeed-queue-count-total' here
             ;; while not use `curl' since its may need long long time
             ;; to eliminate each retrieving why?
             (if (not elfeed-use-curl)
                 (elfeed-queue-count-active)
               (elfeed-queue-count-total))
             0)))
          (curl-procs
           (when elfeed-use-curl
             (delete
              nil
              (mapcar (lambda (x) (when (string-match-p "elfeed-curl" x)
                                    x))
                      (mapcar #'process-name (process-list)))))))
      (if curl-procs
          t
        elfeed-indicator)
      ))

  (defvar __elfeed-orig-curl-args nil)
  (defvar __elfeed-orig-url-proxies nil)
  (defun entropy/emacs-rss--elfeed-fetch-feeds (elfeed-feeds-rich)
    "Synchronously fetch ELFEED-FEEDS-RICH."
    (when (entropy/emacs-rss--elfeed-process-running-p)
      (user-error "Please wait for previous elfeed queue retrieve done!"))
    (unless (executable-find "curl")
      (user-error "Eemacs just support elfeed run in 'curl' mode. \
But we can not found curl in your PATH!"))
    (unless elfeed-use-curl
      (setq elfeed-use-curl t)
      (message "Eemacs just support elfeed run in 'curl' mode, but you customize not to use it. \
So we automatically help you to change to `curl' mode."))
    (let ((common-feeds (plist-get elfeed-feeds-rich :common-feeds))
          (proxy-feeds (plist-get elfeed-feeds-rich :proxy-feeds))
          (orig-filter elfeed-search-filter))
      ;; (elfeed-search-set-filter "")
      (cond ((and proxy-feeds
                  (plist-get entropy/emacs-union-http-proxy-plist :enable))
             ;; common ways
             (when common-feeds
               (message "Update non-proxy feeds ...")
               (setq entropy/emacs-rss--elfeed-use-proxy-p nil)
               (let ((elfeed-feeds common-feeds))
                 (elfeed-update))
               (let (success)
                 (unwind-protect
                     (progn
                       (while (entropy/emacs-rss--elfeed-process-running-p)
                         (sleep-for 0.1)
                         (message "Waiting for non-proxy feeds retrieve done ..."))
                       (setq success t)
                       (message "Update non-proxy feeds done"))
                   (setq entropy/emacs-rss--elfeed-use-proxy-p nil)
                   (unless success
                     (entropy/emacs-unwind-protect-body 10
                       (ignore-errors
                         (entropy/emacs-rss--elfeed-process-stop-all)))
                     (entropy/emacs-error-without-debug
                      "Abort fetching <non-proxy>!")))))
             ;; proxy ways
             (when proxy-feeds
               (message "Update proxy feeds ...")
               (setq entropy/emacs-rss--elfeed-use-proxy-p t)
               (setq __elfeed-orig-curl-args elfeed-curl-extra-arguments
                     __elfeed-orig-url-proxies url-proxy-services)
               (setq elfeed-curl-extra-arguments
                     `("-x" ,(format "http://%s:%s"
                                     (plist-get entropy/emacs-union-http-proxy-plist :host)
                                     (plist-get entropy/emacs-union-http-proxy-plist :port)))
                     url-proxy-services
                     (entropy/emacs-gen-eemacs-union-http-internet-proxy-url-proxy-services))
               (let ((elfeed-feeds proxy-feeds))
                 (elfeed-update))
               (let (success)
                 (unwind-protect
                     (progn
                       (while (entropy/emacs-rss--elfeed-process-running-p)
                         (sleep-for 0.1)
                         (message "Waiting for proxy feeds retrieve done ..."))
                       (setq success t)
                       (message "Update proxy feeds done"))
                   (setq elfeed-curl-extra-arguments __elfeed-orig-curl-args
                         url-proxy-services __elfeed-orig-url-proxies)
                   (unless success
                     (entropy/emacs-unwind-protect-body 10
                       (ignore-errors
                         (entropy/emacs-rss--elfeed-process-stop-all)))
                     (entropy/emacs-error-without-debug
                      "Abort fetching <proxy>!"))))))
            (t
             (setq entropy/emacs-rss--elfeed-use-proxy-p nil)
             (let ((elfeed-feeds common-feeds))
               (elfeed-update))))
      ;; redraw the search buffer when database changed
      (progn
        (entropy/emacs-rss--elfeed-search-update)
        (elfeed-search-set-filter orig-filter))
      (message "Update %s feeds done"
               (+ (length common-feeds)
                  (length proxy-feeds)))))

  (defun entropy/emacs-rss-elfeed-update ()
    "Like `elfeed-update' but specified for eemacs."
    (interactive)
    (let ((tmpvar (list :proxy-feeds entropy/emacs-rss--elfeed-use-prroxy-feeds
                        :common-feeds entropy/emacs-rss--elfeed-non-prroxy-feeds)))
      (entropy/emacs-rss--elfeed-fetch-feeds
       tmpvar)))

  ;; EEMACS_MAINTENANCE: follow upstream
  (defun __ya/elfeed-log (level fmt &rest objects)
    "Like `elfeed-log' but patched for eemacs specs."
    (let ((log-buffer (elfeed-log-buffer))
          (log-level-face (cl-case level
                            (debug 'elfeed-log-debug-level-face)
                            (info 'elfeed-log-info-level-face)
                            (warn 'elfeed-log-warn-level-face)
                            (error 'elfeed-log-error-level-face)))
          (inhibit-read-only t)
          (use-proxy entropy/emacs-rss--elfeed-use-proxy-p))
      (when (>= (elfeed-log--level-number level)
                (elfeed-log--level-number elfeed-log-level))
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert
           (format
            (concat "[" (propertize "%s" 'face 'elfeed-log-date-face) "] "
                    "[" (propertize "%s" 'face log-level-face) "]: %s %s\n")
            (format-time-string "%Y-%m-%d %H:%M:%S")
            level
            (if use-proxy "<use-proxy>" "<non-proxy>")
            (apply #'format fmt objects)))))))
  (advice-add 'elfeed-log :override #'__ya/elfeed-log)

;; *** utilities
  (defun entropy/emacs-rss--elfeed-list-feeds ()
    "List feeds using for querying prompt."
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          (rtn nil))
      (let ((mlist feeds))
        (dolist (el feeds)
          (if (not (member (elfeed-feed-id el) elfeed-feeds))
              (setq mlist (delete el mlist))))
        (setq feeds mlist))
      (let ((pair-list nil))
        (dolist (el feeds)
          (setq pair-list `(,(elfeed-feed-title el) . ,(elfeed-feed-id el)))
          (push pair-list rtn)))
      (let ((mlist nil))
        (dolist (el rtn)
          (if (car el)
              (push `(,(concat (car el) " ↭ " (cdr el)) . ,(cdr el)) mlist)
            (push `(,(concat "☹nil ↭ " (cdr el)) . ,(cdr el)) mlist)))
        (setq rtn mlist))
      rtn))

  (defun entropy/emacs-rss-elfeed-clean-invalid-feeds ()
    "Clean invalid feeds which can not be retrieved.

This function will remove 'as entry' both in `elfeed-db' and
`elfeed-feeds' and `entropy/emacs-elfeed-feeds' which is also
will be automatically modified in `custom-file'."
    (interactive)
    (let ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
          mlist olist)
      (dolist (el feeds)
        (when (and (not (elfeed-feed-url el))
                   (member (elfeed-feed-id el) elfeed-feeds))
          (push `(,(elfeed-feed-id el) . ,el) mlist)))
      (dolist (el mlist)
        (setq elfeed-feeds (delete (car el) elfeed-feeds))
        (push (car el) olist))
      (customize-save-variable 'entropy/emacs-elfeed-feeds
                               (entropy/emacs-rss--elfeed-prun-feeds-plist
                                olist))
      (elfeed-db-gc-empty-feeds)))

  (defvar entropy/emacs-rss--elfeed-feed-prompt-alist '()
    "Alist of feeds prompt string and url.")


  (defun entropy/emacs-rss-elfeed-kill-buffer ()
    "Automatically swtitch to elfeed-search buffer when closed
the current elfeed-show-buffer."
    (interactive)
    (if (not (equal major-mode 'elfeed-show-mode))
        (kill-buffer (current-buffer))
      (progn
        (kill-buffer (current-buffer))
        (let* ((bfl (mapcar #'(lambda (x) (buffer-name x))
                            (buffer-list))))
          (if (member "*elfeed-search*" bfl)
              (progn (switch-to-buffer "*elfeed-search*" nil t)
                     (message "Back to *elfeed-search* buffer."))
            (user-error "Couldn't found *elfeed-search* buffer."))))))


  (defun entropy/emacs-rss-elfeed-clean-filter ()
    "Clean all filter for curren elfeed search buffer."
    (interactive)
    (elfeed-search-set-filter ""))

  (defun entropy/emacs-rss--elfeed-build-feedname-regexp-filter (feedname)
    (replace-regexp-in-string " " ".*" feedname))

  (defun entropy/emacs-rss--elfeed-build-tag-valid-name (tag)
    (let* ((regex " ")
           (tag-name (symbol-name tag))
           rtn)
      (if (string-match-p regex tag-name)
          (progn
            ;; NOTE: do not `unintern' the tag symbol, since it's
            ;; maybe the internal API symbol name
            ;;
            ;; (unintern tag)
            (setq rtn
                  (intern
                   (replace-regexp-in-string
                    " " "_" (symbol-name tag)))))
        (setq rtn tag))
      rtn))

  ;; EEMACS_MAINTENANCE: follow upstream updates
  (defun __ya/elfeed-tag (orig-func &rest orig-args)
    "Like `elfeed-tag' but convert invalid tag name to valid i.e. non
space separator within it before adding to `elfeed-db'."
    (let* ((tags (cdr orig-args))
           (newtags (mapcar (lambda (x)
                              (entropy/emacs-rss--elfeed-build-tag-valid-name
                               x))
                            tags)))
      (apply orig-func (car orig-args) newtags)))
  (advice-add 'elfeed-tag :around #'__ya/elfeed-tag)

  (defun entropy/emacs-rss--elfeed-get-all-entries ()
    "Get all entries from `elfeed-db' and return it."
    (hash-table-values (plist-get elfeed-db :entries)))

  (defun entropy/emacs-rss--elfeed-read-feedname (entry)
    "Get entry's feed name."
    (let* ((feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed)))))
      (if feed-title
          feed-title)))

;; *** udpate search buffer

  (defun entropy/emacs-rss-elfeed-update-search-list-inct (&optional prefix)
    "Interactively format feedtitle which has space.

Optional arg PREFIX when non-nil will call `elfeed-unjam', since
some times the elfeed has messy with procedure like
`header-line-format' indicator be stuck when `elfeed' spawns
sentinels fatal etc. and do it no need care for as."
    (interactive "P")
    (if (equal major-mode 'elfeed-search-mode)
        (entropy/emacs-rss--elfeed-search-update)
      (error "You are not in the 'elfeed-search-mode'!"))
    (when prefix
      (elfeed-unjam)))

;; *** query prompting filter function

  (defun entropy/emacs-rss--elfeed-tags-choice
      (selected-entries &optional tag return-list prompt match)
    "Query and selected tag from the SELECTED-ENTRIES. And final
return the tag symbol or the list of matched entires by this tag.

Arguemnts:

- tag:         if specific it as a string, skip the step of query prompt,
               and using the value as the choice.

- return-list: if non-nil ,then this function return the matched
               entries list, otherwise return tag string

- prompt:      string for specific the query prompt.

- match:       whether require match with query prompt step."
    (if (equal major-mode 'elfeed-search-mode)
        (let (rtn entries tags-list choice)
          ;; extract entries by selected which have existed tags and make initial tags-list list.
          ;; [2018-08-30 Thu 02:32:49] this can be replaced with `elfeed-db-get-all-tags' for full
          ;; tags search.

          (let ((entl selected-entries))
            (dolist (el entl)
              (when (listp (elfeed-entry-tags el))
                (push el entries)
                (mapcar #'(lambda (x)
                            (add-to-list 'tags-list x))
                        (elfeed-entry-tags el)))))
          ;; read user choice
          (if (not tag)
              (setq choice (completing-read
                            (if (not prompt)
                                "Choose tag: "
                              prompt)
                            tags-list nil
                            (if match t nil)))
            (setq choice tag))

          (if return-list
              ;; match entries of choice
              (dolist (el entries)
                (if (member (intern choice) (elfeed-entry-tags el))
                    (push el rtn)))
            (setq rtn (intern choice)))
          rtn)
      (error "Unmatched major-mode of 'elfeed-search-mode'!")))

  (defun entropy/emacs-rss--elfeed-feedname-choice (selected-entries &optional tname return-list)
    "Query with promt for choosing one feedname from the selected
entries, return the choice or one entries list wich matched this
choice.

Arguments:

- tname:       specific feedname without manully choosing one from query prompting.

- return-list: if non-nil then return the matched entries list
               instead of returning the feedname."
    (let ((entry-list selected-entries)
          feedtitle-name-list
          entries
          choice
          rtn)
      ;; make feed title name list and matched entries list
      (dolist (el entry-list)
        (let ((feed-title (entropy/emacs-rss--elfeed-read-feedname el)))
          (when feed-title
            (add-to-list 'feedtitle-name-list feed-title)
            (push el entries))))

      (if tname
          (setq choice tname)
        (setq choice (completing-read
                      (if return-list "Choose feed title: "
                        "Choose feed title or input: ")
                      feedtitle-name-list
                      nil t)))
      (unless return-list
        ;; return the feedname as an filter
        (setq choice
              (entropy/emacs-rss--elfeed-build-feedname-regexp-filter
               choice)))

      (if return-list
          (dolist (el entries)
            (if (equal choice (entropy/emacs-rss--elfeed-read-feedname el))
                (push el rtn)))
        (setq rtn choice))
      rtn))

  (defun entropy/emacs-rss-elfeed-filter-by-tag (tag)
    "Filter with tag choosen, powered by `entropy/emacs-rss--elfeed-tags-choice'"
    (interactive
     (list (entropy/emacs-rss--elfeed-tags-choice
            (entropy/emacs-rss--elfeed-get-all-entries)
            nil nil nil t)))
    (elfeed-search-set-filter (concat "+" (symbol-name tag))))

  (defun entropy/emacs-rss-elfeed-filter-by-feedname (feedname)
    "Filter with feedname, powered by `entropy/emacs-rss--elfeed-feedname-choice'."
    (interactive
     (list (entropy/emacs-rss--elfeed-feedname-choice
            (entropy/emacs-rss--elfeed-get-all-entries))))
    (elfeed-search-set-filter (concat "=" feedname)))

  (defun entropy/emacs-rss-elfeed-untag-selected ()
    "Untag tag for selected entries with query prompt as
requiring matched."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (choice (entropy/emacs-rss--elfeed-tags-choice
                    entries nil nil "Choose tag for remove: " t)))
      (elfeed-untag entries choice)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun entropy/emacs-rss-elfeed-tag-selected ()
    "Adding tag for selected entries with query prompt for
selecting existing tag or input the new one instead."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (full-entrylist (entropy/emacs-rss--elfeed-get-all-entries))
           (tag (entropy/emacs-rss--elfeed-tags-choice
                 full-entrylist nil nil "Choose tag or input one: ")))
      (elfeed-tag entries tag)
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))


;; *** delete entry
  (defun entropy/emacs-rss-elfeed-delete-entry ()
    "Delete entry of elfeed."
    (interactive)
    (if (not (use-region-p))
        (let* ((entry (elfeed-search-selected t))
               (id  (elfeed-entry-id entry))
               (n-entry elfeed-db-entries))
          (avl-tree-delete elfeed-db-index id)
          (remhash id n-entry)
          (setq elfeed-db-entries n-entry)
          (entropy/emacs-rss--elfeed-search-update))
      (let* ((entries (elfeed-search-selected))
             id
             (n-entry elfeed-db-entries))
        (dolist (el entries)
          (add-to-list 'id (elfeed-entry-id el)))
        (dolist (el id)
          (avl-tree-delete elfeed-db-index el))
        (dolist (el id)
          (remhash el n-entry))
        (setq elfeed-db-entries n-entry)
        (entropy/emacs-rss--elfeed-search-update))))

;; *** remove feed function
  (defvar entropy/emacs-rss--elfeed-feed-remove-list '()
    "List stored feeds url of `elfeed-feeds' to remove.")

  (defun entropy/emacs-rss--elfeed-get-feed-title-name-by-feed-url (url)
    "Matching url's refer feed title name."
    (let* ((feeds (hash-table-values (plist-get elfeed-db :feeds)))
           (rtn nil))
      (dolist (el feeds)
        (if (equal url (elfeed-feed-url el))
            (setq rtn (elfeed-feed-title el))))
      (if rtn
          rtn
        "-v-")))

  (defun entropy/emacs-rss--elfeed-remove-read-action (x)
    "Repeatedly read action for removing feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (let ((temp (cdr x)))
      (setq x temp))
    (entropy/cl-ivy-read-repeatedly-function
     x 'entropy/emacs-rss--elfeed-feed-remove-list
     "Removing:"
     #'entropy/emacs-rss--elfeed-get-feed-title-name-by-feed-url))

  (defun entropy/emacs-rss-elfeed-remove-feed ()
    "Remove elfeed feeds with multi-chosen by query prompted
function of `ivy-read'.

This function will remove 'as entry' both in `elfeed-db' and
`elfeed-feeds' and `entropy/emacs-elfeed-feeds' which is also
will automatically be modified in `custom-file'."
    (interactive)
    (setq entropy/emacs-rss--elfeed-feed-remove-list nil
          entropy/emacs-rss--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-rss--elfeed-feed-prompt-alist (entropy/emacs-rss--elfeed-list-feeds))
    (ivy-read "Remove feeds:" entropy/emacs-rss--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-rss--elfeed-remove-read-action)
    (let ((rtn elfeed-feeds))
      (dolist (el entropy/emacs-rss--elfeed-feed-remove-list)
        (when (member el elfeed-feeds)
          (setq rtn (delete el rtn))))
      (setq elfeed-feeds rtn)
      (customize-save-variable 'entropy/emacs-elfeed-feeds
                               (entropy/emacs-rss--elfeed-prun-feeds-plist
                                entropy/emacs-rss--elfeed-feed-remove-list))
      (when (yes-or-no-p "Do you want to remove all empty feeds? ")
        (elfeed-db-gc-empty-feeds))))

  (defun entropy/emacs-rss-elfeed-remove-feed-by-regexp ()
    "Remove feeds matched of regexp expr inputted repeatlly.

This function will remove 'as entry' both in `elfeed-db' and
`elfeed-feeds' and `entropy/emacs-elfeed-feeds' which is also
will automatically be modified in `custom-file'."
    (interactive)
    (let* ((regexp (entropy/cl-repeated-read "Input regexp"))
           (feeds (elfeed-feed-list))
           mlist
           (rtn feeds))
      (dolist (el regexp)
        (dolist (elm feeds)
          (when (string-match-p el elm)
            (add-to-list 'mlist elm))))
      (dolist (el mlist)
        (setq rtn (delete el rtn)))
      (setq elfeed-feeds rtn)
      (customize-save-variable 'entropy/emacs-elfeed-feeds
                               (entropy/emacs-rss--elfeed-prun-feeds-plist
                                mlist))))

;; *** update feed function

  (defvar entropy/emacs-rss--elfeed-multi-update-feeds-list '()
    "Elfeed Feeds for update.")

  (defun entropy/emacs-rss--elfeed-update-read-action (x)
    "Repeatly read action for updating feeds of `elfeed-feeds',
powered by `entropy/cl-ivy-read-repeatedly-function'."
    (require 'entropy-common-library)
    (cond ((stringp x)
           (user-error "Please choose one  matched canidate!"))
          ((consp x)
           (setq x (cdr x))
           (entropy/cl-ivy-read-repeatedly-function
            x 'entropy/emacs-rss--elfeed-multi-update-feeds-list
            "Updating: "
            #'entropy/emacs-rss--elfeed-get-feed-title-name-by-feed-url))))

  (defun entropy/emacs-rss--elfeed-get-multi-update-feeds ()
    "Getting feeds needed for updating through querying with
promptings and injecting them into `entropy/emacs-rss--elfeed-multi-update-feeds-list'."
    (setq entropy/emacs-rss--elfeed-multi-update-feeds-list nil
          entropy/emacs-rss--elfeed-feed-prompt-alist nil)
    (setq entropy/emacs-rss--elfeed-feed-prompt-alist
          (entropy/emacs-rss--elfeed-list-feeds))
    (ivy-read "Update feeds: " entropy/emacs-rss--elfeed-feed-prompt-alist
              :require-match t
              :action #'entropy/emacs-rss--elfeed-update-read-action
              :caller #'entropy/emacs-rss--elfeed-get-multi-update-feeds))

  (defun entropy/emacs-rss-elfeed-multi-update-feeds ()
    "Update feeds interactively by multiplied choicing from
`entropy/emacs-rss--elfeed-feed-prompt-alist'."
    (interactive)
    (let* ((feeds (progn (entropy/emacs-rss--elfeed-get-multi-update-feeds)
                         entropy/emacs-rss--elfeed-multi-update-feeds-list))
           (tmpvar (entropy/emacs-rss--elfeed-arrange-feeds-plist feeds)))
      (entropy/emacs-rss--elfeed-fetch-feeds
       tmpvar)))

;; *** default external browser for feed viewing

  (defun elfeed-search-browse-url (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'.

Note: this function has been modified by entropy-emacs for fixing
one bug of which:

Original func coding logic leaving entry update part after opening
url, this will not redraw the \"*elfeed-search*\" successfully
because that all its behavior are not witin the
\"*elfeed-search*\" buffer as it be on w3m or eww buffer when using
internal `browse-url-browser-function'.

The minor changing was compat for above."
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               do (elfeed-search-update-entry entry)
               when (elfeed-entry-link entry)
               do (if use-generic-p
                      (browse-url-generic it)
                    (browse-url it)))
      (unless (use-region-p) (forward-line))))


  (defun entropy/emacs-rss--elfeed-browse-url-around (oldfun &rest args)
    "Browse feed source by external browser with choosing."
    (let* ((choice
            (completing-read "Choose browse: " `("default"
                                                 "eww"
                                                 ,(if entropy/emacs-browse-url-function "entropy-browse-url-function" "")
                                                 ,(if (executable-find "w3m") "emacs-w3m" ""))
                             nil t))
           (browse-url-browser-function (cl-case (intern choice)
                                          ('default 'browse-url-default-browser)
                                          ('eww 'eww-browse-url)
                                          ('emacs-w3m 'entropy/emacs-textwww--w3m-browse-url)
                                          ('entropy-browse-url-function entropy/emacs-browse-url-function))))
      (funcall oldfun)))
  (advice-add 'elfeed-search-browse-url :around #'entropy/emacs-rss--elfeed-browse-url-around)
  (advice-add 'elfeed-show-visit :around #'entropy/emacs-rss--elfeed-browse-url-around)

;; *** elfeed print entry function
  (defun entropy/emacs-rss--elfeed-search-print-entry--default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) "\t")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (setq elfeed-search-print-entry-function 'entropy/emacs-rss--elfeed-search-print-entry--default)

  )

;; ** newsticker

;; used to read emacs related rss

;; FIXME: newsticker is buggy in some way, see bug of [h-936e4fb3-57d7-4958-89d9-a1cdfb52c495]

(use-package newsticker
  :ensure nil
  :commands (newsticker-show-news newsticker-treeview-quit)
  :eemacs-tpha
  (((:enable t :defer (:data (:adfors (entropy/emacs-hydra-hollow-call-before-hook)
                                      :adtype hook :pdumper-no-end t))))
   ("Rss"
    (("r a" newsticker-show-news
      "Read emacs about news"
      :enable t :exit t))))
;; *** init
  :init
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

;; *** config
  ;; FIXME:  usepackage of `newsticker' take no effects in :config slot
  (entropy/emacs-make-function-inhibit-readonly
   'newsticker-treeview-save)

  (entropy/emacs-make-function-inhibit-readonly
   'newsticker-treeview-quit)

  (defun __adv/after/newsticker-treeview-quit/stop-all (&rest _)
    "Stop newsticker tickers and all lived related buffers and windows."
    (dolist (buff newsticker--treeview-buffers)
      (let ((kill-buffer-hook nil))
        (kill-buffer buff)))
    (dolist (win newsticker--treeview-windows)
      (let (_)
        (when (and (window-live-p win)
                   (not (eq (window-main-window) win)))
          (delete-window win))))
    (newsticker-stop))
  (advice-add 'newsticker-treeview-quit
              :after
              #'__adv/after/newsticker-treeview-quit/stop-all)
  :config
  )


;; * provide
(provide 'entropy-emacs-rss)
