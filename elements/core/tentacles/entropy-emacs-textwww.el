;;; entropy-emacs-testwww.el --- Plain text browser configuration for entropy-emacs
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
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defun)

;; ** uniform
(defvar entropy/emacs-textwww--uniform-func nil)
(defvar entropy/emacs-textwww--uniform-expanded nil)
(setq entropy/emacs-textwww--uniform-func
      (lambda ()
        (unless entropy/emacs-textwww--uniform-expanded
          (cl-defmacro entropy/emacs-textwww--hydra-uniform
              (&key hydra-name hydra-body-map bookmark-library-view bookmark-add
                    browse-with-external
                    current-page-url current-link-url previous-page next-page goto-url
                    search-query)
            (let ((hydra-body-func (intern (concat (symbol-name hydra-name) "/body"))))
              `(progn
                 (defhydra ,hydra-name (:color blue :hint nil)
                   "
^page-move^                ^link-retrieve^           ^bookmak operation^      ^search^
^^-------------------------^^------------------------^^-----------------------^^----------
_l_: previous page         _c_: current-page-url     _b_: view bookmarks      _s_: search
_n_: next page             _u_: current-link-url     _a_: add bookmark
_e_: browse-externally
"
                   ("b" ,bookmark-library-view)
                   ("a" ,bookmark-add)
                   ("e" ,browse-with-external)
                   ("c" ,current-page-url)
                   ("u" ,current-link-url)
                   ("l" ,previous-page)
                   ("n" ,next-page)
                   ("s" ,search-query))
                 (define-key ,hydra-body-map (kbd "C-q") #',hydra-body-func))))
          (setq entropy/emacs-textwww--uniform-expanded t))))

;; ** browsers
;; *** emacs-w3m interface
(use-package w3m
  :if (executable-find "w3m")
  :commands
  (w3m
   w3m-search
   w3m-goto-url)

  :bind
  (:map w3m-mode-map
        ("<down>" . next-line)
        ("<up>" . previous-line)
        ("<left>" . left-char)
        ("<right>" . right-char))
  :init

  ;; w3m personal browse url function
  (defun entropy/emacs-textwww--w3m-browse-url (url &rest args)
    (w3m-goto-url url))

  :config
  (funcall entropy/emacs-textwww--uniform-func)
  (entropy/emacs-textwww--hydra-uniform
   :hydra-name entropy/emacs-textwww--w3m-uniform
   :hydra-body-map w3m-mode-map
   :bookmark-library-view w3m-bookmark-view
   :bookmark-add w3m-bookmark-add-current-url
   :browse-with-external w3m-view-url-with-browse-url
   :current-page-url w3m-print-current-url
   :current-link-url w3m-print-this-url
   :previous-page w3m-view-previous-page
   :next-page w3m-view-next-page
   :search-query w3m-search)

  (entropy/emacs-lazy-load-simple 'w3m-search
    (add-to-list 'w3m-search-engine-alist
                 '("bing" "https://www.bing.com/search?q=%s" utf-8))
    (setq w3m-search-default-engine "bing"))
  (setq w3m-confirm-leaving-secure-page nil)
  (setq w3m-image-no-idle-timer t)
  (setq w3m-image-animate-seconds nil)
  (setq w3m-show-graphic-icons-in-header-line nil)
  (setq w3m-use-favicon nil)
  (setq w3m-use-refresh nil)
  (setq w3m-use-tab nil)
  (setq w3m-use-tab-menubar nil)
  (setq w3m-process-timeout 5)
  (setq w3m-pop-up-windows nil)
  (when (and (eq system-type 'windows-nt)
             (file-exists-p (concat invocation-directory "convert.exe")))
    (setq w3m-imagick-convert-program (concat invocation-directory "convert.exe")))

  ;; session configuration
  (setq w3m-session-autosave nil)
  (setq w3m-session-deleted-save nil)
  (setq w3m-session-crash-recovery nil)

  ;; w3m external browser setting
  (defun entropy/emacs-textwww--w3m-external-advice (oldfunc &rest args)
    (let ((browse-url-browser-function
           (if entropy/emacs-browse-url-function
               entropy/emacs-browse-url-function
             'browse-url-default-browser)))
      (call-interactively oldfunc)))
  (advice-add 'w3m-view-url-with-browse-url :around #'entropy/emacs-textwww--w3m-external-advice))


;; *** eww config
(use-package eww
  :preface
;; **** get image url
  (entropy/emacs-lazy-load-simple 'shr
    (defun entropy/emacs-textwww-get-eww-url ()
      "Get image or point url at eww or it's derived modes."
      (interactive)
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
;; **** eww search engine
  (if entropy/emacs-eww-search-engine-customize
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
    (interactive)
    (unless (not (equal major-mode 'eww-mode))
      (let ((url (eww-copy-page-url))
            (browse-url-browser-function
             (let (rtn choices)
               (when (functionp entropy/emacs-browse-url-function)
                 (add-to-list 'choices `("personal" ,entropy/emacs-browse-url-function)))
               (when (executable-find "w3m")
                 (add-to-list 'choices `("w3m" ,(lambda (url &rest args)
                                                  (w3m-goto-url url)))))
               (add-to-list 'choices '("default" browse-url-default-browser))
               (setq rtn (completing-read "Choice external browser:  "
                                          choices nil t))
               (setq rtn (nth 1 (assoc rtn choices)))
               rtn)))
        (browse-url url))))

;; **** uniform

  (funcall entropy/emacs-textwww--uniform-func)
  (entropy/emacs-textwww--hydra-uniform
   :hydra-name entropy/emacs-textwww--eww-uniform
   :hydra-body-map eww-mode-map
   :bookmark-library-view eww-list-bookmarks
   :bookmark-add eww-add-bookmark
   :browse-with-external entropy/emacs-textwww-eww-open-url-external
   :current-page-url eww-copy-page-url
   :current-link-url entropy/emacs-textwww-get-eww-url
   :previous-page eww-back-url
   :next-page eww-next-url
   :search-query eww))

;; ** caller
;; *** search-web

;; Post web search queries using `browse-url'.
(use-package search-web
  :ensure nil
  :commands (search-web search-web-region)
  :bind (("C-c w" . entropy/emacs-textwww-search-web-toggle)
         ("C-c W" . entropy/emacs-textwww-search-web-region-toggle))
  :config

  (require 'cl)

;; **** default config

  ;; redefine search-web for compat with entropy-emacs
  (defun search-web (engine word)
    "Note this function has been modified for compating with entropy-emacs.

The original one can't recovering default browser function,
fixing it as thus. "
    (interactive (list
                  (search-web-query-engine)
                  (read-string "Search Word: " nil 'search-web-word-history)))
    (destructuring-bind (engine url render)
        (assoc engine search-web-engines)
      (let* ((render
              (case render
                ((nil) search-web-default-browser)
                (In-Emacs search-web-in-emacs-browser)
                (External search-web-external-browser)
                (t render))))
        (setq browse-url-browser-function render)
        (browse-url (format url (url-hexify-string word)))
        (setq-default browse-url-browser-function search-web-default-browser))))


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
  (defun entropy/emacs-textwww-search-web-toggle ()
    (interactive)
    (let ((type (completing-read "Internal or External: " '("Internal" "External") nil t)))
      (let* ((search-web-engines (cond
                                  ((equal type "Internal") entropy/emacs-search-web-engines-internal)
                                  ((equal type "External") entropy/emacs-search-web-engines-external)))
             (engine (entropy/emacs-textwww--search-web-query-egine type))
             (word (read-string "Searching for?: ")))
        (search-web engine word))))

  (defun entropy/emacs-textwww-search-web-region-toggle ()
    (interactive)
    (let ((type (completing-read "Internal or External: " '("Internal" "External") nil t)))
      (let* ((search-web-engines (cond
                                  ((equal type "Internal") entropy/emacs-search-web-engines-internal)
                                  ((equal type "External") entropy/emacs-search-web-engines-external)))
             (engine (entropy/emacs-textwww--search-web-query-egine type)))
        (search-web-region engine))))

  (setq search-web-in-emacs-browser 'eww-browse-url)

  (defun entropy/emacs-textwww--search-web--around (oldfun &rest arg-rest)
    "Partially cancel `entropy/emacs-web-development-environment' if
    it's actived."
    (let* ((entropy/emacs-web-development-environment nil))
      (funcall oldfun)))

  (advice-add 'entropy/emacs-textwww-search-web-toggle :around #'entropy/emacs-textwww--search-web--around)
  (advice-add 'entropy/emacs-textwww-search-web-region-toggle :around #'entropy/emacs-textwww--search-web--around))

;; *** toggle the default browse in emacs
(defun entropy/emacs-textwww--setting-default-browser (browser)
  "Setting the default browser for `search-web' and all the
url-open about function."

  (setq-default search-web-default-browser browser)
  (setq-default search-web-external-browser browser)
  (setq-default search-web-in-emacs-browser 'eww-browse-url)
  (setq-default browse-url-browser-function browser)
  (if (version< emacs-version "27")
      (setq shr-external-browser browser)
    (setq browse-url-secondary-browser-function browser)))

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
  (let* ((list-of-choice (cond ((and entropy/emacs-enable-personal-browse-url-function
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
          (ivy-read "Choose the function you want: " list-of-choice)))
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
(when (and entropy/emacs-browse-url-function entropy/emacs-enable-personal-browse-url-function)
  (if (not (executable-find "w3m"))
      (entropy/emacs-textwww--setting-default-browser entropy/emacs-browse-url-function)
    (if (display-graphic-p)
        (entropy/emacs-textwww--setting-default-browser entropy/emacs-browse-url-function)
      (entropy/emacs-textwww--setting-default-browser 'entropy/emacs-textwww--w3m-browse-url))))


;; * provide
(provide 'entropy-emacs-textwww)
