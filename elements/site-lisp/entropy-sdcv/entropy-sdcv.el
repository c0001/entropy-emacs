;;; entropy-sdcv.el --- entropy emacs sdcv front-end

;; Copyright (C) 20181211  Entropy

;; Author:           Entropy <bmsac0001@gmail.com>
;; Maintainer:       Entropy <bmsac001@gmail.com>
;; URL:              https://github.com/c0001/entropy-sdcv
;; Package-Version:  20191116.2003
;; Version:          0.1.1
;; Created:          2018-12-11 12:48:04
;; Keywords:         sdcv
;; Compatibility:    GNU Emacs 26.1;
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
;;
;;; Commentary:

;; A emacs-lisp implementation of dictionary client.

;; Name component 'sdcv' means [[https://github.com/Dushistov/sdcv][stardict console version]] that this
;; project originally was the front-end for 'sdcv', but now with
;; architecture re-built, it becomes one dictionary query-response
;; frameworks.

;; *Features:*

;; 1. [[https://github.com/xuchunyang/youdao-dictionary.el][youdao-dict]], [[https://github.com/cute-jumper/bing-dict.el][bing-dict]], [[https://github.com/atykhonov/google-translate][google-dict]] defaultly supported.

;; 2. Out-of-box, easily usage with two command
;;    ~entropy/sdcv-search-at-point-tooltip~ and
;;    ~entropy/sdcv-search-input-adjacent~.

;; 3. Extensible wildly. Simple apis for add new dictionary backend
;;    and new displaying method.

;; *Quick starting:*

;; + Preparation:

;;  =entropy-sdcv= have sets of built-in dict backend, but defautly
;;   using 'sdcv', you should put it in your =PATH=, or if you do not
;;   want to using it as default backend, you should picking up your
;;   specified value of =entropy/sdcv-default-query-backend-name= to
;;   one of 'youdao', 'bing' or 'google'.

;;   For sdcv usage, you need cloned your own sdcv dict database
;;   stored in your =~/.stardict=, and structed as folder hosted
;;   individually, as:

;;   #+BEGIN_EXAMPLE
;;   --~/.stardict
;;     |
;;     |--oxford-dict
;;        |--oxford.dict.dz
;;        |--oxford.idx
;;        |--oxford.ifo
;;   #+END_EXAMPLE

;; + Interaction:

;;   Call command ~entropy/sdcv-search-at-point-tooltip~ to search
;;   thesaurus at current point and show it in tooltip buffer. Or if
;;   you want to search by manually inputting, calling
;;   ~entropy/sdcv-search-input-adjacent~ instead.

;;   And for some reason, you want to toggle dict backend, you can
;;   call ~entropy/sdcv-toggle-backend~ for thus, even for calling
;;   ~entropy/sdcv-toggle-show-tooltip-method~ to switch displaying
;;   type (show-type).

;;   Further more, you can enable ~entropy/sdcv-autoshow-mode~ to show
;;   translation response at point automatically with minor delay. You
;;   also can change 'autoshow' dict-backend or show-method with usually
;;   interactive method demoted above.

;;; Development
;;
;; As denoted in commentary, entropy-sdcv provides wildly extensible
;; ability, it makes attention in two aspect i.e. the dict type and
;; query response display method. We give the protocols to represent
;; dictionary type and the display method for rulling the
;; maintainability, also mainly for reducing the cost of development.
;;
;; All the protocols defination are written in
;; =entropy-sdcv-core.el=, read its commentary for briefly
;; understanding.
;;
;;
;;; Chanage log

;; 2019/11/27
;;      * Add `entropy/sdcv-autoshow-mode'
;;        - autoshow for all builtin dict backends.
;;        - fix some typo and minor bugs

;; 2019/11/16
;;      * Fix bugs for face setting
;;        - Api =:show-face= now require the SHOW-METHOD argument for
;;          funcion aspect.
;;        - New api: ~entropy/sdcv-core-use-face~ in core library.

;; 2019/11/06
;;      * Rebuilding features logic to prepare for 0.2.0 release
;;        - using multi-backends instead sdcv maily framework
;;        - protocols made out for specification and featue adding
;;        - more more rigorous srcs splitting style.

;; 2019/10/23
;;      * version v0.1.1 pop out
;;        - Using new auto-gen's tooltip face render
;;        - Add `pos-tip' tooltip type.

;; 2018/12/11
;;      * First release pop out v0.1.0

;;; Code:
;;;; require
(require 'entropy-sdcv-core)
(require 'entropy-sdcv-show)
(require 'entropy-sdcv-backends)

;;;; defcustom
(defgroup entropy/sdcv-interactive-group nil
  "Variable group for `entropy-sdcv' interaction portion."
  :group 'entropy-sdcv)

(defcustom entropy/sdcv-default-show-adjacent-method 'adjacent-common
  "Defualt adjacent query response show method."
  :type 'symbol
  :group 'entropy/sdcv-interactive-group)

(defcustom entropy/sdcv-default-show-tooltip-method 'posframe
  "Defualt tooltip query response show method."
  :type 'symbol
  :group 'entropy/sdcv-interactive-group)

(defcustom entropy/sdcv-default-query-backend-name 'sdcv
  "Default query response backend chosen."
  :type 'symbol
  :group 'entropy/sdcv-interactive-group)

(defcustom entropy/sdcv-autoshow-delay 1
  "Idle delay for `entropy/sdcv-autoshow-mode'."
  :type 'integer
  :group 'entropy/sdcv-interactive-group)

;;;; library
(defun entropy/sdcv-lang-set-process-for-sdcv-backends (old-func &rest args)
  "The around advice for set specification emacs lang set when
calling query process for the reason that:

sdcv cli responsed alwasys for utf-8 encoding information as, the
none utf-8 lang set will not decoding the response string
correctly, the particular problem was for func
`entropy/sdcv--sdcv-check-dicts' which will get the unicode
rsepresentation dict name string when the emacs lang set was not
the subject of utf-8 group."
  (if (not (eq entropy/sdcv-default-query-backend-name 'sdcv))
      (apply old-func args)
    (entropy/sdcv-core-set-specific-lang-env)
    (apply old-func args)
    (entropy/sdcv-core-recovery-user-origin-lang-env)))

;;;; lazy show mode

(defvar entropy/sdcv-autoshow-timer-register nil
  "Timer register for `entropy/sdcv-autoshow-mode'.")

(defvar entropy/sdcv-autoshow-last-query nil
  "The last query-word for `entropy/sdcv-autoshow-create'.")

(defun entropy/sdcv-autoshow-create (buff)
  `(lambda ()
     (let ((thing (entropy/sdcv-core-get-word-or-region t))
           show-instance)
       (when (and entropy/sdcv-autoshow-mode
                  (eq ,buff (current-buffer))
                  (stringp thing)
                  (not (string= entropy/sdcv-autoshow-last-query thing))
                  (fboundp 'bing-dict-brief))
         (setq entropy/sdcv-autoshow-last-query thing)
         (setq show-instance
               (entropy/sdcv-core-query-backend
                thing
                entropy/sdcv-default-query-backend-name
                'minibuffer-common))
         (unless (string= (plist-get show-instance :feedback)
                          entropy/sdcv-core-response-null-prompt)
           (entropy/sdcv-core-response-show
            (cons 'minibuffer-common
                  show-instance)))))))

(define-minor-mode entropy/sdcv-autoshow-mode
  "Automatically show the translation based on point thing."
  nil nil nil
  (if entropy/sdcv-autoshow-mode
      (add-to-list 'entropy/sdcv-autoshow-timer-register
                   (cons (current-buffer)
                         (run-with-idle-timer
                          entropy/sdcv-autoshow-delay t
                          (entropy/sdcv-autoshow-create (current-buffer)))))
    (let ((timer (alist-get (current-buffer)
                            entropy/sdcv-autoshow-timer-register)))
      (when (timerp timer)
        (cancel-timer timer))
      (setq entropy/sdcv-autoshow-timer-register
            (delete (assoc (current-buffer) entropy/sdcv-autoshow-timer-register)
                    entropy/sdcv-autoshow-timer-register)))))

;;;; main
(advice-add 'entropy/sdcv-search-at-point-tooltip :around #'entropy/sdcv-lang-set-process-for-sdcv-backends)
(advice-add 'entropy/sdcv-search-input-adjacent :around #'entropy/sdcv-lang-set-process-for-sdcv-backends)

;;;###autoload
(defun entropy/sdcv-search-at-point-tooltip ()
  "Mainly interactive func for search point or marked region
string with sdcv cli."
  (interactive)
  (let ((query (entropy/sdcv-core-get-word-or-region)))
    (unless (stringp query)
      (error "Could not find word or region at point."))
    (setq query (entropy/sdcv-core-query-rebuit query))
    (entropy/sdcv-core-query-and-show
     query
     entropy/sdcv-default-query-backend-name
     entropy/sdcv-default-show-tooltip-method)))

;;;###autoload
(defun entropy/sdcv-search-input-adjacent ()
  "Mainly interactive func for search with inputted querying
string with sdcv cli."
  (interactive)
  (let* ((query (let (promt empty-error (rtn "") (prompt-initial (entropy/sdcv-core-get-word-or-region)))
                  (cond (prompt-initial
                         (setq prompt (format "Input word (default: %s): " prompt-initial)))
                        (t (setq prompt "Input word: ")))
                  (setq rtn (read-string prompt))
                  (cond ((and (equal "" rtn)
                              prompt-initial)
                         (setq rtn prompt-initial))
                        ((and (equal "" rtn)
                              (not prompt-initial))
                         (error "Input empty!")))
                  rtn)))
    (setq query (entropy/sdcv-core-query-rebuit query))
    (entropy/sdcv-core-query-and-show
     query
     entropy/sdcv-default-query-backend-name
     entropy/sdcv-default-show-adjacent-method)))

;;;###autoload
(defun entropy/sdcv-toggle-backend ()
  (interactive)
  (let ((backends (mapcar (lambda (x) (symbol-name (car x)))
                          entropy/sdcv-core-query-backends-register)))
    (setq entropy/sdcv-default-query-backend-name
          (intern
           (completing-read "Backends: " backends nil t)))))

(defun entropy/sdcv-toggle-show-tooltip-method ()
  (interactive)
  (let ((methods (mapcar (lambda (x) (symbol-name (car x)))
                         entropy/sdcv-core-response-show-frontends)))
    (setq entropy/sdcv-default-show-tooltip-method
          (intern
           (completing-read "Backends: " methods nil t)))))

;;;###autoload
(defun entropy/sdcv-clean-autoshow-all ()
  (interactive)
  (cl-loop for item in entropy/sdcv-autoshow-timer-register
           when (timerp (cdr item))
           do (progn (cancel-timer (cdr item))
                     (when (buffer-live-p (car item))
                       (with-current-buffer (car item)
                         (entropy/sdcv-autoshow-mode 0)))))
  (setq entropy/sdcv-autoshow-timer-register nil))

;;; provide
(provide 'entropy-sdcv)
