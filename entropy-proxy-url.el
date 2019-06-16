;;; entropy-proxy-url ---  Url proxy for emacs eww and w3m
;;
;; * Copyright (C) 20190617  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-proxy-url/
;; Package-Version: package-version
;; Version:       file-version
;; Created:       year-month-date hour:min:sec
;; Keywords:      kewords-1, kewords-2, kewords-3,
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
;; An emacs extension trafficked url matching by proxy regxp rule-set or
;; pull down them into whole proxy tunnel.
;;
;; This package perform as the library role for the functional provision.
;;
;; ** Eww and emacs-w3m proxy stuff
;;
;; Emacs has the ability to retrieving network data steps with internal
;; func ~url-retrieve~ and it's refer url library as. Eww using it as the
;; communication backend.
;;
;; Although, there's another way for emacs to get network data througha
;; the way of using external shell application which obtains its
;; responsing as the render part. One as be in this case was the famous
;; cli plain text browser [[http://w3m.sourceforge.net/][w3m]].
;;
;; The proxy way for the internal data retrieve was based on emacs
;; internal proxy mechanism, and the other one was based on the refer
;; response creator's self mechanism.
;;
;; ** How this package working as?
;;
;; This package taking off the proxy way through two ways for as:
;;
;; 1. Regexp matching of current url retrieving.
;; 2. Full proxy tunnel whichever current transfer url is.
;;
;; As seen as which are easy to understanding of the full proxy
;; type,here's the description for 'regexp' way:
;;
;; Each network transfer request will come with one url string as the
;; requesting wrapper header. URL string has it's feature stored in it's
;; domain part, the domain part can be matched by categorizing using
;; regexp method, thus each network requesting can be filter by sets of
;; regexp rule-set for determining whether go into the proxy tunnerl.
;;
;; The regexp filter can be generized of more functional part, by
;; matching whatever you want. The rule-set internal customized variable
;; =entropy/proxy-url-regexp-list= as is.
;;
;; This package use the [[https://github.com/gfwlist/gfwlist][gfwlist]] as the default ruleset, for the original
;; prototype for it was the simple variant of [[https://en.wikipedia.org/wiki/Proxy_auto-config][PAC]](proxy auto
;; configuration), whose syntax abided by [[https://adblockplus.org/][adblock-plus]] web extensions,
;; for that reason to use this was that gfwlist was originally used for
;; the webbrowser extensions for doing thus as what you have known for
;; the internet charging among on CHINA. Thus for that, this package
;; requires one =PAC= analyzer as now I use the another [[https://github.com/c0001/entropy-emacs][entropy-emacs]]
;; specific package [[https://github.com/c0001/entropy-adblockP-rule-analysis][entropy-adblock+-rule-analysis]] to role as the gfwlist
;; anaylizer, it will auto fetch the latest version gfwlist if possible
;; when you current internet environment allow the connecting for that,
;; or using the package built-in one which was the pre-fetched one, so it
;; will not be the latest version.
;;
;; ** Methods given
;;
;; The time =entropy-proxy-url='s provision as the mentioned above that
;; given for the top web browsing scene both of =eww= and =emacs-w3m=.
;;
;; There's two macro defined for the core proxy diversion filter:
;;
;; 1) ~entropy/proxy-url-type-form~
;;    
;;    Form to automatically creating the proxy setting function,
;;    arguments of:
;;
;;    * =FUNC-REGEXP=:
;;
;;      Function to set proper regexp proxy config to refer target
;;      trigger operation.
;;      
;;    * =FUNC-FULL=:
;;
;;      Function to set proper full proxy config to refer target trigger
;;      operation.
;;
;;    * =FUNC-CLEAN=
;;
;;      Function to recover the original target trigger operation's
;;      config.
;;
;;    * =URL=:
;;
;;      Url requesting string.
;;
;;    * =TYPE-SOURCE=
;;
;;      The proxy type of 't', 'regexp', 'nil' corresponding to 'full',
;;      'regexp', 'clean' respectively.
;;
;; 2) ~entropy/proxy-url-swith-form~
;;
;;    Form of switching the proxy type dynamically.
;;
;;    The only argument =TYPE-SOURCE= as the ~setf~ place for stored the
;;    proxy type selected lastly.
;;
;;
;;
;; All the arguments name as the unified meaning as is. Thus, the
;; implementation both for 'eww' and 'emacs-w3m' feature for:
;;
;; | Target interaction | proxy-type | func-implementation                 |
;; |--------------------+------------+-------------------------------------|
;; | ~w3m-retrieve~     | regexp     | ~entropy/proxy-url--regexp-for-w3m~ |
;; |                    | full       | ~entropy/proxy-url--full-for-w3m~   |
;; |                    | clean      | ~entropy/proxy-url--clean-for-w3m~  |
;; |                    |            |                                     |
;; |--------------------+------------+-------------------------------------|
;; | ~eww-browse-url~   | regexp     | ~entropy/proxy-url--regexp-for-eww~ |
;; |                    | full       | ~entropy/proxy-url--full-for-eww~   |
;; |                    | clean      | ~entropy/proxy-url--clean-for-eww~  |
;;
;;
;;
;; For those implementation's =TYPE-SOURCE= are
;; =entropy/proxy-url-enable-eww= and =entropy/proxy-url-enable-w3m=.
;;
;; ** Proxy reset
;;
;; For those cases that you want to quickly reset the proxy server, you
;; should reset the proxy config among the eww and w3m utilities, the
;; interactive function ~entropy/proxy-url-reset-proxy~ give the way for
;; as. For then, you could reset the =entropy/proxy-mode-url-proxy= value
;; and then all has done.
;;
;;
;; * Configuration:
;;
;; ** Target operation advice
;;
;; Now, as the designation for as be library of proxy toggle one,
;; =entropy-proxy-url= didn't advicing each target operation function as
;; the =:before= type internally, you will do as manually as:
;;
;; #+BEGIN_SRC elisp
;;   (advice-add 'eww-browse-url :before #'entropy/proxy-url-proxy-choice-for-eww)
;;   (advice-add 'w3m-goto-mailto-url :before #'entropy/proxy-url-proxy-choice-for-w3m)
;;   (advice-add 'w3m-goto-ftp-url :before #'entropy/proxy-url-proxy-choice-for-w3m)
;;   (advice-add 'w3m--goto-url--valid-url :before #'entropy/proxy-url-proxy-choice-for-w3m)
;; #+END_SRC
;;
;; ** The regexp rule-set list data
;;
;; Internally, =entropy-proxy-url= has given the sets of regexp rule set
;; tracking by [[https://github.com/gfwlist/gfwlist][github gfw list]] project which maintained the common
;; sensible blocked web domain list directed against to China GFW network
;; ecosystem, however I thought as be compatible for some web transfer
;; chargin area too of that China as the biggest aspect doing for thus.
;; The gfw-rule analyzing provided by [[https://github.com/c0001/entropy-adblockP-rule-analysis][entropy-adbp+-rule-analysis]] package
;; (Add it to ~load-path~ was requested also).
;;
;;
;;
;; 
;; * Code:

(require 'entropy-proxy-mode)
(require 'w3m)
(require 'entropy-proxy-url-gfw-list)

;; ** variable declare
(defvar entropy/proxy-url--enable 'regexp
  "If none-nil enable proxy for two type:

    - symbol ‘regexp’: proxy url matched by regexp of list
      `entropy/proxy-url-regexp-list'

    - symbol ‘t’: proxy for all searching。")

(defvar entropy/proxy-url--enable-eww entropy/proxy-url--enable)
(defvar entropy/proxy-url--enable-w3m entropy/proxy-url--enable)
(defvar entropy/proxy-url--w3m-regexp-injected nil)
(defvar entropy/proxy-url--url-proxy entropy/proxy-mode-url-proxy)

(defgroup entropy/proxy-url-group nil
  "group of `entropy-proxy-url'")

(defcustom entropy/proxy-url-regexp-list
  '(("google")
    ("wikipedia"))
  "The regexp list for matching url for using proxy.

The structer of this variable format was equal with
`w3m-command-arguments-alist'."
  :type 'sexp
  :group 'entropy/proxy-url-group)

;; ** macro
(defmacro entropy/proxy-url--type-form (func-regexp func-full func-clean url type-source)
  "Macro for creating auto proxy setting function accroding the
value of `entropy/proxy-url--enable'."
  `(let ((regular (symbol-value ,type-source)))
     (cond
      ((eq regular 'regexp)
       (funcall ,func-regexp ,url))
      ((eq regular 't)
       (funcall ,func-full))
      ((eq regular nil)
       (funcall ,func-clean)))))

(defmacro entropy/proxy-url--swith-form (type-source)
  "Macro for creating proxy type throung the way of setting each of
`entropy/proxy-url--enable-eww' and `entropy/proxy-url--enable-w3m'."
  `(let ((choice (intern (completing-read "Choose proxy method: "
                                          '("regexp" "t" "nil")))))
     (setf (symbol-value ,type-source) choice)))

;; ** Create eww group function
(defun entropy/proxy-url--regexp-for-eww (url)
  "Proxy url visiting by regexp matching by
`entropy/proxy-url-regexp-list' with eww operation."
  (let ((regexp entropy/proxy-url-regexp-list))
    (if (let ((rtn nil))
          (dolist (el regexp)
            (when (string-match-p (car el) url)
              (setq rtn t)))
          rtn)
        (when (not entropy/proxy-mode)
          (entropy/proxy-mode-enable 'url))
      (when entropy/proxy-mode
        (entropy/proxy-mode-disable)))))

(defun entropy/proxy-url--full-for-eww ()
  "Proxy for all urls visiting for eww operation."
  (when (not entropy/proxy-mode)
    (entropy/proxy-mode-enable 'url)))


(defun entropy/proxy-url--clean-for-eww ()
  "Clean all proxy setting for eww operation."
  (when entropy/proxy-mode
    (entropy/proxy-mode-disable)))

;; ** Create w3m group function

(defun entropy/proxy-url--append-w3m-proxy (regexp)
  (let ((http-proxy (cdr (assoc "http" entropy/proxy-url--url-proxy)))
        (https-proxy (cdr (assoc "https" entropy/proxy-url--url-proxy))))
    (list regexp
          "-o"
          (concat "http_proxy=" (if (string-match-p "^http://" http-proxy)
                                    ""
                                  "http://")
                  http-proxy)
          "-o"
          (concat "https_proxy=" (if (string-match-p "^https://" https-proxy)
                                     ""
                                   "https://")
                  https-proxy))))

(defun entropy/proxy-url--gen-w3m-full-proxy-argument ()
  (entropy/proxy-url--append-w3m-proxy ".*"))

(defun entropy/proxy-url--set-w3m-command-arguments ()
  "Adding proxy config one by one to `w3m-command-arguments-alist'."
  (when (and entropy/proxy-url-regexp-list
             (listp entropy/proxy-url-regexp-list))
    (dolist (el entropy/proxy-url-regexp-list)
      (add-to-list 'w3m-command-arguments-alist
                   (entropy/proxy-url--append-w3m-proxy (car el))))))

(defun entropy/proxy-url--regexp-for-w3m (&optional arg)
  "Proxy url visiting by regexp matching by
`entropy/proxy-url-regexp-list' with w3m operation."
  (when (not entropy/proxy-url--w3m-regexp-injected)
    (entropy/proxy-url--reset-w3m-arg-alist)
    (entropy/proxy-url--set-w3m-command-arguments)))

(defun entropy/proxy-url--reset-w3m-arg-alist (&optional do-all)
  (let (regexp-clean all-match-clean)
    (setq regexp-clean
          (lambda ()
            (dolist (el entropy/proxy-url-regexp-list)
              (setq w3m-command-arguments-alist
                    (delete (assoc (car el) w3m-command-arguments-alist)
                            w3m-command-arguments-alist))))
          all-match-clean
          (lambda ()
            (setq w3m-command-arguments-alist
                  (delete (entropy/proxy-url--gen-w3m-full-proxy-argument)
                          w3m-command-arguments-alist))))
    (cl-case do-all
      (nil (cond ((assoc (caar entropy/proxy-url-regexp-list)
                         w3m-command-arguments-alist)
                  (funcall regexp-clean))
                 ((member (entropy/proxy-url--gen-w3m-full-proxy-argument)
                          w3m-command-arguments-alist)
                  (funcall all-match-clean))))
      (t (funcall regexp-clean)
         (funcall all-match-clean)))))

(defun entropy/proxy-url--full-for-w3m ()
  "Proxy for all urls visiting for eww operation."
  (when entropy/proxy-url--w3m-regexp-injected
    (setq entropy/proxy-url--w3m-regexp-injected nil))
  (entropy/proxy-url--reset-w3m-arg-alist)
  (when (not (member (entropy/proxy-url--gen-w3m-full-proxy-argument)
                     w3m-command-arguments-alist))
    (add-to-list 'w3m-command-arguments-alist
                 (entropy/proxy-url--gen-w3m-full-proxy-argument))))

(defun entropy/proxy-url--clean-for-w3m ()
  "Clean all proxy setting for `emacs-w3m'."
  (entropy/proxy-url--reset-w3m-arg-alist t))


;; ** main
;;;###autoload
(defun entropy/proxy-url-proxy-choice-for-eww (url &rest args)
  "Determined for whether using proxy for searching specific url
or for all searching operation for `eww'.

The determined condition was refer by
`entropy/proxy-url--enable'."
  (entropy/proxy-url--type-form 'entropy/proxy-url--regexp-for-eww
                                'entropy/proxy-url--full-for-eww
                                'entropy/proxy-url--clean-for-eww
                                url
                                'entropy/proxy-url--enable-eww))

;;;###autoload
(defun entropy/proxy-url-proxy-choice-for-w3m (&optional url &rest args)
  "Determined for whether using proxy for searching specific url or
for all searching operation for `emacs-w3m'.

The determined condition was refer by `entropy/proxy-url--enable'."
  (entropy/proxy-url--type-form 'entropy/proxy-url--regexp-for-w3m
                                'entropy/proxy-url--full-for-w3m
                                'entropy/proxy-url--clean-for-w3m
                                url
                                'entropy/proxy-url--enable-w3m))

;;;###autoload
(defun entropy/proxy-url-switch-for-eww ()
  "Switching proxy type for `eww'."
  (interactive)
  (entropy/proxy-url--swith-form 'entropy/proxy-url--enable-eww))

;;;###autoload
(defun entropy/proxy-url-switch-for-w3m ()
  "Switching proxy type for `emacs-w3m'."
  (interactive)
  (entropy/proxy-url--swith-form 'entropy/proxy-url--enable-w3m))

;;;###autoload
(defun entropy/proxy-url-switch-fo-all ()
  "Unified the value of proxy type both of `eww' and `emacs-w3m'."
  (interactive)
  (let ((choice (intern (completing-read "Choose proxy method for all: " '("regexp" "t" "nil")))))
    (mapcar #'(lambda (x)
                (setf (symbol-value x) choice))
            (list 'entropy/proxy-url--enable-eww
                  'entropy/proxy-url--enable-w3m))))


;;;###autoload
(defun entropy/proxy-url-reset-proxy ()
  (interactive)
  (entropy/proxy-url--clean-for-eww)
  (entropy/proxy-url--clean-for-w3m)
  (setq entropy/proxy-url--url-proxy entropy/proxy-mode-url-proxy))



;; * provide
(provide 'entropy-proxy-url)
