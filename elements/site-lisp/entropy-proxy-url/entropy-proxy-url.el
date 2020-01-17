;;; entropy-proxy-url ---  Url proxy for emacs eww and w3m
;;
;;; Copyright (C) 20190906  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-proxy-url/
;; Package-Version: v0.1.1
;; Created:       2018
;; Keywords:      proxy
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "26") (cl-lib "0.5") (eww) (w3m "20190830.742"))
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
;;; Commentary:
;;
;; An emacs extension trafficking url matching by proxy regxp rule-set or
;; pull down them into whole proxy tunnel.
;;
;; This package perform as the library role for the functional provision.
;;
;;;; Eww and emacs-w3m proxy stuff
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
;;;; How this package working as?
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
;; =entropy/proxy-url-gfw-regexp-alist= as is.
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
;;;; Methods given
;;
;; We using property list as a =PROXY-RECIPE= to given the customized
;; way for specify the proxy subroutine.
;;
;; As the focurs on, the =PROXY-RECIPE= mainly use ~advice-add~ to
;; around wrappering the target underline functional commands, like
;; ~w3m-goto-url~ , ~url-retrieve~ etc.
;;
;; The =PROXY-RECIPE= slots valid for those listed below:
;;
;; - =:group-name= : a symbol to indicate the recipe name
;;   identification
;;
;; - =:advice-fors= : list of functions for be wrappered with
;;   `entropy/proxy-url` specification
;;
;; - =:type-source= : a symbol restored the proxy type (i.e. which
;;   described in `entropy/proxy-url-initial-typesource')
;;
;; - =:PROXY-MECHANISM= : a symbol indicate the proxy mechanism
;;   (i.e. describe for `entropy/proxy-url-default-proxy-server-obj')
;;
;; - =:server-host-alist= : a symbol indicate the proxy server host
;;   alist which using the same struct with
;;   `entropy/proxy-url-default-proxy-server-obj'"
;;
;; - =:bind= : a alist which the each car of the element was the
;;   key-map and the cdr was the keybinding specific valid as the
;;   form for =kbd= function.
;;
;;;; Proxy reset
;;
;; For those cases that you want to quickly reset the proxy server,
;; just reset your recipes =:server-host-alist= slot's symbol value,
;; all functional form will obtain the new proxy-host value at next
;; time for proxy-connection doing.
;;
;;; Configuration:
;;
;; Just require it, and building =PROXY-RECIPE= you specified.
;;
;;;; Target operation advice
;;
;; There're two built-in =proxy-recipe= i.e. the
;; ~entropy/proxy-url--eww-recipe~ and
;; ~entropy/proxy-url--w3m-recipe~, you can call function
;; ~entropy/proxy-url-make-builtin-recipes~ to buiding
;; them. Futhermore you can specify your own recipe follow what
;; mentioned above, and use function ~entropy/proxy-url-make-recipes~
;; to buiding it(see its doc-string for more details).
;;
;;;; customized varaibles
;;
;; See customized-variable-group ~entropy/proxy-url-group~ for them.
;;
;;;; The regexp rule-set list data
;;
;; Internally, =entropy-proxy-url= has given the sets of regexp rule set
;; tracking by [[https://github.com/gfwlist/gfwlist][github gfw list]] project which maintained the common
;; sensible blocked web domain list directed against to China GFW network
;; ecosystem, however I thought as be compatible for some web transfer
;; chargin area too of that China as the biggest aspect doing for thus.
;; The gfw-rule analyzing provided by [[https://github.com/c0001/entropy-adblockP-rule-analysis][entropy-adbp+-rule-analysis]] package
;; (Add it to ~load-path~ was requested also).
;;
;; By default the rule-set was gained once at the startup, but you
;; can refresh it by calling
;; ~entropy/proxy-url-refresh-gfw-regexp-alist~ at any time for
;; keeping your rule-set updating with upstream.
;;
;;; Change log:

;; - [2020-01-18] bug fix for boundp check for `w3m-command-arguments-alist'

;; - [2020-01-11] *version 0.1.1* release out

;;   Remove force requiring `w3m` at load time excepted that variable
;;   `entropy/proxy-url-force-require-w3m` is nil.

;;   Defined specified key map when proper feature loaded up.

;; - [2018-10-01] *version 0.1.0* release out

;;   First release out.

;;; Code:

(require 'cl-lib)
(require 'eww)
(require 'entropy-common-library)

(declare-function url-retrieve-internal 'url)

(dolist (el '(w3m-goto-mailto-url
              w3m-goto-ftp-url w3m--goto-url--valid-url
              w3m-goto-url w3m-goto-url-new-session
              w3m-goto-url-with-timer))
  (funcall
   `(lambda ()
      (declare-function ,el 'w3m))))
(defvar w3m-command-arguments-alist)

;;;; variable declare
;;;;; customized var
(defgroup entropy/proxy-url-group nil
  "group of `entropy-proxy-url'")

(defcustom entropy/proxy-url-force-require-w3m t
  "Force require w3m without any compability check."
  :type 'boolean
  :group 'entropy/proxy-url-group)

(defvar entropy/proxy-url--w3m-load-effectively
  (or entropy/proxy-url-force-require-w3m
      (let ((status (ignore-errors (require 'w3m))))
        (if (eq status 'w3m)
            t
          nil)))
  "The requiring CBK of `w3m.el', for t as successfully loaded
and nil otherwise.

Original `w3m.el' loading procedure will checking the 'w3m'
binary version and features applied status, thus error will
occurred then there's no valid 'w3m' binary installed in current
platform, this will corrupt `entropy-proxy-url' load procedure,
and this is the meaning for this variable existed.")

(defcustom entropy/proxy-url-default-proxy-server-obj
  '((emacs-socks "" "127.0.0.1" "1080" "5")
    (emacs-url "http://" "127.0.0.1" "1081")
    (shell-http  "http://" "127.0.0.1" "1081")
    (emacs-w3m "http://" "127.0.0.1" "1081"))
  "The default server host object, a list of each element of a list as
one entry.

The order of the sequence of the CARs of the entry were:

1) `proxy-mechanism` : 

   a symbol valid named for `emacs-url` `emacs-socks` `emacs-w3m`
   `shell-http`.
   
   *`emacs-url`* proxy mechanism used for function who use
   `url-retrieve' like underline of package `url.el'

   *`emacs-socks`* proxy mechanism used for the funcion whose
   subroutine based on `socks.el'.

   *`emacs-w3m`* proxy mechanism used for function whose subroutine
   based on `w3m.el'

   *`shell-http'* proxy mechanism used for shell process calling only.

2) `protocal prefix` : string for like \"http://\".

3) `proxy server host domain` : stirng for the proxy host domain
   indication

4) `proxy server port` : string of integer used for `proxy server host
   domain` to redirected proxy request for specified system port.

5) `proxy underline exec version` : string of integar, used such for
   'socks[4]' or 'socks[5]', only effectively in `proxy-mechanism`
   `eq' `emacs-socks`.
"
  :type 'list
  :group 'entropy/proxy-url-group)

(defcustom entropy/proxy-url-gfw-regexp-alist
  '(("google")
    ("wikipedia"))
  "The regexp list for matching url for using proxy.

The structer of this variable format was equal with
`w3m-command-arguments-alist'."
  :type 'sexp
  :group 'entropy/proxy-url-group)

(require 'entropy-proxy-url-gfw-list)

(defcustom entropy/proxy-url--with-proxy-http-sever-host "127.0.0.1:1081"
  "The default http proxy domain string with its port, no
protocal prefix appended. "
  :type 'string
  :group 'entropy/proxy-url-group)

(defcustom entropy/proxy-url--with-proxy-socks-sever-host "127.0.0.1:1080"
  "The default socks proxy domain string with its port, no
protocal prefix appended. "
  :type 'string
  :group 'entropy/proxy-url-group)

(defcustom entropy/proxy-url--with-proxy-no-proxy
  '("localhost"
    "127.0.0.1"
    "192.168.*"
    "10.*")
  "The default \"no_proxy\" regexp list for url-retrieve
`no_proxy` filter."
  :type 'list
  :group 'entropy/proxy-url-group)

(defcustom entropy/proxy-url-initial-typesource 'regexp
  "If none-nil enable proxy for two type:

- symbol ‘regexp’: proxy url matched by regexp of list
  `entropy/proxy-url-gfw-regexp-alist'

- symbol ‘t’: proxy for all searching"
  :type 'symbol
  :group 'entropy/proxy-url-group)

;;;; library
;;;;; core functional
;;;;;; common retrieve recovery
(defun entropy/proxy-url--rec-for-common ()
  (cond
   ((eq major-mode 'w3m-mode)
    (when (and (not (null entropy/proxy-url--w3m-load-effectively))
               (boundp 'w3m-command-arguments-alist)
               (alist-get ".*" w3m-command-arguments-alist nil nil 'equal))
      (setq w3m-command-arguments-alist
            (delete (assoc ".*" w3m-command-arguments-alist)
                    w3m-command-arguments-alist))))))

;;;;;; underline proxy mechanism
(defun entropy/proxy-url--with-proxy-cl-args-body (args)
  "Remove key-value paire from ARGS."
  (let ((it args))
    (catch 'break
      (while t
        (if (keywordp (car it))
            (setq it (cddr it))
          (throw 'break it))))))

(cl-defmacro entropy/proxy-url--with-url-proxy
    (&rest body &key server-host no-proxy &allow-other-keys)
  (declare (indent 0) (debug t))
  (let ((server-host1 (or server-host 'entropy/proxy-url--with-proxy-http-sever-host))
        (no-proxy1 (or no-proxy 'entropy/proxy-url--with-proxy-no-proxy))
        (body1 (entropy/proxy-url--with-proxy-cl-args-body body)))
    `(with-temp-buffer
       (let ((url-proxy-services
              (list (cons "http" ,server-host1)
                    (cons "https" ,server-host1)
                    (cons "ftp" ,server-host1)
                    (cons "no_proxy" (concat "^\\(" (mapconcat 'identity ,no-proxy1 "\\|") "\\)")))))
         ,@body1))))

(cl-defmacro entropy/proxy-url--with-shell-proxy
    (&rest body &key server-host no-proxy version &allow-other-keys)
  (declare (indent 0) (debug t))
  (let ((server-host1 (or server-host '(concat "http://" entropy/proxy-url--with-proxy-http-sever-host)))
        (body1 (entropy/proxy-url--with-proxy-cl-args-body body)))
    `(with-temp-buffer
       (let ((process-environment (cl-copy-list process-environment)))
         (setenv "HTTP_PROXY" ,server-host1)
         (setenv "HTTPS_PROXY" ,server-host1)
         ,@body1))))

(cl-defmacro entropy/proxy-url--with-w3m-proxy
    (&rest body &key server-host no-proxy version &allow-other-keys)
  (declare (indent 0) (debug t))
  (let ((server-host1 (or server-host '(concat "http://" entropy/proxy-url--with-proxy-http-sever-host)))
        (body1 (entropy/proxy-url--with-proxy-cl-args-body body)))
    `(let ()
       (setq
        w3m-command-arguments-alist
        (list
         (list ".*"
               "-o" (concat "http_proxy=" ,server-host1)
               "-o" (concat "https_proxy=" ,server-host1)))
        w3m-no-proxy-domains
        '("127.0.0.1"
          "localhost"))
       ,@body1)))

(cl-defmacro entropy/proxy-url--with-socks-proxy
    (&rest body &key server-host no-proxy version &allow-other-keys)
  (declare (indent 0) (debug t))
  (let ((body1 (entropy/proxy-url--with-proxy-cl-args-body body))
        (server-host1 (or server-host 'entropy/proxy-url--with-proxy-socks-sever-host))
        (no-proxy1 (or no-proxy 'entropy/proxy-url--with-proxy-no-proxy)))
    `(let ((url-gateway-method 'socks)
           (socks-noproxy ,no-proxy1)
           (socks-server ,server-host1))
       ,@body1)))

;;;;;; proxy-mechanism wrapper
(defun entropy/proxy-url--judge-operation (proxy-mechanism)
  (cond ((eq proxy-mechanism 'emacs-url)
         'entropy/proxy-url--with-url-proxy)
        ((eq proxy-mechanism 'emacs-w3m)
         'entropy/proxy-url--with-w3m-proxy)
        ((eq proxy-mechanism 'emacs-socks)
         'entropy/proxy-url--with-socks-proxy)
        ((eq proxy-mechanism 'shell-http)
         'entropy/proxy-url--with-shell-proxy)))

(cl-defmacro entropy/proxy-url--with-proxy (proxy-mechanism server-host-alist &rest body)
  (declare (indent 1) (debug t))
  `(let* ((operation (entropy/proxy-url--judge-operation  ,proxy-mechanism))
          (server-obj (alist-get
                       ,proxy-mechanism
                       (or ,server-host-alist
                           entropy/proxy-url-default-proxy-server-obj)))
          (protocal (car server-obj))
          (domain (cadr server-obj))
          (port (caddr server-obj))
          (version (cadddr server-obj))
          (server-host-str (concat (if (eq ,proxy-mechanism 'emacs-url) ""  protocal)
                                   domain ":" port)))
     (funcall (list 'lambda nil
                    (list operation
                          ':server-host server-host-str
                          ':no-proxy 'entropy/proxy-url--with-proxy-no-proxy ':version version
                          '(progn ,@body))))))

(defun entropy/proxy-url--do-url-proxy
    (url type-source proxy-mechanism server-host-alist browse-func args)
  (let ((gfw-list entropy/proxy-url-gfw-regexp-alist)
        (judge-source (symbol-value type-source))
        judge form)
    (if (eq judge-source t)
        (setq judge t)
      (when (eq judge-source 'regexp)
        (catch :exit
          (dolist (regexp-list gfw-list)
            (when (string-match-p (car regexp-list) url)
              (setq judge t)
              (throw :exit nil))))))
    (if judge
        (funcall `(lambda ()
                    (message "Proxy for url '%s' ..." ',url)
                    (entropy/proxy-url--with-proxy ',proxy-mechanism
                      ',server-host-alist
                      (apply ',browse-func ',url ',args))))
      (funcall `(lambda ()
                  (entropy/proxy-url--rec-for-common)
                  (apply ',browse-func ',url ',args))))))

(defun entropy/proxy-url--proxy-wrapper
    (advice-for type-source proxy-mechanism server-host-alist &optional remove-advice)
  (let ((func-name (intern
                    (concat "entropy/proxy-url-"
                            (symbol-name advice-for)
                            "-around-advice")))
        form)
    (setq form
          (if remove-advice
              `(advice-remove ',advice-for #',func-name)
            `(progn
               (defun ,func-name (orig-func &rest orig-args)
                 (entropy/proxy-url--do-url-proxy
                  (car orig-args) ',type-source ',proxy-mechanism
                  ',server-host-alist
                  orig-func
                  (cdr orig-args)))
               (advice-add ',advice-for :around #',func-name))))
    (funcall `(lambda () ,form))))

(defmacro entropy/proxy-url--batch-proxy-wrapper
    (group-name advice-fors type-source proxy-mechanism server-host-alist)
  `(progn
     (defun ,(intern (concat "entropy/proxy-url-advice-for-group-of-" (symbol-name group-name)))
         (&optional prefix)
       (interactive "P")
       (dolist (item ,advice-fors)
         (entropy/proxy-url--proxy-wrapper
          item ,type-source ,proxy-mechanism ,server-host-alist prefix)))
     (funcall ',(intern (concat "entropy/proxy-url-advice-for-group-of-" (symbol-name group-name))))))

;;;;;; type source modification
(defmacro entropy/proxy-url--swith-form (type-source)
  "Macro for creating proxy type throung the way of setting each of
`entropy/proxy-url-typesource-for-eww' and `entropy/proxy-url-typesource-for-w3m'."
  `(let ((choice (intern (completing-read (format "Choose proxy method for [%s]: "
                                                  (symbol-name (1value ,type-source)))
                                          '("regexp" "t" "nil")))))
     (setf (symbol-value ,type-source) choice)))

;;;;; recipes

(defvar entropy/proxy-url-typesource-for-eww entropy/proxy-url-initial-typesource)
(defvar entropy/proxy-url-typesource-for-w3m entropy/proxy-url-initial-typesource)

(defvar entropy/proxy-url--w3m-recipe
  '(:group-name
    w3m-group
    :advice-fors
    (w3m-goto-mailto-url
     w3m-goto-ftp-url w3m--goto-url--valid-url
     w3m-goto-url w3m-goto-url-new-session w3m-goto-url-with-timer)
    :type-source
    entropy/proxy-url-typesource-for-w3m
    :proxy-mechanism
    emacs-w3m
    :server-host-alist
    nil
    :bind
    ((w3m) . ((w3m-mode-map . "p")))))

(defvar entropy/proxy-url--eww-recipe
  '(:group-name
    eww-group
    :advice-fors
    (url-retrieve-internal)
    :type-source
    entropy/proxy-url-typesource-for-eww
    :proxy-mechanism
    emacs-url
    :server-host-alist
    nil
    :bind
    ((eww) . ((eww-mode-map . "p")))))

;;;###autoload
(defun entropy/proxy-url-make-recipes (proxy-recipes)
  "Expand proxy-sepcification to its defination, grouped as RECIPE. 

The recipe was one plist.

Recipe slots:

- `:group-name`        : a symbol to indicate the recipe name identification

- `:advice-fors`       : list of functions for be wrappered with
                         `entropy/proxy-url` specification

- `:type-source`       : a symbol restored the proxy type (i.e. which
                         described in `entropy/proxy-url-initial-typesource')

- `:proxy-mechanism`   : a symbol indicate the proxy mechanism
                         (i.e. describe for `entropy/proxy-url-default-proxy-server-obj')

- `:server-host-alist` : a symbol indicate the proxy server host alist
                         which using the same struct with `entropy/proxy-url-default-proxy-server-obj'

- `:bind`              : a cons for the car of a list of feature to lazy load
                         and the cdr of a alist for keymap and keybind stroke
                         specification, keybind-key form as function `kbd' does.
"
  (let ((eval-binds))
    (dolist (proxy-recipe proxy-recipes)
      (let (wrapper-form
            switch-form
            (switch-func-name
             (intern
              (concat
               "entropy/proxy-url-switch-proxy-for-"
               (symbol-name (plist-get proxy-recipe :group-name)))))
            (bind (plist-get proxy-recipe :bind)))
        (setq wrapper-form
              `(entropy/proxy-url--batch-proxy-wrapper
                ,(plist-get proxy-recipe :group-name)
                ',(plist-get proxy-recipe :advice-fors)
                ',(plist-get proxy-recipe :type-source)
                ',(plist-get proxy-recipe :proxy-mechanism)
                ,(plist-get proxy-recipe :server-host-alist))
              
              switch-form
              `(defun ,switch-func-name ()
                 (interactive)
                 (entropy/proxy-url--swith-form ',(plist-get proxy-recipe :type-source))))
        (funcall `(lambda () ,wrapper-form))
        (funcall `(lambda () ,switch-form))
        (let* ((features (car bind))
               (key-binds (cdr bind))
               (form '(eval-after-load feature))
               (replace (mapcar (lambda (x) (list `(1 ',x))) features))
               (body
                `((lambda ()
                    (dolist (bind-form ',key-binds)
                      (define-key (eval (car bind-form)) (kbd (eval (cdr bind-form)))
                        #',switch-func-name)))))
               (macro (entropy/cl-gen-nested-append-form
                       form replace body t)))
          (add-to-list 'eval-binds macro)))
      (dolist (item eval-binds)
        (eval item)))))

;;;###autoload
(defun entropy/proxy-url-make-builtin-recipes ()
  (let ((proxy-recipe (if (not (null entropy/proxy-url--w3m-load-effectively))
                          (list entropy/proxy-url--w3m-recipe
                                entropy/proxy-url--eww-recipe)
                        (list entropy/proxy-url--eww-recipe))))
    (entropy/proxy-url-make-recipes proxy-recipe)))

;;; provide
(provide 'entropy-proxy-url)
