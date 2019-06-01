;;; entropy-proxy-mode.el --- A minor mode to toggle proxy.
;; * COPYRIGHT
;; #+BEGIN_EXAMPLE
;; Authors: stardiviner <numbchild@gmail.com>
;; Changed by: Entropy
;; Package-Requires: ((emacs "25"))
;; Package-Version: 20180520.2030
;; Package-X-Original-Version: 0.1
;; Keywords: comm proxy
;; homepage: https://github.com/stardiviner/proxy-mode
;; maintaining: https://github.com/c0001/entropy-proxy-mode
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;;
;; * Commentary:
;;
;; Using global-mode =entropy/proxy-mode= to toggle proxy type for
;; emacs internal proxy tunnel.
;;
;; There's three way for setting proxy client within emacs itself:
;;
;; 1) The uri way of http proxy (built-in)
;; 2) Environment variable 'http_proxy'
;; 3) Socks way powered by =socks.el= (built-in)
;;
;;
;; Setting the individually for customized variables:
;;
;; 1) =entropy/proxy-mode-url-proxy=
;; 2) =entropy/proxy-mode-http-proxy=
;; 3) =entropy/proxy-mode-socks-proxy=
;;
;; See their default value assignment for form defination.
;;
;; * Code:
;; ** require
(require 'url-gw)
(require 'socks)
(require 'cl-lib)

;; ** variable declaration
(defgroup entropy/proxy-mode nil
  "A minor mode to toggle proxy."
  :prefix "entropy/proxy-mode-"
  :group 'proxy)

(defcustom entropy/proxy-mode-rules-alist nil
  "A list of rules for proxy."
  :type 'alist
  :group 'entropy/proxy-mode)

(defvar entropy/proxy-mode--types
  '(("HTTP Proxy" . http) ("Socks Proxy" . socks) ("URL proxy" . url))
  "A list of `entropy/proxy-mode' supported proxy types.")

(defvar entropy/proxy-mode--proxy-type nil
  "Currently enabled proxy type.")

;; Privoxy
(defcustom entropy/proxy-mode-http-proxy "http://localhost:1081"
  "Default HTTP_PROXY environment variable value."
  :type 'string
  :safe #'stringp
  :group 'entropy/proxy-mode)

(defcustom entropy/proxy-mode-url-proxy
  '(("http"  . "127.0.0.1:1081")
    ("https" . "127.0.0.1:1081")
    ("ftp"   . "127.0.0.1:1081")
    ;; don't use `localhost', avoid robe server (For Ruby) can't response.
    ("no_proxy" . "127.0.0.1")
    ("no_proxy" . "^.*\\(baidu\\|sina)\\.com"))
  "A list of rules for `url-proxy-services'."
  :type 'alist
  :safe #'nested-alist-p
  :group 'entropy/proxy-mode)

(defcustom entropy/proxy-mode-socks-proxy '("Default server" "127.0.0.1" 1080 5)
  "Default `socks-server' value."
  :type 'list
  :safe #'listp
  :group 'entropy/proxy-mode)

;; ** libraries
;; *** HTTP Proxy

(defun entropy/proxy-mode--http-enable ()
  "Enable HTTP proxy."
  (make-local-variable 'process-environment)
  ;; TODO: how to `setenv' buffer locally?
  ;; ( "HTTP_PROXY" process-environment)
  (setenv "HTTP_PROXY"  entropy/proxy-mode-http-proxy)
  (setenv "HTTPS_PROXY" entropy/proxy-mode-http-proxy)
  (setq entropy/proxy-mode--proxy-type "http"))

(defun entropy/proxy-mode--http-disable ()
  "Disable HTTP proxy."
  (setenv "HTTP_PROXY" nil)
  (setenv "HTTPS_PROXY" nil)
  (setq entropy/proxy-mode--proxy-type nil))

;; *** URL Proxy

(defun entropy/proxy-mode--url-enable ()
  "Enable URL proxy."
  (setq url-proxy-services entropy/proxy-mode-url-proxy)
  (setq entropy/proxy-mode--proxy-type "url"))

(defun entropy/proxy-mode--url-disable ()
  "Disable URL proxy."
  (setq url-proxy-services nil)
  (setq entropy/proxy-mode--proxy-type nil))

;; *** Socks Proxy

(defun entropy/proxy-mode--socks-enable ()
  "Enable Socks proxy."
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server entropy/proxy-mode-socks-proxy)
  (setq entropy/proxy-mode--proxy-type "socks"))

(defun entropy/proxy-mode--socks-disable ()
  "Disable Socks proxy."
  (setq url-gateway-method 'native)
  (setq entropy/proxy-mode--proxy-type nil))

;; ** Main

;;;###autoload
(defun entropy/proxy-mode-enable (&optional manually)
  "Enable entropy/proxy-mode.

  You can set optional argument MANUALLY with follow preset valid
  value for elisp coding.

  - ‘http’
  - ‘socks’
  - ‘url’
  "
  (interactive)
  (if (not manually)
      (let ((selected (if entropy/proxy-mode--proxy-type
                          (message "entropy/proxy-mode is already enabled.")
                        (cl-case (cdr (assoc
		                       (completing-read "Select proxy service to enable: " (mapcar 'car entropy/proxy-mode--types))
		                       entropy/proxy-mode--types))
                          ('http (entropy/proxy-mode--http-enable))
                          ('socks (entropy/proxy-mode--socks-enable))
                          ('url (entropy/proxy-mode--url-enable))))))
        (message "%s proxy selected." selected))
    (cond
     ((equal manually 'http)
      (entropy/proxy-mode--http-enable))
     ((equal manually 'socks)
      (entropy/proxy-mode--socks-enable))
     ((equal manually 'url)
      (entropy/proxy-mode--url-enable))
     (t
      (entropy/proxy-mode--url-enable)))
    (setq entropy/proxy-mode t)))

;;;###autoload
(defun entropy/proxy-mode-disable ()
  "Disable entropy/proxy-mode."
  (interactive)
  (pcase entropy/proxy-mode--proxy-type
    ("http" (entropy/proxy-mode--http-disable))
    ("url" (entropy/proxy-mode--url-disable))
    ("socks" (entropy/proxy-mode--socks-disable)))
  (setq entropy/proxy-mode nil))

(defvar entropy/proxy-mode-map nil)

;;;###autoload
(define-minor-mode entropy/proxy-mode
  "A globale minor mode to toggle `entropy/proxy-mode'."
  :require 'entropy/proxy-mode
  :init-value nil
  :lighter " Proxy"
  :group 'entropy/proxy-mode
  :keymap entropy/proxy-mode-map
  :global t
  (if entropy/proxy-mode
      (condition-case nil
          (call-interactively 'entropy/proxy-mode-enable)
        (quit (setq entropy/proxy-mode nil)))
    (call-interactively 'entropy/proxy-mode-disable)))


;; * provide
(provide 'entropy-proxy-mode)

;;; entropy-proxy-mode.el ends here
