;;; TODO entropy-emacs-gnus.el --- Mail configuration for entropy-emacs  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20251005  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-mail.el
;; Keywords:      gnus, news
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
;; Mail configuration for =entropy-emacs=.
;;
;; * Configuration:
;;
;; Using for =entropy-emacs= only.
;;
;; * Code:
;; ** require

;; ** meta
;; *** Email
;; **** class
(defclass eemacs//email-meta/class/user ()
  ((account-name :initarg :account-name :type string
                 :initform (symbol-value 'entropy/emacs-user-mail-address))
   (mail-address :initarg :mail-address :type string
                 :initform (symbol-value 'entropy/emacs-user-mail-address))))
(defclass eemacs//email-meta/class/protocol/sub ()
  ((domain    :initarg :domain    :type string :initform "")
   (protocol  :initarg :protocol  :type string :initform "")
   (port      :initarg :protocol  :type number :initform 0)
   (security  :initarg :security  :type string :initform "")
   (use-proxy :initarg :use-proxy :initform nil)))
(defclass eemacs//email-meta/class/protocol ()
  ((recieve     :initarg  :recieve     :type eemacs//email-meta/class/protocol/sub
                :initform (make-instance 'eemacs//email-meta/class/protocol/sub))
   (sending     :initarg  :recieve     :type eemacs//email-meta/class/protocol/sub
                :initform (make-instance 'eemacs//email-meta/class/protocol/sub))))
(defclass eemacs//email-meta/class/server ()
  ((name        :intiarg  :name        :type string :initform "")
   (domain-regexp :initarg  :domain-regexp :type string :initform "")
   (protocol    :initarg  :protocol    :type eemacs//email-meta/class/protocol
                :initform (make-instance 'eemacs//email-meta/class/protocol))))
(defclass eemacs//email-meta/class ()
  ((user        :initarg  :user        :type eemacs//email-meta/class/user
                :initform (make-instance 'eemacs//email-meta/class/user))
   (server      :initarag :server      :type eemacs//email-meta/class/server
                :initform (make-instance 'eemacs//email-meta/class/server))))

;; **** server register
(defvar eemacs//email-server-alist
  (list
   (eemacs-oset-batch-group (make-instance 'eemacs//email-meta/class/server)
     ("Gmail" :name)
     ("gmail\\.com$" :domain-regexp)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "imap.gmail.com"
        :protocol "IMAP"
        :port 993
        :security "TLS"
        :use-proxy t) :protocol :recieve)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "smtp.gmail.com"
        :protocol "SMTP"
        :port 587
        :security "STARTTLS"
        :use-proxy t) :protocol :sending))
   (eemacs-oset-batch-group (make-instance 'eemacs//email-meta/class/server)
     ("Outlook" :name)
     ("\\(outlook\\|hotmail\\)\\.com$" :domain-regexp)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "outlook.office365.com"
        :protocol "IMAP"
        :port 993
        :security "TLS") :protocol :recieve)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "smtp-mail.outlook.com"
        :protocol "SMTP"
        :port 587
        :security "STARTTLS") :protocol :sending))
   (eemacs-oset-batch-group (make-instance 'eemacs//email-meta/class/server)
     ("163" :name)
     ("163\\.com$" :domain-regexp)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "imap.163.com"
        :protocol "IMAP"
        :port 993
        :security "SSL") :protocol :recieve)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "smtp.163.com"
        :protocol "SMTP"
        :port 465
        :security "SSL") :protocol :sending))
   (eemacs-oset-batch-group (make-instance 'eemacs//email-meta/class/server)
     ("QQ" :name)
     ("qq\\.com$" :domain-regexp)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "imap.qq.com"
        :protocol "IMAP"
        :port 993
        :security "SSL") :protocol :recieve)
     ((eemacs-make-instance 'eemacs//email-meta/class/protocol/sub
        :domain "smtp.qq.com"
        :protocol "SMTP"
        :port 587
        :security "SSL") :protocol :sending))
   ))

;; **** main
;; ***** methods
(defvar eemacs//email-meta/assoc-cache nil
  "The alist of all defined `eemacs//email-meta/class' objects identify by
ID (symbol or string), all of those should be made by
`eemacs//email-meta/method/class/new'.")

(defun eemacs//email-meta/method/get-server-name-from-mail-addr (mail-addr)
  (when-let* (((stringp mail-addr))
              (sym (make-symbol "rtn"))
              (rtn sym))
    (catch :rtn
      (dolist (obj eemacs//email-server-alist)
        (when-let* ((srv-dm-regexp (eemacs-oref obj :domain-regexp))
                    (srv-name (eemacs-oref obj :name)))
          (when (string-match-p (concat "@" srv-dm-regexp) mail-addr)
            (throw :rtn (setq rtn srv-name)))))
      (if (not (eq rtn sym)) rtn
        (entropy/emacs-!error-as-eemacs-internal-error
         "No server name matched for email address '%s'" mail-addr)))))

(cl-defmacro eemacs//email-meta/method/class/new
    (&optional
     id
     &key
     user/account-name
     user/mail-address
     server/name)
  (declare (indent 1))
  (macroexp-let2* ignore
      ((id `(or ,id 'default))
       (user/account-name
        `(or ,user/account-name entropy/emacs-user-mail-address))
       (user/mail-address
        `(or ,user/mail-address entropy/emacs-user-mail-address))
       (obj '(make-instance 'eemacs//email-meta/class))
       (srvnm `(or ,server/name (eemacs//email-meta/method/get-server-name-from-mail-addr
                                 ,user/mail-address))))
    `(let (_)
       (eemacs-oset-batch-group ,obj
         (,user/account-name :user :account-name)
         (,user/mail-address :user :mail-address)
         ((catch :exit
            (dolist (el eemacs//email-server-alist)
              (and (string= (eemacs-oref el :name) ,srvnm)
                   (throw :exit el)))
            (entropy/emacs-error-without-debugger
             "No mail server found for name `%s'" ,srvnm))
          :server))
       (setq eemacs//email-meta/assoc-cache
             (entropy/emacs-alist-set ,id
                 eemacs//email-meta/assoc-cache
               ,obj)))))

;; ***** dispatchers
;; ****** generic
(defvar eemacs//email-meta/var/current-selected-object-id nil)
(cl-defgeneric eemacs//email-meta/method/dispatch (id type)
  "Cat a `eemacs//email-meta/class' object's contents and dispatch them
into a email client which is distinguished by TYPE a symbol.

ID is a object id in `eemacs//email-meta/assoc-cache'.")

(cl-defmethod eemacs//email-meta/method/dispatch :before (id _)
  (setq eemacs//email-meta/var/current-selected-object-id id))

;; ****** instance
;; ******* (emacs bultin) default
(eval-when-compile (require 'smtpmail))
(cl-defmethod  eemacs//email-meta/method/dispatch (id (_ (eql default)))
  "Dispatch a `eemacs//email-meta/class' object into emacs builtin mail client."
  (let ((obj (alist-get id eemacs//email-meta/assoc-cache nil nil 'equal))
        obj/sending/prt)
    (cl-assert (eemacs//email-meta/class-p obj))
    (setq user-full-name (eemacs-oref-batch obj :user :account-name)
          user-mail-address (eemacs-oref-batch obj :user :mail-address))
    (setq obj/sending/prt (eemacs-oref-batch obj :server :protocol :sending :protocol))
    (cond ((string= obj/sending/prt "SMTP")
           (entropy/emacs-require-only-once 'smtpmail)
           (setq smtpmail-smtp-user user-full-name
                 smtpmail-mail-address user-mail-address
                 smtpmail-smtp-server (eemacs-oref-batch obj :server :protocol :sending :domain)
                 smtpmail-default-smtp-server smtpmail-smtp-server
                 smtpmail-smtp-service (eemacs-oref-batch obj :server :protocol :sending :port)
                 smtpmail-stream-type
                 (let ((val (eemacs-oref-batch obj :server :protocol :sending :security)))
                   (cond
                    ((string= "STARTTLS" val) 'starttls)
                    ((string= "TLS" val) 'tls)
                    ((string= "SSL" val) 'ssl)
                    ((string= "PLAIN" val) 'plain)
                    (t (entropy/emacs-error-without-debugger "")))))
           (setq send-mail-function 'smtpmail-send-it)
           (unless (memq 'xoauth2 smtpmail-auth-supported)
             (setq smtpmail-auth-supported
                   (append smtpmail-auth-supported '(xoauth2)))))
          (t (error "\
emacs builtin mail client just support SMTP sending protocol, while current is %s"
                    obj/sending/prt)))
    (setq auth-source-debug t)
    (auth-source-forget-all-cached)))

;; ** init

(entropy/emacs-add-hook-with-lambda (cons t 'eeamcs//email-meta/init/default-user-mail)
  (&rest _)
  "Initial default `eemacs//email-meta/class' for `user-mail-address' after
eemacs startup done."
  :use-append t
  :use-hook 'entropy/emacs-after-startup-idle-hook
  (eemacs//email-meta/method/class/new 'default)
  (eemacs//email-meta/method/dispatch 'default 'default))

;; * provide
(provide 'entropy-emacs-mail)
