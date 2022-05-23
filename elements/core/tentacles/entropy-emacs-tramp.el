;;; entropy-emacs-tramp.el --- entropy-emacs tramp configuratin  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-tramp.el
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
;; `entropy-emacs' self-built tramp wrapper package, used for
;; cross-platform.
;;
;; For WIN32 platform using [[https://www.putty.org/][putty]] utilities, and using native
;; tool-chian for *NIX environment.
;;
;; * Configuration:
;;
;; This file can be used for individual way. Just requiring it in
;; your `load-path'.
;;
;; * Code:

;; ** require

(require 'tramp)

;; ** library
(defun entropy/emacs-tramp--get-ssh-config ()
  "Get ssh config for extracting host candidates using func
`entropy/emacs-tramp--list-ssh-config-group'."
  (let ((file (if (file-exists-p "~/.ssh/config")
                  "~/.ssh/config"
                (error "None ssh config file searched!")))
        content
        groups
        rtn)
    (setq content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
    (setq groups (split-string content "^$"))
    (dolist (el groups)
      (when (not (string= "" el))
        (push (entropy/emacs-tramp--list-ssh-config-group el) rtn)))
    rtn))

(defun entropy/emacs-tramp--list-ssh-config-group (group)
  "Usdng func `entropy/emacs-tramp--tidy-ssh-config-source' for extracting the
substring get from splitting with '\n' from group GROUP.

GROUP is the substring get from ssh config file and splitting by
'^$'."
  (let* ((source (split-string group "\n" t))
         (source-tidy (entropy/emacs-tramp--tidy-ssh-config-source source)))
    source-tidy))

(defun entropy/emacs-tramp--tidy-ssh-config-source (source)
    "Make alist from group GROUP which are the string contained ssh
config group info as:

==========
host test
hostname xxx.xxx.xxx.xxx
user test
port 22
==========

This func divided this string into the return list as:
'((\"host\" \"test\")
  (\"hostname\" \"xxx.xxx.xxx.xxx\")
  (\"user\" \"test\")
  (\"port\" \"22\"))
"
  (let* (key value rtn)
    (dolist (el source)
      (cond
       ((string-match "\\([Hh]ost\\)[ \t]+?\\(\\(\\w\\|\\.\\|-\\|_\\)+\\)" el)
        (setq key (match-string 1 el)
              value (match-string 2 el))
        (push (list key value) rtn))
       ((string-match "\\([Hh]ost[Nn]ame\\)[ \t]+?\\(\\(\\w\\|\\.\\|-\\|_\\)+\\)" el)
        (setq key (match-string 1 el)
              value (match-string 2 el))
        (push (list key value) rtn))
       ((string-match "\\([Pp]ort\\)[ \t]+?\\(\\(\\w\\|\\.\\|-\\|_\\)+\\)" el)
        (setq key (match-string 1 el)
              value (match-string 2 el))
        (push (list key value) rtn))
       ((string-match "\\([Uu]ser\\)[ \t]+?\\(\\(\\w\\|\\.\\|-\\|_\\)+\\)" el)
        (setq key (match-string 1 el)
              value (match-string 2 el))
        (push (list key value) rtn))
       (t nil)))
    rtn))


;; ** dispater
(defun entropy/emacs-tramp--query-ssh-config-groups-list ()
  "Create host list for the query candidates list used for
`entropy/emacs-tramp--query-chosen-open'. "
  (let ((source-tidy (entropy/emacs-tramp--get-ssh-config))
        host-name
        rtn)
    (dolist (el source-tidy)
      (setq host-name (nth 1 (or (assoc "host" el)
                                 (assoc "Host" el))))
      (cond
       ((and host-name (stringp host-name))
        (push (list host-name el) rtn))
       (t nil)))
    rtn))

(defun entropy/emacs-tramp--gen-ssh-config-group-fflink (group)
  "Generate tramp link used for
`entropy/emacs-tramp--query-chosen-open'."
  (require 'tramp)
  (let ((host-address (nth 1 (or (assoc "hostname" group)
                                 (assoc "HostName" group)
                                 (assoc "Hostname" group)
                                 (assoc "hostName" group))))
        (port (nth 1 (or (assoc "port" group)
                         (assoc "Port" group))))
        (user (nth 1 (or (assoc "user" group)
                         (assoc "User" group))))
        link tramp-method)
    (if (not host-address)
        (error "No address found!"))
    (setq tramp-method
          (cond (sys/win32p
                 (completing-read "choose tramp method: "
                                  '("pscp" "plink" "ssh")))
                (t "ssh")))
    (cond
     ((and port user)
      (setq link (concat "/" tramp-method ":" user "@"
                         host-address
                         "#" port ":/")))
     (port
      (setq link (concat "/" tramp-method ":" host-address "#" port ":/")))
     (user
      (setq link (concat "/" tramp-method ":" user "@" host-address ":/"))))
    (find-file link)))


(defun entropy/emacs-tramp-query-ssh-config-groups-chosen-open ()
  "The dispatcher for tramp retrieving of entropy-emacs."
  (interactive)
  (ivy-read "xxx: " (entropy/emacs-tramp--query-ssh-config-groups-list)
            :require-match t
            :action (lambda (arg) (entropy/emacs-tramp--gen-ssh-config-group-fflink
                                   (nth 1 arg)))))

(defun entropy/emacs-tramp-clean-all ()
  "Clean all tramp connections and refer buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  (message "Clean up all tramp refers."))

(entropy/emacs-lazy-initial-for-hook
 (entropy/emacs-after-startup-hook)
 "eemacs-tramp-hydra-hollow-init"
 "eemacs-tramp-hydra-hollow-init"
 prompt-echo
 :pdumper-no-end t
 (entropy/emacs-hydra-hollow-add-for-top-dispatch
  '("Tramp"
    (("C-c s f" entropy/emacs-sudoedit-current-path-maybe
      "Get sudo privileges for CURR-PATH if need to so."
      :enable t :exit t :global-bind t)
     ("C-c s t" entropy/emacs-tramp-query-ssh-config-groups-chosen-open
      "The dispatcher for tramp retrieving of entropy-emacs"
      :enable t :exit t :global-bind t)
     ("C-c s c" tramp-cleanup-this-connection
      "Clean the current tramp session and refer buffers"
      :enable t :exit t :global-bind t)
     ("C-c s a" entropy/emacs-tramp-clean-all
      "Clean all tramp connections and refer buffers"
      :enable t :exit t :global-bind t)))))

(provide 'entropy-emacs-tramp)
