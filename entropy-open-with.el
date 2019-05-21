;;* File name: entropy-open-with.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;; 
;;; Commentary
;; 
;; This package was used to let you can open file in specific apps both in
;; windows system or unix like system.
;; 
;; 
;; You can specific the extension list like:
;; 
;; #+BEGIN_SRC elisp
;;   '((("html" "pdf" "xml" "php" "md" "markdown")
;;      "a:/PortableApps/FirefoxPortable/FirefoxPortable.exe" file "-new-tab")
;;     (("mp3" "mp4" "mkv" "rmvb" "wmv" "flv" "avi")
;;      "a:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe" quote nil)
;;     (("c")
;;      "a:/PortableApps/codeblocks/bin/codeblocks.exe" quote nil))
;; #+END_SRC
;; 
;; More details see below function definition.
;; 
;; * Code:


(defgroup entropy/open-with nil
  "Variable group for entropy-open-with"
  :group 'extensions)

(defcustom entropy/open-with-type-list nil
  "entropy/open-with type list

This list construct was structed as below:

#+BEGIN_SRC elisp
  '(((\"html\" \"pdf\" \"xml\" \"php\" \"md\" \"markdown\")
     \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\" file \"-new-tab\")
    ((\"mp3\" \"mp4\" \"mkv\" \"rmvb\" \"wmv\" \"flv\" \"avi\")
     \"a:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe\" quote nil)
    ((\"c\")
     \"a:/PortableApps/codeblocks/bin/codeblocks.exe\" quote nil))
#+END_SRC

Each element of it calls one _open-with_ group, the group sepcific
for sets of file type which can open with one specified
trigger(process command form).

There's three element for one group of this list
- List of extention name regexp
- Portable programe path
- Path argument format method (see blow for details)
- Executing parameter 

*Note:*
#+BEGIN_QUOTE
 extension name regexp can not format with abbrev dot like
\"\\.pdf\", using pure extension name regexp instead, thus in
this case 'pdf' extension as \"pdf\" regexp specific.
#+END_QUOTE

Like above (the first /open-with/ group):

- (\"html\" \"pdf\" \"xml\" \"php\" \"md\" \"markdown\") was the
list of extention name regexp which I should let them be opened
with portable firefox.

- \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\" was the
  portable firefox path.

- 'file  was the file path transferring type, in this cases
  symbole =file= will hexify the file path and given it the
  =file:///=  uri protocol.

- none

The gather of above elements was called one group setting for
`entropy-open-with'.

The general list tpye as:
( group1---> firefox open \"html php xml txt mardown\"
  group2---> mphc open media type file like \"mp4 mp3 aac ...\"
  gourp3 ....
  ......)

*Path transfer format method:*

1) 'file' method:

   Most of web browser can not use the local file path string as
   the file url argument in case of that the path string contained
   non-ascii characters such as symbol or cjk as for. In this case,
   using file protocol hexified string  as argument is wise choice.

   This transform firstly hexifies the whole local file path string
   and then adding the \"file:///\" protocol prefix string ahead of
   it.

2) 'quote' method:

   It's the most common decoration for commanline arguments, using
   double quotation char pair.

3) nil, passing origin path string to process.

4) Arbitrary function self specified, requesting one arg, the
   file path.

*Arguumet:*

Argumet is the concated command arguments string, the single
string type.
"
  :type 'sexp
  :group 'entropy/open-with)


(defvar entropy/open-with--type-list-full nil
  "Full list for `entropy/open-with-match-open' which gened by
  `entropy/open-with-type-list'")

(defvar entropy/open-with-url-regexp "\\(^https?\\|^s?ftp\\)://"
  "The Regexp string for matching url link.")

(defun entropy/open-with--do-list ()
  "Expand `entropy/open-with-type-list' to `entropy/open-with--type-list-full'.

Append 'ext-regexp', 'exec', 'path-transform' and 'parameter' to
each file suffix 'ext', like:

((\"html\" \"filefox.exe\" 'file \"-new-tab\")
 (\"pdf\" \"filefox.exe\" 'file \"-new-tab\")
 (\"xml\" \"filefox.exe\" 'file \"-new-tab\")
 ....)"
  (setq entropy/open-with--type-list-full nil)
  (if (not entropy/open-with-type-list)
      (error "entropy/open-with-type-list was empty!"))
  (dolist (group entropy/open-with-type-list)
    (let* ((ext-list (car group))
           (exec (nth 1 group))
           (path-transform (nth 2 group))
           (parameter (nth 3 group)))
      (dolist (ext ext-list)
        (let (new-group-e)
          (add-to-list 'new-group-e parameter)
          (add-to-list 'new-group-e path-transform)
          (add-to-list 'new-group-e exec)
          (add-to-list 'new-group-e ext)
          (add-to-list 'entropy/open-with--type-list-full new-group-e))))))

(defun entropy/open-with--judge-file-type (filename)
  "Judge the FILENAME type according to
`entropy/open-with-type-list' and return a plist cotained final
state of file description for `entropy/open-with-match-open'.

If not matched the exist file type then return nil."

  (let ((type-list (progn (entropy/open-with--do-list)
                          entropy/open-with--type-list-full))
        (file-path (expand-file-name filename))
        path-arg rtn)
    (catch :exit
      (dolist (type type-list)
        (when (string-match (concat "\\." (car type) "$") file-path)
          (let ((exec (nth 1 type))
                (path-transform (nth 2 type))
                (parameter (nth 3 type)))
            (cond ((eq 'file path-transform)
                   (cond ((eq system-type 'windows-nt)
                          (let ((path-formated (replace-regexp-in-string "^\\(.\\):/" "\\1/" file-path)))
                            (setq path-arg (concat "file:///" path-formated))))))
                  ((eq 'quote path-transform)
                   (setq path-arg (shell-quote-argument file-path)))
                  ((functionp path-transform)
                   (setq path-arg (funcall path-transform file-path)))
                  (t file-path))
            (setq rtn (list :caller exec :path-arg path-arg
                            :parameter parameter  :w32-arg (concat parameter " " path-arg)
                            :file file-path)))
          (throw :exit nil))))
    rtn))

(defun entropy/open-with--process-default-dir ()
  "Get the system temp directory.

This func was used for temporally reset `default-directory' which
will be the current process handle which will protect it's handle
associated file location be deleted by file removing operaion,
this be for redirected the `default-directory' for each subprocess
procedure who don't want to be as the state as what."
  (cl-case system-type
    (windows-nt
     (getenv "TEMP"))
    (t
     "/tmp")))

(defun entropy/open-with-match-open (files)
  "This function be used for `entropy/open-with-dired-open' and
`entropy/open-with-buffer' to locally open file with sepcific
caller with args specification.

It's core function was to match the ARG files name extension with
`entropy/open-with-type-list' and used the previous function
`entropy/open-with--do-list' to convert the readable list to full
pair list `entropy/open-with--type-list-full'.

*Note:*

ARG--> files: 

    It's name coding type must be consistency with system local
    like \"Chiese-GBK\" or \"ASCII\" and mixtures both of them
    when you are in windows platform without unicode code page
    global support. You could using WIN10 beta optional for utf-8
    defaultly with WIN10 version upper than 1803.

The subprocess for calling with for associated file will not use
`default-dirctory' be the process handle location, using
`entropy/open-with--process-default-dir' to redirected to system
temp dir for preventing some file deleting operation conflicted
occurrence.
"
  (dolist (el files)
    (let (($dowith (entropy/open-with--judge-file-type el))
          (default-directory (entropy/open-with--process-default-dir)))
      (if $dowith
          (if (string-equal system-type "windows-nt")
              (w32-shell-execute "open" (plist-get $dowith :caller) (plist-get $dowith :w32-arg))
            (cond
             ((string-equal system-type "gnu/linux")
              (start-process "" nil "xdg-open" (plist-get $dowith :file)))
             ((string-equal system-type "darwin")
              (shell-command (concat "open" (shell-quote-argument (plist-get $dowith :file)))))
             (t (message "Unsupported operation system type <-- %s -->." system-type))))
        (message "Unknown file type!")))))


(defun entropy/open-with-port (interact &optional filename inemacs)
  "Open with file by accepted the FILENAME arg.

When arg INTERACTIVE was none-nil. If FILENAME was not existed
then prompt for retrying inputting FILENAME.(FILENAME inputting
support ivy-completion) 

When without interactived option chosen, then directly open the
FILENAME, if FILENAME was not exist then return error.

When inemacs was non-nil open FILENAME in emacs.

Tips: none-interactive state support open url.

Note: this func redirected `default-directory' as
`entropy/open-with-match-open' does so."
  (if interact
      (let ((accept (ivy-read "Please input FILENAME or insert url: " 'read-file-name-internal
                              :history 'file-name-history)))
        (if (file-exists-p accept)
            (cond
             ((file-directory-p accept)
              (find-file accept))
             ((file-exists-p accept)
              (entropy/open-with-match-open (list accept))))
          (let ((redop (yes-or-no-p "File not exist! Do you want to redoing? ")))
            (if redop
                (entropy/open-with-port t)
              (error "Abort operation.")))))
    (if filename
        (if (or (and (file-exists-p filename)
                     (not (string= filename "")))
                (string-match-p entropy/open-with-url-regexp filename))
            (cond
             ((file-directory-p filename)
              (find-file filename))
             ((string-match-p entropy/open-with-url-regexp filename)
              (let ((default-directory (entropy/open-with--process-default-dir)))
                (browse-url filename)))
             ((file-exists-p filename)
              (if inemacs
                  (find-file filename)
                (entropy/open-with-match-open (list filename)))))
          (user-error "File %s not exist!" filename))
      (user-error "Invalid filename."))))



;;;###autoload
(defun entropy/open-with-interactively ()
  "Interactively calling `entropy/open-with-port'"
  (interactive)
  (entropy/open-with-port t))


;;;###autoload
(defun entropy/open-with-dired-open ()
  "Dired open with portable apps.

It's core function is `entropy/open-with-match-open', see it for
details.
"
  (interactive)
  (if (equal major-mode 'dired-mode)
      (let ((files (dired-get-marked-files)))
        (entropy/open-with-match-open files))
    (user-error "You are not in dired-mode!")))

;;;###autoload
(defun entropy/open-with-buffer ()
  "Buffer open with portable apps.

It's core function is `entropy/open-with-match-open', see it for
details."
  (interactive)
  (let ((tname (file-truename (buffer-file-name))))
    (entropy/open-with-match-open (list tname))))

(provide 'entropy-open-with)


