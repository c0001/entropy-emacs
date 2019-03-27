;;* File name: entropy-open-with.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary

;; This package was used to let you can open file in specific apps both in
;; windows system or unix like system.

;; Now bug is about can not support unicode file name off-limit in
;; system local setting like in chinese local set on windows system as
;; it can not open the filename which cotain Korea charater.
;;
;; This bug can be answered by gnu mailing list
;; https://lists.gnu.org/archive/html/emacs-devel/2016-01/msg00406.html
;; The main description as:
;; > From: Klaus-Dieter Bauer <address@hidden>
;; > Date: Wed, 6 Jan 2016 16:20:29 +0100
;; > 
;; > Is there a reliable way to pass unicode file names as
;; > arguments through `start-process'?
;;
;; No, not at the moment, not in the native Windows build of Emacs.
;; Arguments to subprocesses are forced to be encoded in the current
;; system codepage.  This commentary in w32.c tells a few more details:
;;
;;    . Running subprocesses in non-ASCII directories and with non-ASCII
;;      file arguments is limited to the current codepage (even though
;;      Emacs is perfectly capable of finding an executable program file
;;      in a directory whose name cannot be encoded in the current
;;      codepage).  This is because the command-line arguments are
;;      encoded _before_ they get to the w32-specific level, and the
;;      encoding is not known in advance (it doesn't have to be the
;;      current ANSI codepage), so w32proc.c functions cannot re-encode
;;      them in UTF-16.  This should be fixed, but will also require
;;      changes in cmdproxy.  The current limitation is not terribly bad
;;      anyway, since very few, if any, Windows console programs that are
;;      likely to be invoked by Emacs support UTF-16 encoded command
;;      lines.
;;
;;    . For similar reasons, server.el and emacsclient are also limited
;;      to the current ANSI codepage for now.
;;
;;    . Emacs itself can only handle command-line arguments encoded in
;;      the current codepage.
;;
;; The main reason for this being a low-priority problem is that the
;; absolute majority of console programs Emacs might invoke don't support
;; UTF-16 encoded command-line arguments anyway, so the efforts to enable
;; this would yield very little gains.  However, patches to do that will
;; be welcome.  (Note that, as the comment above says, the changes will
;; also need to touch cmdproxy, since we invoke all the programs through
;; it.)
;;
;; > I realized two limitations:
;; > 
;; > 1. Using `prefer-coding-system' with anything other than
;; > `locale-default-encoding', e.g. 
;; > (prefer-coding-system 'utf-8), 
;; > causes a file name "Ö.txt" to be misdecoded as by
;; > subprocesses -- notably including "emacs.exe", but also
;; > all other executables I tried (both Windows builtins like
;; > where.exe and third party executables like ffmpeg.exe or
;; > GnuWin32 utilities). 
;; > In my case (German locale, 'utf-8 preferred coding
;; > system) it is mis-decoded as "Ã–.txt", i.e. emacs encodes
;; > the process argument as 'utf-8 but the subprocess decodes
;; > it as 'latin-1 (in my case).
;; > While this can be fixed by an explicit encoding 
;; > (start-process ... 
;; > (encode-coding-string filename locale-coding-system))
;; > such code will probably not be used in most projects, as
;; > the issue occurs only on Windows, dependent on the user
;; > configuration (-> hard-to-find bug?). I have added some
;; > elisp for demonstration at the end of the mail.
;; > 
;; > 2. When a file-name contains characters that cannot be
;; > encoded in the locale's encoding, e.g. Japanese
;; > characters in a German locale, I cannot find any way to
;; > pass the file name through the `start-process' interface; 
;; > Unlike for characters, that are supported by the locale, 
;; > it fails even in a clean "emacs -Q" session. 
;; > Curiously the file name can still be used in cmd.exe,
;; > though entering it may require TAB-completion (even
;; > though the active codepage shouldn't support them).
;;
;; Does the program which you invoke support UTF-16 encoded command-line
;; arguments?  It would need to either use '_wmain' instead of 'main', or
;; access the command-line arguments via GetCommandLineW or such likes,
;; and process them as wchar_t strings.
;;
;; If the program doesn't have these capabilities, it won't help that
;; Emacs passes it UTF-16 encoded arguments, because Windows will attempt
;; to convert them to strings encoded in the current codepage, and will
;; replace any un-encodable characters with question marks or blanks.
;;
;; > ;; Set the preferred coding system. 
;; > (prefer-coding-system 'utf-8)
;;
;; You cannot use UTF-8 to encode command-line arguments on Windows, not
;; in general, even if the program you invoke does understand UTF-8
;; strings as its command-line arguments.  (I can explain if you want.)
;;
;; > ;; On Unix (tested with cygwin), it works fine; Presumably because
;; > ;; the file name is decoded (in `directory-files') and encoded (in
;; > ;; `start-process') with the same preferred coding system.
;;
;; It works with Cygwin because Cygwin does support UTF-8 for passing
;; strings to subprograms.  That support lives inside the Cygwin DLL,
;; which replaces most of the Windows runtime with Posix-compatible
;; APIs.  The native Windows build of Emacs doesn't have that luxury.
;;

;; You can specific the extension list like:

;; ((("html" "pdf" "xml" "php" "md" "markdown")
;;   "a:/PortableApps/FirefoxPortable/FirefoxPortable.exe" "file://")
;;  (("mp3" "mp4" "mkv" "rmvb" "wmv" "flv" "avi")
;;   "a:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe" "")
;;  (("c")
;;   "a:/PortableApps/codeblocks/bin/codeblocks.exe" ""))

;; More details see below function definition.

;;* Code:


(defgroup entropy/open-with nil
  "Variable group for entropy-open-with"
  :group 'extensions)

(defcustom entropy/open-with-type-list nil
  "entropy/open-with type list

This list construct was structed like below:


(((\"html\" \"pdf\" \"xml\" \"php\" \"md\" \"markdown\")
  \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\" \"file://\")
 ((\"mp3\" \"mp4\" \"mkv\" \"rmvb\" \"wmv\" \"flv\" \"avi\")
  \"a:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe\" \"\")
 ((\"c\")
  \"a:/PortableApps/codeblocks/bin/codeblocks.exe\" \"\"))

There's three element for one group of this list
- list of extention name regexp
- portable program path
- program inputed parameter

Note: extension name regexp can not format with abbrev dot like
\"\\.pdf\", using pure extension name regexp instead, thus in
this case 'pdf' extension as \"pdf\" regexp specific.


Like above:

- (\"html\" \"pdf\" \"xml\" \"php\" \"md\" \"markdown\") was the
list of extention name regexp which I should let them be opened
with portable firefox.

- \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\" was the
  portable firefox path.

- \"file://\" was the firefox commandline parameter for open one
  local file.


The gather of above elements was called one group setting for
open-with.

The general list tpye as:
( group1---> firefox open \"html php xml txt mardown\"
  group2---> mphc open media type file like \"mp4 mp3 aac ...\"
  gourp3 ....
  ......)
"
  :type 'sexp
  :group 'entropy/open-with)


(defvar entropy/open-with-type-list-full nil
  "Full list for `entropy/open-with-match-open' which gened by
  `entropy/open-with-type-list'")

(defvar entropy/open-with-url-regexp "\\(^https?\\|^s?ftp\\)://"
  "The Regexp string for matching url link.")

(defun entropy/open-with-do-list ()
  "Expand `entropy/open-with-type-list' to `entropy/open-with-type-list-full'.

Append 'exec' and 'parameter' to each file suffix 'ext', like:

((\"html\" \"filefox.exe\" \"file://\")
 (\"pdf\" \"filefox.exe\" \"file://\")
 (\"xml\" \"filefox.exe\" \"file://\")
 ....)"
  (setq entropy/open-with-type-list-full nil)
  (if (not entropy/open-with-type-list)
      (error "entropy/open-with-type-list was empty!"))
  (dolist (group entropy/open-with-type-list)
    (let* ((ext-list (car group))
           (exec (nth 1 group))
           (param (nth 2 group)))
      (dolist (ext ext-list)
        (let (new-group-e)
          (add-to-list 'new-group-e param)
          (add-to-list 'new-group-e exec)
          (add-to-list 'new-group-e ext)
          (add-to-list 'entropy/open-with-type-list-full new-group-e))))))

(defun entropy/open-with-judge-file-type (filename)
  "Judge the FILENAME type according to
  `entropy/open-with-type-list' and return a list cotained final
  state of file description for `entropy/open-with-match-open'.

If not matched the exist file type then return nil."
  
  (entropy/open-with-do-list)
  (let ((judge-r nil)

        ;; final filename container for loading hexi-formated file-name if needed        
        fcname

        ;; return list
        (fin-rlist nil))

    ;; checking the file type and return the final file open data struct
    (dolist (el entropy/open-with-type-list-full)
      (let* (
             ;; get extention name with regexp format
             (ext-regexp (car el))
             (ext (concat "\\." ext-regexp "$"))

             ;; executable and file open protocal
             (exec (nth 1 el))
             (param (nth 2 el)))

        (if (string-match ext filename)
            (progn
              (cond
               ;; param with file protocal
               ((string-match-p "file://" param)
                (setq fcname (concat
                              param
                              (url-hexify-string
                               filename
                               (url--allowed-chars
                                (let ((hl url-unreserved-chars)
                                      (x 1))
                                  (add-to-list 'hl 58)
                                  (add-to-list 'hl 47))))))
                (setq judge-r t))

               ;; none param
               ((string-match-p "" param)
                (setq fcname (concat "\"" filename "\""))
                (setq judge-r t))

               ;; arbitrary param type
               ((stringp param)
                (setq fcname (concat param fileanme))
                (setq judge-r t)))
              (if judge-r
                  (setq fin-rlist `(,filename ,fcname ,exec)))))))
    (if fin-rlist
        fin-rlist
      nil)))

(defun entropy/open-with-process-default-dir ()
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
`entropy/open-with-buffer'

It's core function was to match the ARG files name extension with
`entropy/open-with-type-list' and used the previous function
`entropy/open-with-do-list' to convert the readable list to full
pair list `entropy/open-with-type-list-full'

*Note:*

ARG files: 
           it's name coding type must be consistency with system
           local like \"Chiese-GBK\" with \"ASCII\" . it's the bug

The subprocess for calling with for associated file will not use
`default-dirctory' be the process handle location, using
`entropy/open-with-process-default-dir' to redirected to system
temp dir for preventing some file deleting operation conflicted
occurrence.
"
  (dolist (el files)
    (let (($dowith (entropy/open-with-judge-file-type el))
          (default-directory (entropy/open-with-process-default-dir)))
      (if $dowith
          (if (string-equal system-type "windows-nt")
              (w32-shell-execute "open" (nth 2 $dowith) (nth 1 $dowith))
            (cond
             ((string-equal system-type "gnu/linux")
              (start-process "" nil "xdg-open" (car $dowith)))
             ((string-equal system-type "darwin")
              (shell-command (concat "open" (shell-quote-argument (car $dowith)))))
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
              (let ((default-directory (entropy/open-with-process-default-dir)))
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


