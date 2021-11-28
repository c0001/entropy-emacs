;;; entropy-open-with.el --- Open file by specific program
;;
;;; Copyright (C) 20190614  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-open-with/blob/master/entropy-open-with.el
;; Package-Requires: ((entropy-common-library "0.1.0"))
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
;; Open files with external executable applications on emacs interface.
;;
;; There's times for emacer try their best-efforts for dealing all the
;; operations within emacs only, but I thought about that this was not
;; sticked enough for daily emacs experience that's most of common
;; emacser prefer to do some task emacs not be good at with and using
;; external application rather than gaze at the lagging for thus the
;; procedure do with elisp, on the purpose for some ultra emacser does
;; said previous.
;;
;; This package given the ability what file openning with wide customized
;; external applications according to the filename extension.
;;
;;;; Installation
;;
;; Just requiring the file [[file:entropy-open-with.el][entropy-open-with.el]] using: : (require
;; 'entropy-open-with)
;;
;; Then bind key for func ~entropy/open-with-dired-open~ and
;; ~entropy/open-with-buffer~ for suggested keybindings =C-M-return= and
;; =C-M-1= respectively.
;;
;; Before doing the above requiring operation, remember to adding this
;; package to the load-path.
;;
;; Or if you using [[https://github.com/jwiegley/use-package][use-pacage]] package management macro, you could doing
;; as below:
;; #+BEGIN_SRC emacs-lisp
;;   (use-package entropy-open-with
;;     :ensure nil
;;     :load-path "path-to-this-package"
;;     :commands (entropy/open-with-dired-open
;;                entropy/open-with-buffer)
;;     :bind (("C-M-1" . entropy/open-with-buffer))
;;     :init
;;     (with-eval-after-load 'dired
;;       (define-key dired-mode-map
;;         (kbd "<C-M-return>") 'entropy/open-with-dired-open)))
;; #+END_SRC
;;
;;;; Configuratioin
;;
;; The core configuration for this package's customized refers was based
;; on the one customized variable =entropy/open-with-type-list= which
;; indicated that you can specify group of filename extension name of
;; regexp expression with the corresponding external application forms as
;; below:
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
;; Each element of this list consists of three subjects:
;;
;; 1) *extension regexp strings list*
;;
;;    Regexp strings match for specified filename extension can not be
;;    include extension dot prefix, thus "\.pdf" was ineffectual, using
;;    "pdf" instead.
;;
;;    Group extensions match was supported as the benefit way for
;;    reducing config literal context length for the case for which
;;    application can be calling by dealing with multi file types,
;;    e.g. =firefox= can be used both of html file open and pdf file as
;;    so.
;;
;; 2) *External application path string*
;;
;;    Called of application's absolute path.
;;
;; 3) *Command line filaname transfer heading type*
;;
;;    For some external application's URI args request as that file name
;;    must be concated with the special head prefix, e.g. for web browser
;;    that the local page file command line arg must have the prefix
;;    "file://", this subject given for thus as. Type was symbol or
;;    function, see below list for details:
;;
;;    - 'file' method:
;;
;;      Most of web browser can not use the local file path string as the
;;      file url argument in case of that the path string contained
;;      non-ascii characters such as symbol or cjk as for. In this case,
;;      using file protocol hexified string as argument is wise choice.
;;
;;      This transform firstly hexifies the whole local file path string
;;      and then adding the \"file:///\" protocol prefix string ahead of
;;      it.
;;
;;    - 'quote' method:
;;
;;      It's the most common decoration for commanline arguments, using
;;      double quotation char pair.
;;
;;    - nil, passing origin path string to process.
;;
;;    - Arbitrary function self specified, requesting one arg, the file path.
;;
;; 4) *command argument string*
;;
;;    Argumet is the concated command arguments string, the single string
;;    type.
;;
;;;; Interaction
;;
;; - *Func:* ~entropy/open-with-interactively~
;;
;;   Manually choose file be opened later with specific external
;;   applications query from list =entropy/open-with-type-list=, gives
;;   the file candidates query filter prompt powered by [[https://github.com/abo-abo/swiper][ivy]] framework.
;;
;; - *Func:* ~entropy/open-with-dired-open~
;;
;;   Calling external application to open marked files in dired-mode.
;;
;; - *Func:* ~entropy/open-with-buffer~
;;
;;   Calling external applications to open current buffer chained file.
;;
;;
;; All functions above mentioned was given as the interactive function
;; for as so. You can binding them to your key-map along with your habits
;; or obey the previous installation init setup.
;;
;;
;;;; Apis
;;
;; =entropy-open-with= was the minor tool for just giving fiews useful
;; api functions:
;;
;; + Func ~entropy/open-with-match-open~
;;
;;   This func was given the simple way for query input filename's
;;   corresponding 'open-with' type and then opened it immediately with
;;   specific external application.
;;
;;   It's arg was one list consists of filenames (path strings), thus
;;   multi files 'open-with' was supported.
;;
;; + Func ~entropy/open-with-port~
;;
;;   This func given the try for open single file with 'open-with' or
;;   using internal emacs openning method.
;;
;;   This func compensates the missing port of func
;;   ~entropy/open-with-match-open~ procedure which can open url(web
;;   link) using external application that original mechanism just allow
;;   exists local file matched with, means that origin one can not
;;   distinguish web link because of that it's do not has any mime
;;   extensions.
;;
;;   Arguments:
;;
;;   - Force one: interact
;;
;;     This arguments was forcefully required by calling with as, it
;;     denoted whether let user manually choose file for 'open-with' for.
;;
;;   - optionals: 1) filename      2) inemacs
;;
;;     These two args are optionally, =filename= using the case just when
;;     the forced arg =interact= are nil, it's used for the case while
;;     developer want to calling it just in elisp situation.
;;
;;     The other one =inemacs= gives the try for open specific file in
;;     emacs method.
;;
;;;; Limitation on windows platform:*
;;
;; In windows, the decoding method was using the one called =code pages=
;; which not compatible with UNIX-LIKE platform which also using one
;; coding method for all cli-transfer with. That the problem occured that
;; for that emacs-windows-port can not decoding unicode cli-args for the
;; subprocess with properly processing for.
;;
;; In =entropy-open-with= interal mechnism, because of that it using
;; ~w32-shell-execute~ as the windows port corresponding subjects core
;; func, unicode file name was not supported excluding the situation
;; until your current code page can be fully decoding the inputting
;; filename as so.
;;
;; This imperfection was the emacs windows port primary implementation
;; state, which was referenced to the gnu-emacs mailing list: [[https://lists.gnu.org/archive/html/emacs-devel/2016-01/msg00406.html][here]].
;;
;; [2019-05-17 Fri 06:08:49]
;;
;; #+BEGIN_QUOTE
;; Now, if you are using the latest version of win10 (any version upper
;; than 1803), you could enable the beta optional setting for globally
;; =utf-8= capability in the =language&region= config panel.
;; #+END_QUOTE
;;
;;; Configuration:
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
;;; Code:
(require 'entropy-common-library)

(defgroup entropy/open-with nil
  "Variable group for entropy-open-with"
  :group 'extensions)

(defcustom entropy/open-with-type-list nil
  "entropy/open-with type list

This list construct was structed as below:

#+BEGIN_SRC elisp
  '(((\"html\" \"pdf\" \"xml\" \"php\" \"md\" \"markdown\")
     \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\" file '(\"-new-tab\"))
    ((\"mp3\" \"mp4\" \"mkv\" \"rmvb\" \"wmv\" \"flv\" \"avi\")
     \"a:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe\" quote nil)
    ((\"c\")
     \"a:/PortableApps/codeblocks/bin/codeblocks.exe\" quote nil))
#+END_SRC

Each element of it calls one _open-with_ group, the group sepcific
for sets of file type which can open with one specified
trigger(process command form).

There's four element for one group of this list
- List of extention name regexp
- Portable programe path
- Path argument format method (see blow for details)
- Executing parameter list

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

- '(\"new-tab\") was the exec args for firefox which indicated to
  open such html file in new-tab buffer.

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
   and then adding the \"file://\" protocol prefix string ahead of
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
  :type '(choice
          (const  :tag "Nothing specified" nil)
          (repeat :tag "Open with specification groups list"
                  (list :tag "Open with specification group"
                        (repeat :tag "File name extension regexp list"
                                (string   :tag "File name extension regexp"))

                        (choice :tag "Open with exec"
                                (string   :tag "Exec path or name")
                                (const    :tag "Use system default" nil))

                        (choice :tag "Path argument format method"
                                (const    :tag "File protocol" file)
                                (const    :tag "Shell argument quote" quote)
                                (function :tag "Sepcified format method function")
                                (const    :tag "Do not transferred" nil))

                        (choice (const    :tag "No exec arguments" nil)
                                (repeat   :tag "Exec arguments list"
                                          (string :tag "Exec argument"))))))
  :group 'entropy/open-with)

(defvar entropy/open-with--type-list-full nil
  "Full list for `entropy/open-with-match-open' which gened by
  `entropy/open-with-type-list'")

(defvar entropy/open-with-url-regexp "\\(^https?\\|^s?ftp\\)://"
  "The Regexp string for matching url link.")

(defun entropy/open-with--on-win32 ()
  (member system-type '(windows-nt cygwin)))

(defvar __entropy/open-with-wsl-env-judged nil)
(defvar __entropy/open-with-wsl-env-p nil)
(defun entropy/open-with-wsl-env-p ()
  (if __entropy/open-with-wsl-env-judged
      __entropy/open-with-wsl-env-p
    (let ((wsl-indcf "/proc/version"))
      (setq __entropy/open-with-wsl-env-p
            (and
             ;; use uname judge
             (executable-find "uname")
             (string-match-p
              "Microsoft"
              (shell-command-to-string "uname -a"))
             ;; cat /proc/version file
             (file-exists-p wsl-indcf)
             (string-match-p
              "\\(microsoft\\|Microsoft\\)"
              (with-temp-buffer
                (insert-file-contents wsl-indcf)
                (buffer-substring
                 (point-min)
                 (point-max)))))))))

(defun entropy/open-with--sudo-file-name-predicate (filename)
  "Shrink trmap path of sudo privilege type when on *NIX platform."
  (if (and (not (entropy/open-with--on-win32))
           (string-match-p "^/sudo:root@.*?:/" filename))
      (replace-regexp-in-string "^/sudo:root@.*?:/" "/" filename)
    filename))

(defun entropy/open-with--do-list ()
  "Expand `entropy/open-with-type-list' to `entropy/open-with--type-list-full'.

Append 'ext-regexp', 'exec', 'path-transform' and 'parameter' to
each file suffix 'ext', like:

((\"html\" \"filefox.exe\" 'file \"-new-tab\")
 (\"pdf\" \"filefox.exe\" 'file \"-new-tab\")
 (\"xml\" \"filefox.exe\" 'file \"-new-tab\")
 ....)"
  (setq entropy/open-with--type-list-full nil)
  (when (not entropy/open-with-type-list)
    (message "entropy/open-with-type-list was empty! Using system native method for all."))
  (dolist (group entropy/open-with-type-list)
    (let* ((ext-list (car group))
           (exec (nth 1 group))
           (path-transform (nth 2 group))
           (caller-pattern (nth 3 group)))
      (when (and exec (stringp exec))
        (dolist (ext ext-list)
          (let (new-group-e)
            (push caller-pattern new-group-e)
            (push path-transform  new-group-e)
            (push exec new-group-e)
            (push ext new-group-e)
            (add-to-list 'entropy/open-with--type-list-full new-group-e)))))))

(defun entropy/open-with--judge-file-type (filename)
  "Judge the FILENAME type according to
`entropy/open-with-type-list' and return a plist cotained final
state of file description for `entropy/open-with-match-open'.

If not matched the exist file type then return nil."
  (let ((type-list (progn (entropy/open-with--do-list)
                          entropy/open-with--type-list-full))
        (file-path (expand-file-name (entropy/open-with--sudo-file-name-predicate filename)))
        $file-pattern rtn)
    (catch :exit
      (dolist (type type-list)
        (when (string-match (concat "\\." (car type) "$") file-path)
          (let ((exec (nth 1 type))
                (path-transform (nth 2 type))
                (caller-pattern (nth 3 type)))
            (cond ((eq 'file path-transform)
                   (cond ((entropy/open-with--on-win32)
                          (let ((path-formated (replace-regexp-in-string "^\\(.\\):/" "\\1/" file-path)))
                            (setq $file-pattern (concat "file:///" path-formated))))
                         ((or (eq system-type 'gnu/linux)
                              (eq system-type 'darwin))
                          (setq $file-pattern (concat "file://" file-path)))))
                  ((eq 'quote path-transform)
                   (setq $file-pattern (shell-quote-argument file-path)))
                  ((functionp path-transform)
                   (setq $file-pattern (funcall path-transform file-path)))
                  (t (setq $file-pattern file-path)))
            (setq rtn (list :caller exec :file-pattern $file-pattern
                            :caller-pattern caller-pattern
                            :open-with-arg (append caller-pattern (list $file-pattern))
                            :open-with-arg-concated (concat
                                                     (mapconcat 'identity caller-pattern " ")
                                                     " " $file-pattern)
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


(defun entropy/open-with--open-file-plist (file-plist)
  (let* ((file-plistp (entropy/cl-plistp file-plist))
         caller file-pattern file-path
         (error-pred (lambda (proc-name proc-buffer)
                       (pop-to-buffer proc-buffer)
                       (error "entropy-open-with <%s> exited with fatal"
                              proc-name)))
         (proc-sentinel
          (lambda (proc _event)
            (let ((proc-name (process-name proc))
                  (proc-buffer (process-buffer proc))
                  (proc-status (process-status proc)))
              (cond ((and (eq 'exit proc-status)
                          (not (= 0 (process-exit-status proc))))
                     (funcall error-pred proc-name proc-buffer))
                    ((member proc-status '(stop signal))
                     (funcall error-pred proc-name proc-buffer))
                    ((and (eq 'exit proc-status)
                          (= 0 (process-exit-status proc)))
                     (message "entropy-open-with <%s> open sucessfully"
                              proc-name)))))))
    (cond
     ;; ==================== plist assoc ====================
     ((and file-plistp
           ;; we don't want wsl env to using app associated open-with
           ;; refer since we must follow windows mimeapps settings
           (not (entropy/open-with-wsl-env-p)))
      (setq caller (plist-get file-plist :caller)
            file-pattern (expand-file-name (plist-get file-plist :file-pattern))
            open-with-arg-concated (plist-get file-plist :open-with-arg-concated)
            open-with-arg (plist-get file-plist :open-with-arg))
      (cond
       ((entropy/open-with--on-win32)
        (set-process-sentinel
         (w32-shell-execute "open" caller open-with-arg-concated)
         proc-sentinel))
       ((or (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
        (let (
              ;; preserve `process-connection-type' to t since while
              ;; nil some unexpected occasion occurred like env
              ;; inherited issue.
              (process-connection-type t))
          ;; use setsid to creat a new controlling terminal so that
          ;; emacs not kill it while open since: gvfs-open and
          ;; xdg-open return before their children are done
          ;; working. Emacs might kill their controlling terminal when
          ;; this happens, killing the children, and stopping refer
          ;; application from opening properly. (see
          ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
          ;; for more details)
          (set-process-sentinel
           (start-process
            (format "eemacs-*nix-open-with_%s_for_file_%s"
                    caller file-path)
            (get-buffer-create " *eemacs-*nix-open-with* ")
            "setsid" "-w" caller open-with-arg)
           proc-sentinel)))))
     ;; ==================== native open ====================
     ((null file-plistp)
      (setq file-path (expand-file-name file-plist))
      (cond
       ;; wsl
       ((entropy/open-with-wsl-env-p)
        (unless (executable-find "wsl-open")
          (user-error "You are in microsoft subsystem linux environment, please install 'wsl-open' \
firstly using 'npm install -g wsl-open'"))
        (let ((process-connection-type t))
          (set-process-sentinel
           (start-process
            (format "eemacs-wsl-open-with_for_file_%s"
                    file-path)
            (get-buffer-create " *eemacs-wsl-open-with* ")
            "setsid" "-w" "wsl-open" file-path)
           proc-sentinel)))
       ;; win32
       ((entropy/open-with--on-win32)
        (set-process-sentinel
         (w32-shell-execute "open" file-path)
         proc-sentinel))
       ;; linux
       ((eq system-type 'gnu/linux)
        (let ((process-connection-type t))
          (set-process-sentinel
           (start-process
            (format "eemacs-linux-open-with_xdg_for_file_%s"
                    file-path)
            (get-buffer-create " *eemacs-linux-open-with* ")
            "setsid" "-w" "xdg-open" file-path)
           proc-sentinel)))
       ;; macos
       ((eq system-type 'darwin)
        ;; FIXME: its not asynchronously
        (shell-command (concat "open " file-path))))

      ;; show warn when non caller found for file
      (unless (not (null caller))
        (message "Can not match any open with type for file '%s', open with native MIME assoc."
                 file-path))))))

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
    (setq el (entropy/open-with--sudo-file-name-predicate el))
    (let (($dowith (cond ((entropy/open-with--judge-file-type el)
                          (entropy/open-with--judge-file-type el))
                         ((ignore-errors (file-exists-p el))
                          el)
                         (t
                          nil)))
          (default-directory (entropy/open-with--process-default-dir)))
      (if (null $dowith)
          (warn "File '%s' not exists" el)
        (entropy/open-with--open-file-plist $dowith)))))


(defun entropy/open-with-port (interact &optional filename inemacs)
  "Open with file by accepted the FILENAME arg.

When arg INTERACTIVE was none-nil. If FILENAME was not existed
then prompt for retrying inputting FILENAME.(FILENAME inputting
support `read-file-name-internal')

When without interactived option chosen, then directly open the
FILENAME, if FILENAME was not exist then return error.

When inemacs was non-nil open FILENAME in emacs.

Tips: non-interactive state support open url.

Note: this func redirected `default-directory' as
`entropy/open-with-match-open' does so."
  (if interact
      (let ((accept (entropy/open-with--sudo-file-name-predicate
                     (completing-read "Please input FILENAME: " 'read-file-name-internal
                                      nil t nil 'file-name-history))))
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
    (if (and filename
             (setq filename
                   (entropy/open-with--sudo-file-name-predicate filename)))
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
(defun entropy/open-with-dired-open (&optional prefix)
  "Dired open with portable apps.

It's core function is `entropy/open-with-match-open', see it for
details.

Optional argument PREFIX if non-nil:

- we use `dired-get-marked-files' to matched files and batch open
  them, otherwise we just open the file at current line e.g. get
  the filename by `dired-get-filename'.
"
  (interactive "P")
  (if (equal major-mode 'dired-mode)
      (let ((files (if prefix
                       (dired-get-marked-files)
                     (list (dired-get-filename)))))
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
