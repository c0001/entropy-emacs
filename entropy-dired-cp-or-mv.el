;;; File name: entropy-dired-cp-or-mv.el ---> for entropy-emacs
;;
;; Copyright (c) 2018 Entropy
;;
;; Author: Entropy
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This package let you can quickly copy or move files (also be folder) from one dired window to
;; another one.
;;
;; The first thing you shoud do is to markd files in one dired window or just be with the entry
;; under current point position, and then called `entropy/cpmv-dired-get-files-list' to get the
;; file-list you marked.
;;
;; The final step is swithed to another dired windows and called `entropy/cpmv-to-current' to copy
;; or move the files in file-list obtain from previous operation.
;;
;;
;;  

;; * Code:
;; ** require

;; ** variable declaration
(defvar entropy/cpmv--marked-files-list nil
  "marked file-list created by `dired-get-marked-files'")
(defvar entropy/cpmv--source-buffer-name nil
  "The buffer name of source marked file buffer which was in
  dired mode and created by dired.")

(defvar entropy/cpmv--get-info
  '("
This is getted file list buffer, please checkout them carefully.

You must confirm that this list doesn't include the item which
equal to your target destination and the selected files of what
marked by your mistake operation!

Be sure in mind that data was the most core of your life!
" "get")
  "The getted-file-list buffer info-string for
`entropy/cpmv-dired-get-files-list'")

(defvar entropy/cpmv--target-info
  '("
Recheckd selected fils list buffer.

Be sure in mind that data was the most core of your life!
" "target")
  "The rechecked buffer info-string for
  `entropy/cpmv-to-current'")

;; ** face declaration
(defgroup entropy/cpmv-face nil
  "face variable group for `entropy-dired-cp-or-mv'.")

(defface entropy/cpmv-title-face
  '((t (:foreground "orange1" :bold t)))
  "'*entropy/cpmv-prompt-buffer*' title face"
  :group 'entropy/cpmv-face)

(defface entropy/cpmv-get-info-face
  '((t (:foreground "sea green")))
  "Get info face."
  :group 'entropy/cpmv-face)

(defface entropy/cpmv-target-info-face
  '((t (:foreground "red")))
  "Target info face."
  :group 'entropy/cpmv-face)

(defface entropy/cpmv-file-face
  '((t (:foreground "PaleGreen2" :bold t)))
  "Face for file string."
  :group 'entropy/cpmv-face)

(defface entropy/cpmv-dir-face
  '((t (:foreground "hot pink" :bold t)))
  "Face for directory string."
  :group 'entropy/cpmv-face)



;; ** function
;; *** library
;; **** tools function
(defun entropy/cpmv--clean-variable ()
  "Clean all the cpmv about variable."
  (setq entropy/cpmv--marked-files-list nil)
  (setq entropy/cpmv--source-buffer-name nil))

(defun entropy/cpmv--reverse (list-symbol)
  "Reverse order the sequence of list.

Because each elements of one list which be handled by `dolist' to the new list container will have
the opposite vector direction as the origin one. For this situation, this function to done with what
your wanted."
  (let (return-list)
    (dolist (el (symbol-value list-symbol))
      (add-to-list 'return-list el))
    (set list-symbol return-list)))

(defun entropy/cpmv--make-files-list (in &optional out)
  "Accroding to the file type to make one list consisted like the
form blow:

((0 \"file-name\")
 (1 \"dir-name\"))

Argument description:

- in: necessary arg of the file-list by dired mark function
  `dired-get-marked-files'.
  
  This is the list type arg.


- out: One symbol of output list variable. 

This is one internal function for `entropy/cpmv-dired-get-files-list'
"
  (let (temp rlist)
    (dolist (el in)
      (if (file-directory-p el)
          (progn
            (add-to-list 'temp el)
            (add-to-list 'temp 1))
        (progn
          (add-to-list 'temp el)
          (add-to-list 'temp 0)))
      (add-to-list 'rlist temp)
      (setq temp nil))
    (entropy/cpmv--reverse 'rlist)
    (if out
        (set out rlist)
      rlist)))


(defun entropy/cpmv--judgement-duplicated-files (in)
  "Judged whether file in file-list which produced by
`entropy/cpmv--make-files-list' has been in current location.

This function used by `entropy/cpmv-to-current'"
  (let ((judge 0)
        (rv nil))
    (dolist (el in)
      (let ((f-o-d (nth 1 el)))
        (if (file-exists-p (concat (dired-current-directory) (file-name-nondirectory f-o-d)))
            (setq judge 1))))
    (if (> judge 0)
        (setq rv t))
    rv))

;; **** print function

;; The following coding was aimed to render the marked file-list buffer like:


;; +------------------------------------------+
;; |                                          |
;; |   Buffer introduction: xxxxxxx           |
;; |   xxxxxxxxxxxxxxxxxxxxxxxxxxxx           |
;; |   xxxxxxxxxxxxxxxxxxxxxxxxxxxx           |
;; |                                          |
;; |   01-file: xxxxx                         |
;; |   02-dir : xxxxxx                        |
;; |   03-file: xxxxxxx                       |
;; |   .......                                |
;; |   nn-dir : xxxxxxx                       |
;; |                                          |
;; |                                          |
;; |                                          |
;; |                                          |
;; +------------------------------------------+
       
;; The concept for print marked file-list could be generally divided into two partition:
       
;; - Detailed file-list for let it contained the serial number for each entry.
;; - Color each entry according to it's file-type that file or directory.
       
;; For print entry with serial number, we should calculate the sum of marked files and then arrange
;; them into the sequence which will also align them by adding the proper amount of '0'. And this
;; process was rely on the `entropy/cpmv--justify-count-visual'

;; And then give the color of each-entry, which determined by file-type so that we choose for entry of
;; file-type 'file' be green and 'directory' be pink. All of this progress was ran by
;; `entropy/cpmv--highlight-line'




(defun entropy/cpmv--print (file-list &optional buffer-info)
  "Print files list in *entropy/cpmv-prompt-buffer*

  This functions' mechanism can divided into two partitaion:
  - file-list detailed
  - color the entry of file-list and then insert them into buffer

  The file-list evolution can express for the follow flow-chart:

  1. Create the first-list(flist) which adding the serial-number for each element of the input
     file-list.

  2. Using `entropy/cpmv--justify-count-visual' for align the serial-number rely on the length of
     marked file-list to make second-list(slist).

  3. Let the file-type number tag be as string presentation , produce the third-list(tlist).

  4. Make one list which concat three element of the entry of tlist to be one string preparing for
     being hightlited by `entropy/cpmv--highlight-line'. This is character-list(clist).

  5. Make print-list(plist) whose each entry of it were adding face attribute in it.


  The last progress was to insert each of them one by one into the buffer named \"*entropy/cpmv-prompt-buffer*\"
"
  (let (count
        topc
        flist slist tlist clist plist)
    ;; calculate topc
    (setq count 0)
    (dolist (el file-list)
      (setq count (+ count 1)))
    (setq topc count)

    ;; flist
    (setq count 0)
    (dolist (el file-list)
      (setq count (+ count 1))
      (add-to-list 'flist `(,count ,(car el) ,(nth 1 el))))
    (entropy/cpmv--reverse 'flist)

    ;; slist
    (dolist (el flist)
      (let ((ns (car el))
            (type (nth 1 el))
            (fname (nth 2 el)))
        (setq ns (entropy/cpmv--justify-count-visual topc ns))
        (add-to-list 'slist `(,ns ,type ,fname))))
    (entropy/cpmv--reverse 'slist)

    ;; tlist
    (dolist (el slist)
      (let ((ns (car el))
            (type (nth 1 el))
            (fname (nth 2 el)))
        (if (equal type 0)
            (setq type "file")
          (setq type "dir"))
        (add-to-list 'tlist `(,ns ,type ,fname))))
    (entropy/cpmv--reverse 'tlist)

    ;; concat elements of tlist to strings
    (dolist (el tlist)
      (let ((ns (car el))
            (type (nth 1 el))
            (fname (nth 2 el)))
        (add-to-list 'clist `(,type ,(concat ns 
                                      (if (equal type "dir")
                                          "--"
                                        "-")
                                      type ": "
                                      fname)))))
    (entropy/cpmv--reverse 'clist)

    ;; make highlight string list
    (dolist (el clist)
      (let ((type (car el))
            (fname (nth 1 el)))
        (add-to-list 'plist (entropy/cpmv--highlight-line fname type))))
    (entropy/cpmv--reverse 'plist)

    ;; insert them to buffer
    (let ((buffer "*entropy/cpmv-prompt-buffer*"))
      (if (get-buffer buffer)
          (progn
            (kill-buffer buffer)
            (get-buffer-create buffer))
        (get-buffer-create buffer))
      (display-buffer buffer)
      (with-current-buffer buffer
        (toggle-truncate-lines 0)
        (insert (propertize "==========Cpmv Buffer==========\n\n" 'face 'entropy/cpmv-title-face))

        ;; If has info docstring then insert it to the top of buffer.
        (if (and buffer-info (listp buffer-info) (stringp (car buffer-info))
                 (or (string=  "get" (nth 1 buffer-info))
                     (string= "target" (nth 1 buffer-info))))
            (let* ((info-type (nth 1 buffer-info))
                   (info-face (cond
                               ((string= info-type "get")
                                'entropy/cpmv-get-info-face)
                               ((string= info-type "target")
                                'entropy/cpmv-target-info-face))))
              (progn
                (insert (propertize (car buffer-info) 'face info-face))
                (insert "\n"))))

        ;; Insert plist
        (dolist (el plist)
          (insert  (eval (car el)))
          (insert "\n")))
      (read-only-mode 1))))


(defun entropy/cpmv--highlight-line (string file-type)
  "Adding the face to string according to the file-type attribute info within it."
  (let (ret)
    (cond
     ((equal file-type "file")
      (setq ret `((propertize ,string 'face 'entropy/cpmv-file-face))))
     ((equal file-type "dir")
      (setq ret `((propertize ,string 'face 'entropy/cpmv-dir-face))))
     (t (error "Invalid file-type.")))
    ret))

(defun entropy/cpmv--judge-file-type (file-name)
  "Judgingt the file-type of 'file-name'."
  (if (file-directory-p file-name)
      1
    0))

(defun entropy/cpmv--justify-count-visual (topc int)
  "Return the string form of visual INT counts which improved by adding the align character '0' for
  the sequence of `prin1'

  =topc= was the bigest amount of list length of marked file-list, =int= was the current entry's list
  sequence serial number.

  This function ran with the machanism of adding the aligned mounts of '0' for current entry's
  serial number according to  =topc= you specific.

  For example:

  If you have the file-list which length was 1000, and the curren serial number of one entry was 98,
  then take them as the arg into this fuction will return '0098'.
  "
  (let ((top-c (entropy/cpmv--return-count-type topc))
        (cur-c (entropy/cpmv--return-count-type int))
        surplus
        (sint (number-to-string int)))
    (setq surplus (- top-c cur-c))
    (let ((count 1))
      (while (<= count surplus)
        (setq sint (concat "0" sint))
        (setq count (+ count 1)))
      sint)))

(defun entropy/cpmv--return-count-type (int)
  "Return the digits of INT.

This function was one recursive function.
"
  (if (< int 10)
      1
    (let (result
          (n-int (/ int 10))
          cur)
      (setq cur (+ (entropy/cpmv--return-count-type n-int) 1))
      cur)))


;; *** main function
;;;###autoload
(defun entropy/cpmv-dired-get-files-list ()
  "Get files list for cp or mv for `entropy/cpmv-to-current' with
checked buffer showing"
  (interactive)
  (let ((rl (dired-get-marked-files))
        (buf (buffer-name))
        (count 0)
        (wc (current-window-configuration)))
    (if rl
        ;; checkout no . or .. folder contained
        (let ((dot nil))
          (dolist (el rl)
            (if (string-match-p "\\(\\.$\\|\\.\\.$\\)" el)
                (setq dot t)))
          (if (equal dot t)
              (error "You can't select dot folder(. or ..)!")))
      (error "No valid selected file!"))
    (entropy/cpmv--print (entropy/cpmv--make-files-list rl) entropy/cpmv--get-info)
    (if (yes-or-no-p "Do your really want to prepare for copying them? ")
        (progn
          (entropy/cpmv--clean-variable)
          (entropy/cpmv--make-files-list rl 'entropy/cpmv--marked-files-list)
          (setq entropy/cpmv--source-buffer-name buf)
          (kill-buffer "*entropy/cpmv-prompt-buffer*")
          (set-window-configuration wc))
      (progn
        (entropy/cpmv--clean-variable)
        (kill-buffer "*entropy/cpmv-prompt-buffer*")
        (set-window-configuration wc)))))

;;;###autoload
(defun entropy/cpmv-to-current ()
  "Cp or Mv fils or directory of file-list created by
`entropy/cpmv-dired-get-files-list' with checked buffer showing"
  (interactive)
  (if (and entropy/cpmv--source-buffer-name
           entropy/cpmv--marked-files-list
           (equal major-mode 'dired-mode))
      (progn ;When pre-check was success execute func.
        (progn
          ;; Detective the mistake operation, if true exit current function.
          ;; Mistake operation include:
          ;; - do cpmv in origin dir:
          ;;   In this case no need to cpmv file by using entropy-dired-cp-or-mv.el.
          ;; - target dir was including of selected files.
          ;;   In this case, selected files contained target dir, which will cause func logical error
          ;; - target dir has duplicate files which equal selected files from origin dir.

          
          ;; Judge whether in source dir
          (if (string= entropy/cpmv--source-buffer-name (buffer-name))
              (progn
                (entropy/cpmv--clean-variable)
                (error "You can't cp or mv files in the same loacation.")))

          ;; Judgement whether has the duplicated file which has the same name as the element of
          ;; source file-list.
          (if (entropy/cpmv--judgement-duplicated-files entropy/cpmv--marked-files-list)
              (progn
                (entropy/cpmv--clean-variable)
                (error "Have duplicated file.")))

          ;; Judgement whether target dir was one of the selected files.
          (let ((curd (dired-current-directory))
                (this-true nil))
            (dolist (el entropy/cpmv--marked-files-list)
              (if (string-match-p (regexp-quote (nth 1 el)) curd)
                  (setq this-true t)))
            (if this-true
                (progn
                  (entropy/cpmv--clean-variable)
                  (error "You couldn't copy self folder!")))))

        ;; ========== Do cpmv operation ==========
        (let (;; Prompt for user to choose copy or mv source marked.
              (option (completing-read "Choose cp or mv: " '("mv" "cp") nil t))
              (wc (current-window-configuration))
              (count 0))

          ;; Render re-checkout prompt buffer and try to obtain the user confirmation.
          (entropy/cpmv--print entropy/cpmv--marked-files-list entropy/cpmv--target-info)

          ;; Do abort operation
          (if (not (yes-or-no-p "Checked out the mission done? "))
              (progn (set-window-configuration wc)
                     (entropy/cpmv--clean-variable)
                     (error "Abort entropy/cpmv-to-current!"))
            (set-window-configuration wc))

          ;; Copy file
          (message "Coping files ...... ")
          (dolist (el entropy/cpmv--marked-files-list)
            (if (equal (car el) 0)
                (copy-file (nth 1 el) (dired-current-directory))
              (copy-directory (nth 1 el) (dired-current-directory))))

          ;; If choose 'mv' operation then prompt whether delete source file interactively.
          (if (string= option "mv")
              (if (yes-or-no-p "Really delete origin files? ")
                  (progn
                    (message "Deleting files ......")
                    (dolist (el entropy/cpmv--marked-files-list)
                      (if (equal (car el) 0)
                          (delete-file (nth 1 el) t)
                        (delete-directory (nth 1 el) t t))))
                (message "Origin files remained.")))

          ;; Prompt finishing operation
          (cond
           ((string= "cp" option)
            (message "Copy files done!"))
           ((string= "mv" option)
            (message "Move files done!"))))
        
        ;; ========== refresh dired displaying ==========
        ;; judge whether origin source dired buffer exist, if t then revert it for presentting
        ;; latest state after cp-or-mv operation
        (let ((p-buffer-list (mapcar (function buffer-name) (buffer-list)))
              (exist nil))
          (dolist (el p-buffer-list)
            (if (equal el entropy/cpmv--source-buffer-name)
                (setq exist t)))
          (if exist
              (with-current-buffer entropy/cpmv--source-buffer-name
                (revert-buffer nil t))))

        ;; Revert target dired buffer and clean func internal variable.
        (entropy/cpmv--clean-variable)
        (revert-buffer nil t))
    
    (progn
      (entropy/cpmv--clean-variable)
      (error "Do entropy/cpmv failure!")))
  (when (get-buffer "*entropy/cpmv-prompt-buffer*")
    (kill-buffer "*entropy/cpmv-prompt-buffer*")))

;; ** provide
(provide 'entropy-dired-cp-or-mv)
