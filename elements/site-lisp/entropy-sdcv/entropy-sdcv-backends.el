;;; code

;;;; require
(require 'entropy-sdcv-core)
(require 'json)
(require 'youdao-dictionary)
(require 'bing-dict)
(require 'google-translate)
(require 'entropy-common-library)

(declare-function wudao/query-word-by-command "wudao-query")
(declare-function wudao/query-word-hash "wudao-query")

;;;; defcustom

(defgroup entropy/sdcv-backends-group nil
  "Customizable variable group for `entropy-sdcv-backends'"
  :group 'entropy-sdcv)

;;;; library
(defun entropy/sdcv-backends--org-colorful (feedback)
  (with-temp-buffer
    (insert feedback)
    (org-mode)
    (org-font-lock-ensure)
    (buffer-string)))

(defun entropy/sdcv-backends--org-show-feedback ()
  (unless (eq major-mode 'org-mode)
    (org-mode)
    (outline-show-all)))

(defun entropy/sdcv-backends--auto-face-common (show-method)
  (cl-case show-method
    (adjacent-common
     nil)
    (t
     'entropy/sdcv-core-common-face)))

(defun entropy/sdcv-backends--make-feedback-single-line (feedback &optional separator)
  (let* ((str-list (split-string feedback "\n" t))
         (rtn "")
         (separator (or (and (stringp separator) separator) "| "))
         (tail-dis (format "%s$" separator)))
    (cl-loop for str in str-list
             do (setq rtn (concat rtn str separator)))
    (replace-regexp-in-string tail-dis "" rtn)))

;;;; backends
;;;;; youdao
(defun entropy/sdcv-backends--query-with-youdao (query &rest _)
  (setq entropy/sdcv-core-response-log
        (youdao-dictionary--format-result (youdao-dictionary--request query))))

(defun entropy/sdcv-backends--youdao-show-predicate (feedback show-method)
  (cl-case show-method
    (pos-tip
     (entropy/sdcv-backends--org-colorful feedback))
    (posframe
     (entropy/sdcv-backends--org-show-feedback))
    (popup
     (entropy/sdcv-backends--org-colorful feedback))
    (minibuffer-common
     (entropy/sdcv-backends--make-feedback-single-line
      (entropy/sdcv-backends--org-colorful feedback)
      " "))
    (adjacent-common
     (entropy/sdcv-backends--org-show-feedback))))

;;;;; bing
(defvar entropy/sdcv-backends--bing-dict-response nil
  "External bing dict process feedback response.")

(defun entropy/sdcv-backends--bing-show-predicate (feedback show-method)
  (cl-case show-method
    (minibuffer-common
     (entropy/sdcv-backends--make-feedback-single-line
      feedback))
    (t
     feedback)))

(defun entropy/sdcv-backends--query-with-bing (query &rest _)
  (setq entropy/sdcv-core-response-log
        (entropy/sdcv-backends--bing-dict-url-retrieve query)))

(defun entropy/sdcv-backends--bing-dict-url-retrieve (query)
  (setq entropy/sdcv-backends--bing-dict-response nil)
  (let (info rtn buffer)
    (setq buffer
          (save-match-data
            (url-retrieve-synchronously
             (concat bing-dict--base-url
                     (url-hexify-string query))
             t t)))
    (with-current-buffer buffer
      (entropy/sdcv-backends--bing-dict-brief-cb (decode-coding-string query 'utf-8))
      (erase-buffer)
      (setq info entropy/sdcv-backends--bing-dict-response)
      (cond
       ((equal (car info)
               "exact-definition")
        (insert (nth 1 info) " " (nth 2 info) "\n\n" (nth 4 info))
        (setq rtn (buffer-string)))
       ((equal (car info)
               "sounds-like")
        (insert (nth 1 info) "\n'n" (nth 3 info))
        (setq rtn (buffer-string)))
       ((equal (car info) "no-matched")
        (setq rtn entropy/sdcv-core-response-null-prompt))
       ((equal (car info) "machine-translation")
        (insert (nth 3 info) "\n\n" (nth 4 info))
        (setq rtn (buffer-string)))))
    rtn))

(defun entropy/sdcv-backends--bing-dict-brief-cb (keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (setq keyword (propertize keyword
                            'face
                            'font-lock-keyword-face))
  (condition-case nil
      (if (bing-dict--has-machine-translation-p)
          (setq entropy/sdcv-backends--bing-dict-response
                (list
                 "machine-translation"
                 bing-dict--machine-translation-text
                 bing-dict-word-def-separator
                 keyword
                 (bing-dict--machine-translation)))
        (let ((defs (bing-dict--definitions))
              extra-defs
              pronunciation
              short-defstr)
          (if defs
              (progn
                (cond
                 ((eq bing-dict-show-thesaurus 'synonym)
                  (when (setq extra-defs (bing-dict--synonyms))
                    (push extra-defs defs)))
                 ((eq bing-dict-show-thesaurus 'antonym)
                  (when (setq extra-defs (bing-dict--antonyms))
                    (push extra-defs defs)))
                 ((eq bing-dict-show-thesaurus 'both)
                  (dolist (func '(bing-dict--synonyms bing-dict--antonyms))
                    (when (setq extra-defs (funcall func))
                      (push extra-defs defs)))))
                (setq
                 pronunciation (bing-dict--pronunciation)
                 short-defstr (mapconcat 'identity (nreverse defs)
                                         "\n"))
                (setq entropy/sdcv-backends--bing-dict-response
                      (list
                       "exact-definition"
                       keyword
                       pronunciation
                       bing-dict-word-def-separator
                       short-defstr)))
            (let ((sounds-like-words (bing-dict--get-sounds-like-words)))
              (if sounds-like-words
                  (setq entropy/sdcv-backends--bing-dict-response
                        (list
                         "sounds-like"
                         bing-dict--sounds-like-text
                         bing-dict-word-def-separator
                         sounds-like-words))
                (setq entropy/sdcv-backends--bing-dict-response
                      (list "no-matched" bing-dict--no-result-text)))))))
    (error (bing-dict--message bing-dict--no-result-text))))

;;;;; google
(defun entropy/sdcv-backends--query-with-google (query &rest _)
  (setq entropy/sdcv-core-response-log
        (entropy/sdcv-backends--google-translate-callback
         "auto" entropy/sdcv-core-source-language query 'popup)))

(defun entropy/sdcv-backends--google-translate-callback
    (source-language target-language text &optional output-destination)
  (let* ((json (google-translate-request source-language
                                         target-language
                                         text)))
    (if (null json)
        (message "Nothing to translate.")
      (let* ((detailed-translation
              (google-translate-json-detailed-translation json))
             (detailed-definition
              (google-translate-json-detailed-definition json))
             (gtos
              (make-gtos
               :source-language source-language
               :target-language target-language
               :auto-detected-language (aref json 2)
               :text text
               :text-phonetic (google-translate-json-text-phonetic json)
               :translation (google-translate-json-translation json)
               :translation-phonetic (google-translate-json-translation-phonetic json)
               :detailed-translation detailed-translation
               :detailed-definition detailed-definition
               :suggestion (when (null detailed-translation)
                             (google-translate-json-suggestion json))))
             (output-destination (if (null output-destination)
                                     google-translate-output-destination
                                   output-destination)))
        (with-temp-buffer
          (google-translate-insert-translation gtos)
          (google-translate--trim-string
           (buffer-substring (point-min) (point-max))))))))

;;;;; sdcv
;;;;;; defcustom
(defcustom entropy/sdcv-backends-sdcv-common-dicts-host
  (expand-file-name ".stardict" (getenv "HOME"))
  "The common ruled stardict collections host directory."
  :type 'directory
  :group 'entropy/sdcv-backends-group)

(defcustom entropy/sdcv-backends-sdcv-user-dicts nil
  "User sdcv dicts directories list, it is priority than
`entropy/sdcv-backends-sdcv-common-dicts-host'."
  :type '(choice (const nil) (repeat directory))
  :group 'entropy/sdcv-backends-group)

(defcustom entropy/sdcv-backends-sdcv-program
  (if (string-equal system-type "darwin") "/usr/local/bin/sdcv" "sdcv")
  "Sdcv programe name custom specification, if it not in your
path, you are required to specify it's path with it's filename
e.g. '/bin/sdcv'."
  :type 'file
  :group 'entropy/sdcv-backends-group)

(defcustom entropy/sdcv-backends-sdcv-command-prefix ""
  "Prefix command argumetns for sdcv exec."
  :type 'string
  :group 'entropy/sdcv-backends-group)

;;;;;; defvar for sdcv query
(defvar entropy/sdcv-backends--sdcv-dicts-info-list nil
  "Alist of user sdcv dicts information formed as ((\"dict name\" \"dict-path\")).")

(defvar entropy/sdcv-backends--sdcv-stick-dict nil
  "Current sdcv dict for searching with. ")

(defvar entropy/sdcv-backends--sdcv-manually-fetch nil
  "Whether prompt for manually choose query target when not
explicit match any sdcv candidates.")

(defface entropy/sdcv-backends--sdcv-box-face '((t :box t))
  "Face sytle with box enable.")

;;;;;; error spec prompt

(defun entropy/sdcv-backends--common-message
    (format-string &rest args)
  "Internal common message function similar to `message' but just
used in sdcv backedns context."
  (apply 'message format-string args))

;;;;;; sdcv dictionaries looking up
;;;;;;; dictionary auto search

(defun entropy/sdcv-backends--sdcv-auto-search-dicts ()
  "Automatically search sdcv dict.

This func assuming that all sdcv dict are stored under home
'.stardict' folder and be separated by individual foder.

All sub-dict dir will be checked the validity by func
`entropy/sdcv-backends--sdcv-judge-dictp', see it for detailes.

Func maily for setting the value of variable
`entropy/sdcv-backends--sdcv-dicts-info-list'.
"
  (catch :exit
    (if (and (not entropy/sdcv-backends-sdcv-user-dicts)
             (file-directory-p entropy/sdcv-backends-sdcv-common-dicts-host))
        (let* ((base-dir entropy/sdcv-backends-sdcv-common-dicts-host)
               (dir-list (entropy/cl-list-subdir base-dir))
               (dict-info-list nil))
          (if (and (listp dir-list)
                   (not (null dir-list)))
              (dolist (el dir-list)
                (let ((dict-info (entropy/sdcv-backends--sdcv-judge-dictp el)))
                  (when dict-info
                    (add-to-list 'dict-info-list dict-info))))
            (entropy/sdcv-backends--common-message
             "Could not auto search any dict!")
            (throw :exit nil))
          (if dict-info-list
              (setq entropy/sdcv-backends--sdcv-dicts-info-list dict-info-list)
            (entropy/sdcv-backends--common-message
             "No sdcv dict found!")
            (throw :exit nil)))
      (cond
       ((not (file-directory-p entropy/sdcv-backends-sdcv-common-dicts-host))
        (entropy/sdcv-backends--common-message
         "Can not find 'entropy/sdcv-backends-sdcv-common-dicts-host' folder.")
        (throw :exit nil))
       (entropy/sdcv-backends-sdcv-user-dicts
        (message "You've setting customized sdcv dict path!")
        (throw :exit nil))))))

(defun entropy/sdcv-backends--sdcv-judge-dictp (dict-dir)
  "Check each dict dir's validation state, if valid then returns
dict info list formed as '(dict-name dict-absolute-path)' powered
by func `entropy/sdcv-backends--sdcv-get-dict-info', otherwise return nil.

The valid dict checking mechanism was for finding whether exits
two main dict file named with extension '.idx' 'ifo'."
  (catch :exit
    (let ((sublist (entropy/cl-list-dir-lite dict-dir))
          flist
          (rtn 0))
      (if (and (listp sublist)
               (not (member nil sublist)))
          (dolist (el sublist)
            (when (equal (car el) "F")
              (add-to-list 'flist (cdr el))))
        (entropy/sdcv-backends--common-message
         "Dir %s was empty!" dict-dir)
        (throw :exit nil))
      (if (not flist)
          (setq rtn nil)
        (dolist (el flist)
          (let ((ext (file-name-extension el)))
            (when (or (equal ext "ifo")
                      (equal ext "idx"))
              (cl-incf rtn)))))
      (cond ((and rtn (>= rtn 2))
             (setq rtn (entropy/sdcv-backends--sdcv-get-dict-info dict-dir)))
            (t (setq rtn nil)))
      rtn)))

(defun entropy/sdcv-backends--sdcv-get-dict-info (dict-dir)
  (catch :exit
    (unless (executable-find entropy/sdcv-backends-sdcv-program)
      (entropy/sdcv-backends--common-message
       "Could not found sdcv in your path.")
      (throw :exit nil))
    (let (sdcv-get dict-info rtn)
      (setq sdcv-get
            (shell-command-to-string
             (concat entropy/sdcv-backends-sdcv-program " -l -2 "
                     dict-dir)))
      (setq dict-info
            (replace-regexp-in-string
             "[ \t]+[0-9]+$" ""
             (nth 1 (split-string (string-trim sdcv-get) "\n"))))
      (when (not dict-info)
        (entropy/sdcv-backends--common-message
         "sdcv dict `%s` not a valid sdcv dict format"
         dict-dir)
        (throw :exit nil))
      (setq rtn (list dict-info dict-dir))
      rtn)))

;;;;;;; dictionary check
(defun entropy/sdcv-backends--sdcv-check-dicts ()
  "Check all dicts path stored in variable
`entropy/sdcv-backends-sdcv-user-dicts' validation status and
generate dict info list made for
`entropy/sdcv-backends--sdcv-dicts-info-list'.

If `entropy/sdcv-backends-sdcv-user-dicts' was invalid type or
nil, using func `entropy/sdcv-backends--sdcv-auto-search-dicts'
auto finding valid dicts stored in
`entropy/sdcv-backends-sdcv-common-dicts-host' (see it for
details)."
  (interactive)
  (catch :exit
    (setq entropy/sdcv-backends--sdcv-dicts-info-list nil)
    (cond
     (entropy/sdcv-backends-sdcv-user-dicts
      (unless (and entropy/sdcv-backends-sdcv-user-dicts
                   (listp entropy/sdcv-backends-sdcv-user-dicts)
                   (not (member nil entropy/sdcv-backends-sdcv-user-dicts)))
        (entropy/sdcv-backends--common-message
         "User sdcv dicts specfication error please check \
`entropy/sdcv-backends-sdcv-user-dicts'.")
        (throw :exit nil))
      (let ((valid-dicts nil))
        (dolist (el entropy/sdcv-backends-sdcv-user-dicts)
          (let ((dict-info (entropy/sdcv-backends--sdcv-judge-dictp el)))
            (when dict-info
              (add-to-list 'valid-dicts dict-info))))
        (when (not valid-dicts)
          (entropy/sdcv-backends--common-message
           "Can not found valid dicts.")
          (throw :exit nil))
        (setq entropy/sdcv-backends--sdcv-dicts-info-list valid-dicts)))
     ((not entropy/sdcv-backends-sdcv-user-dicts)
      (entropy/sdcv-backends--sdcv-auto-search-dicts)))))

;;;;;;; dictionaly chosen
(defun entropy/sdcv-backends-sdcv-choose-dict (&optional call-with-interactive)
  "Choose sdcv dict for initializing query state preparing for
sub-procedure.

This func set the stick variable `entropy/sdcv-backends--sdcv-stick-dict' for
the following operation did with, that means you could change
query dict recalling this again."
  (interactive
   (list t))
  (when (null entropy/sdcv-backends--sdcv-dicts-info-list)
    (entropy/sdcv-backends--sdcv-check-dicts)
    (setq entropy/sdcv-backends--sdcv-stick-dict nil))
  (if (and (or (not entropy/sdcv-backends--sdcv-stick-dict)
               call-with-interactive)
           (not (null entropy/sdcv-backends--sdcv-dicts-info-list)))
    (let ((dicts-info entropy/sdcv-backends--sdcv-dicts-info-list)
          chosen)
      (setq chosen (completing-read "Dict for: " dicts-info))
      (setq chosen (nth 1 (assoc chosen dicts-info)))
      (setq entropy/sdcv-backends--sdcv-stick-dict chosen)
      chosen)
    entropy/sdcv-backends--sdcv-stick-dict))

;;;;;; shell command
(defun entropy/sdcv-backends--sdcv-shell-transfer (str dict-path &optional json-exp)
  (let (command
        response
        (default-directory
          (cond
           ((eq system-type 'windows-nt)
            (getenv "temp"))
           (t "/tmp/"))))
    (cond
     ((not (listp dict-path))
      (setq command (format "%s %s %s -n %s -2 %s"
                            entropy/sdcv-backends-sdcv-program
                            (if entropy/sdcv-backends-sdcv-command-prefix
                                entropy/sdcv-backends-sdcv-command-prefix
                              "")
                            (if json-exp
                                "-j"
                              "")
                            str dict-path)))
     ((listp dict-path) (error "Multi dicts support was under development ...")))
    (setq response (shell-command-to-string command)
          entropy/sdcv-core-response-log response)
    response))

;;;;;; response filter
(defun entropy/sdcv-backends--sdcv-extract-json-response (json-response)
  "Extracting sdcv json response object string for filterable
with lisp processing. And return the response final feedback
string used for tooltip or adjacent buffer shown for. See also
core func `entropy/sdcv-backends--sdcv-parse-response-json'.
"
  (let* ((json-list-ob (entropy/sdcv-backends--sdcv-parse-response-json json-response))
         rtn)
    (setq word (car json-list-ob))
    (setq def (nth 1 json-list-ob))
    (setq def-overflow (nth 2 json-list-ob))
    (cond ((and word def)
           (when def-overflow
             (setq def (entropy/cl-truncate-string-with-length
                        def
                        entropy/sdcv-core-response-column-width-max
                        def-overflow)))
           (let* ((word-count (string-width word))
                  (dress-line (entropy/cl-concat-char "-" (+ 3 word-count))))
             (setq rtn (concat dress-line "\n"
                               "⏺ " word "\n"
                               dress-line
                               "\n" def))))
          (t (setq rtn entropy/sdcv-core-response-null-prompt)))
    rtn))

(defun entropy/sdcv-backends--sdcv-parse-response-json (json-response)
  "Core json object parsing func for
`entropy/sdcv-backends--sdcv-extract-json-response', which using `mapcar' for
mapping for the json lisp corresponding vector parsed by
`json-read-from-string' with callbacks func
`entropy/sdcv-backends--sdcv-parse-json-info' and returned it's alist, or t if
with fuzzy match and confirm for fetch with other approach..

Example:

  Sdcv query condition case arg '-j' will response the result with
  json object string which structed as:

  [{
    \"dict\": \"朗道英漢字典5.0\",
    \"word\": \"tooltips\",
    \"definition\": \"n【計】 工具提示\"
  }, {
    \"dict\": \"朗道英漢字典5.0\",
    \"word\": \"toolkit\",
    \"definition\": \"n【計】 工具包\"
  }]

  It's fuzzy match with un-explicitly one, then popup the
  confirmation with two mentod:
  - Query by using another approach (return t)
  - Choose one fuzzy matched candidate (return normal as explicitly procedure)
  "
  (let* ((jsob (json-read-from-string json-response))
         (jsob-alist (mapcar 'entropy/sdcv-backends--sdcv-parse-json-info
                             jsob))
         rtn)
    (if (> (length jsob-alist) 1)
        (setq rtn
              (when entropy/sdcv-backends--sdcv-manually-fetch
                (or
                 (not
                  (yes-or-no-p
                   (format (propertize
                            "Can not exactly match for world '%s'?\nsee similar canis: "
                            'face 'error)
                           (propertize (concat " " entropy/sdcv-core-query-log " ")
                                       'face
                                       'entropy/sdcv-backends--sdcv-box-face))))
                 (assoc (completing-read "choose similar word: "
                                         jsob-alist
                                         nil t)
                        jsob-alist))))
      (setq rtn (car jsob-alist)))
    (if (eq rtn t)
        nil
      rtn)))

(defun entropy/sdcv-backends--sdcv-parse-json-info (json-object-el)
  "Parsing sdcv json lisp object with definition str square model
analyzing based on max width specified by
`entropy/sdcv-core-response-column-width-max'.

Return value as list as sexp (list word def def-width-overflow-lines)."
  (let* ((word (cdr (assoc 'word json-object-el)))
         (def (cdr (assoc 'definition json-object-el)))
         (def-width (entropy/cl-get-string-max-width def entropy/sdcv-core-response-column-width-max))
         (def-width-overflow-lines (plist-get def-width :match-overflow-lines) )
         rtn)
    (setq rtn (list word def def-width-overflow-lines))
    rtn))

;;;;;; show predicate

(defun entropy/sdcv-backends--sdcv-show-predicate (feedback show-method)
  (entropy/sdcv-core-get-word-or-region)
  (cl-case show-method
    ((pos-tip popup)
     (entropy/sdcv-core-common-propertize-feedback feedback))
    (minibuffer-common
     (let ((feedback
            (replace-regexp-in-string
             "\\(| \\) +" "\\1"
             (replace-regexp-in-string
              "-+|" ""
              (entropy/sdcv-backends--make-feedback-single-line feedback))))
           (overflow (when (> (length feedback) (frame-width)) (- (frame-width) 20))))
       (when overflow
         (setq feedback (concat (substring feedback 0 overflow) "...")))
       feedback))
    (t
     feedback)))

;;;;;; sdcv query
(defun entropy/sdcv-backends--query-with-sdcv (query show-method)
  (let* ((dict (entropy/sdcv-backends-sdcv-choose-dict))
         shell-json-response
         feedback
         rtn)
    (setq entropy/sdcv-backends--sdcv-manually-fetch
          (cl-case show-method
            (minibuffer-common nil)
            (t t)))
    (catch :exit
      (if (null dict)
          (and
           (setq rtn entropy/sdcv-core-response-null-prompt)
           (throw :exit nil))
        (setq shell-json-response
              (entropy/sdcv-backends--sdcv-shell-transfer query dict t)))
      (if (or (equal shell-json-response "[]\n")
              (or (string-match-p "^Nothing similar to" shell-json-response)
                  (string-match-p "sorry :(" shell-json-response)))
          (setq rtn entropy/sdcv-core-response-null-prompt)
        (setq rtn
              (entropy/sdcv-backends--sdcv-extract-json-response
               shell-json-response))))
    rtn))

;;;;; wudao
(defun entropy/sdcv-backends--wudao-require ()
  (let* ((wd-path (executable-find "wd"))
         (error-message
          "You must install 'wudao-dict' from https://github.com/c0001/Wudao-dict.git.

And install it by 'make install'. Finally check whether '~/.local/bin' in your \"PATH\".")
         wd-host)
    (unless (stringp wd-path)
      (error error-message))
    (setq wd-host (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (car (file-attributes wd-path))))))
    (add-to-list 'load-path (expand-file-name "emacs" wd-host))
    (require 'wudao-query)))

(defun entropy/sdcv-backends--wudao-show-predicate (feedback show-method)
  (cl-case show-method
    (minibuffer-common
     (entropy/sdcv-backends--make-feedback-single-line feedback))
    (t
     feedback)))

(defun entropy/sdcv-backends--query-with-wudao-by-hash (query show-method)
  (entropy/sdcv-backends--wudao-require)
  (let ((response (or (ignore-errors
                        (wudao/query-word-by-hash
                         query
                         (eq show-method 'adjacent-common)))
                      entropy/sdcv-core-response-null-prompt)))
    response))

(defun entropy/sdcv-backends--query-with-wudao-by-command (query show-method)
  (entropy/sdcv-backends--wudao-require)
  (let ((response (or (ignore-errors
                        (wudao/query-word-by-command
                         query
                         (eq show-method 'adjacent-common)))
                      entropy/sdcv-core-response-null-prompt)))
    response))

;;; provide

(dolist
    (el
     '((wudao-hash :query-function entropy/sdcv-backends--query-with-wudao-by-hash
                   :show-predicate entropy/sdcv-backends--wudao-show-predicate)
       (wudao-command :query-function entropy/sdcv-backends--query-with-wudao-by-command
                      :show-predicate entropy/sdcv-backends--wudao-show-predicate)
       (sdcv :query-function entropy/sdcv-backends--query-with-sdcv
             :show-predicate entropy/sdcv-backends--sdcv-show-predicate
             :show-face entropy/sdcv-backends--auto-face-common)
       (youdao :query-function entropy/sdcv-backends--query-with-youdao
               :show-predicate entropy/sdcv-backends--youdao-show-predicate)
       (bing :query-function entropy/sdcv-backends--query-with-bing
             :show-predicate entropy/sdcv-backends--bing-show-predicate)
       (google :query-function entropy/sdcv-backends--query-with-google
               :show-face entropy/sdcv-backends--auto-face-common)))
  (add-to-list 'entropy/sdcv-core-query-backends-register el))

(provide 'entropy-sdcv-backends)
