;;; entropy-sdcv.el --- entropy emacs sdcv front-end

;; Copyright (C) 20181211  Entropy

;; Author:           Entropy <bmsac0001@gmail.com>
;; Maintainer:       Entropy <bmsac001@gmail.com>
;; URL:              https://github.com/c0001/entropy-sdcv
;; Package-Version:  20191023.2003
;; Version:          0.1.1
;; Created:          2018-12-11 12:48:04
;; Keywords:         sdcv
;; Compatibility:    GNU Emacs 26.1;
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (entropy-common-library "0.1.0") (popup "0.5.3") (f "0.20.0") (youdao-dictionary "0.4") (bing-dict) (google-translate) (posframe "0.5.0") (pos-tip) (tooltip))
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
;;
;;; Commentary:
;;
;;;; Preamble
;; :PROPERTIES:
;; :CUSTOM_ID: h-2aee4d32-27fa-4237-bbf2-f5544adf63c4
;; :END:
;;
;; =sdcv= was one cli wide-dicts query tool. It was derived from the
;; opensource e-dict application [[http://www.huzheng.org/stardict/][startdict]] founded by [[http://huzheng.org/][Huzheng]].
;;
;; =sdcv= can parse stardict e-dict formt and return through the cli
;; response, various internal optional args can charge the reponse type
;; and the query style, ~sdcv --help~ gives you the prompts.
;;
;; =entrop-sdcv= is the front-end of sdcv and be as the pure elisp
;; written and aims that help you quickly query words within emacs buffer
;; without any window exchanging and be using the offline dict instead of
;; retrieving the response from online dict server e.g. bing-dic or
;; google-translation. This package inspired by [[https://github.com/manateelazycat/sdcv][manateelazycat's sdcv]]
;; package, but reducing some unnecessary feature and rebuild from fresh
;; new coding context.
;;
;; Before using and configuring this package, you should learn some
;; common basic command interaction of it's backend sdcv for
;; understanding some effection and the running mechanism of this
;; package. Below subsection shows what about this.
;;
;;;;; =sdcv= interaction:
;; :PROPERTIES:
;; :CUSTOM_ID: h-809ff094-b70d-40cd-84d2-1e2c0d4bfc51
;; :END:
;;
;; sdcv is simple, cross-platform text-base utility for work with
;; dictionaries in StarDict's format. The word from "list of words" may
;; be string with leading '/' for using Fuzzy search algorithm, string
;; may contain '?' and '*' for using regexp search. It works in
;; interactive and non-interactive mode. Press Ctrl+D to exit from
;; interactive mode.
;;
;; From sdcv help prompting response:
;; #+BEGIN_EXAMPLE
;; ~/.e/e/s/entropy-sdcv/ $ sdcv --help
;; Usage:
;;   sdcv [OPTION...]  words
;;
;; Help Options:
;;   -h, --help                     Show help options
;;
;; Application Options:
;;   -v, --version                  display version information and exit
;;   -l, --list-dicts               display list of available dictionaries and exit
;;   -u, --use-dict=bookname        for search use only dictionary with this bookname
;;   -n, --non-interactive          for use in scripts
;;   -j, --json-output              print the result formatted as JSON
;;   -e, --exact-search             do not fuzzy-search for similar words, only return exact matches
;;   -0, --utf8-output              output must be in utf8
;;   -1, --utf8-input               input of sdcv in utf8
;;   -2, --data-dir=path/to/dir     use this directory as path to stardict data directory
;;   -x, --only-data-dir            only use the dictionaries in data-dir, do not search in user and system directories
;;   -c, --color                    colorize the output
;;
;; #+END_EXAMPLE
;;
;; The basic query operation can be done with (query 'apple' for demo):
;; #+BEGIN_EXAMPLE
;; $ sdcv apple
;;
;; Word: apple
;;
;; [ˋæpL;ˊæpl]
;; < ap.ple >
;; [基本字彙]
;; <<名詞>>
;; 1 a. 蘋果樹
;;   b. 蘋果
;; 2 a. 類似蘋果樹的植物
;;      如: 野生蘋果樹, 釋迦果樹等
;;   b. 類似蘋果的果實
;; <<慣用語>>
;; apple of (one's) eye
;; 掌上明珠, 心肝寶貝
;; Her grandson is the apple of her eye.  她的孫子是她的至寶
;; #+END_EXAMPLE
;;
;;;;; =sdcv= dicts choosen
;; :PROPERTIES:
;; :CUSTOM_ID: h-59d86f4e-8b76-4a37-8cbb-3583ecbb7e54
;; :END:
;;
;; *Valid dict:*
;;
;; All stardict dicts has its dir structure, which was one dimension
;; consistsof sets of individual files, below files denoted are
;; necessary:
;;
;; - *.ifo file:     Dict information indication.
;; - *.idx file:     Dict index.
;; - *.dict.dz file: Dict compressed database.
;;
;;
;; *Default:*
;;
;; =sdcv= default using the dict archived with searching for environment
;; variable =STARDICT_DATA_DIR=, and get the dict info under its 'dict'
;; dir , thus if your set this envrionmet be as =~/.stardict=, then it
;; will will search your querying about by the dict under
;; =~/.startdict/dict/=.
;;
;; *Realtime specification:*
;;
;; =sdcv= optional arg =-2= gives the description that "use this
;; directory as path to stardict data directory", thus you get the
;; ability to search for individual dict, this as one demo from my
;; eshell:
;;
;; #+BEGIN_EXAMPLE
;; ~/.e/e/s/entropy-sdcv/ $ sdcv apple -2 ~/.stardict/stardict-xdict-ec-big5_fix-2.4.2/
;; Found 1 items, similar to apple.
;; -->XDICT英漢辭典
;; -->apple
;;
;; [ˊæpl]
;; n. 蘋果,蘋果電腦公司;人,家伙;手榴彈
;; #+END_EXAMPLE
;;
;;
;;;; Requirements
;; :PROPERTIES:
;; :CUSTOM_ID: h-5259b341-5966-4fbd-a14e-dcba4c8b81df
;; :END:
;;
;; There's sets of melpa emacs extensions are required as the api
;; provider for this package:
;;
;; - [[https://github.com/pitkali/pos-tip][pos-tip]]:
;;
;;   Show query response with tooltip sub-window by emacs builtin
;;   ~x-show-tip~ as the subroutine, used in *graphic* session only,
;;   support emacs version above(include) 22.
;;
;; - [[https://github.com/tumashu/posframe/tree/d141d56d1c747bca51f71f04fdb9d4d463996401][posframe]]: 
;;
;;   show query response with tooltip sub-window by emacs builtin
;;   =child-frame= feature if on emacs-version upper than 26.
;;
;; - [[https://github.com/auto-complete/popup-el/tree/80829dd46381754639fb764da11c67235fe63282][popup]]:
;;
;;   show query response with tooltip sub-window in generally
;;   emacs-version whichever you ran with without graphic display session
;;   limitation.
;;
;; - json:
;;
;;   Parse sdcv's json response, it usually be built-in with.
;;
;; - [[http://github.com/rejeep/f.el][f]]
;;
;;   Working for file based operation.
;;
;; - cl:
;;
;;   Get some common-lisp featuer, this usually be as built-in feature.
;;
;; - [[https://github.com/xuchunyang/youdao-dictionary.el][youdao-dictionary]]
;;
;;   Be as the optioanl exteranl online query channel when no sdcv
;;   response got.
;;
;; - [[https://github.com/cute-jumper/bing-dict.el][bing-dict]]
;;
;;   Be as the optioanl exteranl online query channel when no sdcv
;;   response got.
;;
;; - [[https://github.com/atykhonov/google-translate/tree/17a1ddc074b96cdc3b8199ccb06824a7a95bf9ff][google-translate]]
;;
;;   Be as the optioanl exteranl online query channel when no sdcv
;;   response got.
;;
;;
;; Also as other entropy-built package, the package
;; [[https://github.com/c0001/entropy-common-library][entropy-common-library]] was required on the core position, you can get
;; it from entropy-emacs repositor.
;;
;;;; Dependencies
;; :PROPERTIES:
;; :CUSTOM_ID: h-599612f8-33fc-4be9-bf42-fd18eb05c016
;; :END:
;;
;; The external dependency required was only one =sdcv=, you should get
;; it from [[https://github.com/Dushistov/sdcv][github]] and compile and install it by you self.
;;
;; It quiet simple for the way of compiling on unix-like platform:
;;
;; #+BEGIN_SRC bash
;;   # For building
;;   mkdir /tmp/build-sdcv
;;   cd /tmp/build-sdcv
;;   cmake path/to/source/code/of/sdcv
;;   make
;;
;;   # If you enable nls then you should also type
;;   make lang
;;
;;   # To install
;;   make install
;; #+END_SRC
;;
;; *Build on windows:*
;;
;; Original sdcv was not support Windows platform, thus the way for
;; compiling it basic on pure Windows platform must be patching a lot
;; into it source, however I don't know C enough for understanding it's
;; raw mechanism. However, we can use the posix environment on windows
;; called Msys2(basic on cygwin and Mingw project) to compiling it and
;; run it within this posix environment.
;;
;; Fist of all, install [[https://www.msys2.org/][Msys2]] in your PC and clone the minor patched
;; version of sdcv from [[https://github.com/c0001/sdcv][c0001/sdcv]] with the =patch= branch, and then
;; build using Msys tool chain by satisfying all the dependencies founded
;; the description in repo's README.
;;
;;;; Installation
;; :PROPERTIES:
;; :CUSTOM_ID: h-8e9efb70-4fa5-4556-a143-7b58ab8009fa
;; :END:
;;
;; For famous emacs package configuration management tool =use-package=
;; you can using below coding snippet:
;;
;; #+BEGIN_SRC  emacs-lisp
;;   (use-package entropy-sdcv
;;     :ensure nil
;;     :path "path-to-your-load-path"
;;     :commands (entropy/sdcv-search-at-point-tooltip
;;                entropy/sdcv-search-input-adjacent))
;; #+END_SRC
;;
;; The forcefully 'utf-8' transfer advice was needed as your current
;; coding system are local setting not equaling for 'utf-8', this case
;; usually occurred in windows platform, you could using function like:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (defun  my/lang-set-utf8 ()
;;     (string= lang "UTF-8")
;;     (set-language-environment "UTF-8")
;;     (prefer-coding-system 'utf-8-unix))
;; #+END_SRC
;;
;; #+BEGIN_QUOTE
;; The utf-8 language environment requirement was rely on the cases that
;; some dict information string response at dict query step will be messy
;; coding for. 
;; #+END_QUOTE
;;
;; And then advice the each entropy-sdcv interaction command as:
;; #+BEGIN_SRC emacs-lisp
;;   (with-eval-after-load 'entropy-sdcv
;;     (defun my/sdcv--lang-advice (&rest args)
;;       (my/lang-set-utf8))
;;     (advice-add 'entropy/sdcv-search-at-point-tooltip :before #'my/sdcv--lang-advice)
;;     (advice-add 'entropy/sdcv-search-input-adjacent :before #'my/sdcv--lang-advice))
;; #+END_SRC
;;
;;; Configuration
;; :PROPERTIES:
;; :CUSTOM_ID: h-ec2ff0d8-4223-48f5-b4e9-6ff1b9450df3
;; :END:
;;
;; =entropy-sdcv= exposed dozen of customized feature for user to specify
;; along with their own taste. For the customizing level dividing into
;; what, I gives below customized level categories for understanding and
;; got the proper way of selecting which level you should give a try.
;;
;;;; Classical  suggested configuration  
;; :PROPERTIES:
;; :CUSTOM_ID: h-95394510-3e8b-4370-9424-6d1bc5b17b6b
;; :END:
;;
;; - =entropy/sdcv-user-dicts= : Specified your sdcv dict collection
;;   directory location.
;;
;;   As mentionded upons sections, this variale's default value is
;;   "~/.stardict" which contain the dicts collections structed as:
;;   #+BEGIN_EXAMPLE
;;     .
;;     ├── stardict-21shijishuangxiangcidian-big5-2.4.2
;;     ├── stardict-21shijishuangyukejicidian-big5-2.4.2
;;     ├── stardict-2wwords-2.4.2
;;     ├── stardict-CDICTbig-2.4.2
;;     ├── stardict-cdict-big5-2.4.2
;;     ├── stardict-cedict-big5-2.4.2
;;     ├── stardict-chenyixiaofoxuechangjiancihui-2.4.2
;;     ├── stardict-eng-ch-eng-buddhist-2.4.2
;;     ├── stardict-faxiangcidian-big5-2.4.2
;;     ├── stardict-foguangdacidian-big5-2.4.2
;;     ├── stardict-foxuedacidian-big5-2.4.2
;;     ├── stardict-handedict-big5-2.4.2
;;     ├── stardict-kdic-ec-14w-big5-2.4.2
;;     ├── stardict-langdao-ce-big5-2.4.2
;;     ├── stardict-langdao-ec-big5-2.4.2
;;     ├── stardict-lazyworm-ce-big5-2.4.2
;;     ├── stardict-lazyworm-ec-big5-2.4.2
;;     ├── stardict-oxford-big5-2.4.2
;;     ├── stardict-sanzunfasu-2.4.2
;;     ├── stardict-soothill-buddhist-2.4.2
;;     ├── stardict-sun_dict_e2t-2.4.2
;;     ├── stardict-xdict-ce-big5_fix-2.4.2
;;     ├── stardict-xdict-ce-big5-2.4.2
;;     ├── stardict-xdict-ec-big5_fix-2.4.2
;;     └── stardict-xdict-ec-big5-2.4.2
;;
;;   #+END_EXAMPLE
;;
;;   Each subfolder of it was one dict folder structed as the description
;;   of [[#h-59d86f4e-8b76-4a37-8cbb-3583ecbb7e54][here]], and this show what your must noted due to that whatever
;;   dict collection location you specified for, the collection dir
;;   structer must formed as this default.
;;
;;   On what you see here, the demo of dict collection shown as was what
;;   my self using for, if your want to get the same dict collection
;;   without the toughly searching the usable dicts by paying further
;;   extra vitality which annoyed to you, you can clone follow repo as
;;   with none warranty:
;;
;;   : git clone https://github.com/zdict/dictionaries.git ~/.stardict
;;
;; - =entropy/sdcv-program= : Specified sdcv binary calling path.
;;
;;   In generally cases about, it's you should involve 'sdcv' binary from
;;   your shell =PATH= directly but be from what you specified path
;;   location. But it's just the suggestion for various benefit for
;;   install sdcv in your =PATH=, OFC you can specified the sdcv calling
;;   path by setting =entropy/sdcv-program= as the path string.
;;
;;;; Useful minor feature configuration
;; :PROPERTIES:
;; :CUSTOM_ID: h-b8dcaa05-788f-4292-acbe-a2400edb705c
;; :END:
;;
;; - =entropy/sdcv-command-prefix= : extra sdcv optioanl args transfer to
;;   shell commmand.
;;
;;   The internal default sdcv shell command subprocess getting with
;;   optional arg '-n' which show that "do not using interaction way."
;;   which just response directly without queried returning candidates
;;   selection prompt interactivation.
;;
;;   In which you want to specified the case of be as without fuzzy
;;   matching words quried about, you can sets varible to =-e=.
;;
;; - =entropy/sdcv-tooltip-type= : Chosen the tooltip type with 'popup'
;;   'posframe' or 'pos-tip' type.
;;
;;   By default, when `emacs-version' less than '26.1' using 'popup' or
;;   'pos-tip' else than using 'posframe', because posframe using emacs
;;   featuer chiled-frame which built-in on the version upper than thus.
;;
;;   Note:
;;
;;   When using terminal based UI, limition of `posframe' and 'pos-tip'
;;   will not be supported for, forcing defaultly set it to 'popup'."
;;
;; - =entropy/sdcv-external-query-type= : Chosen the external dict type.
;;
;;   While there's none response returned by sdcv which case that sdcv
;;   can not find the exact word matching of current input(or the
;;   'thing-at-point'), =entropy-sdcv= will try it from querying for
;;   external online dict powered by [[https://github.com/cute-jumper/bing-dict.el][bing-dict]], [[https://github.com/xuchunyang/youdao-dictionary.el][youdao-dict]], and
;;   [[https://github.com/atykhonov/google-translate/tree/17a1ddc074b96cdc3b8199ccb06824a7a95bf9ff][google-translation]].
;;
;;   The valid value of this variable was (symbol type):
;;   1. ’youdao 
;;   2. ’bing
;;   3. ’google
;;
;;
;; 
;;; Chanage log
;; 
;; 2019/10/23
;;      * version v0.1.1 pop out
;;        - Using new auto-gen's tooltip face render
;;        - Add `pos-tip' tooltip type.
;; 
;; 2018/12/11
;;      * First release pop out v0.1.0
;;
;;
;;; Code:
;;;; require
(require 'tooltip)
(require 'entropy-common-library)
(condition-case nil (require 'posframe) (error (message "Warn: You haven't install posframe!")))
(require 'popup)
(require 'pos-tip)
(require 'json)
(require 'f)
(require 'cl-lib)
(require 'youdao-dictionary)
(require 'bing-dict)
(require 'google-translate)

;;;; variable declaration
;;;;; group
(defgroup entropy/sdcv-group nil
  "Group for `entropy-sdcv' feature."
  :group 'extensions)

;;;;; custom

(defcustom entropy/sdcv-user-dicts nil
  "User sdcv dicts directories list"
  :type 'list
  :group 'entroy/sdcv-group)

(defcustom entropy/sdcv-program (if (string-equal system-type "darwin") "/usr/local/bin/sdcv" "sdcv")
  "Sdcv programe name custom specification, if it not in your
  path, you are required to specify it's path with it's filename
  e.g. '/bin/sdcv'."
  :type 'string
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-command-prefix nil
  "Prefix command argumetns for sdcv exec."
  :type 'string
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-tooltip-type
  (if (display-graphic-p)
      (if (version< emacs-version "26.1") 'popup 'posframe)
    'popup)
  "The sdcv response string container type. 

There's three type of tooltip type for chosen, i.e. 'pos-tip',
'popup' and 'posframe'.

By default, when `emacs-version' less than '26.1' using 'popup' or
'pos-tip' else than using 'posframe', because posframe using emacs
featuer chiled-frame which built-in on the version upper than
thus.

Note:

When using terminal based UI, limition of `posframe' and 'pos-tip'
will not be supported for, forcing defaultly set it to 'popup'."
  :type 'symbol
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-pos-tip-height-stretch 1.5
  "The height scale for `pos-tip-show-no-propertize'.

Cause for the bug for `pos-tip-show-no-propertize' can not
caculate the multibyte string block height, this var will do the
proper stretching."
  :type 'sexp
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-response-filter-hook nil
  "Hook for filter with sdcv's response."
  :type 'sexp
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-external-query-type 'bing
  "External query application for failed query from sdcv.

Available chosen are:
- 'youdao 
- 'bing
- 'google
"
  :type 'sexp
  :group 'entropy/sdcv-group)

(defcustom entropy/sdcv-tooltip-border-width 10
  "posframe tooltip width for sdcv callbacking show as."
  :type 'integer
  :group 'entropy/sdcv-group)

;;;;; internal var
(defvar entropy/sdcv--tooltip-last-point nil
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar entropy/sdcv--tooltip-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

(defvar entropy/sdcv--fail-notify-string "Nothing found... \n Try from other dict! ^_^"
  "This string is for notify user when search fail.")

(defvar entropy/sdcv--dicts-info nil
  "Alist of user sdcv dicts information formed as ((\"dict name\" \"dict-path\")).")

(defvar entropy/sdcv--showed-buffer "*entropy/sdcv-showed-buffer*"
  "Entropy sdcv adjacent feedback presenting buffer name.")

(defvar entropy/sdcv--stick-dict nil
  "Current sdcv dict for searching with. ")

(defvar entropy/sdcv--show-response-in-adjacently nil
  "Indicator for whether show sdcv response in adjacent buffer.")

(defvar entropy/sdcv--tooltip-buffer "*entropy/sdcv-tooltip*"
  "Entropy sdcv tooltip default buffer name. Both using for
  posframe or popup shown mechanism.")

(defvar entropy/sdcv--response-column-width-max 60
  "Query feedback info definitions string overflow width used for
func `entropy/sdcv--extract-json-response' to fill lines
destructively or display with truncated occur.")

(defvar entropy/sdcv--response-null-prompt "In the beginning, there's darkness!"
  "Feedback string for none-matching session.")

(defvar entropy/sdcv--query-log nil
  "Variable stored current query string for package debug using.")

(defvar entropy/sdcv--response-log nil
  "Variable stored current query response string for package debug using.")

(defvar entropy/sdcv--bing-dict-response nil
  "External bing dict process feedback response.")

(defvar entropy/sdcv--bing-adjacent-buffer "*entropy/sdcv-external-bing*"
  "Buffer name for create external dict bing-dict adjacent buffer.")

(defvar entropy/sdcv--origin-lang-env (getenv "LANG")
  "Stored user origin specific env lang set.")

(defvar entropy/sdcv--specific-lang "en_US.UTF-8"
  "Pre ordered system lang set used during sdcv query process.")


;;;;; default face
(defface entropy/sdcv-tooltip-face '((t ()))
  "The tooltip buffer background face. ")

(defun entropy/sdcv-tooltip-bgLight-color ()
  "Tooltip buffer background color for dark-theme."
  (face-attribute 'tooltip :background))

(defun entropy/sdcv-tooltip-bgDark-color ()
  "Tooltip buffer background color for dark-theme."
  (face-attribute 'tooltip :background))

(defface entropy/sdcv-box-face '((t :box t))
  "Face sytle with box enable.")

;;;; library
;;;;; lang envrionment pre check
(defun entropy/sdcv--set-specific-lang-env ()
  (unless (equal entropy/sdcv--specific-lang
                 (getenv "LANG"))
    (setenv "LANG" entropy/sdcv--specific-lang)))

(defun entropy/sdcv--recovery-user-origin-lang-env ()
  (unless (equal entropy/sdcv--origin-lang-env
                 (getenv "LANG"))
    (setenv "LANG" entropy/sdcv--origin-lang-env)))

(defun entropy/sdcv--lang-set-process (oldfunc &rest args)
  "The around advice for set specification emacs lang set when
calling query process for the reason that:

sdcv cli responsed alwasys for utf-8 encoding information as, the
none utf-8 lang set will not decoding the response string
correctly, the particular problem was for func
`entropy/sdcv--check-dicts' which will get the unicode
rsepresentation dict name string when the emacs lang set was not
the subject of utf-8 group."
  (entropy/sdcv--set-specific-lang-env)
  (apply oldfunc args)
  (entropy/sdcv--recovery-user-origin-lang-env))

;;;;; dictionary auto search

(defun entropy/sdcv--auto-search-dicts ()
  "Automatically search sdcv dict.

  This func assuming that all sdcv dict are stored under home
  '.stardict' folder and be separated by individual foder.

  All sub-dict dir will be checked the validity by func
  `entropy/sdcv--judge-dictp', see it for detailes.

  Func maily for setting the value of variable
  `entropy/sdcv--dicts-info'.
  "
  (if (and (not entropy/sdcv-user-dicts)
           (f-dir-p "~/.stardict"))
      (let* ((base-dir "~/.stardict")
             (dir-list (entropy/cl-list-subdir base-dir))
             (dict-info-list nil))
        (if (and (listp dir-list)
                 (not (null dir-list))
                 (not (member nil dir-list)))
            (dolist (el dir-list)
              (let ((dict-info (entropy/sdcv--judge-dictp el)))
                (when dict-info
                  (add-to-list 'dict-info-list dict-info))))
          (error "Could not auto search any dict!"))
        (when (and dict-info-list
                   (not (member nil dict-info-list)))
          (setq entropy/sdcv--dicts-info dict-info-list)))
    (cond
     ((not (f-dir-p "~/.stardict"))
      (error "Can not find '~/.stardict' folder."))
     (entropy/sdcv-user-dicts
      (error "You've setting customized sdcv dict path!")))))

(defun entropy/sdcv--judge-dictp (dict-dir)
  "Check each dict dir's validation state, if valid then returns dict
  info list formed as '(dict-name dict-absolute-path)' powered by
  func `entropy/sdcv--get-dict-info', otherwise return nil.

  The valid dict checking mechanism was for finding whether exits
  two main dict file named with extension '.idx' 'ifo'."
  (let ((sublist (entropy/cl-list-dir-lite dict-dir))
        flist
        (rtn 0))
    (if (and (listp sublist)
             (not (member nil sublist)))
        (dolist (el sublist)
          (when (equal (car el) "F")
            (add-to-list 'flist (cdr el))))
      (error (format "Dir %s was empty!" dict-dir)))
    (if (not flist)
        (setq rtn nil)
      (dolist (el flist)
        (let ((ext (file-name-extension el)))
          (when (or (equal ext "ifo")
                    (equal ext "idx"))
            (cl-incf rtn)))))
    (cond ((and rtn (>= rtn 2))
           (setq rtn (entropy/sdcv--get-dict-info dict-dir)))
          (t (setq rtn nil)))
    rtn))

(defun entropy/sdcv--get-dict-info (dict-dir)
  (unless (executable-find entropy/sdcv-program)
    (error "Could not found sdcv in your path."))
  (let (sdcv-get dict-info rtn)
    (setq sdcv-get
          (shell-command-to-string
           (concat entropy/sdcv-program " -l -2 "
                   dict-dir)))
    (setq dict-info (replace-regexp-in-string "[ \t]+[0-9]+$" ""
                                              (nth 1 (split-string (string-trim sdcv-get) "\n"))))
    (when (not dict-info) (error "dict error"))
    (setq rtn (list dict-info dict-dir))
    rtn))

;;;;; dictionary check
(defun entropy/sdcv--check-dicts ()
  "Check all dicts path stored in variable `entropy/sdcv-user-dicts'
  validation status and generate dict info list made for
  `entropy/sdcv--dicts-info'.

  If `entropy/sdcv-user-dicts' was invalid type or nil, using func
  `entropy/sdcv--auto-search-dicts' auto finding valid dicts stored
  in '~/.stardict' (see it for details)."
  (interactive)
  (setq entropy/sdcv--dicts-info nil)
  (cond
   (entropy/sdcv-user-dicts
    (unless (and entropy/sdcv-user-dicts
                 (listp entropy/sdcv-user-dicts)
                 (not (member nil entropy/sdcv-user-dicts)))
      (error "Dicts specfication error."))
    (let ((valid-dicts nil))
      (dolist (el entropy/sdcv-user-dicts)
        (let ((dict-info (entropy/sdcv--judge-dictp el)))
          (when dict-info
            (add-to-list 'valid-dicts dict-info))))
      (when (not valid-dicts)
        (error "Can not found valid dicts."))
      (setq entropy/sdcv--dicts-info valid-dicts)))
   ((not entropy/sdcv-user-dicts)
    (entropy/sdcv--auto-search-dicts))))

(advice-add 'entropy/sdcv--check-dicts :around #'entropy/sdcv--lang-set-process)

;;;;; dictionaly chosen
(defun entropy/sdcv--choose-dict (&optional call-with-interactive)
  "Choose sdcv dict for initializing query state preparing for
  sub-procedure.

  This func set the stick variable `entropy/sdcv--stick-dict' for
  the following operation did with, that means you could change
  query dict recalling this again."
  (interactive
   (list t))
  (unless (and entropy/sdcv--dicts-info
               (listp entropy/sdcv--dicts-info)
               (not (member nil entropy/sdcv--dicts-info)))
    (entropy/sdcv--check-dicts)
    (setq entropy/sdcv--stick-dict nil))
  (if (or (not entropy/sdcv--stick-dict)
          call-with-interactive)
    (let ((dicts-info entropy/sdcv--dicts-info)
          chosen)
      (setq chosen (completing-read "Dict for: " dicts-info))
      (setq chosen (nth 1 (assoc chosen dicts-info)))
      (setq entropy/sdcv--stick-dict chosen)
      chosen)
    entropy/sdcv--stick-dict))

;;;;; string obtains
(defun entropy/sdcv--get-word-or-region ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word t)))

;;;;; external dict query
(defun entropy/sdcv--query-with-external (query show-type app-type)
  "The tuned dispatcher for `entropy/sdcv--command-router' for
  shunting to external dict application when sdcv query result match
  none. 

  Within `entropy-sdcv', there's three supported external dict
  application, all of them are melpa emacs extension:

  1. youdao
  2. bing
  3. google

  The arg app-type was specified with symbol named as above list
  shown. 

  Arg show-type was necessary for did as. It's valid value are
  'tooltip' and 'adjacent', which means for showing as tooltip
  window at point or using popup adjacent buffer with respectively."
  (let ()
    (cl-case app-type
      (youdao
       (entropy/sdcv--query-with-youdao query show-type))
      (bing
       (entropy/sdcv--query-with-bing query show-type))
      (google
       (entropy/sdcv--query-with-google query show-type)))
    nil))

;;;;;; youdao
(defun entropy/sdcv--query-with-youdao (query show-type)
  (cl-case show-type
    (tooltip
     (entropy/sdcv--show-with-tooltip (youdao-dictionary--format-result query)))
    (adjacent
     (youdao-dictionary-search query)))
  nil)

;;;;;; bing
(defun entropy/sdcv--query-with-bing (query show-type)
  (cl-case show-type
    (tooltip
     (entropy/sdcv--show-with-tooltip (entropy/sdcv--bing-dict-url-retrieve query)))
    (adjacent
     (with-current-buffer (get-buffer-create entropy/sdcv--bing-adjacent-buffer)
       (erase-buffer)
       (insert (entropy/sdcv--bing-dict-url-retrieve query))
       (goto-char (point-min)))
     (display-buffer entropy/sdcv--bing-adjacent-buffer)))
  nil)


(defun entropy/sdcv--bing-dict-url-retrieve (query)
  (setq entropy/sdcv--bing-dict-response nil)
  (let (info rtn buffer)
    (setq buffer
          (save-match-data
            (url-retrieve-synchronously
             (concat bing-dict--base-url
                     (url-hexify-string query))
             t t)))
    (with-current-buffer buffer
      (entropy/sdcv--bing-dict-brief-cb (decode-coding-string query 'utf-8))
      (erase-buffer)
      (setq info entropy/sdcv--bing-dict-response)
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
        (setq rtn entropy/sdcv--response-null-prompt))
       ((equal (car info) "machine-translation")
        (insert (nth 3 info) "\n\n" (nth 4 info))
        (setq rtn (buffer-string)))))
    rtn))

(defun entropy/sdcv--bing-dict-brief-cb (keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (setq keyword (propertize keyword
                            'face
                            'font-lock-keyword-face))
  (condition-case nil
      (if (bing-dict--has-machine-translation-p)
          (setq entropy/sdcv--bing-dict-response
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
                (setq entropy/sdcv--bing-dict-response
                      (list
                       "exact-definition"
                       keyword
                       pronunciation
                       bing-dict-word-def-separator
                       short-defstr)))
            (let ((sounds-like-words (bing-dict--get-sounds-like-words)))
              (if sounds-like-words
                  (setq entropy/sdcv--bing-dict-response
                        (list
                         "sounds-like"
                         bing-dict--sounds-like-text
                         bing-dict-word-def-separator
                         sounds-like-words))
                (setq entropy/sdcv--bing-dict-response
                      (list "no-matched" bing-dict--no-result-text)))))))
    (error (bing-dict--message bing-dict--no-result-text))))


;;;;;; google
(defun entropy/sdcv--query-with-google (query show-type)
  (cl-case show-type
    (tooltip
     (google-translate-translate nil nil query 'popup))
    (adjacent
     (google-translate-translate nil nil query))))

;;;;; command transfer
;;;;;; shell port
(defun entropy/sdcv--shell-transfer (str dict-path &optional json-exp)
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
                            entropy/sdcv-program
                            (if entropy/sdcv-command-prefix
                                entropy/sdcv-command-prefix
                              "")
                            (if json-exp
                                "-j"
                              "")
                            str dict-path)))
     ((listp dict-path) (error "Multi dicts support was under development ...")))
    (setq response (shell-command-to-string command)
          entropy/sdcv--response-log response)
    response))

;;;;;; command router
(defun entropy/sdcv--command-router (str dict-path show-type  &optional recall)
  "Router for query with while the state both of matching
response or non-matched of sdcv cli and the state when query
string was sentence which not suitable for doing with sdcv as
then querying them from other external dict application defined
internal in `entropy/sdcv--query-with-external'.

Note: On windows platform, multibyte query was not supported
withs sdcv cli even if the query string was single word e.g. cjk
char string as. Because of that windows using code-page for
decoding each char in command line shell 'cmd', and emacs just
supported encoding query args with locale codepage for it, which
means for example that when you locale code page was 'gbk' that
the query string will be sent with code encoding format with
'gbk' to sdcv cli even if you using `encode-coding-string' to
forcefully encoding query string to 'utf-8', but sdcv just
supportting recieving 'utf-8' query as. For such detailes see
emacs mailing list
https://lists.gnu.org/archive/html/emacs-devel/2016-01/msg00406.html.

Or you can enable WIN10 new beta option for globally UTF-8 support.
"
  (let (str-single-p
        (str-list (split-string str " " t))
        shell-response
        (entropy/sdcv--show-response-in-adjacently
         (if (eq show-type 'adjacent) t nil)))
    (if (> (length str-list) 1)
        (setq str-single-p 'nil)
      (setq str-single-p 't))
    (cond
     ((and (and (eq system-type 'windows-nt)
                (not (eq w32-ansi-code-page 65001)))
           (let ($return (str-elist (split-string str "" t)) (str-count 0))
             (dolist (el str-elist)
               (cl-incf str-count))
             (when (> (string-width str) str-count)
               (setq $return t))
             $return))
      (entropy/sdcv--query-with-external str show-type entropy/sdcv-external-query-type)
      (message (concat "Emacs on windows does not support multibye args transfer to process,"
                       "thus using network query instead.")))
     ((or (eq str-single-p nil)
          recall)
       (entropy/sdcv--query-with-external str show-type entropy/sdcv-external-query-type))
     ((eq str-single-p t)
      (setq shell-response
            (entropy/sdcv--shell-transfer
             str dict-path t))
      (if (equal shell-response "[]\n")
          (entropy/sdcv--command-router str dict-path show-type t)
        (let ((feedback (entropy/sdcv--extract-json-response shell-response)))
          (if (eq feedback t)
              (entropy/sdcv--command-router str dict-path show-type t)
            (cond
             ((eq show-type 'tooltip)
              (entropy/sdcv--show-with-tooltip feedback))
             ((eq show-type 'adjacent)
              (entropy/sdcv--show-with-buffer feedback))))))))))

;;;;; command response show
;;;;;; response filter
;;;;;;; generic (under development)
;;;;;;; json port
(defun entropy/sdcv--extract-json-response (json-response)
  "Extracting sdcv json response object string for filterable
with lisp processing. And return the response final feedback
string used for tooltip or adjacent buffer shown for. See also
core func `entropy/sdcv--parse-response-json'.  
"
  (let* ((json-list-ob (entropy/sdcv--parse-response-json json-response))
         rtn)
    (if (eq json-list-ob t)
        (setq rtn t)
      (setq word (car json-list-ob))
      (setq def (nth 1 json-list-ob))
      (setq def-overflow (nth 2 json-list-ob))               
      (cond ((and word def)
             (when (and def-overflow
                        (or entropy/sdcv--show-response-in-adjacently
                            (not (eq entropy/sdcv-tooltip-type 'popup))))
               (setq def (entropy/cl-truncate-string-with-length
                          def
                          entropy/sdcv--response-column-width-max
                          def-overflow)))
             (let* ((word-count (string-width word))
                    (dress-line (entropy/cl-concat-char "-" (+ 3 word-count))))
               (setq rtn (concat dress-line "\n"
                                 "⏺ " word "\n"
                                 dress-line
                                 "\n" def))))
            (t (setq rtn entropy/sdcv--response-null-prompt))))
    rtn))

(defun entropy/sdcv--parse-response-json (json-response)
  "Core json object parsing func for
`entropy/sdcv--extract-json-response', which using `mapcar' for
mapping for the json lisp corresponding vector parsed by
`json-read-from-string' with callbacks func
`entropy/sdcv--parse-json-info' and returned it's alist, or t if
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
         (jsob-alist (mapcar 'entropy/sdcv--parse-json-info
                             jsob))
         rtn)
    (if (> (length jsob-alist) 1)
        (setq rtn
              (or
               (yes-or-no-p
                (format (propertize
                         "Can not exactly match for world '%s'? \nsearch web[yes] or see similar canis[no]: "
                                    'face 'error)
                        (propertize (concat " " entropy/sdcv--query-log " ")
                                    'face
                                    'entropy/sdcv-box-face)))
               (assoc (completing-read "choose similar word: "
                                       jsob-alist
                                       nil t)
                      jsob-alist)))
      (setq rtn (car jsob-alist)))
    rtn))

(defun entropy/sdcv--parse-json-info (json-object-el)
  "Parsing sdcv json lisp object with definition str square model
analyzing based on max width specified by
`entropy/sdcv--response-column-width-max'.

Return value as list as sexp (list word def def-width-overflow-lines)."
  (let* ((word (cdr (assoc 'word json-object-el)))
         (def (cdr (assoc 'definition json-object-el)))
         (def-width (entropy/cl-get-string-max-width def entropy/sdcv--response-column-width-max))
         (def-width-overflow-lines (plist-get def-width :match-overflow-lines) )
         rtn)
    (setq rtn (list word def def-width-overflow-lines))
    rtn))


;;;;;; response adjacent buffer show

(defun entropy/sdcv--show-with-buffer (feedback)
  (let ((buffer (get-buffer-create entropy/sdcv--showed-buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert feedback)
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer buffer)))

;;;;;; response tooltip show
(defun entropy/sdcv--automatic-faceSet ()
  (let ((Lbg_color (entropy/sdcv-tooltip-bgLight-color))
        (Bbg_color (entropy/sdcv-tooltip-bgDark-color))
        (rtn (list :bg nil :fg nil)))
    (cl-case (ignore-errors (entropy/cl-frameBG-dark-or-light))
      (dark (setq rtn (plist-put rtn :bg Lbg_color))
            (setq rtn (plist-put rtn :fg "goldenrod")))
      (light (setq rtn (plist-put rtn :bg Bbg_color))
             (setq rtn (plist-put rtn :fg "firebrick")))
      (nil (setq rtn (plist-put rtn :bg "black"))
           (setq rtn (plist-put rtn :fg "brightyellow"))))
    (set-face-attribute
     'entropy/sdcv-tooltip-face
     nil
     :foreground (plist-get rtn :fg)
     :background (plist-get rtn :bg))
    rtn))

(defun entropy/sdcv--show-with-tooltip (feedback)
  (if (display-graphic-p)
      (cond
       ((eq entropy/sdcv-tooltip-type 'posframe)
        (entropy/sdcv--show-with-posframe feedback))
       ((eq entropy/sdcv-tooltip-type 'pos-tip)
        (entropy/sdcv--show-with-postip feedback))
       ((eq entropy/sdcv-tooltip-type 'popup)
        (entropy/sdcv--show-with-popup feedback)))
    (setq entropy/sdcv-tooltip-type 'popup)
    (entropy/sdcv--show-with-popup feedback)
    (message "Reset tooltip type to 'popup' due to cli UI.")))

;;;;;;; posframe
(defun entropy/sdcv--show-with-posframe (feedback)
  (let ((tooltip_Ctype (entropy/sdcv--automatic-faceSet)))
    (posframe-show entropy/sdcv--tooltip-buffer
                   :string feedback
                   :position (point)
                   :background-color (plist-get tooltip_Ctype :bg)
                   :foreground-color (plist-get tooltip_Ctype :fg)
                   :internal-border-width entropy/sdcv-tooltip-border-width)
    (setq entropy/sdcv--tooltip-last-point (point)
          entropy/sdcv--tooltip-last-scroll-offset (window-start))
    (add-hook 'post-command-hook 'entropy/sdcv--posframe-hide-after-move)))

(defun entropy/sdcv--posframe-hide-after-move ()
  "Quit and delete `entropy/sdcv--tooltip-buffer' of posframe
show-type whatever keys touching with. 

This func was automatically added into `post-command-hook' by
`entropy/sdcv--show-with-tooltip'."
  (ignore-errors
    (when (get-buffer entropy/sdcv--tooltip-buffer)
      (unless (and
               (equal (point) entropy/sdcv--tooltip-last-point)
               (not (eq this-command 'keyboard-quit))
               (equal (window-start) entropy/sdcv--tooltip-last-scroll-offset))
        (posframe-delete entropy/sdcv--tooltip-buffer)
        (kill-buffer entropy/sdcv--tooltip-buffer)))))

;;;;;;; pos-tip
(defun entropy/sdcv--pos-tip-show (string &optional face)
  (let* ((w-h (pos-tip-string-width-height string))
         (window (selected-window))
         (frame (window-frame window)))
    (pos-tip-show-no-propertize
     string
     face
     (point) nil -1
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (ceiling (* (pos-tip-tooltip-height
                  (cdr w-h)
                  (frame-char-height frame) frame)
                 entropy/sdcv-pos-tip-height-stretch)))))

(defun entropy/sdcv--show-with-postip (feedback)
  (let ((tooltip_Ctype (entropy/sdcv--automatic-faceSet))
        (pos-tip-internal-border-width 12))
    (entropy/sdcv--automatic-faceSet)
    (setq feedback
          (propertize feedback 'face 'entropy/sdcv-tooltip-face))
    (entropy/sdcv--pos-tip-show feedback 'entropy/sdcv-tooltip-face)))

;;;;;;; popup
(defun entropy/sdcv--show-with-popup (feedback)
  (let ((theme_Ctype (entropy/sdcv--automatic-faceSet))
        ($pface_temp (copy-tree
                      (cons (face-attribute 'popup-tip-face :foreground)
                            (face-attribute 'popup-tip-face :background))))
        (popup-tip-max-width entropy/sdcv--response-column-width-max))
    (set-face-attribute 'popup-tip-face nil
                        :foreground (plist-get theme_Ctype :fg)
                        :background (plist-get theme_Ctype :bg))
    (popup-tip feedback :point (point) :margin 1 :truncate t)
    (set-face-attribute 'popup-tip-face nil
                        :foreground (car $pface_temp)
                        :background (cdr $pface_temp))))


;;;;; query rebuit

(defun entropy/sdcv--query-rebuit (str)
  "Rebuilt the query string for be suits as the sdcv receive type
which can be reducing unmatching probabilities.

Note: now this func was under-development and just simply
downcase the query string."
  (let (rtn)
    (setq rtn (downcase str))
    (setq entropy/sdcv--query-log rtn)
    rtn))


;;;; main
;;;###autoload
(defun entropy/sdcv-search-at-point-tooltip ()
  "Mainly interactive func for search point or marked region
string with sdcv cli."
  (interactive)
  (let ((str (entropy/sdcv--get-word-or-region))
        (dict (entropy/sdcv--choose-dict)))
    (unless (stringp str)
      (error "Could not find word or region at point."))
    (setq str (entropy/sdcv--query-rebuit str))
    (entropy/sdcv--command-router str dict 'tooltip)))


;;;###autoload
(defun entropy/sdcv-search-input-adjacent ()
  "Mainly interactive func for search with inputted querying
string with sdcv cli."
  (interactive)
  (let* ((dict (entropy/sdcv--choose-dict))
         (str (let (promt empty-error (rtn "") (prompt-initial (entropy/sdcv--get-word-or-region)))
                (cond (prompt-initial
                       (setq prompt (format "Input word (default: %s): " prompt-initial)))
                      (t (setq prompt "Input word: ")))
                (setq rtn (read-string prompt))
                (cond ((and (equal "" rtn)
                            prompt-initial)
                       (setq rtn prompt-initial))
                      ((and (equal "" rtn)
                            (not prompt-initial))
                       (error "Input empty!")))
                rtn)))
    (setq str (entropy/sdcv--query-rebuit str))
    (entropy/sdcv--command-router str dict 'adjacent)))

(advice-add 'entropy/sdcv-search-at-point-tooltip :around #'entropy/sdcv--lang-set-process)
(advice-add 'entropy/sdcv-search-input-adjacent :around #'entropy/sdcv--lang-set-process)

;;; provide
(provide 'entropy-sdcv)
