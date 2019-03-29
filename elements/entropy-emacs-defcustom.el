;; init-custom.el --- Initialize custom configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Change by: Entropy
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Custom configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the custom-file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * Code:
;; ** basic
;; *** fundamental config
(defgroup entropy/emacs-custom-variable-basic nil
  "Personal Emacs configurations."
  :group 'extensions)

(defcustom entropy/iniext-extensions-dir
  (expand-file-name
   ".entropy-emacs-extension"
   "~/")
  "entropy-emacs extensions directory path."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/iniext-extensions-elpa-dir
  (expand-file-name
   ".entropy-emacs-extension-elpa"
   "~/")
  "entropy-emacs elpa/melpa extensions directory path."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/iniext-deps-dir
  (expand-file-name
   ".entropy-emacs-deps"
   "~/")
  "entropy-emacs extra feature directory path."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/custom-common-file (expand-file-name
                                       "custom.el"
                                       user-emacs-directory)
  "entropy-emacs common custom file"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/custom-navigate-file (expand-file-name
                                         "navigate.el"
                                         user-emacs-directory)
  "entropy-emacs navigate custom file."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/fancy-splash-logo (expand-file-name "logo/logo.png" (file-name-directory load-file-name))
  "Set emacs logo. nil means official logo."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/user-full-name nil
  "Set user full name."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/user-mail-address nil
  "Set user email address."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/package-archive-repo 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna))
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/default-external-browser 'firefox
  "Set default browser for `org-open-at-point'"
  :type '(choice
          (const :tag "google-chrome-stable %s" chrome)
          (const :tag "firefox %s" firefox))
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/initialize-benchmark-enabled nil
  "Enable the init benchmark or not."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/use-emms-mode-line nil
  "Enable emms-modeline or not."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** use emacs extension from git submodules or elpa folder
(defcustom entropy/use-extensions-type 'origin
  "Init emacs with extensions from entropy-emacs submodules or
  elpa place.

Available value are 'submodules' and 'origin'."
  :type 'symbol
  :group 'entropy/emacs-custom-variable-basic)
;; *** yas dir
(defcustom entropy/yas-dir "~/.emacs.d/snippets"
  "Set the default personal snippet dir"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)
;; *** ui theme and modeline style
;; **** enable initial dashboard
(defcustom entropy/enable-initial-dashboard t
  "Enable entropy emacs initial dashboard instead of emacs
  default one."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** theme choise
(defcustom entropy/theme-options 'doom-one
  "Choice emacs gui theme"
  :type 'face
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/enable-option-theme-tty nil
  "Enable option theme in tty, it's useful for true color tty."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** modeline
(defcustom entropy/modeline-style "origin"
  "
Choose the modeline style:

You can choose below four choices:
- spaceline-regular:                                  spaceline
- spaceline-all-the-icons:                            spaceline-icons
- powerline-default:                                  powerline
- origin with none modified until you do it yourself: origin
- doom-modeline                                       doom


Notice:

- spaceline-all-the-icons config of entropy-emacs was in development, so it may
  not be full visibility fine at this time.

- this variable's effectively was rely on the varaible
  `entropy/enable-modeline-toggle' enabled, other wise any setting for this
  variable were none-effectively."
  :type '(choice
          (const :tag "spaceline-regular" "spaceline")
          (const :tag "spaceline-all-the-icons" "spaceline-icons")
          (const :tag "powerline-default" "powerline")
          (const :tag "Origin" "origin")
          (const :tag "doom-modeline" "doom"))
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/enable-modeline-toggle nil
  "Enable modeline toggle function `entropy/mdl-powerline' and
  `entropy/mdl-spaceline' and the customized effectively of
  `entropy/modeline-style'.

Note: spaceline and powerline will cause lagging performancs
issue for emacs while you are in the low performance computer
that you computer is with the older hardware or be out of repair
for a long time and so as the bad head dispersion."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** display time in modeline
(defcustom entropy/display-time-modeline nil
  "Whether show the Real-time TIME in mode line."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** init-frame position
(defcustom entropy/init-fpos-enable nil
  "Whether set init emacs position by `entropy/set-frame-position'."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/init-frame-width-scale 0.6
  "The init scale of frame width within the fully width of
  screen"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/init-frame-height-scale 0.93
  "The init scale of frame height within the fully height of
  screen"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/init-fpos-y nil
  "The y coordinate position value.

  In commonly this value was nil, because most of windows or linux
  default ui setting are that the dock is at the bottom of current
  screen.

  You can setting it to the proper value to be compatible with the
  height of dock:

  #+BEGIN_EXAMPLE

          +--------+-----------------------------+
          |        | ----> y                     | ----->dock height
          +---+----+----------------------+------+
          |   |    | \\-                   |      |
          |   |    |   \\-                 |      |
          |   v    |     \\-               |      |
          |   x    |       emacs position |  ----+------->  emacs height
          |        |                      |      |        
          |        |                      |      |     
          |        |                      |      |
          |        |                      |      |
          +--------+----------------------+------+
  #+END_EXAMPLE"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)
;; *** init display line mode
(defcustom entropy/init-display-line-mode nil
  "Enable `global-display-line-numbers-mode' at start up time when in
emacs 26 or higher emacs version."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** desktop save config
(defcustom entropy/desktop-enable nil
  "Enable desktop-save-mode and persistent scratch buffer"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** recent files
(defcustom entropy/use-recentf nil
  "Whether use recentf-mode after emacs init."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** tab-width or disable it globally
(defcustom entropy/custom-tab-enable nil
  "Enable indent-tab-mode"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/custom-tab-width 4
  "Set the customized tab width"
  :type 'integer
  :group 'entropy/emacs-custom-variable-basic)

;; *** init fill column width
(defcustom entropy/fill-paragraph-width 70
  "Setting fill-paragraph width, default 100."
  :type 'integer
  :group 'entropy/emacs-custom-variable-basic)
;; *** backgroud transparent
(defcustom entropy/init-loop-alpha t
  "Enable transparent at startup of emacs"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
(defcustom entropy/loop-alpha-value '((95 55) (100 100))
  "The value of background transparent"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** dired about
;; **** dired visual type
(defcustom entropy/dired-visual-type "simple-rainbow"
  "Type of dired visual appearance.

  You have two choice:
  - \"simple-rainbow\" : using `dired-rainbow'.
  - \"all-the-icons\" : using `all-the-icons-dired'

  If you use emacs-25.3.1 , you just can using the simple way
  because that emacs 25.3 have the bug of can not show all-the-icons
  fully type."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; **** dired trash enable
(defcustom entropy/dired-enable-trash nil
  "Enable trash function when using `dired-delete-file' in
  `dired-mode'."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** eyebrowse config
(defcustom entropy/enable-eyebrowse-new-workspace-init-function nil
  "Enable personal function for eyebrowse to creating new
  workspace"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/eyebrowse-new-workspace-init-function 'entropy/create-scratch-buffer
  "Create the init buffer or others with you own function when
open one new eyebrowse workspace"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; *** ibuffer projectiitle config
(defcustom entropy/enable-ibuffer-projectitle nil
  "Enable ibuffer-projectitle in ibuffer

Note: ibuffer-projectitle will cause the performance debug.
"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; *** google translation
(defcustom entropy/google-translate-toggle-patched-in-china t
  "Enable google-translate in china"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; *** eww search engine set
(defcustom entropy/eww-search-engine-customize t
  "Enable eww search prefix customized"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/eww-search-engine "https://www.bing.com/search?q="
  "Customized eww search prefix string and the default was 'bing.cn'"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)
;; *** search-web search-engines setting
(defcustom entropy/search-web-engines-internal
  '(("php-manual" "http://php.net/manual-lookup.php?pattern=%s" In-Emacs)
    ("MOZILA MDN" "http://developer.mozilla.org/en-US/search?q=%s" In-Emacs)
    ("baidu" "http://www.baidu.com/s?wd=%s" In-Emacs)
    ("google" "http://www.google.com/search?q=%s" In-Emacs)
    ("google ja" "http://www.google.com/search?hl=ja&q=%s" In-Emacs)
    ("google en" "http://www.google.com/search?hl=en&q=%s" In-Emacs)
    ("stackoveflow en" "http://stackoverflow.com/search?q=%s" In-Emacs)
    ("stackoveflow ja" "http://ja.stackoverflow.com/search?q=%s" In-Emacs)
    ("elpa" "https://elpa.gnu.org/packages/%s.html" In-Emacs)
    ("melpa" "https://melpa.org/#/?q=%s" In-Emacs))
  "Internal search-web engines."
  :type 'list
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/search-web-engines-external
  '(("MOZILA MDN" "http://developer.mozilla.org/en-US/search?q=%s" External)
    ("php-manual" "http://php.net/manual-lookup.php?pattern=%s" External)
    ("github-external" "http://github.com/search?&q=%s" External)
    ("baidu" "http://www.baidu.com/s?wd=%s" External)
    ("google" "http://www.google.com/search?q=%s" External)
    ("google ja" "http://www.google.com/search?hl=ja&q=%s" External)
    ("google en" "http://www.google.com/search?hl=en&q=%s" External)
    ("google maps" "http://maps.google.co.jp/maps?hl=ja&q=%s" External)
    ("google scholar" "https://scholar.google.co.jp/scholar?q=%s" External)
    ("elpa" "https://elpa.gnu.org/packages/%s.html" External)
    ("melpa" "https://melpa.org/#/?q=%s" External)
    ("youtube" "http://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
    ("twitter" "http://twitter.com/search?q=%s" External)
    ("wikipedia en" "http://www.wikipedia.org/search-redirect.php?search=%s&language=en" External)
    ("wikipedia ja" "http://www.wikipedia.org/search-redirect.php?search=%s&language=ja" External)
    ("stackoveflow en" "http://stackoverflow.com/search?q=%s" External)
    ("stackoveflow ja" "http://ja.stackoverflow.com/search?q=%s" External))
  "External search-web engines"
  :type 'list
  :group 'entropy/emacs-custom-variable-basic)

;; *** org config
;; **** org-bullet
(defcustom entropy/enable-org-bullets t
  "Enable org bullets"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/org-bullets-type "roman"
  "Choose org bullets type."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; **** windows org-download temp path
(defcustom entropy/win-org-download-file-name "c:\\temp\\screenshot.png"
  "Setting file name with it's path for screenshot in windows"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/win-org-download-screenshot-method "SnippingTool.exe"
  "Setting screenshot tool in windows and default be SnippingTool
  which is the componets of windows inside."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; *** Font setting
(defcustom entropy/font-setting-enable t
  "
Enable entropy-emacs face font setting

With font below:

      \"Noto Sans Mono\" or \"Noto Mono\" (for emacs 25.3 only)
      \"Source Code Pro\"
      \"Droid Sans\"
      \"Symbola\"
      \"Noto Serif CJK KR\"

1. Google noto mono for the default face-attribute.
2. Source Code Pro for default face also.
3. symbola for the main unicode character show.
4. Droid for display CJ font.
5. KR for such it means.
"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/font-face-default "Noto Mono"
  "Choose the default font for face:
There are two font for indicated:

1. Noto Mono
2. Source Code pro
3. Liberation Mono

Defualt for \"Noto Mono\""
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/font-size-default 10
  "Set the default font size for face-attribute.

Default size was 10, the upper limit was 15."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/font-chinese "Droid Sans"
  "Choose the chinese font setting, default was Droid Sans if
your installed"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; *** project search exec
(defcustom entropy/search-program "ag"
  "Search engine choices:
1. ag
2. pt

Default is ag"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; *** personal browse-url function and varaiable
(defcustom entropy/enable-personal-browse-url-function nil
  "Whether enable personal browse-url function? If yes, set it to
  be 't'"
  :type 'boolean
  :group ' entropy/emacs-custom-variable-basic)


(defcustom entropy/browse-url-function nil
  "The specific browse-url-function by your self,

Mostly of all, you should write your browse-url function with two args:
- 'url'
- '&rest args'

For example:

     (defun entropy/open-with-url (url &rest args)
       (interactive (browse-url-interactive-arg \"URL: \"))
       (w32-shell-execute
        \"open\"
        \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\"
        url))
"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)
;; *** For minmal startup
(defcustom entropy/minimal-start nil
  "For minmal startup"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; *** language envrionment setting
(defcustom entropy/custom-language-environment-enable nil
  "Enable custome language environment"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/language-environment nil
  "Setting emacs language environment"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; *** use pyim
(defcustom entropy/enable-pyim nil
  "Enable pyim be the default pyin input method"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/pyim-dicts
  '((:name "core"
           (expand-file-name "pyim-about/core.pyim" user-emacs-directory))
    (:name "extra"
           (expand-file-name "pyim-about/dict/extra.pyim" user-emacs-directory)))
  "
Setting pyim-dicts file, if nil then use basic dicts for
minimal usage

You can setting like this:

  (setq entropy/pyim-dicts
        '((:name \"popular\":file \"~/.emacs.d/pydict/sougou.pyim\")
          (:name \"core\" :file \"~/.emacs.d/pydict/py++.pyim\")
          (:name \"pyim-greatdict\" :file \"~/.emacs.d/pydict/pyim-greatdict.pyim\")))
"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/pyim-cached-dir nil
  "Set pyim cached dir, if nil use defaults setting (see
  `pyim-dcache-directory')"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/pyim-tooltip nil
  "Setting the pyim toolitip method"
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; *** elfeed config
(defcustom entropy/elfeed-proxyfeeds-regexp-list '()
  "Regexp for matching the feeds which needed for updating through proxy."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; *** gnus
(defcustom entropy/gnus-init-config
  '(:gnus-home "~/.gnus/"
    :gnus-news-dir "~/.gnus/News/"               
    :mail-dir "~/.gnus/Mail/"
    :mail-temp-dir "~/.gnus/temp/"
    :init-file "~/.gnus/gnus-config.el"
    :startup-file "~/.gnus/newsrc"
    :read-newsrc nil
    :save-newsrc nil
    :use-dribble t
    :read-active-file t)
  "Initial setting for gnus for entropy-emacs."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; *** entropy-emacs start with
(defcustom entropy/emacs-startwith-apps nil
  "The external apps for entropy-emacs start with.

This variable forms as one alist of each element's car was the
process-name and the cdr was the executable full path string for
just it's name."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)
;; ** company config
(defcustom entropy/company-lsp nil
  "Enable lsp for company-mode"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
(defcustom entropy/company-posframe-mode nil
  "Enable company-posframe.

  Note:

  company-posframe can not woring well with
  `company-quickhelp-mode', and when set this variable to 't'
  will automatically avoiding startup of
  `company-quickhelp-mode'."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)
;; ** use highlight feature
(defgroup entropy/emacs-highlight nil
  "Personal Emacs configurations."
  :group 'extensions)
;; *** main enable
(defcustom entropy/use-highlight-features t
  "Enable highlight feature package `init-highlight.el'."
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** highlight-indention
(defcustom entropy/hl-highlight-indention-enable-at-startup nil
  "Enable indention highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; *** symbol-over-lay
(defcustom entropy/hl-sysmbol-overlay-enable-at-startup nil
  "Enable symbol-overlay highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** highlight-parentheses
(defcustom entropy/hl-highlight-parentheses-mode-enable-at-startup nil
  "Enable highlight-parentheses highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** rainbow delimiters
(defcustom entropy/hl-rainbow-delimiters-enable-at-startup nil
  "Enable rainbow-delimiters highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** hl-todo-mode
(defcustom entropy/hl-todo-enable-at-startup nil
  "Enable hl-todo highlight feature which can show color of
  `TODO' keywords universal not only the org file"
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** diff-hl
(defcustom entropy/hl-diff-hl-enable-at-startup nil
  "Enable diff-hl highlight feature which can show the git diff
in current buffer"
  :type 'boolean
  :group 'entropy/emacs-highlight)
;; *** whitespace
(defcustom entropy/hl-whitespace-enable-at-startup nil
  "Enable whitespace highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

(defcustom entropy/hl-whitespace-auto-cleanup nil
  "Auto clean up messy or redundant whitespace when in `whitespace-mode'

Notice: If enable this will cause git diff log be messy with long
difference.
"
  :type 'boolean
  :group 'entropy/emacs-highlight)


;; ** eshell
(defgroup entropy/eshell-group nil
  "Custom variable group for eshell in entropy-emacs. "
  :group 'extensions)

(defcustom entropy/eshell-alias-file "~/.eshell-alias"
  "entropy-emacs eshell alias file location."
  :type 'string
  :group 'entropy/eshell-group)

(defcustom entropy/eshell-history-file "~/.eshell-history"
  "entropy-emacs eshell history file location."
  :type 'string
  :group 'entropy/eshell-group)


;; ** specific for windows
(defgroup entropy/emacs-win nil
  "Personal Emacs configurations for windwos."
  :group 'extensions)
;; *** w32 ime config
(defcustom entropy/win-init-ime-enable nil
  "Enable win32 ime at startup (offset to the bug of w32-ime)."
  :type 'boolean
  :group 'entropy/emacs-win)

;; *** emacs lang set for windows
(defcustom entropy/win-env-lang-enable nil
  "Whether enable emacs lang for windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-env-lang-set "zh_CN.UTF-8"
  "Setting emacs lang in windows operation system"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable wsl in windows
(defcustom entropy/wsl-enable nil
  "Set whether you want to use wsl?"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/wsl-apps "c:/git-portable/usr/bin/"
  "Set the baisic wsl apps path for basic shell-command using
which also used in shell-buffer.

Note: now it's just suitable for git-for-windows (no sdk-extra utilitie included)"
  :type 'string
  :group 'entropy/emacs-win)


(defcustom entropy/wsl-enable-extra nil
  "Enable extra wsl apps.

  This ON-OFF variable are setted for follow occurrence:

      If you setting `entropy/wsl-apps' to 'git-for-windows-minimal'
      which just contained the basic UNIX-LIKE commands that doesn't
      contianed commands like 'man' and 'tree' or sth else, you want
      to using them as well in current emacs session.

  Notice:

  It suggested you to using Msys2's '/usr/bin' to be the value of
  this variable, that you could set it as:
            
      \"c:/Msys2/usr/bin\".
  "
  :type 'boolean
  :group 'entropy/emacs-win)


(defcustom entropy/wsl-apps-extra ""
  "Set the extra wsl apps path, used for woman or other
subprocess of emacs that sth called lying on `exec-path'.

And this must using the type for the root of wsl-tool path, like
if you using msys2 , you must set this variable to like:

\"c:/msys2/\"
"
  :type 'string
  :group 'entropy/emacs-win)
;; **** windows git portable setting
(defcustom entropy/git-portable nil
  "Whether enable portable git application when you want to use
  git portable "
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/git-portable-path "c:/git-portable/cmd/"
  "When you enable git-custom set the customize git applicaton's
path default path was my git path please must modify it for yourself"
  :type 'string
  :group 'entropy/emacs-win)

;; *** Adding path for emacs built 'bin' folder
(defcustom entropy/win-emacs-bin-path-add t
  "Whether adding emacs bin folder to path on windows platform.

  This ON-OFF varaible setted for adding emacs bin folder to both of
  `exec-path' and \"PATH\" system variable.

  It's useful that your can call emacs or other bult-in binary as
  'convert' intern.
  "
  :type 'boolean
  :group 'entropy/emacs-win)
;; *** wsl terminal setting
(defcustom entropy/wsl-terminal-enable nil
  "Whether enable the wsl-bash,wsl path config worked when you set this `t'"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/wsl-terminal "c:/git-portable/git-bash.exe"
  "Set the default wsl bash applictions,it suggested to set same
as the wsl-apps main controled applications which I suggested use
git-for-windows-sdk `git-bash.exe'"
  :type 'string
  :group 'entropy/emacs-win)

;; *** gcc and mingw64 setting
(defcustom entropy/win-portable-mingw-enable nil
  "Enable portable mingw64 in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-mingw-path "c:/mingw64/bin/"
  "Setting the path of portable mingw64 for windows plattform
  when `entropy/win-portable-mingw-enable' was set to 't'"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-gcc-parameter ""
  "Setting the gcc compile parameter"
  :type 'string
  :group 'entropy/emacs-win)
(defcustom entropy/win-g++-parameter ""
  "Setting the g++ compile parameter"
  :type 'string
  :group 'entropy/emacs-win)

;; *** clang setting
(defcustom entropy/win-portable-clang-enable nil
  "Enable clang for windows plattform"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/win-portable-clang-path ""
  "Path for portable clang for windows plattform."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; *** windows texlive setting
(defcustom entropy/win-portable-texlive-enable nil
  "Whether to enable texlive in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-texlive-path "c:/texlive/bin"
  "Set up texlive path in windows"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable grep in windows
(defcustom entropy/win-portable-grep-enable nil
  "Enable windows grep program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-grep-path "c:/grep/bin"
  "Set windows grep exec path"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable ag in windows
(defcustom entropy/win-portable-ag-enable nil
  "Enable windows ag program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-ag-path "~/.emacs.d/ag/bin/"
  "Set windows ag (The Silver Searcher) path"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable rg in windows
(defcustom entropy/win-portable-rg-enable nil
  "Enable windows ripgrep program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-rg-path "~/.emacs.d/rg/bin/"
  "Set windows ag path"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable pt in windows
(defcustom entropy/win-portable-pt-enable nil
  "Enable windows pt program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-pt-path "~/.emacs.d/pt/bin/"
  "Set windows pt path"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable cmder in windows
(defcustom entropy/Cmder-enable nil
  "Enable windows Cmder program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/Cmder-path "c:/cmder/bin/Cmder.exe"
  "Set windows Cmder path"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable php in windows
(defcustom entropy/win-portable-php-enable nil
  "Enable php portable in windows"
  :type 'boolean
  :group 'entropy/emacs-win)
(defcustom entropy/win-portable-php-path "c:/php/"
  "Setting the path of portable php executable"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enbale pip in windows
(defcustom entropy/win-portable-pip-enable nil
  "Enable portable pip in windows"
  :type 'boolean
  :group 'entropy/emacs-win)
(defcustom entropy/win-portable-pip-path "c:/python/bin/"
  "Set protable pip path in windows"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable python in windows
(defcustom entropy/win-portable-python-enable nil
  "Enable portable python in windows"
  :type 'boolean
  :group 'entropy/emacs-win)
(defcustom entropy/win-portable-python-path "c:/python/bin/"
  "Set protable python path in windows"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable nodejs in windows
(defcustom entropy/win-portable-nodejs-enable nil
  "Enable nodejs portale in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-nodejs-path "c:/nodejs/"
  "Setting the path of protable nodejs in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable opencc in windows
(defcustom entropy/win-portable-opencc-enable nil
  "Enable opencc portale in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-opencc-path "c:/opencc/"
  "Setting the path of protable opencc in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; *** enable pandoc in windows
(defcustom entropy/win-portable-pandoc-enable nil
  "Enable portable pandoc in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-pandoc-path nil
  "Setting portble pandoc path for windows"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enbale portble jdk
(defcustom entropy/win-portable-jdk-enable nil
  "Enable using portble JDK"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-jdk-path "c:/JDK64/bin"
  "Setting portble JDK pth"
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable zeal
(defcustom entropy/win-portable-zeal-enable 'nil
  "Enable zeal doc http://zealdocs.org/."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-zeal-path ""
  "Setting zeal path on windows plattform."
  :type 'string
  :group 'entropy/emacs-win)
;; *** enable putty in windows
(defcustom entropy/win-portable-putty-enable nil
  "Enable putty portable on windows."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/win-portable-putty-path ""
  "Windows portable putty path."
  :type 'string
  :group 'entropy/emacs-win)


;; ** For Emacs devel
;; e.g. release is 24.5 or 25.1, while devel build is 26.0.90
(when (= emacs-minor-version 0)
  (setq package-user-dir (locate-user-emacs-file "elpa-devel"))
  (setq desktop-base-file-name ".emacs-devel.desktop")
  (setq desktop-base-lock-name ".emacs-devel.desktop.lock"))




;; * provide
(provide 'entropy-emacs-defcustom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
