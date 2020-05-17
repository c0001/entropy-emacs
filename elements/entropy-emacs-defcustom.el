;;; entropy-emacs-defcustom.el --- entropy emacs collection of customizable variables
;;
;; * Copyright (C) 20190602  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-defcustom.el
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
;; This file was the =entropy-emacs= initializing top fundamental,
;; giving the customized variables collection, top APIs and doing
;; initalizing at background,even for providing to other part of
;; `entropy-emacs' to get the user specific variable setting, using
;; emacs's `defcustom' function to define and give the initial
;; value. All the variable has the self-doc for quickl viewing the
;; usage.
;;
;; Like other `entropy-emacs' components, outline context orgnization
;; used in this file as well, reading file content follow the context
;; level division for be benefits.
;;
;; * Configuration:
;;
;; This file was the part of `entropy-emacs', individually using was
;; not in the context of designation.
;;
;;
;; * Code:
;; ** Require
(if (version< emacs-version "27")
    (require 'cl)
  (require 'cl-macs))

;; ** Customizable Variables
;; *** Basic
(defgroup entropy/emacs-custom-variable-basic nil
  "Personal Emacs configurations."
  :group 'extensions)

;; **** fundamental config
(defgroup entropy/emacs-customize-fundametal nil
  "Fundametal customized variable group for entropy-emacs"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-stuffs-topdir
  (expand-file-name "stuffs" entropy/emacs-user-emacs-directory)
  "The stuffs collection host path, for as `savehist-file',
`bookmark-file' cache host. This setting mainly for cleanup
`entropy/emacs-user-emacs-directory'."
  :type 'string
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-minimal-start nil
  "For minmal startup"
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-top-prefix-key-cons '("C-`" .  "C-@")
  "The cons stores the prefix key for `entropy/emacs-top-keymap',
the car for GUI session, and cdr for TUI thus as well.

The intention to get different prefix key for GUI anD TUI is for
that the terminal emulation used for emacs may not have the full
key-stroke experience."
  :type 'cons
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-garbage-collection-delay 5
  "The defaults garbage collection idle delay secons setting
for entropy-emacs."
  :type 'integer
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-garbage-collection-message-p nil
  "whether echo garbage collection message."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-custom-enable-lazy-load t
  "Enable lazy load for entropy-emacs when non-nil.

Notice: when `entropy/emacs-fall-love-with-pdumper' is non-nil or
in daemon session, this variable will be pressed whatever init
value assignments into."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-custom-common-file
  (expand-file-name
   "custom.el"
   entropy/emacs-user-emacs-directory)
  "The value for `custom-file' but specified for =entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-user-full-name nil
  "The value for `user-full-name' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-user-mail-address nil
  "The value for `user-mail-address' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-package-archive-repo 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna))
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-use-popup-window-framework 'shackle
  "Using popup window enhancement framework of `popwin-mode' with
value 'popwin' or `shackle-mode' with value 'shackle'.

Since `popwin' is not under maintaining anymore, defualtly using
`shacle-mode' and it's the suggested."
  :type 'symbol
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-init-display-line-numbers-mode nil
  "Enable `global-display-line-numbers-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-init-hl-line-mode t
  "Enable `global-hl-line-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-fill-paragraph-width 70
  "Setting fill-paragraph width, default 70 to follow the emacs
convention."
  :type 'integer
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-use-icon t
  "Whether to use icon visualization when available."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-use-recentf t
  "Whether use recentf-mode after emacs init."
  :type 'boolean
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-dictionary-backend 'sdcv
  "The dictionary tool type for chosen, valid type are

- 'sdcv'
- 'bing'
- 'youdao'
- 'google'
"
  :type 'symbol
  :group 'entropy/emacs-customize-fundametal)

(defcustom entropy/emacs-google-translate-toggle-patched-in-china t
  "Enable google-translate in china. (GFW banned restriction break)"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-indicate-sshd-session nil
  "Indicate whether current emacs session is on ssh remote
  session."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-use-emacs-in-terminal-with-graphic-features nil
  "Using some eemacs features on graphic session in text-based
  session."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-align-window-center-with? 'olivetti
  "The window central visual toggler type, valid of 'olivetti' or 'basic'.

=olivetti= was a riched window center align package which provide
shrink and expand auto-key, and without alignment be killed
problem, =basic= type is simple but without fully featured.
"
  :type 'symbol
  :group 'entropy/emacs-custom-variable-basic)

;; **** tree
(defgroup entropy/emacs-tree-visual-group nil
  "Tree side bar show group"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-tree-visual-type 'treemacs
  "'neotree' or 'treemacs' tree slide show type."
  :type 'symbol
  :group 'entropy/emacs-tree-visual-group)

(defcustom entropy/emacs-tree-neotree-text-scale -2
  "The text-scale for neotree buffer."
  :type 'integer
  :group 'entropy/emacs-tree-visual-group)

;; **** emacs extension use options
(defgroup entropy/emacs-extensions-customize nil
  "Customized extensions variable group configured for entropy-emacs."
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-ext-elpkg-get-type 'origin
  "Init emacs with elisp extensions from entropy-emacs-extensions or
elpa and melpa.

Available value are 'submodules' 'submodules-melpa-local' and
'origin'.

Type of 'submodules' and 'submodules-melpa-local' indicates to use
=entropy-emacs-extensions= (see
`entropy/emacs-ext-eemacs-elpkg-archive-project-dir' for its brief
introduction) to initialize the elisp extensions, which type
'submodules-melpa-local' means to use that as a remote elisp
packges archive host like what 'elpa' and 'melpa' did. If use type
of 'submodules' that emacs will start using elisp packages git
repo directly within =entropy-emacs-extensions=.

For thus all, you can use original emacs package.el to download
all =entropy-emacs= required elisp packages by set the type of
'origin'.
"
  :type 'symbol
  :group 'entropy/emacs-extensions-customize)

(defun entropy/emacs-ext-elpkg-get-type-valid-p ()
  "The data predicate for `entropy/emacs-ext-elpkg-get-type'."
  (unless (member entropy/emacs-ext-elpkg-get-type
                  '(origin submodules submodules-melpa-local))
    (error "Invalid value for `entropy/emacs-ext-elpkg-get-type'")))

(defun entropy/emacs-ext-elpkg-get-by-emacs-pkgel-p ()
  "Judges whether `entropy/emacs-ext-elpkg-get-type' is based on
`package.el'."
  (or (eq entropy/emacs-ext-elpkg-get-type 'origin)
      (eq entropy/emacs-ext-elpkg-get-type 'submodules-melpa-local)))

(defcustom entropy/emacs-ext-eemacs-elpkg-archive-project-dir
  (expand-file-name
   "entropy-emacs-extensions"
   "~/.config/entropy-emacs")
  "entropy-emacs extensions collection archive location. This
collection used to retrieving all entropy-emacs elpa or melpa
extensions' repos as submodules archived as one single project
using for studying or giving the pr to the origin host when you
found the bug or want to give the improvements. You can get it
from 'https://github.com/c0001/entropy-emacs-extensions'.

This archive used then type of 'submodules' to customized
variable `entropy/emacs-ext-elpkg-get-type'."

  :type 'string
  :group 'entropy/emacs-extensions-customize)

(defcustom entropy/emacs-ext-use-eemacs-lsparc nil
  "Whether to use archived lanuguage servers."
  :type 'string
  :group 'entropy/emacs-extensions-customize)

(defcustom entropy/emacs-ext-lsp-archive-dir
  (expand-file-name
   "entropy-emacs-lsp-archive"
   "~/.config/entropy-emacs")
  "entropy emacs language server archives project location, you
may download from
'https://github.com/c0001/entropy-emacs-lsp-archive'."
  :type 'string
  :group 'entropy/emacs-extensions-customize)

(defcustom entropy/emacs-ext-emacs-pkgel-get-pkgs-root
  (expand-file-name
   "entropy-emacs-extensions-elpa"
   "~/.config/entropy-emacs")
  "entropy-emacs elpa extensions directory for hosting the
upstream installed packages of `package.el'."
  :type 'string
  :group 'entropy/emacs-extensions-customize)

(defcustom entropy/emacs-ext-user-specific-load-paths nil
  "Extra load path list for user specification.

This feature usually used for emacs new feature adding test and
designation."
  :type 'sexp
  :group 'entropy/emacs-extensions-customize)

;; **** yasnippet
(defcustom entropy/emacs-yas-dir
  (expand-file-name "snippets" entropy/emacs-stuffs-topdir)
  "Set the default personal snippet dir"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; **** ui theme and modeline style
;; ***** enable initial dashboard
(defcustom entropy/emacs-enable-initial-dashboard 'rich
  "Enable entropy emacs initial dashboard instead of emacs
default one.

Valid value are 't' or 'rich', otherwise disable this
feature.

When value are either 't' or 'rich', a fancy simple splash buffer
`entropy/emacs-init-welcom-buffer-name' will startup firstly, and
then enable the rich dashbord contents when value is 'rich'."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; ***** theme choise
(defcustom entropy/emacs-theme-options 'doom-one
  "Choice for emacs theme"
  :type 'face
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-solaire-themes-regex-list
  '("^doom-"
    "^atom-one-dark"
    "^spacemacs-")
  "Themes name regex matchs for solaire-mode."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; ***** modeline
(defgroup entropy/emacs-customize-modeline nil
  "Mode line customized variable group configured for entropy-emacs"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-modeline-style "origin"
  "
Choose the modeline style:

You can choose below four choices:
- spaceline-regular:                                  spaceline
- powerline-default:                                  powerline
- origin with none modified until you do it yourself: origin
- doom-modeline                                       doom


Notice:

- this variable's effectively was rely on the varaible
  `entropy/emacs-enable-modeline-toggle' enabled, other wise any setting for this
  variable were none-effectively."
  :type '(choice
          (const :tag "spaceline-regular" "spaceline")
          (const :tag "spaceline-all-the-icons" "spaceline-icons")
          (const :tag "powerline-default" "powerline")
          (const :tag "Origin" "origin")
          (const :tag "doom-modeline" "doom"))
  :group 'entropy/emacs-customize-modeline)

(defcustom entropy/emacs-enable-modeline-toggle t
  "Enable modeline toggle function `entropy/emacs-mdl-powerline'
and `entropy/emacs-mdl-spaceline' and the customized effectively
of `entropy/emacs-modeline-style'.

Note: spaceline and powerline will cause lagging performancs
issue for emacs while you are in the low performance computer
that you computer is with the older hardware or be out of repair
for a long time and so as the bad head dispersion."
  :type 'boolean
  :group 'entropy/emacs-customize-modeline)

;; ***** display time in modeline
(defcustom entropy/emacs-display-time-modeline nil
  "Whether show the Real-time TIME in mode line, it's suggest not
set for that messy with modeline type, default to nil."
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** init-frame position
(defgroup entropy/emacs-initial-position nil
  "Startup position customized variable group configured for 'entropy-emacs'"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-init-fpos-enable nil
  "Whether set init emacs position by `entropy/emacs-set-frame-position'."
  :type 'boolean
  :group 'entropy/emacs-initial-position)

(defcustom entropy/emacs-init-frame-width-scale 0.6
  "The init scale of frame width within the fully width of
screen"
  :type 'sexp
  :group 'entropy/emacs-initial-position)

(defcustom entropy/emacs-init-frame-height-scale 0.93
  "The init scale of frame height within the fully height of
screen"
  :type 'sexp
  :group 'entropy/emacs-initial-position)

(defcustom entropy/emacs-init-fpos-y nil
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
  :group 'entropy/emacs-initial-position)

;; **** desktop save config
(defcustom entropy/emacs-desktop-enable nil
  "Enable desktop-save-mode and persistent scratch buffer"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** tab-width or disable it globally
(defcustom entropy/emacs-custom-tab-enable nil
  "Enable indent-tab-mode"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-custom-tab-width 4
  "Set the customized tab width"
  :type 'integer
  :group 'entropy/emacs-custom-variable-basic)

;; **** backgroud transparent
(defgroup entropy/emacs-transparent-bg nil
  "Group customized variable for frame loop alpha configured for entropy-emacs"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-init-loop-alpha t
  "Enable transparent at startup of emacs"
  :type 'boolean
  :group 'entropy/emacs-transparent-bg)

(defcustom entropy/emacs-loop-alpha-value '((85 100) (100 100))
  "The value of background transparent"
  :type 'boolean
  :group 'entropy/emacs-transparent-bg)

;; **** dired about
(defgroup entropy/emacs-customize-dired nil
  "Dired customized variables group configured for entropy-emacs."
  :group 'entropy/emacs-custom-variable-basic)

;; ***** dired trash enable
(defcustom entropy/emacs-dired-enable-trash nil
  "Enable trash function when using `dired-delete-file' in
`dired-mode'."
  :type 'boolean
  :group 'entropy/emacs-customize-dired)

;; **** eyebrowse config
(defgroup entropy/emacs-customize-eyebrowse nil
  "Customized variables group for entropy-emacs."
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-enable-eyebrowse-new-workspace-init-function nil
  "Enable personal function for eyebrowse to creating new
workspace."
  :type 'boolean
  :group 'entropy/emacs-customize-eyebrowse)

(defcustom entropy/emacs-eyebrowse-new-workspace-init-function 'entropy/emacs-create-scratch-buffer
  "Create the init buffer or others with you own function when
open one new eyebrowse workspace"
  :type 'sexp
  :group 'entropy/emacs-customize-eyebrowse)

;; **** ibuffer projectiitle config
(defcustom entropy/emacs-enable-ibuffer-projectitle nil
  "Enable ibuffer-projectitle in ibuffer

Note: ibuffer-projectitle will cause the performance debug.
"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

;; **** emacs web correspondings
(defgroup entropy/emacs-web nil
  "Group customized variables for emacs web correspondings configured for entropy-emacs.

Configurations for `eww' `w3m' `search-web' etc.
"
  :group 'entropy/emacs-custom-variable-basic)

;; ***** eww search engine set
(defcustom entropy/emacs-eww-search-engine-customize t
  "Enable eww search prefix customized"
  :type 'boolean
  :group 'entropy/emacs-web)

(defcustom entropy/emacs-eww-search-engine "https://www.bing.com/search?q="
  "Customized eww search prefix string and the default was 'bing.cn'"
  :type 'string
  :group 'entropy/emacs-web)

;; ***** search-web search-engines setting
(defcustom entropy/emacs-search-web-engines-internal
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
  :group 'entropy/emacs-web)

(defcustom entropy/emacs-search-web-engines-external
  '(("MOZILA MDN" "https://developer.mozilla.org/en-US/search?q=%s" External)
    ("php-manual" "https://php.net/manual-lookup.php?pattern=%s" External)
    ("github-external" "https://github.com/search?&q=%s" External)
    ("baidu" "http://www.baidu.com/s?wd=%s" External)
    ("google" "https://www.google.com/search?q=%s" External)
    ("google ja" "https://www.google.com/search?hl=ja&q=%s" External)
    ("google en" "https://www.google.com/search?hl=en&q=%s" External)
    ("google maps" "https://maps.google.co.jp/maps?hl=ja&q=%s" External)
    ("google scholar" "https://scholar.google.co.jp/scholar?q=%s" External)
    ("elpa" "https://elpa.gnu.org/packages/%s.html" External)
    ("melpa" "https://melpa.org/#/?q=%s" External)
    ("youtube" "https://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
    ("twitter" "https://twitter.com/search?q=%s" External)
    ("wikipedia en" "https://www.wikipedia.org/search-redirect.php?search=%s&language=en" External)
    ("wikipedia ja" "https://www.wikipedia.org/search-redirect.php?search=%s&language=ja" External)
    ("stackoveflow en" "https://stackoverflow.com/search?q=%s" External)
    ("stackoveflow ja" "https://ja.stackoverflow.com/search?q=%s" External))
  "External search-web engines"
  :type 'list
  :group 'entropy/emacs-web)
;; ***** personal browse-url function and varaiable
(defcustom entropy/emacs-enable-personal-browse-url-function nil
  "Whether enable personal browse-url function? If yes, set it to
  be 't'"
  :type 'boolean
  :group ' entropy/emacs-web)

(defcustom entropy/emacs-browse-url-function nil
  "The specific browse-url-function by your self,

Mostly of all, you should write your browse-url function with two args:
- 'url'
- '&rest args'

For example:

     (defun entropy/emacs-open-with-url (url &rest args)
       (interactive (browse-url-interactive-arg \"URL: \"))
       (w32-shell-execute
        \"open\"
        \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\"
        url))
"
  :type 'sexp
  :group 'entropy/emacs-web)

;; **** org config
(defgroup entropy/emacs-customize-org nil
  "Customized variable group for `org-mode' configured for 'entropy-emacs'"
  :group 'entropy/emacs-custom-variable-basic)

;; ***** org-bullet
(defcustom entropy/emacs-enable-org-bullets t
  "Enable org bullets"
  :type 'boolean
  :group 'entropy/emacs-customize-org)

(defcustom entropy/emacs-org-bullets-type "roman"
  "Choose org bullets type."
  :type 'string
  :group 'entropy/emacs-customize-org)

;; ***** windows org-download temp path
(defcustom entropy/emacs-win-org-download-file-name "c:\\temp\\screenshot.png"
  "Setting file name with it's path for screenshot in windows"
  :type 'string
  :group 'entropy/emacs-customize-org)

(defcustom entropy/emacs-win-org-download-screenshot-method "SnippingTool.exe"
  "Setting screenshot tool in windows and default be SnippingTool
  which is the componets of windows inside."
  :type 'string
  :group 'entropy/emacs-customize-org)

;; ***** org header scale
(defcustom entropy/emacs-disable-org-heading-scale t
  "Diable org heading auto-scale face feature."
  :type 'boolean
  :group 'entropy/emacs-customize-org)

;; **** Font setting
(defgroup entropy/emacs-customize-font nil
  "Fonts settng group for 'entropy-emacs'"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-font-setting-enable nil
  "Enable entropy-emacs specific font setting, when its non-nil.

If its non-nil, the valid value are 'google' for using google noto
family fonts, 'sarasa' for usign sarasa-gothic family which was a
fork for google noto refer, or 'fira-code' which was better for
daily development using.

When its value is 't', then fallback its to 'google'."
  :type 'boolean
  :group 'entropy/emacs-customize-font)

(defcustom entropy/emacs-font-chinese-type 'sc
  "The chinese font use type, 'sc' for simplified chinese, 'tc'
for traditional chinse, default to 'sc' which wildly include the
traditional chinese fonts already."
  :type 'boolean
  :group 'entropy/emacs-customize-font)

(defcustom entropy/emacs-font-size-default 10
  "Set the default font size for face-attribute.

Default size was 10, the upper limit was 15."
  :type 'sexp
  :group 'entropy/emacs-customize-font)

;; **** project search exec
(defcustom entropy/emacs-search-program "rg"
  "The project search engine used for =entropy-emacs=.

The valid value are \"ag\" (i.e. the silver_searcher) or \"rg\"
(i.e. the ripgrep searcher), default for using ripgrep which is
the fasest way.
"
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; **** language envrionment setting
(defgroup entropy/emacs-customize-language nil
  "Customize variables group of language environment
corresponding for 'entropy-emacs'."
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-custom-language-environment-enable nil
  "Enable custome language environment"
  :type 'boolean
  :group 'entropy/emacs-customize-language)

(defcustom entropy/emacs-locale-language-environment
  (copy-tree current-language-environment)
  "Setting emacs language environment"
  :type 'string
  :group 'entropy/emacs-customize-language)

(defcustom entropy/emacs-locale-coding-system
  (car default-process-coding-system)
  "Setting locale coding system."
  :type 'string
  :group 'entropy/emacs-customize-language)


;; **** use pyim
(defgroup entropy/emacs-customize-pyim nil
  "Pyim configration group for 'entropy-emacs'"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-enable-pyim nil
  "Enable pyim be the default pyin input method"
  :type 'boolean
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-use-backend 'internal
  "The pyim backend type choosing configuration."
  :type  '(choice
           (const internal)
           (const liberime))
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-liberime-scheme-data
  (cl-case system-type
    (gnu/linux "/usr/share/rime-data")
    (t nil))
  "The rime scheme-data directory using for liberime"
  :type 'string
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-liberime-cache-dir
  (expand-file-name "pyim/rime-cache" entropy/emacs-stuffs-topdir)
  "The cache dir for liberime"
  :type 'string
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-dicts nil
  "
Setting pyim-dicts file, if nil then use basic dicts for
minimal usage

You can setting like this:

  (setq entropy/emacs-pyim-dicts
        '((:name \"popular\":file \"~/.emacs.d/pydict/sougou.pyim\")
          (:name \"core\" :file \"~/.emacs.d/pydict/py++.pyim\")
          (:name \"pyim-greatdict\" :file \"~/.emacs.d/pydict/pyim-greatdict.pyim\")))
"
  :type 'sexp
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-cached-dir
  (expand-file-name "pyim/internal-cache" entropy/emacs-stuffs-topdir)
  "Set pyim cached dir, if nil use defaults setting (see
`pyim-dcache-directory')"
  :type 'string
  :group 'entropy/emacs-customize-pyim)

(defcustom entropy/emacs-pyim-tooltip nil
  "Setting the pyim toolitip method"
  :type 'sexp
  :group 'entropy/emacs-customize-pyim)

;; **** mpc
(defgroup entropy/emacs-customized-mpd nil
  "Customized variable group for MPD (music player daemon)"
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-mpd-host-url "127.0.0.1"
  "Mpd host url defautl to 'localhost'."
  :type 'boolean
  :group 'entropy/emacs-customized-mpd)

(defcustom entropy/emacs-mpd-host-port "9688"
  "Mpd host port defautl to '9688'."
  :type 'boolean
  :group 'entropy/emacs-customized-mpd)

;; **** gnus
(defgroup entropy/emacs-customize-gnus nil
  "Customize variable group of gnus configured for 'entropy-emacs'."
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-gnus-init-config
  `(:gnus-home ,(expand-file-name "gnus" entropy/emacs-stuffs-topdir)
    :gnus-news-dir ,(expand-file-name "gnus/News" entropy/emacs-stuffs-topdir)
    :mail-dir ,(expand-file-name "gnus/Mail" entropy/emacs-stuffs-topdir)
    :mail-temp-dir ,(expand-file-name "gnus/temp" entropy/emacs-stuffs-topdir)
    :init-file ,(expand-file-name "gnus/gnus-config.el" entropy/emacs-stuffs-topdir)
    :startup-file ,(expand-file-name "gnus/newsrc" entropy/emacs-stuffs-topdir)
    :read-newsrc nil
    :save-newsrc nil
    :use-dribble t
    :read-active-file t)
  "Initial setting for gnus for entropy-emacs."
  :type 'sexp
  :group 'entropy/emacs-customize-gnus)

;; **** entropy-emacs start with
(defcustom entropy/emacs-startwith-apps nil
  "The external apps for entropy-emacs start with.

This variable forms as one alist of each element's car was the
process-name and the cdr was the executable full path string for
just it's name."
  :type 'sexp
  :group 'entropy/emacs-custom-variable-basic)

;; **** ivy framework

(defgroup entropy/emacs-ivy-customize nil
  "Ivy framework customized variables group confitured for 'entropy-emacs'."
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-ivy-rich-type 'ivy-rich-mode
  "The enhancement for ivy-framework, icon abbreviation and other
information displayed in candidates show.

Two valid value defaulted by `entropy-emacs':

- `all-the-icons-ivy'

  The simple one for that, just ehance for `ivy-switch-buffer' and
  `counsel-find-file'.

- `ivy-rich-mode'

  The fully riched mode for that, see its document for more
  details."
  :type 'sexp
  :group 'entropy/emacs-ivy-customize)

;; *** elfeed config
(defgroup entropy/emacs-customize-elfeed nil
  "Emacs RSS client configuration group  for 'entropy-emacs'."
  :group 'extensions)

(defcustom entropy/emacs-elfeed-proxyfeeds-regexp-list '()
  "Regexp for matching the feeds which needed for updating through proxy."
  :type 'list
  :group 'entropy/emacs-customize-elfeed)

(defcustom entropy/emacs-elfeed-multi-update-feeds-list '()
  "Elfeed Feeds for update."
  :type 'list
  :group 'entropy/emacs-customize-elfeed)

(defcustom entropy/emacs-elfeed-url-no-proxy
  '("localhost"
    "127.0.0.1"
    "192.168.*"
    "10.*")
  "No proxy for elfeed proxy setting"
  :type 'list
  :group 'entropy/emacs-customize-elfeed)

(defcustom entropy/emacs-elfeed-retrieve-proxy "127.0.0.1:1081"
  "The default proxy host domain and port concated string for
elfeed proxy setting."
  :type 'string
  :group 'entropy/emacs-customize-elfeed)

;; *** pdumper
(defgroup entropy/emacs-customized-for-pdumper nil
  "Group customized variables for pdumper charging."
  :group 'extensions)

(defcustom entropy/emacs-do-pdumper-in-X t
  "Whether did pdumper for gui prot."
  :type 'boolean
  :group 'entropy/emacs-customized-for-pdumper)

(defcustom entropy/emacs-fall-love-with-pdumper nil
  "The emacs running type indication for pdumper."
  :type 'boolean
  :group 'entropy/emacs-customized-for-pdumper)

;; *** Coworkers
(defgroup entropy/emacs-coworkers-group nil
  "Group for coworkers refer customized variables."
  :group 'extensions)

(defcustom entropy/emacs-install-coworker-immediately nil
  "Install coworker immediatly in needed while."
  :type 'boolean
  :group 'entropy/emacs-coworkers-group)

(defcustom entropy/emacs-coworker-host-root
  (expand-file-name ".eemacs-local" (getenv "HOME"))
  "The coworker host root dir."
  :type 'string
  :group 'entropy/emacs-coworkers-group)

;; *** Codeserver
(defgroup entropy/emacs-codeserver-group nil
  "Group for eemacs code server refer customized variables."
  :group 'extensions)

(defcustom entropy/emacs-codeserver-prefer-pyls-type 'pyls
  "Choose preferred python language server type:

- 'mspyls': microsoft python lanuage server
- 'pyls':   official python language server (used as default)"
  :type 'symbol
  :group 'entropy/emacs-codeserver-group)


;; *** IDE
(defgroup entropy/emacs-ide-config nil
  "The IDE configurations group"
  :group 'extensions)

(defvar entropy/emacs-ide-for-them
  '(c-mode
    c++-mode cmake-mode
    java-mode go-mode
    python-mode php-mode
    js2-mode json-mode css-mode web-mode
    sh-mode powershell-mode))

(defvar entropy/emacs-use-ide-conditions nil)
(defun entropy/emacs-get-ide-condition-symbol (major-mode)
  (intern (format "entropy/emacs-use-ide-type-for-%s" major-mode)))
(defun entropy/emacs-get-use-ide-type (major-mode)
  (symbol-value
   (entropy/emacs-get-ide-condition-symbol major-mode)))

(let (forms)
  (dolist (el entropy/emacs-ide-for-them)
    (let ((sym (entropy/emacs-get-ide-condition-symbol el)))
      (push sym
            entropy/emacs-use-ide-conditions)
      (push
       `(defcustom ,sym 'lsp
          ,(format "The IDE chosen type for major-mode '%s'

Valid type are 'traditional' or 'lsp' which default to use lsp.
"
                   el)
          :type 'symbol
          :group 'entropy/emacs-ide-config)
       forms)))
  (dolist (form forms)
    (eval form)))

;; **** code folding group
(defgroup entropy/emacs-code-folding nil
  "customized variabel group for code folding and expanding
features."
  :group 'entropy/emacs-ide-config
  :prefix "entropy/emacs-code-folding-")

(defcustom entropy/emacs-code-folding-type 'yafolding
  "Type for code folding style embeded in entropy/emacs."
  :type '(choice
          (const native)
          (const yafolding))
  :group 'entropy/emacs-code-folding)

;; **** company config
(defgroup entropy/emacs-company-customized nil
  "The customize variables group for `company-mode' configured
for 'entropy-emacs'."
  :group 'entropy/emacs-ide-config)

(defcustom entropy/emacs-company-posframe-mode nil
  "Enable company-posframe.

Note:

company-posframe can not woring well with
`company-quickhelp-mode', and when set this variable to 't' will
automatically avoiding startup of `company-quickhelp-mode'."
  :type 'boolean
  :group 'entropy/emacs-company-customized)

(defcustom entropy/emacs-company-idle-delay-default 0.65
  "default eemacs specified set for `company-idle-delay`."
  :type 'sexp
  :group 'entropy/emacs-company-customized)

;; **** use highlight feature
(defgroup entropy/emacs-highlight nil
  "Personal Emacs configurations."
  :group 'entropy/emacs-ide-config)

;; ***** main enable
(defcustom entropy/emacs-use-highlight-features t
  "Enable highlight feature package `init-highlight.el'."
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** highlight-indention
(defcustom entropy/emacs-hl-highlight-indention-enable-at-startup nil
  "Enable indention highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** symbol-over-lay
(defcustom entropy/emacs-hl-sysmbol-overlay-enable-at-startup nil
  "Enable symbol-overlay highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** highlight-parentheses
(defcustom entropy/emacs-hl-highlight-parentheses-mode-enable-at-startup nil
  "Enable highlight-parentheses highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** rainbow delimiters
(defcustom entropy/emacs-hl-rainbow-delimiters-enable-at-startup nil
  "Enable rainbow-delimiters highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** hl-todo-mode
(defcustom entropy/emacs-hl-todo-enable-at-startup nil
  "Enable hl-todo highlight feature which can show color of
  `TODO' keywords universal not only the org file"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** diff-hl
(defcustom entropy/emacs-hl-diff-hl-enable-at-startup nil
  "Enable diff-hl highlight feature which can show the git diff
in current buffer"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; ***** whitespace
(defcustom entropy/emacs-hl-whitespace-enable-at-startup nil
  "Enable whitespace highlight feature"
  :type 'boolean
  :group 'entropy/emacs-highlight)

(defcustom entropy/emacs-hl-whitespace-auto-cleanup nil
  "Auto clean up messy or redundant whitespace when in `whitespace-mode'

Notice: If enable this will cause git diff log be messy with long
difference.
"
  :type 'boolean
  :group 'entropy/emacs-highlight)

;; *** eshell
(defgroup entropy/emacs-eshell-group nil
  "Custom variable group for eshell in entropy-emacs. "
  :group 'extensions)

(defcustom entropy/emacs-eshell-alias-file "~/.eshell-alias"
  "entropy-emacs eshell alias file location."
  :type 'string
  :group 'entropy/emacs-eshell-group)

(defcustom entropy/emacs-eshell-history-file "~/.eshell-history"
  "entropy-emacs eshell history file location."
  :type 'string
  :group 'entropy/emacs-eshell-group)

;; *** major-modes
;; **** markdown mode
(defgroup entropy/emacs-markdown-mode-spec nil
  "Custom variables group for markdown mode"
  :group 'extensions)

(defcustom entropy/emacs-markdown-exp-header-context-type
  "application/xhtml+xml"
  "Content type string for the http-equiv header in XHTML output.
When set to an empty string, this attribute is omitted.  Defaults to
‘text/html’."
  :type 'string
  :group 'entropy/emacs-markdown-mode-spec)

(defcustom entropy/emacs-markdown-exp-header-content
  "
        <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
        <style>
        body {
          box-sizing: border-box;
          max-width: 740px;
          width: 100%;
          margin: 40px auto;
          padding: 0 10px;
        }
        </style>
        <script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
        <script>
        document.addEventListener('DOMContentLoaded', () => {
          document.body.classList.add('markdown-body');
          document.querySelectorAll('pre[lang] > code').forEach((code) => {
            code.classList.add(code.parentElement.lang);
            hljs.highlightBlock(code);
          });
        });
        </script>
"
  "Additional content to include in the XHTML <head> block."
  :type 'string
  :group 'entropy/emacs-markdown-mode-spec)

(defcustom entropy/emacs-markdown-exp-css-paths
  '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
    "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
  "URL of CSS file to link to in the output XHTML."
  :type 'list
  :group 'entropy/emacs-markdown-mode-spec)

(defcustom entropy/emacs-markdown-preview-stylesheets
  (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
        "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
        "
         <style>
          .markdown-body {
            box-sizing: border-box;
            min-width: 200px;
            max-width: 980px;
            margin: 0 auto;
            padding: 45px;
          }

          @media (max-width: 767px) {
            .markdown-body {
              padding: 15px;
            }
          }
         </style>")
  "List of client stylesheets for preview."
  :type 'list
  :group 'entropy/emacs-markdown-mode-spec)

(defcustom entropy/emacs-markdown-preview-javascript
  (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
        "
         <script>
          $(document).on('mdContentChange', function() {
            $('pre code').each(function(i, block) {
              hljs.highlightBlock(block);
            });
          });
         </script>")
  "List of client javascript libs for preview."
  :type 'list
  :group 'entropy/emacs-markdown-mode-spec)

;; **** common lisp mode
(defgroup entropy/emacs-common-lisp-mode-spec nil
  "Customized variables group for `lisp-mode' and `slime'."
  :group 'extensions)

(defcustom entropy/emacs-inferior-lisp-program "sbcl"
  "Eemacs specified lisp program value redirected to
`inferrior-lisp-program'."
  :type 'string
  :group 'entropy/emacs-common-lisp-mode-spec)

(defcustom entropy/emacs-slime-lisp-implementations
  '((sbcl ("sbcl") :coding-system utf-8-unix))
  "Eemacs specified `slime-lisp-implementation' value."
  :type 'sexp
  :group 'entropy/emacs-common-lisp-mode-spec)

;; *** specific for windows
(defgroup entropy/emacs-win nil
  "Personal Emacs configurations for windwos."
  :group 'extensions)

;; **** w32 ime config
(defcustom entropy/emacs-win-init-ime-enable nil
  "Enable win32 ime at startup (offset to the bug of w32-ime)."
  :type 'boolean
  :group 'entropy/emacs-win)

;; **** emacs lang set for windows
(defcustom entropy/emacs-win-env-lang-enable nil
  "Whether enable customize shell lanugage environment for
WINDOWS emacs session."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-env-lang-set "en_US.UTF-8"
  "Setting emacs lang in windows operation system, its useful
while you run a process communicates with the inferior daemon
which has a special language environment request."
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable wsl in windows
(defcustom entropy/emacs-wsl-enable nil
  "Set whether you want to use wsl?"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-wsl-apps "c:/git-portable/usr/bin/"
  "Set the baisic wsl apps path for basic shell-command using
which also used in shell-buffer.

Note: now it's just suitable for git-for-windows (no sdk-extra utilitie included)"
  :type 'string
  :group 'entropy/emacs-win)


(defcustom entropy/emacs-wsl-enable-extra nil
  "Enable extra wsl apps.

This ON-OFF variable are setted for follow occurrence:

    If you setting `entropy/emacs-wsl-apps' to
    'git-for-windows-minimal' which just contained the basic
    UNIX-LIKE commands that doesn't contianed commands like 'man'
    and 'tree' or sth else, you want to using them as well in
    current emacs session.

See customized variable `entropy/emacs-wsl-apps-extra' for
details.
  "
  :type 'boolean
  :group 'entropy/emacs-win)


(defcustom entropy/emacs-wsl-apps-extra ""
  "Set the extra wsl apps path, used for woman or other
subprocess of emacs that sth called lying on `exec-path'.

And this must using the type for the root of wsl-tool path(which
we can search the folder stucter of 'usr/bin' under this root
directly), like if you using msys2 , you must set this variable
to like:

\"c:/msys2/\"
"
  :type 'string
  :group 'entropy/emacs-win)

;; ***** windows git portable setting
(defcustom entropy/emacs-git-portable nil
  "Whether enable portable git application when you want to use
  git portable "
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-git-portable-path "c:/git-portable/cmd/"
  "When you enable git-custom set the customize git applicaton's
path default path was my git path please must modify it for yourself"
  :type 'string
  :group 'entropy/emacs-win)

;; **** Adding path for emacs built 'bin' folder
(defcustom entropy/emacs-win-emacs-bin-path-add t
  "Whether adding emacs bin folder to path on windows platform.

This ON-OFF varaible setted for adding emacs bin folder to both of
`exec-path' and \"PATH\" system variable.

It's useful that your can call emacs or other bult-in binary as
'convert' of builtin imagemaick support.
  "
  :type 'boolean
  :group 'entropy/emacs-win)

;; **** wsl terminal setting
(defcustom entropy/emacs-wsl-terminal-enable nil
  "Whether enable the wsl-bash,wsl path config worked when you set this `t'"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-wsl-terminal "c:/git-portable/git-bash.exe"
  "Set the default wsl bash applictions,it suggested to set same
as the wsl-apps main controled applications which I suggested use
git-for-windows-sdk `git-bash.exe'"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable fakecygpty
(defcustom entropy/emacs-win-fakecygpty-enable nil
  "Whether enable fake pty for enabling windows ansi-term."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-fakecygpty-path "c:/fakecgypty"
  "The fakecygpty compiled binaries archive location."
  :type 'string
  :group 'entropy/emacs-win)

;; **** gcc and mingw64 setting
(defcustom entropy/emacs-win-portable-mingw-enable nil
  "Enable portable mingw64 in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-mingw-path "c:/mingw64/bin/"
  "Setting the path of portable mingw64 for windows plattform
  when `entropy/emacs-win-portable-mingw-enable' was set to 't'"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-gcc-parameter ""
  "Setting the gcc compile parameter"
  :type 'string
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-g++-parameter ""
  "Setting the g++ compile parameter"
  :type 'string
  :group 'entropy/emacs-win)

;; **** clang setting
(defcustom entropy/emacs-win-portable-clang-enable nil
  "Enable clang for windows plattform"
  :type 'boolean
  :group 'entropy/emacs-custom-variable-basic)

(defcustom entropy/emacs-win-portable-clang-path ""
  "Path for portable clang for windows plattform."
  :type 'string
  :group 'entropy/emacs-custom-variable-basic)

;; **** windows texlive setting
(defcustom entropy/emacs-win-portable-texlive-enable nil
  "Whether to enable texlive in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-texlive-path "c:/texlive/bin"
  "Set up texlive path in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable grep in windows
(defcustom entropy/emacs-win-portable-grep-enable nil
  "Enable windows grep program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-grep-path "c:/grep/bin"
  "Set windows grep exec path"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable ag in windows
(defcustom entropy/emacs-win-portable-ag-enable nil
  "Enable windows ag program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-ag-path "~/.emacs.d/ag/bin/"
  "Set windows ag (The Silver Searcher) path"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable rg in windows
(defcustom entropy/emacs-win-portable-rg-enable nil
  "Enable windows ripgrep program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-rg-path "~/.emacs.d/rg/bin/"
  "Set windows ag path"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable pt in windows
(defcustom entropy/emacs-win-portable-pt-enable nil
  "Enable windows pt program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-pt-path "~/.emacs.d/pt/bin/"
  "Set windows pt path"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable cmder in windows
(defcustom entropy/emacs-Cmder-enable nil
  "Enable windows Cmder program"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-Cmder-path "c:/cmder/bin/Cmder.exe"
  "Set windows Cmder path"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable php in windows
(defcustom entropy/emacs-win-portable-php-enable nil
  "Enable php portable in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-php-path "c:/php/"
  "Setting the path of portable php executable"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enbale pip in windows
(defcustom entropy/emacs-win-portable-pip-enable nil
  "Enable portable pip in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-pip-path "c:/python/bin/"
  "Set protable pip path in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable python in windows
(defcustom entropy/emacs-win-portable-python-enable nil
  "Enable portable python in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-python-path "c:/python/bin/"
  "Set protable python path in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable nodejs in windows
(defcustom entropy/emacs-win-portable-nodejs-enable nil
  "Enable nodejs portale in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-nodejs-path "c:/nodejs/"
  "Setting the path of protable nodejs in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable opencc in windows
(defcustom entropy/emacs-win-portable-opencc-enable nil
  "Enable opencc portale in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-opencc-path "c:/opencc/"
  "Setting the path of protable opencc in windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable pandoc in windows
(defcustom entropy/emacs-win-portable-pandoc-enable nil
  "Enable portable pandoc in windows"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-pandoc-path nil
  "Setting portble pandoc path for windows"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enbale portble jdk
(defcustom entropy/emacs-win-portable-jdk-enable nil
  "Enable using portble JDK"
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-jdk-path "c:/JDK64/bin"
  "Setting portble JDK pth"
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable zeal
(defcustom entropy/emacs-win-portable-zeal-enable 'nil
  "Enable zeal doc http://zealdocs.org/."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-zeal-path ""
  "Setting zeal path on windows plattform."
  :type 'string
  :group 'entropy/emacs-win)

;; **** enable putty in windows
(defcustom entropy/emacs-win-portable-putty-enable nil
  "Enable putty portable on windows."
  :type 'boolean
  :group 'entropy/emacs-win)

(defcustom entropy/emacs-win-portable-putty-path ""
  "Windows portable putty path."
  :type 'string
  :group 'entropy/emacs-win)

;; ** Top APIs
;; *** top paths
(defvar entropy/emacs-hosted-path
  (file-name-directory load-file-name)
  "entropy-emacs hosted path")

(defvar entropy/emacs-core-components-hosted-path
  (expand-file-name "core" entropy/emacs-hosted-path)
  "entropy-emacs core library hosted path.")

(defvar entropy/emacs-site-lisp-path
  (expand-file-name "site-lisp" entropy/emacs-hosted-path))

(defvar entropy/emacs-fancy-splash-logo-file
  (expand-file-name "logo/logo.png" entropy/emacs-core-components-hosted-path)
  "Set emacs logo. nil means official logo.")

(defvar entropy/emacs-fancy-splash-text-logo-file
  (expand-file-name "logo/logo.txt"
                    entropy/emacs-core-components-hosted-path)
  "Text logo file.")

(defvar entropy/emacs-initial-theme-path
  (expand-file-name "startup-theme" entropy/emacs-core-components-hosted-path)
  "Initial theme path for entropy-emacs. ")

(defvar entropy/emacs-templates-dir
  (expand-file-name "templates" entropy/emacs-core-components-hosted-path)
  "The sourced templated files archive location of entropy-emacs")

(defvar entropy/emacs-doc-path
  `(:org
    ,(expand-file-name
      "entropy-emacs-doc/org/entropy-emacs_introduction.org"
      entropy/emacs-site-lisp-path)
    :html
    ,(expand-file-name
      "entropy-emacs-doc/org/entropy-emacs_introduction.html"
      entropy/emacs-site-lisp-path)
    :texi
    ,(expand-file-name
      "entropy-emacs-doc/org/entropy-emacs_introduction.texi"
      entropy/emacs-site-lisp-path)
    :texinfo
    ,(expand-file-name
      "entropy-emacs-doc/org/entropy-emacs_introduction.info"
      entropy/emacs-site-lisp-path)))

;; *** run-hooks with prompt
(defun entropy/emacs-run-hooks-prompt (orig-func &rest orig-args)
  "Prompt for `run-hooks' for reduce lagging nervous. It's a
advice wrapper, do not calling it in the normal way"
  (let ((condis (lambda ()
                  (or noninteractive
                      (window-minibuffer-p))))
        (indicator (car orig-args))
        rtn)
    (unless (funcall condis)
      (if (symbolp indicator)
          (message "Running hooks '%s' ..." indicator)
        (message "Running hooks ..."))
      (redisplay t)
      (sleep-for 0.0001))
    (setq rtn (apply orig-func orig-args))
    (unless (funcall condis)
      (message ""))
    rtn))

(defmacro entropy/emacs-run-hooks-with-prompt (&rest body)
  "Do BODY with `run-hooks' with prompts feature based on
`entropy/emacs-run-hooks-prompt'."
  `(let (rtn)
     (advice-add 'run-hooks :around
                 #'entropy/emacs-run-hooks-prompt)
     (unwind-protect
         (progn
           (setq rtn
                 (progn
                   ,@body))
           (advice-remove 'run-hooks
                          #'entropy/emacs-run-hooks-prompt)
           rtn)
       (advice-remove 'run-hooks
                      #'entropy/emacs-run-hooks-prompt))))

;; *** entropy-emacs init hooks
(defvar entropy/emacs-init-mini-hook '()
  "Hooks for minimal start.")

(defvar entropy/emacs-init-X-hook '()
  "Hooks of entropy-emacs X init.")

(defvar entropy/emacs-pdumper-load-hook nil
  "Hook for run with pdumper session startup.")

(defun entropy/emacs-select-trail-hook ()
  "Automatically selects a hook specified meaning of a
=entropy-emacs= trail hook. Return for
`entropy/emacs-init-mini-hook' and `entropy/emacs-init-X-hook' if
`entropy/emacs-fall-love-with-pdumper' is nil.

At any time, only one of those hook is recongnized as
=entropy-emacs= trail hook, and it is a hook for trigger
configration enable when all the preparations have done.

See `entropy/emacs-startup-end-hook' either."
  (if entropy/emacs-fall-love-with-pdumper
      'entropy/emacs-pdumper-load-hook
    (if entropy/emacs-minimal-start
        'entropy/emacs-init-mini-hook
      'entropy/emacs-init-X-hook)))

(defvar entropy/emacs-startup-end-hook nil
  "Hook ran after entropy-emacs finally initial-done, all the
functions hosted in this Hook will ran after =entropy-emacs= trail
hook.

Also see `entropy/emacs-run-startup-end-hook' for restriction
description.")

;; *** making procedure
(defun entropy/emacs-is-make-session ()
  "Obtained the 'EEMACS_MAKE' env variable value if valid
otherwise return nil."
  (require 'subr-x)
  (let ((env-p (getenv "EEMACS_MAKE")))
    (cond
     ((or (null env-p)
          (string-empty-p env-p))
      nil)
     (t
      env-p))))

;; *** ssh session justice
(defun entropy/emacs-is-ssh-session ()
  "Justice whether use eemacs in sshd session. Take priority of
`entropy/emacs-indicate-sshd-session'."
  (or entropy/emacs-indicate-sshd-session
      (getenv "SSH_CLIENT")
      (getenv "SSH_TTY")))

;; *** pdumper env check

(defun entropy/emacs-in-pdumper-procedure-p ()
  "Judge whether in pdumper procedure according to the
`command-line-args'."
  (let (rtn)
    (catch :exit
      (dolist (arg command-line-args)
        (when (string-match-p "dump-emacs-portable" arg)
          (setq rtn t)
          (throw :exit nil))))
    rtn))

;; ** entropy-emacs initialize
;; *** intial advice
;; **** find file patch
(define-inline entropy/emacs--unslash-path (path)
  "Remove the final slash in PATH."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (path)
    (inline-quote
     (if (and (> (length ,path) 1)
              (eq ?/ (aref ,path (1- (length ,path)))))
         (substring ,path 0 -1)
       ,path))))

(defun entropy/emacs--dwim-abs-find-file (orig-func &rest orig-args)
  "Find file with enabled `find-file-visit-truename' when file
under the symbolink root dir."
  (let* ((filename (entropy/emacs--unslash-path (car orig-args)))
         (rest (cdr orig-args))
         (ftruename (entropy/emacs--unslash-path (file-truename filename))))
    (let ((find-file-visit-truename
           (and
            (not (equal filename ftruename))
            (not (file-directory-p filename)))))
      (apply orig-func orig-args))))
(advice-add 'find-file-noselect :around #'entropy/emacs--dwim-abs-find-file)


;; *** load specifications
(let ((cus entropy/emacs-custom-common-file))
  (setq-default custom-file entropy/emacs-custom-common-file)
  (when (file-exists-p cus)
    (message "")
    (message "====================================")
    (message "[Loading] custom specifications ...")
    (message "====================================\n")
    (load cus)))

;; *** add eemacs texinfo to info-path

(setq Info-default-directory-list
      (append (list
               (file-name-directory
                (plist-get entropy/emacs-doc-path :texinfo)))
              Info-default-directory-list))

;; *** fake display-graphic
(defun entropy/emacs-display-graphic-fake-advice
    (orig-func &rest orig-args)
  "The `display-graphic-p' around advice for some case that needs
to forcely judge as a displayable status.

This also affects `display-multi-font-p' because it's an alias of
that."
  (cond
   ((and (or entropy/emacs-fall-love-with-pdumper
             (equal (entropy/emacs-is-make-session) "Dump"))
         entropy/emacs-do-pdumper-in-X)
    t)
   (t
    (apply orig-func orig-args))))

(advice-add 'display-graphic-p
            :around
            #'entropy/emacs-display-graphic-fake-advice)

;; Disable `entropy/emacs-display-graphic-fake-advice' when common
;; start procedure (before any other trail hook run) finished, thus
;; when pdumper session start, `display-graphic-p' function will not
;; cause some issue e.g. `window-font-width' will throw out error that
;; the very beginning of pdumper session is starting on cli status so
;; that `font-info' will retrieve 'nil' as the callback of
;; `face-font'.
(let ((hook (if entropy/emacs-minimal-start
                'entropy/emacs-init-mini-hook
              'entropy/emacs-init-X-hook)))
  (add-hook hook
            #'(lambda ()
                (advice-remove
                 'display-graphic-p
                 #'entropy/emacs-display-graphic-fake-advice))))

;; *** clean stuff files
(let ((top entropy/emacs-stuffs-topdir))
  (unless (file-exists-p top)
    (make-directory top))
  ;; root dir host in `top'
  (dolist (item `((bookmark-file . "bookmarks")
                  (recentf-save-file . "recentf")
                  (tramp-persistency-file-name . "tramp")
                  (auto-save-list-file-prefix
                   .
                   ,(cond ((eq system-type 'ms-dos)
                           ;; MS-DOS cannot have initial dot, and allows only 8.3 names
                           "auto-save.list/_s")
                          (t
                           "auto-save-list/.saves-")))
                  ;; savehist caches
                  (savehist-file . "history")
                  (save-place-file . "places")
                  ;; emms caches
                  (emms-directory . "emms/")
                  ;; eshell files
                  (eshell-directory-name . "eshell/")
                  ;; transient files
                  (transient-levels-file . "transient/levels.el")
                  (transient-values-file . "transient/values.el")
                  (transient-history-file . "transient/history.el")
                  ;; url caches
                  (url-configuration-directory . "url/")
                  (nsm-settings-file . "network-security.data")
                  ;; lsp mode
                  (lsp-session-file . "lsp/lsp-session-v1")
                  (lsp-intelephense-storage-path . "lsp/cache/intelephense")
                  (lsp-server-install-dir . "lsp/cache/install/")
                  (lsp-java-workspace-dir . "lsp/workspace/")
                  ;; async log file
                  (async-byte-compile-log-file . "async-bytecomp.log")
                  ;; slime
                  (slime-repl-history-file . "slime-history.eld")
                  ;; irony srever dir
                  (irony-user-dir . "irony/")
                  ;; treemacs
                  (treemacs-persist-file . "treemacs/treemacs-persist")
                  (treemacs-last-error-persist-file . "treemacs/treemacs-persist-at-last-error")
                  ;; projetile
                  (projectile-known-projects-file . "projectile/projectile-bookmarks.eld")
                  (projectile-cache-file . "projectile/projectile.cache")
                  ;; image dired
                  (image-dired-dir . "image-dired/")
                  ;; game dir
                  (gamegrid-user-score-file-directory . "games/")
                  ;; vimish
                  (vimish-fold-dir . "vimish-fold/")
                  ;; anaconda mode
                  (anaconda-mode-installation-directory . "anaconda-mode/")
                  ;; newsticker archive dir
                  (newsticker-dir . "newsticker/")
                  ;; miscellanies
                  (idlwave-config-directory . "idlwave/")
                  ))
    (let* ((var-sym (car item))
           (path (expand-file-name (cdr item) top))
           (path-root (file-name-directory path)))
      (set var-sym path)
      ;; create each subs path chain for preventing unconditionally
      ;; file create fatal from thus.
      (unless (file-directory-p path-root)
        (make-directory path-root t))))

  ;; directly set root dir using `top' dir
  (dolist (item '(eww-bookmarks-directory))
    (set item top)))

;; * provide
(provide 'entropy-emacs-defcustom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
