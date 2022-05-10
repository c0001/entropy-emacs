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
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; ** Internal init
(defvar __eemacs-ext-union-host
  (expand-file-name "~/.config/entropy-config/entropy-emacs"))

;; ** Customizable Variables

;; *** Fundamental
(defgroup entropy/emacs-customize-group-for-fundametal-configuration nil
  "Eemacs fundamental configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-user-full-name (getenv "USERNAME")
  "The value for `user-full-name' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-user-mail-address "Thanos@comos.com"
  "The value for `user-mail-address' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-top-prefix-key-cons '("C-`" .  "C-@")
  "The cons stores the prefix key for `entropy/emacs-top-keymap',
the car for GUI session, and cdr for TUI thus as well.

The intention to get different prefix key for GUI anD TUI is for
that the terminal emulation used for emacs may not have the full
key-stroke experience."
  :type '(cons (key-sequence :tag "GUI bind") (key-sequence :tag "TUI bind"))
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-stuffs-topdir
  (expand-file-name "stuffs" entropy/emacs-user-emacs-directory)
  "The stuffs collection host path, for as `savehist-file',
`bookmark-file' cache host. This setting mainly for cleanup
`entropy/emacs-user-emacs-directory'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-temporary-file-directory
  (expand-file-name "tmp" entropy/emacs-stuffs-topdir)
  "The emacs session spec tmp host path as the system '/tmp' dir."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(make-directory entropy/emacs-temporary-file-directory t)
(setq temporary-file-directory entropy/emacs-temporary-file-directory)

(defcustom entropy/emacs-minimal-start nil
  "Whether start eemacs with minmal feature loaded.

This is used for getting vanilla emacs like experience in
eemacs."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-disable-mouse-at-init-time nil
  "Disable mounse event handler at startup when set non-nil."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-garbage-collection-delay 5
  "The defaults garbage collection idle delay secons setting
for entropy-emacs."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-garbage-collection-message-p nil
  "whether echo garbage collection message."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-custom-enable-lazy-load t
  "Enable lazy load for entropy-emacs when non-nil.

Notice: when `entropy/emacs-fall-love-with-pdumper' is non-nil or
in daemon session, this variable will be pressed whatever init
value assignments into."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-use-recentf t
  "Whether use recentf-mode after emacs init."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-indicate-sshd-session nil
  "Indicate whether current emacs session is on ssh remote
session."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-startwith-apps nil
  "The external apps for entropy-emacs start with.

This variable forms as one alist of each element's car was the
process-name and the cdr was the executable full path string for
just it's name."
  :type '(choice (const nil)
                 (repeat
                  (cons (string :tag "Process name")
                        (file :tag "Process executable file"))))
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

;; *** UI
(defgroup entropy/emacs-customize-group-for-UI nil
  "Eemacs UI configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-use-icon t
  "Whether to use icon visualization when available."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-UI)

;; **** Initial UI
(defgroup entropy/emacs-customize-group-for-initial-ui nil
  "Eemacs initial ui configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-enable-initial-dashboard t
  "Enable entropy emacs initial dashboard instead of emacs
default one.

Valid value are 't' or 'rich', otherwise disable this
feature.

When value are either 't' or 'rich', a fancy simple splash buffer
`entropy/emacs-init-welcome-buffer-name' will startup firstly, and
then enable the rich dashbord contents when value is 'rich'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-initial-ui)

(defcustom entropy/emacs-enable-visual-bell-at-startup nil
  "Enabe visual bell UI feedback at startup."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-initial-ui)

;; ***** Frame Position
(defgroup entropy/emacs-customize-group-for-initial-position nil
  "Eemacs emacs initial position configuration customizable group."
  :group 'entropy/emacs-customize-group-for-initial-ui)

(defcustom entropy/emacs-init-fpos-enable nil
  "Whether set init emacs position by `entropy/emacs-set-frame-position'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-initial-position)

(defcustom entropy/emacs-init-frame-width-scale 0.76
  "The init scale of frame width within the fully width of
screen"
  :type 'number
  :group 'entropy/emacs-customize-group-for-initial-position)

(defcustom entropy/emacs-init-frame-height-scale 0.93
  "The init scale of frame height within the fully height of
screen"
  :type 'number
  :group 'entropy/emacs-customize-group-for-initial-position)

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
  |   |    | \\-                  |      |
  |   |    |   \\-                |      |
  |   v    |     \\-              |      |
  |   x    |       emacs position |  ----+------->  emacs height
  |        |                      |      |
  |        |                      |      |
  |        |                      |      |
  |        |                      |      |
  +--------+----------------------+------+
#+END_EXAMPLE"
  :type '(choice number (const :tag "Not set" nil))
  :group 'entropy/emacs-customize-group-for-initial-position)

;; **** Themes
(defgroup entropy/emacs-customize-group-for-ui-theme nil
  "Eemacs ui theme configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-theme-options 'ujelly
  "Choice for emacs theme"
  :type 'symbol
  :group 'entropy/emacs-customize-group-for-ui-theme)

(defcustom entropy/emacs-solaire-themes-regex-list
  '("^doom-"
    "^atom-one-dark"
    "^spacemacs-")
  "Themes name regex matchs for solaire-mode."
  :type '(repeat string)
  :group 'entropy/emacs-customize-group-for-ui-theme)

;; **** Modeline
(defgroup entropy/emacs-customize-group-for-modeline nil
  "Eemacs mode-line configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-modeline-style "origin"
  "Choose the modeline style:

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
  :group 'entropy/emacs-customize-group-for-modeline)

(defcustom entropy/emacs-enable-modeline-toggle t
  "Enable modeline toggle function `entropy/emacs-mdl-powerline'
and `entropy/emacs-mdl-spaceline' and the customized effectively
of `entropy/emacs-modeline-style'.

Note: spaceline and powerline will cause lagging performancs
issue for emacs while you are in the low performance computer
that you computer is with the older hardware or be out of repair
for a long time and so as the bad head dispersion."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-modeline)


(defcustom entropy/emacs-display-time-modeline nil
  "Whether show the Real-time TIME in mode line, it's suggest not
set for that messy with modeline type, default to nil."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-modeline)

;; **** Fonts
(defgroup entropy/emacs-customize-group-for-eemacs-font-spec nil
  "Eemacs font specifications configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-font-setting-enable nil
  "Enable entropy-emacs specific font setting, when its non-nil.

If its non-nil, the valid value are 'google' for using google noto
family fonts, 'sarasa' for usign sarasa-gothic family which was a
fork for google noto refer, or 'fira-code' which was better for
daily development using.

When its value is 't', then fallback its to 'fira-code'.

NOTE:

1) 'sarasa' is CJK and latin width balance font spec, pretty
aequilate table messed with thus are ok in `org-mode' etc.

2) In old platform without high perfomance ssd and cpu
supporting, using 'sarasa' fonts is not suggested since its size
is so large for each font-family spec, and emacs will using much
more time to loading and caching it while redisplay even for more
gc time."
  :type '(choice
          (const :tag "Disable" nil)
          (const :tag "Default" t)
          (const :tag "Google font family" google)
          (const :tag "Sarasa font family" sarasa)
          (const :tag "Fira code based" fira-code))
  :group 'entropy/emacs-customize-group-for-eemacs-font-spec)

(defcustom entropy/emacs-font-chinese-type 'sc
  "The chinese font use type, 'sc' for simplified chinese, 'tc'
for traditional chinse, default to 'sc' which wildly include the
traditional chinese fonts already."
  :type '(choice
          (const :tag "Simplified chinese" sc)
          (const :tag "traditional chinese" tc))
  :group 'entropy/emacs-customize-group-for-eemacs-font-spec)

(defcustom entropy/emacs-font-size-default 10
  "Set the default font size for face-attribute.

Default size was 10, the upper limit was 24.

This variable is always useable whether
`entropy/emacs-font-setting-enable' is setted but with special tweak
when `entropy/emacs-font-setting-enable' is enabled, or just set the
frame default font size."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-eemacs-font-spec)

;; **** Transparent
(defgroup entropy/emacs-customize-group-for-transparent-backgroud nil
  "Eemacs emacs frame background transparent configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-start-with-frame-transparent-action t
  "Enable frame transparent at startup of =entropy-emacs=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-transparent-backgroud)

(defcustom entropy/emacs-loop-alpha-value 85
  "The default initialize integer value of frame background
transparent, any customization of it may be restrict by the
target process."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-transparent-backgroud)

;; **** Window
(defgroup entropy/emacs-customize-group-for-window-ui nil
  "Eemacs general media player configuration customizable group."
  :group 'entropy/emacs-customize-group-for-UI)

(defcustom entropy/emacs-window-center-mode-use-backend 'olivetti
  "The window central visual trigger type, valid of 'olivetti' or 'basic'.

=olivetti= was a riched window center align package which provide
shrink and expand auto-key, and without alignment be killed
problem, =basic= type is simple but without fully featured.
"
  :type '(choice
          (const :tag "Olivetti mode" olivetti)
          (const :tag "Basic simple way" basic))
  :group 'entropy/emacs-customize-group-for-window-ui)

(defcustom entropy/emacs-window-center-integer 10
  "The integer number used for eemacs window centered operation to
divide the `window-width' for calculating the margin width,
that's say if `window-width'(without margin set) is 135 and this
divider is 10 and then the margin both of left and the right
width (columns count as `set-window-margins') will be set by
`entropy/emacs-window-center-calc-margin-width' seemly as:
: (round (/ 135 (float 10)))
"
  :type 'integer
  :group 'entropy/emacs-customize-group-for-window-ui)

(defcustom entropy/emacs-window-center-auto-mode-enable-p nil
  "The customized variable for indicate whether use center window
feature (see `entropy/emacs-window-center-mode-use-backend' for
details) in some occasions automatically.

NOTE: do not use this variable to judge the case in lisp code,
use
`entropy/emacs-window-auto-center-mode-base-condition-satisfied-judge'
instead since this variable is just a top level user interface
which is not a lisp API."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-window-ui)

;; *** M-x
(defgroup entropy/emacs-customize-group-for-M-x nil
  "Eemacs completion framework configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-command-completion-use-style 'ivy
  "The =entropy-emacs= command completion style.

Valid value are:

- 'ivy': use `ivy-read'.
- 'nil': use native `completion-read'.
"
  :type '(choice
          (const :tag "ivy" ivy)
          (const :tag "native" nil))
  :group 'entropy/emacs-customize-group-for-M-x)

;; **** ivy
(defgroup entropy/emacs-customize-group-for-ivy-mode nil
  "Eemacs ivy-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-M-x)

(defcustom entropy/emacs-ivy-rich-type 'ivy-rich-mode
  "The enhancement for ivy-framework, icon abbreviation and other
information displayed in candidates show. NIL for disabled this
feature.

Two valid value defaulted by `entropy-emacs':

- `all-the-icons-ivy'

  The simple one for that, just ehance for `ivy-switch-buffer' and
  `counsel-find-file'.

- `ivy-rich-mode'

  The fully riched mode for that, see its document for more
  details."
  :type '(choice
          (const :tag "Simple ivy icons" all-the-icons-ivy)
          (const :tag "Ivy rich mode" ivy-rich-mode)
          (const :tag "Neither to use" nil))
  :group 'entropy/emacs-customize-group-for-ivy-mode)

(defcustom entropy/emacs-ivy-rich-extra-display-transformers-list
  nil
  "Extra `ivy-rich-display-transformers-list' specified which
will combined with =entropy-emacs= internal specification."
  :type '(repeat sexp)
  :group 'entropy/emacs-customize-group-for-ivy-mode)

;; *** Editor
(defgroup entropy/emacs-customize-group-for-editor-common nil
  "Eemacs common edidtor configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** tab-width config
(defgroup entropy/emacs-customize-group-for-editor-tab-width nil
  "Eemacs editor tab-width configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-custom-tab-enable nil
  "Enable indent-tab-mode"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-editor-tab-width)

(defcustom entropy/emacs-custom-tab-width 4
  "Set the customized tab width"
  :type 'integer
  :group 'entropy/emacs-customize-group-for-editor-tab-width)

;; **** line hints
(defgroup entropy/emacs-customize-group-for-editor-line-hints nil
  "Eemacs ivy-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-editor-common)

(defcustom entropy/emacs-init-display-line-numbers-mode
  ;; now we can defaulty enable this in emacs 28 and upper since the
  ;; `line-number-at-pos' is C-binding in those emacs-version.
  (version< "28" emacs-version)
  "Enable `global-display-line-numbers-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-editor-line-hints)

(defcustom entropy/emacs-init-hl-line-mode t
  "Enable `global-hl-line-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-editor-line-hints)

(defcustom entropy/emacs-init-beacon-blink nil
  "Enable `beacon-blink' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-editor-line-hints)

(defcustom entropy/emacs-fill-paragraph-width 70
  "Setting fill-paragraph width, default 70 to follow the emacs
convention."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-editor-line-hints)

;; **** kill-ring

(defgroup entropy/emacs-customize-group-for-editor-kill-ring nil
  "Eemacs kill-ring configuration customizable group."
  :group 'entropy/emacs-customize-group-for-editor-common)

(defcustom entropy/emacs-kill-ring-persist-file
  (expand-file-name "eemacs-kill-ring-persist/kill-ring.persist" entropy/emacs-stuffs-topdir)
  "Persist cache file for storing `kill-ring'."
  :type 'file
  :group 'entropy/emacs-customize-group-for-editor-kill-ring)

;; **** comments

(defgroup entropy/emacs-customize-group-for-editor-comments nil
  "Eemacs editor comments configuration customizable group."
  :group 'entropy/emacs-customize-group-for-editor-common)

(defcustom entropy/emacs-custom-comment-dwim-type 'separedit
  "The comment-dwim type chosen, valid of `poporg' or
`separedit' as default.

NOTE: poporg is obsolete as an legacy option."
  :type 'symbol
  :group 'entropy/emacs-customize-group-for-editor-comments)

(defcustom entropy/emacs-custom-comment-dwim-prefix "C-c \""
  "The comment-dwim trigger keybind"
  :type 'string
  :group 'entropy/emacs-customize-group-for-editor-comments)

;; *** Emacs Extension
(defgroup entropy/emacs-customize-group-for-emacs-extensions nil
  "Eemacs emacs extensions management configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-package-archive-repo 'melpa
  "Set package archives from which to fetch.

  It can be a symbol to indicate the archive repo name or a list
  obey the form as the same as `package-archives' does.

  If its a repo name, valid values are:

  - 'melpa'       : Using melpa and elpa official archive remote host.

  - 'emacs-china' : Using melpa and elpa mirror archive remote host
                    of 'https://elpa.emacs-china.org/'.

  - 'tuna'        : Using melpa and elpa mirror archive remote host
                    of 'https://mirrors.tuna.tsinghua.edu.cn/help/elpa/'.

  - 'tencent'     : Using melpa and elpa mirror archive remote host
                    of 'https://mirrors.cloud.tencent.com/help/elpa.html'."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)
          (const :tag "Tencent" tencent))
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; **** elpkg get type
(defcustom entropy/emacs-ext-elpkg-customized-get-type 'origin
  "Init emacs with elisp extensions from entropy-emacs-extensions or
elpa and melpa.

Available value are 'entropy-emacs-extenisons-project' and 'origin'.

Type of 'entropy-emacs-extenisons-project' indicates to use
=entropy-emacs-extensions= (see
`entropy/emacs-ext-eemacs-elpkg-eemacs-ext-project-local-path' for its brief
introduction) to initialize the elisp extensions, which explicitly
means to use that as a local elisp packges archive host like what
'elpa' and 'melpa' did as an extenison ecosystem. This is the most
recommended type for using eemacs, because eemacs are built with
version controlled third-parties for be as stable , but you can
also use original emacs `package.el' to download most of
=entropy-emacs= required elisp packages by set the type of
'origin' without any bug warranty because newest packages are
without testing with eemacs maintainer.

If you persist using 'origin' type, the elpa and melpa mirror host
retriever will obtained the url abided by
`entropy/emacs-package-archive-repo'.
"
  :type '(choice
          (const :tag "Use melpa and elpa" origin)
          (const :tag "Use eemacs-ext local package system"
                 entropy-emacs-extenisons-project))
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; NOTE:
;; We must strictly did judgement for packages refer setting to
;; guarantee the package initialization procedure did correctly before
;; any eemacs specification loading.
(unless (member entropy/emacs-ext-elpkg-customized-get-type
                '(origin entropy-emacs-extenisons-project))
  (error "Invalid value for `entropy/emacs-ext-elpkg-customized-get-type': %s"
         entropy/emacs-ext-elpkg-customized-get-type))

(defcustom entropy/emacs-ext-eemacs-elpkg-eemacs-ext-project-local-path
  (expand-file-name
   "entropy-emacs-extensions"
   __eemacs-ext-union-host)
  "entropy-emacs extensions collection archive location. This
collection used to retrieving all entropy-emacs elpa or melpa
extensions' repos as submodules archived as one single project
used for version controlling. You can get it from
'https://github.com/c0001/entropy-emacs-extensions'.

This archive used when type of 'entropy-emacs-extenisons-project' is set to
customized variable `entropy/emacs-ext-elpkg-get-type'."

  :type 'directory
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

(defvar entropy/emacs-ext-elpkg-get-type
  entropy/emacs-ext-elpkg-customized-get-type
  "The internal variant of variable
`entropy/emacs-ext-elpkg-customized-get-type' which extended its
value type according to non-defined internal definition")

(defconst entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
  "v3.0.2"
  "")

(defconst entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path
  (expand-file-name
   (format ".eemacs-ext-build_%s"
           entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version)
   entropy/emacs-user-emacs-directory)
  "")

(defconst entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path-comprehensive-indicator
  (expand-file-name
   "init"
   entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)
  "")

(defconst entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-get-url
  (format
  "https://github.com/c0001/entropy-emacs-extensions/releases/\
download/%s/entropy-emacs-extensions_build_%s.tar.xz"
  entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version
  entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-version)
  "")

(defconst entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-archive-sha256sum
  "f5c9c48a70388dad091d571fcc9f168c8fa5f2b5fd79790cbb05e7500e9c12e3"
  "")

;; using `entropy-emacs-extensions-project-build' prefer when detected
;; its archive while using `entropy-emacs-extensions-project' project.
(when (and
       (eq entropy/emacs-ext-elpkg-get-type
           'entropy-emacs-extenisons-project)
       (file-exists-p
        entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path-comprehensive-indicator))

  (setq entropy/emacs-ext-elpkg-get-type
        'entropy-emacs-extensions-project-build)
  (let ((archive-fmt `(lambda (name)
                        (expand-file-name
                         name
                         ,entropy/emacs-ext-elpkg-eemacs-ext-stable-build-repo-local-path)))
        )
    (setq package-archives
          `(("entropy-melpa"      . ,(funcall archive-fmt "melpa"))
            ("entropy-elpa"       . ,(funcall archive-fmt "elpa"))
            ;; NOTE: disable elpa devel channel to avoid retrieving package updated with new emacs version.
            ;; ("entropy-elpa-devel" . ,(funcall archive-fmt "elpa-devel"))
            ))))

;; **** elpkg install location
(defcustom entropy/emacs-ext-emacs-pkgel-get-pkgs-root
  (expand-file-name
   "entropy-emacs-extensions-elpa"
   __eemacs-ext-union-host)
  "entropy-emacs elpa extensions directory for hosting the
upstream installed packages of `package.el'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; **** other extensions
;; ***** eemacs-lsp-archive project archive location

(defcustom entropy/emacs-ext-use-eemacs-lsparc nil
  "Whether to use archived lanuguage servers."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

(defcustom entropy/emacs-ext-lsp-archive-dir
  (expand-file-name
   "entropy-emacs-lsp-archive"
   __eemacs-ext-union-host)
  "entropy emacs language server archives project location, you
may download from
'https://github.com/c0001/entropy-emacs-lsp-archive'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-emacs-extensions)



;; ***** eemacs fonts

(defvar entropy/emacs-ext-eemacs-fonts-archive-url
  "https://sourceforge.net/projects/entropy-emacs-cabinet/files/Font/entropy-emacs-fonts/v1.0.1/eemacs-fonts_v1.0.1.tar.xz/download"
  )
(defvar entropy/emacs-ext-eemacs-fonts-archive-sha256sum
  "0c95ab8acc67e5e98755eda5ea2b1ccefcbcc4a0fa876c07d0c599effd47f028")

;; **** extra customized load path

(defcustom entropy/emacs-ext-user-specific-load-paths nil
  "Extra load path list for user specification.

This feature usually used for emacs new feature adding test and
designation."
  :type '(repeat directory)
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; *** Web Corresponding Config
(defgroup entropy/emacs-customize-group-for-web-refer nil
  "Eemacs web referred configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** eww search engine set
(defcustom entropy/emacs-enable-eww-search-engine-customize t
  "Enable eww search prefix customized"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-web-refer)

(defcustom entropy/emacs-eww-search-engine "https://www.bing.com/search?q="
  "Customized eww search prefix string and the default was 'bing.cn'"
  :type 'string
  :group 'entropy/emacs-customize-group-for-web-refer)

;; **** search-web search-engines setting
(defcustom entropy/emacs-search-web-engines-internal
  '(("php-manual" "http://php.net/manual-lookup.php?pattern=%s" In-Emacs)
    ("MOZILA MDN" "http://developer.mozilla.org/en-US/search?q=%s" In-Emacs)
    ("baidu" "http://www.baidu.com/s?wd=%s" In-Emacs)
    ("google" "http://www.google.com/search?q=%s" In-Emacs)
    ("google ja" "http://www.google.com/search?hl=ja&q=%s&ie=utf-8&oe=utf-8&gbv=1" In-Emacs)
    ("google en" "http://www.google.com/search?hl=en&q=%s&ie=utf-8&oe=utf-8&gbv=1" In-Emacs)
    ("stackoveflow en" "http://stackoverflow.com/search?q=%s" In-Emacs)
    ("stackoveflow ja" "http://ja.stackoverflow.com/search?q=%s" In-Emacs)
    ("elpa" "https://elpa.gnu.org/packages/%s.html" In-Emacs)
    ("melpa" "https://melpa.org/#/?q=%s" In-Emacs))
  "Internal search-web engines."
  :type '(repeat
          (list
           (string :tag "Host description")
           (string :tag "Host url")
           (const In-Emacs)))
  :group 'entropy/emacs-customize-group-for-web-refer)

(defcustom entropy/emacs-search-web-engines-external
  '(("MOZILA MDN" "https://developer.mozilla.org/en-US/search?q=%s" External)
    ("php-manual" "https://php.net/manual-lookup.php?pattern=%s" External)
    ("github-external" "https://github.com/search?&q=%s" External)
    ("baidu" "http://www.baidu.com/s?wd=%s" External)
    ("google" "https://www.google.com/search?q=%s" External)
    ("google ja" "https://www.google.com/search?hl=ja&q=%s&ie=utf-8&oe=utf-8&gbv=1" External)
    ("google en" "https://www.google.com/search?hl=en&q=%s&ie=utf-8&oe=utf-8&gbv=1" External)
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
  :type '(repeat
          (list
           (string :tag "Host description")
           (string :tag "Host url")
           (const External)))
  :group 'entropy/emacs-customize-group-for-web-refer)

;; **** personal browse-url function and varaiable
(defcustom entropy/emacs-enable-personal-browse-url-function nil
  "Enable personal browse url function like
`browse-url-browser-function' but embedded with eemacs context,
and then `entropy/emacs-browse-url-function' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-web-refer)

(defcustom entropy/emacs-browse-url-function nil
  "The specific `browse-url-browser-function' by user specification
for eemacs context, only used when
`entropy/emacs-enable-personal-browse-url-function' init with
non-nil value.

Mostly of all, you should write your browse-url function with
arguments list as '(url &rest args)'

For example:

     (defun entropy/emacs-open-with-url (url &rest args)
       (interactive (browse-url-interactive-arg \"URL: \"))
       (w32-shell-execute
        \"open\"
        \"a:/PortableApps/FirefoxPortable/FirefoxPortable.exe\"
        url))

In =entropy-emacs=, you should always preferred specified the url
browse function in this variable than the emacs internal defined
`browse-url-browser-function'. Eemacs arrange it to that in proper
way but with more extensively meaning."
  :type '(choice function (const nil))
  :group 'entropy/emacs-customize-group-for-web-refer)

;; **** web preview setting

(defcustom entropy/emacs-browse-url-function-for-web-preview t
  "The `browse-url-browser-function' used for web preview.

Valid values are:

- 'nil':                follow eemacs browse url function trigger
- 't':                  use `browse-url-default-browser'
- a function:             user spec function.

For eemacs developer, use
`entropy/emacs-browse-url-function-get-for-web-preview' to get the
actual `browse-url-browse-function' used for internal lisp context."
  :type '(choice
          function
          (const :tag "follow eemacs browser change trigger" t)
          (const :tag "using default browser" nil))
  :group 'entropy/emacs-customize-group-for-web-refer)

(defun entropy/emacs-browse-url-function-get-for-web-preview ()
  "Return a `browse-url-browser-function' according to
`entropy/emacs-browse-url-function-for-web-preview'."
  (let (_)
    (cond
     ((eq t entropy/emacs-browse-url-function-for-web-preview)
      'browse-url-default-browser)
     ((eq nil entropy/emacs-browse-url-function-for-web-preview)
      browse-url-browser-function)
     ((functionp entropy/emacs-browse-url-function-for-web-preview)
      entropy/emacs-browse-url-function-for-web-preview)
     (t
      (user-error "Invalid value of `entropy/emacs-browse-url-function-for-web-preview'"
                  entropy/emacs-browse-url-function-for-web-preview)))))

;; *** IME
(defgroup entropy/emacs-customize-group-for-IME nil
  "Eemacs input method configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** union defination

(defcustom entropy/emacs-internal-ime-use-backend nil
  "The eemacs emacs internal IME spec backend.

There's two valid choice:
- 'pyim': the old long time maintained chinese input method which
  provide most riched features for input chinese chars.

  It use plain text format of user dicts to handle the input event,
  and aslo exposed an simple librime elisp binding to handle the
  communication with system rime IME engine to enlarge its input
  experience. (also see `entropy/emacs-pyim-use-backend' to customize
  which pyim backend to use.)

- 'emacs-rime': the pure elisp binding for liberime to handle
  communication with system rime IME engine, its fast than 'pyim''s
  rime dealing since its c part of lisp binding for librime is more
  optimized than thus.

- 'nil': disable this feature.

NOTE: any rime category backend chosen requires emacs built with
'dynamic module' support and the system has installed the librime
depedencies."
  :type '(choice
          (const :tag "Use emac-rime" 'emacs-rime)
          (const :tag "Use pyim" 'pyim)
          (const :tag "disable" nil))
  :group 'entropy/emacs-customize-group-for-IME)

(defcustom entropy/emacs-internal-ime-toggling-kbd-key "C-\\"
  "The `kbd' recognied *single* key stroke for toggling the enable
  status of `entropy/emacs-internal-ime-use-backend'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-IME)

(defcustom entropy/emacs-internal-ime-use-rime-default-schema "luna_pinyin_simp"
  "The default rime schema used for init
`entropy/emacs-internal-ime-use-rime-as'. The specified rime schema
must has been installed in your system, or will with messy with
startup when `entropy/emacs-internal-ime-use-backend' is non-nil.

*Tricks*:

1. Use chinese simplified as default schema in rime conig

If your input schema was =luna_pyinyin=, try to create
'luna_pinyin.custom.yaml' in
`entropy/emacs-internal-ime-rime-user-data-host-path' with the content
to:

#+begin_example
patch:
  switches:                   # 注意縮進
    - name: ascii_mode
      reset: 0                # reset 0 的作用是當從其他輸入方案切換到本方案時，
      states: [ 中文, 西文 ]  # 重設爲指定的狀態，而不保留在前一個方案中設定的狀態。
    - name: full_shape        # 選擇輸入方案後通常需要立即輸入中文，故重設 ascii_mode = 0；
      states: [ 半角, 全角 ]  # 而全／半角則可沿用之前方案中的用法。
    - name: simplification
      reset: 1                # 增加這一行：默認啓用「繁→簡」轉換。
      states: [ 漢字, 汉字 ]
#+end_example

See [[https://github.com/rime/home/wiki/\
CustomizationGuide#\
%E4%B8%80%E4%BE%8B%E5%AE%9A\
%E8%A3%BD%E7%B0%A1%E5%8C%96\
%E5%AD%97%E8%BC%B8%E5%87%BA][the rime wiki]] for details.
"
  :type 'string
  :group 'entropy/emacs-customize-group-for-IME)

(defcustom entropy/emacs-internal-ime-use-rime-as 'emacs-rime
  "The 'librime' based emacs dynamic model backend choice.

Valid as:

- 'emacs-rime': the modern emacs librime binding. (default)
- 'emacs-liberime': the obsolete emacs librime binding.

Leave it as nil to disable this option.
"
  :type '(choice
           (const :tag "Use rime support by `emacs-rime' package" emacs-rime)
           (const :tag "Use rime support by `pyim' package" liberime)
           (const :tag "Disalble the rime support" nil))
  :group 'entropy/emacs-customize-group-for-IME)

(defcustom entropy/emacs-internal-ime-popup-type 'minibuffer
  "The emacs internal IME candidates show type:

- 'minibuffer': use emacs minibuffer echo area as the canididates exhaustion place
- 'popup'     : use overlay `popup' render emulate an display area to exhaust the candidates.
- 'posframe' : use child-frame powered by `posframe' to do so just
               when in GUI env and `emacs-version' larger than 25.
- 'nil'       : Automatically set.

NOTE: for eemacs maintainer, using
`entropy/emacs-internal-ime-popup-type-autoset' instead. Since we must
check this variable while init any referred procedure for type
suitability with the env."
  :type '(choice
          (const :tag "Posframe (in gui)" posframe)
          (const :tag "Popup (in tui)" popup)
          (const :tag "Minibuffer (either tui or gui)" minibuffer)
          (const :tag "Automatically set" nil))
  :group 'entropy/emacs-customize-group-for-IME)

(defvar __eemacs-internal-ime-popup-autoset-for-vars nil)
(defun entropy/emacs-internal-ime-popup-type-autoset ()
  "Automatically set `entropy/emacs-internal-ime-popup-type' as
eemacs internal procedure usage. (see
`entropy/emacs-internal-ime-popup-type' for details) to adapt
current emacs session env.

EEMACS_MAINTENANCE: Further more, this function will set the var
of `__eemacs-internal-ime-popup-autoset-for-vars' to the value
even. So the eemacs maitainer must add the new backend
corresponding var to thus."
  (or (and entropy/emacs-internal-ime-popup-type
           (cond
            ((eq entropy/emacs-internal-ime-popup-type 'posframe)
             (if (and (not (version< emacs-version "26.1"))
                      (display-graphic-p))
                 (setq entropy/emacs-internal-ime-popup-type 'posframe)
               (setq entropy/emacs-internal-ime-popup-type 'popup)))
            (t
             (if (and (eq entropy/emacs-internal-ime-popup-type 'posframe)
                      (not (display-graphic-p)))
                 (setq entropy/emacs-internal-ime-popup-type 'popup)
               (setq entropy/emacs-internal-ime-popup-type
                     entropy/emacs-internal-ime-popup-type)))))
      (cond ((and (not (version< emacs-version "26.1"))
                  (display-graphic-p))
             (setq entropy/emacs-internal-ime-popup-type 'posframe))
            (t
             (setq entropy/emacs-internal-ime-popup-type 'minibuffer))))
  (dolist (var __eemacs-internal-ime-popup-autoset-for-vars)
    (set var entropy/emacs-internal-ime-popup-type)))

(defcustom entropy/emacs-internal-ime-rime-system-share-data-host-path
  (if (member system-type
              '(gnu/linux gnu cygwin gnu/kfreebsd))
      "/usr/share/rime-data"
    nil)
  "The rime scheme-data directory using for `entropy/emacs-internal-ime-use-rime-as'.

Default to '/usr/share/rime-data'. There's no need to manually
set it in linux environment since the default value is for as but
manually set while WINDOWS or MASOS env.
"
  :type '(choice
          (const :tag "automatically set" nil)
          directory)
  :group 'entropy/emacs-customize-group-for-IME)

(defun entropy/emacs-internal-ime-rime-system-share-data-auto-set ()
  "Automatically set
`entropy/emacs-internal-ime-rime-system-share-data-host-path'
according to `system-type' or the environment variable like
'MSYSTEM_PREFIX' (msys2 host indicator), 'LIBRIME_ROOT' etc.

Throw an error while noting found when trying out all methods."
  (unless (and entropy/emacs-internal-ime-rime-system-share-data-host-path
               (file-exists-p
                entropy/emacs-internal-ime-rime-system-share-data-host-path))
    (setq entropy/emacs-internal-ime-rime-system-share-data-host-path
          (cl-case system-type
            ('gnu/linux
             (require 'xdg)
             (cl-some (lambda (parent)
                        (let ((dir (expand-file-name "rime-data" parent)))
                          (when (file-directory-p dir)
                            dir)))
                      (if (fboundp 'xdg-data-dirs)
                          (xdg-data-dirs)
                        '("/usr/local/share" "/usr/share"))))
            ('darwin
             "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
            ('windows-nt
             (if (getenv "MSYSTEM_PREFIX")
                 (concat (getenv "MSYSTEM_PREFIX") "/share/rime-data")
               (if (getenv "LIBRIME_ROOT")
                   (expand-file-name (concat (getenv "LIBRIME_ROOT") "/share/rime-data")))))))
    (unless (and entropy/emacs-internal-ime-rime-system-share-data-host-path
                 (file-exists-p
                  entropy/emacs-internal-ime-rime-system-share-data-host-path))
      (user-error "We can not found the valid path for\
`entropy/emacs-internal-ime-rime-system-share-data-host-path'"))))

(defcustom entropy/emacs-internal-ime-rime-user-data-host-path
  (expand-file-name "rime/rime-user-data-dir" entropy/emacs-stuffs-topdir)
  "The use data dir for 'librime' binding of
`entropy/emacs-internal-ime-use-rime-as'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-pyim)


;; **** pyim config
(defgroup entropy/emacs-customize-group-for-pyim nil
  "Eemacs PYIM configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IME)

(defvar entropy/emacs-enable-pyim (eq entropy/emacs-internal-ime-use-backend 'pyim)
  "Enable pyim be the default pyin input method")

(defcustom entropy/emacs-pyim-use-backend
  (if (eq entropy/emacs-internal-ime-use-rime-as 'emacs-liberime)
      'liberime
    'internal)
  "The pyim backend type choosing configuration."
  :type  '(choice
           (const :tag "Native pyim backend" internal)
           (const :tag "Liberime based on librime" liberime))
  :group 'entropy/emacs-customize-group-for-pyim)

(defvar entropy/emacs-pyim-liberime-scheme-data
  entropy/emacs-internal-ime-rime-system-share-data-host-path
  "The rime scheme-data directory using for liberime")

(defvar entropy/emacs-pyim-liberime-cache-dir
  entropy/emacs-internal-ime-rime-user-data-host-path
  "The cache dir for liberime")

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
  :type '(choice
          (const nil)
          (repeat
           (list
            (const :name)
            (string :tag "Pyim dictionary name")
            (const :file)
            (file :tag "Pyim dictionary file"))))
  :group 'entropy/emacs-customize-group-for-pyim)

(defcustom entropy/emacs-pyim-dcache-host-path
  (expand-file-name "pyim/internal-cache" entropy/emacs-stuffs-topdir)
  "Set pyim dcache dir, same as `pyim-dcache-directory' but used
with eemacs customization procedure."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-pyim)

(defvar entropy/emacs-pyim-tooltip entropy/emacs-internal-ime-popup-type
  "Setting the pyim toolitip method, same as `pyim-page-tooltip'
but used with eemacs customization procedure.")
(add-to-list '__eemacs-internal-ime-popup-autoset-for-vars
             'entropy/emacs-pyim-tooltip)


;; **** emacs-rime config
(defgroup entropy/emacs-customize-group-for-emacs-rime nil
  "Eemacs `emacs-rime' configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IME)

(defvar entropy/emacs-enable-emacs-rime (eq entropy/emacs-internal-ime-use-backend 'emacs-rime)
  "Enable `emacs-rime' be the default pyin input method")

(defvar entropy/emacs-emacs-rime-liberime-scheme-data
  entropy/emacs-internal-ime-rime-system-share-data-host-path
  "The rime scheme-data directory using for `emacs-rime'")

(defvar entropy/emacs-emacs-rime-cache-dir
  entropy/emacs-internal-ime-rime-user-data-host-path
  "The user cache dir for `emacs-rime'")

(defvar entropy/emacs-emacs-rime-tooltip entropy/emacs-internal-ime-popup-type
  "Setting the `emacs-rime' toolitip method")
(add-to-list '__eemacs-internal-ime-popup-autoset-for-vars
             'entropy/emacs-emacs-rime-tooltip)

;; **** _union setting init

;; Fallback to default when multi backend enabled
;; TODO: more intelligence such as detect rime support
(cond ((and entropy/emacs-enable-pyim
            entropy/emacs-enable-emacs-rime)
       (warn "Do not enable `entropy/emacs-enable-emacs-rime' and \
`entropy/emacs-enable-pyim' both at same time, we are help you \
choose `entropy/emacs-enable-emacs-rime' as current chosen.")
       (set entropy/emacs-enable-pyim nil))
      (t
       ;; TODO for the default condition
       t))

;; auto set popup type since the user setting messy
(unless entropy/emacs-internal-ime-popup-type
  (entropy/emacs-internal-ime-popup-type-autoset))

;; auto set rime system schema data dir since user setting messy
(when (or entropy/emacs-enable-emacs-rime
          (eq entropy/emacs-pyim-use-backend 'liberime))
  (entropy/emacs-internal-ime-rime-system-share-data-auto-set)
  (setq entropy/emacs-emacs-rime-liberime-scheme-data
        entropy/emacs-internal-ime-rime-system-share-data-host-path
        entropy/emacs-pyim-liberime-scheme-data
        entropy/emacs-internal-ime-rime-system-share-data-host-path))

;; *** Project
(defgroup entropy/emacs-customize-group-for-project-management nil
  "Eemacs project-management configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** ibuffer
(defcustom entropy/emacs-enable-ibuffer-projectitle nil
  "Enable ibuffer-projectitle in ibuffer

Note: ibuffer-projectitle will cause the performance debug.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-project-management)

;; **** fuzzy search

(defgroup entropy/emacs-customize-group-for-fuzzy-search nil
  "Eemacs fuzzy search configuration customizable group."
  :group 'entropy/emacs-customize-group-for-project-management)

(defcustom entropy/emacs-search-program "rg"
  "The project search engine used for =entropy-emacs=.

The valid value are \"ag\" (i.e. the silver_searcher) or \"rg\"
(i.e. the ripgrep searcher), default for using ripgrep which is
the fasest way.
"
  :type '(choice
          (const :tag "Ripgrep" "rg")
          (const :tag "The silver searcher" "ag"))
  :group 'entropy/emacs-customize-group-for-fuzzy-search)

;; *** Workspace
(defgroup entropy/emacs-customize-group-for-workspace nil
  "Eemacs worskpace configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** eyebrowse config
(defgroup entropy/emacs-customize-group-for-eyebrowse-mode nil
  "Eemacs eyebrowse-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-workspace)

(defcustom entropy/emacs-enable-eyebrowse-new-workspace-init-function nil
  "Enable personal function for eyebrowse to creating new
workspace."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-eyebrowse-mode)

(defcustom entropy/emacs-eyebrowse-new-workspace-init-function
  'entropy/emacs-create-scratch-buffer
  "Create the init buffer or others with you own function when
open one new eyebrowse workspace"
  :type 'function
  :group 'entropy/emacs-customize-group-for-eyebrowse-mode)

;; **** desktop save config
(defgroup entropy/emacs-customize-group-for-desktop-save nil
  "Eemacs `desktop-save' configuration customizable group."
  :group 'entropy/emacs-customize-group-for-workspace)

(defcustom entropy/emacs-desktop-enable nil
  "Enable desktop-save-mode and persistent scratch buffer"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-desktop-save)

;; *** Shell Env Config

(defgroup entropy/emacs-customize-group-for-ShellEnv-configuration nil
  "Eemacs shell env configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-willing-copy-shell-variables nil
  "Shell ENV variables willing to inject to current emacs session.

This list of strings used for `setenv' to copy you default shell
environment variables.

The default shell is obtained from `shell-file-name' if setted
yet, at leasst be from you SHELL env var of current emacs session
as fallback.

The env vars retrieving method made by logining into your default
shell across a swan process under current emacs session."
  :type '(repeat string)
  :group 'entropy/emacs-customize-group-for-ShellEnv-configuration)

;; **** Language Envrionment
(defgroup entropy/emacs-customize-group-for-language-environment nil
  "Eemacs language environment configuration customizable group."
  :group 'entropy/emacs-customize-group-for-ShellEnv-configuration)

(defcustom entropy/emacs-custom-language-environment-enable nil
  "Enable custome language environment, used to customize
`current-language-environment', and then variable
`entropy/emacs-locale-language-environment' and
`entropy/emacs-locale-coding-system' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-language-environment)

(defcustom entropy/emacs-locale-language-environment
  (copy-tree current-language-environment)
  "The language environment follow current system locale, used to
`set-language-environment', instantly getted before eemacs core
procedure invoked.

In most of case, you do not need to customize it, but it's
allowed, if so as, the value must be one key of
`language-info-alist'.

NOTE: this variable just be used when init
`entropy/emacs-custom-language-environment-enable' with non-nil
value."
  :type 'string
  :group 'entropy/emacs-customize-group-for-language-environment)

(defcustom entropy/emacs-locale-coding-system
  (car default-process-coding-system)
  "The preferred coding system follow current system locale, used
to `prefer-coding-system', instantly getted before eemacs core
procedure invoked.

In most of case, you do not need to customize it, but it's
allowed, if so as, the value must be one of
`coding-system-list'.

NOTE: this variable just be used when init
`entropy/emacs-custom-language-environment-enable' with non-nil
value."
  :type 'coding-system
  :group 'entropy/emacs-customize-group-for-language-environment)



;; **** Internet Net Proxy
(defgroup entropy/emacs-customize-group-for-internet-proxy nil
  "Eemacs internet proxy configuration customizable group."
  :group 'entropy/emacs-customize-group-for-ShellEnv-configuration)

(defcustom entropy/emacs-union-proxy-noproxy-list
  '("127.0.0.1" "localhost")
  "No_proxy list via using `entropy/emacs-union-http-proxy-plist'
and `entropy/emacs-union-socks-proxy-plist'.

Each elements of the list of no proxy is a ip address string or an
list of range descriptions supported by
`entropy/emacs-generate-symbols-or-strings-from-range-desc'."
  :type '(repeat (choice (string :tag "Ip address for no proxy")
                         (function :tag "A function to generate a list of ip address for no proxy")))
  :group 'entropy/emacs-customize-group-for-internet-proxy)

(defcustom entropy/emacs-union-http-proxy-plist
  '(:enable
    nil
    :host "127.0.0.1" :port 7890)
  "The plist for indicate eemacs union http proxy for internet connection.

There're three valid key slots:

- ':enable'    : whether to enable eemacs union http proxy setup
- ':host'      : the proxy host address
- ':port'      : the port number of the proxy host
"
  :type '(plist :options
                (((const :tag "Enable" :enable) boolean)
                 ((const :tag "Host Address" :host) string)
                 ((const :tag "Host Port" :port) integer)
                 ))
  :group 'entropy/emacs-customize-group-for-internet-proxy)


(defcustom entropy/emacs-union-socks-proxy-plist
  '(:enable
    nil
    :host "127.0.0.1" :port 7890)
  "The plist for indicate eemacs union socks proxy for internet connection.

There're three valid key slots:

- ':enable'    : whether to enable eemacs union socks proxy setup
- ':host'      : the proxy host address
- ':port'      : the port number of the proxy host
- ':socks-veresion' : socks version specification (i.e. 4/5)
"
  :type '(plist :options
                (((const :tag "Enable" :enable) boolean)
                 ((const :tag "Host Address" :host) string)
                 ((const :tag "Host Port" :port) integer)
                 ((const :tag "Socks Version" :socks-version) integer)))
  :group 'entropy/emacs-customize-group-for-internet-proxy)

;; *** Coworkers
(defgroup entropy/emacs-customize-group-for-coworkers nil
  "Eemacs coworkers integrated configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-install-coworker-immediately nil
  "Install coworker immediatly in needed while.

Also see `entropy/emacs-coworker-host-root'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-coworkers)

(defcustom entropy/emacs-coworker-host-root
  (expand-file-name "entropy-emacs-local" __eemacs-ext-union-host)
  "The coworker host root dir.

Coworker host dir is a eemacs specified folder to host any system
wide dependencies independent from the native system environment,
it works like a dot-local (~/.local) foler per-user did, but just
used for =entropy-emacs=.

For eemacs maintenance, all the sub-folders like bin/, lib/ so on
are pre-defined for preserving the namespace update possible:
- bin/: `entropy/emacs-coworker-bin-host-path'
- lib/: `entropy/emacs-coworker-lib-host-root'
- archive/: `entropy/emacs-coworker-archive-host-root'.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-coworkers)

;; *** IDE
(defgroup entropy/emacs-customize-group-for-IDE-configuration nil
  "Eemacs IDE integrated configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** common config
(defcustom entropy/emacs-ide-suppressed t
  "Inhibit =entropy-emacs= IDE configurations so that just
make emacs simplify for coding."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defcustom entropy/emacs-ide-use-for-all 'lsp
  "Enable =entropy-emacs= IDE configuration for all major-mode
defined in `entropy/emacs-ide-for-them' with a same type.

Valid value are:

- 'lsp'         : use microsoft language server for all of them
- 'eglot'       : use simple LSP client involved from gnu-elpa official suggestion
- 'traditional' : use traditional way for all of them

Although you've set this, you can still set them individually by
each specific name of them, this just a common initializing
option."
  :type '(choice
          (const :tag "microsoft language server protocol" lsp)
          (const :tag "Simple official LSP client" eglot)
          (const :tag "Individual language servers (traditional way)" traditional))
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defvar entropy/emacs-ide-for-them
  '(c-mode java-mode
    c++-mode cmake-mode
    go-mode
    rust-mode
    python-mode php-mode
    js2-mode json-mode css-mode web-mode nxml-mode
    sh-mode powershell-mode))

(defvar entropy/emacs-use-ide-conditions nil)
(defun entropy/emacs-get-ide-condition-symbol (major-mode)
  (intern (format "entropy/emacs-use-ide-type-for-%s" major-mode)))
(defun entropy/emacs-get-use-ide-type (major-mode)
  (unless entropy/emacs-ide-suppressed
    (symbol-value
     (entropy/emacs-get-ide-condition-symbol major-mode))))

(defun entropy/emacs-ide-gen-customized-variables ()
  (let (forms)
    (dolist (el entropy/emacs-ide-for-them)
      (let ((sym (entropy/emacs-get-ide-condition-symbol el)))
        (push sym
              entropy/emacs-use-ide-conditions)
        (push
         `(defcustom ,sym ',entropy/emacs-ide-use-for-all
            ,(format "The IDE chosen type for major-mode '%s'

Valid type are 'traditional' or 'lsp' which default to use lsp.
"
                     el)
            :type '(choice
                    (const :tag "microsoft language server protocol" lsp)
                    (const :tag "Individual language servers (traditional way)" traditional))
            :group 'entropy/emacs-customize-group-for-IDE-configuration)
         forms)))
    (dolist (form forms)
      (eval form))))

(entropy/emacs-ide-gen-customized-variables)

(defcustom entropy/emacs-ide-doc-delay 0.5
  "Ide system doc helper show idle delay

NOTE: large idle delay may help to reduce emacs pressure for
emacs performance."
  :type 'float
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defcustom entropy/emacs-ide-diagnostic-delay 0.5
  "Ide system parseer response idle delay

NOTE: large idle delay may help to reduce emacs pressure for
emacs performance."
  :type 'float
  :group 'entropy/emacs-customize-group-for-IDE-configuration)


;; **** language server specification
(defgroup entropy/emacs-customize-group-for-codeserver nil
  "Eemacs language servers integraged configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defcustom entropy/emacs-codeserver-prefer-pyls-type 'pyright
  "Choose preferred python language server type:

- 'mspyls':  microsoft python lanuage server
- 'pyls':    official python language server
- 'pyright': next generation vscode python language server (used as default)
"
  :type '(choice
          (const :tag "pyls" pyls)
          (const :tag "lsp-ms-pyls" mspyls)
          (const :tag "lsp-pyright" pyright))
  :group 'entropy/emacs-customize-group-for-codeserver)


;; **** code folding group
(defgroup entropy/emacs-customize-group-for-code-folding nil
  "Eemacs context fold/expand configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IDE-configuration
  :prefix "entropy/emacs-code-folding-")

(defcustom entropy/emacs-code-folding-type 'yafolding
  "Type for code folding style embeded in entropy/emacs."
  :type '(choice
          (const native)
          (const yafolding))
  :group 'entropy/emacs-customize-group-for-code-folding)

;; **** yasnippet config
(defcustom entropy/emacs-yas-dir
  (expand-file-name "snippets" entropy/emacs-stuffs-topdir)
  "Set the default personal snippet dir"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(add-hook 'entropy/emacs-startup-end-hook
          ;; Pre checking the exists status of thus for preventing yas
          ;; error prompt from yas loading case.
          #'(lambda ()
              (unless (file-directory-p entropy/emacs-yas-dir)
                (condition-case error
                    (mkdir entropy/emacs-yas-dir t)
                  (error
                   (warn
                    (format
                     "Can not creat non-exist `entropy/emacs-yas-dir' error of %s"
                     error)))))))

;; **** completion framework

(defgroup entropy/emacs-customize-group-for-auto-completion nil
  "Eemacs auto-completion framework configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defcustom entropy/emacs-auto-completion-use-backend-as 'company
  "The eemacs auto-completion framework backend type.

Valid options are:

- 'company': use `company-mode'."
  :type '(choice
          (const :tag "use `company-mode'" company))
  :group 'entropy/emacs-customize-group-for-auto-completion)

;; ***** company mode config
(defgroup entropy/emacs-customize-group-for-company-mode nil
  "Eemacs company-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-auto-completion)

(defcustom entropy/emacs-company-tooltip-use-type 'company-box
  "The tooltip kits used type for `company-mode'.

Company has the built-in popup display tooltip kit, which reflect
to this variable of the value 'default'. Further more with the
rich enhanced purpose there are one another option:

- 'company-box' : the show type of more riched mordern
  visualization, commonly sense of icons prefixed for each
  canidates."
  :type '(choice
          (const :tag "Company-mode native way" default)
          (const :tag "Rich-hence (company-box)" company-box))
  :group 'entropy/emacs-customize-group-for-company-mode)

(defcustom entropy/emacs-company-idle-delay-default 0.45
  "Default eemacs specified set for `company-idle-delay`.

NOTE: must set larger than `entropy/emacs-safe-idle-minimal-secs'."
  :type 'number
  :group 'entropy/emacs-customize-group-for-company-mode)

(defcustom entropy/emacs-company-quickhelp-delay-default nil
  "Default unified company help-doc popup delay seconds used for
all type of `entropy/emacs-company-tooltip-use-type', nil for
disable help doc auto-show."
  :type 'number
  :group 'entropy/emacs-customize-group-for-company-mode)


(defcustom entropy/emacs-company-delete-char-on-the-fly-duration
  70000
  "The flying on `delete-char' duration while company is actived
on in which case `company-candidates' is non-nil.

This integer is used to auto close company daemon so that flying
on hits will not cause emacs lagging on."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-company-mode)

;; **** use highlight feature
(defgroup entropy/emacs-customize-group-for-highlight-features nil
  "Eemacs buffer highlight features configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

;; ***** main enable
(defcustom entropy/emacs-use-highlight-features t
  "Enable highlight feature package `init-highlight.el'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** highlight-indention
(defcustom entropy/emacs-hl-highlight-indention-enable-at-startup nil
  "Enable indention highlight feature"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** symbol-over-lay
(defcustom entropy/emacs-hl-sysmbol-overlay-enable-at-startup nil
  "Enable symbol-overlay highlight feature"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** highlight-parentheses
(defcustom entropy/emacs-hl-highlight-parentheses-mode-enable-at-startup nil
  "Enable highlight-parentheses highlight feature"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** rainbow delimiters
(defcustom entropy/emacs-hl-rainbow-delimiters-enable-at-startup nil
  "Enable rainbow-delimiters highlight feature"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** hl-todo-mode
(defcustom entropy/emacs-hl-todo-enable-at-startup t
  "Enable hl-todo highlight feature which can show color of
  `TODO' keywords universal not only the org file"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** diff-hl
(defcustom entropy/emacs-hl-diff-hl-enable-at-startup nil
  "Enable diff-hl highlight feature which can show the git diff
in current buffer"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; ***** whitespace
(defcustom entropy/emacs-hl-whitespace-enable-at-startup nil
  "Enable whitespace highlight feature"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

(defcustom entropy/emacs-hl-whitespace-auto-cleanup nil
  "Auto clean up messy or redundant whitespace when in `whitespace-mode'

Notice: If enable this will cause git diff log be messy with long
difference.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-highlight-features)

;; *** Eshell
(defgroup entropy/emacs-customize-group-for-eshell nil
  "Eemacs eshell configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-eshell-alias-file "~/.eshell-alias"
  "entropy-emacs eshell alias file location."
  :type 'file
  :group 'entropy/emacs-customize-group-for-eshell)

(defcustom entropy/emacs-eshell-history-file "~/.eshell-history"
  "entropy-emacs eshell history file location."
  :type 'file
  :group 'entropy/emacs-customize-group-for-eshell)

;; *** Rss
(defgroup entropy/emacs-customize-group-for-RSS nil
  "Eemacs Rss reader configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** Elfeed
(defgroup entropy/emacs-customize-group-for-elfeed nil
  "Eemacs elfeed configuration customizable group."
  :group 'entropy/emacs-customize-group-for-RSS)

(defcustom entropy/emacs-elfeed-feeds
  '(
    ;; emacs information
    ("https://emacs-china.org/posts.rss" :name "emacs china posts" :use-proxy nil)
    ("https://emacs-china.org/latest.rss" :name "emacs china latest" :use-proxy nil)
    ("https://manateelazycat.github.io/feed.xml" :name "懒猫。。。" :use-proxy nil)
    ;; english
    ("https://www.nasa.gov/rss/dyn/nasax_vodcast.rss" :use-proxy t)
    ("https://www.nasa.gov/rss/dyn/image_of_the_day.rss" :use-proxy t)
    ("http://feeds.bbci.co.uk/news/england/rss.xml" :use-proxy t)
    ("http://feeds.bbci.co.uk/news/world/europe/rss.xml" :use-proxy t)
    ;; chinese
    ("https://www.douban.com/feed/review/book" :use-proxy nil)
    ("https://www.ithome.com/rss/" :use-proxy nil)
    ("http://rss.zol.com.cn/news.xml" :use-proxy nil)
    ("https://feedx.net/rss/thepaper.xml" :use-proxy nil)
    ("http://www.williamlong.info/blog/rss.xml" :use-proxy nil)
    )
  "Like `elfeed-feeds' but with further more flexible setting for each feed.

Formed as '((feed-url0 :name xx :use-proxy t)
            (feed-url1 :name yy :use-proxy nil))

When key USE-PROXY is non-nil, we use
`entropy/emacs-union-http-proxy-plist' as the proxy sourc."
  :type '(repeat
          (cons (string :tag "Feed Url")
                (plist :options
                       (((const :tag "Feed Name" :url) string)
                        ((const :tag "Use Proxy" :use-proxy) boolean)))))
  :group 'entropy/emacs-customize-group-for-elfeed)

;; **** Gnus
(defgroup entropy/emacs-customize-group-for-gnus nil
  "Eemacs GNUS configuration customizable group."
  :group 'entropy/emacs-customize-group-for-RSS)

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
  :type '(list
          (const :gnus-home) directory
          (const :gnus-news-dir) directory
          (const :mail-dir) directory
          (const :mail-temp-dir) directory
          (const :init-file) file
          (const :startup-file) file
          (const :read-newsrc) boolean
          (const :save-newsrc) boolean
          (const :use-dribble) boolean
          (const :read-active-file) boolean)
  :group 'entropy/emacs-customize-group-for-gnus)

;; *** Major-modes
(defgroup entropy/emacs-customize-group-for-major-modes nil
  "Eemacs major modes configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** dired mode
(defgroup entropy/emacs-customize-group-for-dired-mode nil
  "Eemacs dired-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-major-modes)

;; ***** dired trash enable
(defcustom entropy/emacs-dired-enable-trash nil
  "Enable trash function when using `dired-delete-file' in
`dired-mode'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-dired-mode)

;; **** org mode config
(defgroup entropy/emacs-customize-group-for-org-mode nil
  "Eemacs org-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-major-modes)

;; ***** org-bullet
(defcustom entropy/emacs-enable-org-bullets t
  "Enable org bullets"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-org-mode)

(defcustom entropy/emacs-org-bullets-type "roman"
  "Choose org bullets type."
  :type '(choice
          (const :tag "Roman icon" "roman")
          (const :tag "Origin icon" "origin"))
  :group 'entropy/emacs-customize-group-for-org-mode)

;; ***** windows org-download temp path
(defcustom entropy/emacs-win-org-download-file-name "c:\\temp\\screenshot.png"
  "Setting file name with it's path for screenshot in windows"
  :type 'file
  :group 'entropy/emacs-customize-group-for-org-mode)

;; ***** org header scale
(defcustom entropy/emacs-disable-org-heading-scale t
  "Diable org heading auto-scale face feature."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-org-mode)

;; **** markdown mode
(defgroup entropy/emacs-customize-group-for-markdown-mode nil
  "Eemacs markdown-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-major-modes)

(defcustom entropy/emacs-markdown-exp-header-context-type
  "application/xhtml+xml"
  "Content type string for the http-equiv header in XHTML output.
When set to an empty string, this attribute is omitted.  Defaults to
'text/html'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-markdown-mode)

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
  :group 'entropy/emacs-customize-group-for-markdown-mode)

(defcustom entropy/emacs-markdown-exp-css-paths
  '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
    "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
  "URL of CSS file to link to in the output XHTML."
  :type '(repeat (string :tag "Css file host uri"))
  :group 'entropy/emacs-customize-group-for-markdown-mode)

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
  :type '(repeat (string :tag "Css file host uri or content"))
  :group 'entropy/emacs-customize-group-for-markdown-mode)

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
  :type '(repeat (string :tag "Js script host uri or content"))
  :group 'entropy/emacs-customize-group-for-markdown-mode)

;; **** common lisp mode
(defgroup entropy/emacs-customize-group-for-common-lisp-mode nil
  "Eemacs common lisp configuration customizable group."
  :group 'entropy/emacs-customize-group-for-major-modes)

(defcustom entropy/emacs-inferior-lisp-program "sbcl"
  "Eemacs specified lisp program value redirected to
`inferrior-lisp-program'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-common-lisp-mode)

(defcustom entropy/emacs-slime-lisp-implementations
  '((sbcl ("sbcl") :coding-system utf-8-unix))
  "Eemacs specified `slime-lisp-implementations' value."
  :type 'sexp
  :group 'entropy/emacs-customize-group-for-common-lisp-mode)

;; *** Entertainments
(defgroup entropy/emacs-customize-group-for-entertaiments nil
  "Eemacs general entertainments configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** media player

(defgroup entropy/emacs-customize-group-for-media-player nil
  "Eemacs general media player configuration customizable group."
  :group 'entropy/emacs-customize-group-for-entertaiments)

;; ***** music

(defgroup entropy/emacs-customize-group-for-media-music nil
  "Eemacs general music configuration customizable group."
  :group 'entropy/emacs-customize-group-for-media-player)

;; ****** mpc
(defgroup entropy/emacs-customize-group-for-mpd nil
  "Eemacs MPD (music player daemon) configuration customizable group."
  :group 'entropy/emacs-customize-group-for-media-music)

(defcustom entropy/emacs-mpd-host-url "127.0.0.1"
  "Mpd host url defautl to 'localhost'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-mpd)

(defcustom entropy/emacs-mpd-host-port "9688"
  "Mpd host port defautl to '9688'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-mpd)

;; *** Translate
(defgroup entropy/emacs-customize-group-for-translate nil
  "Eemacs elfeed configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-dictionary-backend 'sdcv
  "The dictionary tool type for chosen, valid type are

- 'sdcv'
- 'bing'
- 'youdao'
- 'google'
"
  :type '(choice
          (const :tag "Sdcv local dictional" sdcv)
          (const :tag "Bing online dictionary" bing)
          (const :tag "Youdao online dictionary" youdao)
          (const :tag "Google online dictionary" google))
  :group 'entropy/emacs-customize-group-for-translate)

(defcustom entropy/emacs-google-translate-toggle-patched-in-china t
  "Enable google-translate in china. (GFW banned restriction break)"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-translate)

;; *** Platform Spec
(defgroup entropy/emacs-customize-group-for-platform-spec nil
  "Eemacs cross platform configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** Specific for windows
(defgroup entropy/emacs-customize-group-for-WINDOWS nil
  "Eemacs Windows platform specifications configuration customizable group."
  :group 'entropy/emacs-customize-group-for-platform-spec)

;; ***** w32 ime config
(defcustom entropy/emacs-microsoft-windows-native-ime-enhancement-enable nil
  "Enable win32 IME bug fix maybe detection at startup (fix
around of the bug of w32-ime)."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** eemacs wsl config
(defcustom entropy/emacs-microsoft-windows-unix-emulator-enable nil
  "Set whether you want to use =eemacs-msWinUnix-emulator=, so that variable
`entropy/emacs-microsoft-windows-unix-emulator-bin-path' will be used.

=eemacs-msWinUnix-emulator= is a abstract of the *nix emulator for emacs on
windows system, it brings up more benefits for windows emacs user
to experience as the mostly as for *nix platform.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-microsoft-windows-unix-emulator-root-path "c:/msys64/"
  "Set the baisic =eemacs-msWinUnix-emulator= apps hosted path for basic
shell-command using which also used in shell-buffer.

That we suggested using the *nix emulator for windows i.e. Msys2
as the apps hosted system, and automatically set it's 'usr/bin'
path as the value of
`entropy/emacs-microsoft-windows-unix-emulator-bin-path'. Defaulty
value is \"c:/msys64/\".

For minimally use, you can obtain a minimal Msys2 env from
git-for-windows-portable (https://git-scm.com/download/win).

NOTE: this variable just be used when `entropy/emacs-microsoft-windows-unix-emulator-enable'
init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defvar entropy/emacs-microsoft-windows-unix-emulator-bin-path
  (expand-file-name "usr/bin" entropy/emacs-microsoft-windows-unix-emulator-root-path)
  "The bin path of
`entropy/emacs-microsoft-windows-unix-emulator-root-path',
automatically calculated by eemacs."
  )

(defcustom entropy/emacs-microsoft-windows-unix-emulator-enable-extra nil
  "Enable extra =eemacs-msWinUnix-emulator= apps usage then variable
`entropy/emacs-microsoft-windows-unix-emulator-extra-root-path' will be used.

This ON-OFF variable are setted for follow occurrence:

    If you setting
    `entropy/emacs-microsoft-windows-unix-emulator-root-path' to
    'git-for-windows-portable' host which just contained the
    basic UNIX-LIKE commands that doesn't contianed commands like
    'man' and 'tree' or sth else, you want to using them as well
    in current emacs session.

See customized variable `entropy/emacs-microsoft-windows-unix-emulator-extra-root-path' for
details.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)


(defcustom entropy/emacs-microsoft-windows-unix-emulator-extra-root-path "c:/mingw64/"
  "Set the extra =eemacs-msWinUnix-emulator= root path, used for
some other subprocess of emacs called lying on `exec-path' of
*nix utils which not include in mainly
=eemacs-msWinUnix-emulator= apps path
`entropy/emacs-microsoft-windows-unix-emulator-bin-path'.

And this must using the type for the root of utils path
(i.e. which we can search the folder stucter of 'usr/bin' under
this root directly), like if you using msys2 , you must set this
variable to like:

\"c:/mingw64/\"

NOTE: this variable just be used when
`entropy/emacs-microsoft-windows-unix-emulator-enable-extra' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ****** windows git portable setting
(defcustom entropy/emacs-git-portable-enable nil
  "Whether enable portable git application usage on windows
platform when you want to use the portable git-for-window, you
can find it [[https://git-scm.com/download/win][here]], and then
variable `entropy/emacs-git-portable-enable' will be used.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-git-portable-path "c:/Git-Portable/cmd/"
  "The customize git applicaton's path which take priority on the
\"git\" command in
`entropy/emacs-microsoft-windows-unix-emulator-bin-path' when
`entropy/emacs-microsoft-windows-unix-emulator-enable' was
non-nil.

NOTE: this variable just be used when
`entropy/emacs-git-portable-enable' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** adding path for emacs built 'bin' folder
(defcustom entropy/emacs-win-emacs-bin-path-add t
  "Whether adding emacs bin folder to path on windows platform.

This ON-OFF varaible setted for adding emacs bin folder to both of
`exec-path' and \"PATH\" system variable.

It's useful that your can call emacs or other bult-in binary as
'convert' of builtin imagemaick support when emacs built with thus
supported.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** wsl terminal setting
(defcustom entropy/emacs-microsoft-windows-unix-emulator-terminal-enable nil
  "Enable external *nix terminal emulator on windows platform and
then variable `entropy/emacs-microsoft-windows-unix-emulator-terminal' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-microsoft-windows-unix-emulator-terminal "c:/msys64/msys2_shell.cmd"
  "Set the default *nix terminal emulator applictions, we recommend
to use Msys2's main terminal as that as, defaultly use the cmd
batch of Msys2 as \"c:/msys64/msys2_shell.cmd\" which has the most
functional has for.

Other suggestion list:

- =git-bash.exe= of git-for-windows
- =mingw32/64.exe= of both Msys2 or git-for-windows

Any other self specification are not with warranty.

NOTE: this variable just be used when
`entropy/emacs-microsoft-windows-unix-emulator-terminal-enable' init with non-nil value."
  :type 'file
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** enable fakecygpty
(defcustom entropy/emacs-win-fakecygpty-enable nil
  "Enable fake pty wrapper which let emacs for windows support
some (not complete) pty support, and then variable
`entropy/emacs-win-fakecygpty-path' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-win-fakecygpty-path "c:/Fakecgypty"
  "The FAKECYGPTY compiled binaries archive location.

FAKECYGPTY is a pty advice patch for windows platform which let
emacs for windows can use some pty feature maily for use bash as
term subprocess in major-mode `term' or `ansi-term' which not
supported originally. It is a fake patcher which built a middle
subroutine to camouflages a pty role to trick on emacs to use for
as.

The source of FAKECYGPTY is a open source software hosted on
github https://github.com/d5884/fakecygpty but without any
maintaining for some years, we fork it in =entropy-emacs=, you can
directly find it under =entropy-emacs= subroutine dir
`entropy/emacs-site-lisp-path'. Read its readme for compiling
methods and understanding its notice.

NOTE: this variable just be used when
`entropy/emacs-win-fakecygpty-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** w32 portable kits
(defgroup entropy/emacs-customize-group-for-w32-portable-kits nil
  "Eemacs w32 portable environment configuration customizable group."
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ****** enable portable mingw
(defcustom entropy/emacs-win-portable-mingw-enable nil
  "Enable mingw portable release usage for emacs in windows and
then variable `entropy/emacs-win-portable-mingw-bin-path' will be
used.

As compare to =eemacs-msWinUnix-emulator= apps `entropy/emacs-microsoft-windows-unix-emulator-bin-path' does
for, mingw was a *nix development toolchain emulator not at the
usage aspect, so that like clang toolchain can be used for emacs
in portable way."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-mingw-root-path "c:/msys64/mingw64/"
  "Setting the path of portable mingw for windows plattform.

If your have set the
`entropy/emacs-microsoft-windows-unix-emulator-root-path' so as
on Msys2 release, you may easily set its mingw path e.g
\"c:/msys64/mingw64\" for this vairable, or you can download
fresh new mingw release from http://www.mingw.org/.

NOTE: this variable just be used when
`entropy/emacs-win-portable-mingw-enable' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defvar entropy/emacs-win-portable-mingw-bin-path
  (expand-file-name "bin/" entropy/emacs-win-portable-mingw-root-path)
  "The bin path of `entropy/emacs-win-portable-mingw-root-path',
automatically calculated by eemacs."
  )

;; ****** enable portable clang
(defcustom entropy/emacs-win-portable-clang-enable nil
  "Enable clang windows port usage in portable way for emacs on
windows and then variable `entropy/emacs-win-portable-clang-path'
will be used.

In cases that when you has set
`entropy/emacs-win-portable-mingw-bin-path', you do not need to turn
on this variable, which you can download clang for windows within
mingw it self directly."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-clang-path "c:/eemacs-win-portable-apps/clang/bin"
  "Path for portable clang for windows plattform.

Its must be the \"bin\" subfolder path in the clang winport root
folder, defaultly set to \"c:/Clang/bin\".

You can download it from https://releases.llvm.org/download.html.

NOTE: this variable just be used when
`entropy/emacs-win-portable-clang-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable texlive
(defcustom entropy/emacs-win-portable-texlive-enable nil
  "Enable texlive portable release for emacs in windows and then
variable `entropy/emacs-win-portable-texlive-path' will be used.

In the case of that you've set the
`entropy/emacs-win-portable-mingw-bin-path', you do not need to turn
on this variable which you can directly install texlive for
windows within mingw release.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-texlive-path "c:/eemacs-win-portable-apps/texlive/bin"
  "Portable texlive winport release archive directory /bin/
path.

You can download it from https://www.tug.org/texlive/acquire-netinstall.html

NOTE: this varialbe just used when
`entropy/emacs-win-portable-texlive-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable grep
(defcustom entropy/emacs-win-portable-grep-enable nil
  "Enable Gnu-grep for windows portable release for emacs in
windows platform, and then variable
`entropy/emacs-win-portable-grep-path' will be used.

In the case of that you've set the `entropy/emacs-microsoft-windows-unix-emulator-bin-path', you
do not need to turn on this variable which almostly exists there
already.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-grep-path "c:/eemacs-win-portable-apps/gnu-grep/bin"
  "Portable grep winport release archive directory /bin/ path.

You can download the release from
https://sourceforge.net/projects/ezwinports/files/ which obtained
from windows Chocholatey project.

NOTE: this variable just be used when
`entropy/emacs-win-portable-grep-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable ag
(defcustom entropy/emacs-win-portable-ag-enable nil
  "Enable silver_searcher portable winport release for emacs in
windows platform, and then variable
`entropy/emacs-win-portable-ag-path' will be used.

In the case of that you've set the `entropy/emacs-microsoft-windows-unix-emulator-bin-path', you
do not need to turn on this variable which you can install it
directly whithin it.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-ag-path "c:/eemacs-win-portable-apps/ag/"
  "Portable silver_searcher winport release archive /bin/ path.

You can download it from
https://github.com/k-takata/the_silver_searcher-win32/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-ag-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable rg
(defcustom entropy/emacs-win-portable-rg-enable nil
  "Enable ripgrep portable winport release for emacs in windows
platform, and then variable `entropy/emacs-win-portable-rg-path'
will be used.

In the case of that you've set the `entropy/emacs-microsoft-windows-unix-emulator-bin-path', you
do not need to turn on this variable which you can install it
directly whithin it.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-rg-path "c:/eemacs-win-portable-apps/ripgrep/"
  "Portable ripgrep winport release archive /bin/ path.

You can download it from
https://github.com/BurntSushi/ripgrep/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-rg-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable cmder
(defcustom entropy/emacs-Cmder-enable nil
  "Enable Cmder portable release usage for emacs in windows platform,
and then variable `entropy/emacs-win-Cmder-path' will be used.

Cmder is a enhanced windows CMD terminal."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-Cmder-path "c:/eemacs-win-portable-apps/cmder/bin/Cmder.exe"
  "Portable Cmder release caller exec path.

You can download Cmder from:
https://github.com/cmderdev/cmder/releases

NOTE: this variable just be used when
`entropy/emacs-Cmder-enable' init with non-nil value."
  :type 'file
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable php
(defcustom entropy/emacs-win-portable-php-enable nil
  "Enable php winport release usage for emacs in windows and then
variable `entropy/emacs-win-portable-php-path' will be used.

In which case that PHP doesn't appear to exist on Msys2 or other
windows *nix emulator official repository, so that this one as be
for. But there's no need to turn on this if upstream of thus has
been added. "
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-php-path "c:/eemacs-win-portable-apps/php/"
  "Portable php winport release executable path.

You can download it from:
https://windows.php.net/download/

NOTE: this variable just be used when
`entropy/emacs-win-portable-php-enable' init with non-nil value. "
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)
;; ****** enable portable python
;; ******* python
(defcustom entropy/emacs-win-portable-python-enable nil
  "Enable python winport release usage for emacs in windows and
then variable `entropy/emacs-win-portable-python-installation-host-path' will be
used.

In the case that you've set the
`entropy/emacs-microsoft-windows-unix-emulator-bin-path', there's
no need to enable this so that you can directly install the
python from that =eemacs-msWinUnix-emulator= env but for lsp
client purpose you need to do this since mingw/msys2 python may
running broken for interactiv with windows-emacs."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-python-installation-host-path
  "c:/eemacs-win-portable-apps/python"
  "Portable win32 python installation dir root.

You can download it from https://winpython.github.io/

NOTE: when set this variable the
`entropy/emacs-win-portable-pip-host-path' will be also setted
automaticlaly by eemacs, so there's no need to set that var
manually again or other occasions occurred (see
`entropy/emacs-win-portable-pip-host-path' for details).

NOTE: this variable just be used when
`entropy/emacs-win-portable-python-enable' init with non-nil value.

NOTE: the installation must be python official site release
i.e. retrieved from
https://www.python.org/ftp/python/$version/python-$version-$arch.exe

Tricks: you can install the 'pip' for this installation by follow steps

#+begin_src bash
  curl -sSL https://bootstrap.pypa.io/get-pip.py -o get-pip.py
  python get-pip.py
#+end_src
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ******* pip

(defcustom entropy/emacs-win-portable-pip-host-path
  (expand-file-name "Scripts" entropy/emacs-win-portable-python-installation-host-path)
  "External portable pip path whicn be along with the
`entropy/emacs-win-portable-python-installation-host-path'.

This default value is automatically set according to
`entropy/emacs-win-portable-python-installation-host-path' by eemacs
for as, so you may not need to manually set this unless be
inconsistent with the actual occasions.

NOTE: this variable just be used when
`entropy/emacs-win-portable-python-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)


;; ****** enable portable nodejs
(defcustom entropy/emacs-win-portable-nodejs-enable nil
  "Enable nodejs winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-nodejs-installation-host-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the nodejs archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-msWinUnix-emulator=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-nodejs-installation-host-path
  "c:/eemacs-win-portable-apps/nodejs/"
  "Portable nodejs winport release installation root path.

You can download it from https://nodejs.org/en/download/

NOTE: this variable just be used when
`entropy/emacs-win-portable-nodejs-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable opencc
(defcustom entropy/emacs-win-portable-opencc-enable nil
  "Enable opencc winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-opencc-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the opencc archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-msWinUnix-emulator=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-opencc-path "c:/eemacs-win-portable-apps/opencc/bin/"
  "Portable opencc winport release /bin/ path.

You can download it from https://github.com/BYVoid/OpenCC/wiki/Download

NOTE: this variable just be used when
`entropy/emacs-win-portable-opencc-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable pandoc
(defcustom entropy/emacs-win-portable-pandoc-enable nil
  "Enable pandoc winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-pandoc-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the pandoc archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-msWinUnix-emulator=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-pandoc-path "c:/eemacs-win-portable-apps/pandoc/"
  "Portable pandoc winport release archive root path.

You can download it from https://github.com/jgm/pandoc/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-pandoc-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enbale portable jdk
(defcustom entropy/emacs-win-portable-jdk-enable nil
  "Enable java-jdk winport portable release usage for emacs in
windows and then variable `entropy/emacs-win-portable-jdk-path'
will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the java-jdk archive for as, or if
does otherwise that you do not need to turn on this, when you
enabled =eemacs-msWinUnix-emulator=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-jdk-path "c:/eemacs-win-portable-apps/openjdk/bin"
  "Portable java-jdk winport release /bin/ path.

You can download it from: https://jdk.java.net/

NOTE: this variable just be used when
`entropy/emacs-win-portable-jdk-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable zeal
(defcustom entropy/emacs-win-portable-zeal-enable 'nil
  "Enable Zealdoc usage for emacs in windows and then variable
`entropy/emacs-win-portable-zeal-path' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-zeal-path "c:/eemacs-win-portable-apps/zeal/"
  "Portable Zealdoc winport release archive root path.

You can download it from http://zealdocs.org/

NOTE: this variable just be used when
`entropy/emacs-win-portable-zeal-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ****** enable portable putty
(defcustom entropy/emacs-win-portable-putty-enable nil
  "Enable putty portable for emacs in windows for provide the
putty tramp method instead of the lag ssh one on windows platform
and then variable `entropy/emacs-win-portable-putty-path' will be
used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-putty-path "c:/eemacs-win-portable-apps/putty/"
  "Portable putty winport release root path.

You download the putty main caller and subroutines individually
from:
https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html
and then gathering them into one place.

NOTE: this variable just be used when
`entropy/emacs-win-portable-putty-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ** Top APIs
;; *** top paths
(defvar entropy/emacs-hosted-path
  (file-name-directory load-file-name)
  "entropy-emacs hosted path.

Eemacs self top root path.")

(defvar entropy/emacs-core-components-hosted-path
  (expand-file-name "core" entropy/emacs-hosted-path)
  "entropy-emacs core library hosted path.")

(defvar entropy/emacs-site-lisp-path
  (expand-file-name "site-lisp" entropy/emacs-hosted-path)
  "Eemacs site-lisp path.

The native binded emacs extensions hosted path.")

(defvar entropy/emacs-fancy-splash-logo-file
  (expand-file-name "logo/logo.png" entropy/emacs-core-components-hosted-path)
  "Eemacs fancy logo file.")

(defvar entropy/emacs-fancy-splash-text-logo-file
  (expand-file-name "logo/logo.txt"
                    entropy/emacs-core-components-hosted-path)
  "Eemacs fancy logo text type file (used for TUI).")

(defvar entropy/emacs-initial-theme-path
  (expand-file-name "startup-theme" entropy/emacs-core-components-hosted-path)
  "Initial theme path for entropy-emacs. ")

(defvar entropy/emacs-templates-dir
  (expand-file-name "templates" entropy/emacs-core-components-hosted-path)
  "The sourced templated files archive location of entropy-emacs")

(defvar entropy/emacs-doc-repo-path
  (expand-file-name "entropy-emacs-doc"
                    entropy/emacs-site-lisp-path)
  "The whole eemacs specified doc repo path")

(defvar entropy/emacs-core-doc-file-archives-plist
  `(:org
    ,(expand-file-name
      "org/entropy-emacs_introduction.org"
      entropy/emacs-doc-repo-path)
    :html
    ,(expand-file-name
      "org/entropy-emacs_introduction.html"
      entropy/emacs-doc-repo-path)
    :texi
    ,(expand-file-name
      "org/entropy-emacs_introduction.texi"
      entropy/emacs-doc-repo-path)
    :texinfo
    ,(expand-file-name
      "org/entropy-emacs_introduction.info"
      entropy/emacs-doc-repo-path))
  "A plist stored variable number of archive formats of eemacs
core document file.")

(defvar entropy/emacs-extra-doc-file-archives-alist
  `((sicp
     :texi
     ,(expand-file-name
       "elements/sicp/sicp.texi"
       entropy/emacs-doc-repo-path)
     :texinfo
     ,(expand-file-name
       "elements/sicp/sicp.info"
       entropy/emacs-doc-repo-path)))
  "An alist stored doc path information of eemacs-specification
whose car is an symbol indicate the doc name and the cdr is same
as `entropy/emacs-core-doc-file-archives-plist'.")

;; *** emacs feature support judger
(defun entropy/emacs-dynamic-module-support-p ()
  "Return non-nil when current emacs session support load dynamic
module."
  (member "MODULES"
          (split-string
           system-configuration-features nil t)))

(defun entropy/emacs-vterm-support-p ()
  "Return non-nil when current emacs session support use
`vterm'."
  (and (not (member system-type
                    '(windows-nt
                      ms-dos)))
       (entropy/emacs-dynamic-module-support-p)
       (and (executable-find "cmake")
            (executable-find "make")
            (executable-find "libtool")
            (executable-find "cmake"))
       t))

;; *** `require' eemacs internal enhancement?

(defun entropy/emacs-top-improvements-for-require (orig-func &rest orig-args)
  "The eemacs top improvements for `require'.

Like `require' but just do `memq' for feature when it is an symbol.

The existence of this advice is for some packages not use
`require' in proper way thus cause the long latency for the
particular lisp program, sicne althought `require' is an C
implementation lisp function, it'll use more time to check an
feature for whether it need to be required with more and more
features are loaded presented in `features'. Because `require'
internally do some extra checks and operations even a feature is
loaded, and the checks and operations are rely on the
`current-load-list' which see the `require' C source:

#+begin_src C
/* Record the presence of `require' in this file
   even if the feature specified is already loaded.
   But not more than once in any file,
   and not when we aren't loading or reading from a file.  */
if (!from_file)
  {
    Lisp_Object tail = Vcurrent_load_list;
    FOR_EACH_TAIL_SAFE (tail)
      if (NILP (XCDR (tail)) && STRINGP (XCAR (tail)))
        from_file = true;
  }

if (from_file)
  {
    tem = Fcons (Qrequire, feature);
    if (NILP (Fmember (tem, Vcurrent_load_list)))
  LOADHIST_ATTACH (tem);
  }
tem = Fmemq (feature, Vfeatures);

if (NILP (tem))
{
...
}
#+end_src

The `curernt-load-list' is growth when the emacs session using,
because mordern emacs configurations like =doom=, =spacemacs=
usually use derfer loading to speedup the emacs startup
procedure.

EEMACS_MAINTENANCE: this advice is just used when
`entropy/emacs-startup-with-Debug-p' is disabled since we can not
judge this patch whether will broken the emacs internal
mechanism, so be carefully.
"
  (let ((fp (car orig-args)))
    (if (and (symbolp fp)
             (not (bound-and-true-p entropy/emacs-startup-with-Debug-p)))
        (if (memq fp features)
            fp
          (apply orig-func orig-args))
      (apply orig-func orig-args))))
(advice-add 'require
            :around #'entropy/emacs-top-improvements-for-require)

;; *** run-hooks with prompt
(defvar entropy/emacs--run-hooks-cache nil)

(defun entropy/emacs-run-hooks-prompt (orig-func &rest orig-args)
  "Prompt for `run-hooks' for reduce lagging nervous. It's a
advice wrapper, do not calling it in the normal way"
  (let ((condis (lambda ()
                  (or noninteractive
                      (minibufferp))))
        (indicator (car orig-args))
        rtn)
    (push (cons (format-time-string "[%Y-%m-%d %a %H:%M:%S]") orig-args)
          entropy/emacs--run-hooks-cache)
    (unless (funcall condis)
      (if (symbolp indicator)
          (message "Running hooks '%s' ..." indicator)
        (message "Running hooks ..."))
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

(defun entropy/emacs-select-trail-hook (&optional pdumper-no-end)
  "Automatically selects a hook specified meaning of a
=entropy-emacs-startup-trail-hook=. Return for
`entropy/emacs-init-mini-hook' and `entropy/emacs-init-X-hook' if
`entropy/emacs-fall-love-with-pdumper' is nil or optional arg
PDUMPER-NO-END is non-nil.

At any time, only one of those hook is recongnized as
=entropy-emacs-startup-trail-hook=, and it is a hook for trigger
configration enable when all the preparations have done.

See `entropy/emacs-startup-end-hook' either."
  (if entropy/emacs-fall-love-with-pdumper
      (if (null pdumper-no-end)
          'entropy/emacs-pdumper-load-hook
        (if entropy/emacs-minimal-start
            'entropy/emacs-init-mini-hook
          'entropy/emacs-init-X-hook))
    (if entropy/emacs-minimal-start
        'entropy/emacs-init-mini-hook
      'entropy/emacs-init-X-hook)))

(defvar entropy/emacs-startup-end-hook nil
  "Hook ran after entropy-emacs finally initial-done, all the
functions hosted in this Hook will ran after
=entropy-emacs-startup-trail-hook= just before
`entropy/emacs-startup-done' be bounded.

This hook used to arrange hooks must ran after the
=entropy-emacs-startup-trail-hook= be ran out in which case it is
the final preparation to the startup eemacs.

Also see `entropy/emacs-run-startup-end-hook' for restriction
description.")

(defvar entropy/emacs-after-startup-hook nil
  "Hook ran after eemacs startup done
i.e. `entropy/emacs-startup-done' is set, used to initialize
startup after procedure of some defaults configs must be without
lazy-load.")

;; *** making procedure
(defun entropy/emacs-is-make-session ()
  "Obtained the 'EEMACS_MAKE' env variable value if valid
otherwise return nil.

This function commonly used to judge whether start emacs in a
`noninteractive' status but in daemon load procedure, where
specially indicate to other subroutines to get the 'batch
run' (e.g. use entropy emacs as a shell) type according to the
value of entropy emacs specified environment variable
\"EEMACS_MAKE\".

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (let ((env-p (getenv "EEMACS_MAKE")))
    (cond
     ((or (null env-p)
          (string-empty-p env-p))
      nil)
     (t
      env-p))))

(defun entropy/emacs-is-make-all-session ()
  "Obtained the 'EEMACS_MAKE_ALL' env variable value if valid
otherwise return nil.

This function commonly used to judge whether start emacs in a
`noninteractive' status but in daemon load procedure, where
specially indicate to other subroutines to get the 'batch
run' (e.g. use entropy emacs as a shell) type according to the
value of entropy emacs specified environment variable
\"EEMACS_MAKE_ALL\".

NOTE: you should always use this function to get thus variable
value where there's no published for any of the internal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
  (let ((env-p (getenv "EEMACS_MAKE_ALL")))
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
    ;; dump manually from command line
    (catch :exit
      (dolist (arg command-line-args)
        (when (string-match-p "dump-emacs-portable" arg)
          (setq rtn t)
          (throw :exit nil))))
    ;; eemacs internal detection
    (setq rtn
          (or rtn
              (and noninteractive
                   (bound-and-true-p
                    entropy/emacs-fall-love-with-pdumper))))
    rtn))

;; *** remote refer apis

(defun entropy/emacs-filename-is-remote-p (filename)
  "Judge whether file name is remote style"
  (if (fboundp 'tramp-tramp-file-p)
      (tramp-tramp-file-p filename)
    nil))

;; *** large file/buffer detection

(defun entropy/emacs-check-buffer-has-long-line-p
    (&optional buffer-or-name long-threshold max-lines-to-detect)
  "Decide whether buffer (default to `current-buffer' unless
BUFFER-OR-NAME is set) has long line functionally by `so-long'.

LONG-THREAD is used to set the `so-long-lines' and default to 1000.

MAX-LINES-TO-DETECT is used when emacs-version lower than 28.0.91
which has the same meaning as `so-long-max' and default to 1000."
  (require 'so-long)
  (let ((func (if (version< emacs-version "28.0.91")
                  'so-long-detected-long-line-p
                'so-long-statistics-excessive-p))
        (so-long-threshold (or long-threshold 1000))
        (so-long-max-lines (or max-lines-to-detect
                               1000)))
    (if buffer-or-name
        (with-current-buffer buffer-or-name
          (funcall func))
      (funcall func))))

;; EEMACS_MAINTENANCE: this var must be predicated `natnump' and
;; `integerp' or will make as messy.
(defconst entropy/emacs-large-file-warning-threshold
  (* 8 (expt 1024 2))
  "Like `large-file-warning-threshold' but used in eemacs internal
as fallback. (also see
`entropy/emacs-large-file-warning-threshold-get').

NOTE: this var is an const var, do not modify it in any cases
include in `let' binding or will make disaster.")
(defun entropy/emacs-large-file-warning-threshold-guard
    (sym newval op where)
  (when op
    (error
     "`%S' to the const `entropy/emacs-large-file-warning-threshold'."
     op)))
(add-variable-watcher 'entropy/emacs-large-file-warning-threshold
                      #'entropy/emacs-large-file-warning-threshold-guard)

(setq large-file-warning-threshold entropy/emacs-large-file-warning-threshold)

(defun entropy/emacs-large-file-warning-threshold-get ()
  "Always return an fixnum value for file size restriction usage.

In order to query `large-file-warning-threshold',
`entropy/emacs-large-file-warning-threshold'."
  (or (and
       (integerp large-file-warning-threshold)
       (natnump large-file-warning-threshold)
       large-file-warning-threshold)
      entropy/emacs-large-file-warning-threshold))

(defvar entropy/emacs-find-file-judge-fllename-need-open-with-external-app-core-filters nil
  "A list of functions to handler a filename whether need to open
with external app used for
`entropy/emacs-find-file-judge-filename-need-open-with-external-app-p'.

NOTE: for eemacs developer notice of that this variable always
need to be initial as nil before `entropy/emacs-startup-done',
since at startup time we do not want to handle any external open
with requests.")
(defun entropy/emacs-find-file-judge-filename-need-open-with-external-app-p (filename)
  "Judge whether open FILENAME externally"
  (and (bound-and-true-p entropy/emacs-startup-done)
       entropy/emacs-find-file-judge-fllename-need-open-with-external-app-core-filters
       (catch :exit
         (dolist (func
                  entropy/emacs-find-file-judge-fllename-need-open-with-external-app-core-filters)
           (when (funcall func filename)
             (throw :exit t)))
         nil)))

(defvar entropy/emacs-find-file-judge-filename-is-emacs-intspecially-core-ignore-handlers
  nil
  "List of file name handler not be handled by
`entropy/emacs-find-file-judge-filename-is-emacs-intspecially-p'
which is stored in `file-name-handler-alist'.")
(defvar entropy/emacs-find-file-judge-filename-is-emacs-intspecially-core-filters
  '((lambda (filename)
      (catch :exit
        (dolist (el file-name-handler-alist)
          (let ((matcher (car el))
                (handler (cdr el)))
            (unless (member
                     handler
                     entropy/emacs-find-file-judge-filename-is-emacs-intspecially-core-ignore-handlers
                     )
              (when (string-match-p matcher filename)
                (throw :exit t)))))
        nil)))
  "List of functions used for
`entropy/emacs-find-file-judge-filename-is-emacs-intspecially-p'
where each one accept one argument of a filename and return
non-nil if the file is a emacs special dealing with file type.")
(defun entropy/emacs-find-file-judge-filename-is-emacs-intspecially-p (filename)
  "Judge a file named as FILENAME whether matched a emacs
internally specially maitained file type, return non-nil while
thus."
  (when entropy/emacs-find-file-judge-filename-is-emacs-intspecially-core-filters
    (catch :exit
      (dolist (func
               entropy/emacs-find-file-judge-filename-is-emacs-intspecially-core-filters)
        (when (funcall func filename)
          (throw :exit t)))
      nil)))

(defconst entropy/emacs-unreadable-buffer-so-long-threshold 1000)
(defvar entropy/emacs-unreadable-file-judge-function nil
  "The eemacs file unreadable judger, usually it store a function
but aslo can be nil, so used after using `functionp' to check
as.")

(defvar entropy/emacs-unreadable-file-unjuge-cases-predicates nil
  "A list of unjudge predicates for
`entropy/emacs-unreadable-file-unjuge-cases' for filtered
of. Escape the filter loop matching while any element of this
list return t i.e. inidcate to unjudge for as.")
(defun entropy/emacs-unreadable-file-unjuge-cases (filename)
  "The predicate to let eemacs unreadable file judge system ignore."
  (require 'image-file)
  (let ((fsize (and
                (file-exists-p filename)
                (file-attribute-size (file-attributes filename)))))
    (catch :exit
      (when (and
             ;; restrict image size under 100M
             (and fsize (<= fsize (* 100 (expt 1024 2))))
             (string-match-p (image-file-name-regexp) filename))
        (throw :exit t))

      (when entropy/emacs-unreadable-file-unjuge-cases-predicates
        (dolist (el entropy/emacs-unreadable-file-unjuge-cases-predicates)
          (when (and (functionp el)
                     (funcall el))
            (throw :exit t))))
      ;; TODO add more unjudge predicates
      )))

(setq entropy/emacs-unreadable-file-judge-function
      (lambda (filename)
        (let* ((inhibit-read-only t)
               (kill-buffer-hook nil)
               (f-readp (file-readable-p filename))
               (f-existp (file-exists-p filename))
               (fsize-max (entropy/emacs-large-file-warning-threshold-get))
               (fsize (and
                       f-readp f-existp
                       (file-attribute-size
                        (file-attributes
                         filename)))))
          (catch :exit
            ;; firstly we should escape check when matching unjudge cases
            (when (entropy/emacs-unreadable-file-unjuge-cases filename)
              (throw :exit nil))
            (cond (
                   ;; Fistly we just determin the filesize limitation
                   (and fsize
                        (> fsize fsize-max))
                   t)
                  ;; and then we detect with `entropy/emacs-unreadable-buffer-judge-function'
                  (t
                   (with-temp-buffer
                     (when
                         ;; Just judge existed and readable file so we
                         ;; ignore errors for the reading procedure.
                         (ignore-errors
                           (insert-file-contents
                            filename))
                       (when (functionp entropy/emacs-unreadable-buffer-judge-function)
                         (funcall entropy/emacs-unreadable-buffer-judge-function
                                  (current-buffer)))))))))))

(defvar entropy/emacs-unreadable-buffer-judge-function nil
  "The eemacs buffer unreadable judger, usually it store a function
but aslo can be nil, so used after using `functionp' to check
as.")
(setq entropy/emacs-unreadable-buffer-judge-function
      (lambda (buff)
        (with-current-buffer buff
          (or (> (buffer-size)
                 (entropy/emacs-large-file-warning-threshold-get))
              (progn
                (entropy/emacs-check-buffer-has-long-line-p
                 nil
                 entropy/emacs-unreadable-buffer-so-long-threshold
                 (when (version< emacs-version "28.0.91")
                   (save-excursion
                     (goto-char (point-max))
                     (line-number-at-pos)))))))))

;; ** entropy-emacs initialize
;; *** basic setting
;; **** make sure gpg pinentry passphrase prompt using emacs minibuffer
(setq epa-pinentry-mode 'loopback)
;; The pinentry-emacs interface
;; >>> https://github.com/ecraven/pinentry-emacs
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd
              (concat
               (replace-regexp-in-string
                "%22" "\""
                (replace-regexp-in-string "%0A" "\n" desc))
               prompt ": "))))
    str))

(defvar pinentry-emacs-binary-path
  (expand-file-name
   "annex/pinentry-emacs/pinentry-emacs"
   entropy/emacs-user-emacs-directory)
  "The 'pinetry-emacs' binary path of eemacs specified

NOTE: 'pinentry-emacs' is archived into eemacs yet")

(defun pinentry-emacs-gpg-agent-conf-patched-p (&rest _)
  "Judge whether current env use `pinetry-emacs' as gpg-agent
pinentry passphrase prompt."
  (let ((conf (expand-file-name "gpg-agent.conf" "~/.gnupg")))
    (and (file-exists-p conf)
         (with-temp-buffer
           (insert-file-contents conf)
           (goto-char (point-min))
           (and
            (re-search-forward "^pinentry-program \\(.*pinentry-emacs\\)$")
            (file-exists-p (match-string 1)))))))

(defun pinentry-emacs-gpg-agent-conf-patch-func (&rest _)
  "Patch gpg-agent.conf under 'HOME/.gnupg' with using
`pinentry-emacs' as default pinentry before makeing backup of
origin config file."
  (let* ((conf (expand-file-name "gpg-agent.conf" "~/.gnupg"))
         (conf-bk-name
          (concat
           conf
           (format
            ".%s.orig"
            (format-time-string "%Y-%m-%d %a %H:%M:%S"))))
         (proc-buff (get-buffer-create
                     "*eemacs-pinentry-emacs-gpg-agent-reload-cbk*"))
         (inhibit-read-only t))
    (when (file-exists-p conf)
      (rename-file conf conf-bk-name))
    (with-current-buffer (find-file-noselect conf)
      (erase-buffer)
      (insert "allow-emacs-pinentry\n")
      (insert
       (format "pinentry-program %s"
               pinentry-emacs-binary-path))
      (save-buffer)
      (let ((kill-buffer-hook))
        (kill-buffer)))
    (with-current-buffer proc-buff
      (erase-buffer))
    (call-process-shell-command "gpgconf --reload gpg-agent" nil proc-buff)
    (with-current-buffer proc-buff
      (let ((cbk (buffer-substring (point-min) (point-max))))
        (unless (string-empty-p cbk)
          (user-error "Patch gpg-agent.conf fatal: %s" cbk))
        (kill-buffer)
        (message "Patch gpg-agent.conf successfully")))))

;; *** intial advice
;; **** find file patch
;; ***** unreadable file detection

(defun __ureadable-file-do-prompt-of-message (format &rest args)
  (if (yes-or-no-p (format format args))
      t
    (user-error "Abort!")))

(defun entropy/emacs-unreadable-file-advice-for-finid-file
    (orig-func &rest orig-args)
  (if (and
       ;; just used in startup done occasion
       (bound-and-true-p entropy/emacs-startup-done)
       (functionp entropy/emacs-unreadable-file-judge-function)
       ;; ignore any existed buffer matched FILENAME
       (not (find-buffer-visiting (car orig-args)))
       ;; Not remote file
       (not (entropy/emacs-filename-is-remote-p (car orig-args)))
       ;; Not judge for magic filename handler for
       ;; `openwith-file-handler', since its open with external
       ;; app.
       (not (entropy/emacs-find-file-judge-filename-need-open-with-external-app-p
             (car orig-args)))
       ;; the file must not be a emacs internally specialed handled type
       (not (entropy/emacs-find-file-judge-filename-is-emacs-intspecially-p
             (car orig-args))))
      (let* ((error-fmtstr
              "File '%s' is not readable e.g. its may freeze emacs, Abort!")
             (filename (car orig-args))
             (wildcards (cadr orig-args))
             (unreadable (if wildcards 'multifiles
                           (funcall entropy/emacs-unreadable-file-judge-function
                            filename))))
        (cond ((null unreadable)
               (apply orig-func orig-args))
              ((eq unreadable 'multifiles)
               (let ((files (condition-case nil
                                (file-expand-wildcards filename t)
                              (error (list filename))))
                     (counts 0))
                 (dolist (file files)
                   (when (funcall entropy/emacs-unreadable-file-judge-function file)
                     (cl-incf counts)
                     (warn
                      (format "[%s] %s"
                              counts
                              (format error-fmtstr file)))))
                 (when (> counts 0)
                   (__ureadable-file-do-prompt-of-message
                    (format "There's %s file are unreadable e.g. which may freeze emacs, \
for guaranteeing the current emacs session workable, eemacs refuse this find-file action. \
Do you want to open it with messy?"
                            counts)))
                 (let (
                       ;; prevent internal `abort-if-file-too-large'
                       ;; prompts since we've judged yet
                       (large-file-warning-threshold most-positive-fixnum))
                   (apply orig-func orig-args))))
              (t
               (__ureadable-file-do-prompt-of-message
                (format "File '%s' is not readable e.g. its may freeze emacs, \
Do you want to open it with messy?"
                        filename))
               (let ((large-file-warning-threshold most-positive-fixnum))
                 (apply orig-func orig-args)))))
    (apply orig-func orig-args)))

(advice-add 'find-file
            :around
            #'entropy/emacs-unreadable-file-advice-for-finid-file)

(defun entropy/emacs-abort-if-file-too-large (orig-func &rest orig-args)
  "Like `abort-if-file-too-large' but escape judge in some cases."
  (let ((filename (nth 2 orig-args)))
    (if (and
         ;; just usage in init done occasion
         (bound-and-true-p entropy/emacs-startup-done)
         (or
          ;; ignore any existed buffer matched FILENAME
          (find-buffer-visiting filename)
          ;; ingore external open needed FILENAME
          (entropy/emacs-find-file-judge-filename-need-open-with-external-app-p
           filename)

          ;; Ignore file which is a emacs internally specialed handled type
          ;;
          ;; FIXME: shall we need to escape this? since a large
          ;; internally handled type file may also freeze emacs, or we
          ;; should use another tunny way?
          ;;
          ;; (entropy/emacs-find-file-judge-filename-is-emacs-intspecially-p
          ;;  filename)

          ;; prevent duplicated call, since the
          ;; `entropy/emacs-unreadable-file-advice-for-finid-file'
          ;; may did the same
          (and
           ;; must check `large-file-warning-threshold' whether null,
           ;; since it can be nil in its API documentation description.
           large-file-warning-threshold
           (= large-file-warning-threshold most-positive-fixnum))
          ;; matched unjudge cases
          (entropy/emacs-unreadable-file-unjuge-cases filename)))
        (let ((large-file-warning-threshold most-positive-fixnum))
          (apply orig-func orig-args))
      (apply orig-func orig-args))))
(advice-add 'abort-if-file-too-large :around #'entropy/emacs-abort-if-file-too-large)

(defun entropy/emacs--supress-fontlock-mode (orig-func &rest orig-args)
  "Disable font-lock render if current-buffer is large"
  (let ((buff (current-buffer)))
    (cond
     ((functionp entropy/emacs-unreadable-buffer-judge-function)
      (unless (funcall entropy/emacs-unreadable-buffer-judge-function buff)
        (apply orig-func orig-args)))
     (t
      (apply orig-func orig-args)))))

(add-hook 'entropy/emacs-after-startup-hook
          #'(lambda ()
              (when (display-graphic-p)
                (unless (bound-and-true-p global-font-lock-mode)
                  (global-font-lock-mode +1))
                (with-eval-after-load 'font-core
                  (advice-add 'turn-on-font-lock
                              :around #'entropy/emacs--supress-fontlock-mode)))))

;; *** add eemacs texinfo to info-path

(with-eval-after-load 'info
  ;; append the eemacs intro doc
  (setq Info-default-directory-list
        (append Info-default-directory-list
                (list
                 (file-name-directory
                  (plist-get
                   entropy/emacs-core-doc-file-archives-plist
                   :texinfo)))))

  ;; append eemacs extra docs
  (dolist (doc entropy/emacs-extra-doc-file-archives-alist)
    (let* ((doc-name (car doc))
           (doc-attr (cdr doc))
           (doc-info-file (plist-get doc-attr :texinfo))
           (doc-info-dir
            (ignore-errors
              (file-name-directory
               doc-info-file))))
      (when (file-exists-p doc-info-dir)
        (setq Info-default-directory-list
              (append
               Info-default-directory-list
               (list doc-info-dir)))))))

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
;; start procedure finished (before any other trail hook run), thus
;; when pdumper session start, `display-graphic-p' function will not
;; cause some issue e.g. `window-font-width' will throw out error
;; since the very beginning of pdumper session may(why?) starting on
;; cli status so that `font-info' will retrieve 'nil' as the callback
;; of `face-font'.
(let ((hook (if entropy/emacs-minimal-start
                'entropy/emacs-init-mini-hook
              'entropy/emacs-init-X-hook)))
  (add-hook hook
            #'(lambda ()
                (advice-remove
                 'display-graphic-p
                 #'entropy/emacs-display-graphic-fake-advice))))

;; *** clean stuff files
(when-let ((_ (not (entropy/emacs-env-init-with-pure-eemacs-env-p)))
           (top entropy/emacs-stuffs-topdir))
  (unless (file-exists-p top)
    (make-directory top))
  ;; root dir host in `top'
  ;; NOTE: if item is a directory the notation of its path must tail with slash
  (dolist (item `((package-user-dir . "elpa/")
                  (bookmark-file . "bookmarks")
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
                  (request-storage-directory . "request/")

                  ;; w3m
                  (w3m-default-save-directory . "w3m/save/")
                  (w3m-profile-directory . "w3m/profile/")
                  (w3m-external-view-temp-directory . "w3m/temp/")
                  (w3m-form-textarea-directory . "w3m/textarea/")

                  ;; lsp mode
                  (lsp-session-file . "lsp/lsp-session-v1")
                  (lsp-intelephense-storage-path . "lsp/cache/intelephense")
                  (lsp-server-install-dir . "lsp/cache/install/")
                  ;; - lsp java
                  (lsp-java-workspace-dir . "lsp/lsp-java-workspace/")

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

                  ;; pyim/liberime
                  (liberime-user-data-dir . "rime")
                  (pyim-dcache-directory . "pyim/dcache/")

                  ;; org
                  (org-id-locations-file . ".org-id-locations")
                  (entropy/org-exptt-html-theme-cache-dir . "org-themes/org-html-themes")

                  ;; miscellanies
                  (idlwave-config-directory . "idlwave/")
                  (elfeed-db-directory . "elfeed-db/")
                  (prescient-save-file . "var/prescient-save.el")
                  ))
    (let* ((var-sym (car item))
           (path nil)
           (path-root nil))
      (unless
          ;; avoid when user pre sets in `custom-file'
          (and (boundp var-sym)
               ;; but for some pre loaded customized var
               (not (eq var-sym 'auto-save-list-file-prefix)))
        (setq path (directory-file-name (expand-file-name (cdr item) top)))
        (setq path-root (file-name-directory path))
        ;; make sure declared as init
        (eval `(defvar ,var-sym))
        (set-default var-sym path)
        ;; create each subs path chain for preventing unconditionally
        ;; file create fatal from thus.
        (unless (file-directory-p path-root)
          (make-directory path-root t))
        ;; create the dir if item is an directory
        (if (and (string-match-p "/$" (cdr item))
                 (null (file-exists-p path)))
            (make-directory path t)))
      ))

  ;; directly set root dir using `top' dir
  (dolist (item '(eww-bookmarks-directory))
    (set item top)))


;; mkdir for pre set
(dolist (dir-obj '((entropy/emacs-internal-ime-rime-user-data-host-path
                    .
                    (lambda (dir)
                      (and (bound-and-true-p
                            entropy/emacs-internal-ime-use-rime-as)
                           (stringp dir)
                           (not (file-exists-p
                                 dir))))))
                 ;; TODO: add more to comprehensively for eemacs customs
                 )
  (let* ((dir-sym (car dir-obj))
         (dir (or (and (stringp dir-sym)
                       dir-sym)
                  (and (functionp dir-sym)
                       (funcall dir-sym))
                  (symbol-value dir-sym))))
    (when (funcall (cdr dir-obj) dir)
      (make-directory dir t))))


;; * provide
(provide 'entropy-emacs-defcustom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
