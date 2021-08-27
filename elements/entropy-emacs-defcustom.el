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

(defconst entropy/emacs-custom-common-file
  (expand-file-name
   "custom.el"
   entropy/emacs-user-emacs-directory)
  "The value for `custom-file' but specified for =entropy-emacs=.")

(let ((cus entropy/emacs-custom-common-file))
  (setq-default custom-file entropy/emacs-custom-common-file)
  (when (file-exists-p cus)
    (message "")
    (message "====================================")
    (message "[Loading] custom specifications ...")
    (message "====================================\n")
    (load cus)))

;; ** Internal init
(defvar __eemacs-ext-union-host "~/.config/entropy-config/entropy-emacs")

;; ** Customizable Variables
(defgroup entropy-emacs-customize-top-group nil
  "Eemacs customizable variables top group."
  :group 'extensions)

;; *** Basic
(defgroup entropy/emacs-customize-group-for-basic-configuration nil
  "Eemacs basic configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** fundamental config
(defgroup entropy/emacs-customize-group-for-fundametal-configuration nil
  "Eemacs fundamental configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; ***** fundamental individuals
(defcustom entropy/emacs-stuffs-topdir
  (expand-file-name "stuffs" entropy/emacs-user-emacs-directory)
  "The stuffs collection host path, for as `savehist-file',
`bookmark-file' cache host. This setting mainly for cleanup
`entropy/emacs-user-emacs-directory'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

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

(defcustom entropy/emacs-top-prefix-key-cons '("C-`" .  "C-@")
  "The cons stores the prefix key for `entropy/emacs-top-keymap',
the car for GUI session, and cdr for TUI thus as well.

The intention to get different prefix key for GUI anD TUI is for
that the terminal emulation used for emacs may not have the full
key-stroke experience."
  :type '(cons (key-sequence :tag "GUI bind") (key-sequence :tag "TUI bind"))
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

(defcustom entropy/emacs-user-full-name (getenv "USERNAME")
  "The value for `user-full-name' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-user-mail-address "bmsac0001@gmail.com"
  "The value for `user-mail-address' but specified for
=entropy-emacs=."
  :type 'string
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

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
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-init-display-line-numbers-mode nil
  "Enable `global-display-line-numbers-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-init-hl-line-mode t
  "Enable `global-hl-line-mode' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-init-beacon-blink nil
  "Enable `beacon-blink' at start up time."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-fill-paragraph-width 70
  "Setting fill-paragraph width, default 70 to follow the emacs
convention."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-use-icon t
  "Whether to use icon visualization when available."
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

(defcustom entropy/emacs-align-window-center-with? 'olivetti
  "The window central visual toggler type, valid of 'olivetti' or 'basic'.

=olivetti= was a riched window center align package which provide
shrink and expand auto-key, and without alignment be killed
problem, =basic= type is simple but without fully featured.
"
  :type '(choice
          (const :tag "Olivetti mode" olivetti)
          (const :tag "Basic simple way" basic))
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-kill-ring-persist-file
  (expand-file-name "kill-ring.persist" entropy/emacs-stuffs-topdir)
  "Persist cache file for storing `kill-ring'."
  :type 'file
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-custom-comment-dwim-type 'separedit
  "The comment-dwim type chosen, valid of `poporg' or
`separedit' as default.

NOTE: poporg is obsolete as an legacy option."
  :type 'symbol
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-custom-comment-dwim-prefix "C-c \""
  "The comment-dwim trigger keybind"
  :type 'string
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

;; ***** emacs extension use options config
(defgroup entropy/emacs-customize-group-for-emacs-extensions nil
  "Eemacs emacs extensions management configuration customizable group."
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-ext-elpkg-get-type 'origin
  "Init emacs with elisp extensions from entropy-emacs-extensions or
elpa and melpa.

Available value are 'submodules-melpa-local' and 'origin'.

Type of 'submodules-melpa-local' indicates to use
=entropy-emacs-extensions= (see
`entropy/emacs-ext-eemacs-elpkg-archive-project-dir' for its brief
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
                 submodules-melpa-local))
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; NOTE:
;; We must strictly did judgement for packages refer setting to
;; guarantee the package initialization procedure did correctly before
;; any eemacs specification loading.
(unless (member entropy/emacs-ext-elpkg-get-type
                '(origin submodules-melpa-local))
  (error "Invalid value for `entropy/emacs-ext-elpkg-get-type'"))

(defcustom entropy/emacs-ext-eemacs-elpkg-archive-project-dir
  (expand-file-name
   "entropy-emacs-extensions"
   __eemacs-ext-union-host)
  "entropy-emacs extensions collection archive location. This
collection used to retrieving all entropy-emacs elpa or melpa
extensions' repos as submodules archived as one single project
used for version controlling. You can get it from
'https://github.com/c0001/entropy-emacs-extensions'.

This archive used when type of 'submodules-melpa-local' is set to
customized variable `entropy/emacs-ext-elpkg-get-type'."

  :type 'directory
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

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

(defcustom entropy/emacs-ext-emacs-pkgel-get-pkgs-root
  (expand-file-name
   "entropy-emacs-extensions-elpa"
   __eemacs-ext-union-host)
  "entropy-emacs elpa extensions directory for hosting the
upstream installed packages of `package.el'."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

(defcustom entropy/emacs-ext-user-specific-load-paths nil
  "Extra load path list for user specification.

This feature usually used for emacs new feature adding test and
designation."
  :type '(repeat directory)
  :group 'entropy/emacs-customize-group-for-emacs-extensions)

;; ***** ui theme and modeline style config
;; ****** enable initial dashboard
(defcustom entropy/emacs-enable-initial-dashboard 'rich
  "Enable entropy emacs initial dashboard instead of emacs
default one.

Valid value are 't' or 'rich', otherwise disable this
feature.

When value are either 't' or 'rich', a fancy simple splash buffer
`entropy/emacs-init-welcome-buffer-name' will startup firstly, and
then enable the rich dashbord contents when value is 'rich'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

;; ****** theme choise
(defcustom entropy/emacs-theme-options 'ujelly
  "Choice for emacs theme"
  :type 'symbol
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-solaire-themes-regex-list
  '("^doom-"
    "^atom-one-dark"
    "^spacemacs-")
  "Themes name regex matchs for solaire-mode."
  :type '(repeat string)
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

;; ****** modeline type choise
(defgroup entropy/emacs-customize-group-for-modeline nil
  "Eemacs mode-line configuration customizable group."
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

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

;; ****** display time in modeline
(defcustom entropy/emacs-display-time-modeline nil
  "Whether show the Real-time TIME in mode line, it's suggest not
set for that messy with modeline type, default to nil."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

;; ***** font setting config
(defgroup entropy/emacs-customize-group-for-eemacs-font-spec nil
  "Eemacs font specifications configuration customizable group."
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

(defcustom entropy/emacs-font-setting-enable nil
  "Enable entropy-emacs specific font setting, when its non-nil.

If its non-nil, the valid value are 'google' for using google noto
family fonts, 'sarasa' for usign sarasa-gothic family which was a
fork for google noto refer, or 'fira-code' which was better for
daily development using.

When its value is 't', then fallback its to 'google'."
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

Default size was 10, the upper limit was 15."
  :type 'integer
  :group 'entropy/emacs-customize-group-for-eemacs-font-spec)

;; ***** language envrionment config
(defgroup entropy/emacs-customize-group-for-language-environment nil
  "Eemacs language environment configuration customizable group."
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

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


;; ***** ivy framework config

(defgroup entropy/emacs-customize-group-for-ivy-mode nil
  "Eemacs ivy-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-fundametal-configuration)

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

;; ***** dictionary config
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



;; **** shell env config

(defgroup entropy/emacs-customize-group-for-ShellEnv-configuration nil
  "Eemacs shell env configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

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

;; **** eyebrowse config
(defgroup entropy/emacs-customize-group-for-eyebrowse-mode nil
  "Eemacs eyebrowse-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

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

;; **** pyim config
(defgroup entropy/emacs-customize-group-for-pyim nil
  "Eemacs PYIM configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

(defcustom entropy/emacs-enable-pyim nil
  "Enable pyim be the default pyin input method"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pyim)

(defcustom entropy/emacs-pyim-use-backend 'internal
  "The pyim backend type choosing configuration."
  :type  '(choice
           (const :tag "Native pyim backend" internal)
           (const :tag "Liberime based on librime" liberime))
  :group 'entropy/emacs-customize-group-for-pyim)

(defcustom entropy/emacs-pyim-liberime-scheme-data
  (cl-case system-type
    (gnu/linux "/usr/share/rime-data")
    (t nil))
  "The rime scheme-data directory using for liberime"
  :type '(choice
          (const nil)
          directory)
  :group 'entropy/emacs-customize-group-for-pyim)

(defcustom entropy/emacs-pyim-liberime-cache-dir
  (expand-file-name "pyim/rime-cache" entropy/emacs-stuffs-topdir)
  "The cache dir for liberime"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-pyim)

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

(defcustom entropy/emacs-pyim-cached-dir
  (expand-file-name "pyim/internal-cache" entropy/emacs-stuffs-topdir)
  "Set pyim cached dir, if nil use defaults setting (see
`pyim-dcache-directory')"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-pyim)

(defcustom entropy/emacs-pyim-tooltip nil
  "Setting the pyim toolitip method"
  :type '(choice
          (const :tag "Posframe (in gui)" posframe)
          (const :tag "Popup (in tui)" popup)
          (const :tag "Minibuffer (either tui or gui)" minibuffer)
          (const :tag "Automatically set" nil))
  :group 'entropy/emacs-customize-group-for-pyim)

;; **** emacs web corresponding config
(defgroup entropy/emacs-customize-group-for-web-refer nil
  "Eemacs web referred configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; ***** eww search engine set
(defcustom entropy/emacs-enable-eww-search-engine-customize t
  "Enable eww search prefix customized"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-web-refer)

(defcustom entropy/emacs-eww-search-engine "https://www.bing.com/search?q="
  "Customized eww search prefix string and the default was 'bing.cn'"
  :type 'string
  :group 'entropy/emacs-customize-group-for-web-refer)

;; ***** search-web search-engines setting
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

;; ***** personal browse-url function and varaiable
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

;; **** project search exec config
(defcustom entropy/emacs-search-program "rg"
  "The project search engine used for =entropy-emacs=.

The valid value are \"ag\" (i.e. the silver_searcher) or \"rg\"
(i.e. the ripgrep searcher), default for using ripgrep which is
the fasest way.
"
  :type '(choice
          (const :tag "Ripgrep" "rg")
          (const :tag "The silver searcher" "ag"))
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; **** ibuffer config
(defcustom entropy/emacs-enable-ibuffer-projectitle nil
  "Enable ibuffer-projectitle in ibuffer

Note: ibuffer-projectitle will cause the performance debug.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; **** desktop save config
(defcustom entropy/emacs-desktop-enable nil
  "Enable desktop-save-mode and persistent scratch buffer"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; **** tab-width config
(defcustom entropy/emacs-custom-tab-enable nil
  "Enable indent-tab-mode"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-basic-configuration)

(defcustom entropy/emacs-custom-tab-width 4
  "Set the customized tab width"
  :type 'integer
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; **** init frame position config
(defgroup entropy/emacs-customize-group-for-initial-position nil
  "Eemacs emacs initial position configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

(defcustom entropy/emacs-init-fpos-enable nil
  "Whether set init emacs position by `entropy/emacs-set-frame-position'."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-initial-position)

(defcustom entropy/emacs-init-frame-width-scale 0.6
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

;; **** backgroud transparent config
(defgroup entropy/emacs-customize-group-for-transparent-backgroud nil
  "Eemacs emacs frame background transparent configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

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

;; **** gnus config
(defgroup entropy/emacs-customize-group-for-gnus nil
  "Eemacs GNUS configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

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

;; **** mpd config
(defgroup entropy/emacs-customize-group-for-mpd nil
  "Eemacs MPD (music player daemon) configuration customizable group."
  :group 'entropy/emacs-customize-group-for-basic-configuration)

(defcustom entropy/emacs-mpd-host-url "127.0.0.1"
  "Mpd host url defautl to 'localhost'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-mpd)

(defcustom entropy/emacs-mpd-host-port "9688"
  "Mpd host port defautl to '9688'."
  :type 'string
  :group 'entropy/emacs-customize-group-for-mpd)

;; **** start with config
(defcustom entropy/emacs-startwith-apps nil
  "The external apps for entropy-emacs start with.

This variable forms as one alist of each element's car was the
process-name and the cdr was the executable full path string for
just it's name."
  :type '(choice (const nil)
                 (repeat
                  (cons (string :tag "Process name")
                        (file :tag "Process executable file"))))
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; *** Pdumper
(defgroup entropy/emacs-customize-group-for-pdumper nil
  "Eemacs portable dump configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

(defcustom entropy/emacs-do-pdumper-in-X t
  "Whether did pdumper for gui prot."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pdumper)

(defcustom entropy/emacs-fall-love-with-pdumper nil
  "The emacs running type indication for pdumper."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-pdumper)

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

For eemacs maintenance, all the sub-folders liek bin/, lib/ so on
are pre-defined for reversing the namespace update possible:
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
  "Ide system doc helper show idle delay"
  :type 'float
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

(defcustom entropy/emacs-ide-diagnostic-delay 0.5
  "Ide system parseer response idle delay"
  :type 'float
  :group 'entropy/emacs-customize-group-for-IDE-configuration)


;; *** language server specification
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

(add-hook 'after-init-hook
          ;; Pre checking the exists status of thus for preventing yas
          ;; error prompt from yas loading case.
          #'(lambda ()
              (unless (file-directory-p entropy/emacs-yas-dir)
                (mkdir entropy/emacs-yas-dir t))))

;; **** company mode config
(defgroup entropy/emacs-customize-group-for-company-mode nil
  "Eemacs company-mode configuration customizable group."
  :group 'entropy/emacs-customize-group-for-IDE-configuration)

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

(defcustom entropy/emacs-company-idle-delay-default 0.5
  "Default eemacs specified set for `company-idle-delay`."
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

;; **** elfeed config
(defgroup entropy/emacs-customize-group-for-elfeed nil
  "Eemacs elfeed configuration customizable group."
  :group 'entropy/emacs-customize-group-for-RSS)

(defcustom entropy/emacs-elfeed-proxyfeeds-regexp-list '()
  "Regexp for matching the feeds which needed for updating through proxy."
  :type '(choice (const nil) (repeat regexp))
  :group 'entropy/emacs-customize-group-for-elfeed)

(defcustom entropy/emacs-elfeed-url-no-proxy
  '("localhost"
    "127.0.0.1"
    "192.168.*"
    "10.*")
  "No proxy for elfeed proxy setting"
  :type '(repeat (string :tag "host url or domain wildcards"))
  :group 'entropy/emacs-customize-group-for-elfeed)

(defcustom entropy/emacs-elfeed-retrieve-proxy "127.0.0.1:1081"
  "The default proxy host domain and port concated string for
elfeed proxy setting."
  :type 'string
  :group 'entropy/emacs-customize-group-for-elfeed)

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

;; *** Specific for windows
(defgroup entropy/emacs-customize-group-for-WINDOWS nil
  "Eemacs Windows platform specifications configuration customizable group."
  :group 'entropy-emacs-customize-top-group)

;; **** w32 ime config
(defcustom entropy/emacs-win-init-ime-enable nil
  "Enable win32 IME bug fix maybe detection at startup (fix
around of the bug of w32-ime)."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; **** eemacs wsl config
(defcustom entropy/emacs-wsl-enable nil
  "Set whether you want to use =eemacs-wsl=, so that variable
`entropy/emacs-wsl-apps' will be used.

=eemacs-wsl= is a abstract of the *nix emulator for emacs on
windows system, it brings up more benefits for windows emacs user
to experience as the mostly as for *nix platform.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-wsl-apps "c:/Msys2/usr/bin/"
  "Set the baisic =eemacs-wsl= apps hosted path for basic
shell-command using which also used in shell-buffer.

That we suggested using the *nix emulator for windows i.e. Msys2
as the apps hosted system, and set it 'usr/bin' path as the value
of this variable. Defaulty value is \"c:/Msys2/usr/bin/\".

For minimally use, you can obtain a minimal Msys2 env from
git-for-windows-portable (https://git-scm.com/download/win).

NOTE: this variable just be used when `entropy/emacs-wsl-enable'
init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)


(defcustom entropy/emacs-wsl-enable-extra nil
  "Enable extra =eemacs-wsl= apps usage then variable
`entropy/emacs-wsl-apps-extra' will be used.

This ON-OFF variable are setted for follow occurrence:

    If you setting `entropy/emacs-wsl-apps' to
    'git-for-windows-portable' subroutine path which just
    contained the basic UNIX-LIKE commands that doesn't contianed
    commands like 'man' and 'tree' or sth else, you want to using
    them as well in current emacs session.

See customized variable `entropy/emacs-wsl-apps-extra' for
details.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)


(defcustom entropy/emacs-wsl-apps-extra "c:/Msys2/"
  "Set the extra wsl apps path, used for some other subprocess of
emacs called lying on `exec-path' of *nix utils which not include
in mainly =eemacs-wsl= apps path `entropy/emacs-wsl-apps'.

And this must using the type for the root of utils path
(i.e. which we can search the folder stucter of 'usr/bin' under
this root directly), like if you using msys2 , you must set this
variable to like:

\"c:/msys2/\"

NOTE: this variable just be used when
`entropy/emacs-wsl-enable-extra' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** windows git portable setting
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
\"git\" command in `entrop/emacs-wsl-apps' when
`entropy/emacs-wsl-enable' was non-nil.

NOTE: this variable just be used when
`entropy/emacs-git-portable-enable' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; **** adding path for emacs built 'bin' folder
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

;; **** wsl terminal setting
(defcustom entropy/emacs-wsl-terminal-enable nil
  "Enable external *nix terminal emulator on windows platform and
then variable `entropy/emacs-wsl-terminal' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-WINDOWS)

(defcustom entropy/emacs-wsl-terminal "c:/Msys2/msys2_shell.cmd"
  "Set the default *nix terminal emulator applictions, we recommend
to use Msys2's main terminal as that as, defaultly use the cmd
batch of Msys2 as \"c:/Msys2/msys2_shell.cmd\" which has the most
functional has for.

Other suggestion list:

- =git-bash.exe= of git-for-windows
- =mingw32/64.exe= of both Msys2 or git-for-windows

Any other self specification are not with warranty.

NOTE: this variable just be used when
`entropy/emacs-wsl-terminal-enable' init with non-nil value."
  :type 'file
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; **** enable fakecygpty
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

;; **** w32 portable kits
(defgroup entropy/emacs-customize-group-for-w32-portable-kits nil
  "Eemacs w32 portable environment configuration customizable group."
  :group 'entropy/emacs-customize-group-for-WINDOWS)

;; ***** enable portable mingw
(defcustom entropy/emacs-win-portable-mingw-enable nil
  "Enable mingw portable release usage for emacs in windows and
then variable `entropy/emacs-win-portable-mingw-path' will be
used.

As compare to =eemacs-wsl= apps `entropy/emacs-wsl-apps' does
for, mingw was a *nix development toolchain emulator not at the
usage aspect, so that like clang toolchain can be used for emacs
in portable way."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-mingw-path "c:/Mingw64/bin/"
  "Setting the path of portable mingw for windows plattform.

If your have set the `entropy/emacs-wsl-apps' so as on Msys2
release, you may easily set its mingw path e.g
\"c:/Msys2/mingw32/64\" for this vairable, or you can download
fresh new mingw release from http://www.mingw.org/.

NOTE: this variable just be used when
`entropy/emacs-win-portable-mingw-enable' init with non-nil value.
"
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable clang
(defcustom entropy/emacs-win-portable-clang-enable nil
  "Enable clang windows port usage in portable way for emacs on
windows and then variable `entropy/emacs-win-portable-clang-path'
will be used.

In cases that when you has set
`entropy/emacs-win-portable-mingw-path', you do not need to open
this turn on-off, which you can download clang for windows within
mingw it self directly."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-basic-configuration)

(defcustom entropy/emacs-win-portable-clang-path "c:/Clang/bin"
  "Path for portable clang for windows plattform.

Its must be the \"bin\" subfolder path in the clang winport root
folder, defaultly set to \"c:/Clang/bin\".

You can download it from https://releases.llvm.org/download.html.

NOTE: this variable just be used when
`entropy/emacs-win-portable-clang-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-basic-configuration)

;; ***** enable portable texlive
(defcustom entropy/emacs-win-portable-texlive-enable nil
  "Enable texlive portable release for emacs in windows and then
variable `entropy/emacs-win-portable-texlive-path' will be used.

In the case of that you've set the
`entropy/emacs-win-portable-mingw-path', you do not need to turn
on this variable which you can directly install texlive for
windows within mingw release.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-texlive-path "c:/Texlive/bin"
  "Portable texlive winport release archive directory /bin/
path.

You can download it from https://www.tug.org/texlive/acquire-netinstall.html

NOTE: this varialbe just used when
`entropy/emacs-win-portable-texlive-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable grep
(defcustom entropy/emacs-win-portable-grep-enable nil
  "Enable Gnu-grep for windows portable release for emacs in
windows platform, and then variable
`entropy/emacs-win-portable-grep-path' will be used.

In the case of that you've set the `entropy/emacs-wsl-apps', you
do not need to turn on this variable which almostly exists there
already.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-grep-path "c:/Gnu-grep/bin"
  "Portable grep winport release archive directory /bin/ path.

You can download the release from
https://sourceforge.net/projects/ezwinports/files/ which obtained
from windows Chocholatey project.

NOTE: this variable just be used when
`entropy/emacs-win-portable-grep-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable ag
(defcustom entropy/emacs-win-portable-ag-enable nil
  "Enable silver_searcher portable winport release for emacs in
windows platform, and then variable
`entropy/emacs-win-portable-ag-path' will be used.

In the case of that you've set the `entropy/emacs-wsl-apps', you
do not need to turn on this variable which you can install it
directly whithin it.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-ag-path "c:/Ag/"
  "Portable silver_searcher winport release archive /bin/ path.

You can download it from
https://github.com/k-takata/the_silver_searcher-win32/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-ag-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable rg
(defcustom entropy/emacs-win-portable-rg-enable nil
  "Enable ripgrep portable winport release for emacs in windows
platform, and then variable `entropy/emacs-win-portable-rg-path'
will be used.

In the case of that you've set the `entropy/emacs-wsl-apps', you
do not need to turn on this variable which you can install it
directly whithin it.
"
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-rg-path "c:/Ripgrep/"
  "Portable ripgrep winport release archive /bin/ path.

You can download it from
https://github.com/BurntSushi/ripgrep/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-rg-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable cmder
(defcustom entropy/emacs-Cmder-enable nil
  "Enable Cmder portable release usage for emacs in windows platform,
and then variable `entropy/emacs-win-Cmder-path' will be used.

Cmder is a enhanced windows CMD terminal."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-Cmder-path "c:/Cmder/bin/Cmder.exe"
  "Portable Cmder release caller exec path.

You can download Cmder from:
https://github.com/cmderdev/cmder/releases

NOTE: this variable just be used when
`entropy/emacs-Cmder-enable' init with non-nil value."
  :type 'file
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable php
(defcustom entropy/emacs-win-portable-php-enable nil
  "Enable php winport release usage for emacs in windows and then
variable `entropy/emacs-win-portable-php-path' will be used.

In which case that PHP doesn't appear to exist on Msys2 or other
windows *nix emulator official repository, so that this one as be
for. But there's no need to turn on this if upstream of thus has
been added. "
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-php-path "c:/Php/"
  "Portable php winport release executable path.

You can download it from:
https://windows.php.net/download/

NOTE: this variable just be used when
`entropy/emacs-win-portable-php-enable' init with non-nil value. "
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enbale portable pip
(defcustom entropy/emacs-win-portable-pip-enable nil
  "Enable external python pip winport portable release usage for
emacs in emacs and then variable
`entropy/emacs-win-portable-python-path' will be used.

The exist meaning for this variable is that pip always not in the
same location which python did for on windows platform, so that
you need to set it manually along with you've set
`entropy/emacs-win-portable-python-path' which has the location
for its own pip version."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-pip-path
  "c:/Winpython/python-3.7.2.amd64/Scripts"
  "External portable pip path whicn be along with the
`entropy/emacs-win-portable-python-path'.

NOTE: this variable just be used when
`entropy/emacs-win-portable-pip-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable python
(defcustom entropy/emacs-win-portable-python-enable nil
  "Enable python winport release usage for emacs in windows and
then variable `entropy/emacs-win-portable-python-path' will be
used.

In the case that you've set the `entropy/emacs-wsl-apps', there's
no need to enable this so that you can directly install the
python from that =eemacs-wsl= env."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-python-path
  "c:/Winpython/python-3.7.2.amd64/"
  "Portable python winport /bin/ path.

You can download it from https://winpython.github.io/

NOTE: this variable just be used when
`entropy/emacs-win-portable-python-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable nodejs
(defcustom entropy/emacs-win-portable-nodejs-enable nil
  "Enable nodejs winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-nodejs-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the nodejs archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-wsl=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-nodejs-path "c:/Nodejs/"
  "Portable python winport release archive root path.

You can download it from https://nodejs.org/en/download/

NOTE: this variable just be used when
`entropy/emacs-win-portable-nodejs-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable opencc
(defcustom entropy/emacs-win-portable-opencc-enable nil
  "Enable opencc winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-opencc-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the opencc archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-wsl=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-opencc-path "c:/Opencc/bin/"
  "Portable opencc winport release /bin/ path.

You can download it from https://github.com/BYVoid/OpenCC/wiki/Download

NOTE: this variable just be used when
`entropy/emacs-win-portable-opencc-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable pandoc
(defcustom entropy/emacs-win-portable-pandoc-enable nil
  "Enable pandoc winport portable release usage for emacs in
windows and then variable
`entropy/emacs-win-portable-pandoc-path' will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the pandoc archive for as, or if
does otherwise that you do not need to turn on this when you
enabled =eemacs-wsl=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-pandoc-path "c:/Pandoc/"
  "Portable pandoc winport release archive root path.

You can download it from https://github.com/jgm/pandoc/releases

NOTE: this variable just be used when
`entropy/emacs-win-portable-pandoc-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enbale portable jdk
(defcustom entropy/emacs-win-portable-jdk-enable nil
  "Enable java-jdk winport portable release usage for emacs in
windows and then variable `entropy/emacs-win-portable-jdk-path'
will be used.

The exist meaning for this variable is that the *nix emulator for
windows like Msys2 doesn't has the java-jdk archive for as, or if
does otherwise that you do not need to turn on this, when you
enabled =eemacs-wsl=."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-jdk-path "c:/Openjdk/bin"
  "Portable java-jdk winport release /bin/ path.

You can download it from: https://jdk.java.net/

NOTE: this variable just be used when
`entropy/emacs-win-portable-jdk-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable zeal
(defcustom entropy/emacs-win-portable-zeal-enable 'nil
  "Enable Zealdoc usage for emacs in windows and then variable
`entropy/emacs-win-portable-zeal-path' will be used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-zeal-path "c:/Zeal-Portable/"
  "Portable Zealdoc winport release archive root path.

You can download it from http://zealdocs.org/

NOTE: this variable just be used when
`entropy/emacs-win-portable-zeal-enable' init with non-nil value."
  :type 'directory
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

;; ***** enable portable putty
(defcustom entropy/emacs-win-portable-putty-enable nil
  "Enable putty portable for emacs in windows for provide the
putty tramp method instead of the lag ssh one on windows platform
and then variable `entropy/emacs-win-portable-putty-path' will be
used."
  :type 'boolean
  :group 'entropy/emacs-customize-group-for-w32-portable-kits)

(defcustom entropy/emacs-win-portable-putty-path "c:/Putty/"
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

(defvar entropy/emacs-core-doc-file-archives-plist
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
      entropy/emacs-site-lisp-path))
  "A plist stored variable number of archive formats of eemacs
core document file.")

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
=entropy-emacs-startup-trail-hook=.

Also see `entropy/emacs-run-startup-end-hook' for restriction
description.")

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
value where there's no published for any of the insternal entropy
emacs specified environment variable references APIs, this is the
only one for thus."
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

;; ***** find file path use absolute path
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

;; ***** unreadable file detection

(defvar __unreadable-file-long-threshold 700)
(defvar __unreadable-file-judge-prepares
  (cond ((and (not (eq system-type 'windows-nt))
              (not (eq system-type 'ms-dos))
              (executable-find "wc"))
         (lambda (filename)
           (ignore-errors
             (< __unreadable-file-long-threshold
                (string-to-number
                 (car
                  (split-string
                   (shell-command-to-string
                    (format "wc -L '%s'" filename))
                   " ")))))))
        ;; FIXME: add window 'wc -L' equivalent to get more performance
        (t
         ;; For backwards compatible we use `so-long.el' as the judger
         ;; but with low performance maybe?
         (require 'so-long)
         (lambda (filename)
           (let ((inhibit-read-only t)
                 (so-long-threshold __unreadable-file-long-threshold))
             (with-temp-buffer
               (ignore-errors
                 (insert-file-contents
                  filename))
               (so-long-detected-long-line-p)))))))

(defun entropy/emacs-unreadable-file-judge (filename)
  (let ((filesize (file-attribute-size
                   (file-attributes
                    filename))))
    (if (and __unreadable-file-judge-prepares
             (ignore-errors
               (<= filesize large-file-warning-threshold)))
        (funcall __unreadable-file-judge-prepares filename)
      nil)))

(defun entropy/emacs-ureadable-file-prompt (format &rest args)
  (if (yes-or-no-p (format format args))
      t
    (user-error "Abort!")))

(defun entropy/emacs-unreadable-file-advice-for-finid-file
    (orig-func &rest orig-args)
  (if __unreadable-file-judge-prepares
      (let* ((error-fmtstr
              "File '%s' is not readable e.g. its may freeze emacs, Abort!")
             (filename (car orig-args))
             (wildcards (cadr orig-args))
             (unreadable (if wildcards 'multifiles
                           (entropy/emacs-unreadable-file-judge
                            filename))))
        (cond ((null unreadable)
               (apply orig-func orig-args))
              ((eq unreadable 'multifiles)
               (let ((files (condition-case nil
                                (file-expand-wildcards filename t)
                              (error (list filename))))
                     (counts 0))
                 (dolist (file files)
                   (when (entropy/emacs-unreadable-file-judge file)
                     (cl-incf counts)
                     (warn
                      (format "[%s] %s"
                              counts
                              (format error-fmtstr file)))))
                 (when (> counts 0)
                   (entropy/emacs-ureadable-file-prompt
                    (format "There's %s file are unreadable e.g. which may freeze emacs, \
for guaranteeing the current emacs session workable, eemacs refuse this find-file action. \
Do you want to open it with messy?"
                            counts)))
                 (apply orig-func orig-args)))
              (t
               (entropy/emacs-ureadable-file-prompt
                (format "File '%s' is not readable e.g. its may freeze emacs, \
Do you want to open it with messy?"
                        filename))
               (apply orig-func orig-args))))
    (apply orig-func orig-args)))

(advice-add 'find-file
            :around
            #'entropy/emacs-unreadable-file-advice-for-finid-file)

;; *** add eemacs texinfo to info-path

(setq Info-default-directory-list
      (append (list
               (file-name-directory
                (plist-get entropy/emacs-core-doc-file-archives-plist :texinfo)))
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
                  ;; miscellanies
                  (idlwave-config-directory . "idlwave/")
                  (entropy/org-exptt-html-theme-cache-dir . "org-themes/org-html-themes")
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
