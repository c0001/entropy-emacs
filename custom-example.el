;;; custom-example.el --- entropy-emacs customized configuration introduction
;;
;; * Copyright (C) 20190524  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/custom-example.el
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
;; 
;; 
;; * Configuration:
;; 
;; configuration
;; 
;; * Code:

;; ** Basic

;; *** Language environment setting

(setq entropy/emacs-custom-language-environment-enable t)
(setq entropy/emacs-language-environment "Chinese-GBK")

;; *** User info

(setq entropy/emacs-fancy-splash-logo nil)
(setq entropy/emacs-user-full-name "user name")
(setq entropy/emacs-user-mail-address "mail address")


;; *** Package-archives setting and bench mark init state

(setq entropy/emacs-package-archive-repo 'tuna)
(setq entropy/emacs-initialize-benchmark-enabled t)

;; *** Initial frame setting

(setq entropy/emacs-init-frame-width-scale 0.5)
(setq entropy/emacs-init-frame-height-scale 0.935)
(setq entropy/emacs-init-loop-alpha nil)

;; *** Projectitle setting

;; (setq entropy/emacs-enable-ibuffer-projectitle t)


;; *** emms setting

;; (setq entropy/emacs-use-emms-mode-line t)                                      ; setting whether to active the emms mode line (t or nil)


;; *** tab-width setting

(progn
  ;; (setq entropy/emacs-custom-tab-enable t)                                    ; Enable global-indent-tab-mode
  ;; (setq entropy/emacs-custom-tab-width 8)                                     ; Set default tab width
  )



;; *** fill paragraph column width
;; (setq entropy/emacs-fill-paragraph-width 66)  ;setting fill column width to 66 (the standard type over international)
;; *** company-mode and yasnippet setting

;; (setq company-idle-delay nil)                                           ; set company-mode idle time to nil for manually invoking it
;; (setq entropy/emacs-yas-dir "~/Documents/snippet")                            ; set personal snippet dirs
;; (setq entropy/emacs-company-lsp t)                                            ; toggle with comapny-lsp instead of traditional company-backend's mechanism
;; (setq entropy/emacs-company-posframe-mode t)                                  ; toggle with company-posframe instead of original company-tooltip show method



;; *** emacs-init-looklike setting

;; (setq entropy/emacs-theme-options 'doom-nova)                                 ; set theme you want to use and you can see the
                                                                                 ; entropy emacs theme list by `M-x customize-themes' 
                                                                                 ; and the default theme was 'doom-one'


;; (setq entropy/emacs-modeline-style "origin")                                  ; chosen modeline style type, you can see the variable doc for chosen value details.

;; (setq entropy/emacs-display-time-modeline t)                                  ; display time in modeline

;; *** desktop save setting

;; Enable desktop-mode and persistant scrach-buffer mode ,the first can restore you last frame &
;; workspace setting in emacs like desktop in windows 10.
;; 
;; (setq entropy/emacs-desktop-enable t)


;; *** eyebrowse new workspace init buffer

;; Enable personal function for creating new workspace of eyebrowse
;; For example:
;;   you can write one function for open one specific file for eyebrowse to init one new workspace，
;;   like:
;;     (defun open-peronal-file ()
;;       (interactive)
;;       (find-file "c:/fiction/Les_miserables.txt")
;;     )
;; And then symbol it to the variable `entropy/emacs-eyebrowse-new-workspace-init-function' like below
(progn
  ;; This area you can create the personal function:
  ;; ----------------------------------------------+
  ;;                                               |
  ;;                                               |
  ;;                                               |
  ;; ----------------------------------------------+
  ;; (setq entropy/emacs-enable-eyebrowse-new-workspace-init-function t)
  ;; (setq entropy/emacs-eyebrowse-new-workspace-init-function 'open-personal-file)
  )


;; *** google translate

;; For google translate
;;    *Notic:* the default variable was 't', If you are in the foreign country, Congratulations! You
;;    must to set it to nil because you are free in 'WIDE WORD WEB'.
;; (setq entropy/emacs-google-translate-toggle-patched-in-china nil)



;; *** eww search engine

;; Customize eww search engine
;; (progn
;;   (setq entropy/emacs-eww-search-engine-customize t)
;;   (setq entropy/emacs-eww-search-engine "https://www.google.com/search?q="))

;; **** search-web engine
(progn
  ;; (setq entropy/emacs-search-web-engines-internal '(("baidu" "http://www.baidu.com/s?wd=%s" In-Emacs)))
  ;; (setq entropy/emacs-search-web-engines-external '(("baidu" "http://www.baidu.com/s?wd=%s" External)
  ;;                                                   ("google" "http://www.google.com/search?q=%s")))
  )



;; *** set fonts

(progn
  ;; (setq entropy/emacs-font-face-default "Source Code Pro")                    ; use 'Source Code Pro' by default
  ;; (setq entropy/emacs-font-chinese "楷体")                                    ; setting han font
  ;; (setq entropy/emacs-font-size-default 12)                                   ; setting defualt font size for face-attribute
  (setq entropy/emacs-font-setting-enable nil)                                   ; disable entropy font setting for all
  ;; -----------------------------------
  ;; 
  ;; when setting upon variable with 'nil' Add you own font setting here
  ;; 
  ;; -----------------------------------
  )


;; *** project internal search tool

;; (setq entropy/emacs-search-program "pt")                                      ; Choose the search program for using pt intead of ag


;; *** search-web setting and external default browser setting

(progn  ;; Set your portable browser to be default browser
  ;; (setq entropy/emacs-enable-personal-browse-url-function t)

  ;; (defun entropy/emacs-open-with-url (url &rest args)
  ;;   (interactive (browse-url-interactive-arg "URL: "))
  ;;   (w32-shell-execute
  ;;    "open"
  ;;    "a:/PortableApps/FirefoxPortable/FirefoxPortable.exe"
  ;;    url))
  ;; (setq entropy/emacs-browse-url-function 'entropy/emacs-open-with-url)
  )

;; ** org setting

;; *** org bullets
(progn
  ;; (setq entropy/emacs-enable-org-bullets nil)                                 ; org bullets default is on
  ;; (setq entropy/emacs-org-bullets-type "number")                              ; you can choose another one "roman"
  )


;; *** org-agenda
;; (setq entropy/emacs-org-agenda-emojify t)                                     ; enable emojify for beautify the org agenda prefix but
                                                                                 ; with performance issue for emacs

;; ** highligt setting
;;
;;    This chapter was plan to enable the highlighting setting which the first option was the on-off
;;    of whole setting.
(progn
  ;; (setq entropy/emacs-use-highlight-features t)                                     ; Add highlight package for emacs (t/nil default:nil)
                                                                                       ;;; Warning: this will cause low performance of emacs interpretering
  ;; (setq entropy/emacs-hl-highlight-indention-enable-at-startup t)
  ;; (setq entropy/emacs-hl-diff-hl-enable-at-startup t)
  ;; (setq entropy/emacs-hl-highlight-parentheses-mode-enable-at-startup t)
  ;; (setq entropy/emacs-hl-rainbow-delimiters-enable-at-startup t)
  ;; (setq entropy/emacs-hl-sysmbol-overlay-enable-at-startup t)
  ;; (setq entropy/emacs-hl-todo-enable-at-startup t)
  ;; (setq entropy/emacs-hl-whitespace-enable-at-startup t)
  )



;; ** windows specific setting
(when sys/win32p

  
  ;; *** Whether use wsl(windows subsystem linux)
  (progn ;;(setq entropy/emacs-wsl-enable t)                                     ; whether enable wsl in windows opertion system
                                                                                 ; recommened applications was git-sdk-64 which contained git-bash and msys2.
         ;;(setq entropy/emacs-wsl-apps "path-to-your-wsl-apps")
                                                                                 ; default wsl-apps path was `c:/git-portable/usr/bin'
                                                                                 ; you must install msys2 to this place and this path was
                                                                                 ; the default path setted in msys2 installer
    )

  ;; **** using gcc within wsl
  (progn
    ;; (setq entropy/emacs-win-portable-mingw-enable t)
    ;; (setq entropy/emacs-win-portable-mingw-path "c:/msys2/mingw64/bin/")                 ; using the mingw-w64 gcc and g++ "x86_64-w64-mingw32-g++/cc.exe"

    ;; ***** setting the parameter for gcc and g++  
    ;; (setq entropy/emacs-win-gcc-parameter "")
    ;; (setq entropy/emacs-win-g++-parameter
    ;;       "-static-libgcc -static-libstdc++ -Wl,-Bstatic -lstdc++ -lpthread -Wl,-Bdynamic")
    )

  ;; **** Whether use wsl terminal for instead of `cmd.exe' .
  (progn ;;(setq entropy/emacs-wsl-terminal-enable t)                            ; enable wsl terminal
    ;;(setq entropy/emacs-wsl-terminal "path-to-your-wsl-bash-applicatons")
                                                                                 ; default wsl-terminal application was `c:/git-portable/cmd/git-bash.exe' and you must
                                                                                 ; install git-for-windows-sdk first accroding to `entropy/emacs-wsl-enable' annotaton

    )
  


  ;; **** Portable git config just use in windows plattform
  (progn
    ;; (setq entropy/emacs-git-portable t)                                       ; use portable git instead of system path's git

    ;; (setq entropy/emacs-git-portable-path "c:/git-portable/cmd/")
                                                                                 ; set portable git path and it just be worked when you enable `git-portable'
                                                                                 ; for attention that you must use `/' in the end of path string
    )


  
  ;; *** Portable texlive in windows
  (progn
    ;; (setq entropy/emacs-win-portable-texlive-enable t)                        ; enable texlive in windows emacs
    ;; (setq entropy/emacs-win-portable-texlive-path "c:/texlive/bin/win32")     ; path for texlive in windows 
    )


  ;; *** Portable php in windows
  (progn
    ;; (setq entropy/emacs-win-portable-php-enable t)
    ;; (setq entropy/emacs-win-portable-php-path "your-path-for-portable-php")
    )

  
  ;; *** use python portable for enable company-anaconda
  ;;      This should not use msys2 for others wsl's python, and should use the completely windows
  ;;      port python distribution like WinPython which sitted in github and daly update frequently.
  (progn
    ;; (setq entropy/emacs-win-portable-pip-enable t)                            ; enable pip portable in windows
    ;; (setq entropy/emacs-win-portable-pip-path "your-path-for-pip-in-windows") ; set path for pip in windows
                                                                                 ; which suggested use
                                                                                 ; WinPython
                                                                                 ; `https://winpython.github.io/'

    ;; (setq entropy/emacs-win-portable-python-enable t)                         ; enable python portable in windows
    ;; (setq entropy/emacs-win-portable-python-path "C:/WinPython/")
    )

  
  ;; *** grep setting for windows
  (progn
    ;; (setq entropy/emacs-win-portable-grep-enable t)                           ; whether enable windows grep and the default path is `c:/grep/bin/'
    ;; (setq entropy/emacs-win-portable-grep-path "your-setting")
    )

  
  ;; *** ag setting for windows
  ;;
  ;; in windows ag can not to handle the cjk unicode perfectly in emacs
  (progn
    ;; (setq entropy/emacs-win-portable-ag-enable t)                             ; whether enable windows ag and the default path is `~/.emacs.d/elements/bin/ag/bin' which has been included in entropy-emacs
    ;; (setq entropy/emacs-win-portable-ag-path "your-ag-path")
    )

  
  ;; *** rg setting for windows
  ;;
  ;; rg can handle cjk character with msvc complier version but can not complete searching cjk fully
  ;; of one file content such as some line with '***'
  (progn
    ;; (setq entropy/emacs-win-portable-rg-enable t)                             ; whether enable windows ag and the default path is `~/.emacs.d/elements/bin/ag/bin' which has been included in entropy-emacs
    ;; (setq entropy/emacs-win-portable-rg-path "your-rg-path") 
    )

  
  ;; *** pt setting for windows
  ;; 
  ;; pt was the only suggested search program in windows both of searching directory and git project
  ;; because of it's good at deal with cjk codeing character for display (neithor for encoding of inputting as ag), and just handling fine wiht helm-pt
  ;; package which is instead of counsel-pt and counsel-ag

  (progn
    ;; (setq entropy/emacs-win-portable-pt-enable t)                             ; whether enable windows pt and the default path is `~/.emacs.d/elements/bin/pt/bin' which has been included in entropy-emacs
    ;; (setq entropy/emacs-win-portable-pt-path "your-pt-path")
    )

  
  ;; *** nodejs path
  ;; (setq entropy/emacs-win-portable-nodejs-enable t)
  ;; (setq entropy/emacs-win-portable-nodejs-path "path-to-your-nodejs")

  ;; *** opencc
  ;;     enable opencc portable in windows
  (progn
    ;; (setq entropy/emacs-win-portable-opencc-enable t)
    ;; (setq entropy/emacs-win-portable-opencc-path "path-to-your-opencc")
    )

  ;; *** pandoc
  ;;     enable pandoc portable for win
  (progn
    ;; (setq entropy/emacs-win-portable-pandoc-enable t)
    ;; (setq entropy/emacs-win-portable-pandoc-path "path-to-your-pandoc")
    )


  ;; *** portable JDK

  ;; using portble JDK which suggested for using the package from
  ;; `https://portableapps.com/apps/utilities/jdkportable'
  (progn
    ;; (setq entropy/emacs-win-portable-jdk-enable t)
    ;; (setq entropy/emacs-win-portable-jdk-path "path/to/portble-jdk")
    )



  ;; *** Cmder enable
  (progn
    ;; (setq entropy/emacs-Cmder-enable t)                                       ; whether enable cmder 
    ;; (setq entropy/emacs-Cmder-path "your-path-to-cmder")                      ; set the path of cmder which must set sytle like "c:/Cmder.exe"
    )

  
  ;; *** emacs lang set for windows operation system
  (progn
    ;; (setq entropy/emacs-win-env-lang-enable)                                      ; whether enable it
    ;; (setq entropy/emacs-win-env-lang-set "your-lang-setting")                     ; set you $LANG for windows which can comptable for pt program
    ))
