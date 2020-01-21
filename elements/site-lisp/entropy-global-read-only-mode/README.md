
- [entropy-global-readonly-mode &#x2014; Simple global read-only mode](#orgd1b639a)
- [Copyright (C) 20200221  Entropy](#org8def3b2)
- [Commentary:](#org0ff6cfa)
  - [Requirements](#orgf231554)
  - [Installation](#org7d572ce)
  - [Configuration](#org1dff69a)
  - [Interaction](#org710f62c)
  - [Redefine functions and advices tracking](#orgbdef079)
- [Changelog:](#org56e5934)

<a id="orgd1b639a"></a>

# entropy-global-readonly-mode &#x2014; Simple global read-only mode


<a id="org8def3b2"></a>

# Copyright (C) 20200221  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           https://github.com/c0001/entropy-global-read-only-mode
    Package-Version: 0.1.0
    Created:       2018
    Compatibility: GNU Emacs 25;
    Package-Requires: ((emacs "25") (cl-lib "0.5"))
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


<a id="org0ff6cfa"></a>

# Commentary:

Whether the time you want let the buffer opening behaviour be read-only defaultly?

This package giving you the try for as.

As it's name 'global read only' meaning, the main feature concept was as that but with some custome rule base. Some buffer e.g. compiling refers can not do so unless you want to get the unexpected things out.

The rule base for is follow the below two way:

1.  Modes respective
    
    Let file start up with read-only mode follow it's buffer major mode specification.

2.  Lock files at startup for general view.
    
    Sing the way for each file opening about, but the special buffer list regexp matching for.


<a id="orgf231554"></a>

## Requirements

The only one extra melpa extension [org](https://github.com/m2ym/popwin-el/tree/95dea14c60019d6cccf9a3b33e0dec4e1f22c304) is required. Org mode utilies need treating for specially read-only setting way, thus this package will give some re-defun coding snippets for the ones member of those utilies. But all the re-defun procedure are just enabled when `org` loaded, there's no need to require `org` with the manually way.


<a id="org7d572ce"></a>

## Installation

Download main [source file](entropy-global-read-only-mode.el) to your load-path.


<a id="org1dff69a"></a>

## Configuration

The based-rule set mentioned above was given by the customize variable `entropy/grom-readonly-type` which gives list of valid internal string type value for as:

-   "modes" :
    
    Initializing read-only type for the major-modes list in `entropy/grom-mode-list` and it's default value is:
    
        emacs-lisp-mode-hook
        c-mode-hook
        php-mode-hook
        web-mode-hook
        python-mode-hook
        js2-mode-hook
        css-mode-hook
        org-mode-hook
        json-mode-hook
        markdown-mode-hook
        bat-mode-hook
        text-mode-hook
    
    This variable was customized, you may want to specified it along with your own benefit.

-   "all" :
    
    Initialize all file opening read-only type based on the wide rule set of the buffer name filters `entropy/grom-find-file-except-bfregexp-list`.

You can select one of them be the global-read-only-type for as.

The `use-packge` configure management type demo as:

```emacs-lisp
(use-package entropy-global-read-only-mode
  :ensure nil
  :load-path "path-to-your-load-path"
  :commands (entropy-grom-mode)
  :init (add-hook 'after-init-hook #'entropy-grom-mode))
```


<a id="org710f62c"></a>

## Interaction

-   Function: `entropy-grom-mode`
    
    Mainly global read only mode enable or disable function. Enabling obeying the rule set `entropy/grom-readonly-type`.

-   Function: `entropy/grom-toggle-read-only`
    
    When `entropy-grom-mode` was non-nil (enabled `entropy-grom-mode` status), toggle global buffers read-only status in `buffer-list` basic on the buffer name regexp matching regexp rule set `entropy/grom-toggle-except-bfregexp-list`. Rule set list was customized variable, you can set it by your specification, but suggested using it's default value.

-   Function: `entropy/grom-read-only-buffer`
    
    Quickly lock current buffer or the otherwise as the emacs internal func `read-only-mode` but with the comfirmation.

-   Function: `entropy/grom-quick-readonly-global`
    
    Quickly lock all active buffers using the rule set of func `entropy/grom-toggle-read-only`.


<a id="orgbdef079"></a>

## Redefine functions and advices tracking

There's some necessary case for redefining some package refered function when value of `entropy/grom-readonly-type` was "all", the majority occurrence one of them is that they operated buffer without buffer read-only status checking, thus they thrown out errors of unexcept process interrupted.

Til now in this package, all redefined function are all the utilities of `org-mode`. Most of org buffer operation are not checking the buffer locked status and for the unlocking automatically way.Thus, the redefined core reason is to embed the unlock codes into them respectively. Below are the redefined org apis list:

| Redefine Function            | Functional                                                  |
|---------------------------- |----------------------------------------------------------- |
| `org-capture-place-template` | Insert the template at the target location                  |
| `org-datetree--find-create`  | Find the datetree matched by REGEX for YEAR, MONTH, or DAY. |

Exception with using redefines for utilities func increasing when type "all", I prefer to using func-advice (internal mechnism `advice-add`) to be as that does, it's safety and without the worries for compacting for utilites upgrading. OFC, below advices for individual ones shown as the table:

| Advice                                     | Ad-Type   | Function              |
|------------------------------------------ |--------- |--------------------- |
| `entropy/grom-agenda-unlock-current-entry` | `:before` | `org-agenda-todo`     |
|                                            |           | `org-agenda-add-note` |
|                                            |           | `org-add-log-note`    |
| `entropy/grom-agenda-lock-current-entry`   | `:after`  | `org-agenda-todo`     |
|                                            |           | `org-store-log-note`  |

The defination won't be recovered when disable `entropy/grom-mode`, but those advice, funcs `entropy/grom-org-setoff` gives the way for.


<a id="org56e5934"></a>

# Changelog:

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-02-21 Fri] </span></span> disable prompt for unlock buffer
    
    "for simply and quickly toggle"

-   <span class="timestamp-wrapper"><span class="timestamp">[2018-08-01 Wed] </span></span> version 0.1.0 release


<a id="orge99b84a"></a>

