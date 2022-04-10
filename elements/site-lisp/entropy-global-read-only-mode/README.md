# Table of Contents

1.  [Copyright (C) 20200221  Entropy](#org4e922cb)
2.  [Commentary:](#org3f3c3be)
    1.  [Requirements](#org135dc1c)
    2.  [Installation](#orgbb1631f)
    3.  [Configuration](#org2fe48c6)
    4.  [Interaction](#orga6725c1)
    5.  [Redefine functions and advices tracking](#org8f6721d)
3.  [Changelog:](#orgb5ba046)


<a id="org4e922cb"></a>

# Copyright (C) 20200221  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           https://github.com/c0001/entropy-global-read-only-mode
    Package-Version: 0.1.2
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


<a id="org3f3c3be"></a>

# Commentary:

Whether the time you want let the buffer opening behaviour be
read-only defaultly?

This package giving you the try for as.

As it's name 'global read only' meaning, the main feature concept was
as that but with some custome rule base. Some buffer
e.g. compiling refers can not do so unless you want to get the
unexpected things out.

The rule base for is follow the below two way:

1.  Modes respective

    Let file start up with read-only mode follow it's buffer major mode
    specification.

2.  Lock files at startup for general view.

    Single the way for each file opening about, but the special
    buffer list regexp matching for.


<a id="org135dc1c"></a>

## Requirements

The only one extra melpa extension [org](https://orgmode.org/) is required. Org mode utilies
need treating for specially read-only setting way, thus this package
will give some re-defun coding snippets for the ones member of those
utilies. But all the re-defun procedure are just enabled when `org`
loaded, there's no need to require `org` with the manually way.


<a id="orgbb1631f"></a>

## Installation

Download main [source file](entropy-global-read-only-mode.el) to your load-path and `require` it as
the most easy way.


<a id="org2fe48c6"></a>

## Configuration

The based-rule set mentioned above was given by the customize variable
`entropy/grom-readonly-type` which gives list of valid internal string
type value for as:

-   "modes" :

    Initializing read-only type for the major-modes list in
    `entropy/grom-mode-hooks-list` and it's default value is:

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

    This variable was customized, you may want to specified it along
    with your own benefit.

-   "all" :

    Initialize all file opening read-only type based on the wide
    rule set of the buffer name filters
    `entropy/grom-customizable-special-buffer-name-regexp-list`. Further
    more, there's also an non-special exception option
    `entropy/grom-customizable-nonspecial-buffer-name-regexp-list`
    which let you add some exclusion that the special sepcification
    wild included.

You can select one of them be the global-read-only-type for as.

The `use-packge` configure management type demo as:

    (use-package entropy-global-read-only-mode
      :ensure nil
      :load-path "path-to-your-load-path"
      :commands (entropy-grom-mode)
      :init (add-hook 'after-init-hook #'entropy-grom-mode))


<a id="orga6725c1"></a>

## Interaction

-   Function: `entropy-grom-mode`

    Mainly global read only mode enable or disable function. Enabling
    obeying the rule set `entropy/grom-readonly-type`.

-   Function: `entropy/grom-toggle-read-only`

    Toggle global buffers read-only status in `buffer-list` basic on
    the buffer name regexp matching regexp rule set of that one is
    `entropy/grom--internal-specified-special-bfregexp-list` the basically core
    native builtin one and what you can customizable one
    `entropy/grom-customizable-special-buffer-name-regexp-list`.

-   Function: `entropy/grom-read-only-buffer`

    Quickly lock current buffer or the otherwise as the emacs internal
    func `read-only-mode` but with this package specification.

-   Function: `entropy/grom-quick-readonly-global`

    Quickly lock all active buffers using the rule set of func
    `entropy/grom-toggle-read-only`.


<a id="org8f6721d"></a>

## Redefine functions and advices tracking

There's some necessary case for redefining some packages refered
functions, the majority occurrence one of them is that they
operated buffer without buffer read-only status checking, thus
they thrown out errors of unexpect process interrupted.

Til now in this package, all redefined function are all the
utilities of `org-mode`. Most of org buffer operation are not
checking the buffer locked status and for the unlocking
automatically way.Thus, the redefined core reason is to embed the
unlock codes into them respectively. The defination will be
recovered when `entropy/grom-mode` disabled.


<a id="orgb5ba046"></a>

# Changelog:

-   <span class="timestamp-wrapper"><span class="timestamp">[2022-04-10 Sun 17:48] </span></span> Bug fix

    For now we will auto detect the \`grep' like buffer which commonly
    use \`wgrep' to handle the read-only, and use \`wgrep' refer
    commands simulate the readonly operations since the commonly
    \`read-only-mode' is not suitable for that buffers and will
    corrupt the emacs-sessioin, e.g. incorrectly unlock with user
    modification will cause 'Text is readonly' error cover whole
    emacs session.

    Others:

    1.  \`entropy/grom&#x2013;buffer-special-p' is migrate to
        \`entropy/grom-buffer-special-p' which be an exposed API.

    2.  Refer to the main bug fix, command
        \`entropy-global-read-only-mode.el' also check the special
        buffer when unlock operation.

-   <span class="timestamp-wrapper"><span class="timestamp">[2021-01-29 Fri 23:34] </span></span> Support org patch to "modes" grom type

    Others:

    -   Require features before patching them instead of autoloads with
        \`eval-after-load' so that we can disable it from patcher
        register directly while disable grom-mode.

-   <span class="timestamp-wrapper"><span class="timestamp">[2021-01-11 Mon 19:18] </span></span> Use internal readonly routine
    \`entropy/grom&#x2013;readonly-1&0' instead of commonly used
    \`read-only-mode' to have more condition cases filter.

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-06-24 Wed 13:51] </span></span> Add nonspecial customizable option and context update

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-05-29 Fri 20:13] </span></span> Optimize namespace
    -   Make internal librariese follow conventions by emacs
        non-provided named specification rule.

    -   Fix logical bug.

    -   Add treemacs-persist file match rule detection.

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-04-04 Sat 08:05] </span></span> Bump to version 0.1.1
    1.  Subroutines mechnism update

    2.  Bug fix

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-02-21 Fri] </span></span> disable prompt for unlock buffer

    "for simply and quickly toggle"

-   <span class="timestamp-wrapper"><span class="timestamp">[2018-08-01 Wed] </span></span> version 0.1.0 release


<a id="org8cf9f58"></a>
