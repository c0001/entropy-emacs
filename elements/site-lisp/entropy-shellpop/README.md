# Table of Contents

1.  [entropy-shellpop.el &#x2014; popup shell buffer for transient](#orga719067)
2.  [Copyright (C) 20190829  Entropy](#orged59d29)
3.  [Commentary:](#org0f15e58)
4.  [Configuration:](#orgf70a954)
5.  [Development](#orgdcb3223)
    1.  [`shellpop-type-register` data structure](#org5017207)
    2.  [`shellpop-buffer-object` data structure](#orgaf3ba1a)
    3.  [Extensible developing](#org02236ea)
6.  [Changelog](#org8546bdb)

<a id="orga719067"></a>

# entropy-shellpop.el &#x2014; popup shell buffer for transient


<a id="orged59d29"></a>

# Copyright (C) 20190829  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac0001@gmail.com>
    
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
    
    Package-Version: 20190829
    Version:       0.1.0
    Created:       2019-08-29
    Keywords:      shell-pop, shell
    Compatibility: GNU Emacs emacs-version 26.1;
    Package-Requires: ((cl-lib "1.0") (shackle "1.0.3") (entropy-common-library "0.1.3") (vterm "0.0.1"))


<a id="org0f15e58"></a>

# Commentary:

The shell popuped feature provision what seems like the `vscode
shellpopup aspect`.

Allow `multi-shell-type` coexistence, that the mode-type of
`eshell-mode`, `shell-mode`, `term-mode` can be popuped together
with keybinding customization.

Allow `multi-shell-buffer` coexistence independently, with
completion query prompt manangement.

This package was inspired by [shell-pop-el](http://github.com/kyagi/shell-pop-el), but built based on
purely fundamental, for optimizing features' detailes and
restructed the popup feature rely on [shackle](https://github.com/wasamasa/shackle).


<a id="orgf70a954"></a>

# Configuration:

Just cloning this repo under the path sepcified on your wish, and
added it to your `load-path`, using `require` or `use-package` to
manage the configuration for this by calling the main function
`entropy/shellpop-start`. Traditionally minor snippet as:

    (require 'entropy-shellpop)
    (entropy/shellpop-start)

The internal builtin shell popup types are:

-   for eshell: `<f9>`
-   for ansi-term: `<f10>`
-   for vterm: `<f12>`

You may customize variable `entropy/shellpop-pop-types` for more
specification, see its doc-string for more.


<a id="orgdcb3223"></a>

# Development

For PR and extented aiming for, `entropy-shellpop` provide its own
code context map, a illustration for thus as below sections:


<a id="org5017207"></a>

## `shellpop-type-register` data structure

This is the global host var-type designed for 'entropy-shellpop' to
get the overview for all shellpop buffers in current emacs session of
arbitrary shell-type (yes `entropy-shellpop` was multi-shell coexist
possibly). It group the shellpop buffers into two-step tree, the top
was referred to the `shell-type-name` and the subtree was for index of
those buffers, that:

                              =========================================
                                 shellpop type register  illustration
                              =========================================
    
    
                                     +--------------------------+
                                     |  shellpop-type-register  |
                                     +-------------+------------+
                                 ---------/        |        \---------
               eshell  ---------/             ansi | term             \---------  shell
           +----------/--+                  +------v------+                  +--\----------+
           | shelltype 1 |                  | shelltype 1 |                  | shelltype 1 |
           +------+------+                  +------+------+                  +------+------+
                  |                                |                                |
        +---------+---------+            +---------+---------+            +---------+---------+
        |         |         |            |         |         |            |         |         |
        v         v         v            v         v         v            v         v         v
    buffer 0   buffer 1  buffer 2    buffer 0   buffer 1  buffer 2    buffer 0   buffer 1  buffer 2
        ^                                          ^                                          ^
        |                                          |                                          |
        |                                          |                                          |
     pointer                                    pointer                                    pointer

With the illustration shown above, `shellpop-type-register` also has
the **pointer** to indicate which shellpop buffer of specific shell-type
are used currently, this **pointer** was used to briefly popping out the
shellpop buffer without items chosen operation.

Var `entropy/shellpop--type-register` was one implementation instance
used for thus, its a alist which key was the `shellpop-type-name` a
string, and the cdr was the plist whose slots are:

-   `:type-func`
    
    This slot hosts the one plist to show the current shell-type caller
    and its core function.
    
    1.  `:core`
        
        Each shellpop-type has one underline function mainly for showing
        the shellpop buffer according to this shell-type
        specification. This slot was on that.
    
    2.  `:interact`
        
        The main caller for this shellpop-type, a function with
        interacive features.

-   `:indexs`
    
    Alist to represent the current shellpop-type shellpop buffers,
    formed as
    
        '((0  . tag-name_0) (1 . tag-name_1) (2 . tag-name_2))'
    
    **tag-name** was the human readable description for its shellpop
    buffer.

-   `:pointer`
    
    **Interger** to indicate which shellpop buffer of this shellpop-type
    is used currently.


<a id="orgaf3ba1a"></a>

## `shellpop-buffer-object` data structure

As that metioned for section for **shellpop-type-register**, each
shellpop buffer of one shellpop-type was mapped in one indexed alist,
thus for term, the index space can be unlimited restricted by the
emacs's threshold for integer. Says in the other hand, in imagination
case, there's infinite shellpop buffers exists in the same time,
exists or inexists as the top level concept.

Each shellpop buffer registed in `entropy/shellpop--type-register` was
a index with its description, but without any status recorded, and for
that the register were not background upate in real-time as a service
running for watching as 'watch-dog'. For thus, `entropy-sdcv` gives a
shellpop buffer status checking probe function
`entropy/shellpop--get-type-buffer-obj`.

The probe function recieve two arguments i.e. the `shellpop-type-name`
and the buffer-index (ps: optionally), return one plist strucured of
`shellpop-buffer-object` struct. This data structure stored the
various buffer status information, include:

1.  Whether this index of buffer was exists, key `:isnew` indicated
    that.
2.  Whether the indexed exists buffer shown in the current frame, key
    `:activep` indicated that.
3.  The index of this shellpop buffer, key `:index` indicated that.
4.  The buffer-naem of the indexed shellpop buffer, key `:buffer-name`
    indicated that.


<a id="org02236ea"></a>

## Extensible developing

As for compability case thought for, I think the
`shellpop-type-register` and `shellpop-buffer-object` provision was
enoughly for you to write some tools to enhance the shellpop behavior,
and given the rich way to experience thus. Just do with your flying
mind.


<a id="org8546bdb"></a>

# Changelog

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-08 Wed] </span></span> Add support for \`vterm\`

-   <span class="timestamp-wrapper"><span class="timestamp">[2019-11-13 Wed] </span></span> **v0.1.0** release out.
    
    The first release of `entropy-sdcv`


<a id="org9753eab"></a>

