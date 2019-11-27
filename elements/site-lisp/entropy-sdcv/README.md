
# Table of Contents

1.  [entropy-sdcv.el &#x2014; entropy emacs sdcv front-end](#orgb146151)
2.  [Commentary:](#org3d0e680)
3.  [Development](#org9efeec1)
4.  [Chanage log](#org76d4a53)


<a id="orgb146151"></a>

# entropy-sdcv.el &#x2014; entropy emacs sdcv front-end

    Copyright (C) 20181211  Entropy
    
    Author:           Entropy <bmsac0001@gmail.com>
    Maintainer:       Entropy <bmsac001@gmail.com>
    URL:              https://github.com/c0001/entropy-sdcv
    Package-Version:  20191116.2003
    Version:          0.1.1
    Created:          2018-12-11 12:48:04
    Keywords:         sdcv
    Compatibility:    GNU Emacs 26.1;
    
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


<a id="org3d0e680"></a>

# Commentary:

A emacs-lisp implementation of dictionary client.

Name component 'sdcv' means [stardict console version](https://github.com/Dushistov/sdcv) that this
project originally was the front-end for 'sdcv', but now with
architecture re-built, it becomes one dictionary query-response
frameworks.

**Features:**

1.  [youdao-dict](https://github.com/xuchunyang/youdao-dictionary.el), [bing-dict](https://github.com/cute-jumper/bing-dict.el), [google-dict](https://github.com/atykhonov/google-translate) defaultly supported.

2.  Out-of-box, easily usage with two command
    `entropy/sdcv-search-at-point-tooltip` and
    `entropy/sdcv-search-input-adjacent`.

3.  Extensible wildly. Simple apis for add new dictionary backend
    and new displaying method.

**Quick starting:**

-   Preparation:
    
    `entropy-sdcv` have sets of built-in dict backend, but defautly
     using 'sdcv', you should put it in your `PATH`, or if you do not
     want to using it as default backend, you should picking up your
     specified value of `entropy/sdcv-default-query-backend-name` to
     one of 'youdao', 'bing' or 'google'.
    
    For sdcv usage, you need cloned your own sdcv dict database
    stored in your `~/.stardict`, and structed as folder hosted
    individually, as:
    
        --~/.stardict
          |
          |--oxford-dict
             |--oxford.dict.dz
             |--oxford.idx
             |--oxford.ifo

-   Interaction:
    
    Call command `entropy/sdcv-search-at-point-tooltip` to search
    thesaurus at current point and show it in tooltip buffer. Or if
    you want to search by manually inputting, calling
    `entropy/sdcv-search-input-adjacent` instead.
    
    And for some reason, you want to toggle dict backend, you can
    call `entropy/sdcv-toggle-backend` for thus, even for calling
    `entropy/sdcv-toggle-show-tooltip-method` to switch displaying
    type (show-type).
    
    Further more, you can enable `entropy/sdcv-autoshow-mode` to show
    translation response at point automatically with minor delay. You
    also can change 'autoshow' dict-backend or show-method with usually
    interactive method demoted above.


<a id="org9efeec1"></a>

# Development

As denoted in commentary, entropy-sdcv provides wildly extensible
ability, it makes attention in two aspect i.e. the dict type and
query response display method. We give the protocols to represent
dictionary type and the display method for rulling the
maintainability, also mainly for reducing the cost of development.

All the protocols defination are written in
`entropy-sdcv-core.el`, read its commentary for briefly
understanding.


<a id="org76d4a53"></a>

# Chanage log

2019/11/27

-   Add \`entropy/sdcv-autoshow-mode'
    -   autoshow for all builtin dict backends.
    -   fix some typo and minor bugs

2019/11/16

-   Fix bugs for face setting
    -   Api `:show-face` now require the SHOW-METHOD argument for
        funcion aspect.
    -   New api: `entropy/sdcv-core-use-face` in core library.

2019/11/06

-   Rebuilding features logic to prepare for 0.2.0 release
    -   using multi-backends instead sdcv maily framework
    -   protocols made out for specification and featue adding
    -   more more rigorous srcs splitting style.

2019/10/23

-   version v0.1.1 pop out
    -   Using new auto-gen's tooltip face render
    -   Add \`pos-tip' tooltip type.

2018/12/11

-   First release pop out v0.1.0

