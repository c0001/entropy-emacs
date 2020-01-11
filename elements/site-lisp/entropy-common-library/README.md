
- [entropy-common-library.el &#x2014; Common library for entropy-emacs elisp programming](#orgb2edc21)
- [Copyright (C) 20190911  Entropy](#org6d6e66d)
- [Commentary:](#org7d82ef8)
  - [Requirements](#org38897e7)
- [Configuration](#org61e889d)
- [Change log:](#org98f56eb)


<a id="orgb2edc21"></a>

# entropy-common-library.el &#x2014; Common library for entropy-emacs elisp programming


<a id="org6d6e66d"></a>

# Copyright (C) 20190911  Entropy

    Author:           Entropy <bmsac0001@gmail.com>
    Maintainer:       Entropy <bmsac001@gmail.com>
    URL:              https://github.com/c0001/entropy-common-library
    Package-Version:  0.1.4
    Created:          2018-10-08
    Package-Requires: ((emacs "25") (dash "2.16.0") (ivy "0.12.0"))
    
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


<a id="org7d82ef8"></a>

# Commentary:

Common elisp library for [entropy-emacs](https://github.com/c0001/entropy-emacs).

This package also was the basic dependency for all 'entropy-' ethnic emacs extension, used for dealing with sets of basic emacs internal basic func's improvement or the public functional extra specified as.

These specified funcs group seperated as groups of:

-   elisp data refer
-   file and dir refer
-   window and buffer refer
-   ivy completion framwork extension
-   mischellaneous and more


<a id="org38897e7"></a>

## Requirements

-   dash (melpa install)
-   cl(emacs built-in with already)
-   ivy (melpa install)

There's just these three library required for compiling with, simplify and easy to use.


<a id="org61e889d"></a>

# Configuration

Just requiring with, do not using lazy laoding of it because of that all the funcs defined in this package are not defined with `autoload` macro denoted.


<a id="org98f56eb"></a>

# Change log:

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-11 Sat]</span></span>
    
    **version 0.1.4 release**
    
    Add nested form methods.

-   <span class="timestamp-wrapper"><span class="timestamp">[2019-11-12 Tue]</span></span>
    
    **version 0.1.3 release**
    
    Add new plist manipulate api \`entropy/cl-plist-get-rest'.

-   <span class="timestamp-wrapper"><span class="timestamp">[2019-11-11 Mon]</span></span>
    
    **version 0.1.2 release**
    
    1.  Optimize \`entropy/cl-truncate-string-with-length' to use emacs-builtin \`fill-paragraph' function.
    
    2.  Remove \`entropy/cl-truncate-single-line-string-with-length'.

-   <span class="timestamp-wrapper"><span class="timestamp">[2019-11-11 Mon]</span></span>
    
    **Version 0.1.1 release**
    
    This is the typo fixed emergency release.

-   <span class="timestamp-wrapper"><span class="timestamp">[2018-10-08 Mon]</span></span>
    
    **Versions 0.1.0 release**
    
    First release out.


<a id="org5850487"></a>

