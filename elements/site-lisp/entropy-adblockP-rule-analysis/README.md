# Table of Contents

1.  [Copyright (C) 20190530  Entropy](#org56ee506)
2.  [Commentary:](#orgdb40b55)
3.  [Configuration:](#org251a3ea)
4.  [Changelog:](#org9df5450)


<a id="org56ee506"></a>

# Copyright (C) 20190530  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           https://github.com/c0001/entropy-adblockP-rule-analysis
    Package-Version: v0.1.0
    Package-Requires: ((emacs "25") (url) (memoize "1.1"))

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


<a id="orgdb40b55"></a>

# Commentary:

This package gives the ability to transfer adblock-plus simple url
rule-set to the elisp regexp string foramt.

You can see all the adblock-plus rule set document [online](https://adblockplus.org/en/filters).

For more, the transforming used to for simple pac(proxy auto
configuration) used by [Switchy-Omega](https://github.com/FelisCatus/SwitchyOmega), which rely on the rule-list
host on [GFWLIST](https://github.com/gfwlist/gfwlist).

This package was the apis orientated designation, the rules get func
`entropy/adbp-rule-get-regexp-matchs-list` and url blacklist check
func `entropy/adbp-rule-blacklist-match-url-p` are the mainly
provision.


<a id="org251a3ea"></a>

# Configuration:


<a id="org9df5450"></a>

# Changelog:

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-11-25 Wed 06:30] </span></span> Update rulesets
    -   Add \`entropy/adbp-rule-update' interaction feature.

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-03-11 Wed 10:50] </span></span> Version 0.1.0 release out


<a id="orgbd83dbf"></a>
