
- [entropy-proxy-url &#x2014;  Url proxy for emacs eww and w3m](#org5932bca)
- [Copyright (C) 20190906  Entropy](#org07ad787)
- [Commentary:](#org97bfca1)
  - [Eww and emacs-w3m proxy stuff](#orgd1640c4)
  - [How this package working as?](#org68f4463)
  - [Methods given](#orga472ab5)
  - [Proxy reset](#orgaa89bf1)
- [Configuration:](#org6b3a7bd)
  - [Target operation advice](#orgdc65ce2)
  - [customized varaibles](#orgbaee913)
  - [The regexp rule-set list data](#org0bd5a7b)
- [Change log:](#org7a83572)

<a id="org5932bca"></a>

# entropy-proxy-url &#x2014;  Url proxy for emacs eww and w3m


<a id="org07ad787"></a>

# Copyright (C) 20190906  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           https://github.com/c0001/entropy-proxy-url/
    Package-Version: v0.1.1
    Created:       2018
    Keywords:      proxy
    Compatibility: GNU Emacs emacs-version;
    Package-Requires: ((emacs "26") (cl-lib "0.5") (eww) (w3m "20190830.742"))
    
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


<a id="org97bfca1"></a>

# Commentary:

An emacs extension trafficking url matching by proxy regxp rule-set or pull down them into whole proxy tunnel.

This package perform as the library role for the functional provision.


<a id="orgd1640c4"></a>

## Eww and emacs-w3m proxy stuff

Emacs has the ability to retrieving network data steps with internal func `url-retrieve` and it's refer url library as. Eww using it as the communication backend.

Although, there's another way for emacs to get network data througha the way of using external shell application which obtains its responsing as the render part. One as be in this case was the famous cli plain text browser [w3m](http://w3m.sourceforge.net/).

The proxy way for the internal data retrieve was based on emacs internal proxy mechanism, and the other one was based on the refer response creator's self mechanism.


<a id="org68f4463"></a>

## How this package working as?

This package taking off the proxy way through two ways for as:

1.  Regexp matching of current url retrieving.
2.  Full proxy tunnel whichever current transfer url is.

As seen as which are easy to understanding of the full proxy type,here's the description for 'regexp' way:

Each network transfer request will come with one url string as the requesting wrapper header. URL string has it's feature stored in it's domain part, the domain part can be matched by categorizing using regexp method, thus each network requesting can be filter by sets of regexp rule-set for determining whether go into the proxy tunnerl.

The regexp filter can be generized of more functional part, by matching whatever you want. The rule-set internal customized variable `entropy/proxy-url-gfw-regexp-alist` as is.

This package use the [gfwlist](https://github.com/gfwlist/gfwlist) as the default ruleset, for the original prototype for it was the simple variant of [PAC](https://en.wikipedia.org/wiki/Proxy_auto-config)(proxy auto configuration), whose syntax abided by [adblock-plus](https://adblockplus.org/) web extensions, for that reason to use this was that gfwlist was originally used for the webbrowser extensions for doing thus as what you have known for the internet charging among on CHINA. Thus for that, this package requires one `PAC` analyzer as now I use the another [entropy-emacs](https://github.com/c0001/entropy-emacs) specific package [entropy-adblock+-rule-analysis](https://github.com/c0001/entropy-adblockP-rule-analysis) to role as the gfwlist anaylizer, it will auto fetch the latest version gfwlist if possible when you current internet environment allow the connecting for that, or using the package built-in one which was the pre-fetched one, so it will not be the latest version.


<a id="orga472ab5"></a>

## Methods given

We using property list as a `PROXY-RECIPE` to given the customized way for specify the proxy subroutine.

As the focurs on, the `PROXY-RECIPE` mainly use `advice-add` to around wrappering the target underline functional commands, like `w3m-goto-url` , `url-retrieve` etc.

The `PROXY-RECIPE` slots valid for those listed below:

-   `:group-name` : a symbol to indicate the recipe name identification

-   `:advice-fors` : list of functions for be wrappered with \`entropy/proxy-url\` specification

-   `:type-source` : a symbol restored the proxy type (i.e. which described in \`entropy/proxy-url-initial-typesource')

-   `:PROXY-MECHANISM` : a symbol indicate the proxy mechanism (i.e. describe for \`entropy/proxy-url-default-proxy-server-obj')

-   `:server-host-alist` : a symbol indicate the proxy server host alist which using the same struct with \`entropy/proxy-url-default-proxy-server-obj'"

-   `:bind` : a alist which the each car of the element was the key-map and the cdr was the keybinding specific valid as the form for `kbd` function.


<a id="orgaa89bf1"></a>

## Proxy reset

For those cases that you want to quickly reset the proxy server, just reset your recipes `:server-host-alist` slot's symbol value, all functional form will obtain the new proxy-host value at next time for proxy-connection doing.


<a id="org6b3a7bd"></a>

# Configuration:

Just require it, and building `PROXY-RECIPE` you specified.


<a id="orgdc65ce2"></a>

## Target operation advice

There're two built-in `proxy-recipe` i.e. the `entropy/proxy-url--eww-recipe` and `entropy/proxy-url--w3m-recipe`, you can call function `entropy/proxy-url-make-builtin-recipes` to buiding them. Futhermore you can specify your own recipe follow what mentioned above, and use function `entropy/proxy-url-make-recipes` to buiding it(see its doc-string for more details).


<a id="orgbaee913"></a>

## customized varaibles

See customized-variable-group `entropy/proxy-url-group` for them.


<a id="org0bd5a7b"></a>

## The regexp rule-set list data

Internally, `entropy-proxy-url` has given the sets of regexp rule set tracking by [github gfw list](https://github.com/gfwlist/gfwlist) project which maintained the common sensible blocked web domain list directed against to China GFW network ecosystem, however I thought as be compatible for some web transfer chargin area too of that China as the biggest aspect doing for thus. The gfw-rule analyzing provided by [entropy-adbp+-rule-analysis](https://github.com/c0001/entropy-adblockP-rule-analysis) package (Add it to `load-path` was requested also).

By default the rule-set was gained once at the startup, but you can refresh it by calling `entropy/proxy-url-refresh-gfw-regexp-alist` at any time for keeping your rule-set updating with upstream.


<a id="org7a83572"></a>

# Change log:

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-18 Sat] </span></span> bug fix for boundp check for \`w3m-command-arguments-alist'

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-11 Sat] </span></span> **version 0.1.1** release out
    
    Remove force requiring \`w3m\` at load time excepted that variable \`entropy/proxy-url-force-require-w3m\` is nil.
    
    Defined specified key map when proper feature loaded up.

-   <span class="timestamp-wrapper"><span class="timestamp">[2018-10-01 Mon] </span></span> **version 0.1.0** release out
    
    First release out.


<a id="org4aefd72"></a>

