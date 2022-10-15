# Table of Contents

1.  [Copyright (C) 20190906  Entropy](#org81686c9)
2.  [Commentary:](#org180c012)
    1.  [Eww and emacs-w3m proxy stuff](#org3a85fdd)
    2.  [How this package working as?](#org92daf4d)
    3.  [Methods given](#org604c1f4)
        1.  [Manually proxy way](#org6cb6787)
        2.  [Proxy recipe](#org0ec4b8b)
    4.  [Proxy reset](#org8613403)
3.  [Configuration:](#org7a07dff)
    1.  [Target operation advice](#org80e85e5)
    2.  [customized varaibles](#org8b65421)
    3.  [The regexp rule-set list data](#orgfbbb47b)
4.  [Change log:](#orge63e500)

<a id="org81686c9"></a>

# Copyright (C) 20190906  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    Package-Version: v0.1.2
    Created:       2018
    Keywords:      proxy
    Compatibility: GNU Emacs emacs-version;

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


<a id="org180c012"></a>

# Commentary:

An emacs extension trafficking url matching by proxy regxp rule-set or
pull down them into whole proxy tunnel.

This package perform as the library role for the functional provision.


<a id="org3a85fdd"></a>

## Eww and emacs-w3m proxy stuff

Emacs has the ability to retrieving network data steps with internal
func `url-retrieve` and it's refer url library as. Eww using it as the
communication backend.

Although, there's another way for emacs to get network data through a
the way of using external shell application which obtains its
responsing as the render part. One as be in this case was the famous
cli plain text browser [w3m](http://w3m.sourceforge.net/).

The proxy way for the internal data retrieve was based on emacs
internal proxy mechanism, and the other one was based on the refer
response creator's self mechanism.


<a id="org92daf4d"></a>

## How this package working as?

This package taking off the proxy way through two ways for as:

1.  Regexp matching of current url retrieving.
2.  Full proxy tunnel whatever current transfer url is.

As seen as which are easy to understanding of the full proxy
type,here's the description for 'regexp' way:

Each network transfer request will come with one url string as the
requesting wrapper header. URL string has it's feature stored in it's
domain part, the domain part can be matched by categorizing using
regexp method, thus each network requesting can be filter by sets of
regexp rule-set for determining whether go into the proxy tunnerl.

The regexp filter can be generized of more functional part, by
matching whatever you want. For user specification aimed, the
customized variable `entropy/proxy-url-user-proxy-match-func` stored
the function for matching the pre-proxied url method(see its docstring
for more details).

This package use the [gfwlist](https://github.com/gfwlist/gfwlist) as the default ruleset, for the original
prototype for it was the simple variant of [PAC](https://en.wikipedia.org/wiki/Proxy_auto-config)(proxy auto
configuration), whose syntax abided by [adblock-plus](https://adblockplus.org/) web extensions,
for that reason to use this was that gfwlist was originally used for
the webbrowser extensions for doing thus as what you have known for
the internet charging among on CHINA. Thus for that, this package
requires one `PAC` analyzer as now I use the another [entropy-emacs](https://github.com/c0001/entropy-emacs)
included package `entropy-adblock+-rule-analysis` to role as the gfwlist
anaylizer, it will auto fetch the latest version gfwlist if possible
when you current internet environment allow the connecting for that,
or using the package built-in one which was the pre-fetched one, so it
will not be the latest version.


<a id="org604c1f4"></a>

## Methods given


<a id="org6cb6787"></a>

### Manually proxy way

There's four macros used for wrapped BODY into an proxy procedure,
and they are:

-   `entropy/proxy-url-with-url-proxy`
-   `entropy/proxy-url-with-w3m-proxy`
-   `entropy/proxy-url-with-socks-proxy`
-   `entropy/proxy-url-with-shell-proxy`

Each of them has the same arguments list in case of:

1.  Keys(optional):

    -   `:server-host-list` : as the from as the cdr of each element
        of `entropy/proxy-url-default-proxy-server-alist`.

    If there's no key specification given in the wrapper, they will
    fallback to use the default one builtin with this package, so
    as these key are optional.

2.  Body: the procedure want to run within the proxy wrapper

Thus if you want to run the spawn process under an shell-proxy you
can use:

    ;; replace host and port with your specification
    (entropy/proxy-url-with-shell-proxy
      :server-host-list ("http://" "127.0.0.1" "1081")
      (make-process
       :name "curl"
       :buffer (get-buffer-create "---*proxy-curl*---")
       :command '("curl" "https://www.google.com")))
    ;; this will make an buffer to show the curl process retrieval


<a id="org0ec4b8b"></a>

### Proxy recipe

We using property list as a `PROXY-RECIPE` to given the customized
way for specify the proxy subroutine.

As the focus on, the `PROXY-RECIPE` mainly use `advice-add` to
around wrappering the target underline functional commands, like
`w3m-goto-url` , `url-retrieve` etc.

The `PROXY-RECIPE` slots described in
`entropy/proxy-url-make-recipes`'s docstring

When building done an your own proxy-recipes, use function
`entropy/proxy-url-make-recipes` to activated your proxy-recipes
in batching way which means for an list of `PROXY-RECIPE`,
optional arg `UNMAKE` means to disable those recipes if you want
to disable any proxy patch feature in those recipes.


<a id="org8613403"></a>

## Proxy reset

For those cases that you want to quickly reset the proxy server,
just reset your recipes `:server-host-alist` slot's symbol value,
and remake your recipe.

For buitin recipes, run the proxy port reset **interactively**
functioin \`entropy/proxy-url-update-proxy-port' for quickly reset
proxy port only.


<a id="org7a07dff"></a>

# Configuration:

Just require it, and building `PROXY-RECIPE` you specified.


<a id="org80e85e5"></a>

## Target operation advice

There're two built-in `proxy-recipe` i.e. the
`entropy/proxy-url--eww-recipe` and
`entropy/proxy-url--w3m-recipe`, you can call function
`entropy/proxy-url-make-builtin-recipes` to buiding
them. Futhermore you can specify your own recipe follow what
mentioned above, and use function `entropy/proxy-url-make-recipes`
to buiding it(see its doc-string for more details).


<a id="org8b65421"></a>

## customized varaibles

See customized-variable-group `entropy/proxy-url-group` for them.

For default settings see `entropy/proxy-url-default-proxy-server-alist`.


<a id="orgfbbb47b"></a>

## The regexp rule-set list data

Internally, `entropy-proxy-url` has given the sets of regexp rule
set tracking by [github gfw list](https://github.com/gfwlist/gfwlist) project which maintained the
common sensible blocked web domain list directed against to China
GFW network ecosystem, my thoughts as be compatible for some web
transfer charging area also as China as the biggest aspect doing
for thus. The gfw-rule analyzing provided by
[entropy-adbp+-rule-analysis](https://github.com/c0001/entropy-adblockP-rule-analysis) package (Add it to `load-path` was
requested also).

By default the rule-set was gained once at the startup, but you
can refresh it by calling `entropy/adbp-rule-update` at any time
for keeping your rule-set updating with upstream.


<a id="orge63e500"></a>

# Change log:

-   <span class="timestamp-wrapper"><span class="timestamp">[2022-10-15 Sat 01:22] </span></span> Context refactor

-   <span class="timestamp-wrapper"><span class="timestamp">[2022-02-24 Thu 01:22] </span></span> API add
    -   Add `entropy/proxy-url-inhbit-all-proxy` to allow permanently
        inhibit the proxy wrapper.

-   <span class="timestamp-wrapper"><span class="timestamp">[2021-02-03 Wed 18:28] </span></span> context update
    -   Fix macro context without quoting in arglist now
    -   Update socks proxy mechanism follow new emacs builtin \`socks.el'
    -   Docstring details update for some API for more clearly restriction.

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-03-11 Wed 10:41] </span></span> **version 0.1.2** release out
    -   Using `entropy/proxy-url-user-proxy-match-func` for more flexible
        aiming user aspect proxy method specification.

    -   Follow `entropy-adblock+-rule-analysis` updates for using more
        quickly way for matching proxy matching way, and thus for that now
        support **whitelist** tunnel.

    -   Add `entropy/proxy-url-update-proxy-port` interactive function to
        quickly rebind all proxy underlines proxy-port. This useful for
        those user who using local proxy client as front-end which proxy
        url matches as '127.0.0.1' etc.

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-18 Sat] </span></span> bug fix for boundp check for \`w3m-command-arguments-alist'

-   <span class="timestamp-wrapper"><span class="timestamp">[2020-01-11 Sat] </span></span> **version 0.1.1** release out

    Remove force requiring \`w3m\` at load time excepted that variable
    \`entropy/proxy-url-force-require-w3m\` is nil.

    Defined specified key map when proper feature loaded up.

-   <span class="timestamp-wrapper"><span class="timestamp">[2018-10-01 Mon] </span></span> **version 0.1.0** release out

    First release out.


<a id="org22c494b"></a>
