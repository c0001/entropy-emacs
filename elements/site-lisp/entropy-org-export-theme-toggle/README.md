# Table of Contents

1.  [Commentary:](#org9aa48af)
    1.  [Preamble](#org05b7ade)
    2.  [Html part](#org7080440)

Copyright (C) 2018-11-09, 2022 Entropy

Author:        Entropy <bmsac0001@gmail.com>
Maintainer:    Entropy <bmsac001@gmail.com>
URL:           none
Package-Version: none
Version:       none
Created:       2018-11-09
Keywords:      org, theme
Compatibility: GNU Emacs emacs-version;
Package-Requires: ((emacs "24") (cl-lib "0.5") (org "9.1.3") (dash "2.19.1"))

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


<a id="org9aa48af"></a>

# Commentary:

**The theme toggle for Org export procedure.**


<a id="org05b7ade"></a>

## Preamble

The theme meta source contained in variable or caching in local
file. All themes denoted as the plist data structure presented as the
readable type both for extracted easily for package configuration and
main internal manipulation function.

Org mode have the exporting framework for using to build various file
format e.g. Markdown(".md") , Html, and also for latex. The
individually file exports procedure have the unique process treatment
thus, this package must given the toggle method respectively. For now,
this package just implemented the html exporting treatment part, thus
follow instruction only has the html part.


<a id="org7080440"></a>

## Html part

-   Themes plist: `entropy/org-exptt-html-theme-plist`

    This customizable variable was the core config place for using
    this package as the theme toggle functional way as an alist
    prototype. Each element of it was one list of whose cdr is an
    plist formed as four key 1) `theme_category` 2) `:theme_css` 3)
    `:theme_js` 4) `:theme_mischellaneous`, and its car is the
    string indicate the theme name.

    1.  `:theme_category`

        Each theme must contained into one category, category was
        represented by the string type, there's no default common
        category type binding for those who doesn't have specified one
        category, so that you can not find it at the interaction
        interface.

        The valid value of this key was one string-list, thus multi
        category crossover relationship is supported. Thus you can
        specified the refer one as:

            (list "common" "index" "section")

        That's demo denoted that this theme are designed into three way
        as what the category name presented

    2.  `:theme_css`

        The css style list place. Each element of this key refer value is
        the css type string consists of the commonly html css embedded
        type as "<link href="">" external type or the
        "<style>&#x2026;</style>" type as the full string stored . Notice, the
        href part must been exists of the external type.

        Demo:

            :theme_css
            ("<link rel=\"stylesheet\" title=\"Standard\" href=\"https://orgmode.org/worg/style/worg.css\" type=\"text/css\" />"
             "<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"https://orgmode.org/worg/style/worg-zenburn.css\" type=\"text/css\" />"
             "<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"https://orgmode.org/worg/style/worg-classic.css\" type=\"text/css\" />"
             "
            <style>
                body #content {
                    padding-top: 0px;
                    width: 55%;
                    margin: 0 auto;
                    margin-top: 5em;
                    background-color: white;
                    padding: 2em;
                    ;; /* box-shadow: 3px 3px 5px #888; */
                }

                body #postamble.status {width:65%;margin:0 auto;padding:2em;border: 0px;}

                pre.src
                {
                  overflow-x: scroll;
                  overflow-y: scroll;
                  max-height: 400px;
                }

                pre.example
                {
                  overflow-x: scroll;
                  overflow-y: scroll;
                  max-height: 400px;
                }


                img{max-width: 700px}

                h3 {
                  margin-left: 0em
                }

                h4,
                h5 {
                  font-size: 1.2em;
                  margin-left: 0em
                }

                h6,
                h7,
                h8,
                h9,
                h10 {
                  font-size: 1.1em;
                  font-weight: bold;
                  color: crimson;
                  margin-left: 1.3em
                }

                blockquote
                {
                  background-color: azure;
                  padding:2%;
                  border: 2px solid;
                  border-color: darkgrey;
                }

                .org-svg
                {
                  max-width: 500px
                }
            </style>")

    3.  `:theme_js`

        The js part was fully similar with the css key expection that the
        'style' tag replaced as 'script'.

        Demo:

            :theme_js
            ("<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-1.11.0.min.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-ui-1.10.2.min.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.localscroll-min.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.scrollTo-1.4.3.1-min.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.zclip.min.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/bigblow.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/hideshow.js\"></script>"
             "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.min.js\"></script>")

    4.  `:theme_mischellaneous`

        Using for external 'link' tag list, as the css key does, but
        without cached (not implemented til now but for the furter.)

        Demo:

            :theme_mischellaneous
            ("<link rel=\"SHORTCUT ICON\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/x-icon\" />"
             "<link rel=\"icon\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/ico\" />")

-   Themes cache:

    This packae support caching the remote cdn meta data both of css and
    js but not of some icons link(no needed as that these resource can
    not embed into the html file). Caching using the emacs
    'url-retrieve' method to retrieving meta data and write the
    responses to into the unique file successively.

    For each specified html theme, caching method will create the
    corresponding file respectively while there's no caching refer to
    the current selected theme. Otherwise, this package will throw one
    confirmation for quering whether update the current theme caches.

    The root dir of the caching was defined of
    `entropy/org-exptt-html-theme-cache-dir`, but you can defined
    elsewhere you wander for.

    **Exceptions:**

    If an theme plist has an enabled key `:theme_dontcache`, then
    this theme can not be cached in anyway.


<a id="org3717719"></a>
