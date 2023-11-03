<h1 style="text-align: center;">
entropy-emacs
</h1>
<br>
<br>
<a href="https://github.com/c0001/entropy-emacs/tree/dev">
    <img src="elements/core/logo/logo.png" alt="drawing" width="30%"
         style="display: block; margin-left: auto; margin-right: auto;"
    />
</a>
<p style="text-align: center;">A self-maintained simple GnuEmacs configuration<p>
<div style="margin: auto; width: 70%;">
    <a href="https://github.com/c0001/entropy-emacs/actions?query=branch%3Adev">
        <img src="https://github.com/c0001/entropy-emacs/actions/workflows/ci.yml/badge.svg?branch=dev"
             title="Build Status" alt="drawing">
    </a>
    <a href="https://github.com/c0001/entropy-emacs/releases">
        <img src="https://img.shields.io/github/tag/c0001/entropy-emacs.svg?label=Release"
             title="Release Tag" alt="drawing">
    </a>
    <a href="https://www.gnu.org/licenses/gpl-3.0.html">
        <img src="http://img.shields.io/:License-GPL3-blue.svg"
             title="Liscense" alt="drawing">
    </a>
    <a href="https://www.gnu.org/distros/distros.html">
        <img src="https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black"
             title="Platform:linux" alt="drawing">
    </a>
    <a href="https://www.microsoft.com/en-US/windows">
        <img src="https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue"
             title="Platform:windows" alt="drawing">
    </a>
    <a href="https://www.apple.com/macos">
        <img src="https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white"
             title="Platform:macos" alt="drawing">
    </a>
</div>

> **Terms convention:** We call **eemacs** as the abbreviation of `entropy-emacs`.
This personal emacs distribution can be used in:

-   Learning emacs.
-   HTML, CSS, JS, PHP, C , PYTHON and more major-modes for coding editing and testing.
-   Write article, recording content you captured and simple task manager with configured Org-mode.
-   Integrate with some utilities by self and be from web and others great idea which also be in github.

Support platform:

-   GNU/Linux (fully support)
-   Windows7/8/8.1/10/11 (partial support)
-   Darwin(MacOS) (partial support and not testing)

Support emacs version:

-   GNU Emacs 27.1 and higher

For see full documents of the introduction of `entropy-emacs` please see [here](elements/site-lisp/entropy-emacs-doc/org/entropy-emacs_introduction.md).

**Extensions:**

`entropy-emacs` using various third-party extensions from [elpa](https://elpa.gnu.org/packages/), [melpa](https://melpa.org), and some ones hosted on gihtub.com, thus most of them are not wrapped within this repo, for reason of module aspect management. There's one main corresponding project tied with `entropy-emacs` for as the archive of current using extensions collection: ([entropy-emacs-extensions](https://github.com/c0001/entropy-emacs-extensions)), for see its explicit meaning, see the commentary of [entropy-emacs-ext.el](elements/core/baron/summon/entropy-emacs-ext.el), and it's the stable(tested) extensions that entropy-emacs used for, it has its own host-server procedure which forked from '[melpa](https://melpa.org/)' to produce as what it means. You must make it before using it and make understanding by following its README or just install its stable release using the `make` section of `eemacs` (see the make eemacs secion below).

For case using by self downloading `entropy-emacs-extension` in a released `entropy-emacs`, there's a version limit restriction usage for this case that each eemacs release was followed a version of `entropy-emacs-extension` release or the release referred updates version follow the "x.y.z" version naming convention, you should download the `entropy-emacs-extension` release which case this case specified.

Or on the most used occasion, you don't need to retrieve that eemacs extension release project, just using the melpa **up-to-date** upstream but may have compatible problem with current eemacs release version, if that, please using eemacs git repo as your eemacs configuration workspace host to track update with `dev` branch, this may solve various compatible issue or kick one issue on [eemacs issue page](https://github.com/c0001/entropy-emacs/issues) for querying/asking on a resolution.

For switch this two extension usage type, the variable `entropy/emacs-ext-elpkg-customized-get-type` are given for customization, the default type was using melpa upstream to give the default ignorance to individually `entropy-emacs-extension` resources retrieving trouble, but in this case should notice the above mentioned compatible issues.

For use `entropy-emacs-extensions`, put this

```emacs-lisp
    (setq entropy/emacs-ext-elpkg-customized-get-type 'entropy-emacs-extenisons-project)
```

to your `custom.el` and then run


```shell
    make install-eemacs-ext-build
```

in eemacs configuration root host. (the eemacs `make` facilities please see below section)

**Make entropy-emacs**

entropy-emacs use GnuMakefile to initialize with rich facilities for the sake of stick on eemacs internal mechanism.

You can run `make help` to see the details in eemacs configuration host root path.

**TIPS:**

1.  emacs hang and can not handle terminate using `C-g`?

    Try using `pgrep` to find the process id of current emacs session and then run `kill -s SIGUSR2 pid` to trigger the emacs internal terminate handle.

**Thanks to:** [Centaur emacs](https://github.com/seagle0128/.emacs.d)

**Release:**

Entropy(bmsac0001@gmail.com) @ <span class="timestamp-wrapper"><span class="timestamp">[2019-11-25 Mon 15:39] </span></span> © GPL/v3

`VERSION: 0.1.0 Tag: ONEPIECE`

<!-- Local Variables: -->
<!-- fill-column: 10000 -->
<!-- End: -->
