# entropy-open-with


# Table of Contents

1.  [Installation](#org48668d2)
2.  [Configuratioin](#orgad6e3df)
3.  [Interaction](#org14ac4eb)
4.  [Apis](#org4266bb2)
5.  [Notice](#org27d4fb4)

Open files with external executable applications on emacs interface. 

There's times for emacer try their best-efforts for dealing all the
operations within emacs onl, but I thought about that this disposition
was not sticked enough for daily emacs experience that's most of
common emacser prefer to do some task emacs not be good at with
external application rather than sticking on the purpose for some
ultra emacser does said previous.

This package given the ability what file openning with wide customized
external applications according to the filename extension.


<a id="org48668d2"></a>

# Installation

Just requiring the file [entropy-open-with.el](entropy-open-with.el)  using:

    (require 'entropy-open-with)

Then bind key for func `entropy/open-with-dired-open` and
`entropy/open-with-buffer` for suggested keybindings `C-M-return` and
`C-M-1` respectively.

Before doing the above requiring operation, remember to adding this
package to the load-path.

Or if you using [use-pacage](https://github.com/jwiegley/use-package) package management macro, you could doing
as below:

    (use-package entropy-open-with
      :ensure nil
      :load-path "path-to-this-package"
      :commands (entropy/open-with-dired-open
                 entropy/open-with-buffer)
      :bind (("C-M-1" . entropy/open-with-buffer))
      :init
      (with-eval-after-load 'dired
        (define-key dired-mode-map 
          (kbd "<C-M-return>") 'entropy/open-with-dired-open)))


<a id="orgad6e3df"></a>

# Configuratioin

The core configuration for this package's customized refers was based
on the one customized variable `entropy/open-with-type-list` which
indicated that you can specify group of filename extension name of
regexp expression with the corresponding external application forms as
below:

    '((("html" "pdf" "xml" "php" "md" "markdown")
       "c:/PortableApps/FirefoxPortable/FirefoxPortable.exe"
       "file://")
      (("mp3" "mp4" "mkv" "rmvb" "wmv" "flv" "avi")
       "c:/PortableApps/MPC-HCPortable/MPC-HCPortable.exe"
       "")
      (("c")
       "c:/PortableApps/codeblocks/bin/codeblocks.exe"
       ""))

Each element of this list consists of three subjects:

-   **extension regexp strings list**
    
    Regexp strings match for specified filename extension can not be
    include extension dot prefix, thus "\\.pdf" was ineffectual, using
    "pdf" instead.
    
    Group extensions match was supported as the benefit way for reducing
    config literal context length for the case for which application can
    be calling by dealing with multi file types, e.g. `firefox` can be
    used both of html file open and pdf file as so.

-   **External application path string**
    
    Called of application's absolute path.

-   **Command line filaname transfer heading type**
    
    For some external application's URI args request as that file name
    must be concated with the special head prefix, e.g. for web browser
    that the local page file command line arg must have the prefix
    "<file://>", this subject given for thus as.
    
    For now, this package just support two file name prefix head
    i.e. "<file://>" and the empty one "", the latter was wrappered
    filename string into quoted style internally.

\`


<a id="org14ac4eb"></a>

# Interaction

-   **Func:** `entropy/open-with-interactively`
    
    Manually choose file be opened later with specific external
    applications query from list `entropy/open-with-type-list`, gives
    the file candidates query filter prompt powered by [ivy](https://github.com/abo-abo/swiper) framework.

-   **Func:** `entropy/open-with-dired-open`
    
    Calling external application to open marked files in dired-mode.

-   **Func:** `entropy/open-with-buffer`
    
    Calling external applications to open current buffer chained file.

All functions above mentioned was given as the interactive function
for as so. You can binding them to your key-map along with your habits
or obey the previous installation init setup.


<a id="org4266bb2"></a>

# Apis

`entropy-open-with` was the minor tool for just giving fiews useful
api functions:

-   Func `entropy/open-with-match-open` 
    
    This func was given the simple way for query input filename's
    corresponding 'open-with' type and then opened it immediately with
    specific external application.
    
    It's arg was one list consists of filenames (path strings), thus
    multi files 'open-with' was supported.

-   Func `entropy/open-with-port`
    
    This func given the try for open single file with 'open-with' or
    using internal emacs openning method.
    
    This func compensates the missing port of func
    `entropy/open-with-match-open` procedure which can open url(web
    link) using external application that original mechanism just allow
    exists local file matched with, means that origin one can not
    distinguish web link because of that it's do not has any mime
    extensions.
    
    Arguments:
    
    -   Force one: interact
        
        This arguments was forcefully required by calling with as, it
        denoted whether let user manually choose file for 'open-with'
        for.
    
    -   optionals: 1) filename      2) inemacs
        
        These two args are optionally, `filename` using the case just when
        the forced arg `interact` are nil, it's used for the case while
        developer want to calling it just in elisp situation.
        
        The other one `inemacs` gives the try for open specific file in
        emacs method.


<a id="org27d4fb4"></a>

# Notice

For current implementation, this package just whole implemented fully
on windows platform, darwin and linux are using `shell-quote-argument`
and `xdg-open` routines respectively for as Thus.

**Limitation on windows platform:**

In windows, the decoding method was using the one called `code pages`
which not compatible with UNIX-LIKE platform which also using one
coding method for all cli-transfer with. That the problem occured that
for that emacs-windows-port can not decoding unicode cli-args for the
subprocess with properly processing for.

In `entropy-open-with` interal mechnism, because of that it using
`w32-shell-execute` as the windows port corresponding subjects core
func, unicode file name was not supported excluding the situation
while your current code page can be fully decoding the inputting
filename as so.

This imperfection was the emacs windows port primary implementation
state, which was referenced to the gnu-emacs mailing list: [here](https://lists.gnu.org/archive/html/emacs-devel/2016-01/msg00406.html).

