#!/usr/bin/env bash
set -e
. "$EEMACS_SPAWN_BASH_SCRIPT_TEMPLATE"
set +e

! command -v sleep &>/dev/null && exit 1

declare _txt _rate="${1:-1}" _size="${2:-524288}"

function _main ()
{
    local i
    if [[ -n $WAYLAND_DISPLAY ]]; then
        # FIXME_0: `wl-copy` will also raise a null x-window as for
        # the FIXME_1 when xwayland feature of the compositor is
        # activated.
        if command -v wl-copy &>/dev/null ; then
            _txt="$(wl-paste 2>/dev/null)"
            if [[ ${#_txt} -gt $_size ]]; then
                Eemacs_Func_Warn "Large wayland clipboard found, reseting"
                wl-copy -c &>/dev/null
            fi
        fi
    fi
    # FIXME_1:
    #
    # Both `xsel` and `xclip` will raise a null X window via grab the
    # clipboard contents which will cause WM logs error of
    # #+begin_example
    # new xwayland surface (null) class: (null) instance: (null)
    # #+end_example
    #
    # And DE's taskbar will popup anonymous iconified app which is
    # caused by them.
    #
    if [[ -n $DISPLAY ]]; then
        if command -v xsel &>/dev/null ; then
            for i in p b s ; do
                _txt="$(xsel -o${i})"
                if [ $? -eq 0 ] && [[ ${#_txt} -gt $_size ]]; then
                    Eemacs_Func_Warn "Large X11(xsel) clipboard($i) found, reseting"
                    xsel --delete ${i}
                fi
            done
        elif command -v xclip &>/dev/null ; then
            for i in primary clipboard secondary ; do
                _txt="$(xclip -o -selection $i 2>/dev/null)"
                if [ $? -eq 0 ] && [[ ${#_txt} -gt $_size ]]; then
                    Eemacs_Func_Warn "Large X11 clipboard($i) found, reseting"
                    echo -n '' | xclip -selection ${i} &>/dev/null
                fi
            done
        fi
    fi
}

{
    while sleep "$_rate" ; do
        _main
    done
}
