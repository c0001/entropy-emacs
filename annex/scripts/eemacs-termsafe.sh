#!/usr/bin/env bash
set -e
. "$EEMACS_SPAWN_BASH_SCRIPT_TEMPLATE"
set +e

! command -v sleep &>/dev/null && exit 1

declare _txt _rate="${1:-1}" _size="${2:-524288}"

function _main ()
{
    if command -v wl-copy &>/dev/null ; then
        _txt="$(wl-paste 2>/dev/null)"
        if [[ ${#_txt} -gt $_size ]]; then
            Eemacs_Func_Warn "Large wayland clipboard found, reseting"
            wl-copy -c &>/dev/null
        fi
    fi

    local i
    if command -v xclip &>/dev/null ; then
        for i in primary clipboard secondary ; do
            _txt="$(xclip -o -selection $i 2>/dev/null)"
            if [ $? -eq 0 ] && [[ ${#_txt} -gt $_size ]]; then
                Eemacs_Func_Warn "Large X11 clipboard($i) found, reseting"
                echo -n '' | xclip -selection ${i} &>/dev/null
            fi
        done
    fi
    if command -v xsel &>/dev/null ; then
        for i in p b s ; do
            _txt="$(xclip -o${i})"
            if [ $? -eq 0 ] && [[ ${#_txt} -gt $_size ]]; then
                Eemacs_Func_Warn "Large X11(xsel) clipboard($i) found, reseting"
                xsel --delete ${i}
            fi
        done
    fi
}

{
    while sleep "$_rate" ; do
        _main
    done
}
