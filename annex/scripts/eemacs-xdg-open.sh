#!/usr/bin/env bash
set -e
declare this_curpwd="$(pwd)"
. "$EEMACS_SPAWN_BASH_SCRIPT_TEMPLATE"
Eemacs_Func_CheckCmds grep sed xdg-mime
set -e
cd "$this_curpwd"

declare this_uri="$1"
declare this_mime this_defapp this_desktop_file
declare -a this_command_args=()

has_display()
{
    if [ -n "$DISPLAY" ] || [ -n "$WAYLAND_DISPLAY" ]; then
        return 0
    else
        return 1
    fi
}
if ! has_display ; then
    Eemacs_Func_Erro "no display for xdg-open for: $this_uri"
fi

# This handles backslashes but not quote marks.
first_word()
{
    local first rest
    read first rest
    echo "$first"
}
# This handles backslashes but not quote marks.
last_word()
{
    read first rest
    echo "$rest"
}

# Returns true if argument is a file:// URL or path
is_file_url_or_path()
{
    if echo "$1" | grep -q '^file://' \
            || ! echo "$1" | grep -Eq '^[[:alpha:]][[:alpha:][:digit:]+\.\-]*:'; then
        return 0
    else
        return 1
    fi
}

# If argument is a file URL, convert it to a (percent-decoded) path.
# If not, leave it as it is.
file_url_to_path()
{
    local file="$1"
    if echo "$file" | grep -q '^file://\(localhost\)\?/'; then
        file=${file#file://localhost}
        file=${file#file://}
        file=${file%%#*}
        file=$(echo "$file" | sed -r 's/\?.*$//')
        local printf=printf
        if [ -x /usr/bin/printf ]; then
            printf=/usr/bin/printf
        fi
        file=$($printf "$(echo "$file" | sed -e 's@%\([a-f0-9A-F]\{2\}\)@\\x\1@g')")
    fi
    echo "$file"
}

check_input_file()
{
    if [ ! -e "$1" ]; then
        Eemacs_Func_Erro "file '$1' does not exist"
    fi
    if [ ! -r "$1" ]; then
        Eemacs_Func_Erro "no permission to read file '$1'"
    fi
}

# Get the value of a key in a desktop file's Desktop Entry group.
# Example: Use get_key foo.desktop Exec
# to get the values of the Exec= key for the Desktop Entry group.
get_key()
{
    local file="${1}"
    local key="${2}"
    local desktop_entry=""

    IFS_="${IFS}"
    IFS=""
    while read line
    do
        case "$line" in
            "[Desktop Entry]")
                desktop_entry="y"
                ;;
            # Reset match flag for other groups
            "["*)
                desktop_entry=""
                ;;
            "${key}="*)
                # Only match Desktop Entry group
                if [ -n "${desktop_entry}" ]
                then
                    echo "${line}" | cut -d= -f 2-
                fi
        esac
    done < "${file}"
    IFS="${IFS_}"
}


# Recursively search .desktop file
search_desktop_file()
{
    local default="$1"
    local dir="$2"
    local target="$3"

    local file=""
    # look for both vendor-app.desktop, vendor/app.desktop
    if [ -r "$dir/$default" ]; then
        file="$dir/$default"
    elif [ -r "$dir/`echo $default | sed -e 's|-|/|'`" ]; then
        file="$dir/`echo $default | sed -e 's|-|/|'`"
    fi

    if [ -r "$file" ] ; then
        command="$(get_key "${file}" "Exec" | first_word)"
        icon="$(get_key "${file}" "Icon")"
        # FIXME: Actually LC_MESSAGES should be used as described in
        # http://standards.freedesktop.org/desktop-entry-spec/latest/ar01s04.html
        localised_name="$(get_key "${file}" "Name")"
        set -- $(get_key "${file}" "Exec" | last_word)
        # We need to replace any occurrence of "%f", "%F" and
        # the like by the target file. We examine each
        # argument and append the modified argument to the
        # end then shift.
        local args=$# arg
        local replaced=0
        while [ $args -gt 0 ]; do
            case $1 in
                %[c])
                    replaced=1
                    arg="${localised_name}"
                    shift
                    set -- "$@" "$arg"
                    ;;
                %[fFuU])
                    replaced=1
                    arg="$target"
                    shift
                    set -- "$@" "$arg"
                    ;;
                %[i])
                    replaced=1
                    shift
                    set -- "$@" "--icon" "$icon"
                    ;;
                *)
                    arg="$1"
                    shift
                    set -- "$@" "$arg"
                    ;;
            esac
            args=$(( $args - 1 ))
        done
        [ $replaced -eq 1 ] || set -- "$@" "$target"
        this_command_args=("$command" "$@")
        this_desktop_file="$file"
    else
        Eemacs_Func_TermEchoStderr \
            "->> not found desktop file $default in '$dir'"
    fi

    if [[ ${#this_command_args[@]} -eq 0 ]] ; then
        for d in "${dir%/}/"*/; do
            if [ -d "$d" ]; then
                search_desktop_file "$default" "$d" "$target"
            fi
        done
    else
        Eemacs_Func_OkMsg \
            "use desktop file $default in '$dir' for file: $target"
    fi
}

open_generic_xdg_mime()
{
    local uri="$1"
    local filetype="$2"
    local default=`xdg-mime query default "$filetype"`
    if [ -n "$default" ] ; then
        xdg_user_dir="$XDG_DATA_HOME"
        [ -n "$xdg_user_dir" ] || xdg_user_dir="$HOME/.local/share"

        xdg_system_dirs="$XDG_DATA_DIRS"
        [ -n "$xdg_system_dirs" ] || xdg_system_dirs=/usr/local/share/:/usr/share/

        Eemacs_Func_TermEchoStderr "--> use xdg dirs: $xdg_user_dir:$xdg_system_dirs"
        for x in `echo "$xdg_user_dir:$xdg_system_dirs" | sed 's/:/ /g'`; do
            search_desktop_file "$default" "${x%/}/applications/" "$uri"
            if [[ ${#this_command_args[@]} -ne 0 ]] ; then
                break
            fi
        done
        if [[ ${#this_command_args[@]} -eq 0 ]] ; then
            Eemacs_Func_Erro \
                "can not generate commands rely on desktop file: '${this_desktop_file}'
with mime type: $default
for target uri: $uri"
        fi
    else
        Eemacs_Func_Erro "not found defapp for file: $uri"
    fi
}

if is_file_url_or_path "$this_uri"; then
    this_uri="$(file_url_to_path "$this_uri")"
    check_input_file "$this_uri"
fi
this_mime="$(xdg-mime query filetype "${this_uri}")"
# this_defapp="$(xdg-mime query default "${this_mime}")"
Eemacs_Func_TermEchoStderr "--> uri: $this_uri"
open_generic_xdg_mime "$this_uri" "$this_mime"
Eemacs_Func_TermEchoStderr "--> run: ${this_command_args[*]}"
exec "${this_command_args[@]}"
