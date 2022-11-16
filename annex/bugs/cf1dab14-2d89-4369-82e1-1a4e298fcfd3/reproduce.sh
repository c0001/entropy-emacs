#!/usr/bin/env sh

_source_name="${BASH_SOURCE[0]}"
while [ -h "$_source_name" ]; do # resolve $_source_name until the file is no longer a symlink
    _dir_name="$( cd -P "$( dirname "$_source_name" )" >/dev/null && pwd )"
    _source_name="$(readlink "$_source_name")"

    # if $_source_name was a relative symlink, we need to resolve it relative
    # to the path where the symlink file was located
    [[ $_source_name != /* ]] && _source_name="$_dir_name/$_source_name"
done
_dir_name="$( cd -P "$( dirname "$_source_name" )" >/dev/null && pwd )"

declare EMACS="$1"
[ -z $EMACS ] && EMACS=emacs

declare reproduce_elispfile="${_dir_name}"/reproduce.el

declare emacs_version=29
declare emacs_cur_version=''
if command -v "${EMACS}" >/dev/null 2>&1
then
    emacs_cur_version=$("$EMACS" -q --batch --eval '(princ emacs-major-version t)')
    if [ ! "$emacs_cur_version" = "$emacs_version" ]
    then
        echo "Error: emacs major version is not ${emacs_version} (detected as ${emacs_cur_version})"
        exit 1
    else
        export HOME="$_dir_name"
        exec "$EMACS" -q -l "${reproduce_elispfile}"
    fi
else
    echo 'Error: no emacs found'
    exit 1
fi
