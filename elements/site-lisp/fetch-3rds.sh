#!/usr/bin/env bash
set -e
_source_name="${BASH_SOURCE[0]}"
while [ -h "$_source_name" ]; do # resolve $_source_name until the file is no longer a symlink
    _dir_name="$( cd -P "$( dirname "$_source_name" )" >/dev/null && pwd )"
    _source_name="$(readlink "$_source_name")"

    # if $_source_name was a relative symlink, we need to resolve it relative
    # to the path where the symlink file was located
    [[ $_source_name != /* ]] && _source_name="$_dir_name/$_source_name"
done
_dir_name="$( cd -P "$( dirname "$_source_name" )" >/dev/null && pwd )"
_current_dir_name="$(pwd)"

pkg="$1"

cd "$_dir_name"

if [[ -n $(git status --short) ]] ; then
    echo 'err: repo modified and/or untracked' ; exit 1
elif [[ -n $(git diff --stat) ]] ; then
    echo 'err: repo dirty' ; exit 1
elif [[ -n $(git status --porcelain) ]]; then
    echo 'err: repo dirty' ; exit 1
fi

declare -A var_prjs=(
    [rime]='4be1cf2::::https://github.com/c0001/emacs-rime.git'
    [liberime]='747649a::::https://github.com/c0001/liberime.git'
    [treemacs]='639180c::::https://github.com/c0001/treemacs.git'
)

for i in "${!var_prjs[@]}" ; do
    j="${var_prjs["${i}"]}" ; k="${j%%::::*}" ; j="${j##*::::}"
    if [[ -n $pkg ]] ; then
        if [[ $i != "$pkg" ]] ; then
            continue
        else
            echo "spec pkg: $i -- $k -- $j"
        fi
    fi
    if [[ -d $i ]] ; then
        git rm -r --cached -- "$i"
        rm -rf "$i"
    fi
    if [[ -n $i ]] ; then
        git clone --recursive "$j" "$i"
        git -C "$i" checkout "$k"
        git -C "$i" submodule update --init --recursive
        cd "$i" && rm -rf .git && cd "$_dir_name"
        git add "$i"
        git commit --gpg-sign='618277E86068F592A4F3C42842EBF24476885D91' -m "\
fetch-3rds: add third party pkg -- $i

from:   $j
commit: $k
"
    fi
done

echo "ok: all jobs done"
