#!/usr/bin/env bash
set -e
# See https://stackoverflow.com/a/246128/3561275
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

gfw_file="${DIR}/gfw-list.txt"

if [[ -f "$gfw_file" ]] ; then rm -v "$gfw_file" ; fi
curl -L 'https://raw.githubusercontent.com/gfwlist/gfwlist/master/gfwlist.txt' \
     -o "$gfw_file"
