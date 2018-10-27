#!/bin/bash


set -Eeuo pipefail

readlinkf() { perl -MCwd -MFile::Glob -l -e 'print Cwd::abs_path File::Glob::bsd_glob shift' "$1"; }
basedir=$(dirname "$(readlinkf "$0")")
script=$(basename "${BASH_SOURCE[${#BASH_SOURCE[@]}-1]}")


pushd "${basedir}" > /dev/null

cd public
for f in *; do
    hidden_version="~/.${f}"
    real_version=$(readlinkf "${f}")
    eval hidden_version="${hidden_version}"
    if [[ -e "${hidden_version}" ]]; then
        echo " -> $f already exists, ignoring"
    else
        echo "$hidden_version -> $f"
        ln -s "$real_version" "$hidden_version"
    fi
done

popd > /dev/null
