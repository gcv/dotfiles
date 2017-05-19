#!/bin/bash


if [[ -z $(command -v greadlink) ]]; then
    echo "no greadlink found; install coreutils from Homebrew first" 1>&2
    return 1
fi


basedir=$(dirname $(greadlink -f "$0"))
pushd $basedir > /dev/null

cd public
for f in *; do
    hidden_version="~/.${f}"
    real_version=$(greadlink -f "${f}")
    eval hidden_version="${hidden_version}"
    if [[ -e "${hidden_version}" ]]; then
        echo " -> $f already exists, ignoring"
    else
        echo "$hidden_version -> $f"
        ln -s "$real_version" "$hidden_version"
    fi
done


popd > /dev/null
