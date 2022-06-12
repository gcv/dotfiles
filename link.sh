#!/usr/bin/env bash


set -Eeuo pipefail

readlinkf() { perl -MCwd -MFile::Glob -l -e 'print Cwd::abs_path File::Glob::bsd_glob shift' "$1"; }
basedir=$(dirname "$(readlinkf "$0")")
script=$(basename "${BASH_SOURCE[${#BASH_SOURCE[@]}-1]}")


pushd "${basedir}" > /dev/null

link() {
    local from=$(readlinkf "${basedir}/$1")
    local to="~/$2"
    eval to="${to}"
    if [[ -L "${to}" || -e "${to}" ]]; then
        echo " -> ${to} already exists, ignoring"
    else
        echo "linking ${from} to ${to}"
        ln -s "${from}" "${to}"
    fi
}

link "public/direnvrc" ".direnvrc"
link "public/editrc" ".editrc"
link "public/gitattributes" ".gitattributes"
link "public/gitconfig" ".gitconfig"
link "public/hammerspoon" ".hammerspoon"
link "public/hushlogin" ".hushlogin"
link "public/screenrc" ".screenrc"
link "public/sqliterc" ".sqliterc"
link "public/terminfo" ".terminfo"
link "public/tmux.conf" ".tmux.conf"

link "public/bash_profile" ".bash_profile"
link "public/bashrc" ".bashrc"
link "public/profile" ".profile"

link "public/zshrc" ".zshrc"

link "public/fish" ".config/fish"
link "public/starship.toml" ".config/starship.toml"

link "emacs" ".emacs.d"

link "private/notmuch-config" ".notmuch-config"
link "private/offlineimaprc" ".offlineimaprc"

# XXX: Too aggressive; breaks on .config directories.
# cd public
# for f in *; do
#     hidden_version="~/.${f}"
#     real_version=$(readlinkf "${f}")
#     eval hidden_version="${hidden_version}"
#     if [[ -L "${hidden_version}" || -e "${hidden_version}" ]]; then
#         echo " -> $f already exists, ignoring"
#     else
#         echo "linking $hidden_version to ${real_version}"
#         ln -s "$real_version" "$hidden_version"
#     fi
# done

popd > /dev/null
