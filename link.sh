#!/usr/bin/env bash


set -Eeuo pipefail

readlinkf() { perl -MCwd -MFile::Glob -l -e 'print Cwd::abs_path File::Glob::bsd_glob shift' "$1"; }
basedir=$(dirname "$(readlinkf "$0")")
script=$(basename "${BASH_SOURCE[${#BASH_SOURCE[@]}-1]}")


pushd "${basedir}" > /dev/null

ln -s $(readlinkf "${basedir}/public/direnvrc") "~/.direnvrc"
ln -s $(readlinkf "${basedir}/public/editrc") "~/.editrc"
ln -s $(readlinkf "${basedir}/public/gitattributes") "~/.gitattributes"
ln -s $(readlinkf "${basedir}/public/gitconfig") "~/.gitconfig"
ln -s $(readlinkf "${basedir}/public/hammerspoon") "~/.hammerspoon"
ln -s $(readlinkf "${basedir}/public/hushlogin") "~/.hushlogin"
ln -s $(readlinkf "${basedir}/public/screenrc") "~/.screenrc"
ln -s $(readlinkf "${basedir}/public/sqliterc") "~/.sqliterc"
ln -s $(readlinkf "${basedir}/public/terminfo") "~/.terminfo"
ln -s $(readlinkf "${basedir}/public/tmux.conf") "~/.tmux.conf"

ln -s $(readlinkf "${basedir}/public/bash_profile") "~/.bash_profile"
ln -s $(readlinkf "${basedir}/public/bashrc") "~/.bashrc"
ln -s $(readlinkf "${basedir}/public/profile") "~/.profile"

ln -s $(readlinkf "${basedir}/public/zshrc") "~/.zshrc"

ln -s $(readlinkf "${basedir}/public/fish") "~/.config/fish"
ln -s $(readlinkf "${basedir}/public/starship.toml") "~/.config/starship.toml"

ln -s $(readlinkf "${basedir}/emacs") "~/.emacs.d"

ln -s $(readlinkf "${basedir}/private/notmuch-config") "~/.notmuch-config"
ln -s $(readlinkf "${basedir}/private/offlineimaprc") "~/.offlineimaprc"

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
