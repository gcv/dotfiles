#!/usr/bin/env bash


set -Eeuo pipefail

readlinkf() { perl -MCwd -MFile::Glob -l -e 'print Cwd::abs_path File::Glob::bsd_glob shift' "$1"; }
basedir=$(dirname "$(readlinkf "$0")")
script=$(basename "${BASH_SOURCE[${#BASH_SOURCE[@]}-1]}")


# FIXME: Rename link.sh to install
# FIXME: Allow installing from GitHub?
# FIXME: This should really use enhanced getopt to only link in things that are truly needed.


DOTFILES="${HOME}/.dotfiles"


link() {
    local from="${DOTFILES}/$1" # $(readlinkf "${basedir}/$1")
    local to="~/$2"
    eval to="${to}" # force ~ expansion
    if [[ -L "${to}" || -e "${to}" ]]; then
        echo " -> ${to} already exists, ignoring"
    else
        echo "linking ${from} to ${to}"
        ln -s "${from}" "${to}"
    fi
}


# link the checkout directory to ~/.dotfiles (i.e.: ~/.dotfiles -> /path/to/dotfiles):
if [[ -L "${DOTFILES}" || -e "${DOTFILES}" ]]; then
    echo " -> ${DOTFILES} already exists, ignoring"
else
    ln -s "${basedir}" "${DOTFILES}"
fi


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
link "private/history-files/fish_$(hostname -s)_history" ".local/share/fish"
link "public/starship.toml" ".config"

link "public/nix" ".config"
link "public/nixpkgs" ".config"

link "emacs" ".emacs.d"

link "public/bin/fzf-history-all" ".local/bin"
link "public/bin/gpgd" ".local/bin"

link "private/notmuch-config" ".notmuch-config"
link "private/offlineimaprc" ".offlineimaprc"
