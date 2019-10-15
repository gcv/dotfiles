#!/usr/bin/env bash


NIX_HOME=/nix

if [[ ! -d "${NIX_HOME}" ]]; then
    sudo mkdir -m 0755 /nix && chown `whoami` /nix
    sh <(curl https://nixos.org/nix/install) --no-daemon
fi


# Recent macOS releases provide reasonable versions of:
# - bash
# - zsh
# - git
# - sqlite
nix-env -i \
    aspell \
    autoconf \
    autojump \
    automake \
    awscli \
    bat \
    cmake \
    coreutils \
    ctags \
    direnv \
    exif \
    fdupes \
    ffmpeg \
    findutils \
    fzf \
    git \
    gnupg-1.4.23 \
    htop \
    jq \
    ledger \
    magic-wormhole \
    mosh \
    nmap \
    offlineimap \
    p7zip \
    pv \
    rclone \
    rlwrap \
    silver-searcher \
    tectonic \
    texinfo \
    tmux \
    tree \
    unrar \
    watch \
    wget \
    xz \
    youtube-dl
