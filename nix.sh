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
    global \
    gnupg-1.4.23 \
    gnutls \
    htop \
    jq \
    ledger \
    magic-wormhole \
    mosh \
    nmap \
    offlineimap \
    p7zip \
    pv \
    rlwrap \
    silver-searcher \
    tectonic \
    texinfo \
    tmux \
    tree \
    watch \
    wget \
    xz \
    youtube-dl