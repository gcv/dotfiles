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
# The following are similar:
# - croc and magic-wormhole
nix-env -i \
    aspell \
    autoconf \
    autojump \
    automake \
    awscli2 \
    bat \
    cmake \
    coreutils \
    croc \
    ctags \
    direnv \
    exif \
    fdupes \
    ffmpeg \
    findutils \
    fzf \
    git \
    git-lfs \
    gnupg \
    htop \
    jq \
    kopia \
    ledger \
    libvterm-neovim \
    magic-wormhole \
    mosh \
    nmap \
    notmuch \
    offlineimap \
    pandoc \
    password-store \
    p7zip \
    pv \
    rclone \
    restic \
    ripgrep \
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
