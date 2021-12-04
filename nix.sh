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
nix-env -iA \
    nixpkgs.aspell \
    nixpkgs.autoconf \
    nixpkgs.autojump \
    nixpkgs.automake \
    nixpkgs.awscli2 \
    nixpkgs.bat \
    nixpkgs.cmake \
    nixpkgs.coreutils \
    nixpkgs.croc \
    nixpkgs.ctags \
    nixpkgs.direnv \
    nixpkgs.exif \
    nixpkgs.fdupes \
    nixpkgs.ffmpeg \
    nixpkgs.findutils \
    nixpkgs.fzf \
    nixpkgs.git \
    nixpkgs.git-lfs \
    nixpkgs.gnupg \
    nixpkgs.htop \
    nixpkgs.jq \
    nixpkgs.lazydocker \
    nixpkgs.lazygit \
    nixpkgs.ledger \
    nixpkgs.libvterm-neovim \
    nixpkgs.magic-wormhole \
    nixpkgs.mosh \
    nixpkgs.nix-direnv \
    nixpkgs.nmap \
    nixpkgs.notmuch \
    nixpkgs.offlineimap \
    nixpkgs.p7zip \
    nixpkgs.pandoc \
    nixpkgs.password-store \
    nixpkgs.pv \
    nixpkgs.rclone \
    nixpkgs.restic \
    nixpkgs.ripgrep \
    nixpkgs.rlwrap \
    nixpkgs.silver-searcher \
    nixpkgs.tectonic \
    nixpkgs.texinfo \
    nixpkgs.tmux \
    nixpkgs.tree \
    nixpkgs.unrar \
    nixpkgs.watch \
    nixpkgs.wget \
    nixpkgs.xz \
    nixpkgs.youtube-dl \
    nixpkgs.yt-dlp
