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
# - bat and cat
# - exa and ls
# - ranger and joshuto (file managers)
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
    nixpkgs.exa \
    nixpkgs.exif \
    nixpkgs.fd \
    nixpkgs.fdupes \
    nixpkgs.ffmpeg \
    nixpkgs.findutils \
    nixpkgs.fish \
    nixpkgs.fzf \
    nixpkgs.git \
    nixpkgs.git-lfs \
    nixpkgs.gnupg \
    nixpkgs.htop \
    nixpkgs.jq \
    nixpkgs.joshuto \
    nixpkgs.lazydocker \
    nixpkgs.lazygit \
    nixpkgs.ledger \
    nixpkgs.libvterm-neovim \
    nixpkgs.magic-wormhole \
    nixpkgs.mosh \
    nixpkgs.miniserve \
    nixpkgs.nix-direnv \
    nixpkgs.nmap \
    nixpkgs.notmuch \
    nixpkgs.offlineimap \
    nixpkgs.p7zip \
    nixpkgs.pandoc \
    nixpkgs.password-store \
    nixpkgs.pastel \
    nixpkgs.pv \
    nixpkgs.ranger \
    nixpkgs.rclone \
    nixpkgs.restic \
    nixpkgs.ripgrep \
    nixpkgs.rlwrap \
    nixpkgs.sd \
    nixpkgs.silver-searcher \
    nixpkgs.starship \
    nixpkgs.tectonic \
    nixpkgs.texinfo \
    nixpkgs.tmux \
    nixpkgs.tree \
    nixpkgs.unrar \
    nixpkgs.watch \
    nixpkgs.wget \
    nixpkgs.xcp \
    nixpkgs.xz \
    nixpkgs.youtube-dl \
    nixpkgs.yt-dlp
