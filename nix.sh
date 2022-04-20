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
    nixpkgs.bat \               # cat replacement
    nixpkgs.bitwarden-cli \     # password manager
    nixpkgs.cmake \
    nixpkgs.coreutils \
    nixpkgs.croc \              # peer-to-peer file transfer, directory capable
    nixpkgs.ctags \
    nixpkgs.direnv \
    nixpkgs.exa \               # ls replacement
    nixpkgs.exif \
    nixpkgs.fd \                # find replacement
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
    nixpkgs.joshuto \           # file manager
    nixpkgs.lazydocker \        # Docker UI
    nixpkgs.lazygit \           # Git UI
    nixpkgs.ledger \
    nixpkgs.libvterm-neovim \
    nixpkgs.magic-wormhole \    # peer-to-peer file transfer
    nixpkgs.mosh \
    nixpkgs.miniserve \         # HTTP server
    nixpkgs.ngrok \             # expose local service over a secure tunnel
    nixpkgs.nix-direnv \
    nixpkgs.nmap \
    nixpkgs.notmuch \           # mail indexer
    nixpkgs.offlineimap \       # mail fetcher
    nixpkgs.p7zip \
    nixpkgs.pandoc \
    nixpkgs.password-store \    # password manager
    nixpkgs.pastel \            # color utility
    nixpkgs.pv \                # pipe viewer
    nixpkgs.ranger \            # file manager
    nixpkgs.rclone \
    nixpkgs.restic \
    nixpkgs.ripgrep \
    nixpkgs.rlwrap \
    nixpkgs.sd \                # a simpler sed for common cases
    nixpkgs.silver-searcher \   # ag
    nixpkgs.starship \          # fancy prompt, good with fish
    nixpkgs.tectonic \          # (La)TeX environment
    nixpkgs.texinfo \
    nixpkgs.tmux \
    nixpkgs.tree \
    nixpkgs.unrar \
    nixpkgs.watch \
    nixpkgs.wget \
    nixpkgs.xcp \               # cp replacement
    nixpkgs.xz \
    nixpkgs.youtube-dl \        # outdated?
    nixpkgs.yt-dlp              # new youtube-dl
