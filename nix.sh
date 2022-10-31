#!/usr/bin/env bash


NIX_HOME=/nix

if [[ ! -d "${NIX_HOME}" ]]; then
    sudo mkdir -m 0755 "${NIX_HOME}" && chown `whoami` "${NIX_HOME}"
    sh <(curl https://nixos.org/nix/install) --no-daemon
fi


# Recent Darwin releases provide reasonable versions of:
# - bash
# - zsh
# - git
# - sqlite
# The following are similar:
# - croc and magic-wormhole
# - bat and cat
# - exa and ls
packages=(
    aspell
    aspell-dict-en
    autoconf
    autojump
    automake
    awscli2
    bat                # cat replacement
    bashInteractive    # when needed
    bitwarden-cli      # password manager
    cacert             # SSL certificates
    cmake
    coreutils
    croc               # peer-to-peer file transfer, directory capable
    ctags
    direnv
    elvish             # good for shell scripting; as of v0.18 too raw for interactive use
    emacs-nox
    exa                # ls replacement
    exif
    fd                 # find replacement
    fdupes
    ffmpeg
    findutils
    fish
    fzf
    git
    git-lfs
    gnupg
    gnutar
    helix              # modal text editor, alternative to (neo)vi(m)
    htop
    httpie             # HTTP client, alternative to curl
    httrack            # web scraper, more stable than wget
    hyperfine          # benchmarking tool
    jq
    lazydocker         # Docker UI
    lazygit            # Git UI
    ledger
    libvterm-neovim
    magic-wormhole     # peer-to-peer file transfer
    mediainfo
    #mosh              # may need to be built from mosh-git.nix derivation
    miniserve          # HTTP server
    ngrok              # expose local service over a secure tunnel
    nix-direnv
    nmap
    notmuch            # mail indexer
    p7zip
    pandoc
    pass               # password manager
    pastel             # color utility
    pv                 # pipe viewer
    rclone
    restic             # backup utility
    ripgrep
    rlwrap
    rsync
    sd                 # a simpler sed for common cases
    silver-searcher    # ag
    sqlite
    starship           # fancy prompt, good with fish
    tectonic           # (La)TeX environment
    texinfo
    tmux
    tree
    unrar
    viddy             # watch replacement
    watch
    wget              # weak web scraper (no restart support)
    xcp               # cp replacement
    xz
    youtube-dl        # outdated?
    yt-dlp            # new youtube-dl
    zsh
)

NIXPKGS_ALLOW_UNFREE=1
for pkg in "${packages[@]}"; do
    nix profile --verbose install --impure "nixpkgs#${pkg}"
done

