#!/usr/bin/env bash


NIX_HOME=/nix

if [[ ! -d "${NIX_HOME}" ]]; then
    sudo mkdir -m 0755 "${NIX_HOME}" && sudo chown `whoami` "${NIX_HOME}"
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
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
    # spelling checkers
    aspell
    aspellDicts.en aspellDicts.en-computers aspellDicts.en-science aspellDicts.ru
    #nuspell
    #hunspellDicts.en_US hunspell.ru_RU
    # everything else:
    #ansible
    autoconf
    autojump
    automake
    awscli2
    #babashka
    bandwhich          # network monitor
    bat                # cat replacement
    bashInteractive    # when needed
    bitwarden-cli      # password manager
    bottom             # top replacement 2 (Rust)
    broot              # file browser / manager
    btop               # top replacement 3 (C++)
    cacert             # SSL certificates
    cmake
    coreutils
    croc               # peer-to-peer file transfer, directory capable (cf magic-wormhole)
    ctags
    direnv
    du-dust            # du replacement
    elvish             # good for shell scripting; as of v0.18 too raw for interactive use
    emacs-nox
    entr               # watch a file for changes and act on them
    exif
    eza                # ls replacement
    fd                 # find replacement
    fdupes
    ffmpeg
    findutils
    fzf
    getopt             # enhanced option parser for shell scripts
    git
    git-lfs
    gnupg
    gnutar
    helix              # modal text editor, alternative to (neo)vi(m)
    htmlq              # command line HTML parser and query tool (like jq)
    htop               # top replacement 1
    httpie             # HTTP client, alternative to curl
    httrack            # web scraper, more stable than wget
    hyperfine          # benchmarking tool
    jq
    #julia
    lazydocker         # Docker UI
    lazygit            # Git UI
    ledger
    libvterm-neovim
    magic-wormhole     # peer-to-peer file transfer (cf croc)
    mediainfo
    mosh               # since 1.4.0, should not need to be built from mosh-git.nix derivation
    miniserve          # HTTP server
    murex              # another shell with interesting scripting ideas and decent interactivity
    ngrok              # expose local service over a secure tunnel
    nix-direnv
    nmap
    nnn                # file manager
    notmuch            # mail indexer
    p7zip
    pandoc
    pass               # password manager
    pastel             # color utility
    pop                # mail sender
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
    tree              # file lister, obsoleted by eza and broot
    tree-sitter       # parser framework
    unrar
    viddy             # watch replacement
    watch
    wget              # weak web scraper (no restart support)
    xcp               # cp replacement
    xz
    #youtube-dl        # outdated?
    yt-dlp            # new youtube-dl
    zsh
)

export NIXPKGS_ALLOW_UNFREE=1
for pkg in "${packages[@]}"; do
    nix profile --verbose install --impure "nixpkgs#${pkg}"
done

