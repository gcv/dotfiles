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
# - eza and ls
packages=(
    # spelling checkers
    aspell
    aspellDicts.en aspellDicts.en-computers aspellDicts.en-science aspellDicts.ru
    #nuspell
    #hunspellDicts.en_US hunspell.ru_RU
    # shells and shell scripting utilities
    bashInteractive    # when needed
    elvish             # good for shell scripting; as of v0.18 too raw for interactive use
    murex              # another shell with interesting scripting ideas and decent interactivity
    shellcheck         # shell script linter
    starship           # fancy prompt, good with fish
    zsh
    # file managers
    broot              # file browser / file manager 2
    mc                 # file browser / file manager 1 (Midnight Commander)
    yazi               # file browser / file manager 3 (Rust)
    # system monitors
    bandwhich          # network monitor
    bottom             # top replacement 2 (Rust)
    btop               # top replacement 3 (C++)
    htop               # top replacement 1
    # utility replacements
    bat                # cat replacement
    coreutils
    delta              # nice diff, use with pipe from `git diff`
    diffr              # nice diff, use with pipe from `git diff`
    difftastic         # nice AST-aware diff
    du-dust            # du replacement
    eza                # ls replacement
    fd                 # find replacement
    findutils
    sd                 # a simpler sed for common cases
    viddy              # watch replacement
    watch
    xcp                # cp replacement
    xz
    # everything else:
    #ansible
    autoconf
    autojump
    automake
    awscli2
    #babashka
    cacert             # SSL certificates
    chafa              # terminal image viewer
    cmake
    croc               # peer-to-peer file transfer, directory capable (cf magic-wormhole)
    ctags
    direnv
    emacs-nox
    entr               # watch a file for changes and act on them
    exif
    fdupes
    ffmpeg
    fzf
    getopt             # enhanced option parser for shell scripts
    git
    git-lfs
    gnupg
    gnutar
    helix              # modal text editor, alternative to (neo)vi(m)
    hledger            # Haskell reimplementation of Ledger CLI (accounting)
    htmlq              # command line HTML parser and query tool (like jq)
    httpie             # HTTP client, alternative to curl
    httrack            # web scraper, more stable than wget
    hyperfine          # benchmarking tool
    jnv                # interactive JSON viewer with jq query preview
    jq                 # JSON query utility
    #julia
    keepassxc          # password manager
    lazydocker         # Docker UI
    lazygit            # Git UI
    ledger             # Ledger CLI (accounting)
    libvterm-neovim
    magic-wormhole     # peer-to-peer file transfer (cf croc)
    mediainfo
    mosh               # since 1.4.0, should not need to be built from mosh-git.nix derivation
    miniserve          # HTTP server
    ngrok              # expose local service over a secure tunnel
    nix-direnv
    nmap
    notmuch            # mail indexer
    p7zip
    pandoc
    pastel             # color utility
    pop                # mail sender
    pinentry-tty       # GPG passphrase entry program for terminal
    pv                 # pipe viewer
    rclone
    restic             # backup utility
    ripgrep
    rlwrap
    rsync
    silver-searcher    # ag
    sqlite
    tectonic           # (La)TeX environment
    texinfo
    tgpt               # terminal GPT client
    tmux
    tree               # file lister, obsoleted by eza and broot
    tree-sitter        # parser framework
    typst              # TeX replacement
    unrar
    wget               # weak web scraper (no restart support)
    yt-dlp             # new youtube-dl
)

export NIXPKGS_ALLOW_UNFREE=1
for pkg in "${packages[@]}"; do
    nix profile --verbose install --impure "nixpkgs#${pkg}"
done

