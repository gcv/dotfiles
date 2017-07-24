#!/bin/bash


if [[ ${UNAME} != "Darwin" ]]; then
    echo "do not run this on anything other than macOS!"
    exit 1
fi


BREW_HOME=/opt/brew

if [[ ! -d "$BREW_HOME" ]]; then

    GITHUB_APP_GIT=/Applications/GitHub.app/Contents/Resources/git/bin/git
    if [[ ! -f $GITHUB_APP_GIT ]]; then
        echo "download the GitHub app and try again"
        exit 1
    fi

    sudo mkdir -p "${BREW_HOME}"
    sudo chown -R ${USER}:wheel "${BREW_HOME}"

    ${GITHUB_APP_GIT} clone git://github.com/Homebrew/brew.git "${BREW_HOME}"

fi


${BREW_HOME}/bin/brew install \
    `# system basics` \
    bash \    
    coreutils \
    findutils \
    python3 \
    xz \
    zsh \
    `# utilities and apps` \
    aspell \
    autojump \
    git \
    gnupg@1.4 \
    htop \
    ledger \
    magic-wormhole \
    nmap \
    offlineimap \
    pv \
    rlwrap \
    sqlite \
    the_silver_searcher \
    tmux \
    tree \
    watch \
    wget \
    youtube-dl \
    `# external service tools` \
    awscli \
    `#heroku # nasty node dependency introduced` \
    `# C and C++ development tools` \
    autoconf \
    automake \
    cmake \
    ctags \
    global \
    swig \
    `# libraries` \
    boost \
    gsl \
    icu4c \
    taglib \
    `# external development environment management tools` \
    haskell-stack \
    leiningen \
    maven \
    `# nvm` \
    `# rbenv` \
    `# pyenv` \
    `# rustup # pretty broken dependency story` \
    roswell
