#!/usr/bin/env bash


if [[ ${UNAME} != "Darwin" ]]; then
    echo "do not run this on anything other than macOS!"
    exit 1
fi


BREW_HOME=/opt/brew

if [[ ! -d "$BREW_HOME" ]]; then

    sudo mkdir -p "${BREW_HOME}"
    sudo chown -R ${USER}:wheel "${BREW_HOME}"

    git clone git://github.com/Homebrew/brew.git "${BREW_HOME}"

    # If /usr/bin/git is not available, the following should work:
    # curl -L -o "${BREW_HOME}/brew.zip" https://github.com/Homebrew/brew/archive/master.zip
    # pushd "${BREW_HOME}"
    # unzip "./brew.zip"
    # mv brew-master/* .
    # mv brew-master/.* .
    # rmdir brew-master
    # bin/brew update
    # popd

fi


# system basics
${BREW_HOME}/bin/brew install \
    bash \
    coreutils \
    findutils \
    python \
    python@2 \
    p7zip \
    xz \
    zsh


# utilities and apps
${BREW_HOME}/bin/brew install \
    aspell \
    autojump \
    exiftool \
    fdupes \
    fzf \
    git \
    git-lfs \
    gnupg@1.4 \
    htop \
    jq \
    ledger \
    magic-wormhole \
    mosh \
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
    youtube-dl


# external service tools
# excluded: heroku, due to nasty dependency (node)
${BREW_HOME}/bin/brew install \
    awscli


# C and C++ development tools
${BREW_HOME}/bin/brew install \
    autoconf \
    automake \
    cmake \
    ctags \
    global \
    swig


# libraries
${BREW_HOME}/bin/brew install \
    boost \
    gnutls \
    gsl \
    icu4c \
    p11-kit \
    taglib


# external development environment management tools
# excluded: roswell, haskell-stack, rbenv, pyenv, rustup (horrible dependency story)
${BREW_HOME}/bin/brew install \
    leiningen \
    maven \
    nvm


# Borg Backup cask
#${BREW_HOME}/bin/brew cask install borgbackup
