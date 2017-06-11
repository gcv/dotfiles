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
     aspell \
     autoconf \
     autojump \
     automake \
     awscli \
     bash \
     boost \
     cmake \
     coreutils \
     ctags \
     findutils \
     git \
     global \
     gnupg@1.4 \
     gsl \
     haskell-stack \
     heroku \
     icu4c \
     ledger \
     leiningen \
     libtool \
     makedepend \
     maven \
     nmap \
     offlineimap \
     pkg-config \
     pv \
     python3 \
     rlwrap \
     roswell \
     sqlite \
     swig \
     taglib \
     the_silver_searcher \
     tmux \
     tree \
     watch \
     wget \
     xz \
     youtube-dl \
     zsh
