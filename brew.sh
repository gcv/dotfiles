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


${BREW_HOME}/bin/brew install \
    libtool \
    gnutls \
    libvterm
