#!/usr/bin/env bash

set -euo pipefail

command_exists() {
  type -f "$1" >/dev/null 2>&1
}

case "$OSTYPE" in
  darwin*)
    if ! command_exists brew
    then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if ! command_exists python3
    then
      brew install python3
    fi

    for PACKAGE in coreutils gnu-tar
    do
      if [ ! -f "/usr/local/opt/$PACKAGE" ]
      then
        brew install "$PACKAGE"
      fi
      PATH="/usr/local/opt/$PACKAGE/libexec/gnubin:$PATH"
    done
 ;;
  linux*)
    DISTRO=$(grep '^ID=' /etc/os-release | cut -d = -f 2)

    case $DISTRO in
      debian)
        if ! command_exists python3
        then
          sudo apt install --yes python3{,-pip}
        fi
        ;;
    esac
esac
