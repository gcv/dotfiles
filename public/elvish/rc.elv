use path


### keys

use readline-binding
# reset C-l to use the directory navigator
set edit:insert:binding[Ctrl-L] = $edit:location:start~


### TODO: enable Nix (must happen early for correct path setup)


### environment variables

set-env DO_NOT_TRACK 1 # https://consoledonottrack.com
set-env LANG en_US.UTF-8
set-env LC_ALL en_US.UTF-8
set-env LESSHISTFILE /dev/null


### path setup

set paths = [
  ~/.nix-profile/sbin
  ~/.nix-profile/bin
  ~/.local/sbin
  ~/.local/bin
  $@paths
]


### external tools

if (has-external starship) {
  eval (starship init elvish)
}

if (has-external direnv) {
  # TODO: Broken in elvish 0.18 and direnv 2.32.
  # eval (direnv hook elvish | slurp)
}