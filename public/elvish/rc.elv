use path


### keys

use readline-binding
# reset C-l and C-n
set edit:insert:binding[Ctrl-L] = $edit:location:start~
set edit:insert:binding[Ctrl-N] = $edit:navigation:start~


### TODO: Deal with Nix (https://nixos.wiki/wiki/Elvish)


### TODO: Deal with Devbox (Nix)


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


### aliases

fn ls { |@a| e:ls --color --sort=version $@a }
fn v { |@a| ls -lahF $@a }
fn grep { |@a| e: grep --color $@a }
fn rm { |@a| e:rm -i $@a }
fn cp { |@a| e:cp -p $@a }
fn ... { cd ../.. }
fn .... { cd ../../.. }
fn ..... { cd ../../../.. }
fn ...... { cd ../../../../.. }
fn ....... { cd ../../../../../.. }


### direnv

if (has-external direnv) {
  # This should just be:
  # eval (direnv hook elvish)
  # but does not work due to some outdated syntax in the direnv code.
  set @edit:before-readline = $@edit:before-readline {
    try {
      var m = [(direnv export elvish | from-json)]
      if (> (count $m) 0) {
        set m = (all $m)
        keys $m | each { |k|
          if $m[$k] {
            set-env $k $m[$k]
          } else {
            unset-env $k
          }
        }
      }
    } catch e {
      echo $e
    }
  }
}


### TODO: deal with ssh-agent


### external tools

if (has-external starship) {
  eval (starship init elvish)
}

if (has-external direnv) {
  # TODO: Broken in elvish 0.18 and direnv 2.32.
  # eval (direnv hook elvish | slurp)
}
