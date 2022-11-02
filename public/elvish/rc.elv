use path


### keys

use readline-binding
# reset C-l and C-n
set edit:insert:binding[Ctrl-L] = $edit:location:start~
set edit:insert:binding[Ctrl-N] = $edit:navigation:start~


### enable Nix (must happen early for correct path setup)

if (not (has-env NIX_PATH)) {
  if (path:is-dir ~/.nix-defexpr) {
    set-env NIX_PATH "nixpkgs="(get-env HOME)"/.nix-defexpr/channels/nixpkgs123"
    if (path:is-regular /etc/ssl/certs/ca-certificates.crt) { # NixOS, Ubuntu, Debian, Gentoo, Arch
      set-env NIX_SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt
    } elif (path:is-regular /etc/pki/tls/certs/ca-bundle.crt) { # Fedora, CentOS
      set-env NIX_SSL_CERT_FILE /etc/pki/tls/certs/ca-bundle.crt
    } elif (path:is-dir (path:eval-symlinks (path:abs ~/.nix-profile))) {
      set-env NIX_SSL_CERT_FILE (get-env HOME)/.nix-profile/etc/ssl/certs/ca-bundle.crt
    }
  }
}

if (path:is-dir "/nix/var/nix/profiles/per-user/"(get-env USER)) {
  set-env NIX_USER_PROFILE_DIR "/nix/var/nix/profiles/per-user/"(get-env USER)
}


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
