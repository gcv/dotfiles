### enable Nix (must happen early for correct path setup)

if not set -q NIX_PATH
    [ -e ~/.nix-defexpr ] && set NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
    if [ -e /etc/ssl/certs/ca-certificates.crt ]    # NixOS, Ubuntu, Debian, Gentoo, Arch
        set NIX_SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt
    else if [ -e /etc/pki/tls/certs/ca-bundle.crt ] # Fedora, CentOS
        set NIX_SSL_CERT_FILE /etc/pki/tls/certs/ca-bundle.crt
    else if [ -e ~/.nix-profile ]
        set NIX_SSL_CERT_FILE "$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt"
    end
end
[ -e "/nix/var/nix/profiles/per-user/$USER" ] && set NIX_USER_PROFILE_DIR "/nix/var/nix/profiles/per-user/$USER"


### environment variables

set fish_greeting
set DO_NOT_TRACK 1 # https://consoledonottrack.com
set LESSHISTFILE /dev/nul


### path setup

fish_add_path --path --prepend ~/.nix-profile/sbin
fish_add_path --path --prepend ~/.nix-profile/bin
fish_add_path --path --prepend ~/.local/sbin
fish_add_path --path --prepend ~/.local/bin


### interactive mode only sessions:

if status is-interactive

   type -q autojump && source ~/.nix-profile/share/autojump/autojump.fish
   type -q direnv && direnv hook fish | source
   type -q starship && starship init fish | source

   set fish_history "fish_$(hostname -s)"

end
