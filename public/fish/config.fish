set fish_greeting

# commands to run in interactive sessions can go here:
if status is-interactive

   type -q autojump && source ~/.nix-profile/share/autojump/autojump.fish
   type -q direnv && direnv hook fish | source
   type -q starship && starship init fish | source
   test -e ~/.nix-profile/share/fzf/key-bindings.fish && source ~/.nix-profile/share/fzf/key-bindings.fish && fzf_key_bindings

end
