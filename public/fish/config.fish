set fish_greeting

# commands to run in interactive sessions can go here:
if status is-interactive

   type -q autojump && source ~/.nix-profile/share/autojump/autojump.fish
   type -q direnv && direnv hook fish | source
   type -q starship && starship init fish | source
   if test -e ~/.nix-profile/share/fzf/key-bindings.fish
       source ~/.nix-profile/share/fzf/key-bindings.fish && fzf_key_bindings
   else if test -e /usr/share/fzf/fzf-key-bindings.fish
       source /usr/share/fzf/fzf-key-bindings.fish && fzf_key_bindings
   else if test -e /usr/local/share/fzf/fzf-key-bindings.fish
       source /usr/local/share/fzf/fzf-key-bindings.fish && fzf_key_bindings
   end

end
