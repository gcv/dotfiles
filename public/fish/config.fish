set fish_greeting

# commands to run in interactive sessions can go here:
if status is-interactive

   type -q autojump && source ~/.nix-profile/share/autojump/autojump.fish
   type -q direnv && direnv hook fish | source
   type -q starship && starship init fish | source

   set fish_history "fish_$(hostname -s)"

end
