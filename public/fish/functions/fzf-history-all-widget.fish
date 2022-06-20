function fzf-history-all-widget --description "fzf-based history search across all shells"
    eval fzf-history-all fish | read -l selected
    and commandline -- $selected
    commandline -f repaint
end
