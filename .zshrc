### environmental variables
export EDITOR=vi
export LESS="-M -R"
export PAGER=less
export CVS_RSH=ssh
export LC_CTYPE=en_US.UTF-8
export LC_COLLATE=C


### path
typeset -U path
path=(/opt/local/bin /usr/local/bin /usr/local/sbin $path)
export MANPATH=$MANPATH:/opt/local/man


### aliases
alias rm='rm -i'
alias dir='ls -aCF'
alias v='ls -lahF'
alias d='dirs -v'

if [[ `uname` == "Darwin" &&
      -a '/Applications/Emacs.app/Contents/MacOS/bin/emacsclient' ]]; then
    alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n'
else
    alias ec='emacsclient -n'
fi

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'


### ls color output
export LS_COLORS="no=00:fi=00:di=95:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:"


### shell history settings
HISTSIZE=10000
HISTFILE=~/.history
SAVEHIST=10000


### shell settings
bindkey -e                        # Emacs keybindings
ulimit -c 0                       # no core dumps
ulimit -s unlimited               # stop limiting the stack


### keyboard settings
bindkey '^?' backward-delete-char     # Mac keyboard backspace
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward
bindkey '^[[1~' beginning-of-line
bindkey '^[[4~' end-of-line
bindkey '^[[2~' overwrite-mode
bindkey '^[[3~' delete-char
bindkey '^[[6~' end-of-history
bindkey '^[[5~' beginning-of-history
bindkey '^[^I' reverse-menu-complete
bindkey '^[OA' up-line-or-history
bindkey '^[[A' up-line-or-history
bindkey '^[[B' down-line-or-history
bindkey '^[OB' down-line-or-history
bindkey '^[OD' backward-char
bindkey '^[OC' forward-char
bindkey '^[[[A' run-help
bindkey '^[[[B' which-command
bindkey '^[[[C' where-is


### history options
setopt extended_history           # saves timestamps on history
setopt hist_expire_dups_first     # expire history duplicates first
setopt hist_no_store              # don't save 'history' cmd in history
setopt inc_append_history         # append history incrementally
setopt share_history              # share history between open shells


### shell options
setopt auto_cd                    # automatically execute chdir
setopt check_jobs                 # check jobs before exiting the shell
setopt correct                    # correct spelling of commands
setopt correct_all                # correct spelling of each word
setopt extended_glob              # globs #, ~, and ^
setopt long_list_jobs             # use the long format for job listings
setopt nobeep                     # stop beeping
setopt noclobber                  # do not clobber existing files
setopt noflow_control             # don't use flow control (^S/^Q)
setopt nohup                      # do not terminate child processes on exit
setopt notify                     # tell me when jobs terminate
setopt prompt_subst               # allow prompt variable substitution


### directory stack customization
setopt auto_pushd                 # automatic directory stack
setopt pushd_minus                # swap +/- for pushd and popd
setopt pushd_silent               # no output after pushd and popd
setopt pushd_to_home              # pushd with no arguments goes to ~
setopt pushd_ignore_dups          # no duplicates in directory stack


### command completion
zstyle ':completion:*' completer _expand _complete _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-compctl false
autoload -Uz compinit
compinit


### colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval COLOR_BOLD_$color="%{$terminfo[bold]$fg[${(L)color}]%}"
    eval COLOR_$color="%{$fg[${(L)color}]%}"
done
COLOR_NONE="%{$terminfo[sgr0]%}"


### prompt (PROMPT is equivalent to PS1, RPROMPT to RPS1)
# PROMPT="[%m:%4c] "
PROMPT="%(!.${COLOR_RED}.${COLOR_NONE})[%m:%4c]${COLOR_NONE} "
RPROMPT="%(?..${COLOR_RED}[%?]${COLOR_NONE})"


### title bar or status bar
case $TERM in
    xterm*|rxvt*|cygwin)
        precmd() {
            print -Pn "\e]0;%n@%m: %~\a"
        }
        preexec() {
            # print -Pn "\e]0;%n@%m: %~ [$1]\a"
        }
        ;;
    screen)
        precmd() {
            print -Pn "\e]0;%n@%m: %~\a"
            print -Pn "\ek \e\\"
        }
        preexec () {
            # print -Pn "\e]0;%n@%m: %~ [$1]\a"
            print -Pn "\ek${1[(wr)^(*=*|sudo|ssh|-*)]}\e\\"
        }
        ;;
esac


### TRAMP environment settings
if [[ $TERM == "dumb" ]]; then
    PS1="%m%# "
    unsetopt zle
fi


### pskill <process name>
function pskill() {
   kill -9 $(ps -aux | grep $1 | grep -v grep | awk '{ print $1 }')
   echo -n "Killed $1."
}


### Search for files containing the given string in the current
### directory.
function findf() {
    find . \( -path '*.svn' -o -path '*.git' \) -prune -o -type f -print0 | \
        xargs -0 grep -I -l -i -e $1
}


### Search for files containing the given string in the current directory,
### and print lines with the string.
function findl() {
    find . \( -path '*.svn' -o -path '*.git' \) -prune -o -type f -print0 | \
        xargs -0 grep -I -n -i -e $1
}
