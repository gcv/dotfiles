### environmental variables
export EDITOR=vi
export LESS="-M -R"
export PAGER=less
export CVS_RSH=ssh
export LC_CTYPE=en_US.UTF-8
export LC_COLLATE=C


### path
typeset -U path
path=($HOME/sw/bin /opt/local/bin /usr/local/bin /usr/bin /bin $HOME/sw/sbin /opt/local/sbin /usr/local/sbin /usr/sbin /sbin $path)
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

if [[ `uname` == "Linux" ]]; then
    alias ls='ls --color'
elif [[ `uname` == "Darwin" &&
        -a '/opt/local/bin/gls' ]]; then
    alias ls='gls --color'
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


### history options
setopt extended_history           # saves timestamps on history
setopt hist_expire_dups_first     # expire history duplicates first
setopt hist_reduce_blanks         # remove superfluous whitespace
setopt hist_no_store              # do not save 'history' cmd in history
setopt hist_save_no_dups          # do not save duplicates
setopt hist_verify                # preview history expansions
# setopt inc_append_history       # append each command (c.f. share_history)
setopt share_history              # share commands between shells (c.f. inc_append_history)


### shell options
setopt auto_cd                    # automatically execute chdir
setopt check_jobs                 # check jobs before exiting the shell
setopt correct                    # correct spelling of commands
setopt correct_all                # correct spelling of each word
setopt extended_glob              # globs #, ~, and ^
setopt long_list_jobs             # use the long format for job listings
setopt nobeep                     # stop beeping
setopt nohist_beep                # turn off no-history item beeps
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


### edit command line in text editor (bound to M-e below)
autoload -U edit-command-line
zle -N edit-command-line


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
bindkey '^@' set-mark-command
bindkey '^W' copy-region-as-kill
bindkey ' ' magic-space
bindkey '\ee' edit-command-line


### command completion
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-compctl false
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' menu yes select
# initialize
autoload -Uz compinit
compinit


### colors
autoload -U colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval COLOR_BOLD_$color="%{$terminfo[bold]$fg[${(L)color}]%}"
    eval COLOR_$color="%{$fg[${(L)color}]%}"
done
COLOR_NONE="%{$terminfo[sgr0]%}"


### prompt (PROMPT is equivalent to PS1, RPROMPT to RPS1)
# PROMPT="[%m:%4~] "
PROMPT="%(!.${COLOR_RED}.${COLOR_NONE})[%m:%4~]${COLOR_NONE} "
RPROMPT="%(?..${COLOR_RED}[%?]${COLOR_NONE})"


### title bar or status bar
case $TERM in
    xterm*|rxvt*|cygwin)
        precmd() {
            print -Pn "\e]0;%n@%m (%y): %5~\a"
        }
        preexec() {
            # print -Pn "\e]0;%n@%m (%y): %5~ [$1]\a"
        }
        ;;
    screen)
        precmd() {
            print -Pn "\e]0;%n@%m (%y): %5~\a"
            print -Pn "\ek \e\\"
        }
        preexec () {
            # print -Pn "\e]0;%n@%m (%y): %5~ [$1]\a"
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


### Directory stack jump: cd to an entry in the directory stack which
### matches the given regexps.
function dsj() {
    local words="$@"
    local regex="${words/\ /.*}"
    local matching_dirs=$( dirs -lp | sort -u | grep "$regex" )
    local dir_to_switch_to=$( echo $matching_dirs | head -1 )
    cd "$dir_to_switch_to"
}


### Maintains a jump-list of used directories. Adapted for zsh from
### http://github.com/rupa/j
function j() {
    local jfile=$HOME/.j

    # add to jump list
    [ "$1" = "--add" -o "$1" = "-a" ] && {
        shift
        # ignore $HOME, it would dominate the jump list
        [ "$*" = "$HOME" ] && return
        # else...
        awk -v q="$*" -v t="$(date +%s)" -F"|" '
$2 >= 1 {
    if ($1 == q) {
        l[$1] = $2 + 1
        d[$1] = t
        found = 1
    }
    else {
        l[$1] = $2
        d[$1] = $3
        count += $2
    }
}
END {
    if (!found) l[q] = 1 && d[q] = t
    if (count > 1000) {
        for (i in l) print i "|" 0.9*l[i] "|" d[i] # aging
    }
    else for (i in l) print i "|" l[i] "|" d[i]
}
        ' $jfile 2>/dev/null > $jfile.tmp
        mv -f $jfile.tmp $jfile
        return
    }

    # autocompletion support
    [ "$1" = "--complete" -o "$1" = "-c" ] && {
        awk -v q="$2" -F"|" '
BEGIN { split(substr(q, 3), a, " ") } {
    if (system("test -d \"" $1 "\"")) next
    for (i in a) $1 !~ a[i] && $1 = ""; if ($1) print $1
}
        ' $jfile 2>/dev/null
        return
    }

    # navigation and other commands
    local x
    local out
    for x; do
        case $x in
            -h|--help)   echo "j [--h[elp]] [--r] [--l] [regex1 ... regexn]"; return;;
            -l|--list)   local list=1;;
            -r|--recent) local recent=r;;
            -s|--short)  local short=1;;
            *)           local out="$out $x";;
        esac;
        shift;
    done

    set -- $out

    if [ -z "$1" -o "$list" ]; then
        [ "$short" ] && return
        awk -v q="$*" -v t="$(date +%s)" -v r="$recent" -F"|" '
BEGIN {
    f = 2;
    split(q, a, " ");
    if (r) f = 3
}
{
    if (system("test -d \"" $1 "\"")) next
    for (i in a) $1 !~ a[i] && $1 = ""
    if ($1) if (f == 3) { print t - $f "\t" $1 } else print $f "\t" $1
}
        ' $jfile 2>/dev/null | sort -n$recent

    # not sure what this does...
    elif [ -d "/${out#*/}" ]; then
        cd "/${out#*/}"

    # prefer case sensitive
    else
        out=$(awk -v q="$*" -v s="$short" -v r="$recent" -F"|" '
BEGIN {
    split(q, a, " ");
    if (r) { f = 3 } else f = 2
}
{
    if (system("test -d \"" $1 "\"")) next
    for (i in a) $1 !~ a[i] && $1 = ""
    if ($1) {
        if (s) {
            if (length($1) <= length(x)) {
                x = $1
            }
            else if (! x) x = $1
        }
        else if ($f >= dx) { x = $1; dx = $f }
    }
}
END {
    if (! x) {
        close(FILENAME)
        while (getline < FILENAME) {
            if (system("test -d \"" $1 "\"")) continue
            for (i in a) tolower($1) !~ tolower(a[i]) && $1 = ""
            if ($1) {
                if (s) {
                    if (length($1) <= length(x)) {
                        x = $1
                    }
                    else if (! x) x = $1
                } else if ($f >= dx) { x = $1; dx = $f }
            }
        }
    }
    if (x) print x
}
        ' $jfile)
        [ "$out" ] && cd "$out"
    fi
}

alias jr="j -r"
alias js="j -s"

function _j() {
    compadd -M 'l:|=* r:|=*' -M 'm:{a-zA-Z}={A-Za-z}' $(j --complete)
}

compdef _j j

function j_precmd() {
    j --add "$(pwd -P)"
}

precmd_functions+=(j_precmd)
