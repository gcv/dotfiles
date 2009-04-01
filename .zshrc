autoload -Uz compinit                                      # prepare completion
autoload -Uz edit-command-line && zle -N edit-command-line # bound to M-e below
autoload -Uz colors zsh/terminfo                           # symbolic colors
autoload -Uz is-at-least                                   # zsh version checks
autoload -Uz vcs_info                                      # VCS status
bindkey -e                                                 # Emacs keybindings
ulimit -c 0                                                # no core dumps
ulimit -s unlimited                                        # no stack limits


### Paths include locally-installed packages under ~/sw as symbolic
### links to the active version:
###   ./configure --prefix=~/sw/package-version && make && make install
###   ln -s ~/sw/package-version ~/sw/package
typeset -U path
path=(
    ~/sw/*(@Ne:'[[ -d ${REPLY}/bin ]] && REPLY=${REPLY}/bin':)
    ~/sw/*(@Ne:'[[ -d ${REPLY}/sbin ]] && REPLY=${REPLY}/sbin':)
    /opt/local/bin /opt/local/sbin
    /usr/local/bin /usr/local/sbin
    /usr/bin /usr/sbin
    /bin /sbin
    $path
)

typeset -U manpath
manpath+=(
    ~/sw/*(@Ne:'[[ -d ${REPLY}/man ]] && REPLY=${REPLY}/man':)
    ~/sw/*(@Ne:'[[ -d ${REPLY}/share/man ]] && REPLY=${REPLY}/share/man':)
    /opt/local/man
)
export MANPATH=${MANPATH}:                                 # append colon


### Switch to a more recent zsh if found in path. Be sure not to
### initialize completion system until after this happens.
if [[ -x $(whence zsh) ]]; then
    is-at-least $(zsh --version | awk '{print $2}') || exec zsh
fi


### environmental variables
export EDITOR=vi
export LESS="-M -R"
export PAGER=less
export CVS_RSH=ssh
export LC_CTYPE=en_US.UTF-8
export LC_COLLATE=C
export UNAME=`uname`


### aliases
alias rm='rm -i'
alias dir='ls -aCF'
alias v='ls -lahF'
alias d='dirs -v'

[[ ${UNAME} == "Darwin" && -x "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" ]] && \
    alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n' || \
    alias ec='emacsclient -n'

[[ ${UNAME} == "Linux" ]] && alias ls='ls --color'
[[ ${UNAME} == "Darwin" && -x "/opt/local/bin/gls" ]] && alias ls='gls --color'

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
setopt no_correct_all             # do not correct spelling of files (annoying)
setopt extended_glob              # globs #, ~, and ^
setopt numeric_glob_sort          # orders files numerically
setopt long_list_jobs             # use the long format for job listings
setopt no_beep                    # stop beeping
setopt no_hist_beep               # turn off no-history item beeps
setopt no_clobber                 # do not clobber existing files
setopt no_flow_control            # don't use flow control (^S/^Q)
setopt no_hup                     # do not terminate child processes on exit
setopt notify                     # tell me when jobs terminate
setopt prompt_subst               # allow prompt variable substitution
setopt transient_rprompt          # remove previous rprompt after command


### directory stack customization
setopt auto_pushd                 # automatic directory stack
setopt pushd_minus                # swap +/- for pushd and popd
setopt pushd_silent               # no output after pushd and popd
setopt pushd_to_home              # pushd with no arguments goes to ~
setopt pushd_ignore_dups          # no duplicates in directory stack


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
compinit
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


### colors
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval COLOR_BOLD_$color="%{$terminfo[bold]$fg[${(L)color}]%}"
    eval COLOR_$color="%{$fg[${(L)color}]%}"
done
COLOR_NONE="%{$terminfo[sgr0]%}"


### vcs_info configuration
zstyle ':vcs_info:*' formats " %s:${COLOR_GREEN}%b${COLOR_NONE}"
zstyle ':vcs_info:*' actionformats " %s:${COLOR_GREEN}%b${COLOR_NONE} ${COLOR_RED}%a${COLOR_NONE}"
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat "${COLOR_GREEN}%b${COLOR_NONE}:%r"

function precmd_vcs_info() {
    vcs_info
}

is-at-least "4.3.7" && precmd_functions+=( precmd_vcs_info )


### prompt (PROMPT is equivalent to PS1, RPROMPT to RPS1)
# PROMPT="[%m:%4~] "
PROMPT="%(!.${COLOR_RED}.${COLOR_NONE})[%m:%4~"'${vcs_info_msg_0_}'"]${COLOR_NONE} "
RPROMPT="%(?..${COLOR_RED}[%?]${COLOR_NONE})"


### title bar or status bar
case $TERM in
    xterm*|rxvt*|cygwin)
        function precmd_xterm() {
            print -Pn "\e]0;%n@%m (%y): %5~\a"
        }
        precmd_functions+=( precmd_xterm )
        function preexec_xterm() {
            # print -Pn "\e]0;%n@%m (%y): %5~ [$1]\a"
        }
        preexec_functions+=( preexec_xterm )
        ;;
    screen)
        function precmd_screen() {
            print -Pn "\e]0;%n@%m (%y): %5~\a"
            print -Pn "\ek \e\\"
        }
        precmd_functions+=( precmd_screen )
        function preexec_screen () {
            # print -Pn "\e]0;%n@%m (%y): %5~ [$1]\a"
            print -Pn "\ek${1[(wr)^(*=*|sudo|ssh|-*)]}\e\\"
        }
        preexec_functions+=( preexec_screen )
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

function precmd_j() {
    j --add "$(pwd -P)"
}

precmd_functions+=( precmd_j )


### Set up the EC2 environment.
function ec2setup() {
    # variables
    local ec2_home
    local ec2_versions
    local key

    # autocompletion support
    if [[ "$1" = "-c" || "$1" = "--complete" ]]; then
        for key in ~/.ec2/pk-*.pem(N:t); do
            echo ${key} | sed -e 's/pk-//' | sed -e 's/\.pem//'
        done
        return 0
    fi

    # sanity check
    if [[ -z "$1" ]]; then
        echo "key ID required; they're usually in ~/.ec2"
        return 1
    fi

    # find EC2 installation
    if [[ "${EC2_HOME}" = "" || -z "$(which ec2-describe-instances)" ]]; then
        ec2_versions=( {~,~/sw,/opt}/ec2-api-tools*(/NOn) )
        if [[ -z ${ec2_versions[@]} ]]; then
            echo "EC2 tools not found anywhere"
            return 1
        else
            ec2_home=${ec2_versions[0]}
        fi
        export EC2_HOME="${ec2_home}"
        echo "export EC2_HOME=${EC2_HOME}"
        export PATH="${EC2_HOME}/bin:${PATH}"
    fi

    # which keys?
    local ec2_private_key=$(echo ~/.ec2/pk-$1.pem)
    if [[ ! -f "${ec2_private_key}" ]]; then
        echo "private key $1 not found"
        return 1
    fi
    local ec2_cert=$(echo ~/.ec2/cert-$1.pem)
    if [[ ! -f "${ec2_cert}" ]]; then
        echo "cert $1 not found"
        return 1
    fi
    export EC2_PRIVATE_KEY="${ec2_private_key}"
    echo "export EC2_PRIVATE_KEY=${EC2_PRIVATE_KEY}"
    export EC2_CERT="${ec2_cert}"
    echo "export EC2_CERT=${EC2_CERT}"
}

function _ec2setup() {
    compadd $(ec2setup --complete)
}

compdef _ec2setup ec2setup
