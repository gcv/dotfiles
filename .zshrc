autoload -Uz compinit                                      # prepare completion
autoload -Uz edit-command-line && zle -N edit-command-line # bound to M-e below
autoload -Uz colors zsh/terminfo                           # symbolic colors
autoload -Uz is-at-least                                   # zsh version checks
autoload -Uz vcs_info                                      # VCS status
bindkey -e                                                 # Emacs keybindings
ulimit -c 0                                                # no core dumps
ulimit -s unlimited                                        # no stack limits


### Paths include locally-installed packages under ~/.local as symbolic links to
### the active version:
###   ./configure --prefix=~/.local/package-version && make && make install
###   ln -s ~/.local/package-version ~/.local/package
typeset -U path
function path_reset() {
    path=(
        ~/.local/bin ~/.local/sbin
        ~/.local/*(@Ne:'[[ -d ${REPLY}/bin ]] && REPLY=${REPLY}/bin':)
        ~/.local/*(@Ne:'[[ -d ${REPLY}/sbin ]] && REPLY=${REPLY}/sbin':)
        /opt/brew/bin /opt/brew/sbin
        /opt/local/bin /opt/local/sbin
        /usr/local/bin /usr/local/sbin
        /usr/bin /usr/sbin
        /bin /sbin
        $path
    )
}
path_reset

typeset -U manpath
manpath+=(
    ~/.local/share/man
    ~/.local/*(@Ne:'[[ -d ${REPLY}/man ]] && REPLY=${REPLY}/man':)
    ~/.local/*(@Ne:'[[ -d ${REPLY}/share/man ]] && REPLY=${REPLY}/share/man':)
    /opt/brew/share/man
    /opt/local/man
)
export MANPATH=${MANPATH}:                                 # append colon

typeset -U fpath
fpath+=(/opt/brew/share/zsh/site-functions)


### Switch to a more recent zsh if found in path. Be sure not to
### initialize completion system until after this happens.
if [[ -x $(whence zsh) ]]; then
    is-at-least $(zsh --version | awk '{print $2}') || exec zsh
fi


### environmental variables
export EDITOR=vi
export LESS="-M -R"
export PAGER=less
export LESSHISTFILE=/dev/null
export CVS_RSH=ssh
export LC_CTYPE=en_US.UTF-8
export LC_COLLATE=C
export UNAME=`uname`
export WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"


### aliases
alias rm='rm -i'
alias dir='ls -aCF'
alias v='ls -lahF'
alias d='dirs -v'
alias pf='open -a "Path Finder.app"'

[[ ${UNAME} == "Darwin" && -x "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" ]] && \
    alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n' || \
    alias ec='emacsclient -n'

[[ ${UNAME} == "Linux" ]] && alias ls='ls --color'
[[ ${UNAME} == "Darwin" && -x "/opt/brew/bin/gls" ]] && alias ls='gls --color'

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
setopt hist_ignore_space          # do not save command entries which start with a space
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
setopt auto_menu                  # turn on menu-completion after two tabs
setopt short_loops                # allow one-line 'for' and 'repeat' loops
setopt rc_expand_param            # repeated array expansions
setopt rc_quotes                  # '' means a single quote in single quotes


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
bindkey '^[W' copy-region-as-kill
bindkey ' ' magic-space
bindkey '\ee' edit-command-line
bindkey '\C-xu' universal-argument
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word


### command completion
compinit -i                                                # no insecure files
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
zstyle ':completion:*:default' menu 'select=0'
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
PROMPT="%(!.${COLOR_RED}.${COLOR_NONE})[%m:%4~"'${vcs_info_msg_0_}${COLOR_MAGENTA}${VIRTUAL_ENV:+ }`basename ${VIRTUAL_ENV:-""}`${COLOR_NONE}'"]${COLOR_NONE} "
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


### Turn on autojump (https://github.com/joelthelion/autojump).
[[ ${UNAME} == "Darwin" && -f `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh
[[ ${UNAME} == "Linux" && -f /usr/share/autojump/autojump.sh ]] && . /usr/share/autojump/autojump.sh


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
        ec2_versions=( {~,~/.local,/opt}/ec2-api-tools*(/NOn) )
        if [[ -z ${ec2_versions[@]} ]]; then
            echo "EC2 tools not found anywhere"
            return 1
        else
            ec2_home=${ec2_versions[1]}
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


### Turn on RVM.
function rvm_on() {
    [[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
    # autocompletion
    fpath=($fpath $HOME/.rvm/scripts/zsh/Completion)
    compinit -i
    function rvm_prompt_update() {
        local rvm_prompt=$(~/.rvm/bin/rvm-prompt i v p g)
        if [ -z $rvm_prompt ]; then
            PROMPT="%(!.${COLOR_RED}.${COLOR_NONE})[%m:%4~"'${vcs_info_msg_0_}'"]${COLOR_NONE} "
        else
            rvm_info_msg=" rvm:${COLOR_CYAN}${rvm_prompt}${COLOR_NONE}"
            PROMPT="┌ ${COLOR_MAGENTA}%8~${COLOR_NONE}"'${vcs_info_msg_0_}${rvm_info_msg}'"
└ %(!.${COLOR_RED}.${COLOR_NONE})${COLOR_MAGENTA}%m${COLOR_NONE} ∴ ${COLOR_NONE}"
        fi
    }
    precmd_functions+=(rvm_prompt_update)
}


### Turn on Python virtualenv.
function pve() {
    local pd=~/.python
    local env_name=$1
    local system_python=/usr/bin/python2
    if [[ -z ${env_name} ]]; then
        if [[ -z ${VIRTUAL_ENV} ]]; then
            echo "usage: pve <envname>"
            return 1
        else
            deactivate
            return 0
        fi
    fi
    if [[ ! -e ${pd} ]]; then
        echo "${pd} not found"
        return 1
    fi
    pushd ${pd}
    if [[ ! -f virtualenv.py ]]; then
        curl -L -O https://raw.github.com/pypa/virtualenv/master/virtualenv.py
    fi
    if [[ ! -d ${env_name} ]]; then
        echo "installing virtualenv"
        ${system_python} virtualenv.py ${env_name} --no-setuptools
        if [[ ! -f get-pip.py ]]; then
            curl -L -O https://raw.github.com/pypa/pip/master/contrib/get-pip.py
        fi
        ${env_name}/bin/python ./get-pip.py
    fi
    echo "switching to existing Python environment ${env_name}"
    VIRTUAL_ENV_DISABLE_PROMPT=true
    source "${env_name}/bin/activate"
    popd
}


### GPG directory encryption.
function gpgd() {

    # Enables mounting GPG-encrypted tar files as directories. Upon
    # dismount, updates the encrypted tar file.
    #
    # Usage:
    #  gpgd init my-new-archive.gpg recipient1 recipient2 recipient3
    #  gpgd mount my-new-archive.gpg "~/Secure Files"
    #    ... edit files in "~/Secure Files/my-new-archive"
    #  gpgd umount "~/Secure Files/my-new-archive"
    #    ... after the unmount, my-new-archive.gpg will be updated
    #    and "~/Secure Files/my-new-archive" securely deleted.
    #
    # Features:
    #  - securely removes traces of unencrypted files after unmounting
    #
    # Limitations:
    #  - currently requires the basename of the archive file to match
    #    the name of the mount point (e.g., my-archive.gpg needs to be
    #    mounted to a directory called my-archive); renaming the archive
    #    will break it; moving the archive when it's mounted will also
    #    break it (not irrevocably, it'll just require editing the
    #    internal .gpgd-archive-file while mounted)
    #  - public-key encryption only; mainly to avoid the hassle of
    #    typing a passphrase on unmount when using GPG in symmetric mode
    #  - written in shell script for convenience of installation, but
    #    the code does suffer for it
    #  - zsh only
    #  - greadlink dependency on Mac OS
    #  - not ideal for larger encrypted directories: sync of encrypted
    #    archives will not allow any block optimizations, secure rm
    #    is fairly slow

    # environment check
    if [[ -z $(command -v gpg) ]]; then
        echo "no gpg found" 1>&2
        return 1
    fi
    if [[ -z $(command -v tar) ]]; then
        echo "no tar found" 1>&2
        return 1
    fi
    if [[ -z $(command -v srm) ]]; then
        echo "no srm found" 1>&2
        return 1
    fi
    if [[ -z $(command -v mktemp) ]]; then
        echo "no mktemp found" 1>&2
        return 1
    fi
    local readlink
    case $(uname) in
        "Darwin")
            if [[ -z $(command -v greadlink) ]]; then
                echo "GNU readlink required; install coreutils from Homebrew" 1>&2
                return 1
            else
                readlink="greadlink"
            fi
        ;;
        "Linux")
            readlink="readlink"
        ;;
    esac

    function gpgd_help() {
        echo "usage:"
        echo "  gpgd init <archive-file> recipient+"
        echo "  gpgd mount <archive-file> <mount-point-parent-directory>"
        echo "  gpgd umount <mount-point>"
        return 0
    }

    function gpgd_init() {
        local archive_file=$2
        local -a recipients
        local recipient
        for recipient in ${argv[3,-1]}; do
            recipients+=${recipient}
        done
        if [[ -z "${archive_file}" ]]; then
            echo "archive required" 1>&2
            return 1
        fi
        if [[ -z ${recipients[1]} ]]; then
            echo "recipients required" 1>&2
            return 1
        fi
        local recipients_gpg_arg=""
        for recipient in ${recipients}; do
            if gpg --list-key ${recipient} &> /dev/null; then
                if [[ -z "${recipients_gpg_arg}" ]]; then
                    recipients_gpg_arg="-r ${recipient}"
                else
                    recipients_gpg_arg="${recipients_gpg_arg} -r ${recipient}"
                fi
            else
                echo "no public key found for ${recipient}, aborting" 1>&2
                return 1
            fi
        done
        local archive_file_dir="$(dirname ${archive_file})"
        local archive_filename="$(basename ${archive_file})"
        local archive_filename_noext="${archive_filename%.*}"
        if [[ "${archive_filename_noext}" == "${archive_filename}" ]]; then
            echo "an extension to ${archive_filename} is required, consider using .gpg"
            return 1
        fi
        if [[ -e "${archive_filename_noext}" ]]; then
            echo "current directory already has ${archive_filename_noext} directory, aborting" 1>&2
            return 1
        fi
        if [[ -e "${archive_file}" ]]; then
            echo "current directory already has ${archive_file}, aborting" 1>&2
            return 1
        fi
        mkdir "${archive_filename_noext}"
        echo ${recipients} > "${archive_filename_noext}/.gpgd-recipients"
        # XXX: Write to a temporary archive_file in current directory first.
        # --output does not support file names with spaces.
        local tmpfile=$(mktemp XXXXX)
        rm -f "${tmpfile}"
        tar cz "./${archive_filename_noext}" | gpg --encrypt "${=recipients_gpg_arg}" --output "${tmpfile}"
        mv "${tmpfile}" "${archive_file}"
        srm -rf -s "${archive_filename_noext}"
    }

    function gpgd_check_shared_mount_points() {
        local check=$1
        while : ; do
            if [[ -e "${check}/.dropbox.cache" ]]; then
                echo "${check} seems to be managed by Dropbox" 1>&2
                exit 1
            fi
            if [[ -e "${check}/.SyncID" ]]; then
                echo "${check} seems to be managed by BitTorrent Sync" 1>&2
                exit 1
            fi
            [[ "/" == "${check}" ]] && break
            check=$(dirname "${check}")
        done
    }

    function gpgd_mount() {
        local archive_file="$(${readlink} -f "${2}")"
        if [[ ! -f "${archive_file}" ]]; then
            echo "archive required" 1>&2
            return 1
        fi
        local mount_point_parent_directory="$(${readlink} -f "${3}")"
        if [[ ! -w "${mount_point_parent_directory}" ]]; then
            echo "writeable mount point parent directory required" 1>&2
            return 1
        fi
        # NB: This will not help detect the case of a symlink from
        # a shared directory into a directory into the secure path.
        gpgd_check_shared_mount_points "${mount_point_parent_directory}"
        gpgd_check_shared_mount_points "$3"
        local archive_file_dir="$(dirname ${archive_file})"
        local archive_filename="$(basename ${archive_file})"
        local archive_filename_noext="${archive_filename%.*}"
        if [[ -e "${mount_point_parent_directory}/${archive_filename_noext}" ]]; then
            echo "${archive_filename_noext} already exists under ${mount_point_parent_directory}; aborting"
            return 1
        fi
        pushd "${mount_point_parent_directory}"
        # XXX: Symlink the archive_file into a temporary file in the current directory first:
        # --decrypt does not support file names with spaces.
        local tmpfile=$(mktemp XXXXX)
        rm -f "${tmpfile}"
        ln -s "${archive_file}" "${tmpfile}"
        gpg -q --decrypt "${tmpfile}" | tar xk -
        rm -f "${tmpfile}"
        if [[ ! -d "${archive_filename_noext}" ]]; then
            # TODO: Deal with the problem of archive filenames differing from the root of the archive.
            # This can occur if someone renames either the mount point or the archive filename.
            echo "SERIOUS ERROR: archive name ${archive_filename_noext} differs from mount point directory name" 1>&2
            return 1
        fi
        pushd "${archive_filename_noext}"
        echo "${archive_file}" > .gpgd-archive-file
        popd
        popd
    }

    function gpgd_check_a_not_inside_b() {
        local a="$1"
        local b="$2"
        # reduce a until it's equal to b
        local check=$a
        while : ; do
            if [[ "$a" == "$b" ]]; then
                echo "you seem to be inside the mount point area; aborting" 1>&2
                exit 1
            fi
            [[ "/" == "${check}" ]] && break
            check=$(dirname "${check}")
        done
    }

    function gpgd_umount() {
        local mount_point="$(${readlink} -f "$2")"
        if [[ ! -d "${mount_point}" ]]; then
            echo "${mount_point} does not exist" 1>&2
            return 1
        fi
        if [[ ! -e "${mount_point}/.gpgd-archive-file" ]]; then
            echo "no .gpgd-archive-file found in ${mount_point}" 1>&2
            return 1
        fi
        if [[ ! -e "${mount_point}/.gpgd-recipients" ]]; then
            echo "no .gpgd-recipients found in ${mount_point}" 1>&2
            return 1
        fi
        local mount_point_parent_directory="$(dirname "${mount_point}")"
        local mount_point_basename="$(basename "${mount_point}")"
        local current_dir_raw="$(pwd)"
        local current_dir="$(${readlink} -f "${current_dir_raw}")"
        local archive_path_raw="$(cat "${mount_point}/.gpgd-archive-file")"
        local archive_path="$(${readlink} -f "${archive_path_raw}")"
        local archive_filename="$(basename "${archive_path}")"
        local archive_filename_noext="${archive_filename%.*}"
        if [[ ! -e "${archive_path}" ]]; then
            echo "no encrypted archive found at ${archive_path}" 1>&2
            return 1
        fi
        if [[ "${mount_point_basename}" != "${archive_filename_noext}" ]]; then
            echo "mount point ${mount_point_basename} does not match archive filename ${archive_filename_noext}"
            return 1
        fi
        gpgd_check_a_not_inside_b "${current_dir}" "${mount_point}"
        local recipients_raw="$(cat "${mount_point}/.gpgd-recipients")"
        local -a recipients
        recipients=("${(s/ /)recipients_raw}")
        # verify recipients, identical to gpgd_init (shell limitations)
        local recipients_gpg_arg=""
        for recipient in ${recipients}; do
            if gpg --list-key ${recipient} &> /dev/null; then
                if [[ -z "${recipients_gpg_arg}" ]]; then
                    recipients_gpg_arg="-r ${recipient}"
                else
                    recipients_gpg_arg="${recipients_gpg_arg} -r ${recipient}"
                fi
            else
                echo "no public key found for ${recipient}, aborting" 1>&2
                return 1
            fi
        done
        pushd "${mount_point_parent_directory}"
        local tmpfile=$(mktemp XXXXX)
        rm -f "${tmpfile}"
        tar cz --exclude .gpgd-archive-file "./${mount_point_basename}" | gpg --encrypt "${=recipients_gpg_arg}" --output "${tmpfile}"
        mv "${tmpfile}" "${archive_path}"
        srm -rf -s "${mount_point}"
        popd
    }

    # command dispatch
    local cmd=$1
    if [[ -z ${cmd} ]]; then
        cmd="help"
    fi
    gpgd_${cmd} $* || return 1

}


######### yesterday we obeyed kings and bent our necks before emperors #########
#########               but today we kneel only to truth               #########
