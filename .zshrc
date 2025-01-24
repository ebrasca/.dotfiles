[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
export ZSH=$HOME/.zsh
export TERM='xterm-256color'
export GPG_TTY=$(tty)
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=14'

#Check if zsh-autosuggestions exist and if don't exist clone it
if [ -d "$ZSH/zsh-autosuggestions" ]; then
else
  git clone https://github.com/zsh-users/zsh-autosuggestions $ZSH/zsh-autosuggestions
fi

source $HOME/.profile
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

## History file configuration
HISTFILE="$HOME/.zhistory"
HISTSIZE=64000
SAVEHIST=32000

## History command configuration
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_verify
setopt share_history

# AUTOCOMPLETION

# initialize autocompletion
autoload -U compinit
compinit

# Enabling cache for the completions for zsh
zstyle ':completion::complete:*' use-cache 1

## USER PROMPT

setopt PROMPT_SUBST
NL=$'\n'
PS1='$NL%B%F{cyan}%3~%f%b$NL%B%(?.%F{green}.%F{red})%(!.#.>)%f%b '

## Functions

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
web-paste() {
    wget --quiet -O- --post-file='$1' 'http://paste.c-net.org/'
}
git-pull-all(){
    find . -type d -mindepth 1 -maxdepth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;
}
lxc-gentoo-run(){
    for container in $(lxc list volatile.last_state.power=RUNNING image.os=Gentoo -c n --format csv);
    do
        echo ">>>> Enter $container"
        lxc exec "$container" -- "$@"
        echo "<<<< Exit $container"
    done
}
run-in-all(){
    echo ">>>> Enter host"
    sudo $@
    echo "<<<< Exit host"
    for container in $(lxc list volatile.last_state.power=RUNNING image.os=Gentoo -c n --format csv);
    do
        echo ">>>> Enter $container"
        lxc exec "$container" -- "$@"
        echo "<<<< Exit $container"
        sleep 0.2
    done
}

## Aliases

alias reload=". ~/.zshrc"
alias dl='yt-dlp'
alias dlw='yt-dlp -f webm'
alias ndl='yt-dlp -o "%(autonumber)s.%(title)s.%(ext)s"'
alias ndlw='yt-dlp -f webm -o "%(autonumber)s.%(title)s.%(ext)s"'
alias trim_all="sudo fstrim -va"
alias ls="ls --color=auto"
alias lsa="ls -lah --color=auto"
alias grep='grep --color=auto'
alias mkdir="mkdir -p"
