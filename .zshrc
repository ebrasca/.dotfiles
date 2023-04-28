[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
export ZSH=$HOME/.oh-my-zsh
export TERM='xterm-256color'
export GPG_TTY=$(tty)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=14'

# Check if .oh-my-zsh exist and if don't exist clone it + plugin
if [ -d "$ZSH" ]; then
else
    git clone https://github.com/ohmyzsh/ohmyzsh $ZSH
    git clone https://github.com/zsh-users/zsh-autosuggestions $ZSH/custom/plugins/zsh-autosuggestions
    git clone https://github.com/chitoku-k/fzf-zsh-completions $ZSH/custom/plugins/fzf-zsh-completions
fi

plugins=(git zsh-autosuggestions fzf-zsh-completions)

source $ZSH/oh-my-zsh.sh

# Enabling Portage completions and Gentoo prompt for zsh
autoload -U compinit promptinit
compinit
promptinit; prompt gentoo

# Enabling cache for the completions for zsh
zstyle ':completion::complete:*' use-cache 1

# Functions

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
web-paste-ix() {
    cat $1 | curl -F 'f:1=<-' ix.io
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

# Aliases

alias reload=". ~/.zshrc"
alias dl='yt-dlp'
alias dlw='yt-dlp -f webm'
alias ndl='yt-dlp -o "%(autonumber)s.%(title)s.%(ext)s"'
alias ndlw='yt-dlp -f webm -o "%(autonumber)s.%(title)s.%(ext)s"'
