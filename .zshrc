# ======================
# Environment Variables
# ======================

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
export ZSH=$HOME/.zsh
export TERM='xterm-256color'
export GPG_TTY=$(tty)
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=14'

# ======================
# Plugin Configuration
# ======================

# Clone zsh-autosuggestions if not already installed
if [ ! -d "$ZSH/zsh-autosuggestions" ]; then
  git clone https://github.com/zsh-users/zsh-autosuggestions $ZSH/zsh-autosuggestions
fi

# Source plugins and configurations
source $HOME/.profile
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# ======================
# Keybindings
# ======================

# Emacs-style keybindings
bindkey -e

# ======================
# History Configuration
# ======================

HISTFILE="$HOME/.zhistory"
HISTSIZE=64000
SAVEHIST=32000

setopt hist_ignore_dups     # Ignore duplicate commands
setopt hist_ignore_space    # Ignore commands starting with a space
setopt hist_save_no_dups    # Save only unique commands in history
setopt hist_verify          # Verify history expansion before executing
setopt share_history        # Share history across sessions

# ======================
# Autocompletion
# ======================

# Initialize and configure autocompletion
autoload -U compinit
compinit

# Enable caching for faster completions
zstyle ':completion::complete:*' use-cache 1

# ======================
# Prompt Customization
# ======================

setopt PROMPT_SUBST
NL=$'\n'
PS1='$NL%B%F{cyan}%3~%f%b$NL%B%(?.%F{green}.%F{red})%(!.#.>)%f%b '

# ======================
# Functions
# ======================

# Print terminal escape sequences (compatible with tmux and screen)
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Upload a file to paste.c-net.org
web-paste() {
    wget --quiet -O- --post-file="$1" 'http://paste.c-net.org/'
}

# Pull latest changes for all Git repositories in the current directory
git-pull-all() {
    find . -type d -mindepth 1 -maxdepth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;
}

# Run a command in all running Gentoo LXC containers
lxc-gentoo-run() {
    for container in $(lxc list volatile.last_state.power=RUNNING image.os=Gentoo -c n --format csv); do
        echo ">>>> Enter $container"
        lxc exec "$container" -- "$@"
        echo "<<<< Exit $container"
    done
}

# Run a command on the host and all running Gentoo LXC containers
run-in-all() {
    echo ">>>> Enter host"
    sudo $@
    echo "<<<< Exit host"
    for container in $(lxc list volatile.last_state.power=RUNNING image.os=Gentoo -c n --format csv); do
        echo ">>>> Enter $container"
        lxc exec "$container" -- "$@"
        echo "<<<< Exit $container"
        sleep 0.2
    done
}

# ======================
# Aliases
# ======================

# Reload .zshrc
alias reload=". ~/.zshrc"

# yt-dlp shortcuts
alias dl='yt-dlp'
alias dlw='yt-dlp -f webm'
alias ndl='yt-dlp -o "%(autonumber)s.%(title)s.%(ext)s"'
alias ndlw='yt-dlp -f webm -o "%(autonumber)s.%(title)s.%(ext)s"'

# System utilities
alias trim_all="sudo fstrim -va"
alias ls="ls --color=auto"
alias lsa="ls -lah --color=auto"
alias grep='grep --color=auto'
alias mkdir="mkdir -p"
