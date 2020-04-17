# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load aliases
if [ -f "$HOME/.bash_aliases" ]; then
    . "$HOME/.bash_aliases"
fi

PS1='\W '
EDITOR=emacsclient
PATH=$PATH:"$HOME/PROG"

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s histappend
shopt -s globstar

# Aliases
alias l='ls --color=never -lh'
alias p='pwd'
alias n='nano'
alias r='sudo protonvpn r'
alias d='sudo protonvpn d'
alias vlcrec='vlc --recursive expand'
alias em='startx 2> /dev/null'
alias feh='feh -B "white"'
alias g='git'
