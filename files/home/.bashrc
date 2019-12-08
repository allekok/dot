# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load aliases
if [ -f "$HOME/.bash_aliases" ]; then
    . "$HOME/.bash_aliases"
fi

PS1='\W \e[93m\$\e[0m '
EDITOR=emacsclient
PATH=$PATH:"$HOME/PROG"

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s histappend
shopt -s globstar
