# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\W \e[93m\$\e[0m '
EDITOR=emacsclient
PATH=$PATH:"$HOME/projects/MY-PROGRAMS"

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s histappend
shopt -s globstar
