PS1='\033[7;32m \w \033[0m '
EDITOR=emacsclient
HISTCONTROL=ignoreboth
HISTSIZE=-1

shopt -s histappend
shopt -s globstar

alias vlcrec='vlc --recursive expand'
alias em='startx 2> /dev/null'
alias e='emacsclient'
