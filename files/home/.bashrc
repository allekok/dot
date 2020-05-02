PS1='\W '
EDITOR=emacsclient
PATH=$PATH:"$HOME/PROG"
HISTCONTROL=ignoreboth
HISTSIZE=-1

shopt -s histappend
shopt -s globstar

alias r='sudo protonvpn r'
alias d='sudo protonvpn d'
alias vlcrec='vlc --recursive expand'
alias em='startx 2> /dev/null'
