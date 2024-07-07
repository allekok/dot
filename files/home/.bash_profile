export PATH="$HOME/PROG:$HOME/go/bin:$HOME/v2rayNG-master/android-ndk-r26b:$PATH"
export EDITOR=emacsclient
export HISTCONTROL=ignoreboth
export HISTSIZE=-1

alias em='startx 2> /dev/null'
alias e='emacsclient'

shopt -s histappend
shopt -s globstar

. "$HOME/.cargo/env"

# Run X server and emacs
em
