#!/bin/sh
export _JAVA_AWT_WM_NONREPARENTING=1

export GTK_THEME=Adwaita
export COLORNOW="light"
BG_COLOR_FILE="$HOME/.color-$COLORNOW"
BG_COLOR=$(head -1 $BG_COLOR_FILE)

xset r rate 200 60
xrdb -merge ~/.Xresources

exec emacs --no-site-lisp --no-site-file --no-splash -bg $BG_COLOR -fs
