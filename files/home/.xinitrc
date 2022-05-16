#!/bin/sh
export _JAVA_AWT_WM_NONREPARENTING=1
xset r rate 200 60

LIGHT=$(cat /sys/devices/platform/applesmc.768/light)
LIGHT_NUM=$(echo -n ${LIGHT:1:1})
if [ $LIGHT_NUM -gt 2 ]; then
    export COLORNOW="light"
else
    export COLORNOW="dark"
fi
BG_COLOR_FILE="$HOME/.color-$COLORNOW"
BG_COLOR=$(head -1 $BG_COLOR_FILE)
FG_COLOR=$(head -2 $BG_COLOR_FILE | tail -1)

xrdb -merge ~/.Xresources
exec emacs --no-site-lisp --no-site-file --no-splash -bg $BG_COLOR -fg $FG_COLOR -fs
