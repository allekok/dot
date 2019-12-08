export _JAVA_AWT_WM_NONREPARENTING=1
xset r rate 200 60

BG_COLOR_FILE=
if [ $(date +%H) >= 8 ] && [ $(date +%H) < 22 ]; then
    {
	BG_COLOR_FILE="$HOME/.color-light"
    }
else
    {
	BG_COLOR_FILE="$HOME/.color-dark"
    }
fi
BG_COLOR=$(head -1 $BG_COLOR_FILE)

exec emacs --no-site-lisp --no-site-file --no-splash -fs -bg $BG_COLOR
