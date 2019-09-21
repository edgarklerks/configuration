#!env zsh

xs="$(xrandr | grep -E "(HDMI|DisplayPort)" | awk '{print $1 " " $2}' | grep -v disconnected | awk '{print $1}')"

d1="$(echo -en "$xs" | head -n1 )"
d2="$(echo -en "$xs" | tail -n1 )"
echo "$d1 $d2"
DISP_TT="$(cat /tmp/_disp)"
if [[ "$DISP_TT" == "RIGHT" ]]; then
    xrandr --output "$d1" --right-of eDP --output eDP --mode 1920x1080 --output "$d2" --left-of eDP
    echo "LEFT" > /tmp/_disp
    echo "$d1, $d2, $DISP_TT"
else
    xrandr --output "$d2" --right-of eDP --output eDP --mode 1920x1080 --output "$d1" --left-of eDP
    echo "RIGHT" > /tmp/_disp
    echo "$d2, $d1, $DISP_TT"

fi
killall trayer
trayer --edge bottom --align right --SetDockType true --transparent true --tint 0x000000 --height 22 --SetPartialStrut true --monitor primary --expand true &
