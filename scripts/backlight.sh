#!/usr/bin/zsh

MAX_LIGHT=$(cat /sys/class/backlight/gmux_backlight/max_brightness)
integer LIGHT=1022;
integer STEP=1;
while true; do
    sudo sh -c "echo $LIGHT > /sys/class/backlight/gmux_backlight/brightness"
    sleep 0.001
    if (( LIGHT == (MAX_LIGHT - 1)  )); then
	(( STEP = -1 ))
    fi

    if (( LIGHT == 0 )); then
	(( STEP = 1 ))
    fi
    (( LIGHT = (LIGHT + STEP)%MAX_LIGHT ))

done
