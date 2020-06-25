#!/usr/bin/zsh
( 
# Basic configuration file
RESOLUTION='1680x1050'
KEYB='us'


trayer --edge bottom --align right --SetDockType true --transparent true --tint 0x000000 --height 22 --SetPartialStrut true --monitor primary --expand true &
diodon &
compton -c &
nm-applet --disable-sm &
megasync &
# feh --randomize --bg-max ~/.config/*.png ~/.config/*.jpg &
# xrandr --output Virtual3 --off --output Virtual2 --off --output Virtual1 --mode ${RESOLUTION} --pos 0x0 --rotate normal --output Virtual7 --off --output Virtual6 --off --output Virtual5 --off --output Virtual4 --off --output Virtual8 --off

# startup linc
# killall pidgin
# daemonize pidgin.sh
#
setxkbmap -layout ${KEYB}

daemonize ~/scripts/carousel.zsh

daemonize flameshot 
)  || true 2>&1 | tee -a /tmp/failure.log  
