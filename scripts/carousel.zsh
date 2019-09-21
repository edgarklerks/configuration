#!/bin/zsh
while true; do
        find -L ~/.wallpapers -type f -name '*.jpg' -o -name '*.png'  |
        shuf | head -n1 | xargs -I '{}' -n1 feh --bg-max '{}'
        sleep 5m
done
