#!/usr/bin/zsh
if [ -z "$DISPLAY" ]; then
    vim "$@"
else
    START_EMACS=1
    # Start up the emacs daemon if needed
    ps aux | grep emacs | grep "daemon" | head -n 1 | while read r; do
          START_EMACS=0
    done
    if [ $START_EMACS -eq 1 ]; then
          emacs --daemon 
    fi

    emacsclient -d"$DISPLAY" -c "$@" &
fi
