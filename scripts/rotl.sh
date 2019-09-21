#!/bin/zsh 

program="$1"
shift
arg1="$1"
shift 

"$PWD/$program" "$@" "$arg1"
