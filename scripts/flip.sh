#!/bin/zsh 

# Flip first arguments 

program="$1"
shift 
arg1="$1"
shift 
arg2="$1"
shift 
"$PWD/$program" "$arg2" "$arg1" "$@"
