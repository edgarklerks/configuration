#!/bin/zsh 

program="$1"
shift 
arg1="$(eval \$$#)"
shift

echo "$program $arg $@"
