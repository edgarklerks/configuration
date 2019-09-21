#!/usr/bin/zsh

# We represent a list as a stream, where the first element is list.

function unit(){
    echo list "$@"
}

function fmap(){
    local f=$1
    shift
    for var in "$@"; do
	f $var
    done
}
