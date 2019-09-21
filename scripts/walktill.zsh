#!/usr/bin/zsh 

zmodload zsh/pcre 

to=$2
from=$1
what=$3
file=$4

function next_diff {
        lines=$(hg diff -r $from -r $to $file)
        if [[ ! -n $lines ]]; then 
                return 
        fi 
        pcre_compile -i "$what"
        if pcre_match -v found $lines; then 
                print "$to and $from"
                print "found: $found"
                print "in $lines"
                exit
        fi 
}

while [[ $to != $from ]]; do 
        next_diff 
        (( from += 1 ))
done 

