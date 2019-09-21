#!/bin/bash

if [[ ! "$1" ]]; then
     echo "Usage: find_package.sh 'perl regexp'"
     exit 0
fi

ack "$1" ~/.package_expressions
