#!/bin/bash

nix-channel --update

nix-env -qaP \* > ~/.package_list

cat ~/.package_list | awk '{print $1}' > ~/.package_expressions
