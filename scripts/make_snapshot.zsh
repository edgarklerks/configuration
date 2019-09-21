#!/usr/bin/zsh

# Makes a snapshot off all my configurations 
local -a STORABLES 
local -a MAKE_DIRS 
local VERSION_DIR 
local VERSION_NAME 

VERSION_NAME='version'
VERSION_DIR='versions'
STORABLES=(
    '.vimrc'
    '.vim'
    'scripts'
    'vimscripts'
    '.hgrc'
    'hg'
    '.xmonad'
    '.xmobarrc'
    '.xmonad.zsh'
    '.vimperatorrc'
    '.vimperator'
    '.ssh'
    '.zshmacros'
    'sessions'
    '.zshenv'
    '.zshrc'
)
pushd 

local -a FILES

function last_version {
        cd $HOME/versions 
        version=$(
                ls | tail -n 1  | sed -e 's/\([0-9]+*\).*/\1/' 
        )
        ret=$version
        print $version 

}

function tar_name {
    version=$1
    (( version = version + 1 ))
    ret="$HOME/$VERSION_DIR/${version}-${VERSION_NAME}.tar.gz"
    print $ret 
}

cd $HOME 

last_version 
print $version
tar_name $version 
tar=$ret 
cd $HOME 

print $tar 
print $STORABLES 
tar -czf $tar $STORABLES 

popd 
print "done"
