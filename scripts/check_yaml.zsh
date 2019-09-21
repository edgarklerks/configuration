#!/usr/bin/zsh 
#
#
#
perl -MYAML -e "use YAML;YAML::LoadFile(${(qq)@})"
