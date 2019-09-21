# templates to use for parallel execution tool
describe_exec_tool='which {}'
find_path_tool='whereis {}'
list_path_tool='whence -pm "*"'
search_file_tool='find {} -executable '
fast_pattern_tool='grep {}'
flexible_pattern_tool=''
transform_tool='awk {}'
stream_tool='sed {}'
current_shell='sh'
provide_tool='auto-apt {}'
parallel_tool='xargs {}'

# State restoring variables
OLD_IFS="$IFS"
OLD_LANGUAGE="$LANGUAGE"
OLD_LANG="$LANG"
OLD_LC_ALL="$LC_ALL"
OLD_LC_COLLATE="$LC_COLLATE"
OLD_LC_CTYPE="$LC_CTYPE"
OLD_LC_NUMERIC="$LC_NUMERIC"
error(){
    echo "$@" 1>&2
    exit 1;
}

warning(){
    echo "$@" 1>&2
}

find_tool(){
    true
}

init_reupdate(){
    # reverting to default
    # We assume everything to be posix compatible
    unset IFS
    export LANG='C'
    export LANGUAGE='C'
    export LC_ALL='C'
    export LC_COLLATE='C'
    export LC_CTYPE='C'
    export LC_NUMERIC='C'
}

restore_reupdate(){
    # restore user environment
    export IFS="$OLD_IFS"
    export LANG="$OLD_LANG"
    export LANGUAGE="$OLD_LANGUAGE"
    export LC_ALL="$OLD_LC_ALL"
    export LC_COLLATE="$OLD_LC_COLLATE"
    export LC_CTYPE="$OLD_LC_CTYPE"
    export LC_NUMERIC="$OLD_LC_NUMERIC"
}

run_command_expanded(){
    $current_shell -c "eval command $@"
}

silent(){
    $current_shell -c "eval $@" >/dev/null  2>&1
}

test_for_tool(){
   if [  -z "$1" ]; then
       error "need a tool name to test";
   fi
   silent "$1"
}

test_for_tool_sure_pattern(){
    if [  -z "$1" ]; then
	error "need a tool name to test and a pattern to search for";
    fi
    if [ -z "$2" ]; then
	error "Expected a pattern to search for"
    fi
    run_command_expanded "$1" | find_pattern "$2" >/dev/null 2>&1 && return 0
}

test_for_tool_unsure_pattern(){
    if [  -z "$1" ]; then
	error "need a tool name to test, a sure pattern to search for and an unsure pattern to search for";
    fi
    if [  -z "$2" ]; then
	error "Expected a sure pattern to search for"
    fi
    if [ -z "$3"]; then
	error "Expected unsure pattern to search for"
    fi
    run_command_expanded "$1" | find_pattern "$2" | find_pattern_flexible "$3" >/dev/null 1>&2 && return 0
    return 1
}

find_pattern(){
    if [ -z "$1" ]; then
	error "Expected pattern to search for"
    fi
    while read line; do
	 echo "$line" | run_command_expanded "$fast_pattern_tool" $@ && return 0
    done
    return 1
}

# This will fail if there is no flexible tool
find_pattern_flexible(){
    if [ -z "$flexible_pattern_tool"]; then
	return 0
    fi
    while read line; do
    echo "$line" | run_command_expanded "$flexible_pattern_tool" $@ && return 0
    done
    return 1
}

# Command finding tools
test_for_whence(){
    test_for_tool "whence"
}

test_for_ack(){
    test_for_tool_unsure_pattern "ack" "This is version .* ack"
}

test_for_ag(){
    test_for_tool_pattern "ag" "ag version"
}

test_for_hash(){
    test_for_tool "hash"
}

# stream tools

test_for_sed(){
    test_for_tool "sed"
}

test_for_ssed(){
    test_for_tool "ssed"
}

# Basic tools, from which we can build more complex find utilities

test_for_ls(){
    test_for_tool ls
}

test_for_find(){
    test_for_tool find
}

test_for_basename(){
    test_for_tool basename
}

# transformation tools
test_for_tre_agrep(){
    test_for_tool tre-agrep
}
test_for_grep(){
    test_for_tool grep
}

test_for_gawk(){
    test_for_tool_unsure_pattern "awk --version" "'GNU Awk'" "-D2 -2 '\d{1}\.\d{1}\.\d{1}'"
}

test_for_perl(){
    test_for_tool_sure_pattern "perl --version" "'This is perl 5'"
}

# parallel execution tools


test_for_parallel(){
    test_for_tool_sure_pattern "parallel --version" "'GNU parallel '"
}

test_for_xargs(){
    test_for_tool_sure_pattern "xargs --version" "'xargs (GNU findutils)'"
}

# shells
test_for_bash4(){
    test_for_tool_sure_pattern "bash --version" "'bash, version 4\.'"
}

test_for_bash3(){
    test_for_tool_sure_pattern "bash --version" "'bash, version 3\.'"
}

test_for_zsh5(){
    test_for_tool_sure_pattern "zsh --version" "'zsh 5\.'"
}

test_for_zsh4(){
    test_for_tool_sure_pattern "zsh --version" "'zsh 4\.'"
}

list_path(){
    (
    if [ "$list_path_tool" = "custom" ]; then
	if [ current_shell = 'sh' -o current_shell = 'bash' ]; then
	    IFS=:
	    for x in $PATH; do
		echo $x
	    done
	elif [ current_shell = 'zsh' ]; then
	    for x in $(echo $PATH | tr ":" " " ); do
		echo $x
	    done
	fi
    else
	run_command_expanded "$list_path_tool"
    fi
    )

}

get_shell(){
    ps -p $$ | awk '{print $4}'
}

check_tools(){
    # Find a search path utility
    (test_for_zsh5 || test_for_zsh4) && test_for_whence
    if [ test_for_zsh5 -o test_for_zsh4 ]; then
	current_shell='zsh'
	list_path_tool="whence -pm '*'"
	current_shell='zsh'
    elif [ test_for_bash4 -o test_for_bash3 ]; then
	current_shell='bash'
	list_path_tool='ls'
    else
	current_shell='sh'
	list_path_tool='ls'
    fi

    # parallel execution
    if [ test_for_parallel ]; then
	parallel_tool="parallel -I{}"
    elif [ test_for_xargs ]; then
	parallel_tool="xargs -I{}"
    else
	error "No parallel execution tool found"
    fi

    # search path utilities
}

whence -pm '*' | grep '.nix-profile' | parallel basename |  while read line; do
    parallel auto-apt search ::: "^bin/$line\s" "^usr/bin/$line\s" "^usr/local/bin/$line\s" "^sbin/$line\s"
done
