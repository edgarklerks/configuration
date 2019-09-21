# Simple static web server 

zmodload zsh/net/tcp 

port=$1
dir=$2

ROOTDIR="$dir"

# Header definitions 

typeset -a header_ok 

header_ok[1]="HTTP/1.0 200 OK\r\n"
header_ok[2]="Connection: Close\r\n"
header_ok[3]="Content-Type: text/plain\r\n"

typeset -a header_not_implemented 
header_ni[1]="HTTP/1.0 501 Not implemented\r\n"
header_ni[2]="Connection: Close\r\n"
header_ni[3]="Content-Type: text/plain\r\n"

typeset -a header_nf
header_nf[1]="HTTP/1.0 404 Not found\r\n"
header_nf[2]="Connection: Close\r\n"
header_nf[3]="Content-Type: text/plain\r\n"

function logger {
    print "[$1] -> $2"
}

# Prints out a header definiton
function header {
    for line in $2; do 
            print -n -u $1 $line
    done 
}

# Header functions 
function ok {
    header $1 $header_ok 
}

function not_implemented {
    header $1 $header_ni 
}

function file_not_found {
    header $1 $header_nf 
}

# Helper functions 
function header_end {
    print -n -u $1 "\r\n"
}

function body {
    header_end $1 
    print -n -u $1 $2 

}

function file_length {
    file=$1
    ret=`du -b $file | awk '{print $1}'`
}


# Serve a file 
function serve_file {

    file=$ROOTDIR$2
    if [[ ! -r $file && $2 != '/' ]]; then 
        file_not_found $1 $2
    else 
            case $2 in 
                /)
                        ok $1 
                        body $1 "Accessing index" 
                        ;;
                *)
                        file_length $file
                        length=$ret 
                        unset ret
                        exec 8< $file

                        print "serving file with length: $length"

                        
                        ok $1 
                        header_end $1

                        unset done 

                        until [ $done ]
                        do 
                                read <&8 line
                                if [ $? != 0 ]; then
                                        done=1 
                                        continue 
                                fi 
                                print -n -u $1 $line  

                        done 


                        ;;
            esac
    fi


}
# Parse and read a request into headers
function read_request {
    local -a lines 
    IFS="\r\n"
    
    # Read request while splitting on \r\n
    while read -u $1 -r line; do 
            [[ -n $line && $line != $'\r' ]] || break
            lines=($lines $line)
    done

    typeset -a words 
    
    # Parse request method url version
    methodtype=$lines[1]
    words=(${(ps: :)methodtype})
    method=$words[1]
    url=$words[2]
    version=$words[3]

    # Parse request headers 
    for line in $lines[2,-1]; do
            for key value in ${(s/: /)line}; do 
                    headers[${key}]=$value 
            done 
    done

    headers[url]=$url
    headers[version]=$version
    headers[method]=$method

    unset method
    unset version
    unset method 
    unset words 
    unset methodtype
    unset lines

}

# Startup the server on port and for tmp
function start_server {
    logger "> Start listening on $port"
    ztcp -l $port 

    listenfd=$REPLY
    
    # Nice close down
    TRAPINT() {
        print "Closing down...."
        ztcp -c $listenfd
        ztcp -c $fd
        exit 0
    }

    while :; do 
            typeset -a lines
            ztcp -a $listenfd 
            fd=$REPLY
             
            unset headers; typeset -A headers
            read_request $fd
            
            logger INFO "File descriptor: $fd"

            case $headers[method] in 

                    GET)
                        serve_file $fd $headers[url]
                        ;;
                    *)
                        not_implemented $fd
                        body $fd "$headers[method] is not implemented"
                        ;;
            esac

            ztcp -c $fd

    done
}

start_server
