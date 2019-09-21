#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  daemonize.pl
#
#        USAGE:  ./daemonize.pl  
#
#  DESCRIPTION:  
#
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  YOUR NAME (), 
#      COMPANY:  
#      VERSION:  1.0
#      CREATED:  07-11-13 09:35:14
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;
use POSIX qw(setsid);



if(!fork()){

    if(!fork()){
        setsid();

        chdir '/';
        umask 0;
        $SIG{TSTP}="IGNORE";
        $SIG{TTIN}="IGNORE";
        $SIG{HUP}="IGNORE";
        close STDOUT;
        close STDIN;
        close STDERR;
        exec @ARGV;
    } else {
        exit()
    }
} else {
    exit()
}
