#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  rip_solr_doc.pl
#
#        USAGE:  ./rip_solr_doc.pl  
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
#      CREATED:  11-11-13 12:42:53
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;

use XML::Parser;
use Data::Dumper;

our $table = {};


my $pl = XML::Parser->new(
    Handlers => {
        Start => \&handle_start,
        End => \&handle_end 
    }
);

# $pl->parsefile($ARGV[0]);
# output_tex_table();

group_max_n({
        a => undef, 
        ab => undef, 
        abc => undef,
        b => undef, 
        ba => undef, 
        bac => undef,
        c => undef, 
        d => undef, 
        da => undef, 
        dab => undef,
        dbac => undef, 
        e => undef,
        egf => undef 
    }, 5);

exit();

sub group_max_n {
    sub is_last {
        my ($keys, $n) = @_;
        return 1, if @{$keys} < $n;
        
        return 1, if(@{$keys} > $n + 1 && $keys->[$n] != $keys->[$n + 1]);
        return 0;

        
    }
    my $table = shift; 
    my $n = shift; 
    my @keys = sort keys $table;    
    my $chars = [];
    my $last = $keys[0];
    for(my $i = 0; $i < @keys; $i++){
        print is_last(\@keys, $i);
        push @$chars, $keys[$i]
    
    }



}

sub output_tex_table {
    print "field_name & type & required & description \\\\\n";
    print "\\hline\n";
    for my $key (keys %$table){
        my $type = $table->{$key}{type};
        my $required = $table->{$key}{required};
        print "$key & $type & $required  & TODO \\\\\n";
    }
}

sub handle_start {
    my ($exp, $el, %attrs) = @_;
    if($el ne 'field'){
        return 
    }
    for my$key(keys %attrs){
        if($attrs{'name'}){
            $table->{$attrs{'name'}} = {
                required => $attrs{required} && $attrs{required} eq 'true' ? 'required' : 'optional',
                type => $attrs{type}


            }
        }
        }
    }


sub handle_end {

}






