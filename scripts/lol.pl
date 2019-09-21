#!/bin/perl -w
use warnings;
use strict;
use English;
use 5.18.2;

use Data::Dump::Streamer;
use Shell::Parser;
use Switch;

my $file = $ARGV[0];
my $parser = new Shell::Parser syntax => 'bash',
    handlers => {default => sub {
	my ($self, %args) = @ARG;
	switch($args{type}){
	    case "assign" {

		print $args{type} . " " . $args{token} . "\n";

	    }


	}
		print $args{type} . " " . $args{token} . "\n";

	}

};


open my $fh,  "<$file";
my @contents = <$fh>;
my $content = join '', @contents;

$parser->parse($content);

close $fh;
