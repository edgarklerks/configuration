#!/usr/bin/perl

use MIME::Base64;
use File::MimeInfo::Magic;
use strict;
use warnings;
use English;

sub createDataUri {
    my ($fn)=@_;
    open my $fh, "<$fn";
    my $mt = mimetype($fn);
    my @cnt = <$fh>;
    my $enc = encode_base64((join "", @cnt),"");
    return sprintf("data:%s;base64,%s", $mt, $enc);

}

print (createDataUri $ARGV[0]);
