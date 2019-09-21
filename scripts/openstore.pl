#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  openstore.pl
#
#        USAGE:  ./openstore.pl  
#
#  DESCRIPTION: Store and retrieve secret keys  from one password database
#
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  YOUR NAME (), 
#      COMPANY:  
#      VERSION:  1.0
#      CREATED:  15-08-17 14:45:35
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;
use Modern::Perl;
use Crypt::KeyDerivation qw/pbkdf2/;
use Crypt::Mode::CBC;
use Crypt::PRNG::Fortuna qw/random_bytes/;
use BerkeleyDB;
use Term::ReadKey;
use Data::Dumper;

sub get_value { 
    my $prompt = shift; 
    print STDERR $prompt ;
    ReadMode('noecho');
    chomp(my $password = <STDIN>);
    ReadMode(0);
    print STDERR "\n";
    return $password;
}

my $HOME = $ENV{"HOME"};

mkdir "$HOME/.keystore";

my $operation = $ARGV[0];
my $key = $ARGV[1];


my $env = new BerkeleyDB::Env(
    -Home => "$HOME/.keystore",
    -ErrFile => "$HOME/.keystore/error.log",
    -MsgFile => "$HOME/.keystore/info.log",
    -Flags => DB_CREATE | DB_INIT_CDB | DB_INIT_MPOOL
) or die "Cannot open environment: $BerkeleyDB::Error";

my $password = "bunk";
if($operation ne "list"){ 
    $password = get_value("password: ");
}

my $store_key = pbkdf2($password, "LKJASD90jdj390jh02   jdopjd90wqefj0jqv90qwefjqwefqwejqwervAWDKI2310-912=2121``-`9-=9W=-0 21I 0CKL;MC,./<>?csm<>CNJ0[2F9231", 50000);




die "need operation", unless defined $operation;

my $db = tie my %passwords, 'BerkeleyDB::Hash', (-Filename => "$HOME/.keystore/passwd.db", -Env => $env, -Flags => DB_CREATE) or die "Cannot tie to $!";
my $m = Crypt::Mode::CBC->new('AES');



if($operation eq "store"){ 
        die "need key", unless defined $key;

        my $key_id = "key_$key";
        my $iv_id = "iv_$key";
        my $value = get_value("value: ");
        my $iv = random_bytes(16);
        my $cipher_value = $m->encrypt($value,$store_key, $iv);
        $passwords{$key_id} = $cipher_value;
        $passwords{$iv_id} = $iv; 
} elsif($operation eq "get"){ 
        die "need key", unless defined $key;

        my $key_id = "key_$key";
        my $iv_id = "iv_$key";
        unless (exists $passwords{$key_id}){ 
            die "Key doesn't exist";
        }
        my $cipher_value = $passwords{$key_id};
        my $iv = $passwords{$iv_id};
        my $value = $m->decrypt($cipher_value, $store_key, $iv);
        print STDOUT $value;
} elsif($operation eq "list"){
    my @keys = grep /^key_/, keys %passwords;
    for my $key(@keys){ 
        $key =~ s/^key_(.*)/$1/;
        print "$key\n";
    }
}

