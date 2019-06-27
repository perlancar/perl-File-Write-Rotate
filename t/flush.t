#!perl

use 5.010;
use strict;
use warnings;
use Test::Exception;
use Test::More 0.98;
use Test::Warnings qw(:no_end_test warnings);

use File::chdir;
use File::Temp qw(tempdir);
use File::Write::Rotate;

my $dir = tempdir(CLEANUP=>1);
$CWD = $dir;

my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
lives_ok { $fwr->flush };

done_testing;
