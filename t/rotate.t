#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.98;

use File::chdir;
use File::Path qw(remove_tree);
use File::Slurp;
use File::Temp qw(tempdir);
use File::Write::Rotate;

my $dir = tempdir(CLEANUP=>1);
$CWD = $dir;

test_rotate(
    # correctly rename, only keep n histories, doesn't touch other prefixes,
    # handle compress suffix
    name   => "basic rotate",
    args   => [prefix=>"a", histories=>3],
    files_before  => [qw/a a.1 a.2.gz a.3  b b.1 b.2 b.3 b.4/],
    before_rotate => sub {
        write_file("a"  ,    "zero");
        write_file("a.1",    "one");
        write_file("a.2.gz", "two");

        write_file("b.1"   , "untouched");
    },
    files_after   => [qw/a.1 a.2 a.3.gz    b b.1 b.2 b.3 b.4/],
    after_rotate  => sub {
        is(~~read_file("a.1"),    "zero", "a -> a.1");
        is(~~read_file("a.2"),    "one",  "a.2 -> a.2.gz");
        is(~~read_file("a.3.gz"), "two",  "a.2.gz -> a.3.gz");

        is(~~read_file("b.1"), "untouched",  "b.1 untouched");
    },
);

test_rotate(
    name   => "period, suffix",
    args   => [prefix=>"a", suffix=>".log", histories=>2],
    files_before  => [qw/a.2010-01
                         a.2011.log a.2012-10.log a.2012-11.log a.2012-12.log/],
    files_after   => [qw/a.2010-01
                         a.2012-11.log a.2012-12.log/],
);

DONE_TESTING:
done_testing;
if (Test::More->builder->is_passing) {
    $CWD = "/";
} else {
    diag "there are failing tests, not deleting test data dir $dir";
}

sub test_rotate {
    my (%args) = @_;

    subtest $args{name} => sub {

        my $fwr = File::Write::Rotate->new(
            dir => $dir,
            @{$args{args}}
        );

        my $dh;

        # remove all files first
        opendir $dh, ".";
        while (my $e = readdir($dh)) {
            next if $e eq '.' || $e eq '..';
            remove_tree($e);
        }

        write_file($_, "") for @{$args{files_before}};
        $args{before_rotate}->($fwr) if $args{before_rotate};

        $fwr->_rotate;

        my @files;
        opendir $dh, ".";
        while (my $e = readdir($dh)) {
            next if $e eq '.' || $e eq '..';
            push @files, $e;
        }
        @files = sort @files;

        is_deeply(\@files, $args{files_after}, "files_after")
            or diag explain \@files;

        $args{after_rotate}->($fwr) if $args{after_rotate};
    };
}

