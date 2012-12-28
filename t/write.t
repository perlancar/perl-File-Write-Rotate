#!perl

use 5.010;
use strict;
use warnings;

use File::chdir;
use File::Path qw(remove_tree);
use File::Slurp;
use File::Temp qw(tempdir);
use File::Write::Rotate;
use Monkey::Patch::Action qw(patch_package);

use Test::Exception;
use Test::More 0.98;

my $dir = tempdir(CLEANUP=>1);
$CWD = $dir;

subtest "basic" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
    $fwr->write("[1]");
    is(~~read_file("a"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a"), "[1][2][3]");
};

subtest "rotate by size" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>3);
    $fwr->write("[1]");
    is(~~read_file("a"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a"), "[2][3]");
    is(~~read_file("a.1"), "[1]");
};

# just testing at some non-negligible size
subtest "rotate size, 20k" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>20*1000);

    my $msg = "x" x 100;
    for (1..200) { $fwr->write($msg) }
    is( (-s "a")  , 20000);
    ok(!(-e "a.1"));

    $fwr->write($msg);
    is( (-s "a")  ,   100);
    is( (-s "a.1"), 20000);
};

subtest "rotate by period, daily" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", period=>"daily");
    $fwr->write("[1]");
    is(~~read_file("a.2012-12-21"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a.2012-12-21"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 86400); # 2012-12-22
    $fwr->write("[4]");
    is(~~read_file("a.2012-12-22"), "[4]");
};

subtest "rotate by period, monthly" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir,
                                       prefix=>"a", period=>"monthly");
    $fwr->write("[1]");
    is(~~read_file("a.2012-12"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a.2012-12"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 31*86400); # 2013-01-21
    $fwr->write("[4]");
    is(~~read_file("a.2013-01"), "[4]");
};

subtest "rotate by period, yearly" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir,
                                       prefix=>"a", period=>"year");
    $fwr->write("[1]");
    is(~~read_file("a.2012"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a.2012"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 31*86400); # 2013-01-21
    $fwr->write("[4]");
    is(~~read_file("a.2013"), "[4]");
};

subtest "rotate by period + size, suffix" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", suffix=>".log",
                                       size=>3, period=>"daily");
    $fwr->write("[1]");
    is(~~read_file("a.2012-12-21.log"), "[1]");
    $fwr->write("[2]", "[3]");
    is(~~read_file("a.2012-12-21.log"), "[2][3]");
    is(~~read_file("a.2012-12-21.log.1"), "[1]");
    $fwr->write("[4]");
    is(~~read_file("a.2012-12-21.log"), "[4]");
    is(~~read_file("a.2012-12-21.log.1"), "[2][3]");
    is(~~read_file("a.2012-12-21.log.2"), "[1]");

    $ph = set_time_to(1356090474 + 86400); # 2012-12-22
    $fwr->write("[5]");
    is(~~read_file("a.2012-12-22.log"), "[5]");
};

subtest "two writers, one rotates" => sub {
    delete_all_files();
    my $fwr1 = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
    my $fwr2 = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>3);
    $fwr1->write("[1.1]");
    is(~~read_file("a"), "[1.1]");
    $fwr2->write("[2.1]");
    is(~~read_file("a"), "[2.1]");
    is(~~read_file("a.1"), "[1.1]");
    $fwr1->write("[1.2]");
    is(~~read_file("a"), "[2.1][1.2]");
    is(~~read_file("a.1"), "[1.1]");
};

# if FWR only rotates after second write(), then there will be cases where the
# file won't get rotated at all.
subtest "rotate on first write()" => sub {
    delete_all_files();
    write_file("$dir/a", "123");
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>3);
    $fwr->write("[1]");
    is(~~read_file("a"), "[1]");
    is(~~read_file("a.1"), "123");
};

subtest "buffer (success)" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", buffer_size=>2);
    $fwr->{_hook_before_print} = sub { die };

    lives_ok { $fwr->write("[1]") } "first message to buffer";
    lives_ok { $fwr->write("[2]") } "second message to buffer";

    undef $fwr->{_hook_before_print};

    $fwr->write("[3]");

    is(~~read_file("a"), "[1][2][3]", "buffered messages gets logged");
    $fwr->write("[4]");
    is(~~read_file("a"), "[1][2][3][4]", "buffered is emptied");
};

subtest "buffer (failed, full), buffer_size attribute" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
    $fwr->buffer_size(2);
    is($fwr->buffer_size, 2, 'buffer_size()');

    local $fwr->{_hook_before_print} = sub { die };

    lives_ok  { $fwr->write("[1]") } "first message to buffer";
    lives_ok  { $fwr->write("[2]") } "second message to buffer";
    throws_ok { $fwr->write("[3]") } qr/\Q[1][2][3]\E/, "buffer is full";
};

DONE_TESTING:
done_testing;
if (Test::More->builder->is_passing) {
    $CWD = "/";
} else {
    diag "there are failing tests, not deleting test data dir $dir";
}

sub delete_all_files {
    # remove all files first
    opendir my($dh), ".";
    while (my $e = readdir($dh)) {
        next if $e eq '.' || $e eq '..';
        remove_tree($e);
    }
}

our $Time;
sub _time() { $Time }

sub set_time_to {
    $Time = shift;
    my $ph = patch_package("File::Write::Rotate", 'time', 'replace', \&_time);
    return $ph;
}
