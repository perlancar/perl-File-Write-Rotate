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
use Monkey::Patch::Action qw(patch_package);
use IO::Handle;

my $dir = tempdir(CLEANUP=>1);
$CWD = $dir;

subtest "basic" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a"), "[1][2][3]");
};

subtest "size" => sub {
    delete_all_files();
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>3);
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a"), "[2][3]");
    is(~~read_file("a.1"), "[1]");
};

subtest "period daily" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", period=>"daily");
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-21"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-21"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 86400); # 2012-12-22
    $fwr->write("[4]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-22"), "[4]");
};

subtest "period monthly" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir,
                                       prefix=>"a", period=>"monthly");
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 31*86400); # 2013-01-21
    $fwr->write("[4]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2013-01"), "[4]");
};

subtest "period yearly" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir,
                                       prefix=>"a", period=>"year");
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012"), "[1][2][3]");

    $ph = set_time_to(1356090474 + 31*86400); # 2013-01-21
    $fwr->write("[4]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2013"), "[4]");
};

subtest "period + size, suffix" => sub {
    delete_all_files();
    my $ph;
    $ph = set_time_to(1356090474); # 2012-12-21
    my $fwr = File::Write::Rotate->new(dir=>$dir, prefix=>"a", suffix=>".log",
                                       size=>3, period=>"daily");
    $fwr->write("[1]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-21.log"), "[1]");
    $fwr->write("[2]", "[3]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-21.log"), "[2][3]");
    is(~~read_file("a.2012-12-21.log.1"), "[1]");
    $fwr->write("[4]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-21.log"), "[4]");
    is(~~read_file("a.2012-12-21.log.1"), "[2][3]");
    is(~~read_file("a.2012-12-21.log.2"), "[1]");

    $ph = set_time_to(1356090474 + 86400); # 2012-12-22
    $fwr->write("[5]");
    $fwr->{_fh}->flush;
    is(~~read_file("a.2012-12-22.log"), "[5]");
};

subtest "two writers, one rotates" => sub {
    delete_all_files();
    my $fwr1 = File::Write::Rotate->new(dir=>$dir, prefix=>"a");
    my $fwr2 = File::Write::Rotate->new(dir=>$dir, prefix=>"a", size=>3);
    $fwr1->write("[1.1]");
    $fwr1->{_fh}->flush;
    is(~~read_file("a"), "[1.1]");
    $fwr2->write("[2.1]");
    $fwr2->{_fh}->flush;
    is(~~read_file("a"), "[1.1][2.1]");
    $fwr2->write("[2.2]");
    $fwr2->{_fh}->flush;
    is(~~read_file("a"), "[2.2]");
    is(~~read_file("a.1"), "[1.1][2.1]");
    $fwr1->write("[1.2]");
    $fwr1->{_fh}->flush;
    is(~~read_file("a"), "[2.2][1.2]");
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
