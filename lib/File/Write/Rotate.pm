package File::Write::Rotate;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Fcntl ':flock';
use Time::HiRes 'time';

# VERSION

sub new {
    my $class = shift;
    my %args = @_;

    defined($args{dir})    or die "Please specify dir";
    defined($args{prefix}) or die "Please specify prefix";
    $args{suffix} //= "";

    $args{size} //= 0;

    if ($args{period}) {
        $args{period} =~ /daily|day|month|year/i
            or die "Invalid period, please use daily/monthly/yearly";
    }

    if (!$args{period} && !$args{size}) {
        $args{size} = 10*1024*1024;
    }

    $args{histories} //= 10;

    bless \%args, $class;
}

# file path, without the rotate suffix
sub file_path {
    my ($self) = @_;

    # _now is calculated every time we access this method
    $self->{_now} = time();

    my @lt = localtime($self->{_now});
    $lt[5] += 1900;
    $lt[4]++;

    my $period;
    if ($self->{period}) {
        if ($self->{period} =~ /year/i) {
            $period = sprintf(".%04d", $lt[5]);
        } elsif ($self->{period} =~ /month/i) {
            $period = sprintf(".%04d-%02d", $lt[5], $lt[4]);
        } elsif ($self->{period} =~ /day|daily/i) {
            $period = sprintf(".%04d-%02d-%02d", $lt[5], $lt[4], $lt[3]);
        }
    } else {
        $period = "";
    }

    join(
        '',
        $self->{dir}, '/',
        $self->{prefix},
        $period,
        $self->{suffix},
    );
}

sub lock_file_path {
    my ($self) = @_;
    join(
        '',
        $self->{dir}, '/',
        $self->{prefix},
        '.lck'
    );
}

# locking is used to prevent multiple writer processes from clobbering one
# another's write. here we (re)open the lock file (as well as creating it if it
# does not already exist), flock it with LOCK_NB, trying several times up to a
# minute if fails to get a lock, then die if still fails. i think this is the
# best compromise compared to: 1) flock without LOCK_NB (blocks indefinitely,
# potentially causing multiple processes to pile up); 2) buffering writes (only
# delaying the disaster, buffered data will be lost anyway unless written to
# another file); 3) ignoring lock (clobbering).

# note: reopening also solves problem with shared lock between parent and forked
# children (this is a note from Log::Dispatch::FileRotate).

# note: ideally, lock should not be held for more than a fraction of a second.
# that's why we lock after each single print and immediately unlock it again. we
# also only lock when creating an empty compressed old file, we do not hold lock
# while compressing.

# the _lock and _unlock routines might be refactored into a module someday. i
# like this better than File::Flock (which is rather heavy) and
# File::Flock::Tiny (which does not have unlink-if-created-by-us feature).

# return 1 if we lock, 0 if already locked, dies on error/failure to get lock
sub _lock {
    my ($self) = @_;

    # already locked
    return 0 if $self->{_lfh};

    my $lfp = $self->lock_file_path;
    my $exists = (-f $lfp);
    open $self->{_lfh}, ">>", $lfp or die "Can't open lock file '$lfp': $!";
    my $tries = 0;
    while (1) {
        $tries++;
        last if flock($self->{_lfh}, LOCK_EX | LOCK_NB);
        $tries > 60 and die "Can't acquire lock on '$lfp' after 1 minute";
        sleep 1;
    }
    $self->{_created_lock_file} = !$exists;
    1;
}

# return 1 if we unlock, 0 if already unlocked
sub _unlock {
    my ($self) = @_;

    my $lfp = $self->lock_file_path;

    return 0 unless $self->{_lfh};

    # delete first to avoid race condition (i.e. we delete lock file created by
    # other process)
    unlink $lfp if delete($self->{_created_lock_file});

    flock $self->{_lfh}, LOCK_UN;
    close delete($self->{_lfh});
    1;
}

# rename (increase rotation suffix) and keep only n histories. note: failure in
# rotating should not be fatal, we just warn and return.
sub _rotate {
    my ($self) = @_;

    my $locked = $self->_lock;
    # each entry is [filename without compress suffix, rotate_suffix (for
    # sorting), period (for sorting), compress suffix (for renaming back)]
    my @files;

    opendir my($dh), $self->{dir} or do {
        warn "Can't opendir '$self->{dir}': $!, rotate skipped";
        return;
    };
    while (my $e = readdir($dh)) {
        my $compress_suffix = $1 if $e =~ s/(\.gz)\z//;

        next unless $e =~ /\A\Q$self->{prefix}\E
                           (?:\. (?<period>\d{4}(?:-\d\d(?:-\d\d)?)?) )?
                           \Q$self->{suffix}\E
                           (?:\. (?<rotate_suffix>\d+) )?
                           \z
                          /x;
        push @files, [$e, $+{rotate_suffix} // 0, $+{period} // "",
                      $compress_suffix // ""];
    }
    closedir($dh);

    my $i;
    my $dir = $self->{dir};
    for my $f (sort {$b->[1] <=> $a->[1] || $a->[2] cmp $b->[2]} @files) {
        my ($orig, $rsuffix, $period, $cs) = @$f;
        $i++;
        if ($i <= @files-$self->{histories}) {
            $log->trace("Deleting old rotated file $dir/$orig$cs ...");
            unlink "$dir/$orig$cs" or warn "Can't delete $dir/$orig$cs: $!";
            next;
        }
        my $new = $orig;
        if ($rsuffix) {
            $new =~ s/\.(\d+)\z/"." . ($1+1)/e;
        } elsif (!$period || delete($self->{_tmp_hack_give_suffix_to_fp})) {
            $new .= ".1";
        }
        if ($new ne $orig) {
            $log->trace(
                "Renaming rotated file $dir/$orig$cs -> $dir/$new$cs ...");
            rename "$dir/$orig$cs", "$dir/$new$cs"
                or warn "Can't rename '$dir/$orig$cs' -> '$dir/$new$cs': $!";
        }
    }

    $self->_unlock if $locked;
}

# (re)open file and optionally rotate if necessary
sub _rotate_and_open {
    my ($self) = @_;

    my $fp = $self->file_path;
    my ($do_open, $do_rotate) = @_;

    my @st = stat($fp);
    unless (-e _) {
        # file does not exist yet, create
        $do_open++;
        goto DOIT;
    }
    die "Can't stat '$fp': $!" unless @st;
    my $finode = $st[1];

    # file is not opened yet, open
    unless ($self->{_fh}) {
        $do_open++;
        goto DOIT;
    }

    # period has changed, rotate
    if ($self->{_fp} ne $fp) {
        $do_rotate++;
        goto DOIT;
    }

    # check whether size has been exceeded
    my $inode;
    if ($self->{size} > 0) {
        @st       = stat($self->{_fh});
        my $size  = $st[7];
        $inode    = $st[1];
        if ($size >= $self->{size}) {
            $do_rotate++;
            $self->{_tmp_hack_give_suffix_to_fp} = 1;
            goto DOIT;
        }
    }

    # check whether other process has rename/rotate under us (for example,
    # 'prefix' has been moved to 'prefix.1'), in which case we need to reopen
    if ($inode && $finode != $inode) {
        $do_open++;
        goto DOIT;
    }

  DOIT:
    # rotate
    if ($do_rotate) {
        $self->_rotate;
    }

    # (re)open
    if ($do_open || $do_rotate) {
        open $self->{_fh}, ">>", $fp or die "Can't open '$fp': $!";
        $self->{_fp} = $fp;
    }
}

sub write {
    my $self = shift;

    my $locked = $self->_lock;
    $self->_rotate_and_open;
    my $fh = $self->{_fh};
    print $fh @_; # print syntax limitation?
    $self->_unlock if $locked;
}

sub compress {
    my ($self) = @_;
    die "Not yet implemented";
}

sub DESTROY {
    my ($self) = @_;
    $self->_unlock;
}

1;
#ABSTRACT: Write to files that archive/rotate themselves

=for Pod::Coverage ^(file_path|lock_file_path|DESTROY)$

=head1 SYNOPSIS

 use File::Write::Rotate;

 my $fwr = File::Write::Rotate->new(
     dir       => '/var/log',    # required
     prefix    => 'myapp',       # required
     #suffix   => '.log',        # default is ''
     size      => 25*1024*1024,  # default is 10MB, unless period is set
     histories => 12,            # default is 10
 );

 # write, will write to /var/log/myapp.log, automatically rotate old log files
 # to myapp.log.1 when myapp.log reaches 25MB. will keep old log files up to
 # myapp.log.12.
 $fwr->write("This is a line\n");
 $fwr->write("This is", " another line\n");

 # compress old log files
 $fwr->compress;


=head1 DESCRIPTION

This module can be used to write to file, usually for logging, that can rotate
itself. File will be opened in append mode. Locking will be done (but can be
disabled) to avoid conflict when there are multiple writers. Rotation can be
done by size (after a certain size is reached), by time (daily/monthly/yearly),
or both.

This code for this module is based on L<Log::Dispatch::FileRotate>. For the
purpose of reducing startup overhead and dependencies, I simplify and strip a
few things including: L<Log::Dispatch>-specific stuffs, the use of
L<Date::Manip>, and some features that I do not need like date pattern. The
result is a module that is less flexible, but it fits all my current needs.

I first wrote this module for logging script STDERR output to files (see
L<Tie::Handle::FileRotate>).


=head1 ATTRIBUTES


=head1 METHODS

=head2 $obj = File::Write::Rotate->new(%args)

Create new object. Known arguments:

=over

=item * dir => STR (required)

Directory to put the files in.

=item * prefix => STR (required)

Name of files. The files will be named like the following:

 <prefix><period><suffix><rotate_suffix>

C<< <period> >> will only be given if the C<period> argument is set. If
C<period> is set to C<yearly>, C<< <period> >> will be C<YYYY> (4-digit year).
If C<period> is C<monthly>, C<< <period> >> will be C<YYYY-MM> (4-digit year and
2-digit month). If C<period> is C<daily>, C<< <period> >> will be C<YYYY-MM-DD>
(4-digit year, 2-digit month, and 2-digit day).

C<< <rotate_suffix> >> is either empty string for current file; or C<.1>, C<.2>
and so on for rotated files. C<.1> is the most recent rotated file, C<.2> is the
next most recent, and so on.

An example, with C<prefix> set to C<myapp>:

 myapp         # current file
 myapp.1       # most recently rotated
 myapp.2       # the next most recently rotated

With C<prefix> set to C<myapp>, C<period> set to C<monthly>, C<suffix> set to
C<.log>:

 myapp.2012-12.log     # file name for december 2012
 myapp.2013-01.log     # file name for january 2013

Like previous, but additionally with C<size> also set (which will also rotate
each period file if it exceeds specified size):

 myapp.2012-12.log     # file(s) for december 2012
 myapp.2012-12.log.1
 myapp.2012-12.log.2
 myapp.2013-01.log     # file(s) for january 2013

All times will use local time, so you probably want to set C<TZ> environment
variable or equivalent methods to set time zone.

=item * suffix => STR (default: '')

Suffix to give to file names, usually file extension like C<.log>. See C<prefix>
for more details.

If you use a yearly period, setting suffix is advised to avoid ambiguity with
rotate suffix (for example, is C<myapp.2012> the current file for year 2012 or
file with C<2012> rotate suffix?)

=item * size => INT (default: 10*1024*1024)

Maximum file size, in bytes, before rotation is triggered. The default is 10MB
(10*1024*1024) I<if> C<period> is not set. If C<period> is set, no default for
C<size> is provided, which means files will not be rotated for size (only for
period).

=item * histories => INT (default: 10)

Number of rotated files to keep. After the number of files exceeds this, the
oldest one will be deleted. 0 means not to keep any history, 1 means to only
keep C<.1> file, and so on.

=back

=head2 $fwr->print($str)

=head2 $fwr->compress

Compress old rotated files.


=head1 SEE ALSO

L<Log::Dispatch::FileRotate>, from which the code of this module is based on.
Differences between File::Write::Rotate (FWR) and Log::Dispatch::FileRotate
(LDFR) are as follows. Firstly, FWR is not part of L<Log::Dispatch> family. FWR
does not use L<Date::Manip> (to be tinier) and does not support DatePattern;
instead, FWR replaces it with a simple daily/monthly/yearly period. FWR supports
compressing and rotating compressed old files.

L<Tie::Handle::FileRotate>, which uses this module.

=cut
