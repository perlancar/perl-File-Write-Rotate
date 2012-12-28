package File::Write::Rotate;

use 5.010;
use strict;
use warnings;
# we must not use Log::Any, looping if we are used as log output
#use Log::Any '$log';

use Time::HiRes 'time';

# VERSION
our $Debug;

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

    my $self = bless \%args, $class;
    $self;
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

sub _lock {
    my ($self) = @_;

    if ($self->{_lock}) {
        return $self->{_lock}->_lock;
    } else {
        require SHARYANTO::File::Flock;
        $self->{_lock} = SHARYANTO::File::Flock->lock(
            $self->lock_file_path, {unlink=>1});
        return 1;
    }
}

sub _unlock {
    my ($self) = @_;

    $self->{_lock}->_unlock if $self->{_lock};
}

# will return \@files. each entry is [filename without compress suffix,
# rotate_suffix (for sorting), period (for sorting), compress suffix (for
# renaming back)]
sub _get_files {
    my ($self) = @_;

    opendir my($dh), $self->{dir} or do {
        warn "Can't opendir '$self->{dir}': $!";
        return;
    };

    my @files;
    while (my $e = readdir($dh)) {
        my $cs = $1 if $e =~ s/(\.gz)\z//;
        next unless $e =~ /\A\Q$self->{prefix}\E
                           (?:\. (?<period>\d{4}(?:-\d\d(?:-\d\d)?)?) )?
                           \Q$self->{suffix}\E
                           (?:\. (?<rotate_suffix>\d+) )?
                           \z
                          /x;
        push @files, [$e, $+{rotate_suffix} // 0, $+{period} // "",
                      $cs // ""];
    }
    closedir($dh);

    [sort {$b->[1] <=> $a->[1] || $a->[2] cmp $b->[2]} @files];
}

# rename (increase rotation suffix) and keep only n histories. note: failure in
# rotating should not be fatal, we just warn and return.
sub _rotate {
    my ($self) = @_;

    my $locked = $self->_lock;
    my $files  = $self->_get_files or goto EXIT;

    # is there a compression process in progress? this is marked by the
    # existence of <prefix>-compress.pid PID file.
    if (-f "$self->{dir}/$self->{prefix}-compress.pid") {
        warn "Compression is in progress, rotation is postponed";
        goto EXIT;
    }

    my $i;
    my $dir = $self->{dir};
    for my $f (@$files) {
        my ($orig, $rs, $period, $cs) = @$f;
        $i++;
        if ($i <= @$files-$self->{histories}) {
            say "D: Deleting old rotated file $dir/$orig$cs ..." if $Debug;
            unlink "$dir/$orig$cs" or warn "Can't delete $dir/$orig$cs: $!";
            next;
        }
        my $new = $orig;
        if ($rs) {
            $new =~ s/\.(\d+)\z/"." . ($1+1)/e;
        } elsif (!$period || delete($self->{_tmp_hack_give_suffix_to_fp})) {
            $new .= ".1";
        }
        if ($new ne $orig) {
            say "D: Renaming rotated file $dir/$orig$cs -> $dir/$new$cs ..." if $Debug;
            rename "$dir/$orig$cs", "$dir/$new$cs"
                or warn "Can't rename '$dir/$orig$cs' -> '$dir/$new$cs': $!";
        }
    }

  EXIT:
    $self->_unlock if $locked;
}

sub _open {
    my $self = shift;

    my $fp = $self->file_path;
    open $self->{_fh}, ">>", $fp or die "Can't open '$fp': $!";
    my $oldfh = select $self->{_fh}; $| = 1; select $oldfh; # set autoflush
    $self->{_fp} = $fp;
}

# (re)open file and optionally rotate if necessary
sub _rotate_and_open {
    my ($self) = @_;

    my $fp = $self->file_path;
    my ($do_open, $do_rotate) = @_;

    # stat the current file (not our handle _fp)
    my @st = stat($fp);
    # file does not exist yet, create
    unless (-e _) {
        $do_open++;
        goto DOIT;
    }
    die "Can't stat '$fp': $!" unless @st;
    my $finode = $st[1];

    # file is not opened yet, open
    unless ($self->{_fh}) {
        $self->_open;
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
            say "D: Size of $self->{_fp} is $size, exceeds $self->{size}, rotating ..." if $Debug;
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
    $self->_rotate if $do_rotate;
    $self->_open   if $do_rotate || $do_open; # (re)open
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
    require File::Which;
    require Proc::PID::File;

    my ($self) = @_;

    my $cmd;
    for (qw/pigz gzip/) {
        if (File::Which::which($_)) {
            $cmd = $_; last;
        }
    }
    if (!$cmd) {
        warn "No compressor program available, compress aborted";
        return;
    }

    my $locked = $self->_lock;
    my $files  = $self->_get_files or goto EXIT1;

    my $pid = Proc::PID::File->new(
        dir    => $self->{dir},
        name   => "$self->{prefix}-compress",
        verify => 1,
    );
    if ($pid->alive) {
        warn "Another compression is in progress";
        goto EXIT1;
    }

    my @tocompress;
    my $dir = $self->{dir};
    for my $f (@$files) {
        my ($orig, $rs, $period, $cs) = @$f;
        next unless $rs;
        next if $cs;
        push @tocompress, $orig;
    }

  EXIT1:
    $self->_unlock if $locked;
    if (@tocompress) {
        system $cmd, "--force", (map {"$self->{dir}/$_"} @tocompress);
    }
}

sub DESTROY {
    my ($self) = @_;
    $self->_unlock;

    # Proc::PID::File's DESTROY seem to create an empty PID file, remove it.
    unlink "$self->{dir}/$self->{prefix}-compress.pid";
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
itself. File will be opened in append mode. Locking will be done to avoid
conflict when there are multiple writers. Rotation can be done by size (after a
certain size is reached), by time (daily/monthly/yearly), or both.

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

=item * period => STR

Can be set to either C<daily>, C<monthly>, or C<yearly>. If set, will
automatically rotate after period change. See C<prefix> for more details.

=item * histories => INT (default: 10)

Number of rotated files to keep. After the number of files exceeds this, the
oldest one will be deleted. 0 means not to keep any history, 1 means to only
keep C<.1> file, and so on.

=back

=head2 $fwr->write(@args)

Write to file. Will automatically rotate file if period changes or file size
exceeds specified limit. When rotating, will only keep a specified number of
histories and delete the older ones. Uses locking, so multiple writers do not
clobber one another. Lock file is named C<< <prefix> >>C<.lck>. Will wait for up
to 1 minute to acquire lock, will die if failed to acquire lock.

Does not append newline so you'll have to do it yourself.

=head2 $fwr->compress

Compress old rotated files. Currently uses B<pigz> or B<gzip> program to do the
compression. Extension given to compressed file is C<.gz>.

Normally, should be done using a separate process so as to avoid blocking the
writers.

Will not lock writers, but will create C<< <prefix> >>C<-compress.pid> PID file
to prevent multiple compression processes running and to signal the writers to
postpone rotation.

After compression is finished, will remove the PID file, so rotation can be done
again on the next C<write()> if necessary.


=head1 SEE ALSO

L<Log::Dispatch::FileRotate>, which inspires this module. Differences between
File::Write::Rotate (FWR) and Log::Dispatch::FileRotate (LDFR) are as follows.
Firstly, FWR is not part of the L<Log::Dispatch> family. FWR does not use
L<Date::Manip> (to be tinier) and does not support DatePattern; instead, FWR
replaces it with a simple daily/monthly/yearly period. FWR supports compressing
and rotating compressed old files.

L<Tie::Handle::FileRotate>, which uses this module.

=cut
