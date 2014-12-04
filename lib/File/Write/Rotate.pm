package File::Write::Rotate;

use 5.010001;
use strict;
use warnings;

# we must not use Log::Any, looping if we are used as log output
#use Log::Any '$log';

use Taint::Runtime qw(untaint is_tainted);
use Time::HiRes 'time';
use IO::Compress::Gzip qw(gzip $GzipError);
use File::Spec;
use Scalar::Util qw(weaken);

# VERSION
our $Debug;

sub new {
    my $class = shift;
    my %args  = @_;

    defined( $args{dir} )    or die "Please specify dir";
    defined( $args{prefix} ) or die "Please specify prefix";
    $args{suffix} //= "";

    $args{size} //= 0;

    if ( $args{period} ) {
        $args{period} =~ /daily|day|month|year/i
          or die "Invalid period, please use daily/monthly/yearly";
    }

    for (grep {/\Ahook_/} keys %args) {
        die "Invalid $_, please supply a coderef"
            unless ref($args{$_}) eq 'CODE';
    }

    if ( !$args{period} && !$args{size} ) {
        $args{size} = 10 * 1024 * 1024;
    }

    $args{histories} //= 10;

    $args{_buffer} = [];

    my $self = bless \%args, $class;
    $self;
}

sub buffer_size {
    my $self = shift;
    if (@_) {
        my $old = $self->{buffer_size};
        $self->{buffer_size} = $_[0];
        return $old;
    }
    else {
        return $self->{buffer_size};
    }
}

sub handle {
    my $self = shift;
    $self->{_fh};
}

sub path {
    my $self = shift;
    $self->{_fp};
}

# file path, without the rotate suffix
sub _file_path {
    my ($self) = @_;

    # _now is calculated every time we access this method
    $self->{_now} = time();

    my @lt = localtime( $self->{_now} );
    $lt[5] += 1900;
    $lt[4]++;

    my $period;
    if ( $self->{period} ) {
        if ( $self->{period} =~ /year/i ) {
            $period = sprintf( "%04d", $lt[5] );
        }
        elsif ( $self->{period} =~ /month/i ) {
            $period = sprintf( "%04d-%02d", $lt[5], $lt[4] );
        }
        elsif ( $self->{period} =~ /day|daily/i ) {
            $period = sprintf( "%04d-%02d-%02d", $lt[5], $lt[4], $lt[3] );
        }
    }
    else {
        $period = "";
    }

    my $path = join(
        '',
        $self->{dir}, '/',
        $self->{prefix},
        length($period) ? ".$period" : "",
        $self->{suffix},
    );
    if (wantarray) {
        return ($path, $period);
    } else {
        return $path;
    }
}

sub lock_file_path {
    my ($self) = @_;
    return File::Spec->catfile($self->{dir}, $self->{prefix} . '.lck');
}

sub _get_lock {
    my ($self) = @_;
    return $self->{_weak_lock} if defined($self->{_weak_lock});

    require SHARYANTO::File::Flock;
    my $lock = SHARYANTO::File::Flock->lock( $self->lock_file_path );
    $self->{_weak_lock} = $lock;
    weaken $self->{_weak_lock};
    return $lock;
}

# will return \@files. each entry is [filename without compress suffix,
# rotate_suffix (for sorting), period (for sorting), compress suffix (for
# renaming back)]
sub _get_files {
    my ($self) = @_;

    opendir my ($dh), $self->{dir} or do {
        warn "Can't opendir '$self->{dir}': $!";
        return;
    };

    my @files;
    while ( my $e = readdir($dh) ) {
        my $cs = $1 if $e =~ s/(\.gz)\z//; # compress suffix
        next unless $e =~ /\A\Q$self->{prefix}\E
                           (?:\. (?<period>\d{4}(?:-\d\d(?:-\d\d)?)?) )?
                           \Q$self->{suffix}\E
                           (?:\. (?<rotate_suffix>\d+) )?
                           \z
                          /x;
        push @files,
          [ $e, $+{rotate_suffix} // 0, $+{period} // "", $cs // "" ];
    }
    closedir($dh);

    [ sort { $a->[2] cmp $b->[2] || $b->[1] <=> $a->[1] } @files ];
}

# rename (increase rotation suffix) and keep only n histories. note: failure in
# rotating should not be fatal, we just warn and return.
sub _rotate_and_delete {
    my ($self, %opts) = @_;

    my $delete_only = $opts{delete_only};
    my $lock = $self->_get_lock;
  CASE:
    {
        my $files = $self->_get_files or last CASE;

        # is there a compression process in progress? this is marked by the
        # existence of <prefix>-compress.pid PID file.
        #
        # XXX check validity of PID file, otherwise a stale PID file will always
        # prevent rotation to be done
        if ( -f "$self->{dir}/$self->{prefix}-compress.pid" ) {
            warn "Compression is in progress, rotation is postponed";
            last CASE;
        }

        $self->{hook_before_rotate}->($self, [map {$_->[0]} @$files])
            if $self->{hook_before_rotate};

        my @deleted;
        my @renamed;

        my $i;
        my $dir = $self->{dir};
        my $rotating_period = @$files ? $files->[-1][2] : undef;
        for my $f (@$files) {
            my ( $orig, $rs, $period, $cs ) = @$f;
            $i++;

            #say "DEBUG: is_tainted \$dir? ".is_tainted($dir);
            #say "DEBUG: is_tainted \$orig? ".is_tainted($orig);
            #say "DEBUG: is_tainted \$cs? ".is_tainted($cs);

            # TODO actually, it's more proper to taint near the source (in this
            # case, _get_files)
            untaint \$orig;

            if ( $i <= @$files - $self->{histories} ) {
                say "DEBUG: Deleting old rotated file $dir/$orig$cs ..."
                    if $Debug;
                if (unlink "$dir/$orig$cs") {
                    push @deleted, "$orig$cs";
                } else {
                    warn "Can't delete $dir/$orig$cs: $!";
                }
                next;
            }
            if ( !$delete_only && defined($rotating_period) && $period eq $rotating_period ) {
                my $new = $orig;
                if ($rs) {
                    $new =~ s/\.(\d+)\z/"." . ($1+1)/e;
                }
                else {
                    $new .= ".1";
                }
                if ( $new ne $orig ) {
                    say "DEBUG: Renaming rotated file $dir/$orig$cs -> ".
                        "$dir/$new$cs ..." if $Debug;
                    if (rename "$dir/$orig$cs", "$dir/$new$cs") {
                        push @renamed, "$new$cs";
                    } else {
                        warn "Can't rename '$dir/$orig$cs' -> '$dir/$new$cs': $!";
                    }
                }
            }
        }

        $self->{hook_after_rotate}->($self, \@renamed, \@deleted)
            if $self->{hook_after_rotate};
    } # CASE
}

sub _open {
    my $self = shift;

    my ($fp, $period) = $self->_file_path;
    open $self->{_fh}, ">>", $fp or die "Can't open '$fp': $!";
    if ( defined $self->{binmode} ) {
        binmode $self->{_fh}, $self->{binmode}
          or die "Can't set PerlIO layer on '$fp': $!";
    }
    my $oldfh = select $self->{_fh};
    $| = 1;
    select $oldfh;    # set autoflush
    $self->{_fp} = $fp;
    $self->{hook_after_create}->($self) if $self->{hook_after_create};
}

# (re)open file and optionally rotate if necessary
sub _rotate_and_open {

    my $self = shift;
    my ( $do_open, $do_rotate ) = @_;
    my $fp = $self->_file_path;
    my %rotate_params = ();

  CASE:
    {
        unless ( -e $fp ) {
            $do_open++;
            $do_rotate++;
            $rotate_params{delete_only} = 1;
            last CASE;
        }

        # file is not opened yet, open
        unless ( $self->{_fh} ) {
            $self->_open;
        }

        # period has changed, rotate
        if ( $self->{_fp} ne $fp ) {
            $do_rotate++;
            $rotate_params{delete_only} = 1;
            last CASE;
        }

        # check whether size has been exceeded
        my $inode;

        if ( $self->{size} > 0 ) {

            my @st   = stat( $self->{_fh} );
            my $size = $st[7];
            $inode = $st[1];

            if ( $size >= $self->{size} ) {
                say "DEBUG: Size of $self->{_fp} is $size, exceeds $self->{size}, rotating ..."
                    if $Debug;
                $do_rotate++;
                last CASE;
            }
            else {

                # stat the current file (not our handle _fp)
                my @st = stat($fp);
                die "Can't stat '$fp': $!" unless @st;
                my $finode = $st[1];

                # check whether other process has rename/rotate under us (for
                # example, 'prefix' has been moved to 'prefix.1'), in which case
                # we need to reopen
                if ( ( defined($inode) ) and ( $finode != $inode ) ) {
                    $do_open++;
                }

            }

        }
    } # CASE

    $self->_rotate_and_delete(%rotate_params) if $do_rotate;
    $self->_open if $do_rotate || $do_open;    # (re)open
}

sub write {
    my $self = shift;

    # the buffering implementation is currently pretty naive. it assume any
    # die() as a write failure and store the message to buffer.

    # FYI: if privilege is dropped from superuser, the failure is usually at
    # locking the lock file (permission denied).

    my @msg = ( map( {@$_} @{ $self->{_buffer} } ), @_ );

    eval {
        my $lock = $self->_get_lock;

        $self->_rotate_and_open;

        $self->{hook_before_write}->($self, \@msg, $self->{_fh})
            if $self->{hook_before_write};

        # syntax limitation? can't do print $self->{_fh} ... directly
        my $fh = $self->{_fh};
        print $fh @msg;
        $self->{_buffer} = [];

    };
    my $err = $@;

    if ($err) {
        if ( ( $self->{buffer_size} // 0 ) > @{ $self->{_buffer} } ) {

            # put message to buffer temporarily
            push @{ $self->{_buffer} }, [@_];
        }
        else {
            # buffer is already full, let's dump the buffered + current message
            # to the die message anyway.
            die join(
                "",
                "Can't write",
                (
                    @{ $self->{_buffer} }
                    ? " (buffer is full, "
                      . scalar( @{ $self->{_buffer} } )
                      . " message(s))"
                    : ""
                ),
                ": $err, message(s)=",
                @msg
            );
        }
    }
}

sub compress {
    my ($self) = shift;

    require Proc::PID::File;

    my $lock           = $self->_get_lock;
    my $files_ref        = $self->_get_files;
    my $done_compression = 0;

    if ( @{$files_ref} ) {
        my $pid = Proc::PID::File->new(
            dir    => $self->{dir},
            name   => "$self->{prefix}-compress",
            verify => 1,
        );
        my $latest_period = $files_ref->[-1][2];

        if ( $pid->alive ) {
            warn "Another compression is in progress";
        } else {
            my @tocompress;
            #use DD; dd $self;
            for my $file_ref ( @{$files_ref} ) {
                my ($orig, $rs, $period, $cs) = @{ $file_ref };
                #say "D:compress: orig=<$orig> rs=<$rs> period=<$period> cs=<$cs>";
                next if $cs; # already compressed
                next if !$self->{period} && !$rs; # not old file
                next if  $self->{period} && $period eq $latest_period; # not old file
                push @tocompress, $orig;
            }

            if (@tocompress) {
                my $dir = $self->{dir};
                foreach my $file (@tocompress) {
                    gzip( $file => File::Spec->catfile( $dir, "$file.gz" ) )
                        or do { warn "gzip failed: $GzipError\n"; next };
                    unlink $file;
                }
                $done_compression = 1;
            }
        }
    }

    return $done_compression;

}

sub DESTROY {
    my ($self) = @_;

    # Proc::PID::File's DESTROY seem to create an empty PID file, remove it.
    unlink "$self->{dir}/$self->{prefix}-compress.pid";
}

1;

#ABSTRACT: Write to files that archive/rotate themselves

=for Pod::Coverage ^(file_path|DESTROY)$

=head1 SYNOPSIS

 use File::Write::Rotate;

 my $fwr = File::Write::Rotate->new(
     dir          => '/var/log',    # required
     prefix       => 'myapp',       # required
     #suffix      => '.log',        # default is ''
     size         => 25*1024*1024,  # default is 10MB, unless period is set
     histories    => 12,            # default is 10
     #buffer_size => 100,           # default is none
 );

 # write, will write to /var/log/myapp.log, automatically rotate old log files
 # to myapp.log.1 when myapp.log reaches 25MB. will keep old log files up to
 # myapp.log.12.
 $fwr->write("This is a line\n");
 $fwr->write("This is", " another line\n");

To compressing old log files:

 $fwr->compress;

This is usually done in a separate process, because it potentially takes a long
time if the files to compress are large; we are rotating automatically in
write() so doing automatic compression too would annoyingly block writer for a
potentially long time.


=head1 DESCRIPTION

This module can be used to write to file, usually for logging, that can rotate
itself. File will be opened in append mode. Locking will be done to avoid
conflict when there are multiple writers. Rotation can be done by size (after a
certain size is reached), by time (daily/monthly/yearly), or both.

I first wrote this module for logging script STDERR output to files (see
L<Tie::Handle::FileWriteRotate>).

=head1 ATTRIBUTES

=head2 buffer_size => int

Get or set buffer size. If set to a value larger than 0, then when a write()
failed, instead of dying, the message will be stored in an internal buffer first
(a regular Perl array). When the number of items in the buffer exceeds this
size, then write() will die upon failure. Otherwise, every write() will try to
flush the buffer.

Can be used for example when a program runs as superuser/root then temporarily
drops privilege to a normal user. During this period, logging can fail because
the program cannot lock the lock file or write to the logging directory. Before
dropping privilege, the program can set buffer_size to some larger-than-zero
value to hold the messages emitted during dropping privilege. The next write()
as the superuser/root will succeed and flush the buffer to disk (provided there
is no other error condition, of course).

=head2 path => str (ro)

Current file's path.

=head2 handle => (ro)

Current file handle. You should not use this directly, but use write() instead.
This attribute is provided for special circumstances (e.g. in hooks, see example
in the hook section).

=head2 hook_before_write => code

Will be called by write() before actually writing to filehandle (but after
locking is done). Code will be passed ($self, \@msgs, $fh) where @msgs is an
array of strings to be written (the contents of buffer, if any, plus arguments
passed to write()) and $fh is the filehandle.

=head2 hook_before_rotate => code

Will be called by the rotating routine before actually doing rotating. Code will
be passed ($self).

This can be used to write a footer to the end of each file, e.g.:

 # hook_before_rotate
 my ($self) = @_;
 my $fh = $self->handle;
 print $fh "Some footer\n";

Since this hook is indirectly called by write(), locking is already done.

=head2 hook_after_rotate => code

Will be called by the rotating routine after the rotating process. Code will be
passed ($self, \@renamed, \@deleted) where @renamed is array of new filenames
that have been renamed, @deleted is array of new filenames that have been
deleted.

=head2 hook_after_create => code

Will be called by after a new file is created. Code will be passed ($self).

This hook can be used to write a header to each file, e.g.:

 # hook_after_create
 my ($self) = @_;
 my $fh $self->handle;
 print $fh "header\n";

Since this is called indirectly by write(), locking is also already done.

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

=item * buffer_size => INT (default: 0)

Set initial value of buffer. See the C<buffer_size> attribute for more
information.

=item * hook_before_write => CODE

=item * hook_before_rotate => CODE

=item * hook_after_rotate => CODE

=item * hook_after_create => CODE

See L</ATTRIBUTES>.

=back

=head2 lock_file_path => STR

Returns a string representing the complete pathname to the lock file, based
on C<dir> and C<prefix> attributes.

=head2 $fwr->write(@args)

Write to file. Will automatically rotate file if period changes or file size
exceeds specified limit. When rotating, will only keep a specified number of
histories and delete the older ones. Uses locking, so multiple writers do not
clobber one another. Lock file is named C<< <prefix> >>C<.lck>. Will wait for up
to 1 minute to acquire lock, will die if failed to acquire lock.

Does not append newline so you'll have to do it yourself.

=head2 $fwr->compress

Compress old rotated files and remove the uncompressed originals. Currently uses
L<IO::Compress::Gzip> to do the compression. Extension given to compressed file
is C<.gz>.

Will not lock writers, but will create C<< <prefix> >>C<-compress.pid> PID file
to prevent multiple compression processes running and to signal the writers to
postpone rotation.

After compression is finished, will remove the PID file, so rotation can be done
again on the next C<write()> if necessary.

=head1 FAQ

=head2 Why use autorotating file?

Mainly convenience and low maintenance. You no longer need a separate rotator
like the Unix B<logrotate> utility (which when accidentally disabled or
misconfigured will cause your logs to stop being rotated and grow indefinitely).

=head2 What is the downside of using FWR (and LDFR)?

Mainly performance overhead, as every write() involves locking to make it safe
to use with multiple processes. Tested on my Core i5 3.1 GHz desktop, writing
lines in the size of ~ 200 bytes, raw writing to disk (SSD) has the speed of
around 3.4mil/s, while using FWR it comes down to around 19.5k/s.

However, this is not something you'll notice or need to worry about unless
you're writing near that speed.


=head1 TODO

Perhaps an option to disable locking.

=head1 SEE ALSO

L<Log::Dispatch::FileRotate>, which inspires this module. Differences between
File::Write::Rotate (FWR) and Log::Dispatch::FileRotate (LDFR) are as follows:

=over

=item * FWR is not part of the L<Log::Dispatch> family.

This makes FWR more general to use.

For using together with Log::Dispatch/Log4perl, I have also written
L<Log::Dispatch::FileWriteRotate> which is a direct (although not a perfect
drop-in) replacement for Log::Dispatch::FileRotate.

=item * Secondly, FWR does not use L<Date::Manip>.

Date::Manip is relatively large (loading Date::Manip 6.37 equals to loading 34
files and ~ 22k lines; while FWR itself is only < 1k lines!)

As a consequence of this, FWR does not support DatePattern; instead, FWR
replaces it with a simple daily/monthly/yearly period.

=item * And lastly, FWR supports compressing and rotating compressed old files.

Using separate processes like the Unix B<logrotate> utility means having to deal
with yet another race condition. FWR takes care of that for you (see the
compress() method). You also have the option to do file compression in the same
script/process if you want, which is convenient.

=back

There is no significant overhead difference between FWR and LDFR (FWR is
slightly faster than LDFR on my testing).

L<Tie::Handle::FileWriteRotate> and Log::Dispatch::FileWriteRotate, which use
this module.

=cut
