package File::Write::Rotate;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Fcntl ':flock';

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
}

# file path, without the rotate suffix
sub file_path {
    my ($self) = @_;

    my @lt = localtime();
    $lt[5] += 1900;
    $lt[4]++;

    my $period;
    if ($self->{period}) {
        if ($self->{period} =~ /year/i) {
            $period = sprintf(".%04d", $lt[5]);
        } elsif ($self->{period} =~ /month/i) {
            $period = sprintf(".%04d-%02d", $lt[5], $lt[4]);
        } elsif ($self->{period} =~ /day/i) {
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

# BEGIN adapted from LDFR


sub print {
    my $self = shift;
    my %p = @_;

        my $max_size = $self->{size};
        my $numfiles = $self->{max};
        my $name     = $self->{params}->{filename};
        my $fh       = $self->{LDF}->{fh};

        # Prime our time based data outside the critical code area
        my ($in_time_mode,$time_to_rotate) = $self->time_to_rotate();

        # Handle critical code for logging. No changes if someone else is in
        if( !$self->lfhlock_test() )
        {
                warn "$$ waiting on lock\n" if $self->{debug};
                unless($self->lfhlock())
                {
                        warn "$$ failed to get lock. returning\n" if $self->{debug};
                        return;
                }
                warn "$$ got lock after wait\n" if $self->{debug};
        }

        my $size   = (stat($fh))[7];   # Stat the handle to get real size
        my $inode  = (stat($fh))[1];   # get real inode
        my $finode = (stat($name))[1]; # Stat the name for comparision
        warn localtime()." $$  s=$size, i=$inode, f=".
                        (defined $finode ? $finode : "undef") .
                         ", n=$name\n" if $self->{debug};

        # If finode and inode are the same then nobody has done a rename
        # under us and we can continue. Otherwise just close and reopen.
        # Time mode overrides Size mode
        if(!defined($finode) || $inode != $finode)
        {
                # Oops someone moved things on us. So just reopen our log
                delete $self->{LDF};  # Should get rid of current LDF
                $self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log

                warn localtime()." $$ Someone else rotated: normal log\n" if $self->{debug};
                $self->logit($p{message});
        }
        elsif($in_time_mode && !$time_to_rotate)
        {
                warn localtime()." $$ In time mode: normal log\n" if $self->{debug};
                $self->logit($p{message});
        }
        elsif(!$in_time_mode && defined($size) && $size < $max_size )
        {
                warn localtime()." $$ In size mode: normal log\n" if $self->{debug};
                $self->logit($p{message});
        }
        # Need to rotate
        elsif(($in_time_mode && $time_to_rotate) ||
              (!$in_time_mode && $size)
                 )
        {
                # Shut down the log
                delete $self->{LDF};  # Should get rid of current LDF

                my $idx = $numfiles -1;

                warn localtime() . " $$ Rotating\n" if $self->{debug};
                while($idx >= 0)
                {
                        if($idx <= 0)
                        {
                                warn "$$ rename $name $name.1\n" if $self->{debug};
                                rename($name, "$name.1");
                        }
                        else
                        {
                                warn "$$ rename $name.$idx $name.".($idx+1)."\n" if $self->{debug};
                                rename("$name.$idx", "$name.".($idx+1));
                        }

                        $idx--;
                }
                warn localtime() . " $$ Rotating Done\n" if $self->{debug};

                # reopen the logfile for writing.
                $self->{LDF} =  Log::Dispatch::File->new(%{$self->{params}});  # Our log

                # Write it out
                warn localtime()." $$ rotated: normal log\n" if $self->{debug};
                $self->logit($p{message});
        }
        #else size is zero :-} just don't do anything!

        $self->lfhunlock();
}

sub DESTROY
{
    my $self = shift;

    if ( $self->{LDF} )
    {
                delete $self->{LDF};  # Should get rid of current LDF
    }

        # Clean up locks
        close $self->{lfh} if $self->{lfh};
        unlink $self->{lf} if -f $self->{lf};
}

sub logit
{
        my $self = $_[0];

        $self->lock();
        $self->{LDF}->log_message(message => $_[1]);
        $self->unlock();
        return;
}


###########################################################################
#
# Subroutine time_to_rotate
#
#       Args: none
#
#       Rtns: (1,n)  if we are in time mode and its time to rotate
#                    n defines the number of timers that expired
#             (1,0)  if we are in time mode but not ready to rotate
#             (0,0)  otherwise
#
# Description:
#     time_to_rotate - update internal clocks and return status as
#     defined above
#
# If we have just been created then the first recurrance is an indication
# to check against the log file.
#
#
#       my ($in_time_mode,$time_to_rotate) = $self->time_to_rotate();
sub time_to_rotate
{
    my $self   = shift;        # My object
        my $mode   = defined($self->{'recurrance'});
        my $rotate = 0;

        if($mode)
        {
                # Then do some checking and update ourselves if we think we need
                # to rotate. Wether we rotate or not is up to our caller. We
                # assume they know what they are doing!

                # Only stat the log file here if we are in our first invocation.
                my $ftime = 0;
                if($self->{'new'})
                {
                        # Last time the log file was changed
                        $ftime   = (stat($self->{LDF}{fh}))[9];

                        # In Date::Manip format
                        # $ftime   = ParseDate(scalar(localtime($ftime)));
                }

                # Check need for rotation. Loop through our recurrances looking
                # for expiration times. Any we find that have expired we update.
                my $tm    = $self->{timer}->();
                my @recur = @{$self->{'recurrance'}};
                @{$self->{'recurrance'}} = ();
                for my $rec (@recur)
                {
                        my ($abs,$pat) = @$rec;

                        # Extra checking
                        unless(defined $abs && $abs)
                        {
                                warn "Bad time found for recurrance pattern $pat: $abs\n";
                                next;
                        }
                        my $dorotate = 0;

                        # If this is first time through
                        if($self->{'new'})
                        {
                                # If it needs a rotate then flag it
                                if($ftime <= $abs)
                                {
                                        # Then we need to rotate
                                        warn "Need rotate file($ftime) <= $abs\n" if $self->{debug};
                                        $rotate++;
                                        $dorotate++;  # Just for debugging
                                }

                                # Move to next occurance regardless
                                warn "Dropping initial occurance($abs)\n" if $self->{debug};
                                $abs = $self->_get_next_occurance($pat);
                                unless(defined $abs && $abs)
                                {
                                        warn "Next occurance is null for $pat\n";
                                        $abs = 0;
                                }
                        }
                        # Elsif it is time to rotate
                        #elsif(Date_Cmp($abs,$tm) <= 0)
                        elsif($abs <= $tm)
                        {
                                # Then we need to rotate
                                warn "Need rotate $abs <= $tm\n" if $self->{debug};
                                $abs = $self->_get_next_occurance($pat);
                                unless(defined $abs && $abs)
                                {
                                        warn "Next occurance is null for $pat\n";
                                        $abs = 0;
                                }
                                $rotate++;
                                $dorotate++;  # Just for debugging
                        }
                        push(@{$self->{'recurrance'}},[$abs,$pat]) if $abs;
                        warn "time_to_rotate(mode,rotate,next) => ($mode,$dorotate,$abs)\n" if $self->{debug};
                }

        }

        $self->{'new'} = 0;  # No longer brand-spankers

        warn "time_to_rotate(mode,rotate) => ($mode,$rotate)\n" if $self->{debug};
        return wantarray ? ($mode,$rotate) : $rotate;
}

###########################################################################
#
# Subroutine _gen_occurance
#
#       Args: Date::Manip occurance pattern
#
#       Rtns: array of dates for next few events
#
#  If asked we will return an inital occurance that is before the current
#  time. This can be used to see if we need to rotate on start up. We are
#  often called by CGI (short lived) proggies :-(
#
sub _gen_occurance
{
    my $self = shift;        # My object
    my $pat  = shift;

        # Do we return an initial occurance before the current time?
        my $initial = shift || 0;

        my $range = '';
        my $base  = 'now'; # default to calcs based on the current time

        if($pat =~ /^0:0:0:0:0/) # Small recurrance less than 1 hour
        {
                $range = "4 hours later";
                $base  = "1 hours ago" if $initial;
        }
        elsif($pat =~ /^0:0:0:0/) # recurrance less than 1 day
        {
                $range = "4 days later";
                $base  = "1 days ago" if $initial;
        }
        elsif($pat =~ /^0:0:0:/) #  recurrance less than 1 week
        {
                $range = "4 weeks later";
                $base  = "1 weeks ago" if $initial;
        }
        elsif($pat =~ /^0:0:/) #  recurrance less than 1 month
        {
                $range = "4 months later";
                $base  = "1 months ago" if $initial;
        }
        elsif($pat =~ /^0:/) #  recurrance less than 1 year
        {
                $range = "24 months later";
                $base  = "24 months ago" if $initial;
        }
        else # years
        {
                my($yrs) = $pat =~ m/^(\d+):/;
                $yrs = 1 unless $yrs;
                my $months = $yrs * 4 * 12;

                $range = "$months months later";
                $base  = "$months months ago" if $initial;
        }

        # The next date must start at least 1 second away from now other wise
        # we may rotate for every message we recieve with in this second :-(
        my $start = DateCalc($base,"+ 1 second");

        warn "ParseRecur($pat,$base,$start,$range);\n" if $self->{debug};
        my @dates = ParseRecur($pat,$base,$start,$range);

        # Just in case we have a bad parse or our assumptions are wrong.
        # We default to days
        unless(scalar @dates >= 2)
        {
                warn "Failed to parse ($pat). Going daily\n";
                @dates = ParseRecur('0:0:0:1*0:0:0',"now","now","1 months later");
                if($initial)
                {
                        @dates = ParseRecur('0:0:0:1*0:0:0',"2 days ago","2 days ago","1 months later");
                }
        }

        # Convert the dates to seconds since the epoch so we can use
        # numerical comparision instead of textual
        my @epochs = ();
        my @a = ('%Y','%m','%d','%H','%M','%S');
        foreach(@dates)
        {
                my($y,$m,$d,$h,$mn,$s) = Date::Manip::UnixDate($_, @a);
                my $e = Date_SecsSince1970GMT($m,$d,$y,$h,$mn,$s);
                if( $self->{debug} )
                {
                        warn "Date to epochs ($_) => ($e)\n";
                }
                push @epochs, $e;
        }

        # Clean out all but the one previous to now if we are doing an
        # initial occurance
        my $now = time();
        if($initial)
        {
                my $before = '';
                while(@epochs && ( $epochs[0] <= $now) )
                {
                        $before = shift(@epochs);
                        #warn "Shifting $before\n";
                }
                #warn "Unshifting $before\n";
                unshift(@epochs,$before) if $before;
        }
        else
        {
                # Clean out dates that occur before now, being careful not to loop
                # forever (thanks James).
                shift(@epochs) while @epochs && ( $epochs[0] <= $now);
        }

        if($self->{debug})
        {
                warn "Recurrances are at: ".join("\n\t", @dates),"\n";
        }
        warn "No recurrances found! Probably a timezone issue!\n" unless @dates;

        return @epochs;
}

###########################################################################
#
# Subroutine _get_next_occurance
#
#       Args: Date::Manip occurance pattern
#
#       Rtns: date
#
# We don't want to call Date::Manip::ParseRecur too often as it is very
# expensive. So, we cache what is returned from _gen_occurance().
sub _get_next_occurance
{
    my $self = shift;        # My object
    my $pat  = shift;

        # (ms) Throw out expired occurances
        my $now = $self->{timer}->();
        if(defined $self->{'dates'}{$pat})
        {
                while( @{$self->{'dates'}{$pat}} )
                {
                        last if $self->{'dates'}{$pat}->[0] >= $now;
                        shift @{$self->{'dates'}{$pat}};
                }
        }

        # If this is first time then generate some new ones including one
        # before our time to test against the log file
        if(!defined $self->{'dates'}{$pat})
        {
                @{$self->{'dates'}{$pat}} = $self->_gen_occurance($pat,1);
        }
        # Elsif close to the end of what we have
        elsif( scalar(@{$self->{'dates'}{$pat}}) < 2)
        {
                @{$self->{'dates'}{$pat}} = $self->_gen_occurance($pat);
        }

        return( shift(@{$self->{'dates'}{$pat}}) );
}


# Lock and unlock routines. For when we need to write a message.
use Fcntl ':flock'; # import LOCK_* constants

sub lock
{
        my $self = shift;

        flock($self->{LDF}->{fh},LOCK_EX);

        # Make sure we are at the EOF
        seek($self->{LDF}->{fh}, 0, 2);

        warn localtime() ." $$ Locked\n" if $self->{debug};
        return;
}

sub unlock
{
        my $self = shift;
        flock($self->{LDF}->{fh},LOCK_UN);
        warn localtime() . " $$ unLocked\n" if $self->{debug};
}

# Lock and unlock routines. For when we need to roll the logs.
#
# Note: On May 1, Dan Waldheim's good news was:
# I discovered something interesting about forked processes and locking.
# If the parent "open"s the filehandle and then forks, exclusive locks
# don't work properly between the parent and children.  Anyone can grab a
# lock while someone else thinks they have it.  To work properly the
# "open" has to be done within each process.
#
# Thanks Dan
sub lfhlock_test
{
        my $self = shift;

        if (open(LFH, ">>$self->{lf}"))
        {
                $self->{lfh} = *LFH;
                if (flock($self->{lfh}, LOCK_EX | LOCK_NB))
                {
                        warn "$$ got lock on Lock File ".$self->{lfh}."\n" if $self->{debug};
                        return 1;
                }
        }
        else
        {
                $self->{lfh} = 0;
                warn "$$ couldn't get lock on Lock File\n" if $self->{debug};
                return 0;
        }
}

sub lfhlock
{
        my $self = shift;

        if (!$self->{lfh})
        {
                if (!open(LFH, ">>$self->{lf}"))
                {
                        return 0;
                }
                $self->{lfh} = *LFH;
        }

        flock($self->{lfh},LOCK_EX);
}

sub lfhunlock
{
        my $self = shift;

        if($self->{lfh})
        {
                flock($self->{lfh},LOCK_UN);
                close $self->{lfh};
                $self->{lfh} = 0;
        }
}

# END adapted from LDFR

sub write {}

sub compress {}

sub lock {}

sub unlock {}

sub rotate {}

sub time_to_rotate {}

1;
#ABSTRACT: Write to files that archive/rotate themselves

=for Pod::Coverage ^(file_path|time_to_rotate|lock|unlock|rotate)$

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
 $fwr->print("This is a line\n");
 $fwr->print("This is another line\n");

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
instead, FWR replaces it with a simple daily/monthly/yearly period. FWR allows
locking to be optional. FWR supports

L<Tie::Handle::FileRotate>, which uses this module.

=cut
