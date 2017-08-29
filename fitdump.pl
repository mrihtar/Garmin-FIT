#! /usr/bin/perl -s

use FindBin;
use lib $FindBin::RealBin;

use Garmin::FIT;

$use_gmtime = 0 if !defined $use_gmtime;
$numeric_date_time = 0 if !defined $numeric_date_time;
$semicircles_to_deg = 1 if !defined $semicircles_to_deg;
$mps_to_kph = 1 if !defined $mps_to_kph;
$show_version = 0 if !defined $show_version;
$maybe_chained = 0 if !defined $maybe_chained;
$print_json = 0 if !defined $print_json;
if ($print_json) {
  $without_unit = 1 if !defined $without_unit;
  $skip_invalid = 1 if !defined $skip_invalid;
} else {
  $without_unit = 0 if !defined $without_unit;
  $skip_invalid = 0 if !defined $skip_invalid;
}

my $version = "0.08";
my $outr = 0;

if ($show_version) {
  printf STDERR "fitdump $version  Copyright (c) 2017 Kiyokazu Suto, Matjaz Rihtar  (Jan 26, 2017)\n";
  printf STDERR "Garmin::FIT  Copyright (c) 2010-2016 Kiyokazu Suto\n";
  printf STDERR "FIT protocol ver: %s, profile ver: %s\n",
    Garmin::FIT->protocol_version_string, Garmin::FIT->profile_version_string;
  exit;
}

sub dump_it {
  my ($self, $desc, $v, $o_cbmap) = @_;

  if ($desc->{message_name} ne '') {
    my $o_cb = $o_cbmap->{$desc->{message_name}};

    ref $o_cb eq 'ARRAY' and ref $o_cb->[0] eq 'CODE' and $o_cb->[0]->($self, $desc, $v, @$o_cb[1 .. $#$o_cb]);
  }

  if ($print_json) {
    print ",\n" if $outr;
    if ($desc->{message_name} ne '') {
      print "    {\"$desc->{message_name}\": {\n";
    } else {
      print "    {\"unknown$desc->{message_number}\": {\n";
    }
    $self->print_all_json($desc, $v, indent => '      ', skip_invalid => $skip_invalid);
    print "\n    }}";
    $outr = $outr + 1;
  }
  else {
    if ($desc->{message_name} ne '') {
      print "$desc->{message_name}";
    } else {
      print "unknown";
    }
    print " (", $desc->{message_number}, ", type: ", $desc->{local_message_type}, ", length: ", $desc->{message_length}, " bytes):\n";
    $self->print_all_fields($desc, $v, indent => '  ', skip_invalid => $skip_invalid);
  }
}

sub fetch_from {
  my $fn = shift;
  my $obj = new Garmin::FIT;

  $obj->use_gmtime($use_gmtime);
  $obj->numeric_date_time($numeric_date_time);
  $obj->semicircles_to_degree($semicircles_to_deg);
  $obj->mps_to_kph($mps_to_kph);
  $obj->without_unit($without_unit);
  $obj->maybe_chained($maybe_chained);
  $obj->file($fn);

  my $o_cbmap = $obj->data_message_callback_by_name('');
  my $msgname;

  foreach $msgname (keys %$o_cbmap) {
    $obj->data_message_callback_by_name($msgname, \&dump_it, $o_cbmap);
  }

  $obj->data_message_callback_by_name('', \&dump_it, $o_cbmap);

  unless ($obj->open) {
    print STDERR $obj->error, "\n";
    return;
  }

  my $chained;
  my $outf = 0;

  print "{\n" if $print_json;

  for (;;) {
    my ($fsize, $proto_ver, $prof_ver, $h_extra, $h_crc_expected, $h_crc_calculated) = $obj->fetch_header;

    unless (defined $fsize) {
      $obj->EOF and $chained and last;
      print STDERR $obj->error, "\n";
      $obj->close;
      return;
    }

    my ($proto_major, $proto_minor) = $obj->protocol_version_major($proto_ver);
    my ($prof_major, $prof_minor) = $obj->profile_version_major($prof_ver);

    if ($print_json) {
      if ($chained) {
        print ", {\n";
      } else {
        print "\"file\": [{\n";
      }
      printf "  \"crc\": \"0x%04X\",\n", $obj->crc;
      print "  \"header\": {\n";
      if (defined $h_crc_calculated) {
        printf "    \"crc\": \"0x%04X\",\n", $h_crc_calculated;
      }
      print "    \"file_size\": $fsize,\n";
      printf "    \"protocol_version\": \"%u.%02u\",\n", $proto_major, $proto_minor;
      printf "    \"profile_version\": \"%u.%02u\"\n", $prof_major, $prof_minor;
      print "  },\n";
      print "  \"records\": [\n";
    }
    else {
      print "\n" if $chained;
      printf "File size: %lu bytes, protocol ver: %u.%02u, profile ver: %u.%02u\n", $fsize, $proto_major, $proto_minor, $prof_major, $prof_minor;

      if ($h_extra ne '') {
        print "Extra bytes in file header";

        my ($i, $n);

        for ($i = 0, $n = length($h_extra) ; $i < $n ; ++$i) {
          print "\n" if !($i % 16);
          print ' ' if !($i % 4);
          printf " %02x", ord(substr($h_extra, $i, 1));
        }

        print "\n";
      }

      if (defined $h_crc_calculated) {
        if ($h_crc_expected != $h_crc_calculated) {
          printf "File header CRC: expected=0x%04X, calculated=0x%04X\n", $h_crc_expected, $h_crc_calculated;
        } else {
          printf "File header CRC: 0x%04X\n", $h_crc_calculated;
        }
      }
    }

    1 while $obj->fetch;

    if ($print_json) {
      print "\n  ]"; # records
      print "\n}"; # file
      $outf = $outf + 1;
    }
    else {
      if ($obj->crc_expected != $obj->crc) {
        printf "CRC: expected=0x%04X, calculated=0x%04X\n", $obj->crc_expected, $obj->crc;
      } else {
        printf "CRC: 0x%04X\n", $obj->crc_expected;
      }
    }

    if ($maybe_chained) {
      $obj->reset;
      $chained = 1;
    }
    else {
      print STDERR $obj->error, "\n" if !$obj->end_of_chunk && !$obj->EOF;
      my $garbage_size = $obj->trailing_garbages;

      print STDERR "Trailing $garbage_size bytes of garbage skipped\n" if $garbage_size > 0;
      last;
    }
  }

  print "]" if $outf;
  print "\n}\n" if $print_json;

  $obj->close;
}

my $old_fh = select(STDOUT);
$| = 1; # turn on autoflush for STDOUT
select($old_fh);

if (@ARGV > 1) {
  do {
    print "***** $ARGV[0] *****\n" if !$print_json;
    &fetch_from(shift @ARGV);
    $outr = 0;
  } while (@ARGV);
}
elsif (@ARGV) {
  &fetch_from($ARGV[0]);
}
else {
  &fetch_from('-');
}

1;
__END__

=head1 NAME

Fitdump - Show contents of Garmin .FIT files

=head1 SYNOPSIS

  fitdump -show_version=1
  fitdump [-semicircles_to_deg=(0|1)] [-mps_to_kph=(0|1)] [-use_gmtime=(0|1)] [-maybe_chained=(0|1)] [<file> ... ]

=head1 DESCRIPTION

B<Fitdump> reads the contents of Garmin .FIT files given on command line
(or standard input if no file is specified),
and print them in (hopefully) human readable form.

=for html The latest version is obtained via

=for html <blockquote>

=for html <!--#include virtual="/cgi-perl/showfile?/cycling/pub/fitdump-[0-9]*.tar.gz"-->.

=for html </blockquote>

It uses a Perl class

=for html <blockquote><a href="GarminFIT.shtml">

C<Garmin::FIT>.

=for html </a></blockquote>

of version 0.23 or later.
The main role of this program is to give a sample application of the class.

=head1 AUTHOR

Kiyokazu SUTO E<lt>suto@ks-and-ks.ne.jpE<gt>

=head1 DISCLAIMER etc.

This program is distributed with
ABSOLUTELY NO WARRANTY.

Anyone can use, modify, and re-distibute this program
without any restriction.

=head1 CHANGES

=head2 0.05 --E<gt> 0.06

=over 4

=item C<$maybe_chained>

new option.
The true value indicates that the input may be a chained FIT file.
Default is 0.

=item C<fetch_from()>

use new method C<reset()>, C<maybe_chained()>, and C<end_of_chunk()> to support chained FIT files.

=back

=head2 0.04 --E<gt> 0.05

=over 4

=item C<dump_it()>

=item C<fetch_from()>

care the case where there are previously defined callback functions.

=back

=head2 0.03 --E<gt> 0.04

=over 4

=item C<fetch_from()>

follows protocol version 1.2.

=back

=head2 0.02 --E<gt> 0.03

=over 4

=item C<fetch_from()>

should call the method C<close()> when C<fetch_header()> failed.

=back

=head2 0.01 --E<gt> 0.02

=over 4

=item C<init_it()>

removed.

=item C<dump_it()>

rewrite with new method C<print_all_fields> of C<Garmin::FIT>.

=item C<fetch_from()>

renamed from C<fetch_all()>.

rewrite with new methods of C<Garmin::FIT>.

=back

=cut
