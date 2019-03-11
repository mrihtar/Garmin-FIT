#!/usr/bin/perl
use strict;
use warnings;
use FindBin;
use lib $FindBin::RealBin;

use Garmin::FIT;
use POSIX;
use POSIX::strftime::GNU;
use File::Basename;
use List::Util qw(sum);
use Config::Simple ('-lc'); # ignore case
#use GD::Graph::Data;
#use GD::Graph::lines;

#------------------------------------------------------------------------------
# Global variables definition
(my $prog = basename($0)) =~ s/.pl$//i;

my $indent = " " x 4; # default indent: 4 spaces
my $timeoffs = 631065600; # sec since Garmin Epoch (1989-12-31 00:00:00 GMT)

# Min/max/average values
my $g_minAlt = 10000;
my $g_maxAlt = -10000;
my $alts = [];
my $g_minHr = 10000;
my $g_maxHr = -10000; # already in session/lap data
my $hrs = []; # already in session/lap data
my $g_minTemp = 10000;
my $g_maxTemp = -10000;
my $temps = [];
my $speedDownhills = [];
my $speedUphills = [];
my $g_minIncl = 10000;
my $g_maxIncl = -10000;
my $incls = [];
my $inclDownhills = [];
my $inclUphills = [];
my $g_minPower = 10000;
my $g_maxPower = -10000;
my $powers = [];
my $g_minRiseRate = 10000;
my $g_maxRiseRate = -10000;
my $riseRates = [];
my $riseRateDownhills = [];
my $riseRateUphills = [];

# General info (from file_id)
my $g_manuf; # garmin, suunto, magellan, holux, sigmasport, ...
my $g_product; # fr___, edge___, hrm___, vivo___, virb___, ...
my $g_serial;
# SigmaSport: cycling, mountainbike, racing_bycicle (road bike), running, ...
my $g_sport; # generic, running, cycling, training, walking, ...
my $g_subSport; # generic, road, mountain, downhill, ...
my $g_trainType; # SigmaSport specific (manual entry)

# Personal info (from user_profile)
my $g_name; my $g_gender; my $g_age; my $g_height; my $g_weight;
my $g_resHr; my $g_hrMax;
my $g_bmr; my $g_bmrs; # basal metabolic rate
my $g_bike;
my $g_bikeName; my $g_bikeType; my $g_bikeWeight; my $g_wheelSize;
my $g_homeAlt;

my $g_trackName; my $g_description;

# Functional threshold power (from zones_target)
my $g_funcThresPower;
my $g_hrMax1; # another one???

# Intensity zones starting percent of maximum heart rate
my $pc_iZone0s = 0.50; # recovery  (aerobic)
my $pc_iZone1s = 0.60; # endurance (aerobic)
my $pc_iZone2s = 0.70; # stamina   (aerobic)
my $pc_iZone3s = 0.80; # economy   (anaerobic)
my $pc_iZone4s = 0.90; # speed     (anaerobic)

my $g_iZone0s; my $g_iZone1s; my $g_iZone2s;
my $g_iZone3s; my $g_iZone4s; my $g_iZone4e;

# Target zone (endurance (aerobic), can be overriden)
my $g_targetZoneStart; # 60% of hrMax
my $g_targetZoneEnd;   # 70% of hrMax

# Power zones starting percent of functional threshold power
my $pc_pZone1s = 0;    # active recovery
my $pc_pZone2s = 0.56; # endurance
my $pc_pZone3s = 0.76; # tempo
my $pc_pZone4s = 0.91; # lactate threshold
my $pc_pZone5s = 1.06; # VO2 max
my $pc_pZone6s = 1.21; # anaerobic capacity
my $pc_pZone7s = 1.50; # neuromuscular power
my $g_pZoneMax = 2000; # W, maximum power

my $g_pZone1s; my $g_pZone2s; my $g_pZone3s;
my $g_pZone4s; my $g_pZone5s; my $g_pZone6s;
my $g_pZone7s; my $g_pZone7e;

# Power calculation
# Crr = coef of rolling resistance ???
# 0 = Clinchers, 1 = Tubulars, 2 = MTB
my @Crr = (0.005, 0.004, 0.012);
# CdA = coef of drag * frontal area ???
# 0 = Hoods, 1 = Bartops, 2 = Bar ends, 3 = Drops, 4 = Aerobar, 5 = ???
my @CdA = (0.388, 0.445, 0.420, 0.300, 0.233, 0.200);

my $g_crr = $Crr[2]; # MTB
my $g_cda = $CdA[0]; # Hoods
my $g_pcf = 0.65;
my $g_kcf = 0.65;

# Totals/summary info (from lap/session)
my $g_startTime;
my $f_startTime = ""; # start time in proper format
my $g_startLon; my $g_startLat;
# endLon and endLat are not present in session!
my $g_endLon; my $g_endLat;
my $g_totElapsTime; my $g_totTimerTime;
my $g_totTimeStand;
my $g_totDistance; my $g_totCycles;
my $g_totCal;
my $g_timeHrZone; # array
my $g_avgSpeed; my $g_maxSpeed;
my $g_totAscent; my $g_totDescent;
my $g_avgHr; # my $g_maxHr;
my $g_avgCad; my $g_maxCad;
my $g_avgPower; # my $g_maxPower;

# Calculated totals/summary info
my $g_distDownhill; my $g_distUphill;
my $g_timeDownhill; my $g_timeUphill;
my $g_pauseTime;
my $g_timeInTargetZone;
my $g_timeOverTargetZone; my $g_timeUnderTargetZone;
my $g_timeInIntZone1; my $g_timeInIntZone2;
my $g_timeInIntZone3; my $g_timeInIntZone4;
my $g_timeOverIntZone; my $g_timeUnderIntZone;

# Previous values of each track point
my $prev_timestamp;
my $prev_lat; my $prev_lon;
# my $prev_dist; my $prev_alt;
my $prev_speed; my $prev_hr; my $prev_cad; my $prev_temp;
my $tot_time;
my $tot_records;

# Previous values of dist/alt for dist/alt filtering (smoothing)
my $histSize = 6;
my @prev_time;
my @prev_dist; my @prev_distDiff;
my @prev_alt; my @prev_altDiff;

my %alt_state = ( q => 0, r => 0, p => 0, x => 0, k => 0, );
my $orig_alt = [];
my $filt_alt = [];
#my %pwr_state = ( q => 0, r => 0, p => 0, x => 0, k => 0, );

#------------------------------------------------------------------------------
# Command line parsing
my $overwrite = 0;
my $fpcalc = 0; # force power calculation
my $zeroll = 0; # allow zero values for latitude/longitude if not present
my $garmin_ext = 1; # 0 means cluetrust_ext
my $rev_coord = 0;  # default: lat=... lon=...
my $file;
foreach (@ARGV) {
  my $arg = $_;
  if ($arg eq "-v") { Usage(1); }
  elsif ($arg eq "-h" || $arg eq "-?") { Usage(0); }
  elsif ($arg eq "-y") { $overwrite = 1; }
  elsif ($arg eq "-p") { $fpcalc = 1; }
  elsif ($arg eq "-z") { $zeroll = 1; }
  elsif ($arg eq "-g") { $garmin_ext = 1; }
  elsif ($arg eq "-c") { $garmin_ext = 0; }
  elsif ($arg eq "-r") { $rev_coord = 1; }
  else { $file = $arg; }
}

Usage(0) if !defined $file;
(my $gpx_file = $file) =~ s/.fit$/.gpx/i;

die "File $gpx_file already exists\n" if -f $gpx_file && !$overwrite;

#------------------------------------------------------------------------------
# Fit file reading
# Array of HASHes where fit data is stored
my $file_ids = [];
my $profiles = [];
my $sensors = [];
my $zones = [];
my $records = [];
my $laps = [];
my $sessions = [];

my $fit = Garmin::FIT->new();
ReadFitFile($file);

#------------------------------------------------------------------------------
# Fill global vars with values from fit file
FillGlobalVars();

# Override values from fit file with values from ini file
ReadIniFile($prog.".ini");

#------------------------------------------------------------------------------
# Convert and display fit file content in slf format
PrintGpxData($gpx_file);

exit(0);
# main

#==============================================================================
# Print version/usage and exit
sub Usage {
  my $ver_only = shift;

  if ($ver_only) {
    printf STDERR "fit2gpx 2.12  Copyright (c) 2016-2019 Matjaz Rihtar  (Mar 11, 2019)\n";
    printf STDERR "Garmin::FIT  Copyright (c) 2010-2017 Kiyokazu Suto\n";
    printf STDERR "FIT protocol ver: %s, profile ver: %s\n",
      Garmin::FIT->protocol_version_string, Garmin::FIT->profile_version_string;
  }
  else {
    printf STDERR "Usage: $prog [-v|-h] [-y] [-p] [-z] [-g|-c] [-r] <fit-file>\n";
    printf STDERR "  -v  Print version and exit\n";
    printf STDERR "  -h  Print short help and exit\n";
    printf STDERR "  -y  Overwrite <slf-file> if it exists (default: don't overwrite)\n";
    printf STDERR "  -p  Force power calculation (default: use fit power data if present)\n";
    printf STDERR "  -z  Allow zero values for latitude/longitude if not present\n";
    printf STDERR "      (default: ignore records with no latitude/longitude)\n";
    printf STDERR "  -g  Use Garmin extension format for hr/cad/temp (default)\n";
    printf STDERR "      <gpxtpx:TrackPointExtension>, <gpxtpx:...>: atemp, hr, cad\n";
    printf STDERR "  -c  Use Cluetrust extension format for hr/cad/temp\n";
    printf STDERR "      <gpxdata:...>: temp, hr, cadence\n";
    printf STDERR "  -r  Reverse print of lat and lon in trkpt entries\n";
    printf STDERR "      (default: <trkpt lat=... lon=...>)\n";
  }
  _exit(1);
} # Usage

#==============================================================================
# Read fit file, parse messages and store interesting data for later processing
sub ReadFitFile {
  my $fname = shift;

  # Setup some defaults
  $fit->use_gmtime(1);
  $fit->numeric_date_time(1);
  $fit->semicircles_to_degree(1);
  $fit->mps_to_kph(1);
  $fit->without_unit(1);
  $fit->maybe_chained(0);
  $fit->drop_developer_data(1);
  $fit->file($fname);

  $fit->data_message_callback_by_name('',
    sub {
      my ($msg, $name) = Message(@_);
      if ($msg) {
        if ($name eq "file_id") { push @$file_ids, $msg; }
        elsif ($name eq "user_profile") { push @$profiles, $msg; }
        elsif ($name eq "sensor") { push @$sensors, $msg; }
        elsif ($name eq "zones_target") { push @$zones, $msg; }
        elsif ($name eq "record") { push @$records, $msg; }
        elsif ($name eq "lap") { push @$laps, $msg; }
        elsif ($name eq "session") { push @$sessions, $msg; }
      }
      return 1;
    }
  );

  # Open and read fit file
  $fit->open() or die $fit->error()."\n";
  my ($f_size) = $fit->fetch_header();
  if (!defined $f_size) {
    if (!defined $fit->error()) { $fit->error("can't read FIT header"); }
    die $fit->error()."\n";
  }
  1 while $fit->fetch();
} # ReadFitFile

#==============================================================================
# Parse messages from fit file (see Garmin::FIT)
sub Message {
  my ($fit, $desc, $v) = @_;

  my $mname = $desc->{message_name};
  return undef if !$mname;

  my $m = {};

  foreach my $i_name (keys %$desc) {
    next if $i_name !~ /^i_/;
    my $name = $i_name;

    $name =~ s/^i_//;

    my $attr = $desc->{'a_' . $name};
    my $tname = $desc->{'t_' . $name};
    my $pname = $name;

    if (ref $attr->{switch} eq 'HASH') {
      my $t_attr = $fit->switched($desc, $v, $attr->{switch});

      if (ref $t_attr eq 'HASH') {
        $attr = $t_attr;
        $tname = $attr->{type_name};
        $pname = $attr->{name};
      }
    }

    my $i = $desc->{$i_name};
    my $c = $desc->{'c_' . $name};
    my $type = $desc->{'T_' . $name};
    my $invalid = $desc->{'I_' . $name};
    my $j;

    for ($j = 0 ; $j < $c ; $j++) {
      Garmin::FIT->isnan($v->[$i + $j]) && next;
      $v->[$i + $j] != $invalid && last;
    }
    if ($j < $c) { # skip invalid
      if ($type == FIT_STRING) {
        $m->{$pname} = $fit->string_value($v, $i, $c);
      }
      else {
        # return only the first value if array
        $m->{$pname} = $fit->value_cooked($tname, $attr, $invalid, $v->[$i]);
      }
    }
  }

  return ($m, $mname);
} # Message

#==============================================================================
# Go through fit data and fill global vars with general data
# For calculating some missing aver/min/max values, routine for processing
# all track points is called at the end with some reasonable defaults.
sub FillGlobalVars {
  my $m; my %mh;
  my $k; my $v;

  # Find manufacturer, product name and serial in file_id
  $m = @{$file_ids}[0];
  if (defined $m) {
    %mh = %$m;
    while (($k, $v) = each %mh) {
      if ($k eq "manufacturer") { $g_manuf = ucfirst $v; }
      elsif ($k eq "garmin_product") { $g_product = $v; }
      elsif ($k eq "product") { $g_product = $v; } # for non Garmin products
      elsif ($k eq "serial_number") { $g_serial = $v; }
    }
  }

  # Fill in default values if no data found
  $g_manuf = "Generic" if !defined $g_manuf;
  $g_product = "unknown" if !defined $g_product;

  # Find profile data in first user_profile
  $m = @{$profiles}[0];
  if (defined $m) {
    %mh = %$m;
    while (($k, $v) = each %mh) {
      if ($k eq "friendly_name") { my $friendlyName = $v; }
      elsif ($k eq "weight") { $g_weight = $v; } # kg
      elsif ($k eq "gender") { $g_gender = $v; }
      elsif ($k eq "age") { $g_age = $v; } # years
      elsif ($k eq "height") { $g_height = $v; } # m
      elsif ($k eq "resting_heart_rate") { $g_resHr = $v; }
      elsif ($k eq "default_max_heart_rate") { $g_hrMax = $v; }
    }
  }

  my $undef_gender = 0;
  if (!defined $g_gender) { $g_gender = "male"; $undef_gender = 1; }
  my $undef_age = 0;
  if (!defined $g_age) { $g_age = 40; $undef_age = 1; }
  my $undef_weight = 0;
  if (!defined $g_weight) { $g_weight = 75.0; $undef_weight = 1; }
  my $undef_resHr = 0;
  if (!defined $g_resHr) { $g_resHr = 72; $undef_resHr = 1; }

  my $undef_hrMax = 0;
  if (!defined $g_hrMax) {
    # Usual formula: 220 - age
    # Tanaka(2001):  208 - 0.7*age
    # Gellish(2007): 206.9 - 0.67*age (207 - 0.7*age)
    # HUNT(2012):    211 - 0.64*age (men: 213 - 0.65*age, women: 210 - 0.62*age)
    if ($g_gender eq "male") { $g_hrMax = int(213 - 0.65*$g_age); }
    else { $g_hrMax = int(210 - 0.62*$g_age); }
    $undef_hrMax = 1;
  }

  # Intensity zones
  $g_iZone0s = int($g_hrMax * $pc_iZone0s); # recovery  (aerobic)
  $g_iZone1s = int($g_hrMax * $pc_iZone1s); # endurance (aerobic)
  $g_iZone2s = int($g_hrMax * $pc_iZone2s); # stamina   (aerobic)
  $g_iZone3s = int($g_hrMax * $pc_iZone3s); # economy   (anaerobic)
  $g_iZone4s = int($g_hrMax * $pc_iZone4s); # speed     (anaerobic)
  $g_iZone4e = $g_hrMax;

  # Target zone
  $g_targetZoneStart = int($g_hrMax * $pc_iZone1s); # endurance (aerobic)
  $g_targetZoneEnd = int($g_hrMax * $pc_iZone2s);

  # Find wheel size in sensors
  foreach (@$sensors) {
    while (($k, $v) = each %$_) {
      if ($k eq "wheel_size") { $g_wheelSize = $v; }
    }
  }

  my $undef_wheelSize = 0;
  if (!defined $g_wheelSize) { $g_wheelSize = 2326; $undef_wheelSize = 1; }

  # find zones data in first zones_target
  $m = @{$zones}[0];
  if (defined $m) {
    %mh = %$m;
    while (($k, $v) = each %mh) {
      if ($k eq "functional_threshold_power") { $g_funcThresPower = $v; }
      elsif ($k eq "max_heart_rate") { $g_hrMax1 = $v; } # ???
    }
  }

  my $undef_funcThresPower = 0;
  if (!defined $g_funcThresPower) {
    $g_funcThresPower = 253; $undef_funcThresPower = 1;
  }

  # Power zones
  $g_pZone1s = $g_funcThresPower * $pc_pZone1s;
  $g_pZone2s = $g_funcThresPower * $pc_pZone2s;
  $g_pZone3s = $g_funcThresPower * $pc_pZone3s;
  $g_pZone4s = $g_funcThresPower * $pc_pZone4s;
  $g_pZone5s = $g_funcThresPower * $pc_pZone5s;
  $g_pZone6s = $g_funcThresPower * $pc_pZone6s;
  $g_pZone7s = $g_funcThresPower * $pc_pZone7s;
  $g_pZone7e = $g_pZoneMax;

  # Find all needed general info in first session
  $m = @{$sessions}[0];
  if (defined $m) {
    %mh = %$m;
    while (($k, $v) = each %mh) {
      if ($k eq "start_time") { $g_startTime = $v; } # + $timeoffs;
      elsif ($k eq "sport") { $g_sport = $v; }
      elsif ($k eq "sub_sport") { $g_subSport = $v; }
      elsif ($k eq "start_position_lat") { $g_startLat = $v; }
      elsif ($k eq "start_position_long") { $g_startLon = $v; }
      elsif ($k eq "total_elapsed_time") { $g_totElapsTime = $v; }
      elsif ($k eq "total_timer_time") { $g_totTimerTime = $v; }
      elsif ($k eq "time_standing") { $g_totTimeStand = $v; } # invalid
      elsif ($k eq "total_distance") { $g_totDistance = $v; } # m
      elsif ($k eq "total_cycles") { $g_totCycles = $v; }
      elsif ($k eq "total_calories") { $g_totCal = $v; }
      elsif ($k eq "time_in_hr_zone") { $g_timeHrZone = $v; } # array
      elsif ($k eq "avg_speed") { $g_avgSpeed = $v; }
      elsif ($k eq "max_speed") { $g_maxSpeed = $v; }
      elsif ($k eq "total_ascent") { $g_totAscent = $v; }
      elsif ($k eq "total_descent") { $g_totDescent = $v; }
      elsif ($k eq "avg_heart_rate") { $g_avgHr = $v; }
      elsif ($k eq "max_heart_rate") { $g_maxHr = $v; }
      elsif ($k eq "avg_cadence") { $g_avgCad = $v; }
      elsif ($k eq "max_cadence") { $g_maxCad = $v; }
      elsif ($k eq "avg_power") { $g_avgPower = $v; } # invalid
      elsif ($k eq "max_power") { $g_maxPower = $v; } # invalid
    }
  }

  if (scalar @$laps <= 1) {
    # Find additional/missing general info in first (and only) lap
    $m = @{$laps}[0];
    if (defined $m) {
      %mh = %$m;
      while (($k, $v) = each %mh) {
        if ($k eq "start_time" && !defined $g_startTime)
          { $g_startTime = $v; } # + $timeoffs;
        elsif ($k eq "sport" && !defined $g_sport)
          { $g_sport = $v; }
        elsif ($k eq "sub_sport" && !defined $g_subSport)
          { $g_subSport = $v; }
        elsif ($k eq "start_position_lat" && !defined $g_startLat)
          { $g_startLat = $v; }
        elsif ($k eq "start_position_long" && !defined $g_startLon)
          { $g_startLon = $v; }
        elsif ($k eq "end_position_lat" && !defined $g_endLat)
          { $g_endLat = $v; }
        elsif ($k eq "end_position_long" && !defined $g_endLon)
          { $g_endLon = $v; }
        elsif ($k eq "total_elapsed_time" && !defined $g_totElapsTime)
          { $g_totElapsTime = $v; }
        elsif ($k eq "total_timer_time" && !defined $g_totTimerTime)
          { $g_totTimerTime = $v; }
        elsif ($k eq "time_standing" && !defined $g_totTimeStand)
          { $g_totTimeStand = $v; } # invalid
        elsif ($k eq "total_distance" && !defined $g_totDistance)
          { $g_totDistance = $v; } # m
        elsif ($k eq "total_cycles" && !defined $g_totCycles)
          { $g_totCycles = $v; }
        elsif ($k eq "total_calories" && !defined $g_totCal)
          { $g_totCal = $v; }
        elsif ($k eq "time_in_hr_zone" && !defined $g_timeHrZone)
          { $g_timeHrZone = $v; } # array
        elsif ($k eq "avg_speed" && !defined $g_avgSpeed)
          { $g_avgSpeed = $v; }
        elsif ($k eq "max_speed" && !defined $g_maxSpeed)
          { $g_maxSpeed = $v; }
        elsif ($k eq "total_ascent" && !defined $g_totAscent)
          { $g_totAscent = $v; }
        elsif ($k eq "total_descent" && !defined $g_totDescent)
          { $g_totDescent = $v; }
        elsif ($k eq "avg_heart_rate" && !defined $g_avgHr)
          { $g_avgHr = $v; }
        elsif ($k eq "max_heart_rate" && !defined $g_maxHr)
          { $g_maxHr = $v; }
        elsif ($k eq "avg_cadence" && !defined $g_avgCad)
          { $g_avgCad = $v; }
        elsif ($k eq "max_cadence" && !defined $g_maxCad)
          { $g_maxCad = $v; }
        elsif ($k eq "avg_power" && !defined $g_avgPower)
          { $g_avgPower = $v; } # invalid
        elsif ($k eq "max_power" && !defined $g_maxPower)
          { $g_maxPower = $v; } # invalid
      }
    }
  }

  # Fill in default values if no data found
  $g_sport = "generic" if !defined $g_sport;
  $g_subSport = "generic" if !defined $g_subSport;
  $g_totCal = 0 if !defined $g_totCal;
  $g_avgHr = 0 if !defined $g_avgHr;

  $g_startTime = time() if !defined $g_startTime;
  my @lt = localtime($g_startTime);
  $f_startTime = POSIX::strftime("%d-%b-%y %H:%M", @lt);
  $g_totElapsTime = 0 if !defined $g_totElapsTime;

  $g_trackName = "Track ".$f_startTime;
  my $product = ucfirst $g_product;
  $g_description = ucfirst "$g_sport ($g_subSport) recorded on $g_manuf $product";

  # Some other defaults needed for the 1st call (overriden later)
  my $undef_homeAlt;
  if (!defined $g_homeAlt) { $g_homeAlt = 315; $undef_homeAlt = 1; }
  my $undef_bikeWeight;
  if (!defined $g_bikeWeight) { $g_bikeWeight = 12.3; $undef_bikeWeight = 1; }

  # Calc missing aver/min/max alt, power, hr, cad and temp from all records
  ProcessRecords();

  if ($undef_gender) { $g_gender = undef; }
  if ($undef_age) { $g_age = undef; }
  if ($undef_weight) { $g_weight = undef; }
  if ($undef_resHr) { $g_resHr = undef; }
  if ($undef_hrMax) { $g_hrMax = undef; }
  if ($undef_wheelSize) { $g_wheelSize = undef; }
  if ($undef_funcThresPower) { $g_funcThresPower = undef; }
  if ($undef_homeAlt) { $g_homeAlt = undef; }
  if ($undef_bikeWeight) { $g_bikeWeight = undef; }
} # FillGlobalVars

#==============================================================================
# Read ini file
# File name is the same as this script's name with extension .ini.
# Format is the same as normal Windows .ini file.
# If file is not found, reasonable defaults are applied to undefined variables.
sub ReadIniFile {
  my $fname = shift;
  my $val;

  my $ini = new Config::Simple(syntax=>"ini");
# $ini->read($fname) or die $ini->error()."\n";
  $ini->read($fname); # ignore error if no ini file

  $val = $ini->param("sport");
# print STDERR "INI: sport = |$val|\n" if defined $val;
  if (defined $val) { $g_sport = $val; }
  elsif (!defined $g_sport) { $g_sport = "mountainbike"; }

  $val = $ini->param("training_type");
# print STDERR "INI: training_type = |$val|\n" if defined $val;
  if (defined $val) { $g_trainType = $val; }
  elsif (!defined $g_trainType) { $g_trainType = "Riding"; }

  $val = $ini->param("home_altitude");
# print STDERR "INI: home_altitude = |$val|\n" if defined $val;
  if (defined $val) { $g_homeAlt = $val; }
  elsif (!defined $g_homeAlt) { $g_homeAlt = 315; }

  $val = $ini->param("track_name");
# print STDERR "INI: track_name = |$val|\n" if defined $val;
  if (defined $val) { $g_trackName = $val; }
  elsif (!defined $g_trackName) { $g_trackName = ""; }

  $val = $ini->param("description");
# print STDERR "INI: track_name = |$val|\n" if defined $val;
  if (defined $val) { $g_description = $val; }
  elsif (!defined $g_description) { $g_description = ""; }

  $val = $ini->param("personal.name");
# print STDERR "INI: personal.name = |$val|\n" if defined $val;
  if (defined $val) { $g_name = $val; }
  elsif (!defined $g_name) { $g_name = "John Smith"; }

  $val = $ini->param("personal.gender");
# print STDERR "INI: personal.gender = |$val|\n" if defined $val;
  if (defined $val) { $g_gender = $val; }
  elsif (!defined $g_gender) { $g_gender = "male"; }

  my $recalc_hrMax = 0;
  $val = $ini->param("personal.age");
# print STDERR "INI: personal.age = |$val|\n" if defined $val;
  if (defined $val) { $g_age = $val; $recalc_hrMax = 1; }
  elsif (!defined $g_age) { $g_age = 40; $recalc_hrMax = 1; }

  $val = $ini->param("personal.height");
# print STDERR "INI: personal.height = |$val|\n" if defined $val;
  if (defined $val) { $g_height = $val; }
  elsif (!defined $g_height) { $g_height = 183; }

  $val = $ini->param("personal.weight");
# print STDERR "INI: personal.weight = |$val|\n" if defined $val;
  if (defined $val) { $g_weight = $val; }
  elsif (!defined $g_weight) { $g_weight = 75.0; }

  $val = $ini->param("personal.resting_heart_rate");
# print STDERR "INI: personal.resting_heart_rate = |$val|\n" if defined $val;
  if (defined $val) { $g_resHr = $val; }
  elsif (!defined $g_resHr) { $g_resHr = 72; }

  my $recalc_targetZone = 0;
  $val = $ini->param("personal.max_heart_rate");
# print STDERR "INI: personal.max_heart_rate = |$val|\n" if defined $val;
  if (defined $val) { $g_hrMax = $val; $recalc_targetZone = 1; }
  elsif (!defined $g_hrMax || $recalc_hrMax) {
    # Usual formula: 220 - age
    # Tanaka(2001):  208 - 0.7*age
    # Gellish(2007): 206.9 - 0.67*age (207 - 0.7*age)
    # HUNT(2012):    211 - 0.64*age (men: 213 - 0.65*age, women: 210 - 0.62*age)
    if ($g_gender eq "male") { $g_hrMax = int(213 - 0.65*$g_age); }
    else { $g_hrMax = int(210 - 0.62*$g_age); }
    $recalc_targetZone = 1;
  }

  $val = $ini->param("personal.target_zone_start");
# print STDERR "INI: personal.target_zone_start = |$val|\n" if defined $val;
  if (defined $val) { $g_targetZoneStart = $val; }
  elsif (!defined $g_targetZoneStart || $recalc_targetZone) {
    $g_targetZoneStart = int($g_hrMax * $pc_iZone1s);
  }

  $val = $ini->param("personal.target_zone_end");
# print STDERR "INI: personal.target_zone_end = |$val|\n" if defined $val;
  if (defined $val) { $g_targetZoneEnd = $val; }
  elsif (!defined $g_targetZoneEnd || $recalc_targetZone) {
    $g_targetZoneEnd = int($g_hrMax * $pc_iZone2s);
  }

  $val = $ini->param("personal.ftp");
# print STDERR "INI: personal.ftp = |$val|\n" if defined $val;
  if (defined $val) { $g_funcThresPower = $val; }
  elsif (!defined $g_funcThresPower) { $g_funcThresPower = 253; }

  $g_bike = $ini->param("bike");
  $g_bike = "bike1" if !defined $g_bike;

  $val = $ini->param($g_bike.".name");
# print STDERR "INI: $g_bike.name = |$val|\n" if defined $val;
  if (defined $val) { $g_bikeName = $val; }
  elsif (!defined $g_bikeName) { $g_bikeName = "Ibis Ripley 29 LS"; }

  $val = $ini->param($g_bike.".type");
# print STDERR "INI: $g_bike.type = |$val|\n" if defined $val;
  if (defined $val) { $g_bikeType = $val; }
  elsif (!defined $g_bikeType) { $g_bikeType = "MTB"; }

  $val = $ini->param($g_bike.".weight");
# print STDERR "INI: $g_bike.weight = |$val|\n" if defined $val;
  if (defined $val) { $g_bikeWeight = $val; }
  elsif (!defined $g_bikeWeight) { $g_bikeWeight = 12.3; }

  $val = $ini->param($g_bike.".wheel_size");
# print STDERR "INI: $g_bike.wheel_size = |$val|\n" if defined $val;
  if (defined $val) { $g_wheelSize = $val; }
  elsif (!defined $g_wheelSize) { $g_wheelSize = 2326; }

  $val = $ini->param($g_bike.".crr");
# print STDERR "INI: $g_bike.crr = |$val|\n" if defined $val;
  if (defined $val) { $g_crr = $val; }
  elsif (!defined $g_crr) { $g_crr = $Crr[2]; } # MTB

  $val = $ini->param($g_bike.".cda");
# print STDERR "INI: $g_bike.cda = |$val|\n" if defined $val;
  if (defined $val) { $g_cda = $val; }
  elsif (!defined $g_cda) { $g_cda = $CdA[0]; } # Hoods

  $val = $ini->param($g_bike.".pcf");
# print STDERR "INI: $g_bike.pcf = |$val|\n" if defined $val;
  if (defined $val) { $g_pcf = $val; }
  elsif (!defined $g_pcf) { $g_pcf = 0.65; }

  $val = $ini->param($g_bike.".kcf");
# print STDERR "INI: $g_bike.kcf = |$val|\n" if defined $val;
  if (defined $val) { $g_kcf = $val; }
  elsif (!defined $g_kcf) { $g_kcf = 0.65; }

  # Mifflin-St.Jeor formula for BMR (1990)
  if ($g_gender eq "male")
    { $g_bmr = 9.99*$g_weight + 6.25*$g_height - 4.92*$g_age + 5; }
  else
    { $g_bmr = 9.99*$g_weight + 6.25*$g_height - 4.92*$g_age - 161; }
  $g_bmrs = $g_bmr / 86400; # bmr/s (bmr is for whole day)

  # Intensity zones
  $g_iZone0s = int($g_hrMax * $pc_iZone0s); # recovery  (aerobic)
  $g_iZone1s = int($g_hrMax * $pc_iZone1s); # endurance (aerobic)
  $g_iZone2s = int($g_hrMax * $pc_iZone2s); # stamina   (aerobic)
  $g_iZone3s = int($g_hrMax * $pc_iZone3s); # economy   (anaerobic)
  $g_iZone4s = int($g_hrMax * $pc_iZone4s); # speed     (anaerobic)
  $g_iZone4e = $g_hrMax;

  # Power zones
  $g_pZone1s = $g_funcThresPower * $pc_pZone1s;
  $g_pZone2s = $g_funcThresPower * $pc_pZone2s;
  $g_pZone3s = $g_funcThresPower * $pc_pZone3s;
  $g_pZone4s = $g_funcThresPower * $pc_pZone4s;
  $g_pZone5s = $g_funcThresPower * $pc_pZone5s;
  $g_pZone6s = $g_funcThresPower * $pc_pZone6s;
  $g_pZone7s = $g_funcThresPower * $pc_pZone7s;
  $g_pZone7e = $g_pZoneMax;
} # ReadIniFile

#==============================================================================
# Calculate average value of an array
sub Average {
  my $len = scalar @_;
  $len ? sum( @_ )/$len : 0;
} # Average

#==============================================================================
# Calculate weighted average of an array
# First parameter determines how many values from the array should be used
# Array's last entered value starts at the end of array!
sub Waverage {
  my $used = shift;
  my $len = scalar @_;
  if (!defined $used || $used > $len) { $used = $len; }
  my $w; my $wsum = 0; my $div = 0;
  for (my $ii = 0, my $jj = -1; $ii < $used; $ii++, $jj--) {
    $w = $len + 1 + $jj;
    $wsum += $_[$jj]*$w;
    $div += $w;
  }
  $div ? $wsum/$div : 0;
} # Waverage

#==============================================================================
# Calculate median value of an array
sub Median {
  my $len = scalar @_;
  $len ? sum( ( sort { $a <=> $b } @_ )[ int( $#_/2 ), ceil( $#_/2 ) ] )/2 : 0;
} # Median

#==============================================================================
# Print gpx block (1st line, header) in gpx
sub PrintGpxHeader {
# <gpx version="1.1 [1]" creator="xsd:string [1]">
#   <metadata> metadataType </metadata> [0..1]
#   <wpt> wptType </wpt> [0..*]
#   <rte> rteType </rte> [0..*]
#   <trk> trkType </trk> [0..*]
#   <extensions> extensionsType </extensions> [0..1]
# </gpx>

  printf "<gpx %s", 'version="1.1" creator="fit2gpx by Matjaz Rihtar"';
  my $loc = 'xsi:schemaLocation="';
  $loc = $loc . "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd";
  $loc = $loc . " http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www.garmin.com/xmlschemas/GpxExtensionsv3.xsd";
  $loc = $loc . " http://www.garmin.com/xmlschemas/TrackPointExtension/v1 http://www.garmin.com/xmlschemas/TrackPointExtensionv1.xsd";
  $loc = $loc . " http://www.garmin.com/xmlschemas/WaypointExtension/v1 http://www.garmin.com/xmlschemas/WaypointExtensionv1.xsd";
  $loc = $loc . " http://www.cluetrust.com/XML/GPXDATA/1/0 http://www.cluetrust.com/Schemas/gpxdata10.xsd";
  $loc = $loc . '"';
  printf " %s", $loc;
  printf " %s", 'xmlns="http://www.topografix.com/GPX/1/1"';
  printf " %s", 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';
  # garmin_ext
  printf " %s", 'xmlns:gpxx="http://www.garmin.com/xmlschemas/GpxExtensions/v3"';
  printf " %s", 'xmlns:gpxtrx="http://www.garmin.com/xmlschemas/GpxExtensions/v3"';
  printf " %s", 'xmlns:gpxtpx="http://www.garmin.com/xmlschemas/TrackPointExtension/v1"';
  printf " %s", 'xmlns:gpxwpx="http://www.garmin.com/xmlschemas/WaypointExtension/v1"';
  # cluetrust_ext
  printf " %s", 'xmlns:gpxdata="http://www.cluetrust.com/XML/GPXDATA/1/0"';
  # trekbuddy_ext
# printf " %s", 'xmlns:nmea="http://trekbuddy.net/2009/01/gpx/nmea"';
# nmea:course, nmea:speed
  printf ">\n";
} # PrintGpxHeader

#==============================================================================
# Print metadata block in gpx
sub PrintGpxMetadata {
# <metadata>
#   <name> xsd:string </name> [0..1]
#   <desc> xsd:string </desc> [0..1]
#   <author> personType </author> [0..1]
#   <copyright> copyrightType </copyright> [0..1]
#   <link> linkType </link> [0..*]
#   <time> xsd:dateTime </time> [0..1]
#   <keywords> xsd:string </keywords> [0..1]
#   <bounds> boundsType </bounds> [0..1]
#   <extensions> extensionsType </extensions> [0..1]
# </metadata>
#
# <author>
#   <name> xsd:string </name> [0..*]
#   <email id="xsd:string [1]" domain="xsd:string [1]"/> [0..*]
# </author>

  printf "%s<metadata>\n", $indent;

  printf "%s<name>%s</name>\n", $indent x 2, $g_trackName;
  printf "%s<desc>%s</desc>\n", $indent x 2, $g_description;

  printf "%s<author>\n", $indent x 2;
  printf "%s<name>%s</name>\n", $indent x 4, $g_name;
  printf "%s</author>\n", $indent x 2;

  printf "%s<link href=\"%s\">\n", $indent x 2, "http://acme.com/fit2gpx.pl";
  printf "%s<text>%s</text>\n", $indent x 4, "fit2gpx by Matjaz Rihtar";
  printf "%s</link>\n", $indent x 2;

  printf "%s<time>%s</time>\n", $indent x 2, $fit->date_string($g_startTime);

  printf "%s</metadata>\n", $indent;
} # PrintGpxMetadata

#==============================================================================
# Get 1st altitude from records. If not found, return home altitude.
sub Get1stAlt {
  my $timestamp = undef;
  my $alt = undef;

  foreach (@$records) {
    my $k; my $v;
    while (($k, $v) = each %$_) {
      if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
      elsif ($k eq "altitude") { $alt = $v; } # can be invalid
    }
    last; # check only 1st record
  }
  if (!defined $alt) { $alt = $g_homeAlt; }

  return $alt;
} # Get1stAlt

#==============================================================================
# Simple Kalman filtering routines (used as low-pass filter) for smoothing data
sub kalman_init {
  my ($state, $q, $r, $p, $initial_value) = @_;

  $state->{q} = $q;
  $state->{r} = $r;
  $state->{p} = $p;
  $state->{x} = $initial_value;
} # kalman_init

#------------------------------------------------------------------------------
sub kalman_update {
  my ($state, $value) = @_;

  my $q = $state->{q};
  my $r = $state->{r};
  my $p = $state->{p};
  my $x = $state->{x};
  my $k;

  $p = $p + $q;
  $k = $p / ($p + $r);
  $x = $x + $k * ($value - $x);
  $p = (1 - $k) * $p;

  $state->{q} = $q;
  $state->{r} = $r;
  $state->{p} = $p;
  $state->{x} = $x;

  return $x;
} # kalman_update

#==============================================================================
# Filter altitude data (smoothing sudden jumps in altitude)
# Because of the delay introduced with filter, final data is shifted left by
# 2 positions.
# Displaying the comparison graph is possible, if code at the end is uncommented.
sub FilterAlt {
  my $priv_alt = shift; # initial/home altitude
  my $k; my $v;

  # q = bigger (>4) => more precisely follows the original curve
  #     smaller (closer to zero) => more smooth, but also very delayed
  # r = bigger (>100) => more smooth, some delay, lowered peaks
  #     smaller (0.1) => exactly follows original curve
  # q & r stay the same, p & x are overwritten in each step
  # p = same as r
  # initial_value = initial/home altitude
  kalman_init(\%alt_state, 1, 15, 1, $priv_alt);

  my $timestamp;
  my $alt; my $altf;

  my $times = [];
  $orig_alt = [];
  $filt_alt = [];
  foreach (@$records) {
    $timestamp = undef;
    $alt = undef;

    while (($k, $v) = each %$_) {
      if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
      elsif ($k eq "altitude") { $alt = $v; } # can be invalid
    }
    if (!defined $alt) { $alt = $priv_alt; }

    $altf = kalman_update(\%alt_state, $alt);
    push @$times, $timestamp;
    push @$orig_alt, $alt;
    push @$filt_alt, $altf;
  # printf STDERR "alt = %g, altf = %g\n", $alt, $altf;

    $priv_alt = $alt;
  }
  # Fix filtered delay (2 positions, depends on kalman_init parameters)
  if (scalar @$filt_alt) {
    shift @$filt_alt;
    shift @$filt_alt;
    push @$filt_alt, $altf;
    push @$filt_alt, $altf;
  }

# Display graph of original and filtered alt
# my $data = GD::Graph::Data->new([$times, $orig_alt, $filt_alt]) or die GD::Graph::Data->error."\n";
# my $graph = GD::Graph::lines->new(5000,1000) or die GD::Graph::lines->error."\n";
# $graph->set(title => 'Altitude');
# my $gp = $graph->plot($data) or die $graph->error."\n";

# Save graph to file
# open OUT, ">", "alt.png" or die $!."\n";
# binmode OUT;
# print OUT $gp->png();
# close OUT;
} # FilterAlt

#==============================================================================
# Process all track points (records) from fit file
# This routine is called for calculating missing aver/min/max values only!
sub ProcessRecords {
  # Initialize arrays
  $alts = [];
  $hrs = [];
  $temps = [];
  $speedDownhills = [];
  $speedUphills = [];
  $incls = [];
  $inclDownhills = [];
  $inclUphills = [];
  $powers = [];
  $riseRates = [];
  $riseRateDownhills = [];
  $riseRateUphills = [];

  # Initiliaze some global vars
  $g_distDownhill = 0; $g_distUphill = 0;
  $g_timeDownhill = 0; $g_timeUphill = 0;
  $g_pauseTime = 0;
  $g_timeInTargetZone = 0;
  $g_timeOverTargetZone = 0; $g_timeUnderTargetZone = 0;
  $g_timeInIntZone1 = 0; $g_timeInIntZone2 = 0;
  $g_timeInIntZone3 = 0; $g_timeInIntZone4 = 0;
  $g_timeOverIntZone = 0; $g_timeUnderIntZone = 0;

  # Initialize previous values/arrays
  $prev_timestamp = $g_startTime;
  @prev_time = (0) x $histSize;
  $prev_lat = 0; $prev_lon = 0; # undef
  @prev_dist = (0) x $histSize;
  @prev_distDiff = (0) x $histSize;
  my $alt0 = Get1stAlt(); # initial/home altitude
  @prev_alt =  ($alt0) x $histSize;
  @prev_altDiff =  (0) x $histSize;
  $prev_speed = 0; $prev_hr = 0; # undef
  $prev_cad = 0; $prev_temp = 0; # undef
  $tot_time = 0;

  FilterAlt($alt0);
# kalman_init(\%pwr_state, 2, 25, 1, $alt0);

  my $fai = 0; # filtered alt index
  foreach (@$records) {
    ProcessRecord(\%$_, \$fai);
  }
} # ProcessRecords

#==============================================================================
# Process single track point (record) from fit file
# This routine stores all/min/max values for later aver/min/max processing.
# Power is calculated according to J.C. Martin et al. (1998).
# For power and incline calculation smoothed altitude data is used.
sub ProcessRecord {
  my $m = shift;
  my %mh = %$m;
  my $fai = shift; # pointer to filtered alt index

  my $timestamp = undef;
  my $lat = undef; my $lon = undef; my $dist = undef;
  my $alt = undef; my $speed = undef; my $power = undef;
  my $hr = undef; my $cad = undef; my $temp = undef;

  my $k; my $v;
  while (($k, $v) = each %mh) {
    if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
    elsif ($k eq "position_lat") { $lat = $v; } # can be missing
    elsif ($k eq "position_long") { $lon = $v; } # can be missing
    elsif ($k eq "distance") { $dist = $v; }
    elsif ($k eq "altitude") { $alt = $v; } # can be invalid
    elsif ($k eq "speed") { $speed = $v; }
    elsif ($k eq "power") { $power = $v; } # can be missing (=> calculated)
    elsif ($k eq "heart_rate") { $hr = $v; }
    elsif ($k eq "cadence") { $cad = $v; }
    elsif ($k eq "temperature") { $temp = $v; }
  }

  # Fill in default values if no data found
  if (!defined $lat) { $zeroll ? $lat = 0 : return; }
  if (!defined $lon) { $zeroll ? $lon = 0 : return; }

  if (defined $timestamp) {
    if (!defined $lat) { $lat = $prev_lat; }
    if (!defined $lon) { $lon = $prev_lon; }
    if (!defined $dist) { $dist = $prev_dist[-1]; }
    if (!defined $alt) { $alt = $prev_alt[-1]; }
    else {
      if ($alt < $g_minAlt) { $g_minAlt = $alt; }
      elsif ($alt > $g_maxAlt) { $g_maxAlt = $alt; }
    }
    push @$alts, $alt;
    if (!defined $speed) { $speed = $prev_speed; }
    else { $speed /= 3.6; } # m/s
  # if (!defined $power) { calculate power (see down below) }
    if (!defined $hr) { $hr = $prev_hr; }
    else {
      if ($hr < $g_minHr) { $g_minHr = $hr; }
    }
    if ($hr < $g_resHr) { $hr = $g_resHr; } # can't be lower than resting HR
    if (!defined $cad) { $cad = $prev_cad; }
    if (!defined $temp)  { $temp = $prev_temp; }
    else {
      if ($temp < $g_minTemp) { $g_minTemp = $temp; }
      elsif ($temp > $g_maxTemp) { $g_maxTemp = $temp; }
    }
    push @$temps, $temp;

    my $diff;

    my $distDiff = $dist - $prev_dist[-1];
    for (my $ii = 0; $ii < scalar @prev_dist; $ii++) {
      if ($ii == 0) { $diff = $dist - $prev_dist[-($ii+1)]; }
      else { $diff = $prev_dist[-$ii] - $prev_dist[-($ii+1)]; }
      unshift @prev_distDiff, $diff;
      pop @prev_distDiff;
    }

#   my $altDiff = $alt - $prev_alt[-1];
    # Calculate alt diff from kalman filtered alt
    my $altDiff = @{$filt_alt}[$$fai] - $prev_alt[-1]; ${$fai}++;
    for (my $ii = 0; $ii < scalar @prev_alt; $ii++) {
      if ($ii == 0) { $diff = $alt - $prev_alt[-($ii+1)]; }
      else { $diff = $prev_alt[-$ii] - $prev_alt[-($ii+1)]; }
      unshift @prev_altDiff, $diff;
      pop @prev_altDiff;
    }

  # my $distErr = Average(@prev_distDiff) * 0.75; # m, less than
    my $distErr = 5.0; # m, less than
    my $microErr = 0.5; # m, less than
    my $inclErr = 0.15; # %, not greater than
    my $altErr = $distErr * $inclErr; # 0.75 m, greater than

    # fix error in position
    my $totDistDiff = $prev_distDiff[-1];
    my $totAltDiff = $prev_altDiff[-1];
    my $used = 1;
    if ($totDistDiff == 0) { $totAltDiff = 0; }
    elsif ($totAltDiff == 0) { }
    elsif (abs($totDistDiff) < $distErr) {
      $totDistDiff += $prev_distDiff[-2];
      $totAltDiff += $prev_altDiff[-2];
      $used++;
      if (abs($totDistDiff) < $distErr) {
        $totDistDiff += $prev_distDiff[-3];
        $totAltDiff += $prev_altDiff[-3];
        $used++;
        if (abs($totDistDiff) < $distErr) {
          $totDistDiff += $prev_distDiff[-4];
          $totAltDiff += $prev_altDiff[-4];
          $used++;
          if (abs($totDistDiff) < $distErr) {
            $totDistDiff += $prev_distDiff[-5];
            $totAltDiff += $prev_altDiff[-5];
            $used++;
            if (abs($totDistDiff) < $distErr) {
              $totDistDiff += $prev_distDiff[-6];
              $totAltDiff += $prev_altDiff[-6];
              $used++;
            }
          }
        }
      }

      # fix error in altitude
      if (abs($totDistDiff) < $microErr) { $totAltDiff = 0; }
      elsif (abs($totDistDiff) < $distErr && abs($totAltDiff) > $altErr) {
        $totAltDiff = 0;
      }
    }
  # $totDistDiff = $prev_distDiff[-1];
  # $totAltDiff = $prev_altDiff[-1];
  # printf STDERR "distDiff = %g, altDiff = %g\n", $prev_distDiff[-1], $prev_altDiff[-1];
  # printf STDERR "totDistDiff = %g, totAltDiff = %g\n", $totDistDiff, $totAltDiff;

    #done: altitude = $alt
    if ($altDiff < 0) {
      #done: altitudeDifferencesDownhill = abs($altDiff)
      #done: altitudeDifferencesUphill = 0
      if ($speed > 0) { push @$speedDownhills, $speed; }
    }
    else {
      #done: altitudeDifferencesDownhill = 0
      #done: altitudeDifferencesUphill = abs($altDiff)
      if ($speed > 0) { push @$speedUphills, $speed; }
    }

    #done: cadence = $cad

    my $time = $timestamp - $prev_timestamp; # sec
    shift @prev_time;
    push @prev_time, $time;
    if ($prev_distDiff[-1] < 1.5) { # m, standing still (error in position)
      $g_pauseTime += $time;
    }

    # cal = % totCal +/- diff(hr, avgHr) % totCal
    my $ct;
    if ($g_totElapsTime != 0) { $ct = $g_totCal*$time/$g_totElapsTime; }
    else { $ct = 0; }
    my $cal;
    if ($g_avgHr != 0) { $cal = $ct + ($hr - $g_avgHr)/$g_avgHr*$ct; }
    else { $cal = $ct; }
    #done: calories = $cal

    #done: distanceAbsolute = $dist
    #done: distance = $distDiff
    if ($altDiff < 0) {
      #done: distanceDownhill = $distDiff
      #done: distanceUphill = 0
      $g_distDownhill += $distDiff;
    }
    else {
      #done: distanceDownhill = 0
      #done: distanceUphill = $distDiff
      $g_distUphill += $distDiff;
    }

    #done: heartrate = $hr

    # fix error in incline
    my $incl;
    if ($totDistDiff > 0) {
      $incl = $totAltDiff/$totDistDiff;
      if (abs($incl) > $inclErr) {
        # take average altitude of last few entries
        $totAltDiff = Waverage($used, @prev_altDiff);
        # fix error in altitude
        if (abs($totDistDiff) < $microErr) { $totAltDiff = 0; }
        elsif (abs($totDistDiff) < $distErr && abs($totAltDiff) > $altErr) {
          $totAltDiff = 0;
        }
        if ($totDistDiff > 0) { $incl = $totAltDiff/$totDistDiff; }
        else { $incl = 0; }
      }
    }
    else { $incl = 0; }
    if ($incl < $g_minIncl) { $g_minIncl = $incl; }
    elsif ($incl > $g_maxIncl) { $g_maxIncl = $incl; }
    if ($totAltDiff < 0) {
      push @$inclDownhills, $incl;
    }
    else {
      push @$inclUphills, $incl;
    }
    #done: incline = $incl * 100 # %
  # printf STDERR "incline = %g\n", $incl;

    my $iZone;
    if ($hr <= $g_iZone1s) { $iZone = 0; }
    elsif ($hr <= $g_iZone2s) { $iZone = 1; }
    elsif ($hr <= $g_iZone3s) { $iZone = 2; }
    elsif ($hr <= $g_iZone4s) { $iZone = 3; }
    elsif ($hr <= $g_iZone4e) { $iZone = 4; }
    else { $iZone = 5; }
    #done: intensityZone = $iZone

    #done: latitude = $lat
    #done: longitude = $lon

    my $pcHrMax;
    if ($g_hrMax != 0) { $pcHrMax = $hr / $g_hrMax * 100; }
    else { $pcHrMax = 0.0; }
    #done: percentHRMax = $pcHrMax

    #------------------------------
    # Power calculation
    #------------------------------
    # Air density (dry air)
    my $stdPress = 101325 * (1 - $alt*0.0000225577)**5.2559; # Pa
    my $gasConst = 287.05; # J/kg*K
    my $tempK = $temp + 273.15; # K
    my $airDens = $stdPress / ($gasConst * $tempK); # kg/m^3
  # printf STDERR "airDens = %g\n", $airDens;

    # 1. Aerodynamic power
    my $speedVec = $speed; # no wind!
    my $coefDragArea = $g_cda; # Hoods
    my $Fw = 0.0044; # spokes drag area ???
    my $Pad = $speed * 0.5 * $airDens * ($coefDragArea + $Fw) * $speedVec**2;
  # printf STDERR "Pad = %g\n", $Pad;

    # 2. Rolling resistance power
    my $coefRolRes = $g_crr; # MTB
    my $totMass = $g_weight + $g_bikeWeight; # kg
    my $g = 9.8067; # m/s^2
    my $atan_roadGrad = atan2($totAltDiff, $totDistDiff); # radians
    my $Prr = $speed * $totMass * $g * $coefRolRes * cos($atan_roadGrad);
  # printf STDERR "Prr = %g\n", $Prr;

    # 3. Wheel bearings power
    my $Pwb = $speed * (91 + 8.7 * $speed) * 0.001;
  # printf STDERR "Pwb = %g\n", $Pwb;

    # 4. Delta potential energy power
    my $Ppe = $speed * $totMass * $g * sin($atan_roadGrad);
    $Ppe = $Ppe * $g_pcf; # correction factor, otherwise PE too big ???
  # printf STDERR "Ppe = %g\n", $Ppe;

    # 5. Delta kinetic energy power
    my $I = 0.14; # moment of wheel inertia ???
    my $wheelRadius = $g_wheelSize / 1000 / (2 * 3.1415926536); # m
    my $Pke;
    if ($time > 0 && $speed > $prev_speed) {
      $Pke = 0.5 * ($totMass + $I / $wheelRadius**2) * ($speed**2 - $prev_speed**2) / $time;
      $Pke = $Pke * $g_kcf; # correction factor, otherwise KE too big ???
    }
    else { $Pke = 0; }
  # printf STDERR "Pke = %g\n", $Pke;

    # Net cycling power
    my $Pnet = $Pad + $Prr + $Pwb + $Ppe + $Pke;
  # printf STDERR "Pnet = %g\n", $Pnet;

    # Total cycling power
    my $Ec = 0.976; # drive chain efficiency (2.4 %)
    my $Ptot = $Pnet / $Ec;
  # printf STDERR "Ptot = %g\n", $Ptot;

    if (!defined $power || $fpcalc) {
      if ($Ptot < 0) { $power = 0; }
#     else { $power = kalman_update(\%pwr_state, $Ptot); }
      else { $power = $Ptot; }
    }
  # printf STDERR "power = %g\n", $power;

    if ($power < $g_minPower) { $g_minPower = $power; }
    elsif ($power > $g_maxPower) { $g_maxPower = $power; }
    push @$powers, $power;

    #done: power = $power
    #done: powerPerKG = $power / $g_weight

    #done: relativeRotations = 0

    my $totTimeDiff = sum(@prev_time);
    $totAltDiff = sum(@prev_altDiff);
    my $riseRate;
    if ($totTimeDiff > 0) { $riseRate = $totAltDiff * 60 / $totTimeDiff; }
    else { $riseRate = 0; }
    if ($riseRate < $g_minRiseRate) { $g_minRiseRate = $riseRate; }
    elsif ($riseRate > $g_maxRiseRate) { $g_maxRiseRate = $riseRate; }
    push @$riseRates, $riseRate;
    if ($totAltDiff < 0) {
      push @$riseRateDownhills, $riseRate;
    }
    else {
      push @$riseRateUphills, $riseRate;
    }
    #done: riseRate = $riseRate # m/min

    #done: rotations = 0

    #done: speed = $speed
    #done: speedReference = sensor

    #done: speedTime = 0

    my $targetZone; # 0 = below, 1 = inside, 2 = above
    if ($hr < $g_targetZoneStart) { # inconsistent (as in Sigma Data Center)
      $g_timeUnderTargetZone += $time; $targetZone = 0;
    }
    elsif ($hr >= $g_targetZoneEnd) {
      $g_timeOverTargetZone += $time; $targetZone = 2;
    }
    else {
      $g_timeInTargetZone += $time; $targetZone = 1;
    }
    #done: targetZone = $targetZone

    # time in intensity zones
    if ($hr <= $g_iZone1s) { $g_timeUnderIntZone += $time; }
    elsif ($hr > $g_iZone1s && $hr <= $g_iZone2s) { $g_timeInIntZone1 += $time; }
    elsif ($hr > $g_iZone2s && $hr <= $g_iZone3s) { $g_timeInIntZone2 += $time; }
    elsif ($hr > $g_iZone3s && $hr <= $g_iZone4s) { $g_timeInIntZone3 += $time; }
    elsif ($hr > $g_iZone4s && $hr <= $g_iZone4e) { $g_timeInIntZone4 += $time; }
    elsif ($hr > $g_iZone4e) { $g_timeOverIntZone += $time; }

    #done: temperature = $temp

    $tot_time += $time;
    #done: trainingTime = $time
    #done: trainingTimeAbsolute = $tot_time
    if ($altDiff < 0) {
      #done: trainingTimeDownhill = $time
      #done: trainingTimeUphill = 0
      $g_timeDownhill += $time;
    }
    else {
      #done: trainingTimeDownhill = 0
      #done: trainingTimeUphill = $time
      $g_timeUphill += $time;
    }

    # Work for international kcal (with 20%-25% efficiency, aver. 22.5%)
    # work[kJ] = energy[kcal] * 4.1868 * 0.225
    #done: workInKJ = $cal * 4.1868 * 0.225

    my $powerZone;
    if ($power < $g_pZone1s) { $powerZone = 0; }
    elsif ($power >= $g_pZone1s && $power < $g_pZone2s) { $powerZone = 1; }
    elsif ($power >= $g_pZone2s && $power < $g_pZone3s) { $powerZone = 2; }
    elsif ($power >= $g_pZone3s && $power < $g_pZone4s) { $powerZone = 3; }
    elsif ($power >= $g_pZone4s && $power < $g_pZone5s) { $powerZone = 4; }
    elsif ($power >= $g_pZone5s && $power < $g_pZone6s) { $powerZone = 5; }
    elsif ($power >= $g_pZone6s && $power < $g_pZone7s) { $powerZone = 6; }
    elsif ($power >= $g_pZone7s && $power < $g_pZone7e) { $powerZone = 7; }
    elsif ($power >= $g_pZone7e) { $powerZone = 8; }
    #done: powerZone = $powerZone

    $prev_timestamp = $timestamp;
    $prev_lat = $lat;
    $prev_lon = $lon;
    shift @prev_dist;
    push @prev_dist, $dist;
    shift @prev_alt;
    push @prev_alt, $alt;
    $prev_speed = $speed;
    $prev_hr = $hr;
    $prev_cad = $cad;
    $prev_temp = $temp;
  }
} # ProcessRecord

#==============================================================================
# Print all track points (trk block) from fit file
sub PrintGpxTracks {
# <trk>
#   <name> xsd:string </name> [0..1]
#   <cmt> xsd:string </cmt> [0..1]
#   <desc> xsd:string </desc> [0..1]
#   <src> xsd:string </src> [0..1]
#   <link> linkType </link> [0..*]
#   <number> xsd:nonNegativeInteger </number> [0..1]
#   <type> xsd:string </type> [0..1]
#   <extensions> extensionsType </extensions> [0..1]
#   <trkseg> trksegType </trkseg> [0..*]
# </trk>
#
# <trkseg>
#   <trkpt> wptType </trkpt> [0..*]
#   <extensions> extensionsType </extensions> [0..1]
# </trkseg>

  printf "%s<trk>\n", $indent;

  printf "%s<name>%s</name>\n", $indent x 2, $g_trackName;
  printf "%s<desc>%s</desc>\n", $indent x 2, $g_description;
  printf "%s<type>%s</type>\n", $indent x 2, ucfirst $g_sport;

  printf "%s<trkseg>\n", $indent x 2;
  my $ai = 0; # alt index
  foreach (@$records) {
    PrintGpxTrkpt(\%$_, $ai);
    $ai++;
  }
  printf "%s</trkseg>\n", $indent x 2;

  printf "%s</trk>\n", $indent;
} # PrintGpxTracks

#==============================================================================
# Print single track point from fit file
# Additional data (heart rate, cadence, temperature & power data) is formatted
# according to selected GPX extension.
sub PrintGpxTrkpt {
# <trkpt lat="latitudeType [1]" lon="longitudeType [1]">
#   <ele> xsd:decimal </ele> [0..1]
#   <time> xsd:dateTime </time> [0..1]
#   <magvar> degreesType </magvar> [0..1]
#   <geoidheight> xsd:decimal </geoidheight> [0..1]
#   <name> xsd:string </name> [0..1]
#   <cmt> xsd:string </cmt> [0..1]
#   <desc> xsd:string </desc> [0..1]
#   <src> xsd:string </src> [0..1]
#   <link> linkType </link> [0..*]
#   <sym> xsd:string </sym> [0..1]
#   <type> xsd:string </type> [0..1]
#   <fix> fixType </fix> [0..1]
#   <sat> xsd:nonNegativeInteger </sat> [0..1]
#   <hdop> xsd:decimal </hdop> [0..1]
#   <vdop> xsd:decimal </vdop> [0..1]
#   <pdop> xsd:decimal </pdop> [0..1]
#   <ageofdgpsdata> xsd:decimal </ageofdgpsdata> [0..1]
#   <dgpsid> dgpsStationType </dgpsid> [0..1]
#   <extensions> extensionsType </extensions> [0..1]
# </trkpt>

  my $m = shift;
  my %mh = %$m;
  my $ai = shift; # alt index

  my $timestamp = undef;
  my $lon = undef; my $lat = undef;
  my $ele = undef;

  my $hr = undef; my $cad = undef; my $temp = undef;

  my $k; my $v;
  while (($k, $v) = each %mh) {
    # mandatory
    if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
    elsif ($k eq "position_long") { $lon = $v; } # can be missing
    elsif ($k eq "position_lat") { $lat = $v; } # can be missing
    # optional
    elsif ($k eq "altitude") { $ele = $v; } # can be invalid
    elsif ($k eq "heart_rate") { $hr = $v; }
    elsif ($k eq "cadence") { $cad = $v; }
    elsif ($k eq "temperature") { $temp = $v; }
  }

  # Fill in default values if no data found
  if (!defined $lat) { $zeroll ? $lat = 0 : return; }
  if (!defined $lon) { $zeroll ? $lon = 0 : return; }
  $ele = @{$orig_alt}[$ai] if !defined $ele; # from FilterAlt

  if (defined $timestamp) {
    if ($rev_coord) {
      printf "%s<trkpt lon=\"%s\" lat=\"%s\">\n", $indent x 3, $lon, $lat;
    }
    else {
      printf "%s<trkpt lat=\"%s\" lon=\"%s\">\n", $indent x 3, $lat, $lon;
    }
    printf "%s<ele>%s</ele>\n", $indent x 4, $ele;
    printf "%s<time>%s</time>\n", $indent x 4, $fit->date_string($timestamp);

    if (defined $hr or defined $cad or defined $temp) {
      printf "%s<extensions>\n", $indent x 4;

      if ($garmin_ext) { printf "%s<gpxtpx:TrackPointExtension>\n", $indent x 5; }

      if (defined $hr) {
        if ($garmin_ext) {
          printf "%s<gpxtpx:hr>%s</gpxtpx:hr>\n", $indent x 6, $hr;
        }
        else { # cluetrust_ext
          printf "%s<gpxdata:hr>%s</gpxdata:hr>\n", $indent x 5, $hr;
        }
      }

      if (defined $cad) {
        if ($garmin_ext) {
          printf "%s<gpxtpx:cad>%s</gpxtpx:cad>\n", $indent x 6, $cad;
        }
        else { # cluetrust_ext
          printf "%s<gpxdata:cadence>%s</gpxdata:cadence>\n", $indent x 5, $cad;
        }
      }

      if (defined $temp) {
        if ($garmin_ext) {
          printf "%s<gpxtpx:atemp>%s</gpxtpx:atemp>\n", $indent x 6, $temp;
        }
        else { # cluetrust_ext
          printf "%s<gpxdata:temp>%s</gpxdata:temp>\n", $indent x 5, $temp;
        }
      }

      if ($garmin_ext) { printf "%s</gpxtpx:TrackPointExtension>\n", $indent x 5; }

      printf "%s</extensions>\n", $indent x 4;
    }

    printf "%s</trkpt>\n", $indent x 3;
    $tot_records++;
  }
} # PrintGpxTrkpt

#==============================================================================
# Print all lap entries (gpx extensions block) from fit file
sub PrintGpxExtensions {
# See https://github.com/pytrainer/pytrainer/wiki/Sample-GPX-

  printf "%s<extensions>\n", $indent;

  # Cluetrust extension, ignored by Garmin
  my $lap_index = 1;
  foreach (@$laps) {
    PrintGpxLap(\%$_, $lap_index);
    $lap_index++;
  }

  printf "%s</extensions>\n", $indent;
} # PrintGpxExtensions

#==============================================================================
# Print sigle lap entry (gpxdata block) from fit file
sub PrintGpxLap {
# <gpxdata:lap>
#   <gpxdata:index> xsd:int </gpxdata:index> [0..1]
#   <gpxdata:startPoint> locationType </gpxdata:startPoint> [0..1]
#   <gpxdata:endPoint> locationType </gpxdata:endPoint> [0..1]
#   <gpxdata:startTime> xsd:dateTime </gpxdata:startTime> [0..1]
#   <gpxdata:elapsedTime> xsd:float </gpxdata:elapsedTime> [0..1]
#   <gpxdata:calories> xsd:nonNegativeInteger </gpxdata:calories> [0..1]
#   <gpxdata:distance> xsd:float <gpxdata:distance> [0..1]
#   <gpxdata:trackReference> trackReferenceType </gpxdata:trackReference> [0..1]
#   <gpxdata:summary> summaryType </gpxdata:summary> [0..*]
#   <gpxdata:trigger> triggerType </gpxdata:trigger> [0..1]
#   <gpxdata:intensity> intensityKind </gpxdata:intensity> [0..1]
#     intensityKind (xsd:token): rest, active
# </gpxdata:lap>
#
# <gpxdata:startPoint lat="latitudeType [1]" lon="longitudeType [1]"/>
# <gpxdata:endPoint lat="latitudeType [1]" lon="longitudeType [1]"/>
# <gpxdata:summary name="xsd:string [1]" kind="summaryKind [1]"> xsd:decimal </gpxdata:summary>
#   summaryKind (xsd:token): min, max, avg
# <gpxdata:trigger kind="triggerKind [1]"/>
#   triggerKind (xsd:token): manual, time, distance, location, hr

  my $m = shift;
  my %mh = %$m;
  my $lap_index = shift;

  my $timestamp = undef; my $stime = undef;
  my $startlon = undef; my $startlat = undef;
  # endlon and endlat are not present in session!
  my $endlon = undef; my $endlat = undef;
  my $tetime = undef; my $tttime = undef; my $tstanding = undef;
  my $tdistance = undef; my $tcycles = undef; my $tcal = undef;
  my $avgspeed = undef; my $maxspeed = undef;
  my $tascent = undef; my $tdescent = undef;
  my $avghr = undef; my $maxhr = undef;
  my $avgcad = undef; my $maxcad = undef;
  my $avgpower = undef; my $maxpower = undef;
  my $ltrigger = undef;

  my $k; my $v;
  while (($k, $v) = each %mh) {
    if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
    elsif ($k eq "start_time") { $stime = $v; } # + $timeoffs;
    elsif ($k eq "start_position_lat") { $startlat = $v; }
    elsif ($k eq "start_position_long") { $startlon = $v; }
    elsif ($k eq "end_position_lat") { $endlat = $v; }
    elsif ($k eq "end_position_long") { $endlon = $v; }
    elsif ($k eq "total_elapsed_time") { $tetime = $v; }
    elsif ($k eq "total_timer_time") { $tttime = $v; }
    elsif ($k eq "time_standing") { $tstanding = $v; } # invalid
    elsif ($k eq "total_distance") { $tdistance = $v; } # m
    elsif ($k eq "total_cycles") { $tcycles = $v; }
    elsif ($k eq "total_calories") { $tcal = $v; }
    elsif ($k eq "time_in_hr_zone") { $g_timeHrZone = $v; } # array
    elsif ($k eq "avg_speed") { $avgspeed = $v; }
    elsif ($k eq "max_speed") { $maxspeed = $v; }
    elsif ($k eq "total_ascent") { $tascent = $v; }
    elsif ($k eq "total_descent") { $tdescent = $v; }
    elsif ($k eq "avg_heart_rate") { $avghr = $v; }
    elsif ($k eq "max_heart_rate") { $maxhr = $v; }
    elsif ($k eq "avg_cadence") { $avgcad = $v; }
    elsif ($k eq "max_cadence") { $maxcad = $v; }
    elsif ($k eq "avg_power") { $avgpower = $v; } # invalid ???
    elsif ($k eq "max_power") { $maxpower = $v; } # invalid ???
    elsif ($k eq "lap_trigger") { $ltrigger = conv_trigger($v); }
  }

  # Fill in default values if no data found
  $startlat = 0 if !defined $startlat;
  $startlon = 0 if !defined $startlon;
  $endlat = 0 if !defined $endlat;
  $endlon = 0 if !defined $endlon;
  $tcycles = 0 if !defined $tcycles;
  $tcal = 0 if !defined $tcal;
  $avgspeed = 0 if !defined $avgspeed;
  $maxspeed = 0 if !defined $maxspeed;
  $tascent = 0 if !defined $tascent;
  $tdescent = 0 if !defined $tdescent;
  $avghr = 0 if !defined $avghr;
  $maxhr = 0 if !defined $maxhr;
  $avgcad = 0 if !defined $avgcad;
  $maxcad = 0 if !defined $maxcad;
  $ltrigger = "manual" if !defined $ltrigger;

  if (defined $timestamp) {
    printf "%s<gpxdata:lap>\n", $indent x 2;

    printf "%s<gpxdata:index>%d</gpxdata:index>\n", $indent x 3, $lap_index;
    if ($rev_coord) {
      printf "%s<gpxdata:startPoint lon=\"%s\" lat=\"%s\"/>\n", $indent x 3, $startlon, $startlat;
    }
    else {
      printf "%s<gpxdata:startPoint lat=\"%s\" lon=\"%s\"/>\n", $indent x 3, $startlat, $startlon;
    }
    if ($rev_coord) {
      printf "%s<gpxdata:endPoint lon=\"%s\" lat=\"%s\"/>\n", $indent x 3, $endlon, $endlat;
    }
    else {
      printf "%s<gpxdata:endPoint lat=\"%s\" lon=\"%s\"/>\n", $indent x 3, $endlat, $endlon;
    }

    printf "%s<gpxdata:startTime>%s</gpxdata:startTime>\n",
      $indent x 3, $fit->date_string($stime);
    printf "%s<gpxdata:elapsedTime>%.1f</gpxdata:elapsedTime>\n",
      $indent x 3, $tetime;
    my $h; my $m; my $s;
    { use integer; $h = $tetime / 3600; }
    $tetime -= $h * 3600;
    { use integer; $m = $tetime / 60; }
    $s = $tetime - $m * 60;
    printf "%s<!-- elapsedTime>%d:%02d:%04.1f</elapsedTime -->\n",
      $indent x 3, $h, $m, $s;

    printf "%s<gpxdata:calories>%s</gpxdata:calories>\n",
      $indent x 3, $tcal;
    printf "%s<gpxdata:distance>%.1f</gpxdata:distance>\n",
      $indent x 3, $tdistance;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_speed", "avg", $avgspeed;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "max_speed", "max", $maxspeed;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "min_altitude", "min", $g_minAlt;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_altitude", "avg", Average(@$alts);
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "max_altitude", "max", $g_maxAlt;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "total_ascent", "max", $tascent;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "total_descent", "max", $tdescent;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "distance_uphill", "max", $g_distUphill;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "distance_downhill", "max", $g_distDownhill;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_incline_uphill", "avg", Average(@$inclUphills) * 100;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_incline_downhill", "avg", Average(@$inclDownhills) * 100;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_rise_rate_uphill", "avg", Average(@$riseRateUphills);
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_rise_rate_downhill", "avg", Average(@$riseRateDownhills);

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "min_heart_rate", "min", $g_minHr;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "avg_heart_rate", "avg", $avghr;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "max_heart_rate", "max", $maxhr;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "min_percent_hrmax", "min", $g_minHr / $g_hrMax * 100;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_percent_hrmax", "avg", $avghr / $g_hrMax * 100;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "max_percent_hrmax", "max", $maxhr / $g_hrMax * 100;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "time_under_target_zone", "max", $g_timeUnderTargetZone;
    printf "%s<!-- percent_under_target_zone>%.1f</percent_under_target_zone -->\n",
      $indent x 3, $g_timeUnderTargetZone / $tot_time * 100;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "time_in_target_zone", "max", $g_timeInTargetZone;
    printf "%s<!-- percent_in_target_zone>%.1f</percent_in_target_zone -->\n",
      $indent x 3, $g_timeInTargetZone / $tot_time * 100;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "time_over_target_zone", "max", $g_timeOverTargetZone;
    printf "%s<!-- percent_over_target_zone>%.1f</percent_over_target_zone -->\n",
      $indent x 3, $g_timeOverTargetZone / $tot_time * 100;

#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_under_intensity_zone", "max", $g_timeUnderIntZone;
#   printf "%s<!-- percent_under_intensity_zone>%.1f</percent_under_intensity_zone -->\n",
#     $indent x 3, $g_timeUnderIntZone / $tot_time * 100;
#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_in_intensity_zone1", "max", $g_timeInIntZone1;
#   printf "%s<!-- percent_in_intensity_zone1>%.1f</percent_in_intensity_zone1 -->\n",
#     $indent x 3, $g_timeInIntZone1 / $tot_time * 100;
#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_in_intensity_zone2", "max", $g_timeInIntZone2;
#   printf "%s<!-- percent_in_intensity_zone2>%.1f</percent_in_intensity_zone2 -->\n",
#     $indent x 3, $g_timeInIntZone2 / $tot_time * 100;
#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_in_intensity_zone3", "max", $g_timeInIntZone3;
#   printf "%s<!-- percent_in_intensity_zone3>%.1f</percent_in_intensity_zone3 -->\n",
#     $indent x 3, $g_timeInIntZone3 / $tot_time * 100;
#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_in_intensity_zone4", "max", $g_timeInIntZone4;
#   printf "%s<!-- percent_in_intensity_zone4>%.1f</percent_in_intensity_zone4 -->\n",
#     $indent x 3, $g_timeInIntZone4 / $tot_time * 100;
#   printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
#     $indent x 3, "time_over_intensity_zone", "max", $g_timeOverIntZone;
#   printf "%s<!-- percent_over_intensity_zone>%.1f</percent_over_intensity_zone -->\n",
#     $indent x 3, $g_timeOverIntZone / $tot_time * 100;

    $avgpower = Average(@$powers) if (!defined $avgpower || $fpcalc);
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_power", "avg", $avgpower;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_power_KJ", "avg", $avgpower * $tetime / 1000; # E(KJ)= P(W) x t(s) / 1000
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_power_W_per_Kg", "avg", $avgpower / $g_weight;
    $maxpower = $g_maxPower if !defined $maxpower;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "max_power", "max", $maxpower;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "avg_cadence", "avg", $avgcad;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "max_cadence", "max", $maxcad;

    $g_minTemp = 0 if $g_minTemp == 10000;
    $g_maxTemp = 0 if $g_maxTemp == -10000;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "min_temperature", "min", $g_minTemp;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "avg_temperature", "avg", Median(@$temps);
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%.1f</gpxdata:summary>\n",
      $indent x 3, "max_temperature", "max", $g_maxTemp;

    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "total_cycles", "max", $tcycles;
    printf "%s<gpxdata:summary name=\"%s\" kind=\"%s\">%d</gpxdata:summary>\n",
      $indent x 3, "total_records", "max", $tot_records;

    printf "%s<gpxdata:trigger kind=\"%s\"/>\n", $indent x 3, $ltrigger;
    printf "%s<gpxdata:intensity>%s</gpxdata:intensity>\n", $indent x 3, "active";

    printf "%s</gpxdata:lap>\n", $indent x 2;
  }
} # PrintGpxLap

#==============================================================================
# Convert FIT trigger type to Cluetrust GPX extension trigger type
sub conv_trigger {
# manual, time, distance, position_start, position_lap,
# position_waypoint, position_marked, session_end, fitness_equipment
# ==> manual, time, distance, location, hr
  my $v = shift;

  my $rv = "manual";
  if ($v eq "manual") { $rv = "manual"; }
  elsif ($v eq "time") { $rv = "time"; }
  elsif ($v eq "distance") { $rv = "distance"; }
  elsif ($v eq "position_start") { $rv = "location"; }
  elsif ($v eq "position_lap") { $rv = "location"; }
  elsif ($v eq "position_waypoint") { $rv = "location"; }
  elsif ($v eq "position_marked") { $rv = "location"; }
  elsif ($v eq "session_end") { $rv = "manual"; }
  elsif ($v eq "fitness_equipment") { $rv = "manual"; }

  return $rv;
} # conv_trigger

#==============================================================================
# Print all data from fit file in gpx format
sub PrintGpxData {
  my $fname = shift;

  # redirect STDOUT to destination file
  open TMP, ">", $fname or die $!."\n";
  select TMP;

  printf "<?xml %s?>\n", 'version="1.0" encoding="UTF-8"';

  PrintGpxHeader(); # version, xmlns

  PrintGpxMetadata(); # name, author, ...

  PrintGpxTracks(); # track segments & points, ...

  PrintGpxExtensions(); # laps, session, ...

  printf "</gpx>\n";

  select STDOUT;
  close TMP;
} # PrintGpxData
