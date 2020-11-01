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
use Data::UUID;
use Config::Simple ('-lc'); # ignore case
#use GD::Graph::Data;
#use GD::Graph::lines;

#------------------------------------------------------------------------------
# Global variables definition
(my $prog = basename($0)) =~ s/.pl$//i;

my $indent = " " x 2; # default indent: 2 spaces
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
# SigmaSport: cycling, mountainbike, racing_bicycle (road bike), running, ...
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
my $file;
foreach (@ARGV) {
  my $arg = $_;
  if ($arg eq "-v") { Usage(1); }
  elsif ($arg eq "-h" || $arg eq "-?") { Usage(0); }
  elsif ($arg eq "-y") { $overwrite = 1; }
  elsif ($arg eq "-p") { $fpcalc = 1; }
  elsif ($arg eq "-z") { $zeroll = 1; }
  else { $file = $arg; }
}

Usage(0) if !defined $file;
(my $slf_file = $file) =~ s/.fit$/.slf/i;

die "File $slf_file already exists\n" if -f $slf_file && !$overwrite;

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
PrintSlfData($slf_file);

exit(0);
# main

#==============================================================================
# Print version/usage and exit
sub Usage {
  my $ver_only = shift;

  if ($ver_only) {
    printf STDERR "fit2slf 2.14  Copyright (c) 2016-2020 Matjaz Rihtar  (Sep 7, 2020)\n";
    printf STDERR "Garmin::FIT  Copyright (c) 2010-2017 Kiyokazu Suto\n";
    printf STDERR "FIT protocol ver: %s, profile ver: %s\n",
      Garmin::FIT->protocol_version_string, Garmin::FIT->profile_version_string;
  }
  else {
    printf STDERR "Usage: $prog [-v|-h] [-y] [-p] [-z] <fit-file>\n";
    printf STDERR "  -v  Print version and exit\n";
    printf STDERR "  -h  Print short help and exit\n";
    printf STDERR "  -y  Overwrite <slf-file> if it exists (default: don't overwrite)\n";
    printf STDERR "  -p  Force power calculation (default: use fit power data if present)\n";
    printf STDERR "  -z  Allow zero values for latitude/longitude if not present\n";
    printf STDERR "      (default: ignore records with no latitude/longitude)\n";
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

    my $len = @$v;
    for ($j = 0 ; $j < $c ; $j++) {
      my $ij = $i + $j;
      $ij >= $len && next;
      Garmin::FIT->isnan($v->[$ij]) && next;
      $v->[$ij] != $invalid && last;
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

  # Override manufacturer and product (required by Sigma Data Center)
  $g_manuf = "SigmaSport";
  $g_product = "rox100";
  $g_serial = 1952031142 if !defined $g_serial; # any serial is OK

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
  $g_startTime = time() if !defined $g_startTime;
  $g_startLat = 0 if !defined $g_startLat;
  $g_startLon = 0 if !defined $g_startLon;
  $g_endLat = 0 if !defined $g_endLat;
  $g_endLon = 0 if !defined $g_endLon;
  $g_totElapsTime = 0 if !defined $g_totElapsTime;
  $g_totDistance = 0 if !defined $g_totDistance;
  $g_totCal = 0 if !defined $g_totCal;
  $g_avgSpeed = 0 if !defined $g_avgSpeed;
  $g_maxSpeed = 0 if !defined $g_maxSpeed;
  $g_totAscent = 0 if !defined $g_totAscent;
  $g_totDescent = 0 if !defined $g_totDescent;
  $g_avgHr = 0 if !defined $g_avgHr;
  $g_avgCad = 0 if !defined $g_avgCad;
  $g_maxCad = 0 if !defined $g_maxCad;

  # Some other defaults needed for the 1st call (overriden later)
  my $undef_homeAlt;
  if (!defined $g_homeAlt) { $g_homeAlt = 315; $undef_homeAlt = 1; }
  my $undef_bikeWeight;
  if (!defined $g_bikeWeight) { $g_bikeWeight = 12.3; $undef_bikeWeight = 1; }

  # Redirect STDOUT to temporary file
  open TMP, ">", undef or die $!."\n";
  select TMP;

  # Calc missing aver/min/max alt, power, hr, cad and temp from all records
  PrintSlfEntries();

  select STDOUT;
  close TMP;

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
# print STDERR "INI: description = |$val|\n" if defined $val;
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
# Generate new GUID (required by Sigma Data Center)
sub NewGuid {
  my $ug = Data::UUID->new();
  my $uuid = $ug->create_str();
  return $uuid;
} # NewGuid

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
# Print activity block (1st line, header) in slf
sub PrintSlfHeader {
  my @wday = qw(Sun Mon Tue Wed Thu Fri Sat);
  my @mon = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
  my @lt = localtime($g_startTime);

# Don't use POSIX::strftime("%a %b %e %T GMT%z %Y", @lt) because of locale
  $f_startTime = $wday[$lt[6]] . " " . $mon[$lt[4]];
  $f_startTime .= POSIX::strftime(" %e %T GMT%z %Y", @lt);
  $f_startTime =~ s/ +/ /g;

  printf "<Activity";
  printf " fileDate=\"%s\"", $f_startTime;
  printf " revision=\"400\"";
  printf ">\n";
} # PrintSlfHeader

#==============================================================================
# Print computer block in slf
sub PrintSlfComputer {
  printf "%s<Computer", $indent;
  printf " unit=\"%s\"", $g_product; # must be rox100!
  printf " serial=\"%s\"", $g_serial;
  printf " activityType=\"%s\"", "Cycling"; # always???
  printf " dateCode=\"%s\"", $f_startTime;
  printf "/>\n";
} # PrintSlfComputer

#==============================================================================
# Print general information block in slf
# Here every possible aver/min/max values are printed.
sub PrintSlfGeneralInfo {
  printf "%s<GeneralInformation>\n", $indent;

  printf "%s<user", $indent x 2;
  printf " color=\"%d\"", 255;
  printf " gender=\"%s\">", $g_gender;
  printf "<![CDATA[%s]]></user>\n", $g_name;

  printf "%s<sport><![CDATA[%s]]></sport>\n",
    $indent x 2, $g_sport;
  printf "%s<age>%d</age>\n",
    $indent x 2, $g_age;
  printf "%s<GUID>%s</GUID>\n",
    $indent x 2, NewGuid();

  printf "%s<altitudeDifferencesDownhill>%d</altitudeDifferencesDownhill>\n",
    $indent x 2, $g_totDescent * 1000; # mm
  printf "%s<altitudeDifferencesUphill>%d</altitudeDifferencesUphill>\n",
    $indent x 2, $g_totAscent * 1000; # mm

  printf "%s<averageAltitude>%d</averageAltitude>\n",
    $indent x 2, Average(@$alts) * 1000; # mm

  printf "%s<averageCadence>%g</averageCadence>\n",
    $indent x 2, $g_avgCad;
  printf "%s<averageHeartrate>%d</averageHeartrate>\n",
    $indent x 2, $g_avgHr;

  printf "%s<averageInclineDownhill>%.1f</averageInclineDownhill>\n",
    $indent x 2, Average(@$inclDownhills) * 100; # %
  printf "%s<averageInclineUphill>%.1f</averageInclineUphill>\n",
    $indent x 2, Average(@$inclUphills) * 100; # %

  my $pcHrMax;
  if ($g_hrMax != 0) { $pcHrMax = $g_avgHr / $g_hrMax * 100; }
  else { $pcHrMax = 0.0; }
  printf "%s<averagePercentHRMax>%.1f</averagePercentHRMax>\n",
    $indent x 2, $pcHrMax;

  $g_avgPower = Average(@$powers) if !defined $g_avgPower;
  printf "%s<averagePower>%.1f</averagePower>\n",
    $indent x 2, $g_avgPower;
  printf "%s<averagePowerKJ>%d</averagePowerKJ>\n",
    $indent x 2, $g_avgPower * $g_totElapsTime / 1000; # E(KJ)= P(W) x t(s) / 1000
  printf "%s<averagePowerWattPerKG>%.1f</averagePowerWattPerKG>\n",
    $indent x 2, $g_avgPower / $g_weight;

  printf "%s<averageRiseRate>%.1f</averageRiseRate>\n",
    $indent x 2, Average(@$riseRates) * 1000; # mm
  printf "%s<averageRiseRateUphill>%.1f</averageRiseRateUphill>\n",
    $indent x 2, Average(@$riseRateUphills) * 1000; # mm/min
  printf "%s<averageRiseRateDownhill>%.1f</averageRiseRateDownhill>\n",
    $indent x 2, Average(@$riseRateDownhills) * 1000; # mm/min

  printf "%s<averageSpeed>%.1f</averageSpeed>\n",
    $indent x 2, $g_avgSpeed / 3.6; # m/s
  printf "%s<averageSpeedDownhill>%.1f</averageSpeedDownhill>\n",
    $indent x 2, Average(@$speedDownhills); # already in m/s
  printf "%s<averageSpeedUphill>%.1f</averageSpeedUphill>\n",
    $indent x 2, Average(@$speedUphills); # already in m/s

  printf "%s<averageTemperature>%.1f</averageTemperature>\n",
    $indent x 2, Median(@$temps);

  printf "%s<bike>%s</bike>\n",
    $indent x 2, $g_bike;
  printf "%s<bikeWeight>%g</bikeWeight>\n",
    $indent x 2, $g_bikeWeight * 1000; # g
  printf "%s<bikeWeightUnit>%s</bikeWeightUnit>\n",
    $indent x 2, "g";

  printf "%s<bodyHeight>%g</bodyHeight>\n",
    $indent x 2, $g_height * 1000; # mm
  printf "%s<bodyHeightUnit>%s</bodyHeightUnit>\n",
    $indent x 2, "mm";
  printf "%s<bodyWeight>%.1f</bodyWeight>\n",
    $indent x 2, $g_weight * 1000; # g
  printf "%s<bodyWeightUnit>%s</bodyWeightUnit>\n",
    $indent x 2, "g";

  printf "%s<calories>%d</calories>\n",
    $indent x 2, $g_totCal;
  printf "%s<caloriesDifferenceFactor>%g</caloriesDifferenceFactor>\n",
    $indent x 2, 0.0; # ???

  printf "%s<description><![CDATA[%s]]></description>\n",
    $indent x 2, $g_description;

  printf "%s<distance>%.1f</distance>\n",
    $indent x 2, $g_totDistance; # m
  printf "%s<distanceDownhill>%.1f</distanceDownhill>\n",
    $indent x 2, $g_distDownhill; # m
  printf "%s<distanceUphill>%.1f</distanceUphill>\n",
    $indent x 2, $g_distUphill; # m

  printf "%s<exerciseTime>%g</exerciseTime>\n",
    $indent x 2, $g_totElapsTime * 100; # 1/100 sec

  printf "%s<externalLink><![CDATA[%s]]></externalLink>\n",
    $indent x 2, ""; # not yet known
  printf "%s<gender>%s</gender>\n",
    $indent x 2, $g_gender;
  printf "%s<hrMax>%d</hrMax>\n",
    $indent x 2, $g_hrMax;

  printf "%s<intensityZone1Start>%d</intensityZone1Start>\n",
    $indent x 2, $g_iZone1s;
  printf "%s<intensityZone2Start>%d</intensityZone2Start>\n",
    $indent x 2, $g_iZone2s;
  printf "%s<intensityZone3Start>%d</intensityZone3Start>\n",
    $indent x 2, $g_iZone3s;
  printf "%s<intensityZone4Start>%d</intensityZone4Start>\n",
    $indent x 2, $g_iZone4s;
  printf "%s<intensityZone4End>%d</intensityZone4End>\n",
    $indent x 2, $g_iZone4e;

  printf "%s<latitudeEnd>%.7f</latitudeEnd>\n",
    $indent x 2, $g_endLat;
  printf "%s<latitudeStart>%.7f</latitudeStart>\n",
    $indent x 2, $g_startLat;
  printf "%s<longitudeEnd>%.7f</longitudeEnd>\n",
    $indent x 2, $g_endLon;
  printf "%s<longitudeStart>%.7f</longitudeStart>\n",
    $indent x 2, $g_startLon;

  printf "%s<lowerLimit>%d</lowerLimit>\n",
    $indent x 2, $g_targetZoneStart; # HR target zone start
  printf "%s<manualTemperature>%g</manualTemperature>\n",
    $indent x 2, 0; # ???

  # Fix unprocessed min/max values
  my $minAlt; my $maxAlt;
  $minAlt = $g_minAlt == 10000 ? 0 : $g_minAlt;
  $maxAlt = $g_maxAlt == -10000 ? 0 : $g_maxAlt;
  my $minHr; my $maxHr;
  $minHr = $g_minHr == 10000 ? 0 : $g_minHr;
  $maxHr = $g_maxHr == -10000 ? 0 : $g_maxHr;
  my $minTemp; my $maxTemp;
  $minTemp = $g_minTemp == 10000 ? 0 : $g_minTemp;
  $maxTemp = $g_maxTemp == -10000 ? 0 : $g_maxTemp;
  my $minIncl; my $maxIncl;
  $minIncl = $g_minIncl == 10000 ? 0 : $g_minIncl;
  $maxIncl = $g_maxIncl == -10000 ? 0 : $g_maxIncl;
  my $minPower; my $maxPower;
  $minPower = $g_minPower == 10000 ? 0 : $g_minPower;
  $maxPower = $g_maxPower == -10000 ? 0 : $g_maxPower;
  my $minRiseRate; my $maxRiseRate;
  $minRiseRate = $g_minRiseRate == 10000 ? 0 : $g_minRiseRate;
  $maxRiseRate = $g_maxRiseRate == -10000 ? 0 : $g_maxRiseRate;

  printf "%s<maximumAltitude>%.1f</maximumAltitude>\n",
    $indent x 2, $maxAlt * 1000; # mm
  printf "%s<maximumCadence>%d</maximumCadence>\n",
    $indent x 2, $g_maxCad;
  printf "%s<maximumHeartrate>%d</maximumHeartrate>\n",
    $indent x 2, $maxHr;
  printf "%s<maximumIncline>%g</maximumIncline>\n",
    $indent x 2, $maxIncl * 100; # %
  printf "%s<maximumInclineDownhill>%g</maximumInclineDownhill>\n",
    $indent x 2, $minIncl * 100; # %, approx???
  printf "%s<maximumInclineUphill>%g</maximumInclineUphill>\n",
    $indent x 2, $maxIncl * 100; # %, approx???

  if ($g_hrMax != 0) { $pcHrMax = $maxHr / $g_hrMax * 100; }
  else { $pcHrMax = 0.0; }
  printf "%s<maximumPercentHRMax>%.1f</maximumPercentHRMax>\n",
    $indent x 2, $pcHrMax;

  printf "%s<maximumPower>%d</maximumPower>\n",
    $indent x 2, $maxPower;

  printf "%s<maximumRiseRate>%.1f</maximumRiseRate>\n",
    $indent x 2, $maxRiseRate * 1000; # mm
  printf "%s<maximumSpeed>%.1f</maximumSpeed>\n",
    $indent x 2, $g_maxSpeed / 3.6; # m/s
  printf "%s<maximumTemperature>%.1f</maximumTemperature>\n",
    $indent x 2, $maxTemp;

  printf "%s<measurement>%s</measurement>\n",
    $indent x 2, "kmh";

  printf "%s<minimumAltitude>%d</minimumAltitude>\n",
    $indent x 2, $minAlt * 1000; # mm
  printf "%s<minimumCadence>%d</minimumCadence>\n",
    $indent x 2, 0; # always zero???
  printf "%s<minimumHeartrate>%d</minimumHeartrate>\n",
    $indent x 2, $minHr;
  printf "%s<minimumIncline>%.1f</minimumIncline>\n",
    $indent x 2, $minIncl * 100; # %, zero???

  if ($g_hrMax != 0) { $pcHrMax = $minHr / $g_hrMax * 100; }
  else { $pcHrMax = 0.0; }
  printf "%s<minimumPercentHRMax>%.1f</minimumPercentHRMax>\n",
    $indent x 2, $pcHrMax;

  printf "%s<minimumPower>%d</minimumPower>\n",
    $indent x 2, 0; # always zero???
  printf "%s<minimumRiseRate>%.1f</minimumRiseRate>\n",
    $indent x 2, $minRiseRate * 1000; # mm
  printf "%s<minimumSpeed>%d</minimumSpeed>\n",
    $indent x 2, 0 / 3.6; # m/s, always zero???
  printf "%s<minimumTemperature>%.1f</minimumTemperature>\n",
    $indent x 2, $minTemp;

  printf "%s<name><![CDATA[%s]]></name>\n",
    $indent x 2, $g_trackName;
  printf "%s<pageHeaderData>%s</pageHeaderData>\n",
    $indent x 2, ""; # comma separated array of numbers???

  printf "%s<pauseTime>%d</pauseTime>\n",
    $indent x 2, $g_pauseTime * 100; # 1/100 sec

  printf "%s<powerZone1Start>%d</powerZone1Start>\n",
    $indent x 2, $g_pZone1s; # W
  printf "%s<powerZone2Start>%d</powerZone2Start>\n",
    $indent x 2, $g_pZone2s; # W
  printf "%s<powerZone3Start>%d</powerZone3Start>\n",
    $indent x 2, $g_pZone3s; # W
  printf "%s<powerZone4Start>%d</powerZone4Start>\n",
    $indent x 2, $g_pZone4s; # W
  printf "%s<powerZone5Start>%d</powerZone5Start>\n",
    $indent x 2, $g_pZone5s; # W
  printf "%s<powerZone6Start>%d</powerZone6Start>\n",
    $indent x 2, $g_pZone6s; # W
  printf "%s<powerZone7End>%d</powerZone7End>\n",
    $indent x 2, $g_pZone7e; # W
  printf "%s<powerZone7Start>%d</powerZone7Start>\n",
    $indent x 2, $g_pZone7s; # W

  printf "%s<rating>%d</rating>\n",
    $indent x 2, 1; # 1 = 1 star???
  printf "%s<feeling>%d</feeling>\n",
    $indent x 2, 2; # 2 = neutral???

  printf "%s<trainingTimeDownhill>%d</trainingTimeDownhill>\n",
    $indent x 2, $g_timeDownhill * 100; # 1/100 sec
  printf "%s<trainingTimeUphill>%d</trainingTimeUphill>\n",
    $indent x 2, $g_timeUphill * 100; # 1/100 sec

  printf "%s<samplingRate>%d</samplingRate>\n",
    $indent x 2, 5; # sec???

  printf "%s<shoulderWidth>%d</shoulderWidth>\n",
    $indent x 2, 0; # ???
  printf "%s<shoulderWidthUnit>%s</shoulderWidthUnit>\n",
    $indent x 2, "cm";

  printf "%s<startDate>%s</startDate>\n",
    $indent x 2, $f_startTime;
  printf "%s<statistic>%s</statistic>\n",
    $indent x 2, "true";

  printf "%s<thresholdPower>%d</thresholdPower>\n",
    $indent x 2, $g_funcThresPower;

  printf "%s<timeInIntensityZone1>%d</timeInIntensityZone1>\n",
    $indent x 2, $g_timeInIntZone1 * 100; # 1/100 sec
  printf "%s<timeInIntensityZone2>%d</timeInIntensityZone2>\n",
    $indent x 2, $g_timeInIntZone2 * 100; # 1/100 sec
  printf "%s<timeInIntensityZone3>%d</timeInIntensityZone3>\n",
    $indent x 2, $g_timeInIntZone3 * 100; # 1/100 sec
  printf "%s<timeInIntensityZone4>%d</timeInIntensityZone4>\n",
    $indent x 2, $g_timeInIntZone4 * 100; # 1/100 sec
  printf "%s<timeInTargetZone>%d</timeInTargetZone>\n",
    $indent x 2, $g_timeInTargetZone * 100; # 1/100 sec

  printf "%s<timeOverIntensityZone>%d</timeOverIntensityZone>\n",
    $indent x 2, $g_timeOverIntZone * 100; # 1/100 sec
  printf "%s<timeOverTargetZone>%d</timeOverTargetZone>\n",
    $indent x 2, $g_timeOverTargetZone * 100; # 1/100 sec
  printf "%s<timeUnderIntensityZone>%d</timeUnderIntensityZone>\n",
    $indent x 2, $g_timeUnderIntZone * 100; # 1/100 sec
  printf "%s<timeUnderTargetZone>%d</timeUnderTargetZone>\n",
    $indent x 2, $g_timeUnderTargetZone * 100; # 1/100 sec

  printf "%s<trackProfile>%d</trackProfile>\n",
    $indent x 2, 0; # ???

  printf "%s<trainingTime>%d</trainingTime>\n",
    $indent x 2, $g_totElapsTime * 100; # 1/100 sec

  printf "%s<trainingType>%s</trainingType>\n",
    $indent x 2, ucfirst $g_trainType;

  # SigmaSport: fitZone, fatZone, ownZone, ownZone1, ownZone2, ownZone3
  printf "%s<trainingZone>%s</trainingZone>\n",
    $indent x 2, "fitZone"; # ???

  printf "%s<upperLimit>%d</upperLimit>\n",
    $indent x 2, $g_targetZoneEnd; # HR target zone end

  printf "%s<weather>%d</weather>\n",
    $indent x 2, 0; # 0 = cloudless???

  printf "%s<wheelSize>%d</wheelSize>\n",
    $indent x 2, $g_wheelSize;

  printf "%s<wind>%d</wind>\n",
    $indent x 2, 0; # 0 = 0 Bft/Calm???

  printf "%s<workInKJ>%.1f</workInKJ>\n",
    $indent x 2, $g_totCal * 4.1868 * 0.225; # for international kcal with 22.5% efficiency

  printf "%s<zone1Start>%d</zone1Start>\n",
    $indent x 2, 0; # which zone???
  printf "%s<zone2Start>%d</zone2Start>\n",
    $indent x 2, 0; # which zone???
  printf "%s<zone3End>%d</zone3End>\n",
    $indent x 2, 0; # which zone???
  printf "%s<zone3Start>%d</zone3Start>\n",
    $indent x 2, 0; # which zone???

  printf "%s<sharingInfo>%s</sharingInfo>\n",
    $indent x 2, "{\"twitterId\":\"0\",\"twoPeaksId\":\"0\",\"stravaId\":\"0\",\"facebookId\":\"0\",\"trainingPeaksId\":\"0\"}";

  printf "%s<Participant>%s</Participant>\n",
    $indent x 2, ""; # training partner, manual entry???

  printf "%s</GeneralInformation>\n", $indent;
} # PrintSlfGeneralInfo

#==============================================================================
# Get 1st altitude from records. If not found, return home altitude.
sub Get1stAlt {
  my $timestamp = undef;
  my $alt = undef;

  foreach (@$records) {
    my $k; my $v;
    while (($k, $v) = each %$_) {
      if ($k eq "timestamp") { $timestamp = $v; } # + $timeoffs;
      elsif ($k eq "altitude") {
        if (!defined $alt) { $alt = $v; } # can be invalid
      }
      elsif ($k eq "enhanced_altitude") {
        if (defined $v) { $alt = $v; } # only if valid
      }
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
      elsif ($k eq "altitude") {
        if (!defined $alt) { $alt = $v; } # can be invalid
      }
      elsif ($k eq "enhanced_altitude") {
        if (defined $v) { $alt = $v; } # only if valid
      }
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
# Print all track points (entries block) from fit file
# Because this routine is called twice, initialize all used variables at the
# beginning.
sub PrintSlfEntries {
  printf "%s<Entries>\n", $indent;

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
    PrintSlfEntry(\%$_, \$fai);
  }

  printf "%s</Entries>\n", $indent;
} # PrintSlfEntries

#==============================================================================
# Print single track point (record) from fit file
# This routine also stores all/min/max values for later aver/min/max processing.
# Power is calculated according to J.C. Martin et al. (1998).
# For power and incline calculation smoothed altitude data is used.
sub PrintSlfEntry {
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
    elsif ($k eq "altitude") {
      if (!defined $alt) { $alt = $v; } # can be invalid
    }
    elsif ($k eq "enhanced_altitude") {
      if (defined $v) { $alt = $v; } # only if valid
    }
    elsif ($k eq "speed") {
      if (!defined $speed) { $speed = $v; } # can be invalid
    }
    elsif ($k eq "enhanced_speed") {
      if (defined $v) { $speed = $v; } # only if valid
    }
    elsif ($k eq "power") { $power = $v; } # can be missing (=> calculated)
    elsif ($k eq "heart_rate") { $hr = $v; }
    elsif ($k eq "cadence") { $cad = $v; }
    elsif ($k eq "temperature") { $temp = $v; }
  }

  # Fill in default values if no data found
  if (!defined $lat) { $zeroll ? $lat = 0 : return; }
  if (!defined $lon) { $zeroll ? $lon = 0 : return; }

  if (defined $timestamp) {
    printf "%s<Entry", $indent x 2;

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

    printf " altitude=\"%.1f\"", $alt * 1000; # mm
    if ($altDiff < 0) {
      printf " altitudeDifferencesDownhill=\"%.1f\"", abs($altDiff) * 1000; # mm
      printf " altitudeDifferencesUphill=\"%g\"", 0;
      if ($speed > 0) { push @$speedDownhills, $speed; }
    }
    else {
      printf " altitudeDifferencesDownhill=\"%g\"", 0;
      printf " altitudeDifferencesUphill=\"%.1f\"", $altDiff * 1000; # mm
      if ($speed > 0) { push @$speedUphills, $speed; }
    }

    printf " cadence=\"%d\"", $cad;

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
    printf " calories=\"%.6f\"", $cal;

    printf " distanceAbsolute=\"%.1f\"", $dist;
    printf " distance=\"%.1f\"", $distDiff;
    if ($altDiff < 0) {
      printf " distanceDownhill=\"%.1f\"", $distDiff;
      printf " distanceUphill=\"%g\"", 0;
      $g_distDownhill += $distDiff;
    }
    else {
      printf " distanceDownhill=\"%g\"", 0;
      printf " distanceUphill=\"%.1f\"", $distDiff;
      $g_distUphill += $distDiff;
    }

    printf " heartrate=\"%d\"", $hr;

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
    printf " incline=\"%g\"", $incl * 100; # %
  # printf STDERR "incline = %g\n", $incl;

    my $iZone;
    if ($hr <= $g_iZone1s) { $iZone = 0; }
    elsif ($hr <= $g_iZone2s) { $iZone = 1; }
    elsif ($hr <= $g_iZone3s) { $iZone = 2; }
    elsif ($hr <= $g_iZone4s) { $iZone = 3; }
    elsif ($hr <= $g_iZone4e) { $iZone = 4; }
    else { $iZone = 5; }
    printf " intensityZone=\"%g\"", $iZone;

    printf " latitude=\"%.7f\"", $lat;
    printf " longitude=\"%.7f\"", $lon;

    my $pcHrMax;
    if ($g_hrMax != 0) { $pcHrMax = $hr / $g_hrMax * 100; }
    else { $pcHrMax = 0.0; }
    printf " percentHRMax=\"%.1f\"", $pcHrMax;

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

    printf " power=\"%.6f\"", $power;
    printf " powerPerKG=\"%.6f\"", $power / $g_weight;

    printf " relativeRotations=\"%.g\"", 0; # ???

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
    printf " riseRate=\"%.1f\"", $riseRate * 1000; # mm/min

    printf " rotations=\"%.g\"", 0; # ???

    printf " speed=\"%.4f\"", $speed;
    printf " speedReference=\"%s\"", "sensor"; 

    printf " speedTime=\"%g\"", 0; # ???

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
    printf " targetZone=\"%g\"", $targetZone;

    # time in intensity zones
    if ($hr <= $g_iZone1s) { $g_timeUnderIntZone += $time; }
    elsif ($hr > $g_iZone1s && $hr <= $g_iZone2s) { $g_timeInIntZone1 += $time; }
    elsif ($hr > $g_iZone2s && $hr <= $g_iZone3s) { $g_timeInIntZone2 += $time; }
    elsif ($hr > $g_iZone3s && $hr <= $g_iZone4s) { $g_timeInIntZone3 += $time; }
    elsif ($hr > $g_iZone4s && $hr <= $g_iZone4e) { $g_timeInIntZone4 += $time; }
    elsif ($hr > $g_iZone4e) { $g_timeOverIntZone += $time; }

    printf " temperature=\"%.1f\"", $temp;

    $tot_time += $time;
    printf " trainingTime=\"%.1f\"", $time * 100; # 1/100 sec
    printf " trainingTimeAbsolute=\"%.1f\"", $tot_time * 100; # 1/100 sec
    if ($altDiff < 0) {
      printf " trainingTimeDownhill=\"%.1f\"", $time * 100; # 1/100 sec
      printf " trainingTimeUphill=\"%g\"", 0;
      $g_timeDownhill += $time;
    }
    else {
      printf " trainingTimeDownhill=\"%g\"", 0;
      printf " trainingTimeUphill=\"%.1f\"", $time * 100; # 1/100 sec
      $g_timeUphill += $time;
    }

    # Work for international kcal (with 20%-25% efficiency, aver. 22.5%)
    # work[kJ] = energy[kcal] * 4.1868 * 0.225
    printf " workInKJ=\"%.1f\"", $cal * 4.1868 * 0.225;

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
    printf " powerZone=\"%g\"", $powerZone;

    printf "/>\n";

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
} # PrintSlfEntry

#==============================================================================
# Print all lap entries (markers block) from fit file
sub PrintSlfMarkers {
  printf "%s<Markers>\n", $indent;

  my $lap_index = 1;
  foreach (@$laps) {
    PrintSlfMarker(\%$_, $lap_index);
    $lap_index++;
  }

  printf "%s</Markers>\n", $indent;
} # PrintSlfMarkers

#==============================================================================
# Print sigle lap entry (= marker by SigmaSport) from fit file
# Currently does nothing because of unknown format of a marker.
sub PrintSlfMarker {
  my $m = shift;
  my %mh = %$m;
  my $lap_index = shift;

} # PrintSlfMarker

#==============================================================================
# Print all data from fit file in slf format
sub PrintSlfData {
  my $fname = shift;

  # redirect STDOUT to destination file
  open TMP, ">", $fname or die $!."\n";
  select TMP;

  printf "<?xml %s?>\n", 'version="1.0" encoding="utf-8"';

  PrintSlfHeader(); # date, version

  PrintSlfComputer(); # computer info

  PrintSlfGeneralInfo(); # general info

  PrintSlfEntries(); # track points

  PrintSlfMarkers(); # laps

  printf "</Activity>\n";

  select STDOUT;
  close TMP;
} # PrintSlfData
