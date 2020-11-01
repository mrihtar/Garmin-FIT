## Config (ini) File Syntax

Config file for conversion scripts is in the classical [Windows **ini** format].
Values specified in config file override the ones read from FIT file (where
applicable).

**&#91;default&#93;** section can contain the following parameters:
- **sport**  
  SigmaSport defines this field as _cycling_, _mountainbike_, _racing_bycicle_
  (road bike), _running_, ... This field determines the track icon in Sigma Data
  Center.
- **training_type**  
  This gets written in the "Training type" field in Sigma Data Center (for
  example _riding_).
- **bike**  
  Pointer to a bike specific section. If names like _bike1_, _bike2_, ... are
  used, Sigma Data Center can match them to the bike names already stored in
  the database.
- **home_altitude**  
  This should be set to the altitude of the starting point (used for smoothing
  altitude deviations), although the program will adapt itself after some track
  points if it's not set correctly.
- **track_name**  
  This determines the track name when importing into Sigma Data Center. Can be
  left out.
- **description**  
  This gets written in the "Description" field in Sigma Data Center. Can be left
  out.

**&#91;personal&#93;** section can contain the following parameters:
- **name**  
  Should be the same as the user's name stored in Sigma Data Center so the
  track data will be assigned to the correct user.
- **gender**  
  Keyword _male_ or _female_.
- **age**  
  Age in years.
- **height**  
  User's height in cm.
- **weight**  
  User's weight in kg (without bike's weight, which is set separately).
- **resting_heart_rate**  
  In bpm, can be left out if you don't know it (set to reasonable default).
- **max_heart_rate**  
  In bpm, can be left out if you don't know it. In such case it's calculated
  from the latest recommended formula ```211 - 0.64*age``` (see [HUNT 2012]).
- **target_zone_start**  
  Can be left out if you don't want to set you own zone. In such case it's
  calculated as 60% of **max_heart_rate**.
- **target_zone_end**  
  Can be left out if you don't want to set you own zone. In such case it's
  calculated as 70% of **max_heart_rate**.
- **ftp**  
  **F**unctional **T**hreshold **P**ower. This is difficult to determine
  without special equipment (see [Wattbike] or [TrainingPeaks]). If left out
  it's set to 253 W.

Bike specific sections (for example **&#91;bike1&#93;**) can contain the
following parameters:
- **name**  
  Bike model/name. Not currently used except for distinguishing different bike
  sections.
- **type**  
  Keyword like _MTB_, _road_bike_, ... Will be used for determining parameters
  for power calculation.
- **weight**  
  Bike's weight in kg.
- **wheel_size**  
  Wheel circumference in mm (if not speficied in FIT data).
- **crr**  
  For power calculation: **C**oefficient of **R**olling **R**esistance. Can
  be left out or set according to the following (type of tyres):  
  ```Clinchers = 0.005, Tubulars = 0.004, MTB = 0.012```
- **cda**  
  For power calculation: **C**oefficient of **D**rag * frontal **A**rea. Can
  be left out or set according to the following (position on a bike):  
  ```Hoods = 0.388, Bartops = 0.445, Bar ends = 0.420, Drops = 0.300, Aerobar = 0.233```
- **pcf**  
  For power calculation: **P**otential energy **C**orrection **F**actor.
  Currently set to 0.65. If set to 1 (as it should be according to formula),
  the difference in potential energy (altitude difference) will have very big
  impact on calculated power.
- **kcf**  
  For power calculation: **K**inetic energy **C**orrection **F**actor.
  Currently set to 0.65. If set to 1 (as it should be according to formula),
  the difference in kinetic energy (speed difference) will have very big impact
  on calculated power.

Power calculation doesn't take into account the direction of the wind, so it
should be taken with a grain of salt (= don't trust it).

[Windows **ini** format]: https://en.wikipedia.org/wiki/INI_file
[HUNT 2012]: https://helseforskning.etikkom.no/ikbViewer/Content/345409/NES%20et%20al,%202012_HR_HUNTsms1445[1].pdf
[Wattbike]: https://support.wattbike.com/hc/en-us/articles/115001848349-Functional-Threshold-Power-FTP-Training-Zones
[TrainingPeaks]: https://www.trainingpeaks.com/blog/what-is-threshold-power/
