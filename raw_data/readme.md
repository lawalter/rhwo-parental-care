## Field definitions for raw data files

In progress.

### NOAA_weather_data.csv

- `STATION	NAME` -	the name of the weather station
- `LATITUDE` -	decimated degrees w/northern hemisphere values > 0, southern hemisphere values < 0
- `LONGITUDE` -	decimated degrees w/western hemisphere values < 0, eastern hemisphere values > 0
- `ELEVATION` -	elevation above mean sea level (tenths of meters)
- `DATE` -	the year of the record (4 digits) followed by month (2 digits) and day (2 digits)
- `PRCP`	- precipitation (mm)
- `SNOW` -	snowfall (mm)
- `SNWD` - snow depth (mm)
- `TMAX` -	maximum temperature (C)
- `TMIN` - minimum temperature (C)

### boris_incubation_data.csv

- `Observation.id` - ID of the video being analyzed
- `Subject` - ID of the woodpecker that is exhibiting a behavior
- `Behavior` - behavior that is being exhibited. types of behaviors include:
    - `head in` - woodpecker puts their head into the cavity (while the body remains perched at the cavity entrance)
    - `head out` - woodpecker puts their head out of the cavity (while the body remains perched inside)
    - `in cavity` - woodpecker is entirely inside the nest cavity
    - `stat` - woodpecker is visibly perched somewhere on the nest snag doing nothing
    - `preening` - woodpecker is adjusting and/or cleaning body feathers
    - `cleaning nest` - woodpecker takes wood chips out of the cavity
    - `stretch` - woodpecker stretches their wings
    - `drumming` - woodpecker drums agains the nest snag
    - `excavating` - woodpecker pecks at the cavity in such a way that it is clear the woodpecker is attempting to remove wood or modify its shape
    - `calling` - woodpecker makes a territorial call, such as "queer!" or "chur-chur-chur"
- `Behavior_type` 
    - `POINT` - a behavior that happens once; momentary
    - `STATE` - a behavior with a duration
- `Start_sec` - seconds since start time of video that the behavior begins, in seconds
- `Stop_sec` - seconds since start time of video that the behavior ends, in seconds
- `Duration_sec` - length of time a behavior was exhibited, in seconds

### boris_provisioning_preyproofed_bestdata.csv

- `Observation_id_boris`
- `Observation.id`
- `Subject` - ID of the woodpecker that is exhibiting a behavior
- `Behavior` - behavior that is being exhibited. types of behaviors include:
    - `head in` - woodpecker puts their head into the cavity (while the body remains perched at the cavity entrance)
    - `head out` - woodpecker puts their head out of the cavity (while the body remains perched inside)
    - `in cavity` - woodpecker is entirely inside the nest cavity
    - `stat` - woodpecker is visibly perched somewhere on the nest snag doing nothing
    - `preening` - woodpecker is adjusting and/or cleaning body feathers
    - `cleaning nest` - woodpecker takes fecal material out of the cavity
    - `stretch` - woodpecker stretches their wings
    - `drumming` - woodpecker drums agains the nest snag
    - `calling` - woodpecker makes a territorial call, such as "queer!" or "chur-chur-chur"
    - `feeding chicks` - woodpecker feeds fruit or animal matter to a nestling 
- `Modifier_general` - more detailed description of `Behavior`
    - `fruit` - food being brought to nestlings is identifiable as fruit (usually huckleberry, blueberry, or blackberry)
    - `insect` - food being brught to nestlings is identifiable as an insect or arachnid, usually by legs, antennae, or other features
    - `unidentified` - food is unidentifiable
    - `None` - equivalent to NA
    - `brooding` - sometimes assigned to `in cavity`, but not definitive, and should not be used to determine if an in cavity event was indeed brooding; largely a guess, but brooding was later defined using duration time using R code--please use clean_data/behaviors.csv to see summarized brooding data per video
    - `chur-chur-chur` - type of call being made
    - `queer` - type of call being made
- `Modifier_specific` - when food item was identified as `fruit` or `insect`, a more specific descriptor of what the food item could be (e.g. blackberry, katydid, moth) or if it was `unidentified` the reason why (e.g. obscured, too small)
- `Behavior_type` 
    - `POINT` - a behavior that happens once; momentary
    - `STATE` - a behavior with a duration
- `Start_sec` - seconds since start time of video that the behavior begins, in seconds
- `Stop_sec` - seconds since start time of video that the behavior ends, in seconds
- `Duration_sec` - length of time a behavior was exhibited, in seconds

### brood_contents.csv

- `Nest_ID` - ID for the nest snag (dead tree that contained the nest cavity)
- `Brood_ID` - ID for the brood attempt
- `Habitat` - "Savanna" or "CC" (Closed Canopy); for a description of how habitat type was determined, please refer to the publication
- `found_date` - date that the nest was first discovered (M/D/Y)
- `found_Julian` - Julian date that the nest was first discovered
- `max_eggs` - maximum number of eggs observed at the nest
- `max_chicks` - maximum number of chicks observed at the nest
- `egg_to_chick` -
- `chicks_fledged` -
- `chick_to_fledge` -
- `oldest_chick` -
- `oldest_chick` -
- `nest_fate` -

### incubation_video.csv

- `Observation.id` -
- `Sample_size` -
- `Egg_count` -
- `Usable_length` -
- `Video_number` -
- `Letter` -
- `Part` -
- `Date` -
- `Month` -
- `Day` -
- `Year` -
- `TA` -
- `Selected` -
- `Brood_ID` -
- `Clutch_laid` -
- `Hatch_date` -
- `Priority` -
- `Nest_ID` -
- `Ref_combo_1` -
- `Ref_combo_2` -
- `With_bpk?` -
- `Bpk_status_1` -
- `Bpk_status_2` -
- `Bpk_status_3` -
- `Bpk_status_4` -
- `Start_time` -
- `Early_or_late` -
- `BORIS_Observer` -
- `Summary/Notes` -

### nest_checks_2018.csv

### nest_checks_abbreviated_2018.csv

### provisioning_video_data.csv

### sex_data.csv

- `Band_number` - 
- `RR` -
- `LL` -	
- `Color_combo`	-
- `Sex`	-
- `Method` -
- `Year Sent`	-
- `2016_Roost_ID`	-
- `2017_Nest_ID` -
- `2018_Nest_ID` -
- `2018_Resight` -
