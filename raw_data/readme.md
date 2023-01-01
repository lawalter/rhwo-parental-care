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
- `egg_to_chick` - proportion of eggs that hatched
- `chicks_fledged` - number of chicks that fledged
- `chick_to_fledge` - proportion of chicks that fledged
- `oldest_chick` - age in days that the chicks grew to be before the nest fledged or failed
- `oldest_chick` - see above, this is a repeated column with NAs instead of 0s and some age values filled out that are missing from the other column
- `nest_fate` - fate of the nest; "fledge" indicates chicks were confirmed to have fledged, "fail" indicates the nest became empty before chicks could have viably left the nest (assumed depredation), "in progress" indicates the nest was still active at the end of the field season, and "uncertain" indicates lack of information to determine fledge or fail

### incubation_video.csv

- `Observation.id` - the file name of the video segment
- `Sample_size` - an ID for the video event
- `Egg_count` - number of eggs being incubated
- `Usable_length` - length of video that can be used for analysis in BORIS
- `Video_number` - an ID for the video event
- `Letter` - sequential alphabetical ID for the video segments within a video event
- `Part` - identifies which video out of the video event a segment covers, e.g. "2 of 3" means it's the second video out of three parts
- `Date` - date (M/D/Y) that the video was recorded
- `Month` - month that the video was recorded
- `Day` - day that the video was recorded
- `Year` - year that the video was recorded
- `TA` - training area in which the nest snag in the video is located
- `Selected` - 
- `Brood_ID` - ID for the brood being incubated (used to differentiate between different nesting attempts by a pair during one season)
- `Clutch_laid` - date that the clutch was laid
- `Hatch_date` - date that the first egg hatched
- `Priority` - field to indicate quality of the video
- `Nest_ID` - ID for the nest snag where the brood is being incubated
- `Ref_combo_1` - colorband combination of one Red-headed Woodpecker parent at the nest
- `Ref_combo_2` - colorband combination of the second Red-headed Woodpecker parent at the nest; if "unbanded" then the mate was not banded
- `With_bpk?` - field to indicate if one of the two parents GPS tagged at the time the video was recorded; `1` indicates yes, `0` indicates no
- `Bpk_status_1` - status of the first Red-headed Woodpecker in relation to wearing a GPS tag
    - `_before` suffix - the bird was GPS tagged on a date after this video was recorded
    - `_with` suffix - the bird is wearing a GPS tag during the video
    - `_after` suffix - the video was recorded on a date after the GPS tag was retrieved from the bird
    - `NA` - neither adult woodpecker was ever GPS tagged
- `Bpk_status_2` - status of the second Red-headed Woodpecker at the time the video was recorded
- `Bpk_status_3` - NA
- `Bpk_status_4` - NA
- `Start_time` - time that the video recording commenced 
- `Early_or_late` 
    - `early` - started before 11:00 am
    - `late` - started after 11:00 am
- `BORIS_Observer` - initials of the person that analyzed the video in BORIS
- `Summary/Notes` - notes about the video

### nest_checks_2018.csv

- `Nest_ID`	- ID of the nest
- `Year_found` - year that the nest was discovered
- `TA` - training area that the nest is located in
- `date_1` - date the nest was checked
- `observer_1` - observer that checked the nest
- `obs_type_1` - how the nest was observed
- `rank_1` 
    - `0` - empty
    - `1` - 1 woodpecker present
    - `2` - 2 woodpeckers present
    - `3` - excavating
    - `4` - eggs
    - `5` - chicks
    - `6` - chicks fledged
- `description_1` - description of the nest, often containing quantity of eggs or chicks, and/or other notes
- `...` - fields `date` through `description` are repeated for each check

### nest_checks_abbreviated_2018.csv

- `Nest_ID`	- ID of the nest
- `TA` - training area that the nest is located in
- `date_1` - date the nest was checked
- `observer_1` - observer that checked the nest
- `obs_type_1` - how the nest was observed
- `rank_1` 
    - `0` - empty
    - `1` - 1 woodpecker present
    - `2` - 2 woodpeckers present
    - `3` - excavating
    - `4` - eggs
    - `5` - chicks
    - `6` - chicks fledged
- `description_1` - description of the nest, often containing quantity of eggs or chicks, and/or other notes
- `...` - fields `date` through `description` are repeated for each check

### provisioning_video_data.csv

- `Observation.id` - name of the video file
- `Video_number` - videos were automatically split into roughly 64-minute parts by the video camera; this number is the identity of the video recording session, and indicates which video parts belong together.
- `Letter` - identifies where in the series the video belongs, e.g. "b" indicates it's the 2nd part out of the video series (series are denoted in the `Video_number` column)
- `Part` - identifies where in the series the video belongs, e.g. "2 of 8" indicates the video part is #2 out of 8 parts total
- `Date` - date the video was recorded (M/D/Y)
- `Month` - month the video was recorded
- `Day` - day the video was recorded
- `Julian_date` - Julian date for the day the video was recorded
- `Year` - year the video was recorded
- `TA` - training area in which the video was recorded
- `Habitat` - "Savanna" or "CC" (Closed Canopy); for a description of how habitat type was determined, please refer to the publication
- `Nearest_neighbor` - identity of the nearest known active Red-headed Woodpecker nest snag 
- `Distance_nearest_neighbor_m` - distance (in meters) to the nearest known active Red-headed Woodpecker nest snag
- `Brood_ID` - identity of the brood being recorded; multiple broods were sometimes attempted in the same nest cavity during a breeding season
- `First_egg_Julian` - Julian date of when the first egg was laid (this column was left blank)
- `Hatch_Julian` - Julian date of when the first egg hatched (this column was left blank)
- `Fledge_Julian` - Julian date of when the first chick fledged (this column was left blank)
- `Clutch_laid` - date of when the first egg was laid
- `Hatch_date` - date of when the first egg hatched
- `Fledge_date` - date of when the first chick fledged
- `Chick_week` - a rough estimate in terms of how old nestlings were, since Red-headed Woodpeckers typically take 26 days to fledge; a chick week of 1 means that the nest is in the first week since hatching, whereas a chick week of 3 indicates nestlings are 18+ days old
- `Exact_age_chick` - age of chicks (in days since hatching) on the day the video was recorded
- `Peeped_chick_count` - number of chicks in the nest on the day the video was recorded
- `Max_number_eggs` - maximum number of eggs that were ever laid for this brood
- `Max_number_chicks` - maximum number of chicks that were ever counted for this brood
- `Number_chick_mortalities` - 
- `Percent_chick_mortalities` -
- `Nest_fate` - 
- `Nest_fate_certainty` - 
- `Chicks_fledged` - 
- `Proportion_fledged` - 
- `Oldest_nestling` - 
- `Priority` - 
- `Nest_ID` - 
- `Ref_combo_1` - 
- `Ref_combo_2` - 
- `Chicks_visible?` - 
- `With_bpk?` - 
- `Bpk_status_1` - 
- `Bpk_status_2` - 
- `Start_time` - 
- `Early_or_late` - 
- `Length_usable` - 
- `Length_discrepancy?` - 
- `BORIS_Observer` - 
- `Length_each` - 
- `Length_total` - 
- `Summary/Notes` - 

### sex_data.csv

- `Band_number` - number on the aluminum band
- `RR` - right leg color combination
    - `S` - aluminum
    - `R` - red
    - `O` - orange
    - `Y` - yellow
    - `G` - green
    - `B` - brown
    - `U` - blue
    - `P` - pink
    - `W` - white
- `LL` - left leg color combination (see letter definitions above)
- `Color_combo`	- 4-band color combination
- `Sex`	- sex of woodpecker
- `Method` - how sex of the woodpecker was determined
    - `DNA` - feather DNA analysis
    - `inferred` - a mate of a woodpecker that was sexed via DNA was assigned the opposite sex
    - `na` - not available
- `Year Sent` - year that the DNA sample was sent to the Animal Genetics lab
- `2016_Roost_ID` - ID of the winter roost for this woodpecker in 2016
- `2017_Nest_ID` - ID of the nest where this woodpecker was breeding in 2017
- `2018_Nest_ID` - ID of the nest where this woodpecker was breeding in 2018; note that if the value is "vagrant", the woodpecker was a hatch year bird or did not belong in that area based on previous observations
- `2018_Resight` - if resighted in 2018, ID of the nearest nest snag
