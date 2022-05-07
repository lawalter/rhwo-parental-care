## Field definitions for clean data files

In progress.

### behaviors.csv

- `video_id` - row key; each line is per parent per video
- `brooding_count` - number of times the nest was brooded during the video
- `cleaning_nest` - 
- `feeding_chicks` - 
- `visit_count` -
- `brooding_min` - brooding min/hr; brooding_count standardized by usable_video
- `poopsearch_min` - 
- `visit_min` - 
- `video_number` - key to video session
- `subject` - ID created from color band combination for each woodpecker parent 
- `sex` - sex of parent woodpecker
- `habitat` - "Savanna" or "CC" (Closed Canopy)
- `exact_age_chick` - age of chicks in the nest at the time the video was recorded
- `peeped_chick_count` - number of chicks in the nest at the time the video was recorded
- `nest_id` - ID for the dead tree (snag) the nest was located in
- `brood_id` - ID for the nesting attempt (multiple nesting attempts possible per snag per season)
- `julian_date` - Julian date for the day the video was recorded
- `std_jdate` - standardized Julian date (z-score)
- `usable_video` - minutes of video in which behaviors were analyzed 
- `year` - year
- `tmax` - maximum daily temperature, derived from NOAA dataset

### bpk_status.csv

- `video_number` - key to video session
- `subject` - key and ID created from color band combination for each woodpecker parent 
- `bpk_status`
    - `before` - the video was recorded before the RHWO was GPS tagged
    - `with` - the video was recorded while the RHWO was GPS tagged
    - `after` - the video was recorded after the RHWO was GPS tagged

### incubation.csv

- `video_number` - key to video session
- `subject` - ID created from color band combination for each woodpecker parent 
- `sex` - sex of parent woodpecker
- `date` - date (YYYY-MM-DD) that the video was recorded
- `tmax` - maximum temperature for that date, in Celcius
- `incubation_rate` - 
- `usable_length_total` - minutes of video in which behaviors were analyzed 
