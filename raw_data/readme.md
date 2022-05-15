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
    - `calling` - woodpecker makes a territorial call, such as "queer!"
- `Behavior_type` 
    - `POINT` - a behavior that happens once; momentary
    - `STATE` - a behavior with a duration
- `Start_sec` - seconds since start time of video that the behavior begins
- `Stop_sec` - seconds since start time of video that the behavior ends
- `Duration_sec` - length of time a behavior was exhibited

### boris_provisioning_preyproofed_bestdata.csv

### brood_contents.csv

### incubation_video.csv

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
