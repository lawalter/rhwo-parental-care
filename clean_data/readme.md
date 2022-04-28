## Field definitions for clean data files

### behaviors.csv

- `video_id` - row key
- `brooding_count`
- `cleaning_nest`
- `feeding_chicks`
- `visit_count`
- `brooding_min`
- `poopsearch_min`
- `visit_min`
- `video_number` - key to video session
- `subject` - parent woodpecker ID created from color band combination
- `sex` - sex of parent woodpecker
- `habitat`
- `exact_age_chick`
- `peeped_chick_count`
- `nest_id`
- `brood_id`
- `julian_date`
- `std_jdate`
- `usable_video`
- `year`
- `tmax`

### bpk_status.csv

- `video_number` - key to video session
- `subject` - key and ID created from color band combination for each woodpecker parent 
- `bpk_status`
    - `before` - the video was recorded before the RHWO was GPS tagged
    - `with` - the video was recorded while the RHWO was GPS tagged
    - `after` - the video was recorded after the RHWO was GPS tagged

### incubation.csv

Coming soon.
