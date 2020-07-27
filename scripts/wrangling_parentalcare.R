# intro -------------------------------------------------------------------

# Red-headed Woodpecker parental care behavior analysis 
# Determines rates of brooding, provisioning, and cleaning
# by: L. Abigail Walter

# last updates ------------------------------------------------------------

# Jul 2020: Finished modernizing the script using the tidyverse. Found a 
#           discrepancy between new finished dataframe and old one with
#           the calculation of total video lengths. The new .csv is the
#           most accurate
# May 2020: More modernizing 
# Mar 2020: More modernizing
# Feb 2020: Modernized script using the tidyverse
# Jan 2020: Best updated version submitted to github
# Aug 2019: Corrected line creating bbyvid that averaged brooding by 
#            parent instead of summing the video parts
# Apr 2019: Fixed provisioning analysis to removed brooding by males if 
#            followed by cleaning event

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)

# import data -------------------------------------------------------------

# Sex data for individual Red-headed Woodpeckers

sex_data <-
  read.csv('raw_data/sex_data.csv', 
           na.strings = c("", "na", "NA"),
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) %>%
  select(subject = color_combo, sex) 

# Video data about the actual recordings

video_data_initial <- 
  read.csv('raw_data/provisioning_video_data.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) %>%
  # Unselect data that is redundant or unnecessary
  select(-c(letter, part, month, day, year, chick_week, max_number_eggs, 
            max_number_chicks, number_chick_mortalities, 
            percent_chick_mortalities, nest_fate, nest_fate_certainty, 
            chicks_fledged, proportion_fledged, priority, chicks_visible., 
            with_bpk., bpk_status_1, bpk_status_2, start_time, 
            early_or_late, length_discrepancy., boris_observer, 
            length_each, length_total, summary.notes))

# Behavior data from videos scored in BORIS

boris_initial <- 
  read.csv('raw_data/boris_provisioning_preyproofed_bestdata.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) %>%
  arrange(observation.id, start_sec) 

# NOAA weather data

NOAA_data <-
  read.csv('raw_data/NOAA_weather_data.csv', 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  # Calculate Julian dates for the NOAA data
  mutate(
    good_date = as.Date(DATE, "%Y-%m-%d"),
    julian_date = lubridate::yday(good_date),
    yr = str_sub(DATE, 1, 4),
    jdayyr = paste(julian_date, yr, sep = '.')) %>%
  select(tmax = TMAX, jdayyr)

# tidying -----------------------------------------------------------------

# Total sample dataframe: make a df with total sample size of n = 288
# Both parents do not participate in all videos (without this step, the 
# data does not fill n = 288)

total_sample <-
  video_data_initial %>%
  select(video_number, ref_combo_1, ref_combo_2) %>%
  mutate(
    ID1 = paste(video_number, ref_combo_1, sep = '_'),
    ID2 = paste(video_number, ref_combo_2, sep = '_')) %>%
  select(-c(video_number, ref_combo_1, ref_combo_2)) %>%
  gather() %>%
  select(-key, video_id = value) %>%
  distinct()

# cleaning table ----------------------------------------------------------

# Calculate the time difference between stop time of 'in cavity' and the 
# time of cleaning event

cleaning <-
  boris_initial %>%
  select(-c(modifier_general, modifier_specific)) %>%
  filter(behavior == "in cavity" | behavior == "cleaning nest") %>%
  group_by(observation.id) %>%
  mutate(
    time_lag = lead(stop_sec),
    diff_sec = time_lag - stop_sec,
    behavior_lag = lead(behavior)) %>%
  ungroup() %>%
  mutate(
    behavior_new = 
      case_when(
        # If a RHWO leaves with a fecal sac, it's 'poopsearch'
        # If a RHWO enters the cavity for <= 60 sec, it's a visit
        # If a RHWO enters the cavity for > 60 sec, it's brooding
        (behavior == "in cavity" & diff_sec < 1) ~ 'poopsearch',
        (behavior == "in cavity" & duration_sec <= 60) ~ 'visit',
        (behavior == 'in cavity' & duration_sec > 60) ~ 'brooding',
        TRUE ~ behavior)) %>% 
  select(-behavior) %>%
  select(1:3, behavior = behavior_new, 5:7)

# Merge final classifications of cleaning, visiting, brooding, and 
# poopsearch with original data
boris <- 
  cleaning %>%
  bind_rows(
    boris_initial %>% 
      filter(behavior != "in cavity" & behavior != "cleaning nest") %>%
      select(-c(modifier_general, modifier_specific, behavior_type))) 

# behavior table ----------------------------------------------------------

# Point behaviors that can be counted

count_behaviors <- 
  # Start with BORIS scored behaviors that were just cleaned/classified
  boris %>%
  filter(subject != 'unknown' & subject != 'chick') %>%
  left_join(
    video_data_initial %>% 
      select(observation.id, video_number), 
    by = 'observation.id') %>%
  mutate(
    video_id = paste(video_number, subject, sep = '_')) %>% 
  select(-c(observation.id, subject, observation_id_boris, start_sec, 
            stop_sec, duration_sec)) %>%
  filter(
    behavior %in% 
      c('cleaning nest', 
        'feeding chicks', 
        'visit', 
        'brooding')) %>%
  mutate(
    times = 1,
    behavior = 
      case_when(
        behavior == 'cleaning nest' ~ 'cleaning_nest',
        behavior == 'feeding chicks' ~ 'feeding_chicks',
        behavior == 'visit' ~ 'visit_count',
        behavior == 'brooding' ~ 'brooding_count',
        TRUE ~ behavior)) %>%
  group_by(video_id, behavior) %>%
  summarize(sum_count = sum(times)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = behavior, 
    values_from = sum_count)

# State behaviors that have a duration

duration_behaviors <- 
  # Start with BORIS scored behaviors that were just cleaned/classified
  boris %>%
  filter(subject != 'unknown' & subject != 'chick') %>%
  left_join(
    video_data_initial %>% 
      select(observation.id, video_number), 
    by = 'observation.id') %>%
  mutate(
    video_id = paste(video_number, subject, sep = '_'),
    duration_min = duration_sec/60) %>% 
  select(-c(observation.id, subject, observation_id_boris, start_sec, 
            stop_sec, duration_sec)) %>%
  filter(behavior %in% c('poopsearch', 'visit', 'brooding')) %>%
  mutate(
    behavior = 
      case_when(
        behavior == 'poopsearch' ~ 'poopsearch_min',
        behavior == 'visit' ~ 'visit_min',
        behavior == 'brooding' ~ 'brooding_min',
        TRUE ~ behavior)) %>%
  group_by(video_id, behavior) %>%
  # Summarize the durations of behaviors and convert from sec to minutes
  summarize(sum_durations = sum(duration_min)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = behavior, 
    values_from = sum_durations)

# Combining the state and point behaviors

behaviors <-
  total_sample %>%
  left_join(count_behaviors, by = 'video_id') %>%
  left_join(duration_behaviors, by = 'video_id') %>%
  # Remove PNA5A1b because it is the only wetland nest
  filter(!str_detect(video_id, "SROB")) %>%
  arrange(desc(video_id)) %>%
  mutate(
    video_number = str_extract(video_id, "^[0-9]{1,3}"),
    video_number = as.numeric(video_number),
    subject = str_remove(video_id, "^[0-9]{1,3}\\_")) %>%
  # Merge in the sex data
  left_join(sex_data, by = "subject") %>%
  # Merge in nest data
  left_join(
    video_data_initial %>%
      select(video_number, date, habitat, exact_age_chick, 
             peeped_chick_count, nest_id, brood_id) %>%
      distinct(), 
    by = "video_number") %>%
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    julian_date = yday(date),
    std_jdate = (julian_date - mean(julian_date))/sd(julian_date)) %>%
  # Merge in usable_length, but first sum all the parts!
  left_join(
    video_data_initial %>%
      select(video_number, length_usable) %>% ## hither
      group_by(video_number) %>%
      summarize(usable_video = sum(length_usable)) %>%
      ungroup(), 
    by = "video_number") %>%
  # Add weather data
  mutate(
    year = str_sub(date, 1, 4),
    jdayyr = paste(julian_date, year, sep = '.')) %>%
  left_join(NOAA_data, by = 'jdayyr') %>%
  select(-c(date, jdayyr)) %>%
  # Set behavior NAs to zero
  replace(is.na(.), 0)
  
# write csv ---------------------------------------------------------------

write.csv(behaviors, "clean_data/behaviors.csv")       
