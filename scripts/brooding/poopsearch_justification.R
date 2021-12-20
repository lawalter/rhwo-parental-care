# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# import data -------------------------------------------------------------

# Sex data for individual Red-headed Woodpeckers

sex_data <-
  read_csv("raw_data/sex_data.csv", na = c("", "na", "NA")) %>%
  # Change field names to all lowercase
  set_names(
    names(.) %>% 
      tolower()) %>%
  select(subject = color_combo, sex) 

# Video metadata

video_metadata <- 
  read_csv("raw_data/provisioning_video_data.csv", na = c("", "na", "NA")) %>%
  # Change field names to all lowercase, remove "?"s and "/"s
  set_names(
    names(.) %>% 
      tolower() %>% 
      str_remove(., "\\?") %>% 
      str_remove(., "\\/")) %>%
  # Unselect data that is redundant or unnecessary
  select(-c(letter, part, month, day, year, chick_week, max_number_eggs, 
            max_number_chicks, number_chick_mortalities, 
            percent_chick_mortalities, nest_fate, nest_fate_certainty, 
            chicks_fledged, proportion_fledged, priority, chicks_visible, 
            with_bpk, bpk_status_1, bpk_status_2, start_time, 
            early_or_late, length_discrepancy, boris_observer, 
            length_each, length_total, summarynotes))

# Backpack data

bpk_status <- 
  read_csv("raw_data/provisioning_video_data.csv", na = c("", "na", "NA")) %>%
  # Change field names to all lowercase, remove "?"s and "/"s
  set_names(
    names(.) %>% 
      tolower() %>% 
      str_remove(., "\\?") %>% 
      str_remove(., "\\/")) %>%
  # Select fields related to backpacks
  select(video_number, bpk_status_1, bpk_status_2) %>%
  distinct() %>%
  pivot_longer(
    cols = starts_with("bpk"),
    names_to = "bpk_status",
    values_to = "status") %>%
  select(-bpk_status) %>%
  separate(status, c("subject", "bpk_status"))

# Behavior data from videos scored in BORIS

boris_initial <- 
  read_csv(
    "raw_data/boris_provisioning_preyproofed_bestdata.csv", 
    na = c("", "na", "NA")) %>%
  # Change field names to all lowercase
  set_names(
    names(.) %>% 
      tolower()) %>%
  arrange(observation.id, start_sec) 

# NOAA weather data

NOAA_data <-
  read_csv("raw_data/NOAA_weather_data.csv") %>%
  # Calculate Julian dates for the NOAA data
  mutate(
    good_date = as.Date(DATE, "%Y-%m-%d"),
    julian_date = lubridate::yday(good_date),
    yr = str_sub(DATE, 1, 4),
    jdayyr = paste(julian_date, yr, sep = '.')) %>%
  select(tmax = TMAX, jdayyr)

# tidying -----------------------------------------------------------------

# Total sample dataframe: make a df with total sample size of n = 288
# Both parents do not participate in all videos (without this step, the data
# does not fill n = 288)

total_sample <-
  video_metadata %>%
  select(video_number, ref_combo_1, ref_combo_2) %>%
  mutate(
    ID1 = paste(video_number, ref_combo_1, sep = '_'),
    ID2 = paste(video_number, ref_combo_2, sep = '_')) %>%
  select(-c(video_number, ref_combo_1, ref_combo_2)) %>%
  gather() %>%
  select(-key, video_id = value) %>%
  distinct()

# cleaning table ----------------------------------------------------------

# Calculate the time difference between stop time of 'in cavity' and the time of
# cleaning event

cleaning <-
  boris_initial %>%
  select(-c(modifier_general, modifier_specific)) %>%
  filter(behavior == "in cavity" | behavior == "cleaning nest") %>%
  group_by(observation.id) %>%
  mutate(
    diff_sec = lead(stop_sec) - stop_sec,
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
  select(
    observation_id_boris,
    observation.id,
    subject, 
    behavior = behavior_new, 
    start_sec,
    stop_sec,
    duration_sec)

# plots and means ---------------------------------------------------------

cleaning %>% 
  filter(behavior != "cleaning nest") %>% 
  mutate(duration_min = duration_sec/60) %>% 
  left_join(sex_data, by = "subject") %>% 
  ggplot(aes(x = behavior, y = duration_min, fill = sex)) +
  geom_boxplot()

cleaning %>% 
  filter(behavior == "brooding") %>% 
  mutate(duration_min = duration_sec/60) %>% 
  left_join(sex_data, by = "subject") %>% 
  select(subject, sex, duration_min) %>% 
  group_by(sex) %>% 
  summarize(dur_mean = mean(duration_min),
            n = n()) %>% 
  ungroup()


# without poopsearch ------------------------------------------------------

cleaning2 <-
  boris_initial %>%
  select(-c(modifier_general, modifier_specific)) %>%
  filter(behavior == "in cavity" | behavior == "cleaning nest") %>%
  group_by(observation.id) %>%
  mutate(
    diff_sec = lead(stop_sec) - stop_sec,
    behavior_lag = lead(behavior)) %>%
  ungroup() %>%
  mutate(
    behavior_new = 
      case_when(
        # If a RHWO leaves with a fecal sac, it's 'poopsearch'
        # If a RHWO enters the cavity for <= 60 sec, it's a visit
        # If a RHWO enters the cavity for > 60 sec, it's brooding
        #(behavior == "in cavity" & diff_sec < 1) ~ 'poopsearch',
        (behavior == "in cavity" & duration_sec <= 60) ~ 'visit',
        (behavior == 'in cavity' & duration_sec > 60) ~ 'brooding',
        TRUE ~ behavior)) %>% 
  select(-behavior) %>%
  select(
    observation_id_boris,
    observation.id,
    subject, 
    behavior = behavior_new, 
    start_sec,
    stop_sec,
    duration_sec)

cleaning2 %>% 
  filter(behavior != "cleaning nest") %>% 
  mutate(duration_min = duration_sec/60) %>% 
  left_join(sex_data, by = "subject") %>% 
  ggplot(aes(x = behavior, y = duration_min, fill = sex)) +
  geom_boxplot()

cleaning2 %>% 
  filter(behavior == "brooding") %>% 
  mutate(duration_min = duration_sec/60) %>% 
  left_join(sex_data, by = "subject") %>% 
  select(subject, sex, duration_min) %>% 
  group_by(sex) %>% 
  summarize(dur_mean = mean(duration_min),
            n = n()) %>% 
  ungroup()
