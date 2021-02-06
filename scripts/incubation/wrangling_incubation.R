# intro -------------------------------------------------------------------

# Red-headed Woodpecker incubation data tidying 
# by: Lynn Abigail Walter

# libraries ---------------------------------------------------------------

library(stringr)
library(lubridate)
library(reshape2)
library(car)
library(tidyverse)

# import data -------------------------------------------------------------

c('raw_data/sex_data.csv', 
  'raw_data/incubation_video.csv',
  'raw_data/boris_incubation_data.csv',
  'raw_data/NOAA_weather_data.csv') %>%
  map(
    function(x){
      read_csv(
        #file = gsheet2text(x, format='csv'),
        x,
        na = c("", "na", "NA", "not sent")) %>%
        set_names(str_replace_all(names(.), '\\?', '')) %>%
        set_names(str_replace_all(names(.), '\\/', '_')) %>%
        set_names(names(.) %>% tolower()) %>%
        # dates for the video data:
        {if(str_detect(x, 'BpH6M')) {
          mutate_at(
            .,
            vars(date, clutch_laid, hatch_date),
            function(y) as.Date(y, "%m/%d/%Y")) %>%
            select(-c(month, day, year, early_or_late))
        } else .} %>%
        # Remove stop_sec:
        {if(str_detect(x, '1KhdK')) {
          select(., -c(stop_sec))
        } else .}
    }) %>% 
  set_names(c('sex_data', 
              'video_data_initial', 
              'boris_data', 
              'NOAA_data')) %>%
  list2env(envir = .GlobalEnv)

# tidying -----------------------------------------------------------------

sex_data <-
  sex_data %>%
  rename(
    subject = color_combo,
    year_sent = `year sent`,
    roost_id_2016 = `2016_roost_id`,
    nest_id_2017 = `2017_nest_id`,
    nest_id_2018 = `2018_nest_id`,
    resight_2018 = `2018_resight`)

sexes <-
  sex_data %>%
  select(band_number, subject, sex, method, year_sent, roost_id_2016, 
         nest_id_2017, nest_id_2018, resight_2018) %>%
  filter(subject != "GSRO") %>%
  mutate(
    subject = ifelse(
      !str_detect(subject, 'mate'),
      str_replace_all(subject, '[1-9|_]', ''),
      subject)) %>%
  distinct %>%
  mutate(rhwo_key = paste0('rhwo_', row_number())) %>%
  select(rhwo_key, subject, band_number, sex:year_sent)

snag_locations <-
  sex_data %>%
  select('roost_id_2016', 
         'resight_2018', 
         'nest_id_2017', 
         'nest_id_2018') %>%
  stack() %>%
  select(location = values) %>%
  filter(!is.na(location)) %>%
  distinct() %>%
  arrange(location) %>%
  mutate(snag_key = paste0('snag_', row_number()),
         latitude = NA,
         longitude = NA) %>%
  select(snag_key, location, latitude, longitude) %>%
  as_tibble()

bird_locations <-
  map_dfr(
    c('roost_id_2016', 'resight_2018', 'nest_id_2017', 'nest_id_2018'),
    function(x){
      sex_data %>%
        mutate(
          year = str_extract(x, '[0-9]{4}') %>%
            as.numeric(),
          id = str_replace_all(x, '[0-9|_]', '')) %>%
        select(band_number, year, id, x) %>%
        na.omit() %>%
        set_names(
          'band_number',
          'year',
          'type',
          str_replace_all(x, '[0-9|_]', '')
        )}) %>%
  mutate(
    location = paste(roostid, resight, nestid) %>%
      str_replace_all( 'NA', '') %>%
      str_trim()) %>% 
  select(-c(roostid, nestid, resight)) %>%
  left_join(snag_locations, by = 'location') %>%
  distinct() %>%
  dcast(., band_number + year ~ type, value.var = "snag_key", 
        fun.aggregate= function(x) {paste(x, collapse = " ")}) %>% 
  mutate_all(na_if,"") %>%
  as_tibble() %>%
  arrange(band_number, year) %>%
  left_join(., sexes) %>%
  mutate(bird_loc_key = paste0('birdloc_', row_number())) %>%
  select(c(bird_loc_key, rhwo_key, roostid, nestid, resight, year))

bpk_data <-
  video_data_initial %>%
  select(ref_combo = ref_combo_1, date, bpk_status = bpk_status_1) %>%
  bind_rows(
    video_data_initial %>%
      select(ref_combo = ref_combo_2, date, bpk_status = bpk_status_2)
  ) %>%
  distinct() %>%
  mutate(
    bpk_status = case_when(
      str_detect(bpk_status, 'before') ~ 'before',
      str_detect(bpk_status, 'after') ~ 'after',
      str_detect(bpk_status, 'with') ~ 'with',
      TRUE ~ bpk_status)
  ) %>%
  left_join(
    sexes %>%
      select(rhwo_key, subject),
    by = c('ref_combo' = 'subject')
  ) %>%
  mutate(bpk_key = paste0('bpk_', row_number())) %>%
  select(bpk_key, rhwo_key, date, bpk_status)

brood_data <-
  video_data_initial %>%
  select(brood_id, location = nest_id, ta, hatch_date, clutch_laid, 
         egg_count) %>%
  distinct() %>%
  left_join(
    snag_locations %>%
      select(snag_key, location),
    by = 'location') %>%
  mutate(brood_key = paste0('brood_', row_number())) %>%
  select(brood_key, snag_key, brood_id:egg_count) 

nest_parents <-
  video_data_initial %>%
  select(brood_id, ref_combo_1, ref_combo_2) %>% 
  mutate(ref_combo_2 = ifelse(
    ref_combo_2 == 'unbanded',
    paste0(ref_combo_1, '_mate'),
    ref_combo_2
  )) %>%
  gather('rc', 'subject', ref_combo_1:ref_combo_2) %>%
  select(-rc) %>%
  distinct() %>% 
  arrange(brood_id) %>%
  mutate(parent_id = paste0('parent_', row_number())) %>%
  left_join(
    sexes %>%
      select(subject, rhwo_key),
    by = "subject") %>%
  select(parent_id, rhwo_key, brood_id) %>%
  left_join(
    brood_data %>%
      select(brood_key, brood_id),
    by = 'brood_id'
  ) %>%
  select(-brood_id)

video_data <-
  video_data_initial %>%
  select(observation.id, usable_length, video_number, part, date, brood_id, 
         start_time:summary_notes) %>%
  left_join(
    brood_data %>%
      select(brood_key, brood_id), 
    by = 'brood_id') %>%
  select(-brood_id) %>%
  mutate(
    video_key = paste0('video_', row_number())) %>%
  select(video_key, brood_key, observation.id:summary_notes)

boris <-
  boris_data %>%
  left_join(
    video_data %>%
      select(video_key, observation.id),
    by = c('observation.id')) %>%
  left_join(
    sexes %>%
      select(subject, rhwo_key),
    by = 'subject') %>%
  mutate(boris_key = paste0('boriskey_', row_number())) %>%
  select(boris_key, video_key, rhwo_key, observation.id, 
         behavior:duration_sec) 

# write tidy tables to rds ------------------------------------------------

list(
  bird_locations, 
  boris, 
  bpk_data, 
  brood_data, 
  snag_locations, 
  nest_parents,
  NOAA_data, 
  sexes, 
  video_data) %>%
  set_names(
    c('bird_locations', 
      'boris_data', 
      'bpk_data', 
      'brood_data',
      'snag_locations', 
      'nest_parents', 
      'NOAA_data', 
      'sex_data',
      'video_data')) %>%
  write_rds('clean_data/incubation.rds')
