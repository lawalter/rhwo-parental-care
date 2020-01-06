## Red-headed Woodpecker incubation analysis 
## By: Lynn Abigail Walter
## git push origin master

library(gsheet)
library(stringr)
library(lubridate)
library(reshape2)
library(car)
library(tidyverse)

# IMPORT DATA -------------------------------------------------------------

c('https://drive.google.com/open?id=1E-BLyxQ-7hMJN7JVMbKbAftsBCQxjFB-ZVDEisfNc5c',
  'https://docs.google.com/spreadsheets/d/1Y5mKHQvY03K8YGkt5nvh3NAj93UiVhInpIx1NjBpH6M/edit?usp=sharing',
  'https://docs.google.com/spreadsheets/d/1KhdKjy8Fy4KrejdrwkJXj1ARfzpgg35g2-a9I1b6vw4/edit?usp=sharing',
  'https://drive.google.com/open?id=1hKlT1dcAO6jA1DkgFUhQ61ZZsCLMhsfN12oiXzfj6go') %>%
  map(
    function(x){
      read_csv(
        file = gsheet2text(x, format='csv'),
        na = c("", "na", "NA", "not sent")) %>%
        set_names(str_replace_all(names(.), '\\?', '')) %>%
        set_names(str_replace_all(names(.), '\\/', '_')) %>%
        # Dates for the video data:
        {if(str_detect(x, 'BpH6M')) {
          mutate_at(
            .,
            vars(Date, Clutch_laid, Hatch_date),
            function(y) as.Date(y, "%m/%d/%Y")) %>%
            select(-c(Month, Day, Year, Early_or_late))
        } else .} %>%
        # Remove Stop_sec:
        {if(str_detect(x, '1KhdK')) {
          select(., -c(Stop_sec))
        } else .}
    }) %>% 
  set_names(c('sex_data', 'video_data_initial', 'boris_data', 'NOAA_data')) %>%
  list2env(envir = .GlobalEnv)


# DATA TIDYING ------------------------------------------------------------

sexes <-
  sex_data %>%
  select(Band_number, Subject = Color_combo, Sex, Method, Year_sent = `Year Sent`) %>%
  filter(Subject != "GSRO") %>%
  mutate(
    Subject = ifelse(
      !str_detect(Subject, 'mate'),
      str_replace_all(Subject, '[1-9|_]', ''),
      Subject
    )) %>%
  distinct %>%
  mutate(rhwo_key = paste0('rhwo_', row_number())) %>%
  select(rhwo_key, Subject, Band_number, Sex: Year_sent)

locations <-
  sex_data %>%
  select('2016_Roost_ID', '2017_Nest_ID', '2018_Nest_ID', '2018_Resight') %>%
  stack() %>%
  select(locations = values) %>%
  na.omit() %>%
  distinct() %>%
  mutate(location_key = paste0('loc_', row_number())) %>%
  select(location_key, locations) 

bird_locations <-
  map_dfr(
    c('2016_Roost_ID', '2018_Resight', '2017_Nest_ID', '2018_Nest_ID'),
    function(x){
      sex_data %>%
        mutate(
          year = str_extract(x, '^[0-9]{1,4}') %>%
            as.numeric())      %>%
        mutate(id = str_replace_all(x, '[0-9|_]', '')) %>%
        select(Band_number, year, id, x) %>%
        na.omit() %>%
        set_names(
          'Band_number',
          'year',
          'id',
          str_replace_all(x, '[0-9|_]', '')
        )}) %>%
  mutate(
    locations = paste(RoostID, Resight, NestID) %>%
      str_replace_all( 'NA', '') %>%
      str_trim()) %>%
  left_join(., locations) %>%
  select(-c(RoostID, NestID, Resight)) %>%
  distinct() %>%
  dcast(., Band_number + year ~ id, value.var = "location_key", 
        fun.aggregate=function(x) paste(x, collapse = " ")) %>% 
  mutate_all(na_if,"") %>%
  as_tibble() %>%
  arrange(Band_number, year) %>%
  left_join(., sexes) %>%
  mutate(bird_loc_key = paste0('birdloc_', row_number())) %>%
  select(c(bird_loc_key, rhwo_key, RoostID, NestID, Resight, year))

bpk_data <-
  video_data_initial %>%
  select(Ref_combo = Ref_combo_1, Date, Bpk_status = Bpk_status_1) %>%
  bind_rows(
    video_data_initial %>%
      select(Ref_combo = Ref_combo_2, Date, Bpk_status = Bpk_status_2)
  ) %>%
  distinct() %>%
  mutate(
    Bpk_status = case_when(
      str_detect(Bpk_status, 'before') ~ 'before',
      str_detect(Bpk_status, 'after') ~ 'after',
      str_detect(Bpk_status, 'with') ~ 'with',
      TRUE ~ Bpk_status)
  ) %>%
  left_join(
    sexes %>%
      select(rhwo_key, Subject),
    by = c('Ref_combo' = 'Subject')
  ) %>%
  mutate(Bpk_key = paste0('bpk_', row_number())) %>%
  select(Bpk_key, rhwo_key, Date, Bpk_status)

brood_data <-
  video_data_initial %>%
  select(Brood_ID, Nest_ID, TA, Hatch_date, Clutch_laid, Egg_count) %>%
  distinct() %>%
  mutate(Brood_key = paste0('brood_', row_number())) %>%
  select(Brood_key, Brood_ID:Egg_count) %>%
  distinct()

nest_parents <-
  video_data_initial %>%
  select(Brood_ID, Ref_combo_1, Ref_combo_2) %>% 
  mutate(Ref_combo_2 = ifelse(
    Ref_combo_2 == 'unbanded',
    paste0(Ref_combo_1, '_mate'),
    Ref_combo_2
  )) %>%
  gather('rc', 'Subject', Ref_combo_1:Ref_combo_2) %>%
  select(-rc) %>%
  distinct() %>% 
  arrange(Brood_ID) %>%
  mutate(Parent_ID = paste0('parent_', row_number())) %>%
  left_join(sexes %>%
              select(Subject, rhwo_key)) %>%
  select(Parent_ID, rhwo_key, Brood_ID) %>%
  left_join(
    brood_data %>%
      select(Brood_key, Brood_ID),
    by  = 'Brood_ID'
  ) %>%
  select(-Brood_ID)

video_data <-
  video_data_initial %>%
  select(Observation.id, Usable_length,Video_number, Part, Date, Brood_ID, Start_time:Summary_Notes) %>%
  left_join(
    brood_data %>%
      select(Brood_key, Brood_ID), 
    by = 'Brood_ID') %>%
  select(-Brood_ID) %>%
  mutate(
    video_key = paste0('video_', row_number())) %>%
  select(video_key, Brood_key, Observation.id:Summary_Notes)

boris_data <-
  boris_data %>%
  left_join(video_data %>%
              select(video_key, Observation.id)) %>%
  left_join(sexes %>%
              select(rhwo_key, Subject)) %>%
  mutate(boris_key = paste0('boriskey_', row_number())) %>%
  select(boris_key, video_key, rhwo_key, Behavior:Duration_sec) 

# Write tidy tables to rds

list(
  bird_locations = bird_locations,
  boris_data = boris_data,
  bpk_data = bpk_data,
  brood_data = brood_data,
  locations = locations,
  nest_parents = nest_parents,
  NOAA_data = NOAA_data,
  sex_data = sexes,
  video_data = video_data
) %>%
  write_rds('cleaned_data/incubation.rds')
