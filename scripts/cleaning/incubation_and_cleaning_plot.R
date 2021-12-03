# intro -------------------------------------------------------------------

# Red-headed Woodpecker incubation analysis 
# by: Lynn Abigail Walter

# setup -------------------------------------------------------------------

library(stringr)
library(lubridate)
library(reshape2)
library(car)
library(tidyverse)
library(psych)

# read rds ----------------------------------------------------------------

read_rds('clean_data/incubation.rds') %>%
  list2env(envir = .GlobalEnv)

# incubation --------------------------------------------------------------

behavior_data <-
  boris_data %>% 
  left_join(
    sex_data %>%
      select(subject, rhwo_key, sex),
    by = 'rhwo_key') %>%
  filter(behavior %in% c('in cavity')) %>%
  mutate_if(
    is.character, 
    str_replace_all, 
    pattern = 'in cavity', 
    replacement = 'incubating') %>%
  left_join(
    video_data %>%
      select(video_key, video_number, usable_length, date) %>%
      group_by(video_number) %>%
      mutate(usable_length_total = sum(usable_length)) %>%
      ungroup(),
    by = 'video_key') %>%
  # RHWO must be in cavity longer than 3 min to be considered incubating
  filter(duration_sec > 180) %>% 
  # Sum incubation per parent by video_number. This is so that incubation
  # events spanning two videos are not divided and incorrectly analyzed as
  # two separate (shorter) events:
  group_by(video_number, subject) %>%
  mutate(
    # Calculate incubation rate as a total duration (mins):
    incubation_min = sum(duration_sec[behavior == 'incubating'])/60,
    # Calculate incubation rate as standard minutes per usable length:
    incubation_rate = 
      ifelse(
        is.nan(incubation_min), 
        0, 
        (incubation_min*60)/first(usable_length_total)),
    date = mdy(date)) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup() %>%
  # Remove incubation events done by unidentified parents
  filter(!str_detect(subject, 'unknown')) %>% 
  left_join(
    NOAA_data %>%
      select(date, tmax),
    by = 'date') %>%
  mutate(jdate = yday(date)) %>%
  select(video_number, subject, sex, date, tmax, incubation_rate, 
         usable_length_total)

incubation_data <-
  boris_data %>% 
  left_join(
    sex_data %>%
      select(subject, rhwo_key, sex),
    by = 'rhwo_key') %>%
  filter(behavior %in% c('in cavity')) %>%
  mutate_if(
    is.character, 
    str_replace_all, 
    pattern = 'in cavity', 
    replacement = 'incubating') %>%
  left_join(
    video_data %>%
      select(video_key, video_number, usable_length, date) %>%
      group_by(video_number) %>%
      mutate(usable_length_total = sum(usable_length)) %>%
      ungroup(),
    by = 'video_key') %>%
  # RHWO must be in cavity longer than 3 min to be considered incubating
  filter(duration_sec > 180) %>% 
  # Calculate incubation minutes
  mutate(incubation_min = duration_sec/60) %>%
  # Remove 1 incubation event done by unidentified parent
  filter(!str_detect(subject, 'unknown')) %>% 
  select(video_number, subject, sex, date, incubation_min)

# Write incubation by video dataframe into csv
#write.csv(behavior_data, "clean_data/incubation.csv", row.names = FALSE)


# cleaning ----------------------------------------------------------------

bbyvid <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() 

# Cleaning
bbyvid <-
  bbyvid %>%
  mutate(
    cleaning_rate = (cleaning_nest/usable_video)*60,
    cleaning_rate_perchk = cleaning_rate/peeped_chick_count)

# plot --------------------------------------------------------------------

# Cleaning

cleaning_plot <- 
  ggplot(
  bbyvid, 
  aes(x = sex, y = cleaning_rate_perchk, fill = NULL)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Cleaning rate (per chick per hr)") +
  scale_fill_grey(start = 0.6, end = 0.3) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")


# Boxplot of incubation by sex (black & white)

incubation_plot <- 
  ggplot(
  behavior_data, aes(x = sex, y = incubation_rate)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Incubation rate (min per hr)",
       title = "Figure 2") +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.position = "none") 


cowplot::plot_grid(incubation_plot, cleaning_plot, 
                   labels = 'AUTO',
                   nrow = 2, ncol = 1) +
  ggsave(
    file = "fig2_cleaning_and_incubation.png",
    path ="plots/manuscript_plots/",
    width = 3.5,
    height = 7,
    units = "in",
    dpi = 300)
