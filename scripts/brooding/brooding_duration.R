# instructions ------------------------------------------------------------

# Run wrangling_parentalcare.R; stop after the creation of the 'boris' df

# about -------------------------------------------------------------------

# Calculates summary stats about RHWO brooding duration times

# libraries ---------------------------------------------------------------

library(tidyverse)
library(psych)

# script ------------------------------------------------------------------

brooding_durations <- 
  boris %>%
  # Exclude unknown woodpeckers and chicks
  filter(subject != "unknown" & subject != "chick") %>%
  left_join(
    video_metadata %>% 
      select(observation.id, video_number), 
    by = "observation.id") %>%
  mutate(
    # Create a video_id
    video_id = paste(video_number, subject, sep = '_'),
    # Add duration of behaviors (in minutes)
    duration_min = duration_sec/60) %>% 
  select(behavior, video_number, video_id, duration_min) %>% 
  # Keep on the behaviors of interest
  filter(behavior %in% c("poopsearch", "visit", "brooding")) %>%
  mutate(
    behavior = 
      case_when(
        behavior == "poopsearch" ~ "poopsearch_min",
        behavior == "visit" ~ "visit_min",
        behavior == "brooding" ~ "brooding_min",
        TRUE ~ behavior)) %>% 
  filter(behavior == "brooding_min") %>% 
  mutate(subject = str_extract(video_id, "[A-Z]{4}")) %>% 
  left_join(sex_data, by = "subject") 

brood_m <-
  brooding_durations %>% filter(sex == "male")

describe(brood_m$duration_min)
# mean = 4.34
# sd = 2.94
# n = 53
# se = 0.4
# range = 1.02-15.07

brood_f <-
  brooding_durations %>% filter(sex == "female")

describe(brood_f$duration_min)
# mean = 7.28
# sd = 5.96
# n = 62
# se = 0.76
# range = 1.04-30.21

ggplot(brooding_durations, aes(x = sex, y = duration_min)) + 
  geom_violin()
