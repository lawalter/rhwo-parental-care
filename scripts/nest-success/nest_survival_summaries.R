
# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)

# import data -------------------------------------------------------------

# Video data about the actual recordings/nests

video_data_initial <- 
  read.csv('raw_data/provisioning_video_data.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) 

# nest survival summary percents ------------------------------------------

video_data_initial %>%
  mutate(
    proportion_hatched = max_number_chicks/max_number_eggs,
    percent_chick_mortalities = number_chick_mortalities/max_number_chicks,
    proportion_fledged = chicks_fledged/max_number_chicks) %>%
  select(
    subject, 
    brood_id, 
    proportion_hatched, 
    proportion_fledged, 
    percent_chick_mortalities) %>%
  distinct() %>%
  arrange(desc(proportion_hatched))