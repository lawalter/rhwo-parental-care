# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# import data -------------------------------------------------------------

# 2018 nest check data

nest_checks_2018 <- 
  read_csv("raw_data/nest_checks_abbreviated_2018.csv") %>%
  rename_all(~tolower(.)) %>% 
  select(nest_id, contains("date")) 
 
# frequency ---------------------------------------------------------------

check_freqs <- 
  nest_checks_2018 %>% 
  mutate(
    across(contains("date"),
    mdy)) %>% 
  mutate(
    d2d1 = ifelse(!is.na(date_1) & !is.na(date_2), date_2 - date_1, NA),
    d3d2 = ifelse(!is.na(date_2) & !is.na(date_3), date_3 - date_2, NA),
    d4d3 = ifelse(!is.na(date_3) & !is.na(date_4), date_4 - date_3, NA),
    d5d4 = ifelse(!is.na(date_4) & !is.na(date_5), date_5 - date_4, NA),
    d6d5 = ifelse(!is.na(date_5) & !is.na(date_6), date_6 - date_5, NA),
    d7d6 = ifelse(!is.na(date_6) & !is.na(date_7), date_7 - date_6, NA),
    d8d7 = ifelse(!is.na(date_7) & !is.na(date_8), date_8 - date_7, NA),
    d9d8 = ifelse(!is.na(date_8) & !is.na(date_9), date_9 - date_8, NA),
    d10d9 = ifelse(!is.na(date_9) & !is.na(date_10), date_10 - date_9, NA),
    d11d10 = ifelse(!is.na(date_10) & !is.na(date_11), date_11 - date_10, NA)) %>% 
  select(nest_id, d2d1:d11d10) 

mean_checks <- rowMeans(check_freqs %>% select(-nest_id), na.rm = T)

mean_all <- mean(mean_checks)

library(psych)
describe(mean_checks)
