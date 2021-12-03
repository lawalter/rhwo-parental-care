# intro -------------------------------------------------------------------

# Red-headed Woodpecker brooding duration models
# By: Lynn Abigail Walter

# Latest updates:
# Nov 2021 - This file replaces "old_brooding_models.R"

# summary -----------------------------------------------------------------

# Variables of interest:
#   
#   nestling char = chick age and brood size
#   exogenous factors = temp/date and habitat
#   parental sex
#   
# Interactions of interest:
#
#   parental sex * chick age --> 
#     males and females may adjust their care differently as nestlings age 
#     (both provisioning and brooding model sets)
#   chick age * date --> chicks earlier in the season may be fed more than later
#     in the season when it is drier and hotter
#   
# Brooding models:
#     
#   null model - random effects only
#   base model (nestling char): chickAge+brdSz
#   base model + Psex
#   base model + exogenous factors
#   base model + Psex + exogenous factors
#   base model + Psex*chickAge interaction
#   global model: Psex*chickAge+brdSz+temp+hab

# libraries ---------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(bbmle)

# read data ---------------------------------------------------------------

# Distance and start time data 

dist_start <-
  read_csv("raw_data/provisioning_video_data.csv") %>%
  filter(Letter == "a") %>%
  select(
    video_number = Video_number,
    start_time = Start_time)

# Behaviors by video

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(video_id, brooding_min, subject, sex, habitat, exact_age_chick,
         peeped_chick_count, nest_id, brood_id, std_jdate, tmax, 
         usable_video, video_number) %>%
  # Calculate standardized temperatures
  mutate(
    std_tmax = (tmax - mean(tmax))/sd(tmax),
    brooding_rate = (brooding_min/usable_video)*60) %>% 
  # Add video start time to dataset from previous datafame
  left_join(dist_start, by = "video_number") %>%
  mutate(
    start_time =
      case_when(
        start_time == "na" ~ NA_character_,
        video_number == "70" ~ "1100",
        video_number == "117" ~ "904",
        TRUE ~ start_time)) %>% 
  mutate(start_time = as.numeric(start_time)) %>% 
  # To eliminate the warnings from glmmTMB
  # "non-integer counts in a truncated_nbinom1 model"
  # Run the following mutate, but it is not needed -- it's fine as dbl
  mutate(brooding_min = as.integer(round(brooding_min)))


# aside -------------------------------------------------------------------

# Notes from LPB:

# Replace few missing values for video start time with the mean:
behaviors <- 
  transform(
    behaviors, 
    start_time = 
      ifelse(
        is.na(start_time), 
        ave(start_time, FUN = function(x) mean(x, na.rm = TRUE)), 
        start_time))

behaviors$scTime <- scale(behaviors$start_time)

# run models --------------------------------------------------------------

# Running models

#   null model - random effects only
null_model <- 
  glmmTMB(
    brooding_min ~ 
      (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model (nestling char): chickAge+brdSz
base_model <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + scTime +
      (1|brood_id) + (1|subject) + offset(log(usable_video)),  
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex
model_sex <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + sex + scTime +
      (1|brood_id) + (1|subject) + offset(log(usable_video)),  
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + exogenous factors
model_env <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + habitat + scTime + 
      std_tmax + (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex + exogenous factors
model_sex_env <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + scTime+ sex + 
      habitat + std_tmax + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)),  
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex*chickAge interaction
model_sex_interaction <- 
  glmmTMB(
    brooding_min ~ 
      sex*exact_age_chick + scTime+
      peeped_chick_count + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   global model1: Psex*chickAge+brdSz+temp+hab
global_model_sex <- 
  glmmTMB(
    brooding_min ~ 
      sex*exact_age_chick + scTime+
      peeped_chick_count + std_tmax + habitat + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)),  
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")


# compare models ----------------------------------------------------------

brooding_aic <- 
  ICtab(
    null_model,
    base_model,
    model_sex,
    model_env,
    model_sex_env,
    model_sex_interaction,
    global_model_sex,
    type = c("AICc"), 
    weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE, sort = TRUE)

write.csv(brooding_aic, "H:/brooding_aic_question.csv")

summary(model_sex_interaction) 

# Calculate confidence intervals

confint(model_sex_interaction, level = 0.95)
