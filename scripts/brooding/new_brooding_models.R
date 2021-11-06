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

# Behaviors by video

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(video_id, brooding_min, subject, sex, habitat, exact_age_chick,
         peeped_chick_count, nest_id, brood_id, std_jdate, tmax, 
         usable_video) %>%
  # Calculate standardized temperatures
  mutate(
    std_tmax = (tmax - mean(tmax))/sd(tmax),
    brooding_rate = (brooding_min/usable_video)*60) 
# To eliminate the warnings from glmmTMB
# "non-integer counts in a truncated_nbinom1 model"
# Run the following mutate, but it is not needed -- it's fine as dbl
# mutate(brooding_min = as.integer(round(brooding_min)))

# run models --------------------------------------------------------------

# Running models

#   null model - random effects only
null_model <- 
  glmmTMB(
    brooding_min ~ (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model (nestling char): chickAge+brdSz
base_model <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + 
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex
model_sex <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + sex +
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + exogenous factors
model_env <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + 
      habitat + std_tmax +
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex + exogenous factors
model_sex_env <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick + peeped_chick_count + 
      sex + habitat + std_tmax +
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   base model + Psex*chickAge interaction
model_interaction <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick*sex + peeped_chick_count + 
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

#   global model: Psex*chickAge+brdSz+temp+hab
global_model <- 
  glmmTMB(
    brooding_min ~ 
      exact_age_chick*sex + peeped_chick_count + habitat + std_tmax +
      (1|brood_id) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

# compare models ----------------------------------------------------------

ICtab(
  null_model,
  base_model,
  model_sex,
  model_env,
  model_sex_env,
  model_interaction,
  global_model,
  type = c("AIC"), 
  weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE, sort = TRUE)

summary(model_interaction) 

# Calculate confidence intervals

confint(model_interaction, level = 0.95)
