# intro -------------------------------------------------------------------

# Red-headed Woodpecker provisioning models
# By: Lynn Abigail Walter

# Latest updates:
# Nov 2021 - This file replaces "old_provisioning_models.R"

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
# Provisioning models:
#     
#   null model - random effects only
#   base model (nestling char): chickAge+brdSz
#   base model + Psex
#   base model + exogenous factors
#   base model + Psex + exogenous factors
#   base model + Psex*chickAge interaction
#   base model + date*chickAge interaction
#   global model1: Psex*chickAge+brdSz+temp+hab
#   global model2: date*chickAge+brdSz+temp+hab

# libraries ---------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(bbmle)

# read data ---------------------------------------------------------------

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(feeding = feeding_chicks) %>%
  select(-feeding_chicks)

# run models --------------------------------------------------------------

# Why include the linear term?
# https://stats.stackexchange.com/questions/28730/does-it-make-sense-to-add-a-quadratic-term-but-not-the-linear-term-to-a-model/28750#28750

# Why include quadratic AND linear interaction?
# https://stackoverflow.com/questions/47372262/r-interactions-between-independent-variable-and-polynomial-term

#   null model - random effects only
null_model <-
  glmmTMB(
    feeding ~ 
      (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model (nestling char): chickAge+brdSz
base_model <-   
  glmmTMB(
    feeding ~ 
      I(exact_age_chick^2) + exact_age_chick + peeped_chick_count + 
      (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model + Psex
model_sex <-   
  glmmTMB(
    feeding ~ 
      I(exact_age_chick^2) + exact_age_chick + peeped_chick_count + sex + 
      (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model + exogenous factors
model_env <-   
  glmmTMB(
    feeding ~ 
      I(exact_age_chick^2) + exact_age_chick + peeped_chick_count + habitat + 
      std_jdate + (1|brood_id) + (1|subject) + offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model + Psex + exogenous factors
model_sex_env <-   
  glmmTMB(
    feeding ~ 
      I(exact_age_chick^2) + exact_age_chick + peeped_chick_count + sex + 
      habitat + std_jdate + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model + Psex*chickAge interaction
model_sex_interaction <-   
  glmmTMB(
    feeding ~ 
      sex*I(exact_age_chick^2) + 
      sex*exact_age_chick + 
      peeped_chick_count + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

#   base model + date*chickAge interaction
model_date_interaction <-   
  glmmTMB(
    feeding ~ 
      std_jdate*I(exact_age_chick^2) + 
      std_jdate*exact_age_chick + 
      peeped_chick_count + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))
  
#   global model1: Psex*chickAge+brdSz+temp+hab
global_model_sex <- 
  glmmTMB(
    feeding ~ 
      sex*I(exact_age_chick^2) + 
      sex*exact_age_chick + 
      peeped_chick_count + std_jdate + habitat + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))
  
#   global model2: date*chickAge+brdSz+temp+hab
global_model_date <- 
  glmmTMB(
    feeding ~ 
      std_jdate*I(exact_age_chick^2) + 
      std_jdate*exact_age_chick + 
      peeped_chick_count + habitat + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

# compare models ----------------------------------------------------------

ICtab(
  null_model,
  base_model,
  model_sex,
  model_env,
  model_sex_env,
  model_sex_interaction,
  model_date_interaction,
  global_model_sex,
  global_model_date,
  type = c("AIC"), 
  weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE, sort = TRUE)

summary(model_date_interaction) 

# Calculate confidence intervals

confint(model_date_interaction, level = 0.95)

