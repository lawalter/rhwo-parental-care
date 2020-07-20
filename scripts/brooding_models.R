# intro -------------------------------------------------------------------

### Red-headed Woodpecker brooding duration models
### By: Lynn Abigail Walter

### Latest updates:
# Aug 2019 - After correcting the bbyvid to sum duration of brooding, re-ran models. In the test models, the Gaussian distribution did not converge with brood_id included as a random effect, so I removed it from all of the test models. All of the models with data converged even with brood_id included.

# libraries ---------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(bbmle)


# read data ---------------------------------------------------------------

bbyvid <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() 

# script ------------------------------------------------------------------

# Calculate standardized temperatures
bbyvid <-
  bbyvid %>%
  mutate(
    std_tmax = (tmax - mean(tmax))/sd(tmax),
    brooding_rate = (brooding_min/usable_video)*60)

# Plot distribution - it looks zero-inflated
ggplot(
  bbyvid, aes(x = brooding_min)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Brooding Distribution", 
       x = "Brooding (min)", 
       y = "Count") 

# model fit testing -------------------------------------------------------

# Test which type of model fits best:
# non-zero inflated or zero-inflated gaussian?

testmodelb_gaussian <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    family = gaussian())

testmodelb_zigaussian <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    ziformula = ~ 1,
    family = gaussian())

testmodelb_p_hurdle <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    ziformula = ~ .,
    family = "truncated_poisson")

testmodelb_nb_hurdle <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    ziformula = ~ .,
    family = "truncated_nbinom1")

# Compare models:
AICtab(
  testmodelb_gaussian, 
  testmodelb_zigaussian, 
  testmodelb_p_hurdle, 
  testmodelb_nb_hurdle)
  # Result: The NB hurdle model is best, followed by zero-inflated gaussian

# quadratic fit testing ---------------------------------------------------

# Test to see if brooding fits better with quadratic chick age  
testmodellinear <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    family = gaussian())

testmodelquad <-
  glmmTMB(
    brooding_min ~ I(exact_age_chick ^ 2) + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    family = gaussian())

testmodellinear_hurdle <-
  glmmTMB(
    brooding_min ~ exact_age_chick + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    ziformula = ~ .,
    family = "truncated_nbinom1")

testmodelquad_hurdle <-
  glmmTMB(
    brooding_min ~ I(exact_age_chick ^ 2) + peeped_chick_count + 
      offset(log(usable_video)),
    data = bbyvid,
    ziformula = ~ .,
    family = "truncated_nbinom1")

# The linear hurdle model is best
AICtab(
  testmodellinear, 
  testmodelquad, 
  testmodellinear_hurdle, 
  testmodelquad_hurdle)

# models ------------------------------------------------------------------

# Brooding hurdle models
# Model 0: brooding ~ exact_age_chick + peeped_chick_count
# Model 1: brooding ~ exact_age_chick + peeped_chick_count + sex 
# Model 2: brooding ~ exact_age_chick + peeped_chick_count + sex + habitat
# Model 8: brooding ~ exact_age_chick + peeped_chick_count + std_tmax
# Model 9: brooding ~ exact_age_chick + peeped_chick_count + std_tmax + sex
# Model 11: brooding ~ exact_age_chick + peeped_chick_count + std_tmax + sex + habitat  
# Model 1.1i: brooding ~ exact_age_chick*sex + peeped_chick_count
# Model 1.2i: brooding ~ exact_age_chick*sex + peeped_chick_count + habitat  
# Model 1.5i: brooding ~ exact_age_chick*sex + peeped_chick_count + std_tmax  
# Model 1.7i: brooding ~ exact_age_chick*sex + peeped_chick_count + std_tmax + habitat  
# Model 3.1i: brooding ~ exact_age_chick*peeped_chick_count + habitat + sex
# Model 3.2i: brooding ~ exact_age_chick*peeped_chick_count + habitat 
# Model 3.3i: brooding ~ exact_age_chick*peeped_chick_count + habitat + std_tmax
# Model 3.4i: brooding ~ exact_age_chick*peeped_chick_count + std_tmax
# Model 6.1i: brooding ~ exact_age_chick*std_tmax + peeped_chick_count 
# Model 6.2i: brooding ~ exact_age_chick*std_tmax + peeped_chick_count + sex 
# Model 6.3i: brooding ~ exact_age_chick*std_tmax + peeped_chick_count + habitat  

# Running models
modelhb0 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count  + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count + sex + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb2 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count + sex + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb8 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count + std_tmax + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb9 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count + std_tmax + sex + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb11 <- glmmTMB(brooding_min ~ exact_age_chick + peeped_chick_count + std_tmax + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

#####

modelhb1.1i <- glmmTMB(brooding_min ~ exact_age_chick*sex + peeped_chick_count + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.2i <- glmmTMB(brooding_min ~ exact_age_chick*sex + peeped_chick_count + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.5i <- glmmTMB(brooding_min ~ exact_age_chick*sex + peeped_chick_count + std_tmax + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.5i2 <- glmmTMB(brooding_min ~ exact_age_chick*sex + peeped_chick_count + std_tmax + I(exact_age_chick^2) + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")
## Testing quadratic effect to the top model, but it's not as good (AIC 0.7)


modelhb1.7i <- glmmTMB(brooding_min ~ exact_age_chick*sex + peeped_chick_count + std_tmax + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.1i <- glmmTMB(brooding_min ~ exact_age_chick*peeped_chick_count + habitat + sex + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.2i <- glmmTMB(brooding_min ~ exact_age_chick*peeped_chick_count + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.3i <- glmmTMB(brooding_min ~ exact_age_chick*peeped_chick_count + habitat + std_tmax + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.4i <- glmmTMB(brooding_min ~ exact_age_chick*peeped_chick_count + std_tmax + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb4i <- glmmTMB(brooding_min ~ exact_age_chick*habitat + peeped_chick_count + habitat*sex + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.1i <- glmmTMB(brooding_min ~ exact_age_chick*std_tmax + peeped_chick_count + (1|brood_id)  + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.2i <- glmmTMB(brooding_min ~ exact_age_chick*std_tmax + peeped_chick_count + sex + (1|brood_id)  + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.3i <- glmmTMB(brooding_min ~ exact_age_chick*std_tmax + peeped_chick_count + habitat + (1|brood_id) + offset(log(usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

#####

# Comparing the modelhbs
AICtab(modelhb0,
       modelhb1, modelhb2, modelhb8, modelhb9, modelhb11,
       modelhb1.1i, modelhb1.2i, modelhb1.5i, modelhb1.7i,
       modelhb3.1i, modelhb3.2i, modelhb3.3i, modelhb3.4i, 
       modelhb6.1i, modelhb6.2i, modelhb6.3i)  

ICtab(modelhb0,
      modelhb1, modelhb2, modelhb8, modelhb9, modelhb11,
      modelhb1.1i, modelhb1.2i, modelhb1.5i, modelhb1.7i, 
      modelhb3.2i, modelhb3.3i, modelhb3.4i, 
      modelhb6.1i, modelhb6.2i, modelhb6.3i, 
      type = c("AIC"), weights = TRUE, delta = TRUE, base = TRUE,
      logLik = TRUE, sort = TRUE)


summary(modelhb1.1i) ## Previously best model using incorrect brooding duration values

summary(modelhb1.5i) ## Best model using correct brooding duration values
