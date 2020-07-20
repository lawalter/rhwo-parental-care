# libraries ---------------------------------------------------------------

library(glmmTMB)
library(bbmle)

# data --------------------------------------------------------------------

bbyvid <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(feeding = feeding_chicks) %>%
  select(-feeding_chicks)

# best distribution fit ---------------------------------------------------

# Zero inflated negative bomial
modelp_test_nbzi <- 
  glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + (1|brood_id) +
            (1|subject) + offset(log(usable_video)), 
          data = bbyvid, 
          ziformula = ~1, family = nbinom2(link = "log"))

# Negative binomial
modelp_test_nb <- 
  glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + (1|brood_id) +
            (1|subject) + offset(log(usable_video)), 
          data = bbyvid, family = nbinom2(link = "log"))

# Poisson
modelp_test_p <- 
  glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + (1|brood_id) + 
            (1|subject) + offset(log(usable_video)), 
          data = bbyvid, family = poisson(link="log"))

AICtab(modelp_test_nbzi, modelp_test_nb, modelp_test_p)

# linear vs quadratic chick age -------------------------------------------

model0 <- 
  glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + (1|brood_id) + 
            (1|subject) + offset(log(usable_video)), 
          data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model0x <- 
  glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + 
            I(exact_age_chick^2) + (1|brood_id) + (1|subject) + 
            offset(log(usable_video)), data = bbyvid, ziformula = ~1,
          family = nbinom2(link = "log"))

ICtab(model0, model0x,
      type = c("AIC"), weights = TRUE, delta = TRUE, base = TRUE,
      logLik = TRUE, sort = TRUE)

# zero-inflated negative binomial models ----------------------------------

# Model 0: feeding ~ chick_age + brood_size      
# Model 1: feeding ~ chick_age + brood_size + sex 
# Model 2: feeding ~ chick_age + brood_size + sex + habitat
# Model 3: feeding ~ chick_age + brood_size + sex + std_jdate
# Model 4: feeding ~ chick_age + brood_size + std_jdate 
# Model 5: feeding ~ chick_age + brood_size + habitat
# Model 6: feeding ~ chick_age + brood_size + habitat + std_jdate
# Model 1.1i: feeding ~ chick_age*sex + brood_size
# Model 1.2i: feeding ~ chick_age*sex + brood_size + habitat 
# Model 1.3i: feeding ~ chick_age*sex + brood_size + std_jdate 
# Model 1.4i: feeding ~ chick_age*sex + brood_size + habitat + std_jdate 
# Model 4.1i: feeding ~ chick_age*std_jdate + brood_size
# Model 4.2i: feeding ~ chick_age*std_jdate + brood_size + sex
# Model 4.3i: feeding ~ chick_age*std_jdate + brood_size + habitat 
# Model 4.4i: feeding ~ chick_age*std_jdate + brood_size + habitat + sex
# Model 5.1i: feeding ~ chick_age*habitat + brood_size 
# Model 5.2i: feeding ~ chick_age*habitat + brood_size + sex
# Model 5.3i: feeding ~ chick_age*habitat + brood_size + std_jdate
# Model 5.4i: feeding ~ chick_age*habitat + brood_size + std_jdate + sex
# Model 0x: feeding ~ chick_age + brood_size + I(chick_age)^2
# Model 1.1x: feeding ~ chick_age*sex + brood_size + I(chick_age)^2
# Model 1.2x: feeding ~ chick_age*sex + brood_size + habitat + I(chick_age)^2 
# Model 1.3x: feeding ~ chick_age*sex + brood_size + std_jdate + I(chick_age)^2 
# Model 1.4x: feeding ~ chick_age*sex + brood_size + habitat + std_jdate + I(chick_age)^2 
# Model 4.1x: feeding ~ chick_age*std_jdate + brood_size + I(chick_age)^2
# Model 4.2x: feeding ~ chick_age*std_jdate + brood_size + sex + I(chick_age)^2
# Model 4.3x: feeding ~ chick_age*std_jdate + brood_size + habitat + I(chick_age)^2 
# Model 4.4x: feeding ~ chick_age*std_jdate + brood_size + habitat + sex + I(chick_age)^2
# Model 5.1x: feeding ~ chick_age*habitat + brood_size + I(chick_age)^2 
# Model 5.2x: feeding ~ chick_age*habitat + brood_size + sex + I(chick_age)^2
# Model 5.3x: feeding ~ chick_age*habitat + brood_size + std_jdate + I(chick_age)^2
# Model 5.4x: feeding ~ chick_age*habitat + brood_size + std_jdate + sex + I(chick_age)^2

model1 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + habitat + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + habitat + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6 <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + habitat + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

#####

model1.1i <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2i <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + habitat + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3i <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4i <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + habitat + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1i <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2i <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + sex + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3i <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + habitat + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4i <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + habitat + sex + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1i <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2i <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + sex + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3i <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + std_jdate + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4i <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + std_jdate + sex + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

#####

model1x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + habitat + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + sex + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + habitat + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6x <- glmmTMB(feeding ~ exact_age_chick + peeped_chick_count + habitat + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.1x <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2x <- glmmTMB(feeding ~ exact_age_chick*sex + I(exact_age_chick^2) + peeped_chick_count + std_jdate + (1|brood_id) + (1|subject)+ offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3x <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4x <- glmmTMB(feeding ~ exact_age_chick*sex + peeped_chick_count + habitat + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1x <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2x <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3x <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + habitat + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4x <- glmmTMB(feeding ~ exact_age_chick*std_jdate + peeped_chick_count + habitat + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1x <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2x <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3x <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4x <- glmmTMB(feeding ~ exact_age_chick*habitat + peeped_chick_count + std_jdate + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

######
## Swapping placement of the quadratic term

model1x2 <- glmmTMB(feeding ~ peeped_chick_count + sex + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2x2 <- glmmTMB(feeding ~ peeped_chick_count + sex + habitat + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3x2 <- glmmTMB(feeding ~ peeped_chick_count + sex + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4x2 <- glmmTMB(feeding ~ peeped_chick_count + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5x2 <- glmmTMB(feeding ~ peeped_chick_count + habitat + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6x2 <- glmmTMB(feeding ~ peeped_chick_count + habitat + std_jdate + I(exact_age_chick^2) + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.1x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*sex + peeped_chick_count + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*sex + exact_age_chick + peeped_chick_count + std_jdate + (1|brood_id) + (1|subject)+ offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*sex + peeped_chick_count + habitat + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*sex + peeped_chick_count + habitat + std_jdate + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*std_jdate + peeped_chick_count + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*std_jdate + peeped_chick_count + sex + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*std_jdate + peeped_chick_count + habitat + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*std_jdate + peeped_chick_count + habitat + sex + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*habitat + peeped_chick_count + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*habitat + peeped_chick_count + sex + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*habitat + peeped_chick_count + std_jdate + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*habitat + peeped_chick_count + std_jdate + sex + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

# model comparison --------------------------------------------------------

# Comparing all the models
AICtab(model0, model1, model2, model3, model4, model5, model6,  
       model1.1i, model1.2i, model1.3i, model1.4i,
       model4.1i, model4.2i, model4.3i, model4.4i, 
       model5.1i, model5.2i, model5.3i, model5.4i,
       model1x, model2x, model3x, model4x, model5x, model6x,
       model1.1x, model1.2x, model1.3x, model1.4x, 
       model4.1x, model4.2x, model4.3x, model4.4x, 
       model5.1x, model5.2x, model5.3x, model5.4x)

# AIC with more info columns
ICtab(model0, model0x,
      model1x, model2x, model3x, model4x, model5x, model6x,
      model1.1x, model1.2x, model1.3x, model1.4x, 
      model4.1x, model4.2x, model4.3x, model4.4x, 
      model5.1x, model5.2x, model5.3x, model5.4x,
      model1x2, model2x2, model3x2, model4x2, model5x2, model6x2,
      model1.1x2, model1.2x2, model1.3x2, model1.4x2, 
      model4.1x2, model4.2x2, model4.3x2, model4.4x2, 
      model5.1x2, model5.2x2, model5.3x2, model5.4x2,
      type = c("AIC"), weights = TRUE, delta = TRUE, base = TRUE,
      logLik = TRUE, sort = TRUE)

### Detailed AIC table
ICtab(model0, model0x,
      model1x, model2x, model3x, model4x, model5x, model6x,
      model1.1x, model1.2x, model1.3x, model1.4x, 
      model4.1x, model4.2x, model4.3x, model4.4x, model4.1x2,
      model5.1x, model5.2x, model5.3x, model5.4x,
      type = c("AIC"), weights = TRUE, delta = TRUE, base = TRUE,
      logLik = TRUE, sort = TRUE)


summary(model4.1x2)

# Note QAIC is not required if the overdispersion in the dataset has been modelled using zero-inflated models, OLREs, or compound probability distributions. Bolker et al. (2009) and Grueber et al. (2011) provide details of how to calculate these criteria.