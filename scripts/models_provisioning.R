###################################################################################        
### START MODELING ################################################################   
###################################################################################
library(glmmTMB)
library(bbmle)

### NEGATIVE BINOMIAL, ZERO INFLATED MODELS ###
# Response: feeding
# Fixed effects: Sex, Exact_age_chick, Peeped_chick_count, Julian_date, Year, Habitat
# Random effects: Brood_ID, Subject
# Offset: Length_total

modelp_test_nbzi <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

modelp_test_nb <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, family = nbinom2(link = "log"))

modelp_test_p <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, family = poisson(link="log"))

AICtab(modelp_test_nbzi, modelp_test_nb, modelp_test_p)

# Negative binomial zero-inflated models w/ crossed random effects
# Running several different models to see what fits best
# Model 0: feeding ~ chick_age + brood_size      
# Model 1: feeding ~ chick_age + brood_size + Sex 
# Model 2: feeding ~ chick_age + brood_size + Sex + Habitat
# Model 3: feeding ~ chick_age + brood_size + Sex + Std_jdate
# Model 4: feeding ~ chick_age + brood_size + Std_jdate 
# Model 5: feeding ~ chick_age + brood_size + Habitat
# Model 6: feeding ~ chick_age + brood_size + Habitat + Std_jdate
# Model 1.1i: feeding ~ chick_age*Sex + brood_size
# Model 1.2i: feeding ~ chick_age*Sex + brood_size + Habitat 
# Model 1.3i: feeding ~ chick_age*Sex + brood_size + Std_jdate 
# Model 1.4i: feeding ~ chick_age*Sex + brood_size + Habitat + Std_jdate 
# Model 4.1i: feeding ~ chick_age*Std_jdate + brood_size
# Model 4.2i: feeding ~ chick_age*Std_jdate + brood_size + Sex
# Model 4.3i: feeding ~ chick_age*Std_jdate + brood_size + Habitat 
# Model 4.4i: feeding ~ chick_age*Std_jdate + brood_size + Habitat + Sex
# Model 5.1i: feeding ~ chick_age*Habitat + brood_size 
# Model 5.2i: feeding ~ chick_age*Habitat + brood_size + Sex
# Model 5.3i: feeding ~ chick_age*Habitat + brood_size + Std_jdate
# Model 5.4i: feeding ~ chick_age*Habitat + brood_size + Std_jdate + Sex
# Model 0x: feeding ~ chick_age + brood_size + I(chick_age)^2
# Model 1.1x: feeding ~ chick_age*Sex + brood_size + I(chick_age)^2
# Model 1.2x: feeding ~ chick_age*Sex + brood_size + Habitat + I(chick_age)^2 
# Model 1.3x: feeding ~ chick_age*Sex + brood_size + Std_jdate + I(chick_age)^2 
# Model 1.4x: feeding ~ chick_age*Sex + brood_size + Habitat + Std_jdate + I(chick_age)^2 
# Model 4.1x: feeding ~ chick_age*Std_jdate + brood_size + I(chick_age)^2
# Model 4.2x: feeding ~ chick_age*Std_jdate + brood_size + Sex + I(chick_age)^2
# Model 4.3x: feeding ~ chick_age*Std_jdate + brood_size + Habitat + I(chick_age)^2 
# Model 4.4x: feeding ~ chick_age*Std_jdate + brood_size + Habitat + Sex + I(chick_age)^2
# Model 5.1x: feeding ~ chick_age*Habitat + brood_size + I(chick_age)^2 
# Model 5.2x: feeding ~ chick_age*Habitat + brood_size + Sex + I(chick_age)^2
# Model 5.3x: feeding ~ chick_age*Habitat + brood_size + Std_jdate + I(chick_age)^2
# Model 5.4x: feeding ~ chick_age*Habitat + brood_size + Std_jdate + Sex + I(chick_age)^2

#maximumm daily temperature

model0 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model0x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

ICtab(model0, model0x,
      type = c("AIC"), weights = TRUE, delta = TRUE, base = TRUE,
      logLik = TRUE, sort = TRUE)

#####

model1 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + Habitat + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Habitat + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6 <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Habitat + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

#####

model1.1i <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2i <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + Habitat + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3i <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4i <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + Habitat + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1i <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2i <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Sex + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3i <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Habitat + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4i <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Habitat + Sex + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1i <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2i <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Sex + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3i <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Std_jdate + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4i <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Std_jdate + Sex + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

#####

model1x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + Habitat + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Sex + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Habitat + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6x <- glmmTMB(feeding ~ Exact_age_chick + Peeped_chick_count + Habitat + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.1x <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2x <- glmmTMB(feeding ~ Exact_age_chick*Sex + I(Exact_age_chick^2) + Peeped_chick_count + Std_jdate + (1|Brood_ID) + (1|Subject)+ offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3x <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4x <- glmmTMB(feeding ~ Exact_age_chick*Sex + Peeped_chick_count + Habitat + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1x <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2x <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3x <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Habitat + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4x <- glmmTMB(feeding ~ Exact_age_chick*Std_jdate + Peeped_chick_count + Habitat + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1x <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2x <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3x <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4x <- glmmTMB(feeding ~ Exact_age_chick*Habitat + Peeped_chick_count + Std_jdate + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

######
## Swapping placement of the quadratic term

model1x2 <- glmmTMB(feeding ~ Peeped_chick_count + Sex + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model2x2 <- glmmTMB(feeding ~ Peeped_chick_count + Sex + Habitat + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model3x2 <- glmmTMB(feeding ~ Peeped_chick_count + Sex + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4x2 <- glmmTMB(feeding ~ Peeped_chick_count + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5x2 <- glmmTMB(feeding ~ Peeped_chick_count + Habitat + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model6x2 <- glmmTMB(feeding ~ Peeped_chick_count + Habitat + Std_jdate + I(Exact_age_chick^2) + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.1x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Sex + Peeped_chick_count + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.2x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Sex + Exact_age_chick + Peeped_chick_count + Std_jdate + (1|Brood_ID) + (1|Subject)+ offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.3x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Sex + Peeped_chick_count + Std_jdate + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model1.4x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Sex + Peeped_chick_count + Habitat + Std_jdate + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.1x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Std_jdate + Peeped_chick_count + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.2x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Std_jdate + Peeped_chick_count + Sex + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.3x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Std_jdate + Peeped_chick_count + Habitat + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model4.4x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Std_jdate + Peeped_chick_count + Habitat + Sex + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.1x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Habitat + Peeped_chick_count + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.2x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Habitat + Peeped_chick_count + Sex + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.3x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Habitat + Peeped_chick_count + Std_jdate + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

model5.4x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Habitat + Peeped_chick_count + Std_jdate + Sex + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

#####
library(bbmle)

# Comparing all the models
AICtab(model0, model1, model2, model3, model4, model5, model6,  
       model1.1i, model1.2i, model1.3i, model1.4i,
       model4.1i, model4.2i, model4.3i, model4.4i, 
       model5.1i, model5.2i, model5.3i, model5.4i,
       model1x, model2x, model3x, model4x, model5x, model6x,
       model1.1x, model1.2x, model1.3x, model1.4x, 
       model4.1x, model4.2x, model4.3x, model4.4x, 
       model5.1x, model5.2x, model5.3x, model5.4x)

### This AIC is the most meaningful
### THESIS TABLE 
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

## Note QAIC is not required if the overdispersion in the dataset has been modelled
#using zero-inflated models, OLREs, or compound probability distributions. Bolker et al.
#(2009) and Grueber et al. (2011) provide details of how to calculate these criteria.

############################################################

## Check models with R package 'performance'
library(performance)

check_collinearity(model1.2i, component = "all")

