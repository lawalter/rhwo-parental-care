### Red-headed Woodpecker brooding duration models
### By: Lynn Abigail Walter

### Latest updates:
# Aug 2019 - After correcting the bbyvid to sum duration of brooding, re-ran models. In the test models, the Gaussian distribution did not converge with Brood_ID included as a random effect, so I removed it from all of the test models. All of the models with data converged even with Brood_ID included.


# Calculate standardized temperatures
bbyvid$Std_TMAX <- (bbyvid$TMAX - mean(bbyvid$TMAX))/sd(bbyvid$TMAX)
bbyvid$brooding_rate <- (bbyvid$brooding_min/bbyvid$Usable_video)*60

# Plot distribution - it looks zero-inflated
library(ggplot2)
ggplot(
  bbyvid, aes(x=brooding_min)) + 
  geom_histogram(bins = 30) + 
  labs(title="Brooding Distribution", 
       x = "Brooding (min)", 
       y = "Count") 

# Testing which type of model fits best, non-zero inflated or zero-inflated gaussian
library(glmmTMB)
library(bbmle)

testmodelb_gaussian <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, family = gaussian())

testmodelb_zigaussian <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = gaussian())

testmodelb_p_hurdle <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, ziformula= ~., family = "truncated_poisson")

testmodelb_nb_hurdle <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, ziformula= ~., family = "truncated_nbinom1")

# The NB hurdle model is best, followed by zero-inflated gaussian
AICtab(testmodelb_gaussian, testmodelb_zigaussian, testmodelb_p_hurdle, testmodelb_nb_hurdle)


# Testing to see if brooding fits better with quadratic chick age  
testmodellinear <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, family = gaussian())

testmodelquad <- glmmTMB(brooding_min ~ I(Exact_age_chick^2) + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, family = gaussian())

testmodellinear_hurdle <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, ziformula= ~., family = "truncated_nbinom1")

testmodelquad_hurdle <- glmmTMB(brooding_min ~ I(Exact_age_chick^2) + Peeped_chick_count + offset(log(Usable_video)), data = bbyvid, ziformula= ~., family = "truncated_nbinom1")

# The linear hurdle model is best
AICtab(testmodellinear, testmodelquad, testmodellinear_hurdle, testmodelquad_hurdle)



# Brooding hurdle models
# Model 0: brooding ~ Exact_age_chick + Peeped_chick_count
# Model 1: brooding ~ Exact_age_chick + Peeped_chick_count + Sex 
# Model 2: brooding ~ Exact_age_chick + Peeped_chick_count + Sex + Habitat
# Model 8: brooding ~ Exact_age_chick + Peeped_chick_count + Std_TMAX
# Model 9: brooding ~ Exact_age_chick + Peeped_chick_count + Std_TMAX + Sex
# Model 11: brooding ~ Exact_age_chick + Peeped_chick_count + Std_TMAX + Sex + Habitat  
# Model 1.1i: brooding ~ Exact_age_chick*Sex + Peeped_chick_count
# Model 1.2i: brooding ~ Exact_age_chick*Sex + Peeped_chick_count + Habitat  
# Model 1.5i: brooding ~ Exact_age_chick*Sex + Peeped_chick_count + Std_TMAX  
# Model 1.7i: brooding ~ Exact_age_chick*Sex + Peeped_chick_count + Std_TMAX + Habitat  
# Model 3.1i: brooding ~ Exact_age_chick*Peeped_chick_count + Habitat + Sex
# Model 3.2i: brooding ~ Exact_age_chick*Peeped_chick_count + Habitat 
# Model 3.3i: brooding ~ Exact_age_chick*Peeped_chick_count + Habitat + Std_TMAX
# Model 3.4i: brooding ~ Exact_age_chick*Peeped_chick_count + Std_TMAX
# Model 6.1i: brooding ~ Exact_age_chick*Std_TMAX + Peeped_chick_count 
# Model 6.2i: brooding ~ Exact_age_chick*Std_TMAX + Peeped_chick_count + Sex 
# Model 6.3i: brooding ~ Exact_age_chick*Std_TMAX + Peeped_chick_count + Habitat  



# Running models
modelhb0 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count  + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + Sex + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb2 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + Sex + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb8 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + Std_TMAX + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb9 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + Std_TMAX + Sex + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb11 <- glmmTMB(brooding_min ~ Exact_age_chick + Peeped_chick_count + Std_TMAX + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

#####

modelhb1.1i <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + Peeped_chick_count + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.2i <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + Peeped_chick_count + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.5i <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + Peeped_chick_count + Std_TMAX + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb1.5i2 <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + Peeped_chick_count + Std_TMAX + I(Exact_age_chick^2) + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")
## Testing quadratic effect to the top model, but it's not as good (AIC 0.7)


modelhb1.7i <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + Peeped_chick_count + Std_TMAX + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.1i <- glmmTMB(brooding_min ~ Exact_age_chick*Peeped_chick_count + Habitat + Sex + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.2i <- glmmTMB(brooding_min ~ Exact_age_chick*Peeped_chick_count + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.3i <- glmmTMB(brooding_min ~ Exact_age_chick*Peeped_chick_count + Habitat + Std_TMAX + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb3.4i <- glmmTMB(brooding_min ~ Exact_age_chick*Peeped_chick_count + Std_TMAX + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb4i <- glmmTMB(brooding_min ~ Exact_age_chick*Habitat + Peeped_chick_count + Habitat*Sex + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.1i <- glmmTMB(brooding_min ~ Exact_age_chick*Std_TMAX + Peeped_chick_count + (1|Brood_ID)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.2i <- glmmTMB(brooding_min ~ Exact_age_chick*Std_TMAX + Peeped_chick_count + Sex + (1|Brood_ID)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")

modelhb6.3i <- glmmTMB(brooding_min ~ Exact_age_chick*Std_TMAX + Peeped_chick_count + Habitat + (1|Brood_ID) + offset(log(Usable_video)), data = bbyvid, ziformula = ~., family = "truncated_nbinom1")




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
