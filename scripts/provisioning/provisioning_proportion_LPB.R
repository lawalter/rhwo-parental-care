library(readr)
bbyvid <- read_csv("scripts/provisioning/final_data_behavior_byvideo.csv")
bbyvid$X1 <- NULL

library(tidyverse)
sub<- select(bbyvid, 5, 7, 9, 12, 16, 17, 29, 33, 30, 37)
sub<-unique(sub)
fem<-filter(sub, Sex == "female")
fem$Brood_ID<-as.factor(fem$Brood_ID)
summary(fem$Brood_ID)

tot<- bbyvid %>% group_by(Brood_ID, Julian_date) %>% 
  summarise(totFeed = sum(feeding))
tot$Brood_ID<-as.factor(tot$Brood_ID)
summary(tot$Brood_ID)

fem %>% group_by(Brood_ID, Julian_date) %>% 
  summarise(feeding = sum(feeding))
fem<-fem[-c(21, 32),]#remove rows where two videos exist in one day

#add in female visits from the deleted rows 
#(add 1 to GUM on date 187 and add 3 for N1718CA2_brd1 on 180)
fem[20, 10] = 3
fem[29, 10] = 12

summary(fem$Brood_ID)

fem<-merge(tot, fem, by = c("Brood_ID", "Julian_date"))

fem<-fem %>% mutate(propfeed = feeding/totFeed)

#so that all models have the same sample size, I replaced the one missing value for
#start_time with 1007 which is the mean value
fem$Start_time[is.na(fem$Start_time)] <- 1007

library(lme4)
hab<-lmer(propfeed~Habitat+(1|Brood_ID), fem)
tod<-lmer(propfeed~Start_time+(1|Brood_ID), fem)
chick<-lmer(propfeed~Peeped_chick_count+(1|Brood_ID), fem)
date<-lmer(propfeed~Julian_date+(1|Brood_ID), fem)
age<-lmer(propfeed~Exact_age_chick+(1|Brood_ID), fem)
ageTod<-lmer(propfeed~Exact_age_chick+Start_time+(1|Brood_ID), fem)
bkpk<-lmer(propfeed~With_bpk.+(1|Brood_ID), fem)
nnd<-lmer(propfeed~Distance_nearest_neighbor_m+(1|Brood_ID), fem)
anova(hab, tod, chick, date, age, ageTod, bkpk, nnd)
summary(age)
summary(ageTod)

ggplot(fem, aes(Exact_age_chick, propfeed))+geom_point()+geom_smooth(method=lm)+
  ylab("Prop of female prov visits")+xlab("Chick age (days)")
ggplot(fem, aes(Start_time, propfeed))+geom_point()+geom_smooth(method=lm)+
  ylab("Prop of female prov visits")+xlab("Time of day")

