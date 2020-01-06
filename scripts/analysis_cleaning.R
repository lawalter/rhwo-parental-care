# Cleaning
bbyvid$Cleaning_rate <- (bbyvid$cleaning/bbyvid$Usable_video)*60
bbyvid$Cleaning_rate_perchk <- (bbyvid$Cleaning_rate)/(bbyvid$Peeped_chick_count)
byob_final_male <- bbyvid %>% filter(Sex == "male")
byob_final_female <- bbyvid %>% filter(Sex == "female")

shapiro.test(byob_final_male$Cleaning_rate_perchk) #not normal
shapiro.test(byob_final_female$Cleaning_rate_perchk) #not normal

cleaning_test <- wilcox.test(byob_final_female$Cleaning_rate_perchk, byob_final_male$Cleaning_rate_perchk, 
                             paired = TRUE, exact=FALSE, alternative = c("two.sided"), conf.level = 0.95)
cleaning_test

Zstat<-qnorm(cleaning_test$p.value/2)
Zstat
