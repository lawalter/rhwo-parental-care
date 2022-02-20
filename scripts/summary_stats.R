# libraries ---------------------------------------------------------------

library(tidyverse)
library(psych)

# data --------------------------------------------------------------------

# Video level behavior observation data

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  mutate(
    cleaning_rate = (cleaning_nest/usable_video)*60,
    cleaning_rate_perchk = cleaning_rate/peeped_chick_count,
    prov_rate = (feeding_chicks/usable_video)*60,
    prov_rate_perchk = prov_rate/peeped_chick_count,
    brooding_rate = (brooding_min/usable_video)*60)

# Brood level data (brood data not repeated for each subject)

brood_level <- 
  behaviors %>%
  select(julian_date, exact_age_chick)

# summary numbers ---------------------------------------------------------

# Length of video used

behaviors %>%
  select(usable_video, video_number) %>%
  distinct() %>%
  select(-video_number) %>%
  sum(.)/60

# Number of broods

behaviors %>%
  select(brood_id) %>%
  distinct()

# Number of nest snags

behaviors %>%
  select(nest_id) %>%
  distinct()

# Number of parents
# Some of these parents participate at > 1 brood

behaviors %>%
  select(subject) %>%
  distinct()

# Video length
describe(behaviors$usable_video)

# Mean provisioning rate per chick
describe(behaviors$prov_rate_perchk)

# Mean provisioning rate 
describe(behaviors$prov_rate)

# video start times -------------------------------------------------------

# Video start times

video_times <- 
  read.csv('raw_data/provisioning_video_data.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) %>%
  # Unselect data that is redundant or unnecessary
  select(video_number, start_time) %>%
  group_by(video_number) %>%
  summarise(start_hr = min(start_time))

behaviors <-
  behaviors %>%
  left_join(video_times, by = "video_number")

# Start time distribution

plot(behaviors$start_hr)

# Start time vs. provisioning

corr.test(behaviors$start_hr, behaviors$prov_rate_perchk)
summary(lm(start_hr ~ prov_rate_perchk, data = behaviors)) # p = 0.4
plot(behaviors$start_hr, behaviors$prov_rate_perchk)

# Start time vs. season

corr.test(behaviors$start_hr, behaviors$julian_date)
summary(lm(julian_date ~ start_hr, data = behaviors)) # p = 0.4
plot(behaviors$julian_date, behaviors$start_hr)

# brooding summaries ------------------------------------------------------

brooding_stats <- 
  behaviors %>% 
  select(brooding_count, brooding_min, sex, usable_video) %>%
  mutate(
    brooding_rate = (brooding_min/usable_video)*60,
    brooding_n_hr = (brooding_count/usable_video)*60) 

brm <-
  brooding_stats %>% 
  filter(sex == "male")

brf <-
  brooding_stats %>% 
  filter(sex == "female")

describe(brm$brooding_rate) # mean = 2.64, SD = 5.65, n = 44
describe(brf$brooding_rate) # mean = 4.22, SD = 9.3, n = 44
describe(brm$brooding_n_hr) # mean = 0.54, SD = 0.92, n = 44
describe(brf$brooding_n_hr) # mean = 0.52, SD = 0.98, n = 44

# See brooding_duration.R:

# Male brooding duration
# mean = 4.34
# sd = 2.94
# n = 53
# se = 0.4
# range = 1.02-15.07

# Female brooding duration:
# mean = 7.28
# sd = 5.96
# n = 62
# se = 0.76
# range = 1.04-30.21

# distributions -----------------------------------------------------------

# Chick age distribution

shapiro.test(brood_level$exact_age_chick) # not normal

# summary tests -----------------------------------------------------------

# Chick age vs julian date (correlation)
# Kendall correlation bc chick age is non-parametric

leveneTest(brood_level$julian_date, brood_level$exact_age_chick) # equal

cor.test(x = brood_level$julian_date, 
         y = brood_level$exact_age_chick,
         method = c("kendall"))

plot(x = brood_level$julian_date, y = brood_level$exact_age_chick)

# comparisons by year -----------------------------------------------------

# T-tests by year

b_17 <- behaviors %>% filter(year == 2017)
b_18 <- behaviors %>% filter(year == 2018)

# T-tests
# Provisioning
shapiro.test(b_17$prov_rate_perchk)  #not normal
shapiro.test(b_18$prov_rate_perchk)  #not normal
wilcox.test(b_17$prov_rate_perchk, 
            b_18$prov_rate_perchk, 
            conf.level = 0.95) 

# Cleaning
shapiro.test(b_17$cleaning_rate_perchk)  #not normal
shapiro.test(b_18$cleaning_rate_perchk)  #not normal
wilcox.test(b_17$cleaning_rate_perchk, 
            b_18$cleaning_rate_perchk, 
            conf.level = 0.95)

# Brooding
shapiro.test(b_17$brooding_rate)  #not normal
shapiro.test(b_18$brooding_rate)  #not normal
wilcox.test(b_17$brooding_rate, 
            b_18$brooding_rate, 
            conf.level = 0.95)

























## Old

## Code
































# old code ----------------------------------------------------------------

jdateda <- bbyvid %>% filter(Julian_date <= "188.5")
jdateda$Date <- c("Early summer")
jdatedb <- bbyvid %>% filter(Julian_date > "188.5")
jdatedb$Date <- c("Late summer")
bbyvid <- rbind(jdateda, jdatedb)


`Early summer` <- bbyvid %>% filter(Date == "Early summer")
`Late summer` <- bbyvid %>% filter(Date == "Late summer")

# `Early summer`
describe(`Early summer`$provrate) # 5.6
describe(`Early summer`$feedingperchkhr) # 2.7
describe(`Early summer`$cleanrate) # 0.8
describe(`Early summer`$brooding_min) # 1.9

# `Late summer`
describe(`Late summer`$provrate) # 3.9
describe(`Late summer`$feedingperchkhr) # 1.9
describe(`Late summer`$cleanrate) # 0.91
describe(`Late summer`$brooding_min) # 2.2


###################
library(gsheet)
brood_url <- 'https://drive.google.com/open?id=1KkZfFEB8H5VqkMeTuAum-I91ycgh4oTFakXP_MH8IYA'
brood_fates <- data.get(brood_url)

## Stats for nest success and failures

library(reshape2)
cast_fledges <- dcast(brood_fates, Brood_ID ~ Habitat, 
                      value.var="Fledge", fun.aggregate=sum)
cast_fledges <- na.omit(cast_fledges)

sum(cast_fledges$`Closed canopy`)
3/14 #of fledged
3/11 #total

sum(cast_fledges$Savanna)
11/14 #of fledged
11/31 #total


cast_fails <- dcast(brood_fates, Brood_ID ~ Habitat, 
                    value.var="Failed", fun.aggregate=sum)
cast_fails <- na.omit(cast_fails)

sum(cast_fails$`Closed canopy`)
3/14 #of failed
3/11 #total

sum(cast_fails$Savanna)
9/14 #of failed
9/31 #total

sum(cast_fails$Wetland)
2/14 #of failed
2/3 #total


cast_ip <- dcast(brood_fates, Brood_ID ~ Habitat, 
                 value.var="In.progress", fun.aggregate=sum)
cast_ip <- na.omit(cast_ip)

sum(cast_ip$`Closed canopy`)
6/19 #of inprog

sum(cast_ip$Savanna)
12/19 #of inprog

sum(cast_ip$Wetland)
1/19 #of inprog


############################################
brood_contents_url <- 'https://docs.google.com/spreadsheets/d/1VLmIWrhkNg9-KWd5kJ6HfAt4vubDv34jJHSJCjPRVso/edit?usp=sharing'
brood_conts <- data.get(brood_contents_url)
brood_conts$Season <- ifelse(brood_conts$found_Julian > 188, "late", "early")

## Stats for number of eggs 
cast_eggs_date <- dcast(brood_conts, Brood_ID ~ Season, 
                        value.var="max_eggs", fun.aggregate=sum)
cast_eggs_date <- na.omit(cast_eggs_date)
cast_eggs_date[cast_eggs_date==0] <- NA

cast_eggs_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                       value.var="max_eggs", fun.aggregate=sum)
cast_eggs_hab <- na.omit(cast_eggs_hab)
cast_eggs_hab[cast_eggs_hab==0] <- NA

describe(cast_eggs_date$early)
describe(cast_eggs_date$late)
describe(cast_eggs_hab$CC)
describe(cast_eggs_hab$Savanna)
describe(cast_eggs_hab$Wetland)
describe(brood_conts$max_eggs)

shapiro.test(cast_eggs_hab$CC)
shapiro.test(cast_eggs_hab$Savanna) # not normal

wilcox.test(cast_eggs_hab$CC, cast_eggs_hab$Savanna, alternative = c("two.sided"), 
            conf.level = 0.95, paired = FALSE, exact = NULL)
##U = W
##http://r.789695.n4.nabble.com/U-value-from-wilcox-test-td2332811.html

t.test(cast_eggs_hab$CC, cast_eggs_hab$Savanna, alternative = c("two.sided"), 
       conf.level = 0.95, paired = FALSE, exact = NULL)

## Stats for number of chicks
cast_chicks_date <- dcast(brood_conts, Brood_ID ~ Season, 
                          value.var="max_chicks", fun.aggregate=sum)
cast_chicks_date <- na.omit(cast_chicks_date)
cast_chicks_date[cast_chicks_date==0] <- NA

cast_chicks_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                         value.var="max_chicks", fun.aggregate=sum)
cast_chicks_hab <- na.omit(cast_chicks_hab)
cast_chicks_hab[cast_chicks_hab==0] <- NA

describe(cast_chicks_date$early)
describe(cast_chicks_date$late)
describe(cast_chicks_hab$CC)
describe(cast_chicks_hab$Savanna)

shapiro.test(cast_chicks_hab$CC) # not normal
shapiro.test(cast_chicks_hab$Savanna) # not normal

wilcox.test(cast_chicks_hab$CC, cast_chicks_hab$Savanna, alternative = c("two.sided"), 
            conf.level = 0.95, paired = FALSE, exact = NULL)

t.test(cast_chicks_hab$CC, cast_chicks_hab$Savanna, alternative = c("two.sided"), 
       conf.level = 0.95, paired = FALSE, exact = NULL)

describe(cast_chicks_hab$Wetland)


## Stats for number of fledges
cast_fledge_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                         value.var="chicks_fledged", fun.aggregate=sum)
cast_fledge_hab <- na.omit(cast_fledge_hab)
cast_fledge_hab[cast_fledge_hab==0] <- NA

describe(cast_fledge_hab$CC)
describe(cast_fledge_hab$Savanna)
describe(cast_fledge_hab$Wetland)


## Stats for hatch proportion
cast_hatch_date <- dcast(brood_conts, Brood_ID ~ Season, 
                         value.var="egg_to_chick", fun.aggregate=sum)
cast_hatch_date <- na.omit(cast_hatch_date)
cast_hatch_date[cast_hatch_date==0] <- NA

cast_hatch_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                        value.var="egg_to_chick", fun.aggregate=sum)
cast_hatch_hab <- na.omit(cast_hatch_hab)
cast_hatch_hab[cast_hatch_hab==0] <- NA

describe(cast_hatch_date$early)
describe(cast_hatch_date$late)
describe(cast_hatch_hab$CC)
describe(cast_hatch_hab$Savanna)
describe(cast_hatch_hab$Wetland)


## Stats for fledge proportion from chicks
cast_fledgec_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                          value.var="chick_to_fledge", fun.aggregate=sum)
cast_fledgec_hab <- na.omit(cast_fledgec_hab)
cast_fledgec_hab[cast_fledgec_hab==0] <- NA

describe(cast_fledgec_hab$CC)
describe(cast_fledgec_hab$Savanna)
describe(cast_fledgec_hab$Wetland)


## Stats for fledge proportion from eggs
brood_conts$egg_to_fledge <- brood_conts$chicks_fledged/brood_conts$max_eggs
brood_conts[brood_conts==Inf] <- NA
brood_conts[brood_conts=="NaN"] <- NA

cast_fledgee_hab <- dcast(brood_conts, Brood_ID ~ Habitat, 
                          value.var="egg_to_fledge", fun.aggregate=sum)
cast_fledgee_hab <- na.omit(cast_fledgee_hab)
cast_fledgee_hab[cast_fledgee_hab==0] <- NA

describe(cast_fledgee_hab$CC)
describe(cast_fledgee_hab$Savanna)
describe(cast_fledgee_hab$Wetland)


# Fate by habitat
# Total fates (all habitats):
# Fail: 25.5%
# Fledge: 33.3%
# In progress: 37.3%
# Uncertain: 3.9%

# Fates (CC, n = 14):
# Fail: 3 (21.4%)
# Fledge: 3 (21.4%)
# In progress: 7 (50.0%)
# Uncertain: 1 (7.1%)

# Fates (sav, n = 34):
# Fail: 8 (23.5%)
# Fledge: 14 (41.2%)
# In progress: 11 (32.4%)
# Uncertain: 1 (2.9%)

################################################
## Literature table 2

clutch_size <- c(4.4, 4, 4.5, 5.4, 4, 3.5, 4.8, 4.2)
percent_chg <- c(0.0124, 0.012, 0.0106, -0.0076, -0.0092, -0.042, -0.061, -0.0851)

clutch_pop <- as.data.frame(clutch_size)
clutch_pop$percent_chg <- percent_chg

plot(clutch_pop$clutch_size, clutch_pop$percent_chg)
cor(clutch_size, percent_chg)

############################################################################

# Early/late summer

f_brd_last_chk <- c(172, 183, 187, 187, 180, 197, 179, 186, 183)
f_brd_empty_date <- c(187, 192, 192, 183, 183, 201, 183, 188, 192)

sec_brd_hatch <- c(206, 191, 200, 208, 193, 183)
sec_brd_first_egg <- c(198, 194, 180, 192, 194)

describe(f_brd_last_chk)     # 183.8 +/- 6.9, n = 9
describe(f_brd_empty_date)   # 189.0 +/- 6.0, n = 9
describe(sec_brd_hatch)      # 196.8 +/- 9.6, n = 6
describe(sec_brd_first_egg)  # 191.6 +/- 6.8, n = 5

# Empty = 189, first egg = 191.6 ... go with 190???
# Julian day 190 is 9 July

bbyvid$Date <- ifelse(bbyvid$Julian_date >= 190, "Late summer", "Early summer")
bbyvid_early <- bbyvid %>% dplyr::filter(bbyvid$Date == "Early summer")
bbyvid_early <- bbyvid_early[!duplicated(bbyvid_early$Video_number), ]
bbyvid_late <- bbyvid %>% dplyr::filter(bbyvid$Date == "Late summer")
bbyvid_late <- bbyvid_early[!duplicated(bbyvid_late$Video_number), ]

describe(bbyvid_early$Exact_age_chick)   # 12.8 +/- 8.2, n = 25
describe(bbyvid_late$Exact_age_chick)   # 14.2 +/- 8.7, n = 13


###############################################################################

cast_fledges[(cast_fledges == 1)] <- c("Fledge")
cast_fails[(cast_fails == 1)] <- c("Fail")
cc_fate <- rbind(cast_fails, cast_fledges)
cc_fate$Savanna <- NULL
cc_fate$Wetland <- NULL
sav_fate <- rbind(cast_fails, cast_fledges)
sav_fate$`Closed canopy` <- NULL
sav_fate$Wetland <- NULL
names(sav_fate)[1] <- "Habitat"
names(cc_fate)[1] <- "Habitat"
names(sav_fate)[2] <- "Fate"
names(cc_fate)[2] <- "Fate"
sav_fate$Habitat <- c("Savanna")
cc_fate$Habitat <- c("CC")
cc_fate <- cc_fate %>% filter(Fate != 0)
sav_fate <- sav_fate %>% filter(Fate != 0)
fate <- rbind(cc_fate, sav_fate)

chisq.test(fate$Habitat, fate$Fate, correct = FALSE)
t.test(cast_fledges$Savanna, cast_fledges$`Closed canopy`)
cc_matrix = c(3, 3)
sav_matrix = c(11, 9)
success.survey = as.data.frame(rbind(cc_matrix, sav_matrix))
names(success.survey) = c('fledge', 'fail')
chisq.test(success.survey, simulate.p.value = TRUE)



cast_nestfate <- dcast(bbyvid, Brood_ID + Habitat ~ Nest_fate, 
                       value.var="Chicks_fledged", fun.aggregate=max)
cast_nestfate <- na.omit(cast_nestfate)
cast_nestfate[(cast_nestfate == -Inf)] <- NA
cast_nestfate$Fledge[is.na(cast_nestfate$Fledge)] <- 0
cast_nestfate$Failed <- NULL

cast_nestfate_etc <- dcast(bbyvid, Brood_ID + Habitat ~ Nest_fate, 
                           value.var="feedingperchkhr", fun.aggregate=mean)
cast_nestfate_etc[(cast_nestfate_etc == "NaN")] <- NA
cast_nestfate_fl <- cast_nestfate_etc %>% select(Brood_ID, Fledge)
cast_nestfate_fa <- cast_nestfate_etc %>% select(Brood_ID, Failed)
names(cast_nestfate_fl)[2] <- c("feedingperchkhr")
names(cast_nestfate_fa)[2] <- c("feedingperchkhr")
cast_nestfate_both <- rbind(cast_nestfate_cc, cast_nestfate_sav)
cast_nestfate_both$feedingperchkhr <- na.omit(cast_nestfate_both$feedingperchkhr)

nestfate <- merge(cast_nestfate, cast_nestfate_both, by = "Brood_ID")
nestfate <- nestfate[!duplicated(nestfate$Brood_ID), ]


ggplot(
  nestfate, aes(x=Fledge, y=feedingperchkhr)) + 
  geom_point(aes(color = Habitat)) +
  geom_smooth(method="lm", aes(Fledge, feedingperchkhr)) +
  theme(text=element_text(size=12, family="mono")) +
  theme_classic()



#########################

headout <- select(provision_data, Exact_age_chick, Chicks_visible.)
headout <- headout %>% filter(Chicks_visible. > 0)

describe(headout$Exact_age_chick)
