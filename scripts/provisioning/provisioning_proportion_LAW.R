library(tidyverse)

behavior <- 
  read_csv("clean_data/behaviors.csv") %>%
  select(-X1)

b_sm <-
  behavior %>%
  select(
    video_number,
    subject,
    sex,
    brood_id,
    julian_date,
    habitat,
    exact_age_chick,
    peeped_chick_count,
    feeding_chicks
    #distance_nearest_neighbor_m
    #start_time
  )

bpk_status <- 
  read_csv("clean_data/bpk_status.csv") %>%
  select(-X1)

b_sm_bpk <-
  b_sm %>%
  left_join(
    bpk_status,
    by = c("video_number", "subject"))

dist_start <-
  read_csv("raw_data/provisioning_video_data.csv") %>%
  select(
    video_number = Video_number,
    dist_nearest_neighbor = Distance_nearest_neighbor_m,
    start_time = Start_time) %>%
  group_by(video_number, dist_nearest_neighbor) %>%
  mutate(start_time = first(start_time)) %>%
  ungroup() %>%
  distinct()

b_tbl <-
  b_sm_bpk %>%
  left_join(
    dist_start,
    by = "video_number")

f_prov <-
  b_tbl %>%
  group_by(video_number) %>%
  mutate(total_feeding = sum(feeding_chicks)) %>%
  ungroup() %>%
  filter(sex == "female") %>%
  mutate(
    f_prop = 
      ifelse(
        total_feeding == 0,
        0,
        feeding_chicks/total_feeding)) %>%
  mutate(start_time = ifelse(str_detect(start_time, "na"), NA, start_time))


library(lme4)
hab <- lmer(f_prop ~ habitat + (1|brood_id), f_prov)
tod <- lmer(f_prop ~ start_time + (1|brood_id), f_prov)
chick <- lmer(f_prop ~ peeped_chick_count + (1|brood_id), f_prov)
date <- lmer(f_prop ~ julian_date + (1|brood_id), f_prov)
age <- lmer(f_prop ~ exact_age_chick + (1|brood_id), f_prov)
#ageTod <- lmer(f_prop ~ exact_age_chick + start_time + (1|brood_id), f_prov)
bkpk <- lmer(f_prop ~ bpk_status + (1|brood_id), f_prov)
nnd <- lmer(f_prop ~ dist_nearest_neighbor + (1|brood_id), f_prov)

# Removed some models from anova due to "models were not all fitted to the same
# size of dataset" error
anova(hab, chick, date, age, nnd)

summary(age)
summary(ageTod)

ggplot(f_prov, aes(x = exact_age_chick, y = f_prop)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ylab("Prop of female prov visits") + 
  xlab("Chick age (days)")

f_prov %>%
  ggplot(aes(x = as.numeric(start_time), y = f_prop)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ylab("Prop of female prov visits") + 
  xlab("Time of day")
