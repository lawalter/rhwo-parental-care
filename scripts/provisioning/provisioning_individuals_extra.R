library(tidyverse)

# read final prov behavior data -------------------------------------------

behavior <- 
  read_csv("clean_data/behaviors.csv") 

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
    feeding_chicks,
    usable_video
    #distance_nearest_neighbor_m
    #start_time
    )

# read bpk info -----------------------------------------------------------

bpk_status <- 
  read_csv("clean_data/bpk_status.csv")

b_sm_bpk <-
  b_sm %>%
  left_join(
    bpk_status,
    by = c("video_number", "subject"))

# read start time & neighbor dist info ------------------------------------

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

# subset to females -------------------------------------------------------

m_bpk <-
  b_tbl %>%
  group_by(video_number) %>%
  mutate(
    bpk_m = ifelse(bpk_status == "with", "1", "0"),
    bpk_m = ifelse(is.na(bpk_status), "0", bpk_m)) %>%
  ungroup() %>% 
  filter(sex == "male") %>%
  select(video_number, subject, sex, bpk_status, bpk_m)

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
        feeding_chicks/total_feeding),
    bpk_binary = 
      ifelse(
        bpk_status == "with",
        "1",
        "0")) %>%
  left_join(
    m_bpk %>%
      select(video_number, bpk_m),
    by = "video_number") 

times <- 
  f_prov %>%
  select(start_time) %>%
  filter(!str_detect(start_time, "na")) %>%
  mutate(start_time = as.integer(start_time))

f_prov$start_time[6]=938

library(lme4)
hab <- lm(f_prop ~ habitat, f_prov)
tod <- lm(f_prop ~ as.integer(start_time), f_prov)
chick <- lm(f_prop ~ peeped_chick_count, f_prov)
date <- lm(f_prop ~ julian_date, f_prov)
age <- lm(f_prop ~ exact_age_chick, f_prov)
ageTod <- lm(f_prop ~ exact_age_chick + as.integer(start_time), f_prov)
bkpk <- lm(f_prop ~ bpk_status, f_prov)
bkpk_binary <- lm(f_prop ~ bpk_binary, f_prov)
bkpk_m <- lm(f_prop ~ bpk_m, f_prov)
nnd <- lm(f_prop ~ dist_nearest_neighbor, f_prov)
usable <- lm(f_prop ~ usable_video, f_prov)
# Quadratic age
# Quadratic date

AIC(hab, chick, date, age, nnd)

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
