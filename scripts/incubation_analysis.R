# intro -------------------------------------------------------------------

# Red-headed Woodpecker incubation analysis 
# by: Lynn Abigail Walter

# setup -------------------------------------------------------------------

library(stringr)
library(lubridate)
library(reshape2)
library(car)
library(tidyverse)
library(psych)

# read rds ----------------------------------------------------------------

read_rds('clean_data/incubation.rds') %>%
  list2env(envir = .GlobalEnv)

# script ------------------------------------------------------------------

behavior_data <-
  boris_data %>% 
  left_join(
    sex_data %>%
      select(subject, rhwo_key, sex),
    by = 'rhwo_key') %>%
  filter(behavior %in% c('in cavity')) %>%
  mutate_if(
    is.character, 
    str_replace_all, 
    pattern = 'in cavity', 
    replacement = 'incubating') %>%
  left_join(
    video_data %>%
      select(video_key, video_number, usable_length, date) %>%
      group_by(video_number) %>%
      mutate(usable_length_total = sum(usable_length)) %>%
      ungroup(),
    by = 'video_key') %>%
  # Below filter added 2/8/2020 in accordance with methods:
  filter(duration_sec > 179) %>%
  # Sum incubation per parent by video_number. This is so that incubation
  # events spanning two videos are not divided and incorrectly analyzed as
  # two separate (shorter) events:
  group_by(video_number, subject) %>%
  mutate(
    # Calculate incubation rate as a total duration (mins):
    incubation_rate_min = sum(duration_sec[behavior == 'incubating'])/60,
    # Calculate incubation rate as standard minutes per usable length:
    incubation_rate = 
      ifelse(
        is.nan(incubation_rate_min), 
        0, 
        (incubation_rate_min*60)/first(usable_length_total)),
    date = mdy(date)) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup() %>%
  # Remove incubation events (2) done by unidentified parents
  filter(!str_detect(subject, 'unknown')) %>% 
  left_join(
    NOAA_data %>%
      select(date, tmax),
    by = 'date') %>%
  mutate(jdate = yday(date)) %>%
  select(video_number, subject, sex, date, tmax, incubation_rate, 
         usable_length_total)

# quick summaries ---------------------------------------------------------

behavior_data %>%
  select(usable_length_total) %>%
  distinct() %>%
  arrange(usable_length_total) %>%
  describe()

video_data %>%
  select(video_key, video_number, usable_length, date) %>%
  group_by(video_number) %>%
  mutate(usable_length = sum(usable_length)) %>%
  select(video_number, usable_length) %>%
  distinct 

# sex t-test --------------------------------------------------------------

# Incubation rates table by sex:

incubation_sex <- 
  behavior_data %>%
  pivot_wider(video_number,
               names_from = 'sex', 
               values_from = 'incubation_rate')

# Testing distributions for normality:

shapiro.test(incubation_sex$female) #normal
shapiro.test(incubation_sex$male) #normal

# Testing for equality of variance:

leveneTest(incubation_rate ~ as.factor(sex), 
           data = behavior_data) #equal

# T-test of incubation rate by sex:

t.test(incubation_sex$female, incubation_sex$male, 
       alternative = c('two.sided'),
       paired = TRUE,
       var.equal = TRUE,
       conf.level = 0.95)
       # p = 0.047

# sex plots ---------------------------------------------------------------

# Define color scheme:

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

# Boxplot of incubation by sex (color)

ggplot(
  behavior_data, aes(x = sex, y = incubation_rate, fill = sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Incubating (min/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        text = element_text(size = 12), 
        legend.position = "none") 

# Boxplot of incubation by sex (black & white)

ggplot(
  behavior_data, aes(x = sex, y = incubation_rate, fill = NA)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Incubation duration (min/hr)",
       title = "Figure 2") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.position = "none")  +
  ggsave(
    file = "incubation_fig.pdf",
    path ="plots/",
    width = 3.5,
    height = 3,
    units = "in",
    dpi = 600)

# Violin plot:

ggplot(
  behavior_data, aes(x = sex, y = incubation_rate, fill = sex)) + 
  geom_violin() + 
  labs(x = "Sex", 
       y = "Incubating (min/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        text = element_text(size = 12), 
        legend.position = "none") 


# number of cavity entries ------------------------------------------------

# TO DO:

# Count how many times each sex went into their cavity:
# Also, count how many of these entries were < 180 sec per sex:

cavity_entries <-
  boris_data %>% 
  left_join(
    sex_data %>%
      select(subject, rhwo_key, sex),
    by = 'rhwo_key') %>%
  filter(behavior %in% c('in cavity')) %>%
  mutate_if(
    is.character, 
    str_replace_all, 
    pattern = 'in cavity', 
    replacement = 'incubating') %>%
  left_join(
    video_data %>%
      select(video_key, video_number, usable_length, date) %>%
      group_by(video_number) %>%
      mutate(usable_length_total = sum(usable_length)) %>%
      ungroup(),
    by = 'video_key') %>%
  # Below filter added 2/8/2020 in accordance with methods:
  filter(duration_sec > 179) %>%
  group_by(video_number, subject) %>%
  mutate(
    incubation_rate_min = sum(duration_sec[behavior == 'incubating'])/60,
    incubation_rate = 
      ifelse(
        is.nan(incubation_rate_min), 
        0, 
        (incubation_rate_min*60)/first(usable_length_total)),
    date = mdy(date)) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup() %>%
  # Remove incubation events (2) done by unidentified parents
  filter(!str_detect(subject, 'unknown')) %>% 
  left_join(
    NOAA_data %>%
      select(date, tmax),
    by = 'date') %>%
  mutate(jdate = yday(date)) %>%
  select(video_number, subject, sex, date, tmax, incubation_rate, 
         usable_length_total)

# tmax regression ---------------------------------------------------------

# Testing TMAX - females
behavior_data_f <- 
  behavior_data %>%
  filter(sex == 'female')

lm_tmax_f <- lm(incubation_rate ~ tmax, data = behavior_data_f)
summary(lm_tmax_f)
  # p: 0.4
  # t: -0.9
  # SE: 1.6
  # df: 8
  # adj R = -0.03

# Testing TMAX - males
behavior_data_m <- 
  behavior_data %>%
  filter(sex == 'male')

lm_tmax_m <- lm(incubation_rate ~ tmax, data = behavior_data_m)
summary(lm_tmax_m)
  # p: 0.5
  # t: -0.8
  # SE: 1.0
  # df: 9
  # adj R > -0.05

ggplot(
  behavior_data, aes(x = tmax, y = incubation_rate)) +
  geom_point()

ggplot(
  behavior_data, aes(x = tmax, y = incubation_rate, color = sex)) +
  geom_point()
