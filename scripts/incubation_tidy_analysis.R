## Red-headed Woodpecker incubation analysis 
## By: Lynn Abigail Walter

# SETUP -------------------------------------------------------------------

library(stringr)
library(lubridate)
library(reshape2)
library(car)
library(tidyverse)

read_rds('cleaned_data/incubation.rds') %>%
  list2env(envir = .GlobalEnv)

# WRANGLING BEHAVIORS -------------------------------------------------------

# Get  video_number, subject, sex, jdate, tmax, incubation_rate in one df

video_data %>%
  select(video_key, Video_number, Usable_length, Date) %>%
  group_by(Video_number) %>%
  mutate(Usable_length = sum(Usable_length))

behavior_data <-
  boris_data %>% 
  left_join(
    sexes %>%
      select(rhwo_key, Subject, Sex),
    by = 'rhwo_key') %>%
  filter(Behavior %in% c('in cavity')) %>%
  mutate_if(
    is.character, 
    str_replace_all, 
    pattern = 'in cavity', 
    replacement = 'incubating') %>%
  left_join(
    video_data %>%
      select(video_key, Video_number, Usable_length, Date) %>%
      group_by(Video_number) %>%
      mutate(Usable_length_total = sum(Usable_length)),
    by = 'video_key'
  ) %>%
  group_by(Video_number, Subject) %>%
  mutate(
    incubation_rate_min = sum(Duration_sec[Behavior == 'incubating'])/60,
    incubation_rate = ifelse(
      is.nan(incubation_rate_min), 
      0, 
      (incubation_rate_min*60)/first(Usable_length_total))
  ) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup() %>%
  # Remove incubation events (2) done by unidentified parents
  filter(!str_detect(Subject, 'unknown')) %>%
  left_join(
    NOAA_data %>%
      select(Date = DATE, TMAX),
    by = 'Date') %>%
  mutate(Date = yday(Date)) %>%
  select(Video_number, Subject, Sex, Date, TMAX, incubation_rate, Usable_length_total)

behavior_data %>%
  select(Usable_length_total) %>%
  distinct() %>%
  describe()

# T-TESTS BY SEX ----------------------------------------------------------

# Incubation rates
incubation_sex <- 
  dcast(behavior_data, Video_number ~ Sex, value.var = "incubation_rate")

shapiro.test(incubation_sex$female) #normal
shapiro.test(incubation_sex$male) #normal

leveneTest(incubation_rate ~ Sex, data = behavior_data) #not equal

t.test(incubation_sex$female, incubation_sex$male, 
       alternative = c('two.sided'),
       paired = TRUE,
       var.equal = FALSE,
       conf.level = 0.95)
# p = 0.05

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
ggplot(
  behavior_data, aes(x=Sex, y=incubation_rate, fill=Sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Incubating (min/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size=24), 
        axis.title.y = element_text(size=24), 
        text = element_text(size=24), 
        legend.position = "none") 

ggplot(
  behavior_data, aes(x=Sex, y=incubation_rate, fill=Sex)) + 
  geom_violin() + 
  labs(x = "Sex", 
       y = "Incubating (min/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size=24), 
        axis.title.y = element_text(size=24), 
        text = element_text(size=24), 
        legend.position = "none") 

# Testing TMAX
lm_tmax <- lm(incubation_rate ~ TMAX, data = behavior_data)
summary(lm_tmax)

ggplot(
  behavior_data, aes(x=TMAX, y=incubation_rate)) +
  geom_point()

ggplot(
  behavior_data, aes(x=TMAX, y=incubation_rate, color=Sex)) +
  geom_point()

# Testing date
lm_date <- lm(incubation_rate ~ Date, data = behavior_data)
summary(lm_date)

ggplot(
  behavior_data, aes(x=Date, y=incubation_rate)) +
  geom_point()

ggplot(
  behavior_data, aes(x=Date, y=incubation_rate, color=Sex)) +
  geom_point()
