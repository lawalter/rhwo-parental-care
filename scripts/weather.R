# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# read data ---------------------------------------------------------------

weather_raw <- 
  read_csv("raw_data/NOAA_weather_data.csv") %>% 
  rename_all(~tolower(.)) %>% 
  select(date, prcp, tmax, tmin) %>% 
  mutate(
    yr = year(date),
    jday = yday(date))

# All weather (period of time covers Julian dates from first incubation video to
# last provisioning video)
weather <-
  weather_raw %>% 
  # Constrain to May 18 - Aug 8
  filter(jday >= 138 & jday <= 220)

# Provisioning weather (period of time covers Julian dates from first to last
# provisioning video)
brood_weather <- 
  weather_raw %>% 
  # Constrain to June 13 - Aug 8
  filter(jday >= 164 & jday <= 220)

# Incubation weather (period of time covers Julian dates from first to last
# incubation video)
inc_weather <- 
  weather_raw %>% 
  # Constrain to May 18 - July 22
  filter(jday >= 138 & jday <= 203)

# Incubation data
inc <- read_csv("clean_data/incubation.csv")

# Provisioning/Brooding data
brood <- read_csv("clean_data/behaviors.csv")

# collinearity ------------------------------------------------------------

inc_weather %>% 
  ggplot() +
  geom_jitter(aes(x = tmin, y = tmax)) +
  stat_smooth(
    aes(x = tmin, y = tmax), 
    alpha = 0.3, method = "lm") +
  labs(title = "Incubation") +
  theme_classic()

shapiro.test(inc_weather$tmax) #not normal
shapiro.test(inc_weather$tmin) #not normal
cor.test(inc_weather$tmax, inc_weather$tmin, method = "pearson") # p < 0.001, t = 10.2, df = 130
cor.test(inc_weather$tmax, inc_weather$tmin, method = "spearman") # p < 0.001, rho = 0.719

brood_weather %>% 
  ggplot() +
  geom_jitter(aes(x = tmin, y = tmax)) +
  stat_smooth(
    aes(x = tmin, y = tmax), 
    alpha = 0.3, method = "lm") +
  labs(title = "Brooding") +
  theme_classic()

shapiro.test(brood_weather$tmax) #not normal
shapiro.test(brood_weather$tmin) #not normal
cor.test(brood_weather$tmax, brood_weather$tmin, method = "pearson") # p < 0.001, t = 6.0, df = 112
cor.test(brood_weather$tmax, brood_weather$tmin, method = "spearman") # p < 0.001, rho = 0.54

# plots -------------------------------------------------------------------

# Incubation
inc_weather %>% 
  left_join(
    inc %>% 
      select(date) %>% 
      mutate(Video = "Incubation"),
    by = "date") %>% 
  distinct() %>% 
  mutate(Video = ifelse(is.na(Video), "None", Video)) %>% 
  ggplot() +
  geom_jitter(aes(x = tmin, y = tmax, color = Video, shape = Video)) +
  stat_smooth(
    aes(x = tmin, y = tmax, color = Video, fill = Video), 
    alpha = 0.3, method = "lm") +
  scale_color_manual(values = c("red", "#999999")) +
  scale_fill_manual(values = c("red", "#999999")) +
  theme_classic() +
  facet_wrap(~yr)

# Brooding
brood_weather %>% 
  left_join(
    brood %>% 
      select(jday = julian_date) %>% 
      mutate(Video = "Brooding"),
    by = "jday") %>% 
  distinct() %>% 
  mutate(Video = ifelse(is.na(Video), "None", Video)) %>% 
  ggplot() +
  geom_jitter(aes(x = tmin, y = tmax, color = Video, shape = Video)) +
  stat_smooth(
    aes(x = tmin, y = tmax, color = Video, fill = Video), 
    alpha = 0.3, method = "lm") +
  scale_color_manual(values = c("red", "#999999")) +
  scale_fill_manual(values = c("red", "#999999")) +
  theme_classic() +
  facet_wrap(~yr)
  
# stats -------------------------------------------------------------------

inc_lm <- lm(tmin ~ tmax, data = inc_weather)
summary(inc_lm)

brood_lm <- lm(tmin ~ tmax, data = brood_weather)
summary(brood_lm)

min2017 <- weather %>% filter(yr == 2017) %>% select(tmin) %>% pull()
min2018 <- weather %>% filter(yr == 2018) %>% select(tmin) %>% pull()
max2017 <- weather %>% filter(yr == 2017) %>% select(tmax) %>% pull()
max2018 <- weather %>% filter(yr == 2018) %>% select(tmax) %>% pull()

# 2017
cor.test(min2017, max2017) # correlated; p < 0.001

# 2018
cor.test(min2018, max2018) # correlated; p < 0.001

# Mins
t.test(min2017, min2018) # p = 0.2

# Maxes
t.test(max2017, max2018) # p = 0.98
