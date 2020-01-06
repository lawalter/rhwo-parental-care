## Red-headed Woodpecker incubation analysis 
## By: Lynn Abigail Walter

library(gsheet)
library(stringr)
library(lubridate)
library(sp)
library(tidyverse)


# IMPORT DATA -------------------------------------------------------------

# Import all data from google drive links
sex_url <- 'https://drive.google.com/open?id=1E-BLyxQ-7hMJN7JVMbKbAftsBCQxjFB-ZVDEisfNc5c'
video_url <- 'https://docs.google.com/spreadsheets/d/1Y5mKHQvY03K8YGkt5nvh3NAj93UiVhInpIx1NjBpH6M/edit?usp=sharing'
video_incubation_data_url <- 'https://docs.google.com/spreadsheets/d/1KhdKjy8Fy4KrejdrwkJXj1ARfzpgg35g2-a9I1b6vw4/edit?usp=sharing'

na_strings <- c("", "na", "NA")

getData <- function(url){
  read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings= na_strings)
}

sex_data <- getData(sex_url)
video_data_initial <- getData(video_url)
boris_data <- getData(video_incubation_data_url)



# DATA TIDYING ------------------------------------------------------------

# Make a df with total sample size of n = 78
total_sample <- video_data_initial[c("Video_number", "Ref_combo_1", "Ref_combo_2")]
total_sample$ID1 <- paste(total_sample$Video_number, total_sample$Ref_combo_1, sep=".")
total_sample$ID2 <- paste(total_sample$Video_number, total_sample$Ref_combo_2, sep=".")
total_sample$Video_number <- NULL
total_sample$Ref_combo_1 <- NULL
total_sample$Ref_combo_2 <- NULL
total_sample <- stack(total_sample)
total_sample$ind <- NULL
names(total_sample) <- c("vID")
total_sample %>%
  distinct(vID)

total_sample <-
  map_dfr(
    c('Ref_combo_1', 'Ref_combo_2'),
    function(x) {
      video_data_initial %>%
        select(Video_number, x) %>%
        tidyr::unite('vID', 'Video_number', x, sep = '.')
    }) %>%
  distinct(vID)


### DATA TIDYING ###
boris_data <-
  boris_data %>% 
  filter(Behavior == 'in cavity' | 
           Behavior == 'preening' | 
           Behavior == 'stretch' | 
           Behavior == 'stat') %>%
  arrange(Observation.id, Start_sec) %>% 
  mutate_if(is.character, str_replace_all, pattern = 'in cavity', replacement = 'incubating')

# Merge boris behavior data and video/nest data
borisdata_w_nestinfo <- 
  merge.data.frame(boris_data, video_data_initial, by="Observation.id")

# Make the sex data into 2 columns
sex_data_sm <- 
  select(sex_data, Color_combo, Sex)
names(sex_data_sm) <- c("Subject", "Sex")

# Merge the sex data with the main dataframe
# Select only relevant columns
incubation_data <- 
  merge.data.frame(borisdata_w_nestinfo, sex_data_sm, by="Subject") %>%
  unite('vID', 'Video_number', 'Subject', sep = '.', remove = FALSE) %>%
  select('vID', 'Video_number', 'Subject', 'Sex', 'Brood_ID', 'Nest_ID',
         'Behavior', 'Duration_sec', 'Usable_length', 'Date', 'Clutch_laid',
         'Year', 'TA', 'With_bpk.', 'Bpk_status_1', 'Bpk_status_2') 

# Determine julian dates
incubation_data <- 
  incubation_data %>%
  as_tibble() %>%
  mutate(Year = str_sub(incubation_data$Date, 6, 10)) %>%
  mutate(Year = str_replace(Year, '^01', '201')) %>%
  mutate_at(
    c('Date', 'Clutch_laid'),
    function(x) {
      as.Date(x, "%m/%d/%Y") %>%
        yday()
    }) %>%
  mutate(Ydate = paste(Date, Year, sep = '.')) 

# Calculate means of behaviors
# duration_behaviors <- 
incubation_data %>%
  filter(Behavior != "stretch") %>%
  map_dfr(
    c('incubating', 'preening', 'stat'),
    function(x) {
      incubation_data %>%
        filter(Behavior == x) %>%
        group_by(vID) %>%
        summarise(x = mean(Duration_sec))
    }
  )

incubating <- 
  incubation_data %>%
  filter(Behavior == 'incubating') %>%
  group_by(vID) %>%
  summarise(incubation_mean_sec = mean(Duration_sec))

preening <- 
  incubation_data %>%
  filter(Behavior == 'preening') %>%
  group_by(vID) %>%
  summarise(preen_mean_sec = mean(Duration_sec))

stat <- 
  incubation_data %>%
  filter(Behavior == 'stat') %>%
  group_by(vID) %>%
  summarise(stat_mean_sec = mean(Duration_sec))

stretching <- 
  incubation_data %>%
  filter(Behavior == 'stretch') %>%
  group_by(vID) %>%
  summarise(stretch_count = length(vID)) 

# Combine means of durations, mean counts of stretching
incubation_data2 <-
  full_join(incubating, preening) 
incubation_data3 <-
  full_join(stretching, stat)
incubation_data4 <-
  full_join(incubation_data2, incubation_data3)

# Combine behavior values with main dataframe
behavior_data <-
  inner_join(incubation_data4, incubation_data, by = "vID")

behavior_data <- behavior_data[!duplicated(behavior_data$vID), ]

# Calculate rates of behaviors standardized by video length
behavior_data <-
  behavior_data %>%
  mutate_at(
    c('incubation_mean_sec', 'preen_mean_sec', 'stat_mean_sec'),
    function(x) {
      x / behavior_data$Usable_length 
    }) 

names(behavior_data)[2:5] <- c('incubation_rate', 'preen_rate', 'stretch_count', 'stat_rate')



# NOAA WEATHER DATA -------------------------------------------------------

### Import NOAA weather data
NOAA_url <- 'https://drive.google.com/open?id=1hKlT1dcAO6jA1DkgFUhQ61ZZsCLMhsfN12oiXzfj6go'
NOAA_data <- getData(NOAA_url)

NOAA_data <- 
  NOAA_data %>%
  as_tibble() %>%
  mutate(YEAR = str_sub(NOAA_data$DATE, 1, 4)) %>%
  mutate_at(
    c('DATE'),
    function(x) {
      as.Date(as.POSIXct(x), "%Y/%M/%D") %>%
        yday()
    }) %>%
  mutate(Ydate = paste(DATE, YEAR, sep = '.'))

# Combine behavior data with weather data
behavior_data <-
  select(NOAA_data, TMAX, TMIN, Ydate) %>%
  inner_join(behavior_data, NOAA_data, by = "Ydate")



# T-TESTS BY SEX ----------------------------------------------------------

# t-test for incubation rates
library(reshape2)

incubation_t <- 
  dcast(behavior_data, Video_number ~ Sex, value.var = "incubation_rate")

shapiro.test(incubation_t$female) #normal
shapiro.test(incubation_t$male) #normal

library(car)
leveneTest(incubation_rate ~ Sex, data = behavior_data) #not equal

t.test(incubation_t$female, incubation_t$male, 
       alternative = c('two.sided'),
       paired = TRUE,
       var.equal = FALSE,
       conf.level = 0.95)

## Not significant
# p = 0.08

library(ggplot2)
ggplot(behavior_data, aes(x= Sex, y=incubation_rate, color = Sex)) +
  geom_boxplot()


colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

ggplot(
  behavior_data, aes(x=Sex, y=incubation_rate, fill=Sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Incubating (min/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme_classic() +
  theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), text = element_text(size=24), legend.position = "none") 



# t-test on stat rates
incubation_s <- 
  dcast(behavior_data, Video_number ~ Sex, value.var = "stat_rate")

shapiro.test(incubation_s$female) #normal
shapiro.test(incubation_s$male) #normal

library(car)
leveneTest(stat_rate ~ Sex, data = behavior_data) #equal

t.test(incubation_s$female, incubation_s$male, 
       alternative = c('two.sided'),
       paired = TRUE,
       var.equal = TRUE,
       conf.level = 0.95)

## Not different
# p = 0.8

library(ggplot2)
ggplot(behavior_data, aes(x= Sex, y=stat_rate, color = Sex)) +
  geom_boxplot()


# t-test on preening rates
incubation_p <- 
  dcast(behavior_data, Video_number ~ Sex, value.var = "preen_rate")

shapiro.test(incubation_p$female) #normal
shapiro.test(incubation_p$male) #normal

library(car)
leveneTest(preen_rate ~ Sex, data = behavior_data) #equal

t.test(incubation_p$female, incubation_p$male, 
       alternative = c('two.sided'),
       paired = TRUE,
       var.equal = TRUE,
       conf.level = 0.95)

## Not different
# p = 0.5

library(ggplot2)
ggplot(behavior_data, aes(x= Sex, y=preen_rate, color = Sex)) +
  geom_boxplot()



# T-TESTS BY TEMP ---------------------------------------------------------
behavior_data <- 
  behavior_data %>%
  mutate(Std_tmax = (TMAX - mean(TMAX)) / sd(TMAX)) %>%
  mutate(T_max = if_else(Std_tmax > 0, "hot", "cool"))

behavior_data <- 
  behavior_data %>%
  mutate(Std_tmin = (TMIN - mean(TMIN)) / sd(TMIN)) %>%
  mutate(T_min = if_else(Std_tmin > 0, "hot", "cool"))

# Testing Tmax
behavior_tmax <- 
  dcast(behavior_data, Video_number ~ T_max, value.var = "incubation_rate", 
        fun.aggregate = mean)
behavior_tmax[behavior_tmax == "NaN"] <- NA

shapiro.test(behavior_tmax$hot) #normal
shapiro.test(behavior_tmax$cool) #normal

leveneTest(incubation_rate ~ T_max, data = behavior_data) #equal

t.test(behavior_tmax$hot, behavior_tmax$cool, 
       alternative = c('two.sided'),
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)

# Testing Tmin
behavior_tmin <- 
  dcast(behavior_data, Video_number ~ T_min, value.var = "incubation_rate", 
        fun.aggregate = mean)
behavior_tmin[behavior_tmin == "NaN"] <- NA

shapiro.test(behavior_tmin$hot) #normal
shapiro.test(behavior_tmin$cool) #normal

leveneTest(incubation_rate ~ T_min, data = behavior_data) #equal

t.test(behavior_tmin$hot, behavior_tmin$cool, 
       alternative = c('two.sided'),
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)


# T-TEST BY DATE ----------------------------------------------------------
behavior_data <- 
  behavior_data %>%
  mutate(Std_jdate = (Date - mean(Date)) / sd(Date)) %>%
  mutate(Season = if_else(Std_jdate > 0, "late", "early"))

behavior_date <- 
  dcast(behavior_data, Video_number ~ Season, value.var = "incubation_rate", 
        fun.aggregate = mean)
behavior_date[behavior_date == "NaN"] <- NA

shapiro.test(behavior_date$early) #not normal
shapiro.test(behavior_date$late) #normal

leveneTest(incubation_rate ~ Season, data = behavior_data) #equal

t.test(behavior_date$early, behavior_date$late, 
       alternative = c('two.sided'),
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)
