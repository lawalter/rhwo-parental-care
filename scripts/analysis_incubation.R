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


# OTHER IMPORT ------------------------------------------------------------

c('https://drive.google.com/open?id=1E-BLyxQ-7hMJN7JVMbKbAftsBCQxjFB-ZVDEisfNc5c',
  'https://docs.google.com/spreadsheets/d/1Y5mKHQvY03K8YGkt5nvh3NAj93UiVhInpIx1NjBpH6M/edit?usp=sharing',
  'https://docs.google.com/spreadsheets/d/1KhdKjy8Fy4KrejdrwkJXj1ARfzpgg35g2-a9I1b6vw4/edit?usp=sharing') %>%
  map(
    function(x){
      df <-
        read.csv(text=gsheet2text(x, format='csv'), stringsAsFactors=FALSE, na.strings= c("", "na", "NA")) %>%
        as_tibble()
      if(sum(str_detect(names(df), 'Date'))){
        df <- mutate(df, Date = as.Date(Date, "%m/%d/%Y"))
      }
      if(sum(str_detect(names(df), 'Clutch_laid'))){
        df <- mutate(df, Clutch_laid = as.Date(Date, "%m/%d/%Y"))
      }
      df
    }
  ) %>% 
  set_names(c('sex_data', 'video_data_initial', 'boris_data')) %>%
  list2env(envir = .GlobalEnv)


# DATA TIDYING ------------------------------------------------------------

# Make a df with total sample size of n = 78
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
incubation_data <-
  boris_data %>% 
  filter(Behavior %in% c('in cavity', 'preening', 'stretch', 'stat')) %>%
  arrange(Observation.id, Start_sec) %>% 
  mutate_if(is.character, str_replace_all, pattern = 'in cavity', replacement = 'incubating') %>%
  left_join(video_data_initial, by = 'Observation.id') %>% 
  left_join(
    sex_data %>%
      select(Subject = Color_combo, Sex),
    by = 'Subject') %>%
  unite(vID, Video_number, Subject, sep = '.', remove = FALSE) %>%
  select(vID, Video_number, Subject, Sex, Brood_ID, Nest_ID,
         Behavior, Duration_sec, Usable_length, Date, Clutch_laid,
         Year, TA, Bpk_status_1, Bpk_status_2)  %>%
  left_join(
    filter(., Behavior != "stretch") %>%
      group_by(vID, Behavior) %>%
      summarize(x = mean(Duration_sec)/.$Usable_length) %>%
      spread(Behavior, x, fill = 0),
    by = "vID"
  ) %>%
  group_by(vID) %>%
  filter(row_number () == first(row_number()))

names(behavior_data)[2:5] <- c('incubation_rate', 'preen_rate', 'stat_rate')



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

# behavior_data <- 
behavior_data %>%
  as_tibble() %>%
  mutate_at(
    c('TMAX', 'TMIN'),
    function(x) {
      mutate(Std_tmin = (x - mean(x)) / sd(x)) %>%
        if_else(Std_tmin > 0, "hot", "cool")
    }) 

behavior_tmax <- 
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



# T-test for temperatures
plot(behavior_data$TMAX)




# T-TEST BY DATE ----------------------------------------------------------
behavior_data <- 
  behavior_data %>%
  mutate(Std_jdate = (Date - mean(Date)) / sd(Date)) %>%
  mutate(Temp = if_else(Std_jdate > 0, "hot", "cool"))
