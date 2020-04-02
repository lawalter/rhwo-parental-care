# intro -------------------------------------------------------------------

# Red-headed Woodpecker parental care behavior analysis 
# Determines rates of brooding, provisioning, and cleaning
# by: L. Abigail Walter

# last updates ------------------------------------------------------------

# Mar 2020: More modernizing
# Feb 2020: Modernized script using the tidyverse
# Jan 2020: Best updated version submitted to github
# Aug 2019: Corrected line creating bbyvid that averaged brooding by 
#            parent instead of summing the video parts
# Apr 2019: Fixed provisioning analysis to removed brooding by males if 
#            followed by cleaning event

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)

# import data -------------------------------------------------------------

# Sex data for individual Red-headed Woodpeckers

sex_data <-
  read.csv('raw_data/sex_data.csv', 
           na.strings = c('', 'na', 'NA'),
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) 

# Video data related about recordings

video_data_initial <- 
  read.csv('raw_data/provisioning_video_data.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) 

# Data from videos scored in Boris

boris_data <- 
  read.csv('raw_data/boris_provisioning_preyproofed_bestdata.csv', 
           na.strings = c("", "na", "NA"), 
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(
    names(.) %>% 
      tolower()) %>%
  arrange(observation.id, start_sec) 

# NOAA weather data

NOAA_data <-
  read.csv('raw_data/NOAA_weather_data.csv', 
           stringsAsFactors = FALSE) %>%
  as_tibble()

# tidying -----------------------------------------------------------------

# Total sample dataframe: make a df with total sample size of n = 288
# Both parents do not participate in all videos (without this step, the 
# data does not fill n = 288)

total_sample <- 
  video_data_initial %>%
  select(observation.id, ref_combo_1, ref_combo_2) %>%
  mutate(
    ID1 = paste(observation.id, ref_combo_1, sep = '.'),
    ID2 = paste(observation.id, ref_combo_2, sep = '.')) %>%
  select(-c(observation.id, ref_combo_1, ref_combo_2)) %>%
  gather() %>%
  select(-key, sample = value)

# cleaning table ----------------------------------------------------------

# Calculate the time difference between stop time of in cavity and the 
# time of cleaning event

#cleaning_new <-
c <-
  boris_data %>%
  select(-c(modifier_general, modifier_specific)) %>%
  filter(behavior == "in cavity" | behavior == "cleaning nest") %>%
  group_by(observation.id) %>%
  mutate(
    time_lag = lead(stop_sec),
    diff_sec = time_lag - stop_sec,
    behavior_lag = lead(behavior)) %>%
  ungroup %>%
  mutate(
    behavior = 
      case_when(
        (behavior == "in cavity" & duration_sec <= 60) ~ 'visit',
        (behavior == 'in cavity' & duration_sec > 60) ~ 'brooding',
        (behavior != "cleaning nest" & diff_sec < 1) ~ 'poopsearch',
        TRUE ~ behavior
      )
  ) %>% 
  select(-c(time_lag, diff_sec, behavior_lag))

# Merge final classifications of cleaning, visiting, brooding, and poopsearch with original data
boris <- 
  cleaning %>%
  bind_rows(boris_data)

# behavior data combined with video/nest data -----------------------------

# Merge boris behavior data and video/nest data
borisdata_w_nestinfo <- 
  boris_data %>%
  left_join(video_data_initial, by = "observation.id") 

# Make the sex data into 2 columns
sex_data_sm <- dplyr::select(sex_data, color_combo, sex)
names(sex_data_sm) <- c("subject", "sex")

# Merge the sex data with the main dataframe
# The merged dataframe will be shorter because it does not include chick events
provision_data <- merge.data.frame(borisdata_w_nestinfo, sex_data_sm, by="subject")


# julian dates ------------------------------------------------------------

provision_data$date <- as.Date(provision_data$date, "%m/%d/%Y")
provision_data$julian_date <- yday(provision_data$date)
provision_data$clutch_laid <- as.Date(provision_data$clutch_laid, "%m/%d/%Y")
provision_data$hatch_date <- as.Date(provision_data$hatch_date, "%m/%d/%Y")
provision_data$fledge_date <- as.Date(provision_data$fledge_date, "%m/%d/%Y")
provision_data$first_egg_julian <- yday(provision_data$clutch_laid)
provision_data$hatch_julian <- yday(provision_data$hatch_date)
provision_data$fledge_julian <- yday(provision_data$fledge_date)


# summary percentages -----------------------------------------------------

provision_data %>%
  mutate(
    proportion_hatched = max_number_chicks/max_number_eggs,
    percent_chick_mortalities = number_chick_mortalities/max_number_chicks,
    proportion_fledged = chicks_fledged/max_number_chicks)

### CALCULATE RATES OF PARENTAL CARE ###
# Double checked on 2/5/19 using Excel pivot table and they are correct

# Add a column of "1"s for counting purposes 
provision_data$count <- as.numeric(c("1"))

# behavior rate per video time
provision_data$countpermin <- provision_data$count/provision_data$length_usable

# Provisioning per chick per video time 
provision_data$countperchkmin <- 
  (provision_data$count/provision_data$peeped_chick_count)/provision_data$length_usable

# Multiply the rate per chick per video minutes by 60 to find the rate per hour
provision_data$countperchkhr <- provision_data$countperchkmin*60

# Convert duration of brooding from per sec to per minute by dividing by 60 
provision_data$duration_min <- provision_data$duration_sec/60

# Find duration of brooding per video by dividing duration (min) by length usable (min)
provision_data$duration_per_vid <- 
  provision_data$duration_min/provision_data$length_usable

# Find duration of brooding per hour by dividing duration (sec) by 60 
provision_data$brooding_min_per_hr <- provision_data$Duration_per_vid*60


### MAKE A PIVOT TABLE OF VIDEO BEHAVIORS BY SUBJECT ###

# Counts of behaviors
cast_provcount <- dcast(provision_data, observation.id + subject ~ behavior, 
                        value.var="count", fun.aggregate=sum)

# Sum of the rates: counts per chick hour
cast_prov <- dcast(provision_data, observation.id + subject ~ behavior, 
                   value.var="countperchkhr", fun.aggregate=sum)

# Duration per hour
cast_duration <- dcast(provision_data, observation.id + subject ~ behavior, 
                       value.var="duration_min", fun.aggregate=sum)


### COMBINE DATAFRAMES ###
# Create a behavior dataframe of rates of feeding chicks, cleaning, and brooding

# Select desired behaviors
behavior_rates_chkhr <- cast_prov[c(1, 2, 5, 7)]
behavior_durations <- cast_duration[c(1:3)]

# Assign an identifier for each video_subject to merge accurately
provision_data$sample <- paste(provision_data$observation.id, 
                               provision_data$subject, sep=".")
behavior_rates_chkhr$sample <- paste(behavior_rates_chkhr$observation.id, 
                                     behavior_rates_chkhr$subject, sep=".")
behavior_durations$sample <- paste(behavior_durations$observation.id, 
                                   behavior_durations$subject, sep=".")

# Remove the columns for Obs.id and subject which are redundant
behavior_rates_chkhr$observation.id <- NULL
behavior_rates_chkhr$subject <- NULL
behavior_durations$observation.id <- NULL
behavior_durations$subject <- NULL

# Merge the behavior rates and durations with the provisioning_data main dataframe
provision_data <- merge.data.frame(provision_data, behavior_rates_chkhr, by="sample")
provision_data <- merge.data.frame(provision_data, behavior_durations, by="sample")


### ADD ZEROS/MISSING OBSERVATIONS TO THE MAIN DATAFRAME ###
# brooding
are_brooding <- cast_duration[c(1:3)]
are_brooding$sample <- paste(are_brooding$observation.id, are_brooding$subject, sep=".")
are_brooding$observation.id <- NULL
are_brooding <- are_brooding[c("sample", "brooding")]

# Find what samples are missing from are_brooding compared to the total_sample
brooding_missing <- anti_join(total_sample, are_brooding)
brooding_missing$brooding <- as.numeric("0")

# Merge existing and missing brooding events
brooding <- rbind(are_brooding, brooding_missing)

# Provisioning and cleaning
are_pc <- cast_provcount[c(1, 2, 5, 7)]
are_pc$sample <- paste(are_pc$observation.id, are_pc$subject, sep=".")
are_pc$observation.id <- NULL
are_pc <- are_pc[c("sample", "feeding chicks", "cleaning nest")]

# Find what samples are missing from are_pc compared to the total_sample
pc_missing <- anti_join(total_sample, are_pc)
pc_missing$`feeding chicks` <- as.numeric("0")
pc_missing$`cleaning nest` <- as.numeric("0")

# Merge existing and missing provisioning and cleaning events
prov_clean <- rbind(are_pc, pc_missing)


### CALCULATE INTER-EVENT TIMES
# Subset data
feeding_chicks <- boris_data %>% filter(behavior == "feeding chicks")

cleaning <- boris_data %>% filter(behavior == "cleaning nest")

# Running calculations
# Feeding chicks
library(data.table)
interev_nas <- c("-Inf", "NaN")

interev_f <- data.table(feeding_chicks)[, list(mean = mean(diff(stop_sec)), 
                                               sd = sd(diff(stop_sec)), 
                                               max = max(diff(stop_sec)), 
                                               min = min(diff(stop_sec))), 
                                        by = observation.id]

interev_f$mean <- abs(interev_f$mean)
inter_f_sm <- as.data.frame(interev_f$observation.id)
inter_f_sm$provisioning_interevent_mean <- interev_f$mean
names(inter_f_sm)[1] <- c("observation.id")

# cleaning nests
interev_c <- data.table(cleaning)[, list(mean = mean(diff(stop_sec)), 
                                         sd = sd(diff(stop_sec)), 
                                         max = max(diff(stop_sec)), 
                                         min = min(diff(stop_sec))), 
                                  by = observation.id]

interev_c$mean <- abs(interev_c$mean)
inter_c_sm <- as.data.frame(interev_c$observation.id)
inter_c_sm$cleaning_interevent_mean <- interev_c$mean
names(inter_c_sm)[1] <- c("observation.id")

# Merging the means together
interevents <- merge(inter_c_sm, inter_f_sm, by = "observation.id", all = TRUE)
interevents[interevents == "NaN"] <- NA

# Convert seconds to minutes
interevents$cleaning_interevent_mean <- interevents$cleaning_interevent_mean/60
interevents$provisioning_interevent_mean <- interevents$provisioning_interevent_mean/60


### COMPILE DATAFRAME FOR MODELING ###
# Make a base dataframe with every sample (n = 288) and corresponding behavior values
prov_model <- total_sample
prov_model <- data.frame(do.call('rbind', strsplit(as.character(prov_model$sample),'.',fixed=TRUE)))
names(prov_model) <- c("observation.id", "subject")
prov_model$sample <- paste(prov_model$observation.id, prov_model$subject, sep=".")
prov_model <- merge.data.frame(prov_model, prov_clean, by = "sample")
prov_model <- merge.data.frame(prov_model, brooding, by = "sample")

# Make a shortened and condensed version of the provision_data df 
provision_data_short <- provision_data[!duplicated(provision_data$sample), ]
provision_data_sm <- dplyr::select(provision_data_short, c(1, 16:27, 59, 32:42, 44, 47:48, 51:53, 60))

# Make a dataframe of missing samples
missing <- brooding_missing
missing$brooding <- NULL
missing <- data.frame(do.call('rbind', strsplit(as.character(missing$sample),'.',fixed=TRUE)))
names(missing) <- c("observation.id", "subject")
missing$sample <- paste(missing$observation.id, missing$subject, sep=".")

# Add additional video/nest info to the missing sample df
# Because you are merging by observation.id here the sex data is incorrect!!!
miss <- merge(missing, provision_data, by = "observation.id")
miss2 <- dplyr::select(miss, c(3, 18:29, 61, 34:44, 46, 49:50, 53:55, 62))
miss2 <- miss2[!duplicated(miss2$sample.x), ]
names(miss2)[1] <- c("sample")

# Bind together the known data and missing data; sample will still be 10 short
almost_done <- rbind(provision_data_sm, miss2)

# Determine which samples are missing from the almost complete df
last_missing <- anti_join(prov_model, almost_done, by = "sample")

# Add video/nest data to these last missing samples
last_missing <- merge(last_missing, video_data_initial)

# Calculate Julian dates for the missing samples
last_missing$date <- as.Date(last_missing$date, "%m/%d/%Y")
last_missing$julian_date <- yday(last_missing$date)
last_missing$clutch_laid <- as.Date(last_missing$clutch_laid, "%m/%d/%Y")
last_missing$hatch_date <- as.Date(last_missing$hatch_date, "%m/%d/%Y")
last_missing$fledge_date <- as.Date(last_missing$fledge_date, "%m/%d/%Y")
last_missing$first_egg_Julian <- yday(last_missing$clutch_laid)
last_missing$hatch_Julian <- yday(last_missing$hatch_date)
last_missing$fledge_Julian <- yday(last_missing$fledge_date)

# Calculate additional nest data for the missing samples
last_missing2 <- dplyr::select(last_missing, c(2, 11:22, 27:37, 39, 42:43, 46:48 ))
last_missing2$proportion_hatched <- last_missing2$max_number_chicks/last_missing2$max_number_eggs
last_missing2$percent_chick_mortalities <- 
  (last_missing2$number_chick_mortalities)/(last_missing2$max_number_chicks) 

# Clean the last missing sample data
last_missing4 <- data.frame(do.call('rbind', strsplit(as.character(last_missing2$sample),'.',fixed=TRUE)))
names(last_missing4) <- c("observation.id", "subject")
last_missing4$sample <- paste(last_missing4$observation.id, last_missing4$subject, sep=".")
last_missing4 <- merge(last_missing4, sex_data_sm, by = "subject")
last_missing3 <- merge (last_missing2, last_missing4, by = "sample")
last_missing3$subject <- NULL
last_missing3$observation.id <- NULL

# Merge the last missing sample data with the known samples
final <- rbind(almost_done, last_missing3)
final <- merge(final, prov_model)
final$Day <- NULL
library(data.table)
setnames(final, "sex", "sex_wrong")
final <- merge(final, sex_data_sm, by = "subject")
final$sex_wrong <- NULL

# Add interevents to final
final <- merge(final, interevents, by = "observation.id", all = TRUE)

# Write actual counts back into the dataframe
final$feeding <- 
  ((final$`feeding chicks`*final$peeped_chick_count)*final$length_usable)/60      

final <- as_tibble(final)

# Organize the column names
final_prov_rates <- final[c("sample", "observation.id", "subject", "sex", "feeding chicks", "feeding",
                            "cleaning nest", "brooding", "provisioning_interevent_mean", 
                            "cleaning_interevent_mean", "nest_id", "brood_id", "month", 
                            "julian_date", "year", "ta", "habitat", "first_egg_julian", 
                            "hatch_julian", "fledge_julian", "exact_age_chick", 
                            "peeped_chick_count", "max_number_eggs", "max_number_chicks", 
                            "proportion_hatched", "chicks_fledged", "proportion_fledged", 
                            "nest_fate", "nest_fate_certainty", "oldest_nestling", 
                            "number_chick_mortalities", "percent_chick_mortalities", 
                            "chicks_visible.", "with_bpk.", "start_time", "early_or_late", 
                            "nearest_neighbor", "distance_nearest_neighbor_m", "length_usable")]

#################
###!!! YAY !!!###
#################

    

#################################      
### MAKE A BY-VIDEO DATAFRAME ###
#################################

### Provisioning and cleaning

# Make a df to make each line a sample by Video_id
# This link is the final product of the above script (?)
byobsid_url <- 'https://drive.google.com/open?id=1bGtwJyH6WK5Vipd0_2edK-6QiqhcEynnMMA94NmFxDA'
byobsid <- 
  data.get(byobsid_url) %>%
  set_names(
    names(.) %>% 
      tolower())

final_ids <- dplyr::select(final, sample, observation.id)
vidnos <- dplyr::select(video_data_initial, observation.id, video_number)

# Clean the dataframe  
byobsid <- merge(byobsid, final_ids, by = "sample")
byobsid <- merge(byobsid, vidnos, by = "observation.id")
byobsid$feeding.chicks <- NULL
byobsid$cleaning.nest <- NULL
byobsid$brooding <- NULL
byobsid$cleaning_interevent_mean <- NULL
byobsid$provisioning_interevent_mean <- NULL

# Add a vID as unique video identifier 
byobsid$vID <- paste(byobsid$video_number, byobsid$subject, sep=".")

# Count the behavior occurrences by vID    
cast_provbynumber <- dcast(provision_data, video_number + subject ~ behavior, 
                           value.var="count", fun.aggregate=sum)

# Merge vID, feeding, and cleaning data
pbn_feeding <- dplyr::select(cast_provbynumber, video_number, subject, `feeding chicks`)
pbn_feeding$vID <- paste(pbn_feeding$video_number, pbn_feeding$subject, sep=".")
pbn_cleaning <- dplyr::select(cast_provbynumber, video_number, subject, `cleaning nest`)
pbn_cleaning$vID <- paste(pbn_cleaning$video_number, pbn_cleaning$subject, sep=".")
pbn_fc <- merge(pbn_feeding, pbn_cleaning, by = "vID")
pbn_fc$video_number.y <- NULL
pbn_fc$subject.y <- NULL
names(pbn_fc)[2] <- c("video_number")
names(pbn_fc)[3] <- c("subject")

# Merge video data with behavior data
byobsid_wdata <- merge(byobsid, pbn_fc, by = "vID")
names(byobsid_wdata)[36] <- c("video_number")
byobsid_wdata$video_number.y <- NULL

# Remove duplicates so that the sample is vID.sample
byobsid_wdata <- byobsid_wdata[!duplicated(byobsid_wdata$vID), ]
byobsid_wdata$subject.x <- NULL
names(byobsid_wdata)[36] <- c("subject")
names(byobsid_wdata)[37] <- c("feeding")
names(byobsid_wdata)[38] <- c("cleaning")

# sample size should be n = 90. There are 45 unique video numbers.
byob_missing <- anti_join(byobsid, byobsid_wdata, by = "vID")
byob_missing <- byob_missing[!duplicated(byob_missing$vID), ]
byob_missing$feeding <- 0
byob_missing$cleaning <- 0

# Merge the by-video final dataframe together
byob_final <- rbind(byobsid_wdata, byob_missing)
total_length <- dplyr::select(video_data_initial, observation.id, length_total)
byob_final <- merge(byob_final, total_length, by = "observation.id")

### brooding
# Sum the brooding durations by vID    
cast_broodbynumber <- dcast(provision_data, video_number + subject ~ behavior, 
                            value.var="duration_sec", fun.aggregate=sum)

# Merge vID and brooding data 
bbn <- dplyr::select(cast_broodbynumber, video_number, subject, brooding)
bbn$vID <- paste(bbn$video_number, bbn$subject, sep=".")
bbn[bbn=="NaN"] <- 0

# Merge video data with brooding data
byobsid_wbrooding <- merge(byobsid, bbn, by = "vID")
names(byobsid_wbrooding)[36] <- c("video_number")
byobsid_wbrooding$video_number.y <- NULL

# Remove duplicates so that the sample is vID.sample
byobsid_wbrooding <- byobsid_wbrooding[!duplicated(byobsid_wbrooding$vID), ]
byobsid_wbrooding$subject.x <- NULL
names(byobsid_wbrooding)[36] <- c("subject")
names(byobsid_wbrooding)[37] <- c("brooding_sec")

# sample size should be n = 90. There are 45 unique video numbers.
byob_missing_brooding <- anti_join(byobsid, byobsid_wbrooding, by = "vID")
byob_missing_brooding <- byob_missing_brooding[!duplicated(byob_missing_brooding$vID), ]
byob_missing_brooding$brooding_sec <- 0

# Merge the by-video final dataframe together
byob_brooding_final <- rbind(byobsid_wbrooding, byob_missing_brooding)
total_length <- dplyr::select(video_data_initial, observation.id, length_total)
byob_brooding_final <- merge(byob_brooding_final, total_length, by = "observation.id")      

### Merging all behaviors by video
# Merge the by-video final dataframe together
subset_brooding <- byob_brooding_final %>% dplyr::select(vID, brooding_sec)
allbehaviors_byvid <- merge(byob_final, subset_brooding, by = "vID", all = FALSE)

# Calculate standardized Julian date
allbehaviors_byvid$std_jdate <- 
  (allbehaviors_byvid$julian_date - 
     mean(allbehaviors_byvid$julian_date))/sd(allbehaviors_byvid$julian_date)

# Calculate brooding by min, and brooding by video
allbehaviors_byvid$brooding_min <- allbehaviors_byvid$brooding_sec/60

# Sum of the lengths usable
cast_length <- dcast(video_data_initial, video_number ~ priority, 
                     value.var="length_usable", fun.aggregate=sum) 
names(cast_length)[2] <- c("usable_video")

# Merge together lengths usable with video_number
allbehaviors_byvid <- merge(allbehaviors_byvid, cast_length, by = "video_number")

# Remove PNA5A1b outlier
bbyvid <- 
  allbehaviors_byvid %>% 
  filter(brood_id != "PNA5A1b_brd1")


# import NOAA weather data ------------------------------------------------

# Calculate Julian dates for the NOAA data
NOAA_data <-
  NOAA_data %>%
  mutate(
    good_date = as.Date(DATE, "%Y-%m-%d"),
    julian_date = lubridate::yday(good_date),
    yr = str_sub(DATE, 1, 4),
    jdayyr = paste(julian_date, yr, sep = '.')) %>%
  select(tmax = TMAX, jdayyr)

### Add max day temps to df
bbyvid <-
  bbyvid %>%
  mutate(jdayyr = paste(julian_date, year, sep = '.')) %>%
  left_join(NOAA_data, by = 'jdayyr')

# write csv ---------------------------------------------------------------

write.csv(bbyvid, "clean_data/bbyvid.csv")       
