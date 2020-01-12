## Red-headed Woodpecker parental care behavior analysis 
## Determines rates of brooding, provisioning, and cleaning
## By: Lynn Abigail Walter
## Last Updates:
#      Jan 2020 - Best updated version submitted to github
#      Aug 2019 - Corrected line creating bbyvid that averaged brooding by parent instead of summing the video parts
#      Apr 2019 - Fixed provisioning analysis to removed brooding by males if followed by cleaning event

library(gsheet)
library(magrittr)
library(dplyr)
library(lubridate)
library(sp)

### IMPORT DATA ###
# Import all data from google drive links
sex_url <- 'https://drive.google.com/open?id=1E-BLyxQ-7hMJN7JVMbKbAftsBCQxjFB-ZVDEisfNc5c'
video_data_prov_url <- 'https://drive.google.com/open?id=1ifQrZegnwghLtEJKEHiLDzZUTyvVB3TN3jA6SYt2lbk'
boris_preyproofed_url <- 'https://drive.google.com/open?id=1m2KZvIUixReKW531jUFiW1FVXAZb-iLjCArnv8Sj4kg'

na_strings <- c("", "na", "NA")

data.get <- function(url){
  read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings= na_strings)
}

sex_data <- data.get(sex_url)
video_data_initial <- data.get(video_data_prov_url)
boris_data <- data.get(boris_preyproofed_url)


### TOTAL SAMPLE DATAFRAME ###
# Make a df with total sample size of n = 288
# Both parents do not participate in all videos
total_sample <- video_data_initial[c("Observation.id", "Ref_combo_1", "Ref_combo_2")]
total_sample$ID1 <- paste(total_sample $Observation.id, total_sample$Ref_combo_1, sep=".")
total_sample$ID2 <- paste(total_sample $Observation.id, total_sample$Ref_combo_2, sep=".")
total_sample$Observation.id <- NULL
total_sample$Ref_combo_1 <- NULL
total_sample$Ref_combo_2 <- NULL
total_sample <- stack(total_sample)
total_sample$ind <- NULL
names(total_sample) <- c("Sample")


### DATA TIDYING ###
boris_data <- boris_data[with(boris_data, order(Observation.id, Start_sec)),]

# Remove cleaning and in cavity from the base dataframe 
withoutclean <- boris_data %>% filter(Behavior != "in cavity" & Behavior != "cleaning nest")

# Create a dataframe to look at in cavity and cleaning
clean <- boris_data %>% filter(Behavior == "in cavity" | Behavior == "cleaning nest")

# Calculate the time difference between stop time of in cavity and the time cleaning happens
library(DataCombine)
clean <- slide(clean, Var = "Stop_sec", GroupVar = "Observation.id", slideBy = 1)
clean$diff_sec <-  clean$Stop_sec1 - clean$Stop_sec
clean <- slide(clean, Var = "Behavior", GroupVar = "Observation.id", slideBy = 1)

# Subset cleaning out and put it back with the main dataframe
cleaning_done <- clean %>% filter(Behavior == "cleaning nest")
cleaning_done$Stop_sec1 <- NULL
cleaning_done$diff_sec <- NULL
cleaning_done$Behavior1 <- NULL
withoutclean <- rbind(withoutclean, cleaning_done)

# Figure out which in_cavity events are brooding or just long-cleaning
clean <- clean %>% filter(Behavior != "cleaning nest")
clean$Behavior <- ifelse(clean$Duration_sec <= 60, "visit", clean$Behavior) 

# Subset visits out and put it back with the main dataframe
visiting_done <- clean %>% filter(Behavior == "visit")
visiting_done$Stop_sec1 <- NULL
visiting_done$diff_sec <- NULL
visiting_done$Behavior1 <- NULL
withoutclean <- rbind(withoutclean, visiting_done)

# Remove visiting from cleaning df
clean <- clean %>% filter(Behavior != "visit")
clean$Behavior <- ifelse(clean$diff_sec < 1, "poopsearch", clean$Behavior) 
clean$Behavior <- ifelse(clean$Behavior == "in cavity", "brooding", clean$Behavior)  
clean$Behavior <- ifelse(is.na(clean$Behavior), "brooding", clean$Behavior) 
clean$Stop_sec1 <- NULL
clean$diff_sec <- NULL
clean$Behavior1 <- NULL

# Merge final classifications of brooding to initial dataframe
withoutclean <- rbind(withoutclean, clean)
boris_data <- withoutclean

# Merge boris behavior data and video/nest data
borisdata_w_nestinfo <- merge.data.frame(boris_data, video_data_initial, by="Observation.id")

# Make the sex data into 2 columns
sex_data_sm <- dplyr::select(sex_data, Color_combo, Sex)
names(sex_data_sm) <- c("Subject", "Sex")

# Merge the sex data with the main dataframe
# The merged dataframe will be shorter because it does not include chick events
provision_data <- merge.data.frame(borisdata_w_nestinfo, sex_data_sm, by="Subject")


### DETERMINE JULIAN DATES ###
library(lubridate)
provision_data$Date <- as.Date(provision_data$Date, "%m/%d/%Y")
provision_data$Julian_date <- yday(provision_data$Date)
provision_data$Clutch_laid <- as.Date(provision_data$Clutch_laid, "%m/%d/%Y")
provision_data$Hatch_date <- as.Date(provision_data$Hatch_date, "%m/%d/%Y")
provision_data$Fledge_date <- as.Date(provision_data$Fledge_date, "%m/%d/%Y")
provision_data$First_egg_Julian <- yday(provision_data$Clutch_laid)
provision_data$Hatch_Julian <- yday(provision_data$Hatch_date)
provision_data$Fledge_Julian <- yday(provision_data$Fledge_date)


### CALCULATE PERCENTAGES ###
# Find proportion hatched 
provision_data$Proportion_hatched <- 
  (provision_data$Max_number_chicks)/(provision_data$Max_number_eggs)

# Find percent of chick mortality 
provision_data$Percent_chick_mortalities <- 
  (provision_data$Number_chick_mortalities)/(provision_data$Max_number_chicks)

# Find the proportion fledged 
provision_data$Proportion_fledged <- 
  (provision_data$Chicks_fledged)/(provision_data$Max_number_chicks)


### CALCULATE RATES OF PARENTAL CARE ###
# Double checked on 2/5/19 using Excel pivot table and they are correct

# Add a column of "1"s for counting purposes 
provision_data$Count <- as.numeric(c("1"))

# Behavior rate per video time
provision_data$Countpermin <- provision_data$Count/provision_data$Length_usable

# Provisioning per chick per video time 
provision_data$Countperchkmin <- 
  (provision_data$Count/provision_data$Peeped_chick_count)/provision_data$Length_usable

# Multiply the rate per chick per video minutes by 60 to find the rate per hour
provision_data$Countperchkhr <- provision_data$Countperchkmin*60

# Convert duration of brooding from per sec to per minute by dividing by 60 
provision_data$Duration_min <- provision_data$Duration_sec/60

# Find duration of brooding per video by dividing duration (min) by length usable (min)
provision_data$Duration_per_vid <- 
  provision_data$Duration_min/provision_data$Length_usable

# Find duration of brooding per hour by dividing duration (sec) by 60 
provision_data$Brooding_min_per_hr <- provision_data$Duration_per_vid*60


### MAKE A PIVOT TABLE OF VIDEO BEHAVIORS BY SUBJECT ###
library(reshape2)

# Counts of behaviors
cast_provcount <- dcast(provision_data, Observation.id + Subject ~ Behavior, 
                        value.var="Count", fun.aggregate=sum)

# Sum of the rates: counts per chick hour
cast_prov <- dcast(provision_data, Observation.id + Subject ~ Behavior, 
                   value.var="Countperchkhr", fun.aggregate=sum)

# Duration per hour
cast_duration <- dcast(provision_data, Observation.id + Subject ~ Behavior, 
                       value.var="Duration_min", fun.aggregate=sum)


### COMBINE DATAFRAMES ###
# Create a behavior dataframe of rates of feeding chicks, cleaning, and brooding

# Select desired behaviors
behavior_rates_chkhr <- cast_prov[c(1, 2, 5, 7)]
behavior_durations <- cast_duration[c(1:3)]

# Assign an identifier for each video_subject to merge accurately
provision_data$Sample <- paste(provision_data$Observation.id, 
                               provision_data$Subject, sep=".")
behavior_rates_chkhr$Sample <- paste(behavior_rates_chkhr$Observation.id, 
                                     behavior_rates_chkhr$Subject, sep=".")
behavior_durations$Sample <- paste(behavior_durations$Observation.id, 
                                   behavior_durations$Subject, sep=".")

# Remove the columns for Obs.id and Subject which are redundant
behavior_rates_chkhr$Observation.id <- NULL
behavior_rates_chkhr$Subject <- NULL
behavior_durations$Observation.id <- NULL
behavior_durations$Subject <- NULL

# Merge the behavior rates and durations with the provisioning_data main dataframe
provision_data <- merge.data.frame(provision_data, behavior_rates_chkhr, by="Sample")
provision_data <- merge.data.frame(provision_data, behavior_durations, by="Sample")


### ADD ZEROS/MISSING OBSERVATIONS TO THE MAIN DATAFRAME ###
# Brooding
are_brooding <- cast_duration[c(1:3)]
are_brooding$Sample <- paste(are_brooding$Observation.id, are_brooding$Subject, sep=".")
are_brooding$Observation.id <- NULL
are_brooding <- are_brooding[c("Sample", "brooding")]

# Find what samples are missing from are_brooding compared to the total_sample
brooding_missing <- anti_join(total_sample, are_brooding)
brooding_missing$brooding <- as.numeric("0")

# Merge existing and missing brooding events
brooding <- rbind(are_brooding, brooding_missing)

# Provisioning and cleaning
are_pc <- cast_provcount[c(1, 2, 5, 7)]
are_pc$Sample <- paste(are_pc$Observation.id, are_pc$Subject, sep=".")
are_pc$Observation.id <- NULL
are_pc <- are_pc[c("Sample", "feeding chicks", "cleaning nest")]

# Find what samples are missing from are_pc compared to the total_sample
pc_missing <- anti_join(total_sample, are_pc)
pc_missing$`feeding chicks` <- as.numeric("0")
pc_missing$`cleaning nest` <- as.numeric("0")

# Merge existing and missing provisioning and cleaning events
prov_clean <- rbind(are_pc, pc_missing)


### CALCULATE INTER-EVENT TIMES
# Subset data
feeding_chicks <- boris_data %>% filter(Behavior == "feeding chicks")

cleaning <- boris_data %>% filter(Behavior == "cleaning nest")

# Running calculations
# Feeding chicks
library(data.table)
interev_nas <- c("-Inf", "NaN")

interev_f <- data.table(feeding_chicks)[, list(mean = mean(diff(Stop_sec)), 
                                               sd = sd(diff(Stop_sec)), 
                                               max = max(diff(Stop_sec)), 
                                               min = min(diff(Stop_sec))), 
                                        by = Observation.id]

interev_f$mean <- abs(interev_f$mean)
inter_f_sm <- as.data.frame(interev_f$Observation.id)
inter_f_sm$Provisioning_interevent_mean <- interev_f$mean
names(inter_f_sm)[1] <- c("Observation.id")

# Cleaning nests
interev_c <- data.table(cleaning)[, list(mean = mean(diff(Stop_sec)), 
                                         sd = sd(diff(Stop_sec)), 
                                         max = max(diff(Stop_sec)), 
                                         min = min(diff(Stop_sec))), 
                                  by = Observation.id]

interev_c$mean <- abs(interev_c$mean)
inter_c_sm <- as.data.frame(interev_c$Observation.id)
inter_c_sm$Cleaning_interevent_mean <- interev_c$mean
names(inter_c_sm)[1] <- c("Observation.id")

# Merging the means together
interevents <- merge(inter_c_sm, inter_f_sm, by = "Observation.id", all = TRUE)
interevents[interevents == "NaN"] <- NA

# Convert seconds to minutes
interevents$Cleaning_interevent_mean <- interevents$Cleaning_interevent_mean/60
interevents$Provisioning_interevent_mean <- interevents$Provisioning_interevent_mean/60


### COMPILE DATAFRAME FOR MODELING ###
# Make a base dataframe with every sample (n = 288) and corresponding behavior values
prov_model <- total_sample
prov_model <- data.frame(do.call('rbind', strsplit(as.character(prov_model$Sample),'.',fixed=TRUE)))
names(prov_model) <- c("Observation.id", "Subject")
prov_model$Sample <- paste(prov_model$Observation.id, prov_model$Subject, sep=".")
prov_model <- merge.data.frame(prov_model, prov_clean, by = "Sample")
prov_model <- merge.data.frame(prov_model, brooding, by = "Sample")

# Make a shortened and condensed version of the provision_data df 
provision_data_short <- provision_data[!duplicated(provision_data$Sample), ]
provision_data_sm <- dplyr::select(provision_data_short, c(1, 16:27, 59, 32:42, 44, 47:48, 51:53, 60))

# Make a dataframe of missing samples
missing <- brooding_missing
missing$brooding <- NULL
missing <- data.frame(do.call('rbind', strsplit(as.character(missing$Sample),'.',fixed=TRUE)))
names(missing) <- c("Observation.id", "Subject")
missing$Sample <- paste(missing$Observation.id, missing$Subject, sep=".")

# Add additional video/nest info to the missing sample df
# Because you are merging by Observation.id here the sex data is incorrect!!!
miss <- merge(missing, provision_data, by = "Observation.id")
miss2 <- dplyr::select(miss, c(3, 18:29, 61, 34:44, 46, 49:50, 53:55, 62))
miss2 <- miss2[!duplicated(miss2$Sample.x), ]
names(miss2)[1] <- c("Sample")

# Bind together the known data and missing data; sample will still be 10 short
almost_done <- rbind(provision_data_sm, miss2)

# Determine which samples are missing from the almost complete df
last_missing <- anti_join(prov_model, almost_done, by = "Sample")

# Add video/nest data to these last missing samples
last_missing <- merge(last_missing, video_data_initial)

# Calculate Julian dates for the missing samples
last_missing$Date <- as.Date(last_missing$Date, "%m/%d/%Y")
last_missing$Julian_date <- yday(last_missing$Date)
last_missing$Clutch_laid <- as.Date(last_missing$Clutch_laid, "%m/%d/%Y")
last_missing$Hatch_date <- as.Date(last_missing$Hatch_date, "%m/%d/%Y")
last_missing$Fledge_date <- as.Date(last_missing$Fledge_date, "%m/%d/%Y")
last_missing$First_egg_Julian <- yday(last_missing$Clutch_laid)
last_missing$Hatch_Julian <- yday(last_missing$Hatch_date)
last_missing$Fledge_Julian <- yday(last_missing$Fledge_date)

# Calculate additional nest data for the missing samples
last_missing2 <- dplyr::select(last_missing, c(2, 11:22, 27:37, 39, 42:43, 46:48 ))
last_missing2$Proportion_hatched <- last_missing2$Max_number_chicks/last_missing2$Max_number_eggs
last_missing2$Percent_chick_mortalities <- 
  (last_missing2$Number_chick_mortalities)/(last_missing2$Max_number_chicks) 

# Clean the last missing sample data
last_missing4 <- data.frame(do.call('rbind', strsplit(as.character(last_missing2$Sample),'.',fixed=TRUE)))
names(last_missing4) <- c("Observation.id", "Subject")
last_missing4$Sample <- paste(last_missing4$Observation.id, last_missing4$Subject, sep=".")
last_missing4 <- merge(last_missing4, sex_data_sm, by = "Subject")
last_missing3 <- merge (last_missing2, last_missing4, by = "Sample")
last_missing3$Subject <- NULL
last_missing3$Observation.id <- NULL

# Merge the last missing sample data with the known samples
final <- rbind(almost_done, last_missing3)
final <- merge(final, prov_model)
final$Day <- NULL
library(data.table)
setnames(final, "Sex", "Sex_wrong")
final <- merge(final, sex_data_sm, by = "Subject")
final$Sex_wrong <- NULL

# Add interevents to final
final <- merge(final, interevents, by = "Observation.id", all = TRUE)

# Write actual counts back into the dataframe
final$feeding <- 
  ((final$`feeding chicks`*final$Peeped_chick_count)*final$Length_usable)/60      

# Organize the column names
final_prov_rates <- final[c("Sample", "Observation.id", "Subject", "Sex", "feeding chicks", "feeding",
                            "cleaning nest", "brooding", "Provisioning_interevent_mean", 
                            "Cleaning_interevent_mean", "Nest_ID", "Brood_ID", "Month", 
                            "Julian_date", "Year", "TA", "Habitat", "First_egg_Julian", 
                            "Hatch_Julian", "Fledge_Julian", "Exact_age_chick", 
                            "Peeped_chick_count", "Max_number_eggs", "Max_number_chicks", 
                            "Proportion_hatched", "Chicks_fledged", "Proportion_fledged", 
                            "Nest_fate", "Nest_fate_certainty", "Oldest_nestling", 
                            "Number_chick_mortalities", "Percent_chick_mortalities", 
                            "Chicks_visible.", "With_bpk.", "Start_time", "Early_or_late", 
                            "Nearest_neighbor", "Distance_nearest_neighbor_m", "Length_usable")]

#################
###!!! YAY !!!###
#################

# Write as csv
# write.csv(final, "final_data_provisioning.csv")            


###################################################################################      

#################################      
### MAKE A BY-VIDEO DATAFRAME ###
#################################

### Provisioning and cleaning

# Make a df to make each line a sample by Video_id
# This link is the final product of the above script (?)
byobsid_url <- 'https://drive.google.com/open?id=1bGtwJyH6WK5Vipd0_2edK-6QiqhcEynnMMA94NmFxDA'
byobsid <- data.get(byobsid_url) 

final_ids <- dplyr::select(final, Sample, Observation.id)
vidnos <- dplyr::select(video_data_initial, Observation.id, Video_number)

# Clean the dataframe  
byobsid <- merge(byobsid, final_ids, by = "Sample")
byobsid <- merge(byobsid, vidnos, by = "Observation.id")
byobsid$feeding.chicks <- NULL
byobsid$cleaning.nest <- NULL
byobsid$brooding <- NULL
byobsid$Cleaning_interevent_mean <- NULL
byobsid$Provisioning_interevent_mean <- NULL

# Add a vID as unique video identifier 
byobsid$vID <- paste(byobsid$Video_number, byobsid$Subject, sep=".")

# Count the behavior occurrences by vID    
cast_provbynumber <- dcast(provision_data, Video_number + Subject ~ Behavior, 
                           value.var="Count", fun.aggregate=sum)

# Merge vID, feeding, and cleaning data
pbn_feeding <- dplyr::select(cast_provbynumber, Video_number, Subject, `feeding chicks`)
pbn_feeding$vID <- paste(pbn_feeding$Video_number, pbn_feeding$Subject, sep=".")
pbn_cleaning <- dplyr::select(cast_provbynumber, Video_number, Subject, `cleaning nest`)
pbn_cleaning$vID <- paste(pbn_cleaning$Video_number, pbn_cleaning$Subject, sep=".")
pbn_fc <- merge(pbn_feeding, pbn_cleaning, by = "vID")
pbn_fc$Video_number.y <- NULL
pbn_fc$Subject.y <- NULL
names(pbn_fc)[2] <- c("Video_number")
names(pbn_fc)[3] <- c("Subject")

# Merge video data with behavior data
byobsid_wdata <- merge(byobsid, pbn_fc, by = "vID")
names(byobsid_wdata)[36] <- c("Video_number")
byobsid_wdata$Video_number.y <- NULL

# Remove duplicates so that the sample is vID.Sample
byobsid_wdata <- byobsid_wdata[!duplicated(byobsid_wdata$vID), ]
byobsid_wdata$Subject.x <- NULL
names(byobsid_wdata)[36] <- c("Subject")
names(byobsid_wdata)[37] <- c("feeding")
names(byobsid_wdata)[38] <- c("cleaning")

# Sample size should be n = 90. There are 45 unique video numbers.
byob_missing <- anti_join(byobsid, byobsid_wdata, by = "vID")
byob_missing <- byob_missing[!duplicated(byob_missing$vID), ]
byob_missing$feeding <- 0
byob_missing$cleaning <- 0

# Merge the by-video final dataframe together
byob_final <- rbind(byobsid_wdata, byob_missing)
total_length <- dplyr::select(video_data_initial, Observation.id, Length_total)
byob_final <- merge(byob_final, total_length, by = "Observation.id")

### Brooding
# Sum the brooding durations by vID    
cast_broodbynumber <- dcast(provision_data, Video_number + Subject ~ Behavior, 
                            value.var="Duration_sec", fun.aggregate=sum)

# Merge vID and brooding data 
bbn <- dplyr::select(cast_broodbynumber, Video_number, Subject, brooding)
bbn$vID <- paste(bbn$Video_number, bbn$Subject, sep=".")
bbn[bbn=="NaN"] <- 0

# Merge video data with brooding data
byobsid_wbrooding <- merge(byobsid, bbn, by = "vID")
names(byobsid_wbrooding)[36] <- c("Video_number")
byobsid_wbrooding$Video_number.y <- NULL

# Remove duplicates so that the sample is vID.Sample
byobsid_wbrooding <- byobsid_wbrooding[!duplicated(byobsid_wbrooding$vID), ]
byobsid_wbrooding$Subject.x <- NULL
names(byobsid_wbrooding)[36] <- c("Subject")
names(byobsid_wbrooding)[37] <- c("brooding_sec")

# Sample size should be n = 90. There are 45 unique video numbers.
byob_missing_brooding <- anti_join(byobsid, byobsid_wbrooding, by = "vID")
byob_missing_brooding <- byob_missing_brooding[!duplicated(byob_missing_brooding$vID), ]
byob_missing_brooding$brooding_sec <- 0

# Merge the by-video final dataframe together
byob_brooding_final <- rbind(byobsid_wbrooding, byob_missing_brooding)
total_length <- dplyr::select(video_data_initial, Observation.id, Length_total)
byob_brooding_final <- merge(byob_brooding_final, total_length, by = "Observation.id")      

### Merging all behaviors by video
# Merge the by-video final dataframe together
subset_brooding <- byob_brooding_final %>% dplyr::select(vID, brooding_sec)
allbehaviors_byvid <- merge(byob_final, subset_brooding, by = "vID", all = FALSE)

# Calculate standardized Julian date
allbehaviors_byvid$Std_jdate <- 
  (allbehaviors_byvid$Julian_date - 
     mean(allbehaviors_byvid$Julian_date))/sd(allbehaviors_byvid$Julian_date)

# Calculate brooding by min, and brooding by video
allbehaviors_byvid$brooding_min <- allbehaviors_byvid$brooding_sec/60

# Sum of the lengths usable
cast_length <- dcast(video_data_initial, Video_number ~ Priority, 
                     value.var="Length_usable", fun.aggregate=sum) 
names(cast_length)[2] <- c("Usable_video")

# Merge together lengths usable with video_number
allbehaviors_byvid <- merge(allbehaviors_byvid, cast_length, by = "Video_number")

# Remove PNA5A1b outlier
bbyvid <- allbehaviors_byvid %>% 
  filter(Brood_ID != "PNA5A1b_brd1")

### Import NOAA weather data
NOAA_url <- 'https://drive.google.com/open?id=1hKlT1dcAO6jA1DkgFUhQ61ZZsCLMhsfN12oiXzfj6go'
NOAA_data <- data.get(NOAA_url)

# Calculate Julian dates for the NOAA data
NOAA_data$DATE <- as.Date(NOAA_data$DATE, "%Y-%m-%d")
NOAA_data$Julian_date <- yday(NOAA_data$DATE)
library(stringr)
NOAA_data$Year <- str_sub(NOAA_data$DATE, 1, 4)
NOAA_data$Jdayyr <- paste(NOAA_data$Julian_date, NOAA_data$Year, sep=".")
NOAA_sm <- select(NOAA_data, TMAX, Jdayyr)


### Add max day temps to df
bbyvid$Jdayyr <- paste(bbyvid$Julian_date, bbyvid$Year, sep=".")
bbyvid <- merge(bbyvid, NOAA_sm, by = "Jdayyr")

# Write as csv
#write.csv(bbyvid, "clean_data/bbyvid.csv")       
