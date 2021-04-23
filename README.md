# Data and Script Repository: Red-headed Woodpecker Parental Care 

## Introduction

This repository contains data, code, analyses, and plots by L. Abigail Walter for research conducted with the [Bulluck Avian Ecology Lab](https://rampages.us/bullucklab) at Virginia Commonwealth University. 

### Requirements

- <b>R Version:</b> v3.5.2+
- <b>Packages:</b> tidyverse, lubridate, reshape2, glmmTMB, AICtab, psych, cowplot, emojifont

## Data and R scripts overview

The R scripts included in this repository are for wrangling, tidying, analyzing, and plotting RHWO behavior data. Please note that it is not strictly necessary to complete Step 1; clean data files are already included in `/clean_data`. 

### Step 1: Data wrangling

Although the clean data files are already included in this repo, their creation is nevertheless outlined below for both incubation and post-hatch parental care.

#### Incubation

- Run `scripts/incubation/wrangling_incubation.R` to wrangle and tidy the raw data into `clean_data/incubation.rds`.

#### Brooding, provisioning, and nest cleaning

- Run `scripts/wrangling_parentalcare.R` to wrangle and tidy the raw data into `clean_data/behaviors.csv`.

### Step 2: Data analyses

The following scripts are used to analyze incubation, brooding, provisioning, and nest cleaning behaviors using the clean data files (generated above).

#### Incubation

- To analyze the incubation data, run `scripts/incubation/incubation_analysis.R`

#### Brooding

1. Run `scripts/brooding/brooding_models.R` to follow the modeling process and see how the "top GLMM" was chosen.
2. Plot predictions using `scripts/brooding/brooding_plots.R`

#### Provisioning

1. Run `scripts/provisioning/provisioning_models.R` to follow the modeling process and see how the "top GLMM" was chosen.
2. Plot predictions using `scripts/provisioning/provisioning_plots.R`
3. Plot individual variation in provisioning using `scripts/provisioning/provisioning_individuals.R`
4. Run additional analyses on reasons for variation in female provisioning using `scripts/provisioning/provisioning_individuals_extra.R`

#### Nest cleaning

- Run `scripts/cleaning/cleaning_analysis.R` for analysis and plot.

#### GPS tags

Since some woodpeckers were GPS tagged for another part of our study, we checked to see if they caused a difference in parental care rates.

1. Run `scripts/gps-tags/gps_tag_parentalcare.R` to analyze effects of GPS tags on behavior.
2. Run `scripts/gps-tags/gps_tag_provisioning_individuals.R` to analyze effects of GPS tags on individual provisioning.

#### Nest success

- Summaries of fledge success can be found in `scripts/nest-success/nest_survival_summaries.R`

## Acknowledgments

We used program [BORIS](https://www.boris.unito.it/) to process videos and generate raw csv files.
