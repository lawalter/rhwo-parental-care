# Data and Script Repository: Red-headed Woodpecker Parental Care 

## Introduction

This repository contains data, code, analyses, and plots by L. Abigail Walter for research on Red-headed Woodpecker (RHWO) parental care behavior with the [Bulluck Avian Ecology Lab](https://rampages.us/bullucklab) at Virginia Commonwealth University. 

#### Field methods

Please refer to our manuscript for a detailed summary of data collection (coming soon).

#### Metadata

Coming soon.

#### Requirements

- <b>R Version:</b> v3.5.2+
- <b>Packages:</b> tidyverse, lubridate, reshape2, glmmTMB, AICtab, psych, cowplot

## Data wrangling

Please note that it is not strictly necessary to run the following R scripts. Clean data files are already included in `/clean_data`. Their creation is nevertheless outlined below.

#### Incubation

- Run `scripts/incubation/wrangling_incubation.R` to wrangle and tidy the raw data into `clean_data/incubation.rds`.

#### Brooding, provisioning, and nest cleaning

- Run `scripts/wrangling_parentalcare.R` to wrangle and tidy the raw data into `clean_data/behaviors.csv`.

## Data analyses

The following scripts are used to analyze incubation, brooding, provisioning, and nest cleaning behaviors using the clean data files (generated above). Also described are files used to check effects of GPS tags on RHWO behavior, summarize nest success, and calculate nest check frequency.

#### Incubation

- To analyze the incubation data, run `scripts/incubation/incubation_analysis.R`
- To plot, run `scripts/cleaning/incubation_and_cleaning_plot.R`

#### Brooding

1. Run `scripts/brooding/brooding_models.R` to follow the modeling process and see how the "top GLMM" was chosen.
2. Plot predictions using `scripts/brooding/brooding_plots.R`
3. View boxplots of brooding by sex with and without behavior classified as "poopsearch" (in cavity for any amount of time, emerge with fecal sac) with `scripts/brooding/poopsearch_justification.R`. Excluding those visits did not change male brooding mean duration.

#### Provisioning

1. Run `scripts/provisioning/provisioning_models.R` to follow the modeling process and see how the "top GLMM" was chosen.
2. Plot predictions using `scripts/provisioning/provisioning_plots.R`
3. Plot individual variation in provisioning using `scripts/provisioning/provisioning_individuals.R`
4. Run additional analyses on reasons for variation in female provisioning using `scripts/provisioning/provisioning_individuals_extra.R`

#### Nest cleaning

- Run `scripts/cleaning/cleaning_analysis.R` for analysis.
- To plot, run `scripts/cleaning/incubation_and_cleaning_plot.R`

#### GPS tags

Since some woodpeckers were GPS tagged for another part of our study, we checked to see if they caused a difference in parental care rates.

1. Run `scripts/gps-tags/gps_tag_parentalcare.R` to analyze effects of GPS tags on behavior.
2. Run `scripts/gps-tags/gps_tag_provisioning_individuals.R` to analyze effects of GPS tags on individual provisioning.

#### Nest success

- Summaries of fledge success can be found in `scripts/summary_stats.R`
- Nest check analyses are in `scripts/nest_monitoring/nest_check_frequency.R`

## Other files

Deprecated R scripts are moved to the `/archive` directory.

## Acknowledgments

We used program [BORIS](https://www.boris.unito.it/) to process videos and generate raw csv files.
