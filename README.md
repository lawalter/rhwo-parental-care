# Red-headed Woodpecker Parental Care 

## Table of Contents

- [Introduction](#introduction)
- [About the Data](#data-collection)
- [Using the Repository](#using-the-repository)

## Introduction

This repository contains data and data analyses that belong to a study on Red-headed Woodpecker (RHWO) parental care behavior conducted by L. Abigail Walter with the [Bulluck Avian Ecology Lab](https://rampages.us/bullucklab) at Virginia Commonwealth University. The Master's thesis that resuled from these data can be found [here](). A manuscript is in progress. 

## About the Data

### Data collection: field methods

We recorded parental care behaviors at RHWO nests at our study site, Fort A.P. Hill, Virginia, USA. We placed video cameras on tripods within 10 m of nest snags to record parental behavior at the nest. Cameras were focused on the nest cavity entrance and recorded woodpecker activity during sampling periods between 0715–1515 from May-August in 2017 and 2018. Because we were interested in sex-specific parental care behavior, we only included videos in our analyses if 1) at least one adult was color-banded and genetically sexed, 2) banded individuals could be clearly identified by their color band combination, and 3) the nest contained a known number of chicks with known age. 

We scored adult woodpecker behaviors at the nest using <a href="http://www.boris.unito.it/">BORIS</a>. We associated behaviors with a woodpecker’s unique color band combination or a unique identifier for each unbanded mate of a banded adult, with the assumption that no additional birds were providing parental care. Cooperative breeding was not observed in our system and has only been reported in two nests in a high-density nesting site reliant on telephone poles for cavities in Illinois (Atterberry-Jones and Peer 2010). For analysis, we included the following four parental care behaviors in the ethogram: incubating eggs (in-cavity for > 1 min), feeding chicks (delivering a food item to the nest), cleaning nest (removal of fecal material from the nest), and brooding (in-cavity for > 1 min and not followed by a cleaning event). Incubation and brooding had duration times, while feeding chicks and cleaning nest were point events. 

### Data analysis

We recorded incubation length for individual parents in videos of nests with eggs. For nests with chicks, we recorded number of cleaning events, number of provisioning visits, and brooding length for each parent in each video. To analyze provisioning counts and brooding durations, we used R package glmmTMB to run generalized linear mixed models (GLMMs, Brooks et al. 2017). To test for differences in incubation durations and nest cleaning between sexes, we used a paired two-sample Student’s t-test and a Wilcoxon signed rank test, respectively. We also tested for differences in incubation using maximum daily temperature with a linear mixed model for each sex. 

To model provisioning rate, we included chick age (days) and brood size in all models because these factors are known to influence feeding rates (Wright and Cuthill 1990, Rossmanith et al. 2009). Since provisioning visits were count data and contained numerous zeros, we compared our simple model with chick age and brood size using a zero-inflated negative binomial distribution to the same model with a Poisson error distribution using AIC; we determined the zero-inflated negative binomial to fit the data best. Date, habitat type (closed canopy and savanna), and sex were included as fixed effects. We standardized ordinal date to a z distribution to facilitate model convergence. We ensured chick age and date were not correlated and included the following interactions: chick age × parent sex; chick age × date; and chick age × habitat. We expected chick age to be the most important factor driving provisioning rate, and were interested to see if sexes responded to this increased demand differently, or if date or habitat affected a parents’ ability to provide resources to nestlings as they developed. We also included a quadratic effect of chick age because parental care may level off or decline after some peak age where nestling demand is greatest. Lastly, woodpecker ID and brood ID were included in provisioning models as random effects because some woodpeckers had more than one nesting attempt and we recorded parental care at the same nest on different days during the nestling period.

To model brooding duration, we used the same simple base model as provisioning with chick age and brood size being included in all models. We compared how this base model performed using a non-zero-inflated Gaussian error distribution, a zero-inflated distribution, and a truncated binomial hurdle model. A hurdle model accounts for a high frequency of true zero observations in the data by first fitting a binomial response (i.e., the nest is brooded or not) and then models the remaining non-zero values separately (Lambert 1992, Martin et al. 2005). The hurdle model fit the data best based on AIC model comparisons. We included the same fixed effects as in the provisioning models except that we replaced ordinal date with maximum daily temperature (Tmax) because we expected temperature to better estimate brooding. Maximum daily temperatures were obtained from the National Oceanic and Atmospheric Administration (NOAA) Global Historical Climatology Network - Daily dataset (Menne et al. 2012a, Menne et al. 2012b) from the station in Corbin, VA (38.2022°N, -77.3747°W) located 8.4 km from our focal TAs. We also included the following interactions: chick age × parent sex; chick age × brood size; chick age × Tmax. We expected brooding duration to decrease with chick age, brood size, and Tmax, and were interested to see if sexes differed in how they adjusted their brooding time as chicks matured. We included a quadratic effect of chick age because we expected brooding to decrease in a nonlinear fashion as chicks mature. 

For both provisioning and brooding analyses, video length was used as an offset in the models to account for variable recording time among samples (Foster and Bravington 2013). Models were compared using AIC, top models were chosen based on the lowest ΔAIC value, and models with ΔAIC < 2.0 were considered equally supported. However, these equally supported models were typically nested iterations of the top model so we identified which parameters were most important in these nested models by assessing β estimates and p-values. Predictions for brooding time and provisioning rate were generated from the top-performing models and then plotted using R package stats::predict. Predicted provisioning counts were adjusted using each sample’s video length and brood size to calculate provisioning visits per chick per hour. Brooding durations were adjusted using only video length to determine brooding minutes per hour. 

## Using the repository

<b>Program:</b> R version 3.5.2+

<b>Required packages:</b> tidyverse, glmmTMB, AICtab

R scripts included are for wrangling, tidying, analyzing, and plotting these data.   

### Incubation

1. Run 
2. To analyze the final table, run 

### Brooding, provisioning, and nest cleaning

1. Run
2. To model brooding durations, run
3. To model provisioning rates, run
4. To assess individual variation in nestling provisioning rates, run
5. To test nest cleaning rates, run


