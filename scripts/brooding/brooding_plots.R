# intro -------------------------------------------------------------------

# Red-headed Woodpecker brooding duration model predictions
# By: Lynn Abigail Walter

# Latest updates:
# Aug 2019 - After correcting the bbyvid to sum duration of brooding, a new top model was selected. My thesis used model 1.1i which did not include Tmax as a factor, but the new top model 1.5i does.

# Best version

# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(glmmTMB)

# data --------------------------------------------------------------------

# Distance and start time data 

dist_start <-
  read_csv("raw_data/provisioning_video_data.csv") %>%
  filter(Letter == "a") %>%
  select(
    video_number = Video_number,
    start_time = Start_time)

# Behaviors by video

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(video_id, brooding_min, subject, sex, habitat, exact_age_chick,
         peeped_chick_count, nest_id, brood_id, std_jdate, tmax, 
         usable_video, video_number) %>%
  # Calculate standardized temperatures
  mutate(
    std_tmax = (tmax - mean(tmax))/sd(tmax),
    brooding_rate = (brooding_min/usable_video)*60) %>% 
  # Add video start time to dataset from previous datafame
  left_join(dist_start, by = "video_number") %>%
  mutate(
    start_time =
      case_when(
        start_time == "na" ~ NA_character_,
        video_number == "70" ~ "1100",
        video_number == "117" ~ "904",
        TRUE ~ start_time)) %>% 
  mutate(start_time = as.numeric(start_time)) %>% 
  # To eliminate the warnings from glmmTMB
  # "non-integer counts in a truncated_nbinom1 model"
  # Run the following mutate, but it is not needed -- it's fine as dbl
  mutate(brooding_min = as.integer(round(brooding_min)))

# Replace few missing values for video start time with the mean:
behaviors <- 
  transform(
    behaviors, 
    start_time = 
      ifelse(
        is.na(start_time), 
        ave(start_time, FUN = function(x) mean(x, na.rm = TRUE)), 
        start_time))

behaviors$scTime <- scale(behaviors$start_time)

# script ------------------------------------------------------------------

# Run best model (see new_brooding_models.R)

brood_mod <- 
  glmmTMB(
    brooding_min ~ 
      sex*exact_age_chick + scTime+
      peeped_chick_count + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~., 
    family = "truncated_nbinom1")

summary(brood_mod)

# Generate dataframes with needed model variables to generate prediction

brooding_subset <- 
  behaviors %>%
  select(brooding_min, subject, brood_id, exact_age_chick, sex, scTime,
         usable_video, peeped_chick_count) 

# Calculate a predicted brooding rate (brooding per minute)

brooding_predictions <-
  brooding_subset %>%
  mutate(
    predicted_brood_per_unit = 
      stats::predict(brood_mod, brooding_subset, type = "response"),
    predicted_brood_per_hr =
      (predicted_brood_per_unit/usable_video)*60,
    brood_per_hr = (brooding_min/usable_video)*60)

# colors ------------------------------------------------------------------

# Set colors

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_bw <- c("Cool" = "#ABABAB", "Hot" = "#333333")

# linear plots ------------------------------------------------------------

# Final brooding plot - by sex (color)

ggplot(brooding_predictions, 
       aes(x = exact_age_chick, 
           y = predicted_brood_per_hr, 
           color = sex, 
           fill = sex)) +
  stat_smooth(method = glm, 
              formula = y ~ x, 
              aes(y = predicted_brood_per_hr, 
                  color = as.factor(sex), 
                  fill = as.factor(sex)), 
              alpha = 0.2,
              se = TRUE, 
              level = 0.95, 
              fullrange = TRUE) +
  geom_point(aes(color = sex, shape = sex)) +
  coord_cartesian(ylim = c(0,40)) + 
  labs(x = "Chick age (day)", y = "Predicted brooding (min/hr)") + 
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colors_sex) + 
  guides(color = guide_legend("Sex")) +
  theme_classic() +
  theme(legend.position = "bottom", 
        text = element_text(size = 12)) 

# Final brooding plot - by sex (black & white)

brooding_sex <-
  ggplot(brooding_predictions, 
       aes(x = exact_age_chick, 
           y = predicted_brood_per_hr)) +
  stat_smooth(
    method = glm, 
    formula = y ~ x, 
    aes(y = predicted_brood_per_hr, 
        linetype = sex,
        color = sex), 
    size = 0.5, 
    se = TRUE, 
    level = 0.95, 
    fullrange = TRUE) +
  geom_point(aes(color = sex, shape = sex)) +
  coord_cartesian(ylim = c(0, 40)) + 
  labs(
    #title = "Figure 3", 
       x = "Chick age (day)", 
       y = "Predicted brooding (min per hr)",
       title = "Figure 3") + 
  guides(color = guide_legend("Sex"), 
         shape = guide_legend("Sex"),
         linetype = guide_legend("Sex")) +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = c("female" = "black", "male" = "black")) +
  scale_fill_grey(start = 0.6, end = 0.6) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  ggplot2::ggsave(
  file = "fig3_brooding.png",
  path ="plots/manuscript_plots/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 300)

# actual brooding data plot -----------------------------------------------

# Plotting real values, not predictions
ggplot(behaviors, 
       aes(exact_age_chick, 
           brooding_min, 
           color = sex, 
           fill = sex)) +
  geom_point(aes(color = sex, shape = sex)) +
  stat_smooth(method = glm, 
              formula = y ~ x, 
              aes(y = brooding_min, 
                  color = as.factor(sex), 
                  fill = as.factor(sex)), 
              alpha = 0.2,
              se = TRUE, 
              level = 0.95) +
  coord_cartesian(ylim = c(0,40)) + 
  labs(x = "Chick age (day)", y = "Actual brooding (min)") + 
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colors_sex) + 
  guides(color=guide_legend("Sex"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), 
        legend.position = "bottom") 

# graphs to compare hurdle and non-hurdle models --------------------------

nonhurdle <- 
  glmmTMB(brooding_min ~ exact_age_chick + as.factor(peeped_chick_count) +
            (1|brood_id) + (1|subject) + offset(log(usable_video)), 
          data = behaviors, ziformula = ~1, family = gaussian())

predbrood_nh <- 
  behaviors %>%
  select(brooding_min, subject, brood_id, exact_age_chick, sex,
         std_tmax, usable_video, peeped_chick_count)

predbrood_nh <-
  predbrood_nh %>%
  mutate(
    predicted_brood_per_unit = 
      stats::predict(nonhurdle, predbrood_nh, type="response"),
    predicted_brood_per_hr = (predicted_brood_per_unit/usable_video)*60,
    brood_per_hr = (brooding_min/usable_video)*60)

ggplot(data = brooding_predictions, 
       aes(x = brooding_min, 
           y = predicted_brood_per_hr)) +
  geom_point() +
  labs(main = "Hurdle model predictions", 
       x = "Brooding (min/hr)", 
       y = "Predicted brooding (min/hr)") + 
  theme_classic()

plot(brooding_predictions$brooding_min, brooding_predictions$predicted_brood_per_hr)
title(main = "Hurdle model predictions")
ylab(main = "Brooding (min/hr)")

plot(predbrood_nh$brooding_min, predbrood_nh$predicted_brood_per_hr) 
title(main = "Zero-inflated gaussian model predictions")
