# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(glmmTMB)

# data --------------------------------------------------------------------

# Distance and time data 

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
  select(video_id, feeding_chicks, subject, sex, habitat, exact_age_chick,
         peeped_chick_count, nest_id, brood_id, std_jdate, usable_video, 
         video_number, julian_date) %>%
  # Feeding variable
  rename(feeding = feeding_chicks) %>%
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
  # Add mean value to records w/ missing start time
  mutate(
    start_time = 
      ifelse(
        is.na(start_time), 
        ave(start_time, FUN = function(x) mean(x, na.rm = TRUE)), 
        start_time)) 

behaviors$scTime <- scale(behaviors$start_time)

# script ------------------------------------------------------------------

# Top GLMM
model_date_interaction <-   
  glmmTMB(
    feeding ~ 
      std_jdate*I(exact_age_chick^2) + 
      std_jdate*exact_age_chick + 
      peeped_chick_count + scTime + (1|brood_id) + (1|subject) + 
      offset(log(usable_video)), 
    data = behaviors, 
    ziformula = ~1, 
    family = nbinom2(link = "log"))

# Generate dataframes with needed model variables to generate prediction
# https://github.com/glmmTMB/glmmTMB/issues/378

# Remove duplicates because there is no significant difference bw sexes
preddata <- behaviors[!duplicated(behaviors$video_number), ]

# Predict feeding rates
initial <- predict(model_date_interaction, preddata, type = "response")

# Calculate rates 
preddata <-
  preddata %>%
  mutate(
    pred.f.per.vid = initial,
    per_min = feeding/usable_video,
    per_hr = per_min*60,
    predper_min = pred.f.per.vid/usable_video,
    predper_hr = predper_min*60,
    predper_chkhr = predper_hr/peeped_chick_count,
    Date = ifelse(julian_date >= 190, "Late summer", "Early summer"),
    group = 'halved'
  )

# plots -------------------------------------------------------------------

# Predicted provisioning rate by date (black & white)

age_plot <- 
  ggplot(
    preddata, 
    aes(
      x = exact_age_chick, 
      y = predper_chkhr)) +
  stat_smooth(
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(y = predper_chkhr
        #color = Date, fill = Date, linetype = Date
        ), 
    size = 0.5, 
    color = "black",
    se = TRUE, 
    level = 0.95,
    fullrange = TRUE) +
  labs(
    title = "Figure 4",
    x = "Chick age (day)", 
    y = "Predicted provisioning \n (per chick per hr)") + 
  #guides(color = guide_legend("Date")) +
  scale_y_continuous(limits = c(0, 4.5)) +
  # scale_linetype_discrete(labels = c("early summer", "late summer")) +
  # scale_shape_manual(values = c(17, 1), labels = c("early summer", "late summer")) +
  # scale_color_manual(values = c("Early summer" = "black", "Late summer" = "black"), labels = c("early summer", "late summer")) +
  # scale_fill_grey(start = 0.6, end = 0.6, labels = c("early summer", "late summer")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") 

# Predicted provisioning rate by brood size

broodsize_plot <- 
  ggplot(
    preddata, 
    aes(
      x = as.factor(peeped_chick_count), 
      y = predper_hr)) +
  geom_violin(lwd = 0.5) +
  labs(x = "Brood size", 
       y = "Predicted provisioning (per hr)") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")



cowplot::plot_grid(age_plot, broodsize_plot, 
                   labels = 'AUTO',
                   nrow = 2, ncol = 1) +
  ggsave(
    file = "fig4_provisioning.png",
    path ="plots/manuscript_plots/",
    width = 3.5,
    height = 7,
    units = "in",
    dpi = 300)

# comparison plot ---------------------------------------------------------

# Provisioning prediction data

preddata2 <- 
  bbyvid %>%
  as_tibble() %>%
  select(video_number, feeding, subject, brood_id, exact_age_chick,
         habitat, julian_date, Std_jdate, usable_video, 
         peeped_chick_count)

initial2 <- predict(model4.1x2, preddata2, type = "response")

preddata2 <-
  preddata2 %>%
  mutate(
    predicted_feeding_per_video = initial2,
    actual_feeding_per_min = feeding/usable_video,
    actual_feeding_per_hr = actual_feeding_per_min*60,
    predicted_feeding_per_min = predicted_feeding_per_video/usable_video,
    predicted_feeding_per_hr = predicted_feeding_per_min*60,
    predicted_feeding_per_chick_hr = 
      predicted_feeding_per_hr/peeped_chick_count,
    Date = ifelse(julian_date >= 190, "Late summer", "Early summer"),
    group = 'all'
  )

bbyvid <- 
  bbyvid %>%
  mutate(group = 'original',
         feeding_rate = ((feeding/peeped_chick_count)*60)/usable_video,
         Date = ifelse(julian_date >= 190, "Late summer", "Early summer"))

# A comparison of:
# 1) original provisioning data = 'original', 
# 2) predicted provisioning rates using 1 observation per nest (since sex
# was not significant = 'halved',
# and 3) predicted provisioning rates using both male and female data at
# each nest = 'all'
# We use the 'halved' data in the final prediction plot

ggplot() +
  stat_smooth(
    data = preddata,
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(x = exact_age_chick, 
        y = predper_chkhr, 
        color = group, 
        #color = Date,
        #fill = group, 
        linetype = Date), 
    size = 0.5, se = FALSE, level = 0.95, fullrange = TRUE) +
  stat_smooth(
    data = preddata2,
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(x = exact_age_chick, 
        y = predicted_feeding_per_chick_hr, 
        color = group,
        fill = NULL,
        #color = Date,
        #fill = group, 
        linetype = Date), 
    size = 0.5, se = FALSE, level = 0.95, fullrange = TRUE) +
  stat_smooth(
    data = bbyvid,
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(x = exact_age_chick, 
        y = feeding_rate, 
        color = group, 
        #color = Date,
        #fill = group, 
        linetype = Date), 
    size = 0.5, se = FALSE, level = 0.95, fullrange = TRUE) +
  labs(title = "Figure 3",
       x = "Chick age (day)", 
       y = "Predicted provisioning \n (per chick/hr)") + 
  guides(color = guide_legend("Date")) +
  scale_shape_manual(values = c(17, 1)) +
  # scale_color_manual(values = c("Early summer" = "black", 
  #                               "Late summer" = "black")) +
  scale_color_manual(values = c('halved' = 'red',
                               'all' = 'blue',
                               'original' = 'green')) +
  # scale_fill_manual(values = c('halved' = 'red',
  #                              'all' = 'yellow',
  #                              'original' = 'green')) +
  theme_classic() +
  theme(legend.position = "bottom")


