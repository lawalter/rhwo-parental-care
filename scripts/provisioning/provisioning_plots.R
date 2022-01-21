# libraries ---------------------------------------------------------------

library(tidyverse)
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
        TRUE ~ start_time),
    Season = 
      case_when(
        julian_date > 188 ~ "Late summer",
        julian_date <= 188 ~ "Early summer",
        TRUE ~ NA_character_
      )) %>% 
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
    group = 'halved'
  )

# plots -------------------------------------------------------------------

# Predicted provisioning rate by chick age (black & white)

# age_plot <- 
#   ggplot(
#     preddata, 
#     aes(
#       x = exact_age_chick, 
#       y = predper_chkhr)) +
#   stat_smooth(
#     method = glm, 
#     formula = y ~ x + I(x^2), 
#     aes(y = predper_chkhr
#         #color = Date, fill = Date, linetype = Date
#         ), 
#     size = 0.5, 
#     color = "black",
#     se = TRUE, 
#     level = 0.95,
#     fullrange = TRUE) +
#   labs(
#     title = "Figure 4",
#     x = "Chick age (day)", 
#     y = "Predicted provisioning \n (per chick per hr)") + 
#   #guides(color = guide_legend("Date")) +
#   scale_y_continuous(limits = c(0, 4.5)) +
#   # scale_linetype_discrete(labels = c("early summer", "late summer")) +
#   # scale_shape_manual(values = c(17, 1), labels = c("early summer", "late summer")) +
#   # scale_color_manual(values = c("Early summer" = "black", "Late summer" = "black"), labels = c("early summer", "late summer")) +
#   # scale_fill_grey(start = 0.6, end = 0.6, labels = c("early summer", "late summer")) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 10), 
#         axis.title.y = element_text(size = 10),
#         axis.text.y = element_text(size = 9),
#         axis.text.x = element_text(size = 9),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 10),
#         legend.position = "bottom") 

# Predicted provisioning rate by date (color)

date_plot <- 
  ggplot(
    preddata, 
    aes(
      x = exact_age_chick, 
      y = predper_chkhr)) +
  stat_smooth(
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(y = predper_chkhr, color = Season, fill = Season, linetype = Season), 
    size = 0.5, 
    se = TRUE, 
    level = 0.95,
    fullrange = TRUE) +
  labs(
    x = "Chick age (day)", 
    y = "Predicted provisioning \n (per chick per hr)") + 
  guides(color = guide_legend("Season")) +
  scale_y_continuous(limits = c(0, 4.5)) +
  scale_linetype_discrete(labels = c("Early", "Late")) +
  scale_shape_manual(values = c(17, 1), labels = c("Early", "Late")) +
  scale_color_manual(values = c("Early summer" = "#28C75F", 
                                "Late summer" = "#C7AE28"), 
                     labels = c("Early", "Late")) +
  scale_fill_manual(values = c("Early summer" = "#28C75F", 
                               "Late summer" = "#C7AE28"), 
                    labels = c("Early", "Late")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "bottom") 

# Predicted provisioning rate by brood size

broodsize_plot <- 
  preddata %>% 
  mutate(peeped_chick_count = as.factor(peeped_chick_count)) %>% 
  ggplot(
    aes(
      x = peeped_chick_count, 
      y = predper_hr,
      fill = peeped_chick_count,
      color = peeped_chick_count
      )) +
  geom_violin(lwd = 0.5, alpha = 0.7) +
  labs(x = "Brood size (number of chicks)", 
       y = "Predicted provisioning (per hr)") +
  theme_classic() +
  scale_fill_manual(values = c("3" = "#C72841", "2" = "#C7AE28", "1" = "#2841C7")) +
  scale_color_manual(values = c("3" = "#FFFFFF", "2" = "#FFFFFF", "1" = "#FFFFFF")) +
  # scale_fill_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  # scale_color_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "none")

cowplot::plot_grid(date_plot, broodsize_plot, 
                   labels = 'AUTO',
                   nrow = 2, ncol = 1) +
  ggsave(
    file = "fig4_provisioning.png",
    path ="plots/manuscript_plots/",
    width = 3.5,
    height = 7,
    units = "in",
    dpi = 300)


# real provisioning plots -------------------------------------------------

# Box
behaviors %>% 
  mutate(peeped_chick_count = as.factor(peeped_chick_count)) %>% 
  ggplot(
    aes(
      x = peeped_chick_count, 
      y = (feeding/usable_video)*60,
      fill = peeped_chick_count,
      color = peeped_chick_count
    )) +
  geom_boxplot(lwd = 0.5, alpha = 0.7) +
  labs(x = "Brood size (number of chicks)", 
       y = "Predicted provisioning (per hr)") +
  #theme_classic() +
  scale_fill_manual(values = c("3" = "#C72841", "2" = "#C7AE28", "1" = "#2841C7")) +
  scale_color_manual(values = c("3" = "#FFFFFF", "2" = "#FFFFFF", "1" = "#FFFFFF")) +
  # scale_fill_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  # scale_color_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "none")

# Predicted provisioning rate by brood size

behaviors %>% 
  mutate(peeped_chick_count = as.factor(peeped_chick_count)) %>% 
  ggplot(
    aes(
      x = peeped_chick_count, 
      y = (feeding/usable_video)*60,
      fill = peeped_chick_count,
      color = peeped_chick_count
    )) +
  geom_violin(lwd = 0.5, alpha = 0.7) +
  labs(x = "Brood size (number of chicks)", 
       y = "Predicted provisioning (per hr)") +
  theme_classic() +
  scale_fill_manual(values = c("3" = "#C72841", "2" = "#C7AE28", "1" = "#2841C7")) +
  scale_color_manual(values = c("3" = "#FFFFFF", "2" = "#FFFFFF", "1" = "#FFFFFF")) +
  # scale_fill_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  # scale_color_manual(values = c("3" = "#8694E0", "2" = "#AFB8EA", "1" = "#D7DBF5")) +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "none")

# summaries of provisioning by brood size ---------------------------------

# ONE PARENT
behaviors %>% 
  select(video_id, feeding, usable_video, peeped_chick_count) %>% 
  distinct() %>% 
  mutate(feeding_rate = (feeding/usable_video)*60) %>% 
  group_by(peeped_chick_count) %>% 
  summarize(
    mean_feeding_perhr = mean(feeding_rate),
    mean_feeding_perchkhr = mean(feeding_rate)/peeped_chick_count,
    sample_size = n()) %>% 
  ungroup() %>% 
  distinct()

one_parent <- 
  behaviors %>% 
  select(video_id, feeding, usable_video, peeped_chick_count) %>% 
  distinct() %>% 
  mutate(feeding_rate = (feeding/usable_video)*60,
         feeding_rate_perchk = ((feeding/usable_video)*60)/peeped_chick_count) %>%  
  distinct()

describe(one_parent$feeding_rate)

describe(one_parent$feeding_rate_perchk)

# TOTAL VISITS TO NEST
behaviors %>% 
  select(video_number, feeding, usable_video, peeped_chick_count) %>% 
  group_by(video_number, usable_video, peeped_chick_count) %>% 
  mutate(feeding_sum = sum(feeding)) %>% 
  ungroup() %>% 
  select(-feeding) %>% 
  distinct() %>% 
  mutate(feeding_rate = (feeding_sum/usable_video)*60) %>% 
  group_by(peeped_chick_count) %>% 
  summarize(
    mean_feeding_perhr = mean(feeding_rate),
    mean_feeding_perchkhr = mean(feeding_rate)/peeped_chick_count,
    sample_size = n()) %>% 
  ungroup() %>% 
  distinct()

two_parent <- 
  behaviors %>% 
  select(video_number, feeding, usable_video, peeped_chick_count) %>% 
  group_by(video_number, usable_video, peeped_chick_count) %>% 
  mutate(feeding_sum = sum(feeding)) %>% 
  ungroup() %>% 
  select(-feeding) %>%
  distinct() %>% 
  mutate(feeding_rate = (feeding_sum/usable_video)*60,
         feeding_rate_perchk = ((feeding_sum/usable_video)*60)/peeped_chick_count) %>%  
  distinct()

describe(two_parent$feeding_rate)

describe(two_parent$feeding_rate_perchk)
