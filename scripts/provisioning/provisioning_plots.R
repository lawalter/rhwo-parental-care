# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(glmmTMB)

# data --------------------------------------------------------------------

bbyvid <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  select(video_number, feeding = feeding_chicks, subject, brood_id, 
         exact_age_chick, habitat, julian_date, std_jdate, usable_video, 
         peeped_chick_count)

# script ------------------------------------------------------------------

# Top GLMM
model4.1x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*std_jdate + peeped_chick_count + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

# Generate dataframes with needed model variables to generate prediction
# https://github.com/glmmTMB/glmmTMB/issues/378

# Remove duplicates because there is no significant difference bw sexes
preddata <- bbyvid[!duplicated(bbyvid$video_number), ]

# Predict feeding rates
initial <- predict(model4.1x2, preddata, type = "response")

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

# Colors
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_hab <- c("CC" = "#76d6b0", "Savanna" = "#FFB84D")
colors_dat <- c("Early summer" = "#6bd2db", "Late summer" = "#fc913a")

# plots -------------------------------------------------------------------

# Brood size prov per hr
ggplot(preddata, 
       aes(exact_age_chick, predper_hr), 
       color=as.factor(peeped_chick_count), 
       fill=as.factor(peeped_chick_count)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color=as.factor(peeped_chick_count), 
                  fill=as.factor(peeped_chick_count)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", 
       y = "Predicted provisioning \n (per hour)") + 
  guides(color=guide_legend("Brood Size"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size = 12, family="mono"), 
        legend.position = "bottom") 

## Overall prov/hr
ggplot(preddata, aes(exact_age_chick, predper_hr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", 
       y = "Predicted provisioning (per hour)") +
  theme_classic() +
  theme(text=element_text(size = 12, family="mono"), 
        legend.position = "none") 

# Overall prov/chk/hr
ggplot(preddata, aes(exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", 
       y = "Predicted provisioning \n (per chick/hour)") +
  theme_classic() +
  theme(text=element_text(size = 12, family="mono"), 
        legend.position = "none") 

# Predicted provisioning rate by date (color)
ggplot(preddata, aes(exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(title = "Figure 4",
       x = "Chick age (day)", 
       y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") 
  # ggsave(
  #   file = "provisioning_fig_color.png",
  #   path ="plots/color/",
  #   width = 3.5,
  #   height = 3,
  #   units = "in",
  #   dpi = 600)

# Predicted provisioning rate by date (black & white)
ggplot(
  preddata, 
  aes(exact_age_chick, predper_chkhr)) +
  stat_smooth(
    method = glm, 
    formula = y ~ x + I(x^2), 
    aes(y = predper_chkhr, color = Date, fill = Date, linetype = Date), 
    size = 0.5, 
    se = TRUE, 
    level = 0.95,
    fullrange = TRUE) +
  labs(title = "Figure 4",
       x = "Chick age (day)", 
       y = "Predicted provisioning \n (per chick per hr)") + 
  guides(color = guide_legend("Date")) +
  scale_y_continuous(limits = c(0, 4.5)) +
  scale_linetype_discrete(labels = c("early summer", "late summer")) +
  scale_shape_manual(values = c(17, 1), labels = c("early summer", "late summer")) +
  scale_color_manual(values = c("Early summer" = "black", "Late summer" = "black"), labels = c("early summer", "late summer")) +
  scale_fill_grey(start = 0.6, end = 0.6, labels = c("early summer", "late summer")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  ggsave(
    file = "provisioning_fig.png",
    path ="plots/manuscript_plots/",
    width = 3.5,
    height = 3,
    units = "in",
    dpi = 1200)


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


