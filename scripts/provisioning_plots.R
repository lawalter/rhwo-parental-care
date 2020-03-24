# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(glmmTMB)

# data --------------------------------------------------------------------

bbyvid <- read_csv("clean_data/bbyvid.csv")

# script ------------------------------------------------------------------

# Top GLMM
model4.1x2 <- glmmTMB(feeding ~ I(exact_age_chick^2)*Std_jdate + peeped_chick_count + exact_age_chick + (1|brood_id) + (1|subject)  + offset(log(usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))

# Generate dataframes with needed model variables to generate prediction
# https://github.com/glmmTMB/glmmTMB/issues/378

preddata <- 
  bbyvid %>%
  select(video_number, feeding, subject, brood_id, exact_age_chick,
         habitat, julian_date, Std_jdate, usable_video, 
         peeped_chick_count)

preddata <- preddata[!duplicated(preddata$video_number), ]

# Removing duplicates because there is no difference bw sexes

## Feeding per min
initial <- predict(model4.1x2, preddata, type="response")
preddata$pred.f.per.vid <- initial
preddata$per_min <- preddata$feeding/preddata$usable_video
preddata$per_hr <- preddata$per_min*60
preddata$predper_min <- preddata$pred.f.per.vid/preddata$usable_video
preddata$predper_hr <- preddata$predper_min*60
preddata$predper_chkhr <- preddata$predper_hr/preddata$peeped_chick_count
preddata$Date <- ifelse(preddata$julian_date >= 190, "Late summer", "Early summer")

bbyvid$provrate <- (bbyvid$feeding/bbyvid$usable_video)*60
bbyvid$cleanrate <- (bbyvid$cleaning/bbyvid$usable_video)*60
bbyvid$feedingperchkhr <- ((bbyvid$feeding/bbyvid$peeped_chick_count)/bbyvid$usable_video)*60
bbyvid$Date <- ifelse(bbyvid$julian_date >= 190, "Late summer", "Early summer")


# plots -------------------------------------------------------------------

# Colors
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_hab <- c("CC" = "#76d6b0", "Savanna" = "#FFB84D")
colors_dat <- c("Early summer" = "#6bd2db", "Late summer" = "#fc913a")

# plots -------------------------------------------------------------------

# Brood size prov per hr
ggplot(preddata, aes(Exact_age_chick, predper_hr), color=as.factor(Peeped_chick_count), 
       fill=as.factor(Peeped_chick_count)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color=as.factor(Peeped_chick_count), 
                  fill=as.factor(Peeped_chick_count)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per hour)") + 
  guides(color=guide_legend("Brood Size"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom") 

## Overall prov/hr
ggplot(preddata, aes(Exact_age_chick, predper_hr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning (per hour)") +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "none") 

# Overall prov/chk/hr
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "none") 

# Predicted provisioning rate for date
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date"), fill = FALSE) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom")

# Regular font 
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(legend.position = "bottom")

# Big font for powerpoint
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=10),
        text = element_text(size=14),
        legend.position = "bottom")

# Plot for manuscript
provisioning_fig <- 
  ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
    stat_smooth(method = glm, formula = y ~ x + I(x^2), 
                aes(y = predper_chkhr, color = Date, 
                    fill = Date, linetype = Date), alpha = 0.2,
                se = TRUE, level = 0.95, fullrange = TRUE) +
    labs(title = "Figure 3",
         x = "Chick age (day)", 
         y = "Predicted provisioning \n (per chick/hr)") + 
    guides(color = guide_legend("Date")) +
    scale_color_grey(start = 0.6, end = 0.3) +
    scale_fill_grey(start = 0.6, end = 0.3) +
    theme_classic() +
    theme(axis.title.x = element_text(size=11), 
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9),
          legend.text = element_text(size=10),
          legend.title = element_text(size=11),
          legend.position = "bottom") 
ggplot2::ggsave(
  file = "provisioning_fig.pdf",
  plot = provisioning_fig,
  path ="plots/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 300)

# B&W plot for manuscript
provisioning_fig <- 
  ggplot(preddata, aes(exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color = Date, 
                  fill = Date, linetype = Date), size = 0.5, 
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(title = "Figure 3",
       x = "Chick age (day)", 
       y = "Predicted provisioning \n (per chick/hr)") + 
  guides(color = guide_legend("Date")) +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = c("Early summer" = "black", 
                                "Late summer" = "black")) +
  scale_fill_grey(start = 0.6, end = 0.6) +
  theme_classic() +
  theme(axis.title.x = element_text(size=11), 
        axis.title.y = element_text(size=11),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = "bottom") 
ggplot2::ggsave(
  file = "provisioning_fig.pdf",
  plot = provisioning_fig,
  path ="plots/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 600)
