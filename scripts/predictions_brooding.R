### Red-headed Woodpecker brooding duration model predictions
### By: Lynn Abigail Walter

### Latest updates:
# Aug 2019 - After correcting the bbyvid to sum duration of brooding, a new top model was selected. My thesis used model 1.1i which did not include Tmax as a factor, but the new top model 1.5i does.

### BEST VERSION

# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(glmmTMB)

# data --------------------------------------------------------------------

bbyvid <- read_csv("clean_data/bbyvid.csv")

# script ------------------------------------------------------------------

# Calculate standardized temperatures
bbyvid$Std_TMAX <- (bbyvid$TMAX - mean(bbyvid$TMAX))/sd(bbyvid$TMAX)
bbyvid$brood_perhr <- (bbyvid$brooding_min/bbyvid$Usable_video)*60

Brood_pred_mod <- glmmTMB(brooding_min ~ Exact_age_chick*Sex + 
                            Peeped_chick_count + Std_TMAX + 
                            (1|Brood_ID) + offset(log(Usable_video)), 
                          data = bbyvid, 
                          ziformula = ~., 
                          family = "truncated_nbinom1")

summary(Brood_pred_mod)

# Generate dataframes with needed model variables to generate prediction
predbrood <- bbyvid[c("brooding_min", "Subject", "Brood_ID", "Exact_age_chick", "Sex", "Habitat",
                      "Std_TMAX", "Usable_video", "Peeped_chick_count")]
## Brooding per min
predbrood$pred.b.per.unit <- stats::predict(Brood_pred_mod, predbrood, type="response")
predbrood$pred.b.per.hr <- (predbrood$pred.b.per.unit/predbrood$Usable_video)*60
predbrood$brood_per_hr <- (predbrood$brooding_min/predbrood$Usable_video)*60
predbrood$Tmax <- ifelse(predbrood$Std_TMAX > 0.12, "Hot", "Cool")

# Colors
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_tem <- c("Cool" = "#6bd2db", "Hot" = "#fc913a")
colors_bw <- c("Cool" = "#ABABAB", "Hot" = "#333333")

predbrood_conditional <- predbrood %>% filter(pred.b.per.unit > 1)

# *** Final linear prov plot By Sex (min per hr)
ggplot(predbrood, aes(Exact_age_chick, pred.b.per.hr, color=Sex, fill=Sex)) +
  stat_smooth(method = glm, formula = y ~ x, 
              aes(y = pred.b.per.hr, color=as.factor(Sex), 
                  fill=as.factor(Sex)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point(aes(color=Sex, shape=Sex)) +
  coord_cartesian(ylim=c(0,40)) + 
  labs(x = "Chick age (day)", y = "Predicted brooding (min/hr)") + 
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colors_sex) + 
  guides(color=guide_legend("Sex")) +
  theme_classic() +
  theme(legend.position = "bottom", text=element_text(size=12)) 

predbrood <-
  predbrood %>%
  mutate(stderr = sd(pred.b.per.hr)/sqrt(length(pred.b.per.hr)))

# B&W by sex for manuscript
brooding_sex_fig <-
  ggplot(predbrood, aes(Exact_age_chick, pred.b.per.hr, color = Sex, fill = Sex)) +
  stat_smooth(method = glm, formula = y ~ x, 
              aes(y = pred.b.per.hr, color = as.factor(Sex), 
                  fill = as.factor(Sex), linetype = Sex), size = 0.5, 
              se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point(aes(color=Sex, shape=Sex)) +
  coord_cartesian(ylim=c(0,40)) + 
  labs(title = "Figure 2a",
       x = "Chick age (day)", 
       y = "Predicted brooding (min/hr)") + 
  guides(color=guide_legend("Sex")) +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = c("female" = "black", "male" = "black")) +
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
  file = "brooding_sex_fig.png",
  plot = brooding_sex_fig,
  path ="plots/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 300)

# Grayscale by sex for manuscript
brooding_sex_grayscale <-
  ggplot(predbrood, aes(Exact_age_chick, pred.b.per.hr, color=Sex, fill=Sex)) +
    stat_smooth(method = glm, formula = y ~ x, 
                aes(y = pred.b.per.hr, color = as.factor(Sex), 
                    fill = as.factor(Sex), linetype = Sex), alpha = 0.2,
                se = TRUE, level = 0.95, fullrange = TRUE) +
    geom_point(aes(color=Sex, shape=Sex)) +
    coord_cartesian(ylim=c(0,40)) + 
    labs(title = "Figure 2a",
         x = "Chick age (day)", 
         y = "Predicted brooding (min/hr)") + 
    guides(color=guide_legend("Sex")) +
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

## Poster 700x500
ggplot(predbrood, aes(Exact_age_chick, pred.b.per.hr, color=Sex, fill=Sex)) +
  stat_smooth(method = glm, formula = y ~ x, 
              aes(y = pred.b.per.hr, color=as.factor(Sex), 
                  fill=as.factor(Sex)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point(aes(color=Sex, shape=Sex, size = 1.5)) +
  coord_cartesian(ylim=c(0,40)) + 
  labs(x = "Chick age (day)", y = "Predicted brooding\n(min/hr)") + 
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colors_sex) + 
  guides(color=guide_legend("Sex")) +
  theme_classic() +
  theme(legend.position = "bottom", text=element_text(size=24)) +
  guides(size=FALSE)


# Actual brooding data
formula_lm <- y ~ x
ggplot(bbyvid, aes(Exact_age_chick, brooding_min, color=Sex, fill=Sex)) +
  geom_point(aes(color=Sex, shape=Sex)) +
  stat_smooth(method = glm, formula = y ~ x, 
              aes(y = brooding_min, color=as.factor(Sex), 
                  fill=as.factor(Sex)), alpha = 0.2,
              se = TRUE, level = 0.95) +
  coord_cartesian(ylim=c(0,40)) + 
  labs(x = "Chick age (day)", y = "Actual brooding (min)") + 
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colors_sex) + 
  guides(color=guide_legend("Sex"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom") +
  stat_poly_eq(aes(label = ..rr.label..), rr.digits = 1, formula = formula_lm, parse = TRUE, label.y.npc = 0.8)



# *** Final linear prov plot By Tmax (min per hr)
ggplot(predbrood, aes(Tmax, pred.b.per.hr, fill=Tmax, color = Tmax)) +
  geom_violin(alpha = 0.5, lwd=1) +
  labs(x = "Maximum air temperature", y = "Predicted brooding (min/hr)") +
  theme_classic() +
  scale_fill_manual(values = colors_bw) +
  scale_color_manual(values = colors_bw) +
  scale_x_discrete(labels=c("Warm\n(23.9 - 31.5 C)","Hot\n(31.5 - 35.6 C)")) +
  theme(legend.position = "none")


# B&W temp violin plot for manuscript
brooding_temp_fig <-
  ggplot(predbrood, aes(Tmax, pred.b.per.hr, fill = Tmax, color = Tmax)) +
    geom_violin(lwd = 0) +
    labs(title = "Figure 2b",
         x = "Maximum air temperature", 
         y = "Predicted brooding (min/hr)") +
    scale_x_discrete(
      labels=c("Warm\n(23.9 - 31.5 C)",
               "Hot\n(31.5 - 35.6 C)")) +
    theme(legend.position = "none") +
    scale_color_grey(start = 0.6, end = 0.3) +
    scale_fill_grey(start = 0.6, end = 0.3) +
    theme_classic() +
    theme(axis.title.x = element_text(size=11), 
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9),
          legend.text = element_text(size=10),
          legend.title = element_text(size=11),
          legend.position = "none") 
ggplot2::ggsave(
  file = "brooding_temp_fig.pdf",
  plot = brooding_temp_fig,
  path ="plots/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 300)

## Poster export 775x400
ggplot(predbrood, aes(Tmax, pred.b.per.hr, fill=Tmax, color = Tmax)) +
  geom_violin() +
  labs(x = "Maximum air temperature", y = "Predicted brooding\n(min/hr)") +
  theme_classic() +
  scale_fill_manual(values = colors_bw) +
  scale_color_manual(values = colors_bw) +
  scale_x_discrete(labels=c("Warm\n(23.9 - 31.5 C)","Hot\n(31.5 - 35.6 C)")) +
  theme(legend.position = "none", text=element_text(size=24)) 

hot <- bbyvid %>% filter(Std_TMAX > 0.12)
hotnot <- bbyvid %>% filter(Std_TMAX < 0.12)

# Comparing interactions b/w Temp and Chick Age
ggplot(predbrood, aes(Exact_age_chick, pred.b.per.hr, color=Tmax, fill=Tmax)) +
  stat_smooth(method = glm, formula = y ~ x, 
              aes(y = pred.b.per.hr, color=as.factor(Tmax), 
                  fill=as.factor(Tmax)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point(aes(color=Tmax, shape=Tmax)) +
  coord_cartesian(ylim=c(0,40)) + 
  labs(x = "Chick age (day)", y = "Predicted brooding (min/hr)") + 
  scale_fill_manual(values = colors_tem) +
  scale_color_manual(values = colors_tem) + 
  guides(color=guide_legend("Tmax")) +
  theme_classic() +
  theme(legend.position = "bottom") 



##############################################################
# Graphs to compare hurdle and non-hurdle versions
# put these at end of slideshow

nonhurdle <- glmmTMB(brooding_min ~ Exact_age_chick + as.factor(Peeped_chick_count) + (1|Brood_ID) + (1|Subject) + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = gaussian())

predbrood_nh <- bbyvid[c("brooding_min", "Subject", "Brood_ID", "Exact_age_chick", "Sex",
                         "Std_TMAX", "Usable_video", "Peeped_chick_count")]

predbrood_nh$pred.b.per.unit <- stats::predict(nonhurdle, predbrood_nh, type="response")
predbrood_nh$pred.b.per.hr <- (predbrood_nh$pred.b.per.unit/predbrood_nh$Usable_video)*60
predbrood_nh$brood_per_hr <- (predbrood_nh$brooding_min/predbrood_nh$Usable_video)*60

ggplot(data = predbrood, aes(x = brooding_min, y= pred.b.per.hr)) +
  geom_point() +
  labs(main = "Hurdle model predictions", x = "Brooding (min/hr)", y = "Predicted brooding (min/hr") + 
  theme_classic()

plot(predbrood$brooding_min, predbrood$pred.b.per.hr)
title(main = "Hurdle model predictions")
ylab(main = "Brooding (min/hr)")

plot(predbrood_nh$brooding_min, predbrood_nh$pred.b.per.hr) 
title(main = "Zero-inflated gaussian model predictions")
