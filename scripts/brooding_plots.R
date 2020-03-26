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

bbyvid <- 
  read.csv("clean_data/bbyvid.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  # Calculate standardized temperatures
  mutate(
    std_tmax = (tmax - mean(tmax))/sd(tmax),
    brood_perhr = (brooding_min/usable_video)*60)

# script ------------------------------------------------------------------

# Run best model
brood_mod <- 
  glmmTMB(brooding_min ~ exact_age_chick*sex + 
            peeped_chick_count + std_tmax + 
            (1|brood_id) + offset(log(usable_video)), 
          data = bbyvid, 
          ziformula = ~., 
          family = "truncated_nbinom1")

summary(brood_mod)

# Generate dataframes with needed model variables to generate prediction
brooding_predictions <- 
  bbyvid %>%
  select(brooding_min, subject, brood_id, exact_age_chick, sex, habitat,
         std_tmax, usable_video, peeped_chick_count) 

# Calculate brooding rate (brooding per minute)
brooding_predictions <-
  brooding_predictions %>%
  mutate(
    predicted_brood_per_unit = 
      stats::predict(brood_mod, brooding_predictions, type = "response"),
    predicted_brood_per_hr =
      (predicted_brood_per_unit/usable_video)*60,
    brood_per_hr = (brooding_min/usable_video)*60,
    tmax = ifelse(std_tmax > 0.12, "Hot", "Cool"))

# Colors
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_tem <- c("Cool" = "#6bd2db", "Hot" = "#fc913a")
colors_bw <- c("Cool" = "#ABABAB", "Hot" = "#333333")

predbrood_conditional <- 
  brooding_predictions %>% 
  filter(predicted_brood_per_unit > 1)

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
  coord_cartesian(ylim = c(0,40)) + 
  labs(title = "Figure 3a",
       x = "Chick age (day)", 
       y = "Predicted brooding (min/hr)") + 
  guides(color = guide_legend("Sex"), 
         shape = guide_legend("Sex"),
         linetype = guide_legend("Sex")) +
  scale_shape_manual(values = c(17, 1)) +
  scale_color_manual(values = c("female" = "black", "male" = "black")) +
  scale_fill_grey(start = 0.6, end = 0.6) +
  theme_classic() +
  theme(axis.title.x = element_text(size=11), 
        axis.title.y = element_text(size=11),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = "bottom") +
  ggplot2::ggsave(
  file = "brooding_sex_fig.pdf",
  path ="plots/bw/brooding/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 600)

# violin plots ------------------------------------------------------------

# Violin plot - by temperature (black & white)
brooding_violin <- 
  ggplot(brooding_predictions, 
       aes(tmax, 
           predicted_brood_per_hr, 
           fill = tmax, 
           color = tmax)) +
  geom_violin(lwd = 0) +
  labs(title = "Figure 3b",
       x = "Maximum daily air temperature", 
       y = "Predicted brooding (min/hr)") +
  scale_x_discrete(
    labels=c("Warm\n(23.9 - 31.5°C)",
             "Hot\n(31.5 - 35.6°C)")) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Cool" = "#333333", "Hot" = "#333333")) +
  scale_fill_manual(values = c("Cool" = "#333333", "Hot" = "#333333")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "none") +
  ggsave(
    file = "brooding_T_fig.pdf",
    path ="plots/brooding/",
    width = 3.5,
    height = 3,
    units = "in",
    dpi = 600)

# stacked brooding plots --------------------------------------------------

cowplot::plot_grid(brooding_sex, brooding_violin, 
          labels = 'AUTO',
          nrow = 2, ncol = 2) +
  ggsave(
    file = "brooding_plots_pair.pdf",
    path ="plots/brooding",
    width = 3.5,
    height = 3,
    units = "in",
    dpi = 600)

# comparison --------------------------------------------------------------
  
# Comparing interactions b/w temp and chick age
ggplot(brooding_predictions, 
       aes(exact_age_chick, 
           predicted_brood_per_hr, 
           color = tmax, 
           fill = tmax)) +
  stat_smooth(method = glm, 
              formula = y ~ x, 
              aes(y = predicted_brood_per_hr, 
                  color = as.factor(tmax), 
                  fill = as.factor(tmax)), 
              alpha = 0.2,
              se = TRUE, 
              level = 0.95, 
              fullrange = TRUE) +
  geom_point(aes(color = tmax, shape = tmax)) +
  coord_cartesian(ylim = c(0,40)) + 
  labs(x = "Chick age (day)", y = "Predicted brooding (min/hr)") + 
  scale_fill_manual(values = colors_tem) +
  scale_color_manual(values = colors_tem) + 
  guides(color = guide_legend("Tmax")) +
  theme_classic() +
  theme(legend.position = "bottom") 

# actual brooding data plot -----------------------------------------------

# Plotting real values, not predictions
ggplot(bbyvid, 
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
          data = bbyvid, ziformula = ~1, family = gaussian())

predbrood_nh <- 
  bbyvid %>%
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
       y = "Predicted brooding (min/hr") + 
  theme_classic()

plot(predbrood$brooding_min, predbrood$predicted_brood_per_hr)
title(main = "Hurdle model predictions")
ylab(main = "Brooding (min/hr)")

plot(predbrood_nh$brooding_min, predbrood_nh$predicted_brood_per_hr) 
title(main = "Zero-inflated gaussian model predictions")
