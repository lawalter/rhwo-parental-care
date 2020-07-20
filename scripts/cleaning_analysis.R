# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)

# data --------------------------------------------------------------------

bbyvid <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(-X) 

# script ------------------------------------------------------------------

# Cleaning
bbyvid <-
  bbyvid %>%
  mutate(
    cleaning_rate = (cleaning_nest/usable_video)*60,
    cleaning_rate_perchk = cleaning_rate/peeped_chick_count)

cleaning_male <- bbyvid %>% filter(sex == "male")
cleaning_female <- bbyvid %>% filter(sex == "female")

shapiro.test(cleaning_male$cleaning_rate_perchk) #not normal
shapiro.test(cleaning_female$cleaning_rate_perchk) #not normal

cleaning_test <- 
  wilcox.test(cleaning_female$cleaning_rate_perchk, 
              cleaning_male$cleaning_rate_perchk, 
              paired = TRUE, 
              exact = FALSE, 
              alternative = c("two.sided"), 
              conf.level = 0.95)
cleaning_test

zstat <- qnorm(cleaning_test$p.value/2)
zstat


# plot --------------------------------------------------------------------

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

# Signficiant cleaning by sex (color)
ggplot(
  bbyvid, aes(x = sex, y = cleaning_rate_perchk, fill = sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Cleaning (per chick/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme(axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=10),
        text = element_text(size=14),
        legend.position = "none") 

# B&W cleaning 
ggplot(
  bbyvid, 
  aes(x = sex, y = cleaning_rate_perchk, fill = NULL)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Cleaning rate (per chick/hr)",
       title = "Figure 6") +
  scale_fill_grey(start = 0.6, end = 0.3) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none") +
  ggsave(
  file = "cleaning_fig.pdf",
  path ="plots/bw/",
  width = 3.5,
  height = 3,
  units = "in",
  dpi = 600)
