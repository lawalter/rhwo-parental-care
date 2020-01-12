# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)

# data --------------------------------------------------------------------

bbyvid <- read_csv("clean_data/bbyvid.csv")

# script ------------------------------------------------------------------

# Cleaning
bbyvid$Cleaning_rate <- (bbyvid$cleaning/bbyvid$Usable_video)*60
bbyvid$Cleaning_rate_perchk <- (bbyvid$Cleaning_rate)/(bbyvid$Peeped_chick_count)

cleaning_male <- bbyvid %>% filter(Sex == "male")
cleaning_female <- bbyvid %>% filter(Sex == "female")

shapiro.test(cleaning_male$Cleaning_rate_perchk) #not normal
shapiro.test(cleaning_female$Cleaning_rate_perchk) #not normal

cleaning_test <- wilcox.test(byob_final_female$Cleaning_rate_perchk, byob_final_male$Cleaning_rate_perchk, 
                             paired = TRUE, exact=FALSE, alternative = c("two.sided"), conf.level = 0.95)
cleaning_test

Zstat<-qnorm(cleaning_test$p.value/2)
Zstat


# plot --------------------------------------------------------------------

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

# Signficiant cleaning 
ggplot(
  bbyvid, aes(x=Sex, y=Cleaning_rate_perchk, fill=Sex)) + 
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
cleaning_fig <-
  ggplot(
    bbyvid, aes(x=Sex, y=Cleaning_rate_perchk, fill=Sex)) + 
    geom_boxplot() + 
    labs(x = "Sex", 
         y = "Cleaning (per chick/hr)") +
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
  file = "cleaning_fig.pdf",
  plot = cleaning_fig,
  path ="plots/",
  width = 3.5,
  units = "in",
  dpi = 300)

# Poster 
ggplot(
  bbyvid, aes(x=Sex, y=Cleaning_rate_perchk, fill=Sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Cleaning rate\n(per chick/hr)") +
  scale_fill_manual(values = colors_sex) +
  theme_classic() +
  theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24), text = element_text(size=24), legend.position = "none") 
