# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(extrafont)

font_import()
loadfonts()
fonts()

# data --------------------------------------------------------------------

# On mac
bbyvid <-
  read.csv("~/Desktop/abby_git/rhwo-parental-care/clean_data/behaviors.csv",
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(feeding = feeding_chicks) %>%
  select(-feeding_chicks)

# On laptop
bbyvid <-
  read.csv('~/Desktop/rhwo-parental-care/clean_data/behaviors.csv',
           stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(feeding = feeding_chicks) %>%
  select(-feeding_chicks)

# wrangling ---------------------------------------------------------------

# Rates
bbyvid <-
  bbyvid %>%
  mutate(
    feeding_rate = (feeding/usable_video)*60,
    feeding_rate_perchk = feeding_rate/peeped_chick_count)

# Provisioning by brood   
cast_broodprov <- 
  dcast(bbyvid, brood_id + year ~ sex, 
        value.var = "feeding_rate_perchk", 
        fun.aggregate = mean) %>% 
  as_tibble() %>%
  mutate(
    m_plus_f_rate = male + female,
    f_proportion = female/m_plus_f_rate,
    m_proportion = male/m_plus_f_rate)

cast_agemeans <- 
  dcast(bbyvid, brood_id + year ~ sex, 
        value.var = "exact_age_chick", 
        fun.aggregate = mean) %>%
  as_tibble() %>%
  select(brood_id, mean_chick_age = female) %>% 
  mutate_if(is.numeric, round, digits = 0)

cast_m_prop <- 
  cast_broodprov %>%
  select(brood_id, proportion = m_proportion) %>%
  mutate(sex = 'male') %>%
  arrange(desc(proportion)) %>%
  mutate(number = row_number())

cast_f_prop <- 
  cast_broodprov %>%
  select(brood_id, proportion = f_proportion) %>%
  mutate(sex = 'female') %>%
  arrange((proportion)) %>%
  mutate(number = row_number())

prov_prop <- 
  bind_rows(cast_f_prop, cast_m_prop) %>%
  left_join(cast_agemeans, by = 'brood_id') %>%
  left_join(
    bbyvid %>%
      as_tibble() %>%
      select(brood_id, subject, sex) %>%
      distinct() %>%
      pivot_wider(names_from = sex, values_from = subject),
    by = 'brood_id') %>%
  mutate(
    male = ifelse(sex == 'male', male, NA),
    female = ifelse(sex =='female', female, NA),
    parent = ifelse(!is.na(male), male, female)) %>%
  select(-c(male, female))

# plot colors -------------------------------------------------------------

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_sex_bw <- c("female" = "#c4c6ce", "male" = "#888a90")

# plots -------------------------------------------------------------------

# Parent repeats
parent_repeats <-
  prov_prop %>%
  select(parent) %>%
  group_by(parent) %>%
  filter(n() > 1) %>%
  distinct() %>%
  arrange(parent) %>%
  pull()

prov_prop_repeats <- 
  prov_prop %>%
  mutate(
    repeat_f = ifelse(parent == 'SUOR', '\U2B21', NA),
    repeat_f = ifelse(parent == 'SORO', '\U25B6', repeat_f),
    repeat_f = ifelse(parent == 'SGBY', '\U25FC', repeat_f),
    repeat_f = ifelse(parent == 'WSPR', '\U25D1', repeat_f),
    repeat_m = ifelse(parent == 'BUSY', '\U2605', NA),
    repeat_m = ifelse(parent == 'ROSO', '\U2715', repeat_m),
    repeat_m2 = ifelse(parent == 'POPS', '\U2B1F', NA)
  )

# Individual provisioning plot (black & white)
ggplot(data = prov_prop_repeats, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75, color = 'white') +
  geom_text(
    aes(label = repeat_m, family = "FreeMono"), 
    size = 4, 
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = repeat_m2, family = "FreeMono"), 
    size = 3.5, 
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = repeat_f, family = "FreeMono"), 
    size = 3.5, 
    position = position_stack(vjust = 1), 
    hjust = 'left') +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood",
       title = "Figure 5") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  coord_flip() + 
  scale_fill_manual(
    values =  c('female' = '#858585', 
                'male' = '#4c4c4c')) +
  guides(fill = guide_legend('Sex', reverse = TRUE)) +
  scale_x_discrete(labels=c("A3_brd2" = "A", 
                            "Timber_brd1" = "B",
                            "N17REC3_brd2" = "C",
                            "NB5A4_brd1" = "D",
                            "Timber_brd2" = "E",
                            "N1718CA2_18" = "F",
                            "N17REC2_brd1" = "G",
                            "Gum" = "H",
                            "Tonto_brd1" = "I",
                            "REC1_brd1" = "J",
                            "Houdini" = "K",
                            "Tonto18_brd1" = "L",
                            "18C_Catfish_brd1" = "M",
                            "5A Cut_brd2" = "N",
                            "RB23_brd2" = "O",
                            "Cross" = "P",
                            "NB5B5_brd1" = "Q",
                            "RB36alt_brd2_18" = "R",
                            "Noisy" = "S",
                            "N1718CA2_brd1" = "T",
                            "NB18C4_18_brd1" = "U")) 
  # ggsave(
  #   file = "provisioning_individual_fig.pdf",
  #   path ="plots/bw/",
  #   width = 3.5,
  #   height = 4,
  #   units = "in",
  #   dpi = 600)