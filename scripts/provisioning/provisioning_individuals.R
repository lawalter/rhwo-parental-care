# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(emojifont) # for greek letters

# data --------------------------------------------------------------------

bbyvid <-
  read_csv("clean_data/behaviors.csv") %>%
  rename(feeding = feeding_chicks)

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
new_colors <- c("female" = "#C72841", "male" = "#2841C7")
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
  # mutate(
  #   repeat_f = ifelse(parent == 'SUOR', '⬡', NA),
  #   repeat_f = ifelse(parent == 'SORO', '▶', repeat_f),
  #   repeat_f = ifelse(parent == 'SGBY', '◼', repeat_f),
  #   repeat_f = ifelse(parent == 'WSPR', '◑', repeat_f),
  #   repeat_m = ifelse(parent == 'BUSY', '★', NA),
  #   repeat_m = ifelse(parent == 'ROSO', '✕', repeat_m),
  #   repeat_m2 = ifelse(parent == 'POPS', '⬟', NA)
  # )
  # mutate(
  #   repeat_f = ifelse(parent == 'SUOR', '\U2B21', NA),
  #   repeat_f = ifelse(parent == 'SORO', '\U25B6', repeat_f),
  #   repeat_f = ifelse(parent == 'SGBY', '\U25FC', repeat_f),
  #   repeat_f = ifelse(parent == 'WSPR', '\U25D1', repeat_f),
  #   repeat_m = ifelse(parent == 'BUSY', '\U2605', NA),
  #   repeat_m = ifelse(parent == 'ROSO', '\U2715', repeat_m),
  #   repeat_m2 = ifelse(parent == 'POPS', '\U2B1F', NA)
  # )
  # mutate(
  #   repeat_f = ifelse(parent == 'SUOR', 'α', NA),
  #   repeat_f = ifelse(parent == 'SORO', 'ε', repeat_f),
  #   repeat_f = ifelse(parent == 'SGBY', 'π', repeat_f),
  #   repeat_f = ifelse(parent == 'WSPR', 'ψ', repeat_f),
  #   repeat_m = ifelse(parent == 'BUSY', 'β', NA),
  #   repeat_m = ifelse(parent == 'ROSO', 'γ', repeat_m),
  #   repeat_m = ifelse(parent == 'POPS', 'δ', repeat_m)) 
  mutate(
    repeat_f = ifelse(parent == 'SUOR', '\U03B1', NA), #alpha
    repeat_f = ifelse(parent == 'SORO', '\U03B5', repeat_f), #epsilon
    repeat_f = ifelse(parent == 'SGBY', '\U03C0', repeat_f), #pi
    repeat_f = ifelse(parent == 'WSPR', '\U03A8', repeat_f), #psi
    repeat_m = ifelse(parent == 'BUSY', '\U03B2', NA), #beta
    repeat_m = ifelse(parent == 'ROSO', '\U03B3', repeat_m), #gamma
    repeat_m = ifelse(parent == 'POPS', '\U03B4', repeat_m)) #delta


# Individual provisioning plot (black & white)
ggplot(data = prov_prop_repeats, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75, color = 'white') +
  geom_text(
    aes(label = repeat_m), 
    size = 3.5, 
    position = position_stack(vjust = 0),
    hjust = 'right'
    ) +
  geom_text(
    aes(label = repeat_f), 
    size = 3.5, 
    position = position_stack(vjust = 1), 
    hjust = 'left'
    ) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  coord_flip() + 
  # scale_fill_manual(values = colors_sex_bw) + #light!!
  # scale_fill_manual(values = c('female' = '#6c6c6c', 'male' = '#333333')) + #dark!! 
  scale_fill_manual(
    values =  c('female' = '#858585', #medium
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
  #   file = "fig5_provisioning_proportions.tiff",
  #   path ="plots/manuscript_plots/",
  #   width = 3.5,
  #   height = 4.5,
  #   units = "in",
  #   dpi = 300)


# Individual provisioning plot (color)
prov_prop_repeats %>% 
  mutate(
    sex = 
      case_when(
        sex == "male" ~ "Male",
        sex == "female" ~ "Female",
        TRUE ~ NA_character_)) %>% 
  ggplot(
    aes(x = reorder(brood_id, number), y = proportion, fill = sex)) +
  geom_col(width = 0.75, color = 'white', alpha = 0.7) +
  geom_text(
    aes(label = repeat_m), 
    size = 12, 
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = repeat_f), 
    size = 12, 
    position = position_stack(vjust = 1), 
    hjust = 'left') +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 42), 
        axis.title.y = element_text(size = 42),
        axis.text.y = element_text(size = 36),
        axis.text.x = element_text(size = 36),
        legend.text = element_text(size = 36),
        legend.title = element_text(size = 42),
        legend.position = "bottom") +
  coord_flip() + 
  scale_fill_manual(
    values =  c('Female' = '#C72841', #medium
                'Male' = '#2841C7')) +
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
                            "NB18C4_18_brd1" = "U")) +
  ggsave(
    file = "fig5_provisioning_proportions.png",
    path ="plots/manuscript_plots/",
    width = 3.5,
    height = 4.5,
    units = "in",
    dpi = 300)

# other plots -------------------------------------------------------------

# Plot for individuals with repeat parents (color)
ggplot(data = prov_prop_repeats, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label = repeat_m), 
    size = 3, 
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = repeat_m2), 
    size = 4, 
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = repeat_f), 
    size = 3, 
    position = position_stack(vjust = 1), 
    hjust = 'left') +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID",
       title = "Figure 5") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  coord_flip() + 
  scale_fill_manual(values = colors_sex) +
  guides(fill = guide_legend(reverse = TRUE)) +
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
                            "NB18C4_18_brd1" = "U")) +
  ggsave(
    file = "provisioning_individual_fig_color.png",
    path ="plots/color/",
    width = 3.5,
    units = "in",
    dpi = 600)

# Previous custom labels

# scale_x_discrete(labels=c("A3_brd2" = "A3_2017", 
#                           "Timber_brd1" = "Timber_brood1_2018",
#                           "N17REC3_brd2" = "Rec3_2017",
#                           "NB5A4_brd1" = "NB5A4_2017",
#                           "Timber_brd2" = "Timber_brood2_2018",
#                           "N1718CA2_18" = "18CA2_2018",
#                           "N17REC2_brd1" = "Rec2_2017",
#                           "Gum" = "Gum_2018",
#                           "Tonto_brd1" = "Tonto_2017",
#                           "REC1_brd1" = "Rec1_2017",
#                           "Houdini" = "Houdini_2018",
#                           "Tonto18_brd1" = "Tonto_2018",
#                           "18C_Catfish_brd1" = "Catfish_2017",
#                           "5A Cut_brd2" = "Cut_2018",
#                           "RB23_brd2" = "RB23_2017",
#                           "Cross" = "Cross_2018",
#                           "NB5B5_brd1" = "NB5B5_2017",
#                           "RB36alt_brd2_18" = "RB36alt_2018",
#                           "Noisy" = "Noisy_2018",
#                           "N1718CA2_brd1" = "18CA2_2017",
#                           "NB18C4_18_brd1" = "NB18C4_2018")) +

# Plot with colors for individuals 

prov_prop_color_repeats <- 
  prov_prop %>%
  mutate(
    mean_chick_age = ifelse(sex == 'male', NA, mean_chick_age),
    sex = ifelse(parent == 'SUOR', 'female2', sex),
    sex = ifelse(parent == 'SORO', 'female3', sex),
    sex = ifelse(parent == 'SGBY', 'female4', sex),
    sex = ifelse(parent == 'WSPR', 'female5', sex),
    sex = ifelse(parent == 'BUSY', 'male2', sex),
    sex = ifelse(parent == 'ROSO', 'male3', sex),
    sex = ifelse(parent == 'POPS', 'male4', sex),
    repeat_m = 
      ifelse(parent %in% parent_repeats & sex == 'male', parent, NA),
    repeat_f = 
      ifelse(parent %in% parent_repeats & sex == 'female', parent, NA)) 

# Repeat parent colors
colors_sex_repeats <-
  c("female" = "#DCDCDC", 
    "male" = "#989aa0",
    'female2' = '#F47C89', #repeat female, red
    'female3' = '#f694cf', #repeat female, pink
    'female4' = '#f29f64', #repeat female, orange
    'female5' = '#f4ea7c', #repeat female, yellow
    'male2' = '#35df44', #repeat male, green
    'male3' = '#7cccf4', #repeat male, light blue
    'male4' = '#b67cf4') #repeat male, purple

# Plot for individuals with repeat parents (color)
ggplot(data = prov_prop_color_repeats, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label = repeat_m), 
    size = 2, 
    position = position_stack(vjust = 0)) +
  geom_text(
    aes(label = repeat_f), 
    size = 2, 
    position = position_stack(vjust = 1)) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID",
       title = "Figure 5") + 
  theme_classic() +
  theme(legend.position = 'none') +
  coord_flip() + 
  scale_fill_manual(values = colors_sex_repeats) +
  guides(fill = guide_legend(reverse = TRUE)) +
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
# +
#   ggsave(
#     file = "provisioning_individual_fig_simple_color.pdf",
#     path ="plots/color/",
#     width = 3.5,
#     units = "in",
#     dpi = 600)


# Plot to show ages
ggplot(
  data = prov_prop, 
  aes(x = reorder(brood_id, number), 
      y = proportion, 
      fill = sex)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.position="bottom") +
  geom_text(aes(label = prov_prop_color_repeats$mean_chick_age), 
            size = 3.5,
            position = position_fill(),
            hjust = 0,
            show.legend = FALSE) +
  coord_flip() + 
  scale_fill_manual(values = colors_sex) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels=c("A3_brd2" = "A3_2017", 
                            "Timber_brd1" = "Timber_brood1_2018",
                            "N17REC3_brd2" = "Rec3_2017",
                            "NB5A4_brd1" = "NB5A4_2017",
                            "Timber_brd2" = "Timber_brood2_2018",
                            "N1718CA2_18" = "18CA2_2018",
                            "N17REC2_brd1" = "Rec2_2017",
                            "Gum" = "Gum_2018",
                            "Tonto_brd1" = "Tonto_2017",
                            "REC1_brd1" = "Rec1_2017",
                            "Houdini" = "Houdini_2018",
                            "Tonto18_brd1" = "Tonto_2018",
                            "18C_Catfish_brd1" = "Catfish_2017",
                            "5A Cut_brd2" = "Cut_2018",
                            "RB23_brd2" = "RB23_2017",
                            "Cross" = "Cross_2018",
                            "NB5B5_brd1" = "NB5B5_2017",
                            "RB36alt_brd2_18" = "RB36alt_2018",
                            "Noisy" = "Noisy_2018",
                            "N1718CA2_brd1" = "18CA2_2017",
                            "NB18C4_18_brd1" = "NB18C4_2018"))

# age groups plot ---------------------------------------------------------

prov_prop <-
  prov_prop %>%
  mutate(
    age_group = 
      ifelse(mean_chick_age > 12, 'Old \u2265 13', 'Young < 13'))

ggplot(data = prov_prop, 
       aes(
         x = reorder(brood_id, number), 
         y = proportion, 
         fill = sex)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.text = element_text(size = 10),
        legend.position="bottom") +
  coord_flip() + 
  scale_fill_manual(values = colors_sex) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels=c("A3_brd2" = "A3_2017", 
                            "Timber_brd1" = "Timber_brood1_2018",
                            "N17REC3_brd2" = "Rec3_2017",
                            "NB5A4_brd1" = "NB5A4_2017",
                            "Timber_brd2" = "Timber_brood2_2018",
                            "N1718CA2_18" = "18CA2_2018",
                            "N17REC2_brd1" = "Rec2_2017",
                            "Gum" = "Gum_2018",
                            "Tonto_brd1" = "Tonto_2017",
                            "REC1_brd1" = "Rec1_2017",
                            "Houdini" = "Houdini_2018",
                            "Tonto18_brd1" = "Tonto_2018",
                            "18C_Catfish_brd1" = "Catfish_2017",
                            "5A Cut_brd2" = "Cut_2018",
                            "RB23_brd2" = "RB23_2017",
                            "Cross" = "Cross_2018",
                            "NB5B5_brd1" = "NB5B5_2017",
                            "RB36alt_brd2_18" = "RB36alt_2018",
                            "Noisy" = "Noisy_2018",
                            "N1718CA2_brd1" = "18CA2_2017",
                            "NB18C4_18_brd1" = "NB18C4_2018")) +
  facet_grid(~age_group)
