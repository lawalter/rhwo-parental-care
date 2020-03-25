# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)

# data --------------------------------------------------------------------

bbyvid <- read.csv("clean_data/bbyvid.csv", stringsAsFactors = FALSE)

# script ------------------------------------------------------------------

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
  mutate_if(is.numeric, round, digits = 1)

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
  mutate(mean_chick_age = ifelse(sex == 'male', NA, mean_chick_age))

Brood_ID_2 <- c("")

# plots -------------------------------------------------------------------

colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

# Individual provisioning plot (color)
ggplot(data = prov_prop, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID",
       title = "Figure 5") + 
  theme_classic() +
  theme(legend.text = element_text(size = 10),
        legend.position = "bottom") +
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
  ggsave(
    file = "provisioning_individual_fig_color.pdf",
    path ="plots/color/",
    width = 3.5,
    units = "in",
    dpi = 600)

# Individual provisioning plot (black & white)
ggplot(
  data = prov_prop, 
  aes(x = reorder(brood_id, number), 
      y = proportion, 
      fill = sex)) +
  geom_col(width=0.75) +
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
  scale_fill_grey(start = 0.6, end = 0.3) +
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
  ggsave(
    file = "provisioning_individual_fig_bw.png",
    path ="plots/bw/",
    width = 3.5,
    units = "in",
    dpi = 600)

# simplified brood name plots ---------------------------------------------

# Individual provisioning plot (color)
ggplot(data = prov_prop, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID",
       title = "Figure 5") + 
  theme_classic() +
  theme(legend.text = element_text(size = 10),
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
    file = "provisioning_individual_fig_simple_color.pdf",
    path ="plots/color/",
    width = 3.5,
    units = "in",
    dpi = 600)

# Individual provisioning plot (black & white)
ggplot(
  data = prov_prop, 
  aes(x = reorder(brood_id, number), 
      y = proportion, 
      fill = sex)) +
  geom_col(width=0.75) +
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
  scale_fill_grey(start = 0.6, end = 0.3) +
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
    file = "provisioning_individual_fig_simple_bw.png",
    path ="plots/bw/",
    width = 3.5,
    units = "in",
    dpi = 600)


# plot with color per bird ------------------------------------------------

ggplot(data = prov_prop, 
       aes(x = reorder(brood_id, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID",
       title = "Figure 5") + 
  theme_classic() +
  theme(legend.text = element_text(size = 10),
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
    file = "provisioning_individual_fig_simple_color.pdf",
    path ="plots/color/",
    width = 3.5,
    units = "in",
    dpi = 600)


# plot with mean chick ages -----------------------------------------------

## Plot to show ages
ggplot(data = prov_prop, aes(reorder(brood_id, number), proportion, fill = sex)) +
  geom_col(width=0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.position="bottom") +
  geom_text(aes(label = prov_prop$mean_chick_age), 
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

## Plot to show ages in groups
prov_prop_cats$`Age Group` <- ifelse(prov_prop_cats$Mean_age > 12, "Old \u2265 13", "Young < 13")

ggplot(data = prov_prop_cats, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_col(width=0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.position="bottom") +
  geom_text(aes(label = prov_prop$Mean_age), 
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
                            "NB18C4_18_brd1" = "NB18C4_2018")) +
  facet_grid(~`Age Group`)



ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.position="bottom") +
  scale_fill_manual(values = colors_sex) 


