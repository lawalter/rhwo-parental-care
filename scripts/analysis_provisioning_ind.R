# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)

# data --------------------------------------------------------------------

bbyvid <- read_csv("clean_data/bbyvid.csv")

# script ------------------------------------------------------------------

# Rates
bbyvid_wrates <- bbyvid
bbyvid_wrates$Feeding_rate <- (bbyvid_wrates$feeding/bbyvid_wrates$Usable_video)*60
bbyvid_wrates$Feeding_rate_perchk <- (bbyvid_wrates$Feeding_rate)/(bbyvid$Peeped_chick_count)

# Provisioning by brood   
cast_broodprov <- dcast(bbyvid_wrates, Brood_ID + Year ~ Sex, 
                        value.var="Feeding_rate_perchk", fun.aggregate=mean)

cast_agemeans <- dcast(bbyvid_wrates, Brood_ID + Year ~ Sex, 
                       value.var="Exact_age_chick", fun.aggregate=mean)
cast_agemeans <- select(cast_agemeans, Brood_ID, female)
names(cast_agemeans)[2] <- c("Mean_age")
cast_agemeans <- cast_agemeans %>% mutate_if(is.numeric, round, digits = 1)

cast_broodprov$MplusF_rate <- cast_broodprov$male + cast_broodprov$female
cast_broodprov$F_proportion <- cast_broodprov$female/cast_broodprov$MplusF_rate
cast_broodprov$M_proportion <- cast_broodprov$male/cast_broodprov$MplusF_rate

cast_m_prop <- select(cast_broodprov, Brood_ID, M_proportion)
cast_m_prop$Sex <- "male"
names(cast_m_prop)[2] <- "proportion"
cast_m_prop <- cast_m_prop[order(-cast_m_prop$proportion),]
cast_m_prop$Number <- seq.int(nrow(cast_m_prop))

cast_f_prop <- select(cast_broodprov, Brood_ID, F_proportion)
cast_f_prop$Sex <- "female"
names(cast_f_prop)[2] <- "proportion"
cast_f_prop <- cast_f_prop[order(cast_f_prop$proportion),]
cast_f_prop$Number <- seq.int(nrow(cast_f_prop))

prov_prop <- rbind(cast_f_prop, cast_m_prop)

prov_prop <- merge(prov_prop, cast_agemeans, by = "Brood_ID")
prov_prop_cats <- prov_prop
prov_prop$Mean_age <- ifelse(prov_prop$Sex == "male", NA, prov_prop$Mean_age)

Brood_ID_2 <- c("")

library(ggplot2)
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")

ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.position="bottom") +
  coord_flip() + 
  scale_fill_manual(values = colors_sex) 

ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=10),
        legend.position="bottom") +
  coord_flip() + 
  scale_fill_manual(values = colors_sex) +
  guides(fill = guide_legend(reverse = TRUE)) 

## Text for thesis document, not 14 pt: 442 x 453
ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_col(width=0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(legend.text = element_text(size=10),
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
                            "NB18C4_18_brd1" = "NB18C4_2018"))

## B&W for manuscript:
## Export as 3.5 in x 4 in
prov_bar_fig <-
  ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
    geom_col(width=0.75) +
    geom_hline(yintercept = 0.50, linetype = "dashed") +
    labs(title = "Figure 4",
         y = "Provisioning proportion", 
         x = "Brood ID") + 
    theme_classic() +
    theme(axis.title.x = element_text(size=11), 
          axis.title.y = element_text(size=11),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9),
          legend.text = element_text(size=10),
          legend.title = element_text(size=11),
          legend.position="bottom") +
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
                              "NB18C4_18_brd1" = "NB18C4_2018"))
ggplot2::ggsave(
       file = "provisioning_individual_fig.pdf",
       plot = prov_bar_fig,
       path ="plots/",
       width = 3.5,
       units = "in",
       dpi = 300)


## Plot to show ages
ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
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



## Poster export: 850x500
ggplot(data = prov_prop, aes(reorder(Brood_ID, Number), proportion, fill = Sex)) +
  geom_col(width=0.75) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Brood ID") + 
  theme_classic() +
  theme(axis.title.x = element_text(size=24), 
        axis.title.y = element_text(size=24),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=24),
        legend.position="right") +
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
