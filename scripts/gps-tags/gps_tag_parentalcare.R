# libraries ---------------------------------------------------------------

library(tidyverse)
library(psych)

# data --------------------------------------------------------------------

# Video level behavior observation data

behaviors <- 
  read.csv("clean_data/behaviors.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  mutate(
    cleaning_rate = (cleaning_nest/usable_video)*60,
    cleaning_rate_perchk = cleaning_rate/peeped_chick_count,
    prov_rate = (feeding_chicks/usable_video)*60,
    prov_rate_perchk = prov_rate/peeped_chick_count,
    brooding_rate = (brooding_min/usable_video)*60)

# Backpack status info

bpk_status <-
  read.csv("clean_data/bpk_status.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() 

# Incubation rds

read_rds('clean_data/incubation.rds') %>%
  list2env(envir = .GlobalEnv)

# Final incubation behaviors

incubation <-
  read.csv("clean_data/incubation.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() 

# bpk status on parental care (prov, brood, clean) ------------------------

bpk_behaviors <-
  behaviors %>%
  left_join(bpk_status, by = c("video_number", "subject")) %>%
  mutate(
    bpk_status = ifelse(bpk_status != "with", "without", bpk_status),
    bpk_status = ifelse(is.na(bpk_status), "without", bpk_status))

parentalcare_bpk <-
  bpk_behaviors %>%
  select(video_number, subject, bpk_status, prov_rate_perchk, sex,
         brooding_rate, cleaning_rate_perchk, exact_age_chick, julian_date) 

# Sexes
bpk_behaviors %>%
  select(subject, sex, bpk_status) %>%
  distinct() %>%
  filter(bpk_status == "with") %>%
  filter(sex == "female")

bpk_behaviors %>%
  select(subject, sex, bpk_status) %>%
  distinct() %>%
  filter(bpk_status == "without") %>%
  filter(sex == "female")

# provisioning w/ backpack ------------------------------------------------

bpk_p <- 
  parentalcare_bpk %>%
  select(video_number, subject, bpk_status, prov_rate_perchk) %>%
  pivot_wider(
    names_from = "bpk_status",
    values_from = "prov_rate_perchk") %>%
  select(-c(video_number, subject)) 

shapiro.test(bpk_p$with) # not normal
shapiro.test(bpk_p$without) # not normal  

# Nonparametric test (this is fine even w/ the NAs in the table, 
# I tried comparing just the cols without NAs with the same result)
bpk_p_test <- wilcox.test(bpk_p$with, bpk_p$without, conf.level = 0.95) 
bpk_p_test

# z statistic
qnorm(bpk_p_test$p.value/2)

# With (mean age = 13.12, mean jdate = 191)
parentalcare_bpk %>%
  select(prov_rate_perchk, bpk_status) %>%
  filter(bpk_status == "with") %>%
  select(prov_rate_perchk) %>% 
  pull() %>%
  describe(.)

# Without (mean age = 13.82, mean jdate = 185)
parentalcare_bpk %>%
  select(prov_rate_perchk, bpk_status) %>%
  filter(bpk_status == "without") %>%
  select(prov_rate_perchk) %>% 
  pull() %>%
  describe(.)

# brooding w/ backpack ----------------------------------------------------

bpk_b <- 
  parentalcare_bpk %>%
  select(video_number, subject, bpk_status, brooding_rate) %>%
  pivot_wider(
    names_from = "bpk_status",
    values_from = "brooding_rate") %>%
  select(-c(video_number, subject)) 

shapiro.test(bpk_b$with) # not normal
shapiro.test(bpk_b$without) # not normal  

# Nonparametric test (this is fine even w/ the NAs in the table, 
# I tried comparing just the cols without NAs with the same result)
bpk_b_test <- wilcox.test(bpk_b$with, bpk_b$without, conf.level = 0.95) 
bpk_b_test 

# z statistic
qnorm(bpk_b_test$p.value/2)

# With 
parentalcare_bpk %>%
  select(brooding_rate, bpk_status) %>%
  filter(bpk_status == "with") %>%
  select(brooding_rate) %>% 
  pull() %>%
  describe(.)

# Without 
parentalcare_bpk %>%
  select(brooding_rate, bpk_status) %>%
  filter(bpk_status == "without") %>%
  select(brooding_rate) %>% 
  pull() %>%
  describe(.)

# cleaning w/ backpack ----------------------------------------------------

bpk_c <- 
  parentalcare_bpk %>%
  select(video_number, subject, bpk_status, cleaning_rate_perchk) %>%
  pivot_wider(
    names_from = "bpk_status",
    values_from = "cleaning_rate_perchk") %>%
  select(-c(video_number, subject)) 

shapiro.test(bpk_c$with) # not normal
shapiro.test(bpk_c$without) # not normal  

# Nonparametric test (this is fine even w/ the NAs in the table, 
# I tried comparing just the cols without NAs with the same result)
bpk_c_test <- wilcox.test(bpk_c$with, bpk_c$without, conf.level = 0.95) 
bpk_c_test 

# z statistic
qnorm(bpk_c_test$p.value/2)

# With 
parentalcare_bpk %>%
  select(cleaning_rate_perchk, bpk_status) %>%
  filter(bpk_status == "with") %>%
  select(cleaning_rate_perchk) %>% 
  pull() %>%
  describe(.)

# Without 
parentalcare_bpk %>%
  select(cleaning_rate_perchk, bpk_status) %>%
  filter(bpk_status == "without") %>%
  select(cleaning_rate_perchk) %>% 
  pull() %>%
  describe(.)

# incubating w/ backpack --------------------------------------------------

incubation_bpk <-
  incubation %>%
  left_join(
    sex_data %>% 
      select(rhwo_key, subject), 
    by = "subject") %>%
  left_join(
    bpk_data %>% 
      select(rhwo_key, bpk_status), 
    by = "rhwo_key") %>%
  mutate(
    bpk_status = ifelse(bpk_status != "with", "without", bpk_status),
    bpk_status = ifelse(is.na(bpk_status), "without", bpk_status),
    video_id = paste(video_number, subject, sep = "_")) %>%
  select(video_id, sex, bpk_status, incubation_rate) %>%
  mutate(
    with = ifelse(bpk_status == "with", incubation_rate, NA),
    without = ifelse(bpk_status == "without", incubation_rate, NA))

shapiro.test(incubation_bpk$with) # normal
shapiro.test(incubation_bpk$without) # normal  

bpk_i_test <- 
  t.test(incubation_bpk$with, incubation_bpk$without, conf.level = 0.95) 
bpk_i_test

# With 
incubation_bpk %>%
  select(video_id, incubation_rate, bpk_status) %>%
  filter(bpk_status == "with") %>%
  select(incubation_rate) %>% 
  pull() %>%
  describe(.)

# Without 
incubation_bpk %>%
  select(incubation_rate, bpk_status) %>%
  filter(bpk_status == "without") %>%
  select(incubation_rate) %>% 
  pull() %>%
  describe(.)

# Sexes
incubation_bpk %>%
  select(video_id, sex, bpk_status) %>%
  mutate(subject = str_remove_all(video_id, "^[0-9]{1,3}\\_")) %>%
  select(-video_id) %>%
  distinct() %>%
  filter(bpk_status == "with") %>%
  filter(sex == "female")

# figure ------------------------------------------------------------------

boxplot_data <-
  parentalcare_bpk %>%
  select(video_number, subject, sex, bpk_status, brooding_rate, 
         prov_rate_perchk, cleaning_rate_perchk) %>%
  bind_rows(
    incubation_bpk %>%
      select(sex, bpk_status, incubation_rate)) %>%
  #select(bpk_status:incubation_rate) %>%
  pivot_longer(
    cols = brooding_rate:incubation_rate,
    names_to = "behavior",
    values_to = "rate"
  ) %>%
  mutate(
    behavior = 
      case_when(
        behavior == "brooding_rate" ~ "brooding",
        behavior == "cleaning_rate_perchk" ~ "cleaning",
        behavior == "incubation_rate" ~ "incubating",
        behavior == "prov_rate_perchk" ~ "provisioning",
        TRUE ~ behavior)
      )

# Durations
      
incubation_box <-
  ggplot(
  boxplot_data %>% 
    filter(behavior == "incubating"), 
  aes(behavior, rate, fill = bpk_status)) +
  geom_boxplot() +
  labs(title = "Figure 7",
       x = NULL, 
       y = "Rate (min/hr)") +
  scale_fill_manual(values = c("with" = "#FFFFFF", "without" = "#ABABAB")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")

brood_box <-
  ggplot(
    boxplot_data %>% 
      filter(behavior == "brooding"), 
    aes(behavior, rate, fill = bpk_status)) +
  geom_boxplot() +
  labs(x = NULL, 
       y = "Rate (min/hr)") +
  scale_fill_manual(values = c("with" = "#FFFFFF", "without" = "#ABABAB")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")

# Point events

clean_box <-
  ggplot(
  boxplot_data %>% 
    filter(behavior == "cleaning"), 
  aes(behavior, rate, fill = bpk_status)) +
  geom_boxplot() +
  labs(x = NULL, 
       y = "Rate per (chick/hr)") +
  guides(fill = guide_legend(title = "GPS tag")) +
  scale_fill_manual(values = c("with" = "#FFFFFF", "without" = "#ABABAB")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")

prov_box <-
  ggplot(
    boxplot_data %>% 
      filter(behavior == "provisioning"), 
    aes(behavior, rate, fill = bpk_status)) +
  geom_boxplot() +
  labs(x = NULL, 
       y = "Rate per (chick/hr)") +
  guides(fill = guide_legend(title = "GPS tag")) +
  scale_fill_manual(values = c("with" = "#FFFFFF", "without" = "#ABABAB")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none")

# Legend
gps_legend <- 
  cowplot::get_legend(
    prov_box + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

# combined plots ----------------------------------------------------------

fourplots <- 
  cowplot::plot_grid(incubation_box, brood_box, prov_box, clean_box,
                     labels = 'AUTO',
                     nrow = 2, ncol = 2)

cowplot::plot_grid(fourplots, 
                   gps_legend,
                   ncol = 1, rel_heights = c(1, .1)) +
  ggsave(
    file = "gps_behavior_plots.pdf",
    path ="plots/bw/",
    width = 7,
    height = 7,
    units = "in",
    dpi = 600)
