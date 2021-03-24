# libraries ---------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(emojifont) # for greek letters

# data --------------------------------------------------------------------

# Behavior data - mac

bbyvid <-
  read_csv("clean_data/behaviors.csv") %>%
  mutate(feeding = feeding_chicks) %>%
  select(-c(feeding_chicks))

# Backpack status info - mac

bpk_status <-
  read_csv("clean_data/bpk_status.csv") %>% 
  mutate(video_id = paste(video_number, subject, sep = "_")) %>%
  select(-c(video_number, subject))


# wrangling ---------------------------------------------------------------

# Rates
individ <-
  bbyvid %>%
  left_join(bpk_status, by = "video_id") %>%
  mutate(
    feeding_rate = (feeding/usable_video)*60,
    feeding_rate_perchk = feeding_rate/peeped_chick_count) %>%
  select(video_id, video_number, subject, sex, year, brood_id, 
         feeding_rate_perchk, bpk_status, exact_age_chick)

# Provisioning by video
# This is where the script diverges from the other version, because
# here we aim to assess variation in parental care on a video level
# to better see how GPS tags might affect M/F feeding proportions
cast_video_prov <- 
  dcast(individ, video_number ~ sex, 
        value.var = "feeding_rate_perchk", 
        fun.aggregate = mean) %>% 
  as_tibble() %>%
  mutate(
    m_plus_f_rate = male + female,
    f_proportion = female/m_plus_f_rate,
    m_proportion = male/m_plus_f_rate)

cast_m_prop <- 
  cast_video_prov %>%
  select(video_number, proportion = m_proportion) %>%
  mutate(sex = 'male') %>%
  arrange(desc(proportion)) %>%
  mutate(number = row_number())

cast_f_prop <- 
  cast_video_prov %>%
  select(video_number, proportion = f_proportion) %>%
  mutate(sex = 'female') %>%
  arrange((proportion)) %>%
  mutate(number = row_number())

prov_prop <- 
  bind_rows(cast_f_prop, cast_m_prop) %>%
  left_join(
    individ %>%
      select(sex, video_number, bpk_status, exact_age_chick), 
    by = c('sex', 'video_number')) %>%
  # Some NAs, but these were just unbanded birds
  mutate(
    bpk_status = 
      ifelse(is.na(bpk_status), "none", bpk_status),
    video_number = as.character(video_number)) %>%
  mutate(
    bpk_status_m = 
      ifelse(!str_detect(bpk_status, "with"), "", bpk_status),
    bpk_status_m = ifelse(sex == "male", bpk_status_m, ""),
    bpk_status_f = 
      ifelse(!str_detect(bpk_status, "with"), "", bpk_status),
    bpk_status_f = ifelse(sex == "female", bpk_status_f, ""),
    exact_age_chick = ifelse(sex == "female", exact_age_chick, ""))

# plots -------------------------------------------------------------------

# Individual provisioning plot (black & white)
ggplot(data = prov_prop, 
       aes(x = reorder(video_number, number), 
           y = proportion, 
           fill = sex)) +
  geom_col(width = 0.75, color = 'white') +
  geom_text(
    aes(label = bpk_status_m),
    size = 3.5,
    position = position_stack(vjust = 0),
    hjust = 'right') +
  geom_text(
    aes(label = bpk_status_f),
    size = 3.5,
    position = position_stack(vjust = 1),
    hjust = 'left') +
  geom_text(
    aes(y = 0.5, label = exact_age_chick),
    size = 3.5,
    hjust = 'left',
    color = 'black') +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  labs(y = "Provisioning proportion", 
       x = "Video",
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
  # scale_fill_manual(
  #   values =  c('female' = '#858585', #medium
  #               'male' = '#4c4c4c')) +
  guides(fill = guide_legend('sex', reverse = TRUE)) 
