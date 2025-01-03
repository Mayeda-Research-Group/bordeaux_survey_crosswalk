# Bordeaux survey data: self-reported memory and health
# Created by: Yingyan Wu
# Apr.21.2023

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}

p_load("readr", "tidyverse", "rstatix")

#---- Load the data ----
bordeaux <- read_csv(
  here::here("data", "analysis_data", "Calishare", "calishare_20230918.csv"))

#---- preprocessing the data ----
bordeaux_cleaned <- bordeaux %>%
  filter(complete == "Yes") %>%
  # Age: < 60, 60 - 69, 70 - 79, 80 - 89, > 90
  mutate(age_group = case_when(age == "[60-69]" ~ 2,
                               age == "[70-79]" ~ 3,
                               age == "[80-89]" ~ 4),
         female = case_when(sex == "female" ~ 1,
                            sex == "male" ~ 0),
         srhealth_memento = mem_health,
         srhealth_share = case_when(sh_health == "1-Excellent" ~ 1,
                                    sh_health == "2-Very good" ~ 2,
                                    sh_health == "3-Good" ~ 3,
                                    sh_health == "4-Fair" ~ 4,
                                    sh_health == "5-Poor" ~ 5),
         srmemory_memento = mem_memory,
         srmemory_share = case_when(sh_memory == "1-Excellent" ~ 1,
                                    sh_memory == "2-Very good" ~ 2,
                                    sh_memory == "3-Good" ~ 3,
                                    sh_memory == "4-Fair" ~ 4,
                                    sh_memory == "5-Poor" ~ 5),
         across(c("srmemory_share", "srhealth_share"),
                ~ case_when(. %in% c(1, 2) ~ 1,
                            . == 3 ~ 2,
                            . %in% c(4, 5) ~ 3), .names = "{.col}_3cat")) %>%
  dplyr::select(female, age_group, rank, contains(c("srhealth", "srmemory")))

##---- Check for outliers ----
# R1: Identifying outliers based on 1.5 IQR rules:
bordeaux_cleaned <- bordeaux_cleaned %>%
  mutate(srhealth_outlier = is_outlier(srhealth_memento),
         srmemory_outlier = is_outlier(srmemory_memento)) %>%
  group_by(srhealth_share_3cat) %>%
  mutate(srhealth_outlier_crs = is_outlier(srhealth_memento)) %>%
  group_by(srmemory_share_3cat) %>%
  mutate(srmemory_outlier_crs = is_outlier(srmemory_memento)) %>%
  ungroup()

with(bordeaux_cleaned, table(srhealth_outlier, srhealth_outlier_crs, useNA = "ifany"))
with(bordeaux_cleaned, table(srmemory_outlier, srmemory_outlier_crs, useNA = "ifany"))

# Sanity check
bordeaux_cleaned %>%
  select(-contains("srmemory")) %>%
  group_by(srhealth_share_3cat) %>%
  identify_outliers("srhealth_memento")

bordeaux_cleaned %>%
  select(-contains("srhealth")) %>%
  group_by(srmemory_share_3cat) %>%
  identify_outliers("srmemory_memento")


# bordeaux_cleaned %>%
#   filter((srmemory_memento < 4 & srmemory_share == 5)|
#            (srmemory_memento > 8 & srmemory_share == 2))
# 
# bordeaux_cleaned %>%
#   filter((srhealth_memento > 50 & srhealth_share == 5)|
#            (srhealth_memento < 55 & srhealth_share == 1)|
#            (srhealth_memento < 25 & srhealth_share == 2))

# # Test
# bordeaux_cleaned %>%
#   dplyr::select(srhealth_memento, srmemory_memento, 
#                 srhealth_share_3cat, srmemory_share_3cat) %>%
#   dplyr::rename(srhealth_share = srhealth_share_3cat,
#                 srmemory_share= srmemory_share_3cat) %>% 
#   pivot_longer(cols = contains(c("srhealth", "srmemory")),
#                names_to = c("measure", ".value"),
#                names_pattern = "(.*)_(.*)") %>%
#   filter(measure == "srmemory", share == 1) %>% 
#   # filter(measure =="srhealth", share ==1, memento < 70) %>% 
#   mutate(share = factor(share, levels = rev(1:3))) %>%
#   ggplot() + 
#   # geom_boxplot(aes(x = share, y = memento))+ # for some reason, adding a box plot will add a dot
#   geom_jitter(aes(x = share,y = memento), width = 0.3, height = 0) +
#   scale_y_continuous(n.breaks = 6) +
#   scale_x_discrete(labels = c("1" = "Excellent/very good",
#                               "2" = "Good",
#                               "3" = "Fair/poor")) +
#   facet_grid(rows = vars(measure), scales = "free_y",
#              labeller = labeller(measure = c("srhealth" = "Self-rated health",
#                                              "srmemory" = "Self-rated memory"))) +
#   theme_bw() +
#   labs(x="Likert scale", y="continuous")

#---- Save the data ----
save(bordeaux_cleaned, file = here::here("data", "analysis_data", "Calishare",
                                         "bordeaux_cleaned.RData"))
