# Bordeaux survey data: self-reported memory and health
# Created by: Yingyan Wu
# Apr.21.2023

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}

p_load("readr", "tidyverse")

#---- function ----
source(here::here("code", "functions.R"))

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
                                    sh_memory == "5-Poor" ~ 5)) %>%
  dplyr::select(female, age_group, rank, contains(c("srhealth", "srmemory")))

#---- **Check for outliers ----
bordeaux_cleaned %>%
  filter((srmemory_memento < 4 & srmemory_share == 5)|
           (srmemory_memento > 8 & srmemory_share == 2))

bordeaux_cleaned %>%
  filter((srhealth_memento > 50 & srhealth_share == 5)|
           (srhealth_memento < 55 & srhealth_share == 1)|
           (srhealth_memento < 25 & srhealth_share == 2))

#---- Save the data ----
save(bordeaux_cleaned, file = here::here("data", "analysis_data", "Calishare",
                                         "bordeaux_cleaned.RData"))
