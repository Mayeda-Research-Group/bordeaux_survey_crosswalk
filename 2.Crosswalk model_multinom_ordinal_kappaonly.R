# Crosswalk model Bordeaux survey data: self-reported memory and health
# Created by: Yingyan Wu
# May.18.2023

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}
p_load("tidyverse", "magrittr", "plyr", "dplyr", "nnet", "MASS", 
       "car", "splines", "psych", "rms")

#---- function ----
source(here::here("code", "functions.R"))

#---- Load the data ----
load(here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))

#---- Preparing for Crosswalk ----
bordeaux_formodel <- bordeaux_cleaned %>%
  dplyr::mutate(across(c("srmemory_share", "srhealth_share"), 
                       ~ as.factor(case_when(. %in% c(1, 2) ~ 1,
                                             . == 3 ~ 2,
                                             . %in% c(4, 5) ~ 3))),
                age_group = as.factor(age_group)) %>%
  dplyr::rename(srmemory_share_3cat = srmemory_share,
                srhealth_share_3cat = srhealth_share) %>%
  cbind(., bordeaux_cleaned %>% dplyr::select(srmemory_share, srhealth_share))

bordeaux_formodel %<>% mutate(srmemory_share_5cat = as.factor(srmemory_share),
                              srhealth_share_5cat = as.factor(srhealth_share))

# Sanity check
bordeaux_formodel %>%
  dplyr::select(contains(c("srmemory_share", "srhealth_share"))) %>%
  map_dfr(class) %>% t()
# 
# save(bordeaux_cleaned, bordeaux_formodel, 
#      file = here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))

#---- Models ----
measures <- c("memory", "health")
# For knots
knots_health <- 
  as.numeric(quantile(bordeaux_formodel$srhealth_memento, c(0.1, 0.5, 0.9)))
knots_memory <- 
  as.numeric(quantile(bordeaux_formodel$srmemory_memento, c(0.1, 0.5, 0.9)))

boundary_health <- 
  as.numeric(quantile(bordeaux_formodel$srhealth_memento, c(0.05, 0.95)))
boundary_memory <- 
  as.numeric(quantile(bordeaux_formodel$srmemory_memento, c(0.05, 0.95)))

models_func <- function(m){
  # m = "health"
  knots_3 <- case_when(m == "memory" ~ knots_memory,
                       m == "health" ~ knots_health)
  boundary_knots <- case_when(m == "memory" ~ boundary_memory,
                              m == "health" ~ boundary_health)
  predictor <- c(
    paste0("sr", m, "_memento"), 
    paste0("ns(sr", m, "_memento, knots = knots_3, Boundary.knots = boundary_knots)"))
  cont_var <- paste0("sr", m, "_share")
  # cat_var <- paste0("sr", m, "_share_5cat")
  cat_var <- paste0("sr", m, "_share_3cat")
  covariates <- c("female", "age_group")
  model_formulas <- c(
    # Multinomial models & Ordinal models
    rep(c(paste0(cat_var, " ~ ", predictor),
          paste0(cat_var, " ~ ", predictor, " + ", paste(covariates, collapse = " + ")),
          paste0(cat_var, " ~ ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2]),
          paste0(cat_var, " ~ ", predictor, "*rank"),
          paste0(cat_var, " ~ ", predictor, "*rank",  " + ", paste(covariates, collapse = " + ")),
          paste0(cat_var, " ~ ", predictor, "*rank",  " + ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2])), 2))
  # print(model_formulas)
  names(model_formulas) <- c(paste0(
    rep(rep(c("", "int_"), each = 6), 2),
    rep(c("multi_", "ordinal_"), each = 12),
    rep(c(m, paste0(paste0(m, "_splines_3knots"))), 12),
    rep(c("", "", "_agesex", "_agesex", "_agesexint", "_agesexint"), 4)))
  # View(model_formulas)
  
  crosswalk_models <- vector(mode = "list", length = length(model_formulas))
  for (i in 1:length(model_formulas)){
    m <- names(model_formulas)[i]
    tryCatch({
      # multinomial
      if (str_detect(m, "multi")){
        crosswalk_models[[i]] <-
          nnet::multinom(as.formula(model_formulas[[i]]),
                         data = bordeaux_formodel)
        # Ordinal
      } else if (str_detect(m, "ordinal")){
        crosswalk_models[[i]] <-
          MASS::polr(as.formula(model_formulas[[i]]),
                     data = bordeaux_formodel, method = "logistic")
      }
    }, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
  }
  
  names(crosswalk_models) <- names(model_formulas)
  
  return(crosswalk_models)
}

crosswalk_models_memory <- models_func("memory")
crosswalk_models_health <- models_func("health")

#---- **predicted values ----
# Predicted categorical memory reports
pred_mat <- bordeaux_formodel %>% 
  dplyr::select(srmemory_memento, srhealth_memento)

for (i in 1:length(crosswalk_models_memory)){
  tryCatch({pred_mat[, names(crosswalk_models_memory)[[i]]] <- 
    predict(crosswalk_models_memory[[i]])}, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
}

for (i in 1:length(crosswalk_models_health)){
  tryCatch({pred_mat[, names(crosswalk_models_health)[[i]]] <- 
    predict(crosswalk_models_health[[i]])}, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
}

summary(pred_mat)

# # Sanity check
# table(bordeaux_formodel$srmemory_share,
#       pred_mat$srmemory_linear)

pred_mat %<>% 
  dplyr::mutate_all(as.numeric)

#---- KAPPA statistics ----
variables <- pred_mat %>% 
  dplyr::select(-srmemory_memento, -srhealth_memento) %>%
  colnames() %>% sort()
variables

kappa_tbl <- tibble()
for (v in variables){
  tryCatch({if (str_detect(v, "memory")){
    temp_table <- table(bordeaux_formodel$srmemory_share_3cat,
                        # temp_table <- table(bordeaux_formodel$srmemory_share_5cat,
                        pred_mat[, v], useNA = "ifany")
  } else if (str_detect(v, "health")){
    temp_table <- table(bordeaux_formodel$srhealth_share_3cat,
                        # temp_table <- table(bordeaux_formodel$srhealth_share_5cat,
                        pred_mat[, v], useNA = "ifany")
  }
    wtd_kappa <- cohen.kappa(temp_table, alpha = .05)
    kappa_tbl[1, paste0(v, "_kappa_pe")] <- wtd_kappa$weighted.kappa
    kappa_tbl[1, paste0(v, "_kappa_2.5th")] <- wtd_kappa$confid["weighted kappa", "lower"]
    kappa_tbl[1, paste0(v, "_kappa_97.5th")] <- wtd_kappa$confid["weighted kappa", "upper"]
  }, error=function(e){cat("ERROR for variable ", v, ": ",
                           conditionMessage(e), "\n")})
}

kappa_tbl %>% dplyr::select(contains("memory")) %>% t()
kappa_tbl %>% dplyr::select(contains("memory") & contains("agesex")) %>% t()
kappa_tbl %>% dplyr::select(ends_with("_pe") & contains("memory")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

kappa_tbl %>% dplyr::select(contains("health")) %>% t()
kappa_tbl %>% dplyr::select(ends_with("_pe") & contains("health")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

# save(kappa_tbl, file = here::here("data", "analysis_data", "Calishare",
#                                   "wtd_kappa_04232024.RData"))

#---- Final model ----
# Memory: multinomial with splines, interaction term with order, age, sex
crosswalk_models_memory[["int_multi_memory_splines_3knots_agesexint"]]

# Health: multinomial with splines, interaction term with age and sex
crosswalk_models_health$multi_health_splines_3knots_agesexint
# For some reason summary() is not working
# Obtain the model object again
knots_health <- 
  as.numeric(quantile(bordeaux_formodel$srhealth_memento, c(0.1, 0.5, 0.9)))
boundary_health <- 
  as.numeric(quantile(bordeaux_formodel$srhealth_memento, c(0.05, 0.95)))
model_final_health <-  
  nnet::multinom(srhealth_share_3cat ~ ns(srhealth_memento, 
                                          knots = knots_health, 
                                          Boundary.knots = boundary_health)*female + 
                          ns(srhealth_memento, knots = knots_health, 
                             Boundary.knots = boundary_health)*age_group,
                        data = bordeaux_formodel)
summary(model_final_health)
# The coefficient are the same with the function results

# Save the model
crosswalk_model <- list(
  "int_multi_memory_splines_3knots_agesexint" =
    crosswalk_models_memory$int_multi_memory_splines_3knots_agesexint,
  "multi_health_splines_3knots_agesexint" = 
    model_final_health)
save(crosswalk_model, file = here::here("data", "model_results", "crosswalk",
                                        "crosswalk_model_300_04232024.RData"))

#---- **linear weighted kappa ----
p_load("irr")
memory_mat <- pred_mat %>% cbind(bordeaux_formodel) %>% 
  dplyr::select(srmemory_share_3cat, 
                int_multi_memory_splines_3knots_agesexint)
kappa2(memory_mat, "squared")
kappa2(memory_mat, "equal")

health_mat <- pred_mat %>% cbind(bordeaux_formodel) %>% 
  dplyr::select(srhealth_share_3cat, 
                multi_health_splines_3knots_agesexint)
kappa2(health_mat, "squared")
kappa2(health_mat, "equal")

#---- Equiprecentile: First response ----
p_load("equate")
# health: 1-excellent, 5-poor;0 (worst)-10(best), need to be reversed
# memory lower number: better, higher number worse
bordeaux_formodel %<>%
  mutate(srhealth_share_3cat_rev = case_when(srhealth_share_3cat == 3 ~ 1,
                                             srhealth_share_3cat == 2 ~ 2,
                                             srhealth_share_3cat == 1 ~ 3),
         srhealth_share_5cat_rev = case_when(srhealth_share == 5 ~ 1,
                                             srhealth_share == 4 ~ 2,
                                             srhealth_share == 3 ~ 3,
                                             srhealth_share == 2 ~ 4,
                                             srhealth_share == 1 ~ 5))
bordeaux_1st_memento <- bordeaux_formodel %>%
  filter(rank == "memento_share")
bordeaux_1st_share <- bordeaux_formodel %>%
  filter(rank == "share_memento")

# health
memento_srhealth_freq <- freqtab(bordeaux_1st_memento$srhealth_memento, design = "ng")
share_srhealth_freq_3cat <- freqtab(bordeaux_1st_share$srhealth_share_3cat_rev, design = "ng")
share_srhealth_freq_5cat <- freqtab(bordeaux_1st_share$srhealth_share_5cat_rev, design = "ng")

eqhealth_linear_3cat <- equate(memento_srhealth_freq, share_srhealth_freq_3cat, type = "l")
eqhealth_mean_3cat <- equate(memento_srhealth_freq, share_srhealth_freq_3cat, type = "mean")
eqhealth_eqp_3cat <- equate(memento_srhealth_freq, share_srhealth_freq_3cat, type = "equipercentile")
eqhealth_eqp_5cat <- equate(memento_srhealth_freq, share_srhealth_freq_5cat, type = "equipercentile")

plot(eqhealth_eqp_3cat, eqhealth_mean_3cat, xlab = "srhealth")
plot(eqhealth_linear_3cat, eqhealth_eqp_3cat, eqhealth_mean_3cat, xlab = "srhealth")

eqhealth_eqp_3cat$concordance
eqhealth_eqp_5cat$concordance
# checked the frequency of rounded eqauted3cat distribution with SHARE 3cat distribution
# It matches.

# memory
memento_srmemory_freq <- freqtab(bordeaux_1st_memento$srmemory_memento, design = "ng")
share_srmemory_freq_3cat <- freqtab(bordeaux_1st_share$srmemory_share_3cat, design = "ng")
share_srmemory_freq_5cat <- freqtab(bordeaux_1st_share$srmemory_share_5cat, design = "ng")

eqmemory_linear_3cat <- equate(memento_srmemory_freq, share_srmemory_freq_3cat, type = "l")
eqmemory_mean_3cat <- equate(memento_srmemory_freq, share_srmemory_freq_3cat, type = "mean")
eqmemory_eqp_3cat <- equate(memento_srmemory_freq, share_srmemory_freq_3cat, type = "equipercentile")
eqmemory_eqp_5cat <- equate(memento_srmemory_freq, share_srmemory_freq_5cat, type = "equipercentile")

plot(eqmemory_linear_3cat, eqmemory_eqp_3cat, eqmemory_mean_3cat, xlab = "srmemory")
eqmemory_eqp_3cat$concordance
eqmemory_eqp_5cat$concordance
# checked the frequency of rounded eqauted3cat distribution with SHARE 3cat distribution
# It matches.

pred_mat <- bordeaux_formodel %>% 
  dplyr::select(srmemory_memento, srhealth_memento, 
                srhealth_share_3cat_rev, srmemory_share_3cat,
                srhealth_share_5cat_rev, srmemory_share_5cat) %>%
  left_join(eqhealth_eqp_3cat$concordance %>% as_tibble() %>% 
              dplyr::rename("srhealth_eqt_3cat" = "yx"),
            by = c("srhealth_memento" = "scale")) %>%
  left_join(eqhealth_eqp_5cat$concordance %>% as_tibble() %>% 
              dplyr::rename("srhealth_eqt_5cat" = "yx"),
            by = c("srhealth_memento" = "scale")) %>%
  left_join(eqmemory_eqp_3cat$concordance %>% as_tibble() %>% 
              dplyr::rename("srmemory_eqt_3cat" = "yx"),
            by = c("srmemory_memento" = "scale")) %>%
  left_join(eqmemory_eqp_5cat$concordance %>% as_tibble() %>% 
              dplyr::rename("srmemory_eqt_5cat" = "yx"),
            by = c("srmemory_memento" = "scale")) %>%
  dplyr::mutate(across(contains("eqt"),
                       round, .names = "{.col}_cleaned"))

# kappa
# The first response may not cover the full range.
# Memory
memory_table_3cat <- with(pred_mat, 
                     table(srmemory_share_3cat, srmemory_eqt_3cat_cleaned))
cohen.kappa(memory_table_3cat, alpha = .05)

# for some reason, cohen.kappa function did not work for the 5 category memory
p_load("irr")
memory_mat <- pred_mat %>% 
  dplyr::select(srmemory_share_5cat, srmemory_eqt_5cat_cleaned)
kappa2(memory_mat, "squared") 
kappa2(memory_mat, "equal") 

# Health
health_table_3cat <- with(pred_mat, 
                     table(srhealth_share_3cat_rev, srhealth_eqt_3cat_cleaned))
cohen.kappa(health_table_3cat, alpha = .05)

health_table_5cat <- with(pred_mat, 
                          table(srhealth_share_5cat_rev, srhealth_eqt_5cat_cleaned))
cohen.kappa(health_table_5cat, alpha = .05)$weighted.kappa
cohen.kappa(health_table_5cat, alpha = .05)$confid["weighted kappa", "lower"]
cohen.kappa(health_table_5cat, alpha = .05)$confid["weighted kappa", "upper"]

health_mat <- pred_mat %>% 
  dplyr::select(srhealth_share_5cat_rev, srhealth_eqt_5cat_cleaned)
kappa2(health_mat, "squared") 
kappa2(health_mat, "equal") 

#---- OLD ----
# #---- **kappa stratify by age, sex and question order(final model) ----
# str_kappa_mat <- pred_mat %>% 
#   dplyr::select(
#     # "srmemory_multi_int", 
#     "srmemory_multi_int_restricted_cubic_spline_3knots_agesexint",
#     "srhealth_ordinal_restricted_cubic_spline_3knots_agesex") %>%
#   cbind(bordeaux_formodel) %>%
#   mutate(age_group_70plus = case_when(age_group == "2" ~ 0,
#                                       age_group %in% c("3", "4") ~ 1),
#          srmemory_memento_tertile = ntile(srmemory_memento, 3),
#          srhealth_memento_tertile = ntile(srhealth_memento, 3))
# # table(str_kappa_mat$srhealth_memento_tertile)
# # table(str_kappa_mat$srmemory_memento_tertile)
# covariates_health <- c("age_group_70plus", "female", "rank", "srhealth_memento_tertile")
# covariates_mem <- c("age_group_70plus", "female", "rank", "srmemory_memento_tertile")
# 
# kappa_tbl_str <- tibble()
# table_str_memory <- tibble()
# for (c in covariates_mem){
#   for (l in sort(unique(str_kappa_mat[, c]))){
#     temp_table <- 
#       with(str_kappa_mat %>% filter(get(c) == l), 
#            table(srmemory_share_3cat, 
#                  srmemory_multi_int_restricted_cubic_spline_3knots_agesexint, 
#                  useNA = "ifany"))
#     table_str_memory %<>% rbind(temp_table %>% as_tibble() %>%
#                                   mutate(covariates = c, level = l))
#     wtd.kappa <- cohen.kappa(temp_table, alpha = .05)
#     kappa_tbl_str[1, paste0("srmemory_kappa_pe_", c, l)] <- wtd.kappa$weighted.kappa
#     kappa_tbl_str[1, paste0("srmemory_kappa_2.5th_", c, l)] <- 
#       wtd.kappa$confid["weighted kappa", "lower"]
#     kappa_tbl_str[1, paste0("srmemory_kappa_97.5th_", c, l)] <- 
#       wtd.kappa$confid["weighted kappa", "upper"]
#   }
# }
# table_str_health <- tibble()
# for (c in covariates_health){
#   for (l in sort(unique(str_kappa_mat[, c]))){
#     tryCatch({
#       temp_table <- with(str_kappa_mat %>% filter(get(c) == l),
#                          table(srhealth_share_3cat, 
#                                srhealth_ordinal_restricted_cubic_spline_3knots_agesex, 
#                                useNA = "ifany"))
#       table_str_health %<>% rbind(temp_table %>% as_tibble() %>%
#                                     mutate(covariates = c, level = l))
#       wtd.kappa <- cohen.kappa(temp_table, alpha = .05)
#       kappa_tbl_str[1, paste0("srhealth_kappa_pe_", c, l)] <- 
#         wtd.kappa$weighted.kappa
#       kappa_tbl_str[1, paste0("srhealth_kappa_2.5th_", c, l)] <- 
#         wtd.kappa$confid["weighted kappa", "lower"]
#       kappa_tbl_str[1, paste0("srhealth_kappa_97.5th_", c, l)] <- 
#         wtd.kappa$confid["weighted kappa", "upper"]
#     }, error = function(e){
#       cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
#   }
# }
# 
# kappa_tbl_str %>% dplyr::select(contains("memory")) %>% t()
# kappa_tbl_str %>% dplyr::select(contains("health")) %>% t()

# save(kappa_tbl_str, file = here::here("data", "analysis_data", "Calishare",
#                                       "wtd_kappa_str_01312024.RData"))

# #---- **proportion of correctly classified individuals ----
# select <- dplyr::select
# table_correct_memory <- table_str_memory %>%
#   mutate(correct_flag = case_when(
#     srmemory_share_3cat == 
#       srmemory_multi_int_restricted_cubic_spline_3knots_agesexint ~ 1,
#     TRUE ~ 0),
#     covariates.level = paste0(covariates, ".", level)) %>%
#   dplyr::group_by(covariates.level, correct_flag) %>%
#   dplyr::summarise(n_memory = sum(n)) %>%
#   group_by(covariates.level) %>%
#   dplyr::mutate(prop_memory = n_memory/sum(n_memory)) %>%
#   filter(correct_flag == 1) %>%
#   select(-correct_flag) %>%
#   ungroup()
# 
# table_correct_health <- table_str_health %>%
#   mutate(correct_flag = case_when(
#     srhealth_share_3cat == 
#       srhealth_ordinal_restricted_cubic_spline_3knots_agesex ~ 1,
#     TRUE ~ 0),
#     covariates.level = paste0(covariates, ".", level)) %>%
#   dplyr::group_by(covariates.level, correct_flag) %>%
#   dplyr::summarise(n_health = sum(n)) %>%
#   group_by(covariates.level) %>%
#   dplyr::mutate(prop_health = n_health/sum(n_health)) %>%
#   filter(correct_flag == 1) %>%
#   select(-correct_flag) %>%
#   ungroup()
# 
# table_correct <- table_correct_memory %>%
#   full_join(table_correct_health, by = "covariates.level")
# 
# # saveRDS(table_correct, here::here("data", "analysis_data", "Calishare",
# #                                   "correct_n_str_12202023.RDS"))

# #---- PREVIOUS ATTEMPT ----
# #---- Eye balling kappa ----
# colnames(bordeaux_formodel)
# bordeaux_formodel %<>%
#   mutate(
#     srhealth_memento_3cat = case_when(
#       srhealth_memento >= 80 ~ 1,
#       srhealth_memento < 80 & srhealth_memento >= 50 ~ 2,
#       srhealth_memento <50 ~ 3),
#     srmemory_memento_3cat = case_when(
#       srmemory_memento < 2 ~ 1,
#       srmemory_memento < 5 & srmemory_memento >= 2 ~ 2,
#       srmemory_memento >= 5 ~ 3
#     ))
# 
# table_health <- with(bordeaux_formodel, table(srhealth_share_3cat,
#                                               srhealth_memento_3cat))
# cohen.kappa(table_health, alpha = .05)$weighted.kappa
# table_memory <- with(bordeaux_formodel, table(srmemory_share_3cat,
#                                               srmemory_memento_3cat))
# cohen.kappa(table_memory, alpha = .05)$weighted.kappa
# 
# #---- Logistic models ----
# bordeaux_formodel %<>%
#   # Fair/poor Yes or no!
#   mutate(srhealth_share_2cat = case_when(srhealth_share %in% c(4, 5) ~ 1,
#                                          srhealth_share %in% c(1, 2, 3) ~ 0),
#          srmemory_share_2cat = case_when(srmemory_share %in% c(4, 5) ~ 1,
#                                          srmemory_share %in% c(1, 2, 3) ~ 0))
# measures = c("health", "memory")
# for (m in measures){
#   knots_3 <- case_when(m == "memory" ~ knots_memory,
#                        m == "health" ~ knots_health)
#   boundary_knots <- case_when(m == "memory" ~ boundary_memory,
#                               m == "health" ~ boundary_health)
#   predictor <- c(paste0("sr", m, "_memento"), 
#                  paste0("ns(sr", m, "_memento)"),
#                  paste0("ns(sr", m, "_memento, knots = knots_3, 
#                         Boundary.knots = boundary_knots)"))
#   cat_var <- paste0("sr", m, "_share_2cat")
#   model_formulas <- c(
#     # logistic models
#     paste0(cat_var, " ~ ", predictor),
#     # Interaction with Rank
#     paste0(cat_var, " ~ ", predictor, "*rank"))
#   
#   crosswalk_models <- vector(mode = "list", length = length(model_formulas))
#   for (i in 1:length(model_formulas)){
#     crosswalk_models[[i]] <- glm(as.formula(model_formulas[i]),
#                                  data = bordeaux_formodel,
#                                  family = binomial(link = "logit"))
#   }
#   
#   names(crosswalk_models) <- 
#     paste0("logistic_", m, c("", "_ns", "_ns_3knots"),
#            c(rep("", 3), rep("_int", 3)))
#   
#   assign(paste0("logistic_", m), crosswalk_models)
# }
# 
# # Predict values
# 
# for (i in 1:length(logistic_memory)){
#   pred_mat[, names(logistic_memory)[[i]]] <- 
#     predict(logistic_memory[[i]], type = "response")
# }
# 
# for (i in 1:length(logistic_health)){
#   pred_mat[, names(logistic_health)[[i]]] <- 
#     predict(logistic_health[[i]], type = "response")
# }
# 
# set.seed("62283")
# pred_mat_logistic <- pred_mat %>%
#   dplyr::select(contains("logistic")) %>%
#   mutate_all(~rbinom(nrow(bordeaux_formodel), 1, prob = .))
# summary(pred_mat_logistic)
# 
# variables <- colnames(pred_mat_logistic)
# variables
# 
# kappa_tbl_logistic <- tibble()
# for (v in variables){
#   if (str_detect(v, "memory")){
#     temp_table <- table(bordeaux_formodel$srmemory_share_2cat,
#                         pred_mat_logistic[, v], useNA = "ifany")
#   } else if (str_detect(v, "health")){
#     temp_table <- table(bordeaux_formodel$srhealth_share_2cat,
#                         pred_mat_logistic[, v], useNA = "ifany")
#   }
#   kappa_tbl_logistic[1, paste0(v, "_kappa")] <- 
#     cohen.kappa(temp_table, alpha = .05)$weighted.kappa
# }
# kappa_tbl_logistic %>% t()
# 
# #---- Dichotomize the predicted 3 category value ----
# pred_mat_dichotomize <- pred_mat %>%
#   dplyr::select(ends_with("cat") & contains("linear"), 
#                 contains(c("multi", "ordinal"))) %>%
#   mutate_all(~case_when(. == 3 ~ 1, . %in% c(1, 2) ~ 0))
# variables <- colnames(pred_mat_dichotomize) %>% sort()
# variables
# 
# kappa_tbl_dichotomize <- tibble()
# for (v in variables){
#   if (str_detect(v, "memory")){
#     temp_table <- table(bordeaux_formodel$srmemory_share_2cat,
#                         pred_mat_dichotomize[, v], useNA = "ifany")
#   } else if (str_detect(v, "health")){
#     temp_table <- table(bordeaux_formodel$srhealth_share_2cat,
#                         pred_mat_dichotomize[, v], useNA = "ifany")
#   }
#   kappa_tbl_dichotomize[1, paste0(v, "_kappa")] <- 
#     cohen.kappa(temp_table, alpha = .05)$weighted.kappa
# }
# 
# kappa_tbl_dichotomize %>% dplyr::select(contains("memory")) %>% t()
# kappa_tbl_dichotomize %>% dplyr::select(contains("health")) %>% t()


# #---- SHARE and MEMENTO distribution ----
# load(here::here("data", "analysis_data", "memento_imp_stacked_40.Rdata"))
# load(here::here("data", "analysis_data", "share_imp_stacked_40_May102023.Rdata"))
# 
# # Use select from dplyr package
# select <- dplyr::select
# 
# ggpubr::ggarrange(
#   bordeaux_cleaned %>%
#     ggplot() +
#     geom_histogram(aes(x = srhealth_memento, y = stat(density))) +
#     scale_y_continuous(limits = c(0, 0.075), breaks = seq(0, 0.08, by = 0.01)) +
#     theme_bw() +
#     labs(x = "srhealth (bordeaux MEMENTO)"),
#   memento_imp_pmm_stacked %>%
#     ggplot() +
#     geom_histogram(aes(x = srhealth, y = stat(density))) +
#     scale_y_continuous(breaks = seq(0, 0.08, by = 0.01)) +
#     theme_bw() +
#     labs(x = "srhealth (MEMENTO, n_imp = 40)"))
# 
# proc_freq(share_imp_pmm_stacked, srhealth) %>%
#   mutate(n = n/40,
#          share_imp_40 = paste0(n, "(", round(`prop(%)`, 2), "%)")) %>% 
#   select(srhealth, share_imp_40) %>%
#   cbind(proc_freq(bordeaux_cleaned, srhealth_share) %>%
#           mutate(share_bordeaux = paste0(n, "(", round(`prop(%)`, 2), "%)")) %>%
#           select(share_bordeaux)) %>%
#   select(srhealth, share_bordeaux, share_imp_40)
# 
# ggpubr::ggarrange(
#   bordeaux_cleaned %>%
#     ggplot() +
#     geom_histogram(aes(x = srmemory_memento, y = stat(density))) +
#     scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
#     theme_bw() +
#     labs(x = "srmemory (bordeaux MEMENTO)"),
#   memento_imp_pmm_stacked %>%
#     ggplot() +
#     geom_histogram(aes(x = cog_complaints, y = stat(density))) +
#     scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
#     theme_bw() +
#     labs(x = "srmemory (MEMENTO, n_imp = 40)"))
# 
# proc_freq(share_imp_pmm_stacked, cog_complaints) %>%
#   mutate(n = n/40,
#          share_imp_40 = paste0(n, "(", round(`prop(%)`, 2), "%)")) %>% 
#   dplyr::rename(srmemory = cog_complaints) %>%
#   select(srmemory, share_imp_40) %>%
#   cbind(proc_freq(bordeaux_cleaned, srmemory_share) %>%
#           mutate(share_bordeaux = paste0(n, "(", round(`prop(%)`, 2), "%)")) %>%
#           select(share_bordeaux)) %>%
#   select(srmemory, share_bordeaux, share_imp_40)

# #---- Harmonized srhealth and srmemory ----
# memento_imp_pmm_stacked %<>%
#   mutate(srmemory_3cat = predict(crosswalk_model$int_multi_memory, 
#                                  newdata = memento_imp_pmm_stacked %>%
#                                    dplyr::rename("srmemory_memento" = "cog_complaints") %>%
#                                    mutate(rank = "memento_share")),
#          srhealth_3cat = predict(crosswalk_model$ordinal_health, 
#                                  newdata = memento_imp_pmm_stacked %>%
#                                    dplyr::rename("srhealth_memento" = "srhealth") %>%
#                                    mutate(rank = "memento_share")))
# # Sanity check
# with(memento_imp_pmm_stacked, table(srmemory_3cat, cog_complaints, useNA = "ifany"))
# with(memento_imp_pmm_stacked, table(srhealth_3cat, srhealth, useNA = "ifany"))
# 
# memento_imp_pmm_stacked %>%
#   ggplot() +
#   geom_histogram(aes(x = srhealth, fill = srhealth_3cat)) +
#   scale_fill_discrete(name = "Categorical",
#                       labels = c("1" = "Excellent/Very good",
#                                  "2" = "Good",
#                                  "3" = "Fair/Poor")) +
#   theme_bw() +
#   labs(x = "Self-reported health")
# 
# 
# 
# memento_imp_pmm_stacked %>%
#   ggplot() +
#   geom_histogram(aes(x = cog_complaints, fill = srmemory_3cat), binwidth = 1) +
#   scale_fill_discrete(name = "Categorical",
#                       labels = c("1" = "Excellent/Very good",
#                                  "2" = "Good",
#                                  "3" = "Fair/Poor")) +
#   scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
#   theme_bw() +
#   labs(x = "Self-reported memory concerns")
# 
# with(memento_imp_pmm_stacked, table(srmemory_3cat, useNA = "ifany"))/40
# with(memento_imp_pmm_stacked, table(srhealth_3cat, useNA = "ifany"))/40
#---- Copula ----
# p_load("copulareg")
# bordeaux_formodel %<>%
#   mutate(rank_n = case_when(rank == "memento_share" ~ 1,
#                             rank == "share_memento" ~ -1),
#          srmemory_memento_int = rank_n * srmemory_memento)
# copula_memory <- copulareg(as.numeric(bordeaux_formodel$srmemory_share),
#                            as.matrix(tibble(bordeaux_formodel$srmemory_memento,
#                                             bordeaux_formodel$rank_n,
#                                             bordeaux_formodel$srmemory_memento_int)),
#                            # bordeaux_formodel$age_group)),
#                            var_type_y = "d", # Discrete
#                            var_type_x = c("c", "d", "c") # Continuous
# )
# test <- bordeaux_formodel %>%
#   dplyr::select(srmemory_memento, rank_n, srmemory_memento_int) %>%
#   as.matrix()
# a <- predict.copulareg(copula_memory, new_x = test)
# 
# a_cat <- case_when(a < 3 ~ 1, a < 4 ~ 2, a >= 4 ~ 3)
# temp_table <- table(bordeaux_formodel$srhealth_share_3cat,
#                     a_cat, useNA = "ifany")
# 
# cohen.kappa(temp_table, alpha = .05)$weighted.kappa
# 
# table(a_cat)
# #---- Equipercentile attempt ----
# All data (irregardless of question order)
# p_load("equate")
# 
# bordeaux_cleaned %<>%
#   mutate(srhealth_share_rev = case_when(srhealth_share == 5 ~ 1,
#                                         srhealth_share == 4 ~ 2,
#                                         srhealth_share == 3 ~ 3,
#                                         srhealth_share == 2 ~ 4,
#                                         srhealth_share == 1 ~ 5))
# # health
# memento_srhealth_freq <- freqtab(bordeaux_cleaned$srhealth_memento, design = "ng")
# share_srhealth_freq <- freqtab(bordeaux_cleaned$srhealth_share_rev, design = "ng")
# 
# eqhealth_linear <- equate(memento_srhealth_freq, share_srhealth_freq, type = "l")
# eqhealth_mean <- equate(memento_srhealth_freq, share_srhealth_freq, type = "mean")
# eqhealth_eqp <- equate(memento_srhealth_freq, share_srhealth_freq, type = "equipercentile")
# 
# plot(eqhealth_eqp, eqhealth_mean, xlab = "srhealth")
# plot(eqhealth_linear, eqhealth_eqp, eqhealth_mean, xlab = "srhealth")
# 
# # memory
# memento_srmemory_freq <- freqtab(bordeaux_cleaned$srmemory_memento, design = "ng")
# share_srmemory_freq <- freqtab(bordeaux_cleaned$srmemory_share, design = "ng")
# 
# eqmemory_linear <- equate(memento_srmemory_freq, share_srmemory_freq, type = "l")
# eqmemory_mean <- equate(memento_srmemory_freq, share_srmemory_freq, type = "mean")
# eqmemory_eqp <- equate(memento_srmemory_freq, share_srmemory_freq, type = "equipercentile")
# 
# plot(eqmemory_linear, eqmemory_eqp, eqmemory_mean, xlab = "srmemory")
