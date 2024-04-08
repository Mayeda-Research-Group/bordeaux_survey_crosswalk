# Crosswalk model Bordeaux survey data: self-reported memory and health
# Created by: Yingyan Wu
# May.18.2023

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}
p_load("readr", "tidyverse", "magrittr", "plyr", "dplyr","labelled",
       "nnet", "MASS")

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

p_load("nnet", "MASS", "car", "splines", "psych", "rms")
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
  knots_3 <- case_when(m == "memory" ~ knots_memory,
                       m == "health" ~ knots_health)
  boundary_knots <- case_when(m == "memory" ~ boundary_memory,
                              m == "health" ~ boundary_health)
  predictor <- c(
    paste0("sr", m, "_memento"), 
    paste0("ns(sr", m, "_memento, knots = knots_3, Boundary.knots = boundary_knots)"))
  cont_var <- paste0("sr", m, "_share")
  cat_var <- paste0("sr", m, "_share_5cat")
  # cat_var <- paste0("sr", m, "_share_3cat")
  covariates <- c("female", "age_group")
  model_formulas <- c(
    # Multinomial models & Ordinal models
    rep(c(paste0(cat_var, " ~ ", predictor),
          paste0(cat_var, " ~ ", predictor, " + ", paste(covariates, collapse = " + ")),
          paste0(cat_var, " ~ ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2]),
          paste0(cat_var, " ~ ", predictor, "*rank"),
          paste0(cat_var, " ~ ", predictor, "*rank",  " + ", paste(covariates, collapse = " + ")),
          paste0(cat_var, " ~ ", predictor, "*rank",  " + ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2])), 2),
    # Linear regression
    paste0(cont_var, " ~ ", predictor),
    paste0(cont_var, " ~ ", predictor, " + ", paste(covariates, collapse = " + ")),
    paste0(cont_var, " ~ ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2]),
    paste0(cont_var, " ~ ", predictor, "*rank"),
    paste0(cont_var, " ~ ", predictor, "*rank", " + ", paste(covariates, collapse = " + ")),
    paste0(cont_var, " ~ ", predictor, "*rank", " + ", predictor, "*", covariates[1], " + ", predictor, "*", covariates[2]))
  # print(model_formulas)
  names(model_formulas) <- c(paste0(
    rep(rep(c("", "int_"), each = 6), 3),
    rep(c("multi_", "ordinal_", "linear_"), each = 12),
    rep(c(m, paste0(paste0(m, "_splines_3knots"))), 18),
    rep(c("", "", "_agesex", "_agesex", "_agesexint", "_agesexint"), 6)))
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
        # Linear
      } else if (str_detect(m, "linear")){
        crosswalk_models[[i]] <-
          lm(as.formula(model_formulas[[i]]), data = bordeaux_formodel)
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

pred_mat_linear <- pred_mat %>% dplyr::select(contains("linear"))
pred_mat_linear %<>% dplyr::mutate_all(~case_when(. < 2 ~ 1, 
                                                  . >= 2 & . < 3 ~ 2,
                                                  . >= 3 & . < 4 ~ 3,
                                                  . >= 4 & . < 5 ~ 4,
                                                  . >= 5 ~ 5))
pred_mat[, paste0(colnames(pred_mat_linear), "_cat")] <- pred_mat_linear

summary(pred_mat)

#---- KAPPA statistics ----
p_load("irr")

variables <- pred_mat %>% 
  dplyr::select(-srmemory_memento, -srhealth_memento,
                # Exclude linear prediction without recategorizing
                -all_of(colnames(pred_mat_linear))) %>%
  colnames() %>% sort()
variables

kappa_tbl_5cat <- tibble()
for (v in variables){
  tryCatch({if (str_detect(v, "memory")){
    temp_mat <- pred_mat %>% cbind(bordeaux_formodel) %>%
      dplyr::select(srmemory_share_5cat, all_of(v))
  } else if (str_detect(v, "health")){
    temp_mat <- pred_mat %>% cbind(bordeaux_formodel) %>%
      dplyr::select(srhealth_share_5cat, all_of(v))
  }
    linear_wtd_kappa <- irr::kappa2(temp_mat, "equal")
    quadratic_wtd_kappa <- irr::kappa2(temp_mat, "squared")
    kappa_tbl_5cat[1, paste0(v, "_kappa_linear")] <- linear_wtd_kappa$value
    kappa_tbl_5cat[1, paste0(v, "_kappa_quadratic")] <- quadratic_wtd_kappa$value
  }, error=function(e){cat("ERROR for variable ", v, ": ",
                           conditionMessage(e), "\n")})
}

kappa_tbl_5cat %>% dplyr::select(contains("memory")) %>% t()
kappa_tbl_5cat %>% dplyr::select(contains("memory")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

kappa_tbl_5cat %>% dplyr::select(contains("health")) %>% t()
kappa_tbl_5cat %>% dplyr::select(contains("health")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

save(kappa_tbl_5cat, file = here::here("data", "analysis_data", "Calishare",
                                  "wtd_kappa_5cat.RData"))

