# Crosswalk model Bordeaux survey data: self-reported memory and health: drop outliers
# Created by: Yingyan Wu
# Oct.9.2024

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}
p_load("readr", "tidyverse", "magrittr", "plyr", "dplyr","labelled",
       "nnet", "MASS")

#---- Load the data ----
load(here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))

#---- Preparing for Crosswalk ----
bordeaux_formodel <- bordeaux_cleaned %>%
  dplyr::mutate(age_group = as.factor(age_group),
                srmemory_share_5cat = as.factor(srmemory_share),
                srhealth_share_5cat = as.factor(srhealth_share),
                srmemory_share_3cat = as.factor(srmemory_share_3cat),
                srhealth_share_3cat = as.factor(srhealth_share_3cat))

# Sanity check
bordeaux_formodel %>%
  dplyr::select(contains(c("srmemory_share", "srhealth_share"))) %>%
  map_dfr(class) %>% t()

p_load("nnet", "MASS", "car", "splines", "psych")

# Bordeaux outliers number: 5 health 2 memory
bordeaux_formodel %>%
  filter(srhealth_outlier_crs == 1) %>% nrow()
bordeaux_formodel %>%
  filter(srmemory_outlier_crs == 1) %>% nrow()

bordeaux_formodel_health <- bordeaux_formodel %>%
  filter(srhealth_outlier_crs != 1)
bordeaux_formodel_memory <- bordeaux_formodel %>%
  filter(srmemory_outlier_crs != 1)

#---- Models ----
measures <- c("memory", "health")
# For knots
knots_health <- 
  as.numeric(quantile(bordeaux_formodel_health$srhealth_memento, c(0.1, 0.5, 0.9)))
knots_memory <- 
  as.numeric(quantile(bordeaux_formodel_memory$srmemory_memento, c(0.1, 0.5, 0.9)))

boundary_health <- 
  as.numeric(quantile(bordeaux_formodel_health$srhealth_memento, c(0.05, 0.95)))
boundary_memory <- 
  as.numeric(quantile(bordeaux_formodel_memory$srmemory_memento, c(0.05, 0.95)))

models_func <- function(m){
  knots_3 <- case_when(m == "memory" ~ knots_memory,
                       m == "health" ~ knots_health)
  boundary_knots <- case_when(m == "memory" ~ boundary_memory,
                              m == "health" ~ boundary_health)
  dataset_formodel <- case_when(m == "memory" ~ list(bordeaux_formodel_memory),
                                m == "health" ~ list(bordeaux_formodel_health))[[1]]
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
                         data = dataset_formodel)
        # Ordinal
      } else if (str_detect(m, "ordinal")){
        crosswalk_models[[i]] <-
          MASS::polr(as.formula(model_formulas[[i]]),
                     data = dataset_formodel, method = "logistic")
        # Linear
      } else if (str_detect(m, "linear")){
        crosswalk_models[[i]] <-
          lm(as.formula(model_formulas[[i]]), data = dataset_formodel)
      }
    }, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
  }
  
  names(crosswalk_models) <- names(model_formulas)
  
  return(crosswalk_models)
}

crosswalk_models_memory <- models_func("memory")
crosswalk_models_health <- models_func("health")

##---- predicted values ----
# Predicted categorical memory reports
# predicted categorical memory reports
pred_mat_health <- bordeaux_formodel_health %>%
  dplyr::select(srhealth_memento)
pred_mat_memory <- bordeaux_formodel_memory %>%
  dplyr::select(srmemory_memento)

for (i in 1:length(crosswalk_models_memory)){
  tryCatch({pred_mat_memory[, names(crosswalk_models_memory)[[i]]] <- 
    predict(crosswalk_models_memory[[i]])}, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
}

for (i in 1:length(crosswalk_models_health)){
  tryCatch({pred_mat_health[, names(crosswalk_models_health)[[i]]] <- 
    predict(crosswalk_models_health[[i]])}, error = function(e){
      cat("ERROR for model ",i,": ", conditionMessage(e), "\n")})
}

summary(pred_mat_health)
summary(pred_mat_memory)

# # Sanity check
# table(bordeaux_formodel$srmemory_share,
#       pred_mat$srmemory_linear)

pred_mat_health %<>%
  dplyr::mutate_all(as.numeric) %>%
  dplyr::select(-contains("linear"))

pred_mat_memory %<>%
  dplyr::mutate_all(as.numeric) %>%
  dplyr::select(-contains("linear"))

#---- KAPPA statistics ----
p_load("irr")
variables <- c(pred_mat_health %>% 
                 dplyr::select(-srhealth_memento) %>% colnames() %>% sort(),
               pred_mat_memory %>%
                 dplyr::select(-srmemory_memento) %>% colnames() %>% sort())
variables

kappa_tbl_drop_outliers <- tibble()
for (v in variables){
  tryCatch({if (str_detect(v, "memory")){
    temp_mat <- pred_mat_memory %>% cbind(bordeaux_formodel_memory) %>%
      dplyr::select(srmemory_share_3cat, all_of(v))
    temp_table <- table(temp_mat$srmemory_share_3cat,
                        temp_mat %>% pull(v), useNA = "ifany")
  } else if (str_detect(v, "health")){
    temp_mat <- pred_mat_health %>% cbind(bordeaux_formodel_health) %>%
      dplyr::select(srhealth_share_3cat, all_of(v))
    temp_table <- table(temp_mat$srhealth_share_3cat,
                        temp_mat %>% pull(v), useNA = "ifany")
  }
    wtd_kappa <- cohen.kappa(temp_table, alpha = .05)
    kappa_tbl_drop_outliers[1, paste0(v, "_kappa_pe")] <- wtd_kappa$weighted.kappa
    kappa_tbl_drop_outliers[1, paste0(v, "_kappa_2.5th")] <- wtd_kappa$confid["weighted kappa", "lower"]
    kappa_tbl_drop_outliers[1, paste0(v, "_kappa_97.5th")] <- wtd_kappa$confid["weighted kappa", "upper"]
  }, error=function(e){cat("ERROR for variable ", v, ": ",
                           conditionMessage(e), "\n")})
}

kappa_tbl_drop_outliers %>% dplyr::select(contains("memory")) %>% t()
kappa_tbl_drop_outliers %>% dplyr::select(contains("memory") & contains("agesex")) %>% t()
kappa_tbl_drop_outliers %>% dplyr::select(ends_with("_pe") & contains("memory")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

kappa_tbl_drop_outliers %>% dplyr::select(contains("health")) %>% t()
kappa_tbl_drop_outliers %>% dplyr::select(ends_with("_pe") & contains("health")) %>% t() %>%
  as.data.frame() %>% mutate(model = rownames(.)) %>% arrange(V1)

save(kappa_tbl_drop_outliers, file = here::here("data", "analysis_data", "Calishare",
                                  "wtd_kappa_3cat_drop_outliers.RData"))