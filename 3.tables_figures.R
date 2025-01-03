# Tables and figures
# Created by: Yingyan Wu
# Apr.21.2023

#---- Load the package ----
rm(list = ls())
if(!require("pacman")){
  install.packages("pacman")
}
p_load("tidyverse", "magrittr", "gtsummary", "dplyr","labelled", "cowplot")


#---- Table 1 ----
load(here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))

table1 <- bordeaux_cleaned %>%
  dplyr::select(age_group, female, rank, srhealth_memento, srhealth_share,
                srmemory_memento, srmemory_share) %>%
  set_variable_labels(
    female = "Women",
    age_group = "Age group",
    rank = "Question order",
    srhealth_memento = "Self-rated health (continuous, range 0-100, 
    higher scores represent better health)",
    srhealth_share = "Self-rated health (Likert)",
    srmemory_memento = "Self-rated memory (continuous, range 0-10, 
    higher scores represent more concerns)",
    srmemory_share = "Self-rated memory (Likert)") %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    age_group = c(
      # "< 60" = 1, "> 90" = 5,
      "60 - 69" = 2, 
      "70 - 79" = 3, 
      "80 - 89" = 4),
    rank = c("Continuous first" = "memento_share",
             "Likert first" = "share_memento"),
    srhealth_share = c("Excellent" = 1,
                       "Very good" = 2,
                       "Good" = 3,
                       "Fair" = 4,
                       "Poor" = 5),
    srmemory_share = c("Excellent" = 1,
                       "Very good" = 2,
                       "Good" = 3,
                       "Fair" = 4,
                       "Poor" = 5)) %>%
  modify_if(is.labelled, to_factor) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)",
                               all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})")),
              type = list(all_continuous() ~ "continuous2"),
              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 0)) %>%
  modify_header(label = "") %>%
  # modify_table_styling(columns = stat_0,
  #                      footnote = "Mean (SD) and meadian (Q1, Q3) for continuous variables; n(%) for categorical variables") %>%
  bold_labels()

table1$table_body <- table1$table_body %>%
  mutate(label = case_when(str_detect(label, "IQR") ~ "Median (Q1, Q3)",
                           TRUE ~ label))

table1 %>% as_flex_table() %>%
  flextable::save_as_docx(
    path = here::here("output", "bordeaux_crosswalk", "table1_bordeaux.docx"))

#---- Table 2 Cohen's kappa ----
load(here::here("data", "analysis_data", "Calishare", "wtd_kappa_04232024.RData"))
select <- dplyr::select
kappa_formatted_tib <- kappa_tbl %>% t() %>%
  as.data.frame() %>% dplyr::mutate(
    estimate = str_replace(rownames(.), "(.*)_", ""),
    Construct = case_when(str_detect(rownames(.), "memory") ~ "srmemory",
                          str_detect(rownames(.), "health") ~ "srhealth"),
    Model = str_replace(rownames(.), paste0("_kappa_", estimate), ""),
    Model = str_replace(Model, "memory_", ""),
    Model = str_replace(Model, "health_", ""),
    Model = str_replace(Model, "_cat", ""),
    # For ordering!
    Model = str_replace(Model, "splines_3knots", "splines"), 
    # Model = str_replace(Model, "int_splines", "splines_int"),
    Construct = case_when(Construct == "srhealth" ~ "Self-rated health",
                          Construct == "srmemory" ~ "Self-rated memory")) %>%
  set_rownames(NULL) %>% dplyr::rename("kappa" = "V1") %>%
  dplyr::mutate(model_label = case_when(
    str_detect(Model, "int_(.*)_splines_agesexint") ~ "M12: spline, interaction with order, age, sex",
    str_detect(Model, "_splines_agesexint") ~ "M10: spline and interaction with age, sex",
    str_detect(Model, "int_(.*)_agesexint") ~ "M11: interaction with order, age, sex",
    str_detect(Model, "_agesexint") ~ "M9: interaction with age, sex",
    str_detect(Model, "int_(.*)_splines_agesex") ~ "M8: spline and interaction with order, further adjusted for age, sex",
    str_detect(Model, "_splines_agesex") ~ "M6: spline and adjusted for age, sex",
    str_detect(Model, "int_(.*)_agesex") ~ "M7: interaction with order and adjusted for age, sex",
    str_detect(Model, "_agesex") ~ "M5: adjusted for age, sex",
    str_detect(Model, "int_(.*)_splines") ~ "M4: spline and interaction with order",
    str_detect(Model, "int_") ~ "M3: interaction with order",
    str_detect(Model, "_splines") ~ "M2: spline",
    TRUE ~ "M1"),
    regression_label = case_when(str_detect(Model, "linear") ~ "Linear",
                                 str_detect(Model, "multi") ~ "Multinomial",
                                 str_detect(Model, "ordinal") ~ "Ordinal"),
    model_index = parse_number(model_label)) %>%
  arrange(Construct, regression_label, model_index)

kappa_formatted_tib_wide <- kappa_formatted_tib  %>%
  dplyr::select(Construct, model_label, model_index, regression_label, estimate, kappa) %>%
  pivot_wider(names_from = c("Construct", "estimate"), 
              values_from = "kappa",
              names_glue = "{Construct} {.value} {estimate}") %>%
  mutate(across(contains("kappa"), function(x) sprintf("%.2f", x))) %>%
  mutate(`Self-rated health weighted kappa` = 
           paste0(`Self-rated health kappa pe`, " (", 
                  `Self-rated health kappa 2.5th`, ", ",
                  `Self-rated health kappa 97.5th`, ")"),
         `Self-rated memory weighted kappa` = 
           paste0(`Self-rated memory kappa pe`, " (", 
                  `Self-rated memory kappa 2.5th`, ", ",
                  `Self-rated memory kappa 97.5th`, ")")) %>%
  mutate(`Self-rated memory weighted kappa` = 
           na_if(`Self-rated memory weighted kappa`, "NA (NA, NA)"),
         `Self-rated health weighted kappa` = 
           na_if(`Self-rated health weighted kappa`, "NA (NA, NA)")) %>%
  select(regression_label, model_label, model_index, contains("weighted"))

kappa_table2 <- kappa_formatted_tib_wide %>%
  filter(regression_label != "Linear") %>%
  bind_rows(tibble(
    regression_label = "Ordinal",
    model_label = c("M10: spline and interaction with age, sex",
                    "M12: spline, interaction with order, age, sex"),
    model_index = c(10, 12),
    `Self-rated health weighted kappa` = NA,
    `Self-rated memory weighted kappa` = NA)) %>%
  mutate(across(ends_with("kappa"), function(x) case_when(is.na(x) ~ "DNC",
                                                          !is.na(x) ~ x))) %>%
  mutate(regression_label = case_when(
    regression_label == "Linear" ~ "Linear model for 3-category outcome",
    regression_label == "Multinomial" ~ "Multinomial model for 3-category outcome",
    regression_label == "Ordinal" ~ "Ordinal model for 3-category outcome")) %>%
  arrange(regression_label, model_index) %>%
  mutate(model_lab = paste0("M", model_index))
writexl::write_xlsx(kappa_table2,
                    path = here::here("output", "bordeaux_crosswalk",
                                      "table2_kappa_bordeaux_03032024.xlsx"))

#---- Table S1. model coefficients ----
load(here::here("data", "model_results", "crosswalk", 
                "crosswalk_model_300_04232024.RData"))
load(here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))
p_load("splines")
names(crosswalk_model)
memory_tidy <- 
  broom::tidy(crosswalk_model$int_multi_memory_splines_3knots_agesexint) %>%
  mutate(term = str_replace(term, ":", "*")) %>%
  mutate(
    y.level = case_when(y.level == 2 ~ "Good",
                        y.level == 3 ~ "Fair/poor"),
    label = str_replace_all(
      term, 
      c("rankshare_memento" = "Question order Likert first",
        "female" = "Female",
        "age_group3" = "Age 70-79",
        "age_group4" = "Age: 80-89",
        "(.*)boundary_knots\\)1" = "Spline term 1",
        "(.*)boundary_knots\\)2" = "Spline term 2",
        "(.*)boundary_knots\\)3" = "Spline term 3",
        "(.*)boundary_knots\\)4" = "Spline term 4"))) %>%
  select(y.level, label, estimate, std.error) %>%
  set_colnames(c("Outcome level", "Coefficient", "Beta", "SE")) %>%
  mutate_if(is.numeric, function(x) sprintf("%.2f", x))

health_tidy <- 
  broom::tidy(crosswalk_model$multi_health_splines_3knots_agesexint) %>%
  mutate(
    y.level = case_when(y.level == 2 ~ "Good",
                        y.level == 3 ~ "Fair/poor"),
    label = str_replace_all(
      term, 
      c("female" = "Female",
        "age_group3" = "Age 70-79",
        "age_group4" = "Age: 80-89",
        "(.*)boundary_health\\)1" = "Spline term 1",
        "(.*)boundary_health\\)2" = "Spline term 2",
        "(.*)boundary_health\\)3" = "Spline term 3",
        "(.*)boundary_health\\)4" = "Spline term 4"))) %>%
  select(y.level, label, estimate, std.error) %>%
  set_colnames(c("Outcome level", "Coefficient", "Beta", "SE")) %>%
  mutate_if(is.numeric, function(x) sprintf("%.2f", x))

writexl::write_xlsx(list("srhealth" = health_tidy, "srmemory" = memory_tidy),
                    path = here::here("output", "bordeaux_crosswalk",
                                      "tableS1_model_coefficients_04232024.xlsx"))

#---- Table S2. linear and quadratic kappa for 5 category ----
load(here::here("data", "analysis_data", "Calishare", "wtd_kappa_5cat.RData"))
select <- dplyr::select
kappa_5cat_formatted_tib <- kappa_tbl_5cat %>% t() %>%
  as.data.frame() %>% dplyr::mutate(
    estimate = str_replace(rownames(.), "(.*)_", ""),
    Construct = case_when(str_detect(rownames(.), "memory") ~ "srmemory",
                          str_detect(rownames(.), "health") ~ "srhealth"),
    Model = str_replace(rownames(.), paste0("_kappa_", estimate), ""),
    Model = str_replace(Model, "memory_", ""),
    Model = str_replace(Model, "health_", ""),
    Model = str_replace(Model, "_cat", ""),
    # For ordering!
    Model = str_replace(Model, "splines_3knots", "splines"), 
    # Model = str_replace(Model, "int_splines", "splines_int"),
    Construct = case_when(Construct == "srhealth" ~ "Self-rated health",
                          Construct == "srmemory" ~ "Self-rated memory")) %>%
  set_rownames(NULL) %>% dplyr::rename("kappa" = "V1") %>%
  dplyr::mutate(model_label = case_when(
    str_detect(Model, "int_(.*)_splines_agesexint") ~ "M12: spline, interaction with order, age, sex",
    str_detect(Model, "_splines_agesexint") ~ "M10: spline and interaction with age, sex",
    str_detect(Model, "int_(.*)_agesexint") ~ "M11: interaction with order, age, sex",
    str_detect(Model, "_agesexint") ~ "M9: interaction with age, sex",
    str_detect(Model, "int_(.*)_splines_agesex") ~ "M8: spline and interaction with order, further adjusted for age, sex",
    str_detect(Model, "_splines_agesex") ~ "M6: spline and adjusted for age, sex",
    str_detect(Model, "int_(.*)_agesex") ~ "M7: interaction with order and adjusted for age, sex",
    str_detect(Model, "_agesex") ~ "M5: adjusted for age, sex",
    str_detect(Model, "int_(.*)_splines") ~ "M4: spline and interaction with order",
    str_detect(Model, "int_") ~ "M3: interaction with order",
    str_detect(Model, "_splines") ~ "M2: spline",
    TRUE ~ "M1"),
    regression_label = case_when(str_detect(Model, "linear") ~ "Linear",
                                 str_detect(Model, "multi") ~ "Multinomial",
                                 str_detect(Model, "ordinal") ~ "Ordinal"),
    model_index = parse_number(model_label)) %>%
  arrange(Construct, regression_label, model_index)

kappa_5cat_formatted_tib_wide <- kappa_5cat_formatted_tib  %>%
  dplyr::select(Construct, model_label, model_index, regression_label, estimate, kappa) %>%
  pivot_wider(names_from = c("Construct", "estimate"), 
              values_from = "kappa",
              names_glue = "{Construct} {.value} {estimate}") %>%
  mutate(across(contains("kappa"), function(x) sprintf("%.2f", x)))

kappa_tableS2 <- kappa_5cat_formatted_tib_wide %>%
  filter(regression_label != "Linear") %>%
  bind_rows(tibble(
    regression_label = "Ordinal",
    model_index = c(10, 12),
    model_label = c("M10: spline and interaction with age, sex",
                    "M12: spline, interaction with order, age, sex"),
    `Self-rated health kappa linear` = NA,
    `Self-rated health kappa quadratic` = NA,
    `Self-rated memory kappa linear` = NA,
    `Self-rated memory kappa quadratic` = NA)) %>%
  # srpintf is not NA friendly, it convert NA to character "NA
  mutate(across(contains("kappa"), 
                function(x) case_when(is.na(x)| x == "NA" ~ "DNC",
                                      !is.na(x)| x != "NA" ~ x))) %>%
  mutate(regression_label = case_when(
    regression_label == "Linear" ~ "Linear model for 5-category outcome",
    regression_label == "Multinomial" ~ "Multinomial model for 5-category outcome",
    regression_label == "Ordinal" ~ "Ordinal model for 5-category outcome")) %>%
  arrange(regression_label, model_index) %>%
  mutate(model_lab = paste0("M", model_index))

writexl::write_xlsx(kappa_tableS2,
                    path = here::here("output", "bordeaux_crosswalk",
                                      "tableS2_kappa_5cat_bordeaux.xlsx"))

#---- Table S4. kappa for percentile crosswalk ----
# results from script 2.

#---- Table S3. kappa dropping outliers ----
load(here::here("data", "analysis_data", "Calishare", "wtd_kappa_3cat_drop_outliers.RData"))
select <- dplyr::select
kappa_3cat_dropout_fmt_tib <- kappa_tbl_drop_outliers %>% t() %>%
  as.data.frame() %>% dplyr::mutate(
    estimate = str_replace(rownames(.), "(.*)_", ""),
    Construct = case_when(str_detect(rownames(.), "memory") ~ "srmemory",
                          str_detect(rownames(.), "health") ~ "srhealth"),
    Model = str_replace(rownames(.), paste0("_kappa_", estimate), ""),
    Model = str_replace(Model, "memory_", ""),
    Model = str_replace(Model, "health_", ""),
    Model = str_replace(Model, "_cat", ""),
    # For ordering!
    Model = str_replace(Model, "splines_3knots", "splines"), 
    # Model = str_replace(Model, "int_splines", "splines_int"),
    Construct = case_when(Construct == "srhealth" ~ "Self-rated health",
                          Construct == "srmemory" ~ "Self-rated memory")) %>%
  set_rownames(NULL) %>% dplyr::rename("kappa" = "V1") %>%
  dplyr::mutate(model_label = case_when(
    str_detect(Model, "int_(.*)_splines_agesexint") ~ "M12: spline, interaction with order, age, sex",
    str_detect(Model, "_splines_agesexint") ~ "M10: spline and interaction with age, sex",
    str_detect(Model, "int_(.*)_agesexint") ~ "M11: interaction with order, age, sex",
    str_detect(Model, "_agesexint") ~ "M9: interaction with age, sex",
    str_detect(Model, "int_(.*)_splines_agesex") ~ "M8: spline and interaction with order, further adjusted for age, sex",
    str_detect(Model, "_splines_agesex") ~ "M6: spline and adjusted for age, sex",
    str_detect(Model, "int_(.*)_agesex") ~ "M7: interaction with order and adjusted for age, sex",
    str_detect(Model, "_agesex") ~ "M5: adjusted for age, sex",
    str_detect(Model, "int_(.*)_splines") ~ "M4: spline and interaction with order",
    str_detect(Model, "int_") ~ "M3: interaction with order",
    str_detect(Model, "_splines") ~ "M2: spline",
    TRUE ~ "M1"),
    regression_label = case_when(str_detect(Model, "linear") ~ "Linear",
                                 str_detect(Model, "multi") ~ "Multinomial",
                                 str_detect(Model, "ordinal") ~ "Ordinal"),
    model_index = parse_number(model_label)) %>%
  arrange(Construct, regression_label, model_index)

kappa_3cat_dropout_fmt_tib_wide <- kappa_3cat_dropout_fmt_tib  %>%
  dplyr::select(Construct, model_label, model_index, regression_label, estimate, kappa) %>%
  pivot_wider(names_from = c("Construct", "estimate"), 
              values_from = "kappa",
              names_glue = "{Construct} {.value} {estimate}") %>%
  mutate(across(contains("kappa"), function(x) sprintf("%.2f", x))) %>%
  mutate(`Self-rated health weighted kappa` = 
           paste0(`Self-rated health kappa pe`, " (", 
                  `Self-rated health kappa 2.5th`, ", ",
                  `Self-rated health kappa 97.5th`, ")"),
         `Self-rated memory weighted kappa` = 
           paste0(`Self-rated memory kappa pe`, " (", 
                  `Self-rated memory kappa 2.5th`, ", ",
                  `Self-rated memory kappa 97.5th`, ")")) %>%
  mutate(`Self-rated memory weighted kappa` = 
           na_if(`Self-rated memory weighted kappa`, "NA (NA, NA)"),
         `Self-rated health weighted kappa` = 
           na_if(`Self-rated health weighted kappa`, "NA (NA, NA)")) %>%
  select(regression_label, model_label, model_index, contains("weighted"))

kappa_tableS3 <- kappa_3cat_dropout_fmt_tib_wide %>%
  filter(regression_label != "Linear") %>%
  bind_rows(tibble(
    regression_label = "Ordinal",
    model_label = c("M10: spline and interaction with age, sex",
                    "M12: spline, interaction with order, age, sex"),
    model_index = c(10, 12),
    `Self-rated health weighted kappa` = NA,
    `Self-rated memory weighted kappa` = NA)) %>%
  mutate(across(ends_with("kappa"), function(x) case_when(is.na(x) ~ "DNC",
                                                          !is.na(x) ~ x))) %>%
  mutate(regression_label = case_when(
    regression_label == "Linear" ~ "Linear model for 3-category outcome",
    regression_label == "Multinomial" ~ "Multinomial model for 3-category outcome",
    regression_label == "Ordinal" ~ "Ordinal model for 3-category outcome")) %>%
  arrange(regression_label, model_index) %>%
  mutate(model_lab = paste0("M", model_index))

writexl::write_xlsx(kappa_tableS3,
                    path = here::here("output", "bordeaux_crosswalk",
                                      "tableS3_kappa_dropout_bordeaux.xlsx"))

#---- Figure 1. Dist ----
# Box plot 3 categories
bordeaux_formodel %>%
  dplyr::select(srhealth_memento, srmemory_memento,
         srhealth_share_3cat, srmemory_share_3cat) %>%
  dplyr::rename(srhealth_share = srhealth_share_3cat,
                srmemory_share =  srmemory_share_3cat) %>%
  pivot_longer(cols = contains(c("srhealth", "srmemory")), 
               names_to = c("measure", ".value"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(share = factor(share, levels = rev(1:5))) %>%
  ggplot() +
  geom_boxplot(aes(x = share, y = memento)) +
  geom_jitter(aes(x = share, y = memento), width = 0.1) +
  scale_y_continuous(n.breaks = 6) +
  scale_x_discrete(labels = c("1" = "Excellent/very good",
                              "2" = "Good",
                              "3" = "Fair/poor")) +
  facet_grid(rows = vars(measure), scales = "free_y",
             labeller = labeller(measure = c("srhealth" = "Self-rated health",
                                             "srmemory" = "Self-rated memory"))) +
  theme_bw() +
  labs(x = "Likert scale", y = "Continuous")

# ggsave(here::here("output", "bordeaux_crosswalk", "fig1_joint_dist_box_bordeaux_3cat.png"),
#        dpi = 300, width = 9, height = 7, units = "in")

#---- Fig S1. residual plot for final models ----
load(here::here("data", "model_results", "crosswalk", 
                "crosswalk_model_300_04232024.RData"))
load(here::here("data", "analysis_data", "Calishare", "bordeaux_cleaned.RData"))

select <- dplyr::select

# Memory
# Multinomial model, multiple sets of coefficients. 
mem_resid <- 
  residuals(crosswalk_model[["int_multi_memory_splines_3knots_agesexint"]]) %>%
  as_tibble() %>%
  set_colnames(paste0("resid_", names(.))) %>%
  mutate(mem_pred = 
           predict(crosswalk_model$int_multi_memory_splines_3knots_agesexint))

mem_resid_aug <- cbind(bordeaux_cleaned, mem_resid)
mem_resid_predictor <- mem_resid_aug %>%
  select(contains("resid_"), srmemory_memento, age_group, female, rank, mem_pred) %>%
  pivot_longer(contains("resid_"),
               names_to = "level",
               names_prefix = "resid_",
               values_to = "residual") %>%
  filter(mem_pred == level)

mem_resid_plot <- mem_resid_predictor %>%
  ggplot() +
  geom_point(aes(x = srmemory_memento, y = residual)) +
  # facet_grid(cols = vars(level),
  #            # rows = vars(rank), 
  #            labeller = labeller(level = c("1" = "Excellent/Very good",
  #                                          "2" = "Good",
  #                                          "3" = "Fair/Poor"))) +
  labs(x = "Continuous self-rated memory", y = "Residuals") +
  theme_bw()

# Health
health_resid <- 
  residuals(crosswalk_model$multi_health_splines_3knots_agesexint) %>%
  as_tibble() %>%
  set_colnames(paste0("resid_", names(.))) %>%
  mutate(health_pred = 
           predict(crosswalk_model$multi_health_splines_3knots_agesexint))

health_resid_aug <- cbind(bordeaux_cleaned, health_resid)
health_resid_predictor <- health_resid_aug %>%
  select(contains("resid_"), srhealth_memento, age_group, female, rank, health_pred) %>%
  pivot_longer(contains("resid_"),
               names_to = "level",
               names_prefix = "resid_",
               values_to = "residual") %>%
  filter(health_pred == level)
health_resid_plot <- health_resid_predictor %>%
  ggplot() +
  geom_point(aes(x = srhealth_memento, y = residual)) +
  # facet_grid(cols = vars(level),
  #            # rows = vars(rank), 
  #            labeller = labeller(level = c("1" = "Excellent/Very good",
  #                                          "2" = "Good",
  #                                          "3" = "Fair/Poor"))) +
  labs(x = "Continuous self-rated health", y = "Residuals") +
  theme_bw()

cowplot::plot_grid(health_resid_plot, mem_resid_plot,
                   label_size = 10, labels = "AUTO",
                   ncol = 1, nrow = 2)

ggsave(here::here("output", "bordeaux_crosswalk", "figS1_residual_predictor_plot_overlay.png"),
       dpi = 300, width = 9, height = 7, units = "in")

# #---- OLD ----
# #---- Table S3. stratified kappa ----
# load(here::here("data", "analysis_data", "Calishare", 
#                 "wtd_kappa_str_01312024.RData"))
# select <- dplyr::select
# kappa_str_formatted_tib <- kappa_tbl_str %>% t() %>%
#   as.data.frame() %>% dplyr::mutate(
#     Construct = str_extract(rownames(.), "[^_]+"),
#     estimate = str_extract(rownames(.), "(?<=_kappa_)[^_]+(?=_)"),
#     Covariates = str_replace(rownames(.), 
#                              paste0(Construct, "_kappa_", estimate,"_"), ""),
#     Construct = case_when(Construct == "srhealth" ~ "Self-rated health",
#                           Construct == "srmemory" ~ "Self-rated memory")) %>%
#   set_rownames(NULL) %>% dplyr::rename("kappa" = "V1") %>%
#   dplyr::select(Construct, Covariates, estimate, kappa) %>%
#   dplyr::mutate(Covariates = case_when(
#     Covariates == "age_group_70plus0" ~ "Age group (60-69)",
#     Covariates == "age_group_70plus1" ~ "Age group (70-89)",
#     Covariates == "female0" ~ "Men",
#     Covariates == "female1" ~ "Women",
#     Covariates == "rankmemento_share" ~ "Question order (continuous first)",
#     Covariates == "rankshare_memento" ~ "Question order (Likert first)",
#     str_detect(Covariates, "tertile1") ~ "Continuous self-rated construct: 1st tertile",
#     str_detect(Covariates, "tertile2") ~ "Continuous self-rated construct: 2nd tertile",
#     str_detect(Covariates, "tertile3") ~ "Continuous self-rated construct: 3rd tertile"))
# 
# kappa_str_formatted_wide <- kappa_str_formatted_tib %>%
#   pivot_wider( names_from = c("Construct", "estimate"), 
#                values_from = "kappa",
#                names_glue = "{Construct} {.value} {estimate}") %>%
#   mutate_if(is.numeric, function(x) sprintf("%.2f", x)) %>%
#   mutate(`Self-rated health weighted kappa` = 
#            paste0(`Self-rated health kappa pe`, " (", 
#                   `Self-rated health kappa 2.5th`, ", ",
#                   `Self-rated health kappa 97.5th`, ")"),
#          `Self-rated memory weighted kappa` = 
#            paste0(`Self-rated memory kappa pe`, " (", 
#                   `Self-rated memory kappa 2.5th`, ", ",
#                   `Self-rated memory kappa 97.5th`, ")")) %>%
#   mutate(`Self-rated memory weighted kappa` = 
#            na_if(`Self-rated memory weighted kappa`, "NA (NA, NA)"),
#          `Self-rated health weighted kappa` = 
#            na_if(`Self-rated health weighted kappa`, "NA (NA, NA)")) %>%
#   select(Covariates, contains("weighted"))
# # writexl::write_xlsx(kappa_str_formatted_wide,
# #                     path = here::here("output", "bordeaux_crosswalk",
# #                                       "table3_kappa_stratified_01312024.xlsx"))
# 
# 
# 
# #---- Table S4. correct proportion ----
# table_correct <- readRDS(here::here("data", "analysis_data", "Calishare", 
#                                     "correct_n_str_12202023.RDS"))
# table_correct_formatted <- table_correct %>%
#   select(covariates.level, prop_health, prop_memory) %>%
#   dplyr::mutate_if(is.numeric, function(x) sprintf("%.2f", x*100)) %>%
#   dplyr::mutate(Covariates = case_when(
#     covariates.level == "age_group_70plus.0" ~ "Age group (60-69)",
#     covariates.level == "age_group_70plus.1" ~ "Age group (70-89)",
#     covariates.level == "female.0" ~ "Men",
#     covariates.level == "female.1" ~ "Women",
#     covariates.level == "rank.memento_share" ~ "Question order (continuous first)",
#     covariates.level == "rank.share_memento" ~ "Question order (Likert first)")) %>%
#   dplyr::rename(`Self-rated health (% correspondence)` = prop_health,
#                 `Self-rated memory (% correspondence)` = prop_memory) %>%
#   select(Covariates, contains("Self-rated"))
# 
# # writexl::write_xlsx(table_correct_formatted,
# #                     here::here("output", "bordeaux_crosswalk", 
# #                                "table4_prop_correspondence.xlsx"))
# 
# 
# #--- **heat map ----
# select <- dplyr::select
# kappa_formatted_tib <- kappa_tbl %>% t() %>%
#   as.data.frame() %>% dplyr::mutate(
#     estimate = str_replace(rownames(.), "(.*)_", ""),
#     Model = str_replace(rownames(.), paste0("_kappa_", estimate), ""),
#     Model = str_replace(Model, "[^_]+_", ""),
#     # For ordering!
#     Model = str_replace(Model, "restricted_cubic_spline_3knots", "splines"), 
#     Model = str_replace(Model, "int_splines", "splines_int"),
#     Construct = str_extract(rownames(.), "[^_]+"),
#     Construct = case_when(Construct == "srhealth" ~ "Self-rated health",
#                           Construct == "srmemory" ~ "Self-rated memory")) %>%
#   set_rownames(NULL) %>% dplyr::rename("kappa" = "V1") %>%
#   dplyr::mutate(model_label = case_when(
#     str_detect(Model, "_splines_int_agesexint") ~ "M12: spline, interaction with order, age, sex",
#     str_detect(Model, "_splines_agesexint") ~ "M11: spline and interaction with age, sex",
#     str_detect(Model, "_int_agesexint") ~ "M10: interaction with order, age, sex",
#     str_detect(Model, "_agesexint") ~ "M9: interaction with age, sex",
#     str_detect(Model, "_splines_int_agesex") ~ "M8: spline and interaction with order, further adjusted for age, sex",
#     str_detect(Model, "_splines_agesex") ~ "M7: spline and adjusted for age, sex",
#     str_detect(Model, "_int_agesex") ~ "M6: interaction with order and adjusted for age, sex",
#     str_detect(Model, "_agesex") ~ "M5: adjusted for age, sex",
#     str_detect(Model, "_splines_int") ~ "M4: spline and interaction with order",
#     str_detect(Model, "_int") ~ "M2: interaction with order",
#     str_detect(Model, "_splines") ~ "M3: spline",
#     TRUE ~ "M1"),
#     regression_label = case_when(str_detect(Model, "linear") ~ "Linear ",
#                                  str_detect(Model, "multi") ~ "Multinomial ",
#                                  str_detect(Model, "ordinal") ~ "Ordinal "),
#     model_index = parse_number(model_label),
#     Model = paste0(regression_label, model_label)) %>%
#   arrange(Construct, regression_label, model_index)
# kappa_forplot <- kappa_formatted_tib  %>%
#   pivot_wider(names_from = "estimate", 
#               values_from = "kappa",
#               names_glue = "{.value}_{estimate}") %>%
#   bind_rows(tibble(
#     Construct = "Self-rated memory",
#     Model = c("Ordinal M4: spline and interaction with order",
#               "Ordinal M8: spline and interaction with order, further adjusted for age, sex",
#               "Ordinal M11: spline and interaction with age, sex",
#               "Ordinal M12: spline, interaction with order, age, sex"),
#     regression_label = "Ordinal ",
#     model_index = c(4, 8, 11, 12),
#     kappa_pe = NA)) %>%
#   bind_rows(tibble(
#     Construct = "Self-rated health",
#     Model = c("Ordinal M9: interaction with age, sex",
#               "Ordinal M10: interaction with order, age, sex",
#               "Ordinal M11: spline and interaction with age, sex",
#               "Ordinal M12: spline, interaction with order, age, sex"),
#     regression_label = "Ordinal ",
#     model_index = c(9, 10, 11, 12),
#     kappa_pe = NA)) %>%
#   mutate(Model = case_when(
#     str_detect(Model, "Linear M8") ~ paste0(Model, "        "),
#     str_detect(Model, "Ordinal M8") ~ paste0(Model, "       "),
#     TRUE ~ Model)) %>%
#   mutate(group = case_when(
#     str_detect(Model, "Linear") ~ "Linear model for 3-category outcome",
#     str_detect(Model, "Multinomial") ~ "Multinomial model for 3-category outcome",
#     str_detect(Model, "Ordinal") ~ "Ordinal model for 3-category outcome"),
#     labels = case_when(!is.na(kappa_pe) ~ paste0(sprintf("%.2f", kappa_pe),
#                                                  " (", sprintf("%.2f", kappa_2.5th),
#                                                  ", ", sprintf("%.2f", kappa_97.5th),
#                                                  ")"),
#                        is.na(kappa_pe) ~ "Non-convergence"))
# 
# 
# kappa_forplot$Model <- factor(kappa_forplot$Model, unique(kappa_forplot$Model))
# 
# kappa_forplot %>%
#   filter(!str_detect(Model, "Linear")) %>%
#   ggplot(aes(x = factor(Construct), y = fct_rev(Model),
#              fill = kappa_pe)) +
#   theme_minimal()+
#   geom_tile(color = "white") + 
#   scale_x_discrete(position = "top") +
#   # scale_fill_gradientn(colors = c("#A0262D", "#EC9963", "#FADF9E", "#9CC1DA"),
#   #                      # rev(c("#384598", "#9CC1DA", "#CAE4EB", "#FADF9E",
#   #                      #          "#EC9963", "#A0262D")),
#   #                      limit = c(0.3, 0.6),
#   #                      breaks = c(0.3, 0.4, 0.5, 0.6),
#   #                      oob = scales::squish,
#   #                      name = "Weighted\nkappa") +
#   geom_text(aes(x = factor(Construct), y = fct_rev(Model), 
#                 label = labels), color = "black", size = 3.6) +
#   # facet_grid(rows = vars(group), scales = "free", switch = "both") +
#   facet_wrap(~group, nrow = 3, scales = "free_y", strip.position = "top") +
#   theme(
#     text=element_text(size = 10, color = "black"),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.x = element_text(color = "black", size = 10),
#     axis.text.y = element_text(hjust = 0, color = "black", size = 10),
#     legend.title = element_text(size = 10),
#     # strip.text.y.left = element_text(size = 10, angle = 0),
#     strip.placement = "outside")
# 
# # ggsave(here::here("output", "bordeaux_crosswalk", "table2_weighted_kappa.png"),
# #        dpi = 300, width = 9, height = 7, units = "in")
# #---- **srhealth ----
# srhealth_plot <- bordeaux_cleaned %>%
#   mutate(srhealth_share_fac = factor(srhealth_share, levels = rev(1:5))) %>%
#   ggplot() +
#   geom_histogram(aes(x = srhealth_memento)) +
#   facet_grid(cols = vars(srhealth_share_fac),
#              # rows = vars(rank), 
#              labeller = labeller(srhealth_share_fac = c("1" = "Excellent",
#                                                         "2" = "Very good",
#                                                         "3" = "Good",
#                                                         "4" = "Fair",
#                                                         "5" = "Poor"))) +
#   
#   theme_bw() +
#   labs(x = "", title = "Self-rated health") +
#   theme(plot.title = element_text(hjust = 0.5, size = 10))
# srhealth_plot
# 
# # with(bordeaux_cleaned %>% filter(srhealth_share == 2), table(srhealth_memento))
# 
# # bordeaux_cleaned %>%
# #   ggplot() +
# #   geom_count(aes(x = srhealth_memento, y = srhealth_share)) +
# #   scale_size_area() +
# #   theme_bw()
# # 
# # bordeaux_cleaned %>%
# #   ggplot() +
# #   geom_jitter(aes(x = srhealth_memento, y = srhealth_share)) +
# #   theme_bw()
# # 
# # # Sanity check
# # with(bordeaux_cleaned %>%
# #        mutate(srhealth_share_fac = factor(srhealth_share, levels = rev(1:5))),
# #      table(srhealth_share_fac, srhealth_share))
# 
# #---- **srmemory ----
# srmemory_plot <- bordeaux_cleaned %>%
#   mutate(srmemory_share_fac = factor(srmemory_share, levels = 1:5)) %>%
#   # srmemory_memento_rev = 10 - srmemory_memento) %>%
#   ggplot() +
#   geom_histogram(aes(x = srmemory_memento), binwidth = 1) +
#   facet_grid(cols = vars(srmemory_share_fac), 
#              # rows = vars(rank), 
#              labeller = labeller(srmemory_share_fac = c("1" = "Excellent",
#                                                         "2" = "Very good",
#                                                         "3" = "Good",
#                                                         "4" = "Fair",
#                                                         "5" = "Poor"))) +
#   scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
#   theme_bw() +
#   labs(x = "", title = "Self-rated memory") +
#   theme(plot.title = element_text(hjust = 0.5, size = 10))
# srmemory_plot
# 
# # bordeaux_cleaned %>%
# #   ggplot() +
# #   geom_count(aes(x = srmemory_memento, y = srmemory_share)) +
# #   scale_size_area() +
# #   theme_bw()
# # 
# # bordeaux_cleaned %>%
# #   ggplot() +
# #   geom_jitter(aes(x = srmemory_memento, y = srmemory_share)) +
# #   theme_bw()
# 
# #---- **Joint dist both ----
# plot_grid(srhealth_plot, srmemory_plot, align = "vh",
#           label_size = 10, labels = "AUTO",
#           ncol = 1, nrow = 2)
# 
# ggsave(here::here("output", "bordeaux_crosswalk", "fig1_joint_dist_bordeaux.png"),
#        dpi = 300, width = 9, height = 7, units = "in")
# 
# #---- Box plot 5 category ----
# bordeaux_cleaned %>%
#   pivot_longer(cols = contains(c("srhealth", "srmemory")), 
#                names_to = c("measure", ".value"),
#                names_pattern = "(.*)_(.*)") %>%
#   mutate(share = factor(share, levels = rev(1:5))) %>%
#   ggplot() +
#   geom_boxplot(aes(x = share, y = memento)) +
#   geom_jitter(aes(x = share, y = memento), width = 0.1, alpha = 0.3) +
#   scale_y_continuous(n.breaks = 6) +
#   scale_x_discrete(labels = c("1" = "Excellent",
#                               "2" = "Very good",
#                               "3" = "Good",
#                               "4" = "Fair",
#                               "5" = "Poor")) +
#   facet_grid(rows = vars(measure), scales = "free_y",
#              labeller = labeller(measure = c("srhealth" = "Self-rated health",
#                                              "srmemory" = "Self-rated memory"))) +
#   theme_bw() +
#   labs(x = "Likert scale", y = "Continuous")
# 
# # ggsave(here::here("output", "bordeaux_crosswalk", "fig1_joint_dist_box_bordeaux.png"),
# #        dpi = 300, width = 9, height = 7, units = "in")
